unit AErenderLauncher.Rendering;

{$M+}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON,

  AErenderLauncher.Math,
  AErenderLauncher.IO,
  AErenderLauncher.AerenderParser;

type
  AERParamException = class(Exception);
  AERFrameSpanException = class(Exception);
  AERCompositionException = class(Exception);

  TRenderState = (RS_WAITING, RS_RENDERING, RS_FINISHED, RS_STOPPED, RS_ERROR);
  TRenderMode = (RM_NORMAL, RM_TILED, RM_ALL_AT_ONCE);

  TExecutable = record
    Contents: WideString;
    Exec: TextFile;
    /// Add procedure to start executable
  end;

  ///
  TFrameSpan = record
    StartFrame, EndFrame: Integer;
    constructor Create(const StartFrame, EndFrame: Integer); overload;
    constructor Create(const StartFrame, EndFrame: String); overload;
    function Split(const Division: Integer): TArray<TFrameSpan>;
  end;

  ///
  TComposition = record
    CompName: String;
    Frames: TFrameSpan;
    Split: Cardinal;
    Executable: TExecutable;
    IsRendering: Boolean;
    constructor Create(const CompName: String; const Frames: TFrameSpan; const Split: Integer = 1);
  end;

  /// Rendering Events
//  TRenderStartEvent = procedure (Task: TObject) of object;
//  TRenderFinishEvent = procedure (Task: TObject) of object;

  /// <summary>
  /// Structure that contains basic required
  /// data for displaying rendering progress
  /// </summary
  TRenderQueueObject = record
    TempFilePath: String;
    Duration: TTimecode;
    FrameRate: TFrameRate;
    constructor Create(const ATempFilePath: String; const ADuration: TTimecode; const AFrameRate: TFrameRate);
    //constructor Create(const ALogFile: String; const ACompositions: TList<TComposition>; const ARenderMode: TRenderMode); overload;
    //constructor Create(const ALogFile: String; const ATask: TRenderTask); overload; deprecated 'Use non-overloaded constructor';
  end;

  /// <summary>
  /// Class for storing rendering tasks data
  /// </summary>
  TRenderTask = class(TObject)
  private   // Declare all the private variables
    FProject,
    FOutput,
    FOutputModule,
    FRenderSettings,
    FCustomProp: String;
    FMissingFiles,
    FSound,
    FMultiprocessing: Boolean;
    FCacheLimit,
    FMemoryLimit: Single;
    //FFrames: TArray<TFrameSpan>;
    FCompositions: TList<TComposition>;
    FLogFilePaths: TList<String>;
    FState: TRenderState;

    //FRenderThread: TRenderThread;

//    FRenderStartEvent: TRenderStartEvent;
//    FRenderFinishEvent: TRenderFinishEvent;

    //procedure TaskTerminate(Sender: TObject);
    function ToExec: TList<String>;
  published   // Make some properties for tasks to use
    // Standard
    property Project: String read FProject write FProject;
    property Output: String read FOutput  write FOutput;
    property OutputModule: String read FOutputModule  write FOutputModule;
    property RenderSettings: String read FRenderSettings write FRenderSettings;

    // AErender Properties
    property MissingFiles: Boolean read FMissingFiles write FMissingFiles default False;
    property Sound: Boolean read FSound write FSound default True;
    property Multiprocessing: Boolean read FMultiprocessing write FMultiprocessing default True;
    property CustomProperties: String read FCustomProp write FCustomProp;
    property CacheLimit: Single read FCacheLimit write FCacheLimit;
    property MemoryLimit: Single read FMemoryLimit write FMemoryLimit;

    // Compositions and Frames
    //property Frames: TArray<TFrameSpan> read FFrames write FFrames;
    property Compositions: TList<TComposition> read FCompositions write FCompositions;

    property LogFilePaths: TList<String> read FLogFilePaths write FLogFilePaths;
    property State: TRenderState read FState write FState default TRenderState.RS_WAITING;

//    property OnRenderStart: TRenderStartEvent read FRenderStartEvent write FRenderStartEvent;
//    property OnRenderFinish: TRenderFinishEvent read FRenderFinishEvent write FRenderFinishEvent;

    constructor Create (const Project, Output, OutputModule, RenderSettings: String;
                        const MissingFiles, Sound, Multiprocessing: Boolean;
                        const CustomProperties: String;
                        const CacheLimit, MemoryLimit: Single;
                        const Compositions: TList<TComposition>);

    constructor CreateFromJSON (const Project, Output, OutputModule, RenderSettings: String;
                                const MissingFiles, Sound, Multiprocessing: Boolean;
                                const CustomProperties: String;
                                const CacheLimit, MemoryLimit: Single;
                                const ProjectJSON: String);

    procedure Render(const ThreadsLimit: Integer = 4{; const Mode: TRenderMode = RM_NORMAL});
    function ToString: String; override;
    function Count: Integer;
  end;

  function GetTaskByProject(const AProject: String): TRenderTask;
  function GetTaskByCompName(const ACompName: String): TRenderTask; deprecated 'Useless in this context, we can get Project without browsing through compositions';
  function GetCompByName(const ATask: TRenderTask; const ACompName: String): TComposition;
  function GetCompIndexInTask(const ATask: TRenderTask; const ACompName: String): Integer;

var
  AErenderPath, Errors: String;
  RenderTasks: TObjectList<TRenderTask>;
  RenderQueue: TList<String>;
  AbortRender: Boolean = False;
  TiledRenderThread: TThread;

implementation

uses
  AErenderLauncher.Types;

(****************)
(*  TFrameSpan  *)
(****************)
function TFrameSpan.Split(const Division: Integer): TArray<TFrameSpan>;
var
  I, J, K: Integer;
begin
  SetLength(Result, Division);
  Result[0].StartFrame := StartFrame;
  J := EndFrame - StartFrame;
  K := J div Division;
  Result[0].EndFrame := StartFrame + K;
  for I := 1 to Division - 1 do
    begin
      Result[I].StartFrame := StartFrame + K * I + 1;
      Result[I].EndFrame := StartFrame + K * (I + 1);
    end;
  Result[Division - 1].EndFrame := EndFrame;
end;

constructor TFrameSpan.Create(const StartFrame, EndFrame: Integer);
begin
  Self.StartFrame := StartFrame;
  Self.EndFrame := EndFrame;
end;

constructor TFrameSpan.Create(const StartFrame, EndFrame: String);
begin
  if StartFrame <> '' then
    Self.StartFrame := StrToInt(StartFrame)
  else
    Self.StartFrame := 0;

  if EndFrame <> '' then
    Self.EndFrame := StrToInt(EndFrame)
  else
    Self.EndFrame := 0;
end;

(******************)
(*  TComposition  *)
(******************)
constructor TComposition.Create(const CompName: String; const Frames: TFrameSpan; const Split: Integer = 1);
begin
  if CompName <> '' then
    Self.CompName := CompName;
  {else
    raise AERCompositionException.Create('Composition name cannot be empty');}

  Self.Frames := Frames;
  Self.Split := Split;
end;

(***********************)
(*  TRenderQueueObject *)
(***********************)
//function TRenderQueueObject.GetDuration: TTimecode;
//begin
//  Result := FDuration;
//end;
//
//procedure TRenderQueueObject.SetDuration(Value: TTimecode);
//begin
//  FDuration := Value;
//end;
//
//function TRenderQueueObject.GetFrameRate: TFrameRate;
//begin
//  Result := FFrameRate;
//end;
//
//procedure TRenderQueueObject.SetFrameRate(Value: TFrameRate);
//begin
//  FFrameRate := Value;
//end;

constructor TRenderQueueObject.Create(const ATempFilePath: String; const ADuration: TTimecode; const AFrameRate: TFrameRate);
begin
  Self.TempFilePath := ATempFilePath;
  Self.Duration := ADuration;
  Self.FrameRate := AFrameRate;
end;

//constructor TRenderQueueObject.Create(const ALogFile: String; const ATask: TRenderTask);
//begin
//  Self.LogFile := ALogFile;
//  Self.Compositions := ATask.Compositions;
//  Self.RenderMode := RM_NORMAL;
//end;

(*****************)
(*  TRenderTask  *)
(*****************)
function TRenderTask.ToString: String;
begin
  var Res: String := Format('(%s, %s, %s, %s, [%s, %s, %s], "%s", %f, %f, ',
  [Project, Output, OutputModule, RenderSettings, BoolToStr(MissingFiles, True), BoolToStr(Sound, True), BoolToStr(Multiprocessing, True), CustomProperties, CacheLimit, MemoryLimit]);
  for var i := 0 to Compositions.Count - 1 do
    with Compositions.Items[i] do
      if i = Compositions.Count - 1  then
        Res := Res + Format('Comp[%d](%s, [%d, %d], %d)', [i + 1, CompName, Frames.StartFrame, Frames.EndFrame, Split])
      else
        Res := Res + Format('Comp[%d](%s, [%d, %d], %d), ', [i + 1, CompName, Frames.StartFrame, Frames.EndFrame, Split]);
  Res := Res + ')';

  Result := Res;
end;

function TRenderTask.Count: Integer;
begin
  var Res: Integer := 0;
  for var Comp in Compositions do begin
    if Comp.Split > 1 then
      Res := Res + Comp.Split
    else
      Res := Res + 1;
  end;
  Result := Res;
end;

/// <summary>
/// Creates executable files for Launcher to start and provides a list of
/// paths to them in *.ext format.
/// </summary>
/// <remarks>
/// "ext" must be replaced with a special format!!!
/// </remarks>
/// <returns>
/// A list of paths to executable files
/// </returns
function TRenderTask.ToExec: TList<String>;
begin
  // Start with empty string
  var TempStr: String := '';
  var List: TList<String> := TList<String>.Create();

  for var Composition: TComposition in Compositions do begin
    for var i := 0 to Composition.Split - 1 do begin

      var SplitFrameSpans: TArray<TFrameSpan> := Composition.Frames.Split(Composition.Split);
      SetLength(SplitFrameSpans, Length(SplitFrameSpans));  // Calculate split once, so we don't have to calculate it everytime

      // should match \[(.*?)\]\_\[(.*?)\]\_\[(.*?)\]\_\[(.*?)\]\.bat
      var FilePath: String := APPFOLDER + Format('[%s]_[%s]_[%d]_[%d].ext', [ExtractFileName(Project).Replace('.aep', ''), Composition.CompName, SplitFrameSpans[i].StartFrame, SplitFrameSpans[i].EndFrame]);

      // Add Pass output path to temporary wariable
      var AdjustedOutput: String := Output;

      // Create folder if '[projectName]/' or '[projectName]\' is used
      // Because aerender won't do it for you
      if Output.Contains('[projectName]' + PLATFORMPATHSEPARATOR) then
        begin
          AdjustedOutput := StringReplace(AdjustedOutput, '[projectName]', ExtractFileName(Project), [rfReplaceAll, rfIgnoreCase]);
          if not DirectoryExists(ExtractFilePath(AdjustedOutput)) then
            CreateDir(ExtractFilePath(AdjustedOutput));
        end;

      // Adjust file output path if split render is enabled
      // because they can conflict
      if Composition.Split > 1 then
        begin
          var OutFilePath: String := ExtractFilePath(AdjustedOutput);
          var OutFileName: String := StringReplace(ExtractFileName(AdjustedOutput), ExtractFileExt(AdjustedOutput), '', [rfReplaceAll, rfIgnoreCase]);
          var OutFileExt:  String := ExtractFileExt(AdjustedOutput);
          AdjustedOutput := OutFilePath + OutFileName + '_' + i.ToString + OutFileExt;
        end;

      // Append all the nesesary information in the beginning
      TempStr := Format('"%s" -project "%s" -output "%s" %s %s %s -OMtemplate "%s" -RStemplate "%s" -mem_usage "%d" "%d" %s -comp "%s" -s "%d" -e "%d"', [
        AErenderPath,
        Project,
        AdjustedOutput,
        IfThenElse(Sound, '-sound ON', ''),                         // yeet
        IfThenElse(Multiprocessing, '-mp', ''),                     // double yeet
        IfThenElse(MissingFiles, '-continueOnMissingFootage', ''),  // tripple yeet
        OutputModule,
        RenderSettings,
        Trunc(MemoryLimit),
        Trunc(CacheLimit),
        IfThenElse(CustomProperties.IsEmpty, CustomProperties, ''), // quadruple yeet
        Composition.CompName,
        SplitFrameSpans[i].StartFrame,
        SplitFrameSpans[i].EndFrame
      ]);

      if SplitFrameSpans[i].StartFrame = 0 then
        if SplitFrameSpans[i].EndFrame = 0 then
          TempStr := TempStr.Replace('-s "0" -e "0"', '');

      CreateCmd(
        {$IFDEF MSWINDOWS}'chcp 65001' + #13#10 + {$ENDIF}Format('(%s) > "%s"', [TempStr, FilePath.Replace('.ext', '.log')]),
        FilePath.Replace('.ext', {$IFDEF MSWINDOWS}'.bat'{$ELSE MACOS}'.command'{$ENDIF})
      );
      List.Add(FilePath);
    end;
  end;

  Result := List;

  {for var Comp in Compositions do begin
    if Comp.Split = 0 then
      // yeet
    else
      for var FrameSpan in Comp.Frames.Split(Comp.Split) do
        TempStr := TempStr + Format('-s %d -e %d ', [FrameSpan.StartFrame, FrameSpan.EndFrame]);
  end; }
end;

procedure TRenderTask.Render(const ThreadsLimit: Integer = 4{; const Mode: TRenderMode = RM_NORMAL});
begin
  var Executables: TList<String> := ToExec;
  //var i: Integer := 0;

  /// If files to render count is less than ThreadsLimit
  /// then render them all at once, no need for continous execution here
  if Executables.Count <= ThreadsLimit then
    for var i := 0 to Executables.Count - 1 do begin
      BackgroundExecute(Executables[i].Replace('.ext', {$IFDEF MSWINDOWS}'.bat'{$ELSE MACOS}'.command'{$ENDIF}));
        RenderQueue.Add(Executables[i]);
    end
  else begin
    /// This thread will launch new renders when the existing ones will finish
    ///
    /// Will be started only if the ammount of things to render is more than ThreadsLimit const
    /// If otherwise, it has a problem of not starting some of the renders, which bothers me...
    TiledRenderThread := TThread.CreateAnonymousThread(procedure begin
      var i: Integer := 0;   /// Counter

      while Executables.Count > 0 do begin
        while RenderQueue.Count <> ThreadsLimit do begin
          BackgroundExecute(Executables[i].Replace('.ext', {$IFDEF MSWINDOWS}'.bat'{$ELSE MACOS}'.command'{$ENDIF}));
          RenderQueue.Add(Executables[i]);
          Executables.Delete(i);
          inc(i);

          if AbortRender then
            exit;
        end;
        i := 0;
      end;
    end);

    TiledRenderThread.Start;
  end;
end;

constructor TRenderTask.Create(const Project, Output, OutputModule, RenderSettings: String;
  const MissingFiles, Sound, Multiprocessing: Boolean;
  const CustomProperties: String;
  const CacheLimit, MemoryLimit: Single;
  const Compositions: TList<TComposition>);
begin
  if (Project.IsEmpty or Output.IsEmpty or (Compositions.Count = 0)) then
    raise AERParamException.Create('Internal Error (EI001) - Project, Output or Compositions may be empty.');

  Self.Project        := Project;
  Self.Output         := Output;
  Self.OutputModule   := OutputModule;
  Self.RenderSettings := RenderSettings;

  Self.MissingFiles   := MissingFiles;
  Self.Sound          := Sound;
  Self.Multiprocessing  := Multiprocessing;
  Self.CustomProperties := CustomProperties;
  Self.CacheLimit     := CacheLimit;
  Self.MemoryLimit    := MemoryLimit;

  Self.Compositions   := Compositions;
end; 

constructor TRenderTask.CreateFromJSON(const Project, Output, OutputModule, RenderSettings: String;
  const MissingFiles, Sound, Multiprocessing: Boolean;
  const CustomProperties: String;
  const CacheLimit, MemoryLimit: Single;
  const ProjectJSON: String);
begin
  var ParsedProjectJSON: TJSONValue := TJSONObject.ParseJSONValue(ProjectJSON);

  var Compositions: TList<TComposition> := TList<TComposition>.Create;

  for var i := 0 to (ParsedProjectJSON as TJSONArray).Count - 1 do

  Compositions.Add(TComposition.Create(
    ParsedProjectJSON.A[i].P['Name'].Value,
    TFrameSpan.Create(ParsedProjectJSON.A[i].P['Frames'].A[0].Value, ParsedProjectJSON.A[i].P['Frames'].A[1].Value),
    1
  ));

  Self.Project        := Project;
  Self.Output         := Output;

  Self.OutputModule   := OutputModule;
  Self.RenderSettings := RenderSettings;

  Self.MissingFiles   := MissingFiles;
  Self.Sound          := Sound;
  Self.Multiprocessing  := Multiprocessing;
  Self.CustomProperties := CustomProperties;
  Self.CacheLimit     := CacheLimit;
  Self.MemoryLimit    := MemoryLimit;

  Self.Compositions   := Compositions;
end;

(*************)
(*  Helpers  *)
(*************)
function GetTaskByProject(const AProject: String): TRenderTask;
begin
  for var Task: TRenderTask in RenderTasks do
    if ExtractFileName(Task.Project) = AProject then begin
      Result := Task;
      exit
    end;
  Result := nil;
end;

function GetTaskByCompName(const ACompName: String): TRenderTask;
begin
  for var Task: TRenderTask in RenderTasks do
    for var Comp in Task.Compositions do
      if Comp.CompName = ACompName then begin
        Result := Task;
        exit;
      end;
  Result := nil;
end;

function GetCompByName(const ATask: TRenderTask; const ACompName: String): TComposition;
begin
  for var Comp in ATask.Compositions do
    if Comp.CompName = ACompName then begin
      Result := Comp;
      exit
    end;
  //Result := nil;
end;

function GetCompIndexInTask(const ATask: TRenderTask; const ACompName: String): Integer;
begin
  for var i := 0 to ATask.Compositions.Count - 1 do
    if ATask.Compositions[i].CompName = ACompName then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

(********************)
(*  Initialization  *)
(********************)
initialization
  RenderTasks := TObjectList<TRenderTask>.Create;
  RenderQueue := TList<String>.Create;
  Errors := '';


(******************)
(*  Finalization  *)
(******************)
finalization
  if Assigned(RenderTasks) then begin
    RenderTasks.OnNotify := nil;
    FreeAndNil(RenderTasks);
  end;

  if Assigned(RenderQueue) then begin
    RenderQueue.OnNotify := nil;
    FreeAndNil(RenderQueue);
  end;

end.
