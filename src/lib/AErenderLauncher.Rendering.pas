unit AErenderLauncher.Rendering;

{$M+}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  AErenderLauncher.Math;

//const
//  RM_NORMAL = 0; RM_TILED = 1; RM_ALL_AT_ONCE = 2;

type
  AERParamException = class(Exception);
  AERFrameSpanException = class(Exception);
  AERCompositionException = class(Exception);

  TRenderState = (RS_WAITING, RS_RENDERING, RS_FINISHED, RS_STOPPED, RS_ERROR);
  TRenderMode = (RM_NORMAL, RM_TILED, RM_ALL_AT_ONCE);
  //TRenderMode = Cardinal;

  //
  TExecutable = record
    Contents: WideString;
    Exec: TextFile;
  end;

  //
  TFrameSpan = record
    StartFrame, EndFrame: Integer;
    constructor Create(const StartFrame, EndFrame: Integer); overload;
    constructor Create(const StartFrame, EndFrame: String); overload;
    function Split(const Division: Integer): TArray<TFrameSpan>;
  end;
  //
  TComposition = record
    CompName: String;
    Frames: TFrameSpan;
    Split: Cardinal;
    Executable: TExecutable;
    constructor Create(const CompName: String; const Frames: TFrameSpan; const Split: Integer = 1);
  end;

  TRenderThread = class(TThread)
  private
    IsRendering: Boolean;
  published
    procedure Execute; override;
  end;

  // Rendering Envents
  TRenderStartEvent = procedure (Task: TObject) of object;
  TRenderFinishEvent = procedure (Task: TObject) of object;

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

    FRenderStartEvent: TRenderStartEvent;
    FRenderFinishEvent: TRenderFinishEvent;

    //procedure TaskTerminate(Sender: TObject);
    function ToExec: TList<TExecutable>;
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

    property OnRenderStart: TRenderStartEvent read FRenderStartEvent write FRenderStartEvent;
    property OnRenderFinish: TRenderFinishEvent read FRenderFinishEvent write FRenderFinishEvent;

    constructor Create (const Project, Output, OutputModule, RenderSettings: String;
                        const MissingFiles, Sound, Multiprocessing: Boolean;
                        const CustomProperties: String;
                        const CacheLimit, MemoryLimit: Single;
                        const Compositions: TList<TComposition>);

    procedure Render(Mode: Integer = 0);
    function ToString: String; override;
    function Count: Integer;
  end;

  //
  TAErender = class(TObject)
  private
    FPath: String;

    FRenderThreads: TArray<TRenderThread>;
    function TaskToExec(Task: TRenderTask): TArray<TExecutable>;
  published
    property Path: String read FPath write FPath;



    procedure Run(Task: TRenderTask);
    procedure BackgroundRun(Task: TRenderTask);
    procedure RenderTasks(Tasks: TArray<TRenderTask>; Mode: TRenderMode);

    constructor Create;
  end;

  function GetTaskByProject(const AProject: String): TRenderTask;
  function GetTaskByCompName(const ACompName: String): TRenderTask; deprecated 'Useless in this context, we CAN get Project without browsing through compositions';
  function GetCompByName(const ATask: TRenderTask; const ACompName: String): TComposition;
  function GetCompIndexInTask(const ATask: TRenderTask; const ACompName: String): Integer;

var
  AErenderPath, Errors: String;
  RenderTasks: TObjectList<TRenderTask>;

implementation


(*******************)
(*  TRenderThread  *)
(*******************)
procedure TRenderThread.Execute;
begin
  var i: Integer := 0;
  while IsRendering do begin
    inc (i);
    Writeln(i);
    if i >= 5 then begin
      IsRendering := False;
    end;
  end;
  Self.DoTerminate;
end;

(****************)
(*  TFrameSpan  *)
(****************)
function TFrameSpan.Split(const Division: Integer): TArray<TFrameSpan>;
var
  I, J, K, Start, Stop: Integer;
  Tm: String;
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

function TRenderTask.ToExec: TList<TExecutable>;
begin
  // Start with empty string
  var TempStr: WideString := '';
  var List: TList<WideString> := TList<WideString>.Create();

  // Append all the nesesary information in the beginning
  TempStr := {$IFDEF MSWINDOWS}'chcp 65001' + #13#10 + {$ENDIF}
    Format('("%s" -project "%s" -output "%s" %s %s %s -OMtemplate "%s" -RStemplate "%s" -mem_usage "%d" "%d" %s', [
      AErenderPath,
      Project,
      Output,
      IfThenElse(Sound, '-sound ON', ''),
      IfThenElse(Multiprocessing, '-mp', ''),
      IfThenElse(MissingFiles, '-continueOnMissingFootage', ''),
      OutputModule,
      RenderSettings,
      Trunc(MemoryLimit),
      Trunc(CacheLimit),
      IfThenElse(CustomProperties.IsEmpty, CustomProperties, '')
    ]);

  List.Add(TempStr);
  {for var Comp in Compositions do begin
    if Comp.Split = 0 then
      // yeet
    else
      for var FrameSpan in Comp.Frames.Split(Comp.Split) do
        TempStr := TempStr + Format('-s %d -e %d ', [FrameSpan.StartFrame, FrameSpan.EndFrame]);
  end; }
end;

procedure TRenderTask.Render(Mode: Integer = 0);
begin
  ToExec;
  {if Assigned(FRenderStartEvent) then FRenderStartEvent(Self);

  FRenderThread := TRenderThread.Create(True);

  FRenderThread.OnTerminate := Self.TaskTerminate;
  FRenderThread.IsRendering := True;
  FRenderThread.Start;}
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


(***************)
(*  TAErender  *)       // but do I really need it, though?
(***************)
constructor TAErender.Create;
begin
  inherited;
end;

function TAErender.TaskToExec(Task: TRenderTask): TArray<TExecutable>;
var
  TempStr: String;
begin
  TempStr := '';
  with Task do begin
    for var Comp in Compositions do begin
      if Comp.Split = 0 then
        // yeet
      else
        for var FrameSpan in Comp.Frames.Split(Comp.Split) do
          TempStr := TempStr + Format('-s %d -e %d ', [FrameSpan.StartFrame, FrameSpan.EndFrame]);
    end;
  end;
end;

procedure TAErender.Run(Task: TRenderTask);
begin
  //
  TaskToExec(Task);
  // Fire event
  //if Assigned(FRenderStartEvent) then FRenderStartEvent(Task as TObject);
end;

procedure TAErender.BackgroundRun(Task: TRenderTask);
begin
  //
end;

procedure TAErender.RenderTasks(Tasks: TArray<TRenderTask>; Mode: TRenderMode);
begin
  case Mode of
    RM_NORMAL: begin

    end;
    RM_TILED: begin

    end;
    RM_ALL_AT_ONCE: begin

    end;
  end;
  // Fire event
  //if Assigned(FRenderStartEvent) then FRenderStartEvent(Tasks[0]);
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
  Errors := '';


(******************)
(*  Finalization  *)
(******************)
finalization
  if Assigned(RenderTasks) then FreeAndNil(RenderTasks);

end.
