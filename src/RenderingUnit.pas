unit RenderingUnit;

(*        AErender Launcher                                                                 *)
(*        RenderingUnit.pas                                                                 *)
(*        Lily Stilson // 2019 - 2021                                                       *)
(*        MIT License                                                                       *)
(*                                                                                          *)
(*        Copyright (c) 2019 - 2021 Alice Romanets                                          *)
(*                                                                                          *)
(*        Permission is hereby granted, free of charge, to any person obtaining a copy      *)
(*        of this software and associated documentation files (the "Software"), to deal     *)
(*        in the Software without restriction, including without limitation the rights      *)
(*        to use, copy, modify, merge, publish, distribute, sublicense, and/or sell         *)
(*        copies of the Software, and to permit persons to whom the Software is             *)
(*        furnished to do so, subject to the following conditions:                          *)
(*                                                                                          *)
(*        The above copyright notice and this permission notice shall be included in all    *)
(*        copies or substantial portions of the Software.                                   *)
(*                                                                                          *)
(*        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR        *)
(*        IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,          *)
(*        FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE       *)
(*        AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER            *)
(*        LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,     *)
(*        OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE     *)
(*        SOFTWARE.                                                                         *)

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Diagnostics,
  System.Generics.Collections,
  System.RegularExpressions,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Effects,
  FMX.BufferedLayout,
  FMX.BehaviorManager,
  FMX.RenderProgress,

  AErenderLauncher.AerenderParser,
  AErenderLauncher.Rendering,
  AErenderLauncher.Math,

  {$IFDEF MSWINDOWS}
    System.Notification, FMX.TaskBar, FMX.Platform.Win, Winapi.Windows, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}

  {$IFDEF MACOS}
  Posix.Unistd, FMX.Platform.Mac, MacApi.Foundation, MacApi.AppKit, MacApi.ObjectiveC, MacApi.CocoaTypes;
  {$ENDIF MACOS}

type
  TRenderingForm = class(TForm)
    WindowTitle: TLabel;
    RenderingStatusBar: TStatusBar;
    totalProgressBottomLabel: TLabel;
    statusBarSeparator1: TLine;
    abortRenderingButton: TButton;
    statusBarTopLayout: TLayout;
    RenderingToolBar: TToolBar;
    statusBarSeparator2: TLine;
    TotalProgressBar: TProgressBar;
    totalProgressLabel: TLabel;
    framesLabel: TLabel;
    topLayoutTitle: TLayout;
    totalProgressPercentage: TLabel;
    renderingTimer: TTimer;
    emptyLabel: TLabel;
    ProgressScrollBox: TVertScrollBox;
    AErenderLayout: TLayout;
    BlurEffect1: TBlurEffect;
    FFMPEGConcatLayout: TLayout;
    Rectangle1: TRectangle;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    Button1: TButton;
    timeElapsedLabel: TLabel;
    StopwatchTimer: TTimer;
    projectNameLabel: TLabel;
    totalFramesLabel: TLabel;
    procedure renderingTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure abortRenderingButtonClick(Sender: TObject);
    procedure StopwatchTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetLanguage(LanguageCode: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormHide(Sender: TObject);

    procedure RenderQueueNotify(Sender: TObject; const Item: String; Action: TCollectionNotification);
  private
    { Private declarations }
  public
    { Public declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  end;
  TRenderGroup = record
    // Visible components
    RenderPanel: TPanel;
    CompNameLabel: TLabel;
    LogButton: TButton;   // It will be styled
    FramesLabel: TLabel;
    StatusLabel: TLabel;
    RenderingProgress: TProgressBar;
    WaitProgress: TAniIndicator;
    LogMemo: TMemo;

    // Invisible components
    Duration: TTimecode;
    FrameRate: TFrameRate;
  end;

var
  RenderingForm: TRenderingForm;
  VISIBLE: Boolean = False;

  RenderProgresses: TObjectList<TRenderProgress>;
  CurrentTime: TDateTime;

  CurrentlyRendering: Integer = 0;

  { DONE -oLily Stilson -cRendering Unit - macOS : Add NSApplication.requestUserAttention for macOS when Rendering is finished }

implementation

{$R *.fmx}

uses
  {$REGION '    AErenderLauncher Liraries    '}
  MainUnit,
  SettingsUnit;
  {$ENDREGION}

{$IFDEF MSWINDOWS}
procedure TRenderingForm.CreateHandle;
begin
  inherited CreateHandle;
  var hWnd: HWND := FormToHWND(Self);

  SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);
end;
{$ENDIF MSWINDOWS}

procedure TRenderingForm.RenderQueueNotify(Sender: TObject; const Item: String; Action: TCollectionNotification);
begin
  /// Stop the timer temporarely
  renderingTimer.Enabled := False;

  var RegExMatch := TRegEx.Match(Item, '\[(.*?)\]\_\[(.*?)\]\_\[(.*?)\]\_\[(.*?)\]');

  //ProgressScrollBox.BeginUpdate;

  case Action of
    cnAdded: begin
      var Progress: TRenderProgress := TRenderProgress.Create(nil);
      Progress.Parent := ProgressScrollBox;
      Progress.Align := TAlignLayout.Top;
      Progress.Margins := TBounds.Create(TRectF.Create(8, 8, 8, 0));
      Progress.Composition := Format('%s[%s]', [RegExmatch.Groups[2].Value, (RenderQueue.IndexOf(Item) + 1).ToString]);
      Progress.ST_WaitingForAErender := Language[LANG].RenderingForm.WaitingForAerender;
      Progress.ST_Rendering := Language[LANG].RenderingForm.Rendering;
      Progress.ST_FinishedRender := Language[LANG].RenderingForm.RenderingFinished;
      Progress.ST_RenderError := Language[LANG].RenderingForm.RenderingError;
      Progress.InFrame := -1;
      Progress.OutFrame := 1;
      Progress.Value := -1;
      Progress.Log.Add('Launched ' + Item + #13#10);

      RenderProgresses.Add(Progress);
    end;
    cnRemoved: begin
      var index: Integer := 0;
      for var i := 0 to RenderProgresses.Count - 1 do
        if (RenderProgresses[i].Composition = RegExmatch.Groups[2].Value) and (RenderProgresses[i].Value = RenderProgresses[i].OutFrame) then
          index := i;
      RenderProgresses.Delete(index);
    end;
  end;

  /// Clear scrollbox for progresses
  //ProgressScrollBox.DeleteChildren;
  //Finalize(RenderProgresses);
  emptyLabel.Visible := RenderQueue.Count = 0;

  //ProgressScrollBox.EndUpdate;

  /// Enable timer back
  renderingTimer.Enabled := True;
end;

procedure TRenderingForm.SetLanguage(LanguageCode: Integer);
begin
  RenderingForm.Caption     := Language[LanguageCode].RenderingForm.RenderingProgress;
  WindowTitle.Text          := Language[LanguageCode].RenderingForm.RenderingProgress;

  //emptyLabel.Text         := Language[LanguageCode].RenderingForm.QueueIsEmpty;
  totalProgressLabel.Text   := Language[LanguageCode].RenderingForm.TotalProgress;
  abortRenderingButton.Text := Language[LanguageCode].RenderingForm.AbortRendering;

  timeElapsedLabel.Text     := Language[LanguageCode].RenderingForm.TimeElapsed + '00:00:00';
end;

procedure TRenderingForm.abortRenderingButtonClick(Sender: TObject);
begin
  try
    begin
      AbortRender := True;
      if (TiledRenderThread <> nil) and (TiledRenderThread.Started) then
        TiledRenderThread.Terminate;
      /// Kill After Effects rendering process
      {$IFDEF MSWINDOWS}KillProcess('AfterFX.com');{$ENDIF MSWINDOWS}
      {$IFDEF MACOS}KillProcess('aerendercore');{$ENDIF MACOS}

      /// Reset taskbar's progress
      {$IFDEF MSWINDOWS}MainTaskBar.TaskBarState := 0;{$ENDIF MSWINDOWS}

      /// Clear RenderQueue
      RenderQueue.Clear;

      /// Disable rendering start limitation
      isRendering := False;

      /// Stop timer and reset progress values
      emptyLabel.Visible := True;
      emptyLabel.Enabled := True;
      renderingTimer.Enabled := False;
      StopwatchTimer.Enabled := False;
      projectNameLabel.Text := '';
      framesLabel.Text := '';
      totalProgressPercentage.Text := '0%';
      TotalProgressBar.Value := 0;
      TotalProgressBar.Max := 1;
    end
  except
    on Exception do
      ShowMessage ('Nothing to abort!')
  end;
end;

procedure TRenderingForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  {$IFDEF MSWINDOWS}
  VISIBLE := False;

  /// If rendering is finished and there is still
  ///  something on the form while closing - clear it
  if TotalProgressBar.Value = TotalProgressBar.Max then
    begin
      abortRenderingButtonClick(Sender);
      TotalProgressBar.Value := 0;
      {$IFDEF MSWINDOWS}MainTaskBar.TaskBarState := 0;{$ENDIF MSWINDOWS}
    end;
  {$ENDIF}
end;

procedure TRenderingForm.FormHide(Sender: TObject);
begin
  {$IFDEF MACOS}    // Workaround
                    // Somehow after update to RAD 10.4.2
                    // form can't get to the OnClose event on macOS
  VISIBLE := False;

  // RU:  При закрытии формы, если рендеринг закончен
  //      очистить окно
  if Trunc(TotalProgressBar.Value) = Trunc(TotalProgressBar.Max) then begin
    abortRenderingButtonClick(Sender);
    TotalProgressBar.Value := 0;
  end;
  {$ENDIF}
end;

procedure TRenderingForm.FormCreate(Sender: TObject);
begin
  /// Reset Total progressbar
  TotalProgressBar.Min := 0;
  TotalProgressBar.Max := 1;
  TotalProgressBar.Value := 0;

  /// Activate smooth scrolling of ScrollBox
  ProgressScrollBox.AniCalculations.Animation := True;

  /// Create list for storing out progress components
  RenderProgresses := TObjectList<TRenderProgress>.Create;

  /// Assign event for RenderQueue
  RenderQueue.OnNotify := RenderQueueNotify;
end;

procedure TRenderingForm.FormShow(Sender: TObject);
begin
  VISIBLE := True;
  if MainUnit.RenderWindowSender = MainForm.launchButton then
    begin
      isRendering := True;

      /// Start timer, stopwatch and enable Windows's taskbar progress
      renderingTimer.Enabled := True;
      CurrentTime := Now;
      StopwatchTimer.Enabled := True;
      {$IFDEF MSWINDOWS}MainTaskBar.TaskBarState := 2;{$ENDIF MSWINDOWS}
    end;
end;

procedure TRenderingForm.renderingTimerTimer(Sender: TObject);
/// We need some data type for a thread to store,
/// read and display whatever is happening in logs
type
  TRenderData = record      /// The type itself
    LogFile: TStringList;   /// Logs will be stored here
    //Data: TStringList;    /// Have no fucking clue what it's doing here, but for whatever reason I put it here. And never used it anywhere in the code...
    Stream: TStream;        /// Reading thread. Data from here will be put in <LogFile> variable
    State: Integer;         /// Three states for rendering status: -1 (error), 0 (finished), 1 (rendering)
  end;
var
  Render: TArray<TRenderData>;
begin
  //var Finished: Integer := 0;
  //var Error: Integer := 0;
  var Completed: Integer := 0;

  /// Create an array for logs
  SetLength(Render, RenderQueue.Count);

/// Will be left commented, until I rememeber why I did this
//  for var j := 0 to High(Render) do
//    begin
//      Render[j].Data := TStringList.Create;
//      Render[j].State := '';
//    end;

  for var i := 0 to High(Render) do begin
    /// Create LogFile field and set it's encoding
    Render[i].LogFile := TStringList.Create;
    Render[i].LogFile.DefaultEncoding := TEncoding.UTF8;

    if FileExists(RenderQueue[i].Replace('.ext', '.log')) then
      try
        /// Create a stream here, from which we'll be copying data into <LogFile>
        /// In fact, each time the log is read anew, we won't be able to hold it in memory
        /// and we don't need to, since it's constantly updating

        Render[i].Stream := TFileStream.Create(RenderQueue[i].Replace('.ext', '.log'), fmOpenRead or fmShareDenyNone);
        try
          Render[i].LogFile.LoadFromStream(Render[i].Stream);
        finally
          Render[i].Stream.Free;
        end;

        /// If the ammount of lines in log file is more than out Memo has, then we
        /// just need to append this line and parse it
        if Render[i].LogFile.Count > RenderProgresses[i].Log.Count then begin
          /// Append the last line to the log and scroll to Memo bottom
          RenderProgresses[i].Log.Add(Render[i].LogFile[RenderProgresses[i].Log.Count]);
          RenderProgresses[i].GoToTextEnd;

          /// The TRenderProgress component has it's default max value at 1
          /// This will help us to determine, which thread already has it's max
          /// ammount of frames and which not yet.
          /// Here we will read some basic information of what we're going to render
          if RenderProgresses[i].OutFrame = 1 then begin
              /// If log's last line contains 'Duration: ', then we're going to
              /// parce that string and extract that information into a temporary variable
              if RenderProgresses[i].Log[RenderProgresses[i].Log.Count - 1].Contains('Duration: ') then
                RenderProgresses[i].Duration := ParseAErenderDurationLogString(RenderProgresses[i].Log[RenderProgresses[i].Log.Count - 1]);

              /// If log's last line contains 'Frame Rate: ', then we're going to
              /// parce that string and extract that information into a temporary variable
              if RenderProgresses[i].Log[RenderProgresses[i].Log.Count - 1].Contains('Frame Rate: ') then
                RenderProgresses[i].FrameRate := ParseAErenderFrameRateLogString(RenderProgresses[i].Log[RenderProgresses[i].Log.Count - 1]);

              /// if we successfully got Duration and Framerate,
              /// then we need to calculate max value for our progress bar.
              ///
              /// Doing it this way is much more accurate then extracting it with
              /// RegEx from executable's file name
              if (RenderProgresses[i].Duration.ToSingleString <> '0:00:00:00') and (RenderProgresses[i].FrameRate <> 0) then begin
                RenderProgresses[i].OutFrame := TimecodeToFrames(RenderProgresses[i].Duration, RenderProgresses[i].FrameRate);
              end;
          end;
        end;

        /// Count total frames from all progresses
        var TotalFrames: Single := 1;
          for var j := 0 to RenderProgresses.Count - 1 do
            if RenderProgresses[j].OutFrame <> 1 then
              TotalFrames := TotalFrames + RenderProgresses[j].OutFrame;

        /// Start displaying progress only if we know the total ammount of frames
        if RenderProgresses[i].OutFrame <> 1 then begin
          /// Try to parce log's last line
          /// Until we will get some data, the function <ParseAErenderFrameLogString>
          /// will be throwing exceptions. If we catch this exception, we'll just leave
          /// progress' value unchanged. This saves us from exceptions at the end of rendering
          /// Here we just ignoring all the useless for Parser lines
          try
            var AERP: TAErenderFrameData := ParseAErenderFrameLogString(RenderProgresses[i].Log[RenderProgresses[i].Log.Count - 1]);
            RenderProgresses[i].Value := AERP.Frame;
            //RenderProgresses[i].StatusLabel.Text := Language[LANG].RenderingForm.Rendering;
            //RenderProgresses[i].FramesLabel.Text := Format('%s / %s', [RenderGroups[i].RenderingProgress.Value.ToString, RenderGroups[i].RenderingProgress.Max.ToString]);
          except
            on Exception do
              RenderProgresses[i].Value := RenderProgresses[i].Value;
          end;
        end;

        /// Check if the rendering is finished yet or not
        if RenderProgresses[i].Log.Text.Contains('Finished composition') then begin
          Render[i].State := 0;
          //RenderProgresses[i]..Text := Language[LANG].RenderingForm.RenderingFinished;
          RenderProgresses[i].Value := RenderProgresses[i].OutFrame;
        end;
        /// Check if the rendering finished with an error
        if RenderProgresses[i].Log.Text.Contains('ERROR:') or RenderProgresses[i].Log.Text.Contains('USAGE:') then begin
          Render[i].State := -1;
          RenderProgresses[i].Value := RenderProgresses[i].OutFrame;
          RenderProgresses[i].IsError := True;
        end;

        /// Set TotalProgressBar's max value, if it's not updated
        if TotalProgressBar.Max <> TotalFrames then begin
          TotalProgressBar.Max := TotalFrames;
        end else begin
          /// Count total rendered frames
          var sum: Single := 0;
          for var j := 0 to RenderProgresses.Count - 1 do
            if Render[j].State <> -1 then
              sum := sum + RenderProgresses[j].Value;

          TotalProgressBar.Value := sum;
          totalProgressPercentage.Text := Trunc((TotalProgressBar.Value / TotalProgressBar.Max) * 100).ToString + '%';
          {$IFDEF MSWINDOWS}MainTaskBar.TaskBarProgress := Trunc((TotalProgressBar.Value / TotalProgressBar.Max) * 100);{$ENDIF MSWINDOWS}
          if (TotalProgressBar.Max = 0) or (TotalProgressBar.Max = 1) then
            framesLabel.Text := Language[LANG].RenderingForm.WaitingForAerender
          else
            framesLabel.Text := TotalProgressBar.Value.ToString + ' / ' + (TotalProgressBar.Max - 1).ToString;
        end;
      finally
        FreeAndNil(Render[i].LogFile);  // RU: Уничтожаем контейнер с логом, чтобы не было проблем
      end;
  end;

  {for var i := 0 to High(Render) do
    if Render[i].State = 0 then
      inc (Finished);

  for var i := 0 to High(Render) do
    if Render[i].State = -1 then
      inc (Error);}

  /// Count how many threads have already finished
  /// Count even if it's errored, otherwise timer
  /// will never stop
  for var i := 0 to RenderProgresses.Count - 1 do
    if RenderProgresses[i].Value = RenderProgresses[i].OutFrame then
      inc(Completed);

  /// If completed threads count == total threads count
  /// we can safely stop the timer
  if (Completed = RenderQueue.Count) then begin
    /// Stop the timer
    StopwatchTimer.Enabled := False;

    {for var i := 0 to High(RenderGroups) do
      if Render[i].State <> -1 then
        RenderGroups[i].StatusLabel.Text := Language[LANG].RenderingForm.RenderingFinished;}

    /// Completely erase our array with logs' data
    Finalize(Render);

    TotalProgressBar.Value := TotalProgressBar.Max;
    totalProgressPercentage.Text := '100%';

    {$IFDEF MSWINDOWS}
    FlashWindow(ApplicationHWND, True);
    MainTaskBar.TaskBarState := 0;
    {if (Error > 0) and (Error <> Length(LogFiles)) then
      MainTaskBar.TaskBarState := 4;
    if Error = Length(LogFiles) then
      MainTaskBar.TaskBarState := 3;}
    {$ELSE MACOS}
    var Application: NSApplication := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
    Application.requestUserAttention(NSCriticalRequest);
    {$ENDIF}

    renderingTimer.Enabled := False;
  end;
end;

procedure TRenderingForm.StopwatchTimerTimer(Sender: TObject);
begin
  // This causes on macOS
  // [dccosx64 Fatal Error] RenderingUnit.pas(587): F2084 Internal Error: URW1237
  // Workaround: Get string that is already in array and pass it through an inline variable
  var TimeElapsed: String := Language[LANG].RenderingForm.TimeElapsed;
  timeElapsedLabel.Text := TimeElapsed + FormatDateTime('hh:nn:ss', Now - CurrentTime);
end;

end.


