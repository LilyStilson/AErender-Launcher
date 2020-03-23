unit RenderingUnit;

(*        AErender Launcher                                                                 *)
(*        RenderingUnit.pas                                                                 *)
(*        Lily Stilson // 2019 - 2020                                                       *)
(*        MIT License                                                                       *)
(*                                                                                          *)
(*        Copyright (c) 2019 - 2020 Alice Romanets                                          *)
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

  AErenderDataParser,

  {$IFDEF MSWINDOWS}
    FMX.Platform.Win, Winapi.Windows, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}

  {$IFDEF MACOS}
  Posix.Unistd;
  {$ENDIF MACOS}

type
  TRenderingForm = class(TForm)
    Title: TLabel;
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
    VertScrollBox1: TVertScrollBox;
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
    procedure ShowLogButtonClick (Sender: TObject);
    procedure renderingTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure abortRenderingButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StopwatchTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  end;
  TRenderGroup = record
    TRenderGroupBox: TGroupBox;
      TRenderGroupBoxMainLayout: TLayout;
        TRenderProgressBar: TProgressBar;
        TRenderProgressLabel: TLabel;
        TRenderShowLogButton: TButton;
      TLogMemo: TMemo;
    Duration: TTimecode;
    FrameRate: TFrameRate;
  end;

var
  RenderingForm: TRenderingForm;
  VISIBLE: Boolean = False;
  RenderGroups: TArray<TRenderGroup>;
  LogIncrement: Integer = 0;
  CurrentTime: TDateTime;

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

function LimitInt (I: Integer): Integer;
begin
  if I < 0 then
    Result := 0
  else
    Result := I;
end;

procedure TRenderingForm.abortRenderingButtonClick(Sender: TObject);
begin
  try
    begin
      {$IFDEF MSWINDOWS}KillProcess('AfterFX.com');{$ENDIF MSWINDOWS}
      {$IFDEF MACOS}KillProcess('aerendercore');{$ENDIF MACOS}
      for var i := 0 to High(RenderGroups) do
        begin
          RenderGroups[i].TLogMemo.Free;
          RenderGroups[i].TRenderShowLogButton.Free;
          RenderGroups[i].TRenderProgressLabel.Free;
          RenderGroups[i].TRenderProgressBar.Value := 0;
          RenderGroups[i].TRenderProgressBar.Max := 1;
          RenderGroups[i].TRenderProgressBar.Destroy;
          RenderGroups[i].TRenderGroupBoxMainLayout.Free;
          RenderGroups[i].TRenderGroupBox.Free;

          RenderGroups[i].Duration.Clear;

          {$IFDEF MSWNDOWS}DeleteFile(MainUnit.LogFiles[i]);{$ENDIF MSWINDOWS}
        end;
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
  VISIBLE := False;
  if TotalProgressBar.Value = TotalProgressBar.Max then
    begin
      abortRenderingButtonClick(Sender);
      TotalProgressBar.Value := 0;
    end;
end;

procedure TRenderingForm.FormCreate(Sender: TObject);
begin
  TotalProgressBar.Min := 0;
  TotalProgressBar.Max := 1;
  TotalProgressBar.Value := 0;
end;

procedure TRenderingForm.FormShow(Sender: TObject);
begin
  VISIBLE := True;
  if MainUnit.RenderWindowSender = MainForm.launchButton then
    begin
      if Length(MainUnit.LogFiles) = 0 then
        begin
          emptyLabel.Visible := True;
          emptyLabel.Enabled := True;
        end
      else
        begin
          emptyLabel.Visible := False;
          emptyLabel.Enabled := False;

          projectNameLabel.Text := ExtractFileName(MainForm.projectPath.Text);

          SetLength (RenderGroups, Length(MainUnit.LogFiles));

          for var i := 0 to High (RenderGroups) do
            begin
              //Initialize GroupBox
              RenderGroups[i].TRenderGroupBox := TGroupBox.Create(Self);
              RenderGroups[i].TRenderGroupBox.Parent := VertScrollBox1;
              RenderGroups[i].TRenderGroupBox.Align := TAlignLayout.Top;
              RenderGroups[i].TRenderGroupBox.Margins.Left := 5;
              RenderGroups[i].TRenderGroupBox.Margins.Bottom := 5;
              RenderGroups[i].TRenderGroupBox.Position.X := 5;
              RenderGroups[i].TRenderGroupBox.Height := 75;
              RenderGroups[i].TRenderGroupBox.Text := ExtractFileName(MainUnit.LogFiles[i]);
              RenderGroups[i].TRenderGroupBox.Tag := i;

              //Initialize GroupBox MainLayout
              RenderGroups[i].TRenderGroupBoxMainLayout := TLayout.Create(Self);
              RenderGroups[i].TRenderGroupBoxMainLayout.Parent := RenderGroups[i].TRenderGroupBox;
              RenderGroups[i].TRenderGroupBoxMainLayout.Align := TAlignLayout.Top;
              RenderGroups[i].TRenderGroupBoxMainLayout.Margins.Left := 5;
              RenderGroups[i].TRenderGroupBoxMainLayout.Margins.Top := 20;
              RenderGroups[i].TRenderGroupBoxMainLayout.Margins.Right := 5;
              RenderGroups[i].TRenderGroupBoxMainLayout.Margins.Bottom := 5;

              //Initialize GroupBox MainLayout ProgressBar
              RenderGroups[i].TRenderProgressBar := TProgressBar.Create(Self);
              RenderGroups[i].TRenderProgressBar.Parent := RenderGroups[i].TRenderGroupBoxMainLayout;
              RenderGroups[i].TRenderProgressBar.Align := TAlignLayout.Top;
              RenderGroups[i].TRenderProgressBar.Orientation := TOrientation.Horizontal;
              RenderGroups[i].TRenderProgressBar.StyleLookup := 'progressbarstyle';
              RenderGroups[i].TRenderProgressBar.Margins.Left := 5;
              RenderGroups[i].TRenderProgressBar.Margins.Top := 10;
              RenderGroups[i].TRenderProgressBar.Margins.Right := 5;
              RenderGroups[i].TRenderProgressBar.Height := 10;
              RenderGroups[i].TRenderProgressBar.Min := 0;
              RenderGroups[i].TRenderProgressBar.Value := 0;
              RenderGroups[i].TRenderProgressBar.Max := 1;

              //Initialise GroupBox MainLayout ProgressLabel
              RenderGroups[i].TRenderProgressLabel := TLabel.Create(Self);
              RenderGroups[i].TRenderProgressLabel.Parent := RenderGroups[i].TRenderGroupBoxMainLayout;
              RenderGroups[i].TRenderProgressLabel.Align := TAlignLayout.Client;
              RenderGroups[i].TRenderProgressLabel.Margins.Left := 5;
              RenderGroups[i].TRenderProgressLabel.AutoSize := False;
              RenderGroups[i].TRenderProgressLabel.TextSettings.WordWrap := False;
              RenderGroups[i].TRenderProgressLabel.Text := 'Waiting for aerender...';
              
              //Initialize GroupBox MainLayout ShowLogButton
              RenderGroups[i].TRenderShowLogButton := TButton.Create(Self);
              RenderGroups[i].TRenderShowLogButton.Parent := RenderGroups[i].TRenderGroupBoxMainLayout;
              RenderGroups[i].TRenderShowLogButton.Align := TAlignLayout.Right;
              RenderGroups[i].TRenderShowLogButton.Width := 150;
              RenderGroups[i].TRenderShowLogButton.Margins.Top := 5;
              RenderGroups[i].TRenderShowLogButton.Margins.Right := 5;
              RenderGroups[i].TRenderShowLogButton.Margins.Bottom := 5;
              RenderGroups[i].TRenderShowLogButton.Text := 'Toggle Render Log';
              RenderGroups[i].TRenderShowLogButton.Tag := i;
              RenderGroups[i].TRenderShowLogButton.OnClick := ShowLogButtonClick;

              //Initialise GroupBox LogMemo
              RenderGroups[i].TLogMemo := TMemo.Create(Self);
              RenderGroups[i].TLogMemo.Parent := RenderGroups[i].TRenderGroupBox;
              RenderGroups[i].TLogMemo.Align := TAlignLayout.Client;
              RenderGroups[i].TLogMemo.Margins.Left := 5;
              RenderGroups[i].TLogMemo.Margins.Right := 5;
              RenderGroups[i].TLogMemo.Margins.Bottom := 5;
              RenderGroups[i].TLogMemo.ReadOnly := True;
              RenderGroups[i].TLogMemo.WordWrap := True;
              RenderGroups[i].TLogMemo.Visible := False;
              RenderGroups[i].TLogMemo.TextSettings.Font.Family := 'Consolas';
            end;
        end;
      renderingTimer.Enabled := True;
      CurrentTime := Now;
      StopwatchTimer.Enabled := True;
    end;
end;

procedure TRenderingForm.renderingTimerTimer(Sender: TObject);
type
  TRenderData = record
    LogFile: TStrings;
    Data: TStrings;
    Stream: TStream;
    State: String[6];
  end;
var
  Render: TArray<TRenderData>;
  i: Integer;
begin
  var Finished: Integer := 0;
  var Error: Integer := 0;

  SetLength (Render, Length(MainUnit.LogFiles));

  for var j := 0 to High(Render) do
    begin
      Render[j].Data := TStrings.Create;
      Render[j].State := '';
    end;

  for i := 0 to High(Render) do
    begin
      Render[i].LogFile := TStringList.Create;
      Render[i].LogFile.Encoding.UTF8;

      try
        Render[i].Stream := TFileStream.Create(LogFiles[i], fmOpenRead or fmShareDenyNone);
        try
          Render[i].LogFile.LoadFromStream(Render[i].Stream);
        finally
          Render[i].Stream.Free;
        end;

        if Render[i].LogFile.Count > RenderGroups[i].TLogMemo.Lines.Count then begin
          RenderGroups[i].TLogMemo.Lines.Add(Render[i].LogFile[RenderGroups[i].TLogMemo.Lines.Count]);
          RenderGroups[i].TLogMemo.GoToTextEnd;
          if RenderGroups[i].TRenderProgressBar.Max = 1 then begin
            //Try to get Duration timecode from log string
            //if RenderGroups[i].TRenderProgressBar.Max = 1 then begin
              //Try to get Duration from log string
              if RenderGroups[i].TLogMemo.Lines[RenderGroups[i].TLogMemo.Lines.Count - 1].Contains('Duration: ') then
                RenderGroups[i].Duration := ParseAErenderDurationLogString(RenderGroups[i].TLogMemo.Lines[RenderGroups[i].TLogMemo.Lines.Count - 1]);

              //Try to get FrameRate from log string
              if RenderGroups[i].TLogMemo.Lines[RenderGroups[i].TLogMemo.Lines.Count - 1].Contains('Frame Rate: ') then
                RenderGroups[i].FrameRate := ParseAErenderFrameRateLogString (RenderGroups[i].TLogMemo.Lines[RenderGroups[i].TLogMemo.Lines.Count - 1]);

              //Try to calculate frames based on recieved Duration and FrameRate
              if (RenderGroups[i].Duration.ToSingleString <> '0:00:00:00') and (RenderGroups[i].FrameRate <> 0) then begin
                RenderGroups[i].TRenderProgressBar.Max := TimecodeToFrames(RenderGroups[i].Duration, RenderGroups[i].FrameRate);
              end;
            //end;

            //RenderGroups[i].TRenderProgressLabel.Text := 'D = ' + RenderGroups[i].Duration.ToSingleString + '; F = ' + RenderGroups[i].FrameRate.ToString + '; TF = ' + RenderGroups[i].TRenderProgressBar.Max.ToString;
          end;
        end;

        if RenderGroups[i].TRenderProgressBar.Max <> 1 then begin
          try
            var AERP: TAErenderFrameData := ParseAErenderFrameLogString(RenderGroups[i].TLogMemo.Lines[RenderGroups[i].TLogMemo.Lines.Count - 1]);
            RenderGroups[i].TRenderProgressBar.Value := AERP.Frame;
            RenderGroups[i].TRenderProgressLabel.Text := 'Rendering: '
                                                        + Round((RenderGroups[i].TRenderProgressBar.Value / RenderGroups[i].TRenderProgressBar.Max) * 100).ToString + '% ('
                                                        + RenderGroups[i].TRenderProgressBar.Value.ToString + ' / ' + RenderGroups[i].TRenderProgressBar.Max.ToString + ')';
          except
            on Exception do
              RenderGroups[i].TRenderProgressBar.Value := 0;
          end;
        end;

        //Try to assign all the known frames to total progressbar value
        var TotalFrames: Single := 1;
          for var j := 0 to High(RenderGroups) do
            if RenderGroups[j].TRenderProgressBar.Max <> 1 then
              TotalFrames := TotalFrames + RenderGroups[j].TRenderProgressBar.Max;

        if TotalProgressBar.Max <> TotalFrames then begin
          TotalProgressBar.Max := TotalFrames;
        end else begin
          var sum: Single := 0;
          for var j := 0 to High(RenderGroups) do
            sum := sum + RenderGroups[j].TRenderProgressBar.Value;

          TotalProgressBar.Value := sum;
          totalProgressPercentage.Text := Round((TotalProgressBar.Value / TotalProgressBar.Max) * 100).ToString + '%';
          if (TotalProgressBar.Max = 0) or (TotalProgressBar.Max = 1) then
            framesLabel.Text := 'Waiting for aerender...'
          else
            framesLabel.Text := TotalProgressBar.Value.ToString + ' / ' + (TotalProgressBar.Max - 1).ToString;
        end;
        if RenderGroups[i].TLogMemo.Text.Contains('Finished composition') then
          begin
            Render[i].State := 'finish';
            RenderGroups[i].TRenderProgressLabel.Text := 'Finished';
            RenderGroups[i].TRenderProgressBar.Value := RenderGroups[i].TRenderProgressBar.Max;
          end;
        if RenderGroups[i].TLogMemo.Text.Contains('aerender ERROR') or RenderGroups[i].TLogMemo.Text.Contains('aerender Error') then
          begin
            Render[i].State := 'error';
            RenderGroups[i].TRenderProgressLabel.Text := 'ERROR: See log for more info';
            RenderGroups[i].TRenderProgressBar.Value := RenderGroups[i].TRenderProgressBar.Max;
            RenderGroups[i].TRenderProgressBar.StyleLookup := 'progressbarerrorstyle'
          end;
      finally
        Render[i].LogFile.Free;
      end;
    end;

    for var j := 0 to High(Render) do
      if Render[j].State = 'finish' then
        inc (Finished);

    for var j := 0 to High(Render) do
      if Render[j].State = 'error' then
        inc (Error);


    if (Finished = Length(LogFiles)) or (Error = Length(LogFiles)) then
      begin
        Sleep (2000);
        TotalProgressBar.Value := TotalProgressBar.Max;
        totalProgressPercentage.Text := '100%';
        renderingTimer.Enabled := False;
        StopwatchTimer.Enabled := False;
      end;
end;

procedure TRenderingForm.ShowLogButtonClick (Sender: TObject);
begin
  var visibleMemos: Integer := 0;
  if RenderGroups[TButton(Sender).Tag].TLogMemo.Visible = False then
    begin
      RenderGroups[TButton(Sender).Tag].TLogMemo.Visible := True;
      RenderGroups[TButton(Sender).Tag].TRenderGroupBox.Height := RenderGroups[TButton(Sender).Tag].TRenderGroupBox.Height + 175;

      for var i := 0 to High(RenderGroups) do
        if RenderGroups[i].TLogMemo.Visible = True then
          inc (visibleMemos);
      if visibleMemos <> 0 then
        LogIncrement := 175
      else
        LogIncrement := 0;
    end
  else
    begin
      RenderGroups[TButton(Sender).Tag].TLogMemo.Visible := False;
      RenderGroups[TButton(Sender).Tag].TRenderGroupBox.Height := RenderGroups[TButton(Sender).Tag].TRenderGroupBox.Height - 175;

      for var i := 0 to High(RenderGroups) do
        if RenderGroups[i].TLogMemo.Visible = True then
          inc (visibleMemos);
      if visibleMemos <> 0 then
        LogIncrement := 175
      else
        LogIncrement := 0;
    end;
end;

procedure TRenderingForm.StopwatchTimerTimer(Sender: TObject);
begin
  timeElapsedLabel.Text := 'Time Elapsed: ' + FormatDateTime('hh:nn:ss', Now - CurrentTime);
end;

end.
