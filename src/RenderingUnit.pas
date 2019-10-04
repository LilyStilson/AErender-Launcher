unit RenderingUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo,
  {$IFDEF MSWINDOWS}
    FMX.Platform.Win, Winapi.Windows, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}

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
    procedure ShowLogButtonClick (Sender: TObject);
    procedure renderingTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure abortRenderingButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;
  TRenderGroup = record
    TRenderGroupBox: TGroupBox;
      TRenderGroupBoxMainLayout: TLayout;
        TRenderProgressBar: TProgressBar;
        TRenderProgressLabel: TLabel;
        TRenderShowLogButton: TButton;
      TLogMemo: TMemo;
  end;

var
  RenderingForm: TRenderingForm;
  RenderGroups: TArray<TRenderGroup>;
  LogIncrement: Integer = 0;

implementation

{$R *.fmx}

uses
  Unit1;

{$IFDEF MSWINDOWS}
function KillProcess(ExeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
      Result := Integer(TerminateProcess(
                        OpenProcess(PROCESS_TERMINATE,
                                    BOOL(0),
                                    FProcessEntry32.th32ProcessID),
                                    0));
     ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

procedure TRenderingForm.CreateHandle;
begin
  inherited CreateHandle;

  SetWindowLong(WindowHandleToPlatform(Handle).Wnd, GWL_EXSTYLE,
    GetWindowLong(WindowHandleToPlatform(Handle).Wnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
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
  if Length(RenderGroups) <> 0 then
    begin
      KillProcess('AfterFX.com');
      for var i := 0 to High(RenderGroups) do
        begin
          RenderGroups[i].TLogMemo.Free;
          RenderGroups[i].TRenderShowLogButton.Free;
          RenderGroups[i].TRenderProgressLabel.Free;
          RenderGroups[i].TRenderProgressBar.Free;
          RenderGroups[i].TRenderGroupBoxMainLayout.Free;
          RenderGroups[i].TRenderGroupBox.Free;

          {$IFDEF MSWNDOWS}DeleteFile(Unit1.LogFiles[i]);{$ENDIF MSWINDOWS}
        end;
      emptyLabel.Visible := True;
      emptyLabel.Enabled := True;
      //renderLayout.Height := 80;
      renderingTimer.Enabled := False;
    end
  else
    ShowMessage ('Nothing to abort!')
end;
{
procedure ResizeFlowLayout (FlowLayout: TFlowLayout; BaseWidth, BaseHeight: Single; Count: Integer);
begin
  var Lines := Trunc(Count / Trunc(FlowLayout.Width / BaseWidth));

  FlowLayout.Height := BaseHeight * Lines;
end;
}
procedure TRenderingForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if TotalProgressBar.Value = TotalProgressBar.Max then
    begin
      abortRenderingButtonClick(Sender);
      TotalProgressBar.Value := 0;
    end;
end;

procedure TRenderingForm.FormResize(Sender: TObject);
begin
  //ResizeFlowLayout(renderLayout, 400, 80 + LogIncrement, Length(RenderGroups));
end;

procedure TRenderingForm.FormShow(Sender: TObject);
begin
  if Unit1.RenderWindowSender = Form1.launchButton then
    begin
      if Length(Unit1.LogFiles) = 0 then
        begin
          emptyLabel.Visible := True;
          emptyLabel.Enabled := True;
        end
      else
        begin
          emptyLabel.Visible := False;
          emptyLabel.Enabled := False;

          SetLength (RenderGroups, Length(Unit1.LogFiles));

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
              RenderGroups[i].TRenderGroupBox.Text := Unit1.LogFiles[i].Remove(Unit1.LogFiles[i].Length - 4);
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
              if Form1.threadsSwitch.IsChecked then
                RenderGroups[i].TRenderProgressBar.Max := Form1.threadsGrid.Cells[1, i].ToSingle() - Form1.threadsGrid.Cells[0, i].ToSingle() + 50
              else
                if Form1.outFrame.Text.IsEmpty or Form1.compSwitch.IsChecked then
                  RenderGroups[i].TRenderProgressBar.Max := 1
                else
                  RenderGroups[i].TRenderProgressBar.Max := Form1.outFrame.Text.ToSingle() + 50;

              //Initialise GroupBox MainLayout ProgressLabel
              RenderGroups[i].TRenderProgressLabel := TLabel.Create(Self);
              RenderGroups[i].TRenderProgressLabel.Parent := RenderGroups[i].TRenderGroupBoxMainLayout;
              RenderGroups[i].TRenderProgressLabel.Align := TAlignLayout.Client;
              RenderGroups[i].TRenderProgressLabel.Margins.Left := 5;
              //RenderGroups[i].TRenderProgressLabel.Width := 100;
              RenderGroups[i].TRenderProgressLabel.AutoSize := False;
              RenderGroups[i].TRenderProgressLabel.TextSettings.WordWrap := False;
              if Form1.threadsSwitch.IsChecked then
                RenderGroups[i].TRenderProgressLabel.Text := '0%'
              else
                if Form1.outFrame.Text.IsEmpty then
                  RenderGroups[i].TRenderProgressLabel.Text := 'N/A'
                else
                  RenderGroups[i].TRenderProgressLabel.Text := '0%';

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
    end;
  FormResize(Sender);
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
  SetLength (Render, Length(Unit1.LogFiles));

  for var j := 0 to High(Render) do
    begin
      Render[j].Data := TStrings.Create;
      Render[j].State := '';
    end;

  for i := 0 to High(Render) do
    begin
      var sum: Integer := 0;
      Render[i].LogFile := TStringList.Create;
      Render[i].LogFile.Encoding.UTF8;

      try
        Render[i].Stream := TFileStream.Create(LogFiles[i], fmOpenRead or fmShareDenyNone);
        try
          Render[i].LogFile.LoadFromStream(Render[i].Stream);
        finally
          Render[i].Stream.Free;
        end;

        if Render[i].LogFile.Count > RenderGroups[i].TLogMemo.Lines.Count then
          begin
            if Form1.threadsSwitch.IsChecked or (not Form1.compSwitch.IsChecked and not Form1.outFrame.Text.IsEmpty) then
              begin
                RenderGroups[i].TRenderProgressBar.Value := Render[i].LogFile.Count;
                RenderGroups[i].TRenderProgressLabel.Text := Round((RenderGroups[i].TRenderProgressBar.Value / RenderGroups[i].TRenderProgressBar.Max) * 100).ToString + '%';
              end
            else
              if Form1.outFrame.Text.IsEmpty then
                begin
                  RenderGroups[i].TRenderProgressLabel.Text := 'Rendering';
                end
              else
                RenderGroups[i].TRenderProgressLabel.Text := Round((RenderGroups[i].TRenderProgressBar.Value / RenderGroups[i].TRenderProgressBar.Max) * 100).ToString + '%';
            RenderGroups[i].TLogMemo.Lines.Add(Render[i].LogFile[RenderGroups[i].TLogMemo.Lines.Count]);
            RenderGroups[i].TLogMemo.GoToTextEnd;
          end;

        for var j := 0 to High(RenderGroups) do
          if (Form1.threadsSwitch.IsChecked) and (not Form1.outFrame.Text.IsEmpty) then
            inc (sum, LimitInt(RenderGroups[j].TRenderProgressBar.Value.ToString.ToInteger))
          else
            inc (sum, RenderGroups[j].TRenderProgressBar.Value.ToString.ToInteger);

        TotalProgressBar.Value := sum;
        totalProgressPercentage.Text := Round((TotalProgressBar.Value / TotalProgressBar.Max) * 100).ToString + '%';

        {framesLabel.Text := 'tpb.max = ' + TotalProgressBar.max.ToString + '; '
                          + 'tpb.val = ' + TotalProgressBar.Value.ToString + '; '
                          + 'rg[0].val = ' + RenderGroups[0].TRenderProgressBar.Value.ToString + '; '
                          + 'rg[0].max = ' + RenderGroups[0].TRenderProgressBar.Max.ToString + '; '
                          + 'rg[1].val = ' + RenderGroups[1].TRenderProgressBar.Value.ToString + '; '
                          + 'rg[1].max = ' + RenderGroups[1].TRenderProgressBar.Max.ToString + '; ';}

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

    if Finished = Length(LogFiles) then
      begin
        Sleep (2000);
        TotalProgressBar.Value := TotalProgressBar.Max;
        totalProgressPercentage.Text := '100%';
        renderingTimer.Enabled := False;
      end;
end;

procedure TRenderingForm.ShowLogButtonClick (Sender: TObject);
begin
  var visibleMemos: Integer := 0;
  if RenderGroups[TButton(Sender).Tag].TLogMemo.Visible = False then
    begin
      RenderGroups[TButton(Sender).Tag].TLogMemo.Visible := True;
      RenderGroups[TButton(Sender).Tag].TRenderGroupBox.Height := RenderGroups[TButton(Sender).Tag].TRenderGroupBox.Height + 175;
      //renderLayout.ItemHeight := 250;

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
      //renderLayout.ItemHeight := 75;

      for var i := 0 to High(RenderGroups) do
        if RenderGroups[i].TLogMemo.Visible = True then
          inc (visibleMemos);
      if visibleMemos <> 0 then
        LogIncrement := 175
      else
        LogIncrement := 0;
    end;
  {FormResize(Sender);
  RenderingForm.Width := RenderingForm.Width - 1;
  RenderingForm.Width := RenderingForm.Width + 1;       }
end;

end.
