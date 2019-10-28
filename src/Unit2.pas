unit Unit2;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  System.Character,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.DialogService.Sync,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,

  {$REGION '  Windows Only Libraries  '}{$IFDEF MSWINDOWS}
    Winapi.ShellAPI, Winapi.Windows, FMX.Platform.Win, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}{$ENDREGION}

  {$REGION '  macOS Only Libraries  '}{$IFDEF MACOS}
    Posix.Stdlib, Posix.Unistd, Posix.SysSysctl, Posix.SysTypes;
  {$ENDIF MACOS}{$ENDREGION}

type
  TForm2 = class(TForm)
    langLabel: TLabel;
    langBox: TComboBox;
    Button1: TButton;
    langLayout: TLayout;
    aerenderPathLayout: TLayout;
    Label1: TLabel;
    aerenderPath: TEdit;
    aerenderPathSelect: TButton;
    OpenDialog1: TOpenDialog;
    defaultProjectsLayout: TLayout;
    Label2: TLabel;
    defaultProjectsPath: TEdit;
    Button2: TButton;
    defaultOutputLayout: TLayout;
    Label3: TLabel;
    defaultOutputPath: TEdit;
    Button3: TButton;
    styleLayout: TLayout;
    styleBox: TComboBox;
    styleLabel: TLabel;
    onRenderStartLayout: TLayout;
    onRenderStartBox: TComboBox;
    onRenderStartLabel: TLabel;
    HandleCheckBox: TCheckBox;
    Layout2: TLayout;
    Layout3: TLayout;
    delFilesCheckBox: TCheckBox;
    ResetButton: TButton;
    StatusBar1: TStatusBar;
    procedure langBoxChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aerenderPathSelectClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure styleBoxChange(Sender: TObject);
    procedure onRenderStartBoxChange(Sender: TObject);
    procedure HandleCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure delFilesCheckBoxChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure ResetButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Unit1, Unit3, Unit4, Unit5, Unit6, RenderingUnit, OutputModuleEditor;

function ExtractIntegerFromString(s: String): Integer;
var
  tempstr: String;
begin
  tempstr := '';
  for var i := 1 to Length(s) do
    begin
      if s[i].IsDigit then
        tempstr := tempstr + s[i];
    end;
  Result := tempstr.ToInteger;
end;

function DetectAerender: String;
var
  AdobeFolder, AEVersions: TArray<String>;
  maxVer: Integer;
  maxVerStr, AdobeFolderPath: String;
begin
  {$IFDEF MSWINDOWS}AdobeFolderPath := 'C:\Program Files\Adobe\';{$ENDIF MSWINDOWS}
  {$IFDEF MACOS}AdobeFolderPath := '/Applications/';{$ENDIF MACOS}

  maxVer := 0;
  AdobeFolder := TDirectory.GetDirectories(AdobeFolderPath);
  for var i := 0 to High(AdobeFolder) do
    begin
      if AdobeFolder[i].Contains ('After Effects') then
        begin
          SetLength(AEVersions, i + 1);
          AEVersions[i] := AdobeFolder[i];
        end;
    end;

  for var i := 0 to High(AEVersions) do
    begin
      if ExtractIntegerFromString(AEVersions[i]) > maxVer then
        begin
          maxVer := ExtractIntegerFromString(AEVersions[i]);
          maxVerStr := AEVersions[i];
        end;
    end;
  Result := maxVerStr;
end;

procedure TForm2.aerenderPathSelectClick(Sender: TObject);
begin
  With OpenDialog1 do
    if Execute then
      begin
        aerenderPath.Text := OpenDialog1.FileName;
        AERPATH := OpenDialog1.FileName;
      end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  {if not aerenderPath.Text.Contains('aerender') then
    if Unit1.LANG = 'EN' then
      MessageDlg(('Please specify valid Adobe After Effects render engine path!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
    else
      MessageDlg(('Пожалуйста, укажите путь к модулю рендеринга After Effects!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
  else        }
    begin
      Form2.Close;
      AERPATH := aerenderPath.Text;
      DEFPRGPATH := defaultProjectsPath.Text;
      DEFOUTPATH := defaultOutputPath.Text;
    end;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  PATH: String;
begin
  {$IFDEF MSWINDOWS}
    SelectDirectory ('Select Default Projects Directory', '%USERPROFILE%\Documents', PATH);
    defaultProjectsPath.Text := PATH;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    SelectDirectory ('Select Default Projects Directory', '~/Documents', PATH);
    defaultProjectsPath.Text := PATH;
  {$ENDIF MACOS}
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  PATH: String;
begin
  {$IFDEF MSWINDOWS}
    SelectDirectory ('Select Default Output Directory', '%USERPROFILE%\Documents', PATH);
    defaultOutputPath.Text := PATH;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    SelectDirectory ('Select Default Output Directory', '~/Documents', PATH);
    defaultOutputPath.Text := PATH;
  {$ENDIF POSX}
end;

procedure TForm2.delFilesCheckBoxChange(Sender: TObject);
begin
  Unit1.DelTempFiles := BoolToStr(delFilesCheckBox.IsChecked, True);
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  {if not aerenderPath.Text.Contains('aerender') then
    begin
      if Unit1.LANG = 'EN' then
        MessageDlg(('Please specify the Adobe After Effects render engine path!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
      else
        MessageDlg(('Пожалуйста, укажите путь к модулю рендеринга After Effects!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      CanClose := False;
    end
  else}
    begin
      CanClose := True;
      Form2.Close;
      AERPATH := aerenderPath.Text;
      DEFPRGPATH := defaultProjectsPath.Text;
      DEFOUTPATH := defaultOutputPath.Text;
    end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  try
    if AERPATH.IsEmpty then
      AERPATH := DetectAerender + {$IFDEF MSWINDOWS}'\Support Files\aerender.exe'{$ENDIF MSWINDOWS}
                                  {$IFDEF MACOS}'/aerender'{$ENDIF MACOS};
  except
    on Exception do
      AERPATH := '';
  end;
end;

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = scAlt then
    begin
      ResetButton.Visible := True;
      ResetButton.Enabled := True;
    end;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  aerenderPath.Text := AERPATH;
  defaultProjectsPath.Text := DEFPRGPATH;
  defaultOutputPath.Text := DEFOUTPATH;
  HandleCheckBox.IsChecked := Unit1.AERH.ToBoolean();
  delFilesCheckBox.IsChecked := Unit1.DelTempFiles.ToBoolean();
  if Unit1.LANG = 'EN' then
    langBox.ItemIndex := 0;
  if Unit1.LANG = 'RU' then
    langBox.ItemIndex := 1;
  {$IFDEF MSWINDOWS}
    aerenderPath.TextPrompt := 'C:\Program Files\Adobe\Adobe After Effects CC\Support Files\aerender.exe';
    OpenDialog1.Filter := 'After Effects Render Engine|aerender.exe';
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    aerenderPath.TextPrompt := '/Applications/Adobe After Effects CC/aerender';
    OpenDialog1.Filter := 'After Effects Render Engine|aerender';
  {$ENDIF MACOS}
end;

procedure TForm2.HandleCheckBoxChange(Sender: TObject);
begin
  if HandleCheckBox.IsChecked then
    begin
      Unit1.AERH := 'True';
      RenderingForm.emptyLabel.Text := 'Queue is Empty'
    end
  else
    begin
      Unit1.AERH := 'False';
      RenderingForm.emptyLabel.Text := 'Aerender handle disabled. Enable aerender handle in Launcher settings.';
    end;
end;

procedure TForm2.langBoxChange(Sender: TObject);
begin
  case langBox.ItemIndex of
    0:begin
        Unit1.LANG := 'EN';
        Form1.Lang1.Lang := 'EN';
        if Form1.compSwitch.IsChecked then
          Form1.compSwitchLabel.Text := 'Multiple Compositions'
        else
          Form1.compSwitchLabel.Text := 'Single Composition';
        if Form1.threadsSwitch.IsChecked then
          Form1.threadsSwitchLabel.Text := 'Split Render'
        else
          Form1.threadsSwitchLabel.Text := 'Single Render';
      end;
    1:begin
        Unit1.LANG := 'RU';
        Form1.Lang1.Lang := 'RU';
        if Form1.compSwitch.IsChecked then
          Form1.compSwitchLabel.Text := 'Несколько композиций'
        else
          Form1.compSwitchLabel.Text := 'Одна композиция';
        if Form1.threadsSwitch.IsChecked then
          Form1.threadsSwitchLabel.Text := 'Рендерить частями'
        else
          Form1.threadsSwitchLabel.Text := 'Рендерить одним файлом';
      end;
  end;
end;

procedure TForm2.onRenderStartBoxChange(Sender: TObject);
begin
  Unit1.ONRENDERSTART := onRenderStartBox.ItemIndex;
end;

procedure TForm2.ResetButtonClick(Sender: TObject);
begin
  if (TDialogServiceSync.MessageDialog(('Launcher configuration will be renewed. This will delete all your setting and output modules. Application will be restarted'
                                        + #13#10 + 'Proceed?'), TMsgDlgType.mtWarning, mbOKCancel, TMsgDlgBtn.mbOK, 0) = 1) then
    begin
      InitConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
      {$IFDEF MSWINDOWS}ShellExecute(0, 'OPEN', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);{$ENDIF MSWINDOWS}
      {$IFDEF MACOS}_system(PAnsiChar('open "' + AnsiString(ParamStr(0)) + '" &'));{$ENDIF MACOS}
    end;
  Application.Terminate;
end;

procedure TForm2.styleBoxChange(Sender: TObject);
begin
  case styleBox.ItemIndex of
    0:begin
        Unit1.STYLE := 0;
        Form1.StyleBook := Form1.AERModernStyle;
        Form1.settingsIconFill.Color := $FFFFFFFF;
        Form1.infoIconFill.Color := $FFFFFFFF;
        Form1.memUsageTrackBar.Margins.Top := 5;
        Form1.cacheUsageTrackBar.Margins.Top := 5;
        Form2.StyleBook := Form1.AERModernStyle;
        Form3.StyleBook := Form1.AERModernStyle;
        Form4.StyleBook := Form1.AERModernStyle;
        Form5.StyleBook := Form1.AERModernStyle;
        Form6.StyleBook := Form1.AERModernStyle;
        RenderingForm.StyleBook := Form1.AERModernStyle;
        OutputModuleEditorForm.StyleBook := Form1.AERModernStyle;
      end;
    1:begin
        Unit1.STYLE := 1;
        Form1.StyleBook := Form1.AERModernAnimatedStyle;
        Form1.settingsIconFill.Color := $FFFFFFFF;
        Form1.infoIconFill.Color := $FFFFFFFF;
        Form1.memUsageTrackBar.Margins.Top := 5;
        Form1.cacheUsageTrackBar.Margins.Top := 5;
        Form2.StyleBook := Form1.AERModernAnimatedStyle;
        Form3.StyleBook := Form1.AERModernAnimatedStyle;
        Form4.StyleBook := Form1.AERModernAnimatedStyle;
        Form5.StyleBook := Form1.AERModernAnimatedStyle;
        Form6.StyleBook := Form1.AERModernAnimatedStyle;
        RenderingForm.StyleBook := Form1.AERModernAnimatedStyle;
        OutputModuleEditorForm.StyleBook := Form1.AERModernAnimatedStyle;
      end;
    2:begin
        Unit1.STYLE := 2;
        Form1.StyleBook := Form1.OnyxBlueStyle;
        Form1.settingsIconFill.Color := $FF000000;
        Form1.infoIconFill.Color := $FF000000;
        Form1.memUsageTrackBar.Margins.Top := 0;
        Form1.cacheUsageTrackBar.Margins.Top := 0;
        Form2.StyleBook := Form1.OnyxBlueStyle;
        Form3.StyleBook := Form1.OnyxBlueStyle;
        Form4.StyleBook := Form1.OnyxBlueStyle;
        Form5.StyleBook := Form1.OnyxBlueStyle;
        Form6.StyleBook := Form1.OnyxBlueStyle;
        RenderingForm.StyleBook := Form1.OnyxBlueStyle;
        OutputModuleEditorForm.StyleBook := Form1.OnyxBlueStyle;
      end;
  end;
end;

end.
