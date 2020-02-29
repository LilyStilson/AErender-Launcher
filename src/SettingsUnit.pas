unit SettingsUnit;

(*        AErender Launcher                                                                 *)
(*        SettingsUnit.pas                                                                  *)
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
  {$REGION '    System Namespaces    '}
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  System.Character,
  {$ENDREGION}

  {$REGION '    FMX Namespaces    '}
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
  {$ENDREGION}

  {$REGION '  Windows Only Libraries  '}{$IFDEF MSWINDOWS}
    Winapi.ShellAPI, Winapi.Windows, FMX.Platform.Win, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}{$ENDREGION}

  {$REGION '  macOS Only Libraries  '}{$IFDEF MACOS}
    Posix.Stdlib, Posix.Unistd, Posix.SysSysctl, Posix.SysTypes;
  {$ENDIF MACOS}{$ENDREGION}

type
  TSettingsForm = class(TForm)
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
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
  {$REGION '    AErenderLauncher Liraries    '}
  MainUnit,
  HelpUnit,
  ImportUnit,
  AboutUnit,
  FFMPEGUnit,
  RenderingUnit,
  OutputModuleEditorUnit;
  {$ENDREGION}

{$R *.fmx}

{$IFDEF MSWINDOWS}
procedure TSettingsForm.CreateHandle;
begin
  inherited CreateHandle;

  var hWnd: HWND := FormToHWND(Self);

  SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);
end;
{$ENDIF MSWINDOWS}

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
  //ver := maxVer;
  Result := maxVerStr;
end;

procedure TSettingsForm.aerenderPathSelectClick(Sender: TObject);
begin
  With OpenDialog1 do
    if Execute then
      begin
        aerenderPath.Text := OpenDialog1.FileName;
        AERPATH := OpenDialog1.FileName;
      end;
end;

procedure TSettingsForm.Button1Click(Sender: TObject);
begin
  {if not aerenderPath.Text.Contains('aerender') then
    if MainUnit.LANG = 'EN' then
      MessageDlg(('Please specify valid Adobe After Effects render engine path!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
    else
      MessageDlg(('Пожалуйста, укажите путь к модулю рендеринга After Effects!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
  else        }
    begin
      SettingsForm.Close;
      AERPATH := aerenderPath.Text;
      MainForm.AEPOpenDialog.InitialDir := defaultProjectsPath.Text;
      MainForm.SaveDialog1.InitialDir := defaultOutputPath.Text;
    end;
end;

procedure TSettingsForm.Button2Click(Sender: TObject);
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

procedure TSettingsForm.Button3Click(Sender: TObject);
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

procedure TSettingsForm.delFilesCheckBoxChange(Sender: TObject);
begin
  MainUnit.DelTempFiles := BoolToStr(delFilesCheckBox.IsChecked, True);
end;

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  {if not aerenderPath.Text.Contains('aerender') then
    begin
      if MainUnit.LANG = 'EN' then
        MessageDlg(('Please specify the Adobe After Effects render engine path!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
      else
        MessageDlg(('Пожалуйста, укажите путь к модулю рендеринга After Effects!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      CanClose := False;
    end
  else}
    begin
      CanClose := True;
      SettingsForm.Close;
      AERPATH := aerenderPath.Text;
      DEFPRGPATH := defaultProjectsPath.Text;
      DEFOUTPATH := defaultOutputPath.Text;
    end;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  try
    if AERPATH.IsEmpty then
      AERPATH := DetectAerender + {$IFDEF MSWINDOWS}'\Support Files\aerender.exe'{$ENDIF MSWINDOWS}
                                  {$IFDEF MACOS}'/aerender'{$ENDIF MACOS};
  except
    on Exception do
      AERPATH := '';
  end;

  {$IFDEF MSWINDOWS}OpenDialog1.InitialDir := 'C:\Program Files\Adobe';{$ENDIF MSWINDOWS}
  {$IFDEF MACOS}OpenDialog1.InitialDir := '/Applications'{$ENDIF MACOS};
end;

procedure TSettingsForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = scAlt then
    begin
      ResetButton.Visible := True;
      ResetButton.Enabled := True;
    end;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  aerenderPath.Text := AERPATH;
  defaultProjectsPath.Text := DEFPRGPATH;
  defaultOutputPath.Text := DEFOUTPATH;
  HandleCheckBox.IsChecked := StrToBool(AERH);
  delFilesCheckBox.IsChecked := StrToBool(DelTempFiles);
  if LANG = 'EN' then
    langBox.ItemIndex := 0;
  if LANG = 'RU' then
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

procedure TSettingsForm.HandleCheckBoxChange(Sender: TObject);
begin
  if HandleCheckBox.IsChecked then
    begin
      AERH := 'True';
      RenderingForm.emptyLabel.Text := 'Queue is Empty'
    end
  else
    begin
      AERH := 'False';
      RenderingForm.emptyLabel.Text := 'Aerender handle disabled. Enable aerender handle in Launcher settings.';
    end;
end;

procedure TSettingsForm.langBoxChange(Sender: TObject);
begin
  case langBox.ItemIndex of
    0:begin
        LANG := 'EN';
        MainForm.Lang1.Lang := 'EN';
        if MainForm.compSwitch.IsChecked then
          MainForm.compSwitchLabel.Text := 'Multiple Compositions'
        else
          MainForm.compSwitchLabel.Text := 'Single Composition';
        if MainForm.threadsSwitch.IsChecked then
          MainForm.threadsSwitchLabel.Text := 'Split Render'
        else
          MainForm.threadsSwitchLabel.Text := 'Single Render';
      end;
    1:begin
        LANG := 'RU';
        MainForm.Lang1.Lang := 'RU';
        if MainForm.compSwitch.IsChecked then
          MainForm.compSwitchLabel.Text := 'Несколько композиций'
        else
          MainForm.compSwitchLabel.Text := 'Одна композиция';
        if MainForm.threadsSwitch.IsChecked then
          MainForm.threadsSwitchLabel.Text := 'Рендерить частями'
        else
          MainForm.threadsSwitchLabel.Text := 'Рендерить одним файлом';
      end;
  end;
end;

procedure TSettingsForm.onRenderStartBoxChange(Sender: TObject);
begin
  ONRENDERSTART := onRenderStartBox.ItemIndex;
end;

procedure TSettingsForm.ResetButtonClick(Sender: TObject);
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

procedure TSettingsForm.styleBoxChange(Sender: TObject);
begin
  case styleBox.ItemIndex of
    0:begin
        STYLE := 0;
        MainForm.StyleBook := MainForm.AERModernStyle;

        MainForm.SettingsIcon.Fill.Color := $FFFFFFFF;
        MainForm.InfoIcon.Fill.Color := $FFFFFFFF;
        MainForm.LaunchIcon.Fill.Color := $FFFFFFFF;

        MainForm.memUsageTrackBar.Margins.Top := 5;
        MainForm.cacheUsageTrackBar.Margins.Top := 5;
        SettingsForm.StyleBook := MainForm.AERModernStyle;
        HelpForm.StyleBook := MainForm.AERModernStyle;
        ImportForm.StyleBook := MainForm.AERModernStyle;
        AboutForm.StyleBook := MainForm.AERModernStyle;
        FFMPEGForm.StyleBook := MainForm.AERModernStyle;
        RenderingForm.StyleBook := MainForm.AERModernStyle;
        OutputModuleEditorForm.StyleBook := MainForm.AERModernStyle;
      end;
    1:begin
        STYLE := 1;
        MainForm.StyleBook := MainForm.AERModernAnimatedStyle;

        MainForm.SettingsIcon.Fill.Color := $FFFFFFFF;
        MainForm.InfoIcon.Fill.Color := $FFFFFFFF;
        MainForm.LaunchIcon.Fill.Color := $FFFFFFFF;

        MainForm.memUsageTrackBar.Margins.Top := 5;
        MainForm.cacheUsageTrackBar.Margins.Top := 5;
        SettingsForm.StyleBook := MainForm.AERModernAnimatedStyle;
        HelpForm.StyleBook := MainForm.AERModernAnimatedStyle;
        ImportForm.StyleBook := MainForm.AERModernAnimatedStyle;
        AboutForm.StyleBook := MainForm.AERModernAnimatedStyle;
        FFMPEGForm.StyleBook := MainForm.AERModernAnimatedStyle;
        RenderingForm.StyleBook := MainForm.AERModernAnimatedStyle;
        OutputModuleEditorForm.StyleBook := MainForm.AERModernAnimatedStyle;
      end;
    2:begin
        STYLE := 2;
        MainForm.StyleBook := MainForm.OnyxBlueStyle;

        MainForm.SettingsIcon.Fill.Color := $FF000000;
        MainForm.InfoIcon.Fill.Color := $FF000000;
        MainForm.LaunchIcon.Fill.Color := $FF000000;

        MainForm.memUsageTrackBar.Margins.Top := 0;
        MainForm.cacheUsageTrackBar.Margins.Top := 0;
        SettingsForm.StyleBook := MainForm.OnyxBlueStyle;
        HelpForm.StyleBook := MainForm.OnyxBlueStyle;
        ImportForm.StyleBook := MainForm.OnyxBlueStyle;
        AboutForm.StyleBook := MainForm.OnyxBlueStyle;
        FFMPEGForm.StyleBook := MainForm.OnyxBlueStyle;
        RenderingForm.StyleBook := MainForm.OnyxBlueStyle;
        OutputModuleEditorForm.StyleBook := MainForm.OnyxBlueStyle;
      end;
  end;
end;

end.
