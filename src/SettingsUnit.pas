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
  FMX.Objects,
  FMX.Menus,
  {$ENDREGION}

  {$REGION '  Windows Only Libraries  '}{$IFDEF MSWINDOWS}
    Winapi.ShellAPI, Winapi.Windows, FMX.Platform.Win, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}{$ENDREGION}

  {$REGION '  macOS Only Libraries  '}{$IFDEF MACOS}
    Posix.Stdlib, Posix.Unistd, Posix.SysSysctl, Posix.SysTypes,
    MacApi.Dialogs, Mac.CodeBlocks, FMX.Platform.Mac,
    Macapi.Appkit, Macapi.ObjectiveC, Macapi.Foundation, Macapi.Helpers, Macapi.ObjCRuntime, Macapi.CocoaTypes;
  {$ENDIF MACOS}{$ENDREGION}

type
  TSettingsForm = class(TForm)
    langLabel: TLabel;
    langBox: TComboBox;
    Button1: TButton;
    langLayout: TLayout;
    aerenderPathLayout: TLayout;
    aerenderPathLabel: TLabel;
    aerenderPath: TEdit;
    aerenderPathSelect: TButton;
    OpenDialog1: TOpenDialog;
    defaultProjectsLayout: TLayout;
    defProjPathLabel: TLabel;
    defaultProjectsPath: TEdit;
    Button2: TButton;
    defaultOutputLayout: TLayout;
    defOutPathLabel: TLabel;
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
    dpiChangeLayout: TLayout;
    Label4: TLabel;
    GridPanelLayout1: TGridPanelLayout;
    dpi100: TRadioButton;
    dpi125: TRadioButton;
    dpi150: TRadioButton;
    dpi200: TRadioButton;
    settingsFormLayout: TLayout;
    SettingsToolbar: TToolBar;
    WindowLabel: TLabel;
    refreshAerender: TButton;
    RefreshIcon: TPath;
    UIExpander: TExpander;
    BehaviourExpander: TExpander;
    langChangeLabel: TLabel;
    hiddenS: TPopupMenu;
    openConfig: TMenuItem;
    resetLanucher: TMenuItem;
    opSettingsLabel: TMenuItem;
    separatorItem5: TMenuItem;
    RenderingExpander: TExpander;
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
    procedure dpi125Change(Sender: TObject);
    procedure dpi100Change(Sender: TObject);
    procedure dpi150Change(Sender: TObject);
    procedure dpi200Change(Sender: TObject);
    procedure refreshAerenderClick(Sender: TObject);
    procedure SetLanguage(LanguageCode: Integer);
    procedure openConfigClick(Sender: TObject);
    procedure resetLanucherClick(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;
  procedure ChangeDPI(DPI: Single);

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

procedure ChangeDPI(DPI: Single);
begin
  var TempScale: Single := MainForm.mainLayout.Scale.X;
  var Scale: TPosition := TPosition.Create(TPointF.Create(DPI, DPI));

  if (TempScale > Scale.X) and (TempScale > Scale.Y) then
    begin
      MainForm.mainLayout.Scale := Scale;
      MainForm.Width := Round(MainForm.Width / TempScale);
      MainForm.Height := Round(MainForm.Height / TempScale);

      SettingsForm.settingsFormLayout.Scale := Scale;
      SettingsForm.Width := Round(SettingsForm.Width / TempScale);
      SettingsForm.Height := Round(SettingsForm.Height / TempScale);
    end
  else
    begin
      MainForm.mainLayout.Scale := Scale;
      MainForm.Width := Round(MainForm.Width * DPI);
      MainForm.Height := Round(MainForm.Height * DPI);

      SettingsForm.settingsFormLayout.Scale := Scale;
      SettingsForm.Width := Round(SettingsForm.Width * DPI);
      SettingsForm.Height := Round(SettingsForm.Height * DPI);
    end;
end;

function ExtractIntegerFromString(s: String): Integer;
var
  tempstr: String;
begin
  tempstr := '';
  for var i := 1 to Length(s) do begin
    if s[i].IsDigit then
      tempstr := tempstr + s[i];
  end;
  Result := tempstr.ToInteger;
end;

function DetectAerender: String;
var
  AdobeFolder: TArray<String>;
  maxVer: Integer;
  maxVerStr, AdobeFolderPath: String;
begin
  {$IFDEF MSWINDOWS}AdobeFolderPath := 'C:\Program Files\Adobe\';{$ENDIF MSWINDOWS}
  {$IFDEF MACOS}AdobeFolderPath := '/Applications/';{$ENDIF MACOS}

  maxVer := 0;
  AdobeFolder := TDirectory.GetDirectories(AdobeFolderPath, '*After Effects*');

  for var i := 0 to High(AdobeFolder) do begin
    if ExtractIntegerFromString(AdobeFolder[i]) > maxVer then begin
      maxVer := ExtractIntegerFromString(AdobeFolder[i]);
      maxVerStr := AdobeFolder[i];
    end;
  end;
  //ver := maxVer;
  if maxVerStr = '' then
    raise Exception.Create('Adobe After Effects may not be installed on this computer.')
  else
    Result := maxVerStr;
end;

procedure TSettingsForm.SetLanguage(LanguageCode: Integer);
begin
  SettingsForm.Caption    := Language[LanguageCode].SettingsForm.LauncherSettings;
  WindowLabel.Text        := Language[LanguageCode].SettingsForm.LauncherSettings;

  aerenderPathLabel.Text  := Language[LanguageCode].SettingsForm.RenderEnginePath;
  defProjPathLabel.Text   := Language[LanguageCode].SettingsForm.DefaultProjectsDirectory;
  defOutPathLabel.Text    := Language[LanguageCode].SettingsForm.DefaultOutputDirectory;

  UIExpander.Text         := Language[LanguageCode].SettingsForm.UserInterface;
  styleLabel.Text         := Language[LanguageCode].SettingsForm.Style;
  langLabel.Text          := Language[LanguageCode].SettingsForm.Language;

  BehaviourExpander.Text  := Language[LanguageCode].SettingsForm.Behaviour;
  HandleCheckBox.Text     := Language[LanguageCode].SettingsForm.HandleAerender;
  delFilesCheckBox.Text   := Language[LanguageCode].SettingsForm.DeleteTemporary;
  onRenderStartLabel.Text := Language[LanguageCode].SettingsForm.OnRenderStart;

  onRenderStartBox.Items.Clear;
  onRenderStartBox.Items.AddStrings([
    Language[LanguageCode].SettingsForm.DoNothing,
    Language[LanguageCode].SettingsForm.MinimizeLauncher,
    Language[LanguageCode].SettingsForm.CloseLauncher
  ]);
  onRenderStartBox.ItemIndex := ONRENDERSTART;

end;

procedure TSettingsForm.aerenderPathSelectClick(Sender: TObject);
{$IFDEF MACOS}
var
  FOpenFile: NSSavePanel;
  NSWin: NSWindow;
begin
  NSWin := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;

  FOpenFile := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);
  FOpenFile.setDirectory(StrToNSStr('/Applications'));
  FOpenFile.setAllowedFileTypes(ArrayToNSArray(['public.data']));
  FOpenFile.setPrompt(StrToNSStr('Select aerender'));

  objc_msgSendP2((FOpenFile as ILocalObject).GetObjectID,
                 sel_getUid(PAnsiChar('beginSheetModalForWindow:completionHandler:')),
                 (NSWin as ILocalObject).GetObjectID,
                 TObjCBlock.CreateBlockWithProcedure(
                 procedure (p1: NSInteger)
                 begin
                    if p1 = 0 then
                      // Handle
                    else begin
                      aerenderPath.Text := NSStrToStr(FOpenFile.URL.relativePath);
                      //AERPATH := OpenDialog1.FileName;
                    end;
                 end));
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
begin
  with OpenDialog1 do
    if Execute then
      begin
        aerenderPath.Text := OpenDialog1.FileName;
        //AERPATH := OpenDialog1.FileName;
      end;
{$ENDIF MSWINDOWS}
  AERPATH := aerenderPath.Text;
end;

procedure TSettingsForm.Button1Click(Sender: TObject);
begin
  SettingsForm.Close;
  //AERPATH := aerenderPath.Text;
  //MainForm.AEPOpenDialog.InitialDir := defaultProjectsPath.Text;
  //MainForm.SaveDialog1.InitialDir := defaultOutputPath.Text;
end;

procedure TSettingsForm.Button2Click(Sender: TObject);
{$IFDEF MACOS}
var
  FOpenFile: NSOpenPanel;
  NSWin: NSWindow;
begin
  NSWin := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;

  FOpenFile := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);
  FOpenFile.setDirectory(StrToNSStr('~/Documents'));
  //FOpenFile.setAllowedFileTypes(ArrayToNSArray(['aep']));
  //FOpenFile.setPrompt(StrToNSStr(''));
  FOpenFile.setCanChooseFiles(False);
  FOpenFile.setCanChooseDirectories(True);

  objc_msgSendP2((FOpenFile as ILocalObject).GetObjectID,
                 sel_getUid(PAnsiChar('beginSheetModalForWindow:completionHandler:')),
                 (NSWin as ILocalObject).GetObjectID,
                 TObjCBlock.CreateBlockWithProcedure(
                 procedure (p1: NSInteger)
                 begin
                    if p1 = 0 then
                      // Handle
                    else
                      defaultProjectsPath.Text := NSStrToStr(FOpenFile.URL.relativePath);
                 end));
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
var
  PATH: String;
begin
    SelectDirectory ('Select Default Projects Directory', '%USERPROFILE%\Documents', PATH);
    defaultProjectsPath.Text := PATH;
{$ENDIF MSWINDOWS}
end;

procedure TSettingsForm.Button3Click(Sender: TObject);
{$IFDEF MACOS}
var
  FOpenFile: NSOpenPanel;
  NSWin: NSWindow;
begin
  NSWin := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;

  FOpenFile := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);
  FOpenFile.setDirectory(StrToNSStr('~/Documents'));
  //FOpenFile.setAllowedFileTypes(ArrayToNSArray(['aep']));
  //FOpenFile.setPrompt(StrToNSStr(''));
  FOpenFile.setCanChooseFiles(False);
  FOpenFile.setCanChooseDirectories(True);

  objc_msgSendP2((FOpenFile as ILocalObject).GetObjectID,
                 sel_getUid(PAnsiChar('beginSheetModalForWindow:completionHandler:')),
                 (NSWin as ILocalObject).GetObjectID,
                 TObjCBlock.CreateBlockWithProcedure(
                 procedure (p1: NSInteger)
                 begin
                    if p1 = 0 then
                      // Handle
                    else
                      defaultOutputPath.Text := NSStrToStr(FOpenFile.URL.relativePath);
                 end));
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
var
  PATH: String;
begin
  SelectDirectory ('Select Default Output Directory', '%USERPROFILE%\Documents', PATH);
  defaultOutputPath.Text := PATH;
{$ENDIF MSWINDOWS}
end;

procedure TSettingsForm.delFilesCheckBoxChange(Sender: TObject);
begin
  MainUnit.DelTempFiles := BoolToStr(delFilesCheckBox.IsChecked, True);
end;

procedure TSettingsForm.dpi100Change(Sender: TObject);
begin
  if dpi100.IsChecked then
    ChangeDPI(1)
end;

procedure TSettingsForm.dpi125Change(Sender: TObject);
begin
  if dpi125.IsChecked then
    ChangeDPI(1.25)
end;

procedure TSettingsForm.dpi150Change(Sender: TObject);
begin
  if dpi150.IsChecked then
    ChangeDPI(1.5)
end;

procedure TSettingsForm.dpi200Change(Sender: TObject);
begin
  if dpi200.IsChecked then
    ChangeDPI(2)
end;

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  SettingsForm.Close;
  AERPATH := aerenderPath.Text;
  LANG := langBox.ItemIndex;
  DEFPRGPATH := defaultProjectsPath.Text;
  DEFOUTPATH := defaultOutputPath.Text;
  ONRENDERSTART := onRenderStartBox.ItemIndex;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
{var
   LangFiles: TArray<String>;}
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
  langBox.ItemIndex := LANG;
  onRenderStartBox.ItemIndex := ONRENDERSTART;
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
      RenderingForm.emptyLabel.Text := Language[LANG].RenderingForm.QueueIsEmpty;
    end
  else
    begin
      AERH := 'False';
      RenderingForm.emptyLabel.Text := Language[LANG].RenderingForm.HandleDisabled;
    end;
end;

procedure TSettingsForm.langBoxChange(Sender: TObject);
begin
  /// We need to have our langBox in focus, since this event
  /// will be invoked by the OnShow event of the form
  if (not langChangeLabel.Visible) and (langBox.IsFocused) then begin
    UIExpander.Height := 112;
    LANG := langBox.ItemIndex;
    langChangeLabel.Text := Language[LANG].SettingsForm.LangChange;
    langChangeLabel.Visible := True;
  end;
  //MainUnit.ChangeLanguage(langBox.ItemIndex);
end;

procedure TSettingsForm.onRenderStartBoxChange(Sender: TObject);
begin
  //ONRENDERSTART := onRenderStartBox.ItemIndex;
end;

procedure TSettingsForm.refreshAerenderClick(Sender: TObject);
begin
  try
    aerenderPath.Text := DetectAerender + {$IFDEF MSWINDOWS}'\Support Files\aerender.exe'{$ENDIF MSWINDOWS}   {$IFDEF MACOS}'/aerender'{$ENDIF MACOS};
  except
    on E: Exception do begin
      TDialogServiceSync.MessageDialog((Language[LANG].Errors.aerenderUndetectable + #13#10 + E.ToString()), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0)
    end;
  end;
end;

procedure TSettingsForm.openConfigClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PWideChar(APPFOLDER + 'AErenderConfiguration.xml'), nil, nil, SW_SHOW);
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    _system(PAnsiChar('open ' + AnsiString('"' + APPFOLDER + 'AErenderConfiguration.xml' + '"')));
  {$ENDIF MACOS}
end;

procedure TSettingsForm.resetLanucherClick(Sender: TObject);
begin
  if (TDialogServiceSync.MessageDialog(('Launcher configuration will be renewed. This will delete all your setting and output modules. Application will be restarted'
                                        + #13#10 + 'This action is irreversible! Proceed?'), TMsgDlgType.mtWarning, mbOKCancel, TMsgDlgBtn.mbOK, 0) = 1) then
    begin
      InitConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
      {$IFDEF MSWINDOWS}ShellExecute(0, 'OPEN', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);{$ENDIF MSWINDOWS}
      {$IFDEF MACOS}_system(PAnsiChar('open "' + AnsiString(ParamStr(0)) + '" &'));{$ENDIF MACOS}
      Halt;
    end;
end;

procedure TSettingsForm.styleBoxChange(Sender: TObject);
begin
  case styleBox.ItemIndex of
    0:begin
        STYLE := 0;

        MainForm.SettingsIcon.Fill.Color := $FFFFFFFF;
        MainForm.InfoIcon.Fill.Color := $FFFFFFFF;
        MainForm.LaunchIcon.Fill.Color := $FFFFFFFF;
        SettingsForm.RefreshIcon.Fill.Color := $FFFFFFFF;

        MainForm.memUsageTrackBar.Margins.Top := 5;
        MainForm.cacheUsageTrackBar.Margins.Top := 5;

        MainForm.StyleBook := MainForm.AERModernStyle;
        MainForm.Invalidate;
        SettingsForm.StyleBook := MainForm.AERModernStyle;
        HelpForm.StyleBook := MainForm.AERModernStyle;
        ImportForm.StyleBook := MainForm.AERModernStyle;
        AboutForm.StyleBook := MainForm.AERModernStyle;
        //FFMPEGForm.StyleBook := MainForm.AERModernStyle;
        RenderingForm.StyleBook := MainForm.AERModernStyle;
        OutputModuleEditorForm.StyleBook := MainForm.AERModernStyle;
      end;
    1:begin
        STYLE := 1;

        MainForm.SettingsIcon.Fill.Color := $FFFFFFFF;
        MainForm.InfoIcon.Fill.Color := $FFFFFFFF;
        MainForm.LaunchIcon.Fill.Color := $FFFFFFFF;
        SettingsForm.RefreshIcon.Fill.Color := $FFFFFFFF;

        MainForm.memUsageTrackBar.Margins.Top := 5;
        MainForm.cacheUsageTrackBar.Margins.Top := 5;

        MainForm.StyleBook := MainForm.AERModernAnimatedStyle;
        MainForm.Invalidate;
        SettingsForm.StyleBook := MainForm.AERModernAnimatedStyle;
        HelpForm.StyleBook := MainForm.AERModernAnimatedStyle;
        ImportForm.StyleBook := MainForm.AERModernAnimatedStyle;
        AboutForm.StyleBook := MainForm.AERModernAnimatedStyle;
        //FFMPEGForm.StyleBook := MainForm.AERModernAnimatedStyle;
        RenderingForm.StyleBook := MainForm.AERModernAnimatedStyle;
        OutputModuleEditorForm.StyleBook := MainForm.AERModernAnimatedStyle;
      end;
    2:begin
        STYLE := 2;

        MainForm.SettingsIcon.Fill.Color := $FF000000;
        MainForm.InfoIcon.Fill.Color := $FF000000;
        MainForm.LaunchIcon.Fill.Color := $FF000000;
        SettingsForm.RefreshIcon.Fill.Color := $FF000000;

        MainForm.memUsageTrackBar.Margins.Top := 0;
        MainForm.cacheUsageTrackBar.Margins.Top := 0;

        MainForm.StyleBook := MainForm.OnyxBlueStyle;
        MainForm.Invalidate;
        SettingsForm.StyleBook := MainForm.OnyxBlueStyle;
        HelpForm.StyleBook := MainForm.OnyxBlueStyle;
        ImportForm.StyleBook := MainForm.OnyxBlueStyle;
        AboutForm.StyleBook := MainForm.OnyxBlueStyle;
        //FFMPEGForm.StyleBook := MainForm.OnyxBlueStyle;
        RenderingForm.StyleBook := MainForm.OnyxBlueStyle;
        OutputModuleEditorForm.StyleBook := MainForm.OnyxBlueStyle;
      end;
  end;
end;

end.
