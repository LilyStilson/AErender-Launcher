unit MainUnit;

(*        AErender Launcher                                                                 *)
(*        MainUnit.pas                                                                      *)
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
  System.JSON,
  System.Net.HttpClient,
  System.Threading,
  System.Rtti,
  System.Bindings.Outputs,
  System.Notification,
  System.ImageList,
  {$ENDREGION}

  {$REGION '    FMX Namespaces    '}
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Grid.Style,
  Fmx.Bind.Editors,
  Fmx.Bind.DBEngExt,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Effects,
  FMX.Objects,
  FMX.Menus,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Grid,
  FMX.ScrollBox,
  FMX.ListBox,
  FMX.Layouts,
  FMX.Edit,
  FMX.Filter.Effects,
  FMX.Controls.Presentation,
  FMX.Ani,
  FMX.Dialogs,
  FMX.Graphics,
  FMX.Platform,
  FMX.MultiView,
  FMX.ExtCtrls,
  FMX.DialogService.Sync,
  FMX.ComboTrackBar,
  FMX.ImgList,
  {$ENDREGION}

  {$REGION '    Data Lib Namespaces    '}
  Data.Bind.EngExt,
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.adomxmldom,
  Xml.XMLDoc,
  Data.Bind.Components,
  Xml.omnixmldom,
  {$ENDREGION}

  {$REGION '    Additional Liraries    '}
  MathExpParser,
  AErenderLauncherLocalization,
  {$ENDREGION}

  {$REGION '    Windows Only Libraries    '}{$IFDEF MSWINDOWS}
    Winapi.ShellAPI, Winapi.Windows, FMX.Platform.Win, Winapi.TlHelp32, WinApi.DwmApi, WinApi.UxTheme, WinApi.Messages;
  {$ENDIF MSWINDOWS}{$ENDREGION}

  {$REGION '    macOS Only Libraries    '}{$IFDEF MACOS}
    Posix.Stdlib, Posix.Unistd, Posix.SysSysctl, Posix.SysTypes, FMX.Platform.Mac,
    Mac.CodeBlocks, MacApi.Dialogs,
    Macapi.Appkit, Macapi.ObjectiveC, Macapi.Foundation, Macapi.Helpers, Macapi.ObjCRuntime, Macapi.CocoaTypes;
  {$ENDIF MACOS}{$ENDREGION}

type
  TMainForm = class(TForm)
    Label1: TLabel;
    inputFileLayout: TLayout;
    Label2: TLabel;
    InputLabels: TLayout;
    pathLayout: TLayout;
    projectPath: TEdit;
    fileChooseLayout: TLayout;
    outputPath: TEdit;
    openFile: TButton;
    saveFile: TButton;
    properties: TGroupBox;
    soundCheckbox: TCheckBox;
    missingFilesCheckbox: TCheckBox;
    threadedRender: TCheckBox;
    propertiesLayout1: TLayout;
    properiesLayout2: TLayout;
    customProp: TEdit;
    customCheckbox: TCheckBox;
    BindingsList1: TBindingsList;
    LinkControlToPropertyEnabled: TLinkControlToProperty;
    compLayout: TLayout;
    bottomButtonsLayout: TLayout;
    launchButton: TButton;
    Lang1: TLang;
    threadsLayout: TLayout;
    compGrid: TStringGrid;
    StringColumn1: TStringColumn;
    compSwitch: TSwitch;
    compTopLayout: TLayout;
    compSwitchLabel: TLabel;
    settingsButton: TButton;
    compCount: TSpinBox;
    compName: TEdit;
    LinkControlToPropertyEnabled2: TLinkControlToProperty;
    LinkControlToPropertyVisible: TLinkControlToProperty;
    LinkControlToPropertyEnabled3: TLinkControlToProperty;
    LinkControlToPropertyVisible2: TLinkControlToProperty;
    LinkControlToPropertyEnabled4: TLinkControlToProperty;
    LinkControlToPropertyVisible3: TLinkControlToProperty;
    threadsTopLayout: TLayout;
    threadsSwitch: TSwitch;
    threadsSwitchLabel: TLabel;
    Label3: TLabel;
    inFrame: TEdit;
    Label4: TLabel;
    outFrame: TEdit;
    threadsGrid: TStringGrid;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    calculateButton: TButton;
    LinkControlToPropertyVisible4: TLinkControlToProperty;
    LinkControlToPropertyEnabled5: TLinkControlToProperty;
    LinkControlToPropertyEnabled6: TLinkControlToProperty;
    LinkControlToPropertyVisible5: TLinkControlToProperty;
    framesLayout: TLayout;
    threadsCount: TComboBox;
    LinkControlToPropertyEnabled7: TLinkControlToProperty;
    LinkControlToPropertyVisible6: TLinkControlToProperty;
    AEPOpenDialog: TOpenDialog;
    SaveDialog1: TSaveDialog;
    LinkControlToPropertyTextPrompt: TLinkControlToProperty;
    LinkControlToPropertyEnabled8: TLinkControlToProperty;
    LinkControlToPropertyEnabled9: TLinkControlToProperty;
    OnyxBlueStyle: TStyleBook;
    infoButton: TButton;
    Image1: TImage;
    Image2: TImage;
    launcherLayout: TLayout;
    outputModuleBox: TComboBox;
    outputModuleLabel: TLabel;
    outputModuleLayout: TLayout;
    renderSettingsLayout: TLayout;
    renderSettings: TComboBox;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    ListBoxItem8: TListBoxItem;
    renderSettingsLabel: TLabel;
    ListBoxItem9: TListBoxItem;
    MainMenu1: TMainMenu;
    launcherItem: TMenuItem;
    fileItem: TMenuItem;
    importConfigItem: TMenuItem;
    exportConfigItem: TMenuItem;
    settingsItem0: TMenuItem;
    separatorItem1: TMenuItem;
    exitItem: TMenuItem;
    helpItem: TMenuItem;
    docsItem: TMenuItem;
    aboutItem: TMenuItem;
    separatorItem2: TMenuItem;
    XMLOpenDialog: TOpenDialog;
    memUsageTrackBar: TTrackBar;
    memUsageLabel: TLabel;
    memUsageLayout: TLayout;
    memUsageInfo: TLabel;
    cacheUsageLayout: TLayout;
    cacheUsageLimitLabel: TLabel;
    cacheUsageTrackBar: TTrackBar;
    cacheUsageInfo: TLabel;
    memUsageInfoEdit: TEdit;
    cacheUsageInfoEdit: TEdit;
    AERModernStyle: TStyleBook;
    settingsIconFill: TFillRGBEffect;
    infoIconFill: TFillRGBEffect;
    MenuBar1: TMenuBar;
    winFileItem: TMenuItem;
    winImportConfigurationItem: TMenuItem;
    winExportConfigItem: TMenuItem;
    winMenuBarSeparator1: TMenuItem;
    winExitItem: TMenuItem;
    winEditItem: TMenuItem;
    winHelpItem: TMenuItem;
    winSettingsItem: TMenuItem;
    winDocItem: TMenuItem;
    winMenuBarSeparator2: TMenuItem;
    winAboutItem: TMenuItem;
    AERModernAnimatedStyle: TStyleBook;
    NotificationC: TNotificationCenter;
    MenuItem1: TMenuItem;
    UpdateLabel: TLabel;
    downloadButton: TButton;
    ffmpegCheckBox: TCheckBox;
    ffmpegConfigButton: TButton;
    ffmpegConcateLayout: TLayout;
    LinkControlToPropertyEnabled11: TLinkControlToProperty;
    renderingBlurEffect: TBlurEffect;
    mainLayout: TLayout;
    FloatAnimation1: TFloatAnimation;
    GridPanelLayout1: TGridPanelLayout;
    GridPanelLayout2: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    GridPanelLayout3: TGridPanelLayout;
    UpdateInfo: TStatusBar;
    winOutModuleEditorItem: TMenuItem;
    outModuleEditorItem0: TMenuItem;
    editItem: TMenuItem;
    outModuleEditorItem: TMenuItem;
    settingsItem: TMenuItem;
    SettingsIcon: TPath;
    InfoIcon: TPath;
    LaunchIcon: TPath;
    procedure FormResize(Sender: TObject);
    procedure compSwitchSwitch(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure compCountChange(Sender: TObject);
    procedure settingsButtonClick(Sender: TObject);
    procedure threadsSwitchSwitch(Sender: TObject);
    procedure calculateButtonClick(Sender: TObject);
    procedure threadsCountChange(Sender: TObject);
    procedure openFileClick(Sender: TObject);
    procedure launchButtonClick(Sender: TObject);
    procedure saveFileClick(Sender: TObject);
    procedure infoButtonClick(Sender: TObject);
    procedure exitItemClick(Sender: TObject);
    procedure docsItemClick(Sender: TObject);
    procedure aboutItemClick(Sender: TObject);
    procedure importConfigItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memUsageTrackBarChange(Sender: TObject);
    procedure cacheUsageTrackBarChange(Sender: TObject);
    procedure memUsageInfoClick(Sender: TObject);
    procedure memUsageInfoEditValidate(Sender: TObject; var Text: string);
    procedure cacheUsageInfoEditValidate(Sender: TObject;
      var Text: string);
    procedure cacheUsageInfoClick(Sender: TObject);
    procedure cacheUsageInfoEditExit(Sender: TObject);
    procedure memUsageInfoEditExit(Sender: TObject);
    procedure downloadButtonClick(Sender: TObject);
    procedure ffmpegConfigButtonClick(Sender: TObject);
    procedure projectPathDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure projectPathDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure outFrameValidate(Sender: TObject; var Text: string);
    procedure outputModuleBoxChange(Sender: TObject);
    procedure compNameTyping(Sender: TObject);
    procedure outModuleEditorItem0Click(Sender: TObject);
    procedure winOutModuleEditorItemClick(Sender: TObject);
    procedure inFrameValidate(Sender: TObject; var Text: string);
  private
    { Private declarations }
    //{$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
    {$IFDEF MSWINDOWS}procedure WMNCPaint(var AMessage: TMessage); message WM_NCPAINT;{$ENDIF}
  public
    { Public declarations }
    procedure DragEnter(const Data: TDragObject; const Point: TPointF); override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
  end;
  OutputModule = record
    Name: String;
    Module: String;
    Mask: String;
  end;
  function GetPlatformMemorySize: Int64;
  function GetHTML(URL: String): String;
  function IsUpdateAvailable: Boolean;
  function GetFFMPEGPath: WideString;
  function GetDirectoryFiles(Directory: String): TArray<System.String>;
  function KillProcess(ProcessName: String): Integer;
  procedure InitOutputModules;
  procedure UpdateOutputModules;
  procedure InitConfiguration(Path: String);
  procedure LoadConfiguration(Path: String);
  procedure SaveConfiguration(Path: String);

const
  APPVERSION = 'v0.8.2-beta';
  PLATFORMPATHSEPARATOR = {$IFDEF MSWINDOWS}'\'{$ENDIF MSWINDOWS}
                          {$IFDEF MACOS}'/'{$ENDIF MACOS};


var
  MainForm: TMainForm;                                      (*  Main Form Declaration                       *)
  APPFOLDER, VER, AERPATH, DEFPRGPATH, DEFOUTPATH,
  ERR, gitResponse, gitVersion, gitDownload, ffmpegPath,
  AERH, tempSavePath, DelTempFiles: String;                 (*  Required variables for configuration file   *)
  gitRelease: TJsonValue;                                   (*  GitHub API's returning JSON file            *)
  UpdateAvailable: Boolean = False;                         (*  Represents app updates availability         *)
  FFMPEG: Boolean = False;                                  (*  Represents FFMPEG availability              *)
  RenderWindowSender: TButton;                              (*  Who opened Rendering window?                *)
  LANG, STYLE, ONRENDERSTART: Integer;                            (*  Theme and OnRenderStart combobox values     *)
  LogFiles: TArray<System.String>;                          (*  All the aerender log files here             *)
  OutputModules: TArray<OutputModule>;                      (*  All the After Effects output modules here   *)
  TMathParser: MathExpParser.TExpressionParser;             (*  Mathematical parser for frames calculation  *)
  FHandleDragDirectly: Boolean = False;                     (*  For implementation of DragDrop functional   *)
  Language: TArray<LauncherText>;

implementation

uses
  {$REGION '    AErenderLauncher Liraries    '}
  SplashScreenUnit,
  SettingsUnit,
  HelpUnit,
  ImportUnit,
  AboutUnit,
  FFMPEGUnit,
  RenderingUnit,
  OutputModuleEditorUnit;
  {$ENDREGION}

{$R *.fmx}

{$REGION '    Routines    '}

///<summary>
/// Routines block.
/// All the AErender Launcher required low-level procedures and functions are stored here.
///</summary>
{$IFDEF MSWINDOWS}
procedure TMainForm.WMNCPaint(var AMessage: TMessage);
begin
  var hWnd: HWND := FormToHWND(Self);

  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);

  {var v: Integer := 2;

  DwmSetWindowAttribute(hWnd, 2, @v, 4);

  var m: MARGINS;
  m.cxLeftWidth := 0;
  m.cxRightWidth := 0;
  m.cyTopHeight := 0;
  m.cyBottomHeight := 1;

  DwmExtendFrameIntoClientArea(hWnd, m);}
end;
{$ENDIF MSWINDOWS}

function GetPlatformMemorySize: Int64;            //BYTES, BLYAD
var
  {$IFDEF MSWINDOWS}
    MS_Ex: MemoryStatusEx;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    res: Int64;
    len: size_t;
  {$ENDIF MACOS}
begin
  {$IFDEF MSWINDOWS}
    FillChar (MS_Ex, SizeOf (MemoryStatusEx), #0);
    MS_Ex.dwLength := SizeOf (MemoryStatusEx);
    GlobalMemoryStatusEx (MS_Ex);
    Result := MS_Ex.ullTotalPhys;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    len := SizeOf (Result);
    res := SysCtlByName ('hw.memsize', @Result, @len, nil, 0);
    if res <> 0 then
      RaiseLastOSError;
  {$ENDIF MACOS}
end;

function GetHTML(URL: String): String;
var
  HttpClient: THttpClient;
  HttpResponse: IHttpResponse;
begin
  HttpClient := THttpClient.Create;
  try
    HttpResponse := HttpClient.Get(URL);
    Result := HttpResponse.ContentAsString();
    HttpClient.Free;
  except
    on Exception do
      begin
        Result := '404';
        HttpClient.Free;
      end;
  end;
end;

function IsUpdateAvailable: Boolean;
begin
  gitResponse := GetHTML('https://api.github.com/repos/lilystilson/aerender-launcher/releases');
  if not (gitResponse = '404') then
    begin
      gitRelease := TJsonObject.ParseJSONValue(gitResponse);
      gitVersion := gitRelease.GetValue<string>('[0].tag_name');
    end
  else
    gitVersion := APPVERSION;

  if APPVERSION = gitVersion  then
    Result := False
  else
    Result := True;
end;

function GetFFMPEGPath: WideString;
var
  AERenderDirectory: String;
  Folders: System.TArray<String>;
begin
  {$IFDEF MSWINDOWS}AERenderDirectory := 'C:\ProgramData\AErender';{$ENDIF MSWINDOWS}
  {$IFDEF MACOS}AERenderDirectory := GetEnvironmentVariable('HOME') + '/Documents/AErender/';{$ENDIF}

  Folders := TDirectory.GetDirectories(AErenderDirectory);
  for var i := 0 to High(Folders) do
    if Folders[i].Contains('ffmpeg') then begin
      Result := Folders[i];
      break
    end else
      Result := '';
end;

function GetDirectoryFiles(Directory: String): TArray<System.String>;
var
  Files: System.TArray<System.String>;
begin
  Files := TDirectory.GetFiles(Directory);
  for var i := 0 to High(Files) do
    begin
      SetLength(Result, i+1);
      Result[i] := Files[i];
    end;
end;

function KillProcess(ProcessName: String): Integer;
{$IFDEF MSWINDOWS}
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin

  try
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

    while Integer(ContinueLoop) <> 0 do
    begin
      if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
        UpperCase(ProcessName)) or (UpperCase(FProcessEntry32.szExeFile) =
        UpperCase(ProcessName))) then
        Result := Integer(TerminateProcess(
                          OpenProcess(PROCESS_TERMINATE,
                                      BOOL(0),
                                      FProcessEntry32.th32ProcessID),
                                      0));
       ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
    CloseHandle(FSnapshotHandle);
    Result := 0;
  except
    Result := -1;
  end;
{$ENDIF MSWINDOWS}
{$IFDEF MACOS}
begin
  try
    _system(PAnsiChar('pkill "' + AnsiString(ProcessName) + '"'));
    Result := 0;
  except
    Result := -1;
  end;
{$ENDIF MACOS}
end;

procedure InitOutputModules;
begin
  SetLength (OutputModules, 10);

  OutputModules[0].Name := 'Lossless';
  OutputModules[0].Module := 'Lossless';
  OutputModules[0].Mask := '[compName].[fileExtension]';

  OutputModules[1].Name := 'AIFF 48kHz';
  OutputModules[1].Module := 'AIFF 48kHz';
  OutputModules[1].Mask := '[compName].[fileExtension]';

  OutputModules[2].Name := 'Alpha Only';
  OutputModules[2].Module := 'Alpha Only';
  OutputModules[2].Mask := '[compName].[fileExtension]';

  OutputModules[3].Name := 'AVI DV NTSC 48kHz';
  OutputModules[3].Module := 'AVI DV NTSC 48kHz';
  OutputModules[3].Mask := '[compName].[fileExtension]';

  OutputModules[4].Name := 'AVI DV PAL 48kHz';
  OutputModules[4].Module := 'AVI DV PAL 48kHz';
  OutputModules[4].Mask := '[compName].[fileExtension]';

  OutputModules[5].Name := 'Lossless with Alpha';
  OutputModules[5].Module := 'Lossless with Alpha';
  OutputModules[5].Mask := '[compName].[fileExtension]';

  OutputModules[6].Name := 'Multi-Machine Sequence';
  OutputModules[6].Module := 'Multi-Machine Sequence';
  OutputModules[6].Mask := '[compName]_[#####].[fileExtension]';

  OutputModules[7].Name := 'Photoshop';
  OutputModules[7].Module := 'Photoshop';
  OutputModules[7].Mask := '[compName]_[#####].[fileExtension]';

  OutputModules[8].Name := 'Save Current Preview';
  OutputModules[8].Module := 'Save Current Preview';
  OutputModules[8].Mask := '[compName].[fileExtension]';

  OutputModules[9].Name := 'TIFF Sequence with Alpha';
  OutputModules[9].Module := 'TIFF Sequence with Alpha';
  OutputModules[9].Mask := '[compName]_[#####].[fileExtension]';
end;

procedure UpdateOutputModules;
begin
  MainForm.outputModuleBox.Items.Clear;
  for var i := 0 to High(OutputModules) do
    MainForm.outputModuleBox.Items.Add(OutputModules[i].Name);
  MainForm.outputModuleBox.Items.Add('Configure Output Modules...');
end;

procedure InitConfiguration(Path: String);
var
  Config: IXMLDocument;
  RootNode: IXMLNode;
  ChildNode: IXMLNode;
begin
  InitOutputModules;
  Config := TXMLDocument.Create(nil);
  Config.Active := True;
  Config.Encoding := 'utf-8';
  Config.Options := [doNodeAutoIndent];

  RootNode := Config.AddChild('launcherconfig');

  RootNode.AddChild('lang').Text := '0';
  RootNode.AddChild('style').Text := '0';
  RootNode.AddChild('aerender').Text := '';
  RootNode.AddChild('onRenderStart').Text := '0';
  RootNode.AddChild('defprgpath').Text := '';
  RootNode.AddChild('defoutpath').Text := '';
  RootNode.AddChild('handle').Text := 'True';
  RootNode.AddChild('delTempFiles').Text := 'True';

  RootNode.AddChild('projectPath').Text := '';
  RootNode.AddChild('outputPath').Text := '';
  RootNode.AddChild('tempSavePath').Text := '';

  RootNode.AddChild('comp').Text := '';
  RootNode.AddChild('startFrame').Text := '';
  RootNode.AddChild('endFrame').Text := '';

  RootNode.AddChild('missingFiles').Text := 'False';
  RootNode.AddChild('sound').Text := 'False';
  RootNode.AddChild('thread').Text := 'False';
  RootNode.AddChild('prop').Text := '';
  RootNode.ChildNodes['prop'].Attributes['enabled'] := 'False';

  RootNode.AddChild('memoryLimit').Text := '100';
  RootNode.AddChild('cacheLimit').Text := '100';

  ChildNode := RootNode.AddChild('outputModule');
  ChildNode.Attributes['selected'] := '0';

  for var i := 0 to High(OutputModules) do
    begin
      var ModuleNode: IXMLNode := RootNode.ChildNodes['outputModule'].AddChild('module');
      ModuleNode.AddChild('name').Text := OutputModules[i].Name;
      ModuleNode.AddChild('moduleName').Text := OutputModules[i].Module;
      MainForm.outputModuleBox.Items.Add(OutputModules[i].Name);
      ModuleNode.AddChild('filemask').Text := OutputModules[i].Mask;
    end;

  MainForm.outputModuleBox.Items.Add('Configure Output Modules...');
  Config.SaveToFile(Path);
end;

procedure LoadConfiguration(Path: String);
var
  Config: IXMLDocument;
  RootNode: IXMLNode;
begin
  Config := TXMLDocument.Create(nil);
  Config.LoadFromFile(Path);
  Config.Active := True;
  RootNode := Config.DocumentElement;

  LANG := RootNode.ChildNodes['lang'].Text.ToInteger();
  STYLE := RootNode.ChildNodes['style'].Text.ToInteger();
  AERPATH := RootNode.ChildNodes['aerender'].Text;
  ONRENDERSTART := RootNode.ChildNodes['onRenderStart'].Text.ToInteger();
  DEFPRGPATH := RootNode.ChildNodes['defprgpath'].Text;
  DEFOUTPATH := RootNode.ChildNodes['defoutpath'].Text;
  AERH := RootNode.ChildNodes['handle'].Text;
  DelTempFiles := RootNode.ChildNodes['delTempFiles'].Text;

  MainForm.projectPath.Text := RootNode.ChildNodes['projectPath'].Text;
  MainForm.outputPath.Text := RootNode.ChildNodes['outputPath'].Text;
  tempSavePath := RootNode.ChildNodes['tempSavePath'].Text;

  MainForm.compName.Text := RootNode.ChildNodes['comp'].Text;
  MainForm.inFrame.Text := RootNode.ChildNodes['startFrame'].Text;
  MainForm.outFrame.Text := RootNode.ChildNodes['endFrame'].Text;

  MainForm.missingFilesCheckbox.IsChecked := RootNode.ChildNodes['missingFiles'].Text.ToBoolean();
  MainForm.soundCheckbox.IsChecked := RootNode.ChildNodes['sound'].Text.ToBoolean();
  MainForm.threadedRender.IsChecked := RootNode.ChildNodes['thread'].Text.ToBoolean();
  MainForm.customCheckbox.IsChecked := StrToBool(RootNode.ChildNodes['prop'].Attributes['enabled']);
  MainForm.customProp.Text := RootNode.ChildNodes['prop'].Text;

  MainForm.memUsageTrackBar.Value := RootNode.ChildNodes['memoryLimit'].Text.ToSingle();
  MainForm.cacheUsageTrackBar.Value := RootNode.ChildNodes['cacheLimit'].Text.ToSingle();

  SetLength (OutputModules, RootNode.ChildNodes['outputModule'].ChildNodes.Count);
  for var i := 0 to High(OutputModules) do
    begin
      OutputModules[i].Name := RootNode.ChildNodes['outputModule'].ChildNodes[i].ChildNodes['name'].Text;
      OutputModules[i].Module := RootNode.ChildNodes['outputModule'].ChildNodes[i].ChildNodes['moduleName'].Text;
      MainForm.outputModuleBox.Items.Add(OutputModules[i].Name);
      OutputModules[i].Mask := RootNode.ChildNodes['outputModule'].ChildNodes[i].ChildNodes['filemask'].Text;
    end;
  MainForm.outputModuleBox.Items.Add('Configure Output Modules...');
  MainForm.outputModuleBox.ItemIndex := StrToInt(RootNode.ChildNodes['outputModule'].Attributes['selected']);
end;

procedure SaveConfiguration(Path: String);
var
  Config: IXMLDocument;
  RootNode: IXMLNode;
  ChildNode: IXMLNode;
begin
  Config := TXMLDocument.Create(nil);
  Config.Active := True;
  Config.Encoding := 'utf-8';
  Config.Options := [doNodeAutoIndent];

  RootNode := Config.AddChild('launcherconfig');

  RootNode.AddChild('lang').Text := LANG.ToString;
  RootNode.AddChild('style').Text := STYLE.ToString;
  RootNode.AddChild('aerender').Text := AERPATH;
  RootNode.AddChild('onRenderStart').Text := ONRENDERSTART.ToString;
  RootNode.AddChild('defprgpath').Text := MainForm.AEPOpenDialog.InitialDir;
  RootNode.AddChild('defoutpath').Text := MainForm.SaveDialog1.InitialDir;
  RootNode.AddChild('handle').Text := AERH;
  RootNode.AddChild('delTempFiles').Text := DelTempFiles;

  RootNode.AddChild('projectPath').Text := MainForm.projectPath.Text;
  RootNode.AddChild('outputPath').Text := MainForm.outputPath.Text;
  RootNode.AddChild('tempSavePath').Text := tempSavePath;

  RootNode.AddChild('comp').Text := MainForm.compName.Text;
  RootNode.AddChild('startFrame').Text := MainForm.inFrame.Text;
  RootNode.AddChild('endFrame').Text := MainForm.outFrame.Text;
  RootNode.AddChild('missingFiles').Text := BoolToStr(MainForm.missingFilesCheckbox.IsChecked, True);

  RootNode.AddChild('sound').Text := BoolToStr(MainForm.soundCheckbox.IsChecked, True);
  RootNode.AddChild('thread').Text := BoolToStr(MainForm.threadedRender.IsChecked, True);
  RootNode.AddChild('prop').Text := MainForm.customProp.Text;
  RootNode.ChildNodes['prop'].Attributes['enabled'] := BoolToStr(MainForm.customCheckbox.IsChecked, True);

  RootNode.AddChild('memoryLimit').Text := MainForm.memUsageTrackBar.Value.ToString;
  RootNode.AddChild('cacheLimit').Text := MainForm.cacheUsageTrackBar.Value.ToString;

  ChildNode := RootNode.AddChild('outputModule');
  ChildNode.Attributes['selected'] := MainForm.outputModuleBox.ItemIndex.ToString;

  for var i := 0 to High(OutputModules) do
    begin
      var ModuleNode: IXMLNode := RootNode.ChildNodes['outputModule'].AddChild('module');
      ModuleNode.AddChild('name').Text := OutputModules[i].Name;
      ModuleNode.AddChild('moduleName').Text := OutputModules[i].Module;
      ModuleNode.AddChild('filemask').Text := OutputModules[i].Mask;
    end;

  Config.SaveToFile(Path);
end;

{$ENDREGION}

{$REGION '    Public    '}
procedure TMainForm.DragEnter(const Data: TDragObject; const Point: TPointF);
begin
  FHandleDragDirectly := (Data.Files <> nil);
  if not FHandleDragDirectly then
    inherited;
end;

procedure TMainForm.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if FHandleDragDirectly then
    Operation := TDragOperation.Copy
  else
    inherited;
end;

procedure TMainForm.DragDrop(const Data: TDragObject; const Point: TPointF);
var
  S: string;
begin
  if FHandleDragDirectly then
    if Data.Source <> nil then
      projectPath.Text := Data.Source.ClassName
    else
      projectPath.Text := Data.Files[0]
  else
    inherited;
end;
{$ENDREGION}

{$REGION '    Published    '}

procedure TMainForm.aboutItemClick(Sender: TObject);
begin
  AboutForm.Show;
end;

procedure TMainForm.calculateButtonClick(Sender: TObject);
var
  I, J, K, Start, Stop: Integer;
  Tm: String;
begin
  try
    threadsGrid.RowCount := threadsCount.Items[threadsCount.ItemIndex].ToInteger;
    Tm := inFrame.Text;
    if Tm.IsEmpty then
      inFrame.Text := '0';
    Start := inFrame.Text.ToInteger;
    Stop := outFrame.Text.ToInteger;
    threadsGrid.Cells[0, 0] := IntToStr(Start);
    J := Stop - Start;
    K := J div threadsCount.Items[threadsCount.ItemIndex].ToInteger;
    threadsGrid.Cells[1, 0] := IntToStr(Start+K);
    for I := 1 to threadsCount.Items[threadsCount.ItemIndex].ToInteger - 1 do
      begin
        threadsGrid.Cells[0, I] := (Start+K*I+1).ToString;
        threadsGrid.Cells[1, I] := (Start+K*(I+1)).ToString;
      end;
    threadsGrid.Cells[1, threadsCount.Items[threadsCount.ItemIndex].ToInteger-1]:=IntToStr(Stop);
  except
    on Exception do
      TDialogServiceSync.MessageDialog('End frame is reqired for calculation!', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0)
  end;
end;

procedure TMainForm.compNameTyping(Sender: TObject);
begin
  compGrid.Cells[0, 0] := compName.Text;
end;

procedure TMainForm.compSwitchSwitch(Sender: TObject);
begin
  if compSwitch.IsChecked then
    begin
      if UpdateAvailable = True then
        begin
          if MainForm.Height <= 470 then
            MainForm.Height := MainForm.Height + 130;
        end
      else
        if MainForm.Height <= 420 then
          MainForm.Height := MainForm.Height + 130;
      //if LANG = 'EN' then
        compSwitchLabel.Text := 'Multiple Compositions';
      {if LANG = 'RU' then
        compSwitchLabel.Text := 'Несколько композиций';}
      compGrid.AniCalculations.AutoShowing := False;
      compGrid.RowCount := Round(compCount.Value);
      compGrid.Cells[0, 0] := compName.Text;
      compGrid.Model.ScrollDirections := TScrollDirections.Vertical;
    end
  else
    begin
      if UpdateAvailable = True then
        begin
          if MainForm.Height >= 580 then
            MainForm.Height := MainForm.Height - 130
        end
      else
        if MainForm.Height <= 550 then
          MainForm.Height := MainForm.Height - 130;
      //if LANG = 'EN' then
        compSwitchLabel.Text := 'Single Composition';
      {if LANG = 'RU' then
        compSwitchLabel.Text := 'Одна композиция';}
      compName.Text := compGrid.Cells[0, 0];
    end;
end;

procedure TMainForm.docsItemClick(Sender: TObject);
begin
  //HelpForm.Show;
  TDialogServiceSync.MessageDialog('Not ready yet!', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0)
end;

procedure TMainForm.downloadButtonClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PWideChar(gitDownload), nil, nil, SW_SHOW);
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    _system(PAnsiChar('open ' + AnsiString('"' + gitDownload + '"')));
  {$ENDIF MACOS}
end;

procedure TMainForm.exitItemClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.ffmpegConfigButtonClick(Sender: TObject);
begin
  FFMPEGForm.ShowModal;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  CFG: TextFile;
begin
  {$IFDEF MSWINDOWS}KillProcess('AfterFX.com');{$ENDIF MSWINDOWS}
  {$IFDEF MACOS}KillProcess('aerendercore');{$ENDIF MACOS}

  SaveConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
  if StrToBool(DelTempFiles) = True then
    begin
      var AerenderDirectory: TArray<String> := GetDirectoryFiles(APPFOLDER);
      for var i := 0 to High(AerenderDirectory) do
        begin
          {$IFDEF MSWINDOWS}
          if (AerenderDirectory[i].Contains('.bat')) then
            System.SysUtils.DeleteFile(PWideChar(AerenderDirectory[i]));
          {$ENDIF MSWINDOWS}
          {$IFDEF MACOS}
          if (AerenderDirectory[i].Contains('.command')) then
            System.SysUtils.DeleteFile(PWideChar(AerenderDirectory[i]));
          {$ENDIF MACOS}
          if (AerenderDirectory[i].Contains('.log')) then
            System.SysUtils.DeleteFile(PWideChar(AerenderDirectory[i]));
        end;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  CFG: TextFile;
begin
  {$IFDEF MSWINDOWS}DwmCompositionEnabled();{$ENDIF MSWINDOWS}
  FormatSettings.DecimalSeparator := '.';
  MainForm.Width := 600;
  MainForm.Caption := 'AErender Launcher (' + APPVERSION + ')';
  APPFOLDER :=  {$IFDEF MSWINDOWS}'C:\ProgramData\AErender\'{$ENDIF MSWINDOWS}
                {$IFDEF MACOS}GetEnvironmentVariable('HOME') + '/Documents/AErender/'{$ENDIF MACOS};
  UpdateAvailable := IsUpdateAvailable;
  if UpdateAvailable = True then
    begin
      MainForm.Height := {$IFDEF MSWINDOWS}460{$ENDIF MSWINDOWS}
                      {$IFDEF MACOS}420{$ENDIF MACOS};
      UpdateInfo.Visible := True;
      UpdateInfo.Enabled := True;
      downloadButton.Text := 'Download from GitHub (' + gitVersion + ')';
      {$IFDEF MSWINDOWS}gitDownload := gitRelease.GetValue<string>('[0].assets[1].browser_download_url');{$ENDIF MSWINDOWS}
      {$IFDEF MACOS}gitDownload := gitRelease.GetValue<string>('[0].assets[0].browser_download_url');{$ENDIF MACOS}
    end
  else
    begin
      UpdateInfo.Visible := False;
      UpdateInfo.Enabled := False;
      MainForm.Height := {$IFDEF MSWINDOWS}420{$ENDIF MSWINDOWS}
                      {$IFDEF MACOS}400{$ENDIF MACOS};
    end;
  try
    if GetFFMPEGPath <> '' then
      begin
        FFMPEG := True;
        ffmpegPath := GetFFMPEGPath;
        ffmpegCheckBox.Enabled := True;
      end
    else
      raise Exception.Create('FFMPEG not found');
  except
    on Exception do
      begin
        FFMPEG := False;
        ffmpegCheckBox.Enabled := False;
        ffmpegCheckBox.Hint := 'FFMPEG is not found at' + {$IFDEF MSWINDOWS} 'C:\ProgramData\AErender\' {$ENDIF MSWINDOWS}
                                                          {$IFDEF MACOS} '~/Documents/AErender/' {$ENDIF MACOS} + 'directory';
      end;
  end;
  {$IFDEF MSWINDOWS}
  launcherItem.Free;
  exitItem.ShortCut := TextToShortCut('Alt+F4');
  exportConfigItem.ShortCut := TextToShortCut('Ctrl+E');
  importConfigItem.ShortCut := TextToShortCut('Ctrl+I');
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
  editItem.Free;
  exitItem.ShortCut := TextToShortCut('Cmd+Q');
  exportConfigItem.ShortCut := TextToShortCut('Cmd+E');
  importConfigItem.ShortCut := TextToShortCut('Cmd+I');
  {$ENDIF MACOS}
  if DirectoryExists (APPFOLDER) then
    AssignFile (CFG, APPFOLDER + 'AErenderConfiguration.xml')
  else
    begin
      CreateDir (APPFOLDER);
      AssignFile (CFG, APPFOLDER + 'AErenderConfiguration.xml');
    end;
  if FileExists(APPFOLDER + 'AErenderConfiguration.xml') then
    begin
      try
        LoadConfiguration (APPFOLDER + 'AErenderConfiguration.xml');
      except
        if (TDialogServiceSync.MessageDialog(('Configuration file is corrupted! Press OK to renew configuration file. Application will be restarted.' + #13#10 +
                              {$IFDEF MSWINDOWS}'C:\ProgramData\AErender\AErenderConfiguration.xml read error.'{$ENDIF MSWINDOWS}
                                  {$IFDEF MACOS}'~/Documents/AErender/AErenderConfiguration.xml read error.'{$ENDIF MACOS}),
              TMsgDlgType.mtError, mbOKCancel, TMsgDlgBtn.mbOK, 0) = 1) then
            begin
              System.SysUtils.DeleteFile (APPFOLDER + 'AErenderConfiguration.xml');
              {$IFDEF MSWINDOWS}ShellExecute(0, 'OPEN', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);{$ENDIF MSWINDOWS}
              {$IFDEF MACOS}_system(PAnsiChar('open -a "' + AnsiString(ParamStr(0)) + '" &'));{$ENDIF MACOS}
            end;
        Application.Terminate;
      end;
    end
  else
    begin
      InitConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
      AERH := 'True';
      DelTempFiles := 'True';
    end;
  AEPOpenDialog.InitialDir := DEFPRGPATH;
  SaveDialog1.InitialDir := DEFOUTPATH;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  //compName.Text := 'H: ' + MainForm.Height.ToString + '; W: ' + MainForm.Width.ToString;
  StringColumn1.Width := compGrid.Width;
  StringColumn2.Width := threadsGrid.Width * 0.5;
  StringColumn3.Width := threadsGrid.Width * 0.5;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SettingsForm.styleBox.ItemIndex := STYLE;
  SettingsForm.onRenderStartBox.ItemIndex := ONRENDERSTART;
  SettingsForm.HandleCheckBox.IsChecked := StrToBool(AERH);
  SettingsForm.delFilesCheckBox.IsChecked := StrToBool(DelTempFiles);

  if memUsageTrackBar.Value = 100 then
    memUsageInfo.Text := 'Unlimited'
  else
    memUsageInfo.Text := Trunc(memUsageTrackBar.Value).ToString + '% (' + Trunc((GetPlatformMemorySize/1024/1024) * (memUsageTrackBar.Value / 100)).ToString + ' MB)';
  if cacheUsageTrackBar.Value = 100 then
    cacheUsageInfo.Text := 'Unlimited'
  else
    cacheUsageInfo.Text := Trunc(cacheUsageTrackBar.Value).ToString + '%';

  StringColumn1.Width := compGrid.Width - 5;
  StringColumn2.Width := threadsGrid.Width * 0.5;
  StringColumn3.Width := threadsGrid.Width * 0.5;

  SplashScreenForm.Close;
  SplashScreenForm.Free;

  if (ParamCount > 0) and (ParamStr(1).Contains('-aer')) then
    begin
      ImportUnit.PARAMSTART := True;
      ImportUnit.XMLPath := ParamStr(2);
      ImportForm.ShowModal;
    end;
end;

procedure TMainForm.importConfigItemClick(Sender: TObject);
begin
  compSwitch.Enabled := True;
  compSwitch.IsChecked := False;
  threadsSwitch.Enabled := True;
  threadsSwitch.IsChecked := False;

  with XMLOpenDialog do
    if Execute then
      ImportForm.ShowModal;
end;

procedure TMainForm.infoButtonClick(Sender: TObject);
begin
  RenderWindowSender := infoButton;
  RenderingForm.ShowModal;
end;

procedure TMainForm.inFrameValidate(Sender: TObject; var Text: string);
var
  tempStr: String;
begin
  try
    tempStr := TMathParser.ParseExpressionToFloat(inFrame.Text).ToString;
  except
    on Exception do
      tempStr := Text;
  end;
  Text := tempStr;
end;

procedure TMainForm.launchButtonClick(Sender: TObject);
type
  exec = record
    script: String;
    F: TextFile;
  end;
var
  threads, comps, emptyComps: Integer;
  PATH, logPath, prgPath: String;
  execFile: array [1..100] of exec;

  Notification: TNotification;
begin
  //Error Codes
  //if LANG = 'EN' then
    begin
      emptyComps := 0;
      if AERPATH.IsEmpty then
        ERR := ERR + #13#10 + '[Error 1]: aerender path not specified';
      if projectPath.Text.IsEmpty then
        ERR := ERR + #13#10 + '[Error 2]: Project path not specified';
      if outputPath.Text.IsEmpty then
        ERR := ERR + #13#10 + '[Error 3]: Output path not specified';
      if compName.Text.IsEmpty then
        ERR := ERR + #13#10 + '[Error 4]: Composition name not specified';
      if compSwitch.IsChecked then
        begin
          for var i := 0 to compCount.Value.ToString.ToInteger-1 do
            if compGrid.Cells[0, i].IsEmpty then
              inc (emptyComps);
          if emptyComps > 0 then
            ERR := ERR + #13#10 + '[Error 5]: Not all compositions specified in composition list';
        end;
      if OutputModules[outputModuleBox.ItemIndex].Module.IsEmpty then
        ERR := ERR + #13#10 + '[Error 6]: Output Module in selected Output Module Preset is not specified';
    end;
  {else
    begin
      emptyComps := 0;
      if AERPATH.IsEmpty then
        ERR := ERR + #13#10 + '[Error 1]: Путь к aerender не указан';
      if projectPath.Text.IsEmpty then
        ERR := ERR + #13#10 + '[Error 2]: Путь к проекту не указан';
      if outputPath.Text.IsEmpty then
        ERR := ERR + #13#10 + '[Error 3]: Выходной путь не указан';
      if compName.Text.IsEmpty then
        ERR := ERR + #13#10 + '[Error 4]: Название композиции не указано';
      if compSwitch.IsChecked then
        begin
          for var i := 0 to compCount.Value.ToString.ToInteger-1 do
            if compGrid.Cells[0, i].IsEmpty then
              inc (emptyComps);
          if emptyComps > 0 then
            ERR := ERR + #13#10 + '[Error 5]: Названия не всех композиций указаны в списке';
        end;
    end;}

  //Proceed if no errors occured
  if not ERR.IsEmpty then
    begin
      //if LANG = 'EN' then
        TDialogServiceSync.MessageDialog(('The following error(s) has occured: ' + ERR), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
      {if LANG = 'RU' then
        TDialogServiceSync.MessageDialog(('Произошли следующие ошибки: ' + ERR), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0); }
      ERR := '';
    end
  else
    begin
      if threadsSwitch.IsChecked then
        begin
          threads := threadsCount.Items[threadsCount.ItemIndex].ToInteger;
          SetLength (LogFiles, threads);
        end
      else
        begin
          threads := 1;
          if Length(LogFiles) = 0 then
            SetLength (LogFiles, threads);
        end;
      if compSwitch.IsChecked then
        begin
          comps := StrToInt(compCount.Value.ToString);
          SetLength (LogFiles, comps);
        end
      else
        begin
          comps := 1;
          if Length(LogFiles) = 0 then
            SetLength (LogFiles, comps);
        end;

      for var j := 0 to comps-1 do
        for var i := 1 to threads do
          begin
            //Script compiling section
            if SettingsForm.HandleCheckBox.IsChecked then
              execFile[i].script := '(';

            if comps = 1 then
              logPath := APPFOLDER + compName.Text + '_' + i.ToString
            else
              logPath := APPFOLDER + compGrid.Cells[0, j] + '_' + i.ToString;

            PATH := outputPath.Text;
            //PATH.Insert(PATH.Length - 12, '_' + compGrid.Cells[0, j] + '_' + i.ToString);

            if outputPath.Text.Contains('[projectName]' + PLATFORMPATHSEPARATOR) then
              begin
                PATH := StringReplace(PATH, '[projectName]', ExtractFileName(projectPath.Text), [rfReplaceAll, rfIgnoreCase]);
                if not DirectoryExists(ExtractFilePath(PATH)) then
                  CreateDir(ExtractFilePath(PATH));
              end;

            if threadsSwitch.IsChecked then
              begin
                var FilePath: String := ExtractFilePath(PATH);
                var FileName: String := StringReplace(ExtractFileName(PATH), ExtractFileExt(PATH), '', [rfReplaceAll, rfIgnoreCase]);
                var FileExt:  String := ExtractFileExt(PATH);
                PATH := FilePath + FileName + '_' + i.ToString + FileExt;
              end;

            if compSwitch.IsChecked then
              if not outputPath.Text.Contains('[compName]') then
                begin
                  var FilePath: String := ExtractFilePath(PATH);
                  var FileName: String := StringReplace(ExtractFileName(PATH), ExtractFileExt(PATH), '', [rfReplaceAll, rfIgnoreCase]);
                  var FileExt:  String := ExtractFileExt(PATH);
                  PATH := FilePath + FileName + '_' + compGrid.Cells[0, j] + FileExt;
                end;



            execFile[i].script := execFile[i].script + '"' + AERPATH + '" ' + '-project "' + projectPath.Text + '" -output "' + PATH + '" ';

            if compSwitch.IsChecked then
              execFile[i].script := execFile[i].script + '-comp "' + compGrid.Cells[0, j] + '" '
            else
              execFile[i].script := execFile[i].script + '-comp "' + compName.Text + '" ';

            if threadsSwitch.IsChecked then
              begin
                execFile[i].script := execFile[i].script + '-s "' + threadsGrid.Cells[0, i-1] + '" ';
                execFile[i].script := execFile[i].script + '-e "' + threadsGrid.Cells[1, i-1] + '" ';
              end
            else
              begin
                if not inFrame.Text.IsEmpty then
                  execFile[i].script := execFile[i].script + '-s "' + inFrame.Text + '" ';
                if not outFrame.Text.IsEmpty then
                  execFile[i].script := execFile[i].script + '-e "' + outFrame.Text + '" ';
              end;

            if soundCheckbox.IsChecked then
              execFile[i].script := execFile[i].script + '-sound ON ';

            if threadedRender.IsChecked then
              execFile[i].script := execFile[i].script + '-mp ';

            if missingFilesCheckbox.IsChecked then
              execFile[i].script := execFile[i].script + '-continueOnMissingFootage ';

            if outputModuleBox.ItemIndex <> -1 then
              execFile[i].script := execFile[i].script + '-OMtemplate "' + OutputModules[outputModuleBox.ItemIndex].Module + '" ';

            execFile[i].script := execFile[i].script + '-mem_usage "' + Trunc(memUsageTrackBar.Value).ToString + '" "' + Trunc(cacheUsageTrackBar.Value).ToString + '" ';

            if customCheckbox.IsChecked then
                  execFile[i].script := execFile[i].script + customProp.Text;

            if SettingsForm.HandleCheckBox.IsChecked then
              begin
                execFile[i].script := execFile[i].script + ') > "' + logPath + '.log"';

                if threadsSwitch.IsChecked then
                  LogFiles[i-1] := logPath + '.log'
                else
                  LogFiles[j] := logPath + '.log';
              end;

            //File section
            {$IFDEF MSWINDOWS}
              if compSwitch.IsChecked then
                AssignFile (execFile[i].F, 'C:\ProgramData\AErender\aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.bat')
              else
                AssignFile (execFile[i].F, 'C:\ProgramData\AErender\aerender' + i.ToString + '.bat');
              Rewrite (execFile[i].F);
              Writeln (execFile[i].F, execFile[i].script);
              //Writeln (execFile[i].F, '@PAUSE');
              CloseFile (execFile[i].F);
              if SettingsForm.HandleCheckBox.IsChecked then
                if compSwitch.IsChecked then
                  ShellExecute(0, 'OPEN', PChar('C:\ProgramData\AErender\aerender' + i.ToString + '_' +  compGrid.Cells[0, j] + '.bat'), '', '', SW_HIDE)
                else
                  ShellExecute(0, 'OPEN', PChar('C:\ProgramData\AErender\aerender' + i.ToString + '.bat'), '', '', SW_HIDE)
              else
                if compSwitch.IsChecked then
                  ShellExecute(0, 'OPEN', PChar('C:\ProgramData\AErender\aerender' + i.ToString + '_' +  compGrid.Cells[0, j] + '.bat'), '', '', SW_SHOWNORMAL)
                else
                  ShellExecute(0, 'OPEN', PChar('C:\ProgramData\AErender\aerender' + i.ToString + '.bat'), '', '', SW_SHOWNORMAL)
            {$ENDIF MSWINOWS}
            {$IFDEF MACOS}
              if compSwitch.IsChecked then
                AssignFile (execFile[i].F, GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.command')
              else
                AssignFile (execFile[i].F, GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command');
              Rewrite (execFile[i].F);
              Writeln (execFile[i].F, execFile[i].script);
              //Writeln (execFile[i].F, 'read -p "Press any key to continue..."');
              CloseFile (execFile[i].F);
              if SettingsForm.HandleCheckBox.IsChecked then
                if compSwitch.IsChecked then
                  begin
                    _system(PAnsiChar('chmod +x "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.command"')));
                    _system(PAnsiChar('command "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.command" & disown')));
                  end
                else
                  begin
                    _system(PAnsiChar('chmod +x "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command"')));
                    _system(PAnsiChar('command "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command" & disown')));
                  end
              else
                if compSwitch.IsChecked then
                  begin
                    _system(PAnsiChar('chmod +x "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.command"')));
                    _system(PAnsiChar('open "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.command"')));
                  end
                else
                  begin
                    _system(PAnsiChar('chmod +x "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command"')));
                    _system(PAnsiChar('open "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command"')));
                  end
            {$ENDIF MACOS}
          end;
      {$REGION '  Notifications Invoker [Windows]  '}
      {$IFDEF MSWINDOWS}
      Notification := NotificationC.CreateNotification;
      try
        Notification.Name := 'AERLNotification';
        Notification.AlertBody := 'Rendering Started!';
        Notification.Title := 'AErender Launcher';
        Notification.FireDate := Now;
        NotificationC.PresentNotification(Notification);
      finally
        Notification.DisposeOf;
      end;
      {$ENDIF MSWINDOWS}
      {$ENDREGION}
      Sleep (2000);
      if SettingsForm.HandleCheckBox.IsChecked then
        begin
          if RenderingUnit.VISIBLE then
            RenderingForm.abortRenderingButtonClick(Sender);
          {if compSwitch.IsChecked or outFrame.Text.IsEmpty then
            RenderingForm.TotalProgressBar.Max := Length(LogFiles)
          else
            RenderingForm.TotalProgressBar.Max := outFrame.Text.ToInteger() + (50 * Length(LogFiles));}
          //RenderingForm.framesLabel.Text := '0 / ' + outFrame.Text + ' Frames';
          RenderWindowSender := launchButton;
          RenderingForm.ShowModal;
        end
      else
        //OnRenderStart Actions
        case SettingsForm.onRenderStartBox.ItemIndex of
          1:begin
              WindowState := TWindowState.wsMinimized;
            end;
        end
    end;
end;

procedure TMainForm.openFileClick(Sender: TObject);
{$IFDEF MACOS}
var
  FOpenFile: NSSavePanel;
  NSWin: NSWindow;
begin
  NSWin := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;

  FOpenFile := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);
  FOpenFile.setDirectory(StrToNSStr(DEFPRGPATH));
  FOpenFile.setAllowedFileTypes(ArrayToNSArray(['aep']));
  FOpenFile.setPrompt(StrToNSStr('Open After Effects project'));

  objc_msgSendP2((FOpenFile as ILocalObject).GetObjectID,
                 sel_getUid(PAnsiChar('beginSheetModalForWindow:completionHandler:')),
                 (NSWin as ILocalObject).GetObjectID,
                 TObjCBlock.CreateBlockWithProcedure(
                 procedure (p1: NSInteger)
                 begin
                    if p1 = 0 then
                      // Handle
                    else
                      projectPath.Text := NSStrToStr(FOpenFile.URL.relativePath);
                 end));
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
begin
  with AEPOpenDialog do
    if Execute then
      projectPath.Text := AEPOpenDialog.FileName;
{$ENDIF MSWINDOWS}
end;

procedure TMainForm.outFrameValidate(Sender: TObject; var Text: string);
var 
  tempStr: String;
begin
  try
    tempStr := TMathParser.ParseExpressionToFloat(outFrame.Text).ToString;
  except
    on Exception do
      tempStr := Text;   
  end;
  Text := tempStr;
end;

procedure TMainForm.outModuleEditorItem0Click(Sender: TObject);
begin
  OutputModuleEditorForm.Show;
end;

procedure TMainForm.winOutModuleEditorItemClick(Sender: TObject);
begin
  OutputModuleEditorForm.Show;
end;

procedure TMainForm.outputModuleBoxChange(Sender: TObject);
begin
  if outputModuleBox.ItemIndex = outputModuleBox.Count-1 then
    begin
      OutputModuleEditorForm.Show;
      outputModuleBox.ItemIndex := 0;
    end
  else
    if not outputPath.Text.IsEmpty then
      if tempSavePath.Contains('Def') or tempSavePath.Contains('def') then
        outputPath.Text := ExtractFilePath(tempSavePath) + OutputModules[outputModuleBox.ItemIndex].Mask
      else
        if ExtractFileName(tempSavePath).IsEmpty then
          outputPath.Text := tempSavePath + OutputModules[outputModuleBox.ItemIndex].Mask
        else
          if ExtractFileExt(tempSavePath).IsEmpty then
            outputPath.Text := ExtractFilePath(tempSavePath) + ExtractFileName(tempSavePath) + '_' + OutputModules[outputModuleBox.ItemIndex].Mask
          else
            outputPath.Text := ExtractFilePath(tempSavePath) + StringReplace(ExtractFileName(tempSavePath), ExtractFileExt(tempSavePath), '', [rfReplaceAll, rfIgnoreCase])
                            + '_' + OutputModules[outputModuleBox.ItemIndex].Mask;
end;

procedure TMainForm.projectPathDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
begin
  if Data.Source <> nil then
    projectPath.Text := Data.Source.ClassName
  else
    projectPath.Text := Data.Files[0];
end;

procedure TMainForm.projectPathDragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Link;
end;

procedure TMainForm.saveFileClick(Sender: TObject);
{$IFDEF MACOS}
var
  FSaveFile: NSSavePanel;
  NSWin: NSWindow;
begin
  NSWin := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;

  FSaveFile :=TNSSavePanel.Wrap(TNSSavePanel.OCClass.savePanel);
  FSaveFile.setAccessoryView(CreateMessageView('Tip: Leaving "Default" in the field will use Output Module file name'));
  FSaveFile.setDirectory(StrToNSStr(DEFOUTPATH));
  FSaveFile.setNameFieldLabel(StrToNSStr('Output file name:'));
  FSaveFile.setNameFieldStringValue(StrToNSStr('Default'));
  FSaveFile.setPrompt(StrToNSStr('Save here'));

  objc_msgSendP2((FSaveFile as ILocalObject).GetObjectID,
                  sel_getUid(PAnsiChar('beginSheetModalForWindow:completionHandler:')),
                  (NSWin as ILocalObject).GetObjectID,
                  TObjCBlock.CreateBlockWithProcedure(
                  procedure (p1: NSInteger)
                  begin
                    if p1 = 0 then
                      // Handle
                    else
                      tempSavePath := NSStrToStr(FSaveFile.URL.relativePath);
                      if tempSavePath.Contains('Def') or tempSavePath.Contains('def') then
                        outputPath.Text := ExtractFilePath(tempSavePath) + OutputModules[outputModuleBox.ItemIndex].Mask
                      else
                        if ExtractFileExt(tempSavePath) = '' then
                          outputPath.Text := tempSavePath + '_' + OutputModules[outputModuleBox.ItemIndex].Mask
                        else
                          outputPath.Text := ExtractFilePath(tempSavePath) + StringReplace(ExtractFileName(tempSavePath), ExtractFileExt(tempSavePath), '', [rfReplaceAll, rfIgnoreCase])
                                          + ExtractFileExt(OutputModules[outputModuleBox.ItemIndex].Mask);
                  end));
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
begin
  with SaveDialog1 do
    if Execute then
      begin
        tempSavePath := SaveDialog1.FileName;

        if tempSavePath.Contains('Def') or tempSavePath.Contains('def') then
          outputPath.Text := ExtractFilePath(tempSavePath) + OutputModules[outputModuleBox.ItemIndex].Mask
        else
          if ExtractFileExt(tempSavePath) = '' then
            outputPath.Text := tempSavePath + '_' + OutputModules[outputModuleBox.ItemIndex].Mask
          else
            outputPath.Text := ExtractFilePath(tempSavePath) + StringReplace(ExtractFileName(tempSavePath), ExtractFileExt(tempSavePath), '', [rfReplaceAll, rfIgnoreCase])
                            + ExtractFileExt(OutputModules[outputModuleBox.ItemIndex].Mask);
      end;
{$ENDIF MSWINDOWS}
end;

procedure TMainForm.settingsButtonClick(Sender: TObject);
begin
  SettingsForm.ShowModal;
end;

procedure TMainForm.compCountChange(Sender: TObject);
begin
  compGrid.RowCount := Round(compCount.Value);
end;

procedure TMainForm.threadsCountChange(Sender: TObject);
begin
  threadsGrid.RowCount := threadsCount.Items[threadsCount.ItemIndex].ToInteger();
  if not outFrame.Text.IsEmpty then
    calculateButtonClick(Sender);
end;

procedure TMainForm.threadsSwitchSwitch(Sender: TObject);
begin
  if threadsSwitch.IsChecked then
    begin
      if UpdateAvailable = True then
        begin
          if MainForm.Height <= 470 then
            MainForm.Height := MainForm.Height + 130;
        end
      else
        if MainForm.Height <= 420 then
          MainForm.Height := MainForm.Height + 130;
      //if LANG = 'EN' then
        threadsSwitchLabel.Text := 'Split Render';
      {if LANG = 'RU' then
        threadsSwitchLabel.Text := 'Рендерить частями';}
      threadsGrid.AniCalculations.AutoShowing := False;
      threadsGrid.Model.ScrollDirections := TScrollDirections.Vertical;
    end
  else
    begin
      if UpdateAvailable = True then
        begin
          if MainForm.Height >= 580 then
            MainForm.Height := MainForm.Height - 130
        end
      else
        if MainForm.Height <= 550 then
          MainForm.Height := MainForm.Height - 130;
      //if LANG = 'EN' then
        threadsSwitchLabel.Text := 'Single Render';
      {if LANG = 'RU' then
        threadsSwitchLabel.Text := 'Рендерить одним файлом';}
    end;
end;

procedure TMainForm.memUsageInfoClick(Sender: TObject);
begin
  memUsageInfoEdit.Text := Trunc((GetPlatformMemorySize/1024/1024) * (memUsageTrackBar.Value / 100)).ToString + ' MB';

  memUsageInfo.Visible := False;
  memUsageInfo.Enabled := False;
  memUsageInfoEdit.Visible := True;
  memUsageInfoEdit.Enabled := True;

  memUsageInfoEdit.ResetSelection;
  memUsageInfoEdit.SelectAll;
end;

procedure TMainForm.memUsageInfoEditExit(Sender: TObject);
begin
  memUsageInfo.Visible := True;
  memUsageInfo.Enabled := True;
  memUsageInfoEdit.Visible := False;
  memUsageInfoEdit.Enabled := False;
end;

procedure TMainForm.memUsageInfoEditValidate(Sender: TObject;
  var Text: string);
var
  tempText: String;
begin
  tempText := memUsageInfoEdit.Text;
  if (tempText.Contains ('unl') or tempText.Contains('max')) then
    memUsageInfoEdit.Text := Trunc(GetPlatformMemorySize / 1024 / 1024 + 1).ToString
  else
    begin
      if (((tempText.Contains ('mb')) or (tempText.Contains ('Mb')) or (tempText.Contains ('MB')) or (tempText.Contains ('mB')))
          or not (tempText[tempText.Length].IsDigit)) then
        memUsageInfoEdit.Text := memUsageInfoEdit.Text.Remove(memUsageInfoEdit.Text.Length - 3);

      if (tempText.Contains ('kb')) or (tempText.Contains ('Kb')) or (tempText.Contains ('KB')) or (tempText.Contains ('kB')) then
        memUsageInfoEdit.Text := (memUsageInfoEdit.Text.ToInteger() / 1024).ToString;    //KB -> MB

      if (tempText.Contains ('gb')) or (tempText.Contains ('Gb')) or (tempText.Contains ('GB')) or (tempText.Contains ('gB')) then
        memUsageInfoEdit.Text := (memUsageInfoEdit.Text.ToInteger() * 1024).ToString;    //GB -> MB
    end;
  if tempText.Contains ('%') then
    begin
      if tempText[tempText.IndexOf('%') - 1] = ' ' then
        memUsageInfoEdit.Text := memUsageInfoEdit.Text.Remove(memUsageInfoEdit.Text.Length - 2)
      else
        memUsageInfoEdit.Text := memUsageInfoEdit.Text.Remove(memUsageInfoEdit.Text.Length - 1);
    end
  else
    begin
      tempText := '';
      try
        memUsageTrackBar.Value := (100 * memUsageInfoEdit.Text.ToInteger()) / (GetPlatformMemorySize / 1024 / 1024);
      except
        on Exception do
          begin
            //if LANG = 'EN' then
              TDialogServiceSync.MessageDialog((Text + ' is not a valid memory value. Try <value unit> or <value> instead.'), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
            //if LANG = 'RU' then
            //  MessageDlg((Text + ' недействительное значение памяти. Используйте <value unit> или <value>.'), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
          end;
      end;
    end;
  memUsageInfo.Visible := True;
  memUsageInfo.Enabled := True;
  memUsageInfoEdit.Visible := False;
  memUsageInfoEdit.Enabled := False;
end;

procedure TMainForm.memUsageTrackBarChange(Sender: TObject);
begin
  if memUsageTrackBar.Value = 100 then
    memUsageInfo.Text := 'Unlimited'
  else
    memUsageInfo.Text := Trunc(memUsageTrackBar.Value).ToString + '% (' + Trunc((GetPlatformMemorySize/1024/1024) * (memUsageTrackBar.Value / 100)).ToString + ' MB)';
end;

procedure TMainForm.cacheUsageInfoClick(Sender: TObject);
begin
  cacheUsageInfoEdit.Text := Trunc(cacheUsageTrackBar.Value).ToString + '%';

  cacheUsageInfo.Visible := False;
  cacheUsageInfo.Enabled := False;
  cacheUsageInfoEdit.Visible := True;
  cacheUsageInfoEdit.Enabled := True;

  cacheUsageInfoEdit.ResetSelection;
  cacheUsageInfoEdit.SelectAll;
end;

procedure TMainForm.cacheUsageInfoEditExit(Sender: TObject);
begin
  cacheUsageInfo.Visible := True;
  cacheUsageInfo.Enabled := True;
  cacheUsageInfoEdit.Visible := False;
  cacheUsageInfoEdit.Enabled := False;
end;

procedure TMainForm.cacheUsageInfoEditValidate(Sender: TObject;
  var Text: string);
var
  tempText: String;
begin
  tempText := cacheUsageInfoEdit.Text;
  try
    if tempText.Contains ('%') then
      if tempText[tempText.IndexOf('%') - 1] = ' ' then
        cacheUsageInfoEdit.Text := cacheUsageInfoEdit.Text.Remove(cacheUsageInfoEdit.Text.Length - 2)
      else
        cacheUsageInfoEdit.Text := cacheUsageInfoEdit.Text.Remove(cacheUsageInfoEdit.Text.Length - 1)
    else
      if tempText.Contains ('unlim') or tempText.Contains ('max') then
        cacheUsageInfoEdit.Text := '100';
  except
    on Exception do
      begin
        //if LANG = 'EN' then
          TDialogServiceSync.MessageDialog((Text + ' is invalid percentage value. Try <value %> or <value> instead.'), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
        //if LANG = 'RU' then
        //  MessageDlg((Text + ' недействительное процентное значение. Используйте <value %> или <value>.'), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      end;
  end;
  tempText := '';
  cacheUsageTrackBar.Value := cacheUsageInfoEdit.Text.ToSingle();

  cacheUsageInfo.Visible := True;
  cacheUsageInfo.Enabled := True;
  cacheUsageInfoEdit.Visible := False;
  cacheUsageInfoEdit.Enabled := False;
end;

procedure TMainForm.cacheUsageTrackBarChange(Sender: TObject);
begin
  if cacheUsageTrackBar.Value = 100 then
    cacheUsageInfo.Text := 'Unlimited'
  else
    cacheUsageInfo.Text := Trunc(cacheUsageTrackBar.Value).ToString + '%';
end;

{$ENDREGION}

end.
