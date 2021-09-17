unit MainUnit;

(*        AErender Launcher                                                                 *)
(*        MainUnit.pas                                                                      *)
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

{$DEFINE DEBUG_MODE}

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
  System.Math,
  System.Notification,
  System.ImageList,
  System.Net.URLClient,
  System.Net.HttpClientComponent,
  System.Generics.Collections,
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
  FMX.ComboEdit,
  FMX.Text,
  FMX.BufferedLayout,
  FMX.CompLabel,
  FMX.TreeView,
  FMX.TabControl,
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
  AErenderLauncher.Types,
  AErenderLauncher.Localization,
  AErenderLauncher.IO,
  AErenderLauncher.Rendering,
  AErenderLauncher.Math,
  AErenderLauncher.Math.ExpParser,
  AErenderLauncher.SysUtils,
  {$ENDREGION}

  {$REGION '    Windows Only Libraries    '}{$IFDEF MSWINDOWS}
    Winapi.ShellAPI, Winapi.Windows, FMX.Platform.Win, Winapi.TlHelp32, WinApi.DwmApi, WinApi.UxTheme, WinApi.Messages;
  {$ENDIF MSWINDOWS}{$ENDREGION}

  {$REGION '    macOS Only Libraries    '}{$IFDEF MACOS}
    Posix.Stdlib, Posix.Unistd, Posix.SysSysctl, Posix.SysTypes, FMX.Platform.Mac,
    Mac.CodeBlocks, Macapi.Appkit, Macapi.ObjectiveC, Macapi.Foundation,
    Macapi.Dialogs, Macapi.Helpers, Macapi.ObjCRuntime, Macapi.CocoaTypes;
  {$ENDIF MACOS}{$ENDREGION}

type
  SizeConstraints = record
  private
    const
      MIN_WIDTH         = 640;

      MIN_HEIGHT_NO_UPD = 540;
      //EXP_HEIGHT_NO_UPD = 540;

      MIN_HEIGHT_UPD    = 576;         // 540 + 36
      //EXP_HEIGHT_UPD    = 576;
  end;
  TMainForm = class(TForm)
    _projectPathLabel: TLabel;
    inputFileLayout: TLayout;
    _outputPathLabel: TLabel;
    InputLabels: TLayout;
    pathLayout: TLayout;
    _projectPath: TEdit;
    fileChooseLayout: TLayout;
    _outputPath: TEdit;
    openFile: TButton;
    _saveFile: TButton;
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
    startFrameLabel: TLabel;
    inFrame: TEdit;
    endFrameLabel: TLabel;
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
    AEPOpenDialog: TOpenDialog;
    SaveDialog1: TSaveDialog;
    LinkControlToPropertyEnabled8: TLinkControlToProperty;
    LinkControlToPropertyEnabled9: TLinkControlToProperty;
    OnyxBlueStyle: TStyleBook;
    infoButton: TButton;
    launcherLayout: TLayout;
    outputModuleBox: TComboBox;
    outputModuleLabel: TLabel;
    outputModuleLayout: TLayout;
    renderSettingsLayout: TLayout;
    renderSettingsBox: TComboBox;
    renderSettingsLabel: TLabel;
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
    AERModernAnimatedStyle: TStyleBook;
    UpdateLabel: TLabel;
    downloadButton: TButton;
    ffmpegCheckBox: TCheckBox;
    ffmpegConfigButton: TButton;
    ffmpegConcateLayout: TLayout;
    LinkControlToPropertyEnabled11: TLinkControlToProperty;
    mainLayout: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    GridPanelLayout2: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    GridPanelLayout3: TGridPanelLayout;
    UpdateInfo: TStatusBar;
    outModuleEditorItem0: TMenuItem;
    editItem: TMenuItem;
    outModuleEditorItem: TMenuItem;
    settingsItem: TMenuItem;
    SettingsIcon: TPath;
    InfoIcon: TPath;
    LaunchIcon: TPath;
    threadsCount: TComboEdit;
    LinkControlToPropertyVisible7: TLinkControlToProperty;
    LinkControlToPropertyEnabled10: TLinkControlToProperty;
    UpdateNetHTTPClient: TNetHTTPClient;
    recentItem: TMenuItem;
    separatorItem3: TMenuItem;
    TasksToolbar: TToolBar;
    TasksLabel: TLabel;
    RemoveTaskButton: TButton;
    AddTaskButton: TButton;
    TasksTreeView: TTreeView;
    TasksPopupMenu: TPopupMenu;
    AddTaskPopupItem: TMenuItem;
    EditTaskPopupItem: TMenuItem;
    DeleteTaskPopupItem: TMenuItem;
    TaskEditorPopup: TPopup;
    PopupAniOpenHeight: TFloatAnimation;
    PopupAniCloseHeight: TFloatAnimation;
    TaskEditorCompButton: TButton;
    ToolbarPopupMenu: TPopupMenu;
    ToolbarIconsOnly: TMenuItem;
    ToolbarIconsAndText: TMenuItem;
    ToolbarTextOnly: TMenuItem;
    TaskEditorTabControl: TTabControl;
    TaskEditorStepOne: TTabItem;
    TaskEditorStepTwo: TTabItem;
    TaskEditorStepOneStatusBar: TStatusBar;
    TaskEditorCancelButton: TButton;
    TaskEditorStepTwoStatusBar: TStatusBar;
    TaskEditorSaveButton: TButton;
    Button3: TButton;
    TaskEditorProjectButton: TButton;
    TaskEditorStepOneToolbar: TToolBar;
    TaskEditorStepOneLabel: TLabel;
    TaskEditorStepTwoToolbar: TToolBar;
    NewTaskStepTwoLabel: TLabel;
    TaskEditorCompGrid: TStringGrid;
    CompositionCol: TStringColumn;
    StartFrameCol: TIntegerColumn;
    EndFrameCol: TIntegerColumn;
    SplitCol: TIntegerColumn;
    TaskEditorCompCount: TSpinBox;
    TaskEditorCompCountLabel: TLabel;
    CompNumCol: TIntegerColumn;
    projectPath: TEdit;
    projectPathLabel: TLabel;
    Layout3: TLayout;
    outputPathLabel: TLabel;
    outputPath: TEdit;
    saveFile: TButton;
    GridPanelLayout4: TGridPanelLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TasksIsEmptyLabel: TLabel;
    TasksIsEmpty: TLayout;
    TeskIsEmptyAddLabel: TLabel;
    Label5: TLabel;
    DebugLabel: TLabel;
    TaskEditorCreateTaskButton: TButton;
    TasksPopupSeparator1: TMenuItem;
    TaskItemDisplay: TMenuItem;
    PopupAniCloseWidth: TFloatAnimation;
    PopupAniOpenWidth: TFloatAnimation;
    ProjectReaderAwait: TTimer;
    ProjectAwaitMessage: TPanel;
    ProjectAwaitIndicator: TAniIndicator;
    ShadowEffect1: TShadowEffect;
    ProjectAwaitLabel: TLabel;
    ProjectAwait: TLayout;
    ProjectAwaitStatusLabel: TLabel;
    Label6: TLabel;
    launchPopupBox: TPopupBox;
    procedure FormResize(Sender: TObject);
    procedure compSwitchSwitch(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure compCountChange(Sender: TObject);
    procedure settingsButtonClick(Sender: TObject);
    procedure threadsSwitchSwitch(Sender: TObject);
    procedure calculateButtonClick(Sender: TObject);
    procedure threadsCount1Change(Sender: TObject);
    procedure openFileClick(Sender: TObject);
    procedure launchButtonClick(Sender: TObject);
    procedure _saveFileClick(Sender: TObject);
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
    procedure cacheUsageInfoEditValidate(Sender: TObject; var Text: string);
    procedure cacheUsageInfoClick(Sender: TObject);
    procedure cacheUsageInfoEditExit(Sender: TObject);
    procedure memUsageInfoEditExit(Sender: TObject);
    procedure downloadButtonClick(Sender: TObject);
    procedure ffmpegConfigButtonClick(Sender: TObject);
    procedure _projectPathDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure _projectPathDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure outFrameValidate(Sender: TObject; var Text: string);
    procedure outputModuleBoxChange(Sender: TObject);
    procedure compNameTyping(Sender: TObject);
    procedure outModuleEditorItem0Click(Sender: TObject);
    procedure winOutModuleEditorItemClick(Sender: TObject);
    procedure inFrameValidate(Sender: TObject; var Text: string);
    procedure threadsCountChange(Sender: TObject);
    procedure UpdateNetHTTPClientRequestError(const Sender: TObject; const AError: string);
    procedure UpdateNetHTTPClientRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);

    function  ReadAERQ(Path: String): TObjectList<TRenderTask>;
    function  ReadAER(Path: String): TRenderTask;

    procedure SetLanguage(LanguageCode: Integer);
    procedure UpdateOutputModules;
    procedure UpdateRenderSettings;

    procedure threadsCountTyping(Sender: TObject);
    procedure threadsCountExit(Sender: TObject);

    procedure AddToRecents(Item: String);
    procedure RecentProjectSelected(Sender: TObject);

    procedure InflateRecents;
    procedure ReInflateRecents;

    procedure compGridEditingDone(Sender: TObject; const ACol, ARow: Integer);
    procedure renderSettingsBoxChange(Sender: TObject);
    procedure TasksTreeViewDragChange(SourceItem, DestItem: TTreeViewItem; var Allow: Boolean);
    procedure TasksTreeViewKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure TasksTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure TasksPopupMenuPopup(Sender: TObject);

    procedure CheckAndPopup(const APath: String; const TryParse: Boolean);

    //procedure TaskAddProcess;
    procedure ExtractParserToAppDir;

    procedure BeginTryParseAEP(const APath: String);

    procedure AddTaskButtonClick(Sender: TObject);
    procedure RemoveTaskButtonClick(Sender: TObject);
    procedure PopupAniOpenFinish(Sender: TObject);
    procedure TaskEditorCancelButtonClick(Sender: TObject);
    procedure PopupAniCloseFinish(Sender: TObject);
    procedure TaskEditorPopupPopup(Sender: TObject);
    procedure TaskEditorCompButtonClick(Sender: TObject);
    procedure TaskEditorProjectButtonClick(Sender: TObject);
    procedure TaskEditorCreateTaskButtonClick(Sender: TObject);
    procedure TaskEditorCompCountChange(Sender: TObject);
    procedure TaskEditorTabControlChange(Sender: TObject);
    procedure TaskEditorCompGridEditingDone(Sender: TObject; const ACol, ARow: Integer);

    procedure UpdateTasksList;

    procedure RenderTasksOnNotify(Sender: TObject; const Item: TRenderTask; Action: TCollectionNotification);

    procedure TasksTreeViewDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);

    procedure TasksTreeViewItemDblClick(Sender: TObject);
    procedure TaskEditorSaveButtonClick(Sender: TObject);
    procedure ProjectReaderAwaitTimer(Sender: TObject);
    procedure TaskEditorToolbarMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }
    FCurrentItem: String;
    //Settings: TSettings;
    TMathParser: TExpressionParser;

    //{$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
    {$IFDEF MSWINDOWS}procedure WMNCPaint(var AMessage: TMessage); message WM_NCPAINT;{$ENDIF}
  public
    { Public declarations }
    ImportableProjectPath: String;
    ProjectJson: String;
    ProjectJsonPath: String;

    procedure DragEnter(const Data: TDragObject; const Point: TPointF); override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
  end;

  function GetPlatformMemorySize: Int64;
  function GetFFMPEGPath: WideString;
  function KillProcess(ProcessName: String): Integer;
  procedure InitOutputModules;
  procedure InitRenderSettings;
  procedure InitConfiguration(Path: String);
  procedure LoadConfiguration(Path: String);
  procedure LoadLegacyConfiguration(Path: String);
  procedure SaveConfiguration(Path: String);
  procedure ChangeLanguage(LanguageCode: Integer);
  procedure InitLanguage(PATH: String);

const
  APPVERSION = 'v0.8.9-beta';
  AERL_REPO_RELEASES = 'https://api.github.com/repos/lilystilson/aerender-launcher/releases';
  PLATFORMPATHSEPARATOR = {$IFDEF MSWINDOWS}'\'{$ENDIF MSWINDOWS}
                          {$IFDEF MACOS}'/'{$ENDIF MACOS};
  AWAIT_TIMEOUT = 10000;
  //MAX_SIMULTANEOUS_THREADS = 8;


var
  MainForm: TMainForm;                                      (*  Main Form Declaration                       *)
  APPFOLDER, VER, AERPATH, DEFPRGPATH, DEFOUTPATH,
  ERR, gitResponse, gitVersion, gitDownload, ffmpegPath,
  AERH, tempSavePath, DelTempFiles: String;                 (*  Required variables for configuration file   *)
  gitRelease: TJsonValue;                                   (*  GitHub API's returning JSON file            *)
  UpdateAvailable: Boolean = False;                         (*  Represents app updates availability         *)
  FFMPEG: Boolean = False;                                  (*  Represents FFMPEG availability              *)
  AEParser: String = '';
  RenderWindowSender: TButton;                              (*  Who opened Rendering window?                *)
  LANG, STYLE, ONRENDERSTART: Integer;                      (*  Language, Theme and OnRenderStart values    *)
  LogFiles: TArray<String>;                                 (*  All the aerender log files here             *)
  OutputModules: TArray<OutputModule>;                      (*  All the After Effects output modules here   *)
  RenderSettings: TArray<RenderSetting>;
  OMCount: Cardinal;
  //TempOutputModule: OutputModule;                         (*  Temporary Output Module used from import    *)
  //TMathParser: TExpressionParser;                           (*  Mathematical parser for frames calculation  *)

  FHandleDragDirectly: Boolean = False;                     (*  For implementation of DragDrop functional   *)

  PARAMSTART: Boolean = False;

  isRendering: Boolean = False;

  TryParseAEP: Boolean = True;

  Language: TArray<LauncherText>;

  Recents: array [0..9] of String;
  RecentsMenuItems: TArray<TMenuItem>;

  TempThreadsStr: String = '';
  ImportedPath: String = '';

  AwaitElapsedTime: Cardinal = 0;

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

  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or  CS_DROPSHADOW);

  SystemParametersInfo(SPI_SETDROPSHADOW, 0, PVOID(True), 0);

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
begin
  {$IFDEF MSWINDOWS}
    var MS_Ex: MemoryStatusEx;
    FillChar(MS_Ex, SizeOf(MemoryStatusEx), #0);
    MS_Ex.dwLength := SizeOf(MemoryStatusEx);
    GlobalMemoryStatusEx(MS_Ex);
    Result := MS_Ex.ullTotalPhys;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    var len: size_t := SizeOf (Result);
    var res: Int64 := SysCtlByName ('hw.memsize', @Result, @len, nil, 0);
    if res <> 0 then
      RaiseLastOSError;
  {$ENDIF MACOS}
end;

function GetFFMPEGPath: WideString;
var
  AERenderDirectory: String;
  Folders: System.TArray<String>;
begin
  {$IFDEF MSWINDOWS}AERenderDirectory := 'C:\ProgramData\AErender';{$ENDIF MSWINDOWS}
  {$IFDEF MACOS}AERenderDirectory := GetEnvironmentVariable('HOME') + '/Documents/AErender/';{$ENDIF}

  Folders := TDirectory.GetDirectories(APPFOLDER);
  for var i := 0 to High(Folders) do
    if Folders[i].Contains('ffmpeg') then begin
      Result := Folders[i];
      break
    end else
      Result := '';
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
        TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0), FProcessEntry32.th32ProcessID), 0);
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

procedure InitLanguage(PATH: String);
begin
  //LANG := 1;
  var ResourceEN: TResourceStream := TResourceStream.Create(HInstance, 'Language_EN', RT_RCDATA);
  var ResourceRU: TResourceStream := TResourceStream.Create(HInstance, 'Language_RU', RT_RCDATA);
  Language := [
                LauncherText.InitFromResourceStream(ResourceEN),
                LauncherText.InitFromResourceStream(ResourceRU)
              ];

  ChangeLanguage(LANG);
end;

procedure InitOutputModules;
begin
  SetLength (OutputModules, 10);

  OutputModules[0].Module := 'Lossless';
  OutputModules[0].Mask := '[compName].[fileExtension]';
  OutputModules[0].Imported := False;

  OutputModules[1].Module := 'AIFF 48kHz';
  OutputModules[1].Mask := '[compName].[fileExtension]';
  OutputModules[1].Imported := False;

  OutputModules[2].Module := 'Alpha Only';
  OutputModules[2].Mask := '[compName].[fileExtension]';
  OutputModules[2].Imported := False;

  OutputModules[3].Module := 'AVI DV NTSC 48kHz';
  OutputModules[3].Mask := '[compName].[fileExtension]';
  OutputModules[3].Imported := False;

  OutputModules[4].Module := 'AVI DV PAL 48kHz';
  OutputModules[4].Mask := '[compName].[fileExtension]';
  OutputModules[4].Imported := False;

  OutputModules[5].Module := 'Lossless with Alpha';
  OutputModules[5].Mask := '[compName].[fileExtension]';
  OutputModules[5].Imported := False;

  OutputModules[6].Module := 'Multi-Machine Sequence';
  OutputModules[6].Mask := '[compName]_[#####].[fileExtension]';
  OutputModules[6].Imported := False;

  OutputModules[7].Module := 'Photoshop';
  OutputModules[7].Mask := '[compName]_[#####].[fileExtension]';
  OutputModules[7].Imported := False;

  OutputModules[8].Module := 'Save Current Preview';
  OutputModules[8].Mask := '[compName].[fileExtension]';
  OutputModules[8].Imported := False;

  OutputModules[9].Module := 'TIFF Sequence with Alpha';
  OutputModules[9].Mask := '[compName]_[#####].[fileExtension]';
  OutputModules[9].Imported := False;
end;

procedure InitRenderSettings;
begin
  SetLength (RenderSettings, 5);

  RenderSettings[0].Setting   := 'Current Settings';
  RenderSettings[0].Imported  := False;

  RenderSettings[1].Setting   := 'Best Settings';
  RenderSettings[1].Imported  := False;

  RenderSettings[2].Setting   := 'DV Settings';
  RenderSettings[2].Imported  := False;

  RenderSettings[3].Setting   := 'Draft Settings';
  RenderSettings[3].Imported  := False;

  RenderSettings[4].Setting   := 'Multi-Machine Settings';
  RenderSettings[4].Imported  := False;

  //RenderSettings[4].Setting   := 'Custom';
  //RenderSettings[4].Imported  := False;
end;

procedure InitConfiguration(Path: String);
var
  Config: IXMLDocument;
  RootNode: IXMLNode;
  ChildNode: IXMLNode;
begin
  //InitLanguage;
  InitOutputModules;
  InitRenderSettings;

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

  for var i := 0 to High(OutputModules) do begin
    var ModuleNode: IXMLNode := RootNode.ChildNodes['outputModule'].AddChild('module');
    ModuleNode.AddChild('moduleName').Text := OutputModules[i].Module;
    ModuleNode.Attributes['imported'] := 'False';
    ModuleNode.AddChild('filemask').Text := OutputModules[i].Mask;
  end;

  MainForm.outputModuleBox.ItemIndex := 0;

  ChildNode := RootNode.AddChild('renderSettings');
  ChildNode.Attributes['selected'] := '0';

  for var i := 0 to High(RenderSettings) do begin
    var SettingNode: IXMLNode := RootNode.ChildNodes['renderSettings'].AddChild('setting');
    SettingNode.Text := RenderSettings[i].Setting;
    SettingNode.Attributes['imported'] := 'False';
  end;

  MainForm.renderSettingsBox.ItemIndex := 0;

  ChildNode := RootNode.AddChild('recentProjects');
  for var i := 0 to 9 do
    ChildNode.AddChild('project').Text := '(empty)';

  Config.SaveToFile(Path);
end;

procedure LoadLegacyConfiguration(Path: String);
var
  Config: IXMLDocument;
  RootNode: IXMLNode;
begin
  Config := TXMLDocument.Create(nil);
  Config.LoadFromFile(Path);
  Config.Active := True;
  RootNode := Config.DocumentElement;

  LANG := 0;
  STYLE := RootNode.ChildNodes['style'].Text.ToInteger();
  AErenderPath := RootNode.ChildNodes['aerender'].Text;
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
  MainForm.customProp.Enabled := StrToBool(RootNode.ChildNodes['prop'].Attributes['enabled']);
  MainForm.customProp.Text := RootNode.ChildNodes['prop'].Text;

  MainForm.memUsageTrackBar.Value := RootNode.ChildNodes['memoryLimit'].Text.ToSingle();
  MainForm.cacheUsageTrackBar.Value := RootNode.ChildNodes['cacheLimit'].Text.ToSingle();

  //MainUnit.OMCount := RootNode.ChildNodes['outputModule'].ChildNodes.Count;
  SetLength (OutputModules, RootNode.ChildNodes['outputModule'].ChildNodes.Count);
  for var i := 0 to High(OutputModules) do
    begin
      OutputModules[i].Module := RootNode.ChildNodes['outputModule'].ChildNodes[i].ChildNodes['moduleName'].Text;
      OutputModules[i].Mask := RootNode.ChildNodes['outputModule'].ChildNodes[i].ChildNodes['filemask'].Text;
      OutputModules[i].Imported := False;
    end;

  InitRenderSettings;

  MainForm.outputModuleBox.ItemIndex := StrToInt(RootNode.ChildNodes['outputModule'].Attributes['selected']);
  MainForm.renderSettingsBox.ItemIndex := 0;
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
  AErenderPath := RootNode.ChildNodes['aerender'].Text;
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
  MainForm.customProp.Enabled := StrToBool(RootNode.ChildNodes['prop'].Attributes['enabled']);
  MainForm.customProp.Text := RootNode.ChildNodes['prop'].Text;

  MainForm.memUsageTrackBar.Value := RootNode.ChildNodes['memoryLimit'].Text.ToSingle();
  MainForm.cacheUsageTrackBar.Value := RootNode.ChildNodes['cacheLimit'].Text.ToSingle();

  if RootNode.ChildNodes['outputModule'].ChildNodes.Count <> 0 then begin
    SetLength (OutputModules, RootNode.ChildNodes['outputModule'].ChildNodes.Count);
    for var i := 0 to High(OutputModules) do begin
      OutputModules[i].Module := RootNode.ChildNodes['outputModule'].ChildNodes[i].ChildNodes['moduleName'].Text;
      OutputModules[i].Mask := RootNode.ChildNodes['outputModule'].ChildNodes[i].ChildNodes['filemask'].Text;
      OutputModules[i].Imported := StrToBool(RootNode.ChildNodes['outputModule'].ChildNodes[i].Attributes['imported']);
    end;
    MainForm.outputModuleBox.ItemIndex := StrToInt(RootNode.ChildNodes['outputModule'].Attributes['selected']);
  end else
    InitOutputModules;

  if RootNode.ChildNodes['renderSettings'].ChildNodes.Count <> 0 then begin
    SetLength (RenderSettings, RootNode.ChildNodes['renderSettings'].ChildNodes.Count);
    for var i := 0 to High(RenderSettings) do begin
      RenderSettings[i].Setting := RootNode.ChildNodes['renderSettings'].ChildNodes[i].Text;
      RenderSettings[i].Imported := StrToBool(RootNode.ChildNodes['renderSettings'].ChildNodes[i].Attributes['imported']);
    end;
    MainForm.renderSettingsBox.ItemIndex := StrToInt(RootNode.ChildNodes['renderSettings'].Attributes['selected']);
  end else
    InitRenderSettings;

  for var i := 0 to 9 do begin
    Recents[i] := RootNode.ChildNodes['recentProjects'].ChildNodes[i].Text;
  end;
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
  RootNode.AddChild('aerender').Text := AErenderPath;
  RootNode.AddChild('onRenderStart').Text := ONRENDERSTART.ToString;
  RootNode.AddChild('defprgpath').Text := DEFPRGPATH;
  RootNode.AddChild('defoutpath').Text := DEFOUTPATH;
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
  for var i := 0 to High(OutputModules) do begin
    var ModuleNode: IXMLNode := RootNode.ChildNodes['outputModule'].AddChild('module');
    ModuleNode.Attributes['imported'] := BoolToStr(OutputModules[i].Imported, True);
    ModuleNode.AddChild('moduleName').Text := OutputModules[i].Module;
    ModuleNode.AddChild('filemask').Text := OutputModules[i].Mask;
  end;

  ChildNode := RootNode.AddChild('renderSettings');
  ChildNode.Attributes['selected'] := MainForm.renderSettingsBox.ItemIndex.ToString;
  for var i := 0 to High(RenderSettings) do begin
    var SettingNode: IXMLNode := RootNode.ChildNodes['renderSettings'].AddChild('setting');
    SettingNode.Text := RenderSettings[i].Setting;
    SettingNode.Attributes['imported'] := RenderSettings[i].Imported;
  end;

  ChildNode := RootNode.AddChild('recentProjects');
  for var i := 0 to 9 do begin
    if Recents[i] = '' then
      ChildNode.AddChild('project').Text := '(empty)'
    else
      ChildNode.AddChild('project').Text := Recents[i]
  end;

  Config.SaveToFile(Path);
end;

function GetOMIndex(AOutputModule: OutputModule): Integer;
begin
  Result := -1;
  for var i := 0 to High(OutputModules) do
    if (AOutputModule.Module = OutputModules[i].Module) and (AOutputModule.Mask = OutputModules[i].Mask) then begin
      Result := i;
      break;
    end;
end;

function GetRSIndex(ARenderSetting: RenderSetting): Integer;
begin
  Result := -1;
  for var i := 0 to High(RenderSettings) do
    if ARenderSetting.Setting = RenderSettings[i].Setting then begin
      Result := i;
      break;
    end;
end;

procedure ChangeLanguage(LanguageCode: Integer);
begin
  MainForm.SetLanguage(LanguageCode);
  SettingsForm.SetLanguage(LanguageCode);
  ImportForm.SetLanguage(LanguageCode);
  AboutForm.SetLanguage(LanguageCode);
  RenderingForm.SetLanguage(LanguageCode);
  OutputModuleEditorForm.SetLanguage(LanguageCode);
end;

function GetRecentIndex(Item: String): Integer;
begin
  for var i := 0 to 9 do
    if Item = Recents[i] then begin
      Result := i;
      break
    end else
      Result := -1;
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

/// This will add from Recents[] array every instance to the
/// recentsItem
procedure TMainForm.InflateRecents;
begin
  SetLength(RecentsMenuItems, 10);
  for var i := 0 to High(RecentsMenuItems) do begin
    if (Recents[i] <> '(empty)') and (Recents[i] <> '') then begin
      try
        RecentsMenuItems[i] := TMenuItem.Create(Self);
        RecentsMenuItems[i].Text := Recents[i];
        RecentsMenuItems[i].Parent := recentItem;
        RecentsMenuItems[i].OnClick := RecentProjectSelected;
      except
        on Exception do
          //
      end;
    end;
  end;
end;

/// This will FreeAndNil every menu item from recentItem
/// and then populate it again anew
procedure TMainForm.ReInflateRecents;
begin
  for var i := 0 to 9 do begin
    FreeAndNil(RecentsMenuItems[i]);
  end;

  Finalize(RecentsMenuItems);

  InflateRecents;
end;

procedure TMainForm.RenderTasksOnNotify(Sender: TObject; const Item: TRenderTask; Action: TCollectionNotification);
begin
  TasksIsEmpty.Visible := RenderTasks.Count = 0;
  /// Because method [Clear()] calls [BeginUpdate()] and [EndUpdate()]
  /// we call it first here
  TasksTreeView.Clear;

  TasksTreeView.BeginUpdate;
  for var Task: TRenderTask in RenderTasks do begin
    var ProjectItem: TTreeViewItem := TTreeViewItem.Create(nil);
    ProjectItem.Text := ExtractFileName(Task.Project);
    ProjectItem.Parent := TasksTreeView;
    ProjectItem.StyledSettings := [TStyledSetting.FontColor, TStyledSetting.Family, TStyledSetting.Size];
    ProjectItem.TextSettings.Font.Style := [TFontStyle.fsBold];
    ProjectItem.IsExpanded := True;
    ProjectItem.OnDblClick := TasksTreeViewItemDblClick;

    for var Comp: TComposition in Task.Compositions do begin
      var CompItem: TTreeViewItem := TTreeViewItem.Create(nil);
      CompItem.Text := Comp.CompName;
      CompItem.Parent := ProjectItem;
      CompItem.OnDblClick := TasksTreeViewItemDblClick;

      var CompLabel: TCompLabel := TCompLabel.Create(nil);
      CompLabel.Parent := CompItem;
      CompLabel.Align := TAlignLayout.Client;
      CompLabel.InFrame := Comp.Frames.StartFrame;
      CompLabel.OutFrame := Comp.Frames.EndFrame;
      CompLabel.Split := IntToStr(Comp.Split);
      CompLabel.Value := -1;//Comp.Frames.StartFrame;
    end;
  end;
  TasksTreeView.EndUpdate;

  TasksTreeView.AllowDrag := True;
end;

procedure TMainForm.UpdateTasksList;
begin
//  TasksTreeView.BeginUpdate;
//  ///
//  TasksTreeView.Clear;
//
//  for var Task: TRenderTask in RenderTasks do begin
//    var ProjectItem: TTreeViewItem := TTreeViewItem.Create(nil);
//    ProjectItem.Text := ExtractFileName(Task.Project);
//    ProjectItem.Parent := TasksTreeView;
//    ProjectItem.StyledSettings := [TStyledSetting.FontColor, TStyledSetting.Family, TStyledSetting.Size];
//    ProjectItem.TextSettings.Font.Style := [TFontStyle.fsBold];
//    ProjectItem.IsExpanded := True;
//
//    for var Comp: TComposition in Task.Compositions do begin
//      var CompItem: TTreeViewItem := TTreeViewItem.Create(nil);
//      CompItem.Text := Comp.CompName;
//      CompItem.Parent := ProjectItem;
//
//      var СompLabel: TCompLabel := TCompLabel.Create(nil);
//      СompLabel.Parent := CompItem;
//      СompLabel.Align := TAlignLayout.Client;
//      СompLabel.InFrame := IntToStr(Comp.Frames.StartFrame);
//      СompLabel.OutFrame := IntToStr(Comp.Frames.EndFrame);
//      СompLabel.Split := IntToStr(Comp.Split);
//    end;
//  end;
//  TasksTreeView.EndUpdate;
end;

procedure TMainForm.RemoveTaskButtonClick(Sender: TObject);
begin
  //Popup1.IsOpen := False;
  RenderTasks.Clear;
end;

procedure TMainForm.ExtractParserToAppDir;
begin
  var ExtractPath: String := APPFOLDER + {$IFDEF MSWINDOWS}'aeparser_win.exe'{$ELSE MACOS}'aeparser_mac'{$ENDIF};

  var ResStream: TResourceStream := TResourceStream.Create(HInstance, {$IFDEF MSWINDOWS}'AEPARSER_WIN'{$ELSE MACOS}'AEPARSER_MAC'{$ENDIF}, RT_RCDATA);

  try
    ResStream.Position := 0;
    ResStream.SaveToFile(ExtractPath);
  finally
    ResStream.Free;
  end;

  {$IFDEF MACOS}
  _system(PAnsiChar('chmod +x "' + AnsiString(ExtractPath) + '"'));
  {$ENDIF}
end;

procedure TMainForm.BeginTryParseAEP(const APath: String);
begin
  //AwaitIndicator.Enabled := True;

  var ConverterPath: String := APPFOLDER + {$IFDEF MSWINDOWS}'aeparser_win.exe'{$ELSE MACOS}'aeparser_mac'{$ENDIF};

//  {$IFDEF MSWINDOWS}
//  var ConverterPath: String := 'Z:\YandexDisk\Development\Delphi\AErender Launcher\src\lib\plugins\AEP Parser\aeparser.exe';
//  {$ELSE MACOS}
//  var ConverterPath: String := '////';
//  {$ENDIF}
  //var Project: String := Data.Files[0];

  ProjectJsonPath := Format('%s%s', [APPFOLDER, ExtractFileName(APath).Replace('aep', 'json')]);;

  //var FileDate := FileDateToDateTime(FileAge(ProjectJsonPath));
  //var ThisTime := Now;

  if FileExists(ProjectJsonPath) then
    if (FileDateToDateTime(FileAge(ProjectJsonPath)) + EncodeTime(0, 0, 1, 0) >= Now) then
      ProjectReaderAwait.Enabled := True
    else begin
      ProjectAwait.Visible := True;
      ProjectAwait.Enabled := True;

      var Executable: TextFile;
      AssignFile(Executable, APPFOLDER + {$IFDEF MSWINDOWS}'project.bat'{$ELSE MACOS}'project.command'{$ENDIF});
      Rewrite(Executable);
      {$IFDEF MSWINDOWS}
      Writeln(Executable, 'chcp 65001');
      {$ENDIF}
      Writeln(Executable, Format('("%s" "%s") > "%s%s"', [ConverterPath, APath, APPFOLDER, ExtractFileName(APath).Replace('aep', 'json')]));
      CloseFile(Executable);

      {$IFDEF MSWINDOWS}
      ShellExecute(0, 'OPEN', PWideChar(APPFOLDER + 'project.bat'), '', '', SW_HIDE);
      {$ELSE MACOS}
      _system(PAnsiChar('chmod +x "' + AnsiString(APPFOLDER + 'project.command"')));
      _system(PAnsiChar('command "' + AnsiString(APPFOLDER + 'project.command" & disown')));
      {$ENDIF}

      //Status.Text := 'Executing...';

      ProjectReaderAwait.Enabled := True;
    end;
end;

procedure TMainForm.CheckAndPopup(const APath: String; const TryParse: Boolean);
begin
  if APath <> '' then begin
    TaskEditorTabControl.TabIndex := 0;
    if (TasksTreeView.ItemByText(ExtractFileName(APath)) <> nil) or (GetTaskByProject(ExtractFileName(APath)) <> nil) then
      TasksTreeViewItemDblClick(TasksTreeView.ItemByText(ExtractFileName(APath)))
    else begin
      TaskEditorSaveButton.Visible := False;
      TaskEditorCreateTaskButton.Visible := True;
      projectPath.Text := APath;

      if TryParse then begin
        BeginTryParseAEP(APath);
      end else
        TaskEditorPopup.PopupModal;
      AddToRecents(APath);
    end;
  end;
end;

procedure TMainForm.AddTaskButtonClick(Sender: TObject);
begin
  var TempStr: String := '';

//  var PathCheckThread: TThread := TThread.CreateAnonymousThread(procedure begin
//    repeat
//      if TempStr <> '' then begin
//        TaskEditorTabControl.TabIndex := 0;
//        if (TasksTreeView.ItemByText(ExtractFileName(TempStr)) <> nil) or (GetTaskByProject(ExtractFileName(TempStr)) <> nil) then
//          TasksTreeViewItemDblClick(TasksTreeView.ItemByText(ExtractFileName(TempStr)))
//        else begin
//          TaskEditorSaveButton.Visible := False;
//          TaskEditorCreateTaskButton.Visible := True;
//
//          if TryParseAEP then begin
//            ImportForm.ShowModal
//          end else
//            TaskEditorPopup.PopupModal;
//          AddToRecents(TempStr);
//        end;
//      end;
//    until TempStr <> '';
//  end);
//
{$IFDEF MACOS}
  var NSWin: NSWindow := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;



  var FOpenFile: NSSavePanel := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);
  FOpenFile.setDirectory(StrToNSStr(DEFPRGPATH));
  FOpenFile.setAllowedFileTypes(ArrayToNSArray(['aep']));
  FOpenFile.setPrompt(StrToNSStr('Open After Effects project'));


  objc_msgSendP2(        // We are calling here method straight from MacApi
    (FOpenFile as ILocalObject).GetObjectID,  // Provide object with callable method
    sel_getUid(PAnsiChar('beginSheetModalForWindow:completionHandler:')),  // Provide method UID
    (NSWin as ILocalObject).GetObjectID,                                   // Provide first method parameter
    TObjCBlock.CreateBlockWithProcedure(procedure (p1: NSInteger) begin    // Provide callback procedure (thanks NightFlight!)
      if p1 = 0 then
        // Handle
      else begin
        TempStr := NSStrToStr(FOpenFile.URL.relativePath);
        CheckAndPopup(TempStr, TryParseAEP);
        //Sleep(200);
      end;
    end)
  );

{$ELSE MSWINDOWS}
  with AEPOpenDialog do
    if Execute then begin
      {TempStr := AEPOpenDialog.FileName;

      if TempStr <> '' then begin
        TaskEditorTabControl.TabIndex := 0;
        if (TasksTreeView.ItemByText(ExtractFileName(TempStr)) <> nil) or (GetTaskByProject(ExtractFileName(TempStr)) <> nil) then
          TasksTreeViewItemDblClick(TasksTreeView.ItemByText(ExtractFileName(TempStr)))
        else begin
          TaskEditorSaveButton.Visible := False;
          TaskEditorCreateTaskButton.Visible := True;
          projectPath.Text := TempStr;

          if TryParseAEP then begin
            //ImportForm.ShowModal
          end else
            TaskEditorPopup.PopupModal;
          AddToRecents(TempStr);
        end;
      end;         }
      CheckAndPopup(AEPOpenDialog.FileName, TryParseAEP);
    end;
{$ENDIF}
//  PathCheckThread.Start;
end;

procedure TMainForm.renderSettingsBoxChange(Sender: TObject);
begin
  if (renderSettingsBox.ItemIndex = renderSettingsBox.Count - 1) and (renderSettingsBox.Count <> 0) then begin
    //OutputModuleEditorForm.Show;
    //renderSettingsBox.ItemIndex := 0;
  end
end;

/// This will add instance to Recents[] and call
/// ReInflateRecents() to update recentItem
procedure TMainForm.AddToRecents(Item: String);
begin
  // Firstly, add to array new item, replacing last one
  if GetRecentIndex(Item) <> -1 then begin
    for var i := GetRecentIndex(Item) downto 1 do begin
      var t: String := Recents[i];
      Recents[i] := Recents[i - 1];
      Recents[i - 1] := t;
    end;
  end else begin
    for var i := 9 downto 1 do begin
      var t: String := Recents[i];
      Recents[i] := Recents[i - 1];
      Recents[i - 1] := t;
    end;
    Recents[0] := Item;
  end;

  ReInflateRecents;
end;

procedure TMainForm.TaskEditorProjectButtonClick(Sender: TObject);
begin
  TaskEditorTabControl.TabIndex := 0;
end;

procedure TMainForm.TaskEditorSaveButtonClick(Sender: TObject);
begin
  var ErrorStr: String := '';
  for var i := 0 to StrToInt(TaskEditorCompCount.Text) - 1 do begin
    /// EC001 :: Composition name is empty
    if TaskEditorCompGrid.Cells[1, i] = '' then
      ErrorStr := ErrorStr + Format('Composition[%d]: (EC001) %s%s', [i + 1, 'Composition name is empty', #13#10]);

    /// EC002 :: [Start Frame] and [End Frame] can''t be empty with [Split] more than 1
    if (TaskEditorCompGrid.Cells[2, i] = '') and (TaskEditorCompGrid.Cells[3, i] = '') and (TaskEditorCompGrid.Cells[4, i] <> '1') then
      ErrorStr := ErrorStr + Format('Composition[%d]: (EC002) %s%s', [i + 1, '[Start Frame] and [End Frame] can''t be empty with [Split] more than 1', #13#10]);

    /// New line for each comp
    if ErrorStr <> '' then
      ErrorStr := ErrorStr + #13#10;
  end;

  if ErrorStr <> '' then begin
    raise AERParamException.Create(Format('%s%s%s', [Language[LANG].Errors.errorsOccured, #13#10, ErrorStr]));
  end else begin
    var TaskIndex: Integer := RenderTasks.IndexOf(GetTaskByProject(ExtractFileName(projectPath.Text)));
    var Compositions: TList<TComposition> := TList<TComposition>.Create;

    for var i := 0 to Trunc(TaskEditorCompCount.Value) - 1 do begin
      Compositions.Add(TComposition.Create(
        TaskEditorCompGrid.Cells[1, i],
        TFrameSpan.Create(TaskEditorCompGrid.Cells[2, i], TaskEditorCompGrid.Cells[3, i]),
        StrToInt(TaskEditorCompGrid.Cells[4, i])
      ));
    end;

    /// Instead of re-creating whole task,
    /// it's faster to rewrite contents of existing one
    RenderTasks.Items[TaskIndex].Project := projectPath.Text;
    RenderTasks.Items[TaskIndex].Output := outputPath.Text;
    RenderTasks.Items[TaskIndex].OutputModule := outputModuleBox.Items[outputModuleBox.ItemIndex];
    RenderTasks.Items[TaskIndex].RenderSettings := renderSettingsBox.Items[renderSettingsBox.ItemIndex];
    RenderTasks.Items[TaskIndex].MissingFiles := missingFilesCheckbox.IsChecked;
    RenderTasks.Items[TaskIndex].Sound := soundCheckbox.IsChecked;
    RenderTasks.Items[TaskIndex].Multiprocessing := threadedRender.IsChecked;
    RenderTasks.Items[TaskIndex].CustomProperties := IfThenElse(customCheckbox.IsChecked = True, customProp.Text, '');  // yeet
    RenderTasks.Items[TaskIndex].CacheLimit := cacheUsageTrackBar.Value;
    RenderTasks.Items[TaskIndex].MemoryLimit := memUsageTrackBar.Value;

    RenderTasks.Items[TaskIndex].Compositions.Free;
    RenderTasks.Items[TaskIndex].Compositions := TList<TComposition>.Create(Compositions);

    {$IFDEF DEBUG_MODE}
    ShowMessage(RenderTasks.Items[TaskIndex].ToString);
    {$ENDIF}

    Compositions.Free;

    RenderTasksOnNotify(Sender, RenderTasks.Items[TaskIndex], cnAdded);

    PopupAniCloseHeight.Enabled := True;
    PopupAniCloseWidth.Enabled := True;

    TaskEditorCompCount.Value := 1;
    TaskEditorCompGrid.Cells[0, 0] := '1';
    TaskEditorCompGrid.Cells[1, 0] := '';
    TaskEditorCompGrid.Cells[2, 0] := '';
    TaskEditorCompGrid.Cells[3, 0] := '';
    TaskEditorCompGrid.Cells[4, 0] := '1';

    //UpdateTasksList;



  end;
end;

procedure TMainForm.TaskEditorToolbarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  //TPopupForm(TaskEditorPopup).StartWindowDrag;
end;

procedure TMainForm.TaskEditorTabControlChange(Sender: TObject);
begin

  if (TaskEditorTabControl.TabIndex = 1) and (TaskEditorCompGrid.Cells[4, 0] = '')
    and (TaskEditorCompGrid.Cells[0, 0] = '') then begin
    TaskEditorCompGrid.Cells[0, 0] := '1';
    TaskEditorCompGrid.Cells[4, 0] := '1';
  end;
end;

procedure TMainForm.RecentProjectSelected(Sender: TObject);
begin
  //ShowMessage('selected');
  //var Selected: TMenuItem := TMenuItem(Sender);

  projectPath.Text  := TMenuItem(Sender).Text;
  threadsSwitch.IsChecked := False;
  compSwitch.IsChecked    := False;

  AddToRecents(TMenuItem(Sender).Text);
end;

procedure TMainForm.SetLanguage(LanguageCode: Integer);
begin
  //Set MainMenu items
//  {$IFDEF MACOS}    //Set macOS specific Launcher item text
//  MainForm.launcherItem.Text          := Language[LanguageCode].MainForm.MainMenu.LauncherMenu;
//  MainForm.outModuleEditorItem0.Text  := Language[LanguageCode].MainForm.MainMenu.OutputModuleEditor;
//  MainForm.settingsItem0.Text         := Language[LanguageCode].MainForm.MainMenu.Settings;
//  {$ENDIF MACOS}
//
  {$IFDEF MSWINDOWS}
  MainForm.fileItem.Text              := Language[LanguageCode].MainForm.MainMenu.FileMenu;
  MainForm.recentItem.Text            := Language[LanguageCode].MainForm.MainMenu.RecentProjects;
  MainForm.importConfigItem.Text      := Language[LanguageCode].MainForm.MainMenu.ImportConfiguration;
  MainForm.exportConfigItem.Text      := Language[LanguageCode].MainForm.MainMenu.ExportConfiguration;
  MainForm.exitItem.Text              := Language[LanguageCode].MainForm.MainMenu.{$IFDEF MSWINDOWS}Close;{$ENDIF MSWINDOWS}{$IFDEF MACOS}CloseDarwin;{$ENDIF MACOS}

  MainForm.editItem.Text              := Language[LanguageCode].MainForm.MainMenu.EditMenu;
  MainForm.outModuleEditorItem.Text   := Language[LanguageCode].MainForm.MainMenu.OutputModuleEditor;
  MainForm.settingsItem.Text          := Language[LanguageCode].MainForm.MainMenu.Settings;

  MainForm.helpItem.Text              := Language[LanguageCode].MainForm.MainMenu.HelpMenu;
  MainForm.aboutItem.Text             := Language[LanguageCode].MainForm.MainMenu.About;
  MainForm.docsItem.Text              := Language[LanguageCode].MainForm.MainMenu.Documentation;
  {$ENDIF MSWINDOWS}

  MainForm.projectPathLabel.Text      := Language[LanguageCode].MainForm.ProjectFile;
  MainForm.outputPathLabel.Text       := Language[LanguageCode].MainForm.OutputFile;
  MainForm.openFile.Text              := Language[LanguageCode].MainForm.OpenSaveProjectButton;
  MainForm.saveFile.Text              := Language[LanguageCode].MainForm.OpenSaveProjectButton;
  MainForm.outputModuleLabel.Text     := Language[LanguageCode].MainForm.OutputModulePreset;
  MainForm.renderSettingsLabel.Text   := Language[LanguageCode].MainForm.RenderSettings;

  MainForm.properties.Text            := Language[LanguageCode].MainForm.Properties;
  MainForm.missingFilesCheckbox.Text  := Language[LanguageCode].MainForm.MissingFiles;
  MainForm.soundCheckbox.Text         := Language[LanguageCode].MainForm.Sound;
  MainForm.threadedRender.Text        := Language[LanguageCode].MainForm.Threaded;
  MainForm.customCheckbox.Text        := Language[LanguageCode].MainForm.Custom;
  MainForm.customProp.TextPrompt      := Language[LanguageCode].MainForm.CustomPropHint;

  MainForm.cacheUsageLimitLabel.Text  := Language[LanguageCode].MainForm.CacheUsageLimit;

  MainForm.memUsageLabel.Text         := Language[LanguageCode].MainForm.MemUsage;

  if MainForm.compSwitch.IsChecked then
    MainForm.compSwitchLabel.Text     := Language[LanguageCode].MainForm.MultiComp
  else
    MainForm.compSwitchLabel.Text     := Language[LanguageCode].MainForm.SingleComp;

  MainForm.compName.TextPrompt        := Language[LanguageCode].MainForm.CompNameHint;
  MainForm.compGrid.Columns[0].Header := Language[LanguageCode].MainForm.MultiCompGridHeader;

  if MainForm.threadsSwitch.IsChecked then begin
    MainForm.threadsSwitchLabel.Text  := Language[LanguageCode].MainForm.SplitRender;
    MainForm.outFrame.TextPrompt      := Language[LanguageCode].MainForm.EndFrameHint;
  end else
    MainForm.threadsSwitchLabel.Text  := Language[LanguageCode].MainForm.SingleRener;

  MainForm.startFrameLabel.Text       := Language[LanguageCode].MainForm.StartFrame;
  MainForm.endFrameLabel.Text         := Language[LanguageCode].MainForm.EndFrame;

  MainForm.calculateButton.Text       := Language[LanguageCode].MainForm.Calculate;

  MainForm.launchButton.Text          := Language[LanguageCode].MainForm.Launch;

  MainForm.UpdateLabel.Text           := Language[LanguageCode].MainForm.NewVersionAvailable;
  MainForm.downloadButton.Text        := Language[LanguageCode].MainForm.Download;
end;

procedure TMainForm.UpdateOutputModules;
begin
  var Index: Integer := outputModuleBox.ItemIndex;
  if (outputModuleBox.Count <> 0) then
    outputModuleBox.Clear;

  for var i := 0 to High(OutputModules) do
    if OutputModules[i].Imported then
      outputModuleBox.Items.Add(OutputModules[i].Module + ' ' + Language[LANG].OutputModuleConfiguratorForm.Imported)
    else
      outputModuleBox.Items.Add(OutputModules[i].Module);
  outputModuleBox.Items.Add(Language[LANG].MainForm.ConfigureOutputModules);
  outputModuleBox.ItemIndex := Index;
end;

procedure TMainForm.UpdateRenderSettings;
begin
  var Index: Integer := renderSettingsBox.ItemIndex;
  if (renderSettingsBox.Count <> 0) then
    renderSettingsBox.Clear;

  for var i := 0 to High(RenderSettings) do
    if RenderSettings[i].Imported then
      renderSettingsBox.Items.Add(RenderSettings[i].Setting + ' ' + Language[LANG].OutputModuleConfiguratorForm.Imported)
    else
      renderSettingsBox.Items.Add(RenderSettings[i].Setting);
  //renderSettingsBox.Items.Add(Language[LANG].MainForm.ConfigureRenderSettings);
  renderSettingsBox.ItemIndex := Index;
end;

function TMainForm.ReadAERQ(Path: String): TObjectList<TRenderTask>;
begin
  Result := TObjectList<TRenderTask>.Create;

  var AERQXMLDocument: IXMLDocument := TXMLDocument.Create(nil);
  AERQXMLDocument.LoadFromFile(Path);
  AERQXMLDocument.Encoding := 'utf-8';

  AERQXMLDocument.Active := True;
  var RootNode: IXMLNode := AERQXMLDocument.DocumentElement;
  //projectPath.Text := RootNode.Attributes['project'];

  for var i := 0 to RootNode.ChildNodes.Count - 1 do begin
    outputPath.Text := RootNode.ChildNodes[i].Attributes['outputFolder'];
    tempSavePath := RootNode.ChildNodes[i].Attributes['outputFolder'] + PLATFORMPATHSEPARATOR;

    /// Add new Output Module to library if it don't exist
    if StrToBool(RootNode.ChildNodes[i].ChildNodes['outputModule'].Attributes['use']) then begin
      var TempOutputModule: OutputModule;
      TempOutputModule.Module := RootNode.ChildNodes[i].ChildNodes['outputModule'].ChildNodes['module'].Text;
      TempOutputModule.Mask   := RootNode.ChildNodes[i].ChildNodes['outputModule'].ChildNodes['mask'].Text;

      if GetOMIndex(TempOutputModule) <> -1 then
        outputModuleBox.ItemIndex := GetOMIndex(TempOutputModule)
      else begin
        SetLength(OutputModules, Length(OutputModules) + 1);
        OutputModules[High(OutputModules)].Module   := RootNode.ChildNodes[i].ChildNodes['outputModule'].ChildNodes['module'].Text;
        OutputModules[High(OutputModules)].Mask     := RootNode.ChildNodes[i].ChildNodes['outputModule'].ChildNodes['mask'].Text;
        OutputModules[High(OutputModules)].Imported := True;

        outputModuleBox.Items.Insert(outputModuleBox.Items.Count - 1, OutputModules[High(OutputModules)].Module + ' ' + Language[LANG].OutputModuleConfiguratorForm.Imported);
        outputModuleBox.ItemIndex := outputModuleBox.Items.Count - 2;
      end;
    end;
    outputModuleBoxChange(Self);

    if RootNode.ChildNodes['queueItem'].ChildNodes['renderSettings'].Text.IsEmpty <> True then begin
      var TempRenderSetting: RenderSetting;
      TempRenderSetting.Setting := RootNode.ChildNodes['queueItem'].ChildNodes['renderSettings'].Text;

      if GetRSIndex(TempRenderSetting) <> -1 then
        renderSettingsBox.ItemIndex := GetRSIndex(TempRenderSetting)
      else begin
        SetLength(RenderSettings, Length(RenderSettings) + 1);
        RenderSettings[High(RenderSettings)].Setting := RootNode.ChildNodes['queueItem'].ChildNodes['renderSettings'].Text;
        RenderSettings[High(RenderSettings)].Imported := True;

        renderSettingsBox.Items.Insert(renderSettingsBox.Items.Count - 1, RenderSettings[High(RenderSettings)].Setting + ' ' + Language[LANG].OutputModuleConfiguratorForm.Imported);
        renderSettingsBox.ItemIndex := renderSettingsBox.Items.Count - 2;
      end;
    end;
    renderSettingsBoxChange(Self);

    var Compositions: TList<TComposition> := TList<TComposition>.Create;
    Compositions.Add(TComposition.Create(
      RootNode.ChildNodes[i].ChildNodes['composition'].ChildNodes['name'].Text,
      TFrameSpan.Create(StrToInt(RootNode.ChildNodes[i].ChildNodes['composition'].ChildNodes['rangeStart'].Text),
      StrToInt(RootNode.ChildNodes[i].ChildNodes['composition'].ChildNodes['rangeEnd'].Text)),
      1
    ));

    //compName.Text := RootNode.ChildNodes['queueItem'].ChildNodes['composition'].ChildNodes['name'].Text;
    //inFrame.Text  := RootNode.ChildNodes['queueItem'].ChildNodes['composition'].ChildNodes['rangeStart'].Text;
    //outFrame.Text := RootNode.ChildNodes['queueItem'].ChildNodes['composition'].ChildNodes['rangeEnd'].Text;

    Result.Add(TRenderTask.Create(
      RootNode.Attributes['project'],
      outputPath.Text,
      outputModuleBox.Items[outputModuleBox.ItemIndex],
      renderSettingsBox.Items[renderSettingsBox.ItemIndex],
      missingFilesCheckbox.IsChecked,
      soundCheckbox.IsChecked,
      threadedRender.IsChecked,
      IfThenElse(customCheckbox.IsChecked = True, customProp.Text, ''),  // yeet
      cacheUsageTrackBar.Value,
      memUsageTrackBar.Value,
      TList<TComposition>.Create(Compositions)
    ));
  end;
end;

function TMainForm.ReadAER(Path: String): TRenderTask;
begin
  var AERXMLDocument: IXMLDocument := TXMLDocument.Create(nil);
  AERXMLDocument.LoadFromFile(Path);
  AERXMLDocument.Encoding := 'utf-8';

  AERXMLDocument.Active := True;

  var RootNode: IXMLNode := AERXMLDocument.DocumentElement;

  if DEFOUTPATH <> '' then begin
    outputPath.Text := DEFOUTPATH;
    tempSavePath := DEFOUTPATH + PLATFORMPATHSEPARATOR;
  end;
  outputModuleBoxChange(Self);

  var Compositions: TList<TComposition> := TList<TComposition>.Create;

  for var i := 0 to StrToInt(RootNode.ChildNodes['compositions'].Attributes['count']) - 1 do begin
    Compositions.Add(TComposition.Create(
      RootNode.ChildNodes['compositions'].ChildNodes[i].ChildNodes['name'].Text,
      TFrameSpan.Create(StrToInt(RootNode.ChildNodes['compositions'].ChildNodes[i].ChildNodes['rangeStart'].Text),
        StrToInt(RootNode.ChildNodes['compositions'].ChildNodes[i].ChildNodes['rangeEnd'].Text)),
      1
    ));
  end;

  Result := TRenderTask.Create(
    RootNode.Attributes['project'],
    outputPath.Text,
    outputModuleBox.Items[outputModuleBox.ItemIndex],
    renderSettingsBox.Items[renderSettingsBox.ItemIndex],
    missingFilesCheckbox.IsChecked,
    soundCheckbox.IsChecked,
    threadedRender.IsChecked,
    IfThenElse(customCheckbox.IsChecked = True, customProp.Text, ''),  // yeet
    cacheUsageTrackBar.Value,
    memUsageTrackBar.Value,
    TList<TComposition>.Create(Compositions)
  );
end;

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
    threadsGrid.RowCount := threadsCount.Text.ToInteger;
    Tm := inFrame.Text;
    if Tm.IsEmpty then
      inFrame.Text := '0';
    Start := inFrame.Text.ToInteger;
    Stop := outFrame.Text.ToInteger;
    threadsGrid.Cells[0, 0] := IntToStr(Start);
    J := Stop - Start;
    K := J div threadsCount.Text.ToInteger;
    threadsGrid.Cells[1, 0] := IntToStr(Start+K);
    for I := 1 to threadsCount.Text.ToInteger - 1 do
      begin
        threadsGrid.Cells[0, I] := (Start + K * I + 1).ToString;
        threadsGrid.Cells[1, I] := (Start + K * (I + 1)).ToString;
      end;
    threadsGrid.Cells[1, threadsCount.Text.ToInteger - 1] := IntToStr(Stop);
  except
    on Exception do
      if outFrame.Text = '' then
        TDialogServiceSync.MessageDialog(Language[LANG].Errors.CalculateFrameError, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0)
      else
        TDialogServiceSync.MessageDialog(Language[LANG].Errors.CalculateUnknownError, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0)
  end;
end;

procedure TMainForm.compNameTyping(Sender: TObject);
begin
  compGrid.Cells[0, 0] := compName.Text;
end;

procedure TMainForm.compSwitchSwitch(Sender: TObject);
begin
//  if compSwitch.IsChecked then begin
//    if UpdateAvailable = True then
//      begin
//        if MainForm.Height <= 490 then
//          MainForm.Height := 630;//MainForm.Height + 130;
//      end
//    else
//      if MainForm.Height <= 450 then
//        MainForm.Height := 580;//MainForm.Height + 130;
//
//    compSwitchLabel.Text := Language[LANG].MainForm.MultiComp;
//
//    compGrid.RowCount := Round(compCount.Value);
//    compGrid.Cells[0, 0] := compName.Text;
//    compGrid.Model.ScrollDirections := TScrollDirections.Vertical;
//  end else begin
//    if UpdateAvailable = True then begin
//      if MainForm.WindowState <> TWindowState.wsMaximized then
//        MainForm.Height := 490//MainForm.Height - 130
//    end else
//      if MainForm.WindowState <> TWindowState.wsMaximized then
//        MainForm.Height := 450;//MainForm.Height - 130;
//
//    compSwitchLabel.Text := Language[LANG].MainForm.SingleComp;
//    compName.Text := compGrid.Cells[0, 0];
//  end;
end;

procedure TMainForm.docsItemClick(Sender: TObject);
begin
  //HelpForm.Show;
  TDialogServiceSync.MessageDialog('Not ready yet!', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0)
end;

procedure TMainForm.downloadButtonClick(Sender: TObject);
begin
  Open(gitDownload);
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
begin
  if SettingsForm.HandleCheckBox.IsChecked then begin
    {$IFDEF MSWINDOWS}KillProcess('AfterFX.com');{$ENDIF MSWINDOWS}
    {$IFDEF MACOS}KillProcess('aerendercore');{$ENDIF MACOS}
  end;

  SaveConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
  if StrToBool(DelTempFiles) = True then
    begin
      var LauncherDirectory: TArray<String> := GetDirectoryFiles(APPFOLDER);
      for var i := 0 to High(LauncherDirectory) do
        begin
          {$IFDEF MSWINDOWS}
          if (LauncherDirectory[i].Contains('.bat')) then
            System.SysUtils.DeleteFile(PWideChar(LauncherDirectory[i]));
          {$ENDIF MSWINDOWS}
          {$IFDEF MACOS}
          if (LauncherDirectory[i].Contains('.command')) then
            System.SysUtils.DeleteFile(PWideChar(LauncherDirectory[i]));
          {$ENDIF MACOS}
          if (LauncherDirectory[i].Contains('.log')) then
            System.SysUtils.DeleteFile(PWideChar(LauncherDirectory[i]));
        end;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  CFG: TextFile;
begin
  {$IFDEF MSWINDOWS}DwmCompositionEnabled();{$ENDIF}
  FormatSettings := TFormatSettings.Invariant;
  MainForm.Width := 960;
  MainForm.Height := 540;
  MainForm.Caption := 'AErender Launcher (' + APPVERSION + ')';
  APPFOLDER :=  {$IFDEF MSWINDOWS}'C:\ProgramData\AErender\'
                {$ELSE MACOS}GetEnvironmentVariable('HOME') + '/Documents/AErender/'{$ENDIF};
  {$IFDEF MSWINDOWS}
  //FreeAndNil(launcherItem);
  launcherItem.Visible := False;
  exitItem.ShortCut := TextToShortCut('Alt+F4');
  exportConfigItem.ShortCut := TextToShortCut('Ctrl+E');
  importConfigItem.ShortCut := TextToShortCut('Ctrl+I');
  {$ELSE MACOS}
  editItem.Visible := False;
  exitItem.Visible := False;
//  FreeAndNil(editItem);
//  FreeAndNil(exitItem);
  {$ENDIF}

  if DirectoryExists(APPFOLDER) then
    AssignFile(CFG, APPFOLDER + 'AErenderConfiguration.xml')
  else
    begin
      CreateDir(APPFOLDER);
      AssignFile(CFG, APPFOLDER + 'AErenderConfiguration.xml');
    end;

  if FileExists(APPFOLDER + 'AErenderConfiguration.xml') then begin
    try
      LoadConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
    except
      on Exception do begin
        try
          LoadLegacyConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
        except
          on Exception do begin
            if (TDialogServiceSync.MessageDialog(
              ('Configuration file is corrupted! Press OK to renew configuration file. Application will be restarted.' + #13#10 +
              {$IFDEF MSWINDOWS}'C:\ProgramData\AErender\AErenderConfiguration.xml'
              {$ELSE MACOS}'~/Documents/AErender/AErenderConfiguration.xml'{$ENDIF}),
              TMsgDlgType.mtError, mbOKCancel, TMsgDlgBtn.mbOK, 0) = 1) then
            begin
              System.SysUtils.DeleteFile(APPFOLDER + 'AErenderConfiguration.xml');
              {$IFDEF MSWINDOWS}ShellExecute(0, 'OPEN', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
              {$ELSE MACOS}_system(PAnsiChar('open -a "' + AnsiString(ParamStr(0)) + '" & disown'));{$ENDIF}
            end;
            Halt;
          end;
        end;
      end;
    end;
  end else begin
    InitConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
    AERH := 'True';
    DelTempFiles := 'True';
  end;
  AEPOpenDialog.InitialDir := DEFPRGPATH;
  SaveDialog1.InitialDir := DEFOUTPATH;

  if not FileExists(APPFOLDER + {$IFDEF MSWINDOWS}'aeparser_win.exe'{$ELSE MACOS}'aeparser_mac'{$ENDIF}) then begin
    ExtractParserToAppDir;
  end;

  InflateRecents;

  var AnimSetupThread: TThread := TThread.CreateAnonymousThread(procedure begin
    outputModuleBox.ListBox.AniCalculations.Animation := True;

    compGrid.AniCalculations.AutoShowing := False;
    compGrid.AniCalculations.Animation := True;

    threadsGrid.AniCalculations.Animation := True;

    TaskEditorCompGrid.AniCalculations.Animation := True;
    TaskEditorCompGrid.Model.ScrollDirections := (TScrollDirections.Vertical);
  end);

//  threadsCount.OnValidate := threadsCount.OnValidateEvent;
//  threadsCount.OnValidating := threadsCount.OnValidatingEvent;

  RenderTasks.OnNotify := MainForm.RenderTasksOnNotify;

  AnimSetupThread.Start;
  //ShowMessage(AErenderPath);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  /// Composition grid collumns width
//  StringColumn1.Width := compGrid.Width;
//  StringColumn2.Width := threadsGrid.Width * 0.5;
//  StringColumn3.Width := threadsGrid.Width * 0.5;

  /// Limit form's width
  if MainForm.Width < SizeConstraints.MIN_WIDTH then
    MainForm.Width := SizeConstraints.MIN_WIDTH;

  /// RU: Ограничение высоты формы в зависимости от платформы и видимости панели с обновлением
  if UpdateAvailable then begin
    if MainForm.Height < SizeConstraints.MIN_HEIGHT_UPD then begin
      MainForm.Height := SizeConstraints.MIN_HEIGHT_UPD;
      {$IFDEF MSWINDOWS}Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);{$ENDIF MSWINDOWS}
    end;
  end else begin
    if MainForm.Height < SizeConstraints.MIN_HEIGHT_NO_UPD then begin
      MainForm.Height := SizeConstraints.MIN_HEIGHT_NO_UPD;
    {$IFDEF MSWINDOWS}Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);{$ENDIF MSWINDOWS}
    end;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin

  /// To speed up startup we will check for FFMPEG in a separate thread
  var FFMPEGCheckThread: TThread := TThread.CreateAnonymousThread(procedure begin
    try
    if GetFFMPEGPath <> '' then
      begin
        FFMPEG := True;
        ffmpegPath := GetFFMPEGPath;
        //ffmpegCheckBox.Enabled := True;
      end
    else
      raise Exception.Create('FFMPEG not found');
    except
      on Exception do
        begin
          FFMPEG := False;
          //ffmpegCheckBox.Enabled := False;
          //ffmpegCheckBox.Hint := 'FFMPEG is not found at' + {$IFDEF MSWINDOWS} 'C:\ProgramData\AErender\' {$ENDIF MSWINDOWS}
          //                                                  {$IFDEF MACOS} '~/Documents/AErender/' {$ENDIF MACOS} + 'directory';
        end;
    end;
  end);

  var aeparserCheckThread: TThread := TThread.CreateAnonymousThread(procedure begin
    //
  end);

  //// Check for available languages
  // It's better to do it in a Show thread, since OnCreate event of main form
  // don't have access to other forms that are not yet created
  if DirectoryExists(APPFOLDER + 'lang' + PLATFORMPATHSEPARATOR) then begin
    //LangFolder := TDirectory.GetFiles(APPFOLDER + 'lang' + PLATFORMPATHSEPARATOR);
    {TODO -oLilyStilson -cLanguage : Set form language from existing file / or load it to resource and set}
  end else begin
    //CreateDir(APPFOLDER + 'lang' + PLATFORMPATHSEPARATOR);
    InitLanguage(APPFOLDER + 'lang' + PLATFORMPATHSEPARATOR);
  end;

  UpdateOutputModules;
  UpdateRenderSettings;

  SettingsForm.styleBox.ItemIndex := STYLE;
  SettingsForm.onRenderStartBox.ItemIndex := ONRENDERSTART;
  SettingsForm.HandleCheckBox.IsChecked := StrToBool(AERH);
  SettingsForm.delFilesCheckBox.IsChecked := StrToBool(DelTempFiles);

  var FormVisualUpdateThread: TThread :=TThread.CreateAnonymousThread(procedure begin

    {$IFDEF MACOS}
    if LANG = 1 then begin
      outputModuleLabel.Width := 148;
      renderSettingsLabel.Width := 148;
    end;
    {$ENDIF MACOS}

    if memUsageTrackBar.Value = 100 then
      memUsageInfo.Text := Language[LANG].MainForm.Unlimited
    else
      memUsageInfo.Text := Trunc(memUsageTrackBar.Value).ToString + '% (' + Trunc((GetPlatformMemorySize/1024/1024) * (memUsageTrackBar.Value / 100)).ToString + ' MB)';
    if cacheUsageTrackBar.Value = 100 then
      cacheUsageInfo.Text := Language[LANG].MainForm.Unlimited
    else
      cacheUsageInfo.Text := Trunc(cacheUsageTrackBar.Value).ToString + '%';

    StringColumn1.Width := compGrid.Width - 5;
    StringColumn2.Width := threadsGrid.Width * 0.5;
    StringColumn3.Width := threadsGrid.Width * 0.5;
  end);

  SplashScreenForm.Close;
  SplashScreenForm.Free;

  FormVisualUpdateThread.Start;

  FFMPEGCheckThread.Start;
  aeparserCheckThread.Start;

  if (ParamCount > 0) and (ParamStr(1) = '-aer') then begin
    ImportUnit.PARAMSTART := True;
    ImportUnit.XMLPath := ParamStr(2);
    ImportForm.FormSender := FS_XML;
    ImportForm.ShowModal;
  end else if (ParamStr(1) = '-aerq') then begin
    MainUnit.PARAMSTART := True;
    RenderTasks.AddRange(ReadAERQ(ParamStr(2)));
  end;

  //ShowMessage(RenderTasks.Count.ToString);
  //TasksIsEmpty.Visible := RenderTasks.Count = 0;

  //  Check updates in separate thread to fasten startup
  MainForm.UpdateNetHTTPClient.Get(AERL_REPO_RELEASES);
end;

procedure TMainForm.importConfigItemClick(Sender: TObject);
begin
  compSwitch.Enabled := True;
  compSwitch.IsChecked := False;
  threadsSwitch.Enabled := True;
  threadsSwitch.IsChecked := False;

{$IFDEF MACOS}
  var NSWin: NSWindow := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;

  var FOpenFile: NSSavePanel := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);
  FOpenFile.setDirectory(StrToNSStr(DEFPRGPATH));
  FOpenFile.setAllowedFileTypes(ArrayToNSArray(['aer']));
  FOpenFile.setPrompt(StrToNSStr('Import configuration'));

  objc_msgSendP2((FOpenFile as ILocalObject).GetObjectID,
                 sel_getUid(PAnsiChar('beginSheetModalForWindow:completionHandler:')),
                 (NSWin as ILocalObject).GetObjectID,
                 TObjCBlock.CreateBlockWithProcedure(
                 procedure (p1: NSInteger)
                 begin
                    if p1 = 0 then
                      // Handle
                    else begin
                      ImportedPath := NSStrToStr(FOpenFile.URL.relativePath);
                      ImportForm.ShowModal;
                    end;
                 end));
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
  with XMLOpenDialog do
    if Execute then
      ImportForm.ShowModal;
{$ENDIF MSWINDOWS}
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
/// So, we need some rendering modes
/// NORMAL MODE: It's the old Launcher's behaviour
///   Will render all the compositions one-by-one and render them
///   if Composition.Split > 1, then all the splits will be rendered with default method in settings
///

type
  exec = record
    script: String;
    F: TextFile;
  end;
var
  threads, comps, emptyComps: Integer;
  PATH, logPath: String;
  execFile: array [1..128] of exec;

  // Notification: TNotification;
begin
  //Error Codes
  if not (SettingsForm.HandleCheckBox.IsChecked and isRendering) then begin
    {emptyComps := 0;
    if AErenderPath.IsEmpty then
      ERR := ERR + #13#10 + '[Error 1]: ' + Language[LANG].Errors.aerenderIsEmpty;
    if projectPath.Text.IsEmpty then
      ERR := ERR + #13#10 + '[Error 2]: ' + Language[LANG].Errors.projectIsEmpty;
    if outputPath.Text.IsEmpty then
      ERR := ERR + #13#10 + '[Error 3]: ' + Language[LANG].Errors.outputIsEmpty;
    if compName.Text.IsEmpty then
      ERR := ERR + #13#10 + '[Error 4]: ' + Language[LANG].Errors.compositionIsEmpty;
    if compSwitch.IsChecked then
      begin
        for var i := 0 to compCount.Value.ToString.ToInteger - 1 do
          if compGrid.Cells[0, i].IsEmpty then
            inc(emptyComps);
        if emptyComps > 0 then
          ERR := ERR + #13#10 + '[Error 5]: ' + Language[LANG].Errors.multiCompIsEmpty;
      end;
    if StrToInt(threadsCount.Text) > 128 then
      ERR := ERR + #13#10 + '[Error 6]: ' + Language[LANG].Errors.tooManyThreads;

    //Proceed if no errors occured
    if not ERR.IsEmpty then
      begin
        TDialogServiceSync.MessageDialog((Language[LANG].Errors.errorsOccured + ERR), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
        ERR := '';
      end
    else    }
      begin
        AbortRender := False;
        ShowMessage(RenderTasks.Items[0].ToString);

        /// It would be really nice to have an await keyword here, but...
        RenderTasks[0].Render();

//        for var Task: TRenderTask in RenderTasks do begin
//          Task.Render(4);
//        end;

        // Wait untill aerender launches
        Sleep (200);
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
  end else begin
    TDialogServiceSync.MessageDialog(Language[LANG].Errors.isCurrentlyRendering, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
    RenderWindowSender := infoButton;
    RenderingForm.ShowModal;
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
                      AddToRecents(projectPath.Text);
                 end));
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
begin
  with AEPOpenDialog do
    if Execute then
      projectPath.Text := AEPOpenDialog.FileName;
{$ENDIF MSWINDOWS}
  AddToRecents(projectPath.Text);
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
  if (outputModuleBox.ItemIndex = outputModuleBox.Count - 1) and (outputModuleBox.Count <> 0) then
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

procedure TMainForm.TaskEditorPopupPopup(Sender: TObject);
begin
  PopupAniOpenHeight.Enabled := True;
  PopupAniOpenWidth.Enabled := True;
end;

procedure TMainForm.PopupAniCloseFinish(Sender: TObject);
begin
  PopupAniCloseHeight.Enabled := False;
  PopupAniCloseWidth.Enabled := False;
  TaskEditorPopup.IsOpen := False;
  TaskEditorPopup.Height := 384;
  TaskEditorTabControl.TabIndex := 0;
end;

procedure TMainForm.PopupAniOpenFinish(Sender: TObject);
begin
  PopupAniOpenHeight.Enabled := False;
  PopupAniOpenWidth.Enabled := False;
end;

procedure TMainForm.ProjectReaderAwaitTimer(Sender: TObject);
begin
  AwaitElapsedTime := AwaitElapsedTime + 5;
  ProjectAwaitStatusLabel.Text := Format('Elapsed %d seconds of %d', [Trunc(AwaitElapsedTime / 1000), Trunc(AWAIT_TIMEOUT / 1000)]);
  {$IFDEF MSWINDOWS}
  ProjectAwaitStatusLabel.Repaint;
  {$ENDIF}

  if AwaitElapsedTime >= AWAIT_TIMEOUT then begin
    //Status.Text := 'Timeout';
    ProjectAwait.Enabled := False;
    ProjectAwait.Visible := False;

    ProjectReaderAwait.Enabled := False;

    TDialogServiceSync.MessageDialog('Reading timeout. Please, specify all project data manually.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);

  end;

  if FileExists(ProjectJsonPath) then begin
    var ProjectJsonFile: TStream := TFileStream.Create(ProjectJsonPath, fmOpenRead or fmShareDenyNone);
    var ProjectString: TStringList := TStringList.Create;
    ProjectString.DefaultEncoding := TEncoding.UTF8;
    ProjectString.LoadFromStream(ProjectJsonFile);
    ProjectJsonFile.Free;

    if ProjectString.Text <> '' then begin
      //Status.Text := 'File is not empty';
      ProjectAwait.Enabled := False;
      ProjectAwait.Visible := False;
      //JsonContents.Text := ProjectString.Text;

      ProjectJson := ProjectString.Text;

      //Status.Text := 'Waiting...';
      AwaitElapsedTime := 0;

      ProjectReaderAwait.Enabled := False;

      ImportForm.FormSender := FS_JSON;
      ImportForm.ShowModal;

//      RenderTasks.Add(TRenderTask.CreateFromJSON(
//        projectPath.Text,
//        outputPath.Text,
//        outputModuleBox.Items[outputModuleBox.ItemIndex],
//        renderSettingsBox.Items[renderSettingsBox.ItemIndex],
//        missingFilesCheckbox.IsChecked,
//        soundCheckbox.IsChecked,
//        threadedRender.IsChecked,
//        IfThenElse(customCheckbox.IsChecked = True, customProp.Text, ''),  // yeet
//        cacheUsageTrackBar.Value,
//        memUsageTrackBar.Value,
//        ProjectJson
//      ));

    end;// else
      //Status.Text := 'File is empty, repeating...';
  end;
end;

procedure TMainForm.TaskEditorCancelButtonClick(Sender: TObject);
begin
  PopupAniCloseHeight.Enabled := True;
  PopupAniCloseWidth.Enabled := True;
end;

procedure TMainForm._projectPathDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
begin
  if Data.Source <> nil then
    projectPath.Text := Data.Source.ClassName
  else
    projectPath.Text := Data.Files[0];
end;

procedure TMainForm._projectPathDragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Link;
end;

procedure TMainForm._saveFileClick(Sender: TObject);
{$IFDEF MACOS}
var
  FSaveFile: NSSavePanel;
  NSWin: NSWindow;
begin
  NSWin := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;

  FSaveFile :=TNSSavePanel.Wrap(TNSSavePanel.OCClass.savePanel);
  FSaveFile.setAccessoryView(CreateMessageView(Language[LANG].MainForm.DarwinDialogTip));
  //FSaveFile.setAccessoryView();
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

/// Settings button click
procedure TMainForm.settingsButtonClick(Sender: TObject);
begin
  SettingsForm.ShowModal;
end;

procedure TMainForm.TaskEditorCompCountChange(Sender: TObject);
begin
  TaskEditorCompGrid.RowCount := StrToInt(TaskEditorCompCount.Text);
  for var i := 0 to TaskEditorCompGrid.RowCount - 1 do begin
    TaskEditorCompGrid.Cells[0, i] := IntToStr(i + 1);
    if TaskEditorCompGrid.Cells[4, i] = '' then
      TaskEditorCompGrid.Cells[4, i] := '1';
  end;
end;

procedure TMainForm.TaskEditorCompGridEditingDone(Sender: TObject; const ACol, ARow: Integer);
begin
  {case ACol of
    3: begin
      if StrToInt(TaskEditorCompGrid.Cells[ACol, ARow]) > 128 then
        TaskEditorCompGrid.StylesData['selection.fill.color'] := $FFFF6157
      else
        TaskEditorCompGrid.StylesData['selection.fill.color'] := $FF2A82E2;
    end;
  end; }
end;

procedure TMainForm.TasksPopupMenuPopup(Sender: TObject);
begin
  if (TasksTreeView.ItemByText(FCurrentItem) <> nil) and (FCurrentItem <> 'nil') {or (TasksTreeView.Selected <> nil)} then begin
    TasksTreeView.ItemByText(FCurrentItem).IsSelected := True;
    EditTaskPopupItem.Enabled := True;
    DeleteTaskPopupItem.Enabled := True;
    if TasksTreeView.ItemByText(FCurrentItem).ParentItem <> nil then  // Composition
      TaskItemDisplay.Text := Format('Composition: %s', [FCurrentItem])
    else  // Project
      TaskItemDisplay.Text := Format('Project: %s', [FCurrentItem]);
  end else begin
    TaskItemDisplay.Text := 'No task is selected!';
    EditTaskPopupItem.Enabled := False;
    DeleteTaskPopupItem.Enabled := False;
  end;
end;

procedure TMainForm.TasksTreeViewItemDblClick(Sender: TObject);
begin
  //TasksTreeView.AllowDrag := False;

  var ProjectItem: TTreeViewItem;
  var Task: TRenderTask;
  var SelectedComp: String := '';

  if TTreeViewItem(Sender).ParentItem <> nil then begin // If composition item was dblclicked
    ProjectItem := TTreeViewItem(Sender).ParentItem;
    TaskEditorTabControl.TabIndex := 1;
    SelectedComp := TTreeViewItem(Sender).Text;
  end else begin // We know for sure that it's a project item
    ProjectItem := TTreeViewItem(Sender);
    TaskEditorTabControl.TabIndex := 0;
  end;

  Task := GetTaskByProject(ProjectItem.Text);

  if ProjectItem <> nil then begin // Ensure, that we won't get an Access Violation
    projectPath.Text := Task.Project;
    outputPath.Text := Task.Output;

    outputModuleBox.Index := outputModuleBox.Items.IndexOf(Task.OutputModule);
    renderSettingsBox.Index := renderSettingsBox.Items.IndexOf(Task.RenderSettings);

    missingFilesCheckbox.IsChecked := Task.MissingFiles;
    soundCheckbox.IsChecked := Task.Sound;
    threadedRender.IsChecked := Task.Multiprocessing;

    customCheckbox.IsChecked := not Task.CustomProperties.IsEmpty;
    customProp.Enabled := not Task.CustomProperties.IsEmpty;
    customProp.Text := Task.CustomProperties;

    cacheUsageTrackBar.Value := Task.CacheLimit;
    memUsageTrackBar.Value := Task.MemoryLimit;

    TaskEditorCompCount.Value := Task.Compositions.Count;
    for var i := 0 to Task.Compositions.Count - 1 do begin
      TaskEditorCompGrid.Cells[1, i] := Task.Compositions.Items[i].CompName;
      TaskEditorCompGrid.Cells[2, i] := Task.Compositions.Items[i].Frames.StartFrame.ToString;
      TaskEditorCompGrid.Cells[3, i] := Task.Compositions.Items[i].Frames.EndFrame.ToString;
      TaskEditorCompGrid.Cells[4, i] := Task.Compositions.Items[i].Split.ToString;
    end;
  end;

  if SelectedComp <> '' then
    TaskEditorCompGrid.SelectRow(GetCompIndexInTask(Task, SelectedComp));

  TaskEditorSaveButton.Visible := True;
  TaskEditorCreateTaskButton.Visible := False;

  TaskEditorPopup.PopupModal; //IsOpen := True;//PopupModal;
end;

procedure TMainForm.TasksTreeViewDragChange(SourceItem, DestItem: TTreeViewItem; var Allow: Boolean);
begin
  //if (SourceItem.Level = 1) and (DestItem.Level = 0) then // Comp -> Project
  //  ShowMessage('Comp -> Project');

  //if (SourceItem.Level = 1) and (DestItem.Level = 1) then // Comp -> Comp

  try
    if (SourceItem.Level = 1) and (DestItem.Level = 1) then  // Project[0] -> Project[1]
      Allow := False;

    if (SourceItem.Level = 2) and (DestItem.Level = 1) then // Project[0].Comp -> Project[1]
      if SourceItem.ParentItem.Text <> DestItem.Text then
        Allow := False;

    if (SourceItem.Level = 1) and (DestItem.Level = 2) then // Project[0].Comp -> Project[0].Comp
      Allow := False;

    if (SourceItem.Level = 2) and (DestItem.Level = 2) then // Project[0].Comp -> Project[1].Comp
      Allow := False;
    //ShowMessage(Format('%d --> %d', [SourceItem.Level, DestItem.Level]));
  except
    if (SourceItem.Level = 1) and (RenderTasks.Count > 1) then begin
      Allow := True;
      RenderTasks.Move(RenderTasks.IndexOf(GetTaskByProject(SourceItem.Text)), RenderTasks.Count - 1);
      //ShowMessage(Format('%d --> %d', [SourceItem.Level, -1]));
      ShowMessage(Format('LastTask.Project = %s', [RenderTasks.Items[RenderTasks.Count - 1].Project]));
    end else
      Allow := False;
  end;

end;

procedure TMainForm.TasksTreeViewDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  if Data.Source = nil then begin     // Ensure that we're recieving drag from outside
    if LowerCase(ExtractFileExt(Data.Files[0])) = LowerCase('.aep') then begin
      BeginTryParseAEP(Data.Files[0]);
    end;

    if LowerCase(ExtractFileExt(Data.Files[0])) = LowerCase('.aerq') then
      ShowMessage('Queue dropped');

    if LowerCase(ExtractFileExt(Data.Files[0])) = LowerCase('.aer') then
      ShowMessage('Project configuration dropped');
  end;
end;

/// Implements basic hot-keys for main treeview
/// [Ctrl + D]  = Duplicate selected
/// [Del]       = Delete selected
/// [Ctrl + N]  = New task
procedure TMainForm.TasksTreeViewKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      68: begin // [D]
        case TasksTreeView.Selected.Level of
          2: begin
            ShowMessage(Format('Duplicate comp %s', [TasksTreeView.Selected.Text]));
          end;
          1: begin
            ShowMessage(Format('Duplicate %s', [TasksTreeView.Selected.Text]));
          end;
        end;
      end;
      90: begin // [Z]
        {try
          var NewItem: TTreeViewItem := (Temporary.Clone(nil) as TTreeViewItem);
          NewItem.Parent := Temporary.Parent;
        finally
          FreeAndNil(Temporary);
        end; }
      end;
    end
  else
    case Key of
      8, 46: begin // [Backspace], [Del]
        if TasksTreeView.Selected <> nil then // We can only delete something selected, othewise - access violation
          if TasksTreeView.Selected.ParentItem <> nil then begin // If composition item was deleted
            var Task: TRenderTask := GetTaskByProject(TasksTreeView.Selected.ParentItem.Text);
            RenderTasks.Items[RenderTasks.IndexOf(Task)].Compositions.Remove(GetCompByName(Task, TasksTreeView.Selected.Text));

            RenderTasksOnNotify(Sender, Task, cnRemoved);

            //raise ENotImplemented.Create('Not implemented yet')
          end else begin // We know for sure that it's a project item
            RenderTasks.Remove(GetTaskByProject(TasksTreeView.Selected.Text));
            FreeAndNil(TasksTreeView.Selected);
          end;

        //TasksIsEmpty.Visible := RenderTasks.Count = 0;
      end;
      19: begin // [Pause/Break]
        {$IFDEF DEBUG_MODE}
        if DebugLabel.Visible then
           DebugLabel.Visible := False
        else
           DebugLabel.Visible := True;
        {$ENDIF}    
      end;
    end;
  {$IFDEF DEBUG_MODE}DebugLabel.Text := Format('key = %d', [Key]);{$ENDIF}
end;

procedure TMainForm.TasksTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if TasksTreeView.ItemByPoint(X, Y) <> nil then
    FCurrentItem := TasksTreeView.ItemByPoint(X, Y).Text
  else
    FCurrentItem := 'nil';
end;

procedure TMainForm.compCountChange(Sender: TObject);
begin
  compGrid.RowCount := Round(compCount.Value);
end;

procedure TMainForm.compGridEditingDone(Sender: TObject; const ACol, ARow: Integer);
begin
  compName.Text := compGrid.Cells[0, 0];
end;

procedure TMainForm.threadsCount1Change(Sender: TObject);
begin
  threadsGrid.RowCount := threadsCount.Text.ToInteger();
  if not outFrame.Text.IsEmpty then
    calculateButtonClick(Sender);
end;

procedure TMainForm.threadsCountChange(Sender: TObject);
begin
  if not outFrame.Text.IsEmpty then begin
    if StrToInt(threadsCount.Text) > 128 then
      threadsCount.StyleLookup := 'comboediterrorstyle'
    else
      threadsCount.StyleLookup := 'comboeditstyle';
    calculateButtonClick(Sender);
  end;
  //threadsCount.Text := TempThreadsStr;
end;

procedure TMainForm.threadsCountExit(Sender: TObject);
begin
  //ShowMessage('Focus Lost');
  //threadsCount.Text := TempThreadsStr;
end;

procedure TMainForm.threadsCountTyping(Sender: TObject);
begin
  // Workaround... that won't work
  // Text in ComboEdit will be resetted to default value,
  // if you type in something that is not in it's list.
  // We must remember what text was in the ComboEdit field
  //TempThreadsStr := threadsCount.Text;
  if not threadsCount.Text.IsEmpty then begin
    if StrToInt(threadsCount.Text) > 128 then
      threadsCount.StyleLookup := 'comboediterrorstyle'
    else
      threadsCount.StyleLookup := 'comboeditstyle';
  end;
end;

procedure TMainForm.threadsSwitchSwitch(Sender: TObject);
begin
//  if threadsSwitch.IsChecked then
//    begin
//      if UpdateAvailable = True then
//        begin
//          if MainForm.Height <= 490 then
//            MainForm.Height := 630;
//        end
//      else
//        if MainForm.Height <= 450 then
//          MainForm.Height := 580;
//
//      threadsSwitchLabel.Text := Language[LANG].MainForm.SplitRender;
//      outFrame.TextPrompt     := Language[LANG].MainForm.EndFrameHint;
//      threadsGrid.AniCalculations.AutoShowing := False;
//      threadsGrid.Model.ScrollDirections := TScrollDirections.Vertical;
//    end
//  else
//    begin
//      if UpdateAvailable = True then
//        begin
//          if MainForm.WindowState <> TWindowState.wsMaximized then
//            MainForm.Height := 490
//        end
//      else
//        if MainForm.WindowState <> TWindowState.wsMaximized then
//          MainForm.Height := 450;
//
//      threadsSwitchLabel.Text := Language[LANG].MainForm.SingleRener;
//      outFrame.TextPrompt     := '';
//    end;
end;

procedure TMainForm.UpdateNetHTTPClientRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
begin
  gitRelease := TJsonObject.ParseJSONValue(AResponse.ContentAsString(TEncoding.UTF8));
  gitVersion := gitRelease.A[0].P['tag_name'].Value;

  if APPVERSION = gitVersion  then
    UpdateAvailable := False
  else
    UpdateAvailable := True;

  if UpdateAvailable then begin
    MainForm.Height := SizeConstraints.MIN_HEIGHT_UPD;
    MainForm.UpdateInfo.Visible := True;
    MainForm.UpdateInfo.Enabled := True;
    MainForm.downloadButton.Text := Language[LANG].MainForm.Download + ' (' + gitVersion + ')';
    {$IFDEF MSWINDOWS}gitDownload := gitRelease.A[0].P['assets'].A[1].P['browser_download_url'].Value;{$ENDIF MSWINDOWS}
    {$IFDEF MACOS}gitDownload := gitRelease.A[0].P['assets'].A[0].P['browser_download_url'].Value;{$ENDIF MACOS}
  end else begin
    MainForm.Height := SizeConstraints.MIN_HEIGHT_NO_UPD;
    MainForm.UpdateInfo.Visible := False;
    MainForm.UpdateInfo.Enabled := False;
  end;
end;

procedure TMainForm.UpdateNetHTTPClientRequestError(const Sender: TObject;
  const AError: string);
begin
  gitVersion := APPVERSION;
  UpdateAvailable := False;
  //ShowMessage(AError);
end;

procedure TMainForm.memUsageInfoClick(Sender: TObject);
begin
  memUsageInfoEdit.Text := Trunc((GetPlatformMemorySize/1024/1024) * (memUsageTrackBar.Value / 100)).ToString + ' MB';

  memUsageInfo.Visible := False;
  memUsageInfo.Enabled := False;
  memUsageInfoEdit.Visible := True;
  memUsageInfoEdit.Enabled := True;

  memUsageInfoEdit.SetFocus;
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
  if (tempText.Contains('unl') or tempText.Contains('max')) then
    memUsageInfoEdit.Text := Trunc(GetPlatformMemorySize / 1024 / 1024 + 1).ToString
  else
    begin
      if ((tempText.ToLower.Contains('mb')) or not (tempText[tempText.Length].IsDigit)) then
        memUsageInfoEdit.Text := memUsageInfoEdit.Text.Remove(memUsageInfoEdit.Text.Length - 3);

      if tempText.ToLower.Contains('kb') then
        memUsageInfoEdit.Text := (memUsageInfoEdit.Text.ToInteger() / 1024).ToString;    //KB -> MB

      if tempText.ToLower.Contains('gb') then
        memUsageInfoEdit.Text := (memUsageInfoEdit.Text.ToInteger() * 1024).ToString;    //GB -> MB
    end;
  if tempText.Contains('%') then
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
          TDialogServiceSync.MessageDialog((Text + ' ' + Language[LANG].Errors.MemoryValueInvalid), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
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
    memUsageInfo.Text := Language[LANG].MainForm.Unlimited
  else
    memUsageInfo.Text := Trunc(memUsageTrackBar.Value).ToString + '% (' + Trunc((GetPlatformMemorySize/1024/1024) * (memUsageTrackBar.Value / 100)).ToString + ' MB)';
end;

procedure TMainForm.TaskEditorCompButtonClick(Sender: TObject);
begin
  TaskEditorTabControl.TabIndex := 1;
end;

procedure TMainForm.TaskEditorCreateTaskButtonClick(Sender: TObject);
begin
  var ErrorStr: String := '';
  for var i := 0 to StrToInt(TaskEditorCompCount.Text) - 1 do begin
    /// EC001 :: Composition name is empty
    if TaskEditorCompGrid.Cells[1, i] = '' then
      ErrorStr := ErrorStr + Format('%s[%d]: (EC001) %s%s', ['Composition', i + 1, 'Composition name is empty', #13#10]);

    /// EC002 :: [Start Frame] and [End Frame] can''t be empty with [Split] more than 1
    if (TaskEditorCompGrid.Cells[2, i] = '') and (TaskEditorCompGrid.Cells[3, i] = '') and (TaskEditorCompGrid.Cells[4, i] <> '1') then
      ErrorStr := ErrorStr + Format('%s[%d]: (EC002) %s%s', ['Composition', i + 1, '[Start Frame] and [End Frame] can''t be empty with [Split] more than 1', #13#10]);

    /// New line for each comp
    if ErrorStr <> '' then
      ErrorStr := ErrorStr + #13#10;
  end;

  if ErrorStr <> '' then begin
    raise AERParamException.Create(Format('%s%s%s', [Language[LANG].Errors.errorsOccured, #13#10, ErrorStr]));
  end else begin
    /// Create all the compositions beforehand
    /// to append them to [Task]
    /// Also, lists should be created on initialization
    /// they are not TArray<> :c
    var Compositions: TList<TComposition> := TList<TComposition>.Create;

    for var i := 0 to Trunc(TaskEditorCompCount.Value) - 1 do begin
      Compositions.Add(TComposition.Create(
        TaskEditorCompGrid.Cells[1, i],
        TFrameSpan.Create(TaskEditorCompGrid.Cells[2, i], TaskEditorCompGrid.Cells[3, i]),
        StrToInt(TaskEditorCompGrid.Cells[4, i])
      ));
    end;

    var Task: TRenderTask := TRenderTask.Create(
      projectPath.Text,
      outputPath.Text,
      outputModuleBox.Items[outputModuleBox.ItemIndex],
      renderSettingsBox.Items[renderSettingsBox.ItemIndex],
      missingFilesCheckbox.IsChecked,
      soundCheckbox.IsChecked,
      threadedRender.IsChecked,
      IfThenElse(customCheckbox.IsChecked = True, customProp.Text, ''),  // yeet
      cacheUsageTrackBar.Value,
      memUsageTrackBar.Value,
      TList<TComposition>.Create(Compositions)
    );

    RenderTasks.Add(Task);
    {$IFDEF DEBUG_MODE}
    ShowMessage(RenderTasks.Items[0].ToString);
    {$ENDIF}

    Compositions.Free;

    PopupAniCloseHeight.Enabled := True;
    PopupAniCloseWidth.Enabled := True;

    TaskEditorCompCount.Value := 1;
    TaskEditorCompGrid.Cells[0, 0] := '1';
    TaskEditorCompGrid.Cells[1, 0] := '';
    TaskEditorCompGrid.Cells[2, 0] := '';
    TaskEditorCompGrid.Cells[3, 0] := '';
    TaskEditorCompGrid.Cells[4, 0] := '1';

    //UpdateTasksList;

    //TasksIsEmpty.Visible := RenderTasks.Count = 0;
  end;
end;

procedure TMainForm.cacheUsageInfoClick(Sender: TObject);
begin
  cacheUsageInfoEdit.Text := Trunc(cacheUsageTrackBar.Value).ToString + '%';

  cacheUsageInfo.Visible := False;
  cacheUsageInfo.Enabled := False;
  cacheUsageInfoEdit.Visible := True;
  cacheUsageInfoEdit.Enabled := True;

  cacheUsageInfoEdit.SetFocus;
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
      TDialogServiceSync.MessageDialog((Text + ' ' + Language[LANG].Errors.CacheValueInvalid), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
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
    cacheUsageInfo.Text := Language[LANG].MainForm.Unlimited
  else
    cacheUsageInfo.Text := Trunc(cacheUsageTrackBar.Value).ToString + '%';
end;

{$ENDREGION}

end.
