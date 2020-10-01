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
    Mac.CodeBlocks, Macapi.Appkit, Macapi.ObjectiveC, Macapi.Foundation,
    Macapi.Dialogs, Macapi.Helpers, Macapi.ObjCRuntime, Macapi.CocoaTypes;
  {$ENDIF MACOS}{$ENDREGION}

type
  // This won't be required in the next RAD Studio version
  TComboEdit = class(TCustomComboEdit)
  procedure OnValidateEvent(Sender: TObject; var Text: string);
  procedure OnValidatingEvent(Sender: TObject; var Text: string);
  published
    property CanFocus default True;
    property CanParentFocus;
    property Cursor default crIBeam;
    property DisableFocusEffect;
    property DropDownCount default TComboEditModel.DefaultDropDownCount;
    property DropDownKind default TComboEditModel.DefaultDropDownKind;
    property KeyboardType;
    property ReadOnly;
    property ItemHeight;
    property ItemWidth;
    property Items;
    property ItemIndex;
    property ListBoxResource;
    property Text;
    property TextSettings;
    property Position;
    property Width;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property StyledSettings;
    property StyleLookup;
    property ClipChildren default False;
    property ClipParent default False;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible default True;
    property Caret;
    property KillFocusByReturn;
    property ParentShowHint;
    property ShowHint;
    { events }
    property OnChange;
    property OnChangeTracking;
    property OnTyping;
    property OnApplyStyleLookup;
    property OnClosePopup;
    property OnPopup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    property OnPresentationNameChoosing;
    property OnValidate;
    property OnValidating;
  end;
  TMainForm = class(TForm)
    projectPathLabel: TLabel;
    inputFileLayout: TLayout;
    outputPathLabel: TLabel;
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
    Image1: TImage;
    Image2: TImage;
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
    settingsIconFill: TFillRGBEffect;
    infoIconFill: TFillRGBEffect;
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
    Button1: TButton;
    recentItem: TMenuItem;
    separatorItem3: TMenuItem;
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
    procedure threadsCountChange(Sender: TObject);
    procedure UpdateNetHTTPClientRequestError(const Sender: TObject;
      const AError: string);
    procedure UpdateNetHTTPClientRequestCompleted(const Sender: TObject;
      const AResponse: IHTTPResponse);
    procedure ReadAERQ(Path: String);
    procedure SetLanguage(LanguageCode: Integer);
    procedure UpdateOutputModules;
    procedure UpdateRenderSettings;
    procedure threadsCountTyping(Sender: TObject);
    procedure threadsCountExit(Sender: TObject);
    procedure AddToRecents(Item: String);
    procedure RecentProjectSelected(Sender: TObject);
    procedure InflateRecents;
    procedure ReInflateRecents;
    procedure compGridEditingDone(Sender: TObject; const ACol,
      ARow: Integer);
    procedure renderSettingsBoxChange(Sender: TObject);
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
    Module,
    Mask: String;
    Imported: Boolean;
  end;
  RenderSetting = record
    Setting: String;
    Imported: Boolean;
  end;
  function GetPlatformMemorySize: Int64;
  function GetFFMPEGPath: WideString;
  function GetDirectoryFiles(Directory: String): TArray<System.String>;
  function KillProcess(ProcessName: String): Integer;
  procedure InitOutputModules;
  procedure InitRenderSettings;
  procedure InitConfiguration(Path: String);
  procedure LoadConfiguration(Path: String);
  procedure LoadLegacyConfiguration(Path: String);
  procedure SaveConfiguration(Path: String);
  function Open(Path: String): Integer;
  function Execute(Path: String): Integer;
  function BackgroundExecute(Path: String): Integer;
  procedure ChangeLanguage(LanguageCode: Integer);
  procedure InitLanguage(PATH: String);

const
  APPVERSION = 'v0.8.5-beta';
  //APPVERSION_DEMO = 'v0.8.5-beta';
  AERL_REPO_RELEASES = 'https://api.github.com/repos/lilystilson/aerender-launcher/releases';
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
  LANG, STYLE, ONRENDERSTART: Integer;                      (*  Language, Theme and OnRenderStart values    *)
  LogFiles: TArray<String>;                                 (*  All the aerender log files here             *)
  OutputModules: TArray<OutputModule>;                      (*  All the After Effects output modules here   *)
  RenderSettings: TArray<RenderSetting>;
  OMCount: Cardinal;
  //TempOutputModule: OutputModule;                         (*  Temporary Output Module used from import    *)
  TMathParser: MathExpParser.TExpressionParser;             (*  Mathematical parser for frames calculation  *)
  FHandleDragDirectly: Boolean = False;                     (*  For implementation of DragDrop functional   *)
  PARAMSTART: Boolean = False;
  isRendering: Boolean = False;
  Language: TArray<LauncherText>;
  Recents: array [0..9] of String;
  RecentsMenuItems: TArray<TMenuItem>;
  TempThreadsStr: String = '';
  ImportedPath: String = '';

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

procedure TComboEdit.OnValidateEvent(Sender: TObject; var Text: string);
begin
  TempThreadsStr := Text;
end;

procedure TComboEdit.OnValidatingEvent(Sender: TObject; var Text: string);
begin
  if (TempThreadsStr <> '') then begin
    Text := TempThreadsStr;
    TempThreadsStr := '';
  end;
end;

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
  RootNode.AddChild('aerender').Text := AERPATH;
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

function Open(Path: String): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'open', PWideChar(Path), nil, nil, SW_SHOW);
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    Result := _system(PAnsiChar('open ' + AnsiString('"' + Path + '"')));
  {$ENDIF MACOS}
end;

function Execute(Path: String): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'OPEN', PWideChar(Path), '', '', SW_SHOWNORMAL)
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    _system(PAnsiChar('chmox +x ' + AnsiString('"' + Path + '"')));
    Result := _system(PAnsiChar('command ' + AnsiString('"' + Path + '"')));
  {$ENDIF MACOS}
end;

function BackgroundExecute(Path: String): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'OPEN', PWideChar(Path), '', '', SW_HIDE)
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    _system(PAnsiChar('chmox +x ' + AnsiString('"' + Path + '"')));
    Result := _system(PAnsiChar('command ' + AnsiString('"' + Path + '" & disown')));
  {$ENDIF MACOS}
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

procedure TMainForm.ReadAERQ(Path: String);
begin
  var AERQXMLDocument: IXMLDocument := TXMLDocument.Create(nil);
  AERQXMLDocument.LoadFromFile(Path);
  AERQXMLDocument.Encoding := 'utf-8';

  AERQXMLDocument.Active := True;
  var RootNode: IXMLNode := AERQXMLDocument.DocumentElement;
  projectPath.Text := RootNode.Attributes['project'];
  outputPath.Text := RootNode.ChildNodes['queueItem'].Attributes['outputFolder'];
  tempSavePath := RootNode.ChildNodes['queueItem'].Attributes['outputFolder'] + PLATFORMPATHSEPARATOR;
  /// Add new Output Module to library if it don't exist
  if StrToBool(RootNode.ChildNodes['queueItem'].ChildNodes['outputModule'].Attributes['use']) then begin
    var TempOutputModule: OutputModule;
    TempOutputModule.Module := RootNode.ChildNodes['queueItem'].ChildNodes['outputModule'].ChildNodes['module'].Text;
    TempOutputModule.Mask   := RootNode.ChildNodes['queueItem'].ChildNodes['outputModule'].ChildNodes['mask'].Text;

    if GetOMIndex(TempOutputModule) <> -1 then
      outputModuleBox.ItemIndex := GetOMIndex(TempOutputModule)
    else begin
      SetLength(OutputModules, Length(OutputModules) + 1);
      OutputModules[High(OutputModules)].Module   := RootNode.ChildNodes['queueItem'].ChildNodes['outputModule'].ChildNodes['module'].Text;
      OutputModules[High(OutputModules)].Mask     := RootNode.ChildNodes['queueItem'].ChildNodes['outputModule'].ChildNodes['mask'].Text;
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

  compName.Text := RootNode.ChildNodes['queueItem'].ChildNodes['composition'].ChildNodes['name'].Text;
  inFrame.Text  := RootNode.ChildNodes['queueItem'].ChildNodes['composition'].ChildNodes['rangeStart'].Text;
  outFrame.Text := RootNode.ChildNodes['queueItem'].ChildNodes['composition'].ChildNodes['rangeEnd'].Text;
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
  if compSwitch.IsChecked then begin
    if UpdateAvailable = True then
      begin
        if MainForm.Height <= 490 then
          MainForm.Height := 630;//MainForm.Height + 130;
      end
    else
      if MainForm.Height <= 450 then
        MainForm.Height := 580;//MainForm.Height + 130;

    compSwitchLabel.Text := Language[LANG].MainForm.MultiComp;

    compGrid.RowCount := Round(compCount.Value);
    compGrid.Cells[0, 0] := compName.Text;
    compGrid.Model.ScrollDirections := TScrollDirections.Vertical;
  end else begin
    if UpdateAvailable = True then begin
      if MainForm.WindowState <> TWindowState.wsMaximized then
        MainForm.Height := 490//MainForm.Height - 130
    end else
      if MainForm.WindowState <> TWindowState.wsMaximized then
        MainForm.Height := 450;//MainForm.Height - 130;

    compSwitchLabel.Text := Language[LANG].MainForm.SingleComp;
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
  FormatSettings := TFormatSettings.Invariant;
  MainForm.Width := 600;
  MainForm.Height := {$IFDEF MSWINDOWS}450{$ENDIF MSWINDOWS}  {$IFDEF MACOS}430{$ENDIF MACOS};
  //MainForm.Caption := 'AErender Launcher (' + APPVERSION_DEMO + ')';
  MainForm.Caption := 'AErender Launcher (' + APPVERSION + ')';
  APPFOLDER :=  {$IFDEF MSWINDOWS}'C:\ProgramData\AErender\'{$ENDIF MSWINDOWS}
                {$IFDEF MACOS}GetEnvironmentVariable('HOME') + '/Documents/AErender/'{$ENDIF MACOS};
  {$IFDEF MSWINDOWS}
  FreeAndNil(launcherItem);
  exitItem.ShortCut := TextToShortCut('Alt+F4');
  exportConfigItem.ShortCut := TextToShortCut('Ctrl+E');
  importConfigItem.ShortCut := TextToShortCut('Ctrl+I');
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
  FreeAndNil(editItem);
  FreeAndNil(exitItem);
  {$ENDIF MACOS}

  if DirectoryExists(APPFOLDER) then
    AssignFile(CFG, APPFOLDER + 'AErenderConfiguration.xml')
  else
    begin
      CreateDir(APPFOLDER);
      AssignFile(CFG, APPFOLDER + 'AErenderConfiguration.xml');
    end;
  if FileExists(APPFOLDER + 'AErenderConfiguration.xml') then
    begin
      try
        LoadConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
      except
        on Exception do begin
          try
            LoadLegacyConfiguration(APPFOLDER + 'AErenderConfiguration.xml');
          except
            on Exception do begin
              if (TDialogServiceSync.MessageDialog(('Configuration file is corrupted! Press OK to renew configuration file. Application will be restarted.' + #13#10 +
                                {$IFDEF MSWINDOWS}'C:\ProgramData\AErender\AErenderConfiguration.xml'{$ENDIF MSWINDOWS}
                                    {$IFDEF MACOS}'~/Documents/AErender/AErenderConfiguration.xml'{$ENDIF MACOS}),
                TMsgDlgType.mtError, mbOKCancel, TMsgDlgBtn.mbOK, 0) = 1) then
              begin
                System.SysUtils.DeleteFile(APPFOLDER + 'AErenderConfiguration.xml');
                {$IFDEF MSWINDOWS}ShellExecute(0, 'OPEN', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);{$ENDIF MSWINDOWS}
                {$IFDEF MACOS}_system(PAnsiChar('open -a "' + AnsiString(ParamStr(0)) + '" & disown'));{$ENDIF MACOS}
              end;
              Halt;
            end;
          end;
        end;
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

  InflateRecents;

  outputModuleBox.ListBox.AniCalculations.Animation := True;

  compGrid.AniCalculations.AutoShowing := False;
  compGrid.AniCalculations.Animation := True;

  threadsGrid.AniCalculations.Animation := True;

  threadsCount.OnValidate := threadsCount.OnValidateEvent;
  threadsCount.OnValidating := threadsCount.OnValidatingEvent;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  // RU: Установка размеров для колонн в таблицах
  StringColumn1.Width := compGrid.Width;
  StringColumn2.Width := threadsGrid.Width * 0.5;
  StringColumn3.Width := threadsGrid.Width * 0.5;

  // RU: Ограничение ширины формы
  if MainForm.Width < 600 then
    MainForm.Width := 600;

  // RU: Ограничение высоты формы в зависимости от платформы и видимости панели с обновлением
  if UpdateAvailable then begin
    if MainForm.Height < {$IFDEF MSWINDOWS}490{$ENDIF MSWINDOWS}  {$IFDEF MACOS}470{$ENDIF MACOS} then begin
      MainForm.Height := {$IFDEF MSWINDOWS}490{$ENDIF MSWINDOWS}  {$IFDEF MACOS}470{$ENDIF MACOS};
      {$IFDEF MSWINDOWS}Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);{$ENDIF MSWINDOWS}
    end;
  end else begin
    if MainForm.Height < {$IFDEF MSWINDOWS}450{$ENDIF MSWINDOWS}  {$IFDEF MACOS}430{$ENDIF MACOS} then begin
      MainForm.Height := {$IFDEF MSWINDOWS}450{$ENDIF MSWINDOWS}  {$IFDEF MACOS}430{$ENDIF MACOS};
    {$IFDEF MSWINDOWS}Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);{$ENDIF MSWINDOWS}
    end;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
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

  SplashScreenForm.Close;
  SplashScreenForm.Free;

  if (ParamCount > 0) and (ParamStr(1) = '-aer') then begin
    ImportUnit.PARAMSTART := True;
    ImportUnit.XMLPath := ParamStr(2);
    ImportForm.ShowModal;
  end else if (ParamStr(1) = '-aerq') then begin
    MainUnit.PARAMSTART := True;
    ReadAERQ(ParamStr(2));
  end;

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
type
  exec = record
    script: String;
    F: TextFile;
  end;
var
  threads, comps, emptyComps: Integer;
  PATH, logPath{, prgPath}: String;
  execFile: array [1..128] of exec;

  // Notification: TNotification;
begin
  //Error Codes
  if not (SettingsForm.HandleCheckBox.IsChecked and isRendering) then begin
    emptyComps := 0;
    if AERPATH.IsEmpty then
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
    else
      begin
        if threadsSwitch.IsChecked then
          begin
            threads := threadsCount.Text.ToInteger;
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
              /// Script compiling section

              // Clear the string
              execFile[i].script := '';

              // Add encoding header to ensure that our CMD on Windows will use UTF-8
              {$IFDEF MSWINDOWS}
              execFile[i].script := 'chcp 65001' + #13#10;
              {$ENDIF MSWINDOWS}

              // Make console output data from it if progress display is enabled
              // Made by using default in Shell and Bash  ( command ) > data/output/path.log
              // Open bracket here
              if SettingsForm.HandleCheckBox.IsChecked then
                execFile[i].script := execFile[i].script + '(';

              // Ensure that logs paths won't comflict with each other
              if comps = 1 then
                logPath := APPFOLDER + compName.Text + '_' + i.ToString
              else
                logPath := APPFOLDER + compGrid.Cells[0, j] + '_' + i.ToString;

              // Add Pass output path to temporary wariable
              PATH := outputPath.Text;

              // Create folder if '[projectName]/' or '[projectName]\' is used
              // Because aerender won't do it for you
              if outputPath.Text.Contains('[projectName]' + PLATFORMPATHSEPARATOR) then
                begin
                  PATH := StringReplace(PATH, '[projectName]', ExtractFileName(projectPath.Text), [rfReplaceAll, rfIgnoreCase]);
                  if not DirectoryExists(ExtractFilePath(PATH)) then
                    CreateDir(ExtractFilePath(PATH));
                end;

              // Adjust file output path if split render is enabled
              // because they can conflict
              if threadsSwitch.IsChecked then
                begin
                  var FilePath: String := ExtractFilePath(PATH);
                  var FileName: String := StringReplace(ExtractFileName(PATH), ExtractFileExt(PATH), '', [rfReplaceAll, rfIgnoreCase]);
                  var FileExt:  String := ExtractFileExt(PATH);
                  PATH := FilePath + FileName + '_' + i.ToString + FileExt;
                end;

              // Adjust file output path if multi comp render is enabled
              // because they can conflict
              if compSwitch.IsChecked then
                if not outputPath.Text.Contains('[compName]') then
                  begin
                    var FilePath: String := ExtractFilePath(PATH);
                    var FileName: String := StringReplace(ExtractFileName(PATH), ExtractFileExt(PATH), '', [rfReplaceAll, rfIgnoreCase]);
                    var FileExt:  String := ExtractFileExt(PATH);
                    PATH := FilePath + FileName + '_' + compGrid.Cells[0, j] + FileExt;
                  end;

              /// Begin executable compiling section
              // Add aerender path to script
              execFile[i].script := execFile[i].script + '"' + AERPATH + '" ' + '-project "' + projectPath.Text + '" -output "' + PATH + '" ';

              // Add comp name to script
              if compSwitch.IsChecked then
                execFile[i].script := execFile[i].script + '-comp "' + compGrid.Cells[0, j] + '" '
              else
                execFile[i].script := execFile[i].script + '-comp "' + compName.Text + '" ';

              // Add start and end ranges to script
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

              // Add sound flag to script
              if soundCheckbox.IsChecked then
                execFile[i].script := execFile[i].script + '-sound ON ';

              // Add multiprocessing flag to script
              if threadedRender.IsChecked then
                execFile[i].script := execFile[i].script + '-mp ';

              // Add missing footage flag to script
              if missingFilesCheckbox.IsChecked then
                execFile[i].script := execFile[i].script + '-continueOnMissingFootage ';

              // Add output module flag to script
              if outputModuleBox.ItemIndex <> -1 then
                execFile[i].script := execFile[i].script + '-OMtemplate "' + OutputModules[outputModuleBox.ItemIndex].Module + '" ';

              if renderSettingsBox.ItemIndex <> -1 then
                execFile[i].script := execFile[i].script + '-RStemplate  "' + RenderSettings[outputModuleBox.ItemIndex].Setting + '" ';

              // Add memory usage flags to script
              execFile[i].script := execFile[i].script + '-mem_usage "' + Trunc(memUsageTrackBar.Value).ToString + '" "' + Trunc(cacheUsageTrackBar.Value).ToString + '" ';

              // Add whatever user typed parameters to script
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
                  AssignFile (execFile[i].F, 'C:\ProgramData\AErender\aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.bat', CP_UTF8)
                else
                  AssignFile (execFile[i].F, 'C:\ProgramData\AErender\aerender' + i.ToString + '.bat', CP_UTF8);
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
                    //Execute('C:\ProgramData\AErender\aerender' + i.ToString + '.bat');
              {$ENDIF MSWINOWS}
              {$IFDEF MACOS}
                if compSwitch.IsChecked then
                  AssignFile (execFile[i].F, GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.command', CP_UTF8)
                else
                  AssignFile (execFile[i].F, GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command', CP_UTF8);
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
                      //Execute(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command"');
                    end
              {$ENDIF MACOS}
            end;
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

procedure TMainForm.settingsButtonClick(Sender: TObject);
begin
  SettingsForm.ShowModal;
end;

procedure TMainForm.compCountChange(Sender: TObject);
begin
  compGrid.RowCount := Round(compCount.Value);
end;

procedure TMainForm.compGridEditingDone(Sender: TObject; const ACol,
  ARow: Integer);
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
  if threadsSwitch.IsChecked then
    begin
      if UpdateAvailable = True then
        begin
          if MainForm.Height <= 490 then
            MainForm.Height := 630;
        end
      else
        if MainForm.Height <= 450 then
          MainForm.Height := 580;

      threadsSwitchLabel.Text := Language[LANG].MainForm.SplitRender;
      outFrame.TextPrompt     := Language[LANG].MainForm.EndFrameHint;
      threadsGrid.AniCalculations.AutoShowing := False;
      threadsGrid.Model.ScrollDirections := TScrollDirections.Vertical;
    end
  else
    begin
      if UpdateAvailable = True then
        begin
          if MainForm.WindowState <> TWindowState.wsMaximized then
            MainForm.Height := 490
        end
      else
        if MainForm.WindowState <> TWindowState.wsMaximized then
          MainForm.Height := 450;

      threadsSwitchLabel.Text := Language[LANG].MainForm.SingleRener;
      outFrame.TextPrompt     := '';
    end;
end;

procedure TMainForm.UpdateNetHTTPClientRequestCompleted(
  const Sender: TObject; const AResponse: IHTTPResponse);
begin
  gitRelease := TJsonObject.ParseJSONValue(AResponse.ContentAsString(TEncoding.UTF8));
  gitVersion := gitRelease.A[0].P['tag_name'].Value;

  if APPVERSION = gitVersion  then
    UpdateAvailable := False
  else
    UpdateAvailable := True;

  if UpdateAvailable then begin
    if threadsSwitch.IsChecked or compSwitch.IsChecked then
      MainForm.Height := 630
    else
      MainForm.Height := {$IFDEF MSWINDOWS}490{$ENDIF MSWINDOWS}  {$IFDEF MACOS}470{$ENDIF MACOS};
    MainForm.UpdateInfo.Visible := True;
    MainForm.UpdateInfo.Enabled := True;
    MainForm.downloadButton.Text := Language[LANG].MainForm.Download + ' (' + gitVersion + ')';
    {$IFDEF MSWINDOWS}gitDownload := gitRelease.A[0].P['assets'].A[1].P['browser_download_url'].Value;{$ENDIF MSWINDOWS}
    {$IFDEF MACOS}gitDownload := gitRelease.A[0].P['assets'].A[0].P['browser_download_url'].Value;{$ENDIF MACOS}
  end else begin
    MainForm.Height := {$IFDEF MSWINDOWS}450{$ENDIF MSWINDOWS}  {$IFDEF MACOS}430{$ENDIF MACOS};
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
