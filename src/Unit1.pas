unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.IOUtils, System.Character,
  System.JSON, System.Net.HttpClient, System.Threading,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Platform,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.EditBox, FMX.SpinBox,
  FMX.ComboTrackBar, System.ImageList, FMX.ImgList, FMX.ListBox, FMX.Objects, FMX.Menus,
  Xml.xmldom, Xml.XMLIntf, Xml.omnixmldom, Xml.XMLDoc, Xml.adomxmldom, FMX.MultiView, FMX.ExtCtrls, FMX.DialogService.Sync,
  FMX.Effects, FMX.Filter.Effects, System.Notification, FMX.Ani, FMX.Memo,
  {$IFDEF MSWINDOWS}
    Winapi.ShellAPI, Winapi.Windows, FMX.Platform.Win, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    Posix.Stdlib, Posix.Unistd, Posix.SysSysctl, Posix.SysTypes;
  {$ENDIF MACOS}


type
  TForm1 = class(TForm)
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
    FlowLayout1: TFlowLayout;
    inFrame: TEdit;
    FlowLayout2: TFlowLayout;
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
    outputModule: TComboBox;
    outputModuleLabel: TLabel;
    outputModuleLayout: TLayout;
    ListBoxItem1: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
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
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    ListBoxItem13: TListBoxItem;
    ListBoxItem14: TListBoxItem;
    ListBoxItem15: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    ListBoxItem17: TListBoxItem;
    MetropolisUIListBoxItem1: TMetropolisUIListBoxItem;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    XMLDocument1: TXMLDocument;
    MainMenu1: TMainMenu;
    launcherItem: TMenuItem;
    fileItem: TMenuItem;
    importConfigItem: TMenuItem;
    exportConfigItem: TMenuItem;
    settingsItem0: TMenuItem;
    separatorItem1: TMenuItem;
    exitItem: TMenuItem;
    editItem: TMenuItem;
    settingsItem: TMenuItem;
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
    UpdateInfoLayout: TLayout;
    UpdateLabel: TLabel;
    downloadButton: TButton;
    Line1: TLine;
    ffmpegCheckBox: TCheckBox;
    ffmpegConfigButton: TButton;
    ffmpegConcateLayout: TLayout;
    LinkControlToPropertyVisible7: TLinkControlToProperty;
    LinkControlToPropertyEnabled10: TLinkControlToProperty;
    LinkControlToPropertyEnabled11: TLinkControlToProperty;
    renderingBlurEffect: TBlurEffect;
    mainLayout: TLayout;
    FloatAnimation1: TFloatAnimation;
    popupBackground: TRectangle;
    renderingPopupProgressLabel: TLabel;
    renderingProgress: TProgressBar;
    renderingProgressLabel: TLabel;
    renderingPopup: TLayout;
    ShadowEffect1: TShadowEffect;
    renderingTimer: TTimer;
    mainRenderingLayout: TLayout;
    Memo1: TMemo;
    Memo2: TMemo;
    stap: TButton;
    test: TLayout;
    StyleBook1: TStyleBook;
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
    procedure compNameChange(Sender: TObject);
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  APPVERSION = 'v0.8.0-beta';

var
  Form1: TForm1;
  CFG: TextFile;
  VER, LANG, AERPATH, DEFPRGPATH, DEFOUTPATH, ERR, gitResponse, gitVersion, gitDownload, ffmpegPath: String;
  gitRelease: TJsonValue;
  UpdateAvailable: Boolean = False;
  FFMPEG: Boolean = False;
  RenderWindowSender: TButton;
  MEMORY, STYLE, ONRENDERSTART: Integer;
  LogFiles: TArray<System.String>;

implementation

{$R *.fmx}

uses
  Unit2, Unit3, Unit4, Unit5, Unit6, RenderingUnit;

{$REGION '  ResetEditorErrorHighlighting = True'}
function ResetEditorErrorHighlighting(): Boolean;
begin
  for var i := 0 to 10 do
    if True then
      Result := true;
end;
{$ENDREGION}

function GetPlatformMemorySize: Int64;            //BYTES, BLYAD
var
  {$IFDEF MSWINDOWS}
    MS_Ex: MemoryStatusEx;
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
    res: Int64;
    len: size_t;
  {$ENDIF POSIX}
begin
  {$IFDEF MSWINDOWS}
    FillChar (MS_Ex, SizeOf (MemoryStatusEx), #0);
    MS_Ex.dwLength := SizeOf (MemoryStatusEx);
    GlobalMemoryStatusEx (MS_Ex);
    Result := MS_Ex.ullTotalPhys;
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
    len := SizeOf (Result);
    res := SysCtlByName ('hw.memsize', @Result, @len, nil, 0);
    if res <> 0 then
      RaiseLastOSError;
  {$ENDIF POSIX}
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

function IsFFMPEGPresent: Boolean;
var
  AERenderDirectory: String;
  Folders: System.TArray<System.String>;
begin
  {$IFDEF MSWINDOWS}AERenderDirectory := 'C:\ProgramData\AErender';{$ENDIF MSWINDOWS}
  {$IFDEF POSIX}AERenderDirectory := GetEnvironmentVariable('HOME') + '/Documents/AErender/';{$ENDIF}

  Folders := TDirectory.GetDirectories(AErenderDirectory);

  for var i := 0 to High(Folders) do
    if Folders[i].Contains('ffmpeg') then
      begin
        Result := True;
        ffmpegPath := Folders[i];
      end
    else
      Result := False;
end;

function GetDirectoryFiles(Directory: String): TArray<System.String>;
var
  Files: System.TArray<System.String>;
  Logs: Integer;
begin
  Logs := 1;
  Files := TDirectory.GetFiles(Directory);
  for var i := 0 to High(Files) do
    if Files[i].Contains('.log') then
      begin
        SetLength(Result, Logs);
        Result[Logs-1] := Files[i];
        inc (Logs);
      end;
end;

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
{$ENDIF MSWINDOWS}

procedure TForm1.aboutItemClick(Sender: TObject);
begin
  Form5.Show;
end;

procedure TForm1.calculateButtonClick(Sender: TObject);
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

procedure TForm1.compNameChange(Sender: TObject);
begin
  compGrid.Cells[0, 0] := compName.Text;
end;

procedure TForm1.compSwitchSwitch(Sender: TObject);
begin
  if compSwitch.IsChecked then
    begin
      if UpdateAvailable = True then
        begin
          if Form1.Height <= 450 then
            Form1.Height := Form1.Height + 130;
        end
      else
        if Form1.Height <= 420 then
          Form1.Height := Form1.Height + 130;
      if LANG = 'EN' then
        compSwitchLabel.Text := 'Multiple Compositions';
      if LANG = 'RU' then
        compSwitchLabel.Text := 'Несколько композиций';
      compGrid.AniCalculations.AutoShowing := False;
      compGrid.RowCount := Round(compCount.Value);
      compGrid.Cells[0, 0] := compName.Text;
      compGrid.Model.ScrollDirections := TScrollDirections.Vertical;
    end
  else
    begin
      if UpdateAvailable = True then
        begin
          if Form1.Height <= 580 then
            Form1.Height := Form1.Height - 130
        end
      else
        if Form1.Height <= 550 then
          Form1.Height := Form1.Height - 130;
      if LANG = 'EN' then
        compSwitchLabel.Text := 'Single Composition';
      if LANG = 'RU' then
        compSwitchLabel.Text := 'Одна композиция';
      compName.Text := compGrid.Cells[0, 0];
    end;
end;

procedure TForm1.docsItemClick(Sender: TObject);
begin
  Form3.Show;
end;

procedure TForm1.downloadButtonClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PWideChar(gitDownload), nil, nil, SW_SHOW);
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
    _system(PAnsiChar('open ' + AnsiString('"' + gitDownload + '"')));
  {$ENDIF POSIX}
end;

procedure TForm1.exitItemClick(Sender: TObject);
begin
  Form1.Close;
end;

procedure TForm1.ffmpegConfigButtonClick(Sender: TObject);
begin
  Form6.ShowModal;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  KillProcess('AfterFX.com');
  Rewrite (CFG);

  //Language
  Writeln (CFG, LANG);

  //Style
  Writeln (CFG, STYLE.ToString);

  //OnRenderStart Event
  Writeln (CFG, ONRENDERSTART.ToString);

  //Project Path
  if projectPath.Text.IsEmpty then
    Writeln (CFG, '')
  else
    Writeln (CFG, projectPath.Text);

  //Output Path
  if outputPath.Text.IsEmpty then
    Writeln (CFG, '')
  else
    Writeln (CFG, outputPath.Text);

  //Composition Name
  if compName.Text.IsEmpty then
    Writeln (CFG, '')
  else
    Writeln (CFG, compName.Text);

  //Start Frame
  if inFrame.Text.IsEmpty then
    Writeln (CFG, '')
  else
    Writeln (CFG, inFrame.Text);

  //End Frame
  if outFrame.Text.IsEmpty then
    Writeln (CFG, '')
  else
    Writeln (CFG, outFrame.Text);

  //Missing Files
  if missingFilesCheckbox.IsChecked then
    Writeln (CFG, 'True')
  else
    Writeln (CFG, 'False');

  //Sound on render finish
  if soundCheckbox.IsChecked then
    Writeln (CFG, 'True')
  else
    Writeln (CFG, 'False');

  //Threaded Render
  if threadedRender.IsChecked then
    Writeln (CFG, 'True')
  else
    Writeln (CFG, 'False');

  //Custom Properties
  if customCheckbox.IsChecked then
    begin
      Writeln (CFG, 'True');
      Writeln (CFG, customProp.Text);
    end
  else
    begin
      Writeln (CFG, 'False');
      Writeln (CFG, '');
    end;

  //AErender Path
  if AERPATH.IsEmpty then
    if Form2.aerenderPath.Text.isEmpty then
      Writeln (CFG, '')
    else
      Writeln (CFG, Form2.aerenderPath.Text)
  else
    Writeln (CFG, AERPATH);

  //Default Projects Path
  if (Form2.defaultProjectsPath.Text.IsEmpty) and (DEFPRGPATH = '') then
    Writeln (CFG, '')
  else
    Writeln (CFG, DEFPRGPATH);

  //Default Outputs Path
  if (Form2.defaultOutputPath.Text.IsEmpty) and (DEFOUTPATH = '') then
    Writeln (CFG, '')
  else
    Writeln (CFG, DEFOUTPATH);

  //Memory Limit
  Writeln (CFG, memUsageTrackBar.Value.ToString);

  //Cache Limit
  Writeln (CFG, cacheUsageTrackBar.Value.ToString);

  CloseFile (CFG);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  PRGP, OUTP, COMP, INFR, OTFR, MF, SOUND, THREAD, PROP, PROPTEXT, MEMLIMIT, CACHELIMIT: String;
begin
  //Form1.Width := 600;
  Form1.Caption := 'AErender Launcher (' + APPVERSION + ')';
  UpdateAvailable := IsUpdateAvailable;
  if UpdateAvailable = True then
    begin
      //Form1.Height := 450;
      UpdateInfoLayout.Visible := True;
      UpdateInfoLayout.Enabled := True;
      downloadButton.Text := 'Download from GitHub (' + gitVersion + ')';
      {$IFDEF MSWINDOWS}
        gitDownload := gitRelease.GetValue<string>('[0].assets[1].browser_download_url');
      {$ENDIF MSWINDOWS}
      {$IFDEF POSIX}
        gitDownload := gitRelease.GetValue<string>('[0].assets[0].browser_download_url');
      {$ENDIF POSIX}
    end
  else
    begin
      UpdateInfoLayout.Visible := False;
      UpdateInfoLayout.Enabled := False;
      //Form1.Height := 420;
    end;

  if IsFFMPEGPresent then
    begin
      FFMPEG := True;
      ffmpegCheckBox.Enabled := True;
    end
  else
    begin
      FFMPEG := False;
      ffmpegCheckBox.Enabled := False;
      ffmpegCheckBox.Hint := 'FFMPEG is not found at' + {$IFDEF MSWINDOWS} 'C:\ProgramData\AErender' {$ENDIF MSWINDOWS}
                                                        {$IFDEF POSIX} '~/Documents/AErender' {$ENDIF POSIX} + 'directory';
    end;
  {$IFDEF MSWINDOWS}
  //Form1.Height := 444;
  MainMenu1.Destroy;
  if DirectoryExists ('C:\ProgramData\AErender') then
    AssignFile (CFG, 'C:\ProgramData\AErender\AErenderConfiguration.cfg')
  else
    begin
      CreateDir ('C:\ProgramData\AErender');
      AssignFile (CFG, 'C:\ProgramData\AErender\AErenderConfiguration.cfg');
    end;
  if FileExists('C:\ProgramData\AErender\AErenderConfiguration.cfg') then
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
  editItem.Visible := False; editItem.Enabled := False;
  MenuBar1.Destroy;
  if DirectoryExists (GetEnvironmentVariable('HOME') + '/Documents/AErender') then
    AssignFile (CFG, GetEnvironmentVariable('HOME') + '/Documents/AErender/AErenderConfiguration.cfg')
  else
    begin
      CreateDir (GetEnvironmentVariable('HOME') + '/Documents/AErender');
      AssignFile (CFG, GetEnvironmentVariable('HOME') + '/Documents/AErender/AErenderConfiguration.cfg');
      AssignFile (CFG, './AErenderConfiguration.cfg');
    end;
  if FileExists(GetEnvironmentVariable('HOME') + '/Documents/AErender/AErenderConfiguration.cfg') then
  {$ENDIF POSIX}
    begin
      //Reading values
      Reset (CFG);
      try
        Readln (CFG, LANG);             //Language
        Readln (CFG, STYLE);            //Style
        Readln (CFG, ONRENDERSTART);    //OnRenderStart Event
        Readln (CFG, PRGP);             //Project Path
        Readln (CFG, OUTP);             //Output Path
        Readln (CFG, COMP);             //Composition Name
        Readln (CFG, INFR);             //Start Frame
        Readln (CFG, OTFR);             //Out Frame
        Readln (CFG, MF);               //Missing Files
        Readln (CFG, SOUND);            //Sound on finish
        Readln (CFG, THREAD);           //Threaded render
        Readln (CFG, PROP);             //Custom Properties
        Readln (CFG, PROPTEXT);         //Custom Properties Text
        Readln (CFG, AERPATH);          //AErender Path
        Readln (CFG, DEFPRGPATH);       //Default Project Path
        Readln (CFG, DEFOUTPATH);       //Default Output Path
        Readln (CFG, MEMLIMIT);         //Memory Limit
        Readln (CFG, CACHELIMIT);       //Cache Limit

        //Assigning values
        projectPath.Text := PRGP;
        outputPath.Text := OUTP;
        compName.Text := COMP;
        inFrame.Text := INFR;
        outFrame.Text := OTFR;
        missingFilesCheckbox.IsChecked := MF.ToBoolean();
        soundCheckbox.IsChecked := SOUND.ToBoolean();
        threadedRender.IsChecked := THREAD.ToBoolean();
        customCheckbox.IsChecked := PROP.ToBoolean();
        customProp.Text := PROPTEXT;
        memUsageTrackBar.Value := MEMLIMIT.ToSingle();
        cacheUsageTrackBar.Value := CACHELIMIT.ToSingle();
        Lang1.Lang := LANG;
        CloseFile (CFG);
      except
        if (TDialogServiceSync.MessageDialog(('Configuration file is corrupted! Press OK to renew configuration file. Application will be restarted.' + #13#10 +
                              {$IFDEF MSWINDOWS}'C:\ProgramData\AErender\AErenderConfiguration.cfg read error.'{$ENDIF MSWINDOWS}
                                  {$IFDEF POSIX}'~/Documents/AErender/AErenderConfiguration.cfg read error.'{$ENDIF POSIX}),
              TMsgDlgType.mtError, mbOKCancel, TMsgDlgBtn.mbOK, 0) = 1) then
            begin
              CloseFile (CFG);
              {$IFDEF MSWINDOWS}DeleteFile ('C:\ProgramData\AErender\AErenderConfiguration.cfg');{$ENDIF MSWINDOWS}
              {$IFDEF POSIX}DeleteFile (GetEnvironmentVariable('HOME') + '/Documents/AErender/AErenderConfiguration.cfg');{$ENDIF POSIX}
              {$IFDEF MSWINDOWS}ShellExecute(0, 'OPEN', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);{$ENDIF MSWINDOWS}
              {$IFDEF POSIX}_system(PAnsiChar('open "' + AnsiString(ParamStr(0)) + '"'));{$ENDIF POSIX}
            end;
        Application.Terminate;
      end;
    end
  else
    begin
      Rewrite (CFG);
      Writeln (CFG, '');             //Language
      Writeln (CFG, '0');            //Style
      Writeln (CFG, '0');            //OnRenderStart Event
      Writeln (CFG, '');             //Project Path
      Writeln (CFG, '');             //Output Path
      Writeln (CFG, '');             //Composition Name
      Writeln (CFG, '');             //Start Frame
      Writeln (CFG, '');             //Out Frame
      Writeln (CFG, 'False');        //Missing Files
      Writeln (CFG, 'False');        //Sound on finish
      Writeln (CFG, 'False');        //Threaded render
      Writeln (CFG, 'False');        //Custom Properties
      Writeln (CFG, '');             //Custom Properties Text
      Writeln (CFG, '');             //AErender Path
      Writeln (CFG, '');             //Default Project Path
      Writeln (CFG, '');             //Default Output Path
      Writeln (CFG, '100');          //Memory Limit
      Writeln (CFG, '100');          //Cache Limit
      LANG := 'EN';
      STYLE := 0;
      ONRENDERSTART := 0;
      VER := '0';
      Lang1.Lang := LANG;
      CloseFile (CFG);
    end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  compLayout.Width := Form1.Width * 0.5;
  FlowLayout1.Width := threadsLayout.Width * 0.5;
  StringColumn1.Width := compGrid.Width;
  StringColumn2.Width := threadsGrid.Width * 0.5;
  StringColumn3.Width := threadsGrid.Width * 0.5;
  propertiesLayout1.Width := properties.Width * 0.5;
  renderingPopup.Width := Form1.Width * 0.75;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Form2.styleBox.ItemIndex := STYLE;
  Form2.onRenderStartBox.ItemIndex := ONRENDERSTART;
  if (ParamCount > 0) and (ParamStr(1).Contains('-aer')) then
    begin
      Unit4.PARAMSTART := True;
      Unit4.XMLPath := ParamStr(2);
      Form4.ShowModal;
    end;
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
end;

procedure TForm1.importConfigItemClick(Sender: TObject);
begin
  compSwitch.Enabled := True;
  compSwitch.IsChecked := False;
  threadsSwitch.Enabled := True;
  threadsSwitch.IsChecked := False;

  with XMLOpenDialog do
    if Execute then
      Form4.ShowModal;
end;

procedure TForm1.infoButtonClick(Sender: TObject);
begin
  RenderWindowSender := infoButton;
  RenderingForm.Show;
end;

procedure TForm1.launchButtonClick(Sender: TObject);
type
  exec = record
    script: String;
    F: TextFile;
  end;
var
  threads, comps, emptyComps: Integer;
  PATH: String;
  execFile: array [1..100] of exec;

  Notification: TNotification;
begin
  //Error Codes
  if LANG = 'EN' then
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
    end
  else
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
    end;

  //Proceed if no errors occured
  if not ERR.IsEmpty then
    begin
      if LANG = 'EN' then
        MessageDlg(('The following error(s) has occured: ' + ERR), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
      else
        MessageDlg(('Произошли следующие ошибки: ' + ERR), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
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
//          SetLength (LogFiles, threads);
        end;
      if compSwitch.IsChecked then
        begin
          comps := StrToInt(compCount.Value.ToString);
          SetLength (LogFiles, comps);
        end
      else
        begin
          comps := 1;
//          SetLength (LogFiles, comps);
        end;

      for var j := 0 to comps-1 do
        for var i := 1 to threads do
          begin
            //Script compiling section
            execFile[i].script := '(';
            PATH := outputPath.Text;
            PATH.Insert(Length(Path)-4, '_' + compGrid.Cells[0, j] + '_' + i.ToString);
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

            execFile[i].script := execFile[i].script + '-mem_usage "' + Trunc(cacheUsageTrackBar.Value).ToString + '" "' + Trunc(memUsageTrackBar.Value).ToString + '" ';


            if customCheckbox.IsChecked then
              execFile[i].script := execFile[i].script + customProp.Text;

            execFile[i].script := execFile[i].script + ') > "' + PATH.Remove(PATH.Length - 4) + '.log"';

            if threadsSwitch.IsChecked then
              LogFiles[i-1] := PATH.Remove(PATH.Length - 4) + '.log'
            else
              LogFiles[j] := PATH.Remove(PATH.Length - 4) + '.log';

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
              if compSwitch.IsChecked then
                ShellExecute(0, 'OPEN', PChar('C:\ProgramData\AErender\aerender' + i.ToString + '_' +  compGrid.Cells[0, j] + '.bat'), '', '', SW_HIDE)
              else
                ShellExecute(0, 'OPEN', PChar('C:\ProgramData\AErender\aerender' + i.ToString + '.bat'), '', '', SW_HIDE);
            {$ENDIF MSWINOWS}
            {$IFDEF POSIX}
              if compSwitch.IsChecked then
                AssignFile (execFile[i].F, GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.command')
              else
                AssignFile (execFile[i].F, GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command');
              Rewrite (execFile[i].F);
              Writeln (execFile[i].F, execFile[i].script);
              Writeln (execFile[i].F, 'read -p "Press any key to continue..."');
              CloseFile (execFile[i].F);
              if compSwitch.IsChecked then
                begin
                  _system(PAnsiChar('chmod +x "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.command"')));
                  _system(PAnsiChar('open "' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '_' + compGrid.Cells[0, j] + '.command"')));
                end
              else
                begin
                  _system(PAnsiChar('chmod +x ' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command')));
                  _system(PAnsiChar('open ' + AnsiString(GetEnvironmentVariable('HOME') + '/Documents/AErender/aerender' + i.ToString + '.command')));
                end;
            {$ENDIF POSIX}
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
    end;

  Sleep (2000);
  if compSwitch.IsChecked then
    RenderingForm.TotalProgressBar.Max := Length(LogFiles)
  else
    RenderingForm.TotalProgressBar.Max := outFrame.Text.ToInteger() + 50 * Length(LogFiles);
  //RenderingForm.framesLabel.Text := '0 / ' + outFrame.Text + ' Frames';
  RenderWindowSender := launchButton;
  RenderingForm.renderingTimer.Enabled := True;
  RenderingForm.Show;

  //OnRenderStart Actions
  case Form2.onRenderStartBox.ItemIndex of
    1:begin
        WindowState := TWindowState.wsMinimized;
      end;
  end
end;

procedure TForm1.openFileClick(Sender: TObject);
begin
  with AEPOpenDialog do
    if Execute then
      projectPath.Text := AEPOpenDialog.FileName;
end;

procedure TForm1.saveFileClick(Sender: TObject);
begin
  with SaveDialog1 do
    if Execute then
      outputPath.Text := SaveDialog1.FileName;
end;

procedure TForm1.settingsButtonClick(Sender: TObject);
begin
  Form2.ShowModal;
end;

procedure TForm1.compCountChange(Sender: TObject);
begin
  compGrid.RowCount := Round(compCount.Value);
end;

procedure TForm1.threadsCountChange(Sender: TObject);
begin
  threadsGrid.RowCount := threadsCount.Items[threadsCount.ItemIndex].ToInteger();
  if not outFrame.Text.IsEmpty then
    calculateButtonClick(Sender);
end;

procedure TForm1.threadsSwitchSwitch(Sender: TObject);
begin
  if threadsSwitch.IsChecked then
    begin
      if UpdateAvailable = True then
        begin
          if Form1.Height <= 450 then
            Form1.Height := Form1.Height + 130;
        end
      else
        if Form1.Height <= 420 then
          Form1.Height := Form1.Height + 130;
      if LANG = 'EN' then
        threadsSwitchLabel.Text := 'Split Render';
      if LANG = 'RU' then
        threadsSwitchLabel.Text := 'Рендерить частями';
      threadsGrid.AniCalculations.AutoShowing := False;
      threadsGrid.Model.ScrollDirections := TScrollDirections.Vertical;
    end
  else
    begin
      if UpdateAvailable = True then
        begin
          if Form1.Height <= 580 then
            Form1.Height := Form1.Height - 130
        end
      else
        if Form1.Height <= 550 then
          Form1.Height := Form1.Height - 130;
      if LANG = 'EN' then
        threadsSwitchLabel.Text := 'Single Render';
      if LANG = 'RU' then
        threadsSwitchLabel.Text := 'Рендерить одним файлом';
    end;
end;

procedure TForm1.memUsageInfoClick(Sender: TObject);
begin
  memUsageInfoEdit.Text := Trunc((GetPlatformMemorySize/1024/1024) * (memUsageTrackBar.Value / 100)).ToString + ' MB';

  memUsageInfo.Visible := False;
  memUsageInfo.Enabled := False;
  memUsageInfoEdit.Visible := True;
  memUsageInfoEdit.Enabled := True;

  memUsageInfoEdit.ResetSelection;
  memUsageInfoEdit.SelectAll;
end;

procedure TForm1.memUsageInfoEditExit(Sender: TObject);
begin
  memUsageInfo.Visible := True;
  memUsageInfo.Enabled := True;
  memUsageInfoEdit.Visible := False;
  memUsageInfoEdit.Enabled := False;
end;

procedure TForm1.memUsageInfoEditValidate(Sender: TObject;
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
            if LANG = 'EN' then
              MessageDlg((Text + ' is not a valid memory value. Try <value unit> or <value> instead.'), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
            if LANG = 'RU' then
              MessageDlg((Text + ' недействительное значение памяти. Используйте <value unit> или <value>.'), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
          end;
      end;
    end;
  memUsageInfo.Visible := True;
  memUsageInfo.Enabled := True;
  memUsageInfoEdit.Visible := False;
  memUsageInfoEdit.Enabled := False;
end;

procedure TForm1.memUsageTrackBarChange(Sender: TObject);
begin
  if memUsageTrackBar.Value = 100 then
    memUsageInfo.Text := 'Unlimited'
  else
    memUsageInfo.Text := Trunc(memUsageTrackBar.Value).ToString + '% (' + Trunc((GetPlatformMemorySize/1024/1024) * (memUsageTrackBar.Value / 100)).ToString + ' MB)';
end;

procedure TForm1.cacheUsageInfoClick(Sender: TObject);
begin
  cacheUsageInfoEdit.Text := Trunc(cacheUsageTrackBar.Value).ToString + '%';

  cacheUsageInfo.Visible := False;
  cacheUsageInfo.Enabled := False;
  cacheUsageInfoEdit.Visible := True;
  cacheUsageInfoEdit.Enabled := True;

  cacheUsageInfoEdit.ResetSelection;
  cacheUsageInfoEdit.SelectAll;
end;

procedure TForm1.cacheUsageInfoEditExit(Sender: TObject);
begin
  cacheUsageInfo.Visible := True;
  cacheUsageInfo.Enabled := True;
  cacheUsageInfoEdit.Visible := False;
  cacheUsageInfoEdit.Enabled := False;
end;

procedure TForm1.cacheUsageInfoEditValidate(Sender: TObject;
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
        if LANG = 'EN' then
          MessageDlg((Text + ' is invalid percentage value. Try <value %> or <value> instead.'), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
        if LANG = 'RU' then
          MessageDlg((Text + ' недействительное процентное значение. Используйте <value %> или <value>.'), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      end;
  end;
  tempText := '';
  cacheUsageTrackBar.Value := cacheUsageInfoEdit.Text.ToSingle();

  cacheUsageInfo.Visible := True;
  cacheUsageInfo.Enabled := True;
  cacheUsageInfoEdit.Visible := False;
  cacheUsageInfoEdit.Enabled := False;
end;

procedure TForm1.cacheUsageTrackBarChange(Sender: TObject);
begin
  if cacheUsageTrackBar.Value = 100 then
    cacheUsageInfo.Text := 'Unlimited'
  else
    cacheUsageInfo.Text := Trunc(cacheUsageTrackBar.Value).ToString + '%';
end;

end.
