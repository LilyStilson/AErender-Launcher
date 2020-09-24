unit AErenderLauncherLocalization;

interface

uses
  System.Types, System.Character, System.Variants, System.IOUtils, System.SysUtils, System.Classes, System.JSON;

type
  /// I didn't use it anywhere, but I'll leave it here
  /// Maybe this will come useful someday
  TLanguageCode = record
    const
      EN: Cardinal = 0;
      RU: Cardinal = 1;
      JP: Cardinal = 2;
  end;

  (*  Main Menu values  *)
  MainMenuText = record
    LauncherMenu, FileMenu, EditMenu, HelpMenu,
    RecentProjects, ImportConfiguration, ExportConfiguration,
    Close, CloseDarwin, OutputModuleEditor, Settings,
    Documentation, About: String
  end;

  (*  Main Form  *)
  MainFormText = record
    MainMenu: MainMenuText;
    ProjectFile,
    OutputFile,
    OpenSaveProjectButton,
    DarwinDialogTip, DarwinDialogOpen, DarwinDialogSaveNameField, DarwinDialogSave,
    OutputModulePreset,
    ConfigureOutputModules,
    RenderSettings,
    ConfigureRenderSettings,
    Properties,
    MissingFiles,
    Sound,
    Threaded,
    Custom,
    CustomPropHint,
    CacheUsageLimit,
    MemUsage,
    Unlimited,
    SingleComp, MultiComp,
    SingleRener, SplitRender,
    CompNameHint, MultiCompGridHeader,
    StartFrame, EndFrame,
    EndFrameHint,
    Calculate,
    Launch,
    NewVersionAvailable, Download: String
  end;

  (*  Settings Form  *)
  SettingsFormText = record
    LauncherSettings, 
    RenderEnginePath,
    DefaultProjectsDirectory,
    DefaultOutputDirectory,
    UserInterface,
    Style,
    Language, LangChange,
    InterfaceScale,
    Behaviour,
    OnRenderStart, DoNothing, MinimizeLauncher, CloseLauncher,
    HandleAerender,
    DeleteTemporary: String;
  end;

  (*  Import Form  *)
  ImportFormText = record
    ImportAEProject,
    ProjectPath,
    Compositions,
    CompositonProperties,
    Name, Resolution, Framerate, RangeStart, RangeEnd,
    ImportProperties,
    PrepareForSplitRendering,
    SelectAll, DeselectAll,
    Import: String;
  end;

  (*  Output Modules Editor Form  *)
  OutputModuleFormText = record
    OutputModulePresetConfigurator, 
    Imported,
    OutputModule, OutputModulePrompt,
    OutputFileNameStructure, OutputFileNamePrompt,
    ProjectTab, CompositionTab, CompositionTimeTab, ImageTab, DateTab,
    Save, Cancel, CloseDialog: String;
  end;

  (*  About Form  *)
  AboutFormText = record
    AErenderLauncher,
    CreatedBy,
    Description,
    FromRussiaWithLove,
    FFMPEG, FFMPEGNotFound,
    Copyright: String;
  end;

  RenderingFormText = record
    RenderingProgress,
    HandleDisabled,
    QueueIsEmpty,
    TotalProgress,
    TimeElapsed,
    AbortRendering,
    WaitingForAerender,
    Rendering,
    RenderingError,
    RenderingFinished: String;
  end;

  ErrorText = record
    MemoryValueInvalid,
    CacheValueInvalid,
    CalculateFrameError,
    CalculateUnknownError,
    ConfigCorrupted,
    aerenderInvalid,
    aerenderUndetectable,
    errorsOccured,
    aerenderIsEmpty,
    projectIsEmpty,
    outputIsEmpty,
    compositionIsEmpty,
    multiCompIsEmpty,
    isCurrentlyRendering,
    IncompatibleFile: String;
  end;

  LauncherText = record
    Language, Author, Completion: String;
    MainForm: MainFormText;
    SettingsForm: SettingsFormText;
    ImportForm: ImportFormText;
    OutputModuleConfiguratorForm: OutputModuleFormText;
    AboutForm: AboutFormText;
    RenderingForm: RenderingFormText;
    Errors: ErrorText;
    Result: Integer;

    /// <summary>
    /// Creates AErender Launcher Language data from specified .aerlang JSON file.
    /// </summary>
    constructor InitFromFile(Path: String);
    constructor InitFromStringList(StringList: TStringList);
    constructor InitFromResourceStream(Stream: TResourceStream);

    /// <summary>
    /// Parses .aerlang JSON file
    /// </summary
    function ParseJSONLanguage(const LanguageData: TJsonValue): LauncherText;
  end;

implementation

function LauncherText.ParseJSONLanguage(const LanguageData: TJsonValue): LauncherText;
begin
  {$REGION '    Language Data   '}
  Result.Language   := LanguageData.P['language'].Value;
  Result.Author     := LanguageData.P['author'].Value;
  Result.Completion := LanguageData.P['completion'].Value;
  {$ENDREGION}

  {$REGION '    Main Form Text    '}

  {$REGION '    Menubar Text    '}
  Result.MainForm.MainMenu.LauncherMenu         := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['LauncherMenu'].Value;
  Result.MainForm.MainMenu.FileMenu             := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['FileMenu'].Value;
  Result.MainForm.MainMenu.EditMenu             := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['EditMenu'].Value;
  Result.MainForm.MainMenu.HelpMenu             := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['HelpMenu'].Value;
  Result.MainForm.MainMenu.RecentProjects       := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['RecentProjects'].Value;
  Result.MainForm.MainMenu.ImportConfiguration  := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['ImportConfiguration'].Value;
  Result.MainForm.MainMenu.ExportConfiguration  := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['ExportConfiguration'].Value;
  Result.MainForm.MainMenu.Close                := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['Close'].Value;
  Result.MainForm.MainMenu.CloseDarwin          := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['CloseDarwin'].Value;
  Result.MainForm.MainMenu.OutputModuleEditor   := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['OutputModuleEditor'].Value;
  Result.MainForm.MainMenu.Settings             := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['Settings'].Value;
  Result.MainForm.MainMenu.Documentation        := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['Documentation'].Value;
  Result.MainForm.MainMenu.About                := LanguageData.P['LauncherText'].P['MainFormText'].P['MainMenuText'].P['About'].Value;
  {$ENDREGION '    Menubar Text    '}

  Result.MainForm.OutputFile                    := LanguageData.P['LauncherText'].P['MainFormText'].P['OutputFile'].Value;
  Result.MainForm.ProjectFile                   := LanguageData.P['LauncherText'].P['MainFormText'].P['ProjectFile'].Value;
  Result.MainForm.OpenSaveProjectButton         := LanguageData.P['LauncherText'].P['MainFormText'].P['OpenSaveProjectButton'].Value;
  Result.MainForm.DarwinDialogTip               := LanguageData.P['LauncherText'].P['MainFormText'].P['DarwinDialogTip'].Value;
  Result.MainForm.DarwinDialogOpen              := LanguageData.P['LauncherText'].P['MainFormText'].P['DarwinDialogOpen'].Value;
  Result.MainForm.DarwinDialogSaveNameField     := LanguageData.P['LauncherText'].P['MainFormText'].P['DarwinDialogSaveNameField'].Value;
  Result.MainForm.DarwinDialogSave              := LanguageData.P['LauncherText'].P['MainFormText'].P['DarwinDialogSave'].Value;
  Result.MainForm.OutputModulePreset            := LanguageData.P['LauncherText'].P['MainFormText'].P['OutputModulePreset'].Value;
  Result.MainForm.ConfigureOutputModules        := LanguageData.P['LauncherText'].P['MainFormText'].P['ConfigureOutputModules'].Value;
  Result.MainForm.RenderSettings                := LanguageData.P['LauncherText'].P['MainFormText'].P['RenderSettings'].Value;
  Result.MainForm.ConfigureRenderSettings       := LanguageData.P['LauncherText'].P['MainFormText'].P['ConfigureRenderSettings'].Value;
  Result.MainForm.Properties                    := LanguageData.P['LauncherText'].P['MainFormText'].P['Properties'].Value;
  Result.MainForm.MissingFiles                  := LanguageData.P['LauncherText'].P['MainFormText'].P['MissingFiles'].Value;
  Result.MainForm.Sound                         := LanguageData.P['LauncherText'].P['MainFormText'].P['Sound'].Value;
  Result.MainForm.Threaded                      := LanguageData.P['LauncherText'].P['MainFormText'].P['Threaded'].Value;
  Result.MainForm.Custom                        := LanguageData.P['LauncherText'].P['MainFormText'].P['Custom'].Value;
  Result.MainForm.CustomPropHint                := LanguageData.P['LauncherText'].P['MainFormText'].P['CustomPropHint'].Value;
  Result.MainForm.CacheUsageLimit               := LanguageData.P['LauncherText'].P['MainFormText'].P['CacheUsageLimit'].Value;
  Result.MainForm.MemUsage                      := LanguageData.P['LauncherText'].P['MainFormText'].P['MemUsage'].Value;
  Result.MainForm.Unlimited                     := LanguageData.P['LauncherText'].P['MainFormText'].P['Unlimited'].Value;
  Result.MainForm.SingleComp                    := LanguageData.P['LauncherText'].P['MainFormText'].P['SingleComp'].Value;
  Result.MainForm.MultiComp                     := LanguageData.P['LauncherText'].P['MainFormText'].P['MultiComp'].Value;
  Result.MainForm.SingleRener                   := LanguageData.P['LauncherText'].P['MainFormText'].P['SingleRener'].Value;
  Result.MainForm.SplitRender                   := LanguageData.P['LauncherText'].P['MainFormText'].P['SplitRender'].Value;
  Result.MainForm.CompNameHint                  := LanguageData.P['LauncherText'].P['MainFormText'].P['CompNameHint'].Value;
  Result.MainForm.MultiCompGridHeader           := LanguageData.P['LauncherText'].P['MainFormText'].P['MultiCompGridHeader'].Value;
  Result.MainForm.StartFrame                    := LanguageData.P['LauncherText'].P['MainFormText'].P['StartFrame'].Value;
  Result.MainForm.EndFrame                      := LanguageData.P['LauncherText'].P['MainFormText'].P['EndFrame'].Value;
  Result.MainForm.EndFrameHint                  := LanguageData.P['LauncherText'].P['MainFormText'].P['EndFrameHint'].Value;
  Result.MainForm.Calculate                     := LanguageData.P['LauncherText'].P['MainFormText'].P['Calculate'].Value;
  Result.MainForm.Launch                        := LanguageData.P['LauncherText'].P['MainFormText'].P['Launch'].Value;
  Result.MainForm.NewVersionAvailable           := LanguageData.P['LauncherText'].P['MainFormText'].P['NewVersionAvailable'].Value;
  Result.MainForm.Download                      := LanguageData.P['LauncherText'].P['MainFormText'].P['Download'].Value;
  {$ENDREGION}

  {$REGION '    Settings Form Text    '}
  Result.SettingsForm.LauncherSettings          := LanguageData.P['LauncherText'].P['SettingsFormText'].P['LauncherSettings'].Value;
  Result.SettingsForm.RenderEnginePath          := LanguageData.P['LauncherText'].P['SettingsFormText'].P['RenderEnginePath'].Value;
  Result.SettingsForm.DefaultProjectsDirectory  := LanguageData.P['LauncherText'].P['SettingsFormText'].P['DefaultProjectsDirectory'].Value;
  Result.SettingsForm.DefaultOutputDirectory    := LanguageData.P['LauncherText'].P['SettingsFormText'].P['DefaultOutputDirectory'].Value;
  Result.SettingsForm.UserInterface             := LanguageData.P['LauncherText'].P['SettingsFormText'].P['UserInterface'].Value;
  Result.SettingsForm.Style                     := LanguageData.P['LauncherText'].P['SettingsFormText'].P['Style'].Value;
  Result.SettingsForm.Language                  := LanguageData.P['LauncherText'].P['SettingsFormText'].P['Language'].Value;
  Result.SettingsForm.LangChange                := LanguageData.P['LauncherText'].P['SettingsFormText'].P['LangChange'].Value;
  Result.SettingsForm.InterfaceScale            := LanguageData.P['LauncherText'].P['SettingsFormText'].P['InterfaceScale'].Value;
  Result.SettingsForm.Behaviour                 := LanguageData.P['LauncherText'].P['SettingsFormText'].P['Behaviour'].Value;
  Result.SettingsForm.OnRenderStart             := LanguageData.P['LauncherText'].P['SettingsFormText'].P['OnRenderStart'].Value;
  Result.SettingsForm.DoNothing                 := LanguageData.P['LauncherText'].P['SettingsFormText'].P['DoNothing'].Value;
  Result.SettingsForm.MinimizeLauncher          := LanguageData.P['LauncherText'].P['SettingsFormText'].P['MinimizeLauncher'].Value;
  Result.SettingsForm.CloseLauncher             := LanguageData.P['LauncherText'].P['SettingsFormText'].P['CloseLauncher'].Value;
  Result.SettingsForm.HandleAerender            := LanguageData.P['LauncherText'].P['SettingsFormText'].P['HandleAerender'].Value;
  Result.SettingsForm.DeleteTemporary           := LanguageData.P['LauncherText'].P['SettingsFormText'].P['DeleteTemporary'].Value;
  {$ENDREGION}

  {$REGION '    Import Form Text    '}
  Result.ImportForm.ImportAEProject             := LanguageData.P['LauncherText'].P['ImportFormText'].P['ImportAEProject'].Value;
  Result.ImportForm.ProjectPath                 := LanguageData.P['LauncherText'].P['ImportFormText'].P['ProjectPath'].Value;
  Result.ImportForm.Compositions                := LanguageData.P['LauncherText'].P['ImportFormText'].P['Compositions'].Value;
  Result.ImportForm.CompositonProperties        := LanguageData.P['LauncherText'].P['ImportFormText'].P['CompositionProperties'].Value;
  Result.ImportForm.Name                        := LanguageData.P['LauncherText'].P['ImportFormText'].P['Name'].Value;
  Result.ImportForm.Resolution                  := LanguageData.P['LauncherText'].P['ImportFormText'].P['Resolution'].Value;
  Result.ImportForm.Framerate                   := LanguageData.P['LauncherText'].P['ImportFormText'].P['Framerate'].Value;
  Result.ImportForm.RangeStart                  := LanguageData.P['LauncherText'].P['ImportFormText'].P['RangeStart'].Value;
  Result.ImportForm.RangeEnd                    := LanguageData.P['LauncherText'].P['ImportFormText'].P['RangeEnd'].Value;
  Result.ImportForm.ImportProperties            := LanguageData.P['LauncherText'].P['ImportFormText'].P['ImportProperties'].Value;
  Result.ImportForm.PrepareForSplitRendering    := LanguageData.P['LauncherText'].P['ImportFormText'].P['PrepareForSplitRendering'].Value;
  Result.ImportForm.SelectAll                   := LanguageData.P['LauncherText'].P['ImportFormText'].P['SelectAll'].Value;
  Result.ImportForm.DeselectAll                 := LanguageData.P['LauncherText'].P['ImportFormText'].P['DeselectAll'].Value;
  Result.ImportForm.Import                      := LanguageData.P['LauncherText'].P['ImportFormText'].P['Import'].Value;
  {$ENDREGION}

  {$REGION '    Output Module Configurator Form Text    '}
  Result.OutputModuleConfiguratorForm.OutputModulePresetConfigurator  := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['OutputModulePresetConfigurator'].Value;
  Result.OutputModuleConfiguratorForm.OutputModule                    := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['OutputModule'].Value;
  Result.OutputModuleConfiguratorForm.OutputModulePrompt              := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['OutputModulePrompt'].Value;
  Result.OutputModuleConfiguratorForm.OutputFileNameStructure         := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['OutputFileNameStructure'].Value;
  Result.OutputModuleConfiguratorForm.OutputFileNamePrompt            := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['OutputFileNamePrompt'].Value;
  Result.OutputModuleConfiguratorForm.Imported                        := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['Imported'].Value;
  Result.OutputModuleConfiguratorForm.ProjectTab                      := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['ProjectTab'].Value;
  Result.OutputModuleConfiguratorForm.CompositionTab                  := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['CompositionTab'].Value;
  Result.OutputModuleConfiguratorForm.CompositionTimeTab              := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['CompositionTimeTab'].Value;
  Result.OutputModuleConfiguratorForm.ImageTab                        := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['ImageTab'].Value;
  Result.OutputModuleConfiguratorForm.DateTab                         := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['DateTab'].Value;
  Result.OutputModuleConfiguratorForm.Save                            := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['Save'].Value;
  Result.OutputModuleConfiguratorForm.Cancel                          := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['Cancel'].Value;
  Result.OutputModuleConfiguratorForm.CloseDialog                     := LanguageData.P['LauncherText'].P['OutputModuleFormText'].P['CloseDialog'].Value;
  {$ENDREGION}

  {$REGION '    Rendering Form Text    '}
  Result.RenderingForm.RenderingProgress        := LanguageData.P['LauncherText'].P['RenderingFormText'].P['RenderingProgress'].Value;
  Result.RenderingForm.HandleDisabled           := LanguageData.P['LauncherText'].P['RenderingFormText'].P['HandleDisabled'].Value;
  Result.RenderingForm.QueueIsEmpty             := LanguageData.P['LauncherText'].P['RenderingFormText'].P['QueueIsEmpty'].Value;
  Result.RenderingForm.TotalProgress            := LanguageData.P['LauncherText'].P['RenderingFormText'].P['TotalProgress'].Value;
  Result.RenderingForm.TimeElapsed              := LanguageData.P['LauncherText'].P['RenderingFormText'].P['TimeElapsed'].Value;
  Result.RenderingForm.AbortRendering           := LanguageData.P['LauncherText'].P['RenderingFormText'].P['AbortRendering'].Value;
  Result.RenderingForm.WaitingForAerender       := LanguageData.P['LauncherText'].P['RenderingFormText'].P['WaitingForAerender'].Value;
  Result.RenderingForm.Rendering                := LanguageData.P['LauncherText'].P['RenderingFormText'].P['Rendering'].Value;
  Result.RenderingForm.RenderingError           := LanguageData.P['LauncherText'].P['RenderingFormText'].P['RenderingError'].Value;
  Result.RenderingForm.RenderingFinished        := LanguageData.P['LauncherText'].P['RenderingFormText'].P['RenderingFinished'].Value;
  {$ENDREGION}

  {$REGION '    About Form Text    '}
  Result.AboutForm.AErenderLauncher             := LanguageData.P['LauncherText'].P['AboutFormText'].P['AErenderLauncher'].Value;
  Result.AboutForm.CreatedBy                    := LanguageData.P['LauncherText'].P['AboutFormText'].P['CreatedBy'].Value;
  Result.AboutForm.Description                  := LanguageData.P['LauncherText'].P['AboutFormText'].P['Description'].Value;
  Result.AboutForm.FromRussiaWithLove           := LanguageData.P['LauncherText'].P['AboutFormText'].P['FromRussiaWithLove'].Value;
  Result.AboutForm.FFMPEG                       := LanguageData.P['LauncherText'].P['AboutFormText'].P['FFMPEG'].Value;
  Result.AboutForm.FFMPEGNotFound               := LanguageData.P['LauncherText'].P['AboutFormText'].P['FFMPEGNotFound'].Value;
  Result.AboutForm.Copyright                    := LanguageData.P['LauncherText'].P['AboutFormText'].P['Copyright'].Value;
  {$ENDREGION}

  {$REGION '    Errors Text    '}
  Result.Errors.MemoryValueInvalid              := LanguageData.P['LauncherText'].P['ErrorText'].P['MemoryValueInvalid'].Value;
  Result.Errors.CacheValueInvalid               := LanguageData.P['LauncherText'].P['ErrorText'].P['CacheValueInvalid'].Value;
  Result.Errors.CalculateFrameError             := LanguageData.P['LauncherText'].P['ErrorText'].P['CalculateFrameError'].Value;
  Result.Errors.CalculateUnknownError           := LanguageData.P['LauncherText'].P['ErrorText'].P['CalculateUnknownError'].Value;
  Result.Errors.ConfigCorrupted                 := LanguageData.P['LauncherText'].P['ErrorText'].P['ConfigCorrupted'].Value;
  Result.Errors.aerenderInvalid                 := LanguageData.P['LauncherText'].P['ErrorText'].P['aerenderInvalid'].Value;
  Result.Errors.aerenderUndetectable            := LanguageData.P['LauncherText'].P['ErrorText'].P['aerenderUndetectable'].Value;
  Result.Errors.errorsOccured                   := LanguageData.P['LauncherText'].P['ErrorText'].P['errorsOccured'].Value;
  Result.Errors.aerenderIsEmpty                 := LanguageData.P['LauncherText'].P['ErrorText'].P['aerenderIsEmpty'].Value;
  Result.Errors.projectIsEmpty                  := LanguageData.P['LauncherText'].P['ErrorText'].P['projectIsEmpty'].Value;  
  Result.Errors.outputIsEmpty                   := LanguageData.P['LauncherText'].P['ErrorText'].P['outputIsEmpty'].Value;
  Result.Errors.compositionIsEmpty              := LanguageData.P['LauncherText'].P['ErrorText'].P['compositionIsEmpty'].Value;
  Result.Errors.multiCompIsEmpty                := LanguageData.P['LauncherText'].P['ErrorText'].P['multiCompIsEmpty'].Value;
  Result.Errors.isCurrentlyRendering            := LanguageData.P['LauncherText'].P['ErrorText'].P['isCurrentlyRendering'].Value;
  Result.Errors.IncompatibleFile                := LanguageData.P['LauncherText'].P['ErrorText'].P['IncompatibleFile'].Value;
  {$ENDREGION}
end;

constructor LauncherText.InitFromFile(Path: String);
var
  LanguageString: WideString;
  LanguageData: TJsonValue;
begin
  try
    LanguageString := TFile.ReadAllText(Path, TEncoding.UTF8);

    LanguageData := TJSONObject.ParseJSONValue(LanguageString);

    Self := ParseJSONLanguage(LanguageData);

    Self.Result := 0;
  except
    on Exception do
      Self.Result := -1;
  end;
end;

constructor LauncherText.InitFromStringList(StringList: TStringList);
var
  LanguageData: TJsonValue;
begin
  try
    LanguageData := TJSONObject.ParseJSONValue(StringList.Text);

    Self := ParseJSONLanguage(LanguageData);

    Self.Result := 0;
  except
    on Exception do
      Self.Result := -1;
  end;
end;

constructor LauncherText.InitFromResourceStream(Stream: TResourceStream);
var
  LanguageSource: TStringList;
  LanguageData: TJsonValue;
begin
  try
    LanguageSource := TStringList.Create;
    LanguageSource.LoadFromStream(Stream, TEncoding.UTF8);

    LanguageData := TJSONObject.ParseJSONValue(LanguageSource.Text);
    Self := ParseJSONLanguage(LanguageData);
    Self.Result := 0;
  except
    on Exception do
      Self.Result := -1;
  end;
end;

end.