unit AErenderLauncherLocalization;

interface

type
  TLanguageCode = record
    const
      EN: Cardinal = 0;
      RU: Cardinal = 1;
      JP: Cardinal = 2;
  end;

  (*  Main Menu values  *)
  MainMenuText = record
    LauncherMenu, FileMenu, EditMenu, HelpMenu,
    ImportConfiguration, ExportConfiguration, Close,
    OutputModuleEditor, Settings,
    Documentation, About: String
  end;

  (*  Main Form  *)
  MainFormText = record
    MainMenu: MainMenuText;
    ProjectFile,
    OutputFile,
    OpenSaveProjectButton,
    OutputModulePreset,
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
    Style,
    DefaultProjectsDirectory,
    DefaultOutputDirectory,
    OnRenderStart,
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
    PresetName,
    OutputModule,
    OutputFileNameStructure,
    ProjectTab, CompositionTab, CompositionTimeTab, ImageTab, DateTab,
    Save, Cancel: String;
  end;

  (*  About Form  *)
  AboutFormText = record
    AErenderLauncher,
    CreatedBy,
    Description,
    FromRussiaWithLove,
    FFMPEG,
    Copyright: String;
  end;

  RenderingFormText = record
    
  end;

  LauncherText = record
    MainForm: MainFormText;
    SettingsForm: SettingsFormText;
    ImportForm: ImportFormText;
    OutputModuleConfiguratorForm: OutputModuleFormText;
    AboutForm: AboutFormText;
    RenderingForm: RenderingFormText;
    constructor InitFromFile(Path: String);
  end;

implementation

constructor LauncherText.InitFromFile(Path: String);
begin
  
end;

end.