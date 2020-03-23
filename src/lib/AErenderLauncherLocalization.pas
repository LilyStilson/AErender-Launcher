unit AErenderLauncherLocalization;

interface

uses
  System.Types, System.Character, System.Variants, System.IOUtils, System.SysUtils, System.JSON;

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
    CloseDarwin, OutputModuleEditor, Settings,
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
    FFMPEG, FFMPEGNotFound,
    Copyright: String;
  end;

  RenderingFormText = record
    RenderingProgress,
    HandleDisabled,
    QueueIsEmpty,
    TotalProgress,
    TimeElapsed,
    AbortRendering: String;
  end;

  LauncherText = record
    Language, Author, Completion: String;
    MainForm: MainFormText;
    SettingsForm: SettingsFormText;
    ImportForm: ImportFormText;
    OutputModuleConfiguratorForm: OutputModuleFormText;
    AboutForm: AboutFormText;
    RenderingForm: RenderingFormText;
    Result: Integer;
    constructor InitFromFile(Path: String);
    //destructor Free;
  end;

implementation

constructor LauncherText.InitFromFile(Path: String);
var
  LanguageString: WideString;
  LanguageData: TJsonValue;
begin
  try
    LanguageString := TFile.ReadAllText(Path, TEncoding.UTF8);

    LanguageData := TJsonObject.ParseJSONValue(LanguageString);

    {$REGION '    Language Data   '}
    Self.Language   := LanguageData.P['language'].Value;
    Self.Author     := LanguageData.P['author'].Value;
    Self.Completion := LanguageData.P['completion'].Value;
    {$ENDREGION}

    {$REGION '    Main Form Text    '}
    {$REGION '    Menubar Text    '}
    Self.MainForm.MainMenu.LauncherMenu         := LanguageData.P['LauncherText.MainFormText.MainMenu.LauncherMenu'].Value;
    Self.MainForm.MainMenu.FileMenu             := LanguageData.P['LauncherText.MainFormText.MainMenu.FileMenu'].Value;
    Self.MainForm.MainMenu.EditMenu             := LanguageData.P['LauncherText.MainFormText.MainMenu.EditMenu'].Value;
    Self.MainForm.MainMenu.HelpMenu             := LanguageData.P['LauncherText.MainFormText.MainMenu.HelpMenu'].Value;
    Self.MainForm.MainMenu.ImportConfiguration  := LanguageData.P['LauncherText.MainFormText.MainMenu.ImportConfiguration'].Value;
    Self.MainForm.MainMenu.ExportConfiguration  := LanguageData.P['LauncherText.MainFormText.MainMenu.ExportConfiguration'].Value;
    Self.MainForm.MainMenu.Close                := LanguageData.P['LauncherText.MainFormText.MainMenu.Close'].Value;
    Self.MainForm.MainMenu.CloseDarwin          := LanguageData.P['LauncherText.MainFormText.MainMenu.CloseDarwin'].Value;
    Self.MainForm.MainMenu.OutputModuleEditor   := LanguageData.P['LauncherText.MainFormText.MainMenu.OutputModuleEditor'].Value;
    Self.MainForm.MainMenu.Settings             := LanguageData.P['LauncherText.MainFormText.MainMenu.Settings'].Value;
    Self.MainForm.MainMenu.Documentation        := LanguageData.P['LauncherText.MainFormText.MainMenu.Documentation'].Value;
    Self.MainForm.MainMenu.About                := LanguageData.P['LauncherText.MainFormText.MainMenu.About'].Value;
    {$ENDREGION '    Menubar Text    '}

    Self.MainForm.ProjectFile                   := LanguageData.P['LauncherText.MainFormText.ProjectFile'].Value;
    Self.MainForm.OutputFile                    := LanguageData.P['LauncherText.MainFormText.OutputFile'].Value;
    Self.MainForm.OpenSaveProjectButton         := LanguageData.P['LauncherText.MainFormText.OpenSaveProjectButton'].Value;
    Self.MainForm.DarwinDialogTip               := LanguageData.P['LauncherText.MainFormText.DarwinDialogTip'].Value;
    Self.MainForm.DarwinDialogOpen              := LanguageData.P['LauncherText.MainFormText.DarwinDialogOpen'].Value;
    Self.MainForm.DarwinDialogSaveNameField     := LanguageData.P['LauncherText.MainFormText.DarwinDialogSaveNameField'].Value;
    Self.MainForm.DarwinDialogSave              := LanguageData.P['LauncherText.MainFormText.DarwinDialogSave'].Value;
    Self.MainForm.OutputModulePreset            := LanguageData.P['LauncherText.MainFormText.OutputModulePreset'].Value;
    Self.MainForm.Properties                    := LanguageData.P['LauncherText.MainFormText.Properties'].Value;
    Self.MainForm.MissingFiles                  := LanguageData.P['LauncherText.MainFormText.MissingFiles'].Value;
    Self.MainForm.Sound                         := LanguageData.P['LauncherText.MainFormText.Sound'].Value;
    Self.MainForm.Threaded                      := LanguageData.P['LauncherText.MainFormText.Threaded'].Value;
    Self.MainForm.Custom                        := LanguageData.P['LauncherText.MainFormText.Custom'].Value;
    Self.MainForm.CustomPropHint                := LanguageData.P['LauncherText.MainFormText.CustomPropHint'].Value;
    Self.MainForm.CacheUsageLimit               := LanguageData.P['LauncherText.MainFormText.CacheUsageLimit'].Value;
    Self.MainForm.MemUsage                      := LanguageData.P['LauncherText.MainFormText.MemUsage'].Value;
    Self.MainForm.Unlimited                     := LanguageData.P['LauncherText.MainFormText.Unlimited'].Value;
    Self.MainForm.SingleComp                    := LanguageData.P['LauncherText.MainFormText.SingleComp'].Value;
    Self.MainForm.MultiComp                     := LanguageData.P['LauncherText.MainFormText.MultiComp'].Value;
    Self.MainForm.SingleRener                   := LanguageData.P['LauncherText.MainFormText.SingleRener'].Value;
    Self.MainForm.SplitRender                   := LanguageData.P['LauncherText.MainFormText.SplitRender'].Value;
    Self.MainForm.CompNameHint                  := LanguageData.P['LauncherText.MainFormText.CompNameHint'].Value;
    Self.MainForm.MultiCompGridHeader           := LanguageData.P['LauncherText.MainFormText.MultiCompGridHeader'].Value;
    Self.MainForm.StartFrame                    := LanguageData.P['LauncherText.MainFormText.StartFrame'].Value;
    Self.MainForm.EndFrame                      := LanguageData.P['LauncherText.MainFormText.EndFrame'].Value;
    Self.MainForm.EndFrameHint                  := LanguageData.P['LauncherText.MainFormText.EndFrameHint'].Value;
    Self.MainForm.Calculate                     := LanguageData.P['LauncherText.MainFormText.Calculate'].Value;
    Self.MainForm.Launch                        := LanguageData.P['LauncherText.MainFormText.Launch'].Value;
    Self.MainForm.NewVersionAvailable           := LanguageData.P['LauncherText.MainFormText.NewVersionAvailable'].Value;
    Self.MainForm.Download                      := LanguageData.P['LauncherText.MainFormText.Download'].Value;
    {$ENDREGION}

    {$REGION '    Settings Form Text    '}
    Self.SettingsForm.LauncherSettings          := LanguageData.P['LauncherText.SettingsFormText.LauncherSettings'].Value;
    Self.SettingsForm.RenderEnginePath          := LanguageData.P['LauncherText.SettingsFormText.RenderEnginePath'].Value;
    Self.SettingsForm.Style                     := LanguageData.P['LauncherText.SettingsFormText.Style'].Value;
    Self.SettingsForm.DefaultProjectsDirectory  := LanguageData.P['LauncherText.SettingsFormText.DefaultProjectsDirectory'].Value;
    Self.SettingsForm.DefaultOutputDirectory    := LanguageData.P['LauncherText.SettingsFormText.DefaultOutputDirectory'].Value;
    Self.SettingsForm.OnRenderStart             := LanguageData.P['LauncherText.SettingsFormText.OnRenderStart'].Value;
    Self.SettingsForm.HandleAerender            := LanguageData.P['LauncherText.SettingsFormText.HandleAerender'].Value;
    Self.SettingsForm.DeleteTemporary           := LanguageData.P['LauncherText.SettingsFormText.DeleteTemporary'].Value;
    {$ENDREGION}

    {$REGION '    Import Form Text    '}
    Self.ImportForm.ImportAEProject             := LanguageData.P['LauncherText.ImportFormText.ImportAEProject'].Value;
    Self.ImportForm.ProjectPath                 := LanguageData.P['LauncherText.ImportFormText.ProjectPath'].Value;
    Self.ImportForm.Compositions                := LanguageData.P['LauncherText.ImportFormText.Compositions'].Value;
    Self.ImportForm.CompositonProperties        := LanguageData.P['LauncherText.ImportFormText.CompositionProperties'].Value;
    Self.ImportForm.Name                        := LanguageData.P['LauncherText.ImportFormText.Name'].Value;
    Self.ImportForm.Resolution                  := LanguageData.P['LauncherText.ImportFormText.Resolution'].Value;
    Self.ImportForm.Framerate                   := LanguageData.P['LauncherText.ImportFormText.Framerate'].Value;
    Self.ImportForm.RangeStart                  := LanguageData.P['LauncherText.ImportFormText.RangeStart'].Value;
    Self.ImportForm.RangeEnd                    := LanguageData.P['LauncherText.ImportFormText.RangeEnd'].Value;
    Self.ImportForm.ImportProperties            := LanguageData.P['LauncherText.ImportFormText.ImportProperties'].Value;
    Self.ImportForm.PrepareForSplitRendering    := LanguageData.P['LauncherText.ImportFormText.PrepareForSplitRendering'].Value;
    Self.ImportForm.SelectAll                   := LanguageData.P['LauncherText.ImportFormText.SelectAll'].Value;
    Self.ImportForm.DeselectAll                 := LanguageData.P['LauncherText.ImportFormText.DeselectAll'].Value;
    Self.ImportForm.Import                      := LanguageData.P['LauncherText.ImportFormText.Import'].Value;
    {$ENDREGION}

    {$REGION '    Output Module Configurator Form Text    '}
    Self.OutputModuleConfiguratorForm.OutputModulePresetConfigurator := LanguageData.P['LauncherText.OutputModuleFormText.OutputModulePresetConfigurator'].Value;
    Self.OutputModuleConfiguratorForm.PresetName                    := LanguageData.P['LauncherText.OutputModuleFormText.PresetName'].Value;
    Self.OutputModuleConfiguratorForm.OutputModule                  := LanguageData.P['LauncherText.OutputModuleFormText.OutputModule'].Value;
    Self.OutputModuleConfiguratorForm.OutputFileNameStructure       := LanguageData.P['LauncherText.OutputModuleFormText.OutputFileNameStructure'].Value;
    Self.OutputModuleConfiguratorForm.ProjectTab                    := LanguageData.P['LauncherText.OutputModuleFormText.ProjectTab'].Value;
    Self.OutputModuleConfiguratorForm.CompositionTab                := LanguageData.P['LauncherText.OutputModuleFormText.CompositionTab'].Value;
    Self.OutputModuleConfiguratorForm.CompositionTimeTab            := LanguageData.P['LauncherText.OutputModuleFormText.CompositionTimeTab'].Value;
    Self.OutputModuleConfiguratorForm.ImageTab                      := LanguageData.P['LauncherText.OutputModuleFormText.ImageTab'].Value;
    Self.OutputModuleConfiguratorForm.DateTab                       := LanguageData.P['LauncherText.OutputModuleFormText.DateTab'].Value;
    Self.OutputModuleConfiguratorForm.Save                          := LanguageData.P['LauncherText.OutputModuleFormText.Save'].Value;
    Self.OutputModuleConfiguratorForm.Cancel                        := LanguageData.P['LauncherText.OutputModuleFormText.Cancel'].Value;
    {$ENDREGION}

    {$REGION '    Rendering Form Text    '}
    Self.RenderingForm.RenderingProgress        := LanguageData.P['LauncherText.RenderingFormText.RenderingProgress'].Value;
    Self.RenderingForm.HandleDisabled           := LanguageData.P['LauncherText.RenderingFormText.HandleDisabled'].Value;
    Self.RenderingForm.QueueIsEmpty             := LanguageData.P['LauncherText.RenderingFormText.QueueIsEmpty'].Value;
    Self.RenderingForm.TotalProgress            := LanguageData.P['LauncherText.RenderingFormText.TotalProgress'].Value;
    Self.RenderingForm.TimeElapsed              := LanguageData.P['LauncherText.RenderingFormText.TimeElapsed'].Value;
    Self.RenderingForm.AbortRendering           := LanguageData.P['LauncherText.RenderingFormText.AbortRendering'].Value;
    {$ENDREGION}

    {$REGION '    About Form Text    '}
    Self.AboutForm.AErenderLauncher             := LanguageData.P['LauncherText.AboutFormText.AErenderLauncher'].Value;
    Self.AboutForm.CreatedBy                    := LanguageData.P['LauncherText.AboutFormText.CreatedBy'].Value;
    Self.AboutForm.Description                  := LanguageData.P['LauncherText.AboutFormText.Description'].Value;
    Self.AboutForm.FromRussiaWithLove           := LanguageData.P['LauncherText.AboutFormText.FromRussiaWithLove'].Value;
    Self.AboutForm.FFMPEG                       := LanguageData.P['LauncherText.AboutFormText.FFMPEG'].Value;
    Self.AboutForm.FFMPEGNotFound               := LanguageData.P['LauncherText.AboutFormText.FFMPEGNotFound'].Value;
    Self.AboutForm.Copyright                    := LanguageData.P['LauncherText.AboutFormText.Copyright'].Value;
    {$ENDREGION}

    Self.Result := 0;
  except
    on Exception do
      Self.Result := -1;
  end;
end;

end.