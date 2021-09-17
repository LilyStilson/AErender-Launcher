unit AErenderLauncher.Types;

interface

uses
  System.Generics.Collections, AErenderLauncher.Rendering;

type
  OutputModule = record
    Module,
    Mask: String;
    Imported: Boolean;
  end;

  RenderSetting = record
    Setting: String;
    Imported: Boolean;
  end;

  TSettings = record
    Language: Integer;
    Style: Integer;
    AErenderPath: String;
    OnRenderStart: Integer;
    DefaultProjectPath: String;
    DefaultOutputPath: String;
    AErenderHandle: Boolean;
    DeleteTemporary: Boolean;

    RenderTasks: TList<TRenderTask>;

    LastProjectPath: String;
    LastOutputPath: String;
    TemporarySavePath: String;

    MissingFiles: Boolean;
    Sound: Boolean;
    Multithreaded: Boolean;
    CustomProperties: String;
    MemoryLimit: Single;
    CacheLimit: Single;

    OutputModules: TArray<OutputModule>;
    RenderSettings: TArray<RenderSetting>;

    RecentProjects: TArray<String>;

    constructor LoadFromXMLFile(const APath: String; const Skip: Boolean);
    function SaveToFile(const APath: String): Boolean;
  end;



implementation

constructor TSettings.LoadFromXMLFile(const APath: string; const Skip: Boolean);
begin
  //
end;

function TSettings.SaveToFile(const APath: string): Boolean;
begin

end;

end.
