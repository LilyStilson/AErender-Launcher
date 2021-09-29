unit AErenderLauncher.Types;

interface

uses
  System.SysUtils, System.Generics.Collections, AErenderLauncher.Rendering,
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.adomxmldom,
  Xml.XMLDoc,
  Xml.omnixmldom;

type
  AERSettingsException = class(Exception);

  OutputModule = record
    Module,
    Mask: String;
    Imported: Boolean;
    class operator Equal(a: OutputModule; b: OutputModule): Boolean;
    class operator NotEqual(a: OutputModule; b: OutputModule): Boolean;
  end;

  TSettings = record
  private
    procedure InitOutputModules;
  public
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
    CustomPropertiesEnabled: Boolean;
    CustomProperties: String;
    MemoryLimit: Single;
    CacheLimit: Single;

    SelectedOutputModule: Integer;
    OutputModules: TArray<OutputModule>;
    RenderSettings: String;

    RecentProjects: array [0..9] of String;
    constructor LoadFromXMLFile(const APath: String; const Skip: Boolean = True);
    constructor LoadFromXMLFileLegacy(const APath: String);
    procedure SaveToFile(const APath: String);
    procedure Reset;
    procedure ResetAndSave(const APath: String);
  end;

const
  PLATFORMPATHSEPARATOR = {$IFDEF MSWINDOWS}'\'{$ELSE MACOS}'/'{$ENDIF};

var
  APPFOLDER: String;
  Settings: TSettings;

implementation

{$REGION '    OutputModule    '}

class operator OutputModule.Equal(a: OutputModule; b: OutputModule): Boolean;
begin
  Result := (a.Module = b.Module) and (a.Mask = b.Mask);
end;

class operator OutputModule.NotEqual(a: OutputModule; b: OutputModule): Boolean;
begin
  Result := (a.Module <> b.Module) or (a.Mask <> b.Mask);
end;

{$ENDREGION}

{$REGION '    TSettings    '}

procedure TSettings.InitOutputModules;
begin
  SetLength(Self.OutputModules, 10);

  Self.OutputModules[0].Module := 'Lossless';
  Self.OutputModules[0].Mask := '[compName].[fileExtension]';
  Self.OutputModules[0].Imported := False;

  Self.OutputModules[1].Module := 'AIFF 48kHz';
  Self.OutputModules[1].Mask := '[compName].[fileExtension]';
  Self.OutputModules[1].Imported := False;

  Self.OutputModules[2].Module := 'Alpha Only';
  Self.OutputModules[2].Mask := '[compName].[fileExtension]';
  Self.OutputModules[2].Imported := False;

  Self.OutputModules[3].Module := 'AVI DV NTSC 48kHz';
  Self.OutputModules[3].Mask := '[compName].[fileExtension]';
  Self.OutputModules[3].Imported := False;

  Self.OutputModules[4].Module := 'AVI DV PAL 48kHz';
  Self.OutputModules[4].Mask := '[compName].[fileExtension]';
  Self.OutputModules[4].Imported := False;

  Self.OutputModules[5].Module := 'Lossless with Alpha';
  Self.OutputModules[5].Mask := '[compName].[fileExtension]';
  Self.OutputModules[5].Imported := False;

  Self.OutputModules[6].Module := 'Multi-Machine Sequence';
  Self.OutputModules[6].Mask := '[compName]_[#####].[fileExtension]';
  Self.OutputModules[6].Imported := False;

  Self.OutputModules[7].Module := 'Photoshop';
  Self.OutputModules[7].Mask := '[compName]_[#####].[fileExtension]';
  Self.OutputModules[7].Imported := False;

  Self.OutputModules[8].Module := 'Save Current Preview';
  Self.OutputModules[8].Mask := '[compName].[fileExtension]';
  Self.OutputModules[8].Imported := False;

  Self.OutputModules[9].Module := 'TIFF Sequence with Alpha';
  Self.OutputModules[9].Mask := '[compName]_[#####].[fileExtension]';
  Self.OutputModules[9].Imported := False;
end;

constructor TSettings.LoadFromXMLFile(const APath: String; const Skip: Boolean = True);
var
  Config: IXMLDocument;
  RootNode: IXMLNode;
begin
  Config := TXMLDocument.Create(nil);
  Config.LoadFromFile(APath);
  Config.Active := True;

  RootNode := Config.DocumentElement;

  Self.Language           := RootNode.ChildNodes['Language'].Text.ToInteger;
  Self.Style              := RootNode.ChildNodes['Style'].Text.ToInteger;
  Self.AErenderPath       := RootNode.ChildNodes['AErenderPath'].Text;
  Self.OnRenderStart      := RootNode.ChildNodes['OnRenderStart'].Text.ToInteger;
  Self.DefaultProjectPath := RootNode.ChildNodes['DefaultProjectPath'].Text;
  Self.DefaultOutputPath  := RootNode.ChildNodes['DefaultOutputPath'].Text;
  Self.AErenderHandle     := RootNode.ChildNodes['AErenderHandle'].Text.ToBoolean;
  Self.DeleteTemporary    := RootNode.ChildNodes['DeleteTemporary'].Text.ToBoolean;

  Self.LastProjectPath    := RootNode.ChildNodes['LastProjectPath'].Text;
  Self.LastOutputPath     := RootNode.ChildNodes['LastOutputPath'].Text;
  Self.TemporarySavePath  := RootNode.ChildNodes['TemporarySavePath'].Text;

  Self.MissingFiles       := RootNode.ChildNodes['MissingFiles'].Text.ToBoolean;
  Self.Sound              := RootNode.ChildNodes['Sound'].Text.ToBoolean;
  Self.Multithreaded      := RootNode.ChildNodes['Multithreaded'].Text.ToBoolean;
  Self.CustomPropertiesEnabled := StrToBool(RootNode.ChildNodes['CustomProperties'].Attributes['Enabled']);
  Self.CustomProperties   := RootNode.ChildNodes['CustomProperties'].Text;
  Self.MemoryLimit        := RootNode.ChildNodes['MemoryLimit'].Text.ToSingle;
  Self.CacheLimit         := RootNode.ChildNodes['CacheLimit'].Text.ToSingle;

  if RootNode.ChildNodes['OutputModules'].ChildNodes.Count <> 0 then begin
    SetLength(Self.OutputModules, RootNode.ChildNodes['OutputModules'].ChildNodes.Count);
    for var i := 0 to High(Self.OutputModules) do begin
      Self.OutputModules[i].Module := RootNode.ChildNodes['OutputModules'].ChildNodes[i].ChildNodes['Module'].Text;
      Self.OutputModules[i].Mask := RootNode.ChildNodes['OutputModules'].ChildNodes[i].ChildNodes['Mask'].Text;
      Self.OutputModules[i].Imported := StrToBool(RootNode.ChildNodes['OutputModules'].ChildNodes[i].Attributes['Imported']);
    end;
    Self.SelectedOutputModule := StrToInt(RootNode.ChildNodes['OutputModules'].Attributes['Selected']);
  end else
    Self.InitOutputModules;

  Self.RenderSettings := RootNode.ChildNodes['RenderSettings'].Text;

  for var i := 0 to 9 do begin
    Self.RecentProjects[i] := RootNode.ChildNodes['RecentProjects'].ChildNodes[i].Text;
  end;

end;

constructor TSettings.LoadFromXMLFileLegacy(const APath: String);
var
  Config: IXMLDocument;
  RootNode: IXMLNode;
begin
  Config := TXMLDocument.Create(nil);
  Config.LoadFromFile(APath);
  Config.Active := True;

  RootNode := Config.DocumentElement;

  Self.Language           := RootNode.ChildNodes['lang'].Text.ToInteger;
  Self.Style              := RootNode.ChildNodes['style'].Text.ToInteger;
  Self.AErenderPath       := RootNode.ChildNodes['aerender'].Text;
  Self.OnRenderStart      := RootNode.ChildNodes['onRenderStart'].Text.ToInteger;
  Self.DefaultProjectPath := RootNode.ChildNodes['defprgpath'].Text;
  Self.DefaultOutputPath  := RootNode.ChildNodes['defoutpath'].Text;
  Self.AErenderHandle     := RootNode.ChildNodes['handle'].Text.ToBoolean;
  Self.DeleteTemporary    := RootNode.ChildNodes['delTempFiles'].Text.ToBoolean;

  Self.LastProjectPath    := RootNode.ChildNodes['projectPath'].Text;
  Self.LastOutputPath     := RootNode.ChildNodes['outputPath'].Text;
  Self.TemporarySavePath  := RootNode.ChildNodes['tempSavePath'].Text;

  Self.MissingFiles       := RootNode.ChildNodes['missingFiles'].Text.ToBoolean;
  Self.Sound              := RootNode.ChildNodes['sound'].Text.ToBoolean;
  Self.Multithreaded      := RootNode.ChildNodes['thread'].Text.ToBoolean;
  Self.CustomPropertiesEnabled := StrToBool(RootNode.ChildNodes['prop'].Attributes['enabled']);
  Self.CustomProperties   := RootNode.ChildNodes['prop'].Text;
  Self.MemoryLimit        := RootNode.ChildNodes['memoryLimit'].Text.ToSingle;
  Self.CacheLimit         := RootNode.ChildNodes['cacheLimit'].Text.ToSingle;

  if RootNode.ChildNodes['outputModule'].ChildNodes.Count <> 0 then begin
    SetLength(Self.OutputModules, RootNode.ChildNodes['outputModule'].ChildNodes.Count);
    for var i := 0 to High(Self.OutputModules) do begin
      Self.OutputModules[i].Module := RootNode.ChildNodes['outputModule'].ChildNodes[i].ChildNodes['moduleName'].Text;
      Self.OutputModules[i].Mask := RootNode.ChildNodes['outputModule'].ChildNodes[i].ChildNodes['filemask'].Text;
      Self.OutputModules[i].Imported := StrToBool(RootNode.ChildNodes['outputModule'].ChildNodes[i].Attributes['imported']);
    end;
    Self.SelectedOutputModule := StrToInt(RootNode.ChildNodes['outputModule'].Attributes['selected']);
  end else
    Self.InitOutputModules;

  Self.RenderSettings := RootNode.ChildNodes['renderSettings'].Text;

  for var i := 0 to 9 do begin
    Self.RecentProjects[i] := RootNode.ChildNodes['recentProjects'].ChildNodes[i].Text;
  end;
end;

procedure TSettings.SaveToFile(const APath: string);
var
  Config: IXMLDocument;
  RootNode: IXMLNode;
begin
  Config := TXMLDocument.Create(nil);
  Config.Active := True;
  Config.Encoding := 'utf-8';
  Config.Options := [doNodeAutoIndent];

  RootNode := Config.AddChild('LauncherConfig');

  RootNode.AddChild('Language').Text := Self.Language.ToString;
  RootNode.AddChild('Style').Text := Self.Style.ToString;
  RootNode.AddChild('AErenderPath').Text := Self.AErenderPath;
  RootNode.AddChild('OnRenderStart').Text := Self.OnRenderStart.ToString;
  RootNode.AddChild('DefaultProjectPath').Text := Self.DefaultProjectPath;
  RootNode.AddChild('DefaultOutputPath').Text := Self.DefaultOutputPath;
  RootNode.AddChild('AErenderHandle').Text := BoolToStr(Self.AErenderHandle, True);
  RootNode.AddChild('DeleteTemporary').Text := BoolToStr(Self.DeleteTemporary, True);

  RootNode.AddChild('LastProjectPath').Text := Self.LastProjectPath;
  RootNode.AddChild('LastOutputPath').Text := Self.LastOutputPath;
  RootNode.AddChild('TemporarySavePath').Text := Self.TemporarySavePath;

  RootNode.AddChild('MissingFiles').Text := BoolToStr(Self.MissingFiles, True);
  RootNode.AddChild('Sound').Text := BoolToStr(Self.Sound, True);
  RootNode.AddChild('Multithreaded').Text := BoolToStr(Self.Multithreaded, True);
  RootNode.AddChild('CustomProperties').Text := Self.CustomProperties;
  RootNode.ChildNodes['CustomProperties'].Attributes['Enabled'] := BoolToStr(Self.CustomPropertiesEnabled, True);

  RootNode.AddChild('MemoryLimit').Text := Self.MemoryLimit.ToString;
  RootNode.AddChild('CacheLimit').Text := Self.CacheLimit.ToString;

  var ChildNode: IXMLNode := RootNode.AddChild('OutputModules');
  ChildNode.Attributes['Selected'] := Self.SelectedOutputModule;
  for var i := 0 to High(OutputModules) do begin
    var ModuleNode: IXMLNode := RootNode.ChildNodes['OutputModules'].AddChild('OutputModule');
    ModuleNode.Attributes['Imported'] := BoolToStr(OutputModules[i].Imported, True);
    ModuleNode.AddChild('Module').Text := OutputModules[i].Module;
    ModuleNode.AddChild('Mask').Text := OutputModules[i].Mask;
  end;

  ChildNode := RootNode.AddChild('RenderSettings');
  ChildNode.Text := Self.RenderSettings;

  ChildNode := RootNode.AddChild('RecentProjects');
  for var i := 0 to 9 do begin
    if Self.RecentProjects[i] = '' then
      ChildNode.AddChild('Project').Text := '(empty)'
    else
      ChildNode.AddChild('Project').Text := Self.RecentProjects[i]
  end;

  Config.SaveToFile(APath);
end;

procedure TSettings.Reset;
begin
  Self.InitOutputModules;

  Self.Language           := 0;
  Self.Style              := 0;
  Self.AErenderPath       := '';
  Self.OnRenderStart      := 0;
  Self.DefaultProjectPath := '';
  Self.DefaultOutputPath  := '';
  Self.AErenderHandle     := True;
  Self.DeleteTemporary    := True;

  Self.LastProjectPath    := '';
  Self.LastOutputPath     := '';
  Self.TemporarySavePath  := '';

  Self.MissingFiles       := False;
  Self.Sound              := True;
  Self.Multithreaded      := False;
  Self.CustomPropertiesEnabled := False;
  Self.CustomProperties   := '';
  Self.MemoryLimit        := 100;
  Self.CacheLimit         := 100;

  Self.RenderSettings := 'Best Settings';

  for var i := 0 to 9 do begin
    Self.RecentProjects[i] := '(empty)';
  end;
end;

procedure TSettings.ResetAndSave(const APath: string);
begin
  Self.Reset;
  Self.SaveToFile(APath);
end;

{$ENDREGION}

initialization
  APPFOLDER := {$IFDEF MSWINDOWS}'C:\ProgramData\AErender'{$ELSE MACOS}GetEnvironmentVariable('HOME') + '/Documents/AErender/'{$ENDIF};

end.
