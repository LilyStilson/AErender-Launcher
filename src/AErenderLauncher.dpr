program AErenderLauncher;





uses
  System.StartUpCopy,
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  SettingsUnit in 'SettingsUnit.pas' {SettingsForm},
  HelpUnit in 'HelpUnit.pas' {HelpForm},
  ImportUnit in 'ImportUnit.pas' {ImportForm},
  AboutUnit in 'AboutUnit.pas' {AboutForm},
  FFMPEGUnit in 'FFMPEGUnit.pas' {FFMPEGForm},
  RenderingUnit in 'RenderingUnit.pas' {RenderingForm},
  MathExpParser in 'MathExpParser.pas',
  OutputModuleEditorUnit in 'OutputModuleEditorUnit.pas' {OutputModuleEditorForm},
  AErenderDataParser in '..\..\AErender Parser\AErenderDataParser.pas',
  AErenderLauncherLocalization in 'AErenderLauncherLocalization.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.CreateForm(TImportForm, ImportForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TFFMPEGForm, FFMPEGForm);
  Application.CreateForm(TRenderingForm, RenderingForm);
  Application.CreateForm(TOutputModuleEditorForm, OutputModuleEditorForm);
  Application.Run;
end.
