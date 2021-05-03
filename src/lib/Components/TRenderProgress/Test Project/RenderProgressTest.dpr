program RenderProgressTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.RenderProgress in '..\Package\FMX.RenderProgress.pas',
  RenderProgressTestUnit in 'RenderProgressTestUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
