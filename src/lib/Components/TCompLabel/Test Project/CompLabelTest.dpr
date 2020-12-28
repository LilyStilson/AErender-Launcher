program CompLabelTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  CompLabelUnit in 'CompLabelUnit.pas' {Form1},
  CompLabelDesign in 'CompLabelDesign.pas' {Form2},
  FMX.CompLabel in '..\Package\FMX.CompLabel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
