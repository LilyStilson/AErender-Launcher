unit CompLabelUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.CompLabel, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    CompLabel1: TCompLabel;
    Label1: TLabel;
    StyleBook1: TStyleBook;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if ObjectAtPoint(TPointF.Create(X, Y)) <> nil then
    Label1.Text := Format('X = %n; Y = %n; Class = %s', [X, Y, ObjectAtPoint(TPointF.Create(X, Y)).GetObject.ClassName]);
end;

end.
