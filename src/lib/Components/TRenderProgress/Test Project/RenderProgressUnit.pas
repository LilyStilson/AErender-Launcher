unit RenderProgressUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.RenderProgress, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    StyleBook2: TStyleBook;
    TrackBar1: TTrackBar;
    CheckBox1: TCheckBox;
    RenderProgress1: TRenderProgress;
    Button1: TButton;
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  RenderProgress1.Log.Add('String ' + Random(9999).ToString);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  RenderProgress1.IsError := CheckBox1.IsChecked;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  RenderProgress1.Value := TrackBar1.Value;
end;

end.
