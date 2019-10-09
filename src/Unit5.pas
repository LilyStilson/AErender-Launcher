unit Unit5;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  {$IFDEF MSWINDOWS}
    Winapi.ShellAPI, Winapi.Windows;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    Posix.Stdlib, Posix.Unistd, Posix.SysSysctl, Posix.SysTypes;
  {$ENDIF MACOS}

type
  TForm5 = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure Label7Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses
  Unit1;

{$R *.fmx}


procedure TForm5.FormShow(Sender: TObject);
begin
  if Unit1.FFMPEG then
    begin
      Label8.FontColor := $FF1E90FF;
      Label8.Text := 'FFMPEG: Found at ' + Unit1.ffmpegPath;
    end
  else
    begin
      Label8.FontColor := $FFDDDDDD;
      Label8.Text := 'FFMPEG: Undetected. Feature will be disabled.';
    end;
end;

procedure TForm5.Label7Click(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PWideChar('http://aerenderlauncher.com'), nil, nil, SW_SHOW);
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    _system(PAnsiChar('open ' + AnsiString('"' + 'http://aerenderlauncher.com' + '"')));
  {$ENDIF MACOS}
end;

end.
