unit AboutUnit;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Ani,
  FMX.Effects,
  {$IFDEF MSWINDOWS}
    FMX.Platform.Win, Winapi.ShellAPI, Winapi.Windows;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    Posix.Stdlib, Posix.Unistd, Posix.SysSysctl, Posix.SysTypes;
  {$ENDIF MACOS}

type
  TAboutForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    GridPanelLayout1: TGridPanelLayout;
    GridPanelLayout2: TGridPanelLayout;
    LogoRotationAnim: TFloatAnimation;
    procedure Label7Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure LogoRotationAnimFinish(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  {$REGION '    AErenderLauncher Liraries    '}
  MainUnit;
  {$ENDREGION}

{$R *.fmx}

{$IFDEF MSWINDOWS}
procedure TAboutForm.CreateHandle;
begin
  inherited CreateHandle;

  var hWnd: HWND := FormToHWND(Self);

  SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);
end;
{$ENDIF MSWINDOWS}

procedure TAboutForm.FormShow(Sender: TObject);
begin
  Label1.Text := 'AErender Launcher (' + APPVERSION + ')';
  if MainUnit.FFMPEG then
    begin
      Label8.FontColor := $FF1E90FF;
      Label8.Text := 'FFMPEG: Found at ' + MainUnit.ffmpegPath;
    end
  else
    begin
      Label8.FontColor := $FFDDDDDD;
      Label8.Text := 'FFMPEG: Undetected. Feature will be disabled.';
    end;
end;

procedure TAboutForm.Image1DblClick(Sender: TObject);
begin
  LogoRotationAnim.Enabled := True;
end;

procedure TAboutForm.Label7Click(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PWideChar('http://aerenderlauncher.com'), nil, nil, SW_SHOW);
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    _system(PAnsiChar('open ' + AnsiString('"' + 'http://aerenderlauncher.com' + '"')));
  {$ENDIF MACOS}
end;

procedure TAboutForm.LogoRotationAnimFinish(Sender: TObject);
begin
  LogoRotationAnim.Enabled := False;
end;

end.
