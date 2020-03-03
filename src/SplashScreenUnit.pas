unit SplashScreenUnit;

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
  FMX.Effects;

type
  TSplashScreenForm = class(TForm)
    Image1: TImage;
    GridPanelLayout1: TGridPanelLayout;
    Label1: TLabel;
    Rectangle1: TRectangle;
    Layout1: TLayout;
    ShadowEffect1: TShadowEffect;
    //StartupTimer: TTimer;
    {procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure StartupTimerTimer(Sender: TObject); }
  private
    //FInitialized: Boolean;
    //procedure LoadMainForm;
  public
    { Public declarations }
  end;

var
  SplashScreenForm: TSplashScreenForm;

implementation

uses
  {$REGION '    AErenderLauncher Liraries    '}
  MainUnit,
  SettingsUnit,
  HelpUnit,
  ImportUnit,
  AboutUnit,
  FFMPEGUnit,
  RenderingUnit,
  OutputModuleEditorUnit;
  {$ENDREGION}

{$R *.fmx}

{procedure TSplashScreenForm.FormCreate(Sender: TObject);
begin
  StartupTimer.Enabled := False;
  StartupTimer.Interval := 500; // can be changed to improve startup speed in later releases
end;

procedure TSplashScreenForm.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  StartupTimer.Enabled := not FInitialized;
end;

procedure TSplashScreenForm.StartupTimerTimer(Sender: TObject);
begin
  StartupTimer.Enabled := False;
  if not FInitialized then begin
    FInitialized := true;
    LoadMainForm;
  end;
end;

procedure TSplashScreenForm.LoadMainForm;
var
  MainForm: TMainForm;
begin
  MainForm := TMainForm.Create(Application);
  MainForm.Show;
  Application.MainForm := MainForm;
  Close;
end;
}
end.
