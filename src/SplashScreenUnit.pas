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
  FMX.Effects,

  {$IFDEF MSWINDOWS}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.DwmApi,
  WinApi.UxTheme,
  FMX.Platform.Win;
  {$ENDIF MSWINDOWS}

  {$IFDEF MACOS}
  MacApi.Foundation,
  MacApi.AppKit,
  MacApi.ObjectiveC,
  MacApi.CocoaTypes,
  FMX.Platform.Mac;
  {$ENDIF MACOS}

type
  TSplashScreenForm = class(TForm)
    Image1: TImage;
    GridPanelLayout1: TGridPanelLayout;
    Rectangle1: TRectangle;
    Layout1: TLayout;
    Path1: TPath;
    Path2: TPath;
    procedure FormCreate(Sender: TObject);
  private
    {$IFDEF MSWINDOWS}procedure WMNCPaint(var AMessage: TMessage); message WM_NCPAINT;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;

const
  UseShadow: Boolean = True;

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

{$IFDEF MSWINDOWS}
procedure TSplashScreenForm.WMNCPaint(var AMessage: TMessage);
var
  v: PInteger;
  m: _MARGINS;
  handle: HWND;
begin
  if UseShadow then begin
    New(v);
    v^ := 2;

    handle := FormToHWND(Self);

    DwmSetWindowAttribute(handle, 2, v, 4);

    m.cxLeftWidth := 0;
    m.cxRightWidth := 0;
    m.cyTopHeight := 0;
    m.cyBottomHeight := 1;

    DwmExtendFrameIntoClientArea(handle, m);
  end;

end;
{$ENDIF MSWINDOWS}

procedure TSplashScreenForm.FormCreate(Sender: TObject);
begin
  Self.Height := 100;
  Self.Width := 500;

  {$IFDEF MACOS}
  Self.Transparency := True;

  var Window: NSWindow := WindowHandleToPlatform(Self.Handle).Wnd;
  Window.setHasShadow(True);

  {var aView: NSView := TNSView.Wrap(TNSView.OCClass.alloc);
  aView.setWantsLayer(True);
  aView.layer.setFrame(aView.frame);
  aView.layer.setCornerRadius(3);
  aView.layer.setMasksToBounds(True);

  var dropShadow: NSShadow := TNSShadow.Wrap(TNSShadow.OCClass.alloc);
  dropShadow.setShadowColor(TNSColor.Wrap(TNSColor.OCClass.blackColor));
  dropShadow.setShadowBlurRadius(10);

  aView.setShadow(dropShadow);

  Window.setOpaque(False);
  Window.setBackgroundColor(TNSColor.Wrap(TNSColor.OCClass.clearColor));
  Window.setContentView(aView);
  Window.makeKeyAndOrderFront(nil);
  Window.setLevel(NSStatusWindowLevel);}

  Window.setOpaque(True);
  //Window.setStyleMask(NSResizableWindowMask);
  Window.setShowsToolbarButton(False);

  Rectangle1.XRadius := 3;
  Rectangle1.YRadius := 3;

  {$ENDIF MACOS}

  {$IFDEF MSWINDOWS}
  if UseShadow then
    DwmCompositionEnabled();
  {$ENDIF MSWINDOWS}
end;

end.
