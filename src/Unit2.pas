unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Edit, FMX.Platform;

type
  TForm2 = class(TForm)
    langLabel: TLabel;
    langBox: TComboBox;
    Button1: TButton;
    Layout1: TLayout;
    langLayout: TLayout;
    aerenderPathLayout: TLayout;
    Label1: TLabel;
    aerenderPath: TEdit;
    aerenderPathSelect: TButton;
    OpenDialog1: TOpenDialog;
    defaultProjectsLayout: TLayout;
    Label2: TLabel;
    defaultProjectsPath: TEdit;
    Button2: TButton;
    defaultOutputLayout: TLayout;
    Label3: TLabel;
    defaultOutputPath: TEdit;
    Button3: TButton;
    styleLayout: TLayout;
    styleBox: TComboBox;
    styleLabel: TLabel;
    onRenderStartLayout: TLayout;
    onRenderStartBox: TComboBox;
    onRenderStartLabel: TLabel;
    procedure langBoxChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aerenderPathSelectClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure styleBoxChange(Sender: TObject);
    procedure onRenderStartBoxChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Unit1, Unit3, Unit4, Unit5, Unit6, RenderingUnit;

procedure TForm2.aerenderPathSelectClick(Sender: TObject);
begin
  With OpenDialog1 do
    if Execute then
      begin
        aerenderPath.Text := OpenDialog1.FileName;
        AERPATH := OpenDialog1.FileName;
      end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  if not aerenderPath.Text.Contains('aerender') then
    if Unit1.LANG = 'EN' then
      MessageDlg(('Please specify valid Adobe After Effects render engine path!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
    else
      MessageDlg(('Пожалуйста, укажите путь к модулю рендеринга After Effects!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
  else
    begin
      Form2.Close;
      AERPATH := aerenderPath.Text;
      DEFPRGPATH := defaultProjectsPath.Text;
      DEFOUTPATH := defaultOutputPath.Text;
    end;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  PATH: String;
begin
  {$IFDEF MSWINDOWS}
    SelectDirectory ('Select Default Projects Directory', '%USERPROFILE%\Documents', PATH);
    defaultProjectsPath.Text := PATH;
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
    SelectDirectory ('Select Default Projects Directory', '~/Documents', PATH);
    defaultProjectsPath.Text := PATH;
  {$ENDIF POSIX}
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  PATH: String;
begin
  {$IFDEF MSWINDOWS}
    SelectDirectory ('Select Default Output Directory', '%USERPROFILE%\Documents', PATH);
    defaultOutputPath.Text := PATH;
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
    SelectDirectory ('Select Default Output Directory', '~/Documents', PATH);
    defaultOutputPath.Text := PATH;
  {$ENDIF POSX}
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not aerenderPath.Text.Contains('aerender') then
    begin
      if Unit1.LANG = 'EN' then
        MessageDlg(('Please specify the Adobe After Effects render engine path!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
      else
        MessageDlg(('Пожалуйста, укажите путь к модулю рендеринга After Effects!'), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      CanClose := False;
    end
  else
    begin
      CanClose := True;
      Form2.Close;
      AERPATH := aerenderPath.Text;
      DEFPRGPATH := defaultProjectsPath.Text;
      DEFOUTPATH := defaultOutputPath.Text;
    end;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  aerenderPath.Text := AERPATH;
  defaultProjectsPath.Text := DEFPRGPATH;
  defaultOutputPath.Text := DEFOUTPATH;
  if Unit1.LANG = 'EN' then
    langBox.ItemIndex := 0;
  if Unit1.LANG = 'RU' then
    langBox.ItemIndex := 1;
  {$IFDEF MSWINDOWS}
    aerenderPath.TextPrompt := 'C:\Program Files\Adobe\Adobe After Effects CC\Support Files\aerender.exe';
    OpenDialog1.Filter := 'After Effects Render Engine|aerender.exe';
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
    aerenderPath.TextPrompt := '/Applications/Adobe After Effects CC/aerender';
    OpenDialog1.Filter := 'After Effects Render Engine|aerender';
  {$ENDIF POSIX}
end;

procedure TForm2.langBoxChange(Sender: TObject);
begin
  case langBox.ItemIndex of
    0:begin
        Unit1.LANG := 'EN';
        Form1.Lang1.Lang := 'EN';
        if Form1.compSwitch.IsChecked then
          Form1.compSwitchLabel.Text := 'Multiple Compositions'
        else
          Form1.compSwitchLabel.Text := 'Single Composition';
        if Form1.threadsSwitch.IsChecked then
          Form1.threadsSwitchLabel.Text := 'Split Render'
        else
          Form1.threadsSwitchLabel.Text := 'Single Render';
      end;
    1:begin
        Unit1.LANG := 'RU';
        Form1.Lang1.Lang := 'RU';
        if Form1.compSwitch.IsChecked then
          Form1.compSwitchLabel.Text := 'Несколько композиций'
        else
          Form1.compSwitchLabel.Text := 'Одна композиция';
        if Form1.threadsSwitch.IsChecked then
          Form1.threadsSwitchLabel.Text := 'Рендерить частями'
        else
          Form1.threadsSwitchLabel.Text := 'Рендерить одним файлом';
      end;
  end;
end;

procedure TForm2.onRenderStartBoxChange(Sender: TObject);
begin
  Unit1.ONRENDERSTART := onRenderStartBox.ItemIndex;
end;

procedure TForm2.styleBoxChange(Sender: TObject);
begin
  case styleBox.ItemIndex of
    0:begin
        Unit1.STYLE := 0;
        Form1.StyleBook := Form1.AERModernStyle;
        Form1.settingsIconFill.Color := $FFFFFFFF;
        Form1.infoIconFill.Color := $FFFFFFFF;
        Form1.memUsageTrackBar.Margins.Top := 5;
        Form1.cacheUsageTrackBar.Margins.Top := 5;
        Form2.StyleBook := Form1.AERModernStyle;
        Form3.StyleBook := Form1.AERModernStyle;
        Form4.StyleBook := Form1.AERModernStyle;
        Form5.StyleBook := Form1.AERModernStyle;
        Form6.StyleBook := Form1.AERModernStyle;
        RenderingForm.StyleBook := Form1.AERModernStyle;
      end;
    1:begin
        Unit1.STYLE := 1;
        Form1.StyleBook := Form1.AERModernAnimatedStyle;
        Form1.settingsIconFill.Color := $FFFFFFFF;
        Form1.infoIconFill.Color := $FFFFFFFF;
        Form1.memUsageTrackBar.Margins.Top := 5;
        Form1.cacheUsageTrackBar.Margins.Top := 5;
        Form2.StyleBook := Form1.AERModernAnimatedStyle;
        Form3.StyleBook := Form1.AERModernAnimatedStyle;
        Form4.StyleBook := Form1.AERModernAnimatedStyle;
        Form5.StyleBook := Form1.AERModernAnimatedStyle;
        Form6.StyleBook := Form1.AERModernAnimatedStyle;
        RenderingForm.StyleBook := Form1.AERModernAnimatedStyle;
      end;
    2:begin
        Unit1.STYLE := 2;
        Form1.StyleBook := Form1.OnyxBlueStyle;
        Form1.settingsIconFill.Color := $FF000000;
        Form1.infoIconFill.Color := $FF000000;
        Form1.memUsageTrackBar.Margins.Top := 0;
        Form1.cacheUsageTrackBar.Margins.Top := 0;
        Form2.StyleBook := Form1.OnyxBlueStyle;
        Form3.StyleBook := Form1.OnyxBlueStyle;
        Form4.StyleBook := Form1.OnyxBlueStyle;
        Form5.StyleBook := Form1.OnyxBlueStyle;
        Form6.StyleBook := Form1.OnyxBlueStyle;
        RenderingForm.StyleBook := Form1.OnyxBlueStyle;
      end;
  end;
end;

end.
