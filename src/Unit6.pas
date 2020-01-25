unit Unit6;

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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Layouts,
  FMX.ListBox,
  FMX.TabControl,
  FMX.Effects,
  FMX.Objects,
  Data.Bind.EngExt,
  Fmx.Bind.DBEngExt,
  System.Rtti,
  System.Bindings.Outputs,
  Fmx.Bind.Editors,
  Data.Bind.Components;

type
  TForm6 = class(TForm)
    ffmpegConfigFormLabel: TLabel;
    outputFileLayout: TLayout;
    outputPathLabel: TLabel;
    outputPathEdit: TEdit;
    outputPathButton: TButton;
    defaultOutputCheckBox: TCheckBox;
    Layout1: TLayout;
    Label1: TLabel;
    styleBox: TComboBox;
    Layout2: TLayout;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    bitrateGroupBox: TGroupBox;
    TabControl1: TTabControl;
    videoTab: TTabItem;
    audioTab: TTabItem;
    memUsageInfo: TLabel;
    filesGroupBox: TGroupBox;
    Layout3: TLayout;
    Label4: TLabel;
    TrackBar2: TTrackBar;
    Label5: TLabel;
    Layout4: TLayout;
    Label6: TLabel;
    TrackBar3: TTrackBar;
    Label7: TLabel;
    Label8: TLabel;
    Layout5: TLayout;
    renderPresetGroupBox: TGroupBox;
    ComboBox1: TComboBox;
    BindingsList1: TBindingsList;
    LinkControlToPropertyEnabled: TLinkControlToProperty;
    LinkControlToPropertyEnabled2: TLinkControlToProperty;
    Layout6: TLayout;
    procedure defaultOutputCheckBoxChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  ffmpeg_lossless_avi = '';
  ffmpeg_lossless_mov = '';
  ffmpeg_highbitrate_mp4 = '';
  ffmpeg_medbitrate_mp4 = '';

var
  Form6: TForm6;
  FFMPEG, OutputFile: String;

implementation

uses
  Unit1, Unit2;

{$R *.fmx}

procedure TForm6.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
      0: outputPathEdit.Text := outputPathEdit.Text.Remove(outputPathEdit.Text.Length - 4) + '.avi';
      1: outputPathEdit.Text := outputPathEdit.Text.Remove(outputPathEdit.Text.Length - 4) + '.mov';
      2: outputPathEdit.Text := outputPathEdit.Text.Remove(outputPathEdit.Text.Length - 4) + '.mp4';
      3: outputPathEdit.Text := outputPathEdit.Text.Remove(outputPathEdit.Text.Length - 4) + '.mp4';
    end;
end;

procedure TForm6.defaultOutputCheckBoxChange(Sender: TObject);
begin
  if defaultOutputCheckBox.IsChecked then
    case ComboBox1.ItemIndex of
      0: outputPathEdit.Text := Form1.outputPath.Text.Remove(Form1.outputPath.Text.Length - 4) + '.avi';
      1: outputPathEdit.Text := Form1.outputPath.Text.Remove(Form1.outputPath.Text.Length - 4) + '.mov';
      2: outputPathEdit.Text := Form1.outputPath.Text.Remove(Form1.outputPath.Text.Length - 4) + '.mp4';
      3: outputPathEdit.Text := Form1.outputPath.Text.Remove(Form1.outputPath.Text.Length - 4) + '.mp4';
    end;
end;

end.
