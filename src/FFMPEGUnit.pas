unit FFMPEGUnit;

(*        AErender Launcher                                                                 *)
(*        FFMPEGUnit.pas                                                                    *)
(*        Lily Stilson // 2019 - 2021                                                       *)
(*        MIT License                                                                       *)
(*                                                                                          *)
(*        Copyright (c) 2019 - 2021 Alice Romanets                                          *)
(*                                                                                          *)
(*        Permission is hereby granted, free of charge, to any person obtaining a copy      *)
(*        of this software and associated documentation files (the "Software"), to deal     *)
(*        in the Software without restriction, including without limitation the rights      *)
(*        to use, copy, modify, merge, publish, distribute, sublicense, and/or sell         *)
(*        copies of the Software, and to permit persons to whom the Software is             *)
(*        furnished to do so, subject to the following conditions:                          *)
(*                                                                                          *)
(*        The above copyright notice and this permission notice shall be included in all    *)
(*        copies or substantial portions of the Software.                                   *)
(*                                                                                          *)
(*        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR        *)
(*        IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,          *)
(*        FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE       *)
(*        AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER            *)
(*        LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,     *)
(*        OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE     *)
(*        SOFTWARE.                                                                         *)

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,
  System.Bindings.Outputs,

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
  Fmx.Bind.Editors,
  Fmx.Bind.DBEngExt,

  Data.Bind.EngExt,
  Data.Bind.Components;

type
  TFFMPEGForm = class(TForm)
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
  FFMPEGForm: TFFMPEGForm;
  FFMPEG, OutputFile: String;

implementation

uses
  {$REGION '    AErenderLauncher Liraries    '}
  MainUnit,
  SettingsUnit;
  {$ENDREGION}

{$R *.fmx}

procedure TFFMPEGForm.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
      0: outputPathEdit.Text := outputPathEdit.Text.Remove(outputPathEdit.Text.Length - 4) + '.avi';
      1: outputPathEdit.Text := outputPathEdit.Text.Remove(outputPathEdit.Text.Length - 4) + '.mov';
      2: outputPathEdit.Text := outputPathEdit.Text.Remove(outputPathEdit.Text.Length - 4) + '.mp4';
      3: outputPathEdit.Text := outputPathEdit.Text.Remove(outputPathEdit.Text.Length - 4) + '.mp4';
    end;
end;

procedure TFFMPEGForm.defaultOutputCheckBoxChange(Sender: TObject);
begin
  if defaultOutputCheckBox.IsChecked then
    case ComboBox1.ItemIndex of
      0: outputPathEdit.Text := MainForm.outputPath.Text.Remove(MainForm.outputPath.Text.Length - 4) + '.avi';
      1: outputPathEdit.Text := MainForm.outputPath.Text.Remove(MainForm.outputPath.Text.Length - 4) + '.mov';
      2: outputPathEdit.Text := MainForm.outputPath.Text.Remove(MainForm.outputPath.Text.Length - 4) + '.mp4';
      3: outputPathEdit.Text := MainForm.outputPath.Text.Remove(MainForm.outputPath.Text.Length - 4) + '.mp4';
    end;
end;

end.
