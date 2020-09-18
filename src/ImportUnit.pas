﻿unit ImportUnit;

(*        AErender Launcher                                                                 *)
(*        ImportUnit.pas                                                                    *)
(*        Lily Stilson // 2019 - 2020                                                       *)
(*        MIT License                                                                       *)
(*                                                                                          *)
(*        Copyright (c) 2019 - 2020 Alice Romanets                                          *)
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
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.TabControl,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.Menus,
  FMX.ListView,
  FMX.Edit,
  FMX.DialogService.Sync,
  FMX.BufferedLayout,
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc,
  FMX.ListBox,
  Data.Bind.EngExt,
  Fmx.Bind.DBEngExt,
  System.Rtti,
  System.Bindings.Outputs,
  Fmx.Bind.Editors,
  Data.Bind.Components,
  {$IFDEF MSWINDOWS}Fmx.Platform.Win, WinApi.Windows;{$ENDIF MSWINDOWS}
  {$IFDEF MACOS}MacApi.Foundation;{$ENDIF MACOS}

type
  TImportForm = class(TForm)
    importButton: TButton;
    topLayout: TLayout;
    importLabel: TLabel;
    projectPathLayout: TLayout;
    aerProjectPathLabel: TLabel;
    aerProjectPath: TEdit;
    compositions: TListBox;
    compLayout: TGroupBox;
    XMLDocument: TXMLDocument;
    compProp: TGroupBox;
    compName: TLabel;
    compResolution: TLabel;
    compFramerate: TLabel;
    compRangeIn: TLabel;
    compRangeOut: TLabel;
    importProp: TGroupBox;
    compImportLayout: TLayout;
    splitRenderCheckbox: TCheckBox;
    BindingsList1: TBindingsList;
    LinkControlToPropertyShowCheckboxes: TLinkControlToProperty;
    XMLFile: TEdit;
    selectallButton: TButton;
    deselallButton: TButton;
    StatusBar1: TStatusBar;
    GridPanelLayout1: TGridPanelLayout;
    BufferedLayout1: TBufferedLayout;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure compositionsChangeCheck(Sender: TObject);
    procedure importButtonClick(Sender: TObject);
    procedure selectallButtonClick(Sender: TObject);
    procedure deselallButtonClick(Sender: TObject);
    procedure compositionsChange(Sender: TObject);
    procedure SetLanguage(LanguageCode: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;

var
  ImportForm: TImportForm;
  RootNode: IXMLNode;
  CheckedComps, CompCount: Integer;
  PARAMSTART: Boolean = False;
  XMLPath: String;

implementation

uses
  {$REGION '    AErenderLauncher Liraries    '}
  MainUnit;
  {$ENDREGION}

{$R *.fmx}

{$IFDEF MSWINDOWS}
procedure TImportForm.CreateHandle;
begin
  inherited CreateHandle;

  var hWnd: HWND := FormToHWND(Self);

  SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);
end;
{$ENDIF MSWINDOWS}

procedure TImportForm.SetLanguage(LanguageCode: Integer);
begin
  ImportForm.Caption        := Language[LanguageCode].ImportForm.ImportAEProject;

  importLabel.Text          := Language[LanguageCode].ImportForm.ImportAEProject;
  aerProjectPathLabel.Text  := Language[LanguageCode].ImportForm.ProjectPath;
  compLayout.Text           := Language[LanguageCode].ImportForm.Compositions;
  selectallButton.Text      := Language[LanguageCode].ImportForm.SelectAll;
  deselallButton.Text       := Language[LanguageCode].ImportForm.DeselectAll;

  compProp.Text             := Language[LanguageCode].ImportForm.CompositonProperties;
  compName.Text             := Language[LanguageCode].ImportForm.Name;
  compResolution.Text       := Language[LanguageCode].ImportForm.Resolution;
  compFramerate.Text        := Language[LanguageCode].ImportForm.Framerate;
  compRangeIn.Text          := Language[LanguageCode].ImportForm.RangeStart;
  compRangeOut.Text         := Language[LanguageCode].ImportForm.RangeEnd;

  importProp.Text           := Language[LanguageCode].ImportForm.ImportProperties;
  splitRenderCheckbox.Text  := Language[LanguageCode].ImportForm.PrepareForSplitRendering;

  importButton.Text         := Language[LanguageCode].ImportForm.Import;
end;

procedure TImportForm.importButtonClick(Sender: TObject);
var
  i, k: Integer;
begin
  MainForm.projectPath.Text := RootNode.Attributes['project'];
  MainForm.outputPath.Text := '';
  k := 0;
  if ((splitRenderCheckbox.Enabled) and (splitRenderCheckbox.IsChecked)) and (CheckedComps <= 1) then
    begin
      MainForm.compSwitch.Enabled := False;
      MainForm.compSwitch.IsChecked := False;
      MainForm.threadsSwitch.Enabled := True;
      MainForm.threadsSwitch.IsChecked := True;

      MainForm.calculateButton.Visible := True;
      MainForm.calculateButton.Enabled := True;
      MainForm.threadsGrid.Visible := True;
      MainForm.threadsGrid.Enabled := True;
      MainForm.threadsCount.Visible := True;
      MainForm.threadsCount.Enabled := True;
      MainForm.compCount.Enabled := False;
      MainForm.compCount.Visible := False;
      MainForm.compGrid.Enabled := False;
      MainForm.compGrid.Visible := False;

      MainForm.compName.Text := RootNode.ChildNodes['compositions'].ChildNodes[compositions.Selected.Index].ChildNodes['name'].Text;
      MainForm.inFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compositions.Selected.Index].ChildNodes['rangeStart'].Text)).ToString;
      MainForm.outFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compositions.Selected.Index].ChildNodes['rangeEnd'].Text)).ToString;
    end
  else
    begin
      if (CheckedComps = 1) or (CheckedComps = 0) then
        begin
          MainForm.compSwitch.Enabled := True;
          MainForm.compSwitch.IsChecked := False;
          MainForm.threadsSwitch.Enabled := True;
          MainForm.threadsSwitch.IsChecked := False;

          MainForm.compName.Enabled := True;
          MainForm.compName.Visible := True;
          MainForm.compCount.Enabled := False;
          MainForm.compCount.Visible := False;
          MainForm.compGrid.Enabled := False;
          MainForm.compGrid.Visible := False;
          MainForm.calculateButton.Visible := False;
          MainForm.calculateButton.Enabled := False;
          MainForm.threadsGrid.Visible := False;
          MainForm.threadsGrid.Enabled := False;
          MainForm.threadsCount.Visible := False;
          MainForm.threadsCount.Enabled := False;

          MainForm.compName.Text := RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['name'].Text;
          MainForm.inFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['rangeStart'].Text)).ToString;
          MainForm.outFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['rangeEnd'].Text)).ToString;
        end
      else
        begin
          MainForm.compSwitch.Enabled := True;
          MainForm.compSwitch.IsChecked := True;
          MainForm.threadsSwitch.Enabled := False;
          MainForm.threadsSwitch.IsChecked := False;
          MainForm.compName.Visible := False;
          MainForm.compName.Enabled := False;

          MainForm.compCount.Enabled := True;
          MainForm.compCount.Visible := True;
          MainForm.compGrid.Enabled := True;
          MainForm.compGrid.Visible := True;
          MainForm.calculateButton.Visible := False;
          MainForm.calculateButton.Enabled := False;
          MainForm.threadsGrid.Visible := False;
          MainForm.threadsGrid.Enabled := False;
          MainForm.threadsCount.Visible := False;
          MainForm.threadsCount.Enabled := False;

          MainForm.compCount.Value := CheckedComps;
          MainForm.inFrame.Text := '';
          MainForm.outFrame.Text := '';
          for i := 0 to CompCount - 1 do
            if compositions.ListItems[i].IsChecked then
              begin
                MainForm.compGrid.Cells[0, k] := RootNode.ChildNodes['compositions'].ChildNodes[i].ChildNodes['name'].Text;
                inc (k);
              end;
        end;
    end;
  XMLDocument.Active := False;
  ImportForm.Close;
end;

procedure TImportForm.compositionsChange(Sender: TObject);
begin
  compName.Text := Language[LANG].ImportForm.Name + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['name'].Text;
  compResolution.Text := Language[LANG].ImportForm.Resolution + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['resolution'].Text;
  compFramerate.Text := Language[LANG].ImportForm.Framerate + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['framerate'].Text;
  compRangeIn.Text := Language[LANG].ImportForm.RangeStart + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['rangeStart'].Text;
  compRangeOut.Text := Language[LANG].ImportForm.RangeEnd + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['rangeEnd'].Text;
end;

procedure TImportForm.compositionsChangeCheck(Sender: TObject);
begin
  CheckedComps := 0;
  for var i := 0 to CompCount - 1 do
    if compositions.ListItems[i].IsChecked then
      inc (CheckedComps);
  if CheckedComps > 0 then
    splitRenderCheckbox.Enabled := False
  else
    splitRenderCheckbox.Enabled := True;
end;

procedure TImportForm.deselallButtonClick(Sender: TObject);
begin
  for var i := 0 to CompCount-1 do
    compositions.ListItems[i].IsChecked := False;
end;

procedure TImportForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CheckedComps := 0;
  CompCount := 0;
  aerProjectPath.Text := '';
  compositions.Clear;
end;

procedure TImportForm.FormCreate(Sender: TObject);
begin
  compositions.AniCalculations.Animation := True;
end;

procedure TImportForm.FormShow(Sender: TObject);
begin
  try
    if PARAMSTART then
      begin
        XMLFile.Text := ExtractFileName(XMLPath);
        XMLDocument.LoadFromFile(XMLPath);
      end
    else
      begin
        XMLFile.Text := ExtractFileName(MainForm.XMLOpenDialog.FileName);
        XMLDocument.LoadFromFile(MainForm.XMLOpenDialog.FileName);
      end;

    XMLDocument.Active := True;

    XMLDocument.Encoding := 'utf-8';

    RootNode := XMLDocument.DocumentElement;

    aerProjectPath.Text := RootNode.Attributes['project'];
    CompCount := StrToInt(RootNode.ChildNodes['compositions'].Attributes['count']);
    for var i := 0 to CompCount - 1 do
      compositions.Items.Add(RootNode.ChildNodes['compositions'].ChildNodes[i].ChildNodes['name'].Text);
  except
    on Exception do begin
      if TDialogServiceSync.MessageDialog(Language[LANG].Errors.IncompatibleFile, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0) = 0 then
        ImportForm.Close;
    end;
  end;
end;

procedure TImportForm.selectallButtonClick(Sender: TObject);
begin
  for var i := 0 to CompCount - 1 do
    compositions.ListItems[i].IsChecked := True;
end;

end.
