unit ImportUnit;

(*        AErender Launcher                                                                 *)
(*        ImportUnit.pas                                                                    *)
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
  System.JSON,
  System.Generics.Collections,
  System.Rtti,
  System.Bindings.Outputs,

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
  Fmx.Bind.Editors,
  Fmx.Bind.DBEngExt,
  FMX.ListBox,

  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc,

  Data.Bind.EngExt,
  Data.Bind.Components,

  AErenderLauncher.Rendering,
  AErenderLauncher.Math,
  AErenderLauncher.Types,

  {$IFDEF MSWINDOWS}Fmx.Platform.Win, WinApi.Windows;{$ENDIF MSWINDOWS}
  {$IFDEF MACOS}MacApi.Foundation;{$ENDIF MACOS}

type
  TFormSender = (FS_JSON, FS_XML);
  TImportForm = class(TForm)
    importButton: TButton;
    topLayout: TLayout;
    importLabel: TLabel;
    projectPathLayout: TLayout;
    aerProjectPathLabel: TLabel;
    aerProjectPath: TEdit;
    compListBox: TListBox;
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
    SplitRenderCheckbox: TCheckBox;
    BindingsList1: TBindingsList;
    LinkControlToPropertyShowCheckboxes: TLinkControlToProperty;
    ImportedFile: TEdit;
    selectallButton: TButton;
    deselallButton: TButton;
    StatusBar1: TStatusBar;
    GridPanelLayout1: TGridPanelLayout;
    SplitRenderLayout: TLayout;
    Splitter1: TSplitter;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure compListBoxChangeCheck(Sender: TObject);
    procedure importButtonClick(Sender: TObject);
    procedure selectallButtonClick(Sender: TObject);
    procedure deselallButtonClick(Sender: TObject);
    procedure compListBoxChange(Sender: TObject);
    procedure SetLanguage(LanguageCode: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    RootNode: IXMLNode;
    Project: TJSONValue;

  public
    { Public declarations }
    FormSender: TFormSender;
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  end;

var
  ImportForm: TImportForm;
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
begin
  var Compositions: TList<TComposition> := TList<TComposition>.Create;

  if (CheckedComps = 1) or (CheckedComps = 0) then begin
    Compositions.Add(TComposition.Create(
       Project.A[compListBox.ItemIndex].P['Name'].Value,
       TFrameSpan.Create(Project.A[compListBox.ItemIndex].P['Frames'].A[0].Value, Project.A[compListBox.ItemIndex].P['Frames'].A[1].Value),
       1
    ));
  end else begin
    for var i := 0 to compListBox.Count - 1 do
      if compListBox.ListItems[i].IsChecked then
        Compositions.Add(TComposition.Create(
           Project.A[i].P['Name'].Value,
           TFrameSpan.Create(Project.A[i].P['Frames'].A[0].Value, Project.A[i].P['Frames'].A[1].Value),
           1
        ));
  end;

  RenderTasks.Add(TRenderTask.Create(
    MainForm.projectPath.Text,
    MainForm.outputPath.Text,

    MainForm.outputModuleBox.Items[MainForm.outputModuleBox.ItemIndex],
    MainForm.renderSettingsEdit.Text,

    MainForm.missingFilesCheckbox.IsChecked,
    MainForm.soundCheckbox.IsChecked,
    MainForm.threadedRender.IsChecked,
    IfThenElse(MainForm.customCheckbox.IsChecked = True, MainForm.customProp.Text, ''),  // yeet,

    MainForm.cacheUsageTrackBar.Value,
    MainForm.memUsageTrackBar.Value,

    Compositions
  ));

  {if ((SplitRenderCheckbox.Enabled) and (SplitRenderCheckbox.IsChecked)) and (CheckedComps <= 1) then begin

  end else begin
    if (CheckedComps = 1) or (CheckedComps = 0) then begin

    end else begin
      for var i := 0 to CheckedComps do

    end;
  end;      }



  {MainForm.projectPath.Text := RootNode.Attributes['project'];
  MainForm.outputPath.Text := '';
  k := 0;
  if ((SplitRenderCheckbox.Enabled) and (SplitRenderCheckbox.IsChecked)) and (CheckedComps <= 1) then
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

      MainForm.compName.Text := RootNode.ChildNodes['compositions'].ChildNodes[compListBox.Selected.Index].ChildNodes['name'].Text;
      MainForm.inFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compListBox.Selected.Index].ChildNodes['rangeStart'].Text)).ToString;
      MainForm.outFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compListBox.Selected.Index].ChildNodes['rangeEnd'].Text)).ToString;
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

          MainForm.compName.Text := RootNode.ChildNodes['compositions'].ChildNodes[compListBox.ItemIndex].ChildNodes['name'].Text;
          MainForm.inFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compListBox.ItemIndex].ChildNodes['rangeStart'].Text)).ToString;
          MainForm.outFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compListBox.ItemIndex].ChildNodes['rangeEnd'].Text)).ToString;
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
          for var i := 0 to CompCount - 1 do
            if compListBox.ListItems[i].IsChecked then
              begin
                MainForm.compGrid.Cells[0, k] := RootNode.ChildNodes['compositions'].ChildNodes[i].ChildNodes['name'].Text;
                inc (k);
              end;
        end;
    end; }



  XMLDocument.Active := False;
  ImportForm.Close;
end;

procedure TImportForm.compListBoxChange(Sender: TObject);
begin
  case FormSender of
    FS_JSON: begin
      compName.Text := Language[Settings.Language].ImportForm.Name + ': ' + Project.A[compListBox.ItemIndex].P['Name'].Value;
      compResolution.Text := Language[Settings.Language].ImportForm.Resolution + ': ' + Project.A[compListBox.ItemIndex].P['FootageDimensions'].A[0].Value + 'x' + Project.A[compListBox.ItemIndex].P['FootageDimensions'].A[1].Value;
      compFramerate.Text :=  Language[Settings.Language].ImportForm.Framerate + ': ' + Project.A[compListBox.ItemIndex].P['FootageFramerate'].Value;
      compRangeIn.Text := Language[Settings.Language].ImportForm.RangeStart + ': ' + Project.A[compListBox.ItemIndex].P['Frames'].A[0].Value;
      compRangeOut.Text := Language[Settings.Language].ImportForm.RangeEnd + ': ' + Project.A[compListBox.ItemIndex].P['Frames'].A[1].Value;
    end;
    FS_XML: begin
      compName.Text := Language[Settings.Language].ImportForm.Name + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compListBox.ItemIndex].ChildNodes['name'].Text;
      compResolution.Text := Language[Settings.Language].ImportForm.Resolution + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compListBox.ItemIndex].ChildNodes['resolution'].Text;
      compFramerate.Text := Language[Settings.Language].ImportForm.Framerate + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compListBox.ItemIndex].ChildNodes['framerate'].Text;
      compRangeIn.Text := Language[Settings.Language].ImportForm.RangeStart + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compListBox.ItemIndex].ChildNodes['rangeStart'].Text;
      compRangeOut.Text := Language[Settings.Language].ImportForm.RangeEnd + ': ' + RootNode.ChildNodes['compositions'].ChildNodes[compListBox.ItemIndex].ChildNodes['rangeEnd'].Text;
    end;
  end;
end;

procedure TImportForm.compListBoxChangeCheck(Sender: TObject);
begin
  CheckedComps := 0;
  for var i := 0 to CompCount - 1 do
    if compListBox.ListItems[i].IsChecked then
      inc (CheckedComps);
  if CheckedComps > 0 then
    SplitRenderCheckbox.Enabled := False
  else
    SplitRenderCheckbox.Enabled := True;
end;

procedure TImportForm.deselallButtonClick(Sender: TObject);
begin
  for var i := 0 to CompCount-1 do
    compListBox.ListItems[i].IsChecked := False;
end;

procedure TImportForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CheckedComps := 0;
  CompCount := 0;
  aerProjectPath.Text := '';
  FreeAndNil(Project);
  compListBox.Clear;
  //compositions.Items.Clear;
end;

procedure TImportForm.FormCreate(Sender: TObject);
begin
  compListBox.AniCalculations.Animation := True;
end;

procedure TImportForm.FormShow(Sender: TObject);
begin
  compListBox.Clear;
  case FormSender of
    FS_JSON: begin
      //ShowMessage(MainForm.ProjectJson);
      Project := TJSONObject.ParseJSONValue(MainForm.ProjectJson);
      ImportedFile.Text := APPFOLDER + ExtractFileName(MainForm.projectPath.Text).Replace('.aep', '.json');
      aerProjectPath.Text := MainForm.projectPath.Text;
      CompCount := (Project as TJSONArray).Count;
      for var i := 0 to CompCount - 1 do
        compListBox.Items.Add(Project.A[i].P['Name'].Value);
    end;
    FS_XML: begin
      try
        if PARAMSTART then begin
          ImportedFile.Text := ExtractFileName(XMLPath);
          XMLDocument.LoadFromFile(XMLPath);
        end else begin
          {$IFDEF MSWINDOWS}
          ImportedFile.Text := ExtractFileName(MainForm.XMLOpenDialog.FileName);
          {$ELSE MACOS}
          ImportedFile.Text := ExtractFileName(ImportedPath);
          {$ENDIF}
          XMLDocument.LoadFromFile(MainForm.XMLOpenDialog.FileName);
        end;

        XMLDocument.Active := True;

        XMLDocument.Encoding := 'utf-8';

        RootNode := XMLDocument.DocumentElement;

        aerProjectPath.Text := RootNode.Attributes['project'];
        CompCount := StrToInt(RootNode.ChildNodes['compositions'].Attributes['count']);
        for var i := 0 to CompCount - 1 do
          compListBox.Items.Add(RootNode.ChildNodes['compositions'].ChildNodes[i].ChildNodes['name'].Text);
      except
        on Exception do begin
          TDialogServiceSync.MessageDialog(Language[Settings.Language].Errors.IncompatibleFile, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
          ImportForm.Close;
        end;
      end;
    end;
  end;
end;

procedure TImportForm.selectallButtonClick(Sender: TObject);
begin
  for var i := 0 to CompCount - 1 do
    compListBox.ListItems[i].IsChecked := True;
end;

end.
