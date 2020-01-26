unit Unit4;

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
  TForm4 = class(TForm)
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
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure compositionsChangeCheck(Sender: TObject);
    procedure importButtonClick(Sender: TObject);
    procedure selectallButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure deselallButtonClick(Sender: TObject);
    procedure compositionsChange(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;

var
  Form4: TForm4;
  XML: IXMLDocument;
  RootNode: IXMLNode;
  CheckedComps, CompCount: Integer;
  PARAMSTART: Boolean = False;
  XMLPath: String;

implementation

uses
  Unit1;

{$R *.fmx}

{$IFDEF MSWINDOWS}
procedure TForm4.CreateHandle;
begin
  inherited CreateHandle;

  var hWnd: HWND := FormToHWND(Self);

  SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);
end;
{$ENDIF MSWINDOWS}

procedure TForm4.importButtonClick(Sender: TObject);
var
  i, k: Integer;
begin
  Form1.projectPath.Text := RootNode.Attributes['file'];
  Form1.outputPath.Text := '';
  if ((splitRenderCheckbox.Enabled) and (splitRenderCheckbox.IsChecked)) and (CheckedComps <= 1) then
    begin
      Form1.compSwitch.Enabled := False;
      Form1.compSwitch.IsChecked := False;
      Form1.threadsSwitch.Enabled := True;
      Form1.threadsSwitch.IsChecked := True;

      Form1.calculateButton.Visible := True;
      Form1.calculateButton.Enabled := True;
      Form1.threadsGrid.Visible := True;
      Form1.threadsGrid.Enabled := True;
      Form1.threadsCount.Visible := True;
      Form1.threadsCount.Enabled := True;
      Form1.compCount.Enabled := False;
      Form1.compCount.Visible := False;
      Form1.compGrid.Enabled := False;
      Form1.compGrid.Visible := False;

      Form1.compName.Text := RootNode.ChildNodes['compositions'].ChildNodes[compositions.Selected.Index].ChildNodes['name'].Text;
      Form1.inFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compositions.Selected.Index].ChildNodes['rangeStart'].Text)).ToString;
      Form1.outFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compositions.Selected.Index].ChildNodes['rangeEnd'].Text)).ToString;
    end
  else
    begin
      if (CheckedComps = 1) or (CheckedComps = 0) then
        begin
          Form1.compSwitch.Enabled := True;
          Form1.compSwitch.IsChecked := False;
          Form1.threadsSwitch.Enabled := True;
          Form1.threadsSwitch.IsChecked := False;

          Form1.compName.Enabled := True;
          Form1.compName.Visible := True;
          Form1.compCount.Enabled := False;
          Form1.compCount.Visible := False;
          Form1.compGrid.Enabled := False;
          Form1.compGrid.Visible := False;
          Form1.calculateButton.Visible := False;
          Form1.calculateButton.Enabled := False;
          Form1.threadsGrid.Visible := False;
          Form1.threadsGrid.Enabled := False;
          Form1.threadsCount.Visible := False;
          Form1.threadsCount.Enabled := False;

          Form1.compName.Text := RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['name'].Text;
          Form1.inFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['rangeStart'].Text)).ToString;
          Form1.outFrame.Text := Trunc(StrToFloat(RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['rangeEnd'].Text)).ToString;
        end
      else
        begin
          Form1.compSwitch.Enabled := True;
          Form1.compSwitch.IsChecked := True;
          Form1.threadsSwitch.Enabled := False;
          Form1.threadsSwitch.IsChecked := False;
          Form1.compName.Visible := False;
          Form1.compName.Enabled := False;

          Form1.compCount.Enabled := True;
          Form1.compCount.Visible := True;
          Form1.compGrid.Enabled := True;
          Form1.compGrid.Visible := True;
          Form1.calculateButton.Visible := False;
          Form1.calculateButton.Enabled := False;
          Form1.threadsGrid.Visible := False;
          Form1.threadsGrid.Enabled := False;
          Form1.threadsCount.Visible := False;
          Form1.threadsCount.Enabled := False;

          Form1.compCount.Value := CheckedComps;
          Form1.inFrame.Text := '';
          Form1.outFrame.Text := '';
          for i := 0 to CompCount-1 do
            if compositions.ListItems[i].IsChecked then
              begin
                Form1.compGrid.Cells[0, k] := RootNode.ChildNodes['compositions'].ChildNodes[i].ChildNodes['name'].Text;
                inc (k);
              end;
        end;
    end;
  k := 0;
  XMLDocument.Active := False;
  Form4.Close;
end;

procedure TForm4.compositionsChange(Sender: TObject);
begin
  compName.Text := 'Name: ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['name'].Text;
  compResolution.Text := 'Resolution: ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['resolution'].Text;
  compFramerate.Text := 'Framerate: ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['framerate'].Text;
  compRangeIn.Text := 'Range start: ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['rangeStart'].Text;
  compRangeOut.Text := 'Range end: ' + RootNode.ChildNodes['compositions'].ChildNodes[compositions.ItemIndex].ChildNodes['rangeEnd'].Text;
end;

procedure TForm4.compositionsChangeCheck(Sender: TObject);
var
  i: Integer;
begin
  CheckedComps := 0;
  for i := 0 to CompCount-1 do
    if compositions.ListItems[i].IsChecked then
      inc (CheckedComps);
  if CheckedComps > 0 then
    splitRenderCheckbox.Enabled := False
  else
    splitRenderCheckbox.Enabled := True;
end;

procedure TForm4.deselallButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to CompCount-1 do
    compositions.ListItems[i].IsChecked := False;
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CheckedComps := 0;
  CompCount := 0;
  aerProjectPath.Text := '';
  compositions.Clear;
end;

procedure TForm4.FormResize(Sender: TObject);
begin
  selectallButton.Width := compositions.Width * 0.5 - 2.5;
  deselallButton.Width := compositions.Width * 0.5 - 2.5;
end;

procedure TForm4.FormShow(Sender: TObject);
var
  i: Integer;
begin
  try
    if PARAMSTART then
      begin
        XMLFile.Text := ExtractFileName(XMLPath);
        XMLDocument.LoadFromFile(XMLPath);
      end
    else
      begin
        XMLFile.Text := ExtractFileName(Form1.XMLOpenDialog.FileName);
        XMLDocument.LoadFromFile(Form1.XMLOpenDialog.FileName);
      end;

    XMLDocument.Active := True;

    RootNode := XMLDocument.DocumentElement;

    aerProjectPath.Text := RootNode.Attributes['file'];
    CompCount := StrToInt(RootNode.ChildNodes['compositions'].Attributes['count']);
    for i := 0 to CompCount-1 do
      compositions.Items.Add(RootNode.ChildNodes['compositions'].ChildNodes[i].ChildNodes['name'].Text);
  except
    on Exception do
    begin
      TDialogServiceSync.MessageDialog('Selected file is not compatible with AErender Launcher. Please, provide proper .aer / .xml file for import!', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
      Form4.Close;
    end;
  end;
end;

procedure TForm4.selectallButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to CompCount-1 do
    compositions.ListItems[i].IsChecked := True;
end;

end.
