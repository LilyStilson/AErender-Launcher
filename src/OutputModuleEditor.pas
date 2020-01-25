unit OutputModuleEditor;

interface

uses
  System.Generics.Collections,
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
  FMX.Layouts,
  FMX.Edit,
  FMX.ListBox,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.TabControl,
  FMX.DialogService.Sync,
  FMX.Objects,
  Unit1,
  {$IFDEF MSWINDOWS}
    FMX.Platform.Win, Winapi.Windows, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    MacApi.Foundation;
  {$ENDIF MACOS}

type
  TOutputModuleEditorForm = class(TForm)
    WindowLabel: TLabel;
    outputModulesBox: TListBox;
    propertiesLayout: TLayout;
    outputModuleNameLayout: TLayout;
    outputModuleLabel: TLabel;
    outputModule: TEdit;
    presetNameLayout: TLayout;
    presetNameLabel: TLabel;
    presetName: TEdit;
    filenameBuilderLayout: TLayout;
    fileMaskLabel: TLabel;
    fileMask: TMemo;
    compTabFlow: TFlowLayout;
    TabControl1: TTabControl;
    projectTab: TTabItem;
    compTab: TTabItem;
    compTimeTab: TTabItem;
    imageTab: TTabItem;
    dateTab: TTabItem;
    projectTabFlow: TFlowLayout;
    compTimeTabFlow: TFlowLayout;
    imageTabFlow: TFlowLayout;
    dateTabFlow: TFlowLayout;
    PresetsConfiguratorToolbar: TToolBar;
    GridPanelLayout1: TGridPanelLayout;
    addModuleButton: TButton;
    removeModuleButton: TButton;
    saveModulesButton: TButton;
    cancelButton: TButton;
    Splitter1: TSplitter;
    Line1: TLine;
    procedure FormCreate(Sender: TObject);
    procedure outputModulesBoxChange(Sender: TObject);
    procedure fileMaskDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure fileMaskDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure addModuleButtonClick(Sender: TObject);
    procedure presetNameTyping(Sender: TObject);
    procedure cancelButtonClick(Sender: TObject);
    procedure removeModuleButtonClick(Sender: TObject);
    procedure outputModuleTyping(Sender: TObject);
    procedure fileMaskChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure saveModulesButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;

const
  availableFlags: TArray<String> =
      ['[projectName]', '[compName]', '[renderSettingsName]', '[outputModuleName]', '[fileExtension]',
      '[width]', '[height]', '[frameRate]', '[startFrame]', '[endFrame]', '[durationFrames]', '[#####]',
      '[startTimecode]', '[endTimecode]', '[durationTimecode]',
      '[channels]', '[projectColorDepth]', '[outputColorDepth]', '[compressor]', '[fieldOrder]', '[pulldownPhase]',
      '[projectFolder]',
      '[dateYear]', '[dateMonth]', '[dateDay]', '[timeHour]', '[timeMins]', '[timeSecs]', '[timeZone]'];

var
  OutputModuleEditorForm: TOutputModuleEditorForm;
  FlagButton: TArray<TButton>;
  TempOutputModules: TArray<Unit1.OutputModule>;

implementation

{$R *.fmx}

{$IFDEF MSWINDOWS}
procedure TOutputModuleEditorForm.CreateHandle;
begin
  inherited CreateHandle;
  var hWnd: HWND := FormToHWND(Self);

  SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);
end;
{$ENDIF MSWINDOWS}

procedure TOutputModuleEditorForm.cancelButtonClick(Sender: TObject);
begin
  Unit1.OutputModules := TempOutputModules;
  OutputModuleEditorForm.Close;
end;

procedure TOutputModuleEditorForm.fileMaskChange(Sender: TObject);
begin
  Unit1.OutputModules[outputModulesBox.ItemIndex].Mask := fileMask.Text;
end;

procedure TOutputModuleEditorForm.fileMaskDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
begin
  if Data.Source <> nil then
    fileMask.Text := fileMask.Text + TButton(Data.Source).Text;
end;

procedure TOutputModuleEditorForm.fileMaskDragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF;
  var Operation: TDragOperation);
begin
  Operation := TDragOperation.Link;
end;

procedure TOutputModuleEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  outputModulesBox.Items.Clear;
end;

procedure TOutputModuleEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Unit1.OutputModules <> TempOutputModules then
    case TDialogServiceSync.MessageDialog('Output modules was modified. Save changes?', TMsgDlgType.mtConfirmation, mbYesNoCancel, TMsgDlgBtn.mbYes, 0) of
      6: begin CanClose := True; saveModulesButtonClick(Sender); end;
      7: begin CanClose := True; cancelButtonClick(Sender); end;
      2: CanClose := False;
    end
  else
    OutputModuleEditorForm.Close;
end;

procedure TOutputModuleEditorForm.FormCreate(Sender: TObject);
begin
  if FlagButton = nil then
    begin
      SetLength (FlagButton, Length(AvailableFlags));
      for var i := 0 to High(FlagButton) do
        begin
          FlagButton[i] := TButton.Create(Self);
          case i of
            0..4: begin
              FlagButton[i].Parent := projectTabFlow;
            end;
            5..7: begin
              FlagButton[i].Parent := compTabFlow;
            end;
            8..14: begin
              FlagButton[i].Parent := compTimeTabFlow;
            end;
            15..20: begin
              FlagButton[i].Parent := imageTabFlow;
            end;
            21..28: begin
              FlagButton[i].Parent := dateTabFlow;
            end;
          end;
          FlagButton[i].Text := availableFlags[i];
          FlagButton[i].DragMode := TDragMode.dmAutomatic;
          FlagButton[i].Width := availableFlags[i].Length * 8;
          FlagButton[i].Margins.Bottom := 5;
          FlagButton[i].Margins.Left := 2;
          FlagButton[i].Margins.Right := 2;
          FlagButton[i].Margins.Top := 5;
        end;
    end;
end;

procedure TOutputModuleEditorForm.FormShow(Sender: TObject);
begin
  TempOutputModules := Unit1.OutputModules;
  for var i := 0 to High(Unit1.OutputModules) do
    outputModulesBox.Items.Add(Unit1.OutputModules[i].Name);
  outputModulesBox.ItemIndex := 0;
end;

procedure TOutputModuleEditorForm.outputModulesBoxChange(Sender: TObject);
begin
  if outputModulesBox.ItemIndex > -1 then
    begin
      presetName.Text := Unit1.OutputModules[outputModulesBox.ItemIndex].Name;
      outputModule.Text := Unit1.OutputModules[outputModulesBox.ItemIndex].Module;
      fileMask.Text := Unit1.OutputModules[outputModulesBox.ItemIndex].Mask;
    end;
end;

procedure TOutputModuleEditorForm.outputModuleTyping(Sender: TObject);
begin
  Unit1.OutputModules[outputModulesBox.ItemIndex].Module := outputModule.Text;
end;

procedure TOutputModuleEditorForm.presetNameTyping(Sender: TObject);
begin
  outputModulesBox.Items[outputModulesBox.ItemIndex] := presetName.Text;
  Unit1.OutputModules[outputModulesBox.ItemIndex].Name := presetName.Text;
end;

procedure TOutputModuleEditorForm.removeModuleButtonClick(Sender: TObject);
begin
  var tempIndex: Integer := outputModulesBox.ItemIndex;

  Unit1.OutputModules[tempIndex].Name := '';
  Unit1.OutputModules[tempIndex].Module := '';
  Unit1.OutputModules[tempIndex].Mask := '';

  if tempIndex <> High(Unit1.OutputModules) then
    for var i := tempIndex + 1 to High(Unit1.OutputModules) - 1 do
      Unit1.OutputModules[i - 1] := Unit1.OutputModules[i];

  SetLength (Unit1.OutputModules, Length (Unit1.OutputModules) - 1);

  outputModulesBox.Items.Delete(tempIndex);
  outputModulesBox.ItemIndex := tempIndex - 1;
end;

procedure TOutputModuleEditorForm.saveModulesButtonClick(Sender: TObject);
begin
  TempOutputModules := Unit1.OutputModules;
  UpdateOutputModules;
  OutputModuleEditorForm.Close;
end;

procedure TOutputModuleEditorForm.addModuleButtonClick(Sender: TObject);
begin
  SetLength (Unit1.OutputModules, Length (Unit1.OutputModules) + 1);
  outputModulesBox.Items.Insert(High(Unit1.OutputModules), 'Untitled');
  outputModulesBox.ItemIndex := High(Unit1.OutputModules);
end;

end.
