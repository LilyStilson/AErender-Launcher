unit OutputModuleEditorUnit;

(*        AErender Launcher                                                                 *)
(*        OutputModuleEditorUnit.pas                                                        *)
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
  FMX.Platform,

  MainUnit,

  {$IFDEF MSWINDOWS}
    FMX.Platform.Win, Winapi.Windows, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    MacApi.Foundation, MacApi.AppKit, MacApi.ObjectiveC, MacApi.CocoaTypes, FMX.Platform.Mac;
  {$ENDIF MACOS}

type
  TOutputModuleEditorForm = class(TForm)
    WindowLabel: TLabel;
    outputModulesBox: TListBox;
    propertiesLayout: TLayout;
    outputModuleNameLayout: TLayout;
    outputModuleLabel: TLabel;
    outputModule: TEdit;
    filenameBuilderLayout: TLayout;
    fileMaskLabel: TLabel;
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
    StatusBar1: TStatusBar;
    fileMask: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure outputModulesBoxChange(Sender: TObject);
    procedure fileMaskDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure fileMaskDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure addModuleButtonClick(Sender: TObject);
    procedure cancelButtonClick(Sender: TObject);
    procedure removeModuleButtonClick(Sender: TObject);
    procedure outputModuleTyping(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure saveModulesButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure fileMaskTyping(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;
  //{$IFDEF MACOS}TOCLocalAccess = class(TOCLocal);{$ENDIF MACOS}
  {$IFDEF MACOS}procedure NSWindowEditStateChange (State: Boolean);{$ENDIF MACOS}

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
  TempOutputModules: TArray<MainUnit.OutputModule>;

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

{$IFDEF MACOS}
procedure NSWindowEditStateChange (State: Boolean);
begin
  var Window: NSWindow := WindowHandleToPlatform(OutputModuleEditorForm.Handle).Wnd;

  if State then
    Window.setDocumentEdited(True)
  else
    Window.setDocumentEdited(False);
end;
{$ENDIF MACOS}

function CompareOutputModules(a, b: TArray<MainUnit.OutputModule>): Boolean;
begin
  for var i := 0 to High(a) do
    if (a[i].Module = b[i].Module) and (a[i].Mask = b[i].Mask) then
      Result := True
    else begin
      Result := False;
      break;
    end;
end;

procedure TOutputModuleEditorForm.cancelButtonClick(Sender: TObject);
begin
  MainUnit.OutputModules := TempOutputModules;
  SetLength(MainUnit.OutputModules, Length(TempOutputModules));
  OutputModuleEditorForm.Close;
end;

procedure TOutputModuleEditorForm.fileMaskDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
begin
  // Get current iterator index
  var IIndex: Integer := fileMask.SelStart;

  // Add data to TEdit
  if Data.Source <> nil then
    fileMask.Text := fileMask.Text.Insert(fileMask.SelStart, TButton(Data.Source).Text);

  // Move iterator to the end of added element
  fileMask.SelStart := IIndex + TButton(Data.Source).Text.Length;

  // Invoke typing event to remember new values
  fileMaskTyping(Sender);
end;

procedure TOutputModuleEditorForm.fileMaskDragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF;
  var Operation: TDragOperation);
begin
  Operation := TDragOperation.Link;
end;

procedure TOutputModuleEditorForm.fileMaskTyping(Sender: TObject);
begin
  MainUnit.OutputModules[outputModulesBox.ItemIndex].Mask := fileMask.Text;
  {$IFDEF MACOS}NSWindowEditStateChange(True);{$ENDIF MACOS}
end;

procedure TOutputModuleEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  outputModulesBox.Items.Clear;
  {$IFDEF MACOS}NSWindowEditStateChange(False);{$ENDIF MACOS}
end;

procedure TOutputModuleEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if not CompareOutputModules(MainUnit.OutputModules, TempOutputModules) then
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
  TempOutputModules := MainUnit.OutputModules;
  SetLength(TempOutputModules, Length(MainUnit.OutputModules));
  for var i := 0 to High(MainUnit.OutputModules) do
    outputModulesBox.Items.Add(MainUnit.OutputModules[i].Module);
  outputModulesBox.ItemIndex := 0;
  {$IFDEF MACOS}NSWindowEditStateChange(False);{$ENDIF MACOS}
end;

procedure TOutputModuleEditorForm.outputModulesBoxChange(Sender: TObject);
begin
  if outputModulesBox.ItemIndex > -1 then
    begin
      outputModule.Text := MainUnit.OutputModules[outputModulesBox.ItemIndex].Module;
      fileMask.Text := MainUnit.OutputModules[outputModulesBox.ItemIndex].Mask;
    end;
end;

procedure TOutputModuleEditorForm.outputModuleTyping(Sender: TObject);
begin
  outputModulesBox.Items[outputModulesBox.ItemIndex] := outputModule.Text;
  MainUnit.OutputModules[outputModulesBox.ItemIndex].Module := outputModule.Text;
  {$IFDEF MACOS}NSWindowEditStateChange(True);{$ENDIF MACOS}
end;

procedure TOutputModuleEditorForm.removeModuleButtonClick(Sender: TObject);
begin
  var tempIndex: Integer := outputModulesBox.ItemIndex;

  MainUnit.OutputModules[tempIndex].Module := '';
  MainUnit.OutputModules[tempIndex].Mask := '';

  if tempIndex <> High(MainUnit.OutputModules) then
    for var i := tempIndex + 1 to High(MainUnit.OutputModules) - 1 do
      MainUnit.OutputModules[i - 1] := MainUnit.OutputModules[i];

  SetLength (MainUnit.OutputModules, Length (MainUnit.OutputModules) - 1);

  outputModulesBox.Items.Delete(tempIndex);
  outputModulesBox.ItemIndex := tempIndex - 1;
end;

procedure TOutputModuleEditorForm.saveModulesButtonClick(Sender: TObject);
begin
  TempOutputModules := MainUnit.OutputModules;
  SetLength(TempOutputModules, Length(MainUnit.OutputModules));
  UpdateOutputModules;
  OutputModuleEditorForm.Close;
end;

procedure TOutputModuleEditorForm.addModuleButtonClick(Sender: TObject);
begin
  SetLength (MainUnit.OutputModules, Length(MainUnit.OutputModules) + 1);
  outputModulesBox.Items.Insert(High(MainUnit.OutputModules), 'Untitled');
  outputModulesBox.ItemIndex := High(MainUnit.OutputModules);
  {$IFDEF MACOS}NSWindowEditStateChange(True);{$ENDIF MACOS}
end;

end.
