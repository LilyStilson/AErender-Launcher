{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2014-2020 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ComboEdit.Style;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Classes, FMX.Presentation.Style, FMX.ComboEdit, FMX.Controls, FMX.Edit.Style,
  FMX.Pickers, FMX.ListBox, FMX.Controls.Presentation, FMX.Controls.Model, FMX.Presentation.Messages;

type

{ TComboEditListBox }

  TStyledComboEdit = class;

  TComboEditListBox = class(TCustomListBox)
  private
    [Weak] FStyledComboEdit: TStyledComboEdit;
    [Weak] FModel: TComboEditModel;
    FInKeyDown: Boolean;
    procedure HandleStringsChanged(const S: string; const Op: TCustomListBox.TStringsChangeOp);
  protected
    function GetDefaultStyleLookupName: string; override;
    function GetObservers: TObservers; override;
    /// <summary>Returns model of <c>TComboEditModel</c></summary>
    property Model: TComboEditModel read FModel write FModel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  end;

{ TStyledComboEditBase }

  TStyledComboEditBase = class(TStyledEdit)
  private
    FNeedSetFocusAfterButtonClick: Boolean;
    FArrowButton: TControl;
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    property ArrowButton: TControl read FArrowButton;
    /// <summary>Drop down list</summary>
    procedure PMDropDown(var AMessage: TDispatchMessage); message PM_DROPDOWN;
    /// <summary>Close the drop-down list.</summary>
    procedure PMCloseDropDown(var AMessage: TDispatchMessage); message PM_CLOSE_DROPDOWN;
    procedure Change; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoComboMouseDown(Sender:TObject; Button: TMouseButton; Shift: TShiftState; x, Y: Single); virtual;
    /// <summary>Can user open Drop Down list by clicking on Arrow Button</summary>
    function CanDropDown(const AButton: TMouseButton; const AShift: TShiftState): Boolean; virtual;
    procedure DropDown; virtual; abstract;
    /// <summary> This method closes the drop-down list, if it was opened </summary>
    procedure CloseDropDown; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TStyledComboEdit }

  TStyledComboEdit = class(TStyledComboEditBase)
  private type
    TStateInfo = record
      Saved: Boolean;
      ItemIndex: Integer;
      Text: string;
    end;
  private
    FPopup: TPopup;
    FListBox: TComboEditListBox;
    FListPicker: TCustomListPicker;
    FSavedState: TStateInfo;
    function GetComboEdit: TCustomComboEdit;
    procedure RebuildList;
    procedure RefreshSelectedItem;
    function GetModel: TComboEditModel;
    procedure DoOnValueChangedFromDropDownList(Sender: TObject; const AValueIndex: Integer);
    procedure DoPopup(Sender: TObject);
    procedure DoClosePopup(Sender: TObject);
  protected
    { Messages From Model}
    procedure MMItemIndexChanged(var AMessage: TDispatchMessage); message MM_ITEMINDEX_CHANGED;
    procedure MMItemHeightChanged(var AMessage: TDispatchMessage); message MM_ITEMHEIGHT_CHANGED;
    procedure MMItemWidthChanged(var AMessage: TDispatchMessage); message MM_ITEMWIDTH_CHANGED;
    procedure MMItemsChanged(var AMessage: TDispatchMessage); message MM_ITEMS_CHANGED;
    { Messages From Controller }
    /// <summary>Notification about initialization of presentation</summary>
    procedure PMInit(var AMessage: TDispatchMessage); message PM_INIT;
    procedure DoAbsoluteChanged; override;
  protected
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    /// <summary>Creates listbox for implementation Drop Down List</summary>
    function CreateListBox: TComboEditListBox; virtual;
    /// <summary>Updates width of drop down list</summary>
    procedure DoRealign; override;
    procedure DoExit; override;
    /// <summary>Saves current value of <c>ItemIndex</c> and <c>Text</c></summary>
    procedure SaveState;
    /// <summary> Restores previous saved save of <c>ItemIndex</c> and <c>Text</c>.
    /// If control didn't save state, this method will ignore invoke. </summary>
    procedure RestoreState;
    /// <summary>Initialization of List Picker</summary>
    procedure InitPicker(AListPicker: TCustomListPicker); virtual;
    /// <summary>Recalculates popup size based on items</summary>
    procedure RecalculatePopupSize; virtual;
    /// <summary>Defines <c>TComboEdit</c> model class</summary>
    function DefineModelClass: TDataModelClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>Opens drop down list. If lsit is opened, it will close popup.</summary>
    procedure DropDown; override;
    /// <summary>Closes drop down list</summary>
    procedure CloseDropDown; override;
    procedure Clear;
    /// <summary>Access to presented control</summary>
    property ComboEdit: TCustomComboEdit read GetComboEdit;
    /// <summary>Returns model of <c>TComboEditModel</c></summary>
    property Model: TComboEditModel read GetModel;
    property ListBox: TComboEditListBox read FListBox;
    /// <summary>Returns list picker, if current platform implements pickers</summary>
    property ListPicker: TCustomListPicker read FListPicker;
  end;

implementation

uses
  System.SysUtils, System.Math.Vectors, System.Rtti, System.Math, FMX.Platform, FMX.Types,
  FMX.Presentation.Factory, FMX.Forms, FMX.Text, FMX.Consts;

{ TComboEditListBox }

constructor TComboEditListBox.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TStyledComboEdit then
    FStyledComboEdit := TStyledComboEdit(AOwner);
  OnStringsChanged := HandleStringsChanged;
  FInKeyDown := False;
  SetAcceptsControls(False);
end;

procedure TComboEditListBox.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  if not FInKeyDown then
  begin
    FInKeyDown := True;
    try
      if FStyledComboEdit <> nil then
        FStyledComboEdit.KeyDown(Key, KeyChar, Shift)
      else
        inherited;
    finally
      FInKeyDown := False;
    end;
  end;
end;

procedure TComboEditListBox.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Item: TListBoxItem;
  LItemIndex: Integer;
begin
  inherited;
  Item := ItemByPoint(X, Y);
  if Item <> nil then
  begin
    if Selected = Item then
      Exit;
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
      if TLinkObservers.EditLinkIsReadOnly(Observers) then
        Exit
      else
        if not TLinkObservers.EditLinkEdit(Observers) then
          Exit;
    TLinkObservers.PositionLinkPosChanging(Observers);
    LItemIndex := ItemIndex;
    if MultiSelectStyle <> TMultiSelectStyle.None then
    begin
      if MultiSelectStyle = TMultiSelectStyle.Default then
      begin
  {$IFDEF MACOS}
        if ssCommand in Shift then
  {$ELSE}
        if ssCtrl in Shift then
  {$ENDIF}
          Item.IsSelected := not Item.IsSelected
        else
          SelectRange(FirstSelectedItem, Item);
        ItemIndex := Item.Index;
      end;
    end
    else
      ItemIndex := Item.Index;
    if LItemIndex <> ItemIndex then
      TLinkObservers.ListSelectionChanged(Observers);
  end;
end;

procedure TComboEditListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LItem : TListBoxItem;
  LItemIndex: Integer;
begin
  inherited;
  if (Parent is TPopup) and TPopup(Parent).IsOpen and (Model <> nil) then
  begin
    LItemIndex := ItemIndex;
    if LocalRect.Contains(PointF(X, Y)) then
    begin
      LItem := ItemByPoint(X, Y) ;
      if LItem <> nil then
      begin
        if LItem.Index <> Model.ItemIndex then
          TLinkObservers.PositionLinkPosChanging(Observers);
        if Observers.IsObserving(TObserverMapping.EditLinkID) then
        begin
          if TLinkObservers.EditLinkIsEditing(Observers) then
            Model.ItemIndex := LItem.Index;
        end
        else
          Model.ItemIndex := LItem.Index;
      end;
      if FStyledComboEdit <> nil then
        FStyledComboEdit.SaveState;
      Model.Change;
    end;
    TPopup(Parent).IsOpen := False;
    if LItemIndex <> ItemIndex then
      TLinkObservers.ListSelectionChanged(Observers);
  end;
end;

function TComboEditListBox.GetDefaultStyleLookupName: string;
begin
  Result := 'listboxstyle'; // do not localize
end;

function TComboEditListBox.GetObservers: TObservers;
begin
  if Model <> nil then
    Result := Model.Owner.Observers
  else
    Result := inherited;
end;

procedure TComboEditListBox.HandleStringsChanged(const S: string; const Op: TCustomListBox.TStringsChangeOp);
begin
  if (Model <> nil) and (Op = TCustomListBox.TStringsChangeOp.Added) then
    Model.Items.Add(S);
end;

{ TStyledComboEditBase }

procedure TStyledComboEditBase.ApplyStyle;
var
  DeviceSrv: IFMXDeviceService;
begin
  inherited;
  if FindStyleResource<TControl>('arrow', FArrowButton) then
  begin
    FArrowButton.HitTest := True;
    FArrowButton.Cursor := crArrow;
    FArrowButton.OnMouseDown := Self.DoComboMouseDown;
    if SupportsPlatformService(IFMXDeviceService, IInterface(DeviceSrv)) and (TDeviceFeature.HasTouchScreen in DeviceSrv.GetFeatures) then
      FArrowButton.TouchTargetExpansion.Rect := TRectF.Create(0, DefaultTouchTargetExpansion, DefaultTouchTargetExpansion, 0)
  end;
end;

function TStyledComboEditBase.CanDropDown(const AButton: TMouseButton; const AShift: TShiftState): Boolean;
begin
  Result := AButton = TMouseButton.mbLeft;
end;

procedure TStyledComboEditBase.Change;
begin
  RepaintEdit;
  inherited;
end;

constructor TStyledComboEditBase.Create(AOwner: TComponent);
var
  DefaultValueService: IFMXDefaultPropertyValueService;
  NeedSetFocusAfterButtonClick: TValue;
begin
  inherited;
  { Define default behavior for platforms }
  NeedSetFocusAfterButtonClick := TValue.Empty;
  if SupportsPlatformService(IFMXDefaultPropertyValueService, IInterface(DefaultValueService)) then
    NeedSetFocusAfterButtonClick := DefaultValueService.GetDefaultPropertyValue(Self.ClassName, 'NeedSetFocusAfterButtonClick');

  if NeedSetFocusAfterButtonClick.IsEmpty then
    FNeedSetFocusAfterButtonClick := True
  else
    FNeedSetFocusAfterButtonClick := NeedSetFocusAfterButtonClick.AsBoolean;
end;

procedure TStyledComboEditBase.DoComboMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  OldPressed: Boolean;
begin
  if CanDropDown(Button, Shift) then
  begin
    if FNeedSetFocusAfterButtonClick then
      SetFocus;
    OldPressed := Pressed;
    try
      Pressed := True;
      DropDown;
    finally
      Pressed := OldPressed;
    end;
  end;
end;

procedure TStyledComboEditBase.FreeStyle;
begin
  if FArrowButton <> nil then
    FArrowButton.OnMouseDown := nil;
  FArrowButton := nil;
  inherited;
end;

procedure TStyledComboEditBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  // if control does not support text input, then we must show drop down list,
  // when user clicked on control.
  if not Model.InputSupport then
    DoComboMouseDown(Self, Button, Shift, X, Y);
end;

procedure TStyledComboEditBase.PMCloseDropDown(var AMessage: TDispatchMessage);
begin
  CloseDropDown;
end;

procedure TStyledComboEditBase.PMDropDown(var AMessage: TDispatchMessage);
begin
  DropDown;
end;

{ TStyledComboEdit }

procedure TStyledComboEdit.Clear;
begin
  FListBox.Clear;
end;

procedure TStyledComboEdit.CloseDropDown;
begin
  if Model.DroppedDown then
  begin
    Model.DroppedDown := False;
    case Model.DropDownKind of
      TDropDownKind.Custom:
        FPopup.IsOpen := False;
      TDropDownKind.Native:
        if FListPicker.IsShown then
          FListPicker.Hide;
    end;
  end;
end;

constructor TStyledComboEdit.Create(AOwner: TComponent);
var
  PickerService: IFMXPickerService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, PickerService) then
  begin
    FListPicker := PickerService.CreateListPicker;
    FListPicker.Parent := Self;
    FListPicker.OnValueChanged := DoOnValueChangedFromDropDownList;
    FListPicker.OnHide := DoClosePopup;
    FListPicker.OnShow := DoPopup;
  end;
  FPopup := TPopup.Create(Self);
  FPopup.StyleLookup := 'combopopupstyle';
  FPopup.PlacementTarget := Self;
  FPopup.Stored := False;
  FPopup.Parent := Self;
  FPopup.Locked := True;
  FPopup.OnClosePopup := DoClosePopup;
  FPopup.OnPopup := DoPopup;
  FPopup.DragWithParent := True;
  FListBox := CreateListBox;
  if FListBox = nil then
    raise EArgumentNilException.CreateFmt(SResultCanNotBeNil, ['CreateListBox']);
  FListBox.Parent := FPopup;
  FListBox.Stored := False;
  FListBox.Align := TAlignLayout.Client;
  FListBox.ShowCheckboxes := False;
  FListBox.ItemIndex := -1;
  FListBox.Model := Model;
  FSavedState.Saved := False;
end;

function TStyledComboEdit.CreateListBox: TComboEditListBox;
begin
  Result := TComboEditListBox.Create(Self);
end;

function TStyledComboEdit.DefineModelClass: TDataModelClass;
begin
  Result := TComboEditModel;
end;

destructor TStyledComboEdit.Destroy;
begin
  FListPicker.Free;
  FPopup.Free;
  inherited;
end;

procedure TStyledComboEdit.DoClosePopup(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    Model.DisableNotify;
    try
      Model.DroppedDown := False;
      Model.SelStart := Model.Text.Length;
      Model.SelLength := 0;
    finally
      Model.EnableNotify;
    end;
    Model.Caret.Show;
    if Assigned(Model.OnClosePopup) then
      Model.OnClosePopup(PresentedControl);
  end;
end;

procedure TStyledComboEdit.DoExit;
begin
  inherited;
  CloseDropDown;
end;

procedure TStyledComboEdit.DoOnValueChangedFromDropDownList(Sender: TObject; const AValueIndex: Integer);
var
  LSaveIndex: Integer;
begin
  if not Model.ReadOnly then
  begin
    if Observers.IsObserving(TObserverMapping.EditLinkID) and not TLinkObservers.EditLinkEdit(Observers) then
      // Editing not permitted, ignore change
      Exit;
    LSaveIndex := Model.ItemIndex;
    Model.ItemIndex := AValueIndex;
    if Model.ItemIndex <> LSaveIndex then
    begin
      if Observers.IsObserving(TObserverMapping.EditLinkID) then
      begin
        TLinkObservers.EditLinkModified(Observers);
        TLinkObservers.EditLinkTrackUpdate(Observers);
      end;
      if Observers.IsObserving(TObserverMapping.ControlValueID) then
      begin
        TLinkObservers.ControlValueModified(Observers);
        TLinkObservers.ControlValueTrackUpdate(Observers);
      end;
    end;
    SaveState;
    Model.Change;
  end;
end;

procedure TStyledComboEdit.DoPopup(Sender: TObject);
begin
  if Assigned(Model.OnPopup) then
    Model.OnPopup(PresentedControl);
end;

procedure TStyledComboEdit.DoRealign;
begin
  inherited;
  if FDisableAlign then
    Exit;
  FDisableAlign := True;
  try
    FPopup.Width := Width;
  finally
    FDisableAlign := False;
  end;
end;

procedure TStyledComboEdit.DropDown;
begin
  if Model.DroppedDown then
    CloseDropDown
  else
  begin
    SaveState;
    Model.DroppedDown := True;

    if Model.Count > 0 then
      case Model.DropDownKind of
        TDropDownKind.Custom:
          begin
            RecalculatePopupSize;
            FPopup.IsOpen := True;
            if FPopup.IsOpen then
              FListBox.SetFocus;
          end;
        TDropDownKind.Native:
          begin
            Model.SelLength := 0;
            Model.Caret.Hide;
            InitPicker(FListPicker);
            FListPicker.Show;
          end;
      end;
  end;
end;

function TStyledComboEdit.GetComboEdit: TCustomComboEdit;
begin
  Result := PresentedControl as TCustomComboEdit;
end;

function TStyledComboEdit.GetModel: TComboEditModel;
begin
  Result := inherited GetModel<TComboEditModel>;
end;

procedure TStyledComboEdit.InitPicker(AListPicker: TCustomListPicker);
begin
  if Pressed or DoubleClick then
    FListPicker.PreferedDisplayIndex := Screen.DisplayFromPoint(Screen.MousePos).Index
  else
    FListPicker.PreferedDisplayIndex := -1;
  FListPicker.Values := Model.Items;
  FListPicker.ItemIndex := Model.ItemIndex;
  FListPicker.ItemWidth := Model.ItemWidth;
  FListPicker.CountVisibleItems := Model.DropDownCount;
end;

procedure TStyledComboEdit.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  LItemIndex: Integer;
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) and ((KeyChar > ' ') or (Key in [vkUp, vkDown])) then
    if TLinkObservers.EditLinkIsReadOnly(Observers) then
      Exit
    else
      TLinkObservers.EditLinkEdit(Observers);

  inherited;

  if not Model.DroppedDown then
    LItemIndex := Model.ItemIndex
  else if Model.DropDownKind = TDropDownKind.Native then
    LItemIndex := FListPicker.ItemIndex
  else
    LItemIndex := FListBox.ItemIndex;

  if (Model.Count > 0) and not Model.ReadOnly and Enabled then
  begin
    case Key of
      vkUp:
        LItemIndex := EnsureRange(LItemIndex - 1, 0, Model.Count - 1);
      vkDown:
        if (Shift = [ssAlt]) and not Model.DroppedDown then
          DropDown
        else
          LItemIndex := EnsureRange(LItemIndex + 1, 0, Model.Count - 1);
      vkReturn:
      begin
        Model.ItemIndex := LItemIndex;
        Model.Change;
        SaveState;
        CloseDropDown;
      end;
      vkEscape:
      begin
        RestoreState;
        CloseDropDown;
        RefreshSelectedItem;
        Exit;
      end
    else
      Exit;
    end;
    TLinkObservers.ListSelectionChanged(Observers);

    if not Model.DroppedDown then
    begin
      Model.ItemIndex := LItemIndex;
      Model.Change;
      SaveState;
      RefreshSelectedItem;
    end
    else if Model.DropDownKind = TDropDownKind.Native then
      FListPicker.ItemIndex := LItemIndex
    else
      FListBox.ItemIndex := LItemIndex;
    Key := 0;
    KeyChar := #0;
  end
  // Mark Return key as processed to avoid handling of Default button
  else if (Key = vkReturn) and ((Shift = [ssAlt]) or (Shift = [])) then
  begin
    Key := 0;
    KeyChar := #0;
  end;
end;

procedure TStyledComboEdit.PMInit(var AMessage: TDispatchMessage);
begin
  inherited;
  FListBox.ItemHeight := Model.ItemHeight;
end;

procedure TStyledComboEdit.DoAbsoluteChanged;
begin
  inherited;
  if FPopup.IsOpen and (not AbsoluteEnabled or not ParentedVisible or AbsoluteClipRect.IsEmpty) then
    CloseDropDown;
end;

procedure TStyledComboEdit.MMItemHeightChanged(var AMessage: TDispatchMessage);
begin
  FListBox.ItemHeight := Model.ItemHeight;
  if FListPicker <> nil then
    FListPicker.ItemHeight := Model.ItemHeight;
end;

procedure TStyledComboEdit.MMItemIndexChanged(var AMessage: TDispatchMessage);
begin
  RefreshSelectedItem;
end;

procedure TStyledComboEdit.MMItemsChanged(var AMessage: TDispatchMessage);
begin
  RebuildList;
  // if component is being loaded or destroyed, we don't assign items for picker. Because it can affect on life cycle of
  // android activity (only for Android) and to bring to deadlock of java thread (Event in CallInUIThreadAndWaitFinishing
  // and mutex in AndroidApi.AppGlue)
  if (FListPicker <> nil) and ([csLoading, csDestroying] * ComponentState = []) then
    FListPicker.Values := Model.Items;
  if InRange(Model.ItemIndex, 0, Model.Count - 1) then
    Text := Model.Items[Model.ItemIndex];
end;

procedure TStyledComboEdit.MMItemWidthChanged(var AMessage: TDispatchMessage);
begin
  if (TDropDownKind.Native = Model.DropDownKind) and (FListPicker <> nil) then
    FListPicker.ItemWidth := Model.ItemWidth
  else
    FPopup.Width := Model.ItemWidth;
end;

procedure TStyledComboEdit.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if Model.Count > 0 then
    if WheelDelta < 0 then
      Model.ItemIndex := EnsureRange(Model.ItemIndex + 1, 0, Model.Count - 1)
    else
      Model.ItemIndex := EnsureRange(Model.ItemIndex - 1, 0, Model.Count - 1);
  Handled := True;
end;

procedure TStyledComboEdit.RebuildList;
var
  SavedItemIndex, I: Integer;
  Item: TListBoxItem;
begin
  if csDestroying in ComponentState then
    Exit;

  FListBox.BeginUpdate;
  try
    SavedItemIndex := FListBox.ItemIndex;
    FListBox.Clear;
    for I := 0 to Model.Items.Count - 1 do
    begin
      Item := TListBoxItem.Create(nil);
      Item.Parent := FListBox;
      Item.AutoTranslate := AutoTranslate;
      Item.Height := Model.ItemHeight;
      Item.Stored := False;
      Item.Locked := True;
      Item.Text := Model.Items[I];
    end;
    FListBox.ItemIndex := EnsureRange(SavedItemIndex, -1, FListBox.Count - 1);
  finally
    FListBox.EndUpdate;
  end;
end;

procedure TStyledComboEdit.RecalculatePopupSize;

  procedure UpdateItemBounds(Index: Integer);
  var
    Item: TListBoxItem;
    NewHeight: Single;
    Content: TControl;
  begin
    Item := FListBox.ItemByIndex(Index);
    if (Item <> nil) and FindStyleResource<TControl>('Content', Content) then // do not localize
    begin
      if Item.Height <> 0 then
        NewHeight := Item.Height
      else if Model.ItemHeight = 0 then
        NewHeight := Content.Height
      else
        NewHeight := Model.ItemHeight;
      Item.SetBounds(Item.Position.X, Item.Position.Y, Item.Width, NewHeight);
      Item.ApplyStyleLookup;
    end;
  end;

  function CalculatePopupContentHeight: Single;
  var
    TotalHeight: Single;
    Num: Integer;
    I: Integer;
    Item: TListBoxItem;
  begin
    TotalHeight := 0;
    Num := 0;
    for I := 0 to FListbox.Count - 1 do
    begin
      Item := FListbox.ListItems[I];
      if Item.Position.Y >= 0 then
      begin
        TotalHeight := TotalHeight + Item.Height;
        Inc(Num);
      end;
      if Num >= Model.DropDownCount then
        Break;
    end;
    Result := TotalHeight;
  end;

var
  PopupContentHeight: Single;
  I: Integer;
begin
  // Resize list items to match the dimensions of the control
  for I := 0 to FListbox.Count - 1 do
    UpdateItemBounds(I);

  FPopup.ApplyStyleLookup;
  if Pressed or DoubleClick then
    FPopup.PreferedDisplayIndex := Screen.DisplayFromPoint(Screen.MousePos).Index
  else
    FPopup.PreferedDisplayIndex := -1;
  if SameValue(Model.ItemWidth, 0, TEpsilon.Position) then
    FPopup.Width := Width
  else
    FPopup.Width := Model.ItemWidth;

  if FListBox.ItemHeight > 0 then
    PopupContentHeight := Min(Model.Count, Model.DropDownCount) * FListBox.ItemHeight
  else
    PopupContentHeight := CalculatePopupContentHeight;
  FPopup.Height := FPopup.Padding.Top + PopupContentHeight + FListBox.BorderHeight + FPopup.Padding.Bottom;
end;

procedure TStyledComboEdit.RefreshSelectedItem;
var
  LValue: Integer;
begin
  LValue := Model.ItemIndex;
  if (Model.DropDownKind = TDropDownKind.Native) and (FListPicker <> nil) then
    FListPicker.ItemIndex := LValue;
  FListBox.ItemIndex := LValue;
  if Model.ItemIndex >= 0 then
  begin
    SetTextInternal(Model.Items[Model.ItemIndex]);
    Model.CaretPosition := Model.Text.Length;
    Model.Caret.Visible := Model.InputSupport;
    if not Model.ReadOnly then
      Edit.SelectAll;
    if ResourceControl <> nil then
      ResourceControl.UpdateEffects;
    Repaint;
  end;
end;

procedure TStyledComboEdit.RestoreState;
begin
  if FSavedState.Saved then
  begin
    Model.ItemIndex := EnsureRange(FSavedState.ItemIndex, -1, Model.Count - 1);
    if Model.ItemIndex = -1 then
      Model.Text := FSavedState.Text;
  end;
end;

procedure TStyledComboEdit.SaveState;
begin
  FSavedState.Saved := True;
  FSavedState.ItemIndex := Model.ItemIndex;
  FSavedState.Text := Model.Text;
end;

initialization
  TPresentationProxyFactory.Current.Register(TComboEdit, TControlType.Styled, TStyledPresentationProxy<TStyledComboEdit>);
finalization
  TPresentationProxyFactory.Current.Unregister(TComboEdit, TControlType.Styled, TStyledPresentationProxy<TStyledComboEdit>);
end.
