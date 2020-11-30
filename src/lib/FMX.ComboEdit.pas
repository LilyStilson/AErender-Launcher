{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2014-2020 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ComboEdit;

interface

{$SCOPEDENUMS ON}

uses
  FMX.Types, FMX.Controls, FMX.ListBox, System.Classes, System.UITypes, FMX.Edit, FMX.Pickers,
  FMX.Presentation.Messages, FMX.Controls.Presentation, FMX.Controls.Model;

const
  MM_ITEMHEIGHT_CHANGED = MM_EDIT_USER + 1;
  MM_ITEMWIDTH_CHANGED = MM_EDIT_USER + 2;
  MM_ITEMS_CHANGED = MM_EDIT_USER + 3;
  MM_ITEMINDEX_CHANGED = MM_EDIT_USER + 4;
  MM_DROPDOWNCOUNT_CHANGED = MM_EDIT_USER + 5;
  MM_DROPDOWNKIND_CHANGED = MM_EDIT_USER + 6;
  MM_LISTBOXRESOURCE_CHANGED = MM_EDIT_USER + 7;
  MM_COMBOEDIT_USER = MM_EDIT_USER + 8;
  PM_DROPDOWN = PM_EDIT_USER + 1;
  PM_CLOSE_DROPDOWN = PM_EDIT_USER + 2;
  PM_COMBOEDIT_USER = PM_EDIT_USER + 5;

type

{ TComboEditBase }

  // Base class for edit with the dropdown list. Provides action on a call popup
  TComboEditBase = class(TCustomEdit)
  public
    constructor Create(AOwner: TComponent); override;
    procedure DropDown; virtual;
    /// <summary> This method trying to close the drop-down list </summary>
    procedure CloseDropDown;
  end;

{ TComboEdit }

  TComboEditModel = class(TCustomEditModel)
  public const
    DefaultDropDownCount = 8;
    DefaultItemHeight = 19;
    DefaultDropDownKind = TDropDownKind.Native;
  private
    FItems: TStrings;
    FNeedSetFocusAfterButtonClick: Boolean;
    FDropDownCount: Integer;
    FPlacement: TPlacement;
    FItemHeight: Single;
    FItemWidth: Single;
    FDroppedDown: Boolean;
    FDropDownKind: TDropDownKind;
    FItemIndex: Integer;
    FListBoxResource: string;
    FOnPopup: TNotifyEvent;
    FOnClosePopup: TNotifyEvent;
    procedure DoItemsChanged(Sender: TObject);
    procedure SetItemHeight(const Value: Single);
    procedure SetItemWidth(const Value: Single);
    function GetCount: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure SetDropDownCount(const Value: Integer);
    procedure SetDropDownKind(const Value: TDropDownKind);
    procedure SetListBoxResource(const Value: string);
  protected
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass; override;
    function DoValidate(const Value: string): string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property DropDownKind: TDropDownKind read FDropDownKind write SetDropDownKind;
    property DroppedDown: Boolean read FDroppedDown write FDroppedDown;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount;
    property Items: TStrings read FItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemHeight: Single read FItemHeight write SetItemHeight;
    property ItemWidth: Single read FItemWidth write SetItemWidth;
    property ListBoxResource: string read FListBoxResource write SetListBoxResource;
    property NeedSetFocusAfterButtonClick: Boolean read FNeedSetFocusAfterButtonClick write FNeedSetFocusAfterButtonClick;
    property Placement: TPlacement read FPlacement;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  end;

  TCustomComboEdit = class(TComboEditBase)
  private
    function GetCount: Integer;
    procedure SetItemIndex(const Value: Integer);
    function GetItemIndex: Integer;
    function GetDroppedDown: Boolean;
    procedure SetDropDownCount(const Value: Integer);
    function GetDropDownCount: Integer;
    procedure SetDropDownKind(const Value: TDropDownKind);
    function GetDropDownKind: TDropDownKind;
    procedure SetListBoxResource(const Value: string);
    function GetListBoxResource: string;
    procedure SetItemHeight(const Value: Single);
    function GetItemHeight: Single;
    procedure SetItems(const Value: TStrings);
    function GetItems: TStrings;
    procedure SetItemWidth(const Value: Single);
    function GetItemWidth: Single;
    procedure SetOnClosePopup(const Value: TNotifyEvent);
    function GetOnClosePopup: TNotifyEvent;
    procedure SetOnPopup(const Value: TNotifyEvent);
    function GetOnPopup: TNotifyEvent;
    function GetModel: TComboEditModel; overload;
  protected
    function DefineModelClass: TDataModelClass; override;
    function DefinePresentationName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    property Action;
    property Model: TComboEditModel read GetModel;
    property DroppedDown: Boolean read GetDroppedDown;
    property Count: Integer read GetCount;
    property Font;
    property TextAlign;
  public
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount;
    property DropDownKind: TDropDownKind read GetDropDownKind write SetDropDownKind;
    property ItemHeight: Single read GetItemHeight write SetItemHeight;
    property ItemWidth: Single read GetItemWidth write SetItemWidth;
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ListBoxResource: string read GetListBoxResource write SetListBoxResource;
    property OnClosePopup: TNotifyEvent read GetOnClosePopup write SetOnClosePopup;
    property OnPopup: TNotifyEvent read GetOnPopup write SetOnPopup;
  end;

  TComboEdit = class(TCustomComboEdit)
  published
    property CanFocus default True;
    property CanParentFocus;
    property Cursor default crIBeam;
    property DisableFocusEffect;
    property DropDownCount default TComboEditModel.DefaultDropDownCount;
    property DropDownKind default TComboEditModel.DefaultDropDownKind;
    property KeyboardType;
    property ReadOnly;
    property ItemHeight;
    property ItemWidth;
    property Items;
    property ItemIndex;
    property ListBoxResource;
    property Text;
    property TextSettings;
    property Position;
    property Width;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property StyledSettings;
    property StyleLookup;
    property ClipChildren default False;
    property ClipParent default False;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible default True;
    property Caret;
    property KillFocusByReturn;
    property ParentShowHint;
    property ShowHint;
    { events }
    property OnChange;
    property OnChangeTracking;
    property OnTyping;
    property OnApplyStyleLookup;
    property OnClosePopup;
    property OnPopup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnValidating;
    property OnValidate;
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    property OnPresentationNameChoosing;
  end;

implementation

uses
  System.Types, System.SysUtils, System.Math, System.Math.Vectors, {$IFDEF MACOS}Macapi.CoreFoundation ,{$ENDIF}
  FMX.Platform, FMX.ComboEdit.Style;

{ TComboEditBase }

constructor TComboEditBase.Create(AOwner: TComponent);
begin
  inherited;
  MinClipWidth := 14;
  MinClipHeight := 14;
end;

procedure TComboEditBase.DropDown;
begin
  if HasPresentationProxy then
    PresentationProxy.SendMessage(PM_DROPDOWN);
end;

procedure TComboEditBase.CloseDropDown;
begin
  if HasPresentationProxy then
    PresentationProxy.SendMessage(PM_CLOSE_DROPDOWN);
end;

{ TComboEdit }

procedure TCustomComboEdit.Clear;
begin
  Model.Items.Clear;
end;

constructor TCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crIBeam;
end;

function TCustomComboEdit.DefineModelClass: TDataModelClass;
begin
  Result := TComboEditModel;
end;

function TCustomComboEdit.DefinePresentationName: string;
begin
  Result := 'ComboEdit-' + GetPresentationSuffix;
end;

procedure TCustomComboEdit.SetDropDownCount(const Value: Integer);
begin
  Model.DropDownCount := Value;
end;

procedure TCustomComboEdit.SetDropDownKind(const Value: TDropDownKind);
begin
  Model.DropDownKind := Value;
end;

procedure TCustomComboEdit.SetItemHeight(const Value: Single);
begin
  Model.ItemHeight := Value;
end;

procedure TCustomComboEdit.SetItems(const Value: TStrings);
begin
  Model.Items.Assign(Value);
end;

procedure TCustomComboEdit.SetItemWidth(const Value: Single);
begin
  Model.ItemWidth := Max(0, Value);
end;

function TCustomComboEdit.GetItemHeight: Single;
begin
  Result := Model.ItemHeight;
end;

function TCustomComboEdit.GetItemIndex: Integer;
begin
  Result := Model.ItemIndex;
end;

function TCustomComboEdit.GetItems: TStrings;
begin
  Result := Model.Items;
end;

function TCustomComboEdit.GetItemWidth: Single;
begin
  Result := Model.ItemWidth;
end;

procedure TCustomComboEdit.SetItemIndex(const Value: Integer);
begin
  Model.ItemIndex := Value;
end;

function TCustomComboEdit.GetCount: Integer;
begin
  Result := Model.Count;
end;

function TCustomComboEdit.GetDropDownCount: Integer;
begin
  Result := Model.DropDownCount;
end;

function TCustomComboEdit.GetDropDownKind: TDropDownKind;
begin
  Result := Model.DropDownKind;
end;

function TCustomComboEdit.GetDroppedDown: Boolean;
begin
  Result := Model.DroppedDown;
end;

function TCustomComboEdit.GetListBoxResource: string;
begin
  Result := Model.ListBoxResource;
end;

function TCustomComboEdit.GetModel: TComboEditModel;
begin
  Result := GetModel<TComboEditModel>;
end;

function TCustomComboEdit.GetOnClosePopup: TNotifyEvent;
begin
  Result := Model.OnClosePopup;
end;

function TCustomComboEdit.GetOnPopup: TNotifyEvent;
begin
  Result := GetModel<TComboEditModel>.OnPopup;
end;

procedure TCustomComboEdit.SetListBoxResource(const Value: string);
begin
  Model.ListBoxResource := Value;
end;

procedure TCustomComboEdit.SetOnClosePopup(const Value: TNotifyEvent);
begin
  Model.OnClosePopup := Value;
end;

procedure TCustomComboEdit.SetOnPopup(const Value: TNotifyEvent);
begin
  Model.OnPopup := Value;
end;

type
  TCustomEditBoxSettings = class(TTextSettingsInfo.TCustomTextSettings)
  public
    constructor Create(const AOwner: TPersistent); override;
  published
    property Font;
    property FontColor;
    property HorzAlign default TTextAlign.Leading;
    property VertAlign default TTextAlign.Center;
  end;

{ TCustomEditBoxSettings }

constructor TCustomEditBoxSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  WordWrap := False;
  HorzAlign := TTextAlign.Leading;
  VertAlign := TTextAlign.Center;
end;

{ TCustomComboEditModel }

constructor TComboEditModel.Create;
begin
  inherited;
  FItems := TStringList.Create;
  TStringList(FItems).CaseSensitive := True;
  TStringList(FItems).OnChange := DoItemsChanged;
  FDropDownKind := DefaultDropDownKind;
  FDropDownCount := DefaultDropDownCount;
  FItemHeight := DefaultItemHeight;
  FItemWidth := 0;
  FItemIndex := -1;
  FDroppedDown := False;
end;

destructor TComboEditModel.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TComboEditModel.DoItemsChanged(Sender: TObject);
begin
  SendMessage(MM_ITEMS_CHANGED);
end;

function TComboEditModel.DoValidate(const Value: string): string;
var
  SavedCaseSensitive: Boolean;
begin
  Result := inherited DoValidate(Value);
  // ComboEdit can include items with the same text values. In this case, when we validate new text value, it can
  // potentially reset item index on the first matches. ItemIndex can be set from Presentation, when user select item
  // in dropdown list. In this case we don't need to change ItemIndex value, if text was not really changed.
  if Text <> Result then
    ItemIndex := Items.IndexOf(Result);
  // IndexOf uses case sensitive search. If we don't find nothing, we try to use case insensitive search.
  if ItemIndex = -1 then
  begin
    SavedCaseSensitive := TStringList(FItems).CaseSensitive;
    try
      TStringList(FItems).CaseSensitive := False;
      ItemIndex := Items.IndexOf(Result);
    finally
      TStringList(FItems).CaseSensitive := SavedCaseSensitive;
    end;
  end;
end;

function TComboEditModel.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TComboEditModel.GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  Result := TCustomEditBoxSettings;
end;

procedure TComboEditModel.SetDropDownCount(const Value: Integer);
begin
  if FDropDownCount <> Value then
  begin
    FDropDownCount := Value;
    SendMessage(MM_DROPDOWNCOUNT_CHANGED);
  end;
end;

procedure TComboEditModel.SetDropDownKind(const Value: TDropDownKind);
begin
  if FDropDownKind <> Value then
  begin
    FDropDownKind := Value;
    SendMessage(MM_DROPDOWNKIND_CHANGED);
  end;
end;

procedure TComboEditModel.SetItemHeight(const Value: Single);
begin
  if not SameValue(FItemHeight, Value, TEpsilon.Vector) then
  begin
    FItemHeight := Value;
    SendMessage(MM_ITEMHEIGHT_CHANGED);
  end;
end;

procedure TComboEditModel.SetItemIndex(const Value: Integer);
begin
  if FItemIndex <> Value then
  begin
    FItemIndex := EnsureRange(Value, -1, Count - 1);
    SendMessage(MM_ITEMINDEX_CHANGED);
  end;
end;

procedure TComboEditModel.SetItemWidth(const Value: Single);
begin
  if not SameValue(FItemWidth, Value, TEpsilon.Vector) then
  begin
    FItemWidth := Value;
    SendMessage(MM_ITEMWIDTH_CHANGED);
  end;
end;

procedure TComboEditModel.SetListBoxResource(const Value: string);
begin
  if FListBoxResource <> Value then
  begin
    FListBoxResource := Value;
    SendMessage(MM_LISTBOXRESOURCE_CHANGED);
  end;
end;

initialization
  RegisterFmxClasses([TComboEdit]);
end.
