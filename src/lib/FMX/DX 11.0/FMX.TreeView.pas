{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2021 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.TreeView;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.UITypes, System.Generics.Collections, System.ImageList, FMX.Types, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls, FMX.TextLayout, FMX.Graphics, FMX.ActnList, FMX.ImgList;

type

{ TTreeViewItem }

  TCustomTreeView = class;

  TTreeViewItem = class(TTextControl, IItemsContainer, IGlyph)
  public const
    DefaultHeight = 19;
  private
    FIsExpanded: Boolean;
    FExpander: TControl;
    FButton: TCustomButton;
    FCheck: TCheckBox;
    FGlobalIndex: Integer;
    FIsChecked: Boolean;
    FIsSelected: Boolean;
    FContent: TContent;
    FNoItemsContent: TLayout;
    FVisibleCount: Integer;
    FMinItemSize: TSizeF;
    FCustomHeight: Boolean;
    FChildrenOffset: Single;
    FOldButtonVisible: Boolean;
    FOldCheckVisible: Boolean;
    FOldChecked: Boolean;
    FGlyph: TGlyph;
    FOldGlyphVisible: Boolean;
    FImageLink: TGlyphImageLink;
    [Weak] FImages: TCustomImageList;
    FCustomChildrenOffset: Single;
    function GetLevelOffset: Single;
    procedure DoButtonClick(Sender: TObject);
    procedure DoCheckClick(Sender: TObject);
    function GetCount: Integer;
    procedure SetIsChecked(const Value: Boolean);
    procedure UpdateCheckBoxVisibility;
    procedure UpdateExpandedButtonVisibility;
    procedure SetIsSelected(const Value: Boolean);
    procedure RecountVisibleItems;
    procedure RequestRecount;
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    { IGlyph }
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const Value: TImageIndex);
    function GetImageList: TBaseImageList; inline;
    procedure SetImageList(const Value: TBaseImageList);
    function IGlyph.GetImages = GetImageList;
    procedure IGlyph.SetImages = SetImageList;
    { IItemsContainer }
    function IItemsContainer.GetItemsCount = GetCount;
    function GetItem(const AIndex: Integer): TFmxObject;
    function GetMinItemSize: TSizeF;
    procedure OnExpandButtonStyleLookup(Sender: TObject);
    procedure UpdateChildrenOffset;
    procedure SetCustomChildrenOffset(const Value: Single);
    function IsCustomChildrenOffsetStored: Boolean;
  protected
    procedure ChangeOrder; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure DragEnd; override;
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass; override;
    function CalcMinSize: TSizeF;
    procedure InvalidateTextSize;
    /// <summary> Determines whether the <b>ImageIndex</b> property needs to be stored in the fmx-file</summary>
    /// <returns> <c>True</c> if <b>ImageIndex</b> property needs to be stored in the fmx file</returns>
    function ImageIndexStored: Boolean; virtual;
    /// <summary> Should be called when you change an instance or reference to instance of <b>TBaseImageList</b> or the
    /// <b>ImageIndex</b> property
    /// <para>See also <b>FMX.ActnList.IGlyph</b></para></summary>
    procedure ImagesChanged; virtual;
    procedure DoChanged; override;
    function IsSizeStored: Boolean; override;
    function IsPositionStored: Boolean; override;
    procedure SetText(const Value: string); override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    function EnterChildren(AObject: IControl): Boolean; override;
    function GetAbsoluteRect: TRectF; override;
    function DoGetUpdateRect: TRectF; override;
    procedure ContentAddObject(const AObject: TFmxObject); virtual;
    procedure ContentInsertObject(const AIndex: Integer; const AObject: TFmxObject); virtual;
    procedure ContentRemoveObject(const AObject: TFmxObject); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Paint; override;
    procedure SetVisible(const Value: Boolean); override;
    procedure SetHeight(const Value: Single); override;
    procedure SetIsExpanded(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Sort(Compare: TFmxObjectSortCompare); override;
    function ItemByPoint(const X, Y: Single): TTreeViewItem;
    function ItemByIndex(const Idx: Integer): TTreeViewItem;
    function TreeView: TCustomTreeView;
    function Level: Integer;
    function ParentItem: TTreeViewItem;
    procedure Select;
    procedure Deselect;
    procedure ExpandAll;
    procedure CollapseAll;
    procedure Expand;
    procedure Collapse;
    procedure Inflate; override;
    property Count: Integer read GetCount;
    property VisibleCount: Integer read FVisibleCount;
    property GlobalIndex: Integer read FGlobalIndex;
    property MinItemSize: TSizeF read GetMinItemSize;
    /// <summary> The actual offset in the right of children elements </summary>
    property ChildrenOffset: Single read FChildrenOffset;
    /// <summary> The offset of children elements which specified by user if this value is greater or equal to
    /// zero otherwise the default value is used. This value affects
    /// <see cref="FMX.TreeView|TTreeViewItem.ChildrenOffset">ChildrenOffset</see> property</summary>
    property CustomChildrenOffset: Single read FCustomChildrenOffset write SetCustomChildrenOffset
      stored IsCustomChildrenOffsetStored nodefault;
    property Items[const Index: Integer]: TTreeViewItem read ItemByIndex; default;
    property Width: Single read GetWidth;
    ///<summary> The list of images. Can be <c>nil</c>. <para>See also <b>FMX.ActnList.IGlyph</b></para></summary>
    property Images: TCustomImageList read GetImages;
  published
    property Align;
    property AutoTranslate default True;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property StyledSettings;
    property TextSettings;
    property Locked default False;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    {triggers}
    property IsChecked: Boolean read FIsChecked write SetIsChecked;
    property IsExpanded: Boolean read FIsExpanded write SetIsExpanded;
    property IsSelected: Boolean read FIsSelected write SetIsSelected;
    ///<summary> Zero based index of an image. The default is <c>-1</c>.
    ///<para> See also <b>FMX.ActnList.IGlyph</b></para></summary>
    ///<remarks> If non-existing index is specified, an image is not drawn and no exception is raised</remarks>
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex stored ImageIndexStored;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property StyleLookup;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible default True;
    property ParentShowHint;
    {events}
    property OnApplyStyleLookup;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
    property OnClick;
    property OnDblClick;
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
  end;

{ TTreeView }

  TOnCompareTreeViewItemEvent = function(Item1, Item2: TTreeViewItem): Integer of object;
  TOnTreeViewDragChange = procedure(SourceItem, DestItem: TTreeViewItem;
    var Allow: Boolean) of object;

{ TTreeView }

  TCustomTreeView = class(TScrollBox, IItemsContainer, IInflatableContent<TTreeViewItem>, IGlyph)
  private
    FMouseSelecting: Boolean;
    FOnChange: TNotifyEvent;
    FSelected: TTreeViewItem;
    FItemHeight: Single;
    FCountExpanded: Integer;
    FShowCheckboxes: Boolean;
    FOnChangeCheck: TNotifyEvent;
    FSorted: Boolean;
    FOnCompare: TOnCompareTreeViewItemEvent;
    FMultiSelect: Boolean;
    FFirstSelect: TTreeViewItem;
    FFocusedSelection: TControl;
    FSelection: TControl;
    FSelections: TControlList;
    FAllowDrag: Boolean;
    FStartDrag: Boolean;
    FDragItem: TTreeViewItem;
    FOnDragChange: TOnTreeViewDragChange;
    FGlobalList: TList<TTreeViewItem>;
    FAlternatingRowBackground: Boolean;
    FOddFill: TBrush;
    FFirstVisibleItem, FLastVisibleItem: Integer;
    FNoItemsContent: TContent;
    [Weak] FHoveredItem: TTreeViewItem;
    FTextLayout: TTextLayout;
    FToInflate: TList<TTreeViewItem>;
    FInflater: TContentInflater<TTreeViewItem>;
    FSelectionChanging: Boolean;
    FImageLink: TGlyphImageLink;
    [Weak] FImages: TCustomImageList;
    procedure SetItemHeight(const Value: Single);
    procedure SetCheckboxesVisibility(const Value: Boolean);
    function GetTreeItem(Index: Integer): TTreeViewItem;
    procedure SetSorted(const Value: Boolean);
    procedure SortItems;
    procedure SelectRange(const Item1, Item2: TTreeViewItem);
    procedure UpdateSelection;
    procedure UpdateVisibleItems;
    procedure UpdateGlobalIndexes(const Force: Boolean = False);
    procedure SetAllowDrag(const Value: Boolean);
    function GetCount: Integer;
    procedure SetAlternatingRowBackground(const Value: Boolean);
    procedure SetHoveredItem(const Value: TTreeViewItem);
    function LocalPointToHoveredItemLocalPoint(const AX, AY: Single): TPointF;
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    { IGlyph }
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const Value: TImageIndex);
    function GetImageList: TBaseImageList; inline;
    procedure SetImageList(const Value: TBaseImageList);
    function IGlyph.GetImages = GetImageList;
    procedure IGlyph.SetImages = SetImageList;
    { IItemsContainer }
    function IItemsContainer.GetItemsCount = GetCount;
    function GetItem(const AIndex: Integer): TFmxObject;
    function MouseInContentLayout(const X, Y: Single): Boolean;
    function GetGlobalCount: Integer;
    procedure InvalidateGlobalList;
    procedure ItemSizeMightHaveChanged(const AItem: TTreeViewItem);
    { Inflatable }
    function GetInflatableItems: TList<TTreeViewItem>;
    procedure NotifyInflated;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations); override;
    procedure Loaded; override;
    /// <summary> Should be called when you change an instance or reference to instance of <b>TBaseImageList</b> or
    /// <b>ImageIndex</b> property
    /// <para>See also <b>FMX.ActnList.IGlyph</b></para></summary>
    procedure ImagesChanged; virtual;
    { Selection }
    procedure ClearSelection;
    procedure SelectAll;
    procedure SetSelected(const Value: TTreeViewItem); virtual;
    { Styles }
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    { Tree View Events }
    procedure DoChange; virtual;
    procedure DoChangeCheck(const Item: TTreeViewItem); virtual;
    { Focus }
    procedure DoEnter; override;
    procedure DoExit; override;
    { Children structure }
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    function IsAddToContent(const AObject: TFmxObject): Boolean; override;
    procedure ContentAddObject(const AObject: TFmxObject); override;
    procedure ContentInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure ContentRemoveObject(const AObject: TFmxObject); override;
    procedure ContentBeforeRemoveObject(AObject: TFmxObject); override;
    { Content Scrolling }
    procedure ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
      const ContentSizeChanged: Boolean); override;
    function CreateScrollContent: TScrollContent; override;
    function DoCalcContentBounds: TRectF; override;
    function GetItemRect(const Item: TTreeViewItem): TRectF;
    { Painting }
    procedure DoContentPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    function IsOpaque: Boolean; override;
    { Mouse events }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    { Keyboards events }
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    { Drag And Drop }
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
    procedure DragLeave; override;
    function ObjectAtPoint(P: TPointF): IControl; override;
    procedure ChangeOrder; override;
    procedure ItemExpanded(const Item: TTreeViewItem);
    property HoveredItem: TTreeViewItem read FHoveredItem write SetHoveredItem;
    property FirstSelectedItem: TTreeViewItem read FFirstSelect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure ExpandAll;
    procedure CollapseAll;
    function ItemByText(const AText: string): TTreeViewItem;
    function ItemByPoint(const X, Y: Single): TTreeViewItem;
    function ItemByIndex(const Idx: Integer): TTreeViewItem;
    function ItemByGlobalIndex(const Idx: Integer): TTreeViewItem;
    property Count: Integer read GetCount;
    property GlobalCount: Integer read GetGlobalCount;
    property CountExpanded: Integer read FCountExpanded;
    property Selected: TTreeViewItem read FSelected write SetSelected;
    property Items[Index: Integer]: TTreeViewItem read GetTreeItem;
    property AllowDrag: Boolean read FAllowDrag write SetAllowDrag;
    property AlternatingRowBackground: Boolean read FAlternatingRowBackground write SetAlternatingRowBackground;
    property ItemHeight: Single read FItemHeight write SetItemHeight;
    ///<summary> The list of images. Can be <c>nil</c>. <para>See also <b>FMX.ActnList.IGlyph</b></para></summary>
    property Images: TCustomImageList read GetImages write SetImages;
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect default False;
    property ShowCheckboxes: Boolean read FShowCheckboxes write SetCheckboxesVisibility default False;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeCheck: TNotifyEvent read FOnChangeCheck write FOnChangeCheck;
    property OnCompare: TOnCompareTreeViewItemEvent read FOnCompare write FOnCompare;
    property OnDragChange: TOnTreeViewDragChange read FOnDragChange write FOnDragChange;
  end;

  TTreeView = class(TCustomTreeView)
  published
    property StyleLookup;
    property CanFocus default True;
    property CanParentFocus;
    property DisableFocusEffect default True;
    property TabOrder;
    property TabStop;
    property AllowDrag default False;
    property AlternatingRowBackground default False;
    property Hint;
    property ItemHeight;
    property Images;
    property MultiSelect default False;
    property ShowCheckboxes default False;
    property Sorted default False;
    property ParentShowHint;
    property ShowHint;
    property OnCanFocus;
    property OnChange;
    property OnChangeCheck;
    property OnCompare;
    property OnDragChange;
    property OnKeyDown;
    property OnKeyUp;
  end;

implementation

uses
  System.Math, System.Math.Vectors, System.RTLConsts, System.SysUtils, FMX.Consts, FMX.Ani, FMX.Utils;

type
  TTreeViewItemContent = class(TContent)
  private
    FItem: TTreeViewItem;
  protected
    function GetFirstVisibleObjectIndex: Integer; override;
    function GetLastVisibleObjectIndex: Integer; override;
    function GetAbsoluteRect: TRectF; override;
    function DoGetUpdateRect: TRectF; override;
    procedure DoRealign; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
  public
  end;

{ TTreeViewItemContent }

procedure TTreeViewItemContent.DoAddObject(const AObject: TFmxObject);
begin
  inherited DoAddObject(AObject);
  if Parent is TTreeViewItem then
    TTreeViewItem(Parent).ContentAddObject(AObject);
end;

procedure TTreeViewItemContent.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  if Parent is TTreeViewItem then
    TTreeViewItem(Parent).ContentRemoveObject(AObject);
end;

procedure TTreeViewItemContent.DoInsertObject(Index: Integer;
  const AObject: TFmxObject);
begin
  inherited DoInsertObject(Index, AObject);
  if Parent is TTreeViewItem then
    TTreeViewItem(Parent).ContentInsertObject(Index, AObject);
end;

procedure TTreeViewItemContent.DoRealign;
begin
end;

function TTreeViewItemContent.GetAbsoluteRect: TRectF;
begin
  if FItem <> nil then
    Result := FItem.GetAbsoluteRect
  else
    Result := inherited;
end;

function TTreeViewItemContent.DoGetUpdateRect: TRectF;
begin
  if FItem <> nil then
    Result := FItem.DoGetUpdateRect
  else
    Result := inherited;
end;

function TTreeViewItemContent.GetFirstVisibleObjectIndex: Integer;
begin
  Result := 0;
end;

function TTreeViewItemContent.GetLastVisibleObjectIndex: Integer;
begin
  if FItem.IsExpanded then
    Result := ControlsCount
  else
    Result := 0;
end;

{ TTreeViewItem }

constructor TTreeViewItem.Create(AOwner: TComponent);
begin
  inherited;
  FImageLink := TGlyphImageLink.Create(Self);
  AutoTranslate := True;
  TextAlign := TTextAlign.Leading;
  Size.SetSizeWithoutNotification(TSizeF.Create(Width, 0));
  FCustomHeight := False;
  HitTest := False;
  CanFocus := False;
  FContent := TTreeViewItemContent.Create(Self);
  FChildrenOffset := 0;
  FCustomChildrenOffset := -1;
  TTreeViewItemContent(FContent).FItem := Self;
  FContent.Stored := False;
  FContent.Parent := Self;
  FContent.Locked := True;
  FContent.HitTest := False;
end;

procedure TTreeViewItem.ChangeOrder;
var
  LTreeView : TCustomTreeView;
begin
  inherited ChangeOrder;
  LTreeView := TreeView;
  if (LTreeView <> nil) and not LTreeView.IsUpdating then
  begin
    LTreeView.UpdateGlobalIndexes(True);
    LTreeView.RealignContent;
  end;
end;

procedure TTreeViewItem.ContentAddObject(const AObject: TFmxObject);
var
  LTreeView : TCustomTreeView;
begin
  if not (AObject is TTreeViewItem) then
  begin
    if FNoItemsContent = nil then
    begin
      FNoItemsContent := TLayout.Create(nil);
      FNoItemsContent.Parent := Self;
      FNoItemsContent.SetBounds(0, 0, 0, 0);
      FNoItemsContent.Stored := False;
      FNoItemsContent.Locked := True;
      FNoItemsContent.HitTest := False;
    end;
    FNoItemsContent.AddObject(AObject)
  end
  else
  begin
    LTreeView := TreeView;
    if LTreeView <> nil then
    begin
      if [csLoading, csDestroying] * LTreeView.ComponentState = [] then
        TTreeViewItem(AObject).SetImages(LTreeView.Images);
      LTreeView.InvalidateGlobalList;
      LTreeView.RealignContent;
    end;
  end;
end;

type
  TTreeViewItemTextSettings = class(TTextSettingsInfo.TCustomTextSettings)
  public
    constructor Create(const AOwner: TPersistent); override;
    property WordWrap default False;
  published
    property Font;
    property FontColor;
    property HorzAlign;
  end;

{ TTreeViewItemTextSettings }

constructor TTreeViewItemTextSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  WordWrap := False;
end;

function TTreeViewItem.GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  Result := TTreeViewItemTextSettings;
end;

procedure TTreeViewItem.ContentInsertObject(const AIndex: Integer;
  const AObject: TFmxObject);
begin
  ContentAddObject(AObject);
end;

procedure TTreeViewItem.ContentRemoveObject(const AObject: TFmxObject);
var
  LTreeView: TCustomTreeView;
begin
  if AObject is TTreeViewItem then
  begin
    // After removing child item, we need recalculate Visible Items Count
    // and update Visible of 'Expand' button
    RecountVisibleItems;
    UpdateExpandedButtonVisibility;

    TTreeViewItem(AObject).Deselect;
    LTreeView := TreeView;
    if LTreeView <> nil then
    begin
      if [csLoading, csDestroying] * LTreeView.ComponentState = [] then
        TTreeViewItem(AObject).SetImages(nil);
      LTreeView.InvalidateGlobalList;
      LTreeView.RealignContent;
    end;
  end;
end;

procedure TTreeViewItem.RecountVisibleItems;
var
  I: Integer;
begin
  FVisibleCount := 0;
  for I := 0 to Count - 1 do
    if FContent.Controls[I].IsVisible then
      Inc(FVisibleCount);
end;

procedure TTreeViewItem.RequestRecount;
var
  LTreeView: TCustomTreeView;
begin
  LTreeView := TreeView;
  if LTreeView <> nil then
  begin
    LTreeView.InvalidateGlobalList;
    LTreeView.InvalidateContentSize;
  end;
end;

procedure TTreeViewItem.DragEnd;
var
  LTreeView : TCustomTreeView;
begin
  inherited;
  DragLeave;
  LTreeView := TreeView;
  if (LTreeView <> nil) and (LTreeView.FDragItem <> nil) then
  begin
    LTreeView.FDragItem.RemoveFreeNotify(LTreeView);
    LTreeView.FDragItem := nil;
  end;
end;

function TTreeViewItem.GetAbsoluteRect: TRectF;
var
  I: Integer;
begin
  Result := inherited GetAbsoluteRect;
  if IsExpanded and (Count > 0) then
  begin
    for I := 0 to Count - 1 do
      Result := UnionRect(Result, Items[I].AbsoluteRect);
  end;
end;

function TTreeViewItem.DoGetUpdateRect: TRectF;
var
  I: Integer;
begin
  Result := inherited DoGetUpdateRect;
  if IsExpanded and (Count > 0) then
    for I := 0 to Count - 1 do
      Result := UnionRect(Result, Items[I].UpdateRect);
end;

function TTreeViewItem.GetLevelOffset: Single;
var
  Item: TTreeViewItem;
  Offs: Single;
  function NextLevel(var Item: TTreeViewItem): Boolean;
  var
    P: TFmxObject;
  begin
    Result := False;
    P := Item.Parent;
    if (P is TContent) then
      P := P.Parent;
    if P is TTreeViewItem then
    begin
      Result := True;
      Item := TTreeViewItem(P);
    end;
  end;
begin
  Result := 0;
  Item := self;
  while NextLevel(Item) do
  begin
    Offs := Item.ChildrenOffset;
    Result := Result + Offs;
  end;
end;

function TTreeViewItem.GetCount: Integer;
begin
  Result := FContent.ControlsCount;
end;

procedure TTreeViewItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('HitTest', IgnoreBooleanValue, nil, False); // do not localize
  Filer.DefineProperty('Width', IgnoreFloatValue, nil, False); // do not localize
end;

procedure TTreeViewItem.Deselect;
begin
  IsSelected := False;
end;

destructor TTreeViewItem.Destroy;
  procedure ClearRef(var Value: TTreeViewItem);
  begin
    if Value = Self then
      Value := nil;
  end;
var
  LTreeView: TCustomTreeView;
begin
  LTreeView := TreeView;
  if LTreeView <> nil then
  begin
    ClearRef(LTreeView.FFirstSelect);
    ClearRef(LTreeView.FDragItem);
    ClearRef(LTreeView.FSelected);
    if LTreeView.FGlobalList <> nil then
      LTreeView.FGlobalList.Remove(Self);
  end;
  FImageLink.DisposeOf;
  FImageLink := nil;
  inherited;
end;

procedure TTreeViewItem.DoAddObject(const AObject: TFmxObject);
begin
  if AObject is TTreeViewItem then
  begin
    if FUpdating > TTreeViewItem(AObject).FUpdating then
      TTreeViewItem(AObject).BeginUpdate;
    FContent.AddObject(AObject);
    RecountVisibleItems;
    UpdateExpandedButtonVisibility;
    if IsExpanded then
      RequestRecount;
  end
  else
    inherited;
end;

procedure TTreeViewItem.DoInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  if AObject is TTreeViewItem then
  begin
    if FUpdating > TTreeViewItem(AObject).FUpdating then
      TTreeViewItem(AObject).BeginUpdate;
    FContent.InsertObject(Index, AObject);
    RecountVisibleItems;
    UpdateExpandedButtonVisibility;
    if IsExpanded then
      RequestRecount;
  end
  else
    inherited;
end;

procedure TTreeViewItem.DoRemoveObject(const AObject: TFmxObject);
begin
  if (AObject is TTreeViewItem) and (TTreeViewItem(AObject).TreeView = TreeView) then
  begin
    TTreeViewItem(AObject).Parent := nil;
    TTreeViewItem(AObject).Deselect;
    RecountVisibleItems;
    UpdateExpandedButtonVisibility;
    if IsExpanded then
      RequestRecount;
  end
  else
    inherited;
end;

function TTreeViewItem.GetItem(const AIndex: Integer): TFmxObject;
begin
  Result := Items[AIndex];
end;

function TTreeViewItem.ItemByPoint(const X, Y: Single): TTreeViewItem;
var
  I: Integer;
  P, P1: TPointF;
  LItem: TTreeViewItem;
begin
  P := LocalToAbsolute(TPointF.Create(X, Y));
  for I := 0 to Count - 1 do
  begin
    LItem := ItemByIndex(I);
    if not LItem.Visible then
      Continue;
    if LItem.PointInObject(P.X, P.Y) then
    begin
      Result := LItem;
      Exit;
    end
    else if (LItem.Count > 0) and (LItem.IsExpanded) then
    begin
      P1 := LItem.AbsoluteToLocal(P);
      Result := LItem.ItemByPoint(P1.X, P1.Y);
      if Result <> nil then
        Exit;
    end;
  end;
  Result := nil;
end;

function TTreeViewItem.IsPositionStored: Boolean;
begin
  Result := False;
end;

function TTreeViewItem.IsSizeStored: Boolean;
begin
  Result := FCustomHeight;
end;

function TTreeViewItem.ItemByIndex(const Idx: Integer): TTreeViewItem;
begin
  if (Idx >= 0) and (Idx < Count) then
    Result := TTreeViewItem(FContent.Controls[Idx])
  else
    raise ERangeError.CreateFMT(sArgumentOutOfRange_Index, [Idx, Count]);
end;

procedure TTreeViewItem.Paint;
var
  R: TRectF;
begin
  inherited Paint;
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.Stroke.Thickness := 1;
    Canvas.Stroke.Dash := TStrokeDash.Dash;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.Stroke.Dash := TStrokeDash.Solid;
  end;
end;

function TTreeViewItem.ParentItem: TTreeViewItem;
begin
  TFMXObjectHelper.FindNearestParentOfClass<TTreeViewItem>(Self, Result);
end;

function TTreeViewItem.EnterChildren(AObject: IControl): Boolean;
var
  LTreeView : TCustomTreeView ;
begin
  Result := inherited EnterChildren(AObject);
  LTreeView := TreeView;
  if LTreeView <> nil then
  begin
    LTreeView.Selected := Self;
    Result := True;
  end;
end;

procedure TTreeViewItem.Collapse;
begin
  IsExpanded := False;
end;

procedure TTreeViewItem.CollapseAll;
var
  i: Integer;
  Item: TTreeViewItem;
begin
  BeginUpdate;
  try
    Collapse;
    for i := Count - 1 downto 0 do
    begin
      Item := ItemByIndex(i);
      if Item <> nil then
        Item.CollapseAll;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TTreeViewItem.Expand;
begin
  IsExpanded := True;
end;

procedure TTreeViewItem.ExpandAll;
var
  i: Integer;
  Item: TTreeViewItem;
begin
  BeginUpdate;
  try
    Expand;
    for i := Count - 1 downto 0 do
    begin
      Item := ItemByIndex(i);
      if Item <> nil then
        Item.ExpandAll;
    end;
  finally
    EndUpdate;
  end;
end;

function TTreeViewItem.Level: Integer;
var
  P: TFmxObject;
begin
  Result := 0;
  P := Parent;
  while (P <> nil) and not (P is TCustomTreeView) do
  begin
    Result := Result + 1;
    P := P.Parent;
    if (P is TContent) then
      P := P.Parent;
  end;
end;

procedure TTreeViewItem.OnExpandButtonStyleLookup(Sender: TObject);
begin
  UpdateExpandedButtonVisibility;
end;

function TTreeViewItem.TreeView: TCustomTreeView;
begin
  TFMXObjectHelper.FindNearestParentOfClass<TCustomTreeView>(Self, Result);
end;

procedure TTreeViewItem.FreeStyle;
begin
  inherited;
  if FButton <> nil then
  begin
    FButton.OnClick := nil;
    FButton.OnApplyStyleLookup := nil;
    FButton.Visible := FOldButtonVisible;
    FButton := nil;
  end;
  FExpander := nil;
  if FCheck <> nil then
  begin
    FCheck.OnClick := nil;
    FCheck.OnChange := nil;
    FCheck.Visible := FOldCheckVisible;
    FCheck.IsChecked := FOldChecked;
    FCheck := nil;
  end;
  if FGlyph <> nil then
  begin
    FGlyph.BeginUpdate;
    try
      FGlyph.ImageIndex := -1;
      FGlyph.Images := nil;
    finally
      FGlyph.EndUpdate;
    end;
    FGlyph := nil;
  end;
end;

procedure TTreeViewItem.UpdateChildrenOffset;
begin
  if FCustomChildrenOffset >= 0 then
    FChildrenOffset := FCustomChildrenOffset
  else if FExpander <> nil then
    FChildrenOffset := FExpander.Width
  else if FCheck <> nil then
    FChildrenOffset := FCheck.Width
  else if FButton <> nil then
    FChildrenOffset := FButton.Width
  else
    FChildrenOffset := DefaultHeight;
end;

procedure TTreeViewItem.ApplyStyle;
var
  LTreeView: TCustomTreeView;
  LHeight: Single;
  LCustomHeight: Boolean;
begin
  LCustomHeight := FCustomHeight and (Height > 0);
  LHeight := Height;
  try
    inherited;
    FindStyleResource<TControl>('expander', FExpander);
    if FindStyleResource<TCustomButton>('button', FButton) then
    begin
      FOldButtonVisible := FButton.Visible;
      FButton.OnClick := DoButtonClick;
      FButton.OnApplyStyleLookup := OnExpandButtonStyleLookup;
      UpdateExpandedButtonVisibility;
    end;
    if FindStyleResource<TCheckBox>('check', FCheck) then
    begin
      FOldCheckVisible := FCheck.Visible;
      FOldChecked := FCheck.IsChecked;
      FCheck.IsChecked := IsChecked;
      FCheck.OnChange := DoCheckClick;
      LTreeView := TreeView ;
      if LTreeView <> nil then
        FCheck.Visible := LTreeView.ShowCheckboxes;
    end;
    if FindStyleResource<TGlyph>('glyphstyle', FGlyph) then
    begin
      FGlyph.Images := TCustomImageList(FImageLink.Images);
      FGlyph.ImageIndex := FImageLink.ImageIndex;
      FOldGlyphVisible := FGlyph.Visible;
    end
    else
      FOldGlyphVisible := False;
    if IsSelected then
    begin
      StartTriggerAnimation(Self, 'IsSelected');
      ApplyTriggerEffect(Self, 'IsSelected');
    end;
    Align := TAlignLayout.None;
    UpdateChildrenOffset;
  finally
    if LCustomHeight then
      Height := LHeight;
    FCustomHeight := LCustomHeight;
  end;
end;

function TTreeViewItem.CalcMinSize: TSizeF;
const
  MinText = '1';

  procedure UpdateWidth(const Control: TControl; const VisibleOnly: Boolean);
  var
    R: TRectF;
  begin
    if (Control <> nil) and (not VisibleOnly or Control.Visible) then
    begin
      R := Control.BoundsRect;
      R := Control.Margins.MarginRect(R);
      Result.cx := Result.cx + R.Width;
    end;
  end;

  function FirstWord(const Text: string; const NeedEllipsis: Boolean): string;
  var
    I: Integer;
  begin
    I := Text.IndexOf(' ');
    if I >= 0 then
    begin
      Result := Text.Substring(0, I);
      if NeedEllipsis then
        Result := Result + SChrHorizontalEllipsis;
    end
    else
      Result := Text;
  end;

var
  R: TRectF;
  LLayout: TTextLayout;
  LTree: TCustomTreeView;
  TextSettings: ITextSettings;
begin
  Result := TSizeF.Create(0, 0);
  LTree := TreeView;
  if LTree = nil then
    Exit;
  if Visible then
  begin
    if (TextObject <> nil) and TextObject.GetInterface(ITextSettings, TextSettings) then
    begin
      LLayout := LTree.FTextLayout;
      if LLayout <> nil then
      begin
        LLayout.BeginUpdate;
        try
          LLayout.WordWrap := False;
          LLayout.HorizontalAlign := TTextAlign.Leading;
          LLayout.VerticalAlign := TTextAlign.Leading;
          LLayout.Color := TAlphaColorRec.Black;
          LLayout.Font := TextSettings.ResultingTextSettings.Font;
          LLayout.Opacity := 1;
          LLayout.Trimming := TTextTrimming.None;
          if Text.IsEmpty then
            LLayout.Text := MinText
          else
            case TextSettings.ResultingTextSettings.Trimming of
              TTextTrimming.None:
              begin
                if TextSettings.ResultingTextSettings.WordWrap then
                  LLayout.Text := FirstWord(Text, False)
                else
                  LLayout.Text := Text;
              end;
              TTextTrimming.Character:
                LLayout.Text := MinText + SChrHorizontalEllipsis;
              TTextTrimming.Word:
                LLayout.Text := FirstWord(Text, True);
            end;
        finally
          LLayout.EndUpdate;
        end;
        R := LLayout.TextRect;
        Result.Width := Ceil(R.Width) + 2;
        Result.Height := Ceil(R.Height) + 4;
        UpdateWidth(FButton, False);
        UpdateWidth(FCheck, True);
        UpdateWidth(FGlyph, True);
        Result.cx := Result.cx + Padding.Left + Padding.Right;
        Result.cy := Result.cy + Padding.Top + Padding.Bottom;
      end;
    end;
  end;
end;

procedure TTreeViewItem.Inflate;
begin
  InvalidateTextSize;
  inherited;
  DisableDisappear := True;
  try
    ApplyStyleLookup;
  finally
    DisableDisappear := False;
  end;
  CalcMinSize;
end;

procedure TTreeViewItem.InvalidateTextSize;
begin
  FMinItemSize := TSizeF.Create(0, 0);
end;

function TTreeViewItem.GetMinItemSize: TSizeF;
begin
  if FMinItemSize.IsZero then
    FMinItemSize := CalcMinSize;
  Result := FMinItemSize;
end;

procedure TTreeViewItem.DoChanged;
begin
  inherited;
  if not FCustomHeight then
    InvalidateTextSize;
end;

procedure TTreeViewItem.DoCheckClick(Sender: TObject);
var
  LTreeView : TCustomTreeView;
begin
  if FCheck <> nil then
    FIsChecked := FCheck.IsChecked;
  LTreeView := TreeView;
  if LTreeView <> nil then
  begin
    LTreeView.SetFocus;
    LTreeView.DoChangeCheck(Self);
  end;
end;

procedure TTreeViewItem.UpdateCheckBoxVisibility;
var
  i, j: Integer;
  LTreeView: TCustomTreeView;
begin
  LTreeView := TreeView;
  if (LTreeView <> nil) and (FCheck <> nil) and
     (FCheck.Visible <> LTreeView.ShowCheckboxes) then
  begin
    FCheck.Visible := LTreeView.ShowCheckboxes;
    InvalidateTextSize;
  end;
  if ControlsCount > 0 then
    for i := 0 to ControlsCount - 1 do
      if Controls[i] is TTreeViewItem then
        TTreeViewItem(Controls[i]).UpdateCheckBoxVisibility
      else if Controls[i] is TTreeViewItemContent then
        for j := 0 to Controls[i].ControlsCount - 1 do
          if Controls[i].Controls[j] is TTreeViewItem then
            TTreeViewItem(Controls[i].Controls[j]).UpdateCheckBoxVisibility
end;

procedure TTreeViewItem.UpdateExpandedButtonVisibility;
begin
  if (FButton <> nil) and not (csLoading in ComponentState) then
  begin
    FButton.Visible := VisibleCount > 0;
    if FButton.Visible then
      TAnimator.StartTriggerAnimation(FButton, Self, 'IsExpanded');
  end;
end;

procedure TTreeViewItem.Select;
begin
  IsSelected := True;
end;

function TTreeViewItem.IsCustomChildrenOffsetStored: Boolean;
begin
  Result := FCustomChildrenOffset >= 0;
end;

procedure TTreeViewItem.SetCustomChildrenOffset(const Value: Single);
var
  Parent: TTreeViewItem;
begin
  if not SameValue(FCustomChildrenOffset, Value, TEpsilon.FontSize) then
  begin
    FCustomChildrenOffset := Value;
    UpdateChildrenOffset;
    if not IsUpdating and IsExpanded and (TreeView <> nil) then
    begin
      Parent := ParentItem;
      while Parent <> nil do
        if Parent.IsExpanded then
          Parent := Parent.ParentItem
        else
          Exit;
      TreeView.RealignContent;
      TreeView.UpdateSelection;
    end;
  end;
end;

procedure TTreeViewItem.SetHeight(const Value: Single);
var
  ItemSize: TSizeF;
begin
  if not SameValue(Value, Height) then
  begin
    if (not (csLoading in ComponentState)) then
    begin
      if Value <= 0 then
        FCustomHeight := False
      else
      begin
        ItemSize := MinItemSize;
        if ItemSize.cy > 0 then
          FCustomHeight := not SameValue(Value, ItemSize.cy)
        else
          FCustomHeight := True;
      end;
    end
    else
    begin
      FCustomHeight := Value >= 1;
    end;
  end;
  inherited;
end;

procedure TTreeViewItem.SetIsChecked(const Value: Boolean);
var
  LTreeView: TCustomTreeView;
begin
  if FIsChecked <> Value then
  begin
    FIsChecked := Value;
    if FCheck <> nil then
      FCheck.IsChecked := FIsChecked;
    LTreeView := TreeView;
    if LTreeView <> nil then
      LTreeView.DoChangeCheck(Self);
  end;
end;

procedure TTreeViewItem.SetIsSelected(const Value: Boolean);
var
  LTreeView : TCustomTreeView;
begin
  if FIsSelected <> Value then
  begin
    FIsSelected := Value;
    StartTriggerAnimation(Self, 'IsSelected');
    LTreeView := TreeView;
    if LTreeView <> nil then
    begin
      if not LTreeView.MultiSelect then
        if Value then
          LTreeView.Selected := Self
        else
          LTreeView.Selected := nil;
      LTreeView.UpdateSelection;
    end;
  end;
end;

procedure TTreeViewItem.SetText(const Value: string);
var
  LTreeView: TCustomTreeView;
begin
  if Value <> Text then
  begin
    inherited;
    InvalidateTextSize;
    LTreeView := TreeView;
    if LTreeView <> nil then
      LTreeView.ItemSizeMightHaveChanged(Self);
  end;
end;

procedure TTreeViewItem.SetVisible(const Value: Boolean);
var
  LChanged: Boolean;
  LParentItem: TTreeViewItem;
  LTreeView: TCustomTreeView;
begin
  LChanged := inherited Visible <> Value;
  inherited;
  if LChanged then
  begin
    LParentItem := ParentItem;
    if LParentItem <> nil then
      ParentItem.RecountVisibleItems;
    LTreeView := TreeView;
    if LTreeView <> nil then
      LTreeView.RealignContent;
  end;
end;

procedure TTreeViewItem.Sort(Compare: TFmxObjectSortCompare);
begin
  FContent.Sort(Compare);
end;

procedure TTreeViewItem.DoButtonClick(Sender: TObject);
begin
  IsExpanded := not IsExpanded;
end;

procedure TTreeViewItem.SetIsExpanded(const Value: Boolean);
var
  LTreeView: TCustomTreeView;
begin
  if FIsExpanded <> Value then
  begin
    FIsExpanded := Value;
    UpdateExpandedButtonVisibility;
    InvalidateTextSize;
    LTreeView := TreeView;
    if LTreeView <> nil then
      LTreeView.ItemExpanded(Self);
  end;
end;

function TTreeViewItem.GetImages: TCustomImageList;
begin
  if FImageLink <> nil then
    Result := TCustomImageList(FImageLink.Images)
  else
    Result := FImages;
end;

procedure TTreeViewItem.SetImages(const Value: TCustomImageList);
begin
  if FImageLink <> nil then
    FImageLink.Images := Value
  else
    FImages := Value;
end;

function TTreeViewItem.GetImageIndex: TImageIndex;
begin
  if FImageLink <> nil then
    Result := FImageLink.ImageIndex
  else
    Result := -1;
end;

procedure TTreeViewItem.SetImageIndex(const Value: TImageIndex);
begin
  if FImageLink <> nil then
    FImageLink.ImageIndex := Value;
end;

function TTreeViewItem.GetImageList: TBaseImageList;
begin
  Result := GetImages;
end;

procedure TTreeViewItem.SetImageList(const Value: TBaseImageList);
begin
  // none.
end;

function TTreeViewItem.ImageIndexStored: Boolean;
begin
  Result := ActionClient or (ImageIndex <> -1);
end;

procedure TTreeViewItem.ImagesChanged;

  procedure UpdateImages;
  var
    I: Integer;
  begin
    if FImages <> FImageLink.Images then
    begin
      FImages := TCustomImageList(FImageLink.Images);
      if [csLoading, csDestroying] * ComponentState = [] then
        for I := 0 to Count - 1 do
          Items[I].SetImages(FImages);
    end;
  end;

  procedure UpdateTreeView;
  var
    LTreeView: TCustomTreeView;
  begin
    if [csLoading, csDestroying] * ComponentState = [] then
    begin
      LTreeView := TreeView;
      if (LTreeView <> nil) and not LTreeView.IsUpdating then
      begin
        LTreeView.BeginUpdate;
        try
          UpdateImages;
          InvalidateTextSize;
          LTreeView.ItemSizeMightHaveChanged(Self);
        finally
          LTreeView.EndUpdate;
        end;
      end
      else
        UpdateImages;
    end;
  end;

var
  NewGlyphVisible: Boolean;
begin
  if FGlyph <> nil then
  begin
    FGlyph.BeginUpdate;
    try
      FGlyph.Images := TCustomImageList(FImageLink.Images);
      FGlyph.ImageIndex := FImageLink.ImageIndex;
    finally
      FGlyph.EndUpdate;
    end;
    if ([csLoading, csDestroying] * ComponentState = []) then
      FGlyph.Repaint;
  end;

  NewGlyphVisible := (FGlyph <> nil) and (FGlyph.Visible);
  if FOldGlyphVisible <> NewGlyphVisible then
  begin
    FOldGlyphVisible := NewGlyphVisible;
    UpdateTreeView;
  end
  else
    UpdateImages;
end;

{ TTreeViewContent }

type

  TTreeViewContent = class(TScrollContent)
  private
    [Weak] FTreeView: TCustomTreeView;
  protected
    function GetFirstVisibleObjectIndex: Integer; override;
    function GetLastVisibleObjectIndex: Integer; override;
    procedure DoRealign; override;
  public
  end;

function TTreeViewContent.GetFirstVisibleObjectIndex: Integer;
var
  Item: TTreeViewItem;
begin
  if FTreeView.FGlobalList.Count > 0 then
  begin
    Item := FTreeView.FGlobalList[FTreeView.FFirstVisibleItem];
    while Item.ParentItem <> nil do
      Item := Item.ParentItem;
    Result := Item.Index;
  end
  else
    Result := 0;
end;

function TTreeViewContent.GetLastVisibleObjectIndex: Integer;
var
  Item: TTreeViewItem;
begin
  if (FTreeView.FGlobalList.Count > 0) and (FTreeView.FLastVisibleItem < FTreeView.FGlobalList.Count) then
  begin
    Item := FTreeView.FGlobalList[FTreeView.FLastVisibleItem];
    while Item.ParentItem <> nil do
      Item := Item.ParentItem;
    Result := Item.Index + 1;
    if Result > ControlsCount then
      Result := ControlsCount;
  end
  else
    Result := ControlsCount;
end;

procedure TTreeViewContent.DoRealign;
begin
end;

{ TTreeView }

constructor TCustomTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FImageLink := TGlyphImageLink.Create(Self);
  FGlobalList := TList<TTreeViewItem>.Create;
  FGlobalList.Capacity := 100;
  FOddFill := TBrush.Create(TBrushKind.Solid, $20000000);
  DisableFocusEffect := True;
  CanFocus := True;
  AutoCapture := True;
  Width := 100;
  Height := 100;
  FItemHeight := 0;
  SetAcceptsControls(False);
  FToInflate := TList<TTreeViewItem>.Create;
  FInflater := TContentInflater<TTreeViewItem>.Create(Self);
end;

function TCustomTreeView.CreateScrollContent: TScrollContent;
begin
  Result := TTreeViewContent.Create(Self);
  TTreeViewContent(Result).FTreeView := Self;
end;

destructor TCustomTreeView.Destroy;
begin
  FreeAndNil(FSelections);
  FreeAndNil(FGlobalList);
  FreeAndNil(FOddFill);
  FInflater.DisposeOf;
  FreeAndNil(FToInflate);
  FImageLink.DisposeOf;
  inherited;
end;

procedure TCustomTreeView.ApplyStyle;
var
  T: TFmxObject;
  ContentLayout: TControl;
begin
  inherited ApplyStyle;
  if FindStyleResource<TControl>('content', ContentLayout) then
    ContentLayout.OnPainting := DoContentPaint;
  if FindStyleResource<TControl>('selection', FSelection) then
    FSelection.Visible := False;
  if FindStyleResource<TControl>('focusedselection', FFocusedSelection) then
    FFocusedSelection.Visible := False;
  T := FindStyleResource('AlternatingRowBackground');
  if T is TBrushObject then
    FOddFill.Assign(TBrushObject(T).Brush);
  if T is TControl then
    TControl(T).Visible := False;
  UpdateSelection;
end;

procedure TCustomTreeView.FreeStyle;
begin
  inherited FreeStyle;
  FSelection := nil;
  FFocusedSelection := nil;
  if FSelections <> nil then
    FSelections.Clear;
end;

procedure TCustomTreeView.UpdateGlobalIndexes(const Force: Boolean);
var
  GlobalIdx: Integer;

  procedure RecountItem(const AItem: TTreeViewItem);
  var
    I: Integer;
  begin
    if AItem <> nil then
    begin
      AItem.FGlobalIndex := GlobalIdx;
      Inc(GlobalIdx);
      FGlobalList.Add(AItem);
      if AItem.IsExpanded then
        for I := 0 to AItem.Count - 1 do
          RecountItem(AItem.ItemByIndex(I));
    end;
  end;

var
  I: Integer;
begin
  if Force then
    FGlobalList.Clear;
  if (FGlobalList.Count = 0) and (Count > 0) then
  begin
    GlobalIdx := 0;
    for I := 0 to Count - 1 do
      RecountItem(ItemByIndex(I));
  end;
end;

function CompareTreeItem(Item1, Item2: TFmxObject): Integer;
var
  LItem1TV: TCustomTreeView;
begin
  if (Item1 is TTreeViewItem) and (Item2 is TTreeViewItem) then
  begin
    LItem1TV := TTreeViewItem(Item1).TreeView;
    if (LItem1TV <> nil) and Assigned(LItem1TV.OnCompare) then
      Result := LItem1TV.OnCompare(TTreeViewItem(Item1),
        TTreeViewItem(Item2))
    else
      Result := CompareText(TTreeViewItem(Item1).Text,
        TTreeViewItem(Item2).Text);
  end
  else
    Result := 0;
end;

procedure TCustomTreeView.SortItems;
begin
  if not FSorted then
    Exit;
  Content.Sort(CompareTreeItem);
  InvalidateGlobalList;
  UpdateGlobalIndexes;
  UpdateVisibleItems;
end;

function TCustomTreeView.GetItemRect(const Item: TTreeViewItem): TRectF;
begin
  if Item <> nil then
  begin
    Result := TRectF.Create(0, 0, Item.Width, Item.Height);
    Result.Offset(Content.AbsoluteToLocal(Item.LocaltoAbsolute(TPointF.Zero)));
  end
  else
    Result := TRectF.Empty;
end;

function TCustomTreeView.GetInflatableItems: TList<TTreeViewItem>;
begin
  Result := FToInflate;
end;

function TCustomTreeView.GetItem(const AIndex: Integer): TFmxObject;
begin
  Result := Items[AIndex];
end;

procedure TCustomTreeView.UpdateSelection;
var
  I: Integer;
  P: TPointF;
  R: TRectF;
  SelRects: array of TRectF;
  Clone: TControl;
  Item: TTreeViewItem;
begin
  if FSelection = nil then
    Exit;
  // calc rects
  SetLength(SelRects, 0);
  if GlobalCount > 0 then
    for I := FFirstVisibleItem to FLastVisibleItem do
    begin
      Item := ItemByGlobalIndex(I);
      if Item.IsSelected and Item.Visible then
      begin
        P := Item.LocaltoAbsolute(TPointF.Zero);
        if (FSelection.Parent <> nil) and (FSelection.Parent is TControl) then
          P := FSelection.ParentControl.AbsoluteToLocal(P);
        R := RectF(P.X, P.Y, P.X + Item.Width,
          P.Y + Item.Height);
        if (Length(SelRects) > 0) and (I > 0) and ItemByGlobalIndex(I - 1).IsSelected then
          SelRects[High(SelRects)] := UnionRect(R, SelRects[High(SelRects)])
        else
        begin
          SetLength(SelRects, Length(SelRects) + 1);
          SelRects[High(SelRects)] := R;
        end;
      end;
    end;
  // Create selection list
  if FSelections = nil then
    FSelections := TControlList.Create;
  for I := 0 to FSelections.Count - 1 do
    FSelections[I].DisposeOf;
  FSelections.Clear;
  // create selections
  for I := 0 to Length(SelRects) - 1 do
  begin
    if Self.IsFocused and (FFocusedSelection <> nil) then
      Clone := TControl(FFocusedSelection.Clone(Self))
    else
      Clone := TControl(FSelection.Clone(Self));
    Clone.StyleName := string.Empty;
    FSelections.Add(Clone);
    Clone.Parent := FSelection.Parent;
  end;
  // hide if not needed
  if Length(SelRects) < FSelections.Count then
    for I := Length(SelRects) to FSelections.Count - 1 do
      FSelections[I].Visible := False;
  // align selections
  for I := 0 to High(SelRects) do
  begin
    FSelections[I].Visible := True;
    R := SelRects[I];
    FSelections[I].SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  end;
end;

procedure TCustomTreeView.UpdateVisibleItems;
var
  First, Last, Mid: Integer;
  R: TRectF;
begin
  First := 0;
  Last := GlobalCount - 1;
  while First < Last do
  begin
    Mid := First + (Last - First) div 2;
    R := FGlobalList[Mid].AbsoluteRect;
    R := AbsoluteToLocal(R);
    if R.Bottom < 0 then
      First := Mid + 1
    else if R.Top > Height then
      Last := Mid
    else
      Break;
  end;
  FFirstVisibleItem := First;
  FLastVisibleItem := Last;
  TTreeViewContent(Content).RecalcUpdateRect;
  if FNoItemsContent <> nil then
    FNoItemsContent.SetBounds(Content.Position.X, Content.Position.Y, Content.Width, Content.Height);
end;

procedure TCustomTreeView.ItemSizeMightHaveChanged(const AItem: TTreeViewItem);
begin
  if (ItemHeight = 0) and not AItem.FCustomHeight then
    RealignContent;
end;

function TCustomTreeView.DoCalcContentBounds: TRectF;
var
  FirstItemHeight: Single;

  function UpdateSize(const AItem: TTreeViewItem; const CurY: Single; const ContentWidth: Single;
    var BottomRight: TPointF): Single;
  var
    I: Integer;
    OffsetX: Single;
    ItemSize: TSizeF;
    TopLeft: TPointF;
    MinWidth: Single;
  begin
    Result := CurY;
    OffsetX := AItem.GetLevelOffset;
    TopLeft := Content.ConvertLocalPointTo(AItem.ParentControl, TPointF.Create(OffsetX + AItem.Margins.Left, CurY + AItem.Margins.Top));

    if AItem.IsVisible or AItem.ShowInDesigner then
    begin
      AItem.UpdateExpandedButtonVisibility;
      MinWidth := ContentWidth - OffsetX - AItem.Margins.Left - AItem.Margins.Right;

      ItemSize := AItem.MinItemSize;
      if FItemHeight > 0 then
        ItemSize.cy := FItemHeight
      else if AItem.FCustomHeight then
        ItemSize.cy := AItem.Height
      else if ItemSize.cy <= 0 then
      begin
        if SameValue(FirstItemHeight, 0, TEpsilon.Position) then
        begin
          AItem.Inflate;
          if AItem.MinItemSize.Height > 0 then
            FirstItemHeight := AItem.MinItemSize.Height
          else
            FirstItemHeight := TTreeViewItem.DefaultHeight;
        end;
        ItemSize.cy := FirstItemHeight;
      end;

      if ItemSize.Width <= 0 then
        ItemSize.cx := MinWidth
      else
        ItemSize.cx := Max(ItemSize.cx, MinWidth);

      Result := CurY + ItemSize.cy + AItem.Margins.Top + AItem.Margins.Bottom;
      BottomRight.X := Max(BottomRight.X, OffsetX + ItemSize.cx + AItem.Margins.Left + AItem.Margins.Right);
      BottomRight.Y := Max(BottomRight.Y, Result);
      AItem.SetBounds(TopLeft.X, TopLeft.Y, AItem.Width, ItemSize.cy);

      if (AItem.Count > 0) and AItem.IsExpanded then
        for I := 0 to AItem.Count - 1 do
          Result := UpdateSize(AItem.ItemByIndex(I), Result, ContentWidth, BottomRight);
    end
    else
      AItem.SetBounds(TopLeft.X, TopLeft.Y, AItem.Width, 0);
  end;

  procedure AdjustWidth(const AItem: TTreeViewItem; const BottomRight: TPointF);
  var
    I: Integer;
    P: TPointF;
  begin
    if AItem.IsVisible or AItem.ShowInDesigner then
    begin
      P := Content.LocalToAbsolute(BottomRight);
      P := AItem.ParentControl.AbsoluteToLocal(Content.LocalToAbsolute(BottomRight));
      AItem.SetWidth(P.X - AItem.Position.X);
      if AItem.IsExpanded then
        for I := 0 to AItem.Count - 1 do
          AdjustWidth(AItem.ItemByIndex(I), BottomRight);
    end;
  end;

var
  I: Integer;
  CurY: Single;
  LLocalRect: TRectF;
begin
  FirstItemHeight := 0;
  UpdateGlobalIndexes;
  if (not IsUpdating) and (ContentLayout <> nil) then
  begin
    LLocalRect := ContentLayout.LocalRect;
    Result := LLocalRect;
    if Result.Bottom = Result.Top then
      Result.Bottom := Result.Top + 1;
    { content }
    FCountExpanded := 0;
    if Content <> nil then
    begin
      { Sort if need }
      SortItems;
      { align }
      CurY := 0;
      FTextLayout := TTextLayoutManager.DefaultTextLayout.Create;
      try
        for I := 0 to Count - 1 do
          CurY := UpdateSize(ItemByIndex(I), CurY, LLocalRect.Width, Result.BottomRight);
      finally
        FreeAndNil(FTextLayout);
      end;
      for I := 0 to Count - 1 do
        AdjustWidth(ItemByIndex(I), Result.BottomRight);
    end;
    UpdateVisibleItems;
  end;
end;

procedure TCustomTreeView.ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
  const ContentSizeChanged: Boolean);
begin
  inherited;
  UpdateVisibleItems;
  UpdateSelection;
end;

function TCustomTreeView.ItemByIndex(const Idx: Integer): TTreeViewItem;
begin
  if (Idx >= 0) and (Idx < Count) then
  begin
    Assert(Content.Controls[Idx] is TTreeViewItem);
    Result := TTreeViewItem(Content.Controls[Idx]);
  end
  else
    raise ERangeError.CreateFMT(sArgumentOutOfRange_Index, [Idx, Count]);
end;

function TCustomTreeView.ItemByGlobalIndex(const Idx: Integer): TTreeViewItem;
begin
  Result := FGlobalList[Idx];
end;

function TCustomTreeView.ObjectAtPoint(P: TPointF): IControl;
var
  Local, Abs: TPointF;
  Inner: IControl;
  function CheckScrollBar(ScrollBar: TScrollBar): Boolean;
  var
    R: TRectF;
  begin
    Result := (ScrollBar <> nil) and
              (ScrollBar.Visible) and
              (ScrollBar.Enabled) and
              (ScrollBar.HitTest);
    if Result then
    begin
      R := ScrollBar.AbsoluteRect;
      Result := R.Contains(Abs);
    end;
  end;
begin
  Local := ScreenToLocal(P);
  if LocalRect.Contains(Local) then
  begin
    Abs := LocalToAbsolute(Local);
    if CheckScrollBar(VScrollBar) then
      Result := IControl(VScrollBar).ObjectAtPoint(P)
    else if CheckScrollBar(HScrollBar) then
      Result := IControl(HScrollBar).ObjectAtPoint(P)
    else
    begin
      if not Supports(ItemByPoint(Local.X, Local.Y), IControl, Result) then
        Result := inherited ObjectAtPoint(P)
      else
      begin
        Inner := Result.ObjectAtPoint(P);
        if Inner <> nil then
          Result := Inner
        else
          if CheckHitTest(HitTest) then
            Result := Self
          else
            Result := inherited ObjectAtPoint(P);
      end;
    end;
  end
  else
    Result := nil;
end;

function TCustomTreeView.ItemByPoint(const X, Y: Single): TTreeViewItem;
var
  I: Integer;
  P: TPointF;
  Item: TTreeViewItem;

  function PointInItem(const Itm: TTreeViewItem; const Pt: TPointF): Boolean;
  var
    ItemPt: TPointF;
  begin
    ItemPt := Item.AbsoluteToLocal(Pt);
    Result := Content.PointInObject(Pt.X, Pt.Y)
      and (ItemPt.Y >= (0 - TouchTargetExpansion.Top)) and (ItemPt.Y <= (Itm.Height + TouchTargetExpansion.Bottom));
  end;

begin
  P := LocaltoAbsolute(PointF(X, Y));
  if GlobalCount > 0 then
    for I := FFirstVisibleItem to FLastVisibleItem do
    begin
      Item := FGlobalList[I];
      if not Item.Visible then
        Continue;
      if not IntersectRect(Item.UpdateRect, UpdateRect) then
        Continue;
      if PointInItem(Item, P) then
        Exit(Item);
    end;
  Result := nil;
end;

function TCustomTreeView.ItemByText(const AText: string): TTreeViewItem;
var
  Item: TTreeViewItem;
  I: Integer;
begin
  Result := nil;
  for I := 0 to GlobalCount - 1 do
  begin
    Item := ItemByGlobalIndex(I);
    if CompareText(AText, Item.Text) = 0 then
      Exit(Item);
  end;
end;

procedure TCustomTreeView.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);

  procedure SelectUpperItem;
  var
    I: Integer;
  begin
    if Selected.GlobalIndex > 0 then
    begin
      I := Selected.GlobalIndex - 1;
      while (I >= 0) and (ItemByGlobalIndex(I).GlobalIndex > 0) and (not ItemByGlobalIndex(I).IsVisible) do
        Dec(I);
      if (I >= 0) and ItemByGlobalIndex(I).IsVisible then
        Selected := ItemByGlobalIndex(I);
    end;
  end;

  procedure SelectLowerItem;
  var
    I: Integer;
  begin
    if Selected.GlobalIndex < GlobalCount - 1 then
    begin
      I := Selected.GlobalIndex + 1;
      while (I < GlobalCount) and (ItemByGlobalIndex(I).GlobalIndex < Pred(GlobalCount)) and (not ItemByGlobalIndex(I).IsVisible) do
        Inc(I);
      if (I < GlobalCount) and ItemByGlobalIndex(I).IsVisible then
        Selected := ItemByGlobalIndex(I);
    end;
  end;

var
  LSelParentItem : TTreeViewItem;
begin
  inherited KeyDown(Key, KeyChar, Shift);
  if (Count > 0) and (Selected <> nil) then
  begin
    case Key of
      vkAdd:
        Selected.Expand;
      vkSubtract:
        Selected.Collapse;
      vkHome:
        begin
          Selected := ItemByGlobalIndex(0);
          if not Selected.Visible then
            SelectLowerItem;
        end;
      vkEnd:
        begin
          Selected := ItemByGlobalIndex(GlobalCount - 1);
          if not Selected.Visible then
            SelectUpperItem;
        end;
      vkUp:
        SelectUpperItem;
      vkDown:
        SelectLowerItem;
      vkLeft:
        if Selected.IsExpanded then
          Selected.Collapse
        else
        begin
          LSelParentItem := Selected.ParentItem;
          if LSelParentItem <> nil then
          begin
            Selected := LSelParentItem;
            if not Selected.IsVisible then
              SelectUpperItem;
          end;
        end;
      vkRight:
        if not Selected.IsExpanded then
          Selected.Expand
        else
          if Selected.FContent.ControlsCount > 0 then
            SelectLowerItem;
    else
      Exit;
    end;
    Key := 0;
  end;
end;

procedure TCustomTreeView.KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  inherited;
end;

function TCustomTreeView.LocalPointToHoveredItemLocalPoint(const AX, AY: Single): TPointF;
begin
  if HoveredItem <> nil then
  begin
    Result := Self.LocalToScreen(PointF(AX, AY));
    Result := HoveredItem.ScreenToLocal(Result);
  end;
end;

procedure TCustomTreeView.DragOver(const Data: TDragObject; const Point: TPointF;
  var Operation: TDragOperation);
var
  Obj: TTreeViewItem;
begin
  inherited ;
  Obj := ItemByPoint(Point.X, Point.Y);
  if Obj <> FDragItem then
  begin
    if FDragItem <> nil then
    begin
      FDragItem.DragLeave;
      FDragItem.RemoveFreeNotify(Self);
    end;
    FDragItem := Obj;
    if FDragItem <> nil then
    begin
      inherited DragLeave;
      FDragItem.AddFreeNotify(Self);
      FDragItem.DragEnter(Data, ConvertLocalPointTo(FDragItem, Point));

      Operation := TDragOperation.Move;
    end
    else
    begin
      DragEnter(Data, Point);
      Operation := TDragOperation.None;
    end;
  end
  else
    Operation := TDragOperation.Move;

  if (Data.Source is TTreeViewItem) and (TTreeViewItem(Data.Source) = FDragItem) then
    Operation := TDragOperation.None;
end;

procedure TCustomTreeView.DragDrop(const Data: TDragObject; const Point: TPointF);
var
  Allow: Boolean;
  WasSelected: Boolean;
  SourceItem, DestItem: TTreeViewItem;
begin
  if FDragItem <> nil then
  begin
    FDragItem.DragLeave;
    FDragItem.RemoveFreeNotify(Self);
    FDragItem := nil;
  end;
  DestItem := ItemByPoint(Point.X, Point.Y);
  if (Data.Source is TTreeViewItem) and IsChild(TTreeViewItem(Data.Source)) then
  begin
    SourceItem := TTreeViewItem(Data.Source);
    Allow := True;
    if Assigned(OnDragChange) then
      OnDragChange(SourceItem, DestItem, Allow);
    if Allow and not SourceItem.IsChild(DestItem) then
    begin
      WasSelected := SourceItem.FIsSelected;
      if DestItem <> nil then
      begin
        SourceItem.Parent := DestItem;
        DestItem.Expand;
      end
      else
      begin
        SourceItem.Parent := Self;
        SourceItem.Expand;
      end;
      SourceItem.IsSelected := WasSelected;
    end;
  end
  else if DestItem <> nil then
    DestItem.DragDrop(Data, ConvertLocalPointTo(DestItem, Point))
  else
    inherited DragDrop(Data, Point);
end;

procedure TCustomTreeView.DragLeave;
begin
  inherited;
  if FDragItem <> nil then
  begin
    FDragItem.DragLeave;
    FDragItem.RemoveFreeNotify(Self);
    FDragItem := nil;
  end;
end;

procedure TCustomTreeView.MouseClick(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  P: TPointF;
begin
  inherited;
  if (HoveredItem <> nil) and MouseInContentLayout(X, Y) then
  begin
    P := LocalPointToHoveredItemLocalPoint(X, Y);
    HoveredItem.MouseClick(Button, Shift, P.X, P.Y);
  end;
end;

procedure TCustomTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);

  procedure TryMakeMultiSelect;
  var
    Item: TTreeViewItem;
  begin
    Item := ItemByPoint(X, Y);
    if Item = nil then
      Exit;

    if ([ssCtrl, ssCommand] * Shift) <> [] then
      Item.IsSelected := not Item.IsSelected
    else if ssShift in Shift then
    begin
      SelectRange(Selected, Item);
      Selected := Item;
    end
    else
    begin
      SelectRange(Item, Item);
      Selected := Item;
    end;
    FFirstSelect := Item;
  end;

  procedure TryMakeSingleSelectAndStartDrag;
  var
    Item: TTreeViewItem;
  begin
    Item := ItemByPoint(X, Y);
    if Item = nil then
      Exit;
    Selected := Item;
    if AllowDrag then
      FStartDrag := True;
  end;

var
  P: TPointF;
  IsMouseInContent: Boolean;
begin
  // It's important to make selection before base mouse down processing. Because developer can remove nodes from this
  // event handler. In this case, if he removes selected items, new node on old place will be repeatedly selected again.
  IsMouseInContent := MouseInContentLayout(X, Y);
  if IsMouseInContent then
  begin
    // Making selection of Items
    if Button = TMouseButton.mbLeft then
    begin
      if MultiSelect then
        TryMakeMultiSelect
      else
        TryMakeSingleSelectAndStartDrag;
      FMouseSelecting := True;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  // Transfer Click to Hovered Item
  if IsMouseInContent and (HoveredItem <> nil) then
  begin
    P := LocalPointToHoveredItemLocalPoint(X, Y);
    HoveredItem.MouseDown(Button, Shift, P.X, P.Y);
  end;
end;

function TCustomTreeView.MouseInContentLayout(const X, Y: Single): Boolean;
var
  P: TPointF;
begin
  Result := False;
  if ContentLayout <> nil then
  begin
    P := TPointF.Create(X, Y);
    P := LocalToAbsolute(P);
    Result := ContentLayout.AbsoluteRect.Contains(P);
  end;
end;

procedure TCustomTreeView.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Item: TTreeViewItem;
  P: TPointF;
  LBitMap: TBitmap;
begin
  inherited MouseMove(Shift, X, Y);
  if MouseInContentLayout(X, Y) then
  begin
    Item := ItemByPoint(X, Y);
    //! Launcher Fix
    /// There is a case when Selected can be equal to nil
    /// by doing additional check here, we are
    /// saving ourselves from yet another AV error
    if AllowDrag and FStartDrag and (Selected <> nil) then
      begin
        FStartDrag := False;
        LBitMap := Selected.MakeScreenshot;
        try
          Root.BeginInternalDrag(Selected, LBitMap);
        finally
          LBitMap.Free;
        end;
      end;
    SetHoveredItem(Item);
    if HoveredItem <> nil then
    begin
      P := LocalPointToHoveredItemLocalPoint(X, Y);
      HoveredItem.MouseMove(Shift, P.X, P.Y);
    end;
  end;
end;

procedure TCustomTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
begin
  inherited MouseUp(Button, Shift, X, Y);
  FFirstSelect := nil;
  FMouseSelecting := False;
  if (HoveredItem <> nil) and MouseInContentLayout(X, Y) then
  begin
    P := LocalPointToHoveredItemLocalPoint(X, Y);
    HoveredItem.MouseUp(Button, Shift, P.X, P.Y);
  end;
  FStartDrag := False;
end;

procedure TCustomTreeView.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;
  if HoveredItem <> nil then
    HoveredItem.MouseWheel(Shift, WheelDelta, Handled);
end;

procedure TCustomTreeView.ChangeOrder;
begin
  inherited ChangeOrder;
  InvalidateContentSize;
  if not IsUpdating then
    RealignContent;
end;

procedure TCustomTreeView.Clear;
var
  i: Integer;
  TmpObj: TFmxObject;
begin
  BeginUpdate;
  try
    FToInflate.Clear;
    if (Content <> nil) and (Count > 0) then
      for i := Content.ControlsCount - 1 downto 0 do
        if Content.Controls[i] is TTreeViewItem then
        begin
          TmpObj := Content.Controls[i];
          TmpObj.DisposeOf;
        end;
    FSelected := nil;
    FFirstVisibleItem := 0;
    FLastVisibleItem := 0;
    UpdateGlobalIndexes;
    UpdateSelection;
  finally
    EndUpdate;
  end;
end;

procedure TCustomTreeView.SelectRange(const Item1, Item2: TTreeViewItem);
var
  i: Integer;
  FirstItemIndex: Integer;
  LastItemIndex: Integer;
begin
  if (Item1 = nil) or (Item2 = nil) then
    Exit;
  FirstItemIndex := Min(Item1.GlobalIndex, Item2.GlobalIndex);
  LastItemIndex := Max(Item1.GlobalIndex, Item2.GlobalIndex);

  for i := 0 to FirstItemIndex - 1 do
    ItemByGlobalIndex(i).Deselect;
  for i := FirstItemIndex to LastItemIndex do
    ItemByGlobalIndex(i).Select;
  for i := LastItemIndex + 1 to GlobalCount - 1 do
    ItemByGlobalIndex(i).Deselect;
end;

procedure TCustomTreeView.ClearSelection;
var
  Item: TTreeViewItem;
begin
  for Item in FGlobalList do
    Item.IsSelected := False;
end;

procedure TCustomTreeView.SelectAll;
var
  Item: TTreeViewItem;
begin
  for Item in FGlobalList do
    Item.IsSelected := True;
end;

procedure TCustomTreeView.DoChange;
begin
  if Assigned(FOnChange) and not (csLoading in ComponentState)  then
    FOnChange(Self);
end;

procedure TCustomTreeView.DoChangeCheck(const Item: TTreeViewItem);
begin
  if Assigned(FOnChangeCheck) then
    FOnChangeCheck(Item);
end;

procedure TCustomTreeView.DoContentPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  I: Integer;
  Item: TTreeViewItem;
  P: TPointF;
  R: TRectF;
begin
  if (Content <> nil) and (ContentLayout <> nil) then
  begin
    if FAlternatingRowBackground then
    begin
      Canvas.Fill.Assign(FOddFill);
      for I := FFirstVisibleItem to FLastVisibleItem  do
      begin
        if Odd(I) then
        begin
          Item := ItemByGlobalIndex(I);
          P := TControl(Sender).ConvertLocalPointFrom(Item, TPointF.Zero);
          R := RectF(0, P.Y, ContentLayout.Width, P.Y + Item.Height);
          if not IntersectRect(R, ARect) then
            Continue;
          Canvas.FillRect(R, 0, 0, [], AbsoluteOpacity);
        end;
      end;
    end;
  end;
end;

procedure TCustomTreeView.DoEnter;
begin
  inherited DoEnter;
  if Selected <> nil then
    UpdateSelection;
end;

procedure TCustomTreeView.DoExit;
begin
  inherited DoExit;
  if Selected <> nil then
    UpdateSelection;
end;

procedure TCustomTreeView.DoInsertObject(Index: Integer;  const AObject: TFmxObject);
begin
  if (Content <> nil) and (AObject is TTreeViewItem) then
  begin
    if FUpdating > TTreeViewItem(AObject).FUpdating then
      TTreeViewItem(AObject).BeginUpdate;
    Content.InsertObject(Index, AObject);
    if [csLoading, csDestroying] * ComponentState = [] then
      TTreeViewItem(AObject).SetImages(Images);
  end
  else
    inherited DoInsertObject(Index, AObject);
end;

procedure TCustomTreeView.DoAddObject(const AObject: TFmxObject);
begin
  if (Content <> nil) and (AObject is TTreeViewItem) then
  begin
    if FUpdating > TTreeViewItem(AObject).FUpdating then
      TTreeViewItem(AObject).BeginUpdate;
    Content.AddObject(AObject);
    if [csLoading, csDestroying] * ComponentState = [] then
      TTreeViewItem(AObject).SetImages(Images);
  end
  else
    inherited DoAddObject(AObject);
end;

procedure TCustomTreeView.DoRemoveObject(const AObject: TFmxObject);
begin
  if (AObject is TTreeViewItem) and (TTreeViewItem(AObject).TreeView = Self) then
  begin
    TTreeViewItem(AObject).Parent := nil;
    if [csLoading, csDestroying] * ComponentState = [] then
      TTreeViewItem(AObject).SetImages(nil);
  end
  else
    inherited DoRemoveObject(AObject);
end;

procedure TCustomTreeView.DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations);
begin
  inherited;
  AAniCalculations.AutoShowing := False;
  AAniCalculations.Animation := False;
  AAniCalculations.BoundsAnimation := False;
end;

function TCustomTreeView.IsAddToContent(const AObject: TFmxObject): Boolean;
begin
  Result := inherited IsAddToContent(AObject) and (AObject <> FNoItemsContent);
end;

function TCustomTreeView.IsOpaque: Boolean;
begin
  Result := True;
end;

procedure TCustomTreeView.ContentAddObject(const AObject: TFmxObject);
begin
  if not (AObject is TTreeViewItem) then
  begin
    if FNoItemsContent = nil then
    begin
      FNoItemsContent := TContent.Create(Self);
      FNoItemsContent.Parent := Self;
      FNoItemsContent.SetBounds(0, 0, 0, 0);
      FNoItemsContent.Stored := False;
      FNoItemsContent.Locked := True;
      FNoItemsContent.HitTest := False;
    end;
    FNoItemsContent.AddObject(AObject)
  end
  else
  begin
    InvalidateContentSize;
    InvalidateGlobalList;
    if not TTreeViewItem(AObject).IsInflated then
      FToInflate.Add(TTreeViewItem(AObject));
    if not IsUpdating then
      RealignContent;
  end;
end;

procedure TCustomTreeView.ContentInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  ContentAddObject(AObject);
end;

procedure TCustomTreeView.ContentRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TTreeViewItem and not IsUpdating then
  begin
    Realign;
    UpdateVisibleItems;
    UpdateSelection;
  end;
end;

procedure TCustomTreeView.ContentBeforeRemoveObject(AObject: TFmxObject);
begin
  inherited;
  if AObject is TTreeViewItem then
  begin
    TTreeViewItem(AObject).FIsSelected := False;
    if AObject = FSelected then
      FSelected := nil;
    InvalidateContentSize;
    InvalidateGlobalList;
    FToInflate.Remove(TTreeViewItem(AObject));
  end;
end;

procedure TCustomTreeView.SetSelected(const Value: TTreeViewItem);
var
  LFmxObject: TFmxObject;
  P: TPointF;
begin
  if (FSelected <> Value) and not FSelectionChanging then
  begin
    FSelectionChanging := True;
    try
      if (FSelected <> nil) and not MultiSelect then
        FSelected.IsSelected := False;

      FSelected := Value;
      if (FSelected <> nil) and (Content <> nil) then
      begin
        LFmxObject := FSelected.Parent;
        while ((LFmxObject <> nil) and not (LFmxObject is TCustomTreeView)) do
        begin
          if LFmxObject is TTreeViewItem then
            TTreeViewItem(LFmxObject).Expand;
          LFmxObject := LFmxObject.Parent;
        end;
        if (Content <> nil) and (ContentLayout <> nil) and (VScrollBar <> nil) then
        begin
          P := ContentLayout.ConvertLocalPointFrom(FSelected, TPointF.Zero);
          if P.Y < 0 then
            VScrollBar.Value := VScrollBar.Value + P.Y;
          if P.Y + FSelected.Margins.Top + FSelected.Margins.Bottom + FSelected.Height > ContentLayout.Height then
            VScrollBar.Value := VScrollBar.Value + (P.Y + FSelected.Margins.Top + FSelected.Margins.Bottom +
              FSelected.Height - ContentLayout.Height);
        end;
        FSelected.IsSelected := True;
      end;
    finally
      FSelectionChanging := False;
    end;
    DoChange;
  end;
end;

procedure TCustomTreeView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FSelected then
      FSelected := nil;
    if AComponent = FDragItem then
      FDragItem := nil;
    if AComponent = FHoveredItem then
      FHoveredItem := nil;
    if AComponent = FFirstSelect then
      FFirstSelect := nil;
  end;
end;

procedure TCustomTreeView.NotifyInflated;
begin
  RealignContent;
end;

procedure TCustomTreeView.SetItemHeight(const Value: Single);
var
  OldSetItemHeight, NewSetItemHeight: Boolean;
  I: Integer;
  procedure ClearHeight(Item: TTreeViewItem);
  var
    I: Integer;
  begin
    Item.Height := 0;
    for I := 0 to Item.Count - 1 do
      ClearHeight(Item.Items[I]);
  end;
begin
  if FItemHeight <> Value then
  begin
    OldSetItemHeight := FItemHeight > 0;
    FItemHeight := Value;
    NewSetItemHeight := FItemHeight > 0;
    if (OldSetItemHeight <> NewSetItemHeight) and (not NewSetItemHeight) then
    begin
      BeginUpdate;
      try
        for I := 0 to Count - 1 do
          ClearHeight(Items[I]);
      finally
        EndUpdate
      end;
    end;
    RealignContent;
  end;
end;

procedure TCustomTreeView.CollapseAll;
var
  i: Integer;
  Item: TTreeViewItem;
begin
  BeginUpdate;
  try
    for i := Count - 1 downto 0 do
    begin
      Item := Items[i];
      if Item <> nil then
        Item.CollapseAll;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomTreeView.ExpandAll;
var
  i: Integer;
  Item: TTreeViewItem;
begin
  BeginUpdate;
  try
    for i := Count - 1 downto 0 do
    begin
      Item := Items[i];
      if Item <> nil then
        Item.ExpandAll;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomTreeView.SetCheckboxesVisibility(const Value: Boolean);
var
  i: Integer;
  Item : TTreeViewItem;
begin
  if FShowCheckboxes <> Value then
  begin
    FShowCheckboxes := Value;
    for i := 0 to Count - 1 do
    begin
      Item := Items[i];
      if Item <> nil then
        Item.UpdateCheckBoxVisibility;
    end;
    RealignContent;
  end;
end;

function TCustomTreeView.GetTreeItem(Index: Integer): TTreeViewItem;
begin
  Result := ItemByIndex(Index);
end;

procedure TCustomTreeView.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    RealignContent;
  end;
end;

procedure TCustomTreeView.SetAllowDrag(const Value: Boolean);
begin
  if FAllowDrag <> Value then
  begin
    FAllowDrag := Value;
    if FAllowDrag then
      EnableDragHighlight := True;
  end;
end;

procedure TCustomTreeView.SetAlternatingRowBackground(const Value: Boolean);
begin
  if FAlternatingRowBackground <> Value then
  begin
    FAlternatingRowBackground := Value;
    Repaint;
  end;
end;

procedure TCustomTreeView.SetHoveredItem(const Value: TTreeViewItem);
begin
  if FHoveredItem <> Value then
  begin
    try
      if FHoveredItem <> nil then
      begin
        TComponent(FHoveredItem).RemoveFreeNotification(Self);
        FHoveredItem.DoMouseLeave;
      end;
    finally
      FHoveredItem := Value;
      if FHoveredItem <> nil then
      begin
        TComponent(FHoveredItem).FreeNotification(Self);
        FHoveredItem.DoMouseEnter;
      end;
    end;
  end;
end;

function TCustomTreeView.GetCount: Integer;
begin
  Result := 0;
  if Content <> nil then
    Result := Content.ControlsCount;
end;

function TCustomTreeView.GetGlobalCount: Integer;
begin
  UpdateGlobalIndexes(False);
  Result := FGlobalList.Count;
end;

procedure TCustomTreeView.InvalidateGlobalList;
begin
  FGlobalList.Clear;
end;

procedure TCustomTreeView.ItemExpanded(const Item: TTreeViewItem);
var
  I: Integer;
  Child: TTreeViewItem;
begin
  InvalidateGlobalList;
  if Item.IsExpanded then
    for I := 0 to Item.Count - 1 do
    begin
      Child := Item.Items[I];
      if not Child.IsInflated then
        Child.Inflate;
    end;
  RealignContent;
end;

function TCustomTreeView.GetImages: TCustomImageList;
begin
  Result := TCustomImageList(FImageLink.Images);
end;

procedure TCustomTreeView.SetImages(const Value: TCustomImageList);
begin
  FImageLink.Images := Value;
end;

function TCustomTreeView.GetImageIndex: TImageIndex;
begin
  Result := -1;
end;

procedure TCustomTreeView.SetImageIndex(const Value: TImageIndex);
begin
  // none
end;

function TCustomTreeView.GetImageList: TBaseImageList;
begin
  Result := GetImages;
end;

procedure TCustomTreeView.Loaded;
begin
  inherited;
  ImagesChanged;
end;

procedure TCustomTreeView.ImagesChanged;
var
  I: Integer;
begin
  if (FImages <> FImageLink.Images) and ([csDestroying, csLoading] * ComponentState = []) then
  begin
    FImages := TCustomImageList(FImageLink.Images);
    BeginUpdate;
    try
      for I := 0 to Count - 1 do
        Items[I].SetImages(FImages);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomTreeView.SetImageList(const Value: TBaseImageList);
begin
  ValidateInheritance(Value, TCustomImageList);
  SetImages(TCustomImageList(Value));
end;

initialization
  RegisterFmxClasses([TTreeView, TTreeViewItem]);
end.
