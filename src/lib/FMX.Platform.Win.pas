{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2021 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Win;

(*$HPPEMIT '#if defined(WIN32) && defined(CreateWindow)'*)
(*$HPPEMIT '  #define __SAVE_CREATEWINDOW CreateWindow'*)
(*$HPPEMIT '  #undef  CreateWindow'*)
(*$HPPEMIT '#endif'*)

(*$HPPEMIT END '#if defined(__SAVE_CREATEWINDOW)'*)
(*$HPPEMIT END '  #define CreateWindow __SAVE_CREATEWINDOW'*)
(*$HPPEMIT END '  #undef  __SAVE_CREATEWINDOW'*)
(*$HPPEMIT END '#endif'*)

{$HPPEMIT NOUSINGNAMESPACE}
{$R-}

interface

{$SCOPEDENUMS ON}

uses
  Winapi.CommCtrl, Winapi.Windows, Winapi.ActiveX, System.Types, System.Classes, System.UITypes, System.UIConsts,
  System.Generics.Collections, FMX.Forms, FMX.Platform, FMX.Types, FMX.Graphics, FMX.ZOrder.Win;

type
  TWinDropTarget = class;

  PRgnRects = ^TRgnRects;
  TRgnRects = array [0..MaxInt div SizeOf(TRect) - 1] of TRect;

  TUpdateRects = array of TRectF;

  TWinWindowHandle = class(TWindowHandle)
  private class var
    FForcedScale: Single;
  private
    FWnd: HWND;
    FZOrderManager: TWinZOrderManager;
    FBufferBitmap: THandle;
    FBitmapInfo: TBitmapInfo;
    FBufferBits: Pointer;
    FBufferHandle: THandle;
    FBufferSize: TSize;
    FForm: TCommonCustomForm;
    FDisableDeactivate: Boolean;
    FWinDropTarget: TWinDropTarget;
    FCurrentScale: Single;
    FClientSize: TSizeF;
    FWndClientSize: TSize;
    FBounds: TRectF;
    FWndBounds: TRect;
    FNearestIntegerMultiple: Integer;
    procedure UpdateLayer;
    function GetZOrderManager: TWinZOrderManager;
    procedure SetBounds(const Value: TRect);
    procedure SetWndBounds(const Value: TRect);
    procedure SetClientSize(const Value: TSize);
    procedure UpdateClientSize;
    procedure SetWindowSizeByClientSize;
    procedure CalcNearestIntegerMultiple;
    function GetNearestIntegerMultiple: Integer;
  protected
    function GetBounds: TRect; virtual;
    function GetClientSize: TSize; virtual;
    function GetWndBounds: TRect; virtual;
    function GetWndClientSize: TSize; virtual;
    function GetTransparency: Boolean; virtual;
    function GetScale: Single; override;
  public
    constructor Create(const AForm: TCommonCustomForm; const AWnd: HWND);
    destructor Destroy; override;
    class procedure SetForcedScale(NewScale: Single);
    procedure CreateBuffer(const Width, Height: Integer);
    procedure ResizeBuffer(const Width, Height: Integer);
    procedure FreeBuffer;
    /// <summary>Rounds physical window bounds to match scale and logical window bounds.</summary>
    procedure CorrectWindowSize(const WindowPos: PWindowPos);
    procedure ScaleChanged;
    procedure DpiChanged(const NewDpi: Integer);
    /// <summary>Converts physical rect to logical.</summary>
    function WndToForm(const Rect: TRect): TRectF; overload;
    /// <summary>Converts physical rect to logical.</summary>
    function WndToForm(const Rect: TRectF): TRectF; overload;
    /// <summary>Converts logical rect to physical.</summary>
    function FormToWnd(const Rect: TRectF): TRectF;
    property Wnd: HWND read FWnd;
    property Form: TCommonCustomForm read FForm;
    property BufferBits: Pointer read FBufferBits;
    property BufferHandle: THandle read FBufferHandle;
    /// <summary>Allocated buffer size for native window.</summary>
    property BufferSize: TSize read FBufferSize;
    property ZOrderManager: TWinZOrderManager read GetZOrderManager;
    property Transparency: Boolean read GetTransparency;
    /// <summary>Logical form's client size.</summary>
    property ClientSize: TSize read GetClientSize write SetClientSize;
    property NearestIntegerMultiple: Integer read GetNearestIntegerMultiple;
    /// <summary>Logical form bounds.</summary>
    property Bounds: TRect read GetBounds write SetBounds;
    /// <summary>Logical form bounds.</summary>
    property BoundsF: TRectF read FBounds;
    /// <summary>Physical form size.</summary>
    property WndClientSize: TSize read GetWndClientSize;
    /// <summary>Physical form bounds.</summary>
    property WndBounds: TRect read GetWndBounds write SetWndBounds;
  end;

  TWinDropTarget = class(TComponent, IDropTarget)
  private
    Form: TCommonCustomForm;
    function GetDataObject: TDragObject;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
  end;

function FindWindow(Handle: HWND): TCommonCustomForm;
function WindowHandleToPlatform(const AHandle: TWindowHandle): TWinWindowHandle;

function FmxHandleToHWND(const FmxHandle: TWindowHandle): HWND;
function FormToHWND(Form: TCommonCustomForm): HWND;
function ApplicationHWND: HWND;

procedure RegisterCorePlatformServices;
procedure UnregisterCorePlatformServices;

const
  // We cannot change constant values in update, however we reallocated resources. So new values are placed in the comments below.
  IDC_NODROP =    PChar(32767); // -> 32760
  IDC_DRAG   =    PChar(32766); // -> 32759
  IDC_MULTIDRAG = PChar(32763); // -> 32756
  IDC_SQLWAIT =   PChar(32762); // -> 32755

type
  TApplicationHWNDProc = function: HWND;

procedure RegisterApplicationHWNDProc(const Proc: TApplicationHWNDProc);

procedure ShutDown; cdecl;

implementation

{$SCOPEDENUMS OFF}

uses
  System.Messaging, System.IOUtils, Winapi.CommDlg, Winapi.Messages, Winapi.ShlObj, Winapi.MMSystem, Winapi.ShellAPI,
  Winapi.MultiMon, Winapi.Imm, Winapi.UxTheme, Winapi.ShellScaling, System.Variants, System.SysUtils, System.Math,
  System.Math.Vectors, System.StrUtils, System.DateUtils, System.RTLConsts, System.SyncObjs, System.Rtti, System.Devices,
  FMX.Consts, FMX.Menus, FMX.Helpers.Win, FMX.Printer, FMX.Printer.Win, FMX.Dialogs.Win, FMX.Canvas.GDIP, FMX.Canvas.D2D,
  FMX.Context.DX9, FMX.Context.DX11, FMX.Canvas.GPU, FMX.Forms.Border.Win, FMX.Controls.Win, FMX.Gestures.Win,
  FMX.TextLayout, FMX.Text, FMX.Types3D, FMX.VirtualKeyboard, FMX.Controls, FMX.BehaviorManager, FMX.Styles,
  FMX.MultiTouch.Win, FMX.ImgList, FMX.WebBrowser, FMX.Surfaces, FMX.Utils, FMX.KeyMapping, FMX.AcceleratorKey,
  FMX.AcceleratorKey.Win;

type
  EUnavailableMenuId = class(Exception);

  TOpenMenuItem = class(TMenuItem);

  MySTGMEDIUM = record // for compatibility
    Tymed: DWORD;
    Case Integer Of
      0:
        (HBITMAP: HBITMAP; UnkForRelease: Pointer { IUnknown } );
      1:
        (HMETAFILEPICT: THandle);
      2:
        (HENHMETAFILE: THandle);
      3:
        (HGLOBAL: HGLOBAL);
      4:
        (lpszFileName: POleStr);
      5:
        (stm: Pointer { IStream } );
      6:
        (stg: Pointer { IStorage } );
  end;

  { TDropSource }

  TDataObjectInfo = record
    FormatEtc: TFormatEtc;
    StgMedium: TStgMedium;
    OwnedByDataObject: Boolean;
  end;

  TDataObjectInfoArray = array of TDataObjectInfo;

  TDropSource = class(TComponent, IDataObject, IDropSource)
  private
    Data: TDragObject;
    Formats: TDataObjectInfoArray;
    { IDropSource }
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HRESULT; stdcall;
    function GiveFeedback(dwEffect: Longint): HRESULT; stdcall;
    { IDataObject }
    function GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HRESULT; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HRESULT; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc; out FormatEtcout: TFormatEtc): HRESULT; stdcall;
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: BOOL): HRESULT; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out EnumFormatEtc: IEnumFormatEtc): HRESULT; stdcall;
    function dAdvise(const FormatEtc: TFormatEtc; advf: Longint; const advsink: IAdviseSink;
      out dwConnection: Longint): HRESULT; stdcall;
    function dUnadvise(dwConnection: Longint): HRESULT; stdcall;
    function EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT; stdcall;
    { For IDropSourceHelper }
    function FindFormatEtc(TestFormatEtc: TFormatEtc): Integer;
    function EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
    function HGlobalClone(HGLOBAL: THandle): THandle;
    function RetrieveOwnedStgMedium(Format: TFormatEtc; var StgMedium: TStgMedium): HRESULT;
    function StgMediumIncRef(const InStgMedium: TStgMedium; var OutStgMedium: TStgMedium;
      CopyInMedium: Boolean): HRESULT;
    function CanonicalIUnknown(const TestUnknown: IUnknown): IUnknown;
  end;

type
  { TPlatformWin }

  TFullScreenParams = record
    BorderStyle: TFmxFormBorderStyle;
    WindowState: TWindowState;
    Position: TPoint;
    Size: TPoint;
  public
    function IsFullScreen: Boolean;
    procedure Clean;
  end;

  TFormInfo = class
    WasLeftMouseButtonPressed: Boolean;
  end;

  TWinTimerService = class;
  TImmManager = class;

  TPlatformWin = class(TInterfacedObject, IFMXApplicationService, IFMXSystemFontService,
    IFMXWindowService, IFMXDragDropService, IFMXCursorService, IFMXMouseService,
    IFMXScreenService, IFMXLocaleService, IFMXTextService, IFMXContextService, IFMXCanvasService, IFMXDeviceService,
    IFMXWindowBorderService, IFMXSystemInformationService, IFMXLoggingService, IFMXFullScreenWindowService,
    IFMXListingService, IFMXSaveStateService, IFMXDeviceMetricsService, IFMXGestureRecognizersService,
    IFMXWindowsTouchService, IFMXDefaultMetricsService, IFMXKeyMappingService)
  private const
    DefaultWindowsFontSize = 12;
  private
    FTitle: string;
    FDefaultTitle: string;
    FApplicationHWNDProc: TApplicationHWNDProc;
    FApplicationHWND: HWND;
    FIsOutApplicationHWND: Boolean;
    FFullScreenSupport: TDictionary<TCommonCustomForm, TFullScreenParams>;
    FDiableUpdateState: Boolean;
    FThreadSyncHandle: HWND;
    FInPaintUpdateRects: TDictionary<TWindowHandle, TUpdateRects>;
    FTerminating: Boolean;
    FRunning: Boolean;
    FCursor: TCursor;
    FCaptionChangedId: Integer;
    FMultiTouchManager: TMultiTouchManagerWin;
    FEnabledInteractiveGestures: TInteractiveGestures;
    FSaveStateStoragePath: string;
    FDragAndDropActive: Boolean;
    FKeyMapping: TKeyMapping;
    FAcceleratorKeyRegistry: IFMXAcceleratorKeyRegistryService;
    FIsPostQuitMessage: Boolean;
    FTimerService: TWinTimerService;
    FFormsInfo: TObjectDictionary<TCommonCustomForm, TFormInfo>;
    { IMM }
    FImmManagers: TObjectDictionary<TCommonCustomForm, TImmManager>;
    procedure ThreadSync(var Msg: TMessage);
    procedure WakeMainThread(Sender: TObject);
    function CreateAppHandle: HWND;
    procedure MinimizeApp;
    procedure RestoreApp;
    // IFMXListingService
    function GetListingHeaderBehaviors: TListingHeaderBehaviors;
    function GetListingSearchFeatures: TListingSearchFeatures;
    function GetListingTransitionFeatures: TListingTransitionFeatures;
    function GetListingEditModeFeatures: TListingEditModeFeatures;
    function IFMXListingService.GetHeaderBehaviors = GetListingHeaderBehaviors;
    function IFMXListingService.GetSearchFeatures = GetListingSearchFeatures;
    function IFMXListingService.GetTransitionFeatures = GetListingTransitionFeatures;
    function IFMXListingService.GetEditModeFeatures = GetListingEditModeFeatures;
    // IFMXSaveStateService
    function GetSaveStateFileName(const ABlockName: string): string;
    function GetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;
    function SetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;
    function GetSaveStateStoragePath: string;
    procedure SetSaveStateStoragePath(const ANewPath: string);
    function GetSaveStateNotifications: Boolean;
    function IFMXSaveStateService.GetBlock = GetSaveStateBlock;
    function IFMXSaveStateService.SetBlock = SetSaveStateBlock;
    function IFMXSaveStateService.GetStoragePath = GetSaveStateStoragePath;
    procedure IFMXSaveStateService.SetStoragePath = SetSaveStateStoragePath;
    function IFMXSaveStateService.GetNotifications = GetSaveStateNotifications;
    // IFMXDeviceMetricsService
    function GetDisplayMetrics: TDeviceDisplayMetrics;
    procedure UpdateAppTitle;
    procedure CaptionChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    function GetApplicationHWND: HWND;
    procedure SetApplicationHWNDProc(const Value: TApplicationHWNDProc);
    procedure UpdateApplicationHwnd;

    //IFMXKeyMappingService
    /// <summary>Registers a platform key as the given virtual key.</summary>
    function RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
    /// <summary>Unegisters a platform key as the given virtual key.</summary>
    function UnregisterKeyMapping(const PlatformKey: Word): Boolean;
    /// <summary>Obtains the virtual key from a given platform key.</summary>
    function PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
    /// <summary>Obtains the platform key from a given virtual key.</summary>
    function VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
    function GetImmManager(const Index: TCommonCustomForm): TImmManager;
    function GetFormInfo(const Index: TCommonCustomForm): TFormInfo;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXApplicationService }
    procedure Run;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetDefaultTitle: string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function Terminating: Boolean;
    function Running: Boolean;
    procedure Terminate;
    function GetVersionString: string;
    property ApplicationHWND: HWND read GetApplicationHWND;
    property ApplicationHWNDProc: TApplicationHWNDProc read FApplicationHWNDProc write SetApplicationHWNDProc;
    { IFMXSystemFontService }
    function GetDefaultFontFamilyName: string;
    function GetDefaultFontSize: Single;
    { IFMXWindowService }
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
    procedure DestroyWindow(const AForm: TCommonCustomForm);
    procedure ReleaseWindow(const AForm: TCommonCustomForm);
    procedure SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
    procedure ShowWindow(const AForm: TCommonCustomForm);
    procedure HideWindow(const AForm: TCommonCustomForm);
    procedure BringToFront(const AForm: TCommonCustomForm);
    procedure SendToBack(const AForm: TCommonCustomForm);
    procedure Activate(const AForm: TCommonCustomForm);
    function ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
    function CanShowModal: Boolean;
    procedure InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
    procedure InvalidateImmediately(const AForm: TCommonCustomForm);
    procedure SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
    function GetWindowRect(const AForm: TCommonCustomForm): TRectF;
    function GetClientSize(const AForm: TCommonCustomForm): TPointF;
    procedure SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
    procedure SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
    procedure SetCapture(const AForm: TCommonCustomForm);
    procedure ReleaseCapture(const AForm: TCommonCustomForm);
    function ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function GetWindowScale(const AForm: TCommonCustomForm): Single;
    // for desingtime and testing only
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
    { IFMXWindowBorderService }
    function CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
    { IFMXDragDropService }
    procedure BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);
    { IFMXCursorService }
    procedure SetCursor(const ACursor: TCursor);
    function GetCursor: TCursor;
    { IFMXMouseService }
    function GetMousePos: TPointF;
    { IFMXScreenService }
    function GetScreenSize: TPointF;
    function GetScreenScale: Single;
    function GetScreenOrientation: TScreenOrientation;
    procedure SetSupportedScreenOrientations(const AOrientations: TScreenOrientations);
    { IFMXLocaleService }
    function GetCurrentLangID: string;
    function GetLocaleFirstDayOfWeek: string;
    function GetFirstWeekday: Byte;
    { IFMXTextService }
    function GetTextServiceClass: TTextServiceClass;
    { IFMXContextService }
    procedure RegisterContextClasses;
    procedure UnregisterContextClasses;
    { IFMXCanvasService }
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
    { IFMXSystemInformationService }
    function GetScrollingBehaviour: TScrollingBehaviours;
    function GetMinScrollThumbSize: Single;
    function GetCaretWidth: Integer;
    function GetMenuShowDelay: Integer;
    { IFMXLoggingService }
    procedure Log(const AFormat: string; const AParams: array of const);
    { IFMXGestureRecognizersService }
    procedure AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    { IFMXWindowsTouchService }
    procedure HookTouchHandler(const AForm: TCommonCustomForm);
    procedure UnhookTouchHandler(const AForm: TCommonCustomForm);
    { IFMXDeviceService }
    function GetModel: string;
    function GetFeatures: TDeviceFeatures;
    function GetDeviceClass: TDeviceInfo.TDeviceClass;
    { IFMXDefaultMetricsService }
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    function GetDefaultSize(const AComponent: TComponentKind): TSize;

    property ImmManager[const Index: TCommonCustomForm]: TImmManager read GetImmManager;
    property TimerService: TWinTimerService read FTimerService;
    /// <summary>Returns additional platform dependent data about fmx form.</summary>
    property FormInfo[const Index: TCommonCustomForm]: TFormInfo read GetFormInfo;
  end;

  TWin32TimerInfo = record
    TimerID: UIntPtr; // the Windows timer ID for this timer
    TimerHandle: TFmxHandle; // the unique FMX Handle for this timer
    TimerFunc: TTimerProc; // owner function to handle timer
  end;

  TWinTimerService = class(TInterfacedObject, IFMXTimerService)
  private
    FHandleCounter: TFmxHandle;
    FTimers: TList<TWin32TimerInfo>;
    FPerformanceFrequency: Int64;
    FTerminating: Boolean;
    procedure DestroyTimers;
    { Handlers }
    class procedure TimerCallback(window_hwnd: HWND; Msg: Longint; idEvent: UINT; dwTime: Longint); static; stdcall;
    procedure ApplicationTerminatingHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXTimerService }
    function CreateTimer(AInterval: Integer; ATimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(Timer: TFmxHandle): Boolean;
    function GetTick: Double;
  end;

  TMenuLooperWin = class
  private
    FView: IMenuView;
    function IsItemSelectable(const Item: TFmxObject): Boolean;
    procedure SelectFirstMenuItem(const AView: IMenuView);
    procedure SelectLastMenuItem(const AView: IMenuView);
    procedure SelectNextMenuItem(const AView: IMenuView; const ABackward: Boolean);
    function BackwardSelectNextMenuItem(const AView: IMenuView; const AStartInd, AEndInd: Integer): Boolean;
    function ForwardSelectNextMenuItem(const AView: IMenuView; const AStartInd, AEndInd: Integer): Boolean;
  public
    procedure StartLoop(const AView: IMenuView);
    procedure EndLoop(const AView: IMenuView);
  end;

  TWin32MenuInfo = record
    MenuID: Integer;
    FMXMenuItem: TMenuItem;
    constructor Create(const AMenuId: Integer; const AnItem: TMenuItem);
  end;

  TMenuServiceWin = class(TInterfacedObject, IFMXMenuService)
  private type
    TState = (CreatingOSMenu, DestroyingMenuItem);
    TStates = set of TState;
    TMenuId = Integer;
  private
    FHMenuMap: TDictionary<TFmxHandle, TWin32MenuInfo>;
    FHMenuIdMap: TDictionary<TMenuId, TFmxHandle>;
    FStates: TStates;
    FMenuLooper: TMenuLooperWin;
    { Menu Item Id}
    function GenerateMenuId: TMenuId;
    function AssignNewIdToMenu(const AParentMenu, AMenu: HMENU): TMenuId;
    function FindMenuInfoById(const AMenuItemId: TMenuId; var AMenuInfo: TWin32MenuInfo): Boolean;
    { Removing }
    procedure DestroysAllMenuItems(const AMenu: IItemsContainer);
    procedure RemoveMenuFromMaps(const AMenuHandle: TFmxHandle);
    { Menu Item Bitmap }
    procedure AddBitmapToMenu(const AParentMenu: HMENU; const AMenuItemId: TMenuId; const ABitmap: HBITMAP);
    procedure RemoveBitmapFromMenu(const AParentMenu, AMenu: HMENU);
  protected
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMInitMenuPopup(var Message: TWMInitMenuPopup); message WM_INITMENUPOPUP;
    procedure WMMenuSelect(var Message: TWMMenuSelect); message WM_MENUSELECT;
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXMenuService }

    procedure StartMenuLoop(const AView: IMenuView);
    function ShortCutToText(ShortCut: TShortCut): string;
    procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
    function TextToShortCut(Text: string): Integer;
    procedure CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);
    procedure UpdateMenuItem(const AItem: IItemsContainer; AChange: TMenuItemChanges);
    procedure DestroyMenuItem(const AItem: IItemsContainer);
    function IsMenuBarOnWindowBorder: Boolean;
    procedure UpdateMenuBar;
  end;

  TVirtualKeyboardWin = class(TInterfacedObject, IFMXVirtualKeyboardService)
  private type
    TvkbState = (None, Hidden, Shown);
  private
    FPath: string;
    FExeName: string;
    FWndClassName: string;
    FHTmerLang: TFmxHandle;
    FHTmerVisible: TFmxHandle;
    FKBPresent: Boolean;
    FFormHandle: HWND;
    FInst: HINST;
    FError: Boolean;
    FLastvkbState: TvkbState;
    FLastHandle: HWND;
    FLastTime: TDateTime;
    FNewvkbState: TvkbState;
    FWait: Boolean;
    FStepActivate: Integer;
    FCodeKeyboard: HKL;
    FTimerService: IFMXTimerService;

    procedure KillTimerLang;
    procedure TimerLangProc;
    procedure StartTimerLang;

    procedure KillTimerVisible;
    procedure TimerVisibleProc;
    procedure StartTimerVisible;
    function FindKeyValue(const Key: HKey; const Name, Value, SubKeyName, SubValueName: string): string;
    function GetVirtualKeyboardState: TVirtualKeyboardStates;
    procedure vkbExecute(FormHandle: HWND);
    function vkbHandle: HWND;
    function vkbState: TvkbState;
    function GetVKBounds: TRect;
  protected
    procedure Clear;
    function IsAutoShow: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
    function HideVirtualKeyboard: Boolean;
    procedure SetTransientState(Value: Boolean);
    property VirtualKeyboardState: TVirtualKeyboardStates read GetVirtualKeyboardState;
    property ExeName: string read FExeName write FExeName;
    property Path: string read FPath write FPath;
    property WndClassName: string read FWndClassName write FWndClassName;
  end;

{ TMultiDisplayWin }

  TOrderedDictionary<TKey, TValue> = class
  private
    FValues: TList<TValue>;
    FIndex: TDictionary<TKey, Integer>;
    function GetCount: Integer;
    function GetValues(const AIndex: Integer): TValue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AKey: TKey; const AValue: TValue);
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    procedure Clear;
    property Values[const AIndex: Integer]: TValue read GetValues; default;
    property Count: Integer read GetCount;
  end;

  TMultiDisplayWin = class(TInterfacedObject, IFMXMultiDisplayService)
  private type
    TParameter = (DisplayCount, Displays, WorkareaRect, DesktopRect);
    TParameters = set of TParameter;
  private
    FDisplays: TOrderedDictionary<HMONITOR, TDisplay>;
    FDisplayCount: Integer;
    FWorkAreaRect: TRect;
    FDesktopRect: TRect;
    FOutdatedParameters: TParameters;
    procedure AddDisplay(const AMonitorHandle: HMONITOR);
    function FindDisplay(const AMonitorHandle: HMONITOR): TDisplay;
    procedure UpdateDisplaysIfNeeded;
    class function ScaleRect(const ARect: TRect; const AScale: Single): TRect;
    class function GetMonitorScale(const AHandle: HMONITOR): Single;
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXMultiDisplayService }
    procedure UpdateDisplayInformation;
    function GetDisplayCount: Integer;
    function GetWorkAreaRect: TRect;
    function GetDesktopRect: TRect;
    function GetDisplay(const AIndex: Integer): TDisplay;
    function GetDesktopCenterRect(const ASize: TSize): TRect;
    function DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;
    function DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay;
  end;

  TOpenForm = class(TCommonCustomForm);
  TTextServiceWin = class;

  TImmManager = class
  private
    [Weak] FForm: TCommonCustomForm;
    function GetFormHandle: HWND;
    procedure UpdateCompositionAttributes(const AContext: HIMC; const ATextService: TTextServiceWin);
    procedure UpdateCompositionCursorPos(const AContext: HIMC; const ATextService: TTextServiceWin);
    procedure ProcessImeParameters(const AContext: HIMC; const AParameters: LPARAM; const ATextService: TTextServiceWin);
  protected
    procedure WMStartComposition(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WMEndComposition(var Message: TMessage); message WM_IME_ENDCOMPOSITION;
    procedure WMSetContext(var Message: TMessage); message WM_IME_SETCONTEXT;
    procedure WMNotify(var Message: TMessage); message WM_IME_NOTIFY;

    function GetComposition(const AContext: HIMC): string;
    function GetResultComposition(const AContext: HIMC): string;
  public
    constructor Create(const AForm: TCommonCustomForm);

    function UpdateIMEWindowPosition: LRESULT;

    property Form: TCommonCustomForm read FForm;
    property FormHandle: HWND read GetFormHandle;
  end;

{ Text Service }

  TTextServiceWin = class(TTextService)
  private const
    LCID_Korean_Default = (SUBLANG_KOREAN shl 10) + LANG_KOREAN;
  private
    FMarkedText: string;
    FIsInputting: Boolean;
    FMarkedTextCursorPosition: Integer;
    procedure SetMarkedTextCursorPosition(const Value: Integer);
    procedure RecreateImmContext(const AFormHandle: TWindowHandle);
    procedure Reset;
  protected
    procedure MarkedTextPositionChanged; override;
    procedure CaretPositionChanged; override;
    function GetMarketTextAttributes: TArray<TMarkedTextAttribute>; override;
  public
    CompAttrBuf: array of Byte;

    procedure InternalSetMarkedText(const AMarkedText: string); override;
    function InternalGetMarkedText: string; override;
    procedure InternalStartIMEInput;
    procedure InternalEndIMEInput;

    procedure RefreshImePosition; override;

    function TargetClausePosition: TPoint; override;

    procedure EnterControl(const AFormHandle: TWindowHandle); override;
    procedure ExitControl(const AFormHandle: TWindowHandle); override;

    { Deprecated }
    procedure DrawSingleLine(const ACanvas: TCanvas; const ARect: TRectF; const AFirstVisibleChar: Integer;
      const AFont: TFont; const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False); overload; override;
    procedure DrawSingleLine(const ACanvas: TCanvas; const S: string; const ARect: TRectF; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False); overload; override;

    function HasMarkedText: Boolean; override;

    /// <summary>Returns caret position in <c>MarkedText</c> bounds.</summary>
    property MarkedTextCursorPosition: Integer read FMarkedTextCursorPosition write SetMarkedTextCursorPosition;
  end;

var
  VirtualKeyboardWin: TVirtualKeyboardWin;
  MultiDisplayWin: TMultiDisplayWin;
  MenuServiceWin: TMenuServiceWin;

const
  CF_FMOBJECT = CF_PRIVATEFIRST + 1;

var
  WindowAtom: TAtom;
  WindowAtomString: string;
  PlatformWin: TPlatformWin;
  CapturedGestureControl: TComponent;

procedure DisableProcessWindowsGhosting;
var
  DisableProcessWindowsGhostingProc: procedure;
begin
  DisableProcessWindowsGhostingProc := GetProcAddress(GetModuleHandle('user32.dll'), 'DisableProcessWindowsGhosting');
  if Assigned(DisableProcessWindowsGhostingProc) then
    DisableProcessWindowsGhostingProc;
end;

procedure RegisterCorePlatformServices;
begin
  PlatformWin := TPlatformWin.Create;
  TPlatformServices.Current.AddPlatformService(IFMXApplicationService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXTimerService, PlatformWin.TimerService);
  TPlatformServices.Current.AddPlatformService(IFMXWindowService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDragDropService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXCursorService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXMouseService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXScreenService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXLocaleService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXTextService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXContextService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXWindowBorderService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXLoggingService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXListingService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXSaveStateService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXGestureRecognizersService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXWindowsTouchService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXKeyMappingService, PlatformWin);

  VirtualKeyboardWin := TVirtualKeyboardWin.Create;
  TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardService, VirtualKeyboardWin);

  MultiDisplayWin := TMultiDisplayWin.Create;
  TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, MultiDisplayWin);

  MenuServiceWin := TMenuServiceWin.Create;
  TPlatformServices.Current.AddPlatformService(IFMXMenuService, MenuServiceWin);

  // If application becomes inactive while a modal dialog is opened it may hang, so we have to DisableProcessWindowsGhosting
  DisableProcessWindowsGhosting;
end;

procedure UnregisterCorePlatformServices;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXMenuService);
  TPlatformServices.Current.RemovePlatformService(IFMXDeviceService);
  TPlatformServices.Current.RemovePlatformService(IFMXDeviceMetricsService);
  TPlatformServices.Current.RemovePlatformService(IFMXApplicationService);
  TPlatformServices.Current.RemovePlatformService(IFMXSystemFontService);
  TPlatformServices.Current.RemovePlatformService(IFMXTimerService);
  TPlatformServices.Current.RemovePlatformService(IFMXWindowService);
  TPlatformServices.Current.RemovePlatformService(IFMXDragDropService);
  TPlatformServices.Current.RemovePlatformService(IFMXCursorService);
  TPlatformServices.Current.RemovePlatformService(IFMXMouseService);
  TPlatformServices.Current.RemovePlatformService(IFMXScreenService);
  TPlatformServices.Current.RemovePlatformService(IFMXLocaleService);
  TPlatformServices.Current.RemovePlatformService(IFMXTextService);
  TPlatformServices.Current.RemovePlatformService(IFMXContextService);
  TPlatformServices.Current.RemovePlatformService(IFMXCanvasService);
  TPlatformServices.Current.RemovePlatformService(IFMXWindowBorderService);
  TPlatformServices.Current.RemovePlatformService(IFMXSystemInformationService);
  TPlatformServices.Current.RemovePlatformService(IFMXFullScreenWindowService);
  TPlatformServices.Current.RemovePlatformService(IFMXVirtualKeyboardService);
  TPlatformServices.Current.RemovePlatformService(IFMXDefaultMetricsService);
  TPlatformServices.Current.RemovePlatformService(IFMXLoggingService);
  TPlatformServices.Current.RemovePlatformService(IFMXListingService);
  TPlatformServices.Current.RemovePlatformService(IFMXSaveStateService);
  TPlatformServices.Current.RemovePlatformService(IFMXGestureRecognizersService);
  TPlatformServices.Current.RemovePlatformService(IFMXWindowsTouchService);
  TPlatformServices.Current.RemovePlatformService(IFMXDefaultMetricsService);
  TPlatformServices.Current.RemovePlatformService(IFMXKeyMappingService);
end;

procedure RaiseIfNil(const AObject: TObject; const AArgumentName: string);
begin
  if AObject = nil then
    raise EArgumentException.CreateFmt(SParamIsNil, [AArgumentName]);
end;

{ TPlatformWin }

constructor TPlatformWin.Create;
begin
  inherited;
  FIsOutApplicationHWND := False;
  WindowAtomString := Format('FIREMONKEY%.8X', [GetCurrentProcessID]);
  WindowAtom := GlobalAddAtomW(PChar(WindowAtomString));

  FFullScreenSupport := TDictionary<TCommonCustomForm, TFullScreenParams>.Create;
  FInPaintUpdateRects := TDictionary<TWindowHandle, TUpdateRects>.Create;
  FThreadSyncHandle := AllocateHWnd(ThreadSync);
  FKeyMapping := TKeyMapping.Create;
  FAcceleratorKeyRegistry := TWinAcceleratorKeyRegistry.Create;
  System.Classes.WakeMainThread := WakeMainThread;
  Application := TApplication.Create(nil);
  FCaptionChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TMainCaptionChangedMessage,
    CaptionChangedHandler);
  FRunning := False;

  FImmManagers := TObjectDictionary<TCommonCustomForm, TImmManager>.Create([doOwnsValues]);
  FFormsInfo := TObjectDictionary<TCommonCustomForm, TFormInfo>.Create([doOwnsValues]);
  FTimerService := TWinTimerService.Create;
end;

destructor TPlatformWin.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMainCaptionChangedMessage, FCaptionChangedId);
  FreeAndNil(FFormsInfo);
  FreeAndNil(FImmManagers);
  FreeAndNil(Application);
  FInPaintUpdateRects.Free;
  FFullScreenSupport.Free;
  FAcceleratorKeyRegistry := nil;
  FTimerService := nil;
  FKeyMapping.Free;
  GlobalDeleteAtom(WindowAtom);

  if FThreadSyncHandle <> 0 then
    DeallocateHWnd(FThreadSyncHandle);
  if PlatformWin = Self then
    PlatformWin := nil;
  inherited;
end;

{ App }

procedure TPlatformWin.Run;
begin
  FRunning := True;
  Application.RealCreateForms;
  UpdateAppTitle;
  repeat
    try
      Application.HandleMessage;
    except
      Application.HandleException(Self);
    end;
  until Application.Terminated;
end;

function TPlatformWin.Terminating: Boolean;
begin
  Result := FTerminating;
end;

function TPlatformWin.Running: Boolean;
begin
  Result := FRunning;
end;

procedure TPlatformWin.Terminate;
begin
  FRunning := False;
  FTerminating := True;
  FIsPostQuitMessage := True;

  FMultiTouchManager.Free; // release multitouch early so that it does not hold reference to services

  // We don't need destroy application handle, if we receive it from out through ApplicationHWNDProc.
  if IsLibrary and FIsOutApplicationHWND and (FApplicationHWND <> 0) then
  begin
    WinApi.Windows.DestroyWindow(FApplicationHWND);
    FApplicationHWND := 0;
  end;
end;

function TPlatformWin.HandleMessage: Boolean;
var
  Msg: TMsg;
begin
  Result := False;
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    Result := True;
    if Msg.Message <> WM_QUIT then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
      if FIsPostQuitMessage then
        PostQuitMessage(0);
    end
    else
      Application.Terminated := True;
  end;
end;

procedure TPlatformWin.WaitMessage;
begin
  Winapi.Windows.WaitMessage;
end;

function TPlatformWin.GetDefaultTitle: string;
begin
  Result := FDefaultTitle;
end;

function TPlatformWin.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TPlatformWin.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    if (Value = SAppDefault) and (FDefaultTitle <> '') then
      FTitle := FDefaultTitle
    else
      FTitle := Value;
    UpdateAppTitle;
  end;
end;

function TPlatformWin.GetVersionString: string;
const
  UndefinedVersionInfo = Cardinal(-1);
var
  VersionInfo: Cardinal;
begin
  VersionInfo := GetFileVersion(ParamStr(0));
  if VersionInfo <> UndefinedVersionInfo then
    Result := Format('%d.%d', [HiWord(VersionInfo), LoWord(VersionInfo)])
  else
    Result := string.Empty;
end;

procedure TPlatformWin.UpdateApplicationHwnd;
var
  OldApplicationHandler: HWND;
begin
  OldApplicationHandler := FApplicationHWND;

  // The first, we try to extract application handle from IDE.
  if Assigned(FApplicationHWNDProc) then
  begin
    FApplicationHWND := FApplicationHWNDProc;
    FIsOutApplicationHWND := FApplicationHWND <> 0;
  end;

  // If IDE or user doesn't set outter application handle, we create our own, if it wasn't created before.
  if FApplicationHWND = 0 then
  begin
    FApplicationHWND := CreateAppHandle;
    FIsOutApplicationHWND := False;
  end;

  if OldApplicationHandler <> FApplicationHWND then
    UpdateAppTitle;
end;

procedure TPlatformWin.UpdateAppTitle;
begin
  if FApplicationHWND <> 0 then
  begin
    { Don't update the title when working in the IDE }
    if (Application <> nil) and (Application.MainForm <> nil) and ((FTitle = SAppDefault) or
      (FTitle = GetDefaultTitle)) then
      SetWindowText(FApplicationHWND, PChar(Application.MainForm.Caption))
    else if FTitle.IsEmpty then
      SetWindowText(FApplicationHWND, PChar(GetDefaultTitle))
    else
      SetWindowText(FApplicationHWND, PChar(FTitle));
  end;
end;

procedure TPlatformWin.CaptionChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  UpdateAppTitle;
end;

procedure TPlatformWin.WakeMainThread(Sender: TObject);
begin
  PostMessage(FThreadSyncHandle, WM_NULL, 0, 0);
end;

{ Text Service }

function TTextServiceWin.GetMarketTextAttributes: TArray<TMarkedTextAttribute>;
var
  I: Integer;
begin
  if FMarkedText.IsEmpty or (Length(CompAttrBuf) = 0) then
  begin
    Result := [];
    Exit;
  end;

  SetLength(Result, FMarkedText.Length);
  for I := 0 to FMarkedText.Length - 1 do
    case CompAttrBuf[I] of
      ATTR_INPUT:
        Result[I] := TMarkedTextAttribute.Input;
      ATTR_TARGET_CONVERTED:
        Result[I] := TMarkedTextAttribute.TargetConverted;
      ATTR_CONVERTED:
        Result[I] := TMarkedTextAttribute.Converted;
      ATTR_TARGET_NOTCONVERTED:
        Result[I] := TMarkedTextAttribute.TargetNotConverted;
      ATTR_INPUT_ERROR:
        Result[I] := TMarkedTextAttribute.InputError;
    end;
end;

procedure TTextServiceWin.SetMarkedTextCursorPosition(const Value: Integer);
begin
  FMarkedTextCursorPosition := Value;
end;

procedure TTextServiceWin.InternalSetMarkedText(const AMarkedText: string);
var
  TextInput: ITextInput;
begin
  if FIsInputting and Supports(Owner, ITextInput, TextInput) then
  begin
    FMarkedText := AMarkedText;
    FMarkedTextCursorPosition := EnsureRange(FMarkedTextCursorPosition, 0, FMarkedText.Length);
    TextInput.IMEStateUpdated;
  end;
end;

procedure TTextServiceWin.InternalStartIMEInput;
var
  TextInput: ITextInput;
begin
  if not FIsInputting and Supports(Owner, ITextInput, TextInput) then
  begin
    FIsInputting := True;
    Reset;
    TextInput.StartIMEInput;
  end;
end;

procedure TTextServiceWin.MarkedTextPositionChanged;
begin
  inherited;
  if FIsInputting then
    RefreshImePosition;
end;

procedure TTextServiceWin.RecreateImmContext(const AFormHandle: TWindowHandle);
var
  IMC: HIMC;
  OldIMC: HIMC;
  WindowHandle: HWND;
begin
  // Our styled controls doesn't have window handle, so form shares IMM context between all text styled controls.
  // It leads to issues in IMM with keeping in memory previous IMM composition string, when user switches focus
  // between out styled text input controls. Even if we rest composition string by ImmSetCompositionString,
  // OS sends WM_IME_COMPOSITION notification message with the previous composition string and this behavior
  // is different, when user uses different IMM provides. So the recreating context  guarantees blank IME context
  // without previous states.

  WindowHandle := WindowHandleToPlatform(AFormHandle).Wnd;
  IMC := ImmCreateContext;
  OldIMC := ImmAssociateContext(WindowHandle, IMC);
  ImmDestroyContext(OldIMC);
  ImmReleaseContext(WindowHandle, IMC);
end;

procedure TTextServiceWin.RefreshImePosition;

  function FindForm: TCommonCustomForm;
  var
    TmpObject: TFmxObject;
  begin
    TmpObject := Owner.GetObject;
    while (TmpObject <> nil) and not (TmpObject is TCommonCustomForm) do
      TmpObject := TmpObject.Parent;

    if TmpObject is TCommonCustomForm then
      Result := TCommonCustomForm(TmpObject)
    else
      Result := nil;
  end;

var
  Form: TCommonCustomForm;
begin
  inherited;
  Form := FindForm;
  if Form <> nil then
    PlatformWin.ImmManager[Form].UpdateIMEWindowPosition;
end;

procedure TTextServiceWin.Reset;
begin
  CompAttrBuf := nil;
  FMarkedText := string.Empty;
  FMarkedTextCursorPosition := 0;
end;

function TTextServiceWin.InternalGetMarkedText: string;
begin
  Result := FMarkedText;
end;

procedure TTextServiceWin.CaretPositionChanged;
begin
  inherited;
  if FIsInputting then
    RefreshImePosition;
end;

function TTextServiceWin.TargetClausePosition: TPoint;
begin
  Result := CaretPosition;
  Result.X := Result.X + MarkedTextCursorPosition;
end;

procedure TTextServiceWin.InternalEndIMEInput;
var
  TextInput: ITextInput;
begin
  if FIsInputting and Supports(Owner, ITextInput, TextInput) then
  begin
    FIsInputting := False;
    FMarkedTextCursorPosition := 0;
    TextInput.EndIMEInput;
    FMarkedText := string.Empty;
  end;
end;

procedure TTextServiceWin.EnterControl(const AFormHandle: TWindowHandle);
var
  WindowHandle: HWND;
begin
  RaiseIfNil(AFormHandle, 'AFormHandle');

  WindowHandle := WindowHandleToPlatform(AFormHandle).Wnd;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWBService) then
    SetFocus(WindowHandle);

  if ImeMode <> TImeMode.imDisable then
    RecreateImmContext(AFormHandle);
  TImeModeHelper.SetIme(WindowHandle, ImeMode);
end;

procedure TTextServiceWin.ExitControl(const AFormHandle: TWindowHandle);
begin
  RaiseIfNil(AFormHandle, 'AFormHandle');

  if ImeMode <> TImeMode.imDisable then
    RecreateImmContext(AFormHandle);

  Reset;
  TImeModeHelper.ResetIme(WindowHandleToPlatform(AFormHandle).Wnd, ImeMode);
end;

procedure TTextServiceWin.DrawSingleLine(const ACanvas: TCanvas; const ARect: TRectF; const AFirstVisibleChar: Integer;
  const AFont: TFont; const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);
var
  S: string;
begin
  S := CombinedText;
  S := S.Substring(AFirstVisibleChar - 1, S.Length - AFirstVisibleChar + 1);
  DrawSingleLine(ACanvas, S, ARect, AFont, AOpacity, AFlags, ATextAlign, AVTextAlign, AWordWrap);
end;

procedure TTextServiceWin.DrawSingleLine(const ACanvas: TCanvas; const S: string; const ARect: TRectF; const Font: TFont;
  const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);

  procedure UnderlineRegion(const ARegions: TRegion);
  var
    I: Integer;
    Region: TRectF;
    HalfThickness: Single;
    StartPoint, EndPoint: TPointF;
  begin
    HalfThickness := ACanvas.Stroke.Thickness / 2;
    for I := Low(ARegions) to High(ARegions) do
    begin
      Region := ACanvas.AlignToPixel(ARegions[I]);

      StartPoint := TPointF.Create(Region.Left, Region.Bottom);
      StartPoint.Offset(-HalfThickness, -HalfThickness);
      EndPoint := Region.BottomRight;
      EndPoint.Offset(-HalfThickness, -HalfThickness);
      ACanvas.DrawLine(StartPoint, EndPoint, AOpacity);
    end;
  end;

  procedure UnderlineMarkedText(const ALayout: TTextLayout; const AAttributes: TArray<TMarkedTextAttribute>);
  var
    SavedState: TCanvasSaveState;
    Region: TRegion;
    TextRange: TTextRange;
    PositionInLine: Integer;
    I: Integer;
  begin
    SavedState := ACanvas.SaveState;
    try
      ACanvas.Stroke.Assign(ACanvas.Fill);
      ACanvas.Stroke.Thickness := 2;
      ACanvas.Stroke.Dash := TStrokeDash.Solid;
      for I := 0 to FMarkedText.Length - 1 do
      begin
        case AAttributes[I] of
          TMarkedTextAttribute.Input:
            begin
              ACanvas.Stroke.Thickness := 1;
              ACanvas.Stroke.Dash := TStrokeDash.Dash
            end;
          TMarkedTextAttribute.TargetConverted:
            begin
              ACanvas.Stroke.Thickness := 2;
              ACanvas.Stroke.Dash := TStrokeDash.Solid;
            end;
          TMarkedTextAttribute.Converted:
            begin
              ACanvas.Stroke.Thickness := 1;
              ACanvas.Stroke.Dash := TStrokeDash.Solid;
            end;
          TMarkedTextAttribute.TargetNotConverted:
            begin
              ACanvas.Stroke.Thickness := 4;
              ACanvas.Stroke.Dash := TStrokeDash.Solid;
            end;
          TMarkedTextAttribute.InputError:
            begin
              ACanvas.Stroke.Thickness := 1;
              ACanvas.Stroke.Dash := TStrokeDash.Dot
            end;
        end;

        // CaretPosition -> MarkedTextPosition
        if CaretPosition.X > S.Length - FMarkedText.Length then
          PositionInLine := S.Length - FMarkedText.Length
        else
          PositionInLine := CaretPosition.X;
        TextRange := TTextRange.Create(PositionInLine + I, 1);

        Region := ALayout.RegionForRange(TextRange);
        UnderlineRegion(Region);
      end;
    finally
      ACanvas.RestoreState(SavedState);
    end;
  end;

var
  Layout: TTextLayout;
  Attributes: TArray<TMarkedTextAttribute>;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(ACanvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    try
      Layout.TopLeft := ARect.TopLeft;
      Layout.MaxSize := TPointF.Create(ARect.Width, ARect.Height);
      Layout.WordWrap := AWordWrap;
      Layout.HorizontalAlign := ATextAlign;
      Layout.VerticalAlign := AVTextAlign;
      Layout.Font := Font;
      Layout.Color := ACanvas.Fill.Color;
      Layout.Opacity := AOpacity;
      Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
      Layout.Text := S;
    finally
      Layout.EndUpdate;
    end;
    Layout.RenderLayout(ACanvas);

    Attributes := GetMarketTextAttributes;
    if not FMarkedText.IsEmpty and (Length(Attributes) > 0) then
      UnderlineMarkedText(Layout, Attributes);
  finally
    FreeAndNil(Layout);
  end;
end;

function TTextServiceWin.HasMarkedText: Boolean;
begin
  Result := not FMarkedText.IsEmpty;
end;

function TPlatformWin.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceWin;
end;

{ Window }

procedure TWinWindowHandle.CalcNearestIntegerMultiple;
const
  MaxMul = 9;
var
  I: Integer;
begin
  if not SameValue(Frac(Scale), 0, TEpsilon.Scale) then
  begin
    FNearestIntegerMultiple := Round(Scale);
    for I := 2 to MaxMul do
      if SameValue(Frac(Scale * I), 0, TEpsilon.Scale) then
      begin
        FNearestIntegerMultiple := Trunc(Scale * I);
        Break;
      end;
  end
  else
    FNearestIntegerMultiple := Round(Scale);
end;

procedure TWinWindowHandle.CorrectWindowSize(const WindowPos: PWindowPos);
var
  LClientSize: TSizeF;
  WndClientSize: TSize;
  WndClientOffset: TPoint;
  LClientRect, LWindowRect: TRect;
  FinalScale: TPointF;
begin
  GetClientRect(Wnd, LClientRect); // dp
  ClientToScreen(Wnd, LClientRect.TopLeft);
  ClientToScreen(Wnd, LClientRect.BottomRight);
  GetWindowRect(Wnd, LWindowRect); // dp

  WndClientOffset := (LClientRect.TopLeft - LWindowRect.TopLeft) + (LWindowRect.BottomRight - LClientRect.BottomRight); // dp
  WndClientSize := TSize.Create(WindowPos^.cx - WndClientOffset.X, WindowPos^.cy - WndClientOffset.Y); // dp
  if SameValue(Frac(WndClientSize.Width / Scale), 0, TEpsilon.Scale) then
    FinalScale.X := Scale
  else
    FinalScale.X := NearestIntegerMultiple;
  if SameValue(Frac(WndClientSize.Height / Scale), 0, TEpsilon.Scale) then
    FinalScale.Y := Scale
  else
    FinalScale.Y := NearestIntegerMultiple;

  LClientSize := TSizeF.Create(Round(WndClientSize.Width / FinalScale.X), Round(WndClientSize.Height / FinalScale.Y)); // dp

  WindowPos^.cx := Round(LClientSize.Width * FinalScale.X) + WndClientOffset.X; // px
  WindowPos^.cy := Round(LClientSize.Height * FinalScale.Y) + WndClientOffset.Y; // px
end;

class procedure TWinWindowHandle.SetForcedScale(NewScale: Single);
begin
  if NewScale < 1 then
    NewScale := 1;
  if not SameValue(FForcedScale, NewScale, TEpsilon.Scale) then
    FForcedScale := NewScale;
end;

constructor TWinWindowHandle.Create(const AForm: TCommonCustomForm; const AWnd: HWND);
begin
  inherited Create;
  if not (TCanvasManager.DefaultCanvas.InheritsFrom(TCustomCanvasD2D)) or GlobalUseGPUCanvas then
    FForcedScale := 1;
  FForm := AForm;
  FWnd := AWnd;
  GetWindowRect(Wnd, FWndBounds);
  FBounds := TRectF.Create(FWndBounds.Left, FWndBounds.Top, FWndBounds.Width / Scale, FWndBounds.Height / Scale);
  UpdateClientSize;
end;

function FindWindow(Handle: HWND): TCommonCustomForm;
begin
  Result := nil;
  if (Handle <> 0) then
  begin
    if GlobalFindAtomW(PChar(WindowAtomString)) = WindowAtom then
      Result := Pointer(GetProp(Handle, MakeIntAtom(WindowAtom)))
    else
      Result := nil;
  end;
end;

function FmxHandleToHWND(const FmxHandle: TWindowHandle): HWND;
begin
  if FmxHandle <> nil then
    Result := WindowHandleToPlatform(FmxHandle).Wnd
  else
    Result := 0;
end;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TWinWindowHandle;
begin
  Result := TWinWindowHandle(AHandle);
end;

function FormToHWND(Form: TCommonCustomForm): HWND;
begin
  if (Form <> nil) and (Form.Handle is TWinWindowHandle) then
    Result := TWinWindowHandle(Form.Handle).Wnd
  else
    Result := 0;
end;

function ApplicationHWND: HWND;
begin
  if PlatformWin <> nil then
    Result := PlatformWin.GetApplicationHWND
  else
    Result := 0;
end;

destructor TWinWindowHandle.Destroy;
begin
  FreeBuffer;
  FZOrderManager.Free;
  inherited;
end;

procedure TWinWindowHandle.ScaleChanged;
begin
  CalcNearestIntegerMultiple;

  FWndClientSize := TSize.Create(Ceil(FClientSize.Width * Scale), Ceil(FClientSize.Height * Scale));
  FClientSize := TSizeF.Create(FWndClientSize.Width / Scale, FWndClientSize.Height / Scale);
  SetWindowSizeByClientSize;

  Form.RecreateResources;
end;

procedure TWinWindowHandle.DpiChanged;
var
  NewScale: Single;
begin
  NewScale := NewDpi / StandardDpi;
  if not SameValue(FCurrentScale, NewScale, TEpsilon.Scale) then
  begin
    FCurrentScale := NewScale;
    ScaleChanged;
  end;
end;

procedure TWinWindowHandle.CreateBuffer;
begin
  FBufferSize := TSize.Create(Width, Height);
  FBitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
  FBitmapInfo.bmiHeader.biPlanes := 1;
  FBitmapInfo.bmiHeader.biBitCount := 32;
  FBitmapInfo.bmiHeader.biCompression := BI_RGB;
  FBitmapInfo.bmiHeader.biWidth := FBufferSize.Width;
  if FBitmapInfo.bmiHeader.biWidth <= 0 then
    FBitmapInfo.bmiHeader.biWidth := 1;
  FBitmapInfo.bmiHeader.biHeight := -FBufferSize.Height;
  if FBitmapInfo.bmiHeader.biHeight >= 0 then
    FBitmapInfo.bmiHeader.biHeight := -1;
  try
    FBufferBitmap := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBufferBits), 0, 0);
    if FBufferBits = nil then
      RaiseLastOSError;
    try
      FBufferHandle := CreateCompatibleDC(0);
      if FBufferHandle = 0 then
        RaiseLastOSError;
      try
        if SelectObject(FBufferHandle, FBufferBitmap) = 0 then
          RaiseLastOSError;
      except
        DeleteDC(FBufferHandle);
        FBufferHandle := 0;
        raise;
      end;
    except
      DeleteObject(FBufferBitmap);
      FBufferBits := nil;
      raise;
    end;
  except
    FBufferBitmap := 0;
    raise;
  end;
end;

procedure TWinWindowHandle.ResizeBuffer(const Width, Height: Integer);
begin
  if FBufferHandle = 0 then
    CreateBuffer(Width, Height)
  else if (FBitmapInfo.bmiHeader.biWidth <> Width) or (Abs(FBitmapInfo.bmiHeader.biHeight) <> Height) then
  begin
    FreeBuffer;
    CreateBuffer(Width, Height)
  end;
end;

function TWinWindowHandle.GetClientSize: TSize;
begin
  Result := FClientSize.Ceiling;
end;

function TWinWindowHandle.GetNearestIntegerMultiple: Integer;
begin
  if FNearestIntegerMultiple = 0 then
    CalcNearestIntegerMultiple;
  Result := FNearestIntegerMultiple;
end;

procedure TWinWindowHandle.SetWindowSizeByClientSize;
var
  LClientRect, LWindowRect: TRect;
  NewWindowSize: TSize;
  BorderThickness: TSize;
begin
  GetClientRect(Wnd, LClientRect);
  GetWindowRect(Wnd, LWindowRect);

  BorderThickness := TSize.Create(LWindowRect.Width - LClientRect.Width, LWindowRect.Height - LClientRect.Height);
  NewWindowSize := FWndClientSize + BorderThickness;

  if LWindowRect.Size <> NewWindowSize then
  begin
    FWndBounds.Width := NewWindowSize.Width;
    FWndBounds.Height := NewWindowSize.Height;
    FBounds.Width := NewWindowSize.Width / Scale;
    FBounds.Height := NewWindowSize.Height / Scale;
    SetWindowPos(Wnd, 0, LWindowRect.Left, LWindowRect.Top, NewWindowSize.Width, NewWindowSize.Height,
      SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE);
  end;
end;

procedure TWinWindowHandle.SetClientSize(const Value: TSize);
begin
  if FClientSize.Ceiling <> Value then
  begin
    FWndClientSize := TSize.Create(Ceil(Value.Width * Scale), Ceil(Value.Height * Scale));
    FClientSize := Value;
    SetWindowSizeByClientSize;
  end;
end;

function TWinWindowHandle.GetBounds: TRect;
begin
  Result := FBounds.Ceiling;
end;

procedure TWinWindowHandle.SetBounds(const Value: TRect);
var
  CurrentWndRect: TRect;
begin
  if FBounds.Ceiling <> Value then
  begin
    FWndBounds.Left := Ceil(Value.Left * Scale);
    FWndBounds.Top := Ceil(Value.Top * Scale);
    FWndBounds.Width := Ceil(Value.Width * Scale);
    FWndBounds.Height := Ceil(Value.Height * Scale);
    FBounds := Value;
    GetWindowRect(Wnd, CurrentWndRect);
    if CurrentWndRect <> FWndBounds then
      SetWindowPos(Wnd, 0, FWndBounds.Left, FWndBounds.Top, FWndBounds.Width, FWndBounds.Height,
        SWP_NOACTIVATE or SWP_NOZORDER);
    UpdateClientSize;
  end;
end;

function TWinWindowHandle.GetWndBounds: TRect;
begin
  Result := FWndBounds;
end;

procedure TWinWindowHandle.SetWndBounds(const Value: TRect);
var
  CurrentWndRect: TRect;
begin
  if FWndBounds <> Value then
  begin
    FWndBounds := Value;
    FBounds.Left := Value.Left / Scale;
    FBounds.Top := Value.Top / Scale;
    FBounds.Width := Value.Width / Scale;
    FBounds.Height := Value.Height / Scale;
    GetWindowRect(Wnd, CurrentWndRect);
    if CurrentWndRect <> Value then
      SetWindowPos(Wnd, 0, Value.Left, Value.Top, Value.Width, Value.Height, SWP_NOACTIVATE or SWP_NOZORDER);
    UpdateClientSize;
  end;
end;

function TWinWindowHandle.GetWndClientSize: TSize;
begin
  Result := FWndClientSize;
end;

procedure TWinWindowHandle.FreeBuffer;
begin
  if FBufferHandle <> 0 then
  begin
    DeleteDC(FBufferHandle);
    FBufferHandle := 0;
  end;
  if FBufferBitmap <> 0 then
  begin
    DeleteObject(FBufferBitmap);
    FBufferBitmap := 0;
  end;
end;

function TWinWindowHandle.GetScale: Single;
begin
  if not SameValue(FForcedScale, 0, TEpsilon.Scale) then
    Result := FForcedScale
  else if not SameValue(FCurrentScale, 0, TEpsilon.Scale) then
    Result := FCurrentScale
  else
    Result := GetWndScale(FWnd);

  FCurrentScale := Result;
end;

function TWinWindowHandle.GetTransparency: Boolean;
begin
  Result := (Form <> nil) and Form.Transparency;
end;

function TWinWindowHandle.GetZOrderManager: TWinZOrderManager;
begin
  if FZOrderManager = nil then
    FZOrderManager := TWinZOrderManager.Create(Self);
  Result := FZOrderManager;
end;

procedure TWinWindowHandle.UpdateClientSize;
var
  R: TRect;
  OldWndClientSize: TSize;
begin
  GetClientRect(Wnd, R);
  OldWndClientSize := FWndClientSize;
  FWndClientSize := TSize.Create(R.Width, R.Height);
  FClientSize := TSizeF.Create(FWndClientSize.Width / Scale, FWndClientSize.Height / Scale);

  // We need to update canvas size, based on new Client Size value.
  if (OldWndClientSize <> FWndClientSize) and FForm.IsHandleAllocated then
    TOpenForm(FForm).ResizeHandle;
end;

procedure TWinWindowHandle.UpdateLayer;
var
  Blend: TBlendFunction;
  Origin, Size, BitmapOrigin: TPoint;
  ContextObject: IContextObject;
begin
  if FBufferHandle <> 0 then
  begin
    { Copy from Context }
    if Supports(Form, IContextObject, ContextObject) and (ContextObject.Context <> nil) then
      ContextObject.Context.CopyToBits(FBufferBits, Form.Width * 4, Rect(0, 0, Form.Width, Form.Height));
    { Update }
    Origin := WindowHandleToPlatform(Form.Handle).WndBounds.TopLeft;
    Size := TPoint(FBufferSize);
    Blend.BlendOp := AC_SRC_OVER;
    Blend.AlphaFormat := $01; // AC_SRC_ALPHA;
    Blend.BlendFlags := 0;
    Blend.SourceConstantAlpha := $FF;
    BitmapOrigin := Point(0, 0);
    UpdateLayeredWindow(Wnd, 0, @Origin, @Size, FBufferHandle, @BitmapOrigin, $00000000, @Blend, ULW_ALPHA);
  end;
end;

function TWinWindowHandle.FormToWnd(const Rect: TRectF): TRectF;
begin
  Result := TRectF.Create(Rect.Left * Scale, Rect.Top * Scale, Rect.Right * Scale, Rect.Bottom * Scale);
end;

function TWinWindowHandle.WndToForm(const Rect: TRect): TRectF;
begin
  Result := WndToForm(TRectF.Create(Rect));
end;

function TWinWindowHandle.WndToForm(const Rect: TRectF): TRectF;
begin
  Result := TRectF.Create(Rect.Left / Scale, Rect.Top / Scale, Rect.Right / Scale, Rect.Bottom / Scale);
end;

function WMPaint(HWND: HWND; uMsg: UINT; wParam: wParam; LPARAM: LPARAM): LRESULT; stdcall;
var
  I, rgnStatus: Integer;
  Region: HRgn;
  RegionSize: DWORD;
  RegionData: PRgnData;
  R: TRect;
  LForm: TCommonCustomForm;
  UpdateRects, InPaintUpdateRects: TUpdateRects;
  PS: TPaintStruct;
  Wnd: Winapi.Windows.HWND;
  PaintControl: IPaintControl;
begin
  LForm := FindWindow(HWND);
  if LForm <> nil then
  begin
    LForm.Dispatch(Message);
    Wnd := FormToHWND(LForm);
    GetUpdateRect(Wnd, R, False);
    Region := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    if Region <> 0 then
      try
        rgnStatus := GetUpdateRgn(Wnd, Region, False);
        if (rgnStatus = 2) or (rgnStatus = 3) then
        begin
          RegionSize := GetRegionData(Region, $FFFF, nil);
          if RegionSize > 0 then
          begin
            GetMem(RegionData, RegionSize);
            try
              if RegionSize = GetRegionData(Region, RegionSize, RegionData) then
              begin
                SetLength(UpdateRects, RegionData.rdh.nCount);
                for I := 0 to RegionData.rdh.nCount - 1 do
                  UpdateRects[I] := WindowHandleToPlatform(LForm.Handle).WndToForm(PRgnRects(@RegionData.buffer[0])[I]);
              end;
            finally
              FreeMem(RegionData, RegionSize);
            end;
            if Supports(LForm, IPaintControl, PaintControl) then
            begin
              PaintControl.ContextHandle := BeginPaint(Wnd, PS);
              try
                if PlatformWin.FInPaintUpdateRects.TryGetValue(LForm.Handle, InPaintUpdateRects) and
                  (Length(InPaintUpdateRects) > 0) then
                begin
                  // add update rects from FInPaintUpdateRects
                  for I := 0 to High(InPaintUpdateRects) do
                  begin
                    SetLength(UpdateRects, Length(UpdateRects) + 1);
                    UpdateRects[High(UpdateRects)] := WindowHandleToPlatform(LForm.Handle).WndToForm(InPaintUpdateRects[I]);
                  end;
                end;
                PaintControl.PaintRects(UpdateRects);
                if PlatformWin.FInPaintUpdateRects.TryGetValue(LForm.Handle, InPaintUpdateRects) and
                  (Length(InPaintUpdateRects) > 0) then
                begin
                  // paint second time - when Repaint called in painting
                  PlatformWin.FInPaintUpdateRects.TryGetValue(LForm.Handle, UpdateRects);
                  SetLength(InPaintUpdateRects, 0);
                  PlatformWin.FInPaintUpdateRects.AddOrSetValue(LForm.Handle, InPaintUpdateRects);
                  PaintControl.PaintRects(UpdateRects);
                end;
                PaintControl.ContextHandle := 0;
              finally
                EndPaint(Wnd, PS);
              end;
            end;
          end;
        end;
      finally
        DeleteObject(Region);
      end;
    Result := DefWindowProc(HWND, uMsg, wParam, LPARAM);
  end
  else
    Result := DefWindowProc(HWND, uMsg, wParam, LPARAM);
end;

// When the WM_GESTURENOTIFY message is received, use SetGestureConfig to specify the gestures to receive.
// This message should always be bubbled up using the DefWindowProc function.
procedure WMGestureNotify(const AForm: TCommonCustomForm; uMsg: UINT; AGestureNotify: LPARAM);
const
  // Gestures
  CPan: array [Boolean] of Cardinal = (0, GC_PAN);
  CZoom: array [Boolean] of Cardinal = (0, GC_ZOOM);
  CRotate: array [Boolean] of Cardinal = (0, GC_ROTATE);
  CPressAndTap: array [Boolean] of Cardinal = (0, GC_PRESSANDTAP);
  CTwoFingerTap: array [Boolean] of Cardinal = (0, GC_TWOFINGERTAP);
var
  LPoint: TPoint;
  LPointF: TPointF;
  LControl: TComponent;
  LConfigs: array of TGestureConfig;
  LGestures: TInteractiveGestures;
  LObj: IControl;
  LGObj: IGestureControl;
  LGestureNotify: ^GESTURENOTIFYSTRUCT;
begin
  LGestureNotify := Pointer(AGestureNotify);

  // Get the location of the gesture.
  LPoint := SmallPointToPoint(LGestureNotify.ptsLocation);
  LPointF := TPointF.Create(LPoint.X, LPoint.Y);

  // Find the object that the gesture belongs to.
  LObj := AForm.ObjectAtPoint(LPointF);
  if LObj <> nil then
    LControl := LObj.GetObject
  else
    LControl := AForm;

  if Supports(LControl, IGestureControl, LGObj) then
    LGestures := LGObj.GetListOfInteractiveGestures;

  SetLength(LConfigs, 5);
  ZeroMemory(@LConfigs[0], SizeOf(GestureConfig) * 5);

  // Pan gesture & options
  LConfigs[0].dwID := GID_PAN;
  LConfigs[0].dwWant := CPan[TInteractiveGesture.Pan in LGestures] or GC_PAN_WITH_SINGLE_FINGER_VERTICALLY or
    GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY or GC_PAN_WITH_INERTIA;
  LConfigs[0].dwBlock := CPan[not (TInteractiveGesture.Pan in LGestures)] or GC_PAN_WITH_GUTTER;

  // Zoom gesture
  LConfigs[1].dwID := GID_ZOOM;
  LConfigs[1].dwWant := CZoom[TInteractiveGesture.Zoom in LGestures];
  LConfigs[1].dwBlock := CZoom[not (TInteractiveGesture.Zoom in LGestures)];

  // Rotate gesture
  LConfigs[2].dwID := GID_ROTATE;
  LConfigs[2].dwWant := CRotate[TInteractiveGesture.Rotate in LGestures];
  LConfigs[2].dwBlock := CRotate[not (TInteractiveGesture.Rotate in LGestures)];

  // TwoFingerTap gesture
  LConfigs[3].dwID := GID_TWOFINGERTAP;
  LConfigs[3].dwWant := CTwoFingerTap[TInteractiveGesture.TwoFingerTap in LGestures];
  LConfigs[3].dwBlock := CTwoFingerTap[not (TInteractiveGesture.TwoFingerTap in LGestures)];

  // PressAnTap gesture
  LConfigs[4].dwID := GID_PRESSANDTAP;
  LConfigs[4].dwWant := CPressAndTap[TInteractiveGesture.PressAndTap in LGestures];
  LConfigs[4].dwBlock := CPressAndTap[not (TInteractiveGesture.PressAndTap in LGestures)];

  SetGestureConfig(FormToHWND(AForm), 0, Length(LConfigs), @LConfigs[0], SizeOf(TGestureConfig));
end;

function WMGesture(const AForm: TCommonCustomForm; uMsg: UINT; AParam: wParam; AGestureInfo: LPARAM): LRESULT;
var
  LPoint: TPointF;
  LControl: TComponent;
  LGestureInfo: GestureInfo;
  EventInfo: TGestureEventInfo;
  Obj: IControl;
  LGObj: IGestureControl;
begin
  Result := 0;
  ZeroMemory(@LGestureInfo, SizeOf(LGestureInfo));
  LGestureInfo.cbSize := SizeOf(LGestureInfo);
  if GetGestureInfo(AGestureInfo, LGestureInfo) then
    try
      ZeroMemory(@EventInfo, SizeOf(EventInfo));
      EventInfo.GestureID := LGestureInfo.dwID + igiFirst;

      // Get the control
      LPoint := TPointF.Create(LGestureInfo.ptsLocation.X, LGestureInfo.ptsLocation.Y);
      Obj := AForm.ObjectAtPoint(LPoint);
      if Obj <> nil then
        LControl := Obj.GetObject
      else
        LControl := AForm;

      if EventInfo.GestureID = igiBegin then
        CapturedGestureControl := LControl;

      // Don't pass GID_BEGIN and GID_END to the control
      if (EventInfo.GestureID <> igiBegin) and
        (EventInfo.GestureID <> igiEnd) then
      begin
        // Set EventInfo fields from LGestureInfo
        EventInfo.Location := AForm.ScreenToClient(LPoint);
        EventInfo.Flags := [];
        if LGestureInfo.dwFlags and GF_BEGIN = GF_BEGIN then
          Include(EventInfo.Flags, TInteractiveGestureFlag.gfBegin);
        if LGestureInfo.dwFlags and GF_INERTIA = GF_INERTIA then
          Include(EventInfo.Flags, TInteractiveGestureFlag.gfInertia);
        if LGestureInfo.dwFlags and GF_END = GF_END then
          Include(EventInfo.Flags, TInteractiveGestureFlag.gfEnd);
        case EventInfo.GestureID of
          igiPan:
            begin
              EventInfo.Distance := Cardinal(LGestureInfo.ullArguments);
              EventInfo.InertiaVector :=
                TPointF(SmallPointToPoint(InertiaVectorFromArgument(LGestureInfo.ullArguments)));
            end;
          igiZoom, igiTwoFingerTap:
            EventInfo.Distance := Cardinal(LGestureInfo.ullArguments);
          igiPressAndTap:
            begin
              // ullArguments is distance/offset. Add to Location to make TapLocation
              LPoint := TPointF(SmallPointToPoint(TSmallPoint(Cardinal(LGestureInfo.ullArguments))));
              // EventInfo.TapLocation := AForm.ScreenToClient(TPointF(LPoint.X + LGestureInfo.ptsLocation.X, LPoint.Y + LGestureInfo.ptsLocation.Y));
            end;
          igiRotate:
            EventInfo.Angle := RotateAngleFromArgument(LGestureInfo.ullArguments);
        end;
        // send message to the control
        if Supports(CapturedGestureControl, IGestureControl, LGObj) then
          LGObj.CMGesture(EventInfo);
      end
      else
        Result := DefWindowProc(FormToHWND(AForm), uMsg, AParam, AGestureInfo);

      if EventInfo.GestureID = igiEnd then
        CapturedGestureControl := nil;

    finally
      CloseGestureInfoHandle(AGestureInfo);
    end;
end;

procedure InitializeMultiTouch(const AForm: TCommonCustomForm);
begin
  if PlatformWin.FMultiTouchManager = nil then
    PlatformWin.FMultiTouchManager := TMultiTouchManagerWin.Create(AForm)
  else if PlatformWin.FMultiTouchManager.Parent <> AForm then
    PlatformWin.FMultiTouchManager.Parent := AForm;
end;

function WMTouch(const AForm: TCommonCustomForm; uMsg: UINT; TouchInputCount: wParam; TouchInputInfo: LPARAM): LRESULT;
var
  TouchCount: Integer;
  Inputs: array of TTouchInput;
  Input: TTouchInput;
  I: Integer;
  Touches: TTouches;
  Action: TTouchAction;
  Point: TPointF;
  Control: IControl;
begin
  Result := 0;
  TouchCount := LoWord(Cardinal(TouchInputCount));
  SetLength(Inputs, TouchCount);
  Action := TTouchAction.None;

  if GetTouchInputInfo(TouchInputInfo, TouchCount, @Inputs[0], SizeOf(TTouchInput)) then
    try
      SetLength(Touches, TouchCount);
      for I := 0 to TouchCount - 1 do
      begin
        Input := Inputs[I];

        if (Input.dwFlags and TOUCHEVENTF_DOWN) <> 0 then
          Action := TTouchAction.Down
        else if ((Input.dwFlags and TOUCHEVENTF_UP) <> 0) then
          Action := TTouchAction.Up
        else if ((Input.dwFlags and TOUCHEVENTF_MOVE) <> 0) then
          Action := TTouchAction.Move;

        // TOUCHINFO point coordinates is in 1/100 of a pixel
        Point := TPointF.Create(Input.X / 100, Input.Y / 100);
        Touches[I].Location := AForm.ScreenToClient(Point);
      end;

      if Length(Touches) = 1 then
        Control := AForm.ObjectAtPoint(Point)
      else if Length(Touches) = 2 then
        Control := AForm.ObjectAtPoint(AForm.ClientToScreen(Touches[0].Location.MidPoint(Touches[1].Location)))
      else
        Control := nil;

      InitializeMultiTouch(AForm);
      PlatformWin.FMultiTouchManager.SetEnabledGestures(PlatformWin.FEnabledInteractiveGestures);
      PlatformWin.FMultiTouchManager.HandleTouches(Touches, Action, Control);
      Result := DefWindowProc(FormToHWND(AForm), uMsg, TouchInputCount, TouchInputInfo);
    finally
      CloseTouchInputHandle(TouchInputInfo);
    end;
end;

procedure HandleMouseGestures(const AForm: TCommonCustomForm; uMsg: UINT; const X, Y: Single);
var
  GestureObj: IGestureControl;
  Control: TComponent;
  Obj: IControl;
  Action: TTouchAction;
  Point: TPointF;
begin
  if TWinTouchGestureEngine.Supported(AForm) then
    if not ((uMsg <> WM_LBUTTONDOWN) and (PlatformWin.FMultiTouchManager = nil)) then
    begin
      Point := TPointF.Create(X, Y);
      Obj := AForm.ObjectAtPoint(Point);
      if Obj <> nil then
        Control := Obj.GetObject
      else
        Control := AForm;

      if Supports(Control, IGestureControl, GestureObj) then
      begin
        Control := GestureObj.GetFirstControlWithGestureEngine;
        if Control <> nil then
        begin
          case uMsg of
            WM_MOUSEMOVE:
              Action := TTouchAction.Move;
            WM_LBUTTONDOWN:
              Action := TTouchAction.Down;
            WM_LBUTTONUP:
              Action := TTouchAction.Up;
          else
            Action := TTouchAction.None;
          end;

          InitializeMultiTouch(AForm);
          PlatformWin.FMultiTouchManager.HandleMouseGestures(Point, Action, Obj);
        end;
      end;
    end;
end;

type
  PDestroyChildData = ^TDestroyChildData;

  TDestroyChildData = record
    Parent: HWND;
    Recreating: Boolean;
  end;

var
  LastKeyIsDeadKey: Boolean = False;
  LastMousePos: TPoint;

const
  ImpossibleMousePosition: TPoint = (X: Low(FixedInt); Y: Low(FixedInt));

function WndProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  UpdateRects: array of TRectF;
  LForm: TCommonCustomForm;
  Wnd: Winapi.Windows.HWND;
  WindowBorder: TWindowBorderWin;

  procedure ProcessUpdateMessages;
  var
    Msg: TMsg;
  begin
    SetLength(UpdateRects, 1);
    UpdateRects[0] := TRectF.Create(TSmallPoint(Cardinal(wParam)).X, TSmallPoint(Cardinal(wParam)).Y,
      TSmallPoint(Cardinal(lParam)).X, TSmallPoint(Cardinal(lParam)).Y);
    while PeekMessage(Msg, hwnd, WM_ADDUPDATERECT, WM_ADDUPDATERECT, PM_REMOVE) do
    begin
      if Msg.Message = WM_QUIT then
      begin
        { Repost WM_QUIT messages }
        PostQuitMessage(Msg.wParam);
        Break;
      end;
      SetLength(UpdateRects, Length(UpdateRects) + 1);
      UpdateRects[High(UpdateRects)] := RectF(TSmallPoint(Cardinal(Msg.wParam)).X, TSmallPoint(Cardinal(Msg.wParam)).Y,
        TSmallPoint(Cardinal(Msg.lParam)).X, TSmallPoint(Cardinal(Msg.lParam)).Y);
    end;
  end;

  procedure PrepareClosePopups;
  begin
    if (Screen <> nil) and (LForm <> nil) and (not WindowHandleToPlatform(LForm.Handle).FDisableDeactivate) then
    begin
      if LForm.FormStyle = TFormStyle.Popup then
        Screen.PrepareClosePopups(LForm)
      else
        Screen.PrepareClosePopups(nil);
    end;
  end;

  procedure ClosePopupList;
  begin
    if (Screen <> nil) and (LForm <> nil) and (not WindowHandleToPlatform(LForm.Handle).FDisableDeactivate) then
      Screen.ClosePopupForms;
  end;

  procedure InitialActionsOfPopups;
  begin
    case uMsg of
      WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN, WM_NCLBUTTONDBLCLK, WM_NCRBUTTONDBLCLK, WM_NCMBUTTONDBLCLK:
        begin
          PrepareClosePopups;
          ClosePopupList;
        end;
      WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN, WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK:
        PrepareClosePopups;
    end;
  end;

  procedure FinalActionsOfPopups;
  begin
    case uMsg of
      WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP, WM_DESTROY:
        begin
          LForm := FindWindow(hwnd);
          ClosePopupList;
        end;
    end;
  end;

  function DispatchMouseWheelToPopups: Boolean;
  var
    I: Integer;
    Handled: Boolean;
  begin
    Handled := False;
    if Screen <> nil then
      for I := Screen.PopupFormCount - 1 downto 0 do
        if Screen.PopupForms[I].Visible then
        begin
          Screen.PopupForms[I].MouseWheel(KeysToShiftState(wParam), TSmallPoint(Cardinal(wParam)).Y, Handled);
          if Handled then
            Break;
        end;
    Result := Handled;
  end;

  procedure CurrentChar(Msg: tagMsg; var Key: Word; var Ch: WideChar; var Shift: TShiftState);
  begin
    Key := wParam;
    Ch := WideChar(Msg.wParam);
    Shift := KeyDataToShiftState(lParam);
    if (Ch >= ' ') then
    begin
      if ((Shift * [ssAlt, ssCtrl]) = [ssAlt, ssCtrl]) then
      begin
        // AltGr + Char (in German keyboard)
        Shift := Shift - [ssAlt, ssCtrl];
      end;
      if (([ssAlt, ssCtrl, ssCommand] * Shift) = []) then
        Key := 0;
    end;
    if ((([ssAlt, ssCtrl, ssCommand] * Shift) <> []) or (Ch < ' ')) and (Key > 0) then
      Ch := #0;
  end;

  procedure ScaleMousePos(const Handle: TWindowHandle; var P: TPoint);
  begin
    ScreenToClient(WindowHandleToPlatform(Handle).Wnd, P);
    P := TPoint.Create(Round(P.X / Handle.Scale), Round(P.Y / Handle.Scale));
  end;

const
  FlagZOrder: UINT = SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE;
var
  R: TRect;
  P: TPoint;
  H: Boolean;
  Key: Word;
  Ch: WideChar;
  tme: TTRACKMOUSEEVENT;
  Message: TMessage;
  Shift: TShiftState;
  Placement: TWindowPlacement;
  Msg: tagMsg;
  PaintControl: IPaintControl;
  Obj: IControl;
  MenuDisplayed: Boolean;
  OldWindowState: TWindowState;
  CharMsg, DeadCharMsg: UInt32;
begin
                                                                       
  Result := 0;
  LForm := FindWindow(hwnd);

  Message.Msg := uMsg;
  Message.WParam := wParam;
  Message.LParam := lParam;
  Message.Result := 0;
  // Check to see if this is a design message
  if (LForm <> nil) and (LForm.Designer <> nil) and LForm.Designer.IsDesignMsg(LForm, Message) then
    Exit;

  if LForm <> nil then
  begin
    Wnd := FormToHWND(LForm);
    try
      InitialActionsOfPopups;
      try
        case uMsg of
          WM_NCHITTEST,
            WM_NCACTIVATE,
            WM_NCADDUPDATERECT,
            WM_NCMOUSEMOVE, WM_NCLBUTTONDOWN, WM_NCLBUTTONUP,
            WM_NCCALCSIZE, WM_NCPAINT:
            Result := WMNCMessages(LForm, uMsg, wParam, lParam);
          $B000 + 74: // CM_DESTROYHANDLE
            begin
              if (LForm.ClassName = 'TFormContainerForm') and (wParam = 1) then
              begin
                // IDE parent recreate
                SetParent(Wnd, GetDesktopWindow);
                SetWindowPos(Wnd, 0, $A000, $A000, 10, 10, SWP_NOSIZE or SWP_NOZORDER);
              end;
            end;
          WM_DESTROY:
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          WM_ACTIVATE:
            begin
              if not ((TFmxFormState.Recreating in LForm.FormState) or (LForm.FormStyle = TFormStyle.Popup) or
                (WindowHandleToPlatform(LForm.Handle).FDisableDeactivate)) then
              begin
                if LoWord(wParam) <> 0 then
                begin
                  if HiWord(wParam) = 0 then
                    LForm.Activate;
                  // If the window is minimized, then do nothing.
                end
                else
                begin
                  PrepareClosePopups;
                  LForm.Deactivate;
                  ClosePopupList;
                end;
              end;
              Result := 0;
            end;
          WM_MOUSEACTIVATE:
            begin
              if not (TFmxFormState.Recreating in LForm.FormState) then
              begin
                if (LForm.FormStyle = TFormStyle.Popup) then
                  Result := MA_NOACTIVATE
                else
                  Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
                // Default result if nothing happens
              end;
            end;
          WM_ERASEBKGND:
            begin
              Result := 1;
            end;
          WM_PAINT:
            begin
              Result := WMPaint(hwnd, uMsg, wParam, lParam);
            end;
          WM_DPICHANGED:
            begin
              MultiDisplayWin.UpdateDisplayInformation;
              WindowHandleToPlatform(LForm.Handle).DpiChanged(HiWord(wParam));
              Result := 0;
            end;
          WM_DISPLAYCHANGE:
            begin
              MultiDisplayWin.UpdateDisplayInformation;
            end;
          WM_ADDUPDATERECT:
            begin
              ProcessUpdateMessages;
              if Supports(LForm, IPaintControl, PaintControl) then
                PaintControl.PaintRects(UpdateRects);
              WindowHandleToPlatform(LForm.Handle).UpdateLayer;
            end;
          WM_WINDOWPOSCHANGING:
            begin
              if ([csLoading, csDesigning] * LForm.ComponentState = [csLoading]) then
              begin
                if (LForm.Position in [TFormPosition.Default, TFormPosition.DefaultPosOnly]) and
                  (LForm.WindowState <> TWindowState.wsMaximized) then
                begin
                  PWindowPos(lParam)^.Flags := PWindowPos(lParam)^.Flags or SWP_NOMOVE;
                end;
                if (LForm.Position in [TFormPosition.Default, TFormPosition.DefaultSizeOnly]) and
                  (LForm.BorderStyle in [TFmxFormBorderStyle.Sizeable, TFmxFormBorderStyle.SizeToolWin]) then
                begin
                  PWindowPos(lParam)^.Flags := PWindowPos(lParam)^.Flags or SWP_NOSIZE;
                end;
              end;
              if (not ((PWindowPos(lParam)^.Flags and FlagZOrder) = FlagZOrder)) then
              begin
                if (Screen <> nil) and (LForm <> nil) and (not WindowHandleToPlatform(LForm.Handle).FDisableDeactivate)
                then
                begin
                  if (LForm.FormStyle = TFormStyle.Popup) then
                    ClosePopupList;
                end;
              end;
              if (not ((PWindowPos(lParam)^.Flags and SWP_NOSIZE) = SWP_NOSIZE)) then
                WindowHandleToPlatform(LForm.Handle).CorrectWindowSize(PWindowPos(lParam));
              if (TFmxFormState.InDesigner in LForm.FormState) and (TFmxFormState.WasNotShown in LForm.FormState) then
                TOpenForm(LForm).ShowInDesigner;
            end;
          WM_WINDOWPOSCHANGED:
            begin
              Placement.Length := SizeOf(TWindowPlacement);
              GetWindowPlacement(hwnd, Placement);
              if (Application.MainForm <> nil) and (LForm = Application.MainForm)
                and (Placement.showCmd = SW_SHOWMINIMIZED) then
              begin
                PlatformWin.MinimizeApp;
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
              end
              else
              begin
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
                if (PWindowPos(lParam)^.Flags and SWP_NOSIZE = 0) or
                  (PWindowPos(lParam)^.Flags and SWP_NOMOVE = 0) then
                begin
                  GetWindowRect(hwnd, R);
                  WindowHandleToPlatform(LForm.Handle).WndBounds := R;
                  R := WindowHandleToPlatform(LForm.Handle).Bounds;
                  LForm.SetBounds(R.Left, R.Top, R.Width, R.Height);
                end;
                if PWindowPos(lParam)^.Flags and SWP_FRAMECHANGED = SWP_FRAMECHANGED then
                  WindowHandleToPlatform(LForm.Handle).UpdateClientSize;
                { update state }
                PlatformWin.FDiableUpdateState := True;
                try
                  OldWindowState := LForm.WindowState;
                  Placement.Length := SizeOf(TWindowPlacement);
                  GetWindowPlacement(hwnd, Placement);
                  case Placement.showCmd of
                    SW_SHOWMINIMIZED:
                      LForm.WindowState := TWindowState.wsMinimized;
                    SW_SHOWMAXIMIZED:
                      LForm.WindowState := TWindowState.wsMaximized;
                  else
                    if csDesigning in LForm.ComponentState then
                    begin
                      { for using Metro-style interface in designer we set Maximized but we can change window size }
                      if LForm.WindowState <> TWindowState.wsMaximized then
                        LForm.WindowState := TWindowState.wsNormal;
                    end
                    else
                    begin
                      if not (TFmxFormState.WasNotShown in LForm.FormState) then
                        LForm.WindowState := TWindowState.wsNormal;
                    end;
                  end;
                  if OldWindowState <> LForm.WindowState then
                  begin
                    PostMessage(hwnd, WM_CLOSEMENU, 0, 0);
                    PrepareClosePopups;
                    ClosePopupList;
                  end;
                finally
                  PlatformWin.FDiableUpdateState := False;
                end;
                WMNCMessages(LForm, uMsg, wParam, lParam);
              end;
            end;
          WM_CLOSE:
            LForm.Close;
          WM_LBUTTONDOWN:
            begin
              PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed := True;
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseDown(TMouseButton.mbLeft, MouseToShiftState(wParam), P.X, P.Y);
              HandleMouseGestures(LForm, uMsg, P.X, P.Y);
            end;
          WM_LBUTTONDBLCLK:
            begin
              PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed := True;
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseDown(TMouseButton.mbLeft, MouseToShiftState(wParam) + [ssDouble], P.X, P.Y);
            end;
          WM_CANCELMODE:
          begin
            // If the user shows dialog from mouse down event, WinApi rejects MouseUp event from queue. In this case,
            // Form.Captured control are not reset and we cannot complete click event correctly. As result button stay
            // pressed.
            // When modal dialog is appeared, WinApi sends WM_CANCELMODE message for rejecting other related events.
            // We save this flag for future emulation MouseUp event, if it's required.

            // It's not Ok for FMX, because FMX depends on normal sequence of mouse events Down -> Up, so
            // Form waits final MouseUp event.
            // If we receive WM_CANCELMODE message while MouseDown is processed, we should emulate MouseUp event.
            if PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed then
            begin
              GetCursorPos(P);
              SendMessage(hwnd, WM_LBUTTONUP, 0, PointToLParam(P));
            end;
            PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed := False;
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          end;
          WM_LBUTTONUP:
            begin
              PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed := False;
              WindowBorder := TWindowBorderWin(LForm.Border.WindowBorder);
              if LForm.Border.IsSupported and WindowBorder.NCClick then
                Result := WMNCMessages(LForm, uMsg, wParam, lParam)
              else
              begin
                GetCursorPos(P);
                LastMousePos := ImpossibleMousePosition;
                ScaleMousePos(LForm.Handle, P);
                LForm.MouseUp(TMouseButton.mbLeft, MouseToShiftState(wParam), P.X, P.Y);
              end;
              HandleMouseGestures(LForm, uMsg, P.X, P.Y);
            end;
           WM_RBUTTONDOWN, WM_RBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              Obj := IControl(LForm.ObjectAtPoint(P));
              if (Obj <> nil) and (Obj.GetObject <> nil) and not (csDesigning in Obj.GetObject.ComponentState) then
              begin
                Obj.SetFocus;
                MenuDisplayed := Obj.ShowContextMenu(P);
              end
              else
                MenuDisplayed := False;
              if not MenuDisplayed then
              begin
                ScaleMousePos(LForm.Handle, P);
                if uMsg = WM_RBUTTONDBLCLK then
                  LForm.MouseDown(TMouseButton.mbRight, MouseToShiftState(wParam) + [ssDouble], P.X, P.Y)
                else
                LForm.MouseDown(TMouseButton.mbRight, MouseToShiftState(wParam), P.X, P.Y);
              end;
            end;
          WM_RBUTTONUP:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseUp(TMouseButton.mbRight, MouseToShiftState(wParam), P.X, P.Y);
            end;
          WM_MBUTTONDOWN:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseDown(TMouseButton.mbMiddle, MouseToShiftState(wParam), P.X, P.Y);
            end;
          WM_MBUTTONUP:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseUp(TMouseButton.mbMiddle, MouseToShiftState(wParam), P.X, P.Y);
            end;
          WM_MBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseDown(TMouseButton.mbMiddle, MouseToShiftState(wParam) + [ssDouble], P.X, P.Y);
            end;
          WM_MENUSELECT,
          WM_INITMENUPOPUP:
            MenuServiceWin.Dispatch(Message);
          WM_MOUSEMOVE:
            begin
              WindowBorder := TWindowBorderWin(LForm.Border.WindowBorder);
              if LForm.Border.IsSupported then
              begin
                if WindowBorder.NCClick then
                  Result := WMNCMessages(LForm, uMsg, wParam, lParam)
                else
                begin
                  WindowBorder.MouseLeave;
                  GetCursorPos(P);
                  if P <> LastMousePos then
                  begin
                    LastMousePos := P;
                    ScaleMousePos(LForm.Handle, P);
                    LForm.MouseMove(MouseToShiftState(wParam), P.X, P.Y);
                  end;
                end;
              end
              else
              begin
                GetCursorPos(P);
                if P <> LastMousePos then
                begin
                  LastMousePos := P;
                  ScaleMousePos(LForm.Handle, P);
                  LForm.MouseMove(MouseToShiftState(wParam), P.X, P.Y);
                end;
              end;
              tme.cbSize := SizeOf(tme);
              tme.hwndTrack := hwnd;
              tme.dwFlags := TME_LEAVE;
              tme.dwHoverTime := HOVER_DEFAULT;
              TrackMouseEvent(tme);
              HandleMouseGestures(LForm, uMsg, P.X, P.Y);
            end;
          WM_MOUSELEAVE:
            begin
              WindowBorder := TWindowBorderWin(LForm.Border.WindowBorder);
              if LForm.Border.IsSupported and WindowBorder.NCClick then
                Result := WMNCMessages(LForm, uMsg, wParam, lParam)
              else
                LForm.MouseLeave;
            end;
          WM_MOUSEWHEEL:
            begin
              H := DispatchMouseWheelToPopups;

              if not H then
                LForm.MouseWheel(KeysToShiftState(wParam),
                  TSmallPoint(Cardinal(wParam)).Y, H);

              Result := Integer(H = True);
            end;
          WM_GETDLGCODE:
            begin
              Result := DLGC_WANTTAB or dlgc_WantArrows or DLGC_WANTCHARS;
            end;
          WM_CHAR:
            begin
              Ch := WideChar(wParam);
              Key := 0;
              LForm.KeyDown(Key, Ch, KeyDataToShiftState(lParam));
              LForm.KeyUp(Key, Ch, KeyDataToShiftState(lParam));
              Result := 0;
            end;
          WM_KEYDOWN,
          WM_SYSKEYDOWN:
            begin
              // Check if this key translates to a WM_CHAR/WM_SYSCHAR message
              // and if it does, pass KeyDown with character code
              // and clear the original WM_CHAR from the queue
              Msg.hwnd := hwnd;
              Msg.Message := uMsg;
              Msg.wParam := wParam;
              Msg.lParam := lParam;

              Result := 0;

              if uMsg = WM_SYSKEYDOWN then
              begin
                CharMsg := WM_SYSCHAR;
                DeadCharMsg := WM_SYSDEADCHAR;
              end
              else
              begin
                CharMsg := WM_CHAR;
                DeadCharMsg := WM_DEADCHAR;
              end;

              LastKeyIsDeadKey := False;
              if PeekMessage(Msg, hwnd, DeadCharMsg, DeadCharMsg, PM_NOREMOVE + PM_NOYIELD) then
                LastKeyIsDeadKey := True
              else if TranslateMessage(Msg) then
              begin
                if PeekMessage(Msg, hwnd, CharMsg, CharMsg, PM_REMOVE) then
                begin
                  CurrentChar(Msg, Key, Ch, Shift);
                  // clear duplicate WM_CHAR
                  if CharMsg = WM_CHAR then
                    PeekMessage(Msg, hwnd, CharMsg, CharMsg, PM_REMOVE);
                  LForm.KeyDown(Key, Ch, Shift);
                end
                else
                begin
                  Key := wParam;
                  Ch := #0;
                  LForm.KeyDown(Key, Ch, KeyDataToShiftState(lParam));
                end;
              end;

              // always let the system handle system shortcuts
              if uMsg = WM_SYSKEYDOWN then
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          WM_DEADCHAR:
            begin
              PeekMessage(Msg, hwnd, WM_DEADCHAR, WM_DEADCHAR, PM_REMOVE);
              PeekMessage(Msg, hwnd, WM_CHAR, WM_CHAR, PM_REMOVE);
            end;
          WM_KEYUP:
            begin
              Ch := #0;
              Key := wParam;
              Shift := KeyDataToShiftState(lParam);
              if LastKeyIsDeadKey then
              begin
                Result := 0;
              end
              else
              begin
                Msg.hwnd := hwnd;
                Msg.Message := WM_KEYDOWN;
                Msg.wParam := wParam;
                Msg.lParam := Msg.lParam and $7FFFFFFF;
                if TranslateMessage(Msg) then
                  if PeekMessage(Msg, hwnd, WM_CHAR, WM_CHAR, PM_REMOVE) then
                  begin
                    CurrentChar(Msg, Key, Ch, Shift);
                  end;
                LForm.KeyUp(Key, Ch, Shift);
                Result := 0;
              end
            end;
          WM_SYSKEYUP:
            begin
              if (wParam = VK_MENU) or (wParam = VK_F10) then
              begin
                LForm.EnterMenuLoop;
                Result := 0;
              end
              else
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          WM_RELEASEFORM:
            begin
              LForm.Free;
            end;
          { IME }
          WM_INPUTLANGCHANGE:
            begin
              // Result := SetIMECompositionWndPosition(LForm, uMsg, wParam, lParam);
              // OnInputLangChange();
            end;
          WM_IME_SETCONTEXT,
          WM_IME_STARTCOMPOSITION,
          WM_IME_COMPOSITION,
          WM_IME_NOTIFY,
          WM_IME_ENDCOMPOSITION:
            begin
              PlatformWin.ImmManager[LForm].Dispatch(Message);
              Result := Message.Result;
            end;              
          WM_COMMAND:
            begin
              MenuServiceWin.Dispatch(Message);
              Result := Message.Result;
            end;
          WM_GESTURENOTIFY:
            begin
              WMGestureNotify(LForm, uMsg, lParam);
              Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          WM_GESTURE:
            begin
              Result := WMGesture(LForm, uMsg, wParam, lParam);
            end;
          WM_TOUCH:
            begin
              GetCursorPos(P);
              Result := WMTouch(LForm, uMsg, wParam, lParam);
              ScreenToClient(Wnd, P);
            end;
          WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
            Result := SendMessage(lParam, CN_BASE + uMsg, wParam, lParam);
        else
          Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          // Default result if nothing happens
        end;
      finally
        FinalActionsOfPopups;
      end;
    except
      on E: Exception do
        Application.HandleException(E);
    end;
  end
  else { if LForm = nil }
  begin
    if (PlatformWin <> nil) and (hwnd = PlatformWin.FApplicationHWND) then
    begin
      case uMsg of
        WM_SYSCOMMAND:
          begin
            case TWMSysCommand(Message).CmdType of
              SC_MINIMIZE:
              begin
                PlatformWin.MinimizeApp;
                Result := 0;
              end;
              SC_RESTORE:
              begin
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
                PlatformWin.RestoreApp;
              end
            else
              Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          end;
        WM_CLOSE:
          begin
            if Application.MainForm <> nil then
            begin
              try
                if Screen <> nil then
                  Screen.ActiveForm := nil;
                Application.MainForm.Close;
              except
                on E: Exception do
                  Application.HandleException(E);
              end;
              Exit;
            end;
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          end;
        WM_DESTROY:
          Application.Terminate;
        WM_ACTIVATEAPP:
          begin
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            if BOOL(wParam) then
              PlatformWin.RestoreApp;
          end;
      else
        Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
      end;
    end
    else
      Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
  end;
  // Default result if nothing happens
end;

var
  FMAppClass: TWndClass = (
    style: 0;
    lpfnWndProc: @WndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TFMAppClass');

function TPlatformWin.CreateAppHandle: HWND;
var
  TempClass: TWndClass;
  P: PChar;
  ClassRegistered: Boolean;
  ModuleName: array [0 .. 255] of Char;
begin
  GetModuleFileName(MainInstance, ModuleName, Length(ModuleName));
  P := AnsiStrRScan(ModuleName, '\');
  if P <> nil then
    StrCopy(ModuleName, P + 1);
  P := AnsiStrScan(ModuleName, '.');
  if P <> nil then
    P^ := #0;
  CharLower(CharNext(ModuleName));
  FDefaultTitle := ModuleName;
  if Application <> nil then
    FTitle := Application.Title
  else
    FTitle := FDefaultTitle;
  FMAppClass.hInstance := hInstance;
  ClassRegistered := GetClassInfo(hInstance, FMAppClass.lpszClassName, TempClass);
  FMAppClass.hIcon := LoadIconW(MainInstance, PChar('MAINICON'));
  if not ClassRegistered or (TempClass.lpfnWndProc <> @WndProc) then
  begin
    if ClassRegistered then
      Winapi.Windows.UnregisterClass(FMAppClass.lpszClassName, hInstance);
    Winapi.Windows.RegisterClass(FMAppClass);
  end;
  Result := CreateWindowEx(WS_EX_WINDOWEDGE or WS_EX_APPWINDOW, FMAppClass.lpszClassName, PChar(FTitle),
                           WS_POPUP or WS_GROUP, 0, 0, 0, 0, GetDesktopWindow, 0, hInstance, nil);
  Winapi.Windows.ShowWindow(Result, SW_SHOWNORMAL);
end;

function TPlatformWin.GetApplicationHWND: HWND;
begin
  UpdateApplicationHwnd;
  Result := FApplicationHWND;
end;

function TPlatformWin.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
var
  DesignerForm: IDesignerForm;
  IsDesignerForm: Boolean;
  WindowClass: TWndClass;
  LDropTarget: TWinDropTarget;
  Style, ExStyle: DWORD;
  Wnd: HWND;
  ParentWnd: HWND;
  WndClassName: string;
  LForm, LParentForm: TCommonCustomForm;

  function GetParentFormHandle: HWND;
  begin
    Result := 0;
    if LForm.ParentForm <> nil then
    begin
      if LForm.ParentForm.Handle = nil then
        raise EArgumentException.CreateFMT(SNotInstance, ['ParentForm.Handle'])at ReturnAddress;
      Result := FormToHWND(LForm.ParentForm);
    end;
  end;

begin
  RaiseIfNil(AForm, 'AForm');

  LDropTarget := nil;
  LForm := AForm;
  Style := WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
  ExStyle := 0;
  WndClassName := 'FM' + LForm.ClassName;
  IsDesignerForm := TFmxFormState.InDesigner in LForm.FormState;
  if not GetClassInfo(hInstance, PChar(WndClassName), WindowClass) then
  begin
    FillChar(WindowClass, SizeOf(WindowClass), 0);
    WindowClass.style := CS_DBLCLKS or CS_HREDRAW or CS_VREDRAW;
    WindowClass.lpfnWndProc := @WndProc;
    WindowClass.cbClsExtra := 0;
    WindowClass.cbWndExtra := 0;
    WindowClass.hInstance := hInstance;
    WindowClass.hIcon := LoadIconW(MainInstance, PChar('MAINICON'));
    if csDesigning in LForm.ComponentState then
      WindowClass.hCursor := LoadCursorW(0, PChar(IDC_ARROW))
    else
      WindowClass.hCursor := 0;
    WindowClass.hbrBackground := GetStockObject(NULL_BRUSH);
    WindowClass.lpszMenuName := nil;
    WindowClass.lpszClassName := PChar(WndClassName);
    if Winapi.Windows.RegisterClass(WindowClass) = 0 then
      RaiseLastOSError;
  end;
  if (csDesigning in LForm.ComponentState) or IsDesignerForm then
  begin
    Style := Style or WS_CHILD;
    // Parent handle going to set in IDE.
    // Now set temporary value
    ParentWnd := GetDesktopWindow;
  end
  else
  begin
    case LForm.FormStyle of
      TFormStyle.Popup:
        begin
          Style := style or WS_POPUP;
          ExStyle := ExStyle or WS_EX_NOACTIVATE;
        end;
      TFormStyle.StayOnTop:
        begin
          ExStyle := ExStyle or WS_EX_TOPMOST;
        end;
    end;
    if LForm.Transparency then
    begin
      Style := style or WS_POPUP;
      ExStyle := ExStyle or WS_EX_LAYERED;
    end
    else
    begin
      case LForm.BorderStyle of
        TFmxFormBorderStyle.None:
          Style := Style or WS_POPUP or WS_SYSMENU;
        TFmxFormBorderStyle.Single, TFmxFormBorderStyle.ToolWindow:
          Style := Style or (WS_CAPTION or WS_BORDER);
        TFmxFormBorderStyle.Sizeable, TFmxFormBorderStyle.SizeToolWin:
          Style := Style or (WS_CAPTION or WS_THICKFRAME);
      end;
      if LForm.BorderStyle in [TFmxFormBorderStyle.ToolWindow, TFmxFormBorderStyle.SizeToolWin] then
        ExStyle := ExStyle or WS_EX_TOOLWINDOW;
      if LForm.BorderStyle <> TFmxFormBorderStyle.None then
      begin
        if TBorderIcon.biMinimize in LForm.BorderIcons then
          Style := Style or WS_MINIMIZEBOX;
        if TBorderIcon.biMaximize in LForm.BorderIcons then
          Style := Style or WS_MAXIMIZEBOX;
        if TBorderIcon.biSystemMenu in LForm.BorderIcons then
          Style := Style or WS_SYSMENU;
      end;
    end;
    // Use handle of parent form
    ParentWnd := GetParentFormHandle;
    // For Dialogs and Popups we used handle of active form
    if (ParentWnd = 0) and ((TFmxFormState.Modal in LForm.FormState) or (LForm.FormStyle = TFormStyle.Popup)) then
    begin
      if (Screen <> nil) and (Screen.ActiveForm <> nil) then
        ParentWnd := FormToHWND(Screen.ActiveForm);
      if (ParentWnd = 0) then
      begin
        ParentWnd := GetActiveWindow;
        LParentForm := FindWindow(ParentWnd);
        if LParentForm = nil then
          ParentWnd := 0;
      end;
    end;
    // If none parent then we use handle of Application
    if ParentWnd = 0 then
      ParentWnd := ApplicationHWND;
  end;
  Wnd := CheckWinapiHandle(CreateWindowEx(ExStyle, WindowClass.lpszClassName, PChar(LForm.Caption), style,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), ParentWnd, 0,
    hInstance, nil));
  try
    SetProp(Wnd, MakeIntAtom(WindowAtom), THandle(LForm));
    try
      if not ((csDesigning in AForm.ComponentState) or Supports(AForm, IDesignerForm, DesignerForm)) then
      begin
        LDropTarget := TWinDropTarget.Create(nil);
        LDropTarget.Form := LForm;
      end;
      try
        if LDropTarget <> nil then
          RegisterDragDrop(Wnd, LDropTarget);
        Result := TWinWindowHandle.Create(LForm, Wnd);
        TWinWindowHandle(Result).FWinDropTarget := LDropTarget;
      except
        if LDropTarget <> nil then
          RevokeDragDrop(Wnd);
        raise;
      end;
    except
      FreeAndNil(LDropTarget);
      raise;
    end;
  except
    Winapi.Windows.DestroyWindow(Wnd);
    raise;
  end;
  TWinWindowHandle(Result).FWinDropTarget := LDropTarget;

  FImmManagers.Add(AForm, TImmManager.Create(AForm));
  FFormsInfo.Add(AForm, TFormInfo.Create);
end;

function TPlatformWin.CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
begin
  Result := FMX.Forms.Border.Win.CreateWindowBorder(AForm);
end;

procedure TPlatformWin.DestroyWindow(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
  DesignerForm: IDesignerForm;
begin
  RaiseIfNil(AForm, 'AForm');

  HideWindow(AForm);
  Wnd := FormToHWND(AForm);
  if not ((csDesigning in AForm.ComponentState) or Supports(AForm, IDesignerForm, DesignerForm)) then
    RevokeDragDrop(Wnd);
  WindowHandleToPlatform(AForm.Handle).FWinDropTarget.Free;
  RemoveProp(Wnd, MakeIntAtom(WindowAtom));
  Winapi.Windows.DestroyWindow(Wnd);

  FImmManagers.Remove(AForm);
  FFormsInfo.Remove(AForm);
end;

procedure TPlatformWin.ReleaseWindow(const AForm: TCommonCustomForm);
begin
end;

procedure TPlatformWin.InvalidateImmediately(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  InvalidateWindowRect(AForm, AForm.ClientRect);
end;

procedure TPlatformWin.InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
var
  WR: TRect;
  Wnd: HWND;
  PaintControl: IPaintControl;
  UpdateRects: TUpdateRects;
  I: Integer;
begin
  RaiseIfNil(AForm, 'AForm');

  if IntersectRect(R, TRectF.Create(0, 0, AForm.ClientWidth, AForm.ClientHeight)) then
  begin
    Wnd := FormToHWND(AForm);
    if AForm.Transparency and not (csDesigning in AForm.ComponentState) then
    begin
      PostMessage(Wnd, WM_ADDUPDATERECT, Integer(SmallPoint(Round(R.Left), Round(R.Top))),
        Integer(SmallPoint(Round(R.Right), Round(R.Bottom))));
    end
    else
    begin
      R := WindowHandleToPlatform(AForm.Handle).FormToWnd(R);
      if Supports(AForm, IPaintControl, PaintControl) and (PaintControl.ContextHandle <> 0) then
      begin
        // In Paint
        if not FInPaintUpdateRects.TryGetValue(AForm.Handle, UpdateRects) then
        begin
          SetLength(UpdateRects, 1);
          UpdateRects[0] := R;
          FInPaintUpdateRects.Add(AForm.Handle, UpdateRects);
          Exit;
        end
        else
          for I := 0 to High(UpdateRects) do
            if (UpdateRects[I] = R) or (UpdateRects[I].Contains(R.TopLeft) and UpdateRects[I].Contains(R.BottomRight))
            then
              Exit;
        SetLength(UpdateRects, Length(UpdateRects) + 1);
        UpdateRects[High(UpdateRects)] := R;
        FInPaintUpdateRects.AddOrSetValue(AForm.Handle, UpdateRects);
      end
      else
      begin
        WR := TRect.Create(Trunc(R.Left), Trunc(R.Top), Ceil(R.Right), Ceil(R.Bottom));
        Winapi.Windows.InvalidateRect(Wnd, @WR, False);
      end;
    end;
  end;
end;

procedure TPlatformWin.MinimizeApp;
var
  AnimationEnable: Boolean;

  function GetAnimation: Boolean;
  var
    Info: TAnimationInfo;
  begin
    Info.cbSize := SizeOf(TAnimationInfo);
    if SystemParametersInfo(SPI_GETANIMATION, Info.cbSize, @Info, 0) then
      Result := Info.iMinAnimate <> 0
    else
      Result := False;
  end;

  procedure SetAnimation(Value: Boolean);
  var
    Info: TAnimationInfo;
  begin
    Info.cbSize := SizeOf(TAnimationInfo);
    Info.iMinAnimate := Integer(BOOL(Value));
    SystemParametersInfo(SPI_SETANIMATION, Info.cbSize, @Info, 0);
  end;

  procedure MinimiseAllForms;
  var
    I: Integer;
    WindowHandle: HWND;
  begin
    for I := 0 to Screen.FormCount - 1 do
    begin
      WindowHandle := FormToHWND(Screen.Forms[I]);
      if IsWindowVisible(WindowHandle) then
        DefWindowProc(WindowHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    end;
  end;

begin
  AnimationEnable := GetAnimation;
  try
    SetAnimation(False);
    if Application.MainForm <> nil then
    begin
      SetWindowPos(ApplicationHWND, FormToHWND(Application.MainForm), Application.MainForm.Left,
        Application.MainForm.Top, Application.MainForm.Width, 0, SWP_SHOWWINDOW);
      MinimiseAllForms;
    end;
    DefWindowProc(ApplicationHWND, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  finally
    SetAnimation(AnimationEnable);
  end;
end;

function TPlatformWin.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    Result := WindowHandleToPlatform(AForm.Handle).Bounds
  else
    Result := TRectF.Create(0, 0, AForm.Width, AForm.Height);
end;

function TPlatformWin.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    Result := AForm.Handle.Scale
  else
    Result := GetScreenScale;
end;

procedure TPlatformWin.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
begin
  RaiseIfNil(AForm, 'AForm');

  { for using Metro-style interface in designer we set Maximized but we can change window size }
  if AForm.IsHandleAllocated and (AForm.WindowState in [TWindowState.wsNormal, TWindowState.wsMaximized]) then
    WindowHandleToPlatform(AForm.Handle).Bounds := ARect.Round;
end;

procedure TPlatformWin.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
begin
  RaiseIfNil(AForm, 'AForm');

  SetWindowText(FormToHWND(AForm), ACaption);
end;

procedure TPlatformWin.RegisterCanvasClasses;
begin
  if GlobalUseGPUCanvas then
    FMX.Canvas.GPU.RegisterCanvasClasses;
  FMX.Canvas.D2D.RegisterCanvasClasses;
  FMX.Canvas.GDIP.RegisterCanvasClasses;
end;

procedure TPlatformWin.UnhookTouchHandler(const AForm: TCommonCustomForm);
begin
  if TOSVersion.Check(6, 1) then
    UnregisterTouchWindow(FormToHWND(AForm));
end;

procedure TPlatformWin.UnregisterCanvasClasses;
begin
  if GlobalUseGPUCanvas then
    FMX.Canvas.GPU.UnregisterCanvasClasses;
  FMX.Canvas.D2D.UnregisterCanvasClasses;
  FMX.Canvas.GDIP.UnregisterCanvasClasses;
end;

procedure TPlatformWin.RegisterContextClasses;
begin
  FMX.Context.DX9.RegisterContextClasses;
  FMX.Context.DX11.RegisterContextClasses;
end;

procedure TPlatformWin.UnregisterContextClasses;
begin
  FMX.Context.DX9.UnregisterContextClasses;
  FMX.Context.DX11.UnregisterContextClasses;
end;

procedure TPlatformWin.ReleaseCapture(const AForm: TCommonCustomForm);
begin
  Winapi.Windows.ReleaseCapture;
end;

procedure TPlatformWin.SetApplicationHWNDProc(const Value: TApplicationHWNDProc);
begin
  if @FApplicationHWNDProc <> @Value then
  begin
    FApplicationHWNDProc := Value;
    UpdateApplicationHwnd;
  end;
end;

procedure TPlatformWin.SetCapture(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  Winapi.Windows.SetCapture(FormToHWND(AForm));
end;

function TPlatformWin.GetCaretWidth: Integer;
begin
  Result := 1;
end;

function TPlatformWin.GetClientSize(const AForm: TCommonCustomForm): TPointF;
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    Result := TSizeF.Create(WindowHandleToPlatform(AForm.Handle).ClientSize)
  else
    Result := TSizeF.Create(AForm.Width, AForm.Height);
end;

procedure TPlatformWin.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    WindowHandleToPlatform(AForm.Handle).ClientSize := TSizeF.Create(ASize).Ceiling;
end;

procedure TPlatformWin.HideWindow(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  SetWindowPos(FormToHWND(AForm), 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or
    SWP_NOACTIVATE);
end;

const
  ShowCommands: array [TWindowState] of Integer = (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED);

procedure TPlatformWin.ShowWindow(const AForm: TCommonCustomForm);
const
  uFlags = SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE;
var
  Wnd, WndParent: HWND;
  nCmdShow: Integer;
  OldActiveForm: TCommonCustomForm;
  OldDisableDeactivate: Boolean;
  OldActiveHandle: TWinWindowHandle;
begin
  RaiseIfNil(AForm, 'AForm');

  Wnd := FormToHWND(AForm);
  nCmdShow := ShowCommands[AForm.WindowState];
  if (AForm.FormStyle = TFormStyle.Popup) then
  begin
    nCmdShow := nCmdShow or SW_SHOWNOACTIVATE;
    OldDisableDeactivate := False;
    OldActiveHandle := nil;
    if Screen <> nil then
    begin
      OldActiveForm := Screen.ActiveForm;
      if OldActiveForm <> nil then
        OldActiveHandle := WindowHandleToPlatform(OldActiveForm.Handle);
    end;
    try
      if OldActiveHandle <> nil then
      begin
        OldDisableDeactivate := OldActiveHandle.FDisableDeactivate;
        OldActiveHandle.FDisableDeactivate := True;
      end;
      Winapi.Windows.ShowWindow(Wnd, nCmdShow);
      if OldActiveHandle <> nil then
        SetWindowPos(OldActiveHandle.Wnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
    finally
      if OldActiveHandle <> nil then
        OldActiveHandle.FDisableDeactivate := OldDisableDeactivate;
    end;
  end
  else
    Winapi.Windows.ShowWindow(Wnd, nCmdShow);

  if AForm.Transparency and not (csDesigning in AForm.ComponentState) then
    PostMessage(FormToHWND(AForm), WM_ADDUPDATERECT, Integer(SmallPoint(0, 0)),
      Integer(SmallPoint(AForm.Width, AForm.Height)));

  if AForm.FormStyle in [TFormStyle.StayOnTop, TFormStyle.Popup] then
  begin
    WndParent := GetParent(Wnd);
    if (WndParent = GetDesktopWindow) or (WndParent = 0) then
      SetWindowPos(Wnd, HWND_TOPMOST, 0, 0, 0, 0, uFlags)
    else
      SetWindowPos(Wnd, HWND_TOP, 0, 0, 0, 0, uFlags);
  end;
end;

procedure TPlatformWin.BringToFront(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  RaiseIfNil(AForm, 'AForm');

  Wnd := FormToHWND(AForm);
  SetWindowPos(Wnd, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TPlatformWin.SendToBack(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  RaiseIfNil(AForm, 'AForm');

  Wnd := FormToHWND(AForm);
  SetWindowPos(Wnd, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TPlatformWin.Activate(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  RaiseIfNil(AForm, 'AForm');

  Wnd := FormToHWND(AForm);
  if not IsWindowVisible(Wnd) or IsIconic(Wnd) then
  begin
    if AForm.FormStyle = TFormStyle.Popup then
      Winapi.Windows.ShowWindow(Wnd, SW_RESTORE or SW_SHOWNOACTIVATE)
    else
      Winapi.Windows.ShowWindow(Wnd, SW_RESTORE);
  end;
  if AForm.FormStyle <> TFormStyle.Popup then
    Winapi.Windows.SetActiveWindow(Wnd);
end;

procedure TPlatformWin.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
var
  Wnd: HWND;

  procedure DoSetState(const AState: TWindowState);
  begin
    if AForm.FormStyle = TFormStyle.Popup then
      Winapi.Windows.ShowWindow(Wnd, ShowCommands[AState] or SW_SHOWNOACTIVATE)
    else
    begin
      if (Application.MainForm = AForm) and (AState = TWindowState.wsMinimized) then
        Winapi.Windows.ShowWindow(ApplicationHWND, ShowCommands[AState])
      else
        Winapi.Windows.ShowWindow(Wnd, ShowCommands[AState])
    end;
  end;

begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.Visible and not FDiableUpdateState then
  begin
    Wnd := FormToHWND(AForm);
    if AForm.FullScreen then
      try
        FDiableUpdateState := True;
        AForm.WindowState := TWindowState.wsMaximized;
        if not Winapi.Windows.IsZoomed(Wnd) then
          DoSetState(TWindowState.wsMaximized);
      finally
        FDiableUpdateState := False;
      end
    else
      DoSetState(AState);
  end;
end;

function TPlatformWin.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
var
  WindowList: Pointer;
  AppService: IFMXApplicationService;
  CloseRes: TCloseAction;
begin
  RaiseIfNil(AForm, 'AForm');

  Result := mrNone;
  if GetCapture <> 0 then
    SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  Winapi.Windows.ReleaseCapture;
  AForm.HandleNeeded;
  WindowList := DisableTaskWindows(FormToHWND(AForm));
  try
    CloseRes := TCloseAction.caNone;
    AForm.Show;
    AForm.ModalResult := mrNone;
    SetActiveWindow(FormToHWND(AForm));
    SetFocus(FormToHWND(AForm));
    Screen.ActiveForm := AForm;
    AppService := IFMXApplicationService(TPlatformServices.Current.GetPlatformService(IFMXApplicationService));
    repeat
      if not Application.HandleMessage then
        AppService.WaitMessage;
      if Application.Terminated then
        Break
      else if AForm.ModalResult <> mrNone then
      begin
        CloseRes := AForm.CloseModal;
        Result := AForm.ModalResult;
      end;
    until Result <> mrNone;
    if not Application.Terminated and (CloseRes <> TCloseAction.caFree) then
      AForm.Hide;
  finally
    EnableTaskWindows(WindowList);
  end;

  Result := AForm.ModalResult;
end;

function TPlatformWin.CanShowModal: Boolean;
begin
  Result := True;
end;

function TPlatformWin.ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  P: TPoint;
  Scale: Single;
begin
  RaiseIfNil(AForm, 'AForm');

  Scale := WindowHandleToPlatform(AForm.Handle).Scale;
  P := (Point * Scale).Round;
  Winapi.Windows.ClientToScreen(FormToHWND(AForm), P);
  Result := TPointF.Create(P) / Scale;
end;

function TPlatformWin.ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  P: TPoint;
  Scale: Single;
begin
  RaiseIfNil(AForm, 'AForm');

  Scale := WindowHandleToPlatform(AForm.Handle).Scale;
  P := (Point * Scale).Round;
  Winapi.Windows.ScreenToClient(FormToHWND(AForm), P);
  Result := TPointF.Create(P) / Scale;
end;

{ Menus }

function TPlatformWin.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  Result := AComponent = TComponentKind.Calendar;
end;

procedure TPlatformWin.ThreadSync(var Msg: TMessage);
begin
  if Msg.Msg = WM_NULL then
  begin
    CheckSynchronize;
    Msg.Result := 0;
  end
  else
    Msg.Result := DefWindowProc(FThreadSyncHandle, Msg.Msg, Msg.wParam, Msg.LPARAM);
end;

procedure TPlatformWin.RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Exclude(FEnabledInteractiveGestures, ARec);
end;

procedure TPlatformWin.RestoreApp;

  function TryFindForm(const AFormHandle: HWND; out AForm: TCommonCustomForm): Boolean;
  var
    I: Integer;
    Form: TCommonCustomForm;
  begin
    for I := 0 to Screen.FormCount - 1 do
    begin
      Form := Screen.Forms[I];
      if FormToHWND(Form) = AFormHandle then
      begin
        AForm := Form;
        Exit(True);
      end;
    end;
    Result := False;
  end;

var
  LWND: HWND;
  Form: TCommonCustomForm;
begin
  if Screen = nil then
    Exit;

  if Screen.ActiveForm <> nil then
    Screen.ActiveForm.Activate
  else
  begin
    LWND := GetActiveWindow;
    if LWND <> 0 then
    begin
      if TryFindForm(LWND, Form) then
        Form.Activate
      else
        SetActiveWindow(LWND);
      Exit;
    end;

    if Application.MainForm <> nil then
      Application.MainForm.Activate;
  end;
end;

procedure TPlatformWin.HookTouchHandler(const AForm: TCommonCustomForm);
begin
  if TOSVersion.Check(6, 1) and TWinTouchGestureEngine.Supported(AForm) then
    RegisterTouchWindow(FormToHWND(AForm), TWF_WANTPALM);
end;

{ Drag and Drop }

const
  SID_IDropTargetHelper = '{4657278B-411B-11d2-839A-00C04FD918D0}';
  CLSID_DragDropHelper: TGUID = (D1: $4657278A; D2: $411B; D3: $11D2; D4: ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));

type
  IDropTargetHelper = interface(IUnknown)
    [SID_IDropTargetHelper]
    function DragEnter(hwndTarget: HWND; const dataObj: IDataObject; var pt: TPoint;
      dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function DragOver(var pt: TPoint; dwEffect: Longint): HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; var pt: TPoint; dwEffect: Longint): HRESULT; stdcall;
    function Show(Show: BOOL): HRESULT; stdcall;
  end;

var
  FDropTargetHelper: IDropTargetHelper;
  FDataObj: IDataObject;

function TWinDropTarget.GetDataObject: TDragObject;
var
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  str: string;
  Drop: HDrop;
  I, numFiles, buflen: Integer;
  buffer: MarshaledString;
begin
  FillChar(Result, SizeOf(Result), 0);
  if FDataObj = nil then
    Exit;
  // get file name first
  FormatEtc.cfFormat := CF_HDROP;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.Tymed := TYMED_HGLOBAL;
  // get FireMonkey
  if PlatformWin.FDragAndDropActive then
  begin
    FormatEtc.cfFormat := CF_FMOBJECT;
    if FDataObj.GetData(FormatEtc, StgMedium) = S_OK then
    begin
      Result := TDropSource(StgMedium.HGLOBAL).Data;
      Exit;
    end;
  end;
  // files
  str := '';
  FormatEtc.cfFormat := CF_HDROP;
  if FDataObj.GetData(FormatEtc, StgMedium) = S_OK then
  begin
    try
      Drop := HDrop(GlobalLock(StgMedium.HGLOBAL));
      { Replace Text }
      numFiles := DragQueryFile(Drop, $FFFFFFFF, nil, 0);
      SetLength(Result.Files, numFiles);
      for I := 0 to numFiles - 1 do
      begin
        // Setting length of buffer plus 1 for string end char (#0)
        buflen := DragQueryFile(Drop, I, nil, 0) + 1;
        buffer := StrAlloc(buflen);
        DragQueryFile(Drop, I, buffer, buflen);
        Result.Files[I] := buffer;
        StrDispose(buffer);
        if I = 0 then
          Result.Data := Result.Files[0];
      end;
    finally
      GlobalUnlock(StgMedium.HGLOBAL);
      { Free the memory }
      ReleaseStgMedium(StgMedium);
    end;
    Exit;
  end;
  // get text
  FormatEtc.cfFormat := CF_UNICODETEXT;
  if FDataObj.GetData(FormatEtc, StgMedium) = S_OK then
  begin
    try
      { Lock the global memory handle to get a pointer to the data }
      str := PChar(GlobalLock(StgMedium.HGLOBAL));
      Result.Data := str;
    finally
      { Finished with the pointer }
      GlobalUnlock(StgMedium.HGLOBAL);
      { Free the memory }
      ReleaseStgMedium(StgMedium);
    end;
    Exit;
  end;
end;

function TWinDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HRESULT;
var
  Res: HRESULT;
begin
  try
    FDataObj := dataObj;
    Result := S_OK;
    dwEffect := DROPEFFECT_NONE;
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IDropTargetHelper,
      FDropTargetHelper))) and (FDropTargetHelper <> nil) then
    begin
      Res := FDropTargetHelper.DragEnter(FormToHWND(Form), dataObj, pt, dwEffect);
      if (Failed(Res)) then
        FDropTargetHelper := nil;
    end;
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TWinDropTarget.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT;
var
  P: TPointF;
  Operation: TDragOperation;
begin
  Result := E_UNEXPECTED;
  try
    dwEffect := DROPEFFECT_NONE;
    P := PointF(pt.X, pt.Y);
    Operation := TDragOperation.None;
    Form.DragOver(GetDataObject, P, Operation);
    case Operation of
      TDragOperation.None:
        dwEffect := DROPEFFECT_NONE;
      TDragOperation.Move:
        dwEffect := DROPEFFECT_MOVE;
      TDragOperation.Copy:
        dwEffect := DROPEFFECT_COPY;
      TDragOperation.Link:
        dwEffect := DROPEFFECT_LINK;
    end;

    // do NOT translate the screen coordinates to form coordinates because
    // it seems that the drop target helper needs screen coordinates
    if FDropTargetHelper <> nil then
      FDropTargetHelper.DragOver(pt, dwEffect);
    Result := S_OK;
  except
    dwEffect := DROPEFFECT_NONE;
  end;
end;

function TWinDropTarget.DragLeave: HRESULT;
begin
  Form.DragLeave;
  if (FDropTargetHelper <> nil) then
    FDropTargetHelper.DragLeave;
  FDropTargetHelper := nil;
  FDataObj := nil;
  Result := S_OK;
end;

function TWinDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HRESULT;
var
  P: TPointF;
begin
  Result := S_OK;
  try
    if (dataObj = nil) then
      Exit;
    P := PointF(pt.X, pt.Y);
    Form.DragDrop(GetDataObject, P);
    // do NOT translate the screen coordinates to form coordinates because
    // it seems that the drop target helper needs screen coordinates
    if (FDropTargetHelper <> nil) then
      FDropTargetHelper.Drop(dataObj, pt, dwEffect)
  finally
    FDataObj := nil;
    FDropTargetHelper := nil;
  end;
end;

{ TDropSource }

{ IDropSource }

function TDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Integer): HRESULT;
var
  ContinueDrop: Boolean;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and (MK_LBUTTON or MK_RBUTTON) = 0) then
  begin
    ContinueDrop := True;
    if ContinueDrop then
      Result := DRAGDROP_S_DROP
    else
      Result := DRAGDROP_S_CANCEL;
  end
  else
    Result := S_OK;
end;

function TDropSource.GiveFeedback(dwEffect: Integer): HRESULT;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

{ IDataObject }

function TDropSource.dAdvise(const FormatEtc: TFormatEtc; advf: Longint; const advsink: IAdviseSink;
  out dwConnection: Longint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.dUnadvise(dwConnection: Longint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.EnumFormatEtc(dwDirection: Longint; out EnumFormatEtc: IEnumFormatEtc): HRESULT;
begin
  if (dwDirection = DATADIR_GET) then
    Result := OleRegEnumFormatEtc(IEnumFormatEtc, dwDirection, EnumFormatEtc)
  else
    Result := E_NOTIMPL;
end;

function TDropSource.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc; out FormatEtcout: TFormatEtc): HRESULT;
begin
  Result := DATA_S_SAMEFORMATETC;
end;

function TDropSource.EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
begin
  Result := (FormatEtc1.cfFormat = FormatEtc2.cfFormat) and (FormatEtc1.ptd = FormatEtc2.ptd) and
    (FormatEtc1.dwAspect = FormatEtc2.dwAspect) and (FormatEtc1.lindex = FormatEtc2.lindex) and
    (FormatEtc1.Tymed = FormatEtc2.Tymed)
end;

function TDropSource.FindFormatEtc(TestFormatEtc: TFormatEtc): Integer;
var
  I: Integer;
  Found: Boolean;
begin
  I := 0;
  Found := False;
  Result := -1;
  while (I < Length(Formats)) and not Found do
  begin
    Found := EqualFormatEtc(Formats[I].FormatEtc, TestFormatEtc);
    if Found then
      Result := I;
    Inc(I);
  end
end;

function TDropSource.HGlobalClone(HGLOBAL: THandle): THandle;
// Returns a global memory block that is a copy of the passed memory block.
var
  Size: LongWord;
  Data, NewData: PByte;
begin
  Size := GlobalSize(HGLOBAL);
  Result := GlobalAlloc(GPTR, Size);
  Data := GlobalLock(HGLOBAL);
  try
    NewData := GlobalLock(Result);
    try
      Move(Data, NewData, Size);
    finally
      GlobalUnlock(Result);
    end
  finally
    GlobalUnlock(HGLOBAL)
  end
end;

function TDropSource.RetrieveOwnedStgMedium(Format: TFormatEtc; var StgMedium: TStgMedium): HRESULT;
var
  I: Integer;
begin
  Result := E_INVALIDARG;
  I := FindFormatEtc(Format);
  if (I > -1) and Formats[I].OwnedByDataObject then
    Result := StgMediumIncRef(Formats[I].StgMedium, StgMedium, False)
end;

function TDropSource.StgMediumIncRef(const InStgMedium: TStgMedium; var OutStgMedium: TStgMedium;
  CopyInMedium: Boolean): HRESULT;
begin
  Result := S_OK;
  // Simply copy all fields to start with.
  OutStgMedium := InStgMedium;
  case InStgMedium.Tymed of
    TYMED_HGLOBAL:
      begin
        if CopyInMedium then
        begin
          // Generate a unique copy of the data passed
          OutStgMedium.HGLOBAL := HGlobalClone(InStgMedium.HGLOBAL);
          if OutStgMedium.HGLOBAL = 0 then
            Result := E_OUTOFMEMORY
        end
        else
          // Don't generate a copy just use ourselves and the copy previoiusly saved
          MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject) // Does increase RefCount
      end;
    TYMED_FILE:
      begin
        if CopyInMedium then
        begin
          OutStgMedium.lpszFileName := CoTaskMemAlloc(lstrLenW(InStgMedium.lpszFileName));
          // !!          StrCopyW(PChar(OutStgMedium.lpszFileName), PChar(InStgMedium.lpszFileName))
        end
        else
          MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject) // Does increase RefCount
      end;
    TYMED_ISTREAM:
                                              
      IUnknown(MySTGMEDIUM(OutStgMedium).stm)._AddRef;
    TYMED_ISTORAGE:
      IUnknown(MySTGMEDIUM(OutStgMedium).stg)._AddRef;
    TYMED_GDI:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject)
        // Does not increase RefCount
      else
        Result := DV_E_TYMED; // Don't know how to copy GDI objects right now
    TYMED_MFPICT:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject)
        // Does not increase RefCount
      else
        Result := DV_E_TYMED;
    // Don't know how to copy MetaFile objects right now
    TYMED_ENHMF:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject)
        // Does not increase RefCount
      else
        Result := DV_E_TYMED;
    // Don't know how to copy enhanced metafiles objects right now
  else
    Result := DV_E_TYMED
  end;

  // I still have to do this. The Compiler will call _Release on the above Self as IDataObject
  // casts which is not what is necessary.  The DataObject is released correctly.
  if (MySTGMEDIUM(OutStgMedium).UnkForRelease <> nil) and (Result = S_OK) then
    IUnknown(MySTGMEDIUM(OutStgMedium).UnkForRelease)._AddRef
end;

function TDropSource.GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HRESULT;
var
  Global: Cardinal;
  P: Pointer;
  TextData: string;
  B: TBitmap;
  BitmapHandle: HBITMAP;
begin
  FillChar(Medium, SizeOf(Medium), 0);
  Result := DV_E_FORMATETC;
  if QueryGetData(FormatEtcIn) <> S_OK then
    Exit;

  case FormatEtcIn.cfFormat of
    CF_UNICODETEXT:
      begin
        TextData := Data.Data.ToString;
        Global := GlobalAlloc(0, (Length(TextData) + 1) * 2);
        P := GlobalLock(Global);
        try
          Move(PChar(TextData)^, P^, GlobalSize(Global));
        finally
          GlobalUnlock(Global);
        end;
        Medium.Tymed := TYMED_HGLOBAL;
        Medium.HGLOBAL := Global;
        Result := S_OK;
      end;
    CF_BITMAP:
      begin
        BitmapHandle := 0;
        if Data.Data.IsType<TBitmap> then
          BitmapHandle := BitmapToWinBitmap(TBitmap(Data.Data.AsObject), TAlphaColorRec.White)
        else if Data.Data.IsType<TBitmapSurface> then
        begin
          B := TBitmap.Create;
          try
            B.Assign(TBitmapSurface(Data.Data.AsObject));
            BitmapHandle := BitmapToWinBitmap(B, TAlphaColorRec.White);
          finally
            B.Free;
          end;
        end;
        if BitmapHandle <> 0 then
        begin
          Medium.Tymed := TYMED_GDI;
          Medium.HBITMAP := BitmapHandle;
          Result := S_OK;
        end;
      end;
    CF_FMOBJECT:
      if PlatformWin.FDragAndDropActive then
      begin
        Medium.Tymed := TYMED_HGLOBAL;
        Medium.HGLOBAL := THandle(Self);
        Result := S_OK;
      end;
  end;
  if (Result <> S_OK) and (Formats <> nil) then
  begin
    Result := QueryGetData(FormatEtcIn);
    if (Result = S_OK) and (RetrieveOwnedStgMedium(FormatEtcIn, Medium) = E_INVALIDARG) then
      Result := E_UNEXPECTED
  end
end;

function TDropSource.GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TDropSource.QueryGetData(const FormatEtc: TFormatEtc): HRESULT;
var
  I: Integer;
begin
  Result := DV_E_FORMATETC;
  case FormatEtc.cfFormat of
    CF_UNICODETEXT:
      if not Data.Data.IsObject then
        Result := S_OK;
    CF_BITMAP:
      if Data.Data.IsObject and (Data.Data.IsType<TBitmap> or Data.Data.IsType<TBitmapSurface>) then
        Result := S_OK;
    CF_FMOBJECT:
      if PlatformWin.FDragAndDropActive then
        Result := S_OK;
  end;
  if Result <> S_OK then
  begin
    if Formats <> nil then
    begin
      I := 0;
      Result := DV_E_FORMATETC;
      while (I < Length(Formats)) and (Result = DV_E_FORMATETC) do
      begin
        if Formats[I].FormatEtc.cfFormat = FormatEtc.cfFormat then
        begin
          if (Formats[I].FormatEtc.dwAspect = FormatEtc.dwAspect) then
          begin
            if (Formats[I].FormatEtc.Tymed and FormatEtc.Tymed <> 0) then
              Result := S_OK
            else
              Result := DV_E_TYMED;
          end
          else
            Result := DV_E_DVASPECT;
        end
        else
          Result := DV_E_FORMATETC;
        Inc(I)
      end
    end
    else
      Result := E_UNEXPECTED;
  end;
end;

function TDropSource.CanonicalIUnknown(const TestUnknown: IUnknown): IUnknown;
// Uses COM object identity: An explicit call to the IUnknown::QueryInterface
// method, requesting the IUnknown interface, will always return the same
// pointer.
begin
  if TestUnknown <> nil then
  begin
    if Supports(TestUnknown, IUnknown, Result) then
      IUnknown(Result)._Release
      // Don't actually need it just need the pointer value
    else
      Result := TestUnknown
  end
  else
    Result := TestUnknown
end;

function TDropSource.SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: BOOL): HRESULT;
var
  Index: Integer;
begin
  // See if we already have a format of that type available.
  Index := FindFormatEtc(FormatEtc);
  if Index > -1 then
  begin
    // Yes we already have that format type stored.  Just use the TClipboardFormat
    // in the List after releasing the data
    ReleaseStgMedium(Formats[Index].StgMedium);
    FillChar(Formats[Index].StgMedium, SizeOf(Formats[Index].StgMedium), #0);
  end
  else
  begin
    // It is a new format so create a new TDataObjectInfo record and store it in
    // the Format array
    SetLength(Formats, Length(Formats) + 1);
    Formats[Length(Formats) - 1].FormatEtc := FormatEtc;
    Index := Length(Formats) - 1;
  end;
  // The data is owned by the TClipboardFormat object
  Formats[Index].OwnedByDataObject := True;

  if fRelease then
  begin
    // We are simply being given the data and we take control of it.
    Formats[Index].StgMedium := Medium;
    Result := S_OK
  end
  else
    // We need to reference count or copy the data and keep our own references
    // to it.
    Result := StgMediumIncRef(Medium, Formats[Index].StgMedium, True);

  // Can get a circular reference if the client calls GetData then calls
  // SetData with the same StgMedium.  Because the unkForRelease and for
  // the IDataObject can be marshalled it is necessary to get pointers that
  // can be correctly compared.
  // See the IDragSourceHelper article by Raymond Chen at MSDN.
  if MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease <> nil then
  begin
    if CanonicalIUnknown(Self) = CanonicalIUnknown(IUnknown(MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease)) then
    begin
      IUnknown(MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease)._Release;
      MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease := nil
    end;
  end;
end;

{ Platform DragDrop }

procedure TPlatformWin.AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Include(FEnabledInteractiveGestures, ARec);
end;

procedure TPlatformWin.BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);
var
  DropSource: TDropSource;
  DropEffect: Longint;
  DragSourceHelper: IDragSourceHelper;
  SHDRAGIMAGE: TSHDRAGIMAGE;
  DragMousePos, Offset: TPointF;
  Control: IControl;
begin
  DropSource := TDropSource.Create(nil);
  try
    DropSource.Data := Data;

    DragMousePos := GetMousePos;
    // CoCreateInstance takes too long to execute so mouse position becomes different when called later
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IDragSourceHelper, DragSourceHelper)
      ))
      and (DragSourceHelper <> nil) then
    begin
      if Data.Source is TControl then
        Offset := TControl(Data.Source).AbsoluteToLocal(AForm.ScreenToClient(DragMousePos))
      else
        Offset := PointF((ABitmap.Width div 2), ABitmap.Height div 2);

      FillChar(SHDRAGIMAGE, SizeOf(SHDRAGIMAGE), 0);
      SHDRAGIMAGE.sizeDragImage.cx := ABitmap.Width;
      SHDRAGIMAGE.sizeDragImage.cy := ABitmap.Height;
      SHDRAGIMAGE.ptOffset.X := Round(Offset.X);
      SHDRAGIMAGE.ptOffset.Y := Round(Offset.Y);
      SHDRAGIMAGE.hbmpDragImage := BitmapToWinBitmap(ABitmap, True);
      if not Succeeded(DragSourceHelper.InitializeFromBitmap(@SHDRAGIMAGE, DropSource)) then
        DeleteObject(SHDRAGIMAGE.hbmpDragImage);
    end;

    if not IsThemeActive then
    begin
      SetCursor(crDrag);
      FDragAndDropActive := True;
      try
        DoDragDrop(DropSource, DropSource, DROPEFFECT_LINK or DROPEFFECT_COPY or DROPEFFECT_MOVE, DropEffect);
      finally
        FDragAndDropActive := False;
        SetCursor(crDefault);
      end;
    end
    else
    try
      FDragAndDropActive := True;
      DoDragDrop(DropSource, DropSource, DROPEFFECT_LINK or DROPEFFECT_COPY or DROPEFFECT_MOVE, DropEffect);
    finally
      FDragAndDropActive := False;
    end;

    if (DropEffect = 0) and Supports(Data.Source, IControl, Control) then
      Control.DragEnd;

    if DragSourceHelper <> nil then
      DragSourceHelper := nil;
  finally
    DropSource.Free;
  end;
end;

{ Mouse }

procedure TPlatformWin.SetCursor(const ACursor: TCursor);
const
  CustomCursorMap: array [crSizeAll .. crNone] of PChar = (
    nil, nil, nil, nil, nil, { IDC_SQLWAIT } PChar(32755), { IDC_MULTIDRAG } PChar(32756), nil, nil, { IDC_NODROP } PChar(32760), { IDC_DRAG } PChar(32759), nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil);
const
  CursorMap: array [crSizeAll .. crNone] of PChar = (
    IDC_SIZEALL, IDC_HAND, IDC_HELP, IDC_APPSTARTING, IDC_NO, nil, nil, IDC_SIZENS, IDC_SIZEWE, nil, nil, IDC_WAIT,
    IDC_UPARROW, IDC_SIZEWE, IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZEALL, IDC_IBEAM, IDC_CROSS, IDC_ARROW, nil);
var
  NewCursor: HCURSOR;
begin
  if not FDragAndDropActive then
  begin
    // We don't set cursor by default, when we create window. So we should use crArrow cursor by default.
    if (ACursor = crDefault) and not (csDesigning in Application.ComponentState) then
      FCursor := crArrow
    else
      FCursor := ACursor;

    if FCursor < 0 then
    begin
      if CustomCursorMap[FCursor] <> nil then
        NewCursor := LoadCursorW(HInstance, CustomCursorMap[FCursor])
      else
        NewCursor := LoadCursorW(0, CursorMap[FCursor]);
      Winapi.Windows.SetCursor(NewCursor);
    end;
  end;
end;

function TPlatformWin.GetCursor: TCursor;
begin
  Result := FCursor;
end;

function TPlatformWin.GetFullScreen(const AForm: TCommonCustomForm): Boolean;
var
  LFSParam: TFullScreenParams;
begin
  Result := False;
  if FFullScreenSupport.TryGetValue(AForm, LFSParam) then
    Result := LFSParam.IsFullScreen;
end;

function TPlatformWin.GetImmManager(const Index: TCommonCustomForm): TImmManager;
begin
  Result := FImmManagers[Index];
end;

procedure TPlatformWin.SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
var
  LFSParam: TFullScreenParams;
  LClean: TFullScreenParams;
begin
  RaiseIfNil(AForm, 'AForm');

  if AValue and not (TFmxFormState.Showing in AForm.FormState) then
    AForm.Visible := True;
  FillChar(LFSParam, SizeOf(LFSParam), 0);
  if not FFullScreenSupport.TryGetValue(AForm, LFSParam) then
    FFullScreenSupport.Add(AForm, LFSParam);
  if AValue and (AForm.Visible or (TFmxFormState.Showing in AForm.FormState)) then
  begin
    LFSParam.WindowState := AForm.WindowState;
    LFSParam.BorderStyle := AForm.BorderStyle;
    if AForm.WindowState = TWindowState.wsNormal then
    begin
      LFSParam.Size := Point(AForm.Width, AForm.Height);
      LFSParam.Position := Point(AForm.Left, AForm.Top);
    end;
    FFullScreenSupport.Items[AForm] := LFSParam;
    if AForm.WindowState = TWindowState.wsMinimized then
      AForm.WindowState := TWindowState.wsMaximized;
    AForm.BorderStyle := TFmxFormBorderStyle.None;
    AForm.WindowState := TWindowState.wsMaximized;
  end
  else
  begin
    LClean := LFSParam;
    LClean.Clean;
    FFullScreenSupport.Items[AForm] := LClean;
    if (LFSParam.Size.X > 0) and (LFSParam.Size.Y > 0) then
    begin
      AForm.BorderStyle := LFSParam.BorderStyle;
      AForm.SetBounds(LFSParam.Position.X, LFSParam.Position.Y, LFSParam.Size.X, LFSParam.Size.Y);
      AForm.WindowState := LFSParam.WindowState;
    end;
  end;
end;

procedure TPlatformWin.SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
begin
end;

function TPlatformWin.GetMousePos: TPointF;
var
  P: TPoint;
  Scale: Single;
begin
  GetCursorPos(P);
  Scale := GetScreenScale;
  Result := TPoint.Create(Round(P.X / Scale), Round(P.Y / Scale));
end;

{ Screen }

function TPlatformWin.GetScreenSize: TPointF;
var
  WR: TRect;
  Scale: Single;
begin
  Winapi.Windows.GetWindowRect(GetDesktopWindow, WR);
  Scale := GetScreenScale;
  Result := PointF(WR.Width / Scale, WR.Height / Scale);
end;

function TPlatformWin.GetScreenScale: Single;
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    Result := GetDCScale(DC);
  finally
    ReleaseDC(0, DC);
  end;
end;

function TPlatformWin.GetScreenOrientation: TScreenOrientation;
begin
  Result := TScreenOrientation.Landscape;
end;

procedure TPlatformWin.SetSupportedScreenOrientations(const AOrientations: TScreenOrientations);
begin
  // Not needed for Windows
end;

{ IFMXDeviceService }

function TPlatformWin.GetFeatures: TDeviceFeatures;
var
  Value: Integer;
begin
  Value := GetSystemMetrics(SM_DIGITIZER);
  if ((Value and NID_READY) = NID_READY) and (((Value and NID_MULTI_INPUT) = NID_MULTI_INPUT)) then
    Result := [TDeviceFeature.HasTouchScreen]
  else
    Result := [];
end;

function TPlatformWin.GetFirstWeekday: Byte;
const
  MondayOffset = 1;
var
  buffer: DWORD;
begin
  // On Windows zero index corresponds to Monday, so we need to add offset to match DayMonday = 1 in RTL
  if GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, @buffer, SizeOf(buffer) div SizeOf(Char)) > 0 then
    Result := buffer - Ord('0') + MondayOffset
  else
    Result := DayMonday;
end;

function TPlatformWin.GetFormInfo(const Index: TCommonCustomForm): TFormInfo;
begin
  Result := FFormsInfo[Index];
end;

function TPlatformWin.GetModel: string;
begin
  Result := '';
end;

function TPlatformWin.GetDeviceClass: TDeviceInfo.TDeviceClass;
begin
  if (GetSystemMetrics(SM_TABLETPC) <> 0) and (GetSystemMetrics(SM_DIGITIZER) and NID_MULTI_INPUT = NID_MULTI_INPUT)
  then
    Result := TDeviceInfo.TDeviceClass.Tablet
  else
    Result := TDeviceInfo.TDeviceClass.Desktop;
end;

{ IFMXSystemInformationService }

function TPlatformWin.GetScrollingBehaviour: TScrollingBehaviours;
var
  Value: Integer;
begin
  Value := GetSystemMetrics(SM_DIGITIZER);
  if ((Value and NID_READY) = NID_READY) and (((Value and NID_MULTI_INPUT) = NID_MULTI_INPUT)) then
    Result := [TScrollingBehaviour.Animation, TScrollingBehaviour.TouchTracking]
  else
    Result := [];
end;

function TPlatformWin.GetMinScrollThumbSize: Single;
begin
  Result := 15;
end;

function TPlatformWin.GetMenuShowDelay: Integer;
begin
  if SystemParametersInfo(SPI_GETMENUSHOWDELAY, 0, @Result, 0) then
    Result := Result div 2
  else
    Result := 0;
end;

function TPlatformWin.GetCurrentLangID: string;
var
  buffer: MarshaledString;
  UserLCID: LCID;
  buflen: Integer;
begin
  // defaults
  UserLCID := GetUserDefaultLCID;
  buflen := GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, nil, 0);
  buffer := StrAlloc(buflen);
  if GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, buffer, buflen) <> 0 then
    Result := buffer
  else
    Result := 'en';
  StrDispose(buffer);
end;

function TPlatformWin.GetDefaultFontFamilyName: string;
begin
  if TOSVersion.Check(6) then
    Result := 'Segoe UI'
  else
    Result := 'Tahoma';
end;

function TPlatformWin.GetDefaultFontSize: Single;
begin
  Result := DefaultWindowsFontSize;
end;

function TPlatformWin.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  if AComponent = TComponentKind.Calendar then
    Result := TSize.Create(202, 180)
  else
    Result := TSize.Create(80, 22);
end;

function TPlatformWin.GetLocaleFirstDayOfWeek: string;
var
  buffer: DWORD;
begin
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, @buffer, SizeOf(buffer) div SizeOf(Char));
  Result := Chr(buffer);
end;

function TPlatformWin.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  RaiseIfNil(AHandle, 'AHandle');

  if AHandle is TWinWindowHandle then
    Result := TWinWindowHandle(AHandle).Form
  else
    Result := nil;
end;

procedure TPlatformWin.Log(const AFormat: string; const AParams: array of const);
begin
  if Length(AParams) = 0 then
    OutputDebugString(PChar(AFormat))
  else
    OutputDebugString(PChar(Format(AFormat, AParams)));
end;

function TPlatformWin.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [];
end;

function TPlatformWin.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop];
end;

function TPlatformWin.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [];
end;

function TPlatformWin.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [];
end;

function TPlatformWin.GetSaveStateFileName(const ABlockName: string): string;
const
  Prefix = '~';
  Separator = '_';
var
  S: TStringBuilder;
  FilePath: string;
begin
  if FSaveStateStoragePath.IsEmpty then
    FilePath := TPath.GetTempPath
  else
    FilePath := FSaveStateStoragePath;
  S := TStringBuilder.Create(FilePath.Length + Length(Prefix) + Length(Separator) + ABlockName.Length);
  try
    S.Append(FilePath);
    S.Append(Prefix);
    S.Append(ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
    S.Append(Separator);
    S.Append(ABlockName);
    Result := S.ToString(True);
  finally
    S.Free;
  end;
end;

function TPlatformWin.GetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

  procedure ReadPersistent(const AFileName: string);
  var
    S: TFileStream;
  begin
    S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      ABlockData.CopyFrom(S, S.Size);
    finally
      S.Free;
    end;
  end;

var
  LFileName: string;
begin
  if ABlockName.IsEmpty or (ABlockData = nil) then
    Exit(False);
  LFileName := GetSaveStateFileName(ABlockName);
  if not TFile.Exists(LFileName) then
    Exit(False);
  try
    ReadPersistent(LFileName);
  except
    Exit(False);
  end;
  Result := True;
end;

function TPlatformWin.SetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

  procedure WritePersistent(const AFileName: string);
  var
    S: TFileStream;
  begin
    S := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
    try
      ABlockData.Seek(0, TSeekOrigin.soBeginning);
      S.CopyFrom(ABlockData, ABlockData.Size);
    finally
      S.Free;
    end;
  end;

var
  LFileName: string;
begin
  if ABlockName.IsEmpty then
    Exit(False);
  LFileName := GetSaveStateFileName(ABlockName);
  if (ABlockData = nil) or (ABlockData.Size < 1) then
  begin
    if TFile.Exists(LFileName) then
      TFile.Delete(LFileName);
  end
  else
    try
      WritePersistent(LFileName);
    except
      Exit(False);
    end;
  Result := True;
end;

function TPlatformWin.GetSaveStateStoragePath: string;
begin
  Result := FSaveStateStoragePath;
end;

procedure TPlatformWin.SetSaveStateStoragePath(const ANewPath: string);
begin
  if not ANewPath.IsEmpty then
    FSaveStateStoragePath := IncludeTrailingPathDelimiter(ANewPath)
  else
    FSaveStateStoragePath := '';
end;

function TPlatformWin.GetSaveStateNotifications: Boolean;
begin
  Result := False;
end;

function TPlatformWin.GetDisplayMetrics: TDeviceDisplayMetrics;
var
  R: TRect;
begin
  Winapi.Windows.GetWindowRect(GetDesktopWindow, R);
  Result.PhysicalScreenSize := TSize.Create(R.Width, R.Height);
  Result.RawScreenSize := Result.PhysicalScreenSize;
  Result.LogicalScreenSize := Result.PhysicalScreenSize;
  if Result.PhysicalScreenSize.cx > 0 then
    Result.AspectRatio := Result.PhysicalScreenSize.cy / Result.PhysicalScreenSize.cx
  else
    Result.AspectRatio := 1;
  Result.PixelsPerInch := 96; // Windows Default
  Result.ScreenScale := 1;
  Result.FontScale := 1;
end;

function TPlatformWin.RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
begin
    Result := FKeyMapping.RegisterKeyMapping(PlatformKey, VirtualKey, KeyKind);
end;

function TPlatformWin.UnregisterKeyMapping(const PlatformKey: Word): Boolean;
begin
    Result := FKeyMapping.UnregisterKeyMapping(PlatformKey);
end;

function TPlatformWin.PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
begin
  Result := FKeyMapping.PlatformKeyToVirtualKey(PlatformKey, KeyKind);
end;

function TPlatformWin.VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
begin
  Result := FKeyMapping.VirtualKeyToPlatformKey(VirtualKey);
end;

procedure RegisterApplicationHWNDProc(const Proc: TApplicationHWNDProc);
begin
  if PlatformWin <> nil then
    PlatformWin.ApplicationHWNDProc := Proc
  else
    raise EArgumentNilException.Create(SArgumentNil);
end;

{ TVirtualKeyboardWin }

constructor TVirtualKeyboardWin.Create;
var
  L: Integer;
  S: string;
  HID: HKey;
begin
  S := '';
  inherited Create;
  SetLength(S, MAX_PATH);
  L := GetSystemDirectory(PChar(S), MAX_PATH);
  SetLength(S, L);
  FPath := S;
  FExeName := 'osk.exe';
  FWndClassName := 'OSKMainClass';
  FKBPresent := True;
  if not TOSVersion.Check(6, 2) then
  begin
    if Winapi.Windows.RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Enum', 0, KEY_READ,
      HID) = ERROR_SUCCESS then
      try
        S := FindKeyValue(HID, 'ClassGUID', '{4D36E96B-E325-11CE-BFC1-08002BE10318}', 'Control',
          'ActiveService');
        FKBPresent := S <> '';
      finally
        RegCloseKey(HID);
      end;
  end;
  FNewvkbState := vkbState;
  StartTimerLang;
end;

procedure TVirtualKeyboardWin.Clear;
var
  H: HWND;
begin
  H := vkbHandle;
  if (H <> 0) and (FInst > 32) then
  begin
    PostMessage(H, WM_SYSCOMMAND, SC_CLOSE, 0);
  end;
  KillTimerVisible;
  KillTimerLang;
  FInst := 0;
  FError := False;
  FLastTime := 0;
  FLastHandle := 0;
end;

destructor TVirtualKeyboardWin.Destroy;
begin
  Clear;
  inherited;
end;

function TVirtualKeyboardWin.FindKeyValue(const Key: HKey; const Name, Value, SubKeyName, SubValueName: string): string;
var
  Buf, Val: string;
  R, I, J: Integer;
  SubKey: HKey;
  BufSize, T, ValSize: Cardinal;
begin
  Result := '';
  I := 0;
  Buf := '';
  Val := '';
  BufSize := 2048;
  SetLength(Buf, BufSize);
  ValSize := BufSize;
  SetLength(Val, ValSize);
  repeat
    BufSize := Length(Buf);
    ValSize := Length(Val);
    R := Winapi.Windows.RegEnumValue(Key, I, @Buf[1], BufSize, nil, @T, @Val[1], @ValSize);
    if (R = ERROR_SUCCESS) then
    begin
      if (string(PChar(Buf)) = Name) and (T = REG_SZ) and (SameText(string(PChar(Val)), Value)) then
      begin
        if Winapi.Windows.RegOpenKeyEx(Key, PChar(SubKeyName), 0, KEY_READ, SubKey) = ERROR_SUCCESS
        then
          try
            J := 0;
            repeat
              BufSize := Length(Buf);
              ValSize := Length(Val);
              R := Winapi.Windows.RegEnumValue(SubKey, J, @Buf[1], BufSize, nil, @T, @Val[1],
                @ValSize);
              if (R = ERROR_SUCCESS) and (string(PChar(Buf)) = SubValueName) and (T = REG_SZ) and
                (string(PChar(Val)) <> '') then
              begin
                Result := string(PChar(Val));
              end;
              Inc(J);
            until (Result <> '') or (R <> ERROR_SUCCESS);
          finally
            RegCloseKey(SubKey);
          end;
      end;
      Inc(I);
    end;
  until (Result <> '') or (R <> ERROR_SUCCESS);
  if Result = '' then
  begin
    I := 0;
    repeat
      R := Winapi.Windows.RegEnumKey(Key, I, PChar(Buf), BufSize);
      if R = ERROR_SUCCESS then
      begin
        if Winapi.Windows.RegOpenKeyEx(Key, PChar(Buf), 0, KEY_READ, SubKey) = ERROR_SUCCESS then
          try
            Result := FindKeyValue(SubKey, Name, Value, SubKeyName, SubValueName);
          finally
            RegCloseKey(SubKey);
          end;
        Inc(I);
      end;
    until (Result <> '') or (R <> ERROR_SUCCESS);
  end;
end;

function TVirtualKeyboardWin.GetVirtualKeyboardState: TVirtualKeyboardStates;
var
  LState: TvkbState;
begin
  if FError then
    Result := [TVirtualKeyboardState.Error]
  else
    Result := [];
  if IsAutoShow then
    Result := Result + [TVirtualKeyboardState.AutoShow];
  if not FError then
  begin
    if Abs(Now - FLastTime) > 1 / SecsPerDay then
      LState := vkbState
    else
      LState := FLastvkbState;
    if LState = TvkbState.Shown then
      Result := Result + [TVirtualKeyboardState.Visible];
  end;
end;

function TVirtualKeyboardWin.GetVKBounds: TRect;
begin
  if FLastHandle <> 0 then
    GetWindowRect(FLastHandle, Result);
end;

function TVirtualKeyboardWin.HideVirtualKeyboard: Boolean;
begin
  Result := not FError;
  if (not FError) then
  begin
    if IsAutoShow then
      FNewvkbState := TvkbState.Hidden
    else
      FNewvkbState := TvkbState.None;
    if FNewvkbState <> vkbState then
    begin
      StartTimerVisible;
    end;
  end;
end;

procedure TVirtualKeyboardWin.SetTransientState(Value: Boolean);
begin
end;

function TVirtualKeyboardWin.ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
var
  Root: IRoot;
begin
  Result := not FError;
  if (not FError) then
  begin
    FNewvkbState := TvkbState.Shown;
    if FNewvkbState <> vkbState then
      StartTimerVisible;
    FWait := True;
    FFormHandle := 0;
    if (AControl <> nil) then
    begin
      Root := AControl.Root;
      if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
        FFormHandle := FormToHWND(TCommonCustomForm(Root.GetObject));
    end;
  end;
end;

function TVirtualKeyboardWin.IsAutoShow: Boolean;
begin
  Result := (VKAutoShowMode = TVKAutoShowMode.Always) or ((VKAutoShowMode = TVKAutoShowMode.DefinedBySystem) and
    (not FKBPresent));
end;

procedure TVirtualKeyboardWin.KillTimerLang;
begin
  if FHTmerLang <> 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FTimerService.DestroyTimer(FHTmerLang);
      FHTmerLang := 0;
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TVirtualKeyboardWin.StartTimerLang;
begin
  if FHTmerLang = 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FHTmerLang := FTimerService.CreateTimer(250, TimerLangProc);
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TVirtualKeyboardWin.TimerLangProc;
var
  LCodeKeyboard: HKL;
begin
  if FStepActivate > 0 then
  begin
    FLastHandle := vkbHandle;
    case FStepActivate of
      1:
        begin
          SetActiveWindow(FLastHandle);
          SetFocus(FLastHandle);
        end;
      4:
        begin
          SetActiveWindow(FFormHandle);
        end;
      5:
        begin
          SetFocus(FFormHandle);
          FCodeKeyboard := GetKeyboardLayout(0);
        end;
    end;
    if FStepActivate = 5 then
      FStepActivate := 0
    else
    begin
      Inc(FStepActivate);
      Exit;
    end;
  end
  else
  begin
    if vkbState = TvkbState.Shown then
    begin
      LCodeKeyboard := GetKeyboardLayout(0);
      if FCodeKeyboard <> LCodeKeyboard then
      begin
        SetActiveWindow(0);
        SetActiveWindow(FFormHandle);
        SetFocus(FFormHandle);
        FCodeKeyboard := LCodeKeyboard;
      end;
    end;
  end;
end;

procedure TVirtualKeyboardWin.KillTimerVisible;
begin
  if FHTmerVisible <> 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FTimerService.DestroyTimer(FHTmerVisible);
      FHTmerVisible := 0;
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TVirtualKeyboardWin.StartTimerVisible;
begin
  if FHTmerVisible = 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FHTmerVisible := FTimerService.CreateTimer(100, TimerVisibleProc);
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TVirtualKeyboardWin.TimerVisibleProc;
var
  LState: TvkbState;
  procedure Quit;
  begin
    if FLastHandle <> 0 then
      PostMessage(FLastHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
    Sleep(40);
    FLastHandle := 0;
    TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(False, TRect.Empty), True);
  end;
  procedure Restore;
  begin
    if FLastHandle <> 0 then
    begin
      if Winapi.Windows.GetActiveWindow <> FLastHandle then
      begin
        SendMessage(FLastHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
        TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(True, GetVKBounds), True);
      end;
    end;
  end;
  procedure Hide;
  begin
    if FLastHandle <> 0 then
      PostMessage(FLastHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
    FWait := True;
    FLastHandle := 0;
    TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(False, TRect.Empty), True);
  end;

begin
  if FWait then
  begin
    FLastHandle := vkbHandle;
    FWait := False;
    Exit;
  end;
  FWait := True;
  LState := vkbState;
  if LState <> FNewvkbState then
  begin
    case LState of
      TvkbState.None:
        case FNewvkbState of
          TvkbState.Hidden: { none }
            ;
          TvkbState.Shown:
            begin
              vkbExecute(FFormHandle);
              FWait := False;
              FStepActivate := 1;
              Exit;
            end;
        end;
      TvkbState.Hidden:
        case FNewvkbState of
          TvkbState.None:
            Quit;
          TvkbState.Shown:
            Restore;
        end;
      TvkbState.Shown:
        case FNewvkbState of
          TvkbState.None:
            Quit;
          TvkbState.Hidden:
            Hide;
        end;
    end;
    FNewvkbState := vkbState;
  end
  else if (FNewvkbState = TvkbState.Shown) and (FStepActivate = 1) then
    // Here we are sending a deferred message, otherwise there will be incorrect coordinates
    TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(True, GetVKBounds), True);
  KillTimerVisible;
end;

procedure TVirtualKeyboardWin.vkbExecute(FormHandle: HWND);

  function IsFileExisted(const AFileName: string): Boolean;
  begin
    if FileExists(AFileName) then
      Exit(True);

    TWow64Redirection.Current.Disable;
    try
      Result := FileExists(AFileName);
    finally
      TWow64Redirection.Current.Restore;
    end;
  end;

  function LaunchVirtualKeyboardApp(const AFileName: string): HINST;
  begin
    TWow64Redirection.Current.Disable;
    try
      Result := ShellExecute(FormHandle, 'open', PChar(AFileName), nil, PChar(ExtractFileDir(AFileName)), SW_SHOWNOACTIVATE);
    finally
      TWow64Redirection.Current.Restore;
    end;
  end;

  function WaitLaunchingVirtualKeyboardApp: Boolean;
  const
    StepPause = 40; //msec
    MaxAttemptsCount = 100;
  var
    AttemptsCount: Integer;
  begin
    AttemptsCount := 0;
    while (AttemptsCount < MaxAttemptsCount) and (vkbState = TvkbState.None) do
    begin
      Inc(AttemptsCount);
      Sleep(StepPause);
    end;
    Result := AttemptsCount < MaxAttemptsCount;
  end;

var
  VKAppFileName: string;
  H: HWND;
begin
  if FError then
    Exit;
  H := vkbHandle;
  if H = 0 then
  begin
    VKAppFileName := TPath.Combine(Path, ExeName);

    if IsFileExisted(VKAppFileName) then
      FInst := LaunchVirtualKeyboardApp(VKAppFileName)
    else
      FInst := 0;

    if FInst <= 32 then
      FError := True
    else if not WaitLaunchingVirtualKeyboardApp then
    begin
      FInst := 0;
      FError := True;
    end;
  end;
end;

function TVirtualKeyboardWin.vkbHandle: HWND;
begin
  Result := Winapi.Windows.FindWindow(PChar(FWndClassName), nil);
end;

function TVirtualKeyboardWin.vkbState: TvkbState;
var
  H: HWND;
begin
  H := vkbHandle;
  if (H <> INVALID_HANDLE_VALUE) and (H <> 0) then
  begin
    if (not IsWindowVisible(H)) or (IsIconic(H)) then
      Result := TvkbState.Hidden
    else
      Result := TvkbState.Shown;
    FLastHandle := H;
  end
  else
  begin
    Result := TvkbState.None;
    FLastHandle := 0;
  end;
  FLastvkbState := Result;
  FLastTime := Now;
end;

{ TOrderedDictionary<TKey, TValue> }

procedure TOrderedDictionary<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  Index: Integer;
begin
  Index := FValues.Add(AValue);
  FIndex.Add(AKey, Index);
end;

procedure TOrderedDictionary<TKey, TValue>.Clear;
begin
  FValues.Clear;
  FIndex.Clear;
end;

constructor TOrderedDictionary<TKey, TValue>.Create;
begin
  FValues := TList<TValue>.Create;
  FIndex := TDictionary<TKey, Integer>.Create;
end;

destructor TOrderedDictionary<TKey, TValue>.Destroy;
begin
  FreeAndNil(FIndex);
  FreeAndNil(FValues);
  inherited;
end;

function TOrderedDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TOrderedDictionary<TKey, TValue>.GetValues(const AIndex: Integer): TValue;
begin
  Result := FValues[AIndex];
end;

function TOrderedDictionary<TKey, TValue>.TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
var
  Index: Integer;
begin
  Result := FIndex.TryGetValue(AKey, Index);
  if Result then
    AValue := FValues[Index];
end;

{ TMultiDisplayWin }

procedure TMultiDisplayWin.AddDisplay(const AMonitorHandle: HMONITOR);
var
  MonInfo: TMonitorInfo;
  Scale: Single;
  IsPrimary: Boolean;
  Bounds: TRect;
  WorkArea: TRect;
  Display: TDisplay;
begin
  MonInfo.cbSize := SizeOf(MonInfo);
  if GetMonitorInfo(AMonitorHandle, @MonInfo) then
  begin
    Scale := GetMonitorScale(AMonitorHandle);
    IsPrimary := (MonInfo.dwFlags and MONITORINFOF_PRIMARY) <> 0;
    Bounds := TMultiDisplayWin.ScaleRect(MonInfo.rcMonitor, Scale); // dp
    WorkArea := TMultiDisplayWin.ScaleRect(MonInfo.rcWork, Scale); // dp
    Display := TDisplay.Create(FDisplays.Count, IsPrimary, Bounds, WorkArea);

    FDisplays.Add(AMonitorHandle, Display);
  end;
end;

constructor TMultiDisplayWin.Create;
begin
  FDisplays := TOrderedDictionary<HMONITOR, TDisplay>.Create;
  FWorkAreaRect := TRect.Empty;
  FDesktopRect := TRect.Empty;
  FOutdatedParameters := [Low(TParameter)..High(TParameter)];
end;

destructor TMultiDisplayWin.Destroy;
begin
  FreeAndNil(FDisplays);
  inherited;
end;

function TMultiDisplayWin.GetDisplayCount: Integer;
begin
  if TParameter.DisplayCount in FOutdatedParameters then
  begin
    FDisplayCount := GetSystemMetrics(SM_CMONITORS);
    Exclude(FOutdatedParameters, TParameter.DisplayCount);
  end;
  Result := FDisplayCount;
end;

class function TMultiDisplayWin.GetMonitorScale(const AHandle: HMONITOR): Single;
const
  StandardDpi = 96;
var
  DpiX, DpiY: Cardinal;
begin
  if TOSVersion.Check(6, 3) then
  begin
    if GetDPIForMonitor(AHandle, MDT_Default, DpiX, DpiY) = S_OK then
      Result := DpiX / StandardDpi
    else
      Result := 1;
  end
  else
    Result := 1;
end;

function TMultiDisplayWin.GetDesktopCenterRect(const ASize: TSize): TRect;

  function PointOutOfDisplay(const P: TPoint): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to GetDisplayCount - 1 do
      if PtInRect(GetDisplay(I).Bounds, P) then
        Exit(False);
    Result := True;
  end;

  function CornerOutOfDisplay(const R: TRect): Boolean;
  begin
    Result := PointOutOfDisplay(R.TopLeft) or PointOutOfDisplay(TPoint.Create(R.Right, R.Top)) or
      PointOutOfDisplay(R.BottomRight) or PointOutOfDisplay(TPoint.Create(R.Left, R.Bottom));
  end;

var
  I, MinI: Integer;
  DesktopCenter: TPoint;
  Dist, MinDist: Double;
  WorkArea: TRect;
begin
  DesktopCenter := GetDesktopRect.CenterPoint;
  Result := TRect.Create(TPoint.Create(DesktopCenter.X - ASize.cx div 2, DesktopCenter.Y - ASize.cy div 2), ASize.cx, ASize.cy);
  MinDist := MaxInt;
  MinI := -1;

  for I := 0 to GetDisplayCount - 1 do
  begin
    Dist := GetDisplay(I).WorkArea.CenterPoint.Distance(DesktopCenter);
    if Dist < MinDist then
    begin
      MinDist := Dist;
      MinI := I;
    end;
  end;
  WorkArea := GetDisplay(MinI).WorkArea;
  if CornerOutOfDisplay(Result) then
  begin
    if Result.Top < WorkArea.Top then
      Result.SetLocation(Result.Left, WorkArea.Top);
    if Result.Bottom > WorkArea.Bottom then
      Result.SetLocation(Result.Left, WorkArea.Bottom - Result.Height);
    if CornerOutOfDisplay(Result) then
    begin
      if Result.Left < WorkArea.Left then
        Result.SetLocation(WorkArea.Left, Result.Top);
      if Result.Right > WorkArea.Right then
        Result.SetLocation(WorkArea.Right - Result.Width, Result.Top);
    end;
  end;
end;

function TMultiDisplayWin.GetDesktopRect: TRect;
begin
  if TParameter.DesktopRect in FOutdatedParameters then
  begin
    FDesktopRect.Left := GetSystemMetrics(SM_XVIRTUALSCREEN);
    FDesktopRect.Top := GetSystemMetrics(SM_YVIRTUALSCREEN);
    FDesktopRect.Width := GetSystemMetrics(SM_CXVIRTUALSCREEN);
    FDesktopRect.Height := GetSystemMetrics(SM_CYVIRTUALSCREEN);
    Exclude(FOutdatedParameters, TParameter.DesktopRect);
  end;

  Result := FDesktopRect;
end;

function TMultiDisplayWin.GetWorkAreaRect: TRect;
begin
  if (TParameter.WorkareaRect in FOutdatedParameters) and SystemParametersInfo(SPI_GETWORKAREA, 0, FWorkAreaRect, 0) then
    Exclude(FOutdatedParameters, TParameter.WorkareaRect);
  Result := FWorkAreaRect;
end;

class function TMultiDisplayWin.ScaleRect(const ARect: TRect; const AScale: Single): TRect;
begin
  Result.Left := Round(ARect.Left / AScale);
  Result.Right := Round(ARect.Right / AScale);
  Result.Top := Round(ARect.Top / AScale);
  Result.Bottom := Round(ARect.Bottom / AScale);
end;

procedure TMultiDisplayWin.UpdateDisplayInformation;
begin
  FOutdatedParameters := [Low(TParameter)..High(TParameter)];
end;

function TMultiDisplayWin.FindDisplay(const AMonitorHandle: HMONITOR): TDisplay;
begin
  UpdateDisplaysIfNeeded;

  if not FDisplays.TryGetValue(AMonitorHandle, Result) then
    raise EInvalidArgument.Create(sArgumentInvalid)
end;

function IsPopupForm(const AForm: TCommonCustomForm): Boolean;
begin
  Result := (AForm <> nil) and ((AForm.FormStyle = TFormStyle.Popup) or (AForm.Owner is TPopup) or (AForm is TCustomPopupForm));
end;

function TMultiDisplayWin.DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;
var
  Wnd: TWinWindowHandle;
  Form, ParentForm: TCommonCustomForm;
  ParentControl: TFmxObject;
  Control: TControl;
  P: TPointF;
  ScreenPosition: TPoint;
  Scene: IScene;
begin
  RaiseIfNil(AHandle, 'AHandle');

  Wnd := WindowHandleToPlatform(AHandle);
  Form := FindWindow(Wnd.Wnd);
  ParentControl := nil;
  if IsPopupForm(Form) then
  begin
    if Form is TCustomPopupForm then
    begin
      if GetKeyState(VK_RBUTTON) <> 0 then
      begin
        GetCursorPos(ScreenPosition);
        Result := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
        Exit;
      end;
      ParentControl := TCustomPopupForm(Form).PlacementTarget;
    end;
    ParentForm := Form.ParentForm;
    if (ParentControl = nil) and (ParentForm <> nil) then
    begin
      while IsPopupForm(ParentForm) and (ParentForm.ParentForm <> nil) do
        ParentForm := ParentForm.ParentForm;
      if ParentControl = nil then
      begin
        ParentControl := Form.Parent;
        while (ParentControl <> nil) and (ParentControl.Root <> nil) and (ParentControl.Root.GetObject <> ParentForm) do
          ParentControl := ParentControl.Parent;
        while (ParentControl <> nil) and not (ParentControl is TControl) do
          ParentControl := ParentControl.Parent;
      end;
      if (ParentControl = nil) and (ParentForm <> nil) and (ParentForm.Focused <> nil) then
        ParentControl := ParentForm.Focused.GetObject;
    end;
    if ParentControl is TControl then
    begin
      Control := TControl(ParentControl);
      P := Control.Padding.Rect.TopLeft;
      P := Control.LocalToAbsolute(P);
      if Supports(Control.Root.GetObject, IScene, Scene) then
        P := Scene.LocalToScreen(P);
      P.Offset((Control.Width - Control.Padding.Right) / 2, (Control.Height - Control.Padding.Bottom) / 2);
      ScreenPosition := TPoint.Create(Round(P.X), Round(P.Y));
      Result := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
      Exit;
    end;
    if ParentForm <> nil then
      Wnd := WindowHandleToPlatform(ParentForm.Handle);
  end;
  if (Wnd = nil) or (Wnd.Wnd = 0) then
    raise EArgumentException.Create(sArgumentInvalid);
  Result := FindDisplay(Winapi.MultiMon.MonitorFromWindow(Wnd.Wnd, MONITOR_DEFAULTTONEAREST));
end;

function TMultiDisplayWin.DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay;
var
  Wnd: TWinWindowHandle;
  Form: TCommonCustomForm;
  DispByPoint, DispByMouse: TDisplay;
  PointF: TPointF;
  ScreenPosition: TPoint;
begin
  RaiseIfNil(AHandle, 'AHandle');

  Wnd := WindowHandleToPlatform(AHandle);
  if (Wnd = nil) or (Wnd.Wnd = 0) then
    raise EArgumentException.Create(sArgumentInvalid);
  Form := FindWindow(Wnd.Wnd);
  if not IsPopupForm(Form) then
  begin
    PointF := TPointF.Create(APoint);
    ScreenPosition := Form.ClientToScreen(PointF).Round;
    DispByPoint := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
    if (GetKeyState(VK_RBUTTON) <> 0) or (GetKeyState(VK_LBUTTON) <> 0) or (GetKeyState(VK_MBUTTON) <> 0) then
    begin
      Result := DisplayFromWindow(AHandle);
      GetCursorPos(ScreenPosition);
      DispByMouse := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
      if DispByMouse.Index <> Result.Index then
        Result := DispByPoint;
    end
    else
      Result := DispByPoint;
  end
  else if Form.Visible and (Form is TCustomPopupForm) then
    Result := FindDisplay(Winapi.MultiMon.MonitorFromWindow(Wnd.Wnd, MONITOR_DEFAULTTONEAREST))
  else
    Result := DisplayFromWindow(AHandle);
end;

function EnumMonitorsProc(hm: HMONITOR; dc: HDC; R: PRect; Data: Pointer): Boolean; stdcall;
var
  Sender: TMultiDisplayWin;
begin
  Sender := TMultiDisplayWin(Data);
  Sender.AddDisplay(hm);
  Result := True;
end;

procedure TMultiDisplayWin.UpdateDisplaysIfNeeded;
begin
  if TParameter.Displays in FOutdatedParameters then
  begin
    FDisplays.Clear;
    EnumDisplayMonitors(0, nil, @EnumMonitorsProc, Winapi.Windows.LPARAM(Self));
    Exclude(FOutdatedParameters, TParameter.Displays);
  end;
end;

function TMultiDisplayWin.GetDisplay(const AIndex: Integer): TDisplay;
begin
  UpdateDisplaysIfNeeded;

  if (AIndex < 0) or (AIndex >= GetDisplayCount) then
    raise EListError.CreateFMT(SListIndexError, [AIndex]);
  Result := FDisplays[AIndex];
end;

{ TFullScreenParams }

procedure TFullScreenParams.Clean;
begin
  Self.BorderStyle := TFmxFormBorderStyle.None;
  Self.WindowState := TWindowState.wsMaximized;
end;

function TFullScreenParams.IsFullScreen: Boolean;
begin
  Result := not ((Self.BorderStyle = TFmxFormBorderStyle.None) and (Self.WindowState = TWindowState.wsMaximized));
end;

{ TWin32MenuInfo }

constructor TWin32MenuInfo.Create(const AMenuId: Integer; const AnItem: TMenuItem);
begin
  MenuID := AMenuId;
  FMXMenuItem := AnItem;
end;

procedure ShutDown;
begin
  if IsLibrary then
  begin
    FMX.Canvas.D2D.UnregisterCanvasClasses;
    FMX.Context.DX11.UnregisterContextClasses;
    FMX.Forms.FinalizeForms;
  end;
end;

{ TMenuServiceWin }

procedure TMenuServiceWin.AddBitmapToMenu(const AParentMenu: HMENU; const AMenuItemId: TMenuId; const ABitmap: HBITMAP);
var
  MenuItemInfo: tagMENUITEMINFOW;
begin
  if ABitmap <> 0 then
  begin
    FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
    MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
    MenuItemInfo.fMask := MIIM_BITMAP;
    if not GetMenuItemInfo(AParentMenu, AMenuItemId, False, MenuItemInfo) then
      RaiseLastOSError;
    MenuItemInfo.hbmpItem := ABitmap;
    try
      if not SetMenuItemInfo(AParentMenu, AMenuItemId, False, MenuItemInfo) then
        RaiseLastOSError;
    except
      MenuItemInfo.hbmpItem := 0;
      SetMenuItemInfo(AParentMenu, AMenuItemId, False, MenuItemInfo);
      raise;
    end;
  end;
end;

function TMenuServiceWin.AssignNewIdToMenu(const AParentMenu, AMenu: HMENU): TMenuId;
var
  MenuItemInfo: tagMENUITEMINFOW;
begin
  Result := GenerateMenuId;

  FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
  MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
  MenuItemInfo.fMask := MIIM_ID;
  if not GetMenuItemInfo(AParentMenu, AMenu, False, MenuItemInfo) then
    RaiseLastOSError;
  MenuItemInfo.wID := Result;
  try
    if not SetMenuItemInfo(AParentMenu, AMenu, False, MenuItemInfo) then
      RaiseLastOSError;
    FHMenuIdMap.Add(Result, AMenu);
  except
    MenuItemInfo.wID := 0;
    SetMenuItemInfo(AParentMenu, AMenu, False, MenuItemInfo);
    raise;
  end;
end;

constructor TMenuServiceWin.Create;
begin
  FHMenuMap := TDictionary<TFmxHandle, TWin32MenuInfo>.Create;
  FHMenuIdMap := TDictionary<TMenuId, TFmxHandle>.Create;
  FMenuLooper := TMenuLooperWin.Create;
end;

procedure TMenuServiceWin.CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);

  function CalculateItemFlags(const AMenuItem: TMenuItem): Integer;
  begin
    Result := 0;
    if AMenuItem.Text = SMenuSeparator then
      Result := Result or MF_SEPARATOR
    else
    begin
      if AMenuItem.IsChecked then
        Result := Result or MF_CHECKED;

      if not AMenuItem.Enabled then
        Result := Result or MF_DISABLED;

      Result := Result or MF_STRING;
    end;
  end;

  function CalculateVisibleCount(const AMenu: IItemsContainer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to AMenu.GetItemsCount - 1 do
      if (AMenu.GetItem(I) is TMenuItem) and TMenuItem(AMenu.GetItem(I)).Visible then
        Inc(Result);
  end;

  procedure InsertItems(const AParent: HMENU; AChild: IItemsContainer; const AMenuLevel: Integer);
  var
    I, Flags, VisibleCount: Integer;
    PopupMenu: HMENU;
    Native: INativeControl;
    MenuItem: TMenuItem;
    SubChild: IItemsContainer;
    S: string;
    Bitmap: HBITMAP;
  begin
    if (AChild <> nil) and (AChild.GetObject is TMenuItem) then
    begin
      Bitmap := 0;
      MenuItem := TMenuItem(AChild.GetObject);
      if MenuItem.Visible and Supports(MenuItem, INativeControl, Native) then
      begin
        Flags := CalculateItemFlags(MenuItem);
        VisibleCount := CalculateVisibleCount(AChild);
        if VisibleCount > 0 then
          Flags := Flags or MF_POPUP;

        Native.Handle := 0;
        PopupMenu := CreateMenu;
        if PopupMenu = 0 then
          RaiseLastOSError;
        try
          if AMenuLevel > 0 then
            S := ShortCutToText(MenuItem.ShortCut)
          else
            S := string.Empty;
          if not S.IsEmpty then
            S := #9 + S;
          S := MenuItem.Text + S;
          if AppendMenu(AParent, Flags, PopupMenu, PChar(S)) then
          begin
            Bitmap := ImageListToMenuBitmap(AMenuLevel = 0, MenuItem.Images, MenuItem.ImageIndex);
            if Bitmap = 0 then
              Bitmap := BitmapToMenuBitmap(AMenuLevel = 0, MenuItem.Bitmap);
            if Bitmap <> 0 then
              try
                AddBitmapToMenu(AParent, PopupMenu, Bitmap);
              except
                DeleteObject(Bitmap);
                Bitmap := 0;
                raise;
              end;
            FHMenuMap.Add(PopupMenu, TWin32MenuInfo.Create(AssignNewIdToMenu(AParent, PopupMenu), MenuItem));
            if VisibleCount > 0 then
              for I := 0 to AChild.GetItemsCount - 1 do
                if Supports(AChild.GetItem(I), IItemsContainer, SubChild) then
                  InsertItems(PopupMenu, SubChild, AMenuLevel + 1);
            Native.Handle := PopupMenu;
          end
          else
            RaiseLastOSError;
        except
          if Bitmap <> 0 then
            RemoveBitmapFromMenu(AParent, PopupMenu);
          DestroyMenu(PopupMenu);
          raise;
        end;
      end;
    end;
  end;

var
  Handle: HMENU;
  Wnd: HWND;
  I, VisibleCount: Integer;
  WindowBorder: TWindowBorderWin;
  Native: INativeControl;
  SavedWindowState: TWindowState;
begin
  if TState.CreatingOSMenu in FStates then
    Exit;
  SavedWindowState := AForm.WindowState;
  try
    Include(FStates, TState.CreatingOSMenu);
    Wnd := FormToHWND(AForm);
    if Wnd = 0 then
      Exit;

    VisibleCount := 0;
    Native := nil;
    if AMenu <> nil then
    begin
      DestroysAllMenuItems(AMenu);
      Supports(AMenu.GetObject, INativeControl, Native);
      if not (csDesigning in AForm.ComponentState) and AForm.Border.IsSupported then
      begin
        WindowBorder := TWindowBorderWin(AForm.Border.WindowBorder);
        WindowBorder.CreateOSMenu(AMenu);
      end
      else if (Native <> nil) and Native.HandleSupported then
        VisibleCount := CalculateVisibleCount(AMenu);
    end;
    Handle := GetMenu(Wnd);
    if VisibleCount > 0 then
    begin
      if Handle = 0 then
      begin
        Native.Handle := 0;
        Handle := CreateMenu;
        if Handle = 0 then
          RaiseLastOSError;
      end;
      try
        for I := 0 to AMenu.GetItemsCount - 1 do
          if AMenu.GetItem(I) is TMenuItem and TMenuItem(AMenu.GetItem(I)).Visible then
            InsertItems(Handle, TMenuItem(AMenu.GetItem(I)), 0);
      except
        DestroyMenu(Handle);
        raise;
      end;
      SetMenu(Wnd, Handle);
    end
    else if Handle <> 0 then
    begin
      if not DestroyMenu(Handle) then
        RaiseLastOSError;
      SetMenu(Wnd, 0);
      Handle := 0;
    end;
    if Native <> nil then
      Native.Handle := Handle;
  finally
    AForm.WindowState := SavedWindowState;
    Exclude(FStates, TState.CreatingOSMenu);
  end;
end;

destructor TMenuServiceWin.Destroy;
begin
  FreeAndNil(FMenuLooper);
  FreeAndNil(FHMenuMap);
  FreeAndNil(FHMenuIdMap);

  inherited;
end;

procedure TMenuServiceWin.DestroyMenuItem(const AItem: IItemsContainer);
var
  Form: TCommonCustomForm;
  MenuItem: TFmxObject;
  Wnd: HWND;
  MenuHandle, Handle: TFmxHandle;
  Native: INativeControl;
  Root: IRoot;
begin
  if not (TState.DestroyingMenuItem in FStates) and (AItem <> nil) and (AItem.GetObject is TFmxObject) then
    MenuItem := AItem.GetObject
  else
    Exit;
  Include(FStates, TState.DestroyingMenuItem);
  try
    Root := MenuItem.Root;
    if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
    begin
      Form := TCommonCustomForm(Root.GetObject);
      if (MenuItem <> nil) and Supports(MenuItem, INativeControl, Native) and (Native.Handle <> 0) then
      begin
        Handle := Native.Handle;
        if not (csDesigning in Form.ComponentState) and (Form.Border.WindowBorder is TWindowBorderWin) and
          TWindowBorderWin(Form.Border.WindowBorder).HandleExists(Handle) then
        begin
          TWindowBorderWin(Form.Border.WindowBorder).RemoveHandle(Handle);
        end
        else
        begin
          if Handle <> INVALID_HANDLE_VALUE then
          begin
            Wnd := FormToHWND(Form);
            MenuHandle := GetMenu(Wnd);
            if MenuHandle = Handle then
            begin
              CreateOSMenu(Form, nil);
              SetMenu(Wnd, 0)
            end
            else if MenuHandle <> 0 then
              RemoveBitmapFromMenu(MenuHandle, Handle);
            if IsMenu(Handle) then
              CheckWinapiResult(DestroyMenu(Handle));
          end;
          RemoveMenuFromMaps(Handle);
        end;
        Native.Handle := 0
      end;
    end;
  finally
    Exclude(FStates, TState.DestroyingMenuItem);
  end;
end;

function TMenuServiceWin.FindMenuInfoById(const AMenuItemId: TMenuId; var AMenuInfo: TWin32MenuInfo): Boolean;
var
  FmxHandle: TFmxHandle;
begin
  Result := FHMenuIdMap.TryGetValue(AMenuItemId, FmxHandle) and FHMenuMap.TryGetValue(FmxHandle, AMenuInfo);
end;

function TMenuServiceWin.GenerateMenuId: TMenuId;
begin
  Result := NewUniqueMenuCommand;
  if Result > 65535 then
  begin
    FreeUniqueMenuCommand(Result);
    raise EUnavailableMenuId.Create(SUnavailableMenuId);
  end;
end;

function TMenuServiceWin.IsMenuBarOnWindowBorder: Boolean;
begin
  Result := True;
end;

procedure TMenuServiceWin.RemoveBitmapFromMenu(const AParentMenu, AMenu: HMENU);
var
  MenuItemInfo: tagMENUITEMINFOW;
  MenuInfo: TWin32MenuInfo;
  MenuId: TMenuId;
begin
  if FHMenuMap.TryGetValue(AMenu, MenuInfo) then
    MenuId := MenuInfo.MenuID
  else
    Exit;
  FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
  MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
  MenuItemInfo.fMask := MIIM_BITMAP;
  if not GetMenuItemInfo(AParentMenu, MenuId, False, MenuItemInfo) then
    RaiseLastOSError;
  if (MenuItemInfo.hbmpItem <> HBITMAP(-1)) and (MenuItemInfo.hbmpItem > HBMMENU_POPUP_MINIMIZE) then
  begin
    if not DeleteObject(MenuItemInfo.hbmpItem) then
      RaiseLastOSError;

    MenuItemInfo.hbmpItem := HBITMAP(-1);
    if not SetMenuItemInfo(AParentMenu, MenuId, False, MenuItemInfo) then
      RaiseLastOSError;
  end;
end;

procedure TMenuServiceWin.DestroysAllMenuItems(const AMenu: IItemsContainer);
var
  I: Integer;
  ItemsContainer: IItemsContainer;
begin
  for I := 0 to AMenu.GetItemsCount - 1 do
    if Supports(AMenu.GetItem(I), IItemsContainer, ItemsContainer) then
      DestroysAllMenuItems(ItemsContainer);
  DestroyMenuItem(AMenu);
end;

procedure TMenuServiceWin.RemoveMenuFromMaps(const AMenuHandle: TFmxHandle);
var
  Pair: TPair<TMenuId, TFmxHandle>;
begin
  FHMenuMap.Remove(AMenuHandle);

  for Pair in FHMenuIdMap do
    if Pair.Value = AMenuHandle then
    begin
      FreeUniqueMenuCommand(Pair.Key);
      FHMenuIdMap.Remove(Pair.Key);
      Break;
    end;
end;

procedure TMenuServiceWin.ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
begin
  FMX.Helpers.Win.ShortCutToKey(ShortCut, Key, Shift);
end;

function TMenuServiceWin.ShortCutToText(ShortCut: TShortCut): string;
begin
  Result := FMX.Helpers.Win.ShortCutToText(ShortCut);
end;

procedure TMenuServiceWin.StartMenuLoop(const AView: IMenuView);
begin
  FMenuLooper.StartLoop(AView);
end;

function TMenuServiceWin.TextToShortCut(Text: string): Integer;
begin
  Result := FMX.Helpers.Win.TextToShortCut(Text);
end;

procedure TMenuServiceWin.UpdateMenuBar;
begin
end;

procedure TMenuServiceWin.UpdateMenuItem(const AItem: IItemsContainer; AChange: TMenuItemChanges);

  function GetParentMenuItemHandle(const AMenuItem: TMenuItem): TFmxHandle;
  var
    NativeControl: INativeControl;
  begin
    if Supports(AMenuItem.ParentMenuItem, INativeControl, NativeControl) or Supports(AMenuItem.MainMenu, INativeControl, NativeControl) then
      Result := NativeControl.Handle
    else
      Result := 0;
  end;

  function GetBitmap(const AMenuItem: TMenuItem): HBITMAP;
  var
    IsTopLevelItem: Boolean;
  begin
    IsTopLevelItem := AMenuItem.Parent is TMainMenu;
    Result := ImageListToMenuBitmap(IsTopLevelItem, AMenuItem.Images, AMenuItem.ImageIndex);
    if Result = 0 then
      Result := BitmapToMenuBitmap(IsTopLevelItem, AMenuItem.Bitmap);
  end;

  procedure UpdateBitmap(const AMenuItem: TMenuItem);
  var
    ParentHandle: TFmxHandle;
    Bitmap: HBITMAP;
    MenuItemHandle: TFmxHandle;
  begin
    MenuItemHandle := (AMenuItem as INativeControl).Handle;
    ParentHandle := GetParentMenuItemHandle(AMenuItem);
    if ParentHandle = 0 then
      Exit;

    Bitmap := GetBitmap(AMenuItem);
    if Bitmap <> 0 then
      try
        AddBitmapToMenu(ParentHandle, FHMenuMap[MenuItemHandle].Menuid, Bitmap);
      except
        DeleteObject(Bitmap);
        raise;
      end
    else
      RemoveBitmapFromMenu(ParentHandle, MenuItemHandle);
  end;

  procedure UpdateEnabled(const AMenuItem: TMenuItem);
  const
    Enables: array[Boolean] of DWORD = (MF_DISABLED or MF_GRAYED, MF_ENABLED);
  var
    ParentHandle: TFmxHandle;
    MenuItemHandle: TFmxHandle;
  begin
    MenuItemHandle := (AMenuItem as INativeControl).Handle;
    ParentHandle := GetParentMenuItemHandle(AMenuItem);
    if ParentHandle = 0 then
      Exit;

    EnableMenuItem(ParentHandle, FHMenuMap[MenuItemHandle].Menuid, MF_BYCOMMAND or Enables[AMenuItem.enabled]);
  end;

  procedure UpdateChecked(const AMenuItem: TMenuItem);
  const
    Checks: array[Boolean] of DWORD = (MF_UNCHECKED, MF_CHECKED);
  var
    ParentHandle: TFmxHandle;
    MenuItemHandle: TFmxHandle;
  begin
    MenuItemHandle := (AMenuItem as INativeControl).Handle;
    ParentHandle := GetParentMenuItemHandle(AMenuItem);
    if ParentHandle = 0 then
      Exit;

    CheckMenuItem(ParentHandle, FHMenuMap[MenuItemHandle].Menuid, MF_BYCOMMAND or Checks[AMenuItem.IsChecked]);
  end;

  procedure RecreateMainMenu(const AMenuItem: TMenuItem);
  var
    MainMenu: TMainMenu;
  begin
    MainMenu := AMenuItem.MainMenu;
    if MainMenu <> nil then
      MainMenu.RecreateOSMenu;
  end;

  function IsStyledBorder(const AMenuItem: TMenuItem): Boolean;
  var
    Form: TCommonCustomForm;
  begin
    if AMenuItem.Root = nil then
      Exit(False);

    Form := AMenuItem.Root.GetObject as TCommonCustomForm;
    Result := Form.Border.IsSupported;
  end;

var
  MenuItem: TMenuItem;
  MenuItemHandle: TFmxHandle;
begin
  if not (AItem.GetObject is TMenuItem) then
    Exit;

  MenuItem := TMenuItem(AItem.GetObject);
  MenuItemHandle := (MenuItem as INativeControl).Handle;
  if (MenuItemHandle = 0) or IsStyledBorder(MenuItem) then
  begin
    RecreateMainMenu(MenuItem);
    Exit;
  end;

  if MenuItemHandle = INVALID_HANDLE_VALUE then
    raise EInvalidFmxHandle.CreateFMT(SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(MenuItemHandle) * 2, MenuItemHandle]);

  if AChange = [TMenuItemChange.Bitmap] then
    UpdateBitmap(MenuItem)
  else if AChange = [TMenuItemChange.Enabled] then
    UpdateEnabled(MenuItem)
  else if AChange = [TMenuItemChange.Checked] then
    UpdateChecked(MenuItem)
  else
    // If Visible, Text or Shortcut are changed, then we need to rebuild menu. Because WinApi doesn't support
    RecreateMainMenu(MenuItem);
end;

procedure TMenuServiceWin.WMCommand(var Message: TWMCommand);
var
  MenuInfo: TWin32MenuInfo;
begin
  if FindMenuInfoById(Message.ItemId, MenuInfo) then
  begin
    TOpenMenuItem(MenuInfo.FMXMenuItem).Click;
    Message.Result := 0;
  end;
end;

procedure TMenuServiceWin.WMInitMenuPopup(var Message: TWMInitMenuPopup);
var
  MenuInfo: TWin32MenuInfo;
begin
  if FHMenuMap.TryGetValue(Message.MenuPopup, MenuInfo) then
    TOpenMenuItem(MenuInfo.FMXMenuItem).Click;
end;

procedure TMenuServiceWin.WMMenuSelect(var Message: TWMMenuSelect);
var
  MenuInfo: TWin32MenuInfo;
  MenuHandle: HMENU;
begin
  MenuHandle := GetSubMenu(Message.Menu, Message.IDItem);
  if MenuHandle = 0 then
    Exit;

  if FindMenuInfoById(Message.IDItem, MenuInfo) then
    Application.Hint := MenuInfo.FMXMenuItem.Hint
  else
    Application.Hint := string.Empty;
end;

{ TMenuLooperWin }

function TMenuLooperWin.BackwardSelectNextMenuItem(const AView: IMenuView; const AStartInd, AEndInd: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AView <> nil then
    for I := AStartInd downto AEndInd do
      if IsItemSelectable(AView.GetItem(I)) then
      begin
        AView.Selected := TMenuItem(AView.GetItem(I));
        Result := True;
        Break;
      end;
end;

procedure TMenuLooperWin.EndLoop(const AView: IMenuView);
var
  View: IMenuView;
begin
  View := AView;
  while View <> nil do
  begin
    View.Loop := False;
    View.Selected := nil;
    View := View.ParentView;
  end;
end;

function TMenuLooperWin.ForwardSelectNextMenuItem(const AView: IMenuView; const AStartInd, AEndInd: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AView <> nil then
    for I := AStartInd to AEndInd do
      if IsItemSelectable(AView.GetItem(I)) then
      begin
        AView.Selected := TMenuItem(AView.GetItem(I));
        Result := True;
        Break;
      end;
end;

function TMenuLooperWin.IsItemSelectable(const Item: TFmxObject): Boolean;
begin
  Result := (Item is TMenuItem) and TMenuItem(Item).Visible and (TMenuItem(Item).Text <> SMenuSeparator);
end;

procedure TMenuLooperWin.SelectFirstMenuItem(const AView: IMenuView);
var
  I: Integer;
begin
  if AView <> nil then
  begin
    I := 0;
    while (I < AView.GetItemsCount) and not IsItemSelectable(AView.GetItem(I)) do
      Inc(I);
    if I < AView.GetItemsCount then
      AView.Selected := TMenuItem(AView.GetItem(I));
  end;
end;

procedure TMenuLooperWin.SelectLastMenuItem(const AView: IMenuView);
var
  I: Integer;
begin
  if AView <> nil then
  begin
    I := AView.GetItemsCount - 1;
    while (I >= 0) and not IsItemSelectable(AView.GetItem(I)) do
      Dec(I);
    if I >= 0 then
      AView.Selected := TMenuItem(AView.GetItem(I));
  end;
end;

procedure TMenuLooperWin.SelectNextMenuItem(const AView: IMenuView; const ABackward: Boolean);
begin
  if ABackward then
  begin
    if (AView.Selected = nil) or not BackwardSelectNextMenuItem(AView, AView.Selected.Index - 1, 0) then
      SelectLastMenuItem(AView);
    // otherwise nothing
  end
  else
  begin
    if (AView.Selected = nil) or not ForwardSelectNextMenuItem(AView, AView.Selected.Index + 1,  AView.GetItemsCount - 1) then
      SelectFirstMenuItem(AView);
    // otherwise nothing
  end;
end;

procedure TMenuLooperWin.StartLoop(const AView: IMenuView);
var
  FirstLoop: Boolean;

  function ContinueLoop: Boolean;
  begin
    Result := AView.Loop;
  end;

  function ParentWindow(AView: IMenuView): HWND;
  var
    Obj: TFmxObject;
    Form: TCommonCustomForm;
  begin
    Result := 0;
    if (AView <> nil) and (AView.Parent <> nil) then
    begin
      Obj := AView.Parent;
      while (Obj <> nil) and not (Obj is TCommonCustomForm) do
        Obj := Obj.Parent;
      if Obj is TCommonCustomForm then
      begin
        Form := TCommonCustomForm(Obj);
        while (Form <> nil) and IsPopup(Form) do
          Form := Form.ParentForm;
        if (Form <> nil) and (Form.Handle <> nil) then
          Result := WindowHandleToPlatform(Form.Handle).Wnd;
      end;
    end;
  end;

  function GetDpMousePos(const AView: IMenuView; const AMsg: TMsg): TPointF;
  var
    WP: TPoint;
    Form: TCommonCustomForm;
    Scale: Single;
  begin
    // WP is client point in Native window in px.
  {$IFDEF CPUX64}
    WP := SmallPointToPoint(TSmallPoint(Cardinal(AMsg.LPARAM))); // px
  {$ELSE}
    WP := SmallPointToPoint(TSmallPoint(AMsg.LPARAM)); // px
  {$ENDIF}
    Winapi.Windows.ClientToScreen(AMsg.HWND, WP);
    if AView.GetObject.Root.GetObject is TCommonCustomForm then
    begin
      Form := TCommonCustomForm(AView.GetObject.Root.GetObject);
      Scale := Form.Handle.Scale;
      Result := TPoint.Create(Round(WP.X / Scale), Round(WP.Y / Scale));
    end
    else
      Result := WP;
  end;

var
  Msg: TMsg;
  WP: TPoint;
  ParentWnd: HWND;
  P: TPointF;
  InMenus: Boolean;
  CurrentView, NewView: IMenuView;
  Obj: IControl;
  TimerID: THandle;
  MenuItem: TMenuItem;
begin
  FView := AView;
  AView.Loop := True;
  TimerID := SetTimer(0, 0, 50, nil);
  try
    FirstLoop := True;
    while ContinueLoop do
    begin
                                                         
      if FirstLoop then
        FirstLoop := False
      else
        WaitMessage;

      while ContinueLoop and PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) do
      begin
        case Msg.Message of
          WM_WINDOWPOSCHANGING:
            begin
              EndLoop(AView);
              Exit;
            end;
          WM_QUIT { , WM_NCLBUTTONDOWN..WM_NCMBUTTONDBLCLK } :
            begin
              EndLoop(AView);
              Continue;
            end;
          WM_TIMER:
            begin
              TranslateMessage(Msg);
            end;
        end;
        if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
        begin
          case Msg.Message of
            WM_CLOSEMENU:
              EndLoop(AView);
            WM_NCMOUSEMOVE, WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCLBUTTONUP:
              begin
                case Msg.Message of
                  WM_NCMOUSEMOVE:
                    begin
                      { Handle MouseOver }
                      P := GetDpMousePos(AView, Msg); // dp
                      Obj := AView.ObjectAtPoint(P);
                      TranslateMessage(Msg);
                      DispatchMessage(Msg);
                      { Find top level menu }
                      CurrentView := AView;
                      while CurrentView.ParentView <> nil do
                        CurrentView := CurrentView.ParentView;
                      { Check all items }
                      while CurrentView <> nil do
                      begin
                        Obj := CurrentView.ObjectAtPoint(P);
                        if (Obj <> nil) and (Obj.GetObject is TMenuItem) and not (TMenuItem(Obj.GetObject).IsSelected) then
                        begin
                          if (CurrentView <> AView) then
                          begin
                            NewView := AView;
                            while NewView <> CurrentView do
                            begin
                              NewView.Loop := False;
                              NewView := NewView.ParentView;
                            end;
                            TOpenMenuItem(Obj.GetObject).NeedPopup;
                            Exit;
                          end;
                        end;
                        CurrentView := CurrentView.ChildView;
                      end;
                      Continue;
                    end;
                  WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN:
                    begin
                      { Handle MouseOver if mouse over not menuitem }
                      P := GetDpMousePos(AView, Msg); // dp
                      Obj := AView.ObjectAtPoint(P);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                      { Menus }
                      if (Obj <> nil) and (Obj.GetObject is TMenuItem) then
                      begin
                        MenuItem := TMenuItem(Obj.GetObject);
                        if not MenuItem.IsSelected and MenuItem.HavePopup then
                          TOpenMenuItem(MenuItem).NeedPopup
                        else if MenuItem.CanBeClicked then
                        begin
                          EndLoop(AView);
                          TOpenMenuItem(Obj.GetObject).Click;
                        end;
                      end
                      else
                      begin
                        CurrentView := AView;
                        InMenus := False;
                        while (CurrentView <> nil) and not InMenus do
                        begin
                          if not (CurrentView.IsMenuBar) and (CurrentView.ObjectAtPoint(P) <> nil) then
                            InMenus := True;
                          CurrentView := CurrentView.ParentView;
                        end;
                        if not InMenus then
                          EndLoop(AView);
                      end;
                    end;
                  WM_NCLBUTTONUP:
                    begin
                      { Handle MouseOver if mouse over not menuitem }
                      P := GetDpMousePos(AView, Msg); // dp
                      Obj := AView.ObjectAtPoint(P);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                    end;
                end;
              end;
            WM_MOUSEFIRST .. WM_MOUSELAST:
              begin
                { Handle MouseOver if mouse over not menuitem }
{$IFDEF CPUX64}
                WP := SmallPointToPoint(TSmallPoint(Cardinal(Msg.LPARAM)));
{$ELSE}
                WP := SmallPointToPoint(TSmallPoint(Msg.LPARAM));
{$ENDIF}
                Winapi.Windows.ClientToScreen(Msg.HWND, WP);
                case Msg.Message of
                  WM_MOUSEMOVE:
                    begin
                      TranslateMessage(Msg);
                      DispatchMessage(Msg);
                      Continue;
                    end;
                  WM_LBUTTONDOWN, WM_RBUTTONDOWN:
                    begin
                      P := GetDpMousePos(AView, Msg); // dp
                      Obj := AView.ObjectAtPoint(P);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                      { Menus }
                      if (Obj <> nil) and (Obj.GetObject is TMenuItem) then
                      begin
                        MenuItem := TMenuItem(Obj.GetObject);
                        if not MenuItem.IsSelected and MenuItem.HavePopup then
                          TOpenMenuItem(MenuItem).NeedPopup
                        else if MenuItem.CanBeClicked then
                        begin
                          EndLoop(AView);
                          TOpenMenuItem(MenuItem).Click;
                        end;
                      end
                      else
                      begin
                        CurrentView := AView;
                        InMenus := False;
                        while (CurrentView <> nil) and not InMenus do
                        begin
                          if not (CurrentView.IsMenuBar) and (CurrentView.ObjectAtPoint(P) <> nil) then
                            InMenus := True;
                          CurrentView := CurrentView.ParentView;
                        end;
                        if not InMenus then
                        begin
                          EndLoop(AView);
                          ParentWnd := ParentWindow(AView);
                          if ParentWnd <> 0 then
                          begin
                            // Redirect messages to the parent form
                            Winapi.Windows.ScreenToClient(ParentWnd, WP);
                            Msg.LPARAM := Cardinal(PointToSmallPoint(WP));
                            PostMessage(ParentWnd, Msg.Message, Msg.wParam, Msg.LPARAM);
                          end;
                        end;
                      end;
                    end;
                  WM_LBUTTONUP, WM_RBUTTONUP:
                    begin
                      P := GetDpMousePos(AView, Msg); // dp
                      Obj := AView.ObjectAtPoint(P);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                    end;
                end;
              end;
            WM_KEYFIRST .. WM_KEYLAST:
              if (GetKeyState(VK_LBUTTON) >= 0) then
                case Msg.Message of
                  WM_KEYDOWN, WM_SYSKEYDOWN:
                    case Msg.wParam of
                      VK_RETURN:
                        begin
                          if AView.Selected <> nil then
                          begin
                            if AView.Selected.HavePopup then
                              AView.Selected.NeedPopup
                            else if AView.Selected.CanBeClicked then
                            begin
                              TOpenMenuItem(AView.Selected).Click;
                              EndLoop(AView);
                            end;
                          end
                          else
                            EndLoop(AView);
                        end;
                      VK_ESCAPE:
                        begin
                          AView.Selected := nil;
                          Exit;
                        end;
                      VK_MENU, VK_F10:
                        EndLoop(AView);
                      VK_LEFT, VK_RIGHT:
                          if AView.IsMenuBar then
                            SelectNextMenuItem(AView, Msg.wParam = VK_LEFT)
                          else if AView.ParentView <> nil then
                            if (AView.Selected <> nil) and AView.Selected.HavePopup and (Msg.wParam = VK_RIGHT) then
                              AView.Selected.NeedPopup
                            else if AView.ParentView.IsMenuBar then
                            begin
                              AView.Loop := False;
                              SelectNextMenuItem(AView.ParentView, Msg.wParam = VK_LEFT);
                              if AView.ParentView.Selected <> nil then
                                AView.ParentView.Selected.NeedPopup;
                              Exit;
                            end
                            else
                              AView.Loop := False;
                      VK_UP, VK_DOWN:
                          if AView.IsMenuBar then
                          begin
                            if (AView.Selected <> nil) and (Msg.wParam = VK_DOWN) then
                              AView.Selected.NeedPopup;
                          end
                          else
                            SelectNextMenuItem(AView, Msg.wParam = VK_UP);
                    end;
                end;
          else
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
        end;
      end;
    end;
  finally
    KillTimer(0, TimerID);
    AView.Loop := False;
    Winapi.Windows.ReleaseCapture;
    FView := nil;
  end;
end;

{ TWinTimerService }

procedure TWinTimerService.ApplicationTerminatingHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  FTerminating := True;
  DestroyTimers;
end;

constructor TWinTimerService.Create;
begin
  inherited;
  FTimers := TList<TWin32TimerInfo>.Create;
  FHandleCounter := 128; // Start counting handles at 128. All valid handles have lower nibble = 0;
  FTerminating := False;
  if not QueryPerformanceFrequency(FPerformanceFrequency) then
    FPerformanceFrequency := 0;

  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationTerminatingMessage, ApplicationTerminatingHandler);
end;

destructor TWinTimerService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationTerminatingMessage, ApplicationTerminatingHandler);

  FreeAndNil(FTimers);
  inherited;
end;

function TWinTimerService.CreateTimer(AInterval: Integer; ATimerFunc: TTimerProc): TFmxHandle;
var
  TimerInfo: TWin32TimerInfo;
begin
  Result := 0;
  if not FTerminating and (AInterval > 0) and Assigned(ATimerFunc) then
  begin
    TimerInfo.TimerFunc := ATimerFunc;
    TimerInfo.TimerID := Winapi.Windows.SetTimer(0, 0, AInterval, @TimerCallback);
    if TimerInfo.TimerID <> 0 then
    begin
{$IFDEF CPUX64}
      TimerInfo.TimerHandle := TInterlocked.Add(Int64(FHandleCounter), 16);
{$ENDIF}
{$IFDEF CPUX86}
      TimerInfo.TimerHandle := TInterlocked.Add(Integer(FHandleCounter), 16);
{$ENDIF}
      FTimers.Add(TimerInfo);
      Result := TimerInfo.TimerHandle;
    end
    else
      raise Exception.CreateFmt(SCannotCreateTimer, [GetLastError]);
  end;
end;

function TWinTimerService.DestroyTimer(Timer: TFmxHandle): Boolean;
var
  Index: Integer;
  TimerInfo: TWin32TimerInfo;
begin
  Result := False;
  Index := FTimers.Count;
  while (Index > 0) do
  begin
    Dec(Index);
    TimerInfo := FTimers[Index];
    if TimerInfo.TimerHandle = Timer then
    begin
      Result := Winapi.Windows.KillTimer(0, TimerInfo.TimerID);
      FTimers.Delete(Index);
    end;
  end;
end;

procedure TWinTimerService.DestroyTimers;
var
  I: Integer;
begin
  for I := FTimers.Count - 1 downto 0 do
    try
      DestroyTimer(FTimers[I].TimerHandle);
    except
    end;
end;

function TWinTimerService.GetTick: Double;
var
  PerformanceCounter: Int64;
begin
  if FPerformanceFrequency <> 0 then
  begin
    QueryPerformanceCounter(PerformanceCounter);
    Result := PerformanceCounter / FPerformanceFrequency;
  end
  else
    Result := timeGetTime / 1000;
end;

class procedure TWinTimerService.TimerCallback(window_hwnd: HWND; Msg: Longint; idEvent: UINT; dwTime: Longint);
var
  Index: Integer;
begin
  try
    Index := PlatformWin.TimerService.FTimers.Count;
    while (Index > 0) do
    begin
      Dec(Index);
      if PlatformWin.TimerService.FTimers[Index].TimerID = idEvent then
      begin
        PlatformWin.TimerService.FTimers[Index].TimerFunc;
        Break;
      end;
    end;
  except
    on E: Exception do
    begin
      if Application <> nil then
        Application.HandleException(nil)
      else
        raise;
    end;
  end;
end;

{ TImmManager }

constructor TImmManager.Create(const AForm: TCommonCustomForm);
begin
  FForm := AForm;
end;

function TImmManager.GetComposition(const AContext: HIMC): string;
var
  BufferLength: Integer;
begin
  BufferLength := ImmGetCompositionString(AContext, GCS_COMPSTR, nil, 0);
  SetLength(Result, BufferLength div SizeOf(Char));
  ImmGetCompositionString(AContext, GCS_COMPSTR, PChar(Result), BufferLength);
end;

function TImmManager.GetFormHandle: HWND;
begin
  Result := FormToHWND(Form);
end;

function TImmManager.GetResultComposition(const AContext: HIMC): string;
var
  BufferLength: Integer;
begin
  BufferLength := ImmGetCompositionString(AContext, GCS_RESULTSTR, nil, 0);
  SetLength(Result, BufferLength div SizeOf(Char));
  ImmGetCompositionString(AContext, GCS_RESULTSTR, PChar(Result), BufferLength);
end;

procedure TImmManager.ProcessImeParameters(const AContext: HIMC; const AParameters: LPARAM; const ATextService: TTextServiceWin);
var
  CompositionString: string;
begin
  CompositionString := GetComposition(AContext);
  ATextService.InternalSetMarkedText(CompositionString);

  if AParameters and GCS_COMPATTR <> 0 then
    UpdateCompositionAttributes(AContext, ATextService);

  if AParameters and GCS_CURSORPOS <> 0 then
    UpdateCompositionCursorPos(AContext, ATextService)
  else if GetKeyboardLayout(0) and $FFFF = TTextServiceWin.LCID_Korean_Default then
    // Native Korean IME Input system doesn't use cursor position. Instead of it it hightlights the last character in
    // marked text. Since FMX doesn't support displaying character selection, we just move caret in the end of marked
    // text and use underline attribute for highlighting last character.
    ATextService.MarkedTextCursorPosition := Max(1, ATextService.MarkedText.Length);
end;

procedure TImmManager.UpdateCompositionAttributes(const AContext: HIMC; const ATextService: TTextServiceWin);
var
  BufferLength: Integer;
begin
  BufferLength := ImmGetCompositionString(AContext, GCS_COMPATTR, nil, 0);
  if BufferLength > 0 then
  begin
    SetLength(ATextService.CompAttrBuf, BufferLength);
    ImmGetCompositionString(AContext, GCS_COMPATTR, @(ATextService.CompAttrBuf[0]), BufferLength);
  end;
end;

procedure TImmManager.UpdateCompositionCursorPos(const AContext: HIMC; const ATextService: TTextServiceWin);
var
 CursorPos: Integer;
begin
  CursorPos := ImmGetCompositionString(AContext, GCS_CURSORPOS, nil, 0);
  if CursorPos >= 0 then
    ATextService.MarkedTextCursorPosition := CursorPos;
end;

function TImmManager.UpdateIMEWindowPosition: LRESULT;

  function DpToPx(const APoint: TPointF): TPoint; overload;
  var
    FormScale: Single;
  begin
    FormScale := Form.Handle.Scale;
    Result := TPointF.Create(APoint.X * FormScale, APoint.Y * FormScale).Round;
  end;

  function DpToPx(const ARect: TRectF): TRect; overload;
  begin
    Result.TopLeft := DpToPx(ARect.TopLeft);
    Result.BottomRight := DpToPx(ARect.BottomRight);
  end;

  function DefineControlRect(const AFocusedControl: IControl; const ATextInput: ITextInput): TRectF;
  var
    SelectionRect: TRectF;
  begin
    SelectionRect := ATextInput.GetSelectionRect;
    Result.TopLeft := Form.ScreenToClient(AFocusedControl.LocalToScreen(SelectionRect.TopLeft));
    Result.BottomRight := Form.ScreenToClient(AFocusedControl.LocalToScreen(SelectionRect.BottomRight));
  end;

var
  IMC: HIMC;
  Candidate: TCandidateForm;
  TextInput: ITextInput;
begin
  Result := 0;

  if not Supports(Form.Focused, ITextInput, TextInput) then
    Exit;

  IMC := ImmGetContext(FormHandle);
  if IMC = 0 then
    Exit;         
    
  try
    Candidate.dwIndex := 0;
    Candidate.dwStyle := CFS_POINT;
    Candidate.ptCurrentPos := DpToPx(TextInput.GetTargetClausePointF);
    Result := LRESULT(ImmSetCandidateWindow(IMC, @Candidate));

    Candidate.dwStyle := CFS_EXCLUDE;
    Candidate.rcArea := DpToPx(DefineControlRect(Form.Focused, TextInput));
    ImmSetCandidateWindow(IMC, @Candidate);
  finally
    ImmReleaseContext(FormHandle, IMC);
  end;
end;

procedure TImmManager.WMComposition(var Message: TMessage);
var
  IMC: HIMC;
  S: string;
  TextInput: ITextInput;
  I: Integer;
  Key: Word;
  Ch: Char;
  IsProcessed: Boolean;
begin
  UpdateIMEWindowPosition;

  IsProcessed := False;

  // User selected result string
  if Message.LParam and GCS_RESULTSTR <> 0 then
  begin
    IsProcessed := True;
    IMC := ImmGetContext(FormHandle);
    if IMC <> 0 then
    begin
      try
        if Supports(Form.Focused, ITextInput, TextInput) then
        begin
          TTextServiceWin(TextInput.GetTextService).Reset;
          TextInput.IMEStateUpdated;
        end;
        S := GetResultComposition(IMC);
      finally
        ImmReleaseContext(FormHandle, IMC);
      end;

      for I := 0 to S.Length - 1 do
      begin
        Key := 0;
        Ch := S.Chars[I];
        Form.KeyDown(Key, Ch, []);
        Form.KeyUp(Key, Ch, []);
      end;
    end;
  end;

  // User is inputting current composition string
  if Message.LParam and GCS_COMPSTR <> 0 then
  begin
    IsProcessed := True;
    IMC := ImmGetContext(FormHandle);
    if IMC <> 0 then
      try
        if Supports(Form.Focused, ITextInput, TextInput) then
        begin
          ProcessImeParameters(IMC, Message.LParam, TTextServiceWin(TextInput.GetTextService));
          TextInput.IMEStateUpdated;
        end;
      finally
        ImmReleaseContext(FormHandle, IMC);
      end;
  end;

  if IsProcessed then
    Message.Result := 0
  else
    Message.Result := DefWindowProc(FormHandle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TImmManager.WMEndComposition(var Message: TMessage);
var
  TextInput: ITextInput;
begin
  UpdateIMEWindowPosition;
  
  if Supports(Form.Focused, ITextInput, TextInput) then
    TTextServiceWin(TextInput.GetTextService).InternalEndIMEInput;
end;

procedure TImmManager.WMNotify(var Message: TMessage);
begin
  if Message.WParam = IMN_OPENCANDIDATE then
    Message.Result := UpdateIMEWindowPosition
  else
    Message.Result := DefWindowProc(FormHandle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TImmManager.WMSetContext(var Message: TMessage);
begin
  Message.Result := DefWindowProc(FormHandle, Message.Msg, Message.WParam, Message.LParam and ISC_SHOWUIALLCANDIDATEWINDOW);
end;

procedure TImmManager.WMStartComposition(var Message: TMessage);
var
  TextInput: ITextInput;
begin
  UpdateIMEWindowPosition;

  if Supports(Form.Focused, ITextInput, TextInput) then
    TTextServiceWin(TextInput.GetTextService).InternalStartIMEInput;

  if System.SysUtils.Win32MajorVersion >= 6 then
    Message.Result := DefWindowProc(FormHandle, Message.Msg, Message.WParam, Message.LParam)
  else
    Message.Result := 0;
end;

initialization
  OleInitialize(nil);
  CapturedGestureControl := nil;
  LastMousePos := ImpossibleMousePosition;
end.