(*      macOS Dialogs. Simplified.    *)
(*      Unit by Lily Stilson [2019]   *)
(*      Licence: MIT // Build: 001    *)

unit MacApi.Dialogs;

{$IFDEF MACOS}
interface

uses
  System.SysUtils, System.Types, System.Classes, System.TypInfo,
  FMX.Forms, FMX.Dialogs, FMX.Platform, FMX.Platform.Mac, FMX.Menus, FMX.Types,
  MacApi.AppKit, MacApi.Foundation, MacApi.CocoaTypes, Macapi.Helpers, MacApi.ObjCRuntime, Macapi.ObjectiveC;

  /// <summary>Converts standard Delphi array of Strtings to MacApi NSArray</summary>
  function ArrayToNSArray(const Arr: TArray<String>): NSArray;

  function CreateMessageView (AMessage: String): NSView;

  function CreateFileTypeView(SavePanel: NSSavePanel; Target: TCommonCustomForm; Types: NSArray): NSView;

  /// <summary>Simplified NSOpenPanel Invoker. Available flags: 'multiple', 'others', 'create', 'hidden'.</summary>
  function InvokeNSOpenPanel(const Title: String; const InitialDir: String;
    const Flags: String; const AllowedFileTypes: TArray<String>;
    var ADir: String): Boolean;

  /// <summary>Simplified NSSavePanel Invoker. Available flags: 'others', 'create', 'hidden'.</summary>
  function InvokeNSSavePanel(const Target: TCommonCustomForm; const Title: String; const InitialDir: String;
    const Flags: String; const FileTypesDescription: TArray<String>; const AllowedFileTypes: TArray<String>;
    var ADir: String): Boolean;

  /// <summary>Simplified NSOpenPanel Invoker in Directory selection mode. Available flags: 'multiple'.</summary>
  function InvokeNSDirOpenPanel(const Title: String; const InitialDir: String;
    const Flags: String; var ADir: String): Boolean;

var
  SavePanel: NSSavePanel;
  OpenPanel: NSOpenPanel;
  FOwner: Pointer;

implementation

function ArrayToNSArray(const Arr: TArray<String>): NSArray;
var
  LArgsPtrs : TArray<Pointer>;
begin
  SetLength(LArgsPtrs, Length(Arr));
  for var i := 0 to Length(Arr) - 1 do
    begin
      LArgsPtrs[i] := NSObjectToID(StrToNSStr(Arr[i]));
    end;
  Result:= TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LArgsPtrs[0], Length(LArgsPtrs)));
end;

procedure selectFormat(sender: Pointer);
begin
  if TNSPopUpButton.Wrap(SavePanel.accessoryView.subviews.objectAtIndex(1)).indexOfSelectedItem = 0 then
    begin
      //ShowMessage('selected item = ' + NSStrToStr(TNSPopUpButton.Wrap(SavePanel.accessoryView.subviews.objectAtIndex(1)).itemTitleAtIndex(0)));
      SavePanel.setNameFieldStringValue(StrToNSStr(Format('%s.%s', [NSStrToStr(SavePanel.nameFieldStringValue.stringByDeletingPathExtension), 'aep'])));
      SavePanel.setAllowedFileTypes(ArrayToNSArray(['aep']));
    end;

  if TNSPopUpButton.Wrap(SavePanel.accessoryView.subviews.objectAtIndex(1)).indexOfSelectedItem = 1 then
    begin
      //ShowMessage('selected item = ' + NSStrToStr(TNSPopUpButton.Wrap(SavePanel.accessoryView.subviews.objectAtIndex(1)).itemTitleAtIndex(1)));
      SavePanel.setNameFieldStringValue(StrToNSStr(Format('%s.%s', [NSStrToStr(SavePanel.nameFieldStringValue.stringByDeletingPathExtension), 'aer'])));
      SavePanel.setAllowedFileTypes(ArrayToNSArray(['aer']));
    end;
end;

function CreateFileTypeView(SavePanel: NSSavePanel; Target: TCommonCustomForm; Types: NSArray): NSView;
var
  AccessoryView: NSView;
  ViewLabel: NSTextField;
  FormatSelector: NSPopUpButton;
begin
  //var Handle: FMX.Platform.Mac.AlertDelegate := TMacWindowHandle.Create(TNSApplication.OCClass.sharedApplication);
  AccessoryView := TNSView.Wrap(TNSView.OCClass.alloc);
  AccessoryView.initWithFrame(NSMakeRect(0, 0, 300, 32));

  ViewLabel := TNSTextField.Wrap(TNSTextField.OCClass.alloc);
  ViewLabel.initWithFrame(NSMakeRect(0, 0, 60, 22));
  ViewLabel.setEditable(False);
  ViewLabel.setStringValue(StrToNSStr('Format:'));
  ViewLabel.setBordered(False);
  ViewLabel.setBezeled(False);
  ViewLabel.setDrawsBackground(False);

  FormatSelector := TNSPopUpButton.Wrap(TNSPopUpButton.OCClass.alloc);
  FormatSelector.initWithFrame(NSMakeRect(50, 2, 240, 22), False);
  FormatSelector.addItemsWithTitles(Types);

  var WindowHandle: NSWindow := WindowHandleToPlatform(Target.Handle).Wnd;
  FormatSelector.setAction(sel_getUid('selectFormat:'));
  //FormatSelector.setTarget(WindowHandleToPlatform(Target.Handle).Wnd.);
  //var popUpButtonHandler := TNSPopUpButton.Alloc

  //var ApplicationDelegate: NSApplicationDelegate := TNSApplication.Wrap(TNSApplication.OCClass.alloc).delegate;

  FormatSelector.setTarget(TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication).delegate);

  //Log.d(('popupButton target = ' + PAnsiChar()));

  //FormatSelector.setTarget(TNSWorkspace.OCClass.sharedWorkspace);
  AccessoryView.addSubview(ViewLabel);
  AccessoryView.addSubview(FormatSelector);

  Result := AccessoryView;
end;

function CreateMessageView (AMessage: String): NSView;
var
  AccessoryView: NSView;
  ViewLabel: NSTextField;
begin
  AccessoryView := TNSView.Wrap(TNSView.OCClass.alloc);
  AccessoryView.initWithFrame(NSMakeRect(0, 0, 380, 32));

  ViewLabel := TNSTextField.Wrap(TNSTextField.OCClass.alloc);
  ViewLabel.initWithFrame(NSMakeRect(0, 0, 380, 22));
  ViewLabel.setEditable(False);
  ViewLabel.setStringValue(StrToNSStr(AMessage));
  ViewLabel.setBordered(False);
  ViewLabel.setBezeled(False);
  ViewLabel.setDrawsBackground(False);

  AccessoryView.addSubview(ViewLabel);

  Result := AccessoryView;
end;

function InvokeNSOpenPanel(const Title: String; const InitialDir: String;
    const Flags: String; const AllowedFileTypes: TArray<String>;
    var ADir: String): Boolean;
var
  IResult: NSInteger;
  NSInitialDir: NSURL;
  multiple, others, create, hidden: Boolean;
begin
  {$REGION 'Flags Assignment'}
  if Flags.Contains('multiple') then
    multiple := True
  else
    multiple := False;

  if Flags.Contains('others') then
    others := True
  else
    others := False;

  if Flags.Contains('create') then
    create := True
  else
    create := False;

  if Flags.Contains('hidden') then
    hidden := True
  else
    hidden := False;
  {$ENDREGION}

  Result := False;
  OpenPanel := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);
  OpenPanel.setAllowsMultipleSelection(multiple);
  OpenPanel.setAllowsOtherFileTypes(others);
  OpenPanel.setCanCreateDirectories(create);
  OpenPanel.setCanSelectHiddenExtension(hidden);
  OpenPanel.setAllowedFileTypes(ArrayToNSArray(AllowedFileTypes));

  if InitialDir <> '' then
  begin
    NSInitialDir := TNSURL.Create;
    NSInitialDir.initFileURLWithPath(StrToNSStr(InitialDir));
    OpenPanel.setDirectoryURL(NSInitialDir);
  end;

  if Title <> '' then
    OpenPanel.setTitle(StrToNSStr(Title));
  OpenPanel.retain;

  try

    IResult := OpenPanel.runModal;
    if IResult = NSOKButton then
      ADir := NSStrToStr(TNSUrl.Wrap(OpenPanel.URLs.objectAtIndex(0)).relativePath);
      Result := True;
  finally
    OpenPanel.Release;
  end;
end;

function InvokeNSSavePanel(const Target: TCommonCustomForm; const Title: String; const InitialDir: String;
    const Flags: String; const FileTypesDescription: TArray<String>; const AllowedFileTypes: TArray<String>;
    var ADir: String): Boolean;
var
  IResult: NSInteger;
  NSInitialDir: NSURL;
  others, create, hidden: Boolean;
begin
  {$REGION 'Flags Assignment'}
  if Flags.Contains('others') then
    others := True
  else
    others := False;

  if Flags.Contains('create') then
    create := True
  else
    create := False;

  if Flags.Contains('hidden') then
    hidden := True
  else
    hidden := False;
  //FOwner := Owner;
  {$ENDREGION}

  Result := False;
  SavePanel := TNSSavePanel.Wrap(TNSSavePanel.OCClass.savePanel);
  SavePanel.setAllowsOtherFileTypes(others);
  SavePanel.setCanCreateDirectories(create);
  SavePanel.setCanSelectHiddenExtension(hidden);
  SavePanel.setAllowedFileTypes(ArrayToNSArray(AllowedFileTypes));
  SavePanel.setAccessoryView(CreateFileTypeView(SavePanel, Target, ArrayToNSArray(FileTypesDescription)));

  if InitialDir <> '' then
  begin
    NSInitialDir := TNSURL.Create;
    NSInitialDir.initFileURLWithPath(StrToNSStr(InitialDir));
    SavePanel.setDirectoryURL(NSInitialDir);
  end;

  if Title <> '' then
    SavePanel.setTitle(StrToNSStr(Title));
  SavePanel.retain;

  try
    IResult := SavePanel.runModal;

    if IResult = NSOKButton then
      ADir := NSStrToStr(SavePanel.URL.relativePath);
      Result := True;
  finally
    SavePanel.Release;
  end;
end;

function InvokeNSDirOpenPanel(const Title: String; const InitialDir: String;
    const Flags: String; var ADir: String): Boolean;
var
  NSInitialDir: NSURL;
  LDlgResult: NSInteger;
  multiple: Boolean;
begin
  {$REGION 'Flags Assignment'}
  if Flags.Contains('multiple') then
    multiple := True
  else
    multiple := False;
  {$ENDREGION}

  Result := False;
  OpenPanel := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);
  OpenPanel.setAllowsMultipleSelection(multiple);
  OpenPanel.setCanChooseFiles(False);
  OpenPanel.setCanChooseDirectories(True);
  if InitialDir <> '' then
  begin
    NSInitialDir := TNSURL.Create;
    NSInitialDir.initFileURLWithPath(StrToNSStr(InitialDir));
    OpenPanel.setDirectoryURL(NSInitialDir);
  end;
  if Title <> '' then
    OpenPanel.setTitle(StrToNSStr(Title));
  OpenPanel.retain;
  try
    LDlgResult := OpenPanel.runModal;
    if LDlgResult = NSOKButton then
    begin
      ADir := string(TNSUrl.Wrap(OpenPanel.URLs.objectAtIndex(0)).relativePath.UTF8String);
      Result := True;
    end;
  finally
    OpenPanel.release;
  end;
end;

{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
interface

implementation

{$ENDIF MSWINDOWS}
end.
