unit Mac.NativeNotifications;

interface

uses
  System.SysUtils, Macapi.Foundation, Macapi.Helpers, Macapi.ObjectiveC,
  Macapi.ObjCRuntime, System.Classes;

  procedure ShowSystemNotification(const aTitle, aSubtitle,  aText: string);
  procedure ShowSystemNotificationWithAction(const aTitle, aSubtitle, aText, aButtonTitle: string; aButtonAction: TProc);

implementation

type

  NSUserNotificationCenterDelegate2 = interface(IObjectiveC)
    ['{07303BC1-E08C-4F22-B833-1CEAE586E423}']
    function userNotificationCenter(center: NSUserNotificationCenter; shouldPresentNotification: NSUserNotification): Boolean;  cdecl;
    procedure userNotificationCenterY(center: NSUserNotificationCenter; didActivateNotification:NSUserNotification);  cdecl;
  end;

  TNotificationCenterDelegate = class (TOCLocal, NSUserNotificationCenterDelegate2)
  public
    constructor Create();
    function userNotificationCenter(center: NSUserNotificationCenter; shouldPresentNotification: NSUserNotification): Boolean;  cdecl;
    [MethodName('userNotificationCenter:didActivateNotification:')]
    procedure userNotificationCenterY(center: NSUserNotificationCenter; didActivateNotification:NSUserNotification);  cdecl;
  end;

 var

  FDelegate: NSUserNotificationCenterDelegate2;
  FProc: TProc;

procedure ShowSystemNotification(const aTitle, aSubtitle,  aText: string);
var
  notification:  NSUserNotification;
begin
  FDelegate:= TNotificationCenterDelegate.Create;
  objc_msgSendP(
           TNSUserNotificationCenter.OCClass.defaultUserNotificationCenter,
           sel_getUid('setDelegate:'),
           (FDelegate as ILocalObject).GetObjectID
               );

  notification:= TNSUserNotification.Wrap(TNSUserNotification.alloc.init);
  if aTitle <> ''
   then notification.setTitle(StrToNSStr(aTitle));
  // In Delphi Seattle, "setSubtitle" is declared incorrectly ("setSubTitle"),
  // so we're using a low-level call here.
  if aSubtitle <> ''
   then  objc_msgSendP(
           (notification as ILocalObject).GetObjectID,
           sel_getUid('setSubtitle:'),
           (StrToNSStr(aSubtitle) as ILocalObject).GetObjectID
               );

  notification.setInformativeText(StrToNSStr(aText));
  notification.setSoundName(NSUserNotificationDefaultSoundName);


  notification.setHasActionButton(false);

  FProc:= nil;

  TNSUserNotificationCenter.Wrap(TNSUserNotificationCenter.OCClass.defaultUserNotificationCenter).deliverNotification(notification);
  notification.release;
end;

procedure ShowSystemNotificationWithAction(const aTitle, aSubtitle, aText, aButtonTitle: string; aButtonAction: TProc);
var
  notification:  NSUserNotification;
begin
  if not Assigned(FDelegate) then
  begin
    FDelegate:= TNotificationCenterDelegate.Create;
    objc_msgSendP(
           TNSUserNotificationCenter.OCClass.defaultUserNotificationCenter,
           sel_getUid('setDelegate:'),
           (FDelegate as ILocalObject).GetObjectID
               );
  end;

  notification:= TNSUserNotification.Wrap(TNSUserNotification.alloc.init);


  if aTitle <> ''
   then notification.setTitle(StrToNSStr(aTitle));
  // In Delphi Seattle, "setSubtitle" is declared incorrectly ("setSubTitle"),
  // so we're using a low-level call here.
  if aSubtitle <> ''
   then  objc_msgSendP(
           (notification as ILocalObject).GetObjectID,
           sel_getUid('setSubtitle:'),
           (StrToNSStr(aSubtitle) as ILocalObject).GetObjectID
               );

  notification.setInformativeText(StrToNSStr(aText));
  notification.setSoundName(NSUserNotificationDefaultSoundName);

  notification.setHasActionButton(true);
  notification.setActionButtonTitle(StrToNSStr(aButtonTitle));

  FProc:= aButtonAction;

  TNSUserNotificationCenter.Wrap(TNSUserNotificationCenter.OCClass.defaultUserNotificationCenter).deliverNotification(notification);
  notification.release;
end;

{ TNotificationCenterDelegate }

constructor TNotificationCenterDelegate.Create;
begin
  inherited Create;
end;

function TNotificationCenterDelegate.userNotificationCenter(
  center: NSUserNotificationCenter;
  shouldPresentNotification: NSUserNotification): Boolean;
begin
  result:= true;
//  exit;
//  TThread.CreateAnonymousThread(
//      procedure
//      begin
//        TNSUserNotificationCenter.Wrap(TNSUserNotificationCenter.OCClass.defaultUserNotificationCenter).setDelegate(nil);
//        FDelegate:=nil;
//      end).Start;
end;



procedure TNotificationCenterDelegate.userNotificationCenterY(
  center: NSUserNotificationCenter;
  didActivateNotification: NSUserNotification);
begin
  if Assigned(FProc)
    then FProc;
end;

initialization

finalization
  TNSUserNotificationCenter.Wrap(TNSUserNotificationCenter.OCClass.defaultUserNotificationCenter).setDelegate(nil);
  FDelegate:= nil;

end.
