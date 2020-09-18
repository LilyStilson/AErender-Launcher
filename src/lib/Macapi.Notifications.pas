unit Macapi.Notifications;

interface

uses
  System.SysUtils, Macapi.Foundation, Macapi.Helpers, FMX.Dialogs;

  /// <summary>Scedules a native Cocoa notification</summary>
  procedure ScheduleNotification(const ATitle, ASubtitile, AInformation: String; const ADeliveryDate: TDateTime; const UseTimeZone: boolean = True);

  /// <summary>Sends a native Cocoa notification</summary>
  procedure PresentNotification(const ATitle, ASubtitile, AInformation: String);

implementation

procedure ScheduleNotification(const ATitle, ASubtitile, AInformation: String; const ADeliveryDate: TDateTime; const UseTimeZone: boolean = True);
begin
  var ANotification: NSUserNotification := TNSUserNotification.Wrap(TNSUserNotification.Alloc.init);
  if ATitle <> '' then
    ANotification.setTitle(StrToNSStr(ATitle));
  ANotification.setSubtitle(StrToNSStr(ASubtitile));
  ANotification.setInformativeText(StrToNSStr(AInformation));
  ANotification.setSoundName(NSUserNotificationDefaultSoundName);
  ANotification.setDeliveryDate(DateTimeToNSDate(ADeliveryDate, UseTimeZone));

  var ANotificationCenter: NSUserNotificationCenter := TNSUserNotificationCenter.Wrap(TNSUserNotificationCenter.OCClass.defaultUserNotificationCenter);
  ANotificationCenter.scheduleNotification(ANotification);
end;

procedure PresentNotification(const ATitle, ASubtitile, AInformation: String);
begin
  ShowMessage('notification start');
  var ANotification: NSUserNotification := TNSUserNotification.Wrap(TNSUserNotification.Alloc.init);
  if ATitle <> '' then
    ANotification.setTitle(StrToNSStr(ATitle));
  ANotification.setSubtitle(StrToNSStr(ASubtitile));
  ANotification.setInformativeText(StrToNSStr(AInformation));
  ANotification.setSoundName(NSUserNotificationDefaultSoundName);
  ShowMessage('notification end / deliver start');

  var ANotificationCenter: NSUserNotificationCenter := TNSUserNotificationCenter.Wrap(TNSUserNotificationCenter.OCClass.defaultUserNotificationCenter);
  ANotificationCenter.deliverNotification(ANotification);

  ShowMessage('deliver end');
end;

end.
