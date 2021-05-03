unit FMX.CompLabel;

interface

uses
  System.Types, System.SysUtils, System.Classes, System.UITypes, 
  FMX.Types, FMX.Controls, FMX.Styles, FMX.StdCtrls, FMX.Objects;

type
  TCompLabel = class(TStyledControl)
  private
    FComposition, FProgress, FSplit: String;
    FValue, FInFrame, FOutFrame: Single;

    function GetCompName: String;
    procedure SetCompName(Value: String);

    function GetProgress: String;
    procedure SetProgress(Value: String);

    function GetValue: Single;
    procedure SetValue(Value: Single);

    function GetInFrame: Single;
    procedure SetInFrame(Value: Single);

    function GetOutFrame: Single;
    procedure SetOutFrame(Value: Single);

    function GetSplit: String;
    procedure SetSplit(Value: String);

  protected
    function GetStyleObject: TFmxObject; override;
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align default TAlignLayout.None;
    property HitTest default False;
    property Position;
    property Size;

    property Composition: String read GetCompName write SetCompName;
    property Progress: String read GetProgress write SetProgress;
    property Value: Single read GetValue write SetValue;
    property InFrame: Single read GetInFrame write SetInFrame;
    property OutFrame: Single read GetOutFrame write SetOutFrame;
    property Split: String read GetSplit write SetSplit;
  end;

procedure Register;

implementation

{$R *.res}

{ TCompLabel }

function TCompLabel.GetStyleObject: TFmxObject;
const
  Style = 'complabelstyle';
begin
  if (StyleLookup = '') then
  begin
    Result := TControl(TStyleStreaming.LoadFromResource(HInstance, Style, RT_RCDATA));
    Exit;
  end;
  Result := inherited GetStyleObject;
end;

{$REGION '    Composition    '}

function TCompLabel.GetCompName: String;
begin
  var Base: TFmxObject := FindStyleResource('comp');
  if Base <> nil then
    FComposition := TText(Base).Text;

  Result := FComposition;
end;

procedure TCompLabel.SetCompName(Value: String);
begin
  FComposition := Value;
  var Base: TFmxObject := FindStyleResource('comp');
  if Base <> nil then
    TText(Base).Text := Value;
end;

{$ENDREGION}

{$REGION '    Progress    '}

function TCompLabel.GetProgress: String;
begin
  var Base: TFmxObject := FindStyleResource('progress');
  if Base <> nil then
    FProgress := TText(Base).Text;

  Result := FProgress;
end;

procedure TCompLabel.SetProgress(Value: String);
begin
  FProgress := Value;
  var Base: TFmxObject := FindStyleResource('progress');
  if Base <> nil then
    TText(Base).Text := Value;
end;

{$ENDREGION}

{$REGION '    Value    '}

function TCompLabel.GetValue: Single;
begin
  var Base: TFmxObject := FindStyleResource('progressbar');
  if Base <> nil then
    FValue := TProgressBar(Base).Value;

  Result := FValue;
end;

procedure TCompLabel.SetValue(Value: Single);
begin
  FValue := Value;
  var Base: TFmxObject := FindStyleResource('progressbar');
  if Base <> nil then begin
    TProgressBar(Base).Value := Value;
    TProgressBar(Base).Visible := Value >= FInFrame;
  end;
end;

{$ENDREGION}

{$REGION '    InFrame    '}

function TCompLabel.GetInFrame: Single;
begin
  var Base: TFmxObject := FindStyleResource('inframe');
  if Base <> nil then
    FInFrame := StrToFloat(TText(Base).Text);

  Result := FInFrame;
end;

procedure TCompLabel.SetInFrame(Value: Single);
begin
  FInFrame := Value;
  var Base: TFmxObject := FindStyleResource('inframe');
  if Base <> nil then
    TText(Base).Text := IntToStr(Trunc(Value));

  var ProgressBarBase: TFmxObject := FindStyleResource('progressbar');
  if ProgressBarBase <> nil then
    TProgressBar(ProgressBarBase).Min := FInFrame;
end;

{$ENDREGION}

{$REGION '    OutFrame    '}

function TCompLabel.GetOutFrame: Single;
begin
  var Base: TFmxObject := FindStyleResource('outframe');
  if Base <> nil then
    FOutFrame := StrToFloat(TText(Base).Text);

  Result := FOutFrame;
end;

procedure TCompLabel.SetOutFrame(Value: Single);
begin
  FOutFrame := Value;
  var Base: TFmxObject := FindStyleResource('outframe');
  if Base <> nil then
    TText(Base).Text := IntToStr(Trunc(Value));

  var ProgressBarBase: TFmxObject := FindStyleResource('progressbar');
  if ProgressBarBase <> nil then
    TProgressBar(ProgressBarBase).Max := FOutFrame;
end;

{$ENDREGION}

{$REGION '    Split    '}

function TCompLabel.GetSplit: String;
begin
  var Base: TFmxObject := FindStyleResource('split');
  if Base <> nil then
    FSplit := TText(Base).Text;

  Result := FSplit;
end;

procedure TCompLabel.SetSplit(Value: String);
begin
  FSplit := Value;
  var Base: TFmxObject := FindStyleResource('split');
  if Base <> nil then
    TText(Base).Text := Value;
end;

{$ENDREGION}

procedure TCompLabel.ApplyStyle;
begin
  inherited;
  SetCompName(FComposition);
  SetProgress(FProgress);
  SetValue(FValue);
  SetInFrame(FInFrame);
  SetOutFrame(FOutFrame);
  SetSplit(FSplit);
end;

constructor TCompLabel.Create(AOwner: TComponent);
begin
  inherited;
  Height := 24;
  Width := 400;

  Composition := '';
  Progress := '';
  Value := -1;
  InFrame := 0;
  OutFrame := 100;
  Split := '';
  HitTest := False;
end;

procedure Register;
begin
  RegisterComponents('AErender Launcher', [TCompLabel]);
end;

end.
