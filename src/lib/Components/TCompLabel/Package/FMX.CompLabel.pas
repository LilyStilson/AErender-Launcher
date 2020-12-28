unit FMX.CompLabel;

interface

uses
  System.Types, System.SysUtils, System.Classes, System.UITypes, FMX.Types, FMX.Controls, FMX.Styles, FMX.StdCtrls, FMX.Objects;

type
  TCompLabel = class(TStyledControl)
  private
    FComposition, FInFrame, FOutFrame, FSplit: String;

    function GetCompName: String;
    procedure SetCompName(Value: String);

    function GetInFrame: String;
    procedure SetInFrame(Value: String);

    function GetOutFrame: String;
    procedure SetOutFrame(Value: String);

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
    property InFrame: String read GetInFrame write SetInFrame;
    property OutFrame: String read GetOutFrame write SetOutFrame;
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
var
  Base: TFmxObject;
begin
  Base := FindStyleResource('comp');
  if Base <> nil then
    FComposition := TText(Base).Text;

  Result := FComposition;
end;

procedure TCompLabel.SetCompName(Value: String);
var
  Base: TFmxObject;
begin
  FComposition := Value;
  Base := FindStyleResource('comp');
  if Base <> nil then
    TText(Base).Text := Value;
end;

{$ENDREGION}

{$REGION '    InFrame    '}

function TCompLabel.GetInFrame: String;
var
  Base: TFmxObject;
begin
  Base := FindStyleResource('inframe');
  if Base <> nil then
    FInFrame := TText(Base).Text;

  Result := FInFrame;
end;

procedure TCompLabel.SetInFrame(Value: String);
var
  Base: TFmxObject;
begin
  FInFrame := Value;
  Base := FindStyleResource('inframe');
  if Base <> nil then
    TText(Base).Text := Value;
end;

{$ENDREGION}

{$REGION '    OutFrame    '}

function TCompLabel.GetOutFrame: String;
var
  Base: TFmxObject;
begin
  Base := FindStyleResource('outframe');
  if Base <> nil then
    FOutFrame := TText(Base).Text;

  Result := FOutFrame;
end;

procedure TCompLabel.SetOutFrame(Value: String);
var
  Base: TFmxObject;
begin
  FOutFrame := Value;
  Base := FindStyleResource('outframe');
  if Base <> nil then
    TText(Base).Text := Value;
end;

{$ENDREGION}

{$REGION '    Split    '}

function TCompLabel.GetSplit: String;
var
  Base: TFmxObject;
begin
  Base := FindStyleResource('split');
  if Base <> nil then
    FSplit := TText(Base).Text;

  Result := FSplit;
end;

procedure TCompLabel.SetSplit(Value: String);
var
  Base: TFmxObject;
begin
  FSplit := Value;
  Base := FindStyleResource('split');
  if Base <> nil then
    TText(Base).Text := Value;
end;

{$ENDREGION}

procedure TCompLabel.ApplyStyle;
begin
  inherited;
  SetCompName(FComposition);
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
  InFrame := '';
  OutFrame := '';
  Split := '';
  HitTest := False;
end;

procedure Register;
begin
  RegisterComponents('AErender Launcher', [TCompLabel]);
end;

end.
