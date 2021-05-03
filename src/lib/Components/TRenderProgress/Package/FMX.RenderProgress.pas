unit FMX.RenderProgress;

interface

uses
  System.SysUtils, System.Classes, System.Types,
  FMX.Types, FMX.Controls, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Styles, FMX.Memo;

type
  TRenderProgress = class(TStyledControl)
  private
    FComposition, FStatus: String;
    FValue, FInFrame, FOutFrame: Single;
    FLog: TStrings;

    function GetComposition: String;
    procedure SetComposition(Value: String);

    function GetStatus: String;
    procedure SetStatus(Value: String);

    function GetValue: Single;
    procedure SetValue(Value: Single);

    function GetInFrame: Single;
    procedure SetInFrame(Value: Single);

    function GetOutFrame: Single;
    procedure SetOutFrame(Value: Single);

    function GetLog: TStrings;
    procedure SetLog(Value: TStrings);

    procedure LogButtonClick(Sender: TObject);
  protected
    function GetStyleObject: TFmxObject; override;
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    // Inherited props
    property Align default TAlignLayout.None;
    property HitTest default True;
    property Position;
    property Size;
    property Height;
    property Width;
    property Tag;
    //property DragMode;
    //property EnableDragHighlight;

    // Inherited events
    //property OnClick;

    // Implemeted props
    property Composition: String read GetComposition write SetComposition;
    property Status: String read GetStatus write SetStatus;
    property Value: Single read GetValue write SetValue;
    property InFrame: Single read GetInFrame write SetInFrame;
    property OutFrame: Single read GetOutFrame write SetOutFrame;
    property Log: TStrings read GetLog write SetLog;

    // Implemented events

  end;

procedure Register;

implementation

{$R *.res}

{  TRenderProgress  }

function TRenderProgress.GetStyleObject: TFmxObject;
const
  Style = 'renderprogressstyle';
begin
  if StyleLookup = '' then begin
    Result := TControl(TStyleStreaming.LoadFromResource(HInstance, Style, RT_RCDATA));
    exit;
  end;
  Result := inherited GetStyleObject;
end;

{$REGION '    Composition    '}

function TRenderProgress.GetComposition: String;
begin
  var Base: TFmxObject := FindStyleResource('comp');
  if Base <> nil then
    FComposition := TLabel(Base).Text;

  Result := FComposition;
end;

procedure TRenderProgress.SetComposition(Value: String);
begin
  FComposition := Value;
  var Base: TFmxObject := FindStyleResource('comp');
  if Base <> nil then
    TLabel(Base).Text := Value;
end;

{$ENDREGION}

{$REGION '    Status    '}

function TRenderProgress.GetStatus: String;
begin
  var Base: TFmxObject := FindStyleResource('status');
  if Base <> nil then
    FStatus := TLabel(Base).Text;

  Result := FStatus;
end;

procedure TRenderProgress.SetStatus(Value: String);
begin
  FStatus := Value;
  var Base: TFmxObject := FindStyleResource('status');
  if Base <> nil then
    TLabel(Base).Text := Value;
end;

{$ENDREGION}

{$REGION '    Value    '}

function TRenderProgress.GetValue: Single;
begin
  var Base: TFmxObject := FindStyleResource('progressbar');
  if Base <> nil then begin
    FValue := TProgressBar(Base).Value;
  end;

  Result := FValue;
end;

procedure TRenderProgress.SetValue(Value: Single);
begin
  FValue := Value;
  var Base: TFmxObject := FindStyleResource('progressbar');
  if Base <> nil then begin
    TProgressBar(Base).Value := Value;

    var FramesBase: TFmxObject := FindStyleResource('frames');
    if FramesBase <> nil then begin
      TLabel(FramesBase).Text := Format('%s / %s', [IntToStr(Trunc(FValue)), IntToStr(Trunc(FOutFrame))]);
    end;

  end;
end;

{$ENDREGION}

{$REGION '    InFrame    '}

function TRenderProgress.GetInFrame: Single;
begin
  var Base: TFmxObject := FindStyleResource('progressbar');
  if Base <> nil then begin
    FInFrame := TProgressBar(Base).Min;
  end;
  
  Result := FValue;
end;

procedure TRenderProgress.SetInFrame(Value: Single);
begin
  FInFrame := Value;
  var Base: TFmxObject := FindStyleResource('progressbar');
  if Base <> nil then
    TProgressBar(Base).Min := Value;

  var FramesBase: TFmxObject := FindStyleResource('frames');
  if FramesBase <> nil then begin
    TLabel(FramesBase).Text := Format('%s / %s', [IntToStr(Trunc(FValue)), IntToStr(Trunc(FOutFrame))]);
  end;
end;

{$ENDREGION}

{$REGION '    OutFrame    '}

function TRenderProgress.GetOutFrame: Single;
begin
  var Base: TFmxObject := FindStyleResource('progressbar');
  if Base <> nil then begin
    FOutFrame := TProgressBar(Base).Max;
  end;
  
  Result := FValue;
end;

procedure TRenderProgress.SetOutFrame(Value: Single);
begin
  FOutFrame := Value;
  var Base: TFmxObject := FindStyleResource('progressbar');
  if Base <> nil then
    TProgressBar(Base).Max := Value;

  var FramesBase: TFmxObject := FindStyleResource('frames');
  if FramesBase <> nil then begin
    TLabel(FramesBase).Text := Format('%s / %s', [IntToStr(Trunc(FValue)), IntToStr(Trunc(FOutFrame))]);
  end;
end;

{$ENDREGION}

{$REGION '    Log    '}

function TRenderProgress.GetLog: TStrings;
begin
  var Base: TFmxObject := FindStyleResource('log');
  if Base <> nil then begin
    FLog := TMemo(Base).Lines;
  end;
  
  Result := FLog;
end;

procedure TRenderProgress.SetLog(Value: TStrings);
begin
  FLog := Value;
  var Base: TFmxObject := FindStyleResource('log');
  if Base <> nil then
    TMemo(Base).Lines := Value;
end;

{$ENDREGION}

procedure TRenderProgress.LogButtonClick(Sender: TObject);
begin
  var LogMemo: TFmxObject := FindStyleResource('log');
  var Panel: TFmxObject := FindStyleResource('renderprogressstyle');
  if (LogMemo <> nil) and (Panel <> nil) then
    if TMemo(LogMemo).Visible = False then begin
      TMemo(LogMemo).Visible := True;
      TPanel(Panel).Height := TPanel(Panel).Height + 192;
    end else begin
      TMemo(LogMemo).Visible := False;
      TPanel(Panel).Height := TPanel(Panel).Height - 192;
    end;
end;

procedure TRenderProgress.ApplyStyle;
begin
  inherited;
  SetComposition(FComposition);
  SetStatus(FStatus);
  SetValue(FValue);
  SetInFrame(FInFrame);
  SetOutFrame(FOutFrame);
  SetLog(FLog);

  var LogButtonBase: TFmxObject := FindStyleResource('logbutton');
  if LogButtonBase <> nil then
    TButton(LogButtonBase).OnClick := Self.LogButtonClick;
end;

constructor TRenderProgress.Create(AOwner: TComponent);
begin
  inherited;
  Composition := 'CompName';
  Status := '...';
  Value := -1;
  InFrame := 0;
  OutFrame := 100;
  //Log := TStrings.Create;
  Height := 240;
  Width := 480;
end;

procedure Register;
begin
  RegisterComponents('AErender Launcher', [TRenderProgress]);
end;

end.
