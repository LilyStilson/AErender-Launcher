unit FMX.RenderProgress;

interface

uses
  System.Types, System.SysUtils, System.Classes,
  FMX.Types, FMX.Controls, FMX.Styles, FMX.StdCtrls, FMX.Objects, FMX.ActnList, FMX.Memo,
  AErenderLauncher.AerenderParser;

type
  TRenderProgress = class(TStyledControl)
  private
    FComposition, FStatus,
    FST_WaitingForAErender, FST_Rendering, FST_FinishedRender, FST_RenderError: String;
    FLog: TStrings;
    FInFrame, FOutFrame, FValue: Single;
    FMemoVisible, FIsError: Boolean;
    FDuration: TTimecode;
    FFrameRate: Real;

    function GetComposition: String;
    procedure SetComposition(Value: String);

//    function GetStatus: String;
//    procedure SetStatus(Value: String);

    function GetInFrame: Single;
    procedure SetInFrame(Value: Single);

    function GetOutFrame: Single;
    procedure SetOutFrame(Value: Single);

    function GetValue: Single;
    procedure SetValue(Value: Single);

    function GetLog: TStrings;
    procedure SetLog(Value: TStrings);

    function GetMemoVisible: Boolean;
    procedure SetMemoVisible(Value: Boolean);

    function GetIsError: Boolean;
    procedure SetIsError(Value: Boolean);

    procedure LogButtonClick(Sender: TObject);
  protected
    function GetStyleObject: TFmxObject; override;
    procedure ApplyStyle; override;
  public
    procedure GoToTextEnd;
    constructor Create(AOwner: TComponent); override;
  published
    property Align default TAlignLayout.None;
    property Size;
    property Position;
    property Margins;

    property Composition: String read GetComposition write SetComposition;

    property ST_WaitingForAErender: String read FST_WaitingForAErender write FST_WaitingForAErender;
    property ST_Rendering: String read FST_Rendering write FST_Rendering;
    property ST_FinishedRender: String read FST_FinishedRender write FST_FinishedRender;
    property ST_RenderError: String read FST_RenderError write FST_RenderError;

    property InFrame: Single read GetInFrame write SetInFrame;
    property OutFrame: Single read GetOutFrame write SetOutFrame;
    property Value: Single read GetValue write SetValue;
    property MemoVisible: Boolean read GetMemoVisible write SetMemoVisible;
    property IsError: Boolean read GetIsError write SetIsError;

    property Log: TStrings read GetLog write SetLog;

    property Duration: TTimecode read FDuration write FDuration;
    property FrameRate: Real read FFrameRate write FFrameRate;
  end;

procedure Register;

implementation

{$R *.res}

{$REGION 'Compoisition'}

function TRenderProgress.GetComposition: String;
begin
  FComposition := StylesData['comp.text'].AsString;
  Result := FComposition;
end;

procedure TRenderProgress.SetComposition(Value: String);
begin
  StylesData['comp.text'] := Value;
  FComposition := Value;
end;

{$ENDREGION}

{$REGION '[DEPRECATED] Status'}
//
//function TRenderProgress.GetStatus: String;
//begin
//  FStatus := StylesData['status.text'].AsString;
//  Result := FStatus;
//end;
//
//procedure TRenderProgress.SetStatus(Value: String);
//begin
//  StylesData['status.text'] := Value;
//  FStatus := Value;
//end;
//
{$ENDREGION}

{$REGION 'InFrame'}

function TRenderProgress.GetInFrame: Single;
begin
  FInFrame := StylesData['progressbar.min'].AsExtended;
  Result := FInFrame;
end;

procedure TRenderProgress.SetInFrame(Value: Single);
begin
  StylesData['progressbar.min'] := Value;
  FInFrame := Value;

  if FValue = -1 then
    StylesData['frames.text'] := '...'
  else
    StylesData['frames.text'] := Format('%s / %s', [IntToStr(Trunc(FValue)), IntToStr(Trunc(FOutFrame))])
end;

{$ENDREGION}

{$REGION 'OutFrame'}

function TRenderProgress.GetOutFrame: Single;
begin
  FOutFrame := StylesData['progressbar.max'].AsExtended;
  Result := FOutFrame;
end;

procedure TRenderProgress.SetOutFrame(Value: Single);
begin
  StylesData['progressbar.max'] := Value;
  FOutFrame := Value;

  if FValue = -1 then
    StylesData['frames.text'] := '...'
  else
    StylesData['frames.text'] := Format('%s / %s', [IntToStr(Trunc(FValue)), IntToStr(Trunc(FOutFrame))])
end;

{$ENDREGION}

{$REGION 'Value'}
function TRenderProgress.GetValue: Single;
begin
  FValue := StylesData['progressbar.value'].AsExtended;
  Result := FValue;
end;

procedure TRenderProgress.SetValue(Value: Single);
begin
  // Set Progressbar value
  StylesData['progressbar.value'] := Value;
  FValue := Value;

  // Set Progressbar visibility
  StylesData['progressbar.visible'] := Trunc(Value) >= 0;
  StylesData['ani.visible'] := Trunc(Value) = -1;

  if not FIsError then
    if FValue = -1 then begin // Waiting for aerender
      StylesData['frames.text'] := '...';
      StylesData['status.text'] := FST_WaitingForAErender;
    end else
      if (FValue >= 0) and (FValue <= FOutFrame) then begin // Rendering
        if FValue = FOutFrame then begin // Finished rendering
          StylesData['status.text'] := FST_FinishedRender;
          StylesData['progressbar.stylelookup'] := 'progressbarminifinishstyle';
        end else begin // Still rendering
          StylesData['status.text'] := FST_Rendering;
          StylesData['progressbar.stylelookup'] := 'progressbarministyle';
        end;
        StylesData['frames.text'] := Format('%s / %s', [IntToStr(Trunc(FValue)), IntToStr(Trunc(FOutFrame))])
      end;

end;
{$ENDREGION}

{$REGION 'Log'}
function TRenderProgress.GetLog: TStrings;
begin
  FLog := StylesData['log.lines'].AsType<TStrings>;
  Result := FLog;
end;

procedure TRenderProgress.SetLog(Value: TStrings);
begin
  StylesData['log.lines'] := Value;
  FLog := Value;

    var memo: TMemo := StylesData['log'].AsType<TMemo>;
  if memo <> nil then
    memo.GoToTextEnd;
end;
{$ENDREGION}

{$REGION 'Memo'}
function TRenderProgress.GetMemoVisible: Boolean;
begin
  FMemoVisible := StylesData['log.visible'].AsBoolean;
  Result := FMemoVisible;
end;

procedure TRenderProgress.SetMemoVisible(Value: Boolean);
begin
  if Value then
    Height := 240
  else
    Height := 48;
  StylesData['log.visible'] := Value;
  FMemoVisible := Value;
end;
{$ENDREGION}

{$REGION 'IsError'}
function TRenderProgress.GetIsError: Boolean;
begin
  Result := FIsError;
end;

procedure TRenderProgress.SetIsError(Value: Boolean);
begin
  if Value then begin
    StylesData['status.text'] := FST_RenderError;
    StylesData['progressbar.stylelookup'] := 'progressbarminierrorstyle';
    //StylesData['frames.text'] := '...';
  end else
    if (FValue >= 0) and (FValue < FOutFrame) then begin
      StylesData['status.text'] := FST_Rendering;
      StylesData['progressbar.stylelookup'] := 'progressbarministyle';
    end else begin
      StylesData['status.text'] := FST_FinishedRender;
      StylesData['progressbar.stylelookup'] := 'progressbarminifinishstyle';
    end;

  FIsError := Value;
end;
{$ENDREGION}

function TRenderProgress.GetStyleObject: TFmxObject;
const
  Style = 'RenderProgressDesignStyle';
begin
  if StyleLookup = '' then begin
    Result := TControl(TStyleStreaming.LoadFromResource(HInstance, Style, RT_RCDATA));
    exit;
  end;
  Result := inherited GetStyleObject;
end;

procedure TRenderProgress.ApplyStyle;
var
  LogButton: TButton;
begin
  inherited;

  if FindStyleResource<TButton>('logbutton', LogButton) then
    if not Assigned(LogButton.OnClick) then
      LogButton.OnClick := LogButtonClick;
end;

procedure TRenderProgress.LogButtonClick(Sender: TObject);
begin
  if MemoVisible then
    MemoVisible := False
  else
    MemoVisible := True;
end;

procedure TRenderProgress.GoToTextEnd;
var
  Memo: TMemo;
begin
  if FindStyleResource<TMemo>('log', Memo) then
    Memo.GoToTextEnd;
end;

constructor TRenderProgress.Create(AOwner: TComponent);
begin
  inherited;
  Height := 240;
  Width := 480;
  Composition := 'comptext';
  ST_WaitingForAErender := 'Waiting for aerender...';
  ST_Rendering := 'Rendering';
  ST_FinishedRender := 'Rendering finished';
  ST_RenderError := 'ERROR: See log for more info';
  InFrame := -1;
  OutFrame := 100;
  Value := -1;
  Log := TStringList.Create;
  MemoVisible := False;

  Duration := TTimecode.Create(0, 0, 0, 0);
  FrameRate := 0.00;
  //Log := TStrings.Create;
end;

procedure Register;
begin
  RegisterComponents('AErender Launcher', [TRenderProgress]);
end;

end.
