unit AErenderLauncher.Math;

interface

  //function IfThen(const AValue: Boolean; const ATrue: Variant): Variant;

  function IfThenElse(const AValue: Boolean; const ATrue: String; const AFalse: String): String; overload;
  function IfThenElse(const AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer; overload;
  function IfThenElse(const AValue: Boolean; const ATrue: Variant; const AFalse: Variant): Variant; overload;

  function IntToCardinal(I: Integer): Cardinal;

implementation

{function IfThen(const AValue: Boolean; const ATrue: Variant): Variant;
begin
  if AValue then Result := ATrue;
end; }

function IfThenElse(const AValue: Boolean; const ATrue: String; const AFalse: String): String; overload;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IfThenElse(const AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer; overload;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IfThenElse(const AValue: Boolean; const ATrue: Variant; const AFalse: Variant): Variant; overload;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IntToCardinal(I: Integer): Cardinal;
begin
  if I < 0 then
    Result := 0
  else
    Result := I;
end;

end.
