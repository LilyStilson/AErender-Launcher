unit AErenderLauncher.Math;

interface

  function IfThenElse(const AValue: Boolean; const ATrue: Variant; const AFalse: Variant): Variant;
  //function IfThenElse(const AValue: Boolean; const ATrue: TObject; const AFalse: TObject): TObject; overload;

implementation

function IfThenElse(const AValue: Boolean; const ATrue: Variant; const AFalse: Variant): Variant;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

end.
