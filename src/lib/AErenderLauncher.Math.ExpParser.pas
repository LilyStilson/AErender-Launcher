unit AErenderLauncher.Math.ExpParser;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils, System.Math, System.Generics.Collections;

type
  TAssociativity = (asNone,asLeft, asRight);
  TOperatorRec = record
    iOperator: char;
    Precedence: integer;
    associativity: TAssociativity;
  end;
  //Expression parsing using the Shunting-yard algorithm
  TExpressionParser = class
  private
    FFormat: TFormatSettings;
    FDecimalSeperator: Char;
    FMaxDec: integer;
    function RemoveSqrt(S: string; os: integer=1): string;
    function RemoveSqr(s: string; os: integer=1): string;
    function CreateRPN(Exp: string): string;
    function ProcessRPN(rpn: string): double;
    procedure SetDecimalSeperator(const Value: Char);
    function GetDecFormat:string;
  public
    constructor Create(ADecimalSeperator: Char = '.'; AMaxDecimals: integer = 2);
    function ParseExpressionToString(Expression: string): string;
    function ParseExpressionToFloat(Expression: string): double;
    property DecimalSeperator: Char read FDecimalSeperator write SetDecimalSeperator;
    property MaxDecimals: integer read FMaxDec write FMaxDec;
  end;

  const
    scOperators = '^*/+-()';
    MaxOperators = 6;
    FloatForm = '####################.%s';
    Operators: array [0..MaxOperators] of TOperatorRec = (
      (iOperator: '^'; Precedence: 4; associativity: asRight),
      (iOperator: '*'; Precedence: 3; associativity: asLeft),
      (iOperator: '/'; Precedence: 3; associativity: asLeft),
      (iOperator: '+'; Precedence: 2; associativity: asLeft),
      (iOperator: '-'; Precedence: 2; associativity: asLeft),
      (iOperator: '('; Precedence: 1; associativity: asNone),
      (iOperator: ')'; Precedence: 1; associativity: asNone)
    );

implementation

{ TExpressionParser }
procedure TExpressionParser.SetDecimalSeperator(const Value: Char);
begin
  FDecimalSeperator := Value;
  FFormat.DecimalSeparator := FDecimalSeperator;
end;

//Calculating square roots in advance.
function TExpressionParser.RemoveSqrt(S: string; os: integer =1): string;
var
  I,J,
  ob,
  cb,
  Depth: integer;
  num, sSqrt: string;
  C: Char;
begin
  S := UpperCase(S);
  I := Pos('SQRT',S, os);
  if (I+1 > Length(s)) or (I=0) then
    Exit(S);
  ob := Pos('(',S,I);
  Depth := 1;
  ob := Pos('(',S,I);
  J := ob;
  repeat
    J := J+1;
    if S[J] = '(' then Depth := Depth+1
    else if S[J] = ')' then Depth := Depth-1;
  until Depth = 0;
  cb := J;
  num := Copy(S, ob+1, (cb-ob)-1);
  sSqrt := Format('SQRT(%s)', [num]);

  for C in num do
    if Pos(C, scOperators)>0 then
      num := ParseExpressionToString(num);

  S := StringReplace(S, sSqrt, Format('%s',[FormatFloat(Format(FloatForm,[GetDecFormat]), Sqrt( StrToInt( num ) ) )]),[]);
  Result := RemoveSqrt(S, I+1);
end;

//Calculating squares in advance.
function TExpressionParser.RemoveSqr(s: string; os: integer=1): string;
var
  I,J,
  ob,
  cb,
  Depth: integer;
  num, sSqr: string;
  C: Char;
begin
  S := UpperCase(S);
  I := Pos('SQR',S, os);
  if (I+1 > Length(s)) or (I=0) then
    Exit(S);
  Depth := 1;
  ob := Pos('(',S,I);
  J := ob;
  repeat
    J := J+1;
    if S[J] = '(' then Depth := Depth+1
    else if S[J] = ')' then Depth := Depth-1;
  until Depth = 0;
  cb := J;
  num := Copy(S, ob+1, (cb-ob)-1);

  for C in num do
    if Pos(C, scOperators)>0 then
    begin
      num := ParseExpressionToString(num);
      break;
    end;
  sSqr := Format('SQR(%s)', [num]);
  S := StringReplace(S, sSqr, Format('%s',[FormatFloat(Format(FloatForm,[GetDecFormat]), Sqr( StrToInt( num ) ) )]),[]);
  Result := RemoveSqr(S, I+1);
end;

constructor TExpressionParser.Create(ADecimalSeperator: Char; AMaxDecimals: integer);
begin
  FDecimalSeperator := ADecimalSeperator;
  FFormat := TFormatSettings.Create;
  FFormat.DecimalSeparator := FDecimalSeperator;
  FMaxDec := AMaxDecimals;
end;

function TExpressionParser.CreateRPN(Exp: string): string;

  function FindOperator(Ch: Char): TOperatorRec;
  var
    I: integer;
  begin
    for I := 0 to MaxOperators do
      if Operators[I].iOperator = Ch then
        exit(Operators[I]);
    raise Exception.Create('Invalid operator!');
  end;

var
  numStack: TStringList;
  opList: TList<TOperatorRec>;
  currOperator: TOperatorRec;
  num: string;
  C: Char;
  I, Depth: integer;
const
  cOperators = '^+-*/()';
begin
  Depth := 0;
  Result := '';
  opList := TList<TOperatorRec>.Create;
  numStack := TStringList.Create;
  Exp := StringReplace(Exp, '--', '+', [rfReplaceAll]);
  Exp := StringReplace(Exp, ' ', '', [rfReplaceAll]);
  num := '';
  Exp := RemoveSqrt(exp);
  Exp := RemoveSqr(exp);

  for C in Exp do
  begin
    if (CharInSet(C, ['0'..'9'])or (C='.')) then
      num := num + C
    else if Pos(C, cOperators)>0 then
    begin
      if num <> '' then
        Result := Result + num + ' ';
      num := '';
      currOperator := FindOperator(C);

      if currOperator.iOperator = ')' then
      begin
        Depth := Depth-1;
        Result := Result + opList[0].iOperator + ' ';
        opList.Delete(0);
        opList.Delete(0);
      end
      else if currOperator.iOperator = '(' then
      begin
        inc(Depth);
        opList.Insert(0, currOperator);
      end
      else if currOperator.iOperator = '^' then
        opList.Insert(0, currOperator)
      else if opList.Count > 0 then
      begin
        if (opList[0].Precedence >= currOperator.Precedence) then
        begin
          Result := Result + opList[0].iOperator + ' ';
          opList.Delete(0);
          opList.Insert(0,currOperator);
        end
        else
          opList.Insert(0, currOperator);
      end
      else
        opList.Add(currOperator);
    end;
  end;
  if num <> '' then
    Result := Result + num + ' ';
  for I := 0 to opList.Count-1 do
    Result := Result + opList[I].iOperator + ' ';
end;

function TExpressionParser.GetDecFormat: string;
begin
  Result := StringOfChar('#', FMaxDec)
end;

function TExpressionParser.ParseExpressionToFloat(Expression: string): double;
var
  //Reversed polish notation
  RevPolNotation: string;
begin
  RevPolNotation := CreateRPN(Expression);
  Result := ProcessRPN(RevPolNotation);
end;

function TExpressionParser.ParseExpressionToString(Expression: string): string;
var
  //Reversed polish notation
  RevPolNotation: string;
begin
  RevPolNotation := CreateRPN(Expression);
  Result := FormatFloat(Format(FloatForm,[GetDecFormat]),ProcessRPN(RevPolNotation))
end;

function TExpressionParser.ProcessRPN(rpn: string): double;
var
  Stack: TList<double>;
  rpnList: TStringList;
  C: Char;
  I: integer;
  res: integer;
const
  cOperators = '^+-*/';
  procedure DoCalculation(code: integer);
  begin
    case code of
      42:Stack[0]:= Stack[1] * Stack[0];// Multiply
      43:Stack[0]:= Stack[1] + Stack[0];// Add
      45:Stack[0]:= Stack[1] - Stack[0];// Substract
      47:Stack[0]:= Stack[1] / Stack[0];// Divide
      94:Stack[0] := Power(Stack[1], Stack[0]);// Power
    end;
    Stack.Delete(1);
  end;
begin
  res := 0;
  Stack := TList<double>.Create;
  rpn := StringReplace(rpn, ' ', ';',[rfReplaceAll]);
  rpnList := TStringList.Create;
  rpnList.Delimiter := ';';
  rpnList.StrictDelimiter := True;
  rpnList.DelimitedText := rpn;

  Stack.Insert(0, StrToFloat(rpnList[0]));
  for I := 1 to rpnList.Count-1 do
  begin
    if rpnList[I] = '' then
      continue;
    if not CharInSet(rpnList[I][1], ['0'..'9']) then
      DoCalculation(Ord(rpnList[I][1]))
    else
      Stack.Insert(0, StrToFloat(rpnList[I]));
  end;
  Result := stack[0];
end;

end.
