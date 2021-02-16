unit AErenderLauncher.IO;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
  {$ELSE MACOS}
  Posix.Stdlib, Posix.Unistd;
  {$ENDIF}

  function Open(Path: String; Args: TArray<String>): Integer;
  function Execute(Path: String): Integer;
  function BackgroundExecute(Path: String): Integer;
  //function ExecuteAndWait(Path: String; Timeout: Cardinal): WideString;

implementation

function Open(Path: String; Args: TArray<String>): Integer;
begin
  {$IFDEF MSWINDOWS}
  var TempPath: String := Path;
  for var i := 0 to High(Args) do
    Path := Path + Args[i];

  Result := ShellExecute(0, 'open', PWideChar(TempPath), nil, nil, SW_SHOW);
  {$ELSE MACOS}
  var TempArgs: String := '';
  for var i := 0 to High(Args) do
    TempArgs := TempArgs + Format('"%s" ', [Args[i]]);

  Result := _system(PAnsiChar(Format('open -a "%s" --args %s & disown', [AnsiString(Path), AnsiString(TempArgs)]));
  {$ENDIF}
end;

function Execute(Path: String): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'OPEN', PWideChar(Path), '', '', SW_SHOWNORMAL)
  {$ELSE MACOS}
    _system(PAnsiChar(Format('chmox +x "%s"', [AnsiString(Path)])));
    Result := _system(PAnsiChar(Format('command "%s"', [AnsiString(Path)])));
  {$ENDIF}
end;

function BackgroundExecute(Path: String): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'OPEN', PWideChar(Path), '', '', SW_HIDE)
  {$ELSE MACOS}
    _system(PAnsiChar('chmox +x ' + AnsiString('"' + Path + '"')));
    Result := _system(PAnsiChar('command ' + AnsiString('"' + Path + '" & disown')));
  {$ENDIF}
end;

{function ExecuteAndWait(Path: String; Timeout: Cardinal): WideString;
begin
  var Res: WideString;
  var Elapsed: Cardinal;

  var CheckOutputThread: TThread := TThread.CreateAnonymousThread(procedure begin
    var F: TextFile;
    AssignFile(F, Path);

    Read(F, Res);
    repeat
      Sleep(1000);
      Elapsed := Elapsed + 1000;
    until (FileExists(Path) and (Res <> '')) or (Elapsed = Timeout);

    if Elapsed >= Timeout then begin
      Res := WideString('timeout');
    end
  end);

  CheckOutputThread.Start;

  Result := Res;

end;    }

function OpenAndWait(Path: String; Args: TArray<String>; Timeout: Cardinal): WideString;
begin
  // We need to create the following
  // ("Path\To\App" "Project\Path") > "JSON\Result\Path"
  //var Res: String := Format('(');


end;

end.
