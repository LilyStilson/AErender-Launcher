unit AErenderLauncher.IO;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
  {$ELSE MACOS}
  Posix.Stdlib, Posix.Unistd;
  {$ENDIF}

  function Open(Path: String): Integer;
  function Execute(Path: String): Integer;
  function BackgroundExecute(Path: String): Integer;

implementation

function Open(Path: String): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'open', PWideChar(Path), nil, nil, SW_SHOW);
  {$ELSE MACOS}
    Result := _system(PAnsiChar('open ' + AnsiString('"' + Path + '"')));
  {$ENDIF}
end;

function Execute(Path: String): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'OPEN', PWideChar(Path), '', '', SW_SHOWNORMAL)
  {$ELSE MACOS}
    _system(PAnsiChar('chmox +x ' + AnsiString('"' + Path + '"')));
    Result := _system(PAnsiChar('command ' + AnsiString('"' + Path + '"')));
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

end.
