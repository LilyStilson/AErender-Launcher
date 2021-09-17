unit AErenderLauncher.IO;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
  {$ELSE MACOS}
  Posix.Stdlib, Posix.Unistd;
  {$ENDIF}

  function Open(Path: String; Args: TArray<String> = []): Integer;
  function Execute(Path: String): Integer;
  function BackgroundExecute(Path: String): Integer;

  procedure CreateCmd(const ACommand, AFile: String);
  //function ExecuteAndWait(Path: String; Timeout: Cardinal): WideString;

implementation

function Open(Path: String; Args: TArray<String> = []): Integer;
begin
  {$IFDEF MSWINDOWS}
  var TempPath: String := Path;
  for var i := 0 to High(Args) do
    TempPath := TempPath + Args[i];

  Result := ShellExecute(0, 'open', PWideChar(TempPath), nil, nil, SW_SHOW);
  {$ELSE MACOS}
  var TempArgs: String := '';
  for var i := 0 to High(Args) do
    TempArgs := TempArgs + Format('"%s" ', [Args[i]]);

  Result := _system(PAnsiChar(Format('open -a "%s" --args %s & disown', [AnsiString(Path), AnsiString(TempArgs)])));
  {$ENDIF}
end;

function Execute(Path: String): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'OPEN', PWideChar(Path), '', '', SW_SHOWNORMAL)
  {$ELSE MACOS}
    _system(PAnsiChar(Format('chmod +x "%s"', [AnsiString(Path)])));
    Result := _system(PAnsiChar(Format('command "%s"', [AnsiString(Path)])));
  {$ENDIF}
end;

function BackgroundExecute(Path: String): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'OPEN', PWideChar(Path), '', '', SW_HIDE);
//    var exInfo: SHELLEXECUTEINFO;
//    exInfo.lpVerb := 'open';
//    exInfo.lpFile := PWideChar(Path);
//    exInfo.nShow := SW_HIDE;
//    exInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
//    Result := ShellExecuteExA(@exInfo).ToInteger;
  {$ELSE MACOS}
    _system(PAnsiChar(Format('chmod +x "%s"', [AnsiString(Path)])));
    Result := _system(PAnsiChar(Format('command "%s" & disown', [AnsiString(Path)])));
  {$ENDIF}
end;

function OpenAndWait(Path: String; Args: TArray<String>; Timeout: Cardinal): WideString;
begin
  //
end;

procedure CreateCmd(const ACommand, AFile: String);
var
  F: TextFile;
begin
  AssignFile(F, AFile);
  Rewrite(F);
  Writeln(F, ACommand);
  CloseFile(F);
end;

procedure CreateCmdAndExecute(const ACommand, AFile: String; const Background: Boolean = True);
begin
  CreateCmd(ACommand, AFile);

  if Background then
    Execute(AFile)
  else
    BackgroundExecute(AFile);
end;

procedure CreateCmdAndOpen(const ACommand, AFile: String; const Args: TArray<String>);
begin
  CreateCmd(ACommand, AFile);

  Open(AFile, Args);
end;

//initialization

end.
