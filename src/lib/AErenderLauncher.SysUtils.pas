unit AErenderLauncher.SysUtils;

interface

uses
  System.SysUtils, System.IOUtils,

  {$IFDEF MSWINDOWS}
  FMX.Platform.Win, Winapi.ShellAPI, Winapi.Windows, Winapi.TlHelp32;
  {$ELSE MACOS}
  FMX.Platform.Mac, Posix.Stdlib, Posix.Unistd, Posix.SysSysctl, Posix.SysTypes;
  {$ENDIF}

  /// <summary>
  /// Gets system's memory size
  /// </summary>
  /// <returns>System's RAM size in bytes</returns>
  function GetPlatformMemorySize: Int64;

  /// <summary>
  /// Gets system's CPU threads count
  /// </summary>
  /// <returns>System's CPU threads count</returns>
  /// <remarks>
  /// This function is actually defined in RTL
  /// but inaccessable for some reason
  /// </remarks>
  function GetPlatformThreadsCount: Integer;

  /// <summary>
  /// Reads directory contents
  /// </summary>
  /// <param name="Directory">Path to directory contents of which need to be returned</param>
  /// <returns>An array of all filepaths in profidd <paramref name="Directory"/></returns>
  function GetDirectoryFiles(Directory: String): TArray<String>;

  /// <summary>
  /// Gets the count of all Launcher's temporary files
  /// </summary>
  /// <returns>Count of all Launcher's temporary files</returns>
  function GetTempFilesCount: Integer;

implementation

uses
  AErenderLauncher.Types;

function GetPlatformMemorySize: Int64;
begin
  {$IFDEF MSWINDOWS}
    var MS_Ex: MemoryStatusEx;
    FillChar(MS_Ex, SizeOf(MemoryStatusEx), #0);
    MS_Ex.dwLength := SizeOf(MemoryStatusEx);
    GlobalMemoryStatusEx(MS_Ex);
    Result := MS_Ex.ullTotalPhys;
  {$ELSE MACOS}
    var len: size_t := SizeOf(Result);
    var res: Int64 := SysCtlByName('hw.memsize', @Result, @len, nil, 0);
    if res <> 0 then
      RaiseLastOSError;
  {$ENDIF MACOS}
end;

function GetPlatformThreadsCount: Integer;
begin
  {$IFDEF MSWINDOWS}
  var SysInfo: TSystemInfo;
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
  {$ELSE MACOS}
  Result := sysconf(_SC_NPROCESSORS_ONLN);
  {$ENDIF}
end;

function GetDirectoryFiles(Directory: String): TArray<String>;
var
  Files: System.TArray<String>;
begin
  Files := TDirectory.GetFiles(Directory);
  for var i := 0 to High(Files) do
    begin
      SetLength(Result, i + 1);
      Result[i] := Files[i];
    end;
end;

function GetTempFilesCount: Integer;
begin
  var LauncherDirectory: TArray<String> := GetDirectoryFiles(APPFOLDER);
  var Res: Integer := 0;
  for var i := 0 to High(LauncherDirectory) do
    if (LauncherDirectory[i].Contains('.bat')) or (LauncherDirectory[i].Contains('.command')) or (LauncherDirectory[i].Contains('.log')) then
      inc(Res);
  Result := Res;
end;

end.
