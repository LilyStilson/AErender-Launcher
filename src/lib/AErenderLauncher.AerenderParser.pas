unit AErenderLauncher.AerenderParser;

(*        AErender Data Parser                                                              *)
(*        Lily Stilson // 2020                                                              *)
(*        MIT License                                                                       *)
(*                                                                                          *)
(*        Copyright (c) 2020 Alice Romanets                                                 *)
(*                                                                                          *)
(*        Permission is hereby granted, free of charge, to any person obtaining a copy      *)
(*        of this software and associated documentation files (the "Software"), to deal     *)
(*        in the Software without restriction, including without limitation the rights      *)
(*        to use, copy, modify, merge, publish, distribute, sublicense, and/or sell         *)
(*        copies of the Software, and to permit persons to whom the Software is             *)
(*        furnished to do so, subject to the following conditions:                          *)
(*                                                                                          *)
(*        The above copyright notice and this permission notice shall be included in all    *)
(*        copies or substantial portions of the Software.                                   *)
(*                                                                                          *)
(*        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR        *)
(*        IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,          *)
(*        FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE       *)
(*        AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER            *)
(*        LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,     *)
(*        OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE     *)
(*        SOFTWARE.                                                                         *)

{$EXTENDEDCOMPATIBILITY ON}

interface

uses
  System.SysUtils;

type
  ///<summary>
  ///A set of constants that will determine current aerender state.
  ///</summary>
  TAErenderLogType = record
    const
      Information: Integer = 0;
      Rendering: Integer = 1;
      Error: Integer = 2;
  end;

  ///<summary>
  ///A more convinient way to call Single-type framerates.
  ///</summary>
  TFrameRate = Real;

  ///<summary>
  ///Record that represents standard After Effects timecode data.
  ///</summary>
  TTimecode = record
    H, MM, SS, FR: Cardinal;
    private
      {Private declartions}
    public
      constructor Create(const H, MM, SS, FR: Cardinal);
      procedure Clear;
      
      /// <summary>
      /// Converts parsed timecode to string with 'H:MM:SS:FR' format.
      /// </summary>
      function ToSingleString: String;

      /// <summary>
      /// Converts parsed timecode to string with H, MM, SS, FR being separated.
      /// </summary>
      function ToExpandedString(Delimeter: String): String;

      /// <summary>
      /// Calculates total ammount of seconds from timecode
      /// </summary>
      function ToSeconds(): Cardinal;

      /// <summary>
      /// Calculates total ammount of frames from timecode
      /// </summary>
      function ToFrames(): Cardinal;
  end;

  ///<summary>
  ///Parsed aerender frame string data type.
  ///</summary>
  TAErenderFrameData = record
    Timecode: TTimecode;
    Frame: Cardinal;
    ElapsedTime: Cardinal;
    InitialMessage: String;
  end;

  /// <summary>
  /// Transforms string with 'H:MM:SS:FR' format to TTimecode.
  /// </summary>
  function StrToTimecode (const ITimecodeString: String; var ATimecode: TTimecode): String;

  function TimecodeToFrames (const Timecode: TTimecode; const FrameRate: TFrameRate): Cardinal;

  ///<summary>
  ///Parses aerender log string and returns record of it's contents.
  ///</summary>
  function ParseAErenderFrameLogString (const ILogString: String): TAErenderFrameData;

  ///<summary>
  ///Parses aerender log string and returns TTimecode value of render area duration.
  ///</summary>
  function ParseAErenderDurationLogString (const ILogString: String): TTimecode;

  ///<summary>
  ///Parses aerender log string and returns TFrameRate value of render framerate.
  ///</summary>
  function ParseAErenderFrameRateLogString (const ILogString: String): TFrameRate;

implementation

constructor TTimecode.Create(const H, MM, SS, FR: Cardinal);
begin
  Self.H  := H;
  Self.MM := MM;
  Self.SS := SS;
  Self.FR := FR;
end;

procedure TTimecode.Clear;
begin
  Self.H := 0;
  Self.MM := 0;
  Self.SS := 0;
  Self.FR := 0;
end;

function TTimecode.ToSingleString(): String;
begin
  Result := Result + Self.H.ToString + ':';

  if Self.MM < 10 then
    Result := Result + '0' + Self.MM.ToString + ':'
  else
    Result := Result + Self.MM.ToString + ':';

  if Self.SS < 10 then
    Result := Result + '0' + Self.SS.ToString + ':'
  else
    Result := Result + Self.SS.ToString + ':';

  if Self.FR < 10 then
    Result := Result + '0' + Self.FR.ToString
  else
    Result := Result + Self.FR.ToString;
end;

function TTimecode.ToExpandedString(Delimeter: String): String;
begin
  Result := Result + 'H: ' + Self.H.ToString + Delimeter;

  if Self.MM < 10 then
    Result := Result + 'M: ' + '0' + Self.MM.ToString + Delimeter
  else
    Result := Result + 'M: ' + Self.MM.ToString + ':';

  if Self.SS < 10 then
    Result := Result + 'S: ' + '0' + Self.SS.ToString + Delimeter
  else
    Result := Result + 'S: ' + Self.SS.ToString + Delimeter;

  if Self.FR < 10 then
    Result := Result + 'FR: ' + '0' + Self.FR.ToString
  else
    Result := Result + 'FR: ' + Self.FR.ToString;
end;

function TTimecode.ToSeconds(): Cardinal;
begin
  //  {H -> MM} + {MM -> SS} + SS
  Result := (Self.H * 60 * 60) + (Self.MM * 60) + Self.SS;
end;

function TTimecode.ToFrames(): Cardinal;
begin
  //  {H -> MM} + {MM -> SS} + SS
  Result := (Self.H * 60 * 60) + (Self.MM * 60) + Self.SS + Self.FR;
end;

function StrToTimecode (const ITimecodeString: String; var ATimecode: TTimecode): String;
var
  ATimecodeString: String;
begin
  ATimecodeString := ITimecodeString;

  //Read hours from timecode and remove it from temporary string
  ATimecode.H := StrToInt(ATimecodeString[1]);
  Delete(ATimecodeString, 1, 2);
  {  AString = '00:00:00 (1): 0 Seconds'  }

  //Read minutes from timecode and remove it from temporary string
  ATimecode.MM := StrToInt(ATimecodeString[1] + ATimecodeString[2]);
  Delete(ATimecodeString, 1, 3);
  {  AString = '00:00 (1): 0 Seconds'  }

  //Read seconds from timecode and remove it from temporary string
  ATimecode.SS := StrToInt(ATimecodeString[1] + ATimecodeString[2]);
  Delete(ATimecodeString, 1, 3);
  {  AString = '00 (1): 0 Seconds'  }

  //Read frames from timecode and remove it from temporary string
  ATimecode.FR := StrToInt(ATimecodeString[1] + ATimecodeString[2]);
  Delete(ATimecodeString, 1, 4);
  {  AString = '1): 0 Seconds'  }

  Result := ATimecodeString;
end;

function TimecodeToFrames (const Timecode: TTimecode; const FrameRate: TFrameRate): Cardinal;
begin
  Result := Trunc(Timecode.ToSeconds * FrameRate) + Timecode.FR;
end;

function ParseAErenderFrameLogString (const ILogString: String): TAErenderFrameData;
var 
  AString: String;
begin
  Result.InitialMessage := ILogString;
  AString := ILogString;
  {  AString = 'PROGRESS:  0:00:00:00 (1): 0 Seconds'  }
  if ILogString.Contains('PROGRESS: ') then begin
    //Get rid of PROGRESS response in initial string
    AString := AString.Replace('PROGRESS:  ', '');
    {  AString = '0:00:00:00 (1): 0 Seconds'  }

    AString := StrToTimecode(AString, Result.Timecode);

    //Read current render frame from timecode and remove it from temporary string
    var AFrame: String;
    while AString[1] <> ')' do begin
      AFrame := AFrame + AString[1];
      Delete(AString, 1, 1);
    end;
    Result.Frame := StrToInt(AFrame);
    Delete(AString, 1, 3);
    {  AString = '0 Seconds'  }

    //Read elapsed time to render one frame and clear the string
    var AElapsedTime: String;
    while AString[2] <> 'S' do begin
      AElapsedTime := AElapsedTime + AString[1];
      Delete(AString, 1, 1);
    end;
    Result.ElapsedTime := StrToInt(AElapsedTime);
    {  AString = ' Seconds'  }
  end else begin
    raise Exception.Create('Parsing error (-1): Provided string cannot be interpreted as current rendering frame data.');
  end;
end;

function ParseAErenderDurationLogString (const ILogString: String): TTimecode;
var
  AString: String;
begin
  AString := ILogString;
  {  AString = 'PROGRESS:  Duration: 0:00:10:00'  }
  //if AString.Contains ('Duration: ') then begin
    AString := AString.Replace('PROGRESS:  Duration: ', '');
    {  AString = '0:00:10:00'  }

    AString := StrToTimecode (AString, Result);
    {  AString = ''  }
  {end else begin
    raise Exception.Create('Parsing error (-1): Provided string cannot be parsed.');
  end;}

end;

function ParseAErenderFrameRateLogString (const ILogString: String): TFrameRate;
var
  AString: String;
begin
  //Added to get rid of decimal separators
  if ILogString.Contains(',') then
    AString := ILogString.Replace(',', '.')
  else
    AString := ILogString;
  {  AString = 'PROGRESS:  Frame Rate: 60.00 (comp)'  }

  //if AString.Contains ('Frame Rate: ') then begin
    AString := AString.Replace('PROGRESS:  Frame Rate: ', '');
    {  AString = '60.00 (comp)'  }
    var AFrameRate: String;
    for var i := 1 to Length (AString) do
      if AString[i + 1] <> '(' then begin
        AFrameRate := AFrameRate + AString[i]
      end else begin
        break
      end;
    Result := StrToFloat(AFrameRate);
  {end else begin
    raise Exception.Create('Parsing error (-1): Provided string cannot be parsed.');
  end;}
end;

end.
