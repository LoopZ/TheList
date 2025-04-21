// Copyright 2025, Jerome Shidel
// BSD 3-Clause License

unit PasExt;

{ warn 5023 off : no warning about unused units }
{$warn 5028 off : no warning about unused constants }

interface

uses
  {$if defined(windows)}
    GetText,
  {$endif}
  {$if defined(darwin)}
    MacOSAll, { CocoaAll, CocoaUtils, }
  {$endif}
    Classes, SysUtils, CRC;

const
  {$if defined(windows)}
  PlatformID = 'WIN';
  {$elseif defined(darwin)}
  PlatformID = 'OSX';
  {$elseif defined(linux)}
  PlatformID = 'LNX';
  {$elseif defined(unix)}
  PlatformID = 'UNX';
  {$else}
  PlatformID = 'UNK';
  {$endif}

const
  SPACE       = #$20;
  TAB         = #$09;
  CR          = #$0d;
  LF          = #$0a;
  UNDERSCORE  = #$5f;
  COMMA       = #$2c;
  PERIOD      = #$2e;
  CRLF        = #$0d#$0a;
  QUOTE       = #$27;
  QUOTEDOUBLE = #$22;
  QUOTEALT    = #$60;

  DirWildCard = '*';

  MaxInteger  = MaxLongInt;

  faAnything  = MaxLongInt or faAnyFile;

  APP_PRODUCTLICENSE='BSD 3-Clause License';

  ERROR_TXT   = 'ERROR: ';
  ERROR_TAB   = '       ';
  WARNING_TXT = 'WARNING: ';
  WARNING_TAB = '         ';

  TIME_ZONE : String = 'EST';


{$I version.inc}

{$I country.inc}

var
  PathDelimiter : String;

  UserLanguage  : String;      { User's Language }
  UserHomePath  : String;      { User's Home directory }
  AppExecPath   : String;      { Path of executable }
  AppExecName   : String;      { Name of executable without path }
  AppDataPath   : String;      { Location for program data files }

type
  TArrayOfBytes = array of Byte;
  TArrayOfWords = array of Word;
  TArrayOfIntegers = array of Integer;
  TArrayOfChars = TCharArray; // Same as: array of Char;
  TArrayOfStrings = TStringArray; // Same as: array of String;
  TArrayOfPointers = array of Pointer;

  // dsEverything can include viable and dead links.
  TDirScanMode = (dsFilesOnly, dsDirsOnly, dsFilesAndDirs, dsEverything);

  TForEachFileFunc = function (FileName : String) : integer of object;

function VerifiedPath (Parent, SubDir : String) : string;

function LastPos(Sub, Str : String) : integer; overload;

function PopDelim(var AStr : String; ADelim: String = SPACE): String; overload;
function FieldStr(AStr : String; AField : integer = 0; ADelim : String = SPACE) : String; overload;
function FieldStr(AStr : String; AField, ACount : integer; ADelim : String = SPACE) : String; overload;

function SubStr(AStr : String; AFrom : String; ATo : String;  MatchCase : boolean = True) : String; overload;
function SubStr(AStr : String; AFrom : String; MatchCase : boolean = True) : String; overload;
function Excise(var AStr : String; AFrom : String; ATo : String;  MatchCase : boolean = True) : String; overload;
function Excise(var AStr : String; AFrom : String; MatchCase : boolean = True) : String; overload;

function AlphaOnly(AStr : String; Substitute : String = '') : String; overload;
function AlphaNumOnly(AStr : String; Substitute : String = '') : String; overload;

function Implode(AStr: String; ADelim : String = CR) : String; overload;
function Implode(AStr: TStringArray; ADelim : String = CR) : String; overload;
function Implode(AStr: TStringList; ADelim : String = CR) : String; overload;

procedure Explode(AStr : String; var AStrs : TStringList; ADelim : String = CR); overload;
function Explode(AStr : String; ADelim : String = CR) : TArrayOfStrings; overload;

function Lookup(AStr : String; AStrs : TStringList; MatchCase : boolean = false) : LongInt; overload;
function LookupValue(AStr : String; AStrs : TStringList; Default : String; MatchCase : boolean = false) : String; overload;
function LookupValue(AStr : String; AStrs : TStringList; MatchCase : boolean = false) : String; overload;

function HasTrailing(ASubStr, AStr : String; CaseSpecific : boolean = true) : boolean; overload;
function ExcludeTrailing(ASubStr, AStr : String; CaseSpecific : boolean = true) : String; overload;
function IncludeTrailing(ASubStr, AStr : String; CaseSpecific : boolean = true) : String; overload;

function HasLeading(ASubStr, AStr : String; CaseSpecific : boolean = true) : boolean; overload;
function ExcludeLeading(ASubStr, AStr : String; CaseSpecific : boolean = true) : String; overload;
function IncludeLeading(ASubStr, AStr : String; CaseSpecific : boolean = true) : String; overload;

function WhenTrue(AState : boolean; ATrue : String;  AFalse : String = '') : String; overload;
function WhenTrue(AState : boolean; ATrue : integer; AFalse : integer = 0) : integer; overload;
function WhenTrue(AState : boolean; ATrue : pointer; AFalse : pointer = nil) : pointer; overload;

function WhenTrue(AStr : String; ATrue : String;  AFalse : String = '') : String; overload;
function WhenTrue(AStr : String; ATrue : integer; AFalse : integer = 0) : integer; overload;
function WhenTrue(AStr : String; ATrue : pointer; AFalse : pointer = nil) : pointer; overload;

function WhenTrue(AInt : Integer; ATrue : String;  AFalse : String = '') : String; overload;
function WhenTrue(AInt : Integer; ATrue : integer; AFalse : integer = 0) : integer; overload;
function WhenTrue(AInt : Integer; ATrue : pointer; AFalse : pointer = nil) : pointer; overload;

function WhenTrue(APtr : Pointer; ATrue : String;  AFalse : String = '') : String; overload;
function WhenTrue(APtr : Pointer; ATrue : integer; AFalse : integer = 0) : integer; overload;
function WhenTrue(APtr : Pointer; ATrue : pointer; AFalse : pointer = nil) : pointer; overload;

function ZeroPad(AValue : LongInt; AWidth: integer) : String; overload;
function LeftPad(AStr : String; AWidth: integer; ASubStr : String = SPACE) : String; overload;
function RightPad(AStr : String; AWidth: integer; ASubStr : String = SPACE) : String; overload;
function CenterPad(AStr : String; AWidth: integer; ASubStr : String = SPACE) : String; overload;
function StringOf(AStr : String; ALength : integer; ACrop : boolean = True) : String;

function SimpleCheckSum(const AStr : String) : word; overload;

procedure ClearArray(var A : TArrayOfBytes; ALength : integer = 0); overload;
procedure ClearArray(var A : TArrayOfWords; ALength : integer = 0); overload;
procedure ClearArray(var A : TArrayOfIntegers; ALength : integer = 0); overload;
procedure ClearArray(var A : TArrayOfChars; ALength : integer = 0); overload;
procedure ClearArray(var A : TArrayOfStrings; ALength : integer = 0); overload;
procedure ClearArray(var A : TArrayOfPointers; ALength : integer = 0); overload;

function AddToArray(var A : TArrayOfBytes; B : Byte) : integer; overload;
function AddToArray(var A : TArrayOfWords; W : Word) : integer; overload;
function AddToArray(var A : TArrayOfIntegers; I : Integer) : integer; overload;
function AddToArray(var A : TArrayOfChars; C : Char) : integer; overload;
function AddToArray(var A : TArrayOfStrings; S : String) : integer; overload;
function AddToArray(var A : TArrayOfPointers; P : Pointer) : integer; overload;

function InArray(AStr : String; A : TArrayOfStrings; CaseSpecific : boolean = true) : boolean; overload;
function ArrayPos(AStr : String; A : TArrayOfStrings; CaseSpecific : boolean = true) : integer; overload;

function ForEachFile(AProc: TForEachFileFunc; APath : String; ARecurse : boolean = True) : integer; overload;

procedure DirScan(var List : TStringList; APathSpec : String; Recurse : boolean = false; Mode : TDirScanMode = dsFilesOnly);  overload;
procedure DirScan(out List : TArrayOfStrings; APathSpec : String; Recurse : boolean = false; Mode : TDirScanMode = dsFilesOnly);  overload;
procedure DirScanByDate(var List : TStringList; APathSpec : String; Mode : TDirScanMode = dsFilesOnly);  overload;
procedure DirScanByDate(out List : TArrayOfStrings; APathSpec : String; Mode : TDirScanMode = dsFilesOnly);  overload;

function SaveToFile(AFileName: String; AValue : String; ARaise : boolean = true) : integer; overload;
function SaveToFile(AFileName: String; AValue : TArrayOfBytes; ARaise : boolean = true) : integer; overload;
function AppendToFile(AFileName: String; AValue : String; ARaise : boolean = true) : integer; overload;
function AppendToFile(AFileName: String; AValue : TArrayOfBytes; ARaise : boolean = true) : integer; overload;
function LoadFromFile(AFileName: String; out AValue : String; ARaise : boolean = true) : integer; overload;
function LoadFromFile(AFileName: String; out AValue : TArrayOfBytes; ARaise : boolean = true) : integer; overload;

function StrToInts(const S : UTF8String) : string; overload;
function IntsToStr(S : String): UTF8String;  overload;

function WordCase(S : String; Allowable : String = '') : String;
function RandomStr(ALength : Integer; AChars : String = '') : String;

{ Word wrap min of 0 means hard wrap and cut lines at Max when cannot split with
a soft-wrap before Max. Indent amount for after each line wrapped. LineEndings
default to CRLF }
function WordWrap(S : String; Max : integer; Min : integer = 1;
  Indent : integer = 0; LineEndings : String = CRLF) : String;

{ returns true if the filename is a filesystem link for a file or dir }
function IsLink(AFilename : String) : boolean;

{ only returns true when the specified item exists, is a link and is broken
before it resolves to the ultimate target of a link or link chain. }
function IsDeadLink(AFileName : String) : boolean;

{ only returns true when the specified item exists and is not broken
before it resolves to the ultimate target of a link or link chain. }
function LinkExists(AFileName : String) : boolean;

{ Returns null string if it does not exist. Returns the target of a filesystem
  link of the file/dir name. If final is true, it will return the final link
  of a chain of links (or null string if the chain is broken). }
function LinkTarget(AFileName : String; Final:Boolean=false) : String;

function CreateLink(Target, Link : String) : boolean;

{ delete a file or entire directory tree }
function DeleteTree (APath: String) : boolean;

{ make a temp directory under a path. If path is not spcified, it will attempt
  to create the directory using the system default path for temporary files.
  Note: While it is a good idea to use the RmTempDir procedure to delete the
  temporary directory and anything under it, this will happen when the  program
  termintates. Regardless if that termination is "clean" or caused by a "Ctrl+C"
  or an exception is raised and not handled. }
function MkTempDir (Parent : String = ''): string;
procedure RmTempDir(ATempDir : String);

function DirectoryIsEmpty(APath : String) : boolean;

// FileCopy does not preserve Owner, Group or permissions
function FileCopy(Source, Dest : String; Overwrite : boolean = true) : integer;
function FileMove(Source, Dest : String; Overwrite : boolean = true) : integer;

// Recursively set permissions of all directories and files under and including
// the specified directory. Default is FILES (RW,R,R) and DIRS (RWX,RX,RX).
function PathPermissions(Path : String; Files : integer = &644;
  Dirs : integer = &755) : boolean;

// Recursively stamp all paths to their latest file/directory contents
function PathStamp(Path : String) : LongInt;

function SizeOfFile(FileName:String; out Size : Int64; FollowLinks : boolean=true) : boolean;
function FileCompare(First, Second : String) : integer;
function FileCRC32(FileName : String) : LongInt;

implementation

{$if defined(windows)}

  function IsLink(AFilename : String) : boolean;
  begin
    IsLink:=False;
    raise Exception.Create('function "IsLink" not implemented for Windows.');
  end;

  function LinkTarget(AFileName : String; Final:Boolean=false) : String;
  begin
    LinkTarget:=AFileName;
    raise Exception.Create('function "LinkTarget" not implemented for Windows.');
  end;

  function IsDeadLink(AFileName : String) : boolean;
  begin
    IsDeadLink:=False;
    raise Exception.Create('function "IsDeadLink" not implemented for Windows.');
  end;

  function LinkExists(AFileName : String) : boolean;
  begin
    LinkExists:=False;
    raise Exception.Create('function "LinkExists" not implemented for Windows.');
  end;

  function PathPermissions(Path : String; Files : integer; Dirs : integer) : boolean;
  begin
     PathPermissions:=False;
     raise Exception.Create('function "PathPermissions" not implemented for Windows.');
   end;

  function CreateLink(Target, Link : String) : boolean;
  begin
      CreateLink:=False;
      raise Exception.Create('function "CreateLink" not implemented for Windows.');
    end;

{$else}

  uses baseUnix, Unix;

  {$PUSH} {$hints off}

  function PathPermissions(Path : String; Files : integer; Dirs : integer) : boolean;
  var
    R : Integer;
    S : TSearchRec;
  begin
    PathPermissions:=fpChMod(ExcludeTrailingPathDelimiter(Path), Dirs)=0;
    if not PathPermissions then Exit;
    Path:=IncludeTrailingPathDelimiter(Path);
    R := FindFirst(Path + DirWildCard, faAnyFile, S);
    if R <> 0 then PathPermissions:=False;
    while R = 0 do begin
      if (S.Name = '.') or (S.Name = '..') then begin
        {Ignore}
      end else if S.Attr and faDirectory = faDirectory then begin
        PathPermissions:=PathPermissions(Path + S.Name, Files, Dirs);
      end else begin
        PathPermissions:=fpChMod(Path+S.Name, Files)=0;
      end;
      if not PathPermissions then Break;
      R := FindNext(S);
    end;
    FindClose(S);
  end;


  function IsLink(AFilename : String) : boolean;
  var
    S : Stat;
  begin
    IsLink := False;
    if fpLStat(AFileName, S) = 0 then
      IsLink:=fpS_ISLNK(S.st_mode);
  end;

  function LinkTarget(AFileName : String; Final:Boolean=false) : String;
  var
    S : Stat;
    E : integer;
  begin
    E := 35;
    while E > 0 do begin
      Dec(E);
      LinkTarget := fpReadLink(AFileName);
      // dead link
      if LinkTarget='' then Break;
      // adjust path to be relative to previous when needed
      if LinkTarget[1] <> PathDelimiter then begin
        LinkTarget := ExtractFilePath(AFileName) + LinkTarget;
      end;
      AFileName:=LinkTarget;
      // check if it is valid
      if fpLStat(LinkTarget, S) <> 0 then begin
        LinkTarget:='';
        Break;
      end;
      // end of chain
      if not fpS_ISLNK(S.st_mode) then break;
    end;
  end;

  function IsDeadLink(AFileName : String) : boolean;
  begin
    IsDeadLink := IsLink(AFileName) and (LinkTarget(AFileName, True)='');
  end;

  function LinkExists(AFileName : String) : boolean;
  begin
    LinkExists := IsLink(AFileName) and (LinkTarget(AFileName, True)<>'');
  end;

  function CreateLink(Target, Link : String) : boolean;
  begin
    CreateLink:=fpSymLink(PChar(Target), PChar(Link)) = 0;
  end;

  {$POP}

{$endif}

var
  TempItems:TStringList;

procedure DoneUnit(Aborted:boolean);
var
  I : integer;
begin
  if Aborted then
    WriteLn('Emergency shutdown clean-up.');
  if Assigned(TempItems) then begin
    for I := TempItems.Count - 1 downto 0 do
      RmTempDir(TempItems[I]);
    FreeAndNil(TempItems);
  end;
end;

procedure HandleSigInt(aSignal: LongInt); cdecl;
begin
  if aSignal=2 then
    Writeln(' - Abort!');
  DoneUnit(True);
  Halt(ExitCode);
end;

procedure InitUnit;
var
  Executable : String;
  {$if defined(darwin)}
    lbuf :  StringPtr;
  {$elseif defined(windows)}
    Lng : String;
  {$else}
    ltmp : String;
  {$endif}
begin
  Randomize;
  TempItems:=nil;
  PathDelimiter := IncludeTrailingPathDelimiter('');
  UserLanguage := '';
  UserHomePath := IncludeTrailingPathDelimiter(SysUtils.GetEnvironmentVariable('HOME'));
  AppExecPath := ExtractFilePath(ExpandFileName(ParamStr(0)));
  Executable  := ExtractFileName(Paramstr(0));
  AppExecName := Executable;
  SetLength(Executable, Length(Executable) - Length(ExtractFileExt(ParamStr(0))));
  Executable := Lowercase(Executable);

  AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + Executable);
  {$if defined(windows)}
    UserHomePath := IncludeTrailingPathDelimiter(
      GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH'));
    GetLanguageIDs(UserLanguage, Lng);
    AppDataPath  := IncludeTrailingPathDelimiter(
      IncludeTrailingPathDelimiter(GetEnvironmentVariable('LOCALAPPDATA')) + Executable);
  {$elseif defined(darwin)}
    lbuf := New(StringPtr);
    if CFStringGetPascalString(CFLocaleGetIdentifier(CFLocaleCopyCurrent),
      lbuf, Sizeof(lbuf^), 0) then UserLanguage := String(lbuf^);
    Dispose(lbuf);
    AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + '.' + Executable);
  {$elseif defined(linux) or defined(unix)}
    ltmp := FieldStr(GetEnvironmentVariable('LANG'), 0, '.');
    if ltmp <> '' then UserLanguage := ltmp;
    AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + '.' + Executable);
  {$else}
    ltmp := FieldStr(GetEnvironmentVariable('LANG'), 0, '.');
    if ltmp <> '' then UserLanguage := ltmp;
    AppDataPath  := IncludeTrailingPathDelimiter(UserHomePath + Executable);
  {$ifend}

  { Install Control-C handler to delete temp files/dirs. Not bothering to
  test if it was successfully installed. If it was installed, yay! If not,
  whatever. }
  FpSignal(SigInt, @HandleSigInt);

end;

function VerifiedPath(Parent, SubDir: String): string;
begin
  Result := '';
  if Parent = '' then exit;
  Parent := IncludeTrailingPathDelimiter(Parent);
  if not DirectoryExists(Parent + SubDir) then
     if not CreateDir(Parent + SubDir) then exit;
  Result := IncludeTrailingPathDelimiter(Parent + SubDir);
end;

function LastPos(Sub, Str: String): integer;
var
  I, J : integer;
begin
  I := Pos(Sub, Str);
  if I > 0 then begin
    J := I;
    repeat
       I := J;
       J := Pos(Sub, Str, I + 1);
    until J < I;
  end;
  Result := I;
end;

function PopDelim(var AStr : String; ADelim: String = SPACE): String;
var
  P : integer;
begin
  P := Pos(ADelim, AStr);
  if P <= 0 then P := Length(AStr) + 1;
  Result := Copy(AStr, 1, P - 1);
  Delete(AStr, 1, P - 1 + Length(ADelim));
end;

function FieldStr(AStr: String; AField: integer; ADelim: String): String;
begin
  Result := '';
  if AField >= 0 then
    repeat
        Result := PopDelim(AStr, ADelim);
        Dec(AField);
    until (AField < 0) or ((Result = '') and (AStr = ''));
end;

function FieldStr(AStr: String; AField, ACount: integer; ADelim: String
  ): String;
begin
  Result := '';
  if (AField >= 0) and (ACount <> 0) then
    repeat
        if AField > 0 then begin
          PopDelim(AStr, ADelim);
          Dec(AField);
        end else begin
          if Result <> '' then Result := Result + ADelim;
          Result := Result + PopDelim(AStr, ADelim);
          if ACount > 0 then Dec(ACount);
        end;
    until (ACount = 0) or  (AStr = '');
end;

function SubStrExcise(var AStr : String; AFrom : String; ATo : String;  MatchCase, Remove : boolean) : String; overload;
var
  S : String;
  P, E, EL : integer;
begin
  if not MatchCase then begin
     AFrom := UpperCase(AFrom);
     ATo := UpperCase(ATo);
     S := UpperCase(AStr);
  end else
      S := AStr;
  P := Pos(AFrom, S);
  if P > 1 then begin
    EL := Length(ATo);
    if EL = 0 then begin
      E := 0;
      EL := 1;
    end else
       E := Pos(ATo, S, P + Length(AFrom));
    if E < 1 then E := Length(S) + 1;
    Result := Copy(AStr, P + Length(AFrom), E - P - Length(AFrom));
    if Remove then
      Delete(AStr, P, E - P + Length(ATo));
  end else
    Result := '';
end;

function SubStr(AStr : String; AFrom : String; ATo : String;  MatchCase : boolean = True) : String; overload;
begin
  Result := SubStrExcise(AStr, AFrom, ATo, MatchCase, False);
end;

function SubStr(AStr : String; AFrom : String; MatchCase : boolean = True) : String; overload;
begin
  Result := SubStrExcise(AStr, AFrom, '', MatchCase, False);
end;

function Excise(var AStr : String; AFrom : String; ATo : String;  MatchCase : boolean = True) : String; overload;
begin
  Result := SubStrExcise(AStr, AFrom, ATo, MatchCase, True);
end;

function Excise(var AStr : String; AFrom : String; MatchCase : boolean = True) : String; overload;
begin
  Result := SubStrExcise(AStr, AFrom, '', MatchCase, True);
end;

function AlphaOnly(AStr: String; Substitute: String): String;
var
  I : integer;
begin
  Result := '';
  for I := 1 to length(AStr) do
    if (AStr[I] in [#$41..#$5A,#$61..#$7A]) then
      Result := Result + AStr[I]
    else
      Result:=Result + Substitute;
end;

function AlphaNumOnly(AStr: String; Substitute: String): String;
var
  I : integer;
begin
  Result := '';
  for I := 1 to length(AStr) do
    if (AStr[I] in [#$30..#$39,#$41..#$5A,#$61..#$7A]) then
      Result := Result + AStr[I]
    else
      Result:=Result + Substitute;
end;

function Implode(AStr: String; ADelim : String ) : String; overload;
begin
  Result :=
    StringReplace(
      StringReplace(
        StringReplace(AStr, CRLF, ADelim, [rfReplaceAll]),
      LF, ADelim, [rfReplaceAll]),
    CR, ADelim, [rfReplaceAll]);
end;

function Implode(AStr: TStringArray; ADelim: String): String;
var
  I : integer;
begin
  Result := '';
  I := 0;
  While I < Length(AStr) do begin
    Result := Result + AStr[I];
    Inc(I);
    if I < Length(AStr) then
      Result := Result + ADelim;
  end;
end;

function Implode(AStr: TStringList; ADelim: String): String;
var
  I : integer;
begin
  Result := '';
  I := 0;
  While I < AStr.Count do begin
    Result := Result + AStr[I];
    Inc(I);
    if I < AStr.Count then
      Result := Result + ADelim;
  end;
end;

procedure Explode(AStr : String; var AStrs : TStringList; ADelim : String); overload;
var
  S : String;
begin
  While Length(AStr) > 0 do begin
    S := PopDelim(AStr, ADelim);
    AStrs.Add(S);
  end;
end;

function Explode(AStr : String; ADelim : String):TArrayOfStrings;
var
  I : Integer;
begin
  I := 0;
  Explode:=[];
  While Length(AStr) > 0 do begin
    if I = Length(Explode) then
      SetLength(Explode, Length(Explode) + 50);
    Explode[I] := PopDelim(AStr, ADelim);
    Inc(I);
  end;
  SetLength(Explode, I);
end;

function Lookup(AStr: String; AStrs: TStringList; MatchCase: boolean): LongInt;
var
  I : LongInt;
begin
  AStr := AStr + '=';
  Result := -1;
  if not MatchCase then begin
    AStr := UpperCase(AStr);
    for I := 0 to AStrs.Count - 1 do begin
      if Uppercase(Copy(AStrs[I], 0, Length(AStr))) = AStr then begin
         Result := I;
         Break;
      end;
    end;
  end else begin
    for I := 0 to AStrs.Count - 1 do begin
      if Copy(AStrs[I], 0, Length(AStr)) = AStr then begin
         Result := I;
         Break;
      end;
    end;
  end;
end;

function LookupValue(AStr: String; AStrs: TStringList; Default: String;
  MatchCase: boolean = false): String;
var
  I : LongInt;
  S : String;
begin
  I := Lookup(AStr, AStrs, MatchCase);
  if I = -1 then
     Result := Default
  else begin
    S := AStrs[I];
    PopDelim(S, '=');
    if Copy(S, 1, 1) = Copy(S, Length(S), 1) then begin
      if Copy(S, 1, 1) = '"' then begin
        Delete(S, 1, 1);
        Delete(S, Length(S), 1);
      end;
    end;
    Result := S;
  end;
end;

function LookupValue(AStr: String; AStrs: TStringList; MatchCase: boolean = false
  ): String;
begin
  Result := LookupValue(AStr, AStrs, '', MatchCase);
end;

function HasTrailing(ASubStr, AStr: String; CaseSpecific: boolean): boolean;
begin
  if CaseSpecific then
    Result := Copy(AStr, Length(AStr) - Length(ASubStr) + 1) = ASubStr
  else
    Result := Uppercase(Copy(AStr, Length(AStr) - Length(ASubStr) + 1)) = Uppercase(ASubStr);
end;

function ExcludeTrailing(ASubStr, AStr: String; CaseSpecific: boolean): String;
begin
  if HasTrailing(ASubStr, AStr, CaseSpecific) then
    Result := Copy(AStr, 1, Length(AStr) - Length(ASubStr))
  else
    Result := AStr;
end;

function IncludeTrailing(ASubStr, AStr: String; CaseSpecific: boolean): String;
begin
  if HasTrailing(ASubStr, AStr, CaseSpecific) then
    Result := AStr
  else
    Result := AStr + ASubStr;
end;

function HasLeading(ASubStr, AStr: String; CaseSpecific: boolean): boolean;
begin
  if CaseSpecific then
    Result := Copy(AStr, 1, Length(ASubStr)) = ASubStr
  else
    Result := Uppercase(Copy(AStr, 1, Length(ASubStr))) = Uppercase(ASubStr);
end;

function ExcludeLeading(ASubStr, AStr: String; CaseSpecific: boolean): String;
begin
  if HasLeading(ASubStr, AStr, CaseSpecific) then
    Result := Copy(AStr, Length(ASubStr) + 1)
  else
    Result := AStr;
end;

function IncludeLeading(ASubStr, AStr: String; CaseSpecific: boolean): String;
begin
  if HasLeading(ASubStr, AStr, CaseSpecific) then
    Result := AStr
  else
    Result := ASubStr + AStr;
end;

function WhenTrue(AState: boolean; ATrue: String; AFalse: String): String;
begin
  if AState then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AState: boolean; ATrue: integer; AFalse: integer): integer;
begin
  if AState then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AState: boolean; ATrue: pointer; AFalse: pointer): pointer;
begin
  if AState then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AStr: String; ATrue: String; AFalse: String): String;
begin
  if AStr <> '' then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AStr: String; ATrue: integer; AFalse: integer): integer;
begin
  if AStr <> '' then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AStr: String; ATrue: pointer; AFalse: pointer): pointer;
begin
  if AStr <> '' then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AInt: Integer; ATrue: String; AFalse: String): String;
begin
  if AInt <> 0 then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AInt: Integer; ATrue: integer; AFalse: integer): integer;
begin
  if AInt <> 0 then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(AInt: Integer; ATrue: pointer; AFalse: pointer): pointer;
begin
  if AInt <> 0 then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(APtr: Pointer; ATrue: String; AFalse: String): String;
begin
  if Assigned(APtr) then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(APtr: Pointer; ATrue: integer; AFalse: integer): integer;
begin
  if Assigned(APtr) then
    Result := ATrue
  else
    Result := AFalse;
end;

function WhenTrue(APtr: Pointer; ATrue: pointer; AFalse: pointer): pointer;
begin
  if Assigned(APtr) then
    Result := ATrue
  else
    Result := AFalse;
end;

function ZeroPad(AValue : LongInt; AWidth: integer): String;
begin
  Result := LeftPad(IntToStr(AValue), AWidth, '0');
end;

function LeftPad(AStr: String; AWidth: integer; ASubStr: String): String;
begin
  if ASubStr = '' then ASubStr := SPACE;
  While (Length(AStr) < AWidth) do begin
    AStr := ASubStr + AStr;
  end;
  Result := AStr;
end;

function RightPad(AStr: String; AWidth: integer; ASubStr: String): String;
begin
  if ASubStr = '' then ASubStr := SPACE;
  While (Length(AStr) < AWidth) do begin
    AStr := AStr + ASubStr;
  end;
  Result := AStr;
end;

function CenterPad(AStr: String; AWidth: integer; ASubStr: String): String;
begin
  if ASubStr = '' then ASubStr := SPACE;
  While (Length(AStr) < AWidth) do begin
    AStr := AStr + ASubStr;
    if (Length(AStr) < AWidth) then
      AStr := ASubStr + AStr;
  end;
  Result := AStr;
end;

function StringOf(AStr : String; ALength : integer; ACrop : boolean = True) : String;
begin
  if AStr = '' then AStr := SPACE;
  StringOf:=AStr;
  While (Length(StringOf) < ALength) do begin
    StringOf := StringOf + AStr;
  end;
  if ACrop then
    StringOf:=Copy(StringOf, 1, ALength);
end;


function SimpleCheckSum(const AStr: String): word;
var
  Sum: word;
  I : integer;
begin
  Sum:= 0;
  for I := 1 to Length(AStr) do
    Sum:=word((Sum shr 1) or ((Sum and 1) shl 15)) + Ord(AStr[I]);
  Result:=Sum;
end;

procedure ClearArray(var A: TArrayOfBytes; ALength: integer);
var
  I : integer;
begin
  SetLength(A, ALength);
  if Length(A) > 0 then
    for I := Low(A) to High(A) do
      A[I] := 0;
end;

procedure ClearArray(var A: TArrayOfWords; ALength: integer);
var
  I : integer;
begin
  SetLength(A, ALength);
  if Length(A) > 0 then
    for I := Low(A) to High(A) do
      A[I] := 0;
end;

procedure ClearArray(var A: TArrayOfIntegers; ALength: integer);
var
  I : integer;
begin
  SetLength(A, ALength);
  if Length(A) > 0 then
    for I := Low(A) to High(A) do
      A[I] := 0;
end;

procedure ClearArray(var A: TArrayOfChars; ALength: integer);
var
  I : integer;
begin
  SetLength(A, ALength);
  if Length(A) > 0 then
    for I := Low(A) to High(A) do
      A[I] := #0;
end;

procedure ClearArray(var A: TArrayOfStrings; ALength: integer);
var
  I : integer;
begin
  SetLength(A, ALength);
  if Length(A) > 0 then
    for I := Low(A) to High(A) do
      A[I] := '';
end;

procedure ClearArray(var A: TArrayOfPointers; ALength: integer);
var
  I : integer;
begin
  SetLength(A, ALength);
  if Length(A) > 0 then
    for I := Low(A) to High(A) do
      A[I] := nil;
end;

function AddToArray(var A: TArrayOfBytes; B: Byte): integer;
begin
  SetLength(A, Length(A) + 1);
  A[High(A)] := B;
  Result := Length(A);
end;

function AddToArray(var A: TArrayOfWords; W: Word): integer;
begin
  SetLength(A, Length(A) + 1);
  A[High(A)] := W;
  Result := Length(A);
end;

function AddToArray(var A: TArrayOfIntegers; I: Integer): integer;
begin
  SetLength(A, Length(A) + 1);
  A[High(A)] := I;
  Result := Length(A);
end;

function AddToArray(var A: TArrayOfChars; C: Char): integer;
begin
  SetLength(A, Length(A) + 1);
  A[High(A)] := C;
  Result := Length(A);
end;

function AddToArray(var A : TArrayOfStrings; S : String) : integer;
begin
  SetLength(A, Length(A) + 1);
  A[High(A)] := S;
  Result := Length(A);
end;

function AddToArray(var A: TArrayOfPointers; P: Pointer): integer;
begin
  SetLength(A, Length(A) + 1);
  A[High(A)] := P;
  Result := Length(A);
end;

function InArray(AStr: String; A: TArrayOfStrings; CaseSpecific: boolean
  ): boolean;
begin
  Result := ArrayPos(AStr, A, CaseSpecific) <> -1;
end;

function ArrayPos(AStr: String; A: TArrayOfStrings; CaseSpecific: boolean
  ): integer;
var
  I : integer;
begin
  Result := -1;
  if CaseSpecific then begin
    for I := Low(A) to High(A) do
      if AStr = A[I] then begin
        Result := I;
        Exit;
      end;
  end else begin
    AStr := Uppercase(AStr);
    for I := Low(A) to High(A) do
      if AStr = Uppercase(A[I]) then begin
        Result := I;
        Exit;
      end;
  end;
end;

function ForEachFile(AProc: TForEachFileFunc; APath : String; ARecurse : boolean) : integer;
var
  PathFlag : boolean;
  AnyFile : boolean;

  function ForEachFiles(const ASubPath : String) : integer;
  var
    R : integer;
    Search : TSearchRec;
  begin
    if ARecurse then begin
      R := FindFirst(IncludeTrailingPathDelimiter(APath + ASubPath) + '*', faAnyFile, Search);
      while (R = 0) do begin
        if (Search.Attr and faDirectory = faDirectory) then begin
          if (Search.Name <> '.') and (Search.Name <> '..') then
            R := ForEachFiles(ASubPath + WhenTrue(ASubPath <> '', DirectorySeparator) + Search.Name);
        end;
        if R = 0 then
          R := FindNext(Search);
      end;
      FindClose(Search);
    end;

    if (R = 0) or (R = -1) then begin
      R := FindFirst(IncludeTrailingPathDelimiter(APath + ASubPath) + '*', faAnyFile, Search);
      while (R = 0) do begin
        if (Search.Attr and faDirectory <> faDirectory) then begin
           AnyFile := True;
           R := AProc(WhenTrue(PathFlag, '', DirectorySeparator) +
             ASubPath + WhenTrue(ASubPath <> '', DirectorySeparator) + Search.Name);
        end;
        if R = 0 then
          R := FindNext(Search);
      end;
      FindClose(Search);
    end;

    if (R = -1) then R := 0;
    Result := R;
  end;

begin
  AnyFile := True;
  PathFlag := APath = IncludeTrailingPathDelimiter(APath);
  if not PathFlag then
    APath := IncludeTrailingPathDelimiter(APath);
  Result := ForEachFiles('');
  if (Result = 0) and (Not AnyFile) then
    Result := -1;
end;

procedure DirScan(var List : TStringList; APathSpec : String; Recurse : boolean = false;
  Mode : TDirScanMode = dsFilesOnly);  overload;

  var
    ScanAttr : LongInt;

  function ScanPath(ASub, APath : String) : integer;
  var
    R : integer;
    Search : TSearchRec;
  begin
    R := FindFirst(APath, ScanAttr, Search);
    while (R = 0) do begin
      if (Search.Attr and faDirectory <> faDirectory) then begin
        if Mode <> dsDirsOnly then
          List.Add(ASub + Search.Name);
      end else begin
        if (Mode <> dsFilesOnly) and (Search.Name <> '.') and (Search.Name <> '..') then
          List.Add(ASub + Search.Name);
      end;
      if R = 0 then
        R := FindNext(Search);
    end;
    FindClose(Search);
    if Recurse then begin
      APath := IncludeTrailingPathDelimiter(ExtractFilePath(APath));
      R := FindFirst(APath + '*', ScanAttr, Search);
      while (R = 0) do begin
        if (Search.Attr and faDirectory = faDirectory) and
        (Search.Name[1] <> '.') then begin // no self, parent or hidden
          R := ScanPath(
            IncludeTrailingPathDelimiter(ASub + Search.Name),
            APath + IncludeTrailingPathDelimiter(Search.Name) + '*');
        end;
        if R = 0 then
          R := FindNext(Search);
      end;
      FindClose(Search);
    end;
    Result := 0;
  end;

begin
  if not Assigned(List) then exit;
  List.Clear;
  if Mode = dsEverything then
    ScanAttr:=faAnyFile or MaxLongInt
  else
    ScanAttr:=faAnyFile;
  ScanPath('', APathspec);
end;

procedure DirScan(out List : TArrayOfStrings; APathSpec : String; Recurse : boolean = false;
  Mode : TDirScanMode = dsFilesOnly);  overload;
var
  C : Integer;
  ScanAttr : LongInt;

  procedure ListAdd(const S : String);
  begin
    if C = Length(List) then
      SetLength(List, Length(List) + 16);
    List[C] := S;
    Inc(C);
  end;

  function ScanPath(ASub, APath : String) : integer;
  var
     P : String;
     R : integer;
     Search : TSearchRec;
  begin
    P := ExtractFilePath(APath);
    if P <> '' then P:=IncludeTrailingPathDelimiter(P);
    R := FindFirst(APath, ScanAttr, Search);
    while (R = 0) do begin
      if (Search.Attr and faDirectory <> faDirectory) then begin
        if Mode <> dsDirsOnly then
          ListAdd(ASub + Search.Name);
      end else begin
        if (Mode <> dsFilesOnly) and (Search.Name <> '.') and (Search.Name <> '..') then
          ListAdd(ASub + Search.Name);
      end ;
      if R = 0 then
        R := FindNext(Search);
    end;
    FindClose(Search);
    if Recurse then begin
      APath := IncludeTrailingPathDelimiter(ExtractFilePath(APath));
      R := FindFirst(APath + '*', ScanAttr, Search);
      while (R = 0) do begin
        if (Search.Attr and faDirectory = faDirectory) and
        (Search.Name[1] <> '.') then begin // no self, parent or hidden
          R := ScanPath(
            IncludeTrailingPathDelimiter(ASub + Search.Name),
            APath + IncludeTrailingPathDelimiter(Search.Name) + '*');
        end;
        if R = 0 then
          R := FindNext(Search);
      end;
      FindClose(Search);
    end;
    Result := 0;
  end;

begin
  C := 0;
  List:=[];
  if Mode = dsEverything then
    ScanAttr:=faAnyFile or MaxLongInt
  else
    ScanAttr:=faAnyFile;
  ScanPath('', APathSpec);
  SetLength(List, C);
end;

procedure DirScanningByDate(var List : TStringList; APathSpec : String; Mode : TDirScanMode = dsFilesOnly);  overload;

const
  Digits = 12; // 12 Digits to store time value. 32-bit + about 5,000 years.

var
  R : integer;
  Search : TSearchRec;

begin
  List.Clear;
  List.Sorted:=True;
  R := FindFirst(APathSpec, faAnyFile, Search);
  while (R = 0) do begin
    if (Search.Attr and faDirectory <> faDirectory) then begin
      if Mode <> dsDirsOnly then
        List.Add(ZeroPad(Search.Time,Digits) + ' ' + Search.Name);
    end else begin
      if (Mode <> dsFilesOnly) and (Search.Name <> '.') and (Search.Name <> '..') then
        List.Add(ZeroPad(Search.Time,Digits) + ' ' + Search.Name);
    end;
    if R = 0 then
      R := FindNext(Search);
  end;
  FindClose(Search);
  List.Sorted:=False;
  for R := 0 to List.Count - 1 do
    List[R]:=Copy(List[R], Digits + 2);
end;

procedure DirScanByDate(var List : TStringList; APathSpec : String; Mode : TDirScanMode = dsFilesOnly);  overload;
begin
  DirScanningByDate(List,APathSpec,Mode);
end;

procedure DirScanByDate(out List : TArrayOfStrings; APathSpec : String; Mode : TDirScanMode = dsFilesOnly);  overload;
var
  I : integer;
  L : TStringList;
begin
  L := TStringList.Create;
  DirScanningByDate(L,APathSpec,Mode);
  List:=[];
  SetLength(List, L.Count);
  for I := 0 to High(List) do
    List[I] := L[I];
  L.Free;
end;

{$PUSH}{$I-}
function SaveToFile(AFileName: String; AValue: String; ARaise: boolean
  ): integer;
var
   T : Text;
   R, E : integer;
begin
  System.Assign(T, AFileName);
  Rewrite(T);
  R := IOResult;
  if R = 0 then begin
    WriteLn(T, AValue);
    R := IOResult;
    Close(T);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R;
  if (R <> 0) and ARaise then
    raise exception.Create('file save error ' + IntToStr(R));
end;

function SaveToFile(AFileName: String; AValue: TArrayOfBytes; ARaise: boolean
  ): integer;
var
   F : File;
   R, E : integer;
begin
  System.Assign(F, AFileName);
  Rewrite(F,1);
  R := IOResult;
  if R = 0 then begin
    BlockWrite(F, AValue[0], Length(AValue));
    R := IOResult;
    Close(F);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R;
  if (R <> 0) and ARaise then
    raise exception.Create('file save error ' + IntToStr(R));
end;

function AppendToFile(AFileName: String; AValue: String; ARaise: boolean
  ): integer;
var
   T : Text;
   R, E : integer;
begin
  System.Assign(T, AFileName);
  if FileExists(AFileName) then
    Append(T)
  else
    Rewrite(T);
  R := IOResult;
  if R = 0 then begin
    WriteLn(T, AValue);
    R := IOResult;
    Close(T);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R;
  if (R <> 0) and ARaise then
    raise exception.Create('file save error ' + IntToStr(R));
end;

function AppendToFile(AFileName: String; AValue: TArrayOfBytes; ARaise: boolean
  ): integer;
var
   F : File;
   R, E : integer;
begin
  System.Assign(F, AFileName);
  if FileExists(AFileName) then
    Reset(F,1)
  else
    Rewrite(F,1);
  R := IOResult;
  if R = 0 then begin
    Seek(F, FileSize(F));
    R := IOResult;
    if R = 0 then begin
      BlockWrite(F, AValue[0], Length(AValue));
      R := IOResult;
    end;
    Close(F);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R;
  if (R <> 0) and ARaise then
    raise exception.Create('file save error ' + IntToStr(R));
end;

function LoadFromFile(AFileName: String; out AValue: String; ARaise: boolean
  ): integer;
var
   T : Text;
   X, S : String;
   R, E : integer;
begin
  X := '';
  AValue := X;
  System.Assign(T, AFileName);
  Reset(T);
  R := IOResult;
  if R = 0 then begin
    while (R = 0) and (not EOF(T)) do begin
      ReadLn(T, S);
      R := IOResult;
      if (R = 0) then begin
        if (not EOF(T)) then
          S:=S + CRLF;
        X := X + S;
      end;
    end;
    Close(T);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R;
  if R = 0 then
    AValue := X
  else if ARaise then
    raise exception.Create('file load error ' + IntToStr(R));
end;

function LoadFromFile(AFileName: String; out AValue: TArrayOfBytes; ARaise: boolean
  ): integer;
var
   F : File;
   R, E : integer;
begin
  AValue:=[];
  System.Assign(F, AFileName);
  Reset(F, 1);
  R := IOResult;
  if R = 0 then begin
    if FileSize(F) >= MaxInteger then
      R := 8
    else begin
        SetLength(AValue, FileSize(F));
        BlockRead(F, AValue[0], Length(AValue));
    end;
    Close(F);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R;
  if R <> 0 then begin
    SetLength(AValue, 0);
    if ARaise then
      raise exception.Create('file load error ' + IntToStr(R));
  end;
end;

{$POP}

function StrToInts(const S : UTF8String) : string; overload;
var
  I : integer;
begin
  Result := '';
  for I := 1 to length(S) do begin
    Result := Result + IntToStr(Word(S[I]));
    if I < length(S) then Result := Result + ' ';
  end;
end;

function IntsToStr(S : String): UTF8String;  overload;
var
  C : String;
  V, E : integer;
begin
  Result := '';
  While S <> '' do begin
    C := PopDelim(S, SPACE);
    Val(C, V, E);
    if E <> 0 then begin
      Result := '';
      Break;
    end;
    Result := Result + char(V);
  end;
end;

function WordCase(S: String; Allowable : String): String;
var
  I : Integer;
  U : String;
  F : Boolean;
begin
  F := True;
  for I := 1 to Length(S) do begin
     U := UpperCase(S[I]);
     if F then S[I] := U[1];
     F := ((U < 'A') or (U > 'Z')) and (Pos(S[I], Allowable) = 0);
  end;
  WordCase := S;
end;

function WordWrap(S: String; Max: integer; Min: integer; Indent: integer;
  LineEndings: String): String;
var
  I : integer;
begin
  WordWrap := '';
  while S <> '' do begin
    if Length(S) <= Max then begin
      WordWrap := WordWrap + S;
      Break;
    end;
    I := Max;
    While I > Min do begin
      if (S[I] = SPACE) or (S[I] = TAB) then Break;
      Dec(I);
    end;
    if I <= Min then begin
      if Min = 0 then
        I := Max
      else begin
        for I := Max to Length(S) do
          if (S[I] = SPACE) or (S[I] = TAB) then Break;
        if I = Length(S) then begin
          WordWrap := WordWrap + S;
          Break;
        end;
      end;
    end;
    WordWrap := WordWrap + TrimRight(Copy(S, 1, I)) + LineEndings;
    S := Trim(Copy(S, I + 1));
    if Length(S) > 0 then S := LeftPad('', Indent) + S;
  end;
end;

function RandomStr(ALength : Integer; AChars : String = '') : String;
var
  R : integer;
begin
  RandomStr:='';
  if AChars = '' then
    AChars:='0123456789abcdefghijkmnpqrstuvwxyz';  // No L or O.
  while ALength > 0 do begin
    R:=Random(Length(AChars)) + 1;
    RandomStr:=RandomStr + AChars[R];
    Dec(ALength);
  end;
end;

function MkTempDir (Parent : String = ''): string;
var
  R : String;
begin
  MkTempDir:='';
  if Parent = '' then
    Parent:=GetTempDir;
  Parent:=IncludeTrailingPathDelimiter(Parent);
  if Not DirectoryExists(Parent) then
    if Not CreateDir(Parent) then Exit;
  repeat
    R:=Parent+'tmp-'+RandomStr(16);
  until Not DirectoryExists(R);
  if not CreateDir(R) then Exit;
  R:=IncludeTrailingPathDelimiter(R);
  if not Assigned(TempItems) then
    TempItems:=TStringlist.Create;
  TempItems.Add(R);
  MkTempDir:=R;
end;

procedure RmTempDir(ATempDir : String);
var
  I : integer;
begin
  if DeleteTree(ATempDir) then begin
    if Assigned(TempItems) then begin
      I:=TempItems.IndexOf(ATempDir);
      if I <> -1 then TempItems.Delete(I);
    end;
  end;
end;

function DeleteTree (APath: String) : boolean;
var
  S : TSearchRec;
  R : Integer;
begin
  DeleteTree:=False;
  if APath='' then exit;
  if FileExists(APath) then begin
    DeleteTree:=DeleteFile(APath);
    Exit;
  end;
  APath:=IncludeTrailingPathDelimiter(APath);
  // Using MaxLongInt for search attribute. Broken links do not show up
  // in the search with only faAnyfile. (At least on macOS)
  R:=FindFirst(APath + DirWildCard,faAnything, S);
  While R = 0 do begin
    if (S.Name <> '.') and (S.Name <> '..') then begin
      if IsLink(APath + S.Name) then begin
         if not DeleteFile(APath+S.Name) then exit;
      end else begin
        if S.Attr and faDirectory = faDirectory then begin
          if not DeleteTree(APath+S.Name) then exit;
        end else begin
          if not DeleteFile(APath+S.Name) then exit;
        end;
      end;
    end;
    R:= FindNext(S);
  end;
  FindClose(S);
  DeleteTree:=RemoveDir(APath);
end;

{$PUSH}
{$I-}
{$HINTS off}

function DirectoryIsEmpty(APath: String): boolean;
var
  R : Integer;
  S : TSearchRec;
begin
  R := FindFirst(IncludeTrailingpathDelimiter(APath) + DirWildCard, MaxLongInt or
    faAnyFile, S);
  if R <> 0 then begin
    DirectoryIsEmpty:=False;
  end else begin
    DirectoryIsEmpty:=True;
    while (R = 0) do begin
      if (S.Name <> '.') and (S.Name <> '..') then begin
        DirectoryIsEmpty:=False;
        Break;
      end;
      R:=FindNext(S);
    end;
  end;
  FindClose(S);
end;

function FileCopy(Source, Dest: String; Overwrite: boolean): integer;
const
  BufSize = 32*1024;
var
  B : array[1..BufSize] of byte;
  SF, DF : File;
  SC, DC : boolean;
  R, W : LongInt;
  DT : TDateTime;
begin
  FileCopy:=-1;
  if (not Overwrite) and FileExists(Dest) then Exit;
  SC:=False;
  DC:=False;
  try
    Assign(SF, Source);
    Reset(SF, 1);
    if IOResult<>0 then
      raise Exception.Create('could open source file');
    SC := True;
    Assign(DF, Dest);
    Rewrite(DF,1);
    if IOResult<>0 then
      raise Exception.Create('could open destinaton file');
    DC:=True;
    repeat
      BlockRead(SF, B, Sizeof(B), R);
      if IOResult <> 0 then
        raise Exception.Create('error reading source file');
      if R <> 0 then begin
        BlockWrite(DF, B, R, W);
        if (R<>W) or (IOResult <> 0) then
          raise Exception.Create('error writing destination file');
      end;
    until (R = 0);
    Close(SF);
    IOResult;
    SC:=False;
    Close(DF);
    IOResult;
    DC:=False;
    if not FileAge(Source, DT) then
      raise Exception.Create('error reading source timestamp');
    if FileSetDate(Dest, DateTimeToFileDate(DT)) <> 0 then
      raise Exception.Create('error writing destination timestamp');
    FileCopy:=0;
  except
    if SC then begin
      Close(SF);
      IOResult;
    end;
    if DC then begin
      Close(DF);
      IOResult;
    end;
    if FileExists(Dest) then
      DeleteFile(Dest);
  end;
end;

function SizeOfFile(FileName:String; out Size : Int64; FollowLinks : boolean=true) : boolean;
var
  R : Integer;
  S : TSearchRec;
begin
  SizeOfFile:=false;
  Size:=-1;
  if FollowLinks then begin
    if IsLink(FileName) then
      FileName:=LinkTarget(FileName, True);
  end;
  if FileName = '' then Exit;
  R := FindFirst(FileName, faAnything, S);
  if R <> 0 then Exit;
  FindClose(S);
  Size:=S.Size;
  SizeOfFile:=True;
end;

function FileCompare(First, Second: String): integer;
const
  BufSize = 32*1024;
var
  B1, B2 : array [1..BufSize] of byte;
  R1, R2 : LongInt;
  F1, F2 : File;
  O1, O2 : Boolean;
  A1, A2 : LongInt;
  S1, S2 : Int64;
  DT : TDateTime;
  Match : Boolean;
  I : integer;

begin
  FileCompare:=-1;
  if not (FileExists(First) and FileExists(Second)) then Exit;
  if not FileAge(First, DT, True) then Exit;
  A1:=DateTimeToFileDate(DT);
  if not FileAge(Second, DT, True) then Exit;
  A2:=DateTimeToFileDate(DT);
  if not SizeOfFile(First, S1, True) then Exit;
  if not SizeOfFile(Second, S2, True) then Exit;
  if S1 <> S2 then begin
    Match:=False;
  end else begin
    FileCompare:=0;
    Match:=True;
    O1:=False;
    O2:=False;
    try
      Assign(F1, First);
      Reset(F1,1);
      if IOResult <> 0 then
        Raise Exception.Create('error opening file ' + First);
      O1:=True;
      Assign(F2, Second);
      Reset(F2,1);
      if IOResult <> 0 then
        Raise Exception.Create('error opening file ' + Second);
      O2:=True;
      repeat
        BlockRead(F1, B1, Sizeof(B1), R1);
        if IOResult <> 0 then
          raise Exception.Create('error reading file ' + First);
        BlockRead(F2, B2, Sizeof(B2), R2);
        if IOResult <> 0 then
          raise Exception.Create('error reading file ' + Second);
        if R1 <> R2 then // should never happen.
          raise Exception.Create('file read mismatch');
        for I := 1 to R1 do
          if B1[I] <> B2[I] then begin
             Match:=False;
             Break;
          end;
      until (R1 = 0) or (Not Match);
    except
      FileCompare:=-1;
    end;
    try
      if O2 then Close(F2);
    finally
      if O1 then Close(F1);
    end;
  end;
  if not Match then begin
    if A1 >= A2 then
      FileCompare:=1
    else
      FileCompare:=2;
  end;
end;

function FileCRC32(FileName : String) : LongInt;
const
  BufSize = 32*1024;
var
  B : array[0..BufSize-1] of byte;
  F : File;
  C : LongInt;
begin
   FileCRC32 := crc.crc32(0, nil, 0);
   {$I-}
   Assign(F, FileName);
   Reset(F, 1);
   if IOResult = 0 then begin
       C := SizeOf(B);
       while (C = SizeOf(B)) do begin
         BlockRead(F, B, SizeOf(B), C);
         if IOResult <> 0 then begin
           FileCRC32:=0;
           Break;
         end;
         if C > 0 then begin
            FileCRC32 := crc.crc32(FileCRC32, B, C);
         end;
       end;
       Close(F);
   end else
       FileCRC32 := 0;
   {$I+}
end;
{$POP}

function FileMove(Source, Dest: String; Overwrite: boolean): integer;
begin
  FileMove:=-1;
  if (not Overwrite) and FileExists(Dest) then Exit;
  if not RenameFile(Source, Dest) then begin
    if FileCopy(Source, Dest, true) <> 0 then Exit;
    if not DeleteFile(Source) then begin
       DeleteFile(Dest);
       Exit;
    end;
  end;
  FileMove:=0;
end;

function PathStamp(Path : String) : LongInt;
var
  R : integer;
  P : LongInt;
  S : TSearchRec;
begin
  PathStamp:= -1;
  Path:=IncludeTrailingPathDelimiter(Path);
  R := FindFirst(Path + DirWildCard,faAnyFile, S);
  while R = 0 do begin
    if (S.Name = '.') or (S.Name = '..') or IsLink(Path + S.Name) then begin
      { Ignored }
    end else if S.Attr and faDirectory = faDirectory then begin
      P:=PathStamp(Path + S.Name);
    end else begin
      P:=DateTimeToFileDate(S.TimeStamp);
      // WriteLn(ZeroPad(P,14), ' ', Path + S.Name);
    end;
    if P > PathStamp then PathStamp:=P;
    R :=FindNext(S);
  end;
  FindClose(S);
  if P <> -1 then begin
    PathStamp:=P;
    FileSetDate(ExcludeTrailingPathDelimiter(Path), P);
    // Writeln(Tab, DateTimeToStr(FileDateToDateTime(P)), ' - ', Path);
  end;
end;



initialization

  InitUnit;

finalization

  DoneUnit(False);

end.
