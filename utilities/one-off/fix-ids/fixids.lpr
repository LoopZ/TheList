// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

program fixids;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils
  { you can add units after this },
  PasExt;

type
  TSectionStyle = (ssNormal, ssGlossary, ssInterrupts, ssTables, ssSMM, ssMSR,
  ssI2C, ssMemory, ssCMOS, ssFarCall, ssPorts);

const
  SRCDIR='../../../source/';

var
  BAR : String;
  GROUP : String;
  COUNT: integer;
  SectionStyle : TSectionStyle;

function CompressWhitespace(S : RawByteString) : RawByteString;
begin
 Result:=Trim(StringReplace(S, TAB, SPACE, [rfReplaceAll]));
 repeat
   S:=Result;
   Result:=StringReplace(S, SPACE+SPACE, SPACE, [rfReplaceAll]);
 until Result=S;
end;

function GenerateId(Data : String) : String;
begin
 Result:='';
end;

procedure ProcessFile(FileName : String);
var
  Orig, Work, Dest, Title, S : RawByteString;
  Needed : Boolean;
  A, B : TArrayOfRawByteString;
  I : Integer;
  DT : TDateTime;
  NewID, OldID : String;
begin
  if FileLoad(Filename, Orig) <> 0 then begin
    WriteLn('Error loading file: ' + FileName);
    Halt(1);
  end;
  if not FileAge(FileName, DT) then DT:=-1;
  Orig:=NormalizeLineEndings(Orig, CRLF);
  Work:=Orig;
  NewID:= '';
  OldId:='';
  // Search to start of header. "Should be" the first line, but just in case...
  repeat
    S := PopDelim(Work, CRLF);
    if Copy(S, 1, 75) = Copy(BAR, 1, 75) then Break;
    if Work = '' then begin
     // File has no header.
      Exit;
    end;
  until false;
  Dest:='';
  // Parse current header
  repeat
    S := PopDelim(Work, CRLF);
    if HasLeading(Trim(S), 'Unique ID:', false) then begin
      PopDelim(S, COLON);
      OldId:=CompressWhitespace(S);
      Continue;
    end;
    Cat(Dest, S + CRLF);
    if Copy(S, 1, 75) = Copy(BAR, 1, 75) then Break;
    if Work = '' then begin
      WriteLn('File header not terminated: ' + Filename);
      Exit;
    end;
  until false;
  // Get Entry Title
  repeat
     S := Trim(PopDelim(Work, CRLF));
  until (S <> '') or (Work = '');
  if S = '' then begin
    WriteLn('Entry Title not found: ' + FileName);
    Exit;
  end;
  Title:=CompressWhiteSpace(S);

  // File roughly validated as an entry, display Group Once
  if GROUP <> '' then begin
    WriteLn(GROUP);
    GROUP:='';
  end;

  NewID:=GenerateID(Title + CRLF + Work);

  // Create entry with updated header
  Dest:=BAR+CRLF + LeftPad('UniqueID:', 11) + SPACE + NewID + CRLF + Dest;
  Cat(Dest, CRLF + Title + CRLF + Work); // Copy body into Dest

  // If no change, we are done.
  if Dest = Orig then Exit;
  // Test for significant changes
  A:=Explode(Orig, CRLF);
  B:=Explode(Dest, CRLF);
  if High(A) = High(B) then begin
    Needed:=False;
    for I := 0 to High(A) do
      if CompressWhiteSpace(A[I]) <> CompressWhiteSpace(B[I]) then begin
        Needed:=True;
        Break;
      end;
    if not Needed then Exit;
  end;
  // WriteLn(TAB + FileName);
  Inc(COUNT);
  exit;
  WriteLn(StringOf('*', 80));
  WriteLn(OldID, ' -> ', NewID);
  WriteLn(NormalizeLineEndings(Orig));
  WriteLn(NormalizeLineEndings(Dest));
  if count > 3 then Halt;
  exit;
  if FileSave(Filename, Dest) <> 0 then begin
    WriteLn('Error saving file: ' + FileName);
    Halt(1);
  end;
  if DT <> -1 then FileSetDate(Filename, DateTimeToFileDate(DT));
end;

procedure ProcessGroup(G : String);
var
  I : Integer;
  L : TStringList;
begin
  case G of
    'Interrupt List'               : SectionStyle:=ssInterrupts;
  (*
    'Model-Specific Registers'     : SectionStyle:=ssMSR;
    'Bus Devices I2C'              : SectionStyle:=ssI2C;
    'Memory Map'                   : SectionStyle:=ssMemory;
    'Cmos-Memory Map'              : SectionStyle:=ssCMOS;
    'Far Call Interface'           : SectionStyle:=ssFarCall;
    'Ports List'                   : SectionStyle:=ssPorts;
  *)
  else
    Exit;
  end;
  L := TStringList.Create;
  GROUP:='Working: ' + G;
  WriteLn(G);
  DirScan(SRCDIR + G + PathDelimiter + WildCard, L, [dsFiles, dsRecursive]);
  for I := 0 to L.Count - 1 do
    if (not HasLeading(L[I], UNDERSCORE)) and (HasTrailing(L[I], '.txt', false)) then
      ProcessFile(SRCDIR + G + PathDelimiter + L[I]);
  L.Free;
end;

procedure ProcessAll;
var
  I : Integer;
  G : TArrayOfString;
begin
  COUNT:=0;
  DirScan(SRCDIR + Wildcard, G, [dsDirectories]);
  for I := 0 to High(G) do
    if LowerCase(G[I]) <> 'miscellaneous' then
      ProcessGroup(G[I]);
  WriteLn(COUNT, ' files updated');
end;

begin
  BAR:=StringOf('-', 80);
  ProcessAll;
end.


