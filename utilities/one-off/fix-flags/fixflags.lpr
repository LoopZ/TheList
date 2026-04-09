// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

program fixflags;

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

const
  SRCDIR='../../../source/';

var
  BAR : String;
  GROUP : String;
  COUNT: integer;

const
  Flags : array of record
    Flag : String;
    Text : String;
  end = (
    (Flag:'U'; Text:'undocumented'),
    (Flag:'u'; Text:'partially documented'),
    (Flag:'P'; Text:'available only in protected mode'),
    (Flag:'R'; Text:'available only in real or V86 mode '),
    (Flag:'C'; Text:'callout or callback (usually hooked rather than called)'),
    (Flag:'O'; Text:'obsolete (no longer present in current versions)')
  );


type
  THeadField = record
    Key, Value, Text : String;
  end;
  THeadFields = array of THeadField;

function CompressWhitespace(S : RawByteString) : RawByteString;
begin
 Result:=Trim(StringReplace(S, TAB, SPACE, [rfReplaceAll]));
 repeat
   S:=Result;
   Result:=StringReplace(S, SPACE+SPACE, SPACE, [rfReplaceAll]);
 until Result=S;
end;

function GetFlag(Flag : String) : String;
var
  I : Integer;
begin
  Result:='';
  for I := 0 to High(Flags) do
    if Flag=Flags[I].Flag then begin
      Result:=Flags[I].Text;
      Break;
    end;
end;

procedure ProcessFile(FileName : String);
var
  Orig, Work, Dest, Title, S, K, V : RawByteString;
  Head : THeadFields;
  A, B : TArrayOfRawByteString;
  Needed : Boolean;
  I : Integer;
  DT : TDateTime;

begin
  if FileLoad(Filename, Orig) <> 0 then begin
    WriteLn('Error loading file: ' + FileName);
    Halt(1);
  end;
  if not FileAge(FileName, DT) then DT:=-1;
  Orig:=NormalizeLineEndings(Orig, CRLF);
  Work:=Orig;
  // Search to start of header. "Should be" the first line, but just in case...
  repeat
    S := PopDelim(Work, CRLF);
    if Copy(S, 1, 75) = Copy(BAR, 1, 75) then Break;
    if Work = '' then begin
     // File has no header.
      Exit;
    end;
  until false;
  Head:=[];
  // Parse current header
  repeat
    S := PopDelim(Work, CRLF);
    if Copy(S, 1, 75) = Copy(BAR, 1, 75) then Break;
    if Work = '' then begin
      WriteLn('File header not terminated: ' + Filename);
      Exit;
    end;
    S:=CompressWhitespace(S);
    if S = '' then Continue;
    K:= PopDelim(S, COLON);
    S:=Trim(S);
    V:= PopDelim(S, SPACE);
    S:=Trim(S);
    Case Lowercase(K) of
       'unique id' : begin
         SetLength(Head, Length(Head)+1);
         Head[High(Head)].Key:='Unique ID';
         Head[High(Head)].Value:=V;
         Head[High(Head)].Text:=S;
       end;
       'category' : begin
         SetLength(Head, Length(Head)+1);
         Head[High(Head)].Key:='Category';
         Head[High(Head)].Value:=V;
         Head[High(Head)].Text:=S;
       end;
       'flag' : begin
         // Ignore old flags
         (*
         SetLength(Head, Length(Head)+1);
         Head[High(Head)].Key:=K;
         Head[High(Head)].Value:=V;
         Head[High(Head)].Text:=S;
         *)
       end;
    else
      WriteLn('Unknown header field "' + K + '" in: ' + Filename);
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
  Title:=S;
  // File roughly validated as an entry, display Group Once
  if GROUP <> '' then begin
    WriteLn(GROUP);
    GROUP:='';
  end;
  // Parse the Title Flags
  S:=CompressWhiteSpace(S);
  S:=PopDelim(S, '-');
  V:=Trim(CutDelim(S, SPACE, 3));
  if Length(V) = 0 then begin
    SetLength(Head, Length(Head)+1);
    Head[High(Head)].Key:='Flag';
    Head[High(Head)].Value:='n/a';
    Head[High(Head)].Text:='';
  end else begin
    for I := 1 to Length(V) do begin
      SetLength(Head, Length(Head)+1);
      Head[High(Head)].Key:='Flag';
      Head[High(Head)].Value:=V[I];
      S:=GetFlag(V[I]);
      if S = '' then begin
        WriteLn('Unknown flag "' + V[I] + '" in file: ' + FileName);
        Exit;
      end;
      Head[High(Head)].Text:=S;
    end;
  end;

  // Create entry with updated header
  Dest:=BAR + CRLF;
  for I := 0 to High(Head) do
    Cat(Dest, TrimRight(
      LeftPad(Head[I].Key, 10) + ':' + SPACE +
      RightPad(Head[I].Value, 3) + SPACE +
      Head[I].Text ) +
      CRLF);
  Cat(Dest, BAR + CRLF + CRLF + Title + CRLF + Work); // Copy body into Dest


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
  WriteLn(TAB + FileName);
  Inc(COUNT);
  // exit;
  if FileLoad(Filename, Dest) <> 0 then begin
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
  L := TStringList.Create;
  GROUP:='Working: ' + G;
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

