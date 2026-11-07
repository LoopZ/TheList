// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit GloData;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Version information defines

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  { you can add units after this }
  Version, PasExt, BinTree;

type
  // Types of entries in a LIST file
  TListType = (lfUnknown, lfExclude, lfSubPart, lfList, lfLinks, lfFAQ, lfSMM,
    lfBiblio, lfGlossary, lfTables, lfOverview, lfCategories, lfTextFile);

  // Record for information on open LIST file
  TListFile =record
    Kind     : TListType;
    Name     : String;
    Group    : String;
    Header   : String;
    TextData : String;
    Comments : TBinaryTree;
    Entries  : TBinaryTree;
  end;

  // Array type for information LIST files
  TListFiles = array of TListFile;

var
  // Information about open LIST files;
  ListFiles : TListFiles;
  // Total count of Critical Errors encountered while processing The List
  CriticalErrors : integer;

const
  {$IFDEF Windows}
  Source : String = '..\..\TheList\';
  Output : String = '..\..\html\';
  {$ELSE}
  Source : String = '../../TheList/';
  Output : String = '../../html/';
  {$ENDIF}

  CICD : boolean = false;

function KeyToVal(S : String) : int64;

function ValToKey(V : Int64) : String;

implementation

function KeyToVal(S : String) : int64;
var
  I : integer;
begin
  Result:=0;
  I := 0;
  While Length(S) > I do begin
    Inc(I);
    Result:=Result shl 8 + Ord(S[I]);
  end;
end;

function ValToKey(V : Int64) : String;
begin
  Result := '';
  While V > 0 do begin
    Result := Chr(V and $ff) + Result;
    V:= V shr 8;
  end;
end;

initialization

  ListFiles:=[];
  CriticalErrors:=0;

finalization

end.
