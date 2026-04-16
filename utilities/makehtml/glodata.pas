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
    lfBiblio, lfGlossary, lfTables, lfOverview);

  // Record for information on open LIST file
  TListFile =record
    Kind     : TListType;
    Name     : String;
    Header   : String;
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

implementation


initialization

  ListFiles:=[];
  CriticalErrors:=0;

finalization

end.
