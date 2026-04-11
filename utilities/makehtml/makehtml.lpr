// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

program makehtml;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Version information defines

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, IniFiles, StrUtils,
  { you can add units after this }
  Version, PasExt, BinTree, CfgOpts;

type
  TListType = (lfUnknown, lfExclude, lfSubPart);

var
  ListType : TListType;

procedure ProcessFile(Filename : String);
var
  Ext : String;
begin
  Ext:=UpperCase(ExtractFileExt(FileName));
  ListType := lfUnknown;
  if (Length(Ext) = 2) and ((Ext >= '.B') and (Ext <= '.Z')) then
    ListType:=lfSubPart
  else if (Length(Ext) = 3) and ((Ext >= '.ZA') and (Ext <= '.ZZ')) then
    ListType:=lfSubPart
  else if (Length(Ext) = 4) and ((Ext >= '.ZZA') and (Ext <= '.ZZZ')) then
    ListType:=lfSubPart
  else case UpperCase(ExtractFileBase(FileName)) of
    'CATEGORY' : if Ext = '.KEY' then ListType:=lfExclude;
    'LICENSE'  : if Ext = ''     then ListType:=lfExclude;
    'README'   : if Ext = '.NOW' then ListType:=lfExclude;
    'BIBLIO',
    'CMOS',
    'FAQ',
    'FARCALL',
    'GLOSSARY',
    'I2C',
    'LINKS',
    'MEMORY',
    'MSR',
    'OVERVIEW',
    'PORTS',
    'SMM',
    'TABLES'   : if (Ext = '.LST') or (Ext = '.A') then ListType:=lfExclude;
    'INTERRUP' : case Ext of
      '.PRI' : ListType:=lfExclude;
      '.1ST' : ListType:=lfExclude;
      '.LST',
      '.A'   : ListType:=lfExclude;
    end;
  end;
  if ListType = lfExclude then begin
    LogMessage(vbExcessive, 'Skip file: ' + Filename);
    Exit;
  end;
  if ListType = lfUnknown then begin
    LogMessage(vbNormal, 'Unrecognized file: ' + Filename);
    Exit;
  end;
end;

procedure Conversion;
var
  I : Integer;
  L : TArrayOfRawByteString;
begin
  if not DirectoryExists(Source) then begin
    LogMessage(vbCritical, 'Unable to locate The List files at: ' + Source);
    Halt(1);
  end;
  if DirectoryExists(Output) then
    LogMessage(vbNormal, 'HTML output directory already exists: ' + Output)
  else begin
    if not CreateTree(Output) then begin
      LogMessage(vbCritical, 'Unable to create HTML output directory: ' + Output);
      Halt(1);
    end;
    LogMessage(vbNormal, 'Created HTML output directory: ' + Output)
  end;
  DirScan(Source + Wildcard, L, [dsFiles]);
  for I := 0 to High(L) do
    ProcessFile(L[I]);
  LogMessage(vbNormal, 'Conversion finished.');
end;

begin
  Options;
  Banner;
  Conversion;
end.

