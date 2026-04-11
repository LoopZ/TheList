// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit SrcData;

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
  Version, PasExt;

type
  TListType = (lfUnknown, lfExclude, lfSubPart, lfList);

var
  ListType : TListType;

procedure ProcessList(FileName : String);
procedure ProcessFile(Filename : String);

implementation

procedure ProcessList(FileName : String);
begin
  WriteLn(FileName);
end;

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
      '.A'   : ListType:=lfList;
    end;
  end;
  case ListType of
    lfExclude : begin
      LogMessage(vbExcessive, 'Skip file: ' + Filename);
      Exit;
    end;
    lfUnknown : begin
      LogMessage(vbNormal, 'Unrecognized file: ' + Filename);
      Exit;
    end;
    lfSubPart : Exit;
    lfList : ProcessList(Filename);
  end;
end;


initialization

finalization

end.
