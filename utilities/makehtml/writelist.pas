// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit WriteList;

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
  Version, PasExt, BinTree, GloData;

procedure WriteTheList(Pathname : String);

implementation

procedure WriteGroup(Pathname : String; List : integer);
begin
  LogMessage(vbNormal, 'output group: ' + ListFiles[List].Group);
end;

procedure WriteTheList(Pathname: String);
var
  I : integer;
begin
  if DirectoryExists(Pathname) then begin
    LogMessage(vbNormal, 'HTML output directory already exists: ' + Pathname);
    if not DeleteTree(Pathname) then begin
      LogMessage(vbCritical, 'Unable to remove previous HTML output directory: ' + Pathname);
      Halt(1);
    end else
      LogMessage(vbNormal, 'Removed previous HTML output directory: ' + Pathname);
  end;
  if not CreateTree(Pathname) then begin
    LogMessage(vbCritical, 'Unable to create HTML output directory: ' + Pathname);
    Halt(1);
  end else
    LogMessage(vbNormal, 'Created HTML output directory: ' + Pathname);
  for I := Low(ListFiles) to High(ListFiles) do begin
      WriteGroup(PathName, I);
  end;
end;

end.
