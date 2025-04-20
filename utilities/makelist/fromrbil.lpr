// Copyright 2025, Jerome Shidel
// BSD 3-Clause License

program fromrbil;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils
  { you can add units after this },
  PasExt;

{$R *.res}

const
  SRC='../../rbil/';
  DST='../../source/';

procedure Banner;
begin
  { Program Information Banner }
  WriteLn(APP_PRODUCTNAME, ' v', APP_VERSION);
  WriteLn('Copyright ', APP_LEGALCOPYRIGHT);
  WriteLn(APP_PRODUCTLICENSE);
  WriteLn;
end;

procedure Convert;
begin
  if not DirectoryExists(SRC) then begin
    WriteLn('Directory "', SRC, '" with original RBIL files was not found.');
    Halt(1);
  end;
  if DirectoryExists(DST) then begin
    WriteLn('Delete current "TheList" sources directory.');
    if not DeleteTree(DST) then begin
      WriteLn('Failed to remove directory.');
      Halt(1);
    end;
  end;
  WriteLn('Create new "TheList" sources directory.');
  if not CreateDir(DST) then begin
    WriteLn('Failed to create directory.');
    Halt(1);
  end;
end;

begin
  Banner;
  Convert;
end.

