// Copyright 2025-2026, Jerome Shidel
// BSD 3-Clause License

program makelist;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Version information defines

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils
  { you can add units after this },
  Version, PasExt;

{$R *.res}

procedure Banner;
begin
  { Program Information Banner }
  WriteLn(APP_PRODUCTNAME, ' v', APP_VERSION, ' (build ', APP_BUILD, ')');
  WriteLn('Copyright ', APP_LEGALCOPYRIGHT);
  WriteLn('BSD 3-Clause License');
  WriteLn;
end;

begin
  Banner;
end.

