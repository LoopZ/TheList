// Copyright 2025-2026, Jerome Shidel
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
  Classes, SysUtils, IniFiles
  { you can add units after this },
  Version, PasExt, BinTree, StrUtils;

{$R *.res}

begin

end.

