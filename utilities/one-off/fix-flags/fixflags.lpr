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

begin

end.

