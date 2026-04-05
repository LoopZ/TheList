{ Application Version Information File                                 }
{                                                                      }
{ This file is created automatically by the version.sh script whenever }
{ the project is built by Lazarus. Manual changes will be lost.        }

unit Version;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.

interface

{$IFDEF USES_CWString}
  uses cwstring;
{$ENDIF}

const
  { The default Free Pascal Compiler }
  FPC_VERSION = '3.2.2';
  FPC_PLATFORM = 'darwin';
  FPC_TARGET = 'x86_64';

  { The Lazarus I.D.E }
  LAZARUS_VERSION = '4.0';

  { Source version and most recent project commit }
  SOURCE_VERSION = '1.1.2';
  SOURCE_REVISION = '4';
  SOURCE_URL = 'http://github.com/LoopZ/TheList';
  SOURCE_COMMIT = '095f776cbd4f443d02e36cfc8e39ee29de6e2abf';

  { Version Build Atributes } 
  BUILD_DEBUG: Boolean = False;
  BUILD_PRERELEASE: Boolean = False;
  BUILD_PATCHED: Boolean = False;
  BUILD_PRIVATE: Boolean = False;
  BUILD_SPECIAL: Boolean = False;
  BUILD_DATE: String = '2026-04-05 08:51:41';

  { General Application Information }
  APP_VERSION: String = '1.1.2';
  APP_BUILD: String = '0';
  APP_TITLE: String = 'MakeList';
  APP_LEGALCOPYRIGHT: String = '(c) 2025-2026 Jerome Shidel';
  APP_PRODUCTNAME: String = 'The List Release File Assembler';
  APP_PRODUCTVERSION: String = '1.0';
  APP_YEAR: String = '2026';

implementation

end.
