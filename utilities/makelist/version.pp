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
  SOURCE_VERSION = '1.0.0';
  SOURCE_REVISION = '15';
  SOURCE_URL = 'http://github.com/LoopZ/TheList';
  SOURCE_COMMIT = '1a9df9378f574ce36f9a400525ca1f36ab4c1355';

  { Version Build Atributes } 
  BUILD_DEBUG: Boolean = False;
  BUILD_PRERELEASE: Boolean = True;
  BUILD_PATCHED: Boolean = False;
  BUILD_PRIVATE: Boolean = False;
  BUILD_SPECIAL: Boolean = False;
  BUILD_DATE: String = '2026-02-10 07:58:10';

  { General Application Information }
  APP_VERSION: String = '1.0.0';
  APP_BUILD: String = '0';
  APP_TITLE: String = 'fromRBIL';
  APP_LEGALCOPYRIGHT: String = '(c) 2025-2026 Jerome Shidel';
  APP_PRODUCTNAME: String = 'RBIL to Source Files Convertor';
  APP_PRODUCTVERSION: String = '1.0';
  APP_YEAR: String = '2026';

implementation

end.
