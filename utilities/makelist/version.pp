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
  { General Application Information }
  APP_VERSION: String = '1.1.5';
  APP_BUILD: String = '0';
  APP_TITLE: String = 'makelist';
  APP_LEGALCOPYRIGHT: String = '(c) 2025-2026 Jerome Shidel';
  APP_PRODUCTNAME: String = 'The List Release File Assembler';
  APP_PRODUCTVERSION: String = '1.0';
  APP_YEAR: String = '';

implementation

end.
