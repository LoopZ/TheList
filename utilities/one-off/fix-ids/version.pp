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
  APP_VERSION: String = '0.0.0';
  APP_BUILD: String = '0';
  APP_TITLE: String = 'fixflags';
  APP_YEAR: String = '';

implementation

end.
