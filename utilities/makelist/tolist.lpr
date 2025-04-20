// Copyright 2025, Jerome Shidel
// BSD 3-Clause License

program tolist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils
  { you can add units after this },
  PasExt;

{$R *.res}

procedure Banner;
begin
  { Program Information Banner }
  WriteLn(APP_PRODUCTNAME, ' v', APP_VERSION, ' (build ', APP_BUILD, ')');
  WriteLn('Copyright ', APP_LEGALCOPYRIGHT);
  WriteLn(APP_PRODUCTLICENSE);
  WriteLn;
end;

begin
  Banner;
end.

