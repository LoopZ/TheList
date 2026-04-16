// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit CfgOpts;

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
  Version, PasExt, GloData;

{ Display Program Banner and Version }
procedure Banner;

{ Process Command Line Options }
procedure Options;

{ Display the "See Help" Message and terminate with an error code. }
procedure SeeHelp(Message : String);

{ Display program help and terminate. }
procedure Help;


implementation

procedure Banner;
begin
  LogMessage(vbNormal,APP_PRODUCTNAME + ' v' + APP_VERSION);
  LogMessage(vbNormal,'Copyright ' + APP_LEGALCOPYRIGHT);
  LogMessage(vbNormal,'The Clear BSD License ');
  LogMessage(vbNormal, '');
end;

procedure Help;

const
  Switches : array of record
    S, L, V, M : String;
  end = (
    (S:''; L:'--version'; V:''; M:'Display version information and exit.'),
    (S:'-h'; L:'--help'; V:''; M:'Display command-line help and exit.'),
    (S:''; L:''; V:''; M:''),
    (S:'-v'; L:'--verbose'; V:'(level)'; M:'Specify a verbosity level (0-4).'),
    (S:''; L:''; V:''; M:''),
    (S:'-s'; L:'--source'; V:'(path)'; M:'Specify a path that contains "The List" files.'),
    (S:'-o'; L:'--output'; V:'(path)'; M:'Specify a path to store the HTML files.'),
    (S:''; L:''; V:''; M:''),
    (S:'-s'; L:'--cicd'; V:'(path)'; M:'For use with CI/CD pipelines and will fails on errors.')
  );

var
  I, J, WA, WB : Integer;
  S : String;
begin
  WriteLn('usage: ', ExtractFileName(ParamStr(0)), ' [option]');
  WriteLn;
  WA:=0;
  WB:=0;
  for I := 0 to High(Switches) do begin
    J:=Length(Switches[I].S);
    if Length(Switches[I].L) > 0 then Inc(J, Length(Switches[I].L) + 3);
    if WA < J then WA:=J;
    if Length(Switches[I].V) >= WB then WB:=Length(Switches[I].V) + 1;
  end;
  for I := 0 to High(Switches) do begin
    S:=RightPad(Switches[I].S, 2);
    if Switches[I].L <> '' then
      Cat(S, WhenTrue(Trim(S) <> '', ', ', SPACE2) + Switches[I].L);
    if S = '' then begin
      WriteLn;
      if Switches[I].M <> '' then begin
        WriteLn(Switches[I].M);
        WriteLn;
      end;
      continue;
    end else
      S:=RightPad(S, WA) + RightPad(Switches[I].V, WB) + Switches[I].M;
    WriteLn(TrimRight(S));
  end;
  WriteLn;
  Halt(0);
end;

procedure SeeHelp(Message : String);
begin
  LogMessage(vbCritical, Message);
  LogMessage(vbCritical, 'see: ' + ExtractFileName(ParamStr(0)) + ' --help');
  Halt(1);
end;


procedure Options;
var
  I : Integer;
  Opt : String;

  function NextOpt(Required : boolean = true) : string;
  begin
    if I > ParamCount then
      SeeHelp('Missing parameter for command-line option: ' + Opt);
    if Required and (Trim(ParamStr(I)) = '') then
      SeeHelp('Cannot provide blank parameter for command-line option: ' + Opt);
    Result:=ParamStr(I);
    Inc(I);
  end;

var
  V, E : Integer;

begin
  I:=1;
  While I <= ParamCount do begin
    Opt:=NextOpt;
    case Opt of
      '--version' : begin
        Banner;
        Halt(0);
      end;
      '-h', '--help' : begin
        Help;
      end;
      '-v', '--verbose' : begin
        Val(NextOpt, V, E);
        if (E<>0) then V:=-1;
        case V of
          0 : VerboseLevel:=vbCritical;
          1 : VerboseLevel:=vbMinimal;
          2 : VerboseLevel:=vbNormal;
          3 : VerboseLevel:=vbVerbose;
          4 : VerboseLevel:=vbExcessive;
        else
          SeeHelp('Invalid verbosity level: ' + IntToStr(V));
        end;
      end;
      '-s', '--source' : Source:=IncludeTrailingPathDelimiter(Trim(NextOpt));
      '-o', '--output' : Output:=IncludeTrailingPathDelimiter(Trim(NextOpt));
      '--cicd' : CICD := True;
    else
      SeeHelp('Invalid command-line option: ' + Opt);
    end;
  end;
end;

end.
