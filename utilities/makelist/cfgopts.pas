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
  Version, PasExt, GloDat;

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
    (S:'-s'; L:'--source'; V:'(path)'; M:'Specify a path that contains the source files.'),
    (S:'-o'; L:'--output'; V:'(path)'; M:'Specify a path to store the output files.'),
    (S:'-m'; L:'--maximum'; V:'(size)'; M:'Split List files at a specific Kb size.'),
    (S:SPACE; L:''; V:''; M:'Default is 360Kb. Use none to turn off file spliting.'),
    (S:''; L:''; V:''; M:''),
    (S:''; L:'--modern'; V:''; M:'Disable legacy mode for modern parsers (default).'),
    (S:''; L:'--legacy'; V:''; M:'Enable compatibility mode for legacy parsers.'),
    (S:''; L:''; V:''; M:''),
    (S:''; L:'--cicd'; V:''; M:'More strict verification for usage with CI/CD.'),
    (S:''; L:'--issues'; V:'(file)'; M:'Write a list of the files with issues to a file.')
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
  V, E, U, M : Integer;
  S : String;

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
        Halt(0);
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
      '--cicd' : CICD:=True;
      '--modern' : LegacyMode:=False;
      '--legacy' : LegacyMode:=True;
      '--issues' : IssueFileName:=Trim(NextOpt);
      '-s', '--source' : DirSource:=IncludeTrailingPathDelimiter(Trim(NextOpt));
      '-o', '--output' : DirOutput:=IncludeTrailingPathDelimiter(Trim(NextOpt));
      '-m', '--maximum' : begin
        S:=UpperCase(NextOpt);
        if UpperCase(S) = 'NONE' then begin
          MaxFileSize:=-1;
          Continue;
        end;
        U:=1024;
        if HasTrailing(S, 'K') then
          S:=ExcludeTrailing(S, 'K')
        else if HasTrailing(S, 'KB') then
          S:=ExcludeTrailing(S, 'KB')
        else if HasTrailing(S, 'M') then begin
          S:=ExcludeTrailing(S, 'M');
          U:=1024*1024;
        end else if HasTrailing(S, 'MB') then begin
          S:=ExcludeTrailing(S, 'MB');
          U:=1024*1024;
        end;
        Val(S, M, E);
        MaxFileSize:=U * M;
        if (E <> 0) or (M < 1) or (MaxFileSize < 120 * 1024) then
           SeeHelp('Invalid parameter for command-line option: ' + Opt);
      end
      { TODO 1 -cDevel Add Option to create ZIP Archive version of release }
    else
      SeeHelp('Invalid command-line option: ' + Opt);
    end;
  end;
end;

end.
