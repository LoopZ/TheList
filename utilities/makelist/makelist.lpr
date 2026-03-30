// Copyright 2025-2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

program makelist;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Version information defines

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils
  { you can add units after this },
  Version, PasExt, BinTree;

const
  {$IFDEF Windows}
  DirSource : String = '..\..\source\';
  DirOutput : String = '..\..\TheList\';
  {$ELSE}
  DirSource : String = '../../source/';
  DirOutput : String = '../../TheList/';
  {$ENDIF}
  MaxFileSize : integer = 360 * 1024; // Maximum bytes allowed in list file

  DOSNAMES : array of record // Conversion of Long File Names to DOS versions
    Name, DOS : String;
  end = (
    // Miscellaneous files
    (Name:'Advertisement.txt';         DOS:'_ADVERT.TXT'),
    (Name:'FAQ.txt';                   DOS:'faq.lst'),
    (Name:'Interrupt Primer.txt';      DOS:'INTERRUP.PRI'),
    (Name:'Need Help.txt';             DOS:'NEEDHELP.TXT'),
    (Name:'Ralf Brown.txt';            DOS:'RBROWN.TXT'),
    (Name:'Read Me Now.txt';           DOS:'README.NOW'),
    // List Files
    (Name:'Bibliography';              DOS:'BIBLIO.LST'),
    (Name:'Category';                  DOS:'CATEGORY.KEY'),
    (Name:'Cmos-Memory Map';           DOS:'CMOS.LST'),
    (Name:'Far Call Interface';        DOS:'FARCALL.LST'),
    (Name:'Glossary';                  DOS:'GLOSSARY.LST'),
    (Name:'I2C-Bus Devices';           DOS:'I2C.LST'),
    (Name:'Interrupt List';            DOS:'INTERRPU.LST'),
    (Name:'Links';                     DOS:'LINKS.LST'),
    (Name:'Memory Map';                DOS:'MEMORY.LST'),
    (Name:'Model-Specific Registers';  DOS:'MSR.LST'),
    (Name:'Overview';                  DOS:'OVERVIEW.LST'),
    (Name:'Ports List';                DOS:'PORTS.LST'),
    (Name:'Selected Tables';           DOS:'TABLES.LST'),
    (Name:'System-Management Mode';    DOS:'SMM.LST')
  );

{ ---------------------------------------------------------------------------- }

function DosFileName(FileName : String) : String; // Returns the DOS file name
var
  I : Integer;
  B, E : String;
begin
  Result:=UpperCase(FileName);
 for I := 0 to High(DOSNAMES) do
    if Result=UpperCase(DOSNAMES[I].Name) then begin
      Result:=DOSNAMES[I].DOS;
      Break;
    end;
  E:=ExtractFileExt(Result);
  B:=Copy(Result, 1, Length(Result) - Length(E));
  B:=StringReplace(B, SPACE, '', [rfReplaceAll]);
  B:=Copy(B, 1, 8);
  E:=Copy(E, 1, 4);
  Result:=B+E;
end;

{ ---------------------------------------------------------------------------- }
procedure CreateListFiles; // Assemble the group data into listing files.
var
  I : Integer;
  L : TArrayOfString;
  N : String;
begin
  L:=DirScan(DirSource + WildCard, [dsDirectories]);
  for I := 0 to High(L) do begin
    if UpperCase(L[I]) = 'MISCELLANEOUS' then Continue;
    N:=DosFileName(L[I]);
    LogMessage(vbNormal, 'Group: ' + L[I] + SPACE + '(' + N + ')');
  end;
end;

procedure CopyMiscellaneousFiles; // Simply copies Un-broken Miscellaneous files
var
  I, E : Integer;
  L : TArrayOfString;
begin
  L:=DirScan(DirSource + 'Miscellaneous' + PathDelimiter + Wildcard, [dsFiles]);
  for I := 0 to High(L) do begin
    if VerboseLevel > vbVerbose then
      LogMessage(vbVerbose, 'Copy miscellaneous file: "' + L[I] + '" to "' + DosFileName(L[I]) + '"')
    else
      LogMessage(vbVerbose, 'Copy miscellaneous file: ' + L[I]);
    E:=FileCopy(DirSource + 'Miscellaneous' + PathDelimiter + L[I], DirOutput + DosFileName(L[I]));
    if E <> 0 then begin
      LogMessage(vbCritical, 'Error #'+ IntToStr(E) + ' while trying to copy "' +
        DirSource + 'Miscellaneous' + PathDelimiter + L[I] + '" to "' +
        DirOutput + DosFileName(L[I]) + '"');
      Halt(1);
    end;
  end;
end;

procedure Build; // Assemble the List Files;
begin
  // Verify paths
  if not DirectoryExists(DirSource) then begin
    LogMessage(vbCritical, 'Source path not found: ' + DirSource);
    Halt(1);
  end else
    LogMessage(vbVerbose, 'Source path: ' + FriendlyPath(AppBasePath, DirSource));
  if DirectoryExists(DirOutput) then
    LogMessage(vbMinimal, 'Output path already exists: ' + FriendlyPath(AppBasePath, DirOutput))
  else begin
    LogMessage(vbNormal, 'Create output path: ' + FriendlyPath(AppBasePath, DirOutput));
    if not CreateTree(DirOutput) then begin
      LogMessage(vbCritical, 'Failed to create output path: ' + DirOutput);
      Halt(1);
    end;
  end;
  CopyMiscellaneousFiles;
  CreateListFiles;
end;

{ ---------------------------------------------------------------------------- }

procedure Banner;  // Display Program Banner and Version
begin
  { Program Information Banner }
  LogMessage(vbNormal,APP_PRODUCTNAME + ' v' + APP_VERSION + ' (build ' + APP_BUILD+ ')');
  LogMessage(vbNormal,'Copyright ' + APP_LEGALCOPYRIGHT);
  LogMessage(vbNormal,'BSD 3-Clause License');
  LogMessage(vbNormal, '');
end;

procedure Help; // Show Command-Line help

const
  Switches : array of record
    S, L, V, M : String;
  end = (
    (S:''; L:'--version'; V:''; M:'Display version information and exit.'),
    (S:'-h'; L:'--help'; V:''; M:'Display command-line help and exit.'),
    (S:''; L:''; V:''; M:''),
    (S:'-v'; L:'--verbose'; V:'(level)'; M:'Specify a verbosity level (0-4).'),
    (S:'-s'; L:'--source'; V:'(path)'; M:'Specify a path that contains the source files.'),
    (S:'-o'; L:'--output'; V:'(path)'; M:'Specify a path to store the output files.')
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
    S:=Switches[I].S;
    if Switches[I].L <> '' then
      Cat(S, WhenTrue(S <> '', ', ') + Switches[I].L);
    if S = '' then begin
      WriteLn;
      if Switches[I].M <> '' then begin
        WriteLn(Switches[I].M);
        WriteLn;
      end;
      continue;
    end else
      S:=RightPad(S, WA) + RightPad(Switches[I].V, WB) + Switches[I].M;
    WriteLn(S);
  end;
  WriteLn;
end;

procedure SeeHelp(Message : String);
begin
  LogMessage(vbCritical, Message);
  LogMessage(vbCritical, 'see: ' + ExtractFileName(ParamStr(0)) + ' --help');
  Halt(1);
end;

procedure Options; // Parse Program Command-Line Options
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
      '-s', '--source' : DirSource:=IncludeTrailingPathDelimiter(Trim(NextOpt));
      '-o', '--output' : DirOutput:=IncludeTrailingPathDelimiter(Trim(NextOpt));
    else
      SeeHelp('Invalid command-line option: ' + Opt);
    end;
  end;
end;

{$R *.res}

begin
  Options;
  Banner;
  Build;
  LogMessage(vbNormal, '');
  LogMessage(vbNormal, 'Done.');
end.

