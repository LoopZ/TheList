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
  Version, PasExt;

const
  SrcExt = '.txt';                  // File extension for all source files
  HeaderFile = '_Header' + SrcExt;  // LST Header Data file
  SectionLead = '--------';         // Leading characters for all section breaks
  SectionWidth = 45;                // Number of charcters in a section break
  IntListDir = 'Interrupt List';    // Main Interrupt List source directory subpath
  IntFirstDir = 'Interrupt First';  // The Interrup.1st source directory subpath
  MiscFileDir = 'Miscellaneous';    // Dir with Miscellaneous files that are just copied

  {$IFDEF Windows}
  DirSource : String = '..\..\source\';
  DirOutput : String = '..\..\TheList\';
  {$ELSE}
  DirSource : String = '../../source/';
  DirOutput : String = '../../TheList/';
  {$ENDIF}

  MaxFileSize : integer = 360 * 1024; // Maximum bytes allowed in LST file
  ReleaseNumber : integer = 62;       // For LST file headers, probably not used

  DOSNAMES : array of record // Conversion of Long File Names to DOS versions
    Name, DOS : String;
  end = (
    // Miscellaneous files
    (Name:'Advertisement'+SrcExt;      DOS:'_ADVERT.TXT'),
    (Name:'FAQ'+SrcExt;                DOS:'faq.lst'),
    (Name:'Interrupt Primer'+SrcExt;   DOS:'INTERRUP.PRI'),
    (Name:'Need Help'+SrcExt;          DOS:'NEEDHELP.TXT'),
    (Name:'Ralf Brown'+SrcExt;         DOS:'RBROWN.TXT'),
    (Name:'Read Me Now'+SrcExt;        DOS:'README.NOW'),
    // List Files
    (Name:'Bibliography';              DOS:'BIBLIO.LST'),
    (Name:'Category';                  DOS:'CATEGORY.KEY'),
    (Name:'Cmos-Memory Map';           DOS:'CMOS.LST'),
    (Name:'Far Call Interface';        DOS:'FARCALL.LST'),
    (Name:'Glossary';                  DOS:'GLOSSARY.LST'),
    (Name:'I2C-Bus Devices';           DOS:'I2C.LST'),
    (Name:IntListDir;                  DOS:'INTERRUP.LST'),
    (Name:IntFirstDir;                 DOS:'INTERRUP.1ST'),
    (Name:'Links';                     DOS:'LINKS.LST'),
    (Name:'Memory Map';                DOS:'MEMORY.LST'),
    (Name:'Model-Specific Registers';  DOS:'MSR.LST'),
    (Name:'Overview';                  DOS:'OVERVIEW.LST'),
    (Name:'Ports List';                DOS:'PORTS.LST'),
    (Name:'Selected Tables';           DOS:'TABLES.LST'),
    (Name:'System-Management Mode';    DOS:'SMM.LST')
  );

  // Sections to include in the INTERRUP.1ST file
  INTERRUPT_1ST : TArrayOfString = (
    'Filelist',
    'Copyright',
    'Disclaimer',
    'Availability',
    'Abbreviations',
    'Credits',
    'Addresses',
    'Trademarks',
    'Quotes',
    'Contact Info'
  );

{ ---------------------------------------------------------------------------- }
// Global processing variables
var
  BaseDir    : String;           // LST file source base directory
  OutName    : String;           // LST Output FileName
  TitleWidth : integer;          // Maximum LST file title width (+1)
  Header     : String;           // Header for LST file
  HeaderData : RawByteString;    // Text from _Header file when present
  BuildTime  : TDateTime;        // Release Build Date/Time
  LastChange : String;           // LST file release time stamp

  CommentFiles : TStringList; // For verification all section comment files were used.

{ ---------------------------------------------------------------------------- }

// Returns the DOS file name from the Long file name or Group Title
function DosFileName(FileName : String) : String;
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

// Creates a File List of all the _*.txt files in a group. Used to insure all
// were written to the LST file.
procedure CreateCommentFilesList(PathName : String);
begin
  CommentFiles.Clear;
  CommentFiles.Sorted:=True;
  DirScan(PathName + UNDERSCORE + WildCard + SrcExt, CommentFiles, [dsFiles]);
  UpperCase(CommentFiles);
end;

// Removes a file from the CommentFiles List
procedure PopCommentFile(FileName : String);
var
  I : Integer;
begin
  FileName:=IncludeLeading(UpperCase(IncludeTrailing(FileName,SrcExt, false)), UNDERSCORE);
  if not CommentFiles.Find(FileName, I) then Exit;
  CommentFiles.Delete(I);
end;

// Show Comment Files not included in LST file
procedure CommentOrphans;
var
  I : Integer;
begin
  if CommentFiles.Count = 0 then Exit;
  LogMessage(vbMinimal, 'Group Comment files for ' + OutName + ' were excluded:');
  for I := 0 to CommentFiles.Count - 1 do
    LogMessage(vbMinimal, TAB + CommentFiles[I]);
end;

// Reads a file if it exists, normalizes line endings and truncates trailing blank lines.
function ReadFile(FileName : String; out Data : RawByteString) : boolean;
var
  E : Integer;
begin
  if not FileExists(FileName) then begin
    Data:='';
    Exit(False);
  end;
  E:=PasExt.FileLoad(FileName, Data);
  if E <> 0 then begin
    LogMessage(vbCritical, 'File Read Error #' + IntToStr(E) + ': ' + FileName);
    Halt(E);
  end;
  Data:=NormalizeLineEndings(Data, leCRLF);
  while Copy(Data, Length(Data) - 3) = CRLF+CRLF do
    SetLength(Data, Length(Data) - 2);
  Result:=True;
end;

// Saves a LST file to the output directory
procedure WriteFile(FileName : String; const Data : RawByteString);
var
  E : Integer;
begin
 FileName:=DirOutput + FileName;
  E:=PasExt.FileSave(FileName, Data);
   if E <> 0 then begin
     LogMessage(vbCritical, 'File Write Error #' + IntToStr(E) + ': ' + FileName);
     Halt(E);
   end;
end;

// Generate LST file header
procedure SetHeader(Title : String; Part : integer = 0; Total : integer = 0);
begin
   if Part + Total = 0 then
     Header:=RightPad(Title, TitleWidth) + TAB +
       RightPad('Release ' + IntToStr(ReleaseNumber), 12) + TAB +
       'Last Change ' + LastChange
   else
     Header:=Title + COMMA + SPACE + 'part ' + IntToStr(Part) + ' of ' +
       IntToStr(Total);
   if HeaderData <> '' then
     Cat(Header, CRLF + HeaderData);
   Header:=IncludeTrailing(Header, CRLF);
end;

function SectionComment(Section : String) : String;
begin
  Section:=StringReplace(Section, SPACE, UNDERSCORE, [rfReplaceAll]);
  Result:=RightPad(SectionLead + '!---' + Section, SectionWidth, '-') + CRLF;
end;

{ ---------------------------------------------------------------------------- }
// Create the list of Files for the release
function CreateFileList : String;
begin
  Result:='(pending)'+CRLF;
end;

// Create the INTERRUP.1ST file
procedure CreateInterruptFirst;
var
  I : Integer;
  S, Section : String;
  Data : RawByteString;
begin
  BaseDir:=DirSource + IntFirstDir + PathDelimiter;
  CreateCommentFilesList(BaseDir);
  ReadFile(BaseDir + HeaderFile, HeaderData);
  SetHeader(IntFirstDir);
  S:=Header;
  PopCommentFile(HeaderFile);
  for I := 0 to High(INTERRUPT_1ST) do begin
    Section:=UpperCase(INTERRUPT_1ST[I]);
    PopCommentFile(Section);
    Cat(S, SectionComment(Section));
    if Section <> 'FILELIST' then begin
      if not ReadFile(BaseDir + UNDERSCORE + INTERRUPT_1ST[I] + SrcExt, Data) then
        Continue;
      Cat(S, NormalizeLineEndings(Data, leCRLF));
    end else
      Cat(S, CreateFileList);
    // S:=IncludeTrailing(S, CRLF);
  end;
  OutName:=DosFileName(IntFirstDir);
  WriteFile(OutName, S);
  CommentOrphans;
end;

// Assemble the group data into listing files.
procedure CreateListFiles;
var
  I : Integer;
  L : TArrayOfString;
begin
  L:=DirScan(DirSource + WildCard, [dsDirectories]);
  for I := 0 to High(L) do begin
    if UpperCase(L[I]) = UpperCase(MiscFileDir) then Continue;
    if UpperCase(L[I]) = UpperCase(IntFirstDir) then Continue;
    OutName:=DosFileName(L[I]);
    LogMessage(vbNormal, 'Group: ' + L[I] + SPACE + '(' + OutName + ')');
    BaseDir:=DirSource + L[I] + PathDelimiter;
    ReadFile(BaseDir + HeaderFile, HeaderData);
    CreateCommentFilesList(BaseDir);
    CommentOrphans;
 end;
end;

// Simply copies Miscellaneous files that are not broken down for editing
procedure CopyMiscellaneousFiles;
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

// Assemble the List Files
procedure Build;
var
  I : Integer;
begin
  CommentFiles:=TStringList.Create;
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
  BuildTime:=Now;
  LastChange:=Lowercase(FormatDateTime('ddmmmYY hh:nn', BuildTime));
  TitleWidth:=0;
  for I := 0 to High(DOSNAMES) do
     if Length(DOSNAMES[I].Name) > TitleWidth then
       TitleWidth:=Length(DOSNAMES[I].Name);
  Inc(TitleWidth);
  CopyMiscellaneousFiles;
  CreateListFiles;
  CreateInterruptFirst;
  CommentFiles.Free;
end;

{ ---------------------------------------------------------------------------- }

// Display Program Banner and Version
procedure Banner;
begin
  LogMessage(vbNormal,APP_PRODUCTNAME + ' v' + APP_VERSION + ' (build ' + APP_BUILD+ ')');
  LogMessage(vbNormal,'Copyright ' + APP_LEGALCOPYRIGHT);
  LogMessage(vbNormal,'The Clear BSD License ');
  LogMessage(vbNormal, '');
end;

// Show Command-Line help
procedure Help;

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

// Print the SeeHelp error message and exit
procedure SeeHelp(Message : String);
begin
  LogMessage(vbCritical, Message);
  LogMessage(vbCritical, 'see: ' + ExtractFileName(ParamStr(0)) + ' --help');
  Halt(1);
end;

// Parse Program Command-Line Options
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

