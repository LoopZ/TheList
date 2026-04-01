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

type
  TSectionKind = (
    skNone, // Simply copied files or is not broken into multiple files
    skInterrupt, skCMOS, skFarCall, skI2C, skMemory, skMSR, skPort,
    skStandard, // All the same
    skGlossary, // Just glued together
    skTable, // Just glued together in an empty comment section
    skSMM, // Just glued together in separate "other" sections
    skLink // Just glued together in a Links comment section
  );

const
  SrcExt = '.txt';                  // File extension for all source files
  HeaderFile = '_Header' + SrcExt;  // LST Header Data file
  ReleaseFile = '_Release' + SrcExt; // TheList Release version, fairly static.
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

  DOSNAMES : array of record // Conversion of Long File Names to DOS versions
    Name, DOS : String;
    Kind : TSectionKind;
  end = (
    // Miscellaneous files
    (Name:'Advertisement'+SrcExt;      DOS:'_ADVERT.TXT';  Kind:skNone),
    (Name:'FAQ'+SrcExt;                DOS:'faq.lst';      Kind:skNone),
    (Name:'Interrupt Primer'+SrcExt;   DOS:'INTERRUP.PRI'; Kind:skNone),
    (Name:'Need Help'+SrcExt;          DOS:'NEEDHELP.TXT'; Kind:skNone),
    (Name:'Ralf Brown'+SrcExt;         DOS:'RBROWN.TXT';   Kind:skNone),
    (Name:'Read Me Now'+SrcExt;        DOS:'README.NOW';   Kind:skNone),
    // List Files
    (Name:'Bibliography';              DOS:'BIBLIO.LST';   Kind:skNone),
    (Name:'Category';                  DOS:'CATEGORY.KEY'; Kind:skNone),
    (Name:'Cmos-Memory Map';           DOS:'CMOS.LST';     Kind:skCMOS),
    (Name:'Far Call Interface';        DOS:'FARCALL.LST';  Kind:skFarCall),
    (Name:'Glossary';                  DOS:'GLOSSARY.LST'; Kind:skGlossary),
    (Name:'I2C-Bus Devices';           DOS:'I2C.LST';      Kind:skI2C),
    (Name:IntFirstDir;                 DOS:'INTERRUP.1ST'; Kind:skNone),
    (Name:IntListDir;                  DOS:'INTERRUP.LST'; Kind:skInterrupt),
    (Name:'Links';                     DOS:'LINKS.LST';    Kind:skLink),
    (Name:'Memory Map';                DOS:'MEMORY.LST';   Kind:skMemory),
    (Name:'Model-Specific Registers';  DOS:'MSR.LST';      Kind:skMSR),
    (Name:'Overview';                  DOS:'OVERVIEW.LST'; Kind:skNone),
    (Name:'Ports List';                DOS:'PORTS.LST';    Kind:skPort),
    (Name:'Selected Tables';           DOS:'TABLES.LST';   Kind:skTable),
    (Name:'System-Management Mode';    DOS:'SMM.LST';      Kind:skSMM)
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

  SECTION_ORDER : TArrayOfString = (
    'Introduction',
    'Disclaimer',
    'Note',
    'Flags',
    'Categories',
    'Category Keys',
    'Titles',
    '*',
    'Bibliography',
    'Undone',
    'History',
    'Contributors',
    'Credits',
    'Contact Info',
    'Admin'
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
  SectionKind : TSectionKind;    // Current List Type

  ReleaseVersion : RawByteString;// For LST file headers, probably not used
  WorkingData  : RawByteString;  // Working output data for LST file
  CommentFiles : TStringList;    // For verification all section comment files were used.
  SectionTree : TBinaryTree;     // For assembling the section file data

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

// Returns the Section Kind for Processing Data files
function FindSectionKind(FileName : String) : TSectionKind;
var
  I : Integer;
begin
  FileName:=UpperCase(FileName);
  for I := 0 to High(DOSNAMES) do
    if (FileName=UpperCase(DOSNAMES[I].Name)) or
    (FileName=UpperCase(DOSNAMES[I].DOS)) then begin
      Result:=DOSNAMES[I].Kind;
      Exit;
    end;
  LogMessage(vbMinimal, 'Unknown section kind for: ' + FileName);
  Result:=skNone;
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
  if CommentFiles.Count = 0 then begin
    LogMessage(vbExcessive, TAB+'No orphan comment sections for: ' + OutName);
    Exit;
  end;
  LogMessage(vbMinimal, 'Group Comment files for ' + OutName + ' were excluded:');
  for I := 0 to CommentFiles.Count - 1 do
    LogMessage(vbMinimal, TAB + CommentFiles[I]);
end;

// Reads a file if it exists, normalizes line endings and truncates trailing blank lines.
function ReadFile(FileName : String; out Data : RawByteString) : boolean;
var
  E : Integer;
  A : TArrayOfString;
  I : Integer;
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
  A:=Explode(NormalizeLineEndings(Data, leLF));
  for I := 0 to High(A) do
    A[I]:=TrimRight(A[I]);
  Data:=Implode(A, CRLF);
  while Copy(Data, Length(Data) - 3) = CRLF+CRLF do
    SetLength(Data, Length(Data) - 2);
  while Copy(Data, 1, 2) = CRLF do System.Delete(Data, 1, 2);
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
       RightPad(ReleaseVersion, 14) + TAB +
       'Last Change ' + LastChange
   else
     Header:=Title + COMMA + SPACE + 'part ' + IntToStr(Part) + ' of ' +
       IntToStr(Total);
   if HeaderData <> '' then
     Cat(Header, CRLF + HeaderData);
   Header:=IncludeTrailing(Header, CRLF);
end;

function SectionMarker(Section : String; Kind : Char = '!') : String;
begin
  Section:=StringReplace(Section, SPACE, UNDERSCORE, [rfReplaceAll]);
  Result:=RightPad(SectionLead + Kind + '---' + Section, SectionWidth, '-') + CRLF;
end;

{ ---------------------------------------------------------------------------- }
// Create the list of Files for the release
function CreateFileList : String;
begin
  Result:='(pending)'+CRLF;
end;

procedure AddGlossary(const Name : String; var Data : RawByteString);
var
  N : TBinaryTreeNode;
  ID : RawByteString;
begin
  ID:=Trim(PopDelim(Data, CRLF));
   if ID='' then begin
     LogMessage(vbMinimal, TAB + 'No Term specified for file: ' + Name);
     Exit;
   end;
   while Copy(Data, 1, 2) = CRLF do System.Delete(Data, 1, 2);
   if Length(Data) = 0 then begin
     LogMessage(vbMinimal, TAB + 'No definition, excluded for file: ' + Name);
     Exit;
   end;
   N := SectionTree.Add(ID, ID + CRLF + Data);
   if not Assigned(N) then
     LogMessage(vbMinimal, TAB + 'Duplicate Term for file: ' + Name);
end;

procedure AddDataFiles;
var
  I : Integer;
  L : TStringList;
  S, Data : RawByteString;
  N : TBinaryTreeNode;
begin
  SectionTree.Clear;
  L := TStringList.Create;
  DirScan(BaseDir + Wildcard, L, [dsFiles, dsRecursive]);
  for I := 0 to L.Count - 1 do begin
    S:=ExtractFileName(L[I]);
    if HasLeading(S, UNDERSCORE) then Continue;
    if not ReadFile(BaseDir + L[I], Data) then begin
      LogMessage(vbMinimal, 'File went missing: ' + BaseDir + L[I]);
      Continue;
    end;
    Data:=NormalizeLineEndings(Data, leCRLF);
    case SectionKind of
      skNone : begin
        LogMessage(vbMinimal, TAB + 'ignored file: ' + L[I]);
      end;
      skStandard, skInterrupt, skCMOS, skFarCall,
      skI2C, skMemory,skMSR, skPort : begin
      end;
      skGlossary : AddGlossary(L[I], Data);
      skTable : begin // Just glued together in an empty comment section
      end;
      skSMM : begin // Just glued together in separate "other" sections
      end;
      skLink: begin // Just glued together in a Links comment section
      end;
    end;
  end;
  L.Free;
  if SectionTree.Count = 0 then begin
    LogMessage(vbNormal, TAB + 'no entries');
    Exit;
  end;
  case SectionKind of
    skGlossary : Cat(WorkingData, SectionMarker('GLOSSARY'));
    skTable    : Cat(WorkingData, SectionMarker('TABLES'));
    skLink     : Cat(WorkingData, SectionMarker('LINKS'));
  end;
  N := SectionTree.First;
  While Assigned(N) do begin
    Cat(WorkingData, N.Text);
    N:=N.Next;
    if Assigned(N) then
      Cat(WorkingData, CRLF);
  end;
end;

procedure CreateList(PathName: String; const Sections : TArrayOfString);
var
  I : Integer;
  Section : String;
  Data : RawByteString;
begin
  OutName:=DosFileName(PathName);
  LogMessage(vbNormal, 'Group: ' + PathName + SPACE + '(' + OutName + ')');
  SectionKind:=FindSectionKind(PathName);
  BaseDir:=DirSource + PathName + PathDelimiter;
  CreateCommentFilesList(BaseDir);
  ReadFile(BaseDir + HeaderFile, HeaderData);
  SetHeader(PathName);
  PopCommentFile(HeaderFile);
  WorkingData:='';
  for I := 0 to High(Sections) do begin
    Section:=UpperCase(Sections[I]);
    PopCommentFile(Section);
    if Section = '*' then begin
      AddDataFiles;
      Continue;
    end;
    if Section <> 'FILELIST' then begin
      if not ReadFile(BaseDir + UNDERSCORE + Sections[I] + SrcExt, Data) then
        Continue;
      Data:=NormalizeLineEndings(Data, leCRLF);
    end else begin
      Data:=CreateFileList;
    end;
    if Data = '' then Continue;
    LogMessage(vbVerbose, TAB+'Added comment section: ' + Section);
    Cat(WorkingData, SectionMarker(Section));
    Cat(WorkingData, Data);
  end;
  if WorkingData <> '' then
  WriteFile(OutName, Header + WorkingData);
  CommentOrphans;
end;

// Create the INTERRUP.1ST file
procedure CreateInterruptFirst;
begin
  CreateList(IntFirstDir, INTERRUPT_1ST);
end;

// Assemble the group data into listing files.
procedure CreateListFiles;
var
  I : Integer;
  L : TArrayOfString;
begin
  L:=DirScan(DirSource + WildCard, [dsDirectories]);
  for I := 0 to High(L) do begin
    if UpperCase(L[I]) = UpperCase(MiscFileDir) then Continue; // Handled elsewhere
    if UpperCase(L[I]) = UpperCase(IntFirstDir) then Continue; // Handled last
    CreateList(L[I], SECTION_ORDER);
 end;
  CreateInterruptFirst;
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
  if FileLoad(DirSource + ReleaseFile, ReleaseVersion) <> 0 then
    ReleaseVersion:='Release ?';
  ReleaseVersion:=StringReplace(ReleaseVersion, CR, SPACE, [rfReplaceAll]);
  ReleaseVersion:=StringReplace(ReleaseVersion, LF, SPACE, [rfReplaceAll]);
  ReleaseVersion:=Trim(StringReplace(ReleaseVersion, TAB, SPACE, [rfReplaceAll]));
  SectionTree := TBinaryTree.Create;
  CommentFiles:=TStringList.Create;
  BuildTime:=Now;
  LastChange:=Lowercase(FormatDateTime('ddmmmYY hh:nn', BuildTime));
  TitleWidth:=0;
  for I := 0 to High(DOSNAMES) do
     if Length(DOSNAMES[I].Name) > TitleWidth then
       TitleWidth:=Length(DOSNAMES[I].Name);
  Inc(TitleWidth);
  CopyMiscellaneousFiles;
  CreateListFiles;
  CommentFiles.Free;
  SectionTree.Free;
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

