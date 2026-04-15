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
  Classes, SysUtils, IniFiles, StrUtils,
  { you can add units after this }
  Version, PasExt, BinTree, CfgOpts;

type
  TSectionKind = (
    skNone,      // Simply copied files or is not broken into multiple files
    skInterrupt, skCMOS, skFarCall, skI2C, skMemory, skMSR, skPort,
    skStandard,  // All the same
    skGlossary,  // Just glued together
    skTable,     // Just glued together in an empty comment section
    skSMM,       // Just glued together in separate "other" sections
    skLink,      // Just glued together in LINK comment sections
    skFAQ        // Just glued together in FAQ comment sections
  );

const
  SrcExt = '.txt';                  // File extension for all source files
  MapFile = '_Mapping' + SrcExt;    // Directory Mapping file
  HeaderFile = '_Header' + SrcExt;  // LST Header Data file
  ReleaseFile = '_Release' + SrcExt; // TheList Release version, fairly static.
  SectionLead = '--------';         // Leading characters for all section breaks
  SectionWidth = 45;                // Number of charcters in a section break
  IntFirstDir : String = 'Interrupt First';  // The Interrup.1st source directory subpath
  MiscFileDir : String = 'Miscellaneous'; // Dir with Miscellaneous files that are just copied

  FILE_1ST = 'INTERRUP.1ST'; // That pesky non-interrupt list interrupt file :-)


  MaxLinkID : integer = 19; // Maximum Length of Link Section ID in the LINKS.LST

  // Default conversion information of Long File Names to DOS versions
  DOSNAMES : array of record
    Name,                  // Long file name of File or Group Path.
    DOS : String;          // DOS file name for output file.
    Kind : TSectionKind;   // Type of sections used by that file.
  end = (
    // Miscellaneous files
    (Name:'Advertisement'+SrcExt;      DOS:'_ADVERT.TXT';  Kind:skNone),
    (Name:'Interrupt Primer'+SrcExt;   DOS:'INTERRUP.PRI'; Kind:skNone),
    (Name:'Need Help'+SrcExt;          DOS:'NEEDHELP.TXT'; Kind:skNone),
    (Name:'Ralf Brown'+SrcExt;         DOS:'RBROWN.TXT';   Kind:skNone),
    (Name:'Read Me Now'+SrcExt;        DOS:'README.NOW';   Kind:skNone),
    // List Files
    (Name:'Frequently-Asked Questions';DOS:'FAQ.LST';      Kind:skFAQ),
    (Name:'Bibliography';              DOS:'BIBLIO.LST';   Kind:skNone),
    (Name:'Category';                  DOS:'CATEGORY.KEY'; Kind:skNone),
    (Name:'Cmos-Memory Map';           DOS:'CMOS.LST';     Kind:skCMOS),
    (Name:'Far Call Interface';        DOS:'FARCALL.LST';  Kind:skFarCall),
    (Name:'Glossary';                  DOS:'GLOSSARY.LST'; Kind:skGlossary),
    (Name:'I2C-Bus Devices';           DOS:'I2C.LST';      Kind:skI2C),
    (Name:'Interrupt First';           DOS:FILE_1ST;       Kind:skNone),
    (Name:'Interrupt List';            DOS:'INTERRUP.LST'; Kind:skInterrupt),
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

  // General order of sections for all LST files (excluding INTERRUP.1ST).
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

  // Default file information for INTERRUP.1ST's FILELIST section.
  DOSINFO : array of record
    Name, Text : String;
  end = (
  (Name:FILE_1ST;       Text:'this file'),
  (Name:'INTERRUP.LST'; Text:'Interrupts'),
  (Name:'PORTS.LST';    Text:'listing of I/O Ports'),
  (Name:'INTERRUP.PRI'; Text:'a brief primer on interrupts'),
  (Name:'OVERVIEW.LST'; Text:'brief listing of major uses of each interrupt'),
  (Name:'BIBLIO.LST';	Text:'bibliography of information sources for the list'),
  (Name:'CMOS.LST';     Text:'a description of the CMOS RAM data bytes'),
  (Name:'FARCALL.LST';  Text:'APIs available through FAR CALLs'),
  (Name:'GLOSSARY.LST'; Text:'a glossary of terms, abbreviations, and acronyms'),
  (Name:'MEMORY.LST';   Text:'format of the BIOS data area and other memory regions'),
  (Name:'MSR.LST';      Text:'a listing of Model-Specific Registers'),
  (Name:'CATEGORY.KEY'; Text:'descriptions of divider-line category letters'),
  (Name:'TABLES.LST';   Text:'a list of selected tables'),
  (Name:'LINKS.LST';    Text:'a list of links to reference and sites in the docs'),
  (Name:'FAQ.LST';      Text:'a list of frequently asked questions'),
  (Name:'README.NOW';   Text:'a read me document')
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
  HeaderBar : String;            // 80-chars, but well will just test it is at least 70
  TableTree : TBinaryTree;       // For verification, table numbers are unique
  TableCode : String;            // Table Letter Code
  TableHigh : integer;           // Highest Table Number
  TableCount : integer;          // Number of tables in LST

  TotalEntries : integer;        // total of all entries in all sections
  TotalTables  : integer;        // total of all tables in all sections
  TotalErrors  : integer;        // total number of actual errors
  TotalProblems : integer;       // Number of non-errors that will be problems for parsers
  TotalWarnings : integer;       // Less Severe problems that should be fixed at some point
  TotalDuplicates : integer;     // Number of entries with duplicate IDs
  FileInfo : TBinaryTree;        // File Info for the FILELIST Section

{ ---------------------------------------------------------------------------- }
const
  IssueOpen : boolean = false;   // Tracks if the Issues file has been opened.

var
  LastFileName: String;          // The current working Entry file name for Issues.
  IssueF : Text;                 // The Issues text file handle.

// Saves Filename of Entry with a problem, error, etc to a file when the
// "Issues" file name is set.
procedure WriteIssue;
begin
  if Issues= '' then Exit;
  if not IssueOpen then begin
    System.Assign(IssueF, Issues);
    System.Rewrite(IssueF);
    IssueOpen:=True;
  end;
  WriteLn(IssueF, LastFileName);
end;

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

// Adds an item to the list of files in the FILELIST section of INTERRUP.1ST
procedure AddFileInfo(Filename, Title : String; Data : String = '');

  function GetText(Name : String) : String;
  var
    I : Integer;
  begin
     Result:='';
     for I := 0 to High(DOSINFO) do
       if UpperCase(Name) = DOSINFO[I].Name then begin
         Result :=DOSINFO[I].Text;
         Exit;
       end;
  end;

var
  S: String;

begin
  FileName:=DosFileName(FileName);
  if Uppercase(FileName) = FILE_1ST then exit;
  if FileInfo.Count = 0 then
    FileInfo.Add(FILE_1ST, GetText(FILE_1ST));
  S:=GetText(FileName);
  if S='' then
    S:=GetText(ChangeFileExt(FileName, '.LST'));
  if S='' then
    S:=Title;
  if Data <> '' then
    Cat(S, COMMA + SPACE);
  FileInfo.Add(FileName, Trim(S + Data));
end;

// Remove extra CRLF from head and tail of text
function TrimCRLF(const Text : String) : String;
begin
 Result:=Text;
 while Copy(Result, Length(Result) - 3) = CRLF+CRLF do
   SetLength(Result, Length(Result) - 2);
 while Copy(Result, 1, 2) = CRLF do
   System.Delete(Result, 1, 2);
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

// Generate LST file header
// With file Title, release version, current date and copyright information.
// Or, Title with part n of NN for files that are broken into parts 2+ of NN.
procedure SetHeader(Title : String; Part : integer = 0; Total : integer = 0);
begin
   if LegacyMode then Title:=UpperCase(Title);
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

// Returns a string used for a Comment section divider in LST files.
function SectionMarker(Section : String; Kind : Char = '!') : String;
begin
  Section:=StringReplace(Section, SPACE, UNDERSCORE, [rfReplaceAll]);
  Result:=RightPad(SectionLead + Kind + '---' + Section, SectionWidth, '-') + CRLF;
end;

// Returns a string used for an Entry section divider in LST files.
function SectionEntry(Section : String; Category : String = '-') : String;
begin
  Section:=StringReplace(Section, SPACE, UNDERSCORE, [rfReplaceAll]);
  Cat(Category, '--');
  Category:=Copy(Category, 1, 2);
  Result:=RightPad(SectionLead + Category + Section, SectionWidth, '-') + CRLF;
end;

// Returns a string of FROM to TO of the Entries in a LST file Part.
// For use in the INTERRUP.1ST FILELIST section.
function GetFromTo(Const Data : RawByteString) : String;
  function IntID(ID : String) : String;
  begin
    Result:=Copy(ID, 1, 6);
    While HasTrailing(Result, '-') do SetLength(Result, Length(Result) - 1);
    case Length(Result) of
      2 : Result:=Result + 'h';
      4 : Result:=Copy(Result, 1,2) + 'h/AH=' + Copy(Result, 3,2);
      6 : Result:=Copy(Result, 1,2) + 'h/AX=' + Copy(Result, 3,4);
    end;
    Result:='INT ' + Result;
  end;

var
  P, E, I : integer;
  F, L : String;
begin
  Result:='';
  F:='';
  L:='';
  P:=0;
  // Find first entry section header
  repeat
    Inc(P);
    P:=Pos(CRLF + SectionLead, Data, P);
    if P < 1 then Exit;
    Inc(P, 12);
    if Copy(Data, P, 2) <> '--' then begin
      E:=Pos(CRLF, Data, P);
      if E < P then Exit;
      F :=Copy(Data, P, E-P);
      While HasTrailing(F, '-') do
        SetLength(F, Length(F) - 1);
      Break;
    end;
    P:=Pos(CRLF, Data, P+1);
  until P < 1;
  if F = '' then Exit;
  // Find last entry section header
  P:=Length(Data);
  repeat
    Dec(P);
    P:=RPosEx(CRLF + SectionLead, Data, P);
    if P < 1 then Exit;
    I:=P+12;
    if Copy(Data, I, 2) <> '--' then begin
      E:=Pos(CRLF, Data, I);
      if E < I then Exit;
      L :=Copy(Data, I, E-I);
      While HasTrailing(L, '-') do
        SetLength(L, Length(L) - 1);
      Break;
    end;
  until P < 1;
  if L = '' then Exit;
  case SectionKind of
    skPort : begin
      F:=ExcludeLeading(F, 'P');
      L:=ExcludeLeading(L, 'P');
      F:=Copy(F, 1, 4);
      L:=Copy(L, Length(L) - 3);
    end;
    skInterrupt: begin
      F:=RightPad(IntID(F), 16);
      L:=IntID(L);
    end;
  end;
  Result:='from ' + F + ' to ' + L;
end;

// Reads a file if it exists, normalizes line endings and truncates trailing
// and leading blank lines.
function ReadFile(FileName : String; out Data : RawByteString) : boolean;
var
  E : Integer;
  A : TArrayOfString;
  I : Integer;
begin
  LastFileName:='';
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
  Data:=TrimCRLF(Implode(A, CRLF) + CRLF);
  Result:=True;
  LastFileName:=FileName;
end;

// Saves a LST file to the output directory
procedure WriteFile(Title, FileName : String; Data : RawByteString);
var
  I, E, L : Integer;
  Parts : TArrayOfString;
  S : String;
  X, M : Integer;
begin
  if (MaxFileSize <> -1) and (Length(Data) > MaxFileSize) then begin
    Parts:=[];
    L:=Length(SectionMarker(''));
    Header := '';  // First part already has the header in the data.
    // Split the files into parts of MaxFileSize or less.
    repeat
      M:=MaxFileSize - L + 2 - Length(Header);
      X:=M;
      repeat
        X := RPosEx(CRLF + SectionLead, Data, X);
        S:=Copy(Data, X + 2, L);
        if (X <1)then begin
          LogMessage(vbCritical, 'Unable to split large list file: ' + FileName);
          Halt(1);
        end;
      until (Length(S) = L) and HasTrailing(S, '-' + CRLF);
      SetLength(Parts, Length(Parts)+1);
      Parts[High(Parts)]:=Copy(Data, 1, X);
      System.Delete(Data, 1, X);
      SetHeader(Title, 10, 100);
      Header:=ExcludeTrailing(Header, CRLF);
    until Length(Data)<=M;
    if Length(Data) > 0 then begin
      SetLength(Parts, Length(Parts)+1);
      Parts[High(Parts)]:=Data;
    end;
    // output the divided files
    FileName:=ChangeFileExt(DirOutput + FileName, '.A');
    for I := 0 to High(Parts) do begin
      Data:=Parts[I];
      if I > 0 then begin
        SetHeader(Title, I + 1, Length(Parts));
        Data:=ExcludeTrailing(Header, CRLF) + Data;
      end;
      if I < High(Parts) then
        Cat(Data, SectionMarker('Section'))
      else
        LogMessage(vbNormal, TAB + 'Divided into ' + IntToStr(Length(Parts)) +
          ' parts (A thru ' + ExcludeLeading(ExtractFileExt(FileName), '.') + ')');
     AddFileInfo(ExtractFileName(FileName), Title, GetFromTo(Data));
     E:=PasExt.FileSave(FileName, Data);
      if E <> 0 then begin
        LogMessage(vbCritical, 'File Write Error #' + IntToStr(E) + ': ' + FileName);
        Halt(E);
      end;
      S:=Copy(FileName, Length(FileName));
      // should never be more than 26, but just in case, this give us another
      // 52 possible extensions that will remain in order for use with tools
      // like "cat", before no longer being compatible with Standard DOS
      // 8+3 file names.
      if S < 'Z' then
        FileName[Length(FileName)] :=Char(Ord(S[1]) + 1)
      else
        Cat(FileName, 'A');
    end;

    Exit;
  end;
  FileName:=DirOutput + FileName;
  E:=PasExt.FileSave(FileName, Data);
  AddFileInfo(ExtractFileName(FileName), Title);
  if E <> 0 then begin
    LogMessage(vbCritical, 'File Write Error #' + IntToStr(E) + ': ' + FileName);
    Halt(E);
  end;
end;
{ ---------------------------------------------------------------------------- }
// Create the list of Files for the release
function CreateFileList : String;
var
  I : Integer;
  List : TStringList;
  N : TBinaryTreeNode;
begin
  List := TStringList.Create;
  Result:='';
  DirScan(DirOutput + WildCard, List, [dsFiles]);
  List.Sort;
  List.Sorted:=True;
  if not List.Find(FILE_1ST, I) then begin
    List.Add(FILE_1ST);
    List.Sort;
  end;
  for I := 0 to List.Count - 1 do begin
    Cat(Result, SPACE4 + RightPad(List[I], 16));
    N:=FileInfo.Find(List[I]);
    if Assigned(N) then
      Cat(Result, N.Text)
    else
      Cat(Result, '(Unspecified)');
    Cat(Result, CRLF);
  end;
  Cat(Result, CRLF +
    'Number of Entries = ' + IntToStr(TotalEntries) + CRLF +
    'Number of Tables = ' + IntToStr(TotalTables) + CRLF);
  List.Free;
end;

// Lowest level to add a Entry section into the Section Tree.
procedure AddToTree(const Name, ID : String; const Data : RawByteString);
var
  N : TBinaryTreeNode;
  XX : Integer;
begin
  N:= SectionTree.Add(ID, Data);
  if not Assigned(N) then begin
    LogMessage(vbVerbose, TAB + 'Duplicate UniqueID (' + ID + ') for file: ' + Name);
    Inc(TotalDuplicates);
    XX:=0;
    repeat
      N := SectionTree.Add(ID + SPACE + ZeroPad(XX, 6), Data);
      Inc(XX);
    until Assigned(N);
  end;
end;

// Adds a Glossary Entry into the Section Tree for the GLOSSARY.LST
procedure AddGlossary(const Name : String; var Data : RawByteString);
var
  ID : RawByteString;
begin
  ID:=Trim(PopDelim(Data, CRLF));
  if ID='' then begin
    LogMessage(vbMinimal, TAB + 'No Term specified for file: ' + Name);
    WriteIssue;
    Inc(TotalErrors);
    Exit;
  end;
  Data:=TrimCRLF(Data);
  if Length(Data) = 0 then begin
    LogMessage(vbMinimal, TAB + 'No definition, excluded file: ' + Name);
    WriteIssue;
    Inc(TotalErrors);
    Exit;
  end;
  AddToTree(Name, ID, ID + CRLF + Data);
end;

// Adds a Table Entry into the Section Tree for TABLES.LST
procedure AddTable(const Name : String; var Data : RawByteString);
begin
  AddGlossary(Name, Data);
end;

// Adds a LINK Entry into the Section Tree for LINKS.LST
procedure AddLink(const Name : String; var Data : RawByteString);
var
  ID, Title : RawByteString;
begin
  if LegacyMode then begin
    AddGlossary(Name, Data);
    Exit;
  end;
  Title:=Trim(PopDelim(Data, CRLF));
  ID:=UpperCase(StringReplace(Trim(Copy(Title, 1, MaxLinkID)), SPACE, '-', [rfReplaceAll]));
  if ID='' then begin
    LogMessage(vbMinimal, TAB + 'No Term specified for file: ' + Name);
    WriteIssue;
    Inc(TotalErrors);
    Exit;
  end;
  Data:=TrimCRLF(Data);
  if Length(Data) = 0 then begin
    LogMessage(vbMinimal, TAB + 'No definition, excluded file: ' + Name);
    WriteIssue;
    Inc(TotalErrors);
    Exit;
  end;
  AddToTree(Name, ID, SectionMarker('LINK:' + ID) + Title + CRLF + Data);
end;

// Adds a FAQ Entry into the Section Tree for FAQ.LST
procedure AddFAQ(const Name : String; var Data : RawByteString);
var
  ID : RawByteString;
begin
  if Length(Data) = 0 then begin
    LogMessage(vbMinimal, TAB + 'No data, excluded file: ' + Name);
    WriteIssue;
    Inc(TotalErrors);
    Exit;
  end;
  ID:=Name;
  ID:=PopDelim(ID, SPACE);
  ID:=PopDelim(ID, '-');
  ID:=PopDelim(ID, PERIOD);
  AddToTree(Name, ID, SectionMarker('FAQ:' + ID) + Data);
end;

// Adds a SMM Entry into the Section Tree for SMM.LST
procedure AddSMM(const Name : String; var Data : RawByteString);
var
  ID, Title : RawByteString;
begin
  Title:=Trim(PopDelim(Data, CRLF));
  ID:=UpperCase(StringReplace(Trim(Copy(Title, 1, MaxLinkID)), SPACE, '-', [rfReplaceAll]));
  ID:=ExcludeLeading(ID, 'SMM-');
  if ID='' then begin
    LogMessage(vbMinimal, TAB + 'No ID specified for file: ' + Name);
    WriteIssue;
    Inc(TotalErrors);
    Exit;
  end;
  Data:=TrimCRLF(Data);
  if Length(Data) = 0 then begin
    LogMessage(vbMinimal, TAB + 'No data, excluded file: ' + Name);
    WriteIssue;
    Inc(TotalErrors);
    Exit;
  end;
  if LegacyMode then
    AddToTree(Name, ID, SectionMarker('', '-') + Title + CRLF + Data)
  else
    AddToTree(Name, ID, SectionMarker('SMM:' + ID) + Title + CRLF + Data);
end;

// Parses a standard Entry file's header to return various information.
// Also, it removes that header from the data for eventual storage in the
// Section Tree for that specific LST file.
procedure FileToSection(const Name : String; var Data : RawByteString;
  out IDSORT, IDLIST, Category, Flags : String);
var
  I, SORTAS : Integer;
  H : TArrayOfString;
  K, V : String;
begin
  IDSORT:='';
  IDLIST:='';
  Category:='';
  Flags:='';
  // Remove Header Stub from Data
  PopDelim(Data, HeaderBar + CRLF);
  H:=Explode(PopDelim(Data, CRLF + HeaderBar));
  PopDelim(Data, CRLF);
  Data:=TrimCRLF(Data);
  // Parse Header Stub
  for I := 0 to High(H) do begin
    H[I]:=StringReplace(H[I], TAB, SPACE, [rfReplaceAll]);
    K:=Trim(PopDelim(H[I], COLON));
    H[I]:=Trim(H[I]);
    V:=PopDelim(H[I], SPACE);
    H[I]:=Trim(H[I]);
    case UpperCase(K) of
      'UNIQUE ID' : begin
        IDSORT:=V;
        IDLIST:=V;
        if H[I] <> '' then
          LogMessage(vbMinimal, 'Extraneous Data in Unique ID for file: '+ Name);
        SORTAS := Pos('-sort-as-', IDSORT);
        if SORTAS > 0 then begin
          IDSORT := Copy(IDSORT, SORTAS + Length('-sort-as-'));
          IDLIST := Copy(IDLIST, 1, SORTAS - 1);
        end;
      end;
      'SORT AS' : begin
         IDSORT:=V;
         if H[I] <> '' then
           LogMessage(vbMinimal, 'Extraneous Data in Sort As for file: '+ Name);
      end;
      'CATEGORY' : begin
        if UpperCase(V) = 'N/A' then
          V := '-';
        If Length(V) > 1 then
          LogMessage(vbMinimal, 'Extraneous Data in Category for file: '+ Name);
        Category:=Copy(V, 1,1);
      end;
      'FLAG', 'FLAGS' : begin
        if UpperCase(V) = 'N/A' then
          V := '-';
        If Length(V) > 1 then
          LogMessage(vbMinimal, 'Extraneous Data in Flags for file: '+ Name);
        FLAGS:=Copy(V, 1,1);
      end;
    else
      LogMessage(vbVerbose, 'Extraneous header field "' +  K + '" in file: '+ Name);
    end;
  end;
end;

// Finds tables in a section.
// Tracks the Highest Table number in that List.
// Verifies that no table ID is used more than once.
procedure HighestTable(const Name : String; const Data : RawByteString);
var
  P, F, E, V : Integer;
  Srch, S, T : String;
  N : TBinaryTreeNode;
begin
  Srch:=UpperCase(Data);
  P := 1;
  while True do begin
    F:=Pos('(TABLE', Srch, P);
    if F < P then Exit;
    E:=Pos(')', Srch, F + 6);
    if E < F then Break;
    P:=E+1;
    S:=Trim(Copy(Data, F+6, E - F - 6));
    // test for references to full tables and other types of status messages
    case Uppercase(S) of
      '',
      'FULL', 'IS FULL',
      'EMPTY', 'IS EMPTY' : Continue;
    end;
    Inc(TableCount);
    T := S;
    // set table prefix
    if TableHigh = -1 then begin
      TableCode:=PasExt.AlphaOnly(S);
      LogMessage(vbExcessive, TAB+'Table prefix code: '+
        WhenTrue(TableCode = '', '(null)', TableCode));
    end;
    if (TableCode <> '') then begin
      if (not HasLeading(S, TableCode)) then begin
        LogMessage(vbMinimal, TAB + 'Invalid Table Prefix in file: ' + Name);
        WriteIssue;
        Inc(TotalErrors);
        Continue;
      end;
      S:=ExcludeLeading(S, TableCode);
    end;
    Val(S, V, E);
    if E <> 0 then begin
      LogMessage(vbMinimal, TAB + 'Invalid Table Number in file: ' + Name);
      WriteIssue;
      Inc(TotalErrors);
      Continue;
    end;
    if V > TableHigh then TableHigh:=V;
    N:=TableTree.Add(T);
    if not Assigned(N) then begin
      LogMessage(vbMinimal, TAB + 'Duplicated Table ID (' + T + ') in file: ' + Name);
      WriteIssue;
      Inc(TotalErrors);
    end;
  end;
  LogMessage(vbMinimal, TAB+ 'Broken Table declaration in file: ' + Name);
  WriteIssue;
  Inc(TotalErrors);
end;

// Add a standard Entry (like INT, PORTS, MEMORY, etc) to the Section Tree.
procedure AddStandard(const Name : String; var Data : RawByteString);
var
  IDSORT, IDLIST, Category, Flags : String;
begin
  FileToSection(Name, Data, IDSORT, IDLIST, Category, Flags);
  if IDLIST='' then begin
    LogMessage(vbMinimal, TAB + 'No ID found for file: ' + Name);
    WriteIssue;
    Inc(TotalErrors);
    Exit;
  end;
  if Pos(HeaderBar, Data) > 0 then begin
    LogMessage(vbNormal, TAB+ 'Probably joined entry data in file: ' + Name);
    WriteIssue;
    Inc(TotalProblems);
  end;
  { TODO 5 -cDevel Add UID validation, not really possible }
  { TODO 5 -cDevel Add Category validation }
  { TODO 5 -cDevel Add Flags validation }
  HighestTable(Name, Data);
  if Length(Data) = 0 then begin
    LogMessage(vbMinimal, TAB + 'No data, excluded file: ' + Name);
    WriteIssue;
    Inc(TotalErrors);
    Exit;
  end;
  AddToTree(Name, IDSORT, SectionEntry(IDLIST, Category) + Data);
end;

// Recursively scan a Group directory, loading each Entry file and add their
// contents to the Section Tree based on the type of entries in that group.
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
        LogMessage(vbMinimal, TAB + 'ignore file: ' + L[I]);
      end;
      skStandard, skInterrupt, skCMOS, skFarCall,
      skI2C, skMemory,skMSR, skPort : AddStandard(L[I], Data);
      skGlossary : AddGlossary(L[I], Data);
      skTable : AddTable(L[I], Data);
      skSMM : AddSMM(L[I], Data);
      skLink: AddLink(L[I], Data);
      skFAQ: AddFAQ(L[I], Data);
    end;
  end;
  L.Free;
  if SectionTree.Count = 0 then begin
    if SectionKind <> skNone then
      LogMessage(vbNormal, TAB + 'no entries');
    Exit;
  end;
  case SectionKind of
    skGlossary : begin
      if LegacyMode then
        Cat(WorkingData, CRLF)
      else
        Cat(WorkingData, SectionMarker('GLOSSARY'));
    end;
    skTable : begin
      if LegacyMode then
        Cat(WorkingData, SectionMarker('') + CRLF)
      else
        Cat(WorkingData, SectionMarker('TABLES'));
    end;
    skLink : begin
      // Not in original RBIL, but will treat it like TABLES.LST in LegacyMode
      if LegacyMode then
        Cat(WorkingData, SectionMarker('') + CRLF)
    end;
    skSMM : begin
      if LegacyMode then
        Cat(WorkingData, CRLF)
    end;
  end;
  LogMessage(vbNormal, TAB + 'Number of Entries = ' + IntToStr(SectionTree.Count));
  LogMessage(vbNormal, TAB + 'Number of Tables = ' + IntToStr(TableCount));
  Inc(TotalEntries, SectionTree.Count);
  Inc(TotalTables, TableCount);
  N := SectionTree.First;
  While Assigned(N) do begin
    Cat(WorkingData, N.Text);
    N:=N.Next;
     if Assigned(N) then begin
       if (SectionKind = skGlossary) or (SectionKind = skTable) then
         Cat(WorkingData, CRLF)
       else if LegacyMode and (SectionKind = skLink) then
         Cat(WorkingData, CRLF);
     end;
  end;
end;

// Create a LST file with all of its Comment and Entry Sections in a
// order specified by the "Sections" parameter.
procedure CreateList(PathName: String; const Sections : TArrayOfString);
var
  I : Integer;
  Section : String;
  Data : RawByteString;
  Y : String;
begin
  TableCode:='';
  TableHigh:=-1;
  TableCount:=0;
  OutName:=DosFileName(PathName);
  LogMessage(vbNormal, 'Group: ' + PathName + SPACE + '(' + OutName + ')');
  SectionKind:=FindSectionKind(PathName);
  BaseDir:=DirSource + PathName + PathDelimiter;
  CreateCommentFilesList(BaseDir);
  ReadFile(BaseDir + HeaderFile, HeaderData);
  // Patch Header Year if needed
  Y:=FormatDateTime('YYYY', BuildTime);
  if (Pos('The List Project', HeaderData) > 0) and (Pos(Y + SPACE + 'The List Project', HeaderData) = 0) then begin
    LogMessage(vbNormal, TAB + 'Automatic patch of The List Project copyright dates.');
    HeaderData:=StringReplace(HeaderData, SPACE + 'The List Project',
     '-' + Y + SPACE + 'The List Project', []);
  end;
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
    if Section = 'ADMIN' then begin
      if TableHigh = -1 then Continue;
      Data:='Highest Table Number = ' + TableCode + ZeroPad(TableHigh, 4);
      LogMessage(vbNormal, TAB + Data);
    end else
    if Section <> 'FILELIST' then begin
      if not ReadFile(BaseDir + UNDERSCORE + Sections[I] + SrcExt, Data) then
        Continue;
      Data:=NormalizeLineEndings(Data, leCRLF);
    end else begin
      if ReadFile(BaseDir + UNDERSCORE + Sections[I] + SrcExt, Data) then
        Data:=NormalizeLineEndings(Data, leCRLF) + CRLF
      else
        Data:='';
      Cat(Data, CreateFileList);
    end;
    if Data = '' then Continue;
    LogMessage(vbExcessive, TAB+'Added comment section: ' + Section);
    Cat(WorkingData, SectionMarker(Section));
    Cat(WorkingData, Data);
  end;
  if WorkingData <> '' then
    WriteFile(PathName, OutName, Header + WorkingData);
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
    AddFileInfo(L[I], L[I]);
    E:=FileCopy(DirSource + 'Miscellaneous' + PathDelimiter + L[I], DirOutput + DosFileName(L[I]));
    if E <> 0 then begin
      LogMessage(vbCritical, 'Error #'+ IntToStr(E) + ' while trying to copy "' +
        DirSource + 'Miscellaneous' + PathDelimiter + L[I] + '" to "' +
        DirOutput + DosFileName(L[I]) + '"');
      Halt(1);
    end;
  end;
end;

// Build a Release of The List
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
  TableTree := TBinaryTree.Create;
  CommentFiles:=TStringList.Create;
  FileInfo := TBinaryTree.Create;
  BuildTime:=Now;
  if LegacyMode then
    LastChange:=Lowercase(FormatDateTime('ddmmmYY', BuildTime))
  else
    LastChange:=Lowercase(FormatDateTime('ddmmmYY hh:nn', BuildTime));
  TitleWidth:=0;
  for I := 0 to High(DOSNAMES) do
     if Length(DOSNAMES[I].Name) > TitleWidth then
       TitleWidth:=Length(DOSNAMES[I].Name);
  Inc(TitleWidth);
  CopyMiscellaneousFiles;
  CreateListFiles;
  CommentFiles.Free;
  TableTree.Free;
  FileInfo.Free;
  SectionTree.Free;
end;

{ ---------------------------------------------------------------------------- }

function KindFromText(S : String) : TSectionKind;
begin
  case LowerCase(Trim(S)) of
    'none'       : Result:=skNone;
    'standard'   : Result:=skStandard;
    'interrupt'  : Result:=skInterrupt;
    'cmos'       : Result:=skCMOS;
    'farcall'    : Result:=skFarCall;
    'i2c'        : Result:=skI2C;
    'memory'     : Result:=skMemory;
    'msr'        : Result:=skMSR;
    'port'       : Result:=skPort;
    'glossary'   : Result:=skGlossary;
    'table'      : Result:=skTable;
    'smm'        : Result:=skSMM;
    'link'       : Result:=skLink;
    'faq'        : Result:=skFAQ;
  else
    Result:=skNone;
    raise Exception.Create('Section Type "'+Trim(S)+'" was not recognized.');
  end;
end;

procedure ReadFileMap;
{ TODO 2 -cDevel Remove a lot of duplicate and similar code }
var
  INI : TIniFile;
  Strs : TStringList;
  K, S : String;
  I, J : Integer;

  procedure ReadSection(SectName : String);
  var
    X : Integer;
  begin
    INI.ReadSectionRaw(SectName, Strs);
    // Blanks are already removed. Remove Comments.
    for X := Strs.Count - 1 downto 0 do
       if HasLeading(Strs[X], ';', false) then
         Strs.Delete(X);
  end;

begin
  if not FileExists(DirSource + MapFile) then Exit;
  INI:=nil;
  Strs:=nil;
  try
    Strs:=TStringList.Create;
    INI := TIniFile.Create(DirSource + MapFile);
    // Replace Miscellaneous Directory when defined
    S:= ExcludeTrailing(INI.ReadString('MISC', 'Directory', MiscFileDir), PathDelimiter);
    if not DirectoryExists(DirSource + S) then begin
      LogMessage(vbMinimal, 'Invalid directory for miscellaneous files: ' + S);
      Halt(1);
    end else
      MiscFileDir:=S;
    // Replace INTERRUPT_1ST sections, replaces internal table
    ReadSection('1ST');
    if Strs.Count > 0 then begin
      SetLength(INTERRUPT_1ST, Strs.Count);
      for I := 0 to Strs.Count - 1 do begin
        S:=Strs[I];
        PopDelim(S, EQUAL);
        S:=Trim(S);
        INTERRUPT_1ST[I]:=Trim(S);
      end;
    end;
    // Replace SECTION_ORDER sections, replaces internal table
    ReadSection('ORDER');
    if Strs.Count > 0 then begin
      SetLength(SECTION_ORDER, Strs.Count);
      for I := 0 to Strs.Count - 1 do begin
        S:=Strs[I];
        PopDelim(S, EQUAL);
        S:=Trim(S);
        SECTION_ORDER[I]:=Trim(S);
      end;
    end;
    // Update File/Path Name mappings, updates and appends internal table
    ReadSection('NAMES');
    if Strs.Count > 0 then begin
      for I := 0 to Strs.Count - 1 do begin
        S:=Strs[I];
        K:=Trim(PopDelim(S, EQUAL));
        S:=Trim(S);
        if S = '' then Continue;
        for J := 0 to High(DOSNAMES) do
          if UpperCase(DOSNAMES[J].DOS) = UpperCase(K) then begin
            if Uppercase(K) = FILE_1ST then
               IntFirstDir:=S;
            DOSNAMES[J].Name:=S;
            K:='';
            Break;
          end;
        if K = '' then Continue;
        SetLength(DOSNAMES, Length(DOSNAMES) + 1);
        DOSNAMES[High(DOSNAMES)].DOS:=K;
        DOSNAMES[High(DOSNAMES)].Name:=S;
        DOSNAMES[High(DOSNAMES)].Kind:=skNone;
      end;
    end;
    // Update File/Path Name TYPE, updates and appends internal table
    ReadSection('TYPE');
    if Strs.Count > 0 then begin
      for I := 0 to Strs.Count - 1 do begin
        S:=Strs[I];
        K:=Trim(PopDelim(S, EQUAL));
        S:=Trim(S);
        if S = '' then Continue;
        for J := 0 to High(DOSNAMES) do
          if UpperCase(DOSNAMES[J].DOS) = UpperCase(K) then begin
            DOSNAMES[J].Kind:=KindFromText(S);
            Break;
          end;
      end;
    end;
    // Update File Description Information for FILELIST, , updates and appends
    // internal table
     ReadSection('INFO');
     if Strs.Count > 0 then begin
       for I := 0 to Strs.Count - 1 do begin
         S:=Strs[I];
         K:=Trim(PopDelim(S, EQUAL));
         S:=Trim(S);
         if S = '' then Continue;
         for J := 0 to High(DOSINFO) do
           if UpperCase(DOSINFO[J].Name) = UpperCase(K) then begin
             DOSINFO[J].Text:=S;
             K:='';
             Break;
           end;
         if K = '' then Continue;
         SetLength(DOSINFO, Length(DOSINFO) + 1);
         DOSINFO[High(DOSINFO)].Name:=K;
         DOSINFO[High(DOSINFO)].Text:=S;
       end;
     end;

  except
    on E : Exception do begin
      LogMessage(vbCritical, 'Mapping file exception: ' + E.Message);
      Halt(1);
    end;
  end;
  if Assigned(INI) then INI.Free;
  if Assigned(Strs) then Strs.Free;
end;

// We do not really need to include the Lazarus Resourse in the executable.
// But, Lazarus keeps sticking it back in here. So, whatever, its only a couple
// of bytes.
{$R *.res}

begin
  TotalTables:=0;
  TotalEntries:=0;
  TotalErrors:=0;
  TotalProblems:=0;
  TotalWarnings:=0;
  TotalDuplicates:=0;
  HeaderBar:=StringOf('-', 70);
  Options;
  if (Issues <> '') and (FileExists(Issues)) then
     DeleteFile(Issues);
  ReadFileMap;
  Banner;
  Build;
  if IssueOpen then begin
    System.Close(IssueF);
    IssueOpen:=False;
  end;
  LogMessage(vbNormal, '');
  LogMessage(vbNormal, 'Total Number of Entries:    ' + IntToStr(TotalEntries));
  LogMessage(vbNormal, 'Total Number of Tables:     ' + IntToStr(TotalTables));
  if TotalDuplicates > 0 then
    LogMessage(vbNormal, 'Total Number of Duplicates: ' + IntToStr(TotalDuplicates));
  if TotalWarnings > 0 then
    LogMessage(vbMinimal, 'Total Number of Warnings:   ' + IntToStr(TotalWarnings));
  if TotalProblems > 0 then
    LogMessage(vbMinimal, 'Total Number of Problems:   ' + IntToStr(TotalProblems));
  if TotalErrors > 0 then
    LogMessage(vbMinimal, 'Total Number of Errors:     ' + IntToStr(TotalErrors));
  if CICD then begin
    if (TotalProblems > 0) or (TotalErrors > 0)then begin
      LogMessage(vbCritical, 'Done, CI/CD verification error.');
      Halt(1);
    end;
    LogMessage(vbMinimal, 'Done, verified for CI/CD.');
  end else
    LogMessage(vbMinimal, 'Done.');
end.

