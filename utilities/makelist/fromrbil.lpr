// Copyright 2025-2026, Jerome Shidel
// BSD 3-Clause License


// Note: This is a One-Off program to break down the Existing RBIL 61 into
// a file and directory structure more suitable for group editing using
// a version control system like Git. Once that conversion has been completed,
// this program will be retired and no longer maintained.

program fromrbil;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Version information defines

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils
  { you can add units after this },
  Version, PasExt;

{$R *.res}

const
  SRC='../../rbil61/';  // path to original RBIL files
  DST='../../source/';  // path to output conversion

  SectCount: integer= 0;

  // List and order of file (groups) to process
  LST : array of string = (
    'CATEGORY.KEY',
    'GLOSSARY.LST',
    'OVERVIEW.LST',
    'INTERRUP.1ST',
    'INTERRUP.A',
    'PORTS.A',
    'CMOS.LST',
    'FARCALL.LST',
    'I2C.LST',
    'MEMORY.LST',
    'MSR.LST',
    'BIBLIO.LST',
    'TABLES.LST',
    // these need handled a little differently
    // 'OPCODES.LST', // Excluded for possbile copyright issues.
    // '86BUGS.LST', // Excluded for possbile copyright issues.
    'SMM.LST'

  );

  // List of files to simply copy
  TXT : array of record
    S, D : String;
  end = (
    (S:'_ADVERT.TXT';  D:'Advertisement.txt'),
    (S:'INTERRUP.PRI'; D:'Interrupt Primer.txt'),
    (S:'NEEDHELP.TXT'; D:'Need Help.txt'),
    (S:'RBROWN.TXT';   D:'Ralf Brown.txt'),
    (S:'README.NOW';   D:'Read Me Now.txt'),
    (S:'faq.lst';      D:'FAQ.txt')
  );

  { Section flags }
  FLAGS : array of record
    F, D : String;
  end = ();

  CATEGORIES : array of record
    C, D : String;
  end = (
    (C:'!'; D:'Notes; Comments; Other information')
  );

  INT_TITLES : TArrayOfRawByteString = ();

type
  TSectionStyle = (ssNormal, ssGlossary, ssInterrupts, ssTables, ssSMM, ssMSR,
  ssI2C, ssMemory, ssCMOS, ssFarCall, ssPorts);

  TParseStyle = (psIgnore, psHeader, psComment, psPlain, psCategories, psFlags,
    psCategoryKeys, psAbbreviations, psGlossary, psTitles,
    psInterrupts);

{ Appends a String to a text file }
function AppendToFile(AFileName: String; AValue: String; ARaise: boolean
    ): integer;
  var
     T : Text;
     R, E : integer;
  begin
    System.Assign(T, AFileName);
    if FileExists(AFileName) then
      Append(T)
    else
      Rewrite(T);
    R := IOResult;
    if R = 0 then begin
      WriteLn(T, AValue);
      R := IOResult;
      Close(T);
      E := IOResult;
      if R = 0 then R := E;
    end;
    Result := R;
    if (R <> 0) and ARaise then
      raise exception.Create('file "' + AFileName + '" save error #' + IntToStr(R));
  end;

{ Filter any character which is not a letter or number out of a string }
function AlphaNumOnly(AStr: String; Substitute: String =''; AllowSpace : boolean = false): String;
var
  I : integer;
begin
  Result := '';
  for I := 1 to length(AStr) do
    if (AStr[I] in [#$30..#$39,#$41..#$5A,#$61..#$7A])
    or ((AStr[I]=#$20) and AllowSpace) then
      Result := Result + AStr[I]
    else
      Result:=Result + Substitute;
end;

{ Simple function to Increment a Character, returns A-Z or b-z }
function IncAlpha(AValue : String) : String;
var
  C : Char;
  U : Boolean;
begin
  IncAlpha:='';
  if AValue = '' then begin
    IncAlpha:='A';
    Exit;
  end;
  U := UpperCase(AValue) = AValue;
  if not U then AValue:=LowerCase(AValue);
  C := AValue[Length(AValue)];
  if (C = 'Z') or (C = 'z') then Exit;
  IncAlpha := Copy(AValue, 1, Length(AValue) - 1) + Char(Byte(C)+1);
end;

{ Creates a directory if it does not exist }
procedure InsureDir(DirName : String);
begin
  if not DirectoryExists(DirName) then
    if not CreateDir(DirName) then
      raise exception.Create('Unable to create "' + DirName + '" directory.');
end;

{ Returns the section tite for a file in a TitleCase }
function GetFileTitle(FileName, S : String) : String;
var
  Title : String;
begin
  Title := LowerCase(StringReplace(S, TAB, SPACE, [rfReplaceAll]));
  Title := StringReplace(Title, 'release 61', SPACE, [rfReplaceAll]);
  Title := PopDelim(Title, COMMA);
  Title := Trim(PopDelim(Title, SPACE + SPACE));
  if (Title = '') or (Title[1] = '[') then Title:=LowerCase(
    Copy(FileName,1, Length(FileName) - Length(ExtractFileExt(FileName))));
  Result:=TitleCase(Title);
end;

{ Returns the timestamp for a file }
function GetFileStamp(FileName : String) : LongInt;
var
  DT : TDateTime;
begin
  if not FileAge(FileName, DT) then
    raise Exception.Create(FileName +': error reading source timestamp');
  Result:=DateTimeToFileDate(DT);
end;

function CombineSpaces(const S : RawByteString) : RawByteString; overload;
var
  T : RawByteString;
begin
  T:=S;
  repeat
    Result:=T;
    T:=StringReplace(T, SPACE + SPACE, SPACE, [rfReplaceAll]);
  until Result=T;
end;


{ Global variables and constants used during processing a LST file }
const
  ItemBreak = '--------';        // String that separates parts of the LST file
  HeadPadding = 12;              // Left Padding of fields shown in some file headers
  NoneString = 'n/a';            // When no flags or category for LST entry
  TextExt = '.txt';
  _Header = '_Header' + TextExt; // Startng output "Header" file name
  LineEnding = CRLF;             // Line ending used for text file output

var
  FileTitle : String;            // Current Input file Title
  FileStamp : LongInt;           // File stamp of original LST file
  InStrs  : TStringList;         // Original File Text data
  OutPath : String;              // Output file path
  OutFile : String;              // Output file name
  OutStr  : String;              // Output file String
  SectionStyle: TSectionStyle;   // Type of Sections for a file
  ParseStyle : TParseStyle;      // Type of section that is being parsed
  Index : integer;               // Current Line position of InStrs
  SectionBreak : boolean;        // Processing flag for end of a file section
  SectionTitle : String;         // Section Title
  SectionCategory : String;
  SectionFlags : String;
  UniqueID: String;

function FilterFlags(S : String) : String;
var
  T : String;
begin
  Result:=S;
  S:=StringReplace(S, TAB, SPACE8, [rfReplaceAll]);
  SectionFlags:='';
  T:=Trim(PopDelim(S, ' - '));
  SectionFlags:=CutDelim(T, SPACE, 3,3);
  if SectionFlags <> '' then
    Result:=CutDelim(T, SPACE, 1,2) + ' - ' + S;
end;

function GetCategoryInfo : String;
var
  I : Integer;
  S : String;
begin
  Result:='';
  for I := Low(CATEGORIES) to High(CATEGORIES) do
    if SectionCategory = CATEGORIES[I].C then begin
      S:=CATEGORIES[I].D;
      Break;
    end;
  if S = '' then
    for I := Low(CATEGORIES) to High(CATEGORIES) do
      if '*' = CATEGORIES[I].C then begin
        S:=CATEGORIES[I].D;
        Break;
      end;
  if S = '' then S:='Unknown Category';
  Result:=Leftpad('Category: ',HeadPadding) + SectionCategory + SPACE+TAB + S + LF;
end;

function GetFlagInfo : String;
var
  I, P : Integer;
  S : String;
begin
  Result:='';
  if Length(SectionFlags) = 0 then
    if SectionFlags = '' then begin
      Result:=LeftPad('Flag: ', HeadPadding) + NoneString + LF;
      Exit;
    end;
  for P := 1 to Length(SectionFlags) do begin
    S:='';
    for I := Low(FLAGS) to High(FLAGS) do
      if SectionFlags[P] = FLAGS[I].F then begin
        S:=FLAGS[I].D;
        Break;
      end;
    if S = '' then S:='Unknown Flag';
    Result:=LeftPad('Flag: ', HeadPadding) + SectionFlags[P] + SPACE+TAB + S + LF;
  end;
end;

function GetIntOutFile(S : String) : String;
var
  T : String;
  V, E : Integer;
begin
  Result:='';
  T:=CutDelim(S, SPACE, 2, 2);
  Val('$' + T, V, E);
  Result:=CombineSpaces(AlphaNumOnly(CutDelim(S, SPACE, 1, 2), '', True));
  if (E=0) and (V>=0) and (V<=255) then
    Cat(Result, SPACE + CombineSpaces(AlphaNumOnly(INT_TITLES[V], '', True)));
  if Result <> '' then
    Result:=IncludeTrailingPathDelimiter(Trim(Result));
  Cat(Result, 'INT ' + Trim(CombineSpaces(AlphaNumOnly(
    StringReplace(UniqueID, '-', '_', [rfReplaceAll]) +
    SPACE + CutDelim(S, SPACE, 3), '', True))) + TextExt);
end;

function MakeHeader(Line : integer) : String;
begin
  Result:=Copy(InStrs[Line], 10);
  While HasLeading(Result, '-') do Result:=ExcludeLeading(Result, '-');
  While HasTrailing(Result, '-') do Result:=ExcludeTrailing(Result, '-');
  UniqueID:=Result;
  Result:=StringOf('-', 80) + LF + LeftPad('Unique ID: ', HeadPadding) + Result + LF;
  Cat(Result, GetCategoryInfo);
  Cat(Result, GetFlagInfo);
  case SectionStyle of
    ssMSR : begin end;
  end;
  Result:=Result + StringOf('-', 80) + LF + LF;
end;

procedure SaveSection; forward;

{ determine if a line is the end of a section }
function EndOfSection(Line : integer) : boolean;
begin
  Result:=Line >= InStrs.Count; // Initially set if at end of file
  if not Result then
    Result :=Copy(InStrs[Line], 1, Length(ItemBreak)) = ItemBreak;
  if not Result then begin
    case ParseStyle of
      psGlossary : Result:=Trim(InStrs[Line]) = '';
      psHeader : begin
        if FileTitle = 'Glossary' then // FileTitle is in TitleCase
          Result:=Trim(InStrs[Line]) = '';
      end
    end;
  end;
end;

{ Reformats the Category Keys and Flag Listings, Break Each item onto it's
  own line. For the Search Categories from the Categor.lst file, it also
  breaks down search terms to an indented line unde the first term. }
procedure PostProcessCategories;
var
  Pre, S, T : String;
  A : TArrayOfString;
  I, CatStart, FlagStart : Integer;
begin
  CatStart:=Length(CATEGORIES);
  FlagStart:=Length(FLAGS);
  if ParseStyle=psFlags then begin
    Pre:=PopDelim(OutStr, COLON + SPACE + SPACE);
    if OutStr = '' then begin
      OutStr:=Pre;
      Pre:='';
    end else
      Cat(Pre, COLON + LF + LF);
  end else begin
    Pre:=PopDelim(OutStr, COLON + LF);
    if OutStr = '' then begin
      OutStr:=Pre;
      Pre:='';
    end else
      Cat(Pre, COLON + LF + LF);
  end;
  OutStr:=StringReplace(OutStr, ', etc', '! etc', [rfReplaceAll]);
  OutStr:=StringReplace(OutStr, COMMA, LF, [rfReplaceAll]);
  OutStr:=StringReplace(OutStr, LF+LF, LF, [rfReplaceAll]);
  A:=Explode(OutStr);
  OutStr:=Pre;
  for I := 0 to High(A) do begin
    S:=Trim(StringReplace(A[I], TAB, SPACE, [rfReplaceAll]));
    S:=StringReplace(S, '! etc', ', etc', [rfReplaceAll]);
    T:=S;
    if Pre <> '' then
      S:=Tab+S;
    S:=StringReplace(S, ';', LF + WhenTrue(Pre <> '', TAB) + SPACE4, [rfReplaceAll]);
    Cat(OutStr,S + LF);
    if ParseStyle = psCategories then begin
      S:=PopDelim(T, '-');
      SetLength(CATEGORIES, Length(CATEGORIES) + 1);
      CATEGORIES[High(CATEGORIES)].C:=Trim(S);
      CATEGORIES[High(CATEGORIES)].D:=Trim(T);
    end;
    if ParseStyle = psFLags then begin
      S:=PopDelim(T, '-');
      SetLength(FLAGS, Length(FLAGS) + 1);
      FLAGS[High(FLAGS)].F:=Trim(S);
      FLAGS[High(FLAGS)].D:=Trim(T);
    end;
  end;
  if ParseStyle = psCategories then begin
    WriteLn('Added ', Length(CATEGORIES) - CatStart, ' category classifications');
  end;
  if ParseStyle = psFlags then begin
    WriteLn('Added ', Length(FLAGS) - FlagStart, ' flag classifications');
  end;
end;

{ Each Abbrevaition gets broken into it's own file. These are also relocated
  from within the Interrupt List into thier own Abrreviations root directory. }
procedure PostProcessAbbreviations;
var
  HoldPath : String;
  Lines: TArrayOfRawByteString;
  I, C : Integer;
  T : String;
begin
  ParseStyle:=psPlain;
  HoldPath:=OutPath;
  Outpath:=DST+IncludeTrailingPathDelimiter(Trim(AlphaNumOnly(SectionTitle, '', True)));
  OutStr:=StringReplace(OutStr, TAB, SPACE8, [rfReplaceAll]);
  T:=PopDelim(OutStr, LF + SPACE8);
  if OutStr = '' then
    Lines:=Trim(Explode(T))
  else begin
    Lines:=Trim(Explode(OutStr));
    OutStr:=T;
    OutFile:=_Header;
    SaveSection;
  end;
  C:=0;
  for I := 0 to High(Lines) do begin
    T:=Trim(PopDelim(Lines[I], SPACE));
    OutStr:=T + ' - ' + Trim(Lines[I]);
    if Lines[I] = '' then Continue;
    OutFile:=Trim(AlphaNumOnly(T)) + TextExt;
    SaveSection;
    Inc(C);
  end;
  WriteLn('Added ', C, ' abbreviation entries');
  OutPath:=HoldPath;
  ParseStyle:=psIgnore;
end;


{ Populates the INT_TITLES array }
procedure PostProcessTitles;
var
  I : Integer;
  T : TArrayOfRawByteString;
  N : String;
  V, E : integer;
begin
  SetLength(INT_TITLES, 256);
  for I := 0 to High(INT_TITLES) do
    INT_TITLES[I]:='';
  T:=Trim(Explode(OutStr));
  for I := 0 to High(T) do begin
    if T[I] = '' then Continue;
    N:=PopDelim(T[I], '-');
    PopDelim(N, SPACE);
    Val('$'+Trim(N), V, E);
    if (E<>0) or (V>255) or (V<0) then begin
       WriteLn('ERROR: Invalid Interrupt number ', Trim(N), ' for title');
       Continue;
    end;
    INT_TITLES[V]:=Trim(T[I]);
  end;
end;

{ Write Section OutData to new file or append to existing file }
procedure SaveSection;
var
  P : String;
begin
  if ParseStyle <> psIgnore then begin
    if (OutFile <> '') and (OutStr<>'') then begin
      Inc(SectCount);
      case ParseStyle of
        psGlossary : begin end;
        psFlags,
        psCategories, psCategoryKeys : PostProcessCategories;
        psAbbreviations : PostProcessAbbreviations;
        psTitles : PostProcessTitles;
      end;
      if ParseStyle <> psIgnore then begin
        P:=ExtractFilePath(OutFile);
        InsureDir(OutPath);
        if P <> '' then InsureDir(OutPath + P);
        if FileExists(OutPath + OutFile) then begin
          WriteLn('Warning: Appending existing file: ' + OutFile);
          AppendToFile(OutPath + OutFile, StringOf('-', 80) + LineEnding, true);
        end;
        AppendToFile(OutPath + OutFile, NormalizeLineEndings(OutStr, LineEnding), true);
        if FileSetDate(OutPath + OutFile, FileStamp) <> 0 then
          raise Exception.Create('error writing timestamp: ' + OutPath + OutFile);
      end;
    end;
  end else begin
    if SectionTitle <> '' then
      WriteLn('ignored section: ', SectionTitle);
  end;
  OutFile:='';
  OutStr:='';
  SectionTitle:='';
end;

{ Process a normal or other Section Header. }
procedure SectionHead;
var
  S : String;
begin
  SectionCategory:=ExcludeTrailing(Copy(InStrs[Index], 9, 2), '-');
  // Not a Section Comment, so need to do something else with it.
  case SectionStyle of
    ssGlossary, ssTables : begin
      ParseStyle:=psGlossary;
      // Skip blank lines;
      while (Index < InStrs.Count) and (Trim(InStrs[Index]) = '') do Inc(Index);
      OutStr:=Trim(InStrs[Index]) + LF + LF;
      S:=Trim(InStrs[Index]);
      // Remove "See" stuff from ssTables File Names
      if (SectionStyle=ssTables) and (Pos('(see', S) > 0) then
        S:=CutDelim(S, '(see', 1, 1);
      OutFile:=Trim(AlphaNumOnly(S, '', true));
      // If end of file, we are done
      if (OutFile = 'end of file') or (OutFile = 'endoffile') then
        ParseStyle:=psIgnore
      else begin
        Cat(OutFile, TextExt);
        Inc(Index);
      end;
    end;
    ssMSR, ssI2C, ssMemory, ssCMOS, ssFarCall, ssPorts, ssInterrupts : begin
      ParseStyle:=psPlain;
      if (Index < InStrs.Count - 1) then begin
        // Separate out Flags from Title
        S:=FilterFlags(InStrs[Index+1]);
        SectionTitle:=Trim(CombineSpaces(AlphaNumOnly(StringReplace(
           StringReplace(S, '-', '', [rfReplaceAll]), '_',
           SPACE, [rfReplaceAll]), ' ', True)));
        if SectionTitle = '' then begin
          SectionTitle:='Note';
          OutFile:='_' + SectionTitle + TextExt;
          ParseStyle:=psComment;
          OutStr:='';
          Inc(Index);
          Exit;
        end;

        OutStr:=MakeHeader(Index);

        OutFile:=SectionTitle + TextExt;
        if SectionStyle = ssInterrupts then begin
          ParseStyle:=psInterrupts;
          OutFile:=GetIntOutFile(S);
        end;

        Inc(Index);
      end;
      // WriteLn(SectionTitle);
    end;
  end;
  // if Copy(SectionFlags,2,1) <> '-' then
  //  WriteLn(S);
  // WriteLn(S);

end;

 { Process a Comment Header Section }
procedure CommentHead;
begin
  // A comment, note, etc.
   SectionTitle:= TitleCase(LowerCase(StringReplace(
     StringReplace(Copy(InStrs[Index], 10), '-', '', [rfReplaceAll]), '_',
     SPACE, [rfReplaceAll])));

   case SectionStyle of
     ssTables : begin
       // Tables are wrapped in an unnamed comment. So, we can swtich modes.
       // And now process them like Glossary Entries.
       if (SectionTitle='') or (SectionTitle='Tables') then begin
         ParseStyle:=psGlossary;
         OutFile:='';
         // Because the Glossary processor ingnore the first block of text,
         // exit here so it is not incremented below.
         Exit;
       end;
     end;
   end;

   case SectionTitle of
     // Sections that are ignored for various reasons. Mostly, because it is
     // duplicate information, obsolete or created through other means.
     // 'Filelist'     : ParseStyle:=psIgnore; {it will need pruned a lot}
     // Gets reformated and used for Category names
     'Categories'   : ParseStyle:=psCategories;
     // Gets reformated
     'Categorykeys' : begin
       SectionTitle:='Category Keys';
       ParseStyle:=psCategoryKeys
     end;
     // Gets reformated
     'Flags' : ParseStyle:=psFlags;
     // Gets Reformated and busted up.
     'Abbreviations' : ParseStyle:=psAbbreviations;
     'Titles' : ParseStyle:=psTitles;
   end;

   OutFile:='_' + SectionTitle + TextExt;

   Inc(Index);
end;

procedure NullHead;
begin
  SectionTitle:='';
  OutStr:='';
  case SectionStyle of
    ssSMM : begin
      Inc(Index);
      if (Index < InStrs.Count) then begin
        SectionTitle:=StringReplace(
           StringReplace(InStrs[Index], '-', '', [rfReplaceAll]), '_',
           SPACE, [rfReplaceAll]);
        OutFile:=SectionTitle + TextExt;
      end;
      OutStr:='';
    end;
  else
    SectionHead;
  end;
end;

procedure ParseSectionHead;
begin
  ParseStyle:=psPlain;
  SectionTitle:='';
  OutFile:='';
  OutStr:='';
  if Index >= InStrs.Count then Exit;
  if Copy(InStrs[Index],9,2) = '!-' then
    CommentHead
  else if Copy(InStrs[Index],9,2) = '--' then
    NullHead
  else
    SectionHead;
  if ParseStyle = psIgnore then
    OutFile:='';
end;

{ Main process to convert a single file }
procedure ProcessLST(FileName : String);
begin
  case UpperCase(CutDelim(FileName, '.', 1, 1)) of
    'GLOSSARY' : SectionStyle:=ssGlossary;
    'INTERRUP' : SectionStyle:=ssInterrupts;
    'TABLES'   : SectionStyle:=ssTables;
    'SMM'      : SectionStyle:=ssSMM;
    'MSR'      : SectionStyle:=ssMSR;
    'I2C'      : SectionStyle:=ssI2C;
    'MEMORY'   : SectionStyle:=ssMemory;
    'CMOS'     : SectionStyle:=ssCMOS;
    'FARCALL'  : SectionStyle:=ssFarCall;
    'PORTS'    : SectionStyle:=ssPorts;
  else
    SectionStyle:=ssNormal;
  end;
  FileStamp:=GetFileStamp(SRC+FileName);
  InStrs := TStringList.Create;
  InStrs.LoadFromFile(SRC+FileName);
  FileTitle:=GetFileTitle(FileName, InStrs[0]);
  WriteLn('Processing: ', FileName, ', ', InStrs.Count, ' lines, ', FileTitle);
  OutPath:=DST + FileTitle;
  InsureDir(OutPath);
  OutPath:=IncludeTrailingPathDelimiter(OutPath);
  OutFile:=_Header;
  ParseStyle:=psHeader;
  // First Line of file is usually a simple header that we will ignore;
  // But, sometimes it is a reference we will  want include.
  OutStr:=InStrs[0] + LF;
  if LowerCase(OutStr).Contains('release 61') then OutStr:='';
  // Starting with the second line in the file. Continue processing Header
  // and other sections.
  Index:=1;
  While Index < InStrs.Count do begin
    Cat(OutStr, InStrs[Index] + LF);
    Inc(Index);
    SectionBreak:=EndOfSection(Index);
    if SectionBreak then begin
      SaveSection;
      ParseSectionHead;
    end;
  end;
  FreeAndNil(InStrs);
end;

{  Procedure to run through the files that need conversion }
procedure ConvertLST(FileName : String);
begin
  if not FileExists(SRC + FileName) then begin
    WriteLn('List "', FileName, '" is not present.');
    Exit;
  end;
  while FileExists(SRC + FileName) do begin
    ProcessLST(FileName);
    if Length(ExtractFileExt(FileName)) = 2 then begin
      FileName:= Copy(FileName, 1, Length(FileName) - 2) + '.' +
        IncAlpha(Copy(FileName, Length(FileName)));
    end else Break;
  end;
end;

{ Copies the Text files hat are listed in the TXT array }
procedure CopyTextDocs;
var
  I : integer;
begin
  for I := Low(TXT) to High(TXT) do begin
    if not FileExists(SRC + TXT[I].S) then begin
      WriteLn('Document "', TXT[I].S, '" is not present.');
      Continue;
    end;
    if not DirectoryExists(DST + 'Miscellaneous') then
      if not CreateDir(DST + 'Miscellaneous') then begin
        WriteLn('Unable to create "', DST, 'Miscellaneous" directory.');
        Halt(1);
      end;
    if FileCopy(SRC + TXT[I].S, IncludeTrailingPathDelimiter(DST + 'Miscellaneous') +
    TXT[I].D) <> 0 then begin
      WriteLn('Error copying "', TXT[I].S, '" document to "', TXT[I].D, '"');
      Halt(1);
    end;
    WriteLn('Copied "', TXT[I].S, '" document to "',
    IncludeTrailingPathDelimiter('Miscellaneous') + TXT[I].D, '"');

  end;

end;

{ procedure to call all of the processes for the conversion }
procedure ConvertAll;
var
  I : Integer;
begin
  if not DirectoryExists(SRC) then begin
    WriteLn('Directory "', SRC, '" with original RBIL files was not found.');
    Halt(1);
  end;
  if DirectoryExists(DST) then begin
    WriteLn('Delete current "TheList" sources directory.');
    if not DeleteTree(DST) then begin
      WriteLn('Failed to remove directory.');
      Halt(1);
    end;
  end;
  WriteLn('Create new "TheList" sources directory.');
  if not CreateDir(DST) then begin
    WriteLn('Failed to create directory.');
    Halt(1);
  end;
  CopyTextDocs;
  for I := Low(LST) to High(LST) do
    ConvertLST(LST[I]);
  WriteLn('Total sections: ', SectCount);

end;

{ Conversion program banner text }
procedure Banner;
begin
  { Program Information Banner }
  WriteLn(APP_PRODUCTNAME, ' v', APP_VERSION);
  WriteLn('Copyright ', APP_LEGALCOPYRIGHT);
  WriteLn('BSD 3-Clause License');
  WriteLn;
end;

begin
  Banner;
  ConvertAll;
end.

