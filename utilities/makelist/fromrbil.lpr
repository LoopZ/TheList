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
    'INTERRUP.1ST',
    'INTERRUP.A',
    'PORTS.A',
    'CMOS.LST',
    'FARCALL.LST',
    'I2C.LST',
    'MEMORY.LST',
    'MSR.LST',
    'OVERVIEW.LST',
    // these need handled a little differently
    'GLOSSARY.LST'
    // 'BIBLIO.LST'
    // 'OPCODES.LST'
    // 'TABLES.LST'
    // '86BUGS.LST'
    // 'SMM.LST'

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

  FLAGS : array of record
    F, D : String;
  end = (
    (F:'U'; D:'undocumented function'),
    (F:'u'; D:'partially documented function'),
    (F:'P'; D:'available only in protected mode'),
    (F:'R'; D:'available only in real or V86 mode'),
    (F:'C'; D:'callout or callback (usually hooked rather than called)'),
    (F:'O'; D:'obsolete (no longer present in current versions)')
  );

  CATEGORIES : array of record
    C, D : String;
  end = (
    (C:'!'; D:'Notes; Comments; Other information')
  );

type

  TParseStyle = (psIgnore, psHeader, psComment, psCategories, psCategoryKeys,
    psNormal, psGlossary);

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

{ Global variables and constants used during processing a LST file }
const
  ItemBreak = '--------';        // String that separates parts of the LST file
  OFN_Header = '_Header.txt';    // Startng output "Header" file name
  LineEnding = CRLF;             // Line ending used for text file output

var
  FileTitle : String;            // Current Input file Title
  FileStamp : LongInt;           // File stamp of original LST file
  InStrs  : TStringList;         // Original File Text data
  OutPath : String;              // Output file path
  OutFile : String;              // Output file name
  OutStr  : String;              // Output file String
  ParseStyle : TParseStyle;      // Type of section that is being parsed
  Index : integer;               // Current Line position of InStrs
  SectionBreak : boolean;        // Processing flag for end of a file section
  SectionTitle : String;         // Section Title

{ determine if a line is the end of a section }
function EndOfSection(Line : integer) : boolean;
begin
  Result:=Line >= InStrs.Count; // Initially set if at end of file
  if not Result then begin
    case ParseStyle of
      psGlossary : Result:=Trim(InStrs[Line]) = '';
      psHeader : begin
        if FileTitle = 'Glossary' then // FileTitle is in TitleCase
          Result:=Trim(InStrs[Line]) = ''
        else
          Result :=Copy(InStrs[Line], 1, Length(ItemBreak)) = ItemBreak;
      end
    else
        Result :=Copy(InStrs[Line], 1, Length(ItemBreak)) = ItemBreak;
    end;
  end;
end;

procedure PostProcessGlossary;
begin
  { nothing to do here, move along }
end;

{ Reformats the category keys }
procedure PostProcessCategories;
var
  Pre, S, T : String;
  A : TArrayOfString;
  I, CatStart : Integer;
begin
  CatStart:=Length(CATEGORIES);
  Pre:=PopDelim(OutStr, COLON + LF);
  if OutStr = '' then begin
    OutStr:=Pre;
    Pre:='';
  end else
    Cat(Pre, COLON + LF + LF);
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
  end;
  if ParseStyle = psCategories then begin
    WriteLn('Added ', Length(CATEGORIES) - CatStart, ' category classifications');
  end;
end;

{ Write Section OutData to new file or append to existing file }
procedure SaveSection;
begin
  if ParseStyle <> psIgnore then begin
    if (OutFile <> '') and (OutStr<>'') then begin
      case ParseStyle of
        psGlossary : PostProcessGlossary;
        psCategories, psCategoryKeys : PostProcessCategories;
      end;
      Inc(SectCount);
      if FileExists(OutPath + OutFile) then
        AppendToFile(OutPath + OutFile, StringOf('-', 80) + LineEnding, true);
      AppendToFile(OutPath + OutFile, NormalizeLineEndings(OutStr, LineEnding), true);
      if FileSetDate(OutPath + OutFile, FileStamp) <> 0 then
        raise Exception.Create('error writing timestamp: ' + OutPath + OutFile);
    end;
  end else begin
    WriteLn('ignored section: ', SectionTitle);
    // WriteLn(OutStr);
  end;
  OutFile:='';
  OutStr:='';
  SectionTitle:='';
end;

procedure ParseSectionHead;
var
  S: String;
begin
  ParseStyle:=psNormal;
  S := Copy(InStrs[Index], 9);
  if Copy(S,1,2) = '!-' then begin

    // A comment, note, etc.
    SectionTitle:= TitleCase(LowerCase(StringReplace(
      StringReplace(Copy(S, 2), '-', '', [rfReplaceAll]), '_', SPACE,
      [rfReplaceAll])));

    if SectionTitle = 'Categories' then ParseStyle:=psCategories;
    if SectionTitle = 'Categorykeys' then begin
      SectionTitle:='Category Keys';
      ParseStyle:=psCategoryKeys
    end;

    OutFile:='_' + SectionTitle + '.txt';

    // Sections that are ignored for various reasons. Mostly, because it is
    // duplicate information, obsolete or created through other means.
    if SectionTitle = 'Filelist' then ParseStyle:=psIgnore;

    Inc(Index);
  end else begin
    // WriteLn(S);
  end;
  if ParseStyle = psIgnore then
    OutFile:='';

end;

procedure NewSection;
begin
  if Index < InStrs.Count then begin
    if FileTitle = 'Glossary' then begin // FileTitle is TitleCase

      ParseStyle:=psGlossary;
      while (Index < InStrs.Count) and (Trim(InStrs[Index]) = '') do Inc(Index);
      OutFile:=Trim(AlphaNumOnly(Trim(InStrs[Index]), '', true));
      if (OutFile = 'end of file') or (OutFile = 'endoffile') then
        OutFile:=''
      else
        Cat(OutFile, '.txt');

    end else begin
      ParseSectionHead;
    end;
  end;

end;

{ Main process to convert a single file }
procedure ProcessLST(FileName : String);
begin
  FileStamp:=GetFileStamp(SRC+FileName);
  InStrs := TStringList.Create;
  InStrs.LoadFromFile(SRC+FileName);
  FileTitle:=GetFileTitle(FileName, InStrs[0]);
  WriteLn('Processing: ', FileName, ', ', InStrs.Count, ' lines, ', FileTitle);
  OutPath:=DST + FileTitle;
  InsureDir(OutPath);
  OutPath:=IncludeTrailingPathDelimiter(OutPath);
  OutFile:=OFN_Header;
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
      NewSection;
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

