// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit ReadList;

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
  Version, PasExt, BinTree, GloData;

// Loads The List release files from a directory and prepares for the process
// of converting their data to HTML.
procedure ReadTheList(Pathname : String);

implementation

const
  ZeroPadding    = 4;
  SectionDivider = '--------';

var
  // Type of List file that is being loaded and processed.
  ListType  : TListType;

// Removes extra leading and trailing line feeds
function TrimLF(const Data : String) : String;
begin
  Result:=Data;
  While HasLeading(Result, LF) do
    Delete(Result, 1, 1);
  While HasTrailing(Result, LF + LF) do
    SetLength(Result, Length(Result) - 1);
end;

// Determines if a section header is an Entry or a Comment.
// Returns the ID for the Section. Returns True if it is an Entry.
function IsEntry(const SectionHeader : RawByteString; out ID : String) : boolean;
begin
  ID:='';
  Result:=False;
  if not HasLeading(SectionHeader, SectionDivider) then Exit;
  Result:=Copy(SectionHeader, 11, 2) <> '--';
  if Result then
    ID:=Copy(SectionHeader, 11)
  else
    ID:=Copy(SectionHeader, 13);
  while (Length(ID) > 1) and (ID[Length(ID)] = '-') do
    SetLength(ID, Length(ID) - 1);
  case ListType of
    lfLinks : if HasLeading(ID, 'LINK:') then begin
      ID:=ExcludeLeading(ID, 'LINK:');
      Result:=True;
    end;
    lfFAQ : if HasLeading(ID, 'FAQ:') then begin
      ID:=ExcludeLeading(ID, 'FAQ:');
      Result:=True;
    end;
    lfSMM : if HasLeading(ID, 'SMM:') then begin
      ID:=ExcludeLeading(ID, 'SMM:');
      Result:=True;
    end;
  end;
end;

// Returns the next section from the Data. Removes that section from the Data.
function PopSection(var Data : RawByteString):RawByteString;
var
  P : Integer;
begin
   Result:='';
   if HasLeading(Data, SectionDivider) then begin
     P:=Pos(LF, Data);
     if P < 1 then P:= 1;
   end else
     P:=1;
   P:=Pos(LF + SectionDivider, Data);
   if P < 1 then begin
     Result:=Data;
     Data:='';
     Exit;
   end;
   Result:=TrimLF(Copy(Data, 1, P - 1));
   Delete(Data, 1, P);
end;

// Adds an entry or section to a tree
function AddToTree(const Tree : TBinaryTree; const ID, Section : String) : TBinaryTreeNode;
var
  Dupe : Integer;
  T : String;
begin
  Result:=nil;
  if Length(Section) = 0 then Exit;
  Dupe:=0;
  T:=ID;
  repeat
    Result:=Tree.Add(T, Section);
    if not Assigned(Result) then begin
      Inc(Dupe);
      T:=ID + '+' + ZeroPad(Dupe, ZeroPadding);
    end;
  until Assigned(Result);
end;

// Separates a glossary section into individual entries.
procedure AddGlossary(Section : String);
var
  ID, Item : String;
  N : TBinaryTreeNode;
begin
  while (Length(Section) > 0) do begin
    Item:=PopDelim(Section, LF + LF);
    ID:=Trim(PopDelim(Item, LF));
    Item:=TrimLF(Item);
    if (ID = '') and (Item = '') then begin
      // Should not happen, but just in case...
      LogMessage(vbMinimal, '(null item)');
    end else if (ID = '') or (Item = '') then begin
       LogMessage(vbMinimal, 'Error identifying item: ' + ID);
       if CICD then Halt(1);
    end else begin
      N:=AddToTree(ListFiles[High(ListFiles)].Entries, ID, Item);
      LogMessage(vbExcessive, TAB + 'Entry: ' + N.UniqueID);
    end;
  end;
end;

// Separates a bibliography section into individual entries.
procedure AddBibliography(Section : String);
var
  ID : Int64;
  Item : String;
  N : TBinaryTreeNode;
begin
  ID:=0;
  while (Length(Section) > 0) do begin
    Item:=PopDelim(Section, LF + LF);
    Item:=TrimLF(Item);
    if (Item = '') then begin
      // Should not happen, but just in case...
      LogMessage(vbMinimal, '(null item)');
    end else begin
      N:=AddToTree(ListFiles[High(ListFiles)].Entries, '$'+HexStr(ID,8), Item);
      Inc(ID);
      LogMessage(vbExcessive, TAB + 'Entry: ' + N.UniqueID);
    end;
  end;
end;

// Separates a tables section into individual entries.
procedure AddTables(Section : String);
begin
  AddGlossary(Section);
end;

// Separates a titles section into individual entries.
procedure AddTitles(Section : String);
var
  ID, Item : String;
  N : TBinaryTreeNode;
begin
  while (Length(Section) > 0) do begin
    Item:=PopDelim(Section, LF);
    ID:=Trim(PopDelim(Item, '-'));
    Item:=Trim(Item);
    if (ID = '') or (Item = '') then begin
      // Should not happen, but just in case...
      LogMessage(vbMinimal, '(null item)');
    end else begin
      N:=AddToTree(ListFiles[High(ListFiles)].Entries, ID, ID + ' - ' + Item);
      LogMessage(vbExcessive, TAB + 'Entry: ' + N.UniqueID);
    end;
  end;
end;

// Loads and processes the sections in a LST file or group of LST files.
procedure ProcessList(Filename : String);
var
  E : Integer;
  Data : RawByteString;
  First : Boolean;
  Header, Section, ID : String;
  L : Char;
  N : TBinaryTreeNode;
begin
  SetLength(ListFiles, Length(ListFiles) + 1);
  ListFiles[High(ListFiles)].Kind:=ListType;
  ListFiles[High(ListFiles)].Name:=UpperCase(ExtractFileBase(Filename));
  ListFiles[High(ListFiles)].Header:='';
  ListFiles[High(ListFiles)].Comments:=TBinaryTree.Create;
  ListFiles[High(ListFiles)].Entries:=TBinaryTree.Create;
  First:=True;
  repeat
    E:=FileLoad(Filename, Data);
    if E <> 0 then begin
      LogMessage(vbCritical, 'Error #'+ IntToStr(E)+ ', loading file: ' +
        ExtractFileName(Filename));
      Inc(CriticalErrors);
      Exit;
    end;
    LogMessage(vbNormal, 'processing file: ' +ExtractFileName(Filename));
    Data:=NormalizeLineEndings(Data);
    // Get File Header
    Header:=PopSection(Data);
    if First then begin
      ListFiles[High(ListFiles)].Header:=Header;
      First:=False;
    end;
    N:=nil;
    While Length(Data) <> 0 do begin
      Section:=PopSection(Data);
      Header:=PopDelim(Section, LF);
      if Length(Section) = 0 then begin
        LogMessage(vbNormal, 'processing file: ' +ExtractFileName(Filename));

      end else begin
        if IsEntry(Header, ID) then begin
          // It is a Section entry
          N:=AddToTree(ListFiles[High(ListFiles)].Entries, ID, Section);
          LogMessage(vbExcessive, TAB + 'Entry: ' + N.UniqueID);
        end else if (ID = '-') and Assigned(N) then begin
          // Divider is a continuation of previous section or entry
          N.Text:=N.Text + Header + LF + Section;
          LogMessage(vbExcessive, TAB + 'Continue: ' + N.UniqueID);
        end else begin
          N:=nil;
          case Uppercase(ID) of
            '' : LogMessage(vbNormal, TAB + 'Unidentified section');
            'BIBLIOGRAPHY' : begin
              if ListType = lfBiblio then
                AddBibliography(Section)
              else
                N:=AddToTree(ListFiles[High(ListFiles)].Comments, ID, Section);
            end;
            'GLOSSARY' : begin
              if ListType = lfGlossary then
                AddGlossary(Section)
              else
                N:=AddToTree(ListFiles[High(ListFiles)].Comments, ID, Section);
            end;
            'TABLES' : begin
              if ListType = lfTables then
                AddTables(Section)
              else
                N:=AddToTree(ListFiles[High(ListFiles)].Comments, ID, Section);
            end;
            'TITLES' : begin
              if ListType = lfOverview then
                AddTitles(Section)
              else
                N:=AddToTree(ListFiles[High(ListFiles)].Comments, ID, Section);
            end
          else
            N:=AddToTree(ListFiles[High(ListFiles)].Comments, ID, Section);
          end;
          if Assigned(N) then
            LogMessage(vbExcessive, TAB + 'Comment: ' + N.UniqueID);
        end;
      end;
    end;
    if HasTrailing(Filename, '.LST', false) then Break;
    L:=Filename[Length(Filename)];
    L:=Upcase(L);
    if L < 'Z' then
      Filename[Length(Filename)]:=Char(Ord(L)+1)
    else
      Cat(Filename, 'A');
  until (not FileExists(Filename));
  LogMessage(vbNormal, TAB + 'Comments ' +
    IntToStr(ListFiles[High(ListFiles)].Comments.Count));
  LogMessage(vbNormal, TAB + 'Entries  ' +
    IntToStr(ListFiles[High(ListFiles)].Entries.Count));
end;

// Determines and initiates the Process for a file in 'The List' release.
procedure ProcessFile(Filename : String);
var
  Ext : String;

  function IsList(T : TListType) : TListType;
  begin
    if (Ext = '.LST') or (Ext = '.A') then
      Result:=T
    else
      Result:=lfExclude;
  end;

begin
  Ext:=UpperCase(ExtractFileExt(FileName));
  ListType := lfUnknown;
  if (Length(Ext) = 2) and ((Ext >= '.B') and (Ext <= '.Z')) then
    ListType:=lfSubPart
  else if (Length(Ext) = 3) and ((Ext >= '.ZA') and (Ext <= '.ZZ')) then
    ListType:=lfSubPart
  else if (Length(Ext) = 4) and ((Ext >= '.ZZA') and (Ext <= '.ZZZ')) then
    ListType:=lfSubPart
  else case UpperCase(ExtractFileBase(FileName)) of
    'CATEGORY' : if Ext = '.KEY' then ListType:=lfExclude;
    'LICENSE'  : if Ext = ''     then ListType:=lfExclude;
    'README'   : if Ext = '.NOW' then ListType:=lfExclude;
    'BIBLIO'   : ListType:=IsList(lfBiblio);
    'GLOSSARY' : ListType:=IsList(lfGlossary);
    'OVERVIEW' : ListType:=IsList(lfOverview);
    'TABLES'   : ListType:=IsList(lfTables);
    'CMOS',
    'FARCALL',
    'I2C',
    'MEMORY',
    'MSR',
    'PORTS'    : ListType:=IsList(lfList);
    'LINKS'    : ListType:=IsList(lfLinks);
    'FAQ'      : ListType:=IsList(lfFAQ);
    'SMM'      : ListType:=IsList(lfSMM);
    'INTERRUP' : case Ext of
      '.PRI' : ListType:=lfExclude;
      '.1ST' : ListType:=lfList;
      '.LST',
      '.A'   : ListType:=IsList(lfList);
    end;
  end;
  case ListType of
    lfExclude : begin
      LogMessage(vbExcessive, 'Skip file: ' + Filename);
      Exit;
    end;
    lfUnknown : begin
      LogMessage(vbNormal, 'Unrecognized file: ' + Filename);
      Exit;
    end;
    lfSubPart : Exit;
    lfBiblio, lfGlossary, lfTables, lfOverview,
    lfList, lfLinks, lfFAQ, lfSMM : ProcessList(Filename);
  end;
end;

// Process 'The List' Release files in a directory
procedure ReadTheList(Pathname: String);
var
  I : Integer;
  L : TArrayOfRawByteString;
begin
  DirScan(IncludeTrailingPathDelimiter(Pathname) + Wildcard, L, [dsFiles]);
  for I := 0 to High(L) do
    ProcessFile(IncludeTrailingPathDelimiter(Pathname) + L[I]);
end;

end.
