// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit SrcData;

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
  Version, PasExt, BinTree;

type
  TListType = (lfUnknown, lfExclude, lfSubPart, lfList);

  TListFile =record
    Name     : String;
    Header   : String;
    Sections : TBinaryTree;
    Entries  : TBinaryTree;
  end;

  TListFiles = array of TListFile;

var
  ListFiles : TListFiles;
  CriticalErrors : integer;

procedure ProcessFiles(Pathname : String);

implementation

const
  ZeroPadding    = 4;
  SectionDivider = '--------';

var
  ListType  : TListType;

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
   Result:=Copy(Data, 1, P - 1);
   Delete(Data, 1, P);
end;

// Loads and processes the sections in a LST file or group of LST files.
procedure ProcessList(Filename : String);
var
  E : Integer;
  Data : RawByteString;
  First : Boolean;
  Header, Section, ID, T : String;
  L : Char;
  N : TBinaryTreeNode;
begin
  SetLength(ListFiles, Length(ListFiles) + 1);
  ListFiles[High(ListFiles)].Name:=UpperCase(ExtractFileBase(FileName));
  ListFiles[High(ListFiles)].Header:='';
  ListFiles[High(ListFiles)].Sections:=TBinaryTree.Create;
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
      if Length(Section) > 0 then begin
        E:=0;
        if IsEntry(Header, ID) then begin
          // It is a Section entry
          T:=ID;
          repeat
            N:=ListFiles[High(ListFiles)].Entries.Add(ID, Section);
            if not Assigned(N) then begin
              Inc(E);
              ID:=T + '+' + ZeroPad(E, ZeroPadding);
            end;
          until Assigned(N);
          LogMessage(vbExcessive, TAB + 'Entry: ' + N.UniqueID);
        end else if (ID = '-') and Assigned(N) then begin
          // Divider is a continuation of previous section or entry
          N.Text:=N.Text + Header + LF + Section;
          LogMessage(vbExcessive, TAB + 'Continue: ' + N.UniqueID);
        end else if ID <> '' then begin
          // It is some sort of Comment section
          T:=ID;
          repeat
            N:=ListFiles[High(ListFiles)].Sections.Add(ID, Section);
            if not Assigned(N) then begin
              Inc(E);
              ID:=T + '+' + ZeroPad(E, ZeroPadding);
            end;
          until Assigned(N);
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
end;

// Determines and initiates the Process for a file in 'The List' release.
procedure ProcessFile(Filename : String);
var
  Ext : String;
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
    'BIBLIO',
    'CMOS',
    'FAQ',
    'FARCALL',
    'GLOSSARY',
    'I2C',
    'LINKS',
    'MEMORY',
    'MSR',
    'OVERVIEW',
    'PORTS',
    'SMM',
    'TABLES'   : if (Ext = '.LST') or (Ext = '.A') then ListType:=lfList;
    'INTERRUP' : case Ext of
      '.PRI' : ListType:=lfExclude;
      '.1ST' : ListType:=lfList;
      '.LST',
      '.A'   : ListType:=lfList;
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
    lfList : ProcessList(Filename);
  end;
end;

// Clears and resets all processed data. Not sure why I added this procedure.
procedure ProcessClear;
var
  I : Integer;
begin
  for I := High(ListFiles) downto 0 do begin
    FreeAndNil(ListFiles[I].Entries);
    FreeAndNil(ListFiles[I].Sections);
  end;
  SetLength(ListFiles, 0);
end;

// Process 'The List' Release files in a directory
procedure ProcessFiles(Pathname: String);
var
  I : Integer;
  L : TArrayOfRawByteString;
begin
  ProcessClear;
  DirScan(IncludeTrailingPathDelimiter(Pathname) + Wildcard, L, [dsFiles]);
  for I := 0 to High(L) do
    ProcessFile(IncludeTrailingPathDelimiter(Pathname) + L[I]);
end;


initialization

  ListFiles:=[];
  CriticalErrors:=0;

finalization

end.
