// Copyright 2025, Jerome Shidel
// BSD 3-Clause License

program fromrbil;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils
  { you can add units after this },
  PasExt;

{$R *.res}

const
  SRC='../../rbil61/';
  DST='../../source/';

  SectCount: integer= 0;
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

type

  TParseStyle = (psIgnore, psHeader, psNormal, psGlossary);

procedure ProcessLST(FileName : String);

const
  ItemBreak = '--------';

var
  Data : TStringList;
  DT : TDateTime;
  Line : Integer;
  Title : String;
  Dest : String;
  OName : String;
  OData : String;
  Style : TParseStyle;
  Brk : boolean;
  S : String;

begin
  Data:=nil;
  try
    if not FileAge(SRC+FileName, DT) then
      raise Exception.Create('error reading source timestamp');
    Data := TStringList.Create;
    Data.LoadFromFile(SRC+FileName);

    // Get Title/Directory Name
    Title := LowerCase(StringReplace(Data[0], TAB, SPACE, [rfReplaceAll]));
    Title := StringReplace(Title, 'release 61', SPACE, [rfReplaceAll]);
    Title := PopDelim(Title, COMMA);
    Title := Trim(PopDelim(Title, SPACE + SPACE));
    if (Title = '') or (Title[1] = '[') then Title:=LowerCase(
      Copy(FileName,1, Length(FileName) - Length(ExtractFileExt(FileName))));
    Title:=WordCase(Title);
    WriteLn('Processing: ', FileName, ', ', Data.Count, ' lines, ', Title);
    // create output directory
    if not DirectoryExists(DST + Title) then
      if not CreateDir(DST + Title) then begin
        WriteLn('Unable to create "', Title, '" directory.');
        Halt(1);
      end;
    // Start output with header file.
    Dest:=DST + IncludeTrailingPathDelimiter(Title);
    OName:='_Header.txt';
    Style:=psHeader;
    // First Line of file is usually a simple header that we will ignore;
    // Sometimes it is a reference we will include.
    OData:=Data[0] + CRLF;
    if LowerCase(OData).Contains('release 61') then OData:='';

    // Continue with copying the header and remaining items in the file.
    // Starting with the second line in the file.
    Line:=1;
    while Line < Data.Count do begin
      OData:=OData + Data[Line] + CRLF;
      Inc(Line);

      // Test if Item is complete and new Item should be started.
      Brk:= Line >= Data.Count;
      if not Brk then
        case Style of
          psGlossary : Brk:=Trim(Data[Line]) = '';
          psHeader : begin
            if Title = 'Glossary' then
              Brk:=Trim(Data[Line]) = ''
            else
              Brk :=Copy(Data[Line], 1, Length(ItemBreak)) = ItemBreak;
          end
        else
            Brk :=Copy(Data[Line], 1, Length(ItemBreak)) = ItemBreak;
        end;

      // Write completed item
      if Brk then begin
        if Style <> psIgnore then begin
          if (OName <> '') and (OData<>'') then begin
            Inc(SectCount);
            if FileExists(Dest + OName) then
              AppendToFile(Dest + OName, StringOf('-', 80) + CRLF, true);
            AppendToFile(Dest + OName, OData, true);
            if FileSetDate(Dest + OName, DateTimeToFileDate(DT)) <> 0 then
              raise Exception.Create('error writing timestamp: ' + Dest + OName);
          end;
        end;
        OName:='';
        OData:='';
        if Line < Data.Count then begin
          if Title = 'Glossary' then begin
            Style:=psGlossary;
            while (Line < Data.Count) and (Trim(Data[Line]) = '') do Inc(Line);
            OName:=AlphaNumOnly(Trim(Data[Line]));
            if OName = 'endoffile' then
              OName:=''
            else
              OName:=OName + '.txt';
          end else begin
            Style:=psNormal;
            S := Copy(Data[Line], 9);
            if Copy(S,1,2) = '!-' then begin
              // A comment, note, etc.
              S:= WordCase(LowerCase(StringReplace(
                StringReplace(Copy(S, 2), '-', '', [rfReplaceAll]), '_', SPACE,
                [rfReplaceAll])));
              OName:='_' + S + '.txt';
              // Sections that are ignored for various reasons. Mostly, because
              // it is duplicate information or other parsers.
              if S = 'Filelist' then Style:=psIgnore;
              if S = 'Section' then Style:=psIgnore;

              Inc(Line);
            end;
          end;
        end;
      end;
    end;

    // for I := 0 to Data.Count - 1 do
    // if Copy(Data[I], 1, 5) = '-----' then
    //    WriteLn(Data[I]);
  finally
    if Assigned(Data) then FreeAndNil(Data);
  end;
end;

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

procedure Banner;
begin
  { Program Information Banner }
  WriteLn(APP_PRODUCTNAME, ' v', APP_VERSION);
  WriteLn('Copyright ', APP_LEGALCOPYRIGHT);
  WriteLn(APP_PRODUCTLICENSE);
  WriteLn;
end;

begin
  Banner;
  ConvertAll;
end.

