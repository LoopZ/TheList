// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit CfgMap;

{$mode objfpc}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Version information defines

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, IniFiles,
  { you can add units after this }
  Version, PasExt, GloDat;

// Read and apply settings contained in the _Mapping.txt file.
procedure ReadFileMap;

implementation


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



end.
