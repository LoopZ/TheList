// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit HtmlKit;

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
  Version, PasExt;

const
  HtmlExt : String = '.html';

function HtmlRef(Filename : RawByteString) : RawByteString;
function url(PathName, Filename : RawByteString) : RawByteString; overload;
function url(Filename : RawByteString) : RawByteString; overload;
procedure SaveHtml(Pathname, Filename, Title, Contents : RawByteString);

function tagTargetID(const TargetID, Content : RawByteString) : RawByteString;

function tagComment(const Content : RawByteString; Escape : boolean = false) : RawByteString;

function beginDiv(const ID : RawByteString) : RawByteString;
function endDiv : RawByteString;
function tagDiv(const ID, Content : RawByteString; Escape : boolean = false) : RawByteString;

function beginSpan(const ID : RawByteString) : RawByteString;
function endSpan : RawByteString;
function tagSpan(const ID, Content : RawByteString; Escape : boolean = false) : RawByteString;

function tagA(const Href, Content : RawByteString; Escape : boolean = false) : RawByteString; overload;
function tagA(const ID, Href, Content : RawByteString; Escape : boolean = false) : RawByteString; overload;

function beginTable(const ID : RawByteString) : RawByteString;
function endTable : RawByteString;
function tagTable(const ID, Content : RawByteString; Escape : boolean = false) : RawByteString;

function beginTableRow(const ID : RawByteString) : RawByteString;
function endTableRow : RawByteString;
function tagTableRow(const ID, Content : RawByteString; Escape : boolean = false) : RawByteString;

function beginTableData(const ID : RawByteString) : RawByteString;
function endTableData : RawByteString;
function tagTableData(const ID, Content : RawByteString; Escape : boolean = false) : RawByteString;

function StyleSheet : RawByteString;

implementation

var
  CSSData : RawByteString;

function HtmlRef(Filename : RawByteString) : RawByteString;
begin
  Filename:=StringReplace(EscapeHTML(Filename), '/', '%2f', [rfReplaceAll]);
  Result:=Filename;
end;

function url(Filename : RawByteString) : RawByteString;  overload;
begin
  Result:=url('', Filename);
end;

function url(PathName, Filename : RawByteString) : RawByteString;  overload;
begin
  if Pathname <> '' then
    PathName:=IncludeTrailingpathDelimiter(PathName);
  Result:=PathName + HtmlRef(Filename) + HtmlExt;
end;

procedure SaveHtml(Pathname, Filename, Title, Contents : RawByteString);
begin
  if not CreateTree(Pathname, True) then begin
    LogMessage(vbCritical, 'Unable to create directory: ' + Pathname);
    Halt(1);
  end;
  Contents := '<html>' + LF +
    '<head><title>' + EscapeHtml(Title) + '</title></head>' + LF +
    StyleSheet +
    '<body>' + LF +
    Contents +
    '</body></html>' + LF;
  Filename:=Url(IncludeTrailingPathDelimiter(Pathname), Filename);
  if FileSave(Filename, Contents) <> 0 then begin
    LogMessage(vbCritical, 'Unable to write file: ' + Filename );
    Halt(1);
  end;
end;

function tagTargetID(const TargetID, Content : RawByteString) : RawByteString;
var
  T, C : String;
  X : boolean;
begin
  if not HasLeading(Content, '<') then
    Result:='<span id="' + TargetID + '">' + Content + '</span>'
  else begin
    C := Content;
    T := Trim(PopDelim(C, '>'));
    X:= HasTrailing(T, '/');
    if X then begin
      T :=Trim(ExcludeTrailing(T, '/'));
      X := True;
    end;
    Cat(T, SPACE + 'id="' + TargetID + '"');
    if X then
      Cat(T, SPACE + '/');
    Result:=T + '>' + C;
  end;
end;

function tagComment(const Content : RawByteString; Escape : boolean = false) : RawByteString;
begin
  if Escape then
    Result:='<!-- ' + PasExt.EscapeHTML(Content) + ' -->'
  else
    Result:='<!-- ' + Content + ' -->';
end;

function beginDiv(const ID : RawByteString) : RawByteString;
begin
  Result:='<div class="' + ID + '">';
end;

function endDiv : RawByteString;
begin
     Result:='</div>' + LF;
end;

function tagDiv(const ID, Content : RawByteString; Escape : boolean = false) : RawByteString;
begin
  if Escape then
    Result:=beginDiv(ID) + PasExt.EscapeHTML(Content) + endDiv
  else
    Result:=beginDiv(ID) + Content + endDiv;
end;

function beginSpan(const ID : RawByteString) : RawByteString;
begin
  Result:='<span class="' + ID + '">';
end;

function endSpan : RawByteString;
begin
     Result:='</span>' + LF;
end;

function tagSpan(const ID, Content : RawByteString; Escape : boolean = false) : RawByteString;
begin
  if Escape then
    Result:=beginSpan(ID) + PasExt.EscapeHTML(Content) + endSpan
  else
    Result:=beginSpan(ID) + Content + endSpan;
end;

function tagA(const Href, Content : RawByteString; Escape : boolean = false) : RawByteString;
begin
  if Escape then
    Result:='<a href="' + Href + '">' + PasExt.EscapeHTML(Content) + '</a>'
  else
    Result:='<a href="' + Href + '">' + Content + '</a>';
end;

function tagA(const ID, HRef, Content : RawByteString; Escape : boolean = false) : RawByteString;
begin
  if Escape then
    Result:='<a class="' + ID + '"> href="' + Href + '">' + PasExt.EscapeHTML(Content) + '</a>'
  else
    Result:='<a class="' + ID + '"> href="' + Href + '">' + Content + '</a>';
end;

function beginTable(const ID: RawByteString): RawByteString;
begin
  Result:='<table class="' + ID + '">';
end;

function endTable: RawByteString;
begin
  Result:='</table>';
end;

function tagTable(const ID, Content: RawByteString; Escape: boolean
  ): RawByteString;
begin
  if Escape then
    Result:=beginTable(ID) + PasExt.EscapeHTML(Content) + endTable + LF
  else
    Result:=beginTable(ID) + Content + endTable + LF;
end;

function beginTableRow(const ID: RawByteString): RawByteString;
begin
   Result:='<tr class="' + ID + '">';
end;

function endTableRow: RawByteString;
begin
  Result:='</tr>';
end;

function tagTableRow(const ID, Content: RawByteString; Escape: boolean
  ): RawByteString;
begin
  if Escape then
    Result:=beginTableRow(ID) + PasExt.EscapeHTML(Content) + endTableRow + LF
  else
    Result:=beginTableRow(ID) + Content + endTableRow + LF;
end;

function beginTableData(const ID: RawByteString): RawByteString;
begin
  Result:='<td class="' + ID + '">';
end;

function endTableData: RawByteString;
begin
  Result:='</td>';
end;

function tagTableData(const ID, Content: RawByteString; Escape: boolean
  ): RawByteString;
begin
  if Escape then
    Result:=beginTableData(ID) + PasExt.EscapeHTML(Content) + endTableData
  else
    Result:=beginTableData(ID) + Content + endTableData;
end;

function StyleSheet : RawByteString;
begin
  Result:='<style>' + LF +
  CSSData +
    '</style>' + LF;
end;

initialization

  if FileLoad(AppExecPath + 'stylesheet.css', CSSData) <> 0 then
    CSSData:='/* no style sheet found */';
  CSSData:=IncludeTrailing(NormalizeLineEndings(CSSData), LF);

end.
