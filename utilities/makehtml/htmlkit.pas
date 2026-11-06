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

function url(const Filename : String) : RawByteString;

function tagComment(const Content : RawByteString; Escape : boolean = false) : RawByteString;

function beginDiv(const ID : RawByteString) : RawByteString;
function endDiv : RawByteString;
function tagDiv(const ID, Content : RawByteString; Escape : boolean = false) : RawByteString;

function beginSpan(const ID : RawByteString) : RawByteString;
function endSpan : RawByteString;
function tagSpan(const ID, Content : RawByteString; Escape : boolean = false) : RawByteString;

function tagA(const Href, Content : RawByteString; Escape : boolean = false) : RawByteString; overload;
function tagA(const ID, Href, Content : RawByteString; Escape : boolean = false) : RawByteString; overload;

implementation

function url(const Filename : String) : RawByteString;
begin
  Result:=Filename;
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
    Result:='<a href="' + url(Href) + '">' + PasExt.EscapeHTML(Content) + '</a>'
  else
    Result:='<a href="' + url(Href) + '">' + Content + '</a>';
end;

function tagA(const ID, HRef, Content : RawByteString; Escape : boolean = false) : RawByteString;
begin
  if Escape then
    Result:='<a class="' + ID + '"> href="' + url(Href) + '">' + PasExt.EscapeHTML(Content) + '</a>'
  else
    Result:='<a class="' + ID + '"> href="' + url(Href) + '">' + Content + '</a>';
end;

end.
