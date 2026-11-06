// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit WriteList;

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
  Version, PasExt, BinTree, GloData, HtmlKit;

procedure WriteTheList(Pathname : String);

implementation

function CommentTitle(S : String) : String;
begin
  case Uppercase(S) of
    'ADMIN' : Result:= 'Administration';
    'NOTE'  : Result:= 'Notes';
  else
    Result :=TitleCase(Lowercase(StringReplace(S, '_', ' ', [rfReplaceAll])));
  end;
end;


procedure WriteGroupFiles(Pathname : String; List : integer);
var
  body, T, ID, Flags, CatKey : String;
  E : TBinaryTreeNode;
begin
  body:= tagDiv('list-head', ListFiles[List].Header);

  // Index Comments
  E := ListFiles[List].Comments.First;
  if  Assigned(E) then begin
    Cat(body, beginDiv('list-comments') + LF);
    While Assigned(E) do begin
      Cat(body, tagDiv('list-comment-item', tagA(Url('comments', E.UniqueID),
        CommentTitle(E.UniqueID))));
      SaveHtml(Pathname + 'comments', E.UniqueID, CommentTitle(E.UniqueID), E.Text);
      E:=E.Next;
    end;
    Cat(body, endDiv);
  end;

  // Index Entries
  E := ListFiles[List].Entries.First;
  Cat(body, beginTable('list-index') + LF);
  While Assigned(E) do begin
    T := E.Text;
    T := Trim(PopDelim(T, LF));
    CatKey := ValToKey(E.Value);
    ID := PopDelim(T, ' - ');
    Flags := '';
    Cat(body,
      tagTableRow('list-index-row',
        tagTableData('identifier-col', ID) +
        tagTableData('flags-col', Flags) +
        tagComment('Category: ' + CatKey) +
        tagTableData('title-col', tagA(url(E.UniqueID), T))
      )
    );
    SaveHtml(Pathname, E.UniqueID, T, E.Text);
    E:=E.Next;
  end;
  Cat(body, endTable);

  SaveHtml(Pathname, 'index', TitleCase(ListFiles[List].Group), body);
end;

procedure WriteGroup(Pathname : String; List : integer);
begin
  LogMessage(vbNormal, 'output group: ' + ListFiles[List].Group);
  Pathname := IncludeTrailingPathDelimiter(Pathname) + ListFiles[List].Group;
  if not CreateTree(Pathname, True) then begin
    LogMessage(vbCritical, 'Unable to create directory: ' + Pathname);
    Halt(1);
  end;
  Pathname:=IncludeTrailingPathDelimiter(Pathname);
  WriteGroupFiles(Pathname, List);
end;

procedure WriteTheList(Pathname: String);
var
  I : integer;
begin
  if DirectoryExists(Pathname) then begin
    LogMessage(vbNormal, 'HTML output directory already exists: ' + Pathname);
    if not DeleteTree(Pathname) then begin
      LogMessage(vbCritical, 'Unable to remove previous HTML output directory: ' + Pathname);
      Halt(1);
    end else
      LogMessage(vbNormal, 'Removed previous HTML output directory: ' + Pathname);
  end;
  if not CreateTree(Pathname) then begin
    LogMessage(vbCritical, 'Unable to create HTML output directory: ' + Pathname);
    Halt(1);
  end else
    LogMessage(vbNormal, 'Created HTML output directory: ' + Pathname);
  for I := Low(ListFiles) to High(ListFiles) do begin
      WriteGroup(PathName, I);
  end;
end;

end.
