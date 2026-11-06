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
  Body, T, ID, Flags, CatKey : String;
  E : TBinaryTreeNode;
begin
  Body:= tagDiv('list-head', ListFiles[List].Header);

  // Index Comments
  E := ListFiles[List].Comments.First;
  if  Assigned(E) then begin
    Cat(Body, beginDiv('list-comments') + LF);
    While Assigned(E) do begin
      Cat(Body, tagDiv('list-comment-item', tagA(Url('comments', E.UniqueID),
        CommentTitle(E.UniqueID))));
      SaveHtml(Pathname + 'comments', E.UniqueID, CommentTitle(E.UniqueID),
        tagDiv('list-text', EscapeHtml(E.Text)));
      E:=E.Next;
    end;
    Cat(Body, endDiv);
  end;

  // Index Entries
  Cat(Body, beginDiv('list-entries'));
  E := ListFiles[List].Entries.First;
  Cat(Body, beginTable('list-index') + LF);
  While Assigned(E) do begin
    T := E.Text;
    T := Trim(PopDelim(T, LF));
    CatKey := ValToKey(E.Value);
    ID := PopDelim(T, ' - ');
    Flags := CutDelim(ID, SPACE, 3);
    ID := CutDelim(ID, SPACE, 1, 2);
    Cat(Body,
      tagTableRow('list-index-row',
        tagTableData('identifier-col', ID) +
        tagTableData('flags-col', Flags) +
        tagTableData('category-col', CatKey) +
        tagTableData('title-col', tagA(url(E.UniqueID), T, true))
      )
    );
    SaveHtml(Pathname, E.UniqueID, T, tagDiv('list-entry', EscapeHtml(E.Text)));
    E:=E.Next;
  end;
  Cat(Body, endTable + endDiv);

  SaveHtml(Pathname, 'index', TitleCase(ListFiles[List].Group), Body);
end;

function WriteGroup(Pathname : String; List : integer) : RawByteString;
begin
  LogMessage(vbNormal, 'output group: ' + ListFiles[List].Group);
  Pathname := IncludeTrailingPathDelimiter(Pathname) + ListFiles[List].Group;
  if not CreateTree(Pathname, True) then begin
    LogMessage(vbCritical, 'Unable to create directory: ' + Pathname);
    Halt(1);
  end;
  Pathname:=IncludeTrailingPathDelimiter(Pathname);
  WriteGroupFiles(Pathname, List);
  Result:=tagTableRow('list-index-row',
    tagTableData('title-col',
      tagA(url(ListFiles[List].Group, 'index'), ListFiles[List].Group)
    )
 );
end;

procedure WriteTheList(Pathname: String);
var
  I : integer;
  Body : RawByteString;
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
  Body:=beginTable('list-index') + LF;
  for I := Low(ListFiles) to High(ListFiles) do begin
    Cat(Body, WriteGroup(PathName, I));
  end;
  Cat(Body, endTable);
  SaveHtml(Pathname, 'index', 'The List', body);
end;

end.
