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

function NavBarHtml(Node : TBinaryTreeNode; TargetId : RawByteString;
  TargetPath : RawByteString ) : RawByteString;
var
  P, N : RawByteString;
begin
  P := '';
  N := '';
  if Assigned(Node.Previous) then
    P := tagA(url(Node.Previous.UniqueID), 'Previous');
  if Assigned(Node.Next) then
    N := tagA(url(Node.Next.UniqueID), 'Next');
  if (TargetID <> '') then
    TargetID:=IncludeLeading(TargetID, '#');
  if (TargetPath <> '') then
    TargetPath:=IncludeTrailing(TargetPath, '/');
  Result:=
    tagDiv('list-navbar', LF +
      tagDiv('list-nav-prev', P) +
      tagDiv('list-nav-up',tagA(TargetPath +  url('index') + TargetID, 'Index')) +
      tagDiv('list-nav-next', N)
    );
end;

function WriteComments(PathName: RawByteString; Node : TBinaryTreeNode) : RawByteString;
begin
  Result:='';
  if not Assigned(Node) then exit;

  Cat(Result, beginDiv('list-comments') + LF);
  While Assigned(Node) do begin
    Cat(Result, tagDiv('list-comment-item', tagA(Url('comments', Node.UniqueID),
      CommentTitle(Node.UniqueID))));

    // Save Comments File
    SaveHtml(Pathname + 'comments', Node.UniqueID, CommentTitle(Node.UniqueID),
      NavBarHtml(Node, '', '..') + tagDiv('list-text', EscapeHtml(Node.Text)));

    Node:=Node.Next;
  end;
  Cat(Result, endDiv);
end;

function WriteEntries(PathName: RawByteString; Node : TBinaryTreeNode) : RawByteString;
 var
   T, ID, Flags, CatKey : RawByteString;
begin
  Result:='';
  if not Assigned(Node) then exit;
  Cat(Result, beginDiv('list-entries'));
  Cat(Result, beginTable('list-index') + LF);
  While Assigned(Node) do begin
    T := Node.Text;
    T := Trim(PopDelim(T, LF));
    CatKey := ValToKey(Node.Value);
    ID := PopDelim(T, ' - ');
    Flags := CutDelim(ID, SPACE, 3);
    ID := CutDelim(ID, SPACE, 1, 2);
    Cat(Result,
      tagTargetID(Node.UniqueID,
        tagTableRow('list-index-row',
          tagTableData('identifier-col', ID) +
          tagTableData('flags-col', Flags) +
          tagTableData('category-col', CatKey) +
          tagTableData('title-col', tagA(url(Node.UniqueID), T, true))
        )
      )
    );

    // Save Entry File
    SaveHtml(Pathname, Node.UniqueID, ID,
      NavBarHtml(Node, Node.UniqueID, '') +
      tagDiv('list-entry', EscapeHtml(Node.Text))
    );

    Node:=Node.Next;
  end;
  Cat(Result, endTable + endDiv);

end;

procedure WriteGroupFiles(Pathname : String; List : integer);
var
  Body : RawByteString;
begin
  Body:=
    tagDiv('list-navbar', LF +
      tagDiv('list-nav-prev', '') +
      tagDiv('list-nav-up', tagA(url('..', 'index'), 'Index')) +
      tagDiv('list-nav-next', '')
    ) +
    tagDiv('list-head', ListFiles[List].Header);

  Cat(Body, WriteComments(PathName, ListFiles[List].Comments.First));

  // Index Entries
  Cat(Body, WriteEntries(PathName, ListFiles[List].Entries.First));

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
