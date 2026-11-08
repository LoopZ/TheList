// Copyright 2026, Jerome Shidel
// The Clear BSD License
// All rights reserved.

unit AboutLst;

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

var
  AboutIndex : integer;
  Listings, Categories, IntFlags : TBinaryTree;
  ListIntro : RawByteString;

procedure ProcessAboutLst;

implementation

procedure FindAboutData;
var
  I : integer;
begin
  if AboutIndex <> -1 then Exit;
  for I := Low(ListFiles) to High(ListFiles) do
    if Uppercase(ListFiles[I].Name) = 'ABOUT' then begin
      AboutIndex := I;
      Exit;
    end;
  LogMessage(vbMinimal, 'ABOUT.LST file data not found.');
end;

function GetTextSection(Section : RawByteString) : RawByteString;
var
  Node : TBinaryTreeNode;
begin
  Node:=ListFiles[AboutIndex].Comments.Find(Section, false);
  if Assigned(Node) then
    Result := Node.Text
  else
    Result := '';

end;

procedure PopulateTree(var Tree : TBinaryTree; Section : RawByteString);
var
  Node : TBinaryTreeNode;
  Data, ID, Value : RawByteString;
begin
  Tree.Clear;
  Node:=ListFiles[AboutIndex].Comments.Find(Section, false);
  if not Assigned(Node) then Exit;
  Data:=Trim(Node.Text);
  while Data <> '' do begin
    Value:=Trim(PopDelim(Data, LF));
    ID := Trim(PopDelim(Value, '-'));
    Value:=Trim(Value);
    if (Value = '') or (ID = '') then Continue;
    Tree.Add(ID, Value);
  end;
  LogMessage(vbVerbose, 'About ' + Section + ' ' + IntToStr(Tree.Count) + ' entries');
end;

procedure PopulateData;
begin
  FindAboutData;
  if not Assigned(Listings) then Listings:=TBinaryTree.Create;
  if not Assigned(Categories) then Categories:=TBinaryTree.Create;
  if not Assigned(IntFlags) then IntFlags:=TBinaryTree.Create;
  PopulateTree(Listings, 'listings');
  PopulateTree(Categories, 'categories');
  PopulateTree(IntFlags, 'flags');
  ListIntro:=GetTextSection('introduction');
end;

procedure ProcessAboutLst;
begin
  if not Assigned(Listings) then
    PopulateData;
end;

initialization

  AboutIndex:=-1;
  Listings:=nil;
  Categories:=nil;
  IntFlags:=nil;

end.
