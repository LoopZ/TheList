{
   Copyright (c) 2025-2026 Jerome Shidel
   The Clear BSD License
   All rights reserved.

   This modified version of the BinTree unit that is part of the
   MPLA frame available at:

   https://gitlab.com/mpla-oss/mpla/

}

unit BinTree;

{$mode ObjFPC}{$H+}

{$I patches.pp}  // Various compiler directives to "fix" things.
{$I version.def} // Include directives for project option build flags.

interface

uses
  {$IFDEF USES_CWString} cwstring, {$ENDIF}
  Classes, SysUtils, PasExt;

type

  TBinaryTreeNode = class;
  TCustomBinaryTree = class;
  TBinaryTreeIterator = procedure(Node: TBinaryTreeNode) of object;

  { TBinaryTreeNode }

  TBinaryTreeNode = class
  private
    FColor: Boolean;
    FData: TArrayOfByte;
    FData32: TArrayOfInt32;
    FFlags: dword;
    FSubTree: TCustomBinaryTree;
    FGreater: TBinaryTreeNode;
    FLesser: TBinaryTreeNode;
    FOwner: TCustomBinaryTree;
    FParent: TBinaryTreeNode;
    FText: String;
    FUniqueID: RawByteString;
    FValue: Int64;
    function GetNext: TBinaryTreeNode;
    function GetPrevious: TBinaryTreeNode;
    procedure SetData(AValue: TArrayOfByte);
    procedure SetData32(AValue: TArrayOfInt32);
    procedure SetFlags(AValue: dword);
    procedure SetSubTree(AValue: TCustomBinaryTree);
    procedure SetText(AValue: String);
    procedure SetValue(AValue: Int64);
  protected
    property Parent : TBinaryTreeNode read FParent;
    property Lesser : TBinaryTreeNode read FLesser;
    property Greater : TBinaryTreeNode read FGreater;
  public
    constructor Create(const AUniqueID : RawByteString); virtual; overload;
    {constructor Create(const AUniqueID : RawByteString; AData : TArrayOfByte); virtual; overload;
    constructor Create(const AUniqueID : RawByteString; AData : TArrayOfInt32); virtual; overload;}
    destructor Destroy; override;
    property Owner : TCustomBinaryTree read FOwner;
    property Previous : TBinaryTreeNode read GetPrevious;
    property Next : TBinaryTreeNode read GetNext;
    property UniqueID : RawByteString read FUniqueID;
    property SubTree : TCustomBinaryTree read FSubTree write SetSubTree;
    property Data : TArrayOfByte read FData write SetData;
    property Data32 : TArrayOfInt32 read FData32 write SetData32;
    property Flags : dword read FFlags write SetFlags;
    property Value : Int64 read FValue write SetValue;
    property Text : String read FText write SetText;
  published
  end;

  { TCustomBinaryTree }

  TCustomBinaryTree = class
  private
    FCount: Int64;
    FModified: boolean;
    FRoot: TBinaryTreeNode;
    function GetFirst: TBinaryTreeNode;
    function GetLast: TBinaryTreeNode;
    procedure SetModified(AValue: boolean);
  protected
    procedure Attach(var Node : TBinaryTreeNode); virtual;
    procedure Detach(var Node : TBinaryTreeNode); virtual;
    function DetachSafe(const Node : TBinaryTreeNode) : boolean; virtual;
    procedure RotateLeft(const Node: TBinaryTreeNode); virtual;
    procedure RotateRight(const Node: TBinaryTreeNode); virtual;
    procedure Transplant(U, V: TBinaryTreeNode); virtual;
    procedure SelfBalance(Node: TBinaryTreeNode); virtual;
    procedure DeleteBalance(Node, Parent: TBinaryTreeNode); virtual;
    property Root : TBinaryTreeNode read FRoot;
    function  StreamID : String; virtual;
    procedure WriteStream(Stream : TStream); virtual;
    procedure ReadStream(Stream : TStream); virtual;
    procedure WriteHead(Stream : TStream); virtual;
    procedure ReadHead(Stream: TStream; out ExpectedCount : integer); virtual;
    procedure WriteNodes(Stream : TStream; Node : TBinaryTreeNode); virtual;
    procedure ReadNodes(Stream : TStream; ExpectedCount : integer; out ReadCount : Integer); virtual;
    procedure WriteNodeData(Stream : TStream; Node : TBinaryTreeNode); virtual;
    procedure ReadNodeData(Stream : TStream; Node : TBinaryTreeNode); virtual;
    procedure WriteSubTree(Stream : TStream; Node : TBinaryTreeNode); virtual;
    procedure ReadSubTree(Stream : TStream; Node : TBinaryTreeNode; TreeClass : ShortString); virtual;
    procedure WriteTail(Stream : TStream); virtual;
    procedure ReadTail(Stream : TStream); virtual;
    procedure SaveToFile(FileName : String); virtual;
    procedure LoadFromFile(FileName : String); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Optimize; virtual;
    function CheckIntegrity : boolean; virtual;

    function Add(UniqueID : UnicodeString) : TBinaryTreeNode; overload;
    function Add(UniqueID : Int64) : TBinaryTreeNode; overload;
    function Add(UniqueID : RawByteString) : TBinaryTreeNode; overload;
    function Find(UniqueID : RawByteString; CaseSensitive : boolean = true) : TBinaryTreeNode; overload;
    function Find(UniqueID : UnicodeString; CaseSensitive : boolean = true) : TBinaryTreeNode; overload;
    function Find(UniqueID : Int64) : TBinaryTreeNode; overload;
    procedure Delete(var Node : TBinaryTreeNode); overload;
    procedure Foreach(Callback : TBinaryTreeIterator);
    property First : TBinaryTreeNode read GetFirst;
    property Last : TBinaryTreeNode read GetLast;
    property Count : Int64 read FCount;
    property Modified : boolean read FModified write SetModified;
  published
  end;


  { TRedBlackBinaryTree }

  TRedBlackBinaryTree = class (TCustomBinaryTree)
  protected
    function  StreamID : String; override;
    procedure Detach(var Node : TBinaryTreeNode); override;
    procedure SelfBalance(Node: TBinaryTreeNode); override;
    procedure DeleteBalance(Node, Parent: TBinaryTreeNode); override;
  public
    function CheckIntegrity : boolean; override;
  end;

  TBinaryTree=class(TRedBlackBinaryTree)
  public
    property Root;
    function Add(UniqueID : RawByteString; Data : TArrayOfByte) : TBinaryTreeNode; overload;
    function Add(UniqueID : UnicodeString; Data : TArrayOfByte) : TBinaryTreeNode; overload;
    function Add(UniqueID : Int64; Data : TArrayOfByte) : TBinaryTreeNode; overload;
    function Add(UniqueID : RawByteString; Data32 : TArrayOfInt32) : TBinaryTreeNode; overload;
    function Add(UniqueID : UnicodeString; Data32 : TArrayOfInt32) : TBinaryTreeNode; overload;
    function Add(UniqueID : Int64; Data32 : TArrayOfInt32) : TBinaryTreeNode; overload;
    function Add(UniqueID : RawByteString; Value : Int64) : TBinaryTreeNode; overload;
    function Add(UniqueID : UnicodeString; Value : Int64) : TBinaryTreeNode; overload;
    function Add(UniqueID : Int64; Value : Int64) : TBinaryTreeNode; overload;

    function Add(UniqueID : RawByteString; Value : String) : TBinaryTreeNode; overload;
    function Add(UniqueID : UnicodeString; Value : String) : TBinaryTreeNode; overload;
    function Add(UniqueID : Int64; Value : String) : TBinaryTreeNode; overload;
  end;

procedure LogMessage(Verbosity : TVerbosity; Message : String; Node : TBinaryTreeNode); overload;

implementation

procedure LogMessage(Verbosity: TVerbosity; Message: String;
  Node: TBinaryTreeNode);
begin
  if Assigned(Node) then
    LogMessage(Verbosity, Message + ' [' + Node.UniqueID + ']', Node.Data)
  else
    LogMessage(Verbosity, Message + '(nil)');
end;

const
  rcRed=False;
  rcBlack=True;

{ TBinaryTreeNode }

function TBinaryTreeNode.GetNext: TBinaryTreeNode;
var
  Hold : TBinaryTreeNode;
begin
  if Assigned(FGreater) then begin
    Result := FGreater;
    While Assigned(Result.FLesser) do
      Result:=Result.FLesser;
  end else begin
    Result:=Self;
    repeat
      Hold:=Result;
      Result:=Result.FParent;
    until (not Assigned(Result)) or (Hold=Result.FLesser);
  end;
end;

function TBinaryTreeNode.GetPrevious: TBinaryTreeNode;
var
  Hold : TBinaryTreeNode;
begin
  if Assigned(FLesser) then begin
    Result := FLesser;
    While Assigned(Result.FGreater) do
      Result:=Result.FGreater;
  end else begin
    Result:=Self;
    repeat
      Hold:=Result;
      Result:=Result.FParent;
    until (not Assigned(Result)) or (Hold=Result.FGreater);
  end;
end;

procedure TBinaryTreeNode.SetData(AValue: TArrayOfByte);
begin
  if FData=AValue then Exit;
//   LogMessage(vbExcessive, TAB + UniqueID + ' = ', AValue);
  FData:=Copy(AValue, 0, Length(AValue));
  if Assigned(FOwner) then FOwner.FModified:=True;
end;

procedure TBinaryTreeNode.SetData32(AValue: TArrayOfInt32);
begin
  if FData32=AValue then Exit;
  FData32:=Copy(AValue, 0, Length(AValue));;
  if Assigned(FOwner) then FOwner.FModified:=True;
end;

procedure TBinaryTreeNode.SetFlags(AValue: dword);
begin
  if FFlags=AValue then Exit;
  FFlags:=AValue;
  if Assigned(FOwner) then FOwner.FModified:=True;
end;

procedure TBinaryTreeNode.SetSubTree(AValue: TCustomBinaryTree);
begin
  if FSubTree=AValue then Exit;
  FSubTree:=AValue;
  if Assigned(FOwner) then FOwner.FModified:=True;
end;

procedure TBinaryTreeNode.SetText(AValue: String);
begin
  if FText=AValue then Exit;
  FText:=AValue;
end;

procedure TBinaryTreeNode.SetValue(AValue: Int64);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  if Assigned(FOwner) then FOwner.FModified:=True;
end;

constructor TBinaryTreeNode.Create(const AUniqueID : RawByteString);
begin
  inherited Create;
  FUniqueID:=AUniqueID;
  FOwner:=nil;
  FParent:=nil;
  FLesser:=nil;
  FGreater:=nil;
  FSubTree:=nil;
  FData:=[];
  FData32:=[];
  FFlags:=0;
  FValue:=0;
  FText:='';
  FColor:=rcRed;
end;

destructor TBinaryTreeNode.Destroy;
begin
  if Assigned(FParent) then begin
    if FParent.FLesser = Self then
      FParent.FLesser := nil
    else if FParent.FGreater = Self then
      FParent.FGreater := nil;
    FParent:=nil;
  end;
  if Assigned(FSubTree) then FreeAndNil(FSubTree);
  if Assigned(FLesser) then FreeAndNil(FLesser);
  if Assigned(FGreater) then FreeAndNil(FGreater);
  inherited Destroy;
end;

{ TCustomBinaryTree }

function TCustomBinaryTree.GetFirst: TBinaryTreeNode;
begin
  Result := FRoot;
  if Assigned(Result) then
    while Assigned(Result.FLesser) do
      Result:=Result.FLesser;
end;

function TCustomBinaryTree.GetLast: TBinaryTreeNode;
begin
  Result := FRoot;
  if Assigned(Result) then
    while Assigned(Result.FGreater) do
      Result:=Result.FGreater;
end;

procedure TCustomBinaryTree.SetModified(AValue: boolean);
begin
  if FModified=AValue then Exit;
  FModified:=AValue;
end;

procedure TCustomBinaryTree.Attach(var Node: TBinaryTreeNode);
var
  This, Hold : TBinaryTreeNode;
begin
  if not Assigned(Node) then
    raise Exception.Create('cannot attach null Binary Tree node');

  if Assigned(Node.FOwner) then
    raise Exception.Create('node is already attach to a Binary Tree');

  FModified:=True;

  Node.FColor:=rcRed;

  Hold:=nil;
  This:=FRoot;
  While Assigned(This) do begin
    if This.FUniqueID = Node.FUniqueID then begin
      // FUniqueID cannot already exist in tree.
      FreeAndNil(Node);
      Exit;
    end;
    Hold:=This;
    if Node.FUniqueID < This.FUniqueID then
      This:=This.FLesser
    else
      This:=This.FGreater;
  end;
  // Add to tree under Hold Node (or new FRoot)
  if Not Assigned(Hold) then
    FRoot:=Node
  else if Node.FUniqueID < Hold.FUniqueID then
    Hold.FLesser:=Node
  else
    Hold.FGreater:=Node;
  Node.FParent:=Hold;
  Node.FOwner:=Self;

  Inc(FCount);
  SelfBalance(Node);
end;

procedure TCustomBinaryTree.Detach(var Node: TBinaryTreeNode);
begin
  if not DetachSafe(Node) then Exit;
  FModified:=True;
  Node.FOwner := nil;

  if Assigned(Node.FParent) then begin
    if Node.FParent.FLesser=Node then
      Node.FParent.FLesser:=nil
    else
      Node.FParent.FGreater:=nil;
    Node.FParent:=nil;
  end else begin
    FRoot:=nil;
  end;

  Dec(FCount);

  if Assigned(Node.FLesser) then begin
    Dec(FCount);
    Node.FLesser.FOwner:= nil;
    Node.FLesser.FParent:= nil;
    Attach(Node.FLesser);
    Node.FLesser:=nil;
  end;
  if Assigned(Node.FGreater) then begin
    Dec(FCount);
    Node.FGreater.FOwner:= nil;
    Node.FGreater.FParent:= nil;
    Attach(Node.FGreater);
    Node.FGreater:=nil;
  end;
end;

function TCustomBinaryTree.DetachSafe(const Node: TBinaryTreeNode) : boolean;
begin
  Result:=False;
  if not Assigned(Node) then begin
    raise Exception.Create('cannot detach null Binary Tree node');
    exit;
  end;

  if Node.FOwner <> Self then begin
    if Assigned(Node.FOwner) then
      raise Exception.Create('cannot detach a node from different Binary Tree')
    else
      raise Exception.Create('cannot detach orphaned Binary Tree node');
    exit;
  end;
  Result:=True;
end;

procedure TCustomBinaryTree.RotateLeft(const Node: TBinaryTreeNode);
var
  G: TBinaryTreeNode;
begin
  G := Node.FGreater;

  Node.FGreater := G.FLesser;
  if Assigned(G.FLesser) then
     G.FLesser.FParent := Node;

  G.FParent := Node.FParent;

  if Node.FParent = nil then
     FRoot := G
  else if Node = Node.FParent.FLesser then
     Node.FParent.FLesser := G
  else
    Node.FParent.FGreater := G;

  G.FLesser := Node;
  Node.FParent := G;
end;

procedure TCustomBinaryTree.RotateRight(const Node: TBinaryTreeNode);
var
  L: TBinaryTreeNode;
begin
  L := Node.FLesser;
  Node.FLesser := L.FGreater;
  if Assigned(L.FGreater) then
    L.FGreater.FParent := Node;
  L.FParent := Node.FParent;
  if Node.FParent = nil then
    FRoot := L
  else if Node = Node.FParent.FLesser then
    Node.FParent.FLesser := L
  else
    Node.FParent.FGreater := L;
  L.FGreater := Node;
  Node.FParent := L;
end;

procedure TCustomBinaryTree.Transplant(U, V: TBinaryTreeNode);
begin
  if U.FParent = nil then
    FRoot := V
  else if U = U.FParent.FLesser then
    U.FParent.FLesser := V
  else
    U.FParent.FGreater := V;
  if V <> nil then
    V.FParent := U.FParent;
end;

procedure TCustomBinaryTree.SelfBalance(Node: TBinaryTreeNode);
begin
  { Do nothing }
  IgnoreParameter(Node);
end;

procedure TCustomBinaryTree.DeleteBalance(Node, Parent: TBinaryTreeNode);
begin
  { Do nothing }
  IgnoreParameter([Node, Parent]);
end;

function TCustomBinaryTree.StreamID: String;
begin
  Result:='CBTX1';
end;

procedure TCustomBinaryTree.WriteStream(Stream: TStream);
begin
  try
    WriteHead(Stream);
    WriteNodes(Stream, Root);
    WriteNodes(Stream, nil);
    WriteTail(Stream);
    FModified:=False;
  except
    on E : Exception do begin
      LogMessage(vbExcessive, E.Message);
    end;
  end;
end;

procedure TCustomBinaryTree.WriteHead(Stream: TStream);
var
  S : String;
  C : Integer;
begin
  S:=StreamID;
  Stream.Write(Pointer(S)^, Length(S));
  C:=Count;
  Stream.Write(C, Sizeof(C));
end;

procedure TCustomBinaryTree.WriteNodes(Stream: TStream; Node: TBinaryTreeNode);
var
  C : Integer;
begin
  if Not Assigned(Node) then begin
    C:=0;
    Stream.Write(C, Sizeof(C));
    Exit;
  end;
  C:=Length(Node.UniqueID);
  Stream.Write(C, Sizeof(C));
  Stream.Write(Pointer(Node.UniqueID)^, C);
  WriteNodeData(Stream, Node);
  if Assigned(Node.FLesser) then WriteNodes(Stream, Node.FLesser);
  if Assigned(Node.FGreater) then WriteNodes(Stream, Node.FGreater);
end;

procedure TCustomBinaryTree.WriteNodeData(Stream: TStream; Node: TBinaryTreeNode
  );
var
  C, I : integer;
  B : Byte;
  S : String;
begin
  Stream.Write(Node.Flags, Sizeof(Node.Flags));
  Stream.Write(Node.Value, Sizeof(Node.Value));
  C:=Length(Node.Data);
  Stream.Write(C, Sizeof(C));
  for I := 0 to C - 1 do
    Stream.Write(Node.Data[I], Sizeof(Byte));
  C:=Length(Node.Data32);
  Stream.Write(C, Sizeof(C));
  for I := 0 to C - 1 do
    Stream.Write(Node.Data32[I], Sizeof(Int32));
  if not Assigned(Node.SubTree) then begin
    B:=0;
    Stream.Write(B, Sizeof(B));
  end else begin
    S:=Node.SubTree.ClassName;
    B:=Length(S);
    Stream.Write(B, Sizeof(B));
    Stream.Write(Pointer(S)^, B);
    WriteSubTree(Stream, Node);
  end;
end;

procedure TCustomBinaryTree.WriteSubTree(Stream: TStream; Node: TBinaryTreeNode
  );
begin
  Node.SubTree.WriteStream(Stream);
end;

procedure TCustomBinaryTree.WriteTail(Stream: TStream);
begin
  { nothing to see here, move along }
  IgnoreParameter(Stream);
end;

procedure TCustomBinaryTree.ReadStream(Stream: TStream);
var
  C, R : integer;
begin
  try
    FModified:=False;
    ReadHead(Stream, C);
    ReadNodes(Stream, C, R);
    ReadTail(Stream);
    if C < R then
      raise Exception.Create('expected node count not satisfied');
    if C > R then
      raise Exception.Create('expected node count was exceeded');
  except
    on E : Exception do begin
      Clear;
      LogMessage(vbExcessive, E.Message);
    end;
  end;
end;

procedure TCustomBinaryTree.ReadHead(Stream: TStream; out ExpectedCount : integer);
var
  S : String;
begin
  ExpectedCount:=-1;
  S:='';
  SetLength(S, Length(StreamID));
  Stream.Read(Pointer(S)^, Length(StreamID));
  if S <> StreamID then
    raise Exception.Create('invalid stream header');
  Stream.Read(ExpectedCount, Sizeof(ExpectedCount));
end;

procedure TCustomBinaryTree.ReadNodes(Stream: TStream; ExpectedCount : integer;
  out ReadCount : Integer);
var
  C : Integer;
  S : String;
  Node : TBinaryTreeNode;
begin
  // Allow a little past expected count to test for nil node and probably
  // corrupt file.
  ReadCount:=0;
  while ReadCount <= ExpectedCount + 1 do begin
    C:=-1;
    Stream.Read(C, Sizeof(C));
    if C < 1 then Exit;
    Inc(ReadCount);
    S:='';
    SetLength(S, C);
    Stream.Read(Pointer(S)^, C);
    Node:=Add(S);
    if not Assigned(Node) then
      raise Exception.Create('unable to create node: ' + S);
    ReadNodeData(Stream, Node);
  end;
end;

procedure TCustomBinaryTree.ReadNodeData(Stream: TStream; Node: TBinaryTreeNode
  );
var
  DF : DWord;
  DV : Int64;
  DB : TArrayOfByte;
  DD : TArrayOfInt32;
  C, I : integer;
  B : Byte;
  S : String;
begin
  DF:=0;
  DV:=0;
  DB:=[];
  DD:=[];
  C:=0;
  B:=0;
  S:='';
  Stream.Read(DF, Sizeof(DF));
  Node.FFlags:=DF;
  Stream.Read(DV, Sizeof(DV));
  Node.FValue:=DV;

  Stream.Read(C, Sizeof(C));
  SetLength(DB, C);
  for I := 0 to C - 1 do
    Stream.Read(DB[I], Sizeof(Byte));
  Node.Data:=Copy(DB, 0, C);

  Stream.Read(C, Sizeof(C));
  SetLength(DD, C);
  for I := 0 to C - 1 do
    Stream.Read(DD[I], Sizeof(Int32));
  Node.Data32:=Copy(DD, 0, C);

  Stream.Read(B, Sizeof(B));
  if B > 0 then begin
    SetLength(S, B);
    Stream.Read(Pointer(S)^, B);
    ReadSubTree(Stream, Node, S);
  end;
end;

procedure TCustomBinaryTree.ReadSubTree(Stream: TStream; Node: TBinaryTreeNode;
  TreeClass: ShortString);
begin
  case TreeClass of
    'TCustomBinaryTree' : begin
      Node.SubTree:=TCustomBinaryTree.Create;
      Node.SubTree.ReadStream(Stream);
    end;
    'TRedBlackBinaryTree' : begin
      Node.SubTree:=TRedBlackBinaryTree.Create;
      Node.SubTree.ReadStream(Stream);
    end;
    'TBinaryTree' : begin
      Node.SubTree:=TBinaryTree.Create;
      Node.SubTree.ReadStream(Stream);
    end
  else
    raise Exception.Create('unknown SubTree class');
  end;
end;

procedure TCustomBinaryTree.ReadTail(Stream: TStream);
begin
  { nothing to see here, move along }
  IgnoreParameter(Stream);
end;

procedure TCustomBinaryTree.SaveToFile(FileName: String);
var
  S : TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    Optimize;
    WriteStream(S);
    Modified:=False;
  finally
    S.Free;
  end;
end;

procedure TCustomBinaryTree.LoadFromFile(FileName: String);
var
  S : TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ReadStream(S);
    Optimize;
    Modified:=False;
  finally
    S.Free;
  end;
end;

constructor TCustomBinaryTree.Create;
begin
  inherited Create;
  FRoot:=nil;
  FCount:=0;
  FModified:=False;
end;

destructor TCustomBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCustomBinaryTree.Clear;
begin
  FCount:=0;
  if Assigned(FRoot) then FreeAndNil(FRoot);
end;

procedure TCustomBinaryTree.Optimize;
var
  Nodes : array of TBinaryTreeNode;

  procedure Gather;
  var
    I : integer;
    N : TBinaryTreeNode;
  begin
    SetLength(Nodes, Count);
    N:=GetFirst;
    for I := 0 to Count - 1 do begin
      Nodes[I]:=N;
      N:=N.GetNext;
    end;
  end;

  procedure Orphan;
  var
    I : integer;
  begin
    FRoot:=nil;
    FCount:=0;
    for I := Low(Nodes) to High(Nodes) do begin
      Nodes[I].FOwner:=nil;
      Nodes[I].FParent:=nil;
      Nodes[I].FLesser:=nil;
      Nodes[I].FGreater:=nil;
    end;
  end;

  procedure Rettach(L, H : integer);
  var
    M : integer;
  begin
    M := L + ((H - L) div 2);
    Attach(Nodes[M]);
    if M <> L then Rettach(L, M - 1);
    if M <> H then Rettach(M + 1, H);
  end;

begin
  if Count < 3 then Exit;
  If Count > 100000 then Exit;
  try
    Gather;
    Orphan;
    Rettach(Low(Nodes),High(Nodes));
  except
  end;
  SetLength(Nodes, 0);
end;

function TCustomBinaryTree.CheckIntegrity: boolean;
var
  N : TBinaryTreeNode;
  C : integer;
begin
  C:=0;
  N:=First;
  while Assigned(N) do begin
    Inc(C);
    N:=N.Next;
  end;
  Result:=FCount=C;
  if not Result then Exit;
  N:=Last;
  while Assigned(N) do begin
    Dec(C);
    N:=N.Previous;
  end;
  Result:=C=0;
end;

function TCustomBinaryTree.Add(UniqueID: RawByteString): TBinaryTreeNode;
begin
  Result:=nil;
  if UniqueID='' then Exit;
  Result := TBinaryTreeNode.Create(UniqueID);
  Attach(Result);
//  if Assigned(Result) then
  // LogMessage(vbExcessive, UniqueID + ':' + Result.UniqueID);
  // May return nil if UniqueID already exists in tree
end;

function TCustomBinaryTree.Add(UniqueID: UnicodeString): TBinaryTreeNode;
begin
  Result:=Add(RawByteString(UniqueID));
end;

function TCustomBinaryTree.Add(UniqueID: Int64): TBinaryTreeNode;
begin
  Result:=Add('$'+HexStr(UniqueID,8));
end;

function TCustomBinaryTree.Find(UniqueID: RawByteString; CaseSensitive : boolean = true): TBinaryTreeNode;
var
  T : RawByteString;
begin
  Result := FRoot;
  While Assigned(Result) do begin
    if UniqueID = Result.FUniqueID then Exit;
    if UniqueID < Result.FUniqueID then
      Result := Result.FLesser
    else
      Result := Result.FGreater;
  end;
  // did not find an exact match
  if CaseSensitive then Exit;
  UniqueID:=UpperCase(UniqueID);
  Result := FRoot;
   While Assigned(Result) do begin
     T := UpperCase(Result.FUniqueID);
     if UniqueID = T then Exit;
     if UniqueID < T then
       Result := Result.FLesser
     else
       Result := Result.FGreater;
   end;
   // did not find case insensitive match
end;

function TCustomBinaryTree.Find(UniqueID: UnicodeString; CaseSensitive: boolean
  ): TBinaryTreeNode;
var
  R : RawByteString;
begin
  // Exact match can just use the RawByteString find.
  Result:=Find(RawByteString(UniqueID),true);
  if Assigned(Result) or CaseSensitive then Exit;
  // Unique IDs are stored as RawByteString.
  // But, the UpperCase function could behave differently when
  // processing RawByteString and UnicodeString. So, we should
  // use the Unicode version for comparison and the Raw version
  // for tree traversal.
  R:=RawByteString(UniqueID);
  UniqueID:=UpperCase(UniqueID);
  Result := FRoot;
   While Assigned(Result) do begin
     if UniqueID = UpperCase(UnicodeString(Result.FUniqueID)) then Exit;
     if R < Result.FUniqueID then
       Result := Result.FLesser
     else
       Result := Result.FGreater;
   end;
   // did not find case insensitive match
end;

function TCustomBinaryTree.Find(UniqueID: Int64): TBinaryTreeNode;
begin
  Result:=Find('$'+HexStr(UniqueID,8),true);
end;

procedure TCustomBinaryTree.Delete(var Node: TBinaryTreeNode);
begin
  if not DetachSafe(Node) then Exit;
  Detach(Node);
  FreeAndNil(Node);
end;

procedure TCustomBinaryTree.Foreach(Callback: TBinaryTreeIterator);
var
  N, NN: TBinaryTreeNode;
begin
  if not Assigned(Callback) then Exit;
  N := GetFirst;
  while Assigned(N) do begin
    NN := N.GetNext;
    Callback(N);
    N := NN;
  end;
end;

{ TRedBlackBinaryTree }

function TRedBlackBinaryTree.StreamID: String;
begin
  Result:=inherited StreamID;
  // Since the trees are compatible, we will keep the same stream ID.
  // Result:='RBTX1';
end;

procedure TRedBlackBinaryTree.Detach(var Node: TBinaryTreeNode);
var
  A, B, P : TBinaryTreeNode;
  C : Boolean;
begin
  if not DetachSafe(Node) then Exit;
  FModified:=True;
  B:=Node;
  C:=B.FColor;
  if not Assigned(Node.FLesser) then begin
    A := Node.FGreater;
    P:=Node.FParent;
    Transplant(Node, Node.FGreater);
  end else if not Assigned(Node.FGreater) then begin
    A := Node.FLesser;
    P:=Node.FParent;
    Transplant(Node, Node.FLesser);
  end else begin
    B := Node.FGreater;
    while Assigned(B.FLesser) do
      B := B.FLesser;
    C := B.FColor;
    A := B.FGreater;

    if B.FParent = Node then
      P := B
    else
      P := B.FParent;

    if B.FParent = Node then begin
      if Assigned(A) then
        A.FParent := B;
    end else begin
      Transplant(B, B.FGreater);
      B.FGreater := Node.FGreater;
      B.FGreater.FParent := B;
    end;
    Transplant(Node, B);
    B.FLesser := Node.FLesser;
    B.FLesser.FParent := B;
    B.FColor := Node.FColor;
  end;

  Dec(FCount);

  if (C = rcBlack) then
    DeleteBalance(A, P);
  Node.FOwner := nil;
  Node.FParent := nil;
  Node.FLesser := nil;
  Node.FGreater := nil;
end;

procedure TRedBlackBinaryTree.SelfBalance(Node: TBinaryTreeNode);
var
  U, GP: TBinaryTreeNode;
begin
  while (Assigned(Node.FParent)) and (Node.FParent.FColor=rcRed) do begin
    GP:=Node.FParent.FParent;
    if not Assigned(GP) then Break;

    if Node.FParent=GP.FLesser then begin
      U:=GP.FGreater;
      if (Assigned(U)) and (U.FColor=rcRed) then begin
        Node.FParent.FColor:=rcBlack;
        U.FColor:=rcBlack;
        GP.FColor:= rcRed;
        Node:=GP;
      end else begin
        if Node=Node.FParent.FGreater then begin
          Node:=Node.FParent;
          RotateLeft(Node);
        end;
        Node.FParent.FColor:=rcBlack;
        GP.FColor:=rcRed;
        RotateRight(GP);
      end;
    end else begin
      U:=GP.FLesser;
      if (Assigned(U)) and (U.FColor=rcRed) then begin
        Node.FParent.FColor:=rcBlack;
        U.FColor:=rcBlack;
        GP.FColor:=rcRed;
        Node:=GP;
      end else begin
        if Node=Node.FParent.FLesser then begin
          Node:=Node.FParent;
          RotateRight(Node);
        end;
        Node.FParent.FColor:=rcBlack;
        GP.FColor:=rcRed;
        RotateLeft(GP);
      end;
    end;
  end;
  FRoot.FColor:=rcBlack;
end;

procedure TRedBlackBinaryTree.DeleteBalance(Node, Parent: TBinaryTreeNode);
var
  S: TBinaryTreeNode;
begin

  while (Node <> FRoot) and Assigned(Parent) and
  ((Not Assigned(Node)) or (Node.FColor = rcBlack)) do begin
    if Node = Parent.FLesser then begin
      S:=Parent.FGreater;
      if Assigned(S) and (S.FColor = rcRed) then begin
        S.FColor:=rcBlack;
        Parent.FColor:=rcRed;
        RotateLeft(Parent);
        S:=Parent.FGreater;
      end;
      if (Not Assigned(S)) or (
      ((Not Assigned(S.FLesser)) or (S.FLesser.FColor = rcBlack)) and
      ((Not Assigned(S.FGreater)) or (S.FGreater.FColor = rcBlack)) ) then begin
        if Assigned(S) then
          S.FColor:=rcRed;
        Node:=Parent;
        Parent:=Node.Parent;
      end else begin
        if (Not Assigned(S.FGreater)) or (S.FGreater.FColor = rcBlack) then begin
          if Assigned(S.FLesser) then
            S.FLesser.FColor:=rcBlack;
          S.FColor:=rcRed;
          RotateRight(S);
          S:=Parent.FGreater;
        end;
        S.FColor:=Parent.FColor;
        Parent.FColor:=rcBlack;
        if Assigned(S.FGreater) then
          S.FGreater.FColor:=rcBlack;
        RotateLeft(Parent);
        Node:=FRoot;
      end;
    end else begin
      S:=Parent.FLesser;
      if Assigned(S) and (S.FColor = rcRed) then begin
        S.FColor:=rcBlack;
        Parent.FColor:=rcRed;
        RotateRight(Parent);
        S:=Parent.FLesser;
      end;
      if (Not Assigned(S)) or (
      ((Not Assigned(S.FGreater)) or (S.FGreater.FColor = rcBlack)) and
      ((Not Assigned(S.FLesser)) or (S.FLesser.FColor = rcBlack)) ) then begin
        if Assigned(S) then
          S.FColor:=rcRed;
        Node:=Parent;
        Parent:=Node.FParent;
      end else begin
        if (Not Assigned(S.FLesser)) or (S.FLesser.FColor = rcBlack) then begin
          if Assigned(S.FGreater) then S.FGreater.FColor:=rcBlack;
          S.FColor:=rcRed;
          RotateLeft(S);
          S:=Parent.FLesser;
        end;
        S.FColor:=Parent.FColor;
        Parent.FColor:=rcBlack;
        if Assigned(S.FLesser) then
          S.FLesser.FColor:=rcBlack;
        RotateRight(Parent);
        Node:=FRoot;
      end;
    end;
  end;
  if Assigned(Node) then Node.FColor:=rcBlack;
end;

function TRedBlackBinaryTree.CheckIntegrity: boolean;

  function GetBlackHeight(Node: TBinaryTreeNode): integer;
  var
    LH, RH: integer;
  begin
    if Node = nil then Exit(1);
    if Node.FColor = rcRed then begin
      if (Assigned(Node.FLesser) and (Node.FLesser.FColor = rcRed)) or
      (Assigned(Node.FGreater) and (Node.FGreater.FColor = rcRed)) then
        raise Exception.Create('Red-Red Violation at ' + Node.FUniqueID);
    end;
    LH := GetBlackHeight(Node.FLesser);
    RH := GetBlackHeight(Node.FGreater);
    if LH <> RH then
      raise Exception.Create('Black-Height Mismatch at ' + Node.FUniqueID);
    Result := LH;
    if Node.FColor = rcBlack then Inc(Result);
  end;

begin
  Result:=True;
  if FRoot = nil then Exit;
  try
    if FRoot.FColor = rcRed then
      raise Exception.Create('Root is Red');
    GetBlackHeight(FRoot);
  except
    on E: Exception do begin
      LogMessage(vbCritical, 'Binary tree failed integrity check: ' + E.Message);
      Result := False;
    end;
  end;
end;

{ TBinaryTree }

function TBinaryTree.Add(UniqueID: RawByteString; Data: TArrayOfByte
  ): TBinaryTreeNode;
begin
  Result:=Add(UniqueID);
  if Assigned(Result) then
    Result.Data:=Data;
end;

function TBinaryTree.Add(UniqueID: UnicodeString; Data: TArrayOfByte
  ): TBinaryTreeNode;
begin
  Result:=Add(UniqueID);
  if Assigned(Result) then
    Result.Data:=Data;
end;

function TBinaryTree.Add(UniqueID: Int64; Data: TArrayOfByte): TBinaryTreeNode;
begin
  Result:=nil;
  if UniqueID<0 then Exit;
  Result := TBinaryTreeNode.Create('$'+HexStr(UniqueID,8));
  Result.Data:=Data;
  Attach(Result);
end;

function TBinaryTree.Add(UniqueID: RawByteString; Data32: TArrayOfInt32
  ): TBinaryTreeNode;
begin
  Result:=Add(UniqueID);
  if Assigned(Result) then
    Result.Data32:=Data32;
end;

function TBinaryTree.Add(UniqueID: UnicodeString; Data32: TArrayOfInt32
  ): TBinaryTreeNode;
begin
  Result:=Add(UniqueID);
  if Assigned(Result) then
    Result.Data32:=Data32;
end;

function TBinaryTree.Add(UniqueID: Int64; Data32: TArrayOfInt32
  ): TBinaryTreeNode;
begin
  Result:=nil;
  if UniqueID<0 then Exit;
  Result := TBinaryTreeNode.Create('$'+HexStr(UniqueID,8));
  Result.Data32:=Data32;
  Attach(Result);
end;

function TBinaryTree.Add(UniqueID: RawByteString; Value: Int64
  ): TBinaryTreeNode;
begin
  Result:=Add(UniqueID);
  if Assigned(Result) then
    Result.FValue:=Value;
end;

function TBinaryTree.Add(UniqueID: UnicodeString; Value: Int64
  ): TBinaryTreeNode;
begin
  Result:=Add(UniqueID);
  if Assigned(Result) then
    Result.FValue:=Value;
end;

function TBinaryTree.Add(UniqueID: Int64; Value: Int64): TBinaryTreeNode;
begin
  Result:=nil;
  if UniqueID<0 then Exit;
  Result := TBinaryTreeNode.Create('$'+HexStr(UniqueID,8));
  if not Assigned(Result) then Exit;
  Result.FValue:=Value;
  Attach(Result);
end;

function TBinaryTree.Add(UniqueID: RawByteString; Value: String
  ): TBinaryTreeNode;
begin
  Result:=Add(UniqueID);
  if Assigned(Result) then
    Result.FText:=Value;
end;

function TBinaryTree.Add(UniqueID: UnicodeString; Value: String
  ): TBinaryTreeNode;
begin
  Result:=Add(UniqueID);
  if Assigned(Result) then
    Result.FText:=Value;
end;

function TBinaryTree.Add(UniqueID: Int64; Value: String): TBinaryTreeNode;
begin
  Result:=Add(UniqueID);
  if Assigned(Result) then
    Result.FText:=Value;
end;


end.
