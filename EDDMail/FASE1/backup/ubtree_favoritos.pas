unit UBTree_Favoritos;

{$mode ObjFPC}{$H+}

interface

const
  B_ORDER      = 5;              // máximo hijos por nodo
  B_MAX_KEYS   = B_ORDER - 1;    // 4
  B_MIN_KEYS   = (B_ORDER div 2); // 2 (para orden 5)

type
  PFavItem = ^TFavItem;
  TFavItem = record
    // DATOS DEL CORREO FAVORITO
    ID        : Integer;      // ID único del correo
    Remitente : AnsiString;   // quien envió
    Asunto    : AnsiString;   // asunto del correo - CLAVE DE ORDENAMIENTO
    Fecha     : AnsiString;   // fecha del correo
    Mensaje   : AnsiString;   // cuerpo del mensaje
    Activo    : Boolean;      // para "borrado lógico" (true=visible)
  end;

  TFavVisitProc = procedure(const It: TFavItem) of object;
  TFavVisitProcPlain = procedure(const It: TFavItem);

  PBNode = ^TBNode;
  TBNode = record
    Leaf : Boolean;
    N    : Integer;
    Key  : array[1..B_MAX_KEYS] of TFavItem;
    C    : array[0..B_ORDER-1] of PBNode;
  end;

procedure BInit(var Root: PBNode);
function  BSearch(Root: PBNode; const Asunto: AnsiString; out Item: TFavItem): Boolean;
procedure BInsert(var Root: PBNode; const Item: TFavItem);
function  BDeleteLogical(Root: PBNode; const Asunto: AnsiString): Boolean;
procedure BTraverseInOrder(Root: PBNode; Proc: TFavVisitProc);
procedure BTraverseInOrderPlain(Root: PBNode; Proc: TFavVisitProcPlain);

implementation

uses SysUtils;

procedure BInit(var Root: PBNode);
begin
  Root := nil;
end;

function NewNode(Leaf: Boolean): PBNode;
var i: Integer; P: PBNode;
begin
  New(P);
  P^.Leaf := Leaf;
  P^.N    := 0;
  for i:=0 to B_ORDER-1 do P^.C[i] := nil;
  Result := P;
end;

function CompareKey(const A, B: AnsiString): Integer; inline;
begin
  Result := AnsiCompareText(A, B);
end;

function BSearch(Root: PBNode; const Asunto: AnsiString; out Item: TFavItem): Boolean;
var i: Integer; x: PBNode;
begin
  Result := False; x := Root;
  while x <> nil do
  begin
    i := 1;
    while (i <= x^.N) and (CompareKey(Asunto, x^.Key[i].Asunto) > 0) do Inc(i);

    if (i <= x^.N) and (CompareKey(Asunto, x^.Key[i].Asunto) = 0) then
    begin
      if x^.Key[i].Activo then begin Item := x^.Key[i]; Exit(True); end
      else Exit(False);
    end;

    if x^.Leaf then Exit(False)
    else x := x^.C[i-1];
  end;
end;

procedure SplitChild(x: PBNode; i: Integer);
var z, y: PBNode; t: Integer;
begin
  y := x^.C[i];
  z := NewNode(y^.Leaf);

  z^.N := 2;
  z^.Key[1] := y^.Key[3];
  z^.Key[2] := y^.Key[4];

  if not y^.Leaf then
  begin
    z^.C[0] := y^.C[2];
    z^.C[1] := y^.C[3];
    z^.C[2] := y^.C[4];
  end;

  y^.N := 2;

  for t := x^.N downto i+1 do
    x^.C[t] := x^.C[t-1];
  x^.C[i] := z;

  for t := x^.N downto i do
    x^.Key[t+1] := x^.Key[t];

  x^.Key[i] := y^.Key[2];
  Inc(x^.N);
end;

procedure InsertNonFull(x: PBNode; const k: TFavItem);
var i: Integer;
begin
  i := x^.N;
  if x^.Leaf then
  begin
    while (i >= 1) and (CompareKey(k.Asunto, x^.Key[i].Asunto) < 0) do
    begin
      x^.Key[i+1] := x^.Key[i];
      Dec(i);
    end;
    x^.Key[i+1] := k;
    Inc(x^.N);
  end
  else
  begin
    while (i >= 1) and (CompareKey(k.Asunto, x^.Key[i].Asunto) < 0) do Dec(i);
    Inc(i);
    if x^.C[i-1]^.N = B_MAX_KEYS then
    begin
      SplitChild(x, i);
      if CompareKey(k.Asunto, x^.Key[i].Asunto) > 0 then Inc(i);
    end;
    InsertNonFull(x^.C[i-1], k);
  end;
end;

procedure BInsert(var Root: PBNode; const Item: TFavItem);
var r, s: PBNode; k: TFavItem;
begin
  k := Item;
  k.Activo := True;
  if Root = nil then
  begin
    Root := NewNode(True);
    Root^.N := 1;
    Root^.Key[1] := k;
    Exit;
  end;

  r := Root;
  if r^.N = B_MAX_KEYS then
  begin
    s := NewNode(False);
    Root := s;
    s^.N := 0;
    s^.C[0] := r;
    SplitChild(s, 1);
    InsertNonFull(s, k);
  end
  else
    InsertNonFull(r, k);
end;

function BDeleteLogical(Root: PBNode; const Asunto: AnsiString): Boolean;
var x: PBNode; i: Integer;
begin
  Result := False; x := Root;
  while x <> nil do
  begin
    i := 1;
    while (i <= x^.N) and (CompareKey(Asunto, x^.Key[i].Asunto) > 0) do Inc(i);
    if (i <= x^.N) and (CompareKey(Asunto, x^.Key[i].Asunto) = 0) then
    begin
      if x^.Key[i].Activo then
      begin
        x^.Key[i].Activo := False;
        Exit(True);
      end else Exit(False);
    end;
    if x^.Leaf then Exit(False) else x := x^.C[i-1];
  end;
end;

procedure BTraverseInOrder(Root: PBNode; Proc: TFavVisitProc);
var i: Integer;
begin
  if Root = nil then Exit;
  for i := 1 to Root^.N do
  begin
    if not Root^.Leaf then BTraverseInOrder(Root^.C[i-1], Proc);
    if Root^.Key[i].Activo then Proc(Root^.Key[i]);
  end;
  if not Root^.Leaf then BTraverseInOrder(Root^.C[Root^.N], Proc);
end;

procedure BTraverseInOrderPlain(Root: PBNode; Proc: TFavVisitProcPlain);
var i: Integer;
begin
  if Root = nil then Exit;
  for i := 1 to Root^.N do
  begin
    if not Root^.Leaf then BTraverseInOrderPlain(Root^.C[i-1], Proc);
    if Root^.Key[i].Activo then Proc(Root^.Key[i]);
  end;
  if not Root^.Leaf then BTraverseInOrderPlain(Root^.C[Root^.N], Proc);
end;

end.
