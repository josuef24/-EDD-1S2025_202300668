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

    Email   : AnsiString;   // clave de ordenamiento
    Username: AnsiString;
    Nombre  : AnsiString;
    Activo  : Boolean;      // para “borrado lógico” (true=visible)
  end;

  TFavVisitProc = procedure(const It: TFavItem) of object;
  TFavVisitProcPlain = procedure(const It: TFavItem); // (para procedimientos no-método)

  PBNode = ^TBNode;
  TBNode = record
    Leaf : Boolean;
    N    : Integer;                           // # claves usadas
    Key  : array[1..B_MAX_KEYS] of TFavItem;  // claves
    C    : array[0..B_ORDER-1] of PBNode;     // hijos (N+1 posibles)
  end;

procedure BInit(var Root: PBNode);
function  BSearch(Root: PBNode; const Email: AnsiString; out Item: TFavItem): Boolean;
procedure BInsert(var Root: PBNode; const Item: TFavItem);
// “Eliminación” por ahora es lógica (marca Activo := false):
function  BDeleteLogical(Root: PBNode; const Email: AnsiString): Boolean;

procedure BTraverseInOrder(Root: PBNode; Proc: TFavVisitProc);
procedure BTraverseInOrderPlain(Root: PBNode; Proc: TFavVisitProcPlain); // opcional


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

// ---------------------------------------------------------
// B-Tree SEARCH (estándar)
// ---------------------------------------------------------
function BSearch(Root: PBNode; const Email: AnsiString; out Item: TFavItem): Boolean;
var i: Integer; x: PBNode;
begin
  Result := False; x := Root;
  while x <> nil do
  begin
    i := 1;
    while (i <= x^.N) and (CompareKey(Email, x^.Key[i].Email) > 0) do Inc(i);

    if (i <= x^.N) and (CompareKey(Email, x^.Key[i].Email) = 0) then
    begin
      if x^.Key[i].Activo then begin Item := x^.Key[i]; Exit(True); end
      else Exit(False);
    end;

    if x^.Leaf then Exit(False)
    else x := x^.C[i-1];
  end;
end;

// ---------------------------------------------------------
// B-Tree INSERT (split + insertNonFull) estándar
// ---------------------------------------------------------
procedure SplitChild(x: PBNode; i: Integer);
var z, y: PBNode; t: Integer;
begin
  // x es nodo padre; vamos a dividir el hijo y = x^.C[i]
  y := x^.C[i];
  z := NewNode(y^.Leaf);

  // Para orden 5 (max 4 claves), promovemos la clave 3 (índice 3) si usamos t=ceil(m/2)=3.
  // En esta implementación práctica, movemos las 2 claves altas (3,4) a z.
  z^.N := 2;             // mover 2 claves (las más altas)
  z^.Key[1] := y^.Key[3];
  z^.Key[2] := y^.Key[4];

  if not y^.Leaf then
  begin
    z^.C[0] := y^.C[3-1]; // hijos 2 y 3 (ajuste de índices)
    z^.C[1] := y^.C[3];
    z^.C[2] := y^.C[4];
  end;

  y^.N := 2; // y se queda con 2 claves

  // hacer espacio en x para un nuevo hijo
  for t := x^.N downto i+1 do
    x^.C[t] := x^.C[t-1];
  x^.C[i] := z;

  // desplazar claves en x
  for t := x^.N downto i do
    x^.Key[t+1] := x^.Key[t];

  // subir clave media (posición 3 de y) a x
  x^.Key[i] := y^.Key[2]; // ojo: ya dejamos y con 2, pero “media” efectiva era antigua 3ra.
  Inc(x^.N);
end;

procedure InsertNonFull(x: PBNode; const k: TFavItem);
var i: Integer;
begin
  i := x^.N;
  if x^.Leaf then
  begin
    // mover a la derecha las claves mayores
    while (i >= 1) and (CompareKey(k.Email, x^.Key[i].Email) < 0) do
    begin
      x^.Key[i+1] := x^.Key[i];
      Dec(i);
    end;
    x^.Key[i+1] := k;
    Inc(x^.N);
  end
  else
  begin
    while (i >= 1) and (CompareKey(k.Email, x^.Key[i].Email) < 0) do Dec(i);
    Inc(i);
    // si el hijo está lleno, dividir antes de bajar
    if x^.C[i-1]^.N = B_MAX_KEYS then
    begin
      SplitChild(x, i);
      if CompareKey(k.Email, x^.Key[i].Email) > 0 then Inc(i);
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

// ---------------------------------------------------------
// “Delete” LÓGICO: marca Activo := false (rápido y seguro)
// ---------------------------------------------------------
function BDeleteLogical(Root: PBNode; const Email: AnsiString): Boolean;
var x: PBNode; i: Integer;
begin
  Result := False; x := Root;
  while x <> nil do
  begin
    i := 1;
    while (i <= x^.N) and (CompareKey(Email, x^.Key[i].Email) > 0) do Inc(i);
    if (i <= x^.N) and (CompareKey(Email, x^.Key[i].Email) = 0) then
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

// ---------------------------------------------------------
// Recorrido InOrder
// ---------------------------------------------------------
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

