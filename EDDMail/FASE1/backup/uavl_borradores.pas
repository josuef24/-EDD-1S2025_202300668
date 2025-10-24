unit UAVL_Borradores;

{$mode ObjFPC}{$H+}

interface

type
  // Registro de borrador (evitamos chocar con tus tipos de Fase 1)
  PMailDraft = ^TMailDraft;
  TMailDraft = record
    ID          : LongInt;
    Remitente   : AnsiString;
    Destinatario: AnsiString;
    Asunto      : AnsiString;
    Mensaje     : AnsiString;


  end;

  type
  TVisitDraftProc = procedure(const D: TMailDraft) of object;

  // Nodo del AVL
  PAVLNode = ^TAVLNode;
  TAVLNode = record
    KeyID : LongInt;        // Usamos el ID del correo como clave
    Data  : TMailDraft;     // El borrador completo
    Height: SmallInt;       // Altura del nodo
    Left, Right: PAVLNode;  // Hijos
  end;

  // API mínima que vas a usar
procedure AVL_Init(var Root: PAVLNode);
function  AVL_Insert(var Root: PAVLNode; const D: TMailDraft): boolean; // false si duplicado (ID repetido)
function  AVL_Find(Root: PAVLNode; ID: LongInt; out D: TMailDraft): boolean;
function  AVL_Delete(var Root: PAVLNode; ID: LongInt): boolean;

procedure AVL_InOrder(Root: PAVLNode; Visit: TVisitDraftProc);
procedure AVL_PreOrder(Root: PAVLNode; Visit: TVisitDraftProc);
procedure AVL_PostOrder(Root: PAVLNode; Visit: TVisitDraftProc);

function AVL_Count(Root: PAVLNode): LongInt;


implementation


function AVL_Count(Root: PAVLNode): LongInt;
begin
  if Root=nil then exit(0);
  Result := 1 + AVL_Count(Root^.Left) + AVL_Count(Root^.Right);
end;


function Height(N: PAVLNode): SmallInt; inline;
begin
  if N=nil then Exit(0);
  Result := N^.Height;
end;

function Max(a,b: SmallInt): SmallInt; inline;
begin
  if a>b then Result:=a else Result:=b;
end;

function NewNode(const D: TMailDraft): PAVLNode;
var N: PAVLNode;
begin
  New(N);
  N^.KeyID  := D.ID;
  N^.Data   := D;
  N^.Height := 1;
  N^.Left   := nil;
  N^.Right  := nil;
  Result := N;
end;

function RotateRight(y: PAVLNode): PAVLNode;
var x, T2: PAVLNode;
begin
  x := y^.Left;  T2 := x^.Right;
  x^.Right := y; y^.Left := T2;
  y^.Height := Max(Height(y^.Left), Height(y^.Right)) + 1;
  x^.Height := Max(Height(x^.Left), Height(x^.Right)) + 1;
  Exit(x);
end;

function RotateLeft(x: PAVLNode): PAVLNode;
var y, T2: PAVLNode;
begin
  y := x^.Right; T2 := y^.Left;
  y^.Left := x;  x^.Right := T2;
  x^.Height := Max(Height(x^.Left), Height(x^.Right)) + 1;
  y^.Height := Max(Height(y^.Left), Height(y^.Right)) + 1;
  Exit(y);
end;

function GetBalance(N: PAVLNode): SmallInt; inline;
begin
  if N=nil then Exit(0);
  Result := Height(N^.Left) - Height(N^.Right);
end;

procedure AVL_Init(var Root: PAVLNode);
begin
  Root := nil;
end;

function _Insert(var Node: PAVLNode; const D: TMailDraft; out Inserted: boolean): PAVLNode;
var bal: SmallInt;
begin
  if Node=nil then begin
    Inserted:=true; Exit(NewNode(D));
  end;

  if D.ID < Node^.KeyID then
    Node^.Left  := _Insert(Node^.Left,  D, Inserted)
  else if D.ID > Node^.KeyID then
    Node^.Right := _Insert(Node^.Right, D, Inserted)
  else begin
    Inserted := false;     // ID duplicado
    Exit(Node);
  end;

  Node^.Height := Max(Height(Node^.Left), Height(Node^.Right)) + 1;
  bal := GetBalance(Node);

  // Casos de rebalanceo
  if (bal>1) and (D.ID < Node^.Left^.KeyID) then Exit(RotateRight(Node));         // LL
  if (bal<-1) and (D.ID > Node^.Right^.KeyID) then Exit(RotateLeft(Node));        // RR
  if (bal>1) and (D.ID > Node^.Left^.KeyID) then begin                             // LR
    Node^.Left := RotateLeft(Node^.Left);
    Exit(RotateRight(Node));
  end;
  if (bal<-1) and (D.ID < Node^.Right^.KeyID) then begin                            // RL
    Node^.Right := RotateRight(Node^.Right);
    Exit(RotateLeft(Node));
  end;

  Result := Node;
end;

function AVL_Insert(var Root: PAVLNode; const D: TMailDraft): boolean;
begin
  Root := _Insert(Root, D, Result);
end;

procedure AVL_InOrder(Root: PAVLNode; Visit: TVisitDraftProc);

begin
  if Root=nil then exit;
  AVL_InOrder(Root^.Left, Visit);
  Visit(Root^.Data);
  AVL_InOrder(Root^.Right, Visit);
end;

procedure AVL_PreOrder(Root: PAVLNode; Visit: TVisitDraftProc);

begin
  if Root=nil then exit;
  Visit(Root^.Data);
  AVL_PreOrder(Root^.Left, Visit);
  AVL_PreOrder(Root^.Right, Visit);
end;

procedure AVL_PostOrder(Root: PAVLNode; Visit: TVisitDraftProc);

begin
  if Root=nil then exit;
  AVL_PostOrder(Root^.Left, Visit);
  AVL_PostOrder(Root^.Right, Visit);
  Visit(Root^.Data);
end;

function AVL_Find(Root: PAVLNode; ID: LongInt; out D: TMailDraft): boolean;
begin
  while Root<>nil do
    if ID = Root^.KeyID then begin
      D := Root^.Data; exit(true);
    end else
    if ID < Root^.KeyID then Root := Root^.Left
                        else Root := Root^.Right;
  Result := false;
end;

function _MinNode(N: PAVLNode): PAVLNode;
begin
  while (N<>nil) and (N^.Left<>nil) do N := N^.Left;
  Exit(N);
end;

function AVL_Delete(var Root: PAVLNode; ID: LongInt): boolean;
  function _Delete(Node: PAVLNode; Key: LongInt; out Deleted: boolean): PAVLNode;
  var bal: SmallInt; temp: PAVLNode;
  begin
    if Node=nil then begin Deleted:=false; exit(nil); end;

    if Key < Node^.KeyID then
      Node^.Left := _Delete(Node^.Left, Key, Deleted)
    else if Key > Node^.KeyID then
      Node^.Right := _Delete(Node^.Right, Key, Deleted)
    else begin
      Deleted := true;
      // 0 o 1 hijo
      if (Node^.Left=nil) or (Node^.Right=nil) then begin
        temp := Node^.Left; if temp=nil then temp := Node^.Right;
        if temp=nil then begin Dispose(Node); exit(nil); end   // sin hijos
        else begin Node^ := temp^; Dispose(temp); end;        // un hijo
      end
      else begin
        // 2 hijos: reemplazar por el sucesor
        temp := _MinNode(Node^.Right);
        Node^.KeyID := temp^.KeyID;
        Node^.Data  := temp^.Data;
        Node^.Right := _Delete(Node^.Right, temp^.KeyID, Deleted);
      end;
    end;

    // actualizar altura y reequilibrar
    Node^.Height := Max(Height(Node^.Left), Height(Node^.Right)) + 1;
    bal := GetBalance(Node);

    if (bal>1) and (GetBalance(Node^.Left)>=0) then exit(RotateRight(Node));        // LL
    if (bal>1) and (GetBalance(Node^.Left)<0)  then begin                            // LR
      Node^.Left := RotateLeft(Node^.Left); exit(RotateRight(Node));
    end;
    if (bal<-1) and (GetBalance(Node^.Right)<=0) then exit(RotateLeft(Node));       // RR
    if (bal<-1) and (GetBalance(Node^.Right)>0)  then begin                          // RL
      Node^.Right := RotateRight(Node^.Right); exit(RotateLeft(Node));
    end;

    Result := Node;
  end;
var ok: boolean;
begin
  Root := _Delete(Root, ID, ok);
  Result := ok;
end;

end.

