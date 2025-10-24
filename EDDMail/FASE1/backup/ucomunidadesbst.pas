unit UComunidadesBST;

{$mode ObjFPC}{$H+}

interface

uses SysUtils;

type
  // --- Lista de miembros ---
  PMember = ^TMember;
  TMember = record
    Id     : AnsiString;   // email del usuario
    Nombre : AnsiString;   // nombre visible
    Next   : PMember;      // lista simple
  end;

  // --- Lista de mensajes ---
  PMsg = ^TMsg;
  TMsg = record
    Autor : AnsiString;    // email del autor
    Texto : AnsiString;    // contenido
    Fecha : AnsiString;    // yyyy-mm-dd hh:nn
    Next  : PMsg;          // lista simple
  end;

  // --- Nodo BST de comunidad ---
  PComNode = ^TComNode;
  TComNode = record
    Nombre  : AnsiString;  // CLAVE del BST
    Left    : PComNode;
    Right   : PComNode;
    Members : PMember;     // cabeza de miembros
    MsgHead : PMsg;        // cabeza de mensajes
  end;

procedure BST_Init(var Root: PComNode);
function  BST_Find(Root: PComNode; const Nombre: AnsiString): PComNode;
function  BST_Insert(var Root: PComNode; const Nombre: AnsiString): PComNode;

function  BST_ExistsMember(const C: PComNode; const AId: AnsiString): Boolean;
function  BST_AddMember(const C: PComNode; const AId, ANombre: AnsiString): Boolean;
function  BST_RemoveMember(const C: PComNode; const AId: AnsiString): Boolean;

function  BST_AddMessageToExisting(const C: PComNode; const Autor, Texto, AFecha: AnsiString): Boolean;
function  BST_AddMessage(var Root: PComNode; const Comunidad, Autor, Texto, AFecha: AnsiString): Boolean;

function  BST_AddMemberByCommName(var Root: PComNode; const Comunidad, AId, ANombre: AnsiString): Boolean;

implementation

procedure BST_Init(var Root: PComNode);
begin
  Root := nil;
end;

function BST_Find(Root: PComNode; const Nombre: AnsiString): PComNode;
begin
  while Root <> nil do
    if AnsiCompareText(Nombre, Root^.Nombre) = 0 then Exit(Root)
    else if AnsiCompareText(Nombre, Root^.Nombre) < 0 then Root := Root^.Left
    else Root := Root^.Right;
  Result := nil;
end;

function BST_Insert(var Root: PComNode; const Nombre: AnsiString): PComNode;
begin
  if Root = nil then
  begin
    New(Root);
    Root^.Nombre  := Nombre;
    Root^.Left    := nil;
    Root^.Right   := nil;
    Root^.Members := nil;
    Root^.MsgHead := nil;
    Exit(Root);
  end;

  if AnsiCompareText(Nombre, Root^.Nombre) = 0 then Exit(Root)
  else if AnsiCompareText(Nombre, Root^.Nombre) < 0 then
    Result := BST_Insert(Root^.Left, Nombre)
  else
    Result := BST_Insert(Root^.Right, Nombre);
end;

function BST_ExistsMember(const C: PComNode; const AId: AnsiString): Boolean;
var M: PMember;
begin
  if C = nil then Exit(False);
  M := C^.Members;
  while M <> nil do
  begin
    if SameText(M^.Id, AId) then Exit(True);
    M := M^.Next;
  end;
  Result := False;
end;

function BST_AddMember(const C: PComNode; const AId, ANombre: AnsiString): Boolean;
var N: PMember;
begin
  Result := False;
  if (C = nil) or (AId = '') then Exit;
  if BST_ExistsMember(C, AId) then Exit;

  New(N);
  N^.Id     := AId;
  N^.Nombre := ANombre;
  N^.Next   := C^.Members;   // push-front O(1)
  C^.Members := N;
  Result := True;
end;

function BST_RemoveMember(const C: PComNode; const AId: AnsiString): Boolean;
var Ant, Cur: PMember;
begin
  Result := False;
  if C = nil then Exit;
  Ant := nil; Cur := C^.Members;
  while Cur <> nil do
  begin
    if SameText(Cur^.Id, AId) then
    begin
      if Ant = nil then C^.Members := Cur^.Next
                   else Ant^.Next  := Cur^.Next;
      Dispose(Cur);
      Exit(True);
    end;
    Ant := Cur; Cur := Cur^.Next;
  end;
end;

function BST_AddMemberByCommName(var Root: PComNode; const Comunidad, AId, ANombre: AnsiString): Boolean;
var C: PComNode;
begin
  if Comunidad = '' then Exit(False);
  C := BST_Find(Root, Comunidad);
  if C = nil then C := BST_Insert(Root, Comunidad);
  Result := BST_AddMember(C, AId, ANombre);
end;

function BST_AddMessageToExisting(const C: PComNode; const Autor, Texto, AFecha: AnsiString): Boolean;
var
  M : PMsg;
  F : AnsiString;
begin
  Result := False;
  if (C = nil) or (Trim(Texto) = '') then Exit;

  if AFecha <> '' then F := AFecha
                  else F := FormatDateTime('yyyy-mm-dd hh:nn', Now);

  New(M);
  M^.Autor := Autor;
  M^.Texto := Texto;
  M^.Fecha := F;
  M^.Next  := C^.MsgHead;     // push-front O(1)
  C^.MsgHead := M;
  Result := True;
end;

function BST_AddMessage(var Root: PComNode; const Comunidad, Autor, Texto, AFecha: AnsiString): Boolean;
var
  C: PComNode;
begin
  C := BST_Find(Root, Comunidad);
  if C = nil then Exit(False);              // solo si existe
  Result := BST_AddMessageToExisting(C, Autor, Texto, AFecha);
end;

end.

