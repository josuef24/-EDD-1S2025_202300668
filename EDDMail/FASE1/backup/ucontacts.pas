unit uContacts;

{$mode objfpc}{$H+}

interface

type
  PContact = ^TContactNode;

  TContactNode = record
    // Datos del contacto (ajústalos según tu enunciado)
    Name:     AnsiString;
    Username: AnsiString;
    Email:    AnsiString;
    Phone:    AnsiString;

    Next: PContact;  // lista CIRCULAR (último^.Next -> Head)
  end;

  // Lista circular simple + un cursor para navegar (Anterior/Siguiente)
  TContactList = record
    Head:    PContact;  // nil si está vacía
    Current: PContact;  // puntero de navegación (opcional)
    Count:   Integer;
  end;

procedure InitContacts(var L: TContactList);
function  AddContact(var L: TContactList; const AName, AUser, AEmail, APhone: AnsiString): PContact;
function  ExistsContact(const L: TContactList; const Key: AnsiString): Boolean;
function  FindContact(const L: TContactList; const Key: AnsiString): PContact;

procedure SetCurrentFirst(var L: TContactList);
function  NextContact(var L: TContactList): PContact;
function  PrevContact(var L: TContactList): PContact;

implementation

uses
  SysUtils;

procedure InitContacts(var L: TContactList);
begin
  L.Head    := nil;
  L.Current := nil;
  L.Count   := 0;
end;

// Inserta al final manteniendo circularidad
function AddContact(var L: TContactList; const AName, AUser, AEmail, APhone: AnsiString): PContact;
var
  N, Tail: PContact;
begin
  New(N);
  N^.Name     := AName;
  N^.Username := AUser;
  N^.Email    := AEmail;
  N^.Phone    := APhone;
  N^.Next     := nil;

  if L.Head = nil then
  begin
    // primer nodo -> se apunta a sí mismo
    L.Head    := N;
    N^.Next   := N;
    L.Current := N;
  end
  else
  begin
    // buscar el "tail" (el que apunta a Head)
    Tail := L.Head;
    while Tail^.Next <> L.Head do
      Tail := Tail^.Next;

    // insertar al final: tail -> N -> head
    Tail^.Next := N;
    N^.Next    := L.Head;
  end;

  Inc(L.Count);
  Result := N;
end;

// Key puede ser email o username (case-insensitive)
function ExistsContact(const L: TContactList; const Key: AnsiString): Boolean;
begin
  Result := FindContact(L, Key) <> nil;
end;

function FindContact(const L: TContactList; const Key: AnsiString): PContact;
var
  C: PContact;
  k: AnsiString;
begin
  Result := nil;
  if L.Head = nil then Exit;

  k := LowerCase(Trim(Key));
  C := L.Head;
  repeat
    if (LowerCase(C^.Email) = k) or (LowerCase(C^.Username) = k) then
      Exit(C);
    C := C^.Next;
  until C = L.Head;
end;

procedure SetCurrentFirst(var L: TContactList);
begin
  L.Current := L.Head; // puede quedar en nil si está vacío
end;

// Avanza Current y lo devuelve (para navegar)
function NextContact(var L: TContactList): PContact;
begin
  if L.Current = nil then
    L.Current := L.Head
  else
    L.Current := L.Current^.Next;

  Result := L.Current;
end;

// Retrocede Current O(n) (porque la lista es simple)
// Suficiente para la UI de "Anterior/Siguiente"
function PrevContact(var L: TContactList): PContact;
var
  P: PContact;
begin
  if (L.Head = nil) then
  begin
    L.Current := nil; Exit(nil);
  end;

  if (L.Current = nil) then
    L.Current := L.Head
  else
  begin
    // buscar el nodo previo a Current
    P := L.Head;
    while (P^.Next <> L.Current) do
    begin
      P := P^.Next;
      if P = L.Head then Break; // seguridad
    end;
    L.Current := P;
  end;

  Result := L.Current;
end;

end.

