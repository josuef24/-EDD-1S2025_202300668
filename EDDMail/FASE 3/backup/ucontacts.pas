unit uContacts;

{$mode objfpc}{$H+}

interface

type
  PContact = ^TContact;
  TContact = record
    Name    : AnsiString;
    Username: AnsiString;
    Email   : AnsiString;
    Phone   : AnsiString;
    Next    : PContact;  // lista **circular** simple
  end;

  // Lista circular con puntero a cola (Tail^.Next es la cabeza)
  TContactList = record
    Tail : PContact;   // nil si está vacía
    Curr : PContact;   // para navegar (UI)
    Count: Integer;
  end;

procedure InitContacts(var L: TContactList);
procedure ClearContacts(var L: TContactList);

// Inserta al final si no existe (por email/username). Devuelve True si se agregó.
function AddContact(var L: TContactList; const AName, AUser, AEmail, APhone: AnsiString): Boolean;

// ¿Existe un contacto por email o username?
function ExistsInContacts(const L: TContactList; const Key: AnsiString): Boolean;

// Iteración para la UI (siguiente / anterior en la lista circular)
function HeadContact(const L: TContactList): PContact;
function NextContact(var L: TContactList): PContact;  // avanza Curr y lo retorna
function PrevContact(var L: TContactList): PContact;  // retrocede Curr y lo retorna

implementation

uses SysUtils;

procedure InitContacts(var L: TContactList);
begin
  L.Tail  := nil;
  L.Curr  := nil;
  L.Count := 0;
end;

procedure ClearContacts(var L: TContactList);
var
  H, N: PContact;
begin
  if L.Tail = nil then Exit;
  H := L.Tail^.Next;           // cabeza
  L.Tail^.Next := nil;         // rompe el ciclo para recorrer lineal
  while H <> nil do
  begin
    N := H^.Next;
    Dispose(H);
    H := N;
  end;
  InitContacts(L);
end;

function ExistsInContacts(const L: TContactList; const Key: AnsiString): Boolean;
var
  H, C: PContact;
begin
  Result := False;
  if L.Tail = nil then Exit;
  H := L.Tail^.Next;  // cabeza
  C := H;
  repeat
    if (AnsiCompareText(C^.Email, Key) = 0) or
       (AnsiCompareText(C^.Username, Key) = 0) then Exit(True);
    C := C^.Next;
  until C = H;
end;

function AddContact(var L: TContactList; const AName, AUser, AEmail, APhone: AnsiString): Boolean;
var
  N: PContact;
begin
  // evita duplicados por email/username
  if ExistsInContacts(L, AEmail) or ExistsInContacts(L, AUser) then
    Exit(False);

  New(N);
  N^.Name     := AName;
  N^.Username := AUser;
  N^.Email    := AEmail;
  N^.Phone    := APhone;

  if L.Tail = nil then
  begin
    N^.Next := N;       // primer nodo: se apunta a sí mismo
    L.Tail  := N;
  end
  else
  begin
    N^.Next     := L.Tail^.Next; // a la cabeza actual
    L.Tail^.Next:= N;            // viejo tail apunta al nuevo
    L.Tail      := N;            // nuevo tail
  end;

  Inc(L.Count);
  if L.Curr = nil then L.Curr := L.Tail^.Next; // apunta a la cabeza
  Result := True;
end;

function HeadContact(const L: TContactList): PContact;
begin
  if L.Tail = nil then Exit(nil);
  Result := L.Tail^.Next;
end;

function NextContact(var L: TContactList): PContact;
begin
  if L.Tail = nil then Exit(nil);
  if L.Curr = nil then L.Curr := L.Tail^.Next else L.Curr := L.Curr^.Next;
  Result := L.Curr;
end;

function PrevContact(var L: TContactList): PContact;
var
  P: PContact;
begin
  if L.Tail = nil then Exit(nil);
  if (L.Curr = nil) then
    L.Curr := L.Tail^.Next   // cabeza
  else
  begin
    // para retroceder en circular simple, buscamos el anterior
    P := L.Tail^.Next;
    while (P^.Next <> L.Curr) do P := P^.Next;
    L.Curr := P;
  end;
  Result := L.Curr;
end;

end.

