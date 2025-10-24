unit uContacts;

{$mode objfpc}{$H+}

interface

type
  PContact = ^TContact;  // [APUNTADOR] Puntero a un nodo de contacto
  TContact = record
    Name    : AnsiString;
    Username: AnsiString;
    Email   : AnsiString;
    Phone   : AnsiString;
    Next    : PContact;  // [APUNTADOR] Lista **circular** simple: Next del último apunta a la cabeza
  end;

  // Lista circular con puntero a cola (Tail^.Next es la cabeza)
  TContactList = record
    Tail : PContact;   // [APUNTADOR] nil si está vacía; Tail^.Next = cabeza
    Curr : PContact;   // [APUNTADOR] cursor para navegar en UI (siguiente/anterior)
    Count: Integer;    // cantidad de contactos
  end;

procedure InitContacts(var L: TContactList);
procedure ClearContacts(var L: TContactList);

function ContactExists(const L: TContactList; const Username: AnsiString): Boolean;
function ContactRemove(var L: TContactList; const Username: AnsiString): Boolean;


// Inserta al final si no existe (por email/username). Devuelve True si se agregó.
function AddContact(var L: TContactList; const AName, AUser, AEmail, APhone: AnsiString): Boolean;

// Existe un contacto por email o username?
function ExistsInContacts(const L: TContactList; const Key: AnsiString): Boolean;

// Iteración para la UI (siguiente / anterior en la lista circular)
function HeadContact(const L: TContactList): PContact;
function NextContact(var L: TContactList): PContact;  //  avanzo y lo retorno
function PrevContact(var L: TContactList): PContact;  //  retrocedo y lo retorno

implementation

uses SysUtils;

procedure InitContacts(var L: TContactList);
begin

  L.Tail  := nil;  // [APUNTADOR] sin cola => no hay cabeza (Tail^.Next) tampoco
  L.Curr  := nil;  // [APUNTADOR] cursor nulo
  L.Count := 0;
end;

procedure ClearContacts(var L: TContactList);
var
  H, N: PContact; // [APUNTADORES] H = cabeza, N = siguiente temporal
begin
  //  Si está vacía, no hago nada
  if L.Tail = nil then Exit;


  H := L.Tail^.Next;   // [APUNTADOR] cabeza actual
  L.Tail^.Next := nil; // rompo el ciclo; ahora termino en nil
  while H <> nil do
  begin
    N := H^.Next; // guardo siguiente antes de liberar
    Dispose(H);   // libero nodo
    H := N;       // avanzo
  end;


  InitContacts(L);
end;

function ExistsInContacts(const L: TContactList; const Key: AnsiString): Boolean;
var
  H, C: PContact; // [APUNTADORES] H = cabeza para corte de ciclo; C = cursor
begin
  //  Recorro circularmente comparando Email o Username (case-insensitive)
  Result := False;
  if L.Tail = nil then Exit;
  H := L.Tail^.Next;  // [APUNTADOR] cabeza
  C := H;
  repeat
    if (AnsiCompareText(C^.Email, Key) = 0) or
       (AnsiCompareText(C^.Username, Key) = 0) then Exit(True);
    C := C^.Next; // [APUNTADOR] avanzo circular
  until C = H;    // doy la vuelta completa
end;

function AddContact(var L: TContactList; const AName, AUser, AEmail, APhone: AnsiString): Boolean;
var
  N: PContact; // [APUNTADOR] nuevo nodo
begin
  // Evito duplicados por email o username
  if ExistsInContacts(L, AEmail) or ExistsInContacts(L, AUser) then
    Exit(False);

  //  Creo el nodo y seteo sus campos
  New(N);
  N^.Name     := AName;
  N^.Username := AUser;
  N^.Email    := AEmail;
  N^.Phone    := APhone;

  if L.Tail = nil then
  begin
    //  Primer nodo se apunta a sí mismo (circularidad mínima)
    N^.Next := N;
    L.Tail  := N; // ahora hay un único nodo y es la cola a la vez
  end
  else
  begin
    // INSERCIÓN FINAL Inserto después de Tail y muevo Tail al nuevo
    N^.Next      := L.Tail^.Next; // [APUNTADOR] el siguiente del nuevo es la cabeza actual
    L.Tail^.Next := N;            // Tail anterior ahora apunta al nuevo
    L.Tail       := N;            // el nuevo se convierte en Tail
  end;

  Inc(L.Count);
  //  Si el cursor estaba nulo, lo fijo a la cabeza (Tail^.Next) para navegar desde ahí
  if L.Curr = nil then L.Curr := L.Tail^.Next;
  Result := True;
end;

function HeadContact(const L: TContactList): PContact;
begin
  //  Devuelvo la cabeza (Tail^.Next), o nil si no hay nodos
  if L.Tail = nil then Exit(nil);
  Result := L.Tail^.Next; // [APUNTADOR]
end;

function NextContact(var L: TContactList): PContact;
begin
  //  Avanzo el cursor circularmente y lo retorno
  if L.Tail = nil then Exit(nil);
  if L.Curr = nil then
    L.Curr := L.Tail^.Next  // si estaba nulo, inicio en cabeza
  else
    L.Curr := L.Curr^.Next; // [APUNTADOR] paso al siguiente
  Result := L.Curr;
end;

function PrevContact(var L: TContactList): PContact;
var
  P: PContact; // [APUNTADOR] busco el "anterior" al cursor (en circular simple no tengo Prev directo)
begin
  //  Para retroceder en una lista circular simple, camino hasta encontrar el que apunta a Curr
  if L.Tail = nil then Exit(nil);
  if (L.Curr = nil) then
    L.Curr := L.Tail^.Next   // si no tengo cursor, parto de la cabeza
  else
  begin
    // Recorro desde cabeza hasta que P^.Next sea Curr
    P := L.Tail^.Next;       // cabeza
    while (P^.Next <> L.Curr) do P := P^.Next;
    L.Curr := P;             // ahora Curr es el anterior
  end;
  Result := L.Curr;
end;

function ExistsContact(const L: TContactList; const Email: string): Boolean;
var
  First, C: PContact;
begin
  Result := False;
  if L.Tail = nil then Exit;

  First := L.Tail^.Next; // cabeza real
  C := First;
  repeat
    if AnsiCompareText(C^.Email, Email) = 0 then Exit(True);
    C := C^.Next;
  until C = First;
end;

function RemoveContact(var L: TContactList; const Email: string): Boolean;
var
  First, Prev, Cur: PContact;
begin
  Result := False;
  if L.Tail = nil then Exit;        // lista vacía

  First := L.Tail^.Next;
  Prev := L.Tail;                   // previo al primero (en circular)
  Cur  := First;

  // Recorremos UNA vuelta máximo
  repeat
    if AnsiCompareText(Cur^.Email, Email) = 0 then
    begin
      // único nodo
      if (Cur = Prev) and (Cur = L.Tail) then
      begin
        Dispose(Cur);
        L.Tail := nil;
      end
      else
      begin
        // puenteo
        Prev^.Next := Cur^.Next;

        // si quité la cabeza, la nueva cabeza es Prev^.Next (no hace falta guardarla)
        // si quité el tail, muevo tail a Prev
        if Cur = L.Tail then
          L.Tail := Prev;

        Dispose(Cur);
      end;

      Exit(True);
    end;

    Prev := Cur;
    Cur  := Cur^.Next;
  until Cur = First;
end;

function ContactExists(const L: TContactList; const Username: AnsiString): Boolean;
var
  First, C: PContact;
begin
  Result := False;
  if L.Tail = nil then Exit;

  First := L.Tail^.Next;
  C := First;
  repeat
    if AnsiCompareText(C^.Username, Username) = 0 then Exit(True);
    C := C^.Next;
  until C = First;
end;

function ContactRemove(var L: TContactList; const Username: AnsiString): Boolean;
var
  First, Prev, Cur: PContact;
begin
  Result := False;
  if L.Tail = nil then Exit;

  First := L.Tail^.Next;
  Prev  := L.Tail;
  Cur   := First;

  repeat
    if AnsiCompareText(Cur^.Email, Email) = 0 then
    begin
      if (Cur = Prev) and (Cur = L.Tail) then
      begin
        Dispose(Cur);
        L.Tail := nil;
      end
      else
      begin
        Prev^.Next := Cur^.Next;
        if Cur = L.Tail then
          L.Tail := Prev;
        Dispose(Cur);
      end;
      Exit(True);
    end;
    Prev := Cur;
    Cur  := Cur^.Next;
  until Cur = First;
end;



end.

