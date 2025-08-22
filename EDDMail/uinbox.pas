unit uInbox;

{$mode objfpc}{$H+}

interface

type
  PMail = ^TMailNode;
  TMailNode = record
    Id:        Integer;       // id interno del correo (autoincremental)
    Remitente: AnsiString;
    Estado:    ShortString;   // 'NL' (No leído) o 'L' (Leído)
    Programado:Boolean;       // True si viene de "Programar Correo"
    Asunto:    AnsiString;
    Fecha:     AnsiString;    // formateado como texto (yyyy-mm-dd hh:nn)
    Mensaje:   AnsiString;
    Prev:      PMail;
    Next:      PMail;
  end;

  // Un “par” (cabeza/cola) para manejar la lista de la bandeja del usuario activo
  TInbox = record
    Head: PMail;
    Tail: PMail;
  end;

var
  Inbox: TInbox;  // bandeja del usuario actualmente logueado
  NextMailId: Integer = 1;

procedure InitInbox(var B: TInbox);
function  AddMail(var B: TInbox; const ARem, AAsunto, AFecha, AMensaje: AnsiString;
                  const AProg: Boolean): PMail;
function  CountUnread(const B: TInbox): Integer;
function  GetMailByIndex(const B: TInbox; Index: Integer): PMail;
procedure MarkRead(M: PMail);
procedure DetachMail(var B: TInbox; M: PMail);     // saca M de la lista (no libera)
procedure SortBySubject(var B: TInbox);            // A–Z (ascendente, case-insensitive)

implementation

uses
  SysUtils;

procedure InitInbox(var B: TInbox);
begin
  B.Head := nil;
  B.Tail := nil;
  NextMailId := 1;
end;

function AddMail(var B: TInbox; const ARem, AAsunto, AFecha, AMensaje: AnsiString;
                 const AProg: Boolean): PMail;
var
  N: PMail;
begin
  New(N);
  N^.Id        := NextMailId; Inc(NextMailId);
  N^.Remitente := ARem;
  N^.Estado    := 'NL';
  N^.Programado:= AProg;
  N^.Asunto    := AAsunto;
  N^.Fecha     := AFecha;
  N^.Mensaje   := AMensaje;
  N^.Prev := B.Tail;
  N^.Next := nil;

  if B.Head = nil then
    B.Head := N
  else
    B.Tail^.Next := N;

  B.Tail := N;
  Result := N;
end;

function CountUnread(const B: TInbox): Integer;
var
  C: PMail;
begin
  Result := 0;
  C := B.Head;
  while C <> nil do
  begin
    if C^.Estado = 'NL' then Inc(Result);
    C := C^.Next;
  end;
end;

function GetMailByIndex(const B: TInbox; Index: Integer): PMail;
var
  C: PMail;
  i: Integer;
begin
  C := B.Head; i := 0;
  while (C <> nil) and (i < Index) do
  begin
    C := C^.Next; Inc(i);
  end;
  Result := C; // puede ser nil si Index es grande
end;

procedure MarkRead(M: PMail);
begin
  if (M <> nil) and (M^.Estado = 'NL') then
    M^.Estado := 'L';
end;

procedure DetachMail(var B: TInbox; M: PMail);
begin
  if M = nil then Exit;

  if M^.Prev <> nil then
    M^.Prev^.Next := M^.Next
  else
    B.Head := M^.Next;

  if M^.Next <> nil then
    M^.Next^.Prev := M^.Prev
  else
    B.Tail := M^.Prev;

  M^.Prev := nil;
  M^.Next := nil;
end;

procedure SortBySubject(var B: TInbox);
var
  SortedHead, SortedTail, Curr, NextN, P, InsBefore: PMail;
begin
  SortedHead := nil;
  SortedTail := nil;
  Curr := B.Head;

  while Curr <> nil do
  begin
    NextN := Curr^.Next;
    Curr^.Prev := nil;
    Curr^.Next := nil;

    // insertar Curr en lista Sorted por Asunto (A-Z, case-insensitive)
    if SortedHead = nil then
    begin
      SortedHead := Curr; SortedTail := Curr;
    end
    else
    begin
      P := SortedHead; InsBefore := nil;
      while P <> nil do
      begin
        if AnsiCompareText(Curr^.Asunto, P^.Asunto) <= 0 then
        begin
          InsBefore := P; Break;
        end;
        P := P^.Next;
      end;

      if InsBefore = nil then
      begin
        // va al final
        Curr^.Prev := SortedTail;
        SortedTail^.Next := Curr;
        SortedTail := Curr;
      end
      else if InsBefore^.Prev = nil then
      begin
        // va al inicio
        Curr^.Next := InsBefore;
        InsBefore^.Prev := Curr;
        SortedHead := Curr;
      end
      else
      begin
        // en medio
        Curr^.Prev := InsBefore^.Prev;
        Curr^.Next := InsBefore;
        InsBefore^.Prev^.Next := Curr;
        InsBefore^.Prev := Curr;
      end;
    end;

    Curr := NextN;
  end;

  B.Head := SortedHead;
  B.Tail := SortedTail;
end;

end.

