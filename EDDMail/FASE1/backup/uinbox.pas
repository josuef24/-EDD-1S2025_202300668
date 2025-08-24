unit uInbox;

{$mode objfpc}{$H+}

interface

type
  PMail = ^TMail;
  TMail = record
    // Enlaces (lista doble)
    Prev, Next : PMail;

    // Datos del correo
    Id        : Integer;      // autoincremental
    Remitente : AnsiString;
    Estado    : ShortString;  // 'NL' (No leído) / 'L' (Leído)
    Programado: Boolean;
    Asunto    : AnsiString;
    Fecha     : AnsiString;   // yyyy-mm-dd hh:nn
    Mensaje   : AnsiString;
  end;

  // Bandeja del usuario (punteros a cabeza/cola y cantidad)
  TInbox = record
    Head, Tail : PMail;
    Count      : Integer;
  end;

var
  NextMailId: Integer = 1;

procedure InitInbox(var B: TInbox);

// Inserta al final y devuelve el puntero al correo creado
function AddMail(var B: TInbox; const ARem, AAsunto, AFecha, AMensaje: AnsiString;
                 const AProg: Boolean): PMail;

// Utilidades
function  CountUnread(const B: TInbox): Integer;
function  GetMailByIndex(const B: TInbox; Index: Integer): PMail;
procedure MarkRead(M: PMail);
procedure DetachMail(var B: TInbox; M: PMail);            // saca M de la lista (no libera)
procedure SortBySubject(var B: TInbox);                    // ordena A–Z por Asunto
function  ExtractMailAt(var I: TInbox; Index: Integer): PMail; // elimina en Index y devuelve M (sin liberar)

implementation

uses
  SysUtils;

procedure InitInbox(var B: TInbox);
begin
  B.Head  := nil;
  B.Tail  := nil;
  B.Count := 0;
  // No toco NextMailId aquí para que sea global al proceso
end;

function AddMail(var B: TInbox; const ARem, AAsunto, AFecha, AMensaje: AnsiString;
                 const AProg: Boolean): PMail;
var
  N: PMail;
begin
  New(N);
  // datos
  N^.Id         := NextMailId;  Inc(NextMailId);
  N^.Remitente  := ARem;
  N^.Estado     := 'NL';
  N^.Programado := AProg;
  N^.Asunto     := AAsunto;
  N^.Fecha      := AFecha;
  N^.Mensaje    := AMensaje;

  // enlaces
  N^.Prev := B.Tail;
  N^.Next := nil;

  if B.Head = nil then
    B.Head := N
  else
    B.Tail^.Next := N;

  B.Tail := N;
  Inc(B.Count);
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
  if (Index < 0) or (Index >= B.Count) then Exit(nil);
  // recorrido sencillo (si quieres, puedes optimizar desde Tail cuando Index > Count/2)
  C := B.Head; i := 0;
  while (C <> nil) and (i < Index) do
  begin
    C := C^.Next; Inc(i);
  end;
  Result := C; // será <> nil por el chequeo de rango
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
  Dec(B.Count);
end;

procedure SortBySubject(var B: TInbox);
var
  SortedHead, SortedTail, Curr, NextN, P, InsBefore: PMail;
begin
  SortedHead := nil;
  SortedTail := nil;
  Curr := B.Head;

  // “insertion sort” sobre la lista, por Asunto (case-insensitive)
  while Curr <> nil do
  begin
    NextN := Curr^.Next;
    Curr^.Prev := nil;
    Curr^.Next := nil;

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
        // al final
        Curr^.Prev := SortedTail;
        SortedTail^.Next := Curr;
        SortedTail := Curr;
      end
      else if InsBefore^.Prev = nil then
      begin
        // al inicio
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
  // B.Count no cambia
end;

function ExtractMailAt(var I: TInbox; Index: Integer): PMail;
var
  M: PMail;
begin
  Result := nil;
  M := GetMailByIndex(I, Index);
  if M = nil then Exit;

  // sacar de la lista pero NO liberar, lo devuelve
  DetachMail(I, M);
  Result := M;
end;

end.

