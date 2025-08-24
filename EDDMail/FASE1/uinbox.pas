unit uInbox;

{$mode objfpc}{$H+}

interface

type
  PMail = ^TMail;
  TMail = record
    // enlaces
    Prev, Next : PMail;
    // datos del correo
    Id        : Integer;
    Remitente : AnsiString;
    Estado    : ShortString;   // 'NL' / 'L'
    Programado: Boolean;
    Asunto    : AnsiString;
    Fecha     : AnsiString;
    Mensaje   : AnsiString;
  end;

  TInbox = record
    Head, Tail : PMail;
    Count      : Integer;
  end;

var
  NextMailId: Integer = 1;

procedure InitInbox(var B: TInbox);
function AddMail(var B: TInbox; const ARem, AAsunto, AFecha, AMensaje: AnsiString;
                 const AProg: Boolean): PMail;
function CountUnread(const B: TInbox): Integer;
function GetMailByIndex(const B: TInbox; Index: Integer): PMail;
procedure MarkRead(M: PMail);
procedure DetachMail(var B: TInbox; M: PMail);
procedure SortBySubject(var B: TInbox);
// NUEVO
function ExtractMailAt(var I: TInbox; Index: Integer): PMail;

implementation

uses SysUtils;

procedure InitInbox(var B: TInbox);
begin
  B.Head := nil;
  B.Tail := nil;
  B.Count := 0;
end;

function AddMail(var B: TInbox; const ARem, AAsunto, AFecha, AMensaje: AnsiString;
                 const AProg: Boolean): PMail;
var
  N: PMail;
begin
  New(N);
  N^.Id         := NextMailId; Inc(NextMailId);
  N^.Remitente  := ARem;
  N^.Estado     := 'NL';
  N^.Programado := AProg;
  N^.Asunto     := AAsunto;
  N^.Fecha      := AFecha;
  N^.Mensaje    := AMensaje;

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
var C: PMail;
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
var C: PMail; i: Integer;
begin
  if (Index < 0) or (Index >= B.Count) then Exit(nil);
  C := B.Head; i := 0;
  while (C <> nil) and (i < Index) do
  begin
    C := C^.Next; Inc(i);
  end;
  Result := C;
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
var SortedHead, SortedTail, Curr, NextN, P, InsBefore: PMail;
begin
  SortedHead := nil; SortedTail := nil;
  Curr := B.Head;
  while Curr <> nil do
  begin
    NextN := Curr^.Next;
    Curr^.Prev := nil; Curr^.Next := nil;

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
        Curr^.Prev := SortedTail;
        SortedTail^.Next := Curr;
        SortedTail := Curr;
      end
      else if InsBefore^.Prev = nil then
      begin
        Curr^.Next := InsBefore;
        InsBefore^.Prev := Curr;
        SortedHead := Curr;
      end
      else
      begin
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

function ExtractMailAt(var I: TInbox; Index: Integer): PMail;
var M: PMail;
begin
  Result := nil;
  M := GetMailByIndex(I, Index);
  if M = nil then Exit;
  DetachMail(I, M);
  Result := M; // queda “suelto”, lo puedes mandar a Trash
end;

end.

