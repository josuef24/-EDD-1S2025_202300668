unit uQueue;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  // Nodo de la cola de correos programados (FIFO)
  PSchedNode = ^TSchedNode;
  TSchedNode = record
    Remitente : AnsiString;   // quien envía (p.ej., CurrentUser^.Email)
    DestKey   : AnsiString;   // email o usuario de destino
    Asunto    : AnsiString;
    Mensaje   : AnsiString;
    SendAt    : TDateTime;    // cuándo debe enviarse
    Next      : PSchedNode;
  end;

  // Cola simple (cabeza/cola)
  TSchedQueue = record
    Head, Tail : PSchedNode;
    Count      : Integer;
  end;

procedure InitQueue(var Q: TSchedQueue);
procedure EnqueueMail(var Q: TSchedQueue; const ARem, ADestKey, AAsunto, AMensaje: AnsiString; ASendAt: TDateTime);
function  DequeueMail(var Q: TSchedQueue): PSchedNode;   // recuerda Dispose() después de usarlo
function  PeekMail(const Q: TSchedQueue): PSchedNode;
function  IsQueueEmpty(const Q: TSchedQueue): Boolean;

// Procesa todos los correos cuyo SendAt <= Now, entregándolos a la bandeja
// Devuelve cuántos correos se enviaron.
function  ProcessDue(var Q: TSchedQueue): Integer;

implementation

uses
  uUsers, uInbox;  // para FindUserByEmailOrUsername y AddMail

procedure InitQueue(var Q: TSchedQueue);
begin
  Q.Head := nil;
  Q.Tail := nil;
  Q.Count := 0;
end;

procedure EnqueueMail(var Q: TSchedQueue; const ARem, ADestKey, AAsunto, AMensaje: AnsiString; ASendAt: TDateTime);
var
  N: PSchedNode;
begin
  New(N);
  N^.Remitente := ARem;
  N^.DestKey   := ADestKey;
  N^.Asunto    := AAsunto;
  N^.Mensaje   := AMensaje;
  N^.SendAt    := ASendAt;
  N^.Next      := nil;

  if Q.Tail <> nil then
    Q.Tail^.Next := N
  else
    Q.Head := N;

  Q.Tail := N;
  Inc(Q.Count);
end;

function DequeueMail(var Q: TSchedQueue): PSchedNode;
begin
  Result := Q.Head;
  if Result = nil then Exit;

  Q.Head := Result^.Next;
  if Q.Head = nil then
    Q.Tail := nil;

  Result^.Next := nil;
  Dec(Q.Count);
end;

function PeekMail(const Q: TSchedQueue): PSchedNode;
begin
  Result := Q.Head;
end;

function IsQueueEmpty(const Q: TSchedQueue): Boolean;
begin
  Result := Q.Head = nil;
end;

function ProcessDue(var Q: TSchedQueue): Integer;
var
  N: PSchedNode;
  Dest: PUser;
  sent: Integer;
  fechaTxt: AnsiString;
begin
  sent := 0;
  // Mientras el de la cabeza ya esté vencido, procesarlo
  while (Q.Head <> nil) and (Q.Head^.SendAt <= Now) do
  begin
    N := DequeueMail(Q);        // saca FIFO
    if N <> nil then
    begin
      // buscar destinatario por email o usuario
      Dest := FindUserByEmailOrUsername(N^.DestKey);
      if Dest <> nil then
      begin
        fechaTxt := FormatDateTime('yyyy-mm-dd hh:nn', Now);
        // entrega a bandeja del destinatario (Programado = False al entregarse)
        AddMail(Dest^.Inbox, N^.Remitente, N^.Asunto, fechaTxt, N^.Mensaje, False);
        Inc(sent);
      end;
      Dispose(N);
    end;
  end;
  Result := sent;
end;

end.

