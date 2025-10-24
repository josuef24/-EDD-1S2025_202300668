unit uQueue;

{$mode objfpc}{$H+}

interface

type
  PSchedItem = ^TSchedItem;
  TSchedItem = record
    Dest    : AnsiString;   // email o username del destinatario
    Asunto  : AnsiString;
    Fecha   : AnsiString;   // solo para mostrar; ya no controla nada
    Mensaje : AnsiString;
    Next    : PSchedItem;
  end;

  TSchedQueue = record
    Head, Tail : PSchedItem;
    Count      : Integer;
  end;

procedure InitQueue(var Q: TSchedQueue);
procedure ClearQueue(var Q: TSchedQueue);

// Encola al final (FIFO)
procedure EnqueueScheduled(var Q: TSchedQueue; const ADest, AAsunto, AFecha, AMsg: AnsiString);

// PROCESA TODO en orden FIFO. Devuelve cuántos envió.
function ProcessFIFO(var Q: TSchedQueue): Integer;

// Reporte DOT de la cola del usuario (no muta la cola)
function ExportSchedDOTForUser(const Q: TSchedQueue;
                               const UserEmail, BaseDir: string;
                               out DotPath: string): Boolean;

implementation

uses
  SysUtils, uUsers, uInbox, uMatrix;

procedure InitQueue(var Q: TSchedQueue);
begin
  Q.Head := nil; Q.Tail := nil; Q.Count := 0;
end;

procedure ClearQueue(var Q: TSchedQueue);
var
  N, T: PSchedItem;
begin
  N := Q.Head;
  while N <> nil do begin
    T := N^.Next; Dispose(N); N := T;
  end;
  InitQueue(Q);
end;

procedure EnqueueScheduled(var Q: TSchedQueue;
  const ADest, AAsunto, AFecha, AMsg: AnsiString);
var
  N: PSchedItem;
begin
  New(N);
  N^.Dest    := ADest;
  N^.Asunto  := AAsunto;
  N^.Fecha   := AFecha;     // solo decorativo
  N^.Mensaje := AMsg;
  N^.Next    := nil;

  if Q.Head = nil then Q.Head := N
  else Q.Tail^.Next := N;

  Q.Tail := N;
  Inc(Q.Count);
end;

function ProcessFIFO(var Q: TSchedQueue): Integer;
var
  It: PSchedItem;
  Dest: PUser;
begin
  Result := 0;
  while Q.Head <> nil do
  begin
    It := Q.Head;
    Q.Head := It^.Next;
    if Q.Head = nil then Q.Tail := nil;
    Dec(Q.Count);

    Dest := FindUserByEmailOrUsername(It^.Dest);
    if (Dest <> nil) and (CurrentUser <> nil) then
    begin
      // Se marca como Programado=True al insertarlo en la bandeja
      AddMail(Dest^.Inbox, CurrentUser^.Email, It^.Asunto, It^.Fecha, It^.Mensaje, True);
      IncrementEdge(RelMatrix, CurrentUser, Dest); // matriz dispersa
      Inc(Result);
    end;

    Dispose(It);
  end;
end;


function ExportSchedDOTForUser(const Q: TSchedQueue;
  const UserEmail, BaseDir: string; out DotPath: string): Boolean;
var
  F: TextFile;
  N: PSchedItem;
  Path: string;
  idx: Integer;
begin
  Result := False;
  DotPath := '';

  if (BaseDir = '') then Exit;
  if not DirectoryExists(BaseDir) then
    if not ForceDirectories(BaseDir) then Exit;

  Path := IncludeTrailingPathDelimiter(BaseDir) + 'programados_' + UserEmail + '.dot';
  AssignFile(F, Path);
  try
    Rewrite(F);
    Writeln(F, 'digraph "Reporte de Correos Programados" {');
    Writeln(F, '  rankdir=TB;');
    Writeln(F, '  labelloc="t";');
    Writeln(F, '  label="Reporte de Correos Programados";');
    Writeln(F, '  node [shape=box, style="rounded,filled", fillcolor="#d7f5f9"];');

    Writeln(F, '  subgraph cluster_q {');
    Writeln(F, '    label="Cola"; color="#bbbbbb";');

    N := Q.Head; idx := 1;
    while N <> nil do
    begin
      Writeln(F, '    n', idx, ' [label=<',
        '<b>ID:</b> ', idx, '<br/>',
        '<b>Remitente:</b> ', UserEmail, '<br/>',
        '<b>Estado:</b> Programado<br/>',
        '<b>Programado:</b> Sí<br/>',
        '<b>Asunto:</b> ', N^.Asunto, '<br/>',
        '<b>Fecha:</b> ', N^.Fecha, '<br/>',
        '<b>Mensaje:</b> ', StringReplace(N^.Mensaje, '<', '&lt;', [rfReplaceAll]),
        '> , width=3 ];');
      if N^.Next <> nil then
        Writeln(F, '    n', idx, ' -> n', idx+1, ' [arrowsize=0.7];');
      N := N^.Next;
      Inc(idx);
    end;

    Writeln(F, '  }');
    Writeln(F, '}');
    CloseFile(F);

    DotPath := Path;
    Result := True;
  except
    on E: Exception do
    begin
      {$I-} CloseFile(F); {$I+}
      Result := False;
    end;
  end;
end;

function ExportSchedDOT(const UserEmail: string; const Q: TSchedQueue;
                        const BaseDir: string; out DotPath: string): Boolean;
var
  F: TextFile;
  It: PSchedItem;
  Path: string;
  idn: Integer;
begin
  Result := False;
  DotPath := '';
  if BaseDir = '' then Exit;

  if not DirectoryExists(BaseDir) then
    if not ForceDirectories(BaseDir) then Exit;

  Path := IncludeTrailingPathDelimiter(BaseDir) + 'sched_' + UserEmail + '.dot';
  AssignFile(F, Path);
  try
    Rewrite(F);
    Writeln(F, 'digraph "ColaProgramados" {');
    Writeln(F, '  rankdir=LR;');
    Writeln(F, '  node [shape=record, style=filled, fillcolor="#d0f0ff"];');
    Writeln(F, '  label="Correos Programados (Cola FIFO)"; labelloc="t";');

    It := Q.Head;
    idn := 0;
    while It <> nil do
    begin
      Writeln(F, Format('  n%d [label="Dest: %s | Asunto: %s | Fecha: %s"];',
        [idn, It^.Dest, It^.Asunto, It^.Fecha]));
      if (It^.Next <> nil) then
        Writeln(F, Format('  n%d -> n%d;', [idn, idn+1]));
      Inc(idn);
      It := It^.Next;
    end;

    Writeln(F, '}');
    CloseFile(F);
    DotPath := Path;
    Result := True;
  except
    on E: Exception do
    begin
      {$I-} CloseFile(F); {$I+}
      Result := False;
    end;
  end;
end;

end.

