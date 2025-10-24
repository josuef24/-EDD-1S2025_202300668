unit uQueue;

{$mode objfpc}{$H+}

interface

type
  PSchedItem = ^TSchedItem;   // [APUNTADOR] Puntero a nodo de la cola (cada item programado)
  TSchedItem = record
    Dest    : AnsiString;     // correo o username del destinatario
    Asunto  : AnsiString;
    Fecha   : AnsiString;     // para mostrar en reporte
    Mensaje : AnsiString;
    Next    : PSchedItem;     // [APUNTADOR] enlace al siguiente nodo (cola simple)
  end;

  TSchedQueue = record
    Head, Tail : PSchedItem;  // [APUNTADOR] cabeza y cola de la cola FIFO (nil si vacía)
    Count      : Integer;     // cantidad de elementos (lo llevo por conveniencia)
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
  // Dejo la cola vacía: ambos punteros a nil y contador en 0
  Q.Head := nil; Q.Tail := nil; Q.Count := 0;
end;

procedure ClearQueue(var Q: TSchedQueue);
var
  N, T: PSchedItem; // [APUNTADOR] N recorre; T guarda el siguiente antes de liberar
begin
  //  Recorro desapilando memoria nodo por nodo
  N := Q.Head;
  while N <> nil do begin
    T := N^.Next; Dispose(N); N := T; // [APUNTADOR] avanzo usando el siguiente
  end;
  InitQueue(Q); //  restauro a estado vacío
end;

procedure EnqueueScheduled(var Q: TSchedQueue;
  const ADest, AAsunto, AFecha, AMsg: AnsiString);
var
  N: PSchedItem; // [APUNTADOR] nuevo nodo a encolar
begin
  //  Reservo nodo y seteo sus campos
  New(N);
  N^.Dest    := ADest;
  N^.Asunto  := AAsunto;
  N^.Fecha   := AFecha;     // decorativo para mostrar
  N^.Mensaje := AMsg;
  N^.Next    := nil;        // [APUNTADOR] como va al final, su siguiente es nil

  // FIFO Si la cola estaba vacía, el nuevo es Head; si no, lo engancho al final
  if Q.Head = nil then Q.Head := N
  else Q.Tail^.Next := N;   // [APUNTADOR] apunto el último actual al nuevo

  Q.Tail := N;              // [APUNTADOR] el nuevo pasa a ser la cola
  Inc(Q.Count);             //  actualizo tamaño
end;

function ProcessFIFO(var Q: TSchedQueue): Integer;
var
  It: PSchedItem; // [APUNTADOR] cursor al elemento que proceso (pop de la cola)
  Dest: PUser;    // [APUNTADOR] puntero al usuario destino (buscado por email/usuario)
begin
  Result := 0;
  //  Saco de la cabeza hasta vaciar
  while Q.Head <> nil do
  begin
    //  Quito la cabeza y adelanto
    It := Q.Head;                  // [APUNTADOR] guardo el item actual
    Q.Head := It^.Next;            // [APUNTADOR] adelanto la cabeza
    if Q.Head = nil then Q.Tail := nil; // si quedó vacío, muevo Tail también
    Dec(Q.Count);

    // Busco al usuario destinatario por email/username
    Dest := FindUserByEmailOrUsername(It^.Dest);
    if (Dest <> nil) and (CurrentUser <> nil) then
    begin
      //  Inserto en la bandeja de Dest; lo marco Programado=True
      AddMail(Dest^.Inbox, CurrentUser^.Email, It^.Asunto, It^.Fecha, It^.Mensaje, True);
      // MATRIZ DISPERSA Actualizo la relación emisor->receptor
      IncrementEdge(RelMatrix, CurrentUser, Dest);
      Inc(Result); // contado como enviado
    end;

    //  Libero el nodo procesado
    Dispose(It);
  end;
end;


function ExportSchedDOTForUser(const Q: TSchedQueue;
  const UserEmail, BaseDir: string; out DotPath: string): Boolean;
var
  F: TextFile;     //  archivo .dot
  N: PSchedItem;   // [APUNTADOR] cursor para recorrer sin mutar la cola
  Path: string;    //  ruta destino
  idx: Integer;    //  índice para etiquetar nodos consecutivos
begin
  Result := False;
  DotPath := '';

  //  Directorio de salida
  if (BaseDir = '') then Exit;
  if not DirectoryExists(BaseDir) then
    if not ForceDirectories(BaseDir) then Exit;

  Path := IncludeTrailingPathDelimiter(BaseDir) + 'programados_' + UserEmail + '.dot';
  AssignFile(F, Path);
  try
    Rewrite(F);
    //  Encabezado y estilo
    Writeln(F, 'digraph "Reporte de Correos Programados" {');
    Writeln(F, '  rankdir=TB;');
    Writeln(F, '  labelloc="t";');
    Writeln(F, '  label="Reporte de Correos Programados";');
    Writeln(F, '  node [shape=box, style="rounded,filled", fillcolor="#d7f5f9"];');

    Writeln(F, '  subgraph cluster_q {');
    Writeln(F, '    label="Cola"; color="#bbbbbb";');

    // [RECORRIDO] Camino desde Head hasta nil, dibujando nodos y flechas en orden FIFO
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
        Writeln(F, '    n', idx, ' -> n', idx+1, ' [arrowsize=0.7];'); // [APUNTADOR] enlazo con el siguiente
      N := N^.Next; // [APUNTADOR] avanzo
      Inc(idx);
    end;

    Writeln(F, '  }');
    Writeln(F, '}');
    CloseFile(F);

    DotPath := Path; //  devuelvo la ruta
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
  It: PSchedItem;// [APUNTADOR] cursor de lectura
  Path: string;
  idn: Integer;  // [DATA] contador para nombres de nodo
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
    //  Variante simple LR
    Writeln(F, 'digraph "ColaProgramados" {');
    Writeln(F, '  rankdir=LR;');
    Writeln(F, '  node [shape=record, style=filled, fillcolor="#d0f0ff"];');
    Writeln(F, '  label="Correos Programados (Cola FIFO)"; labelloc="t";');

    // [RECORRIDO] Camino de Head a nil y dibujo cadena hacia la derecha
    It := Q.Head;
    idn := 0;
    while It <> nil do
    begin
      Writeln(F, Format('  n%d [label="Dest: %s | Asunto: %s | Fecha: %s"];',
        [idn, It^.Dest, It^.Asunto, It^.Fecha]));
      if (It^.Next <> nil) then
        Writeln(F, Format('  n%d -> n%d;', [idn, idn+1])); // [APUNTADOR] flecha al siguiente
      Inc(idn);
      It := It^.Next; // [APUNTADOR] avanzo
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


