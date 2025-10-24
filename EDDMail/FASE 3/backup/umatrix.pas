unit uMatrix;

{$mode objfpc}{$H+}

interface

uses
  uUsers;  // [APUNTADOR] Aquí está PUser (puntero a usuario)

const
  OUT_DIR_REL = 'Root-Reportes/';  //  Carpeta donde dejo el DOT de relaciones

type
  PRelNode = ^TRelNode; // [APUNTADOR] Puntero a celda de la matriz (nodo ortogonal)
  TRelNode = record
    RowUser : PUser;   // [APUNTADOR] Remitente (fila)
    ColUser : PUser;   // [APUNTADOR] Destinatario (columna)
    Count   : Integer; // Número de correos enviados Row->Col

    //  Enlaces ortogonales: me permiten recorrer por fila (Right/Left) y por columna (Down/Up)
    Right, Left : PRelNode; // [APUNTADOR] Vecinos en la fila
    Down,  Up   : PRelNode; // [APUNTADOR] Vecinos en la columna
  end;

  PHead = ^THead; // [APUNTADOR] Puntero a cabecera de fila o columna
  THead = record
    User : PUser;   // [APUNTADOR] Usuario que identifica la fila o la columna
    Next : PHead;   // [APUNTADOR] Siguiente cabecera (lista simple, ordenada por Id)
    Node : PRelNode; // [APUNTADOR] Primer nodo real de esa fila/columna
  end;

  TRelMatrix = record
    RowHeads : PHead;   // [APUNTADOR] Lista de cabeceras de FILAS (remitentes)
    ColHeads : PHead;   // [APUNTADOR] Lista de cabeceras de COLUMNAS (destinatarios)
    Total    : Integer; // Total de celdas existentes (pares con al menos 1 envío)
  end;

var
  RelMatrix: TRelMatrix;  // Matriz global de relaciones (estructura contenedora, no puntero)

procedure InitRelMatrix(var M: TRelMatrix);
procedure InitRel; // Inicializa la global y asegura carpeta de salida

// Sumo 1 al conteo de (Sender -> Receiver)
procedure IncrementEdge(var M: TRelMatrix; Sender, Receiver: PUser);

// Exporto a DOT en 'Root-Reportes/relaciones.dot'
function ExportRelationsDOT(const DirPath: string): Boolean;

implementation

uses
  SysUtils;

function EnsureHead(var H: PHead; U: PUser): PHead;
var C, P, N: PHead; // [APUNTADORES] C = cursor, P = previo, N = nuevo
begin
  // Yo mantengo las cabeceras ordenadas por Id del usuario (garantiza orden estable en filas/columnas)
  C := H; P := nil;
  while (C <> nil) and (C^.User^.Id < U^.Id) do begin P := C; C := C^.Next; end;

  // Si ya existe la cabecera para este usuario, la regreso
  if (C <> nil) and (C^.User = U) then Exit(C);

  // Si no existe, creo una nueva cabecera y la inserto en la lista simple
  New(N);                 // [APUNTADOR] Reservo memoria para la cabecera
  N^.User := U;           // [APUNTADOR] Guardo el puntero al usuario
  N^.Next := C;           // [APUNTADOR] Inserto entre P y C
  N^.Node := nil;         // [APUNTADOR] Aún no hay celdas en esta fila/columna
  if P = nil then H := N else P^.Next := N;
  Result := N;
end;

procedure InsertInRow(RowH: PHead; N: PRelNode);
var C, P: PRelNode; // [APUNTADORES] C = cursor, P = previo
begin
  // Inserto el nodo N en la FILA correspondiente, ordenado por Id del destinatario (ColUser^.Id)
  C := RowH^.Node; P := nil;
  while (C <> nil) and (C^.ColUser^.Id < N^.ColUser^.Id) do begin P := C; C := C^.Right; end;

  // Enlazo N con sus vecinos horizontales
  N^.Right := C; N^.Left := P;
  if P = nil then RowH^.Node := N else P^.Right := N;
  if C <> nil then C^.Left := N;
end;

procedure InsertInCol(ColH: PHead; N: PRelNode);
var C, P: PRelNode; // [APUNTADORES] C = cursor, P = previo
begin
  // Inserto el nodo N en la COLUMNA correspondiente, ordenado por Id del remitente (RowUser^.Id)
  C := ColH^.Node; P := nil;
  while (C <> nil) and (C^.RowUser^.Id < N^.RowUser^.Id) do begin P := C; C := C^.Down; end;

  // Enlazo N con sus vecinos verticales
  N^.Down := C; N^.Up := P;
  if P = nil then ColH^.Node := N else P^.Down := N;
  if C <> nil then C^.Up := N;
end;

function FindCell(RowH: PHead; ColUser: PUser): PRelNode;
var C: PRelNode; // [APUNTADOR]
begin
  // Busco en la FILA de RowH, avanzando por la derecha, hasta encontrar la columna ColUser
  C := RowH^.Node;
  while (C <> nil) and (C^.ColUser^.Id < ColUser^.Id) do C := C^.Right;
  if (C <> nil) and (C^.ColUser = ColUser) then Exit(C);
  Result := nil;
end;

procedure InitRelMatrix(var M: TRelMatrix);
begin
  // Dejo vacías las listas de cabeceras y reseteo conteo
  M.RowHeads := nil; // [APUNTADOR]
  M.ColHeads := nil; // [APUNTADOR]
  M.Total := 0;
end;

procedure InitRel;
begin
  // Inicializo la matriz global y aseguro la carpeta de reportes
  InitRelMatrix(RelMatrix);
  if not DirectoryExists(OUT_DIR_REL) then
    ForceDirectories(OUT_DIR_REL);
end;

procedure IncrementEdge(var M: TRelMatrix; Sender, Receiver: PUser);
var RH, CH: PHead;   // [APUNTADORES] Cabeceras de fila (remitente) y columna (destinatario)
    Cell: PRelNode;  // [APUNTADOR] Celda Row->Col
begin
  // Si no hay remitente o destinatario, no hago nada
  if (Sender = nil) or (Receiver = nil) then Exit;

  // Aseguro la existencia de las cabeceras para fila y columna
  RH := EnsureHead(M.RowHeads, Sender);
  CH := EnsureHead(M.ColHeads, Receiver);

  // Busco si ya existe la celda Row->Col en esta fila
  Cell := FindCell(RH, Receiver);
  if Cell = nil then
  begin
    // Si no existe, creo una nueva celda y la enlazo ortogonalmente
    New(Cell);                    // [APUNTADOR] Reservo memoria para la celda
    Cell^.RowUser := Sender;      // [APUNTADOR] Guardo puntero al remitente
    Cell^.ColUser := Receiver;    // [APUNTADOR] Guardo puntero al destinatario
    Cell^.Count   := 0;
    Cell^.Right := nil; Cell^.Left := nil; Cell^.Down := nil; Cell^.Up := nil;

    // Inserto en la fila y en la columna, manteniendo orden
    InsertInRow(RH, Cell);
    InsertInCol(CH, Cell);

    Inc(M.Total); // Llevo control de cuántas celdas existen
  end;

  // Sumo un envío en esa relación
  Inc(Cell^.Count);
end;

function ExportRelationsDOT(const DirPath: string): Boolean;
var
  F: TextFile;
  R: PHead;        // [APUNTADOR] Recorro cabeceras de FILAS (remitentes)
  C: PHead;        // [APUNTADOR] Recorro cabeceras de COLUMNAS (destinatarios)
  N: PRelNode;     // [APUNTADOR] Recorro celdas de una fila (derecha)
  OutPath: string;

  procedure W(const S: string); inline;
  begin
    Writeln(F, S);
  end;

  function RowId(U: PUser): string; inline;
  begin
    // Identificador único de nodo cabecera de fila
    Result := 'row_' + IntToStr(U^.Id);
  end;

  function ColId(U: PUser): string; inline;
  begin
    // Identificador único de nodo cabecera de columna
    Result := 'col_' + IntToStr(U^.Id);
  end;

  function CellId(FromU, ToU: PUser): string; inline;
  begin
    // Identificador único de celda (From->To)
    Result := 'cell_' + IntToStr(FromU^.Id) + '_' + IntToStr(ToU^.Id);
  end;

begin
  Result := False;
  // Valido/creo directorio
  if (DirPath = '') then Exit;
  if not DirectoryExists(DirPath) then
    if not ForceDirectories(DirPath) then Exit;

  // Archivo destino
  OutPath := IncludeTrailingPathDelimiter(DirPath) + 'relaciones.dot';
  AssignFile(F, OutPath);
  try
    Rewrite(F);

    // Cabecera DOT
    W('digraph G {');
    W('  graph [label="Matriz Dispersa", labelloc=top, fontsize=20, pad=0.3];');
    W('  node  [shape=box, fontname="Sans"];');
    W('  edge  [arrowsize=0.7];');
    W('  rankdir=LR;');      // Yo ubico filas a la izquierda y columnas arriba
    W('');

    // 1) Cabeceras de columnas (arriba)
    W('  // Cabeceras de columnas (destinatarios)');
    W('  { rank=same;');
    W('    col_title [label="", width=0.1, height=0.1, shape=box, style=invis];');
    C := RelMatrix.ColHeads;
    while C <> nil do
    begin
      W(Format('    %s [label="%s", style="filled", fillcolor="#bfe3f0"];',
         [ColId(C^.User), StringReplace(C^.User^.Email, '"', '\"', [rfReplaceAll])]));
      C := C^.Next; // [APUNTADOR]
    end;
    W('  }');
    W('');

    // 2) Por cada fila: cabecera de fila + placeholders por columna + reemplazo con celdas reales
    R := RelMatrix.RowHeads;

    // "placeholders invisibles" por cada columna para alinear las celdas en rejilla
    while R <> nil do
    begin
      // 2.a) rango de fila (cabecera + placeholders)
      W(Format('  { rank=same; %s [label="%s", style="filled", fillcolor="#c9f7c2"];',
        [RowId(R^.User), StringReplace(R^.User^.Email, '"', '\"', [rfReplaceAll])]));

      // Creo placeholders invisibles para todas las columnas
      C := RelMatrix.ColHeads;
      while C <> nil do
      begin
        W(Format('    %s [label="", width=1.2, height=0.6, style=invis, shape=box];',
          [CellId(R^.User, C^.User)]));
        C := C^.Next; // [APUNTADOR]
      end;
      W('  }');

      // 2.b) Reemplazo placeholders por celdas reales donde exista relación en esta fila
      N := R^.Node; // [APUNTADOR] primer nodo real de la fila
      while N <> nil do
      begin
        W(Format('  %s [label="%d", style="filled", fillcolor="#f9a825"];',
          [CellId(N^.RowUser, N^.ColUser), N^.Count]));
        // Opcional: enlaces decorativos desde cabecera de fila hacia celda y de celda hacia cabecera de columna
        W(Format('  %s -> %s [color="#888888", dir=none, penwidth=1];',
          [RowId(R^.User), CellId(N^.User, N^.ColUser)]));
        W(Format('  %s -> %s [color="#888888", dir=none, penwidth=1];',
          [CellId(N^.RowUser, N^.ColUser), ColId(N^.ColUser)]));
        N := N^.Right; // [APUNTADOR] avanzo a la siguiente celda en la fila
      end;

      R := R^.Next; // [APUNTADOR] siguiente fila
      W('');
    end;

    // 3) Alineación vertical: uno cada "columna" con aristas invisibles para que Graphviz respete el grid
    W('  // Alineación vertical por columnas');
    C := RelMatrix.ColHeads;
    while C <> nil do
    begin
      W(Format('  col_title -> %s [style=invis, weight=10];', [ColId(C^.User)]));
      R := RelMatrix.RowHeads;
      if R <> nil then
      begin
        W(Format('  %s -> %s [style=invis, weight=10];',
          [ColId(C^.User), CellId(R^.User, C^.User)]));
        while (R <> nil) and (R^.Next <> nil) do
        begin
          W(Format('  %s -> %s [style=invis, weight=10];',
            [CellId(R^.User, C^.User), CellId(R^.Next^.User, C^.User)]));
          R := R^.Next; // [APUNTADOR]
        end;
      end;
      C := C^.Next; // [APUNTADOR]
    end;

    // 4) Alineación horizontal de la fila de cabeceras de columnas
    W('  // Alineación horizontal de la fila de cabeceras');
    C := RelMatrix.ColHeads;
    if C <> nil then
    begin
      W(Format('  col_title -> %s [style=invis, weight=50];', [ColId(C^.User)]));
      while (C <> nil) and (C^.Next <> nil) do
      begin
        W(Format('  %s -> %s [style=invis, weight=50];',
          [ColId(C^.User), ColId(C^.Next^.User)]));
        C := C^.Next; // [APUNTADOR]
      end;
    end;

    W('}');
    CloseFile(F);
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

