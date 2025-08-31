unit uMatrix;

{$mode objfpc}{$H+}

interface

uses
  uUsers;  // PUser

const
  OUT_DIR_REL = 'Root-Reportes/';  // carpeta para reportes de relaciones

type
  PRelNode = ^TRelNode;
  TRelNode = record
    RowUser : PUser;   // remitente (fila)
    ColUser : PUser;   // destinatario (columna)
    Count   : Integer; // # correos enviados Row->Col
    // enlaces ortogonales
    Right, Left : PRelNode;
    Down,  Up   : PRelNode;
  end;

  PHead = ^THead;
  THead = record
    User : PUser;   // identifica fila o columna
    Next : PHead;   // cabeceras ordenadas por Id (o por puntero)
    Node : PRelNode; // primer nodo de esa fila/columna
  end;

  TRelMatrix = record
    RowHeads : PHead; // cabeceras por remitente
    ColHeads : PHead; // cabeceras por destinatario
    Total    : Integer; // # celdas (pares con al menos 1 envío)
  end;

var
  RelMatrix: TRelMatrix;  // matriz global de relaciones

procedure InitRelMatrix(var M: TRelMatrix);
procedure InitRel; // inicializa la global

// Suma 1 al conteo de (Sender -> Receiver)
procedure IncrementEdge(var M: TRelMatrix; Sender, Receiver: PUser);

// (opcional) Exportar a DOT en 'Root-Reportes/relaciones.dot'
function ExportRelationsDOT(const DirPath: string): Boolean;

implementation

uses
  SysUtils;

function EnsureHead(var H: PHead; U: PUser): PHead;
var C, P, N: PHead;
begin
  // Mantén orden por Id (siempre tenemos Id en TUserNode)
  C := H; P := nil;
  while (C <> nil) and (C^.User^.Id < U^.Id) do begin P := C; C := C^.Next; end;

  if (C <> nil) and (C^.User = U) then Exit(C);

  New(N);
  N^.User := U; N^.Next := C; N^.Node := nil;
  if P = nil then H := N else P^.Next := N;
  Result := N;
end;

procedure InsertInRow(RowH: PHead; N: PRelNode);
var C, P: PRelNode;
begin
  // Inserta en la fila ordenado por ColUser^.Id
  C := RowH^.Node; P := nil;
  while (C <> nil) and (C^.ColUser^.Id < N^.ColUser^.Id) do begin P := C; C := C^.Right; end;
  N^.Right := C; N^.Left := P;
  if P = nil then RowH^.Node := N else P^.Right := N;
  if C <> nil then C^.Left := N;
end;

procedure InsertInCol(ColH: PHead; N: PRelNode);
var C, P: PRelNode;
begin
  // Inserta en la columna ordenado por RowUser^.Id
  C := ColH^.Node; P := nil;
  while (C <> nil) and (C^.RowUser^.Id < N^.RowUser^.Id) do begin P := C; C := C^.Down; end;
  N^.Down := C; N^.Up := P;
  if P = nil then ColH^.Node := N else P^.Down := N;
  if C <> nil then C^.Up := N;
end;

function FindCell(RowH: PHead; ColUser: PUser): PRelNode;
var C: PRelNode;
begin
  C := RowH^.Node;
  while (C <> nil) and (C^.ColUser^.Id < ColUser^.Id) do C := C^.Right;
  if (C <> nil) and (C^.ColUser = ColUser) then Exit(C);
  Result := nil;
end;

procedure InitRelMatrix(var M: TRelMatrix);
begin
  M.RowHeads := nil;
  M.ColHeads := nil;
  M.Total := 0;
end;

procedure InitRel;
begin
  InitRelMatrix(RelMatrix);
  if not DirectoryExists(OUT_DIR_REL) then
    ForceDirectories(OUT_DIR_REL);
end;

procedure IncrementEdge(var M: TRelMatrix; Sender, Receiver: PUser);
var RH, CH: PHead;
    Cell: PRelNode;
begin
  if (Sender = nil) or (Receiver = nil) then Exit;
  RH := EnsureHead(M.RowHeads, Sender);
  CH := EnsureHead(M.ColHeads, Receiver);

  Cell := FindCell(RH, Receiver);
  if Cell = nil then
  begin
    New(Cell);
    Cell^.RowUser := Sender;
    Cell^.ColUser := Receiver;
    Cell^.Count   := 0;
    Cell^.Right := nil; Cell^.Left := nil; Cell^.Down := nil; Cell^.Up := nil;
    InsertInRow(RH, Cell);
    InsertInCol(CH, Cell);
    Inc(M.Total);
  end;

  Inc(Cell^.Count);
end;

function ExportRelationsDOT(const DirPath: string): Boolean;
var
  F: TextFile;
  R: PHead;        // recorre filas (remitentes)
  C: PHead;        // recorre columnas (destinatarios)
  N: PRelNode;     // recorre celdas de una fila
  OutPath: string;

  procedure W(const S: string); inline;
  begin
    Writeln(F, S);
  end;

  function RowId(U: PUser): string; inline;
  begin
    Result := 'row_' + IntToStr(U^.Id);
  end;

  function ColId(U: PUser): string; inline;
  begin
    Result := 'col_' + IntToStr(U^.Id);
  end;

  function CellId(FromU, ToU: PUser): string; inline;
  begin
    Result := 'cell_' + IntToStr(FromU^.Id) + '_' + IntToStr(ToU^.Id);
  end;

begin
  Result := False;
  if (DirPath = '') then Exit;
  if not DirectoryExists(DirPath) then
    if not ForceDirectories(DirPath) then Exit;

  OutPath := IncludeTrailingPathDelimiter(DirPath) + 'relaciones.dot';
  AssignFile(F, OutPath);
  try
    Rewrite(F);

    // --- Cabecera DOT ---
    W('digraph G {');
    W('  graph [label="Matriz Dispersa", labelloc=top, fontsize=20, pad=0.3];');
    W('  node  [shape=box, fontname="Sans"];');
    W('  edge  [arrowsize=0.7];');
    W('  rankdir=LR;');      // filas a la izquierda, columnas arriba (las forzaremos con ranks)
    W('');

    // --- 1) Nodos cabecera de columnas (arriba) ---
    W('  // Cabeceras de columnas (destinatarios)');
    W('  { rank=same;');
    W('    col_title [label="", width=0.1, height=0.1, shape=box, style=invis];');
    C := RelMatrix.ColHeads;
    while C <> nil do
    begin
      W(Format('    %s [label="%s", style="filled", fillcolor="#bfe3f0"];',
         [ColId(C^.User), StringReplace(C^.User^.Email, '"', '\"', [rfReplaceAll])]));
      C := C^.Next;
    end;
    W('  }');
    W('');

    // --- 2) Nodos cabecera de filas (a la izquierda) y celdas por fila ---
    R := RelMatrix.RowHeads;

    // Para mantener las columnas alineadas, creamos nodos “fantasma” por columna en cada fila
    // (invisibles y ligados en vertical) y colocamos cada celda en el mismo rango que su fila.
    while R <> nil do
    begin
      // 2.a) rango de fila: cabecera de fila + “fantasmas”/celdas
      W(Format('  { rank=same; %s [label="%s", style="filled", fillcolor="#c9f7c2"];',
        [RowId(R^.User), StringReplace(R^.User^.Email, '"', '\"', [rfReplaceAll])]));

      // crea placeholders invisibles para TODAS las columnas (así cada fila tiene misma cantidad de posiciones)
      C := RelMatrix.ColHeads;
      while C <> nil do
      begin
        // por defecto un placeholder invisible en la intersección
        W(Format('    %s [label="", width=1.2, height=0.6, style=invis, shape=box];',
          [CellId(R^.User, C^.User)]));
        C := C^.Next;
      end;
      W('  }');

      // 2.b) Reemplaza placeholders por celdas reales donde exista relación
      N := R^.Node;
      while N <> nil do
      begin
        W(Format('  %s [label="%d", style="filled", fillcolor="#f9a825"];',
          [CellId(N^.RowUser, N^.ColUser), N^.Count]));
        // flechas chicas dobles entre cabeceras para imitar el enunciado (opcional decorativo)
        W(Format('  %s -> %s [color="#888888", dir=none, penwidth=1];',
          [RowId(R^.User), CellId(N^.RowUser, N^.ColUser)]));
        W(Format('  %s -> %s [color="#888888", dir=none, penwidth=1];',
          [CellId(N^.RowUser, N^.ColUser), ColId(N^.ColUser)]));
        N := N^.Right;
      end;

      R := R^.Next;
      W('');
    end;

    // --- 3) Reglas de alineación: unir verticalmente cada columna con edges invisibles ---
    W('  // Alineación vertical por columnas');
    C := RelMatrix.ColHeads;
    while C <> nil do
    begin
      // une cabecera de columna con la “primera fila” invisible, y así sucesivamente
      W(Format('  col_title -> %s [style=invis, weight=10];', [ColId(C^.User)]));
      R := RelMatrix.RowHeads;
      if R <> nil then
      begin
        // une cabecera de columna con la primera celda/placeholder y el resto en cadena
        W(Format('  %s -> %s [style=invis, weight=10];',
          [ColId(C^.User), CellId(R^.User, C^.User)]));
        while (R <> nil) and (R^.Next <> nil) do
        begin
          W(Format('  %s -> %s [style=invis, weight=10];',
            [CellId(R^.User, C^.User), CellId(R^.Next^.User, C^.User)]));
          R := R^.Next;
        end;
      end;
      C := C^.Next;
    end;

    // --- 4) Unir horizontalmente: título -> columnas (invisibles) para fijar la fila superior
    W('  // Alineación horizontal de la fila de cabeceras');
    C := RelMatrix.ColHeads;
    if C <> nil then
    begin
      W(Format('  col_title -> %s [style=invis, weight=50];', [ColId(C^.User)]));
      while (C <> nil) and (C^.Next <> nil) do
      begin
        W(Format('  %s -> %s [style=invis, weight=50];',
          [ColId(C^.User), ColId(C^.Next^.User)]));
        C := C^.Next;
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

