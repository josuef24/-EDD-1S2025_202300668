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
  R: PHead;
  N: PRelNode;
  OutPath: string;
begin
  Result := False;
  if DirPath = '' then Exit;
  if not DirectoryExists(DirPath) then
    if not ForceDirectories(DirPath) then Exit;

  OutPath := IncludeTrailingPathDelimiter(DirPath) + 'relaciones.dot';
  AssignFile(F, OutPath);
  try
    Rewrite(F);
    Writeln(F, 'digraph Relaciones {');
    Writeln(F, '  rankdir=LR;');
    Writeln(F, '  node [shape=box];');
    // recorrer filas
    R := RelMatrix.RowHeads;
    while R <> nil do
    begin
      N := R^.Node;
      while N <> nil do
      begin
        // "sender" -> "receiver" [label=count]
        Writeln(F, '  "', N^.RowUser^.Email, '" -> "', N^.ColUser^.Email,
                   '" [label="', N^.Count, '"];');
        N := N^.Right;
      end;
      R := R^.Next;
    end;
    Writeln(F, '}');
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

