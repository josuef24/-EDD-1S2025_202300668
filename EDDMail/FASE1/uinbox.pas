unit uInbox;

{$mode objfpc}{$H+}

interface

type
  PMail = ^TMail;  // [APUNTADOR] Puntero a un nodo de correo en la lista doble
  TMail = record
    // enlaces (lista doblemente enlazada)
    Prev, Next : PMail; // [APUNTADOR] Prev = anterior, Next = siguiente
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
    Head, Tail : PMail; // [APUNTADOR] Head = primer correo, Tail = último
    Count      : Integer; // cantidad de correos en la bandeja
  end;

var
  NextMailId: Integer = 1; //  autoincremental para nuevos correos

procedure InitInbox(var B: TInbox);
function AddMail(var B: TInbox; const ARem, AAsunto, AFecha, AMensaje: AnsiString;
                 const AProg: Boolean): PMail;
function CountUnread(const B: TInbox): Integer;
function GetMailByIndex(const B: TInbox; Index: Integer): PMail;
procedure MarkRead(M: PMail);
procedure DetachMail(var B: TInbox; M: PMail);
procedure SortBySubject(var B: TInbox);

function ExtractMailAt(var I: TInbox; Index: Integer): PMail;
function ExportInboxDOTForUser(const UserEmail: string; const B: TInbox;
                               const BaseDir: string; out DotPath: string): Boolean;


implementation

uses SysUtils, uMatrix, Classes, StrUtils;

procedure InitInbox(var B: TInbox);
begin
  //  Dejo la lista vacía: sin nodos y conteo en cero
  B.Head := nil;
  B.Tail := nil;
  B.Count := 0;
end;

function AddMail(var B: TInbox; const ARem, AAsunto, AFecha, AMensaje: AnsiString;
                 const AProg: Boolean): PMail;
var
  N: PMail; // [APUNTADOR] nuevo nodo de correo
begin
  //  Reservo nodo y cargo sus datos
  New(N);
  N^.Id         := NextMailId; Inc(NextMailId);
  N^.Remitente  := ARem;
  N^.Estado     := 'NL';       // [EDIT] los nuevos entran como No Leído
  N^.Programado := AProg;
  N^.Asunto     := AAsunto;
  N^.Fecha      := AFecha;
  N^.Mensaje    := AMensaje;

  //  Inserto al final (push back): ajusto punteros Prev/Next
  N^.Prev := B.Tail; // [APUNTADOR] el anterior del nuevo es el Tail actual
  N^.Next := nil;

  if B.Head = nil then
    B.Head := N             // si estaba vacía, este es el primer nodo
  else
    B.Tail^.Next := N;      // si no, enlazo el último con el nuevo

  B.Tail := N;              // actualizo Tail al nuevo
  Inc(B.Count);             // aumento conteo
  Result := N;
end;

function CountUnread(const B: TInbox): Integer;
var C: PMail; // [APUNTADOR] cursor para recorrer desde Head hacia la derecha
begin
  //  Cuento los 'NL' caminando la lista
  Result := 0;
  C := B.Head;
  while C <> nil do
  begin
    if C^.Estado = 'NL' then Inc(Result);
    C := C^.Next; // [APUNTADOR] avanzo al siguiente
  end;
end;

function GetMailByIndex(const B: TInbox; Index: Integer): PMail;
var C: PMail; i: Integer; // [APUNTADOR] cursor + índice
begin

  if (Index < 0) or (Index >= B.Count) then Exit(nil);

  // [RECORRIDO] camino desde Head hasta el índice
  C := B.Head; i := 0;
  while (C <> nil) and (i < Index) do
  begin
    C := C^.Next; Inc(i);
  end;
  Result := C;
end;

procedure MarkRead(M: PMail);
begin
  //  Marco como leído si estaba en 'NL'
  if (M <> nil) and (M^.Estado = 'NL') then
    M^.Estado := 'L';
end;

procedure DetachMail(var B: TInbox; M: PMail);
begin
  //  Saco el nodo M de la lista doble (no lo libero, solo lo separo)
  if M = nil then Exit;

  //  Puenteo al nodo, cuidando extremos
  if M^.Prev <> nil then
    M^.Prev^.Next := M^.Next
  else
    B.Head := M^.Next;

  if M^.Next <> nil then
    M^.Next^.Prev := M^.Prev
  else
    B.Tail := M^.Prev;

  //  dejo sueltos los enlaces del nodo extraído
  M^.Prev := nil;
  M^.Next := nil;
  Dec(B.Count);
end;

procedure SortBySubject(var B: TInbox);
var SortedHead, SortedTail, Curr, NextN, P, InsBefore: PMail; // [APUNTADORES]
begin
  //  Hago un insertion sort estable por Asunto (A-Z) rearmando enlaces Prev/Next
  SortedHead := nil; SortedTail := nil;
  Curr := B.Head; // recorro la lista original
  while Curr <> nil do
  begin
    NextN := Curr^.Next;          // [APUNTADOR] guardo el siguiente antes de desconectar
    Curr^.Prev := nil; Curr^.Next := nil; // desconecto el nodo para reinsertarlo

    if SortedHead = nil then
    begin
      // primera inserción
      SortedHead := Curr; SortedTail := Curr;
    end
    else
    begin
      // busco dónde insertar por orden alfabético (CompareText case-insensitive)
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
        // inserto al final
        Curr^.Prev := SortedTail;
        SortedTail^.Next := Curr;
        SortedTail := Curr;
      end
      else if InsBefore^.Prev = nil then
      begin
        // inserto al inicio
        Curr^.Next := InsBefore;
        InsBefore^.Prev := Curr;
        SortedHead := Curr;
      end
      else
      begin
        // inserto en medio (entre InsBefore^.Prev y InsBefore)
        Curr^.Prev := InsBefore^.Prev;
        Curr^.Next := InsBefore;
        InsBefore^.Prev^.Next := Curr;
        InsBefore^.Prev := Curr;
      end;
    end;

    Curr := NextN; // [APUNTADOR] continuo con el siguiente de la lista original
  end;

  //  Actualizo los extremos de la bandeja al ordenado
  B.Head := SortedHead;
  B.Tail := SortedTail;
end;

function ExtractMailAt(var I: TInbox; Index: Integer): PMail;
var M: PMail; // [APUNTADOR]
begin
  //  Saco (desengancho) el correo en la posición Index y lo devuelvo suelto
  Result := nil;
  M := GetMailByIndex(I, Index);
  if M = nil then Exit;
  DetachMail(I, M);
  Result := M; // ahora puedo mandarlo a Trash (pila) o a otro lado
end;

function ExportInboxDOTForUser(const UserEmail: string; const B: TInbox;
                               const BaseDir: string; out DotPath: string): Boolean;
var
  F: TextFile;  //  manejador del archivo DOT
  N: PMail;     // [APUNTADOR] cursor para recorrer la lista
  Path: string; //  ruta final del archivo
begin
  Result := False;
  DotPath := '';
  if BaseDir = '' then Exit;

  //  Aseguro carpeta de salida
  if not DirectoryExists(BaseDir) then
    if not ForceDirectories(BaseDir) then Exit;

  //  Armo nombre: inbox_<email>.dot
  Path := IncludeTrailingPathDelimiter(BaseDir) + 'inbox_' + UserEmail + '.dot';
  AssignFile(F, Path);
  try
    Rewrite(F);
    Writeln(F, 'digraph "Reporte de Correos Recibidos" {');
    Writeln(F, '  rankdir=LR;');
    Writeln(F, '  labelloc="t";');
    Writeln(F, '  label="Reporte de Correos Recibidos";');
    Writeln(F, '  node [shape=box, style="rounded,filled", fillcolor="#fff7cc"];');

    //  Contenedor visual para resaltar que es lista doblemente enlazada
    Writeln(F, '  subgraph cluster_inbox {');
    Writeln(F, '    label="Lista Doblemente Enlazada";');
    Writeln(F, '    color="#bbbbbb";');

    //  Emisión de nodos y aristas dobles (prev<->next)
    N := B.Head;
    while N <> nil do
    begin
      Writeln(F, '    n', N^.Id, ' [label=<',
        '<b>ID:</b> ', N^.Id, '<br/>',
        '<b>Remitente:</b> ', N^.Remitente, '<br/>',
        '<b>Estado:</b> ', N^.Estado, '<br/>',
        '<b>Programado:</b> ', IfThen(N^.Programado, 'Sí', 'No'), '<br/>',
        '<b>Asunto:</b> ', N^.Asunto, '<br/>',
        '<b>Fecha:</b> ', N^.Fecha, '<br/>',
        '<b>Mensaje:</b> ', StringReplace(N^.Mensaje, '<', '&lt;', [rfReplaceAll]),
        '>, width=3];');

      if N^.Next <> nil then
        Writeln(F, '    n', N^.Id, ' -> n', N^.Next^.Id, ' [dir=both, arrowsize=0.6];');

      N := N^.Next; // [APUNTADOR] sigo recorriendo hacia adelante
    end;

    Writeln(F, '  }'); // fin subgraph
    Writeln(F, '}');
    CloseFile(F);

    DotPath := Path;
    Result  := True;
  except
    on E: Exception do
    begin
      // Intento cerrar y reporto fallo
      {$I-} CloseFile(F); {$I+}
      Result := False;
    end;
  end;
end;

end.


