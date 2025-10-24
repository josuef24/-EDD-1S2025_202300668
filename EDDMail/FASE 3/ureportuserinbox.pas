unit uReportUserInbox;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, uUsers, uInbox, StrUtils;

function ExportInboxDOT(const DirPath: string; const U: PUser): Boolean;

implementation

function Esc(const S: AnsiString): AnsiString;
begin
  //  Aquí escapo comillas dobles y convierto saltos de línea a \n para que DOT no se rompa
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, LineEnding, '\n', [rfReplaceAll]);
end;

function ExportInboxDOT(const DirPath: string; const U: PUser): Boolean;
var
  F     : TextFile;     // Manejador del archivo .dot
  OutDir, DotPath: string; // Carpeta y ruta del .dot
  N     : PMail;        // Puntero a nodo de correo (lista doblemente enlazada)
begin
  Result := False;
  // Si no me pasan usuario (puntero nulo), no genero nada
  if (U = nil) then Exit;

  // Aseguro la carpeta de salida del reporte del usuario
  OutDir := IncludeTrailingPathDelimiter(DirPath);
  if (not DirectoryExists(OutDir)) and (not ForceDirectories(OutDir)) then Exit;

  // Defino el archivo DOT usando el Id del usuario
  DotPath := OutDir + 'inbox_' + IntToStr(U^.Id) + '.dot';
  AssignFile(F, DotPath);
  try
    Rewrite(F);

    // Encabezado y estilos del grafo
    Writeln(F, 'digraph "Reporte de Correos Recibidos" {');
    Writeln(F, '  rankdir=LR;');
    Writeln(F, '  labelloc="t";');
    Writeln(F, '  fontsize=16;');
    Writeln(F, '  node [shape=record, style="filled,rounded", fillcolor="#FFF7CC", color="#B7B7B7", fontname="Helvetica"];');
    Writeln(F, '  edge [color="#555555"];');
    Writeln(F, '  subgraph cluster_inbox {');
    Writeln(F, '    label="Lista Doblemente Enlazada";');
    Writeln(F, '    color="#CFCFCF";');
    Writeln(F, '    style="rounded";');

    // Primero emito todos los nodos de la lista doble (de Head hacia adelante)
    // [APUNTADOR] U^.Inbox.Head es puntero al primer correo; si es nil, la bandeja está vacía
    N := U^.Inbox.Head;
    while N <> nil do
    begin
      //  Represento cada correo como un nodo tipo record con sus campos visibles
      Writeln(F, Format('    m%d [label="<f0>ID: %d|Remitente: %s|Estado: %s|Programado: %s|Asunto: %s|Fecha: %s|Mensaje: %s"];',
        [N^.Id, N^.Id, Esc(N^.Remitente), Esc(N^.Estado),
         IfThen(N^.Programado, 'Sí', 'No'),
         Esc(N^.Asunto), Esc(N^.Fecha), Esc(N^.Mensaje)]));
      N := N^.Next; // [APUNTADOR] avanzo al siguiente nodo de la lista doble
    end;

    // Ahora dibujo las flechas dobles entre consecutivos para reflejar prev <-> next
    N := U^.Inbox.Head;
    while (N <> nil) and (N^.Next <> nil) do
    begin
      // [APUNTADOR] N^.Next es puntero al siguiente correo; enlazo ambos sentidos con dir=both
      Writeln(F, Format('    m%d -> m%d [dir=both, arrowsize=0.7];', [N^.Id, N^.Next^.Id]));
      N := N^.Next; // [APUNTADOR] continúo hacia adelante
    end;

    Writeln(F, '  }');
    Writeln(F, '}');

    //  Cierro archivo y marco éxito
    CloseFile(F);
    Result := True;
  except
    on E: Exception do
    begin
      // Si algo falla, intento cerrar y retorno False
      {$I-} CloseFile(F); {$I+}
      Result := False;
    end;
  end;
end;

end.

