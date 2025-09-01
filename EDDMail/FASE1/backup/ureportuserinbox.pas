unit uReportUserInbox;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, uUsers, uInbox;

function ExportInboxDOT(const DirPath: string; const U: PUser): Boolean;

implementation

function Esc(const S: AnsiString): AnsiString;
begin
  // escapa comillas dobles y reemplaza saltos por \n para DOT
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, LineEnding, '\n', [rfReplaceAll]);
end;

function ExportInboxDOT(const DirPath: string; const U: PUser): Boolean;
var
  F     : TextFile;
  OutDir, DotPath: string;
  N     : PMail;
begin
  Result := False;
  if (U = nil) then Exit;

  // carpeta de salida (por proyecto: reportes de usuario)
  OutDir := IncludeTrailingPathDelimiter(DirPath);
  if (not DirectoryExists(OutDir)) and (not ForceDirectories(OutDir)) then Exit;

  DotPath := OutDir + 'inbox_' + IntToStr(U^.Id) + '.dot';
  AssignFile(F, DotPath);
  try
    Rewrite(F);

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

    // nodos
    N := U^.Inbox.Head;
    while N <> nil do
    begin
      Writeln(F, Format('    m%d [label="<f0>ID: %d|Remitente: %s|Estado: %s|Programado: %s|Asunto: %s|Fecha: %s|Mensaje: %s"];',
        [N^.Id, N^.Id, Esc(N^.Remitente), Esc(N^.Estado),
         IfThen(N^.Programado, 'Sí', 'No'),
         Esc(N^.Asunto), Esc(N^.Fecha), Esc(N^.Mensaje)]));
      N := N^.Next;
    end;

    // aristas doble (prev <-> next)
    N := U^.Inbox.Head;
    while (N <> nil) and (N^.Next <> nil) do
    begin
      Writeln(F, Format('    m%d -> m%d [dir=both, arrowsize=0.7];', [N^.Id, N^.Next^.Id]));
      N := N^.Next;
    end;

    Writeln(F, '  }');
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

