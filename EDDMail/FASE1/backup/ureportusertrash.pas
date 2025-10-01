unit uReportUserTrash;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, uTrash, uInbox;   // TTrash, PTrashNode, PMail

// Genera el .dot de la papelera del usuario:
//   BaseDir/User-Reportes/papelera_<email>.dot
// Devuelve True si el archivo se creó correctamente y deja el path en DotPath.
function ExportTrashDOTForUser(const UserEmail: string; const T: TTrash;
                               const BaseDir: string; out DotPath: string): Boolean;

implementation

function HtmlSafe(const S: string): string;
var R: string;
begin
  R := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  R := StringReplace(R, '<', '&lt;',  [rfReplaceAll]);
  R := StringReplace(R, '>', '&gt;',  [rfReplaceAll]);
  Result := R;
end;

function YesNo(B: Boolean): string;
begin
  if B then Result := 'Sí' else Result := 'No';
end;

function ExportTrashDOTForUser(const UserEmail: string; const T: TTrash;
                               const BaseDir: string; out DotPath: string): Boolean;
var
  F: TextFile;
  N: PTrashNode;
  OutDir: string;
begin
  Result  := False;
  DotPath := '';

  if BaseDir = '' then Exit;

  OutDir := IncludeTrailingPathDelimiter(BaseDir);
  if not DirectoryExists(OutDir) then
    if not ForceDirectories(OutDir) then Exit;

  DotPath := OutDir + 'papelera_' + UserEmail + '.dot';

  AssignFile(F, DotPath);
  try
    Rewrite(F);

    // Encabezado
    Writeln(F, 'digraph "Reporte de Papelera" {');
    Writeln(F, '  labelloc="t";');
    Writeln(F, '  label="Reporte de Papelera";');
    Writeln(F, '  rankdir=TB;');  // de arriba hacia abajo
    Writeln(F, '  node [shape=box, style="rounded,filled", fillcolor="#f8d6d6"];');

    // Contenedor (simula la pila)
    Writeln(F, '  subgraph cluster_trash {');
    Writeln(F, '    label="Pila";');
    Writeln(F, '    color="#bbbbbb";');

    // Recorremos de TOP hacia abajo
    N := T.Top;
    while N <> nil do
    begin
      // Un rectángulo por correo con su información
      Writeln(F, '    n', N^.Mail^.Id, ' [label=<',
        '<b>ID:</b> ', N^.Mail^.Id, '<br/>',
        '<b>Remitente:</b> ', HtmlSafe(N^.Mail^.Remitente), '<br/>',
        '<b>Estado:</b> Eliminado<br/>',
        '<b>Programado:</b> ', YesNo(N^.Mail^.Programado), '<br/>',
        '<b>Asunto:</b> ', HtmlSafe(N^.Mail^.Asunto), '<br/>',
        '<b>Fecha:</b> ', HtmlSafe(N^.Mail^.Fecha), '<br/>',
        '<b>Mensaje:</b> ', HtmlSafe(N^.Mail^.Mensaje),
        '>];');

      // Flecha hacia el siguiente (tope -> siguiente)
      if N^.Next <> nil then
      begin
        // flecha sólida simple hacia abajo
        Writeln(F, '    n', N^.Mail^.Id, ' -> n', N^.Next^.Mail^.Id, ' [arrowsize=0.8];');
      end;

      N := N^.Next;
    end;

    Writeln(F, '  }'); // fin cluster_trash
    Writeln(F, '}');
    CloseFile(F);
    Result := True;

  except
    on E: Exception do
    begin
      {$I-} CloseFile(F); {$I+}
      DotPath := '';
      Result := False;
    end;
  end;
end;

end.
