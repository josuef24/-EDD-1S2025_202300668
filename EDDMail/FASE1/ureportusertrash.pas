unit uReportUserTrash;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, uTrash, uInbox;

//  Yo genero el .dot de la papelera del usuario en BaseDir/User-Reportes/papelera_<email>.dot
//  Devuelvo True si el archivo se creó bien y dejo la ruta en DotPath
function ExportTrashDOTForUser(const UserEmail: string; const T: TTrash;
                               const BaseDir: string; out DotPath: string): Boolean;

implementation

function HtmlSafe(const S: string): string;
var R: string;
begin
  //  Escapo caracteres HTML para que el DOT no se rompa en etiquetas <...>
  R := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  R := StringReplace(R, '<', '&lt;',  [rfReplaceAll]);
  R := StringReplace(R, '>', '&gt;',  [rfReplaceAll]);
  Result := R;
end;

function YesNo(B: Boolean): string;
begin
  //  Devuelvo "Sí/No" para campos booleanos visibles en el reporte
  if B then Result := 'Sí' else Result := 'No';
end;

function ExportTrashDOTForUser(const UserEmail: string; const T: TTrash;
                               const BaseDir: string; out DotPath: string): Boolean;
var
  F: TextFile;        //  Manejador del archivo .dot
  N: PTrashNode;      // [APUNTADOR] Cursor puntero a nodo de la pila (recorro Top -> siguiente)
  OutDir: string;     //  Carpeta de salida
begin
  Result  := False;
  DotPath := '';

  //  Si no me pasan BaseDir, no puedo escribir
  if BaseDir = '' then Exit;

  //  Aseguro que exista la carpeta de salida
  OutDir := IncludeTrailingPathDelimiter(BaseDir);
  if not DirectoryExists(OutDir) then
    if not ForceDirectories(OutDir) then Exit;

  //  Defino ruta final del DOT
  DotPath := OutDir + 'papelera_' + UserEmail + '.dot';

  AssignFile(F, DotPath);
  try
    Rewrite(F);

    //  Encabezado del grafo
    Writeln(F, 'digraph "Reporte de Papelera" {');
    Writeln(F, '  labelloc="t";');
    Writeln(F, '  label="Reporte de Papelera";');
    Writeln(F, '  rankdir=TB;');  // [DISEÑO] De arriba hacia abajo refleja la pila (Top arriba)
    Writeln(F, '  node [shape=box, style="rounded,filled", fillcolor="#f8d6d6"];');

    // Contenedor para simular el marco de la pila
    Writeln(F, '  subgraph cluster_trash {');
    Writeln(F, '    label="Pila";');
    Writeln(F, '    color="#bbbbbb";');

    //  Recorro desde el tope hacia abajo usando el puntero N
    N := T.Top; // [APUNTADOR] T.Top es puntero al nodo tope o nil si vacío
    while N <> nil do
    begin
      // Un nodo por correo con sus datos. Accedo a PMail con N^.Mail (puntero) y luego a sus campos con ^.
      Writeln(F, '    n', N^.Mail^.Id, ' [label=<',
        '<b>ID:</b> ', N^.Mail^.Id, '<br/>',
        '<b>Remitente:</b> ', HtmlSafe(N^.Mail^.Remitente), '<br/>',
        '<b>Estado:</b> Eliminado<br/>',
        '<b>Programado:</b> ', YesNo(N^.Mail^.Programado), '<br/>',
        '<b>Asunto:</b> ', HtmlSafe(N^.Mail^.Asunto), '<br/>',
        '<b>Fecha:</b> ', HtmlSafe(N^.Mail^.Fecha), '<br/>',
        '<b>Mensaje:</b> ', HtmlSafe(N^.Mail^.Mensaje),
        '>];');

      //  Dibujo la flecha al siguiente elemento de la pila (del tope hacia abajo)
      if N^.Next <> nil then
      begin
        // [APUNTADOR] N^.Next es puntero al siguiente nodo; también accedo a su Mail^.Id
        Writeln(F, '    n', N^.Mail^.Id, ' -> n', N^.Next^.Mail^.Id, ' [arrowsize=0.8];');
      end;

      // [APUNTADOR] Avanzo el cursor al siguiente nodo de la pila
      N := N^.Next;
    end;

    Writeln(F, '  }'); // fin cluster_trash
    Writeln(F, '}');
    CloseFile(F);
    Result := True;

  except
    on E: Exception do
    begin
      //  Si algo falla al escribir, intento cerrar y marco fallo
      {$I-} CloseFile(F); {$I+}
      DotPath := '';
      Result := False;
    end;
  end;
end;

end.

