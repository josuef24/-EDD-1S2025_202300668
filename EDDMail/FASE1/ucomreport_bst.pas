unit UComReport_BST;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, UComunidadesBST;

function ExportComunidadesBST_DOT(Root: PComNode; const OutDir: string; out DotPath: string): Boolean;

implementation

function CountMsgs(M: PMsg): Integer;
begin
  Result := 0;
  while M <> nil do begin Inc(Result); M := M^.Next; end;
end;

function EnsureDir(const D: string): Boolean;
begin
  if (D='') then Exit(False);
  if DirectoryExists(D) then Exit(True);
  Result := ForceDirectories(D);
end;

function ExportComunidadesBST_DOT(Root: PComNode; const OutDir: string; out DotPath: string): Boolean;
var
  F   : TextFile;
  IdC : Integer;

  function Esc(const S: string): string;
  begin
    Result := StringReplace(S, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
    Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  end;

  function Emit(N: PComNode): Integer;
  var
    my, L, R, msgs: Integer;
  begin
    if N = nil then Exit(0);

    // emito hijos primero para poder tener sus ids si prefieres, pero aquí
    // emitimos padre y luego pedimos ids de hijos recursivamente:
    my := IdC; Inc(IdC);

    msgs := CountMsgs(N^.MsgHead);

    // Nodo (HTML-like label)
    WriteLn(F, '  n', my, ' [shape=plain, label=<');
    WriteLn(F, '    <TABLE BORDER="1" CELLBORDER="0" CELLSPACING="0" BGCOLOR="#fff7ef">');
    WriteLn(F, '      <TR><TD><B>', Esc(N^.Nombre), '</B></TD></TR>');
    WriteLn(F, '      <TR><TD>Fecha creaci&oacute;n: ', Esc(N^.FechaCreacion), '</TD></TR>');
    WriteLn(F, '      <TR><TD>Mensajes publicados: ', msgs, '</TD></TR>');
    WriteLn(F, '    </TABLE>');
    WriteLn(F, '  >];');

    // hijos
    L := 0; R := 0;
    if N^.Left  <> nil then L := Emit(N^.Left);
    if N^.Right <> nil then R := Emit(N^.Right);

    if L <> 0 then WriteLn(F, '  n', my, ' -> n', L, ' [arrowsize=0.8];');
    if R <> 0 then WriteLn(F, '  n', my, ' -> n', R, ' [arrowsize=0.8];');

    Result := my;
  end;

begin
  Result  := False;
  DotPath := '';

  if Root = nil then Exit(False);
  if not EnsureDir(OutDir) then Exit(False);

  DotPath := IncludeTrailingPathDelimiter(OutDir) + 'comunidades_bst.dot';
  AssignFile(F, DotPath);
  try
    Rewrite(F);

    // Encabezado y estilo general
    WriteLn(F, 'digraph "Reporte de comunidades (Árbol BST)" {');
    WriteLn(F, '  rankdir=TB;');
    WriteLn(F, '  labelloc="t";');
    WriteLn(F, '  node [fontname="Helvetica"];');
    WriteLn(F, '  edge [color="#555555"];');
    WriteLn(F, '  label="Reporte de comunidades (Árbol BST)";');

    IdC := 1;
    Emit(Root);

    WriteLn(F, '}');
    CloseFile(F);
    Result := True;
  except
    on E: Exception do
    begin
      {$I-} CloseFile(F); {$I+}
      Result  := False;
      DotPath := '';
    end;
  end;
end;

end.

