unit uFavReport_BTree;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, UBTree_Favoritos;  // PBNode, TFavItem

function ExportFavBTreeDOT(const Root: PBNode; const OutDir: string;
                           out DotPath: string): Boolean;

implementation

function EscapeHTML(const S: AnsiString): AnsiString;
begin
  Result := StringReplace(S, '&', '&amp;',   [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''','&#39;',   [rfReplaceAll]);
end;

procedure EmitNode(var F: TextFile; const X: PBNode);
var
  name: String;
  i: Integer;
begin
  name := 'n' + IntToStr(PtrUInt(X)); // único por dirección

  Writeln(F, '    ', name, ' [label=<');
  Writeln(F, '      <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" BGCOLOR="#b6f2b6">');
  Writeln(F, '        <TR>');
  for i := 1 to X^.N do
  begin
    if not X^.Key[i].Activo then Continue;
    Writeln(F, '          <TD ALIGN="LEFT" CELLPADDING="6">',
                  '<B>ID:</B> ', X^.Key[i].ID, '<BR/>',
                  '<B>Remitente:</B> ', EscapeHTML(X^.Key[i].Remitente), '<BR/>',
                  '<B>Asunto:</B> ', EscapeHTML(X^.Key[i].Asunto), '<BR/>',
                  '<B>Fecha:</B> ', EscapeHTML(X^.Key[i].Fecha), '<BR/>',
                  '<B>Mensaje:</B> ', EscapeHTML(X^.Key[i].Mensaje),
                '</TD>');
  end;
  Writeln(F, '        </TR>');
  Writeln(F, '      </TABLE>');
  Writeln(F, '    >, shape=box, style="rounded,filled", fillcolor="#b6f2b6", color="#3a7f3a"];');
end;

procedure EmitEdges(var F: TextFile; const X: PBNode);
var parent, child: String; j: Integer;
begin
  parent := 'n' + IntToStr(PtrUInt(X));
  for j := 0 to X^.N do
    if X^.C[j] <> nil then
    begin
      child := 'n' + IntToStr(PtrUInt(X^.C[j]));
      Writeln(F, '    ', parent, ' -> ', child, ';');
    end;
end;

procedure TraverseEmit(var F: TextFile; const X: PBNode);
var j: Integer;
begin
  if X = nil then Exit;
  EmitNode(F, X);
  EmitEdges(F, X);
  for j := 0 to X^.N do
    TraverseEmit(F, X^.C[j]);
end;

function ExportFavBTreeDOT(const Root: PBNode; const OutDir: string;
                           out DotPath: string): Boolean;
var
  F: TextFile; Path: String;
begin
  Result := False; DotPath := '';
  if OutDir = '' then Exit;
  if not DirectoryExists(OutDir) then
    if not ForceDirectories(OutDir) then Exit;

  Path := IncludeTrailingPathDelimiter(OutDir) + 'favoritos_btree.dot';
  AssignFile(F, Path);
  try
    Rewrite(F);
    Writeln(F, 'digraph "Correos Favoritos (Árbol B)" {');
    Writeln(F, '  rankdir=TB;');
    Writeln(F, '  graph [fontsize=12, labelloc="t"];');
    Writeln(F, '  node  [fontname="Helvetica", fontsize=10];');
    Writeln(F, '  edge  [color="#3a7f3a", arrowsize=0.7];');
    Writeln(F, '  label="Correos Favoritos (Árbol B)";');
    Writeln(F, '  subgraph cluster_fav {');
    Writeln(F, '    label="Árbol B (Orden 5) - Correos Favoritos"; color="#8ccc8c"; style="rounded";');

    if Root <> nil then TraverseEmit(F, Root);

    Writeln(F, '  }');
    Writeln(F, '}");
    CloseFile(F);
    DotPath := Path; Result := True;
  except
    {$I-} CloseFile(F); {$I+}
    Result := False; DotPath := '';
  end;
end;

end.

