unit uDraftsReport_AVL;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, UAVL_Borradores;

function ExportDraftsAVL_DOT(const Root: PAVLNode; const OutDir: string;
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

procedure EmitNode(var F: TextFile; const N: PAVLNode);
var name: String; D: TMailDraft;
begin
  D := N^.Data;
  name := 'n' + IntToStr(PtrUInt(N));
  Writeln(F, '    ', name, ' [label=<',
    '<b>ID: </b>', D.ID, '<br/>',
    '<b>Remitente:</b> ', EscapeHTML(D.Remitente), '<br/>',
    '<b>Estado:</b> &mdash;<br/>',
    '<b>Asunto:</b> ', EscapeHTML(D.Asunto), '<br/>',
    '<b>Fecha:</b> &mdash;<br/>',
    '<b>Mensaje:</b> ', EscapeHTML(D.Mensaje),
    '>, width=3];');
end;

procedure EmitEdges(var F: TextFile; const N: PAVLNode);
var p,l,r: String;
begin
  p := 'n' + IntToStr(PtrUInt(N));
  if N^.Left  <> nil then begin l := 'n' + IntToStr(PtrUInt(N^.Left));  Writeln(F,'    ',p,' -> ',l,';'); end;
  if N^.Right <> nil then begin r := 'n' + IntToStr(PtrUInt(N^.Right)); Writeln(F,'    ',p,' -> ',r,';'); end;
end;

procedure TraverseEmit(var F: TextFile; const N: PAVLNode);
begin
  if N = nil then Exit;
  TraverseEmit(F, N^.Left);
  EmitNode(F, N);
  EmitEdges(F, N);
  TraverseEmit(F, N^.Right);
end;

function ExportDraftsAVL_DOT(const Root: PAVLNode; const OutDir: string;
                             out DotPath: string): Boolean;
var F: TextFile; Path: String;
begin
  Result := False; DotPath := '';
  if OutDir = '' then Exit;
  if not DirectoryExists(OutDir) then
    if not ForceDirectories(OutDir) then Exit;

  Path := IncludeTrailingPathDelimiter(OutDir) + 'borradores_avl.dot';
  AssignFile(F, Path);
  try
    Rewrite(F);
    Writeln(F, 'digraph "Reporte de Borradores de correos (Árbol AVL)" {');
    Writeln(F, '  rankdir=TB;');
    Writeln(F, '  graph [fontsize=12, labelloc="t"];');
    Writeln(F, '  node  [shape=box, style="rounded,filled", fillcolor="#fff7cc", color="#666666", fontname="Helvetica", fontsize=10];');
    Writeln(F, '  edge  [color="#666666", arrowsize=0.7];');
    Writeln(F, '  label="Reporte de Borradores de correos (Árbol AVL)";');
    Writeln(F, '  subgraph cluster_borradores {');
    Writeln(F, '    label="Árbol AVL - Correos"; color="#aaaaaa"; style="rounded";');

    if Root <> nil then TraverseEmit(F, Root);

    Writeln(F, '  }');
    Writeln(F, '}');
    CloseFile(F);
    DotPath := Path; Result := True;
  except
    {$I-} CloseFile(F); {$I+}
    Result := False; DotPath := '';
  end;
end;

end.

