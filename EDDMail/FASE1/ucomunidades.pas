unit UComunidades;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes;

type
  PUsuarioComunidad = ^TUsuarioComunidad;
  TUsuarioComunidad = record
    id: String;                 // neutral: cada quien convierte su tipo a String
    nombre: String;             // nombre o username visible
    siguiente: PUsuarioComunidad;
  end;

  PComunidad = ^TComunidad;
  TComunidad = record
    nombre: String;             // nombre de la comunidad
    usuarios: PUsuarioComunidad;// lista simple de usuarios de la comunidad
    siguiente: PComunidad;      // siguiente comunidad (lista simple)
  end;

var
  ComunidadesHead: PComunidad = nil;

procedure InitComunidades;
function  BuscarComunidad(const ANombre: String): PComunidad;
function  CrearComunidad(const ANombre: String): PComunidad; // idempotente
function  ExisteUsuarioEnComunidad(const C: PComunidad; const AId: String): Boolean;
function  AgregarUsuarioAComunidad(const C: PComunidad; const AId, ANombre: String): Boolean;
function  AgregarUsuarioAComunidadPorNombre(const ANombreComunidad, AId, ANombre: String): Boolean;
function  EliminarUsuarioDeComunidad(const C: PComunidad; const AId: String): Boolean;
procedure ExportarReporteComunidadesDOT(const CarpetaDestino: String; const NombreArchivo: String = 'Reporte_Comunidades.dot');
procedure LiberarComunidades;

implementation

procedure InitComunidades;
begin
  ComunidadesHead := nil;
end;

function BuscarComunidad(const ANombre: String): PComunidad;
var
  p: PComunidad;
begin
  p := ComunidadesHead;
  while p <> nil do
  begin
    if SameText(p^.nombre, ANombre) then exit(p);
    p := p^.siguiente;
  end;
  Result := nil;
end;

function CrearComunidad(const ANombre: String): PComunidad;
var
  nueva: PComunidad;
begin
  Result := BuscarComunidad(ANombre);
  if Result <> nil then Exit; // ya existe
  New(nueva);
  nueva^.nombre := ANombre;
  nueva^.usuarios := nil;
  nueva^.siguiente := ComunidadesHead;
  ComunidadesHead := nueva;
  Result := nueva;
end;

function ExisteUsuarioEnComunidad(const C: PComunidad; const AId: String): Boolean;
var
  u: PUsuarioComunidad;
begin
  Result := False;
  if C = nil then Exit;
  u := C^.usuarios;
  while u <> nil do
  begin
    if SameText(u^.id, AId) then exit(True);
    u := u^.siguiente;
  end;
end;

function AgregarUsuarioAComunidad(const C: PComunidad; const AId, ANombre: String): Boolean;
var
  nuevo: PUsuarioComunidad;
begin
  Result := False;
  if (C = nil) or (AId = '') then Exit;
  if ExisteUsuarioEnComunidad(C, AId) then Exit; // no duplicar
  New(nuevo);
  nuevo^.id := AId;
  nuevo^.nombre := ANombre;
  nuevo^.siguiente := C^.usuarios;
  C^.usuarios := nuevo;
  Result := True;
end;

function AgregarUsuarioAComunidadPorNombre(const ANombreComunidad, AId, ANombre: String): Boolean;
var
  c: PComunidad;
begin
  c := BuscarComunidad(ANombreComunidad);
  if c = nil then c := CrearComunidad(ANombreComunidad);
  Result := AgregarUsuarioAComunidad(c, AId, ANombre);
end;

function EliminarUsuarioDeComunidad(const C: PComunidad; const AId: String): Boolean;
var
  ant, act: PUsuarioComunidad;
begin
  Result := False;
  if C = nil then Exit;
  ant := nil; act := C^.usuarios;
  while act <> nil do
  begin
    if SameText(act^.id, AId) then
    begin
      if ant = nil then C^.usuarios := act^.siguiente
      else ant^.siguiente := act^.siguiente;
      Dispose(act);
      Exit(True);
    end;
    ant := act;
    act := act^.siguiente;
  end;
end;

procedure ExportarReporteComunidadesDOT(const CarpetaDestino: String; const NombreArchivo: String);
var
  dot: TStringList;
  c: PComunidad;
  u: PUsuarioComunidad;
  rutaDot, prevComId, thisComId, firstUserId, prevUserId: String;
  idxC, idxU: Integer;
  communitiesRankLine: String;

  function Esc(const s: String): String;
  begin
    Result := StringReplace(s, '"', '\"', [rfReplaceAll]);
  end;

  procedure EnsureDir(const Dir: String);
  begin
    if (Dir <> '') and (not DirectoryExists(Dir)) then
      ForceDirectories(Dir);
  end;

begin
  dot := TStringList.Create;
  try
    dot.Add('digraph "Reporte de Comunidades" {');
    dot.Add('  rankdir=LR;');
    dot.Add('  labelloc="t";');
    dot.Add('  label="Reporte de Comunidades";');
    dot.Add('  node  [fontname="Helvetica"];');
    dot.Add('  edge  [arrowsize=0.8];');

    // estilos sugeridos (como en tu mockup)
    dot.Add('  // Estilos');
    dot.Add('  subgraph cluster_legend { style=invis; }');
    dot.Add('  // Comunidad: caja azul');
    dot.Add('  // Usuario: caja amarilla');

    idxC := 0;
    prevComId := '';
    communitiesRankLine := '  { rank=same; ';

    c := ComunidadesHead;
    while c <> nil do
    begin
      Inc(idxC);
      thisComId := Format('c%d', [idxC]);

      // Nodo de comunidad (fila superior)
      dot.Add(Format('  %s [shape=box, style="rounded,filled", fillcolor="#a9d1e6", color="#2c3e50", ' +
                     'label=< <b>%s</b> >, width=2.8, height=0.9];',
                     [thisComId, Esc(c^.nombre)]));

      // Conexión comunidad anterior -> comunidad actual (lista simple)
      if prevComId <> '' then
        dot.Add(Format('  %s -> %s;', [prevComId, thisComId]));
      prevComId := thisComId;

      // Mantener en la misma fila
      communitiesRankLine := communitiesRankLine + thisComId + '; ';

      // Usuarios debajo (columna vertical)
      u := c^.usuarios;
      idxU := 0;
      firstUserId := '';
      prevUserId := '';

      while u <> nil do
      begin
        Inc(idxU);
        // id único por comunidad/usuario
        // nota: usamos índice para evitar choques por emails con caracteres especiales
        dot.Add(Format('  %s_u%d [shape=box, style="rounded,filled", fillcolor="#f6f4c0", color="#7f8c8d", ' +
                       'label=< %s >, width=3.3, height=0.8];',
                       [thisComId, idxU, Esc(u^.id)]));

        if idxU = 1 then
        begin
          firstUserId := Format('%s_u%d', [thisComId, idxU]);
          // Flecha comunidad -> primer usuario (vertical)
          dot.Add(Format('  %s -> %s;', [thisComId, firstUserId]));
        end
        else
        begin
          // Flechas verticales usuarioN-1 -> usuarioN
          dot.Add(Format('  %s -> %s;', [prevUserId, Format('%s_u%d', [thisComId, idxU])]));
        end;

        prevUserId := Format('%s_u%d', [thisComId, idxU]);
        u := u^.siguiente;
      end;

      // Restringir los usuarios de una comunidad a una misma columna (misma x)
      // con subgraph para fijar mismo rank vertical relativo
      if idxU > 0 then
      begin
        dot.Add('  {');
        dot.Add('    rank=same; ' + thisComId + ';');
        // también podemos añadir constraints leves si hiciera falta
        dot.Add('  }');
      end;

      c := c^.siguiente;
    end;

    communitiesRankLine := communitiesRankLine + ' }';
    dot.Add(communitiesRankLine); // todas las comunidades en la misma fila

    dot.Add('}');

    EnsureDir(CarpetaDestino);
    rutaDot := IncludeTrailingPathDelimiter(CarpetaDestino) + NombreArchivo;
    dot.SaveToFile(rutaDot);
  finally
    dot.Free;
  end;
end;



procedure LiberarComunidades;
var
  c, cnext: PComunidad;
  u, unext: PUsuarioComunidad;
begin
  c := ComunidadesHead;
  while c <> nil do
  begin
    u := c^.usuarios;
    while u <> nil do
    begin
      unext := u^.siguiente;
      Dispose(u);
      u := unext;
    end;
    cnext := c^.siguiente;
    Dispose(c);
    c := cnext;
  end;
  ComunidadesHead := nil;
end;

end.
