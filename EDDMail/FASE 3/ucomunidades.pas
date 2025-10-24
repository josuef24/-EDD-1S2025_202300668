unit UComunidades;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes;

type
  PUsuarioComunidad = ^TUsuarioComunidad; // [APUNTADOR] Puntero a nodo de usuario dentro de una comunidad
  TUsuarioComunidad = record
    id: String;                 //  id neutral en String (yo convierto desde int/username/email según el caso)
    nombre: String;             //  nombre visible o username
    siguiente: PUsuarioComunidad; // [APUNTADOR] enlace al siguiente usuario en la lista simple
  end;

  PComunidad = ^TComunidad; // [APUNTADOR] Puntero a nodo de comunidad
  TComunidad = record
    nombre: String;             //  nombre de la comunidad
    usuarios: PUsuarioComunidad;// [APUNTADOR] cabeza de lista simple de usuarios
    siguiente: PComunidad;      // [APUNTADOR] siguiente comunidad en la lista simple
  end;

var
  ComunidadesHead: PComunidad = nil; // [APUNTADOR] cabeza de la lista de comunidades

procedure InitComunidades;
function  BuscarComunidad(const ANombre: String): PComunidad;
function  CrearComunidad(const ANombre: String): PComunidad;
function  ExisteUsuarioEnComunidad(const C: PComunidad; const AId: String): Boolean;
function  AgregarUsuarioAComunidad(const C: PComunidad; const AId, ANombre: String): Boolean;
function  AgregarUsuarioAComunidadPorNombre(const ANombreComunidad, AId, ANombre: String): Boolean;
function  EliminarUsuarioDeComunidad(const C: PComunidad; const AId: String): Boolean;
procedure ExportarReporteComunidadesDOT(const CarpetaDestino: String; const NombreArchivo: String = 'Reporte_Comunidades.dot');
procedure LiberarComunidades;

implementation

procedure InitComunidades;
begin

  ComunidadesHead := nil; // [APUNTADOR] nil indica que no hay nodos
end;

function BuscarComunidad(const ANombre: String): PComunidad;
var
  p: PComunidad; // [APUNTADOR] cursor para recorrer la lista simple
begin
  // [BUSQUEDA] Camino desde la cabeza comparando por nombre sin distinguir mayúsculas
  p := ComunidadesHead;
  while p <> nil do
  begin
    if SameText(p^.nombre, ANombre) then exit(p); // [APUNTADOR] accedo al campo con ^.
    p := p^.siguiente; // [APUNTADOR] avanzo al siguiente
  end;
  Result := nil; // no encontrada
end;

function CrearComunidad(const ANombre: String): PComunidad;
var
  nueva: PComunidad; // [APUNTADOR] nodo nuevo a reservar
begin
  //  Si ya existe, devuelvo la existente y no creo otra
  Result := BuscarComunidad(ANombre);
  if Result <> nil then Exit; // ya existe

  //  Reserva y alta al inicio de la lista (O(1))
  New(nueva);                 // [APUNTADOR] reservo memoria para la comunidad
  nueva^.nombre := ANombre;
  nueva^.usuarios := nil;     // [APUNTADOR] lista de usuarios vacía
  nueva^.siguiente := ComunidadesHead; // enlazo con la cabeza actual
  ComunidadesHead := nueva;   // actualizo cabeza
  Result := nueva;
end;

function ExisteUsuarioEnComunidad(const C: PComunidad; const AId: String): Boolean;
var
  u: PUsuarioComunidad; // [APUNTADOR] cursor de usuarios de esa comunidad
begin
  //  Si la comunidad es nil, no hay nada que buscar
  Result := False;
  if C = nil then Exit;

  //  Recorro la lista simple de usuarios comparando por id
  u := C^.usuarios;
  while u <> nil do
  begin
    if SameText(u^.id, AId) then exit(True);
    u := u^.siguiente;
  end;
end;

function AgregarUsuarioAComunidad(const C: PComunidad; const AId, ANombre: String): Boolean;
var
  nuevo: PUsuarioComunidad; // [APUNTADOR] nodo a insertar
begin
  Result := False;
  //  Comunidad válida y AId no vacío
  if (C = nil) or (AId = '') then Exit;
  //  Evito duplicados por id dentro de la comunidad
  if ExisteUsuarioEnComunidad(C, AId) then Exit;

  //  Inserto al inicio de la lista de usuarios de la comunidad (O(1))
  New(nuevo);                  // [APUNTADOR]
  nuevo^.id := AId;
  nuevo^.nombre := ANombre;
  nuevo^.siguiente := C^.usuarios; // [APUNTADOR] cuelgo del head actual
  C^.usuarios := nuevo;            // [APUNTADOR] actualizo head
  Result := True;
end;

function AgregarUsuarioAComunidadPorNombre(const ANombreComunidad, AId, ANombre: String): Boolean;
var
  c: PComunidad; // [APUNTADOR]
begin
  //  Busco o creo la comunidad por nombre y delego al insert genérico
  c := BuscarComunidad(ANombreComunidad);
  if c = nil then c := CrearComunidad(ANombreComunidad);
  Result := AgregarUsuarioAComunidad(c, AId, ANombre);
end;

function EliminarUsuarioDeComunidad(const C: PComunidad; const AId: String): Boolean;
var
  ant, act: PUsuarioComunidad; // [APUNTADORES] ant = previo, act = actual
begin
  Result := False;
  if C = nil then Exit;

  //  Lista simple: necesito trackear el anterior para puentear al eliminar
  ant := nil; act := C^.usuarios;
  while act <> nil do
  begin
    if SameText(act^.id, AId) then
    begin
      // [DELETE] Reenlazo saltando el nodo a eliminar
      if ant = nil then C^.usuarios := act^.siguiente
      else ant^.siguiente := act^.siguiente;
      Dispose(act); // [MEM] libero el nodo de usuario
      Exit(True);
    end;
    ant := act;
    act := act^.siguiente;
  end;
end;

procedure ExportarReporteComunidadesDOT(const CarpetaDestino: String; const NombreArchivo: String);
var
  dot: TStringList; //  buffer de salida
  c: PComunidad;    // [APUNTADOR] cursor de comunidades
  u: PUsuarioComunidad; // [APUNTADOR] cursor de usuarios
  rutaDot, prevComId, thisComId, firstUserId, prevUserId: String;
  idxC, idxU: Integer;
  communitiesRankLine: String;

  function Esc(const s: String): String;
  begin

    Result := StringReplace(s, '"', '\"', [rfReplaceAll]);
  end;

  procedure EnsureDir(const Dir: String);
  begin
    //  Creo carpeta si no existe
    if (Dir <> '') and (not DirectoryExists(Dir)) then
      ForceDirectories(Dir);
  end;

begin
  //  Construyo el grafo con comunidades en fila y sus usuarios en columnas verticales
  dot := TStringList.Create;
  try
    dot.Add('digraph "Reporte de Comunidades" {');
    dot.Add('  rankdir=LR;');
    dot.Add('  labelloc="t";');
    dot.Add('  label="Reporte de Comunidades";');
    dot.Add('  node  [fontname="Helvetica"];');
    dot.Add('  edge  [arrowsize=0.8];');

    dot.Add('  // Estilos');
    dot.Add('  subgraph cluster_legend { style=invis; }');
    dot.Add('  // Comunidad: caja azul');
    dot.Add('  // Usuario: caja amarilla');

    idxC := 0;
    prevComId := '';
    communitiesRankLine := '  { rank=same; ';

    //  Camino la lista simple de comunidades
    c := ComunidadesHead;
    while c <> nil do
    begin
      Inc(idxC);
      thisComId := Format('c%d', [idxC]);

      // Nodo de comunidad
      dot.Add(Format('  %s [shape=box, style="rounded,filled", fillcolor="#a9d1e6", color="#2c3e50", ' +
                     'label=< <b>%s</b> >, width=2.8, height=0.9];',
                     [thisComId, Esc(c^.nombre)]));

      // Lista simple de comunidades: anterior -> actual
      if prevComId <> '' then
        dot.Add(Format('  %s -> %s;', [prevComId, thisComId]));
      prevComId := thisComId;

      //  Mantengo todas las comunidades en la misma fila
      communitiesRankLine := communitiesRankLine + thisComId + '; ';

      //  Debajo, la lista vertical de usuarios
      u := c^.usuarios;
      idxU := 0;
      firstUserId := '';
      prevUserId := '';

      while u <> nil do
      begin
        Inc(idxU);
        //  Uso índice local para evitar choques por caracteres especiales
        dot.Add(Format('  %s_u%d [shape=box, style="rounded,filled", fillcolor="#f6f4c0", color="#7f8c8d", ' +
                       'label=< %s >, width=3.3, height=0.8];',
                       [thisComId, idxU, Esc(u^.id)]));

        if idxU = 1 then
        begin
          firstUserId := Format('%s_u%d', [thisComId, idxU]);
          //  comunidad -> primer usuario
          dot.Add(Format('  %s -> %s;', [thisComId, firstUserId]));
        end
        else
        begin
          //  usuario N-1 -> usuario N
          dot.Add(Format('  %s -> %s;', [prevUserId, Format('%s_u%d', [thisComId, idxU])]));
        end;

        prevUserId := Format('%s_u%d', [thisComId, idxU]);
        u := u^.siguiente; // [APUNTADOR] avanzo en la lista de usuarios
      end;

      // Subgraph de alineación suave para mantener verticalidad por comunidad
      if idxU > 0 then
      begin
        dot.Add('  {');
        dot.Add('    rank=same; ' + thisComId + ';');
        dot.Add('  }');
      end;

      c := c^.siguiente; // [APUNTADOR] siguiente comunidad
    end;

    // Cierro la línea de comunidades en la misma fila
    communitiesRankLine := communitiesRankLine + ' }';
    dot.Add(communitiesRankLine);

    dot.Add('}');

    // Guardo el DOT en la carpeta solicitada
    EnsureDir(CarpetaDestino);
    rutaDot := IncludeTrailingPathDelimiter(CarpetaDestino) + NombreArchivo;
    dot.SaveToFile(rutaDot);
  finally
    dot.Free; // libero el buffer
  end;
end;

procedure LiberarComunidades;
var
  c, cnext: PComunidad;         // [APUNTADORES] cursor de comunidad y su siguiente
  u, unext: PUsuarioComunidad;  // [APUNTADORES] cursor de usuario y su siguiente
begin
  // Recorro toda la estructura y libero memoria nodo por nodo
  c := ComunidadesHead;
  while c <> nil do
  begin
    // Primero libero toda la lista de usuarios de esta comunidad
    u := c^.usuarios;
    while u <> nil do
    begin
      unext := u^.siguiente; // [APUNTADOR] guardo siguiente antes de liberar
      Dispose(u);            // libero usuario
      u := unext;            // [APUNTADOR] avanzo
    end;

    // Ahora libero el nodo de comunidad y paso a la siguiente
    cnext := c^.siguiente; // [APUNTADOR]
    Dispose(c);            // libero comunidad
    c := cnext;            // [APUNTADOR] avanzo
  end;

  // Dejo la cabeza en nil para indicar que quedó vacía
  ComunidadesHead := nil;
end;

end.

