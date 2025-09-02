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
  rutaDot: String;

  function Esc(const s: String): String;
  begin
    Result := StringReplace(s, '"', '\"', [rfReplaceAll]);
  end;

begin
  dot := TStringList.Create;
  try
    dot.Add('digraph Comunidades {');
    dot.Add('  rankdir=LR;');
    dot.Add('  node [shape=box, fontname="Helvetica"];');
    dot.Add('  graph [label="Reporte de Comunidades", labelloc=top, fontsize=20];');

    c := ComunidadesHead;
    while c <> nil do
    begin
      dot.Add(Format('  "%s" [shape=folder, style=filled, fillcolor="#E6F0FF"];',
        [Esc('Comunidad: ' + c^.nombre)]));
      u := c^.usuarios;
      while u <> nil do
      begin
        dot.Add(Format('  "%s" [shape=note];', [Esc(u^.id + ' - ' + u^.nombre)]));
        dot.Add(Format('  "%s" -> "%s";',
          [Esc('Comunidad: ' + c^.nombre), Esc(u^.id + ' - ' + u^.nombre)]));
        u := u^.siguiente;
      end;
      c := c^.siguiente;
    end;

    dot.Add('}');

    if (CarpetaDestino <> '') and (not DirectoryExists(CarpetaDestino)) then
      CreateDir(CarpetaDestino);

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
