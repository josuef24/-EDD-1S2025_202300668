unit UComunidadesAdapters;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Process, UComunidades;

function AddUserToCommunity_IntId(const Comunidad: String; const UserIdInt: Integer; const UserName: String): Boolean;
function AddUserToCommunity_StrId(const Comunidad, UserIdStr, UserName: String): Boolean;

// Intentar generar PNG desde .dot con Graphviz (opcional)
function TryGeneratePNGFromDOT(const RutaDOT, RutaPNG: String): Boolean;

implementation

function AddUserToCommunity_IntId(const Comunidad: String; const UserIdInt: Integer; const UserName: String): Boolean;
begin
  //  convertit el id entero a string y delego al método principal
  // Internamente, UComunidades maneja punteros (PComunidad/PUsuarioComunidad) al insertar
  Result := AgregarUsuarioAComunidadPorNombre(Comunidad, IntToStr(UserIdInt), UserName);
end;

function AddUserToCommunity_StrId(const Comunidad, UserIdStr, UserName: String): Boolean;
begin
  //  Versión directa con id en string (neutral). Mantengo una sola ruta de inserción
  //  UComunidades hace la gestión de memoria/enlaces de la lista de listas
  Result := AgregarUsuarioAComunidadPorNombre(Comunidad, UserIdStr, UserName);
end;

function TryGeneratePNGFromDOT(const RutaDOT, RutaPNG: String): Boolean;
var
  AProcess: TProcess; //  Proceso externo para llamar a 'dot' de Graphviz
begin
  Result := False;
  //  Si no existe el .dot, no intento nada
  if not FileExists(RutaDOT) then Exit;

  //  Armo el proceso: dot -Tpng <in.dot> -o <out.png>
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'dot'; //  Requiere Graphviz en PATH
    AProcess.Parameters.Add('-Tpng');
    AProcess.Parameters.Add(RutaDOT);
    AProcess.Parameters.Add('-o');
    AProcess.Parameters.Add(RutaPNG);
    AProcess.Options := [poWaitOnExit]; //  Espero a que termine para verificar el archivo

    try
      AProcess.Execute;
      //  Considero éxito si efectivamente se generó el PNG
      Result := FileExists(RutaPNG);
    except
      //  Si dot falla (no instalado, ruta inválida, permisos), retorno False
      Result := False;
    end;
  finally
    //  Libero el proceso siempre
    AProcess.Free;
  end;
end;

end.

