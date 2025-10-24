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
  Result := AgregarUsuarioAComunidadPorNombre(Comunidad, IntToStr(UserIdInt), UserName);
end;

function AddUserToCommunity_StrId(const Comunidad, UserIdStr, UserName: String): Boolean;
begin
  Result := AgregarUsuarioAComunidadPorNombre(Comunidad, UserIdStr, UserName);
end;

function TryGeneratePNGFromDOT(const RutaDOT, RutaPNG: String): Boolean;
var
  AProcess: TProcess;
begin
  Result := False;
  if not FileExists(RutaDOT) then Exit;
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'dot'; // requiere Graphviz instalado
    AProcess.Parameters.Add('-Tpng');
    AProcess.Parameters.Add(RutaDOT);
    AProcess.Parameters.Add('-o');
    AProcess.Parameters.Add(RutaPNG);
    AProcess.Options := [poWaitOnExit];
    try
      AProcess.Execute;
      Result := FileExists(RutaPNG);
    except
      Result := False;
    end;
  finally
    AProcess.Free;
  end;
end;

end.
