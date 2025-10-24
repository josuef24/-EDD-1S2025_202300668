unit UMailLoader_JSON;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, UDataMail;

type
  // Callback: se llama una vez por cada correo del JSON
  TOnCorreo = procedure(const C: TCorreo) of object;

function LoadCorreosJSON(const FileName: string; OnCorreo: TOnCorreo; out Count: Integer): Boolean;

implementation

function GetStrSafe(Obj: TJSONObject; const Key: string; const Def: string = ''): string;
var D: TJSONData;
begin
  D := Obj.Find(Key);
  if (D <> nil) and (D.JSONType in [jtString, jtNumber, jtBoolean]) then
    Result := D.AsString
  else
    Result := Def;
end;

function GetIntSafe(Obj: TJSONObject; const Key: string; const Def: LongInt = 0): LongInt;
var D: TJSONData;
begin
  D := Obj.Find(Key);
  if (D <> nil) and (D.JSONType in [jtNumber, jtString]) then
    Result := StrToIntDef(D.AsString, Def)
  else
    Result := Def;
end;

function LoadCorreosJSON(const FileName: string; OnCorreo: TOnCorreo; out Count: Integer): Boolean;
var
  S      : TStringList;
  Root   : TJSONData;
  Obj    : TJSONObject;
  Arr    : TJSONArray;
  I      : Integer;
  C      : TCorreo;
begin
  Result := False; Count := 0;
  if (not FileExists(FileName)) then Exit;

  S := TStringList.Create;
  try
    S.LoadFromFile(FileName); // asume UTF-8 (Lazarus por defecto en Linux)
    Root := GetJSON(S.Text);
    try
      if (Root = nil) or (Root.JSONType <> jtObject) then Exit;
      Obj := TJSONObject(Root);

      if not Obj.Find('correos', Arr) then Exit;
      if (Arr = nil) or (Arr.JSONType <> jtArray) then Exit;

      for I := 0 to Arr.Count - 1 do
      begin
        if Arr.Items[I].JSONType <> jtObject then Continue;
        with TJSONObject(Arr.Items[I]) do
        begin
          C.ID           := GetIntSafe(TJSONObject(Arr.Items[I]), 'id', 0);
          C.Remitente    := GetStrSafe(TJSONObject(Arr.Items[I]), 'remitente', '');
          C.Destinatario := GetStrSafe(TJSONObject(Arr.Items[I]), 'destinatario', '');
          C.Estado       := GetStrSafe(TJSONObject(Arr.Items[I]), 'estado', 'NL');
          C.Asunto       := GetStrSafe(TJSONObject(Arr.Items[I]), 'asunto', '');
          C.Mensaje      := GetStrSafe(TJSONObject(Arr.Items[I]), 'mensaje', '');
        end;

        Inc(Count);
        if Assigned(OnCorreo) then OnCorreo(C);
      end;

      Result := True;
    finally
      Root.Free;
    end;
  finally
    S.Free;
  end;
end;

end.

