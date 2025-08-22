unit uUsers;

{$mode objfpc}{$H+}

interface

function LoadUsersFromJSON(const FileName: string;
                           out Imported, Duplicated, Errors: Integer): Boolean;
function ExistsId(const AId: Integer): Boolean;
function AddUserWithId(const AId: Integer; const AName, AUsername, AEmail, APhone, APass: AnsiString;
                       const ARoot: Boolean): Integer;


type
  PUser = ^TUserNode;
  TUserNode = record
    Id:       Integer;      // autoincremental
    Name:     AnsiString;   // Nombre
    Username: AnsiString;   // Usuario
    Email:    AnsiString;   // Email
    Phone:    AnsiString;   // Teléfono
    Password: AnsiString;   // Contraseña
    IsRoot:   Boolean;      // Root?
    Next:     PUser;        // Siguiente
  end;

var
  UsersHead: PUser = nil;
  NextId: Integer = 0;

procedure InitUsers;

// Inserta usuario y devuelve el ID asignado
function AddUser(const AName, AUsername, AEmail, APhone, APass: AnsiString;
                 const ARoot: Boolean): Integer;

function  FindUserByEmailOrUsername(const Key: AnsiString): PUser;
function  ExistsEmailOrUsername(const Key: AnsiString): Boolean;

// Valida login por email **o** usuario + password
function  ValidateUser(const Key, APass: AnsiString; out OutIsRoot: Boolean): Boolean;

implementation

uses SysUtils, fpjson, jsonparser, Classes;

function ExistsId(const AId: Integer): Boolean;
var
  C: PUser;
begin
  C := UsersHead;
  while C <> nil do
  begin
    if C^.Id = AId then Exit(True);
    C := C^.Next;
  end;
  Result := False;
end;

function AddUserWithId(const AId: Integer; const AName, AUsername, AEmail, APhone, APass: AnsiString;
                       const ARoot: Boolean): Integer;
var
  NewNode: PUser;
begin
  New(NewNode);
  NewNode^.Id       := AId;
  NewNode^.Name     := AName;
  NewNode^.Username := AUsername;
  NewNode^.Email    := AEmail;
  NewNode^.Phone    := APhone;
  NewNode^.Password := APass;
  NewNode^.IsRoot   := ARoot;

  NewNode^.Next := UsersHead;
  UsersHead := NewNode;

  // Mantén NextId preparado para el siguiente alta automática
  if AId >= NextId then
    NextId := AId + 1;

  Result := AId;
end;

function LoadUsersFromJSON(const FileName: string;
                           out Imported, Duplicated, Errors: Integer): Boolean;
var
  FS: TFileStream;
  Parser: TJSONParser;
  Root: TJSONData;
  Arr: TJSONArray;
  i, jid: Integer;
  Obj: TJSONObject;
  nombre, usuario, email, telefono, pass: AnsiString;
  maxIdSeen: Integer;
begin
  Imported := 0; Duplicated := 0; Errors := 0;
  Result := False;
  maxIdSeen := -1;

  if not FileExists(FileName) then Exit(False);

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Parser := TJSONParser.Create(FS);
    try
      Root := Parser.Parse;
      try
        Arr := Root.FindPath('usuarios') as TJSONArray;
        if Arr = nil then Exit(False);

        for i := 0 to Arr.Count - 1 do
        begin
          if Arr.Items[i].JSONType <> jtObject then
          begin
            Inc(Errors);
            Continue;
          end;

          Obj      := TJSONObject(Arr.Items[i]);

          // lee todos los campos del JSON
          jid      := Obj.Get('id', -1);
          nombre   := Obj.Get('nombre',   '');
          usuario  := Obj.Get('usuario',  '');
          email    := Obj.Get('email',    '');
          telefono := Obj.Get('telefono', '');
          pass     := Obj.Get('password', Obj.Get('contraseña', ''));

          // validaciones mínimas
          if (jid < 0) or (nombre='') or (usuario='') or (email='') or (pass='') then
          begin
            Inc(Errors);
            Continue;
          end;

          // evita colisiones:
          //  - ID ya existente (incluye el root=0)
          //  - email o usuario ya existentes
          if ExistsId(jid) or ExistsEmailOrUsername(email) or ExistsEmailOrUsername(usuario) then
          begin
            Inc(Duplicated);
            Continue;
          end;

          // inserta usando el id del JSON (NO rompemos el autoincremento)
          AddUserWithId(jid, nombre, usuario, email, telefono, pass, False);
          if jid > maxIdSeen then maxIdSeen := jid;

          Inc(Imported);
        end;

        // asegura que NextId quede después del mayor ID importado
        if maxIdSeen >= NextId then
          NextId := maxIdSeen + 1;

        Result := True;
      finally
        Root.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    FS.Free;
  end;
end;



function AddUser(const AName, AUsername, AEmail, APhone, APass: AnsiString;
                 const ARoot: Boolean): Integer;
var
  NewNode: PUser;
begin
  New(NewNode);
  NewNode^.Id       := NextId;
  NewNode^.Name     := AName;
  NewNode^.Username := AUsername;
  NewNode^.Email    := AEmail;
  NewNode^.Phone    := APhone;
  NewNode^.Password := APass;
  NewNode^.IsRoot   := ARoot;

  // Inserción al inicio (O(1))
  NewNode^.Next := UsersHead;
  UsersHead := NewNode;

  Result := NextId;
  Inc(NextId);
end;

function FindUserByEmailOrUsername(const Key: AnsiString): PUser;
var
  Curr: PUser;
begin
  Curr := UsersHead;
  while Curr <> nil do
  begin
    if (AnsiCompareText(Curr^.Email, Key) = 0) or
       (AnsiCompareText(Curr^.Username, Key) = 0) then
      Exit(Curr);
    Curr := Curr^.Next;
  end;
  Result := nil;
end;

function ExistsEmailOrUsername(const Key: AnsiString): Boolean;
begin
  Result := FindUserByEmailOrUsername(Key) <> nil;
end;

function ValidateUser(const Key, APass: AnsiString; out OutIsRoot: Boolean): Boolean;
var
  U: PUser;
begin
  U := FindUserByEmailOrUsername(Key);
  if (U <> nil) and (U^.Password = APass) then
  begin
    OutIsRoot := U^.IsRoot;
    Exit(True);
  end;
  OutIsRoot := False;
  Result := False;
end;

procedure InitUsers;
begin
  UsersHead := nil;
  NextId := 0;
  // Root obligatorio (ID=0)
  AddUser('root', 'root', 'root@edd.com', '', 'root123', True);
  // Demo (ID=1)
  AddUser('Usuario Demo', 'user', 'user@edd.com', '555-0000', 'user123', False);
end;

end.


