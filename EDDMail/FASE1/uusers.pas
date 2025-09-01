unit uUsers;

{$mode objfpc}{$H+}

interface
uses uInbox, uTrash, uQueue, uContacts;

function LoadUsersFromJSON(const FileName: string;
                           out Imported, Duplicated, Errors: Integer): Boolean;
function ExistsId(const AId: Integer): Boolean;
function AddUserWithId(const AId: Integer; const AName, AUsername, AEmail, APhone, APass: AnsiString;
                       const ARoot: Boolean): Integer;
function ExportUsersDot(const DirPath: string): Boolean;
function ExportContactsDOTForUser(const L: TContactList;
                                  const OwnerEmail, BaseDir: string;
                                  out DotPath: string): Boolean;



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
    Inbox:    TInbox;       // Bandeja del user
    Next:     PUser;        // Siguiente
    Trash: TTrash;
    Contacts: TContactList;
    Sched: TSchedQueue;      //Cola de correos programados
  end;

var
  UsersHead: PUser = nil;
  NextId: Integer = 0;
  CurrentUser: PUser = nil;

procedure InitUsers;

// Inserta usuario y devuelve el ID asignado
function AddUser(const AName, AUsername, AEmail, APhone, APass: AnsiString;
                 const ARoot: Boolean): Integer;

function  FindUserByEmailOrUsername(const Key: AnsiString): PUser;
function  ExistsEmailOrUsername(const Key: AnsiString): Boolean;

// Valida login por email **o** usuario + password
function  ValidateUser(const Key, APass: AnsiString; out OutIsRoot: Boolean): Boolean;

implementation

uses SysUtils, fpjson, jsonparser, Classes, uMatrix;

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

  InitInbox(NewNode^.Inbox);
  InitTrash(NewNode^.Trash);
  InitQueue(NewNode^.Sched);
  InitContacts(NewNode^.Contacts);

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

  InitInbox(NewNode^.Inbox);
  InitTrash(NewNode^.Trash);
  InitQueue(NewNode^.Sched);
  InitContacts(NewNode^.Contacts);

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
    CurrentUser := U;
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

  // inicializar la matriz global de relaciones
  InitRel;

  // Root obligatorio (ID=0)
  AddUserWithId(0, 'root', 'root', 'root@edd.com', '', 'root123', True);
  NextID := 1;
end;

function ExportUsersDOT(const DirPath: string): Boolean;
var
  F: TextFile;
  OutPath: string;
  List: array of PUser;
  Count, i: Integer;
  U: PUser;
begin
  Result := False;

  if DirPath = '' then Exit;
  if not DirectoryExists(DirPath) then
    if not ForceDirectories(DirPath) then Exit;

  OutPath := IncludeTrailingPathDelimiter(DirPath) + 'usuarios.dot';
  AssignFile(F, OutPath);
  try
    Rewrite(F);

    // Encabezado DOT con estilo parecido al del enunciado
    Writeln(F, 'digraph "Reporte de Usuarios" {');
    Writeln(F, '  rankdir=LR;');
    Writeln(F, '  graph [fontsize=12, labelloc="t"];');
    Writeln(F, '  node  [shape=box, style="rounded,filled", color="#2c3e50", fillcolor="#cfe9f3", fontname="Helvetica", fontsize=10];');
    Writeln(F, '  edge  [color="#2c3e50", arrowsize=0.6];');
    Writeln(F, '  label="Reporte de Usuarios";');

    // Cajita contenedora (cluster) para dar marco como en el PDF
    Writeln(F, '  subgraph cluster_usuarios {');
    Writeln(F, '    label="Lista Enlazada";');
    Writeln(F, '    style="rounded";');
    Writeln(F, '    color="#7f8c8d";');

    // 1) Recorremos la lista enlazada y guardamos punteros en un array
    Count := 0;
    U := UsersHead;
    while U <> nil do
    begin
      Inc(Count);
      SetLength(List, Count);
      List[Count-1] := U;
      U := U^.Next;
    end;

    // 2) Emitimos los nodos en orden 0..N (invirtiendo el array)
    for i := Count-1 downto 0 do
    begin
      U := List[i];
      Writeln(F, '    u', U^.Id, ' [label=<',
                '<b>ID: ', U^.Id, '</b><br/>',
                'Nombre: ', U^.Name, '<br/>',
                'Usuario: ', U^.Username, '<br/>',
                'Email: ', U^.Email, '<br/>',
                'Teléfono: ', U^.Phone,
                '>];');

      // Flecha al siguiente (de menor a mayor ID)
      if i > 0 then
        Writeln(F, '    u', U^.Id, ' -> u', List[i-1]^.Id, ';');
    end;

    Writeln(F, '  }'); // fin cluster
    Writeln(F, '}');   // fin digraph

    CloseFile(F);
    Result := True;
  except
    on E: Exception do
    begin
      {$I-} CloseFile(F); {$I+}
      Result := False;
    end;
  end;
end;

function ExportContactsDOTForUser(const L: TContactList;
  const OwnerEmail, BaseDir: string; out DotPath: string): Boolean;
var
  F: TextFile;
  First, C: PContact;
  Path: string;
  idx, i: Integer;
begin
  Result  := False;
  DotPath := '';

  if (BaseDir = '') then Exit;
  if not DirectoryExists(BaseDir) then
    if not ForceDirectories(BaseDir) then Exit;

  Path := IncludeTrailingPathDelimiter(BaseDir) + 'contactos_' + OwnerEmail + '.dot';
  AssignFile(F, Path);
  try
    Rewrite(F);
    Writeln(F, 'digraph "Reporte de Contactos" {');
    Writeln(F, '  rankdir=LR;');
    Writeln(F, '  labelloc="t";');
    Writeln(F, '  label="Reporte de Contactos";');
    Writeln(F, '  node [shape=box, style="rounded,filled", fillcolor="#cfe9f7"];');

    // contenedor (como en otros reportes)
    Writeln(F, '  subgraph cluster_contacts {');
    Writeln(F, '    label="Lista Circular"; color="#bbbbbb";');

    if L.Tail <> nil then
    begin
      First := L.Tail^.Next;  // cabeza
      C     := First;
      idx   := 1;

      // nodos
      repeat
        Writeln(F, '    n', idx, ' [label=<',
          '<b>ID:</b> ', idx, '<br/>',
          '<b>Nombre:</b> ', C^.Name, '<br/>',
          '<b>Usuario:</b> ', C^.Username, '<br/>',
          '<b>Email:</b> ', C^.Email, '<br/>',
          '<b>Teléfono:</b> ', C^.Phone,
          '>, width=3];');
        C := C^.Next;
        Inc(idx);
      until C = First;

      // aristas doble flecha para mostrar circularidad
      if idx > 2 then
      begin
        for i := 1 to idx-2 do
          Writeln(F, '    n', i, ' -> n', i+1, ' [dir=both, arrowsize=0.7];');
        // último <-> primero
        Writeln(F, '    n', idx-1, ' -> n1 [dir=both, arrowsize=0.7];');
      end;
      // si solo hay 1, no dibujamos aristas
    end;

    Writeln(F, '  }'); // cluster
    Writeln(F, '}');
    CloseFile(F);

    DotPath := Path;
    Result  := True;
  except
    on E: Exception do
    begin
      {$I-} CloseFile(F); {$I+}
      Result := False;
    end;
  end;
end;



end.


