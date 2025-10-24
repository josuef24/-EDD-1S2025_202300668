unit uUsers;

{$mode objfpc}{$H+}

interface
uses uInbox, uTrash, uQueue, uContacts;

// cargo usuarios desde un JSON. Devuelvo true/false y también conteos por referencia (importados/duplicados/errores).
function LoadUsersFromJSON(const FileName: string;
                           out Imported, Duplicated, Errors: Integer): Boolean;

// Verifico si ya existe un ID (me sirve para evitar choques cuando cargo desde archivo).
function ExistsId(const AId: Integer): Boolean;

// Agrego un usuario con ID explícito (lo uso en carga masiva). Devuelvo el ID asignado.
// Nota: aquí inicializo también sus estructuras internas (Inbox, Trash, Sched, Contacts).
function AddUserWithId(const AId: Integer; const AName, AUsername, AEmail, APhone, APass: AnsiString;
                       const ARoot: Boolean): Integer;

// Exporto el DOT de todos los usuarios para Graphviz.
function ExportUsersDot(const DirPath: string): Boolean;

// Exporto el DOT de contactos de un usuario puntual (lista circular) para Graphviz.
function ExportContactsDOTForUser(const L: TContactList;
                                  const OwnerEmail, BaseDir: string;
                                  out DotPath: string): Boolean;



type
  // APUNTADOR PUser es un PUNTERO a TUserNode. Siempre valido contra nil antes de usar U^.
  PUser = ^TUserNode;

  //  nodo de mi lista simple de usuarios (enlazo con Next).
  TUserNode = record
    Id:       Integer;
    Name:     AnsiString;
    Username: AnsiString;
    Email:    AnsiString;
    Phone:    AnsiString;
    Password: AnsiString;
    IsRoot:   Boolean;
    Inbox:    TInbox;       // Bandeja del usuario (estructura propia; no es puntero)
    Next:     PUser;        // APUNTADOR Siguiente nodo en la lista simple de usuarios
    Trash:    TTrash;       // Papelera del usuario (pila; no es puntero)
    Contacts: TContactList; // Lista circular de contactos (no es puntero)
    Sched:    TSchedQueue;  // Cola de correos programados (no es puntero)
  end;

var
  UsersHead: PUser = nil;    // Cabeza de la lista simple de usuarios (puntero o nil si vacía)
  NextId: Integer = 0;       // Siguiente ID disponible para altas automáticas
  CurrentUser: PUser = nil;  // Usuario actualmente logueado (puntero o nil si no hay sesión)

procedure InitUsers;

// Inserto usuario con ID autogenerado. Devuelvo el ID asignado.
function AddUser(const AName, AUsername, AEmail, APhone, APass: AnsiString;
                 const ARoot: Boolean): Integer;

//  Devuelvo puntero al usuario si encuentro por email o username; nil si no existe.
function  FindUserByEmailOrUsername(const Key: AnsiString): PUser;

//  True si ya existe un email o username igual (útil para validar altas).
function  ExistsEmailOrUsername(const Key: AnsiString): Boolean;

//  Valido por email O username + password. Devuelvo si es root por parámetro de salida.
function  ValidateUser(const Key, APass: AnsiString; out OutIsRoot: Boolean): Boolean;

implementation

uses SysUtils, fpjson, jsonparser, Classes, uMatrix;

function ExistsId(const AId: Integer): Boolean;
var
  C: PUser; // APUNTADOR Recorro con un puntero temporal
begin
  // Recorro la lista simple desde la cabeza
  C := UsersHead;
  while C <> nil do
  begin
    if C^.Id = AId then Exit(True); //  Accedo con C^.Id
    C := C^.Next;                   //  Avanzo al siguiente nodo
  end;
  Result := False;
end;

function AddUserWithId(const AId: Integer; const AName, AUsername, AEmail, APhone, APass: AnsiString;
                       const ARoot: Boolean): Integer;
var
  NewNode: PUser; // APUNTADOR Reservo un nuevo nodo usuario
begin
  // [MEM] Reservo memoria para el nodo y seteo todos sus campos
  New(NewNode);                 // APUNTADOR New asigna memoria para el registro y me da el puntero
  NewNode^.Id       := AId;     // APUNTADOR Desreferencio con ^ para asignar cada campo
  NewNode^.Name     := AName;
  NewNode^.Username := AUsername;
  NewNode^.Email    := AEmail;
  NewNode^.Phone    := APhone;
  NewNode^.Password := APass;
  NewNode^.IsRoot   := ARoot;

  // LISTA Inserto al inicio de la lista (O(1)); dejo NewNode como nueva cabeza
  NewNode^.Next := UsersHead;   // APUNTADOR Enlazo el siguiente del nuevo con la cabeza actual
  UsersHead := NewNode;         // APUNTADOR Actualizo la cabeza

  // Inicializo las estructuras internas del usuario recién creado
  InitInbox(NewNode^.Inbox);
  InitTrash(NewNode^.Trash);
  InitQueue(NewNode^.Sched);
  InitContacts(NewNode^.Contacts);

  // Mantengo NextId listo por si luego hago altas automáticas
  if AId >= NextId then
    NextId := AId + 1;

  Result := AId; // Devuelvo el ID que acabo de asignar
end;


function LoadUsersFromJSON(const FileName: string;
                           out Imported, Duplicated, Errors: Integer): Boolean;
var
  FS: TFileStream;      //  Stream del archivo JSON
  Parser: TJSONParser;  //  Parser JSON (fpjson/jsonparser)
  Root: TJSONData;      //  Raíz del JSON ya parseado
  Arr: TJSONArray;      //  Arreglo de usuarios: Root['usuarios']
  i, jid: Integer;      //  i para iterar; jid = id leído del JSON
  Obj: TJSONObject;     //  Objeto usuario actual
  nombre, usuario, email, telefono, pass: AnsiString; //  Campos del JSON
  maxIdSeen: Integer;   //  Llevo el mayor id visto para ajustar NextId
begin
  //  Inicializo conteos de importación
  Imported := 0; Duplicated := 0; Errors := 0;
  Result := False;
  maxIdSeen := -1;

  //  Si no existe el archivo, salgo en falso
  if not FileExists(FileName) then Exit(False);

  //  Abro el archivo en modo solo lectura
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    //  Creo el parser sobre el stream
    Parser := TJSONParser.Create(FS);
    try
      //  Parseo todo el JSON a memoria
      Root := Parser.Parse;
      try
        //  Busco el arreglo "usuarios"
        Arr := Root.FindPath('usuarios') as TJSONArray;
        if Arr = nil then Exit(False);

        //  Recorro cada item del arreglo
        for i := 0 to Arr.Count - 1 do
        begin
          //  Debe ser un objeto JSON por usuario
          if Arr.Items[i].JSONType <> jtObject then
          begin
            Inc(Errors);   // sumo error por formato inválido
            Continue;
          end;

          Obj      := TJSONObject(Arr.Items[i]);

          //  Levanto todos los campos del JSON (acepto 'password' o 'contraseña')
          jid      := Obj.Get('id', -1);
          nombre   := Obj.Get('nombre',   '');
          usuario  := Obj.Get('usuario',  '');
          email    := Obj.Get('email',    '');
          telefono := Obj.Get('telefono', '');
          pass     := Obj.Get('password', Obj.Get('contraseña', ''));

          //  Campos mínimos obligatorios
          if (jid < 0) or (nombre='') or (usuario='') or (email='') or (pass='') then
          begin
            Inc(Errors);
            Continue;
          end;


          //  ID ya existente (incluye el root=0)
          //  email o usuario ya existentes
          if ExistsId(jid) or ExistsEmailOrUsername(email) or ExistsEmailOrUsername(usuario) then
          begin
            Inc(Duplicated);
            Continue;
          end;

          // Inserto respetando el id del JSON (no rompo el autoincremento NextId)
          //  AddUserWithId reserva un nodo (PUser) y lo enlaza en la lista
          AddUserWithId(jid, nombre, usuario, email, telefono, pass, False);
          if jid > maxIdSeen then maxIdSeen := jid;

          Inc(Imported); // cuento como importado correcto
        end;

        // Ajusto NextId para que quede después del mayor ID importado
        if maxIdSeen >= NextId then
          NextId := maxIdSeen + 1;

        Result := True; //  La lectura/recorrido fue exitosa
      finally
        Root.Free;   //  libero JSON raíz
      end;
    finally
      Parser.Free;   //  libero parser
    end;
  finally
    FS.Free;         // cierro y libero el stream
  end;
end;



function AddUser(const AName, AUsername, AEmail, APhone, APass: AnsiString;
                 const ARoot: Boolean): Integer;
var
  NewNode: PUser; //  Puntero al nuevo nodo usuario
begin
  //  Reservo memoria para el nuevo nodo de usuario
  New(NewNode);
  //  Asigno campos desreferenciando con ^ (porque es un puntero)
  NewNode^.Id       := NextId;
  NewNode^.Name     := AName;
  NewNode^.Username := AUsername;
  NewNode^.Email    := AEmail;
  NewNode^.Phone    := APhone;
  NewNode^.Password := APass;
  NewNode^.IsRoot   := ARoot;

  //  Inicializo las estructuras internas de este usuario
  InitInbox(NewNode^.Inbox);
  InitTrash(NewNode^.Trash);
  InitQueue(NewNode^.Sched);
  InitContacts(NewNode^.Contacts);

  //  Inserto al inicio (O(1)): apunto Next al head actual y muevo la cabeza
  NewNode^.Next := UsersHead; // [APUNTADOR] enlace al siguiente
  UsersHead := NewNode;       // [APUNTADOR] nueva cabeza de la lista

  //  Devuelvo el ID asignado y avanzo el autoincremental
  Result := NextId;
  Inc(NextId);
end;

function FindUserByEmailOrUsername(const Key: AnsiString): PUser;
var
  Curr: PUser; //  Puntero cursor para recorrer la lista
begin
  //  Camino la lista simple desde la cabeza
  Curr := UsersHead;
  while Curr <> nil do
  begin
    //  Uso AnsiCompareText para comparar sin sensibilidad a may/min
    if (AnsiCompareText(Curr^.Email, Key) = 0)  or
        (AnsiCompareText(Curr^.Username, Key) = 0)then
      Exit(Curr);          //  devuelvo el puntero al nodo encontrado
    Curr := Curr^.Next;    //  avanzo al siguiente
  end;
  Result := nil;           //  no lo encontré
end;

function ExistsEmailOrUsername(const Key: AnsiString): Boolean;
begin
  //  Reuso la búsqueda: existe si devuelve puntero no nulo
  Result := FindUserByEmailOrUsername(Key) <> nil;
end;

function ValidateUser(const Key, APass: AnsiString; out OutIsRoot: Boolean): Boolean;
var
  U: PUser; // [ Puntero al usuario que intento validar
begin
  //  Busco por email o username
  U := FindUserByEmailOrUsername(Key);
  if (U <> nil) and (U^.Password = APass) then
  begin
    //  Guardo el usuario actual y retorno si es root
    CurrentUser := U;          //  sesiono con ese puntero
    OutIsRoot := U^.IsRoot;
    Exit(True);
  end;
  // FALLO
  OutIsRoot := False;
  Result := False;
end;

procedure InitUsers;
begin
  //  Dejo la lista vacía y el siguiente ID en 0
  UsersHead := nil; // [APUNTADOR] cabeza nula indica lista vacía
  NextId := 0;

  //  Inicializo la matriz global de relaciones (uMatrix)
  InitRel;

  //  Creo el usuario root obligatorio (ID=0). Es un alta con ID explícito.
  //  Internamente AddUserWithId reserva/enlaza el nodo (PUser)
  AddUserWithId(0, 'root', 'root', 'root@edd.com', '', 'root123', True);

  //  Aseguro que el siguiente ID disponible sea 1
  NextID := 1;
end;

function ExportUsersDOT(const DirPath: string): Boolean;
var
  F: TextFile;              //  Archivo de salida .dot
  OutPath: string;          //  Ruta completa del .dot
  List: array of PUser;     // APUNTADOR Arreglo temporal de punteros a usuarios para imprimir en orden
  Count, i: Integer;        // Conteo de nodos y cursor para invertir
  U: PUser;                 //  Cursor para recorrer la lista simple: UsersHead -> ... -> nil
begin
  Result := False;

  //  Valido directorio de salida; si no existe, intento crearlo
  if DirPath = '' then Exit;
  if not DirectoryExists(DirPath) then
    if not ForceDirectories(DirPath) then Exit;

  // Defino archivo DOT
  OutPath := IncludeTrailingPathDelimiter(DirPath) + 'usuarios.dot';
  AssignFile(F, OutPath);
  try
    Rewrite(F);

    //  Encabezado y estilo del grafo (similar al enunciado)
    Writeln(F, 'digraph "Reporte de Usuarios" {');
    Writeln(F, '  rankdir=LR;');
    Writeln(F, '  graph [fontsize=12, labelloc="t"];');
    Writeln(F, '  node  [shape=box, style="rounded,filled", color="#2c3e50", fillcolor="#cfe9f3", fontname="Helvetica", fontsize=10];');
    Writeln(F, '  edge  [color="#2c3e50", arrowsize=0.6];');
    Writeln(F, '  label="Reporte de Usuarios";');

    // enmarcar la lista enlazada
    Writeln(F, '  subgraph cluster_usuarios {');
    Writeln(F, '    label="Lista Enlazada";');
    Writeln(F, '    style="rounded";');
    Writeln(F, '    color="#7f8c8d";');

    //  Recorro la lista simple de usuarios y guardo los punteros en un arreglo
    Count := 0;
    U := UsersHead;       // APUNTADOR inicio desde la cabeza
    while U <> nil do
    begin
      Inc(Count);
      SetLength(List, Count);
      List[Count-1] := U; // APUNTADOR guardo el puntero al nodo actual
      U := U^.Next;       // APUNTADOR avanzo al siguiente
    end;

    // Emite nodos en orden inverso (del fondo al tope), para que las flechas queden de menor a mayor id visualmente
    for i := Count-1 downto 0 do
    begin
      U := List[i]; // APUNTADOR tomo el puntero guardado
      // DOT Creo el nodo con su etiqueta HTML-like
      Writeln(F, '    u', U^.Id, ' [label=<',
                '<b>ID: ', U^.Id, '</b><br/>',
                'Nombre: ', U^.Name, '<br/>',
                'Usuario: ', U^.Username, '<br/>',
                'Email: ', U^.Email, '<br/>',
                'Teléfono: ', U^.Phone,
                '>];');

      // DOT Enlazo al siguiente del orden invertido (u actual -> u anterior en el array)
      if i > 0 then
        Writeln(F, '    u', U^.Id, ' -> u', List[i-1]^.Id, ';'); // [APUNTADOR] uso el Id del puntero adyacente
    end;

    Writeln(F, '  }'); // fin cluster
    Writeln(F, '}');   // fin digraph

    //  Cierro y confirmo éxito
    CloseFile(F);
    Result := True;
  except
    on E: Exception do
    begin
      //  En caso de error, intento cerrar y retorno False
      {$I-} CloseFile(F); {$I+}
      Result := False;
    end;
  end;
end;

function ExportContactsDOTForUser(const L: TContactList;
  const OwnerEmail, BaseDir: string; out DotPath: string): Boolean;
var
  F: TextFile;          //  Archivo de salida .dot
  First, C: PContact;   //  Punteros de la lista circular: First es cabeza, C es cursor
  Path: string;         //  Ruta final del .dot
  idx, i: Integer;      // [ Índices para nombrar nodos y aristas
begin
  Result  := False;
  DotPath := '';

  //  Directorio de salida (lo creo si no existe)
  if (BaseDir = '') then Exit;
  if not DirectoryExists(BaseDir) then
    if not ForceDirectories(BaseDir) then Exit;

  //  Nombre del archivo DOT por propietario
  Path := IncludeTrailingPathDelimiter(BaseDir) + 'contactos_' + OwnerEmail + '.dot';
  AssignFile(F, Path);
  try
    Rewrite(F);

    //  Encabezado básico
    Writeln(F, 'digraph "Reporte de Contactos" {');
    Writeln(F, '  rankdir=LR;');
    Writeln(F, '  labelloc="t";');
    Writeln(F, '  label="Reporte de Contactos";');
    Writeln(F, '  node [shape=box, style="rounded,filled", fillcolor="#cfe9f7"];');

    //  Cluster para la lista circular
    Writeln(F, '  subgraph cluster_contacts {');
    Writeln(F, '    label="Lista Circular"; color="#bbbbbb";');

    // LISTA CIRCULAR Si la lista no está vacía, L.Tail apunta al último nodo
    if L.Tail <> nil then
    begin
      First := L.Tail^.Next;  // [APUNTADOR] cabeza real (siguiente del tail)
      C     := First;         // [APUNTADOR] cursor de recorrido
      idx   := 1;

      // Emisión de nodos recorriendo hasta volver a First
      repeat
        // DOT Nodo con datos del contacto
        Writeln(F, '    n', idx, ' [label=<',
          '<b>ID:</b> ', idx, '<br/>',
          '<b>Nombre:</b> ', C^.Name, '<br/>',
          '<b>Usuario:</b> ', C^.Username, '<br/>',
          '<b>Email:</b> ', C^.Email, '<br/>',
          '<b>Teléfono:</b> ', C^.Phone,
          '>, width=3];');

        C := C^.Next; // APUNTADOR avanzo en la lista circular
        Inc(idx);
      until C = First; // CIRCULAR me detengo cuando doy la vuelta completa

      // ARISTAS Si hay más de un nodo, dibujo doble flecha entre consecutivos y cierro el ciclo
      if idx > 2 then
      begin
        for i := 1 to idx-2 do
          Writeln(F, '    n', i, ' -> n', i+1, ' [dir=both, arrowsize=0.7];');
        //  enlace final: último con primero
        Writeln(F, '    n', idx-1, ' -> n1 [dir=both, arrowsize=0.7];');
      end;
      // CASO 1 NODO Si solo hay uno, no dibujo aristas
    end;

    Writeln(F, '  }'); // fin cluster
    Writeln(F, '}');   // fin digraph
    CloseFile(F);

    // Devuelvo la ruta y marco éxito
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
