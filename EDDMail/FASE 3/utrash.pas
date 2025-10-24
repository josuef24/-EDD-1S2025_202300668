unit uTrash;

{$mode objfpc}{$H+}

interface

uses
  uInbox; // APUNTADOR Aquí está PMail (puntero al correo)

type
  PTrashNode = ^TTrashNode; // APUNTADOR Puntero a nodo de la pila (cada nodo apunta al siguiente)
  TTrashNode = record
    Mail: PMail;       // APUNTADOR Puntero al correo eliminado (lo saco de la bandeja y lo referencio aquí)
    Next: PTrashNode;  // APUNTADOR Siguiente nodo en la pila (enlazo como lista simple tipo stack)
  end;

  TTrash = record
    Top: PTrashNode;   // APUNTADOR Tope de la pila; si es nil, la pila está vacía
    Count: Integer;    //  Llevo el conteo de elementos por conveniencia
  end;

procedure InitTrash(out T: TTrash);
procedure PushTrash(var T: TTrash; M: PMail);
function  PopTrash(var T: TTrash): PMail;
function  PeekTrash(const T: TTrash): PMail;
function  IsTrashEmpty(const T: TTrash): Boolean;

implementation

procedure InitTrash(out T: TTrash);
begin
  //  Inicializo la pila vacía
  T.Top := nil;    // APUNTADOR nil indica que no hay nodos
  T.Count := 0;
end;

procedure PushTrash(var T: TTrash; M: PMail);
var
  N: PTrashNode; // APUNTADOR Nodo nuevo que voy a crear
begin
  //  Si no me pasan un correo válido, no hago nada
  if M = nil then Exit;

  //  Reservo memoria para el nuevo nodo y lo apilo al tope
  New(N);          // [APUNTADOR] New me da el puntero al nodo recién creado
  N^.Mail := M;    // [APUNTADOR] Guardo la referencia al correo
  N^.Next := T.Top;// [APUNTADOR] El siguiente del nuevo es el tope actual
  T.Top := N;      // [APUNTADOR] Ahora el nuevo pasa a ser el tope
  Inc(T.Count);    // Actualizo el tamaño de la pila
end;

function PopTrash(var T: TTrash): PMail;
var
  N: PTrashNode; // [APUNTADOR] Nodo que voy a desapilar y liberar
begin
  //  Si está vacía, retorno nil
  if T.Top = nil then Exit(nil);

  N := T.Top;          // [APUNTADOR] Guardo el puntero al tope
  T.Top := N^.Next;    // [APUNTADOR] Muevo el tope al siguiente
  Dec(T.Count);        // Actualizo el tamaño
  Result := N^.Mail;   // [APUNTADOR] Devuelvo el puntero al correo

  // Libero el nodo de la pila (el correo no lo libero aquí)
  Dispose(N);
end;

function PeekTrash(const T: TTrash): PMail;
begin
  //  Solo miro el tope sin desapilar; si no hay, devuelvo nil
  if T.Top <> nil then Result := T.Top^.Mail else Result := nil; //  Acceso al campo del nodo
end;

function IsTrashEmpty(const T: TTrash): Boolean;
begin
  //  La pila está vacía si Top es nil
  Result := T.Top = nil;
end;

end.
unit uTrash;

{$mode objfpc}{$H+}

interface

uses
  uInbox; // [APUNTADOR] Aquí está PMail (puntero al correo)

type
  PTrashNode = ^TTrashNode; // [APUNTADOR] Puntero a nodo de la pila (cada nodo apunta al siguiente)
  TTrashNode = record
    Mail: PMail;       // [APUNTADOR] Puntero al correo eliminado (lo saco de la bandeja y lo referencio aquí)
    Next: PTrashNode;  // [APUNTADOR] Siguiente nodo en la pila (enlazo como lista simple tipo stack)
  end;

  TTrash = record
    Top: PTrashNode;   // [APUNTADOR] Tope de la pila; si es nil, la pila está vacía
    Count: Integer;    //  Llevo el conteo de elementos por conveniencia
  end;

procedure InitTrash(out T: TTrash);
procedure PushTrash(var T: TTrash; M: PMail);
function  PopTrash(var T: TTrash): PMail;
function  PeekTrash(const T: TTrash): PMail;
function  IsTrashEmpty(const T: TTrash): Boolean;

implementation

procedure InitTrash(out T: TTrash);
begin
  //  Inicializo la pila vacía
  T.Top := nil;    // [APUNTADOR] nil indica que no hay nodos
  T.Count := 0;
end;

procedure PushTrash(var T: TTrash; M: PMail);
var
  N: PTrashNode; // [APUNTADOR] Nodo nuevo que voy a crear
begin
  if M = nil then Exit;

  // Reservo memoria para el nuevo nodo y lo apilo al tope
  New(N);          // [APUNTADOR] New me da el puntero al nodo recién creado
  N^.Mail := M;    // [APUNTADOR] Guardo la referencia al correo
  N^.Next := T.Top;// [APUNTADOR] El siguiente del nuevo es el tope actual
  T.Top := N;      // [APUNTADOR] Ahora el nuevo pasa a ser el tope
  Inc(T.Count);    // Actualizo el tamaño de la pila
end;

function PopTrash(var T: TTrash): PMail;
var
  N: PTrashNode; // [APUNTADOR] Nodo que voy a desapilar y liberar
begin
  //  Si está vacía, retorno nil
  if T.Top = nil then Exit(nil);

  //  Quito el tope y regreso el correo que contenía
  N := T.Top;          // [APUNTADOR] Guardo el puntero al tope
  T.Top := N^.Next;    // [APUNTADOR] Muevo el tope al siguiente
  Dec(T.Count);        // [DATA] Actualizo el tamaño
  Result := N^.Mail;   // [APUNTADOR] Devuelvo el puntero al correo

  //  Libero el nodo de la pila (el correo no lo libero aquí)
  Dispose(N);
end;

function PeekTrash(const T: TTrash): PMail;
begin
  //  Solo miro el tope sin desapilar; si no hay, devuelvo nil
  if T.Top <> nil then Result := T.Top^.Mail else Result := nil; // [APUNTADOR] Acceso al campo del nodo
end;

function IsTrashEmpty(const T: TTrash): Boolean;
begin
  //  La pila está vacía si Top es nil
  Result := T.Top = nil;
end;

end.

