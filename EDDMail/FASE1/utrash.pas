unit uTrash;

{$mode objfpc}{$H+}

interface

uses
  uInbox; // para el tipo PMail (puntero al correo)

type
  PTrashNode = ^TTrashNode;
  TTrashNode = record
    Mail: PMail;       // puntero al correo eliminado
    Next: PTrashNode;  // siguiente en la pila
  end;

  TTrash = record
    Top: PTrashNode;   // tope de la pila
    Count: Integer;    // cantidad de elementos
  end;

procedure InitTrash(out T: TTrash);
procedure PushTrash(var T: TTrash; M: PMail);
function  PopTrash(var T: TTrash): PMail;
function  PeekTrash(const T: TTrash): PMail;
function  IsTrashEmpty(const T: TTrash): Boolean;

implementation

procedure InitTrash(out T: TTrash);
begin
  T.Top := nil;
  T.Count := 0;
end;

procedure PushTrash(var T: TTrash; M: PMail);
var
  N: PTrashNode;
begin
  if M = nil then Exit;
  New(N);
  N^.Mail := M;
  N^.Next := T.Top;
  T.Top := N;
  Inc(T.Count);
end;

function PopTrash(var T: TTrash): PMail;
var
  N: PTrashNode;
begin
  if T.Top = nil then Exit(nil);
  N := T.Top;
  T.Top := N^.Next;
  Dec(T.Count);
  Result := N^.Mail;
  Dispose(N);
end;

function PeekTrash(const T: TTrash): PMail;
begin
  if T.Top <> nil then Result := T.Top^.Mail else Result := nil;
end;

function IsTrashEmpty(const T: TTrash): Boolean;
begin
  Result := T.Top = nil;
end;

end.

