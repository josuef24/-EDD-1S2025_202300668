unit uTrash;

{$mode objfpc}{$H+}

interface

uses uInbox; // para PMail

type
  PTrashNode = ^TTrashNode;
  TTrashNode = record
    Mail: PMail;        // apuntamos al mismo nodo de correo
    Next: PTrashNode;
  end;

  TTrash = record
    Top: PTrashNode;
    Count: Integer;
  end;

procedure InitTrash(var T: TTrash);
procedure PushTrash(var T: TTrash; M: PMail);   // apila un correo
function  PopTrash(var T: TTrash): PMail;       // desapila correo (o nil)
function  PeekTrash(const T: TTrash): PMail;    // ver tope (no desapila)
function  IsTrashEmpty(const T: TTrash): Boolean;
procedure ClearTrash(var T: TTrash; FreeMails: Boolean); // vacía; si FreeMails=True libera los PMail

implementation

procedure InitTrash(var T: TTrash);
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
  Result := nil;
  if T.Top = nil then Exit;
  N := T.Top;
  T.Top := N^.Next;
  Result := N^.Mail;
  Dispose(N);
  Dec(T.Count);
end;

function PeekTrash(const T: TTrash): PMail;
begin
  if T.Top = nil then Exit(nil);
  Result := T.Top^.Mail;
end;

function IsTrashEmpty(const T: TTrash): Boolean;
begin
  Result := (T.Top = nil);
end;

procedure ClearTrash(var T: TTrash; FreeMails: Boolean);
var
  M: PMail;
begin
  while not IsTrashEmpty(T) do
  begin
    M := PopTrash(T);
    if FreeMails and (M <> nil) then
      Dispose(M);
  end;
end;

end.

