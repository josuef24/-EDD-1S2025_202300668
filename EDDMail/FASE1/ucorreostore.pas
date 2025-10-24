unit UCorreoStore;

{$mode ObjFPC}{$H+}

interface

uses UDataMail;

procedure MailStore_Clear;
function  MailStore_Count: Integer;
function  MailStore_IndexOfID(const ID: LongInt): Integer;
function  MailStore_AddIfNew(const C: TCorreo): Boolean; // true si se agregó, false si duplicado
function  MailStore_Get(const Index: Integer; out C: TCorreo): Boolean;

implementation

var
  GList: array of TCorreo = nil;

procedure MailStore_Clear;
begin
  SetLength(GList, 0);
end;

function MailStore_Count: Integer;
begin
  Result := Length(GList);
end;

function MailStore_IndexOfID(const ID: LongInt): Integer;
var i: Integer;
begin
  for i := 0 to High(GList) do
    if GList[i].ID = ID then Exit(i);
  Result := -1;
end;

function MailStore_AddIfNew(const C: TCorreo): Boolean;
var idx: Integer;
begin
  idx := MailStore_IndexOfID(C.ID);
  Result := (idx = -1);
  if Result then
  begin
    SetLength(GList, Length(GList)+1);
    GList[High(GList)] := C;
  end;
end;

function MailStore_Get(const Index: Integer; out C: TCorreo): Boolean;
begin
  Result := (Index>=0) and (Index<Length(GList));
  if Result then C := GList[Index];
end;

end.

