unit UDataBT_Fav;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, Classes, UBTree_Favoritos;

var
  FavRoot      : PBNode = nil;  // IMPORTANTE: Inicializar en nil
  FavFilePath  : string;

procedure FavInit;
procedure FavAdd(const Email, Username, Nombre: AnsiString);
function  FavFind(const Email: AnsiString; out It: TFavItem): Boolean;
function  FavDelete(const Email: AnsiString): Boolean;
procedure FavSaveToFile(const AFile: string);
procedure FavLoadFromFile(const AFile: string);

implementation

var
  _FavSaveList: TStringList;

function Esc(const S: string): string;
begin
  Result := StringReplace(S, #9, '\t', [rfReplaceAll]);
  Result := StringReplace(Result, LineEnding, '\n', [rfReplaceAll]);
end;

function UnEsc(const S: string): string;
begin
  Result := StringReplace(S, '\t', #9, [rfReplaceAll]);
  Result := StringReplace(Result, '\n', LineEnding, [rfReplaceAll]);
end;

procedure _DumpFavToList(const It: TFavItem);
var sAct: string;
begin
  if Assigned(_FavSaveList) then
  begin
    if It.Activo then sAct := '1' else sAct := '0';
    _FavSaveList.Add(
      Esc(It.Email) + #9 + Esc(It.Username) + #9 + Esc(It.Nombre) + #9 + sAct
    );
  end;
end;

procedure FavInit;
begin
  if FavRoot = nil then
  begin
    BInit(FavRoot);
    FavFilePath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'favoritos.tsv';
  end;
end;

procedure FavAdd(const Email, Username, Nombre: AnsiString);
var it: TFavItem;
begin
  if FavRoot = nil then FavInit;

  it.Email    := Trim(Email);
  it.Username := Trim(Username);
  it.Nombre   := Trim(Nombre);
  it.Activo   := True;
  BInsert(FavRoot, it);
end;

function FavFind(const Email: AnsiString; out It: TFavItem): Boolean;
begin
  if FavRoot = nil then
  begin
    FillChar(It, SizeOf(It), 0);
    Exit(False);
  end;
  Result := BSearch(FavRoot, Trim(Email), It);
end;

function FavDelete(const Email: AnsiString): Boolean;
begin
  if FavRoot = nil then Exit(False);
  Result := BDeleteLogical(FavRoot, Trim(Email));
end;

procedure FavSaveToFile(const AFile: string);
var L: TStringList;
begin
  if FavRoot = nil then Exit;

  L := TStringList.Create;
  _FavSaveList := L;
  try
    BTraverseInOrderPlain(FavRoot, @_DumpFavToList);
    L.SaveToFile(AFile);
  finally
    _FavSaveList := nil;
    L.Free;
  end;
end;

procedure FavLoadFromFile(const AFile: string);
var L, parts: TStringList; i: Integer; it: TFavItem;
begin
  if not FileExists(AFile) then Exit;

  if FavRoot = nil then FavInit;

  L := TStringList.Create;
  parts := TStringList.Create;
  try
    parts.StrictDelimiter := True;
    parts.Delimiter := #9;
    L.LoadFromFile(AFile);
    for i := 0 to L.Count-1 do
    begin
      parts.DelimitedText := L[i];
      if parts.Count<4 then continue;
      it.Email    := UnEsc(parts[0]);
      it.Username := UnEsc(parts[1]);
      it.Nombre   := UnEsc(parts[2]);
      it.Activo   := (parts[3] = '1');
      BInsert(FavRoot, it);
      if not it.Activo then
        BDeleteLogical(FavRoot, it.Email);
    end;
  finally
    parts.Free;
    L.Free;
  end;
end;

initialization
  FavRoot := nil;

end.
