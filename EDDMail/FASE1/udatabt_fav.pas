unit UDataBT_Fav;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, Classes, UBTree_Favoritos;

var
  FavRoot      : PBNode = nil;
  FavFilePath  : string;
  FavNextID    : Integer = 1;

procedure FavInit;
procedure FavAdd(AID: Integer; const Remitente, Asunto, Fecha, Mensaje: AnsiString);
function  FavFind(const Asunto: AnsiString; out It: TFavItem): Boolean;
function  FavDelete(const Asunto: AnsiString): Boolean;
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
      IntToStr(It.ID) + #9 +
      Esc(It.Remitente) + #9 +
      Esc(It.Asunto) + #9 +
      Esc(It.Fecha) + #9 +
      Esc(It.Mensaje) + #9 +
      sAct
    );
  end;
end;

procedure FavInit;
begin
  if FavRoot = nil then
  begin
    BInit(FavRoot);
    FavFilePath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'favoritos.tsv';
    FavNextID := 1;
  end;
end;

procedure FavAdd(AID: Integer; const Remitente, Asunto, Fecha, Mensaje: AnsiString);
var it: TFavItem;
begin
  if FavRoot = nil then FavInit;

  it.ID        := AID;
  it.Remitente := Trim(Remitente);
  it.Asunto    := Trim(Asunto);
  it.Fecha     := Trim(Fecha);
  it.Mensaje   := Trim(Mensaje);
  it.Activo    := True;

  BInsert(FavRoot, it);

  if AID >= FavNextID then
    FavNextID := AID + 1;
end;

function FavFind(const Asunto: AnsiString; out It: TFavItem): Boolean;
begin
  if FavRoot = nil then
  begin
    FillChar(It, SizeOf(It), 0);
    Exit(False);
  end;
  Result := BSearch(FavRoot, Trim(Asunto), It);
end;

function FavDelete(const Asunto: AnsiString): Boolean;
begin
  if FavRoot = nil then Exit(False);
  Result := BDeleteLogical(FavRoot, Trim(Asunto));
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
var
  L, parts: TStringList;
  i, maxID: Integer;
  it: TFavItem;
begin
  if not FileExists(AFile) then Exit;

  if FavRoot = nil then FavInit;

  L := TStringList.Create;
  parts := TStringList.Create;
  maxID := 0;
  try
    parts.StrictDelimiter := True;
    parts.Delimiter := #9;
    L.LoadFromFile(AFile);
    for i := 0 to L.Count-1 do
    begin
      parts.DelimitedText := L[i];
      if parts.Count < 6 then continue;

      it.ID        := StrToIntDef(parts[0], 0);
      it.Remitente := UnEsc(parts[1]);
      it.Asunto    := UnEsc(parts[2]);
      it.Fecha     := UnEsc(parts[3]);
      it.Mensaje   := UnEsc(parts[4]);
      it.Activo    := (parts[5] = '1');

      BInsert(FavRoot, it);
      if not it.Activo then
        BDeleteLogical(FavRoot, it.Asunto);

      if it.ID > maxID then
        maxID := it.ID;
    end;

    FavNextID := maxID + 1;
  finally
    parts.Free;
    L.Free;
  end;
end;

initialization
  FavRoot := nil;
  FavNextID := 1;

end.
