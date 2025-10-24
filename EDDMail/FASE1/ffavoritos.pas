unit FFavoritos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Grids, StdCtrls,
  UBTree_Favoritos, UDataBT_Fav, uUsers, frmUser;

type
  { TFormFavoritos }
  TFormFavoritos = class(TForm)
    btnEliminar: TButton;
    btnRefrescar: TButton;
    btnRegresar: TButton;
    btnVerCorreo: TButton;
    sgFav: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnRegresarClick(Sender: TObject);
    procedure btnVerCorreoClick(Sender: TObject);
    procedure sgFavDblClick(Sender: TObject);
  private
    FItems: TStringList;
    procedure LlenarGrid;
    procedure RecolectarItems(Root: PBNode);
    function  AsuntoSeleccionado(out S: string): Boolean;
  public
  end;

var
  FormFavoritos: TFormFavoritos;

implementation

uses fViewMail;

{$R *.lfm}

procedure TFormFavoritos.FormCreate(Sender: TObject);
begin
  if FavRoot = nil then
    FavInit;

  Caption := 'Correos Favoritos (Árbol B - orden 5)';

  sgFav.ColCount := 4;
  sgFav.RowCount := 1;
  sgFav.FixedCols := 0;
  sgFav.FixedRows := 1;
  sgFav.Cells[0,0] := 'ID';
  sgFav.Cells[1,0] := 'Asunto';
  sgFav.Cells[2,0] := 'Remitente';
  sgFav.Cells[3,0] := 'Fecha';

  sgFav.ColWidths[0] := 50;
  sgFav.ColWidths[1] := 250;
  sgFav.ColWidths[2] := 200;
  sgFav.ColWidths[3] := 150;

  FItems := TStringList.Create;
end;

procedure TFormFavoritos.FormDestroy(Sender: TObject);
begin
  if Assigned(FItems) then
    FItems.Free;
end;

procedure TFormFavoritos.FormShow(Sender: TObject);
begin
  LlenarGrid;
end;

procedure TFormFavoritos.RecolectarItems(Root: PBNode);
var
  i: Integer;
begin
  if Root = nil then Exit;

  for i := 1 to Root^.N do
  begin
    if not Root^.Leaf then
      RecolectarItems(Root^.C[i-1]);

    if Root^.Key[i].Activo then
    begin
      FItems.Add(
        IntToStr(Root^.Key[i].ID) + #9 +
        Root^.Key[i].Asunto + #9 +
        Root^.Key[i].Remitente + #9 +
        Root^.Key[i].Fecha + #9 +
        Root^.Key[i].Mensaje
      );
    end;
  end;

  if not Root^.Leaf then
    RecolectarItems(Root^.C[Root^.N]);
end;

procedure TFormFavoritos.LlenarGrid;
var
  i: Integer;
  parts: TStringList;
begin
  if not Assigned(sgFav) then Exit;

  sgFav.RowCount := 1;
  FItems.Clear;

  if FavRoot = nil then
  begin
    Caption := 'Correos Favoritos (0)';
    Exit;
  end;

  try
    RecolectarItems(FavRoot);

    if FItems.Count > 0 then
    begin
      parts := TStringList.Create;
      try
        parts.Delimiter := #9;
        parts.StrictDelimiter := True;

        sgFav.RowCount := FItems.Count + 1;

        for i := 0 to FItems.Count - 1 do
        begin
          parts.DelimitedText := FItems[i];
          if parts.Count >= 4 then
          begin
            sgFav.Cells[0, i+1] := parts[0];
            sgFav.Cells[1, i+1] := parts[1];
            sgFav.Cells[2, i+1] := parts[2];
            sgFav.Cells[3, i+1] := parts[3];
          end;
        end;

        Caption := Format('Correos Favoritos (%d)', [FItems.Count]);
      finally
        parts.Free;
      end;
    end
    else
      Caption := 'Correos Favoritos (0)';
  except
    on E: Exception do
    begin
      ShowMessage('Error al llenar grid: ' + E.Message);
      Caption := 'Correos Favoritos - Error';
    end;
  end;
end;

procedure TFormFavoritos.btnRefrescarClick(Sender: TObject);
begin
  try
    LlenarGrid;
  except
    on E: Exception do
      ShowMessage('Error al refrescar: ' + E.Message);
  end;
end;

procedure TFormFavoritos.btnEliminarClick(Sender: TObject);
var
  asunto: string;
begin
  if not AsuntoSeleccionado(asunto) then
  begin
    ShowMessage('Selecciona un correo favorito en la tabla.');
    Exit;
  end;

  if FavRoot = nil then Exit;

  try
    if FavDelete(asunto) then
    begin
      ShowMessage('Correo eliminado de favoritos');
      FavSaveToFile(FavFilePath);
      LlenarGrid;
    end
    else
      ShowMessage('No se pudo eliminar el correo');
  except
    on E: Exception do
      ShowMessage('Error al eliminar: ' + E.Message);
  end;
end;

procedure TFormFavoritos.btnVerCorreoClick(Sender: TObject);
var
  r: Integer;
  parts: TStringList;
  it: TFavItem;
begin
  r := sgFav.Row;
  if (r < 1) or (r >= sgFav.RowCount) then
  begin
    ShowMessage('Selecciona un correo favorito');
    Exit;
  end;

  if (r - 1) >= FItems.Count then
  begin
    ShowMessage('Error: índice fuera de rango');
    Exit;
  end;

  parts := TStringList.Create;
  try
    parts.Delimiter := #9;
    parts.StrictDelimiter := True;
    parts.DelimitedText := FItems[r-1];

    if parts.Count >= 5 then
    begin
      it.ID        := StrToIntDef(parts[0], 0);
      it.Asunto    := parts[1];
      it.Remitente := parts[2];
      it.Fecha     := parts[3];
      it.Mensaje   := parts[4];

      ShowMessage(
        'ID: ' + IntToStr(it.ID) + LineEnding +
        'Asunto: ' + it.Asunto + LineEnding +
        'De: ' + it.Remitente + LineEnding +
        'Fecha: ' + it.Fecha + LineEnding +
        LineEnding +
        it.Mensaje
      );
    end;
  finally
    parts.Free;
  end;
end;

procedure TFormFavoritos.sgFavDblClick(Sender: TObject);
begin
  btnVerCorreoClick(Sender);
end;

function TFormFavoritos.AsuntoSeleccionado(out S: string): Boolean;
var
  r: Integer;
begin
  Result := False;

  if not Assigned(sgFav) then Exit;

  r := sgFav.Row;
  if (r < 1) or (r >= sgFav.RowCount) then Exit;

  S := Trim(sgFav.Cells[1, r]);
  Result := (S <> '');
end;

procedure TFormFavoritos.btnRegresarClick(Sender: TObject);
begin
  try
    Hide;
    if Assigned(frmUserN) then
      frmUserN.Show
    else
      ShowMessage('Error: Formulario de usuario no disponible');
  except
    on E: Exception do
      ShowMessage('Error al regresar: ' + E.Message);
  end;
end;

end.
