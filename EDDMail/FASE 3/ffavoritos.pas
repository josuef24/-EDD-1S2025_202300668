unit FFavoritos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Grids, StdCtrls,
  UBTree_Favoritos, UDataBT_Fav, uUsers, frmUser; // uUsers para CurrentUser, etc.

type
  { TFormFavoritos }
  TFormFavoritos = class(TForm)
    btnAgregar: TButton;
    btnEliminar: TButton;
    btnBuscar: TButton;
    btnRefrescar: TButton;
    btnRegresar: TButton;
    edtEmail: TEdit;
    edtUsername: TEdit;
    edtNombre: TEdit;
    lblEmail: TLabel;
    lblUsername: TLabel;
    lblNombre: TLabel;
    sgFav: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure btnAgregarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnBuscarClick(Sender: TObject);
    procedure btnRegresarClick(Sender: TObject);

  private
    FFila: Integer;
    procedure LlenarGrid;
    procedure VisitAdd(const It: TFavItem);
    function  EmailSeleccionado(out S: string): Boolean;
  public
  end;

var
  FormFavoritos: TFormFavoritos;

implementation

{$R *.lfm}

procedure TFormFavoritos.FormCreate(Sender: TObject);
begin
  Caption := 'Favoritos (Árbol B - orden 5)';
  sgFav.ColCount := 3;
  sgFav.RowCount := 1;
  sgFav.FixedCols := 0;
  sgFav.Cells[0,0] := 'Email';
  sgFav.Cells[1,0] := 'Username';
  sgFav.Cells[2,0] := 'Nombre';
end;

procedure TFormFavoritos.FormShow(Sender: TObject);
begin
  LlenarGrid;
end;

procedure TFormFavoritos.LlenarGrid;
begin
  FFila := 1;
  sgFav.RowCount := 1;

  // ¡guardia contra árbol vacío!
  if FavRoot = nil then Exit;

  // si tu UBTree no admite nil visitor, igual pasa porque ya salimos antes
  BTraverseInOrder(FavRoot, @VisitAdd);
end;

procedure TFormFavoritos.VisitAdd(const It: TFavItem);
begin
  sgFav.RowCount := FFila + 1;
  sgFav.Cells[0, FFila] := It.Email;
  sgFav.Cells[1, FFila] := It.Username;
  sgFav.Cells[2, FFila] := It.Nombre;
  Inc(FFila);
end;

procedure TFormFavoritos.btnRefrescarClick(Sender: TObject);
begin
  LlenarGrid;
end;

procedure TFormFavoritos.btnAgregarClick(Sender: TObject);
var em,u,n: string;
begin
  em := Trim(edtEmail.Text);
  u  := Trim(edtUsername.Text);
  n  := Trim(edtNombre.Text);
  if em = '' then begin ShowMessage('Email requerido.'); Exit; end;

  FavAdd(em, u, n);
  ShowMessage('Agregado a favoritos: ' + em);
  LlenarGrid;
end;

procedure TFormFavoritos.btnEliminarClick(Sender: TObject);
var em: string;
begin
  if not EmailSeleccionado(em) then
  begin
    ShowMessage('Selecciona un favorito en la tabla.'); Exit;
  end;

  if FavRoot = nil then Exit;

  if FavDelete(em) then
  begin
    ShowMessage('Eliminado de favoritos: ' + em);
    LlenarGrid;
  end
  else
    ShowMessage('No se encontró activo: ' + em);
end;

procedure TFormFavoritos.btnBuscarClick(Sender: TObject);
var em: string; it: TFavItem;
begin
  em := Trim(edtEmail.Text);
  if em = '' then begin ShowMessage('Ingresa un email para buscar.'); Exit; end;
  if FavFind(em, it) then
    ShowMessage('Encontrado: ' + it.Email + ' (' + it.Username + ' / ' + it.Nombre + ')')
  else
    ShowMessage('No está en favoritos.');
end;

function TFormFavoritos.EmailSeleccionado(out S: string): Boolean;
var r: Integer;
begin
  Result := False;
  r := sgFav.Row;
  if (r < 1) or (r >= sgFav.RowCount) then Exit;
  S := Trim(sgFav.Cells[0, r]);
  Result := (S <> '');
end;

procedure TFormFavoritos.btnRegresarClick(Sender: TObject);
begin
  Hide; frmUserN.Show;
end;

end.


