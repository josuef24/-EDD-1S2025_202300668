unit FBorradores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Grids, StdCtrls,
  UAVL_Borradores, UDataAVL, frmUser;

type
  { TFormBorradores }
  TFormBorradores = class(TForm)
    btnRefrescar: TButton;
    btnAbrir: TButton;
    btnEliminar: TButton;
    btnRegresar: TButton;
    sgBorradores: TStringGrid;
    procedure btnRegresarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure btnAbrirClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    procedure LlenarGrid;
    function  IDSeleccionado(out AID: LongInt): boolean;
    procedure VisitAddToGrid(const D: TMailDraft);
    var FFila: Integer;
  public
  end;

var
  FormBorradores: TFormBorradores;

implementation

{$R *.lfm}

uses fSendMail;

procedure TFormBorradores.FormCreate(Sender: TObject);
begin
  Caption := 'Borradores';
  sgBorradores.ColCount := 4;
  sgBorradores.RowCount := 1;
  sgBorradores.Cells[0,0] := 'ID';
  sgBorradores.Cells[1,0] := 'Remitente';
  sgBorradores.Cells[2,0] := 'Destinatario';
  sgBorradores.Cells[3,0] := 'Asunto';
end;

procedure TFormBorradores.btnRegresarClick(Sender: TObject);
begin
  FormBorradores.Hide;
  frmUserN.Show;
end;

procedure TFormBorradores.LlenarGrid;
begin
  FFila := 1;
  sgBorradores.RowCount := 1;
  AVL_InOrder(BorradoresRoot, @VisitAddToGrid); // método del form
end;

procedure TFormBorradores.FormShow(Sender: TObject);
begin
  LlenarGrid;           // refresca cada vez que se muestra
end;


procedure TFormBorradores.VisitAddToGrid(const D: TMailDraft);
begin
  sgBorradores.RowCount := FFila + 1;
  sgBorradores.Cells[0, FFila] := IntToStr(D.ID);
  sgBorradores.Cells[1, FFila] := D.Remitente;
  sgBorradores.Cells[2, FFila] := D.Destinatario;
  sgBorradores.Cells[3, FFila] := D.Asunto;
  Inc(FFila);
end;



procedure TFormBorradores.btnRefrescarClick(Sender: TObject);
begin
  LlenarGrid;
end;

function TFormBorradores.IDSeleccionado(out AID: LongInt): boolean;
var r: Integer;
begin
  Result := false;
  r := sgBorradores.Row;
  if r<=0 then exit;
  Result := TryStrToInt(sgBorradores.Cells[0,r], AID);
end;

procedure TFormBorradores.btnAbrirClick(Sender: TObject);
var id: LongInt; d: TMailDraft;
begin
  if not IDSeleccionado(id) then begin ShowMessage('Selecciona un borrador.'); exit; end;
  if not AVL_Find(BorradoresRoot, id, d) then begin ShowMessage('No encontrado.'); exit; end;

  // Usamos tu form real de envío
  if not Assigned(frmSendMail) then Application.CreateForm(TfrmSendMail, frmSendMail);

  // Cargar datos y marcar de qué borrador vienen
  frmSendMail.CargarDesdeBorrador(d, id);
  frmSendMail.Show; frmSendMail.BringToFront;
end;

procedure TFormBorradores.btnEliminarClick(Sender: TObject);
var id: LongInt;
begin
  if not IDSeleccionado(id) then begin ShowMessage('Selecciona un borrador.'); exit; end;
  if AVL_Delete(BorradoresRoot, id) then begin
    ShowMessage('Borrador eliminado.');
    LlenarGrid;
  end
  else
    ShowMessage('No se pudo eliminar.');
end;

end.

