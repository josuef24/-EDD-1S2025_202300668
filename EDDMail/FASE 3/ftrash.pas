unit fTrash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmTrash }

  TfrmTrash = class(TForm)
    lblTitulo: TLabel;
    edtBuscar: TEdit;
    btnBuscar: TButton;
    btnEliminar: TButton;
    btnRegresar: TButton;
    lstTrash: TListBox;
    procedure FormShow(Sender: TObject);
    procedure btnBuscarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnRegresarClick(Sender: TObject);
  private
    FUltimoFiltro: string;
    procedure LlenarLista(const FiltroAsunto: string = '');
    procedure EliminarPorIndiceDesdeTop(IndexFromTop: Integer);
  end;

var
  frmTrash: TfrmTrash;

implementation

uses uUsers, uTrash, frmUser, uInbox;

{$R *.lfm}

procedure TfrmTrash.FormShow(Sender: TObject);
begin
  Caption := 'Papelera';
  lblTitulo.Caption := 'Papelera';
  FUltimoFiltro := '';
  LlenarLista('');
end;

procedure TfrmTrash.LlenarLista(const FiltroAsunto: string);
var
  N: PTrashNode;
  fil, preview, linea: string;
begin
  lstTrash.Items.BeginUpdate;
  try
    lstTrash.Clear;
    if (CurrentUser = nil) then Exit;

    fil := LowerCase(Trim(FiltroAsunto));
    N := CurrentUser^.Trash.Top; // tope arriba
    while N <> nil do
    begin
      if (fil='') or (Pos(fil, LowerCase(N^.Mail^.Asunto)) > 0) then
      begin
        preview := Trim(N^.Mail^.Mensaje);
        if Length(preview) > 40 then
          preview := Copy(preview, 1, 40) + '...';
        linea := Format('%s | %s | %s',
                        [N^.Mail^.Asunto, N^.Mail^.Remitente, preview]);
        lstTrash.Items.Add(linea);
      end;
      N := N^.Next;
    end;

    if lstTrash.Count > 0 then
      lstTrash.ItemIndex := 0;
  finally
    lstTrash.Items.EndUpdate;
  end;
end;

procedure TfrmTrash.btnBuscarClick(Sender: TObject);
begin
  FUltimoFiltro := Trim(edtBuscar.Text);
  LlenarLista(FUltimoFiltro);
end;

procedure TfrmTrash.EliminarPorIndiceDesdeTop(IndexFromTop: Integer);
var
  Temp: TTrash;
  i: Integer;
  M: PMail;
begin
  // mueve top->temp hasta llegar al índice deseado
  InitTrash(Temp);
  for i := 1 to IndexFromTop do
  begin
    if IsTrashEmpty(CurrentUser^.Trash) then Exit;
    PushTrash(Temp, PopTrash(CurrentUser^.Trash));
  end;

  // ahora el deseado está en el tope
  M := PopTrash(CurrentUser^.Trash);
  if M <> nil then
    Dispose(M); // eliminación permanente del correo

  // regresar lo temporal
  while not IsTrashEmpty(Temp) do
    PushTrash(CurrentUser^.Trash, PopTrash(Temp));
end;

procedure TfrmTrash.btnEliminarClick(Sender: TObject);
var
  k: Integer;
begin
  if (CurrentUser = nil) or IsTrashEmpty(CurrentUser^.Trash) then
  begin
    ShowMessage('La papelera está vacía.');
    Exit;
  end;

  k := lstTrash.ItemIndex; // 0 = elemento más reciente (tope)
  if k < 0 then
  begin
    ShowMessage('Selecciona un elemento.');
    Exit;
  end;

  if MessageDlg('Eliminar',
                '¿Eliminar permanentemente el correo seleccionado?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    EliminarPorIndiceDesdeTop(k);
    LlenarLista(FUltimoFiltro);
  end;
end;

procedure TfrmTrash.btnRegresarClick(Sender: TObject);
begin
  frmTrash.Hide;
  frmUserN.Show;
end;

end.


