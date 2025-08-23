unit fTrash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type

  { TfrmTrash }

  TfrmTrash = class(TForm)
    btnBuscar: TButton;
    btnEliminar: TButton;
    edtBuscar: TEdit;
    lblPalabra: TLabel;
    lblTitulo: TLabel;
    btnRegresar: TButton;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnBuscarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnRegresarClick(Sender: TObject);
  private
    FUltimaBusqueda: string;
    procedure ConfigGrid;
    procedure LlenarGrid(const FiltroAsunto: string = '');
    function  IndexDesdeTopSeleccionado: Integer; // 0 = tope
    procedure EliminarEnPosDesdeTop(IndexFromTop: Integer);
  end;

var
  frmTrash: TfrmTrash;

implementation

uses
  uTrash, uInbox, uUsers, frmUser;

{$R *.lfm}

procedure TfrmTrash.ConfigGrid;
begin
  // Opciones seguras y cabeceras
  grid.Options    := grid.Options + [goRowSelect] - [goEditing];
  grid.FixedRows  := 1;
  grid.ColCount   := 3;
  grid.RowCount   := 1;     // solo encabezado
  grid.Row        := 0;

  grid.DefaultColWidth := 200;
  grid.ColWidths[0] := 180;
  grid.ColWidths[1] := 170;
  grid.ColWidths[2] := 260;

  grid.Cells[0,0] := 'Asunto';
  grid.Cells[1,0] := 'Remitente';
  grid.Cells[2,0] := 'Mensaje';
end;

procedure TfrmTrash.FormCreate(Sender: TObject);
begin
  Caption := 'Papelera';
  lblTitulo.Caption := 'Papelera';
  edtBuscar.Text := '';
  FUltimaBusqueda := '';
  ConfigGrid;
end;   

procedure TfrmTrash.FormShow(Sender: TObject);
begin
  ConfigGrid;
  LlenarGrid;
end;

procedure TfrmTrash.LlenarGrid(const FiltroAsunto: string);
var
  N: PTrashNode;
  r: Integer;
  fil, preview: string;
begin
  if (CurrentUser = nil) then Exit;

  fil := LowerCase(Trim(FiltroAsunto));

  grid.BeginUpdate;
  try
    // Limpia selección ANTES de tocar RowCount
    if grid.RowCount > 0 then
      grid.Row := 0;

    // deja solo encabezado
    grid.RowCount := 1;

    r := 1;
    N := CurrentUser^.Trash.Top;
    while N <> nil do
    begin
      if (fil = '') or (Pos(fil, LowerCase(N^.Mail^.Asunto)) > 0) then
      begin
        grid.RowCount := r + 1;  // asegura fila disponible

        grid.Cells[0, r] := N^.Mail^.Asunto;
        grid.Cells[1, r] := N^.Mail^.Remitente;

        preview := Trim(N^.Mail^.Mensaje);
        if Length(preview) > 40 then SetLength(preview, 40);
        if preview <> '' then preview := preview + '...';
        grid.Cells[2, r] := preview;

        Inc(r);
      end;
      N := N^.Next;
    end;

    // Selecciona primera fila de datos (si hay)
    if grid.RowCount > 1 then
      grid.Row := 1
    else
      grid.Row := 0;
  finally
    grid.EndUpdate;
  end;
end;

procedure TfrmTrash.btnBuscarClick(Sender: TObject);
begin
  FUltimaBusqueda := Trim(edtBuscar.Text);

  // Evita cambiar Row si no hay filas
  if grid.RowCount > 0 then
    grid.Row := 0
  else
    grid.RowCount := 1; // garantiza encabezado

  LlenarGrid(FUltimaBusqueda);
end;


function TfrmTrash.IndexDesdeTopSeleccionado: Integer;
var
  selAsunto, selRem: string;
  N: PTrashNode;
  idx: Integer;
begin
  Result := -1;
  if (grid.RowCount <= 1) or (grid.Row < 1) then Exit; // nada seleccionado

  selAsunto := grid.Cells[0, grid.Row];
  selRem    := grid.Cells[1, grid.Row];

  N := CurrentUser^.Trash.Top;
  idx := 0;
  while N <> nil do
  begin
    if (AnsiCompareText(N^.Mail^.Asunto, selAsunto) = 0) and
       (AnsiCompareText(N^.Mail^.Remitente, selRem) = 0) then
    begin
      Result := idx; Exit;
    end;
    N := N^.Next; Inc(idx);
  end;
end;

procedure TfrmTrash.EliminarEnPosDesdeTop(IndexFromTop: Integer);
var
  Temp: TTrash;
  i: Integer;
  M: PMail;
begin
  if (IndexFromTop < 0) then Exit;

  // pila temporal para conservar orden mientras "saltamos" hasta el seleccionado
  InitTrash(Temp);

  for i := 1 to IndexFromTop do
    begin
      if IsTrashEmpty(CurrentUser^.Trash) then Exit;  // nada que mover
      PushTrash(Temp, PopTrash(CurrentUser^.Trash));
    end;

  // el seleccionado está ahora en el tope de la pila original
  M := PopTrash(CurrentUser^.Trash);
  if M <> nil then
    Dispose(M); // eliminación permanente

  // regresar los que apilamos temporalmente, preservando el orden original
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

  if (grid.RowCount <= 1) or (grid.Row < 1) then
  begin
    ShowMessage('Selecciona una fila.');
    Exit;
  end;

  k := IndexDesdeTopSeleccionado; // 0 = tope
  if k < 0 then
  begin
    ShowMessage('No se pudo ubicar el elemento seleccionado.');
    Exit;
  end;

  // Si tu TTrash tiene Count, valida rango:
  if (CurrentUser^.Trash.Count >= 0) and (k >= CurrentUser^.Trash.Count) then
  begin
    ShowMessage('Selección fuera de rango.'); Exit;
  end;

  if MessageDlg('Eliminar',
      '¿Eliminar permanentemente el correo seleccionado?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    grid.Row := 0;               // limpia selección ANTES de tocar RowCount
    EliminarEnPosDesdeTop(k);
    LlenarGrid(FUltimaBusqueda); // refresca
  end;
end;



procedure TfrmTrash.btnRegresarClick(Sender: TObject);
begin
  Hide;
  frmUserN.Show;
end;

end.

