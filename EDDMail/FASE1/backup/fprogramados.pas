unit fProgramados;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmProgramados }

  TfrmProgramados = class(TForm)
    lblTitulo: TLabel;
    lstQueue: TListBox;
    btnProcesar: TButton;
    btnRegresar: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnProcesarClick(Sender: TObject);
    procedure btnRegresarClick(Sender: TObject);
    procedure lstQueueClick(Sender: TObject);
  private
    procedure LlenarLista;
  public
  end;

var
  frmProgramados: TfrmProgramados;

implementation

uses
  uUsers, uQueue, uInbox, frmUser; // <- importante: uQueue y uUsers

{$R *.lfm}

procedure TfrmProgramados.LlenarLista;
var
  P: PSchedNode;
  linea: string;
begin
  lstQueue.Items.BeginUpdate;
  try
    lstQueue.Clear;

    if (CurrentUser = nil) then Exit;

    P := CurrentUser^.Sched.Head;  // cabeza de la cola (FIFO)
    while P <> nil do
    begin
      linea := Format('%s | %s | %s',
        [FormatDateTime('yyyy-mm-dd hh:nn', P^.SendAt),
         P^.DestKey,
         P^.Asunto]);
      lstQueue.Items.Add(linea);
      P := P^.Next;
    end;

    if lstQueue.Count > 0 then
      lstQueue.ItemIndex := 0;
  finally
    lstQueue.Items.EndUpdate;
  end;
end;

procedure TfrmProgramados.FormShow(Sender: TObject);
begin
  Caption := 'Correos Programados';
  lblTitulo.Caption := 'Correos Programados';
  LlenarLista;
end;

procedure TfrmProgramados.btnProcesarClick(Sender: TObject);
var
  enviados: Integer;
begin
  if (CurrentUser = nil) then Exit;

  // Procesa los que ya vencieron (<= Now)
  enviados := ProcessDue(CurrentUser^.Sched);

  ShowMessage(Format('Enviados: %d', [enviados]));
  LlenarLista; // refresca la cola (algunos se habrán ido)
end;

procedure TfrmProgramados.btnRegresarClick(Sender: TObject);
begin
  Hide;
  frmUserN.Show;
end;

procedure TfrmProgramados.lstQueueClick(Sender: TObject);
begin

end;

end.

