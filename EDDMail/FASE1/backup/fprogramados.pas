unit fProgramados;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uQueue;

type

  { TfrmProgramados }

  TfrmProgramados = class(TForm)
    lblTitulo: TLabel;
    lstQueue: TListBox;
    btnProcesar: TButton;
    btnRegresar: TButton;
    procedure FormCreate(Sender: TObject);
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
  uUsers, uInbox, frmUser;

{$R *.lfm}

procedure TfrmProgramados.LlenarLista;
var
  P: PSchedItem;   // <-- antes ponías PSchedNode
  linea: string;
begin
  lstQueue.Items.BeginUpdate;
  try
    lstQueue.Clear;

    if (CurrentUser = nil) then Exit;

    // cabeza de la cola FIFO del usuario actual
    P := CurrentUser^.Sched.Head;
    while P <> nil do
    begin
      // En cola nueva: Fecha es string (no DateTime), así que la mostramos directo
      linea := Format('%s | %s | %s', [P^.Dest, P^.Asunto, P^.Fecha]);
      lstQueue.Items.Add(linea);
      P := P^.Next;
    end;
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

procedure TfrmProgramados.FormCreate(Sender: TObject);
begin

end;

procedure TfrmProgramados.btnProcesarClick(Sender: TObject);
var
  enviados: Integer;
begin
  if (CurrentUser = nil) then Exit;

  enviados := ProcessFIFO(CurrentUser^.Sched);

  ShowMessage(Format('Enviados: %d', [enviados]));
  LlenarLista; // refresca la cola
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

