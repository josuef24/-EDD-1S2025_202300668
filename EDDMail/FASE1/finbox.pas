unit fInbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Math;

type

  { TfrmInbox }

  TfrmInbox = class(TForm)
    lstMails: TListBox;      // lista (Estado | Asunto | Remitente)
    memBody: TMemo;          // mensaje seleccionado
    lblUnread: TLabel;       // “No leídos: N”
    btnOrdenarAZ: TButton;   // ordena por Asunto A–Z
    btnEliminar: TButton;    // elimina de la bandeja
    btnRegresar: TButton;    // vuelve al menú de usuario

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstMailsClick(Sender: TObject);
    procedure btnOrdenarAZClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnRegresarClick(Sender: TObject);
  private
    procedure RefrescarLista;
    procedure MostrarSeleccion;
    function  IndexValido: Boolean;
  end;

var
  frmInbox: TfrmInbox;

implementation

uses
  uInbox, frmUser;  // Inbox global y regreso a menú de usuario

{$R *.lfm}

procedure TfrmInbox.FormShow(Sender: TObject);
begin
  RefrescarLista;
  memBody.Clear;
end;

procedure TfrmInbox.FormCreate(Sender: TObject);
begin

end;

procedure TfrmInbox.RefrescarLista;
var
  i: Integer;
  N: PMail;
  linea: String;
begin
  lstMails.Clear;

  // recorrer lista doblemente enlazada
  N := Inbox.Head;
  while N <> nil do
  begin
    // Estado como ‘NL’/‘L’ + Asunto + Remitente
    linea := Format('%s | %s | %s', [N^.Estado, N^.Asunto, N^.Remitente]);
    lstMails.Items.Add(linea);
    N := N^.Next;
  end;

  // actualizar contador de no leídos
  lblUnread.Caption := 'No leídos: ' + IntToStr(CountUnread(Inbox));

  // selección por defecto (opcional)
  if lstMails.Count > 0 then
    lstMails.ItemIndex := 0;
end;

procedure TfrmInbox.MostrarSeleccion;
var
  node: PMail;
begin
  if not IndexValido then Exit;

  node := GetMailByIndex(Inbox, lstMails.ItemIndex);
  if node = nil then Exit;

  // mostrar cuerpo
  memBody.Lines.Text :=
    'Asunto: ' + node^.Asunto + LineEnding +
    'De: '     + node^.Remitente + LineEnding +
    'Fecha: '  + node^.Fecha + LineEnding + LineEnding +
    node^.Mensaje;

  // marcar leído y refrescar estado/contador
  MarkRead(node);
  RefrescarLista;
  // mantener resaltado el seleccionado
  if lstMails.Count > 0 then
    lstMails.ItemIndex := Min(lstMails.Count-1, lstMails.ItemIndex);
end;

procedure TfrmInbox.lstMailsClick(Sender: TObject);
begin
  MostrarSeleccion;
end;

procedure TfrmInbox.btnOrdenarAZClick(Sender: TObject);
begin
  SortBySubject(Inbox);
  RefrescarLista;
  memBody.Clear;
end;

procedure TfrmInbox.btnEliminarClick(Sender: TObject);
var
  node: PMail;
begin
  if not IndexValido then
  begin
    ShowMessage('Selecciona un correo.');
    Exit;
  end;

  node := GetMailByIndex(Inbox, lstMails.ItemIndex);
  if node = nil then Exit;

  // sacar de la lista (luego lo enviaremos a la Pila/Papelera)
  DetachMail(Inbox, node);
  Dispose(node); // de momento lo liberamos; en “Papelera” lo apilaremos

  memBody.Clear;
  RefrescarLista;
end;

procedure TfrmInbox.btnRegresarClick(Sender: TObject);
begin
  frmInbox.Hide;
  frmUserN.Show;
end;

function TfrmInbox.IndexValido: Boolean;
begin
  Result := (lstMails.ItemIndex >= 0) and (lstMails.ItemIndex < lstMails.Count);
end;

end.

