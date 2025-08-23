unit fInbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Math;

type

  { TfrmInbox }

  TfrmInbox = class(TForm)
    lstMails: TListBox;      // lista (Estado | Asunto | Remitente)
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
    procedure lstMailsDblClick(Sender: TObject);
  private
    procedure RefrescarLista;
    function  IndexValido: Boolean;
  end;

var
  frmInbox: TfrmInbox;

implementation

uses
  uInbox, frmUser, Uusers, fViewmail;  // Inbox global y regreso a menú de usuario

{$R *.lfm}

procedure TfrmInbox.FormShow(Sender: TObject);
begin
  RefrescarLista;
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
  N := CurrentUser^.Inbox.Head;
  while N <> nil do
  begin
    // Estado como ‘NL’/‘L’ + Asunto + Remitente
    linea := Format('%s | %s | %s', [N^.Estado, N^.Asunto, N^.Remitente]);
    lstMails.Items.Add(linea);
    N := N^.Next;
  end;

  // actualizar contador de no leídos
  lblUnread.Caption := 'No leídos: ' + IntToStr(CountUnread(CurrentUser^.Inbox));

  // selección por defecto (opcional)
  if lstMails.Count > 0 then
    lstMails.ItemIndex := 0;
end;

procedure TfrmInbox.lstMailsClick(Sender: TObject);
var
  idx: Integer;
  mail: PMail;

begin
  idx := lstMails.ItemIndex;
  if idx < 0 then Exit;

  mail := GetMailByIndex(CurrentUser^.Inbox, idx);
  if mail <> nil then
  begin
    if not Assigned(frmViewMail) then
      Application.CreateForm(TfrmViewMail, frmViewMail);
    frmViewMail.ShowMail(mail);
    frmViewMail.Show;
  end;

end;

procedure TfrmInbox.btnOrdenarAZClick(Sender: TObject);
begin
  SortBySubject(CurrentUser^.Inbox);
  RefrescarLista;
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

  node := GetMailByIndex(CurrentUser^.Inbox, lstMails.ItemIndex);
  if node = nil then Exit;

  // sacar de la lista (luego lo enviaremos a la Pila/Papelera)
  DetachMail(CurrentUser^.Inbox, node);
  Dispose(node); // de momento lo liberamos; en “Papelera” lo apilaremos

  RefrescarLista;
end;

procedure TfrmInbox.btnRegresarClick(Sender: TObject);
begin
  frmInbox.Hide;
  frmUserN.Show;
end;

procedure TfrmInbox.lstMailsDblClick(Sender: TObject);
var
  node: PMail;
begin
  if (lstMails.ItemIndex < 0) then Exit;
  node := GetMailByIndex(CurrentUser^.Inbox, lstMails.ItemIndex);
  if node = nil then Exit;

  if not Assigned(frmViewMail) then
    Application.CreateForm(TfrmViewMail, frmViewMail);

  frmViewMail.ShowMail(node);
  // refrescamos lista para que el estado pase a L
  RefrescarLista;
end;

function TfrmInbox.IndexValido: Boolean;
begin
  Result := (lstMails.ItemIndex >= 0) and (lstMails.ItemIndex < lstMails.Count);
end;

end.

