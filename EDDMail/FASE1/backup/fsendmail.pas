unit fSendMail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmSendMail }

  TfrmSendMail = class(TForm)
    lblParaa: TLabel; lblAsunto: TLabel; lblMensaje: TLabel;
    edtPara: TEdit; edtAsunto: TEdit;
    memMensaje: TMemo;
    btnEnviar: TButton; btnCancelar: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  end;

var
  frmSendMail: TfrmSendMail;

implementation

uses uUsers, uInbox, frmUser, uContacts;   // CurrentUser, AddMail, y volver al menú

{$R *.lfm}

procedure TfrmSendMail.FormCreate(Sender: TObject);
begin
  Caption := 'Enviar Correo';
  lblParaa.Caption    := 'Para (usuario o email):';
  lblAsunto.Caption  := 'Asunto:';
  lblMensaje.Caption := 'Mensaje:';
  btnEnviar.Caption  := 'Enviar';
  btnCancelar.Caption:= 'Cancelar';
end;

procedure TfrmSendMail.btnEnviarClick(Sender: TObject);
var
  key, asunto, cuerpo, fecha: AnsiString;
  dest: PUser;
begin

  key    := Trim(edtPara.Text);
  asunto := Trim(edtAsunto.Text);
  cuerpo := Trim(memMensaje.Lines.Text);

  if (key='') or (asunto='') or (cuerpo='') then
  begin
    ShowMessage('Completa Para, Asunto y Mensaje.');
    Exit;
  end;

  dest := FindUserByEmailOrUsername(key);
  if dest = nil then
  begin
    ShowMessage('Destinatario no encontrado.');
    Exit;
  end;

    // Solo permitido si está en contactos del usuario actual
  if not ExistsInContacts(CurrentUser^.Contacts, dest^.Email)
     and not ExistsInContacts(CurrentUser^.Contacts, dest^.Username) then
  begin
    ShowMessage('Solo puedes enviar a tus contactos.');
    Exit;
  end;

  if SameText(dest^.Email, CurrentUser^.Email) then
  begin
    ShowMessage('No puedes enviarte correos a ti mismo.');
    Exit;
  end;


  // fecha como texto simple
  fecha := FormatDateTime('yyyy-mm-dd hh:nn', Now);

  // Insertar en la bandeja del destinatario
  AddMail(dest^.Inbox,
          CurrentUser^.Email,  // remitente = el usuario logueado
          asunto,
          fecha,
          cuerpo,
          False);              // no programado

  ShowMessage('Correo enviado a ' + dest^.Email);
  Close;   // o Hide; y volver al menú
  frmUserN.Show;
end;

procedure TfrmSendMail.btnCancelarClick(Sender: TObject);
begin
  edtPara.Clear;
  edtAsunto.Clear;
  memMensaje.Clear;

  frmSendMail.Hide;
  frmUserN.Show;
end;

end.

