unit fSendMail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UAVL_Borradores, UDataAVL;

type

  { TfrmSendMail }

  TfrmSendMail = class(TForm)
      btnGuardarBorrador: TButton;
    lblParaa: TLabel; lblAsunto: TLabel; lblMensaje: TLabel;
    edtPara: TEdit; edtAsunto: TEdit;
    lblWelcome: TLabel;
    memMensaje: TMemo;
    btnEnviar: TButton; btnCancelar: TButton;
    procedure btnGuardarBorradorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);


    public
    BorradorIDEnEdicion: LongInt; // 0 si no viene de borrador
    procedure CargarDesdeBorrador(const D: TMailDraft; const IDBorrador: LongInt);



  end;

var
  frmSendMail: TfrmSendMail;

implementation

uses uUsers, uInbox, frmUser, uContacts, uMatrix, FBorradores;

{$R *.lfm}

procedure TfrmSendMail.FormCreate(Sender: TObject);
begin
  Caption := 'Enviar Correo';
  lblParaa.Caption    := 'Para (usuario o email):';
  lblAsunto.Caption  := 'Asunto:';
  lblMensaje.Caption := 'Mensaje:';
  btnEnviar.Caption  := 'Enviar';
  btnCancelar.Caption:= 'Cancelar';

  BorradorIDEnEdicion := 0;
  if Assigned(frmSendMail) then
  frmSendMail.OnClick := @btnGuardarBorradorClick;


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

  // Registrar relación remitente -> destinatario
  IncrementEdge(RelMatrix, CurrentUser, dest);

  //borrar de borradores
   if BorradorIDEnEdicion <> 0 then
   begin
      AVL_Delete(BorradoresRoot, BorradorIDEnEdicion);
      BorradorIDEnEdicion := 0;
   end;

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

procedure TfrmSendMail.CargarDesdeBorrador(const D: TMailDraft; const IDBorrador: LongInt);
begin
  edtPara.Text    := D.Destinatario;
  edtAsunto.Text  := D.Asunto;
  memMensaje.Text := D.Mensaje;
  // El remitente es el usuario actual; no lo pedimos en UI
  BorradorIDEnEdicion := IDBorrador;
end;

procedure TfrmSendMail.btnGuardarBorradorClick(Sender: TObject);

var
  D: TMailDraft;

begin

  if BorradorIDEnEdicion <> 0 then
    AVL_Delete(BorradoresRoot, BorradorIDEnEdicion);

  if BorradorIDEnEdicion = 0 then
    BorradorIDEnEdicion := NextDraftID();

  D.ID           := BorradorIDEnEdicion;
  D.Remitente    := CurrentUser^.Email;
  D.Destinatario := Trim(edtPara.Text);
  D.Asunto       := Trim(edtAsunto.Text);
  D.Mensaje      := memMensaje.Lines.Text;

  if AVL_Insert(BorradoresRoot, D) then
    ShowMessage('Borrador guardado (ID=' + IntToStr(D.ID) + ').')
  else
    ShowMessage('No se guardó (ID duplicado).');

  ShowMessage('Guardado. Total borradores = ' + IntToStr(AVL_Count(BorradoresRoot)));

  if Assigned(FormBorradores) then
  FormBorradores.btnRefrescarClick(FormBorradores);


end;




end.

