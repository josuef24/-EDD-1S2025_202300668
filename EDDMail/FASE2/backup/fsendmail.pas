unit fSendMail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UAVL_Borradores, UDataAVL;

type

  { TfrmSendMail }

  TfrmSendMail = class(TForm)
      Button1: TButton;
    lblParaa: TLabel; lblAsunto: TLabel; lblMensaje: TLabel;
    edtPara: TEdit; edtAsunto: TEdit;
    lblWelcome: TLabel;
    memMensaje: TMemo;
    btnEnviar: TButton; btnCancelar: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);


    public
    BorradorIDEnEdicion: LongInt; // 0 si no viene de borrador
    procedure CargarDesdeBorrador(const D: TMailDraft; const IDBorrador: LongInt);
    procedure btnGuardarBorradorClick(Sender: TObject);



  end;

var
  frmSendMail: TfrmSendMail;

implementation

uses uUsers, uInbox, frmUser, uContacts, uMatrix, UAVL_Borradores, UDataAVL, FBorradores;

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
  btnGuardarBorrador.OnClick := @btnGuardarBorradorClick;
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
begin

var d: TMailDraft;
begin
  // Si estamos editando un borrador existente, primero lo quitamos para no duplicar
  if BorradorIDEnEdicion <> 0 then
    AVL_Delete(BorradoresRoot, BorradorIDEnEdicion);

  d.ID           := BorradorIDEnEdicion;
  if d.ID = 0 then d.ID := NextDraftID(); // nuevo ID si era un borrador nuevo
  d.Remitente    := CurrentUser^.Email;
  d.Destinatario := Trim(edtPara.Text);
  d.Asunto       := Trim(edtAsunto.Text);
  d.Mensaje      := memMensaje.Lines.Text;

  if AVL_Insert(BorradoresRoot, d) then begin
    ShowMessage('Borrador guardado (ID=' + IntToStr(d.ID) + ').');
    BorradorIDEnEdicion := d.ID; // quedamos apuntando al mismo
  end else
    ShowMessage('No se guardó (ID duplicado).');

end;


procedure TfrmMain.mnBorradoresClick(Sender: TObject);
begin
  if not Assigned(FormBorradores) then Application.CreateForm(TFormBorradores, FormBorradores);
  FormBorradores.Show;
  FormBorradores.btnRefrescar.Click;
end;

end.

