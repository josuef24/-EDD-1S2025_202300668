unit frmUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uInbox;



type

  { TfrmUserN }

  TfrmUserN = class(TForm)
    btnBandeja: TButton;
    btnCargaMasiva1: TButton;
    btnPapelera: TButton;
    btnProgramarCorreo: TButton;
    btnVerProgramados: TButton;
    btnAgregarContacto: TButton;
    btnContactos: TButton;
    btnPerfil: TButton;
    btnCargaMasiva8: TButton;
    btnCerrarSesion: TButton;
    lblWelcome: TLabel;
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnBandejaClick(Sender: TObject);
    procedure btnCargaMasiva1Click(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnCerrarSesionClick(Sender: TObject);
    procedure btnPerfilClick(Sender: TObject);
    procedure btnProcesarProgramadosClick(Sender: TObject);
    procedure btnProgramarCorreoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmUserN: TfrmUserN;

implementation

uses fLogin, fSendMail, fTrash, fInbox, uQueue, uUsers, fProgramarMail,
     fProgramados, fContacts, fAddContact, fPerfil;

{$R *.lfm}

{ TfrmUserN }



procedure TfrmUserN.btnCerrarSesionClick(Sender: TObject);
begin
  if not Assigned(frmLogin) then
     Application.CreateForm(TfrmLogin, frmLogin);
  frmLogin.Show;
  Self.Hide;
end;

procedure TfrmUserN.btnPerfilClick(Sender: TObject);
begin
  if not Assigned(frmPerfil) then
    Application.CreateForm(TfrmPerfil, frmPerfil);
  frmPerfil.Show;
  frmUserN.Hide;
end;

procedure TfrmUserN.btnProcesarProgramadosClick(Sender: TObject);
  var
  n: Integer;
begin
  n := ProcessDue(CurrentUser^.Sched);
  ShowMessage(Format('Enviados: %d', [n]));
end;

procedure TfrmUserN.btnProgramarCorreoClick(Sender: TObject);
begin
  if not Assigned(frmProgramarMail) then
    Application.CreateForm(TfrmProgramarMail, frmProgramarMail);
  frmUserN.Hide;
  frmProgramarMail.Show;

end;

procedure TfrmUserN.btnBandejaClick(Sender: TObject);
begin
  if not Assigned(frmInbox) then
    Application.CreateForm(TfrmInbox, frmInbox);
  frmUserN.Hide;
  frmInbox.Show;
end;

procedure TfrmUserN.btnAgregarContactoClick(Sender: TObject);
begin
  frmUserN.Hide;
  frmAddContact.Show;
end;

procedure TfrmUserN.btnCargaMasiva1Click(Sender: TObject);
begin
  if not Assigned(frmSendMail) then
    Application.CreateForm(TfrmSendMail, frmSendMail);
  frmUserN.Hide;
  frmSendMail.Show;
end;

procedure TfrmUserN.btnContactosClick(Sender: TObject);
begin
  if not Assigned(frmContacts) then
    Application.CreateForm(TfrmContacts, frmContacts);
  frmUserN.Hide;
  frmContacts.Show;
end;

procedure TfrmUserN.btnPapeleraClick(Sender: TObject);
begin
  if not Assigned(frmTrash) then
    Application.CreateForm(TfrmTrash, frmTrash);
  frmUserN.Hide;
  frmTrash.Show;
end;

procedure TfrmUserN.FormCreate(Sender: TObject);
begin

end;

end.

