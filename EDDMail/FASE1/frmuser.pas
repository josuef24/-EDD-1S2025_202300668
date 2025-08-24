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
    btnCargaMasiva5: TButton;
    btnCargaMasiva6: TButton;
    btnCargaMasiva7: TButton;
    btnCargaMasiva8: TButton;
    btnCerrarSesion: TButton;
    btnPruebaProgramar: TButton;
    btnProcesarProgramados: TButton;
    lblWelcome: TLabel;
    procedure btnBandejaClick(Sender: TObject);
    procedure btnCargaMasiva1Click(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnCerrarSesionClick(Sender: TObject);
    procedure btnProcesarProgramadosClick(Sender: TObject);
    procedure btnProgramarCorreoClick(Sender: TObject);
    procedure btnPruebaProgramarClick(Sender: TObject);
    procedure btnVerProgramadosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmUserN: TfrmUserN;

implementation

uses fLogin, fSendMail, fTrash, fInbox, uQueue, uUsers, fProgramarMail, fProgramados;

{$R *.lfm}

{ TfrmUserN }



procedure TfrmUserN.btnCerrarSesionClick(Sender: TObject);
begin
  if not Assigned(frmLogin) then
     Application.CreateForm(TfrmLogin, frmLogin);
  frmLogin.Show;
  Self.Hide;
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
  Hide;
  frmProgramarMail.Show;

end;

procedure TfrmUserN.btnPruebaProgramarClick(Sender: TObject);
var
  sendTime: TDateTime;
begin
  // 30 segundos en el futuro
  sendTime := Now + EncodeTime(0,0,30,0);

  EnqueueMail(CurrentUser^.Sched,
              CurrentUser^.Email,      // remitente
              'user@edd.com',          // destinatario (email o usuario existente)
              'Asunto programado',
              'Este mensaje saldrá en 30s',
              sendTime);
  ShowMessage('Programado para: ' + DateTimeToStr(sendTime));
end;

procedure TfrmUserN.btnVerProgramadosClick(Sender: TObject);
begin
  if not Assigned(frmProgramados) then
    Application.CreateForm(TfrmProgramados, frmProgramados);
  frmUserN.Hide;
  frmProgramados.Show;
end;

procedure TfrmUserN.btnBandejaClick(Sender: TObject);
begin
  if not Assigned(frmInbox) then
    Application.CreateForm(TfrmInbox, frmInbox);
  frmUserN.Hide;
  frmInbox.Show;
end;

procedure TfrmUserN.btnCargaMasiva1Click(Sender: TObject);
begin
  if not Assigned(frmSendMail) then
    Application.CreateForm(TfrmSendMail, frmSendMail);
  frmUserN.Hide;
  frmSendMail.Show;
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

