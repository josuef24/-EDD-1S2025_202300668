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
    btnReportes: TButton;
    btnCerrarSesion: TButton;
    lblWelcome: TLabel;
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnBandejaClick(Sender: TObject);
    procedure btnCargaMasiva1Click(Sender: TObject);
    procedure btnReportesClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnCerrarSesionClick(Sender: TObject);
    procedure btnPerfilClick(Sender: TObject);
    procedure btnProcesarProgramadosClick(Sender: TObject);
    procedure btnProgramarCorreoClick(Sender: TObject);
    procedure btnVerProgramadosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmUserN: TfrmUserN;

implementation

uses fLogin, fSendMail, fTrash, fInbox, uQueue, uUsers, fProgramarMail,
     fProgramados, fContacts, fAddContact, fPerfil, Process, uReportUserInbox,
     uReportUserTrash;

{$R *.lfm}

{ TfrmUserN }

var
  P: TProcess;
  OutFile: string;

function RunGraphviz(const DotPath: string; const OutFormat: string = 'png'): Boolean;


begin
  Result := False;
  if not FileExists(DotPath) then Exit;

  OutFile := ChangeFileExt(DotPath, '.' + OutFormat);

  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';               // requiere graphviz instalado
    P.Parameters.Add('-T' + OutFormat);  // png|svg|pdf
    P.Parameters.Add(DotPath);
    P.Parameters.Add('-o');
    P.Parameters.Add(OutFile);
    P.Options := [poWaitOnExit];
    P.ShowWindow := swoHIDE;
    P.Execute;
    Result := (P.ExitStatus = 0) and FileExists(OutFile);
  finally
    P.Free;
  end;
end;




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
  n := ProcessFIFO(CurrentUser^.Sched);
  ShowMessage(Format('Enviados: %d', [n]));
end;

procedure TfrmUserN.btnProgramarCorreoClick(Sender: TObject);
begin
  if not Assigned(frmProgramarMail) then
    Application.CreateForm(TfrmProgramarMail, frmProgramarMail);
  frmUserN.Hide;
  frmProgramarMail.Show;

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

const
  OUT_USER_DIR = 'User-Reportes';
var
  DotPath, PngPath: string;

procedure TfrmUserN.btnReportesClick(Sender: TObject);

begin
  if (CurrentUser = nil) then
  begin
    ShowMessage('No hay usuario activo.');
    Exit;
  end;

  // ===== Reporte: Bandeja (recibidos) =====
  if ExportInboxDOTForUser(CurrentUser^.Email, CurrentUser^.Inbox,
                           OUT_USER_DIR, DotPath) then
  begin
    if RunGraphviz(DotPath, 'png') then
    begin
      PngPath := ChangeFileExt(DotPath, '.png');
      ShowMessage('Reporte de correos recibidos generado: ' + PngPath);
    end
    else
      ShowMessage('Se creó el .dot del inbox, pero no pude ejecutar "dot" (Graphviz).');
  end
  else
    ShowMessage('No se pudo generar el .dot de la bandeja.');

  // ===== REPORTE: PAPELERA =====
  if ExportTrashDOTForUser(CurrentUser^.Email, CurrentUser^.Trash, OUT_USER_DIR, DotPath) then
  begin
    if RunGraphviz(DotPath, 'png') then
    begin
      PngPath := ChangeFileExt(DotPath, '.png');
      ShowMessage('Reporte de papelera generado: ' + PngPath);
    end
    else
      ShowMessage('Se creó el .dot de papelera, pero no pude ejecutar "dot" (Graphviz).');
  end
  else
    ShowMessage('No se pudo generar el .dot de la papelera.');

  // ===== Reporte: Programados (cola FIFO) =====
  if ExportSchedDOTForUser(CurrentUser^.Sched, CurrentUser^.Email,
                           OUT_USER_DIR, DotPath) then
  begin
    if RunGraphviz(DotPath, 'png') then
    begin
      PngPath := ChangeFileExt(DotPath, '.png');
      ShowMessage('Reporte de correos programados generado: ' + PngPath);
    end
    else
      ShowMessage('Se creó el .dot de Programados, pero no pude ejecutar "dot" (Graphviz).');
  end
  else
    ShowMessage('No se pudo generar el .dot de Programados.');
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

