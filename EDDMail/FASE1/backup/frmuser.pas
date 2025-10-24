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
    btnBorradores: TButton;
    btnCargaMasiva1: TButton;
    btnEliminarContacto: TButton;
    btnPapelera: TButton;
    btnProgramarCorreo: TButton;
    btnVerProgramados: TButton;
    btnAgregarContacto: TButton;
    btnContactos: TButton;
    btnPerfil: TButton;
    btnReportes: TButton;
    btnCerrarSesion: TButton;
    btnFavoritos: TButton;
    btnMensajeComunidad: TButton;
    lblWelcome: TLabel;
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnBandejaClick(Sender: TObject);
    procedure btnBorradoresClick(Sender: TObject);
    procedure btnCargaMasiva1Click(Sender: TObject);
    procedure btnEliminarContactoClick(Sender: TObject);
    procedure btnMensajeComunidadClick(Sender: TObject);
    procedure btnReportesClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnCerrarSesionClick(Sender: TObject);
    procedure btnPerfilClick(Sender: TObject);
    procedure btnProcesarProgramadosClick(Sender: TObject);
    procedure btnProgramarCorreoClick(Sender: TObject);
    procedure btnVerProgramadosClick(Sender: TObject);
    procedure btnFavoritosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  frmUserN: TfrmUserN;

implementation

uses fLogin, fSendMail, fTrash, fInbox, uQueue, uUsers, fProgramarMail,
     fProgramados, fContacts, fAddContact, fPerfil, Process,
     uReportUserTrash, FBorradores, FFavoritos, UDataBT_Fav,
     uDraftsReport_AVL, UDataAVL, uFavReport_BTree, fMensajeComunidad;

{$R *.lfm}

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
    P.Executable := 'dot';
    P.Parameters.Add('-T' + OutFormat);
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

procedure TfrmUserN.btnBorradoresClick(Sender: TObject);
begin
  if not Assigned(FormBorradores) then
     Application.CreateForm(TFormBorradores, FormBorradores);
  FormBorradores.Show;
  FormBorradores.btnRefrescar.Click;
  frmUserN.Hide;
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

procedure TfrmUserN.btnEliminarContactoClick(Sender: TObject);
begin
  if not Assigned(frmDelContact) then
    Application.CreateForm(TfrmDelContact, frmDelContact);
  frmDelContact.Show;
  frmUserN.Hide;
end;

procedure TfrmUserN.btnMensajeComunidadClick(Sender: TObject);
begin
  if not Assigned(frmMensajeComunidad) then
    Application.CreateForm(TfrmMensajeComunidad, frmMensajeComunidad);
  frmUserN.Hide;
  frmMensajeComunidad.Show;
end;

procedure TfrmUserN.btnFavoritosClick(Sender: TObject);
begin
  FavInit;

  if not Assigned(FormFavoritos) then
    Application.CreateForm(TFormFavoritos, FormFavoritos);

  frmUserN.Hide;
  FormFavoritos.Show;
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

  if ExportContactsDOTForUser(CurrentUser^.Contacts, CurrentUser^.Email, OUT_USER_DIR, DotPath) then
  begin
    if RunGraphviz(DotPath, 'png') then
    begin
      PngPath := ChangeFileExt(DotPath, '.png');
    end
    else
      ShowMessage('Se creó el .dot de contactos, pero no pude ejecutar Graphviz (dot).');
  end
  else
    ShowMessage('No se pudo generar el .dot de contactos.');

  if ExportDraftsAVL_DOT(BorradoresRoot, OUT_USER_DIR, DotPath) then
  begin
    if RunGraphviz(DotPath, 'png') then
    begin
      PngPath := ChangeFileExt(DotPath, '.png');
      ShowMessage('Reporte de borradores (AVL) generado: ' + PngPath);
    end
    else
      ShowMessage('Se creó el .dot de borradores, pero no pude ejecutar "dot" (Graphviz).');
  end
  else
    ShowMessage('No se pudo generar el .dot de borradores.');

   if ExportFavBTreeDOT(FavRoot, OUT_USER_DIR, DotPath) then
  begin
    if RunGraphviz(DotPath, 'png') then
    begin
      PngPath := ChangeFileExt(DotPath, '.png');
      ShowMessage('Reporte de favoritos (Árbol B) generado: ' + PngPath);
    end
    else
      ShowMessage('Se creó el .dot de favoritos, pero no pude ejecutar "dot" (Graphviz).');
  end
  else
    ShowMessage('No se pudo generar el .dot de favoritos.');

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
  FavInit;
end;

end.
