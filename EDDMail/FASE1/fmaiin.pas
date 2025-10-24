unit fMaiin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uUsers, UDataMail;

type

  { TfrmRoot }

  TfrmRoot = class(TForm)
  published
    btnExportRel: TButton;
    btnExportUsers: TButton;
    btnComunidades: TButton;
    btnExportComunidades: TButton;
    btnSalir: TButton;
    btnCargaMasiva: TButton;
    btnCargaCorreos: TButton;
    Label1: TLabel;
    procedure btnCargaCorreosClick(Sender: TObject);
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnComunidadesClick(Sender: TObject);
    procedure btnExportComunidadesClick(Sender: TObject);
    procedure btnExportRelClick(Sender: TObject);
    procedure btnExportUsersClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure Button1Click(Sender: TObject);
    procedure ActionCrearComunidad;
    procedure ActionAgregarUsuarioAComunidad;
    procedure ActionExportarComunidades;

  private
    FImp, FDup: Integer;
    procedure AddCorreo(const C: TCorreo);
  end;


var
  frmRoot: TfrmRoot;

const
  OUT_DIR = 'Root-Reportes';

implementation

uses fLogin, uMatrix, UComunidades, UComunidadesAdapters, fComunidades,
     UMailLoader_JSON, UCorreoStore, Math, uInbox, UComunidadesBST,
     UDataComunidadesBST, UComReport_BST;

procedure ImportCorreosStoreToUsers; forward;

{$R *.lfm}

procedure TfrmRoot.FormCreate(Sender: TObject);
begin
  // Inicializa la lista de Comunidades al abrir el root
  InitComunidades;
end;

procedure TfrmRoot.Button1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmRoot.btnCargaMasivaClick(Sender: TObject);
var
  dlg: TOpenDialog;
  ok: Boolean;
  imp, dup, err: Integer;
begin
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Title  := 'Seleccionar archivo JSON de usuarios';
    dlg.Filter := 'Archivos JSON|*.json|Todos|*.*';
    if not dlg.Execute then Exit;

    ok := LoadUsersFromJSON(dlg.FileName, imp, dup, err);
    if not ok then
    begin
      ShowMessage('No se pudo leer el archivo o el formato es inválido.');
      Exit;
    end;

    ShowMessage(Format('Carga completada.'#10'Importados: %d'#10'Duplicados: %d'#10'Errores: %d',
                       [imp, dup, err]));
  finally
    dlg.Free;
  end;
end;



procedure TfrmRoot.btnComunidadesClick(Sender: TObject);
begin
  if not Assigned(frmComunidades) then
    Application.CreateForm(TfrmComunidades, frmComunidades);
  frmRoot.Hide;
  frmComunidades.Show;
end;

procedure TfrmRoot.btnExportComunidadesClick(Sender: TObject);
begin
  ActionExportarComunidades;
end;

var
  DotPath, PngPath: string;

function RunGraphviz(const DotPath: string; const OutFormat: string = 'png'): Boolean;
var
  P: TProcess;
  OutFile: string;
begin
  Result := False;
  if not FileExists(DotPath) then Exit;

  OutFile := ChangeFileExt(DotPath, '.' + OutFormat);

  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';              // requiere Graphviz instalado
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

procedure TfrmRoot.btnExportRelClick(Sender: TObject);
begin
  if ExportRelationsDOT(OUT_DIR) then
  begin
    DotPath := IncludeTrailingPathDelimiter(OUT_DIR) + 'relaciones.dot';

    if RunGraphviz(DotPath, 'png') then
    begin
      PngPath := ChangeFileExt(DotPath, '.png');
      ShowMessage('Reporte creado: ' + PngPath);
      // OpenDocument(OUT_DIR); // si quisieras abrir la carpeta (requiere LCLIntf en uses)
    end
    else
      ShowMessage('Se generó el .dot, pero no pude ejecutar Graphviz (dot). Verifica que esté instalado.');
  end
  else
    ShowMessage('No se pudo generar el .dot de relaciones.');
end;

procedure TfrmRoot.btnExportUsersClick(Sender: TObject);
begin
  if ExportUsersDOT(OUT_DIR) then
  begin
    DotPath := IncludeTrailingPathDelimiter(OUT_DIR) + 'usuarios.dot';
    if RunGraphviz(DotPath, 'png') then
    begin
      PngPath := ChangeFileExt(DotPath, '.png');
      ShowMessage('Reporte creado: ' + PngPath);
    end
    else
      ShowMessage('Se generó el .dot, pero no pude ejecutar Graphviz (dot). Verifica la instalación.');
  end
  else
    ShowMessage('No se pudo generar el .dot de usuarios.');
end;

procedure TfrmRoot.ActionCrearComunidad;
var
  nom: String;
begin
  nom := '';
  if InputQuery('Crear Comunidad', 'Nombre de la comunidad:', nom) and (Trim(nom) <> '') then
  begin
    nom := Trim(nom);

    // ¿ya existe en el BST?
    if BST_Find(ComunidadesRoot, nom) <> nil then
    begin
      ShowMessage('La comunidad "'+nom+'" ya existe.');
      Exit;
    end;

    // crear en el BST (fecha = ahora)
    BST_Insert(ComunidadesRoot, nom);

    ShowMessage('Comunidad "'+nom+'" creada en el árbol BST.');
  end
  else
    ShowMessage('Operación cancelada o nombre vacío.');
end;


procedure TfrmRoot.ActionAgregarUsuarioAComunidad;
var
  nomCom, uid, uname: String;
begin
  nomCom := ''; uid := ''; uname := '';

  if not (InputQuery('Agregar a Comunidad', 'Nombre de la comunidad:', nomCom) and (Trim(nomCom)<>'')) then
  begin
    ShowMessage('Operación cancelada o nombre vacío.'); Exit;
  end;

  if not (InputQuery('Agregar a Comunidad', 'ID de usuario (String):', uid) and (Trim(uid)<>'')) then
  begin
    ShowMessage('Operación cancelada o ID vacío.'); Exit;
  end;

  if not (InputQuery('Agregar a Comunidad', 'Nombre/Username del usuario:', uname) and (Trim(uname)<>'')) then
  begin
    ShowMessage('Operación cancelada o nombre vacío.'); Exit;
  end;

  if AddUserToCommunity_StrId(Trim(nomCom), Trim(uid), Trim(uname)) then
    ShowMessage('Usuario agregado a "'+Trim(nomCom)+'".')
  else
    ShowMessage('No se pudo agregar (¿ya existe en esa comunidad?).');
end;

procedure TfrmRoot.ActionExportarComunidades;
const
  Carpeta = OUT_DIR;
var
  rutaDot, rutaPng: String;
begin
  if ExportComunidadesBST_DOT(ComunidadesRoot, Carpeta, rutaDot) then
  begin
    if RunGraphviz(rutaDot, 'png') then
    begin
      rutaPng := ChangeFileExt(rutaDot, '.png');
      ShowMessage('Reporte creado: ' + rutaPng);
    end
    else
      ShowMessage('DOT generado: ' + rutaDot + sLineBreak +
                  'No pude ejecutar Graphviz (dot).');
  end
  else
    ShowMessage('No se pudo generar el .dot del BST de comunidades.');
end;

procedure TfrmRoot.btnSalirClick(Sender: TObject);
begin
  frmRoot.Hide;
  frmLogin.Show;
end;

procedure TfrmRoot.AddCorreo(const C: TCorreo);
begin
  if MailStore_AddIfNew(C) then
    Inc(FImp)
  else
    Inc(FDup);
end;

procedure ImportCorreosStoreToUsers;
var
  i: Integer;
  C: TCorreo;
  U: PUser;  // tu tipo de usuario en uUsers
begin
  for i := 0 to MailStore_Count - 1 do
  begin
    if not MailStore_Get(i, C) then Continue;

    // buscar destinatario en tus usuarios
    U := FindUserByEmail(C.Destinatario);   // asegúrate de tener esta función en uUsers
    if U = nil then Continue;               // si no existe, lo saltamos

    // insertar en su Inbox (sin duplicar)
    InboxAppendFromJSON(U^.Inbox,
                        C.ID,
                        C.Remitente,
                        C.Destinatario,
                        C.Estado,       // "NL" en tu JSON
                        C.Asunto,
                        '',             // Fecha (si no viene en JSON)
                        C.Mensaje);
  end;
end;

procedure TfrmRoot.btnCargaCorreosClick(Sender: TObject);
var
  dlg  : TOpenDialog;
  total: Integer;
  ok   : Boolean;
begin
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Title  := 'Seleccionar archivo JSON de correos';
    dlg.Filter := 'Archivos JSON|*.json|Todos|*.*';
    if not dlg.Execute then Exit;

    FImp := 0; FDup := 0;
    // MailStore_Clear; // opcional

    ok := LoadCorreosJSON(dlg.FileName, @AddCorreo, total);

    if ok then
    begin
      ShowMessage(Format('Carga completada.'#10'Leídos: %d'#10'Importados: %d'#10'Duplicados: %d',
                         [total, FImp, FDup]));
      ImportCorreosStoreToUsers;  // ← aquí, DENTRO del if ok
    end
    else
      ShowMessage('No se pudo leer el archivo o el formato es inválido.');
  finally
    dlg.Free;                     // ← cierra el try..finally
  end;
end;

end.

