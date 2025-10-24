unit fMaiin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uUsers;

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
    Label1: TLabel;
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
  end;

var
  frmRoot: TfrmRoot;

const
  OUT_DIR = 'Root-Reportes';

implementation

uses fLogin, uMatrix, UComunidades, UComunidadesAdapters, fComunidades;

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
    CrearComunidad(Trim(nom));
    ShowMessage('Comunidad "'+Trim(nom)+'" creada/lista.');
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
  Carpeta = OUT_DIR;              // ya tienes OUT_DIR = 'Root-Reportes'
  Archivo = 'Reporte_Comunidades.dot';
var
  rutaDot, rutaPng: String;
begin
  ExportarReporteComunidadesDOT(Carpeta, Archivo);
  rutaDot := IncludeTrailingPathDelimiter(Carpeta) + Archivo;
  rutaPng := ChangeFileExt(rutaDot, '.png');

  // Reusa tu runner para Graphviz:
  if RunGraphviz(rutaDot, 'png') then
    ShowMessage('Reporte creado: ' + rutaPng)
  else
    ShowMessage('DOT generado: ' + rutaDot + sLineBreak +
                'No pude ejecutar Graphviz (dot). Instálalo o corre: ' +
                'dot -Tpng "'+rutaDot+'" -o "'+rutaPng+'"');
end;

procedure TfrmRoot.btnSalirClick(Sender: TObject);
begin
  frmRoot.Hide;
  frmLogin.Show;
end;

end.

