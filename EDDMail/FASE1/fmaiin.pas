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
    btnSalir: TButton;
    btnCargaMasiva: TButton;
    Label1: TLabel;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnExportRelClick(Sender: TObject);
    procedure btnExportUsersClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure Button1Click(Sender: TObject);
  end;

var
  frmRoot: TfrmRoot;

const
  OUT_DIR = 'Root-Reportes';

implementation

uses fLogin, uMatrix;

{$R *.lfm}

procedure TfrmRoot.FormCreate(Sender: TObject);
begin
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


procedure TfrmRoot.btnSalirClick(Sender: TObject);
begin
  frmRoot.Hide;
  frmLogin.Show;
end;

end.

