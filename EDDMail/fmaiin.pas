unit fMaiin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uUsers;

type

  { TfrmRoot }

  TfrmRoot = class(TForm)
  published
    btnSalir: TButton;
    btnCargaMasiva: TButton;
    Label1: TLabel;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure Button1Click(Sender: TObject);
  end;

var
  frmRoot: TfrmRoot;

implementation

uses fLogin;

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

procedure TfrmRoot.btnSalirClick(Sender: TObject);
begin
  frmRoot.Hide;
  frmLogin.Show;
end;


end.

