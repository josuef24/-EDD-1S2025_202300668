unit fLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fMaiin, fCreateUser, frmUser, uUsers; // importa la ventana principal

type

  { TfrmLogin }

  TfrmLogin = class(TForm)
    CrearCuenta: TButton;
    Label1: TLabel;
    lblEmail: TLabel;
    edtEmail: TEdit;
    lblPass: TLabel;
    edtPass: TEdit;
    btnIngresar: TButton;

    procedure CrearCuentaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnIngresarClick(Sender: TObject);
  private
    procedure AbrirMain;
  public
  end;

var
  frmLogin: TfrmLogin;

implementation

{$R *.lfm}

{ TfrmLogin }



procedure TfrmLogin.CrearCuentaClick(Sender: TObject);
begin
  frmLogin.Hide;

  if not Assigned(frmCreateUser) then
     Application.CreateForm(TfrmCreateUser, frmCreateUser);

  frmCreateUser.Show;
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  Caption := 'Inicio de Sesión';
  // valores iniciales vacíos
  edtEmail.Text := '';
  edtPass.Text := '';
end;





procedure TfrmLogin.btnIngresarClick(Sender: TObject);
var
  key, pass: string;
  isRoot: Boolean;
begin
  key   := Trim(edtEmail.Text);  // puede ser email o usuario
  pass  := Trim(edtPass.Text);
  isRoot := False;               // evita la advertencia

  if not ValidateUser(key, pass, isRoot) then
  begin
    ShowMessage('Credenciales incorrectas');
    Exit;
  end;

  // Abrir form según el tipo
  frmLogin.Hide;
  if isRoot then
  begin
    if not Assigned(frmRoot) then
      Application.CreateForm(TfrmRoot, frmRoot);
    frmRoot.Show;
  end
  else
  begin
    if not Assigned(frmUserN) then
      Application.CreateForm(TfrmUserN, frmUserN);
    frmUserN.Show;
  end;
end;


procedure TfrmLogin.AbrirMain;
begin
  frmLogin.Hide;
  if not Assigned(frmRoot) then
    Application.CreateForm(TfrmRoot, frmRoot);
  frmRoot.Show;
end;

end.

