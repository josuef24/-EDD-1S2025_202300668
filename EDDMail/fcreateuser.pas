unit fCreateUser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmCreateUser }

  TfrmCreateUser = class(TForm)
    lblTitulo:   TLabel;
    lblNombre:   TLabel;
    lblUsuario:  TLabel;
    lblEmail:    TLabel;
    lblTelefono: TLabel;
    lblPass:     TLabel;

    edtNombre:   TEdit;   // ← OJO con los nombres: edtNombre/edtUsuario/...
    edtUsuario:  TEdit;
    edtEmail:    TEdit;
    edtTelefono: TEdit;
    edtPass:     TEdit;

    btnCrear:    TButton;
    btnCancelar: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnCrearClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  end;

var
  frmCreateUser: TfrmCreateUser;

implementation

uses uUsers, fLogin;  // ← necesarios (AddUser/Exists... y volver al Login)

{$R *.lfm}

procedure TfrmCreateUser.FormCreate(Sender: TObject);
begin
  Caption := 'Crear Usuario';
  lblTitulo.Caption   := 'Crear Cuenta';
  lblNombre.Caption   := 'Nombre';
  lblUsuario.Caption  := 'Usuario';
  lblEmail.Caption    := 'Email';
  lblTelefono.Caption := 'Teléfono';
  lblPass.Caption     := 'Contraseña';
  btnCrear.Caption    := 'Crear';
  btnCancelar.Caption := 'Cancelar';
end;

procedure TfrmCreateUser.btnCrearClick(Sender: TObject);
var
  nombre, usuario, email, tel, pass: AnsiString;
  id: Integer;
begin
  nombre  := Trim(edtNombre.Text);
  usuario := Trim(edtUsuario.Text);
  email   := Trim(edtEmail.Text);
  tel     := Trim(edtTelefono.Text);
  pass    := Trim(edtPass.Text);

  if (nombre = '') or (usuario = '') or (email = '') or (pass = '') then
  begin
    ShowMessage('Nombre, Usuario, Email y Contraseña son obligatorios.');
    Exit;
  end;

  if (Pos('@', email) = 0) or (Pos('.', email) = 0) then
  begin
    ShowMessage('El email no parece válido.');
    Exit;
  end;

  if ExistsEmailOrUsername(email) or ExistsEmailOrUsername(usuario) then
  begin
    ShowMessage('Ya existe un usuario con ese email o nombre de usuario.');
    Exit;
  end;

  id := AddUser(nombre, usuario, email, tel, pass, False); // ← guarda en la lista
  ShowMessage('Usuario creado con ID = ' + IntToStr(id));

  if not Assigned(frmLogin) then
    Application.CreateForm(TfrmLogin, frmLogin);
  frmLogin.Show;
  Self.Hide;
end;

procedure TfrmCreateUser.btnCancelarClick(Sender: TObject);
begin
  if not Assigned(frmLogin) then
    Application.CreateForm(TfrmLogin, frmLogin);
  frmLogin.Show;
  Self.Hide;
end;

end.

