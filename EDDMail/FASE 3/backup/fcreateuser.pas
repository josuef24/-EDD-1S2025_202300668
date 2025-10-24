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
    edtNombre:   TEdit;
    edtUsuario:  TEdit;
    edtEmail:    TEdit;
    edtTelefono: TEdit;
    edtPass:     TEdit;
    btnCrear:    TButton;
    btnCancelar: TButton;

    procedure FormCreate(Sender: TObject);       //  Configuro textos iniciales del form
    procedure btnCrearClick(Sender: TObject);    // Intento crear el usuario
    procedure btnCancelarClick(Sender: TObject); // Regreso a la pantalla de login
  end;

var
  frmCreateUser: TfrmCreateUser;

implementation

uses uUsers, fLogin;

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
  nombre, usuario, email, tel, pass: AnsiString; // Datos capturados del formulario
  id: Integer;                                   // ID asignado tras crear el usuario
begin
  // Tomo y normalizo los valores de las cajas de texto
  nombre  := Trim(edtNombre.Text);
  usuario := Trim(edtUsuario.Text);
  email   := Trim(edtEmail.Text);
  tel     := Trim(edtTelefono.Text);
  pass    := Trim(edtPass.Text);

  // Campos obligatorios
  if (nombre = '') or (usuario = '') or (email = '') or (pass = '') then
  begin
    ShowMessage('Nombre, Usuario, Email y Contraseña son obligatorios.');
    Exit;
  end;

  // Chequeo muy básico de formato de email (solo presencia de @ y .)
  if (Pos('@', email) = 0) or (Pos('.', email) = 0) then
  begin
    ShowMessage('El email no parece válido.');
    Exit;
  end;

  // No permito duplicados por email o username
  // ExistsEmailOrUsername busca en la lista enlazada de usuarios (internamente recorre punteros PUser)
  if ExistsEmailOrUsername(email) or ExistsEmailOrUsername(usuario) then
  begin
    ShowMessage('Ya existe un usuario con ese email o nombre de usuario.');
    Exit;
  end;

  // agrego el usuario a la lista enlazada y obtengo su ID autoincremental
  //  AddUser internamente crea un nodo (puntero PUser) y lo enlaza al inicio
  id := AddUser(nombre, usuario, email, tel, pass, False);
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


