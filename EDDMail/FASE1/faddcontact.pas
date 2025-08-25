unit fAddContact;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmAddContact }

  TfrmAddContact = class(TForm)
    lblTitulo: TLabel;
    edtEmail: TEdit;
    btnAgregar: TButton;
    btnCancelar: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnAgregarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  end;

var
  frmAddContact: TfrmAddContact;

implementation

uses
  uUsers, uContacts, frmUser;  // CurrentUser, AddContact, volver al menú

{$R *.lfm}

procedure TfrmAddContact.FormShow(Sender: TObject);
begin
  Caption := 'Agregar Contacto';
  lblTitulo.Caption := 'Agregar contacto (por correo)';
  edtEmail.Text := '';
  edtEmail.SetFocus;
end;

procedure TfrmAddContact.btnAgregarClick(Sender: TObject);
var
  key: AnsiString;
  U: PUser;
begin
  if CurrentUser = nil then Exit;

  key := Trim(edtEmail.Text);
  if key = '' then
  begin
    ShowMessage('Ingrese el correo del contacto.');
    Exit;
  end;

  // evitar agregarse a sí mismo
  if AnsiCompareText(key, CurrentUser^.Email) = 0 then
  begin
    ShowMessage('No puede agregarse a sí mismo.');
    Exit;
  end;

  // buscar usuario por email/usuario
  U := FindUserByEmailOrUsername(key);
  if U = nil then
  begin
    ShowMessage('No existe un usuario con ese correo/usuario.');
    Exit;
  end;

  // evitar duplicado en contactos
  if ExistsInContacts(CurrentUser^.Contacts, U^.Email) or
     ExistsInContacts(CurrentUser^.Contacts, U^.Username) then
  begin
    ShowMessage('Ese contacto ya existe.');
    Exit;
  end;

  // agregar a la lista circular
  if AddContact(CurrentUser^.Contacts, U^.Name, U^.Username, U^.Email, U^.Phone) then
  begin
    ShowMessage('Contacto agregado.');
    frmAddContact.Hide;
    frmUserN.Show;   // regresar al menú de usuario
  end
  else
    ShowMessage('No se pudo agregar el contacto.');
end;

procedure TfrmAddContact.btnCancelarClick(Sender: TObject);
begin
  frmAddContact.Hide;
  frmUserN.Show;
end;

end.


