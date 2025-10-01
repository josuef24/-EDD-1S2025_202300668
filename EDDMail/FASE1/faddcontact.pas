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
  uUsers, uContacts, frmUser;  // [UI] Yo uso estas unidades: CurrentUser, AddContact y para volver al menú de usuario

{$R *.lfm}

procedure TfrmAddContact.FormShow(Sender: TObject);
begin
  // [UI] Al mostrar el form, dejo todo listo para escribir el correo del contacto
  // Paso 1: coloco el título de la ventana y del label
  Caption := 'Agregar Contacto';
  lblTitulo.Caption := 'Agregar contacto (por correo)';

  // Paso 2: limpio el campo y doy foco para escribir de una vez
  edtEmail.Text := '';
  edtEmail.SetFocus;
end;

procedure TfrmAddContact.btnAgregarClick(Sender: TObject);
var
  key: AnsiString;
  U: PUser;
begin
  // Si por alguna razón no hay sesión de usuario, se sale
  if CurrentUser = nil then Exit;

  // Aquí leo el texto ingresado (correo o usuario) y elimino espacios
  key := Trim(edtEmail.Text);
  if key = '' then
  begin
    // Le aviso que debe ingresar un correo/usuario válido
    ShowMessage('Ingrese el correo del contacto.');
    Exit;
  end;

  // Evito que yo me agregue a mí mismo como contacto
  if AnsiCompareText(key, CurrentUser^.Email) = 0 then
  begin
    ShowMessage('No puede agregarse a sí mismo.');
    Exit;
  end;

  //
  U := FindUserByEmailOrUsername(key);
  if U = nil then
  begin
    ShowMessage('No existe un usuario con ese correo/usuario.');
    Exit;
  end;

  // Evito duplicados en mi lista circular de contactos
  // verificaciones: por email y por username
  if ExistsInContacts(CurrentUser^.Contacts, U^.Email) or
     ExistsInContacts(CurrentUser^.Contacts, U^.Username) then
  begin
    ShowMessage('Ese contacto ya existe.');
    Exit;
  end;

  // LISTA CIRCULAR Agrego el contacto a mi lista (nombre, usuario, email, teléfono)
  if AddContact(CurrentUser^.Contacts, U^.Name, U^.Username, U^.Email, U^.Phone) then
  begin
    ShowMessage('Contacto agregado.');


    frmAddContact.Hide;
    frmUserN.Show;
  end
  else
    // Si falla el alta, puede ser por memoria o por un puntero roto en la lista circular
    ShowMessage('No se pudo agregar el contacto.');
end;

procedure TfrmAddContact.btnCancelarClick(Sender: TObject);
begin

  frmAddContact.Hide;
  frmUserN.Show;
end;

end.


