unit fDelContact;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs,
  uUsers, uContacts;

type

  { TfrmDelContact }

  TfrmDelContact = class(TForm)
    lblCorreo: TLabel;
    edEmail: TEdit;
    btnEliminar: TButton;
    btnCerrar: TButton;
    procedure btnEliminarClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

var
  frmDelContact: TfrmDelContact;

implementation

uses frmUser;

{$R *.lfm}

procedure TfrmDelContact.FormShow(Sender: TObject);
begin
  edEmail.Clear;
  edEmail.SetFocus;
end;

procedure TfrmDelContact.btnEliminarClick(Sender: TObject);
var
  Username: string;
begin

  Username := Trim(edEmail.Text);
  if Username = '' then
  begin
    ShowMessage('Ingresa el correo del contacto a eliminar.'); Exit;
  end;

  // Validar que SÍ es contacto
  if not ContactExists(CurrentUser^.Contacts, Username) then
  begin
    ShowMessage('El correo no está en tus contactos.'); Exit;
  end;

  // Eliminar
  if ContactRemove(CurrentUser^.Contacts, Username) then
  begin
    ShowMessage('Contacto eliminado: ' + Username);
    edEmail.Clear;
  end
  else
    ShowMessage('No se pudo eliminar el contacto (intenta de nuevo).');
end;

procedure TfrmDelContact.btnCerrarClick(Sender: TObject);
begin
  frmDelContact.Hide;
  frmUserN.Show;
end;

procedure TfrmDelContact.FormCreate(Sender: TObject);
begin

end;

end.

