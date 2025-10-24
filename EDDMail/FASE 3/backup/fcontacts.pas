unit fContacts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmContacts }

  TfrmContacts = class(TForm)
    brnPrev: TButton;
    btnRergresar: TButton;
    lblAsunto: TLabel;
    lblAsunto1: TLabel;
    lblAsunto2: TLabel;
    lblPara: TLabel;
    lblTitulo: TLabel;
    btnPrev: TButton;
    btnNext: TButton;
    // labels de valores:
    valNombre: TLabel;
    valUsuario: TLabel;
    valCorreo: TLabel;
    valTelefono: TLabel;
    procedure btnRergresarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
  private
    procedure MostrarActual;
    procedure HabilitarNavegacion(const OnOff: Boolean);
  public
  end;

var
  frmContacts: TfrmContacts;

implementation

uses
  uUsers, uContacts, frmUser; // CurrentUser, lista circular y regresar

{$R *.lfm}

procedure TfrmContacts.HabilitarNavegacion(const OnOff: Boolean);
begin
  if (btnPrev = nil) or (btnNext = nil) then Exit;
  btnPrev.Enabled := OnOff;
  btnNext.Enabled := OnOff;
end;

procedure TfrmContacts.MostrarActual;
var
  C: PContact;
begin
  // ¿Sin usuario o sin contactos?
  if (CurrentUser = nil) or (CurrentUser^.Contacts.Tail = nil) then
  begin
    valNombre.Caption   := '—';
    valUsuario.Caption  := '—';
    valCorreo.Caption   := '—';
    valTelefono.Caption := '—';
    HabilitarNavegacion(False);
    Exit;
  end;

  // Asegurar “actual” inicial
  if CurrentUser^.Contacts.Curr = nil then
    CurrentUser^.Contacts.Curr := HeadContact(CurrentUser^.Contacts);

  C := CurrentUser^.Contacts.Curr;
  if C <> nil then
  begin
    valNombre.Caption   := C^.Name;
    valUsuario.Caption  := C^.Username;
    valCorreo.Caption   := C^.Email;
    valTelefono.Caption := C^.Phone;
  end;

  // Navegación activa sólo si hay más de 1
  HabilitarNavegacion(CurrentUser^.Contacts.Count > 1);
end;

procedure TfrmContacts.FormShow(Sender: TObject);
begin
  Caption := 'Contactos';
  lblTitulo.Caption := 'Contactos';
  MostrarActual;
end;

procedure TfrmContacts.btnRergresarClick(Sender: TObject);
begin
  frmContacts.Hide;
  frmuserN.Show;
end;

procedure TfrmContacts.btnPrevClick(Sender: TObject);
begin
  PrevContact(CurrentUser^.Contacts);
  MostrarActual;
end;

procedure TfrmContacts.btnNextClick(Sender: TObject);
begin
  NextContact(CurrentUser^.Contacts);
  MostrarActual;
end;

end.
