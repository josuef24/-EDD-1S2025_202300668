unit fPerfil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfrmPerfil }
  TfrmPerfil = class(TForm)
    edtUsuarioE: TLabel;
    lblPara1: TLabel;
    lblTitulo: TLabel;
    lblUsuario: TLabel;
    lblTelefono: TLabel;
    edtUsuario: TEdit;
    edtTelefono: TEdit;
    btnActualizar: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnActualizarClick(Sender: TObject);
  private
    procedure CargarDatosActuales;
  public
  end;

var
  frmPerfil: TfrmPerfil;

implementation

uses
  uUsers, frmUser;  // CurrentUser y frmUserN

{$R *.lfm}

{ TfrmPerfil }

procedure TfrmPerfil.CargarDatosActuales;
begin
  if CurrentUser = nil then Exit;
  edtUsuario.Text  := CurrentUser^.Username;
  edtTelefono.Text := CurrentUser^.Phone;
end;

procedure TfrmPerfil.FormShow(Sender: TObject);
begin
  Caption := 'Actualizar Perfil';
  lblTitulo.Caption := 'Actualizar Perfil';
  CargarDatosActuales;
end;

procedure TfrmPerfil.btnActualizarClick(Sender: TObject);
var
  newUser, newPhone: AnsiString;
  cambio: Boolean;
begin
  if CurrentUser = nil then Exit;

  newUser  := Trim(edtUsuario.Text);
  newPhone := Trim(edtTelefono.Text);
  cambio := False;

  // Actualizar USUARIO (si cambió)
  if newUser <> CurrentUser^.Username then
  begin
    if newUser = '' then
    begin
      ShowMessage('El usuario no puede estar vacío.');
      Exit;
    end;
    // Evita duplicados (permite el propio usuario actual)
    if (newUser <> CurrentUser^.Username) and ExistsEmailOrUsername(newUser) then
    begin
      ShowMessage('El nombre de usuario ya existe.');
      Exit;
    end;
    CurrentUser^.Username := newUser;
    cambio := True;
  end;

  // Actualizar TELÉFONO (si cambió)
  if newPhone <> CurrentUser^.Phone then
  begin
    CurrentUser^.Phone := newPhone; // puede ser vacío si así lo desea
    cambio := True;
  end;

  if cambio then
    ShowMessage('Perfil actualizado.')
  else
    ShowMessage('No hay cambios.');

  // Regresar al menú del usuario
  frmPerfil.Hide;          // cierra este form
  frmUserN.Show;  // vuelve al menú
end;

end.


