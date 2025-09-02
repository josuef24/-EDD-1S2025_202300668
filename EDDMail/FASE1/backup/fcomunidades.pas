unit fComunidades;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs,
  UComunidades, UComunidadesAdapters, uUsers;

type
  { TfrmComunidades }
  TfrmComunidades = class(TForm)
    btnCrear: TButton;
    btnAgregar: TButton;
    btnSalir: TButton;
    edNombreComunidad: TEdit;
    edComunidad: TEdit;
    edCorreo: TEdit;
    lblNombre1: TLabel;
    lblTitulo: TLabel;
    lblNombre: TLabel;
    lblComunidad: TLabel;
    lblCorreo: TLabel;
    procedure btnCrearClick(Sender: TObject);
    procedure btnAgregarClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function BuscarNombrePorCorreo(const Email: String): String;
    procedure Msg(const s: String);
  public
  end;

var
  frmComunidades: TfrmComunidades;

implementation

{$R *.lfm}

const
  OUT_DIR = 'Root-Reportes';

procedure TfrmComunidades.Msg(const s: String);
begin
  ShowMessage(s);
end;

// Intenta obtener el nombre desde tu lista de usuarios por email.
// Variante A: si uUsers tiene una función para buscar por email, úsala aquí.
// Variante B: si no la tienes, de momento devuelve el mismo correo como “nombre” (fallback).
function TfrmComunidades.BuscarNombrePorCorreo(const Email: String): String;
var
  U: PUser;
begin
  // Tu uUsers ya trae FindUserByEmailOrUsername (acepta email o username)
  U := FindUserByEmailOrUsername(Email);
  if U <> nil then
    Result := U^.Name   // nombre real del usuario
  else
    Result := '';       // no encontrado
end;


procedure TfrmComunidades.FormShow(Sender: TObject);
begin
  // Limpia campos cada vez que abras el form
  edNombreComunidad.Clear;
  edComunidad.Clear;
  edCorreo.Clear;
end;

procedure TfrmComunidades.btnCrearClick(Sender: TObject);
var
  nom: String;
begin
  nom := Trim(edNombreComunidad.Text);
  if nom = '' then
  begin
    Msg('Ingresa un nombre de comunidad.');
    Exit;
  end;
  if BuscarComunidad(nom) <> nil then
    Msg('La comunidad ya existe.')
  else
  begin
    CrearComunidad(nom);
    Msg('Comunidad "' + nom + '" creada/lista.');
    edComunidad.Text := nom; // comodidad: copia al campo de “Comunidad”
  end;
end;

procedure TfrmComunidades.btnAgregarClick(Sender: TObject);
var
  com, email, nombre: String;
  U: PUser;
begin
  com   := Trim(edComunidad.Text);
  email := Trim(edCorreo.Text);

  if (com = '') or (email = '') then
  begin
    Msg('Completa Comunidad y Correo.');
    Exit;
  end;

  // Verificar que el correo exista en tu lista de usuarios
  U := FindUserByEmailOrUsername(email);
  if U = nil then
  begin
    Msg('El correo no existe en la lista de usuarios: ' + email);
    Exit;
  end;

  nombre := U^.Name; // ya que existe, tomamos el nombre real

  // Crea la comunidad si no existe
  if BuscarComunidad(com) = nil then
    CrearComunidad(com);

  // En comunidades usamos el correo como id String (neutral)
  if AddUserToCommunity_StrId(com, email, nombre) then
    Msg('Usuario agregado a "' + com + '".')
  else
    Msg('No se pudo agregar');
end;

procedure TfrmComunidades.btnSalirClick(Sender: TObject);
begin
  frmComunidades.Hide;
  frmRoot.Show;
end;


procedure TfrmComunidades.FormCreate(Sender: TObject);
begin

end;

end.

