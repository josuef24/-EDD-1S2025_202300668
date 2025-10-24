unit fComunidades;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs,
  UComunidades, UComunidadesAdapters, uUsers, fMaiin;

type
  // Esta es la clase del formulario de Comunidades (yo manejo aquí los eventos de crear comunidad y agregar usuario)
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
    function BuscarNombrePorCorreo(const Email: String): String; // Si necesito resolver nombre por email manualmente, lo hago aquí
    procedure Msg(const s: String);                               // Atajo para mostrar mensajes
  public
  end;

var
  frmComunidades: TfrmComunidades;

implementation

{$R *.lfm}

const
  OUT_DIR = 'Root-Reportes'; // Directorio estándar de reportes del root (no lo uso directo aquí, pero lo dejo por consistencia)

procedure TfrmComunidades.Msg(const s: String);
begin

  ShowMessage(s);
end;

// intento obtener el nombre real del usuario con base en su email (o username).
// PUser es un PUNTERO a un registro/estructura de usuario (definido en uUsers).
function TfrmComunidades.BuscarNombrePorCorreo(const Email: String): String;
var
  U: PUser; // APUNTADOR U es un puntero (PUser). Debo validar nil antes de desreferenciar.
begin
  // Uso la función que ya tengo para buscar por email/username
  U := FindUserByEmailOrUsername(Email);
  if U <> nil then
    Result := U^.Name   // Accedo a los campos con ^.  Aquí U^.Name es el nombre real.
  else
    Result := '';       // No lo encontré
end;

procedure TfrmComunidades.FormShow(Sender: TObject);
begin
  // Cada vez que abro este form lo dejo limpio para evitar datos residuales
  edNombreComunidad.Clear;
  edComunidad.Clear;
  edCorreo.Clear;
end;

procedure TfrmComunidades.btnCrearClick(Sender: TObject);
var
  nom: String;
begin
  // Cuando doy clic en "Crear", tomo el nombre y valido
  nom := Trim(edNombreComunidad.Text);
  if nom = '' then
  begin
    Msg('Ingresa un nombre de comunidad.');
    Exit;
  end;

  // BuscarComunidad devuelve un puntero a la comunidad si existe (tipo PComunidad)
  // Comparo contra nil para saber si existe o no.
  if BuscarComunidad(nom) <> nil then
    Msg('La comunidad ya existe.')
  else
  begin
    // Si no existe, la creo. CrearComunidad internamente maneja memoria punteros (lista simple de comunidades).
    CrearComunidad(nom);
    Msg('Comunidad "' + nom + '" creada/lista.');
    edComunidad.Text := nom; // Me copio el nombre a la caja de "Comunidad" para agregar usuarios de una vez
  end;
end;

procedure TfrmComunidades.btnAgregarClick(Sender: TObject);
var
  com, email, nombre: String;
  U: PUser; // Este es puntero al usuario recuperado desde uUsers.
begin
  // Leo los campos
  com   := Trim(edComunidad.Text);
  email := Trim(edCorreo.Text);

  if (com = '') or (email = '') then
  begin
    Msg('Completa Comunidad y Correo.');
    Exit;
  end;

  // Verifico que el correo exista en mi lista de usuarios cargada
  U := FindUserByEmailOrUsername(email);
  if U = nil then
  begin
    Msg('El correo no existe en la lista de usuarios: ' + email);
    Exit;
  end;

  // APUNTADOR Como U es puntero válido, accedo al campo con U^.Name
  nombre := U^.Name;

  // UComunidades Si la comunidad no existe, la creo (CrearComunidad devuelve/gestiona un PComunidad internamente)
  if BuscarComunidad(com) = nil then
    CrearComunidad(com);

  // UComunidadesAdapters Aquí agrego al usuario a la comunidad usando su correo como id (neutro String)
  // EDIT Si en el futuro quiero usar otro id (por ejemplo el username), aquí es donde lo cambiaría.
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

