unit fComunidades;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs,
  UComunidades, UComunidadesAdapters, uUsers, fMaiin,
  UComunidadesBST, UDataComunidadesBST;

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

   if BST_Find(ComunidadesRoot, nom) <> nil then
  begin
    Msg('La comunidad "'+nom+'" ya existe.');
    Exit;
  end;

  BST_Insert(ComunidadesRoot, nom);
  Msg('Comunidad "' + nom + '" creada en el Árbol.');
  edComunidad.Text := nom;

end;

procedure TfrmComunidades.btnAgregarClick(Sender: TObject);
var
  com, email, nombre: String;
  U: PUser;
  C: PComNode;
begin
  com   := Trim(edComunidad.Text);
  email := Trim(edCorreo.Text);

  if (com = '') or (email = '') then
  begin
    Msg('Completa Comunidad y Correo.');
    Exit;
  end;

  // Verifico que el correo exista en la lista de usuarios cargada
  U := FindUserByEmailOrUsername(email);
  if U = nil then
  begin
    Msg('El correo no existe en la lista de usuarios: ' + email);
    Exit;
  end;
  nombre := U^.Name;

  // Aseguro la comunidad en el BST
  C := BST_Find(ComunidadesRoot, com);
  if C = nil then
    C := BST_Insert(ComunidadesRoot, com);

  // Intento agregar miembro (evita duplicados por Id/email)
  if BST_AddMember(C, email, nombre) then
    Msg('Usuario agregado a "' + com + '".')
  else
    Msg('Ese usuario ya pertenece a la comunidad "' + com + '".');
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

