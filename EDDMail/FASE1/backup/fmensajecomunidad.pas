unit fMensajeComunidad;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uUsers, UComunidadesBST, UDataComunidadesBST, frmUser;

type

  { TfrmMensajeComunidad }

  TfrmMensajeComunidad = class(TForm)
    btnEnviar: TButton;
    edNombreComunidad: TEdit;
    lblPara: TLabel;
    lblPara1: TLabel;
    memMensaje: TMemo;
    procedure btnEnviarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmMensajeComunidad: TfrmMensajeComunidad;

implementation

{$R *.lfm}

{ TfrmMensajeComunidad }

procedure TfrmMensajeComunidad.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMensajeComunidad.btnEnviarClick(Sender: TObject);
var
  com, texto, autor: String;
  CNode: PComNode;

begin
  com   := Trim(edNombreComunidad.Text);
  texto := Trim(memMensaje.Lines.Text);

  if com = '' then
  begin
    ShowMessage('Ingresa el nombre de la comunidad.');
    Exit;
  end;

  if texto = '' then
  begin
    ShowMessage('Escribe un mensaje.');
    Exit;
  end;

  if CurrentUser = nil then
  begin
    ShowMessage('No hay sesión activa.');
    Exit;
  end;
  autor := CurrentUser^.Email;

  // Validar que la comunidad exista en el BST
  CNode := BST_Find(ComunidadesRoot, com);
  if CNode = nil then
  begin
    ShowMessage('La comunidad "'+com+'" no existe.');
    Exit;
  end;

  if BST_AddMessageToExisting(CNode, autor, texto, '') then
  begin
    ShowMessage('Mensaje publicado en "'+com+'".');
    memMensaje.Clear;
  end
  else
    ShowMessage('No se pudo publicar el mensaje.');
end;

end.

