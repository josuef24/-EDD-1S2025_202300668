unit fViewMail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfrmViewMail }
  TfrmViewMail = class(TForm)
    btnCerar: TButton;
    mnFavoritos: TButton;
    lblAsunto: TLabel;
    lblEstado: TLabel;
    lblFecha: TLabel;
    lblDe: TLabel;
    memCuerpo: TMemo;
    btnCerrar: TButton;
    procedure btnCerrarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnFavoritosClick(Sender: TObject);
  private
    FCurrentMail: Pointer;
  public
    procedure ShowMail(AMail: Pointer);
  end;

var
  frmViewMail: TfrmViewMail;

implementation

uses
  uInbox,           // Para PMail, MarkRead
  UBTree_Favoritos, // Para TFavItem (IMPORTANTE!)
  UDataBT_Fav,      // Para FavInit, FavFind, FavAdd, etc.
  FFavoritos;       // Para FormFavoritos

{$R *.lfm}

procedure TfrmViewMail.ShowMail(AMail: Pointer);
var
  M: PMail;
begin
  M := PMail(AMail);
  if M = nil then Exit;

  FCurrentMail := AMail;

  lblAsunto.Caption := 'Asunto: ' + M^.Asunto;
  lblDe.Caption     := 'De: '     + M^.Remitente;
  lblFecha.Caption  := 'Fecha: '  + M^.Fecha;
  lblEstado.Caption := 'Estado: ' + M^.Estado;

  memCuerpo.Lines.Text := M^.Mensaje;

  if M^.Estado = 'NL' then
  begin
    MarkRead(M);
    lblEstado.Caption := 'Estado: L';
  end;

  Show;
end;

procedure TfrmViewMail.btnCerrarClick(Sender: TObject);
begin
  frmViewMail.hide;
end;

procedure TfrmViewMail.FormCreate(Sender: TObject);
begin
  FCurrentMail := nil;
  FavInit;
end;

procedure TfrmViewMail.mnFavoritosClick(Sender: TObject);
var
  M: PMail;
  existe: TFavItem;
begin
  FavInit;

  if FCurrentMail = nil then
  begin
    ShowMessage('No hay correo seleccionado');
    Exit;
  end;

  M := PMail(FCurrentMail);

  if FavFind(M^.Asunto, existe) then
  begin
    ShowMessage('Este correo ya está en favoritos');
  end
  else
  begin
    try
      FavAdd(
        FavNextID,
        M^.Remitente,
        M^.Asunto,
        M^.Fecha,
        M^.Mensaje
      );

      ShowMessage('¡Correo agregado a favoritos exitosamente!');

      FavSaveToFile(FavFilePath);
    except
      on E: Exception do
        ShowMessage('Error al agregar a favoritos: ' + E.Message);
    end;
  end;

  if not Assigned(FormFavoritos) then
    Application.CreateForm(TFormFavoritos, FormFavoritos);

end;

end.
