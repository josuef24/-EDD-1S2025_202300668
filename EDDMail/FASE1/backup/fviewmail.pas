unit fViewMail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmViewMail }

  TfrmViewMail = class(TForm)
    btnCerar: TButton;
    lblAsunto: TLabel;
    lblEstado: TLabel;
    lblFecha: TLabel;
    lblDe: TLabel;
    memCuerpo: TMemo;
    btnCerrar: TButton;
    procedure btnCerrarClick(Sender: TObject);
  public
    procedure ShowMail(AMail: Pointer); // recibe PMail
  end;

var
  frmViewMail: TfrmViewMail;

implementation

uses uInbox; // PMail, MarkRead

{$R *.lfm}

procedure TfrmViewMail.ShowMail(AMail: Pointer);
var
  M: PMail;
begin
  M := PMail(AMail);
  if M = nil then Exit;

  // llenar cabecera
  lblAsunto.Caption := 'Asunto: ' + M^.Asunto;
  lblDe.Caption     := 'De: '     + M^.Remitente;
  lblFecha.Caption  := 'Fecha: '  + M^.Fecha;
  lblEstado.Caption := 'Estado: ' + M^.Estado;

  // cuerpo
  memCuerpo.Lines.Text := M^.Mensaje;

  // marcar leído si estaba NL
  if M^.Estado = 'NL' then
  begin
    MarkRead(M);
    lblEstado.Caption := 'Estado: L';
  end;

  Show; // mostrar la ventana
end;

procedure TfrmViewMail.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

end.

