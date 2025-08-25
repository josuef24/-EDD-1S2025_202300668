unit fPerfil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmPerfil }

  TfrmPerfil = class(TForm)
    btnActualizar: TButton;
    btnCancelar: TButton;
    edtUsuario: TEdit;
    edtTelefono: TEdit;
    edtUsuarioE: TLabel;
    lblPara1: TLabel;
    lblTitulo: TLabel;
    procedure btnCancelarClick(Sender: TObject);
  private

  public

  end;

var
  frmPerfil: TfrmPerfil;

implementation

{$R *.lfm}

{ TfrmPerfil }

procedure TfrmPerfil.btnCancelarClick(Sender: TObject);
begin

end;

end.

