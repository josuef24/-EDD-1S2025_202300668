unit frmUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;



type

  { TfrmUserN }

  TfrmUserN = class(TForm)
    btnCerrarSesion: TButton;
    lblWelcome: TLabel;
    procedure btnCerrarSesionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmUserN: TfrmUserN;

implementation

uses fLogin;

{$R *.lfm}

{ TfrmUserN }



procedure TfrmUserN.btnCerrarSesionClick(Sender: TObject);
begin
  if not Assigned(frmLogin) then
     Application.CreateForm(TfrmLogin, frmLogin);
  frmLogin.Show;
  Self.Hide;
end;

procedure TfrmUserN.FormCreate(Sender: TObject);
begin

end;

end.

