unit frmUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uInbox;



type

  { TfrmUserN }

  TfrmUserN = class(TForm)
    btnBandeja: TButton;
    btnCargaMasiva1: TButton;
    btnPapelera: TButton;
    btnCargaMasiva3: TButton;
    btnCargaMasiva4: TButton;
    btnCargaMasiva5: TButton;
    btnCargaMasiva6: TButton;
    btnCargaMasiva7: TButton;
    btnCargaMasiva8: TButton;
    btnCerrarSesion: TButton;
    lblWelcome: TLabel;
    procedure btnBandejaClick(Sender: TObject);
    procedure btnCargaMasiva1Click(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnCerrarSesionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmUserN: TfrmUserN;

implementation

uses fLogin, fSendMail, fTrash;

{$R *.lfm}

{ TfrmUserN }



procedure TfrmUserN.btnCerrarSesionClick(Sender: TObject);
begin
  if not Assigned(frmLogin) then
     Application.CreateForm(TfrmLogin, frmLogin);
  frmLogin.Show;
  Self.Hide;
end;


procedure TfrmUserN.btnBandejaClick(Sender: TObject);
begin
  if not Assigned(frmInbox) then
    Application.CreateForm(TfrmInbox, frmInbox);
  frmUserN.Hide;
  frmInbox.Show;
end;

procedure TfrmUserN.btnCargaMasiva1Click(Sender: TObject);
begin
  if not Assigned(frmSendMail) then
    Application.CreateForm(TfrmSendMail, frmSendMail);
  frmUserN.Hide;
  frmSendMail.Show;
end;

procedure TfrmUserN.btnPapeleraClick(Sender: TObject);
begin
  if not Assigned(frmTrash) then
    Application.CreateForm(TfrmTrash, frmTrash);
  Hide;
  frmTrash.Show;
end;

procedure TfrmUserN.FormCreate(Sender: TObject);
begin

end;

end.

