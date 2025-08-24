unit fProgramarMail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmProgramarMail }

  TfrmProgramarMail = class(TForm)
    lblPara: TLabel;
    lblAsunto: TLabel;
    lblFecha: TLabel;
    lblHora: TLabel;
    edtPara: TEdit;
    edtAsunto: TEdit;
    memMensaje: TMemo;
    edtFecha: TEdit;   // yyyy-mm-dd
    edtHora: TEdit;    // hh:nn
    btnProgramar: TButton;
    btnCancelar: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnProgramarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  private
    function ParseDateTimeSafe(const SDate, STime: string; out DT: TDateTime): Boolean;
  public
  end;

var
  frmProgramarMail: TfrmProgramarMail;

implementation

uses
  uUsers, uInbox, uQueue, frmUser;

{$R *.lfm}

procedure TfrmProgramarMail.FormShow(Sender: TObject);
begin
  Caption := 'Programar Correo';
  edtPara.Clear;
  edtAsunto.Clear;
  memMensaje.Clear;

  // Sugerir fecha/hora actual para editar
  edtFecha.Text := FormatDateTime('yyyy-mm-dd', Now);
  edtHora.Text  := FormatDateTime('hh:nn', Now);
end;

function TfrmProgramarMail.ParseDateTimeSafe(const SDate, STime: string; out DT: TDateTime): Boolean;
var
  y, m, d, hh, nn: Integer;
begin
  Result := False;
  // muy sencillo y robusto: yyyy-mm-dd y hh:nn
  if (Length(SDate)<>10) or (SDate[5]<>'-') or (SDate[8]<>'-') then Exit;
  if (Length(STime)<>5)  or (STime[3]<>':') then Exit;

  try
    y  := StrToInt(Copy(SDate,1,4));
    m  := StrToInt(Copy(SDate,6,2));
    d  := StrToInt(Copy(SDate,9,2));
    hh := StrToInt(Copy(STime,1,2));
    nn := StrToInt(Copy(STime,4,2));
    DT := EncodeDate(y,m,d) + EncodeTime(hh,nn,0,0);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TfrmProgramarMail.btnProgramarClick(Sender: TObject);
var
  destKey, asunto, cuerpo: AnsiString;
  when: TDateTime;
begin
  if CurrentUser = nil then
  begin
    ShowMessage('No hay sesión activa.');
    Exit;
  end;

  destKey := Trim(edtPara.Text);
  asunto  := Trim(edtAsunto.Text);
  cuerpo  := memMensaje.Lines.Text;

  if (destKey='') or (asunto='') or (Trim(cuerpo)='') then
  begin
    ShowMessage('Completa Para, Asunto y Mensaje.');
    Exit;
  end;

  if not ParseDateTimeSafe(Trim(edtFecha.Text), Trim(edtHora.Text), when) then
  begin
    ShowMessage('Fecha/Hora inválidas. Usa yyyy-mm-dd y hh:nn');
    Exit;
  end;

  // (opcional) Validar que el destinatario exista; más adelante restringimos a contactos
  if FindUserByEmailOrUsername(destKey) = nil then
  begin
    ShowMessage('El destinatario no existe.');
    Exit;
  end;

  // Encolar en la cola del usuario actual
  EnqueueMail(CurrentUser^.Sched,
              CurrentUser^.Email,  // remitente
              destKey,
              asunto,
              cuerpo,
              when);

  ShowMessage('Programado para: ' + DateTimeToStr(when));
  frmProgramarMail.Hide;
  frmUserN.Show;
end;

procedure TfrmProgramarMail.btnCancelarClick(Sender: TObject);
begin
  frmProgramarMailHide;
  frmUserN.Show;
end;

end.


