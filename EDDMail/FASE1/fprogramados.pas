unit fProgramados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmProgramados }

  TfrmProgramados = class(TForm)
    btnProcesar: TButton;
    btnProcesar1: TButton;
    lblTitulo: TLabel;
    ListBox1: TListBox;
  private

  public

  end;

var
  frmProgramados: TfrmProgramados;

implementation

{$R *.lfm}

end.

