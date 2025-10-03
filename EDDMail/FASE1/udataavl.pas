unit UDataAVL;

{$mode ObjFPC}{$H+}

interface
uses UAVL_Borradores;

var
  BorradoresRoot: PAVLNode; // raíz del AVL (global)

function NextDraftID: LongInt;

implementation

var
  _Seq: LongInt = 1;

function NextDraftID: LongInt;
begin
  Inc(_Seq);
  Result := _Seq;
end;

initialization
  AVL_Init(BorradoresRoot);
finalization
  // (opcional) liberar nodos si lo deseas
end.

