unit UDataComunidadesBST;

{$mode ObjFPC}{$H+}

interface
uses UComunidadesBST;

var
  ComunidadesRoot: PComNode = nil;

implementation

initialization
  BST_Init(ComunidadesRoot);

end.

