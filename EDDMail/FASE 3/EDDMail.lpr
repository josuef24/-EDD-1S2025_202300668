uses
  {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
  Interfaces, Forms, fLogin, fMaiin, fCreateUser, frmUser, uUsers, uInbox,
  fViewMail, uTrash, fTrash, uQueue, fProgramarMail, fProgramados,
  uContacts, fContacts, fAddContact, fPerfil, uMatrix, uReportUserInbox,
uReportUserTrash, UComunidades, UComunidadesAdapters, fComunidades,
UAVL_Borradores, UDataAVL, FBorradores, fSendMail, UBTree_Favoritos,
UDataBT_Fav, FFavoritos;

begin
  Application.Scaled:=True;
  Application.Initialize;
  InitUsers;
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.CreateForm(TfrmAddContact, frmAddContact);
  Application.CreateForm(TfrmPerfil, frmPerfil);
  Application.CreateForm(TfrmComunidades, frmComunidades);
  Application.CreateForm(TFormBorradores, FormBorradores);
  Application.CreateForm(TfrmViewMail, frmViewMail);
  Application.CreateForm(TfrmSendMail, frmSendMail);
  Application.CreateForm(TFormFavoritos, FormFavoritos);
  Application.Run;
end.

