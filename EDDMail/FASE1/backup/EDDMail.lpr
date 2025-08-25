uses
  {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
  Interfaces, Forms, fLogin, fMaiin, fCreateUser, frmUser, uUsers, uInbox,
  fSendMail, fViewMail, uTrash, fTrash, uQueue, fProgramarMail, fProgramados,
  uContacts, fContacts, fAddContact;

begin
  Application.Scaled:=True;
  Application.Initialize;
  InitUsers;
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.CreateForm(TfrmAddContact, frmAddContact);
  Application.Run;
end.

