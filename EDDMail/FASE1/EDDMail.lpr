uses
  {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
  Interfaces, Forms, fLogin, fMaiin, fCreateUser, frmUser, uUsers, uInbox,
  fInbox, fSendMail, fViewMail, uTrash, fTrash;

begin
  Application.Scaled:=True;
  Application.Initialize;
  InitUsers;
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.Run;
end.

