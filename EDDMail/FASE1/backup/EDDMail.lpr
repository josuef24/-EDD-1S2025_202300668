uses
  {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
  Interfaces, Forms, fLogin, fMaiin, fCreateUser, frmUser, uUsers, uInbox,
  fInbox, fSendMail;

begin
  Application.Initialize;
  InitUsers;
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.CreateForm(TfrmCreateUser, frmCreateUser);
  Application.CreateForm(TfrmUserN, frmUserN);
  Application.CreateForm(TfrmInbox, frmInbox);
  Application.CreateForm(TfrmSendMail, frmSendMail);
  Application.Run;
end.

