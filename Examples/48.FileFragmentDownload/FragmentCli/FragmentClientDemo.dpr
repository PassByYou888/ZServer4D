program FragmentClientDemo;

uses
  Vcl.Forms,
  FragmentClientFrm in 'FragmentClientFrm.pas' {FragmentClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFragmentClientForm, FragmentClientForm);
  Application.Run;
end.
