program UserClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  UserClientFrm in 'UserClientFrm.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
