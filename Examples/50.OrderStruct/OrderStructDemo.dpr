program OrderStructDemo;

uses
  Vcl.Forms,
  OrderStructFrm in 'OrderStructFrm.pas' {OrderStructForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOrderStructForm, OrderStructForm);
  Application.Run;
end.
