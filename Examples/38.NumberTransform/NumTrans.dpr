program NumTrans;

uses
  Vcl.Forms,
  NumTransFrm in 'NumTransFrm.pas' {NumTransForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TNumTransForm, NumTransForm);
  Application.Run;
end. 
