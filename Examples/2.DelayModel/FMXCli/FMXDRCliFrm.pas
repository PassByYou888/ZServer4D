unit FMXDRCliFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  CommunicationFramework_Client_Indy, DataFrameEngine,
  CommunicationFramework, CoreClasses, DoStatusIO;

type
  TFMXDRClientForm = class(TForm)
    Memo1: TMemo;
    connectButton: TButton;
    HostEdit: TEdit;
    SendRequestBtn: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure SendRequestBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    client: TCommunicationFramework_Client_Indy;
  end;

var
  FMXDRClientForm: TFMXDRClientForm;

implementation

{$R *.fmx}

{ TFMXClientForm }

procedure TFMXDRClientForm.connectButtonClick(Sender: TObject);
begin
  client.Connect(HostEdit.Text, 9818);
end;

procedure TFMXDRClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TFMXDRClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TCommunicationFramework_Client_Indy.Create;
end;

procedure TFMXDRClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TFMXDRClientForm.SendRequestBtnClick(Sender: TObject);
var
  SendDe: TDataFrameEngine;
begin
  // 异步方式发送，并且接收Stream指令，反馈以proc回调触发
  SendDe := TDataFrameEngine.Create;
  client.SendStreamCmdP('DelayResponse', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      while ResultData.Reader.NotEnd do
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);
end;

procedure TFMXDRClientForm.Timer1Timer(Sender: TObject);
begin
  client.Progress;
end;

end.
