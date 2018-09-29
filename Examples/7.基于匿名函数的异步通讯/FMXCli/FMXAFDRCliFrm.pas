unit FMXAFDRCliFrm;

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

procedure ProcessBackground;
begin
  Application.ProcessMessages;
end;

procedure TFMXDRClientForm.FormCreate(Sender: TObject);
begin
  CommunicationFramework.ProgressBackgroundProc := ProcessBackground;
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
  de1: TDataFrameEngine;
begin
  // 异步方式发送，并且接收Stream指令，反馈以proc回调触发
  de1 := TDataFrameEngine.Create;

  // 基于匿名函数的一级嵌套
  client.SendStreamCmdP('DelayResponse', de1,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      de2: TDataFrameEngine;
    begin
      while ResultData.Reader.NotEnd do
          DoStatus('server response in level 1:%s', [ResultData.Reader.ReadString]);

      de2 := TDataFrameEngine.Create;

      // 基于匿名函数的二级嵌套
      client.SendStreamCmdP('DelayResponse', de2, nil, nil,
        procedure(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine)
        var
          de3: TDataFrameEngine;
        begin
          while ResultData.Reader.NotEnd do
              DoStatus('server response in level 2:%s', [ResultData.Reader.ReadString]);
          de3 := TDataFrameEngine.Create;

          // 基于匿名函数的三级嵌套
          client.SendStreamCmdP('DelayResponse', de3,
            procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
            begin
              while ResultData.Reader.NotEnd do
                  DoStatus('server response in level 3:%s', [ResultData.Reader.ReadString]);
            end);

          DisposeObject(de3);
        end);

      DisposeObject(de2);
    end);
  DisposeObject([de1]);
end;

procedure TFMXDRClientForm.Timer1Timer(Sender: TObject);
begin
  client.Progress;
end;

end.
