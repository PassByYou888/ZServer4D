unit DRCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CommunicationFramework,
  DoStatusIO, CoreClasses,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Client_ICS,
  CommunicationFramework_Client_Indy,
  Cadencer, DataFrameEngine;

type
  TDRClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    DelayResponseBtn: TButton;
    DelayResponse2Btn: TButton;
    procedure DelayResponse2BtnClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DelayResponseBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    client: TCommunicationFramework_Client_CrossSocket;
  end;

var
  DRClientForm: TDRClientForm;

implementation

{$R *.dfm}


procedure TDRClientForm.DelayResponse2BtnClick(Sender: TObject);
type
  TMyDefine = record
    a, b, c: Integer;
  end;

  PMyDefine = ^TMyDefine;

var
  SendDe: TDataFrameEngine;
  p: PMyDefine;
begin
  // 由于异步操作,客户端往往难以用正常流程来编写,因此,我们经常会需要用到交换结构
  // PMyDefine就是交换结构,它维系了异步程序的数据一致性
  new(p);
  p^.a := 1;
  p^.b := 2;
  p^.c := 3;

  SendDe := TDataFrameEngine.Create;
  client.SendStreamCmdP('DelayResponse', SendDe, p, nil,
    procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine)
    var
      p2: PMyDefine;
    begin
      // 如果客户端未离线,并且受到服务器反馈,触发该事件
      // 该事件触发时,DelayResponse2BtnClick已经调用结束,这时候我们不能直接访问p变量,因为堆栈已经被破坏,我们需要重新获取PMyDefine的指针数据到p2

      p2 := Param1;

      DoStatus('a:%d', [p2^.a]);
      DoStatus('b:%d', [p2^.b]);
      DoStatus('c:%d', [p2^.c]);

      while ResultData.Reader.NotEnd do
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);

      // 如果对方离线,该事件不会触发,我们刚才申请的PMyDefine内存也会丢失
      dispose(p2);
    end,
    procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine)
    var
      p2: PMyDefine;
    begin
      p2 := Param1;
      // 正在等反馈中,断线了,触发该事件
      DoStatus('未收到反馈,异常断线');
      // 由于不会触发成功接收反馈事件,所以需要在这里释放PMyDefine
      dispose(p2);
    end
    );
  disposeObject([SendDe]);
end;

procedure TDRClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TDRClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TCommunicationFramework_Client_CrossSocket.Create;
end;

procedure TDRClientForm.FormDestroy(Sender: TObject);
begin
  disposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TDRClientForm.DelayResponseBtnClick(Sender: TObject);
var
  SendDe: TDataFrameEngine;
  a: Integer;
begin
  // 异步方式发送，并且接收Stream指令，反馈以proc回调触发
  a := 123;
  SendDe := TDataFrameEngine.Create;
  client.SendStreamCmdP('DelayResponse', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      // 这里的事件在触发时,其实DelayResponseBtnClick已经执行完毕,变量a也已经不再存在,至少它脱离了正常程序范围
      // 在异步事件触发时,a在一个未被破坏的堆栈空间中,这是脱离正常使用范围的,因为这是异步事件
      // 不要在异步事件中引用外面的local变量,尽量用全局变量,或则使用para方式的异步事件,将变量以指针方式指定传递,参考DelayResponse2BtnClick实现
      while ResultData.Reader.NotEnd do
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
      // 那么你知道变量a的引用是copy还是指针吗?
      // 答案是指针,delphi的匿名函数会自动化的引用的外部变量成为一个指针,当你引用时,是在访问一个未知区域的东西
      // 这里打印出来的a是456
      DoStatus(a);
    end);
  disposeObject([SendDe]);
  a := 456;
end;

procedure TDRClientForm.Timer1Timer(Sender: TObject);
begin
  client.Progress;
end;

procedure TDRClientForm.ConnectButtonClick(Sender: TObject);
begin
  if client.Connect(HostEdit.Text, 9818) then
      DoStatus('connect success');
end;

end.
