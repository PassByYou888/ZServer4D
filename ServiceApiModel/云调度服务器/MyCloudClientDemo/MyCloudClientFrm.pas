unit MyCloudClientFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo,
  FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls,

  CoreClasses, PascalStrings, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine,
  CommunicationFrameworkDoubleTunnelIO_ServMan,
  CommunicationFramework_Client_Indy, ManagerClientAPI, MyCloudClientAPI;

type
  TForm1 = class(TForm)
    QueryButton: TButton;
    AddrEdit: TEdit;
    Memo: TMemo;
    Timer1: TTimer;
    QueryMinButton: TButton;
    WaitQueryButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WaitQueryButtonClick(Sender: TObject);
    procedure QueryButtonClick(Sender: TObject);
    procedure QueryMinButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FManQueryCli: TCommunicationFramework_Client_Indy;
    FManQuery: TManagerQuery;
    FMyCloudClient: TMyCloudClient;
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

procedure TForm1.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
  Memo.GoToTextEnd;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  FManQueryCli := TCommunicationFramework_Client_Indy.Create;
  FManQuery := TManagerQuery.Create(FManQueryCli);
  FMyCloudClient := TMyCloudClient.Create(TCommunicationFramework_Client_Indy);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  DisposeObject(FManQuery);
  DisposeObject(FManQueryCli);
  DisposeObject(FMyCloudClient);
end;

procedure TForm1.QueryButtonClick(Sender: TObject);
begin
  // 异步查询全局服务器
  FManQuery.AsyncQuery(
    AddrEdit.Text, // 调度服务器地址
    stLogic,       // 查询的目标服务器种类
      procedure(const state: Boolean)
    var
        a: TQueryResultInfo;
    begin
        // 触发该事件时候，表示异步查询服务器已经完成，state表示查询状态是否成功
        if state then
        begin
            DoStatus('异步查询全局服务器完成');
            // 我们开始处理返回信息
            for a in FManQuery.ServerList do
            begin
                if FMyCloudClient.Connect(a.Host, a.RecvPort, a.SendPort) then
                begin
                    DoStatus(FMyCloudClient.MyAPI(100, 99));
                    FMyCloudClient.Disconnect;
                end;
            end;
        end;
    end);
end;

procedure TForm1.QueryMinButtonClick(Sender: TObject);
begin
  // 查询全局最小负载的一台服务器，查询返回事件只触发一次
  FManQuery.Query(
    AddrEdit.Text, // 调度服务器地址
    stLogic,       // 查询的目标服务器种类
      procedure(const state: Boolean; const Addr: TQueryResultInfo)
    begin
        // 这里是查询的反馈触发
        if state then
        begin
            // 查询成功时，Addr参数是目标服务器的地址，端口，当前负载等等相关信息
            // 如果是云服务器框架，通过这里的反馈信息做目标服务器登录连接
            DoStatus('最小负载服务器查询完成');

            if FMyCloudClient.Connect(Addr.Host, Addr.RecvPort, Addr.SendPort) then
            begin
                DoStatus(FMyCloudClient.MyAPI(100, 99));
                FMyCloudClient.Disconnect;
            end;
        end
      else
          DoStatus('最小负载服务器查询失败');
    end);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  FManQuery.Progress;
  FMyCloudClient.Progress;
end;

procedure TForm1.WaitQueryButtonClick(Sender: TObject);
var
  a: TQueryResultInfo;
begin
  // 阻塞式查询全局服务器，返回的查询结果在ServerList中
  FManQuery.WaitQuery(
    AddrEdit.Text, // 调度服务器地址
    stLogic        // 查询的目标服务器种类
    );

  if FManQuery.ServerList.Count > 0 then
    for a in FManQuery.ServerList do
      begin
        DoStatus('调度服务器查询完成');
        // 查询成功时，a是目标服务器的地址，端口，当前负载等等相关信息
        // 如果是云服务器框架，通过这里的反馈信息做目标服务器登录连接
        if FMyCloudClient.Connect(a.Host, a.RecvPort, a.SendPort) then
          begin
            DoStatus(FMyCloudClient.MyAPI(100, 99));
            FMyCloudClient.Disconnect;
          end;
      end
  else
      DoStatus('调度服务器查询失败');
end;

end.
