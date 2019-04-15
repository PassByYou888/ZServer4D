unit FMXBatchDataClientFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation,
  FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.ScrollBox, FMX.Memo,
  CommunicationFrameworkDataStoreService, ZDBEngine,
  ZDBLocalManager, CommunicationFramework_Client_Indy,
  NotifyObjectBase,
  CommunicationFramework, CoreClasses, DoStatusIO,
  PascalStrings, MemoryStream64, UnicodeMixedLib,
  CommunicationFrameworkDataStoreService_VirtualAuth,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth;

type
  TMyDataStoreClient = class(TDataStoreClient_VirtualAuth)
  protected
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); override;

  end;

  TFMXBatchDataClientForm = class(TForm)
    TabControl: TTabControl;
    LoginTabItem: TTabItem;
    Layout1: TLayout;
    Layout2: TLayout;
    Label1: TLabel;
    UserIDEdit: TEdit;
    Layout3: TLayout;
    Label2: TLabel;
    PasswdEdit: TEdit;
    LoginBtn: TButton;
    Layout4: TLayout;
    Label3: TLabel;
    ServerEdit: TEdit;
    Timer1: TTimer;
    StatusMemo: TMemo;
    OfflineTabItem: TTabItem;
    Layout5: TLayout;
    DisconnectButton: TButton;
    DBOperationDataTabItem: TTabItem;
    Gen10JsonButton: TButton;
    DisconnectCheckTimer: TTimer;
    Layout6: TLayout;
    Label4: TLabel;
    JsonDestDBEdit: TEdit;
    Gen100kJsonButton: TButton;
    ResultTabItem: TTabItem;
    ResultMemo: TMemo;
    QueryJsonButton: TButton;
    Layout7: TLayout;
    Label5: TLabel;
    JsonKeyEdit: TEdit;
    Layout8: TLayout;
    Label6: TLabel;
    JsonValueEdit: TEdit;
    ResetJsonDBButton: TButton;
    AnalysisJsonButton: TButton;
    Layout9: TLayout;
    Label7: TLabel;
    AnalysisDestDBEdit: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoginBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure DisconnectCheckTimerTimer(Sender: TObject);
    procedure Gen10JsonButtonClick(Sender: TObject);
    procedure Gen100kJsonButtonClick(Sender: TObject);
    procedure QueryJsonButtonClick(Sender: TObject);
    procedure ResetJsonDBButtonClick(Sender: TObject);
    procedure AnalysisJsonButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    RecvTunnel, SendTunnel: TCommunicationFrameworkClient;
    DBClient: TMyDataStoreClient;
    procedure DoStatusNear(AText: string; const ID: Integer);
  end;

var
  FMXBatchDataClientForm: TFMXBatchDataClientForm;

implementation

{$R *.fmx}


procedure TMyDataStoreClient.ClientDisconnect(Sender: TCommunicationFrameworkClient);
begin
  FMXBatchDataClientForm.TabControl.ActiveTab := FMXBatchDataClientForm.LoginTabItem;
  inherited;
end;

procedure TFMXBatchDataClientForm.AnalysisJsonButtonClick(Sender: TObject);
var
  vl: TDBEngineVL; // TDBEngineVL是个key-value数据结构原型
begin
  vl := TDBEngineVL.Create;
  vl['Key'] := 'RandomValue';
  vl['Value'] := 1; // 我们要进行统计的值为1

  // 统计和分析使用服务器端注册的 MyCustomAnalysis 过滤器进行触发
  // 统计和分析在服务器端进行时，可以进行深度匹配，图像相似性，文本相似性，语音相似性等等，它们都会在并行化平台中工作，当然，你还需要相应的算法模块支持
  // 统计和分析可以在服务器静态执行（无碎片缓冲区反馈，这对统计大型数据库，作用很明显）
  // 当服务器安静的执行完成统计和分析后，我们通过事件，进行step to step的操作，ZDB完全兼容和支持匿名函数机制
  // 是不是感觉和单机一样？
  DBClient.QueryDBP(
    'MyCustomAnalysis',      // MyCustomAnalysis 在服务器注册和实现
    False,                   // 缓冲碎片是否同步到客户端，因为我们的统计追求的是结果，这里不需要同步，让服务器去干，我们只需要在完成事件中指定统计完成后干什么事
    True,                    // 是否将查询结果写入到Output数据库，这个Output相当于是select到视图，但是Output会Copy
    False,                   // output数据为内存数据库，如果是False，查询的output会以一个实体文件进行存储
    False,                   // 是否反向查询，从最后开始查
    JsonDestDBEdit.Text,     // 查询的数据库名称
    AnalysisDestDBEdit.Text, // 统计的Output名称
    1.0,                     // 碎片缓冲时间,因为查询过于频率，ZDB底层会在该时间内对查询结果进行缓存和压缩，然后再发送过来,0是即时反馈
    0,                       // 最大等待的查询时间，0是无限
    0,                       // 最大匹配查询的反馈条目数
    vl,                      // 发送给MyCustomQuery用的KeyValue参数
    nil,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      // 服务器查询完成时，触发这里的事件
      DoStatus('统计 %s 完成 总共输出有 %d 条在数据库%s中', [dbN, TotalResult, outN]);
      ResultMemo.BeginUpdate;
      ResultMemo.Lines.Clear;
      // 统计完成后会输出一个永久性文件数据库
      // 我们在改事件中可以反复对改数据库进行再次统计，再次查询，以得到我们需要的结果
      // 但是这里不做多次查询了，直接将统计结果下载到本地并且显示
      DBClient.DownloadDBP(False, outN,
        procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
        var
          ns: TStringList;
        begin
          // 服务器查询到结果后，这里是数据反馈
          // 该事件的数据是临时的，调用结束即被干掉，如果要暂存查询结果数据，必须在这里做
          ns := TStringList.Create;
          ns.LoadFromStream(DataSour);
          ResultMemo.Lines.AddStrings(ns);
          DisposeObject(ns);
        end,
        procedure(dbN, outN, pipeN: string; TotalResult: Int64)
        begin
          // 因为下载完成了统计结果，服务器不再需要这个统计数据库了，现在，我们将删除它
          // 注意：如果统计库正在被某个管线访问，这里的删除就会出错
          // 要避免该问题，我们在统计时，只需要确保统计输出的文件数据库是唯一的
          DBClient.CloseDB(dbN, True);

          DoStatus('统计结果 %s 下载完成 总共 %d 条', [dbN, TotalResult]);
          ResultMemo.EndUpdate;
          TabControl.ActiveTab := ResultTabItem;
        end);
    end);

  DisposeObject(vl);
end;

procedure TFMXBatchDataClientForm.Button1Click(Sender: TObject);
var
  i: Integer;
  j: TDBEngineJson;
begin
  TabControl.Enabled := False;
  // InitDB的第一个参数是内存数据库，我们设置成false是创建一个文件数据库
  DBClient.InitDB(False, JsonDestDBEdit.Text);

  // 产生100个json对象到实体文件库
  // value从11开始计数，注意，这里value我们使用字符串
  for i := 10 + 1 to 100 + 10 do
    begin
      j := TDBEngineJson.Create;
      j.S['myKey'] := IntToStr(i);
      // randomValue用于演示统计和分析功能
      j.i['RandomValue'] := umlRandomRange(1, 10);
      DBClient.FastPostCompleteBuffer(JsonDestDBEdit.Text, j);
      DisposeObject(j);
    end;
  TabControl.Enabled := True;
end;

procedure TFMXBatchDataClientForm.DisconnectButtonClick(Sender: TObject);
begin
  DBClient.Disconnect;
end;

procedure TFMXBatchDataClientForm.DisconnectCheckTimerTimer(
  Sender: TObject);
begin
  // 因为跨平台的问题，indy在ios和安卓平台底层都不支持断线事件
  // 必须手动检查断线状态
  // 当连接成功后，我们激活一个计时器，循环检查断线
  if not DBClient.Connected then
    begin
      DBClient.RecvTunnel.TriggerDoDisconnect;
      DisconnectCheckTimer.Enabled := False;
    end;
end;

procedure TFMXBatchDataClientForm.DoStatusNear(AText: string;
const ID: Integer);
begin
  StatusMemo.Lines.Add(AText);
  StatusMemo.GoToTextEnd;
end;

procedure TFMXBatchDataClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  RecvTunnel := TCommunicationFramework_Client_Indy.Create;
  SendTunnel := TCommunicationFramework_Client_Indy.Create;
  DBClient := TMyDataStoreClient.Create(RecvTunnel, SendTunnel);
  DBClient.RegisterCommand;

  RecvTunnel.QuietMode := True;
  SendTunnel.QuietMode := True;
end;

procedure TFMXBatchDataClientForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
  DisposeObject([DBClient, RecvTunnel, SendTunnel]);
end;

procedure TFMXBatchDataClientForm.Gen10JsonButtonClick(Sender: TObject);
var
  i: Integer;
  j: TDBEngineJson;
begin
  // InitDB的第一个参数是内存数据库，我们设置成false是创建一个文件数据库
  DBClient.InitDB(False, JsonDestDBEdit.Text);

  // 产生10个json对象到实体文件库
  for i := 1 to 10 do
    begin
      j := TDBEngineJson.Create;
      j.S['myKey'] := IntToStr(i);
      // randomValue用于演示统计和分析功能
      j.i['RandomValue'] := umlRandomRange(1, 10);
      DBClient.PostAssembleStream(JsonDestDBEdit.Text, j);
      DisposeObject(j);
    end;
end;

procedure TFMXBatchDataClientForm.Gen100kJsonButtonClick(Sender: TObject);
var
  i: Integer;
  j: TDBEngineJson;
begin
  TabControl.Enabled := False;
  // InitDB的第一个参数是内存数据库，我们设置成false是创建一个文件数据库
  DBClient.InitDB(False, JsonDestDBEdit.Text);

  // 产生100000个json对象到实体文件库
  // value从11开始计数，注意，这里value我们使用字符串
  for i := 10 + 1 to 100000 + 10 do
    begin
      j := TDBEngineJson.Create;
      j.S['myKey'] := IntToStr(i);
      // randomValue用于演示统计和分析功能
      j.i['RandomValue'] := umlRandomRange(1, 10);
      DBClient.FastPostCompleteBuffer(JsonDestDBEdit.Text, j);
      DisposeObject(j);
    end;
  TabControl.Enabled := True;
end;

procedure TFMXBatchDataClientForm.LoginBtnClick(Sender: TObject);
begin
  SendTunnel.AsyncConnectP(ServerEdit.Text, 10099, procedure(const sState: Boolean)
    begin
      if sState then
          RecvTunnel.AsyncConnectP(ServerEdit.Text, 10098, procedure(const rState: Boolean)
          begin
            if rState then
                DBClient.UserLoginP(UserIDEdit.Text, PasswdEdit.Text,
                procedure(const State: Boolean)
                begin
                  if State then
                    begin
                      DoStatus('登录成功');
                      DBClient.TunnelLinkP(
                        procedure(const State: Boolean)
                        begin
                          if State then
                            begin
                              DoStatus('双通道链接成功');
                              TabControl.ActiveTab := DBOperationDataTabItem;

                              // 因为跨平台的问题，indy在ios和安卓平台底层都不支持断线事件
                              // 必须手动检查断线状态
                              // 当连接成功后，我们激活一个计时器，循环检查断线
                              DisconnectCheckTimer.Enabled := True;
                              DBClient.ProgressEngine.PostExecuteP(1, procedure(Sender: TNPostExecute)
                                begin
                                  while not DBClient.DataCipherKeyFinished do
                                      DBClient.Progress;
                                end)
                            end;
                        end);
                    end;
                end);
          end);
    end);
end;

procedure TFMXBatchDataClientForm.QueryJsonButtonClick(Sender: TObject);
var
  vl: TDBEngineVL; // TDBEngineVL是个key-value数据结构原型
begin
  vl := TDBEngineVL.Create;
  vl['Key'] := JsonKeyEdit.Text;
  vl['Value'] := JsonValueEdit.Text;

  ResultMemo.BeginUpdate;
  ResultMemo.Lines.Clear;

  DBClient.QueryDBP(
    'MyCustomQuery',   // MyCustomQuery在服务器注册和实现
  True,                // 缓冲碎片是否同步到客户端
  False,               // 是否将查询结果写入到Output数据库，这个Output相当于是select到视图，但是Output会Copy
  True,                // output数据为内存数据库，如果是False，查询的output会以一个实体文件进行存储
  False,               // 是否反向查询，从最后开始查
  JsonDestDBEdit.Text, // 查询的数据库名称
  '',                  // 查询的Output名称，因为我们不写入Output，又是临时内存，这里可以忽略掉
  1.0,                 // 碎片缓冲时间,因为查询过于频率，ZDB底层会在该时间内对查询结果进行缓存和压缩，然后再发送过来,0是即时反馈
  0,                   // 最大等待的查询时间，0是无限
  0,                   // 最大匹配查询的反馈条目数
  vl,                  // 发送给MyCustomQuery用的KeyValue参数
    procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
    var
      ns: TStringList;
    begin
      // 服务器查询到结果后，这里是数据反馈
      // 该事件的数据是临时的，调用结束即被干掉，如果要暂存查询结果数据，必须在这里做
      ns := TStringList.Create;
      ns.LoadFromStream(DataSour);
      ResultMemo.Lines.AddStrings(ns);
      DisposeObject(ns);
    end,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      // 服务器查询完成时，触发这里的事件
      ResultMemo.EndUpdate;
      TabControl.ActiveTab := ResultTabItem;
      DoStatus('查询 %s 完成 总共找到匹配 %d 条', [dbN, TotalResult]);
    end);

  DisposeObject(vl);
end;

procedure TFMXBatchDataClientForm.ResetJsonDBButtonClick(Sender: TObject);
begin
  DBClient.ResetData(JsonDestDBEdit.Text);
  DBClient.ResetData(AnalysisDestDBEdit.Text);
end;

procedure TFMXBatchDataClientForm.Timer1Timer(Sender: TObject);
begin
  DBClient.Progress;
end;

end.
