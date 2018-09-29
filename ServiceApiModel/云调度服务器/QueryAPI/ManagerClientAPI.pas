unit ManagerClientAPI;

interface

uses CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine,
  CommunicationFrameworkDoubleTunnelIO_ServMan;

type
  TQueryResultInfo = record
    RegName: string;
    Host: string;
    RecvPort, SendPort: WORD;
    WorkLoad: Integer;
    ServerType: TServerType;
    procedure Init;
  end;

  TManagerQueryProc = reference to procedure(const State: Boolean; const Addr: TQueryResultInfo);

  TManagerQuery = class
  private
    ClientIntf: TCommunicationFrameworkClient;
  public
    ServerList: TGenericsList<TQueryResultInfo>;

    constructor Create(AClientIntf: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure Connect(Addr: string; Port: WORD); virtual;

    // 阻塞式查询全局服务器，返回的查询结果在ServerList中
    procedure WaitQuery(Addr: string; QueryType: TServerType);
    // 异步式查询全局服务器，ResultProc事件被触发时表示异步查询已经完成，返回查询结果在ServerList中，
    procedure AsyncQuery(Addr: string; QueryType: TServerType; ResultProc: TStateProc);
    // 异步式查询全局服务器，触发ResultProc是一次性，里面是一个负载最小的服务器值，所有的返回的查询结果在ServerList中
    procedure Query(Addr: string; QueryType: TServerType; ResultProc: TManagerQueryProc);

    procedure Disconnect;
    function ExistsServerType(ServerType: TServerType): Boolean;
    procedure Clear;

    procedure Progress;
  end;

implementation

procedure TQueryResultInfo.Init;
begin
  RegName := '';
  Host := '';
  RecvPort := 0;
  SendPort := 0;
  WorkLoad := -1;
  ServerType := TServerType.stUnknow;
end;

constructor TManagerQuery.Create(AClientIntf: TCommunicationFrameworkClient);
begin
  inherited Create;
  ClientIntf := AClientIntf;
  ServerList := TGenericsList<TQueryResultInfo>.Create;
end;

destructor TManagerQuery.Destroy;
begin
  DisposeObject(ServerList);
  inherited Destroy;
end;

procedure TManagerQuery.Progress;
begin
  ClientIntf.Progress;
end;

procedure TManagerQuery.Connect(Addr: string; Port: WORD);
begin
  ClientIntf.Connect(Addr, Port);
end;

procedure TManagerQuery.WaitQuery(Addr: string; QueryType: TServerType);
var
  sendDE, ResultDE: TDataFrameEngine;
  vl: THashVariantList;
  a: TQueryResultInfo;
begin
  ServerList.Clear;
  try
    Connect(Addr, CDEFAULT_MANAGERSERVICE_QUERYPORT);
    if (ClientIntf.Connected) and (ClientIntf.RemoteInited) then
      begin
        // 查询所有在管理中心注册的服务器，返回所有服务器
        sendDE := TDataFrameEngine.Create;
        sendDE.WriteByte(Byte(QueryType));

        ResultDE := TDataFrameEngine.Create;
        ClientIntf.WaitSendStreamCmd('Query', sendDE, ResultDE, 5000);

        vl := THashVariantList.Create;

        if ResultDE.Count > 0 then
          begin
            while not ResultDE.Reader.IsEnd do
              begin
                ResultDE.Reader.ReadVariantList(vl);
                a.RegName := vl.GetDefaultValue('Name', '');
                a.Host := vl.GetDefaultValue('Host', '');
                a.RecvPort := vl.GetDefaultValue('RecvPort', 0);
                a.SendPort := vl.GetDefaultValue('SendPort', 0);
                a.WorkLoad := vl.GetDefaultValue('WorkLoad', 0);
                a.ServerType := vl.GetDefaultValue('Type', TServerType.stUnknow);
                ServerList.Add(a);
                vl.Clear;
              end;
          end;

        DisposeObject(vl);

        DisposeObject(sendDE);
        DisposeObject(ResultDE);
      end;
  except
  end;
end;

procedure TManagerQuery.AsyncQuery(Addr: string; QueryType: TServerType; ResultProc: TStateProc);
var
  sendDE: TDataFrameEngine;
begin
  ServerList.Clear;
  try
    Connect(Addr, CDEFAULT_MANAGERSERVICE_QUERYPORT);
    if (ClientIntf.Connected) and (ClientIntf.RemoteInited) then
      begin
        // 查询所有在管理中心注册的服务器，返回所有服务器
        sendDE := TDataFrameEngine.Create;
        sendDE.WriteByte(Byte(QueryType));
        ClientIntf.SendStreamCmdP('Query', sendDE, procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
          var
            vl: THashVariantList;
            a: TQueryResultInfo;
          begin
            vl := THashVariantList.Create;

            if ResultData.Count > 0 then
              begin
                while not ResultData.Reader.IsEnd do
                  begin
                    ResultData.Reader.ReadVariantList(vl);
                    a.RegName := vl.GetDefaultValue('Name', '');
                    a.Host := vl.GetDefaultValue('Host', '');
                    a.RecvPort := vl.GetDefaultValue('RecvPort', 0);
                    a.SendPort := vl.GetDefaultValue('SendPort', 0);
                    a.WorkLoad := vl.GetDefaultValue('WorkLoad', 0);
                    a.ServerType := vl.GetDefaultValue('Type', TServerType.stUnknow);
                    ServerList.Add(a);
                    vl.Clear;
                  end;
                ResultProc(True);
              end
            else
                ResultProc(False);

            DisposeObject(vl);
          end);

        DisposeObject(sendDE);
      end;
  except
  end;
end;

procedure TManagerQuery.Query(Addr: string; QueryType: TServerType; ResultProc: TManagerQueryProc);
var
  sendDE: TDataFrameEngine;
begin
  ServerList.Clear;
  try
    Connect(Addr, CDEFAULT_MANAGERSERVICE_QUERYPORT);
    if (ClientIntf.Connected) and (ClientIntf.RemoteInited) then
      begin
        // 查询所有在管理中心注册的服务器，返回最小负载
        sendDE := TDataFrameEngine.Create;
        sendDE.WriteByte(Byte(QueryType));
        ClientIntf.SendStreamCmdP('QueryMinLoad', sendDE, procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
          var
            vl: THashVariantList;
            a: TQueryResultInfo;
          begin
            vl := THashVariantList.Create;

            if ResultData.Count > 0 then
              begin
                while not ResultData.Reader.IsEnd do
                  begin
                    ResultData.Reader.ReadVariantList(vl);
                    a.RegName := vl.GetDefaultValue('Name', '');
                    a.Host := vl.GetDefaultValue('Host', '');
                    a.RecvPort := vl.GetDefaultValue('RecvPort', 0);
                    a.SendPort := vl.GetDefaultValue('SendPort', 0);
                    a.WorkLoad := vl.GetDefaultValue('WorkLoad', 0);
                    a.ServerType := vl.GetDefaultValue('Type', TServerType.stUnknow);
                    if Assigned(ResultProc) then
                      begin
                        ResultProc(True, a);
                      end;
                    ServerList.Add(a);
                    vl.Clear;
                  end;
              end
            else
              begin
                a.Init;
                if Assigned(ResultProc) then
                    ResultProc(False, a);
              end;

            DisposeObject(vl);
          end);

        DisposeObject(sendDE);
      end;
  except
  end;
end;

procedure TManagerQuery.Disconnect;
begin
  try
      ClientIntf.ClientIO.Disconnect;
  except
  end;
end;

function TManagerQuery.ExistsServerType(ServerType: TServerType): Boolean;
var
  a: TQueryResultInfo;
begin
  Result := True;
  for a in ServerList do
    if a.ServerType = ServerType then
        exit;
  Result := False;
end;

procedure TManagerQuery.Clear;
begin
  ServerList.Clear;
end;

end.
