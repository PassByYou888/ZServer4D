unit AC2ManagerServerMobileClient;

interface

uses System.IOUtils,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine,
  CommunicationFrameworkDoubleTunnelIO_ServMan;

type
  TAddressInfo = record
    RegName: string;
    Host: string;
    RecvPort, SendPort: Word;
    WorkLoad: Integer;
    ServerType: TServerType;
    procedure Init;
  end;

  TQuerySuccess = reference to procedure(const State: Boolean; const Addr: TAddressInfo);

  TManagerQueryBase = class
  protected
    Client: TCommunicationFrameworkClient;
  public
    ServerList: TGenericsList<TAddressInfo>;

    constructor Create(AClientIntf: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure Connect(Addr: string; Port: Word); virtual;
    procedure Query(Addr: string; QueryType: TServerType; ResultProc: TQuerySuccess);
    procedure QueryMinLoad(Addr: string; QueryType: TServerType; ResultProc: TQuerySuccess);
    procedure Disconnect;
    function ExistsServerType(ServerType: TServerType): Boolean;
    procedure Clear;

    procedure Progress;
  end;

implementation

uses CommonServiceDefine;

procedure TAddressInfo.Init;
begin
  RegName := '';
  Host := '';
  RecvPort := 0;
  SendPort := 0;
  WorkLoad := -1;
  ServerType := TServerType.stUnknow;
end;

constructor TManagerQueryBase.Create(AClientIntf: TCommunicationFrameworkClient);
begin
  inherited Create;
  Client := AClientIntf;
  ServerList := TGenericsList<TAddressInfo>.Create;
end;

destructor TManagerQueryBase.Destroy;
begin
  DisposeObject(ServerList);
  inherited Destroy;
end;

procedure TManagerQueryBase.Progress;
begin
  Client.ProgressBackground;
end;

procedure TManagerQueryBase.Connect(Addr: string; Port: Word);
begin
  RaiseInfo('no client interface');
end;

procedure TManagerQueryBase.Query(Addr: string; QueryType: TServerType; ResultProc: TQuerySuccess);
var
  sendDE: TDataFrameEngine;
begin
  ServerList.Clear;
  try
    Connect(Addr, cManagerService_QueryPort);
    if (Client.Connected) and (Client.RemoteInited) then
      begin
        // 查询大厅服务器
        sendDE := TDataFrameEngine.Create;
        sendDE.WriteByte(Byte(QueryType));
        Client.SendStreamCmd('Query', sendDE, procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
          var
            vl: THashVariantList;
            a: TAddressInfo;
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
                    // DoStatus('query success:%s server type:%s', [a.RegName, serverType2Str(a.ServerType)]);
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

procedure TManagerQueryBase.QueryMinLoad(Addr: string; QueryType: TServerType; ResultProc: TQuerySuccess);
var
  sendDE: TDataFrameEngine;
begin
  ServerList.Clear;
  try
    Connect(Addr, cManagerService_QueryPort);
    if (Client.Connected) and (Client.RemoteInited) then
      begin
        // 查询大厅服务器
        sendDE := TDataFrameEngine.Create;
        sendDE.WriteByte(Byte(QueryType));
        Client.SendStreamCmd('QueryMinLoad', sendDE, procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
          var
            vl: THashVariantList;
            a: TAddressInfo;
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

procedure TManagerQueryBase.Disconnect;
begin
  try
      Client.ClientIO.Disconnect;
  except
  end;
end;

function TManagerQueryBase.ExistsServerType(ServerType: TServerType): Boolean;
var
  a: TAddressInfo;
begin
  Result := True;
  for a in ServerList do
    if a.ServerType = ServerType then
        exit;
  Result := False;
end;

procedure TManagerQueryBase.Clear;
begin
  ServerList.Clear;
end;

end.
