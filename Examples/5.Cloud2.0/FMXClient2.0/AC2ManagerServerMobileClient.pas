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
  Client.Progress;
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
        Client.SendStreamCmdP('Query', sendDE, procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
          var
            vl: THashVariantList;
            a2: TAddressInfo;
          begin
            vl := THashVariantList.Create;

            if ResultData.Count > 0 then
              begin
                while not ResultData.Reader.IsEnd do
                  begin
                    ResultData.Reader.ReadVariantList(vl);
                    a2.RegName := vl.GetDefaultValue('Name', '');
                    a2.Host := vl.GetDefaultValue('Host', '');
                    a2.RecvPort := vl.GetDefaultValue('RecvPort', 0);
                    a2.SendPort := vl.GetDefaultValue('SendPort', 0);
                    a2.WorkLoad := vl.GetDefaultValue('WorkLoad', 0);
                    a2.ServerType := vl.GetDefaultValue('Type', TServerType.stUnknow);
                    // DoStatus('query success:%s server type:%s', [a2.RegName, serverType2Str(a2.ServerType)]);
                    if Assigned(ResultProc) then
                      begin
                        ResultProc(True, a2);
                      end;
                    ServerList.Add(a2);
                    vl.Clear;
                  end;
              end
            else
              begin
                a2.Init;
                if Assigned(ResultProc) then
                    ResultProc(False, a2);
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
  a2: TAddressInfo;
begin
  ServerList.Clear;
  try
    Connect(Addr, cManagerService_QueryPort);
    if (Client.Connected) and (Client.RemoteInited) then
      begin
        // 查询大厅服务器
        sendDE := TDataFrameEngine.Create;
        sendDE.WriteByte(Byte(QueryType));
        Client.SendStreamCmdP('QueryMinLoad', sendDE, procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
          var
            vl: THashVariantList;
            a2: TAddressInfo;
          begin
            vl := THashVariantList.Create;

            if ResultData.Count > 0 then
              begin
                while not ResultData.Reader.IsEnd do
                  begin
                    ResultData.Reader.ReadVariantList(vl);
                    a2.RegName := vl.GetDefaultValue('Name', '');
                    a2.Host := vl.GetDefaultValue('Host', '');
                    a2.RecvPort := vl.GetDefaultValue('RecvPort', 0);
                    a2.SendPort := vl.GetDefaultValue('SendPort', 0);
                    a2.WorkLoad := vl.GetDefaultValue('WorkLoad', 0);
                    a2.ServerType := vl.GetDefaultValue('Type', TServerType.stUnknow);
                    if Assigned(ResultProc) then
                      begin
                        ResultProc(True, a2);
                      end;
                    ServerList.Add(a2);
                    vl.Clear;
                  end;
              end
            else
              begin
                a2.Init;
                if Assigned(ResultProc) then
                    ResultProc(False, a2);
              end;

            DisposeObject(vl);
          end);

        DisposeObject(sendDE);
      end
    else
      begin
        a2.Init;
        if Assigned(ResultProc) then
            ResultProc(False, a2);
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
  a2: TAddressInfo;
begin
  Result := True;
  for a2 in ServerList do
    if a2.ServerType = ServerType then
        exit;
  Result := False;
end;

procedure TManagerQueryBase.Clear;
begin
  ServerList.Clear;
end;

end.
