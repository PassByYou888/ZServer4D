unit ManagerServerQueryClient;

interface

uses System.IOUtils,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine;

type
  TAddressInfo = record
    RegName: string;
    Host: string;
    RecvPort, SendPort: Word;
    WorkLoad: Integer;
    ServerType: Byte;
  end;

  TManagerQueryBase = class
  protected
    procedure Query_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine); virtual;
  public
    Client    : TCommunicationFrameworkClient;
    ServerList: TGenericsList<TAddressInfo>;

    constructor Create(AClientIntf: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure Connect(Addr: string; Port: Word); virtual;
    function Query(Addr: string): Boolean;
    procedure Disconnect;
    function ExistsServerType(ServerType: Byte): Boolean;
    procedure Clear;

    procedure Progress;
  end;

const
  cDBServer        = 1;
  cCoreLogicServer = 2;
  cManagerServer   = 3;
  cPayService      = 61;
  cPayQueryService = 88;
  cUnknowServer    = 99;

function serverType2Str(t: Byte): string;

implementation

function serverType2Str(t: Byte): string;
begin
  case t of
    cDBServer: Result := 'DB_Server';
    cCoreLogicServer: Result := 'CoreLogic_Server';
    cManagerServer: Result := 'manager_Server';
    cPayService: Result := 'payment Server';
    cPayQueryService: Result := 'payment query Server';
    else Result := 'unknow_server';
  end;
end;

procedure TManagerQueryBase.Query_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
var
  vl: THashVariantList;
  a : TAddressInfo;
begin
  vl := THashVariantList.Create;

  while not ResultData.Reader.IsEnd do
    begin
      ResultData.Reader.ReadVariantList(vl);
      a.RegName := vl.GetDefaultValue('Name', '');
      a.Host := vl.GetDefaultValue('Host', '');
      a.RecvPort := vl.GetDefaultValue('RecvPort', 0);
      a.SendPort := vl.GetDefaultValue('SendPort', 0);
      a.WorkLoad := vl.GetDefaultValue('WorkLoad', 0);
      a.ServerType := vl.GetDefaultValue('Type', cUnknowServer);
      ServerList.Add(a);
      vl.Clear;
    end;

  DisposeObject(vl);
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

function TManagerQueryBase.Query(Addr: string): Boolean;
var
  sendDE: TDataFrameEngine;
begin
  Result := False;
  ServerList.Clear;
  try
    Connect(Addr, 8388);
    if (Client.Connected) and (Client.RemoteInited) then
      begin
        sendDE := TDataFrameEngine.Create;
        Client.SendStreamCmd('Query', sendDE, Query_StreamResult);
        DisposeObject(sendDE);

        Client.Wait(10000);

        Result := True;
        Disconnect;
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

function TManagerQueryBase.ExistsServerType(ServerType: Byte): Boolean;
var
  a: TAddressInfo;
begin
  for a in ServerList do
    begin
      if a.ServerType = ServerType then
          exit(True);
    end;
  Result := False;
end;

procedure TManagerQueryBase.Clear;
begin
  ServerList.Clear;
end;

end.
