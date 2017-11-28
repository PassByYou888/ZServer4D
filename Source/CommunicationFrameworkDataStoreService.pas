{ ****************************************************************************** }
{ * DataStore Service framework(incl File service)                             * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ ****************************************************************************** }

unit CommunicationFrameworkDataStoreService;

interface

{$I ..\zDefine.inc}


uses Classes,
  CoreClasses, ListEngine, UnicodeMixedLib, DataFrameEngine, MemoryStream64, CommunicationFramework, TextDataEngine,
  DoStatusIO, Cadencer, NotifyObjectBase, PascalStrings, CoreCipher, ZDBEngine, ItemStream,
  CommunicationFrameworkDoubleTunnelIO, ZDBLocalManager;

type
  TDataStoreService                      = class;
  TDataStoreService_PeerClientSendTunnel = class;

  TDataStoreService_PeerClientRecvTunnel = class(TPeerClientUserDefineForRecvTunnel)
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    function SendTunnelDefine: TDataStoreService_PeerClientSendTunnel;
  end;

  TDataStoreService_PeerClientSendTunnel = class(TPeerClientUserDefineForSendTunnel)
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    function RecvTunnelDefine: TDataStoreService_PeerClientRecvTunnel;
  end;

  TDataStoreService = class(TCommunicationFramework_DoubleTunnelService)
  private
    FZDBLocal: TZDBLocalManager;
  protected
    procedure Command_InitDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_CloseDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_QueryDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_RequestDownloadAssembleStream(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    // Append data assemble in service
    procedure Command_AppendAssemble(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_RequestAppendBigStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_AppendAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;

    // Insert data assemble in service
    procedure Command_InsertAssemble(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_RequestInsertBigStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_InsertAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;

    // Modify data assemble in service
    procedure Command_ModifyAssemble(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_RequestModifyBigStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_ModifyAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;

    // delete
    procedure Command_DeleteData(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    // base information
    procedure Command_GetDBInfo(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetDBStoreInfo(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;

    function GetDataStoreUserDefine(RecvCli: TPeerClient): TDataStoreService_PeerClientRecvTunnel;

    // send client command
    procedure Send_BeginQuery(SendID: Cardinal; pipeName: string);
    procedure Send_EndQuery(SendID: Cardinal; pipeName: string);
  end;

  TDataStoreClient = class(TCommunicationFramework_DoubleTunnelClient)
  protected
    procedure Command_BeginQuery(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_FragmentBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
    procedure Command_EndQuery(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;

    procedure PostStreamToDB(dbN: string; stream: TCoreClassStream; UserProperty: Cardinal);
  end;

implementation


constructor TDataStoreService_PeerClientRecvTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TDataStoreService_PeerClientRecvTunnel.Destroy;
begin
  inherited Destroy;
end;

function TDataStoreService_PeerClientRecvTunnel.SendTunnelDefine: TDataStoreService_PeerClientSendTunnel;
begin
  Result := SendTunnel as TDataStoreService_PeerClientSendTunnel;
end;

constructor TDataStoreService_PeerClientSendTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TDataStoreService_PeerClientSendTunnel.Destroy;
begin
  inherited Destroy;
end;

function TDataStoreService_PeerClientSendTunnel.RecvTunnelDefine: TDataStoreService_PeerClientRecvTunnel;
begin
  Result := RecvTunnel as TDataStoreService_PeerClientRecvTunnel;
end;

procedure TDataStoreService.Command_InitDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
end;

procedure TDataStoreService.Command_CloseDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_QueryDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_RequestDownloadAssembleStream(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_AppendAssemble(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_RequestAppendBigStream(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_AppendAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin

end;

procedure TDataStoreService.Command_InsertAssemble(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_RequestInsertBigStream(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_InsertAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin

end;

procedure TDataStoreService.Command_ModifyAssemble(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_RequestModifyBigStream(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_ModifyAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin

end;

procedure TDataStoreService.Command_DeleteData(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_GetDBInfo(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_GetDBStoreInfo(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin

end;

constructor TDataStoreService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  FRecvTunnel.PeerClientUserDefineClass := TDataStoreService_PeerClientRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TDataStoreService_PeerClientSendTunnel;

  FZDBLocal := TZDBLocalManager.Create;
end;

destructor TDataStoreService.Destroy;
begin
  DisposeObject([FZDBLocal]);
  inherited Destroy;
end;

procedure TDataStoreService.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TDataStoreService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

procedure TDataStoreService.Progress;
begin
  inherited Progress;
end;

function TDataStoreService.GetDataStoreUserDefine(RecvCli: TPeerClient): TDataStoreService_PeerClientRecvTunnel;
begin
  Result := RecvCli.UserDefine as TDataStoreService_PeerClientRecvTunnel;
end;

procedure TDataStoreService.Send_BeginQuery(SendID: Cardinal; pipeName: string);
begin

end;

procedure TDataStoreService.Send_EndQuery(SendID: Cardinal; pipeName: string);
begin

end;

procedure TDataStoreClient.Command_BeginQuery(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreClient.Command_FragmentBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin

end;

procedure TDataStoreClient.Command_EndQuery(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

constructor TDataStoreClient.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
end;

destructor TDataStoreClient.Destroy;
begin
  inherited Destroy;
end;

procedure TDataStoreClient.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TDataStoreClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

procedure TDataStoreClient.Progress;
begin
  inherited Progress;
end;

procedure TDataStoreClient.PostStreamToDB(dbN: string; stream: TCoreClassStream; UserProperty: Cardinal);
begin

end;

end.
