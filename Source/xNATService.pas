{ ****************************************************************************** }
{ * x Nat tunnel service support, written by QQ 600585@qq.com                  * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit xNATService;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib, ListEngine, TextDataEngine,
  CoreCipher, DataFrameEngine, MemoryStream64, CommunicationFramework, xNATPhysics;

type
  TXNATService = class;
  TXServerCustomProtocol = class;
  TXServiceListen = class;

  TXServiceRecvVM_Special = class(TPeerClientUserSpecial)
  private
    OwnerMapping: TXServiceListen;
    RecvID, SendID: Cardinal;
  public
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXServiceSendVM_Special = class(TPeerClientUserSpecial)
  private
    OwnerMapping: TXServiceListen;
    RecvID, SendID: Cardinal;
  public
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXCustomP2PVM_Server = class(TCommunicationFrameworkWithP2PVM_Server)
  private
    OwnerMapping: TXServiceListen;
  end;

  TXServiceListen = class(TCoreClassObject)
  private
    ListenAddr: TPascalString;
    ListenPort: TPascalString;
    Mapping: TPascalString;

    Protocol: TXServerCustomProtocol;
    FActivted: Boolean;

    RecvTunnel: TXCustomP2PVM_Server;
    RecvTunnel_IPV6: TIPV6;
    RecvTunnel_Port: Word;

    SendTunnel: TXCustomP2PVM_Server;
    SendTunnel_IPV6: TIPV6;
    SendTunnel_Port: Word;

    XServerTunnel: TXNATService;

    procedure Init;
    function Open: Boolean;

    // seealso double tunnel:link
    procedure cmd_RequestListen(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    // connect forward
    procedure cmd_connect_reponse(Sender: TPeerIO; InData: TDataFrameEngine);
    procedure cmd_disconnect_reponse(Sender: TPeerIO; InData: TDataFrameEngine);
    // data forward
    procedure cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    // states
    procedure SetActivted(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property Activted: Boolean read FActivted write SetActivted;
  end;

  TXServerUserSpecial = class(TPeerClientUserSpecial)
  private
    RemoteProtocol_ID: Cardinal;
    RemoteProtocol_Inited: Boolean;
    RequestBuffer: TMemoryStream64;
  public
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXServerCustomProtocol = class(TXPhysicsServer)
  private
    ShareListen: TXServiceListen;
  public
    procedure OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt); override;
    procedure DoClientConnectBefore(Sender: TPeerIO); override;
    procedure DoClientDisconnect(Sender: TPeerIO); override;
  end;

  TXNATService = class(TCoreClassInterfacedObject, ICommunicationFrameworkVMInterface)
  private
    // external tunnel
    ShareListenList: TCoreClassListForObj;
    // internal physics tunnel
    PhysicsEngine: TXPhysicsServer;
  protected
    // physis protocol
    procedure IPV6Listen(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    // p2p VM intf
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
  public
    // tunnel parameter
    TunnelListenAddr: TPascalString;
    TunnelListenPort: TPascalString;
    AuthToken: TPascalString;
    MaxVMFragment, MaxRealBuffer: TPascalString;
    constructor Create;
    destructor Destroy; override;
    procedure AddMapping(const ListenAddr, ListenPort, Mapping: TPascalString);
    procedure OpenTunnel;
    procedure Progress;
  end;

procedure BuildBuff(buff: PByte; siz: NativeInt; local_id, remote_id: Cardinal; var NewSiz: NativeInt; var NewBuff: PByte);
procedure FillBuff(sour: PByte; siz: NativeInt; var local_id, remote_id: Cardinal; var destSiz: NativeInt; var destBuff: PByte);

implementation

procedure BuildBuff(buff: PByte; siz: NativeInt; local_id, remote_id: Cardinal; var NewSiz: NativeInt; var NewBuff: PByte);
var
  nb: PByte;
begin
  NewSiz := siz + 8;
  nb := System.GetMemory(NewSiz);
  NewBuff := nb;
  PCardinal(nb)^ := local_id;
  inc(nb, 4);
  PCardinal(nb)^ := remote_id;
  inc(nb, 4);
  CopyPtr(buff, nb, siz);
end;

procedure FillBuff(sour: PByte; siz: NativeInt; var local_id, remote_id: Cardinal; var destSiz: NativeInt; var destBuff: PByte);
begin
  destSiz := siz - 8;
  local_id := PCardinal(sour)^;
  inc(sour, 4);
  remote_id := PCardinal(sour)^;
  inc(sour, 4);
  destBuff := sour;
end;

constructor TXServiceRecvVM_Special.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
  OwnerMapping := nil;
  RecvID := 0;
  SendID := 0;
end;

destructor TXServiceRecvVM_Special.Destroy;
begin
  if (OwnerMapping <> nil) then
    begin
      if (OwnerMapping.Activted) then
          OwnerMapping.Activted := False;

      OwnerMapping.SendTunnel.Disconnect(SendID);
    end;

  inherited Destroy;
end;

constructor TXServiceSendVM_Special.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
  OwnerMapping := nil;
  RecvID := 0;
  SendID := 0;
end;

destructor TXServiceSendVM_Special.Destroy;
begin
  if (OwnerMapping <> nil) then
    begin
      if (OwnerMapping.Activted) then
          OwnerMapping.Activted := False;

      OwnerMapping.RecvTunnel.Disconnect(RecvID);
    end;

  inherited Destroy;
end;

procedure TXServiceListen.Init;
begin
  ListenAddr := '';
  ListenPort := '0';
  Mapping := '';
  Protocol := nil;
  FActivted := False;
  RecvTunnel := nil;
  FillPtrByte(@RecvTunnel_IPV6, SizeOf(TIPV6), 0);
  RecvTunnel_Port := 0;
  SendTunnel := nil;
  FillPtrByte(@SendTunnel_IPV6, SizeOf(TIPV6), 0);
  SendTunnel_Port := 0;
  XServerTunnel := nil;
end;

function TXServiceListen.Open: Boolean;
begin
  // build receive tunnel
  if RecvTunnel = nil then
      RecvTunnel := TXCustomP2PVM_Server.Create;

  // mapping interface
  RecvTunnel.OwnerMapping := Self;
  RecvTunnel.UserSpecialClass := TXServiceRecvVM_Special;
  // build virtual address
  PNativeUInt(@RecvTunnel_IPV6)^ := NativeUInt(@RecvTunnel);
  // build virtual port
  RecvTunnel_Port := umlCRC16(@RecvTunnel_IPV6, SizeOf(TIPV6));
  // disable data status print
  RecvTunnel.PrintParams['connect_reponse'] := False;
  RecvTunnel.PrintParams['disconnect_reponse'] := False;
  RecvTunnel.PrintParams['data'] := False;

  if not RecvTunnel.ExistsRegistedCmd('RequestListen') then
      RecvTunnel.RegisterStream('RequestListen').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RequestListen;

  if not RecvTunnel.ExistsRegistedCmd('connect_reponse') then
      RecvTunnel.RegisterDirectStream('connect_reponse').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_connect_reponse;

  if not RecvTunnel.ExistsRegistedCmd('disconnect_reponse') then
      RecvTunnel.RegisterDirectStream('disconnect_reponse').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_disconnect_reponse;

  if not RecvTunnel.ExistsRegistedCmd('data') then
      RecvTunnel.RegisterCompleteBuffer('data').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_data;

  // build send tunnel
  if SendTunnel = nil then
      SendTunnel := TXCustomP2PVM_Server.Create;
  // mapping interface
  SendTunnel.OwnerMapping := Self;
  SendTunnel.UserSpecialClass := TXServiceSendVM_Special;
  // build virtual address
  PNativeUInt(@SendTunnel_IPV6)^ := NativeUInt(@SendTunnel);
  // build virtual port
  SendTunnel_Port := umlCRC16(@SendTunnel_IPV6, SizeOf(TIPV6));
  // disable data status print
  SendTunnel.PrintParams['connect_request'] := False;
  SendTunnel.PrintParams['disconnect_request'] := False;
  SendTunnel.PrintParams['data'] := False;

  RecvTunnel.StartService(IPv6ToStr(RecvTunnel_IPV6), RecvTunnel_Port);
  SendTunnel.StartService(IPv6ToStr(SendTunnel_IPV6), SendTunnel_Port);

  if Protocol = nil then
      Protocol := TXServerCustomProtocol.Create;
  Protocol.ShareListen := Self;
  Protocol.Protocol := cpCustom;
  Protocol.UserSpecialClass := TXServerUserSpecial;

  SetActivted(True);
  Result := FActivted;
  SetActivted(False);

  if not Result then
      DoStatus('detect listen bind %s:%s failed!', [TranslateBindAddr(ListenAddr), ListenPort.Text]);
end;

procedure TXServiceListen.cmd_RequestListen(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RecvID, SendID: Cardinal;
  rVM: TXServiceRecvVM_Special;
  sVM: TXServiceSendVM_Special;
begin
  if Activted then
    begin
      OutData.WriteBool(False);
      exit;
    end;

  RecvID := InData.Reader.ReadCardinal;
  SendID := InData.Reader.ReadCardinal;

  if not RecvTunnel.Exists(RecvID) then
    begin
      OutData.WriteBool(False);
      exit;
    end;

  if not SendTunnel.Exists(SendID) then
    begin
      OutData.WriteBool(False);
      exit;
    end;

  Activted := True;
  if not Activted then
    begin
      OutData.WriteBool(False);
      exit;
    end;

  rVM := TXServiceRecvVM_Special(RecvTunnel.ClientFromID[RecvID].UserSpecial);
  rVM.OwnerMapping := Self;
  rVM.RecvID := RecvID;
  rVM.SendID := SendID;

  sVM := TXServiceSendVM_Special(SendTunnel.ClientFromID[SendID].UserSpecial);
  sVM.OwnerMapping := Self;
  sVM.RecvID := RecvID;
  sVM.SendID := SendID;

  OutData.WriteBool(True);
end;

procedure TXServiceListen.cmd_connect_reponse(Sender: TPeerIO; InData: TDataFrameEngine);
var
  cState: Boolean;
  remote_id, local_id: Cardinal;
  phy_io: TPeerIO;
  io: TXServerUserSpecial;
  nSiz: NativeInt;
  nBuff: PByte;
begin
  cState := InData.Reader.ReadBool;
  remote_id := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;
  phy_io := Protocol.ClientFromID[local_id];

  if phy_io = nil then
      exit;

  if cState then
    begin
      io := TXServerUserSpecial(phy_io.UserSpecial);
      io.RemoteProtocol_ID := remote_id;
      io.RemoteProtocol_Inited := True;

      if io.RequestBuffer.Size > 0 then
        begin
          BuildBuff(io.RequestBuffer.Memory, io.RequestBuffer.Size, Sender.ID, io.RemoteProtocol_ID, nSiz, nBuff);
          SendTunnel.SendCompleteBuffer(SendTunnel.FirstClient, 'data', nBuff, nSiz, True);
          io.RequestBuffer.Clear;
        end;
    end
  else
      phy_io.Disconnect;
end;

procedure TXServiceListen.cmd_disconnect_reponse(Sender: TPeerIO; InData: TDataFrameEngine);
var
  remote_id, local_id: Cardinal;
  phy_io: TPeerIO;
begin
  remote_id := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;
  phy_io := Protocol.ClientFromID[local_id];

  if phy_io <> nil then
      phy_io.Disconnect;
end;

procedure TXServiceListen.cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  local_id, remote_id: Cardinal;
  destSiz: NativeInt;
  destBuff: PByte;
  phy_io: TPeerIO;
begin
  FillBuff(InData, DataSize, remote_id, local_id, destSiz, destBuff);
  phy_io := Protocol.ClientFromID[local_id];

  if phy_io <> nil then
      Protocol.WriteBuffer(phy_io, destBuff, destSiz);
end;

procedure TXServiceListen.SetActivted(const Value: Boolean);
begin
  if Value then
    begin
      FActivted := Protocol.StartService(ListenAddr, umlStrToInt(ListenPort));
      DoStatus('Start listen %s %s', [TranslateBindAddr(ListenAddr.Text), ListenPort.Text]);
    end
  else
    begin
      Protocol.StopService;
      FActivted := False;
      DoStatus('Close listen %s %s', [TranslateBindAddr(ListenAddr.Text), ListenPort.Text]);
    end;
end;

constructor TXServiceListen.Create;
begin
  inherited Create;
  Init;
end;

destructor TXServiceListen.Destroy;
begin
  if Protocol <> nil then
    begin
      Protocol.StopService;
      DisposeObject(Protocol);
    end;

  if RecvTunnel <> nil then
    begin
      RecvTunnel.StopService;
      DisposeObject(RecvTunnel);
    end;

  if SendTunnel <> nil then
    begin
      SendTunnel.StopService;
      DisposeObject(SendTunnel);
    end;

  inherited Destroy;
end;

constructor TXServerUserSpecial.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
  RemoteProtocol_ID := 0;
  RemoteProtocol_Inited := False;
  RequestBuffer := TMemoryStream64.Create;
end;

destructor TXServerUserSpecial.Destroy;
begin
  DisposeObject(RequestBuffer);
  inherited Destroy;
end;

procedure TXServerCustomProtocol.OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt);
var
  io: TXServerUserSpecial;
  nSiz: NativeInt;
  nBuff: PByte;
begin
  if ShareListen.SendTunnel.Count <> 1 then
    begin
      Sender.Print('share listen "%s:%s" no remote support', [ShareListen.ListenAddr.Text, ShareListen.ListenPort.Text]);
      exit;
    end;

  io := TXServerUserSpecial(Sender.UserSpecial);
  if not io.RemoteProtocol_Inited then
    begin
      io.RequestBuffer.WritePtr(buffer, Size);
      exit;
    end;

  BuildBuff(buffer, Size, Sender.ID, io.RemoteProtocol_ID, nSiz, nBuff);
  ShareListen.SendTunnel.SendCompleteBuffer(ShareListen.SendTunnel.FirstClient, 'data', nBuff, nSiz, True);
end;

procedure TXServerCustomProtocol.DoClientConnectBefore(Sender: TPeerIO);
var
  de: TDataFrameEngine;
begin
  if ShareListen.SendTunnel.Count <> 1 then
    begin
      Sender.Print('share listen "%s:%s" no remote support', [ShareListen.ListenAddr.Text, ShareListen.ListenPort.Text]);
      exit;
    end;

  if TXServerUserSpecial(Sender.UserSpecial).RemoteProtocol_Inited then
      exit;

  de := TDataFrameEngine.Create;
  de.WriteCardinal(Sender.ID);
  ShareListen.SendTunnel.SendDirectStreamCmd(ShareListen.SendTunnel.FirstClient, 'connect_request', de);
  DisposeObject(de);
  inherited DoClientConnectAfter(Sender);
end;

procedure TXServerCustomProtocol.DoClientDisconnect(Sender: TPeerIO);
var
  de: TDataFrameEngine;
begin
  if ShareListen.SendTunnel.Count <> 1 then
    begin
      Sender.Print('share listen "%s:%s" no remote support', [ShareListen.ListenAddr.Text, ShareListen.ListenPort.Text]);
      exit;
    end;

  if not TXServerUserSpecial(Sender.UserSpecial).RemoteProtocol_Inited then
      exit;

  de := TDataFrameEngine.Create;
  de.WriteCardinal(Sender.ID);
  de.WriteCardinal(TXServerUserSpecial(Sender.UserSpecial).RemoteProtocol_ID);
  ShareListen.SendTunnel.SendDirectStreamCmd(ShareListen.SendTunnel.FirstClient, 'disconnect_request', de);
  DisposeObject(de);
  inherited DoClientDisconnect(Sender);
end;

procedure TXNATService.IPV6Listen(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  for i := 0 to ShareListenList.Count - 1 do
    begin
      shLt := ShareListenList[i] as TXServiceListen;
      OutData.WriteString(shLt.Mapping);

      OutData.WriteString(shLt.ListenAddr);
      OutData.WriteString(shLt.ListenPort);

      OutData.WriteString(IPv6ToStr(shLt.RecvTunnel_IPV6));
      OutData.WriteWORD(shLt.RecvTunnel_Port);

      OutData.WriteString(IPv6ToStr(shLt.SendTunnel_IPV6));
      OutData.WriteWORD(shLt.SendTunnel_Port);
    end;
end;

procedure TXNATService.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  {
    QuantumCryptographyPassword: used sha-3 shake256 cryptography as 512 bits password

    SHA-3 (Secure Hash Algorithm 3) is the latest member of the Secure Hash Algorithm family of standards,
    released by NIST on August 5, 2015.[4][5] Although part of the same series of standards,
    SHA-3 is internally quite different from the MD5-like structure of SHA-1 and SHA-2.

    Keccak is based on a novel approach called sponge construction.
    Sponge construction is based on a wide random function or random permutation, and allows inputting ("absorbing" in sponge terminology) any amount of data,
    and outputting ("squeezing") any amount of data,
    while acting as a pseudorandom function with regard to all previous inputs. This leads to great flexibility.

    NIST does not currently plan to withdraw SHA-2 or remove it from the revised Secure Hash Standard.
    The purpose of SHA-3 is that it can be directly substituted for SHA-2 in current applications if necessary,
    and to significantly improve the robustness of NIST's overall hash algorithm toolkit

    ref wiki
    https://en.wikipedia.org/wiki/SHA-3
  }
  Accept := CompareQuantumCryptographyPassword(AuthToken, Token);

  if not Accept then
      Sender.Print('p2p auth failed');
end;

procedure TXNATService.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  DoStatus('XTunnel Open Before on %s', [Sender.PeerIP]);
  for i := ShareListenList.Count - 1 downto 0 do
    begin
      shLt := ShareListenList[i] as TXServiceListen;
      Sender.p2pVM.MaxVMFragmentSize := umlStrToInt(MaxVMFragment, Sender.p2pVM.MaxVMFragmentSize);
      Sender.p2pVM.MaxRealBuffer := umlStrToInt(MaxRealBuffer, Sender.p2pVM.MaxRealBuffer);
      Sender.p2pVM.InstallLogicFramework(shLt.RecvTunnel);
      Sender.p2pVM.InstallLogicFramework(shLt.SendTunnel);
    end;
end;

procedure TXNATService.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  DoStatus('XTunnel Open on %s', [Sender.PeerIP]);
end;

procedure TXNATService.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  DoStatus('XTunnel Open After on %s', [Sender.PeerIP]);
end;

procedure TXNATService.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  DoStatus('XTunnel Close on %s', [Sender.PeerIP]);
  for i := ShareListenList.Count - 1 downto 0 do
    begin
      shLt := ShareListenList[i] as TXServiceListen;
      Sender.p2pVM.UnInstallLogicFramework(shLt.RecvTunnel);
      Sender.p2pVM.UnInstallLogicFramework(shLt.SendTunnel);
    end;
end;

constructor TXNATService.Create;
begin
  inherited Create;
  ShareListenList := TCoreClassListForObj.Create;

  // parameter
  TunnelListenAddr := '';
  TunnelListenPort := '4921';
  AuthToken := 'ZServer';
  MaxVMFragment := '200';
  MaxRealBuffer := umlIntToStr(2048 * 1024);
end;

destructor TXNATService.Destroy;
var
  i: Integer;
  shLt: TXServiceListen;
begin
  for i := 0 to ShareListenList.Count - 1 do
    begin
      shLt := ShareListenList[i] as TXServiceListen;
      DisposeObject(shLt);
    end;
  DisposeObject(ShareListenList);

  PhysicsEngine.StopService;
  DisposeObject(PhysicsEngine);

  inherited Destroy;
end;

procedure TXNATService.AddMapping(const ListenAddr, ListenPort, Mapping: TPascalString);
var
  shLt: TXServiceListen;
begin
  shLt := TXServiceListen.Create;
  shLt.ListenAddr := ListenAddr;
  shLt.ListenPort := ListenPort;
  shLt.Mapping := Mapping;
  shLt.XServerTunnel := Self;
  ShareListenList.Add(shLt);
end;

procedure TXNATService.OpenTunnel;
var
  i: Integer;
  shLt: TXServiceListen;
begin
  // init tunnel engine
  if PhysicsEngine = nil then
      PhysicsEngine := TXPhysicsServer.Create;
  PhysicsEngine.VMInterface := Self;
  // regsiter protocol
  if not PhysicsEngine.ExistsRegistedCmd('IPV6Listen') then
      PhysicsEngine.RegisterStream('IPV6Listen').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}IPV6Listen;
  // Security protocol
  PhysicsEngine.SwitchMaxSecurity;
  // start service
  if PhysicsEngine.StartService(TunnelListenAddr, umlStrToInt(TunnelListenPort)) then
      DoStatus('Tunnel Open %s:%s successed', [TranslateBindAddr(TunnelListenAddr), TunnelListenPort.Text])
  else
      DoStatus('error: Tunnel is Closed for %s:%s', [TranslateBindAddr(TunnelListenAddr), TunnelListenPort.Text]);

  // open share listen
  for i := 0 to ShareListenList.Count - 1 do
    begin
      shLt := ShareListenList[i] as TXServiceListen;
      shLt.Open;
    end;
end;

procedure TXNATService.Progress;
var
  i: Integer;
  shLt: TXServiceListen;
begin
  PhysicsEngine.Progress;
  for i := ShareListenList.Count - 1 downto 0 do
    begin
      shLt := ShareListenList[i] as TXServiceListen;
      try
        shLt.Protocol.Progress;
        shLt.RecvTunnel.Progress;
        shLt.SendTunnel.Progress;
      except
      end;
    end;
end;

end.
