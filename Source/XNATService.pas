{ ****************************************************************************** }
{ * XNAT tunnel                  written by QQ 600585@qq.com                   * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit XNATService;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib, ListEngine, TextDataEngine,
  CoreCipher, DataFrameEngine, MemoryStream64, CommunicationFramework, XNATPhysics;

type
  TXNATService = class;
  TXServerCustomProtocol = class;
  TXServiceListen = class;

  TXServiceRecvVM_Special = class(TPeerClientUserSpecial)
  private
    OwnerMapping: TXServiceListen;
    RecvID, SendID: Cardinal;
    MaxWorkload, CurrentWorkload: Cardinal;
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXServiceSendVM_Special = class(TPeerClientUserSpecial)
  private
    OwnerMapping: TXServiceListen;
    RecvID, SendID: Cardinal;
  public
    constructor Create(Owner_: TPeerIO); override;
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

    { Distributed Workload supported }
    DistributedWorkload: Boolean;

    XServerTunnel: TXNATService;
    TimeOut: TTimeTick;

    procedure Init;
    function Open: Boolean;

    { worker tunnel }
    procedure PickWorkloadTunnel(var rID, sID: Cardinal);

    { requestListen: activted listen and reponse states }
    procedure cmd_RequestListen(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    { workload: update workload states }
    procedure cmd_workload(Sender: TPeerIO; InData: TDataFrameEngine);
    { connect forward }
    procedure cmd_connect_reponse(Sender: TPeerIO; InData: TDataFrameEngine);
    procedure cmd_disconnect_reponse(Sender: TPeerIO; InData: TDataFrameEngine);
    { data forward }
    procedure cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    { states }
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
    r_id, s_id: Cardinal; { IO in TXServiceListen }
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXServerCustomProtocol = class(TXPhysicsServer)
  private
    ShareListen: TXServiceListen;
  public
    procedure OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
    procedure DoIOConnectBefore(Sender: TPeerIO); override;
    procedure DoIODisconnect(Sender: TPeerIO); override;
  end;

  TPhysicsEngine_Special = class(TPeerIOUserSpecial)
  protected
    XNAT: TXNATService;
    procedure PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
    procedure PhysicsVMBuildAuthToken_Result;
    procedure PhysicsOpenVM_Result(const cState: Boolean);
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXNATService = class(TCoreClassInterfacedObject, IIOInterface, ICommunicationFrameworkVMInterface)
  private
    { external tunnel }
    ShareListenList: TCoreClassListForObj;
    { internal physics tunnel }
    PhysicsEngine: TCommunicationFramework;
    Activted: Boolean;
    WaitAsyncConnecting: Boolean;
    WaitAsyncConnecting_BeginTime: TTimeTick;

    { physis protocol }
    procedure IPV6Listen(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    { IO Interface }
    procedure PeerIO_Create(const Sender: TPeerIO);
    procedure PeerIO_Destroy(const Sender: TPeerIO);
    { p2pVM Interface }
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    { backcall }
    procedure PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
  public
    { tunnel parameter }
    Host: TPascalString;
    Port: TPascalString;
    AuthToken: TPascalString;
    MaxVMFragment, MaxRealBuffer: TPascalString;
    {
      Compression of CompleteBuffer packets using zLib
      feature of zLib: slow compression and fast decompression.
      XNAT is used to non compression or non encryption protocol, the option can be opened so upspeed.
      else. protocol is encrypted or compressed, opening this ProtocolCompressed additional burden on CPU.
      ProtocolCompressed set closed by default.
    }
    ProtocolCompressed: Boolean;

    constructor Create;
    destructor Destroy; override;
    procedure AddMapping(const ListenAddr, ListenPort, Mapping: TPascalString; TimeOut: TTimeTick);
    procedure AddNoDistributedMapping(const ListenAddr, ListenPort, Mapping: TPascalString; TimeOut: TTimeTick);
    procedure OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL); overload;
    procedure OpenTunnel; overload;
    procedure Progress;
  end;

implementation

constructor TXServiceRecvVM_Special.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  OwnerMapping := nil;
  RecvID := 0;
  SendID := 0;
  MaxWorkload := 100;
  CurrentWorkload := 0;
end;

destructor TXServiceRecvVM_Special.Destroy;
var
  IO_Array: TIO_Array;
  p_id: Cardinal;
  p_io: TPeerIO;
begin
  if (OwnerMapping <> nil) then
    begin
      OwnerMapping.SendTunnel.Disconnect(SendID);

      OwnerMapping.Protocol.GetIO_Array(IO_Array);
      for p_id in IO_Array do
        begin
          p_io := OwnerMapping.Protocol.PeerIO[p_id];
          if TXServerUserSpecial(p_io.UserSpecial).r_id = Owner.ID then
              p_io.DelayClose(0);
        end;
    end;

  inherited Destroy;
end;

constructor TXServiceSendVM_Special.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  OwnerMapping := nil;
  RecvID := 0;
  SendID := 0;
end;

destructor TXServiceSendVM_Special.Destroy;
begin
  if (OwnerMapping <> nil) then
      OwnerMapping.RecvTunnel.Disconnect(RecvID);

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
  DistributedWorkload := False;
  XServerTunnel := nil;
  TimeOut := 0;
end;

function TXServiceListen.Open: Boolean;
var
  nt: Pointer;
begin
  { build receive tunnel }
  if RecvTunnel = nil then
      RecvTunnel := TXCustomP2PVM_Server.Create;

  { sequence sync }
  RecvTunnel.SyncOnCompleteBuffer := True;
  RecvTunnel.SyncOnResult := True;
  RecvTunnel.SwitchMaxPerformance;
  { mapping interface }
  RecvTunnel.OwnerMapping := Self;
  RecvTunnel.UserSpecialClass := TXServiceRecvVM_Special;
  { compressed complete buffer }
  RecvTunnel.CompleteBufferCompressed := XServerTunnel.ProtocolCompressed;
  { build virtual address }
  nt := @RecvTunnel;
  TSHA3.SHAKE128(@RecvTunnel_IPV6, @nt, SizeOf(nt), 128);
  { build virtual port }
  RecvTunnel_Port := umlCRC16(@RecvTunnel_IPV6, SizeOf(TIPV6));
  { disable data status print }
  RecvTunnel.PrintParams[C_Connect_reponse] := False;
  RecvTunnel.PrintParams[C_Disconnect_reponse] := False;
  RecvTunnel.PrintParams[C_Data] := False;
  RecvTunnel.PrintParams[C_Workload] := False;

  if not RecvTunnel.ExistsRegistedCmd(C_RequestListen) then
      RecvTunnel.RegisterStream(C_RequestListen).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RequestListen;

  if not RecvTunnel.ExistsRegistedCmd(C_Workload) then
      RecvTunnel.RegisterDirectStream(C_Workload).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_workload;

  if not RecvTunnel.ExistsRegistedCmd(C_Connect_reponse) then
      RecvTunnel.RegisterDirectStream(C_Connect_reponse).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_connect_reponse;

  if not RecvTunnel.ExistsRegistedCmd(C_Disconnect_reponse) then
      RecvTunnel.RegisterDirectStream(C_Disconnect_reponse).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_disconnect_reponse;

  if not RecvTunnel.ExistsRegistedCmd(C_Data) then
      RecvTunnel.RegisterCompleteBuffer(C_Data).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_data;

  { build send tunnel }
  if SendTunnel = nil then
      SendTunnel := TXCustomP2PVM_Server.Create;

  { sequence sync }
  SendTunnel.SyncOnCompleteBuffer := True;
  SendTunnel.SyncOnResult := True;
  SendTunnel.SwitchMaxPerformance;
  { mapping interface }
  SendTunnel.OwnerMapping := Self;
  SendTunnel.UserSpecialClass := TXServiceSendVM_Special;
  { compressed complete buffer }
  SendTunnel.CompleteBufferCompressed := XServerTunnel.ProtocolCompressed;
  { build virtual address }
  nt := @SendTunnel;
  TSHA3.SHAKE128(@SendTunnel_IPV6, @nt, SizeOf(nt), 128);
  { build virtual port }
  SendTunnel_Port := umlCRC16(@SendTunnel_IPV6, SizeOf(TIPV6));
  { disable data status print }
  SendTunnel.PrintParams[C_Connect_request] := False;
  SendTunnel.PrintParams[C_Disconnect_request] := False;
  SendTunnel.PrintParams[C_Data] := False;

  RecvTunnel.StartService(IPv6ToStr(RecvTunnel_IPV6), RecvTunnel_Port);
  SendTunnel.StartService(IPv6ToStr(SendTunnel_IPV6), SendTunnel_Port);

  if Protocol = nil then
      Protocol := TXServerCustomProtocol.Create;
  Protocol.ShareListen := Self;
  Protocol.Protocol := cpCustom;
  Protocol.UserSpecialClass := TXServerUserSpecial;
  Protocol.TimeOutIDLE := TimeOut;

  SetActivted(True);
  Result := FActivted;
  SetActivted(False);

  if not Result then
      DoStatus('detect listen bind %s:%s failed!', [TranslateBindAddr(ListenAddr), ListenPort.Text]);
end;

procedure TXServiceListen.PickWorkloadTunnel(var rID, sID: Cardinal);
var
  rVM: TXServiceRecvVM_Special;
  buff: TIO_Array;
  ID: Cardinal;
  r_io: TPeerIO;
  f, d: Double;
begin
  rID := 0;
  sID := 0;
  if RecvTunnel.Count = 0 then
      exit;
  if SendTunnel.Count = 0 then
      exit;

  rVM := TXServiceRecvVM_Special(RecvTunnel.FirstIO.UserSpecial);
  f := rVM.CurrentWorkload / rVM.MaxWorkload;

  RecvTunnel.GetIO_Array(buff);
  for ID in buff do
    begin
      r_io := RecvTunnel.PeerIO[ID];
      if (r_io <> nil) and (r_io.UserSpecial <> rVM) then
        begin
          with TXServiceRecvVM_Special(r_io.UserSpecial) do
              d := CurrentWorkload / MaxWorkload;
          if d < f then
            begin
              f := d;
              rVM := TXServiceRecvVM_Special(r_io.UserSpecial);
            end;
        end;
    end;

  if not SendTunnel.Exists(rVM.SendID) then
      exit;

  rID := rVM.RecvID;
  sID := rVM.SendID;
end;

procedure TXServiceListen.cmd_RequestListen(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RecvID, SendID: Cardinal;
  rVM: TXServiceRecvVM_Special;
  sVM: TXServiceSendVM_Special;
begin
  RecvID := InData.Reader.ReadCardinal;
  SendID := InData.Reader.ReadCardinal;

  if DistributedWorkload then
    begin
      if not RecvTunnel.Exists(RecvID) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('receive tunnel ID illegal %d', [RecvID]));
          exit;
        end;

      if not SendTunnel.Exists(SendID) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('send tunnel ID illegal %d', [SendID]));
          exit;
        end;

      if not Activted then
        begin
          Activted := True;
          if (not Activted) then
            begin
              OutData.WriteBool(False);
              OutData.WriteString(PFormat('remote service illegal bind IP %s port:%s', [ListenAddr.Text, ListenPort.Text]));
              exit;
            end;
        end;

      rVM := TXServiceRecvVM_Special(RecvTunnel.PeerIO[RecvID].UserSpecial);
      rVM.OwnerMapping := Self;
      rVM.RecvID := RecvID;
      rVM.SendID := SendID;

      sVM := TXServiceSendVM_Special(SendTunnel.PeerIO[SendID].UserSpecial);
      sVM.OwnerMapping := Self;
      sVM.RecvID := RecvID;
      sVM.SendID := SendID;

      OutData.WriteBool(True);
      OutData.WriteString(PFormat('bridge XNAT service successed, bind IP %s port:%s', [ListenAddr.Text, ListenPort.Text]));
    end
  else
    begin
      if Activted then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('bridge service no support distributed workload', []));
          exit;
        end;

      if not RecvTunnel.Exists(RecvID) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('receive tunnel ID illegal %d', [RecvID]));
          exit;
        end;

      if not SendTunnel.Exists(SendID) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('send tunnel ID illegal %d', [SendID]));
          exit;
        end;

      Activted := True;
      if (not Activted) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('remote service illegal bind IP %s port:%s', [ListenAddr.Text, ListenPort.Text]));
          exit;
        end;

      rVM := TXServiceRecvVM_Special(RecvTunnel.PeerIO[RecvID].UserSpecial);
      rVM.OwnerMapping := Self;
      rVM.RecvID := RecvID;
      rVM.SendID := SendID;

      sVM := TXServiceSendVM_Special(SendTunnel.PeerIO[SendID].UserSpecial);
      sVM.OwnerMapping := Self;
      sVM.RecvID := RecvID;
      sVM.SendID := SendID;

      OutData.WriteBool(True);
      OutData.WriteString(PFormat('bridge XNAT service successed, bind IP %s port:%s', [ListenAddr.Text, ListenPort.Text]));
    end;
end;

procedure TXServiceListen.cmd_workload(Sender: TPeerIO; InData: TDataFrameEngine);
var
  rVM: TXServiceRecvVM_Special;
begin
  rVM := TXServiceRecvVM_Special(Sender.UserSpecial);
  rVM.MaxWorkload := InData.Reader.ReadCardinal;
  rVM.CurrentWorkload := InData.Reader.ReadCardinal;
end;

procedure TXServiceListen.cmd_connect_reponse(Sender: TPeerIO; InData: TDataFrameEngine);
var
  cState: Boolean;
  remote_id, local_id: Cardinal;
  phy_io, s_io: TPeerIO;
  xUserSpec: TXServerUserSpecial;
  nSiz: NativeInt;
  nBuff: PByte;
begin
  cState := InData.Reader.ReadBool;
  remote_id := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;
  phy_io := Protocol.PeerIO[local_id];

  if phy_io = nil then
      exit;

  if cState then
    begin
      xUserSpec := TXServerUserSpecial(phy_io.UserSpecial);
      xUserSpec.RemoteProtocol_ID := remote_id;
      xUserSpec.RemoteProtocol_Inited := True;

      if xUserSpec.RequestBuffer.Size > 0 then
        begin
          s_io := SendTunnel.PeerIO[xUserSpec.s_id];
          if s_io <> nil then
            begin
              BuildBuff(xUserSpec.RequestBuffer.Memory, xUserSpec.RequestBuffer.Size, Sender.ID, xUserSpec.RemoteProtocol_ID, nSiz, nBuff);
              s_io.SendCompleteBuffer(C_Data, nBuff, nSiz, True);
            end;
          xUserSpec.RequestBuffer.Clear;
        end;
    end
  else
      phy_io.DelayClose;
end;

procedure TXServiceListen.cmd_disconnect_reponse(Sender: TPeerIO; InData: TDataFrameEngine);
var
  remote_id, local_id: Cardinal;
  phy_io: TPeerIO;
begin
  remote_id := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;
  phy_io := Protocol.PeerIO[local_id];

  if phy_io = nil then
      exit;

  phy_io.DelayClose(1.0);
end;

procedure TXServiceListen.cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  local_id, remote_id: Cardinal;
  destSiz: NativeInt;
  destBuff: PByte;
  phy_io: TPeerIO;
begin
  FillBuff(InData, DataSize, remote_id, local_id, destSiz, destBuff);
  phy_io := Protocol.PeerIO[local_id];

  if phy_io <> nil then
    begin
      Protocol.WriteBuffer(phy_io, destBuff, destSiz);
    end;
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

constructor TXServerUserSpecial.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  RemoteProtocol_ID := 0;
  RemoteProtocol_Inited := False;
  RequestBuffer := TMemoryStream64.Create;
  r_id := 0;
  s_id := 0;
end;

destructor TXServerUserSpecial.Destroy;
begin
  DisposeObject(RequestBuffer);
  inherited Destroy;
end;

procedure TXServerCustomProtocol.OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
var
  xUserSpec: TXServerUserSpecial;
  nSiz: NativeInt;
  nBuff: PByte;
  s_io: TPeerIO;
begin
  if (ShareListen.SendTunnel.Count <> 1) and (not ShareListen.DistributedWorkload) then
    begin
      Sender.Print('share listen "%s:%s" no remote support', [ShareListen.ListenAddr.Text, ShareListen.ListenPort.Text]);
      exit;
    end;

  xUserSpec := TXServerUserSpecial(Sender.UserSpecial);
  if not xUserSpec.RemoteProtocol_Inited then
    begin
      xUserSpec.RequestBuffer.WritePtr(buffer, Size);
      exit;
    end;

  s_io := ShareListen.SendTunnel.PeerIO[xUserSpec.s_id];
  if s_io <> nil then
    begin
      BuildBuff(buffer, Size, Sender.ID, xUserSpec.RemoteProtocol_ID, nSiz, nBuff);
      s_io.SendCompleteBuffer(C_Data, nBuff, nSiz, True);
    end
  else
      Sender.DelayClose(1.0);
end;

procedure TXServerCustomProtocol.DoIOConnectBefore(Sender: TPeerIO);
var
  de: TDataFrameEngine;
  xUserSpec: TXServerUserSpecial;
  s_io: TPeerIO;
begin
  if (ShareListen.SendTunnel.Count <> 1) and (not ShareListen.DistributedWorkload) then
    begin
      Sender.Print('share listen "%s:%s" no remote support', [ShareListen.ListenAddr.Text, ShareListen.ListenPort.Text]);
      exit;
    end;

  xUserSpec := TXServerUserSpecial(Sender.UserSpecial);

  if xUserSpec.RemoteProtocol_Inited then
      exit;

  ShareListen.PickWorkloadTunnel(xUserSpec.r_id, xUserSpec.s_id);

  if ShareListen.SendTunnel.Exists(xUserSpec.s_id) then
    begin
      s_io := ShareListen.SendTunnel.PeerIO[xUserSpec.s_id];
      de := TDataFrameEngine.Create;
      de.WriteCardinal(Sender.ID);
      de.WriteString(Sender.PeerIP);
      s_io.SendDirectStreamCmd(C_Connect_request, de);
      DisposeObject(de);
      s_io.Progress;
    end;
  inherited DoIOConnectBefore(Sender);
end;

procedure TXServerCustomProtocol.DoIODisconnect(Sender: TPeerIO);
var
  de: TDataFrameEngine;
  xUserSpec: TXServerUserSpecial;
  s_io: TPeerIO;
begin
  if (ShareListen.SendTunnel.Count <> 1) and (not ShareListen.DistributedWorkload) then
    begin
      Sender.Print('share listen "%s:%s" no remote support', [ShareListen.ListenAddr.Text, ShareListen.ListenPort.Text]);
      exit;
    end;

  xUserSpec := TXServerUserSpecial(Sender.UserSpecial);
  if not xUserSpec.RemoteProtocol_Inited then
      exit;

  if ShareListen.SendTunnel.Exists(xUserSpec.s_id) then
    begin
      s_io := ShareListen.SendTunnel.PeerIO[xUserSpec.s_id];
      de := TDataFrameEngine.Create;
      de.WriteCardinal(Sender.ID);
      de.WriteCardinal(TXServerUserSpecial(Sender.UserSpecial).RemoteProtocol_ID);
      s_io.SendDirectStreamCmd(C_Disconnect_request, de);
      DisposeObject(de);
      s_io.Progress;
    end;
  inherited DoIODisconnect(Sender);
end;

procedure TPhysicsEngine_Special.PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
begin
  if cState then
      Owner.BuildP2PAuthTokenM({$IFDEF FPC}@{$ENDIF FPC}PhysicsVMBuildAuthToken_Result)
  else
      XNAT.WaitAsyncConnecting := False;
end;

procedure TPhysicsEngine_Special.PhysicsVMBuildAuthToken_Result;
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
  Owner.OpenP2pVMTunnelM(True, GenerateQuantumCryptographyPassword(XNAT.AuthToken), {$IFDEF FPC}@{$ENDIF FPC}PhysicsOpenVM_Result)
end;

procedure TPhysicsEngine_Special.PhysicsOpenVM_Result(const cState: Boolean);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  if cState then
    begin
      Owner.p2pVMTunnel.MaxVMFragmentSize := umlStrToInt(XNAT.MaxVMFragment, Owner.p2pVMTunnel.MaxVMFragmentSize);
      Owner.p2pVMTunnel.MaxRealBuffer := umlStrToInt(XNAT.MaxRealBuffer, Owner.p2pVMTunnel.MaxRealBuffer);
      XNAT.Activted := True;

      { open share listen }
      for i := 0 to XNAT.ShareListenList.Count - 1 do
        begin
          shLt := XNAT.ShareListenList[i] as TXServiceListen;

          shLt.Open;

          { install p2pVM }
          Owner.p2pVMTunnel.InstallLogicFramework(shLt.SendTunnel);
          Owner.p2pVMTunnel.InstallLogicFramework(shLt.RecvTunnel);
        end;
    end;
  XNAT.WaitAsyncConnecting := False;
end;

constructor TPhysicsEngine_Special.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  XNAT := nil;
end;

destructor TPhysicsEngine_Special.Destroy;
begin
  inherited Destroy;
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

procedure TXNATService.PeerIO_Create(const Sender: TPeerIO);
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
      TPhysicsEngine_Special(Sender.UserSpecial).XNAT := Self;
    end;
end;

procedure TXNATService.PeerIO_Destroy(const Sender: TPeerIO);
begin
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

  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
    end;

  Accept := CompareQuantumCryptographyPassword(AuthToken, Token);
  if Accept then
      Sender.Print('p2pVM auth Successed!')
  else
      Sender.Print('p2pVM auth failed!');
end;

procedure TXNATService.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
      for i := ShareListenList.Count - 1 downto 0 do
        begin
          shLt := ShareListenList[i] as TXServiceListen;
          Sender.p2pVM.MaxVMFragmentSize := umlStrToInt(MaxVMFragment, Sender.p2pVM.MaxVMFragmentSize);
          Sender.p2pVM.MaxRealBuffer := umlStrToInt(MaxRealBuffer, Sender.p2pVM.MaxRealBuffer);
          Sender.p2pVM.InstallLogicFramework(shLt.RecvTunnel);
          Sender.p2pVM.InstallLogicFramework(shLt.SendTunnel);
        end;
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
    end;
  DoStatus('XTunnel Open Before on %s', [Sender.PeerIP]);
end;

procedure TXNATService.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
    end;
  DoStatus('XTunnel Open on %s', [Sender.PeerIP]);
end;

procedure TXNATService.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
    end;
  DoStatus('XTunnel Open After on %s', [Sender.PeerIP]);
end;

procedure TXNATService.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
      for i := ShareListenList.Count - 1 downto 0 do
        begin
          shLt := ShareListenList[i] as TXServiceListen;
          Sender.p2pVM.UnInstallLogicFramework(shLt.RecvTunnel);
          Sender.p2pVM.UnInstallLogicFramework(shLt.SendTunnel);
        end;
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
    end;
  DoStatus('XTunnel Close on %s', [Sender.PeerIP]);
end;

procedure TXNATService.PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
      if TCommunicationFrameworkClient(PhysicsEngine).ClientIO <> nil then
          TPhysicsEngine_Special(TCommunicationFrameworkClient(PhysicsEngine).ClientIO.UserSpecial).PhysicsConnect_Result_BuildP2PToken(cState);
    end;
end;

constructor TXNATService.Create;
begin
  inherited Create;
  ShareListenList := TCoreClassListForObj.Create;

  PhysicsEngine := nil;
  Activted := False;
  WaitAsyncConnecting := False;

  { parameter }
  Host := '';
  Port := '4921';
  AuthToken := 'ZServer';
  MaxVMFragment := '8192';
  MaxRealBuffer := umlIntToStr(2048 * 1024);
  ProtocolCompressed := False;
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

  if PhysicsEngine <> nil then
    begin
      if PhysicsEngine is TCommunicationFrameworkServer then
        begin
          TCommunicationFrameworkServer(PhysicsEngine).StopService;
        end
      else if PhysicsEngine is TCommunicationFrameworkClient then
        begin
          TCommunicationFrameworkClient(PhysicsEngine).Disconnect;
        end;
      DisposeObject(PhysicsEngine);
    end;

  inherited Destroy;
end;

procedure TXNATService.AddMapping(const ListenAddr, ListenPort, Mapping: TPascalString; TimeOut: TTimeTick);
var
  shLt: TXServiceListen;
begin
  shLt := TXServiceListen.Create;
  shLt.ListenAddr := ListenAddr;
  shLt.ListenPort := ListenPort;
  shLt.Mapping := Mapping;
  shLt.DistributedWorkload := True;
  shLt.XServerTunnel := Self;
  shLt.TimeOut := TimeOut;
  ShareListenList.Add(shLt);
end;

procedure TXNATService.AddNoDistributedMapping(const ListenAddr, ListenPort, Mapping: TPascalString; TimeOut: TTimeTick);
var
  shLt: TXServiceListen;
begin
  shLt := TXServiceListen.Create;
  shLt.ListenAddr := ListenAddr;
  shLt.ListenPort := ListenPort;
  shLt.Mapping := Mapping;
  shLt.DistributedWorkload := False;
  shLt.XServerTunnel := Self;
  shLt.TimeOut := TimeOut;
  ShareListenList.Add(shLt);
end;

procedure TXNATService.OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  Activted := True;

  { init tunnel engine }
  if PhysicsEngine = nil then
    begin
      if MODEL = TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_SERVICE then
          PhysicsEngine := TXPhysicsServer.Create
      else
          PhysicsEngine := TXPhysicsClient.Create;
    end;

  PhysicsEngine.UserSpecialClass := TPhysicsEngine_Special;
  PhysicsEngine.IOInterface := Self;
  PhysicsEngine.VMInterface := Self;

  { Security protocol }
  PhysicsEngine.SwitchMaxPerformance;

  { regsiter protocol }
  if not PhysicsEngine.ExistsRegistedCmd(C_IPV6Listen) then
      PhysicsEngine.RegisterStream(C_IPV6Listen).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}IPV6Listen;

  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
      { service }
      if TCommunicationFrameworkServer(PhysicsEngine).StartService(Host, umlStrToInt(Port)) then
          DoStatus('Tunnel Open %s:%s successed', [TranslateBindAddr(Host), Port.Text])
      else
          DoStatus('error: Tunnel is Closed for %s:%s', [TranslateBindAddr(Host), Port.Text]);

      { open share listen }
      for i := 0 to ShareListenList.Count - 1 do
        begin
          shLt := ShareListenList[i] as TXServiceListen;
          shLt.Open;
        end;
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
      { reverse connection }
      if not TCommunicationFrameworkClient(PhysicsEngine).Connected then
        begin
          WaitAsyncConnecting := True;
          WaitAsyncConnecting_BeginTime := GetTimeTick;
          TCommunicationFrameworkClient(PhysicsEngine).AsyncConnectM(Host, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}PhysicsConnect_Result_BuildP2PToken);
        end;
    end;
end;

procedure TXNATService.OpenTunnel;
begin
  OpenTunnel(TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_SERVICE);
end;

procedure TXNATService.Progress;
var
  i: Integer;
  shLt: TXServiceListen;
begin
  if (PhysicsEngine <> nil) then
    begin
      if (PhysicsEngine is TCommunicationFrameworkClient) then
        begin
          if WaitAsyncConnecting and (GetTimeTick - WaitAsyncConnecting_BeginTime > 15000) then
              WaitAsyncConnecting := False;

          if Activted and (not TCommunicationFrameworkClient(PhysicsEngine).Connected) then
            begin
              if not WaitAsyncConnecting then
                begin
                  TCoreClassThread.Sleep(100);
                  OpenTunnel(TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_CLIENT);
                end;
            end;
        end;

      PhysicsEngine.Progress;
    end;

  for i := ShareListenList.Count - 1 downto 0 do
    begin
      shLt := ShareListenList[i] as TXServiceListen;
      if (shLt.RecvTunnel <> nil) and (shLt.SendTunnel <> nil) then
        begin
          if (shLt.RecvTunnel.Count = 0) and (shLt.SendTunnel.Count = 0) and (shLt.Activted) then
              shLt.Activted := False;

          shLt.RecvTunnel.Progress;
          shLt.SendTunnel.Progress;
          shLt.Protocol.Progress;
        end;
    end;
end;

end.
