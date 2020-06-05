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
unit XNATClient;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib, ListEngine, TextDataEngine,
  CoreCipher, DataFrameEngine, MemoryStream64, CommunicationFramework, NotifyObjectBase,
  XNATPhysics;

type
  TXNATClient = class;

  TXClientMapping = class(TCoreClassObject)
  private
    Addr: TPascalString;
    Port: TPascalString;
    Mapping: TPascalString;

    ProtocolPool: TCoreClassListForObj;
    LastProtocolID: Cardinal;
    ProtocolHash: TUInt32HashObjectList;

    RecvTunnel: TCommunicationFrameworkWithP2PVM_Client;
    RecvTunnel_IPV6: TPascalString;
    RecvTunnel_Port: Word;

    SendTunnel: TCommunicationFrameworkWithP2PVM_Client;
    SendTunnel_IPV6: TPascalString;
    SendTunnel_Port: Word;

    MaxWorkload: Cardinal;
    LastUpdateWorkload: Cardinal;
    LastUpdateTime: TTimeTick;

    Remote_ListenAddr, Remote_ListenPort: TPascalString;

    XClientTunnel: TXNATClient;

    procedure Init;
    procedure SendTunnel_ConnectResult(const cState: Boolean);
    procedure RecvTunnel_ConnectResult(const cState: Boolean);

    procedure RequestListen_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
    procedure delay_RequestListen(Sender: TNPostExecute);

    procedure Open;

    procedure cmd_connect_request(Sender: TPeerIO; InData: TDataFrameEngine);
    procedure cmd_disconnect_request(Sender: TPeerIO; InData: TDataFrameEngine);
    procedure cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateWorkload(force: Boolean);
  end;

  TXClientCustomProtocol = class(TXPhysicsClient)
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    LocalProtocol_ID, RemoteProtocol_ID: Cardinal;
    Remote_IP: SystemString;
    Mapping: TXClientMapping;
    Activted: Boolean;
    RequestBuffer: TMemoryStream64;

    procedure OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
    procedure OnConnect_Result(const cState: Boolean);
    constructor Create; override;
    destructor Destroy; override;
  end;

  TPhysicsEngine_Special = class(TPeerIOUserSpecial)
  protected
    XNAT: TXNATClient;
    procedure PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
    procedure PhysicsVMBuildAuthToken_Result;
    procedure PhysicsOpenVM_Result(const cState: Boolean);
    procedure IPV6Listen_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXNATClient = class(TCoreClassInterfacedObject, IIOInterface, ICommunicationFrameworkVMInterface)
  private
    MappingList: TCoreClassListForObj;
    HashMapping: THashObjectList;
    Activted: Boolean;
    WaitAsyncConnecting: Boolean;
    WaitAsyncConnecting_BeginTime: TTimeTick;
    PhysicsEngine: TCommunicationFramework;

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
    procedure AddMapping(const MAddr, MPort, Mapping: TPascalString; MaxWorkload: Cardinal);
    procedure OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL); overload;
    procedure OpenTunnel; overload;
    procedure Progress;
  end;

implementation

procedure TXClientMapping.Init;
begin
  Addr := '';
  Port := '';
  Mapping := '';
  ProtocolPool := nil;
  LastProtocolID := 1;
  ProtocolHash := nil;
  RecvTunnel := nil;
  RecvTunnel_IPV6 := '';
  RecvTunnel_Port := 0;
  SendTunnel := nil;
  SendTunnel_IPV6 := '';
  SendTunnel_Port := 0;
  MaxWorkload := 100;
  LastUpdateWorkload := 0;
  LastUpdateTime := GetTimeTick();
  Remote_ListenAddr := '';
  Remote_ListenPort := '0';
  XClientTunnel := nil;
end;

procedure TXClientMapping.SendTunnel_ConnectResult(const cState: Boolean);
begin
  if cState then
    begin
      DoStatus('[%s] Send Tunnel connect success.', [Mapping.Text]);
      if not RecvTunnel.Connected then
          RecvTunnel.AsyncConnectM(RecvTunnel_IPV6, RecvTunnel_Port, {$IFDEF FPC}@{$ENDIF FPC}RecvTunnel_ConnectResult)
      else
          RecvTunnel_ConnectResult(True);
    end
  else
      DoStatus('error: [%s] Send Tunnel connect failed!', [Mapping.Text]);
end;

procedure TXClientMapping.RecvTunnel_ConnectResult(const cState: Boolean);
begin
  if cState then
    begin
      DoStatus('[%s] Receive Tunnel connect success.', [Mapping.Text]);
      SendTunnel.ProgressPost.PostExecuteM(0, {$IFDEF FPC}@{$ENDIF FPC}delay_RequestListen);
    end
  else
      DoStatus('error: [%s] Receive Tunnel connect failed!', [Mapping.Text]);
end;

procedure TXClientMapping.RequestListen_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
begin
  if ResultData.Reader.ReadBool then
    begin
      DoStatus('success: remote host:%s port:%s mapping to host:%s port:%s', [XClientTunnel.Host.Text, Remote_ListenPort.Text, Addr.Text, Port.Text]);
      UpdateWorkload(True);
    end
  else
      DoStatus('failed: remote host:%s port:%s listen error!', [XClientTunnel.Host.Text, Remote_ListenPort.Text]);
end;

procedure TXClientMapping.delay_RequestListen(Sender: TNPostExecute);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteCardinal(SendTunnel.RemoteID);
  de.WriteCardinal(RecvTunnel.RemoteID);
  SendTunnel.SendStreamCmdM(C_RequestListen, de, {$IFDEF FPC}@{$ENDIF FPC}RequestListen_Result);
  disposeObject(de);
end;

procedure TXClientMapping.Open;
var
  io_array: TIO_Array;
  p_id: Cardinal;
  p_io: TPeerIO;
begin
  if ProtocolPool = nil then
      ProtocolPool := TCoreClassListForObj.Create;

  if ProtocolHash = nil then
      ProtocolHash := TUInt32HashObjectList.CustomCreate(8192);

  if RecvTunnel = nil then
      RecvTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  if SendTunnel = nil then
      SendTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;

  XClientTunnel.PhysicsEngine.GetIO_Array(io_array);
  for p_id in io_array do
    begin
      p_io := TPeerIO(XClientTunnel.PhysicsEngine.PeerIO_HashPool[p_id]);
      if p_io <> nil then
        begin
          { uninstall p2pVM }
          p_io.p2pVMTunnel.UninstallLogicFramework(SendTunnel);
          p_io.p2pVMTunnel.UninstallLogicFramework(RecvTunnel);

          { install p2pVM }
          p_io.p2pVMTunnel.InstallLogicFramework(SendTunnel);
          p_io.p2pVMTunnel.InstallLogicFramework(RecvTunnel);
        end;
    end;
  SetLength(io_array, 0);

  { sequence sync }
  RecvTunnel.SyncOnCompleteBuffer := True;
  RecvTunnel.SyncOnResult := True;
  RecvTunnel.SwitchMaxPerformance;

  SendTunnel.SyncOnCompleteBuffer := True;
  SendTunnel.SyncOnResult := True;
  SendTunnel.SwitchMaxPerformance;

  { compressed complete buffer }
  SendTunnel.CompleteBufferCompressed := XClientTunnel.ProtocolCompressed;
  RecvTunnel.CompleteBufferCompressed := XClientTunnel.ProtocolCompressed;

  if not RecvTunnel.ExistsRegistedCmd(C_Connect_request) then
      RecvTunnel.RegisterDirectStream(C_Connect_request).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_connect_request;

  if not RecvTunnel.ExistsRegistedCmd(C_Disconnect_request) then
      RecvTunnel.RegisterDirectStream(C_Disconnect_request).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_disconnect_request;

  if not RecvTunnel.ExistsRegistedCmd(C_Data) then
      RecvTunnel.RegisterCompleteBuffer(C_Data).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_data;

  if not SendTunnel.Connected then
      SendTunnel.AsyncConnectM(SendTunnel_IPV6, SendTunnel_Port, {$IFDEF FPC}@{$ENDIF FPC}SendTunnel_ConnectResult)
  else
      SendTunnel_ConnectResult(True);

  SendTunnel.PrintParams[C_Connect_reponse] := False;
  SendTunnel.PrintParams[C_Disconnect_reponse] := False;
  SendTunnel.PrintParams[C_Data] := False;
  SendTunnel.PrintParams[C_Workload] := False;

  RecvTunnel.PrintParams[C_Connect_request] := False;
  RecvTunnel.PrintParams[C_Disconnect_request] := False;
  RecvTunnel.PrintParams[C_Data] := False;
end;

procedure TXClientMapping.cmd_connect_request(Sender: TPeerIO; InData: TDataFrameEngine);
var
  Remote_id: Cardinal;
  Remote_IP: SystemString;
  xCli: TXClientCustomProtocol;
begin
  Remote_id := InData.Reader.ReadCardinal;
  Remote_IP := InData.Reader.ReadString;

  xCli := TXClientCustomProtocol.Create;
  xCli.Protocol := cpCustom;
  while ProtocolHash.Exists(LastProtocolID) do
      inc(LastProtocolID);
  xCli.LocalProtocol_ID := LastProtocolID;
  xCli.Remote_IP := Remote_IP;
  inc(LastProtocolID);
  ProtocolPool.Add(xCli);
  ProtocolHash.Add(xCli.LocalProtocol_ID, xCli, False);

  xCli.RemoteProtocol_ID := Remote_id;
  xCli.Mapping := Self;
  xCli.Activted := False;

  { async connection }
  xCli.AsyncConnectM(Addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}xCli.OnConnect_Result);
end;

procedure TXClientMapping.cmd_disconnect_request(Sender: TPeerIO; InData: TDataFrameEngine);
var
  local_id, Remote_id: Cardinal;
  phy_io: TXClientCustomProtocol;
begin
  Remote_id := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;

  phy_io := TXClientCustomProtocol(ProtocolHash[local_id]);
  if phy_io <> nil then
      phy_io.Disconnect;
end;

procedure TXClientMapping.cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  local_id, Remote_id: Cardinal;
  destSiz: NativeInt;
  destBuff: PByte;
  phy_io: TXClientCustomProtocol;
begin
  FillBuff(InData, DataSize, Remote_id, local_id, destSiz, destBuff);

  phy_io := TXClientCustomProtocol(ProtocolHash[local_id]);
  if phy_io <> nil then
      phy_io.WriteBuffer(destBuff, destSiz);
end;

constructor TXClientMapping.Create;
begin
  inherited Create;
  Init;
end;

destructor TXClientMapping.Destroy;
var
  j: integer;
begin
  if ProtocolPool <> nil then
    begin
      for j := 0 to ProtocolPool.Count - 1 do
        begin
          TXClientCustomProtocol(ProtocolPool[j]).Disconnect;
          disposeObject(ProtocolPool[j]);
        end;
      disposeObject(ProtocolPool);
    end;

  if ProtocolHash <> nil then
      disposeObject(ProtocolHash);

  if SendTunnel <> nil then
    begin
      SendTunnel.Disconnect;
      disposeObject(SendTunnel);
    end;

  if RecvTunnel <> nil then
    begin
      RecvTunnel.Disconnect;
      disposeObject(RecvTunnel);
    end;

  inherited Destroy;
end;

procedure TXClientMapping.UpdateWorkload(force: Boolean);
var
  de: TDataFrameEngine;
begin
  if SendTunnel = nil then
      exit;
  if RecvTunnel = nil then
      exit;
  if not SendTunnel.Connected then
      exit;
  if (not force) then
    if (ProtocolHash.Count = LastUpdateWorkload) or (GetTimeTick() - LastUpdateTime < 1000) then
        exit;

  de := TDataFrameEngine.Create;
  de.WriteCardinal(MaxWorkload);
  de.WriteCardinal(ProtocolHash.Count);
  SendTunnel.SendDirectStreamCmd(C_Workload, de);
  disposeObject(de);

  LastUpdateWorkload := ProtocolHash.Count;
  LastUpdateTime := GetTimeTick();
end;

procedure TXClientCustomProtocol.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TXClientCustomProtocol.DoDisconnect(Sender: TPeerIO);
var
  de: TDataFrameEngine;
begin
  if Activted then
    begin
      de := TDataFrameEngine.Create;
      de.WriteCardinal(LocalProtocol_ID);
      de.WriteCardinal(RemoteProtocol_ID);
      Mapping.SendTunnel.SendDirectStreamCmd(C_Disconnect_reponse, de);
      disposeObject(de);
    end;
  inherited DoDisconnect(Sender);
end;

procedure TXClientCustomProtocol.OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
var
  nSiz: NativeInt;
  nBuff: PByte;
begin
  if Activted then
    begin
      BuildBuff(buffer, Size, LocalProtocol_ID, RemoteProtocol_ID, nSiz, nBuff);
      Mapping.SendTunnel.SendCompleteBuffer(C_Data, nBuff, nSiz, True);
    end
  else
    begin
      RequestBuffer.WritePtr(buffer, Size);
    end;
end;

procedure TXClientCustomProtocol.OnConnect_Result(const cState: Boolean);
var
  de: TDataFrameEngine;
  nSiz: NativeInt;
  nBuff: PByte;
begin
  de := TDataFrameEngine.Create;
  de.WriteBool(cState);
  de.WriteCardinal(LocalProtocol_ID);
  de.WriteCardinal(RemoteProtocol_ID);
  Mapping.SendTunnel.SendDirectStreamCmd(C_Connect_reponse, de);
  disposeObject(de);

  if cState then
    begin
      Activted := True;
      if RequestBuffer.Size > 0 then
        begin
          BuildBuff(RequestBuffer.Memory, RequestBuffer.Size, LocalProtocol_ID, RemoteProtocol_ID, nSiz, nBuff);
          Mapping.SendTunnel.SendCompleteBuffer(C_Data, nBuff, nSiz, True);
          RequestBuffer.Clear;
        end;
    end
  else
    begin
      de := TDataFrameEngine.Create;
      de.WriteCardinal(LocalProtocol_ID);
      de.WriteCardinal(RemoteProtocol_ID);
      Mapping.SendTunnel.SendDirectStreamCmd(C_Disconnect_reponse, de);
      disposeObject(de);

      disposeObject(Self);
    end;
end;

constructor TXClientCustomProtocol.Create;
begin
  inherited Create;
  LocalProtocol_ID := 0;
  RemoteProtocol_ID := 0;
  Remote_IP := '';
  Mapping := nil;
  Activted := False;
  RequestBuffer := TMemoryStream64.Create;
end;

destructor TXClientCustomProtocol.Destroy;
var
  i: integer;
begin
  Mapping.ProtocolHash.Delete(LocalProtocol_ID);
  i := 0;
  while i < Mapping.ProtocolPool.Count do
    begin
      if Mapping.ProtocolPool[i] = Self then
          Mapping.ProtocolPool.Delete(i)
      else
          inc(i);
    end;
  disposeObject(RequestBuffer);
  inherited Destroy;
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
begin
  if cState then
    begin
      Owner.p2pVMTunnel.MaxVMFragmentSize := umlStrToInt(XNAT.MaxVMFragment, Owner.p2pVMTunnel.MaxVMFragmentSize);
      Owner.p2pVMTunnel.MaxRealBuffer := umlStrToInt(XNAT.MaxRealBuffer, Owner.p2pVMTunnel.MaxRealBuffer);
      Owner.SendStreamCmdM(C_IPV6Listen, nil, {$IFDEF FPC}@{$ENDIF FPC}IPV6Listen_Result);
    end
  else
      XNAT.WaitAsyncConnecting := False;
end;

procedure TPhysicsEngine_Special.IPV6Listen_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
var
  Mapping: TPascalString;
  Remote_ListenAddr, Remote_ListenPort: TPascalString;
  RecvTunnel_IPV6: TPascalString;
  RecvTunnel_Port: Word;
  SendTunnel_IPV6: TPascalString;
  SendTunnel_Port: Word;
  tunMp: TXClientMapping;
begin
  while ResultData.Reader.NotEnd do
    begin
      Mapping := ResultData.Reader.ReadString;
      Remote_ListenAddr := ResultData.Reader.ReadString;
      Remote_ListenPort := ResultData.Reader.ReadString;
      SendTunnel_IPV6 := ResultData.Reader.ReadString;
      SendTunnel_Port := ResultData.Reader.ReadWord;
      RecvTunnel_IPV6 := ResultData.Reader.ReadString;
      RecvTunnel_Port := ResultData.Reader.ReadWord;
      tunMp := TXClientMapping(XNAT.HashMapping[Mapping]);
      if tunMp <> nil then
        begin
          tunMp.RecvTunnel_IPV6 := RecvTunnel_IPV6;
          tunMp.RecvTunnel_Port := RecvTunnel_Port;
          tunMp.SendTunnel_IPV6 := SendTunnel_IPV6;
          tunMp.SendTunnel_Port := SendTunnel_Port;
          tunMp.Remote_ListenAddr := Remote_ListenAddr;
          tunMp.Remote_ListenPort := Remote_ListenPort;
          tunMp.Open;
        end;
    end;
  XNAT.Activted := True;
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

procedure TXNATClient.PeerIO_Create(const Sender: TPeerIO);
begin
  TPhysicsEngine_Special(Sender.UserSpecial).XNAT := Self;
end;

procedure TXNATClient.PeerIO_Destroy(const Sender: TPeerIO);
begin
end;

procedure TXNATClient.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
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

procedure TXNATClient.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
    end;
  DoStatus('XTunnel Open Before on %s', [Sender.PeerIP]);
end;

procedure TXNATClient.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
    end;
  DoStatus('XTunnel Open on %s', [Sender.PeerIP]);
end;

procedure TXNATClient.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
      Sender.SendStreamCmdM(C_IPV6Listen, nil, {$IFDEF FPC}@{$ENDIF FPC}TPhysicsEngine_Special(Sender.UserSpecial).IPV6Listen_Result);
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
    end;
  DoStatus('XTunnel Open After on %s', [Sender.PeerIP]);
end;

procedure TXNATClient.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
    end;
  DoStatus('XTunnel Close on %s', [Sender.PeerIP]);
end;

procedure TXNATClient.PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
begin
  if cState then
      TPhysicsEngine_Special(TCommunicationFrameworkClient(PhysicsEngine).ClientIO.UserSpecial).PhysicsConnect_Result_BuildP2PToken(cState)
  else
    begin
      DoStatus('P2PVM not connection');
      WaitAsyncConnecting := False;
    end;
end;

constructor TXNATClient.Create;
begin
  inherited Create;
  Host := '';
  Port := '4921';
  AuthToken := '';
  MaxVMFragment := '8192';
  MaxRealBuffer := umlIntToStr(1024 * 1024 * 2);
  ProtocolCompressed := False;
  MappingList := TCoreClassListForObj.Create;
  HashMapping := THashObjectList.Create(False);
  Activted := False;
  WaitAsyncConnecting := False;
  PhysicsEngine := nil;
end;

destructor TXNATClient.Destroy;
var
  i: integer;
  tunMp: TXClientMapping;
begin
  for i := MappingList.Count - 1 downto 0 do
    begin
      tunMp := MappingList[i] as TXClientMapping;
      disposeObject(tunMp);
    end;
  disposeObject(MappingList);
  disposeObject(HashMapping);

  if PhysicsEngine <> nil then
    begin
      if PhysicsEngine is TCommunicationFrameworkClient then
        begin
          TCommunicationFrameworkClient(PhysicsEngine).Disconnect;
        end;
    end;

  disposeObject(PhysicsEngine);
  inherited Destroy;
end;

procedure TXNATClient.AddMapping(const MAddr, MPort, Mapping: TPascalString; MaxWorkload: Cardinal);
var
  tunMp: TXClientMapping;
begin
  tunMp := TXClientMapping.Create;
  tunMp.Addr := MAddr;
  tunMp.Port := MPort;
  tunMp.Mapping := Mapping;
  tunMp.MaxWorkload := MaxWorkload;
  tunMp.XClientTunnel := Self;
  MappingList.Add(tunMp);
  HashMapping.Add(tunMp.Mapping, tunMp);
end;

procedure TXNATClient.OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL);
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

  if PhysicsEngine is TCommunicationFrameworkServer then
    begin
      if TCommunicationFrameworkServer(PhysicsEngine).StartService(Host, umlStrToInt(Port)) then
          DoStatus('Tunnel Open %s:%s successed', [TranslateBindAddr(Host), Port.Text])
      else
          DoStatus('error: Tunnel is Closed for %s:%s', [TranslateBindAddr(Host), Port.Text]);
    end
  else if PhysicsEngine is TCommunicationFrameworkClient then
    begin
      if not TCommunicationFrameworkClient(PhysicsEngine).Connected then
        begin
          WaitAsyncConnecting := True;
          WaitAsyncConnecting_BeginTime := GetTimeTick;
          TCommunicationFrameworkClient(PhysicsEngine).AsyncConnectM(Host, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}PhysicsConnect_Result_BuildP2PToken);
        end;
    end;
end;

procedure TXNATClient.OpenTunnel;
begin
  OpenTunnel(TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_CLIENT);
end;

procedure TXNATClient.Progress;
var
  i, j: integer;
  tunMp: TXClientMapping;
  xCliProt: TXClientCustomProtocol;
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

  i := 0;
  while i < MappingList.Count do
    begin
      tunMp := MappingList[i] as TXClientMapping;

      tunMp.UpdateWorkload(False);

      if tunMp.RecvTunnel <> nil then
          tunMp.RecvTunnel.Progress;

      if tunMp.SendTunnel <> nil then
          tunMp.SendTunnel.Progress;

      if tunMp.ProtocolPool <> nil then
        begin
          j := 0;
          while j < tunMp.ProtocolPool.Count do
            begin
              xCliProt := TXClientCustomProtocol(tunMp.ProtocolPool[j]);
              try
                if xCliProt.Activted and (not xCliProt.Connected) then
                  begin
                    disposeObject(xCliProt);
                    break;
                  end
                else
                  begin
                    xCliProt.Progress;
                    inc(j);
                  end;
              except
                  break;
              end;
            end;
        end;
      inc(i);
    end;
end;

end.
