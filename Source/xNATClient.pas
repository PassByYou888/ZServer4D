{ ****************************************************************************** }
{ * x Nat tunnel access support, written by QQ 600585@qq.com                   * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit xNATClient;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib, ListEngine, TextDataEngine,
  CoreCipher, DataFrameEngine, MemoryStream64, CommunicationFramework, NotifyObjectBase,
  xNATPhysics;

type
  TXNATClient = class;
  TXClientCustomProtocol = class;

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
  end;

  TXClientCustomProtocol = class(TXPhysicsClient)
  protected
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    LocalProtocol_ID, RemoteProtocol_ID: Cardinal;
    Mapping: TXClientMapping;
    Activted: Boolean;
    RequestBuffer: TMemoryStream64;

    procedure OnReceiveBuffer(const buffer: PByte; const Size: NativeInt); override;
    procedure OnConnect_Result(const cState: Boolean);
    constructor Create; override;
    destructor Destroy; override;
  end;

  TXNATClient = class(TCoreClassObject)
  private
    MappingList: TCoreClassListForObj;
    HashMapping: THashObjectList;
    Activted: Boolean;
    WaitAsyncConnecting: Boolean;
    WaitAsyncConnecting_BeginTime: TTimeTick;
    PhysicsEngine: TXPhysicsClient;
  protected
    procedure PhysicsConnect_Result(const cState: Boolean);
    procedure PhysicsVMBuildAuthToken_Result;
    procedure PhysicsOpenVM_Result(const cState: Boolean);
    procedure IPV6Listen_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
  public
    // tunnel parameter
    RemoteTunnelAddr: TPascalString;
    RemoteTunnelPort: TPascalString;
    AuthToken: TPascalString;
    MaxVMFragment, MaxRealBuffer: TPascalString;

    constructor Create;
    destructor Destroy; override;
    procedure AddMapping(const Addr, Port, Mapping: TPascalString);
    procedure OpenTunnel;
    procedure Progress;
  end;

implementation

uses xNATService;

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
      DoStatus('success: remote listen %s -> mapping local %s', [Remote_ListenPort.Text, Port.Text])
  else
      DoStatus('failed: remote listen %s alread used', [Remote_ListenPort.Text]);
end;

procedure TXClientMapping.delay_RequestListen(Sender: TNPostExecute);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteCardinal(SendTunnel.RemoteID);
  de.WriteCardinal(RecvTunnel.RemoteID);
  SendTunnel.SendStreamCmdM('RequestListen', de, {$IFDEF FPC}@{$ENDIF FPC}RequestListen_Result);
  disposeObject(de);
end;

procedure TXClientMapping.Open;
begin
  if ProtocolPool = nil then
      ProtocolPool := TCoreClassListForObj.Create;

  if ProtocolHash = nil then
      ProtocolHash := TUInt32HashObjectList.Create(8192);

  if RecvTunnel = nil then
      RecvTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  if SendTunnel = nil then
      SendTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;

  XClientTunnel.PhysicsEngine.ClientIO.p2pVMTunnel.UninstallLogicFramework(SendTunnel);
  XClientTunnel.PhysicsEngine.ClientIO.p2pVMTunnel.UninstallLogicFramework(RecvTunnel);

  XClientTunnel.PhysicsEngine.ClientIO.p2pVMTunnel.InstallLogicFramework(SendTunnel);
  XClientTunnel.PhysicsEngine.ClientIO.p2pVMTunnel.InstallLogicFramework(RecvTunnel);

  if not RecvTunnel.ExistsRegistedCmd('connect_request') then
      RecvTunnel.RegisterDirectStream('connect_request').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_connect_request;

  if not RecvTunnel.ExistsRegistedCmd('disconnect_request') then
      RecvTunnel.RegisterDirectStream('disconnect_request').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_disconnect_request;

  if not RecvTunnel.ExistsRegistedCmd('data') then
      RecvTunnel.RegisterCompleteBuffer('data').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_data;

  if not SendTunnel.Connected then
      SendTunnel.AsyncConnectM(SendTunnel_IPV6, SendTunnel_Port, {$IFDEF FPC}@{$ENDIF FPC}SendTunnel_ConnectResult)
  else
      SendTunnel_ConnectResult(True);

  SendTunnel.PrintParams['connect_reponse'] := False;
  SendTunnel.PrintParams['disconnect_reponse'] := False;
  SendTunnel.PrintParams['data'] := False;

  RecvTunnel.PrintParams['connect_request'] := False;
  RecvTunnel.PrintParams['disconnect_request'] := False;
  RecvTunnel.PrintParams['data'] := False;
end;

procedure TXClientMapping.cmd_connect_request(Sender: TPeerIO; InData: TDataFrameEngine);
var
  remote_id: Cardinal;
  xCli: TXClientCustomProtocol;
begin
  remote_id := InData.Reader.ReadCardinal;

  xCli := TXClientCustomProtocol.Create;
  xCli.Protocol := cpCustom;
  while ProtocolHash.Exists(LastProtocolID) do
      inc(LastProtocolID);
  xCli.LocalProtocol_ID := LastProtocolID;
  inc(LastProtocolID);
  ProtocolPool.Add(xCli);
  ProtocolHash.Add(xCli.LocalProtocol_ID, xCli, False);

  xCli.RemoteProtocol_ID := remote_id;
  xCli.Mapping := Self;
  xCli.Activted := False;
  xCli.AsyncConnectM(Addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}xCli.OnConnect_Result);
end;

procedure TXClientMapping.cmd_disconnect_request(Sender: TPeerIO; InData: TDataFrameEngine);
var
  local_id, remote_id: Cardinal;
  phy_io: TXClientCustomProtocol;
begin
  remote_id := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;

  phy_io := TXClientCustomProtocol(ProtocolHash[local_id]);
  if phy_io <> nil then
    begin
      try
          phy_io.Disconnect;
      except
      end;
      disposeObject(phy_io);
    end;
end;

procedure TXClientMapping.cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  local_id, remote_id: Cardinal;
  destSiz: NativeInt;
  destBuff: PByte;
  phy_io: TXClientCustomProtocol;
begin
  FillBuff(InData, DataSize, remote_id, local_id, destSiz, destBuff);

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
  j: Integer;
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

procedure TXClientCustomProtocol.DoDisconnect(Sender: TPeerIO);
var
  de: TDataFrameEngine;
begin
  if Activted then
    begin
      de := TDataFrameEngine.Create;
      de.WriteCardinal(LocalProtocol_ID);
      de.WriteCardinal(RemoteProtocol_ID);
      Mapping.SendTunnel.SendDirectStreamCmd('disconnect_reponse', de);
      disposeObject(de);
    end;
  inherited DoDisconnect(Sender);
end;

procedure TXClientCustomProtocol.OnReceiveBuffer(const buffer: PByte; const Size: NativeInt);
var
  nSiz: NativeInt;
  nBuff: PByte;
begin
  if Activted then
    begin
      BuildBuff(buffer, Size, LocalProtocol_ID, RemoteProtocol_ID, nSiz, nBuff);
      Mapping.SendTunnel.SendCompleteBuffer('data', nBuff, nSiz, True);
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
  Mapping.SendTunnel.SendDirectStreamCmd('connect_reponse', de);
  disposeObject(de);

  if cState then
    begin
      Activted := True;
      if RequestBuffer.Size > 0 then
        begin
          BuildBuff(RequestBuffer.Memory, RequestBuffer.Size, LocalProtocol_ID, RemoteProtocol_ID, nSiz, nBuff);
          Mapping.SendTunnel.SendCompleteBuffer('data', nBuff, nSiz, True);
          RequestBuffer.Clear;
        end;
    end
  else
    begin
      de := TDataFrameEngine.Create;
      de.WriteCardinal(LocalProtocol_ID);
      de.WriteCardinal(RemoteProtocol_ID);
      Mapping.SendTunnel.SendDirectStreamCmd('disconnect_reponse', de);
      disposeObject(de);

      disposeObject(Self);
    end;
end;

constructor TXClientCustomProtocol.Create;
begin
  inherited Create;
  LocalProtocol_ID := 0;
  RemoteProtocol_ID := 0;
  Mapping := nil;
  Activted := False;
  RequestBuffer := TMemoryStream64.Create;
end;

destructor TXClientCustomProtocol.Destroy;
var
  i: Integer;
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

procedure TXNATClient.PhysicsConnect_Result(const cState: Boolean);
begin
  if cState then
      PhysicsEngine.ClientIO.BuildP2PAuthTokenM({$IFDEF FPC}@{$ENDIF FPC}PhysicsVMBuildAuthToken_Result)
  else
      WaitAsyncConnecting := False;
end;

procedure TXNATClient.PhysicsVMBuildAuthToken_Result;
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
  PhysicsEngine.ClientIO.OpenP2pVMTunnelM(True, GenerateQuantumCryptographyPassword(AuthToken), {$IFDEF FPC}@{$ENDIF FPC}PhysicsOpenVM_Result)
end;

procedure TXNATClient.PhysicsOpenVM_Result(const cState: Boolean);
begin
  if cState then
    begin
      PhysicsEngine.ClientIO.p2pVMTunnel.MaxVMFragmentSize := umlStrToInt(MaxVMFragment, PhysicsEngine.ClientIO.p2pVMTunnel.MaxVMFragmentSize);
      PhysicsEngine.ClientIO.p2pVMTunnel.MaxRealBuffer := umlStrToInt(MaxRealBuffer, PhysicsEngine.ClientIO.p2pVMTunnel.MaxRealBuffer);
      PhysicsEngine.SendStreamCmdM('IPV6Listen', nil, {$IFDEF FPC}@{$ENDIF FPC}IPV6Listen_Result);
    end
  else
      WaitAsyncConnecting := False;
end;

procedure TXNATClient.IPV6Listen_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
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
      tunMp := TXClientMapping(HashMapping[Mapping]);
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
  Activted := True;
  WaitAsyncConnecting := False;
end;

constructor TXNATClient.Create;
begin
  inherited Create;
  RemoteTunnelAddr := '';
  RemoteTunnelPort := '4921';
  AuthToken := '';
  MaxVMFragment := '1024';
  MaxRealBuffer := umlIntToStr(1024 * 1024 * 2);
  MappingList := TCoreClassListForObj.Create;
  HashMapping := THashObjectList.Create(False);
  Activted := False;
  WaitAsyncConnecting := False;
  PhysicsEngine := nil;
end;

destructor TXNATClient.Destroy;
var
  i: Integer;
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
      PhysicsEngine.Disconnect;
  disposeObject(PhysicsEngine);
  inherited Destroy;
end;

procedure TXNATClient.AddMapping(const Addr, Port, Mapping: TPascalString);
var
  tunMp: TXClientMapping;
begin
  tunMp := TXClientMapping.Create;
  tunMp.Addr := Addr;
  tunMp.Port := Port;
  tunMp.Mapping := Mapping;
  tunMp.XClientTunnel := Self;
  MappingList.Add(tunMp);
  HashMapping.Add(tunMp.Mapping, tunMp);
end;

procedure TXNATClient.OpenTunnel;
begin
  Activted := True;

  if PhysicsEngine = nil then
    begin
      PhysicsEngine := TXPhysicsClient.Create;
      PhysicsEngine.SwitchMaxSafe;
    end;

  if not PhysicsEngine.Connected then
    begin
      WaitAsyncConnecting := True;
      WaitAsyncConnecting_BeginTime := GetTimeTick;
      PhysicsEngine.AsyncConnectM(RemoteTunnelAddr, umlStrToInt(RemoteTunnelPort), {$IFDEF FPC}@{$ENDIF FPC}PhysicsConnect_Result);
    end;
end;

procedure TXNATClient.Progress;
var
  i, j: Integer;
  tunMp: TXClientMapping;
  xCliProt: TXClientCustomProtocol;
begin
  if PhysicsEngine <> nil then
    begin
      if WaitAsyncConnecting and (GetTimeTick - WaitAsyncConnecting_BeginTime > 15000) then
          WaitAsyncConnecting := False;

      if Activted and (not PhysicsEngine.Connected) then
        begin
          if not WaitAsyncConnecting then
            begin
              TCoreClassThread.Sleep(500);
              OpenTunnel;
            end;
        end;

      PhysicsEngine.Progress;
    end;

  i := 0;
  while i < MappingList.Count do
    begin
      tunMp := MappingList[i] as TXClientMapping;

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
