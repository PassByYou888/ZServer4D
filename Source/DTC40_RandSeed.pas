{ ****************************************************************************** }
{ * cloud 4.0 global network random Seed                                       * }
{ ****************************************************************************** }
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
unit DTC40_RandSeed;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib, ListEngine,
  Geometry2DUnit, DataFrameEngine, ZJson, zExpression,
  NotifyObjectBase, CoreCipher, MemoryStream64,
  ZDB2_Core, GHashList,
  CommunicationFramework, PhysicsIO, CommunicationFrameworkDoubleTunnelIO_NoAuth, DTC40;

type
  TDTC40_RandSeed_Client = class;
  TBigSeedPool = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TUInt32HashPointerList>;

  TDTC40_RandSeed_Service = class(TDTC40_Base_NoAuth_Service)
  protected
  private
    procedure cmd_MakeSeed(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_RemoveSeed(sender: TPeerIO; InData: TDFE);
  public
    BigSeedPool: TBigSeedPool;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    function GetSeedGroup(Name_: U_String): TUInt32HashPointerList;
  end;

  TON_MakeSeedC = procedure(sender: TDTC40_RandSeed_Client; Seed_: UInt32);
  TON_MakeSeedM = procedure(sender: TDTC40_RandSeed_Client; Seed_: UInt32) of object;
{$IFDEF FPC}
  TON_MakeSeedP = procedure(sender: TDTC40_RandSeed_Client; Seed_: UInt32) is nested;
{$ELSE FPC}
  TON_MakeSeedP = reference to procedure(sender: TDTC40_RandSeed_Client; Seed_: UInt32);
{$ENDIF FPC}

  TON_MakeSeed = class(TOnResultBridge)
  public
    Client: TDTC40_RandSeed_Client;
    OnResultC: TON_MakeSeedC;
    OnResultM: TON_MakeSeedM;
    OnResultP: TON_MakeSeedP;
    constructor Create; override;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TDTC40_RandSeed_Client = class(TDTC40_Base_NoAuth_Client)
  public
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;

    procedure MakeSeed_C(Group_: U_String; Min_, Max_: UInt32; OnResult: TON_MakeSeedC);
    procedure MakeSeed_M(Group_: U_String; Min_, Max_: UInt32; OnResult: TON_MakeSeedM);
    procedure MakeSeed_P(Group_: U_String; Min_, Max_: UInt32; OnResult: TON_MakeSeedP);
    procedure RemoveSeed(Group_: U_String; Seed_: UInt32);
  end;

implementation


procedure TDTC40_RandSeed_Service.cmd_MakeSeed(sender: TPeerIO; InData, OutData: TDFE);
var
  Group_: TUInt32HashPointerList;
  Min_: UInt32;
  Max_: UInt32;
  tmp: UInt32;
begin
  Group_ := GetSeedGroup(InData.R.ReadString);
  Min_ := InData.R.ReadCardinal;
  Max_ := InData.R.ReadCardinal;

  repeat
      tmp := umlRandomRange(Min_, Max_);
  until not Group_.Exists(tmp);
  Group_.Add(tmp, nil, False);

  OutData.WriteCardinal(tmp);
end;

procedure TDTC40_RandSeed_Service.cmd_RemoveSeed(sender: TPeerIO; InData: TDFE);
var
  Group_: TUInt32HashPointerList;
begin
  Group_ := GetSeedGroup(InData.R.ReadString);
  Group_.Delete(InData.R.ReadCardinal);
end;

constructor TDTC40_RandSeed_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  DTNoAuthService.RecvTunnel.RegisterStream('MakeSeed').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_MakeSeed;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveSeed').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveSeed;
  // is only instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;

  BigSeedPool := TBigSeedPool.Create(True, 1024 * 1024, nil);
end;

destructor TDTC40_RandSeed_Service.Destroy;
begin
  DisposeObjectAndNil(BigSeedPool);
  inherited Destroy;
end;

procedure TDTC40_RandSeed_Service.SafeCheck;
begin
  inherited SafeCheck;
end;

function TDTC40_RandSeed_Service.GetSeedGroup(Name_: U_String): TUInt32HashPointerList;
begin
  Result := BigSeedPool[Name_];
  if Result = nil then
    begin
      Result := TUInt32HashPointerList.CustomCreate(1024 * 1024);
      Result.AutoFreeData := False;
      Result.AccessOptimization := True;
      BigSeedPool.FastAdd(Name_, Result);
    end;
end;

constructor TON_MakeSeed.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_MakeSeed.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Seed_: UInt32;
begin
  Seed_ := Result_.R.ReadCardinal;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Seed_);
    if Assigned(OnResultM) then
        OnResultM(Client, Seed_);
    if Assigned(OnResultP) then
        OnResultP(Client, Seed_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_RandSeed_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
end;

destructor TDTC40_RandSeed_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TDTC40_RandSeed_Client.MakeSeed_C(Group_: U_String; Min_, Max_: UInt32; OnResult: TON_MakeSeedC);
var
  tmp: TON_MakeSeed;
  D: TDFE;
begin
  tmp := TON_MakeSeed.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(Group_);
  D.WriteCardinal(Min_);
  D.WriteCardinal(Max_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('MakeSeed', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TDTC40_RandSeed_Client.MakeSeed_M(Group_: U_String; Min_, Max_: UInt32; OnResult: TON_MakeSeedM);
var
  tmp: TON_MakeSeed;
  D: TDFE;
begin
  tmp := TON_MakeSeed.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(Group_);
  D.WriteCardinal(Min_);
  D.WriteCardinal(Max_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('MakeSeed', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TDTC40_RandSeed_Client.MakeSeed_P(Group_: U_String; Min_, Max_: UInt32; OnResult: TON_MakeSeedP);
var
  tmp: TON_MakeSeed;
  D: TDFE;
begin
  tmp := TON_MakeSeed.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(Group_);
  D.WriteCardinal(Min_);
  D.WriteCardinal(Max_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('MakeSeed', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TDTC40_RandSeed_Client.RemoveSeed(Group_: U_String; Seed_: UInt32);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(Group_);
  D.WriteCardinal(Seed_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveSeed', D);
  DisposeObject(D);
end;

initialization

RegisterC40('RandSeed', TDTC40_RandSeed_Service, TDTC40_RandSeed_Client);

end.
