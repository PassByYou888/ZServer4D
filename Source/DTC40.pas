{ ****************************************************************************** }
{ * cloud 4.0 framework                                                        * }
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
unit DTC40;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib, ListEngine,
  Geometry2DUnit, DataFrameEngine, ZJson,
  NotifyObjectBase, CoreCipher, MemoryStream64,
  zExpression, OpCode,
  CommunicationFramework, PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO,
  CommunicationFrameworkDataStoreService,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth,
  CommunicationFrameworkDataStoreService_VirtualAuth,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CommunicationFrameworkDataStoreService_NoAuth;

type
  TDTC40_PhysicsService = class;
  TDTC40_PhysicsServicePool = class;
  TDTC40_PhysicsTunnel = class;
  TDTC40_PhysicsTunnelPool = class;
  TDTC40_Info = class;
  TDTC40_InfoList = class;
  TDTC40_Custom_Service = class;
  TDTC40_Custom_ServicePool = class;
  TDTC40_Custom_Client = class;
  TDTC40_Custom_ClientPool = class;
  TDTC40_Dispatch_Service = class;
  TDTC40_Dispatch_Client = class;
  TDTC40_Base_NoAuth_Service = class;
  TDTC40_Base_NoAuth_Client = class;
  TDTC40_Base_DataStoreNoAuth_Service = class;
  TDTC40_Base_DataStoreNoAuth_Client = class;
  TDTC40_Base_VirtualAuth_Service = class;
  TDTC40_Base_VirtualAuth_Client = class;
  TDTC40_Base_DataStoreVirtualAuth_Service = class;
  TDTC40_Base_DataStoreVirtualAuth_Client = class;

{$REGION 'PhysicsService'}
  TDTC40_DependNetworkString = U_StringArray;

  TDTC40_DependNetworkInfo = record
    Typ: U_String;
    Param: U_String;
  end;

  TDTC40_DependNetworkInfoArray = array of TDTC40_DependNetworkInfo;

  IDTC40_PhysicsService_Event = interface
    procedure DTC40_PhysicsService_Build_Network(Sender: TDTC40_PhysicsService; Custom_Service_: TDTC40_Custom_Service);
    procedure DTC40_PhysicsService_Start(Sender: TDTC40_PhysicsService);
    procedure DTC40_PhysicsService_Stop(Sender: TDTC40_PhysicsService);
    procedure DTC40_PhysicsService_LinkSuccess(Sender: TDTC40_PhysicsService; Custom_Service_: TDTC40_Custom_Service; Trigger_: TCoreClassObject);
    procedure DTC40_PhysicsService_UserOut(Sender: TDTC40_PhysicsService; Custom_Service_: TDTC40_Custom_Service; Trigger_: TCoreClassObject);
  end;

  { automated physics service }
  TDTC40_PhysicsService = class(TCoreClassInterfacedObject)
  private
    FActivted: Boolean;
    procedure cmd_QueryInfo(Sender: TPeerIO; InData, OutData: TDFE);
  public
    PhysicsAddr: U_String;
    PhysicsPort: Word;
    PhysicsTunnel: TCommunicationFrameworkServer;
    AutoFreePhysicsTunnel: Boolean;
    DependNetworkServicePool: TDTC40_Custom_ServicePool;
    OnEvent: IDTC40_PhysicsService_Event;
    { api }
    constructor Create(PhysicsAddr_: U_String; PhysicsPort_: Word; PhysicsTunnel_: TCommunicationFrameworkServer); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    function BuildDependNetwork(const Depend_: TDTC40_DependNetworkInfoArray): Boolean; overload;
    function BuildDependNetwork(const Depend_: TDTC40_DependNetworkString): Boolean; overload;
    function BuildDependNetwork(const Depend_: U_String): Boolean; overload;
    property Activted: Boolean read FActivted;
    procedure StartService; virtual;
    procedure StopService; virtual;
    { event }
    procedure DoLinkSuccess(Custom_Service_: TDTC40_Custom_Service; Trigger_: TCoreClassObject);
    procedure DoUserOut(Custom_Service_: TDTC40_Custom_Service; Trigger_: TCoreClassObject);
  end;

  TDTC40_PhysicsServicePool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDTC40_PhysicsService>;

  TDTC40_PhysicsServicePool = class(TDTC40_PhysicsServicePool_Decl)
  public
    procedure Progress;
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
  end;
{$ENDREGION 'PhysicsService'}
{$REGION 'PhysicsTunnel'}

  TDCT40_OnQueryResultC = procedure(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList);
  TDCT40_OnQueryResultM = procedure(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList) of object;
{$IFDEF FPC}
  TDCT40_OnQueryResultP = procedure(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList) is nested;
{$ELSE FPC}
  TDCT40_OnQueryResultP = reference to procedure(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList);
{$ENDIF FPC}

  TDCT40_QueryResultData = class
  private
    procedure DoStreamParam(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
    procedure DoStreamFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
    procedure DoRun;
  public
    DTC40_PhysicsTunnel: TDTC40_PhysicsTunnel;
    L: TDTC40_InfoList;
    OnResultC: TDCT40_OnQueryResultC;
    OnResultM: TDCT40_OnQueryResultM;
    OnResultP: TDCT40_OnQueryResultP;
    constructor Create;
    destructor Destroy; override;
  end;

  TDCT40_QueryResultAndDependProcessor = class
  private
    procedure DCT40_OnCheckDepend(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList);
    procedure DCT40_OnAutoP2PVMConnectionDone(Sender: TCommunicationFramework; P_IO: TPeerIO);
    procedure DCT40_OnBuildDependNetwork(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList);
    procedure DoRun(const state: Boolean);
  public
    DTC40_PhysicsTunnel: TDTC40_PhysicsTunnel;
    OnCall: TStateCall;
    OnMethod: TStateMethod;
    OnProc: TStateProc;
    constructor Create;
    destructor Destroy; override;
  end;

  IDTC40_PhysicsTunnel_Event = interface
    procedure DTC40_PhysicsTunnel_Connected(Sender: TDTC40_PhysicsTunnel);
    procedure DTC40_PhysicsTunnel_Disconnect(Sender: TDTC40_PhysicsTunnel);
    procedure DTC40_PhysicsTunnel_Build_Network(Sender: TDTC40_PhysicsTunnel; Custom_Client_: TDTC40_Custom_Client);
    procedure DTC40_PhysicsTunnel_Client_Connected(Sender: TDTC40_PhysicsTunnel; Custom_Client_: TDTC40_Custom_Client);
  end;

  { automated tunnel }
  TDTC40_PhysicsTunnel = class(TCoreClassInterfacedObject, ICommunicationFrameworkClientInterface)
  private
    IsConnecting: Boolean;
    BuildNetworkIsDone: Boolean;
    OfflineTime: TTimeTick;
    procedure DoDelayConnect();
    procedure DoConnectOnResult(const state: Boolean);
    procedure DoConnectAndQuery(Param1: Pointer; Param2: TObject; const state: Boolean);
    procedure DoConnectAndCheckDepend(Param1: Pointer; Param2: TObject; const state: Boolean);
    procedure DoConnectAndBuildDependNetwork(Param1: Pointer; Param2: TObject; const state: Boolean);
  protected
    procedure ClientConnected(Sender: TCommunicationFrameworkClient); virtual;
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); virtual;
  public
    PhysicsAddr: U_String;
    PhysicsPort: Word;
    p2pVM_Auth: U_String;
    PhysicsTunnel: TCommunicationFrameworkClient;
    DependNetworkInfoArray: TDTC40_DependNetworkInfoArray;
    DependNetworkClientPool: TDTC40_Custom_ClientPool;
    OnEvent: IDTC40_PhysicsTunnel_Event;
    { api }
    constructor Create(Addr_: U_String; Port_: Word);
    destructor Destroy; override;
    procedure Progress; virtual;
    function ResetDepend(const Depend_: TDTC40_DependNetworkInfoArray): Boolean; overload;
    function ResetDepend(const Depend_: TDTC40_DependNetworkString): Boolean; overload;
    function ResetDepend(const Depend_: U_String): Boolean; overload;
    function CheckDepend(): Boolean;
    function CheckDependC(OnResult: TStateCall): Boolean;
    function CheckDependM(OnResult: TStateMethod): Boolean;
    function CheckDependP(OnResult: TStateProc): Boolean;
    function BuildDependNetwork(): Boolean;
    function BuildDependNetworkC(OnResult: TStateCall): Boolean;
    function BuildDependNetworkM(OnResult: TStateMethod): Boolean;
    function BuildDependNetworkP(OnResult: TStateProc): Boolean;
    procedure QueryInfoC(OnResult: TDCT40_OnQueryResultC);
    procedure QueryInfoM(OnResult: TDCT40_OnQueryResultM);
    procedure QueryInfoP(OnResult: TDCT40_OnQueryResultP);
    function DependNetworkIsConnected: Boolean;
    { event }
    procedure DoClientConnected(Custom_Client_: TDTC40_Custom_Client);
  end;

  TDTC40_PhysicsTunnelPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDTC40_PhysicsTunnel>;

  TDTC40_PhysicsTunnelPool = class(TDTC40_PhysicsTunnelPool_Decl)
  public
    { find addr }
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    function GetPhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word): TDTC40_PhysicsTunnel;
    { get or create from addr }
    function GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word): TDTC40_PhysicsTunnel; overload;
    function GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word;
      const Depend_: TDTC40_DependNetworkInfoArray; const OnEvent_: IDTC40_PhysicsTunnel_Event): TDTC40_PhysicsTunnel; overload;
    function GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word;
      const Depend_: U_String; const OnEvent_: IDTC40_PhysicsTunnel_Event): TDTC40_PhysicsTunnel; overload;
    { get or create from define }
    function GetOrCreatePhysicsTunnel(dispInfo: TDTC40_Info): TDTC40_PhysicsTunnel; overload;
    function GetOrCreatePhysicsTunnel(dispInfo: TDTC40_Info;
      const Depend_: TDTC40_DependNetworkInfoArray; const OnEvent_: IDTC40_PhysicsTunnel_Event): TDTC40_PhysicsTunnel; overload;
    function GetOrCreatePhysicsTunnel(dispInfo: TDTC40_Info;
      const Depend_: U_String; const OnEvent_: IDTC40_PhysicsTunnel_Event): TDTC40_PhysicsTunnel; overload;
    { fast service connection }
    procedure SearchServiceAndBuildConnection(PhysicsAddr: U_String; PhysicsPort: Word; FullConnection_: Boolean;
      const ServiceTyp: U_String; const OnEvent_: IDTC40_PhysicsTunnel_Event);
    { progress }
    procedure Progress;
  end;
{$ENDREGION 'PhysicsTunnel'}
{$REGION 'infoDefine'}

  TDTC40_Info = class
  private
    Ignored: Boolean;
    procedure MakeHash;
  public
    { share }
    OnlyInstance: Boolean;
    ServiceTyp: U_String;
    PhysicsAddr: U_String;
    PhysicsPort: Word;
    p2pVM_Auth: U_String;
    p2pVM_RecvTunnel_Addr: U_String;
    p2pVM_RecvTunnel_Port: Word;
    p2pVM_SendTunnel_Addr: U_String;
    p2pVM_SendTunnel_Port: Word;
    Workload, MaxWorkload: Integer;
    Hash: TMD5;

    { client translate }
    property p2pVM_ClientRecvTunnel_Addr: U_String read p2pVM_SendTunnel_Addr;
    property p2pVM_ClientRecvTunnel_Port: Word read p2pVM_SendTunnel_Port;
    property p2pVM_ClientSendTunnel_Addr: U_String read p2pVM_RecvTunnel_Addr;
    property p2pVM_ClientSendTunnel_Port: Word read p2pVM_RecvTunnel_Port;

    { api }
    constructor Create;
    destructor Destroy; override;
    procedure Assign(source: TDTC40_Info);
    function Clone: TDTC40_Info;
    procedure Load(stream: TCoreClassStream);
    procedure Save(stream: TCoreClassStream);
    function Same(Data_: TDTC40_Info): Boolean;
    function SameServiceTyp(Data_: TDTC40_Info): Boolean;
    function SamePhysicsAddr(PhysicsAddr_: U_String; PhysicsPort_: Word): Boolean; overload;
    function SamePhysicsAddr(Data_: TDTC40_Info): Boolean; overload;
    function SamePhysicsAddr(Data_: TDTC40_PhysicsTunnel): Boolean; overload;
    function SamePhysicsAddr(Data_: TDTC40_PhysicsService): Boolean; overload;
    function SameP2PVMAddr(Data_: TDTC40_Info): Boolean;
    function ReadyDTC40Client: Boolean;
    function GetOrCreateDTC40Client(Param_: U_String): TDTC40_Custom_Client;
  end;

  TDTC40_InfoList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDTC40_Info>;

  TDTC40_Info_Array = array of TDTC40_Info;

  TDTC40_InfoList = class(TDTC40_InfoList_Decl)
  public
    AutoFree: Boolean;
    constructor Create(AutoFree_: Boolean);
    destructor Destroy; override;
    procedure Remove(obj: TDTC40_Info);
    procedure Delete(index: Integer);
    procedure Clear;
    class procedure SortWorkLoad(L: TDTC40_InfoList);
    function GetInfoArray: TDTC40_Info_Array;
    function IsOnlyInstance(ServiceTyp: U_String): Boolean;
    function GetServiceTypNum(ServiceTyp: U_String): Integer;
    function SearchService(ServiceTyp: U_String): TDTC40_Info_Array;
    function ExistsService(ServiceTyp: U_String): Boolean;
    function ExistsServiceAndPhysicsTunnel(ServiceTyp: U_String; PhysicsTunnel_: TDTC40_PhysicsTunnel): Boolean;
    function FindSame(Data_: TDTC40_Info): TDTC40_Info;
    function FindHash(Hash: TMD5): TDTC40_Info;
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    procedure RemovePhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word);
    function OverwriteInfo(Data_: TDTC40_Info): Boolean;
    function MergeFromDF(D: TDFE): Boolean;
    procedure SaveToDF(D: TDFE);
  end;
{$ENDREGION 'infoDefine'}
{$REGION 'p2pVMCustomService'}

  TDTC40_Custom_Service = class(TCoreClassInterfacedObject)
  private
    FLastSafeCheckTime: TTimeTick;
  public
    Param: U_String;
    ParamList: THashStringList;
    SafeCheckTime: TTimeTick;
    ServiceInfo: TDTC40_Info;
    DTC40PhysicsService: TDTC40_PhysicsService;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); virtual;
    destructor Destroy; override;
    procedure SafeCheck; virtual;
    procedure Progress; virtual;
    procedure SetWorkload(Workload_, MaxWorkload_: Integer);
    procedure UpdateToGlobalDispatch;
    function GetHash: TMD5;
    property Hash: TMD5 read GetHash;
    { event }
    procedure DoLinkSuccess(Trigger_: TCoreClassObject);
    procedure DoUserOut(Trigger_: TCoreClassObject);
  end;

  TDTC40_Custom_Service_Class = class of TDTC40_Custom_Service;

  TDTC40_Custom_ServicePool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDTC40_Custom_Service>;
  TDTC40_Custom_Service_Array = array of TDTC40_Custom_Service;

  TDTC40_Custom_ServicePool = class(TDTC40_Custom_ServicePool_Decl)
  private
    FIPV6_Seed: Word;
  public
    constructor Create;
    procedure Progress;
    procedure MakeP2PVM_IPv6_Port(var ip6, port: U_String);
    function GetServiceFromHash(Hash: TMD5): TDTC40_Custom_Service;
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    function ExistsOnlyInstance(ServiceTyp: U_String): Boolean;
    function GetDTC40Array: TDTC40_Custom_Service_Array;
    function GetFromServiceTyp(ServiceTyp: U_String): TDTC40_Custom_Service_Array;
    function GetFromPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): TDTC40_Custom_Service_Array;
    function GetFromClass(Class_: TDTC40_Custom_Service_Class): TDTC40_Custom_Service_Array;
  end;
{$ENDREGION 'p2pVMCustomService'}
{$REGION 'p2pVMCustomClient'}

  TDTC40_Custom_Client = class(TCoreClassInterfacedObject)
  protected
    procedure DoNetworkOffline; virtual;
  public
    Param: U_String;
    ParamList: THashStringList;
    ClientInfo: TDTC40_Info;
    DTC40PhysicsTunnel: TDTC40_PhysicsTunnel;
    constructor Create(source_: TDTC40_Info; Param_: U_String); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure Connect; virtual;
    function Connected: Boolean; virtual;
    procedure Disconnect; virtual;
    function GetHash: TMD5;
    property Hash: TMD5 read GetHash;
    { event }
    procedure DoClientConnected;
  end;

  TDTC40_Custom_Client_Class = class of TDTC40_Custom_Client;

  TDTC40_Custom_ClientPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDTC40_Custom_Client>;

  TDTC40_Custom_Client_Array = array of TDTC40_Custom_Client;

  TDTC40_Custom_ClientPool_Wait_Data = record
    ServiceTyp_: U_String;
    Client_: TDTC40_Custom_Client;
  end;

  TDTC40_Custom_ClientPool_Wait_States = array of TDTC40_Custom_ClientPool_Wait_Data;

  TOn_DTC40_Custom_Client_EventC = procedure(States_: TDTC40_Custom_ClientPool_Wait_States);
  TOn_DTC40_Custom_Client_EventM = procedure(States_: TDTC40_Custom_ClientPool_Wait_States) of object;
{$IFDEF FPC}
  TOn_DTC40_Custom_Client_EventP = procedure(States_: TDTC40_Custom_ClientPool_Wait_States) is nested;
{$ELSE FPC}
  TOn_DTC40_Custom_Client_EventP = reference to procedure(States_: TDTC40_Custom_ClientPool_Wait_States);
{$ENDIF FPC}

  TDTC40_Custom_ClientPool_Wait = class
  private
    procedure DoRun;
  public
    States_: TDTC40_Custom_ClientPool_Wait_States;
    Pool_: TDTC40_Custom_ClientPool;
    OnCall: TOn_DTC40_Custom_Client_EventC;
    OnMethod: TOn_DTC40_Custom_Client_EventM;
    OnProc: TOn_DTC40_Custom_Client_EventP;
    constructor Create(dependNetwork_: U_String);
    destructor Destroy; override;
  end;

  TDTC40_Custom_ClientPool = class(TDTC40_Custom_ClientPool_Decl)
  private
  public
    procedure Progress;
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    function ExistsServiceInfo(info_: TDTC40_Info): Boolean;
    function ExistsServiceTyp(ServiceTyp: U_String): Boolean;
    function ExistsClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client;
    function ExistsConnectedClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client;
    function ExistsConnectedServiceTyp(ServiceTyp: U_String): TDTC40_Custom_Client;
    function FindPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    function FindServiceInfo(info_: TDTC40_Info): Boolean;
    function FindServiceTyp(ServiceTyp: U_String): Boolean;
    function FindClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client;
    function FindConnectedClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client;
    function FindConnectedServiceTyp(ServiceTyp: U_String): TDTC40_Custom_Client;
    function GetClientFromHash(Hash: TMD5): TDTC40_Custom_Client;
    function GetDTC40Array: TDTC40_Custom_Client_Array;
    function GetFromServiceTyp(ServiceTyp: U_String): TDTC40_Custom_Client_Array;
    function GetFromPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): TDTC40_Custom_Client_Array;
    function GetFromClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client_Array;
    procedure WaitConnectedDoneC(dependNetwork_: U_String; OnResult: TOn_DTC40_Custom_Client_EventC);
    procedure WaitConnectedDoneM(dependNetwork_: U_String; OnResult: TOn_DTC40_Custom_Client_EventM);
    procedure WaitConnectedDoneP(dependNetwork_: U_String; OnResult: TOn_DTC40_Custom_Client_EventP);
  end;
{$ENDREGION 'p2pVMCustomClient'}
{$REGION 'DispatchService'}

  TOnRemovePhysicsNetwork = class
  public
    PhysicsAddr: U_String;
    PhysicsPort: Word;
    constructor Create;
    procedure DoRun; virtual;
  end;

  TOnServiceInfoChange = procedure(Sender: TCoreClassObject; ServiceInfoList: TDTC40_InfoList) of object;

  // dispatch service
  TDTC40_Dispatch_Service = class(TDTC40_Custom_Service)
  private
    FOnServiceInfoChange: TOnServiceInfoChange;
    FWaiting_UpdateServerInfoToAllClient: Boolean;
    FWaiting_UpdateServerInfoToAllClient_TimeTick: TTimeTick;
    DelayCheck_Working: Boolean;
    procedure cmd_UpdateServiceInfo(Sender: TPeerIO; InData: TDFE);
    procedure cmd_UpdateServiceState(Sender: TPeerIO; InData: TDFE);
    procedure cmd_IgnoreChange(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RequestUpdate(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RemovePhysicsNetwork(Sender: TPeerIO; InData: TDFE);
    procedure Prepare_UpdateServerInfoToAllClient;
    procedure UpdateServerInfoToAllClient;

    procedure DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
    procedure DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
    procedure DoDelayCheckLocalServiceInfo;
  public
    Service: TDT_P2PVM_NoAuth_Custom_Service;
    ServiceInfoList: TDTC40_InfoList;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure IgnoreChangeToAllClient(Hash__: TMD5; Ignored: Boolean);
    procedure UpdateServiceStateToAllClient;
    { event }
    property OnServiceInfoChange: TOnServiceInfoChange read FOnServiceInfoChange write FOnServiceInfoChange;
  end;
{$ENDREGION 'DispatchService'}
{$REGION 'DispatchClient'}

  // dispatch client
  TDTC40_Dispatch_Client = class(TDTC40_Custom_Client)
  private
    FOnServiceInfoChange: TOnServiceInfoChange;
    DelayCheck_Working: Boolean;
    procedure cmd_UpdateServiceInfo(Sender: TPeerIO; InData: TDFE);
    procedure cmd_UpdateServiceState(Sender: TPeerIO; InData: TDFE);
    procedure cmd_IgnoreChange(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RemovePhysicsNetwork(Sender: TPeerIO; InData: TDFE);
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
    procedure DoDelayCheckLocalServiceInfo;
  public
    Client: TDT_P2PVM_NoAuth_Custom_Client;
    ServiceInfoList: TDTC40_InfoList;
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure PostLocalServiceInfo(forcePost_: Boolean);
    procedure RequestUpdate();
    procedure IgnoreChangeToService(Hash__: TMD5; Ignored: Boolean);
    procedure UpdateLocalServiceState;
    procedure RemovePhysicsNetwork(PhysicsAddr: U_String; PhysicsPort: Word);
    { event }
    property OnServiceInfoChange: TOnServiceInfoChange read FOnServiceInfoChange write FOnServiceInfoChange;
  end;
{$ENDREGION 'DispatchClient'}
{$REGION 'RegistedData'}

  TDTC40_RegistedData = record
    ServiceTyp: U_String;
    ServiceClass: TDTC40_Custom_Service_Class;
    ClientClass: TDTC40_Custom_Client_Class;
  end;

  PDTC40_RegistedData = ^TDTC40_RegistedData;

  TDTC40_RegistedDataList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PDTC40_RegistedData>;

  TDTC40_RegistedDataList = class(TDTC40_RegistedDataList_Decl)
  public
    destructor Destroy; override;
    procedure Clean;
    procedure Print;
  end;
{$ENDREGION 'RegistedData'}
{$REGION 'DTC40NoAuthModel'}

  TDTC40_Base_NoAuth_Service = class(TDTC40_Custom_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
  public
    Service: TDT_P2PVM_NoAuth_Custom_Service;
    DTNoAuthService: TDTService_NoAuth;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
  end;

  TDTC40_Base_NoAuth_Client = class(TDTC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_NoAuth_Custom_Client;
    DTNoAuthClient: TDTClient_NoAuth;
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
  end;

  TDTC40_Base_DataStoreNoAuth_Service = class(TDTC40_Custom_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
  public
    Service: TDT_P2PVM_NoAuth_Custom_Service;
    DTNoAuthService: TDataStoreService_NoAuth;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
  end;

  TDTC40_Base_DataStoreNoAuth_Client = class(TDTC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_DataStoreNoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_NoAuth_Custom_Client;
    DTNoAuthClient: TDataStoreClient_NoAuth;
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
  end;

{$ENDREGION 'DTC40NoAuthModel'}
{$REGION 'DTC40VirtualAuthModel'}

  TDTC40_Base_VirtualAuth_Service = class(TDTC40_Custom_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); virtual;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); virtual;
    procedure DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
  public
    Service: TDT_P2PVM_VirtualAuth_Custom_Service;
    DTVirtualAuthService: TDTService_VirtualAuth;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
  end;

  TDTC40_Base_VirtualAuth_Client = class(TDTC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_VirtualAuth_Custom_Client;
    DTVirtualAuthClient: TDTClient_VirtualAuth;
    UserName, Password: U_String;
    NoDTLink: Boolean;
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    function LoginIsSuccessed: Boolean;
  end;

  TDTC40_Base_DataStoreVirtualAuth_Service = class(TDTC40_Custom_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); virtual;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); virtual;
    procedure DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
  public
    Service: TDT_P2PVM_VirtualAuth_Custom_Service;
    DTVirtualAuthService: TDataStoreService_VirtualAuth;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
  end;

  TDTC40_Base_DataStoreVirtualAuth_Client = class(TDTC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_VirtualAuth_Custom_Client;
    DTVirtualAuthClient: TDataStoreClient_VirtualAuth;
    UserName, Password: U_String;
    NoDTLink: Boolean;
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    function LoginIsSuccessed: Boolean;
  end;

{$ENDREGION 'DTC40VirtualAuthModel'}
{$REGION 'DTC40BuildInAuthModel'}

  TDTC40_Base_Service = class(TDTC40_Custom_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
  public
    Service: TDT_P2PVM_Custom_Service;
    DTService: TDTService;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

  TDTC40_Base_Client = class(TDTC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_Custom_Client;
    DTClient: TDTClient;
    UserName, Password: U_String;
    NoDTLink: Boolean;
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    function LoginIsSuccessed: Boolean;
  end;

  TDTC40_Base_DataStore_Service = class(TDTC40_Custom_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
  public
    Service: TDT_P2PVM_Custom_Service;
    DTService: TDataStoreService;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

  TDTC40_Base_DataStore_Client = class(TDTC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_Custom_Client;
    DTClient: TDataStoreClient;
    UserName, Password: U_String;
    NoDTLink: Boolean;
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    function LoginIsSuccessed: Boolean;
  end;

{$ENDREGION 'DTC40BuildInAuthModel'}


var
  { quiet mode, defualt is false }
  DTC40_QuietMode: Boolean;
  { physics service safeCheck time, default is 10 minute }
  DTC40_SafeCheckTime: TTimeTick;
  { DTC40 reconnection delay time, default is 5.0(float) seconds }
  DTC40_PhysicsReconnectionDelayTime: Double;
  { DTC40 Dispatch Service info update delay, default is 1 seconds }
  DTC40_UpdateServiceInfoDelayTime: TTimeTick;
  { physics service timeout, default is 1 minute }
  DTC40_PhysicsServiceTimeout: TTimeTick;
  { physics tunnel timeout, default is 30 seconds }
  DTC40_PhysicsTunnelTimeout: TTimeTick;
  { kill dead physics connection timeout, default is 1 minute }
  DTC40_KillDeadPhysicsConnectionTimeout: TTimeTick;
  { kill IDC fault timeout, default is 1 hours }
  DTC40_KillIDCFaultTimeout: TTimeTick;
  { root path, default is current Directory }
  DTC40_RootPath: U_String;
  { p2pVM default password }
  DTC40_Password: SystemString = 'DTC40@ZSERVER';
  { PhysicsTunnel interface }
  DTC40_PhysicsClientClass: TCommunicationFrameworkClientClass;
  { automated matched }
  DTC40_Registed: TDTC40_RegistedDataList;
  { physics service pool }
  DTC40_PhysicsServicePool: TDTC40_PhysicsServicePool;
  { custom service pool }
  DTC40_ServicePool: TDTC40_Custom_ServicePool;
  { physics tunnel pool }
  DTC40_PhysicsTunnelPool: TDTC40_PhysicsTunnelPool;
  { custom client pool }
  DTC40_ClientPool: TDTC40_Custom_ClientPool;

procedure C40Progress; { DTC40 main progress }

{ free all DTC40 system }
procedure C40Clean;

{ print state }
procedure C40PrintRegistation;
function C40ExistsPhysicsNetwork(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;

{ Kill physics tunnel }
procedure C40RemovePhysics(PhysicsAddr: U_String; PhysicsPort: Word;
  Remove_P2PVM_Client_, Remove_Physics_Client_, RemoveP2PVM_Service_, Remove_Physcis_Service_: Boolean); overload;
procedure C40RemovePhysics(Tunnel_: TDTC40_PhysicsTunnel); overload;
procedure C40RemovePhysics(Service_: TDTC40_PhysicsService); overload;
procedure C40CheckAndKillDeadPhysicsTunnel();

{ register }
function RegisterC40(ServiceTyp: U_String; ServiceClass: TDTC40_Custom_Service_Class; ClientClass: TDTC40_Custom_Client_Class): Boolean;
function FindRegistedC40(ServiceTyp: U_String): PDTC40_RegistedData;

{ misc }
function ExtractDependInfo(info: U_String): TDTC40_DependNetworkInfoArray; overload;
function ExtractDependInfo(arry: TDTC40_DependNetworkString): TDTC40_DependNetworkInfoArray; overload;

implementation

var
  C40Progress_Working: Boolean = False;

procedure C40Progress;
begin
  if C40Progress_Working then
      exit;
  C40Progress_Working := True;
  try
    CheckThread;
    DTC40_PhysicsServicePool.Progress;
    DTC40_ServicePool.Progress;
    DTC40_PhysicsTunnelPool.Progress;
    DTC40_ClientPool.Progress;
    C40CheckAndKillDeadPhysicsTunnel();
  finally
      C40Progress_Working := False;
  end;
end;

procedure C40Clean;
begin
  while DTC40_ClientPool.Count > 0 do
      DisposeObject(DTC40_ClientPool[0]);
  while DTC40_ServicePool.Count > 0 do
      DisposeObject(DTC40_ServicePool[0]);
  DTC40_ServicePool.FIPV6_Seed := 1;
  while DTC40_PhysicsTunnelPool.Count > 0 do
      DisposeObject(DTC40_PhysicsTunnelPool[0]);
  while DTC40_PhysicsServicePool.Count > 0 do
      DisposeObject(DTC40_PhysicsServicePool[0]);
end;

procedure C40PrintRegistation;
begin
  DTC40_Registed.Print;
end;

function C40ExistsPhysicsNetwork(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
begin
  Result := True;
  if
    DTC40_PhysicsServicePool.ExistsPhysicsAddr(PhysicsAddr, PhysicsPort) or
    DTC40_ServicePool.ExistsPhysicsAddr(PhysicsAddr, PhysicsPort) or
    DTC40_PhysicsTunnelPool.ExistsPhysicsAddr(PhysicsAddr, PhysicsPort) or
    DTC40_ClientPool.ExistsPhysicsAddr(PhysicsAddr, PhysicsPort) then
      exit;
  Result := False;
end;

procedure C40RemovePhysics(PhysicsAddr: U_String; PhysicsPort: Word;
  Remove_P2PVM_Client_, Remove_Physics_Client_, RemoveP2PVM_Service_, Remove_Physcis_Service_: Boolean);
var
  i: Integer;
begin
  if Remove_P2PVM_Client_ then
    begin
      try
        { remove client }
        i := 0;
        while i < DTC40_ClientPool.Count do
          if PhysicsAddr.Same(@DTC40_ClientPool[i].ClientInfo.PhysicsAddr) and (PhysicsPort = DTC40_ClientPool[i].ClientInfo.PhysicsPort) then
            begin
              DisposeObject(DTC40_ClientPool[i]);
              i := 0;
            end
          else
              inc(i);
      except
      end;
    end;

  { remove dispatch info }
  for i := 0 to DTC40_ClientPool.Count - 1 do
    if DTC40_ClientPool[i] is TDTC40_Dispatch_Client then
        TDTC40_Dispatch_Client(DTC40_ClientPool[i]).ServiceInfoList.RemovePhysicsAddr(PhysicsAddr, PhysicsPort);

  if Remove_Physics_Client_ then
    begin
      try
        { remove physics tunnel }
        i := 0;
        while i < DTC40_PhysicsTunnelPool.Count do
          begin
            if PhysicsAddr.Same(@DTC40_PhysicsTunnelPool[i].PhysicsAddr) and (PhysicsPort = DTC40_PhysicsTunnelPool[i].PhysicsPort) then
              begin
                DisposeObject(DTC40_PhysicsTunnelPool[i]);
                i := 0;
              end
            else
                inc(i);
          end;
      except
      end;
    end;

  if RemoveP2PVM_Service_ then
    begin
      try
        { remove service }
        i := 0;
        while i < DTC40_ServicePool.Count do
          if PhysicsAddr.Same(@DTC40_ServicePool[i].ServiceInfo.PhysicsAddr) and (PhysicsPort = DTC40_ServicePool[i].ServiceInfo.PhysicsPort) then
            begin
              DisposeObject(DTC40_ServicePool[i]);
              i := 0;
            end
          else
              inc(i);
      except
      end;
    end;

  { remove service info }
  for i := 0 to DTC40_ServicePool.Count - 1 do
    if DTC40_ServicePool[i] is TDTC40_Dispatch_Service then
        TDTC40_Dispatch_Service(DTC40_ServicePool[i]).ServiceInfoList.RemovePhysicsAddr(PhysicsAddr, PhysicsPort);

  if Remove_Physcis_Service_ then
    begin
      try
        { remove physics service }
        i := 0;
        while i < DTC40_PhysicsServicePool.Count do
          begin
            if PhysicsAddr.Same(@DTC40_PhysicsServicePool[i].PhysicsAddr) and (PhysicsPort = DTC40_PhysicsServicePool[i].PhysicsPort) then
              begin
                DisposeObject(DTC40_PhysicsServicePool[i]);
                i := 0;
              end
            else
                inc(i);
          end;
      except
      end;
    end;
end;

procedure C40RemovePhysics(Tunnel_: TDTC40_PhysicsTunnel);
begin
  C40RemovePhysics(Tunnel_.PhysicsAddr, Tunnel_.PhysicsPort, True, True, False, False);
end;

procedure C40RemovePhysics(Service_: TDTC40_PhysicsService);
begin
  C40RemovePhysics(Service_.PhysicsAddr, Service_.PhysicsPort, True, True, True, True);
end;

procedure C40CheckAndKillDeadPhysicsTunnel();
var
  i: Integer;
  tmp: TDTC40_PhysicsTunnel;
begin
  i := 0;
  while i < DTC40_PhysicsTunnelPool.Count do
    begin
      tmp := DTC40_PhysicsTunnelPool[i];
      if (not tmp.PhysicsTunnel.RemoteInited) and (not tmp.BuildNetworkIsDone) and
        (tmp.OfflineTime > 0) and (GetTimeTick - tmp.OfflineTime > DTC40_KillDeadPhysicsConnectionTimeout) then
        begin
          C40RemovePhysics(tmp);
          i := 0;
        end
      else if (not tmp.PhysicsTunnel.RemoteInited) and (tmp.BuildNetworkIsDone) and
        (tmp.OfflineTime > 0) and (GetTimeTick - tmp.OfflineTime > DTC40_KillIDCFaultTimeout) then
        begin
          C40RemovePhysics(tmp);
          i := 0;
        end
      else
          inc(i);
    end;
end;

function RegisterC40(ServiceTyp: U_String; ServiceClass: TDTC40_Custom_Service_Class; ClientClass: TDTC40_Custom_Client_Class): Boolean;
var
  i: Integer;
  p: PDTC40_RegistedData;
begin
  Result := False;
  for i := 0 to DTC40_Registed.Count - 1 do
    if ServiceTyp.Same(@DTC40_Registed[i]^.ServiceTyp) then
      begin
        RaiseInfo('"%s" repeat registion.', [ServiceTyp.Text]);
        exit;
      end;

  new(p);
  p^.ServiceTyp := ServiceTyp;
  p^.ServiceClass := ServiceClass;
  p^.ClientClass := ClientClass;
  DTC40_Registed.Add(p);
  Result := True;
end;

function FindRegistedC40(ServiceTyp: U_String): PDTC40_RegistedData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to DTC40_Registed.Count - 1 do
    if ServiceTyp.Same(@DTC40_Registed[i]^.ServiceTyp) then
      begin
        Result := DTC40_Registed[i];
        exit;
      end;
end;

function ExtractDependInfo(info: U_String): TDTC40_DependNetworkInfoArray;
var
  tmp: TDTC40_DependNetworkString;
begin
  umlGetSplitArray(info, tmp, '|<>');
  Result := ExtractDependInfo(tmp);
end;

function ExtractDependInfo(arry: TDTC40_DependNetworkString): TDTC40_DependNetworkInfoArray;
var
  i: Integer;
  info_: TDTC40_DependNetworkInfo;
begin
  SetLength(Result, Length(arry));
  for i := 0 to Length(arry) - 1 do
    begin
      info_.Typ := umlTrimSpace(umlGetFirstStr(arry[i], '@'));
      info_.Param := umlTrimSpace(umlDeleteFirstStr(arry[i], '@'));
      Result[i] := info_;
    end;
end;

procedure TDTC40_PhysicsService.cmd_QueryInfo(Sender: TPeerIO; InData, OutData: TDFE);
var
  i, j: Integer;
  L: TDTC40_InfoList;
  DPS_: TDTC40_Dispatch_Service;
begin
  L := TDTC40_InfoList.Create(False);

  // search all service
  for i := 0 to DTC40_ServicePool.Count - 1 do
    begin
      if L.FindSame(DTC40_ServicePool[i].ServiceInfo) = nil then
          L.Add(DTC40_ServicePool[i].ServiceInfo);

      // dispatch service
      if DTC40_ServicePool[i] is TDTC40_Dispatch_Service then
        begin
          DPS_ := DTC40_ServicePool[i] as TDTC40_Dispatch_Service;
          for j := 0 to DPS_.ServiceInfoList.Count - 1 do
            if L.FindSame(DPS_.ServiceInfoList[j]) = nil then
                L.Add(DPS_.ServiceInfoList[j]);
        end;
    end;

  L.SaveToDF(OutData);
  DisposeObject(L);
end;

constructor TDTC40_PhysicsService.Create(PhysicsAddr_: U_String; PhysicsPort_: Word; PhysicsTunnel_: TCommunicationFrameworkServer);
begin
  inherited Create;
  FActivted := False;
  PhysicsAddr := umlTrimSpace(PhysicsAddr_);
  PhysicsPort := PhysicsPort_;
  PhysicsTunnel := PhysicsTunnel_;
  PhysicsTunnel.AutomatedP2PVMAuthToken := DTC40_Password;
  PhysicsTunnel.TimeOutKeepAlive := True;
  PhysicsTunnel.IdleTimeOut := DTC40_PhysicsServiceTimeout;
  PhysicsTunnel.RegisterStream('QueryInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_QueryInfo;
  PhysicsTunnel.PrintParams['QueryInfo'] := False;
  PhysicsTunnel.QuietMode := DTC40_QuietMode;
  AutoFreePhysicsTunnel := False;
  DependNetworkServicePool := TDTC40_Custom_ServicePool.Create;
  OnEvent := nil;
  DTC40_PhysicsServicePool.Add(Self);
end;

destructor TDTC40_PhysicsService.Destroy;
begin
  DTC40_PhysicsServicePool.Remove(Self);
  PhysicsTunnel.DeleteRegistedCMD('QueryInfo');
  DisposeObject(DependNetworkServicePool);
  if AutoFreePhysicsTunnel then
      DisposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TDTC40_PhysicsService.Progress;
begin
  PhysicsTunnel.Progress;
end;

function TDTC40_PhysicsService.BuildDependNetwork(const Depend_: TDTC40_DependNetworkInfoArray): Boolean;
var
  i: Integer;
  p: PDTC40_RegistedData;
  tmp: TDTC40_Custom_Service;
begin
  Result := False;

  for i := 0 to Length(Depend_) - 1 do
    begin
      p := FindRegistedC40(Depend_[i].Typ);
      if p = nil then
        begin
          PhysicsTunnel.Print('no found Registed service "%s"', [Depend_[i].Typ.Text]);
          exit;
        end;

      tmp := p^.ServiceClass.Create(Self, p^.ServiceTyp, Depend_[i].Param);
      PhysicsTunnel.Print('Build Depend service "%s" instance class "%s"', [tmp.ServiceInfo.ServiceTyp.Text, tmp.ClassName]);
      PhysicsTunnel.Print('service %s p2pVM Received tunnel ip %s port: %d', [tmp.ServiceInfo.ServiceTyp.Text, tmp.ServiceInfo.p2pVM_RecvTunnel_Addr.Text, tmp.ServiceInfo.p2pVM_RecvTunnel_Port]);
      PhysicsTunnel.Print('service %s p2pVM Send tunnel ip %s port: %d', [tmp.ServiceInfo.ServiceTyp.Text, tmp.ServiceInfo.p2pVM_SendTunnel_Addr.Text, tmp.ServiceInfo.p2pVM_SendTunnel_Port]);

      if Assigned(OnEvent) then
          OnEvent.DTC40_PhysicsService_Build_Network(Self, tmp);
    end;
  Result := True;
end;

function TDTC40_PhysicsService.BuildDependNetwork(const Depend_: TDTC40_DependNetworkString): Boolean;
begin
  Result := BuildDependNetwork(ExtractDependInfo(Depend_));
end;

function TDTC40_PhysicsService.BuildDependNetwork(const Depend_: U_String): Boolean;
begin
  Result := BuildDependNetwork(ExtractDependInfo(Depend_));
end;

procedure TDTC40_PhysicsService.StartService;
begin
  FActivted := PhysicsTunnel.StartService(PhysicsAddr, PhysicsPort);
  if FActivted then
    begin
      PhysicsTunnel.Print('Physics Service Listening successed, internet addr: %s port: %d', [PhysicsAddr.Text, PhysicsPort]);
      if Assigned(OnEvent) then
          OnEvent.DTC40_PhysicsService_Start(Self);
    end
  else
      PhysicsTunnel.Print('Physics Service Listening failed, internet addr: %s port: %d', [PhysicsAddr.Text, PhysicsPort]);
end;

procedure TDTC40_PhysicsService.StopService;
begin
  if not FActivted then
      exit;
  PhysicsTunnel.StopService;
  PhysicsTunnel.Print('Physics Service Listening Stop.', []);
  if Assigned(OnEvent) then
      OnEvent.DTC40_PhysicsService_Stop(Self);
end;

procedure TDTC40_PhysicsService.DoLinkSuccess(Custom_Service_: TDTC40_Custom_Service; Trigger_: TCoreClassObject);
begin
  if Assigned(OnEvent) then
      OnEvent.DTC40_PhysicsService_LinkSuccess(Self, Custom_Service_, Trigger_);
end;

procedure TDTC40_PhysicsService.DoUserOut(Custom_Service_: TDTC40_Custom_Service; Trigger_: TCoreClassObject);
begin
  if Assigned(OnEvent) then
      OnEvent.DTC40_PhysicsService_UserOut(Self, Custom_Service_, Trigger_);
end;

procedure TDTC40_PhysicsServicePool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

function TDTC40_PhysicsServicePool.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
        exit;
  Result := False;
end;

procedure TDCT40_QueryResultData.DoStreamParam(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
begin
  L.MergeFromDF(Result_);
  DoRun;
end;

procedure TDCT40_QueryResultData.DoStreamFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
begin
  DoRun;
end;

procedure TDCT40_QueryResultData.DoRun;
begin
  try
    if Assigned(OnResultC) then
        OnResultC(DTC40_PhysicsTunnel, L);
    if Assigned(OnResultM) then
        OnResultM(DTC40_PhysicsTunnel, L);
    if Assigned(OnResultP) then
        OnResultP(DTC40_PhysicsTunnel, L);
  except
  end;
  Free;
end;

constructor TDCT40_QueryResultData.Create;
begin
  inherited Create;
  DTC40_PhysicsTunnel := nil;
  L := TDTC40_InfoList.Create(True);
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TDCT40_QueryResultData.Destroy;
begin
  DisposeObject(L);
  inherited Destroy;
end;

procedure TDCT40_QueryResultAndDependProcessor.DCT40_OnCheckDepend(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList);
var
  i: Integer;
  state: Boolean;
begin
  state := True;
  for i := 0 to Length(Sender.DependNetworkInfoArray) - 1 do
    begin
      if L.ExistsService(Sender.DependNetworkInfoArray[i].Typ) then
        begin
          Sender.PhysicsTunnel.Print('Check addr %s port:%d service "%s" passed.', [Sender.PhysicsAddr.Text, Sender.PhysicsPort, Sender.DependNetworkInfoArray[i].Typ.Text]);
        end
      else
        begin
          Sender.PhysicsTunnel.Print('failed! Check addr %s port:%d no found service "%s".', [Sender.PhysicsAddr.Text, Sender.PhysicsPort, Sender.DependNetworkInfoArray[i].Typ.Text]);
          state := False;
        end;
    end;
  DoRun(state);
end;

procedure TDCT40_QueryResultAndDependProcessor.DCT40_OnAutoP2PVMConnectionDone(Sender: TCommunicationFramework; P_IO: TPeerIO);
var
  i: Integer;
begin
  Sender.AutomatedP2PVMClient := True;

  for i := 0 to DTC40_PhysicsTunnel.DependNetworkClientPool.Count - 1 do
    with DTC40_PhysicsTunnel.DependNetworkClientPool[i] do
      if not Connected then
          Connect;

  DTC40_PhysicsTunnel.BuildNetworkIsDone := True;
  DTC40_PhysicsTunnel.OfflineTime := 0;
  DoRun(True);
end;

procedure TDCT40_QueryResultAndDependProcessor.DCT40_OnBuildDependNetwork(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList);
var
  i, j: Integer;
  found_: Integer;
  tmp: TDTC40_Custom_Client;
begin
  found_ := 0;
  for i := 0 to Length(Sender.DependNetworkInfoArray) - 1 do
    if L.ExistsService(Sender.DependNetworkInfoArray[i].Typ) then
        inc(found_);

  if found_ = 0 then
    begin
      DoRun(False);
      exit;
    end;

  for i := 0 to Length(Sender.DependNetworkInfoArray) - 1 do
    for j := 0 to L.Count - 1 do
      begin
        if L[j].SamePhysicsAddr(Sender) and L[j].ServiceTyp.Same(@Sender.DependNetworkInfoArray[i].Typ) and
          (not Sender.DependNetworkClientPool.ExistsServiceInfo(L[j])) then
          begin
            tmp := L[j].GetOrCreateDTC40Client(Sender.DependNetworkInfoArray[i].Param);
            if tmp <> nil then
              begin
                Sender.PhysicsTunnel.Print('build "%s" network done.', [L[j].ServiceTyp.Text]);
                Sender.PhysicsTunnel.Print('"%s" network physics address "%s" physics port "%d" DCT40 Class:%s',
                  [L[j].ServiceTyp.Text, Sender.PhysicsAddr.Text, Sender.PhysicsPort, tmp.ClassName]);
                Sender.PhysicsTunnel.Print('"%s" network p2pVM Received Tunnel IPV6 "%s" Port:%d',
                  [L[j].ServiceTyp.Text, L[j].p2pVM_RecvTunnel_Addr.Text, L[j].PhysicsPort]);
                Sender.PhysicsTunnel.Print('"%s" network p2pVM Send Tunnel IPV6 "%s" Port:%d',
                  [L[j].ServiceTyp.Text, L[j].p2pVM_SendTunnel_Addr.Text, L[j].PhysicsPort]);

                if Assigned(DTC40_PhysicsTunnel.OnEvent) then
                    DTC40_PhysicsTunnel.OnEvent.DTC40_PhysicsTunnel_Build_Network(DTC40_PhysicsTunnel, tmp);
              end
            else
              begin
                Sender.PhysicsTunnel.Print('build "%s" network error.', [L[j].ServiceTyp.Text]);
              end;
          end;
      end;
  Sender.PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DCT40_OnAutoP2PVMConnectionDone;
  Sender.PhysicsTunnel.AutomatedP2PVM_Open(Sender.PhysicsTunnel.ClientIO);
end;

procedure TDCT40_QueryResultAndDependProcessor.DoRun(const state: Boolean);
begin
  if Assigned(OnCall) then
      OnCall(state);
  if Assigned(OnMethod) then
      OnMethod(state);
  if Assigned(OnProc) then
      OnProc(state);
  Free;
end;

constructor TDCT40_QueryResultAndDependProcessor.Create;
begin
  inherited Create;
  DTC40_PhysicsTunnel := nil;
  OnCall := nil;
  OnMethod := nil;
  OnProc := nil;
end;

destructor TDCT40_QueryResultAndDependProcessor.Destroy;
begin
  inherited Destroy;
end;

procedure TDTC40_PhysicsTunnel.DoDelayConnect;
begin
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, {$IFDEF FPC}@{$ENDIF FPC}DoConnectOnResult);
end;

procedure TDTC40_PhysicsTunnel.DoConnectOnResult(const state: Boolean);
begin
  if state then
      PhysicsTunnel.Print('Physics Tunnel connection successed, internet addr: %s port: %d', [PhysicsAddr.Text, PhysicsPort])
  else
      PhysicsTunnel.Print('Physics Tunnel connection failed, internet addr: %s port: %d', [PhysicsAddr.Text, PhysicsPort]);
  IsConnecting := False;
end;

procedure TDTC40_PhysicsTunnel.DoConnectAndQuery(Param1: Pointer; Param2: TObject; const state: Boolean);
var
  tmp: TDCT40_QueryResultData;
begin
  DoConnectOnResult(state);
  tmp := TDCT40_QueryResultData(Param2);
  if state then
    begin
      PhysicsTunnel.SendStreamCmdM('QueryInfo', nil, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParam, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailed);
    end
  else
    begin
      try
          tmp.DoRun;
      except
      end;
    end;
end;

procedure TDTC40_PhysicsTunnel.DoConnectAndCheckDepend(Param1: Pointer; Param2: TObject; const state: Boolean);
var
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  DoConnectOnResult(state);
  tmp := TDCT40_QueryResultAndDependProcessor(Param2);
  if state then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
    end
  else
    begin
      try
          tmp.DoRun(state);
      except
      end;
    end;
end;

procedure TDTC40_PhysicsTunnel.DoConnectAndBuildDependNetwork(Param1: Pointer; Param2: TObject; const state: Boolean);
var
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  DoConnectOnResult(state);
  tmp := TDCT40_QueryResultAndDependProcessor(Param2);
  if state then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
    end
  else
    begin
      try
          tmp.DoRun(state);
      except
      end;
    end;
end;

procedure TDTC40_PhysicsTunnel.ClientConnected(Sender: TCommunicationFrameworkClient);
begin
  try
    if Assigned(OnEvent) then
        OnEvent.DTC40_PhysicsTunnel_Connected(Self);
  except
  end;
end;

procedure TDTC40_PhysicsTunnel.ClientDisconnect(Sender: TCommunicationFrameworkClient);
var
  i: Integer;
begin
  try
    if Assigned(OnEvent) then
        OnEvent.DTC40_PhysicsTunnel_Disconnect(Self);
  except
  end;

  try
    for i := 0 to DependNetworkClientPool.Count - 1 do
        DependNetworkClientPool[i].DoNetworkOffline;
  except
  end;
end;

constructor TDTC40_PhysicsTunnel.Create(Addr_: U_String; Port_: Word);
var
  i: Integer;
begin
  inherited Create;
  IsConnecting := False;
  BuildNetworkIsDone := False;
  OfflineTime := GetTimeTick;

  PhysicsAddr := umlTrimSpace(Addr_);
  PhysicsPort := Port_;
  PhysicsTunnel := DTC40_PhysicsClientClass.Create;
  PhysicsTunnel.AutomatedP2PVMAuthToken := DTC40_Password;
  PhysicsTunnel.TimeOutKeepAlive := True;
  PhysicsTunnel.IdleTimeOut := DTC40_PhysicsTunnelTimeout;
  PhysicsTunnel.SyncOnResult := False;
  PhysicsTunnel.SyncOnCompleteBuffer := True;
  PhysicsTunnel.SwitchDefaultPerformance;
  PhysicsTunnel.OnInterface := Self;
  PhysicsTunnel.PrintParams['QueryInfo'] := False;
  PhysicsTunnel.QuietMode := DTC40_QuietMode;

  p2pVM_Auth := PhysicsTunnel.AutomatedP2PVMAuthToken;
  SetLength(DependNetworkInfoArray, 0);
  DependNetworkClientPool := TDTC40_Custom_ClientPool.Create;
  OnEvent := nil;
  DTC40_PhysicsTunnelPool.Add(Self);
end;

destructor TDTC40_PhysicsTunnel.Destroy;
begin
  OnEvent := nil;
  DTC40_PhysicsTunnelPool.Remove(Self);
  PhysicsAddr := '';
  SetLength(DependNetworkInfoArray, 0);
  DisposeObject(DependNetworkClientPool);
  DisposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TDTC40_PhysicsTunnel.Progress;
begin
  PhysicsTunnel.Progress;

  { check state and reconnection }
  if BuildNetworkIsDone and (not IsConnecting) and (not PhysicsTunnel.RemoteInited) then
    begin
      IsConnecting := True;
      PhysicsTunnel.PostProgress.PostExecuteM_NP(DTC40_PhysicsReconnectionDelayTime, {$IFDEF FPC}@{$ENDIF FPC}DoDelayConnect);
    end;

  { check offline state }
  if (OfflineTime = 0) and (not PhysicsTunnel.RemoteInited) then
      OfflineTime := GetTimeTick;
end;

function TDTC40_PhysicsTunnel.ResetDepend(const Depend_: TDTC40_DependNetworkInfoArray): Boolean;
var
  i: Integer;
begin
  SetLength(DependNetworkInfoArray, Length(Depend_));
  for i := 0 to Length(Depend_) - 1 do
      DependNetworkInfoArray[i] := Depend_[i];

  Result := False;
  for i := 0 to Length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
        exit;
  Result := True;
end;

function TDTC40_PhysicsTunnel.ResetDepend(const Depend_: TDTC40_DependNetworkString): Boolean;
begin
  Result := ResetDepend(ExtractDependInfo(Depend_));
end;

function TDTC40_PhysicsTunnel.ResetDepend(const Depend_: U_String): Boolean;
begin
  Result := ResetDepend(ExtractDependInfo(Depend_));
end;

function TDTC40_PhysicsTunnel.CheckDepend(): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;

  Result := True;
  for i := 0 to Length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.DTC40_PhysicsTunnel := Self;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndCheckDepend);
end;

function TDTC40_PhysicsTunnel.CheckDependC(OnResult: TStateCall): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;

  Result := True;
  for i := 0 to Length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.DTC40_PhysicsTunnel := Self;
  tmp.OnCall := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndCheckDepend);
end;

function TDTC40_PhysicsTunnel.CheckDependM(OnResult: TStateMethod): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;

  Result := True;
  for i := 0 to Length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.DTC40_PhysicsTunnel := Self;
  tmp.OnMethod := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndCheckDepend);
end;

function TDTC40_PhysicsTunnel.CheckDependP(OnResult: TStateProc): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;

  Result := True;
  for i := 0 to Length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.DTC40_PhysicsTunnel := Self;
  tmp.OnProc := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndCheckDepend);
end;

function TDTC40_PhysicsTunnel.BuildDependNetwork: Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;
  if BuildNetworkIsDone then
      exit;

  Result := True;
  for i := 0 to Length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.DTC40_PhysicsTunnel := Self;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndBuildDependNetwork);
end;

function TDTC40_PhysicsTunnel.BuildDependNetworkC(OnResult: TStateCall): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;
  if BuildNetworkIsDone then
    begin
      tmp := TDCT40_QueryResultAndDependProcessor.Create;
      tmp.DTC40_PhysicsTunnel := Self;
      tmp.OnCall := OnResult;
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      PhysicsTunnel.AutomatedP2PVM_Open();
      exit;
    end;

  Result := True;
  for i := 0 to Length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.DTC40_PhysicsTunnel := Self;
  tmp.OnCall := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndBuildDependNetwork);
end;

function TDTC40_PhysicsTunnel.BuildDependNetworkM(OnResult: TStateMethod): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;
  if BuildNetworkIsDone then
    begin
      tmp := TDCT40_QueryResultAndDependProcessor.Create;
      tmp.DTC40_PhysicsTunnel := Self;
      tmp.OnMethod := OnResult;
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      PhysicsTunnel.AutomatedP2PVM_Open();
      exit;
    end;

  Result := True;
  for i := 0 to Length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.DTC40_PhysicsTunnel := Self;
  tmp.OnMethod := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndBuildDependNetwork);
end;

function TDTC40_PhysicsTunnel.BuildDependNetworkP(OnResult: TStateProc): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;
  if BuildNetworkIsDone then
    begin
      tmp := TDCT40_QueryResultAndDependProcessor.Create;
      tmp.DTC40_PhysicsTunnel := Self;
      tmp.OnProc := OnResult;
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      PhysicsTunnel.AutomatedP2PVM_Open();
      exit;
    end;

  Result := True;
  for i := 0 to Length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.DTC40_PhysicsTunnel := Self;
  tmp.OnProc := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndBuildDependNetwork);
end;

procedure TDTC40_PhysicsTunnel.QueryInfoC(OnResult: TDCT40_OnQueryResultC);
var
  tmp: TDCT40_QueryResultData;
begin
  tmp := TDCT40_QueryResultData.Create;
  tmp.DTC40_PhysicsTunnel := Self;
  tmp.OnResultC := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      PhysicsTunnel.SendStreamCmdM('QueryInfo', nil, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParam, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailed);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndQuery);
end;

procedure TDTC40_PhysicsTunnel.QueryInfoM(OnResult: TDCT40_OnQueryResultM);
var
  tmp: TDCT40_QueryResultData;
begin
  tmp := TDCT40_QueryResultData.Create;
  tmp.DTC40_PhysicsTunnel := Self;
  tmp.OnResultM := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      PhysicsTunnel.SendStreamCmdM('QueryInfo', nil, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParam, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailed);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndQuery);
end;

procedure TDTC40_PhysicsTunnel.QueryInfoP(OnResult: TDCT40_OnQueryResultP);
var
  tmp: TDCT40_QueryResultData;
begin
  tmp := TDCT40_QueryResultData.Create;
  tmp.DTC40_PhysicsTunnel := Self;
  tmp.OnResultP := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      PhysicsTunnel.SendStreamCmdM('QueryInfo', nil, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParam, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailed);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndQuery);
end;

function TDTC40_PhysicsTunnel.DependNetworkIsConnected: Boolean;
var
  i: Integer;
begin
  Result := False;
  if IsConnecting then
      exit;
  if not PhysicsTunnel.RemoteInited then
      exit;
  if not BuildNetworkIsDone then
      exit;
  for i := 0 to DependNetworkClientPool.Count - 1 do
    if not DependNetworkClientPool[i].Connected then
        exit;
  Result := True;
end;

procedure TDTC40_PhysicsTunnel.DoClientConnected(Custom_Client_: TDTC40_Custom_Client);
begin
  if Assigned(OnEvent) then
      OnEvent.DTC40_PhysicsTunnel_Client_Connected(Self, Custom_Client_);
end;

function TDTC40_PhysicsTunnelPool.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
        exit;
  Result := False;
end;

function TDTC40_PhysicsTunnelPool.GetPhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word): TDTC40_PhysicsTunnel;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
      begin
        Result := Items[i];
        exit;
      end;
end;

function TDTC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word): TDTC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(PhysicsAddr, PhysicsPort);
  if Result = nil then
      Result := TDTC40_PhysicsTunnel.Create(PhysicsAddr, PhysicsPort);
end;

function TDTC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word;
  const Depend_: TDTC40_DependNetworkInfoArray; const OnEvent_: IDTC40_PhysicsTunnel_Event): TDTC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(PhysicsAddr, PhysicsPort);
  if (Result = nil) then
    begin
      Result := TDTC40_PhysicsTunnel.Create(PhysicsAddr, PhysicsPort);
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end
  else if (not Result.IsConnecting) and (not Result.BuildNetworkIsDone) then
    begin
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end;
end;

function TDTC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word;
  const Depend_: U_String; const OnEvent_: IDTC40_PhysicsTunnel_Event): TDTC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(PhysicsAddr, PhysicsPort);
  if Result = nil then
    begin
      Result := TDTC40_PhysicsTunnel.Create(PhysicsAddr, PhysicsPort);
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end
  else if (not Result.IsConnecting) and (not Result.BuildNetworkIsDone) then
    begin
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end;
end;

function TDTC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(dispInfo: TDTC40_Info): TDTC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
  if Result = nil then
      Result := TDTC40_PhysicsTunnel.Create(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
end;

function TDTC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(dispInfo: TDTC40_Info;
  const Depend_: TDTC40_DependNetworkInfoArray; const OnEvent_: IDTC40_PhysicsTunnel_Event): TDTC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
  if Result = nil then
    begin
      Result := TDTC40_PhysicsTunnel.Create(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end
  else if (not Result.IsConnecting) and (not Result.BuildNetworkIsDone) then
    begin
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end;
end;

function TDTC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(dispInfo: TDTC40_Info;
  const Depend_: U_String; const OnEvent_: IDTC40_PhysicsTunnel_Event): TDTC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
  if Result = nil then
    begin
      Result := TDTC40_PhysicsTunnel.Create(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end
  else if (not Result.IsConnecting) and (not Result.BuildNetworkIsDone) then
    begin
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end;
end;

type
  TTemp_SearchServiceBridge = class
  public
    Pool: TDTC40_PhysicsTunnelPool;
    FullConnection_: Boolean;
    ServiceTyp: U_String;
    OnEvent_: IDTC40_PhysicsTunnel_Event;
    procedure Do_SearchService_Event(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList);
  end;

procedure TTemp_SearchServiceBridge.Do_SearchService_Event(Sender: TDTC40_PhysicsTunnel; L: TDTC40_InfoList);
var
  arry: TDTC40_Info_Array;
  i: Integer;
begin
  arry := L.SearchService(ServiceTyp);
  if FullConnection_ then
    begin
      for i := low(arry) to high(arry) do
          Pool.GetOrCreatePhysicsTunnel(arry[i], ServiceTyp, OnEvent_);
    end
  else if Length(arry) > 0 then
    begin
      Pool.GetOrCreatePhysicsTunnel(arry[0], ServiceTyp, OnEvent_);
    end;
  DelayFreeObj(1.0, Self);
end;

procedure TDTC40_PhysicsTunnelPool.SearchServiceAndBuildConnection(PhysicsAddr: U_String; PhysicsPort: Word; FullConnection_: Boolean;
  const ServiceTyp: U_String; const OnEvent_: IDTC40_PhysicsTunnel_Event);
var
  tmp: TTemp_SearchServiceBridge;
  Tunnel_: TDTC40_PhysicsTunnel;
begin
  tmp := TTemp_SearchServiceBridge.Create;
  tmp.Pool := Self;
  tmp.FullConnection_ := FullConnection_;
  tmp.ServiceTyp := ServiceTyp;
  tmp.OnEvent_ := OnEvent_;
  Tunnel_ := GetOrCreatePhysicsTunnel(PhysicsAddr, PhysicsPort);
  Tunnel_.QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.Do_SearchService_Event);
end;

procedure TDTC40_PhysicsTunnelPool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

procedure TDTC40_Info.MakeHash;
var
  n: U_String;
  buff: TBytes;
begin
  n := umlTrimSpace(PhysicsAddr) + '_' + umlIntToStr(PhysicsPort) + '_' + p2pVM_RecvTunnel_Addr + '_' + p2pVM_SendTunnel_Addr;
  n := n.LowerText;
  buff := n.Bytes;
  n := '';
  Hash := umlMD5(@buff[0], Length(buff));
  SetLength(buff, 0);
end;

constructor TDTC40_Info.Create;
begin
  inherited Create;
  Ignored := False;
  // share
  OnlyInstance := False;
  ServiceTyp := '';
  PhysicsAddr := '';
  PhysicsPort := 0;
  p2pVM_Auth := '';
  p2pVM_RecvTunnel_Addr := '';
  p2pVM_RecvTunnel_Port := 0;
  p2pVM_SendTunnel_Addr := '';
  p2pVM_SendTunnel_Port := 0;
  Workload := 0;
  MaxWorkload := 0;
  Hash := NullMD5;
end;

destructor TDTC40_Info.Destroy;
begin
  ServiceTyp := '';
  PhysicsAddr := '';
  p2pVM_Auth := '';
  p2pVM_RecvTunnel_Addr := '';
  p2pVM_SendTunnel_Addr := '';
  inherited Destroy;
end;

procedure TDTC40_Info.Assign(source: TDTC40_Info);
begin
  Ignored := source.Ignored;
  OnlyInstance := source.OnlyInstance;
  ServiceTyp := source.ServiceTyp;
  PhysicsAddr := source.PhysicsAddr;
  PhysicsPort := source.PhysicsPort;
  p2pVM_Auth := source.p2pVM_Auth;
  p2pVM_RecvTunnel_Addr := source.p2pVM_RecvTunnel_Addr;
  p2pVM_RecvTunnel_Port := source.p2pVM_RecvTunnel_Port;
  p2pVM_SendTunnel_Addr := source.p2pVM_SendTunnel_Addr;
  p2pVM_SendTunnel_Port := source.p2pVM_SendTunnel_Port;
  Workload := source.Workload;
  MaxWorkload := source.MaxWorkload;
  Hash := source.Hash;
end;

function TDTC40_Info.Clone: TDTC40_Info;
begin
  Result := TDTC40_Info.Create;
  Result.Assign(Self);
end;

procedure TDTC40_Info.Load(stream: TCoreClassStream);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.LoadFromStream(stream);

  OnlyInstance := D.R.ReadBool;
  ServiceTyp := D.R.ReadString;
  PhysicsAddr := D.R.ReadString;
  PhysicsPort := D.R.ReadWord;
  p2pVM_Auth := D.R.ReadString;
  p2pVM_RecvTunnel_Addr := D.R.ReadString;
  p2pVM_RecvTunnel_Port := D.R.ReadWord;
  p2pVM_SendTunnel_Addr := D.R.ReadString;
  p2pVM_SendTunnel_Port := D.R.ReadWord;
  Workload := D.R.ReadInteger;
  MaxWorkload := D.R.ReadInteger;
  Hash := D.R.ReadMD5;

  DisposeObject(D);
end;

procedure TDTC40_Info.Save(stream: TCoreClassStream);
var
  D: TDFE;
begin
  D := TDFE.Create;

  D.WriteBool(OnlyInstance);
  D.WriteString(ServiceTyp);
  D.WriteString(PhysicsAddr);
  D.WriteWord(PhysicsPort);
  D.WriteString(p2pVM_Auth);
  D.WriteString(p2pVM_RecvTunnel_Addr);
  D.WriteWord(p2pVM_RecvTunnel_Port);
  D.WriteString(p2pVM_SendTunnel_Addr);
  D.WriteWord(p2pVM_SendTunnel_Port);
  D.WriteInteger(Workload);
  D.WriteInteger(MaxWorkload);
  D.WriteMD5(Hash);

  D.FastEncodeTo(stream);
  DisposeObject(D);
end;

function TDTC40_Info.Same(Data_: TDTC40_Info): Boolean;
begin
  Result := False;
  if not ServiceTyp.Same(@Data_.ServiceTyp) then
      exit;
  if not PhysicsAddr.Same(@Data_.PhysicsAddr) then
      exit;
  if PhysicsPort <> Data_.PhysicsPort then
      exit;
  if not p2pVM_RecvTunnel_Addr.Same(@Data_.p2pVM_RecvTunnel_Addr) then
      exit;
  if p2pVM_RecvTunnel_Port <> Data_.p2pVM_RecvTunnel_Port then
      exit;
  if not p2pVM_SendTunnel_Addr.Same(@Data_.p2pVM_SendTunnel_Addr) then
      exit;
  if p2pVM_SendTunnel_Port <> Data_.p2pVM_SendTunnel_Port then
      exit;
  Result := True;
end;

function TDTC40_Info.SameServiceTyp(Data_: TDTC40_Info): Boolean;
begin
  Result := ServiceTyp.Same(@Data_.ServiceTyp);
end;

function TDTC40_Info.SamePhysicsAddr(PhysicsAddr_: U_String; PhysicsPort_: Word): Boolean;
begin
  Result := False;
  if not PhysicsAddr.Same(@PhysicsAddr_) then
      exit;
  if PhysicsPort <> PhysicsPort_ then
      exit;
  Result := True;
end;

function TDTC40_Info.SamePhysicsAddr(Data_: TDTC40_Info): Boolean;
begin
  Result := False;
  if not PhysicsAddr.Same(@Data_.PhysicsAddr) then
      exit;
  if PhysicsPort <> Data_.PhysicsPort then
      exit;
  Result := True;
end;

function TDTC40_Info.SamePhysicsAddr(Data_: TDTC40_PhysicsTunnel): Boolean;
begin
  Result := False;
  if not PhysicsAddr.Same(@Data_.PhysicsAddr) then
      exit;
  if PhysicsPort <> Data_.PhysicsPort then
      exit;
  Result := True;
end;

function TDTC40_Info.SamePhysicsAddr(Data_: TDTC40_PhysicsService): Boolean;
begin
  Result := False;
  if not PhysicsAddr.Same(@Data_.PhysicsAddr) then
      exit;
  if PhysicsPort <> Data_.PhysicsPort then
      exit;
  Result := True;
end;

function TDTC40_Info.SameP2PVMAddr(Data_: TDTC40_Info): Boolean;
begin
  Result := False;
  if not p2pVM_RecvTunnel_Addr.Same(@Data_.p2pVM_RecvTunnel_Addr) then
      exit;
  if p2pVM_RecvTunnel_Port <> Data_.p2pVM_RecvTunnel_Port then
      exit;
  if not p2pVM_SendTunnel_Addr.Same(@Data_.p2pVM_SendTunnel_Addr) then
      exit;
  if p2pVM_SendTunnel_Port <> Data_.p2pVM_SendTunnel_Port then
      exit;
  Result := True;
end;

function TDTC40_Info.ReadyDTC40Client: Boolean;
var
  p: PDTC40_RegistedData;
begin
  p := FindRegistedC40(ServiceTyp);
  Result := (p <> nil) and (p^.ClientClass <> nil);
end;

function TDTC40_Info.GetOrCreateDTC40Client(Param_: U_String): TDTC40_Custom_Client;
var
  p: PDTC40_RegistedData;
  i: Integer;
begin
  Result := nil;
  for i := 0 to DTC40_ClientPool.Count - 1 do
    if Same(DTC40_ClientPool[i].ClientInfo) then
      begin
        Result := DTC40_ClientPool[i];
        exit;
      end;

  p := FindRegistedC40(ServiceTyp);
  if p <> nil then
      Result := p^.ClientClass.Create(Self, Param_);
end;

constructor TDTC40_InfoList.Create(AutoFree_: Boolean);
begin
  inherited Create;
  AutoFree := AutoFree_;
end;

destructor TDTC40_InfoList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDTC40_InfoList.Remove(obj: TDTC40_Info);
begin
  if AutoFree then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TDTC40_InfoList.Delete(index: Integer);
begin
  if AutoFree then
      DisposeObject(Items[index]);
  inherited Delete(index);
end;

procedure TDTC40_InfoList.Clear;
var
  i: Integer;
begin
  if AutoFree then
    for i := 0 to Count - 1 do
        DisposeObject(Items[i]);
  inherited Clear;
end;

class procedure TDTC40_InfoList.SortWorkLoad(L: TDTC40_InfoList);
  function Compare_(Left, Right: TDTC40_Info): ShortInt;
  begin
    Result := CompareFloat(Left.Workload / Left.MaxWorkload, Right.Workload / Right.MaxWorkload);
    if Result = 0 then
        Result := CompareGeoInt(Right.MaxWorkload, Left.MaxWorkload);
  end;

  procedure fastSort_(var Arry_: TDTC40_InfoList; L, R: Integer);
  var
    i, j: Integer;
    p: TDTC40_Info;
  begin
    repeat
      i := L;
      j := R;
      p := Arry_[(L + R) shr 1];
      repeat
        while Compare_(Arry_[i], p) < 0 do
            inc(i);
        while Compare_(Arry_[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                Arry_.Exchange(i, j);
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          fastSort_(Arry_, L, j);
      L := i;
    until i >= R;
  end;

begin
  if L.Count > 1 then
      fastSort_(L, 0, L.Count - 1);
end;

function TDTC40_InfoList.GetInfoArray: TDTC40_Info_Array;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i];
end;

function TDTC40_InfoList.IsOnlyInstance(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if umlMultipleMatch(True, ServiceTyp, Items[i].ServiceTyp) and Items[i].OnlyInstance then
      begin
        Result := True;
        exit;
      end;
end;

function TDTC40_InfoList.GetServiceTypNum(ServiceTyp: U_String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if umlMultipleMatch(True, ServiceTyp, Items[i].ServiceTyp) then
        inc(Result);
end;

function TDTC40_InfoList.SearchService(ServiceTyp: U_String): TDTC40_Info_Array;
var
  L: TDTC40_InfoList;
  i: Integer;
begin
  L := TDTC40_InfoList.Create(False);
  { filter }
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ServiceTyp) then
        L.Add(Items[i]);
  { sort }
  TDTC40_InfoList.SortWorkLoad(L);
  Result := L.GetInfoArray;
  DisposeObject(L);
end;

function TDTC40_InfoList.ExistsService(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ServiceTyp) then
        exit;
  Result := False;
end;

function TDTC40_InfoList.ExistsServiceAndPhysicsTunnel(ServiceTyp: U_String; PhysicsTunnel_: TDTC40_PhysicsTunnel): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if umlMultipleMatch(True, ServiceTyp, Items[i].ServiceTyp) and (Items[i].SamePhysicsAddr(PhysicsTunnel_)) then
        exit;
  Result := False;
end;

function TDTC40_InfoList.FindSame(Data_: TDTC40_Info): TDTC40_Info;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Same(Data_) then
      begin
        Result := Items[i];
        exit;
      end;
end;

function TDTC40_InfoList.FindHash(Hash: TMD5): TDTC40_Info;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if umlCompareMD5(Hash, Items[i].Hash) then
      begin
        Result := Items[i];
        exit;
      end;
end;

function TDTC40_InfoList.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
        exit;
  Result := False;
end;

procedure TDTC40_InfoList.RemovePhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
        Delete(i)
    else
        inc(i);
end;

function TDTC40_InfoList.OverwriteInfo(Data_: TDTC40_Info): Boolean;
var
  found_: TDTC40_Info;
begin
  Result := False;
  found_ := FindSame(Data_);
  if found_ <> nil then
    begin
      if found_ <> Data_ then
          found_.Assign(Data_);
    end
  else
    begin
      if AutoFree then
        begin
          Add(Data_.Clone);
          Result := True;
        end
      else
          DoStatus('not autofree a memory leak.');
    end;
end;

function TDTC40_InfoList.MergeFromDF(D: TDFE): Boolean;
var
  i: Integer;
  m64: TMS64;
  tmp, found_: TDTC40_Info;
begin
  Result := False;
  while D.R.NotEnd do
    begin
      m64 := TMS64.Create;
      D.R.ReadStream(m64);
      m64.Position := 0;
      tmp := TDTC40_Info.Create;
      tmp.Load(m64);
      DisposeObject(m64);
      found_ := FindSame(tmp);
      if found_ <> nil then
        begin
          DisposeObject(tmp);
        end
      else
        begin
          if not AutoFree then
            begin
              DoStatus('not autofree a memory leak.');
            end
          else if (tmp.OnlyInstance) and (GetServiceTypNum(tmp.ServiceTyp) > 0) then
            begin
              DoStatus('"%s" is only instance.', [tmp.ServiceTyp.Text]);
            end
          else
            begin
              Add(tmp);
              Result := True;
            end;
        end;
    end;
end;

procedure TDTC40_InfoList.SaveToDF(D: TDFE);
var
  i: Integer;
  m64: TMS64;
begin
  m64 := TMS64.Create;
  for i := 0 to Count - 1 do
    if not Items[i].Ignored then
      begin
        Items[i].Save(m64);
        D.WriteStream(m64);
        m64.Clear;
      end;
  DisposeObject(m64);
end;

constructor TDTC40_Custom_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_: U_String;
  P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: U_String;
  tmp: TPascalStringList;
begin
  inherited Create;

  Param := Param_;
  DTC40PhysicsService := PhysicsService_;

  ParamList := THashStringList.Create;
  try
    tmp := TPascalStringList.Create;
    umlSeparatorText(Param, tmp, ',;' + #13#10);
    ParamList.ImportFromStrings(tmp);
    DisposeObject(tmp);
  except
  end;

  FLastSafeCheckTime := GetTimeTick;
  SafeCheckTime := EStrToInt64(ParamList.GetDefaultValue('SafeCheckTime', umlIntToStr(DTC40_SafeCheckTime)), DTC40_SafeCheckTime);

  P2PVM_Recv_Name_ := ServiceTyp + 'R';
  DTC40_ServicePool.MakeP2PVM_IPv6_Port(P2PVM_Recv_IP6_, P2PVM_Recv_Port_);
  P2PVM_Send_Name_ := ServiceTyp + 'S';
  DTC40_ServicePool.MakeP2PVM_IPv6_Port(P2PVM_Send_IP6_, P2PVM_Send_Port_);

  ServiceInfo := TDTC40_Info.Create;
  ServiceInfo.Ignored := EStrToBool(ParamList.GetDefaultValue('Ignored', if_(ServiceInfo.Ignored, 'True', 'False')), ServiceInfo.Ignored);
  ServiceInfo.OnlyInstance := EStrToBool(ParamList.GetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False')), ServiceInfo.OnlyInstance);
  ServiceInfo.ServiceTyp := ServiceTyp;
  ServiceInfo.PhysicsAddr := DTC40PhysicsService.PhysicsAddr;
  ServiceInfo.PhysicsPort := DTC40PhysicsService.PhysicsPort;
  ServiceInfo.p2pVM_Auth := DTC40PhysicsService.PhysicsTunnel.AutomatedP2PVMAuthToken;
  ServiceInfo.p2pVM_RecvTunnel_Addr := P2PVM_Recv_IP6_;
  ServiceInfo.p2pVM_RecvTunnel_Port := umlStrToInt(P2PVM_Recv_Port_);
  ServiceInfo.p2pVM_SendTunnel_Addr := P2PVM_Send_IP6_;
  ServiceInfo.p2pVM_SendTunnel_Port := umlStrToInt(P2PVM_Send_Port_);
  SetWorkload(0, 100);
  ServiceInfo.MakeHash;

  DTC40_ServicePool.Add(Self);
  DTC40PhysicsService.DependNetworkServicePool.Add(Self);
end;

destructor TDTC40_Custom_Service.Destroy;
begin
  DTC40PhysicsService.DependNetworkServicePool.Remove(Self);
  DTC40_ServicePool.Remove(Self);
  DisposeObject(ServiceInfo);
  DisposeObject(ParamList);
  inherited Destroy;
end;

procedure TDTC40_Custom_Service.SafeCheck;
begin

end;

procedure TDTC40_Custom_Service.Progress;
begin
  if GetTimeTick - FLastSafeCheckTime > SafeCheckTime then
    begin
      SafeCheck;
      FLastSafeCheckTime := GetTimeTick;
    end;
end;

procedure TDTC40_Custom_Service.SetWorkload(Workload_, MaxWorkload_: Integer);
begin
  ServiceInfo.Workload := MaxWorkload_;
  ServiceInfo.MaxWorkload := MaxWorkload_;
end;

procedure TDTC40_Custom_Service.UpdateToGlobalDispatch;
var
  i: Integer;
  dps: TDTC40_Dispatch_Service;
  dpc: TDTC40_Dispatch_Client;
begin
  for i := 0 to DTC40_ServicePool.Count - 1 do
    begin
      if DTC40_ServicePool[i] is TDTC40_Dispatch_Service then
        begin
          dps := TDTC40_Dispatch_Service(DTC40_ServicePool[i]);
          if dps.ServiceInfoList.OverwriteInfo(ServiceInfo) then
              dps.Prepare_UpdateServerInfoToAllClient;
        end;
    end;
  for i := 0 to DTC40_ClientPool.Count - 1 do
    begin
      if DTC40_ClientPool[i] is TDTC40_Dispatch_Client then
        begin
          dpc := TDTC40_Dispatch_Client(DTC40_ClientPool[i]);
          if dpc.ServiceInfoList.OverwriteInfo(ServiceInfo) and dpc.Connected then
              dpc.PostLocalServiceInfo(True);
        end;
    end;
end;

function TDTC40_Custom_Service.GetHash: TMD5;
begin
  Result := ServiceInfo.Hash;
end;

procedure TDTC40_Custom_Service.DoLinkSuccess(Trigger_: TCoreClassObject);
begin
  DTC40PhysicsService.DoLinkSuccess(Self, Trigger_);
end;

procedure TDTC40_Custom_Service.DoUserOut(Trigger_: TCoreClassObject);
begin
  DTC40PhysicsService.DoUserOut(Self, Trigger_);
end;

constructor TDTC40_Custom_ServicePool.Create;
begin
  inherited Create;
  FIPV6_Seed := 1;
end;

procedure TDTC40_Custom_ServicePool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

procedure TDTC40_Custom_ServicePool.MakeP2PVM_IPv6_Port(var ip6, port: U_String);
var
  tmp: TIPV6;
  i: Integer;
begin
  for i := 0 to 7 do
      tmp[i] := FIPV6_Seed;
  inc(FIPV6_Seed);
  port := '1';
  ip6 := IPV6ToStr(tmp);
end;

function TDTC40_Custom_ServicePool.GetServiceFromHash(Hash: TMD5): TDTC40_Custom_Service;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if umlCompareMD5(Hash, Items[i].ServiceInfo.Hash) then
        Result := Items[i];
end;

function TDTC40_Custom_ServicePool.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].ServiceInfo.PhysicsAddr) and (PhysicsPort = Items[i].ServiceInfo.PhysicsPort) then
        exit;
  Result := False;
end;

function TDTC40_Custom_ServicePool.ExistsOnlyInstance(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].ServiceInfo.OnlyInstance and ServiceTyp.Same(@Items[i].ServiceInfo.ServiceTyp) then
        exit;
  Result := False;
end;

function TDTC40_Custom_ServicePool.GetDTC40Array: TDTC40_Custom_Service_Array;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i];
end;

function TDTC40_Custom_ServicePool.GetFromServiceTyp(ServiceTyp: U_String): TDTC40_Custom_Service_Array;
var
  L: TDTC40_Custom_ServicePool;
  i: Integer;
begin
  L := TDTC40_Custom_ServicePool.Create;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ServiceInfo.ServiceTyp) then
        L.Add(Items[i]);
  Result := L.GetDTC40Array;
  DisposeObject(L);
end;

function TDTC40_Custom_ServicePool.GetFromPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): TDTC40_Custom_Service_Array;
var
  L: TDTC40_Custom_ServicePool;
  i: Integer;
begin
  L := TDTC40_Custom_ServicePool.Create;
  for i := 0 to Count - 1 do
    if (PhysicsPort = Items[i].ServiceInfo.PhysicsPort) and PhysicsAddr.Same(@Items[i].ServiceInfo.PhysicsAddr) then
        L.Add(Items[i]);
  Result := L.GetDTC40Array;
  DisposeObject(L);
end;

function TDTC40_Custom_ServicePool.GetFromClass(Class_: TDTC40_Custom_Service_Class): TDTC40_Custom_Service_Array;
var
  L: TDTC40_Custom_ServicePool;
  i: Integer;
begin
  L := TDTC40_Custom_ServicePool.Create;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) then
        L.Add(Items[i]);
  Result := L.GetDTC40Array;
  DisposeObject(L);
end;

procedure TDTC40_Custom_Client.DoNetworkOffline;
begin

end;

constructor TDTC40_Custom_Client.Create(source_: TDTC40_Info; Param_: U_String);
var
  tmp: TPascalStringList;
begin
  inherited Create;
  Param := Param_;
  ClientInfo := TDTC40_Info.Create;
  ClientInfo.Assign(source_);

  ParamList := THashStringList.Create;
  try
    tmp := TPascalStringList.Create;
    umlSeparatorText(Param, tmp, ',;' + #13#10);
    ParamList.ImportFromStrings(tmp);
    DisposeObject(tmp);
  except
  end;

  DTC40_ClientPool.Add(Self);
  DTC40PhysicsTunnel := DTC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(ClientInfo);
  DTC40PhysicsTunnel.DependNetworkClientPool.Add(Self);
end;

destructor TDTC40_Custom_Client.Destroy;
begin
  DTC40_ClientPool.Remove(Self);
  DTC40PhysicsTunnel.DependNetworkClientPool.Remove(Self);
  DisposeObject(ClientInfo);
  DisposeObject(ParamList);
  inherited Destroy;
end;

procedure TDTC40_Custom_Client.Progress;
begin

end;

procedure TDTC40_Custom_Client.Connect;
begin

end;

function TDTC40_Custom_Client.Connected: Boolean;
begin
  Result := False;
end;

procedure TDTC40_Custom_Client.Disconnect;
begin

end;

function TDTC40_Custom_Client.GetHash: TMD5;
begin
  Result := ClientInfo.Hash;
end;

procedure TDTC40_Custom_Client.DoClientConnected;
begin
  DTC40PhysicsTunnel.DoClientConnected(Self);
end;

procedure TDTC40_Custom_ClientPool_Wait.DoRun;
  function ExistsClientFromStatesDone(c_: TDTC40_Custom_Client): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to Length(States_) - 1 do
      if States_[i].Client_ = c_ then
          exit;
    Result := False;
  end;

  function MatchServiceTypForPool(var d_: TDTC40_Custom_ClientPool_Wait_Data): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to Pool_.Count - 1 do
      begin
        if Pool_[i].Connected and d_.ServiceTyp_.Same(@Pool_[i].ClientInfo.ServiceTyp) and (not ExistsClientFromStatesDone(Pool_[i])) then
          begin
            d_.Client_ := Pool_[i];
            exit;
          end;
      end;
    Result := False;
  end;

  function IsAllDone: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to Length(States_) - 1 do
      if States_[i].Client_ = nil then
          exit;
    Result := True;
  end;

var
  i: Integer;
begin
  for i := 0 to Length(States_) - 1 do
      MatchServiceTypForPool(States_[i]);

  if IsAllDone then
    begin
      try
        if Assigned(OnCall) then
            OnCall(States_);
        if Assigned(OnMethod) then
            OnMethod(States_);
        if Assigned(OnProc) then
            OnProc(States_);
      except
      end;
      DelayFreeObject(0.5, Self, nil);
    end
  else
      SystemPostProgress.PostExecuteM_NP(0.1, {$IFDEF FPC}@{$ENDIF FPC}DoRun);
end;

constructor TDTC40_Custom_ClientPool_Wait.Create(dependNetwork_: U_String);
var
  arry: TArrayPascalString;
  i: Integer;
begin
  inherited Create;
  umlGetSplitArray(dependNetwork_, arry, '|<>');
  SetLength(States_, Length(arry));
  for i := 0 to Length(arry) - 1 do
    begin
      States_[i].ServiceTyp_ := arry[i];
      States_[i].Client_ := nil;
    end;

  Pool_ := nil;
  OnCall := nil;
  OnMethod := nil;
  OnProc := nil;
end;

destructor TDTC40_Custom_ClientPool_Wait.Destroy;
begin
  SetLength(States_, 0);
  inherited Destroy;
end;

procedure TDTC40_Custom_ClientPool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

function TDTC40_Custom_ClientPool.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].ClientInfo.PhysicsAddr) and (PhysicsPort = Items[i].ClientInfo.PhysicsPort) then
        exit;
  Result := False;
end;

function TDTC40_Custom_ClientPool.ExistsServiceInfo(info_: TDTC40_Info): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if info_.Same(Items[i].ClientInfo) then
        exit;
  Result := False;
end;

function TDTC40_Custom_ClientPool.ExistsServiceTyp(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) then
        exit;
  Result := False;
end;

function TDTC40_Custom_ClientPool.ExistsClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) then
        exit(Items[i]);
end;

function TDTC40_Custom_ClientPool.ExistsConnectedClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) and Items[i].Connected then
        exit(Items[i]);
end;

function TDTC40_Custom_ClientPool.ExistsConnectedServiceTyp(ServiceTyp: U_String): TDTC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) and Items[i].Connected then
        exit(Items[i]);
end;

function TDTC40_Custom_ClientPool.FindPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].ClientInfo.PhysicsAddr) and (PhysicsPort = Items[i].ClientInfo.PhysicsPort) then
        exit;
  Result := False;
end;

function TDTC40_Custom_ClientPool.FindServiceInfo(info_: TDTC40_Info): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if info_.Same(Items[i].ClientInfo) then
        exit;
  Result := False;
end;

function TDTC40_Custom_ClientPool.FindServiceTyp(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) then
        exit;
  Result := False;
end;

function TDTC40_Custom_ClientPool.FindClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) then
        exit(Items[i]);
end;

function TDTC40_Custom_ClientPool.FindConnectedClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) and Items[i].Connected then
        exit(Items[i]);
end;

function TDTC40_Custom_ClientPool.FindConnectedServiceTyp(ServiceTyp: U_String): TDTC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) and Items[i].Connected then
        exit(Items[i]);
end;

function TDTC40_Custom_ClientPool.GetClientFromHash(Hash: TMD5): TDTC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if umlCompareMD5(Hash, Items[i].ClientInfo.Hash) then
        Result := Items[i];
end;

function TDTC40_Custom_ClientPool.GetDTC40Array: TDTC40_Custom_Client_Array;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i];
end;

function TDTC40_Custom_ClientPool.GetFromServiceTyp(ServiceTyp: U_String): TDTC40_Custom_Client_Array;
var
  L: TDTC40_Custom_ClientPool;
  i: Integer;
begin
  L := TDTC40_Custom_ClientPool.Create;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) then
        L.Add(Items[i]);
  Result := L.GetDTC40Array;
  DisposeObject(L);
end;

function TDTC40_Custom_ClientPool.GetFromPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): TDTC40_Custom_Client_Array;
var
  L: TDTC40_Custom_ClientPool;
  i: Integer;
begin
  L := TDTC40_Custom_ClientPool.Create;
  for i := 0 to Count - 1 do
    if (PhysicsPort = Items[i].ClientInfo.PhysicsPort) and PhysicsAddr.Same(@Items[i].ClientInfo.PhysicsAddr) then
        L.Add(Items[i]);
  Result := L.GetDTC40Array;
  DisposeObject(L);
end;

function TDTC40_Custom_ClientPool.GetFromClass(Class_: TDTC40_Custom_Client_Class): TDTC40_Custom_Client_Array;
var
  L: TDTC40_Custom_ClientPool;
  i: Integer;
begin
  L := TDTC40_Custom_ClientPool.Create;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) then
        L.Add(Items[i]);
  Result := L.GetDTC40Array;
  DisposeObject(L);
end;

procedure TDTC40_Custom_ClientPool.WaitConnectedDoneC(dependNetwork_: U_String; OnResult: TOn_DTC40_Custom_Client_EventC);
var
  tmp: TDTC40_Custom_ClientPool_Wait;
begin
  tmp := TDTC40_Custom_ClientPool_Wait.Create(dependNetwork_);
  tmp.Pool_ := Self;
  tmp.OnCall := OnResult;
  SystemPostProgress.PostExecuteM_NP(0.1, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);
end;

procedure TDTC40_Custom_ClientPool.WaitConnectedDoneM(dependNetwork_: U_String; OnResult: TOn_DTC40_Custom_Client_EventM);
var
  tmp: TDTC40_Custom_ClientPool_Wait;
begin
  tmp := TDTC40_Custom_ClientPool_Wait.Create(dependNetwork_);
  tmp.Pool_ := Self;
  tmp.OnMethod := OnResult;
  SystemPostProgress.PostExecuteM_NP(0.1, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);
end;

procedure TDTC40_Custom_ClientPool.WaitConnectedDoneP(dependNetwork_: U_String; OnResult: TOn_DTC40_Custom_Client_EventP);
var
  tmp: TDTC40_Custom_ClientPool_Wait;
begin
  tmp := TDTC40_Custom_ClientPool_Wait.Create(dependNetwork_);
  tmp.Pool_ := Self;
  tmp.OnProc := OnResult;
  SystemPostProgress.PostExecuteM_NP(0.1, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);
end;

constructor TOnRemovePhysicsNetwork.Create;
begin
  PhysicsAddr := '';
  PhysicsPort := 0;
end;

procedure TOnRemovePhysicsNetwork.DoRun;
begin
  C40RemovePhysics(PhysicsAddr, PhysicsPort, True, True, True, True);
  DelayFreeObject(1.0, Self);
end;

procedure TDTC40_Dispatch_Service.cmd_UpdateServiceInfo(Sender: TPeerIO; InData: TDFE);
begin
  if ServiceInfoList.MergeFromDF(InData) then
    begin
      Prepare_UpdateServerInfoToAllClient;

      if Assigned(FOnServiceInfoChange) then
          FOnServiceInfoChange(Self, ServiceInfoList);
    end;
end;

procedure TDTC40_Dispatch_Service.cmd_UpdateServiceState(Sender: TPeerIO; InData: TDFE);
var
  D: TDFE;
  Hash__: TMD5;
  Workload, MaxWorkload: Integer;
  info_: TDTC40_Info;
  i: Integer;
begin
  D := TDFE.Create;
  while InData.R.NotEnd do
    begin
      InData.R.ReadDataFrame(D);
      Hash__ := D.R.ReadMD5;
      Workload := D.R.ReadInteger;
      MaxWorkload := D.R.ReadInteger;
      info_ := ServiceInfoList.FindHash(Hash__);
      if (info_ <> nil) then
        begin
          info_.Workload := Workload;
          info_.MaxWorkload := MaxWorkload;
        end;
    end;
  DisposeObject(D);

  for i := 0 to DTC40_ServicePool.Count - 1 do
    begin
      info_ := ServiceInfoList.FindSame(DTC40_ServicePool[i].ServiceInfo);
      if info_ <> nil then
          info_.Assign(DTC40_ServicePool[i].ServiceInfo);
    end;
end;

procedure TDTC40_Dispatch_Service.cmd_IgnoreChange(Sender: TPeerIO; InData: TDFE);
var
  Hash__: TMD5;
  Ignored: Boolean;
  info_: TDTC40_Info;
begin
  Hash__ := InData.R.ReadMD5;
  Ignored := InData.R.ReadBool;
  info_ := ServiceInfoList.FindHash(Hash__);
  if (info_ <> nil) and (info_.Ignored <> Ignored) then
    begin
      info_.Ignored := Ignored;
      IgnoreChangeToAllClient(info_.Hash, info_.Ignored);
    end;
end;

procedure TDTC40_Dispatch_Service.cmd_RequestUpdate(Sender: TPeerIO; InData: TDFE);
begin
  Prepare_UpdateServerInfoToAllClient;
end;

procedure TDTC40_Dispatch_Service.cmd_RemovePhysicsNetwork(Sender: TPeerIO; InData: TDFE);
var
  tmp: TOnRemovePhysicsNetwork;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  IODef_: TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  tmp := TOnRemovePhysicsNetwork.Create;
  tmp.PhysicsAddr := InData.R.ReadString;
  tmp.PhysicsPort := InData.R.ReadWord;
  SysPost.PostExecuteM_NP(2.0, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);

  if C40ExistsPhysicsNetwork(tmp.PhysicsAddr, tmp.PhysicsPort) then
    begin
      Service.RecvTunnel.GetIO_Array(Arry_);
      for ID_ in Arry_ do
        begin
          IO_ := Service.RecvTunnel[ID_];
          if (IO_ <> nil) and (IO_ <> Sender) and TPeerClientUserDefineForRecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
            begin
              IODef_ := TPeerClientUserDefineForRecvTunnel_NoAuth(IO_.UserDefine);
              IODef_.SendTunnel.Owner.SendDirectStreamCmd('RemovePhysicsNetwork', InData);
            end;
        end;
    end;
end;

procedure TDTC40_Dispatch_Service.Prepare_UpdateServerInfoToAllClient;
begin
  FWaiting_UpdateServerInfoToAllClient := True;
  FWaiting_UpdateServerInfoToAllClient_TimeTick := GetTimeTick + DTC40_UpdateServiceInfoDelayTime;
end;

procedure TDTC40_Dispatch_Service.UpdateServerInfoToAllClient;
var
  D: TDFE;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
begin
  D := TDFE.Create;
  ServiceInfoList.SaveToDF(D);
  Service.SendTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := Service.SendTunnel[ID_];
      if (IO_ <> nil) and TPeerClientUserDefineForSendTunnel_NoAuth(IO_.UserDefine).LinkOk then
          IO_.SendDirectStreamCmd('UpdateServiceInfo', D);
    end;
  DisposeObject(D);
end;

procedure TDTC40_Dispatch_Service.DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TDTC40_Dispatch_Service.DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoUserOut(UserDefineIO);
end;

procedure TDTC40_Dispatch_Service.DoDelayCheckLocalServiceInfo;
var
  i: Integer;
  isChange_: Boolean;
  info_: TDTC40_Info;
begin
  DelayCheck_Working := False;
  isChange_ := False;
  for i := 0 to DTC40_ServicePool.Count - 1 do
    begin
      info_ := ServiceInfoList.FindSame(DTC40_ServicePool[i].ServiceInfo);
      if info_ = nil then
        begin
          ServiceInfoList.Add(DTC40_ServicePool[i].ServiceInfo.Clone);
          isChange_ := True;
        end
      else
          info_.Assign(DTC40_ServicePool[i].ServiceInfo);
    end;
  if isChange_ then
    begin
      Prepare_UpdateServerInfoToAllClient;
    end
  else
    begin
      UpdateServiceStateToAllClient;
    end;
end;

constructor TDTC40_Dispatch_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  i: Integer;
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  FOnServiceInfoChange := nil;
  FWaiting_UpdateServerInfoToAllClient := False;
  FWaiting_UpdateServerInfoToAllClient_TimeTick := 0;
  DelayCheck_Working := False;

  { custom p2pVM service }
  Service := TDT_P2PVM_NoAuth_Custom_Service.Create(TDTService_NoAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;

  Service.DTService.PublicFileDirectory := umlCombinePath(DTC40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);

  Service.RecvTunnel.RegisterDirectStream('UpdateServiceInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_UpdateServiceInfo;
  Service.RecvTunnel.RegisterDirectStream('UpdateServiceState').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_UpdateServiceState;
  Service.RecvTunnel.RegisterDirectStream('IgnoreChange').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_IgnoreChange;
  Service.RecvTunnel.RegisterDirectStream('RequestUpdate').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RequestUpdate;
  Service.RecvTunnel.RegisterDirectStream('RemovePhysicsNetwork').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemovePhysicsNetwork;

  Service.RecvTunnel.PrintParams['UpdateServiceInfo'] := False;
  Service.RecvTunnel.PrintParams['UpdateServiceState'] := False;
  Service.RecvTunnel.PrintParams['IgnoreChange'] := False;
  Service.RecvTunnel.PrintParams['RequestUpdate'] := False;

  Service.SendTunnel.PrintParams['UpdateServiceInfo'] := False;
  Service.SendTunnel.PrintParams['UpdateServiceState'] := False;
  Service.SendTunnel.PrintParams['IgnoreChange'] := False;
  Service.SendTunnel.PrintParams['RequestUpdate'] := False;

  { register local service. }
  ServiceInfoList := TDTC40_InfoList.Create(True);
  for i := 0 to DTC40_ServicePool.Count - 1 do
    if ServiceInfoList.FindSame(DTC40_ServicePool[i].ServiceInfo) = nil then
        ServiceInfoList.Add(DTC40_ServicePool[i].ServiceInfo.Clone);

  UpdateToGlobalDispatch;
end;

destructor TDTC40_Dispatch_Service.Destroy;
begin
  DisposeObject(Service);
  DisposeObject(ServiceInfoList);
  inherited Destroy;
end;

procedure TDTC40_Dispatch_Service.Progress;
begin
  inherited Progress;
  Service.Progress;

  if FWaiting_UpdateServerInfoToAllClient and (GetTimeTick > FWaiting_UpdateServerInfoToAllClient_TimeTick) then
    begin
      FWaiting_UpdateServerInfoToAllClient := False;
      FWaiting_UpdateServerInfoToAllClient_TimeTick := 0;
      UpdateServerInfoToAllClient;
    end;
  ServiceInfo.Workload := Service.DTService.TotalLinkCount * 2;

  if not DelayCheck_Working then
    begin
      DelayCheck_Working := True;
      DTC40PhysicsService.PhysicsTunnel.PostProgress.PostExecuteM_NP(2.0, {$IFDEF FPC}@{$ENDIF FPC}DoDelayCheckLocalServiceInfo);
    end;
end;

procedure TDTC40_Dispatch_Service.IgnoreChangeToAllClient(Hash__: TMD5; Ignored: Boolean);
var
  D: TDFE;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
begin
  D := TDFE.Create;
  D.WriteMD5(Hash__);
  D.WriteBool(Ignored);
  Service.SendTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := Service.SendTunnel[ID_];
      if (IO_ <> nil) and TPeerClientUserDefineForSendTunnel_NoAuth(IO_.UserDefine).LinkOk then
          IO_.SendDirectStreamCmd('IgnoreChange', D);
    end;
  DisposeObject(D);
end;

procedure TDTC40_Dispatch_Service.UpdateServiceStateToAllClient;
var
  i: Integer;
  D, tmp: TDFE;
  info_: TDTC40_Info;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
begin
  D := TDFE.Create;
  for i := 0 to DTC40_ServicePool.Count - 1 do
    begin
      info_ := DTC40_ServicePool[i].ServiceInfo;
      tmp := TDFE.Create;
      tmp.WriteMD5(info_.Hash);
      tmp.WriteInteger(info_.Workload);
      tmp.WriteInteger(info_.MaxWorkload);
      D.WriteDataFrame(tmp);
      DisposeObject(tmp);
    end;

  Service.SendTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := Service.SendTunnel[ID_];
      if (IO_ <> nil) and TPeerClientUserDefineForSendTunnel_NoAuth(IO_.UserDefine).LinkOk then
          IO_.SendDirectStreamCmd('UpdateServiceState', D);
    end;
  DisposeObject(D);
end;

procedure TDTC40_Dispatch_Client.cmd_UpdateServiceInfo(Sender: TPeerIO; InData: TDFE);
var
  i: Integer;
  Arry_: TDTC40_Custom_Client_Array;
  cc: TDTC40_Custom_Client;
begin
  if ServiceInfoList.MergeFromDF(InData) then
    begin
      if Assigned(FOnServiceInfoChange) then
          FOnServiceInfoChange(Self, ServiceInfoList);

      { broadcast to all service }
      Arry_ := DTC40_ClientPool.GetFromClass(TDTC40_Dispatch_Client);
      for cc in Arry_ do
        if (cc <> Self) and (cc.Connected) then
            TDTC40_Dispatch_Client(cc).Client.SendTunnel.SendDirectStreamCmd('UpdateServiceInfo', InData);
    end;
end;

procedure TDTC40_Dispatch_Client.cmd_UpdateServiceState(Sender: TPeerIO; InData: TDFE);
var
  D: TDFE;
  Hash__: TMD5;
  Workload, MaxWorkload: Integer;
  info_: TDTC40_Info;
  i: Integer;
begin
  D := TDFE.Create;
  while InData.R.NotEnd do
    begin
      InData.R.ReadDataFrame(D);
      Hash__ := D.R.ReadMD5;
      Workload := D.R.ReadInteger;
      MaxWorkload := D.R.ReadInteger;
      info_ := ServiceInfoList.FindHash(Hash__);
      if (info_ <> nil) then
        begin
          info_.Workload := Workload;
          info_.MaxWorkload := MaxWorkload;
        end;
    end;
  DisposeObject(D);

  for i := 0 to DTC40_ServicePool.Count - 1 do
    begin
      info_ := ServiceInfoList.FindSame(DTC40_ServicePool[i].ServiceInfo);
      if info_ <> nil then
          info_.Assign(DTC40_ServicePool[i].ServiceInfo);
    end;
end;

procedure TDTC40_Dispatch_Client.cmd_IgnoreChange(Sender: TPeerIO; InData: TDFE);
var
  Hash__: TMD5;
  Ignored: Boolean;
  info_: TDTC40_Info;
  Arry_: TDTC40_Custom_Client_Array;
  cc: TDTC40_Custom_Client;
begin
  Hash__ := InData.R.ReadMD5;
  Ignored := InData.R.ReadBool;
  info_ := ServiceInfoList.FindHash(Hash__);
  if (info_ <> nil) then
    begin
      info_.Ignored := Ignored;
    end;

  { broadcast to all service }
  Arry_ := DTC40_ClientPool.GetFromClass(TDTC40_Dispatch_Client);
  for cc in Arry_ do
    if (cc <> Self) and (cc.Connected) then
        TDTC40_Dispatch_Client(cc).Client.SendTunnel.SendDirectStreamCmd('IgnoreChange', InData);
end;

procedure TDTC40_Dispatch_Client.cmd_RemovePhysicsNetwork(Sender: TPeerIO; InData: TDFE);
var
  tmp: TOnRemovePhysicsNetwork;
  Arry_: TDTC40_Custom_Client_Array;
  cc: TDTC40_Custom_Client;
begin
  tmp := TOnRemovePhysicsNetwork.Create;
  tmp.PhysicsAddr := InData.R.ReadString;
  tmp.PhysicsPort := InData.R.ReadWord;
  SysPost.PostExecuteM_NP(2.0, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);

  if C40ExistsPhysicsNetwork(tmp.PhysicsAddr, tmp.PhysicsPort) then
    begin
      { broadcast to all service }
      Arry_ := DTC40_ClientPool.GetFromClass(TDTC40_Dispatch_Client);
      for cc in Arry_ do
        if (cc <> Self) and (cc.Connected) then
            TDTC40_Dispatch_Client(cc).Client.SendTunnel.SendDirectStreamCmd('RemovePhysicsNetwork', InData);
    end;
end;

procedure TDTC40_Dispatch_Client.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  PostLocalServiceInfo(True);
  RequestUpdate();
  DoClientConnected();
end;

procedure TDTC40_Dispatch_Client.DoDelayCheckLocalServiceInfo;
var
  i: Integer;
begin
  DelayCheck_Working := False;
  PostLocalServiceInfo(False);
  UpdateLocalServiceState;

  { check and build network }
  for i := 0 to ServiceInfoList.Count - 1 do
      DTC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(ServiceInfoList[i], DTC40PhysicsTunnel.DependNetworkInfoArray, DTC40PhysicsTunnel.OnEvent);
end;

constructor TDTC40_Dispatch_Client.Create(source_: TDTC40_Info; Param_: U_String);
var
  i: Integer;
begin
  inherited Create(source_, Param_);
  FOnServiceInfoChange := nil;
  DelayCheck_Working := False;

  { custom p2pVM client }
  Client := TDT_P2PVM_NoAuth_Custom_Client.Create(
    TDTClient_NoAuth, DTC40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink;

  Client.RecvTunnel.RegisterDirectStream('UpdateServiceInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_UpdateServiceInfo;
  Client.RecvTunnel.RegisterDirectStream('UpdateServiceState').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_UpdateServiceState;
  Client.RecvTunnel.RegisterDirectStream('IgnoreChange').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_IgnoreChange;
  Client.RecvTunnel.RegisterDirectStream('RemovePhysicsNetwork').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemovePhysicsNetwork;

  Client.RecvTunnel.PrintParams['UpdateServiceInfo'] := False;
  Client.RecvTunnel.PrintParams['UpdateServiceState'] := False;
  Client.RecvTunnel.PrintParams['IgnoreChange'] := False;
  Client.RecvTunnel.PrintParams['RequestUpdate'] := False;

  Client.SendTunnel.PrintParams['UpdateServiceInfo'] := False;
  Client.SendTunnel.PrintParams['UpdateServiceState'] := False;
  Client.SendTunnel.PrintParams['IgnoreChange'] := False;
  Client.SendTunnel.PrintParams['RequestUpdate'] := False;

  { register local service. }
  ServiceInfoList := TDTC40_InfoList.Create(True);
  for i := 0 to DTC40_ServicePool.Count - 1 do
    if ServiceInfoList.FindSame(DTC40_ServicePool[i].ServiceInfo) = nil then
        ServiceInfoList.Add(DTC40_ServicePool[i].ServiceInfo.Clone);

  { check and build network }
  for i := 0 to ServiceInfoList.Count - 1 do
      DTC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(ServiceInfoList[i], DTC40PhysicsTunnel.DependNetworkInfoArray, DTC40PhysicsTunnel.OnEvent);
end;

destructor TDTC40_Dispatch_Client.Destroy;
begin
  DisposeObject(Client);
  DisposeObject(ServiceInfoList);
  inherited Destroy;
end;

procedure TDTC40_Dispatch_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
  if not DelayCheck_Working then
    begin
      DelayCheck_Working := True;
      DTC40PhysicsTunnel.PhysicsTunnel.PostProgress.PostExecuteM_NP(2.0, {$IFDEF FPC}@{$ENDIF FPC}DoDelayCheckLocalServiceInfo);
    end;
end;

procedure TDTC40_Dispatch_Client.Connect;
begin
  inherited Connect;
  Client.Connect();
end;

function TDTC40_Dispatch_Client.Connected: Boolean;
begin
  Result := Client.DTClient.LinkOk;
end;

procedure TDTC40_Dispatch_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

procedure TDTC40_Dispatch_Client.PostLocalServiceInfo(forcePost_: Boolean);
var
  i: Integer;
  isChange_: Boolean;
  info: TDTC40_Info;
  D: TDFE;
begin
  isChange_ := False;
  for i := 0 to DTC40_ServicePool.Count - 1 do
    begin
      info := ServiceInfoList.FindSame(DTC40_ServicePool[i].ServiceInfo);
      if info = nil then
        begin
          ServiceInfoList.Add(DTC40_ServicePool[i].ServiceInfo.Clone);
          isChange_ := True;
        end
      else
          info.Assign(DTC40_ServicePool[i].ServiceInfo);
    end;

  if isChange_ or forcePost_ then
    begin
      D := TDFE.Create;
      ServiceInfoList.SaveToDF(D);
      Client.SendTunnel.SendDirectStreamCmd('UpdateServiceInfo', D);
      DisposeObject(D);
    end;
end;

procedure TDTC40_Dispatch_Client.RequestUpdate;
begin
  Client.SendTunnel.SendDirectStreamCmd('RequestUpdate');
end;

procedure TDTC40_Dispatch_Client.IgnoreChangeToService(Hash__: TMD5; Ignored: Boolean);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteMD5(Hash__);
  D.WriteBool(Ignored);
  Client.SendTunnel.SendDirectStreamCmd('IgnoreChange', D);
  DisposeObject(D);
end;

procedure TDTC40_Dispatch_Client.UpdateLocalServiceState;
var
  i: Integer;
  D, tmp: TDFE;
  info_: TDTC40_Info;
begin
  D := TDFE.Create;
  for i := 0 to DTC40_ServicePool.Count - 1 do
    begin
      info_ := DTC40_ServicePool[i].ServiceInfo;
      tmp := TDFE.Create;
      tmp.WriteMD5(info_.Hash);
      tmp.WriteInteger(info_.Workload);
      tmp.WriteInteger(info_.MaxWorkload);
      D.WriteDataFrame(tmp);
      DisposeObject(tmp);
    end;
  Client.SendTunnel.SendDirectStreamCmd('UpdateServiceState', D);
  DisposeObject(D);
end;

procedure TDTC40_Dispatch_Client.RemovePhysicsNetwork(PhysicsAddr: U_String; PhysicsPort: Word);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(PhysicsAddr);
  D.WriteWord(PhysicsPort);
  Client.SendTunnel.SendDirectStreamCmd('RemovePhysicsNetwork', D);
  DisposeObject(D);
end;

destructor TDTC40_RegistedDataList.Destroy;
begin
  Clean;
  inherited Destroy;
end;

procedure TDTC40_RegistedDataList.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      Items[i]^.ServiceTyp := '';
      Dispose(Items[i]);
    end;
  inherited Clear;
end;

procedure TDTC40_RegistedDataList.Print;
var
  i: Integer;
  p: PDTC40_RegistedData;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      DoStatus('Type "%s" Service "%s" Client "%s"', [p^.ServiceTyp.Text, p^.ServiceClass.ClassName, p^.ClientClass.ClassName]);
    end;
end;

procedure TDTC40_Base_NoAuth_Service.DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TDTC40_Base_NoAuth_Service.DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoUserOut(UserDefineIO);
end;

constructor TDTC40_Base_NoAuth_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_NoAuth_Custom_Service.Create(TDTService_NoAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.PublicFileDirectory := umlCombinePath(DTC40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTNoAuthService := Service.DTService;
  UpdateToGlobalDispatch;
end;

destructor TDTC40_Base_NoAuth_Service.Destroy;
begin
  DisposeObject(Service);
  inherited Destroy;
end;

procedure TDTC40_Base_NoAuth_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.TotalLinkCount * 2;
end;

procedure TDTC40_Base_NoAuth_Client.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  DoClientConnected();
end;

constructor TDTC40_Base_NoAuth_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_NoAuth_Custom_Client.Create(
    TDTClient_NoAuth, DTC40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink;
  DTNoAuthClient := Client.DTClient;
end;

destructor TDTC40_Base_NoAuth_Client.Destroy;
begin
  DisposeObject(Client);
  inherited Destroy;
end;

procedure TDTC40_Base_NoAuth_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TDTC40_Base_NoAuth_Client.Connect;
begin
  inherited Connect;
  Client.Connect();
end;

function TDTC40_Base_NoAuth_Client.Connected: Boolean;
begin
  Result := Client.DTClient.LinkOk;
end;

procedure TDTC40_Base_NoAuth_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

procedure TDTC40_Base_DataStoreNoAuth_Service.DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TDTC40_Base_DataStoreNoAuth_Service.DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoUserOut(UserDefineIO);
end;

constructor TDTC40_Base_DataStoreNoAuth_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_NoAuth_Custom_Service.Create(TDataStoreService_NoAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.PublicFileDirectory := umlCombinePath(DTC40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTNoAuthService := Service.DTService as TDataStoreService_NoAuth;
  UpdateToGlobalDispatch;
end;

destructor TDTC40_Base_DataStoreNoAuth_Service.Destroy;
begin
  DisposeObject(Service);
  inherited Destroy;
end;

procedure TDTC40_Base_DataStoreNoAuth_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.TotalLinkCount * 2;
end;

procedure TDTC40_Base_DataStoreNoAuth_Client.Do_DT_P2PVM_DataStoreNoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  DoClientConnected();
end;

constructor TDTC40_Base_DataStoreNoAuth_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_NoAuth_Custom_Client.Create(
    TDataStoreClient_NoAuth, DTC40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_DataStoreNoAuth_Custom_Client_TunnelLink;
  DTNoAuthClient := Client.DTClient as TDataStoreClient_NoAuth;
end;

destructor TDTC40_Base_DataStoreNoAuth_Client.Destroy;
begin
  DisposeObject(Client);
  inherited Destroy;
end;

procedure TDTC40_Base_DataStoreNoAuth_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TDTC40_Base_DataStoreNoAuth_Client.Connect;
begin
  inherited Connect;
  Client.Connect();
end;

function TDTC40_Base_DataStoreNoAuth_Client.Connected: Boolean;
begin
  Result := Client.DTClient.LinkOk;
end;

procedure TDTC40_Base_DataStoreNoAuth_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

procedure TDTC40_Base_VirtualAuth_Service.DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO);
begin
  RegIO.Accept;
end;

procedure TDTC40_Base_VirtualAuth_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
begin
  AuthIO.Accept;
end;

procedure TDTC40_Base_VirtualAuth_Service.DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TDTC40_Base_VirtualAuth_Service.DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoUserOut(UserDefineIO);
end;

constructor TDTC40_Base_VirtualAuth_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_VirtualAuth_Custom_Service.Create(TDTService_VirtualAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnUserAuth := {$IFDEF FPC}@{$ENDIF FPC}DoUserAuth_Event;
  Service.DTService.OnUserReg := {$IFDEF FPC}@{$ENDIF FPC}DoUserReg_Event;
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.PublicFileDirectory := umlCombinePath(DTC40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTVirtualAuthService := Service.DTService;
  UpdateToGlobalDispatch;
end;

destructor TDTC40_Base_VirtualAuth_Service.Destroy;
begin
  DisposeObject(Service);
  inherited Destroy;
end;

procedure TDTC40_Base_VirtualAuth_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.TotalLinkCount * 2;
end;

procedure TDTC40_Base_VirtualAuth_Client.Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Custom_Client);
begin
  if Client.LoginIsSuccessed then
    begin
      UserName := Client.LastUser;
      Password := Client.LastPasswd;
      NoDTLink := False;
      Client.RegisterUserAndLogin := False;
    end;
  DoClientConnected();
end;

constructor TDTC40_Base_VirtualAuth_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_VirtualAuth_Custom_Client.Create(
    TDTClient_VirtualAuth, DTC40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink;
  DTVirtualAuthClient := Client.DTClient;
  UserName := ParamList.GetDefaultValue('UserName', '');
  Password := ParamList.GetDefaultValue('Password', '');
  Client.RegisterUserAndLogin := EStrToBool(ParamList.GetDefaultValue('RegUser', 'False'), False);
  NoDTLink := EStrToBool(ParamList.GetDefaultValue('NoDTLink', 'True'), True);
end;

destructor TDTC40_Base_VirtualAuth_Client.Destroy;
begin
  DisposeObject(Client);
  inherited Destroy;
end;

procedure TDTC40_Base_VirtualAuth_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TDTC40_Base_VirtualAuth_Client.Connect;
begin
  inherited Connect;
  if not NoDTLink then
      Client.Connect(UserName, Password);
end;

function TDTC40_Base_VirtualAuth_Client.Connected: Boolean;
begin
  if NoDTLink then
      Result := Client.DTClient.RecvTunnel.RemoteInited and Client.DTClient.SendTunnel.RemoteInited
  else
      Result := Client.DTClient.LinkOk;
end;

procedure TDTC40_Base_VirtualAuth_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TDTC40_Base_VirtualAuth_Client.LoginIsSuccessed: Boolean;
begin
  Result := Client.LoginIsSuccessed;
end;

procedure TDTC40_Base_DataStoreVirtualAuth_Service.DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO);
begin
  RegIO.Accept;
end;

procedure TDTC40_Base_DataStoreVirtualAuth_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
begin
  AuthIO.Accept;
end;

procedure TDTC40_Base_DataStoreVirtualAuth_Service.DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TDTC40_Base_DataStoreVirtualAuth_Service.DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoUserOut(UserDefineIO);
end;

constructor TDTC40_Base_DataStoreVirtualAuth_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_VirtualAuth_Custom_Service.Create(TDataStoreService_VirtualAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnUserAuth := {$IFDEF FPC}@{$ENDIF FPC}DoUserAuth_Event;
  Service.DTService.OnUserReg := {$IFDEF FPC}@{$ENDIF FPC}DoUserReg_Event;
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.PublicFileDirectory := umlCombinePath(DTC40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTVirtualAuthService := Service.DTService as TDataStoreService_VirtualAuth;
  UpdateToGlobalDispatch;
end;

destructor TDTC40_Base_DataStoreVirtualAuth_Service.Destroy;
begin
  DisposeObject(Service);
  inherited Destroy;
end;

procedure TDTC40_Base_DataStoreVirtualAuth_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.TotalLinkCount * 2;
end;

procedure TDTC40_Base_DataStoreVirtualAuth_Client.Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Custom_Client);
begin
  if Client.LoginIsSuccessed then
    begin
      UserName := Client.LastUser;
      Password := Client.LastPasswd;
      NoDTLink := False;
      Client.RegisterUserAndLogin := False;
    end;
  DoClientConnected();
end;

constructor TDTC40_Base_DataStoreVirtualAuth_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_VirtualAuth_Custom_Client.Create(
    TDataStoreClient_VirtualAuth, DTC40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink;
  DTVirtualAuthClient := Client.DTClient as TDataStoreClient_VirtualAuth;
  UserName := ParamList.GetDefaultValue('UserName', '');
  Password := ParamList.GetDefaultValue('Password', '');
  Client.RegisterUserAndLogin := EStrToBool(ParamList.GetDefaultValue('RegUser', 'False'), False);
  NoDTLink := EStrToBool(ParamList.GetDefaultValue('NoDTLink', 'True'), True);
end;

destructor TDTC40_Base_DataStoreVirtualAuth_Client.Destroy;
begin
  DisposeObject(Client);
  inherited Destroy;
end;

procedure TDTC40_Base_DataStoreVirtualAuth_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TDTC40_Base_DataStoreVirtualAuth_Client.Connect;
begin
  inherited Connect;
  if not NoDTLink then
      Client.Connect(UserName, Password);
end;

function TDTC40_Base_DataStoreVirtualAuth_Client.Connected: Boolean;
begin
  if NoDTLink then
      Result := Client.DTClient.RecvTunnel.RemoteInited and Client.DTClient.SendTunnel.RemoteInited
  else
      Result := Client.DTClient.LinkOk;
end;

procedure TDTC40_Base_DataStoreVirtualAuth_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TDTC40_Base_DataStoreVirtualAuth_Client.LoginIsSuccessed: Boolean;
begin
  Result := Client.LoginIsSuccessed;
end;

procedure TDTC40_Base_Service.DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TDTC40_Base_Service.DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoUserOut(UserDefineIO);
end;

constructor TDTC40_Base_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_Custom_Service.Create(TDTService, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.AllowRegisterNewUser := True;
  Service.DTService.AllowSaveUserInfo := True;
  Service.DTService.PublicPath := umlCombinePath(DTC40_RootPath, ServiceInfo.ServiceTyp.Text);
  Service.DTService.RootPath := Service.DTService.PublicPath;
  if not umlDirectoryExists(Service.DTService.PublicPath) then
      umlCreateDirectory(Service.DTService.PublicPath);
  DTService := Service.DTService;
  UpdateToGlobalDispatch;
end;

destructor TDTC40_Base_Service.Destroy;
begin
  DisposeObject(Service);
  inherited Destroy;
end;

procedure TDTC40_Base_Service.SafeCheck;
begin
  inherited SafeCheck;
  Service.DTService.SaveUserDB;
end;

procedure TDTC40_Base_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.TotalLinkCount * 2;
end;

procedure TDTC40_Base_Client.Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Custom_Client);
begin
  if Client.LoginIsSuccessed then
    begin
      UserName := Client.LastUser;
      Password := Client.LastPasswd;
      NoDTLink := False;
      Client.RegisterUserAndLogin := False;
    end;
  DoClientConnected();
end;

constructor TDTC40_Base_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_Custom_Client.Create(
    TDTClient, DTC40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_Custom_Client_TunnelLink;
  DTClient := Client.DTClient;
  UserName := ParamList.GetDefaultValue('UserName', umlMD5ToStr(ClientInfo.Hash).Text);
  Password := ParamList.GetDefaultValue('Password', UserName.Text);
  Client.RegisterUserAndLogin := EStrToBool(ParamList.GetDefaultValue('RegUser', 'False'), False);
  NoDTLink := EStrToBool(ParamList.GetDefaultValue('NoDTLink', 'True'), True);
end;

destructor TDTC40_Base_Client.Destroy;
begin
  DisposeObject(Client);
  inherited Destroy;
end;

procedure TDTC40_Base_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TDTC40_Base_Client.Connect;
begin
  inherited Connect;
  if not NoDTLink then
      Client.Connect(UserName, Password);
end;

function TDTC40_Base_Client.Connected: Boolean;
begin
  if NoDTLink then
      Result := Client.DTClient.RecvTunnel.RemoteInited and Client.DTClient.SendTunnel.RemoteInited
  else
      Result := Client.DTClient.LinkOk;
end;

procedure TDTC40_Base_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TDTC40_Base_Client.LoginIsSuccessed: Boolean;
begin
  Result := Client.LoginIsSuccessed;
end;

procedure TDTC40_Base_DataStore_Service.DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TDTC40_Base_DataStore_Service.DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoUserOut(UserDefineIO);
end;

constructor TDTC40_Base_DataStore_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_Custom_Service.Create(TDataStoreService, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.AllowRegisterNewUser := True;
  Service.DTService.AllowSaveUserInfo := True;
  Service.DTService.PublicPath := umlCombinePath(DTC40_RootPath, ServiceInfo.ServiceTyp.Text);
  Service.DTService.RootPath := Service.DTService.PublicPath;
  if not umlDirectoryExists(Service.DTService.PublicPath) then
      umlCreateDirectory(Service.DTService.PublicPath);

  DTService := Service.DTService as TDataStoreService;
  UpdateToGlobalDispatch;
end;

destructor TDTC40_Base_DataStore_Service.Destroy;
begin
  DisposeObject(Service);
  inherited Destroy;
end;

procedure TDTC40_Base_DataStore_Service.SafeCheck;
begin
  inherited SafeCheck;
  Service.DTService.SaveUserDB;
end;

procedure TDTC40_Base_DataStore_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.TotalLinkCount * 2;
end;

procedure TDTC40_Base_DataStore_Client.Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Custom_Client);
begin
  if Client.LoginIsSuccessed then
    begin
      UserName := Client.LastUser;
      Password := Client.LastPasswd;
      NoDTLink := False;
      Client.RegisterUserAndLogin := False;
    end;
  DoClientConnected();
end;

constructor TDTC40_Base_DataStore_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_Custom_Client.Create(
    TDataStoreClient, DTC40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_Custom_Client_TunnelLink;
  DTClient := Client.DTClient as TDataStoreClient;
  UserName := ParamList.GetDefaultValue('UserName', umlMD5ToStr(ClientInfo.Hash).Text);
  Password := ParamList.GetDefaultValue('Password', UserName.Text);
  Client.RegisterUserAndLogin := EStrToBool(ParamList.GetDefaultValue('RegUser', 'False'), False);
  NoDTLink := EStrToBool(ParamList.GetDefaultValue('NoDTLink', 'True'), True);
end;

destructor TDTC40_Base_DataStore_Client.Destroy;
begin
  DisposeObject(Client);
  inherited Destroy;
end;

procedure TDTC40_Base_DataStore_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TDTC40_Base_DataStore_Client.Connect;
begin
  inherited Connect;
  if not NoDTLink then
      Client.Connect(UserName, Password);
end;

function TDTC40_Base_DataStore_Client.Connected: Boolean;
begin
  if NoDTLink then
      Result := Client.DTClient.RecvTunnel.RemoteInited and Client.DTClient.SendTunnel.RemoteInited
  else
      Result := Client.DTClient.LinkOk;
end;

procedure TDTC40_Base_DataStore_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TDTC40_Base_DataStore_Client.LoginIsSuccessed: Boolean;
begin
  Result := Client.LoginIsSuccessed;
end;

initialization

// init
ProgressBackgroundProc := {$IFDEF FPC}@{$ENDIF FPC}C40Progress;

DTC40_QuietMode := False;
DTC40_SafeCheckTime := 1000 * 60 * 10;
DTC40_PhysicsReconnectionDelayTime := 5.0;
DTC40_UpdateServiceInfoDelayTime := 1000 * 1;
DTC40_PhysicsServiceTimeout := 1000 * 60;
DTC40_PhysicsTunnelTimeout := 30 * 1000;
DTC40_KillDeadPhysicsConnectionTimeout := 1000 * 60;
DTC40_KillIDCFaultTimeout := 1000 * 60 * 60;

{$IFDEF FPC}
DTC40_RootPath := umlCurrentPath;
{$ELSE FPC}
DTC40_RootPath := TPath.GetLibraryPath;
{$ENDIF FPC}

DTC40_PhysicsClientClass := PhysicsIO.TPhysicsClient;
DTC40_Registed := TDTC40_RegistedDataList.Create;
DTC40_PhysicsServicePool := TDTC40_PhysicsServicePool.Create;
DTC40_ServicePool := TDTC40_Custom_ServicePool.Create;
DTC40_PhysicsTunnelPool := TDTC40_PhysicsTunnelPool.Create;
DTC40_ClientPool := TDTC40_Custom_ClientPool.Create;

// build-in registration
RegisterC40('DP', TDTC40_Dispatch_Service, TDTC40_Dispatch_Client);
RegisterC40('NA', TDTC40_Base_NoAuth_Service, TDTC40_Base_NoAuth_Client);
RegisterC40('DNA', TDTC40_Base_DataStoreNoAuth_Service, TDTC40_Base_DataStoreNoAuth_Client);
RegisterC40('VA', TDTC40_Base_VirtualAuth_Service, TDTC40_Base_VirtualAuth_Client);
RegisterC40('DVA', TDTC40_Base_DataStoreVirtualAuth_Service, TDTC40_Base_DataStoreVirtualAuth_Client);
RegisterC40('D', TDTC40_Base_Service, TDTC40_Base_Client);
RegisterC40('DD', TDTC40_Base_DataStore_Service, TDTC40_Base_DataStore_Client);

finalization

C40Clean;

DisposeObject(DTC40_PhysicsServicePool);
DisposeObject(DTC40_ServicePool);
DisposeObject(DTC40_PhysicsTunnelPool);
DisposeObject(DTC40_ClientPool);
DisposeObject(DTC40_Registed);

end.
