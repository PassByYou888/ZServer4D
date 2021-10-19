{ ****************************************************************************** }
{ * cloud 4.0 User Database                                                    * }
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
unit DTC40_UserDB;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib,
  Geometry2DUnit, DataFrameEngine, zExpression,
  ZJson, GHashList, ZDB2_Core, ZDB2_Json, CoreCipher,
  NotifyObjectBase, MemoryStream64,
  CommunicationFramework, PhysicsIO, CommunicationFrameworkDoubleTunnelIO_NoAuth, DTC40;

type
  TDTC40_UserDB_Client = class;

  TDTC40_UserDB_Service = class(TDTC40_Base_NoAuth_Service)
  private type
    TJsonHashList = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TZDB2_Json>;

    TDTC40_UserDB_Service_SendTunnel_NoAuth = class(TPeerClientUserDefineForSendTunnel_NoAuth)
    public
      constructor Create(Owner_: TPeerIO); override;
      destructor Destroy; override;
    end;

    TDTC40_UserDB_Service_RecvTunnel_NoAuth = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
    public
      OpenUserIdentifier: TJsonHashList;
      constructor Create(Owner_: TPeerIO); override;
      destructor Destroy; override;
      procedure SendUser_Msg(FromUserName_, ToUserName_, Msg_: U_String);
      procedure SendUser_Open(UserName_, ToUserName_: U_String);
      procedure SendUser_Close(UserName_, ToUserName_: U_String);
      procedure SendUser_Request_Friend(FromUserName_, DestFriendUserName_, Msg_: U_String);
      procedure SendUser_Kick(UserName_: U_String);
    end;
  protected
    // IM
    procedure cmd_Usr_Open(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Close(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_IsOpen(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Msg(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_GetFriends(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_RemoveFriend(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_RequestAddFriend(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_ReponseAddFriend(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_OnlineNum(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Kick(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Enabled(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Disable(sender: TPeerIO; InData: TDFE);
    // db
    procedure cmd_Usr_Reg(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Exists(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Auth(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_ChangePassword(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_NewIdentifier(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_GetPrimaryIdentifier(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Get(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Set(sender: TPeerIO; InData: TDFE);
  public
    DTC40_UserDB_FileName: U_String;
    UserIdentifierHash: TJsonHashList;
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;
    JsonDatabase: TZDB2_List_Json;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
    function RegUser(UserName_, passwd_: U_String): Boolean;
    procedure SendMsg(FromUserName_, ToUserName_, Msg_: U_String);
  end;

  I_ON_DTC40_UserDB_Client_Notify = interface
    procedure Do_User_Msg(sender: TDTC40_UserDB_Client; FromUserName_, ToUserName_, Msg_: U_String);
    procedure Do_User_Open(sender: TDTC40_UserDB_Client; UserName_, ToUserName_: U_String);
    procedure Do_User_Close(sender: TDTC40_UserDB_Client; UserName_, ToUserName_: U_String);
    procedure Do_User_Request_Friend(sender: TDTC40_UserDB_Client; FromUserName_, DestFriendUserName_, Msg_: U_String);
    procedure Do_User_Kick(sender: TDTC40_UserDB_Client; UserName_: U_String);
  end;

  TDTC40_UserDB_Client = class(TDTC40_Base_NoAuth_Client)
  public type
{$REGION 'bridge_define'}
    TON_Usr_IsOpenC = procedure(sender: TDTC40_UserDB_Client; State_: Boolean);
    TON_Usr_IsOpenM = procedure(sender: TDTC40_UserDB_Client; State_: Boolean) of object;
{$IFDEF FPC}
    TON_Usr_IsOpenP = procedure(sender: TDTC40_UserDB_Client; State_: Boolean) is nested;
{$ELSE FPC}
    TON_Usr_IsOpenP = reference to procedure(sender: TDTC40_UserDB_Client; State_: Boolean);
{$ENDIF FPC}

    TON_Usr_IsOpen = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_IsOpenC;
      OnResultM: TON_Usr_IsOpenM;
      OnResultP: TON_Usr_IsOpenP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TON_Usr_GetFriendsC = procedure(sender: TDTC40_UserDB_Client; FriendArry: U_StringArray);
    TON_Usr_GetFriendsM = procedure(sender: TDTC40_UserDB_Client; FriendArry: U_StringArray) of object;
{$IFDEF FPC}
    TON_Usr_GetFriendsP = procedure(sender: TDTC40_UserDB_Client; FriendArry: U_StringArray) is nested;
{$ELSE FPC}
    TON_Usr_GetFriendsP = reference to procedure(sender: TDTC40_UserDB_Client; FriendArry: U_StringArray);
{$ENDIF FPC}

    TON_Usr_GetFriends = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_GetFriendsC;
      OnResultM: TON_Usr_GetFriendsM;
      OnResultP: TON_Usr_GetFriendsP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TON_Usr_OnlineNumC = procedure(sender: TDTC40_UserDB_Client; Online_Num, User_Num: Integer);
    TON_Usr_OnlineNumM = procedure(sender: TDTC40_UserDB_Client; Online_Num, User_Num: Integer) of object;
{$IFDEF FPC}
    TON_Usr_OnlineNumP = procedure(sender: TDTC40_UserDB_Client; Online_Num, User_Num: Integer) is nested;
{$ELSE FPC}
    TON_Usr_OnlineNumP = reference to procedure(sender: TDTC40_UserDB_Client; Online_Num, User_Num: Integer);
{$ENDIF FPC}

    TON_Usr_OnlineNum = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_OnlineNumC;
      OnResultM: TON_Usr_OnlineNumM;
      OnResultP: TON_Usr_OnlineNumP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TON_Usr_RegC = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString);
    TON_Usr_RegM = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
    TON_Usr_RegP = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
    TON_Usr_RegP = reference to procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

    TON_Usr_Reg = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_RegC;
      OnResultM: TON_Usr_RegM;
      OnResultP: TON_Usr_RegP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TON_Usr_ExistsC = procedure(sender: TDTC40_UserDB_Client; State_: Boolean);
    TON_Usr_ExistsM = procedure(sender: TDTC40_UserDB_Client; State_: Boolean) of object;
{$IFDEF FPC}
    TON_Usr_ExistsP = procedure(sender: TDTC40_UserDB_Client; State_: Boolean) is nested;
{$ELSE FPC}
    TON_Usr_ExistsP = reference to procedure(sender: TDTC40_UserDB_Client; State_: Boolean);
{$ENDIF FPC}

    TON_Usr_Exists = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_ExistsC;
      OnResultM: TON_Usr_ExistsM;
      OnResultP: TON_Usr_ExistsP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TON_Usr_AuthC = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString);
    TON_Usr_AuthM = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
    TON_Usr_AuthP = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
    TON_Usr_AuthP = reference to procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

    TON_Usr_Auth = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_AuthC;
      OnResultM: TON_Usr_AuthM;
      OnResultP: TON_Usr_AuthP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TON_Usr_ChangePasswordC = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString);
    TON_Usr_ChangePasswordM = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
    TON_Usr_ChangePasswordP = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
    TON_Usr_ChangePasswordP = reference to procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

    TON_Usr_ChangePassword = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_ChangePasswordC;
      OnResultM: TON_Usr_ChangePasswordM;
      OnResultP: TON_Usr_ChangePasswordP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TON_Usr_NewIdentifierC = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString);
    TON_Usr_NewIdentifierM = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
    TON_Usr_NewIdentifierP = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
    TON_Usr_NewIdentifierP = reference to procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

    TON_Usr_NewIdentifier = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_NewIdentifierC;
      OnResultM: TON_Usr_NewIdentifierM;
      OnResultP: TON_Usr_NewIdentifierP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TON_Usr_GetC = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
    TON_Usr_GetM = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ) of object;
{$IFDEF FPC}
    TON_Usr_GetP = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ) is nested;
{$ELSE FPC}
    TON_Usr_GetP = reference to procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
{$ENDIF FPC}

    TON_Usr_Get = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_GetC;
      OnResultM: TON_Usr_GetM;
      OnResultP: TON_Usr_GetP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TON_Usr_GetPrimaryIdentifierC = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString);
    TON_Usr_GetPrimaryIdentifierM = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString) of object;
{$IFDEF FPC}
    TON_Usr_GetPrimaryIdentifierP = procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString) is nested;
{$ELSE FPC}
    TON_Usr_GetPrimaryIdentifierP = reference to procedure(sender: TDTC40_UserDB_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString);
{$ENDIF FPC}

    TON_Usr_GetPrimaryIdentifier = class(TOnResultBridge)
    public
      Client: TDTC40_UserDB_Client;
      OnResultC: TON_Usr_GetPrimaryIdentifierC;
      OnResultM: TON_Usr_GetPrimaryIdentifierM;
      OnResultP: TON_Usr_GetPrimaryIdentifierP;
      constructor Create; override;
      procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

{$ENDREGION 'bridge_define'}
  private
    procedure cmd_Usr_Msg(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Open(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Close(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Request_Friend(sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Kick(sender: TPeerIO; InData: TDFE);
  public
    ON_DTC40_UserDB_Client_Notify: I_ON_DTC40_UserDB_Client_Notify;
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;

    // IM
    procedure Usr_Open(UserName_: U_String);
    procedure Usr_Close(UserName_: U_String);
    procedure Usr_IsOpenC(UserName_: U_String; OnResult: TON_Usr_IsOpenC);
    procedure Usr_IsOpenM(UserName_: U_String; OnResult: TON_Usr_IsOpenM);
    procedure Usr_IsOpenP(UserName_: U_String; OnResult: TON_Usr_IsOpenP);
    procedure Usr_Msg(FromUserName_, ToUserName_, Msg_: U_String);
    procedure Usr_GetFriendsC(UserName_: U_String; OnResult: TON_Usr_GetFriendsC);
    procedure Usr_GetFriendsM(UserName_: U_String; OnResult: TON_Usr_GetFriendsM);
    procedure Usr_GetFriendsP(UserName_: U_String; OnResult: TON_Usr_GetFriendsP);
    procedure Usr_RemoveFriend(UserName_, DestFriendUserName_: U_String);
    procedure Usr_RequestAddFriend(FromUserName_, DestFriendUserName_, Msg_: U_String);
    procedure Usr_ReponseAddFriend(FromUserName_, DestFriendUserName_, Msg_: U_String; Accept_: Boolean);
    procedure Usr_OnlineNumC(OnResult: TON_Usr_OnlineNumC);
    procedure Usr_OnlineNumM(OnResult: TON_Usr_OnlineNumM);
    procedure Usr_OnlineNumP(OnResult: TON_Usr_OnlineNumP);
    // user registration
    procedure Usr_RegC(UserName_, passwd_: U_String; OnResult: TON_Usr_RegC);
    procedure Usr_RegM(UserName_, passwd_: U_String; OnResult: TON_Usr_RegM);
    procedure Usr_RegP(UserName_, passwd_: U_String; OnResult: TON_Usr_RegP);
    // find user
    procedure Usr_ExistsC(UserName_: U_String; OnResult: TON_Usr_ExistsC);
    procedure Usr_ExistsM(UserName_: U_String; OnResult: TON_Usr_ExistsM);
    procedure Usr_ExistsP(UserName_: U_String; OnResult: TON_Usr_ExistsP);
    // auth: Quantum Cryptography Password
    procedure Usr_AuthC(UserName_, passwd_: U_String; OnResult: TON_Usr_AuthC);
    procedure Usr_AuthM(UserName_, passwd_: U_String; OnResult: TON_Usr_AuthM);
    procedure Usr_AuthP(UserName_, passwd_: U_String; OnResult: TON_Usr_AuthP);
    // change password
    procedure Usr_ChangePasswordC(UserName_, passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordC);
    procedure Usr_ChangePasswordM(UserName_, passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordM);
    procedure Usr_ChangePasswordP(UserName_, passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordP);
    // user Identifier
    procedure Usr_NewIdentifierC(UserName_, NewIdentifier_: U_String; OnResult: TON_Usr_NewIdentifierC);
    procedure Usr_NewIdentifierM(UserName_, NewIdentifier_: U_String; OnResult: TON_Usr_NewIdentifierM);
    procedure Usr_NewIdentifierP(UserName_, NewIdentifier_: U_String; OnResult: TON_Usr_NewIdentifierP);
    procedure Usr_GetPrimaryIdentifierC(UserName_: U_String; OnResult: TON_Usr_GetPrimaryIdentifierC);
    procedure Usr_GetPrimaryIdentifierM(UserName_: U_String; OnResult: TON_Usr_GetPrimaryIdentifierM);
    procedure Usr_GetPrimaryIdentifierP(UserName_: U_String; OnResult: TON_Usr_GetPrimaryIdentifierP);
    // get json object
    procedure Usr_GetC(UserName_, ObjName_: U_String; OnResult: TON_Usr_GetC);
    procedure Usr_GetM(UserName_, ObjName_: U_String; OnResult: TON_Usr_GetM);
    procedure Usr_GetP(UserName_, ObjName_: U_String; OnResult: TON_Usr_GetP);
    // set json object
    procedure Usr_Set(UserName_, ObjName_: U_String; Json_: TZJ);
  end;

implementation

constructor TDTC40_UserDB_Service.TDTC40_UserDB_Service_SendTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
end;

destructor TDTC40_UserDB_Service.TDTC40_UserDB_Service_SendTunnel_NoAuth.Destroy;
begin
  inherited Destroy;
end;

constructor TDTC40_UserDB_Service.TDTC40_UserDB_Service_RecvTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  OpenUserIdentifier := TJsonHashList.Create(False, 1024 * 1024, nil);
end;

destructor TDTC40_UserDB_Service.TDTC40_UserDB_Service_RecvTunnel_NoAuth.Destroy;
begin
  DisposeObject(OpenUserIdentifier);
  inherited Destroy;
end;

procedure TDTC40_UserDB_Service.TDTC40_UserDB_Service_RecvTunnel_NoAuth.SendUser_Msg(FromUserName_, ToUserName_, Msg_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(ToUserName_);
  D.WriteString(Msg_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Msg', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Service.TDTC40_UserDB_Service_RecvTunnel_NoAuth.SendUser_Open(UserName_, ToUserName_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ToUserName_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Open', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Service.TDTC40_UserDB_Service_RecvTunnel_NoAuth.SendUser_Close(UserName_, ToUserName_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ToUserName_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Close', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Service.TDTC40_UserDB_Service_RecvTunnel_NoAuth.SendUser_Request_Friend(FromUserName_, DestFriendUserName_, Msg_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(DestFriendUserName_);
  D.WriteString(Msg_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Request_Friend', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Service.TDTC40_UserDB_Service_RecvTunnel_NoAuth.SendUser_Kick(UserName_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(UserName_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Kick', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Open(sender: TPeerIO; InData: TDFE);
var
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;
  UserName_: U_String;
  j_: TZDB2_Json;
  identifier_arry: TZJArry;
  msg_arry: TZJArry;
  friend_arry: TZJArry;
  request_friend_arry: TZJArry;
  i: Integer;
  FromUserName_, Msg_: U_String;
begin
  Recv_IO_Def := sender.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  UserName_ := UserIdentifierHash[UserName_].Data.S['PrimaryIdentifier'];
  j_ := UserIdentifierHash[UserName_];

  identifier_arry := j_.Data.A['Identifier'];
  for i := 0 to identifier_arry.Count - 1 do
      Recv_IO_Def.OpenUserIdentifier.Add(identifier_arry.S[i], j_);

  msg_arry := j_.Data.A['Msg'];
  for i := 0 to msg_arry.Count - 1 do
    begin
      FromUserName_ := umlGetFirstStr_Discontinuity(msg_arry.S[i], '|');
      Msg_ := umlDeleteFirstStr_Discontinuity(msg_arry.S[i], '|');
      Recv_IO_Def.SendUser_Msg(FromUserName_, UserName_, Msg_);
    end;
  msg_arry.Clear;

  friend_arry := j_.Data.A['friend'];
  for i := 0 to friend_arry.Count - 1 do
      Recv_IO_Def.SendUser_Open(UserName_, friend_arry.S[i]);

  request_friend_arry := j_.Data.A['request_friend'];
  for i := 0 to request_friend_arry.Count - 1 do
    begin
      FromUserName_ := umlGetFirstStr_Discontinuity(request_friend_arry.S[i], '|');
      Msg_ := umlDeleteFirstStr_Discontinuity(request_friend_arry.S[i], '|');
      Recv_IO_Def.SendUser_Request_Friend(FromUserName_, UserName_, Msg_);
    end;
  request_friend_arry.Clear;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Close(sender: TPeerIO; InData: TDFE);
var
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;
  UserName_: U_String;
  j_: TZDB2_Json;
  identifier_arry: TZJArry;
  friend_arry: TZJArry;
  i: Integer;
begin
  Recv_IO_Def := sender.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  UserName_ := UserIdentifierHash[UserName_].Data.S['PrimaryIdentifier'];
  j_ := UserIdentifierHash[UserName_];

  identifier_arry := j_.Data.A['Identifier'];
  for i := 0 to identifier_arry.Count - 1 do
      Recv_IO_Def.OpenUserIdentifier.Delete(identifier_arry.S[i]);

  friend_arry := j_.Data.A['friend'];
  for i := 0 to friend_arry.Count - 1 do
      Recv_IO_Def.SendUser_Close(UserName_, friend_arry.S[i]);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_IsOpen(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_: U_String;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      exit;
    end;
  UserName_ := UserIdentifierHash[UserName_].Data.S['PrimaryIdentifier'];

  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TDTC40_UserDB_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(UserName_) then
            begin
              OutData.WriteBool(True);
              exit;
            end;
        end;
    end;
  SetLength(Arry_, 0);
  OutData.WriteBool(False);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Msg(sender: TPeerIO; InData: TDFE);
var
  FromUserName_, ToUserName_, Msg_: U_String;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;
  sendSuccessed_: Boolean;
  j_: TZDB2_Json;
  arry: TZJArry;
begin
  FromUserName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(FromUserName_) then
      exit;
  if not UserIdentifierHash.Exists(ToUserName_) then
      exit;

  FromUserName_ := UserIdentifierHash[FromUserName_].Data.S['PrimaryIdentifier'];
  ToUserName_ := UserIdentifierHash[ToUserName_].Data.S['PrimaryIdentifier'];

  sendSuccessed_ := False;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TDTC40_UserDB_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(ToUserName_) then
            begin
              Recv_IO_Def.SendUser_Msg(FromUserName_, ToUserName_, Msg_);
              sendSuccessed_ := True;
            end;
        end;
    end;
  SetLength(Arry_, 0);

  if not sendSuccessed_ then
    begin
      j_ := UserIdentifierHash[ToUserName_];
      arry := j_.Data.A['Msg'];
      arry.Add(PFormat('%s|%s', [FromUserName_.Text, Msg_.Text]));
    end;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_GetFriends(sender: TPeerIO; InData, OutData: TDFE);
var
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;
  UserName_: U_String;
  j_: TZDB2_Json;
  friend_arry: TZJArry;
  i: Integer;
begin
  Recv_IO_Def := sender.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  j_ := UserIdentifierHash[UserName_];

  friend_arry := j_.Data.A['friend'];
  for i := 0 to friend_arry.Count - 1 do
      OutData.WriteString(friend_arry.S[i]);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_RemoveFriend(sender: TPeerIO; InData: TDFE);
var
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;
  UserName_, DestFriendUserName_: U_String;
  j_: TZDB2_Json;
  friend_arry: TZJArry;
  i: Integer;
begin
  Recv_IO_Def := sender.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;
  UserName_ := InData.R.ReadString;
  DestFriendUserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  if not UserIdentifierHash.Exists(DestFriendUserName_) then
      exit;
  DestFriendUserName_ := UserIdentifierHash[DestFriendUserName_].Data.S['PrimaryIdentifier'];

  j_ := UserIdentifierHash[UserName_];
  friend_arry := j_.Data.A['friend'];
  i := 0;
  while i < friend_arry.Count do
    if DestFriendUserName_.Same(friend_arry.S[i]) then
        friend_arry.Delete(i)
    else
        inc(i);

  j_ := UserIdentifierHash[DestFriendUserName_];
  friend_arry := j_.Data.A['friend'];
  i := 0;
  while i < friend_arry.Count do
    if UserName_.Same(friend_arry.S[i]) then
        friend_arry.Delete(i)
    else
        inc(i);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_RequestAddFriend(sender: TPeerIO; InData: TDFE);
var
  FromUserName_, DestFriendUserName_, Msg_: U_String;

  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;
  sendSuccessed_: Boolean;

  j_: TZDB2_Json;
  request_friend_arry: TZJArry;
  i: Integer;
begin
  FromUserName_ := InData.R.ReadString;
  DestFriendUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;

  if not UserIdentifierHash.Exists(FromUserName_) then
      exit;
  if not UserIdentifierHash.Exists(DestFriendUserName_) then
      exit;
  FromUserName_ := UserIdentifierHash[FromUserName_].Data.S['PrimaryIdentifier'];
  DestFriendUserName_ := UserIdentifierHash[DestFriendUserName_].Data.S['PrimaryIdentifier'];

  sendSuccessed_ := False;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TDTC40_UserDB_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(DestFriendUserName_) then
            begin
              Recv_IO_Def.SendUser_Request_Friend(FromUserName_, DestFriendUserName_, Msg_);
              sendSuccessed_ := True;
            end;
        end;
    end;
  SetLength(Arry_, 0);

  if not sendSuccessed_ then
    begin
      j_ := UserIdentifierHash[DestFriendUserName_];
      request_friend_arry := j_.Data.A['request_friend'];
      request_friend_arry.Add(PFormat('%s|%s', [FromUserName_.Text, Msg_.Text]));
    end;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_ReponseAddFriend(sender: TPeerIO; InData: TDFE);
var
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;

  FromUserName_, DestFriendUserName_, Msg_: U_String;
  Accept_: Boolean;
  j_: TZDB2_Json;
  friend_arry: TZJArry;
  i: Integer;
  found_: Boolean;
begin
  Recv_IO_Def := sender.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;

  FromUserName_ := InData.R.ReadString;
  DestFriendUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  Accept_ := InData.R.ReadBool;

  if not UserIdentifierHash.Exists(FromUserName_) then
      exit;
  if not UserIdentifierHash.Exists(DestFriendUserName_) then
      exit;

  FromUserName_ := UserIdentifierHash[FromUserName_].Data.S['PrimaryIdentifier'];
  DestFriendUserName_ := UserIdentifierHash[DestFriendUserName_].Data.S['PrimaryIdentifier'];

  if Accept_ then
    begin
      j_ := UserIdentifierHash[FromUserName_];
      friend_arry := j_.Data.A['friend'];
      found_ := False;
      for i := 0 to friend_arry.Count - 1 do
        if DestFriendUserName_.Same(friend_arry.S[i]) then
          begin
            found_ := True;
            break;
          end;
      if not found_ then
          friend_arry.Add(DestFriendUserName_);

      j_ := UserIdentifierHash[DestFriendUserName_];
      friend_arry := j_.Data.A['friend'];
      found_ := False;
      for i := 0 to friend_arry.Count - 1 do
        if FromUserName_.Same(friend_arry.S[i]) then
          begin
            found_ := True;
            break;
          end;
      if not found_ then
          friend_arry.Add(FromUserName_);

      SendMsg(FromUserName_, DestFriendUserName_, PFormat('"%s" Accept Friend Request: %s', [FromUserName_.Text, Msg_.Text]));
    end
  else
    begin
      j_ := UserIdentifierHash[FromUserName_];
      friend_arry := j_.Data.A['friend'];
      i := 0;
      while i < friend_arry.Count do
        if DestFriendUserName_.Same(friend_arry.S[i]) then
            friend_arry.Delete(i)
        else
            inc(i);

      j_ := UserIdentifierHash[DestFriendUserName_];
      friend_arry := j_.Data.A['friend'];
      i := 0;
      while i < friend_arry.Count do
        if FromUserName_.Same(friend_arry.S[i]) then
            friend_arry.Delete(i)
        else
            inc(i);

      SendMsg(FromUserName_, DestFriendUserName_, PFormat('"%s" Reject Friend Request: %s', [FromUserName_.Text, Msg_.Text]));
    end;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_OnlineNum(sender: TPeerIO; InData, OutData: TDFE);
var
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  num: Integer;
begin
  num := 0;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TDTC40_UserDB_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
          inc(num, Recv_IO_Def.OpenUserIdentifier.Count);
        end;
    end;
  SetLength(Arry_, 0);

  OutData.WriteInteger(num);
  OutData.WriteInteger(UserIdentifierHash.Count);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Kick(sender: TPeerIO; InData: TDFE);
var
  UserName_: U_String;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  UserName_ := UserIdentifierHash[UserName_].Data.S['PrimaryIdentifier'];

  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TDTC40_UserDB_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(UserName_) then
            begin
              Recv_IO_Def.SendUser_Kick(UserName_);
            end;
        end;
    end;
  SetLength(Arry_, 0);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Enabled(sender: TPeerIO; InData: TDFE);
var
  UserName_, passwd_: U_String;
  j_: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  passwd_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  j_ := UserIdentifierHash[UserName_];
  j_.Data.B['Enabled'] := True;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Disable(sender: TPeerIO; InData: TDFE);
var
  UserName_, passwd_: U_String;
  j_: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  passwd_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  j_ := UserIdentifierHash[UserName_];
  j_.Data.B['Enabled'] := False;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Reg(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, passwd_: U_String;
  j_: TZDB2_Json;
  arry: TZJArry;
  i: Integer;
begin
  UserName_ := InData.R.ReadString;
  passwd_ := InData.R.ReadString;
  if (UserName_.L < 6) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('User name "%s" is too short', [UserName_.Text]);
      exit;
    end;

  if (passwd_.L < 6) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('password is too short');
      exit;
    end;

  if UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('repeat user "%s"', [UserName_.Text]);
      exit;
    end;

  j_ := JsonDatabase.NewData;
  arry := j_.Data.A['Identifier'];
  arry.Add(UserName_);
  j_.Data.S['PrimaryIdentifier'] := UserName_;
  j_.Data.S['Password'] := GenerateQuantumCryptographyPassword(passwd_.LowerText);
  j_.Data.D['RegTime'] := umlNow;
  j_.Data.D['LastAuth'] := umlNow;
  j_.Data.B['Enabled'] := True;
  JsonDatabase.Add(j_);
  for i := 0 to arry.Count - 1 do
      UserIdentifierHash.Add(arry.S[i], j_);
  OutData.WriteBool(True);
  OutData.WriteString('user "%s" registration done.', [UserName_.Text]);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Exists(sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteBool(UserIdentifierHash.Exists(InData.R.ReadString));
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Auth(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, passwd_: U_String;
  j_: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  passwd_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;

  j_ := UserIdentifierHash[UserName_];
  if (j_.Data.IndexOf('Enabled') >= 0) and (not j_.Data.B['Enabled']) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('account "%s" is Disable.', [UserName_.Text]);
      exit;
    end;

  if CompareQuantumCryptographyPassword(passwd_.LowerText, j_.Data.S['Password']) then
    begin
      j_.Data.D['LastAuth'] := umlNow;
      OutData.WriteBool(True);
      OutData.WriteString('user "%s" auth successed.', [UserName_.Text]);
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth password from user "%s"', [UserName_.Text]);
    end;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_ChangePassword(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, passwd_, NewPasswd_: U_String;
  j_: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  passwd_ := InData.R.ReadString;
  NewPasswd_ := InData.R.ReadString;

  if (NewPasswd_.L < 6) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('new password is too short');
      exit;
    end;

  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;

  j_ := UserIdentifierHash[UserName_];

  if CompareQuantumCryptographyPassword(passwd_.LowerText, j_.Data.S['Password']) then
    begin
      j_.Data.S['Password'] := GenerateQuantumCryptographyPassword(NewPasswd_.LowerText);
      OutData.WriteBool(True);
      OutData.WriteString('"%s" change password successed.', [UserName_.Text]);
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no match password from user "%s"', [UserName_.Text]);
    end;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_NewIdentifier(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, NewIdentifier_: U_String;
  j_: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  NewIdentifier_ := InData.R.ReadString;

  if (NewIdentifier_.L < 6) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('New Identifier is too short');
      exit;
    end;

  if UserIdentifierHash.Exists(NewIdentifier_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('found user "%s"', [NewIdentifier_.Text]);
      exit;
    end;

  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;

  j_ := UserIdentifierHash[UserName_];
  j_.Data.A['Identifier'].Add(NewIdentifier_);
  UserIdentifierHash.Add(NewIdentifier_, j_);
  OutData.WriteBool(True);
  OutData.WriteString('new Identifier "%s" for user "%s"', [NewIdentifier_.Text, UserName_.Text]);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_GetPrimaryIdentifier(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_: U_String;
  j_: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;

  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;

  j_ := UserIdentifierHash[UserName_];
  OutData.WriteBool(True);
  OutData.WriteString('primary Identifier "%s"', [j_.Data.S['PrimaryIdentifier']]);
  OutData.WriteString(j_.Data.S['PrimaryIdentifier']);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Get(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, ObjName_: U_String;
  j_: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;
  ObjName_ := InData.R.ReadString;
  j_ := UserIdentifierHash[UserName_];
  OutData.WriteBool(True);
  OutData.WriteString('get user "%s" json object %s', [UserName_.Text, ObjName_.Text]);
  OutData.WriteJson(j_.Data.O[ObjName_]);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Set(sender: TPeerIO; InData: TDFE);
var
  UserName_, ObjName_: U_String;
  j_: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  ObjName_ := InData.R.ReadString;
  j_ := UserIdentifierHash[UserName_];
  InData.R.ReadJson(j_.Data.O[ObjName_]);
end;

constructor TDTC40_UserDB_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  fs: TCoreClassFileStream;
  i, j: Integer;
  json: TZDB2_Json;
  identifier_arry: TZJArry;
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  DTNoAuthService.RecvTunnel.UserDefineClass := TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  DTNoAuthService.SendTunnel.UserDefineClass := TDTC40_UserDB_Service_SendTunnel_NoAuth;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Open').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Open;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Close').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Close;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_IsOpen').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_IsOpen;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Msg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Msg;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_GetFriends').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_GetFriends;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_RemoveFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_RemoveFriend;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_RequestAddFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_RequestAddFriend;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_ReponseAddFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_ReponseAddFriend;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_OnlineNum').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_OnlineNum;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Kick').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Kick;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Enabled').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Enabled;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Disable').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Disable;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Reg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Reg;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Exists').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Exists;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Auth').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Auth;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_ChangePassword').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_ChangePassword;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_NewIdentifier').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_NewIdentifier;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_GetPrimaryIdentifier').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_GetPrimaryIdentifier;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Get').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Get;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Set').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Set;
  // is only instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;

  DTC40_UserDB_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.Space', [ServiceInfo.ServiceTyp.Text]));
  UserIdentifierHash := TJsonHashList.Create(False, 1024 * 1024 * 128, nil);
  UserIdentifierHash.AccessOptimization := True;
  UserIdentifierHash.IgnoreCase := False;

  if umlFileExists(DTC40_UserDB_FileName) then
      fs := TCoreClassFileStream.Create(DTC40_UserDB_FileName, fmOpenReadWrite)
  else
      fs := TCoreClassFileStream.Create(DTC40_UserDB_FileName, fmCreate);

  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '60*1000'), 60 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '128*1024*1024'), 128 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '1024'), 1024);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'True'), True);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csRijndael]);
  ZDB2Password := ParamList.GetDefaultValue('Password', DTC40.DTC40_Password);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;

  JsonDatabase := TZDB2_List_Json.Create(TZDB2_Json, nil, ZDB2RecycleMemoryTimeOut, fs, ZDB2DeltaSpace, ZDB2BlockSize, ZDB2Cipher);
  JsonDatabase.AutoFreeStream := True;

  DoStatus('extract user Database.');
  for j := 0 to JsonDatabase.Count - 1 do
    begin
      json := JsonDatabase[j];
      identifier_arry := json.Data.A['Identifier'];
      for i := 0 to identifier_arry.Count - 1 do
          UserIdentifierHash.Add(identifier_arry.S[i], json);
      json.RecycleMemory;
    end;
  JsonDatabase.Flush;
  DoStatus('extract user Database done.');
end;

destructor TDTC40_UserDB_Service.Destroy;
begin
  DisposeObject(UserIdentifierHash);
  DisposeObject(JsonDatabase);
  DisposeObject(ZDB2Cipher);
  inherited Destroy;
end;

procedure TDTC40_UserDB_Service.SafeCheck;
begin
  inherited SafeCheck;
  JsonDatabase.Flush;
end;

procedure TDTC40_UserDB_Service.Progress;
begin
  inherited Progress;
  JsonDatabase.Progress;
end;

function TDTC40_UserDB_Service.RegUser(UserName_, passwd_: U_String): Boolean;
var
  j_: TZDB2_Json;
  arry: TZJArry;
  i: Integer;
begin
  Result := False;
  if (UserName_.L < 6) then
    begin
      DoStatus('User name "%s" is too short', [UserName_.Text]);
      exit;
    end;

  if (passwd_.L < 6) then
    begin
      DoStatus('password is too short');
      exit;
    end;

  if UserIdentifierHash.Exists(UserName_) then
    begin
      DoStatus('repeat user "%s"', [UserName_.Text]);
      exit;
    end;

  j_ := JsonDatabase.NewData;
  arry := j_.Data.A['Identifier'];
  arry.Add(UserName_);
  j_.Data.S['PrimaryIdentifier'] := UserName_;
  j_.Data.S['Password'] := GenerateQuantumCryptographyPassword(passwd_.LowerText);
  j_.Data.D['RegTime'] := umlNow;
  j_.Data.D['LastAuth'] := umlNow;
  j_.Data.B['Enabled'] := True;
  JsonDatabase.Add(j_);
  for i := 0 to arry.Count - 1 do
      UserIdentifierHash.Add(arry.S[i], j_);
  DoStatus('user "%s" registration done.', [UserName_.Text]);
  Result := True;
end;

procedure TDTC40_UserDB_Service.SendMsg(FromUserName_, ToUserName_, Msg_: U_String);
var
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TDTC40_UserDB_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TDTC40_UserDB_Service_SendTunnel_NoAuth;
  sendSuccessed_: Boolean;
  j_: TZDB2_Json;
  arry: TZJArry;
begin
  if not UserIdentifierHash.Exists(FromUserName_) then
      exit;
  if not UserIdentifierHash.Exists(ToUserName_) then
      exit;

  sendSuccessed_ := False;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TDTC40_UserDB_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TDTC40_UserDB_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TDTC40_UserDB_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(ToUserName_) then
            begin
              Recv_IO_Def.SendUser_Msg(FromUserName_, ToUserName_, Msg_);
              sendSuccessed_ := True;
            end;
        end;
    end;
  SetLength(Arry_, 0);

  if not sendSuccessed_ then
    begin
      j_ := UserIdentifierHash[ToUserName_];
      arry := j_.Data.A['Msg'];
      arry.Add(PFormat('%s|%s', [FromUserName_.Text, Msg_.Text]));
    end;
end;

constructor TDTC40_UserDB_Client.TON_Usr_IsOpen.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_IsOpen.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
begin
  State_ := Result_.R.ReadBool;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.TON_Usr_IsOpen.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
begin
  State_ := False;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_UserDB_Client.TON_Usr_GetFriends.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_GetFriends.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  FriendArry: U_StringArray;
  i: Integer;
begin
  SetLength(FriendArry, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      FriendArry[i] := Result_.ReadString(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, FriendArry);
    if Assigned(OnResultM) then
        OnResultM(Client, FriendArry);
    if Assigned(OnResultP) then
        OnResultP(Client, FriendArry);
  except
  end;
  SetLength(FriendArry, 0);
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.TON_Usr_GetFriends.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  FriendArry: U_StringArray;
begin
  SetLength(FriendArry, 0);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, FriendArry);
    if Assigned(OnResultM) then
        OnResultM(Client, FriendArry);
    if Assigned(OnResultP) then
        OnResultP(Client, FriendArry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_UserDB_Client.TON_Usr_OnlineNum.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_OnlineNum.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  Online_Num, User_Num: Integer;
  i: Integer;
begin
  Online_Num := Result_.R.ReadInteger;
  User_Num := Result_.R.ReadInteger;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Online_Num, User_Num);
    if Assigned(OnResultM) then
        OnResultM(Client, Online_Num, User_Num);
    if Assigned(OnResultP) then
        OnResultP(Client, Online_Num, User_Num);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.TON_Usr_OnlineNum.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  Online_Num, User_Num: Integer;
  i: Integer;
begin
  Online_Num := 0;
  User_Num := 0;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Online_Num, User_Num);
    if Assigned(OnResultM) then
        OnResultM(Client, Online_Num, User_Num);
    if Assigned(OnResultP) then
        OnResultP(Client, Online_Num, User_Num);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_UserDB_Client.TON_Usr_Reg.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_Reg.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.TON_Usr_Reg.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_UserDB_Client.TON_Usr_Exists.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_Exists.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
begin
  State_ := False;
  if Result_.Count > 0 then
    begin
      State_ := Result_.R.ReadBool;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.TON_Usr_Exists.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
begin
  State_ := False;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_UserDB_Client.TON_Usr_Auth.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_Auth.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.TON_Usr_Auth.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_UserDB_Client.TON_Usr_ChangePassword.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_ChangePassword.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.TON_Usr_ChangePassword.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_UserDB_Client.TON_Usr_NewIdentifier.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_NewIdentifier.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.TON_Usr_NewIdentifier.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_UserDB_Client.TON_Usr_Get.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_Get.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  j_: TZJ;
begin
  State_ := False;
  info_ := 'error.';
  j_ := TZJ.Create;
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
      if State_ then
          Result_.R.ReadJson(j_);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, j_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, j_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, j_);
  except
  end;
  DelayFreeObject(1.0, self, j_);
end;

procedure TDTC40_UserDB_Client.TON_Usr_Get.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  j_: TZJ;
begin
  State_ := False;
  info_ := 'error.';
  j_ := TZJ.Create;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, j_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, j_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, j_);
  except
  end;
  DelayFreeObject(1.0, self, j_);
end;

constructor TDTC40_UserDB_Client.TON_Usr_GetPrimaryIdentifier.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_UserDB_Client.TON_Usr_GetPrimaryIdentifier.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_, PrimaryIdentifier_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  PrimaryIdentifier_ := '';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
      if State_ then
          PrimaryIdentifier_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, PrimaryIdentifier_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, PrimaryIdentifier_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, PrimaryIdentifier_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.TON_Usr_GetPrimaryIdentifier.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_, PrimaryIdentifier_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  PrimaryIdentifier_ := '';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, PrimaryIdentifier_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, PrimaryIdentifier_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, PrimaryIdentifier_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TDTC40_UserDB_Client.cmd_Usr_Msg(sender: TPeerIO; InData: TDFE);
var
  FromUserName_, ToUserName_, Msg_: U_String;
begin
  FromUserName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  if Assigned(ON_DTC40_UserDB_Client_Notify) then
      ON_DTC40_UserDB_Client_Notify.Do_User_Msg(self, FromUserName_, ToUserName_, Msg_);
end;

procedure TDTC40_UserDB_Client.cmd_Usr_Open(sender: TPeerIO; InData: TDFE);
var
  UserName_, ToUserName_: U_String;
begin
  UserName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  if Assigned(ON_DTC40_UserDB_Client_Notify) then
      ON_DTC40_UserDB_Client_Notify.Do_User_Open(self, UserName_, ToUserName_);
end;

procedure TDTC40_UserDB_Client.cmd_Usr_Close(sender: TPeerIO; InData: TDFE);
var
  UserName_, ToUserName_: U_String;
begin
  UserName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  if Assigned(ON_DTC40_UserDB_Client_Notify) then
      ON_DTC40_UserDB_Client_Notify.Do_User_Close(self, UserName_, ToUserName_);
end;

procedure TDTC40_UserDB_Client.cmd_Usr_Request_Friend(sender: TPeerIO; InData: TDFE);
var
  FromUserName_, DestFriendUserName_, Msg_: U_String;
begin
  FromUserName_ := InData.R.ReadString;
  DestFriendUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  if Assigned(ON_DTC40_UserDB_Client_Notify) then
      ON_DTC40_UserDB_Client_Notify.Do_User_Request_Friend(self, FromUserName_, DestFriendUserName_, Msg_);
end;

procedure TDTC40_UserDB_Client.cmd_Usr_Kick(sender: TPeerIO; InData: TDFE);
var
  UserName_: U_String;
begin
  UserName_ := InData.R.ReadString;
  if Assigned(ON_DTC40_UserDB_Client_Notify) then
      ON_DTC40_UserDB_Client_Notify.Do_User_Kick(self, UserName_);
end;

constructor TDTC40_UserDB_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Msg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Msg;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Open').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Open;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Close').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Close;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Request_Friend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Request_Friend;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Kick').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Kick;
  ON_DTC40_UserDB_Client_Notify := nil;
end;

destructor TDTC40_UserDB_Client.Destroy;
begin
  ON_DTC40_UserDB_Client_Notify := nil;
  inherited Destroy;
end;

procedure TDTC40_UserDB_Client.Usr_Open(UserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Open', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_Close(UserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Close', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_IsOpenC(UserName_: U_String; OnResult: TON_Usr_IsOpenC);
var
  tmp: TON_Usr_IsOpen;
  D: TDFE;
begin
  tmp := TON_Usr_IsOpen.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_IsOpenM(UserName_: U_String; OnResult: TON_Usr_IsOpenM);
var
  tmp: TON_Usr_IsOpen;
  D: TDFE;
begin
  tmp := TON_Usr_IsOpen.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_IsOpenP(UserName_: U_String; OnResult: TON_Usr_IsOpenP);
var
  tmp: TON_Usr_IsOpen;
  D: TDFE;
begin
  tmp := TON_Usr_IsOpen.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_Msg(FromUserName_, ToUserName_, Msg_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(ToUserName_);
  D.WriteString(Msg_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Msg', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetFriendsC(UserName_: U_String; OnResult: TON_Usr_GetFriendsC);
var
  tmp: TON_Usr_GetFriends;
  D: TDFE;
begin
  tmp := TON_Usr_GetFriends.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetFriends', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetFriendsM(UserName_: U_String; OnResult: TON_Usr_GetFriendsM);
var
  tmp: TON_Usr_GetFriends;
  D: TDFE;
begin
  tmp := TON_Usr_GetFriends.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetFriends', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetFriendsP(UserName_: U_String; OnResult: TON_Usr_GetFriendsP);
var
  tmp: TON_Usr_GetFriends;
  D: TDFE;
begin
  tmp := TON_Usr_GetFriends.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetFriends', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RemoveFriend(UserName_, DestFriendUserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(DestFriendUserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_RemoveFriend', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RequestAddFriend(FromUserName_, DestFriendUserName_, Msg_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(DestFriendUserName_);
  D.WriteString(Msg_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_RequestAddFriend', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ReponseAddFriend(FromUserName_, DestFriendUserName_, Msg_: U_String; Accept_: Boolean);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(DestFriendUserName_);
  D.WriteString(Msg_);
  D.WriteBool(Accept_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_ReponseAddFriend', D);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_OnlineNumC(OnResult: TON_Usr_OnlineNumC);
var
  tmp: TON_Usr_OnlineNum;
  D: TDFE;
begin
  tmp := TON_Usr_OnlineNum.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineNum', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_OnlineNumM(OnResult: TON_Usr_OnlineNumM);
var
  tmp: TON_Usr_OnlineNum;
  D: TDFE;
begin
  tmp := TON_Usr_OnlineNum.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineNum', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_OnlineNumP(OnResult: TON_Usr_OnlineNumP);
var
  tmp: TON_Usr_OnlineNum;
  D: TDFE;
begin
  tmp := TON_Usr_OnlineNum.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineNum', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RegC(UserName_, passwd_: U_String; OnResult: TON_Usr_RegC);
var
  tmp: TON_Usr_Reg;
  D: TDFE;
begin
  tmp := TON_Usr_Reg.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RegM(UserName_, passwd_: U_String; OnResult: TON_Usr_RegM);
var
  tmp: TON_Usr_Reg;
  D: TDFE;
begin
  tmp := TON_Usr_Reg.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RegP(UserName_, passwd_: U_String; OnResult: TON_Usr_RegP);
var
  tmp: TON_Usr_Reg;
  D: TDFE;
begin
  tmp := TON_Usr_Reg.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ExistsC(UserName_: U_String; OnResult: TON_Usr_ExistsC);
var
  tmp: TON_Usr_Exists;
  D: TDFE;
begin
  tmp := TON_Usr_Exists.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Exists', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ExistsM(UserName_: U_String; OnResult: TON_Usr_ExistsM);
var
  tmp: TON_Usr_Exists;
  D: TDFE;
begin
  tmp := TON_Usr_Exists.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Exists', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ExistsP(UserName_: U_String; OnResult: TON_Usr_ExistsP);
var
  tmp: TON_Usr_Exists;
  D: TDFE;
begin
  tmp := TON_Usr_Exists.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Exists', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_AuthC(UserName_, passwd_: U_String; OnResult: TON_Usr_AuthC);
var
  tmp: TON_Usr_Auth;
  D: TDFE;
begin
  tmp := TON_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_AuthM(UserName_, passwd_: U_String; OnResult: TON_Usr_AuthM);
var
  tmp: TON_Usr_Auth;
  D: TDFE;
begin
  tmp := TON_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_AuthP(UserName_, passwd_: U_String; OnResult: TON_Usr_AuthP);
var
  tmp: TON_Usr_Auth;
  D: TDFE;
begin
  tmp := TON_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ChangePasswordC(UserName_, passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordC);
var
  tmp: TON_Usr_ChangePassword;
  D: TDFE;
begin
  tmp := TON_Usr_ChangePassword.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_ChangePassword', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ChangePasswordM(UserName_, passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordM);
var
  tmp: TON_Usr_ChangePassword;
  D: TDFE;
begin
  tmp := TON_Usr_ChangePassword.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_ChangePassword', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ChangePasswordP(UserName_, passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordP);
var
  tmp: TON_Usr_ChangePassword;
  D: TDFE;
begin
  tmp := TON_Usr_ChangePassword.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_ChangePassword', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_NewIdentifierC(UserName_, NewIdentifier_: U_String; OnResult: TON_Usr_NewIdentifierC);
var
  tmp: TON_Usr_NewIdentifier;
  D: TDFE;
begin
  tmp := TON_Usr_NewIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(NewIdentifier_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_NewIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_NewIdentifierM(UserName_, NewIdentifier_: U_String; OnResult: TON_Usr_NewIdentifierM);
var
  tmp: TON_Usr_NewIdentifier;
  D: TDFE;
begin
  tmp := TON_Usr_NewIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(NewIdentifier_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_NewIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_NewIdentifierP(UserName_, NewIdentifier_: U_String; OnResult: TON_Usr_NewIdentifierP);
var
  tmp: TON_Usr_NewIdentifier;
  D: TDFE;
begin
  tmp := TON_Usr_NewIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(NewIdentifier_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_NewIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetPrimaryIdentifierC(UserName_: U_String; OnResult: TON_Usr_GetPrimaryIdentifierC);
var
  tmp: TON_Usr_GetPrimaryIdentifier;
  D: TDFE;
begin
  tmp := TON_Usr_GetPrimaryIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetPrimaryIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetPrimaryIdentifierM(UserName_: U_String; OnResult: TON_Usr_GetPrimaryIdentifierM);
var
  tmp: TON_Usr_GetPrimaryIdentifier;
  D: TDFE;
begin
  tmp := TON_Usr_GetPrimaryIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetPrimaryIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetPrimaryIdentifierP(UserName_: U_String; OnResult: TON_Usr_GetPrimaryIdentifierP);
var
  tmp: TON_Usr_GetPrimaryIdentifier;
  D: TDFE;
begin
  tmp := TON_Usr_GetPrimaryIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetPrimaryIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetC(UserName_, ObjName_: U_String; OnResult: TON_Usr_GetC);
var
  tmp: TON_Usr_Get;
  D: TDFE;
begin
  tmp := TON_Usr_Get.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ObjName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetM(UserName_, ObjName_: U_String; OnResult: TON_Usr_GetM);
var
  tmp: TON_Usr_Get;
  D: TDFE;
begin
  tmp := TON_Usr_Get.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ObjName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetP(UserName_, ObjName_: U_String; OnResult: TON_Usr_GetP);
var
  tmp: TON_Usr_Get;
  D: TDFE;
begin
  tmp := TON_Usr_Get.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ObjName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_Set(UserName_, ObjName_: U_String; Json_: TZJ);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ObjName_);
  D.WriteJson(Json_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Set', D);
  DisposeObject(D);
end;

initialization

RegisterC40('UserDB', TDTC40_UserDB_Service, TDTC40_UserDB_Client);

end.
