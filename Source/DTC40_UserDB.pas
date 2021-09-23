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
  Geometry2DUnit, DataFrameEngine,
  ZJson, GHashList,
  NotifyObjectBase, CoreCipher, MemoryStream64,
  ObjectData, ObjectDataManager, ItemStream,
  CommunicationFramework, PhysicsIO, CommunicationFrameworkDoubleTunnelIO_NoAuth, DTC40;

type
  TDTC40_UserDB_Client = class;
  TJsonHashList = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TZ_JsonObject>;

  TDTC40_UserDB_Service = class(TDTC40_Base_NoAuth_Service)
  protected
    // init build-in data
    IsLoading: Boolean;
    IsSaveing: Boolean;
    procedure DoLoading();
    procedure DoBackground_Save(thSender: TCompute);
  private
    procedure cmd_Usr_Reg(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Exists(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Auth(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_ChangePassword(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_NewIdentifier(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Get(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Set(sender: TPeerIO; InData: TDFE);
  public
    UserIdentifierHash: TJsonHashList;
    UserJsonPool: TZJL;
    DTC40_UserDB_FileName: U_String;

    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;

    procedure LoadUserDB;
    procedure SaveUserDBAsDFE(DFE: TDFE);
  end;

{$REGION 'bridge_define'}

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

{$ENDREGION 'bridge_define'}

  TDTC40_UserDB_Client = class(TDTC40_Base_NoAuth_Client)
  public
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;

    // user registration
    procedure Usr_RegC(UserName_, Passwd_: U_String; OnResult: TON_Usr_RegC);
    procedure Usr_RegM(UserName_, Passwd_: U_String; OnResult: TON_Usr_RegM);
    procedure Usr_RegP(UserName_, Passwd_: U_String; OnResult: TON_Usr_RegP);
    // find user
    procedure Usr_ExistsC(UserName_: U_String; OnResult: TON_Usr_ExistsC);
    procedure Usr_ExistsM(UserName_: U_String; OnResult: TON_Usr_ExistsM);
    procedure Usr_ExistsP(UserName_: U_String; OnResult: TON_Usr_ExistsP);
    // auth: Quantum Cryptography Password
    procedure Usr_AuthC(UserName_, Passwd_: U_String; OnResult: TON_Usr_AuthC);
    procedure Usr_AuthM(UserName_, Passwd_: U_String; OnResult: TON_Usr_AuthM);
    procedure Usr_AuthP(UserName_, Passwd_: U_String; OnResult: TON_Usr_AuthP);
    // change password
    procedure Usr_ChangePasswordC(UserName_, Passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordC);
    procedure Usr_ChangePasswordM(UserName_, Passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordM);
    procedure Usr_ChangePasswordP(UserName_, Passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordP);
    // create user Identifier
    procedure Usr_NewIdentifierC(UserName_, NewIdentifier_: U_String; OnResult: TON_Usr_NewIdentifierC);
    procedure Usr_NewIdentifierM(UserName_, NewIdentifier_: U_String; OnResult: TON_Usr_NewIdentifierM);
    procedure Usr_NewIdentifierP(UserName_, NewIdentifier_: U_String; OnResult: TON_Usr_NewIdentifierP);
    // get json object
    procedure Usr_GetC(UserName_, ObjName_: U_String; OnResult: TON_Usr_GetC);
    procedure Usr_GetM(UserName_, ObjName_: U_String; OnResult: TON_Usr_GetM);
    procedure Usr_GetP(UserName_, ObjName_: U_String; OnResult: TON_Usr_GetP);
    // set json object
    procedure Usr_Set(UserName_, ObjName_: U_String; Json_: TZJ);
  end;

implementation

procedure TDTC40_UserDB_Service.DoLoading;
begin
  IsLoading := True;
  IsSaveing := False;
  try
      LoadUserDB;
  except
  end;
  IsLoading := False;
end;

procedure TDTC40_UserDB_Service.DoBackground_Save(thSender: TCompute);
var
  D: TDFE;
begin
  try
    D := TDFE(thSender.UserObject);
    D.SaveToFile(DTC40_UserDB_FileName);
    DisposeObject(D);
    DoStatus('Save User Database Done.');
  except
  end;
  IsSaveing := False;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Reg(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, Passwd_: U_String;
  j_: TZJ;
begin
  UserName_ := InData.R.ReadString;
  Passwd_ := InData.R.ReadString;
  if (UserName_.L < 6) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('User name "%s" is too short', [UserName_.Text]);
      exit;
    end;

  if (Passwd_.L < 6) then
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

  j_ := TZJ.Create;
  j_.A['Identifier'].Add(UserName_);
  j_.S['Password'] := GenerateQuantumCryptographyPassword(Passwd_.LowerText);
  UserJsonPool.Add(j_);
  UserIdentifierHash.Add(UserName_, j_);
  OutData.WriteBool(True);
  OutData.WriteString('user "%s" registration done.', [UserName_.Text]);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Exists(sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteBool(UserIdentifierHash.Exists(InData.R.ReadString));
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Auth(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, Passwd_: U_String;
  j_: TZJ;
begin
  UserName_ := InData.R.ReadString;
  Passwd_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;

  j_ := UserIdentifierHash[UserName_];

  if CompareQuantumCryptographyPassword(Passwd_.LowerText, j_.S['Password']) then
    begin
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
  UserName_, Passwd_, NewPasswd_: U_String;
  j_: TZJ;
begin
  UserName_ := InData.R.ReadString;
  Passwd_ := InData.R.ReadString;
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

  if CompareQuantumCryptographyPassword(Passwd_.LowerText, j_.S['Password']) then
    begin
      j_.S['Password'] := GenerateQuantumCryptographyPassword(NewPasswd_.LowerText);
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
  j_: TZJ;
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
  j_.A['Identifier'].Add(NewIdentifier_);
  UserIdentifierHash.Add(NewIdentifier_, j_);
  OutData.WriteBool(True);
  OutData.WriteString('new Identifier "%s" for user "%s"', [NewIdentifier_.Text, UserName_.Text]);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Get(sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, ObjName_: U_String;
  j_: TZJ;
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
  OutData.WriteJson(j_.O[ObjName_]);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Set(sender: TPeerIO; InData: TDFE);
var
  UserName_, ObjName_: U_String;
  j_: TZJ;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  ObjName_ := InData.R.ReadString;
  j_ := UserIdentifierHash[UserName_];
  InData.R.ReadJson(j_.O[ObjName_]);
end;

constructor TDTC40_UserDB_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  ServiceInfo.OnlyInstance := True;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Reg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Reg;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Exists').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Exists;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Auth').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Auth;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_ChangePassword').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_ChangePassword;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_NewIdentifier').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_NewIdentifier;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Get').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Get;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Set').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Set;
  UpdateToGlobalDispatch;

  UserIdentifierHash := TJsonHashList.Create(False, 1024 * 1024, nil);
  UserIdentifierHash.AccessOptimization := True;
  UserIdentifierHash.IgnoreCase := False;
  UserJsonPool := TZJL.Create(True);
  DTC40_UserDB_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.DFE', [ServiceInfo.ServiceTyp.Text]));
  DoLoading;
end;

destructor TDTC40_UserDB_Service.Destroy;
begin
  DisposeObject(UserIdentifierHash);
  DisposeObject(UserJsonPool);
  inherited Destroy;
end;

procedure TDTC40_UserDB_Service.SafeCheck;
var
  D: TDFE;
begin
  inherited SafeCheck;
  if IsSaveing then
      exit;

  IsSaveing := True;
  D := TDFE.Create;
  DoStatus('Extract User Json.');
  SaveUserDBAsDFE(D);
  DoStatus('Save User Database.');
  TCompute.RunM(nil, D, {$IFDEF FPC}@{$ENDIF FPC}DoBackground_Save);
end;

procedure TDTC40_UserDB_Service.LoadUserDB;
var
  D: TDFE;
  j_: TZJ;
  arry: TZJArry;
  i: Integer;
begin
  if not umlFileExists(DTC40_UserDB_FileName) then
      exit;

  UserJsonPool.Clear;
  UserIdentifierHash.Clear;

  D := TDFE.Create;

  try
    DoStatus('Load user database "%s"', [DTC40_UserDB_FileName.Text]);
    D.LoadFromFile(DTC40_UserDB_FileName);

    DoStatus('extract user Database.');
    while D.R.NotEnd do
      begin
        j_ := TZJ.Create;
        D.R.ReadJson(j_);
        UserJsonPool.Add(j_);
        arry := j_.A['Identifier'];
        for i := 0 to arry.Count - 1 do
            UserIdentifierHash.Add(arry.S[i], j_);
      end;
    DoStatus('extract user Database done.');
  except
  end;

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Service.SaveUserDBAsDFE(DFE: TDFE);
var
  i: Integer;
begin
  for i := 0 to UserJsonPool.Count - 1 do
      DFE.WriteJson(UserJsonPool[i], False);
end;

constructor TON_Usr_Reg.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Usr_Reg.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count = 2 then
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

procedure TON_Usr_Reg.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TON_Usr_Exists.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Usr_Exists.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
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

procedure TON_Usr_Exists.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TON_Usr_Auth.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Usr_Auth.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count = 2 then
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

procedure TON_Usr_Auth.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TON_Usr_ChangePassword.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Usr_ChangePassword.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count = 2 then
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

procedure TON_Usr_ChangePassword.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TON_Usr_NewIdentifier.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Usr_NewIdentifier.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count = 2 then
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

procedure TON_Usr_NewIdentifier.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TON_Usr_Get.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Usr_Get.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  j_: TZJ;
begin
  State_ := False;
  info_ := 'error.';
  j_ := TZJ.Create;
  if Result_.Count = 2 then
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

procedure TON_Usr_Get.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TDTC40_UserDB_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
end;

destructor TDTC40_UserDB_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TDTC40_UserDB_Client.Usr_RegC(UserName_, Passwd_: U_String; OnResult: TON_Usr_RegC);
var
  tmp: TON_Usr_Reg;
  D: TDFE;
begin
  tmp := TON_Usr_Reg.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(Passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RegM(UserName_, Passwd_: U_String; OnResult: TON_Usr_RegM);
var
  tmp: TON_Usr_Reg;
  D: TDFE;
begin
  tmp := TON_Usr_Reg.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(Passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RegP(UserName_, Passwd_: U_String; OnResult: TON_Usr_RegP);
var
  tmp: TON_Usr_Reg;
  D: TDFE;
begin
  tmp := TON_Usr_Reg.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(Passwd_);
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

procedure TDTC40_UserDB_Client.Usr_AuthC(UserName_, Passwd_: U_String; OnResult: TON_Usr_AuthC);
var
  tmp: TON_Usr_Auth;
  D: TDFE;
begin
  tmp := TON_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(Passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_AuthM(UserName_, Passwd_: U_String; OnResult: TON_Usr_AuthM);
var
  tmp: TON_Usr_Auth;
  D: TDFE;
begin
  tmp := TON_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(Passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_AuthP(UserName_, Passwd_: U_String; OnResult: TON_Usr_AuthP);
var
  tmp: TON_Usr_Auth;
  D: TDFE;
begin
  tmp := TON_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(Passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ChangePasswordC(UserName_, Passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordC);
var
  tmp: TON_Usr_ChangePassword;
  D: TDFE;
begin
  tmp := TON_Usr_ChangePassword.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(Passwd_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_ChangePassword', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ChangePasswordM(UserName_, Passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordM);
var
  tmp: TON_Usr_ChangePassword;
  D: TDFE;
begin
  tmp := TON_Usr_ChangePassword.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(Passwd_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_ChangePassword', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_ChangePasswordP(UserName_, Passwd_, NewPasswd_: U_String; OnResult: TON_Usr_ChangePasswordP);
var
  tmp: TON_Usr_ChangePassword;
  D: TDFE;
begin
  tmp := TON_Usr_ChangePassword.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(Passwd_);
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
RegisterC40('UserDB0', TDTC40_UserDB_Service, TDTC40_UserDB_Client);
RegisterC40('UserDB1', TDTC40_UserDB_Service, TDTC40_UserDB_Client);
RegisterC40('UserDB2', TDTC40_UserDB_Service, TDTC40_UserDB_Client);
RegisterC40('UserDB3', TDTC40_UserDB_Service, TDTC40_UserDB_Client);
RegisterC40('UserDB4', TDTC40_UserDB_Service, TDTC40_UserDB_Client);
RegisterC40('UserDB5', TDTC40_UserDB_Service, TDTC40_UserDB_Client);
RegisterC40('UserDB6', TDTC40_UserDB_Service, TDTC40_UserDB_Client);
RegisterC40('UserDB7', TDTC40_UserDB_Service, TDTC40_UserDB_Client);
RegisterC40('UserDB8', TDTC40_UserDB_Service, TDTC40_UserDB_Client);
RegisterC40('UserDB9', TDTC40_UserDB_Service, TDTC40_UserDB_Client);

end.
