{ ****************************************************************************** }
{ * single tunnel IO framework(incl auth service)                              * }
{ * written by QQ 600585@qq.com                                                * }
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
(*
  update history
*)

unit CommunicationFrameworkIO;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, ListEngine, UnicodeMixedLib,
  DataFrameEngine, MemoryStream64, CommunicationFramework, TextDataEngine,
  DoStatusIO, PascalStrings, CoreCipher;

type
  TCommunicationFramework_UserAuthService = class;

  TPeerClientUserDefineForIO = class(TPeerClientUserDefine)
  public
    UserFlag, UserID: SystemString;
    UserPath: SystemString;
    UserConfigFile: TSectionTextData;
    UserAuthService: TCommunicationFramework_UserAuthService;
    UserDBIntf: THashVariantList;
    LoginSuccessed: Boolean;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    function MakeFilePath(fn: SystemString): SystemString;
    function GetUserID: SystemString;
  end;

  TCommunicationFramework_UserAuthService = class(TCoreClassObject)
  protected
    FRootPath: SystemString;
    FUserDB: TSectionTextData;
    FCanRegisterNewUser: Boolean;
    FCanSaveUserInfo: Boolean;
    FLoginUserList: THashVariantList;
  protected
    procedure Command_UserLogin(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_RegisterUser(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
  public
    Communication: TCommunicationFramework;

    constructor Create(ACommunication: TCommunicationFramework);
    destructor Destroy; override;

    procedure SwitchServiceAsMaxPerformance;
    procedure SwitchServiceAsMaxSafe;
    procedure SwitchServiceAsDefaultPerformance;

    procedure Progress; virtual;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    function MakeUserFlag: SystemString;
    function GetUserDefineClient(cli: TPeerIO): TPeerClientUserDefineForIO;

    property CanRegisterNewUser: Boolean read FCanRegisterNewUser write FCanRegisterNewUser;
    property RootPath: SystemString read FRootPath;
    property CanSaveUserInfo: Boolean read FCanSaveUserInfo write FCanSaveUserInfo;
    property LoginUserList: THashVariantList read FLoginUserList;
  end;

  TCommunicationFramework_UserAuthClient = class(TCoreClassObject)
  public
    Client: TCommunicationFrameworkClient;

    constructor Create(ACommunication: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure SwitchServiceAsMaxPerformance;
    procedure SwitchServiceAsMaxSafe;
    procedure SwitchServiceAsDefaultPerformance;

    procedure Progress; virtual;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    function UserLogin(UserID, passwd: SystemString): Boolean;
    function RegisterUser(UserID, passwd: SystemString): Boolean;
  end;

implementation

uses SysUtils;

const
  C_UserLogin = 'UserLogin';
  C_RegisterUser = 'RegisterUser';

constructor TPeerClientUserDefineForIO.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  UserFlag := '';
  UserID := '';
  UserPath := '';
  UserConfigFile := TSectionTextData.Create;
  UserAuthService := nil;
  UserDBIntf := nil;
  LoginSuccessed := False;
end;

destructor TPeerClientUserDefineForIO.Destroy;
begin
  if LoginSuccessed then
    begin
      UserAuthService.FLoginUserList.Delete(UserID);
    end;

  try
      DisposeObject(UserConfigFile);
  except
  end;

  inherited Destroy;
end;

function TPeerClientUserDefineForIO.MakeFilePath(fn: SystemString): SystemString;
begin
  Result := umlCombineFileName(UserPath, fn);
end;

function TPeerClientUserDefineForIO.GetUserID: SystemString;
begin
  Result := UserConfigFile.GetDefaultValue('UserInfo', 'UserID', '');
end;

procedure TCommunicationFramework_UserAuthService.Command_UserLogin(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserID, UserPasswd: SystemString;
  UserDefineIO: TPeerClientUserDefineForIO;
begin
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;

  if not FUserDB.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user name Invalid:%s', [UserID]));
      Exit;
    end;

  if FLoginUserList.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user already online:%s', [UserID]));
      Exit;
    end;

  if not CompareQuantumCryptographyPassword(UserPasswd, SystemString(FUserDB.GetDefaultValue(UserID, 'password', ''))) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('password error', []));
      Exit;
    end;

  UserDefineIO := GetUserDefineClient(Sender);

  UserDefineIO.UserDBIntf := FUserDB.VariantList[UserID];
  UserDefineIO.UserFlag := UserDefineIO.UserDBIntf['UserFlag'];
  UserDefineIO.UserID := UserID;
  UserDefineIO.UserPath := umlCombinePath(FRootPath, UserDefineIO.UserFlag);
  UserDefineIO.UserAuthService := Self;
  UserDefineIO.LoginSuccessed := True;

  if umlFileExists(UserDefineIO.MakeFilePath('User.Config')) then
      UserDefineIO.UserConfigFile.LoadFromFile(UserDefineIO.MakeFilePath('User.Config'));
  UserDefineIO.UserConfigFile.Hit['UserInfo', 'UserID'] := UserID;

  FLoginUserList[UserID] := Now;

  OutData.WriteBool(True);
  OutData.WriteString(Format('success Login:%s', [UserID]));
  OutData.WriteString(UserDefineIO.UserFlag);
end;

procedure TCommunicationFramework_UserAuthService.Command_RegisterUser(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserID, UserPasswd: SystemString;
  UserDefineIO: TPeerClientUserDefineForIO;
begin
  if not FCanRegisterNewUser then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('disable user register in server', []));
      Exit;
    end;
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;

  if umlExistsChar(UserID, '[]:'#13#10#9#8#0) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user name Illegal:%s', [UserID]));
      Exit;
    end;

  if FUserDB.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user already registed:%s', [UserID]));
      Exit;
    end;

  if FLoginUserList.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user already online:%s', [UserID]));
      Exit;
    end;

  UserDefineIO := GetUserDefineClient(Sender);
  UserDefineIO.UserFlag := MakeUserFlag;
  UserDefineIO.UserID := UserID;
  UserDefineIO.UserPath := umlCombinePath(FRootPath, UserDefineIO.UserFlag);
  umlCreateDirectory(UserDefineIO.UserPath);
  UserDefineIO.UserDBIntf := FUserDB.VariantList[UserID];
  UserDefineIO.UserDBIntf['UserFlag'] := UserDefineIO.UserFlag;
  UserDefineIO.UserDBIntf['password'] := GenerateQuantumCryptographyPassword(UserPasswd).Text;
  UserDefineIO.UserAuthService := Self;
  UserDefineIO.LoginSuccessed := True;

  if umlFileExists(UserDefineIO.MakeFilePath('User.Config')) then
      UserDefineIO.UserConfigFile.LoadFromFile(UserDefineIO.MakeFilePath('User.Config'));
  UserDefineIO.UserConfigFile.Hit['UserInfo', 'UserID'] := UserID;

  OutData.WriteBool(True);
  OutData.WriteString(Format('success registed:%s', [UserID]));
  OutData.WriteString(UserDefineIO.UserFlag);

  if FCanSaveUserInfo then
      FUserDB.SaveToFile(umlCombineFileName(FRootPath, 'UserDB'));

  FLoginUserList[UserID] := Now;
end;

constructor TCommunicationFramework_UserAuthService.Create(ACommunication: TCommunicationFramework);
begin
  inherited Create;
  Communication := ACommunication;
  FRootPath := umlCurrentPath;
  FUserDB := TSectionTextData.Create;
  FCanRegisterNewUser := True;
  FCanSaveUserInfo := False;
  FLoginUserList := THashVariantList.Create;

  if umlFileExists(umlCombineFileName(FRootPath, 'UserDB')) then
      FUserDB.LoadFromFile(umlCombineFileName(FRootPath, 'UserDB'));

  SwitchServiceAsDefaultPerformance;
end;

destructor TCommunicationFramework_UserAuthService.Destroy;
begin
  if FCanSaveUserInfo then
      FUserDB.SaveToFile(umlCombineFileName(FRootPath, 'UserDB'));
  DisposeObject(FLoginUserList);
  DisposeObject(FUserDB);
  inherited Destroy;
end;

procedure TCommunicationFramework_UserAuthService.SwitchServiceAsMaxPerformance;
begin
  Communication.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_UserAuthService.SwitchServiceAsMaxSafe;
begin
  Communication.SwitchMaxSecurity;
end;

procedure TCommunicationFramework_UserAuthService.SwitchServiceAsDefaultPerformance;
begin
  Communication.SwitchDefaultPerformance;
end;

procedure TCommunicationFramework_UserAuthService.Progress;
begin
  Communication.Progress;
end;

procedure TCommunicationFramework_UserAuthService.RegisterCommand;
begin
  Communication.PeerClientUserDefineClass := TPeerClientUserDefineForIO;

{$IFDEF FPC}
  Communication.RegisterStream(C_UserLogin).OnExecute := @Command_UserLogin;
  Communication.RegisterStream(C_RegisterUser).OnExecute := @Command_RegisterUser;
{$ELSE}
  Communication.RegisterStream(C_UserLogin).OnExecute := Command_UserLogin;
  Communication.RegisterStream(C_RegisterUser).OnExecute := Command_RegisterUser;
{$ENDIF}
end;

procedure TCommunicationFramework_UserAuthService.UnRegisterCommand;
begin
  Communication.DeleteRegistedCMD(C_UserLogin);
  Communication.DeleteRegistedCMD(C_RegisterUser);
  Communication.PeerClientUserDefineClass := TPeerClientUserDefine;
end;

function TCommunicationFramework_UserAuthService.MakeUserFlag: SystemString;
var
  d: Double;
  p: PInt64;
begin
  repeat
    TCoreClassThread.Sleep(1);
    d := Now;
    p := @d;
    Result := IntToHex(p^, 16);
  until not umlDirectoryExists(umlCombinePath(FRootPath, Result));
end;

function TCommunicationFramework_UserAuthService.GetUserDefineClient(cli: TPeerIO): TPeerClientUserDefineForIO;
begin
  Result := cli.UserDefine as TPeerClientUserDefineForIO;
end;

constructor TCommunicationFramework_UserAuthClient.Create(ACommunication: TCommunicationFrameworkClient);
begin
  inherited Create;
  Client := ACommunication;
  SwitchServiceAsMaxSafe;
end;

destructor TCommunicationFramework_UserAuthClient.Destroy;
begin
  inherited Destroy;
end;

procedure TCommunicationFramework_UserAuthClient.SwitchServiceAsMaxPerformance;
begin
  Client.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_UserAuthClient.SwitchServiceAsMaxSafe;
begin
  Client.SwitchMaxSecurity;
end;

procedure TCommunicationFramework_UserAuthClient.SwitchServiceAsDefaultPerformance;
begin
  Client.SwitchDefaultPerformance;
end;

procedure TCommunicationFramework_UserAuthClient.Progress;
begin
  Client.Progress;
end;

procedure TCommunicationFramework_UserAuthClient.RegisterCommand;
begin
end;

procedure TCommunicationFramework_UserAuthClient.UnRegisterCommand;
begin
end;

function TCommunicationFramework_UserAuthClient.UserLogin(UserID, passwd: SystemString): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);
  Client.WaitSendStreamCmd(C_UserLogin, sendDE, resDE, 10000);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      DoStatus(resDE.ReadString(1));
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_UserAuthClient.RegisterUser(UserID, passwd: SystemString): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);
  Client.WaitSendStreamCmd(C_RegisterUser, sendDE, resDE, 10000);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      DoStatus(resDE.ReadString(1));
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

end.
