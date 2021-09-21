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
    procedure cmd_Usr_Find(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Reg(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Get(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Post(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Remove(sender: TPeerIO; InData, OutData: TDFE);
  public
    UserIdentifierHash: TJsonHashList;
    UserJsonList: TZJL;
    DTC40_UserDB_FileName: U_string;

    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_string); override;
    destructor Destroy; override;
    procedure SafeCheck; override;

    function MatchIdentifier(Identifier_: U_string; j: TZJ): Boolean;
    function ExistsIdentifier(j: TZJ): Boolean;
    function ExtractJsonToHash(j: TZJ): Boolean;
    procedure LoadUserDB;
    procedure SaveUserDBAsDFE(DFE: TDFE);
    procedure CleanLoseJson;
  end;

  TON_Usr_FindC = procedure(sender: TDTC40_UserDB_Client; List: TZJL);
  TON_Usr_FindM = procedure(sender: TDTC40_UserDB_Client; List: TZJL) of object;
{$IFDEF FPC}
  TON_Usr_FindP = procedure(sender: TDTC40_UserDB_Client; List: TZJL) is nested;
{$ELSE FPC}
  TON_Usr_FindP = reference to procedure(sender: TDTC40_UserDB_Client; List: TZJL);
{$ENDIF FPC}

  TOnUsrFind = class(TOnResultBridge)
  public
    Client: TDTC40_UserDB_Client;
    OnResultC: TON_Usr_FindC;
    OnResultM: TON_Usr_FindM;
    OnResultP: TON_Usr_FindP;
    constructor Create; override;
    procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TON_Usr_RegC = procedure(sender: TDTC40_UserDB_Client; States: TArrayBool);
  TON_Usr_RegM = procedure(sender: TDTC40_UserDB_Client; States: TArrayBool) of object;
{$IFDEF FPC}
  TON_Usr_RegP = procedure(sender: TDTC40_UserDB_Client; States: TArrayBool) is nested;
{$ELSE FPC}
  TON_Usr_RegP = reference to procedure(sender: TDTC40_UserDB_Client; States: TArrayBool);
{$ENDIF FPC}

  TOnUsrReg = class(TOnResultBridge)
  public
    Client: TDTC40_UserDB_Client;
    OnResultC: TON_Usr_RegC;
    OnResultM: TON_Usr_RegM;
    OnResultP: TON_Usr_RegP;
    constructor Create; override;
    procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TON_Usr_GetC = procedure(sender: TDTC40_UserDB_Client; List: TZJL);
  TON_Usr_GetM = procedure(sender: TDTC40_UserDB_Client; List: TZJL) of object;
{$IFDEF FPC}
  TON_Usr_GetP = procedure(sender: TDTC40_UserDB_Client; List: TZJL) is nested;
{$ELSE FPC}
  TON_Usr_GetP = reference to procedure(sender: TDTC40_UserDB_Client; List: TZJL);
{$ENDIF FPC}

  TOnUsrGet = class(TOnResultBridge)
  public
    Client: TDTC40_UserDB_Client;
    OnResultC: TON_Usr_GetC;
    OnResultM: TON_Usr_GetM;
    OnResultP: TON_Usr_GetP;
    constructor Create; override;
    procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TON_Usr_PostC = procedure(sender: TDTC40_UserDB_Client; States: TArrayBool);
  TON_Usr_PostM = procedure(sender: TDTC40_UserDB_Client; States: TArrayBool) of object;
{$IFDEF FPC}
  TON_Usr_PostP = procedure(sender: TDTC40_UserDB_Client; States: TArrayBool) is nested;
{$ELSE FPC}
  TON_Usr_PostP = reference to procedure(sender: TDTC40_UserDB_Client; States: TArrayBool);
{$ENDIF FPC}

  TOnUsrPost = class(TOnResultBridge)
  public
    Client: TDTC40_UserDB_Client;
    OnResultC: TON_Usr_PostC;
    OnResultM: TON_Usr_PostM;
    OnResultP: TON_Usr_PostP;
    constructor Create; override;
    procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TON_Usr_RemoveC = procedure(sender: TDTC40_UserDB_Client; States: TArrayBool);
  TON_Usr_RemoveM = procedure(sender: TDTC40_UserDB_Client; States: TArrayBool) of object;
{$IFDEF FPC}
  TON_Usr_RemoveP = procedure(sender: TDTC40_UserDB_Client; States: TArrayBool) is nested;
{$ELSE FPC}
  TON_Usr_RemoveP = reference to procedure(sender: TDTC40_UserDB_Client; States: TArrayBool);
{$ENDIF FPC}

  TOnUsrRemove = class(TOnResultBridge)
  public
    Client: TDTC40_UserDB_Client;
    OnResultC: TON_Usr_RemoveC;
    OnResultM: TON_Usr_RemoveM;
    OnResultP: TON_Usr_RemoveP;
    constructor Create; override;
    procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TDTC40_UserDB_Client = class(TDTC40_Base_NoAuth_Client)
  public
    constructor Create(source_: TDTC40_Info; Param_: U_string); override;
    destructor Destroy; override;

    procedure Usr_FindC(Identifier_: SystemString; MaxResult: Integer; OnResult: TON_Usr_FindC);
    procedure Usr_FindM(Identifier_: SystemString; MaxResult: Integer; OnResult: TON_Usr_FindM);
    procedure Usr_FindP(Identifier_: SystemString; MaxResult: Integer; OnResult: TON_Usr_FindP);

    procedure Usr_RegC(L: TZJL; OnResult: TON_Usr_RegC);
    procedure Usr_RegM(L: TZJL; OnResult: TON_Usr_RegM);
    procedure Usr_RegP(L: TZJL; OnResult: TON_Usr_RegP);

    procedure Usr_GetC(Identifier_: U_StringArray; OnResult: TON_Usr_GetC);
    procedure Usr_GetM(Identifier_: U_StringArray; OnResult: TON_Usr_GetM);
    procedure Usr_GetP(Identifier_: U_StringArray; OnResult: TON_Usr_GetP);

    procedure Usr_PostC(L: TZJL; OnResult: TON_Usr_PostC);
    procedure Usr_PostM(L: TZJL; OnResult: TON_Usr_PostM);
    procedure Usr_PostP(L: TZJL; OnResult: TON_Usr_PostP);

    procedure Usr_RemoveC(L: TZJL; OnResult: TON_Usr_RemoveC);
    procedure Usr_RemoveM(L: TZJL; OnResult: TON_Usr_RemoveM);
    procedure Usr_RemoveP(L: TZJL; OnResult: TON_Usr_RemoveP);
  end;

function GetIdentifier(j: TZJ): TZJArry;

implementation

function GetIdentifier(j: TZJ): TZJArry;
begin
  Result := j.A['Identifier'];
end;

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

procedure TDTC40_UserDB_Service.cmd_Usr_Find(sender: TPeerIO; InData, OutData: TDFE);
var
  Identifier_: SystemString;
  MaxResult: Integer;
  L: TZJL;

{$IFDEF FPC}
  procedure fpc_Progress_(const Name: PSystemString; Obj: TZJ);
  begin
    if (MaxResult <= 0) or (L.Count < MaxResult) then
      if MatchIdentifier(Identifier_, Obj) then
        if not L.IndexOf(Obj) < 0 then
            L.Add(Obj);
  end;
{$ENDIF FPC}


var
  i: Integer;
begin
  L := TZJL.Create(False);
  Identifier_ := InData.R.ReadString;
  MaxResult := InData.R.ReadInteger;

{$IFDEF FPC}
  UserIdentifierHash.ProgressP(@fpc_Progress_);
{$ELSE FPC}
  UserIdentifierHash.ProgressP(
    procedure(const Name: PSystemString; Obj: TZJ)
    begin
      if (MaxResult <= 0) or (L.Count < MaxResult) then
        if MatchIdentifier(Identifier_, Obj) then
          if not L.IndexOf(Obj) < 0 then
              L.Add(Obj);
    end);
{$ENDIF FPC}
  for i := 0 to L.Count - 1 do
      OutData.WriteJson(L[i]);
  DisposeObject(L);
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Reg(sender: TPeerIO; InData, OutData: TDFE);
var
  j: TZJ;
begin
  while InData.R.NotEnd do
    begin
      j := TZJ.Create;
      InData.R.ReadJson(j);

      if (not ExistsIdentifier(j)) and ExtractJsonToHash(j) then
        begin
          UserJsonList.Add(j);
          OutData.WriteBool(True);
        end
      else
        begin
          OutData.WriteBool(False);
          DisposeObject(j);
        end;
    end;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Get(sender: TPeerIO; InData, OutData: TDFE);
var
  Identifier_: U_string;
begin
  while InData.R.NotEnd do
    begin
      Identifier_ := InData.R.ReadString;
      if UserIdentifierHash.Exists(Identifier_) then
        begin
          OutData.WriteJson(UserIdentifierHash[Identifier_]);
        end
      else
        begin
          OutData.WriteBool(False);
        end;
    end;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Post(sender: TPeerIO; InData, OutData: TDFE);
var
  j: TZJ;
begin
  while InData.R.NotEnd do
    begin
      j := TZJ.Create;
      InData.R.ReadJson(j);
      if ExtractJsonToHash(j) then
        begin
          UserJsonList.Add(j);
          OutData.WriteBool(True);
        end
      else
        begin
          OutData.WriteBool(False);
          DisposeObject(j);
        end;
    end;
end;

procedure TDTC40_UserDB_Service.cmd_Usr_Remove(sender: TPeerIO; InData, OutData: TDFE);
var
  j: TZJ;
  arry: TZJArry;
  i: Integer;
  found_: Integer;
begin
  while InData.R.NotEnd do
    begin
      j := TZJ.Create;
      InData.R.ReadJson(j);
      arry := GetIdentifier(j);
      found_ := 0;
      for i := 0 to arry.Count - 1 do
        if (arry.S[i] <> '') and UserIdentifierHash.Exists(arry.S[i]) then
          begin
            UserIdentifierHash.Delete(arry.S[i]);
            inc(found_);
          end;
      OutData.WriteBool(found_ > 0);
      DisposeObject(j);
    end;
end;

constructor TDTC40_UserDB_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_string);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  ServiceInfo.OnlyInstance := True;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Find').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Find;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Reg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Reg;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Get').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Get;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Post').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Post;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Remove').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Remove;
  UpdateToGlobalDispatch;

  UserIdentifierHash := TJsonHashList.Create(False, 1024 * 1024, nil);
  UserIdentifierHash.AccessOptimization := True;
  UserIdentifierHash.IgnoreCase := False;
  UserJsonList := TZJL.Create(True);
  DTC40_UserDB_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.DFE', [ServiceInfo.ServiceTyp.Text]));
  DoLoading;
end;

destructor TDTC40_UserDB_Service.Destroy;
begin
  DisposeObject(UserIdentifierHash);
  DisposeObject(UserJsonList);
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

function TDTC40_UserDB_Service.MatchIdentifier(Identifier_: U_string; j: TZJ): Boolean;
var
  arry: TZJArry;
  i: Integer;
begin
  Result := True;
  arry := GetIdentifier(j);
  for i := 0 to arry.Count - 1 do
    if umlMultipleMatch(True, Identifier_, arry.S[i]) then
        exit;
  Result := False;
end;

function TDTC40_UserDB_Service.ExistsIdentifier(j: TZJ): Boolean;
var
  arry: TZJArry;
  i: Integer;
begin
  Result := True;
  arry := GetIdentifier(j);
  for i := 0 to arry.Count - 1 do
    if (arry.S[i] <> '') and UserIdentifierHash.Exists(arry.S[i]) then
        exit;
  Result := False;
end;

function TDTC40_UserDB_Service.ExtractJsonToHash(j: TZJ): Boolean;
var
  arry: TZJArry;
  i: Integer;
  tmp: TZJ;
begin
  Result := False;
  arry := GetIdentifier(j);
  if arry.Count = 0 then
      exit;

  for i := 0 to arry.Count - 1 do
    if (arry.S[i] <> '') then
      begin
        tmp := UserIdentifierHash[arry.S[i]];
        if (tmp <> nil) then
            tmp.Assign(j)
        else
            UserIdentifierHash.Add(arry.S[i], j);
      end;

  Result := True;
end;

procedure TDTC40_UserDB_Service.LoadUserDB;
var
  D: TDFE;
  j: TZJ;
begin
  if not umlFileExists(DTC40_UserDB_FileName) then
      exit;

  UserJsonList.Clear;
  UserIdentifierHash.Clear;

  D := TDFE.Create;

  try
    DoStatus('Load user database "%s"', [DTC40_UserDB_FileName.Text]);
    D.LoadFromFile(DTC40_UserDB_FileName);

    DoStatus('extract user Database.');
    while D.R.NotEnd do
      begin
        j := TZJ.Create;
        D.R.ReadJson(j);
        if not ExtractJsonToHash(j) then
            DTC40PhysicsService.PhysicsTunnel.Error('error json'#13#10'%s', [j.ToJSONString(True).Text]);
      end;
    DoStatus('extract user Database done.');
  except
  end;

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Service.SaveUserDBAsDFE(DFE: TDFE);
{$IFDEF FPC}
  procedure fpc_Progress_(const Name: PSystemString; Obj: TZJ);
  begin
    if ExistsIdentifier(Obj) then
        Obj.Tag := 1;
  end;
{$ENDIF FPC}


var
  i: Integer;
begin
  for i := 0 to UserJsonList.Count - 1 do
      UserJsonList[i].Tag := 0;

{$IFDEF FPC}
  UserIdentifierHash.ProgressP(@fpc_Progress_);
{$ELSE FPC}
  UserIdentifierHash.ProgressP(
    procedure(const Name: PSystemString; Obj: TZJ)
    begin
      if ExistsIdentifier(Obj) then
          Obj.Tag := 1;
    end);
{$ENDIF FPC}
  for i := 0 to UserJsonList.Count - 1 do
    if UserJsonList[i].Tag = 1 then
        DFE.WriteJson(UserJsonList[i]);
end;

procedure TDTC40_UserDB_Service.CleanLoseJson;
{$IFDEF FPC}
  procedure fpc_Progress_(const Name: PSystemString; Obj: TZJ);
  begin
    if ExistsIdentifier(Obj) then
        Obj.Tag := 1;
  end;
{$ENDIF FPC}


var
  i: Integer;
begin
  for i := 0 to UserJsonList.Count - 1 do
      UserJsonList[i].Tag := 0;

{$IFDEF FPC}
  UserIdentifierHash.ProgressP(@fpc_Progress_);
{$ELSE FPC}
  UserIdentifierHash.ProgressP(
    procedure(const Name: PSystemString; Obj: TZJ)
    begin
      if ExistsIdentifier(Obj) then
          Obj.Tag := 1;
    end);
{$ENDIF FPC}
  i := 0;
  while i < UserJsonList.Count do
    if UserJsonList[i].Tag <> 1 then
      begin
        UserJsonList.Delete(i);
      end
    else
      begin
        UserJsonList[i].Tag := 0;
        inc(i);
      end;
end;

constructor TOnUsrFind.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TOnUsrFind.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  tmp: TZJL;
  j: TZJ;
begin
  tmp := TZJL.Create(True);
  while Result_.R.NotEnd do
    begin
      j := TZJ.Create;
      Result_.R.ReadJson(j);
      tmp.Add(j);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp);
    if Assigned(OnResultM) then
        OnResultM(Client, tmp);
    if Assigned(OnResultP) then
        OnResultP(Client, tmp);
  except
  end;

  DelayFreeObject(1.0, self, tmp);
end;

procedure TOnUsrFind.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil);
    if Assigned(OnResultM) then
        OnResultM(Client, nil);
    if Assigned(OnResultP) then
        OnResultP(Client, nil);
  except
  end;

  DelayFreeObject(1.0, self);
end;

constructor TOnUsrReg.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TOnUsrReg.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  tmp: TArrayBool;
  i: Integer;
begin
  SetLength(tmp, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      tmp[i] := Result_.ReadBool(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp);
    if Assigned(OnResultM) then
        OnResultM(Client, tmp);
    if Assigned(OnResultP) then
        OnResultP(Client, tmp);
  except
  end;

  DelayFreeObject(1.0, self);
end;

procedure TOnUsrReg.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil);
    if Assigned(OnResultM) then
        OnResultM(Client, nil);
    if Assigned(OnResultP) then
        OnResultP(Client, nil);
  except
  end;

  DelayFreeObject(1.0, self);
end;

constructor TOnUsrGet.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TOnUsrGet.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  tmp: TZJL;
  j: TZJ;
begin
  tmp := TZJL.Create(True);
  while Result_.R.NotEnd do
    begin
      j := TZJ.Create;
      Result_.R.ReadJson(j);
      tmp.Add(j);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp);
    if Assigned(OnResultM) then
        OnResultM(Client, tmp);
    if Assigned(OnResultP) then
        OnResultP(Client, tmp);
  except
  end;

  DelayFreeObject(1.0, self, tmp);
end;

procedure TOnUsrGet.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil);
    if Assigned(OnResultM) then
        OnResultM(Client, nil);
    if Assigned(OnResultP) then
        OnResultP(Client, nil);
  except
  end;

  DelayFreeObject(1.0, self);
end;

constructor TOnUsrPost.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TOnUsrPost.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  tmp: TArrayBool;
  i: Integer;
begin
  SetLength(tmp, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      tmp[i] := Result_.ReadBool(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp);
    if Assigned(OnResultM) then
        OnResultM(Client, tmp);
    if Assigned(OnResultP) then
        OnResultP(Client, tmp);
  except
  end;

  DelayFreeObject(1.0, self);
end;

procedure TOnUsrPost.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil);
    if Assigned(OnResultM) then
        OnResultM(Client, nil);
    if Assigned(OnResultP) then
        OnResultP(Client, nil);
  except
  end;

  DelayFreeObject(1.0, self);
end;

constructor TOnUsrRemove.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TOnUsrRemove.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  tmp: TArrayBool;
  i: Integer;
begin
  SetLength(tmp, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      tmp[i] := Result_.ReadBool(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp);
    if Assigned(OnResultM) then
        OnResultM(Client, tmp);
    if Assigned(OnResultP) then
        OnResultP(Client, tmp);
  except
  end;

  DelayFreeObject(1.0, self);
end;

procedure TOnUsrRemove.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil);
    if Assigned(OnResultM) then
        OnResultM(Client, nil);
    if Assigned(OnResultP) then
        OnResultP(Client, nil);
  except
  end;

  DelayFreeObject(1.0, self);
end;

constructor TDTC40_UserDB_Client.Create(source_: TDTC40_Info; Param_: U_string);
begin
  inherited Create(source_, Param_);
end;

destructor TDTC40_UserDB_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TDTC40_UserDB_Client.Usr_FindC(Identifier_: SystemString; MaxResult: Integer; OnResult: TON_Usr_FindC);
var
  D: TDFE;
  tmp: TOnUsrFind;
begin
  D := TDFE.Create;
  D.WriteString(Identifier_);
  D.WriteInteger(MaxResult);

  tmp := TOnUsrFind.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Find', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_FindM(Identifier_: SystemString; MaxResult: Integer; OnResult: TON_Usr_FindM);
var
  D: TDFE;
  tmp: TOnUsrFind;
begin
  D := TDFE.Create;
  D.WriteString(Identifier_);
  D.WriteInteger(MaxResult);

  tmp := TOnUsrFind.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Find', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_FindP(Identifier_: SystemString; MaxResult: Integer; OnResult: TON_Usr_FindP);
var
  D: TDFE;
  tmp: TOnUsrFind;
begin
  D := TDFE.Create;
  D.WriteString(Identifier_);
  D.WriteInteger(MaxResult);

  tmp := TOnUsrFind.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Find', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RegC(L: TZJL; OnResult: TON_Usr_RegC);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrReg;
begin
  D := TDFE.Create;
  for i := 0 to L.Count - 1 do
      D.WriteJson(L[i]);

  tmp := TOnUsrReg.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RegM(L: TZJL; OnResult: TON_Usr_RegM);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrReg;
begin
  D := TDFE.Create;
  for i := 0 to L.Count - 1 do
      D.WriteJson(L[i]);

  tmp := TOnUsrReg.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RegP(L: TZJL; OnResult: TON_Usr_RegP);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrReg;
begin
  D := TDFE.Create;
  for i := 0 to L.Count - 1 do
      D.WriteJson(L[i]);

  tmp := TOnUsrReg.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetC(Identifier_: U_StringArray; OnResult: TON_Usr_GetC);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrGet;
begin
  D := TDFE.Create;
  for i := 0 to length(Identifier_) - 1 do
      D.WriteString(Identifier_[i]);

  tmp := TOnUsrGet.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetM(Identifier_: U_StringArray; OnResult: TON_Usr_GetM);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrGet;
begin
  D := TDFE.Create;
  for i := 0 to length(Identifier_) - 1 do
      D.WriteString(Identifier_[i]);

  tmp := TOnUsrGet.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_GetP(Identifier_: U_StringArray; OnResult: TON_Usr_GetP);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrGet;
begin
  D := TDFE.Create;
  for i := 0 to length(Identifier_) - 1 do
      D.WriteString(Identifier_[i]);

  tmp := TOnUsrGet.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_PostC(L: TZJL; OnResult: TON_Usr_PostC);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrPost;
begin
  D := TDFE.Create;
  for i := 0 to L.Count - 1 do
      D.WriteJson(L[i]);

  tmp := TOnUsrPost.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Post', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_PostM(L: TZJL; OnResult: TON_Usr_PostM);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrPost;
begin
  D := TDFE.Create;
  for i := 0 to L.Count - 1 do
      D.WriteJson(L[i]);

  tmp := TOnUsrPost.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Post', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_PostP(L: TZJL; OnResult: TON_Usr_PostP);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrPost;
begin
  D := TDFE.Create;
  for i := 0 to L.Count - 1 do
      D.WriteJson(L[i]);

  tmp := TOnUsrPost.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Post', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RemoveC(L: TZJL; OnResult: TON_Usr_RemoveC);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrRemove;
begin
  D := TDFE.Create;
  for i := 0 to L.Count - 1 do
      D.WriteJson(L[i]);

  tmp := TOnUsrRemove.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Remove', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RemoveM(L: TZJL; OnResult: TON_Usr_RemoveM);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrRemove;
begin
  D := TDFE.Create;
  for i := 0 to L.Count - 1 do
      D.WriteJson(L[i]);

  tmp := TOnUsrRemove.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Remove', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

  DisposeObject(D);
end;

procedure TDTC40_UserDB_Client.Usr_RemoveP(L: TZJL; OnResult: TON_Usr_RemoveP);
var
  D: TDFE;
  i: Integer;
  tmp: TOnUsrRemove;
begin
  D := TDFE.Create;
  for i := 0 to L.Count - 1 do
      D.WriteJson(L[i]);

  tmp := TOnUsrRemove.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Remove', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);

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
