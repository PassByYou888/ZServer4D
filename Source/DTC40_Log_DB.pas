{ ****************************************************************************** }
{ * cloud 4.0 log database                                                     * }
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
unit DTC40_Log_DB;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib, ListEngine,
  Geometry2DUnit, DataFrameEngine, ZJson, zExpression,
  NotifyObjectBase, CoreCipher, MemoryStream64,
  ZDB2_Core, ZDB2_HS, GHashList,
  CommunicationFramework, PhysicsIO, CommunicationFrameworkDoubleTunnelIO_NoAuth, DTC40;

type
  TDTC40_Log_DB_Service = class(TDTC40_Base_NoAuth_Service)
  private type
    TDTC40_ZDB2_List_HashString = class(TZDB2_List_HashString)
    protected
      Name: U_String;
      LastActivtTime: TTimeTick;
      LastPostTime: TDateTime;
    end;

    TLog_DB_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TDTC40_ZDB2_List_HashString>;
    TLog_DB_List = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericsList<TDTC40_ZDB2_List_HashString>;
  private
    procedure cmd_PostLog(Sender: TPeerIO; InData: TDFE);
    procedure cmd_QueryLog(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_QueryAndRemoveLog(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RemoveLog(Sender: TPeerIO; InData: TDFE);
    procedure cmd_GetLogDB(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_CloseDB(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RemoveDB(Sender: TPeerIO; InData: TDFE);
  private
    WaitFreeList: TLog_DB_List;
    procedure Do_Create_ZDB2_HashString(Sender: TZDB2_List_HashString; Obj: TZDB2_HashString);
    procedure Do_DB_Pool_SafeCheck(const Name_: PSystemString; Obj_: TDTC40_ZDB2_List_HashString);
    procedure Do_DB_Pool_Progress(const Name_: PSystemString; Obj_: TDTC40_ZDB2_List_HashString);
  public
    DTC40_DB_Directory: U_String;
    DB_Pool: TLog_DB_Pool;
    LogDBRecycleMemoryTimeOut: TTimeTick;
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;

    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;

    function GetDB(const DBName_: SystemString): TDTC40_ZDB2_List_HashString;
    procedure PostLog(const DBName_, Log1_, Log2_: SystemString);
  end;

  TLogData__ = record
    LogTime: TDateTime;
    Log1, Log2: SystemString;
    Index: Integer;
  end;

  TArrayLogData = array of TLogData__;

  TDTC40_Log_DB_Client = class(TDTC40_Base_NoAuth_Client)
  public type

    TON_QueryLogC = procedure(Sender: TDTC40_Log_DB_Client; var arry: TArrayLogData);
    TON_QueryLogM = procedure(Sender: TDTC40_Log_DB_Client; var arry: TArrayLogData) of object;
{$IFDEF FPC}
    TON_QueryLogP = procedure(Sender: TDTC40_Log_DB_Client; var arry: TArrayLogData) is nested;
{$ELSE FPC}
    TON_QueryLogP = reference to procedure(Sender: TDTC40_Log_DB_Client; var arry: TArrayLogData);
{$ENDIF FPC}

    TON_QueryLog = class(TOnResultBridge)
    public
      Client: TDTC40_Log_DB_Client;
      OnResultC: TON_QueryLogC;
      OnResultM: TON_QueryLogM;
      OnResultP: TON_QueryLogP;
      constructor Create; override;
      procedure DoStreamEvent(Sender: TPeerIO; Result_: TDFE); override;
    end;

    TON_GetLogDBC = procedure(Sender: TDTC40_Log_DB_Client; arry: U_StringArray);
    TON_GetLogDBM = procedure(Sender: TDTC40_Log_DB_Client; arry: U_StringArray) of object;
{$IFDEF FPC}
    TON_GetLogDBP = procedure(Sender: TDTC40_Log_DB_Client; arry: U_StringArray) is nested;
{$ELSE FPC}
    TON_GetLogDBP = reference to procedure(Sender: TDTC40_Log_DB_Client; arry: U_StringArray);
{$ENDIF FPC}

    TON_GetLogDB = class(TOnResultBridge)
    public
      Client: TDTC40_Log_DB_Client;
      OnResultC: TON_GetLogDBC;
      OnResultM: TON_GetLogDBM;
      OnResultP: TON_GetLogDBP;
      constructor Create; override;
      procedure DoStreamEvent(Sender: TPeerIO; Result_: TDFE); override;
    end;
  public
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;

    procedure PostLog(DBName_, Log1_, Log2_: SystemString); overload;
    procedure PostLog(DBName_, Log_: SystemString); overload;
    procedure QueryLogC(DBName_: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogC);
    procedure QueryLogM(DBName_: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogM);
    procedure QueryLogP(DBName_: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogP);
    procedure QueryAndRemoveLog(DBName_: SystemString; bTime, eTime: TDateTime);
    procedure RemoveLog(DBName_: SystemString; arry_index: array of Integer);
    procedure GetLogDBC(OnResult: TON_GetLogDBC);
    procedure GetLogDBM(OnResult: TON_GetLogDBM);
    procedure GetLogDBP(OnResult: TON_GetLogDBP);
    procedure CloseDB(DBName_: SystemString);
    procedure RemoveDB(DBName_: SystemString);
  end;

implementation

uses DateUtils;

procedure SortLog(var L_: TArrayLogData);
  function Compare_(var Left, Right: TLogData__): ShortInt;
  begin
    Result := CompareDateTime(Left.LogTime, Right.LogTime);
  end;

  procedure fastSort_(var Arry_: TArrayLogData; L, R: Integer);
  var
    i, j: Integer;
    p: ^TLogData__;
    tmp: TLogData__;
  begin
    repeat
      i := L;
      j := R;
      p := @Arry_[(L + R) shr 1];
      repeat
        while Compare_(Arry_[i], p^) < 0 do
            inc(i);
        while Compare_(Arry_[j], p^) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                tmp := Arry_[i];
                Arry_[i] := Arry_[j];
                Arry_[j] := tmp;
              end;
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
  if length(L_) > 1 then
      fastSort_(L_, 0, length(L_) - 1);
end;

procedure TDTC40_Log_DB_Service.cmd_PostLog(Sender: TPeerIO; InData: TDFE);
var
  DBName_, Log1_, Log2_: SystemString;
begin
  DBName_ := InData.R.ReadString;
  Log1_ := InData.R.ReadString;
  Log2_ := InData.R.ReadString;
  PostLog(DBName_, Log1_, Log2_);
end;

procedure TDTC40_Log_DB_Service.cmd_QueryLog(Sender: TPeerIO; InData, OutData: TDFE);
var
  DBName_: SystemString;
  bTime, eTime: TDateTime;
  db_: TDTC40_ZDB2_List_HashString;
  i: Integer;
  d_: TDateTime;
  dt: SystemString;
  n1, n2: SystemString;
begin
  DBName_ := InData.R.ReadString;
  bTime := InData.R.ReadDouble;
  eTime := InData.R.ReadDouble;

  db_ := GetDB(DBName_);
  if db_ = nil then
      exit;

  dt := umlDateTimeToStr(umlNow);
  for i := 0 to db_.Count - 1 do
    begin
      try
        d_ := umlStrToDateTime(db_[i].Data.GetDefaultValue('Time', dt));
        if DateTimeInRange(d_, bTime, eTime) then
          begin
            n1 := db_[i].Data.GetDefaultValue('Log1', '');
            n2 := db_[i].Data.GetDefaultValue('Log2', '');
            OutData.WriteDouble(d_);
            OutData.WriteString(n1);
            OutData.WriteString(n2);
            OutData.WriteInteger(i);
          end
      except
      end;
    end;
end;

procedure TDTC40_Log_DB_Service.cmd_QueryAndRemoveLog(Sender: TPeerIO; InData: TDFE);
var
  DBName_: SystemString;
  bTime, eTime: TDateTime;
  db_: TDTC40_ZDB2_List_HashString;
  i: Integer;
  d_: TDateTime;
  dt: SystemString;
begin
  DBName_ := InData.R.ReadString;
  bTime := InData.R.ReadDouble;
  eTime := InData.R.ReadDouble;

  db_ := GetDB(DBName_);
  if db_ = nil then
      exit;

  dt := umlDateTimeToStr(umlNow);
  i := 0;
  while i < db_.Count do
    begin
      try
        d_ := umlStrToDateTime(db_[i].Data.GetDefaultValue('Time', dt));
        if DateTimeInRange(d_, bTime, eTime) then
          begin
            db_.Delete(i, true);
          end
        else
            inc(i);
      except
      end;
    end;
end;

procedure TDTC40_Log_DB_Service.cmd_RemoveLog(Sender: TPeerIO; InData: TDFE);
var
  DBName_: SystemString;
  arry: TDataFrameArrayInteger;
  db_: TDTC40_ZDB2_List_HashString;
  L: TCoreClassListForObj;
  i: Integer;
begin
  DBName_ := InData.R.ReadString;
  arry := InData.R.ReadArrayInteger;

  db_ := GetDB(DBName_);
  if db_ = nil then
      exit;

  L := TCoreClassListForObj.Create;
  for i := 0 to arry.Count - 1 do
    if (arry[i] >= 0) and (arry[i] < db_.Count) then
        L.Add(db_[arry[i]]);

  i := 0;
  while i < db_.Count do
    if L.IndexOf(db_[i]) >= 0 then
        db_.Delete(i, true)
    else
        inc(i);

  disposeObject(L);
end;

procedure TDTC40_Log_DB_Service.cmd_GetLogDB(Sender: TPeerIO; InData, OutData: TDFE);
var
  fArry: U_StringArray;
  fn: U_SystemString;
begin
  fArry := umlGetFileListWithFullPath(DTC40_DB_Directory);
  for fn in fArry do
    if umlMultipleMatch(true, '*.Log_ZDB2', fn) then
        OutData.WriteString(umlChangeFileExt(umlGetFileName(fn), ''));
  SetLength(fArry, 0);
end;

procedure TDTC40_Log_DB_Service.cmd_CloseDB(Sender: TPeerIO; InData: TDFE);
var
  DBName_: SystemString;
begin
  DBName_ := InData.R.ReadString;
  DB_Pool.Delete(DBName_);
end;

procedure TDTC40_Log_DB_Service.cmd_RemoveDB(Sender: TPeerIO; InData: TDFE);
var
  DBName_: SystemString;
  fn: U_String;
begin
  DBName_ := InData.R.ReadString;
  DB_Pool.Delete(DBName_);
  fn := umlCombineFileName(DTC40_DB_Directory, umlConverStrToFileName(DBName_) + '.Log_ZDB2');
  if umlFileExists(fn) then
      umlDeleteFile(fn);
end;

procedure TDTC40_Log_DB_Service.Do_Create_ZDB2_HashString(Sender: TZDB2_List_HashString; Obj: TZDB2_HashString);
begin

end;

procedure TDTC40_Log_DB_Service.Do_DB_Pool_SafeCheck(const Name_: PSystemString; Obj_: TDTC40_ZDB2_List_HashString);
begin
  Obj_.Flush;
end;

procedure TDTC40_Log_DB_Service.Do_DB_Pool_Progress(const Name_: PSystemString; Obj_: TDTC40_ZDB2_List_HashString);
begin
  Obj_.Progress;
  if GetTimeTick - Obj_.LastActivtTime > LogDBRecycleMemoryTimeOut then
      WaitFreeList.Add(Obj_);
end;

constructor TDTC40_Log_DB_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  DTNoAuthService.RecvTunnel.SendDataCompressed := true;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('PostLog').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_PostLog;
  DTNoAuthService.RecvTunnel.RegisterStream('QueryLog').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_QueryLog;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('QueryAndRemoveLog').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_QueryAndRemoveLog;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveLog').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveLog;
  DTNoAuthService.RecvTunnel.RegisterStream('GetLogDB').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetLogDB;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('CloseDB').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_CloseDB;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveDB').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveDB;
  Service.QuietMode := true;
  // is only instance
  ServiceInfo.OnlyInstance := true;
  UpdateToGlobalDispatch;

  WaitFreeList := TLog_DB_List.Create;
  DTC40_DB_Directory := umlCombinePath(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s', [ServiceInfo.ServiceTyp.Text]));
  umlCreateDirectory(DTC40_DB_Directory);
  DB_Pool := TLog_DB_Pool.Create(true, 128 * 1024 * 1024, nil);
  LogDBRecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('LogDBRecycleMemory', '60*1000'), 60 * 1000);
  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '5*1000'), 5 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '1*1024*1024'), 1 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '100'), 100);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'True'), true);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csRijndael]);
  ZDB2Password := ParamList.GetDefaultValue('Password', DTC40.DTC40_Password);
  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, true, true)
  else
      ZDB2Cipher := nil;
end;

destructor TDTC40_Log_DB_Service.Destroy;
begin
  disposeObject(WaitFreeList);
  disposeObject(DB_Pool);
  inherited Destroy;
end;

procedure TDTC40_Log_DB_Service.SafeCheck;
begin
  inherited SafeCheck;
  DB_Pool.ProgressM({$IFDEF FPC}@{$ENDIF FPC}Do_DB_Pool_SafeCheck);
end;

procedure TDTC40_Log_DB_Service.Progress;
var
  i: Integer;
  tk: TTimeTick;
begin
  inherited Progress;

  WaitFreeList.Clear;
  DB_Pool.ProgressM({$IFDEF FPC}@{$ENDIF FPC}Do_DB_Pool_Progress);
  if WaitFreeList.Count > 0 then
    begin
      tk := GetTimeTick;
      for i := 0 to WaitFreeList.Count - 1 do
        begin
          DoStatus('recycle IDLE Log Database: %s', [WaitFreeList[i].Name.Text]);
          DB_Pool.Delete(WaitFreeList[i].Name);
          if GetTimeTick - tk > 100 then
              break;
        end;
      WaitFreeList.Clear;
    end;
end;

function TDTC40_Log_DB_Service.GetDB(const DBName_: SystemString): TDTC40_ZDB2_List_HashString;
var
  fn: U_String;
  fs: TCoreClassFileStream;
  DBName__: U_String;
begin
  DBName__ := umlConverStrToFileName(DBName_);

  Result := DB_Pool[DBName__];
  if Result = nil then
    begin
      fn := umlCombineFileName(DTC40_DB_Directory, DBName__ + '.Log_ZDB2');
      try
        if umlFileExists(fn) then
            fs := TCoreClassFileStream.Create(fn, fmOpenReadWrite)
        else
            fs := TCoreClassFileStream.Create(fn, fmCreate);
        Result := TDTC40_ZDB2_List_HashString.Create(
          TZDB2_HashString,
{$IFDEF FPC}@{$ENDIF FPC}Do_Create_ZDB2_HashString,
          ZDB2RecycleMemoryTimeOut,
          fs,
          ZDB2DeltaSpace,
          ZDB2BlockSize,
          ZDB2Cipher);
        Result.AutoFreeStream := true;
        Result.Name := DBName__;
        Result.LastActivtTime := GetTimeTick;
        Result.LastPostTime := umlNow;
        DB_Pool.FastAdd(DBName__, Result);
      except
          Result := nil;
      end;
    end
  else if Result <> nil then
      Result.LastActivtTime := GetTimeTick;
end;

procedure TDTC40_Log_DB_Service.PostLog(const DBName_, Log1_, Log2_: SystemString);
var
  db_: TDTC40_ZDB2_List_HashString;
  hs_: TZDB2_HashString;
  t_: TDateTime;
begin
  db_ := GetDB(DBName_);
  if db_ = nil then
      exit;
  hs_ := db_.NewData;

  t_ := umlNow;
  while CompareDateTime(t_, db_.LastPostTime) <= 0 do
      t_ := IncSecond(t_, 1);
  db_.LastPostTime := t_;

  hs_.Data['Time'] := umlDateTimeToStr(t_);
  if Log1_ <> '' then
      hs_.Data['Log1'] := Log1_;
  if Log2_ <> '' then
      hs_.Data['Log2'] := Log2_;
end;

constructor TDTC40_Log_DB_Client.TON_QueryLog.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_Log_DB_Client.TON_QueryLog.DoStreamEvent(Sender: TPeerIO; Result_: TDFE);
var
  arry: TArrayLogData;
  i: Integer;
begin
  SetLength(arry, Result_.Count shr 2);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].LogTime := Result_.R.ReadDouble;
      arry[i].Log1 := Result_.R.ReadString;
      arry[i].Log2 := Result_.R.ReadString;
      arry[i].Index := Result_.R.ReadInteger;
      inc(i);
    end;

  SortLog(arry);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_Log_DB_Client.TON_GetLogDB.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_Log_DB_Client.TON_GetLogDB.DoStreamEvent(Sender: TPeerIO; Result_: TDFE);
var
  arry: U_StringArray;
  i: Integer;
begin
  SetLength(arry, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      arry[i] := Result_.ReadString(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

constructor TDTC40_Log_DB_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
  Client.QuietMode := true;
end;

destructor TDTC40_Log_DB_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TDTC40_Log_DB_Client.PostLog(DBName_, Log1_, Log2_: SystemString);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DBName_);
  d.WriteString(Log1_);
  d.WriteString(Log2_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('PostLog', d);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.PostLog(DBName_, Log_: SystemString);
begin
  PostLog(DBName_, Log_, '');
end;

procedure TDTC40_Log_DB_Client.QueryLogC(DBName_: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogC);
var
  tmp: TON_QueryLog;
  d: TDFE;
begin
  tmp := TON_QueryLog.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DBName_);
  d.WriteDouble(bTime);
  d.WriteDouble(eTime);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('QueryLog', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.QueryLogM(DBName_: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogM);
var
  tmp: TON_QueryLog;
  d: TDFE;
begin
  tmp := TON_QueryLog.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DBName_);
  d.WriteDouble(bTime);
  d.WriteDouble(eTime);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('QueryLog', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.QueryLogP(DBName_: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogP);
var
  tmp: TON_QueryLog;
  d: TDFE;
begin
  tmp := TON_QueryLog.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DBName_);
  d.WriteDouble(bTime);
  d.WriteDouble(eTime);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('QueryLog', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.QueryAndRemoveLog(DBName_: SystemString; bTime, eTime: TDateTime);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DBName_);
  d.WriteDouble(bTime);
  d.WriteDouble(eTime);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('QueryAndRemoveLog', d);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.RemoveLog(DBName_: SystemString; arry_index: array of Integer);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DBName_);
  d.WriteArrayInteger.WriteArray(arry_index);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveLog', d);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.GetLogDBC(OnResult: TON_GetLogDBC);
var
  tmp: TON_GetLogDB;
  d: TDFE;
begin
  tmp := TON_GetLogDB.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetLogDB', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.GetLogDBM(OnResult: TON_GetLogDBM);
var
  tmp: TON_GetLogDB;
  d: TDFE;
begin
  tmp := TON_GetLogDB.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetLogDB', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.GetLogDBP(OnResult: TON_GetLogDBP);
var
  tmp: TON_GetLogDB;
  d: TDFE;
begin
  tmp := TON_GetLogDB.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetLogDB', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.CloseDB(DBName_: SystemString);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DBName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('CloseDB', d);
  disposeObject(d);
end;

procedure TDTC40_Log_DB_Client.RemoveDB(DBName_: SystemString);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DBName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveDB', d);
  disposeObject(d);
end;

initialization

RegisterC40('Log', TDTC40_Log_DB_Service, TDTC40_Log_DB_Client);

end.
