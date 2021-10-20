{ ****************************************************************************** }
{ * cloud 4.0 File System                                                      * }
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
unit DTC40_FS;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib, ListEngine,
  Geometry2DUnit, DataFrameEngine, ZJson, zExpression,
  NotifyObjectBase, CoreCipher, MemoryStream64,
  ZDB2_Core, ZDB2_MS64, GHashList,
  CommunicationFramework, PhysicsIO, CommunicationFrameworkDoubleTunnelIO_NoAuth, DTC40;

type
  TDTC40_FS_Service = class(TDTC40_Base_NoAuth_Service)
  protected
    // init build-in data
    IsLoading: Boolean;
    IsChanged: Boolean;
    procedure DoLoading();
  protected
    // command
    procedure cmd_FS_PostFile(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_FS_GetFile(Sender: TPeerIO; InData: TDFE);
    procedure cmd_FS_GetFileMD5(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_FS_RemoveFile(Sender: TPeerIO; InData: TDFE);
  private
    FIOHnd: TIOHnd;
  public
    DTC40_FS_FileName: U_String;
    FileNameHash: THashVariantList;
    FileMD5Hash: THashVariantList;
    // ZDB2 Core
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2Space: TZDB2_Core_Space;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Do_FS_RemoveFile(Token: U_String; Token_is_MD5: Boolean);
  end;

  TDTC40_FS_Client = class(TDTC40_Base_NoAuth_Client)
  public type
{$REGION 'bridge_define'}
    TFileCacheData = class
      Owner: TDTC40_FS_Client;
      Stream: TZDB2_MS64;
      constructor Create(Owner_: TDTC40_FS_Client);
      destructor Destroy; override;
    end;

    TFileCacheHash = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TFileCacheData>;

    TON_FS_PostFile_DoneC = procedure(Sender: TDTC40_FS_Client; Token: U_String);
    TON_FS_PostFile_DoneM = procedure(Sender: TDTC40_FS_Client; Token: U_String) of object;
{$IFDEF FPC}
    TON_FS_PostFile_DoneP = procedure(Sender: TDTC40_FS_Client; Token: U_String) is nested;
{$ELSE FPC}
    TON_FS_PostFile_DoneP = reference to procedure(Sender: TDTC40_FS_Client; Token: U_String);
{$ENDIF FPC}

    TFS_Temp_Post_File_Tunnel = class
    public
      p2pClient: TCommunicationFrameworkWithP2PVM_Client;
      Client: TDTC40_FS_Client;
      Token: U_String;
      Stream: TMS64;
      OnResultC: TON_FS_PostFile_DoneC;
      OnResultM: TON_FS_PostFile_DoneM;
      OnResultP: TON_FS_PostFile_DoneP;

      constructor Create;
      destructor Destroy; override;
      procedure DoP2PVM_CloneConnectAndPostFile(Sender: TCommunicationFrameworkWithP2PVM_Client);
      procedure cmd_PostDone(Sender: TPeerIO; InData: SystemString);
    end;

    TON_FS_GetFile_DoneC = procedure(Sender: TDTC40_FS_Client; Stream: TMS64; Token: U_String; Successed: Boolean);
    TON_FS_GetFile_DoneM = procedure(Sender: TDTC40_FS_Client; Stream: TMS64; Token: U_String; Successed: Boolean) of object;
{$IFDEF FPC}
    TON_FS_GetFile_DoneP = procedure(Sender: TDTC40_FS_Client; Stream: TMS64; Token: U_String; Successed: Boolean) is nested;
{$ELSE FPC}
    TON_FS_GetFile_DoneP = reference to procedure(Sender: TDTC40_FS_Client; Stream: TMS64; Token: U_String; Successed: Boolean);
{$ENDIF FPC}

    TFS_Temp_Get_File_Tunnel = class
    public
      p2pClient: TCommunicationFrameworkWithP2PVM_Client;
      Client: TDTC40_FS_Client;
      Token: U_String;
      Token_is_MD5: Boolean;
      OnResultC: TON_FS_GetFile_DoneC;
      OnResultM: TON_FS_GetFile_DoneM;
      OnResultP: TON_FS_GetFile_DoneP;

      constructor Create;
      destructor Destroy; override;
      procedure cmd_Save(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
      procedure cmd_Error(Sender: TPeerIO; InData: SystemString);
      procedure DoP2PVM_CloneConnectAndGetFile(Sender: TCommunicationFrameworkWithP2PVM_Client);
    end;

    TFS_Temp_GetFileMD5C = procedure(Sender: TDTC40_FS_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
    TFS_Temp_GetFileMD5M = procedure(Sender: TDTC40_FS_Client; State_: Boolean; info_: SystemString; MD5_: TMD5) of object;
{$IFDEF FPC}
    TFS_Temp_GetFileMD5P = procedure(Sender: TDTC40_FS_Client; State_: Boolean; info_: SystemString; MD5_: TMD5) is nested;
{$ELSE FPC}
    TFS_Temp_GetFileMD5P = reference to procedure(Sender: TDTC40_FS_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
{$ENDIF FPC}

    TFS_Temp_GetFileMD5 = class(TOnResultBridge)
    public
      Client: TDTC40_FS_Client;
      OnResultC: TFS_Temp_GetFileMD5C;
      OnResultM: TFS_Temp_GetFileMD5M;
      OnResultP: TFS_Temp_GetFileMD5P;
      constructor Create; override;
      procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TFS_Temp_GetFileMD5_Cache = class
    public
      Client: TDTC40_FS_Client;
      Token: U_String;
      OnResultC: TON_FS_GetFile_DoneC;
      OnResultM: TON_FS_GetFile_DoneM;
      OnResultP: TON_FS_GetFile_DoneP;

      constructor Create;
      destructor Destroy; override;
      procedure Do_FS_GetFileMD5(Sender: TDTC40_FS_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
    end;

{$ENDREGION 'bridge_define'}
  public
    DTC40_FS_Cache_FileName: U_String;
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;
    FileCacheHash: TFileCacheHash;
    Cache: TZDB2_List_MS64;
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;

    // upload
    procedure FS_PostFile(Token: U_String; Stream: TCoreClassStream; doneFree: Boolean);
    procedure FS_PostFile_C(Token: U_String; Stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneC);
    procedure FS_PostFile_M(Token: U_String; Stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneM);
    procedure FS_PostFile_P(Token: U_String; Stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneP);
    // download
    procedure FS_GetFile_C(useCache_: Boolean; Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneC);
    procedure FS_GetFile_M(useCache_: Boolean; Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneM);
    procedure FS_GetFile_P(useCache_: Boolean; Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneP);
    // md5
    procedure FS_GetFileMD5C(Token: U_String; OnResult: TFS_Temp_GetFileMD5C);
    procedure FS_GetFileMD5M(Token: U_String; OnResult: TFS_Temp_GetFileMD5M);
    procedure FS_GetFileMD5P(Token: U_String; OnResult: TFS_Temp_GetFileMD5P);
    // remove
    procedure FS_RemoveFile(Token: U_String; Token_is_MD5: Boolean);
  end;

implementation

procedure TDTC40_FS_Service.DoLoading;
var
  id_arry: TZDB2_BlockHandle;
  id_: Integer;
  m64: TZDB2_Mem;
  Token: U_String;
  MD5_: U_String;
begin
  IsLoading := True;

  DoStatus('extract FileSystem hash.');
  try
    if umlFileSize(ZDB2Space.Space_IOHnd^) = 0 then
      begin
        ZDB2Space.BuildSpace(ZDB2DeltaSpace, ZDB2BlockSize);
        ZDB2Space.Save;
      end
    else if ZDB2Space.Open then
      begin
        id_arry := ZDB2Space.BuildTableID;
        m64 := TZDB2_Mem.Create;
        for id_ in id_arry do
          begin
            if ZDB2Space.ReadData(m64, id_) then
              begin
                m64.Position := 0;
                Token := m64.ReadString;
                MD5_ := umlMD5String(m64.PosAsPtr, m64.Size - m64.Position);
                FileNameHash.Add(Token, id_);
                FileMD5Hash.Add(MD5_, id_);
              end;
          end;
        DisposeObject(m64);
      end;
    DoStatus('extract FileSystem Done.');
  except
  end;

  IsLoading := False;
  IsChanged := False;
end;

procedure TDTC40_FS_Service.cmd_FS_PostFile(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  m64: TZDB2_Mem;
  Token: U_String;
  MD5_: U_String;
  id_: Integer;
begin
  m64 := TZDB2_Mem.Create;
  m64.Mapping(InData, DataSize);
  Token := m64.ReadString;
  MD5_ := umlMD5String(m64.PosAsPtr, m64.Size - m64.Position);
  Sender.Print('fill post file data "%s" md5 "%s"', [Token.Text, MD5_.Text]);

  Do_FS_RemoveFile(Token, False);
  Do_FS_RemoveFile(MD5_, True);

  if not ZDB2Space.CheckWriteSpace(m64.Size) then
      ZDB2Space.AppendSpace(ZDB2DeltaSpace, ZDB2BlockSize);

  if ZDB2Space.WriteData(m64, id_, False) then
    begin
      IsChanged := True;
      FileNameHash.FastAdd(Token, id_);
      FileMD5Hash.FastAdd(MD5_, id_);
      Sender.Print('accept post file data "%s" md5 "%s" spcae ID:%d', [Token.Text, MD5_.Text, id_]);
    end;

  DisposeObject(m64);
  Sender.SendDirectConsoleCmd('PostDone', MD5_);
end;

procedure TDTC40_FS_Service.cmd_FS_GetFile(Sender: TPeerIO; InData: TDFE);
var
  Token: U_String;
  Token_is_MD5: Boolean;
  IO_ID: Cardinal;
  IO_: TPeerIO;
  id_: Integer;
  m64: TZDB2_Mem;
begin
  Token := InData.R.ReadString;
  Token_is_MD5 := InData.R.ReadBool;
  IO_ID := InData.R.ReadCardinal;

  IO_ := DTNoAuthService.RecvTunnel[IO_ID];
  if IO_ = nil then
      exit;

  if Token_is_MD5 then
    begin
      id_ := FileMD5Hash.GetDefaultValue(Token, -1);
      if id_ < 0 then
        begin
          IO_.SendDirectConsoleCmd('Error', PFormat('no found md5 "%s"', [Token.Text]));
          exit;
        end;
    end
  else
    begin
      id_ := FileNameHash.GetDefaultValue(Token, -1);
      if id_ < 0 then
        begin
          IO_.SendDirectConsoleCmd('Error', PFormat('no found file "%s"', [Token.Text]));
          exit;
        end;
    end;

  m64 := TZDB2_Mem.Create;
  if not ZDB2Space.ReadData(m64, id_) then
    begin
      IO_.SendDirectConsoleCmd('Error', PFormat('ZDB2 error "%s"', [LastDoStatus]));
      DisposeObject(m64);
      exit;
    end;

  IO_.SendCompleteBuffer('Save', m64.Memory, m64.Size, True);
  m64.DiscardMemory;
  DisposeObject(m64);
end;

procedure TDTC40_FS_Service.cmd_FS_GetFileMD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  Token: U_String;
  id_: Integer;
  m64: TZDB2_Mem;
  ori_token_: U_String;
  MD5_: TMD5;
begin
  Token := InData.R.ReadString;

  id_ := FileNameHash.GetDefaultValue(Token, -1);
  if id_ < 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found file "%s"', [Token.Text]);
      exit;
    end;

  m64 := TZDB2_Mem.Create;
  if not ZDB2Space.ReadData(m64, id_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('ZDB2 error "%s"', [LastDoStatus]);
      DisposeObject(m64);
      exit;
    end;

  ori_token_ := m64.ReadString;
  MD5_ := umlMD5(m64.PosAsPtr, m64.Size - m64.Position);
  DisposeObject(m64);
  OutData.WriteBool(True);
  OutData.WriteString(umlMD5ToStr(MD5_));
  OutData.WriteMD5(MD5_);
end;

procedure TDTC40_FS_Service.cmd_FS_RemoveFile(Sender: TPeerIO; InData: TDFE);
var
  Token: U_String;
  Token_is_MD5: Boolean;
begin
  Token := InData.R.ReadString;
  Token_is_MD5 := InData.R.ReadBool;
  Do_FS_RemoveFile(Token, Token_is_MD5);
end;

constructor TDTC40_FS_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  DTNoAuthService.RecvTunnel.CompleteBufferCompressed := False;
  // max complete buffer 128M
  DTNoAuthService.RecvTunnel.MaxCompleteBufferSize := 128 * 1024 * 1024;
  DTNoAuthService.RecvTunnel.RegisterCompleteBuffer('FS_PostFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS_PostFile;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS_GetFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS_GetFile;
  DTNoAuthService.RecvTunnel.RegisterStream('FS_GetFileMD5').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS_GetFileMD5;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS_RemoveFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS_RemoveFile;
  // is only instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;

  FileNameHash := THashVariantList.CustomCreate(1024 * 1024 * 128);
  FileMD5Hash := THashVariantList.CustomCreate(1024 * 1024 * 128);

  // delta physics space
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '512*1024*1024'), 512 * 1024 * 1024);
  // block
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '1536'), 1536);
  // IO
  InitIOHnd(FIOHnd);
  DTC40_FS_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.Space', [ServiceInfo.ServiceTyp.Text]));
  if umlFileExists(DTC40_FS_FileName) then
    begin
      if not umlFileOpen(DTC40_FS_FileName, FIOHnd, False) then
          RaiseInfo('open file "%s" error.', [DTC40_FS_FileName.Text]);
    end
  else
    begin
      if not umlFileCreate(DTC40_FS_FileName, FIOHnd) then
          RaiseInfo('create file "%s" error.', [DTC40_FS_FileName.Text]);
    end;
  ZDB2Space := TZDB2_Core_Space.Create(@FIOHnd);
  ZDB2Space.Mode := smBigData;
  ZDB2Space.AutoCloseIOHnd := True;

  IsLoading := False;
  IsChanged := False;
  DoLoading;
end;

destructor TDTC40_FS_Service.Destroy;
begin
  ZDB2Space.Save;
  DisposeObject(FileNameHash);
  DisposeObject(FileMD5Hash);
  DisposeObject(ZDB2Space);
  inherited Destroy;
end;

procedure TDTC40_FS_Service.SafeCheck;
begin
  inherited SafeCheck;
  if IsChanged then
    begin
      DoStatus('Update FileSystem IO.');
      ZDB2Space.Save;
      DoStatus('Update FileSystem IO Done.');
      IsChanged := False;
    end;
end;

procedure TDTC40_FS_Service.Do_FS_RemoveFile(Token: U_String; Token_is_MD5: Boolean);
var
  id_: Integer;
  m64: TZDB2_Mem;
  ori_token_: U_String;
  MD5_: U_String;
begin
  if Token_is_MD5 then
    begin
      id_ := FileMD5Hash.GetDefaultValue(Token, -1);
      if id_ < 0 then
          exit;
    end
  else
    begin
      id_ := FileNameHash.GetDefaultValue(Token, -1);
      if id_ < 0 then
          exit;
    end;

  m64 := TZDB2_Mem.Create;
  if not ZDB2Space.ReadData(m64, id_) then
    begin
      DisposeObject(m64);
      exit;
    end;
  ZDB2Space.RemoveData(id_, False);
  IsChanged := True;

  m64.Position := 0;
  ori_token_ := m64.ReadString;
  MD5_ := umlMD5String(m64.PosAsPtr, m64.Size - m64.Position);
  FileNameHash.Delete(ori_token_);
  FileMD5Hash.Delete(MD5_);
  DisposeObject(m64);
end;

constructor TDTC40_FS_Client.TFileCacheData.Create(Owner_: TDTC40_FS_Client);
begin
  inherited Create;
  Owner := Owner_;
  Stream := Owner.Cache.NewData;
end;

destructor TDTC40_FS_Client.TFileCacheData.Destroy;
begin
  Owner.Cache.Remove(Stream, True);
  inherited Destroy;
end;

constructor TDTC40_FS_Client.TFS_Temp_Post_File_Tunnel.Create;
begin
  inherited Create;
  p2pClient := nil;
  Client := nil;
  Token := '';
  Stream := TMS64.Create;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TDTC40_FS_Client.TFS_Temp_Post_File_Tunnel.Destroy;
begin
  DisposeObject(Stream);
  inherited Destroy;
end;

procedure TDTC40_FS_Client.TFS_Temp_Post_File_Tunnel.DoP2PVM_CloneConnectAndPostFile(Sender: TCommunicationFrameworkWithP2PVM_Client);
var
  tmp_token_: U_String;
  Cache: TFileCacheData;
begin
  Cache := Client.FileCacheHash[Token];
  if Cache = nil then
    begin
      Cache := TFileCacheData.Create(Client);
      Client.FileCacheHash.FastAdd(Token, Cache);
      Sender.Print('build cache %s', [Token.Text]);
    end;
  Cache.Stream.Data.LoadFromStream(Stream);
  Sender.Print('update cache %s', [Token.Text]);

  p2pClient := Sender;
  p2pClient.RegisterDirectConsole('PostDone').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_PostDone;
  Sender.SendCompleteBuffer('FS_PostFile', Stream.Memory, Stream.Size, True);
  Stream.DiscardMemory;
end;

procedure TDTC40_FS_Client.TFS_Temp_Post_File_Tunnel.cmd_PostDone(Sender: TPeerIO; InData: SystemString);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, Token);
    if Assigned(OnResultM) then
        OnResultM(Client, Token);
    if Assigned(OnResultP) then
        OnResultP(Client, Token);
  except
  end;
  p2pClient.IO_IDLE_Trace_And_FreeSelf(Self);
end;

constructor TDTC40_FS_Client.TFS_Temp_Get_File_Tunnel.Create;
begin
  inherited Create;
  p2pClient := nil;
  Client := nil;
  Token := '';
  Token_is_MD5 := False;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TDTC40_FS_Client.TFS_Temp_Get_File_Tunnel.Destroy;
begin
  inherited Destroy;
end;

procedure TDTC40_FS_Client.TFS_Temp_Get_File_Tunnel.cmd_Save(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  tmp1, tmp2: TMS64;
  tmp_token_: U_String;
  Cache: TFileCacheData;
begin
  tmp1 := TMS64.Create;
  tmp1.Mapping(InData, DataSize);
  tmp_token_ := tmp1.ReadString;
  tmp2 := TMS64.Create;
  tmp2.Mapping(tmp1.PosAsPtr, tmp1.Size - tmp1.Position);

  if not Token_is_MD5 then
    begin
      Cache := Client.FileCacheHash[Token];
      if Cache = nil then
        begin
          Cache := TFileCacheData.Create(Client);
          Client.FileCacheHash.FastAdd(Token, Cache);
          Sender.Print('build cache %s', [Token.Text]);
        end;
      Cache.Stream.Data.LoadFromStream(tmp2);
      Sender.Print('update cache %s', [Token.Text]);
    end;

  Sender.Print('get file md5 %s', [umlStreamMD5String(tmp2).Text]);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp2, tmp_token_, True);
    if Assigned(OnResultM) then
        OnResultM(Client, tmp2, tmp_token_, True);
    if Assigned(OnResultP) then
        OnResultP(Client, tmp2, tmp_token_, True);
  except
  end;

  DisposeObject(tmp1);
  DisposeObject(tmp2);
  p2pClient.IO_IDLE_Trace_And_FreeSelf(Self);
end;

procedure TDTC40_FS_Client.TFS_Temp_Get_File_Tunnel.cmd_Error(Sender: TPeerIO; InData: SystemString);
begin
  Sender.PrintError(InData);
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil, Token, False);
    if Assigned(OnResultM) then
        OnResultM(Client, nil, Token, False);
    if Assigned(OnResultP) then
        OnResultP(Client, nil, Token, False);
  except
  end;

  p2pClient.IO_IDLE_Trace_And_FreeSelf(Self);
end;

procedure TDTC40_FS_Client.TFS_Temp_Get_File_Tunnel.DoP2PVM_CloneConnectAndGetFile(Sender: TCommunicationFrameworkWithP2PVM_Client);
var
  d: TDFE;
begin
  p2pClient := Sender;
  Sender.RegisterCompleteBuffer('Save').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Save;
  Sender.RegisterDirectConsole('Error').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Error;
  d := TDFE.Create;
  d.WriteString(Token);
  d.WriteBool(Token_is_MD5);
  d.WriteCardinal(Sender.ClientIO.id);
  Client.DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS_GetFile', d);
  DisposeObject(d);
end;

constructor TDTC40_FS_Client.TFS_Temp_GetFileMD5.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TDTC40_FS_Client.TFS_Temp_GetFileMD5.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  md5: TMD5;
begin
  State_ := False;
  info_ := 'error';
  md5 := NullMD5;
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
      md5 := Result_.R.ReadMD5;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, md5);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, md5);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, md5);
  except
  end;
  DelayFreeObject(1.0, Self);
end;

procedure TDTC40_FS_Client.TFS_Temp_GetFileMD5.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  md5: TMD5;
begin
  State_ := False;
  info_ := 'error.';
  md5 := NullMD5;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, md5);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, md5);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, md5);
  except
  end;
  DelayFreeObject(1.0, Self);
end;

constructor TDTC40_FS_Client.TFS_Temp_GetFileMD5_Cache.Create;
begin
  inherited Create;
  Client := nil;
  Token := '';
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TDTC40_FS_Client.TFS_Temp_GetFileMD5_Cache.Destroy;
begin
  Token := '';
  inherited Destroy;
end;

procedure TDTC40_FS_Client.TFS_Temp_GetFileMD5_Cache.Do_FS_GetFileMD5(Sender: TDTC40_FS_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
var
  Cache: TFileCacheData;
  tmp: TFS_Temp_Get_File_Tunnel;
begin
  if not State_ then
    begin
      DoStatus(info_);
      try
        if Assigned(OnResultC) then
            OnResultC(Client, nil, Token, False);
        if Assigned(OnResultM) then
            OnResultM(Client, nil, Token, False);
        if Assigned(OnResultP) then
            OnResultP(Client, nil, Token, False);
      except
      end;
      Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(1.0, Self, nil);
      exit;
    end;

  Cache := Client.FileCacheHash[Token];
  if (Cache <> nil) and umlMD5Compare(umlStreamMD5(Cache.Stream.Data), MD5_) then
    begin
      Sender.DTNoAuth.RecvTunnel.Print('get file %s from cache', [Token.Text]);;
      try
        Cache.Stream.Data.Position := 0;
        if Assigned(OnResultC) then
            OnResultC(Client, Cache.Stream.Data, Token, True);
        if Assigned(OnResultM) then
            OnResultM(Client, Cache.Stream.Data, Token, True);
        if Assigned(OnResultP) then
            OnResultP(Client, Cache.Stream.Data, Token, True);
      except
      end;
      Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(1.0, Self, nil);
    end
  else
    begin
      tmp := TFS_Temp_Get_File_Tunnel.Create;
      tmp.Client := Client;
      tmp.Token := Token;
      tmp.Token_is_MD5 := False;
      tmp.OnResultC := OnResultC;
      tmp.OnResultM := OnResultM;
      tmp.OnResultP := OnResultP;
      Client.Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
      Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(1.0, Self, nil);
    end;
end;

constructor TDTC40_FS_Client.Create(source_: TDTC40_Info; Param_: U_String);
var
  i: Integer;
begin
  inherited Create(source_, Param_);

  DTC40_FS_Cache_FileName := umlCombineFileName(DTC40_RootPath, PFormat('DTC40_FS_Cache_%s_%s.Space', [source_.ServiceTyp.Text, umlMD5ToStr(source_.Hash).Text]));

  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '5*1000'), 5 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '512*1024*1024'), 512 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '1536'), 1536);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'True'), True);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csRijndael]);
  ZDB2Password := ParamList.GetDefaultValue('Password', DTC40.DTC40_Password);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;

  FileCacheHash := TFileCacheHash.Create(True, 64 * 1024 * 1024, nil);
  Cache := TZDB2_List_MS64.Create(
    TZDB2_MS64,
    nil,
    ZDB2RecycleMemoryTimeOut,
    TCoreClassFileStream.Create(DTC40_FS_Cache_FileName, fmCreate),
    ZDB2DeltaSpace,
    ZDB2BlockSize,
    ZDB2Cipher);
  Cache.AutoFreeStream := True;
end;

destructor TDTC40_FS_Client.Destroy;
begin
  DisposeObject(FileCacheHash);
  DisposeObject(Cache);
  inherited Destroy;
end;

procedure TDTC40_FS_Client.SafeCheck;
begin
  inherited SafeCheck;
  Cache.Flush;
end;

procedure TDTC40_FS_Client.Progress;
begin
  inherited Progress;
  Cache.Progress;
end;

procedure TDTC40_FS_Client.FS_PostFile(Token: U_String; Stream: TCoreClassStream; doneFree: Boolean);
var
  tmp: TFS_Temp_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(Stream);
      exit;
    end;
  tmp := TFS_Temp_Post_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.Stream.WriteString(Token);
  Stream.Position := 0;
  tmp.Stream.CopyFrom(Stream, Stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(Stream);
end;

procedure TDTC40_FS_Client.FS_PostFile_C(Token: U_String; Stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneC);
var
  tmp: TFS_Temp_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(Stream);
      exit;
    end;
  tmp := TFS_Temp_Post_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.OnResultC := OnResult;
  tmp.Stream.WriteString(Token);
  Stream.Position := 0;
  tmp.Stream.CopyFrom(Stream, Stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(Stream);
end;

procedure TDTC40_FS_Client.FS_PostFile_M(Token: U_String; Stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneM);
var
  tmp: TFS_Temp_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(Stream);
      exit;
    end;
  tmp := TFS_Temp_Post_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.OnResultM := OnResult;
  tmp.Stream.WriteString(Token);
  Stream.Position := 0;
  tmp.Stream.CopyFrom(Stream, Stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(Stream);
end;

procedure TDTC40_FS_Client.FS_PostFile_P(Token: U_String; Stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneP);
var
  tmp: TFS_Temp_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(Stream);
      exit;
    end;
  tmp := TFS_Temp_Post_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.OnResultP := OnResult;
  tmp.Stream.WriteString(Token);
  Stream.Position := 0;
  tmp.Stream.CopyFrom(Stream, Stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(Stream);
end;

procedure TDTC40_FS_Client.FS_GetFile_C(useCache_: Boolean; Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneC);
var
  tmp_cache_: TFS_Temp_GetFileMD5_Cache;
  tmp: TFS_Temp_Get_File_Tunnel;
begin
  if useCache_ and (not Token_is_MD5) and FileCacheHash.Exists(Token) then
    begin
      tmp_cache_ := TFS_Temp_GetFileMD5_Cache.Create;
      tmp_cache_.Client := Self;
      tmp_cache_.Token := Token;
      tmp_cache_.OnResultC := OnResult;
      FS_GetFileMD5M(Token, {$IFDEF FPC}@{$ENDIF FPC}tmp_cache_.Do_FS_GetFileMD5);
    end
  else
    begin
      tmp := TFS_Temp_Get_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.Token := Token;
      tmp.Token_is_MD5 := Token_is_MD5;
      tmp.OnResultC := OnResult;
      Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
    end;
end;

procedure TDTC40_FS_Client.FS_GetFile_M(useCache_: Boolean; Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneM);
var
  tmp_cache_: TFS_Temp_GetFileMD5_Cache;
  tmp: TFS_Temp_Get_File_Tunnel;
begin
  if useCache_ and (not Token_is_MD5) and FileCacheHash.Exists(Token) then
    begin
      tmp_cache_ := TFS_Temp_GetFileMD5_Cache.Create;
      tmp_cache_.Client := Self;
      tmp_cache_.Token := Token;
      tmp_cache_.OnResultM := OnResult;
      FS_GetFileMD5M(Token, {$IFDEF FPC}@{$ENDIF FPC}tmp_cache_.Do_FS_GetFileMD5);
    end
  else
    begin
      tmp := TFS_Temp_Get_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.Token := Token;
      tmp.Token_is_MD5 := Token_is_MD5;
      tmp.OnResultM := OnResult;
      Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
    end;
end;

procedure TDTC40_FS_Client.FS_GetFile_P(useCache_: Boolean; Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneP);
var
  tmp_cache_: TFS_Temp_GetFileMD5_Cache;
  tmp: TFS_Temp_Get_File_Tunnel;
begin
  if useCache_ and (not Token_is_MD5) and FileCacheHash.Exists(Token) then
    begin
      tmp_cache_ := TFS_Temp_GetFileMD5_Cache.Create;
      tmp_cache_.Client := Self;
      tmp_cache_.Token := Token;
      tmp_cache_.OnResultP := OnResult;
      FS_GetFileMD5M(Token, {$IFDEF FPC}@{$ENDIF FPC}tmp_cache_.Do_FS_GetFileMD5);
    end
  else
    begin
      tmp := TFS_Temp_Get_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.Token := Token;
      tmp.Token_is_MD5 := Token_is_MD5;
      tmp.OnResultP := OnResult;
      Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
    end;
end;

procedure TDTC40_FS_Client.FS_GetFileMD5C(Token: U_String; OnResult: TFS_Temp_GetFileMD5C);
var
  tmp: TFS_Temp_GetFileMD5;
  d: TDFE;
begin
  tmp := TFS_Temp_GetFileMD5.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(Token);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS_GetFileMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TDTC40_FS_Client.FS_GetFileMD5M(Token: U_String; OnResult: TFS_Temp_GetFileMD5M);
var
  tmp: TFS_Temp_GetFileMD5;
  d: TDFE;
begin
  tmp := TFS_Temp_GetFileMD5.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(Token);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS_GetFileMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TDTC40_FS_Client.FS_GetFileMD5P(Token: U_String; OnResult: TFS_Temp_GetFileMD5P);
var
  tmp: TFS_Temp_GetFileMD5;
  d: TDFE;
begin
  tmp := TFS_Temp_GetFileMD5.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(Token);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS_GetFileMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TDTC40_FS_Client.FS_RemoveFile(Token: U_String; Token_is_MD5: Boolean);
var
  d: TDFE;
begin
  if not Token_is_MD5 then
      FileCacheHash.Delete(Token);
  d := TDFE.Create;
  d.WriteString(Token);
  d.WriteBool(Token_is_MD5);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS_RemoveFile', d);
  DisposeObject(d);
end;

initialization

RegisterC40('FS', TDTC40_FS_Service, TDTC40_FS_Client);

end.
