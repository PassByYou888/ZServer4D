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
  ZDB2_Core,
  CommunicationFramework, PhysicsIO, CommunicationFrameworkDoubleTunnelIO_NoAuth, DTC40;

type
  TDTC40_FS_Client = class;

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

{$REGION 'bridge_define'}

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
    stream: TMS64;
    OnResultC: TON_FS_PostFile_DoneC;
    OnResultM: TON_FS_PostFile_DoneM;
    OnResultP: TON_FS_PostFile_DoneP;

    constructor Create;
    destructor Destroy; override;
    procedure DoIDLE_Trace_Event(data_: TCoreClassObject);
    procedure DoP2PVM_CloneConnectAndPostFile(Sender: TCommunicationFrameworkWithP2PVM_Client);
  end;

  TON_FS_GetFile_DoneC = procedure(Sender: TDTC40_FS_Client; stream: TMS64; Token: U_String; Successed: Boolean);
  TON_FS_GetFile_DoneM = procedure(Sender: TDTC40_FS_Client; stream: TMS64; Token: U_String; Successed: Boolean) of object;
{$IFDEF FPC}
  TON_FS_GetFile_DoneP = procedure(Sender: TDTC40_FS_Client; stream: TMS64; Token: U_String; Successed: Boolean) is nested;
{$ELSE FPC}
  TON_FS_GetFile_DoneP = reference to procedure(Sender: TDTC40_FS_Client; stream: TMS64; Token: U_String; Successed: Boolean);
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
{$ENDREGION 'bridge_define'}

  TDTC40_FS_Client = class(TDTC40_Base_NoAuth_Client)
  public
    constructor Create(source_: TDTC40_Info; Param_: U_String); override;
    destructor Destroy; override;

    // post file
    procedure FS_PostFile(Token: U_String; stream: TCoreClassStream; doneFree: Boolean);
    procedure FS_PostFile_C(Token: U_String; stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneC);
    procedure FS_PostFile_M(Token: U_String; stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneM);
    procedure FS_PostFile_P(Token: U_String; stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneP);
    // get file
    procedure FS_GetFile_C(Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneC);
    procedure FS_GetFile_M(Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneM);
    procedure FS_GetFile_P(Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneP);
    // remove file
    procedure FS_RemoveFile(Token: U_String; Token_is_MD5: Boolean);
  end;

implementation

procedure TDTC40_FS_Service.DoLoading;
var
  id_arry: TZDB2_BlockHndle;
  id_: Integer;
  m64: TZDB2_Mem;
  Token: U_String;
  md5_: U_String;
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
                md5_ := umlMD5String(m64.PosAsPtr, m64.Size - m64.Position);
                FileNameHash.Add(Token, id_);
                FileMD5Hash.Add(md5_, id_);
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
  md5_: U_String;
  id_: Integer;
begin
  m64 := TZDB2_Mem.Create;
  m64.Mapping(InData, DataSize);
  Token := m64.ReadString;
  md5_ := umlMD5String(m64.PosAsPtr, m64.Size - m64.Position);
  Sender.Print('fill post file data "%s" md5 "%s"', [Token.Text, md5_.Text]);

  Do_FS_RemoveFile(Token, False);
  Do_FS_RemoveFile(md5_, True);

  if not ZDB2Space.CheckWriteSpace(m64.Size) then
      ZDB2Space.AppendSpace(ZDB2DeltaSpace, ZDB2BlockSize);

  if ZDB2Space.WriteData(m64, id_, False) then
    begin
      IsChanged := True;
      FileNameHash.FastAdd(Token, id_);
      FileMD5Hash.FastAdd(md5_, id_);
      Sender.Print('accept post file data "%s" md5 "%s" spcae ID:%d', [Token.Text, md5_.Text, id_]);
    end;

  DisposeObject(m64);
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
          IO_.SendDirectConsoleCmd('Error', PFormat('warning: empty file "%s"', [Token.Text]));
          exit;
        end;
    end
  else
    begin
      id_ := FileNameHash.GetDefaultValue(Token, -1);
      if id_ < 0 then
        begin
          IO_.SendDirectConsoleCmd('Error', PFormat('warning: empty file "%s"', [Token.Text]));
          exit;
        end;
    end;

  m64 := TZDB2_Mem.Create;
  if not ZDB2Space.ReadData(m64, id_) then
    begin
      IO_.SendDirectConsoleCmd('Error', PFormat('ZDB2 data error "%s"', [LastDoStatus]));
      DisposeObject(m64);
      exit;
    end;

  IO_.SendCompleteBuffer('Save', m64.Memory, m64.Size, True);
  m64.DiscardMemory;
  DisposeObject(m64);
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
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS_RemoveFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS_RemoveFile;
  // is only instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;

  FileNameHash := THashVariantList.CustomCreate(1024 * 1024 * 128);
  FileMD5Hash := THashVariantList.CustomCreate(1024 * 1024 * 128);

  // delta physics space
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '1024*1024*1024'), 1024 * 1024 * 1024);
  // block
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '1024'), 1024);
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

  IsLoading := False;
  IsChanged := False;
  DoLoading;
end;

destructor TDTC40_FS_Service.Destroy;
begin
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
  md5_: U_String;
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
  md5_ := umlMD5String(m64.PosAsPtr, m64.Size - m64.Position);
  FileNameHash.Delete(ori_token_);
  FileMD5Hash.Delete(md5_);
  DisposeObject(m64);
end;

constructor TFS_Temp_Post_File_Tunnel.Create;
begin
  inherited Create;
  p2pClient := nil;
  Client := nil;
  Token := '';
  stream := TMS64.Create;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TFS_Temp_Post_File_Tunnel.Destroy;
begin
  DisposeObject(stream);
  inherited Destroy;
end;

procedure TFS_Temp_Post_File_Tunnel.DoIDLE_Trace_Event(data_: TCoreClassObject);
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
  DelayFreeObject(1.0, data_, Self);
end;

procedure TFS_Temp_Post_File_Tunnel.DoP2PVM_CloneConnectAndPostFile(Sender: TCommunicationFrameworkWithP2PVM_Client);
begin
  p2pClient := Sender;
  Sender.SendCompleteBuffer('FS_PostFile', stream.Memory, stream.Size, True);
  stream.DiscardMemory;
  Sender.IO_IDLE_TraceM(Sender, {$IFDEF FPC}@{$ENDIF FPC}DoIDLE_Trace_Event);
end;

constructor TFS_Temp_Get_File_Tunnel.Create;
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

destructor TFS_Temp_Get_File_Tunnel.Destroy;
begin
  inherited Destroy;
end;

procedure TFS_Temp_Get_File_Tunnel.cmd_Save(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  tmp1, tmp2: TMS64;
  tmp_token_: U_String;
begin
  tmp1 := TMS64.Create;
  tmp1.Mapping(InData, DataSize);
  tmp_token_ := tmp1.ReadString;
  tmp2 := TMS64.Create;
  tmp2.Mapping(tmp1.PosAsPtr, tmp1.Size - tmp1.Position);

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

procedure TFS_Temp_Get_File_Tunnel.cmd_Error(Sender: TPeerIO; InData: SystemString);
begin
  Sender.PrintError(InData);
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil, Token, True);
    if Assigned(OnResultM) then
        OnResultM(Client, nil, Token, True);
    if Assigned(OnResultP) then
        OnResultP(Client, nil, Token, True);
  except
  end;

  p2pClient.IO_IDLE_Trace_And_FreeSelf(Self);
end;

procedure TFS_Temp_Get_File_Tunnel.DoP2PVM_CloneConnectAndGetFile(Sender: TCommunicationFrameworkWithP2PVM_Client);
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

constructor TDTC40_FS_Client.Create(source_: TDTC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
end;

destructor TDTC40_FS_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TDTC40_FS_Client.FS_PostFile(Token: U_String; stream: TCoreClassStream; doneFree: Boolean);
var
  tmp: TFS_Temp_Post_File_Tunnel;
begin
  if stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(stream);
      exit;
    end;
  tmp := TFS_Temp_Post_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.stream.WriteString(Token);
  stream.Position := 0;
  tmp.stream.CopyFrom(stream, stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(stream);
end;

procedure TDTC40_FS_Client.FS_PostFile_C(Token: U_String; stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneC);
var
  tmp: TFS_Temp_Post_File_Tunnel;
begin
  if stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(stream);
      exit;
    end;
  tmp := TFS_Temp_Post_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.OnResultC := OnResult;
  tmp.stream.WriteString(Token);
  stream.Position := 0;
  tmp.stream.CopyFrom(stream, stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(stream);
end;

procedure TDTC40_FS_Client.FS_PostFile_M(Token: U_String; stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneM);
var
  tmp: TFS_Temp_Post_File_Tunnel;
begin
  if stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(stream);
      exit;
    end;
  tmp := TFS_Temp_Post_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.OnResultM := OnResult;
  tmp.stream.WriteString(Token);
  stream.Position := 0;
  tmp.stream.CopyFrom(stream, stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(stream);
end;

procedure TDTC40_FS_Client.FS_PostFile_P(Token: U_String; stream: TCoreClassStream; doneFree: Boolean; OnResult: TON_FS_PostFile_DoneP);
var
  tmp: TFS_Temp_Post_File_Tunnel;
begin
  if stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(stream);
      exit;
    end;
  tmp := TFS_Temp_Post_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.OnResultP := OnResult;
  tmp.stream.WriteString(Token);
  stream.Position := 0;
  tmp.stream.CopyFrom(stream, stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(stream);
end;

procedure TDTC40_FS_Client.FS_GetFile_C(Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneC);
var
  tmp: TFS_Temp_Get_File_Tunnel;
begin
  tmp := TFS_Temp_Get_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.Token_is_MD5 := Token_is_MD5;
  tmp.OnResultC := OnResult;
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
end;

procedure TDTC40_FS_Client.FS_GetFile_M(Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneM);
var
  tmp: TFS_Temp_Get_File_Tunnel;
begin
  tmp := TFS_Temp_Get_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.Token_is_MD5 := Token_is_MD5;
  tmp.OnResultM := OnResult;
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
end;

procedure TDTC40_FS_Client.FS_GetFile_P(Token: U_String; Token_is_MD5: Boolean; OnResult: TON_FS_GetFile_DoneP);
var
  tmp: TFS_Temp_Get_File_Tunnel;
begin
  tmp := TFS_Temp_Get_File_Tunnel.Create;
  tmp.Client := Self;
  tmp.Token := Token;
  tmp.Token_is_MD5 := Token_is_MD5;
  tmp.OnResultP := OnResult;
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
end;

procedure TDTC40_FS_Client.FS_RemoveFile(Token: U_String; Token_is_MD5: Boolean);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(Token);
  d.WriteBool(Token_is_MD5);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS_RemoveFile', d);
  DisposeObject(d);
end;

initialization

RegisterC40('FS', TDTC40_FS_Service, TDTC40_FS_Client);

end.
