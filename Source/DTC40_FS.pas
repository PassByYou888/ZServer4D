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
  Geometry2DUnit, DataFrameEngine, ZJson,
  NotifyObjectBase, CoreCipher, MemoryStream64,
  ObjectData, ObjectDataManager, ItemStream,
  CommunicationFramework, PhysicsIO, CommunicationFrameworkDoubleTunnelIO_NoAuth, DTC40;

type
  TDTC40_FS_Client = class;

  TDTC40_FS_Service = class(TDTC40_Base_NoAuth_Service)
  protected
    // init build-in data
    IsLoading: Boolean;
    procedure DoLoading();
  protected
    // command
    procedure cmd_FS_PostFile(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_FS_GetFile(Sender: TPeerIO; InData: TDFE);
  public
    DTC40_FS_FileName: U_String;
    FileNameHash: THashVariantList;
    FileMD5Hash: THashVariantList;
    StoreEng: TObjectDataManager;
    constructor Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
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
  end;

implementation

procedure TDTC40_FS_Service.DoLoading;
var
  sr: TItemSearch;
  M64: TMS64;
begin
  IsLoading := True;

  DoStatus('extract FileSystem hash.');
  try
    if StoreEng.ItemFastFindFirst(StoreEng.RootField, '*', sr) then
      begin
        M64 := TMS64.CustomCreate(8 * 1024 * 1024);
        repeat
          StoreEng.ItemReadToStream(sr.HeaderPOS, M64);
          FileNameHash.Add(M64.ReadString, sr.HeaderPOS);
          FileMD5Hash.Add(umlMD5String(M64.PosAsPtr, M64.Size - M64.Size), sr.HeaderPOS);
          M64.Clear;
        until not StoreEng.ItemFastFindNext(sr);
        DisposeObject(M64);
      end;
    DoStatus('extract FileSystem Done.');
  except
  end;

  IsLoading := False;
end;

procedure TDTC40_FS_Service.cmd_FS_PostFile(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  M64: TMS64;
  Token: U_String;
  md5_: U_String;
  itmHnd: TItemHandle;
begin
  M64 := TMS64.Create;
  M64.Mapping(InData, DataSize);
  Token := M64.ReadString;
  md5_ := umlMD5String(M64.PosAsPtr, M64.Size - M64.Position);
  Sender.Print('post file md5 %s', [md5_.Text]);
  DisposeObject(M64);

  try
    StoreEng.ItemFastCreate(StoreEng.RootField, Token, '', itmHnd);
    StoreEng.ItemWrite(itmHnd, DataSize, InData^);
    FileNameHash.Add(Token, itmHnd.Item.RHeader.CurrentHeader);
    FileMD5Hash.Add(md5_, itmHnd.Item.RHeader.CurrentHeader);
    StoreEng.ItemClose(itmHnd);
  except
  end;
end;

procedure TDTC40_FS_Service.cmd_FS_GetFile(Sender: TPeerIO; InData: TDFE);
var
  Token: U_String;
  Token_is_MD5: Boolean;
  itmPos: Int64;
  IO_ID: Cardinal;
  IO_: TPeerIO;
  itmStream: TItemStream;
  M64: TMem64;
begin
  Token := InData.R.ReadString;
  Token_is_MD5 := InData.R.ReadBool;
  IO_ID := InData.R.ReadCardinal;

  IO_ := DTNoAuthService.RecvTunnel[IO_ID];
  if IO_ = nil then
      exit;

  if Token_is_MD5 then
      itmPos := FileMD5Hash.GetDefaultValue(Token, 0)
  else
      itmPos := FileNameHash.GetDefaultValue(Token, 0);

  if itmPos = 0 then
    begin
      IO_.SendDirectConsoleCmd('Error', PFormat('no found file "%s"', [Token.Text]));
      exit;
    end;

  itmStream := TItemStream.Create(StoreEng, itmPos);
  M64 := TMem64.Create;
  M64.Size := itmStream.Size;
  M64.CopyFrom(itmStream, itmStream.Size);
  DisposeObject(itmStream);
  if M64.Size > 0 then
    begin
      IO_.SendCompleteBuffer('Save', M64.Memory, M64.Size, True);
      M64.DiscardMemory;
    end
  else
    begin
      DisposeObject(M64);
      IO_.SendDirectConsoleCmd('Error', PFormat('warning: empty file "%s"', [Token.Text]));
    end;
end;

constructor TDTC40_FS_Service.Create(PhysicsService_: TDTC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  DTNoAuthService.RecvTunnel.CompleteBufferCompressed := False;
  // max complete buffer 128M
  DTNoAuthService.RecvTunnel.MaxCompleteBufferSize := 128 * 1024 * 1024;
  DTNoAuthService.RecvTunnel.RegisterCompleteBuffer('FS_PostFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS_PostFile;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS_GetFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS_GetFile;
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;

  FileNameHash := THashVariantList.CustomCreate(1024 * 1024);
  FileMD5Hash := THashVariantList.CustomCreate(1024 * 1024);

  DTC40_FS_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.OX', [ServiceInfo.ServiceTyp.Text]));
  if umlFileExists(DTC40_FS_FileName) then
    begin
      StoreEng := ObjectDataMarshal.Open(DTC40_FS_FileName, False);
      DoStatus('Open DB file: %s', [DTC40_FS_FileName.Text]);
    end
  else
    begin
      StoreEng := ObjectDataMarshal.NewDB(200, DTC40_FS_FileName, False);
      DoStatus('create new DB file: %s', [DTC40_FS_FileName.Text]);
    end;
  StoreEng.OverWriteItem := False;
  DoLoading;
end;

destructor TDTC40_FS_Service.Destroy;
begin
  DisposeObject(FileNameHash);
  DisposeObject(FileMD5Hash);
  inherited Destroy;
end;

procedure TDTC40_FS_Service.SafeCheck;
begin
  inherited SafeCheck;
  DoStatus('Update FileSystem IO.');
  StoreEng.UpdateIO;
  DoStatus('Update FileSystem IO Done.');
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
  d.WriteCardinal(Sender.ClientIO.ID);
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

initialization

RegisterC40('FS', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS0', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS1', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS2', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS3', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS4', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS5', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS6', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS7', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS8', TDTC40_FS_Service, TDTC40_FS_Client);
RegisterC40('FS9', TDTC40_FS_Service, TDTC40_FS_Client);

end.
