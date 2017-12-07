{ ****************************************************************************** }
{ * double tunnel IO framework(incl File service)                              * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
*)

unit CommunicationFrameworkDoubleTunnelIO_NoAuth;

interface

{$I zDefine.inc}


uses CoreClasses,
  ListEngine, UnicodeMixedLib,
  DataFrameEngine, MemoryStream64, CommunicationFramework, TextDataEngine,
  DoStatusIO, Cadencer, NotifyObjectBase, PascalStrings, CoreCipher;

type
  TCommunicationFramework_DoubleTunnelService_NoAuth = class;
  TPeerClientUserDefineForRecvTunnel_NoAuth          = class;

  TPeerClientUserDefineForSendTunnel_NoAuth = class(TPeerClientUserDefine)
  public
    RecvTunnel         : TPeerClientUserDefineForRecvTunnel_NoAuth;
    RecvTunnelID       : Cardinal;
    DoubleTunnelService: TCommunicationFramework_DoubleTunnelService_NoAuth;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    function LinkOk: Boolean;
  end;

  TPeerClientUserDefineForRecvTunnel_NoAuth = class(TPeerClientUserDefine)
  public
    SendTunnel             : TPeerClientUserDefineForSendTunnel_NoAuth;
    SendTunnelID           : Cardinal;
    DoubleTunnelService    : TCommunicationFramework_DoubleTunnelService_NoAuth;
    FCurrentFileStream     : TCoreClassStream;
    FCurrentReceiveFileName: string;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    function LinkOk: Boolean;
  end;

  PPostBatchBackcallData_NoAuth = ^TPostBatchBackcallData_NoAuth;

  TPostBatchBackcallData_NoAuth = record
    OnCall: TStateCall;
    OnMethod: TStateMethod;
    {$IFNDEF FPC}
    OnProc: TStateProc;
    {$ENDIF}
    procedure init; inline;
  end;

  TCommunicationFramework_DoubleTunnelService_NoAuth = class(TCoreClassInterfacedObject)
  protected
    FRecvTunnel, FSendTunnel: TCommunicationFrameworkServer;
    FCanStatus              : Boolean;
    FCadencerEngine         : TCadencer;
    FProgressEngine         : TNProgressPost;
    FFileReceiveDirectory   : string;
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth; fn: string); virtual;
  protected
    // registed server command
    procedure Command_TunnelLink(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetCurrentCadencer(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_GetFileTime(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetFile(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_PostFileInfo(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostFile(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerClient; InData: TDataFrameEngine); virtual;

    procedure Command_NewBatchStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSafe;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    function GetUserDefineRecvTunnel(RecvCli: TPeerClient): TPeerClientUserDefineForRecvTunnel_NoAuth;

    function TotalLinkCount: Integer;

    procedure PostBatchStream(cli: TPeerClient; stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    procedure PostBatchStream(cli: TPeerClient; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall); overload;
    procedure PostBatchStream(cli: TPeerClient; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod); overload;
    {$IFNDEF FPC}
    procedure PostBatchStream(cli: TPeerClient; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc); overload;
    {$ENDIF}
    procedure ClearBatchStream(cli: TPeerClient);
    procedure GetBatchStreamState(cli: TPeerClient; OnResult: TStreamMethod); overload;
    {$IFNDEF FPC}
    procedure GetBatchStreamState(cli: TPeerClient; OnResult: TStreamProc); overload;
    {$ENDIF}
    property CanStatus: Boolean read FCanStatus write FCanStatus;
    property CadencerEngine: TCadencer read FCadencerEngine;
    property ProgressEngine: TNProgressPost read FProgressEngine;
    property FileReceiveDirectory: string read FFileReceiveDirectory;

    property RecvTunnel: TCommunicationFrameworkServer read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkServer read FSendTunnel;
  end;

  TCommunicationFramework_DoubleTunnelClient_NoAuth = class;

  TClientUserDefineForSendTunnel_NoAuth = class;

  TClientUserDefineForRecvTunnel_NoAuth = class(TPeerClientUserDefine)
  public
    Client    : TCommunicationFramework_DoubleTunnelClient_NoAuth;
    SendTunnel: TClientUserDefineForSendTunnel_NoAuth;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TClientUserDefineForSendTunnel_NoAuth = class(TPeerClientUserDefine)
  public
    Client    : TCommunicationFramework_DoubleTunnelClient_NoAuth;
    RecvTunnel: TClientUserDefineForRecvTunnel_NoAuth;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TFileComplete_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream; const fileName: string) of object;

  PRemoteFileBackcall_NoAuth = ^TRemoteFileBackcall_NoAuth;

  TRemoteFileBackcall_NoAuth = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    OnComplete: TFileComplete_NoAuth;
  end;

  TCommunicationFramework_DoubleTunnelClient_NoAuth = class(TCoreClassInterfacedObject, ICommunicationFrameworkClientInterface)
  protected
    FSendTunnel, FRecvTunnel: TCommunicationFrameworkClient;
    FLinkOk                 : Boolean;
    FWaitCommandTimeout     : Cardinal;

    FCurrentStream               : TCoreClassStream;
    FCurrentReceiveStreamFileName: string;

    FCadencerEngine: TCadencer;
    FProgressEngine: TNProgressPost;

    FLastCadencerTime: Double;
    FServerDelay     : Double;
  protected
    // client notify interface
    procedure ClientConnected(Sender: TCommunicationFrameworkClient); virtual;
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); virtual;
  protected
    // registed client command
    procedure Command_FileInfo(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostFile(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerClient; InData: TDataFrameEngine); virtual;

    procedure GetCurrentCadencer_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine); virtual;

    // batch stream suppport
    procedure Command_NewBatchStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    function Connected: Boolean; virtual;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSafe;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    function Connect(addr: string; const RecvPort, SendPort: word): Boolean; virtual;
    procedure Disconnect; virtual;

    // block mode TunnelLink
    function TunnelLink: Boolean; overload; virtual;
    // unblock mode TunnelLink
    {$IFNDEF FPC}
    procedure TunnelLink(OnProc: TStateProc); overload; virtual;
    {$ENDIF}
    // unblock mode SyncCadencer
    procedure SyncCadencer; virtual;

    procedure GetFileTime(RemoteFilename: string; CallResultProc: TStreamMethod); overload;

    // 异步方式从服务器下载文件，完成时触发通知
    procedure GetFile_StreamParamResult(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
    procedure GetFile(fileName, saveToPath: string; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileComplete_NoAuth); overload;
    // 同步方式等待从服务器下载文件完成
    function GetFile(fileName, saveToPath: string): Boolean; overload;
    // 异步上传本地文件
    procedure PostFile(fileName: string); overload;
    // 异步上传一个Stream，完成后会自动释放filestream
    procedure PostFile(fn: string; fileStream: TCoreClassStream); overload;

    // batch stream suppport
    procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall); overload;
    procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod); overload;
    {$IFNDEF FPC}
    procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc); overload;
    {$ENDIF}
    procedure ClearBatchStream;
    procedure GetBatchStreamState(OnResult: TStreamMethod); overload;
    {$IFNDEF FPC}
    procedure GetBatchStreamState(OnResult: TStreamProc); overload;
    {$ENDIF}
    function GetBatchStreamState(ResultData: TDataFrameEngine; ATimeOut: TTimeTickValue): Boolean; overload;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    property LinkOk: Boolean read FLinkOk;
    property WaitCommandTimeout: Cardinal read FWaitCommandTimeout write FWaitCommandTimeout;

    property CadencerEngine: TCadencer read FCadencerEngine;
    property ProgressEngine: TNProgressPost read FProgressEngine;
    property ServerDelay: Double read FServerDelay;

    function RemoteInited: Boolean;

    property RecvTunnel: TCommunicationFrameworkClient read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkClient read FSendTunnel;
  end;

implementation

uses SysUtils;

constructor TPeerClientUserDefineForSendTunnel_NoAuth.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  RecvTunnel := nil;
  RecvTunnelID := 0;
  DoubleTunnelService := nil;
end;

destructor TPeerClientUserDefineForSendTunnel_NoAuth.Destroy;
begin
  if (DoubleTunnelService <> nil) and (RecvTunnelID > 0) and (RecvTunnel <> nil) then
    begin
      if DoubleTunnelService.FRecvTunnel.Exists(RecvTunnelID) then
          DoubleTunnelService.FRecvTunnel.ClientFromID[RecvTunnelID].Disconnect;
    end;
  inherited Destroy;
end;

function TPeerClientUserDefineForSendTunnel_NoAuth.LinkOk: Boolean;
begin
  Result := DoubleTunnelService <> nil;
end;

constructor TPeerClientUserDefineForRecvTunnel_NoAuth.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  SendTunnel := nil;
  SendTunnelID := 0;
  DoubleTunnelService := nil;
  FCurrentFileStream := nil;
  FCurrentReceiveFileName := '';
end;

destructor TPeerClientUserDefineForRecvTunnel_NoAuth.Destroy;
begin
  if DoubleTunnelService <> nil then
    begin
      DoubleTunnelService.UserOut(Self);

      if (DoubleTunnelService <> nil) and (SendTunnelID > 0) and (SendTunnel <> nil) then
        begin
          if DoubleTunnelService.FSendTunnel.Exists(SendTunnelID) then
              DoubleTunnelService.FSendTunnel.ClientFromID[SendTunnelID].Disconnect;
        end;

      DoubleTunnelService := nil;
    end;

  if FCurrentFileStream <> nil then
      DisposeObject(FCurrentFileStream);
  FCurrentFileStream := nil;
  inherited Destroy;
end;

function TPeerClientUserDefineForRecvTunnel_NoAuth.LinkOk: Boolean;
begin
  Result := DoubleTunnelService <> nil;
end;

procedure TPostBatchBackcallData_NoAuth.init;
begin
  OnCall := nil;
  OnMethod := nil;
  {$IFNDEF FPC}
  OnProc := nil;
  {$ENDIF}
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth; fn: string);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_TunnelLink(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  RecvID, SendID: Cardinal;
  UserDefineIO  : TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  RecvID := InData.Reader.ReadCardinal;
  SendID := InData.Reader.ReadCardinal;

  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not FSendTunnel.Exists(SendID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('send tunnel Illegal:%d', [SendID]));
      Exit;
    end;

  if not FRecvTunnel.Exists(RecvID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('recv tunnel Illegal:%d', [RecvID]));
      Exit;
    end;

  if Sender.ID <> RecvID then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('recv tunnel Illegal:%d-%d', [Sender.ID, RecvID]));
      Exit;
    end;

  UserDefineIO.SendTunnel := FSendTunnel.ClientFromID[SendID].UserDefine as TPeerClientUserDefineForSendTunnel_NoAuth;
  UserDefineIO.SendTunnelID := SendID;
  UserDefineIO.DoubleTunnelService := Self;

  UserDefineIO.SendTunnel.RecvTunnel := UserDefineIO;
  UserDefineIO.SendTunnel.RecvTunnelID := RecvID;
  UserDefineIO.SendTunnel.DoubleTunnelService := Self;

  OutData.WriteBool(True);
  OutData.WriteString(Format('tunnel link success! recv:%d <-> send:%d', [RecvID, SendID]));

  UserLinkSuccess(UserDefineIO);
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_GetCurrentCadencer(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  FCadencerEngine.Progress;
  OutData.WriteDouble(FCadencerEngine.CurrentTime);
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_GetFileTime(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO    : TPeerClientUserDefineForRecvTunnel_NoAuth;
  fullfn, fileName: string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  fullfn := umlCombinePath(FFileReceiveDirectory, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      Exit;
    end;
  OutData.WriteBool(True);
  OutData.WriteString(fileName);
  OutData.WriteDouble(umlGetFileTime(fullfn));
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_GetFile(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO                : TPeerClientUserDefineForRecvTunnel_NoAuth;
  fullfn, fileName, remoteinfo: string;
  RemoteBackcallAddr          : UInt64;
  sendDE                      : TDataFrameEngine;
  fs                          : TCoreClassFileStream;
  md5                         : UnicodeMixedLib.TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombinePath(FFileReceiveDirectory, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FileInfo', sendDE);
  DisposeObject(sendDE);

  md5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream('PostFile', fs, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(md5);
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(Format('post %s to send tunnel', [fileName]));
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_PostFileInfo(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
  fn          : string;
  fsize       : Int64;
  fullfn      : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
    begin
      Sender.Disconnect;
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;
    end;

  fn := InData.Reader.ReadString;
  fsize := InData.Reader.ReadInt64;

  fullfn := umlCombinePath(FFileReceiveDirectory, fn);
  UserDefineIO.FCurrentReceiveFileName := fullfn;
  try
    UserDefineIO.FCurrentFileStream := TCoreClassFileStream.Create(fullfn, fmCreate);
    Sender.Print('post file to public: %s', [fullfn]);
  except
    Sender.Print('create file failed: %s', [fullfn]);
    UserDefineIO.FCurrentFileStream := nil;
  end;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_PostFile(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
    begin
      Sender.Disconnect;
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
          UserDefineIO.FCurrentFileStream.CopyFrom(InData, InData.Size);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_PostFileOver(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserDefineIO  : TPeerClientUserDefineForRecvTunnel_NoAuth;
  ClientMD5, md5: UnicodeMixedLib.TMD5;
  fn            : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
    begin
      Sender.Disconnect;
      Exit;
    end;

  ClientMD5 := InData.Reader.ReadMD5;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      md5 := umlStreamMD5(UserDefineIO.FCurrentFileStream);
      fn := UserDefineIO.FCurrentReceiveFileName;
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;

      if umlMD5Compare(md5, ClientMD5) then
        begin
          Sender.Print('Received File Completed:%s', [fn]);
          UserPostFileSuccess(UserDefineIO, fn);
        end
      else
        begin
          Sender.Print('File md5 error:%s', [fn]);
          umlDeleteFile(fn);
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_NewBatchStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt: TPeerClientUserDefineForRecvTunnel_NoAuth;
  p : PBigStreamBatchPostData;
begin
  rt := GetUserDefineRecvTunnel(Sender);
  if not rt.LinkOk then
      Exit;
  p := rt.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_PostBatchStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  rt: TPeerClientUserDefineForRecvTunnel_NoAuth;
  p : PBigStreamBatchPostData;
  de: TDataFrameEngine;
begin
  rt := GetUserDefineRecvTunnel(Sender);
  if not rt.LinkOk then
      Exit;

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
    begin
      p := rt.BigStreamBatchList.Last;
      p^.Source.Position := p^.Source.Size;
      p^.Source.CopyFrom(InData, InData.Size);
      if (p^.Source.Size >= BigStreamTotal) then
        begin
          p^.Source.Position := 0;
          p^.SourceMD5 := umlStreamMD5(p^.Source);

          if p^.CompletedBackcallPtr <> 0 then
            begin
              de := TDataFrameEngine.Create;
              de.WriteMD5(p^.RemoteMD5);
              de.WriteMD5(p^.SourceMD5);
              de.WritePointer(p^.CompletedBackcallPtr);
              rt.SendTunnel.Owner.SendDirectStreamCmd('PostBatchStreamDone', de);
              DisposeObject(de);
            end;
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_ClearBatchStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt: TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  rt := GetUserDefineRecvTunnel(Sender);
  if not rt.LinkOk then
      Exit;
  rt.BigStreamBatchList.Clear;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_PostBatchStreamDone(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt            : TPeerClientUserDefineForRecvTunnel_NoAuth;
  rMD5, sMD5    : UnicodeMixedLib.TMD5;
  backCallVal   : UInt64;
  backCallValPtr: PPostBatchBackcallData_NoAuth;
  MD5Verify     : Boolean;
begin
  rt := GetUserDefineRecvTunnel(Sender);
  if not rt.LinkOk then
      Exit;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := PPostBatchBackcallData_NoAuth(Pointer(backCallVal));
  MD5Verify := umlMD5Compare(rMD5, sMD5);

  if backCallValPtr = nil then
      Exit;

  try
    if Assigned(backCallValPtr^.OnCall) then
        backCallValPtr^.OnCall(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.OnMethod) then
        backCallValPtr^.OnMethod(MD5Verify);
  except
  end;

  {$IFNDEF FPC}
  try
    if Assigned(backCallValPtr^.OnProc) then
        backCallValPtr^.OnProc(MD5Verify);
  except
  end;
  {$ENDIF}
  try
      Dispose(backCallValPtr);
  except
  end;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Command_GetBatchStreamState(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  rt: TPeerClientUserDefineForRecvTunnel_NoAuth;
  i : Integer;
  p : PBigStreamBatchPostData;

  de: TDataFrameEngine;
begin
  rt := GetUserDefineRecvTunnel(Sender);
  if not rt.LinkOk then
      Exit;

  for i := 0 to rt.BigStreamBatchList.Count - 1 do
    begin
      p := rt.BigStreamBatchList[i];
      de := TDataFrameEngine.Create;
      p^.Encode(de);
      OutData.WriteDataFrame(de);
      DisposeObject(de);
    end;
end;

constructor TCommunicationFramework_DoubleTunnelService_NoAuth.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create;
  FRecvTunnel := ARecvTunnel;
  FRecvTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForRecvTunnel_NoAuth;
  FSendTunnel := ASendTunnel;
  FSendTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForSendTunnel_NoAuth;

  FCanStatus := True;
  FCadencerEngine := TCadencer.Create;
  {$IFDEF FPC}
  FCadencerEngine.OnProgress := @CadencerProgress;
  {$ELSE}
  FCadencerEngine.OnProgress := CadencerProgress;
  {$ENDIF}
  FProgressEngine := TNProgressPost.Create;

  FFileReceiveDirectory := umlCurrentPath;

  if not umlDirectoryExists(FFileReceiveDirectory) then
      umlCreateDirectory(FFileReceiveDirectory);

  SwitchAsDefaultPerformance;
end;

destructor TCommunicationFramework_DoubleTunnelService_NoAuth.Destroy;
begin
  DisposeObject(FCadencerEngine);
  DisposeObject(FProgressEngine);
  inherited Destroy;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.SwitchAsMaxSafe;
begin
  FRecvTunnel.SwitchMaxSafe;
  FSendTunnel.SwitchMaxSafe;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.Progress;
begin
  FCadencerEngine.Progress;
  FRecvTunnel.ProgressBackground;
  FSendTunnel.ProgressBackground;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.RegisterCommand;
begin
  {$IFDEF FPC}
  FRecvTunnel.RegisterStream('TunnelLink').OnExecute := @Command_TunnelLink;
  FRecvTunnel.RegisterStream('GetCurrentCadencer').OnExecute := @Command_GetCurrentCadencer;
  FRecvTunnel.RegisterStream('GetFileTime').OnExecute := @Command_GetFileTime;
  FRecvTunnel.RegisterStream('GetFile').OnExecute := @Command_GetFile;
  FRecvTunnel.RegisterDirectStream('PostFileInfo').OnExecute := @Command_PostFileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := @Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := @Command_PostFileOver;
  FRecvTunnel.RegisterDirectStream('NewBatchStream').OnExecute := @Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream('PostBatchStream').OnExecute := @Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream('ClearBatchStream').OnExecute := @Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream('PostBatchStreamDone').OnExecute := @Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream('GetBatchStreamState').OnExecute := @Command_GetBatchStreamState;
  {$ELSE}
  FRecvTunnel.RegisterStream('TunnelLink').OnExecute := Command_TunnelLink;
  FRecvTunnel.RegisterStream('GetCurrentCadencer').OnExecute := Command_GetCurrentCadencer;
  FRecvTunnel.RegisterStream('GetFileTime').OnExecute := Command_GetFileTime;
  FRecvTunnel.RegisterStream('GetFile').OnExecute := Command_GetFile;
  FRecvTunnel.RegisterDirectStream('PostFileInfo').OnExecute := Command_PostFileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := Command_PostFileOver;
  FRecvTunnel.RegisterDirectStream('NewBatchStream').OnExecute := Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream('PostBatchStream').OnExecute := Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream('ClearBatchStream').OnExecute := Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream('PostBatchStreamDone').OnExecute := Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream('GetBatchStreamState').OnExecute := Command_GetBatchStreamState;
  {$ENDIF}
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD('TunnelLink');
  FRecvTunnel.DeleteRegistedCMD('GetCurrentCadencer');

  FRecvTunnel.DeleteRegistedCMD('GetFileTime');
  FRecvTunnel.DeleteRegistedCMD('GetFile');
  FRecvTunnel.DeleteRegistedCMD('PostFileInfo');
  FRecvTunnel.DeleteRegistedCMD('PostFile');
  FRecvTunnel.DeleteRegistedCMD('PostFileOver');

  FRecvTunnel.DeleteRegistedCMD('NewBatchStream');
  FRecvTunnel.DeleteRegistedCMD('PostBatchStream');
  FRecvTunnel.DeleteRegistedCMD('ClearBatchStream');
  FRecvTunnel.DeleteRegistedCMD('PostBatchStreamDone');
  FRecvTunnel.DeleteRegistedCMD('GetBatchStreamState');
end;

function TCommunicationFramework_DoubleTunnelService_NoAuth.GetUserDefineRecvTunnel(RecvCli: TPeerClient): TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  Result := RecvCli.UserDefine as TPeerClientUserDefineForRecvTunnel_NoAuth;
end;

function TCommunicationFramework_DoubleTunnelService_NoAuth.TotalLinkCount: Integer;
begin
  Result := RecvTunnel.Count;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.PostBatchStream(cli: TPeerClient; stream: TCoreClassStream; doneFreeStream: Boolean);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(0);
  cli.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  cli.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.PostBatchStream(cli: TPeerClient; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall);
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.init;
      p^.OnCall := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  cli.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  cli.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.PostBatchStream(cli: TPeerClient; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod);
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.init;
      p^.OnMethod := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  cli.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  cli.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelService_NoAuth.PostBatchStream(cli: TPeerClient; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc);
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.init;
      p^.OnProc := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  cli.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  cli.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;
{$ENDIF}


procedure TCommunicationFramework_DoubleTunnelService_NoAuth.ClearBatchStream(cli: TPeerClient);
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;
  cli.SendDirectStreamCmd('ClearBatchStream', de);
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.GetBatchStreamState(cli: TPeerClient; OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmd('GetBatchStreamState', de, OnResult);
  DisposeObject(de);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelService_NoAuth.GetBatchStreamState(cli: TPeerClient; OnResult: TStreamProc);
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmd('GetBatchStreamState', de, OnResult);
  DisposeObject(de);
end;
{$ENDIF}


constructor TClientUserDefineForRecvTunnel_NoAuth.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  Client := nil;
  SendTunnel := nil;
end;

destructor TClientUserDefineForRecvTunnel_NoAuth.Destroy;
begin
  if Client <> nil then
      Client.FLinkOk := False;
  inherited Destroy;
end;

constructor TClientUserDefineForSendTunnel_NoAuth.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  Client := nil;
  RecvTunnel := nil;
end;

destructor TClientUserDefineForSendTunnel_NoAuth.Destroy;
begin
  if Client <> nil then
      Client.FLinkOk := False;
  inherited Destroy;
end;

// client notify interface
procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.ClientConnected(Sender: TCommunicationFrameworkClient);
begin
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.ClientDisconnect(Sender: TCommunicationFrameworkClient);
begin
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Command_FileInfo(Sender: TPeerClient; InData: TDataFrameEngine);
var
  fn        : string;
  fsize     : Int64;
  remoteinfo: string;
  fullfn    : string;
begin
  if FCurrentStream <> nil then
    begin
      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  fn := InData.Reader.ReadString;
  fsize := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;

  if not umlDirectoryExists(remoteinfo) then
      umlCreateDirectory(remoteinfo);

  fullfn := umlCombinePath(remoteinfo, fn);
  FCurrentReceiveStreamFileName := fullfn;
  try
      FCurrentStream := TCoreClassFileStream.Create(fullfn, fmCreate);
  except
    Sender.Print('create file failed: %s', [fullfn]);
    // FRecvTunnel.ClientIO.Disconnect;
    FCurrentStream := nil;
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Command_PostFile(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  if FCurrentStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
        begin
          FCurrentStream.Position := FCurrentStream.Size;
          FCurrentStream.CopyFrom(InData, InData.Size);
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Command_PostFileOver(Sender: TPeerClient; InData: TDataFrameEngine);
var
  servMD5, md5      : UnicodeMixedLib.TMD5;
  RemoteBackcallAddr: UInt64;
  p                 : PRemoteFileBackcall_NoAuth;
  fn                : string;
begin
  servMD5 := InData.Reader.ReadMD5;
  RemoteBackcallAddr := InData.Reader.ReadPointer;
  p := Pointer(RemoteBackcallAddr);
  fn := FCurrentReceiveStreamFileName;

  if FCurrentStream <> nil then
    begin
      md5 := umlStreamMD5(FCurrentStream);
      if umlMD5Compare(servMD5, md5) then
          Sender.Print(Format('Receive %s ok', [umlGetFileName(fn).Text]))
      else
          Sender.Print(Format('Receive %s failed!', [umlGetFileName(fn).Text]));

      try
        if p <> nil then
          begin
            if Assigned(p^.OnComplete) then
              begin
                FCurrentStream.Position := 0;
                p^.OnComplete(p^.UserData, p^.UserObject, FCurrentStream, fn);
              end;
            Dispose(p);
          end;
      except
      end;

      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.GetCurrentCadencer_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
var
  servTime: Double;
begin
  servTime := ResultData.Reader.ReadDouble;

  FCadencerEngine.Progress;
  FServerDelay := FCadencerEngine.CurrentTime - FLastCadencerTime;

  FCadencerEngine.CurrentTime := servTime + FServerDelay;
  FCadencerEngine.Progress;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Command_NewBatchStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt: TClientUserDefineForRecvTunnel_NoAuth;
  p : PBigStreamBatchPostData;
begin
  if not LinkOk then
      Exit;
  rt := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;
  p := rt.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Command_PostBatchStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  rt: TClientUserDefineForRecvTunnel_NoAuth;
  p : PBigStreamBatchPostData;
  de: TDataFrameEngine;
begin
  if not LinkOk then
      Exit;
  rt := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
    begin
      p := rt.BigStreamBatchList.Last;
      p^.Source.Position := p^.Source.Size;
      p^.Source.CopyFrom(InData, InData.Size);
      if (p^.Source.Size >= BigStreamTotal) then
        begin
          p^.Source.Position := 0;
          p^.SourceMD5 := umlStreamMD5(p^.Source);

          if p^.CompletedBackcallPtr <> 0 then
            begin
              de := TDataFrameEngine.Create;
              de.WriteMD5(p^.RemoteMD5);
              de.WriteMD5(p^.SourceMD5);
              de.WritePointer(p^.CompletedBackcallPtr);
              SendTunnel.SendDirectStreamCmd('PostBatchStreamDone', de);
              DisposeObject(de);
            end;
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Command_ClearBatchStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt: TClientUserDefineForRecvTunnel_NoAuth;
  p : PBigStreamBatchPostData;
  de: TDataFrameEngine;
begin
  if not LinkOk then
      Exit;
  rt := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;
  rt.BigStreamBatchList.Clear;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Command_PostBatchStreamDone(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt            : TClientUserDefineForRecvTunnel_NoAuth;
  rMD5, sMD5    : UnicodeMixedLib.TMD5;
  backCallVal   : UInt64;
  backCallValPtr: PPostBatchBackcallData_NoAuth;
  MD5Verify     : Boolean;
begin
  if not LinkOk then
      Exit;
  rt := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := PPostBatchBackcallData_NoAuth(Pointer(backCallVal));
  MD5Verify := umlMD5Compare(rMD5, sMD5);

  if backCallValPtr = nil then
      Exit;

  try
    if Assigned(backCallValPtr^.OnCall) then
        backCallValPtr^.OnCall(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.OnMethod) then
        backCallValPtr^.OnMethod(MD5Verify);
  except
  end;

  {$IFNDEF FPC}
  try
    if Assigned(backCallValPtr^.OnProc) then
        backCallValPtr^.OnProc(MD5Verify);
  except
  end;
  {$ENDIF}
  try
      Dispose(backCallValPtr);
  except
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Command_GetBatchStreamState(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  rt: TClientUserDefineForRecvTunnel_NoAuth;
  i : Integer;
  p : PBigStreamBatchPostData;

  de: TDataFrameEngine;
begin
  if not LinkOk then
      Exit;
  rt := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;

  for i := 0 to rt.BigStreamBatchList.Count - 1 do
    begin
      p := rt.BigStreamBatchList[i];
      de := TDataFrameEngine.Create;
      p^.Encode(de);
      OutData.WriteDataFrame(de);
      DisposeObject(de);
    end;
end;

constructor TCommunicationFramework_DoubleTunnelClient_NoAuth.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  inherited Create;
  FRecvTunnel := ARecvTunnel;
  FRecvTunnel.NotyifyInterface := Self;
  FRecvTunnel.PeerClientUserDefineClass := TClientUserDefineForRecvTunnel_NoAuth;

  FSendTunnel := ASendTunnel;
  FSendTunnel.NotyifyInterface := Self;
  FSendTunnel.PeerClientUserDefineClass := TClientUserDefineForSendTunnel_NoAuth;

  FLinkOk := False;
  FWaitCommandTimeout := 5000;

  FCurrentStream := nil;
  FCurrentReceiveStreamFileName := '';

  FCadencerEngine := TCadencer.Create;
  {$IFDEF FPC}
  FCadencerEngine.OnProgress := @CadencerProgress;
  {$ELSE}
  FCadencerEngine.OnProgress := CadencerProgress;
  {$ENDIF}
  FProgressEngine := TNProgressPost.Create;

  FLastCadencerTime := 0;
  FServerDelay := 0;

  SwitchAsDefaultPerformance;
end;

destructor TCommunicationFramework_DoubleTunnelClient_NoAuth.Destroy;
begin
  DisposeObject([FCadencerEngine, FProgressEngine]);

  inherited Destroy;
end;

function TCommunicationFramework_DoubleTunnelClient_NoAuth.Connected: Boolean;
begin
  try
      Result := FSendTunnel.Connected and FRecvTunnel.Connected;
  except
      Result := False;
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.SwitchAsMaxSafe;
begin
  FRecvTunnel.SwitchMaxSafe;
  FSendTunnel.SwitchMaxSafe;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Progress;
begin
  FCadencerEngine.Progress;

  try
    FRecvTunnel.ProgressBackground;
    FSendTunnel.ProgressBackground;
    if not Connected then
        FLinkOk := False;
  except
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

function TCommunicationFramework_DoubleTunnelClient_NoAuth.Connect(addr: string; const RecvPort, SendPort: word): Boolean;
var
  t: Cardinal;
begin
  Result := False;
  Disconnect;

  if not FSendTunnel.Connect(addr, SendPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      Exit;
    end;
  if not FRecvTunnel.Connect(addr, RecvPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      Exit;
    end;

  t := GetTimeTick + 10000;
  while not RemoteInited do
    begin
      if TCoreClassThread.GetTickCount > t then
          break;
      if not Connected then
          break;
      Progress;
    end;

  Result := Connected;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Disconnect;
begin
  if FSendTunnel.ClientIO <> nil then
      FSendTunnel.ClientIO.Disconnect;

  if FRecvTunnel.ClientIO <> nil then
      FRecvTunnel.ClientIO.Disconnect;
end;

function TCommunicationFramework_DoubleTunnelClient_NoAuth.TunnelLink: Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  if FLinkOk then
      Exit(True);
  FLinkOk := False;
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  SyncCadencer;

  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);

  FSendTunnel.WaitSendStreamCmd('TunnelLink', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));

      if Result then
        begin
          TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine);

          TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine);

          FLinkOk := True;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.TunnelLink(OnProc: TStateProc);
var
  sendDE: TDataFrameEngine;
begin
  if FLinkOk then
      Exit;

  FLinkOk := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  SyncCadencer;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);

  FSendTunnel.SendStreamCmd('TunnelLink', sendDE,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      r: Boolean;
    begin
      r := False;
      if ResultData.Count > 0 then
        begin
          r := ResultData.ReadBool(0);
          FSendTunnel.ClientIO.Print(ResultData.ReadString(1));

          if r then
            begin
              TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine).Client := Self;
              TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine);

              TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine).Client := Self;
              TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine);

              FLinkOk := True;
            end;
        end;

      if Assigned(OnProc) then
          OnProc(r);
    end);

  DisposeObject(sendDE);
end;
{$ENDIF}


procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.SyncCadencer;
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;

  FCadencerEngine.Progress;
  FLastCadencerTime := FCadencerEngine.CurrentTime;
  FServerDelay := 0;
  sendDE.WriteDouble(FLastCadencerTime);
  {$IFDEF FPC}
  FSendTunnel.SendStreamCmd('GetCurrentCadencer', sendDE, @GetCurrentCadencer_StreamResult);
  {$ELSE}
  FSendTunnel.SendStreamCmd('GetCurrentCadencer', sendDE, GetCurrentCadencer_StreamResult);
  {$ENDIF}
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.GetFileTime(RemoteFilename: string; CallResultProc: TStreamMethod);
var
  sendDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(RemoteFilename);

  FSendTunnel.SendStreamCmd('GetFileTime', sendDE, CallResultProc);

  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.GetFile_StreamParamResult(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PRemoteFileBackcall_NoAuth;
begin
  if ResultData.Count > 0 then
    begin
      if ResultData.Reader.ReadBool then
          Exit;
      Sender.Print('get file failed:%s', [ResultData.Reader.ReadString]);
    end;

  p := Param1;
  Dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.GetFile(fileName, saveToPath: string; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileComplete_NoAuth);
var
  sendDE: TDataFrameEngine;
  p     : PRemoteFileBackcall_NoAuth;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete := OnComplete;
  sendDE.WritePointer(p);

  {$IFDEF FPC}
  FSendTunnel.SendStreamCmd('GetFile', sendDE, p, nil, @GetFile_StreamParamResult);
  {$ELSE}
  FSendTunnel.SendStreamCmd('GetFile', sendDE, p, nil, GetFile_StreamParamResult);
  {$ENDIF}
  DisposeObject(sendDE);
end;

function TCommunicationFramework_DoubleTunnelClient_NoAuth.GetFile(fileName, saveToPath: string): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveToPath);
  sendDE.WritePointer(0);

  FSendTunnel.WaitSendStreamCmd('GetFile', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.PostFile(fileName: string);
var
  sendDE: TDataFrameEngine;
  fs    : TCoreClassFileStream;
  md5   : UnicodeMixedLib.TMD5;
begin
  if not umlFileExists(fileName) then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  fs := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd('PostFileInfo', sendDE);
  DisposeObject(sendDE);

  md5 := umlStreamMD5(fs);

  fs.Position := 0;
  FSendTunnel.SendBigStream('PostFile', fs, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(md5);
  FSendTunnel.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.PostFile(fn: string; fileStream: TCoreClassStream);
var
  sendDE: TDataFrameEngine;
  md5   : UnicodeMixedLib.TMD5;
begin
  if (not FSendTunnel.Connected) or (not FRecvTunnel.Connected) then
    begin
      DisposeObject(fileStream);
      Exit;
    end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(umlGetFileName(fn));
  sendDE.WriteInt64(fileStream.Size);
  FSendTunnel.SendDirectStreamCmd('PostFileInfo', sendDE);
  DisposeObject(sendDE);

  fileStream.Position := 0;
  md5 := umlStreamMD5(fileStream);

  fileStream.Position := 0;
  FSendTunnel.SendBigStream('PostFile', fileStream, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(md5);
  FSendTunnel.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(0);
  SendTunnel.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  SendTunnel.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall);
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.init;
      p^.OnCall := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  SendTunnel.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  SendTunnel.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod);
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.init;
      p^.OnMethod := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  SendTunnel.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  SendTunnel.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc);
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.init;
      p^.OnProc := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  SendTunnel.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  SendTunnel.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;
{$ENDIF}


procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.ClearBatchStream;
var
  de: TDataFrameEngine;
  p : PPostBatchBackcallData_NoAuth;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendDirectStreamCmd('ClearBatchStream', de);
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.GetBatchStreamState(OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmd('GetBatchStreamState', de, OnResult);
  DisposeObject(de);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.GetBatchStreamState(OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmd('GetBatchStreamState', de, OnResult);
  DisposeObject(de);
end;
{$ENDIF}


function TCommunicationFramework_DoubleTunnelClient_NoAuth.GetBatchStreamState(ResultData: TDataFrameEngine; ATimeOut: TTimeTickValue): Boolean;
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.WaitSendStreamCmd('GetBatchStreamState', de, ResultData, ATimeOut);
  Result := ResultData.Count > 0;
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.RegisterCommand;
begin
  {$IFDEF FPC}
  FRecvTunnel.RegisterDirectStream('FileInfo').OnExecute := @Command_FileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := @Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := @Command_PostFileOver;
  FRecvTunnel.RegisterDirectStream('NewBatchStream').OnExecute := @Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream('PostBatchStream').OnExecute := @Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream('ClearBatchStream').OnExecute := @Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream('PostBatchStreamDone').OnExecute := @Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream('GetBatchStreamState').OnExecute := @Command_GetBatchStreamState;
  {$ELSE}
  FRecvTunnel.RegisterDirectStream('FileInfo').OnExecute := Command_FileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := Command_PostFileOver;
  FRecvTunnel.RegisterDirectStream('NewBatchStream').OnExecute := Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream('PostBatchStream').OnExecute := Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream('ClearBatchStream').OnExecute := Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream('PostBatchStreamDone').OnExecute := Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream('GetBatchStreamState').OnExecute := Command_GetBatchStreamState;
  {$ENDIF}
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD('FileInfo');
  FRecvTunnel.DeleteRegistedCMD('PostFile');
  FRecvTunnel.DeleteRegistedCMD('PostFileOver');

  FRecvTunnel.DeleteRegistedCMD('NewBatchStream');
  FRecvTunnel.DeleteRegistedCMD('PostBatchStream');
  FRecvTunnel.DeleteRegistedCMD('ClearBatchStream');
  FRecvTunnel.DeleteRegistedCMD('PostBatchStreamDone');
  FRecvTunnel.DeleteRegistedCMD('GetBatchStreamState');
end;

function TCommunicationFramework_DoubleTunnelClient_NoAuth.RemoteInited: Boolean;
begin
  Result := FSendTunnel.RemoteInited and FRecvTunnel.RemoteInited;
end;

end.
