{ ****************************************************************************** }
{ * double tunnel IO framework(incl File service)                              * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }

unit CommunicationFrameworkDoubleTunnelIO_NoAuth;

interface

{$I ..\zDefine.inc}


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
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure SwitchServiceAsMaxPerformance;
    procedure SwitchServiceAsMaxSafe;
    procedure SwitchServiceAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;
    function GetUserDefineRecvTunnel(RecvCli: TPeerClient): TPeerClientUserDefineForRecvTunnel_NoAuth;

    function TotalLinkCount: Integer;

    property CanStatus: Boolean read FCanStatus write FCanStatus;
    property CadencerEngine: TCadencer read FCadencerEngine;
    property ProgressEngine: TNProgressPost read FProgressEngine;
    property FileReceiveDirectory: string read FFileReceiveDirectory;

    property RecvTunnel: TCommunicationFrameworkServer read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkServer read FSendTunnel;
  end;

  TCommunicationFramework_DoubleTunnelClient_NoAuth = class;

  TPeerClientUserDefineForDoubleTunnelClient_NoAuth = class(TPeerClientUserDefine)
  public
    Client: TCommunicationFramework_DoubleTunnelClient_NoAuth;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TFileComplete_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject; Stream: TCoreClassStream; const fileName: string) of object;

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
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    function Connected: Boolean; virtual;

    procedure SwitchServiceAsMaxPerformance;
    procedure SwitchServiceAsMaxSafe;
    procedure SwitchServiceAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

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
  RemoteBackcallAddr := InData.Reader.ReadUInt64;

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
  sendDE.WriteUInt64(RemoteBackcallAddr);
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

  SwitchServiceAsDefaultPerformance;
end;

destructor TCommunicationFramework_DoubleTunnelService_NoAuth.Destroy;
begin
  DisposeObject(FCadencerEngine);
  DisposeObject(FProgressEngine);
  inherited Destroy;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.SwitchServiceAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.SwitchServiceAsMaxSafe;
begin
  FRecvTunnel.SwitchMaxSafe;
  FSendTunnel.SwitchMaxSafe;
end;

procedure TCommunicationFramework_DoubleTunnelService_NoAuth.SwitchServiceAsDefaultPerformance;
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
  {$ELSE}
  FRecvTunnel.RegisterStream('TunnelLink').OnExecute := Command_TunnelLink;
  FRecvTunnel.RegisterStream('GetCurrentCadencer').OnExecute := Command_GetCurrentCadencer;
  FRecvTunnel.RegisterStream('GetFileTime').OnExecute := Command_GetFileTime;
  FRecvTunnel.RegisterStream('GetFile').OnExecute := Command_GetFile;
  FRecvTunnel.RegisterDirectStream('PostFileInfo').OnExecute := Command_PostFileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := Command_PostFileOver;
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
end;

function TCommunicationFramework_DoubleTunnelService_NoAuth.GetUserDefineRecvTunnel(RecvCli: TPeerClient): TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  Result := RecvCli.UserDefine as TPeerClientUserDefineForRecvTunnel_NoAuth;
end;

function TCommunicationFramework_DoubleTunnelService_NoAuth.TotalLinkCount: Integer;
begin
  Result := RecvTunnel.Count;
end;

constructor TPeerClientUserDefineForDoubleTunnelClient_NoAuth.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  Client := nil;
end;

destructor TPeerClientUserDefineForDoubleTunnelClient_NoAuth.Destroy;
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
  RemoteBackcallAddr := InData.Reader.ReadUInt64;
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
            dispose(p);
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

constructor TCommunicationFramework_DoubleTunnelClient_NoAuth.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  inherited Create;
  FRecvTunnel := ARecvTunnel;
  FRecvTunnel.NotyifyInterface := Self;
  FRecvTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForDoubleTunnelClient_NoAuth;

  FSendTunnel := ASendTunnel;
  FSendTunnel.NotyifyInterface := Self;
  FSendTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForDoubleTunnelClient_NoAuth;

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

  SwitchServiceAsDefaultPerformance;
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

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.SwitchServiceAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.SwitchServiceAsMaxSafe;
begin
  FRecvTunnel.SwitchMaxSafe;
  FSendTunnel.SwitchMaxSafe;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.SwitchServiceAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.Progress;
begin
  FCadencerEngine.Progress;

  try
    if Connected then
      begin
        FRecvTunnel.ProgressBackground;
        FSendTunnel.ProgressBackground;
      end
    else
      begin
        FLinkOk := False;
      end;
  except
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
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
          TPeerClientUserDefineForDoubleTunnelClient_NoAuth(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TPeerClientUserDefineForDoubleTunnelClient_NoAuth(FRecvTunnel.ClientIO.UserDefine).Client := Self;
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
              TPeerClientUserDefineForDoubleTunnelClient_NoAuth(FSendTunnel.ClientIO.UserDefine).Client := Self;
              TPeerClientUserDefineForDoubleTunnelClient_NoAuth(FRecvTunnel.ClientIO.UserDefine).Client := Self;
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
  dispose(p);
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
  sendDE.WriteUInt64(UInt64(Pointer(p)));

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
  sendDE.WriteUInt64(0);

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

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.RegisterCommand;
begin
  {$IFDEF FPC}
  FRecvTunnel.RegisterDirectStream('FileInfo').OnExecute := @Command_FileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := @Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := @Command_PostFileOver;
  {$ELSE}
  FRecvTunnel.RegisterDirectStream('FileInfo').OnExecute := Command_FileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := Command_PostFileOver;
  {$ENDIF}
end;

procedure TCommunicationFramework_DoubleTunnelClient_NoAuth.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD('FileInfo');
  FRecvTunnel.DeleteRegistedCMD('PostFile');
  FRecvTunnel.DeleteRegistedCMD('PostFileOver');
end;

function TCommunicationFramework_DoubleTunnelClient_NoAuth.RemoteInited: Boolean;
begin
  Result := FSendTunnel.RemoteInited and FRecvTunnel.RemoteInited;
end;

end.
