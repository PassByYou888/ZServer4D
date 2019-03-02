unit AC2LogicMobileClient;

interface

uses System.SysUtils, System.IOUtils,
  CommunicationFrameworkDoubleTunnelIO,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine,
  NotifyObjectBase, MemoryStream64;

type
  ILogicBackCallInterface = interface
    procedure LogicDisconnect;
  end;

  TFileDownloadCompleteProc = reference to procedure(filename: string; fileSour: TMemoryStream64);
  PFileDownloadCompleteData = ^TFileDownloadCompleteData;

  TFileDownloadCompleteData = record
    filename: string;
    OnComplete: TFileDownloadCompleteProc;
  end;

  TLogicClientBase = class(TCommunicationFramework_DoubleTunnelClient)
  protected
    FCacheUserInfo: THashObjectList;
    NetRecvTunnelIntf, NetSendTunnelIntf: TCommunicationFrameworkClient;
    LogicBackCallInterface: ILogicBackCallInterface;
    LastConnectState: Boolean;
  protected
    procedure ClientConnected(Sender: TCommunicationFrameworkClient); override;
    procedure Disconnect_PostExecute(Sender: TNPostExecute);
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); override;
  protected
    // file operation hook
    procedure Command_FileInfo(Sender: TPeerClient; InData: TDataFrameEngine); override;
  protected
    procedure cmd_UserChange(Sender: TPeerClient; InData: string);

    procedure cmd_SendLogicFileCompleted(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_SendAdvertisementFileCompleted(Sender: TPeerClient; InData: TDataFrameEngine);
  public
    AutoReconnect: Boolean;
    constructor Create(ALogicBackCallInterface: ILogicBackCallInterface; ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure Progress; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Disconnect; override;

    procedure AntiIdle();
    function RegGuestUser(out UserName, UserPasswd: string): Boolean;

    procedure GetLogicFileList(OnResult: TStreamProc);
    procedure GetLogicFile(fn: string; OnComplete: TFileDownloadCompleteProc);

    procedure GetAdvertisementFileList(OnResult: TStreamProc);
    procedure GetAdvertisementFile(fn: string; OnComplete: TFileDownloadCompleteProc);

    property CacheUserInfo: THashObjectList read FCacheUserInfo;
    function GetUserInfo(force: Boolean; UserID: string): TSectionTextData;
    function GetUserAlias(UserID: string): string;

    function ChangeAlias(NewAlias: string): Boolean;

    procedure ClearUserCache(UserID: string);
  end;

implementation

procedure TLogicClientBase.ClientConnected(Sender: TCommunicationFrameworkClient);
begin
end;

procedure TLogicClientBase.Disconnect_PostExecute(Sender: TNPostExecute);
begin
  LogicBackCallInterface.LogicDisconnect;
end;

procedure TLogicClientBase.ClientDisconnect(Sender: TCommunicationFrameworkClient);
begin
  if AutoReconnect then
      ProgressEngine.PostExecuteM(0.5, Disconnect_PostExecute);
end;

procedure TLogicClientBase.Command_FileInfo(Sender: TPeerClient; InData: TDataFrameEngine);
var
  fn: string;
  fsize: Int64;
  remoteinfo: string;
  fullfn: string;
begin
  if FCurrentStream <> nil then
    begin
      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  fn := InData.Reader.ReadString;
  fsize := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;

  fullfn := umlCombinePath(remoteinfo, fn);
  FCurrentReceiveStreamFileName := fullfn;
  FCurrentStream := TMemoryStream64.Create;
end;

procedure TLogicClientBase.cmd_UserChange(Sender: TPeerClient; InData: string);
begin
  ClearUserCache(InData);
end;

procedure TLogicClientBase.cmd_SendLogicFileCompleted(Sender: TPeerClient; InData: TDataFrameEngine);
var
  p: PFileDownloadCompleteData;
  ph: string;
  fn: string;
begin
  p := PFileDownloadCompleteData(InData.Reader.ReadPointer);
  if p = nil then
      exit;
  try
    if Assigned(p^.OnComplete) then
      if Sender.UserDefine.BigStreamBatchList.Count > 0 then
        begin
          try
            ph := System.IOUtils.TPath.GetDocumentsPath;
            fn := umlCombineFileName(ph, umlMD5ToStr(Sender.UserDefine.BigStreamBatchList.Last^.SourceMD5)).Text;
            Sender.UserDefine.BigStreamBatchList.Last^.Source.SaveToFile(fn);
          except
          end;

          try
            Sender.UserDefine.BigStreamBatchList.Last^.Source.Position := 0;
            p^.OnComplete(p^.filename, Sender.UserDefine.BigStreamBatchList.Last^.Source);
          except
          end;
          Sender.UserDefine.BigStreamBatchList.DeleteLast;
        end;
  except
  end;
  dispose(p);
end;

procedure TLogicClientBase.cmd_SendAdvertisementFileCompleted(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  cmd_SendLogicFileCompleted(Sender, InData);
end;

constructor TLogicClientBase.Create(ALogicBackCallInterface: ILogicBackCallInterface; ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  NetRecvTunnelIntf := ARecvTunnel;
  NetSendTunnelIntf := ASendTunnel;
  inherited Create(NetRecvTunnelIntf, NetSendTunnelIntf);
  LogicBackCallInterface := ALogicBackCallInterface;
  NetSendTunnelIntf.PrintParams['AntiIdle'] := False;
  FCacheUserInfo := THashObjectList.Create(True);
  LastConnectState := False;
  AutoReconnect := False;

  SwitchAsDefaultPerformance;
end;

destructor TLogicClientBase.Destroy;
begin
  Disconnect;
  DisposeObject(FCacheUserInfo);
  inherited Destroy;
end;

procedure TLogicClientBase.Progress;
begin
  inherited Progress;

  if not NetSendTunnelIntf.Connected then
    begin
      LastConnectState := False;
      exit;
    end;

  if NetSendTunnelIntf.ClientIO.StopCommunicationTime > 5 * 1000 then
    begin
      NetSendTunnelIntf.WaitP(5000, procedure(const cState: Boolean)
        begin
          if cState then
            begin
              NetSendTunnelIntf.ClientIO.UpdateLastCommunicationTime;
              NetRecvTunnelIntf.ClientIO.UpdateLastCommunicationTime;
            end
          else
            begin
              NetSendTunnelIntf.Disconnect;
            end;
        end);
    end;
end;

procedure TLogicClientBase.RegisterCommand;
begin
  inherited RegisterCommand;
  FRecvTunnel.RegisterDirectConsole('UserChange').OnExecute := cmd_UserChange;

  FRecvTunnel.RegisterDirectStream('SendLogicFileCompleted').OnExecute := cmd_SendLogicFileCompleted;
  FRecvTunnel.RegisterDirectStream('SendAdvertisementFileCompleted').OnExecute := cmd_SendAdvertisementFileCompleted;
end;

procedure TLogicClientBase.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('UserChange');

  FRecvTunnel.DeleteRegistedCMD('SendLogicFileCompleted');
  FRecvTunnel.DeleteRegistedCMD('SendAdvertisementFileCompleted');
end;

procedure TLogicClientBase.Disconnect;
begin
  UnRegisterCommand;
  if NetSendTunnelIntf.ClientIO <> nil then
      NetSendTunnelIntf.ClientIO.Disconnect;
  if NetRecvTunnelIntf.ClientIO <> nil then
      NetRecvTunnelIntf.ClientIO.Disconnect;
end;

procedure TLogicClientBase.AntiIdle;
var
  sendDE: TDataFrameEngine;
begin
  if not Connected then
      exit;
  if SendTunnel.ClientIO.WaitSendBusy then
      exit;
  sendDE := TDataFrameEngine.Create;
  SendTunnel.SendDirectStreamCmd('AntiIdle', sendDE);
  DisposeObject(sendDE);
end;

function TLogicClientBase.RegGuestUser(out UserName, UserPasswd: string): Boolean;
var
  sendDE: TDataFrameEngine;
  ResultDE: TDataFrameEngine;
begin
  Result := False;

  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;
  SendTunnel.WaitSendStreamCmd('RegGuestUser', sendDE, ResultDE, 10000);

  if ResultDE.Count > 0 then
    begin
      Result := ResultDE.Reader.ReadBool;
      if Result then
        begin
          UserName := ResultDE.Reader.ReadString;
          UserPasswd := ResultDE.Reader.ReadString;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(ResultDE);
end;

procedure TLogicClientBase.GetLogicFileList(OnResult: TStreamProc);
begin
  SendTunnel.SendStreamCmdP('GetLogicFileList', nil, OnResult);
end;

procedure TLogicClientBase.GetLogicFile(fn: string; OnComplete: TFileDownloadCompleteProc);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fn);
  SendTunnel.SendStreamCmdP('GetLogicFileMD5', sendDE,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      sendDE: TDataFrameEngine;
      p: PFileDownloadCompleteData;
      ph: string;
      md5: UnicodeMixedLib.TMD5;
      m64: TMemoryStream64;
    begin
      if ResultData.Count > 0 then
          md5 := ResultData.ReadMD5(0)
      else
          md5 := NullMD5;

      if not umlIsNullMD5(md5) then
        begin
          ph := System.IOUtils.TPath.GetDocumentsPath;
          if umlFileExists(umlCombineFileName(ph, umlMD5ToStr(md5))) then
            begin
              m64 := TMemoryStream64.Create;
              m64.LoadFromFile(umlCombineFileName(ph, umlMD5ToStr(md5)));
              m64.Position := 0;
              OnComplete(fn, m64);
              DisposeObject(m64);
              exit;
            end;
        end;

      sendDE := TDataFrameEngine.Create;
      sendDE.WriteString(fn);
      new(p);
      p^.filename := fn;
      p^.OnComplete := OnComplete;
      sendDE.WritePointer(p);
      SendTunnel.SendDirectStreamCmd('GetLogicFile', sendDE);
      DisposeObject(sendDE);
    end);
  DisposeObject(sendDE);
end;

procedure TLogicClientBase.GetAdvertisementFileList(OnResult: TStreamProc);
begin
  SendTunnel.SendStreamCmdP('GetAdvertisementFileList', nil, OnResult);
end;

procedure TLogicClientBase.GetAdvertisementFile(fn: string; OnComplete: TFileDownloadCompleteProc);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fn);
  SendTunnel.SendStreamCmdP('GetAdvertisementFileMD5', sendDE,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      sendDE: TDataFrameEngine;
      p: PFileDownloadCompleteData;
      ph: string;
      md5: UnicodeMixedLib.TMD5;
      m64: TMemoryStream64;
    begin
      if ResultData.Count > 0 then
          md5 := ResultData.ReadMD5(0)
      else
          md5 := NullMD5;

      if not umlIsNullMD5(md5) then
        begin
          ph := System.IOUtils.TPath.GetDocumentsPath;
          if umlFileExists(umlCombineFileName(ph, umlMD5ToStr(md5))) then
            begin
              m64 := TMemoryStream64.Create;
              m64.LoadFromFile(umlCombineFileName(ph, umlMD5ToStr(md5)));
              m64.Position := 0;
              OnComplete(fn, m64);
              DisposeObject(m64);
              exit;
            end;
        end;

      sendDE := TDataFrameEngine.Create;
      sendDE.WriteString(fn);
      new(p);
      p^.filename := fn;
      p^.OnComplete := OnComplete;
      sendDE.WritePointer(p);
      SendTunnel.SendDirectStreamCmd('GetAdvertisementFile', sendDE);
      DisposeObject(sendDE);
    end);
  DisposeObject(sendDE);
end;

function TLogicClientBase.GetUserInfo(force: Boolean; UserID: string): TSectionTextData;
var
  sendDE, ResultDE: TDataFrameEngine;
begin
  if force then
      FCacheUserInfo.Delete(UserID);

  Result := FCacheUserInfo[UserID] as TSectionTextData;
  if Result <> nil then
      exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(UserID);

  ResultDE := TDataFrameEngine.Create;

  SendTunnel.WaitSendStreamCmd('GetUserInfo', sendDE, ResultDE, 5000);
  if ResultDE.Count > 0 then
    begin
      if ResultDE.Reader.ReadBool then
        begin
          Result := TSectionTextData.Create;
          ResultDE.Reader.ReadSectionText(Result);
          FCacheUserInfo.Add(Result.GetDefaultValue('UserInfo', 'UserID', UserID), Result);
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(ResultDE);
end;

function TLogicClientBase.GetUserAlias(UserID: string): string;
var
  te: TSectionTextData;
begin
  te := GetUserInfo(False, UserID);
  if te <> nil then
      Result := te.GetDefaultValue('UserInfo', 'Alias', UserID)
  else
      Result := UserID;
end;

function TLogicClientBase.ChangeAlias(NewAlias: string): Boolean;
var
  sendDE, ResultDE: TDataFrameEngine;
begin
  Result := False;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(NewAlias);

  ResultDE := TDataFrameEngine.Create;

  SendTunnel.WaitSendStreamCmd('ChangeAlias', sendDE, ResultDE, 5000);

  if ResultDE.Count > 0 then
      Result := ResultDE.Reader.ReadBool;

  DisposeObject(sendDE);
  DisposeObject(ResultDE);
end;

procedure TLogicClientBase.ClearUserCache(UserID: string);
begin
  FCacheUserInfo.Delete(UserID);
end;

end.
