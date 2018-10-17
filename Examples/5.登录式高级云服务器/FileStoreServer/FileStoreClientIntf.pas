unit FileStoreClientIntf;

interface

uses System.IOUtils, SysUtils, Types, DateUtils,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine, Cadencer,
  NotifyObjectBase,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Client_ICS;

type
  TFileStoreService_DoubleTunnelClient     = class;
  TFileStoreService_DoubleTunnelClientPool = class;

  TFileStoreCliConnectInfo = record
    DBServAddr, RegAddr: string;
    DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort: Word;
  end;

  IFileStoreClientInterface = interface
  end;

  TFileStoreService_DoubleTunnelClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  protected
  public
    Owner                               : TFileStoreService_DoubleTunnelClientPool;
    NetRecvTunnelIntf, NetSendTunnelIntf: TCommunicationFramework_Client_CrossSocket;
    ConnectInfo                         : TFileStoreCliConnectInfo;
    ReconnectTotal                      : Integer;

    constructor Create(AOwner: TFileStoreService_DoubleTunnelClientPool);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    function Connect(addr: string; const DBCliRecvPort, DBCliSendPort: Word): Boolean; override;
    procedure Disconnect; override;

    procedure AntiIdle(workLoad: Word);
    function RegHallServer(const RegAddr: string; const RegRecvPort, RegSendPort: Word): Boolean;

    procedure UserIsLock(userID: string; OnResult: TStreamMethod);
    procedure UserLock(userID: string);
    procedure UserUnLock(userID: string);
  end;

  TStoreFileInfoList = class;

  PStoreFileInfo = ^TStoreFileInfo;

  TStoreFileInfo = record
    Owner: TStoreFileInfoList;
    client: TFileStoreService_DoubleTunnelClient;
    completed: Boolean;
    Filename: string;
    FileLastTime: TDateTime;

    procedure Init;
    procedure GetFileTime_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
  end;

  TStoreFileInfoComplete = procedure(Sender: TStoreFileInfoList) of object;

  TStoreFileInfoList = class(TGenericsList<PStoreFileInfo>)
  public
    Filename       : string;
    Owner          : TFileStoreService_DoubleTunnelClientPool;
    CompleteTime   : Double;
    OnComplete     : TStoreFileInfoComplete;
    PeerClientData : TPeerClient;
    InData, OutData: TDataFrameEngine;
    SerialNo       : Integer;

    procedure SortOfFileLastTime;
    function AllCompleted: Boolean;
  end;

  TFileStoreCompleteInfoList = TGenericsList<TStoreFileInfoList>;

  TFileStoreUserStateList = class;

  PFileStoreUserState = ^TFileStoreUserState;

  TFileStoreUserState = record
    Owner: TFileStoreUserStateList;
    client: TFileStoreService_DoubleTunnelClient;
    completed: Boolean;
    userID: string;
    userState: Boolean;

    procedure Init;
    procedure State_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
  end;

  TFileStoreUserStateComplete = procedure(Sender: TFileStoreUserStateList) of object;

  TFileStoreUserStateList = class(TGenericsList<PFileStoreUserState>)
  public
    userID         : string;
    Owner          : TFileStoreService_DoubleTunnelClientPool;
    CompleteTime   : Double;
    OnComplete     : TFileStoreUserStateComplete;
    PeerClientData : TPeerClient;
    InData, OutData: TDataFrameEngine;

    function AllCompleted: Boolean;
  end;

  TDBCompleteUserStateList = TGenericsList<TFileStoreUserStateList>;

  TFileStoreService_DoubleTunnelClientPool = class(TCoreClassPersistent)
  protected
    FClientList            : TCoreClassListForObj;
    AntiIdleIsRun          : Boolean;
    CadencerEng            : TCadencer;
    PostProgress           : TNProgressPost;
    DBCompleteFileInfoList : TFileStoreCompleteInfoList;
    DBCompleteUserStateList: TDBCompleteUserStateList;

    function GetItems(index: Integer): TFileStoreService_DoubleTunnelClient;
    procedure SetItems(index: Integer; const Value: TFileStoreService_DoubleTunnelClient);

    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);

    procedure PostExecuteProc_StoreFileInfoListComplete(Sender: TNPostExecute);

    procedure PostExecuteProc_FileStoreUserStateListComplete(Sender: TNPostExecute);
  public
    DBClientInterface: IFileStoreClientInterface;

    constructor Create(const ADBClientInterface: IFileStoreClientInterface);
    destructor Destroy; override;

    function Count: Integer;
    property Items[index: Integer]: TFileStoreService_DoubleTunnelClient read GetItems write SetItems; default;

    procedure Clear;

    procedure Progress;
    procedure AntiIdle(workLoad: Word);
    function BuildClientAndConnect(const DBServAddr, RegAddr: string; const DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort: Word): Boolean;

    // async method query
    function QueryDBFile(RemoteFileName: string; completeProc: TStoreFileInfoComplete): TStoreFileInfoList;
    function QueryUserIsLock(userID: string; completeProc: TFileStoreUserStateComplete): TFileStoreUserStateList;
  end;

implementation

constructor TFileStoreService_DoubleTunnelClient.Create(AOwner: TFileStoreService_DoubleTunnelClientPool);
begin
  Owner := AOwner;
  NetRecvTunnelIntf := TCommunicationFramework_Client_CrossSocket.Create;
  NetSendTunnelIntf := TCommunicationFramework_Client_CrossSocket.Create;
  NetSendTunnelIntf.PrintParams['AntiIdle'] := False;
  ConnectInfo.DBServAddr := '';
  ConnectInfo.RegAddr := '';
  ConnectInfo.RegRecvPort := 0;
  ConnectInfo.RegSendPort := 0;
  ReconnectTotal := 0;
  inherited Create(NetRecvTunnelIntf, NetSendTunnelIntf);

  SwitchAsMaxPerformance;
end;

destructor TFileStoreService_DoubleTunnelClient.Destroy;
begin
  Disconnect;
  DisposeObject(NetRecvTunnelIntf);
  DisposeObject(NetSendTunnelIntf);
  inherited Destroy;
end;

procedure TFileStoreService_DoubleTunnelClient.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TFileStoreService_DoubleTunnelClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

function TFileStoreService_DoubleTunnelClient.Connect(addr: string; const DBCliRecvPort, DBCliSendPort: Word): Boolean;
var
  t: Cardinal;
begin
  Result := False;
  Disconnect;
  RegisterCommand;

  if not NetSendTunnelIntf.Connect(addr, IntToStr(DBCliSendPort)) then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
    end;
  if not NetRecvTunnelIntf.Connect(addr, IntToStr(DBCliRecvPort)) then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
    end;

  t := TCoreClassThread.GetTickCount + 4000;
  while not RemoteInited do
    begin
      if TCoreClassThread.GetTickCount > t then
          break;
      if not Connected then
          break;
      Progress;
    end;

  if Connected then
    begin
      DoStatus('connect FileStore service "%s" ok!', [addr]);
      Result := TunnelLink;

      if Result then
        begin
          ConnectInfo.DBServAddr := addr;
          ConnectInfo.DBCliRecvPort := DBCliRecvPort;
          ConnectInfo.DBCliSendPort := DBCliSendPort;
        end;
    end;
end;

procedure TFileStoreService_DoubleTunnelClient.Disconnect;
begin
  UnRegisterCommand;
  NetSendTunnelIntf.Disconnect;
  NetRecvTunnelIntf.Disconnect;
end;

procedure TFileStoreService_DoubleTunnelClient.AntiIdle(workLoad: Word);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteWORD(workLoad);
  SendTunnel.SendDirectStreamCmd('AntiIdle', sendDE);
  DisposeObject(sendDE);
end;

function TFileStoreService_DoubleTunnelClient.RegHallServer(const RegAddr: string; const RegRecvPort, RegSendPort: Word): Boolean;
var
  SendData, ResultData: TDataFrameEngine;
begin
  ConnectInfo.RegAddr := RegAddr;
  ConnectInfo.RegRecvPort := RegRecvPort;
  ConnectInfo.RegSendPort := RegSendPort;

  Result := False;

  SendData := TDataFrameEngine.Create;
  ResultData := TDataFrameEngine.Create;

  SendData.WriteString(RegAddr);
  SendData.WriteWORD(RegRecvPort);
  SendData.WriteWORD(RegSendPort);

  DoStatus('send reg Hall to FileStore cmd [addr:%s][r:%d][s:%d][w:%d]', [RegAddr, RegRecvPort, RegSendPort, 0]);

  SendTunnel.WaitSendStreamCmd('RegHallServer', SendData, ResultData, 5000);

  if ResultData.Count = 2 then
    begin
      Result := ResultData.Reader.ReadBool;
      DoStatus(ResultData.Reader.ReadString);
    end;

  DisposeObject(SendData);
  DisposeObject(ResultData);
end;

procedure TFileStoreService_DoubleTunnelClient.UserIsLock(userID: string; OnResult: TStreamMethod);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(userID);

  SendTunnel.SendStreamCmdM('UserIsLock', sendDE, OnResult);

  DisposeObject(sendDE);
end;

procedure TFileStoreService_DoubleTunnelClient.UserLock(userID: string);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(userID);

  SendTunnel.SendDirectStreamCmd('UserLock', sendDE);

  DisposeObject(sendDE);
end;

procedure TFileStoreService_DoubleTunnelClient.UserUnLock(userID: string);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(userID);

  SendTunnel.SendDirectStreamCmd('UserUnLock', sendDE);

  DisposeObject(sendDE);
end;

procedure TStoreFileInfo.Init;
begin
  Owner := nil;
  client := nil;
  completed := False;
  Filename := '';
  FileLastTime := 0;
end;

procedure TStoreFileInfo.GetFileTime_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  completed := True;
  if ResultData.Reader.ReadBool then
    begin
      Filename := ResultData.Reader.ReadString;
      FileLastTime := ResultData.Reader.ReadDouble;
    end;
  if (Owner.AllCompleted) then
    with Owner.Owner.PostProgress.PostExecuteM(1, Owner.Owner.PostExecuteProc_StoreFileInfoListComplete) do
        Data1 := self.Owner;
end;

procedure TStoreFileInfoList.SortOfFileLastTime;

  function SortComp(const v1, v2: PStoreFileInfo): TValueRelationship; inline;
  begin
    Result := CompareDateTime(v1^.FileLastTime, v2^.FileLastTime);
  end;

type
  TArrayStoreFileInfo = array of PStoreFileInfo;
  PArrayStoreFileInfo = ^TArrayStoreFileInfo;

  procedure QuickSortList(var SortList: TArrayStoreFileInfo; l, r: Integer); inline;
  var
    i, j: Integer;
    p, t: PStoreFileInfo;
  begin
    repeat
      i := l;
      j := r;
      p := SortList[(l + r) shr 1];
      repeat
        while SortComp(SortList[i], p) < 0 do
            Inc(i);
        while SortComp(SortList[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if l < j then
          QuickSortList(SortList, l, j);
      l := i;
    until i >= r;
  end;

var
  pbuff: PArrayStoreFileInfo;
begin
  if Count > 1 then
    begin
      pbuff := ListData;
      QuickSortList(pbuff^, 0, Count - 1);
    end;
end;

function TStoreFileInfoList.AllCompleted: Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 0 to Count - 1 do
    if (Items[i]^.client.Connected) and (Items[i]^.client.LinkOk) then
        Result := Result and Items[i]^.completed;
end;

procedure TFileStoreUserState.Init;
begin
  Owner := nil;
  client := nil;
  completed := False;
  userID := '';
  userState := False;
end;

procedure TFileStoreUserState.State_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  completed := True;
  self.userState := ResultData.Reader.ReadBool;

  if (Owner.AllCompleted) then
    with Owner.Owner.PostProgress.PostExecuteM(1, Owner.Owner.PostExecuteProc_FileStoreUserStateListComplete) do
        Data1 := self.Owner;
end;

function TFileStoreUserStateList.AllCompleted: Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 0 to Count - 1 do
    if (Items[i]^.client.Connected) and (Items[i]^.client.LinkOk) then
        Result := Result and Items[i]^.completed;
end;

function TFileStoreService_DoubleTunnelClientPool.GetItems(index: Integer): TFileStoreService_DoubleTunnelClient;
begin
  Result := FClientList[index] as TFileStoreService_DoubleTunnelClient;
end;

procedure TFileStoreService_DoubleTunnelClientPool.SetItems(index: Integer; const Value: TFileStoreService_DoubleTunnelClient);
begin
  FClientList[index] := Value;
end;

procedure TFileStoreService_DoubleTunnelClientPool.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
var
  i           : Integer;
  FileInfoLst : TStoreFileInfoList;
  UserStateLst: TFileStoreUserStateList;
begin
  i := 0;
  while i < DBCompleteFileInfoList.Count do
    begin
      FileInfoLst := DBCompleteFileInfoList[i];
      if FileInfoLst.AllCompleted then
          DBCompleteFileInfoList.Delete(i)
      else
        begin
          FileInfoLst.CompleteTime := FileInfoLst.CompleteTime - deltaTime;
          if FileInfoLst.CompleteTime <= 0 then
            begin
              with PostProgress.PostExecuteM(1, PostExecuteProc_StoreFileInfoListComplete) do
                begin
                  Data1 := FileInfoLst;
                end;
              DBCompleteFileInfoList.Delete(i)
            end
          else
              Inc(i);
        end;
    end;

  i := 0;
  while i < DBCompleteUserStateList.Count do
    begin
      UserStateLst := DBCompleteUserStateList[i];
      if UserStateLst.AllCompleted then
          DBCompleteUserStateList.Delete(i)
      else
        begin
          UserStateLst.CompleteTime := UserStateLst.CompleteTime - deltaTime;
          if UserStateLst.CompleteTime <= 0 then
            begin
              with PostProgress.PostExecuteM(1, PostExecuteProc_FileStoreUserStateListComplete) do
                begin
                  Data1 := UserStateLst;
                end;
              DBCompleteUserStateList.Delete(i)
            end
          else
              Inc(i);
        end;
    end;

  PostProgress.Progress(deltaTime);
end;

procedure TFileStoreService_DoubleTunnelClientPool.PostExecuteProc_StoreFileInfoListComplete(Sender: TNPostExecute);
var
  l: TStoreFileInfoList;
  p: PStoreFileInfo;
begin
  l := TStoreFileInfoList(Sender.Data1);

  l.SortOfFileLastTime;

  if Assigned(l.OnComplete) then
      l.OnComplete(l);

  for p in l do
      dispose(p);

  DisposeObject(l);
end;

procedure TFileStoreService_DoubleTunnelClientPool.PostExecuteProc_FileStoreUserStateListComplete(Sender: TNPostExecute);
var
  l: TFileStoreUserStateList;
  p: PFileStoreUserState;
begin
  l := TFileStoreUserStateList(Sender.Data1);

  if Assigned(l.OnComplete) then
      l.OnComplete(l);

  for p in l do
      dispose(p);

  DisposeObject(l);
end;

constructor TFileStoreService_DoubleTunnelClientPool.Create(const ADBClientInterface: IFileStoreClientInterface);
begin
  inherited Create;
  FClientList := TCoreClassListForObj.Create;
  AntiIdleIsRun := False;
  DBClientInterface := ADBClientInterface;
  CadencerEng := TCadencer.Create;
  CadencerEng.OnProgress := CadencerProgress;
  PostProgress := TNProgressPost.Create;
  DBCompleteFileInfoList := TFileStoreCompleteInfoList.Create;
  DBCompleteUserStateList := TDBCompleteUserStateList.Create;
end;

destructor TFileStoreService_DoubleTunnelClientPool.Destroy;
begin
  Clear;
  DisposeObject(FClientList);
  DisposeObject(CadencerEng);
  DisposeObject(PostProgress);
  DisposeObject(DBCompleteFileInfoList);
  DisposeObject(DBCompleteUserStateList);
  inherited Destroy;
end;

function TFileStoreService_DoubleTunnelClientPool.Count: Integer;
begin
  Result := FClientList.Count;
end;

procedure TFileStoreService_DoubleTunnelClientPool.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Count - 1 do
      begin
        Items[i].Disconnect;
        DisposeObject(Items[i]);
      end;
  except
  end;
  FClientList.Clear;
end;

procedure TFileStoreService_DoubleTunnelClientPool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
  CadencerEng.Progress;
end;

procedure TFileStoreService_DoubleTunnelClientPool.AntiIdle(workLoad: Word);
var
  i       : Integer;
  conninfo: TFileStoreCliConnectInfo;
  c       : TFileStoreService_DoubleTunnelClient;
begin
  if AntiIdleIsRun then
      exit;
  AntiIdleIsRun := True;
  try
    i := 0;
    while i < Count do
      begin
        c := Items[i];
        if (not c.LinkOk) or (not c.Connected) then
          begin
            conninfo := c.ConnectInfo;
            Inc(c.ReconnectTotal);

            if c.Connect(conninfo.DBServAddr, conninfo.DBCliRecvPort, conninfo.DBCliSendPort) then
              begin
                DoStatus('reconnect call reg Hall to FileStore send cmd:%s [addr:%s][r:%d][s:%d][w:%d]',
                  [conninfo.DBServAddr, conninfo.RegAddr, conninfo.RegRecvPort, conninfo.RegSendPort, 0]);
                if c.RegHallServer(conninfo.RegAddr, conninfo.RegRecvPort, conninfo.RegSendPort) then
                  begin
                    Inc(i);
                    continue;
                  end;
                c.Disconnect;
              end;
          end
        else
            c.AntiIdle(workLoad);
        Inc(i);
      end;
  except
  end;
  AntiIdleIsRun := False;
end;

function TFileStoreService_DoubleTunnelClientPool.BuildClientAndConnect(const DBServAddr, RegAddr: string; const DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort: Word): Boolean;
var
  c: TFileStoreService_DoubleTunnelClient;
  i: Integer;
begin
  Result := False;

  for i := 0 to Count - 1 do
    begin
      c := Items[i];
      if SameText(c.ConnectInfo.DBServAddr, DBServAddr) and
        SameText(c.ConnectInfo.RegAddr, RegAddr) and
        (c.ConnectInfo.RegRecvPort = RegRecvPort) and
        (c.ConnectInfo.RegSendPort = RegSendPort) then
        begin
          c.ReconnectTotal := 0;
          exit;
        end;
    end;

  c := TFileStoreService_DoubleTunnelClient.Create(self);
  if c.Connect(DBServAddr, DBCliRecvPort, DBCliSendPort) then
    begin
      DoStatus('call reg hall to FileStore api send cmd [addr:%s][r:%d][s:%d][w:%d]', [RegAddr, RegRecvPort, RegSendPort, 0]);
      Result := c.RegHallServer(RegAddr, RegRecvPort, RegSendPort);
      if Result then
        begin
          FClientList.Add(c);
        end;
    end
  else
      DisposeObject(c);
end;

function TFileStoreService_DoubleTunnelClientPool.QueryDBFile(RemoteFileName: string; completeProc: TStoreFileInfoComplete): TStoreFileInfoList;
var
  i: Integer;
  c: TFileStoreService_DoubleTunnelClient;
  l: TStoreFileInfoList;
  p: PStoreFileInfo;
begin
  l := TStoreFileInfoList.Create;
  l.Filename := RemoteFileName;
  l.Owner := self;
  l.CompleteTime := 10;
  l.OnComplete := completeProc;
  l.PeerClientData := nil;
  l.InData := nil;
  l.OutData := nil;
  l.SerialNo := 0;

  for i := 0 to Count - 1 do
    begin
      c := Items[i];
      if (c.Connected) and (c.LinkOk) then
        begin
          new(p);

          p^.Init;
          p^.Owner := l;
          p^.client := c;
          l.Add(p);
          c.GetFileTimeM(RemoteFileName, p^.GetFileTime_StreamResult);
        end;
    end;
  DBCompleteFileInfoList.Add(l);
  Result := l;
end;

function TFileStoreService_DoubleTunnelClientPool.QueryUserIsLock(userID: string; completeProc: TFileStoreUserStateComplete): TFileStoreUserStateList;
var
  i: Integer;
  c: TFileStoreService_DoubleTunnelClient;
  l: TFileStoreUserStateList;
  p: PFileStoreUserState;
begin
  l := TFileStoreUserStateList.Create;
  l.userID := userID;
  l.Owner := self;
  l.CompleteTime := 10;
  l.OnComplete := completeProc;
  l.PeerClientData := nil;
  l.InData := nil;
  l.OutData := nil;

  for i := 0 to Count - 1 do
    begin
      c := Items[i];
      if (c.Connected) and (c.LinkOk) then
        begin
          new(p);

          p^.Init;
          p^.Owner := l;
          p^.client := c;
          l.Add(p);
          c.UserIsLock(userID, p^.State_StreamResult);
        end;
    end;
  DBCompleteUserStateList.Add(l);
  Result := l;
end;

end.
