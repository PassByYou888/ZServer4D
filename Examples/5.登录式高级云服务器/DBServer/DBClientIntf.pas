unit DBClientIntf;

interface

uses System.IOUtils, SysUtils, Types, DateUtils,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine, Cadencer,
  NotifyObjectBase,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Client_ICS;

type
  TDB_DoubleTunnelClient  = class;
  TDB_DoubleTunnelClients = class;

  TDBCliConnectInfo = record
    DBServAddr, RegAddr: string;
    DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort: Word;
  end;

  IDBClientInterface = interface
  end;

  TDB_DoubleTunnelClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  protected
  public
    Owner                               : TDB_DoubleTunnelClients;
    NetRecvTunnelIntf, NetSendTunnelIntf: TCommunicationFramework_Client_CrossSocket;
    ConnectInfo                         : TDBCliConnectInfo;
    ReconnectTotal                      : Integer;

    constructor Create(AOwner: TDB_DoubleTunnelClients);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    function Connect(addr: string; const DBCliRecvPort, DBCliSendPort: Word): Boolean;
    procedure Disconnect;

    procedure AntiIdle(workLoad: Word);
    function RegLoginServer(const RegAddr: string; const RegRecvPort, RegSendPort: Word): Boolean;

    procedure UserIsLock(userID: string; OnResult: TStreamMethod);
    procedure UserLock(userID: string);
    procedure UserUnLock(userID: string);

    procedure GetReplayInfo(userID: string; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
  end;

  TDBFileInfoList = class;

  PDBFileInfo = ^TDBFileInfo;

  TDBFileInfo = record
    Owner: TDBFileInfoList;
    client: TDB_DoubleTunnelClient;
    completed: Boolean;
    Filename: string;
    FileLastTime: TDateTime;

    procedure Init;
    procedure GetFileTime_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
  end;

  TDBFileInfoComplete = procedure(Sender: TDBFileInfoList) of object;

  TDBFileInfoList = class(TGenericsList<PDBFileInfo>)
  public
    Filename       : string;
    Owner          : TDB_DoubleTunnelClients;
    CompleteTime   : Double;
    OnComplete     : TDBFileInfoComplete;
    PeerClientData : TPeerClient;
    InData, OutData: TDataFrameEngine;
    SerialNo       : Integer;

    procedure SortOfFileLastTime;
    function AllCompleted: Boolean;
  end;

  TDBCompleteFileInfoList = TGenericsList<TDBFileInfoList>;

  TDBUserStateList = class;

  PDBUserState = ^TDBUserState;

  TDBUserState = record
    Owner: TDBUserStateList;
    client: TDB_DoubleTunnelClient;
    completed: Boolean;
    userID: string;
    userState: Boolean;

    procedure Init;
    procedure State_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
  end;

  TDBUserStateComplete = procedure(Sender: TDBUserStateList) of object;

  TDBUserStateList = class(TGenericsList<PDBUserState>)
  public
    userID         : string;
    Owner          : TDB_DoubleTunnelClients;
    CompleteTime   : Double;
    OnComplete     : TDBUserStateComplete;
    PeerClientData : TPeerClient;
    InData, OutData: TDataFrameEngine;

    function AllCompleted: Boolean;
  end;

  TDBCompleteUserStateList = TGenericsList<TDBUserStateList>;

  TDB_DoubleTunnelClients = class(TCoreClassPersistent)
  protected
    FClientList            : TCoreClassListForObj;
    AntiIdleIsRun          : Boolean;
    CadencerEng            : TCadencer;
    PostProgress           : TNProgressPost;
    DBCompleteFileInfoList : TDBCompleteFileInfoList;
    DBCompleteUserStateList: TDBCompleteUserStateList;

    function GetItems(index: Integer): TDB_DoubleTunnelClient;
    procedure SetItems(index: Integer; const Value: TDB_DoubleTunnelClient);

    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);

    procedure PostExecuteProc_DBFileInfoListComplete(Sender: TNPostExecute);

    procedure PostExecuteProc_DBUserStateListComplete(Sender: TNPostExecute);
  public
    DBClientInterface: IDBClientInterface;

    constructor Create(const ADBClientInterface: IDBClientInterface);
    destructor Destroy; override;

    function Count: Integer;
    property Items[index: Integer]: TDB_DoubleTunnelClient read GetItems write SetItems; default;

    procedure Clear;

    procedure Progress;
    procedure AntiIdle(workLoad: Word);
    function BuildClientAndConnect(const DBServAddr, RegAddr: string; const DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort: Word): Boolean;

    // async method query
    function QueryDBFile(RemoteFileName: string; completeProc: TDBFileInfoComplete): TDBFileInfoList;
    function QueryUserIsLock(userID: string; completeProc: TDBUserStateComplete): TDBUserStateList;
  end;

implementation

constructor TDB_DoubleTunnelClient.Create(AOwner: TDB_DoubleTunnelClients);
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
end;

destructor TDB_DoubleTunnelClient.Destroy;
begin
  Disconnect;
  DisposeObject(NetRecvTunnelIntf);
  DisposeObject(NetSendTunnelIntf);
  inherited Destroy;
end;

procedure TDB_DoubleTunnelClient.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TDB_DoubleTunnelClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

function TDB_DoubleTunnelClient.Connect(addr: string; const DBCliRecvPort, DBCliSendPort: Word): Boolean;
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
      DoStatus('connect DB service "%s" ok!', [addr]);
      Result := TunnelLink;

      if Result then
        begin
          ConnectInfo.DBServAddr := addr;
          ConnectInfo.DBCliRecvPort := DBCliRecvPort;
          ConnectInfo.DBCliSendPort := DBCliSendPort;
        end;
    end;
end;

procedure TDB_DoubleTunnelClient.Disconnect;
begin
  UnRegisterCommand;
  NetSendTunnelIntf.Disconnect;
  NetRecvTunnelIntf.Disconnect;
end;

procedure TDB_DoubleTunnelClient.AntiIdle(workLoad: Word);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteWORD(workLoad);
  SendTunnel.SendDirectStreamCmd('AntiIdle', sendDE);
  DisposeObject(sendDE);
end;

function TDB_DoubleTunnelClient.RegLoginServer(const RegAddr: string; const RegRecvPort, RegSendPort: Word): Boolean;
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

  DoStatus('send reg Login to DBServer cmd [addr:%s][r:%d][s:%d][w:%d]', [RegAddr, RegRecvPort, RegSendPort, 0]);

  SendTunnel.WaitSendStreamCmd('RegLoginServer', SendData, ResultData, 5000);

  if ResultData.Count = 2 then
    begin
      Result := ResultData.Reader.ReadBool;
      DoStatus(ResultData.Reader.ReadString);
    end;

  DisposeObject(SendData);
  DisposeObject(ResultData);
end;

procedure TDB_DoubleTunnelClient.UserIsLock(userID: string; OnResult: TStreamMethod);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(userID);

  SendTunnel.SendStreamCmd('UserIsLock', sendDE, OnResult);

  DisposeObject(sendDE);
end;

procedure TDB_DoubleTunnelClient.UserLock(userID: string);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(userID);

  SendTunnel.SendDirectStreamCmd('UserLock', sendDE);

  DisposeObject(sendDE);
end;

procedure TDB_DoubleTunnelClient.UserUnLock(userID: string);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(userID);

  SendTunnel.SendDirectStreamCmd('UserUnLock', sendDE);

  DisposeObject(sendDE);
end;

procedure TDB_DoubleTunnelClient.GetReplayInfo(userID: string; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(userID);

  SendTunnel.SendStreamCmd('GetReplayInfo', sendDE, Param1, Param2, OnResult);

  DisposeObject(sendDE);
end;

procedure TDBFileInfo.Init;
begin
  Owner := nil;
  client := nil;
  completed := False;
  Filename := '';
  FileLastTime := 0;
end;

procedure TDBFileInfo.GetFileTime_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  completed := True;
  if ResultData.Reader.ReadBool then
    begin
      Filename := ResultData.Reader.ReadString;
      FileLastTime := ResultData.Reader.ReadDouble;
    end;
  if (Owner.AllCompleted) then
    with Owner.Owner.PostProgress.PostExecute(1, Owner.Owner.PostExecuteProc_DBFileInfoListComplete) do
        Data1 := self.Owner;
end;

procedure TDBFileInfoList.SortOfFileLastTime;

  function SortComp(const v1, v2: PDBFileInfo): TValueRelationship; inline;
  begin
    Result := CompareDateTime(v1^.FileLastTime, v2^.FileLastTime);
  end;

type
  TArrayDBFileInfo = array of PDBFileInfo;
  PArrayDBFileInfo = ^TArrayDBFileInfo;

  procedure QuickSortList(var SortList: TArrayDBFileInfo; l, r: Integer); inline;
  var
    i, j: Integer;
    p, t: PDBFileInfo;
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
  pbuff: PArrayDBFileInfo;
begin
  if Count > 1 then
    begin
      pbuff := ListData;
      QuickSortList(pbuff^, 0, Count - 1);
    end;
end;

function TDBFileInfoList.AllCompleted: Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 0 to Count - 1 do
    if (Items[i]^.client.Connected) and (Items[i]^.client.LinkOk) then
        Result := Result and Items[i]^.completed;
end;

procedure TDBUserState.Init;
begin
  Owner := nil;
  client := nil;
  completed := False;
  userID := '';
  userState := False;
end;

procedure TDBUserState.State_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  completed := True;
  self.userState := ResultData.Reader.ReadBool;

  if (Owner.AllCompleted) then
    with Owner.Owner.PostProgress.PostExecute(1, Owner.Owner.PostExecuteProc_DBUserStateListComplete) do
        Data1 := self.Owner;
end;

function TDBUserStateList.AllCompleted: Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 0 to Count - 1 do
    if (Items[i]^.client.Connected) and (Items[i]^.client.LinkOk) then
        Result := Result and Items[i]^.completed;
end;

function TDB_DoubleTunnelClients.GetItems(index: Integer): TDB_DoubleTunnelClient;
begin
  Result := FClientList[index] as TDB_DoubleTunnelClient;
end;

procedure TDB_DoubleTunnelClients.SetItems(index: Integer; const Value: TDB_DoubleTunnelClient);
begin
  FClientList[index] := Value;
end;

procedure TDB_DoubleTunnelClients.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
var
  i           : Integer;
  FileInfoLst : TDBFileInfoList;
  UserStateLst: TDBUserStateList;
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
              with PostProgress.PostExecute(1, PostExecuteProc_DBFileInfoListComplete) do
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
              with PostProgress.PostExecute(1, PostExecuteProc_DBUserStateListComplete) do
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

procedure TDB_DoubleTunnelClients.PostExecuteProc_DBFileInfoListComplete(Sender: TNPostExecute);
var
  l: TDBFileInfoList;
  p: PDBFileInfo;
begin
  l := TDBFileInfoList(Sender.Data1);

  l.SortOfFileLastTime;

  if Assigned(l.OnComplete) then
      l.OnComplete(l);

  for p in l do
      dispose(p);

  DisposeObject(l);
end;

procedure TDB_DoubleTunnelClients.PostExecuteProc_DBUserStateListComplete(Sender: TNPostExecute);
var
  l: TDBUserStateList;
  p: PDBUserState;
begin
  l := TDBUserStateList(Sender.Data1);

  if Assigned(l.OnComplete) then
      l.OnComplete(l);

  for p in l do
      dispose(p);

  DisposeObject(l);
end;

constructor TDB_DoubleTunnelClients.Create(const ADBClientInterface: IDBClientInterface);
begin
  inherited Create;
  FClientList := TCoreClassListForObj.Create;
  AntiIdleIsRun := False;
  DBClientInterface := ADBClientInterface;
  CadencerEng := TCadencer.Create;
  CadencerEng.OnProgress := CadencerProgress;
  PostProgress := TNProgressPost.Create;
  DBCompleteFileInfoList := TDBCompleteFileInfoList.Create;
  DBCompleteUserStateList := TDBCompleteUserStateList.Create;
end;

destructor TDB_DoubleTunnelClients.Destroy;
begin
  Clear;
  DisposeObject(FClientList);
  DisposeObject(CadencerEng);
  DisposeObject(PostProgress);
  DisposeObject(DBCompleteFileInfoList);
  DisposeObject(DBCompleteUserStateList);
  inherited Destroy;
end;

function TDB_DoubleTunnelClients.Count: Integer;
begin
  Result := FClientList.Count;
end;

procedure TDB_DoubleTunnelClients.Clear;
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

procedure TDB_DoubleTunnelClients.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
  CadencerEng.Progress;
end;

procedure TDB_DoubleTunnelClients.AntiIdle(workLoad: Word);
var
  i       : Integer;
  conninfo: TDBCliConnectInfo;
  c       : TDB_DoubleTunnelClient;
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
                DoStatus('reconnect call reg Login to DBServer send cmd:%s [addr:%s][r:%d][s:%d][w:%d]',
                  [conninfo.DBServAddr, conninfo.RegAddr, conninfo.RegRecvPort, conninfo.RegSendPort, 0]);
                if c.RegLoginServer(conninfo.RegAddr, conninfo.RegRecvPort, conninfo.RegSendPort) then
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

function TDB_DoubleTunnelClients.BuildClientAndConnect(const DBServAddr, RegAddr: string; const DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort: Word): Boolean;
var
  c: TDB_DoubleTunnelClient;
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

  c := TDB_DoubleTunnelClient.Create(self);
  if c.Connect(DBServAddr, DBCliRecvPort, DBCliSendPort) then
    begin
      DoStatus('call reg Login to DB api send cmd [addr:%s][r:%d][s:%d][w:%d]', [RegAddr, RegRecvPort, RegSendPort, 0]);
      Result := c.RegLoginServer(RegAddr, RegRecvPort, RegSendPort);
      if Result then
        begin
          FClientList.Add(c);
        end;
    end
  else
      DisposeObject(c);
end;

function TDB_DoubleTunnelClients.QueryDBFile(RemoteFileName: string; completeProc: TDBFileInfoComplete): TDBFileInfoList;
var
  i: Integer;
  c: TDB_DoubleTunnelClient;
  l: TDBFileInfoList;
  p: PDBFileInfo;
begin
  l := TDBFileInfoList.Create;
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
          c.GetFileTime(RemoteFileName, p^.GetFileTime_StreamResult);
        end;
    end;
  DBCompleteFileInfoList.Add(l);
  Result := l;
end;

function TDB_DoubleTunnelClients.QueryUserIsLock(userID: string; completeProc: TDBUserStateComplete): TDBUserStateList;
var
  i: Integer;
  c: TDB_DoubleTunnelClient;
  l: TDBUserStateList;
  p: PDBUserState;
begin
  l := TDBUserStateList.Create;
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
