{ *External API support, baidu translation service* }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ ****************************************************************************** }

program BaiduTranslateService;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  PascalStrings,
  CommunicationFramework,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket,
  DoStatusIO,
  CoreClasses,
  DataFrameEngine,
  UnicodeMixedLib,
  MemoryStream64,
  ZJson,
  ZDBLocalManager,
  ZDBEngine,
  ListEngine,
  BaiduTranslateAPI in 'BaiduTranslateAPI.pas',
  BaiduTranslateClient in 'Client.Lib\BaiduTranslateClient.pas';

{ Baidu translation server is written in Delphi xe10.1.2
  If you want to use it under Linux, please replace Delphi xe10.2.2 or above. If we can't find Linux in the platform drop-down item, create a new console project and copy the code
  The HTTP query of Baidu translation is done in the thread
  If a client sends 1000 query requests at the same time, 1000 threads will not occur, but after one query is completed, the next one will be queried
  This server has a security mechanism, which is limited to 100ip simultaneous query
  Note: this server model uses a database and does not have hot standby components such as datastoreservice (mainly because I don't want to make a small translation service too large)
  When you use Ctrl + F2 to shut down the server, it is equivalent to power failure. ZDB has a safe write back mechanism. The safe way is to shut down all clients first, and then Ctrl + F2 after 2 seconds
  If the database is damaged, it cannot be recovered. You can only directly delete history.ox and restart the server }

var
  MiniDB: TZDBLocalManager;

const
  C_Mini_DB_Name = 'zTranslate';

type
  TMyServer = class(TCommunicationFramework_Server_CrossSocket)
  public
    procedure DoIOConnectAfter(Sender: TPeerIO); override;
    procedure DoIODisconnect(Sender: TPeerIO); override;
  end;

procedure TMyServer.DoIOConnectAfter(Sender: TPeerIO);
begin
  DoStatus('id: %d ip:%s connected', [Sender.ID, Sender.PeerIP]);
  Sender.UserVariants['LastIP'] := Sender.PeerIP;
  inherited DoIOConnectAfter(Sender);
end;

procedure TMyServer.DoIODisconnect(Sender: TPeerIO);
begin
  DoStatus('id: %d ip: %s disconnect', [Sender.ID, VarToStr(Sender.UserVariants['LastIP'])]);
  inherited DoIODisconnect(Sender);
end;

procedure cmd_BaiduTranslate(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
type
  PDelayReponseSource = ^TDelayReponseSource;

  TDelayReponseSource = packed record
    serv: TMyServer;
    ID: Cardinal;
    sourLan, destLan: TTranslateLanguage;
    s: TPascalString;
    UsedCache: Boolean;
    Hash64: THash64;
  end;

var
  sp: PDelayReponseSource;
begin
  { The baidu translate command implemented here is also an advanced server technology demonstration }
  { If some code reads like it's too big and involves too much, it's because you don't care about the database engine ZDB }
  { If you don't care about ZDB, just skip the ZDB link. Please refer to the use of Baidu translatewith http }

  { In consideration of server security }
  { If there are more than 100 online IP addresses and queries are issued at the same time, an error is returned }
  { This condition is triggered only when more than 100 people are online and 100 people are sending translation requests }
  if BaiduTranslateTh > BaiduTranslate_MaxSafeThread then
    begin
      OutData.WriteBool(False);
      Exit;
    end;

  { Turn on the delayed response mode, the technical system and function of ZS delay, please understand the relevant demo in the standard demonstration }
  Sender.PauseResultSend;

  { We create a callback data structure for delayed safe release without leakage }
  new(sp);
  sp^.serv := TMyServer(Sender.OwnerFramework);
  sp^.ID := Sender.ID;
  { Translation data from client }
  sp^.sourLan := TTranslateLanguage(InData.Reader.ReadByte); { Source language of translation }
  sp^.destLan := TTranslateLanguage(InData.Reader.ReadByte); { Target language of translation }
  sp^.s := InData.Reader.ReadString;                         { There is no string correction here. Change the string correction to the client }
  sp^.UsedCache := InData.Reader.ReadBool;                   { Whether to use cache database }
  sp^.Hash64 := FastHash64PPascalString(@sp^.s);             { High speed hash }

  { It is more efficient to query our translation from the cache database }
  MiniDB.QueryDBP(
    False,          { The query results are written to the return table }
    True,           { The returned table of the query is a memory table. If false, it is an entity file table }
    True,           { Query from last }
    C_Mini_DB_Name, { The name of the target database for the query }
    '',             { Return the name of the table, because we don't output it. It's empty here }
    True,           { Release the return table when the query is complete }
    0,              { The delay time, in seconds, to release the returned table when the query is completed }
    0.1,            { Fragment accumulation time: when there is a lot of feedback in the query, the feedback event will be triggered every time it is accumulated to facilitate batch operation. During the accumulation time, the data exists in memory }
    0,              { Query execution time, 0 is infinite }
    0,              { The maximum number of matching query entries, 0 is infinite }
    1,              { For the largest query result feedback, we only check one of our translation cache }
      procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    var
        p: PDelayReponseSource;
        J: TZ_JsonObject;
        cli: TPeerIO;
    begin
        { Query filter callback }
        p := dPipe.UserPointer;

        { If the client usedcache is false, we directly end the query and jump to the query completion event }
        if not p^.UsedCache then
        begin
            dPipe.stop;
            Exit;
        end;

        { This step is an important acceleration mechanism of ZDB. The instance of JSON is managed by the annealing engine. When the database is busy, JSON will not be released and is stored in memory as a cache }
        J := qState.dbEng.GetJson(qState);

        Allowed :=
        (p^.Hash64 = J.u['h']) { We use hash to improve the traversal speed }
        and (TTranslateLanguage(J.i['sl']) = p^.sourLan) and (TTranslateLanguage(J.i['dl']) = p^.destLan)
        and (p^.s.Same(TPascalString(J.s['s'])));

        if Allowed then
        begin
            cli := p^.serv.PeerIO[p^.ID];

            { In the delay technology system, the client may be disconnected after sending the request }
            { If the line is broken, CLI is nil }
            if cli <> nil then
            begin
                cli.OutDataFrame.WriteBool(True);       { Translation success status }
                cli.OutDataFrame.WriteString(J.s['d']); { Target language for translation completion }
                cli.OutDataFrame.WriteBool(True);       { Is the translation from the cache database }
                cli.ContinueResultSend;
            end;

            { After exiting here, the background query will end automatically, because we only need one feedback }
        end;
    end,
    procedure(dPipe: TZDBPipeline)
    var
      p: PDelayReponseSource;
    begin
      p := dPipe.UserPointer;
      { If a feedback is found, dpipe. Queryresultcounter will be 1. Now let's free the memory we just applied for }
      if dPipe.QueryResultCounter > 0 then
        begin
          Dispose(p);
          Exit;
        end;

      { If it is not found in the cache database, we call Baidu API and save the translation results to the cahce database }
      BaiduTranslateWithHTTP(False, p^.sourLan, p^.destLan, p^.s, p, procedure(UserData: Pointer; Success: Boolean; sour, dest: TPascalString)
        var
          cli: TPeerIO;
          n: TPascalString;
          js: TZ_JsonObject;
          p: PDelayReponseSource;
        begin
          p := UserData;
          cli := TPeerIO(PDelayReponseSource(UserData)^.serv.PeerIO[PDelayReponseSource(UserData)^.ID]);
          { In the delay technology system, the client may be disconnected after sending the request }
          { If the line is broken, CLI is nil }
          if cli <> nil then
            begin
              cli.OutDataFrame.WriteBool(Success);
              if Success then
                begin
                  cli.OutDataFrame.WriteString(dest);
                  cli.OutDataFrame.WriteBool(False); { Is the translation from the cache database }

                  { Only when the usedcache of the client is true can we write translation information to the database }
                  if p^.UsedCache then
                    begin
                      { Record query results to database }
                      { Because there are more than 2 million translations, you must pay Baidu }
                      js := TZ_JsonObject.Create;
                      js.i['sl'] := Integer(p^.sourLan);
                      js.i['dl'] := Integer(p^.destLan);
                      js.u['h'] := FastHash64PPascalString(@p^.s);
                      js.F['t'] := Now;
                      js.s['s'] := p^.s.Text;
                      js.s['d'] := dest.Text;
                      js.s['ip'] := cli.PeerIP;

                      MiniDB.PostData(C_Mini_DB_Name, js);

                      { Chinese cannot be displayed in Ubuntu server mode }
{$IFNDEF Linux}
                      DoStatus('new cache %s', [js.ToString]);
{$IFEND}
                      DisposeObject(js);
                    end;
                end;

              { Continue to respond }
              cli.ContinueResultSend;
            end;
          Dispose(p);
        end);
    end).UserPointer := sp;
end;

{ Update the cache database. The implementation mechanism here is to add a translation record to the end of the database indefinitely. The previous things will not be deleted here }
{ Baidu translation's query on the cache database starts from the end, so we add it is also equivalent to modifying it }
procedure cmd_UpdateTranslate(Sender: TPeerIO; InData: TDataFrameEngine);
var
  sourLan, destLan: TTranslateLanguage;
  s, d: TPascalString;
  Hash64: THash64;
  js: TZ_JsonObject;
begin
  sourLan := TTranslateLanguage(InData.Reader.ReadByte); { Source language of translation }
  destLan := TTranslateLanguage(InData.Reader.ReadByte); { Target language of translation }
  s := InData.Reader.ReadString;                         { Source text }
  d := InData.Reader.ReadString;                         { translate }
  Hash64 := FastHash64PPascalString(@s);                 { High speed hash }

  js := TZ_JsonObject.Create;
  js.i['sl'] := Integer(sourLan);
  js.i['dl'] := Integer(destLan);
  js.u['h'] := Hash64;
  js.F['t'] := Now;
  js.s['s'] := s.Text;
  js.s['d'] := d.Text;
  js.s['ip'] := Sender.PeerIP;
  MiniDB.PostData(C_Mini_DB_Name, js);

  { Chinese cannot be displayed in Ubuntu server mode }
{$IFNDEF Linux}
  DoStatus('update cache %s', [js.ToString]);
{$IFEND}
  DisposeObject(js);
end;

procedure Init_BaiduTranslateAccound;
var
  cfg: THashStringList;
begin
  { Use the following address to apply for Baidu translation }
  // http://api.fanyi.baidu.com

  cfg := THashStringList.Create;
  if umlFileExists(umlCombineFileName(umlCurrentPath(), 'zTranslate.conf')) then
    begin
      cfg.LoadFromFile(umlCombineFileName(umlCurrentPath(), 'zTranslate.conf'));
      BaiduTranslate_Appid := cfg.GetDefaultValue('AppID', BaiduTranslate_Appid);
      BaiduTranslate_Key := cfg.GetDefaultValue('Key', BaiduTranslate_Key);
    end
  else
    begin
      cfg.SetDefaultValue('AppID', BaiduTranslate_Appid);
      cfg.SetDefaultValue('Key', BaiduTranslate_Key);
      cfg.SaveToFile(umlCombineFileName(umlCurrentPath(), 'zTranslate.conf'));
    end;
  DisposeObject(cfg);
end;

var
  server_1, server_2: TMyServer;

begin
  { Initialize Baidu translation account }
  Init_BaiduTranslateAccound;

  MiniDB := TZDBLocalManager.Create;
  { Because the database in the form of file is created, the database is easy to be damaged due to the frequent strong fallback of Ctrl + F2 }
  MiniDB.InitDB(C_Mini_DB_Name);

  server_1 := TMyServer.Create;
  { Using the strongest encryption system, 3 secondary des repeated encryption combined with ECB }
  server_1.SwitchMaxSecurity;
  { If the Indy server is used under Ubuntu, the bound loopback address must be specified here }
  // if server_IPv4.StartService('0.0.0.0', 59813) then

  { The new version of crosssocket has fixed the problem that IPv4 + IPv6 listen on one port at the same time under Ubuntu }
  { We use null characters to listen for 59813 of IPv4 + IPv6 at the same time }
  if server_1.StartService('', 59813) then
      DoStatus('start service with ipv4:59813 success')
  else
      DoStatus('start service with ipv4:59813 failed!');

  { However, we can still start multiple services at the same time, listen to IPv6, IPv4 and different ports at the same time, and then point the instruction trigger point to the same place }
  { In this way, it can be used for any external server interface, diocp, cross, Indy, ICs and other server interfaces, which can realize multi listening centralized service in this way }

  { If there is an IPv6 listening error in Linux, either install the IPv6 services and modules yourself, or ignore it }
  server_2 := TMyServer.Create;
  { Using the strongest encryption system, 3 secondary des repeated encryption combined with ECB }
  server_2.SwitchMaxSecurity;
  if server_2.StartService('::', 59814) then
      DoStatus('start service with ipv6:59814 success')
  else
      DoStatus('start service with ipv6:59814 failed!');

  server_1.RegisterStream('BaiduTranslate').OnExecuteCall := cmd_BaiduTranslate;
  server_2.RegisterStream('BaiduTranslate').OnExecuteCall := cmd_BaiduTranslate;

  server_1.RegisterDirectStream('UpdateTranslate').OnExecuteCall := cmd_UpdateTranslate;
  server_2.RegisterDirectStream('UpdateTranslate').OnExecuteCall := cmd_UpdateTranslate;

  { Disconnect when idle for 15 seconds }
  server_1.IdleTimeout := 15000;
  server_2.IdleTimeout := 15000;

  server_1.QuietMode := True;
  server_2.QuietMode := True;

  while True do
    begin
      MiniDB.Progress;
      server_1.Progress;
      server_2.Progress;

      { Green and environmental protection to avoid unnecessary expenses }
      if server_1.Count + server_2.Count > 0 then
          CoreClasses.CheckThreadSynchronize(1)
      else
          CoreClasses.CheckThreadSynchronize(100);
    end;

end.
