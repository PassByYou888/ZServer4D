unit BaiduTranslateClient;

interface

{ 百度翻译客户端支持手机和fmx，不支持fpc }

uses Classes, CoreClasses,
  PascalStrings, UnicodeMixedLib, DoStatusIO, DataFrameEngine, NotifyObjectBase,
  CommunicationFramework;

var
  // 主机地址支持ipv6
  BaiduTranslateServiceHost: string = '127.0.0.1';
  BaiduTranslateServicePort: Word   = 59813;

type
  TBaiduL = (
    L_auto, // 自动
    L_zh,   // 中文
    L_en,   // 英语
    L_yue,  // 粤语
    L_wyw,  // 文言文
    L_jp,   // 日语
    L_kor,  // 韩语
    L_fra,  // 法语
    L_spa,  // 西班牙语
    L_th,   // 泰语
    L_ara,  // 阿拉伯语
    L_ru,   // 俄语
    L_pt,   // 葡萄牙语
    L_de,   // 德语
    L_it,   // 意大利语
    L_el,   // 希腊语
    L_nl,   // 荷兰语
    L_pl,   // 波兰语
    L_bul,  // 保加利亚语
    L_est,  // 爱沙尼亚语
    L_dan,  // 丹麦语
    L_fin,  // 芬兰语
    L_cs,   // 捷克语
    L_rom,  // 罗马尼亚语
    L_slo,  // 斯洛文尼亚语
    L_swe,  // 瑞典语
    L_hu,   // 匈牙利语
    L_cht,  // 繁体中文
    L_vie); // 越南语

  TBaiduTranslate_CompleteProc = reference to procedure(UserData: Pointer; Success, Cached: Boolean; TranslateTime: TTimeTick; sour, dest: TPascalString);

  // 翻译api
procedure BaiduTranslate(UsedCache: Boolean; sourLanguage, desLanguage: TBaiduL; text: TPascalString;
  UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc); overload;
procedure BaiduTranslate(UsedCache: Boolean; sourLanguage, desLanguage: Byte; text: TPascalString;
  UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc); overload;
procedure BaiduTranslate(sourLanguage, desLanguage: Byte; text: TPascalString;
  UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc); overload;

// 更新翻译服务器的Cache
procedure UpdateTranslate(sourLanguage, desLanguage: TBaiduL; SourText, DestText: TPascalString); overload;
procedure UpdateTranslate(sourLanguage, desLanguage: Byte; SourText, DestText: TPascalString); overload;

procedure OpenBaiduTranslate;
procedure CloseBaiduTranslate;

implementation

{$IF Defined(IOS) or Defined(ANDROID) or Defined(OSX)}


uses CommunicationFramework_Client_Indy;

type
  TBaiduTranslateClientBase = TCommunicationFramework_Client_Indy;
{$ELSE}


uses CommunicationFramework_Client_CrossSocket;

type
  TBaiduTranslateClientBase = TCommunicationFramework_Client_CrossSocket;
  {$IFEND}

  TBaiduTranslateClient = class(TBaiduTranslateClientBase)
  public
    constructor Create; override;
    // 翻译服务器api
    procedure BaiduTranslate(UsedCache: Boolean; sourLanguage, desLanguage: TBaiduL; text: TPascalString;
      UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc); overload;
    procedure BaiduTranslate(UsedCache: Boolean; sourLanguage, desLanguage: Byte; text: TPascalString;
      UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc); overload;
    procedure BaiduTranslate(sourLanguage, desLanguage: Byte; text: TPascalString;
      UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc); overload;

    // 更新翻译服务器的Cache
    procedure UpdateTranslate(sourLanguage, desLanguage: TBaiduL; SourText, DestText: TPascalString); overload;
    procedure UpdateTranslate(sourLanguage, desLanguage: Byte; SourText, DestText: TPascalString); overload;
  end;

constructor TBaiduTranslateClient.Create;
begin
  inherited Create;
  QuietMode := True;
  // 使用最强加密系统，3次级DES反复加密结合ECB
  SwitchMaxSafe;
end;

procedure TBaiduTranslateClient.BaiduTranslate(UsedCache: Boolean; sourLanguage, desLanguage: TBaiduL; text: TPascalString;
  UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
type
  PUserDef = ^TUserDef;

  TUserDef = record
    sourLanguage, desLanguage: TBaiduL;
    text: TPascalString;
    UserData: Pointer;
    OnResult: TBaiduTranslate_CompleteProc;
    LastTime: TTimeTick;
  end;
var
  p : PUserDef;
  de: TDataFrameEngine;
begin
  if not Connected then
    begin
      OnResult(UserData, False, False, 0, text, '!翻译错误!');
      exit;
    end;

  new(p);
  p^.sourLanguage := sourLanguage;
  p^.desLanguage := desLanguage;
  p^.text := text;
  p^.UserData := UserData;
  p^.OnResult := OnResult;
  p^.LastTime := GetTimeTick;

  de := TDataFrameEngine.Create;
  de.WriteByte(Byte(sourLanguage));
  de.WriteByte(Byte(desLanguage));
  de.WriteString(umlTrimSpace(text));
  de.WriteBool(UsedCache);

  SendStreamCmd('BaiduTranslate', de, p, nil,
    procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine)
    var
      p2: PUserDef;
      n: TPascalString;
      Cached: Boolean;
    begin
      p2 := Param1;
      if ResultData.Reader.ReadBool then
        begin
          n := ResultData.Reader.ReadString;
          Cached := ResultData.Reader.ReadBool;
          p2^.OnResult(p2^.UserData, True, Cached, GetTimeTick - p2^.LastTime, p2^.text, n);
        end
      else
        begin
          n := '!翻译错误!';
          p2^.OnResult(p2^.UserData, False, False, GetTimeTick - p2^.LastTime, p2^.text, n);
        end;
      dispose(p2);
    end);
end;

procedure TBaiduTranslateClient.BaiduTranslate(UsedCache: Boolean; sourLanguage, desLanguage: Byte; text: TPascalString;
UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
begin
  BaiduTranslate(UsedCache, TBaiduL(sourLanguage), TBaiduL(desLanguage), text, UserData, OnResult);
end;

procedure TBaiduTranslateClient.BaiduTranslate(sourLanguage, desLanguage: Byte; text: TPascalString;
UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
begin
  BaiduTranslate(True, TBaiduL(sourLanguage), TBaiduL(desLanguage), text, UserData, OnResult);
end;

procedure TBaiduTranslateClient.UpdateTranslate(sourLanguage, desLanguage: TBaiduL; SourText, DestText: TPascalString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteByte(Byte(sourLanguage));
  de.WriteByte(Byte(desLanguage));
  de.WriteString(umlTrimSpace(SourText));
  de.WriteString(umlTrimSpace(DestText));

  SendDirectStreamCmd('UpdateTranslate', de);

  disposeObject(de);
end;

procedure TBaiduTranslateClient.UpdateTranslate(sourLanguage, desLanguage: Byte; SourText, DestText: TPascalString);
begin
  UpdateTranslate(TBaiduL(sourLanguage), TBaiduL(desLanguage), SourText, DestText);
end;

type
  TTranslateClient_Th = class(TCoreClassThread)
  protected
    procedure Execute; override;
    procedure SyncCheck;
  end;

var
  tCliThProcessing     : Boolean;
  Client_Th            : TTranslateClient_Th;
  BaiduTranslate_Client: TBaiduTranslateClient;

procedure TTranslateClient_Th.SyncCheck;
begin
  BaiduTranslate_Client.ProgressBackground;

  if BaiduTranslate_Client.Connected then
    if BaiduTranslate_Client.ClientIO <> nil then
      if BaiduTranslate_Client.ClientIO.StopCommunicationTime > 5000 then
        begin
          BaiduTranslate_Client.Wait(2000, procedure(const cState: Boolean)
            begin
            end)
        end;
end;

procedure TTranslateClient_Th.Execute;
begin
  FreeOnTerminate := True;
  while tCliThProcessing do
    begin
      Sleep(10);
      try
          Synchronize(SyncCheck);
      except
      end;
    end;
  Client_Th := nil;
end;

procedure BaiduTranslate(UsedCache: Boolean; sourLanguage, desLanguage: TBaiduL; text: TPascalString; UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
begin
  if not BaiduTranslate_Client.Connected then
    if not BaiduTranslate_Client.Connect(BaiduTranslateServiceHost, BaiduTranslateServicePort) then
      begin
        OnResult(UserData, False, False, 0, text, '!翻译错误!');
        exit;
      end;
  BaiduTranslate_Client.BaiduTranslate(UsedCache, sourLanguage, desLanguage, text, UserData, OnResult);
end;

procedure BaiduTranslate(UsedCache: Boolean; sourLanguage, desLanguage: Byte; text: TPascalString; UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
begin
  BaiduTranslate(UsedCache, TBaiduL(sourLanguage), TBaiduL(desLanguage), text, UserData, OnResult);
end;

procedure BaiduTranslate(sourLanguage, desLanguage: Byte; text: TPascalString; UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
begin
  BaiduTranslate(True, TBaiduL(sourLanguage), TBaiduL(desLanguage), text, UserData, OnResult);
end;

procedure UpdateTranslate(sourLanguage, desLanguage: TBaiduL; SourText, DestText: TPascalString);
begin
  if not BaiduTranslate_Client.Connected then
    if not BaiduTranslate_Client.Connect(BaiduTranslateServiceHost, BaiduTranslateServicePort) then
        exit;
  BaiduTranslate_Client.UpdateTranslate(sourLanguage, desLanguage, SourText, DestText);
end;

procedure UpdateTranslate(sourLanguage, desLanguage: Byte; SourText, DestText: TPascalString);
begin
  UpdateTranslate(TBaiduL(sourLanguage), TBaiduL(desLanguage), SourText, DestText);
end;

procedure OpenBaiduTranslate;
begin
  if not BaiduTranslate_Client.Connected then
      BaiduTranslate_Client.AsyncConnect(BaiduTranslateServiceHost, BaiduTranslateServicePort,
      procedure(const cState: Boolean)
      begin
        if cState then
            DoStatus('OpenBaidu Translate Success,server: %s', [BaiduTranslateServiceHost])
        else
            DoStatus('OpenBaidu Translate Failed,server: %s', [BaiduTranslateServiceHost]);
      end);
end;

procedure CloseBaiduTranslate;
begin
  BaiduTranslate_Client.Disconnect;
end;

initialization

BaiduTranslate_Client := TBaiduTranslateClient.Create;
tCliThProcessing := True;
Client_Th := TTranslateClient_Th.Create(False);

finalization

tCliThProcessing := False;
while Client_Th <> nil do
    Classes.CheckSynchronize(1);
disposeObject(BaiduTranslate_Client);

end.
