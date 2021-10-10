{ *External API support, baidu translation service HTTP support* }
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
unit BaiduTranslateAPI;

interface

uses Classes, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryStream64,
  ZS_JsonDataObjects;

var
  { Use the following address to apply for Baidu translation }
  // http://api.fanyi.baidu.com

  { Application for API use qualification is free and will be completed after application }
  { On the front page of the developer console, you will see the appid and key, and paste them below }

  { Baidu translation interface can be used for client and FMX }
  { However, the direct use of translation on the client is easy to cause key loss, and the user's translation history and accelerated translation cannot be recorded, and the translation volume is easy to exceed the standard, resulting in uncontrollable withholding }

  { Baidu translation key }
  BaiduTranslate_Appid: string = '2015063000000001';
  BaiduTranslate_Key: string = '12345678';

  { The largest safe thread interface for Baidu translation }
  { Each thread of the system will occupy 1-4m (8m under Linux x64) stack space, and the background resources will be arranged reasonably }
  { For the technical summary of thread stack, please refer to the following introduction }
  // http://blog.csdn.net/cherish_2012/article/details/45073399
  BaiduTranslate_MaxSafeThread: Integer = 100; { 100 is a luxury Linux virtual machine }

type
  TTranslateLanguage = (
    tL_auto, { automatic }
    tL_zh,   { chinese }
    tL_en,   { English }
    tL_yue,  { Cantonese }
    tL_wyw,  { classical Chinese }
    tL_jp,   { Japanese }
    tL_kor,  { Korean }
    tL_fra,  { French }
    tL_spa,  { Spanish }
    tL_th,   { Thai }
    tL_ara,  { Arabic }
    tL_ru,   { Russian }
    tL_pt,   { Portuguese }
    tL_de,   { German }
    tL_it,   { Italian }
    tL_el,   { Greek }
    tL_nl,   { Dutch }
    tL_pl,   { Polish }
    tL_bul,  { Bulgarian }
    tL_est,  { Estonian }
    tL_dan,  { Danish }
    tL_fin,  { Finnish }
    tL_cs,   { Czech }
    tL_rom,  { romanian }
    tL_slo,  { Slovenian }
    tL_swe,  { Swedish }
    tL_hu,   { Hungarian }
    tL_cht,  { Traditional Chinese }
    tL_vie); { Vietnamese }

  TTranslateCompleteProc = reference to procedure(UserData: Pointer; Success: Boolean; sour, dest: TPascalString);

procedure BaiduTranslateWithHTTP(UsedSSL: Boolean;
  sourLanguage, desLanguage: TTranslateLanguage;
  Text: TPascalString;
  UserData: Pointer;
  OnResult: TTranslateCompleteProc);

threadvar BaiduTranslateTh: Int64;

implementation

uses SysUtils, IDHttp;

var
  LastAPI_TimeTick: TTimeTick;

type
  THTTPGetTh = class;

  THTTPSyncIntf = class
    th: THTTPGetTh;
    url: string;
    HTTP: TIdCustomHTTP;
    m64: TMemoryStream64;
    UserData: Pointer;
    RepleatGet: Integer;
    OnResult: TTranslateCompleteProc;

    procedure AsyncGet;
  end;

  THTTPGetTh = class(TThread)
    syncIntf: THTTPSyncIntf;
    procedure Execute; override;
  end;

procedure THTTPSyncIntf.AsyncGet;
var
  js: TJsonObject; { For high-frequency translation, we need to use jsondataobjects. Its defect is that it does not support FPC, but it is stable and fast }
  ja: TJsonArray;
  i: Integer;
  Success: Boolean;
  sour, Dst: TPascalString;
begin
  { Since Baidu translation limits the access frequency, we need to wait 1.1 seconds before triggering HTTP access }
  while GetTimeTick() - LastAPI_TimeTick < 1100 do
      Sleep(10);
  LastAPI_TimeTick := GetTimeTick();

  try
    HTTP.ReadTimeout := 2000;
    HTTP.Get(url, m64);
  except
    Inc(RepleatGet);

    { Due to the network quality, HTTPS is often disconnected and abnormal. We repeat it for up to 5 times until it succeeds }
    if RepleatGet < 5 then
      begin
        AsyncGet;
      end
    else
      begin
        th.Synchronize(
          procedure
          begin
            OnResult(UserData, False, '', '');
          end);
      end;
    Exit;
  end;
  m64.Position := 0;
  js := TJsonObject.Create;
  try
      js.LoadFromStream(m64, TEncoding.UTF8, True);
  except
    DisposeObject([js]);
    OnResult(UserData, False, '', '');
    Exit;
  end;

  Success := False;
  sour := '';
  Dst := '';

  try
    { Safety check the translation results }
    if js.IndexOf('trans_result') >= 0 then
      begin
        ja := js.A['trans_result'];
        if ja.Count > 0 then
          begin
            for i := 0 to ja.Count - 1 do
              begin
                Success := True;
                if i = 0 then
                  begin
                    sour := ja[i].s['src'];
                    Dst := ja[i].s['dst'];
                  end
                else
                  begin
                    sour.Append(#13#10 + ja[i].s['src']);
                    Dst.Append(#13#10 + ja[i].s['dst']);
                  end;
              end;
          end;
      end;
  except
    DisposeObject([js]);
    th.Synchronize(
      procedure
      begin
        OnResult(UserData, False, '', '');
      end);
    Exit;
  end;

  th.Synchronize(
    procedure
    begin
      OnResult(UserData, Success, sour, Dst);
    end);

  DisposeObject([js]);

  DisposeObject([m64]);
end;

procedure THTTPGetTh.Execute;
begin
  Inc(BaiduTranslateTh);
  FreeOnTerminate := True;
  syncIntf.HTTP := TIdCustomHTTP.Create(nil);
  try
      syncIntf.AsyncGet;
  finally
    { Safe recycling }
    try
        DisposeObject(syncIntf.HTTP);
    except
    end;
    DisposeObject(syncIntf);
    Dec(BaiduTranslateTh);
  end;
end;

function TranslateLanguage2Token(T: TTranslateLanguage): TPascalString; inline;
begin
  { I have commented on each translation tag }
  case T of
    tL_auto: Result := 'auto'; { Auto will determine the translation language according to the HTTP request code, and others will not }
    tL_zh: Result := 'zh';     { chinese }
    tL_en: Result := 'en';     { English }
    tL_yue: Result := 'yue';   { Cantonese }
    tL_wyw: Result := 'wyw';   { classical Chinese }
    tL_jp: Result := 'jp';     { Japanese }
    tL_kor: Result := 'kor';   { Korean }
    tL_fra: Result := 'fra';   { French }
    tL_spa: Result := 'spa';   { Spanish }
    tL_th: Result := 'th';     { Thai }
    tL_ara: Result := 'ara';   { Arabic }
    tL_ru: Result := 'ru';     { Russian }
    tL_pt: Result := 'pt';     { Portuguese }
    tL_de: Result := 'de';     { German }
    tL_it: Result := 'it';     { Italian }
    tL_el: Result := 'el';     { Greek }
    tL_nl: Result := 'nl';     { Dutch }
    tL_pl: Result := 'pl';     { Polish }
    tL_bul: Result := 'bul';   { Bulgarian }
    tL_est: Result := 'est';   { Estonian }
    tL_dan: Result := 'dan';   { Danish }
    tL_fin: Result := 'fin';   { Finnish }
    tL_cs: Result := 'cs';     { Czech }
    tL_rom: Result := 'rom';   { romanian }
    tL_slo: Result := 'slo';   { Slovenian }
    tL_swe: Result := 'swe';   { Swedish }
    tL_hu: Result := 'hu';     { Hungarian }
    tL_cht: Result := 'cht';   { Traditional Chinese }
    tL_vie: Result := 'vie';   { Vietnamese }
    else
      Result := 'auto'; { Auto will determine the translation language according to the HTTP request code, and others will not }
  end;
end;

procedure BaiduTranslateWithHTTP(UsedSSL: Boolean; sourLanguage, desLanguage: TTranslateLanguage; Text: TPascalString; UserData: Pointer; OnResult: TTranslateCompleteProc);
var
  salt: Integer;
  httpurl: TPascalString;
  soursign: TPascalString;
  lasturl: TPascalString;
  Intf: THTTPSyncIntf;
  th: THTTPGetTh;
begin
  if Text.Len > 2000 then
    begin
      OnResult(UserData, False, '', '');
      Exit;
    end;
  salt := umlRandomRange(32767, 1024 * 1024 * 2);
  soursign := BaiduTranslate_Appid + Text + IntToStr(salt) + BaiduTranslate_Key;

  if UsedSSL then
      httpurl := 'https://api.fanyi.baidu.com/api/trans/vip/translate'
  else
      httpurl := 'http://api.fanyi.baidu.com/api/trans/vip/translate';

  lasturl.Text := httpurl + '?' +
    'q=' + umlURLEncode(Text) +
    '&from=' + TranslateLanguage2Token(sourLanguage) +
    '&to=' + TranslateLanguage2Token(desLanguage) +
    '&appid=' + BaiduTranslate_Appid +
    '&salt=' + IntToStr(salt) +
    '&sign=' + umlStringMD5(soursign);
  Intf := THTTPSyncIntf.Create;
  Intf.th := THTTPGetTh.Create;
  Intf.th.syncIntf := Intf;
  Intf.url := lasturl;
  Intf.HTTP := nil;
  Intf.m64 := TMemoryStream64.Create;
  Intf.UserData := UserData;
  Intf.RepleatGet := 0;
  Intf.OnResult := OnResult;
  Intf.th.Suspended := False;
end;

initialization

LastAPI_TimeTick := GetTimeTick();

end.
