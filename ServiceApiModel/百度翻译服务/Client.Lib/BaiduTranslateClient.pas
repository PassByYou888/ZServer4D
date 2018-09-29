{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit BaiduTranslateClient;

interface

{ Baidu translation client does not support FPC }

uses Classes, CoreClasses,
  PascalStrings, UnicodeMixedLib, DoStatusIO, DataFrameEngine, NotifyObjectBase,
  CommunicationFramework;

var
  { Host address support IPv6 }
  BaiduTranslateServiceHost: string = '127.0.0.1';
  BaiduTranslateServicePort: Word   = 59813;

type
  TBaiduL = (
    L_auto, { automatic }
    L_zh,   { Chinese }
    L_en,   { English }
    L_yue,  { Cantonese }
    L_wyw,  { Classical Chinese }
    L_jp,   { Japanese }
    L_kor,  { Korean }
    L_fra,  { French }
    L_spa,  { Spanish }
    L_th,   { Thai }
    L_ara,  { Arabic }
    L_ru,   { Russian }
    L_pt,   { Portuguese }
    L_de,   { German }
    L_it,   { Italian }
    L_el,   { Greek language }
    L_nl,   { Dutch }
    L_pl,   { Polish }
    L_bul,  { Bulgarian }
    L_est,  { Estonia language }
    L_dan,  { Danish }
    L_fin,  { Finnish }
    L_cs,   { Czech }
    L_rom,  { Romanian }
    L_slo,  { Slovenia language }
    L_swe,  { Swedish }
    L_hu,   { Hungarian }
    L_cht,  { Traditional Chinese }
    L_vie); { Vietnamese }

  TBaiduTranslate_CompleteProc = reference to procedure(UserData: Pointer; Success, Cached: Boolean; TranslateTime: TTimeTick; sour, dest: TPascalString);

  { Translation API }
procedure BaiduTranslate(RealTime, UsedCache: Boolean; sourLanguage, desLanguage: TBaiduL; text: TPascalString;
  UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc); overload;
procedure BaiduTranslate(RealTime, UsedCache: Boolean; sourLanguage, desLanguage: Byte; text: TPascalString;
  UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc); overload;
procedure BaiduTranslate(sourLanguage, desLanguage: Byte; text: TPascalString;
  UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc); overload;

{ Update the Cache of the translation server }
procedure UpdateTranslate(sourLanguage, desLanguage: TBaiduL; SourText, DestText: TPascalString); overload;
procedure UpdateTranslate(sourLanguage, desLanguage: Byte; SourText, DestText: TPascalString); overload;

procedure OpenBaiduTranslate;
procedure CloseBaiduTranslate;

implementation


uses CommunicationFramework_Client_Indy;

type
  TBaiduTranslateClientBase = TCommunicationFramework_Client_Indy;

  TBaiduTranslateClient = class(TBaiduTranslateClientBase)
  private type
    PUserDef = ^TUserDef;

    TUserDef = record
      sourLanguage, desLanguage: TBaiduL;
      text: TPascalString;
      UsedCache: Boolean;
      UserData: Pointer;
      OnResult: TBaiduTranslate_CompleteProc;
      LastTime: TTimeTick;
    end;
  private
    FTranslateList   : TCoreClassList;
    FTranslateBusy   : Boolean;
    FCurrentTranslate: PUserDef;
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;

    procedure ClearTranslateList;
    procedure doBaiduTranslate(p: PUserDef);
    procedure BaiduTranslate_Result(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure BaiduTranslate(RealTime, UsedCache: Boolean; sourLanguage, desLanguage: TBaiduL; text: TPascalString;
      UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);

    procedure UpdateTranslate(sourLanguage, desLanguage: TBaiduL; SourText, DestText: TPascalString);
  end;

procedure TBaiduTranslateClient.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TBaiduTranslateClient.DoDisconnect(Sender: TPeerIO);
begin
  inherited DoDisconnect(Sender);
end;

constructor TBaiduTranslateClient.Create;
begin
  inherited Create;
  QuietMode := True;
  FTranslateList := TCoreClassList.Create;
  FTranslateBusy := False;
  FCurrentTranslate := nil;
end;

destructor TBaiduTranslateClient.Destroy;
begin
  ClearTranslateList;
  DisposeObject(FTranslateList);
  inherited Destroy;
end;

procedure TBaiduTranslateClient.ClearTranslateList;
var
  i: Integer;
begin
  for i := 0 to FTranslateList.Count - 1 do
      dispose(PUserDef(FTranslateList[i]));
  FTranslateList.Clear;
end;

procedure TBaiduTranslateClient.doBaiduTranslate(p: PUserDef);
var
  de: TDataFrameEngine;
begin
  FCurrentTranslate := p;

  p^.LastTime := GetTimeTick;

  de := TDataFrameEngine.Create;
  de.WriteByte(Byte(p^.sourLanguage));
  de.WriteByte(Byte(p^.desLanguage));
  de.WriteString(umlTrimSpace(p^.text));
  de.WriteBool(p^.UsedCache);
  SendStreamCmdM('BaiduTranslate', de, p, nil, BaiduTranslate_Result);
  DisposeObject(de);
end;

procedure TBaiduTranslateClient.BaiduTranslate_Result(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p2    : PUserDef;
  n     : TPascalString;
  Cached: Boolean;
begin
  p2 := Param1;
  if ResultData.Reader.ReadBool then
    begin
      n := ResultData.Reader.ReadString;
      Cached := ResultData.Reader.ReadBool;
      p2^.OnResult(p2^.UserData, True, Cached, GetTimeTick - p2^.LastTime, p2^.text, n);

      DoStatus('Original:%s', [p2^.text.text]);
      DoStatus('Translation:%s', [n.text]);
      DoStatus('Time:%dms', [GetTimeTick - p2^.LastTime]);
      DoStatus('Translation success!');
      if Cached then
          DoStatus('Translation data from ZServer')
      else
          DoStatus('Translation data from Baidu');
      DoStatus('');
    end
  else
    begin
      n := 'Translation error!';
      p2^.OnResult(p2^.UserData, False, False, GetTimeTick - p2^.LastTime, p2^.text, n);

      DoStatus('Original:%s', [p2.text.text]);
      DoStatus('Time:%dms', [GetTimeTick - p2^.LastTime]);
      DoStatus('Translation failed!');
      DoStatus('');
    end;
  dispose(p2);

  if FTranslateList.Count > 0 then
    begin
      p2 := FTranslateList[0];
      FTranslateList.Delete(0);
      doBaiduTranslate(p2);
      exit;
    end
  else
      FTranslateBusy := False;
end;

procedure TBaiduTranslateClient.BaiduTranslate(RealTime, UsedCache: Boolean; sourLanguage, desLanguage: TBaiduL; text: TPascalString;
  UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
var
  p : PUserDef;
  de: TDataFrameEngine;
begin
  new(p);
  p^.sourLanguage := sourLanguage;
  p^.desLanguage := desLanguage;
  p^.text := text;
  p^.UsedCache := UsedCache;
  p^.UserData := UserData;
  p^.OnResult := OnResult;
  p^.LastTime := GetTimeTick;

  if Connected and RealTime then
    begin
      doBaiduTranslate(p);
      exit;
    end;

  if (not Connected) or (FTranslateBusy) then
    begin
      FTranslateList.Add(p);
      exit;
    end;

  FTranslateBusy := True;
  doBaiduTranslate(p);
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

  DisposeObject(de);
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
var
  p2: TBaiduTranslateClient.PUserDef;
  n : TPascalString;
begin
  BaiduTranslate_Client.Progress;

  if BaiduTranslate_Client.Connected then
    begin
      if BaiduTranslate_Client.ClientIO <> nil then
        if BaiduTranslate_Client.ClientIO.StopCommunicationTime > 5000 then
          begin
            BaiduTranslate_Client.WaitP(2000, procedure(const cState: Boolean)
              begin
              end)
          end;
    end
  else
    begin
      if (BaiduTranslate_Client.FTranslateBusy) and (BaiduTranslate_Client.FCurrentTranslate <> nil) then
        begin
          p2 := BaiduTranslate_Client.FCurrentTranslate;
          n := 'Translation error!';
          p2^.OnResult(p2^.UserData, False, False, GetTimeTick - p2^.LastTime, p2^.text, n);

          DoStatus('Original:%s', [p2.text.text]);
          DoStatus('Time:%dms', [GetTimeTick - p2^.LastTime]);
          DoStatus('Translation failed!');
          DoStatus('');

          dispose(p2);
          BaiduTranslate_Client.FCurrentTranslate := nil;

          if BaiduTranslate_Client.Connect(BaiduTranslateServiceHost, BaiduTranslateServicePort) then
            begin
              if BaiduTranslate_Client.FTranslateList.Count > 0 then
                begin
                  p2 := BaiduTranslate_Client.FTranslateList[0];
                  BaiduTranslate_Client.FTranslateList.Delete(0);
                  BaiduTranslate_Client.doBaiduTranslate(p2);
                  exit;
                end;
            end
          else
            begin
              BaiduTranslate_Client.FTranslateBusy := False;
              BaiduTranslate_Client.ClearTranslateList;
            end;
        end;
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

procedure BaiduTranslate(RealTime, UsedCache: Boolean; sourLanguage, desLanguage: TBaiduL; text: TPascalString; UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
begin
  if not BaiduTranslate_Client.Connected then
    if not BaiduTranslate_Client.Connect(BaiduTranslateServiceHost, BaiduTranslateServicePort) then
      begin
        OnResult(UserData, False, False, 0, text, 'Translation error!');
        exit;
      end;
  BaiduTranslate_Client.BaiduTranslate(RealTime, UsedCache, sourLanguage, desLanguage, text, UserData, OnResult);
end;

procedure BaiduTranslate(RealTime, UsedCache: Boolean; sourLanguage, desLanguage: Byte; text: TPascalString; UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
begin
  BaiduTranslate(RealTime, UsedCache, TBaiduL(sourLanguage), TBaiduL(desLanguage), text, UserData, OnResult);
end;

procedure BaiduTranslate(sourLanguage, desLanguage: Byte; text: TPascalString; UserData: Pointer; OnResult: TBaiduTranslate_CompleteProc);
begin
  BaiduTranslate(True, True, TBaiduL(sourLanguage), TBaiduL(desLanguage), text, UserData, OnResult);
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
    if BaiduTranslate_Client.Connect(BaiduTranslateServiceHost, BaiduTranslateServicePort) then
        DoStatus('connect Translate Success,server %s', [BaiduTranslateServiceHost])
    else
        DoStatus('connect  Translate Failed,server %s', [BaiduTranslateServiceHost]);
end;

procedure CloseBaiduTranslate;
begin
  BaiduTranslate_Client.FTranslateBusy := False;
  BaiduTranslate_Client.FCurrentTranslate := nil;
  BaiduTranslate_Client.ClearTranslateList;
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
DisposeObject(BaiduTranslate_Client);

end.
