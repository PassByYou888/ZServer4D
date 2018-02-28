{ ****************************************************************************** }
{ * Developer refrence Support                                                 * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ ****************************************************************************** }
(*
  update history
*)

unit CommunicationFramework_Client_Refrence;

{$I ..\zDefine.inc}

interface

uses SysUtils, Classes,
  PascalStrings,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64,
  NotifyObjectBase;

type
  TPeerIOWithRefrenceClient = class(TPeerIO)
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    function WriteBufferEmpty: Boolean; override;
  end;

  TCommunicationFramework_Client_Refrence = class(TCommunicationFrameworkClient)
  private
    FOnAsyncConnectNotifyCall  : TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    FOnAsyncConnectNotifyProc  : TStateProc;
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;
    procedure TriggerQueueData(v: PQueueData); override;
    procedure ProgressBackground; override;

    procedure AsyncConnect(Addr: SystemString; Port: Word); overload;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall); overload; override;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod); overload; override;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc); overload; override;

    function Connect(Addr: SystemString; Port: Word): Boolean; overload; override;
    function Connect(host: SystemString; Port: SystemString): Boolean; overload;
    procedure Disconnect; override;
  end;

implementation


procedure TPeerIOWithRefrenceClient.CreateAfter;
begin
  inherited CreateAfter;
end;

destructor TPeerIOWithRefrenceClient.Destroy;
begin
  inherited Destroy;
end;

function TPeerIOWithRefrenceClient.Connected: Boolean;
begin
end;

procedure TPeerIOWithRefrenceClient.Disconnect;
begin
end;

procedure TPeerIOWithRefrenceClient.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if not Connected then
      exit;
end;

procedure TPeerIOWithRefrenceClient.WriteBufferOpen;
begin
end;

procedure TPeerIOWithRefrenceClient.WriteBufferFlush;
begin
end;

procedure TPeerIOWithRefrenceClient.WriteBufferClose;
begin
end;

function TPeerIOWithRefrenceClient.GetPeerIP: SystemString;
begin
end;

function TPeerIOWithRefrenceClient.WriteBufferEmpty: Boolean;
begin
end;

procedure TCommunicationFramework_Client_Refrence.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TCommunicationFramework_Client_Refrence.DoDisconnect(Sender: TPeerIO);
begin
  inherited DoDisconnect(Sender);
end;

constructor TCommunicationFramework_Client_Refrence.Create;
begin
  inherited Create;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

destructor TCommunicationFramework_Client_Refrence.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TCommunicationFramework_Client_Refrence.TriggerDoConnectFailed;
begin
  inherited TriggerDoConnectFailed;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(False);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(False);
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(False);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

procedure TCommunicationFramework_Client_Refrence.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(True);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(True);
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(True);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

function TCommunicationFramework_Client_Refrence.Connected: Boolean;
begin
  Result := (ClientIO <> nil) and (ClientIO.Connected);
end;

function TCommunicationFramework_Client_Refrence.ClientIO: TPeerIO;
begin
end;

procedure TCommunicationFramework_Client_Refrence.TriggerQueueData(v: PQueueData);
begin
  if not Connected then
    begin
      DisposeQueueData(v);
      exit;
    end;

  ClientIO.PostQueueData(v);
  ClientIO.ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Client_Refrence.ProgressBackground;
begin
  inherited ProgressBackground;
end;

procedure TCommunicationFramework_Client_Refrence.AsyncConnect(Addr: SystemString; Port: Word);
begin
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

procedure TCommunicationFramework_Client_Refrence.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall);
begin
end;

procedure TCommunicationFramework_Client_Refrence.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod);
begin
end;

procedure TCommunicationFramework_Client_Refrence.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc);
begin
end;

function TCommunicationFramework_Client_Refrence.Connect(Addr: SystemString; Port: Word): Boolean;
begin
end;

function TCommunicationFramework_Client_Refrence.Connect(host: SystemString; Port: SystemString): Boolean;
begin
  Result := Connect(host, umlStrToInt(Port, 0));
end;

procedure TCommunicationFramework_Client_Refrence.Disconnect;
begin
  if Connected then
      ClientIO.Disconnect;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

initialization

finalization

end.
