{ ****************************************************************************** }
{ * Developer refrence Support                                                 * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
*)

unit CommunicationFramework_Server_Refrence;

{$I ..\zDefine.inc}

interface

uses SysUtils, Classes,
  PascalStrings,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64, DataFrameEngine;

type
  TPeerIOWithRefrenceServer = class(TPeerIO)
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

  TCommunicationFramework_Server_Refrence = class(TCommunicationFrameworkServer)
  private
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    function StartService(Host: SystemString; Port: Word): Boolean; override;
    procedure StopService; override;

    procedure TriggerQueueData(v: PQueueData); override;
    procedure ProgressBackground; override;

    function WaitSendConsoleCmd(Client: TPeerIO;
      const Cmd, ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; override;
    procedure WaitSendStreamCmd(Client: TPeerIO;
      const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); override;
  end;

implementation

procedure TPeerIOWithRefrenceServer.CreateAfter;
begin
  inherited CreateAfter;
end;

destructor TPeerIOWithRefrenceServer.Destroy;
begin
  inherited Destroy;
end;

function TPeerIOWithRefrenceServer.Connected: Boolean;
begin
end;

procedure TPeerIOWithRefrenceServer.Disconnect;
begin
end;

procedure TPeerIOWithRefrenceServer.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if not Connected then
      exit;
end;

procedure TPeerIOWithRefrenceServer.WriteBufferOpen;
begin
end;

procedure TPeerIOWithRefrenceServer.WriteBufferFlush;
begin
end;

procedure TPeerIOWithRefrenceServer.WriteBufferClose;
begin
end;

function TPeerIOWithRefrenceServer.GetPeerIP: SystemString;
begin
end;

function TPeerIOWithRefrenceServer.WriteBufferEmpty: Boolean;
begin
end;

constructor TCommunicationFramework_Server_Refrence.Create;
begin
  inherited Create;
end;

destructor TCommunicationFramework_Server_Refrence.Destroy;
begin
  StopService;
  inherited Destroy;
end;

function TCommunicationFramework_Server_Refrence.StartService(Host: SystemString; Port: Word): Boolean;
begin
end;

procedure TCommunicationFramework_Server_Refrence.StopService;
begin
end;

procedure TCommunicationFramework_Server_Refrence.TriggerQueueData(v: PQueueData);
begin
  if not Exists(v^.Client) then
    begin
      DisposeQueueData(v);
      exit;
    end;

  v^.Client.PostQueueData(v);
  v^.Client.ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Server_Refrence.ProgressBackground;
begin
  inherited ProgressBackground;
end;

function TCommunicationFramework_Server_Refrence.WaitSendConsoleCmd(Client: TPeerIO;
  const Cmd, ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TCommunicationFramework_Server_Refrence.WaitSendStreamCmd(Client: TPeerIO;
  const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
begin
  RaiseInfo('WaitSend no Suppport');
end;

initialization

finalization

end.
