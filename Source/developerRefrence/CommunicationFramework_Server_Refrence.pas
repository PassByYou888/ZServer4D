{ ****************************************************************************** }
{ * Developer refrence Support                                                 * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
(*
  update history
*)

unit CommunicationFramework_Server_Refrence;

{$INCLUDE ..\zDefine.inc}

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
    procedure SendByteBuffer(const buff: PByte; const Size: nativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    function WriteBufferEmpty: Boolean; override;
    procedure Progress; override;
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
    procedure Progress; override;

    function WaitSendConsoleCmd(Client: TPeerIO;
      const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString; override;
    procedure WaitSendStreamCmd(Client: TPeerIO;
      const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue); override;
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

procedure TPeerIOWithRefrenceServer.SendByteBuffer(const buff: PByte; const Size: nativeInt);
begin
  if not Connected then
      Exit;
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

procedure TPeerIOWithRefrenceServer.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
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
var
  c: TPeerIO;
begin
  c := ClientFromID[v^.ClientID];
  if c <> nil then
    begin
      c.PostQueueData(v);
      c.ProcessAllSendCmd(nil, False, False);
    end
  else
      DisposeQueueData(v);
end;

procedure TCommunicationFramework_Server_Refrence.Progress;
begin
  inherited Progress;
end;

function TCommunicationFramework_Server_Refrence.WaitSendConsoleCmd(Client: TPeerIO;
  const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TCommunicationFramework_Server_Refrence.WaitSendStreamCmd(Client: TPeerIO;
  const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
begin
  RaiseInfo('WaitSend no Suppport');
end;

initialization

finalization

end.
