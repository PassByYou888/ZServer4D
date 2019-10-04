{ ****************************************************************************** }
{ * Developer refrence Support                                                 * }
{ * written by QQ 600585@qq.com                                                * }
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
  TServer_PeerIO = class(TPeerIO)
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    { core interface: return connection state }
    function Connected: Boolean; override;

    { core interface: disconnect imp. }
    procedure Disconnect; override;

    { core interface: kernel triggers when sending data. }
    procedure SendByteBuffer(const buff: PByte; const Size: nativeInt); override;
    { core interface: kernel will do WriteBufferOpen before sending data. }
    procedure WriteBufferOpen; override;
    { core interface: kernel will do WriteBufferFlush after sending data. }
    procedure WriteBufferFlush; override;
    { core interface: kernel will do WriteBufferClose after sending a batch of data. }
    procedure WriteBufferClose; override;

    { core interface: get the IP information. }
    function GetPeerIP: SystemString; override;

    { select: If your data is in memory and wait been sent, it returns to False. }
    { select: if you do not consider high concurrency optimization, you can ignore the interface. }
    function WriteBufferEmpty: Boolean; override;

    { select: Kernel main loop, you can do ignore the interface }
    procedure Progress; override;
  end;

  TCommunicationFramework_Server_Refrence = class(TCommunicationFrameworkServer)
  private
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    { core interface: The StartService can be Bluetooth, TCP, UDP, rs232 com, GPIO, anywhere Communication interace. }
    function StartService(Host: SystemString; Port: Word): Boolean; override;
    { core interface: StopService. }
    procedure StopService; override;

    { core interface: Kernel main loop, you can do ignore the interface }
    procedure Progress; override;

    { select: in the kernel post a queue command, it triggers. }
    procedure TriggerQueueData(v: PQueueData); override;

    { select: recommended no used blocking communication calls on the server, unstable!! }
    function WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; override;
    { select: recommended no used blocking communication calls on the server, unstable!! }
    procedure WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick); override;
  end;

implementation

procedure TServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
end;

destructor TServer_PeerIO.Destroy;
begin
  inherited Destroy;
end;

function TServer_PeerIO.Connected: Boolean;
begin
  Result := True;
end;

procedure TServer_PeerIO.Disconnect;
begin
end;

procedure TServer_PeerIO.SendByteBuffer(const buff: PByte; const Size: nativeInt);
begin
  if not Connected then
      Exit;
end;

procedure TServer_PeerIO.WriteBufferOpen;
begin
end;

procedure TServer_PeerIO.WriteBufferFlush;
begin
end;

procedure TServer_PeerIO.WriteBufferClose;
begin
end;

function TServer_PeerIO.GetPeerIP: SystemString;
begin
  Result := '';
end;

function TServer_PeerIO.WriteBufferEmpty: Boolean;
begin
  Result := True;
end;

procedure TServer_PeerIO.Progress;
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
  Result := False;
end;

procedure TCommunicationFramework_Server_Refrence.StopService;
begin
end;

procedure TCommunicationFramework_Server_Refrence.Progress;
begin
  inherited Progress;
end;

procedure TCommunicationFramework_Server_Refrence.TriggerQueueData(v: PQueueData);
var
  c: TPeerIO;
begin
  c := PeerIO[v^.IO_ID];
  if c <> nil then
    begin
      c.PostQueueData(v);
      c.ProcessAllSendCmd(nil, False, False);
    end
  else
      DisposeQueueData(v);
end;

function TCommunicationFramework_Server_Refrence.WaitSendConsoleCmd(p_io: TPeerIO;
  const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TCommunicationFramework_Server_Refrence.WaitSendStreamCmd(p_io: TPeerIO;
  const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport');
end;

initialization

finalization

end.
