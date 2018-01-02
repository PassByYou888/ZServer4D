unit CommunicationTest;

interface

{$I zDefine.inc}


uses SysUtils, CommunicationFramework, DataFrameEngine,
  UnicodeMixedLib, CoreClasses, DoStatusIO, MemoryStream64, PascalStrings,
  CommunicationFrameworkIO;

type
  TCommunicationTestIntf = class(TCoreClassObject)
  private
    FPrepareSendConsole, FPrepareResultConsole    : string;
    FPrepareSendDataFrame, FPrepareResultDataFrame: TDataFrameEngine;
    FTempStream                                   : TMemoryStream64;
    FLastReg                                      : TCommunicationFramework;
  public
    constructor Create;
    destructor Destroy; override;

    // client test command
    procedure Cmd_TestStream(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    procedure Cmd_TestConsole(Sender: TPeerClient; InData: string; var OutData: string);
    procedure Cmd_TestDirectStream(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure Cmd_TestDirectConsole(Sender: TPeerClient; InData: string);
    procedure Cmd_TestBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
    procedure Cmd_BigStreamPostInfo(Sender: TPeerClient; InData: string);
    procedure Cmd_RemoteInfo(Sender: TPeerClient; InData: string);

    // server test command result
    procedure CmdResult_TestConsole(Sender: TPeerClient; ResultData: string);
    procedure CmdResult_TestStream(Sender: TPeerClient; ResultData: TDataFrameEngine);

    procedure RegCmd(intf: TCommunicationFramework);
    procedure ExecuteTest(intf: TPeerClient);
    procedure ExecuteAsyncTest(intf: TPeerClient);
    procedure ExecuteAsyncTestWithBigStream(intf: TPeerClient);

    property LastReg: TCommunicationFramework read FLastReg;
  end;

implementation

var
  TestStreamData: TMemoryStream64 = nil;
  TestStreamMD5 : string;

constructor TCommunicationTestIntf.Create;
var
  i: Integer;
begin
  inherited;
  FPrepareSendConsole := 'console test';
  FPrepareResultConsole := 'console result';
  FPrepareSendDataFrame := TDataFrameEngine.Create;
  FPrepareResultDataFrame := TDataFrameEngine.Create;
  for i := 1 to 10 do
    begin
      FPrepareSendDataFrame.WriteInteger(i);
      FPrepareResultDataFrame.WriteInteger(i);
    end;

  FTempStream := TMemoryStream64.Create;
  FLastReg := nil;
end;

destructor TCommunicationTestIntf.Destroy;
begin
  DisposeObject(FPrepareSendDataFrame);
  DisposeObject(FPrepareResultDataFrame);
  DisposeObject(FTempStream);
  inherited;
end;

procedure TCommunicationTestIntf.Cmd_TestStream(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  if not InData.Compare(FPrepareSendDataFrame) then
      DoStatus('TestStream in Data failed!');
  OutData.Assign(FPrepareResultDataFrame);
end;

procedure TCommunicationTestIntf.Cmd_TestConsole(Sender: TPeerClient; InData: string; var OutData: string);
begin
  if InData <> FPrepareSendConsole then
      DoStatus('TestConsole in Data failed!');
  OutData := FPrepareResultConsole;
end;

procedure TCommunicationTestIntf.Cmd_TestDirectStream(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  if not InData.Compare(FPrepareSendDataFrame) then
      DoStatus('TestDirectStream in Data failed!');
end;

procedure TCommunicationTestIntf.Cmd_TestDirectConsole(Sender: TPeerClient; InData: string);
begin
  if InData <> FPrepareSendConsole then
      DoStatus('TestDirectConsole in Data failed!');
  FTempStream.Clear;
end;

procedure TCommunicationTestIntf.Cmd_TestBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  FTempStream.CopyFrom(InData, InData.Size);
end;

procedure TCommunicationTestIntf.Cmd_BigStreamPostInfo(Sender: TPeerClient; InData: string);
begin
  if InData <> umlStreamMD5Char(FTempStream).Text then
      DoStatus('TestBigStream failed!');
  FTempStream.Clear;
end;

procedure TCommunicationTestIntf.Cmd_RemoteInfo(Sender: TPeerClient; InData: string);
begin
  DoStatus('remote:' + InData);
end;

procedure TCommunicationTestIntf.CmdResult_TestConsole(Sender: TPeerClient; ResultData: string);
begin
  if ResultData <> FPrepareResultConsole then
      DoStatus('TestResultConsole Data failed!');
end;

procedure TCommunicationTestIntf.CmdResult_TestStream(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  if not ResultData.Compare(FPrepareResultDataFrame) then
      DoStatus('TestResultStream Data failed!');
end;

procedure TCommunicationTestIntf.RegCmd(intf: TCommunicationFramework);
begin
  {$IFDEF FPC}
  intf.RegisterStream('TestStream').OnExecute := @Cmd_TestStream;
  intf.RegisterConsole('TestConsole').OnExecute := @Cmd_TestConsole;
  intf.RegisterDirectStream('TestDirectStream').OnExecute := @Cmd_TestDirectStream;
  intf.RegisterDirectConsole('TestDirectConsole').OnExecute := @Cmd_TestDirectConsole;
  intf.RegisterBigStream('TestBigStream').OnExecute := @Cmd_TestBigStream;
  intf.RegisterDirectConsole('BigStreamPostInfo').OnExecute := @Cmd_BigStreamPostInfo;
  intf.RegisterDirectConsole('RemoteInfo').OnExecute := @Cmd_RemoteInfo;
  {$ELSE}
  intf.RegisterStream('TestStream').OnExecute := Cmd_TestStream;
  intf.RegisterConsole('TestConsole').OnExecute := Cmd_TestConsole;
  intf.RegisterDirectStream('TestDirectStream').OnExecute := Cmd_TestDirectStream;
  intf.RegisterDirectConsole('TestDirectConsole').OnExecute := Cmd_TestDirectConsole;
  intf.RegisterBigStream('TestBigStream').OnExecute := Cmd_TestBigStream;
  intf.RegisterDirectConsole('BigStreamPostInfo').OnExecute := Cmd_BigStreamPostInfo;
  intf.RegisterDirectConsole('RemoteInfo').OnExecute := Cmd_RemoteInfo;
  {$ENDIF}
  FLastReg := intf;
end;

procedure TCommunicationTestIntf.ExecuteTest(intf: TPeerClient);
var
  tmpdf: TDataFrameEngine;
begin
  {$IFDEF FPC}
  intf.SendConsoleCmd('TestConsole', FPrepareSendConsole, @CmdResult_TestConsole);
  intf.SendStreamCmd('TestStream', FPrepareSendDataFrame, @CmdResult_TestStream);
  {$ELSE}
  intf.SendConsoleCmd('TestConsole', FPrepareSendConsole, CmdResult_TestConsole);
  intf.SendStreamCmd('TestStream', FPrepareSendDataFrame, CmdResult_TestStream);
  {$ENDIF}
  intf.SendDirectConsoleCmd('TestDirectConsole', FPrepareSendConsole);
  intf.SendDirectStreamCmd('TestDirectStream', FPrepareSendDataFrame);
  intf.SendBigStream('TestBigStream', TestStreamData, False);
  intf.SendDirectConsoleCmd('BigStreamPostInfo', umlStreamMD5Char(TestStreamData).Text);

  if intf.OwnerFramework is TCommunicationFrameworkClient then
    begin
      if intf.WaitSendConsoleCmd('TestConsole', FPrepareSendConsole, 0) <> FPrepareResultConsole then
          DoStatus('wait Mode:TestResultConsole Data failed!');

      tmpdf := TDataFrameEngine.Create;
      intf.WaitSendStreamCmd('TestStream', FPrepareSendDataFrame, tmpdf, 0);
      if not tmpdf.Compare(FPrepareResultDataFrame) then
          DoStatus('wait Mode:TestResultStream Data failed!');
      DisposeObject(tmpdf);
    end;

  intf.SendDirectConsoleCmd('RemoteInfo', 'client id[' + IntToStr(intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteAsyncTest(intf: TPeerClient);
var
  tmpdf: TDataFrameEngine;
begin
  {$IFDEF FPC}
  intf.SendConsoleCmd('TestConsole', FPrepareSendConsole, @CmdResult_TestConsole);
  intf.SendStreamCmd('TestStream', FPrepareSendDataFrame, @CmdResult_TestStream);
  {$ELSE}
  intf.SendConsoleCmd('TestConsole', FPrepareSendConsole, CmdResult_TestConsole);
  intf.SendStreamCmd('TestStream', FPrepareSendDataFrame, CmdResult_TestStream);
  {$ENDIF}
  intf.SendDirectConsoleCmd('TestDirectConsole', FPrepareSendConsole);
  intf.SendDirectStreamCmd('TestDirectStream', FPrepareSendDataFrame);

  intf.SendDirectConsoleCmd('RemoteInfo', 'client id[' + IntToStr(intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteAsyncTestWithBigStream(intf: TPeerClient);
var
  tmpdf: TDataFrameEngine;
begin
  {$IFDEF FPC}
  intf.SendConsoleCmd('TestConsole', FPrepareSendConsole, @CmdResult_TestConsole);
  intf.SendStreamCmd('TestStream', FPrepareSendDataFrame, @CmdResult_TestStream);
  {$ELSE}
  intf.SendConsoleCmd('TestConsole', FPrepareSendConsole, CmdResult_TestConsole);
  intf.SendStreamCmd('TestStream', FPrepareSendDataFrame, CmdResult_TestStream);
  {$ENDIF}
  intf.SendDirectConsoleCmd('TestDirectConsole', FPrepareSendConsole);
  intf.SendDirectStreamCmd('TestDirectStream', FPrepareSendDataFrame);

  intf.SendBigStream('TestBigStream', TestStreamData, False);
  intf.SendDirectConsoleCmd('BigStreamPostInfo', TestStreamMD5);

  intf.SendDirectConsoleCmd('RemoteInfo', 'client id[' + IntToStr(intf.ID) + '] test over!');
end;

initialization

TestStreamData := TMemoryStream64.Create;
TestStreamData.SetSize(Int64(1024 * 1024 * 10));
FillPtrByte(TestStreamData.Memory, TestStreamData.Size, $99);
TestStreamMD5 := umlStreamMD5String(TestStreamData).Text;

finalization

DisposeObject(TestStreamData);
TestStreamData := nil;

end.
