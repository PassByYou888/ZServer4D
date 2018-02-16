{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ ****************************************************************************** }
unit CommunicationTest;

interface

{$I zDefine.inc}


uses SysUtils, CommunicationFramework, DataFrameEngine,
  UnicodeMixedLib, CoreClasses, DoStatusIO, MemoryStream64, PascalStrings,
  CommunicationFrameworkIO, CoreCipher;

type
  TCommunicationTestIntf = class(TCoreClassObject)
  private
    FPrepareSendConsole, FPrepareResultConsole    : SystemString;
    FPrepareSendDataFrame, FPrepareResultDataFrame: TDataFrameEngine;
    FLastReg                                      : TCommunicationFramework;
  public
    constructor Create;
    destructor Destroy; override;

    // client test command
    procedure Cmd_TestStream(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure Cmd_TestConsole(Sender: TPeerIO; InData: SystemString; var OutData: SystemString);
    procedure Cmd_TestDirectStream(Sender: TPeerIO; InData: TDataFrameEngine);
    procedure Cmd_TestDirectConsole(Sender: TPeerIO; InData: SystemString);
    procedure Cmd_TestBigStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
    procedure Cmd_BigStreamPostInfo(Sender: TPeerIO; InData: SystemString);
    procedure Cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure Cmd_RemoteInfo(Sender: TPeerIO; InData: SystemString);

    // server test command result
    procedure CmdResult_TestConsole(Sender: TPeerIO; ResultData: SystemString);
    procedure CmdResult_TestStream(Sender: TPeerIO; ResultData: TDataFrameEngine);

    procedure RegCmd(intf: TCommunicationFramework);
    procedure ExecuteTest(intf: TPeerIO);
    procedure ExecuteAsyncTest(intf: TPeerIO);
    procedure ExecuteAsyncTestWithBigStream(intf: TPeerIO);

    property LastReg: TCommunicationFramework read FLastReg;
  end;

implementation

var
  TestStreamData: TMemoryStream64 = nil;
  TestStreamMD5 : SystemString;
  TestBuff      : PByte;
  TestBuffSize  : NativeInt;
  TestBuffMD5   : SystemString;

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

  FLastReg := nil;
end;

destructor TCommunicationTestIntf.Destroy;
begin
  DisposeObject(FPrepareSendDataFrame);
  DisposeObject(FPrepareResultDataFrame);
  inherited;
end;

procedure TCommunicationTestIntf.Cmd_TestStream(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  if not InData.Compare(FPrepareSendDataFrame) then
      Sender.Print('TestStream in Data failed!');
  OutData.Assign(FPrepareResultDataFrame);
end;

procedure TCommunicationTestIntf.Cmd_TestConsole(Sender: TPeerIO; InData: SystemString; var OutData: SystemString);
begin
  if InData <> FPrepareSendConsole then
      Sender.Print('TestConsole in Data failed!');
  OutData := FPrepareResultConsole;
end;

procedure TCommunicationTestIntf.Cmd_TestDirectStream(Sender: TPeerIO; InData: TDataFrameEngine);
begin
  if not InData.Compare(FPrepareSendDataFrame) then
      Sender.Print('TestDirectStream in Data failed!');
end;

procedure TCommunicationTestIntf.Cmd_TestDirectConsole(Sender: TPeerIO; InData: SystemString);
begin
  if InData <> FPrepareSendConsole then
      Sender.Print('TestDirectConsole in Data failed!');
end;

procedure TCommunicationTestIntf.Cmd_TestBigStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  if Sender.UserDefine.BigStreamBatchList.Count = 0 then
      Sender.UserDefine.BigStreamBatchList.NewPostData;

  Sender.UserDefine.BigStreamBatchList.Last^.Source.CopyFrom(InData, InData.Size);
end;

procedure TCommunicationTestIntf.Cmd_BigStreamPostInfo(Sender: TPeerIO; InData: SystemString);
begin
  if InData <> umlStreamMD5String(Sender.UserDefine.BigStreamBatchList.Last^.Source).Text then
      Sender.Print('TestBigStream failed!');
  Sender.UserDefine.BigStreamBatchList.Clear;
end;

procedure TCommunicationTestIntf.Cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
begin
  if umlMD5Char(InData, DataSize).Text <> TestBuffMD5 then
      Sender.Print('TestCompleteBuffer failed!');
end;

procedure TCommunicationTestIntf.Cmd_RemoteInfo(Sender: TPeerIO; InData: SystemString);
begin
  Sender.Print('remote:' + InData);
end;

procedure TCommunicationTestIntf.CmdResult_TestConsole(Sender: TPeerIO; ResultData: SystemString);
begin
  if ResultData <> FPrepareResultConsole then
      Sender.Print('TestResultConsole Data failed!');
end;

procedure TCommunicationTestIntf.CmdResult_TestStream(Sender: TPeerIO; ResultData: TDataFrameEngine);
begin
  if not ResultData.Compare(FPrepareResultDataFrame) then
      Sender.Print('TestResultStream Data failed!');
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
  intf.RegisterCompleteBuffer('TestCompleteBuffer').OnExecute := @Cmd_TestCompleteBuffer;
  intf.RegisterDirectConsole('RemoteInfo').OnExecute := @Cmd_RemoteInfo;
  {$ELSE}
  intf.RegisterStream('TestStream').OnExecute := Cmd_TestStream;
  intf.RegisterConsole('TestConsole').OnExecute := Cmd_TestConsole;
  intf.RegisterDirectStream('TestDirectStream').OnExecute := Cmd_TestDirectStream;
  intf.RegisterDirectConsole('TestDirectConsole').OnExecute := Cmd_TestDirectConsole;
  intf.RegisterBigStream('TestBigStream').OnExecute := Cmd_TestBigStream;
  intf.RegisterDirectConsole('BigStreamPostInfo').OnExecute := Cmd_BigStreamPostInfo;
  intf.RegisterCompleteBuffer('TestCompleteBuffer').OnExecute := Cmd_TestCompleteBuffer;
  intf.RegisterDirectConsole('RemoteInfo').OnExecute := Cmd_RemoteInfo;
  {$ENDIF}
  FLastReg := intf;
end;

procedure TCommunicationTestIntf.ExecuteTest(intf: TPeerIO);
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
  intf.SendDirectConsoleCmd('BigStreamPostInfo', umlStreamMD5String(TestStreamData).Text);
  intf.SendCompleteBuffer('TestCompleteBuffer', TestBuff, TestBuffSize, False);

  if intf.OwnerFramework is TCommunicationFrameworkClient then
    begin
      if intf.WaitSendConsoleCmd('TestConsole', FPrepareSendConsole, 0) <> FPrepareResultConsole then
          intf.Print('wait Mode:TestResultConsole Data failed!');

      tmpdf := TDataFrameEngine.Create;
      intf.WaitSendStreamCmd('TestStream', FPrepareSendDataFrame, tmpdf, 0);
      if not tmpdf.Compare(FPrepareResultDataFrame) then
          intf.Print('wait Mode:TestResultStream Data failed!');
      DisposeObject(tmpdf);
    end;

  intf.SendDirectConsoleCmd('RemoteInfo', 'client id[' + IntToStr(intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteAsyncTest(intf: TPeerIO);
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

procedure TCommunicationTestIntf.ExecuteAsyncTestWithBigStream(intf: TPeerIO);
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
  intf.SendCompleteBuffer('TestCompleteBuffer', TestBuff, TestBuffSize, False);

  intf.SendDirectConsoleCmd('RemoteInfo', 'client id[' + IntToStr(intf.ID) + '] test over!');
end;

procedure MakeRndBuff(v: Integer; p: Pointer; siz: NativeInt);
var
  seed: Integer;
  i   : Integer;
begin
  seed := v;
  for i := 0 to (siz div 4) - 1 do
      PInteger(Pointer(NativeUInt(p) + (i * 4)))^ := TMISC.Ran03(seed);
end;

initialization

TestStreamData := TMemoryStream64.Create;
TestStreamData.SetSize(Int64(16 + 1024 * 1024));
MakeRndBuff(99999933, TestStreamData.Memory, TestStreamData.Size);
TestStreamMD5 := umlStreamMD5String(TestStreamData).Text;

TestBuffSize := 512 * 1024 + 64;
TestBuff := AllocMem(TestBuffSize);
MakeRndBuff(777777, TestBuff, TestBuffSize);
TestBuffMD5 := umlMD5String(TestBuff, TestBuffSize).Text;

finalization

DisposeObject(TestStreamData);
TestStreamData := nil;
FreeMem(TestBuff, TestBuffSize);

end.
