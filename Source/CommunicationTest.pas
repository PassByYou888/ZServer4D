{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit CommunicationTest;

{$INCLUDE ..\..\zDefine.inc}

interface

uses SysUtils, CommunicationFramework, DataFrameEngine,
  UnicodeMixedLib, CoreClasses, DoStatusIO, MemoryStream64, PascalStrings,
  CommunicationFrameworkIO, CoreCipher;

type
  TCommunicationTestIntf = class(TCoreClassObject)
  private
    FPrepareSendConsole, FPrepareResultConsole: SystemString;
    FPrepareSendDataFrame, FPrepareResultDataFrame: TDataFrameEngine;
    FLastReg: TCommunicationFramework;
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

    procedure RegCmd(Intf: TCommunicationFramework);
    procedure ExecuteTest(Intf: TPeerIO);
    procedure ExecuteAsyncTest(Intf: TPeerIO);
    procedure ExecuteAsyncTestWithBigStream(Intf: TPeerIO);

    property LastReg: TCommunicationFramework read FLastReg;
  end;

implementation

var
  TestStreamData: TMemoryStream64 = nil;
  TestStreamMD5: SystemString;
  TestBuff: PByte;
  TestBuffSize: NativeInt;
  TestBuffMD5: SystemString;

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

procedure TCommunicationTestIntf.RegCmd(Intf: TCommunicationFramework);
begin
  Intf.RegisterStream('TestStream').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestStream;
  Intf.RegisterConsole('TestConsole').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestConsole;
  Intf.RegisterDirectStream('TestDirectStream').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestDirectStream;
  Intf.RegisterDirectConsole('TestDirectConsole').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestDirectConsole;
  Intf.RegisterBigStream('TestBigStream').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestBigStream;
  Intf.RegisterDirectConsole('BigStreamPostInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_BigStreamPostInfo;
  Intf.RegisterCompleteBuffer('TestCompleteBuffer').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestCompleteBuffer;
  Intf.RegisterDirectConsole('RemoteInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_RemoteInfo;

  FLastReg := Intf;
end;

procedure TCommunicationTestIntf.ExecuteTest(Intf: TPeerIO);
var
  tmpdf: TDataFrameEngine;
begin
  Intf.SendConsoleCmdM('TestConsole', FPrepareSendConsole, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestConsole);
  Intf.SendStreamCmdM('TestStream', FPrepareSendDataFrame, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestStream);
  Intf.SendDirectConsoleCmd('TestDirectConsole', FPrepareSendConsole);
  Intf.SendDirectStreamCmd('TestDirectStream', FPrepareSendDataFrame);
  Intf.SendBigStream('TestBigStream', TestStreamData, False);
  Intf.SendDirectConsoleCmd('BigStreamPostInfo', umlStreamMD5String(TestStreamData).Text);
  Intf.SendCompleteBuffer('TestCompleteBuffer', TestBuff, TestBuffSize, False);

  if Intf.OwnerFramework is TCommunicationFrameworkClient then
    begin
      if Intf.WaitSendConsoleCmd('TestConsole', FPrepareSendConsole, 0) <> FPrepareResultConsole then
          Intf.Print('wait Mode:TestResultConsole Data failed!');

      tmpdf := TDataFrameEngine.Create;
      Intf.WaitSendStreamCmd('TestStream', FPrepareSendDataFrame, tmpdf, 0);
      if not tmpdf.Compare(FPrepareResultDataFrame) then
          Intf.Print('wait Mode:TestResultStream Data failed!');
      DisposeObject(tmpdf);
    end;

  Intf.SendDirectConsoleCmd('RemoteInfo', 'client id[' + IntToStr(Intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteAsyncTest(Intf: TPeerIO);
begin
  Intf.SendConsoleCmdM('TestConsole', FPrepareSendConsole, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestConsole);
  Intf.SendStreamCmdM('TestStream', FPrepareSendDataFrame, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestStream);
  Intf.SendDirectConsoleCmd('TestDirectConsole', FPrepareSendConsole);
  Intf.SendDirectStreamCmd('TestDirectStream', FPrepareSendDataFrame);

  Intf.SendDirectConsoleCmd('RemoteInfo', 'client id[' + IntToStr(Intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteAsyncTestWithBigStream(Intf: TPeerIO);
begin
  Intf.SendConsoleCmdM('TestConsole', FPrepareSendConsole, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestConsole);
  Intf.SendStreamCmdM('TestStream', FPrepareSendDataFrame, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestStream);
  Intf.SendDirectConsoleCmd('TestDirectConsole', FPrepareSendConsole);
  Intf.SendDirectStreamCmd('TestDirectStream', FPrepareSendDataFrame);

  Intf.SendBigStream('TestBigStream', TestStreamData, False);
  Intf.SendDirectConsoleCmd('BigStreamPostInfo', TestStreamMD5);
  Intf.SendCompleteBuffer('TestCompleteBuffer', TestBuff, TestBuffSize, False);

  Intf.SendDirectConsoleCmd('RemoteInfo', 'client id[' + IntToStr(Intf.ID) + '] test over!');
end;

procedure MakeRndBuff(v: Integer; p: Pointer; siz: NativeInt);
var
  Seed: Integer;
  i: Integer;
begin
  Seed := v;
  for i := 0 to (siz div 4) - 1 do
      PInteger(Pointer(nativeUInt(p) + (i * 4)))^ := TMISC.Ran03(Seed);
end;

initialization

TestStreamData := TMemoryStream64.Create;
TestStreamData.SetSize(Int64(16 + 1024 * 1024));
MakeRndBuff(99999933, TestStreamData.Memory, TestStreamData.Size);
TestStreamMD5 := umlStreamMD5String(TestStreamData).Text;

TestBuffSize := 512 * 1024 + 64;
TestBuff := System.GetMemory(TestBuffSize);
MakeRndBuff(777777, TestBuff, TestBuffSize);
TestBuffMD5 := umlMD5String(TestBuff, TestBuffSize).Text;

finalization

DisposeObject(TestStreamData);
TestStreamData := nil;
FreeMem(TestBuff, TestBuffSize);

end.
