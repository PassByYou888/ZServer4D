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
unit CommunicationTest;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CommunicationFramework, DataFrameEngine,
  UnicodeMixedLib, CoreClasses, DoStatusIO, MemoryStream64, PascalStrings,
  CommunicationFrameworkIO, CoreCipher, NotifyObjectBase;

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
    procedure Delay_RunTestReponse(Sender: TNPostExecute);
    procedure Cmd_RunTestReponse(Sender: TPeerIO; InData: TDataFrameEngine);

    // server test command result
    procedure CmdResult_TestConsole(Sender: TPeerIO; ResultData: SystemString);
    procedure CmdResult_TestStream(Sender: TPeerIO; ResultData: TDataFrameEngine);

    procedure RegCmd(Intf: TCommunicationFramework);
    procedure ExecuteTest(Intf: TPeerIO);
    procedure ExecuteAsyncTest(Intf: TPeerIO);
    procedure ExecuteAsyncTestWithBigStream(Intf: TPeerIO);
    procedure ExecuteTestReponse(Intf: TPeerIO);

    property LastReg: TCommunicationFramework read FLastReg;
  end;

implementation

const
  C_TestStream = '__@TestStream';
  C_TestConsole = '__@TestConsole';
  C_TestDirectStream = '__@TestDirectStream';
  C_TestDirectConsole = '__@TestDirectConsole';
  C_TestBigStream = '__@TestBigStream';
  C_BigStreamPostInfo = '__@BigStreamPostInfo';
  C_TestCompleteBuffer = '__@TestCompleteBuffer';
  C_RemoteInfo = '__@RemoteInfo';
  C_RunTestReponse = '__@RunTestReponse';

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

procedure TCommunicationTestIntf.Delay_RunTestReponse(Sender: TNPostExecute);
begin
  ExecuteAsyncTestWithBigStream(TPeerIO(Sender.Data1));
end;

procedure TCommunicationTestIntf.Cmd_RunTestReponse(Sender: TPeerIO; InData: TDataFrameEngine);
begin
  Sender.OwnerFramework.PostProgress.PostExecuteM(3, {$IFDEF FPC}@{$ENDIF FPC}Delay_RunTestReponse).Data1 := Sender;
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
  Intf.RegisterStream(C_TestStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestStream;
  Intf.RegisterConsole(C_TestConsole).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestConsole;
  Intf.RegisterDirectStream(C_TestDirectStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestDirectStream;
  Intf.RegisterDirectConsole(C_TestDirectConsole).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestDirectConsole;
  Intf.RegisterBigStream(C_TestBigStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestBigStream;
  Intf.RegisterDirectConsole(C_BigStreamPostInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_BigStreamPostInfo;
  Intf.RegisterCompleteBuffer(C_TestCompleteBuffer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_TestCompleteBuffer;
  Intf.RegisterDirectConsole(C_RemoteInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_RemoteInfo;
  Intf.RegisterDirectStream(C_RunTestReponse).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Cmd_RunTestReponse;

  FLastReg := Intf;
end;

procedure TCommunicationTestIntf.ExecuteTest(Intf: TPeerIO);
var
  tmpdf: TDataFrameEngine;
begin
  Intf.SendConsoleCmdM(C_TestConsole, FPrepareSendConsole, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestConsole);
  Intf.SendStreamCmdM(C_TestStream, FPrepareSendDataFrame, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestStream);
  Intf.SendDirectConsoleCmd(C_TestDirectConsole, FPrepareSendConsole);
  Intf.SendDirectStreamCmd(C_TestDirectStream, FPrepareSendDataFrame);
  Intf.SendBigStream(C_TestBigStream, TestStreamData, False);
  Intf.SendDirectConsoleCmd(C_BigStreamPostInfo, umlStreamMD5String(TestStreamData).Text);
  Intf.SendCompleteBuffer(C_TestCompleteBuffer, TestBuff, TestBuffSize, False);

  if Intf.OwnerFramework is TCommunicationFrameworkClient then
    begin
      if Intf.WaitSendConsoleCmd(C_TestConsole, FPrepareSendConsole, 0) <> FPrepareResultConsole then
          Intf.Print('wait Mode:TestResultConsole Data failed!');

      tmpdf := TDataFrameEngine.Create;
      Intf.WaitSendStreamCmd(C_TestStream, FPrepareSendDataFrame, tmpdf, 0);
      if not tmpdf.Compare(FPrepareResultDataFrame) then
          Intf.Print('wait Mode:TestResultStream Data failed!');
      DisposeObject(tmpdf);
    end;

  Intf.SendDirectConsoleCmd(C_RemoteInfo, 'client id[' + IntToStr(Intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteAsyncTest(Intf: TPeerIO);
begin
  Intf.SendConsoleCmdM(C_TestConsole, FPrepareSendConsole, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestConsole);
  Intf.SendStreamCmdM(C_TestStream, FPrepareSendDataFrame, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestStream);
  Intf.SendDirectConsoleCmd(C_TestDirectConsole, FPrepareSendConsole);
  Intf.SendDirectStreamCmd(C_TestDirectStream, FPrepareSendDataFrame);

  Intf.SendDirectConsoleCmd(C_RemoteInfo, 'client id[' + IntToStr(Intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteAsyncTestWithBigStream(Intf: TPeerIO);
begin
  Intf.SendConsoleCmdM(C_TestConsole, FPrepareSendConsole, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestConsole);
  Intf.SendStreamCmdM(C_TestStream, FPrepareSendDataFrame, {$IFDEF FPC}@{$ENDIF FPC}CmdResult_TestStream);
  Intf.SendDirectConsoleCmd(C_TestDirectConsole, FPrepareSendConsole);
  Intf.SendDirectStreamCmd(C_TestDirectStream, FPrepareSendDataFrame);

  Intf.SendBigStream(C_TestBigStream, TestStreamData, False);
  Intf.SendDirectConsoleCmd(C_BigStreamPostInfo, TestStreamMD5);
  Intf.SendCompleteBuffer(C_TestCompleteBuffer, TestBuff, TestBuffSize, False);

  Intf.SendDirectConsoleCmd(C_RemoteInfo, 'client id[' + IntToStr(Intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteTestReponse(Intf: TPeerIO);
begin
  Intf.SendDirectStreamCmd(C_RunTestReponse);
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
