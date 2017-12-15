unit PeformanceTestServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.TypInfo,
  CommunicationFramework,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket, DoStatusIO, CoreClasses,
  DataFrameEngine, UnicodeMixedLib, MemoryStream64, CommunicationTest,
  PascalStrings, ListEngine;

type
  TEZServerForm = class(TForm)
    readmeMemo: TMemo;
    Timer: TTimer;
    StateMemo: TMemo;
    RefStateTimer: TTimer;
    ReceiveMemo: TMemo;
    SendMemo: TMemo;
    CpuMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure RefStateTimerTimer(Sender: TObject);
  private
    { Private declarations }
    tempStream: TMemoryStream64;
  public
    { Public declarations }
    server: TCommunicationFramework_Server_CrossSocket;
    test  : TCommunicationTestIntf;
  end;

var
  EZServerForm: TEZServerForm;

implementation

{$R *.dfm}


procedure TEZServerForm.FormCreate(Sender: TObject);
begin
  server := TCommunicationFramework_Server_CrossSocket.Create;
  test := TCommunicationTestIntf.Create;

  server.AllowPrintCommand := False;
  server.SwitchMaxPerformance;

  test.RegCmd(server);

  // 基于CrosssSocket官方文档，绑定字符串如果为空，绑定IPV6+IPV4
  if server.StartService('', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!')
end;

procedure TEZServerForm.FormDestroy(Sender: TObject);
begin
  DisposeObject([test, server]);
  DeleteDoStatusHook(self);
end;

procedure TEZServerForm.RefStateTimerTimer(Sender: TObject);
  procedure PrintServerState(const arry: array of TCommunicationFramework);
  var
    buff: array [TStatisticsType] of Int64;
    comm: TCommunicationFramework;
    st  : TStatisticsType;
    i   : Integer;
    v   : Int64;
  begin
    for st := low(TStatisticsType) to high(TStatisticsType) do
        buff[st] := 0;

    for comm in arry do
      begin
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];
      end;

    StateMemo.Lines.BeginUpdate;
    StateMemo.Lines.Clear;
    StateMemo.Lines.Add('Statistics...');
    for st := low(TStatisticsType) to high(TStatisticsType) do
      begin
        v := buff[st];
        StateMemo.Lines.Add(GetEnumName(TypeInfo(TStatisticsType), Ord(st)) + ' : ' + IntToStr(v));
      end;
    StateMemo.Lines.EndUpdate;
  end;

  procedure PrintServerCMDStatistics(const arry: array of TCommunicationFramework);
  var
    RecvLst, SendLst, ExecuteConsumeLst: THashVariantList;
    comm                               : TCommunicationFramework;
    i                                  : Integer;
    lst                                : TListString;
  begin
    RecvLst := THashVariantList.Create;
    SendLst := THashVariantList.Create;
    ExecuteConsumeLst := THashVariantList.Create;
    for comm in arry do
      begin
        RecvLst.IncValue(comm.CmdRecvStatistics);
        SendLst.IncValue(comm.CmdSendStatistics);
        ExecuteConsumeLst.SetMax(comm.CmdMaxExecuteConsumeStatistics);
      end;

    lst := TListString.Create;
    RecvLst.GetNameList(lst);

    ReceiveMemo.Lines.BeginUpdate;
    ReceiveMemo.Lines.Clear;
    ReceiveMemo.Lines.Add('Received commands...');
    for i := 0 to lst.Count - 1 do
        ReceiveMemo.Lines.Add(lst[i] + ' : ' + VarToStr(RecvLst[lst[i]]));
    ReceiveMemo.Lines.EndUpdate;

    DisposeObject(lst);

    lst := TListString.Create;
    SendLst.GetNameList(lst);

    SendMemo.Lines.BeginUpdate;
    SendMemo.Lines.Clear;
    SendMemo.Lines.Add('Send commands...');
    for i := 0 to lst.Count - 1 do
        SendMemo.Lines.Add(lst[i] + ' : ' + VarToStr(SendLst[lst[i]]));
    SendMemo.Lines.EndUpdate;

    DisposeObject(lst);

    lst := TListString.Create;
    ExecuteConsumeLst.GetNameList(lst);

    CpuMemo.Lines.BeginUpdate;
    CpuMemo.Lines.Clear;
    CpuMemo.Lines.Add('usage cpu...');
    for i := 0 to lst.Count - 1 do
        CpuMemo.Lines.Add(lst[i] + ' : ' + VarToStr(ExecuteConsumeLst[lst[i]]) + 'ms');
    CpuMemo.Lines.EndUpdate;

    DisposeObject(lst);

    DisposeObject([RecvLst, SendLst, ExecuteConsumeLst]);
  end;

begin
  PrintServerState([server]);
  PrintServerCMDStatistics([server]);
end;

procedure TEZServerForm.TimerTimer(Sender: TObject);
begin
  server.ProgressBackground;
  Caption := Format('online client:%d', [server.Count]);
end;

end.
