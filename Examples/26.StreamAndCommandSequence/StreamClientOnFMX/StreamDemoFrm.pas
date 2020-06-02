unit StreamDemoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  CoreClasses, PascalStrings, UnicodeMixedLib, MemoryStream64, DataFrameEngine, NotifyObjectBase,
  CommunicationFramework, XNATPhysics, DoStatusIO, CoreCipher, CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Layout1: TLayout;
    Label1: TLabel;
    hostEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label2: TLabel;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label3: TLabel;
    Button8: TButton;
    Timer1: TTimer;
    BusyLabel: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SingleTunnelClient: TXPhysicsClient;
    DoubleTunnelClient: TCommunicationFramework_DoubleTunnelClient_NoAuth;
    Prepared_Stream: TMemoryStream64;

    procedure Backcall_DoStatus(AText: SystemString; const ID: Integer);

    procedure cmd1;
    procedure cmd2;
    procedure cmd3;

    procedure MyDataframe;

    procedure MyDataframe_Direct;
    procedure MyBigStream;

    procedure MyCompleteBuffer;

    procedure MyBatchStream;
    procedure MyBatchStreamOver(Sender: TPeerIO; InData: TDataFrameEngine);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.Button1Click(Sender: TObject);
begin
  SingleTunnelClient.AsyncConnectP(hostEdit.Text, 19991, procedure(const cState: Boolean)
    begin
      if cState then
          DoStatus('single tunnel connected!');
    end);

  DoubleTunnelClient.AsyncConnectP(hostEdit.Text, 19993, 19992, procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('double tunnel connected!');
          DoubleTunnelClient.TunnelLinkP(procedure(const lState: Boolean)
            begin
              DoStatus('double tunnel link ok successed!');
            end);
        end;
    end);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SingleTunnelClient.Disconnect;
  DoubleTunnelClient.Disconnect;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SingleTunnelClient.SyncOnResult := True;
  cmd1;
  cmd2;
  cmd3;
  SingleTunnelClient.WaitP(1000, procedure(const cState: Boolean)
    begin
      SingleTunnelClient.PostProgress.PostExecuteP(1.0, procedure(Sender: TNPostExecute)
        begin
          SingleTunnelClient.SyncOnResult := False;
          cmd1;
          cmd2;
          cmd3;
        end);
    end);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  MyDataframe;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  MyDataframe_Direct;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  MyBigStream;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  MyCompleteBuffer;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  MyBatchStream;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  key: TBytes;
  x: Integer;
begin
  AddDoStatusHookM(Self, Backcall_DoStatus);
  SingleTunnelClient := TXPhysicsClient.Create;
  DoubleTunnelClient := TCommunicationFramework_DoubleTunnelClient_NoAuth.Create(TXPhysicsClient.Create, TXPhysicsClient.Create);
  DoubleTunnelClient.RegisterCommand;
  DoubleTunnelClient.RecvTunnel.RegisterDirectStream('MyBatchStreamOver').OnExecute := MyBatchStreamOver;
  Prepared_Stream := TMemoryStream64.Create;

  // 16M size of stream
  Prepared_Stream.SetSize(1024 * 1024 * 16);
  key := umlBytesOf('123456');
  SequEncryptCBC(TCipherSecurity.csRijndael, Prepared_Stream.Memory, Prepared_Stream.Size, key, True, False);

  // print prepared
  DoStatus('prepared stream crc32: ' + TCipher.GenerateHashString(THashSecurity.hsCRC32, Prepared_Stream.Memory, Prepared_Stream.Size));
  DoStatus('prepared stream md5: ' + TCipher.GenerateHashString(THashSecurity.hsFastMD5, Prepared_Stream.Memory, Prepared_Stream.Size));
  DoStatus('prepared stream sha256: ' + TCipher.GenerateHashString(THashSecurity.hsSHA256, Prepared_Stream.Memory, Prepared_Stream.Size));

  Timer1.Enabled := True;

  try
    x := 12;
    exit;
  finally
      x := 11;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  SingleTunnelClient.Progress;
  DoubleTunnelClient.Progress;

  if SingleTunnelClient.IOBusy or
    DoubleTunnelClient.RecvTunnel.IOBusy or
    DoubleTunnelClient.SendTunnel.IOBusy then
      BusyLabel.Text := 'Busy...'
  else
      BusyLabel.Text := 'IDLE...';
end;

procedure TForm1.Backcall_DoStatus(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm1.cmd1;
begin
  SingleTunnelClient.SendStreamCmdP('1', nil, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
    begin
      DoStatus('cmd Response 1');
    end);
end;

procedure TForm1.cmd2;
begin
  SingleTunnelClient.SendStreamCmdP('2', nil, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
    begin
      DoStatus('cmd Response 2');
    end);
end;

procedure TForm1.cmd3;
begin
  SingleTunnelClient.SendStreamCmdP('3', nil, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
    begin
      DoStatus('cmd Response 3');
    end);
end;

procedure TForm1.MyDataframe;
var
  m: TMemoryStream64;
  d: TTimeTick;
begin
  SingleTunnelClient.SyncOnResult := True;
  d := GetTimeTick;
  m := TMemoryStream64.Create;
  TComputeThread.RunP(nil, nil,
    procedure(Sender: TComputeThread)
    var
      de: TDataFrameEngine;
    begin
      de := TDataFrameEngine.Create;
      de.write(Prepared_Stream.Memory^, Prepared_Stream.Size);
      de.EncodeTo(m, True);
      disposeObject(de);
    end,
    procedure(Sender: TComputeThread)
    begin
      SingleTunnelClient.SendStreamCmdP('MyDataframe', m, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
        begin
          Sender.Print('complete,time:%d', [GetTimeTick - d]);
        end, True);
    end);
end;

procedure TForm1.MyDataframe_Direct;
var
  de: TDataFrameEngine;
  d: TTimeTick;
begin
  SingleTunnelClient.SyncOnResult := True;
  de := TDataFrameEngine.Create;
  de.WriteStream(Prepared_Stream);
  d := GetTimeTick;
  SingleTunnelClient.SendDirectStreamCmd('MyDataframe_Direct', de);
  disposeObject(de);
  SingleTunnelClient.WaitP(0, procedure(const cState: Boolean)
    begin
      SingleTunnelClient.ClientIO.Print('complete,time:%d', [GetTimeTick - d]);
    end);
end;

procedure TForm1.MyBigStream;
var
  d: TTimeTick;
begin
  SingleTunnelClient.SyncOnResult := True;
  d := GetTimeTick;
  SingleTunnelClient.SendDirectStreamCmd('MyBigStreamInit');
  SingleTunnelClient.SendBigStream('MyBigStream', Prepared_Stream, False);
  SingleTunnelClient.WaitP(0, procedure(const cState: Boolean)
    begin
      SingleTunnelClient.ClientIO.Print('complete,time:%d', [GetTimeTick - d]);
    end);
end;

procedure TForm1.MyCompleteBuffer;
var
  d: TTimeTick;
begin
  SingleTunnelClient.SyncOnResult := True;
  d := GetTimeTick;
  SingleTunnelClient.SendCompleteBuffer('MyCompleteBuffer', Prepared_Stream.Memory, Prepared_Stream.Size, False);
  SingleTunnelClient.WaitP(0, procedure(const cState: Boolean)
    begin
      SingleTunnelClient.ClientIO.Print('complete,time:%d', [GetTimeTick - d]);
    end);
end;

procedure TForm1.MyBatchStream;
var
  d: TTimeTick;
begin
  d := GetTimeTick;
  SingleTunnelClient.SyncOnResult := True;
  DoubleTunnelClient.ClearBatchStream;
  DoubleTunnelClient.PostBatchStream(Prepared_Stream, False);
  DoubleTunnelClient.PostBatchStream(Prepared_Stream, False);
  DoubleTunnelClient.PostBatchStream(Prepared_Stream, False);
  DoubleTunnelClient.SendTunnel.WaitP(0, procedure(const cState: Boolean)
    begin
      SingleTunnelClient.ClientIO.Print('complete,time:%d', [GetTimeTick - d]);
    end);
  DoubleTunnelClient.SendTunnel.SendDirectStreamCmd('MyBatchStream');
  DoubleTunnelClient.ClearBatchStream;
end;

procedure TForm1.MyBatchStreamOver(Sender: TPeerIO; InData: TDataFrameEngine);
var
  i: Integer;
  m: TMemoryStream64;
begin
  DoStatus('batch echo to local');
  for i := 0 to Sender.UserDefine.BigStreamBatchList.Count - 1 do
    begin
      m := Sender.UserDefine.BigStreamBatchList[i]^.Source;
      DoStatus('echo batch ' + IntToStr(i) + ' stream crc32: ' + TCipher.GenerateHashString(THashSecurity.hsCRC32, m.Memory, m.Size));
      DoStatus('echo batch ' + IntToStr(i) + ' stream md5: ' + TCipher.GenerateHashString(THashSecurity.hsFastMD5, m.Memory, m.Size));
      DoStatus('echo batch ' + IntToStr(i) + ' stream sha256: ' + TCipher.GenerateHashString(THashSecurity.hsSHA256, m.Memory, m.Size));
    end;
end;

end.
 
