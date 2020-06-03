unit AsyncServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ZS_JsonDataObjects,
  CommunicationFramework,
  PhysicsIO,
  DoStatusIO, CoreClasses,
  DataFrameEngine, UnicodeMixedLib, MemoryStream64;

type
  // TPeerClientUserSpecial是基于每用户链接后自动创建的实例
  // 使用时候请注意释放内存
  // TPeerClientUserDefine用于Auth,DB等等服务
  // TPeerClientUserSpecial的作用是与高级服务的Auth,DB发生冲突时，对开发者提供独享实例
  TMySpecialDefine = class(TPeerClientUserSpecial)
  public
    tempStream: TMemoryStream64;
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TAsyncServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
    procedure cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);

    procedure cmd_Json_Stream(Sender: TPeerClient; InData: TDataFrameEngine);

    procedure cmd_TestMiniStream(Sender: TPeerClient; InData: TDataFrameEngine);

    procedure cmd_Test128MBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);

    procedure cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
  public
    { Public declarations }
    server: TPhysicsServer;
  end;

var
  AsyncServerForm: TAsyncServerForm;

implementation

{$R *.dfm}


procedure TAsyncServerForm.cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
begin
  DoStatus('client: %s', [InData]);
end;

procedure TAsyncServerForm.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TAsyncServerForm.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  OutData.WriteString('result 654321');
end;

procedure TAsyncServerForm.cmd_Json_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  js: TJsonObject;
  ns: TStringList;
begin
  js := TJsonObject.Create;
  ns := TStringList.Create;
  InData.Reader.ReadJson(js);
  js.SaveToLines(ns);
  DoStatus(ns);
  disposeObject(ns);
  disposeObject(js);
end;

procedure TAsyncServerForm.cmd_TestMiniStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  InData.Reader.ReadStream(ms);

  DoStatus(umlMD5Char(ms.Memory, ms.Size).Text);

  disposeObject(ms);
end;

procedure TAsyncServerForm.cmd_Test128MBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  tempStream: TMemoryStream64;
begin
  tempStream := TMySpecialDefine(Sender.UserSpecial).tempStream;
  tempStream.CopyFrom(InData, InData.Size);
  DoStatus('%d/%d', [BigStreamTotal, BigStreamCompleteSize]);

  // bigstream complete
  if tempStream.Size = BigStreamTotal then
    begin
      Sender.Print('bigsteram finish');
      Sender.Print('bigsteram md5:' + umlMD5Char(tempStream.Memory, tempStream.Size).Text);
      tempStream.Clear;
    end;
end;

procedure TAsyncServerForm.cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
begin
  Sender.Print('Complete buffer md5: %s', [umlMD5String(InData, DataSize).Text]);
end;

procedure TAsyncServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAsyncServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  server := TPhysicsServer.Create;
  server.PeerClientUserSpecialClass := TMySpecialDefine;
  server.TimeOutIDLE := 15 * 1000;

  // 更改最大completeBuffer，这里只用于测试，正常运行服务器，这里一般给4M就可以了
  server.MaxCompleteBufferSize := 128 * 1024 * 1024;

  server.RegisterDirectConsole('helloWorld_Console').OnExecute := cmd_helloWorld_Console;
  server.RegisterDirectStream('helloWorld_Stream').OnExecute := cmd_helloWorld_Stream;
  server.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;

  server.RegisterDirectStream('Json_Stream').OnExecute := cmd_Json_Stream;

  server.RegisterDirectStream('TestMiniStream').OnExecute := cmd_TestMiniStream;
  server.RegisterBigStream('Test128MBigStream').OnExecute := cmd_Test128MBigStream;

  // 注册Completebuffer指令
  server.RegisterCompleteBuffer('TestCompleteBuffer').OnExecute := cmd_TestCompleteBuffer;
end;

procedure TAsyncServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject(server);
  DeleteDoStatusHook(self);
end;

procedure TAsyncServerForm.StartServiceButtonClick(Sender: TObject);
begin
  // 基于CrosssSocket官方文档，绑定字符串如果为空，绑定IPV6+IPV4
  if server.StartService('', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!')
end;

procedure TAsyncServerForm.Timer1Timer(Sender: TObject);
begin
  server.Progress;
end;

{ TMySpecialDefine }

constructor TMySpecialDefine.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  tempStream := TMemoryStream64.Create;
end;

destructor TMySpecialDefine.Destroy;
begin
  DoStatus('%s disconnect', [Owner.GetPeerIP]);
  disposeObject(tempStream);
  inherited Destroy;
end;

end.
