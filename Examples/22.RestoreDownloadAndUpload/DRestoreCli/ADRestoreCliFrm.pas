unit ADRestoreCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  DoStatusIO, CoreClasses, PascalStrings, UnicodeMixedLib, ListEngine, MemoryStream64, NotifyObjectBase,
  CommunicationFramework,
  CommunicationFrameworkDoubleTunnelIO,
  CommunicationFramework_Client_CrossSocket,
  Cadencer, DataFrameEngine;

type
  TAuthDoubleTunnelClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    BlockBatchOperationBtn: TButton;
    UserEdit: TLabeledEdit;
    PasswdEdit: TLabeledEdit;
    RegUserButton: TButton;
    AsyncConnectButton: TButton;
    RestoreDownloadButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RegUserButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure AsyncConnectButtonClick(Sender: TObject);
    procedure BlockBatchOperationBtnClick(Sender: TObject);
    procedure RestoreDownloadButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    RecvTunnel: TCommunicationFramework_Client_CrossSocket;
    SendTunnel: TCommunicationFramework_Client_CrossSocket;
    client: TCommunicationFramework_DoubleTunnelClient;
  end;

var
  AuthDoubleTunnelClientForm: TAuthDoubleTunnelClientForm;

implementation

{$R *.dfm}


procedure TAuthDoubleTunnelClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TCommunicationFramework_Client_CrossSocket.Create;
  SendTunnel := TCommunicationFramework_Client_CrossSocket.Create;
  client := TCommunicationFramework_DoubleTunnelClient.Create(RecvTunnel, SendTunnel);

  client.RegisterCommand;
end;

procedure TAuthDoubleTunnelClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TAuthDoubleTunnelClientForm.Timer1Timer(Sender: TObject);
begin
  client.Progress;
end;

procedure TAuthDoubleTunnelClientForm.RegUserButtonClick(Sender: TObject);
begin
  SendTunnel.Connect(HostEdit.Text, 9815);
  RecvTunnel.Connect(HostEdit.Text, 9816);

  // 检查双通道是否都已经成功链接，确保完成了对称加密等等初始化工作
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
      client.RegisterUser(UserEdit.Text, PasswdEdit.Text);

  SendTunnel.Disconnect;
  RecvTunnel.Disconnect;
end;

procedure TAuthDoubleTunnelClientForm.ConnectButtonClick(Sender: TObject);
begin
  SendTunnel.Connect(HostEdit.Text, 9815);
  RecvTunnel.Connect(HostEdit.Text, 9816);

  // 检查双通道是否都已经成功链接，确保完成了对称加密等等初始化工作
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
    begin
      // 嵌套式匿名函数支持
      client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
        procedure(const State: Boolean)
        begin
          if State then
              client.TunnelLinkP(
              procedure(const State: Boolean)
              begin
                DoStatus('double tunnel link success!');
              end)
        end);
    end;
end;

procedure TAuthDoubleTunnelClientForm.AsyncConnectButtonClick(Sender: TObject);
begin
  // 方法2，异步式双通道链接
  client.AsyncConnectP(HostEdit.Text, 9816, 9815,
    procedure(const cState: Boolean)
    begin
      if cState then
        begin
          // 嵌套式匿名函数支持
          client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
            procedure(const lState: Boolean)
            begin
              if lState then
                begin
                  client.TunnelLinkP(
                    procedure(const tState: Boolean)
                    begin
                      if tState then
                          DoStatus('double tunnel link success!')
                      else
                          DoStatus('double tunnel link failed!');
                    end)
                end
              else
                begin
                  if lState then
                      DoStatus('login success!')
                  else
                      DoStatus('login failed!');
                end;
            end);
        end
      else
        begin
          if cState then
              DoStatus('connected success!')
          else
              DoStatus('connected failed!');
        end;
    end);

end;

procedure TAuthDoubleTunnelClientForm.BlockBatchOperationBtnClick(Sender: TObject);
var
  lst: TCoreClassStringList;
  hashSiz: THashVariantList;
  hashMD5: THashStringList;
  i: Integer;
begin
  lst := TCoreClassStringList.Create;
  client.GetPublicFileList('*.*', lst);
  hashSiz := THashVariantList.CustomCreate(1024);
  hashMD5 := THashStringList.CustomCreate(1024);

  // 异步获取远程文件信息，由于在一个for中循环遍历所有而不经过Progress(时间片主循环)，所以这里就是一个批次
  for i := 0 to lst.Count - 1 do
    begin
      client.GetPublicFileInfoP(lst[i], nil, nil,
        procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
          const fileName: SystemString; const Existed: Boolean; const fSiz: Int64)
        begin
          if Existed then
              hashSiz.Add(fileName, fSiz);
        end);
    end;

  // 等待上面异步命令处理完成，这里是也是在按一个批次模拟阻塞
  client.SendTunnel.Wait(20 * 1000);

  // 当一个批次被执行完成，开始遍历第二个批次
  hashSiz.ProgressP(procedure(Sender: THashVariantList; Name: PSystemString; const v: Variant)
    begin
      client.GetPublicFileMD5P(Name^, 0, v, nil, nil,
        procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
          const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5)
        begin
          hashMD5.Add(fileName, umlMD5ToStr(MD5).Text);
        end);
    end);

  // 等待上面异步命令处理完成，这里是也是在按一个批次模拟阻塞
  client.SendTunnel.Wait(20 * 1000);

  // 当第二个批次遍历完成，我们将md5信息打印出来
  DoStatus('远程文件列表:');
  hashMD5.ProgressP(procedure(Sender: THashStringList; Name: PSystemString; const v: SystemString)
    begin
      DoStatus('文件名:%s 体积:%s md5:%s', [Name^, umlSizeToStr(hashSiz[Name^]).Text, v]);
    end);

  DisposeObject(lst);
  DisposeObject(hashSiz);
  DisposeObject(hashMD5);
end;

procedure TAuthDoubleTunnelClientForm.RestoreDownloadButtonClick(Sender: TObject);
begin
  // 先从远程下载完整文件
  client.GetPublicFileP('ADRestoreServer.exe', umlCurrentPath, nil, nil,
    procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream; const fileName: SystemString)
    var
      m5: TMD5;
      m64: TMemoryStream64;
    begin
      m5 := umlStreamMD5(stream, 0, 512);
      DoStatus('Local MD5(0..512):%s', [umlMD5ToStr(m5).Text]);
      m64 := TMemoryStream64.Create;
      m64.LoadFromStream(stream);

      // 获取远程文件截断的md5
      client.GetPublicFileMD5P('ADRestoreServer.exe', 0, 512, nil, nil,
        procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
          const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5)
        begin
          // 只取文件的头512byte重新来存储，为后面的断点续传demo打下基础
          DoStatus('remote MD5(0..512):%s', [umlMD5ToStr(MD5).Text]);
          m64.Size := 512;
          m5 := umlStreamMD5(m64);
          DoStatus('Local MD5(0..512):%s', [umlMD5ToStr(m5).Text]);
          m64.SaveToFile(umlCombineFileName(umlCurrentPath, 'ADRestoreServer.exe'));
          DisposeObject(m64);

          // 获取远程文件完整的md5
          client.GetPublicFileMD5P('ADRestoreServer.exe', 0, 0, nil, nil,
            procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
              const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5)
            begin
              DoStatus('remote MD5:%s', [umlMD5ToStr(MD5).Text]);
              // 从远程断点续传下载
              client.GetPublicFileP('ADRestoreServer.exe', 512, umlCurrentPath, nil, nil,
                procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream; const fileName: SystemString)
                begin
                  // 如果上面两个md5打印结果相同，则说明断点续传的文件下载是完整的
                  DoStatus('Local MD5:%s', [umlMD5ToStr(umlStreamMD5(stream)).Text]);

                  DoStatus('大约1秒后开始演示断点续传的上传');

                  // 现在我们来演示断点续传的上传功能
                  client.ProgressEngine.PostExecuteP(1.0, procedure(Sender: TNPostExecute)
                    var
                      m64: TMemoryStream64;
                    begin
                      m64 := TMemoryStream64.Create;
                      m64.loadFromFile(umlCombineFileName(umlCurrentPath, 'ADRestoreServer.exe'));
                      // 截断文件体，让它长度为999byte
                      m64.Size := 999;
                      DoStatus('private local md5(0..999):%s', [umlMD5ToStr(umlStreamMD5(m64)).Text]);
                      // 我们使用私人空间来存储上传文件，最后的一个参数表示完成上传后，自动释放m64
                      client.PostStreamToPrivate('testUpload.dat', '', m64, True);
                      // 因为上传文件使用的SendTunnel，现在，我们在SendTunnel做一个Wait事件来侦测上传是否完成
                      client.SendTunnel.WaitP(10 * 1000,
                        procedure(const State: Boolean)
                        begin
                          // 在这里触发事件，表示上传已经完成了
                          client.GetPrivateFileMD5P('testUpload.dat', '', 0, 0, nil, nil,
                            procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
                              const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5)
                            var
                              m64_2: TMemoryStream64;
                            begin
                              DoStatus('private remote md5(0..999):%s', [umlMD5ToStr(MD5).Text]);
                              m64_2 := TMemoryStream64.Create;
                              m64_2.loadFromFile(umlCombineFileName(umlCurrentPath, 'ADRestoreServer.exe'));
                              DoStatus('private local md5:%s', [umlMD5ToStr(umlStreamMD5(m64_2)).Text]);
                              // 我们开始做断点续传的上传
                              client.PostStreamToPrivate('testUpload.dat', '', m64_2, 999, True);

                              // 因为上传文件使用的SendTunnel，现在，我们在SendTunnel做一个Wait事件来侦测上传是否完成
                              client.SendTunnel.WaitP(10 * 1000,
                                procedure(const State: Boolean)
                                begin
                                  client.GetPrivateFileMD5P('testUpload.dat', '', 0, 0, nil, nil,
                                    procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
                                      const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5)
                                    begin
                                      // 如果上面两个md5相同，则表示断点续传的上传已经成功
                                      DoStatus('private remote md5:%s', [umlMD5ToStr(MD5).Text]);
                                      DoStatus('restore demo over!');
                                    end);
                                end);
                            end);
                        end);
                    end);
                end);
            end);
        end);
    end);
end;

procedure TAuthDoubleTunnelClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

end.
