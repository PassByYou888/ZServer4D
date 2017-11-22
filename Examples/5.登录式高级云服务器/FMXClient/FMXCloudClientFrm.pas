unit FMXCloudClientFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  LoginMobileClient, ManagerServerQueryClient,
  CommunicationFramework_Client_Indy, CommunicationFramework, DoStatusIO,
  CoreClasses;

type
  TManagerQuery = class(TManagerQueryBase)
  public
    procedure Connect(Addr: string; Port: Word); override;
  end;

  TFMXCloudClientForm = class(TForm, ILoginBackCallInterface)
    Memo1: TMemo;
    QueryButton: TButton;
    HostEdit: TEdit;
    Timer1: TTimer;
    Label1: TLabel;
    UserIDEdit: TEdit;
    PasswdEdit: TEdit;
    RegGuestUserButton: TButton;
    LoginButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure QueryButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RegGuestUserButtonClick(Sender: TObject);
    procedure LoginButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoginDisconnect;
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    ManagerServerQuery: TManagerQuery;
    LoginClient       : TLoginClientBase;
  end;

var
  FMXCloudClientForm: TFMXCloudClientForm;

implementation

{$R *.fmx}


procedure TManagerQuery.Connect(Addr: string; Port: Word);
begin
  TCommunicationFramework_Client_Indy(Client).Connect(Addr, Port);
end;

procedure TFMXCloudClientForm.LoginButtonClick(Sender: TObject);
var
  a: TAddressInfo;
  d: TTimeTickValue;
begin
  for a in ManagerServerQuery.ServerList do
    begin
      if a.ServerType = cCoreLogicServer then
        begin
          TCommunicationFramework_Client_Indy(LoginClient.NetSendTunnelIntf).Connect(a.Host, a.SendPort);
          TCommunicationFramework_Client_Indy(LoginClient.NetRecvTunnelIntf).Connect(a.Host, a.RecvPort);

          d := GetTimeTickCount;
          while (LoginClient.Connected) and (not LoginClient.RemoteInited) do
            begin
              LoginClient.Progress;
              if GetTimeTickCount - d > 5000 then
                begin
                  LoginClient.Disconnect;
                  exit;
                end;
            end;

          if (LoginClient.Connected) and (LoginClient.RemoteInited) then
            begin
              if LoginClient.UserLogin(UserIDEdit.Text, PasswdEdit.Text) then
                if LoginClient.TunnelLink then
                    DoStatus('登录成功！');
            end;
        end;
    end;
end;

procedure TFMXCloudClientForm.LoginDisconnect;
begin

end;

procedure TFMXCloudClientForm.RegGuestUserButtonClick(Sender: TObject);
var
  a: TAddressInfo;
  d: TTimeTickValue;
begin
  for a in ManagerServerQuery.ServerList do
    begin
      if a.ServerType = cCoreLogicServer then
        begin
          TCommunicationFramework_Client_Indy(LoginClient.NetSendTunnelIntf).Connect(a.Host, a.SendPort);
          TCommunicationFramework_Client_Indy(LoginClient.NetRecvTunnelIntf).Connect(a.Host, a.RecvPort);

          d := GetTimeTickCount;
          while (LoginClient.Connected) and (not LoginClient.RemoteInited) do
            begin
              LoginClient.Progress;
              if GetTimeTickCount - d > 5000 then
                begin
                  LoginClient.Disconnect;
                  exit;
                end;
            end;

          if (LoginClient.Connected) and (LoginClient.RemoteInited) then
            begin
              LoginClient.RegGuestUser(
                procedure(const UserName, UserPasswd: string; const state: Boolean)
                begin
                  if state then
                    begin
                      UserIDEdit.Text := UserName;
                      PasswdEdit.Text := UserPasswd;
                    end;
                  LoginClient.Disconnect;
                end
                );
            end;
        end;
    end;
end;

procedure TFMXCloudClientForm.Timer1Timer(Sender: TObject);
begin
  ManagerServerQuery.Progress;
  LoginClient.Progress;
end;

procedure TFMXCloudClientForm.QueryButtonClick(Sender: TObject);
var
  a: TAddressInfo;
begin
  if ManagerServerQuery.Query(HostEdit.Text) then
    begin
      for a in ManagerServerQuery.ServerList do
          DoStatus('类型:%s 注册名:%s 地址:%s 接收端口:%d 发送端口:%d)', [serverType2Str(a.ServerType), a.RegName, a.Host, a.RecvPort, a.SendPort]);
    end;
end;

procedure TFMXCloudClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure BackCall_ProgressBackgroundProc;
begin
  Application.ProcessMessages;
end;

procedure TFMXCloudClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusNear);

  ProgressBackgroundProc := BackCall_ProgressBackgroundProc;

  ManagerServerQuery := TManagerQuery.Create(TCommunicationFramework_Client_Indy.Create);
  LoginClient := TLoginClientBase.Create(Self, TCommunicationFramework_Client_Indy.Create, TCommunicationFramework_Client_Indy.Create);
end;

procedure TFMXCloudClientForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  DisposeObject([ManagerServerQuery, LoginClient]);
end;

end.
