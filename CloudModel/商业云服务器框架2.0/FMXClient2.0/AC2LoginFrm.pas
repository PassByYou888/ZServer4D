unit AC2LoginFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects, FMX.Edit, FMX.Effects, FMX.Ani,
  System.IOUtils,

  Geometry2DUnit, CoreClasses, TextDataEngine,
  Cadencer,
  DoStatusIO, NotifyObjectBase, CommunicationFrameworkDoubleTunnelIO_ServMan,
  UnicodeMixedLib, MemoryStream64;

type
  TAC2LoginForm = class(TForm)
    MainLayout: TLayout;
    GameNameLabel: TLabel;
    FloatAnimation5: TFloatAnimation;
    GlowEffect4: TGlowEffect;
    GameEditionInfoLabel: TLabel;
    UserIDEdit: TEdit;
    UserPasswdEdit: TEdit;
    LoginButton: TButton;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LoginButtonClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure UserIDEditKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    procedure Progress(deltaTime: Double);

    procedure DoUserLogin;
    procedure DoUserLogout;
    procedure DoUserRegister;
  end;

var
  AC2LoginForm: TAC2LoginForm;

implementation

{$R *.fmx}


uses AC2ClientGlobal, AC2ProgressFrm, AC2ManagerServerMobileClient,
  AC2LogicFrm, CommonServiceDefine;

procedure TAC2LoginForm.FormResize(Sender: TObject);
begin
  ResetMainLayout(MainLayout, self);
end;

procedure TAC2LoginForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure TAC2LoginForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  GameEditionInfoLabel.Text := CopyRightText;
  GameNameLabel.Text := AppName;

  InitGlobalResource;
  ResetFormSize(self);
  ResetMainLayout(MainLayout, self);
end;

procedure TAC2LoginForm.FormShow(Sender: TObject);
begin
  ReStartAnimation(MainLayout);
  EditChange(nil);
end;

procedure TAC2LoginForm.LoginButtonClick(Sender: TObject);
begin
  if (umlTrimSpace(UserIDEdit.Text).len = 0) or (umlTrimSpace(UserPasswdEdit.Text).len = 0) then
    begin
      DoUserRegister;
    end
  else
    begin
      DoUserLogin;
    end;
end;

procedure TAC2LoginForm.FormDestroy(Sender: TObject);
begin
  FreeGlobalResource;
  DeleteDoStatusHook(self);
end;

procedure TAC2LoginForm.DoStatusNear(AText: string; const ID: Integer);
begin
end;

procedure TAC2LoginForm.Progress(deltaTime: Double);
begin
  if Visible then
      Invalidate;
end;

procedure TAC2LoginForm.UserIDEditKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  EditChange(nil);
end;

procedure TAC2LoginForm.DoUserLogin;
begin
  Hide;
  AC2ProgressForm.Show;
  LogicClient.Disconnect;
  QueryClient.QueryMinLoad(ManagerServerHost, TServerType.stLogic, procedure(const State: Boolean; const Addr: TAddressInfo)
    begin
      if not State then
        begin
          AC2ProgressForm.Hide;
          Show;
          exit;
        end;
      if LogicClient.Connect(Addr.Host, Addr.RecvPort, Addr.SendPort) then
        begin
          if LogicClient.UserLogin(UserIDEdit.Text, UserPasswdEdit.Text) then
            begin
              if LogicClient.TunnelLink then
                begin
                  UserConfig.SetDefaultValue('LogicLogin', 'id', UserIDEdit.Text);
                  UserConfig.SetDefaultValue('LogicLogin', 'pw', UserPasswdEdit.Text);

                  // login success
                  LastLoginUserID := UserIDEdit.Text;
                  LastLoginUserAlias := LogicClient.GetUserAlias(LastLoginUserID);

                  LogicClient.AutoReconnect := True;
                  AC2ProgressForm.Hide;
                  AC2LogicForm.Show;
                  FogComputeClient.Disconnect;
                  exit;
                end;
            end;
        end;
      AC2ProgressForm.Hide;
      Show;
      if LogicClient.AutoReconnect then
          DoUserLogin;
    end);
end;

procedure TAC2LoginForm.DoUserLogout;
begin
  LogicClient.AutoReconnect := False;
  LogicClient.Disconnect;
end;

procedure TAC2LoginForm.DoUserRegister;
var
  // 用户id,用户密码
  uid, upw: string;
begin
  Hide;
  AC2ProgressForm.Show;
  DoUserLogout;

  QueryClient.QueryMinLoad(ManagerServerHost, TServerType.stLogic, procedure(const State: Boolean; const Addr: TAddressInfo)
    begin
      if not State then
        begin
          AC2ProgressForm.Hide;
          Show;
          exit;
        end;
      if LogicClient.Connect(Addr.Host, Addr.RecvPort, Addr.SendPort) then
        begin
          if LogicClient.RegGuestUser(uid, upw) then
            begin
              UserIDEdit.Text := uid;
              UserPasswdEdit.Text := upw;
            end;

          System.Classes.CheckSynchronize(1000);
          DoUserLogout;
        end;
      AC2ProgressForm.Hide;
      Show;
    end);
end;

procedure TAC2LoginForm.EditChange(Sender: TObject);
begin
  if (umlTrimSpace(UserIDEdit.Text).len = 0) or (umlTrimSpace(UserPasswdEdit.Text).len = 0) then
    begin
      LoginButton.Text := '注册新用户';
    end
  else
    begin
      LoginButton.Text := '登录';
    end;
end;

end.
