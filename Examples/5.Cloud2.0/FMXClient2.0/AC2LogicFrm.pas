unit AC2LogicFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Cadencer,
  Geometry2DUnit, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  CoreClasses, MemoryStream64,
  DoStatusIO, ListEngine, UnicodeMixedLib,
  FMX.Objects, FMX.Ani, FMX.ListBox, AC2LogicMobileClient,
  CommunicationFramework, DataFrameEngine,
  CommunicationFrameworkDoubleTunnelIO_ServMan,
  AC2ManagerServerMobileClient, FMX.Edit, FMX.ScrollBox, FMX.Memo;

type
  TAC2LogicForm = class(TForm, ILogicBackCallInterface)
    MainLayout: TLayout;
    ExpEdit: TEdit;
    computeButton: TButton;
    ValueEdit: TEdit;
    AntiIdleCheckBox: TCheckBox;
    AntiIdleTimer: TTimer;
    LogicFileInfoMemo: TMemo;
    GetLogicFileInfoButton: TButton;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure computeButtonClick(Sender: TObject);
    procedure AntiIdleCheckBoxChange(Sender: TObject);
    procedure AntiIdleTimerTimer(Sender: TObject);
    procedure GetLogicFileInfoButtonClick(Sender: TObject);
  private
    { hall interface }
    procedure LogicDisconnect;
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    procedure Progress(deltaTime: Double);
  end;

var
  AC2LogicForm: TAC2LogicForm;

implementation

{$R *.fmx}


uses AC2LoginFrm, AC2ClientGlobal, CommonServiceDefine, AC2ProgressFrm, FOGComputeClientIntf;

procedure TAC2LogicForm.FormResize(Sender: TObject);
begin
  ResetMainLayout(MainLayout, self);
end;

procedure TAC2LogicForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  ResetFormSize(self);
  ResetMainLayout(MainLayout, self);
end;

procedure TAC2LogicForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TAC2LogicForm.FormShow(Sender: TObject);
begin
  ResetFormSize(self);
  ReStartAnimation(MainLayout);
end;

procedure TAC2LogicForm.GetLogicFileInfoButtonClick(Sender: TObject);
begin
  LogicFileInfoMemo.Lines.Clear;

  LogicClient.GetLogicFileList(procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      n: string;
    begin
      while ResultData.Reader.NotEnd do
        begin
          n := ResultData.Reader.ReadString;
          LogicClient.GetLogicFile(n, procedure(filename: string; fileSour: TMemoryStream64)
            var
              md5: TMD5;
            begin
              LogicFileInfoMemo.Lines.Add(Format('业务文件:%s 大小:%s', [filename, umlSizeToStr(fileSour.size).Text]));
              LogicFileInfoMemo.GoToTextEnd;
            end);
        end;
    end);

  LogicClient.GetAdvertisementFileList(procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      n: string;
    begin
      while ResultData.Reader.NotEnd do
        begin
          n := ResultData.Reader.ReadString;
          LogicClient.GetAdvertisementFile(n, procedure(filename: string; fileSour: TMemoryStream64)
            var
              md5: TMD5;
            begin
              LogicFileInfoMemo.Lines.Add(Format('广告版文件:%s 大小:%s', [filename, umlSizeToStr(fileSour.size).Text]));
              LogicFileInfoMemo.GoToTextEnd;
            end);
        end;
    end);
end;

procedure TAC2LogicForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
  AC2LoginForm.DoUserLogout;
  AC2LoginForm.Show;
end;

procedure TAC2LogicForm.LogicDisconnect;
begin
  LogicClient.AutoReconnect := True;
  AC2LoginForm.DoUserLogin;
end;

procedure TAC2LogicForm.AntiIdleCheckBoxChange(Sender: TObject);
begin
  AntiIdleTimer.Enabled := AntiIdleCheckBox.IsChecked;
end;

procedure TAC2LogicForm.AntiIdleTimerTimer(Sender: TObject);
begin
  LogicClient.AntiIdle;
end;

procedure TAC2LogicForm.computeButtonClick(Sender: TObject);
begin
  QueryClient.Query(ManagerServerHost, TServerType.stFOGCompute, procedure(const State: Boolean; const Addr: TAddressInfo)
    begin
      if not State then
        begin
          AC2ProgressForm.Hide;
          Show;
          exit;
        end;
      if FogComputeClient.Connect(Addr.Host, Addr.RecvPort, Addr.SendPort) and FogComputeClient.TunnelLink then
        begin
          ValueEdit.Text := '';
          ValueEdit.TextPrompt := '远程雾服务器正在计算中...';
          FogComputeClient.SimulateCompute5Sec(ExpEdit.Text, procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
            begin
              ValueEdit.Text := ResultData.Reader.ReadString;
            end);
        end;
    end);
end;

procedure TAC2LogicForm.DoStatusNear(AText: string; const ID: Integer);
begin
  // DrawPool(self, DrawIntf).PostScrollText(10, AText, 10, DEColor(1, 1, 1, 1));
end;

procedure TAC2LogicForm.Progress(deltaTime: Double);
begin
  if Visible then
      Invalidate;
end;

end.
