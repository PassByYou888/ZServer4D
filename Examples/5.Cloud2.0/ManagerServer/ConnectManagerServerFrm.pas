unit ConnectManagerServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.IOUtils,

  CoreClasses, TextDataEngine, UnicodeMixedLib,
  CommunicationFrameworkDoubleTunnelIO_ServMan, PascalStrings,
  CommonServiceDefine;

type
  TConnectManagerServerForm = class(TForm)
    RegNameEdit: TLabeledEdit;
    RegServerHostEdit: TLabeledEdit;
    ServerManagerHostEdit: TLabeledEdit;
    ConnectButton: TButton;
    CancelButton: TButton;
    ManCliRecvPortEdit: TLabeledEdit;
    ManCliSendPortEdit: TLabeledEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    function ExistsConfig: Boolean;
    procedure LoadConfig;
    procedure SaveConfig;
  end;

function ShowAndConnectManagerServer(AClients: TServerManager_ClientPool; RegRecvPort, RegSendPort: Word; serverType: TServerType): Boolean;

function AutoConnectManagerServer(AClients: TServerManager_ClientPool; ManServAddr, RegAddr: SystemString;
  ManCliRecvPort, ManCliSendPort, RegRecvPort, RegSendPort: Word; serverType: TServerType): Boolean; overload;

function AutoConnectManagerServer(AClients: TServerManager_ClientPool; ManServAddr, RegAddr: SystemString;
  RegRecvPort, RegSendPort: Word; serverType: TServerType): Boolean; overload;

implementation

{$R *.dfm}


function ShowAndConnectManagerServer(AClients: TServerManager_ClientPool; RegRecvPort, RegSendPort: Word; serverType: TServerType): Boolean;
var
  f: TConnectManagerServerForm;
begin
  Result := False;
  f := TConnectManagerServerForm.Create(nil);
  f.RegNameEdit.Text := serverType2Str(serverType);
  f.RegServerHostEdit.Text := '127.0.0.1';
  f.ServerManagerHostEdit.Text := '127.0.0.1';
  f.ManCliRecvPortEdit.Text := IntToStr(cManagerService_SendPort);
  f.ManCliSendPortEdit.Text := IntToStr(cManagerService_RecvPort);
  if f.ShowModal = mrOk then
    begin
      Result := AClients.BuildClientAndConnect(f.RegNameEdit.Text,
        f.ServerManagerHostEdit.Text, f.RegServerHostEdit.Text,
        umlStrToInt(f.ManCliRecvPortEdit.Text, cManagerService_SendPort),
        umlStrToInt(f.ManCliSendPortEdit.Text, cManagerService_RecvPort),
        RegRecvPort, RegSendPort, serverType);
    end;
  DisposeObject(f);
end;

function AutoConnectManagerServer(AClients: TServerManager_ClientPool; ManServAddr, RegAddr: SystemString;
  ManCliRecvPort, ManCliSendPort, RegRecvPort, RegSendPort: Word; serverType: TServerType): Boolean;
var
  f: TConnectManagerServerForm;
begin
  f := TConnectManagerServerForm.Create(nil);
  f.RegNameEdit.Text := serverType2Str(serverType);
  f.RegServerHostEdit.Text := RegAddr;
  f.ServerManagerHostEdit.Text := ManServAddr;
  f.ManCliRecvPortEdit.Text := IntToStr(ManCliRecvPort);
  f.ManCliSendPortEdit.Text := IntToStr(ManCliSendPort);

  Result := AClients.BuildClientAndConnect(f.RegNameEdit.Text,
    f.ServerManagerHostEdit.Text, f.RegServerHostEdit.Text,
        umlStrToInt(f.ManCliRecvPortEdit.Text, cManagerService_SendPort),
        umlStrToInt(f.ManCliSendPortEdit.Text, cManagerService_RecvPort),
    RegRecvPort, RegSendPort, serverType);

  if Result then
      f.SaveConfig;

  DisposeObject(f);
end;

function AutoConnectManagerServer(AClients: TServerManager_ClientPool; ManServAddr, RegAddr: SystemString;
  RegRecvPort, RegSendPort: Word; serverType: TServerType): Boolean;
var
  f: TConnectManagerServerForm;
begin
  f := TConnectManagerServerForm.Create(nil);
  f.RegNameEdit.Text := serverType2Str(serverType);
  f.RegServerHostEdit.Text := RegAddr;
  f.ServerManagerHostEdit.Text := ManServAddr;
  f.ManCliRecvPortEdit.Text := IntToStr(cManagerService_SendPort);
  f.ManCliSendPortEdit.Text := IntToStr(cManagerService_RecvPort);

  Result := AClients.BuildClientAndConnect(f.RegNameEdit.Text,
    f.ServerManagerHostEdit.Text, f.RegServerHostEdit.Text,
    umlStrToInt(f.ManCliRecvPortEdit.Text, cManagerService_SendPort),
    umlStrToInt(f.ManCliSendPortEdit.Text, cManagerService_RecvPort),
    RegRecvPort, RegSendPort, serverType);

  if Result then
      f.SaveConfig;

  DisposeObject(f);
end;

procedure TConnectManagerServerForm.FormShow(Sender: TObject);
begin
  LoadConfig;
end;

procedure TConnectManagerServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveConfig;
  Action := caHide;
end;

procedure TConnectManagerServerForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: ModalResult := mrCancel;
  end;
end;

function TConnectManagerServerForm.ExistsConfig: Boolean;
var
  n: SystemString;
begin
  n := umlChangeFileExt(Application.Exename, '') + '_connectInfo.ini';

  Result := umlFileExists(n);
end;

procedure TConnectManagerServerForm.LoadConfig;
var
  te: TSectionTextData;
  n : SystemString;
begin
  n := umlChangeFileExt(Application.Exename, '') + '_connectInfo.ini';

  if umlFileExists(n) then
    begin
      te := TSectionTextData.Create;
      te.LoadFromFile(n);
      RegNameEdit.Text := te.GetDefaultValue('reg', RegNameEdit.Name, RegNameEdit.Text);
      RegServerHostEdit.Text := te.GetDefaultValue('reg', RegServerHostEdit.Name, RegServerHostEdit.Text);
      ServerManagerHostEdit.Text := te.GetDefaultValue('reg', ServerManagerHostEdit.Name, ServerManagerHostEdit.Text);
      ManCliRecvPortEdit.Text := te.GetDefaultValue('reg', ManCliRecvPortEdit.Name, ManCliRecvPortEdit.Text);
      ManCliSendPortEdit.Text := te.GetDefaultValue('reg', ManCliSendPortEdit.Name, ManCliSendPortEdit.Text);
      DisposeObject(te);
    end;
end;

procedure TConnectManagerServerForm.SaveConfig;
var
  te: TSectionTextData;
  n : SystemString;
begin
  n := umlChangeFileExt(Application.Exename, '') + '_connectInfo.ini';

  te := TSectionTextData.Create;

  te.SetDefaultValue('reg', RegNameEdit.Name, RegNameEdit.Text);
  te.SetDefaultValue('reg', RegServerHostEdit.Name, RegServerHostEdit.Text);
  te.SetDefaultValue('reg', ServerManagerHostEdit.Name, ServerManagerHostEdit.Text);
  te.SetDefaultValue('reg', ManCliRecvPortEdit.Name, ManCliRecvPortEdit.Text);
  te.SetDefaultValue('reg', ManCliSendPortEdit.Name, ManCliSendPortEdit.Text);

  te.SaveToFile(n);
  DisposeObject(te);
end;

end.
