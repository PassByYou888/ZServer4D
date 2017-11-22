unit ConnectManagerServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.IOUtils,

  ManagerServer_ClientIntf, CoreClasses, TextDataEngine, UnicodeMixedLib;

type
  TConnectManagerServerForm = class(TForm)
    RegNameEdit: TLabeledEdit;
    RegServerHostEdit: TLabeledEdit;
    ManagerServerHostEdit: TLabeledEdit;
    ConnectButton: TButton;
    CancelButton: TButton;
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

function ShowAndConnectManagerServer(AClients: TManagerClients; RegRecvPort, RegSendPort: Word; serverType: Byte): Boolean;

function AutoConnectManagerServer(AClients: TManagerClients;
  ManServAddr, RegAddr: string; RegRecvPort, RegSendPort: Word; serverType: Byte): Boolean;

implementation

{$R *.dfm}


function ShowAndConnectManagerServer(AClients: TManagerClients; RegRecvPort, RegSendPort: Word; serverType: Byte): Boolean;
var
  f: TConnectManagerServerForm;
begin
  Result := False;
  f := TConnectManagerServerForm.Create(nil);
  f.RegNameEdit.Text := serverType2Str(serverType);
  f.RegServerHostEdit.Text := '127.0.0.1';
  f.ManagerServerHostEdit.Text := '127.0.0.1';
  if f.ShowModal = mrOk then
    begin
      Result := AClients.BuildClientAndConnect(f.RegNameEdit.Text, f.ManagerServerHostEdit.Text, f.RegServerHostEdit.Text, RegRecvPort, RegSendPort, serverType);
    end;
  DisposeObject(f);
end;

function AutoConnectManagerServer(AClients: TManagerClients; ManServAddr, RegAddr: string; RegRecvPort, RegSendPort: Word; serverType: Byte): Boolean;
var
  f: TConnectManagerServerForm;
begin
  Result := False;
  f := TConnectManagerServerForm.Create(nil);
  f.RegNameEdit.Text := serverType2Str(serverType);
  f.RegServerHostEdit.Text := RegAddr;
  f.ManagerServerHostEdit.Text := ManServAddr;

  Result := AClients.BuildClientAndConnect(f.RegNameEdit.Text, f.ManagerServerHostEdit.Text, f.RegServerHostEdit.Text, RegRecvPort, RegSendPort, serverType);
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
  n: string;
begin
  n := umlChangeFileExt(Application.Exename, '') + '_connectInfo.ini';

  Result := umlFileExists(n);
end;

procedure TConnectManagerServerForm.LoadConfig;
var
  te: TSectionTextData;
  n : string;
begin
  n := umlChangeFileExt(Application.Exename, '') + '_connectInfo.ini';

  if umlFileExists(n) then
    begin
      te := TSectionTextData.Create;
      te.LoadFromFile(n);
      RegNameEdit.Text := te.GetDefaultValue('reg', RegNameEdit.Name, RegNameEdit.Text);
      RegServerHostEdit.Text := te.GetDefaultValue('reg', RegServerHostEdit.Name, RegServerHostEdit.Text);
      ManagerServerHostEdit.Text := te.GetDefaultValue('reg', ManagerServerHostEdit.Name, ManagerServerHostEdit.Text);
      DisposeObject(te);
    end;
end;

procedure TConnectManagerServerForm.SaveConfig;
var
  te: TSectionTextData;
  n : string;
begin
  n := umlChangeFileExt(Application.Exename, '') + '_connectInfo.ini';

  te := TSectionTextData.Create;

  te.SetDefaultValue('reg', RegNameEdit.Name, RegNameEdit.Text);
  te.SetDefaultValue('reg', RegServerHostEdit.Name, RegServerHostEdit.Text);
  te.SetDefaultValue('reg', ManagerServerHostEdit.Name, ManagerServerHostEdit.Text);

  te.SaveToFile(n);
  DisposeObject(te);
end;

end.
