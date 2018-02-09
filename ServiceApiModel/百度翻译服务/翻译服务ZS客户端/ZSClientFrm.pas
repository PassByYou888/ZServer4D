{ * 外部api支持，百度翻译服务的客户端接口演示                                  * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ ****************************************************************************** }
unit ZSClientFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CoreClasses,
  PascalStrings, DoStatusIO, DataFrameEngine, NotifyObjectBase,
  CommunicationFramework,
  CommunicationFramework_Client_Indy;

type
  TZSClientForm = class(TForm)
    HostEdit: TLabeledEdit;
    SourMemo: TMemo;
    Label1: TLabel;
    DestMemo: TMemo;
    Label2: TLabel;
    SourComboBox: TComboBox;
    DestComboBox: TComboBox;
    TransButton: TButton;
    LogMemo: TMemo;
    ProgressTimer: TTimer;
    procedure TransButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    cli: TCommunicationFramework_Client_Indy;
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  end;

var
  ZSClientForm: TZSClientForm;

implementation

{$R *.dfm}


procedure HookProgressBackgroundProc;
begin
  Application.ProcessMessages;
end;

procedure TZSClientForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  LogMemo.Lines.Add(AText);
end;

procedure TZSClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  cli := TCommunicationFramework_Client_Indy.Create;
  ProgressBackgroundProc := HookProgressBackgroundProc;
end;

procedure TZSClientForm.ProgressTimerTimer(Sender: TObject);
begin
  cli.ProgressBackground;
end;

procedure TZSClientForm.TransButtonClick(Sender: TObject);
begin
  cli.AsyncConnect(HostEdit.Text, 59813,
    procedure(const cState: Boolean)
    var
      de: TDataFrameEngine;
    begin
      if cState then
        begin
          de := TDataFrameEngine.Create;
          de.WriteByte(SourComboBox.ItemIndex);
          de.WriteByte(DestComboBox.ItemIndex);
          de.WriteString(SourMemo.Text);
          cli.SendStreamCmd('BaiduTranslate', de, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
            begin
              if ResultData.Reader.ReadBool then
                  DestMemo.Text := ResultData.Reader.ReadString
              else
                  DestMemo.Text := '!翻译错误!';

              // 翻译完成后断开链接
              // 我们使用延迟事件引擎触发，0秒表示在该事件结束后的下一个主循环点断开
              // 因为服务器会自动在5秒后断线，并且服务器断线会有个影响过来，我们可以收到断线，而不用心跳检查，所以我们可以不用在这里直接断线
              (*
                cli.ProgressPost.PostExecute(0,
                procedure(Sender: TNPostExecute)
                begin
                cli.Disconnect;
                end);
              *)
            end);
          DisposeObject(de);
        end;
    end);
end;

end.
