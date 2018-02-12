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
  PascalStrings, UnicodeMixedLib, DoStatusIO, DataFrameEngine, NotifyObjectBase,
  CommunicationFramework, BaiduTranslateClient;

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
    UsedCacheCheckBox: TCheckBox;
    UpdateTranslateButton: TButton;
    procedure TransButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateTranslateButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  end;

var
  ZSClientForm: TZSClientForm;

implementation

{$R *.dfm}


procedure HookProgressBackgroundProc;
begin
  // Application.ProcessMessages;
end;

procedure TZSClientForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  LogMemo.Lines.Add(AText);
end;

procedure TZSClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);

  // 通讯引擎的主循环钩子
  ProgressBackgroundProc := HookProgressBackgroundProc;
end;

procedure TZSClientForm.TransButtonClick(Sender: TObject);
var
  sour: TPascalString;
begin
  BaiduTranslateServiceHost := HostEdit.Text;

  sour := SourMemo.Text;
  while (sour.Len > 0) and (CharIn(sour.Last, [#13, #10])) do
      sour.DeleteLast;

  BaiduTranslate(False, UsedCacheCheckBox.Checked, SourComboBox.ItemIndex, DestComboBox.ItemIndex, sour, nil,
    procedure(UserData: Pointer; Success, Cached: Boolean; TranslateTime: TTimeTick; sour, dest: TPascalString)
    begin
      DestMemo.Text := dest.Text;
    end);
end;

procedure TZSClientForm.UpdateTranslateButtonClick(Sender: TObject);
var
  sour, dest: TPascalString;
begin
  BaiduTranslateServiceHost := HostEdit.Text;

  sour := SourMemo.Text;
  while (sour.Len > 0) and (CharIn(sour.Last, [#13, #10])) do
      sour.DeleteLast;

  dest := DestMemo.Text;
  while (dest.Len > 0) and (CharIn(dest.Last, [#13, #10])) do
      dest.DeleteLast;

  UpdateTranslate(SourComboBox.ItemIndex, DestComboBox.ItemIndex, sour, dest);
end;

end.
