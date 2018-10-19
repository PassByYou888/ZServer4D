unit FMXServFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  CommunicationFramework_Server_Indy,
  DataFrameEngine,
  CommunicationFramework, CoreClasses, DoStatusIO, MemoryStream64,
  UnicodeMixedLib;

type
  TFMXClientForm = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    StartServiceButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StartServiceButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    tempStream: TMemoryStream64;

    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
    procedure cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);

    procedure cmd_TestMiniStream(Sender: TPeerClient; InData: TDataFrameEngine);

    procedure cmd_Test128MBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
  public
    { Public declarations }
    server: TCommunicationFramework_Server_Indy;
  end;

var
  FMXClientForm: TFMXClientForm;

implementation

{$R *.fmx}

{ TFMXClientForm }

procedure TFMXClientForm.cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
begin
  DoStatus('client: %s', [InData]);
end;

procedure TFMXClientForm.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TFMXClientForm.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  OutData.WriteString('result 654321');
end;

procedure TFMXClientForm.cmd_Test128MBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  tempStream.CopyFrom(InData, InData.Size);

  // bigstream complete
  if tempStream.Size = BigStreamTotal then
    begin
      Sender.Print('bigsteram finish');
      Sender.Print('bigsteram md5:' + umlMD5Char(tempStream.Memory, tempStream.Size).Text);
      tempStream.Clear;
    end;
end;

procedure TFMXClientForm.cmd_TestMiniStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  InData.Reader.ReadStream(ms);

  DoStatus(umlMD5Char(ms.Memory, ms.Size).Text);

  DisposeObject(ms);
end;

procedure TFMXClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TFMXClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  server := TCommunicationFramework_Server_Indy.Create;

  server.RegisterDirectConsole('helloWorld_Console').OnExecute := cmd_helloWorld_Console;
  server.RegisterDirectStream('helloWorld_Stream').OnExecute := cmd_helloWorld_Stream;
  server.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;

  server.RegisterDirectStream('TestMiniStream').OnExecute := cmd_TestMiniStream;
  server.RegisterBigStream('Test128MBigStream').OnExecute := cmd_Test128MBigStream;

  tempStream := TMemoryStream64.Create;
end;

procedure TFMXClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(tempStream);
  DeleteDoStatusHook(self);
  DisposeObject(server);
end;

procedure TFMXClientForm.StartServiceButtonClick(Sender: TObject);
begin
  if server.StartService('0.0.0.0', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!')
end;

procedure TFMXClientForm.Timer1Timer(Sender: TObject);
begin
  server.Progress;
end;

end.
