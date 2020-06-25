unit DatasetOnFMXMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  CoreClasses, ZDBEngine, ZDBLocalManager, DoStatusIO, DataFrameEngine, PascalStrings,
  ListEngine, UnicodeMixedLib, MemoryStream64, zExpression, OpCode,
  CommunicationFramework, CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CommunicationFramework_Client_CrossSocket;

type
  TMyQueryClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  public type
    TQueryMyDatabaseResultProc = reference to procedure(QueryResultCount: Integer; PipelineName: SystemString; dataSource: TMemoryStream64);

    PQueryRunStruct = ^TQueryRunStruct;

    TQueryRunStruct = record
      QueryResultCount: Integer;
      PipelineName: SystemString;
      OnResult: TQueryMyDatabaseResultProc;
    end;
  private
    procedure cmd_QueryDone(Sender: TPeerIO; InData: TDataFrameEngine);
  public
    procedure QueryMyDatabase(sql: SystemString; OnResult: TQueryMyDatabaseResultProc);
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TForm1 = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    HostEdit: TEdit;
    connectButton: TButton;
    Memo1: TMemo;
    Layout2: TLayout;
    Label2: TLabel;
    ExpEdit: TEdit;
    QueryButton: TButton;
    DisconnectButton: TButton;
    Timer1: TTimer;
    procedure connectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure QueryButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    myQueryClient: TMyQueryClient;
    procedure backcall_DoStatus(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TMyQueryClient.cmd_QueryDone(Sender: TPeerIO; InData: TDataFrameEngine);
var
  p: PQueryRunStruct;
begin
  p := PQueryRunStruct(InData.Reader.ReadPointer);
  p^.QueryResultCount := InData.Reader.ReadInteger;
  p^.PipelineName := InData.Reader.ReadString;

  if Assigned(p^.OnResult) then
      p^.OnResult(p^.QueryResultCount, p^.PipelineName, Sender.UserDefine.BigStreamBatchList.Last^.Source);
  Dispose(p);
end;

procedure TMyQueryClient.QueryMyDatabase(sql: SystemString; OnResult: TQueryMyDatabaseResultProc);
var
  p: PQueryRunStruct;
  de: TDataFrameEngine;
begin
  new(p);
  p^.QueryResultCount := -1;
  p^.PipelineName := '';
  p^.OnResult := OnResult;

  de := TDataFrameEngine.Create;
  de.WritePointer(p);
  de.WriteString(sql);
  SendTunnel.SendDirectStreamCmd('QueryMyDatabase', de);
  disposeObject(de);
end;

procedure TMyQueryClient.RegisterCommand;
begin
  inherited;
  FRecvTunnel.RegisterDirectStream('QueryDone').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_QueryDone;
end;

procedure TMyQueryClient.UnRegisterCommand;
begin
  inherited;
  FRecvTunnel.DeleteRegistedCMD('QueryDone');
end;

procedure TForm1.backcall_DoStatus(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm1.connectButtonClick(Sender: TObject);
begin
  myQueryClient.AsyncConnectP(HostEdit.Text, 10992, 10991, procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('connect ok');
          myQueryClient.TunnelLinkP(procedure(const lState: Boolean)
            begin
              if lState then
                  DoStatus('tunnel link ok');
            end);
        end
      else
          DoStatus('connect error');
    end);
end;

procedure TForm1.DisconnectButtonClick(Sender: TObject);
begin
  myQueryClient.Disconnect;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, backcall_DoStatus);
  myQueryClient := TMyQueryClient.Create(
    TCommunicationFramework_Client_CrossSocket.Create.StableIO,
    TCommunicationFramework_Client_CrossSocket.Create.StableIO);
  myQueryClient.RegisterCommand;
end;

procedure TForm1.QueryButtonClick(Sender: TObject);
begin
  myQueryClient.QueryMyDatabase(ExpEdit.Text, procedure(QueryResultCount: Integer; PipelineName: SystemString; dataSource: TMemoryStream64)
    var
      dbeng: TDBStore;
    begin
      DoStatus('"%s" Result: %d', [PipelineName, QueryResultCount]);
      dbeng := TDBStore.CreateNewMemory;
      dataSource.Position := 0;
      dbeng.LoadFromStream(dataSource);
      dbeng.WaitQueryP(
        procedure(var qState: TQueryState)
        begin
          DoStatus(qState.eng.VT[qState.StorePos].AsText);
        end);
      disposeObject(dbeng);
    end);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  myQueryClient.Progress;
end;

end.
