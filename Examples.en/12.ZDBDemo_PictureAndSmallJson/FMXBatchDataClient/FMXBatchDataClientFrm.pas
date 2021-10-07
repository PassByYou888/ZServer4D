unit FMXBatchDataClientFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation,
  FMX.DialogService, FMX.Layouts,
  NotifyObjectBase,
  CommunicationFrameworkDataStoreService, ZDBEngine,
  ZDBLocalManager, CommunicationFramework_Client_Indy,
  CommunicationFramework, CoreClasses, DoStatusIO, FMX.ScrollBox, FMX.Memo,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, PascalStrings, MemoryStream64, UnicodeMixedLib,
  CommunicationFrameworkDataStoreService_VirtualAuth,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth, FileBuffOfCode,
  FMX.ListBox, FMX.Objects, DataFrameEngine, ZS_JsonDataObjects,
  FMX.Memo.Types;

type
  TMyDataStoreClient = class(TDataStoreClient_VirtualAuth)
  protected
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); override;

  end;

  TFMXBatchDataClientForm = class(TForm)
    TabControl: TTabControl;
    LoginTabItem: TTabItem;
    Layout1: TLayout;
    Layout2: TLayout;
    Label1: TLabel;
    UserIDEdit: TEdit;
    Layout3: TLayout;
    Label2: TLabel;
    PasswdEdit: TEdit;
    LoginBtn: TButton;
    Layout4: TLayout;
    Label3: TLabel;
    ServerEdit: TEdit;
    Timer1: TTimer;
    StatusMemo: TMemo;
    OfflineTabItem: TTabItem;
    Layout5: TLayout;
    DisconnectButton: TButton;
    DBOperationDataTabItem: TTabItem;
    Gen1JsonButton: TButton;
    DisconnectCheckTimer: TTimer;
    Layout6: TLayout;
    Label4: TLabel;
    JsonDestDBEdit: TEdit;
    ResultTabItem: TTabItem;
    QueryJsonButton: TButton;
    Layout7: TLayout;
    Label5: TLabel;
    JsonKeyEdit: TEdit;
    Layout8: TLayout;
    Label6: TLabel;
    JsonValueEdit: TEdit;
    ResetJsonDBButton: TButton;
    AnalysisJsonButton: TButton;
    Layout9: TLayout;
    Label7: TLabel;
    AnalysisDestDBEdit: TEdit;
    PictureListBox: TListBox;
    Layout10: TLayout;
    Label8: TLabel;
    ResultListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoginBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure DisconnectCheckTimerTimer(Sender: TObject);
    procedure Gen1JsonButtonClick(Sender: TObject);
    procedure QueryJsonButtonClick(Sender: TObject);
    procedure ResetJsonDBButtonClick(Sender: TObject);
    procedure AnalysisJsonButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    RecvTunnel, SendTunnel: TCommunicationFrameworkClient;
    DBClient: TMyDataStoreClient;
    procedure DoStatusNear(AText: string; const ID: Integer);
  end;

var
  FMXBatchDataClientForm: TFMXBatchDataClientForm;

implementation

{$R *.fmx}


procedure TMyDataStoreClient.ClientDisconnect(Sender: TCommunicationFrameworkClient);
begin
  FMXBatchDataClientForm.TabControl.ActiveTab := FMXBatchDataClientForm.LoginTabItem;
  inherited;
end;

procedure TFMXBatchDataClientForm.AnalysisJsonButtonClick(Sender: TObject);
var
  vl: TDBEngineVL; {  Tdbenginevl is a key value data structure prototype  }
begin
  vl := TDBEngineVL.Create;
  vl['Key'] := 'RandomValue';
  vl['Value'] := 1; {  The value we want to count is 1  }

  {  Statistics and analysis are triggered using the mycustomanalysis filter registered on the server side  }
  {  When statistics and analysis are carried out on the server side, depth matching, image similarity, text similarity, voice similarity and so on can be carried out. They will all work in the parallel platform. Of course, you also need the support of corresponding algorithm modules  }
  {  Statistics and analysis can be performed statically on the server (no fragment buffer feedback, which plays an obvious role in statistics of large databases)  }
  {  After the server completes the statistics and analysis quietly, we perform the step to step operation through events. ZDB is fully compatible with and supports the anonymous function mechanism  }
  {  Does it feel the same as a single machine?  }
  DBClient.QueryDBP(
    'MyCustomAnalysis',      {  Mycustomanalysis is registered and implemented on the server  }
    False,                   {  Whether the buffer fragments are synchronized to the client or not, because our statistics pursue the results. There is no need to synchronize here. Let the server do it. We only need to specify what to do after the statistics are completed in the completion event  }
    True,                    {  Whether to write the query results to the output database. This output is equivalent to selecting to the view, but the output will copy  }
    False,                   {  The output data is an in memory database. If false, the output of the query will be stored as an entity file  }
    False,                   {  Whether to reverse query, starting from the last  }
    JsonDestDBEdit.Text,     {  Database name of the query  }
    AnalysisDestDBEdit.Text, {  Output name of Statistics  }
    1.0,                     {  Fragment buffer time. Because the query is too frequent, the bottom layer of ZDB will cache and compress the query results within this time, and then send them. 0 is immediate feedback  }
    0,                       {  The maximum waiting query time, 0 is infinite  }
    0,                       {  Maximum number of feedback entries for matching query, 0 is infinite  }
    vl,                      {  KeyValue parameter sent to mycustomquery  }
    nil,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      ResultListBox.Clear;
      {  This event is triggered when the server query is completed  }
      DoStatus('Statistics of %s completed. There are %d outputs in the database %s', [dbN, TotalResult, outN]);
      {  After the statistics are completed, a permanent file database will be output  }
      {  During the change event, we can repeatedly make statistics on the change database and query again to get the results we need  }
      {  However, there is no need to make multiple queries here. The statistical results are directly downloaded to the local and displayed  }
      if TotalResult > 0 then
          DBClient.DownloadDBP(False, outN,
          procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
          var
            js: TJsonObject;
            litm: TListBoxItem;
          begin
            {  After the server finds the result, here is the data feedback  }
            {  The data of this event is temporary and will be killed after the call. If you want to temporarily store the query result data, you must do it here  }
            js := TJsonObject.Create;
            js.LoadFromStream(DataSour);
            litm := TListBoxItem.Create(ResultListBox);
            litm.Parent := ResultListBox;
            litm.Text := js.ToString;
            litm.Selectable := False;
            ResultListBox.AddObject(litm);
            DisposeObject(js);
          end,
          procedure(dbN, outN, pipeN: string; TotalResult: Int64)
          begin
            {  Because the statistical results are downloaded, the server no longer needs this statistical database. Now, we will delete it  }
            {  Note: if the statistics library is being accessed by a pipeline, the deletion here will make an error  }
            {  To avoid this problem, we only need to ensure that the file database of statistical output is unique  }
            DBClient.CloseDB(dbN, True);

            DoStatus('Statistics result %s download completed, %d in total', [dbN, TotalResult]);
            TabControl.ActiveTab := ResultTabItem;
          end);
    end);

  DisposeObject(vl);
end;

procedure TFMXBatchDataClientForm.DisconnectButtonClick(Sender: TObject);
begin
  DBClient.Disconnect;
end;

procedure TFMXBatchDataClientForm.DisconnectCheckTimerTimer(
  Sender: TObject);
begin
  {  Due to cross platform problems, Indy does not support disconnection events at the bottom of IOS and Android platforms  }
  {  The disconnection status must be checked manually  }
  {  When the connection is successful, we activate a timer to cycle to check for disconnection  }
  if not DBClient.Connected then
    begin
      DBClient.RecvTunnel.TriggerDoDisconnect;
      DisconnectCheckTimer.Enabled := False;
    end;
end;

procedure TFMXBatchDataClientForm.DoStatusNear(AText: string;
const ID: Integer);
begin
  StatusMemo.Lines.Add(AText);
  StatusMemo.GoToTextEnd;
end;

procedure TFMXBatchDataClientForm.FormCreate(Sender: TObject);
type
  TGetStreamProc = procedure(Output: TStream);
  procedure RegisterFileStream(MD5Text: string; OnProc: TGetStreamProc; FileName: string);
  var
    m: TMemoryStream64;
    img: TImage;
    litm: TListBoxItem;
  begin
    m := TMemoryStream64.Create;
    OnProc(m);

    litm := TListBoxItem.Create(PictureListBox);
    litm.Parent := PictureListBox;
    litm.Selectable := True;
    img := TImage.Create(litm);
    img.Parent := litm;
    img.HitTest := False;
    m.Position := 0;
    img.Bitmap.LoadFromStream(m);
    img.Align := TAlignLayout.Client;
    litm.TagObject := img;
    PictureListBox.AddObject(litm);
    DisposeObject(m);
  end;

begin
  AddDoStatusHook(self, DoStatusNear);
  RecvTunnel := TCommunicationFramework_Client_Indy.Create;
  SendTunnel := TCommunicationFramework_Client_Indy.Create;
  DBClient := TMyDataStoreClient.Create(RecvTunnel, SendTunnel);
  DBClient.RegisterCommand;

  RecvTunnel.QuietMode := True;
  SendTunnel.QuietMode := True;

  RegisterFileStream('c81c2ef1794dfa4863e6ed5752201313', Get_Chrysanthemum_Stream, 'Chrysanthemum.jpg');
  RegisterFileStream('e805490727905eada15ca44916412449', Get_Desert_Stream, 'Desert.jpg');
  RegisterFileStream('7697f1b7e9cac01203bb56eb83c9dc83', Get_Hydrangeas_Stream, 'Hydrangeas.jpg');
  RegisterFileStream('e24ba2b3e84bd20cb4ecf1e8947b82bc', Get_Jellyfish_Stream, 'Jellyfish.jpg');
  RegisterFileStream('cef583eeb89487665ac09dd963787546', Get_Koala_Stream, 'Koala.jpg');
  RegisterFileStream('7bfc6de65b3020ed89c849020527bcfd', Get_Lighthouse_Stream, 'Lighthouse.jpg');
  RegisterFileStream('0223d1c7652587fbb7b3eeace6dbd5c6', Get_Penguins_Stream, 'Penguins.jpg');
  RegisterFileStream('c97ece645bdf4973adf6a645a5121da0', Get_Tulips_Stream, 'Tulips.jpg');

  PictureListBox.ListItems[0].IsSelected := True;
end;

procedure TFMXBatchDataClientForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
  DisposeObject([DBClient, RecvTunnel, SendTunnel]);
end;

procedure TFMXBatchDataClientForm.Gen1JsonButtonClick(Sender: TObject);
var
  i: Integer;
  img: TImage;
  m: TMemoryStream64;
begin
  if PictureListBox.Selected = nil then
    begin
      TDialogService.ShowMessage('You must select a picture');
      exit;
    end;

  img := nil;
  for i := 0 to PictureListBox.Count - 1 do
    if PictureListBox.ListItems[i].IsSelected then
      begin
        img := PictureListBox.ListItems[i].TagObject as TImage;
        break;
      end;

  if img = nil then
    begin
      TDialogService.ShowMessage('You must select a picture');
      exit;
    end;
  m := TMemoryStream64.Create;
  img.Bitmap.SaveToStream(m);
  m.Position := 0;
  DoStatus('post size:%d md5:%s', [m.Size, umlMD5Char(m.Memory, m.Size).Text]);
  {  The first parameter of initdb is the memory database. We set it to false to create a file database  }
  DBClient.InitDB(False, JsonDestDBEdit.Text);
  {  111 is our custom picture ID  }
  {  Because the query only indexes C_ The background query will jump over Jason's ID, so we can directly submit img to the same table  }
  DBClient.BeginAssembleStream;                                   {  The function of beginassemblestream is to empty the staging buffer of the batch submitted stream to the data server  }
  DBClient.PostAssembleStream(JsonDestDBEdit.Text, m, 111, True); {  Submit the stream to the data server immediately  }
  {  Obtain the database storage information of the last submitted img from the remote database  }
  DBClient.GetBatchStreamStateP(
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      bpInfo: TBigStreamBatchPostData;
      df: TDataFrameEngine;
      j: TDBEngineJson;
    begin
      if ResultData.Count > 0 then
        begin
          {  If the batch stream submitted by us has temporary data in the database, many statuses will be obtained here. We take the last batch stream status and decode it  }
          df := TDataFrameEngine.Create;
          ResultData.ReadDataFrame(ResultData.Count - 1, df);
          bpInfo.Decode(df);
          DisposeObject(df);
          {  Generate a JSON object to the entity file library  }
          j := TDBEngineJson.Create;
          j.S['myKey'] := '1';                  {  This is equivalent to our daily data  }
          j.L['StorePos'] := bpInfo.DBStorePos; {  Here is the storepos we submitted img in the database this time. Here is the variable type of Int64  }
          {  Randomvalue is used to demonstrate statistics and analysis capabilities  }
          j.i['RandomValue'] := 1;
          DBClient.PostAssembleStream(JsonDestDBEdit.Text, j);
          DisposeObject(j);
          DBClient.EndAssembleStream; {  Endassemblystream is used to empty the staging buffer of the batch submitted stream to the data server  }
        end;
    end);
end;

procedure TFMXBatchDataClientForm.LoginBtnClick(Sender: TObject);
begin
  if not SendTunnel.Connect(ServerEdit.Text, 10099) then
      exit;
  if not RecvTunnel.Connect(ServerEdit.Text, 10098) then
      exit;

  DBClient.UserLoginP(UserIDEdit.Text, PasswdEdit.Text,
    procedure(const State: Boolean)
    begin
      if State then
        begin
          DoStatus('Login succeeded');
          DBClient.TunnelLinkP(
            procedure(const State: Boolean)
            begin
              if State then
                begin
                  DoStatus('Dual channel link succeeded');
                  TabControl.ActiveTab := DBOperationDataTabItem;

                  {  Due to cross platform problems, Indy does not support disconnection events at the bottom of IOS and Android platforms  }
                  {  The disconnection status must be checked manually  }
                  {  When the connection is successful, we activate a timer to cycle to check for disconnection  }
                  DisconnectCheckTimer.Enabled := True;
                  DBClient.ProgressEngine.PostExecuteP(1, procedure(Sender: TNPostExecute)
                    begin
                      while not DBClient.DataCipherKeyFinished do
                          DBClient.Progress;
                    end)
                end;
            end);
        end;
    end);
end;

procedure TFMXBatchDataClientForm.QueryJsonButtonClick(Sender: TObject);
var
  vl: TDBEngineVL; {  Tdbenginevl is a key value data structure prototype  }
begin
  vl := TDBEngineVL.Create;
  vl['Key'] := JsonKeyEdit.Text;
  vl['Value'] := JsonValueEdit.Text;

  ResultListBox.Clear;
  //
  DBClient.QueryDBP('MyCustomQuery', {  Registration and implementation of mycustomquery in server  }
  True,                              {  Whether the buffer fragments are synchronized to the client  }
  False,                             {  Whether to write the query results to the output database. This output is equivalent to selecting to the view, but the output will copy  }
  True,                              {  The output data is an in memory database. If false, the output of the query will be stored as an entity file  }
  False,                             {  Whether to reverse query, starting from the last  }
  JsonDestDBEdit.Text,               {  Database name of the query  }
  '',                                {  The output name of the query, because we do not write output, but also temporary memory, can be ignored here  }
  1.0,                               {  Fragment buffer time. Because the query is too frequent, the bottom layer of ZDB will cache and compress the query results within this time, and then send them. 0 is immediate feedback  }
  0,                                 {  The maximum waiting query time, 0 is infinite  }
  0,                                 {  Maximum number of feedback entries for matching query, 0 is infinite  }
  vl,                                {  KeyValue parameter sent to mycustomquery  }
    procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
    var
      js: TJsonObject;
      litm: TListBoxItem;
    begin
      {  After the server finds the result, here is the data feedback  }
      {  The data of this event is temporary and will be killed after the call. If you want to temporarily store the query result data, you must do it here  }
      js := TJsonObject.Create;
      js.LoadFromStream(DataSour);
      litm := TListBoxItem.Create(ResultListBox);
      litm.Parent := ResultListBox;
      litm.Text := js.ToString;
      litm.Selectable := False;
      ResultListBox.AddObject(litm);

      {  Downloadassemblystream this method will be maximally compressed and encrypted in the data server before downloading. This method is applicable to public network communication downloading, such as mobile terminal and PC accessing internet server  }
      {  Fastdownloadassemblystream is the same as downloadassemblystream, but fastdownloadassemblystream does not process data, and its speed is faster. This method is mainly used for communication between servers  }
      DBClient.FastDownloadAssembleStreamP(JsonDestDBEdit.Text, js.L['StorePos'],
        procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64)
        var
          img: TImage;
          m: TMemoryStream64;
        begin
          {  Stream is temporary and encoded data, which must be decoded using the decodezdbfragment method  }
          {  After completion, it is stored in M. pay attention to releasing m at the end of the call  }
          m := DecodeZDBFragment(stream);
          img := TImage.Create(litm);
          img.Parent := litm;
          img.Align := TAlignLayout.Right;
          stream.Position := 0;
          // DoStatus('download size:%d md5:%s', [m.Size, umlMD5Char(m.Memory, m.Size).Text]);
          m.Position := 0;
          img.Bitmap.LoadFromStream(m);
          DisposeObject(m);
        end);

      DisposeObject(js);
    end,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      {  This event is triggered when the server query is completed  }
      TabControl.ActiveTab := ResultTabItem;
      DoStatus('Query %s completed. A total of %d matches were found', [dbN, TotalResult]);
    end);

  DisposeObject(vl);
end;

procedure TFMXBatchDataClientForm.ResetJsonDBButtonClick(Sender: TObject);
begin
  DBClient.ResetData(JsonDestDBEdit.Text);
  DBClient.ResetData(AnalysisDestDBEdit.Text);
end;

procedure TFMXBatchDataClientForm.Timer1Timer(Sender: TObject);
begin
  DBClient.Progress;
end;

end.
