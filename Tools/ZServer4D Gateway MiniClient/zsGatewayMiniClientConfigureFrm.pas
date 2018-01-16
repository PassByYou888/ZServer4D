unit zsGatewayMiniClientConfigureFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  System.IOUtils,
  ShellApi, TLHelp32,
  CoreClasses, MemoryStream64, ObjectDataManager, TextDataEngine, UnicodeMixedLib,
  PascalStrings, DoStatusIO, ItemStream, NotifyObjectBase, DataFrameEngine,
  CommunicationFramework,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Server_ICSCustomSocket;

type
  TExecThread = class(TThread)
  public
    FileName                       : umlString;
    ExecCode                       : DWORD;
    SA                             : TSecurityAttributes;
    SI                             : TStartupInfo;
    PI                             : TProcessInformation;
    StdOutPipeRead, StdOutPipeWrite: THandle;

    procedure Execute; override;
  end;

  TzsGatewayMiniClientConfigureForm = class(TForm)
    Memo: TMemo;
    Timer: TTimer;
    RemoteIPEdit: TLabeledEdit;
    RemotePortEdit: TLabeledEdit;
    GetConfigureButton: TButton;
    TrayIcon: TTrayIcon;
    ControlPanel: TPanel;
    Label1: TLabel;
    CfgPageControl: TPageControl;
    ServTabSheet: TTabSheet;
    RemoteInfoLabel: TLabel;
    ListView: TListView;
    OpenWebButton: TButton;
    IPV6CheckBox: TCheckBox;
    X86RadioButton: TRadioButton;
    x64RadioButton: TRadioButton;
    AboutButton: TButton;
    StartClientListenButton: TButton;
    MinimizedToTaskButton: TButton;
    TokenEdit: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure StartClientListenButtonClick(Sender: TObject);
    procedure AboutButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure GetConfigureButtonClick(Sender: TObject);
    procedure MinimizedToTaskButtonClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure OpenWebButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DBEng                  : TObjectDataManagerOfCache;
    cliTh                  : TExecThread;
    ProgressPost           : TNProgressPostWithCadencer;
    ConfigureClient        : TCommunicationFrameworkClient;
    NatPort, WebPort, Token: string;

    procedure DoStatusMethod(AText: SystemString; const ID: Integer);

    procedure BuildFRPClientCfg(ns: TCoreClassStrings);
    procedure StopAllTh(th: TExecThread);
  end;

var
  zsGatewayMiniClientConfigureForm: TzsGatewayMiniClientConfigureForm = nil;
  ExecThreadList                  : TCoreClassListForObj              = nil;
  DefaultPath                     : string                            = '';

function FindProcessCount(AFileName: string): Integer;
function FindProcess(AFileName: string): Boolean;
procedure EndProcess(AFileName: string);
function WinExecFile(fn: umlString): TExecThread;

implementation

{$R *.dfm}


function FindProcessCount(AFileName: string): Integer;
var
  hSnapshot : THandle;         // 用于获得进程列表
  lppe      : TProcessEntry32; // 用于查找进程
  Found     : Boolean;         // 用于判断进程遍历是否完成
  KillHandle: THandle;         // 用于杀死进程
begin
  Result := 0;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // 获得系统进程列表
  lppe.dwSize := SizeOf(TProcessEntry32);                       // 在调用Process32First API之前，需要初始化lppe记录的大小
  Found := Process32First(hSnapshot, lppe);                     // 将进程列表的第一个进程信息读入ppe记录中
  while Found do
    begin
      if SameText(ExtractFileName(lppe.szExeFile), AFileName) or SameText(lppe.szExeFile, AFileName) then
          inc(Result);
      Found := Process32Next(hSnapshot, lppe); // 将进程列表的下一个进程信息读入lppe记录中
    end;
end;

function FindProcess(AFileName: string): Boolean;
var
  hSnapshot : THandle;         // 用于获得进程列表
  lppe      : TProcessEntry32; // 用于查找进程
  Found     : Boolean;         // 用于判断进程遍历是否完成
  KillHandle: THandle;         // 用于杀死进程
begin
  Result := False;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // 获得系统进程列表
  lppe.dwSize := SizeOf(TProcessEntry32);                       // 在调用Process32First API之前，需要初始化lppe记录的大小
  Found := Process32First(hSnapshot, lppe);                     // 将进程列表的第一个进程信息读入ppe记录中
  while Found do
    begin
      if SameText(ExtractFileName(lppe.szExeFile), AFileName) or SameText(lppe.szExeFile, AFileName) then
        begin
          Result := True;
        end;
      Found := Process32Next(hSnapshot, lppe); // 将进程列表的下一个进程信息读入lppe记录中
    end;
end;

procedure EndProcess(AFileName: string);
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop   : BOOL;
  FSnapShotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapShotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapShotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
    begin
      if SameText(ExtractFileName(FProcessEntry32.szExeFile), AFileName) or SameText(FProcessEntry32.szExeFile, AFileName) then
          TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0), FProcessEntry32.th32ProcessID), 0);
      ContinueLoop := Process32Next(FSnapShotHandle, FProcessEntry32);
    end;
end;

function WinExecFile(fn: umlString): TExecThread;
begin
  Result := TExecThread.Create(True);
  Result.FileName := fn;
  Result.Suspended := False;
end;

procedure TExecThread.Execute;
const
  Buffsize   = 65535;
  HideWindow = True;
var
  WasOK    : Boolean;
  Buffer   : array [0 .. Buffsize] of Char;
  BytesRead: Cardinal;
begin
  FreeOnTerminate := True;

  TThread.Synchronize(Self, procedure
    begin
      ExecThreadList.Add(Self);
    end);

  if HideWindow then
    begin
      with SA do
        begin
          nLength := SizeOf(SA);
          bInheritHandle := True;
          lpSecurityDescriptor := nil;
        end;

      { create pipe for standard output redirection }
      CreatePipe(StdOutPipeRead, { read handle }
      StdOutPipeWrite,           { write handle }
      @SA,                       { security attributes }
      0                          { number of bytes reserved for pipe - 0 default }
        );
    end;

  try
    { Make child process use StdOutPipeWrite as standard out, and
      make sure it does not show on screen }
    with SI do
      begin
        FillChar(SI, SizeOf(SI), 0);
        cb := SizeOf(SI);
        dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        if HideWindow then
          begin
            wShowWindow := SW_HIDE;
            hStdInput := GetStdHandle(STD_INPUT_HANDLE); { don't redirect stdinput }
            hStdOutput := StdOutPipeWrite;
            hStdError := StdOutPipeWrite;
          end
        else
          begin
            wShowWindow := SW_SHOW;
          end;
      end;

    WasOK := CreateProcess(nil, PChar(FileName.Text), nil, nil, True, 0, nil, nil, SI, PI);
    {
      Now that the handle has been inherited, close write to be safe.We don't
      want to read or write to it accidentally
    }
    if HideWindow then
        CloseHandle(StdOutPipeWrite);

    { if process could be created then handle its output }
    if WasOK then
      begin
        try
          if HideWindow then
            begin
              { get all output until DOS app finishes }
              repeat
                { read block of characters (might contain carriage returns and line feeds) }
                WasOK := ReadFile(StdOutPipeRead, Buffer, Buffsize, BytesRead, nil);
                { has anything been read? }
                if (WasOK) and (BytesRead > 0) then
                  begin
                    { finish buffer to PChar }
                    Buffer[BytesRead + 1] := #0;
                    OemToAnsi(@Buffer, @Buffer);
                    { combine the buffer with the rest of the last run }
                    TThread.Synchronize(Self, procedure
                      begin
                        DoStatus(StrPas(PAnsiChar(@Buffer)));
                      end);
                  end;
              until (not WasOK) or (BytesRead = 0);
            end;
          { wait for console app to finish (should be already at this point) }
          WaitForSingleObject(PI.hProcess, INFINITE);
          GetExitCodeProcess(PI.hProcess, ExecCode);
        finally
          { Close all remaining handles }
          CloseHandle(PI.hThread);
          CloseHandle(PI.hProcess);
        end;
      end
    else
      begin
        ExecCode := 0;
      end;
  finally
    if HideWindow then
        CloseHandle(StdOutPipeRead);

    TThread.Synchronize(Self, procedure
      var
        i: Integer;
      begin
        i := 0;
        while i < ExecThreadList.Count do
          begin
            if ExecThreadList[i] = Self then
                ExecThreadList.Delete(i)
            else
                inc(i);
          end;

        if zsGatewayMiniClientConfigureForm.cliTh = Self then
            zsGatewayMiniClientConfigureForm.cliTh := nil;
      end);
  end;
end;

procedure TzsGatewayMiniClientConfigureForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopAllTh(nil);

  if FindProcess('_fc.exe') then
      EndProcess('_fc.exe');

  umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.ini'));
  umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.exe'));

  Action := caFree;
end;

procedure TzsGatewayMiniClientConfigureForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TzsGatewayMiniClientConfigureForm.FormCreate(Sender: TObject);
var
  c64: TResourceStream;
  m64: TMemoryStream64;
begin
  if FindProcess('_fc.exe') then
      EndProcess('_fc.exe');
  umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.ini'));
  umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.exe'));

  AddDoStatusHook(Self, DoStatusMethod);
  {$IFDEF CPU64}
  x64RadioButton.Checked := True;
  {$ELSE}
  X86RadioButton.Checked := True;
  {$IFEND}
  //
  c64 := TResourceStream.Create(hInstance, 'FRPClientPackage', RT_RCDATA);
  m64 := TMemoryStream64.Create;
  DecompressStream(c64, m64);
  m64.Position := 0;
  DBEng := TObjectDataManagerOfCache.CreateAsStream(m64, '', 0, True, False, True);
  DisposeObject(c64);

  ExecThreadList := TCoreClassListForObj.Create;

  cliTh := nil;
  ProgressPost := TNProgressPostWithCadencer.Create;

  ConfigureClient := TCommunicationFramework_Client_CrossSocket.Create;
  ConfigureClient.AllowPrintCommand := False;

  NatPort := '';
  WebPort := '';
  Token := '';

  Timer.Enabled := True;
end;

procedure TzsGatewayMiniClientConfigureForm.FormDestroy(Sender: TObject);
begin
  DisposeObject([DBEng, ExecThreadList, ProgressPost, ConfigureClient]);
end;

procedure TzsGatewayMiniClientConfigureForm.GetConfigureButtonClick(Sender: TObject);
begin
  if ConfigureClient.Connect(RemoteIPEdit.Text, umlStrToInt(RemotePortEdit.Text, 4797)) then
    begin
      ConfigureClient.SendStreamCmd('GetConfigure', nil, procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
        var
          te: TSectionTextData;
          ns: TStrings;
          i: Integer;
          n: string;
        begin
          te := TSectionTextData.Create;
          ResultData.Reader.ReadTextSection(te);

          NatPort := te.GetDefaultValue('Options', 'NatPortEdit', NatPort);
          WebPort := te.GetDefaultValue('Options', 'WebPortEdit', WebPort);
          Token := te.GetDefaultValue('Options', 'TokenEdit', Token);
          TokenEdit.Text := Token;

          ns := te.Names['NATListens'];

          ListView.Items.BeginUpdate;
          ListView.Items.Clear;
          for i := 0 to ns.Count - 1 do
            begin
              n := ns[i];
              with ListView.Items.Add do
                begin
                  Caption := Format('s%s', [n]);
                  SubItems.Add(n);
                  ImageIndex := -1;
                  StateIndex := -1;
                  Checked := True;
                end;
            end;
          ListView.Items.EndUpdate;

          RemoteInfoLabel.Caption := Format(
            'Remote NAT Listen port:%s' + #13#10 +
            'Remote Web Watch Listen port:%s' + #13#10 +
            'Remote token: %s',
            [NatPort, WebPort, Token]);

          DisposeObject(te);

          ControlPanel.Visible := True;
          Position := TPosition.poScreenCenter;

          ConfigureClient.Disconnect;
        end);
    end;
end;

procedure TzsGatewayMiniClientConfigureForm.MinimizedToTaskButtonClick(Sender: TObject);
begin
  Hide;
  TrayIcon.Visible := True;
end;

procedure TzsGatewayMiniClientConfigureForm.OpenWebButtonClick(
  Sender: TObject);
begin
  ShellExecute(Handle, nil, PWideChar(Format('http://%s:%s', [RemoteIPEdit.Text, WebPort])), nil, nil, SW_SHOW);
end;

procedure TzsGatewayMiniClientConfigureForm.AboutButtonClick(Sender: TObject);
begin
  MessageDlg('Gateway Service create qq600585' + #13#10 + '2018-1-15', mtInformation, [mbYes], 0);
end;

procedure TzsGatewayMiniClientConfigureForm.StartClientListenButtonClick(Sender: TObject);
var
  itmStream: TItemStream;
  m64      : TMemoryStream64;
  ns       : TCoreClassStringList;
begin
  if cliTh <> nil then
    begin
      Self.StopAllTh(cliTh);
      cliTh := nil;
      StartClientListenButton.Caption := 'Start NAT Client';
      umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.ini'));
      umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.exe'));
      exit;
    end;

  if x64RadioButton.Checked then
      itmStream := TItemStream.Create(DBEng, '/frp64', 'frpc.exe')
  else
      itmStream := TItemStream.Create(DBEng, '/frp86', 'frpc.exe');

  m64 := TMemoryStream64.Create;
  m64.CopyFrom(itmStream, itmStream.Size);
  DisposeObject(itmStream);
  try
      m64.SaveToFile(umlCombineFileName(DefaultPath, '_fc.exe'));
  except
  end;
  DisposeObject(m64);

  ns := TCoreClassStringList.Create;
  BuildFRPClientCfg(ns);
  ns.SaveToFile(umlCombineFileName(DefaultPath, '_fc.ini'));
  DisposeObject(ns);

  cliTh := WinExecFile(umlCombineFileName(DefaultPath, '_fc.exe') + Format(' -c "%s"', [umlCombineFileName(DefaultPath, '_fc.ini').Text]));

  StartClientListenButton.Caption := 'Stop NAT Client';

  ProgressPost.PostExecute(2.0, procedure(Sender: TNPostExecute)
    begin
      umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.ini'));
    end);
end;

procedure TzsGatewayMiniClientConfigureForm.StopAllTh(th: TExecThread);
var
  i: Integer;
begin
  if th = nil then
    begin
      while ExecThreadList.Count > 0 do
        begin
          th := TExecThread(ExecThreadList[0]);
          th.Suspended := True;
          ExecThreadList.Delete(0);
          TerminateProcess(th.PI.hProcess, 0);
          th.Terminate;
        end;
    end
  else
    begin
      i := 0;
      while i < ExecThreadList.Count do
        begin
          if ExecThreadList[i] = th then
            begin
              th.Suspended := True;
              ExecThreadList.Delete(i);
              TerminateProcess(th.PI.hProcess, 0);
              th.Terminate;
              break;
            end
          else
              inc(i);
        end;
    end;
  Memo.Visible := ExecThreadList.Count > 0;
  if not Memo.Visible then
    begin
      Memo.Clear;
      Position := TPosition.poScreenCenter;
    end;
end;

procedure TzsGatewayMiniClientConfigureForm.TimerTimer(Sender: TObject);
begin
  ConfigureClient.ProgressBackground;

  ProgressPost.Progress;

  if cliTh <> nil then
      StartClientListenButton.Caption := 'Stop NAT Client'
  else
    begin
      StartClientListenButton.Caption := 'Start NAT Client';
      EndProcess('_fc.exe');
      umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.ini'));
      umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.exe'));
    end;
end;

procedure TzsGatewayMiniClientConfigureForm.TrayIconClick(Sender: TObject);
begin
  TrayIcon.Visible := False;
  Show;
end;

procedure TzsGatewayMiniClientConfigureForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  if not Memo.Visible then
    begin
      Memo.Visible := True;
      Position := TPosition.poScreenCenter;
    end;

  Memo.Lines.Add(AText);
end;

procedure TzsGatewayMiniClientConfigureForm.BuildFRPClientCfg(ns: TCoreClassStrings);
var
  i: Integer;
begin
  ns.Clear;

  ns.Add('# client build from ZSGatewayMiniClient v1.0');
  ns.Add('[common]');
  ns.Add(Format('server_addr = %s', [RemoteIPEdit.Text]));
  ns.Add(Format('server_port = %s', [NatPort]));
  ns.Add(Format('auth_token = %s', [Token]));
  ns.Add(Format('privilege_token = %s', [Token]));

  for i := 0 to ListView.Items.Count - 1 do
    if (ListView.Items[i].Checked) and (ListView.Items[i].SubItems.Count > 0) then
      begin
        ns.Add(Format('', []));
        ns.Add(Format('[%s]', [ListView.Items[i].Caption]));
        ns.Add(Format('type = tcp', []));

        if IPV6CheckBox.Checked then
            ns.Add(Format('local_ip = [::1]', []))
        else
            ns.Add(Format('local_ip = 127.0.0.1', []));

        ns.Add(Format('local_port = %s', [ListView.Items[i].SubItems[0]]));
      end;
end;

initialization

DefaultPath := System.IOUtils.TPath.GetTempPath;

finalization

end.
