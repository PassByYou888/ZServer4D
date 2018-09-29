unit zsGatewayConfigureFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  System.IOUtils,
  shellapi, TLHelp32,
  CoreClasses, MemoryStream64, ObjectDataManager, TextDataEngine, UnicodeMixedLib,
  PascalStrings, DoStatusIO, ItemStream, NotifyObjectBase,
  CommunicationFramework_Server_ICSCustomSocket;

type
  TExecThread = class(TThread)
  public
    FileName: U_String;
    ExecCode: DWord;
    SA: TSecurityAttributes;
    SI: TStartupInfo;
    pi: TProcessInformation;
    StdOutPipeRead, StdOutPipeWrite: THandle;

    procedure Execute; override;
  end;

  TzsGatewayConfigureForm = class(TForm)
    CfgPageControl: TPageControl;
    ServTabSheet: TTabSheet;
    Label2: TLabel;
    SharePortComboBox: TComboBox;
    NatPortEdit: TLabeledEdit;
    WebPortEdit: TLabeledEdit;
    ListView: TListView;
    Label4: TLabel;
    IPComboBox: TComboBox;
    AddItmButton: TButton;
    StartServerListenButton: TButton;
    StartClientListenButton: TButton;
    AboutButton: TButton;
    BrowseButton: TButton;
    DeleteItmButton: TButton;
    TokenEdit: TLabeledEdit;
    RandomTokenButton: TButton;
    X86RadioButton: TRadioButton;
    x64RadioButton: TRadioButton;
    Memo: TMemo;
    SaveButton: TButton;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure AddItmButtonClick(Sender: TObject);
    procedure DeleteItmButtonClick(Sender: TObject);
    procedure RandomTokenButtonClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure StartServerListenButtonClick(Sender: TObject);
    procedure AboutButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure StartClientListenButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    dbEng: TObjectDataManagerOfCache;
    Configure: TSectionTextData;
    servTh, cliTh: TExecThread;
    ProgressPost: TNProgressPostWithCadencer;

    procedure LoadConfig;
    procedure SaveConfig;

    procedure DoStatusMethod(AText: SystemString; const ID: Integer);

    procedure BuildFRPClientCfg(ns: TCoreClassStrings);
    procedure BuildFRPServerCfg(ns: TCoreClassStrings);
    procedure StopAllTh(th: TExecThread);
  end;

var
  zsGatewayConfigureForm: TzsGatewayConfigureForm = nil;
  ExecThreadList: TCoreClassListForObj            = nil;
  DefaultPath: string                             = '';
  LocalPath: string                               = '';

function FindProcessCount(aFileName: string): Integer;
function FindProcess(aFileName: string): Boolean;
procedure EndProcess(aFileName: string);
function WinExecFile(fn: U_String): TExecThread;

implementation

{$R *.dfm}


function FindProcessCount(aFileName: string): Integer;
var
  hSnapshot: THandle;    // 用于获得进程列表
  lppe: TProcessEntry32; // 用于查找进程
  Found: Boolean;        // 用于判断进程遍历是否完成
  KillHandle: THandle;   // 用于杀死进程
begin
  Result := 0;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // 获得系统进程列表
  lppe.dwSize := SizeOf(TProcessEntry32);                       // 在调用Process32First API之前，需要初始化lppe记录的大小
  Found := Process32First(hSnapshot, lppe);                     // 将进程列表的第一个进程信息读入ppe记录中
  while Found do
    begin
      if SameText(ExtractFilename(lppe.szExeFile), aFileName) or SameText(lppe.szExeFile, aFileName) then
          Inc(Result);
      Found := Process32Next(hSnapshot, lppe); // 将进程列表的下一个进程信息读入lppe记录中
    end;
end;

function FindProcess(aFileName: string): Boolean;
var
  hSnapshot: THandle;    // 用于获得进?塘斜?
  lppe: TProcessEntry32; // 用于查找进程
  Found: Boolean;        // 用于判断进程遍历是否完成
  KillHandle: THandle;   // 用于杀死进程
begin
  Result := False;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // 获得系统进程列表
  lppe.dwSize := SizeOf(TProcessEntry32);                       // 在调用Process32First API之前，需要初始化lppe记录的大小
  Found := Process32First(hSnapshot, lppe);                     // 将进程列表的第一个进程信息读入ppe记录中
  while Found do
    begin
      if SameText(ExtractFilename(lppe.szExeFile), aFileName) or SameText(lppe.szExeFile, aFileName) then
        begin
          Result := True;
        end;
      Found := Process32Next(hSnapshot, lppe); // 将进程列表的下一个进程信息读入lppe记录中
    end;
end;

procedure EndProcess(aFileName: string);
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapShotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapShotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapShotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
    begin
      if SameText(ExtractFilename(FProcessEntry32.szExeFile), aFileName) or SameText(FProcessEntry32.szExeFile, aFileName) then
          TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0), FProcessEntry32.th32ProcessID), 0);
      ContinueLoop := Process32Next(FSnapShotHandle, FProcessEntry32);
    end;
end;

function WinExecFile(fn: U_String): TExecThread;
begin
  Result := TExecThread.Create(True);
  Result.FileName := fn;
  Result.Suspended := False;
end;

procedure TExecThread.Execute;
const
  BuffSize   = 65535;
  HideWindow = True;
var
  WasOK: Boolean;
  buffer: array [0 .. BuffSize] of Char;
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
        CB := SizeOf(SI);
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

    WasOK := CreateProcess(nil, PChar(FileName.Text), nil, nil, True, 0, nil, nil, SI, pi);
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
                WasOK := ReadFile(StdOutPipeRead, buffer, BuffSize, BytesRead, nil);
                { has anything been read? }
                if (WasOK) and (BytesRead > 0) then
                  begin
                    { finish buffer to PChar }
                    buffer[BytesRead + 1] := #0;
                    OemToAnsi(@buffer, @buffer);
                    { combine the buffer with the rest of the last run }
                    TThread.Synchronize(Self, procedure
                      begin
                        DoStatus(StrPas(PAnsiChar(@buffer)));
                      end);
                  end;
              until (not WasOK) or (BytesRead = 0);
            end;
          { wait for console app to finish (should be already at this point) }
          WaitForSingleObject(pi.hProcess, Infinite);
          GetExitCodeProcess(pi.hProcess, ExecCode);
        finally
          { Close all remaining handles }
          CloseHandle(pi.hThread);
          CloseHandle(pi.hProcess);
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
                Inc(i);
          end;

        if zsGatewayConfigureForm.servTh = Self then
            zsGatewayConfigureForm.servTh := nil;
        if zsGatewayConfigureForm.cliTh = Self then
            zsGatewayConfigureForm.cliTh := nil;
      end);
  end;
end;

procedure TzsGatewayConfigureForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopAllTh(nil);

  if FindProcess('_zsfc.exe') then
      EndProcess('_zsfc.exe');
  umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfc.ini'));
  umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfc.exe'));

  if FindProcess('_zsfs.exe') then
      EndProcess('_zsfs.exe');
  umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfs.ini'));
  umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfs.exe'));

  Action := caFree;
end;

procedure TzsGatewayConfigureForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveConfig;
  CanClose := True;
end;

procedure TzsGatewayConfigureForm.FormCreate(Sender: TObject);
var
  C64: TResourceStream;
  m64: TMemoryStream64;
begin
  if FindProcess('_zsfc.exe') then
      EndProcess('_zsfc.exe');
  umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfc.ini'));
  umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfc.exe'));

  if FindProcess('_zsfs.exe') then
      EndProcess('_zsfs.exe');
  umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfs.ini'));
  umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfs.exe'));

  AddDoStatusHook(Self, DoStatusMethod);
{$IFDEF CPU64}
  x64RadioButton.Checked := True;
{$ELSE}
  X86RadioButton.Checked := True;
{$IFEND}
  //
  C64 := TResourceStream.Create(HInstance, 'frpFilePackage', RT_RCDATA);
  m64 := TMemoryStream64.Create;
  DecompressStream(C64, m64);
  m64.Position := 0;
  dbEng := TObjectDataManagerOfCache.CreateAsStream(m64, '', 0, True, False, True);
  DisposeObject(C64);

  RandomTokenButtonClick(RandomTokenButton);

  IPComboBox.Items.Assign(WSAIPList);

  Configure := TSectionTextData.Create;
  LoadConfig;
  ExecThreadList := TCoreClassListForObj.Create;

  servTh := nil;
  cliTh := nil;
  ProgressPost := TNProgressPostWithCadencer.Create;

  Timer.Enabled := True;
end;

procedure TzsGatewayConfigureForm.FormDestroy(Sender: TObject);
begin
  DisposeObject([dbEng, Configure, ExecThreadList, ProgressPost]);
end;

procedure TzsGatewayConfigureForm.AboutButtonClick(Sender: TObject);
begin
  MessageDlg('Gateway Service create qq600585' + #13#10 + '2018-1-15', mtInformation, [mbYes], 0);
end;

procedure TzsGatewayConfigureForm.AddItmButtonClick(Sender: TObject);
var
  i: Integer;
  n: string;
begin
  n := Format('s%s', [SharePortComboBox.Text]);
  for i := 0 to ListView.Items.Count - 1 do
    begin
      if SameText(n, ListView.Items[i].Caption) then
          Exit;
      if (ListView.Items[i].SubItems.Count > 0) and (SameText(SharePortComboBox.Text, ListView.Items[i].SubItems[0])) then
          Exit;
    end;

  with ListView.Items.Add do
    begin
      Caption := n;
      SubItems.Add(SharePortComboBox.Text);
      ImageIndex := -1;
      StateIndex := -1;
    end;

  umlAddNewStrTo(SharePortComboBox.Text, SharePortComboBox.Items);
  umlAddNewStrTo(IPComboBox.Text, IPComboBox.Items);
end;

procedure TzsGatewayConfigureForm.DeleteItmButtonClick(Sender: TObject);
begin
  ListView.DeleteSelected;
end;

procedure TzsGatewayConfigureForm.RandomTokenButtonClick(Sender: TObject);
var
  n: string;
  i: Integer;
  C: SystemChar;
begin
  n := '';
  for i := 1 to 16 do
    begin
      repeat
          C := SystemChar(umlRandomRange(32, 128));
      until CharIn(C, [c1to9, cLoAtoZ, cHiAtoZ]);
      n := n + C;
    end;
  TokenEdit.Text := n;
end;

procedure TzsGatewayConfigureForm.SaveButtonClick(Sender: TObject);
begin
  SaveConfig;
end;

procedure TzsGatewayConfigureForm.StartClientListenButtonClick(Sender: TObject);
var
  itmStream: TItemStream;
  m64: TMemoryStream64;
  ns: TCoreClassStringList;
begin
  if cliTh <> nil then
    begin
      Self.StopAllTh(cliTh);
      cliTh := nil;
      StartClientListenButton.Caption := 'Start NAT Client';
      umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfc.ini'));
      umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfc.exe'));
      Exit;
    end;

  SaveConfig;

  if x64RadioButton.Checked then
      itmStream := TItemStream.Create(dbEng, '/frp64', 'frpc.exe')
  else
      itmStream := TItemStream.Create(dbEng, '/frp86', 'frpc.exe');

  m64 := TMemoryStream64.Create;
  m64.CopyFrom(itmStream, itmStream.Size);
  DisposeObject(itmStream);
  try
      m64.SaveToFile(umlCombineFileName(DefaultPath, '_zsfc.exe'));
  except
  end;
  DisposeObject(m64);

  ns := TCoreClassStringList.Create;
  BuildFRPClientCfg(ns);
  ns.SaveToFile(umlCombineFileName(DefaultPath, '_zsfc.ini'));
  DisposeObject(ns);

  cliTh := WinExecFile(umlCombineFileName(DefaultPath, '_zsfc.exe') + Format(' -c "%s"', [umlCombineFileName(DefaultPath, '_zsfc.ini').Text]));

  StartClientListenButton.Caption := 'Stop NAT Client';

  ProgressPost.PostExecuteP(2.0, procedure(Sender: TNPostExecute)
    begin
      umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfc.ini'));
    end);
end;

procedure TzsGatewayConfigureForm.StartServerListenButtonClick(Sender: TObject);
var
  itmStream: TItemStream;
  m64: TMemoryStream64;
  ns: TCoreClassStringList;
begin
  if servTh <> nil then
    begin
      Self.StopAllTh(servTh);
      servTh := nil;
      StartServerListenButton.Caption := 'Start NAT Service';
      umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfs.ini'));
      umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfs.exe'));
      Exit;
    end;

  SaveConfig;

  if x64RadioButton.Checked then
      itmStream := TItemStream.Create(dbEng, '/frp64', 'frps.exe')
  else
      itmStream := TItemStream.Create(dbEng, '/frp86', 'frps.exe');

  m64 := TMemoryStream64.Create;
  m64.CopyFrom(itmStream, itmStream.Size);
  DisposeObject(itmStream);
  try
      m64.SaveToFile(umlCombineFileName(DefaultPath, '_zsfs.exe'));
  except
  end;
  DisposeObject(m64);

  ns := TCoreClassStringList.Create;
  BuildFRPServerCfg(ns);
  ns.SaveToFile(umlCombineFileName(DefaultPath, '_zsfs.ini'));
  DisposeObject(ns);

  servTh := WinExecFile(umlCombineFileName(DefaultPath, '_zsfs.exe') + Format(' -c "%s"', [umlCombineFileName(DefaultPath, '_zsfs.ini').Text]));

  StartServerListenButton.Caption := 'Stop NAT Service';

  ProgressPost.PostExecuteP(2.0, procedure(Sender: TNPostExecute)
    begin
      umlDeleteFile(umlCombineFileName(DefaultPath, '_zsfs.ini'));
    end);
end;

procedure TzsGatewayConfigureForm.StopAllTh(th: TExecThread);
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
          TerminateProcess(th.pi.hProcess, 0);
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
              TerminateProcess(th.pi.hProcess, 0);
              th.Terminate;
              Break;
            end
          else
              Inc(i);
        end;
    end;
  Memo.Visible := ExecThreadList.Count > 0;
  if not Memo.Visible then
    begin
      Memo.Clear;
      Position := TPosition.poScreenCenter;
    end;
end;

procedure TzsGatewayConfigureForm.TimerTimer(Sender: TObject);
begin
  ProgressPost.Progress;

  if servTh <> nil then
      StartServerListenButton.Caption := 'Stop NAT Service'
  else
    begin
      StartServerListenButton.Caption := 'Start NAT Service';
      EndProcess('_zsfs.exe');
    end;

  if cliTh <> nil then
      StartClientListenButton.Caption := 'Stop NAT Client'
  else
    begin
      StartClientListenButton.Caption := 'Start NAT Client';
      EndProcess('_zsfc.exe');
    end;
end;

procedure TzsGatewayConfigureForm.LoadConfig;
var
  ns: TCoreClassStrings;
  i: Integer;
  n: string;
begin
  if umlFileExists(umlCombineFileName(LocalPath, 'zsGatewayConfigure.ini')) then
    begin
      Configure.LoadFromFile(umlCombineFileName(LocalPath, 'zsGatewayConfigure.ini'));

      IPComboBox.Items.Assign(Configure.Names['RemoteIP']);
      SharePortComboBox.Items.Assign(Configure.Names['SharePort']);

      IPComboBox.Text := Configure.GetDefaultValue('options', IPComboBox.Name, IPComboBox.Text);
      SharePortComboBox.Text := Configure.GetDefaultValue('options', SharePortComboBox.Name, SharePortComboBox.Text);
      NatPortEdit.Text := Configure.GetDefaultValue('options', NatPortEdit.Name, NatPortEdit.Text);
      WebPortEdit.Text := Configure.GetDefaultValue('options', WebPortEdit.Name, WebPortEdit.Text);
      TokenEdit.Text := Configure.GetDefaultValue('options', TokenEdit.Name, TokenEdit.Text);

      ns := Configure.Names['NATListens'];

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
            end;
        end;
      ListView.Items.EndUpdate;
    end;
end;

procedure TzsGatewayConfigureForm.SaveConfig;
var
  ns: TCoreClassStrings;
  i: Integer;
begin
  Configure.Names['RemoteIP'].Assign(IPComboBox.Items);
  Configure.Names['SharePort'].Assign(SharePortComboBox.Items);

  Configure.SetDefaultValue('options', IPComboBox.Name, IPComboBox.Text);
  Configure.SetDefaultValue('options', SharePortComboBox.Name, SharePortComboBox.Text);
  Configure.SetDefaultValue('options', NatPortEdit.Name, NatPortEdit.Text);
  Configure.SetDefaultValue('options', WebPortEdit.Name, WebPortEdit.Text);
  Configure.SetDefaultValue('options', TokenEdit.Name, TokenEdit.Text);

  ns := Configure.Names['NATListens'];
  ns.Clear;

  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].SubItems.Count > 0 then
        umlAddNewStrTo(ListView.Items[i].SubItems[0], ns);

  Configure.SaveToFile(umlCombineFileName(LocalPath, 'zsGatewayConfigure.ini'));
end;

procedure TzsGatewayConfigureForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  if not Memo.Visible then
    begin
      Memo.Visible := True;
      Position := TPosition.poScreenCenter;
    end;

  Memo.Lines.Add(AText);
end;

procedure TzsGatewayConfigureForm.BrowseButtonClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, PWideChar(Format('http://%s:%s', [IPComboBox.Text, WebPortEdit.Text])), nil, nil, SW_SHOW);
end;

procedure TzsGatewayConfigureForm.BuildFRPClientCfg(ns: TCoreClassStrings);
var
  i: Integer;
begin
  ns.Clear;

  ns.Add('# client build from zsGateway v1.0');
  ns.Add('[common]');
  ns.Add(Format('server_addr = %s', [IPComboBox.Text]));
  ns.Add(Format('server_port = %s', [NatPortEdit.Text]));
  ns.Add(Format('auth_token = %s', [TokenEdit.Text]));
  ns.Add(Format('privilege_token = %s', [TokenEdit.Text]));

  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].SubItems.Count > 0 then
      begin
        ns.Add(Format('', []));
        ns.Add(Format('[%s]', [ListView.Items[i].Caption]));
        ns.Add(Format('type = tcp', []));
        ns.Add(Format('local_ip = 127.0.0.1', []));
        ns.Add(Format('local_port = %s', [ListView.Items[i].SubItems[0]]));
      end;
end;

procedure TzsGatewayConfigureForm.BuildFRPServerCfg(ns: TCoreClassStrings);
var
  i: Integer;
begin
  ns.Clear;

  ns.Add('# Service build from zsGateway v1.0');
  ns.Add(Format('[common]', []));
  ns.Add(Format('bind_addr = 0.0.0.0', []));
  ns.Add(Format('bind_port = %s', [NatPortEdit.Text]));
  ns.Add(Format('dashboard_port = %s', [WebPortEdit.Text]));
  ns.Add(Format('dashboard_user = %s', [TokenEdit.Text]));
  ns.Add(Format('dashboard_pwd = %s', [TokenEdit.Text]));
  ns.Add(Format('privilege_mode = true', []));
  ns.Add(Format('privilege_token = %s', [TokenEdit.Text]));

  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].SubItems.Count > 0 then
      begin
        ns.Add(Format('', []));
        ns.Add(Format('[%s]', [ListView.Items[i].Caption]));
        ns.Add(Format('type = tcp', []));
        ns.Add(Format('auth_token = %s', [TokenEdit.Text]));
        ns.Add(Format('bind_addr = 0.0.0.0', []));
        ns.Add(Format('listen_port = %s', [ListView.Items[i].SubItems[0]]));
      end;
end;

initialization

DefaultPath := System.IOUtils.TPath.GetTempPath;
LocalPath := umlGetFilePath(Application.Exename);

finalization

end. 
