unit ZSGatewayMultiClientConfigureFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus,
  System.IOUtils, Winapi.ShellAPI, Winapi.TlHelp32,

  CoreClasses, MemoryStream64, ObjectDataManager, TextDataEngine, UnicodeMixedLib,
  PascalStrings, DoStatusIO, ItemStream, NotifyObjectBase, DataFrameEngine,
  CommunicationFramework,
  CommunicationFramework_Client_CrossSocket;

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

  PNAT_Port = ^TNAT_Port;

  TNAT_Port = record
    Name, Remote, Local: string;
    Enabled: Boolean;
  end;

  PNAT_Struct = ^TNAT_Struct;

  TNAT_Struct = record
    Host, RemotePort, NatPort, WebPort, token, info: string;
    PortArry: array of TNAT_Port;
    cliTh: TExecThread;
    ConfigureClient: TCommunicationFrameworkClient;

    procedure BuildFRPClientCfg(ns: TCoreClassStrings);
  end;

  TNatList = TGenericsList<PNAT_Struct>;

  TZSGatewayMultiClientConfigureForm = class(TForm)
    RemoteIPEdit: TLabeledEdit;
    RemotePortEdit: TLabeledEdit;
    GetConfigureButton: TButton;
    ControlPanel: TPanel;
    Label1: TLabel;
    IPV6CheckBox: TCheckBox;
    X86RadioButton: TRadioButton;
    x64RadioButton: TRadioButton;
    AboutButton: TButton;
    StartClientListenButton: TButton;
    Memo: TMemo;
    TabControl: TTabControl;
    OpenWebButton: TButton;
    TokenEdit: TLabeledEdit;
    RemoteInfoLabel: TLabel;
    ListView: TListView;
    sysProcessTimer: TTimer;
    NetworkTimer: TTimer;
    ListPopupMenu: TPopupMenu;
    ModifyLocalPort1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NetworkTimerTimer(Sender: TObject);
    procedure sysProcessTimerTimer(Sender: TObject);
    procedure GetConfigureButtonClick(Sender: TObject);
    procedure RemoteIPEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TabControlChange(Sender: TObject);
    procedure OpenWebButtonClick(Sender: TObject);
    procedure ModifyLocalPort1Click(Sender: TObject);
    procedure StartClientListenButtonClick(Sender: TObject);
    procedure AboutButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    NatList: TNatList;
    dbEng: TObjectDataManagerOfCache;
    CurrentNat: PNAT_Struct;
    ProgressPost: TNProgressPostWithCadencer;
    function ActivtedNatThreadCount: Integer;
    procedure StopAllTh(th: TExecThread);
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
    procedure UpdateCurrentNat(p: PNAT_Struct);
    procedure EndFRPProcess;
  end;

var
  ZSGatewayMultiClientConfigureForm: TZSGatewayMultiClientConfigureForm;

  ExecThreadList: TCoreClassListForObj = nil;
  DefaultPath: string                  = '';

function FindProcessCount(aFileName: string): Integer;
function FindProcess(aFileName: string): Boolean;
procedure EndProcess(aFileName: string);
function WinExecFile(fn: U_String): TExecThread;

implementation

{$R *.dfm}


function FindProcessCount(aFileName: string): Integer;
var
  hSnapshot: THandle;
  lppe: TProcessEntry32;
  Found: Boolean;
  KillHandle: THandle;
begin
  Result := 0;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  lppe.dwSize := SizeOf(TProcessEntry32);
  Found := Process32First(hSnapshot, lppe);
  while Found do
    begin
      if SameText(ExtractFilename(lppe.szExeFile), aFileName) or SameText(lppe.szExeFile, aFileName) then
          Inc(Result);
      Found := Process32Next(hSnapshot, lppe);
    end;
end;

function FindProcess(aFileName: string): Boolean;
var
  hSnapshot: THandle;
  lppe: TProcessEntry32;
  Found: Boolean;
  KillHandle: THandle;
begin
  Result := False;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  lppe.dwSize := SizeOf(TProcessEntry32);
  Found := Process32First(hSnapshot, lppe);
  while Found do
    begin
      if SameText(ExtractFilename(lppe.szExeFile), aFileName) or SameText(lppe.szExeFile, aFileName) then
        begin
          Result := True;
          break;
        end;
      Found := Process32Next(hSnapshot, lppe);
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
  BuffSize   = 65535 * 2;
  HideWindow = True;
var
  WasOK: Boolean;
  buffer: array [0 .. BuffSize] of Byte;
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
                    buffer[BytesRead + 1] := 0;
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
        p: PNAT_Struct;
      begin
        i := 0;
        while i < ExecThreadList.Count do
          begin
            if ExecThreadList[i] = Self then
                ExecThreadList.Delete(i)
            else
                Inc(i);
          end;
        i := 0;
        while i < ZSGatewayMultiClientConfigureForm.NatList.Count do
          begin
            p := ZSGatewayMultiClientConfigureForm.NatList[i];
            if p^.cliTh = Self then
                p^.cliTh := nil;

            Inc(i);
          end;
      end);
  end;
end;

procedure TNAT_Struct.BuildFRPClientCfg(ns: TCoreClassStrings);
var
  i: Integer;
begin
  ns.Clear;

  ns.Add('# client build from ZSGatewayMultiClient v1.0');
  ns.Add('[common]');
  ns.Add(Format('server_addr = %s', [Host]));
  ns.Add(Format('server_port = %s', [NatPort]));
  ns.Add(Format('auth_token = %s', [token]));
  ns.Add(Format('privilege_token = %s', [token]));

  for i := 0 to length(PortArry) - 1 do
    if (PortArry[i].Enabled) then
      begin
        ns.Add(Format('', []));
        ns.Add(Format('[%s]', [PortArry[i].Name]));
        ns.Add(Format('type = tcp', []));

        if ZSGatewayMultiClientConfigureForm.IPV6CheckBox.Checked then
            ns.Add(Format('local_ip = [::1]', []))
        else
            ns.Add(Format('local_ip = 127.0.0.1', []));

        ns.Add(Format('remote_port = %s', [PortArry[i].Remote]));
        ns.Add(Format('local_port = %s', [PortArry[i].Local]));
      end;
end;

procedure TZSGatewayMultiClientConfigureForm.FormCreate(Sender: TObject);
var
  C64: TResourceStream;
  m64: TMemoryStream64;
begin

  AddDoStatusHook(Self, DoStatusMethod);
{$IFDEF CPU64}
  x64RadioButton.Checked := True;
{$ELSE}
  X86RadioButton.Checked := True;
{$IFEND}
  //
  C64 := TResourceStream.Create(HInstance, 'FRPClientPackage', RT_RCDATA);
  m64 := TMemoryStream64.Create;
  DecompressStream(C64, m64);
  m64.Position := 0;
  dbEng := TObjectDataManagerOfCache.CreateAsStream(m64, '', 0, True, False, True);
  DisposeObject(C64);

  NatList := TNatList.Create;
  ProgressPost := TNProgressPostWithCadencer.Create;

  StopAllTh(nil);
end;

procedure TZSGatewayMultiClientConfigureForm.FormDestroy(Sender: TObject);
var
  i: Integer;
  p: PNAT_Struct;
begin
  DisposeObject(dbEng);

  StopAllTh(nil);
  for i := 0 to NatList.Count - 1 do
    begin
      p := NatList[i];
      p^.ConfigureClient.Disconnect;
      DisposeObject(p^.ConfigureClient);
      SetLength(p^.PortArry, 0);
      Dispose(p);
    end;
  DisposeObject(NatList);
  DisposeObject(ProgressPost);
end;

procedure TZSGatewayMultiClientConfigureForm.NetworkTimerTimer(Sender: TObject);
var
  i: Integer;
begin
  for i := NatList.Count - 1 downto 0 do
      NatList[i]^.ConfigureClient.Progress;
end;

procedure TZSGatewayMultiClientConfigureForm.sysProcessTimerTimer(Sender: TObject);
begin
  if ActivtedNatThreadCount > 0 then
      StartClientListenButton.Caption := 'Stop NAT Client'
  else
      StartClientListenButton.Caption := 'Start NAT Client';

  ProgressPost.Progress;
end;

procedure TZSGatewayMultiClientConfigureForm.GetConfigureButtonClick(Sender: TObject);
var
  p: PNAT_Struct;
  existed: Boolean;
begin
  existed := False;
  for p in NatList do
    if SameText(p^.Host, RemoteIPEdit.Text) and SameText(p^.RemotePort, RemotePortEdit.Text) then
      begin
        existed := True;
        break;
      end;

  if not existed then
    begin
      New(p);
      p^.Host := RemoteIPEdit.Text;
      p^.RemotePort := RemotePortEdit.Text;
      p^.NatPort := '';
      p^.WebPort := '';
      p^.token := '';
      p^.info := '';
      p^.cliTh := nil;
      SetLength(p^.PortArry, 0);
      p^.ConfigureClient := TCommunicationFramework_Client_CrossSocket.Create;
      p^.ConfigureClient.QuietMode := True;
      p^.ConfigureClient.SwitchMaxSafe;

      NatList.Add(p);
      TabControl.TabIndex := TabControl.Tabs.Add(p^.Host);
    end;

  if p^.ConfigureClient.Connect(p^.Host, umlStrToInt(p^.RemotePort, 4797)) then
    begin
      p^.ConfigureClient.SendStreamCmd('GetConfigure', nil, p, nil,
        procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine)
        var
          te: TSectionTextData;
          ns: TStrings;
          i: Integer;
          n: string;
          p: PNAT_Struct;
        begin
          p := Param1;
          te := TSectionTextData.Create;
          ResultData.Reader.ReadTextSection(te);

          p^.NatPort := te.GetDefaultValue('Options', 'NatPortEdit', p^.NatPort);
          p^.WebPort := te.GetDefaultValue('Options', 'WebPortEdit', p^.WebPort);
          p^.token := te.GetDefaultValue('Options', 'TokenEdit', p^.token);

          ns := te.Names['NATListens'];

          SetLength(p^.PortArry, ns.Count);

          for i := 0 to ns.Count - 1 do
            begin
              p^.PortArry[i].Name := Format('s%s', [ns[i]]);
              p^.PortArry[i].Remote := ns[i];
              p^.PortArry[i].Local := ns[i];
              p^.PortArry[i].Enabled := True;
            end;

          p^.info := Format(
            'Host:%s' + #13#10 +
            'NAT Listen port:%s' + #13#10 +
            'Web Listen port:%s' + #13#10 +
            'Remote token: %s',
            [p^.Host, p^.NatPort, p^.WebPort, p^.token]);

          DisposeObject(te);

          ControlPanel.Visible := True;
          Position := TPosition.poScreenCenter;

          p^.ConfigureClient.Disconnect;

          UpdateCurrentNat(p);
        end);
    end
  else
    begin
      if not existed then
        begin
          TabControl.Tabs.Delete(TabControl.Tabs.Count - 1);
          UpdateCurrentNat(nil);
          NatList.Delete(NatList.Count - 1);
          DisposeObject(p^.ConfigureClient);
          SetLength(p^.PortArry, 0);
          Dispose(p);
        end;
    end;
end;

procedure TZSGatewayMultiClientConfigureForm.RemoteIPEditKeyUp(Sender: TObject;
var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
      GetConfigureButtonClick(GetConfigureButton);
end;

procedure TZSGatewayMultiClientConfigureForm.TabControlChange(Sender: TObject);
begin
  if TabControl.TabIndex <= NatList.Count - 1 then
      UpdateCurrentNat(NatList[TabControl.TabIndex])
  else
      UpdateCurrentNat(nil);
end;

procedure TZSGatewayMultiClientConfigureForm.OpenWebButtonClick(Sender:
  TObject);
begin
  if CurrentNat = nil then
      exit;
  ShellExecute(Handle, nil, PWideChar(Format('http://%s:%s', [CurrentNat.Host, CurrentNat.WebPort])), nil, nil, SW_SHOW);
end;

procedure TZSGatewayMultiClientConfigureForm.ModifyLocalPort1Click(Sender:
  TObject);
var
  s: string;
begin
  if ListView.SelCount <> 1 then
      exit;
  s := PNAT_Port(ListView.selected.Data)^.Local;
  if InputQuery('Modify local port', 'local port:', s) then
    begin
      PNAT_Port(ListView.selected.Data)^.Local := s;
      UpdateCurrentNat(CurrentNat);
    end;
end;

procedure TZSGatewayMultiClientConfigureForm.StartClientListenButtonClick(Sender: TObject);
var
  itmStream: TItemStream;
  m64: TMemoryStream64;
  i: Integer;
begin
  if ActivtedNatThreadCount > 0 then
    begin
      StopAllTh(nil);
      exit;
    end;

  if x64RadioButton.Checked then
      itmStream := TItemStream.Create(dbEng, '/frp64', 'frpc.exe')
  else
      itmStream := TItemStream.Create(dbEng, '/frp86', 'frpc.exe');

  m64 := TMemoryStream64.Create;
  m64.CopyFrom(itmStream, itmStream.Size);
  DisposeObject(itmStream);
  try
      m64.SaveToFile(umlCombineFileName(DefaultPath, '_fc.exe'));
  except
  end;
  DisposeObject(m64);

  for i := 0 to NatList.Count - 1 do
    begin
      ProgressPost.PostExecute(i * 0.5,
        procedure(Sender: TNPostExecute)
        var
          p: PNAT_Struct;
          fn: string;
          ns: TCoreClassStringList;
        begin
          p := Sender.Data5;
          ns := TCoreClassStringList.Create;
          p^.BuildFRPClientCfg(ns);
          fn := umlCombineFileName(DefaultPath, Format('_fc_%s.ini', [p^.Host]));
          ns.SaveToFile(fn);
          DisposeObject(ns);

          p^.cliTh := WinExecFile(umlCombineFileName(DefaultPath, '_fc.exe') + Format(' -c "%s"', [fn]));

          ProgressPost.PostExecute(10,
            procedure(Sender: TNPostExecute)
            begin
              umlDeleteFile(SystemString(Sender.Data3));
            end).Data3 := fn;
        end).Data5 := NatList[i];
    end;
end;

procedure TZSGatewayMultiClientConfigureForm.AboutButtonClick(Sender: TObject);
begin
  MessageDlg('Gateway Service create qq600585' + #13#10 + '2018-1-15', mtInformation, [mbYes], 0);
end;

function TZSGatewayMultiClientConfigureForm.ActivtedNatThreadCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := NatList.Count - 1 downto 0 do
    if NatList[i]^.cliTh <> nil then
        Inc(Result);
end;

procedure TZSGatewayMultiClientConfigureForm.StopAllTh(th: TExecThread);
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
              break;
            end
          else
              Inc(i);
        end;
    end;

  for i := 0 to NatList.Count - 1 do
      NatList[i]^.cliTh := nil;

  EndFRPProcess;

  Memo.Visible := ExecThreadList.Count > 0;
  if not Memo.Visible then
    begin
      Memo.Clear;
      Position := TPosition.poScreenCenter;
    end;
end;

procedure TZSGatewayMultiClientConfigureForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  if not Memo.Visible then
    begin
      Memo.Visible := True;
      Position := TPosition.poScreenCenter;
    end;

  Memo.Lines.Add(AText);
end;

procedure TZSGatewayMultiClientConfigureForm.UpdateCurrentNat(p: PNAT_Struct);
var
  i: Integer;
begin
  CurrentNat := p;

  if CurrentNat = nil then
    begin
      TokenEdit.Text := '';
      RemoteInfoLabel.Caption := '';
      ListView.Clear;
      TabControl.TabIndex := -1;
      exit;
    end;

  TokenEdit.Text := CurrentNat^.token;
  RemoteInfoLabel.Caption := CurrentNat^.info;

  ListView.Items.BeginUpdate;
  ListView.Clear;
  for i := 0 to length(p^.PortArry) - 1 do
    begin
      with ListView.Items.Add do
        begin
          Caption := p^.PortArry[i].Name;
          SubItems.Add(p^.PortArry[i].Remote);
          SubItems.Add(p^.PortArry[i].Local);
          ImageIndex := -1;
          StateIndex := -1;
          Checked := p^.PortArry[i].Enabled;
          Data := @p^.PortArry[i];
        end;
    end;
  ListView.Items.EndUpdate;
end;

procedure TZSGatewayMultiClientConfigureForm.EndFRPProcess;
var
  fl: U_StringArray;
  i: Integer;
begin
  EndProcess('_fc.exe');
  umlDeleteFile(umlCombineFileName(DefaultPath, '_fc.exe'));

  fl := umlGetFileListWithFullPath(DefaultPath);
  for i := 0 to length(fl) - 1 do
    if umlMultipleMatch(True, '_fc_*.ini', umlGetFileName(fl[i])) then
        umlDeleteFile(fl[i]);
end;

initialization

DefaultPath := System.IOUtils.TPath.GetTempPath;
ExecThreadList := TCoreClassListForObj.Create;

finalization

DisposeObject(ExecThreadList);

end.
