{ ****************************************************************************** }
{ * host query service. written by QQ 600585@qq.com                            * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit HostQuery;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, ListEngine, UnicodeMixedLib, DoStatusIO,
  DataFrameEngine, MemoryStream64, PascalStrings, CoreCipher, NotifyObjectBase, Cadencer,
  TextDataEngine,
  CommunicationFramework, PhysicsIO;

type
  THPhysicsServer = TPhysicsServer;
  THPhysicsClient = TPhysicsClient;

  THostRec = record
    Host, Identifier: TPascalString;
  end;

  PHostRec = ^THostRec;

  THostQueryServer = class
  protected
    FPhysics: THPhysicsServer;
    FHostList: TCoreClassList;

    procedure cmd_query(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure SaveConfigure(stream: TCoreClassStream); overload;
    procedure SaveConfigure(fn: SystemString); overload;

    procedure LoadConfigure(stream: TCoreClassStream); overload;
    procedure LoadConfigure(fn: SystemString); overload;

    procedure AddHost(const Host, Identifier: SystemString);
    procedure StartService;
    procedure StopService;
    procedure Progress;

    property Physics: THPhysicsServer read FPhysics;
  end;

  THostQueryClient = class
  protected
    FPhysics: THPhysicsClient;
  public
    HostList: TPascalStringList;

    constructor Create(const Host, Filter: SystemString);
    destructor Destroy; override;
  end;

var
  HostQuery_ListenBind: SystemString = {$IFDEF PhysicsIO_On_CrossSocket} '' {$ELSE PhysicsIO_On_CrossSocket} '0.0.0.0' {$ENDIF PhysicsIO_On_CrossSocket};
  HostQuery_ListenPort: Word = 8398;

implementation

procedure THostQueryServer.cmd_query(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  Identifier: TPascalString;
  i: Integer;
  p: PHostRec;
begin
  Identifier := InData.Reader.ReadString;
  for i := FHostList.Count - 1 downto 0 do
    begin
      p := PHostRec(FHostList[i]);
      if (Identifier.Len = 0) or (umlMultipleMatch(True, Identifier, p^.Identifier)) then
          OutData.WriteString(p^.Host);
    end;
  Sender.DelayClose(15);
end;

constructor THostQueryServer.Create;
begin
  inherited Create;

  FPhysics := THPhysicsServer.Create;
  FPhysics.SwitchDefaultPerformance;
  FPhysics.RegisterStream('query').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_query;

  FHostList := TCoreClassList.Create;
end;

destructor THostQueryServer.Destroy;
begin
  FPhysics.StopService;
  Clear;
  DisposeObject(FPhysics);
  DisposeObject(FHostList);
  inherited Destroy;
end;

procedure THostQueryServer.Clear;
var
  i: Integer;
  p: PHostRec;
begin
  for i := 0 to FHostList.Count - 1 do
    begin
      p := PHostRec(FHostList[i]);
      p^.Host := '';
      p^.Identifier := '';
      dispose(p);
    end;
  FHostList.Clear;
end;

procedure THostQueryServer.SaveConfigure(stream: TCoreClassStream);
var
  te: THashTextEngine;
  i: Integer;
  p: PHostRec;
  n: SystemString;
  hl: THashStringList;
begin
  te := THashTextEngine.Create;

  for i := 0 to FHostList.Count - 1 do
    begin
      p := PHostRec(FHostList[i]);
      n := PFormat('%s-%s', [p^.Host.Text, p^.Identifier.Text]);
      hl := te.HStringList[n];
      hl['host'] := p^.Host;
      hl['Identifier'] := p^.Identifier;
    end;

  te.SaveToStream(stream);
  DisposeObject(te);
end;

procedure THostQueryServer.SaveConfigure(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmCreate);
  SaveConfigure(fs);
  DisposeObject(fs);
end;

procedure THostQueryServer.LoadConfigure(stream: TCoreClassStream);
var
  te: THashTextEngine;
  ls: TListPascalString;
  i: Integer;
  n: SystemString;
  hl: THashStringList;
  p: PHostRec;
begin
  Clear;

  te := THashTextEngine.Create;
  te.LoadFromStream(stream);

  ls := TListPascalString.Create;

  te.GetSectionList(ls);

  for i := 0 to ls.Count - 1 do
    begin
      n := ls[i];
      hl := te.HStringList[n];
      new(p);
      p^.Host := hl.GetDefaultValue('host', n);
      p^.Identifier := hl.GetDefaultValue('Identifier', n);
      FHostList.Add(p);
    end;

  DisposeObject(ls);

  DisposeObject(te);
end;

procedure THostQueryServer.LoadConfigure(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  LoadConfigure(fs);
  DisposeObject(fs);
end;

procedure THostQueryServer.AddHost(const Host, Identifier: SystemString);
var
  p: PHostRec;
begin
  new(p);
  p^.Host := Host;
  p^.Identifier := Identifier;
  FHostList.Add(p);
end;

procedure THostQueryServer.StartService;
begin
  if FPhysics.StartService(HostQuery_ListenBind, HostQuery_ListenPort) then
      DoStatus('Host Query server listen started, bind: %s port: %d', [TranslateBindAddr(HostQuery_ListenBind), HostQuery_ListenPort])
  else
      DoStatus('error: Host Query server failed!!, bind: %s port: %d', [TranslateBindAddr(HostQuery_ListenBind), HostQuery_ListenPort]);
end;

procedure THostQueryServer.StopService;
begin
  FPhysics.StopService;
end;

procedure THostQueryServer.Progress;
begin
  FPhysics.Progress;
end;

constructor THostQueryClient.Create(const Host, Filter: SystemString);
var
  SendDE, ResultDE: TDataFrameEngine;
begin
  inherited Create;
  FPhysics := THPhysicsClient.Create;
  FPhysics.SwitchDefaultPerformance;
  HostList := TPascalStringList.Create;

  if FPhysics.Connect(Host, HostQuery_ListenPort) then
    begin
      SendDE := TDataFrameEngine.Create;
      ResultDE := TDataFrameEngine.Create;

      SendDE.WriteString(Filter);
      FPhysics.WaitSendStreamCmd('query', SendDE, ResultDE, 2000);

      while ResultDE.Reader.NotEnd do
          HostList.Append(ResultDE.Reader.ReadString);

      DisposeObject(SendDE);
      DisposeObject(ResultDE);
      FPhysics.Disconnect;
    end;
end;

destructor THostQueryClient.Destroy;
begin
  DisposeObject(HostList);
  DisposeObject(FPhysics);
  inherited Destroy;
end;

end.
