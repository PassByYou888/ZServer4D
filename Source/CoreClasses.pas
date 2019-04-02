{ ****************************************************************************** }
{ * Core class library  written by QQ 600585@qq.com                            * }
{ ****************************************************************************** }
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

(*
  update history
  2017-12-6
  timetick
*)

unit CoreClasses;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Classes, Types,
  {$IFDEF parallel}
  {$IFDEF FPC}
  mtprocs,
  {$ELSE FPC}
  Threading,
  {$ENDIF FPC}
  {$ENDIF parallel}
  PascalStrings,
  SyncObjs
  {$IFDEF FPC}
    , FPCGenericStructlist
  {$ELSE FPC}
  , System.Generics.Collections
  {$ENDIF FPC}
  ,Math;

const
  fmCreate        = Classes.fmCreate;
  soFromBeginning = Classes.soFromBeginning;
  soFromCurrent   = Classes.soFromCurrent;
  soFromEnd       = Classes.soFromEnd;

  fmOpenRead      = SysUtils.fmOpenRead;
  fmOpenWrite     = SysUtils.fmOpenWrite;
  fmOpenReadWrite = SysUtils.fmOpenReadWrite;

  fmShareExclusive = SysUtils.fmShareExclusive;
  fmShareDenyWrite = SysUtils.fmShareDenyWrite;
  fmShareDenyNone  = SysUtils.fmShareDenyNone;

type
  TBytes = SysUtils.TBytes;
  TPoint = Types.TPoint;

  TTimeTick = UInt64;
  PTimeTick = ^TTimeTick;

  TSeekOrigin = Classes.TSeekOrigin;
  TNotify     = Classes.TNotifyEvent;

  TCoreClassObject     = TObject;
  TCoreClassPersistent = TPersistent;

  TCoreClassStream         = TStream;
  TCoreClassFileStream     = TFileStream;
  TCoreClassStringStream   = TStringStream;
  TCoreClassResourceStream = TResourceStream;

  TCoreClassThread = TThread;

  CoreClassException = Exception;

  TCoreClassMemoryStream = TMemoryStream;
  TCoreClassStrings    = TStrings;
  TCoreClassStringList = TStringList;
  TCoreClassReader     = TReader;
  TCoreClassWriter     = TWriter;
  TCoreClassComponent  = TComponent;

  {$IFDEF FPC}
  PUInt64 = ^UInt64;

  TCoreClassInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: longint; {$IFNDEF WINDOWS} cdecl {$ELSE} stdcall {$ENDIF};
    function _Release: longint; {$IFNDEF WINDOWS} cdecl {$ELSE} stdcall {$ENDIF};
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  PCoreClassPointerList = Classes.PPointerList;
  TCoreClassPointerList = Classes.TPointerList;
  TCoreClassListSortCompare = Classes.TListSortCompare;
  TCoreClassListNotification = Classes.TListNotification;

  TCoreClassList = class(TList)
    property ListData: PPointerList read GetList;
  end;

  TCoreClassListForObj = specialize TGenericsList<TCoreClassObject>;

  {$ELSE FPC}
  TCoreClassInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TGenericsList<t>=class(System.Generics.Collections.TList<t>)
  private type
    TGArry = array of t;
  public var Arry:TGArry;
    function ListData: Pointer;
  end;

  TGenericsObjectList<t:class>=class(System.Generics.Collections.TList<t>)
  private type
    TGArry = array of t;
  public var Arry:TGArry;
    function ListData: Pointer;
  end;

  TCoreClassPointerList = array of Pointer;
  PCoreClassPointerList = ^TCoreClassPointerList;

  TCoreClassList = class(TGenericsList<Pointer>)
    function ListData: PCoreClassPointerList;
  end;

  TCoreClassForObjectList = array of TCoreClassObject;
  PCoreClassForObjectList = ^TCoreClassForObjectList;

  TCoreClassListForObj = class(TGenericsList<TCoreClassObject>)
    function ListData: PCoreClassForObjectList;
  end;
  {$ENDIF FPC}

  TCoreClassObjectList = class(TCoreClassListForObj)
  private
    AutoFreeObj: Boolean;
  public
    constructor Create; overload;
    constructor Create(AutoFreeObj_: Boolean); overload;
    destructor Destroy; override;

    procedure Remove(obj: TCoreClassObject);
    procedure Delete(index: Integer);
    procedure Clear;
  end;

  TSoftCritical = class(TCoreClassObject)
  private
    L: Boolean;
  public
    constructor Create;
    procedure Acquire;
    procedure Release;
    property Busy:Boolean read L;
  end;

{$IFDEF SoftCritical}
  TCritical = TSoftCritical;
{$ELSE SoftCritical}
  TCritical = TCriticalSection;
{$ENDIF SoftCritical}

  TComputeThread = class;

  TRunWithThreadCall = procedure(ThSender: TComputeThread);
  TRunWithThreadMethod = procedure(ThSender: TComputeThread) of object;
  {$IFNDEF FPC} TRunWithThreadProc = reference to procedure(ThSender: TComputeThread); {$ENDIF FPC}

  TComputeThread = class(TCoreClassThread)
  protected
    OnRunCall: TRunWithThreadCall;
    OnRunMethod: TRunWithThreadMethod;
    {$IFNDEF FPC} OnRunProc: TRunWithThreadProc; {$ENDIF FPC}
    OnDoneCall: TRunWithThreadCall;
    OnDoneMethod: TRunWithThreadMethod;
    {$IFNDEF FPC} OnDoneProc: TRunWithThreadProc; {$ENDIF FPC}
    procedure Execute; override;
    procedure Done_Sync;
    procedure Halt_Sync;
  public
    UserData: Pointer;
    UserObject: TCoreClassObject;

    constructor Create;

    class procedure RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadCall); overload;
    class procedure RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadCall); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadMethod); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadMethod); overload;
    {$IFNDEF FPC}
    class procedure RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadProc); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadProc); overload;
    {$ENDIF FPC}
  end;

  TExecutePlatform = (epWin32, epWin64, epOSX32, epOSX64, epIOS, epIOSSIM, epANDROID32, epANDROID64, epLinux64, epLinux32, epUnknow);

const
  {$IF Defined(WIN32)}
  CurrentPlatform = TExecutePlatform.epWin32;
  {$ELSEIF Defined(WIN64)}
  CurrentPlatform = TExecutePlatform.epWin64;
  {$ELSEIF Defined(OSX)}
    {$IFDEF CPU64}
      CurrentPlatform = TExecutePlatform.epOSX64;
    {$ELSE CPU64}
      CurrentPlatform = TExecutePlatform.epOSX32;
    {$IFEND CPU64}
  {$ELSEIF Defined(IOS)}
    {$IFDEF CPUARM}
    CurrentPlatform = TExecutePlatform.epIOS;
    {$ELSE CPUARM}
    CurrentPlatform = TExecutePlatform.epIOSSIM;
    {$ENDIF CPUARM}
  {$ELSEIF Defined(ANDROID)}
    {$IFDEF CPU64}
    CurrentPlatform = TExecutePlatform.epANDROID64;
    {$ELSE CPU64}
    CurrentPlatform = TExecutePlatform.epANDROID32;
    {$IFEND CPU64}
  {$ELSEIF Defined(Linux)}
    {$IFDEF CPU64}
      CurrentPlatform = TExecutePlatform.epLinux64;
    {$ELSE CPU64}
      CurrentPlatform = TExecutePlatform.epLinux32;
    {$IFEND CPU64}
  {$ELSE}
  CurrentPlatform = TExecutePlatform.epUnknow;
  {$IFEND}

procedure FreeCoreThreadPool;

procedure DisposeObject(const Obj: TObject); overload;
procedure DisposeObject(const objs: array of TObject); overload;
procedure FreeObject(const Obj: TObject); overload;
procedure FreeObject(const objs: array of TObject); overload;

procedure LockObject(Obj: TObject);
procedure UnLockObject(Obj: TObject);

procedure AtomInc(var x: Int64); overload;
procedure AtomInc(var x: Int64; const v: Int64); overload;
procedure AtomDec(var x: Int64); overload;
procedure AtomDec(var x: Int64; const v: Int64); overload;
procedure AtomInc(var x: UInt64); overload;
procedure AtomInc(var x: UInt64; const v: UInt64); overload;
procedure AtomDec(var x: UInt64); overload;
procedure AtomDec(var x: UInt64; const v: UInt64); overload;
procedure AtomInc(var x: Integer); overload;
procedure AtomInc(var x: Integer; const v:Integer); overload;
procedure AtomDec(var x: Integer); overload;
procedure AtomDec(var x: Integer; const v:Integer); overload;
procedure AtomInc(var x: Cardinal); overload;
procedure AtomInc(var x: Cardinal; const v:Cardinal); overload;
procedure AtomDec(var x: Cardinal); overload;
procedure AtomDec(var x: Cardinal; const v:Cardinal); overload;

procedure FillPtrByte(const dest:Pointer; Count: NativeInt; const Value: Byte);
function CompareMemory(const p1, p2: Pointer; Count: NativeInt): Boolean;
procedure CopyPtr(const sour, dest:Pointer; Count: NativeInt);

procedure RaiseInfo(const n: SystemString); overload;
procedure RaiseInfo(const n: SystemString; const Args: array of const); overload;

function IsMobile: Boolean;

const
  C_Tick_Second = TTimeTick(1000);
  C_Tick_Minute = TTimeTick(C_Tick_Second) * 60;
  C_Tick_Hour   = TTimeTick(C_Tick_Minute) * 60;
  C_Tick_Day    = TTimeTick(C_Tick_Hour) * 24;
  C_Tick_Week   = TTimeTick(C_Tick_Day) * 7;
  C_Tick_Year   = TTimeTick(C_Tick_Day) * 365;

function GetTimeTick: TTimeTick;
function GetTimeTickCount: TTimeTick;
function GetCrashTimeTick: TTimeTick;

function ROL8(const Value: Byte; Shift: Byte): Byte;
function ROL16(const Value: Word; Shift: Byte): Word;
function ROL32(const Value: Cardinal; Shift: Byte): Cardinal;
function ROL64(const Value: UInt64; Shift: Byte): UInt64;
function ROR8(const Value: Byte; Shift: Byte): Byte;
function ROR16(const Value: Word; Shift: Byte): Word;
function ROR32(const Value: Cardinal; Shift: Byte): Cardinal;
function ROR64(const Value: UInt64; Shift: Byte): UInt64;

function Endian(const AValue: SmallInt): SmallInt; overload;
function Endian(const AValue: Word): Word; overload;
function Endian(const AValue: Integer): Integer; overload;
function Endian(const AValue: Cardinal): Cardinal; overload;
function Endian(const AValue: Int64): Int64; overload;
function Endian(const AValue: UInt64): UInt64; overload;

function BE2N(const AValue: SmallInt): SmallInt; overload;
function BE2N(const AValue: Word): Word; overload;
function BE2N(const AValue: Integer): Integer; overload;
function BE2N(const AValue: Cardinal): Cardinal; overload;
function BE2N(const AValue: Int64): Int64; overload;
function BE2N(const AValue: UInt64): UInt64; overload;

function LE2N(const AValue: SmallInt): SmallInt; overload;
function LE2N(const AValue: Word): Word; overload;
function LE2N(const AValue: Integer): Integer; overload;
function LE2N(const AValue: Cardinal): Cardinal; overload;
function LE2N(const AValue: Int64): Int64; overload;
function LE2N(const AValue: UInt64): UInt64; overload;

function N2BE(const AValue: SmallInt): SmallInt; overload;
function N2BE(const AValue: Word): Word; overload;
function N2BE(const AValue: Integer): Integer; overload;
function N2BE(const AValue: Cardinal): Cardinal; overload;
function N2BE(const AValue: Int64): Int64; overload;
function N2BE(const AValue: UInt64): UInt64; overload;

function N2LE(const AValue: SmallInt): SmallInt; overload;
function N2LE(const AValue: Word): Word; overload;
function N2LE(const AValue: Integer): Integer; overload;
function N2LE(const AValue: Cardinal): Cardinal; overload;
function N2LE(const AValue: Int64): Int64; overload;
function N2LE(const AValue: UInt64): UInt64; overload;

procedure Swap(var v1, v2: Byte); overload;
procedure Swap(var v1, v2: Word); overload;
procedure Swap(var v1, v2: Integer); overload;
procedure Swap(var v1, v2: Cardinal); overload;
procedure Swap(var v1, v2: Int64); overload;
procedure Swap(var v1, v2: UInt64); overload;
procedure Swap(var v1, v2: SystemString); overload;
procedure Swap(var v1, v2: Single); overload;
procedure Swap(var v1, v2: Double); overload;
procedure Swap(var v1, v2: Pointer); overload;

function Swap(const v: Word): Word; overload;
function Swap(const v: Cardinal): Cardinal; overload;
function Swap(const v: UInt64): UInt64; overload;

function SAR16(const AValue: SmallInt; const Shift: Byte): SmallInt;
function SAR32(const AValue: Integer; Shift: Byte): Integer;
function SAR64(const AValue: Int64; Shift: Byte): Int64;

function MemoryAlign(addr: Pointer; alignment: nativeUInt): Pointer;

// NoP = No Operation. It's the empty function, whose purpose is only for the
// debugging, or for the piece of code where intentionaly nothing is planned to be.
procedure Nop;

procedure CheckThreadSynchronize; overload;
function CheckThreadSynchronize(Timeout: Integer): Boolean; overload;

var
  GlobalMemoryHook: Boolean;

implementation

uses DoStatusIO;

{$INCLUDE CoreAtomic.inc}

procedure DisposeObject(const Obj: TObject);
begin
  if Obj <> nil then
    begin
      try
        {$IFDEF AUTOREFCOUNT}
        Obj.DisposeOf;
        {$ELSE AUTOREFCOUNT}
        Obj.Free;
        {$ENDIF AUTOREFCOUNT}
        {$IFDEF CriticalSimulateAtomic}
        _RecycleLocker(Obj);
        {$ENDIF CriticalSimulateAtomic}
      except
      end;
    end;
end;

procedure DisposeObject(const objs: array of TObject);
var
  Obj: TObject;
begin
  for Obj in objs do
      DisposeObject(Obj);
end;

procedure FreeObject(const Obj: TObject);
begin
  DisposeObject(Obj);
end;

procedure FreeObject(const objs: array of TObject);
var
  Obj: TObject;
begin
  for Obj in objs do
      FreeObject(Obj);
end;

procedure LockObject(Obj: TObject);
{$IFNDEF CriticalSimulateAtomic}
{$IFDEF ANTI_DEAD_ATOMIC_LOCK}
var
  d: TTimeTick;
{$ENDIF ANTI_DEAD_ATOMIC_LOCK}
{$ENDIF CriticalSimulateAtomic}
begin
{$IFDEF FPC}
  _LockCriticalObj(Obj);
{$ELSE FPC}
{$IFDEF CriticalSimulateAtomic}
  _LockCriticalObj(Obj);
{$ELSE CriticalSimulateAtomic}
  {$IFDEF ANTI_DEAD_ATOMIC_LOCK}
  d := GetTimeTick;
  TMonitor.Enter(Obj, 5000);
  if GetTimeTick - d >= 5000 then
      RaiseInfo('dead lock');
  {$ELSE ANTI_DEAD_ATOMIC_LOCK}
  TMonitor.Enter(Obj);
  {$ENDIF ANTI_DEAD_ATOMIC_LOCK}
{$ENDIF CriticalSimulateAtomic}
{$ENDIF FPC}
end;

procedure UnLockObject(Obj: TObject);
begin
{$IFDEF FPC}
  _UnLockCriticalObj(Obj);
{$ELSE FPC}
  {$IFDEF CriticalSimulateAtomic}
  _UnLockCriticalObj(Obj);
  {$ELSE CriticalSimulateAtomic}
  TMonitor.Exit(Obj);
  {$ENDIF CriticalSimulateAtomic}
{$ENDIF FPC}
end;

procedure FillPtrByte(const dest: Pointer; Count: NativeInt; const Value: Byte);
var
  d: PByte;
  v: UInt64;
begin
  if Count <= 0 then
      Exit;
  v := Value or (Value shl 8) or (Value shl 16) or (Value shl 24);
  v := v or (v shl 32);
  d := dest;
  while Count >= 8 do
    begin
      PUInt64(d)^ := v;
      dec(Count, 8);
      inc(d, 8);
    end;
  if Count >= 4 then
    begin
      PCardinal(d)^ := PCardinal(@v)^;
      dec(Count, 4);
      inc(d, 4);
    end;
  if Count >= 2 then
    begin
      PWORD(d)^ := PWORD(@v)^;
      dec(Count, 2);
      inc(d, 2);
    end;
  if Count > 0 then
      d^ := Value;
end;

function CompareMemory(const p1, p2: Pointer; Count: NativeInt): Boolean;
var
  b1, b2: PByte;
begin;
  if Count <= 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
  b1 := p1;
  b2 := p2;
  while (Count >= 8) do
    begin
      if PUInt64(b2)^ <> PUInt64(b1)^ then
          Exit;
      dec(Count, 8);
      inc(b2, 8);
      inc(b1, 8);
    end;
  if Count >= 4 then
    begin
      if PCardinal(b2)^ <> PCardinal(b1)^ then
          Exit;
      dec(Count, 4);
      inc(b2, 4);
      inc(b1, 4);
    end;
  if Count >= 2 then
    begin
      if PWORD(b2)^ <> PWORD(b1)^ then
          Exit;
      dec(Count, 2);
      inc(b2, 2);
      inc(b1, 2);
    end;
  if Count > 0 then
    if b2^ <> b1^ then
        Exit;
  Result := True;
end;

procedure CopyPtr(const sour, dest: Pointer; Count: NativeInt);
var
  s, d: PByte;
begin
  if Count <= 0 then
      Exit;
  s := sour;
  d := dest;
  while Count >= 8 do
    begin
      PUInt64(d)^ := PUInt64(s)^;
      dec(Count, 8);
      inc(d, 8);
      inc(s, 8);
    end;
  if Count >= 4 then
    begin
      PCardinal(d)^ := PCardinal(s)^;
      dec(Count, 4);
      inc(d, 4);
      inc(s, 4);
    end;
  if Count >= 2 then
    begin
      PWORD(d)^ := PWORD(s)^;
      dec(Count, 2);
      inc(d, 2);
      inc(s, 2);
    end;
  if Count > 0 then
      d^ := s^;
end;

procedure RaiseInfo(const n: SystemString);
begin
  DoStatus('raise exception: ' + n);
  raise Exception.Create(n);
end;

procedure RaiseInfo(const n: SystemString; const Args: array of const);
begin
  raise Exception.Create(Format(n, Args));
end;

function IsMobile: Boolean;
begin
  case CurrentPlatform of
    epIOS, epIOSSIM, epANDROID32, epANDROID64: Result := True;
    else Result := False;
  end;
end;

var
  Core_RunTime_Tick: TTimeTick;
  Core_Step_Tick: Cardinal;

function GetTimeTick: TTimeTick;
var
  tick: Cardinal;
begin
  CoreTimeTickCritical.Acquire;
  try
    tick := TCoreClassThread.GetTickCount();
    inc(Core_RunTime_Tick, tick - Core_Step_Tick);
    Core_Step_Tick := tick;
    Exit(Core_RunTime_Tick);
  finally
      CoreTimeTickCritical.Release;
  end;
end;

function GetTimeTickCount: TTimeTick;
begin
  Result := GetTimeTick();
end;

function GetCrashTimeTick: TTimeTick;
begin
  Result := $FFFFFFFFFFFFFFFF - GetTimeTick();
end;

{$INCLUDE CoreEndian.inc}

{$IFDEF FPC}

function TCoreClassInterfacedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := 1;
end;

function TCoreClassInterfacedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := 1;
end;

procedure TCoreClassInterfacedObject.AfterConstruction;
begin
end;

procedure TCoreClassInterfacedObject.BeforeDestruction;
begin
end;

{$ELSE}


function TCoreClassInterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TCoreClassInterfacedObject._Release: Integer;
begin
  Result := 1;
end;

procedure TCoreClassInterfacedObject.AfterConstruction;
begin
end;

procedure TCoreClassInterfacedObject.BeforeDestruction;
begin
end;

function TGenericsList<t>.ListData: Pointer;
begin
  // set array pointer
  Arry := TGArry(Pointer(inherited List));
  // @ array
  Result := @Arry;
end;

function TGenericsObjectList<t>.ListData: Pointer;
begin
  // set array pointer
  Arry := TGArry(Pointer(inherited List));
  // @ array
  Result := @Arry;
end;

function TCoreClassList.ListData: PCoreClassPointerList;
begin
  Result := PCoreClassPointerList(inherited ListData);
end;

function TCoreClassListForObj.ListData: PCoreClassForObjectList;
begin
  Result := PCoreClassForObjectList(inherited ListData);
end;

{$ENDIF}

constructor TCoreClassObjectList.Create;
begin
  inherited Create;
  AutoFreeObj := True;
end;

constructor TCoreClassObjectList.Create(AutoFreeObj_: Boolean);
begin
  inherited Create;
  AutoFreeObj := AutoFreeObj_;
end;

destructor TCoreClassObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCoreClassObjectList.Remove(obj: TCoreClassObject);
begin
  inherited Remove(obj);
  if AutoFreeObj then
      DisposeObject(obj);
end;

procedure TCoreClassObjectList.Delete(index: Integer);
begin
  if index >= 0 then
    begin
      if AutoFreeObj then
          disposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TCoreClassObjectList.Clear;
var
  i: Integer;
begin
  if AutoFreeObj then
    for i := 0 to Count - 1 do
        disposeObject(Items[i]);
  inherited Clear;
end;


{$INCLUDE CoreComputeThread.inc}

procedure Nop;
begin
end;

var
  CheckThreadSynchronizeing: Boolean;

procedure CheckThreadSynchronize;
begin
  CheckThreadSynchronize(0);
end;

function CheckThreadSynchronize(Timeout: Integer): Boolean;
begin
  if TCoreClassThread.CurrentThread.ThreadID <> MainThreadID then
    begin
      TCoreClassThread.Sleep(Timeout);
      Result := False;
    end
  else
    begin
      DoStatus;
      if not CheckThreadSynchronizeing then
        begin
          CheckThreadSynchronizeing := True;
          try
              Result := CheckSynchronize(Timeout);
          finally
              CheckThreadSynchronizeing := False;
          end;
        end
      else
        Result := False;
    end;
end;

initialization
  GlobalMemoryHook := True;
  CheckThreadSynchronizeing := False;
  Core_RunTime_Tick := 1000 * 60 * 60 * 24 * 3;
  Core_Step_Tick := TCoreClassThread.GetTickCount();
  InitCriticalLock;
  InitCoreThreadPool(CpuCount * 2);
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
finalization
  FreeCoreThreadPool;
  FreeCriticalLock;
  GlobalMemoryHook := False;
end.

