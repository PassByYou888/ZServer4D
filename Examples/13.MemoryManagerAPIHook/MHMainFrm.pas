unit MHMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  DoStatusIO, PascalStrings, CoreClasses, UnicodeMixedLib, ListEngine;

type
  TMHMainForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  end;

var
  MHMainForm: TMHMainForm;

implementation

{$R *.dfm}


uses MH_1, MH_2, MH_3, MH;

procedure TMHMainForm.Button1Click(Sender: TObject);

  procedure leakproc(x, m: Integer);
  begin
    GetMemory(x);
    if x > m then
        leakproc(x - 1, m);
  end;

begin
  MH.BeginMemoryHook_1;
  leakproc(100, 98);
  MH.EndMemoryHook_1;

  // 这里我们会发现泄漏
  DoStatus('leakproc函数分配了 %d 字节的内存', [MH.GetHookMemorySize_1]);

  MH.GetHookPtrList_1.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      DoStatus('泄漏的地址:0x%s', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2)]);
      DoStatus(NPtr, uData, 80);

      // 现在我们可以直接释放该地址
      Dispose(NPtr);

      DoStatus('已成功释放 地址:0x%s 占用了 %d 字节内存', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2), uData]);
    end);
end;

procedure TMHMainForm.Button2Click(Sender: TObject);
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    s2: string;
    s3: TPascalString;
    obj: TObject;
  end;

var
  p: PMyRec;
begin
  MH.BeginMemoryHook_1;
  new(p);
  p^.s1 := #7#8#9;
  p^.s2 := #$20#$20#$20#$20#$20#$20#$20#$20#$20#$20#$20#$20;
  p^.s3.Text := #1#2#3#4#5#6;
  p^.obj := TObject.Create;
  MH.EndMemoryHook_1;

  // 这里我们会发现泄漏
  DoStatus('TMyRec总分分配了 %d 次内存，占用 %d 字节空间，', [MH.GetHookPtrList_1.Count, MH.GetHookMemorySize_1]);

  MH.GetHookPtrList_1.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      DoStatus('泄漏的地址:0x%s', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2)]);
      DoStatus(NPtr, uData, 80);

      // 现在我们可以直接释放该地址
      FreeMem(NPtr);

      DoStatus('已成功释放 地址:0x%s 占用了 %d 字节内存', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2), uData]);
    end);
end;

procedure TMHMainForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText)
end;

procedure TMHMainForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
end;

procedure TMHMainForm.Button3Click(Sender: TObject);
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    p: PMyRec;
  end;

var
  p: PMyRec;
  i: Integer;
begin
  // 100万次的反复勾，反复释放
  // 这种场景情况，可以用于统计你的程序开销，记录内存消耗
  for i := 0 to 100 * 10000 do
    begin
      MH_2.BeginMemoryHook(4);
      new(p);
      p^.s1 := '12345';
      new(p^.p);
      p^.p^.s1 := '54321';
      MH_2.EndMemoryHook;

      MH_2.HookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
        begin
          // 现在我们可以释放该地址
          FreeMem(NPtr);
        end);
    end;
end;

procedure TMHMainForm.Button4Click(Sender: TObject);
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    p: PMyRec;
  end;

var
  p: PMyRec;
  i: Integer;
  hl: TPointerHashNativeUIntList;
begin
  // 200万次的大批量记录内存申请，最后一次性释放
  // 这种场景情况，可以用于批量释放泄漏的内存

  // 我们内建20万个Hash数组进行存储
  // BeginMemoryHook的参数越大，面对对大批量存储的高频率记录性能就越好，但也越消耗内存
  MH_3.BeginMemoryHook(200000);

  for i := 0 to 200 * 10000 do
    begin
      new(p);
      new(p^.p);
      // 模拟字符串赋值，高频率触发Realloc调用
      p^.s1 := '111111111111111';
      p^.s1 := '1111111111111111111111111111111111';
      p^.s1 := '11111111111111111111111111111111111111111111111111111111111111';
      p^.p^.s1 := '1';
      p^.p^.s1 := '11111111111111111111';
      p^.p^.s1 := '1111111111111111111111111111111111111';
      p^.p^.s1 := '11111111111111111111111111111111111111111111111111111111111111111111111111';

      if i mod 99999 = 0 then
        begin
          // 这里是迭代调用，我们不记录，将MH_3.MemoryHooked设置为False即可
          MH_3.MemoryHooked.V := False;
          Button1Click(nil);
          Application.ProcessMessages;
          // 继续记录内存申请
          MH_3.MemoryHooked.V := True;
        end;
    end;
  MH_3.EndMemoryHook;

  DoStatus('总共内存分配 %d 次 占用 %s 空间，地址跨度为：%s ', [MH_3.HookPtrList.Count, umlSizeToStr(MH_3.GetHookMemorySize).Text,
    umlSizeToStr(NativeUInt(MH_3.GetHookMemoryMaximumPtr) - NativeUInt(MH_3.GetHookMemoryMinimizePtr)).Text]);

  MH_3.HookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      // 现在我们可以释放该地址
      FreeMem(NPtr);
    end);
  MH_3.HookPtrList.PrintHashReport;
  MH_3.HookPtrList.SetHashBlockCount(0);
end;

procedure TMHMainForm.Button5Click(Sender: TObject);

var
  s: string;
  sptr: PString;
begin
  MH_1.BeginMemoryHook(16);

  Memo1.Lines.Add('123'); // 因为没有前后文参考，这里的Realloc和GetMem均不会被记录
  s := '12345';           // 因为s字符串在调用开始时已经初始化，没有前后文参考，这里的Realloc不会被记录

  new(sptr); // 这里会记录sptr的GetMem地址
  sptr^ := '123';
  sptr^ := '123456789'; // 在发生了对sptr的Realloc时，mh会寻找前后文，这里符合了realloc的记录条件，mh将记录它，并且在后续释放

  // mh支持控件创建和释放
  // mh不支持tform窗口释放，因为tform窗口会注册全局参数，mh在释放了tform以后，某些回调进来没有地址就会报错
  TButton.Create(Self).Free;

  MH_1.EndMemoryHook;

  MH_1.HookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      // 现在我们可以释放该地址
      DoStatus(NPtr, uData, 80);
      FreeMem(NPtr);
    end);

  MH_1.HookPtrList.SetHashBlockCount(0);
end;

end.
