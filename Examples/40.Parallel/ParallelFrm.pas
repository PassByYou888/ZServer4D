unit ParallelFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  System.Threading,
  CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, Vcl.ExtCtrls;

type
  TParallelForm = class(TForm)
    ParaAddButton: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    ParaLockButton: TButton;
    Memo3: TMemo;
    Para19937Button: TButton;
    Memo4: TMemo;
    ParallelTRandomButton: TButton;
    Memo5: TMemo;
    ParaDelphiRandomButton: TButton;
    Memo6: TMemo;
    ComputeThreadButton: TButton;
    Timer1: TTimer;
    StateLabel: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ParaAddButtonClick(Sender: TObject);
    procedure ParaLockButtonClick(Sender: TObject);
    procedure Para19937ButtonClick(Sender: TObject);
    procedure ParallelTRandomButtonClick(Sender: TObject);
    procedure ParaDelphiRandomButtonClick(Sender: TObject);
    procedure ComputeThreadButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure Backcall_DoStatus(Text_: SystemString; const ID: Integer);
  public
    atomNum: TAtomInt64;     // 以TAtomXXX命名的结构支持，多线程和并行程序使用，与普通变量差异在于：使用前需要初始化，不用时需要释放
    atomString: TAtomString; // 以TAtomXXX命名的结构支持，多线程和并行程序使用，与普通变量差异在于：使用前需要初始化，不用时需要释放
  end;

var
  ParallelForm: TParallelForm;

implementation

{$R *.dfm}


procedure TParallelForm.FormDestroy(Sender: TObject);
begin
  atomNum.Free;
  atomString.Free;
end;

procedure TParallelForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, Backcall_DoStatus);

  // WorkInParallelCore在非ide环境下才会打开，在Debug模式下是关闭的，我们将它强制打开
  WorkInParallelCore.V := True;

  atomNum := TAtomInt64.Create(0);      // 以TAtomXXX命名的结构支持，多线程和并行程序使用，与普通变量差异在于：使用前需要初始化，不用时需要释放
  atomString := TAtomString.Create(''); // 以TAtomXXX命名的结构支持，多线程和并行程序使用，与普通变量差异在于：使用前需要初始化，不用时需要释放
end;

procedure TParallelForm.ParaAddButtonClick(Sender: TObject);
begin
  atomNum.Value := 0;
  DelphiParallelFor(0, 100000 - 1, procedure(pass: Integer)
    begin
      // 操作方法1，锁住后取值，做一次inc操作，再解锁
      // 1,2两种操作方法可以兼容使用
      atomNum.UnLock(atomNum.Lock + 1);
      // 操作方法2，锁住后取值，做一次inc操作，再解锁
      // 1,2两种操作方法可以兼容使用
      inc(atomNum.LockP^);
      atomNum.UnLock();
      // 同样，我么也可以写成
      atomNum.Lock;
      atomNum.p^ := atomNum.p^ + 1;
      atomNum.UnLock();

      // 要避免的常识性程序错误
      // 以下代码的工作流程如下
      // 1,锁住后取值再解锁
      // 2,值+1
      // 3,锁住后赋值再解锁
      // atomNum.V := atomNum.V + 1;
    end);
  DelphiParallelFor(0, 100000 - 1, procedure(pass: Integer)
    begin
      // 原子api的操作方法与锁方法不兼容，不能同时操作
      // 操作方法3，通过原子api直接操作指针值，原子api只能支持整数操作
      AtomInc(atomNum.p^);
    end);
  DelphiParallelFor(0, 100000 - 1, procedure(pass: Integer)
    begin
      // 原子api的操作方法与锁方法不兼容，不能同时操作
      // 操作方法4，通过原子api直接操作指针值，原子api只能支持整数操作
      AtomicIncrement(atomNum.p^);
    end);
  DoStatus(umlIntToStr(atomNum.Value));
end;

procedure TParallelForm.ParaLockButtonClick(Sender: TObject);
begin
  atomString.Value := '';
  DelphiParallelFor(0, 10000 - 1, procedure(pass: Integer)
    begin
      // 锁住后赋值再解锁
      atomString.Value := umlIntToStr(pass);

      // 锁住，取值，如果值是'55'，处理，解锁
      atomString.Lock;
      if atomString.p^ = '55' then
          DoStatus(atomString.p^);
      atomString.UnLock();

      // 锁住后取值再解锁，如果值是'99'
      if atomString.Value = '99' then
          DoStatus('99');
    end);
  DoStatus();
end;

procedure TParallelForm.Para19937ButtonClick(Sender: TObject);
begin
  atomString.Value := '';
  DelphiParallelFor(0, CpuCount - 1, procedure(pass: Integer)
    var
      i: Integer;
      n: U_String;
      Buff: array of Integer;
      num: Integer;
    begin
      SetMT19937Seed(0);
      n := '';

      for i := 1 to 20 do
        begin
          // 一次产生一个随机数
          // MT19937Rand32每次调用时会找线程捆绑的MT19937实例，线程在这里会发生短暂卡顿
          // 高频率调用应该采用TRandom类来代替MT19937Rand32
          num := MT19937Rand32(10);

          if n.L > 0 then
              n.Append(#32);
          n.Append(umlIntToStr(num));
        end;

      atomString.Lock;
      atomString.p^ := atomString.p^ + Format('线程[%d]随机数序列: %s'#13#10, [TCompute.CurrentThread.ThreadID, n.Text]);
      atomString.UnLock;
    end);
  DoStatus(atomString.V);
end;

procedure TParallelForm.ParallelTRandomButtonClick(Sender: TObject);
begin
  atomString.Value := '';
  DelphiParallelFor(0, CpuCount - 1, procedure(pass: Integer)
    var
      i: Integer;
      n: U_String;
      rnd: TRandom;
      num: Integer;
    begin
      rnd := TRandom.Create;
      rnd.seed := 0;
      n := '';

      for i := 1 to 20 do
        begin
          // 通过TRandom类产生随机数，通过TRandom类产生随机数不会对线程造成卡顿，适用于追求极致性能的并发程序
          num := rnd.Rand32(10);

          if n.L > 0 then
              n.Append(#32);
          n.Append(umlIntToStr(num));
        end;

      atomString.Lock;
      atomString.p^ := atomString.p^ + Format('线程[%d]随机数序列: %s'#13#10, [TCompute.CurrentThread.ThreadID, n.Text]);
      atomString.UnLock;

      rnd.Free;
    end);
  DoStatus(atomString.V);
end;

procedure TParallelForm.ParaDelphiRandomButtonClick(Sender: TObject);
begin
  if not MT19937CoreToDelphi then
    begin
      DoStatus('未打开InstallMT19937CoreToDelphi编译选项' + #13#10 +
        '请编辑文件 zDefine.inc'#13#10 +
        '打开编译选项: InstallMT19937CoreToDelphi'#13#10 +
        '打开编译选项: MT19937SeedOnTComputeThreadIs0'#13#10);
      exit;
    end;
  atomString.Value := '';
  TParallel.For(0, CpuCount - 1, procedure(pass: Integer)
    var
      i: Integer;
      n: U_String;
      num: Integer;
    begin
      // 通过delphi自带Random函数产生线程统一性随机数，使用该功能前，必须初始化
      SetMT19937Seed(0);

      n := '';
      for i := 1 to 20 do
        begin
          // 通过delphi自带Random函数产生线程统一性随机数
          num := Random(10);
          if n.L > 0 then
              n.Append(#32);
          n.Append(umlIntToStr(num));
        end;

      atomString.Lock;
      atomString.p^ := atomString.p^ + Format('线程[%d]随机数序列: %s'#13#10, [TCompute.CurrentThread.ThreadID, n.Text]);
      atomString.UnLock;
    end);
  DoStatus(atomString.V);
end;

procedure TParallelForm.ComputeThreadButtonClick(Sender: TObject);
var
  MyThreadNum: TAtomInteger;
  i: Integer;
begin
  MyThreadNum := TAtomInteger.Create(0);

  // 我们开10个TCompute线程
  for i := 0 to 10 - 1 do
    begin
      // MyThreadNum是我们自定义的线程计数器，创建线程就让它+1
      MyThreadNum.UnLock(MyThreadNum.Lock + 1);
      // RunP_NP = Run procedure no parameter缩写, 该方法提供了无参数的匿名线程
      TCompute.RunP_NP(procedure
        var
          delTick: Integer;
        begin
          // TCompute.Sync 是一种相对更快捷的同步方式,等同于 TThread.Synchronize
          TCompute.Sync(procedure
            begin
              TCompute.Sleep(umlRandomRange(10, 200));
            end);
          // 打乱随机数种子
          MT19937Randomize;
          // DoStatusNoLn=DoStatus No line，不打印换行，不给参数是打印并清空当前行，它是线程安全的
          DoStatusNoLn;
          DoStatusNoLn('线程启动');
          // 随机延迟1-5秒
          delTick := umlRandomRange(1000, 5000);
          TCompute.Sleep(delTick);
          // doStatus 是状态打印支持方法，它是线程安全的
          DoStatusNoLn(' 模拟延迟 %d ms', [delTick]);
          // MyThreadNum是我们自定义的线程计数器，线程结束就让它-1
          MyThreadNum.UnLock(MyThreadNum.Lock - 1);
          DoStatusNoLn(' 完成.');
          DoStatusNoLn;
        end);
    end;

  // 简单实现等待这10个TCompute线程计算结束
  while MyThreadNum.V > 0 do
    begin
      // 如果在子线程中等TCompute，直接写成 TThread.Sleep(10)

      // 如果在主线程等TCompute可以用如下方法
      CheckThreadSynchronize(10);
      Application.ProcessMessages;
    end;

  MyThreadNum.Free;
  DoStatus('所有线程已结束');
end;

procedure TParallelForm.Backcall_DoStatus(Text_: SystemString; const ID: Integer);
begin
  Memo6.Lines.Add(Text_);
end;

procedure TParallelForm.Timer1Timer(Sender: TObject);
begin
  StateLabel.Caption := TCompute.State;
  DoStatus();
end;

end.
