unit FillCharMultiThreadBenchmark1Unit;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TFillCharThreads = class(TFastcodeMMBenchmark)
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetSpeedWeight: Double; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

uses SysUtils;

type

  TFillCharThread = class(TThread)
     FBenchmark: TFastcodeMMBenchmark;
     procedure Execute; override;
  end;

//Author:            Dennis Kjaer Christensen
//Instructionset(s): IA32, MMX, SSE, SSE2
//Does nothing to align writes. Will run much faster on 16 byte aligned blocks

procedure FillCharSpecial(var Dest; count: Integer; Value: Char);
asm
   test edx,edx
   jle  @Exit2
   cmp  edx,15
   jnbe @CaseElse
   jmp  dword ptr [edx*4+@Case1JmpTable]
 @CaseCount0 :
   ret
 @CaseCount1 :
   mov  [eax],cl
   ret
 @CaseCount2 :
   mov  ch,cl
   mov  [eax],cx
   ret
 @CaseCount3 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cl
   ret
 @CaseCount4 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   ret
 @CaseCount5 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cl
   ret
 @CaseCount6 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   ret
 @CaseCount7 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cl
   ret
 @CaseCount8 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   ret
 @CaseCount9 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cl
   ret
 @CaseCount10 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   ret
 @CaseCount11 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cl
   ret
 @CaseCount12 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   ret
 @CaseCount13 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cl
   ret
 @CaseCount14 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   ret
 @CaseCount15 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cl
   ret
 @CaseCount16 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   ret
 @CaseElse :
   //Need at least 16 bytes here.
   push    esi
   //Broadcast value
   mov     ch, cl
   movd    xmm0, ecx
   pshuflw xmm0, xmm0, 0
   pshufd  xmm0, xmm0, 0
   movdqu  [eax],xmm0
   //Fill the rest
   movdqu  [eax+edx-16],xmm0
   sub     edx,15
   mov     esi,eax
   //16 byte alignment?
   and     esi,$F
   test    esi,esi
   jnz     @UnAlign
   xor     esi,esi
 @Repeat1 :
   movdqa  [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat1
   jmp     @Exit1
 @UnAlign :
   xor     esi,esi
 @Repeat4 :
   movdqu  [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat4
 @Exit1 :
   pop     esi
 @Exit2 :
   ret

@Case1JmpTable:
 dd @CaseCount0
 dd @CaseCount1
 dd @CaseCount2
 dd @CaseCount3
 dd @CaseCount4
 dd @CaseCount5
 dd @CaseCount6
 dd @CaseCount7
 dd @CaseCount8
 dd @CaseCount9
 dd @CaseCount10
 dd @CaseCount11
 dd @CaseCount12
 dd @CaseCount13
 dd @CaseCount14
 dd @CaseCount15

end;

//Allocate a block, fill it with SSE2 instruction without aligning, free block,
//measure amount of allocated memory

procedure TFillCharThread.Execute;
var
 P1, P2, P3, P4, P5 : Pointer; //Need some pointers to get proper alignment distribution
 RunNo, FillRunNo : Integer;
const
 RUNNOMAX : Integer = 33;
 FILLRUNNOMAX : Integer = 3;
 SIZE1 : Integer = 300000; //300 kB
 SIZE2 : Integer = 650000; //650 kB
 SIZE3 : Integer = 900000; //900 kB
 SIZE4 : Integer = 1250000;//1.25 MB
 SIZE5 : Integer = 2500000;//2.5 MB

begin
 for RunNo := 1 to RUNNOMAX do
  begin
   GetMem(P1, SIZE1);
   GetMem(P2, SIZE2);
   GetMem(P3, SIZE3);
   GetMem(P4, SIZE4);
   GetMem(P5, SIZE5);
   //Compete for cache sets
   //Repeat to make sure Fill is bottleneck
   for FillRunNo := 1 to FILLRUNNOMAX do
    begin
     FillCharSpecial(P1^, SIZE1, 'A');
     FillCharSpecial(P2^, SIZE2, 'B');
     FillCharSpecial(P3^, SIZE3, 'C');
     FillCharSpecial(P4^, SIZE4, 'D');
     FillCharSpecial(P5^, SIZE5, 'E');
    end;
   FreeMem(P1);
   FreeMem(P2);
   FreeMem(P3);
   FreeMem(P4);
   FreeMem(P5);
  end;
 FBenchmark.UpdateUsageStatistics;
end;

class function TFillCharThreads.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark that measures write speed to allocated block - gives bonus for 16 byte alignment '
          + 'Measures memory usage after all blocks have been freed '
          + 'Benchmark submitted by Dennis Kjaer Christensen.';
end;

class function TFillCharThreads.GetBenchmarkName: string;
begin
  Result := 'FillCharMultiThread';
end;

class function TFillCharThreads.GetCategory: TBenchmarkCategory;
begin
  Result := bmMemoryAccessSpeed;
end;

class function TFillCharThreads.GetSpeedWeight: Double;
begin
  Result := 0.5;
end;

procedure TFillCharThreads.RunBenchmark;
var
 FillCharThread1 : TFillCharThread;
 FillCharThread2 : TFillCharThread;

begin
  inherited;
  FillCharThread1 := TFillCharThread.Create(True);
  FillCharThread2 := TFillCharThread.Create(True);
  FillCharThread1.FreeOnTerminate := False;
  FillCharThread2.FreeOnTerminate := False;
  FillCharThread1.FBenchmark := Self;
  FillCharThread2.FBenchmark := Self;
  FillCharThread1.Resume;
  FillCharThread2.Resume;
  FillCharThread1.WaitFor;
  FillCharThread2.WaitFor;
  FillCharThread1.Free;
  FillCharThread2.Free;
end;

end.
