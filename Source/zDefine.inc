{$IFDEF FPC}

{$MODE objfpc}
{$MODESWITCH AdvancedRecords}
{$NOTES OFF}

{$ASMMODE intel}

{$IFDEF FPC_BIG_ENDIAN}
{$DEFINE BIG_ENDIAN}
{$ENDIF}

{$UNDEF Delphi}
{$UNDEF FastMD5}
{$DEFINE parallel}
{$DEFINE OptimizationMemoryStreamMD5}
{$DEFINE CriticalSimulateAtomic}

{$ELSE}

{$IFDEF ANDROID}
{$DEFINE FirstCharInZero}
{$ENDIF}

{$IFDEF IOS}
{$DEFINE FirstCharInZero}
{$ENDIF}

{$DEFINE Delphi}
{$DEFINE FastMD5}
{$DEFINE parallel}
{$DEFINE OptimizationMemoryStreamMD5}

// used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
// if CriticalSimulateAtomic defined so performance to be reduced
{.$DEFINE CriticalSimulateAtomic}

{$IFDEF Linux}
{$DEFINE CriticalSimulateAtomic}
{$UNDEF parallel}
{$ENDIF}

{$ENDIF}

{$WARNINGS OFF}
{$HINTS OFF}

{$H+} { long string }
{$R-} { range check }
{$A+} { Word Align Data }

(*
  {$IFDEF FPC}
  {$ELSE }
  {$ENDIF FPC}

  {$IFNDEF FPC}
  {$ENDIF FPC}
*)

{$IFDEF release}
{$DEFINE INLINE_ASM}
{$ELSE}
{$UNDEF INLINE_ASM}
{$ENDIF}

{$UNDEF CPU64}
{$IFDEF CPU64BITS}
{$DEFINE CPU64}
{$ELSE}
{$IFDEF CPUX64}
{$DEFINE CPU64}
{$ENDIF}
{$ENDIF}
{$IFNDEF CPU64}
{$DEFINE CPU32}
{$ENDIF}


