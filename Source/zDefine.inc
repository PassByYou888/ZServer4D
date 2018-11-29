{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
{$IFDEF FPC}
  {$MODE objfpc}
  {$MODESWITCH AdvancedRecords}
  {$NOTES OFF}
  {$STACKFRAMES ON}

  {$DEFINE LITTLE_ENDIAN}
  {$UNDEF BIG_ENDIAN}
  {$IFDEF FPC_BIG_ENDIAN}
    {$UNDEF LITTLE_ENDIAN}
    {$DEFINE BIG_ENDIAN}
  {$ENDIF}

  {$UNDEF FirstCharInZero}

  {$UNDEF Delphi}
  {$UNDEF FastMD5}
  {$DEFINE parallel}
  {$DEFINE OptimizationMemoryStreamMD5}

  // used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
  // CriticalSimulateAtomic defined so performance to be reduced
  {$DEFINE CriticalSimulateAtomic}

  // used soft Simulate Critical(ring)
  // SoftCritical defined so performance to be reduced
  // {$DEFINE SoftCritical}

  // {$DEFINE ANTI_DEAD_ATOMIC_LOCK}

  // Using Sequential Packet Model will consume more memory and reduce IO performance,
  // it can memory communication status and facilitate us to return to the original scene.
  // {$DEFINE SEQUENCE_PACKET}

  {$DEFINE release}
  {$UNDEF debug}
  {$DEFINE INLINE_ASM}
  {$R-} { range check }
  {$O-} { optimization }
{$ELSE FPC}
  {$DEFINE LITTLE_ENDIAN}
  {$UNDEF BIG_ENDIAN}

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
  // CriticalSimulateAtomic defined so performance to be reduced
  // {$DEFINE CriticalSimulateAtomic}

  // used soft Simulate Critical(ring)
  // SoftCritical defined so performance to be reduced
  // {$DEFINE SoftCritical}

  // {$DEFINE ANTI_DEAD_ATOMIC_LOCK}

  // Using Sequential Packet Model will consume more memory and reduce IO performance,
  // it can memory communication status and facilitate us to return to the original scene.
  // {$DEFINE SEQUENCE_PACKET}

  {$IFDEF release}
    {$DEFINE INLINE_ASM}
    {$R-} { range check }
    {$I-} { Input output checking }
    {$O+} { optimization }
  {$ELSE}
    {$UNDEF INLINE_ASM}
    {$R+} { range check }
    {$I+} { Input output checking }
    {$O-} { optimization }
  {$ENDIF}

  {$IF Defined(Android) or Defined(IOS)}
    {$DEFINE PhysicsIO_On_Indy}
  {$ELSE}
    // PhysicsIO interface
    // {$DEFINE PhysicsIO_On_ICS}
    {$DEFINE PhysicsIO_On_CrossSocket}
    // {$DEFINE PhysicsIO_On_DIOCP}
    // {$DEFINE PhysicsIO_On_Indy}
    // {$DEFINE PhysicsIO_On_Synapse}
  {$IFEND}
{$ENDIF FPC}

{$WARNINGS ON}
{$HINTS ON}
{$H+} { long string }
{$A+} { Word Align Data }
{$Q-} { Overflow checking }
{$B-} { Complete boolean evaluation }
{$J+} { Writeable typed constants }

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

{$IFDEF BIG_ENDIAN}
  {$MESSAGE FATAL 'Big-endian system not supported'}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE RangeCheck}
{$ENDIF}

{$IFOPT Q+}
  {$DEFINE OverflowCheck}
{$ENDIF}

(*
  {$IFDEF FPC}
  {$ELSE FPC}
  {$ENDIF FPC}

  {$IFDEF FPC}
  {$ENDIF FPC}

  {$IFDEF FPC}@{$ENDIF FPC}

  {$IFNDEF FPC}
  {$ENDIF FPC}

  {$IFDEF parallel}
  {$ELSE parallel}
  {$ENDIF parallel}

  {$IFNDEF parallel}
  {$ENDIF parallel}

  {$Region 'RegionName'}
  {$EndRegion 'RegionName'}

  {$IFDEF OverflowCheck}{$Q-}{$ENDIF}
  {$IFDEF OverflowCheck}{$Q+}{$ENDIF}

  {$IFDEF RangeCheck}{$R-}{$ENDIF}
  {$IFDEF RangeCheck}{$R+}{$ENDIF}
*)



