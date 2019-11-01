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

{$IFDEF FPC}
  {$IFDEF FPC_DELPHI_MODE}
    {$MODE delphi}
  {$ELSE FPC_DELPHI_MODE}
    {$MODE objfpc}
  {$ENDIF FPC_DELPHI_MODE}

  {$MODESWITCH AdvancedRecords}
  {$MODESWITCH NestedProcVars}
  {$NOTES OFF}
  {$STACKFRAMES OFF}
  {$COPERATORS ON}
  {$GOTO ON}
  {$INLINE ON}

  {$DEFINE LITTLE_ENDIAN}
  {$UNDEF BIG_ENDIAN}
  {$IFDEF FPC_BIG_ENDIAN}
    {$UNDEF LITTLE_ENDIAN}
    {$DEFINE BIG_ENDIAN}
  {$ENDIF}

  {$UNDEF FirstCharInZero}

  {$UNDEF Delphi}

  // fast MD5 only delphi supported, https://github.com/PassByYou888/FastMD5
  {$UNDEF FastMD5}

  // stream is MemoryStream64 or MemoryStream, usage fastMD5 or PurePascal MD5
  // be associate api: UnicodeMixedLib.umlStreamMD5, Fast_MD5.FastMD5
  {$DEFINE OptimizationMemoryStreamMD5}

  // multi thread parallel switch.
  {$DEFINE parallel}

  // automated loading common AI data sets on boot-time
  {$DEFINE Z_AI_Dataset_Build_In}

  // With SMALL_RASTER_FONT_Build_In and LARGE_RASTER_FONT_Build_In, boot-time memory usage increase by 100M-200M and start-up time to be delay 100ms
  {$DEFINE SMALL_RASTER_FONT_Build_In}
  // {$DEFINE LARGE_RASTER_FONT_Build_In}

  // ZDB_BACKUP is automatically made and replica caching is enabled.
  // usage ZDB_BACKUP so slows the open of large size ZDB file, after time, but does is high performance.
  // {$DEFINE ZDB_BACKUP}

  // used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
  // CriticalSimulateAtomic defined so performance to be reduced
  {$DEFINE CriticalSimulateAtomic}

  // used soft Simulate Critical(ring)
  // SoftCritical defined so performance to be reduced
  // {$DEFINE SoftCritical}
  // {$DEFINE ANTI_DEAD_ATOMIC_LOCK}

  {$DEFINE release}
  {$UNDEF debug}
  {$DEFINE INLINE_ASM}
  {$R-} { range check }
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

  // fast MD5 only delphi supported, https://github.com/PassByYou888/FastMD5
  {$DEFINE FastMD5}

  // stream is MemoryStream64 or MemoryStream, usage fastMD5 or PurePascal MD5
  // be associate api: UnicodeMixedLib.umlStreamMD5, Fast_MD5.FastMD5
  {$DEFINE OptimizationMemoryStreamMD5}

  // multi thread parallel switch.
  {$DEFINE parallel}

  // paper: Mersenne Twister: A 623-dimensionallyequidistributed uniformpseudorandom number generator
  // Using this paper replace of Delphi Random() and Randomize() function
  {$DEFINE InstallMT19937CoreToDelphi}

  // automated loading common AI data sets on boot-time
  // {$DEFINE Z_AI_Dataset_Build_In}

  // With SMALL_RASTER_FONT_Build_In and LARGE_RASTER_FONT_Build_In, boot-time memory usage increase by 100M-200M and start-up time to be delay 100ms
  // {$DEFINE SMALL_RASTER_FONT_Build_In}
  // {$DEFINE LARGE_RASTER_FONT_Build_In}

  // ZDB_BACKUP is automatically made and replica caching is enabled.
  // usage ZDB_BACKUP so slows the open of large size ZDB file, after time, but does is high performance.
  // {$DEFINE ZDB_BACKUP}

  // used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
  // CriticalSimulateAtomic defined so performance to be reduced
  // {$DEFINE CriticalSimulateAtomic}

  // used soft Simulate Critical(ring)
  // SoftCritical defined so performance to be reduced
  // {$DEFINE SoftCritical}
  // {$DEFINE ANTI_DEAD_ATOMIC_LOCK}

  {$IFDEF release}
    {$DEFINE INLINE_ASM}
    {$R-} { range check }
    {$I-} { Input output checking }
    {$IF Defined(Android) or Defined(IOS)}
      {$O-} { close optimization }
    {$ELSE}
      {$O+} { open optimization }
      {$INLINE AUTO} { inline }
    {$IFEND}
  {$ELSE}
    {$UNDEF INLINE_ASM}
    {$O-} { close optimization }
    {$R-} { range check }
    {$I-} { Input output checking }
    {$D+} { debug information }
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

  {$X+} { Extended syntax }
  {$Z1} { Minimum enum size }
{$ENDIF FPC}

{$WARNINGS OFF}
{$HINTS OFF}
{$C+} { Assertions }
{$M-} { Run-Time Type Information }
{$H+} { long string }
{$A+} { Word Align Data }
{$Q-} { Overflow checking }
{$B-} { Complete boolean evaluation }
{$J-} { Writeable typed constants }

(*
  Pointer math is simply treating any given typed pointer in some narrow,
  instances as a scaled ordinal where you can perform simple arithmetic operations directly on the pointer variable.
*)
{$POINTERMATH OFF}

{$UNDEF CPU64}

{$IFDEF CPU64BITS}
  {$DEFINE CPU64}
{$ELSE CPU64BITS}
  {$IFDEF CPUX64}
    {$DEFINE CPU64}
  {$ENDIF CPUX64}
{$ENDIF CPU64BITS}

{$IFNDEF CPU64}
  {$DEFINE CPU32}
{$ENDIF CPU64}

{$IFDEF BIG_ENDIAN}
  {$MESSAGE FATAL 'Big-endian system not supported'}
{$ENDIF BIG_ENDIAN}

{$IFOPT R+}
  {$DEFINE RangeCheck}
{$ENDIF}

{$IFOPT Q+}
  {$DEFINE OverflowCheck}
{$ENDIF}
