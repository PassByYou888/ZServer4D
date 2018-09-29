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
  {$STACKFRAMES OFF}

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
  {$DEFINE CriticalSimulateAtomic}
  {$DEFINE release}
  {$UNDEF debug}
  {$DEFINE INLINE_ASM}
  {$R-} { range check }
  {$O+} { optimization }
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
  // if CriticalSimulateAtomic defined so performance to be reduced
  // {$DEFINE CriticalSimulateAtomic}

  {$IFDEF Linux}
    {$DEFINE CriticalSimulateAtomic}
  {$ENDIF}

  {$IFDEF release}
    {$DEFINE INLINE_ASM}
    {$R-} { range check }
    {$I-} { Input output checking }
    {$IFDEF MSWINDOWS}
      {$O+} { optimization }
    {$ELSE MSWINDOWS}
      {$O-} { optimization }
    {$ENDIF MSWINDOWS}
  {$ELSE}
    {$UNDEF INLINE_ASM}
    {$R-} { range check }
    {$I-} { Input output checking }
    {$O-} { optimization }
  {$ENDIF}

  {$IF Defined(Android) or Defined(IOS)}
    {$DEFINE NatTunnel_On_Indy}
    {$DEFINE HostQuery_On_Indy}
  {$ELSE}
    // xNat Physics interface
    // {$DEFINE NatTunnel_On_ICS}
    {$DEFINE NatTunnel_On_CrossSocket}
    // {$DEFINE NatTunnel_On_DIOCP}
    // {$DEFINE NatTunnel_On_Indy}


    // HostQuery Physics interface
    // {$DEFINE HostQuery_On_ICS}
    {$DEFINE HostQuery_On_CrossSocket}
    // {$DEFINE HostQuery_On_DIOCP}
    // {$DEFINE HostQuery_On_Indy}
  {$IFEND}
{$ENDIF FPC}

{$WARNINGS OFF}
{$HINTS OFF}
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
