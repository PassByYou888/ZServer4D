{$IFDEF FPC}

{$MODE objfpc}
{$MODESWITCH AdvancedRecords}
{$NOTES OFF}

{$ELSE}

{$DEFINE DELPHI}

{$IFDEF ANDROID}
{$DEFINE FirstCharInZero}
{$ENDIF}

{$IFDEF IOS}
{$DEFINE FirstCharInZero}
{$ENDIF}

{$ENDIF}

{$WARNINGS OFF}
{$HINTS OFF}
{$H+} {long string}
{$R-} {range check}
{$A+} {Word Align Data}

(*
{$IFDEF FPC}
{$ELSE}
{$ENDIF}

{$IFNDEF FPC}
{$ENDIF}
*)

{$define parallel}

{$IFDEF FPC}
{$ASMMODE intel}
{$ENDIF}

{$IFDEF FPC_BIG_ENDIAN}
{$DEFINE BIG_ENDIAN}
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

