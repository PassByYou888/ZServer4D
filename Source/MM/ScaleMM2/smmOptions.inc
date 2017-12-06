{.$DEFINE SCALEMM_DEBUG}    //much(!) slower but better debugging (no inline functions etc)
{.$DEFINE IntelVTune}       //debug info in release build
{$DEFINE MMSharingEnabled}  //Share MM with dlls
{.$DEFINE SCALEMM_MAGICTEST} //extra safety checks with low overhead

{$IFDEF SCALEMM_DEBUG}
  {$OPTIMIZATION   OFF}
  {$STACKFRAMES    ON}
  {$ASSERTIONS     ON}
  {$DEBUGINFO      ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS    ON}
  {$define SCALEMM_MAGICTEST}
  {$define SCALEMM_FILLFREEMEM}
  {.$define SCALEMM_OUTPUTSTRING}
  {$define PURE_PASCAL}
{$ELSE}      // default "release" mode, much faster!
  {$OPTIMIZATION   ON}         // 235% faster!
  {$STACKFRAMES    OFF}        // 12% faster
  {$ASSERTIONS     OFF}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS    OFF}
  {$if CompilerVersion >= 17}  // Delphi 2005 or newer
    {$define HASINLINE}
  {$ifend}
  {$define INLINEGOWN}
//  {$D-}
//  {$L-}
{$ENDIF}

//use this to get line debug info in Intel vTune
{$IFDEF IntelVTune}
  {$undef HASINLINE}
  {$undef INLINEGOWN}
  {$OPTIMIZATION   OFF}
  {$STACKFRAMES    ON}
  {$DEBUGINFO      ON}
  {$LOCALSYMBOLS   ON}
{$ENDIF}

{$IFDEF SCALEMM_MAGICTEST}
  {$ASSERTIONS     ON}
{$ENDIF}

// other posible defines:
{.$DEFINE PURE_PASCAL}    // no assembly, pure delphi code
{$DEFINE Align8Bytes}     // 8 byte aligned header, needed for OmniThreadLibrary etc
{.$DEFINE Align16Bytes}   // 16 byte aligned header, so some more overhead

{$IFDEF CPUX64}
  {$DEFINE PURE_PASCAL}    // no assembly, pure delphi code
  {$DEFINE Align16Bytes}   // 16 byte aligned header, so some more overhead
  {$undef INLINEGOWN}
{$ENDIF}

{$IFDEF Align16Bytes}
  {$UNDEF Align8Bytes}   // 16 byte aligned header, so some more overhead
{$ENDIF}

{$IFDEF DEBUG}
  {.$DEFINE SCALEMM_FILLFREEMEM}
{$ENDIF}

{$if CompilerVersion >= 17}
  {$define USEMEMMANAGEREX}
{$ifend}

{$IFnDEF CPUX64}
  /// internal GetThreadMemManager function is 2% faster with an injected offset (only 32bit)
  {$DEFINE SCALE_INJECT_OFFSET}
{$ENDIF}

// inlined TLS access
// - injected offset + GetThreadMemManager call can be slower than offset loading
{$ifdef INLINEGOWN}
  {$ifndef HASINLINE} // inlined Getmem/Freemem will call GetThreadMemManager
    {$UNDEF SCALE_INJECT_OFFSET}
  {$endif}
{$endif}

{$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types in Delphi 7}
{$WARN UNSAFE_CODE OFF} {Disable warning for unsafe code in Delphi 7}

{$IFDEF 4GB}
	{$SetPEFlags $0020 (*IMAGE_FILE_LARGE_ADDRESS_AWARE*) }
{$ENDIF}

