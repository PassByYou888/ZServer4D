//
// use fastcode for function fillchar & move
//
{.$define fastcode}

//
// use of API.Windows::tlsAlloc/tslSetValue/tlsGetValue
// else: threadvar
//
// threadvar is 5-10% faster
//
{.$define tls_mode}

{$if CompilerVersion <= 15.0}
  {$define release}
  {$define debug}
{$ifend}

{$if defined(release) and defined(debug)}
  {$undef debug}
{$ifend}

{$if CompilerVersion >= 17.0}
  {$define has_mm_ex}
  {$ifdef release}
    {$define has_inline}
  {$endif}
  {$define has_thread_exit}
{$ifend}

{$ifdef release}
  {$optimization on}
  {$stackframes off}
  {$assertions off}
  {$overflowchecks off}
  {$rangechecks off}
  {$debuginfo off}
  {$localsymbols off}
{$endif}
