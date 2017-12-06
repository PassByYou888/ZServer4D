// for debug mm
{.$define qmm_debug}

//{$if CompilerVersion <= 20.0}
{$if not defined(release) and not defined(debug)}
  {$define release}
{$ifend}

{$ifdef debug}
  {$define TRACE_STACK}

  {$stackframes on} // {$W+}
{$endif}

{$ifdef release}
  {$ifdef qmm_debug}
    {$undef qmm_debug}
  {$endif}
  {$ifdef debug}
    {$undef debug}
  {$endif}
{$endif}

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
  {$warn unsafe_type off}
  {$warn unsafe_code off}
  {$warn unsafe_cast off}
{$endif}
