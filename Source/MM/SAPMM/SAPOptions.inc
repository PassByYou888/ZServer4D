{.$DEFINE SAP_DEBUG}               // debug of the MM logic
{.$DEFINE SAP_DEBUG_MEMORY_LEAKS}  // memory leaks reporting

{$ASSERTIONS     OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS    OFF}

{$IFDEF SAP_DEBUG}
  {$DEFINE SAP_CHECKMAGIC}   // check blocks correctness
  {$DEFINE SAP_STAT}         // turn on statistics calculation

  {$OPTIMIZATION OFF}
  {$DEBUGINFO    ON}
{$ELSE}
  {$IFDEF SAP_DEBUG_MEMORY_LEAKS}

    {$DEFINE SAP_STAT}           // turn on statistics
    {$DEFINE SAP_MEMORYLEAKS}    // turn on memory leaks checking
    {$DEFINE SAP_SHOWCALLSTACK}  // show call stack for memory leaks
    {$DEFINE SAP_STATIP}         // turn on saving IP for allocate calls

    {$OPTIMIZATION   OFF}        // must be turned off for the SAP_STATIP may work

  {$ELSE} // normal mode, maximum execution speed

    {$OPTIMIZATION ON}
    {$DEBUGINFO    OFF}

    {$STACKFRAMES  OFF}
  {$ENDIF}

{$ENDIF}
