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

