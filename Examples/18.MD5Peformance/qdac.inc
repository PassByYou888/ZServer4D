{$DEFINE QDAC}

{$IF RTLVersion>=24}
{$LEGACYIFEND ON}
{$IFEND}

{$IF RTLVersion<18}
{$MESSAGE Error '!!!QDAC 3 ONLY work and tested for 2007+,No support in older version!!!'}
{$IFEND <RadStudio2007}
{$IF RTLVersion>=22}
{$DEFINE ENABLE_REGEX}
{$ELSE}
{$IFNDEF BCB}
{$DEFINE ENABLE_REGEX}
{$ENDIF}
{$IFEND}

