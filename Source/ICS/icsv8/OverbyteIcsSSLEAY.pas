{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Delphi encapsulation for SSLEAY32.DLL (OpenSSL)
              Renamed libssl32.dll for OpenSSL 1.1.0 and later
              This is only the subset needed by ICS.
Creation:     Jan 12, 2003
Version:      8.41
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2017 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Dec 11, 2004 Fixed Load to correctly check for LoadLibrary failure and to
             return correct error number.
Nov 08, 2005 F. Piette Worked around Delphi 6 bug related to type redefinition.
             Search for NoTypeEnforce in the type declarations.
Nov 08, 2005 Arno Garrels - Checked non-dummy structures against different
             OpenSSL versions see comments.
             Changed declaration of TX509_EXTENSION according OpenSSL v.0.9.7g.
Nov 19, 2005 F. Piette fixed internal error for Delphi 5. Same fix as Delphi 6.
             Introduced symbol NoTypeEnforce for that purpose.
Dec 07, 2005 A. Garrels support of OSSL v0.9.8a added.
Jan 27, 2006 A. Garrels made BDS2006 (BCB & Pascal) compilers happy.
Mar 03, 2007 A. Garrels: Small changes to support OpenSSL 0.9.8e.
             Read comments in OverbyteIcsSslDefs.inc.
Jun 30, 2008 A.Garrels made some changes to prepare code for Unicode.
             Added a few constants and dummy records.
Aug 02, 2008 Still one PChar caught in one of the records.
Dec 20, 2009 A.Garrels added plenty of stuff. Some is not yet used some is, like
             Server Name Indication (SNI).
May 08, 2010 A. Garrels added two declarations required to support
             Open SSL 0.9.8n.
Apr 23, 2011 A. Garrels added C-macro f_SSL_clear_options.
Apr 24, 2011 Arno - Record TEVP_PKEY_st changed in 1.0.0 and had to
             be declared as dummy. See helper functions Ics_Ssl_EVP_PKEY_xxx
             in OverbyteIcsLibeay.pas.
May 03, 2011 Arno added some function declarations.
May 31, 2011 Arno removed the packed modifier from non-dummy records.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Mar 13, 2015 V8.01 Angus updated SSL_OP option literals, added TLS v1.1 and 1.2 methods
             Added functions need to generate DH keys for EDH ciphers with Forward Secrecy
             Note, only OpenSSL 1.0.1 and later are now supported, removed various conditionals
May 08, 2015 V8.02 Angus adding missing SSL_OP_SINGLE_ECDH_USE
Nov 20, 2015 V8.03 Eugene Kotlyarov added RSA key related stuff
Mar 3, 2016  V8.04 Angus support define OPENSSL_ALLOW_SSLV2 to load old OpenSSL
                     DLLs that still export such methods
May 24, 2016 V8.27 Angus match version to Wsocket where most of this API is used
             Initial support for OpenSSL 1.1.0, new DLL file names, old exports gone
             Moved all public GLIBEAY_xx variables here from OverbyteIcsLIBEAY
             Add public variable GSSLEAY_DLL_IgnoreNew which should be set to TRUE before calling
              any SSL functions if OpenSSL 1.1.0 should be ignored.  Otherwise libcrypto-1_1.dll
              found in the PATH will override libeay32.dll in the local directory
             Added public variable GSSL_BUFFER_SIZE defaults to 16384, previously fixed
               at 4096, may improve SSL performance if larger
             Added public variable GSSL_DLL_DIR if set before OpenSSL loaded,
               will use this directory for DLLs, must have trailing \
             Load now SsleayLoad, WhichFailedToLoad now SsleayWhichFailedToLoad
             Added f_SSL_get_ciphers and related functions to get lists of ciphers
             Added TSslHandshakeState more detail about handshakes in 1.1.0
             GetFileVerInfo renamed IcsGetFileVerInfo to prevent conflicts with other libs
June 26, 2016 V8.29 Angus Implement GSSL_DLL_DIR properly to report full file path on error
Aug 5, 2016   V8.31 Angus testing OpenSSL 1.1.0 beta 6
Aug 27, 2016  V8.32 Angus, suuport final release OpenSSL 1.1.0
                OpenSSL 64-bit DLLs have different file names with -x64 added
Sept 5, 2016  V8.34 Angus, make ICS work up to OpenSSL release 1.1.1
                (security and bug releases are 1.1.0a/b/etc with no changed exports, in theory)
              Added public variable GSSLEAY_DLL_IgnoreOld so only OpenSSL 1.1.0 and later are loaded
Oct 18, 2016  V8.35 Angus, major rewrite to simplify loading OpenSSL DLL functions
              Reversed V8.34 fix so this release only supports 1.1.0 not 1.1.1
              OPENSSL_ALLOW_SSLV2 gone with all SSLv2 functions
              stub more removed functions to save some exceptions
Oct 26, 2016  V8.36 Angus more clean up of old stuff gone from 1.1.0
Nov 15, 2016  V8.38 Added public variable GSSL_SignTest_Check to check OpenSSL
                DLLs are digitally signed, and GSSL_SignTest_Certificate to
                check for a valid certificate, both default to false
              Moved IcsGetFileVerInfo to OverbyteIcsUtils
Nov 22, 2016  V8.39 Added functions to check certificate params using X509_VERIFY_PARAM, X509_OBJECT
              Minimum OpenSSL supported is now 1.0.2 (1.0.1 support ceases Dec 2016)
Jan 27, 2017  V8.40 Added more functions to get and check context certs
              Added Protocol Message callback functions for handshake debugging
              Added Security Level functions (1.1.0 and later)
Feb 24, 2017  V8.41 Added more constants



Notes - OpenSSL ssleay32 changes between 1.0.2 and 1.1.0 - August 2016

file ssleay32.dll > libssl-1_1.dll and libssl-1_1-x64.dll

OpenSSL now auto initialises using OPENSSL_init_crypto and OPENSSL_init_ssl
so these are gone:
method SSL_library_init
method SSL_load_error_strings

method SSL_state > SSL_get_state (with different return value)

new version selection using:
TLS_client_method
TLS_method
TLS_server_method
SSL_set_min_proto_version
SSL_set_max_proto_version

Old exports gone:
SSLv3_method
SSLv3_client_method
SSLv3_server_method
SSLv23_method
SSLv23_client_method
SSLv23_server_method
All version specific TLSv1_1x_methods deprecated so don't load them either


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{$I Include\OverbyteIcsDefs.inc}
{$I Include\OverbyteIcsSslDefs.inc}
{$A8}

unit OverbyteIcsSSLEAY;

{$IFDEF VER140}
    // Delphi 6 is bugged
    // [Erreur fatale] IcsSSLEAY.pas: Erreur interne : URW533
    {$DEFINE NoTypeEnforce}
{$ENDIF}
{$IFDEF VER130}
    // Delphi 5 is bugged
    // [Erreur fatale] IcsSSLEAY.pas: Erreur interne : URW533
    {$DEFINE NoTypeEnforce}
{$ENDIF}

interface

{$IFDEF USE_SSL}

uses
  {$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.Errno,
  {$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    OverbyteIcsUtils;

const
    IcsSSLEAYVersion   = 841;
    CopyRight : String = ' IcsSSLEAY (c) 2003-2017 F. Piette V8.41 ';

    EVP_MAX_IV_LENGTH                 = 16;       { 03/02/07 AG }
    EVP_MAX_BLOCK_LENGTH              = 32;       { 11/08/07 AG }
    EVP_MAX_KEY_LENGTH                = 32;       { 11/08/07 AG }

{ const - why were these variables ever declared as const??? }
{ V8.27 consolidated from LIBEAY so all in one place }
var
    GLIBEAY_DLL_Handle          : THandle = 0;
    GLIBEAY_DLL_Name            : String  = {$IFDEF MACOS}
                                                '/usr/lib/libcrypto.dylib';
                                            {$ELSE}
                                                'libeay32.dll';
                                            {$ENDIF}
    GLIBEAY_110DLL_Name         : String  = {$IFDEF MACOS}
                                               '/usr/lib/libcrypto.dylib';  { V8.32 !!!! not tested, unknown file name }
                                            {$ELSE}
                                                {$IFDEF CPUX64}
                                                    'libcrypto-1_1-x64.dll';  { V8.32 }
                                                {$ELSE}
                                                    'libcrypto-1_1.dll';  { V8.27 }
                                                {$ENDIF}
                                             {$ENDIF}
    GLIBEAY_DLL_FileName        : String  = '*NOT LOADED*';
    GSSLEAY_DLL_Handle          : THandle = 0;
    GSSLEAY_DLL_Name            : String  = {$IFDEF MACOS}
                                                '/usr/lib/libssl.dylib';
                                            {$ELSE}
                                                'ssleay32.dll';
                                            {$ENDIF}
    GSSLEAY_110DLL_Name         : String  = {$IFDEF MACOS}
                                                '/usr/lib/libssl.dylib';
                                            {$ELSE}
                                                {$IFDEF CPUX64}
                                                    'libssl-1_1-x64.dll';  { V8.32 }
                                                {$ELSE}
                                                    'libssl-1_1.dll';   { V8.27 }
                                                {$ENDIF}
                                            {$ENDIF}
    GSSLEAY_DLL_FileName        : String  = '*NOT_LOADED*';
    GSSLEAY_DLL_FileVersion     : String = '';
    GSSLEAY_DLL_FileDescription : String = '';
 { V8.27 don't attempt to find new name libcrypto-1_1.dll, use libeay32.dll }
    GSSLEAY_DLL_IgnoreNew       : Boolean = False;
 { V8.34 only use libcrypto-1_1.dll, not libeay32.dll }
    GSSLEAY_DLL_IgnoreOld       : Boolean = False;
 { V8.27 write buffer size, was fixed at 4096, but send used a 16K buffer }
    GSSL_BUFFER_SIZE            : Integer = 16384;
 { V8.27 if set before OpenSSL loaded, will use this directory for DLLs, must have trailing \ }
    GSSL_DLL_DIR                : string = '';

 { V8.38 wintrust stuff for authenticode digital signing checking }
    GSSL_SignTest_Check         : Boolean = False;    { check OpenSSL DLLs are digitally signed }
    GSSL_SignTest_Certificate   : Boolean = False;    { False only checks hash, True also checks certificate (longer) }

    { Version stuff added 07/12/05  = V8.27 moved from OverbyteIcsLIBEAY  }
    ICS_OPENSSL_VERSION_NUMBER  : Longword  = 0;
    ICS_SSL_NO_RENEGOTIATION    : Boolean = FALSE;
    ICS_RAND_INIT_DONE          : Boolean = FALSE;   { V8.35 have we initialised random numbers }

const
 { found in \include\openssl\opensslv.h }
    //OSSL_VER_0906G = $0090607f; no longer supported
 {  OSSL_VER_0907G = $0090707f;
    OSSL_VER_1000  = $10000000; // Untested, did not build with MinGW
    OSSL_VER_1000D = $1000004f; // Might be still buggy, had to incl. one workaround so far, see TSslContext.InitContext
    OSSL_VER_1000J = $100000af; // just briefly tested}

{ only supporting versions with TLS 1.1 and 1.2 }
{ V8.27 moved from OverbyteIcsLIBEAY  }
    OSSL_VER_MIN   = $0000000F; // minimum version     { V8.35 }
    OSSL_VER_1001  = $1000100F; // 1.0.1 untested
    OSSL_VER_1001G = $1000107F; // just briefly tested  {
    OSSL_VER_1001H = $1000108F; // just briefly tested
    OSSL_VER_1001I = $1000109F; // just briefly tested
    OSSL_VER_1001J = $100010AF; // untested
    OSSL_VER_1001K = $100010BF; // just briefly tested
    OSSL_VER_1001L = $100010CF; // untested
    OSSL_VER_1002  = $10002000; // 1.0.2 just briefly tested
    OSSL_VER_1002A = $1000201F; // just briefly tested
    OSSL_VER_1002ZZ= $10002FFF; // not yet released
    OSSL_VER_1100  = $1010000F; // 1.1.0                 { V8.32 }
    OSSL_VER_1100A = $1010001F; // 1.1.0a                { V8.35 }
    OSSL_VER_1100B = $1010002F; // 1.1.0b                { V8.35 }
    OSSL_VER_1100C = $1010003F; // 1.1.0c                { V8.38 }
    OSSL_VER_1100D = $1010004F; // 1.1.0d                { V8.41 }
    OSSL_VER_1100ZZ= $10100FFF; // not yet released      { V8.35 }
    OSSL_VER_1101  = $10101000; // 1.1.1 next feature release  { V8.34 }
    OSSL_VER_1199  = $10101FFF; // not yet released      { V8.34 }
    OSSL_VER_MAX   = $FFFFFFFF; // maximum version       { V8.35 }

    { Basically versions listed above are tested if not otherwise commented.  }
    { Versions between are assumed to work, however they are untested.        }
    { OpenSSL libraries for ICS are available for download here:              }
    { http://wiki.overbyte.be/wiki/index.php/ICS_Download                     }

    MIN_OSSL_VER   = OSSL_VER_1002;   { V8.39 minimum is now 1.0.2 }
    MAX_OSSL_VER   = OSSL_VER_1100ZZ; { V8.35 1.1.0zz }

    { V8.41 PEM base64 file titles }
    PEM_STRING_HDR_BEGIN   = '-----BEGIN ';    { six hyphens }
    PEM_STRING_HDR_END     = '-----END ';
    PEM_STRING_HDR_TAIL    = '-----'+#13#10;
    PEM_STRING_X509_OLD    = 'X509 CERTIFICATE' ;
    PEM_STRING_X509        = 'CERTIFICATE' ;
    PEM_STRING_X509_TRUSTED= 'TRUSTED CERTIFICATE' ;
    PEM_STRING_X509_REQ_OLD= 'NEW CERTIFICATE REQUEST' ;
    PEM_STRING_X509_REQ    = 'CERTIFICATE REQUEST' ;
    PEM_STRING_X509_CRL    = 'X509 CRL' ;
    PEM_STRING_EVP_PKEY    = 'ANY PRIVATE KEY' ;
    PEM_STRING_PUBLIC      = 'PUBLIC KEY' ;
    PEM_STRING_RSA         = 'RSA PRIVATE KEY' ;
    PEM_STRING_RSA_PUBLIC  = 'RSA PUBLIC KEY' ;
    PEM_STRING_DSA         = 'DSA PRIVATE KEY' ;
    PEM_STRING_DSA_PUBLIC  = 'DSA PUBLIC KEY' ;
    PEM_STRING_PKCS7       = 'PKCS7' ;
    PEM_STRING_PKCS7_SIGNED= 'PKCS #7 SIGNED DATA' ;
    PEM_STRING_PKCS8       = 'ENCRYPTED PRIVATE KEY' ;
    PEM_STRING_PKCS8INF    = 'PRIVATE KEY' ;
    PEM_STRING_DHPARAMS    = 'DH PARAMETERS' ;
    PEM_STRING_DHXPARAMS   = 'X9.42 DH PARAMETERS' ;
    PEM_STRING_SSL_SESSION = 'SSL SESSION PARAMETERS' ;
    PEM_STRING_DSAPARAMS   = 'DSA PARAMETERS' ;
    PEM_STRING_ECDSA_PUBLIC= 'ECDSA PUBLIC KEY' ;
    PEM_STRING_ECPARAMETERS= 'EC PARAMETERS' ;
    PEM_STRING_ECPRIVATEKEY= 'EC PRIVATE KEY' ;
    PEM_STRING_PARAMETERS  = 'PARAMETERS' ;
    PEM_STRING_CMS         = 'CMS' ;

type

    TOSSLImports = record   { V8.35 }
        F: PPointer;   // function pointer
        N: PAnsiChar;  // export name
        MI: LongWord;  // minimum OpenSSL version
        MX: LongWord;  // maximum OpenSSL version
    end;

    EIcsSsleayException = class(Exception);
    PPChar   = ^PChar;
    PPAnsiChar = ^PAnsiChar;
    //PInteger = ^Integer;

    // All datatypes defined below using the Dummy array can't be used
    // directly. They all must be used thru pointers only. If you really need
    // to use those structure, you must replace Dummy member with the actual
    // members defined in the OpenSSL header !
    TCRYPTO_THREADID_st = packed record
        Dummy : array [0..0] of Byte;
          //ptr : Pointer;
          //val : LongWord;
    end;
    PCRYPTO_THREADID = ^TCRYPTO_THREADID_st;

{$IFNDEF OPENSSL_NO_ENGINE}
    TEngine_st = record
        Dummy : array [0..0] of Byte;
    end;
    PENGINE = ^TEngine_st;
{$ENDIF}

    TSSL_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PSSL            = ^TSSL_st;

    TSSL_SESSION_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PSSL_SESSION    = ^TSSL_SESSION_st;
    PPSSL_SESSION   = ^PSSL_SESSION;

    TBIO_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PBIO            = ^TBIO_st;
    PPBIO           = ^PBIO;

    TBIO_METHOD_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PBIO_METHOD     = ^TBIO_METHOD_st;

    TSSL_CTX_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PSSL_CTX        = ^TSSL_CTX_st;

    TSSL_METHOD_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PSSL_METHOD     = ^TSSL_METHOD_st;

    TSSL_CIPHER_st = packed record           { V8.27 }
        Dummy : array [0..0] of Byte;
    end;
    PSSL_CIPHER     = ^TSSL_CIPHER_st;

    TX509_STORE_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_STORE = ^TX509_STORE_st;

    TX509_STORE_CTX_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_STORE_CTX = ^TX509_STORE_CTX_st;

    TX509_NAME_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_NAME = ^TX509_NAME_st;

    TSTACK_st = packed record               //AG
        Dummy : array [0..0] of Byte;
    end;
    PSTACK = ^TSTACK_st;

    TASN1_TYPE_st = packed record                   //AG
        Dummy : array [0..0] of Byte;
    end;
    PASN1_TYPE = ^TASN1_TYPE_st;

    TX509_VERIFY_PARAM_st = packed record        { V8.39 }
        Dummy : array [0..0] of Byte;
    end;
    PX509_VERIFY_PARAM = ^TX509_VERIFY_PARAM_st;

    TBN_CTX_st = packed record                   { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PBN_CTX = ^TBN_CTX_st;

    TEC_GROUP_st = packed record                 { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PEC_GROUP = ^TEC_GROUP_st;

    TEC_METHOD_st = packed record                 { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PEC_METHOD = ^TEC_METHOD_st;

    TEC_POINT_st = packed record                  { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PEC_POINT = ^TEC_POINT_st;

    TEC_PKPARAMETERS_st = packed record           { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PEC_PKPARAMETERS = ^TEC_PKPARAMETERS_st;

    TEC_PARAMETERS_st = packed record             { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PEC_PARAMETERS = ^TEC_PARAMETERS_st;

    TEC_BUILTIN_CURVE_st = packed record          { V8.40 }
        nid: Integer;
        comment: PAnsiChar;
    end;
    PEC_BUILTIN_CURVE = ^TEC_BUILTIN_CURVE_st;
    TEC_BUILTIN_CURVES = array of TEC_BUILTIN_CURVE_st;

    TBN_MONT_CTX_st = packed record             { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PBN_MONT_CTX = ^TBN_MONT_CTX_st;
    { Stack - dummies i.e. STACK_OF(X509) }

    PSTACK_OF_X509_EXTENSION    = PStack;     //AG
    PPSTACK_OF_X509_EXTENSION   = ^PSTACK_OF_X509_EXTENSION; { V8.41 }
    PSTACK_OF_X509_ALGOR        = PStack;     //AG

    PSTACK_OF_X509              = PSTACK;     //AG
    PSTACK_OF_X509_CRL          = PSTACK;     //AG

    PPSTACK_OF_X509             = ^PSTACK_OF_X509; //AG

    PSTACK_OF_PKCS7_RECIP_INFO  = PStack;     //AG
    PSTACK_OF_X509_ATTRIBUTE    = PStack;     //AG
    PSTACK_OF_PKCS7_SIGNER_INFO = PStack;     //AG
    PSTACK_OF_509_LOOKUP        = PStack;     //AG
    PSTACK_OF_X509_OBJECT       = PStack;     //AG

    PSTACK_OF_X509_NAME = {$IFNDEF NoTypeEnforce}type{$ENDIF} PStack;
    PSTACK_OF_X509_INFO = {$IFNDEF NoTypeEnforce}type{$ENDIF} PStack;
    PSTACK_OF_X509_VERIFY_PARAM = PStack;     { V8.39 }

    PSTACK_OF_SSL_CIPHER        = PSTACK;                 { V8.27 }
    PPSTACK_OF_SSL_CIPHER       = ^PSTACK_OF_SSL_CIPHER;  { V8.27 }
    PCRYPTO_EX_DATA             = PSTACK;                 { V8.40 }

    TX509_lookup_method_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_LOOKUP_METHOD = ^TX509_lookup_method_st;

    TX509_lookup_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_LOOKUP = ^TX509_lookup_st;

    TX509_OBJECT_st = packed record         //AG
        Dummy : array [0..0] of Byte;
    end;
    PX509_OBJECT = ^TX509_OBJECT_st;

    TX509_LOOKUP_TYPE = (X509_LU_NONE, X509_LU_X509, X509_LU_CRL);  { V8.39 }

    TX509_NAME_ENTRY_st = packed record      //AG
        Dummy : array [0..0] of Byte;
    end;
    PX509_NAME_ENTRY = ^TX509_NAME_ENTRY_st;

    TEVP_MD_st = packed record               //AG
        Dummy : array [0..0] of Byte;
    end;
    PEVP_MD = ^TEVP_MD_st;

    TEVP_MD_CTX_st = packed record      { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PEVP_MD_CTX = ^TEVP_MD_CTX_st;

    TEVP_PKEY_CTX_st = packed record    { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PEVP_PKEY_CTX = ^TEVP_PKEY_CTX_st;

    BN_ULONG = Cardinal;               { V8.03 }

    TBIGNUM_st = packed record         { V8.03 }
        Dummy : array [0..0] of Byte;
    end;
    PBIGNUM = ^TBIGNUM_st;

    TRSA_st = packed record
        Dummy : array [0..0] of Byte;      //AG
    end;
    PRSA = ^TRSA_st;
    PPRSA = ^PRSA;                        { V8.03 }

    TDSA_st = packed record                //AG
        Dummy : array [0..0] of Byte;
    end;
    PDSA = ^TDSA_st;

    TDH_st = packed record                 //AG
        Dummy : array [0..0] of Byte;
    end;
    PDH = ^TDH_st;
    PPDH = ^PDH;                        { V8.40 }

    TEC_KEY_st = packed record                 //AG
        Dummy : array [0..0] of Byte;
    end;
    PEC_KEY = ^TEC_KEY_st;

    { We may no longer define it since changed in 1.0.0+               }
    { See helper functions Ics_Ssl_EVP_PKEYxxx in OverbyteIcsLibeay32  }
    TEVP_PKEY_st = packed record
        Dummy : array [0..0] of Byte;
    (*
        type_       : Longint;
        save_type   : Longint;
        references  : Longint;
    {OSSL_100 two fields added}
        ameth       : Pointer; //PEVP_PKEY_ASN1_METHOD;
        engine      : Pointer; //PENGINE;
    {/OSSL_100}
        case Integer of
        0 : (ptr  : PAnsiChar);
        1 : (rsa  : PRSA); // RSA
        2 : (dsa  : PDSA); // DSA
        3 : (dh   : PDH);  // DH
        4 : (ec   : PEC_KEY); //* ECC */
        { more not needed ...
        int save_parameters;
        STACK_OF(X509_ATTRIBUTE) *attributes; /* [ 0 ] */ }
    *)
    end;
    PEVP_PKEY = ^TEVP_PKEY_st;
    PPEVP_PKEY = ^PEVP_PKEY;

    TEVP_CIPHER_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PEVP_CIPHER = ^TEVP_CIPHER_st;

    TASN1_OBJECT_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PASN1_OBJECT = ^TASN1_OBJECT_st;

    PSTACK_OF_ASN1_OBJECT = PStack;     { V8.39 }

    TX509_ALGOR_st = record
        algorithm : PASN1_OBJECT;
        parameter : PASN1_TYPE;
    end;
    PX509_ALGOR = ^TX509_ALGOR_st;

    TX509_PURPOSE_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_PURPOSE = ^TX509_PURPOSE_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TASN1_STRING_st = record
        length : Integer;
        type_  : Integer;
        data   : PAnsiChar;
        //* The value of the following field depends on the type being
        //* held.  It is mostly being used for BIT_STRING so if the
        //* input data has a non-zero 'unused bits' value, it will be
        //* handled correctly */
        flags  : Longword;
    end;
    PASN1_STRING       = ^TASN1_STRING_st;
    TASN1_OCTET_STRING = TASN1_STRING_st;
    PASN1_OCTET_STRING = ^TASN1_OCTET_STRING;
    TASN1_BIT_STRING   = TASN1_STRING_st;
    PASN1_BIT_STRING   = ^TASN1_BIT_STRING;

    TASN1_TIME = {$IFNDEF NoTypeEnforce}type{$ENDIF} TASN1_STRING_st;
    PASN1_TIME = ^TASN1_TIME;

    TASN1_INTEGER = {$IFNDEF NoTypeEnforce}type{$ENDIF} TASN1_STRING_st;
    PASN1_INTEGER = ^TASN1_INTEGER;

    TASN1_VALUE_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PASN1_VALUE  = ^TASN1_VALUE_st;
    PPASN1_VALUE = ^PASN1_VALUE;

    Tasn1_pctx_st = packed record      { V8.40 }
        flags: DWORD;
        nm_flags: DWORD;
        cert_flags: DWORD;
        oid_flags: DWORD;
        str_flags: DWORD;
    end;
    PASN1_PCTX = ^Tasn1_pctx_st;       { V8.40 }

const
    GEN_OTHERNAME  = 0;          { V8.40 type of GENERAL_NAME }
    GEN_EMAIL      = 1;
    GEN_DNS        = 2;
    GEN_X400       = 3;
    GEN_DIRNAME    = 4;
    GEN_EDIPARTY   = 5;
    GEN_URI        = 6;
    GEN_IPADD      = 7;
    GEN_RID        = 8;

type
    TGENERAL_NAME_st  = packed record     { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PGENERAL_NAME  = ^TGENERAL_NAME_st;

    // 0.9.7g, 0.9.8a 0.9.8e, 1.0.0d
    TEVP_CIPHER_INFO_st = record      { 03/02/07 AG }
        cipher : PEVP_CIPHER;
        iv     : array [0..EVP_MAX_IV_LENGTH - 1] of AnsiChar;
    end;
    EVP_CIPHER_INFO  = TEVP_CIPHER_INFO_st;
    PEVP_CIPHER_INFO = ^EVP_CIPHER_INFO;

    // 0.9.7g, 0.9.8a 0.9.8e, 1.0.0d
    TPrivate_key_st = record            //AG
        //Dummy : array [0..0] of Byte;
        version     : Integer;
        // The PKCS#8 data types
        enc_algor   : PX509_ALGOR;
        enc_pkey    : PASN1_OCTET_STRING; // encrypted pub key
        // When decrypted, the following will not be NULL
        dec_pkey    : PEVP_PKEY;
        // used to encrypt and decrypt
        key_length  : Integer ;
        key_data    : PAnsiChar;
        key_free    : Integer; // true if we should auto free key_data
        // expanded version of 'enc_algor'
        cipher      : PEVP_CIPHER_INFO;
        references  : Integer ;
    end;
    PX509_PKEY = ^TPrivate_key_st;

 {   TX509_REQ_st = packed record    V8.36 using longer version
        Dummy : array [0..0] of Byte;
    end;
    PX509_REQ = ^TX509_REQ_st;   }

    // 0.9.7g, 0.9.8a 0.9.8e, 1.0.0d
    TX509_CRL_INFO_st = record
        version     : PASN1_INTEGER;
        sig_alg     : PX509_ALGOR;
        issuer      : PX509_NAME;
        lastUpdate  : PASN1_TIME;
        nextUpdate  : PASN1_TIME;
        {
        STACK_OF(X509_REVOKED) *revoked;
        STACK_OF(X509_EXTENSION) /* [0] */ *extensions;
        ASN1_ENCODING enc; }
    end;
    PX509_CRL_INFO = ^TX509_CRL_INFO_st;

    TX509_CRL_st = record
        //* actual signature *//
        crl       : PX509_CRL_INFO;
        sig_alg   : PX509_ALGOR;
        signature : PASN1_BIT_STRING;
        references: Integer;
        {more..}
    end;
    PX509_CRL = ^TX509_CRL_st;
    PPX509_CRL = ^PX509_CRL;


    PX509  = ^TX509_st;
    PPX509 = ^PX509;

    // 0.9.7g, 0.9.8a 0.9.8e, 1.0.0d
    TX509_INFO_st = record
        x509        : PX509;
        crl         : PX509_CRL;
        x_pkey      : PX509_PKEY;
        enc_cipher  : EVP_CIPHER_INFO;
        enc_len     : Integer;
        enc_data    : PAnsiChar;
        references  : Integer;
    end;
    PX509_INFO = ^TX509_INFO_st;

    (* // 0.9.6g                  {11/07/05 AG}
    TX509_EXTENSION = packed record
        object_       : PASN1_OBJECT;
        critical      : SmallInt;
        netscape_hack : SmallInt;
        value         : PASN1_OCTET_STRING;
        method        : PX509V3_EXT_METHOD;
        ext_val       : Pointer;	        // extension value
    end;
    PX509_EXTENSION = ^TX509_EXTENSION;
    *)

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TX509_VAL_st = record                    {AG 02/06/06}
        notBefore : PASN1_TIME;
        notAfter  : PASN1_TIME;
    end;
    PX509_VAL = ^TX509_VAL_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TX509_PUBKEY_st = record                 //AG
        algor       : PX509_ALGOR;
        public_key  : PASN1_BIT_STRING;
        pkey        : PEVP_PKEY;
    end;
    PX509_PUBKEY = ^TX509_PUBKEY_st;

    { Certinfo }  // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d         {AG 02/06/06}
    TX509_CINF_st = record
        version         : PASN1_INTEGER;            // [ 0 ] default of v1
        serialNumber    : PASN1_INTEGER;
        signature       : PX509_ALGOR;
        issuer          : PX509_NAME;
        validity        : PX509_VAL;
        subject         : PX509_NAME;
        key             : PX509_PUBKEY;
        {issuerUID       : PASN1_BIT_STRING;         // [ 1 ] optional in v2
        subjectUID      : PASN1_BIT_STRING;         // [ 2 ] optional in v2
        extensions      : PSTACK_OF_X509_EXTENSION; // [ 3 ] optional in v3}
    end;
    PX509_CINF = ^TX509_CINF_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d             {11/07/05 AG}
    ASN1_BOOLEAN = {$IFNDEF NoTypeEnforce}type{$ENDIF} Longint;
    TX509_EXTENSION_st = record
        object_       : PASN1_OBJECT;
        critical      : ASN1_BOOLEAN;
        value         : PASN1_OCTET_STRING;
    end;
    PX509_EXTENSION = ^TX509_EXTENSION_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TX509_st = record
        cert_info   : PX509_CINF;
        sig_alg     : PX509_ALGOR;
        signature   : PASN1_BIT_STRING;
        valid       : Integer ;
        references  : Integer;
        name        : PAnsiChar;
        {more ...}
    end;

// 8.35 moved lots of declarations from OverbyteIcsLibeayEx so they are all together

type
    TEVP_CIPHER_CTX_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PEVP_CIPHER_CTX = ^TEVP_CIPHER_CTX_st;

{$IFDEF OPENSSL_NO_ENGINE}
    TEngine_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PEngine = ^TEngine_st;
{$ENDIF}

    TASN1_ENCODING_st = packed record
        enc       : PAnsiChar;
        len       : LongWord;
        modified  : Integer;
    end;
    TASN1_ENCODING = TASN1_ENCODING_st;
    PASN1_ENCODING = ^TASN1_ENCODING_st;

    TLHASH_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PLHASH = ^TLHASH_st;

    TX509V3_CTX_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509V3_CTX = ^TX509V3_CTX_st;

    {TX509_PUBKEY_st = packed record
        algor       : PX509_ALGOR;
        public_key  : PASN1_BIT_STRING;
        pkey        : PEVP_PKEY;
    end;
    PX509_PUBKEY = ^TX509_PUBKEY_st;}

    TX509_REQ_INFO_st = packed record
        enc         : TASN1_ENCODING;
        version     : PASN1_INTEGER;
        subject     : PX509_NAME;
        pubkey      : PX509_PUBKEY;
        attributes  : PSTACK;
    end;
    PX509_REQ_INFO = ^TX509_REQ_INFO_st;

    TX509_REQ_st = packed record
        req_info    : PX509_REQ_INFO;
        sig_alg     : PX509_ALGOR;
        signature   : PASN1_STRING;
        references  : Integer;
    end;
    PX509_REQ = ^TX509_REQ_st;
    PPX509_REQ = ^PX509_REQ;            { V8.40 }

    TECDSA_SIG_st = packed record       { V8.40 }
        r  : PBIGNUM;
        s  : PBIGNUM;
    end;                    
    ECDSA_SIG = TECDSA_SIG_st;
    PECDSA_SIG = ^TECDSA_SIG_st;

    TBN_GENCB = packed record            { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PBN_GENCB = ^TBN_GENCB;

    TPKCS7_ISSUER_AND_SERIAL_st = packed record     //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS7_ISSUER_AND_SERIAL = ^TPKCS7_ISSUER_AND_SERIAL_st;

    TPKCS7_ENC_CONTENT_st = packed record           //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS7_ENC_CONTENT = ^TPKCS7_ENC_CONTENT_st;

    TPKCS7_DIGEST_st = packed record                //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS7_DIGEST = ^TPKCS7_DIGEST_st;

    TPKCS7_ENCRYPT_st = packed record               //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS7_ENCRYPT = ^TPKCS7_ENCRYPT_st;

    { Caution! Structures may change in future versions 0.96g-0.98 beta OK} {AG}
    { However needed for S/MIME PKCS#7 parsing }
    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TPKCS7_SIGNER_INFO_st = record
        version             : PASN1_INTEGER;                // version 1
        issuer_and_serial   : PPKCS7_ISSUER_AND_SERIAL;
        digest_alg          : PX509_ALGOR;
        auth_attr           : PSTACK_OF_X509_ATTRIBUTE;     // [ 0 ]
        digest_enc_alg      : PX509_ALGOR;
        enc_digest          : PASN1_OCTET_STRING;
        unauth_attr         : PSTACK_OF_X509_ATTRIBUTE;     // [ 1 ]
        // The private key to sign with //
        pkey                : PEVP_PKEY;
    end;
    //PKCS7_SIGNER_INFO = ^TPKCS7_SIGNER_INFO_st; // **Name conflict with wincrypt.h**
    PKCS7_SIGNER_INFO_OSSL = ^TPKCS7_SIGNER_INFO_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TPKCS7_ENVELOPED_st = record
        version       : PASN1_INTEGER;
        recipientinfo : PSTACK_OF_PKCS7_SIGNER_INFO;
        enc_data      : PPKCS7_ENC_CONTENT;
    end;
    PPKCS7_ENVELOPE = ^TPKCS7_ENVELOPED_st;

    PPKCS7  = ^TPKCS7_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TPKCS7_SIGNED_st = record
        version     : PASN1_INTEGER;
        md_algs     : PSTACK_OF_X509_ALGOR;
        cert        : PSTACK_OF_X509;
        crl         : PSTACK_OF_X509_CRL;
        signer_info : PSTACK_OF_PKCS7_SIGNER_INFO;
        contents    : PPKCS7;
    end;
    PPKCS7_SIGNED = ^TPKCS7_SIGNED_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    PKCS7_signedandenveloped = record
        version         : PASN1_INTEGER;
        md_algs         : PSTACK_OF_X509_ALGOR;
        cert            : PSTACK_OF_X509;
        crl             : PSTACK_OF_X509_CRL;
        signer_info     : PSTACK_OF_PKCS7_SIGNER_INFO;
        enc_data        : PPKCS7_ENC_CONTENT;
        recipientinfo   : PSTACK_OF_PKCS7_RECIP_INFO;
    end;
    PPKCS7_SIGN_ENVELOPE = ^PKCS7_signedandenveloped;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TPKCS7_st = record                         //AG
      { The following is non NULL if it contains ASN1 encoding of this structure }
        asn1        : PAnsiChar;
        length      : Integer;
        state       : Integer;
        detached    : Integer;
        type_       : PASN1_OBJECT;
        case Integer of
        0: (ptr                  : PAnsiChar);
        // NID_pkcs7_data
        1: (data                 : PASN1_OCTET_STRING);
        // NID_pkcs7_signed
        2: (sign                 : PPKCS7_SIGNED);
        // NID_pkcs7_enveloped
        3: (enveloped            : PPKCS7_ENVELOPE);
        // NID_pkcs7_signedAndEnveloped
        4: (signed_and_enveloped : PPKCS7_SIGN_ENVELOPE);
        // NID_pkcs7_digest
        5: (digest               : PPKCS7_DIGEST);
        // NID_pkcs7_encrypted
        6: (encrypted            : PPKCS7_ENCRYPT);
        // Anything else
        7: (other                : PASN1_TYPE);
    end;
    PPPKCS7 = ^PPKCS7;
    { Danger ends } {AG}

    TPKCS12_st = packed record                          //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS12 = ^TPKCS12_st;
    PPPKCS12 = ^PPKCS12;


    TV3_EXT_CTX_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PV3_EXT_CTX = ^TV3_EXT_CTX_st;

    // 0.9.7g, 0.9.8a, 0.9.8e
    TCONF_VALUE = record
        Section : PAnsiChar;
        Name    : PAnsiChar;
        Value   : PAnsiChar;
    end;
    PCONF_VALUE = ^TCONF_VALUE;

     //used in old PostConnectionCheck()  {11/07/05 AG}
    { TCONF_VALUE_STACK_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PCONF_VALUE_STACK = ^TCONF_VALUE_STACK_st; }

    TASN1_ITEM_st  = packed record
        Dummy : array [0..0] of Byte;
    end;
    PASN1_ITEM  = ^TASN1_ITEM_st;

    PASN1_ITEM_EXP     = function: PASN1_ITEM; cdecl;
    PX509V3_EXT_NEW    = function: Pointer; cdecl;
    PX509V3_EXT_FREE   = procedure(Arg: Pointer); cdecl;
    PX509V3_EXT_D2I    = function(Arg1: Pointer; Arg2: PPAnsiChar; Arg3: LongInt): Pointer; cdecl;
    PX509V3_EXT_I2D    = function(Arg1: Pointer; Arg2: PPAnsiChar): Integer; cdecl;
    PX509V3_EXT_I2S    = function(X509V3_EXT_METHOD: Pointer; Ext: Pointer): PAnsiChar; cdecl;
    PX509V3_EXT_S2I    = function(X509V3_EXT_METHOD: Pointer; Ctx: PV3_EXT_CTX; S: PAnsiChar): Pointer; cdecl;
    PX509V3_EXT_I2V    = function(X509V3_EXT_METHOD: Pointer; Ext: Pointer; ExtList: PSTACK): PSTACK; cdecl;
    PX509V3_EXT_V2I    = function(X509V3_EXT_METHOD: Pointer; Ctx: PV3_EXT_CTX; Values: PSTACK): Pointer; cdecl;
    PX509V3_EXT_I2R    = function(X509V3_EXT_METHOD: Pointer; Ext: Pointer; Output: PBIO; Indent: Integer): Integer; cdecl;
    PX509V3_EXT_R2I    = function(X509V3_EXT_METHOD: Pointer; Ctx: PV3_EXT_CTX; S: PAnsiChar): Pointer; cdecl;

    // V3 extension structure 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TX509V3_EXT_METHOD = record // struct v3_ext_method
        ext_nid   : Integer;
        ext_flags : Integer;

        // If this is set the following four fields are ignored - since v0.9.0.7
        it        : PASN1_ITEM_EXP;                              //AG

        // Old style ASN1 calls
        ext_new   : PX509V3_EXT_NEW;
        ext_free  : PX509V3_EXT_FREE;
        d2i       : PX509V3_EXT_D2I;
        i2d       : PX509V3_EXT_I2D;
        // The following pair is used for string extensions
        i2s       : PX509V3_EXT_I2S;
        s2i       : PX509V3_EXT_S2I;
        // The following pair is used for multi-valued extensions
        i2v       : PX509V3_EXT_I2V;
        v2i       : PX509V3_EXT_V2I;
        // The following are used for raw extensions
        i2r       : PX509V3_EXT_I2R;
        r2i       : PX509V3_EXT_R2I;
        // Any extension specific data
        usr_data  : Pointer;
    end;
    PX509V3_EXT_METHOD = ^TX509V3_EXT_METHOD;

  { V8.27  The valid handshake states (one for each type message sent and one for each
           type of message received). There are also two "special" states:
     TLS = TLS or DTLS state
     DTLS = DTLS specific state
     CR/SR = Client Read/Server Read
     CW/SW = Client Write/Server Write
       The "special" states are:
     TLS_ST_BEFORE = No handshake has been initiated yet
     TLS_ST_OK = A handshake has been successfully completed }

    TSslHandshakeState = (       { V8.27 OSSL_HANDSHAKE_STATE for SSL_get_state }
        TLS_ST_Before,
        TLS_ST_OK,
        DTLS_ST_CR_Hello_Verify_Request,
        TLS_ST_CR_Srvr_Hello,
        TLS_ST_CR_Cert,
        TLS_ST_CR_Cert_Status,
        TLS_ST_CR_Key_Exch,
        TLS_ST_CR_Cert_Req,
        TLS_ST_CR_Srvr_Done,
        TLS_ST_CR_Session_Ticket,
        TLS_ST_CR_Change,
        TLS_ST_CR_Finished,
        TLS_ST_CW_Client_Hello,
        TLS_ST_CW_Cert,
        TLS_ST_CW_Key_Exch,
        TLS_ST_CW_Cert_Verify,
        TLS_ST_CW_Change,
        TLS_ST_CW_Next_Proto,
        TLS_ST_CW_Finished,
        TLS_ST_SW_Hello_Req,
        TLS_ST_SR_Client_Hello,
        DTLS_ST_SW_Hello_Verify_Request,
        TLS_ST_SW_Server_Hello,
        TLS_ST_SW_Cert,
        TLS_ST_SW_Key_Exch,
        TLS_ST_SW_Cert_Req,
        TLS_ST_SW_Server_Done,
        TLS_ST_SR_Cert,
        TLS_ST_SR_Key_Exch,
        TLS_ST_SR_Cert_Verify,
        TLS_ST_SR_Next_Proto,
        TLS_ST_SR_Change,
        TLS_ST_SR_Finished,
        TLS_ST_SW_Session_Ticket,
        TLS_ST_SW_Cert_Status,
        TLS_ST_SW_Change,
        TLS_ST_SW_Finished);

const
 { V8.27 values for handshake SSL_state up to 1.1.0, no longer used }
    SSL_ST_CONNECT                              = $1000;
    SSL_ST_ACCEPT                               = $2000;
    SSL_ST_MASK                                 = $0FFF;
    SSL_ST_INIT                                 = (SSL_ST_CONNECT or SSL_ST_ACCEPT);
    SSL_ST_BEFORE                               = $4000;
    SSL_ST_OK                                   = $03;
    SSL_ST_RENEGOTIATE                          = ($04 or SSL_ST_INIT);

type
    TPem_password_cb = function(Buf      : PAnsiChar;
                                Num      : Integer;
                                RWFlag   : Integer;
                                UserData : Pointer) : Integer; cdecl;

    TRSA_genkey_cb = procedure(N1, N2 : Integer;     //AG
                               cb_arg : Pointer); cdecl;

    TSetVerify_cb = function(Ok : Integer; StoreCtx : PX509_STORE_CTX) : Integer; cdecl;
    TSetInfo_cb   = procedure(const ssl: PSSL; CbType: Integer; Val: Integer); cdecl;

    TNew_session_cb = function(const Ssl : PSSL; Sess : PSSL_SESSION): Integer; cdecl;
    PNew_session_cb = ^TNew_session_cb;
    TRemove_session_cb = procedure(const Ctx : PSSL_CTX; Sess : PSSL_SESSION); cdecl;
    PRemove_session_cb = ^TRemove_session_cb;
    TGet_session_cb = function(const Ssl : PSSL; SessID : Pointer; IdLen : Integer; Ref : PInteger) : PSSL_SESSION; cdecl;
    PGet_session_cb = ^TGet_session_cb;
    TClient_cert_cb = function (Ssl : PSSL; X509 : PPX509; PKEY : PPEVP_PKEY): Integer; cdecl;
    PClient_cert_cb = ^TClient_cert_cb;

    TCallback_ctrl_fp = procedure (p : Pointer); cdecl;
    TSsl_servername_cb = function (s: PSSL; var ad: Integer; arg: Pointer): Integer; cdecl;

    TProto_msg_cb = function (write_p, version, content_type: integer;
              buf: PAnsiChar; size_t: integer; ssl: PSSL; arg: Pointer): Integer; cdecl;   { V8.40 handshake protocol message callback }

    TSecurity_level_cb = function  (s: PSSL; ctx: PSSL_CTX; op, bits, nid: integer; other, ex: Pointer): Integer; cdecl;  { V8.40 security level callback }

const
    SSL2_VERSION                                = $0002;
    SSL2_VERSION_MAJOR                          = $00;
    SSL2_VERSION_MINOR                          = $02;

    SSL3_VERSION                                = $0300;
    SSL3_VERSION_MAJOR                          = $03;
    SSL3_VERSION_MINOR                          = $00;

    TLS1_VERSION                                = $0301;
    TLS1_VERSION_MAJOR                          = $03;
    TLS1_VERSION_MINOR                          = $01;

    TLS1_1_VERSION                              = $0302;  // V8.01
    TLS1_1_VERSION_MAJOR                        = $03;    // V8.01
    TLS1_1_VERSION_MINOR                        = $02;    // V8.01

    TLS1_2_VERSION                              = $0303;  // V8.01
    TLS1_2_VERSION_MAJOR                        = $03;    // V8.01
    TLS1_2_VERSION_MINOR                        = $03;    // V8.01

    TLS1_3_VERSION                              = $0304;  // V8.40
    TLS1_3_VERSION_MAJOR                        = $03;    // V8.40
    TLS1_3_VERSION_MINOR                        = $04;    // V8.40

    TLS_MAX_VERSION                             = TLS1_2_VERSION;  // V8.27
    TLS_ANY_VERSION                             = $10000;          // V8.27

{$IFNDEF NO_DEBUG_LOG}
{ V8.40 literals for Protocol Message debugging }
type
    TLitLookups = record
        S: String;
        L: integer;
    end ;

const        
    LitsSslVersions:  array[0..4] of TLitLookups = (
        (S: 'SSL 3.0'; L: SSL3_VERSION),
        (S: 'TLS 1.0'; L: TLS1_VERSION),
        (S: 'TLS 1.1'; L: TLS1_1_VERSION),
        (S: 'TLS 1.2'; L: TLS1_2_VERSION),
        (S: 'TLS 1.3'; L: TLS1_3_VERSION) );


    LitsAlertTypes:  array[0..28] of TLitLookups = (
        (S: 'Close Notify'; L:  0),
        (S: 'Unexpected Message'; L:  10),
        (S: 'Bad Record Mac'; L:  20),
        (S: 'Decryption Failed'; L:  21),
        (S: 'Record Overflow'; L:  22),
        (S: 'Decompression Failure'; L:  30),
        (S: 'Handshake Failure'; L:  40),
        (S: 'Bad Certificate'; L:  42),
        (S: 'Unsupported Certificate'; L:  43),
        (S: 'Certificate Revoked'; L:  44),
        (S: 'Certificate Expired'; L:  45),
        (S: 'Certificate Unknown'; L:  46),
        (S: 'Illegal Parameter'; L:  47),
        (S: 'Unknown CA'; L:  48),
        (S: 'Access Denied'; L:  49),
        (S: 'Decode Error'; L:  50),
        (S: 'Decrypt Error'; L:  51),
        (S: 'Export Restriction'; L:  60),
        (S: 'Protocol Version'; L:  70),
        (S: 'Insufficient Security'; L:  71),
        (S: 'Internal Error'; L:  80),
        (S: 'User Cancelled'; L:  90),
        (S: 'No Renegotiation'; L:  100),
        (S: 'Unsupported Extension'; L:  110),
        (S: 'Certificate Unobtainable'; L:  111),
        (S: 'Unrecognized Name'; L:  112),
        (S: 'Bad Certificate Status Response'; L:  113),
        (S: 'Bad Certificate Hash Value'; L:  114),
        (S: 'Unknown PSK Identity'; L:  115) ) ;

    LitsHandshake:  array[0..14] of TLitLookups = (
        (S: 'Hello Request'; L:  0),
        (S: 'Client Hello'; L:  1),
        (S: 'Server Hello'; L:  2),
        (S: 'Hello Verify Request'; L:  3),
        (S: 'New Session Ticket'; L:  4),
        (S: 'Certificate'; L:  11),
        (S: 'Server Key Exchange'; L:  12),
        (S: 'Certificate Request'; L:  13),
        (S: 'Server Hello Done'; L:  14),
        (S: 'Certificate Verify'; L:  15),
        (S: 'Client Key Exchange'; L:  16),
        (S: 'Finished'; L:  20),
        (S: 'Certificate URL'; L:  21),
        (S: 'Certificate Status'; L:  22),
        (S: 'Supplemental Data'; L:  23) ) ;

{$ENDIF}

const

 {   DTLS1_2_VERSION is for UDP, sorry not supported yet }

    BIO_NOCLOSE                                 = 0;
    BIO_CLOSE                                   = 1;
    SSL_ERROR_NONE                              = 0;
    SSL_ERROR_SSL                               = 1;
    SSL_ERROR_WANT_READ                         = 2;
    SSL_ERROR_WANT_WRITE                        = 3;
    SSL_ERROR_WANT_X509_LOOKUP                  = 4;
    SSL_ERROR_SYSCALL                           = 5;
    SSL_ERROR_ZERO_RETURN                       = 6;
    SSL_ERROR_WANT_CONNECT                      = 7;
    SSL_ERROR_WANT_ACCEPT                       = 8;

    X509_FILETYPE_PEM                           = 1;
    X509_FILETYPE_ASN1                          = 2;
    X509_FILETYPE_DEFAULT                       = 3;

    X509_EXT_PACK_UNKNOWN                       = 1;
    X509_EXT_PACK_STRING                        = 2;
    SSL_FILETYPE_ASN1                           = X509_FILETYPE_ASN1;
    SSL_FILETYPE_PEM                            = X509_FILETYPE_PEM;
    SSL_VERIFY_NONE                             = 0;
    SSL_VERIFY_PEER                             = 1;
    SSL_VERIFY_FAIL_IF_NO_PEER_CERT             = 2;
    SSL_VERIFY_CLIENT_ONCE                      = 4;

    { V8.27 Flags for building certificate chains )
    { treat any existing certificates as untrusted CAs }
    SSL_BUILD_CHAIN_FLAG_UNTRUSTED              = $00000001;
    { Don't include root CA in chain }
    SSL_BUILD_CHAIN_FLAG_NO_ROOT                = $00000002;
    { Just check certificates already there }
    SSL_BUILD_CHAIN_FLAG_CHECK                  = $00000004;
    { Ignore verification errors }
    SSL_BUILD_CHAIN_FLAG_IGNORE_ERROR           = $00000008;
    { clear verification errors from queue }
    SSL_BUILD_CHAIN_FLAG_CLEAR_ERROR            = $00000010;

    { Removed 12/07/05 - due to changes in v0.9.8a - restored and corrected V8.01 }
    SSL_CTRL_NEED_TMP_RSA                       = 1;
    SSL_CTRL_SET_TMP_RSA                        = 2;
    SSL_CTRL_SET_TMP_DH                         = 3;
    SSL_CTRL_SET_TMP_ECDH                       = 4;
    SSL_CTRL_SET_TMP_RSA_CB                     = 5;
    SSL_CTRL_SET_TMP_DH_CB                      = 6;
    SSL_CTRL_SET_TMP_ECDH_CB                    = 7;
    SSL_CTRL_GET_SESSION_REUSED                 = 8;
    SSL_CTRL_GET_CLIENT_CERT_REQUEST            = 9;
    SSL_CTRL_GET_NUM_RENEGOTIATIONS             = 10;
    SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS           = 11;
    SSL_CTRL_GET_TOTAL_RENEGOTIATIONS           = 12;
    SSL_CTRL_GET_FLAGS                          = 13;
    SSL_CTRL_EXTRA_CHAIN_CERT                   = 14;
    SSL_CTRL_SET_MSG_CALLBACK                   = 15;
    SSL_CTRL_SET_MSG_CALLBACK_ARG               = 16;
    SSL_CTRL_SET_MTU                            = 17; // only applies to datagram connections

    { These constants will be set dynamically in IcsLibeay.Load() } //12/07/05 added   V8.01 not needed
//    SSL_CTRL_EXTRA_CHAIN_CERT    : Integer = 12; // v.0.9.7 - Ssl.h SSL_CTRL_EXTRA_CHAIN_CERT;
//    SSL_CTRL_GET_SESSION_REUSED  : Integer = 6; // v.0.9.7  - Ssl.h SSL_CTRL_GET_SESSION_REUSED

    // stats
    SSL_CTRL_SESS_NUMBER                        = 20;
    SSL_CTRL_SESS_CONNECT                       = 21;
    SSL_CTRL_SESS_CONNECT_GOOD                  = 22;
    SSL_CTRL_SESS_CONNECT_RENEGOTIATE           = 23;
    SSL_CTRL_SESS_ACCEPT                        = 24;
    SSL_CTRL_SESS_ACCEPT_GOOD                   = 25;
    SSL_CTRL_SESS_ACCEPT_RENEGOTIATE            = 26;
    SSL_CTRL_SESS_HIT                           = 27;
    SSL_CTRL_SESS_CB_HIT                        = 28;
    SSL_CTRL_SESS_MISSES                        = 29;
    SSL_CTRL_SESS_TIMEOUTS                      = 30;
    SSL_CTRL_SESS_CACHE_FULL                    = 31;
    SSL_CTRL_OPTIONS                            = 32;
    SSL_CTRL_MODE                               = 33;
    SSL_CTRL_GET_READ_AHEAD                     = 40;
    SSL_CTRL_SET_READ_AHEAD                     = 41;
    SSL_CTRL_SET_SESS_CACHE_SIZE                = 42;
    SSL_CTRL_GET_SESS_CACHE_SIZE                = 43;
    SSL_CTRL_SET_SESS_CACHE_MODE                = 44;
    SSL_CTRL_GET_SESS_CACHE_MODE                = 45;
    SSL_CTRL_GET_MAX_CERT_LIST                  = 50;   // V8.01
    SSL_CTRL_SET_MAX_CERT_LIST                  = 51;   // V8.01
    SSL_CTRL_SET_MAX_SEND_FRAGMENT              = 52;   // V8.01
 { TLSXEXT stuff later }
    SSL_CTRL_GET_RI_SUPPORT                     = 76; { 0.9.8n }
    SSL_CTRL_CLEAR_OPTIONS                      = 77; { 0.9.8n }
    SSL_CTRL_CLEAR_MODE                         = 78;   // V8.01
    SSL_CTRL_GET_EXTRA_CHAIN_CERTS              = 82;   // V8.01
    SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS            = 83;   // V8.01
    SSL_CTRL_CHAIN                              = 88;   // V8.01
    SSL_CTRL_CHAIN_CERT                         = 89;   // V8.01
    SSL_CTRL_GET_CURVES                         = 90;   // V8.01
    SSL_CTRL_SET_CURVES                         = 91;   // V8.01
    SSL_CTRL_SET_CURVES_LIST                    = 92;   // V8.01
    SSL_CTRL_GET_SHARED_CURVE                   = 93;   // V8.01
    SSL_CTRL_SET_ECDH_AUTO                      = 94;   // V8.01
    SSL_CTRL_SET_SIGALGS                        = 97;   // V8.01
    SSL_CTRL_SET_SIGALGS_LIST                   = 98;   // V8.01
    SSL_CTRL_CERT_FLAGS                         = 99;   // V8.01
    SSL_CTRL_CLEAR_CERT_FLAGS                   = 100;   // V8.01
    SSL_CTRL_SET_CLIENT_SIGALGS                 = 101;   // V8.01
    SSL_CTRL_SET_CLIENT_SIGALGS_LIST            = 102;   // V8.01
    SSL_CTRL_GET_CLIENT_CERT_TYPES              = 103;   // V8.01
    SSL_CTRL_SET_CLIENT_CERT_TYPES              = 104;   // V8.01
    SSL_CTRL_BUILD_CERT_CHAIN                   = 105;   // V8.01
    SSL_CTRL_SET_VERIFY_CERT_STORE              = 106;   // V8.01
    SSL_CTRL_SET_CHAIN_CERT_STORE               = 107;   // V8.01
    SSL_CTRL_GET_PEER_SIGNATURE_NID             = 108;   // V8.01
    SSL_CTRL_GET_SERVER_TMP_KEY                 = 109;   // V8.01
    SSL_CTRL_GET_RAW_CIPHERLIST                 = 110;   // V8.01
    SSL_CTRL_GET_EC_POINT_FORMATS               = 111;   // V8.01
    SSL_CTRL_GET_CHAIN_CERTS                    = 115;   // V8.01
    SSL_CTRL_SELECT_CURRENT_CERT                = 116;   // V8.01
    SSL_CTRL_SET_CURRENT_CERT                   = 117;   // V8.01
    SSL_CTRL_SET_DH_AUTO                        = 118;   // V8.27
    SSL_CTRL_CHECK_PROTO_VERSION                = 119;   // V8.01
    DTLS_CTRL_SET_LINK_MTU                      = 120;   // V8.01
    DTLS_CTRL_GET_LINK_MIN_MTU                  = 121;   // V8.01
    SSL_CTRL_GET_EXTMS_SUPPORT                  = 122;   // V8.27
    SSL_CTRL_SET_MIN_PROTO_VERSION              = 123;   // V8.27
    SSL_CTRL_SET_MAX_PROTO_VERSION              = 124;   // V8.27
    SSL_CTRL_SET_SPLIT_SEND_FRAGMENT            = 125;   // V8.27
    SSL_CTRL_SET_MAX_PIPELINES                  = 126;   // V8.27

    SSL_CERT_SET_FIRST                          = 1;   // V8.27
    SSL_CERT_SET_NEXT                           = 2;   // V8.27
    SSL_CERT_SET_SERVER                         = 3;   // V8.27

    SSL_OP_MICROSOFT_SESS_ID_BUG                = $00000001;
    SSL_OP_NETSCAPE_CHALLENGE_BUG               = $00000002;
    SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG     = $00000008;
    SSL_OP_TLSEXT_PADDING                       = $00000010;   // V8.01
    SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG          = $00000000;   // gone V8.01
    SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER           = $00000020;
    SSL_OP_SAFARI_ECDHE_ECDSA_BUG               = $00000040;   // V8.01
    SSL_OP_MSIE_SSLV2_RSA_PADDING               = $00000000;   //gone V8.01
    SSL_OP_SSLEAY_080_CLIENT_DH_BUG             = $00000080;
    SSL_OP_TLS_D5_BUG                           = $00000100;
    SSL_OP_TLS_BLOCK_PADDING_BUG                = $00000200;

    // Disable SSL 3.0/TLS 1.0 CBC vulnerability workaround that was added
    // in OpenSSL 0.9.6d.  Usually (depending on the application protocol)
    // the workaround is not needed.  Unfortunately some broken SSL/TLS
    //implementations cannot handle it at all, which is why we include
    //it in SSL_OP_ALL.

    SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS          = $00000800;
    //SSL_OP_ALL: various bug workarounds that should be rather harmless.
    //This used to be 0x000FFFFFL before 0.9.7.
    // 0.9.8h, 0.9.8n, 0.9.8e, 0.9.7g $00000FFF
    SSL_OP_ALL                                  = $00000BFF;    // V8.01
    //SSL_OP_ALL                                  = $80000FFF; 1.0.0d

    //* DTLS options */ since 0.9.8
    SSL_OP_NO_QUERY_MTU                         = $00001000;
    //Turn on Cookie Exchange (on relevant for servers)
    SSL_OP_COOKIE_EXCHANGE                      = $00002000;

    // Don't use RFC4507 ticket extension
    SSL_OP_NO_TICKET                            = $00004000;

    // Use Cisco's "speshul" version of DTLS_BAD_VER (as client)
    SSL_OP_CISCO_ANYCONNECT                     = $00008000;    // V8.01

    // As server, disallow session resumption on renegotiation
    SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION  = $00010000;

    // Don't use compression even if supported
    SSL_OP_NO_COMPRESSION                          = $00020000; // 1.0.0x

    // Permit unsafe legacy renegotiation { 0.9.8n }
    // which can be set with SSL_CTX_set_options(). This is really
    // not recommended unless you know what you are doing.
    SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION    = $00040000;

    // If set, always create a new key when using tmp_ecdh parameters
    SSL_OP_SINGLE_ECDH_USE                       = $00080000;  // V8.02

    // If set, always create a new key when using tmp_ecdh parameters
    SSL_OP_SINGLE_DH_USE                        = $00100000;

   // Set to always use the tmp_rsa key when doing RSA operations,
   // even when this violates protocol specs
    SSL_OP_EPHEMERAL_RSA                        = $00200000;

    // Set on servers to choose the cipher according to the server's
    // preferences */
    SSL_OP_CIPHER_SERVER_PREFERENCE             = $00400000;

    // If set, a server will allow a client to issue a SSLv3.0 version number
    // as latest version supported in the premaster secret, even when TLSv1.0
    // (version 3.1) was announced in the client hello. Normally this is
    // forbidden to prevent version rollback attacks.
    SSL_OP_TLS_ROLLBACK_BUG                     = $00800000;

    SSL_OP_NO_SSLv2                             = $01000000;
    SSL_OP_NO_SSLv3                             = $02000000;
    SSL_OP_NO_TLSv1                             = $04000000;
    SSL_OP_NO_TLSv1_2                           = $08000000;     // V8.01
    SSL_OP_NO_TLSv1_1                           = $10000000;     // V8.01

// These next two were never actually used for anything since SSLeay
// zap so we have some more flags.
    SSL_OP_PKCS1_CHECK_1                        = $00000000;    // gone V8.01
    SSL_OP_PKCS1_CHECK_2                        = $00000000;    // gone V8.01

    SSL_OP_NETSCAPE_CA_DN_BUG                   = $20000000;

    SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG      = $40000000;
    // Make server add server-hello extension from early version of
    // cryptopro draft, when GOST ciphersuite is negotiated.
    // Required for interoperability with CryptoPro CSP 3.x
    SSL_OP_CRYPTOPRO_TLSEXT_BUG                 = $80000000; // 1.0.0x


    SSL_MODE_ENABLE_PARTIAL_WRITE               = $00000001;

    SSL_SESS_CACHE_OFF                          = $0000;
    SSL_SESS_CACHE_CLIENT                       = $0001;
    SSL_SESS_CACHE_SERVER                       = $0002;
    SSL_SESS_CACHE_BOTH                         = (SSL_SESS_CACHE_CLIENT or SSL_SESS_CACHE_SERVER);
    SSL_SESS_CACHE_NO_AUTO_CLEAR                = $0080;
    //* enough comments already ... see SSL_CTX_set_session_cache_mode(3) */
    SSL_SESS_CACHE_NO_INTERNAL_LOOKUP           = $0100;
    SSL_SESS_CACHE_NO_INTERNAL_STORE            = $0200;
    SSL_SESS_CACHE_NO_INTERNAL                  = (SSL_SESS_CACHE_NO_INTERNAL_LOOKUP or SSL_SESS_CACHE_NO_INTERNAL_STORE);

    SSL_SESSION_CACHE_MAX_SIZE_DEFAULT          = (1024 * 20);

    SSL_CB_LOOP                                 = 1;
    SSL_CB_EXIT                                 = 2;
    SSL_CB_READ                                 = 4;
    SSL_CB_WRITE                                = 8;
    SSL_CB_ALERT                                = $4000; // used in callback
    SSL_CB_READ_ALERT                           = (SSL_CB_ALERT or SSL_CB_READ);
    SSL_CB_WRITE_ALERT                          = (SSL_CB_ALERT or SSL_CB_WRITE);
    SSL_CB_ACCEPT_LOOP                          = (SSL_ST_ACCEPT or SSL_CB_LOOP);
    SSL_CB_ACCEPT_EXIT                          = (SSL_ST_ACCEPT or SSL_CB_EXIT);
    SSL_CB_CONNECT_LOOP                         = (SSL_ST_CONNECT or SSL_CB_LOOP);
    SSL_CB_CONNECT_EXIT                         = (SSL_ST_CONNECT or SSL_CB_EXIT);
    SSL_CB_HANDSHAKE_START                      = $10;
    SSL_CB_HANDSHAKE_DONE                       = $20;

    SSL_NOTHING                                 = 1;
    SSL_WRITING                                 = 2;
    SSL_READING                                 = 3;
    SSL_X509_LOOKUP                             = 4;

    // Used in SSL_set_shutdown()/SSL_get_shutdown()
    SSL_SENT_SHUTDOWN                           = 1;
    SSL_RECEIVED_SHUTDOWN                       = 2;

    X509_TRUST_COMPAT                           = 1; //AG
    X509_TRUST_SSL_CLIENT                       = 2; //AG
    X509_TRUST_SSL_SERVER                       = 3; //AG
    X509_TRUST_EMAIL                            = 4; //AG
    X509_TRUST_OBJECT_SIGN                      = 5; //AG
    X509_TRUST_OCSP_SIGN                        = 6; //AG
    X509_TRUST_OCSP_REQUEST                     = 7; //AG

    SSL_MAX_SSL_SESSION_ID_LENGTH               = 32; //AG
    SSL_MAX_SID_CTX_LENGTH                      = 32; //AG

    {* ExtensionType values from RFC 3546 *}
    TLSEXT_TYPE_server_name                     = 0;
    TLSEXT_TYPE_max_fragment_length             = 1;
    TLSEXT_TYPE_client_certificate_url          = 2;
    TLSEXT_TYPE_trusted_ca_keys                 = 3;
    TLSEXT_TYPE_truncated_hmac                  = 4;
    TLSEXT_TYPE_status_request                  = 5;
    TLSEXT_TYPE_elliptic_curves                 = 10;
    TLSEXT_TYPE_ec_point_formats                = 11;
    TLSEXT_TYPE_session_ticket                  = 35;

    TLSEXT_MAXLEN_host_name                     = 255;
    TLSEXT_NAMETYPE_host_name                   = 0;

    SSL_CTRL_SET_TLSEXT_SERVERNAME_CB           = 53;
    SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG          = 54;
    SSL_CTRL_SET_TLSEXT_HOSTNAME                = 55;
    SSL_CTRL_SET_TLSEXT_DEBUG_CB                = 56;
    SSL_CTRL_SET_TLSEXT_DEBUG_ARG               = 57;
    SSL_CTRL_GET_TLSEXT_TICKET_KEYS             = 58;   // V8.01
    SSL_CTRL_SET_TLSEXT_TICKET_KEYS             = 59;   // V8.01
    SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT        = 60;   // V8.01
    SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB     = 61;   // V8.01
    SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB_ARG   = 62;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB           = 63;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG       = 64;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE         = 65;   // V8.01
    SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS         = 66;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS         = 67;   // V8.01
    SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS          = 68;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS          = 69;   // V8.01
    SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP    = 70;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP    = 71;   // V8.01
    SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB           = 72;   // V8.01
    SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB        = 75;   // V8.01
    SSL_CTRL_SET_SRP_VERIFY_PARAM_CB            = 76;   // V8.01
    SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB         = 77;   // V8.01
    SSL_CTRL_SET_SRP_ARG                        = 78;   // V8.01
    SSL_CTRL_SET_TLS_EXT_SRP_USERNAME           = 79;   // V8.01
    SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH           = 80;   // V8.01
    SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD           = 81;   // V8.01
    SSL_CTRL_TLS_EXT_SEND_HEARTBEAT             = 85;   // V8.01
    SSL_CTRL_GET_TLS_EXT_HEARTBEAT_PENDING      = 86;   // V8.01
    SSL_CTRL_SET_TLS_EXT_HEARTBEAT_NO_REQUESTS  = 87;   // V8.01

    SSL_TLSEXT_ERR_OK                           = 0;
    SSL_TLSEXT_ERR_ALERT_WARNING                = 1;
    SSL_TLSEXT_ERR_ALERT_FATAL                  = 2;
    SSL_TLSEXT_ERR_NOACK                        = 3;

const
    f_BIO_f_ssl :                              function : PBIO_METHOD; cdecl = nil;
    f_SSL_CIPHER_description :                 function(Cipher: Pointer; buf: PAnsiChar; size: Integer): PAnsiChar; cdecl = nil;
    f_SSL_CIPHER_get_bits :                    function(Cipher, Alg_Bits: Pointer): Integer; cdecl = nil;
    f_SSL_CIPHER_get_name :                    function(Cipher: Pointer): PAnsiChar; cdecl = nil;
    f_SSL_CTX_add_client_CA :                  function(C: PSSL_CTX; CaCert: PX509): Integer; cdecl = nil; //AG
    f_SSL_CTX_callback_ctrl:                   function(ctx: PSSL_CTX; cb_id: Integer; fp: TCallback_ctrl_fp): Longint; cdecl = nil;
    f_SSL_CTX_check_private_key :              function(Ctx: PSSL_CTX): integer; cdecl = nil;     { V8.40 }
    f_SSL_CTX_ctrl :                           function(C: PSSL_CTX; Cmd: Integer; LArg: LongInt; PArg: PAnsiChar): LongInt; cdecl = nil;
    f_SSL_CTX_free :                           procedure(C: PSSL_CTX); cdecl = nil;
    f_SSL_CTX_get0_certificate :               function(Ctx: PSSL_CTX): PX509; cdecl = nil;       { V8.40 }
    f_SSL_CTX_get0_param :                     function(Ctx: PSSL_CTX): PX509_VERIFY_PARAM; cdecl = nil;                { V8.39 1.0.2 }
    f_SSL_CTX_get0_privatekey :                function(Ctx: PSSL_CTX): PEVP_PKEY; cdecl = nil;   { V8.40 }
    f_SSL_CTX_get0_security_ex_data :          function(Ctx: PSSL_CTX): Pointer; cdecl = nil;            { V8.40 }
    f_SSL_CTX_get_cert_store :                 function(const Ctx: PSSL_CTX): PX509_STORE; cdecl = nil; //AG
    f_SSL_CTX_get_client_cert_cb:              function(CTX: PSSL_CTX): TClient_cert_cb; cdecl = nil; //AG
    f_SSL_CTX_get_ex_data :                    function(const C: PSSL_CTX; Idx: Integer): PAnsiChar; cdecl = nil;
    f_SSL_CTX_get_security_level :             function(Ctx: PSSL_CTX): Integer; cdecl = nil;             { V8.40 }
    f_SSL_CTX_get_verify_depth :               function(const ctx: PSSL_CTX): Integer; cdecl = nil; //AG
    f_SSL_CTX_get_verify_mode :                function(const C: PSSL_CTX): Integer; cdecl = nil; //AG
    f_SSL_CTX_load_verify_locations :          function(C: PSSL_CTX; const FileName: PAnsiChar; const SearchPath: PAnsiChar): Integer; cdecl = nil;
    f_SSL_CTX_new :                            function(Meth: PSSL_METHOD): PSSL_CTX; cdecl = nil;
    f_SSL_CTX_sess_get_get_cb:                 function(CTX: PSSL_CTX): TGet_session_cb; cdecl = nil; //AG
    f_SSL_CTX_sess_get_new_cb:                 function (CTX: PSSL_CTX): TNew_session_cb; cdecl = nil; //AG
    f_SSL_CTX_sess_get_remove_cb:              function(CTX: PSSL_CTX): TRemove_session_cb; cdecl = nil; //AG
    f_SSL_CTX_sess_set_get_cb:                 procedure(Ctx: PSSL_CTX; CB: TGet_session_cb); cdecl = nil; //AG
    f_SSL_CTX_sess_set_new_cb:                 procedure(Ctx: PSSL_CTX; CB: TNew_session_cb); cdecl = nil; //AG
    f_SSL_CTX_sess_set_remove_cb:              procedure(Ctx: PSSL_CTX; CB: TRemove_session_cb); cdecl = nil; //AG
    f_SSL_CTX_set0_security_ex_data :          procedure(Ctx: PSSL_CTX;  ex: Pointer);  cdecl = nil;      { V8.40 }
    f_SSL_CTX_set1_param :                     function(Ctx: PSSL_CTX; vpm: PX509_VERIFY_PARAM): integer; cdecl = nil;  { V8.39 1.0.2 }
    f_SSL_CTX_set_cipher_list :                function(C: PSSL_CTX; CipherString: PAnsiChar): Integer; cdecl = nil;
    f_SSL_CTX_set_client_CA_list :             procedure(C: PSSL_CTX; List: PSTACK_OF_X509_NAME); cdecl = nil; //AG
    f_SSL_CTX_set_client_cert_cb:              procedure(CTX: PSSL_CTX; CB: TClient_cert_cb); cdecl = nil; //AG
    f_SSL_CTX_set_default_passwd_cb :          procedure(C: PSSL_CTX; CallBack: TPem_password_cb); cdecl = nil;
    f_SSL_CTX_set_default_passwd_cb_userdata : procedure(C: PSSL_CTX; UData: Pointer); cdecl = nil;
    f_SSL_CTX_set_default_verify_paths :       function(C: PSSL_CTX): Integer; cdecl = nil;
    f_SSL_CTX_set_ex_data :                    function(C: PSSL_CTX; Idx: Integer; Arg: PAnsiChar): Integer; cdecl = nil;
    f_SSL_CTX_set_info_callback:               procedure(ctx: PSSL_CTX; cb : TSetInfo_cb); cdecl = nil;
    f_SSL_CTX_set_msg_callback :               procedure(Ctx: PSSL_CTX; cb: TProto_msg_cb); cdecl = nil;  { V8.40 }
    f_SSL_CTX_set_security_callback :          procedure(Ctx: PSSL_CTX; cb: TSecurity_level_cb); cdecl = nil;   { V8.40 }
    f_SSL_CTX_set_security_level :             procedure(Ctx: PSSL_CTX; level: Integer); cdecl = nil;     { V8.40 }
    f_SSL_CTX_set_session_id_context :         function(Ctx: PSSL_CTX; const Sid_ctx: PAnsiChar; sid_ctx_len: Integer): Integer; cdecl = nil;
    f_SSL_CTX_set_timeout :                    function(Ctx: PSSL_CTX; Timeout: Longword): Longword; cdecl = nil;
    f_SSL_CTX_set_trust :                      function(C: PSSL_CTX; Trust: Integer): Integer; cdecl = nil; //AG
    f_SSL_CTX_set_verify :                     procedure(C: PSSL_CTX; Mode: Integer; CallBack : TSetVerify_cb); cdecl = nil;
    f_SSL_CTX_set_verify_depth :               procedure(C: PSSL_CTX; Depth: Integer); cdecl = nil;
    f_SSL_CTX_use_PrivateKey :                 function(C: PSSL_CTX; pkey: PEVP_PKEY): Integer; cdecl = nil;
    f_SSL_CTX_use_PrivateKey_file :            function(C: PSSL_CTX; const FileName: PAnsiChar; CertType: Integer): Integer; cdecl = nil;
    f_SSL_CTX_use_certificate :                function(C: PSSL_CTX; Cert: PX509): Integer; cdecl = nil;     { V8.27 }
    f_SSL_CTX_use_certificate_chain_file :     function(C: PSSL_CTX; const FileName: PAnsiChar): Integer; cdecl = nil;
    f_SSL_CTX_use_certificate_file :           function(C: PSSL_CTX; const FileName: PAnsiChar; type_: Integer): Integer; cdecl = nil; //AG
    f_SSL_SESSION_get_id:                      function (const Ses: PSSL_SESSION; var Len: LongInt): PAnsiChar; cdecl = nil; //AG
    f_SSL_SESSION_get_time :                   function(const Sess: PSSL_SESSION): Longword; cdecl = nil;
    f_SSL_SESSION_get_timeout :                function(const Sess: PSSL_SESSION): Longword; cdecl = nil;
    f_SSL_SESSION_set_time :                   function(Sess: PSSL_SESSION; T: Longword): Longword; cdecl = nil;
    f_SSL_SESSION_set_timeout :                function(Sess: PSSL_SESSION; T: Longword): Longword; cdecl = nil;
    f_SSL_accept :                             function(S: PSSL): Integer; cdecl = nil;
    f_SSL_add_client_CA :                      function(ssl: PSSL; CaCert: PX509): Integer; cdecl = nil; //AG
    f_SSL_alert_desc_string_long :             function(value: Integer): PAnsiChar; cdecl = nil;
    f_SSL_alert_type_string_long :             function(value: Integer): PAnsiChar; cdecl = nil;
    f_SSL_callback_ctrl:                       function(s: PSSL; cb_id: Integer; fp: TCallback_ctrl_fp): Longint; cdecl = nil;
    f_SSL_clear :                              procedure(S: PSSL); cdecl = nil;
    f_SSL_connect :                            function(S: PSSL): Integer; cdecl = nil;
    f_SSL_ctrl :                               function(S: PSSL; Cmd: Integer; LArg: LongInt; PArg: Pointer): LongInt; cdecl = nil;
    f_SSL_do_handshake :                       function(S: PSSL): Integer; cdecl = nil; //AG
    f_SSL_free :                               procedure(S: PSSL); cdecl = nil;
    f_SSL_get0_param :                         function(S: PSSL): PX509_VERIFY_PARAM; cdecl = nil;                      { V8.39 1.0.2 }
    f_SSL_get0_security_ex_data :              function(S: PSSL): Pointer; cdecl = nil;                  { V8.40 }
    f_SSL_get1_session :                       function(S: PSSL): PSSL_SESSION; cdecl = nil;
    f_SSL_get1_supported_ciphers :             function(S: PSSL): PSTACK_OF_SSL_CIPHER; cdecl = nil;   { V8.27 }
    f_SSL_get_SSL_CTX:                         function(const S: PSSL): PSSL_CTX; cdecl = nil;
    f_SSL_get_cipher_list :                    function(S: PSSL; Priority: Integer): PAnsiChar; cdecl = nil;  { V8.27 }
    f_SSL_get_ciphers :                        function(S: PSSL): PSTACK_OF_SSL_CIPHER; cdecl = nil;   { V8.27 }
    f_SSL_get_client_CA_list :                 function(const S: PSSL): PSTACK_OF_X509_NAME; cdecl = nil;
    f_SSL_get_client_ciphers :                 function(S: PSSL): PSTACK_OF_SSL_CIPHER; cdecl = nil;   { V8.27 }
    f_SSL_get_current_cipher :                 function(S: PSSL): Pointer; cdecl = nil;
    f_SSL_get_error :                          function(S: PSSL; ErrCode: Integer): Integer; cdecl = nil;
    f_SSL_get_ex_data :                        function(S: PSSL; Idx: Integer): Pointer; cdecl = nil;
    f_SSL_get_ex_data_X509_STORE_CTX_idx:      function: Integer; cdecl = nil;
    f_SSL_get_fd:                              function(S: PSSL): Integer; cdecl = nil; // B.S.
    f_SSL_get_peer_cert_chain :                function(const S: PSSL): PSTACK_OF_X509; cdecl = nil;
    f_SSL_get_peer_certificate :               function(S: PSSL): PX509; cdecl = nil;
    f_SSL_get_rbio :                           function(S: PSSL): PBIO; cdecl = nil;
    f_SSL_get_rfd:                             function(S: PSSL): Integer; cdecl = nil; // B.S.
    f_SSL_get_servername:                      function(const S: PSSL; const type_: Integer): PAnsiChar; cdecl = nil;
    f_SSL_get_servername_type:                 function(const S: PSSL): Integer; cdecl = nil;
    f_SSL_get_session :                        function(S: PSSL): PSSL_SESSION; cdecl = nil;
    f_SSL_get_shutdown :                       function(S: PSSL): Integer; cdecl = nil;
    f_SSL_get_security_level :                 function(S: PSSL): Integer; cdecl = nil;                   { V8.40 }
    f_SSL_get_state :                          function(S: PSSL): TSslHandshakeState; cdecl = nil;   { V8.27 }
    f_SSL_get_verify_depth :                   function(const S: PSSL): Integer; cdecl = nil;
    f_SSL_get_verify_result :                  function(S: PSSL): LongInt; cdecl = nil;
    f_SSL_get_version :                        function(S: PSSL): PAnsiChar; cdecl = nil;
    f_SSL_get_wbio :                           function(S: PSSL): PBIO; cdecl = nil;
    f_SSL_get_wfd:                             function(S: PSSL): Integer; cdecl = nil; // B.S.
    f_SSL_library_init :                       function: Integer; cdecl = nil;
    f_SSL_load_client_CA_file :                function(const FileName: PAnsiChar): PSTACK_OF_X509_NAME; cdecl = nil; //AG
    f_SSL_load_error_strings :                 procedure; cdecl = nil;
    f_SSL_new :                                function(Ctx: PSSL_CTX): PSSL; cdecl = nil;
    f_SSL_read :                               function(S: PSSL; Buf: Pointer; Num: Integer): Integer; cdecl = nil;
    f_SSL_renegotiate :                        function(S: PSSL): Integer; cdecl = nil; //AG
    f_SSL_renegotiate_pending :                function(S: PSSL): Integer; cdecl = nil; //AG
    f_SSL_session_free :                       procedure(Session: PSSL_SESSION); cdecl = nil;
    f_SSL_set0_security_ex_data :              procedure(S: PSSL;  ex: Pointer);  cdecl = nil;            { V8.40 }
    f_SSL_set1_param :                         function(S: PSSL; vpm: PX509_VERIFY_PARAM): integer; cdecl = nil;        { V8.39 1.0.2 }
    f_SSL_set_SSL_CTX:                         function(S: PSSL; ctx: PSSL_CTX): PSSL_CTX; cdecl = nil;
    f_SSL_set_accept_state :                   procedure(S: PSSL); cdecl = nil; //AG
    f_SSL_set_bio :                            procedure(S: PSSL; RBio: PBIO; WBio: PBIO); cdecl = nil;
    f_SSL_set_client_CA_list :                 procedure(s: PSSL; List: PSTACK_OF_X509_NAME); cdecl = nil; //AG
    f_SSL_set_connect_state :                  procedure(S: PSSL); cdecl = nil;
    f_SSL_set_ex_data :                        function(S: PSSL; Idx: Integer; Arg: Pointer): Integer; cdecl = nil;
    f_SSL_set_fd:                              function(S: PSSL; fd: Integer): Integer; cdecl = nil; // B.S.
    f_SSL_set_info_callback :                  procedure(S: PSSL; cb : TSetInfo_cb); cdecl = nil;
    f_SSL_set_msg_callback :                   procedure(S: PSSL; cb: TProto_msg_cb); cdecl = nil;        { V8.40 }
    f_SSL_set_rfd:                             function(S: PSSL; fd: Integer): Integer; cdecl = nil; // B.S.
    f_SSL_set_security_level :                 procedure(S: PSSL; level: Integer); cdecl = nil;           { V8.40 }
    f_SSL_set_security_callback :              procedure(S: PSSL; cb: TSecurity_level_cb); cdecl = nil;   { V8.40 }
    f_SSL_set_session :                        function(S: PSSL; Session: PSSL_SESSION): Integer; cdecl = nil;
    f_SSL_set_session_id_context :             function(S: PSSL; const Sid_ctx: PAnsiChar; sid_ctx_len: Integer): Integer; cdecl = nil;
    f_SSL_set_shutdown :                       procedure(S: PSSL; Mode: Integer); cdecl = nil;
    f_SSL_set_verify :                         procedure(S: PSSL; Mode: Integer; CallBack : TSetVerify_cb); cdecl = nil;
    f_SSL_set_verify_result :                  procedure(S: PSSL; VResult: LongInt); cdecl = nil;
    f_SSL_set_wfd:                             function(S: PSSL; fd: Integer): Integer; cdecl = nil; // B.S.
    f_SSL_shutdown :                           function(S: PSSL): Integer; cdecl = nil;
    f_SSL_state :                              function(S: PSSL): Integer; cdecl = nil;   { V8.27 gone 1.1.0 }
    f_SSL_state_string :                       function(S: PSSL): PAnsiChar; cdecl = nil;    { V8.40 }
    f_SSL_state_string_long :                  function(S: PSSL): PAnsiChar; cdecl = nil;
    f_SSL_version :                            function(const S: PSSL): Integer; cdecl = nil; //AG
    f_SSL_want :                               function(S: PSSL): Integer; cdecl = nil;
    f_SSL_write :                              function(S: PSSL; const Buf: Pointer; Num: Integer): Integer; cdecl = nil;
    f_SSLv23_client_method :                   function: PSSL_METHOD; cdecl = nil;
    f_SSLv23_method :                          function: PSSL_METHOD; cdecl = nil;
    f_SSLv23_server_method :                   function: PSSL_METHOD; cdecl = nil;
    f_SSLv3_client_method :                    function: PSSL_METHOD; cdecl = nil;
    f_SSLv3_method :                           function: PSSL_METHOD; cdecl = nil;
    f_SSLv3_server_method :                    function: PSSL_METHOD; cdecl = nil;
    f_TLS_client_method :                      function: PSSL_METHOD; cdecl = nil;   { V8.27 }
    f_TLS_method :                             function: PSSL_METHOD; cdecl = nil;   { V8.27 }
    f_TLS_server_method :                      function: PSSL_METHOD; cdecl = nil;   { V8.27 }
    f_TLSv1_1_client_method :                  function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_1_method :                         function: PSSL_METHOD; cdecl = nil;    // V8.01 added TLS 1.1 and 1.2
    f_TLSv1_1_server_method :                  function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_2_client_method :                  function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_2_method :                         function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_2_server_method :                  function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_client_method :                    function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_method :                           function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_server_method :                    function: PSSL_METHOD; cdecl = nil;
    f_d2i_SSL_SESSION :                        function(Session: PPSSL_SESSION; const pp: PPAnsiChar; Length: Longword): PSSL_SESSION; cdecl = nil;
    f_i2d_SSL_SESSION :                        function(InSession: PSSL_SESSION; pp: PPAnsiChar): Integer; cdecl = nil;


{$IFNDEF OPENSSL_NO_ENGINE}
    f_SSL_CTX_set_client_cert_engine :         function(Ctx: PSSL_CTX; e: PENGINE): Integer; cdecl = nil; //AG
{$ENDIF}


function SsleayLoad : Boolean;
function SslGetImports (Handle: THandle; List: array of TOSSLImports): string ;  { V8.35 }

{ V8.38 Windows API to check authenticode code signing digital certificate on OpenSSL files }
procedure IcsVerifySslDll (const Fname: string);

// macro functions not exported from DLL
function  f_SSL_CTX_set_options(C: PSSL_CTX; Op: LongInt): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_CTX_get_options(C: PSSL_CTX): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_get_options(S: PSSL): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_set_options(S: PSSL; Op: LongInt): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_clear_options(S: PSSL; Op: LongInt): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_want_read(S: PSSL) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_want_write(S: PSSL) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_want_nothing(S: PSSL) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_want_x509_lookup(S: PSSL) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_session_reused(SSL: PSSL): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_CTX_set_session_cache_mode(Ctx: PSSL_CTX; Mode: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_CTX_sess_set_cache_size(Ctx: PSSL_CTX; CacheSize: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_CTX_add_extra_chain_cert(Ctx: PSSL_CTX; Cert: PX509): Longword; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_CTX_set_tmp_dh(C: PSSL_CTX; DH: Pointer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.01 }
function  f_SSL_CTX_set_tmp_ecdh(C: PSSL_CTX; ECDH: Pointer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.01 }
function  f_SSL_CTX_set_ecdh_auto(C: PSSL_CTX; onoff: integer) : Integer;    { V8.01 }
function  f_SSL_set_tmp_dh(S: PSSL; DH: Pointer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}            { V8.01 }
function  f_SSL_set_tmp_ecdh(S: PSSL; ECDH: Pointer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.01 }
function  f_SSL_set_ecdh_auto(S: PSSL; onoff: integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.01 }
function  f_SSL_CTX_set_min_proto_version(C: PSSL_CTX; version: integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  f_SSL_CTX_set_max_proto_version(C: PSSL_CTX;  version: integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  f_SSL_set_min_proto_version(S: PSSL; version: integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  f_SSL_set_max_proto_version(S: PSSL; version: integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  f_SSL_CTX_set0_chain(C: PSSL_CTX; sk: PSTACK_OF_X509): Integer;  {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  f_SSL_CTX_add0_chain_cert(C: PSSL_CTX; Cert: PX509): Integer;  {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  f_SSL_CTX_get0_chain_certs(C: PSSL_CTX; sk: PPSTACK_OF_X509): Integer;  {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  f_SSL_CTX_clear_chain_certs(C: PSSL_CTX): Integer;   {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  f_SSL_CTX_build_cert_chain(C: PSSL_CTX; flags: integer): Integer;   {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }

function f_SSL_set_tlsext_host_name(const S: PSSL; const name: String): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX; cb: TCallback_ctrl_fp): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_SSL_CTX_set_tlsext_servername_arg(ctx: PSSL_CTX; arg: Pointer): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_SSL_set_tlsext_debug_callback(S: PSSL; cb: TCallback_ctrl_fp): Longint; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_SSL_set_tlsext_debug_arg(S: PSSL; arg: Pointer): Longint; {$IFDEF USE_INLINE} inline; {$ENDIF}

function IcsSslGetState(S: PSSL): TSslHandshakeState;    { V8.27 }
function IcsSslStub: integer;                            { V8.35 }

procedure  f_SSL_CTX_set_msg_callback_arg(Ctx: PSSL_CTX; arg: Pointer);  {$IFDEF USE_INLINE} inline; {$ENDIF}  { V8.40 }
procedure  f_SSL_set_msg_callback_arg(S: PSSL; arg: Pointer); {$IFDEF USE_INLINE} inline; {$ENDIF}             { V8.40 }


// V8.35 all OpenSSL exports now in tables, with versions if only available conditionally
const
    GSSLEAYImports1: array[0..148] of TOSSLImports = (

    (F: @@f_SSL_CTX_get0_certificate;               N: 'SSL_CTX_get0_certificate';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_SSL_CTX_get0_privatekey;                N: 'SSL_CTX_get0_privatekey';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_SSL_CTX_check_private_key;              N: 'SSL_CTX_check_private_key';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }

    (F: @@f_SSL_CTX_set_msg_callback;               N: 'SSL_CTX_set_msg_callback';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_SSL_set_msg_callback;                   N: 'SSL_set_msg_callback';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }

    (F: @@f_SSL_CTX_get_security_level;             N: 'SSL_CTX_get_security_level';                 MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_SSL_CTX_set_security_level;             N: 'SSL_CTX_set_security_level';                 MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_SSL_get_security_level;                 N: 'SSL_get_security_level';                     MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_SSL_set_security_level;                 N: 'SSL_set_security_level';                     MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_SSL_CTX_set_security_callback;          N: 'SSL_CTX_set_security_callback';              MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_SSL_set_security_callback;              N: 'SSL_set_security_callback';                  MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_SSL_CTX_set0_security_ex_data;          N: 'SSL_CTX_set0_security_ex_data';              MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_SSL_CTX_get0_security_ex_data;          N: 'SSL_CTX_get0_security_ex_data';              MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_SSL_get0_security_ex_data;              N: 'SSL_get0_security_ex_data';                  MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_SSL_set0_security_ex_data;              N: 'SSL_set0_security_ex_data';                  MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.40 }

    (F: @@f_BIO_f_ssl;                              N: 'BIO_f_ssl';                                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CIPHER_description;                 N: 'SSL_CIPHER_description';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CIPHER_get_bits;                    N: 'SSL_CIPHER_get_bits';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CIPHER_get_name;                    N: 'SSL_CIPHER_get_name';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_add_client_CA;                  N: 'SSL_CTX_add_client_CA';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_callback_ctrl;                  N: 'SSL_CTX_callback_ctrl';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_ctrl;                           N: 'SSL_CTX_ctrl';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_free;                           N: 'SSL_CTX_free';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_get0_param;                     N: 'SSL_CTX_get0_param';                        MI: OSSL_VER_1002; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@f_SSL_CTX_get_cert_store;                 N: 'SSL_CTX_get_cert_store';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_get_client_cert_cb;             N: 'SSL_CTX_get_client_cert_cb';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_get_ex_data;                    N: 'SSL_CTX_get_ex_data';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_get_verify_depth;               N: 'SSL_CTX_get_verify_depth';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_get_verify_mode;                N: 'SSL_CTX_get_verify_mode';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_load_verify_locations;          N: 'SSL_CTX_load_verify_locations';             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_new;                            N: 'SSL_CTX_new';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_sess_get_get_cb;                N: 'SSL_CTX_sess_get_get_cb';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_sess_get_new_cb;                N: 'SSL_CTX_sess_get_new_cb';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_sess_get_remove_cb;             N: 'SSL_CTX_sess_get_remove_cb';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_sess_set_get_cb;                N: 'SSL_CTX_sess_set_get_cb';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_sess_set_new_cb;                N: 'SSL_CTX_sess_set_new_cb';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_sess_set_remove_cb;             N: 'SSL_CTX_sess_set_remove_cb';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set1_param;                     N: 'SSL_CTX_set1_param';                        MI: OSSL_VER_1002; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@f_SSL_CTX_set_cipher_list;                N: 'SSL_CTX_set_cipher_list';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_client_CA_list;             N: 'SSL_CTX_set_client_CA_list';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_client_cert_cb;             N: 'SSL_CTX_set_client_cert_cb';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_default_passwd_cb;          N: 'SSL_CTX_set_default_passwd_cb';             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_default_passwd_cb_userdata; N: 'SSL_CTX_set_default_passwd_cb_userdata';    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_default_verify_paths;       N: 'SSL_CTX_set_default_verify_paths';          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_ex_data;                    N: 'SSL_CTX_set_ex_data';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_info_callback;              N: 'SSL_CTX_set_info_callback';                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_session_id_context;         N: 'SSL_CTX_set_session_id_context';            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_timeout;                    N: 'SSL_CTX_set_timeout';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_trust;                      N: 'SSL_CTX_set_trust';                         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_verify;                     N: 'SSL_CTX_set_verify';                        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_set_verify_depth;               N: 'SSL_CTX_set_verify_depth';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_use_PrivateKey;                 N: 'SSL_CTX_use_PrivateKey';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_use_PrivateKey_file;            N: 'SSL_CTX_use_PrivateKey_file';               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_use_certificate;                N: 'SSL_CTX_use_certificate';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_use_certificate_chain_file;     N: 'SSL_CTX_use_certificate_chain_file';        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_CTX_use_certificate_file;           N: 'SSL_CTX_use_certificate_file';              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_SESSION_free;                       N: 'SSL_SESSION_free';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_SESSION_get_id;                     N: 'SSL_SESSION_get_id';                        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_SESSION_get_time;                   N: 'SSL_SESSION_get_time';                      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_SESSION_get_timeout;                N: 'SSL_SESSION_get_timeout';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_SESSION_set_time;                   N: 'SSL_SESSION_set_time';                      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_SESSION_set_timeout;                N: 'SSL_SESSION_set_timeout';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_accept;                             N: 'SSL_accept';                                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_add_client_CA;                      N: 'SSL_add_client_CA';                         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_alert_desc_string_long;             N: 'SSL_alert_desc_string_long';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_alert_type_string_long;             N: 'SSL_alert_type_string_long';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_callback_ctrl;                      N: 'SSL_callback_ctrl';                         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_clear;                              N: 'SSL_clear';                                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_connect;                            N: 'SSL_connect';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_ctrl;                               N: 'SSL_ctrl';                                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_do_handshake;                       N: 'SSL_do_handshake';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_free;                               N: 'SSL_free';                                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get0_param;                         N: 'SSL_get0_param';                            MI: OSSL_VER_1002; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@f_SSL_get1_session;                       N: 'SSL_get1_session';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get1_supported_ciphers;             N: 'SSL_get1_supported_ciphers';                MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_SSL_CTX;                        N: 'SSL_get_SSL_CTX';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_cipher_list;                    N: 'SSL_get_cipher_list';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_ciphers;                        N: 'SSL_get_ciphers';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_client_CA_list;                 N: 'SSL_get_client_CA_list';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_client_ciphers;                 N: 'SSL_get_client_ciphers';                    MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_current_cipher;                 N: 'SSL_get_current_cipher';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_error;                          N: 'SSL_get_error';                             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_ex_data;                        N: 'SSL_get_ex_data';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_ex_data_X509_STORE_CTX_idx;     N: 'SSL_get_ex_data_X509_STORE_CTX_idx';        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_fd;                             N: 'SSL_get_fd';                                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_peer_cert_chain;                N: 'SSL_get_peer_cert_chain';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_peer_certificate;               N: 'SSL_get_peer_certificate';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_rbio;                           N: 'SSL_get_rbio';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_rfd;                            N: 'SSL_get_rfd';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_servername;                     N: 'SSL_get_servername';                        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_servername_type;                N: 'SSL_get_servername_type';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_session;                        N: 'SSL_get_session';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_shutdown;                       N: 'SSL_get_shutdown';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_state;                          N: 'SSL_get_state';                             MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_verify_depth;                   N: 'SSL_get_verify_depth';                      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_verify_result;                  N: 'SSL_get_verify_result';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_version;                        N: 'SSL_get_version';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_wbio;                           N: 'SSL_get_wbio';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_get_wfd;                            N: 'SSL_get_wfd';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_library_init;                       N: 'SSL_library_init';                          MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_SSL_load_client_CA_file;                N: 'SSL_load_client_CA_file';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_load_error_strings;                 N: 'SSL_load_error_strings';                    MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_SSL_new;                                N: 'SSL_new';                                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_read;                               N: 'SSL_read';                                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_renegotiate;                        N: 'SSL_renegotiate';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_renegotiate_pending;                N: 'SSL_renegotiate_pending';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set1_param;                         N: 'SSL_set1_param';                            MI: OSSL_VER_1002; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@f_SSL_set_SSL_CTX;                        N: 'SSL_set_SSL_CTX';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_accept_state;                   N: 'SSL_set_accept_state';                      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_bio;                            N: 'SSL_set_bio';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_client_CA_list;                 N: 'SSL_set_client_CA_list';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_connect_state;                  N: 'SSL_set_connect_state';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_ex_data;                        N: 'SSL_set_ex_data';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_fd;                             N: 'SSL_set_fd';                                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_info_callback;                  N: 'SSL_set_info_callback';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_rfd;                            N: 'SSL_set_rfd';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_session;                        N: 'SSL_set_session';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_session_id_context;             N: 'SSL_set_session_id_context';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_shutdown;                       N: 'SSL_set_shutdown';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_verify;                         N: 'SSL_set_verify';                            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_verify_result;                  N: 'SSL_set_verify_result';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_set_wfd;                            N: 'SSL_set_wfd';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_shutdown;                           N: 'SSL_shutdown';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_state;                              N: 'SSL_state';                                 MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_SSL_state_string;                       N: 'SSL_state_string';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),      { V8.40 }
    (F: @@f_SSL_state_string_long;                  N: 'SSL_state_string_long';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_version;                            N: 'SSL_version';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_want;                               N: 'SSL_want';                                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSL_write;                              N: 'SSL_write';                                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_SSLv23_client_method;                   N: 'SSLv23_client_method';                      MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_SSLv23_method;                          N: 'SSLv23_method';                             MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_SSLv23_server_method;                   N: 'SSLv23_server_method';                      MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_SSLv3_client_method;                    N: 'SSLv3_client_method';                       MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_SSLv3_method;                           N: 'SSLv3_method';                              MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_SSLv3_server_method;                    N: 'SSLv3_server_method';                       MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_TLS_client_method;                      N: 'TLS_client_method';                         MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_TLS_method;                             N: 'TLS_method';                                MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_TLS_server_method;                      N: 'TLS_server_method';                         MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_TLSv1_1_client_method;                  N: 'TLSv1_1_client_method';                     MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_TLSv1_1_method;                         N: 'TLSv1_1_method';                            MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_TLSv1_1_server_method;                  N: 'TLSv1_1_server_method';                     MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_TLSv1_2_client_method;                  N: 'TLSv1_2_client_method';                     MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_TLSv1_2_method;                         N: 'TLSv1_2_method';                            MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_TLSv1_2_server_method;                  N: 'TLSv1_2_server_method';                     MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_TLSv1_client_method;                    N: 'TLSv1_client_method';                       MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_TLSv1_method;                           N: 'TLSv1_method';                              MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_TLSv1_server_method;                    N: 'TLSv1_server_method';                       MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_d2i_SSL_SESSION;                        N: 'd2i_SSL_SESSION';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_i2d_SSL_SESSION;                        N: 'i2d_SSL_SESSION';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX) );

{$IFNDEF OPENSSL_NO_ENGINE}
    GSSLEAYImports2: array[0..0] of TOSSLImports = (
    (F: @@f_SSL_CTX_set_client_cert_engine;         N: 'SSL_CTX_set_client_cert_engine';            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX) );
{$ENDIF}

{$ENDIF} // USE_SSL

implementation

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$IFDEF MSWINDOWS}

{ V8.38 Windows API to check authenticode code signing digital certificate on OpenSSL files }
procedure IcsVerifySslDll (const Fname: string);
var
    ErrCode: integer;
    TrustResp: String;
begin
    ErrCode := IcsVerifyTrust (FName, NOT GSSL_SignTest_Certificate, false, TrustResp);
    if (ErrCode = TRUST_E_SUBJECT_NOT_TRUSTED) or (ErrCode = TRUST_E_BAD_DIGEST) or
         (ErrCode = TRUST_E_NOSIGNATURE) or (ErrCode = TRUST_E_EXPLICIT_DISTRUST) then begin
         raise  EIcsSsleayException.Create('Failed to load ' + FName + ' - ' + TrustResp);
    end;
    if GSSL_SignTest_Certificate and ((ErrCode = CERT_E_CHAINING) or
        (ErrCode = CERT_E_UNTRUSTEDROOT) or (ErrCode = CERT_E_UNTRUSTEDTESTROOT)) then begin
         raise  EIcsSsleayException.Create('Failed to load ' + FName + ' - ' + TrustResp);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}

// import OpenSSL functions from DLLs, returns blank for OK, or list of missing exports

function SslGetImports (Handle: THandle; List: array of TOSSLImports): string ;  { V8.35 }
var
    I: integer ;
begin
    result := '';
    if (Length (List) = 0) then begin
        result := 'No import list specified' ;
    end;
    for I := 0 to Length(List) - 1 do begin
        if (ICS_OPENSSL_VERSION_NUMBER >= List[I].MI) and
               (ICS_OPENSSL_VERSION_NUMBER <= List[I].MX) then begin
            List[I].F^ := GetProcAddress (Handle, List[I].N);
            if List[I].F^ = nil then
                result := result + String(List[I].N) + ',' ;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SsleayLoad : Boolean;      {  V8.27 make unique }
var
    ErrCode: Integer;
    FullName, errs: String;   { V8.29 }
begin
    Result := TRUE;
    if GSSLEAY_DLL_Handle <> 0 then Exit; // Already loaded

  { V8.27 sanity check }
    if ICS_OPENSSL_VERSION_NUMBER = 0 then begin
       raise EIcsSsleayException.Create('Must load LIBEAY DLL before SSLEAY');
    end;

  { V8.27 see if opening new or old DLL }
  { V8.27 allow a specific DLL directory to be specified in GSSL_DLL_DIR }
    if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_1100 then begin
        FullName := GSSL_DLL_DIR+GSSLEAY_110DLL_Name;  { V8.29 }
        GSSLEAY_DLL_Handle := LoadLibrary(PChar(FullName));
        if GSSLEAY_DLL_Handle = 0 then begin
            ErrCode            := GetLastError;
            GSSLEAY_DLL_Handle := 0;
            if ErrCode = {$IFDEF POSIX} ENOENT {$ELSE} ERROR_MOD_NOT_FOUND {$ENDIF} then
                raise EIcsSsleayException.Create('File not found: ' + FullName)
            else
                raise EIcsSsleayException.Create('Unable to load ' + FullName + '. Win32 error #' + IntToStr(ErrCode));
        end;
    end
    else begin
        FullName := GSSL_DLL_DIR+GSSLEAY_DLL_Name;  { V8.29 }
        GSSLEAY_DLL_Handle := LoadLibrary(PChar(FullName));
        if GSSLEAY_DLL_Handle = 0 then begin
            ErrCode            := GetLastError;
            GSSLEAY_DLL_Handle := 0;
            if ErrCode = {$IFDEF POSIX} ENOENT {$ELSE} ERROR_MOD_NOT_FOUND {$ENDIF} then
                raise EIcsSsleayException.Create('File not found: ' + FullName)
            else
                raise EIcsSsleayException.Create('Unable to load ' + FullName  + '. Win32 error #' + IntToStr(ErrCode));
        end;
    end;
    SetLength(GSSLEAY_DLL_FileName, 256);
    SetLength(GSSLEAY_DLL_FileName, GetModuleFileName(GSSLEAY_DLL_Handle,
                 PChar(GSSLEAY_DLL_FileName), Length(GSSLEAY_DLL_FileName)));

  {$IFDEF MSWINDOWS}
    IcsGetFileVerInfo(GSSLEAY_DLL_FileName,      { V8.27 use full path }
                   GSSLEAY_DLL_FileVersion,
                   GSSLEAY_DLL_FileDescription);

   { V8.38 check authenticode digital signature on DLL }
    if GSSL_SignTest_Check then IcsVerifySslDll (GSSLEAY_DLL_FileName);
  {$ENDIF}

  { V8.35 load all main GSSLEAY_DLL exports }
    errs := SslGetImports (GSSLEAY_DLL_Handle, GSSLEAYImports1) ;
    if errs <> '' then
        raise  EIcsSsleayException.Create('Unable to load ' + FullName + '. Can not find: ' + errs);

{$IFNDEF OPENSSL_NO_ENGINE}
  { V8.35 load engine GSSLEAY_DLL exports }
    errs := SslGetImports (GSSLEAY_DLL_Handle, GSSLEAYImports2) ;
    if errs <> '' then
        raise  EIcsSsleayException.Create('Unable to load ' + FullName + '. Can not find: ' + errs);
{$ENDIF}

    { V8.27 older OpenSSL versions have different export }
    if ICS_OPENSSL_VERSION_NUMBER < OSSL_VER_1100 then begin
    // V8.27 fake new best method with old best method }
       f_TLS_method                := @f_SSLv23_method;
       f_TLS_client_method         := @f_SSLv23_client_method;
       f_TLS_server_method         := @f_SSLv23_server_method;
    end

    { V8.27 new OpenSSL versions have some new, fake old ones }
    else begin
    // V8.27 1.1.0 has lost v3 and v23, and tlsv1x are deprecated, so fake the lot }
       @f_SSLv3_method             := @f_TLS_method;
       @f_SSLv3_client_method      := @f_TLS_client_method;
       @f_SSLv3_server_method      := @f_TLS_server_method;
       @f_SSLv23_method            := @f_TLS_method;
       @f_SSLv23_client_method     := @f_TLS_client_method;
       @f_SSLv23_server_method     := @f_TLS_server_method;
       @f_TLSv1_method             := @f_TLS_method;
       @f_TLSv1_client_method      := @f_TLS_client_method;
       @f_TLSv1_server_method      := @f_TLS_server_method;
       @f_TLSv1_1_method           := @f_TLS_method;
       @f_TLSv1_1_client_method    := @f_TLS_client_method;
       @f_TLSv1_1_server_method    := @f_TLS_server_method;
       @f_TLSv1_2_method           := @f_TLS_method;
       @f_TLSv1_2_client_method    := @f_TLS_client_method;
       @f_TLSv1_2_server_method    := @f_TLS_server_method;
       @f_SSL_library_init         := @IcsSslStub;  { V8.35 }
       @f_SSL_load_error_strings   := @IcsSslStub;  { V8.35 }
       @f_SSL_state                := @IcsSslStub;  { V8.35 }
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_set_options(C: PSSL_CTX; Op: LongInt): LongInt;
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_OPTIONS, Op, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_get_options(C: PSSL_CTX): LongInt;
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_OPTIONS, 0, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_set_options(S: PSSL; Op: LongInt): LongInt;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_OPTIONS, Op, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_clear_options(S: PSSL; Op: LongInt): LongInt;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_CLEAR_OPTIONS, Op, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_get_options(S: PSSL): LongInt;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_OPTIONS, 0, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_want_read(S: PSSL) : Boolean;
begin
    Result := (f_SSL_want(S) = SSL_READING);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_want_write(S: PSSL) : Boolean;
begin
    Result := (f_SSL_want(S) = SSL_WRITING);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_want_nothing(S: PSSL) : Boolean;
begin
    Result := (f_SSL_want(S) = SSL_NOTHING);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_want_x509_lookup(S: PSSL) : Boolean;
begin
    Result := (f_SSL_want(S) = SSL_X509_LOOKUP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_set_session_cache_mode(Ctx: PSSL_CTX; Mode: Integer): Integer;
begin
    Result := f_SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_SESS_CACHE_MODE, Mode, nil)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_session_reused(SSL: PSSL): Integer;
begin
    Result := f_SSL_ctrl(SSL, SSL_CTRL_GET_SESSION_REUSED, 0, nil)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_sess_set_cache_size(Ctx: PSSL_CTX; CacheSize: Integer): Integer;
begin
    Result := f_SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_SESS_CACHE_SIZE, CacheSize, nil)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_add_extra_chain_cert(Ctx: PSSL_CTX; Cert: PX509): Longword;
begin
    Result := f_SSL_CTX_ctrl(Ctx, SSL_CTRL_EXTRA_CHAIN_CERT, 0, PAnsiChar(Cert))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_set_tlsext_host_name(const S: PSSL; const name: String): Longint;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_SET_TLSEXT_HOSTNAME,
                      TLSEXT_NAMETYPE_host_name, Pointer(StringToUtf8(name)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX;
  cb: TCallback_ctrl_fp): Longint;
begin
    Result := f_SSL_CTX_callback_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_CB, cb);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_set_tlsext_servername_arg(ctx: PSSL_CTX; arg: Pointer): Longint;
begin
    Result := f_SSL_CTX_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_set_tlsext_debug_callback(S: PSSL; cb: TCallback_ctrl_fp): Longint;
begin
    Result := f_SSL_callback_ctrl(S, SSL_CTRL_SET_TLSEXT_DEBUG_CB, cb);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_set_tlsext_debug_arg(S: PSSL; arg: Pointer): Longint;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_SET_TLSEXT_DEBUG_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_set_tmp_dh(C: PSSL_CTX; DH: Pointer) : Integer;   { V8.01 }
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_SET_TMP_DH, 0, DH);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_set_tmp_ecdh(C: PSSL_CTX; ECDH: Pointer) : Integer;    { V8.01 }
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_SET_TMP_ECDH, 0, ECDH);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_set_ecdh_auto(C: PSSL_CTX; onoff: Integer): Integer;    { V8.01 }
begin
    if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_1100 then
        Result := 1  { V8.27 always enabled }
    else
        Result := f_SSL_CTX_ctrl(C, SSL_CTRL_SET_ECDH_AUTO, onoff, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_set_tmp_dh(S: PSSL; DH: Pointer) : Integer;     { V8.01 }
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_SET_TMP_DH, 0, DH);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_set_tmp_ecdh(S: PSSL; ECDH: Pointer) : Integer;    { V8.01 }
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_SET_TMP_ECDH, 0, ECDH);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_set_ecdh_auto(S: PSSL; onoff: integer) : Integer;    { V8.01 }
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_SET_ECDH_AUTO, onoff, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_set_min_proto_version(C: PSSL_CTX; version: integer) : Integer;  { V8.27 }
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_SET_MIN_PROTO_VERSION, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_set_max_proto_version(C: PSSL_CTX;  version: integer) : Integer;  { V8.27 }
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_SET_MAX_PROTO_VERSION, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_set_min_proto_version(S: PSSL; version: integer) : Integer;     { V8.27 }
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_SET_MIN_PROTO_VERSION, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_set_max_proto_version(S: PSSL; version: integer) : Integer;     { V8.27 }
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_SET_MAX_PROTO_VERSION, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_set0_chain(C: PSSL_CTX; sk: PSTACK_OF_X509): Integer;       { V8.27 }
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_CHAIN, 0, PAnsiChar(sk));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_add0_chain_cert(C: PSSL_CTX; Cert: PX509): Integer;         { V8.27 }
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_CHAIN_CERT, 0, PAnsiChar(Cert));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_get0_chain_certs(C: PSSL_CTX; sk: PPSTACK_OF_X509): Integer; { V8.27 }
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_GET_CHAIN_CERTS, 0, PAnsiChar(Sk));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_clear_chain_certs(C: PSSL_CTX): Integer;                    { V8.27 }
begin
    Result := f_SSL_CTX_set0_chain (C, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  f_SSL_CTX_build_cert_chain(C: PSSL_CTX; flags: integer): Integer;     { V8.27 }
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_BUILD_CERT_CHAIN, flags, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ alternate for f_SSL_state which has gone in 1.1.0 and later }
function IcsSslGetState(S: PSSL): TSslHandshakeState;                           { V8.27 }
var
    oldstate: Integer;
begin
    if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_1100 then
        Result := f_SSL_get_state(S)
    else begin
        oldstate := f_SSL_state(S);
        if ((oldstate and SSL_ST_INIT) <> 0) then
            Result := TLS_ST_CR_SRVR_HELLO
         else if oldstate = SSL_ST_OK then
            Result := TLS_ST_OK
         else
            Result := TLS_ST_Before;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ stub for old removed functions }
function IcsSslStub: integer;                            { V8.35 }
begin
    result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure  f_SSL_CTX_set_msg_callback_arg(Ctx: PSSL_CTX; arg: Pointer);    { V8.40 }
begin
    f_SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_MSG_CALLBACK_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure  f_SSL_set_msg_callback_arg(S: PSSL; arg: Pointer);           { V8.40 }
begin
    f_SSL_ctrl(S, SSL_CTRL_SET_MSG_CALLBACK_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}//USE_SSL

end.








