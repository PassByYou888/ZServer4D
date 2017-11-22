{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Delphi encapsulation for LIBEAY32.DLL (OpenSSL)
              Renamed libcrypto32.dll for OpenSSL 1.1.0 and later
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
Dec 07, 2005 A. Garrels support of OSSL v0.9.8a added. New version check,
             see comments in source. In order to disable version check uncomment
             define NO_OSSL_VERSION_CHECK below and rebuild all. New functions
             OpenSslVersion, OpenSslCompilerFlags, OpenSslBuiltOn,
             OpenSslPlatForm, OpenSslDir all return a string type.
Jan 27, 2006 A. Garrels, made BDS2006 (BCB & Pascal) compilers happy.
Mar 03, 2006 A. Garrels: Added functions f_Ics_X509_get_notBefore,
             f_Ics_X509_get_notAfter, Asn1ToUTDateTime.
Mar 03, 2007 A. Garrels: Small changes to support OpenSSL 0.9.8e.
             Read comments in OverbyteIcsSslDefs.inc.
May 24, 2007 A.Garrels: Added code to handle ASN1 BMPString and Utf8 string
             types.
Jun 30, 2008 A.Garrels made some changes to prepare code for Unicode.
Jul 18, 2008 A. Garrels made some changes to get rid of some string cast
             warnings.
Jun 05, 2008 A.Garrels revised Asn1ToString(), made some string casts explicit.
Aug 19, 2008 A.Garrels checked against OpenSSL v0.9.8h and added that version
             as maximum version.
Nov 17, 2008 A.Garrels checked against OpenSSL v0.9.8i and added that version
             as maximum version.
Apr 10, 2009 A.Garrels checked against OpenSSL v0.9.8k and made it the maximum
             supported version.
Sep 24, 2009 Arno - Use OverbyteIcsUtils.IcsBufferToHex()
Nov 05, 2009 A.Garrels checked against OpenSSL v0.9.8L and made it the maximum
             supported version. OpenSSL V0.9.8L disables session renegotiation
             due to TLS renegotiation vulnerability.
Dec 20, 2009 A.Garrels added plenty of stuff. Some is not yet used some is, like
             Server Name Indication (SNI) and an option to let OpenSSL use the
             default Delphi memory manager (both needs to be turned on in
             OverbyteIcsSslDefs.inc).
May 07, 2010 A. Garrels moved declaration of size_t to OverbyteIcsTypes,
             changed user type CRYPTO_dynlock_value to use TRTLCriticalSection.
May 08, 2010 Arno Garrels added support for OpenSSL 0.9.8n.
             In OSSL v0.9.8L and v0.9.8m renegotiation support was disabled
             due to vulnerability of the SSL protocol. In v0.9.8n renegotiation
             support was re-enabled and RFC5746 implemented but require the
             extension as needed. It's also possible to enable unsafe legacy
             renegotiation explicitly by setting new option
             sslOpt_ALLOW_UNSAFE_LEGACY_RENEGOTIATION of TSslContext.
Apr 15, 2011 Arno prepared for 64-bit.
Apr 23, 2011 Arno added support for OpenSSL 0.9.8r and 1.0.0d.
Apr 24, 2011 Arno added some helper rountines since record TEVP_PKEY_st
             changed in 1.0.0 and had to be declared as dummy.
May 03, 2011 Arno added some function declarations.
May 08, 2011 Arno added function f_ERR_remove_thread_state new in v1.0.0+.
May 17, 2011 Arno made one hack thread-safe and got rid of another hack with
             OSSL v1.0.0+.
May 31, 2011 Arno changed the 64-bit hack in Ics_Ssl_EVP_PKEY_GetKey.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Feb 13, 2014 V8.01 - Angus added more NID_xx literals
Apr 19, 2014 V8.02 - Arno allow load of OSSL 1.0.1g (untested so far)
Jul 07, 2014 V8.03 - Angus allow load of OSSL 1.0.1h (briefly tested)
Aug 08, 2014 V8.04 - Angus allow load of OSSL 1.0.1i (briefly tested)
Aug 08, 2014 V8.04 - Angus allow load of OSSL 1.0.1i (briefly tested)
Dec 09, 2014 V8.05 - Angus allow load of OSSL 1.0.1j (untested)
                     Added Ics_Ssl_ERR_GET_LIB, Ics_Ssl_ERR_GET_FUNC and Ics_Ssl_ERR_GET_REASON
                     Added error literals for library, function and reason
Jan 19, 2015 V8.06 - Angus allow load of OSSL 1.0.1k (briefly tested)
Mar 13, 2015 V8.07 - Angus allow load of OSSL 1.0.2 (briefly tested)
                     Note, only OpenSSL 1.0.1 and later are now supported, removed some old conditionals and code
                     Added functions and literals for DH and EC key support
Mar 17, 2015 V8.08 - Angus allow load of OSSL 1.0.2a (untested)
Mar 26, 2015 V8.09   Angus, the OpenSSL version check is relaxed so minor versions with a letter suffix
                      are now supported up to the next major version, so now support up to 1.0.2z
Oct 23, 2015 V8.10   Angus, another NID literal
Nov 20, 2015 V8.11   Eugene Kotlyarov added RSA key related stuff
Nov 23, 2015 V8.12   Angus added f_PEM_write_bio_RSAPrivateKey and f_PEM_write_bio_RSAPublicKey
Feb 1, 2016  V8.13   Angus changed GLIBEAY_DLL_xx from const to var
May 24, 2016 V8.27   Angus match version to Wsocket where most of this API is used
                     Initial support for OpenSSL 1.1.0, new DLL file names, old exports gone
                      Load now LibeayLoad, WhichFailedToLoad now LibeayWhichFailedToLoad
                     Moved all public GLIBEAY_xx variables to top of OverbyteIcsSSLEAY
June 26, 2016 V8.29 Angus Implement GSSL_DLL_DIR properly to report full file path on error
Aug 5, 2016   V8.31 Angus testing OpenSSL 1.1.0 beta 6, more renamed exports
Aug 27, 2016  V8.32 Angus, suuport final release OpenSSL 1.1.0
                    OpenSSL 64-bit DLLs have different file names with -x64 added
                    Two new 1.1.0 exports renamed
Sept 5, 2016  V8.34 Angus, check GSSLEAY_DLL_IgnoreOld so only OpenSSL 1.1.0 and later are loaded
Oct 18, 2016  V8.35 Angus, major rewrite to simplify loading OpenSSL DLL functions
              Reversed V8.34 fix so this release only supports 1.1.0 not 1.1.1
              OPENSSL_ALLOW_SSLV2 gone with all SSLv2 functions
              stub more removed functions to save some exceptions
              combined all imports from OverbyteIcsLibeayEx to make maintenance and use easier
              EVP_CIPHER_CTX_xx is now backward compatible with 1.1.0
Oct 26, 2016  V8.36 more clean up of old stuff gone from 1.1.0
              Now using new names for imports renamed in 1.1.0
Nov 15, 2016  V8.38 Added public variable GSSL_SignTest_Check to check OpenSSL
                DLLs are digitally signed, and GSSL_SignTest_Certificate to
                check for a valid certificate, both default to false
Nov 22, 2016  V8.39 Added functions to check certificate params using X509_VERIFY_PARAM
Jan 27, 2017  V8.40 Added 200 more certificate, EC key, digest, encryption and signing functions
                   (but still over 3,000 missing, fortunately not needed often)
Feb 24, 2017  V8.41 Added more NIDs, few more imported functions



Old Cryptography
Several algorithms no longer provide adequate security, so will not be supported
by ICS, including SSLv2, DSA keys, RSA keys less than 768 bits, DSS, MD5 and SHA1
digests/hashes

Preferred algorithms moving forward are TLS/1.2 and 1.3 (not yet), SHA224 or longer,
RSA keys 2,048 or longer, ECDSA (EC) keys, ED25519 keys (not yet)

Notes - OpenSSL libeay32 changes between 1.0.2 and 1.1.0 - August 2016

file libeay32.dll > libcrypto-1_1.dll and libcrypto-1_1-x64.dll
function SSLeay >  OpenSSL_version_num
function SSLeay_version > OpenSSL_version
constants SSLEAY_xx to OPENSSL_xx (and values changes)
stack functions sk_xx to OPENSSL_sk_xx
lhash functions lh_xx to OPENSSL_lh_xx  (not used here)

New threading API used so locking functions removed:
(also see OverbyteIcsSslThrdLock.pas)
CRYPTO_lock
CRYPTO_add_lock
CRYPTO_num_locks
CRYPTO_set_locking_callback
CRYPTO_set_id_callback
CRYPTO_set_dynlock_create_callback
CRYPTO_set_dynlock_lock_callback
CRYPTO_set_dynlock_destroy_callback

Other functions gone:
i2d_ASN1_bytes
OpenSSL_add_all_ciphers
OpenSSL_add_all_digests
EVP_CIPHER_CTX_init
EVP_CIPHER_CTX_cleanup
ERR_free_strings
CRYPTO_cleanup_all_ex_data
X509_STORE_CTX_get_chain
X509_STORE_CTX_trusted_stack
EVP_cleanup
ENGINE_cleanup
RAND_cleanup

Macros which are now new exported functions:
X509_get0_notBefore
X509_get0_notAfter
X509_get_signature_nid
X509_REQ_get_subject_name

New functions:
EVP_CIPHER_CTX_reset

A lot of data types have been made opaque so the elements can no
longer be accessed directly, only by functions, many new.
These data types include: X509_OBJECT, X509_STORE_CTX, X509_STORE, X509_LOOKUP,
X509_LOOKUP_METHOD, DH, DH_METHOD, RSA, RSA_METHOD, DSA, DSA_METHOD, BIO,
BIO_METHOD, EVP_MD_CTX, EVP_MD, EVP_CIPHER_CTX, EVP_CIPHER, HMAC_CTX.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }

{$WARN SYMBOL_DEPRECATED OFF}
{$I Include\OverbyteIcsDefs.inc}
{$I Include\OverbyteIcsSslDefs.inc}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}

unit OverbyteIcsLIBEAY;

interface

{$IFDEF USE_SSL}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.PsApi{$ELSE}PsApi{$ENDIF},
{$ENDIF}
    OverbyteIcsTypes, // size_t
{$IFDEF POSIX}
    Posix.SysTypes,
    Ics.Posix.WinTypes,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    OverbyteIcsUtils,
    OverbyteIcsSSLEAY;

const
    IcsLIBEAYVersion   = 841;
    CopyRight : String = ' IcsLIBEAY (c) 2003-2017 F. Piette V8.41 ';

type
    EIcsLibeayException = class(Exception);

{ V8.27 thread locking no longer used in OpenSSL 1.1.0 and later }
    TStatLockLockCallback = procedure(Mode : Integer; N : Integer; const _File : PAnsiChar; Line : Integer); cdecl;
    TStatLockIDCallback   = function : Longword; cdecl;
    TCryptoThreadIDCallback = procedure (ID : PCRYPTO_THREADID); cdecl;

    TCRYPTO_dynlock_value_st = record
        Mutex : TIcsCriticalSection;
    end;
    PCRYPTO_dynlock_value = ^TCRYPTO_dynlock_value_st;
    CRYPTO_dynlock_value  = TCRYPTO_dynlock_value_st;

    TDynLockCreateCallback  = function(const _file : PAnsiChar; Line: Integer): PCRYPTO_dynlock_value; cdecl;
    TDynLockLockCallback    = procedure(Mode : Integer; L : PCRYPTO_dynlock_value; _File : PAnsiChar; Line: Integer); cdecl;
    TDynLockDestroyCallback = procedure(L : PCRYPTO_dynlock_value; _File : PAnsiChar; Line: Integer); cdecl;

const
    V_ASN1_UNIVERSAL                    = $00;
    V_ASN1_APPLICATION                  = $40;
    V_ASN1_CONTEXT_SPECIFIC             = $80;
    V_ASN1_PRIVATE                      = $c0;

    V_ASN1_CONSTRUCTED                  = $20;
    V_ASN1_PRIMITIVE_TAG                = $1f;

    V_ASN1_UNDEF                        = -1;
    V_ASN1_EOC                          = 0;
    V_ASN1_BOOLEAN                      = 1;
    V_ASN1_INTEGER                      = 2;
    V_ASN1_BIT_STRING                   = 3;
    V_ASN1_OCTET_STRING                 = 4;
    V_ASN1_NULL                         = 5;
    V_ASN1_OBJECT                       = 6;
    V_ASN1_OBJECT_DESCRIPTOR            = 7;
    V_ASN1_EXTERNAL                     = 8;
    V_ASN1_REAL                         = 9;
    V_ASN1_ENUMERATED                   = 10;
    V_ASN1_UTF8STRING                   = 12;
    V_ASN1_SEQUENCE                     = 16;
    V_ASN1_SET                          = 17;
    V_ASN1_NUMERICSTRING                = 18;
{ An ASN.1 NumericString object may represent any arbitrary string of numeric }
{ characters including the space character: 0,1,2,...,9,SPACE                 }
    V_ASN1_PRINTABLESTRING              = 19;
{ An ASN.1 PrintableString may represent any arbitrary string of  printable  }
{ characters (A,B,...,Z; a,b,...,z; 0,1,...,9; space ' () + , - . / : = ?)   }
    V_ASN1_T61STRING                    = 20;
    V_ASN1_TELETEXSTRING                = 20;  (* alias *)
    V_ASN1_VIDEOTEXSTRING               = 21;
    V_ASN1_IA5STRING                    = 22;
{ An ASN.1 IA5String object may represent any arbitrary string of ASCII   }
{ characters. The term IA5 denotes International Alphabet 5 (= ASCII).    }
    V_ASN1_UTCTIME                      = 23;
    V_ASN1_GENERALIZEDTIME              = 24;
    V_ASN1_GRAPHICSTRING                = 25;
    V_ASN1_ISO64STRING                  = 26;
    V_ASN1_VISIBLESTRING                = 26;  (* alias *)
    V_ASN1_GENERALSTRING                = 27;
{ The ASN.1 character string type GeneralString encompasses all registered }
{ graphic and character sets (see ISO 2375) plus SPACE and DELETE.         }
    V_ASN1_UNIVERSALSTRING              = 28;
{ UniversalString is defined in ASN.1:1993.                                }
    V_ASN1_BMPSTRING                    = 30;
{ BMPString is a subtype of the UniversalString type and models the Basic  }
{ Multilingual Plane of ISO/IEC/ITU 10646-1, a two-octet (USC-2) encoding  }
{ form, which is identical to Unicode 1.1.                                 }

    {ERR_NUM_ERRORS   = 10;
    ERR_TXT_MALLOCED = 1; }
    ERR_TXT_STRING   = 2;
    { Changed from 32 in v0.9.7 up }
    ERR_R_FATAL      = 64;

  { V8.05 Library codes for SSLErr() }
    ERR_LIB_NONE     = 1;
    ERR_LIB_SYS      = 2;
    ERR_LIB_BN       = 3;
    ERR_LIB_RSA      = 4;
    ERR_LIB_DH       = 5;
    ERR_LIB_EVP      = 6;
    ERR_LIB_BUF      = 7;
    ERR_LIB_OBJ      = 8;
    ERR_LIB_PEM      = 9;
    ERR_LIB_DSA      = 10;
    ERR_LIB_X509     = 11;
    ERR_LIB_METH     = 12;
    ERR_LIB_ASN1     = 13;
    ERR_LIB_CONF     = 14;
    ERR_LIB_CRYPTO   = 15;
    ERR_LIB_SSL      = 20;
    ERR_LIB_SSL23    = 21;
    ERR_LIB_SSL2     = 22;
    ERR_LIB_SSL3     = 23;
    ERR_LIB_RSAREF   = 30;
    ERR_LIB_PROXY    = 31;
    ERR_LIB_BIO      = 32;
    ERR_LIB_PKCS7    = 33;
    ERR_LIB_X509V3   = 34;
    ERR_LIB_PKCS12   = 35;
    ERR_LIB_RAND     = 36;
    ERR_LIB_DSO      = 37;
    ERR_LIB_ENGINE   = 38;
    ERR_LIB_OCSP     = 39;
    ERR_LIB_UI       = 40;
    ERR_LIB_COMP     = 41;
    ERR_LIB_ECDSA    = 42;
    ERR_LIB_ECDH     = 43;
    ERR_LIB_STORE    = 44;
    ERR_LIB_FIPS     = 45;
    ERR_LIB_CMS      = 46;
    ERR_LIB_TS       = 47;
    ERR_LIB_HMAC     = 48;
    ERR_LIB_JPAKE    = 49;
    ERR_LIB_USER     = 128;

  { V8.05 function codes for SSLErr() }
    SSL_F_CLIENT_CERTIFICATE = 100;
    SSL_F_CLIENT_FINISHED = 167;
    SSL_F_CLIENT_HELLO = 101;
    SSL_F_CLIENT_MASTER_KEY = 102;
    SSL_F_D2I_SSL_SESSION = 103;
    SSL_F_DO_DTLS1_WRITE = 245;
    SSL_F_DO_SSL3_WRITE = 104;
    SSL_F_DTLS1_ACCEPT = 246;
    SSL_F_DTLS1_ADD_CERT_TO_BUF = 295;
    SSL_F_DTLS1_BUFFER_RECORD = 247;
    SSL_F_DTLS1_CHECK_TIMEOUT_NUM = 316;
    SSL_F_DTLS1_CLIENT_HELLO = 248;
    SSL_F_DTLS1_CONNECT = 249;
    SSL_F_DTLS1_ENC = 250;
    SSL_F_DTLS1_GET_HELLO_VERIFY = 251;
    SSL_F_DTLS1_GET_MESSAGE = 252;
    SSL_F_DTLS1_GET_MESSAGE_FRAGMENT = 253;
    SSL_F_DTLS1_GET_RECORD = 254;
    SSL_F_DTLS1_HANDLE_TIMEOUT = 297;
    SSL_F_DTLS1_HEARTBEAT = 305;
    SSL_F_DTLS1_OUTPUT_CERT_CHAIN = 255;
    SSL_F_DTLS1_PREPROCESS_FRAGMENT = 288;
    SSL_F_DTLS1_PROCESS_OUT_OF_SEQ_MESSAGE = 256;
    SSL_F_DTLS1_PROCESS_RECORD = 257;
    SSL_F_DTLS1_READ_BYTES = 258;
    SSL_F_DTLS1_READ_FAILED = 259;
    SSL_F_DTLS1_SEND_CERTIFICATE_REQUEST = 260;
    SSL_F_DTLS1_SEND_CLIENT_CERTIFICATE = 261;
    SSL_F_DTLS1_SEND_CLIENT_KEY_EXCHANGE = 262;
    SSL_F_DTLS1_SEND_CLIENT_VERIFY = 263;
    SSL_F_DTLS1_SEND_HELLO_VERIFY_REQUEST = 264;
    SSL_F_DTLS1_SEND_SERVER_CERTIFICATE = 265;
    SSL_F_DTLS1_SEND_SERVER_HELLO = 266;
    SSL_F_DTLS1_SEND_SERVER_KEY_EXCHANGE = 267;
    SSL_F_DTLS1_WRITE_APP_DATA_BYTES = 268;
    SSL_F_GET_CLIENT_FINISHED = 105;
    SSL_F_GET_CLIENT_HELLO = 106;
    SSL_F_GET_CLIENT_MASTER_KEY = 107;
    SSL_F_GET_SERVER_FINISHED = 108;
    SSL_F_GET_SERVER_HELLO = 109;
    SSL_F_GET_SERVER_VERIFY = 110;
    SSL_F_I2D_SSL_SESSION = 111;
    SSL_F_READ_N = 112;
    SSL_F_REQUEST_CERTIFICATE = 113;
    SSL_F_SERVER_FINISH = 239;
    SSL_F_SERVER_HELLO = 114;
    SSL_F_SERVER_VERIFY = 240;
    SSL_F_SSL23_ACCEPT = 115;
    SSL_F_SSL23_CLIENT_HELLO = 116;
    SSL_F_SSL23_CONNECT = 117;
    SSL_F_SSL23_GET_CLIENT_HELLO = 118;
    SSL_F_SSL23_GET_SERVER_HELLO = 119;
    SSL_F_SSL23_PEEK = 237;
    SSL_F_SSL23_READ = 120;
    SSL_F_SSL23_WRITE = 121;
    SSL_F_SSL2_ACCEPT = 122;
    SSL_F_SSL2_CONNECT = 123;
    SSL_F_SSL2_ENC_INIT = 124;
    SSL_F_SSL2_GENERATE_KEY_MATERIAL = 241;
    SSL_F_SSL2_PEEK = 234;
    SSL_F_SSL2_READ = 125;
    SSL_F_SSL2_READ_INTERNAL = 236;
    SSL_F_SSL2_SET_CERTIFICATE = 126;
    SSL_F_SSL2_WRITE = 127;
    SSL_F_SSL3_ACCEPT = 128;
    SSL_F_SSL3_ADD_CERT_TO_BUF = 296;
    SSL_F_SSL3_CALLBACK_CTRL = 233;
    SSL_F_SSL3_CHANGE_CIPHER_STATE = 129;
    SSL_F_SSL3_CHECK_CERT_AND_ALGORITHM = 130;
    SSL_F_SSL3_CHECK_CLIENT_HELLO = 304;
    SSL_F_SSL3_CLIENT_HELLO = 131;
    SSL_F_SSL3_CONNECT = 132;
    SSL_F_SSL3_CTRL = 213;
    SSL_F_SSL3_CTX_CTRL = 133;
    SSL_F_SSL3_DIGEST_CACHED_RECORDS = 293;
    SSL_F_SSL3_DO_CHANGE_CIPHER_SPEC = 292;
    SSL_F_SSL3_ENC = 134;
    SSL_F_SSL3_GENERATE_KEY_BLOCK = 238;
    SSL_F_SSL3_GET_CERTIFICATE_REQUEST = 135;
    SSL_F_SSL3_GET_CERT_STATUS = 289;
    SSL_F_SSL3_GET_CERT_VERIFY = 136;
    SSL_F_SSL3_GET_CLIENT_CERTIFICATE = 137;
    SSL_F_SSL3_GET_CLIENT_HELLO = 138;
    SSL_F_SSL3_GET_CLIENT_KEY_EXCHANGE = 139;
    SSL_F_SSL3_GET_FINISHED = 140;
    SSL_F_SSL3_GET_KEY_EXCHANGE = 141;
    SSL_F_SSL3_GET_MESSAGE = 142;
    SSL_F_SSL3_GET_NEW_SESSION_TICKET = 283;
    SSL_F_SSL3_GET_NEXT_PROTO = 306;
    SSL_F_SSL3_GET_RECORD = 143;
    SSL_F_SSL3_GET_SERVER_CERTIFICATE = 144;
    SSL_F_SSL3_GET_SERVER_DONE = 145;
    SSL_F_SSL3_GET_SERVER_HELLO = 146;
    SSL_F_SSL3_HANDSHAKE_MAC = 285;
    SSL_F_SSL3_NEW_SESSION_TICKET = 287;
    SSL_F_SSL3_OUTPUT_CERT_CHAIN = 147;
    SSL_F_SSL3_PEEK = 235;
    SSL_F_SSL3_READ_BYTES = 148;
    SSL_F_SSL3_READ_N = 149;
    SSL_F_SSL3_SEND_CERTIFICATE_REQUEST = 150;
    SSL_F_SSL3_SEND_CLIENT_CERTIFICATE = 151;
    SSL_F_SSL3_SEND_CLIENT_KEY_EXCHANGE = 152;
    SSL_F_SSL3_SEND_CLIENT_VERIFY = 153;
    SSL_F_SSL3_SEND_SERVER_CERTIFICATE = 154;
    SSL_F_SSL3_SEND_SERVER_HELLO = 242;
    SSL_F_SSL3_SEND_SERVER_KEY_EXCHANGE = 155;
    SSL_F_SSL3_SETUP_KEY_BLOCK = 157;
    SSL_F_SSL3_SETUP_READ_BUFFER = 156;
    SSL_F_SSL3_SETUP_WRITE_BUFFER = 291;
    SSL_F_SSL3_WRITE_BYTES = 158;
    SSL_F_SSL3_WRITE_PENDING = 159;
    SSL_F_SSL_ADD_CLIENTHELLO_RENEGOTIATE_EXT = 298;
    SSL_F_SSL_ADD_CLIENTHELLO_TLSEXT = 277;
    SSL_F_SSL_ADD_CLIENTHELLO_USE_SRTP_EXT = 307;
    SSL_F_SSL_ADD_DIR_CERT_SUBJECTS_TO_STACK = 215;
    SSL_F_SSL_ADD_FILE_CERT_SUBJECTS_TO_STACK = 216;
    SSL_F_SSL_ADD_SERVERHELLO_RENEGOTIATE_EXT = 299;
    SSL_F_SSL_ADD_SERVERHELLO_TLSEXT = 278;
    SSL_F_SSL_ADD_SERVERHELLO_USE_SRTP_EXT = 308;
    SSL_F_SSL_BAD_METHOD = 160;
    SSL_F_SSL_BYTES_TO_CIPHER_LIST = 161;
    SSL_F_SSL_CERT_DUP = 221;
    SSL_F_SSL_CERT_INST = 222;
    SSL_F_SSL_CERT_INSTANTIATE = 214;
    SSL_F_SSL_CERT_NEW = 162;
    SSL_F_SSL_CHECK_PRIVATE_KEY = 163;
    SSL_F_SSL_CHECK_SERVERHELLO_TLSEXT = 280;
    SSL_F_SSL_CHECK_SRVR_ECC_CERT_AND_ALG = 279;
    SSL_F_SSL_CIPHER_PROCESS_RULESTR = 230;
    SSL_F_SSL_CIPHER_STRENGTH_SORT = 231;
    SSL_F_SSL_CLEAR = 164;
    SSL_F_SSL_COMP_ADD_COMPRESSION_METHOD = 165;
    SSL_F_SSL_CREATE_CIPHER_LIST = 166;
    SSL_F_SSL_CTRL = 232;
    SSL_F_SSL_CTX_CHECK_PRIVATE_KEY = 168;
    SSL_F_SSL_CTX_MAKE_PROFILES = 309;
    SSL_F_SSL_CTX_NEW = 169;
    SSL_F_SSL_CTX_SET_CIPHER_LIST = 269;
    SSL_F_SSL_CTX_SET_CLIENT_CERT_ENGINE = 290;
    SSL_F_SSL_CTX_SET_PURPOSE = 226;
    SSL_F_SSL_CTX_SET_SESSION_ID_CONTEXT = 219;
    SSL_F_SSL_CTX_SET_SSL_VERSION = 170;
    SSL_F_SSL_CTX_SET_TRUST = 229;
    SSL_F_SSL_CTX_USE_CERTIFICATE = 171;
    SSL_F_SSL_CTX_USE_CERTIFICATE_ASN1 = 172;
    SSL_F_SSL_CTX_USE_CERTIFICATE_CHAIN_FILE = 220;
    SSL_F_SSL_CTX_USE_CERTIFICATE_FILE = 173;
    SSL_F_SSL_CTX_USE_PRIVATEKEY = 174;
    SSL_F_SSL_CTX_USE_PRIVATEKEY_ASN1 = 175;
    SSL_F_SSL_CTX_USE_PRIVATEKEY_FILE = 176;
    SSL_F_SSL_CTX_USE_PSK_IDENTITY_HINT = 272;
    SSL_F_SSL_CTX_USE_RSAPRIVATEKEY = 177;
    SSL_F_SSL_CTX_USE_RSAPRIVATEKEY_ASN1 = 178;
    SSL_F_SSL_CTX_USE_RSAPRIVATEKEY_FILE = 179;
    SSL_F_SSL_DO_HANDSHAKE = 180;
    SSL_F_SSL_GET_NEW_SESSION = 181;
    SSL_F_SSL_GET_PREV_SESSION = 217;
    SSL_F_SSL_GET_SERVER_SEND_CERT = 182;
    SSL_F_SSL_GET_SERVER_SEND_PKEY = 317;
    SSL_F_SSL_GET_SIGN_PKEY = 183;
    SSL_F_SSL_INIT_WBIO_BUFFER = 184;
    SSL_F_SSL_LOAD_CLIENT_CA_FILE = 185;
    SSL_F_SSL_NEW = 186;
    SSL_F_SSL_PARSE_CLIENTHELLO_RENEGOTIATE_EXT = 300;
    SSL_F_SSL_PARSE_CLIENTHELLO_TLSEXT = 302;
    SSL_F_SSL_PARSE_CLIENTHELLO_USE_SRTP_EXT = 310;
    SSL_F_SSL_PARSE_SERVERHELLO_RENEGOTIATE_EXT = 301;
    SSL_F_SSL_PARSE_SERVERHELLO_TLSEXT = 303;
    SSL_F_SSL_PARSE_SERVERHELLO_USE_SRTP_EXT = 311;
    SSL_F_SSL_PEEK = 270;
    SSL_F_SSL_PREPARE_CLIENTHELLO_TLSEXT = 281;
    SSL_F_SSL_PREPARE_SERVERHELLO_TLSEXT = 282;
    SSL_F_SSL_READ = 223;
    SSL_F_SSL_RSA_PRIVATE_DECRYPT = 187;
    SSL_F_SSL_RSA_PUBLIC_ENCRYPT = 188;
    SSL_F_SSL_SESSION_NEW = 189;
    SSL_F_SSL_SESSION_PRINT_FP = 190;
    SSL_F_SSL_SESSION_SET1_ID_CONTEXT = 312;
    SSL_F_SSL_SESS_CERT_NEW = 225;
    SSL_F_SSL_SET_CERT = 191;
    SSL_F_SSL_SET_CIPHER_LIST = 271;
    SSL_F_SSL_SET_FD = 192;
    SSL_F_SSL_SET_PKEY = 193;
    SSL_F_SSL_SET_PURPOSE = 227;
    SSL_F_SSL_SET_RFD = 194;
    SSL_F_SSL_SET_SESSION = 195;
    SSL_F_SSL_SET_SESSION_ID_CONTEXT = 218;
    SSL_F_SSL_SET_SESSION_TICKET_EXT = 294;
    SSL_F_SSL_SET_TRUST = 228;
    SSL_F_SSL_SET_WFD = 196;
    SSL_F_SSL_SHUTDOWN = 224;
    SSL_F_SSL_SRP_CTX_INIT = 313;
    SSL_F_SSL_UNDEFINED_CONST_FUNCTION = 243;
    SSL_F_SSL_UNDEFINED_FUNCTION = 197;
    SSL_F_SSL_UNDEFINED_VOID_FUNCTION = 244;
    SSL_F_SSL_USE_CERTIFICATE = 198;
    SSL_F_SSL_USE_CERTIFICATE_ASN1 = 199;
    SSL_F_SSL_USE_CERTIFICATE_FILE = 200;
    SSL_F_SSL_USE_PRIVATEKEY = 201;
    SSL_F_SSL_USE_PRIVATEKEY_ASN1 = 202;
    SSL_F_SSL_USE_PRIVATEKEY_FILE = 203;
    SSL_F_SSL_USE_PSK_IDENTITY_HINT = 273;
    SSL_F_SSL_USE_RSAPRIVATEKEY = 204;
    SSL_F_SSL_USE_RSAPRIVATEKEY_ASN1 = 205;
    SSL_F_SSL_USE_RSAPRIVATEKEY_FILE = 206;
    SSL_F_SSL_VERIFY_CERT_CHAIN = 207;
    SSL_F_SSL_WRITE = 208;
    SSL_F_TLS1_CERT_VERIFY_MAC = 286;
    SSL_F_TLS1_CHANGE_CIPHER_STATE = 209;
    SSL_F_TLS1_CHECK_SERVERHELLO_TLSEXT = 274;
    SSL_F_TLS1_ENC = 210;
    SSL_F_TLS1_EXPORT_KEYING_MATERIAL = 314;
    SSL_F_TLS1_HEARTBEAT = 315;
    SSL_F_TLS1_PREPARE_CLIENTHELLO_TLSEXT = 275;
    SSL_F_TLS1_PREPARE_SERVERHELLO_TLSEXT = 276;
    SSL_F_TLS1_PRF = 284;
    SSL_F_TLS1_SETUP_KEY_BLOCK = 211;
    SSL_F_WRITE_PENDING = 212;

  { V8.05 reason codes for SSLErr() }
    SSL_R_APP_DATA_IN_HANDSHAKE = 100;
    SSL_R_ATTEMPT_TO_REUSE_SESSION_IN_DIFFERENT_CONTEXT = 272;
    SSL_R_BAD_ALERT_RECORD = 101;
    SSL_R_BAD_AUTHENTICATION_TYPE = 102;
    SSL_R_BAD_CHANGE_CIPHER_SPEC = 103;
    SSL_R_BAD_CHECKSUM = 104;
    SSL_R_BAD_DATA_RETURNED_BY_CALLBACK = 106;
    SSL_R_BAD_DECOMPRESSION = 107;
    SSL_R_BAD_DH_G_LENGTH = 108;
    SSL_R_BAD_DH_PUB_KEY_LENGTH = 109;
    SSL_R_BAD_DH_P_LENGTH = 110;
    SSL_R_BAD_DIGEST_LENGTH = 111;
    SSL_R_BAD_DSA_SIGNATURE = 112;
    SSL_R_BAD_ECC_CERT = 304;
    SSL_R_BAD_ECDSA_SIGNATURE = 305;
    SSL_R_BAD_ECPOINT = 306;
    SSL_R_BAD_HANDSHAKE_LENGTH = 332;
    SSL_R_BAD_HELLO_REQUEST = 105;
    SSL_R_BAD_LENGTH = 271;
    SSL_R_BAD_MAC_DECODE = 113;
    SSL_R_BAD_MAC_LENGTH = 333;
    SSL_R_BAD_MESSAGE_TYPE = 114;
    SSL_R_BAD_PACKET_LENGTH = 115;
    SSL_R_BAD_PROTOCOL_VERSION_NUMBER = 116;
    SSL_R_BAD_PSK_IDENTITY_HINT_LENGTH = 316;
    SSL_R_BAD_RESPONSE_ARGUMENT = 117;
    SSL_R_BAD_RSA_DECRYPT = 118;
    SSL_R_BAD_RSA_ENCRYPT = 119;
    SSL_R_BAD_RSA_E_LENGTH = 120;
    SSL_R_BAD_RSA_MODULUS_LENGTH = 121;
    SSL_R_BAD_RSA_SIGNATURE = 122;
    SSL_R_BAD_SIGNATURE = 123;
    SSL_R_BAD_SRP_A_LENGTH = 347;
    SSL_R_BAD_SRP_B_LENGTH = 348;
    SSL_R_BAD_SRP_G_LENGTH = 349;
    SSL_R_BAD_SRP_N_LENGTH = 350;
    SSL_R_BAD_SRP_PARAMETERS = 371;
    SSL_R_BAD_SRP_S_LENGTH = 351;
    SSL_R_BAD_SRTP_MKI_VALUE = 352;
    SSL_R_BAD_SRTP_PROTECTION_PROFILE_LIST = 353;
    SSL_R_BAD_SSL_FILETYPE = 124;
    SSL_R_BAD_SSL_SESSION_ID_LENGTH = 125;
    SSL_R_BAD_STATE = 126;
    SSL_R_BAD_WRITE_RETRY = 127;
    SSL_R_BIO_NOT_SET = 128;
    SSL_R_BLOCK_CIPHER_PAD_IS_WRONG = 129;
    SSL_R_BN_LIB = 130;
    SSL_R_CA_DN_LENGTH_MISMATCH = 131;
    SSL_R_CA_DN_TOO_LONG = 132;
    SSL_R_CCS_RECEIVED_EARLY = 133;
    SSL_R_CERTIFICATE_VERIFY_FAILED = 134;
    SSL_R_CERT_LENGTH_MISMATCH = 135;
    SSL_R_CHALLENGE_IS_DIFFERENT = 136;
    SSL_R_CIPHER_CODE_WRONG_LENGTH = 137;
    SSL_R_CIPHER_OR_HASH_UNAVAILABLE = 138;
    SSL_R_CIPHER_TABLE_SRC_ERROR = 139;
    SSL_R_CLIENTHELLO_TLSEXT = 226;
    SSL_R_COMPRESSED_LENGTH_TOO_LONG = 140;
    SSL_R_COMPRESSION_DISABLED = 343;
    SSL_R_COMPRESSION_FAILURE = 141;
    SSL_R_COMPRESSION_ID_NOT_WITHIN_PRIVATE_RANGE = 307;
    SSL_R_COMPRESSION_LIBRARY_ERROR = 142;
    SSL_R_CONNECTION_ID_IS_DIFFERENT = 143;
    SSL_R_CONNECTION_TYPE_NOT_SET = 144;
    SSL_R_COOKIE_MISMATCH = 308;
    SSL_R_DATA_BETWEEN_CCS_AND_FINISHED = 145;
    SSL_R_DATA_LENGTH_TOO_LONG = 146;
    SSL_R_DECRYPTION_FAILED = 147;
    SSL_R_DECRYPTION_FAILED_OR_BAD_RECORD_MAC = 281;
    SSL_R_DH_PUBLIC_VALUE_LENGTH_IS_WRONG = 148;
    SSL_R_DIGEST_CHECK_FAILED = 149;
    SSL_R_DTLS_MESSAGE_TOO_BIG = 334;
    SSL_R_DUPLICATE_COMPRESSION_ID = 309;
    SSL_R_ECC_CERT_NOT_FOR_KEY_AGREEMENT = 317;
    SSL_R_ECC_CERT_NOT_FOR_SIGNING = 318;
    SSL_R_ECC_CERT_SHOULD_HAVE_RSA_SIGNATURE = 322;
    SSL_R_ECC_CERT_SHOULD_HAVE_SHA1_SIGNATURE = 323;
    SSL_R_ECGROUP_TOO_LARGE_FOR_CIPHER = 310;
    SSL_R_EMPTY_SRTP_PROTECTION_PROFILE_LIST = 354;
    SSL_R_ENCRYPTED_LENGTH_TOO_LONG = 150;
    SSL_R_ERROR_GENERATING_TMP_RSA_KEY = 282;
    SSL_R_ERROR_IN_RECEIVED_CIPHER_LIST = 151;
    SSL_R_EXCESSIVE_MESSAGE_SIZE = 152;
    SSL_R_EXTRA_DATA_IN_MESSAGE = 153;
    SSL_R_GOT_A_FIN_BEFORE_A_CCS = 154;
    SSL_R_GOT_NEXT_PROTO_BEFORE_A_CCS = 355;
    SSL_R_GOT_NEXT_PROTO_WITHOUT_EXTENSION = 356;
    SSL_R_HTTPS_PROXY_REQUEST = 155;
    SSL_R_HTTP_REQUEST = 156;
    SSL_R_ILLEGAL_PADDING = 283;
    SSL_R_INAPPROPRIATE_FALLBACK = 373;
    SSL_R_INCONSISTENT_COMPRESSION = 340;
    SSL_R_INVALID_CHALLENGE_LENGTH = 158;
    SSL_R_INVALID_COMMAND = 280;
    SSL_R_INVALID_COMPRESSION_ALGORITHM = 341;
    SSL_R_INVALID_PURPOSE = 278;
    SSL_R_INVALID_SRP_USERNAME = 357;
    SSL_R_INVALID_STATUS_RESPONSE = 328;
    SSL_R_INVALID_TICKET_KEYS_LENGTH = 325;
    SSL_R_INVALID_TRUST = 279;
    SSL_R_KEY_ARG_TOO_LONG = 284;
    SSL_R_KRB5 = 285;
    SSL_R_KRB5_C_CC_PRINC = 286;
    SSL_R_KRB5_C_GET_CRED = 287;
    SSL_R_KRB5_C_INIT = 288;
    SSL_R_KRB5_C_MK_REQ = 289;
    SSL_R_KRB5_S_BAD_TICKET = 290;
    SSL_R_KRB5_S_INIT = 291;
    SSL_R_KRB5_S_RD_REQ = 292;
    SSL_R_KRB5_S_TKT_EXPIRED = 293;
    SSL_R_KRB5_S_TKT_NYV = 294;
    SSL_R_KRB5_S_TKT_SKEW = 295;
    SSL_R_LENGTH_MISMATCH = 159;
    SSL_R_LENGTH_TOO_SHORT = 160;
    SSL_R_LIBRARY_BUG = 274;
    SSL_R_LIBRARY_HAS_NO_CIPHERS = 161;
    SSL_R_MESSAGE_TOO_LONG = 296;
    SSL_R_MISSING_DH_DSA_CERT = 162;
    SSL_R_MISSING_DH_KEY = 163;
    SSL_R_MISSING_DH_RSA_CERT = 164;
    SSL_R_MISSING_DSA_SIGNING_CERT = 165;
    SSL_R_MISSING_EXPORT_TMP_DH_KEY = 166;
    SSL_R_MISSING_EXPORT_TMP_RSA_KEY = 167;
    SSL_R_MISSING_RSA_CERTIFICATE = 168;
    SSL_R_MISSING_RSA_ENCRYPTING_CERT = 169;
    SSL_R_MISSING_RSA_SIGNING_CERT = 170;
    SSL_R_MISSING_SRP_PARAM = 358;
    SSL_R_MISSING_TMP_DH_KEY = 171;
    SSL_R_MISSING_TMP_ECDH_KEY = 311;
    SSL_R_MISSING_TMP_RSA_KEY = 172;
    SSL_R_MISSING_TMP_RSA_PKEY = 173;
    SSL_R_MISSING_VERIFY_MESSAGE = 174;
    SSL_R_MULTIPLE_SGC_RESTARTS = 346;
    SSL_R_NON_SSLV2_INITIAL_PACKET = 175;
    SSL_R_NO_CERTIFICATES_RETURNED = 176;
    SSL_R_NO_CERTIFICATE_ASSIGNED = 177;
    SSL_R_NO_CERTIFICATE_RETURNED = 178;
    SSL_R_NO_CERTIFICATE_SET = 179;
    SSL_R_NO_CERTIFICATE_SPECIFIED = 180;
    SSL_R_NO_CIPHERS_AVAILABLE = 181;
    SSL_R_NO_CIPHERS_PASSED = 182;
    SSL_R_NO_CIPHERS_SPECIFIED = 183;
    SSL_R_NO_CIPHER_LIST = 184;
    SSL_R_NO_CIPHER_MATCH = 185;
    SSL_R_NO_CLIENT_CERT_METHOD = 331;
    SSL_R_NO_CLIENT_CERT_RECEIVED = 186;
    SSL_R_NO_COMPRESSION_SPECIFIED = 187;
    SSL_R_NO_GOST_CERTIFICATE_SENT_BY_PEER = 330;
    SSL_R_NO_METHOD_SPECIFIED = 188;
    SSL_R_NO_PRIVATEKEY = 189;
    SSL_R_NO_PRIVATE_KEY_ASSIGNED = 190;
    SSL_R_NO_PROTOCOLS_AVAILABLE = 191;
    SSL_R_NO_PUBLICKEY = 192;
    SSL_R_NO_RENEGOTIATION = 339;
    SSL_R_NO_REQUIRED_DIGEST = 324;
    SSL_R_NO_SHARED_CIPHER = 193;
    SSL_R_NO_SRTP_PROFILES = 359;
    SSL_R_NO_VERIFY_CALLBACK = 194;
    SSL_R_NULL_SSL_CTX = 195;
    SSL_R_NULL_SSL_METHOD_PASSED = 196;
    SSL_R_OLD_SESSION_CIPHER_NOT_RETURNED = 197;
    SSL_R_OLD_SESSION_COMPRESSION_ALGORITHM_NOT_RETURNED = 344;
    SSL_R_ONLY_TLS_ALLOWED_IN_FIPS_MODE = 297;
    SSL_R_OPAQUE_PRF_INPUT_TOO_LONG = 327;
    SSL_R_PACKET_LENGTH_TOO_LONG = 198;
    SSL_R_PARSE_TLSEXT = 227;
    SSL_R_PATH_TOO_LONG = 270;
    SSL_R_PEER_DID_NOT_RETURN_A_CERTIFICATE = 199;
    SSL_R_PEER_ERROR = 200;
    SSL_R_PEER_ERROR_CERTIFICATE = 201;
    SSL_R_PEER_ERROR_NO_CERTIFICATE = 202;
    SSL_R_PEER_ERROR_NO_CIPHER = 203;
    SSL_R_PEER_ERROR_UNSUPPORTED_CERTIFICATE_TYPE = 204;
    SSL_R_PRE_MAC_LENGTH_TOO_LONG = 205;
    SSL_R_PROBLEMS_MAPPING_CIPHER_FUNCTIONS = 206;
    SSL_R_PROTOCOL_IS_SHUTDOWN = 207;
    SSL_R_PSK_IDENTITY_NOT_FOUND = 223;
    SSL_R_PSK_NO_CLIENT_CB = 224;
    SSL_R_PSK_NO_SERVER_CB = 225;
    SSL_R_PUBLIC_KEY_ENCRYPT_ERROR = 208;
    SSL_R_PUBLIC_KEY_IS_NOT_RSA = 209;
    SSL_R_PUBLIC_KEY_NOT_RSA = 210;
    SSL_R_READ_BIO_NOT_SET = 211;
    SSL_R_READ_TIMEOUT_EXPIRED = 312;
    SSL_R_READ_WRONG_PACKET_TYPE = 212;
    SSL_R_RECORD_LENGTH_MISMATCH = 213;
    SSL_R_RECORD_TOO_LARGE = 214;
    SSL_R_RECORD_TOO_SMALL = 298;
    SSL_R_RENEGOTIATE_EXT_TOO_LONG = 335;
    SSL_R_RENEGOTIATION_ENCODING_ERR = 336;
    SSL_R_RENEGOTIATION_MISMATCH = 337;
    SSL_R_REQUIRED_CIPHER_MISSING = 215;
    SSL_R_REQUIRED_COMPRESSSION_ALGORITHM_MISSING = 342;
    SSL_R_REUSE_CERT_LENGTH_NOT_ZERO = 216;
    SSL_R_REUSE_CERT_TYPE_NOT_ZERO = 217;
    SSL_R_REUSE_CIPHER_LIST_NOT_ZERO = 218;
    SSL_R_SCSV_RECEIVED_WHEN_RENEGOTIATING = 345;
    SSL_R_SERVERHELLO_TLSEXT = 275;
    SSL_R_SESSION_ID_CONTEXT_UNINITIALIZED = 277;
    SSL_R_SHORT_READ = 219;
    SSL_R_SIGNATURE_ALGORITHMS_ERROR = 360;
    SSL_R_SIGNATURE_FOR_NON_SIGNING_CERTIFICATE = 220;
    SSL_R_SRP_A_CALC = 361;
    SSL_R_SRTP_COULD_NOT_ALLOCATE_PROFILES = 362;
    SSL_R_SRTP_PROTECTION_PROFILE_LIST_TOO_LONG = 363;
    SSL_R_SRTP_UNKNOWN_PROTECTION_PROFILE = 364;
    SSL_R_SSL23_DOING_SESSION_ID_REUSE = 221;
    SSL_R_SSL2_CONNECTION_ID_TOO_LONG = 299;
    SSL_R_SSL3_EXT_INVALID_ECPOINTFORMAT = 321;
    SSL_R_SSL3_EXT_INVALID_SERVERNAME = 319;
    SSL_R_SSL3_EXT_INVALID_SERVERNAME_TYPE = 320;
    SSL_R_SSL3_SESSION_ID_TOO_LONG = 300;
    SSL_R_SSL3_SESSION_ID_TOO_SHORT = 222;
    SSL_R_SSLV3_ALERT_BAD_CERTIFICATE = 1042;
    SSL_R_SSLV3_ALERT_BAD_RECORD_MAC = 1020;
    SSL_R_SSLV3_ALERT_CERTIFICATE_EXPIRED = 1045;
    SSL_R_SSLV3_ALERT_CERTIFICATE_REVOKED = 1044;
    SSL_R_SSLV3_ALERT_CERTIFICATE_UNKNOWN = 1046;
    SSL_R_SSLV3_ALERT_DECOMPRESSION_FAILURE = 1030;
    SSL_R_SSLV3_ALERT_HANDSHAKE_FAILURE = 1040;
    SSL_R_SSLV3_ALERT_ILLEGAL_PARAMETER = 1047;
    SSL_R_SSLV3_ALERT_NO_CERTIFICATE = 1041;
    SSL_R_SSLV3_ALERT_UNEXPECTED_MESSAGE = 1010;
    SSL_R_SSLV3_ALERT_UNSUPPORTED_CERTIFICATE = 1043;
    SSL_R_SSL_CTX_HAS_NO_DEFAULT_SSL_VERSION = 228;
    SSL_R_SSL_HANDSHAKE_FAILURE = 229;
    SSL_R_SSL_LIBRARY_HAS_NO_CIPHERS = 230;
    SSL_R_SSL_SESSION_ID_CALLBACK_FAILED = 301;
    SSL_R_SSL_SESSION_ID_CONFLICT = 302;
    SSL_R_SSL_SESSION_ID_CONTEXT_TOO_LONG = 273;
    SSL_R_SSL_SESSION_ID_HAS_BAD_LENGTH = 303;
    SSL_R_SSL_SESSION_ID_IS_DIFFERENT = 231;
    SSL_R_TLSV1_ALERT_ACCESS_DENIED = 1049;
    SSL_R_TLSV1_ALERT_DECODE_ERROR = 1050;
    SSL_R_TLSV1_ALERT_DECRYPTION_FAILED = 1021;
    SSL_R_TLSV1_ALERT_DECRYPT_ERROR = 1051;
    SSL_R_TLSV1_ALERT_EXPORT_RESTRICTION = 1060;
    SSL_R_TLSV1_ALERT_INAPPROPRIATE_FALLBACK = 1086;
    SSL_R_TLSV1_ALERT_INSUFFICIENT_SECURITY = 1071;
    SSL_R_TLSV1_ALERT_INTERNAL_ERROR = 1080;
    SSL_R_TLSV1_ALERT_NO_RENEGOTIATION = 1100;
    SSL_R_TLSV1_ALERT_PROTOCOL_VERSION = 1070;
    SSL_R_TLSV1_ALERT_RECORD_OVERFLOW = 1022;
    SSL_R_TLSV1_ALERT_UNKNOWN_CA = 1048;
    SSL_R_TLSV1_ALERT_USER_CANCELLED = 1090;
    SSL_R_TLSV1_BAD_CERTIFICATE_HASH_VALUE = 1114;
    SSL_R_TLSV1_BAD_CERTIFICATE_STATUS_RESPONSE = 1113;
    SSL_R_TLSV1_CERTIFICATE_UNOBTAINABLE = 1111;
    SSL_R_TLSV1_UNRECOGNIZED_NAME = 1112;
    SSL_R_TLSV1_UNSUPPORTED_EXTENSION = 1110;
    SSL_R_TLS_CLIENT_CERT_REQ_WITH_ANON_CIPHER = 232;
    SSL_R_TLS_HEARTBEAT_PEER_DOESNT_ACCEPT = 365;
    SSL_R_TLS_HEARTBEAT_PENDING = 366;
    SSL_R_TLS_ILLEGAL_EXPORTER_LABEL = 367;
    SSL_R_TLS_INVALID_ECPOINTFORMAT_LIST = 157;
    SSL_R_TLS_PEER_DID_NOT_RESPOND_WITH_CERTIFICATE_LIST = 233;
    SSL_R_TLS_RSA_ENCRYPTED_VALUE_LENGTH_IS_WRONG = 234;
    SSL_R_TRIED_TO_USE_UNSUPPORTED_CIPHER = 235;
    SSL_R_UNABLE_TO_DECODE_DH_CERTS = 236;
    SSL_R_UNABLE_TO_DECODE_ECDH_CERTS = 313;
    SSL_R_UNABLE_TO_EXTRACT_PUBLIC_KEY = 237;
    SSL_R_UNABLE_TO_FIND_DH_PARAMETERS = 238;
    SSL_R_UNABLE_TO_FIND_ECDH_PARAMETERS = 314;
    SSL_R_UNABLE_TO_FIND_PUBLIC_KEY_PARAMETERS = 239;
    SSL_R_UNABLE_TO_FIND_SSL_METHOD = 240;
    SSL_R_UNABLE_TO_LOAD_SSL2_MD5_ROUTINES = 241;
    SSL_R_UNABLE_TO_LOAD_SSL3_MD5_ROUTINES = 242;
    SSL_R_UNABLE_TO_LOAD_SSL3_SHA1_ROUTINES = 243;
    SSL_R_UNEXPECTED_MESSAGE = 244;
    SSL_R_UNEXPECTED_RECORD = 245;
    SSL_R_UNINITIALIZED = 276;
    SSL_R_UNKNOWN_ALERT_TYPE = 246;
    SSL_R_UNKNOWN_CERTIFICATE_TYPE = 247;
    SSL_R_UNKNOWN_CIPHER_RETURNED = 248;
    SSL_R_UNKNOWN_CIPHER_TYPE = 249;
    SSL_R_UNKNOWN_DIGEST = 368;
    SSL_R_UNKNOWN_KEY_EXCHANGE_TYPE = 250;
    SSL_R_UNKNOWN_PKEY_TYPE = 251;
    SSL_R_UNKNOWN_PROTOCOL = 252;
    SSL_R_UNKNOWN_REMOTE_ERROR_TYPE = 253;
    SSL_R_UNKNOWN_SSL_VERSION = 254;
    SSL_R_UNKNOWN_STATE = 255;
    SSL_R_UNSAFE_LEGACY_RENEGOTIATION_DISABLED = 338;
    SSL_R_UNSUPPORTED_CIPHER = 256;
    SSL_R_UNSUPPORTED_COMPRESSION_ALGORITHM = 257;
    SSL_R_UNSUPPORTED_DIGEST_TYPE = 326;
    SSL_R_UNSUPPORTED_ELLIPTIC_CURVE = 315;
    SSL_R_UNSUPPORTED_PROTOCOL = 258;
    SSL_R_UNSUPPORTED_SSL_VERSION = 259;
    SSL_R_UNSUPPORTED_STATUS_TYPE = 329;
    SSL_R_USE_SRTP_NOT_NEGOTIATED = 369;
    SSL_R_WRITE_BIO_NOT_SET = 260;
    SSL_R_WRONG_CIPHER_RETURNED = 261;
    SSL_R_WRONG_MESSAGE_TYPE = 262;
    SSL_R_WRONG_NUMBER_OF_KEY_BITS = 263;
    SSL_R_WRONG_SIGNATURE_LENGTH = 264;
    SSL_R_WRONG_SIGNATURE_SIZE = 265;
    SSL_R_WRONG_SIGNATURE_TYPE = 370;
    SSL_R_WRONG_SSL_VERSION = 266;
    SSL_R_WRONG_VERSION_NUMBER = 267;
    SSL_R_X509_LIB = 268;
    SSL_R_X509_VERIFICATION_SETUP_PROBLEMS = 269;

 { following from objects.h }
    NID_undef                       =  0;  //AG
    NID_rsaEncryption               =  6;  //AG
    NID_md2WithRSAEncryption        =  7;  //  md2WithRSAEncryption
    NID_md5WithRSAEncryption        =  8;  //  md5WithRSAEncryption
    NID_commonName                  = 13;  //AG
    NID_countryName                 = 14;  //AG
    NID_localityName                = 15;  //AG
    NID_stateOrProvinceName         = 16;  //AG
    NID_organizationName            = 17;  //AG
    NID_organizationalUnitName      = 18;  //AG
    NID_pkcs7                       = 20;
    NID_pkcs7_data                  = 21;
    NID_pkcs7_signed                = 22;
    NID_pkcs7_enveloped             = 23;
    NID_pkcs7_signedAndEnveloped    = 24;
    NID_pkcs7_digest                = 25;
    NID_pkcs7_encrypted             = 26;
    NID_dhKeyAgreement              = 28;  // Angus
    NID_pkcs9_emailAddress          = 48;  // emailAddress - Email
    NID_netscape                    = 57;
    NID_netscape_cert_extension     = 58;
    NID_netscape_data_type          = 59;
    NID_sha1WithRSAEncryption       = 65;   // sha1WithRSAEncryption
    NID_dsaWithSHA                  = 66;   // dsaWithSHA
    NID_netscape_base_url           = 72;
    NID_netscape_ca_revocation_url  = 74;
    NID_netscape_cert_type          = 71;
    NID_netscape_revocation_url     = 73;
    NID_netscape_renewal_url        = 75;
    NID_netscape_ca_policy_url      = 76;
    NID_netscape_ssl_server_name    = 77;
    NID_netscape_comment            = 78;
    NID_netscape_cert_sequence      = 79;
    NID_subject_key_identifier      = 82;  // X509v3 Subject Key Identifier - subjectKeyIdentifier
    NID_key_usage                   = 83;  // X509v3 Key Usage - keyUsage
    NID_private_key_usage_period    = 84;  // X509v3 Private Key Usage Period - privateKeyUsagePeriod
    NID_subject_alt_name            = 85;  // X509v3 Subject Alternative Name - subjectAltName
    NID_issuer_alt_name             = 86;  // X509v3 Issuer Alternative Name - issuerAltName
    NID_basic_constraints           = 87;  // X509v3 Basic Constraints - basicConstraints
    NID_crl_number                  = 88;  // X509v3 CRL Number - crlNumber
    NID_certificate_policies        = 89;  // X509v3 Certificate Policies - certificatePolicies
    NID_authority_key_identifier    = 90;  // X509v3 Authority Key Identifier - authorityKeyIdentifier
    NID_givenName                   = 99;  // givenName - G
    NID_surname                     = 100; // surname - S
    NID_initials                    = 101; // initials - I
    NID_x500UniqueIdentifier        = 102; // uniqueIdentifier - UID
    NID_crl_distribution_points     = 103; // X509v3 CRL Distribution Points - crlDistributionPoints
    NID_serialNumber                = 105; // serialNumber - SN
    NID_title                       = 106; // title - T
    NID_description                 = 107; // description - D
    NID_dsa                         = 116;
    NID_ext_key_usage               = 126;  // X509v3 Extended Key Usage - extendedKeyUsage
    // PKIX extended key usage OIDs
    NID_server_auth                 = 129;  // TLS Web Server Authentication - serverAuth
    NID_client_auth                 = 130;  // TLS Web Client Authentication - clientAuth
    NID_code_sign                   = 131;  // Code Signing - codeSigning
    NID_email_protect               = 132;  // E-mail Protection - emailProtection
    NID_pbe_WithSHA1And128BitRC4           = 144;
    NID_pbe_WithSHA1And40BitRC4            = 145;
    NID_pbe_WithSHA1And3_Key_TripleDES_CBC = 146;
    NID_pbe_WithSHA1And2_Key_TripleDES_CBC = 147;
    NID_pbe_WithSHA1And128BitRC2_CBC       = 148;
    NID_pbe_WithSHA1And40BitRC2_CBC        = 149;
    NID_info_access                 = 177;  // Authority Information Access - authorityInfoAccess
 { following from obj_mac.h }
    NID_X9_62_id_ecPublicKey        = 408;  // id-ecPublicKey
    NID_X9_62_prime192v1            = 409; // Angus elliptic curves
    NID_X9_62_prime192v2            = 410; // Angus
    NID_X9_62_prime192v3            = 411; // Angus
    NID_X9_62_prime239v1            = 412; // Angus
    NID_X9_62_prime239v2            = 413; // Angus
    NID_X9_62_prime239v3            = 414; // Angus
    NID_X9_62_prime256v1            = 415; // Angus  NIST Prime-Curve P-256
    NID_sha256WithRSAEncryption     = 668;  // sha256WithRSAEncryption
    NID_sha384WithRSAEncryption     = 669;  // sha384WithRSAEncryption
    NID_sha512WithRSAEncryption     = 670;  // sha512WithRSAEncryption
    NID_sha224WithRSAEncryption     = 671;  // sha224WithRSAEncryption
    NID_secp224k1                   = 713; // Angus  NIST Prime-Curve P-224
    NID_secp256k1                   = 714; // Angus  NID_X9_62_prime256v1 NIST Prime-Curve P-256
    NID_secp384r1                   = 715; // Angus  NIST Prime-Curve P-384
    NID_secp521r1                   = 716; // Angus  NIST Prime-Curve P-521
    NID_ecdsa_with_SHA224           = 793; // ecdsa-with-SHA224
    NID_ecdsa_with_SHA256           = 794; // ecdsa-with-SHA256
    NID_ecdsa_with_SHA384           = 795; // ecdsa-with-SHA384
    NID_ecdsa_with_SHA512           = 796; // ecdsa-with-SHA512
    NID_X25519                      = 1034; // V8.40 253-bits

    { Asn1.h - For use with ASN1_mbstring_copy() } //AG
    MBSTRING_FLAG  = $1000;               //AG
    MBSTRING_ASC   = MBSTRING_FLAG or 1;  //AG
    MBSTRING_BMP   = MBSTRING_FLAG or 2;  //AG
    { 0.9.7 }
    MBSTRING_UNIV  : Longword = MBSTRING_FLAG or 3;
    MBSTRING_UTF8  : Longword = MBSTRING_FLAG or 4;
    (*
    { 0.9.8 they are set dynamically on load }
    MBSTRING_UNIV  = MBSTRING_FLAG or 4;  //AG
    MBSTRING_UTF8  = MBSTRING_FLAG;       //AG
    *)

    RSA_F4         = $10001;              //AG
    EVP_PKEY_RSA   = NID_rsaEncryption;   //AG
    EVP_PKEY_DSA   = NID_dsa;
    EVP_PKEY_EC    = NID_X9_62_id_ecPublicKey;  { following V8.40 }
    EVP_PKEY_DH    = NID_dhKeyAgreement;
    EVP_PKEY_NONE  = NID_undef;

    EVP_MAX_MD_SIZE                   = 64; //* longest known is SHA512 */

    { V8.40 some values for the EC_KEY encoding_flag }
    EC_PKEY_NO_PARAMETERS    = $01;
    EC_PKEY_NO_PUBKEY        = $02;

    { V8.40 some values for the EC_KEY flags field }
    EC_FLAG_NON_FIPS_ALLOW  = $01;
    EC_FLAG_FIPS_CHECKED    = $02;
    EC_FLAG_COFACTOR_ECDH   = $1000;

  { point_conversion_form_t Enum for the point conversion form as defined in X9.62 (ECDSA) for the encoding of a elliptic curve point (x,y) }
    POINT_CONVERSION_COMPRESSED = 2; { the point is encoded as z||x||y, where z is the octet 0x04  }
    POINT_CONVERSION_UNCOMPRESSED = 4; { the point is encoded as z||x||y, where the octet z specifies which solution of the quadratic equation y is  }
    POINT_CONVERSION_HYBRID = 6;

    { Crypto.h - params for f_SSLeay_version() }
    SSLEAY_VERSION      = 0;
    SSLEAY_OPTIONS      = 1; //no longer supported
    SSLEAY_CFLAGS       = 2;
    SSLEAY_BUILT_ON     = 3;
    SSLEAY_PLATFORM     = 4;
    SSLEAY_DIR          = 5; // since 0.9.7

    { V8.27 Crypto.h - params for f_SOpenSSL_version() - WARNING changed from SSLEAY versions }
    OPENSSL_VERSION        = 0;
    OPENSSL_CFLAGS         = 1;
    OPENSSL_BUILT_ON       = 2;
    OPENSSL_PLATFORM       = 3;
    OPENSSL_DIR            = 4;
    OPENSSL_ENGINES_DIR    = 5;

    X509_V_OK                                           = 0;
    // illegal error (for uninitialized values, to avoid X509_V_OK): 1
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT                = 2;
    X509_V_ERR_UNABLE_TO_GET_CRL                        = 3;
    X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE         = 4;
    X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE          = 5;
    X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY       = 6;
    X509_V_ERR_CERT_SIGNATURE_FAILURE                   = 7;
    X509_V_ERR_CRL_SIGNATURE_FAILURE                    = 8;
    X509_V_ERR_CERT_NOT_YET_VALID                       = 9;
    X509_V_ERR_CERT_HAS_EXPIRED                         = 10;
    X509_V_ERR_CRL_NOT_YET_VALID                        = 11;
    X509_V_ERR_CRL_HAS_EXPIRED                          = 12;
    X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD           = 13;
    X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD            = 14;
    X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD           = 15;
    X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD           = 16;
    X509_V_ERR_OUT_OF_MEM                               = 17;
    X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT              = 18;
    X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN                = 19;
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY        = 20;
    X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE          = 21;
    X509_V_ERR_CERT_CHAIN_TOO_LONG                      = 22;
    X509_V_ERR_CERT_REVOKED                             = 23;
    X509_V_ERR_INVALID_CA                               = 24;
    X509_V_ERR_PATH_LENGTH_EXCEEDED                     = 25;
    X509_V_ERR_INVALID_PURPOSE                          = 26;
    X509_V_ERR_CERT_UNTRUSTED                           = 27;
    X509_V_ERR_CERT_REJECTED                            = 28;
    // These are 'informational' when looking for issuer cert
    X509_V_ERR_SUBJECT_ISSUER_MISMATCH                  = 29;
    X509_V_ERR_AKID_SKID_MISMATCH                       = 30;
    X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH              = 31;
    X509_V_ERR_KEYUSAGE_NO_CERTSIGN                     = 32;

    X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER                 = 33;
    X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION             = 34;
    X509_V_ERR_KEYUSAGE_NO_CRL_SIGN                     = 35;
    X509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION         = 36;
    X509_V_ERR_INVALID_NON_CA                           = 37;
    X509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED               = 38;
    X509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE            = 39;
    X509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED           = 40;

    X509_V_ERR_INVALID_EXTENSION                        = 41;
    X509_V_ERR_INVALID_POLICY_EXTENSION                 = 42;
    X509_V_ERR_NO_EXPLICIT_POLICY                       = 43;
{ V8.39 added v_err 44 to 72 }
    X509_V_ERR_DIFFERENT_CRL_SCOPE                 = 44;
    X509_V_ERR_UNSUPPORTED_EXTENSION_FEATURE       = 45;
    X509_V_ERR_UNNESTED_RESOURCE                   = 46;
    X509_V_ERR_PERMITTED_VIOLATION                 = 47;
    X509_V_ERR_EXCLUDED_VIOLATION                  = 48;
    X509_V_ERR_SUBTREE_MINMAX                      = 49;
    // The application is not happy
    X509_V_ERR_APPLICATION_VERIFICATION            = 50;
    X509_V_ERR_UNSUPPORTED_CONSTRAINT_TYPE         = 51;
    X509_V_ERR_UNSUPPORTED_CONSTRAINT_SYNTAX       = 52;
    X509_V_ERR_UNSUPPORTED_NAME_SYNTAX             = 53;
    X509_V_ERR_CRL_PATH_VALIDATION_ERROR           = 54;
    // Another issuer check debug option
    X509_V_ERR_PATH_LOOP                           = 55;
    // Suite B mode algorithm violation
    X509_V_ERR_SUITE_B_INVALID_VERSION             = 56;
    X509_V_ERR_SUITE_B_INVALID_ALGORITHM           = 57;
    X509_V_ERR_SUITE_B_INVALID_CURVE               = 58;
    X509_V_ERR_SUITE_B_INVALID_SIGNATURE_ALGORITHM = 59;
    X509_V_ERR_SUITE_B_LOS_NOT_ALLOWED             = 60;
    X509_V_ERR_SUITE_B_CANNOT_SIGN_P_384_WITH_P_256 = 61;
    // Host, email and IP check errors
    X509_V_ERR_HOSTNAME_MISMATCH                   = 62;
    X509_V_ERR_EMAIL_MISMATCH                      = 63;
    X509_V_ERR_IP_ADDRESS_MISMATCH                 = 64;
    // DANE TLSA errors
    X509_V_ERR_DANE_NO_MATCH                       = 65;
    // security level errors
    X509_V_ERR_EE_KEY_TOO_SMALL                    = 66;
    X509_V_ERR_CA_KEY_TOO_SMALL                    = 67;
    X509_V_ERR_CA_MD_TOO_WEAK                      = 68;
    // Caller error
    X509_V_ERR_INVALID_CALL                        = 69;
    // Issuer lookup error
    X509_V_ERR_STORE_LOOKUP                        = 70;
    // Certificate transparency
    X509_V_ERR_NO_VALID_SCTS                       = 71;
    X509_V_ERR_PROXY_SUBJECT_NAME_VIOLATION        = 72;

{ V8.41 Flags for X509V3_add1_i2d  }
    X509V3_ADD_OP_MASK             = $0f;
    X509V3_ADD_DEFAULT             = 0;
    X509V3_ADD_APPEND              = 1;
    X509V3_ADD_REPLACE             = 2;
    X509V3_ADD_REPLACE_EXISTING    = 3;
    X509V3_ADD_KEEP_EXISTING       = 4;
    X509V3_ADD_DELETE              = 5;
    X509V3_ADD_SILENT              = $10;

{ note these literals are not normally used, OpenSSL API instead }
{$IFDEF OPENSSL_USE_RESOURCE_STRINGS}
resourcestring
  { Verify error strings from x509_txt.c }
  sX509_V_OK                                      = 'ok';
  sX509_V_ERR_UNABLE_TO_GET_ISSUER_CERT           = 'unable to get issuer certificate';
  sX509_V_ERR_UNABLE_TO_GET_CRL                   = 'unable to get certificate CRL';
  sX509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE    = 'unable to decrypt certificate''s signature';
  sX509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE     = 'unable to decrypt CRL''s signature';
  sX509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY  = 'unable to decode issuer public key';
  sX509_V_ERR_CERT_SIGNATURE_FAILURE              = 'certificate signature failure';
  sX509_V_ERR_CRL_SIGNATURE_FAILURE               = 'CRL signature failure';
  sX509_V_ERR_CERT_NOT_YET_VALID                  = 'certificate is not yet valid';
  sX509_V_ERR_CRL_NOT_YET_VALID                   = 'CRL is not yet valid';
  sX509_V_ERR_CERT_HAS_EXPIRED                    = 'certificate has expired';
  sX509_V_ERR_CRL_HAS_EXPIRED                     = 'CRL has expired';
  sX509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD      = 'format error in certificate''s notBefore field';
  sX509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD       = 'format error in certificate''s notAfter field';
  sX509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD      = 'format error in CRL''s lastUpdate field';
  sX509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD      = 'format error in CRL''s nextUpdate field';
  sX509_V_ERR_OUT_OF_MEM                          = 'out of memory';
  sX509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT         = 'self signed certificate';
  sX509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN           = 'self signed certificate in certificate chain';
  sX509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY   = 'unable to get local issuer certificate';
  sX509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE     = 'unable to verify the first certificate';
  sX509_V_ERR_CERT_CHAIN_TOO_LONG                 = 'certificate chain too long';
  sX509_V_ERR_CERT_REVOKED                        = 'certificate revoked';
  sX509_V_ERR_INVALID_CA                          = 'invalid CA certificate';
  sX509_V_ERR_INVALID_NON_CA                      = 'invalid non-CA certificate (has CA markings)';
  sX509_V_ERR_PATH_LENGTH_EXCEEDED                = 'path length constraint exceeded';
  sX509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED          = 'proxy path length constraint exceeded';
  sX509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED      = 'proxy certificates not allowed, please set the appropriate flag';
  sX509_V_ERR_INVALID_PURPOSE                     = 'unsupported certificate purpose';
  sX509_V_ERR_CERT_UNTRUSTED                      = 'certificate not trusted';
  sX509_V_ERR_CERT_REJECTED                       = 'certificate rejected';
  sX509_V_ERR_APPLICATION_VERIFICATION            = 'application verification failure';
  sX509_V_ERR_SUBJECT_ISSUER_MISMATCH             = 'subject issuer mismatch';
  sX509_V_ERR_AKID_SKID_MISMATCH                  = 'authority and subject key identifier mismatch';
  sX509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH         = 'authority and issuer serial number mismatch';
  sX509_V_ERR_KEYUSAGE_NO_CERTSIGN                = 'key usage does not include certificate signing';
  sX509_V_ERR_UNABLE_TO_GET_CRL_ISSUER            = 'unable to get CRL issuer certificate';
  sX509_V_ERR_UNHANDLED_CRITICAL_EXTENSION        = 'unhandled critical extension';
  sX509_V_ERR_KEYUSAGE_NO_CRL_SIGN                = 'key usage does not include CRL signing';
  sX509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE       = 'key usage does not include digital signature';
  sX509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION    = 'unhandled critical CRL extension';
  sX509_V_ERR_INVALID_EXTENSION                   = 'invalid or inconsistent certificate extension';
  sX509_V_ERR_INVALID_POLICY_EXTENSION            = 'invalid or inconsistent certificate policy extension';
  sX509_V_ERR_NO_EXPLICIT_POLICY                  = 'no explicit policy';
{ V8.39 more }
  sX509_V_ERR_DIFFERENT_CRL_SCOPE               = 'Different CRL scope';
  sX509_V_ERR_UNSUPPORTED_EXTENSION_FEATURE     = 'Unsupported extension feature';
  sX509_V_ERR_UNNESTED_RESOURCE                 = 'RFC 3779 resource not subset of parent's resources';
  sX509_V_ERR_PERMITTED_VIOLATION               = 'permitted subtree violation';
  sX509_V_ERR_EXCLUDED_VIOLATION                = 'excluded subtree violation';
  sX509_V_ERR_SUBTREE_MINMAX                    = 'name constraints minimum and maximum not supported';
  sX509_V_ERR_APPLICATION_VERIFICATION          = 'application verification failure';
  sX509_V_ERR_UNSUPPORTED_CONSTRAINT_TYPE       = 'unsupported name constraint type';
  sX509_V_ERR_UNSUPPORTED_CONSTRAINT_SYNTAX     = 'unsupported or invalid name constraint syntax';
  sX509_V_ERR_UNSUPPORTED_NAME_SYNTAX           = 'unsupported or invalid name syntax';
  sX509_V_ERR_CRL_PATH_VALIDATION_ERROR         = 'CRL path validation error';
  sX509_V_ERR_PATH_LOOP                         = 'Path Loop';
  sX509_V_ERR_SUITE_B_INVALID_VERSION           = 'Suite B: certificate version invalid';
  sX509_V_ERR_SUITE_B_INVALID_ALGORITHM         = 'Suite B: invalid public key algorithm';
  sX509_V_ERR_SUITE_B_INVALID_CURVE             = 'Suite B: invalid ECC curve';
  sX509_V_ERR_SUITE_B_INVALID_SIGNATURE_ALGORITHM   = 'Suite B: invalid signature algorithm';
  sX509_V_ERR_SUITE_B_LOS_NOT_ALLOWED           = 'Suite B: curve not allowed for this LOS';
  sX509_V_ERR_SUITE_B_CANNOT_SIGN_P_384_WITH_P_256  = 'Suite B: cannot sign P-384 with P-256';
  sX509_V_ERR_HOSTNAME_MISMATCH                 = 'Hostname mismatch';
  sX509_V_ERR_EMAIL_MISMATCH                    = 'Email address mismatch';
  sX509_V_ERR_IP_ADDRESS_MISMATCH               = 'IP address mismatch';
  sX509_V_ERR_DANE_NO_MATCH                     = 'No matching DANE TLSA records';
  sX509_V_ERR_EE_KEY_TOO_SMALL                  = 'EE certificate key too weak';
  sX509_V_ERR_CA_KEY_TOO_SMALL                  = 'CA certificate key too weak';
  sX509_V_ERR_CA_MD_TOO_WEAK                    = 'CA signature digest algorithm too weak';
  sX509_V_ERR_INVALID_CALL                      = 'Invalid certificate verification context';
  sX509_V_ERR_STORE_LOOKUP                      = 'Issuer certificate lookup error';
  sX509_V_ERR_NO_VALID_SCTS                     = 'Certificate Transparency required, but no valid SCTs found';
  sX509_V_ERR_PROXY_SUBJECT_NAME_VIOLATION      = 'proxy subject name violation';
  sX509_V_ERR_NUMBER                              = 'Error number ';

const
{$ENDIF}

  { Lock IDs for use with CRYPTO_lock() }
  CRYPTO_LOCK_ERR                     = 1;
  CRYPTO_LOCK_EX_DATA                 = 2;
  CRYPTO_LOCK_X509                    = 3;
  CRYPTO_LOCK_X509_INFO               = 4;
  CRYPTO_LOCK_X509_PKEY               = 5;
  CRYPTO_LOCK_X509_CRL                = 6;
  CRYPTO_LOCK_X509_REQ                = 7;
  CRYPTO_LOCK_DSA                     = 8;
  CRYPTO_LOCK_RSA                     = 9;
  CRYPTO_LOCK_EVP_PKEY                = 10;
  CRYPTO_LOCK_X509_STORE              = 11;
  CRYPTO_LOCK_SSL_CTX                 = 12;
  CRYPTO_LOCK_SSL_CERT                = 13;
  CRYPTO_LOCK_SSL_SESSION             = 14;
  CRYPTO_LOCK_SSL_SESS_CERT           = 15;
  CRYPTO_LOCK_SSL                     = 16;
  CRYPTO_LOCK_SSL_METHOD              = 17;
  CRYPTO_LOCK_RAND                    = 18;
  CRYPTO_LOCK_RAND2                   = 19;
  CRYPTO_LOCK_MALLOC                  = 20;
  CRYPTO_LOCK_BIO                     = 21;
  CRYPTO_LOCK_GETHOSTBYNAME           = 22;
  CRYPTO_LOCK_GETSERVBYNAME           = 23;
  CRYPTO_LOCK_READDIR                 = 24;
  CRYPTO_LOCK_RSA_BLINDING            = 25;
  CRYPTO_LOCK_DH                      = 26;
  CRYPTO_LOCK_MALLOC2                 = 27;
  CRYPTO_LOCK_DSO                     = 28;
  CRYPTO_LOCK_DYNLOCK                 = 29;
  CRYPTO_LOCK_ENGINE                  = 30;
  CRYPTO_LOCK_UI                      = 31;
  CRYPTO_LOCK_ECDSA                   = 32;
  CRYPTO_LOCK_EC                      = 33;
  CRYPTO_LOCK_ECDH                    = 34;
  CRYPTO_LOCK_BN                      = 35;
  CRYPTO_LOCK_EC_PRE_COMP             = 36;
  CRYPTO_LOCK_STORE                   = 37;
  CRYPTO_LOCK_COMP                    = 38;
  CRYPTO_LOCK_FIPS                    = 39;
  CRYPTO_LOCK_FIPS2                   = 40;
  CRYPTO_NUM_LOCKS                    = 41;

  { mode param of CRYPTO_lock()                                              }
  { These values are pairwise exclusive, with undefined behaviour if misused }
  {(for example, CRYPTO_READ and CRYPTO_WRITE should not be used together):  }
  CRYPTO_LOCK                         = 1;
  CRYPTO_UNLOCK                       = 2;
  CRYPTO_READ                         = 4;
  CRYPTO_WRITE                        = 8;

    // Certificate verify flags  X509_VERIFY_PARAM flags

    // Send issuer+subject checks to verify_cb
    X509_V_FLAG_CB_ISSUER_CHECK                         = $0;   { Deprecated }
    // Use check time instead of current time
    X509_V_FLAG_USE_CHECK_TIME                          = $2;
    // Lookup CRLs
    X509_V_FLAG_CRL_CHECK                               = $4;
    // Lookup CRLs for whole chain
    X509_V_FLAG_CRL_CHECK_ALL                           = $8;
    // Ignore unhandled critical extensions
    X509_V_FLAG_IGNORE_CRITICAL                         = $10;
    // Disable workarounds for broken certificates
    X509_V_FLAG_X509_STRICT                             = $20;
    // Enable proxy certificate validation
    X509_V_FLAG_ALLOW_PROXY_CERTS                       = $40;
{ V8.39 more X509_VERIFY_PARAM flags }
    // Enable policy checking
    X509_V_FLAG_POLICY_CHECK                = $80;
    // Policy variable require-explicit-policy
    X509_V_FLAG_EXPLICIT_POLICY             = $100;
    // Policy variable inhibit-any-policy
    X509_V_FLAG_INHIBIT_ANY                 = $200;
    // Policy variable inhibit-policy-mapping
    X509_V_FLAG_INHIBIT_MAP                 = $400;
    // Notify callback that policy is OK
    X509_V_FLAG_NOTIFY_POLICY               = $800;
    // Extended CRL features such as indirect CRLs, alternate CRL signing keys
    X509_V_FLAG_EXTENDED_CRL_SUPPORT        = $1000;
    // Delta CRL support
    X509_V_FLAG_USE_DELTAS                  = $2000;
    // Check self-signed CA signature
    X509_V_FLAG_CHECK_SS_SIGNATURE          = $4000;
    // Use trusted store first
    X509_V_FLAG_TRUSTED_FIRST               = $8000;
    // Suite B 128 bit only mode: not normally used
    X509_V_FLAG_SUITEB_128_LOS_ONLY         = $10000;
    // Suite B 192 bit only mode
    X509_V_FLAG_SUITEB_192_LOS              = $20000;
    // Suite B 128 bit mode allowing 192 bit algorithms
    X509_V_FLAG_SUITEB_128_LOS              = $30000;
    // Allow partial chains if at least one certificate is in trusted store
    X509_V_FLAG_PARTIAL_CHAIN               = $80000;
   {  If the initial chain is not trusted, do not attempt to build an alternative
      chain. Alternate chain checking was introduced in 1.1.0. Setting this flag
      will force the behaviour to match that of previous versions. }
    X509_V_FLAG_NO_ALT_CHAINS               = $100000;
    // Do not check certificate/CRL validity against current time  
    X509_V_FLAG_NO_CHECK_TIME               = $200000;

    //Purposes
    X509_PURPOSE_SSL_CLIENT                             = 1;
    X509_PURPOSE_SSL_SERVER                             = 2;
    X509_PURPOSE_NS_SSL_SERVER                          = 3;
    X509_PURPOSE_SMIME_SIGN                             = 4;
    X509_PURPOSE_SMIME_ENCRYPT                          = 5;
    X509_PURPOSE_CRL_SIGN                               = 6;
    X509_PURPOSE_ANY                                    = 7;
    X509_PURPOSE_OCSP_HELPER                            = 8;

    X509_PURPOSE_MIN                                    = 1;
    X509_PURPOSE_MAX                                    = 8;

{ V8.39 flags for X509_check_host and X509_VERIFY_PARAM_set_hostflags }
    // Always check subject name for host match even if subject alt names present
    X509_CHECK_FLAG_ALWAYS_CHECK_SUBJECT            = $1;
    // Disable wildcard matching for dnsName fields and common name.
    X509_CHECK_FLAG_NO_WILDCARDS                    = $2;
    // Wildcards must not match a partial label.
    X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS            = $4;
    // Allow (non-partial) wildcards to match multiple labels.
    X509_CHECK_FLAG_MULTI_LABEL_WILDCARDS           = $8;
    // Constraint verifier subdomain patterns to match a single labels.
    X509_CHECK_FLAG_SINGLE_LABEL_SUBDOMAINS         = $10;
    // Never check the subject CN
    X509_CHECK_FLAG_NEVER_CHECK_SUBJECT             = $20;

 { V8.40 add bit masks for print, warning might have got shifted bits wrong !!! }
 { Flags for X509_print_ex() }
    X509_FLAG_COMPAT                = $00;
    X509_FLAG_NO_HEADER             = $01;
    X509_FLAG_NO_VERSION            = $02;  // (1L << 1)
    X509_FLAG_NO_SERIAL             = $04;
    X509_FLAG_NO_SIGNAME            = $08;
    X509_FLAG_NO_ISSUER             = $10;
    X509_FLAG_NO_VALIDITY           = $20;
    X509_FLAG_NO_SUBJECT            = $40;
    X509_FLAG_NO_PUBKEY             = $80;
    X509_FLAG_NO_EXTENSIONS         = $0100;
    X509_FLAG_NO_SIGDUMP            = $0200;
    X509_FLAG_NO_AUX                = $0400;
    X509_FLAG_NO_ATTRIBUTES         = $0800;
    X509_FLAG_NO_IDS                = $1000;  // (1L << 12)

 { Flags specific to X509_NAME_print_ex() }
 { The field separator information }
    XN_FLAG_SEP_MASK        = $0f000000; { (0xf << 16) }

    XN_FLAG_COMPAT          = $00;  // Traditional; use old X509_NAME_print
    XN_FLAG_SEP_COMMA_PLUS  = $010000; // (1 << 16) RFC2253 ,+
    XN_FLAG_SEP_CPLUS_SPC   = $020000; //* (2 << 16) + spaced: more readable
    XN_FLAG_SEP_SPLUS_SPC   = $040000; //* (3 << 16) + spaced
    XN_FLAG_SEP_MULTILINE   = $080000; //* (4 << 16) One line per field

    XN_FLAG_DN_REV          = $01000000; //(1 << 20) Reverse DN order

 { How the field name is shown }
  //  XN_FLAG_FN_MASK         (0x3 << 21)

    XN_FLAG_FN_SN           = $00; // Object short name
    XN_FLAG_FN_LN           = $02000000; // (1 << 21) Object long name
    XN_FLAG_FN_OID          = $04000000; // (2 << 21) Always use OIDs
    XN_FLAG_FN_NONE         = $08000000; // (3 << 21) No field names

    XN_FLAG_SPC_EQ          = $10000000; // (1 << 23) Put spaces round '='

{$IFNDEF OPENSSL_NO_ENGINE}
//const
    // engine.h //

    //* These flags are used to control combinations of algorithm (methods)
    //* by bitwise "OR"ing.
    ENGINE_METHOD_RSA                   = $0001;
    ENGINE_METHOD_DSA                   = $0002;
    ENGINE_METHOD_DH                    = $0004;
    ENGINE_METHOD_RAND                  = $0008;
    ENGINE_METHOD_ECDH                  = $0010;
    ENGINE_METHOD_ECDSA                 = $0020;
    ENGINE_METHOD_CIPHERS               = $0040;
    ENGINE_METHOD_DIGESTS               = $0080;
    ENGINE_METHOD_STORE                 = $0100;
    //* Obvious all-or-nothing cases. */
    ENGINE_METHOD_ALL                   = $FFFF;
    ENGINE_METHOD_NONE                  = $0000;

    //* Error codes for the ENGINE functions. */

    //* Function codes. */
    {
    ENGINE_F_DYNAMIC_CTRL                 = 180;
    ENGINE_F_DYNAMIC_GET_DATA_CTX         = 181;
    ENGINE_F_DYNAMIC_LOAD                 = 182;
    ENGINE_F_DYNAMIC_SET_DATA_CTX         = 183;
    ENGINE_F_ENGINE_ADD                   = 105;
    ENGINE_F_ENGINE_BY_ID                 = 106;
    ENGINE_F_ENGINE_CMD_IS_EXECUTABLE     = 170;
    ENGINE_F_ENGINE_CTRL                  = 142;
    ENGINE_F_ENGINE_CTRL_CMD              = 178;
    ENGINE_F_ENGINE_CTRL_CMD_STRING       = 171;
    ENGINE_F_ENGINE_FINISH                = 107;
    ENGINE_F_ENGINE_FREE_UTIL             = 108;
    ENGINE_F_ENGINE_GET_CIPHER            = 185;
    ENGINE_F_ENGINE_GET_DEFAULT_TYPE      = 177;
    ENGINE_F_ENGINE_GET_DIGEST            = 186;
    ENGINE_F_ENGINE_GET_NEXT              = 115;
    ENGINE_F_ENGINE_GET_PREV              = 116;
    ENGINE_F_ENGINE_INIT                  = 119;
    ENGINE_F_ENGINE_LIST_ADD              = 120;
    ENGINE_F_ENGINE_LIST_REMOVE           = 121;
    ENGINE_F_ENGINE_LOAD_PRIVATE_KEY      = 150;
    ENGINE_F_ENGINE_LOAD_PUBLIC_KEY       = 151;
    ENGINE_F_ENGINE_LOAD_SSL_CLIENT_CERT  = 192;
    ENGINE_F_ENGINE_NEW                   = 122;
    ENGINE_F_ENGINE_REMOVE                = 123;
    ENGINE_F_ENGINE_SET_DEFAULT_STRING    = 189;
    ENGINE_F_ENGINE_SET_DEFAULT_TYPE      = 126;
    ENGINE_F_ENGINE_SET_ID                = 129;
    ENGINE_F_ENGINE_SET_NAME              = 130;
    ENGINE_F_ENGINE_TABLE_REGISTER        = 184;
    ENGINE_F_ENGINE_UNLOAD_KEY            = 152;
    ENGINE_F_ENGINE_UNLOCKED_FINISH       = 191;
    ENGINE_F_ENGINE_UP_REF                = 190;
    ENGINE_F_INT_CTRL_HELPER              = 172;
    ENGINE_F_INT_ENGINE_CONFIGURE         = 188;
    ENGINE_F_INT_ENGINE_MODULE_INIT       = 187;
    ENGINE_F_LOG_MESSAGE                  = 141;
    }
    //* Reason codes. */
    {
    ENGINE_R_ALREADY_LOADED               = 100;
    ENGINE_R_ARGUMENT_IS_NOT_A_NUMBER     = 133;
    ENGINE_R_CMD_NOT_EXECUTABLE           = 134;
    ENGINE_R_COMMAND_TAKES_INPUT          = 135;
    ENGINE_R_COMMAND_TAKES_NO_INPUT       = 136;
    ENGINE_R_CONFLICTING_ENGINE_ID        = 103;
    ENGINE_R_CTRL_COMMAND_NOT_IMPLEMENTED = 119;
    ENGINE_R_DH_NOT_IMPLEMENTED           = 139;
    ENGINE_R_DSA_NOT_IMPLEMENTED          = 140;
    ENGINE_R_DSO_FAILURE                  = 104;
    ENGINE_R_DSO_NOT_FOUND                = 132;
    ENGINE_R_ENGINES_SECTION_ERROR        = 148;
    ENGINE_R_ENGINE_IS_NOT_IN_LIST        = 105;
    ENGINE_R_ENGINE_SECTION_ERROR         = 149;
    ENGINE_R_FAILED_LOADING_PRIVATE_KEY   = 128;
    ENGINE_R_FAILED_LOADING_PUBLIC_KEY    = 129;
    ENGINE_R_FINISH_FAILED                = 106;
    ENGINE_R_GET_HANDLE_FAILED            = 107;
    ENGINE_R_ID_OR_NAME_MISSING           = 108;
    ENGINE_R_INIT_FAILED                  = 109;
    ENGINE_R_INTERNAL_LIST_ERROR          = 110;
    ENGINE_R_INVALID_ARGUMENT             = 143;
    ENGINE_R_INVALID_CMD_NAME             = 137;
    ENGINE_R_INVALID_CMD_NUMBER           = 138;
    ENGINE_R_INVALID_INIT_VALUE           = 151;
    ENGINE_R_INVALID_STRING               = 150;
    ENGINE_R_NOT_INITIALISED              = 117;
    ENGINE_R_NOT_LOADED                   = 112;
    ENGINE_R_NO_CONTROL_FUNCTION          = 120;
    ENGINE_R_NO_INDEX                     = 144;
    ENGINE_R_NO_LOAD_FUNCTION             = 125;
    ENGINE_R_NO_REFERENCE                 = 130;
    ENGINE_R_NO_SUCH_ENGINE               = 116;
    ENGINE_R_NO_UNLOAD_FUNCTION           = 126;
    ENGINE_R_PROVIDE_PARAMETERS           = 113;
    ENGINE_R_RSA_NOT_IMPLEMENTED          = 141;
    ENGINE_R_UNIMPLEMENTED_CIPHER         = 146;
    ENGINE_R_UNIMPLEMENTED_DIGEST         = 147;
    ENGINE_R_VERSION_INCOMPATIBILITY      = 145;
    }
{$ENDIF}

//const
    BIO_CTRL_RESET         = 1;  // opt - rewind/zero etc
    BIO_CTRL_EOF           = 2;  // opt - are we at the eof
    BIO_CTRL_INFO          = 3;  // opt - extra tit-bits
    BIO_CTRL_SET           = 4;  // man - set the 'IO' type
    BIO_CTRL_GET           = 5;  // man - get the 'IO' type
    BIO_CTRL_PUSH          = 6;  // opt - internal, used to signify change
    BIO_CTRL_POP           = 7;  // opt - internal, used to signify change
    BIO_CTRL_GET_CLOSE     = 8;  // man - set the 'close' on free
    BIO_CTRL_SET_CLOSE     = 9;  // man - set the 'close' on free
    BIO_CTRL_PENDING       = 10; // opt - is their more data buffered
    BIO_CTRL_FLUSH         = 11; // opt - 'flush' buffered output
    BIO_CTRL_DUP           = 12; // man - extra stuff for 'duped' BIO
    BIO_CTRL_WPENDING      = 13; // opt - number of bytes still to write
    BIO_CTRL_SET_CALLBACK  = 14; // opt - set callback function
    BIO_CTRL_GET_CALLBACK  = 15; // opt - set callback function
    BIO_CTRL_SET_FILENAME  = 30; // BIO_s_file special

    BIO_C_SET_CONNECT                       = 100;
    BIO_C_DO_STATE_MACHINE                  = 101;
    BIO_C_SET_NBIO                          = 102;
    BIO_C_SET_PROXY_PARAM                   = 103;
    BIO_C_SET_FD                            = 104;
    BIO_C_GET_FD                            = 105;
    BIO_C_SET_FILE_PTR                      = 106;
    BIO_C_GET_FILE_PTR                      = 107;
    BIO_C_SET_FILENAME                      = 108;
    BIO_C_SET_SSL                           = 109;
    BIO_C_GET_SSL                           = 110;
    BIO_C_SET_MD                            = 111;
    BIO_C_GET_MD                            = 112;
    BIO_C_GET_CIPHER_STATUS                 = 113;
    BIO_C_SET_BUF_MEM                       = 114;
    BIO_C_GET_BUF_MEM_PTR                   = 115;
    BIO_C_GET_BUFF_NUM_LINES                = 116;
    BIO_C_SET_BUFF_SIZE                     = 117;
    BIO_C_SET_ACCEPT                        = 118;
    BIO_C_SSL_MODE                          = 119;
    BIO_C_GET_MD_CTX                        = 120;
    BIO_C_GET_PROXY_PARAM                   = 121;
    BIO_C_SET_BUFF_READ_DATA                = 122; // data to read first
    BIO_C_GET_CONNECT                       = 123;
    BIO_C_GET_ACCEPT                        = 124;
    BIO_C_SET_SSL_RENEGOTIATE_BYTES         = 125;
    BIO_C_GET_SSL_NUM_RENEGOTIATES          = 126;
    BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT       = 127;
    BIO_C_FILE_SEEK                         = 128;
    BIO_C_GET_CIPHER_CTX                    = 129;
    BIO_C_SET_BUF_MEM_EOF_RETURN            = 130; // return end of input value
    BIO_C_SET_BIND_MODE                     = 131;
    BIO_C_GET_BIND_MODE                     = 132;
    BIO_C_FILE_TELL                         = 133;
    BIO_C_GET_SOCKS                         = 134;
    BIO_C_SET_SOCKS                         = 135;

    BIO_C_SET_WRITE_BUF_SIZE                = 136; // for BIO_s_bio
    BIO_C_GET_WRITE_BUF_SIZE                = 137;
    BIO_C_MAKE_BIO_PAIR                     = 138;
    BIO_C_DESTROY_BIO_PAIR                  = 139;
    BIO_C_GET_WRITE_GUARANTEE               = 140;
    BIO_C_GET_READ_REQUEST                  = 141;
    BIO_C_SHUTDOWN_WR                       = 142;
    BIO_C_NREAD0                            = 143;
    BIO_C_NREAD                             = 144;
    BIO_C_NWRITE0                           = 145;
    BIO_C_NWRITE                            = 146;
    BIO_C_RESET_READ_REQUEST                = 147;

    BIO_NOCLOSE                             = 0;
    BIO_CLOSE                               = 1;

//const
    BIO_FLAGS_READ                          = 1;
    BIO_FLAGS_WRITE                         = 2;
    BIO_FLAGS_IO_SPECIAL                    = 4;
    BIO_FLAGS_RWS                           = (BIO_FLAGS_READ or
                                               BIO_FLAGS_WRITE or
                                               BIO_FLAGS_IO_SPECIAL);
    BIO_FLAGS_SHOULD_RETRY                  = 8;

//const
    // These are passed by the BIO callback //
    BIO_CB_FREE     = $01;
    BIO_CB_READ     = $02;
    BIO_CB_WRITE    = $03;
    BIO_CB_PUTS     = $04;
    BIO_CB_GETS     = $05;
    BIO_CB_CTRL     = $06;

    // The callback is called before and after the underling operation,
    // The BIO_CB_RETURN flag indicates if it is after the call
    BIO_CB_RETURN   = $80;

//const
    X509V3_EXT_DYNAMIC      = $1;
    X509V3_EXT_CTX_DEP      = $2;
    X509V3_EXT_MULTILINE    = $4;

{$IFNDEF OPENSSL_NO_ENGINE}
type
    TUi_method_st = record
        Dummy : array [0..0] of Byte;
    end;
    PUI_METHOD = ^TUi_method_st;

    TUi_st = record
        Dummy : array [0..0] of Byte;
    end;
    PUI = ^TUi_st;

    TUi_string_st = record
        Dummy : array [0..0] of Byte;
    end;
    PUI_STRING = ^TUi_string_st;

    TPinCallBack = function(ui: PUI; uis: PUI_STRING): Integer; cdecl; //AG
{$ENDIF}
{$IFDEF OPENSSL_USE_DELPHI_MM}
type
    TCryptoMallocFunc   = function(Size: size_t): Pointer; cdecl;
    TCryptoReallocFunc  = function(P: Pointer; Size: size_t): Pointer; cdecl;
    TCryptoFreeMemFunc  = procedure(P: Pointer); cdecl;
{$ENDIF}

{ V8.40 callbacks for CRYPTO_get_ex_new_index }
    TCryptoExNewFunc    = function(parent: Pointer; ptr: Pointer; ad: PCRYPTO_EX_DATA; idx: integer; argl: DWord; argp: Pointer): Integer; cdecl;
    TCryptoExFreeFunc   = procedure(parent: Pointer; ptr: Pointer; ad: PCRYPTO_EX_DATA; idx: integer; argl: DWord; argp: Pointer); cdecl;
    TCryptoExDupFunc    = function(dto: PCRYPTO_EX_DATA; dfrom: PCRYPTO_EX_DATA; srcp: Pointer; idx: integer; argl: DWord; argp: Pointer): Integer; cdecl;
{ V8.40 progress callback for DH_generate_paramaters and RSA_generate_key_ex }
    TBNGENCBcallFunc    = function(a: Integer; b: Integer; cb: PBN_GENCB): Integer; cdecl;

{ V8.40 classes for CRYPTO_get_ex_new_index }
const
    CRYPTO_EX_INDEX_SSL             = 0;
    CRYPTO_EX_INDEX_SSL_CTX         = 1;
    CRYPTO_EX_INDEX_SSL_SESSION     = 2;
    CRYPTO_EX_INDEX_X509            = 3;
    CRYPTO_EX_INDEX_X509_STORE      = 4;
    CRYPTO_EX_INDEX_X509_STORE_CTX  = 5;
    CRYPTO_EX_INDEX_DH              = 6;
    CRYPTO_EX_INDEX_DSA             = 7;
    CRYPTO_EX_INDEX_EC_KEY          = 8;
    CRYPTO_EX_INDEX_RSA             = 9;
    CRYPTO_EX_INDEX_ENGINE          = 10;
    CRYPTO_EX_INDEX_UI              = 11;
    CRYPTO_EX_INDEX_BIO             = 12;
    CRYPTO_EX_INDEX_APP             = 13;
    CRYPTO_EX_INDEX__COUNT          = 14;

// 8.35 moved lots of declarations from OverbyteIcsLibeayEx so they are all together

const
    RSA_PKCS1_PADDING                 = 1;
    RSA_SSLV23_PADDING                = 2;
    RSA_NO_PADDING                    = 3;
    RSA_PKCS1_OAEP_PADDING            = 4;

    RSA_PKCS1_PADDING_SIZE            = 11;
    RSA_PKCS1_OAEP_PADDING_SIZE       = 41;
    PKCS5_SALT_LEN                    =  8;

const
//    f_SSLeay :                                 function: Longword; cdecl = nil; //AG   V8.36 renamed in 1.1.0
//    f_SSLeay_version :                         function(t: Integer): PAnsiChar; cdecl = nil; //AG   V8.36 renamed in 1.1.0

    f_ASN1_INTEGER_get :                       function(Asn1_Int : PASN1_INTEGER): Integer; cdecl = nil;
    f_ASN1_INTEGER_get_int64 :                 function(var pr: int64 ; Asn1_Int : PASN1_INTEGER): Integer; cdecl = nil;              { V8.40 1.1.0 }
    f_ASN1_INTEGER_get_uint64 :                function(var pr: uint64 ; Asn1_Int : PASN1_INTEGER): Integer; cdecl = nil;             { V8.40 1.1.0 }
    f_ASN1_INTEGER_set :                       function(a: PASN1_INTEGER; v: LongInt) : Integer; cdecl = nil;//AG
    f_ASN1_INTEGER_set_int64 :                 function(a: PASN1_INTEGER; r: Int64) : Integer; cdecl = nil;//AG     { V8.40 1.1.0 }
    f_ASN1_INTEGER_set_uint64 :                function(a: PASN1_INTEGER; r: UInt64) : Integer; cdecl = nil;//AG    { V8.40 1.1.0 }
    f_ASN1_STRING_free :                       procedure(a: PASN1_STRING); cdecl = nil;//AG;
    f_ASN1_STRING_get0_data :                  function(str: PASN1_STRING): PAnsiChar; cdecl = nil;                       { V8.40 aka ASN1_STRING_data until 1.1.0 }
    f_ASN1_STRING_length :                     function(str: PASN1_STRING): Integer; cdecl = nil;                         { V8.40 }
    f_ASN1_STRING_length_set :                 procedure(str: PASN1_STRING; n: Integer); cdecl = nil;                     { V8.40 }
    f_ASN1_STRING_new :                        function: PASN1_STRING; cdecl = nil;                              { V8.40 }
    f_ASN1_STRING_print :                      function(B: PBIO; v: PASN1_STRING): integer; cdecl = nil;//AG;
    f_ASN1_STRING_type :                       function(str: PASN1_STRING): Integer; cdecl = nil;                         { V8.40 }
    f_ASN1_STRING_set0 :                       procedure(str: PASN1_STRING; data: PAnsiChar; len: Integer); cdecl = nil;  { V8.40 }
    f_ASN1_STRING_to_UTF8 :                    function(POut: PPAnsiChar; PIn: PASN1_STRING) : Integer; cdecl = nil;//AG
    f_ASN1_item_d2i :                          function(Val: PPASN1_VALUE; _In: PPAnsiChar; Len: Longword; const It: PASN1_ITEM): PASN1_VALUE; cdecl = nil;//AG;
    f_ASN1_item_free :                         procedure(Val: PASN1_VALUE; const It: PASN1_ITEM); cdecl = nil; //AG
    f_BIO_ctrl :                               function(bp: PBIO; Cmd: Integer; LArg: LongInt; PArg: Pointer): LongInt; cdecl = nil;
    f_BIO_ctrl_get_read_request :              function(b: PBIO): size_t; cdecl = nil;
    f_BIO_ctrl_get_write_guarantee :           function(b: PBIO): size_t; cdecl = nil;
    f_BIO_ctrl_pending :                       function(b: PBIO): size_t; cdecl = nil;
    f_BIO_free :                               function(B: PBIO): Integer; cdecl = nil;
    f_BIO_get_retry_BIO :                      function(B: PBIO; Reason : PInteger): PBIO; cdecl = nil;
    f_BIO_get_retry_reason :                   function(B: PBIO): Integer; cdecl = nil;
    f_BIO_gets :                               function(B: PBIO; Buf: PAnsiChar; Size: Integer): Integer; cdecl = nil;
    f_BIO_new :                                function(BioMethods: PBIO_METHOD): PBIO; cdecl = nil;
    f_BIO_new_bio_pair :                       function(Bio1: PPBIO; WriteBuf1: size_t; Bio2: PPBIO; WriteBuf2: size_t): Integer; cdecl = nil;
    f_BIO_new_fd :                             function(Fd: Integer; CloseFlag: Integer): PBIO; cdecl = nil;
    f_BIO_new_file :                           function(FileName: PAnsiChar; Mode: PAnsiChar): PBIO; cdecl = nil;
    f_BIO_new_mem_buf :                        function(Buf : Pointer; Len : Integer): PBIO; cdecl = nil;
    f_BIO_new_socket :                         function(Sock: Integer; CloseFlag: Integer): PBIO; cdecl = nil;
    f_BIO_nread :                              function(B: PBIO; PBuf: PPAnsiChar; Num: Integer): Integer; cdecl = nil;
    f_BIO_nread0 :                             function(B: PBIO; PBuf: PPAnsiChar): Integer; cdecl = nil;
    f_BIO_nwrite :                             function(B: PBIO; PBuf: PPAnsiChar; Num: Integer): Integer; cdecl = nil;
    f_BIO_nwrite0 :                            function(B: PBIO; PBuf: PPAnsiChar): Integer; cdecl = nil;
    f_BIO_push :                               function(B: PBIO; B_Append: PBIO): PBIO; cdecl = nil;
    f_BIO_puts :                               function(B: PBIO; Buf: PAnsiChar): Integer; cdecl = nil;
    f_BIO_read :                               function(B: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl = nil;
    f_BIO_s_mem :                              function : PBIO_METHOD; cdecl = nil;
    f_BIO_write :                              function(B: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl = nil;
    f_BN_GENCB_free :                          procedure(cb: PBN_GENCB); cdecl = nil;                { V8.40 }
    f_BN_GENCB_get_arg :                       function(cb: PBN_GENCB): Pointer; cdecl = nil;        { V8.40 }
    f_BN_GENCB_new :                           function : PBN_GENCB; cdecl = nil;                    { V8.40 }
    f_BN_GENCB_set :                           procedure(cb: PBN_GENCB; callback: TBNGENCBcallFunc; arg: pointer); cdecl = nil;  { V8.40 }
    f_BN_free:                                 procedure (a: PBIGNUM); cdecl = nil;
    f_BN_new:                                  function : PBIGNUM; cdecl = nil;                 { V8.11 }
    f_BN_set_word:                             function (a: PBIGNUM; w: BN_ULONG): Integer; cdecl = nil;
    f_CONF_modules_unload :                    procedure(all: Integer); cdecl = nil;//AG;
    f_CRYPTO_THREADID_set_callback :           function(CB : TCryptoThreadIDCallback) : Integer; cdecl = nil;
    f_CRYPTO_THREADID_set_numeric :            procedure(id : PCRYPTO_THREADID; val: LongWord); cdecl = nil;
    f_CRYPTO_THREADID_set_pointer :            procedure(id : PCRYPTO_THREADID; ptr: Pointer); cdecl = nil;
    f_CRYPTO_add_lock :                        procedure(IntPtr: PInteger; amount: Integer; type_: Integer; const file_ : PAnsiChar; line: Integer); cdecl = nil;
    f_CRYPTO_cleanup_all_ex_data :             procedure; cdecl = nil;
    f_CRYPTO_free :                            procedure(P: Pointer); cdecl = nil;//AG
    f_CRYPTO_free_ex_data :                    procedure(class_index: integer; obj: Pointer; r: PCRYPTO_EX_DATA); cdecl = Nil;             { V8.40 }
    f_CRYPTO_free_ex_index :                   procedure(class_index: integer; idx: integer); cdecl = Nil;                                 { V8.40 }
    f_CRYPTO_get_ex_data :                     function(r: PCRYPTO_EX_DATA; idx: integer): integer; cdecl = Nil;                           { V8.40 }
    f_CRYPTO_get_ex_new_index :                function(class_index: integer; argl: DWord; argp: Pointer; new_func: TCryptoExNewFunc; dup_func: TCryptoExDupFunc; free_func: TCryptoExFreeFunc): integer; cdecl = Nil; { V8.40 }
    f_CRYPTO_lock :                            procedure(mode, n: Longint; file_: PAnsiChar; line: Longint); cdecl = nil; //AG
    f_CRYPTO_new_ex_data :                     function(class_index: integer; obj: pointer; ad: PCRYPTO_EX_DATA): integer; cdecl = Nil;    { V8.40 }
    f_CRYPTO_num_locks :                       function: Integer; cdecl = nil;
    f_CRYPTO_set_dynlock_create_callback :     procedure(CB : TDynLockCreateCallBack); cdecl = nil;
    f_CRYPTO_set_dynlock_destroy_callback :    procedure(CB : TDynLockDestroyCallBack); cdecl = nil;
    f_CRYPTO_set_dynlock_lock_callback :       procedure(CB : TDynLockLockCallBack); cdecl = nil;
    f_CRYPTO_set_ex_data :                     function(r: PCRYPTO_EX_DATA; idx: integer; arg: Pointer): integer; cdecl = Nil;             { V8.40 }
    f_CRYPTO_set_id_callback :                 procedure(CB : TStatLockIDCallback); cdecl = nil;
    f_CRYPTO_set_locking_callback :            procedure(CB : TStatLockLockCallback); cdecl = nil;
    f_DH_free :                                procedure(dh: PDH) cdecl = nil;  { V8.07 }
    f_DH_check :                               function(dh: PDH; var codes: Integer): Integer; cdecl = Nil;       { V8.40 }
    f_DH_generate_parameters_ex :              function(dh: PDH; prime_len: Integer; generator: Integer; cb: PBN_GENCB): Integer; cdecl = Nil;  { V8.40 }
    f_DH_new :                                 function: PDH;  cdecl = nil;                                       { V8.40 }
    f_DH_size :                                function(Dh: PDH): Integer; cdecl = nil;   //Angus
    f_DSA_free :                               procedure(DSA: PDSA); cdecl = nil;   //Angus
    f_DSA_print :                              function(B: PBIO; Dsa: PDSA; Offset: Integer): Integer; cdecl = nil;//AG;
    f_DSA_size :                               function(Dsa: PDSA): Integer; cdecl = nil; //Angus
    f_EC_GROUP_check :                         function(group: PEC_GROUP; ctx: PBN_CTX): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_check_discriminant :            function(group: PEC_GROUP; ctx: PBN_CTX): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_clear_free :                    procedure(group: PEC_GROUP); cdecl = nil;                           { V8.40 }
    f_EC_GROUP_cmp :                           function(agroup, bgroup: PEC_GROUP; ctx: PBN_CTX): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_copy :                          function(dst: PEC_GROUP; src: PEC_GROUP): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_dup :                           function(src: PEC_GROUP): PEC_GROUP; cdecl = Nil;                   { V8.40 }
    f_EC_GROUP_free :                          procedure(group: PEC_GROUP); cdecl = nil;                           { V8.40 }
    f_EC_GROUP_get0_cofactor :                 function(group: PEC_GROUP): PBIGNUM; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get0_generator :                function(group: PEC_GROUP): PEC_POINT; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get0_order :                    function(group: PEC_GROUP): PBIGNUM; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get0_seed :                     function(group: PEC_GROUP): PAnsiChar; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_asn1_flag :                 function(group: PEC_GROUP): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_basis_type :                function(group: PEC_GROUP): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_cofactor :                  function(group: PEC_GROUP; cofactor: PBIGNUM; ctx: PBN_CTX): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_curve_gf2m :                function(group: PEC_GROUP; p, a, b: PBIGNUM; ctx: PBN_CTX): integer; cdecl = Nil;    { V8.40 }
    f_EC_GROUP_get_curve_gfp :                 function(group: PEC_GROUP; p, a, b: PBIGNUM; ctx: PBN_CTX): integer; cdecl = Nil;    { V8.40 }
    f_EC_GROUP_get_curve_name :                function(group: PEC_GROUP): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_degree :                    function(group: PEC_GROUP): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_ecparameters :              function(group: PEC_GROUP; params: PEC_PARAMETERS): PEC_PARAMETERS; cdecl = nil;     { V8.40 }
    f_EC_GROUP_get_ecpkparameters :            function(group: PEC_GROUP; params: PEC_PKPARAMETERS): PEC_PKPARAMETERS; cdecl = nil; { V8.40 }
    f_EC_GROUP_get_mont_data :                 function(group: PEC_GROUP): PBN_MONT_CTX; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_order :                     function(group: PEC_GROUP; order: PBIGNUM; ctx: PBN_CTX): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_pentanomial_basis :         function(group: PEC_GROUP; var k1, k2, k3: integer): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_point_conversion_form :     function(group: PEC_GROUP): integer ; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_seed_len :                  function(group: PEC_GROUP): SIZE_T; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_get_trinomial_basis :           function(group: PEC_GROUP; var k: Integer): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_method_of :                     function(group: PEC_GROUP): PEC_METHOD; cdecl = Nil;                { V8.40 }
    f_EC_GROUP_new :                           function(method: PEC_METHOD): PEC_GROUP; cdecl = Nil;               { V8.40 }
    f_EC_GROUP_new_by_curve_name :             function(nid: integer): PEC_GROUP; cdecl = Nil;                     { V8.40 }
    f_EC_GROUP_new_curve_GF2m :                function(p, a, b: PBIGNUM; ctx: PBN_CTX): PEC_GROUP; cdecl = Nil;   { V8.40 }
    f_EC_GROUP_new_curve_GFp :                 function(p, a, b: PBIGNUM; ctx: PBN_CTX): PEC_GROUP; cdecl = Nil;   { V8.40 }
    f_EC_GROUP_new_from_ecparameters :         function (params: PEC_PARAMETERS): PEC_GROUP; cdecl = Nil;          { V8.40 }
    f_EC_GROUP_new_from_ecpkparameters :       function(params: PEC_PKPARAMETERS): PEC_GROUP; cdecl = Nil;         { V8.40 }
    f_EC_GROUP_order_bits :                    function(group: PEC_GROUP): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_set_asn1_flag :                 procedure(group: PEC_GROUP; flag: integer); cdecl = Nil;     { V8.40 }
    f_EC_GROUP_set_curve_GF2m :                function(group: PEC_GROUP; p, a, b: PBIGNUM; ctx: PBN_CTX): integer; cdecl = Nil;    { V8.40 }
    f_EC_GROUP_set_curve_GFp :                 function(group: PEC_GROUP; p, a, b: PBIGNUM; ctx: PBN_CTX): integer; cdecl = Nil;    { V8.40 }
    f_EC_GROUP_set_curve_name :                procedure(group: PEC_GROUP; nid: integer); cdecl = Nil;     { V8.40 }
    f_EC_GROUP_set_generator :                 function(group: PEC_GROUP; generator: PEC_POINT; order, cofactor: PBIGNUM): integer; cdecl = Nil;     { V8.40 }
    f_EC_GROUP_set_point_conversion_form :     procedure(group: PEC_GROUP; t_form: integer); cdecl = Nil;     { V8.40 }
    f_EC_GROUP_set_seed :                      function(group: PEC_GROUP;  seed: PAnsiChar; len: size_t): size_t; cdecl = Nil;     { V8.40 }
    f_EC_KEY_can_sign :                        function(key: PEC_KEY): Integer; cdecl = Nil;                 { V8.40 }
    f_EC_KEY_check_key :                       function(key: PEC_KEY): Integer ; cdecl = Nil;                { V8.40 }
    f_EC_KEY_clear_flags :                     procedure(key: PEC_KEY; flags: Integer) ; cdecl = Nil;         { V8.40 }
    f_EC_KEY_copy :                            function(dst: PEC_KEY; src: PEC_KEY): PEC_KEY ; cdecl = Nil;  { V8.40 }
    f_EC_KEY_dup :                             function(src: PEC_KEY): PEC_KEY; cdecl = Nil;                 { V8.40 }
    f_EC_KEY_free :                            procedure (key: PEC_KEY); cdecl = nil;           { V8.07 }
    f_EC_KEY_generate_key :                    function(key: PEC_KEY): Integer ; cdecl = Nil;                { V8.40 }
    f_EC_KEY_get0_group :                      function(key: PEC_KEY): PEC_GROUP; cdecl = Nil;               { V8.40 }
    f_EC_KEY_get0_private_key :                function(key: PEC_KEY): PBIGNUM ; cdecl = Nil;                { V8.40 }
    f_EC_KEY_get0_public_key :                 function(key: PEC_KEY): PEC_POINT; cdecl = Nil;               { V8.40 }
    f_EC_KEY_get_conv_form :                   function(key: PEC_KEY): Integer; cdecl = Nil;                 { V8.40 }
    f_EC_KEY_get_enc_flags :                   function(key: PEC_KEY): DWord; cdecl = Nil;                   { V8.40 }
    f_EC_KEY_get_ex_data :                     function(key: PEC_KEY; idx: Integer): Pointer; cdecl = Nil;   { V8.40 }
    f_EC_KEY_get_flags :                       function(const key: PEC_KEY): Integer; cdecl = Nil;           { V8.40 }
    f_EC_KEY_key2buf :                         function(key: PEC_KEY; cform: integer; pbuf: PAnsiChar; ctx: PBN_CTX): integer; cdecl = Nil; { V8.40 }
    f_EC_KEY_new :                             function: PEC_KEY; cdecl = Nil;                              { V8.40 }
    f_EC_KEY_new_by_curve_name :               function (nid: integer): PEC_KEY; cdecl = nil;   { V8.07 }
    f_EC_KEY_oct2key :                         function(key: PEC_KEY; buf: PAnsiChar; len: integer; ctx: PBN_CTX): Integer; cdecl = Nil;    { V8.40 }
    f_EC_KEY_oct2priv :                        function(key: PEC_KEY; buf: PAnsiChar; len: integer): Integer; cdecl = Nil;    { V8.40 }
    f_EC_KEY_precompute_mult :                 function(key: PEC_KEY; ctx: PBN_CTX): Integer; cdecl = Nil;   { V8.40 }
    f_EC_KEY_print :                           function(B: PBIO; const EC: PEC_KEY; Offset: Integer): Integer; cdecl = nil;//AG;
    f_EC_KEY_priv2buf :                        function(key: PEC_KEY; pbuf: PPAnsiChar): integer; cdecl = Nil;                { V8.40 }
    f_EC_KEY_priv2oct :                        function(key: PEC_KEY; buf: PAnsiChar; len: integer): Integer; cdecl = Nil;    { V8.40 }
    f_EC_KEY_set_asn1_flag :                   procedure(key: PEC_KEY; asn1_flag: Integer); cdecl = Nil;     { V8.40 }
    f_EC_KEY_set_conv_form :                   procedure(key: PEC_KEY; cform: integer); cdecl = Nil;          { V8.40 }
    f_EC_KEY_set_enc_flags :                   procedure(key: PEC_KEY; flags: DWord); cdecl = Nil;           { V8.40 }
    f_EC_KEY_set_flags :                       procedure(key: PEC_KEY; flags: Integer) ; cdecl = Nil;         { V8.40 }
    f_EC_KEY_set_group :                       function(key: PEC_KEY; group: PEC_GROUP): Integer; cdecl = Nil; { V8.40 }
    f_EC_KEY_set_private_key :                 function(key: PEC_KEY; prv: PBIGNUM): Integer; cdecl = Nil;   { V8.40 }
    f_EC_KEY_set_public_key :                  function(key: PEC_KEY; pub: PEC_POINT): Integer; cdecl = Nil; { V8.40 }
    f_EC_KEY_set_public_key_affine_coordinates: function(key: PEC_KEY; x: PBIGNUM; y: PBIGNUM): Integer; cdecl = Nil;     { V8.40 }
    f_EC_KEY_up_ref :                          function(key: PEC_KEY): Integer; cdecl = Nil;                 { V8.40 }
    f_EC_METHOD_get_field_type :               function(method: PEC_METHOD): integer; cdecl = Nil;     { V8.40 }
    f_EC_curve_nid2nist:                       function(nid: Integer): PAnsiChar; cdecl = Nil;                     { V8.40 }
    f_EC_curve_nist2nid:                       function(ename: PAnsiChar): Integer; cdecl = Nil;                   { V8.40 }
    f_EC_get_builtin_curves :                  function(items: TEC_BUILTIN_CURVES; nitems: size_t): size_t; cdecl = Nil;           { V8.40 }
    f_ECDSA_SIG_free :                         procedure(sig: PECDSA_SIG); cdecl = Nil;          { V8.40 }
    f_ECDSA_SIG_get0 :                         procedure(sig: PECDSA_SIG; r: PBIGNUM; s: PBIGNUM); cdecl = Nil;                       { V8.40 }
    f_ECDSA_SIG_new :                          function: PECDSA_SIG; cdecl = Nil;                { V8.40 }
    f_ECDSA_SIG_set0 :                         function(sig: PECDSA_SIG; r: PBIGNUM; s: PBIGNUM): PECDSA_SIG; cdecl = Nil;            { V8.40 }
    f_ECDSA_do_sign :                          function(dgst: PAnsiChar; dgst_len: integer; eckey: PEC_KEY): PECDSA_SIG; cdecl = Nil; { V8.40 }
    f_ECDSA_do_sign_ex :                       function(dgst: PAnsiChar; dgst_len: integer; kinv: PBIGNUM; rp: PBIGNUM; eckey: PEC_KEY): PECDSA_SIG; cdecl = Nil; { V8.40 }
    f_ECDSA_do_verify :                        function(dgst: PAnsiChar; dgst_len: integer; sig: PECDSA_SIG; eckey: PEC_KEY): integer; cdecl = Nil;               { V8.40 }
    f_ECDSA_sign :                             function(xtype: integer; dgst: PAnsiChar; dgst_len: integer; sig: PAnsiChar; signlen: integer; eckey: PEC_KEY): integer; cdecl = Nil;               { V8.40 }
    f_ECDSA_sign_ex :                          function(xtype: integer; dgst: PAnsiChar; dgst_len: integer; sig: PAnsiChar; signlen: integer; kinv: PBIGNUM; rp: PBIGNUM; eckey: PEC_KEY): integer; cdecl = Nil;  { V8.40 }
    f_ECDSA_sign_setup :                       function(eckey: PEC_KEY; ctc: PBN_CTX; kinv: PBIGNUM; rp: PBIGNUM): integer; cdecl = Nil;                           { V8.40 }
    f_ECDSA_size :                             function(eckey: PEC_KEY): integer; cdecl = Nil;    { V8.40 }
    f_ECDSA_verify :                           function(xtype: integer; dgst: PAnsiChar; dgst_len: integer; sig: PAnsiChar; signlen: integer; eckey: PEC_KEY): integer; cdecl = Nil;               { V8.40 }
    f_ERR_clear_error :                        procedure; cdecl = nil; //empties the current thread's error queue
    f_ERR_error_string :                       function(Err: Cardinal; Buf: PAnsiChar): PAnsiChar; cdecl = nil;
    f_ERR_error_string_n :                     procedure(Err: Cardinal; Buf: PAnsiChar; Len: size_t); cdecl = nil;
    f_ERR_free_strings :                       procedure; cdecl = nil; //"Brutal" (thread-unsafe) Application-global cleanup functions
    f_ERR_func_error_string:                   function (e: Cardinal): PAnsiChar; cdecl = nil;
    f_ERR_get_error :                          function: Cardinal; cdecl = nil;
    f_ERR_get_error_line_data :                function(const FileName: PPAnsiChar; Line: PInteger; const Data: PPAnsiChar; Flags: PInteger): Cardinal; cdecl = nil;
    f_ERR_lib_error_string:                    function (e: Cardinal): PAnsiChar; cdecl = nil;   { V8.11 }
    f_ERR_load_crypto_strings:                 procedure; cdecl = nil;
    f_ERR_peek_error :                         function : Cardinal; cdecl = nil;
    f_ERR_peek_last_error :                    function : Cardinal; cdecl = nil;
    f_ERR_reason_error_string:                 function (e: Cardinal): PAnsiChar; cdecl = nil;
    f_ERR_remove_state :                       procedure(ThreadID: Longword); cdecl = nil;
    f_ERR_remove_thread_state :                procedure(tid: PCRYPTO_THREADID); cdecl = nil;
    f_EVP_BytesToKey :                         function(const type_: PEVP_CIPHER; const md: PEVP_MD; const salt: PAnsiChar; const data: PAnsiChar; datalen, count : Integer; key, iv: PAnsiChar): Integer; cdecl = nil;
    f_EVP_CIPHER_CTX_cleanup :                 function(ctx: PEVP_CIPHER_CTX): Integer; cdecl = nil;   { V8.27 gone with OpenSSL 1.1.0 }
    f_EVP_CIPHER_CTX_ctrl :                    function(ctx: PEVP_CIPHER_CTX; ctype: Integer; arg: Integer; ptr: Pointer): Integer; cdecl = nil;   { V8.40 }
    f_EVP_CIPHER_CTX_free :                    procedure(ctx: PEVP_CIPHER_CTX); cdecl = nil;
    f_EVP_CIPHER_CTX_init :                    procedure(ctx: PEVP_CIPHER_CTX); cdecl = nil;  { V8.27 gone with OpenSSL 1.1.0 }
    f_EVP_CIPHER_CTX_new :                     function: PEVP_CIPHER_CTX; cdecl = nil;
    f_EVP_CIPHER_CTX_reset :                   procedure(ctx: PEVP_CIPHER_CTX); cdecl = nil;  { V8.27 new with OpenSSL 1.1.0 }
    f_EVP_CIPHER_CTX_set_padding :             function(ctx: PEVP_CIPHER_CTX; pad: Integer): Integer; cdecl = nil;   { V8.40 }
    f_EVP_CIPHER_CTX_set_key_length :          function(ctx: PEVP_CIPHER_CTX; keyl: Integer): LongBool; cdecl = nil;
    f_EVP_CipherFinal_ex :                     function(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer): LongBool; cdecl = nil;
    f_EVP_CipherInit_ex :                      function(ctx: PEVP_CIPHER_CTX; const cipher: PEVP_CIPHER; impl: PEngine; key, iv: PAnsiChar; enc: Integer): LongBool; cdecl = nil;
    f_EVP_CipherUpdate :                       function(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer; const in_: PAnsiChar; inl: Integer): LongBool; cdecl = nil;
    f_EVP_DecryptFinal_ex :                    function(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer): LongBool; cdecl = nil;   { V8.40 }
    f_EVP_DecryptInit_ex :                     function(ctx: PEVP_CIPHER_CTX; const cipher: PEVP_CIPHER; impl: PEngine; const key: PAnsiChar; const iv: PAnsiChar): LongBool; cdecl = nil;
    f_EVP_DecryptUpdate :                      function(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer; const in_: PAnsiChar; inl: Integer): LongBool; cdecl = nil;
    f_EVP_Digest :                             function(edata: Pointer; count: SIZE_T; var md: Byte; var size: Word; etype: PEVP_MD; impl: PEngine): Integer; cdecl = Nil;    { V8.40 }
    f_EVP_DigestFinal_ex :                     function(ctx: PEVP_MD_CTX; md: Byte; var s: Word): Integer; cdecl = Nil;               { V8.40 }
    f_EVP_DigestInit_ex :                      function(ctx: PEVP_MD_CTX; etype: PEVP_MD; impl: PEngine): Integer; cdecl = Nil;       { V8.40 }
    f_EVP_DigestUpdate :                       function(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer; cdecl = Nil;             { V8.40 }
    f_EVP_DigestFinal :                        function(ctx: PEVP_MD_CTX; var md: Byte; var s: Word): Integer; cdecl = Nil;           { V8.40 }
    f_EVP_DigestInit :                         function(ctx: PEVP_MD_CTX; etype: PEVP_MD): Integer; cdecl = Nil;                      { V8.40 }
    f_EVP_DigestSignInit :                     function(ctx: PEVP_MD_CTX; pctx: PEVP_PKEY_CTX; etype: PEVP_MD; impl: PEngine; pkey: PEVP_PKEY): Integer; cdecl = Nil;   { V8.40 }
    f_EVP_DigestSignFinal :                    function(ctx: PEVP_MD_CTX; var sigret: Byte; var siglen: SIZE_T): Integer; cdecl = Nil;           { V8.40 }
    f_EVP_DigestVerifyInit :                   function(ctx: PEVP_MD_CTX; pctx: PEVP_PKEY_CTX; etype: PEVP_MD; impl: PEngine): Integer; cdecl = Nil;   { V8.40 }
    f_EVP_DigestVerifyFinal:                   function(ctx: PEVP_MD_CTX; sig: PByte; siglen: SIZE_T): Integer; cdecl = Nil;                     { V8.40 }
    f_EVP_EncryptFinal_ex :                    function(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer): LongBool; cdecl = nil;   { V8.40 }
    f_EVP_EncryptInit_ex :                     function(ctx: PEVP_CIPHER_CTX; const cipher: PEVP_CIPHER; impl: PEngine; const key: PAnsiChar; const iv: PAnsiChar): LongBool; cdecl = nil;
    f_EVP_EncryptUpdate :                      function(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer; const in_: PAnsiChar; inl: Integer): LongBool; cdecl = nil;
    f_EVP_MD_CTX_new :                         function: PEVP_MD_CTX; cdecl = Nil;                                     { V8.40 }
    f_EVP_MD_CTX_reset :                       function(ctx: PEVP_MD_CTX): Integer; cdecl = Nil;                       { V8.40 }
    f_EVP_MD_CTX_free :                        procedure(ctx: PEVP_MD_CTX); cdecl = Nil;                               { V8.40 }
    f_EVP_MD_CTX_copy :                        function(outp: PEVP_MD_CTX; inp: PEVP_MD_CTX): Integer; cdecl = Nil;    { V8.40 }
    f_EVP_MD_CTX_copy_ex :                     function(outp: PEVP_MD_CTX; inp: PEVP_MD_CTX): Integer; cdecl = Nil;    { V8.40 }
    f_EVP_MD_type :                            function(md: PEVP_MD): Integer; cdecl = Nil;                       { V8.40 }
    f_EVP_MD_pkey_type :                       function(md: PEVP_MD): Integer; cdecl = Nil;                       { V8.40 }
    f_EVP_MD_size :                            function(md: PEVP_MD): Integer; cdecl = Nil;                       { V8.40 }
    f_EVP_MD_block_size :                      function(md: PEVP_MD): Integer; cdecl = Nil;                       { V8.40 }
    f_EVP_MD_flags :                           function(md: PEVP_MD): LongInt; cdecl = Nil;                       { V8.40 }
    f_EVP_MD_CTX_md :                          function(ctx: PEVP_MD_CTX): PEVP_MD; cdecl = Nil;                  { V8.40 }
    f_EVP_PKEY_assign :                        function(PKey: PEVP_PKEY; Type_: Integer; Key: PAnsiChar): Integer; cdecl = nil;//AG
    f_EVP_PKEY_base_id :                       function(Pkey: PEVP_PKEY): Integer; cdecl = nil;                   { V8.40 }
    f_EVP_PKEY_bits :                          function(Pkey: PEVP_PKEY): Integer; cdecl = nil;//AG
    f_EVP_PKEY_free :                          procedure(PKey: PEVP_PKEY); cdecl = nil;//AG
    f_EVP_PKEY_get0 :                          function(PKey: PEVP_PKEY): Pointer; cdecl = nil;//AG
    f_EVP_PKEY_get1_DH :                       function (pkey: PEVP_PKEY): PDH; cdecl = nil; //Angus
    f_EVP_PKEY_get1_DSA :                      function (pkey: PEVP_PKEY): PDSA; cdecl = nil; //Angus
    f_EVP_PKEY_get1_EC_KEY :                   function (pkey: PEVP_PKEY): PEC_KEY; cdecl = nil; //Angus
    f_EVP_PKEY_get1_RSA :                      function (pkey: PEVP_PKEY): PRSA; cdecl = nil; //Angus
    f_EVP_PKEY_new :                           function: PEVP_PKEY; cdecl = nil;//AG
    f_EVP_PKEY_print_params :                  function (outbio: PBIO; pkey: PEVP_PKEY; indent: Integer; pctx: PASN1_PCTX): Integer; cdecl = Nil;        { V8.40 }
    f_EVP_PKEY_print_private :                 function (outbio: PBIO; pkey: PEVP_PKEY; indent: Integer; pctx: PASN1_PCTX): Integer; cdecl = Nil;        { V8.40 }
    f_EVP_PKEY_print_public :                  function (outbio: PBIO; pkey: PEVP_PKEY; indent: Integer; pctx: PASN1_PCTX): Integer; cdecl = Nil;        { V8.40 }
    f_EVP_PKEY_size :                          function(Pkey: PEVP_PKEY): Integer; cdecl = nil;//AG
    f_EVP_SignFinal :                          function(ctx: PEVP_MD_CTX; var md: Byte; var s: Word; pkey: PEVP_PKEY): Integer; cdecl = Nil;        { V8.40 }
    f_EVP_VerifyFinal :                        function(ctx: PEVP_MD_CTX; sigbuf: PByte; siglen: Word; pkey: PEVP_PKEY): Integer; cdecl = Nil;      { V8.40 }
    f_EVP_aes_128_cbc :                        function: PEVP_CIPHER; cdecl = nil;
    f_EVP_aes_128_cfb128 :                     function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_128_ecb :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_128_ofb :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_128_gcm :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_128_ocb :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_128_ccm :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_192_cbc :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_192_cfb128 :                     function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_192_ecb :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_192_ofb :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_192_gcm :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_192_ocb :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_192_ccm :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_256_cbc :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_256_cfb128 :                     function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_256_ecb :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_256_ofb :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_256_gcm :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_256_ocb :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_aes_256_ccm :                        function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_bf_cbc :                             function: PEVP_CIPHER; cdecl = nil;
    f_EVP_bf_cfb64 :                           function: PEVP_CIPHER; cdecl = nil;
    f_EVP_bf_ecb :                             function: PEVP_CIPHER; cdecl = nil;
    f_EVP_bf_ofb :                             function: PEVP_CIPHER; cdecl = nil;
    f_EVP_cleanup :                            procedure; cdecl = nil;
    f_EVP_chacha20 :                           function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_chacha20_poly1305 :                  function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_des_ede3_cbc :                       function: PEVP_CIPHER; cdecl = nil;//AG
    f_EVP_des_ede3_cfb64 :                     function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_des_ede3_ecb :                       function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_des_ede3_ofb :                       function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_enc_null :                           function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_get_cipherbyname :                   function(name: PAnsiChar): PEVP_CIPHER; cdecl = nil;//AG
    f_EVP_get_digestbyname :                   function(name: PAnsiChar): PEVP_MD; cdecl = Nil;                   { V8.40 }
    f_EVP_idea_cbc :                           function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_idea_cfb64 :                         function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_idea_ecb :                           function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_idea_ofb :                           function: PEVP_CIPHER; cdecl = nil; { V8.40 }
    f_EVP_md5 :                                function: PEVP_MD; cdecl = nil;//AG
    f_EVP_mdc2 :                               function: PEVP_MD; cdecl = nil;     { V8.40 }
    f_EVP_sha1 :                               function: PEVP_MD; cdecl = nil;//AG
    f_EVP_sha224 :                             function: PEVP_MD; cdecl = nil;     { V8.40 }
    f_EVP_sha256 :                             function: PEVP_MD; cdecl = nil;//AG
    f_EVP_sha384 :                             function: PEVP_MD; cdecl = nil;     { V8.40 }
    f_EVP_sha512 :                             function: PEVP_MD; cdecl = nil;     { V8.40 }
    f_EVP_ripemd160 :                          function: PEVP_MD; cdecl = nil;     { V8.40 }
    f_GENERAL_NAME_free :                      procedure(a: PGENERAL_NAME); cdecl = nil;                                { V8.40 }
    f_GENERAL_NAME_get0_value :                function(gen: PGENERAL_NAME; var atype: Integer): Pointer; cdecl = nil;  { V8.40 }
    f_GENERAL_NAME_new :                       function: PGENERAL_NAME; cdecl = nil;                                    { V8.40 }
    f_GENERAL_NAME_set0_value :                procedure(gen: PGENERAL_NAME; atype: Integer; avalue: Pointer); cdecl = nil;  { V8.40 }
    f_HMAC :                                   function(evp: pEVP_MD; key: PByte; key_len: integer; data: PByte; data_len: integer; md: PByte; var md_len: integer): PByte; cdecl = nil;    { V8.03 }
    f_OBJ_nid2ln :                             function(N: Integer): PAnsiChar; cdecl = nil;
    f_OBJ_nid2obj :                            function(N: Integer): PASN1_OBJECT; cdecl = nil;    { V8.40 }
    f_OBJ_nid2sn :                             function(N: Integer): PAnsiChar; cdecl = nil;
    f_OBJ_obj2nid :                            function(O: PASN1_OBJECT): Integer; cdecl = nil;
    f_OPENSSL_sk_delete :                      function(Stack: PSTACK; Item: Integer): PAnsiChar; cdecl = nil;//AG;
    f_OPENSSL_sk_dup :                         function(Stack: PSTACK): PSTACK; cdecl = nil;//AG;
    f_OPENSSL_sk_find :                        function(Stack: PSTACK; Data: PAnsiChar): Integer; cdecl = nil;//AG;
    f_OPENSSL_sk_free :                        procedure(Stack: PSTACK); cdecl = nil;//AG;
    f_OPENSSL_sk_insert :                      function(Stack: PSTACK; Data: PAnsiChar; Index: Integer): Integer; cdecl = nil;//AG;
    f_OPENSSL_sk_new_null:                     function: PSTACK; cdecl = nil;//AG;
    f_OPENSSL_sk_num :                         function(Stack: PSTACK): Integer; cdecl = nil;
    f_OPENSSL_sk_pop :                         function(Stack: PSTACK): PAnsiChar; cdecl = nil;//AG;
    f_OPENSSL_sk_pop_free :                    procedure(Stack: PSTACK; PFreeProc: Pointer); cdecl = nil;//AG;
    f_OPENSSL_sk_push :                        function(Stack: PSTACK; Data: PAnsiChar): Integer; cdecl = nil;//AG;
    f_OPENSSL_sk_set :                         function(Stack: PSTACK; Index: Integer; value: PAnsiChar): PAnsiChar; cdecl = nil;//AG;
    f_OPENSSL_sk_value :                       function(Stack: PSTACK; Item: Integer): PAnsiChar; cdecl = nil;
    f_OpenSSL_version :                        function(t: Integer): PAnsiChar; cdecl = nil;  { V8.27  }
    f_OpenSSL_version_num :                    function: Longword; cdecl = nil;               { V8.27  }
    f_PEM_X509_INFO_read_bio :                 function(B: PBIO; Stack: PSTACK_OF_X509_INFO; CallBack: TPem_password_cb; UData: Pointer): PSTACK_OF_X509_INFO; cdecl = nil;//AG;   { V8.27 was PAnsiChar }
    f_PEM_do_header :                          function(cipher: PEVP_CIPHER_INFO; data: PAnsiChar; var len: Integer; callback: TPem_password_cb; u: Pointer): Integer; cdecl = nil;//AG;
    f_PEM_read_bio_DHparams :                  function(B: PBIO; X: PPDH; CallBack: TPem_password_cb; UData: Pointer): PDH; cdecl = nil;  { V8.07 }  { V8.27 was PAnsiChar }
    f_PEM_read_bio_PKCS7 :                     function(B: PBIO; X: PPPKCS7; CallBack: TPem_password_cb; UData: Pointer): PPKCS7; cdecl = nil;//AG; { V8.27 was PAnsiChar }
    f_PEM_read_bio_PrivateKey :                function(B: PBIO; X: PPEVP_PKEY; CB: TPem_password_cb; UData: Pointer): PEVP_PKEY; cdecl = nil; //AG   { V8.27 was PAnsiChar }
    f_PEM_read_bio_PUBKEY :                    function(B: PBIO; X: PPEVP_PKEY; CB: TPem_password_cb; UData: Pointer): PEVP_PKEY; cdecl = nil;         { V8.40 }
    f_PEM_read_bio_RSAPrivateKey :             function(B: PBIO; x: PPRSA; cb: TPem_password_cb; u: pointer): PRSA; cdecl = nil;
    f_PEM_read_bio_RSAPublicKey :              function(B: PBIO; x: PPRSA; CB: TPem_password_cb; UData: Pointer): PEVP_PKEY; cdecl = nil;              { V8.40 }
    f_PEM_read_bio_RSA_PUBKEY :                function(B: PBIO; x: PPRSA; cb: TPem_password_cb; u: pointer): PRSA; cdecl = nil;        { V8.11 }
    f_PEM_read_bio_X509 :                      function(B: PBIO; C509: PPX509; CallBack: TPem_password_cb; UData: Pointer): PX509; cdecl = nil;     { V8.27 was PAnsiChar }
    f_PEM_read_bio_X509_CRL :                  function(B: PBIO; CRL: PPX509_CRL; CallBack: TPem_password_cb; UData: Pointer): PX509_CRL; cdecl = nil;//AG    { V8.27 was PAnsiChar }
    f_PEM_read_bio_X509_REQ :                  function(B: PBIO; X: PPX509_REQ; CallBack: TPem_password_cb; UData: Pointer): PX509_REQ; cdecl = nil;   { V8.40 }
    f_PEM_write_bio_DHparams :                 function(B: PBIO; X: PDH): Integer; cdecl = nil;                  { V8.40 }
    f_PEM_write_bio_PKCS7 :                    function(B: PBIO; P7: PPKCS7): Integer; cdecl = nil;
    f_PEM_write_bio_PKCS8PrivateKey :          function(B: PBIO; X: PEVP_PKEY; const Enc: PEVP_CIPHER; Kstr: PAnsiChar; Klen: Integer; CallBack: TPem_password_cb; U: Pointer): Integer; cdecl = nil; { V8.40 }
    f_PEM_write_bio_PUBKEY :                   function(B: PBIO; X: PEVP_PKEY): Integer; cdecl = nil;            { V8.40 }
    f_PEM_write_bio_PrivateKey :               function(B: PBIO; X: PEVP_PKEY; const Enc: PEVP_CIPHER; Kstr: PAnsiChar; Klen: Integer; CallBack: TPem_password_cb; U: Pointer): Integer; cdecl = nil;//AG
    f_PEM_write_bio_RSAPrivateKey :            function(B: PBIO; X: PRSA; const Enc: PEVP_CIPHER; Kstr: PAnsiChar; Klen: Integer; CallBack: TPem_password_cb; U: Pointer): Integer; cdecl = nil; { V8.12 }
    f_PEM_write_bio_RSAPublicKey :             function(B: PBIO; X: PRSA): Integer; cdecl = nil; { V8.12 }
    f_PEM_write_bio_RSA_PUBKEY :               function(B: PBIO; X: PRSA): Integer; cdecl = nil; { V8.40 }
    f_PEM_write_bio_X509 :                     function(B: PBIO; Cert: PX509): Integer; cdecl = nil;
    f_PEM_write_bio_X509_CRL :                 function(B: PBIO; CRL: PX509_CRL) : Integer; cdecl = nil;
    f_PEM_write_bio_X509_REQ :                 function(B: PBIO; Cert_Req: PX509_REQ) : Integer; cdecl = nil;
    f_PKCS12_create :                          function(pass: PAnsiChar; name: PAnsiChar; pkey: PEVP_PKEY; cert: PX509; ca: PSTACK_OF_X509; nid_key, nid_cert, iter, mac_iter, keytype: Integer):PPKCS12; cdecl = nil;//AG;
    f_PKCS12_free :                            procedure(P12: PPKCS12); cdecl = nil;//AG;
    f_PKCS12_parse :                           function(P12: PPKCS12; Pass: PAnsiChar; var Pkey: PEVP_PKEY; var Cert: PX509; var Ca: PSTACK_OF_X509): Integer; cdecl = nil;//AG
    f_PKCS12_verify_mac :                      function(p12: PPKCS12; const pass: PAnsiChar; passlen: Integer): Integer; cdecl = nil;//AG;
    f_PKCS7_SIGNED_new :                       function: PPKCS7_SIGNED; cdecl = nil;  { V8.40 }
    f_PKCS7_add_certificate :                  function (p7: PPKCS7; x509: PX509): Integer; cdecl = nil;//AG;
    f_PKCS7_content_new :                      function(P7: PPKCS7; nid: Integer): Integer; cdecl = nil;//AG;
    f_PKCS7_free :                             procedure(P7: PPKCS7); cdecl = nil;//AG;
    f_PKCS7_new :                              function: PPKCS7; cdecl = nil;//AG;
    f_PKCS7_set_type :                         function(P7: PPKCS7; type_: Integer): Integer; cdecl = nil;//AG;
    f_RAND_add :                               procedure(buf: Pointer; num: Integer; entropy: Double); cdecl = nil;
    f_RAND_bytes :                             function(buf: PAnsiChar; num: Integer): Integer; cdecl = nil;
    f_RAND_cleanup :                           procedure; cdecl = nil;              { gone V8.27 }
    f_RAND_load_file :                         function(const FileName: PAnsiChar; Max_Bytes: Longint): Integer; cdecl = nil;
    f_RAND_poll :                              function: Integer; cdecl = nil;
    f_RAND_screen :                            procedure; cdecl = nil;
    f_RAND_seed :                              procedure(Buf: Pointer; Num: Integer); cdecl = nil;
    f_RAND_status :                            function: Integer; cdecl = nil;
    f_RAND_write_file :                        function(const FileName: PAnsiChar): Integer; cdecl = nil;
    f_RSA_free :                               procedure(RSA: PRSA); cdecl = nil;
    f_RSA_generate_key :                       function(Num: Integer; E: Cardinal; CallBack: TRSA_genkey_cb; cb_arg: Pointer): PRSA; cdecl = nil;//AG
    f_RSA_generate_key_ex :                    function(Rsa: PRSA; Bits: Integer; e: Pointer; cb: PBN_GENCB): Integer; cdecl = nil; //Angus  { V8.03 }
    f_RSA_new :                                function: PRSA; cdecl = nil;            { V8.03 }
    f_RSA_print :                              function(B: PBIO; Rsa: PRSA; Offset: Integer): Integer; cdecl = nil;//AG;
    f_RSA_private_decrypt :                    function(flen: Integer; from: PAnsiChar; to_: PAnsiChar; rsa: PRSA; padding: Integer): Integer; cdecl = nil;
    f_RSA_public_encrypt :                     function(flen: Integer; from: PAnsiChar; to_: PAnsiChar; rsa: PRSA; padding: Integer): Integer; cdecl = nil;
    f_RSA_size :                               function(Rsa: PRSA): Integer; cdecl = nil; //Angus
    f_X509V3_EXT_conf_nid :                    function(Conf: PLHASH; Ctx: PX509V3_CTX; ext_nid: Integer; value: PAnsiChar): PX509_EXTENSION; cdecl = nil;
    f_X509V3_EXT_d2i :                         function(Ext: PX509_EXTENSION): Pointer; cdecl = nil;//AG;
    f_X509V3_EXT_get :                         function(Ext: PX509_EXTENSION): PX509V3_EXT_METHOD; cdecl = nil;
    f_X509V3_EXT_print :                       function(B: PBIO; Ext: PX509_EXTENSION; Flag: Integer; Indent: Integer):Integer; cdecl = nil;//AG;
    f_X509V3_add1_i2d :                        function(Ext: PSTACK_OF_X509_EXTENSION; nid: integer; value: Pointer; crit: integer; flags: longword): Integer; cdecl = Nil;  { V8.41 }
    f_X509V3_conf_free :                       procedure(Val: PCONF_VALUE); cdecl = nil;//AG
    f_X509_CRL_dup :                           function(CRL: PX509_CRL): PX509_CRL; cdecl = nil;//AG;
    f_X509_CRL_free :                          procedure(CRL: PX509_CRL); cdecl = nil;//AG
    f_X509_CRL_sign :                          function(Cert: PX509_CRL; pkey: PEVP_PKEY; md: PEVP_MD): Integer; cdecl = Nil;    { V8.40 }
    f_X509_CRL_sign_ctx :                      function(Cert: PX509_CRL; ctx: PEVP_MD_CTX): Integer; cdecl = Nil;    { V8.40 }
    f_X509_CRL_verify :                        function(Cert: PX509_CRL; pkey: PEVP_PKEY): Integer; cdecl = Nil;     { V8.40 }
    f_X509_EXTENSION_free :                    procedure(Ext: PX509_EXTENSION); cdecl = nil;
    f_X509_EXTENSION_get_critical :            function(Ext: PX509_EXTENSION): Integer; cdecl = nil;//AG;
    f_X509_EXTENSION_get_data :                function(Ext: PX509_EXTENSION): PASN1_STRING; cdecl = nil; //AG;     { V8.40 was octetstring }
    f_X509_EXTENSION_get_object :              function(Ext: PX509_EXTENSION): PASN1_OBJECT; cdecl = nil;
    f_X509_INFO_free :                         procedure(Xi: PX509_INFO); cdecl = nil;//AG;
    f_X509_LOOKUP_by_fingerprint :             function(Ctx: PX509_LOOKUP; Typ_: Integer; Bytes: PAnsiChar; Len: Integer; Ret: PX509_OBJECT ): Integer; cdecl = nil;//AG;
    f_X509_LOOKUP_by_issuer_serial :           function(Ctx: PX509_LOOKUP; Typ_: Integer; Name: PX509_NAME; Serial: PASN1_INTEGER; Ret: PX509_OBJECT): Integer; cdecl = nil;//AG;
    f_X509_LOOKUP_ctrl :                       function(Ctx: PX509_LOOKUP; Cmd: Integer; Argc: PAnsiChar; Argl: Cardinal; Ret: PPAnsiChar): Integer; cdecl = nil;//AG;
    f_X509_LOOKUP_file :                       function: PX509_LOOKUP_METHOD; cdecl = nil;//AG;
    f_X509_LOOKUP_free :                       procedure(Ctx: PX509_LOOKUP); cdecl = nil;//AG;
    f_X509_LOOKUP_hash_dir :                   function: PX509_LOOKUP_METHOD; cdecl = nil;//AG;
    f_X509_LOOKUP_new :                        function(Method: PX509_LOOKUP_METHOD): PX509_LOOKUP; cdecl = nil;//AG;
    f_X509_NAME_ENTRY_get_data :               function(Ne: PX509_NAME_ENTRY) : PASN1_STRING; cdecl = nil;//AG
    f_X509_NAME_ENTRY_get_object :             function(Ne: PX509_NAME_ENTRY): PASN1_OBJECT; cdecl = nil;//AG
    f_X509_NAME_add_entry_by_NID :             function(Name: PX509_NAME; Nid: Integer; Type_: Integer; Buf: PAnsiChar; BufferSize: Integer; Loc: Integer; Set_: Integer): Integer; cdecl = nil;//AG
    f_X509_NAME_add_entry_by_txt :             function(Name: PX509_NAME; Field: PAnsiChar; Type_: Integer; Buf: PAnsiChar; BufferSize: Integer; Loc: Integer; Set_: Integer): Integer; cdecl = nil;//AG
    f_X509_NAME_cmp :                          function(const a: PX509_NAME; const b: PX509_NAME): Integer; cdecl = nil;//AG;
    f_X509_NAME_dup :                          function(Name: PX509_NAME): PX509_NAME; cdecl = nil;        { V8.40 }
    f_X509_NAME_entry_count :                  function(Name: PX509_NAME) : Integer; cdecl = nil; //AG
    f_X509_NAME_free :                         procedure(AName: PX509_NAME); cdecl = nil;//AG;
    f_X509_NAME_get_entry :                    function(Name: PX509_NAME; Loc: Integer): PX509_NAME_ENTRY; cdecl = nil;//AG
    f_X509_NAME_get_index_by_NID:              function(CertName: PX509_NAME; Nid: Integer; LastPos: Integer): Integer; cdecl = nil; //AG
    f_X509_NAME_get_text_by_NID :              function(CertName: PX509_NAME; Nid: Integer; Buf : PAnsiChar; Len : Integer): Integer; cdecl = nil;
    f_X509_NAME_new :                          function: PX509_NAME; cdecl = nil;//AG
    f_X509_NAME_oneline :                      function(CertName: PX509_NAME; Buf: PAnsiChar; BufSize: Integer): PAnsiChar; cdecl = nil;
    f_X509_NAME_print_ex :                     function(B: PBIO; Name: PX509_NAME; indent: Integer; flags: Integer): Integer; cdecl = Nil;  { V8.40 }
    f_X509_PKEY_free :                         procedure(PKey: PX509_PKEY); cdecl = nil;//AG;
    f_X509_OBJECT_get_type :                   function (a: PX509_OBJECT): TX509_LOOKUP_TYPE; cdecl = nil; { V8.39 }
    f_X509_OBJECT_get0_X509 :                  function (a: PX509_OBJECT): PX509; cdecl = nil; { V8.39 }
    f_X509_OBJECT_get0_X509_CRL :              function (a: PX509_OBJECT): PX509_CRL; cdecl = nil; { V8.39 }
    f_X509_PUBKEY_free :                       procedure(Key: PEVP_PKEY); cdecl = nil; //AG;
    f_X509_PURPOSE_get0 :                      function(Idx: Integer): PX509_PURPOSE; cdecl = nil;//AG;
    f_X509_PURPOSE_get0_name :                 function(XP: PX509_PURPOSE): PAnsiChar; cdecl = nil;//AG;
    f_X509_PURPOSE_get0_sname :                function(XP: PX509_PURPOSE): PAnsiChar; cdecl = nil;//AG;
    f_X509_PURPOSE_get_count :                 function: Integer; cdecl = nil;//AG;
    f_X509_PURPOSE_get_id :                    function(XP: PX509_PURPOSE): Integer; cdecl = nil;//AG;
    f_X509_REQ_add_extensions :                function(Req: PX509_REQ; Exts: PSTACK): Integer; cdecl = nil;
    f_X509_REQ_dup :                           function(Req: PX509_REQ): PX509_REQ; cdecl = nil;         { V8.40 }
    f_X509_REQ_free :                          procedure(Req: PX509_REQ); cdecl = nil;
    f_X509_REQ_get0_pubkey :                   function(Req: PX509_REQ): PEVP_PKEY; cdecl = nil;  { V8.40 }
    f_X509_REQ_get_extensions :                function(Req: PX509_REQ): PSTACK; cdecl = nil;     { V8.41 }
    f_X509_REQ_get_pubkey :                    function(Req: PX509_REQ): PEVP_PKEY; cdecl = nil;  { V8.40 }
    f_X509_REQ_get_subject_name :              function(AReq: PX509_REQ): PX509_NAME; cdecl = nil; { V8.36 OpenSSL 1.1.0 some macros are now exported functions }
    f_X509_REQ_new :                           function: PX509_REQ; cdecl = nil;
    f_X509_REQ_print :                         function(B: PBIO; Req: PX509_REQ): Integer; cdecl = Nil;  { V8.40 }
    f_X509_REQ_print_ex :                      function(B: PBIO; Req: PX509_REQ; indent: Integer; flags: Integer): Integer; cdecl = Nil;  { V8.40 }
    f_X509_REQ_set_pubkey :                    function(Req: PX509_REQ; PKey: PEVP_PKEY): Integer; cdecl = nil;
    f_X509_REQ_set_version :                   function(Req: PX509_REQ; Version: LongInt): Integer; cdecl = nil;
    f_X509_REQ_sign :                          function(Req: PX509_REQ; PKey: PEVP_PKEY; const Md: PEVP_MD): Integer; cdecl = nil;
    f_X509_REQ_sign_ctx :                      function(Req: PX509_REQ; ctx: PEVP_MD_CTX): Integer; cdecl = Nil;    { V8.40 }
    f_X509_REQ_to_X509 :                       function(Req: PX509_REQ; days: integer; PKey: PEVP_PKEY): PX509; cdecl = nil;         { V8.40 self signed cert only }
    f_X509_REQ_verify :                        function(Req: PX509_REQ; pkey: PEVP_PKEY): Integer; cdecl = Nil;     { V8.40 }
    f_X509_STORE_CTX_cleanup :                 procedure(Ctx: PX509_STORE_CTX); cdecl = nil;//AG;
    f_X509_STORE_CTX_free :                    procedure(Ctx: PX509_STORE_CTX); cdecl = nil;//AG;
    f_X509_STORE_CTX_get_chain :               function(Ctx: PX509_STORE_CTX): PSTACK_OF_X509; cdecl = nil;//AG;
    f_X509_STORE_CTX_get_current_cert :        function(Ctx: PX509_STORE_CTX): PX509; cdecl = nil;
    f_X509_STORE_CTX_get_error :               function(Ctx: PX509_STORE_CTX): Integer; cdecl = nil;
    f_X509_STORE_CTX_get_error_depth :         function(Ctx: PX509_STORE_CTX): Integer; cdecl = nil;
    f_X509_STORE_CTX_get_ex_data :             function(Ctx: PX509_STORE_CTX; Idx: Integer): Pointer; cdecl = nil;
    f_X509_STORE_CTX_init :                    function(Ctx: PX509_STORE_CTX; Store: PX509_STORE; Cert: PX509; UnTrustedChain: PSTACK_OF_X509): Integer; cdecl = nil;//AG;
    f_X509_STORE_CTX_new :                     function: PX509_STORE_CTX; cdecl = nil;//AG;
    f_X509_STORE_CTX_set_error :               procedure(Ctx: PX509_STORE_CTX; s: Integer); cdecl = nil;
    f_X509_STORE_CTX_set_ex_data :             function(Ctx: PX509_STORE_CTX; Idx: Integer; Data: Pointer): Integer; cdecl = nil;//AG;
    f_X509_STORE_CTX_set_purpose :             function(Ctx: PX509_STORE_CTX; Purpose: Integer): Integer; cdecl = nil;//AG;
    f_X509_STORE_CTX_set_verify_cb :           procedure(Ctx: PX509_STORE_CTX; Cb: TSetVerify_cb); cdecl = nil;//AG;
    f_X509_STORE_CTX_trusted_stack :           procedure(Ctx: PX509_STORE_CTX; STACK_OF_X509: PSTACK_OF_X509); cdecl = nil;//AG;
    f_X509_STORE_add_cert :                    function(Store: PX509_STORE; Cert: PX509): Integer; cdecl = nil;//AG;
    f_X509_STORE_add_crl :                     function(Store: PX509_STORE; CRL: PX509_CRL): Integer; cdecl = nil;//AG;
    f_X509_STORE_add_lookup :                  function(Store: PX509_STORE; Meth: PX509_LOOKUP_METHOD): PX509_LOOKUP; cdecl = nil;//AG;
    f_X509_STORE_free :                        procedure(Store: PX509_STORE); cdecl = nil;//AG;
    f_X509_STORE_get0_objects :                function(Ctx: PX509_STORE): PSTACK_OF_X509_OBJECT; cdecl = nil;   { V8.39 }
    f_X509_STORE_get0_param :                  function(Ctx: PX509_STORE): PX509_VERIFY_PARAM; cdecl = nil;   { V8.39 }
    f_X509_STORE_set1_param :                  function(Ctx: PX509_STORE; pm: PX509_VERIFY_PARAM): Integer; cdecl = nil;   { V8.39 }
    f_X509_STORE_new :                         function: PX509_STORE; cdecl = nil;//AG;
    f_X509_STORE_set_flags :                   procedure(Store: PX509_STORE; Flags: Longword); cdecl = nil;//AG;
    f_X509_VERIFY_PARAM_add0_policy :          function(param: PX509_VERIFY_PARAM; policy: PASN1_OBJECT): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_add0_table :           function(param: PX509_VERIFY_PARAM): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_add1_host :            function(param: PX509_VERIFY_PARAM; Pname: PAnsiChar; namelen: size_t): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_clear_flags :          function(param: PX509_VERIFY_PARAM; flags: Cardinal): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_free :                 procedure(param: PX509_VERIFY_PARAM); cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_get0 :                 function(id: Integer): PX509_VERIFY_PARAM; cdecl = nil;
    f_X509_VERIFY_PARAM_get0_name :            function(param: PX509_VERIFY_PARAM): PAnsiChar; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_get0_peername :        function(param: PX509_VERIFY_PARAM): PAnsiChar; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_get_auth_level :       function(param: PX509_VERIFY_PARAM): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_get_count :            function: Integer; cdecl = nil;
    f_X509_VERIFY_PARAM_get_depth :            function(param: PX509_VERIFY_PARAM): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_get_flags :            function(param: PX509_VERIFY_PARAM): Cardinal; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_inherit :              function(Pto: PX509_VERIFY_PARAM; Pfrom: PX509_VERIFY_PARAM): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_lookup :               function(Pname: PAnsiChar): PX509_VERIFY_PARAM; cdecl = nil;
    f_X509_VERIFY_PARAM_move_peername :        procedure(param1: PX509_VERIFY_PARAM; param2: PX509_VERIFY_PARAM); cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_new :                  function: PX509_VERIFY_PARAM; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set1 :                 function(Pto: PX509_VERIFY_PARAM; Pfrom: PX509_VERIFY_PARAM): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set1_email :           function(param: PX509_VERIFY_PARAM; email: PAnsiChar; namelen: size_t): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set1_host :            function(param: PX509_VERIFY_PARAM; Pname: PAnsiChar; namelen: size_t): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set1_ip :              function(param: PX509_VERIFY_PARAM; ip: PAnsiChar; namelen: size_t): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set1_ip_asc :          function(param: PX509_VERIFY_PARAM; ipasc: PAnsiChar; namelen: size_t): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set1_name :            function(param: PX509_VERIFY_PARAM; Pname: PAnsiChar): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set1_policies :        function(param: PX509_VERIFY_PARAM; policies: PSTACK_OF_ASN1_OBJECT): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set_auth_level :       procedure(param: PX509_VERIFY_PARAM; auth_level: Integer); cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set_depth :            procedure(param: PX509_VERIFY_PARAM; depth: Integer); cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set_flags :            function(param: PX509_VERIFY_PARAM; flags: Cardinal): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set_hostflags :        procedure(param: PX509_VERIFY_PARAM; flags: Cardinal); cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set_purpose :          function(param: PX509_VERIFY_PARAM; purpose: Integer): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set_time :             procedure(param: PX509_VERIFY_PARAM; t: longword); cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_set_trust :            function(param: PX509_VERIFY_PARAM; trust: Integer): Integer; cdecl = nil;   { V8.39 }
    f_X509_VERIFY_PARAM_table_cleanup :        procedure; cdecl = nil;   { V8.39 }
    f_X509_add_ext :                           function(Cert: PX509; Ex: PX509_EXTENSION; loc: Integer): Integer; cdecl = nil;
    f_X509_add1_ext_i2d :                      function(Cert: PX509; nid: integer; value: Pointer; crit: integer; flags: longword): Integer; cdecl = nil;     { V8.40 }
    f_X509_check_ca :                          function(X: PX509): Integer; cdecl = nil;//AG;
    f_X509_check_email :                       function(Cert: PX509; Paddress: PAnsiChar; namelen: size_t;  flags: Cardinal): Integer; cdecl = nil;   { V8.39 }
    f_X509_check_host :                        function(Cert: PX509; Pname: PAnsiChar; namelen: size_t;  flags: Cardinal; var peername: AnsiString): Integer; cdecl = nil;   { V8.39 }
    f_X509_check_ip :                          function(Cert: PX509; Paddress: PAnsiChar; namelen: size_t;  flags: Cardinal): Integer; cdecl = nil;   { V8.39 }
    f_X509_check_ip_asc :                      function(Cert: PX509; Paddress: PAnsiChar; namelen: size_t;  flags: Cardinal): Integer; cdecl = nil;   { V8.39 }
    f_X509_check_issued :                      function(Issuer: PX509; Subject: PX509): Integer; cdecl = nil;//AG;
    f_X509_check_private_key :                 function(Cert: PX509; PKey: PEVP_PKEY): Integer; cdecl = nil; //AG
    f_X509_check_purpose :                     function(Cert: PX509; ID: Integer; CA: Integer): Integer; cdecl = nil;//AG;
    f_X509_digest :                            function(Cert: PX509; Type_: PEVP_MD; Buf: PAnsiChar; BufSize: PInteger): Integer; cdecl = nil; //AG
    f_X509_dup :                               function(X: PX509): PX509; cdecl = nil;//AG;
    f_X509_free :                              procedure(Cert: PX509); cdecl = nil;
    f_X509_get0_notAfter :                     function(X: PX509): PASN1_TIME; cdecl = nil;    { V8.32 OpenSSL 1.1.0 some macros are now exported functions }
    f_X509_get0_notBefore :                    function(X: PX509): PASN1_TIME; cdecl = nil;    { V8.32 OpenSSL 1.1.0 some macros are now exported functions }
    f_X509_get0_pubkey :                       function(Cert: PX509): PEVP_PKEY; cdecl = nil;  { V8.40 OpenSSL 1.1.0 }
    f_X509_get_ext :                           function(Cert: PX509; Loc: Integer): PX509_EXTENSION; cdecl = nil;
    f_X509_get_ext_d2i :                       function(Cert: PX509; nid: Integer; var crit: Integer; var idx: Integer): Pointer; cdecl = nil;    { V8.40 }
    f_X509_get_ext_count :                     function(Cert: PX509): Integer; cdecl = nil;
    f_X509_get_issuer_name :                   function(Cert: PX509): PX509_NAME; cdecl = nil;
    f_X509_get_pubkey :                        function(Cert: PX509): PEVP_PKEY; cdecl = nil; //AG;
    f_X509_get_serialNumber :                  function(Cert: PX509): PASN1_INTEGER; cdecl = nil;
    f_X509_get_signature_nid :                 function(X: PX509): Integer; cdecl = nil;      { V8.32 OpenSSL 1.1.0 some macros are now exported functions }
    f_X509_get_subject_name :                  function(Cert: PX509): PX509_NAME; cdecl = nil;
    f_X509_gmtime_adj :                        function(S: PASN1_TIME; Adj: LongInt): PASN1_TIME; cdecl = nil;//AG
    f_X509_load_crl_file :                     function(Ctx: PX509_LOOKUP; const Filename: PAnsiChar; type_: Integer): Integer; cdecl = nil;//AG;
    f_X509_new :                               function: PX509; cdecl = nil;//AG
    f_X509_print :                             function(B: PBIO; Cert: PX509): Integer; cdecl = nil;
    f_X509_print_ex :                          function(B: PBIO; Cert: PX509; nmflag: Integer; cflag: Integer): Integer; cdecl = Nil;        { V8.40 }
    f_X509_set_issuer_name :                   function(Cert: PX509; Name: PX509_NAME): Integer; cdecl = nil;//AG
    f_X509_set_pubkey :                        function(Cert: PX509; PKey: PEVP_PKEY): Integer; cdecl = nil;//AG
    f_X509_set_subject_name :                  function(Cert: PX509; Name: PX509_NAME): Integer; cdecl = nil;   { V8.40 }
    f_X509_set_version :                       function(Cert: PX509; Version: LongInt): Integer; cdecl = nil;//AG
    f_X509_sign :                              function(Cert: PX509; PKey: PEVP_PKEY; const Md: PEVP_MD): Integer; cdecl = nil;//AG
    f_X509_sign_ctx :                          function(Cert: PX509; ctx: PEVP_MD_CTX): Integer; cdecl = Nil;        { V8.40 }
    f_X509_subject_name_hash :                 function(Cert: PX509): Cardinal; cdecl = nil;
    f_X509_to_X509_REQ :                       function(Cert: PX509; PKey: PEVP_PKEY; const Md: PEVP_MD): PX509_REQ; cdecl = nil;    { V8.40 }
    f_X509_verify :                            function(Cert: PX509; pkey: PEVP_PKEY): Integer; cdecl = Nil;         { V8.40 }
    f_X509_verify_cert :                       function(Ctx: PX509_STORE_CTX): Integer; cdecl = nil;//AG;
    f_X509_verify_cert_error_string :          function(ErrCode : Integer): PAnsiChar; cdecl = nil;
    f_d2i_ECDSA_SIG :                          function(sig: PECDSA_SIG; pp: PAnsiChar; len: DWORD): PECDSA_SIG; cdecl = Nil;         { V8.40 }
    f_d2i_PKCS12_bio :                         function(B: PBIO; p12: PPPKCS12): PPKCS12; cdecl = nil; //AG
    f_d2i_PKCS7_bio:                           function(B: PBIO; p7: PPKCS7): PPKCS7; cdecl = nil; //AG
    f_d2i_PKCS8PrivateKey_bio:                 function(bp: PBIO; x: PPEVP_PKEY; cb: Tpem_password_cb; u: pointer): PEVP_PKEY; cdecl = nil;     { V8.11 }
    f_d2i_PrivateKey :                         function(type_: Integer; var a: PEVP_PKEY; var pp : PAnsiChar; length: Integer): PEVP_PKEY; cdecl = nil;//AG
    f_d2i_PrivateKey_bio :                     function(B: PBIO; A: PPEVP_PKEY): PEVP_PKEY; cdecl = nil;//AG
    f_d2i_RSAPrivateKey:                       function(a: PPRSA; var pp: PByte; length: Integer): PRSA; cdecl = nil;
    f_d2i_X509 :                               function(C509: PPX509; Buf: PPAnsiChar; Len: Integer): PX509; cdecl = nil;
    f_d2i_X509_REQ :                           function(CReq: PPX509_REQ; Buf: PPAnsiChar; Len: Integer): PX509; cdecl = nil;   { V8.40 }
    f_d2i_X509_REQ_bio :                       function(B: PBIO; Req: PPX509_REQ): PX509_REQ; cdecl = nil;                     { V8.40 }
    f_d2i_X509_bio :                           function(B: PBIO; X509: PPX509): PX509; cdecl = nil;
    f_i2a_ASN1_OBJECT :                        function(B: PBIO; A: PASN1_OBJECT): Integer; cdecl = nil;//AG;
    f_i2d_ECDSA_SIG :                          function(sig: PECDSA_SIG; pp: PAnsiChar; len: DWORD): Integer; cdecl = Nil;     { V8.40 }
    f_i2d_PKCS12_bio :                         function(B: PBIO; p12: PPKCS12): Integer; cdecl = nil;
    f_i2d_PKCS7_bio :                          function(B: PBIO; p7: PPKCS7): Integer; cdecl = nil;                            { V8.41 }
    f_i2d_PrivateKey :                         function(A: PEVP_PKEY; PP: PPAnsiChar): Integer; cdecl = nil;//AG
    f_i2d_PrivateKey_bio :                     function(B: PBIO; pkey: PEVP_PKEY): Integer; cdecl = nil;//AG
    f_i2d_RSAPublicKey:                        function(a: PRSA; var pp: PByte): Integer; cdecl = nil;
    f_i2d_RSA_PUBKEY:                          function(a: PRSA; var pp: PByte): Integer; cdecl = nil;
    f_i2d_X509 :                               function(Cert: PX509; pOut: PPAnsiChar): Integer; cdecl = nil;//AG
    f_i2d_X509_REQ :                           function(Req: PX509_REQ; pOut: PPAnsiChar): Integer; cdecl = nil;               { V8.40 }
    f_i2d_X509_REQ_bio :                       function(B: PBIO; Req: PX509_REQ): Integer; cdecl = nil;                        { V8.40 }
    f_i2d_X509_bio :                           function(B: PBIO; X509: PX509): Integer; cdecl = nil;


{$IFDEF OPENSSL_USE_DELPHI_MM}
    f_CRYPTO_set_mem_functions :               function(M: TCryptoMallocFunc; R: TCryptoReallocFunc; F: TCryptoFreeMemFunc): Integer; cdecl = nil; //AG
{$ENDIF}

{$IFNDEF OPENSSL_NO_ENGINE}
    f_ENGINE_load_builtin_engines :            procedure; cdecl = nil; //AG;
    f_ENGINE_register_all_complete :           procedure; cdecl = nil; //AG;
    f_ENGINE_cleanup :                         procedure; cdecl = nil; //AG;
    f_ENGINE_by_id :                           function(const id: PAnsiChar): PENGINE; cdecl = nil; //AG;
    f_ENGINE_init :                            function(e: PENGINE): Integer; cdecl = nil; //AG;
    f_ENGINE_finish :                          function(e: PENGINE): Integer; cdecl = nil; //AG;
    f_ENGINE_set_default :                     function(e: PENGINE; flags: Cardinal): Integer; cdecl = nil; //AG;
    f_ENGINE_ctrl_cmd_string :                 function(e: PENGINE; const cmd_name: PAnsiChar; const arg: PAnsiChar; cmd_optional: Integer): Integer; cdecl = nil; //AG;
    f_ENGINE_free :                            function(e: PENGINE): Integer; cdecl = nil; //AG;
    f_ENGINE_load_private_key :                function(e: PENGINE; key_id: PAnsiChar; ui_method: PUI_METHOD; callback_data: Pointer): PEVP_PKEY; cdecl = nil; //AG;
    f_ENGINE_load_public_key :                 function(e: PENGINE; const key_id: PAnsiChar; ui_method: PUI_METHOD; callback_data: Pointer): PEVP_PKEY; cdecl = nil; //AG;
    f_ENGINE_load_ssl_client_cert :            function(e: PENGINE; SSL: PSSL; ca_dn: PSTACK_OF_X509_NAME; pcert: PPX509; ppkey: PPEVP_PKEY;
                                               pother: PSTACK_OF_X509; ui_method: PUI_METHOD; callback_data: Pointer): Integer; cdecl = nil;
    f_UI_new :                                 function: PUI; cdecl = nil; //AG;
    f_UI_new_method :                          function(const method: PUI_METHOD): PUI; cdecl = nil; //AG;
    f_UI_free :                                procedure(ui: PUI); cdecl = nil; //AG;
    f_UI_create_method :                       function(name: PAnsiChar): PUI_METHOD; cdecl = nil; //AG;
    f_UI_destroy_method :                      procedure(ui_method: PUI_METHOD); cdecl = nil; //AG;
    f_UI_set_ex_data :                         function(r: PUI; idx: Integer; arg: Pointer): Integer; cdecl = nil; //AG;
    f_UI_get_ex_data :                         function(r: PUI; idx: Integer): Pointer; cdecl = nil; //AG;
    f_UI_method_set_reader :                   function(method: PUI_METHOD; reader: TPinCallBack):Integer; cdecl = nil; //AG;
    f_UI_set_result :                          function(ui: PUI; uis: PUI_STRING; const result: PAnsiChar): Integer; cdecl = nil; //AG;
    f_UI_OpenSSL :                             function: PUI_METHOD; cdecl = nil; //AG;
 (*
    http://openssl.org/docs/crypto/engine.html
    Here we'll assume we want to load and register all ENGINE implementations
    bundled with OpenSSL, such that for any cryptographic algorithm required by
    OpenSSL - if there is an ENGINE that implements it and can be initialise, it
    should be used. The following code illustrates how this can work;

    /* Load all bundled ENGINEs into memory and make them visible */
    ENGINE_load_builtin_engines();
    /* Register all of them for every algorithm they collectively implement */
    ENGINE_register_all_complete();

    That's all that's required. Eg. the next time OpenSSL tries to set up an
    RSA key, any bundled ENGINEs that implement RSA_METHOD will be passed to
    ENGINE_init() and if any of those succeed, that ENGINE will be set as the
    default for RSA use from then on.
    *)
{$ENDIF}

    { Function name constants }
    FN_SSLeay                                 = 'SSLeay';                { up to 1.1.0 }
    FN_OpenSSL_version_num                    = 'OpenSSL_version_num';   { V8.27 from 1.1.0  }

function LibeayLoad : Boolean;

// most of these functions replace OpenSSL macros
function ERR_GET_REASON(ErrCode : Cardinal) : Cardinal; {$IFDEF USE_INLINE} inline; {$ENDIF}
function ERR_GET_LIB(ErrCode : Cardinal) : Cardinal; {$IFDEF USE_INLINE} inline; {$ENDIF}
function ERR_GET_FUNC(ErrCode : Cardinal) : Cardinal; {$IFDEF USE_INLINE} inline; {$ENDIF}
function ERR_FATAL_ERROR(ErrCode : Cardinal) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_get_flags(b: PBIO): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_should_retry(b: PBIO): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_should_read(b: PBIO): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_should_write(b: PBIO): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_should_io_special(b: PBIO): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_retry_type(b: PBIO): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function ASN1_ITEM_ptr(iptr: PASN1_ITEM_EXP): PASN1_ITEM; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslVersion : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslCompilerFlags : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslBuiltOn : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslPlatForm : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslDir : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function IcsX509VerifyErrorToStr(ErrCode: Integer): String;
function f_Ics_X509_get_notBefore(X: PX509): PASN1_TIME;
function f_Ics_X509_get_notAfter(X: PX509): PASN1_TIME;
function f_Ics_X509_CRL_get_issuer(crl: PX509_CRL): PX509_NAME; {$IFDEF USE_INLINE} inline; {$ENDIF}// Macro
function f_Ics_X509_get_version(X509: PX509): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_Ics_X509_REQ_get_subject_name(AReq: PX509_REQ): PX509_NAME;  { V8.36 renamed }
function f_Ics_X509_LOOKUP_load_file(Ctx: PX509_LOOKUP; FileName: PAnsiChar; Type_: Longword): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_Ics_X509_LOOKUP_add_dir(Ctx: PX509_LOOKUP; DirName: PAnsiChar; Type_: Longword): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_Ics_X509_get_signature_algorithm(X509: PX509): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function Ics_EVP_PKEY_dup(PKey: PEVP_PKEY): PEVP_PKEY;       { V8.40 was in wsocket }
procedure Ics_Ssl_EVP_PKEY_IncRefCnt(K: PEVP_PKEY; Increment: Integer = 1); {$IFDEF USE_INLINE} inline; {$ENDIF}
function Ics_Ssl_EVP_PKEY_GetKey(K: PEVP_PKEY): Pointer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function Ics_Ssl_EVP_PKEY_GetType(K: PEVP_PKEY): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_SSL_get_secure_renegotiation_support(S: PSSL): Longint; {$IFDEF USE_INLINE} inline; {$ENDIF}
function Ics_Ssl_ERR_GET_LIB(E: DWORD): Integer;
function Ics_Ssl_ERR_GET_FUNC(E: DWORD): Integer;
function Ics_Ssl_ERR_GET_REASON(E: DWORD): Integer;
function Asn1ToUTDateTime(Asn1Time: PASN1_TIME; out UT: TDateTime): Boolean;
function Asn1ToString(PAsn1 : PASN1_STRING): String;
function f_EVP_MD_CTX_create: PEVP_MD_CTX;                     { V8.40 }
function f_EVP_MD_CTX_init(ctx: PEVP_MD_CTX): integer;          { V8.40 }
procedure f_EVP_MD_CTX_destroy(ctx: PEVP_MD_CTX);               { V8.40 }
function f_EVP_MD_nid(e: PEVP_MD): Integer;                     { V8.40 }
function f_EVP_MD_name(e: PEVP_MD): PAnsiChar;                  { V8.40 }
function f_EVP_MD_CTX_size(e: PEVP_MD_CTX): Integer;            { V8.40 }
function f_EVP_MD_CTX_block_size(e: PEVP_MD_CTX): Integer;      { V8.40 }
function f_EVP_MD_CTX_type(e: PEVP_MD_CTX): Integer;            { V8.40 }
function f_EVP_get_digestbynid(a: Integer): PEVP_MD;            { V8.40 }
function f_EVP_get_digestbyobj(a: PASN1_OBJECT): PEVP_MD;       { V8.40 }
function f_EVP_get_cipherbynid(a: Integer): PEVP_CIPHER;        { V8.40 }
function f_EVP_get_cipherbyobj(a: PASN1_OBJECT): PEVP_CIPHER;   { V8.40 }
function f_EVP_SignInit_ex(ctx: PEVP_MD_CTX; etype: PEVP_MD; impl: PEngine): Integer;   { V8.40 }
function f_EVP_SignInit(ctx: PEVP_MD_CTX; etype: PEVP_MD): Integer; cdecl;              { V8.40 }
function f_EVP_SignUpdate(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;          { V8.40 }
function f_EVP_OpenUpdate(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer; const in_: PAnsiChar; inl: Integer): LongBool;  { V8.40 }
function f_EVP_SealUpdate(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer; const in_: PAnsiChar; inl: Integer): LongBool;  { V8.40 }
function f_EVP_VerifyInit_ex(ctx: PEVP_MD_CTX; etype: PEVP_MD; impl: PEngine): Integer; { V8.40 }
function f_EVP_VerifyInit(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;          { V8.40 }
function f_EVP_VerifyUpdate(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;        { V8.40 }
function f_DigestSignUpdate(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;        { V8.40 }
function f_DigestVerifyUpdate(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;      { V8.40 }
function f_EC_KEY_get_ex_new_index(argl: DWord; argp: Pointer; new_func: TCryptoExNewFunc; dup_func: TCryptoExDupFunc; free_func: TCryptoExFreeFunc): integer;  { V8.40 }

function  IcsRandSeedFromFile(const FileName: String; MaxBytes: Integer = -1): Integer;

{$IFNDEF OPENSSL_NO_ENGINE}
function f_Ics_UI_set_app_data(r: PUI; arg: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_Ics_UI_get_app_data(r: PUI): Pointer; {$IFDEF USE_INLINE} inline; {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}  { V8.04 }
procedure IcsRandPoll;
{$ENDIF}

// V8.35 all OpenSSL exports now in tables, with versions if only available conditionally
const
    GLIBEAYImports1: array[0..531] of TOSSLImports = (

    (F: @@f_ASN1_INTEGER_get ;        N: 'ASN1_INTEGER_get';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ASN1_INTEGER_get_int64 ;  N: 'ASN1_INTEGER_get_int64';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),    { V8.40 }
    (F: @@f_ASN1_INTEGER_get_uint64 ; N: 'ASN1_INTEGER_get_uint64';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_ASN1_INTEGER_set ;        N: 'ASN1_INTEGER_set';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ASN1_INTEGER_set_int64 ;  N: 'ASN1_INTEGER_set_int64';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),    { V8.40 }
    (F: @@f_ASN1_INTEGER_set_uint64 ; N: 'ASN1_INTEGER_set_uint64';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_ASN1_STRING_free ;        N: 'ASN1_STRING_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ASN1_STRING_get0_data ;   N: 'ASN1_STRING_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),          { V8.40 load deprecated export }
    (F: @@f_ASN1_STRING_length ;      N: 'ASN1_STRING_length';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),         { V8.40 }
    (F: @@f_ASN1_STRING_length_set ;  N: 'ASN1_STRING_length_set';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_ASN1_STRING_new ;         N: 'ASN1_STRING_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),            { V8.40 }
    (F: @@f_ASN1_STRING_print;        N: 'ASN1_STRING_print';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ASN1_STRING_type ;        N: 'ASN1_STRING_type';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),           { V8.40 }
    (F: @@f_ASN1_STRING_set0 ;        N: 'ASN1_STRING_set0';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),           { V8.40 }
    (F: @@f_ASN1_STRING_to_UTF8;      N: 'ASN1_STRING_to_UTF8';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ASN1_item_d2i;            N: 'ASN1_item_d2i';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ASN1_item_free ;          N: 'ASN1_item_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_ctrl ;   N: 'BIO_ctrl';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_ctrl_get_read_request;   N: 'BIO_ctrl_get_read_request';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_ctrl_get_write_guarantee ;   N: 'BIO_ctrl_get_write_guarantee';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_ctrl_pending ;   N: 'BIO_ctrl_pending';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_free ;   N: 'BIO_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_get_retry_BIO;   N: 'BIO_get_retry_BIO';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_get_retry_reason ;   N: 'BIO_get_retry_reason';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_gets ;   N: 'BIO_gets';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_new;   N: 'BIO_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_new_bio_pair ;   N: 'BIO_new_bio_pair';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_new_fd ;   N: 'BIO_new_fd';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_new_file ;   N: 'BIO_new_file';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_new_mem_buf;   N: 'BIO_new_mem_buf';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_new_socket ;   N: 'BIO_new_socket';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_nread0 ;   N: 'BIO_nread0';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_nread;   N: 'BIO_nread';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_nwrite ;   N: 'BIO_nwrite';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_nwrite0;   N: 'BIO_nwrite0';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_push ;   N: 'BIO_push';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_puts ;   N: 'BIO_puts';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_read ;   N: 'BIO_read';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_s_mem;   N: 'BIO_s_mem';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BIO_write;   N: 'BIO_write';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BN_GENCB_free ;    N: 'BN_GENCB_free';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),    { V8.40 }
    (F: @@f_BN_GENCB_get_arg ; N: 'BN_GENCB_get_arg';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX), { V8.40 }
    (F: @@f_BN_GENCB_new ;     N: 'BN_GENCB_new';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_BN_GENCB_set ;     N: 'BN_GENCB_set';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),      { V8.40 }
    (F: @@f_BN_free;           N: 'BN_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BN_new ;           N: 'BN_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_BN_set_word;       N: 'BN_set_word';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_CONF_modules_unload;   N: 'CONF_modules_unload';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_CRYPTO_THREADID_set_callback ;   N: 'CRYPTO_THREADID_set_callback';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_THREADID_set_numeric;   N: 'CRYPTO_THREADID_set_numeric';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_THREADID_set_pointer;   N: 'CRYPTO_THREADID_set_pointer';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_add_lock;   N: 'CRYPTO_add_lock';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_cleanup_all_ex_data ;   N: 'CRYPTO_cleanup_all_ex_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_free;   N: 'CRYPTO_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_CRYPTO_free_ex_data ;     N: 'CRYPTO_free_ex_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_CRYPTO_free_ex_index ;    N: 'CRYPTO_free_ex_index';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_CRYPTO_get_ex_data ;      N: 'CRYPTO_get_ex_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_CRYPTO_get_ex_new_index ; N: 'CRYPTO_get_ex_new_index';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_CRYPTO_lock;   N: 'CRYPTO_lock';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_new_ex_data ;      N: 'CRYPTO_new_ex_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_CRYPTO_num_locks ;   N: 'CRYPTO_num_locks';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_set_dynlock_create_callback ;   N: 'CRYPTO_set_dynlock_create_callback';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_set_dynlock_destroy_callback;   N: 'CRYPTO_set_dynlock_destroy_callback';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_set_dynlock_lock_callback ;   N: 'CRYPTO_set_dynlock_lock_callback';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_set_ex_data ;      N: 'CRYPTO_set_ex_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_CRYPTO_set_id_callback ;   N: 'CRYPTO_set_id_callback';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_CRYPTO_set_locking_callback;  N: 'CRYPTO_set_locking_callback';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_DH_check ;                   N: 'DH_check';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_DH_free;                     N: 'DH_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_DH_generate_parameters_ex ;  N: 'DH_generate_parameters_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_DH_new ;                     N: 'DH_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_DH_size;                     N: 'DH_size';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_DSA_free;                    N: 'DSA_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_DSA_print;                   N: 'DSA_print';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_DSA_size;                    N: 'DSA_size';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EC_GROUP_check ;                   N: 'EC_GROUP_check';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_check_discriminant ;      N: 'EC_GROUP_check_discriminant';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_clear_free ;              N: 'EC_GROUP_clear_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_cmp ;                     N: 'EC_GROUP_cmp';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_copy ;                    N: 'EC_GROUP_copy';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_dup ;                     N: 'EC_GROUP_dup';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_free ;                    N: 'EC_GROUP_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get0_cofactor ;           N: 'EC_GROUP_get0_cofactor';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get0_generator ;          N: 'EC_GROUP_get0_generator';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get0_order ;              N: 'EC_GROUP_get0_order';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get0_seed ;               N: 'EC_GROUP_get0_seed';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_asn1_flag ;           N: 'EC_GROUP_get_asn1_flag';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_basis_type ;          N: 'EC_GROUP_get_basis_type';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_cofactor ;            N: 'EC_GROUP_get_cofactor';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_curve_GF2m ;          N: 'EC_GROUP_get_curve_GF2m';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_curve_GFp ;           N: 'EC_GROUP_get_curve_GFp';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_curve_name ;          N: 'EC_GROUP_get_curve_name';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_degree ;              N: 'EC_GROUP_get_degree';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_ecparameters ;        N: 'EC_GROUP_get_ecparameters';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_ecpkparameters ;      N: 'EC_GROUP_get_ecpkparameters';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_mont_data ;           N: 'EC_GROUP_get_mont_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_order ;               N: 'EC_GROUP_get_order';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_pentanomial_basis ;   N: 'EC_GROUP_get_pentanomial_basis';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_point_conversion_form ; N: 'EC_GROUP_get_point_conversion_form';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_seed_len ;            N: 'EC_GROUP_get_seed_len';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_get_trinomial_basis ;     N: 'EC_GROUP_get_trinomial_basis';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_method_of ;               N: 'EC_GROUP_method_of';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_new ;                     N: 'EC_GROUP_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_new_by_curve_name ;       N: 'EC_GROUP_new_by_curve_name';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_new_curve_GF2m ;          N: 'EC_GROUP_new_curve_GF2m';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_new_curve_GFp ;           N: 'EC_GROUP_new_curve_GFp';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_new_from_ecparameters ;   N: 'EC_GROUP_new_from_ecparameters';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_new_from_ecpkparameters ; N: 'EC_GROUP_new_from_ecpkparameters';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_order_bits ;              N: 'EC_GROUP_order_bits';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_set_asn1_flag ;           N: 'EC_GROUP_set_asn1_flag';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_set_curve_GF2m ;          N: 'EC_GROUP_set_curve_GF2m';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_set_curve_GFp ;           N: 'EC_GROUP_set_curve_GFp';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_set_curve_name ;          N: 'EC_GROUP_set_curve_name';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_set_generator ;           N: 'EC_GROUP_set_generator';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_set_point_conversion_form ; N: 'EC_GROUP_set_point_conversion_form';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_GROUP_set_seed ;                N: 'EC_GROUP_set_seed';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_can_sign ;         N: 'EC_KEY_can_sign';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_check_key ;        N: 'EC_KEY_check_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_clear_flags ;      N: 'EC_KEY_clear_flags';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_copy ;             N: 'EC_KEY_copy';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_dup ;              N: 'EC_KEY_dup';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_free;   N: 'EC_KEY_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EC_KEY_generate_key ;     N: 'EC_KEY_generate_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_get0_group ;       N: 'EC_KEY_get0_group';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_get0_private_key ; N: 'EC_KEY_get0_private_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_get0_public_key ;  N: 'EC_KEY_get0_public_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_get_conv_form ;    N: 'EC_KEY_get_conv_form';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_get_enc_flags ;    N: 'EC_KEY_get_enc_flags';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_get_ex_data ;      N: 'EC_KEY_get_ex_data';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_get_flags ;        N: 'EC_KEY_get_flags';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_key2buf ;          N: 'EC_KEY_key2buf';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_new ;              N: 'EC_KEY_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_new_by_curve_name ;   N: 'EC_KEY_new_by_curve_name';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EC_KEY_oct2key ;          N: 'EC_KEY_oct2key';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_oct2priv ;         N: 'EC_KEY_oct2priv';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_precompute_mult ;  N: 'EC_KEY_precompute_mult';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_print ;   N: 'EC_KEY_print';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EC_KEY_priv2buf ;         N: 'EC_KEY_priv2buf';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_priv2oct ;         N: 'EC_KEY_priv2oct';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_set_asn1_flag ;    N: 'EC_KEY_set_asn1_flag';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_set_conv_form ;    N: 'EC_KEY_set_conv_form';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_set_enc_flags ;    N: 'EC_KEY_set_enc_flags';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_set_flags ;        N: 'EC_KEY_set_flags';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_set_group ;        N: 'EC_KEY_set_group';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_set_private_key ;  N: 'EC_KEY_set_private_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_set_public_key ;   N: 'EC_KEY_set_public_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_set_public_key_affine_coordinates ;   N: 'EC_KEY_set_public_key_affine_coordinates';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_KEY_up_ref ;           N: 'EC_KEY_up_ref';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_METHOD_get_field_type ; N: 'EC_METHOD_get_field_type';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_curve_nid2nist ;       N: 'EC_curve_nid2nist';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_curve_nist2nid ;       N: 'EC_curve_nist2nid';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EC_get_builtin_curves ;   N: 'EC_get_builtin_curves';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_ERR_clear_error;          N: 'ERR_clear_error';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_error_string ;        N: 'ERR_error_string';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_error_string_n ;      N: 'ERR_error_string_n';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_free_strings ;        N: 'ERR_free_strings';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_ERR_func_error_string;    N: 'ERR_func_error_string';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_get_error;            N: 'ERR_get_error';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_get_error_line_data;  N: 'ERR_get_error_line_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_lib_error_string ;    N: 'ERR_lib_error_string';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_load_crypto_strings;  N: 'ERR_load_crypto_strings';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_ERR_peek_error ;          N: 'ERR_peek_error';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_peek_last_error;      N: 'ERR_peek_last_error';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_reason_error_string;  N: 'ERR_reason_error_string';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_remove_state ;        N: 'ERR_remove_state';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ERR_remove_thread_state;  N: 'ERR_remove_thread_state';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_BytesToKey;           N: 'EVP_BytesToKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_CIPHER_CTX_cleanup;   N: 'EVP_CIPHER_CTX_cleanup';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_EVP_CIPHER_CTX_ctrl ;     N: 'EVP_CIPHER_CTX_ctrl';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_CIPHER_CTX_free;      N: 'EVP_CIPHER_CTX_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_CIPHER_CTX_init;      N: 'EVP_CIPHER_CTX_init';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_EVP_CIPHER_CTX_new;       N: 'EVP_CIPHER_CTX_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_CIPHER_CTX_reset;     N: 'EVP_CIPHER_CTX_reset';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_EVP_CIPHER_CTX_set_key_length; N: 'EVP_CIPHER_CTX_set_key_length';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_CIPHER_CTX_set_padding ;   N: 'EVP_CIPHER_CTX_set_padding';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_CipherFinal_ex;       N: 'EVP_CipherFinal_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_CipherInit_ex;        N: 'EVP_CipherInit_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_CipherUpdate;         N: 'EVP_CipherUpdate';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_DecryptFinal_ex ;     N: 'EVP_DecryptFinal_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DecryptInit_ex;       N: 'EVP_DecryptInit_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_DecryptUpdate;        N: 'EVP_DecryptUpdate';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_Digest ;              N: 'EVP_Digest';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DigestFinal ;         N: 'EVP_DigestFinal';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DigestFinal_ex ;      N: 'EVP_DigestFinal_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DigestInit ;          N: 'EVP_DigestInit';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DigestInit_ex ;       N: 'EVP_DigestInit_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DigestSignFinal ;     N: 'EVP_DigestSignFinal';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DigestSignInit ;      N: 'EVP_DigestSignInit';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DigestUpdate ;        N: 'EVP_DigestUpdate';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DigestVerifyFinal;    N: 'EVP_DigestVerifyFinal';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_DigestVerifyInit ;    N: 'EVP_DigestVerifyInit';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_EncryptFinal_ex ;     N: 'EVP_EncryptFinal_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_EncryptInit_ex;       N: 'EVP_EncryptInit_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_EncryptUpdate;        N: 'EVP_EncryptUpdate';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_MD_CTX_copy ;         N: 'EVP_MD_CTX_copy';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_CTX_copy_ex ;      N: 'EVP_MD_CTX_copy_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_CTX_free ;         N: 'EVP_MD_CTX_free';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_CTX_md ;           N: 'EVP_MD_CTX_md';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_CTX_new ;          N: 'EVP_MD_CTX_new';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_CTX_reset ;        N: 'EVP_MD_CTX_reset';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_block_size ;       N: 'EVP_MD_block_size';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_flags ;            N: 'EVP_MD_flags';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_pkey_type ;        N: 'EVP_MD_pkey_type';    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_size ;             N: 'EVP_MD_size';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_MD_type ;             N: 'EVP_MD_type';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_PKEY_assign;          N: 'EVP_PKEY_assign';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_PKEY_base_id;         N: 'EVP_PKEY_base_id';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_PKEY_bits;            N: 'EVP_PKEY_bits';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_PKEY_free;            N: 'EVP_PKEY_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_PKEY_get0;            N: 'EVP_PKEY_get0';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_PKEY_get1_DH;         N: 'EVP_PKEY_get1_DH';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_PKEY_get1_DSA;        N: 'EVP_PKEY_get1_DSA';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_PKEY_get1_EC_KEY;     N: 'EVP_PKEY_get1_EC_KEY';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_PKEY_get1_RSA;        N: 'EVP_PKEY_get1_RSA';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_PKEY_new ;            N: 'EVP_PKEY_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_PKEY_print_params ;   N: 'EVP_PKEY_print_params';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_EVP_PKEY_print_private ;  N: 'EVP_PKEY_print_private';  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_EVP_PKEY_print_public ;   N: 'EVP_PKEY_print_public';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_EVP_PKEY_size;            N: 'EVP_PKEY_size';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_SignFinal ;           N: 'EVP_SignFinal';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_EVP_VerifyFinal ;         N: 'EVP_VerifyFinal';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_128_cbc;          N: 'EVP_aes_128_cbc';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_aes_128_cfb128 ;      N: 'EVP_aes_128_cfb128';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.40 }
    (F: @@f_EVP_aes_128_ecb ;         N: 'EVP_aes_128_ecb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_128_ofb ;         N: 'EVP_aes_128_ofb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_128_gcm ;         N: 'EVP_aes_128_gcm';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_128_ocb ;         N: 'EVP_aes_128_ocb';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_128_ccm ;         N: 'EVP_aes_128_ccm';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_192_cbc ;         N: 'EVP_aes_192_cbc';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_192_cfb128 ;      N: 'EVP_aes_192_cfb128';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.40 }
    (F: @@f_EVP_aes_192_ecb ;         N: 'EVP_aes_192_ecb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_192_ofb ;         N: 'EVP_aes_192_ofb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_192_gcm ;         N: 'EVP_aes_192_gcm';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_192_ocb ;         N: 'EVP_aes_192_ocb';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_192_ccm ;         N: 'EVP_aes_192_ccm';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_256_cbc ;         N: 'EVP_aes_256_cbc';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_256_cfb128 ;      N: 'EVP_aes_256_cfb128';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.40 }
    (F: @@f_EVP_aes_256_ecb ;         N: 'EVP_aes_256_ecb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_256_ofb ;         N: 'EVP_aes_256_ofb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_256_gcm ;         N: 'EVP_aes_256_gcm';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_256_ocb ;         N: 'EVP_aes_256_ocb';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_aes_256_ccm ;         N: 'EVP_aes_256_ccm';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_bf_cbc;               N: 'EVP_bf_cbc';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_bf_cfb64;             N: 'EVP_bf_cfb64';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_bf_ecb;               N: 'EVP_bf_ecb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_bf_ofb;               N: 'EVP_bf_ofb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_chacha20 ;            N: 'EVP_chacha20';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),          { V8.40 }
    (F: @@f_EVP_chacha20_poly1305 ;   N: 'EVP_chacha20_poly1305';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX), { V8.40 }
    (F: @@f_EVP_cleanup;              N: 'EVP_cleanup';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_EVP_des_ede3_cbc ;        N: 'EVP_des_ede3_cbc';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_des_ede3_cfb64 ;      N: 'EVP_des_ede3_cfb64';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.40 }
    (F: @@f_EVP_des_ede3_ecb ;        N: 'EVP_des_ede3_ecb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_des_ede3_ofb ;        N: 'EVP_des_ede3_ofb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_enc_null ;            N: 'EVP_enc_null';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),       { V8.40 }
    (F: @@f_EVP_get_cipherbyname ;    N: 'EVP_get_cipherbyname';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_get_digestbyname ;    N: 'EVP_get_digestbyname';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_idea_cbc ;            N: 'EVP_idea_cbc';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_idea_cfb64 ;          N: 'EVP_idea_cfb64';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.40 }
    (F: @@f_EVP_idea_ecb ;            N: 'EVP_idea_ecb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_idea_ofb ;            N: 'EVP_idea_ofb';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_EVP_md5;                  N: 'EVP_md5';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_mdc2 ;                N: 'EVP_mdc2';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),       { V8.40 }
    (F: @@f_EVP_ripemd160 ;           N: 'EVP_ripemd160';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_EVP_sha1 ;                N: 'EVP_sha1';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_sha224 ;              N: 'EVP_sha224';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_EVP_sha256 ;              N: 'EVP_sha256';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_EVP_sha384 ;              N: 'EVP_sha384';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_EVP_sha512 ;              N: 'EVP_sha512';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_GENERAL_NAME_free ;       N: 'GENERAL_NAME_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),        { V8.40 }
    (F: @@f_GENERAL_NAME_get0_value ; N: 'GENERAL_NAME_get0_value';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_GENERAL_NAME_new ;        N: 'GENERAL_NAME_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),          { V8.40 }
    (F: @@f_GENERAL_NAME_set0_value ; N: 'GENERAL_NAME_set0_value';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_HMAC  ;        N: 'HMAC';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_OBJ_nid2ln ;   N: 'OBJ_nid2ln';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_OBJ_nid2obj ;  N: 'OBJ_nid2obj';  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),              { V8.40 }
    (F: @@f_OBJ_nid2sn ;   N: 'OBJ_nid2sn';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_OBJ_obj2nid;   N: 'OBJ_obj2nid';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_OpenSSL_version ;   N: 'OpenSSL_version';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_PEM_X509_INFO_read_bio ;   N: 'PEM_X509_INFO_read_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_do_header;   N: 'PEM_do_header';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_read_bio_DHparams;   N: 'PEM_read_bio_DHparams';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_read_bio_PKCS7 ;   N: 'PEM_read_bio_PKCS7';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_read_bio_PrivateKey;   N: 'PEM_read_bio_PrivateKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_read_bio_PUBKEY ;  N: 'PEM_read_bio_PUBKEY';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_PEM_read_bio_RSAPrivateKey ;   N: 'PEM_read_bio_RSAPrivateKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_read_bio_RSAPublicKey ;  N: 'PEM_read_bio_RSAPublicKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_PEM_read_bio_RSA_PUBKEY;   N: 'PEM_read_bio_RSA_PUBKEY';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_read_bio_X509;   N: 'PEM_read_bio_X509';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_read_bio_X509_CRL;   N: 'PEM_read_bio_X509_CRL';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_read_bio_X509_REQ ;  N: 'PEM_read_bio_X509_REQ';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_PEM_write_bio_DHparams ;  N: 'PEM_write_bio_DHparams';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_PEM_write_bio_PKCS7;   N: 'PEM_write_bio_PKCS7';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_write_bio_PKCS8PrivateKey ;  N: 'PEM_write_bio_PKCS8PrivateKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_PEM_write_bio_PrivateKey ;   N: 'PEM_write_bio_PrivateKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_write_bio_PUBKEY ;   N: 'PEM_write_bio_PUBKEY';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_PEM_write_bio_RSA_PUBKEY ;  N: 'PEM_write_bio_RSA_PUBKEY';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_PEM_write_bio_RSAPrivateKey;   N: 'PEM_write_bio_RSAPrivateKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_write_bio_RSAPublicKey ;   N: 'PEM_write_bio_RSAPublicKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_write_bio_X509 ;   N: 'PEM_write_bio_X509';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_write_bio_X509_CRL ;   N: 'PEM_write_bio_X509_CRL';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PEM_write_bio_X509_REQ ;   N: 'PEM_write_bio_X509_REQ';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PKCS12_create;   N: 'PKCS12_create';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PKCS12_free;   N: 'PKCS12_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PKCS12_parse ;   N: 'PKCS12_parse';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PKCS12_verify_mac;   N: 'PKCS12_verify_mac';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PKCS7_SIGNED_new;   N: 'PKCS7_SIGNED_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),        { V8.40 }
    (F: @@f_PKCS7_add_certificate;   N: 'PKCS7_add_certificate';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PKCS7_content_new;   N: 'PKCS7_content_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PKCS7_free ;   N: 'PKCS7_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PKCS7_new;   N: 'PKCS7_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_PKCS7_set_type ;   N: 'PKCS7_set_type';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RAND_add ;   N: 'RAND_add';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RAND_bytes;   N: 'RAND_bytes';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RAND_cleanup;   N: 'RAND_cleanup';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_RAND_load_file;   N: 'RAND_load_file';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RAND_poll;   N: 'RAND_poll';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RAND_screen;   N: 'RAND_screen';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RAND_seed;   N: 'RAND_seed';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RAND_status;   N: 'RAND_status';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RAND_write_file;   N: 'RAND_write_file';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RSA_free;   N: 'RSA_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RSA_generate_key ;   N: 'RSA_generate_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RSA_generate_key_ex;   N: 'RSA_generate_key_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RSA_new;   N: 'RSA_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RSA_print;   N: 'RSA_print';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RSA_private_decrypt;   N: 'RSA_private_decrypt';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RSA_public_encrypt;   N: 'RSA_public_encrypt';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_RSA_size;   N: 'RSA_size';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_OpenSSL_version ;   N: 'SSLeay_version';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_X509V3_EXT_conf_nid;   N: 'X509V3_EXT_conf_nid';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509V3_EXT_d2i ;   N: 'X509V3_EXT_d2i';       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509V3_EXT_get ;   N: 'X509V3_EXT_get';       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509V3_EXT_print ;   N: 'X509V3_EXT_print';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509V3_add1_i2d;   N: 'X509V3_add1_i2d';      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),         { V8.41 }
    (F: @@f_X509V3_conf_free ;   N: 'X509V3_conf_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_CRL_dup ;   N: 'X509_CRL_dup';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_CRL_free;   N: 'X509_CRL_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_CRL_sign ;           N: 'X509_CRL_sign';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),       { V8.40 }
    (F: @@f_X509_CRL_sign_ctx ;       N: 'X509_CRL_sign_ctx';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_X509_CRL_verify ;         N: 'X509_CRL_verify';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.40 }
    (F: @@f_X509_EXTENSION_free;   N: 'X509_EXTENSION_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_EXTENSION_get_critical;   N: 'X509_EXTENSION_get_critical';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_EXTENSION_get_data;   N: 'X509_EXTENSION_get_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_EXTENSION_get_object;   N: 'X509_EXTENSION_get_object';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_INFO_free ;   N: 'X509_INFO_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_LOOKUP_by_fingerprint ;   N: 'X509_LOOKUP_by_fingerprint';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_LOOKUP_by_issuer_serial ;   N: 'X509_LOOKUP_by_issuer_serial';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_LOOKUP_ctrl ;   N: 'X509_LOOKUP_ctrl';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_LOOKUP_file ;   N: 'X509_LOOKUP_file';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_LOOKUP_free ;   N: 'X509_LOOKUP_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_LOOKUP_hash_dir ;   N: 'X509_LOOKUP_hash_dir';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_LOOKUP_new;   N: 'X509_LOOKUP_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_ENTRY_get_data ;   N: 'X509_NAME_ENTRY_get_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_ENTRY_get_object ; N: 'X509_NAME_ENTRY_get_object';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_add_entry_by_NID ; N: 'X509_NAME_add_entry_by_NID';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_add_entry_by_txt ; N: 'X509_NAME_add_entry_by_txt';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_cmp;               N: 'X509_NAME_cmp';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_dup;               N: 'X509_NAME_dup';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),                { V8.40 }
    (F: @@f_X509_NAME_entry_count;       N: 'X509_NAME_entry_count';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_free ;             N: 'X509_NAME_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_get_entry;         N: 'X509_NAME_get_entry';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_get_index_by_NID ; N: 'X509_NAME_get_index_by_NID';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_get_text_by_NID;  N: 'X509_NAME_get_text_by_NID';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_new;              N: 'X509_NAME_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_oneline;          N: 'X509_NAME_oneline';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_NAME_print_ex;         N: 'X509_NAME_print_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_X509_OBJECT_get_type;       N: 'X509_OBJECT_get_type';       MI: OSSL_VER_1100; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_OBJECT_get0_X509;      N: 'X509_OBJECT_get0_X509';      MI: OSSL_VER_1100; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_OBJECT_get0_X509_CRL;  N: 'X509_OBJECT_get0_X509_CRL';  MI: OSSL_VER_1100; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_PKEY_free ;            N: 'X509_PKEY_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_PUBKEY_free ;          N: 'X509_PUBKEY_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_PUBKEY_free;           N: 'X509_PUBKEY_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_PURPOSE_get0;          N: 'X509_PURPOSE_get0';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_PURPOSE_get0_name ;    N: 'X509_PURPOSE_get0_name';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_PURPOSE_get0_sname;    N: 'X509_PURPOSE_get0_sname';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_PURPOSE_get_count ;    N: 'X509_PURPOSE_get_count';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_PURPOSE_get_id;        N: 'X509_PURPOSE_get_id';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_REQ_add_extensions;    N: 'X509_REQ_add_extensions';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_REQ_dup;               N: 'X509_REQ_dup';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_REQ_free;              N: 'X509_REQ_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_REQ_get0_pubkey;       N: 'X509_REQ_get0_pubkey';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),           { V8.40 }
    (F: @@f_X509_REQ_get_extensions;    N: 'X509_REQ_get_extensions';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),         { V8.41 }
    (F: @@f_X509_REQ_get_pubkey;        N: 'X509_REQ_get_pubkey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),             { V8.40 }
    (F: @@f_X509_REQ_get_subject_name;  N: 'X509_REQ_get_subject_name';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.36 }
    (F: @@f_X509_REQ_new;               N: 'X509_REQ_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_REQ_print ;            N: 'X509_REQ_print';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),          { V8.40 }
    (F: @@f_X509_REQ_print_ex ;         N: 'X509_REQ_print_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),       { V8.40 }
    (F: @@f_X509_REQ_set_pubkey;        N: 'X509_REQ_set_pubkey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_REQ_set_version;       N: 'X509_REQ_set_version';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_REQ_sign;              N: 'X509_REQ_sign';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_REQ_sign_ctx ;         N: 'X509_REQ_sign_ctx';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_X509_REQ_to_X509 ;          N: 'X509_REQ_to_X509';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_X509_REQ_verify ;           N: 'X509_REQ_verify';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_X509_STORE_CTX_cleanup ;         N: 'X509_STORE_CTX_cleanup';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_free;             N: 'X509_STORE_CTX_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_get_chain ;       N: 'X509_STORE_CTX_get_chain';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_X509_STORE_CTX_get_current_cert; N: 'X509_STORE_CTX_get_current_cert';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_get_error ;       N: 'X509_STORE_CTX_get_error';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_get_error_depth ; N: 'X509_STORE_CTX_get_error_depth';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_get_ex_data ;     N: 'X509_STORE_CTX_get_ex_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_init;             N: 'X509_STORE_CTX_init';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_new ;             N: 'X509_STORE_CTX_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_set_error ;       N: 'X509_STORE_CTX_set_error';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_set_ex_data ;     N: 'X509_STORE_CTX_set_ex_data';      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_set_purpose ;     N: 'X509_STORE_CTX_set_purpose';      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_set_verify_cb ;   N: 'X509_STORE_CTX_set_verify_cb';    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_CTX_trusted_stack ;   N: 'X509_STORE_CTX_trusted_stack';    MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_X509_STORE_add_cert;             N: 'X509_STORE_add_cert';             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_add_crl ;             N: 'X509_STORE_add_crl';              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_add_lookup ;          N: 'X509_STORE_add_lookup';           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_get0_objects ;        N: 'X509_STORE_get0_objects';         MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.39 }
    (F: @@f_X509_STORE_get0_param ;          N: 'X509_STORE_get0_param';           MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.39 }
    (F: @@f_X509_STORE_set1_param ;          N: 'X509_STORE_set1_param';           MI: OSSL_VER_1100; MX: OSSL_VER_MAX),  { V8.39 }
    (F: @@f_X509_STORE_free;                 N: 'X509_STORE_free';                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_new ;                 N: 'X509_STORE_new';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_STORE_set_flags ;           N: 'X509_STORE_set_flags';            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_VERIFY_PARAM_add0_policy;   N: 'X509_VERIFY_PARAM_add0_policy';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_add0_table;    N: 'X509_VERIFY_PARAM_add0_table';    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_add1_host;     N: 'X509_VERIFY_PARAM_add1_host';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_clear_flags;   N: 'X509_VERIFY_PARAM_clear_flags';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_free  ;        N: 'X509_VERIFY_PARAM_free';          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_get_flags;     N: 'X509_VERIFY_PARAM_get_flags';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_get_depth;     N: 'X509_VERIFY_PARAM_get_depth';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_get_auth_level;N: 'X509_VERIFY_PARAM_get_auth_level'; MI: OSSL_VER_1100; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_get_count;     N: 'X509_VERIFY_PARAM_get_count';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_get0;          N: 'X509_VERIFY_PARAM_get0';          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_get0_name;     N: 'X509_VERIFY_PARAM_get0_name';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_get0_peername; N: 'X509_VERIFY_PARAM_get0_peername'; MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_inherit;       N: 'X509_VERIFY_PARAM_inherit';       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_lookup;        N: 'X509_VERIFY_PARAM_lookup';        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_move_peername; N: 'X509_VERIFY_PARAM_move_peername'; MI: OSSL_VER_1100; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_new ;          N: 'X509_VERIFY_PARAM_new';           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set1;          N: 'X509_VERIFY_PARAM_set1';          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set1_name;     N: 'X509_VERIFY_PARAM_set1_name';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set_flags;     N: 'X509_VERIFY_PARAM_set_flags';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set_purpose;   N: 'X509_VERIFY_PARAM_set_purpose';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set_trust;     N: 'X509_VERIFY_PARAM_set_trust';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set_depth;     N: 'X509_VERIFY_PARAM_set_depth';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set_auth_level;N: 'X509_VERIFY_PARAM_set_auth_level'; MI: OSSL_VER_1100; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set_time;      N: 'X509_VERIFY_PARAM_set_time';      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set1_policies; N: 'X509_VERIFY_PARAM_set1_policies'; MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set1_host;     N: 'X509_VERIFY_PARAM_set1_host';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set_hostflags; N: 'X509_VERIFY_PARAM_set_hostflags'; MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set1_email;    N: 'X509_VERIFY_PARAM_set1_email';    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set1_ip;       N: 'X509_VERIFY_PARAM_set1_ip';       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_set1_ip_asc;   N: 'X509_VERIFY_PARAM_set1_ip_asc';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_VERIFY_PARAM_table_cleanup; N: 'X509_VERIFY_PARAM_table_cleanup'; MI: OSSL_VER_MIN; MX: OSSL_VER_MAX), { V8.39 }
    (F: @@f_X509_add_ext ;             N: 'X509_add_ext';       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_add1_ext_i2d ;        N: 'X509_add1_ext_i2d';  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),      { V8.40 }
    (F: @@f_X509_check_ca;             N: 'X509_check_ca';      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_check_email;          N: 'X509_check_email';   MI: OSSL_VER_1002; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@f_X509_check_host;           N: 'X509_check_host';    MI: OSSL_VER_1002; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@f_X509_check_ip;             N: 'X509_check_ip';      MI: OSSL_VER_1002; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@f_X509_check_ip_asc;         N: 'X509_check_ip_asc';  MI: OSSL_VER_1002; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@f_X509_check_issued;         N: 'X509_check_issued';  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_check_private_key ;   N: 'X509_check_private_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_check_purpose ;       N: 'X509_check_purpose';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_digest;               N: 'X509_digest';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_dup ;                 N: 'X509_dup';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_free;                 N: 'X509_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_get0_notAfter ;       N: 'X509_get0_notAfter';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_X509_get0_notBefore;       N: 'X509_get0_notBefore';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_X509_get0_pubkey;          N: 'X509_get0_pubkey';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),           { V8.40 }
    (F: @@f_X509_get_ext ;             N: 'X509_get_ext';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_get_ext_d2i ;         N: 'X509_get_ext_d2i';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),      { V8.40 }
    (F: @@f_X509_get_ext_count ;       N: 'X509_get_ext_count';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_get_issuer_name ;     N: 'X509_get_issuer_name';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_get_pubkey;           N: 'X509_get_pubkey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),//AG
    (F: @@f_X509_get_serialNumber;     N: 'X509_get_serialNumber';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_get_signature_nid ;   N: 'X509_get_signature_nid';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_X509_get_subject_name;     N: 'X509_get_subject_name';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_gmtime_adj;           N: 'X509_gmtime_adj';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_load_crl_file ;       N: 'X509_load_crl_file';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_new ;                 N: 'X509_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_print ;               N: 'X509_print';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_print_ex ;            N: 'X509_print_ex';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),       { V8.40 }
    (F: @@f_X509_set_issuer_name ;     N: 'X509_set_issuer_name';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_set_pubkey;           N: 'X509_set_pubkey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_set_subject_name ;    N: 'X509_set_subject_name';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@f_X509_set_version ;         N: 'X509_set_version';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_sign;                 N: 'X509_sign';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_sign_ctx ;            N: 'X509_sign_ctx';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_X509_subject_name_hash ;   N: 'X509_subject_name_hash';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_to_X509_REQ ;         N: 'X509_to_X509_REQ';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_X509_verify ;              N: 'X509_verify';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_X509_verify_cert ;         N: 'X509_verify_cert';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_X509_verify_cert_error_string;   N: 'X509_verify_cert_error_string';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_d2i_PKCS12_bio ;           N: 'd2i_PKCS12_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_d2i_PKCS7_bio;             N: 'd2i_PKCS7_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_d2i_PKCS8PrivateKey_bio;   N: 'd2i_PKCS8PrivateKey_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_d2i_PrivateKey ;           N: 'd2i_PrivateKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_d2i_PrivateKey_bio ;       N: 'd2i_PrivateKey_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_d2i_RSAPrivateKey;         N: 'd2i_RSAPrivateKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_d2i_X509 ;                 N: 'd2i_X509';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_d2i_X509_REQ ;             N: 'd2i_X509_REQ';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),       { V8.40 }
    (F: @@f_d2i_X509_REQ_bio ;         N: 'd2i_X509_REQ_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_d2i_X509_bio ;             N: 'd2i_X509_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_i2a_ASN1_OBJECT;           N: 'i2a_ASN1_OBJECT';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_i2d_PKCS12_bio ;           N: 'i2d_PKCS12_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_i2d_PKCS7_bio ;            N: 'i2d_PKCS7_bio';    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.41 }
    (F: @@f_i2d_PrivateKey ;           N: 'i2d_PrivateKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_i2d_PrivateKey_bio ;       N: 'i2d_PrivateKey_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_i2d_RSAPublicKey ;         N: 'i2d_RSAPublicKey';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_i2d_RSA_PUBKEY ;           N: 'i2d_RSA_PUBKEY';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_i2d_X509 ;                 N: 'i2d_X509';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_i2d_X509_REQ ;             N: 'i2d_X509_REQ';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),       { V8.40 }
    (F: @@f_i2d_X509_REQ_bio ;         N: 'i2d_X509_REQ_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@f_i2d_X509_bio ;             N: 'i2d_X509_bio';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_delete;   N: 'OPENSSL_sk_delete';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_dup ;     N: 'OPENSSL_sk_dup';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_find;     N: 'OPENSSL_sk_find';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_free;     N: 'OPENSSL_sk_free';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_insert;   N: 'OPENSSL_sk_insert';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_new_null; N: 'OPENSSL_sk_new_null';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_num ;     N: 'OPENSSL_sk_num';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_pop ;     N: 'OPENSSL_sk_pop';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_pop_free; N: 'OPENSSL_sk_pop_free';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_push;     N: 'OPENSSL_sk_push';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_set ;     N: 'OPENSSL_sk_set';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_value ;   N: 'OPENSSL_sk_value';   MI: OSSL_VER_1100; MX: OSSL_VER_MAX),
    (F: @@f_OPENSSL_sk_delete;   N: 'sk_delete';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_dup ;     N: 'sk_dup';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_find;     N: 'sk_find';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_free;     N: 'sk_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_insert;   N: 'sk_insert';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_new_null; N: 'sk_new_null';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_num ;     N: 'sk_num';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_pop ;     N: 'sk_pop';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_pop_free; N: 'sk_pop_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_push;     N: 'sk_push';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_set ;     N: 'sk_set';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_OPENSSL_sk_value ;   N: 'sk_value';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ) ) ;

{$IFDEF OPENSSL_USE_DELPHI_MM}
    GLIBEAYImports2: array[0..0] of TOSSLImports = (
    (F: @@f_CRYPTO_set_mem_functions ;   N: 'CRYPTO_set_mem_functions';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX) ) ;
{$ENDIF}

{$IFNDEF OPENSSL_NO_ENGINE}
    GLIBEAYImports3: array[0..21] of TOSSLImports = (
    (F: @@f_ENGINE_by_id ;   N: 'ENGINE_by_id';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_cleanup ;   N: 'ENGINE_cleanup';   MI: OSSL_VER_MIN; MX: OSSL_VER_1002ZZ),
    (F: @@f_ENGINE_ctrl_cmd_string ;   N: 'ENGINE_ctrl_cmd_string';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_finish;   N: 'ENGINE_finish';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_free;   N: 'ENGINE_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_init;   N: 'ENGINE_init';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_load_builtin_engines;   N: 'ENGINE_load_builtin_engines';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_load_private_key;   N: 'ENGINE_load_private_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_load_public_key ;   N: 'ENGINE_load_public_key';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_load_ssl_client_cert;   N: 'ENGINE_load_ssl_client_cert';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_register_all_complete ;   N: 'ENGINE_register_all_complete';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_ENGINE_set_default ;   N: 'ENGINE_set_default';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_OpenSSL ;   N: 'UI_OpenSSL';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_create_method ;   N: 'UI_create_method';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_destroy_method;   N: 'UI_destroy_method';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_free;   N: 'UI_free';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_get_ex_data ;   N: 'UI_get_ex_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_method_set_reader ;   N: 'UI_method_set_reader';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_new ;   N: 'UI_new';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_new_method;   N: 'UI_new_method';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_set_ex_data ;   N: 'UI_set_ex_data';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@f_UI_set_result;   N: 'UI_set_result';   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX) ) ;
{$ENDIF}

{$ENDIF} // USE_SSL
implementation

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsMalloc(Size: size_t): Pointer; cdecl;
begin
    GetMem(Result, Size);
    FillChar(Result^, Size, 0);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsRealloc(P: Pointer; Size: size_t): Pointer; cdecl;
begin
    Result := P;
    ReallocMem(Result, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsFreeMem(P: Pointer); cdecl;
begin
    FreeMem(P);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsRandSeedFromFile(const FileName: String; MaxBytes: Integer = -1): Integer;
begin
    Result := 0;
    if FileExists(FileName) then
    begin
        if MaxBytes < -1 then MaxBytes := -1;
        Result := f_RAND_load_file(PAnsiChar(AnsiString(FileName)), MaxBytes);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}  { V8.04 }
procedure IcsRandPoll;
var
    ProcIDs, P : PDWORD;
    cbNeeded, cb, hProc, PID : DWORD;
    I : Integer;
    hPsApi : THandle;
    ProcMemCnt : TProcessMemoryCounters;
begin
    f_RAND_Poll; // Uses Tool Help32 Functions beside other methods to seed the RNG
    // depending on the windows version.
    // if PSAPI.dll is available I think we should add some additional seed
    hPsApi := LoadLibrary('PSAPI.dll');
    if hPsApi < 32 then Exit;
    FreeLibrary(hPsApi);

    cb := 1024;
    GetMem(ProcIDs, cb);
    try
        while True do
        begin
            if not EnumProcesses(ProcIDs, cb, cbNeeded) then
                RaiseLastOSError;
            if cbNeeded < cb then
                Break;
            Inc(cb, 1024);
            FreeMem(ProcIDs);
            GetMem(ProcIDs, cb);
        end;
        P := ProcIDs;
        for I := 1 to cbNeeded div SizeOf(DWORD) do
        begin
            PID := P^;
            Inc(P);
            hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, PID);
            if hProc <> 0 then
            try
                if GetProcessMemoryInfo(hProc, @ProcMemCnt, SizeOf(ProcMemCnt)) then
                    f_RAND_seed(@ProcMemCnt.PageFaultCount, SizeOf(ProcMemCnt.PageFaultCount));
            finally
                CloseHandle(hProc);
            end;
        end;
    finally
        FreeMem(ProcIDs);
    end;
    ICS_RAND_INIT_DONE := true;  { V8.35 can check this to avoid calling repeatedly }
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LibeayLoad : Boolean;        { V8.27 make unique }
var
    ErrCode : Integer;
    FullName, errs: String ;  { V8.29 }
begin
    Result := TRUE;
    if GLIBEAY_DLL_Handle <> 0 then Exit;  // Already loaded
    ICS_OPENSSL_VERSION_NUMBER := 0;

{ V8.27 try v1.1.0 and later first with new file name - libcrypto-1_1.dll }
  { V8.27 allow a specific DLL directory to be specified in GSSL_DLL_DIR }
    if NOT GSSLEAY_DLL_IgnoreNew then begin
        FullName := GSSL_DLL_DIR+GLIBEAY_110DLL_Name;  { V8.29 }
        GLIBEAY_DLL_Handle := LoadLibrary(PChar(FullName));
    end;
    if GLIBEAY_DLL_Handle {$IFDEF POSIX} > 0 {$ELSE} > HINSTANCE_ERROR {$ENDIF} then begin
        SetLength(GLIBEAY_DLL_FileName, 256);
        SetLength(GLIBEAY_DLL_FileName, GetModuleFileName(GLIBEAY_DLL_Handle,
                 PChar(GLIBEAY_DLL_FileName), Length(GLIBEAY_DLL_FileName)));

        // all SSLeay_xx functions renamed to OpenSSL_xx with v1.1.0 and later
        f_OpenSSL_version_num := GetProcAddress(GLIBEAY_DLL_Handle, FN_OpenSSL_version_num);
        if @f_OpenSSL_version_num = nil then begin
            Result := False;
            Exit;
        end;
  //      f_SSLeay := @f_OpenSSL_version_num;  // keep old version for compability
        ICS_OPENSSL_VERSION_NUMBER := f_OpenSSL_version_num;
    end
    else begin
        if GSSLEAY_DLL_IgnoreOld then begin   { V8.34 }
            Result := False;
            Exit;
        end;
{ now try old file name - libeay32.dll }
        FullName := GSSL_DLL_DIR+GLIBEAY_DLL_Name;  { V8.29 }
        GLIBEAY_DLL_Handle := LoadLibrary(PChar(FullName));
        if GLIBEAY_DLL_Handle {$IFDEF POSIX} = 0 {$ELSE} < HINSTANCE_ERROR {$ENDIF} then begin
            ErrCode            := GLIBEAY_DLL_Handle;
            GLIBEAY_DLL_Handle := 0;
            raise EIcsLIBEAYException.Create('Unable to load ' + FullName +
                             '. Error #' + IntToStr(ErrCode) + #13#10 + SysErrorMessage(GetLastError));
        end;
        SetLength(GLIBEAY_DLL_FileName, 256);
        SetLength(GLIBEAY_DLL_FileName, GetModuleFileName(GLIBEAY_DLL_Handle,
                     PChar(GLIBEAY_DLL_FileName), Length(GLIBEAY_DLL_FileName)));

        //This function is available in all versions so we can safely call it
        f_OpenSSL_version_num := GetProcAddress(GLIBEAY_DLL_Handle, FN_SSLeay);
        if @f_OpenSSL_version_num = nil then begin
            Result := False;
            Exit;
        end;
        ICS_OPENSSL_VERSION_NUMBER := f_OpenSSL_version_num;
    end;
    { Version Check }
{$IFNDEF NO_OSSL_VERSION_CHECK}
    if (ICS_OPENSSL_VERSION_NUMBER < MIN_OSSL_VER) or
             (ICS_OPENSSL_VERSION_NUMBER > MAX_OSSL_VER) then begin
        FreeLibrary({OverbyteIcsLIBEAY.}GLIBEAY_DLL_Handle);
        {OverbyteIcsLIBEAY.}GLIBEAY_DLL_Handle := 0;
        raise EIcsLibeayException.Create(
                  'Unsupported OpenSSL version (0x' +
                  IntToHex(ICS_OPENSSL_VERSION_NUMBER, 8) + ') !'#13#10 +
                  'Supported versions are 0x' +
                  IntToHex(MIN_OSSL_VER, 8) +
                  ' - 0x' + IntToHex(MAX_OSSL_VER, 8) + #13#10 +
                  'FileName: ' + GLIBEAY_DLL_FileName);
    end;
{$ENDIF}
    { Let's set some values of constants having changed in v0.9.8 }
//       SSL_CTRL_EXTRA_CHAIN_CERT    := 14;                     // Ssl.h   V8.07 now set as literal
//       SSL_CTRL_GET_SESSION_REUSED  :=  8;                     // Ssl.h   V8.07 now set as literal
        MBSTRING_UNIV                := MBSTRING_FLAG or 4;     // Asn1.h
        MBSTRING_UTF8                := MBSTRING_FLAG;          // Asn1.h

  {$IFDEF MSWINDOWS}
   { V8.38 check authenticode digital signature on DLL }
    if GSSL_SignTest_Check then IcsVerifySslDll (GLIBEAY_DLL_FileName);
  {$ENDIF}

  { V8.35 load all main GLIBEAY_DLL exports }
    errs := SslGetImports (GLIBEAY_DLL_Handle, GLIBEAYImports1) ;
    if errs <> '' then
        raise  EIcsSsleayException.Create('Unable to load ' + FullName + '. Can not find: ' + errs);

{$IFDEF OPENSSL_USE_DELPHI_MM}
 { V8.35 load memory manager GLIBEAY_DLL exports }
    errs := SslGetImports (GLIBEAY_DLL_Handle, GLIBEAYImports2) ;
    if errs <> '' then
        raise  EIcsSsleayException.Create('Unable to load ' + FullName + '. Can not find: ' + errs);
{$ENDIF}

{$IFNDEF OPENSSL_NO_ENGINE}
  { V8.35 load engine GLIBEAY_DLL exports }
    errs := SslGetImports (GLIBEAY_DLL_Handle, GLIBEAYImports3) ;
    if errs <> '' then
        raise  EIcsSsleayException.Create('Unable to load ' + FullName + '. Can not find: ' + errs);
{$ENDIF}

 { handle some backward compatible stuff }
    if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_1100 then begin
        @f_EVP_CIPHER_CTX_init       := @f_EVP_CIPHER_CTX_reset;
        // pending, should create duplicates OPENSSL_sk_xx for sk_xx
        @f_CRYPTO_lock                            := @IcsSslStub;  { V8.35 }
        @f_CRYPTO_add_lock                        := @IcsSslStub;  { V8.35 }
        @f_CRYPTO_num_locks                       := @IcsSslStub;  { V8.35 }
        @f_CRYPTO_set_locking_callback            := @IcsSslStub;  { V8.35 }
        @f_CRYPTO_set_id_callback                 := @IcsSslStub;  { V8.35 }
        @f_CRYPTO_set_dynlock_create_callback     := @IcsSslStub;  { V8.35 }
        @f_CRYPTO_set_dynlock_lock_callback       := @IcsSslStub;  { V8.35 }
        @f_CRYPTO_set_dynlock_destroy_callback    := @IcsSslStub;  { V8.35 }
        @f_ERR_free_strings                       := @IcsSslStub;  { V8.35 }
        @f_CRYPTO_cleanup_all_ex_data             := @IcsSslStub;  { V8.35 }
        @f_X509_STORE_CTX_get_chain               := @IcsSslStub;  { V8.35 }
        @f_X509_STORE_CTX_trusted_stack           := @IcsSslStub;  { V8.35 }
        @f_EVP_CIPHER_CTX_cleanup                 := @IcsSslStub;  { V8.35 }
        @f_EVP_cleanup                            := @IcsSslStub;  { V8.35 }
        @f_ENGINE_cleanup                         := @IcsSslStub;  { V8.35 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsX509VerifyErrorToStr(ErrCode: Integer): String;
begin
{$IFNDEF OPENSSL_USE_RESOURCE_STRINGS}
   Result := String(AnsiString(f_X509_verify_cert_error_string(ErrCode)));
{$ELSE}
    case ErrCode of
        X509_V_OK :
            Result := sX509_V_OK;
        X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
            Result := sX509_V_ERR_UNABLE_TO_GET_ISSUER_CERT;
        X509_V_ERR_UNABLE_TO_GET_CRL:
            Result := sX509_V_ERR_UNABLE_TO_GET_CRL;
        X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE:
            Result := sX509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE;
        X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE:
            Result := sX509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE;
        X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY:
            Result := sX509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY;
        X509_V_ERR_CERT_SIGNATURE_FAILURE:
            Result := sX509_V_ERR_CERT_SIGNATURE_FAILURE;
        X509_V_ERR_CRL_SIGNATURE_FAILURE:
            Result := sX509_V_ERR_CRL_SIGNATURE_FAILURE;
        X509_V_ERR_CERT_NOT_YET_VALID:
            Result := sX509_V_ERR_CERT_NOT_YET_VALID;
        X509_V_ERR_CRL_NOT_YET_VALID:
            Result := sX509_V_ERR_CRL_NOT_YET_VALID;
        X509_V_ERR_CERT_HAS_EXPIRED:
            Result := sX509_V_ERR_CERT_HAS_EXPIRED;
        X509_V_ERR_CRL_HAS_EXPIRED:
            Result := sX509_V_ERR_CRL_HAS_EXPIRED;
        X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
            Result := sX509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD;
        X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
            Result := sX509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD;
        X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD:
            Result := sX509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD;
        X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD:
            Result := sX509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD;
        X509_V_ERR_OUT_OF_MEM:
            Result := sX509_V_ERR_OUT_OF_MEM;
        X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
            Result := sX509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT;
        X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
            Result := sX509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN;
        X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
            Result := sX509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY;
        X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE:
            Result := sX509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE;
        X509_V_ERR_CERT_CHAIN_TOO_LONG:
            Result := sX509_V_ERR_CERT_CHAIN_TOO_LONG;
        X509_V_ERR_CERT_REVOKED:
            Result := sX509_V_ERR_CERT_REVOKED;
        X509_V_ERR_INVALID_CA:
            Result := sX509_V_ERR_INVALID_CA;
        X509_V_ERR_INVALID_NON_CA:
            Result := sX509_V_ERR_INVALID_NON_CA;
        X509_V_ERR_PATH_LENGTH_EXCEEDED:
            Result := sX509_V_ERR_PATH_LENGTH_EXCEEDED;
        X509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED:
            Result := sX509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED;
        X509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED:
            Result := sX509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED;
        X509_V_ERR_INVALID_PURPOSE:
            Result := sX509_V_ERR_INVALID_PURPOSE;
        X509_V_ERR_CERT_UNTRUSTED:
            Result := sX509_V_ERR_CERT_UNTRUSTED;
        X509_V_ERR_CERT_REJECTED:
            Result := sX509_V_ERR_CERT_REJECTED;
        X509_V_ERR_APPLICATION_VERIFICATION:
            Result := sX509_V_ERR_APPLICATION_VERIFICATION;
        X509_V_ERR_SUBJECT_ISSUER_MISMATCH:
            Result := sX509_V_ERR_SUBJECT_ISSUER_MISMATCH;
        X509_V_ERR_AKID_SKID_MISMATCH:
            Result := sX509_V_ERR_AKID_SKID_MISMATCH;
        X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH:
            Result := sX509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH;
        X509_V_ERR_KEYUSAGE_NO_CERTSIGN:
            Result := sX509_V_ERR_KEYUSAGE_NO_CERTSIGN;
        X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER:
            Result := sX509_V_ERR_UNABLE_TO_GET_CRL_ISSUER;
        X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION:
            Result := sX509_V_ERR_UNHANDLED_CRITICAL_EXTENSION;
        X509_V_ERR_KEYUSAGE_NO_CRL_SIGN:
            Result := sX509_V_ERR_KEYUSAGE_NO_CRL_SIGN;
        X509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE:
            Result := sX509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE;
        X509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION:
            Result := sX509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION;
        X509_V_ERR_INVALID_EXTENSION:
            Result := sX509_V_ERR_INVALID_EXTENSION;
        X509_V_ERR_INVALID_POLICY_EXTENSION:
            Result := sX509_V_ERR_INVALID_POLICY_EXTENSION;
        X509_V_ERR_NO_EXPLICIT_POLICY:
            Result := sX509_V_ERR_NO_EXPLICIT_POLICY;
     { V8.39 lots more }
     xx
        X509_V_ERR_UNNESTED_RESOURCE:
            Result := sX509_V_ERR_UNNESTED_RESOURCE;

    else
        Result := sX509_V_ERR_NUMBER + IntToStr(ErrCode);
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_CRL_get_issuer(crl: PX509_CRL): PX509_NAME;
begin
    Result := crl^.crl^.issuer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_get_version(X509: PX509): Integer;
begin
    Result := f_ASN1_INTEGER_get(X509^.cert_info^.version);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_get_signature_algorithm(X509: PX509): Integer;
begin
    Result := f_OBJ_obj2nid(X509^.sig_alg^.algorithm);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF OPENSSL_NO_ENGINE}
function f_Ics_UI_set_app_data(r: PUI; arg: Pointer): Integer;
begin
    Result := f_UI_set_ex_data(r, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_UI_get_app_data(r: PUI): Pointer;
begin
    Result := f_UI_get_ex_data(r, 0);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
     X509_L_FILE_LOAD   = 1;
     X509_L_ADD_DIR     = 2;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Openssl macro }
function f_Ics_X509_LOOKUP_load_file(Ctx: PX509_LOOKUP; FileName: PAnsiChar;
    Type_: Longword): Integer;
begin
    Result := f_X509_LOOKUP_ctrl(Ctx, X509_L_FILE_LOAD, FileName, Type_, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Openssl macro }
function f_Ics_X509_LOOKUP_add_dir(Ctx: PX509_LOOKUP; DirName: PAnsiChar;
    Type_: Longword): Integer;
begin
    Result := f_X509_LOOKUP_ctrl(Ctx, X509_L_ADD_DIR, DirName, Type_, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_GET_REASON(ErrCode : Cardinal) : Cardinal;
begin
    Result := (ErrCode and $FFF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_GET_LIB(ErrCode : Cardinal) : Cardinal;
begin
    Result := ((ErrCode shr 24) and $FF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_GET_FUNC(ErrCode : Cardinal) : Cardinal;
begin
    Result := ((ErrCode shr 12) and $FFF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_FATAL_ERROR(ErrCode : Cardinal) : Boolean;
begin
    Result := ((ErrCode and ERR_R_FATAL) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function f_Ics_X509_get_notBefore(X: PX509): PASN1_TIME;        {AG 03/03/06}
var
    PCInfo : PX509_CINF;
begin
    if Assigned(X) then begin
        if ICS_OPENSSL_VERSION_NUMBER < OSSL_VER_1100 then begin
            PCInfo := Pointer(PINT_PTR(X)^);
            Result := PCInfo^.Validity^.notBefore;
        end
        else
            Result := f_X509_get0_notBefore(X);  { V8.32 no longer a macro }
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_get_notAfter(X: PX509): PASN1_TIME;         {AG 03/03/06}
var
    PCInfo : PX509_CINF;
begin
    if Assigned(X) then begin
        if ICS_OPENSSL_VERSION_NUMBER < OSSL_VER_1100 then begin
            PCInfo := Pointer(PINT_PTR(X)^);
            Result := PCInfo^.Validity^.notAfter;
        end
        else
            Result := f_X509_get0_notAfter(X);  { V8.32 no longer a macro }
    end
    else
        Result := nil;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_REQ_get_subject_name(AReq: PX509_REQ): PX509_NAME;   {V8.36 was f_X509_REQ_get_subject_name }
begin
    if Assigned (AReq) then begin
        if ICS_OPENSSL_VERSION_NUMBER < OSSL_VER_1100 then
            Result := AReq^.req_info^.subject
        else
            Result := f_X509_REQ_get_subject_name(AReq);   { V8.36 no longer a macro }
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Asn1ToUTDateTime(Asn1Time: PASN1_TIME;               {AG 03/03/06}
    out UT: TDateTime): Boolean;

    function IncHour(const DT: TDateTime; const IncBy: Integer): TDateTime;
    begin
        Result := ((DT * 24) + IncBy) / 24;
    end;

    function IncMin(const DT: TDateTime; const IncBy: Integer): TDateTime;
    begin
        Result := ((DT * 1440) + IncBy) / 1440;
    end;

var
    Y, M, D, H, N, S : Word;
    I : Integer;
    YC : Word;  { Current century }
    P  : PAnsiChar;
    Offset : Integer;
    Str    : AnsiString;
    IntH, IntM : Integer;
    Sign : Boolean;
begin
    Result  := FALSE;
    UT      := MinDateTime;
    if not Assigned(Asn1Time) then
        Exit;
    try
        I := Asn1Time^.length;
        if I < 10 then Exit;
        P   := Asn1Time.data;
        Y := 0; M := 0; D := 0; {H := 0; N := 0;} S := 0;

        if Asn1Time^.Type_ = V_ASN1_UTCTIME then begin
            {if I < 10 then
                Exit;}
            for I := 0 to 10 - 1 do
                if not (P[I] in ['0'..'9']) then
                    Exit;
            DecodeDate(Now, Y, M, D);
            YC := (Trunc(Y / 100) * 100);
            Y := atoi(P[0] + P[1]);
            if Y < 50 then   { fix century }
                Y := Y + YC
            else
                Y := Y + YC - 100;
            M := atoi(P[2] + P[3]);
            if (M > 12) or (M < 1) then
                Exit;
            D := atoi(P[4] + P[5]);
            H := atoi(P[6] + P[7]);
            N := atoi(P[8] + P[9]);
            { Do we have seconds? }
            if (P[10] in ['0'..'9']) and
               (P[11] in ['0'..'9']) then
            S := atoi(P[10] + P[11]);
        end else
        if Asn1Time^.Type_ = V_ASN1_GENERALIZEDTIME then begin
            if I < 12 then Exit;
            for I := 0 to 12 - 1 do
                if not (P[I] in ['0'..'9']) then
                    Exit;
            Y := atoi(P[0] + P[1] + P[2] + P[3]);
            M := atoi(P[4] + P[5]);
            if (M > 12) or (M < 1) then
                Exit;
            D := atoi(P[6] + P[7]);
            H := atoi(P[8] + P[9]);
            N := atoi(P[10] + P[11]);
            { Do we have seconds? }
            if (P[12] in ['0'..'9']) and
               (P[13] in ['0'..'9']) then
            S := atoi(P[12] + P[13]);
        end else
            Exit;
        UT := EncodeDate(Y, M, D) + EncodeTime(H, N, S, 0);

        { Timezone Offset                                          }
        { '980101000000Z' sample V_ASN1_UTCTIME GMT                }
        { '990630000000+1000' sample timezone + 10 hours           }
        { '20000322085551Z' sample V_ASN1_GENERALIZEDTIME GMT      }
        I := Asn1Time^.length;
        if P[I - 1] <> 'Z' then  // Z = GMT = offset = 0
           { Offset := 0         // offset 0
        else} begin              // get the offset
            SetLength(Str, I);
            Dec(I);
            while I >= 0 do begin
                if P[I] in ['0'..'9'] then
                    Dec(I)
                else begin
                    if P[I] in ['-', '+'] then
                    begin
                        if P[I] = '-' then
                            Sign := TRUE
                        else
                            Sign := FALSE;
                        StrECopy(PAnsiChar(Str), PAnsiChar(@P[I + 1]));
                        SetLength(Str, StrLen(PAnsiChar(Str)));
                        Offset := atoi(Str);
                        if Sign then
                            Offset := -Offset;
                        if (Offset <> 0) and (Offset >= -1200) and
                           (Offset <= 1300) then begin
                            IntH := (Offset div 100);
                            IntM := (Offset mod 100);
                            if IntH <> 0 then
                                UT := IncHour(UT, IntH);
                            if IntM <> 0 then
                                UT := IncMin(UT,  IntM);
                        end;
                    end;
                    Break;
                end;
            end;
        end;
        Result := True;
    except
        // do nothing
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_get_flags(b: PBIO): Integer;
begin
    // This is a hack : BIO structure has not been defined. But I know
    // flags member is the 6th field in the structure (index is 5)
    // This could change when OpenSSL is updated. Check "struct bio_st".
    Result := PInteger(PAnsiChar(b) + 3 * SizeOf(Pointer) + 2 * SizeOf(Integer))^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_retry(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_SHOULD_RETRY) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ASN1_ITEM_ptr(iptr: PASN1_ITEM_EXP): PASN1_ITEM;
begin
    Result := iptr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_read(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_READ) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_write(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_WRITE) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_io_special(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_IO_SPECIAL) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_retry_type(b: PBIO): Integer;
begin
    Result := (BIO_get_flags(b) and BIO_FLAGS_RWS);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslVersion : String;
begin
    Result := String(StrPas(f_OpenSSL_version(OPENSSL_VERSION)))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslCompilerFlags : String;
begin
    Result := String(StrPas(f_OpenSSL_version(OPENSSL_CFLAGS)))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslBuiltOn : String;
begin
    Result := String(StrPas(f_OpenSSL_version(OPENSSL_BUILT_ON)))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslPlatForm : String;
begin
    Result := String(StrPas(f_OpenSSL_version(OPENSSL_PLATFORM)))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslDir : String;
begin
    Result := String(StrPas(f_OpenSSL_version(OPENSSL_DIR)))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BMPStrToWideStr(Str : PAnsiChar; Len : Integer): UnicodeString;
var
    I : Integer;
begin
    SetLength(Result, Len shr 1);
    for I := 0 to (Len shr 1) - 1 do
        Result[I + 1] := WideChar(Byte(Str[I * 2 + 1]) or Byte(Str[I * 2]) shl 8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function EncodeOctetStr(Str : PAnsiChar; Len : Integer) : String;
var
    I : Integer;
    Item : String;
begin
    if (Len = 0) or (Str = nil) then Exit;
    SetLength(Result, Len * 3);
    I := 0;
    while I <= Len - 1 do begin
        Item := IntToHex(Ord(Str[I]), 2) + ':';
        Move(Item[1], Result[I * 3 + 1], 3 * SizeOf(Char));
        Inc(I);
    end;
    SetLength(Result, Length(Result) - 1);
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Asn1ToString(PAsn1 : PASN1_STRING): String;
var
    DataPtr : PAnsiChar;
    DataLen, DataType : Integer;
{$IFDEF UNICODE}
    Len : Integer;
{$ENDIF}
begin
    if (PAsn1 = nil){ or (PAsn1^.data = nil) or (PAsn1^.length <= 0) } then
        Exit;

  { V8.40 use APIs instead of internal structure }
    Result := '';
    DataPtr := f_ASN1_STRING_get0_data(PAsn1);
    DataLen := f_ASN1_STRING_length(PAsn1);
    DataType := f_ASN1_STRING_type(PAsn1);
     if (DataPtr = nil) or (DataLen <= 0) then Exit;

    case DataType { PAsn1^.type_ } of

      V_ASN1_OCTET_STRING :
          //Result := EncodeOctetStr(PAsn1^.data, PAsn1^.length);
      //    Result := IcsBufferToHex(PAsn1^.data, PAsn1^.length, ':');
          Result := IcsBufferToHex(DataPtr, DataLen, ':');

{$IFNDEF UNICODE}
      V_ASN1_UTF8STRING :
      begin  { Slow, but rarely used }
  //        SetLength(Result, PAsn1^.length);
  //        Move(PAnsiChar(PAsn1^.data)^, PAnsiChar(Result)^, PAsn1^.length);
          SetLength(Result, DataLen);
          Move(PAnsiChar(DataPtr)^, PAnsiChar(Result)^, DataLen);
          Result := Utf8ToStringA(Result); { convert to Ansi }
      end;

      V_ASN1_BMPSTRING :
          { Reverse byte order and convert to Ansi }
          Result := UnicodeToAnsi(BMPStrToWideStr(DataPtr, DataLen));

      else  { dump }
//          SetLength(Result, PAsn1^.length);
//          Move(Pointer(PAsn1^.data)^, Pointer(Result)^, PAsn1^.length);
          SetLength(Result, PAsn1^.length);
          Move(Pointer(DataPtr)^, Pointer(Result)^, DataLen);
{$ELSE}
      V_ASN1_UTF8STRING :
      begin
//          Len := IcsMbToWc(CP_UTF8, 0, PAsn1^.data,  PAsn1^.length, nil, 0);
          Len := IcsMbToWc(CP_UTF8, 0, DataPtr,  DataLen, nil, 0);
          SetLength(Result, Len);
          if Len > 0 then
//              IcsMbToWc(CP_UTF8, 0, PAsn1^.data, PAsn1^.length, Pointer(Result), Len);
              IcsMbToWc(CP_UTF8, 0, DataPtr, DataLen, Pointer(Result), Len);
      end;

      V_ASN1_BMPSTRING :
          { Reverse byte order }
//          Result := BMPStrToWideStr(PAsn1^.data, PAsn1^.length);
          Result := BMPStrToWideStr(DataPtr, DataLen);

      else  { dump }
//          Len := IcsMbToWc(CP_ACP, 0, PAsn1^.data, PAsn1^.length, nil, 0);
          Len := IcsMbToWc(CP_ACP, 0, DataPtr, DataLen, nil, 0);
          SetLength(Result, Len);
          if Len > 0 then
//              IcsMbToWc(CP_ACP, 0, PAsn1^.data, PAsn1^.length, Pointer(Result), Len);
              IcsMbToWc(CP_ACP, 0, DataPtr, DataLen, Pointer(Result), Len);
{$ENDIF}
      end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_get_secure_renegotiation_support(S: PSSL): Longint;
begin
//    if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_0908N then
        Result := f_SSL_ctrl(S, SSL_CTRL_GET_RI_SUPPORT, 0, nil)
//    else
//        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Ics_Ssl_EVP_PKEY_IncRefCnt(K: PEVP_PKEY; Increment: Integer = 1);
begin
    { This is thread-safe only with a TSslStaticLock or TSslDynamicLock.    }
    { From the OpenSSL sources I know that lock-ID CRYPTO_LOCK_EVP_PKEY is  }
    { used by OpenSSL to protect EVP_PKEY_st.references field.              }
    f_Crypto_lock(CRYPTO_LOCK, CRYPTO_LOCK_EVP_PKEY,
                  PAnsiChar('Ics_Ssl_EVP_PKEY_IncRefCnt'), 0);
    try
        { This is a hack and might change with new OSSL version, search for }
        { "struct EVP_PKEY_st" field "references".                          }
        Inc(PInteger(PAnsiChar(K) + 2 * SizeOf(Longint))^, Increment);
    finally
        f_Crypto_lock(CRYPTO_UNLOCK, CRYPTO_LOCK_EVP_PKEY,
                      PAnsiChar('Ics_Ssl_EVP_PKEY_IncRefCnt'), 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_EVP_PKEY_dup(PKey: PEVP_PKEY): PEVP_PKEY;     { V8.40 moved from wsocket }
begin
    Result := nil;
    if PKey <> nil then begin
        Ics_Ssl_EVP_PKEY_IncRefCnt(PKey);
        Result := PKey;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_Ssl_EVP_PKEY_GetKey(K: PEVP_PKEY): Pointer;
begin
//    if @f_EVP_PKEY_get0 <> nil then // v1.0.0+
    Result := f_EVP_PKEY_get0(K);        { V8.40 }
(*    else
        { * This is a hack * }
    {$IFDEF CPUX64} // Alignment of OSSL records is 8 bytes!
        Result := Pointer(PSize_t(PAnsiChar(K) + 4 * SizeOf(Longint))^);
    {$ELSE}
        Result := Pointer(PSize_t(PAnsiChar(K) + 3 * SizeOf(Longint))^);
    {$ENDIF}     *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_Ssl_EVP_PKEY_GetType(K: PEVP_PKEY): Integer;
begin
     Result := f_EVP_PKEY_base_id(K);        { V8.40 }
    { This is a hack and might change with new OSSL version, search }
    { for "struct EVP_PKEY_st"                                      }
//    Result := PInteger(K)^;      { V8.40 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.05 get library from OpenSsl error code - ERR_LIB_xxx literals }
function Ics_Ssl_ERR_GET_LIB(E: DWORD): Integer;
begin
    result := (E shr 24) and $0ff;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.05 get function from OpenSsl error code - SSL_F_xxx literals }
function Ics_Ssl_ERR_GET_FUNC(E: DWORD): Integer;
begin
    result := (E shr 12) and $0fff;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.05 get reason from OpenSsl error code - SSL_R_xxx literals }
function Ics_Ssl_ERR_GET_REASON(E: DWORD): Integer;
begin
    Result := E and $0fff;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_MD_CTX_create: PEVP_MD_CTX;
begin
    result := f_EVP_MD_CTX_new;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_MD_CTX_init(ctx: PEVP_MD_CTX): integer;
begin
    result := f_EVP_MD_CTX_reset(ctx);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
procedure f_EVP_MD_CTX_destroy(ctx: PEVP_MD_CTX);
begin
    f_EVP_MD_CTX_free(ctx);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_MD_nid(e: PEVP_MD): Integer;
begin
    result := f_EVP_MD_type(e);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_MD_name(e: PEVP_MD): PAnsiChar;
begin
    result := f_OBJ_nid2sn(f_EVP_MD_nid(e));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_MD_CTX_size(e: PEVP_MD_CTX): Integer;
begin
    result := f_EVP_MD_size(f_EVP_MD_CTX_md(e));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_MD_CTX_block_size(e: PEVP_MD_CTX): Integer;
begin
    result := f_EVP_MD_block_size(f_EVP_MD_CTX_md(e));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_MD_CTX_type(e: PEVP_MD_CTX): Integer;
begin
    result := f_EVP_MD_type(f_EVP_MD_CTX_md(e));
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_get_digestbynid(a: Integer): PEVP_MD;
begin
    result := f_EVP_get_digestbyname(f_OBJ_nid2sn(a));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_get_digestbyobj(a: PASN1_OBJECT): PEVP_MD;
begin
    result := f_EVP_get_digestbynid(f_OBJ_obj2nid(a));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_get_cipherbynid(a: Integer): PEVP_CIPHER;
begin
    result := f_EVP_get_cipherbyname(f_OBJ_nid2sn(a));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_get_cipherbyobj(a: PASN1_OBJECT): PEVP_CIPHER;
begin
    result := f_EVP_get_cipherbynid(f_OBJ_obj2nid(a));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_SignInit_ex(ctx: PEVP_MD_CTX; etype: PEVP_MD; impl: PEngine): Integer;     { V8.40 }
begin
    result := f_EVP_DigestInit_ex(ctx, etype, impl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_SignInit(ctx: PEVP_MD_CTX; etype: PEVP_MD): Integer; cdecl;          { V8.40 }
begin
    result := f_EVP_DigestInit(ctx, etype);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_SignUpdate(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;  { V8.40 }
begin
    result := f_EVP_DigestUpdate(ctx, d, cnt);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_OpenUpdate(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer; const in_: PAnsiChar; inl: Integer): LongBool;  { V8.40 }
begin
    result := f_EVP_DecryptUpdate(ctx, out_, outl, in_, inl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_SealUpdate(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; var outl: Integer; const in_: PAnsiChar; inl: Integer): LongBool;   { V8.40 }
begin
    result := f_EVP_EncryptUpdate(ctx, out_, outl, in_, inl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_VerifyInit_ex(ctx: PEVP_MD_CTX; etype: PEVP_MD; impl: PEngine): Integer;     { V8.40 }
begin
    result := f_EVP_DigestInit_ex(ctx, etype, impl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_VerifyInit(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;  { V8.40 }
begin
    result := f_EVP_DigestUpdate(ctx, d, cnt);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EVP_VerifyUpdate(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;  { V8.40 }
begin
    result := f_EVP_DigestUpdate(ctx, d, cnt);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_DigestSignUpdate(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;  { V8.40 }
begin
    result := f_EVP_DigestUpdate(ctx, d, cnt);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_DigestVerifyUpdate(ctx: PEVP_MD_CTX; d: Pointer; cnt: SIZE_T): Integer;  { V8.40 }
begin
    result := f_EVP_DigestUpdate(ctx, d, cnt);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 macros }
function f_EC_KEY_get_ex_new_index(argl: DWord; argp: Pointer; new_func: TCryptoExNewFunc; dup_func: TCryptoExDupFunc; free_func: TCryptoExFreeFunc): integer;  { V8.40 }
begin
    result := f_CRYPTO_get_ex_new_index(CRYPTO_EX_INDEX_EC_KEY, argl, argp, new_func, dup_func, free_func);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} //USE_SSL
end.


