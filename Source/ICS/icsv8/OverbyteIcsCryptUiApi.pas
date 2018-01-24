{******************************************************************************}
{                                                                              }
{ Windows Cryptographic UI API Prototypes and Definitions for Delphi           }
{                                                                              }
{ The original file is: cryptuiapi.h, the original Pascal code is:             }
{ CryptuiApi.pas, released May 2011. The initial developer of the              }
{ original translation is Arno Garrels (arno dott garrels att gmx dott de).    }
{                                                                              }
{ Portions created by Arno Garrels are Copyright (C) 2015                      }
{ Arno Garrels. All Rights Reserved.                                           }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1992-1999 Microsoft          }
{ Corporation.                                                                 }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License          }
{ Version 1.1 (the "License"). You may not use this file except in compliance  }
{ with the License. You may obtain a copy of the License at                    }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Modification history:                                                        }
{ June 2015 - Angus renamed from CryptuiApi and moved to main source dir       }
{                   now using OverbyteIcsWinCrypt                              }
{                                                                              }

{******************************************************************************}

unit OverbyteIcsCryptuiApi;

{$WEAKPACKAGEUNIT}

{.$DEFINE WIN7_UP}  // WinXp+, most likely W2K+

interface

uses
  Windows,
//#include <wintrust.h>
  OverbyteIcsWinCrypt;
//#include <prsht.h>

{$HPPEMIT '#include <OverbyteIcscryptuiapi.h>'}

//#include <pshpack8.h>
{.$ALIGN 8}

type
{ Dummies from wintrust.h }
  {$EXTERNALSYM PCRYPT_PROVIDER_DATA}
  PCRYPT_PROVIDER_DATA = Pointer;
{ Dummies from prsht.h }
  {$EXTERNALSYM LPCPROPSHEETPAGEW}
  LPCPROPSHEETPAGEW = Pointer;
  {$EXTERNALSYM LPCPROPSHEETPAGEA}
  LPCPROPSHEETPAGEA = Pointer;

const
  {$EXTERNALSYM CERT_CREDENTIAL_PROVIDER_ID}
  CERT_CREDENTIAL_PROVIDER_ID         = - 509;

//+----------------------------------------------------------------------------
//  Dialog viewer of a certificate, CTL or CRL context.
//
//  dwContextType and associated pvContext's
//      CERT_STORE_CERTIFICATE_CONTEXT  PCCERT_CONTEXT
//      CERT_STORE_CRL_CONTEXT          PCCRL_CONTEXT
//      CERT_STORE_CTL_CONTEXT          PCCTL_CONTEXT
//
//  dwFlags currently isn't used and should be set to 0.
//-----------------------------------------------------------------------------
{$EXTERNALSYM CryptUIDlgViewContext}
function CryptUIDlgViewContext(
  dwContextType: DWORD;
  pvContext: Pointer;
  hwnd: HWND;          // OPTIONAL Defaults to the desktop window
  pwszTitle: LPCWSTR;  // OPTIONAL Defaults to the context type title
  dwFlags: DWORD;
  pvReserved: Pointer
): BOOL; stdcall;



//+----------------------------------------------------------------------------
//  Dialog to select a certificate from the specified store.
//
//  Returns the selected certificate context. If no certificate was
//  selected, NULL is returned.
//
//  pwszTitle is either NULL or the title to be used for the dialog.
//  If NULL, the default title is used.  The default title is
//  "Select Certificate".
//
//  pwszDisplayString is either NULL or the text statement in the selection
//  dialog.  If NULL, the default phrase
//  "Select a certificate you wish to use" is used in the dialog.
//
//  dwDontUseColumn can be set to exclude columns from the selection
//  dialog. See the CRYPTDLG_SELECTCERT_*_COLUMN definitions below.
//
//  dwFlags currently isn't used and should be set to 0.
//-----------------------------------------------------------------------------
{$EXTERNALSYM CryptUIDlgSelectCertificateFromStore}
function CryptUIDlgSelectCertificateFromStore(
  hCertStore: HCERTSTORE;
  hwnd: HWND;                  // OPTIONAL Defaults to the desktop window
  pwszTitle: LPCWSTR;          // OPTIONAL
  pwszDisplayString: LPCWSTR;  // OPTIONAL
  dwDontUseColumn: DWORD;
  dwFlags: DWORD;
  pvReserved: Pointer
): PCCERT_CONTEXT; stdcall;

const
// flags for dwDontUseColumn
  {$EXTERNALSYM CRYPTUI_SELECT_ISSUEDTO_COLUMN}
  CRYPTUI_SELECT_ISSUEDTO_COLUMN      = $000000001;
  {$EXTERNALSYM CRYPTUI_SELECT_ISSUEDBY_COLUMN}
  CRYPTUI_SELECT_ISSUEDBY_COLUMN      = $000000002;
  {$EXTERNALSYM CRYPTUI_SELECT_INTENDEDUSE_COLUMN}
  CRYPTUI_SELECT_INTENDEDUSE_COLUMN   = $000000004;
  {$EXTERNALSYM CRYPTUI_SELECT_FRIENDLYNAME_COLUMN}
  CRYPTUI_SELECT_FRIENDLYNAME_COLUMN  = $000000008;
  {$EXTERNALSYM CRYPTUI_SELECT_LOCATION_COLUMN}
  CRYPTUI_SELECT_LOCATION_COLUMN      = $000000010;
  {$EXTERNALSYM CRYPTUI_SELECT_EXPIRATION_COLUMN}
  CRYPTUI_SELECT_EXPIRATION_COLUMN    = $000000020;

//+----------------------------------------------------------------------------
//
// The select cert dialog can be passed a filter proc to reduce the set of
// certificates displayed.  Return TRUE to display the certificate and FALSE to
// hide it.  If TRUE is returned then optionally the pfInitialSelectedCert
// boolean may be set to TRUE to indicate to the dialog that this cert should
// be the initially selected cert.  Note that the most recent cert that had the
// pfInitialSelectedCert boolean set during the callback will be the initially
// selected cert.
//
//-----------------------------------------------------------------------------
type
  TFNCFILTERPROC = function(pCertContext: PCCERT_CONTEXT;
    pfInitialSelectedCert: PBool; pvCallbackData: Pointer): BOOL;

{$IFDEF WIN7_UP}
  PCertSelectuiInput = ^TCertSelectuiInput;
  {$EXTERNALSYM CERT_SELECTUI_INPUT}
  CERT_SELECTUI_INPUT = record
    hStore: HCERTSTORE;
    prgpChain: ^PCCERT_CHAIN_CONTEXT;
    cChain: DWORD;
  end;
  {$EXTERNALSYM PCERT_SELECTUI_INPUT}
  PCERT_SELECTUI_INPUT = ^CERT_SELECTUI_INPUT;
  TCertSelectuiInput = CERT_SELECTUI_INPUT;

//+----------------------------------------------------------------------------
//
// CertSelectionGetSerializedBlob
//
// The API to obtain serialized blob from an input struct
//
//
//-----------------------------------------------------------------------------
{$EXTERNALSYM CertSelectionGetSerializedBlob}
function CertSelectionGetSerializedBlob(
  pcsi: PCERT_SELECTUI_INPUT;
  ppOutBuffer: PPointer;
  var pulOutBufferSize: ULONG): HResult; stdcall;
{$ENDIF WIN7_UP}

//+----------------------------------------------------------------------------
// Valid values for dwFlags in CRYPTUI_CERT_MGR_STRUCT struct.
//-----------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPTUI_CERT_MGR_TAB_MASK}
  CRYPTUI_CERT_MGR_TAB_MASK           = $0000000F;
  {$EXTERNALSYM CRYPTUI_CERT_MGR_PUBLISHER_TAB}
  CRYPTUI_CERT_MGR_PUBLISHER_TAB      = $00000004;
  {$EXTERNALSYM CRYPTUI_CERT_MGR_SINGLE_TAB_FLAG}
  CRYPTUI_CERT_MGR_SINGLE_TAB_FLAG    = $00008000;

//+----------------------------------------------------------------------------
//
// CRYPTUI_CERT_MGR_STRUCT
//
// dwSize               IN Required: Should be set to
//                                   sizeof(CRYPTUI_CERT_MGR_STRUCT)
//
// hwndParent           IN Optional: Parent of this dialog.
//
// dwFlags              IN Optional: Personal is the default initially selected
//                                   tab.
//
//                                   CRYPTUI_CERT_MGR_PUBLISHER_TAB may be set
//                                   to select Trusted Publishers as the
//                                   initially selected tab.
//
//                                   CRYPTUI_CERT_MGR_SINGLE_TAB_FLAG may also
//                                   be set to only display the Trusted
//                                   Publishers tab.
//
// pwszTitle            IN Optional: Title of the dialog.
//
// pszInitUsageOID      IN Optional: The enhanced key usage object identifier
//                                   (OID). Certificates with this OID will
//                                   initially be shown as a default. User
//                                   can then choose different OIDs. NULL
//                                   means all certificates will be shown
//                                   initially.
//
//-----------------------------------------------------------------------------
type
  PCryptuiCertMgrStruct = ^TCryptuiCertMgrStruct;
  {$EXTERNALSYM _CRYPTUI_CERT_MGR_STRUCT}
  _CRYPTUI_CERT_MGR_STRUCT = record
    dwSize: DWORD;
    hwndParent: HWND;
    dwFlags: DWORD;
    pwszTitle: LPCWSTR;
    pszInitUsageOID: LPCSTR;
  end;
  {$EXTERNALSYM CRYPTUI_CERT_MGR_STRUCT}
  CRYPTUI_CERT_MGR_STRUCT = _CRYPTUI_CERT_MGR_STRUCT;
  {$EXTERNALSYM PCRYPTUI_CERT_MGR_STRUCT}
  PCRYPTUI_CERT_MGR_STRUCT = ^_CRYPTUI_CERT_MGR_STRUCT;
  TCryptuiCertMgrStruct = _CRYPTUI_CERT_MGR_STRUCT;

  {$EXTERNALSYM PCCRYPTUI_CERT_MGR_STRUCT}
  PCCRYPTUI_CERT_MGR_STRUCT = ^CRYPTUI_CERT_MGR_STRUCT;


//+----------------------------------------------------------------------------
//
// CryptUIDlgCertMgr
//
// The wizard to manage certificates in store.
//
// pCryptUICertMgr      IN  Required: Poitner to CRYPTUI_CERT_MGR_STRUCT
//                                    structure.
//
//-----------------------------------------------------------------------------
{$EXTERNALSYM CryptUIDlgCertMgr}
function CryptUIDlgCertMgr(
  pCryptUICertMgr: PCCRYPTUI_CERT_MGR_STRUCT
): BOOL; stdcall;


//+----------------------------------------------------------------------------
//
// CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO
//
// dwSize               IN Required: Should be set to
//                                   sizeof(CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO)
//
// pGuidSubject         IN Required: Idenfity the sip functions to load
//
// cbBlob               IN Required: The size of blob, in bytes
//
// pwszDispalyName      IN Optional: The display name of the blob to sign
//
//-----------------------------------------------------------------------------
type
  PCryptuiWizDigitalSignBlobInfo = ^TCryptuiWizDigitalSignBlobInfo;
  {$EXTERNALSYM _CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO}
  _CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO = record
    dwSize: DWORD;
    pGuidSubject: PGUID;
    cbBlob: DWORD;
    pbBlob: PByte;
    pwszDisplayName: LPCWSTR;
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO}
  CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO = _CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO;
  {$EXTERNALSYM PCRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO}
  PCRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO = ^_CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO;
  TCryptuiWizDigitalSignBlobInfo = _CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO;

  {$EXTERNALSYM PCCRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO}
  PCCRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO = ^CRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO;

//+----------------------------------------------------------------------------
//
// CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO
//
// dwSize               IN Required: Should be set to
//                                   sizeof(CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO)
//
// cCertStore           IN Required: The acount of certificate store array that
//                                   includes potentical sining certs
//
// rghCertStore         IN Required: The certificate store array that includes
//                                   potential signing certs
//
// pFilterCallback      IN Optional: The filter call back function for display
//                                   the certificate
//
// pvCallbackData       IN Optional: The call back data
//
//-----------------------------------------------------------------------------
  PCryptuiWizDigitalSignStoreInfo = ^TCryptuiWizDigitalSignStoreInfo;
  {$EXTERNALSYM _CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO}
  _CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO = record
    dwSize: DWORD;
    cCertStore: DWORD;
    rghCertStore: PHCERTSTORE;
    pFilterCallback: TFNCFILTERPROC;
    pvCallbackData: Pointer;
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO}
  CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO = _CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO;
  {$EXTERNALSYM PCRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO}
  PCRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO = ^_CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO;
  TCryptuiWizDigitalSignStoreInfo = _CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO;

  {$EXTERNALSYM PCCRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO}
  PCCRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO = ^CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO;

//+----------------------------------------------------------------------------
//
// CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO
//
// dwSize               IN Required: Should be set to
//                                   sizeof(CRYPT_WIZ_DIGITAL_SIGN_PVK_FILE_INFO)
//
// pwszPvkFileName      IN Required: The PVK file name
//
// pwszProvName         IN Required: The provider name
//
// dwProvType           IN Required: The provider type
//
//-----------------------------------------------------------------------------
  PCryptuiWizDigitalSignPvkFileInfo = ^TCryptuiWizDigitalSignPvkFileInfo;
  {$EXTERNALSYM _CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO}
  _CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO = record
    dwSize: DWORD;
    pwszPvkFileName: LPWSTR;
    pwszProvName: LPWSTR;
    dwProvType: DWORD;
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO}
  CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO = _CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO;
  {$EXTERNALSYM PCRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO}
  PCRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO = ^_CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO;
  TCryptuiWizDigitalSignPvkFileInfo = _CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO;

  {$EXTERNALSYM PCCRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO}
  PCCRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO = ^CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO;

//+----------------------------------------------------------------------------
// Valid values for dwPvkChoice in CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO
// struct.
//-----------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE}
  CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE   = $01;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_PVK_PROV}
  CRYPTUI_WIZ_DIGITAL_SIGN_PVK_PROV   = $02;

//+----------------------------------------------------------------------------
//
// CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO
//
// dwSize                   IN Required: Should be set to
//                                       sizeof(CRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO)
//
// pwszSigningCertFileName  IN Required: The file name that contains the
//                                       signing cert(s)
//
// dwPvkChoice              IN Required: Indicate the private key type.
//                                       It can be one of the following:
//                                           CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE
//                                           CRYPTUI_WIZ_DIGITAL_SIGN_PVK_PROV
//
//  pPvkFileInfo            IN Required: If dwPvkChoice == CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE
//
//  pPvkProvInfo            IN Required: If dwPvkContainer == CRYPTUI_WIZ_DIGITAL_SIGN_PVK_PROV
//
//-----------------------------------------------------------------------------
type
  PCryptuiWizDigitalSignCertPvkInfo = ^TCryptuiWizDigitalSignCertPvkInfo;
  {$EXTERNALSYM _CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO}
  _CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO = record
    dwSize: DWORD;
    pwszSigningCertFileName: LPWSTR;
    case dwPvkChoice: DWORD of
       // union {
       CRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE:
        (pPvkFileInfo : PCCRYPTUI_WIZ_DIGITAL_SIGN_PVK_FILE_INFO);
       CRYPTUI_WIZ_DIGITAL_SIGN_PVK_PROV:
        (pPvkProvInfo : PCRYPT_KEY_PROV_INFO);
       //}
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO}
  CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO = _CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO;
  {$EXTERNALSYM PCRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO}
  PCRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO = ^_CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO;
  TCryptuiWizDigitalSignCertPvkInfo = _CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO;

  {$EXTERNALSYM PCCRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO}
  PCCRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO = ^CRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO;

//+----------------------------------------------------------------------------
// Valid values for dwAttrFlags in CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO
// struct.
//-----------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_COMMERCIAL}
  CRYPTUI_WIZ_DIGITAL_SIGN_COMMERCIAL = $0001;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_INDIVIDUAL}
  CRYPTUI_WIZ_DIGITAL_SIGN_INDIVIDUAL = $0002;

//+----------------------------------------------------------------------------
//
// CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO
//
// dwSize                       IN Required: Should be set to
//                                           sizeof(CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO)
//
// dwAttrFlags                  IN Required: Flag to indicate signing options.
//                                           It can be one of the following:
//                                               CRYPTUI_WIZ_DIGITAL_SIGN_COMMERCIAL
//                                               CRYPTUI_WIZ_DIGITAL_SIGN_INDIVIDUAL
//
// pwszDescription              IN Optional: The description of the signing
//                                           subject.

// pwszMoreInfoLocation         IN Optional: The localtion to get more
//                                           information about file this
//                                           information will be shown upon
//                                           download time.
//
// pszHashAlg                   IN Optional: The hashing algorithm for the
//                                           signature. NULL means using SHA1
//                                           hashing algorithm.
//
// pwszSigningCertDisplayString IN Optional: The display string to be
//                                           displayed on the signing
//                                           certificate wizard page. The
//                                           string should prompt user to
//                                           select a certificate for a
//                                           particular purpose.
//
// hAddtionalCertStores         IN Optional: The addtional cert store to add to
//                                           the signature.
//
// psAuthenticated              IN Optional: User supplied authenticated
//                                           attributes added to the signature.
//
// psUnauthenticated            IN Optional: User supplied unauthenticated
//                                           attributes added to the signature.
//
//-----------------------------------------------------------------------------
type
  PCryptuiWizDigitalSignExtendedInfo = ^TCryptuiWizDigitalSignExtendedInfo;
  {$EXTERNALSYM _CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO}
  _CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO = record
    dwSize: DWORD;
    dwAttrFlags: DWORD;
    pwszDescription: LPCWSTR;
    pwszMoreInfoLocation: LPCWSTR;
    pszHashAlg: LPCSTR;
    pwszSigningCertDisplayString: LPCWSTR;
    hAdditionalCertStore: HCERTSTORE;
    psAuthenticated: PCRYPT_ATTRIBUTES;
    psUnauthenticated: PCRYPT_ATTRIBUTES;
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO}
  CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO = _CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO;
  {$EXTERNALSYM PCRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO}
  PCRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO = ^_CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO;
  TCryptuiWizDigitalSignExtendedInfo = _CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO;

  {$EXTERNALSYM PCCRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO}
  PCCRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO = ^CRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO;

//+----------------------------------------------------------------------------
// Valid values for dwSubjectChoice in CRYPTUI_WIZ_DIGITAL_SIGN_INFO struct.
//-----------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_FILE}
  CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_FILE= $01;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_BLOB}
  CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_BLOB= $02;

//+----------------------------------------------------------------------------
// Valid values for dwSigningCertChoice in CRYPTUI_WIZ_DIGITAL_SIGN_INFO
// struct.
//-----------------------------------------------------------------------------
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_CERT}
  CRYPTUI_WIZ_DIGITAL_SIGN_CERT       = $01;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_STORE}
  CRYPTUI_WIZ_DIGITAL_SIGN_STORE      = $02;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_PVK}
  CRYPTUI_WIZ_DIGITAL_SIGN_PVK        = $03;

//+----------------------------------------------------------------------------
// Valid values for dwAddtionalCertChoice in CRYPTUI_WIZ_DIGITAL_SIGN_INFO
// struct.
//-----------------------------------------------------------------------------
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_ADD_CHAIN}
  CRYPTUI_WIZ_DIGITAL_SIGN_ADD_CHAIN          = $00000001;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_ADD_CHAIN_NO_ROOT}
  CRYPTUI_WIZ_DIGITAL_SIGN_ADD_CHAIN_NO_ROOT  = $00000002;

//+----------------------------------------------------------------------------
//
// CRYPTUI_WIZ_DIGITAL_SIGN_INFO
//
// dwSize                   IN Required: Should be set to
//                                       sizeof(CRYPTUI_WIZ_DIGITAL_SIGN_INFO)
//
// dwSubjectChoice          IN Required: If CRYPTUI_WIZ_NO_UI is set in dwFlags
//                                       of the CryptUIWizDigitalSign call.
//
//                             Optional: If CRYPTUI_WIZ_NO_UI is not set in
//                                       dwFlags of the CryptUIWizDigitalSign
//                                       call.
//
//                                       Indicate whether to sign a file or to
//                                       sign a memory blob. 0 means promting
//                                       user for the file to sign.
//
//                                       It can be one of the following:
//                                           CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_FILE
//                                           CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_BLOB
//
// pwszFileName             IN Required: If dwSubjectChoice == CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_FILE
//
// pSignBlobInfo            IN Required: If dwSubhectChoice == CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_BLOB
//
// dwSigningCertChoice      IN Optional: Indicate the signing certificate.
//                                       0 means using the certificates in
//                                       "My" store".
//
//                                       It can be one of the following choices:
//                                           CRYPTUI_WIZ_DIGITAL_SIGN_CERT
//                                           CRYPTUI_WIZ_DIGITAL_SIGN_STORE
//                                           CRYPTUI_WIZ_DIGITAL_SIGN_PVK
//
//                                       If CRYPTUI_WIZ_NO_UI is set in dwFlags
//                                       of the CryptUIWizDigitalSign call,
//                                       dwSigningCertChoice has to be
//                                       CRYPTUI_WIZ_DIGITAL_SIGN_CERT or
//                                       CRYPTUI_WIZ_DIGITAL_SIGN_PVK
//
// pSigningCertContext      IN Required: If dwSigningCertChoice == CRYPTUI_WIZ_DIGITAL_SIGN_CERT
//
// pSigningCertStore        IN Required: If dwSigningCertChoice == CRYPTUI_WIZ_DIGITAL_SIGN_STORE
//
// pSigningCertPvkInfo      IN Required: If dwSigningCertChoise == CRYPTUI_WIZ_DIGITAL_SIGN_PVK
//
// pwszTimestampURL         IN Optional: The timestamp URL address.
//
// dwAdditionalCertChoice   IN Optional: Indicate additional certificates to be
//                                       included in the signature. 0 means no
//                                       addtional certificates will be added.
//
//                                       The following flags are mutually
//                                       exclusive.
//                                       Only one of them can be set:
//                                           CRYPTUI_WIZ_DIGITAL_SIGN_ADD_CHAIN
//                                           CRYPTUI_WIZ_DIGITAL_SIGN_ADD_CHAIN_NO_ROOT
//
// pSignExtInfo             IN Optional: The extended information for signing.
//
//-----------------------------------------------------------------------------
type
  PCryptuiWizDigitalSignInfo = ^TCryptuiWizDigitalSignInfo;
  {$EXTERNALSYM _CRYPTUI_WIZ_DIGITAL_SIGN_INFO}
  _CRYPTUI_WIZ_DIGITAL_SIGN_INFO = record
    dwSize: DWORD;
    case dwSubjectChoice: DWORD of
      // union {
      CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_FILE:
        (pwszFileName  : LPCWSTR);
      CRYPTUI_WIZ_DIGITAL_SIGN_SUBJECT_BLOB:
        (pSignBlobInfo : PCCRYPTUI_WIZ_DIGITAL_SIGN_BLOB_INFO;
      // };
        case dwSigningCertChoice: DWORD of
        // union {
          CRYPTUI_WIZ_DIGITAL_SIGN_CERT:
            (pSigningCertContext : PCCERT_CONTEXT);                          //Pointer
          CRYPTUI_WIZ_DIGITAL_SIGN_STORE:
            (pSigningCertStore   : PCCRYPTUI_WIZ_DIGITAL_SIGN_STORE_INFO);   //Pointer
          CRYPTUI_WIZ_DIGITAL_SIGN_PVK:
            (pSigningCertPvkInfo : PCCRYPTUI_WIZ_DIGITAL_SIGN_CERT_PVK_INFO; //Pointer
        // };
            pwszTimestampURL: LPCWSTR;
            dwAdditionalCertChoice: DWORD;
            pSignExtInfo: PCCRYPTUI_WIZ_DIGITAL_SIGN_EXTENDED_INFO));
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_INFO}
  CRYPTUI_WIZ_DIGITAL_SIGN_INFO = _CRYPTUI_WIZ_DIGITAL_SIGN_INFO;
  {$EXTERNALSYM PCRYPTUI_WIZ_DIGITAL_SIGN_INFO}
  PCRYPTUI_WIZ_DIGITAL_SIGN_INFO = ^_CRYPTUI_WIZ_DIGITAL_SIGN_INFO;
  TCryptuiWizDigitalSignInfo = _CRYPTUI_WIZ_DIGITAL_SIGN_INFO;

  {$EXTERNALSYM PCCRYPTUI_WIZ_DIGITAL_SIGN_INFO}
  PCCRYPTUI_WIZ_DIGITAL_SIGN_INFO = ^CRYPTUI_WIZ_DIGITAL_SIGN_INFO;

//+----------------------------------------------------------------------------
//
// CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT
//
// dwSize               IN Required: Should be set to
//                                   sizeof(CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT)
//
// cbBlob               IN Required: The size of pbBlob in bytes.
//
// pbBlob               IN Required: The signed blob.
//
//-----------------------------------------------------------------------------

  PCryptuiWizDigitalSignContext = ^TCryptuiWizDigitalSignContext;
  {$EXTERNALSYM _CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT}
  _CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT = record
    dwSize: DWORD;
    cbBlob: DWORD;
    pbBlob: PByte;
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT}
  CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT = _CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT;
  {$EXTERNALSYM PCRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT}
  PCRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT = ^_CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT;
  TCryptuiWizDigitalSignContext = _CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT;

  {$EXTERNALSYM PCCRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT}
  PCCRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT = ^CRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT;

//+----------------------------------------------------------------------------
// Valid values for dwFlags parameter to CryptUIWizDigitalSign.
//-----------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPTUI_WIZ_NO_UI}
  CRYPTUI_WIZ_NO_UI                             = $0001;
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_EXCLUDE_PAGE_HASHES}
  CRYPTUI_WIZ_DIGITAL_SIGN_EXCLUDE_PAGE_HASHES  = $0002;

// The above CRYPTUI_WIZ_DIGITAL_SIGN_EXCLUDE_PAGE_HASHES takes precedence if
// also set.
  {$EXTERNALSYM CRYPTUI_WIZ_DIGITAL_SIGN_INCLUDE_PAGE_HASHES}
  CRYPTUI_WIZ_DIGITAL_SIGN_INCLUDE_PAGE_HASHES  = $0004;

//+----------------------------------------------------------------------------
//
// CryptUIWizDigitalSign
//
// The wizard to digitally sign a document or a blob.
//
// If CRYPTUI_WIZ_NO_UI is set in dwFlags, no UI will be shown.  Otherwise,
// user will be prompted for input through a wizard.
//
// dwFlags              IN Required: See dwFlags values above.
//
// hwndParent           IN Optional: The parent window handle.
//
// pwszWizardTitle      IN Optional: The title of the wizard.
//
// pDigitalSignInfo     IN Required: The information about the signing process.
//
// ppSignContext        OUT Optional: The context pointer points to the signed
//                                    blob.
//
//-----------------------------------------------------------------------------
{$EXTERNALSYM CryptUIWizDigitalSign}
function CryptUIWizDigitalSign(
  dwFlags: DWORD;
  hwndParent: HWND;                                     // OPTIONAL
  pwszWizardTitle: LPCWSTR;                             //OPTIONAL
  pDigitalSignInfo: PCCRYPTUI_WIZ_DIGITAL_SIGN_INFO;
  var ppSignContext: PCCRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT //OPTIONAL
): BOOL; stdcall;

{$EXTERNALSYM CryptUIWizFreeDigitalSignContext}
function CryptUIWizFreeDigitalSignContext(
  pSignContext: PCCRYPTUI_WIZ_DIGITAL_SIGN_CONTEXT
): BOOL; stdcall;



/////////////////////////////////////////////////////////////////////////////////////////////////////
//
// dwSize                          size of this struct
// hwndParent                      parent of this dialog                                    (OPTIONAL)
// dwFlags                         flags, may a combination of any of the flags below       (OPTIONAL)
// szTitle                         title for the window                                     (OPTIONAL)
// pCertContext                    the cert context that is to be displayed
// rgszPurposes                    array of purposes that this cert is to be validated for  (OPTIONAL)
// cPurposes                       number of purposes                                       (OPTIONAL)
// pCryptProviderData/hWVTStateData if WinVerifyTrust has already been called for the cert  (OPTIONAL)
//                                 then pass in a pointer to the state struct that was
//                                 acquired through a call to WTHelperProvDataFromStateData(),
//                                 or pass in the hWVTStateData of the WINTRUST_DATA struct
//                                 if WTHelperProvDataFromStateData() was not called.
//                                 if pCryptProviderData/hWVTStateData is used then
//                                 fpCryptProviderDataTrustedUsage, idxSigner, idxCert, and
//                                 fCounterSignature must be set
// fpCryptProviderDataTrustedUsage if WinVerifyTrust was called this is the result of whether (OPTIONAL)
//                                 the cert was trusted
// idxSigner                       the index of the signer to view                          (OPTIONAL)
// idxCert                         the index of the cert that is being viewed within the    (OPTIONAL)
//                                 signer chain.  the cert context of this cert MUST match
//                                 pCertContext
// fCounterSigner                  set to TRUE if a counter signature is being viewed.  if  (OPTIONAL)
//                                 this is TRUE then idxCounterSigner must be valid
// idxCounterSigner                the index of the counter signer to view                  (OPTIONAL)
// cStores                         Count of other stores to search when building and        (OPTIONAL)
//                                 validating chain
// rghStores                       Array of other stores to search when buliding and        (OPTIONAL)
//                                 validating chain
// cPropSheetPages                 number of extra pages to add to the dialog.              (OPTIONAL)
// rgPropSheetPages                extra pages to add to the dialog.                        (OPTIONAL)
//                                 each page in this array will NOT recieve the lParam in
//                                 the PROPSHEET structure as the lParam in the
//                                 WM_INITDIALOG, instead it will receive a pointer to a
//                                 CRYPTUI_INITDIALOG_STRUCT (defined below) which contains
//                                 the lParam in the PROPSSHEET structure AND the
//                                 PCCERT_CONTEXT for which the page is being displayed.
// nStartPage                      this is the index of the initial page that will be
//                                 displayed.  if the upper most bit (0x8000) is set then
//                                 the index is assumed to index rgPropSheetPages
//                                 (after the upper most bit has been stripped off.  eg.
//                                 0x8000 will indicate the first page in rgPropSheetPages),
//                                 if the upper most bit is 0 then nStartPage will be the
//                                 starting index of the default certificate dialog pages.
//
/////////////////////////////////////////////////////////////////////////////////////////////////////
const
// dwFlags
  {$EXTERNALSYM CRYPTUI_HIDE_HIERARCHYPAGE}
  CRYPTUI_HIDE_HIERARCHYPAGE                = $00000001;
  {$EXTERNALSYM CRYPTUI_HIDE_DETAILPAGE}
  CRYPTUI_HIDE_DETAILPAGE                   = $00000002;
  {$EXTERNALSYM CRYPTUI_DISABLE_EDITPROPERTIES}
  CRYPTUI_DISABLE_EDITPROPERTIES            = $00000004;
  {$EXTERNALSYM CRYPTUI_ENABLE_EDITPROPERTIES}
  CRYPTUI_ENABLE_EDITPROPERTIES             = $00000008;
  {$EXTERNALSYM CRYPTUI_DISABLE_ADDTOSTORE}
  CRYPTUI_DISABLE_ADDTOSTORE                = $00000010;
  {$EXTERNALSYM CRYPTUI_ENABLE_ADDTOSTORE}
  CRYPTUI_ENABLE_ADDTOSTORE                 = $00000020;
  {$EXTERNALSYM CRYPTUI_ACCEPT_DECLINE_STYLE}
  CRYPTUI_ACCEPT_DECLINE_STYLE              = $00000040;
  {$EXTERNALSYM CRYPTUI_IGNORE_UNTRUSTED_ROOT}
  CRYPTUI_IGNORE_UNTRUSTED_ROOT             = $00000080;
  {$EXTERNALSYM CRYPTUI_DONT_OPEN_STORES}
  CRYPTUI_DONT_OPEN_STORES                  = $00000100;
  {$EXTERNALSYM CRYPTUI_ONLY_OPEN_ROOT_STORE}
  CRYPTUI_ONLY_OPEN_ROOT_STORE              = $00000200;
  {$EXTERNALSYM CRYPTUI_WARN_UNTRUSTED_ROOT}
  CRYPTUI_WARN_UNTRUSTED_ROOT               = $00000400;  // For use with viewing of certificates on remote
                                                          // machines only.  If this flag is used rghStores[0]
                                                          // must be the handle of the root store on the remote machine.
  {$EXTERNALSYM CRYPTUI_ENABLE_REVOCATION_CHECKING}
  CRYPTUI_ENABLE_REVOCATION_CHECKING        = $00000800;  // This flag is only valid if pCryptProviderData/hWVTStateData
                                                          // is not passed in.
  {$EXTERNALSYM CRYPTUI_WARN_REMOTE_TRUST}
  CRYPTUI_WARN_REMOTE_TRUST                 = $00001000;
  {$EXTERNALSYM CRYPTUI_DISABLE_EXPORT}
  CRYPTUI_DISABLE_EXPORT                    = $00002000;  // If this flag is set, then the "Copy to file" button will be
                                                          // disabled on the Detail page.
  // Revocation flags is only valid if pCryptProviderData/hWVTStateData is not passed in.
  {$EXTERNALSYM CRYPTUI_ENABLE_REVOCATION_CHECK_END_CERT}
  CRYPTUI_ENABLE_REVOCATION_CHECK_END_CERT  = $00004000;
  {$EXTERNALSYM CRYPTUI_ENABLE_REVOCATION_CHECK_CHAIN}
  CRYPTUI_ENABLE_REVOCATION_CHECK_CHAIN     = $00008000;
  {$EXTERNALSYM CRYPTUI_ENABLE_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT}
  CRYPTUI_ENABLE_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT  = CRYPTUI_ENABLE_REVOCATION_CHECKING;  // Changed the default behavior
                                                                                             // to not check root.
  {$EXTERNALSYM CRYPTUI_DISABLE_HTMLLINK}
  CRYPTUI_DISABLE_HTMLLINK                  = $00010000;  // to disable helplink in viewing certificate
  {$EXTERNALSYM CRYPTUI_DISABLE_ISSUERSTATEMENT}
  CRYPTUI_DISABLE_ISSUERSTATEMENT           = $00020000;  // to disable issuer statement button

  {$EXTERNALSYM CRYPTUI_CACHE_ONLY_URL_RETRIEVAL}
  CRYPTUI_CACHE_ONLY_URL_RETRIEVAL          = $00040000;  // to disable online revocation checking
//
// this struct is passed as the lParam in the WM_INITDIALOG call to each
// property sheet that is in the rgPropSheetPages array of the
// CRYPTUI_VIEWCERTIFICATE_STRUCT structure
//
type
  PCryptuiInitdialogStruct = ^TCryptuiInitdialogStruct;
  {$EXTERNALSYM tagCRYPTUI_INITDIALOG_STRUCT}
  tagCRYPTUI_INITDIALOG_STRUCT = record
    lParam: LPARAM;
    pCertContext: PCCERT_CONTEXT;
  end;
  {$EXTERNALSYM CRYPTUI_INITDIALOG_STRUCT}
  CRYPTUI_INITDIALOG_STRUCT = tagCRYPTUI_INITDIALOG_STRUCT;
  {$EXTERNALSYM PCRYPTUI_INITDIALOG_STRUCT}
  PCRYPTUI_INITDIALOG_STRUCT = ^tagCRYPTUI_INITDIALOG_STRUCT;
  TCryptuiInitdialogStruct = tagCRYPTUI_INITDIALOG_STRUCT;

  PCryptuiViewcertificateStructw = ^TCryptuiViewcertificateStructw;
  {$EXTERNALSYM tagCRYPTUI_VIEWCERTIFICATE_STRUCTW}
  tagCRYPTUI_VIEWCERTIFICATE_STRUCTW = record
    dwSize: DWORD;
    hwndParent: HWND;                                  // OPTIONAL
    dwFlags: DWORD;                                    // OPTIONAL
    szTitle: LPCWSTR;                                  // OPTIONAL
    pCertContext: PCCERT_CONTEXT;
    rgszPurposes: ^LPCSTR;                             // OPTIONAL
    cPurposes: DWORD;                                  // OPTIONAL
    case Boolean of
      //union {
      False:(pCryptProviderData  : PCRYPT_PROVIDER_DATA); // OPTIONAL  // Pointer-size
      True: (hWVTStateData       : THandle;               // OPTIONAL  // Pointer-size
      //};
        fpCryptProviderDataTrustedUsage: BOOL;         // OPTIONAL
        idxSigner: DWORD;                              // OPTIONAL
        idxCert: DWORD;                                // OPTIONAL
        fCounterSigner: BOOL;                          // OPTIONAL
        idxCounterSigner: DWORD;                       // OPTIONAL
        cStores: DWORD;                                // OPTIONAL
        rghStores: PHCERTSTORE;                        // OPTIONAL
        cPropSheetPages: DWORD;                        // OPTIONAL
        rgPropSheetPages: LPCPROPSHEETPAGEW;           // OPTIONAL
        nStartPage: DWORD);
    end;
    {$EXTERNALSYM CRYPTUI_VIEWCERTIFICATE_STRUCTW}
    CRYPTUI_VIEWCERTIFICATE_STRUCTW = tagCRYPTUI_VIEWCERTIFICATE_STRUCTW;
    {$EXTERNALSYM PCRYPTUI_VIEWCERTIFICATE_STRUCTW}
    PCRYPTUI_VIEWCERTIFICATE_STRUCTW = ^tagCRYPTUI_VIEWCERTIFICATE_STRUCTW;
    TCryptuiViewcertificateStructw = tagCRYPTUI_VIEWCERTIFICATE_STRUCTW;
    {$EXTERNALSYM PCCRYPTUI_VIEWCERTIFICATE_STRUCTW}
    PCCRYPTUI_VIEWCERTIFICATE_STRUCTW = ^CRYPTUI_VIEWCERTIFICATE_STRUCTW;

  PCryptuiViewcertificateStructa = ^TCryptuiViewcertificateStructa;
  {$EXTERNALSYM tagCRYPTUI_VIEWCERTIFICATE_STRUCTA}
  tagCRYPTUI_VIEWCERTIFICATE_STRUCTA = record
    dwSize: DWORD;
    hwndParent: HWND;                                    // OPTIONAL
    dwFlags: DWORD;                                      // OPTIONAL
    szTitle: LPCSTR;                                     // OPTIONAL
    pCertContext: PCCERT_CONTEXT;
    rgszPurposes: ^LPCSTR;                               // OPTIONAL
    cPurposes: DWORD;                                    // OPTIONAL
    case Integer of
      //union {
      0:(pCryptProviderData  : PCRYPT_PROVIDER_DATA); // OPTIONAL  // Pointer-size
      1: (hWVTStateData      : THandle;               // OPTIONAL  // Pointer-size
      //};
      fpCryptProviderDataTrustedUsage: BOOL;             // OPTIONAL
      idxSigner: DWORD;                                  // OPTIONAL
      idxCert: DWORD;                                    // OPTIONAL
      fCounterSigner: BOOL;                              // OPTIONAL
      idxCounterSigner: DWORD;                           // OPTIONAL
      cStores: DWORD;                                    // OPTIONAL
      rghStores: PHCERTSTORE;                            // OPTIONAL
      cPropSheetPages: DWORD;                            // OPTIONAL
      rgPropSheetPages: LPCPROPSHEETPAGEA;               // OPTIONAL
      nStartPage: DWORD);
  end;
  {$EXTERNALSYM CRYPTUI_VIEWCERTIFICATE_STRUCTA}
  CRYPTUI_VIEWCERTIFICATE_STRUCTA = tagCRYPTUI_VIEWCERTIFICATE_STRUCTA;
  {$EXTERNALSYM PCRYPTUI_VIEWCERTIFICATE_STRUCTA}
  PCRYPTUI_VIEWCERTIFICATE_STRUCTA = ^tagCRYPTUI_VIEWCERTIFICATE_STRUCTA;
  TCryptuiViewcertificateStructa = tagCRYPTUI_VIEWCERTIFICATE_STRUCTA;
  {$EXTERNALSYM PCCRYPTUI_VIEWCERTIFICATE_STRUCTA}
  PCCRYPTUI_VIEWCERTIFICATE_STRUCTA = ^CRYPTUI_VIEWCERTIFICATE_STRUCTA;

//
// pfPropertiesChanged             this will be set by the dialog proc to inform the caller
//                                 if any properties have been changed on certs in the chain
//                                 while the dialog was open
//
{$EXTERNALSYM CryptUIDlgViewCertificateW}
function CryptUIDlgViewCertificateW(
  pCertViewInfo: PCCRYPTUI_VIEWCERTIFICATE_STRUCTW;
  pfPropertiesChanged: PBOOL // OPTIONAL
): BOOL; stdcall;


{$EXTERNALSYM CryptUIDlgViewCertificateA}
function CryptUIDlgViewCertificateA(
  pCertViewInfo: PCCRYPTUI_VIEWCERTIFICATE_STRUCTA;
  pfPropertiesChanged: PBOOL // OPTIONAL
): BOOL; stdcall;

type
{$IFDEF UNICODE}
  {$EXTERNALSYM PCRYPTUI_VIEWCERTIFICATE_STRUCT}
  PCRYPTUI_VIEWCERTIFICATE_STRUCT     = PCRYPTUI_VIEWCERTIFICATE_STRUCTW;
  {$EXTERNALSYM CRYPTUI_VIEWCERTIFICATE_STRUCT}
  CRYPTUI_VIEWCERTIFICATE_STRUCT      = CRYPTUI_VIEWCERTIFICATE_STRUCTW;
  {$EXTERNALSYM PCCRYPTUI_VIEWCERTIFICATE_STRUCT}
  PCCRYPTUI_VIEWCERTIFICATE_STRUCT    = PCCRYPTUI_VIEWCERTIFICATE_STRUCTW;
{$ELSE}
  {$EXTERNALSYM PCRYPTUI_VIEWCERTIFICATE_STRUCT}
  PCRYPTUI_VIEWCERTIFICATE_STRUCT     = PCRYPTUI_VIEWCERTIFICATE_STRUCTA;
  {$EXTERNALSYM CRYPTUI_VIEWCERTIFICATE_STRUCT}
  CRYPTUI_VIEWCERTIFICATE_STRUCT      = CRYPTUI_VIEWCERTIFICATE_STRUCTA;
  {$EXTERNALSYM PCCRYPTUI_VIEWCERTIFICATE_STRUCT}
  PCCRYPTUI_VIEWCERTIFICATE_STRUCT    = PCCRYPTUI_VIEWCERTIFICATE_STRUCTA;
{$ENDIF}

{$EXTERNALSYM CryptUIDlgViewCertificate}
function CryptUIDlgViewCertificate(
  pCertViewInfo: PCCRYPTUI_VIEWCERTIFICATE_STRUCT;
  pfPropertiesChanged: PBOOL // OPTIONAL
): BOOL; stdcall;


//-------------------------------------------------------------------------
//
//  Valid values for dwSubjectChoice in CRYPTUI_WIZ_EXPORT_INFO
//-------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_CERT_CONTEXT}
  CRYPTUI_WIZ_EXPORT_CERT_CONTEXT                 = 1;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_CTL_CONTEXT}
  CRYPTUI_WIZ_EXPORT_CTL_CONTEXT                  = 2;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_CRL_CONTEXT}
  CRYPTUI_WIZ_EXPORT_CRL_CONTEXT                  = 3;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_CERT_STORE}
  CRYPTUI_WIZ_EXPORT_CERT_STORE                   = 4;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_CERT_STORE_CERTIFICATES_ONLY}
  CRYPTUI_WIZ_EXPORT_CERT_STORE_CERTIFICATES_ONLY = 5;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_FORMAT_CRL}
  CRYPTUI_WIZ_EXPORT_FORMAT_CRL                   = 6;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_FORMAT_CTL}
  CRYPTUI_WIZ_EXPORT_FORMAT_CTL                   = 7;

//-------------------------------------------------------------------------
//
//  Struct to define the object to be exported and where to export it to
//
//  CRYPTUI_WIZ_EXPORT_SUBJECT_INFO
//
//-------------------------------------------------------------------------
type
  PCryptuiWizExportInfo = ^TCryptuiWizExportInfo;
  {$EXTERNALSYM _CRYPTUI_WIZ_EXPORT_INFO}
  _CRYPTUI_WIZ_EXPORT_INFO = record
    dwSize: DWORD;
         //Required: should be set to sizeof(CRYPTUI_WIZ_EXPORT_INFO)
    pwszExportFileName: LPCWSTR;
         //Required if the CRYPTUI_WIZ_NO_UI flag is set, Optional otherwise.
         //The fully qualified file name to export to, if this is
         //non-NULL and the CRYPTUI_WIZ_NO_UI flag is NOT set, then it is
         //displayed to the user as the default file name
    case dwSubjectChoice: DWORD of
      //Required: indicate the type of the subject:
      //          If can one of the following:
      //          CRYPTUI_WIZ_EXPORT_CERT_CONTEXT
      //          CRYPTUI_WIZ_EXPORT_CTL_CONTEXT
      //          CRYPTUI_WIZ_EXPORT_CRL_CONTEXT
      //          CRYPTUI_WIZ_EXPORT_CERT_STORE
      //          CRYPTUI_WIZ_EXPORT_CERT_STORE_CERTIFICATES_ONLY
      // union {
      CRYPTUI_WIZ_EXPORT_CERT_CONTEXT:              // all pointer-size
        (pCertContext : PCCERT_CONTEXT);
      CRYPTUI_WIZ_EXPORT_CTL_CONTEXT:
        (pCTLContext  : PCCTL_CONTEXT);
      CRYPTUI_WIZ_EXPORT_CRL_CONTEXT:
        (pCRLContext  : PCCRL_CONTEXT);
      CRYPTUI_WIZ_EXPORT_CERT_STORE:
        (hCertStore   : HCERTSTORE;
      //};
        cStores: DWORD;
        // Optional: count of extra stores to search for the certs in the
        //           trust chain if the chain is being exported with a cert.
        //           this is ignored if dwSubjectChoice is anything other
        //           than CRYPTUI_WIZ_EXPORT_CERT_CONTEXT
        rghStores: PHCERTSTORE);
        // Optional: array of extra stores to search for the certs in the
        //           trust chain if the chain is being exported with a cert.
        //           this is ignored if dwSubjectChoice is anything other
        //           than CRYPTUI_WIZ_EXPORT_CERT_CONTEXT
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_INFO}
  CRYPTUI_WIZ_EXPORT_INFO = _CRYPTUI_WIZ_EXPORT_INFO;
  {$EXTERNALSYM PCRYPTUI_WIZ_EXPORT_INFO}
  PCRYPTUI_WIZ_EXPORT_INFO = ^_CRYPTUI_WIZ_EXPORT_INFO;
  TCryptuiWizExportInfo = _CRYPTUI_WIZ_EXPORT_INFO;

  {$EXTERNALSYM PCCRYPTUI_WIZ_EXPORT_INFO}
  PCCRYPTUI_WIZ_EXPORT_INFO = ^CRYPTUI_WIZ_EXPORT_INFO;


//-------------------------------------------------------------------------
//
//  Valid values for dwExportFormat in CRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO
//-------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_FORMAT_DER}
  CRYPTUI_WIZ_EXPORT_FORMAT_DER                   = 1;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_FORMAT_PFX}
  CRYPTUI_WIZ_EXPORT_FORMAT_PFX                   = 2;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_FORMAT_PKCS7}
  CRYPTUI_WIZ_EXPORT_FORMAT_PKCS7                 = 3;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_FORMAT_BASE64}
  CRYPTUI_WIZ_EXPORT_FORMAT_BASE64                = 4;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_FORMAT_SERIALIZED_CERT_STORE}
  CRYPTUI_WIZ_EXPORT_FORMAT_SERIALIZED_CERT_STORE = 5;  // NOTE: not currently supported!!

//-------------------------------------------------------------------------
//
//  Struct to define the information needed to export a CERT_CONTEXT
//
//  CRYPTUI_WIZ_EXPORT_NOUI_INFO
//
//-------------------------------------------------------------------------
type
  PCryptuiWizExportCertcontextInfo = ^TCryptuiWizExportCertcontextInfo;
  {$EXTERNALSYM _CRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO}
  _CRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO = record
    dwSize: DWORD;
    //Required: should be set to sizeof(CRYPTUI_WIZ_EXPORT_NOUI_INFO)
    dwExportFormat: DWORD;
    //Required:
    //          It can be one of the following:
    //          CRYPTUI_WIZ_EXPORT_FORMAT_DER
    //          CRYPTUI_WIZ_EXPORT_FORMAT_PFX
    //          CRYPTUI_WIZ_EXPORT_FORMAT_PKCS7
    //          CRYPTUI_WIZ_EXPORT_FORMAT_SERIALIZED_CERT_STORE
    fExportChain: BOOL;
    //Required
    fExportPrivateKeys: BOOL;
    //Required
    pwszPassword: LPCWSTR;
    //Required if the fExportPrivateKeys boolean is TRUE, otherwise,
    //it is ignored
    fStrongEncryption: BOOL;
    //Required if dwExportFormat is CRYPTUI_WIZ_EXPORT_FORMAT_PFX
    //Note that if this flag is TRUE then the PFX blob produced is
    //NOT compatible with IE4.
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO}
  CRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO = _CRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO;
  {$EXTERNALSYM PCRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO}
  PCRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO = ^_CRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO;
  TCryptuiWizExportCertcontextInfo = _CRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO;

  {$EXTERNALSYM PCCRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO}
  PCCRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO = ^CRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO;

//-----------------------------------------------------------------------
//
// CryptUIWizExport
//
//  The export wizard to export public key related objects to a file
//
//  If dwFlags is set to CRYPTUI_WIZ_NO_UI, no UI will be shown.  Otherwise,
//  User will be prompted for input through a wizard.
//
//  If CRYPTUI_WIZ_NO_UI is set in dwFlags:
//      hwndParent:         Ignored
//      pwszWizardTitle:    Ignored
//      pExportInfo:        IN Required:    The subject to export.
//      pvoid:              IN Required:    Contains information about how to do the export based on what
//                                          is being exported
//
//                                          dwSubjectChoice                     INPUT TYPE
//                                          -------------------------------------------------------------------------
//                                          CRYPTUI_WIZ_EXPORT_CERT_CONTEXT     PCCRYPTUI_WIZ_EXPORT_CERTCONTEXT_INFO
//                                          CRYPTUI_WIZ_EXPORT_CTL_CONTEXT      NULL
//                                          CRYPTUI_WIZ_EXPORT_CRL_CONTEXT      NULL
//                                          CRYPTUI_WIZ_EXPORT_CERT_STORE       NULL
//
//  If CRYPTUI_WIZ_NO_UI is not set in dwFlags:
//      hwndPrarent:        IN Optional:    The parent window for the wizard
//      pwszWizardTitle:    IN Optional:    The title of the wizard
//                                          If NULL, the default will be IDS_EXPORT_WIZARD_TITLE
//      pExportInfo:        IN Required:    The subject to export.
//      pvoid:              IN Optional:    Contains information about how to do the export based on what
//                                          is being exported.  See above table for values, if this is non-NULL
//                                          the values are displayed to the user as the default choices.
//------------------------------------------------------------------------
{$EXTERNALSYM CryptUIWizExport}
function CryptUIWizExport(
  dwFlags: DWORD;
  hwndParent: HWND;
  pwszWizardTitle: LPCWSTR;
  pExportInfo: PCCRYPTUI_WIZ_EXPORT_INFO;
  pvoid: Pointer
): BOOL; stdcall;


//-------------------------------------------------------------------------
//
//  Valid values for dwSubjectChoice in IMPORT_SUBJECT_INFO
//-------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_SUBJECT_FILE}
  CRYPTUI_WIZ_IMPORT_SUBJECT_FILE         = 1;
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_SUBJECT_CERT_CONTEXT}
  CRYPTUI_WIZ_IMPORT_SUBJECT_CERT_CONTEXT = 2;
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_SUBJECT_CTL_CONTEXT}
  CRYPTUI_WIZ_IMPORT_SUBJECT_CTL_CONTEXT  = 3;
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_SUBJECT_CRL_CONTEXT}
  CRYPTUI_WIZ_IMPORT_SUBJECT_CRL_CONTEXT  = 4;
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_SUBJECT_CERT_STORE}
  CRYPTUI_WIZ_IMPORT_SUBJECT_CERT_STORE   = 5;

//-------------------------------------------------------------------------
//
//  Struct to define the subject CertImportWizard
//
//  CRYPTUI_WIZ_IMPORT_SUBJECT_INFO
//
//-------------------------------------------------------------------------
type
  PCryptuiWizImportSrcInfo = ^TCryptuiWizImportSrcInfo;
  {$EXTERNALSYM _CRYPTUI_WIZ_IMPORT_SUBJECT_INFO}
  _CRYPTUI_WIZ_IMPORT_SUBJECT_INFO = record
    dwSize: DWORD;
    //Required: should be set to sizeof(IMPORT_SUBJECT_INFO)
    case dwSubjectChoice: DWORD of
      //Required:   indicate the type of the subject:
      //          If can one of the following:
      //          CRYPTUI_WIZ_IMPORT_SUBJECT_FILE
      //          CRYPTUI_WIZ_IMPORT_SUBJECT_CERT_CONTEXT
      //          CRYPTUI_WIZ_IMPORT_SUBJECT_CTL_CONTEXT
      //          CRYPTUI_WIZ_IMPORT_SUBJECT_CRL_CONTEXT
      //          CRYPTUI_WIZ_IMPORT_SUBJECT_CERT_STORE
      // union {
      CRYPTUI_WIZ_IMPORT_SUBJECT_FILE:
        (pwszFileName : LPCWSTR);
      CRYPTUI_WIZ_IMPORT_SUBJECT_CERT_CONTEXT:
        (pCertContext : PCCERT_CONTEXT);
      CRYPTUI_WIZ_IMPORT_SUBJECT_CTL_CONTEXT:
        (pCTLContext  : PCCTL_CONTEXT);
      CRYPTUI_WIZ_IMPORT_SUBJECT_CRL_CONTEXT:
        (pCRLContext  : PCCRL_CONTEXT);
      CRYPTUI_WIZ_IMPORT_SUBJECT_CERT_STORE:
        (hCertStore   : HCERTSTORE;
      // };
        dwFlags: DWORD;
        //Required if pwszFileName contains a PFX BLOB.
        //Ignored otherwise
        //This is the same flag for PFXImportCertStore
        pwszPassword: LPCWSTR);
        //Required if pwszFileName contains a PFX BLOB.
        //ignored otherwise
  end;
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_SRC_INFO}
  CRYPTUI_WIZ_IMPORT_SRC_INFO = _CRYPTUI_WIZ_IMPORT_SUBJECT_INFO;
  {$EXTERNALSYM PCRYPTUI_WIZ_IMPORT_SRC_INFO}
  PCRYPTUI_WIZ_IMPORT_SRC_INFO = ^_CRYPTUI_WIZ_IMPORT_SUBJECT_INFO;
  TCryptuiWizImportSrcInfo = _CRYPTUI_WIZ_IMPORT_SUBJECT_INFO;

  {$EXTERNALSYM PCCRYPTUI_WIZ_IMPORT_SRC_INFO}
  PCCRYPTUI_WIZ_IMPORT_SRC_INFO = ^CRYPTUI_WIZ_IMPORT_SRC_INFO;

//-----------------------------------------------------------------------
//
// Valid flags for dwFlags in CryptUIWizImport
//
//-----------------------------------------------------------------------
//if this flag is set in dwFlags, user will not be allowed to change
//the hDesCertStore in the wizard page
const
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_NO_CHANGE_DEST_STORE}
  CRYPTUI_WIZ_IMPORT_NO_CHANGE_DEST_STORE = $00010000;

//Allow importing certificate
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_ALLOW_CERT}
  CRYPTUI_WIZ_IMPORT_ALLOW_CERT           = $00020000;

//Allow importing certificate revocation list
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_ALLOW_CRL}
  CRYPTUI_WIZ_IMPORT_ALLOW_CRL            = $00040000;

//Allow importing certificate trust list
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_ALLOW_CTL}
  CRYPTUI_WIZ_IMPORT_ALLOW_CTL            = $00080000;

//import contents to local machine (currently only applicable for PFX imports)
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_TO_LOCALMACHINE}
  CRYPTUI_WIZ_IMPORT_TO_LOCALMACHINE      = $00100000;

//import contents to current user (currently only applicable for PFX imports)
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_TO_CURRENTUSER}
  CRYPTUI_WIZ_IMPORT_TO_CURRENTUSER       = $00200000;

//if the hDesCertStore is a remote store handle, this flag should be set
  {$EXTERNALSYM CRYPTUI_WIZ_IMPORT_REMOTE_DEST_STORE}
  CRYPTUI_WIZ_IMPORT_REMOTE_DEST_STORE    = $00400000;

//-----------------------------------------------------------------------
//
// CryptUIWizImport
//
//  The import wizard to import public key related files to a certificate
//  store
//
//  dwFlags can be set to any combination of the following flags:
//  CRYPTUI_WIZ_NO_UI                           No UI will be shown.  Otherwise, User will be
//                                              prompted by a wizard.
//  CRYPTUI_WIZ_IMPORT_ALLOW_CERT               Allow importing certificate
//  CRYPTUI_WIZ_IMPORT_ALLOW_CRL                Allow importing CRL(certificate revocation list)
//  CRYPTUI_WIZ_IMPORT_ALLOW_CTL                Allow importing CTL(certificate trust list)
//  CRYPTUI_WIZ_IMPORT_NO_CHANGE_DEST_STORE     user will not be allowed to change
//                                              the hDesCertStore in the wizard page
//  CRYPTUI_WIZ_IMPORT_TO_LOCALMACHINE          the contents should be imported to local machine
//                                              (currently only applicable for PFX imports)
//  CRYPTUI_WIZ_IMPORT_TO_CURRENTUSER           the contents should be imported to current user
//                                              (currently only applicable for PFX imports)
//
//  Please notice that if neither of following three flags is in dwFlags, default to is
//  allow everything.
//  CRYPTUI_WIZ_IMPORT_ALLOW_CERT
//  CRYPTUI_WIZ_IMPORT_ALLOW_CRL
//  CRYPTUI_WIZ_IMPORT_ALLOW_CTL
//
//  Also, note that the CRYPTUI_WIZ_IMPORT_TO_LOCALMACHINE and CRYPTUI_WIZ_IMPORT_TO_CURRENTUSER
//  flags are used force the content of a pfx blob into either local machine or current user.
//  If neither of these flags are used and hDesCertStore is NULL then:
//  1) The private key in the pfx blob will be forced to be imported into current user.
//  2) If CRYPTUI_WIZ_NO_UI is NOT set, the wizard will prompt the user to select a certificate
//     store from the current user stores.
//
//
//
//  If CRYPTUI_WIZ_NO_UI is set in dwFlags:
//      hwndParent:         Ignored
//      pwszWizardTitle:    Ignored
//      pImportSubject:     IN Required:    The subject to import.
//      hDesCertStore:      IN Optional:    The destination certficate store
//
//  If CRYPTUI_WIZ_NO_UI is not set in dwFlags:
//      hwndPrarent:        IN Optional:    The parent window for the wizard
//      pwszWizardTitle:    IN Optional:    The title of the wizard
//                                          If NULL, the default will be IDS_IMPORT_WIZARD_TITLE
//      pImportSubject:     IN Optional:    The file name to import.
//                                          If NULL, the wizard will prompt user to enter the file name
//      hDesCertStore:      IN Optional:    The destination certificate store where the file wil be
//                                          imported to.  The store should be opened with
//                                          flag CERT_STORE_SET_LOCALIZED_NAME_FLAG.  If NULL, the wizard will prompt user to select
//                                          a certificate store.
//------------------------------------------------------------------------
{$EXTERNALSYM CryptUIWizImport}
function CryptUIWizImport(
  dwFlags: DWORD;
  hwndParent: HWND;
  pwszWizardTitle: LPCWSTR;
  pImportSrc: PCCRYPTUI_WIZ_IMPORT_SRC_INFO;
  hDestCertStore: HCERTSTORE
): BOOL; stdcall;

implementation

const
  CryptUI = 'Cryptui.dll';

{$IFDEF WIN7_UP}
function CertSelectionGetSerializedBlob; external CryptUI name 'CertSelectionGetSerializedBlob';
{$ENDIF}
function CryptUIDlgCertMgr; external CryptUI name 'CryptUIDlgCertMgr';
function CryptUIDlgSelectCertificateFromStore; external CryptUI name 'CryptUIDlgSelectCertificateFromStore';
function CryptUIDlgViewCertificateA; external CryptUI name 'CryptUIDlgViewCertificateA';
function CryptUIDlgViewCertificateW; external CryptUI name 'CryptUIDlgViewCertificateW';
{$IFDEF UNICODE}
function CryptUIDlgViewCertificate; external CryptUI name 'CryptUIDlgViewCertificateW';
{$ELSE}
function CryptUIDlgViewCertificate; external CryptUI name 'CryptUIDlgViewCertificateA';
{$ENDIF}
function CryptUIDlgViewContext; external CryptUI name 'CryptUIDlgViewContext';
function CryptUIWizDigitalSign; external CryptUI name 'CryptUIWizDigitalSign';
function CryptUIWizExport; external CryptUI name 'CryptUIWizExport';
function CryptUIWizFreeDigitalSignContext; external CryptUI name 'CryptUIWizFreeDigitalSignContext';
function CryptUIWizImport; external CryptUI name 'CryptUIWizImport';

end.

