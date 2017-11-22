{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  MS crypto API utilities. These allow checking and validation of
              SSL certificates using the Windows root certificate store to
              avoid applications needing to include their own file of root
              PEM certificates, also includes certificate Revocation checks,
              warning these are slow since they need to access remote web sites.
              See sample OverbyteIcsMsVerify for usage and demos 
Creation:     May 2011
Version:      8.00
EMail:        arno.garrels@gmx.de
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2015 by Arno Garrels, Berlin <arno.garrels@gmx.de>

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


History:
June 2015 - V8.00 Angus moved to main source dir
                  now using OverbyteIcsWinCrypt and OverbyteIcsCryptUiApi


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMsSslUtils;

{$I include\OverbyteIcsDefs.inc}

interface

uses
  Windows, SysUtils, SysConst,
  OverbyteIcsWinCrypt, OverbyteIcsCryptUiApi,
  OverbyteIcsLibeay, OverbyteIcsSsleay,
  OverbyteIcsWSocket;

type
  EMsCrypto = class(Exception);
{$IFNDEF COMPILER12_UP}
//  UnicodeString = type WideString;  // angus not used  May 2015
{$ENDIF}

  TWinDlgStoreType = (wdstRoot, wdstCA, wdstPersonal, wdstEnterpriseTrust,
                      wdstTrustedPublisher, wdstTrustedPeople,
                      wdstOtherPeople, wdst3PartyRoot, wdstCertRequests,
                      wdstDisallowed);

  EMsCertChainEngine = Exception;

  TMsVerifyOptions = set of (mvoRevocationCheckEndCert,
                             mvoRevocationCheckChain,
                             mvoRevocationCheckChainExcludeRoot,
                             mvoRevocationCheckCacheOnly,
                             mvoRevocationAccumulativeTimeOut,
                             mvoCertChainDisableAuthRootAutoUpdate);

  TMsCertChainEngine = class(TObject)
  private
    FhCertChainEngine         : HCERTCHAINENGINE;
    FCertEngineConfig         : TCertChainEngineConfig;
    FEnhkeyUsage              : CERT_ENHKEY_USAGE;
    FCertUsage                : TCertUsageMatch;
    FChainPara                : TCertChainPara;
    FCurCertCtx               : PCCERT_CONTEXT;
    FhCurTempStore            : HCERTSTORE;
    FUrlRetrievalTimeoutMsec  : LongWord;
    FVerifyOptions            : TMsVerifyOptions;
    FMsVerifyOptions          : LongWord;
    FhCAStore                 : HCERTSTORE;
    FhRootStore               : HCERTSTORE;
    procedure Init;
    procedure Deinit;
    procedure SetUrlRetrievalTimeoutMsec(const Value: LongWord);
    procedure SetVerifyOptions(const Value: TMsVerifyOptions);
    function  InternalViewCert(ACertCtx: PCCERT_CONTEXT; AHwnd: HWND;
      AEnableAddToStore: Boolean; ATitle: PChar): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function VerifyCert(ACert: TX509Base; ACertChain: TX509List;
      out AChainVerifyResult: LongWord; AUpdateCertChain: Boolean = TRUE): Boolean;
    function ViewCertLastVerified(AHwnd: HWND = 0;
      ADlgEnableAddToStore: Boolean = True; ATitle: PChar = nil): Boolean;
    function ViewCertVerified(ACert: TX509Base; AHwnd: HWND = 0;
      ADlgEnableAddToStore: Boolean = True; ATitle: PChar = nil): Boolean;
    function ViewCert(ACert: TX509Base; ACertChain: TX509List;
      AHwnd: HWND = 0; ADlgEnableAddToStore: Boolean = True;
      ATitle: PChar = nil): Boolean;
    property UrlRetrievalTimeoutMsec: LongWord
                                           read  FUrlRetrievalTimeoutMsec
                                           write SetUrlRetrievalTimeoutMsec;
    property VerifyOptions: TMsVerifyOptions
                                           read  FVerifyOptions
                                           write SetVerifyOptions;
  end;

  function X509ToMsCert(x: PX509): PCCERT_CONTEXT;
  function MsCertToX509(x: PCCERT_CONTEXT): PX509;
  function MsChainVerifyErrorToStr(const ErrCode: LongWord): string;
  function MsCertVerifyErrorToStr(const ErrCode: LongWord): string;

implementation

const
  CRYPT_E_REVOKED     = $80092010;
  CRYPT_E_EXISTS      = $80092005;
  { http://technet.microsoft.com/en-us/library/bb457027.aspx }
  {WinDlgStoreTypeStrings : array [TWinDlgStoreType] of string =
  ('Root', 'CA', 'My', 'Trust', 'TrustedPublisher', 'TrustedPeople',
   'Addressbook', 'AuthRoot', 'REQUEST', 'Disallowed'); }
   
  sCryptoApiError   = 'CryptoAPI Error. Code: %d.'+#13#10+'%s';
  sUnKnownCryptoApi = 'A call to a CryptoAPI function failed';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RaiseOSSLError(AErrCode: LongWord);
begin
  if AErrCode > 0 then
    raise EOpenSslError.CreateFmt('OpenSSL Error #%d.'+ #13#10 + '%s',
                                  [AErrCode, OpenSslErrMsg(AErrCode)])
  else
    raise EOpenSslError.Create('A call to an OpenSSL function failed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RaiseLastOSSLError;
begin
  RaiseOSSLError(f_ERR_get_error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RaiseMsCryptoError(LastError: LongWord);
begin
  if LastError <> 0 then
    raise EMsCrypto.CreateFmt(sCryptoApiError, [LastError,
                              SysErrorMessage(LastError)])
  else
    raise EMsCrypto.Create(sUnKnownCryptoApi);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RaiseMyLastCryptoError;
var
  ErrCode: LongWord;
begin
  ErrCode := GetLastError;
  if ErrCode > 0 then
    RaiseMsCryptoError(ErrCode)
  else begin
    ErrCode := f_ERR_get_error;
    //f_ERR_clear_error;
    RaiseOSSLError(ErrCode);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function X509ToMsCert(x: PX509): PCCERT_CONTEXT;
var
  Buf, P: PByte;
  Len: Integer;
begin
  Result := nil;
  Len := f_i2d_X509(x, nil);
  if Len > 0 then
  begin
    GetMem(Buf, Len);
    try
      P := Buf; // This is important since f_i2d_X509 increments P by Len
      Len := f_i2d_X509(x, @P);
      if Len > 0 then
        Result := CertCreateCertificateContext(X509_ASN_ENCODING, Buf, Len);
    finally
      FreeMem(Buf);
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MsCertToX509(x: PCCERT_CONTEXT): PX509;
var
  p: PByte;
begin
  p := x.pbCertEncoded; // Use of a temporary variable is mandatory
  Result := f_d2i_X509(nil, @p, x.cbCertEncoded);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function InternalChainVerifyErrorToStr(const ErrCode: LongWord): string;
begin
  Result := '';
  if ErrCode and CERT_TRUST_IS_NOT_TIME_VALID = CERT_TRUST_IS_NOT_TIME_VALID then
    Result := Result + 'One of the certificates in the certificate chain is not time valid.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_TIME_NESTED = CERT_TRUST_IS_NOT_TIME_NESTED then
    Result := Result + 'One of the certificates in the certificate chain is not time nested.'#13#10; //??
  if ErrCode and CERT_TRUST_IS_REVOKED = CERT_TRUST_IS_REVOKED then
    Result := Result + 'Trust for one of the certificates in the certificate chain has been revoked.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_SIGNATURE_VALID = CERT_TRUST_IS_NOT_SIGNATURE_VALID then
    Result := Result + 'One of the certificates in the certificate chain does not have a valid signature.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_VALID_FOR_USAGE = CERT_TRUST_IS_NOT_VALID_FOR_USAGE then
    Result := Result + 'The certificate chain is not valid for its proposed usage.'#13#10;
  if ErrCode and CERT_TRUST_IS_UNTRUSTED_ROOT = CERT_TRUST_IS_UNTRUSTED_ROOT then
    Result := Result + 'The certificate chain is based on an untrusted root.'#13#10;
  if ErrCode and CERT_TRUST_REVOCATION_STATUS_UNKNOWN = CERT_TRUST_REVOCATION_STATUS_UNKNOWN then
    Result := Result + 'The revocation status of one of the certificates in the certificate chain is unknown.'#13#10;
  if ErrCode and CERT_TRUST_IS_CYCLIC = CERT_TRUST_IS_CYCLIC then
    Result := Result + 'One of the certificates in the chain was issued by a certification authority that the original certificate had certified.'#13#10;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function InternalCertVerifyErrorToStr(const ErrCode: LongWord): string;
begin
  Result := '';
  if ErrCode and CERT_TRUST_IS_NOT_TIME_VALID = CERT_TRUST_IS_NOT_TIME_VALID then
    Result := Result + 'This certificate is not time valid.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_TIME_NESTED = CERT_TRUST_IS_NOT_TIME_NESTED then
    Result := Result + 'This certificate is not time nested.'#13#10; //??
  if ErrCode and CERT_TRUST_IS_REVOKED = CERT_TRUST_IS_REVOKED then
    Result := Result + 'Trust for this certificate has been revoked.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_SIGNATURE_VALID = CERT_TRUST_IS_NOT_SIGNATURE_VALID then
    Result := Result + 'The certificate does not have a valid signature.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_VALID_FOR_USAGE = CERT_TRUST_IS_NOT_VALID_FOR_USAGE then
    Result := Result + 'The certificate is not valid for its proposed usage.'#13#10;
  if ErrCode and CERT_TRUST_IS_UNTRUSTED_ROOT = CERT_TRUST_IS_UNTRUSTED_ROOT then
    Result := Result + 'The certificate is based on an untrusted root.'#13#10;
  if ErrCode and CERT_TRUST_REVOCATION_STATUS_UNKNOWN = CERT_TRUST_REVOCATION_STATUS_UNKNOWN then
    Result := Result + 'The revocation status of the certificate is unknown.'#13#10;
  if ErrCode and CERT_TRUST_IS_CYCLIC = CERT_TRUST_IS_CYCLIC then
    Result := Result + 'One of the certificates in the chain was issued by a certification authority that the original certificate had certified.'#13#10;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MsChainVerifyErrorToStr(const ErrCode: LongWord): string;
begin
  if ErrCode = 0 then
    Result := 'No error found for this chain.'
  else begin
    Result := '';
    if ErrCode and CERT_TRUST_IS_PARTIAL_CHAIN = CERT_TRUST_IS_PARTIAL_CHAIN then
      Result := Result + 'The certificate chain is not complete.'#13#10;
    if ErrCode and CERT_TRUST_CTL_IS_NOT_TIME_VALID = CERT_TRUST_CTL_IS_NOT_TIME_VALID then
      Result := Result + 'A certificate trust list (CTL) used to create this chain was not time valid.'#13#10;
    if ErrCode and CERT_TRUST_CTL_IS_NOT_SIGNATURE_VALID = CERT_TRUST_CTL_IS_NOT_SIGNATURE_VALID then
      Result := Result + 'A CTL used to create this chain did not have a valid signature.'#13#10;
    if ErrCode and CERT_TRUST_CTL_IS_NOT_VALID_FOR_USAGE = CERT_TRUST_CTL_IS_NOT_VALID_FOR_USAGE then
      Result := Result + 'A CTL used to create this chain is not valid for this usage.'#13#10;
    Result := Result + InternalChainVerifyErrorToStr(ErrCode);
    while (Length(Result) > 0) and (Ord(Result[Length(Result)]) in [$0d, $0a]) do
      SetLength(Result, Length(Result) - 1);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MsCertVerifyErrorToStr(const ErrCode: LongWord): string;
begin
  if ErrCode = 0 then
    Result := 'No error found for this certificate.'
  else begin
    Result := '';
    Result := Result + InternalCertVerifyErrorToStr(ErrCode);
    while (Length(Result) > 0) and (Ord(Result[Length(Result)]) in [$0d, $0a]) do
      SetLength(Result, Length(Result) - 1);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TCertChainEngine }

constructor TMsCertChainEngine.Create;
begin
  inherited Create;
  FhCAStore    := CertOpenSystemStore(0, 'CA');
  if FhCAStore = nil then
    RaiseMsCryptoError(GetLastError);
  FhRootStore  := CertOpenSystemStore(0, 'Root');
  if FhRootStore = nil then
    RaiseMsCryptoError(GetLastError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsCertChainEngine.Deinit;
begin
  if FhCertChainEngine <> 0 then begin
      CertFreeCertificateChainEngine(FhCertChainEngine);
     FhCertChainEngine := 0;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMsCertChainEngine.Destroy;
begin
  DeInit;
  if FCurCertCtx <> nil then
    CertFreeCertificateContext(FCurCertCtx);
  if FhCurTempStore <> nil then
    CertCloseStore(FhCurTempStore, 0);
  if FhCAStore <> nil then
    CertCloseStore(FhCAStore, 0);
  if FhRootStore <> nil then
    CertCloseStore(FhRootStore, 0);
  inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsCertChainEngine.Init;
begin
  DeInit;
  FEnhkeyUsage.cUsageIdentifier           := 0;
  FEnhkeyUsage.rgpszUsageIdentifier       := nil;

  FCertUsage.dwType                       := USAGE_MATCH_TYPE_AND;
  FCertUsage.Usage                        := FEnhkeyUsage;

  FChainPara.cbSize                       := SizeOf(TCertChainPara);
  FChainPara.RequestedUsage               := FCertUsage;

  FillChar(FCertEngineConfig, SizeOf(FCertEngineConfig), 0);
  FCertEngineConfig.cbSize                     := SizeOf(FCertEngineConfig);
  //FCertEngineConfig.hRestrictedRoot            := nil;
  //FCertEngineConfig.hRestrictedTrust           := nil;
  //FCertEngineConfig.hRestrictedOther           := nil;
  //FCertEngineConfig.cAdditionalStore           := 0;
  //FCertEngineConfig.rghAdditionalStore         := nil;
  FCertEngineConfig.dwFlags                    := CERT_CHAIN_CACHE_END_CERT;
  FCertEngineConfig.dwUrlRetrievalTimeout      := FUrlRetrievalTimeoutMsec;
  //FChainConfig.MaximumCachedCertificates       := 0;
  //FChainConfig.CycleDetectionModulus           := 0;
  if not CertCreateCertificateChainEngine(@FCertEngineConfig, FhCertChainEngine) then
      RaiseMsCryptoError(GetLastError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function FindX509InChain(x: PX509; AChain: TX509List): Integer;
var
  Len: Integer;
  Hash: AnsiString;
  I: Integer;
begin
  Assert(Assigned(x) and Assigned(AChain));
  Len := 20;
  SetLength(Hash, Len);
  f_X509_digest(x, f_EVP_sha1, PAnsiChar(Hash), @Len);
  for I := 0 to AChain.Count -1 do
  begin
    if CompareMem(Pointer(AChain[I].Sha1Hash), Pointer(Hash), Len) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsCertChainEngine.SetUrlRetrievalTimeoutMsec(const Value: LongWord);
begin
  if Value <> FUrlRetrievalTimeoutMsec then
  begin
    FUrlRetrievalTimeoutMsec := Value;
    Init;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsCertChainEngine.SetVerifyOptions(const Value: TMsVerifyOptions);
begin
  if FVerifyOptions <> Value then
  begin
    FVerifyOptions := Value;
    FMsVerifyOptions := 0;
    if mvoRevocationCheckEndCert in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_CHECK_END_CERT;
    if mvoRevocationCheckChain in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_CHECK_CHAIN;
    if mvoRevocationCheckChainExcludeRoot in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT;
    if mvoRevocationCheckCacheOnly in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_CHECK_CACHE_ONLY;
    if mvoRevocationAccumulativeTimeOut in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_ACCUMULATIVE_TIMEOUT;
    if mvoCertChainDisableAuthRootAutoUpdate in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_DISABLE_AUTH_ROOT_AUTO_UPDATE;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMsCertChainEngine.VerifyCert(ACert: TX509Base; ACertChain: TX509List;
  out AChainVerifyResult: LongWord; AUpdateCertChain: Boolean = TRUE): Boolean;
type
  TCertSimpleChain   = array [0..0] of PCERT_SIMPLE_CHAIN;
  PCertSimpleChain   = ^TCertSimpleChain;
  TCertChainElements = array [0..31] of PCERT_CHAIN_ELEMENT;
  PCertChainElements = ^TCertChainElements;

var
  pMsCertCtx      : PCCERT_CONTEXT;
  pTemp           : PCCERT_CONTEXT;
  I{, J}          : Integer;
  PChainElements  : PCertChainElements;
  PChainElement   : PCERT_CHAIN_ELEMENT;
  PSimpleChain    : PCertSimpleChain;
  x               : PX509;
  ChainContext    : PCCERT_CHAIN_CONTEXT;
  Err             : LongWord;
begin
  AChainVerifyResult := LongWord(-1);
  Assert(ACert <> nil);
  try
    if ACert.X509 = nil then
      raise EMsCertChainEngine.Create('Invalid parameter. ACert.X509 is nil');

    if FCurCertCtx <> nil then
    begin
      CertFreeCertificateContext(FCurCertCtx);
      FCurCertCtx := nil;
    end;

    if FhCurTempStore = nil then
    begin
      FhCurTempStore := CertOpenStore(sz_CERT_STORE_PROV_MEMORY,
                                      X509_ASN_ENCODING, 0, 0, nil);
      if FhCurTempStore = nil then
        RaiseMsCryptoError(GetLastError);
    end;

    if FhCertChainEngine = 0 then
      Init;

    if Assigned(ACertChain) and (ACertChain.Count > 0) then
    begin
      for I := ACertChain.Count -1 downto 0 do
      begin
        { We store all certificates except the end certificate if they are }
        { not in the system store temporarily.                             }
        if ACert.SameHash(ACertChain[I]) then
          Continue;
        pMsCertCtx := X509ToMsCert(ACertChain[I].X509);
        if pMsCertCtx = nil then
          RaiseMyLastCryptoError;
        try
          if f_X509_check_issued(ACertChain[I].X509, ACertChain[I].X509) = 0 then
            { Self-signed Root }
            pTemp := CertFindCertificateInStore(FhRootStore,
                      X509_ASN_ENCODING, 0, CERT_FIND_EXISTING, pMsCertCtx, nil)
          else
            { Assume CA }
            pTemp := CertFindCertificateInStore(FhCaStore,
                      X509_ASN_ENCODING, 0, CERT_FIND_EXISTING, pMsCertCtx, nil);
          if Assigned(pTemp) then
          begin
            CertFreeCertificateContext(pTemp);
            Continue; // Found
          end;
          if not CertAddCertificateContextToStore(FhCurTempStore,
                      pMsCertCtx, CERT_STORE_ADD_NEW, nil) then
          begin
            Err := GetLastError;
            if Err <> CRYPT_E_EXISTS then
                RaiseMsCryptoError(Err);
          end;
        finally
            CertFreeCertificateContext(pMsCertCtx);
        end;
      end;
    end;

    FCurCertCtx := X509ToMsCert(ACert.X509);
    if FCurCertCtx = nil then
      RaiseMyLastCryptoError;
    try
      if not CertGetCertificateChain(
                  0,                    // use the default chain engine
                  FCurCertCtx,          // pointer to the end certificate
                  nil,                  // use the default time
                  FhCurTempStore,       // search additional stores
                  @FChainPara,          // use AND logic and enhanced key usage
                                        // as indicated in the ChainPara
                                        // data structure
                  FMsVerifyOptions,     // Flags
                  nil,                  // currently reserved
                  @ChainContext)        // return a pointer to the chain created
      then
        RaiseMsCryptoError(GetLastError);

      AChainVerifyResult := ChainContext^.TrustStatus.dwErrorStatus;
      ACert.CustomVerifyResult := Integer(AChainVerifyResult);
      Result := AChainVerifyResult = 0;

      if Assigned(ACertChain) and AUpdateCertChain then
      begin
        ACertChain.Clear;
        { If AUpdateCertChain is TRUE the function returns the certificate }
        { chain and sets each cert's property CustomVerifyResult.          }
        if ChainContext^.cChain > 0 then
        begin
          PSimpleChain  := Pointer(ChainContext^.rgpChain);
          { It's not clear (to me) in which cases there will be multiple simple chains }
          if PSimpleChain^[0].cElement > 0 then
          begin
            if PSimpleChain^[0].cElement >
                SizeOf(TCertChainElements) div SizeOf(Pointer) then
              raise EMsCertChainEngine.CreateFmt(
                '! Fatal: Certificate chain too long (%d)',
                [PSimpleChain^[0].cElement]);
            PChainElements := Pointer(PSimpleChain^[0].rgpElement);
            for I := 0 to PSimpleChain^[0].cElement - 1 do
            begin
              PChainElement := PChainElements^[I];
              x := MsCertToX509(PChainElement^.pCertContext);
              if x = nil then
                RaiseLastOSSLError;
              try
                {J := FindX509InChain(x, ACertChain);
                if J >= 0 then
                  ACertChain[J].CustomVerifyResult :=
                                  PChainElement^.TrustStatus.dwErrorStatus
                else begin}
                  ACertChain.Insert(0, x);
                  ACertChain[0].VerifyDepth := I;
                  ACertChain[0].CustomVerifyResult :=
                                Integer(PChainElement^.TrustStatus.dwErrorStatus);
                //end;
              finally
                f_X509_free(x);
              end;
            end;
          end;
        end;
      end; // UpdateCertChain
    finally
      CertFreeCertificateChain(ChainContext);
    end;
  except
    on E: Exception do begin
      if FCurCertCtx <> nil then
      begin
        CertFreeCertificateContext(FCurCertCtx);
        FCurCertCtx := nil;
      end;
      if E is EMsCertChainEngine then
        raise
      else
        raise EMsCertChainEngine.Create(E.Message);
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Use this method if ACert has not been verified yet (slow).                }
function TMsCertChainEngine.ViewCert(ACert: TX509Base; ACertChain: TX509List;
  AHwnd: HWND = 0; ADlgEnableAddToStore: Boolean = True;
  ATitle: PChar = nil): Boolean;
var
  ChainVerifyResult : LongWord;
begin
  VerifyCert(ACert, ACertChain, ChainVerifyResult, False);
  Result := ViewCertLastVerified(AHwnd, ADlgEnableAddToStore, ATitle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Use this method if ACert has been already verified (faster).              }
function TMsCertChainEngine.ViewCertVerified(ACert: TX509Base; AHwnd: HWND = 0;
  ADlgEnableAddToStore: Boolean = True; ATitle: PChar = nil): Boolean;
var
  PMsCtx: PCCERT_CONTEXT;
begin
  if (ACert <> nil) and (ACert.X509 <> nil) then
  begin
    PMsCtx := X509ToMsCert(ACert.X509);
    if PMsCtx = nil then
      RaiseMyLastCryptoError;
    try
      Result := InternalViewCert(PMsCtx, AHwnd, ADlgEnableAddToStore, ATitle);
    finally
      CertFreeCertificateContext(PMsCtx);
    end;
  end
  else
      Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Use this method to show the last verified cert (fastest).                 }
function TMsCertChainEngine.ViewCertLastVerified(AHwnd: HWND = 0;
  ADlgEnableAddToStore: Boolean = True; ATitle: PChar = nil): Boolean;
begin
  Result := InternalViewCert(FCurCertCtx, AHwnd, ADlgEnableAddToStore, ATitle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMsCertChainEngine.InternalViewCert(ACertCtx: PCCERT_CONTEXT; AHwnd: HWND;
  AEnableAddToStore: Boolean; ATitle: PChar): Boolean;
var
  Info : CRYPTUI_VIEWCERTIFICATE_STRUCT;
begin
  if FCurCertCtx = nil then
  begin
    Result := False;
    Exit;
  end;
  FillChar(Info, SizeOf(Info), 0);
  Info.dwSize       := SizeOf(Info);
  Info.hwndParent   := AHwnd;
  Info.szTitle      := ATitle;
  Info.pCertContext := ACertCtx;
  if not AEnableAddToStore then
    Info.dwFlags := CRYPTUI_DISABLE_ADDTOSTORE;
  if FhCurTempStore <> nil then
  begin
    Info.cStores   := 1;
    Info.rghStores := @FhCurTempStore;
  end;
  Result := CryptUIDlgViewCertificate(@Info, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
