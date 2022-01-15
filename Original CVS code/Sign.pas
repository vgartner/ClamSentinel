unit Sign;

interface

type TVerifyResult = (SignOK, NoSignFound, SignInvalid, NoTrustProvider
           , SignNotTrusted, SignDisabled, SignFailed, SignExpired);

type TDetectionsType = (good, Suspicious, Obfuscated, Crypted, badSignature, SignatureExpired, badPEHeader, SuspiciousExt, Infected);
type TFileTypeDetected = (IsUndefined, IsDll, IsSys, IsOCX, IsExe);
           
function CheckFileTrust(const sFilename: string): TVerifyResult;


implementation

uses
  Windows, SysUtils;

const
  WTD_CHOICE_FILE             = 1;
  WTD_CHOICE_CATALOG          = 2;

  WTD_STATEACTION_IGNORE           = 0;
  WTD_STATEACTION_VERIFY           = 1;
  WTD_STATEACTION_CLOSE            = 2;
  WTD_STATEACTION_AUTO_CACHE_FLUSH = 4;

  WTD_UI_NONE                 = 2;

  //No additional revocation checking will be done...
  WTD_REVOKE_NONE             = 0;
  //Revocation checking will be done on the whole chain.
  WTD_REVOKE_WHOLECHAIN       = 1;

  WTD_SAFER_FLAG              = 256;

  WINTRUST_ACTION_GENERIC_VERIFY_V2: TGUID = '{00AAC56B-CD44-11d0-8CC2-00C04FC295EE}';
  DRIVER_ACTION_VERIFY: TGUID = '{F750E6C3-38EE-11D1-85E5-00C04FC295EE}';

  // The cryptography operation failed due to a local security option setting
  CRYPT_E_SECURITY_SETTINGS   = HRESULT($80092026);
    
  // A system-level error occurred while verifying trust.
  TRUST_E_SYSTEM_ERROR = HRESULT($80096001);

  // The certificate for the signer of the message is invalid or not found.
  TRUST_E_NO_SIGNER_CERT = HRESULT($80096002);

  // One of the counter signatures was invalid.
  TRUST_E_COUNTER_SIGNER = HRESULT($80096003);

  // The signature of the certificate cannot be verified.
  TRUST_E_CERT_SIGNATURE = HRESULT($80096004);

  // The timestamp signature and/or certificate could not be verified or is malformed.
  TRUST_E_TIME_STAMP = HRESULT($80096005);

  // The digital signature of the object did not verify.
  TRUST_E_BAD_DIGEST = HRESULT($80096010);

  // A certificate's basic constraint extension has not been observed.
  TRUST_E_BASIC_CONSTRAINTS = HRESULT($80096019);

  // The certificate does not meet or contain the Authenticode(tm) financial extensions.
  TRUST_E_FINANCIAL_CRITERIA = HRESULT($8009601E);

  // Unknown trust provider.
  TRUST_E_PROVIDER_UNKNOWN = HRESULT($800B0001);

  // The trust verification action specified is not supported by the specified trust provider.
  TRUST_E_ACTION_UNKNOWN = HRESULT($800B0002);

  // The form specified for the subject is not one supported or known by the specified trust provider.
  TRUST_E_SUBJECT_FORM_UNKNOWN = HRESULT($800B0003);

  // The subject is not trusted for the specified action.
  TRUST_E_SUBJECT_NOT_TRUSTED = HRESULT($800B0004);

  // No signature was present in the subject.
  TRUST_E_NOSIGNATURE = HRESULT($800B0100);

  // A required certificate is not within its validity period when verifying against the current system clock or the timestamp in the signed file.
  CERT_E_EXPIRED = HRESULT($800B0101);

  // The validity periods of the certification chain do not nest correctly.
  CERT_E_VALIDITYPERIODNESTING = HRESULT($800B0102);

  // A certificate that can only be used as an end-entity is being used as a CA or visa versa.
  CERT_E_ROLE = HRESULT($800B0103);

  // A path length constraint in the certification chain has been violated.
  CERT_E_PATHLENCONST = HRESULT($800B0104);

  // A certificate contains an unknown extension that is marked 'critical'.
  CERT_E_CRITICAL = HRESULT($800B0105);

  // A certificate being used for a purpose other than the ones specified by its CA.
  CERT_E_PURPOSE = HRESULT($800B0106);

  // A parent of a given certificate in fact did not issue that child certificate.
  CERT_E_ISSUERCHAINING = HRESULT($800B0107);

  // A certificate is missing or has an empty value for an important field, such as a subject or issuer name.
  CERT_E_MALFORMED = HRESULT($800B0108);

  // A certificate chain processed, but terminated in a root certificate which is not trusted by the trust provider.
  CERT_E_UNTRUSTEDROOT = HRESULT($800B0109);

  // A certificate chain could not be built to a trusted root authority.
  CERT_E_CHAINING = HRESULT($800B010A);

  // Generic trust failure.
  TRUST_E_FAIL = HRESULT($800B010B);

  // A certificate was explicitly revoked by its issuer.
  CERT_E_REVOKED = HRESULT($800B010C);

  // The certification path terminates with the test root which is not trusted with the current policy settings.
  CERT_E_UNTRUSTEDTESTROOT = HRESULT($800B010D);

  // The revocation process could not continue - the certificate(s) could not be checked.
  CERT_E_REVOCATION_FAILURE = HRESULT($800B010E);

  // The certificate's CN name does not match the passed value.
  CERT_E_CN_NO_MATCH = HRESULT($800B010F);

  // The certificate is not valid for the requested usage.
  CERT_E_WRONG_USAGE = HRESULT($800B0110);

  // The certificate was explicitly marked as untrusted by the user.
  TRUST_E_EXPLICIT_DISTRUST = HRESULT($800B0111);

  // A certification chain processed correctly, but one of the CA certificates is not trusted by the policy provider.
  CERT_E_UNTRUSTEDCA = HRESULT($800B0112);

  // The certificate has invalid policy.
  CERT_E_INVALID_POLICY = HRESULT($800B0113);

  // The certificate has an invalid name. The name is not included in the permitted list or is explicitly excluded.
  CERT_E_INVALID_NAME = HRESULT($800B0114);

  //  The revocation function was unable to check revocation for the certificate.
  CRYPT_E_NO_REVOCATION_CHECK = HRESULT($80092012);

  // It was not possible to connect to the revocation server.
  CRYPT_E_REVOCATION_OFFLINE = HRESULT($80092013);

  // The context was revoked. dwReason in pRevStatus contains the reason for revocation.
  CRYPT_E_REVOKED = HRESULT(80092010);
type

  CATALOG_INFO = record
    cbStruct: DWORD;
    sCatalogFile: array[0..MAX_PATH] of WCHAR;
  end;
  PCATALOG_INFO = ^CATALOG_INFO;


  WINTRUST_CATALOG_INFO = record
    cbStruct: DWORD;
    dwCatalogVersion: DWORD;                      // optional
    pcwszCatalogFilePath: LPCWSTR;
    pcwszMemberTag: LPCWSTR;
    pcwszMemberFilePath: LPCWSTR;
    hMemberFile: THANDLE;                         // optional
  end;
  PWINTRUST_CATALOG_INFO = ^WINTRUST_CATALOG_INFO;


  WINTRUST_FILE_INFO = record
    cbStruct: DWORD;
    pcwszFilePath: LPCWSTR;
    pgKnownSubject: PGUID;                        // optional
    hFile: THANDLE;                               // optional
  end;
  PWINTRUST_FILE_INFO = ^WINTRUST_FILE_INFO;


  WINTRUST_DATA = packed record
    cbStruct: DWORD;
    pPolicyCallbackData: pointer;
    pSIPClientData: pointer;
    dwUIChoice: DWORD;
    fdwRevocationChecks: DWORD;
    dwUnionChoice: DWORD;
    pWTDINFO: pointer;
    pFake: pointer;
    pFake1: pointer;
    pFake2: pointer;
    pFake3: pointer;
    dwStateAction: DWORD;
    hWVTStateData: THANDLE;
    pwszURLReference: PWChar;
    dwProvFlags: DWORD;
    dwUIContext: DWORD;

  end;
  PWINTRUST_DATA = ^WINTRUST_DATA;

  HCatAdmin = THANDLE;
  PHCatAdmin = ^HCatAdmin;

var
  hLibWintrust  : THANDLE;
  hLibCryptFunz : THANDLE;

  CryptCATAdminAcquireContext : function(PHCatAdmin: PHCatAdmin; pgSubsystem: PGUID; dwFlags: DWORD): BOOL; stdcall;
  CryptCATAdminReleaseContext : function(HCatAdmin: HCatAdmin; dwFlags: DWORD): BOOL; stdcall;
  CryptCATAdminCalcHashFromFileHandle: function(hFile: THANDLE; pHashSize: PDWORD; pbHash: PByteArray; dwFlags: DWORD): BOOL; stdcall;
  CryptCATAdminEnumCatalogFromHash: function(HCatAdmin: HCatAdmin; pbHash: PByteArray; pHashSize: DWORD; dwFlags: DWORD; phPrevCatInfo: PHandle): THANDLE; stdcall;
  CryptCATCatalogInfoFromContext: function(hCatInfo: THANDLE; psCatInfo: PCATALOG_INFO; dwFlags: DWORD): BOOL; stdcall;
  CryptCATAdminReleaseCatalogContext: function(HCatAdmin: HCatAdmin; hCatInfo: THANDLE; dwFlags: DWORD): BOOL; stdcall;
  WinVerifyTrust   : function(hwnd: THANDLE; pgActionID: PGUID; pWintrustData: PWINTRUST_DATA): HRESULT; stdcall;

  //Windows 8
  CryptCATAdminAcquireContext2: function(PHCatAdmin: PHCatAdmin; pgSubsystem: PGUID; pwszHashAlgorithm : PWIDECHAR; pStrongHashPolicy : PHandle; dwFlags: DWORD): BOOL; stdcall;
  CryptCATAdminCalcHashFromFileHandle2: function(HCatAdmin: HCatAdmin; hFile: THANDLE; pHashSize: PDWORD; pbHash: PByteArray; dwFlags: DWORD): BOOL; stdcall;

function StringToWide(const s : string): WideString;
const
  SIZE = 4096;
var
  Buffer: array[0..SIZE-1] of WideChar;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  result := WideString(StringToWideChar(Copy(s,1,SIZE-1), Buffer, SizeOf(Buffer)));
end;

function CheckFileTrust(const sFilename: string): TVerifyResult;
var
  aByteHash        : array[0..255] of Byte;
  iByteCount       : DWORD;

  hCatAdminContext : HCatAdmin;
  WTrustData       : WINTRUST_DATA;
  WTDCatalogInfo   : WINTRUST_CATALOG_INFO;
  WTDFileInfo      : WINTRUST_FILE_INFO;
  CatalogInfo      : CATALOG_INFO;

  hFile : THANDLE;
  hCatalogContext  : THANDLE;

  swFilename       : WideString;
  swMemberTag      : WideString;

  Res              : HRESULT;
  x                : Integer;
  bIsWin9xMe : boolean;
  bHash : boolean;
  bCATA2 : boolean;
begin
  Result := NoSignFound;

  hCatAdminContext:=0;
  hCatalogContext:=0;

  bIsWin9xMe := (Win32Platform = VER_PLATFORM_WIN32_WINDOWS);

  hLibWintrust := LoadLibrary('wintrust.dll');

  if bIsWin9xMe then
    hLibCryptFunz := LoadLibrary('mscat32.dll')
  else
    hLibCryptFunz := hLibWintrust;

if (hLibWintrust <> 0) and (hLibCryptFunz <> 0) then
begin
try
  CryptCATAdminAcquireContext := GetProcAddress(hLibCryptFunz, 'CryptCATAdminAcquireContext');
  CryptCATAdminReleaseContext := GetProcAddress(hLibCryptFunz, 'CryptCATAdminReleaseContext');
  CryptCATAdminCalcHashFromFileHandle := GetProcAddress(hLibCryptFunz, 'CryptCATAdminCalcHashFromFileHandle');
  CryptCATAdminEnumCatalogFromHash := GetProcAddress(hLibCryptFunz, 'CryptCATAdminEnumCatalogFromHash');
  CryptCATCatalogInfoFromContext := GetProcAddress(hLibCryptFunz, 'CryptCATCatalogInfoFromContext');
  CryptCATAdminReleaseCatalogContext := GetProcAddress(hLibCryptFunz, 'CryptCATAdminReleaseCatalogContext');

  //Windows 8
  CryptCATAdminAcquireContext2 := GetProcAddress(hLibCryptFunz, 'CryptCATAdminAcquireContext2');
  CryptCATAdminCalcHashFromFileHandle2 := GetProcAddress(hLibCryptFunz, 'CryptCATAdminCalcHashFromFileHandle2');

  WinVerifyTrust := GetProcAddress(hLibWintrust, 'WinVerifyTrust');
  
  swFilename := StringToWide(sFilename);

  ZeroMemory(@CatalogInfo, SizeOf(CatalogInfo));
  ZeroMemory(@WTDFileInfo, SizeOf(WTDFileInfo));
  ZeroMemory(@WTDCatalogInfo, SizeOf(WTDCatalogInfo));
  ZeroMemory(@WTrustData, SizeOf(WTrustData));

  bCATA2 := false;

  if Assigned(CryptCATAdminAcquireContext2) then
  begin
    if CryptCATAdminAcquireContext2(@hCatAdminContext, @DRIVER_ACTION_VERIFY, 'SHA256', nil, 0) then
      bCATA2 := true;
  end;

  if not bCATA2 then
    if CryptCATAdminAcquireContext(@hCatAdminContext, nil, 0) = False then Exit;

  hFile := CreateFile(PChar(sFilename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);

  if hFile = INVALID_HANDLE_VALUE then Exit;

  iByteCount := SizeOf(aByteHash);

  bHash := false;

  if bCATA2 and Assigned(CryptCATAdminCalcHashFromFileHandle2) then
  begin
    bCATA2 := false;
    if CryptCATAdminCalcHashFromFileHandle2(hCatAdminContext, hFile, @iByteCount, @aByteHash, 0) then
    begin
      bHash := true;
      bCATA2 := true;
    end;
  end;

  if not bCATA2 then
  begin
    if CryptCATAdminCalcHashFromFileHandle(hFile, @iByteCount, @aByteHash, 0) then
      bHash := true;
  end;

  if bHash then
  begin
     swMemberTag := '';
     for x := 0 to iByteCount - 1 do
     begin
       swMemberTag := swMemberTag + IntToHex(aByteHash[x], 2);
     end;
  end;

  CloseHandle(hFile);

  hCatalogContext := CryptCATAdminEnumCatalogFromHash(hCatAdminContext, @aByteHash, iByteCount, 0, nil);

  if hCatalogContext = 0 then
  begin
    WTDFileInfo.cbStruct := SizeOf(WTDFileInfo);
    WTDFileInfo.pcwszFilePath := PWideChar(swFilename);
    WTDFileInfo.pgKnownSubject := nil;
    WTDFileInfo.hFile := 0;

    WTrustData.cbStruct := SizeOf(WTrustData);
    WTrustData.dwUnionChoice := WTD_CHOICE_FILE;
    WTrustData.pWTDINFO := @WTDFileInfo;
    WTrustData.dwUIChoice := WTD_UI_NONE;
    WTrustData.fdwRevocationChecks := WTD_REVOKE_NONE;
    WTrustData.dwStateAction := WTD_STATEACTION_IGNORE;
    WTrustData.dwProvFlags := WTD_SAFER_FLAG;
    WTrustData.hWVTStateData := 0;
    WTrustData.pwszURLReference := nil;
  end
  else
  begin
    CryptCATCatalogInfoFromContext(hCatalogContext, @CatalogInfo, 0);

    WTDCatalogInfo.cbStruct := SizeOf(WTDCatalogInfo);
    WTDCatalogInfo.pcwszCatalogFilePath := CatalogInfo.sCatalogFile;
    WTDCatalogInfo.pcwszMemberFilePath := PWideChar(swFilename);
    WTDCatalogInfo.pcwszMemberTag := PWideChar(swMemberTag);

    WTrustData.cbStruct := SizeOf(WTrustData);
    WTrustData.dwUnionChoice := WTD_CHOICE_CATALOG;
    WTrustData.pWTDINFO := @WTDCatalogInfo;
    WTrustData.dwUIChoice := WTD_UI_NONE;
    WTrustData.fdwRevocationChecks := WTD_REVOKE_NONE;
    WTrustData.pPolicyCallbackData := nil;
    WTrustData.pSIPClientData := nil;
    WTrustData.dwStateAction := WTD_STATEACTION_VERIFY;
    WTrustData.dwProvFlags := 0;
    WTrustData.hWVTStateData := 0;
    WTrustData.pwszURLReference := nil;
  end;

  Res := WinVerifyTrust(INVALID_HANDLE_VALUE, @WINTRUST_ACTION_GENERIC_VERIFY_V2, @WTrustData);

  case Res of
    ERROR_SUCCESS:                result := SignOK;
    TRUST_E_NOSIGNATURE:          result := NoSignFound;
    TRUST_E_SUBJECT_FORM_UNKNOWN: result := NoSignFound;

    TRUST_E_BAD_DIGEST:           result := SignInvalid;
    TRUST_E_NO_SIGNER_CERT:       result := SignInvalid;
    TRUST_E_COUNTER_SIGNER:       result := SignInvalid;
    CERT_E_PATHLENCONST:          result := SignInvalid;
    CERT_E_CRITICAL:              result := SignInvalid;
    CERT_E_PURPOSE:               result := SignInvalid;
    CERT_E_ISSUERCHAINING:        result := SignInvalid;
    CERT_E_MALFORMED:             result := SignInvalid;
    CERT_E_UNTRUSTEDROOT:         result := SignInvalid;
    CERT_E_CHAINING:              result := SignInvalid;
    CERT_E_REVOKED:               result := SignInvalid;
    CERT_E_UNTRUSTEDTESTROOT:     result := SignInvalid;
    CERT_E_CN_NO_MATCH:           result := SignInvalid;
    CERT_E_WRONG_USAGE:           result := SignInvalid;
    CERT_E_INVALID_POLICY:        result := SignInvalid;
    TRUST_E_EXPLICIT_DISTRUST:    result := SignInvalid;
    CERT_E_INVALID_NAME:          result := SignInvalid;
    TRUST_E_BASIC_CONSTRAINTS:    result := SignInvalid;

    CRYPT_E_SECURITY_SETTINGS:    result := SignDisabled;
    TRUST_E_SUBJECT_NOT_TRUSTED:  result := SignNotTrusted;
    TRUST_E_PROVIDER_UNKNOWN:     result := NoTrustProvider;

    //TRUST_E_SYSTEM_ERROR:
    TRUST_E_CERT_SIGNATURE:       result := SignInvalid;
    //TRUST_E_TIME_STAMP:
    //TRUST_E_FINANCIAL_CRITERIA:
    //TRUST_E_ACTION_UNKNOWN:

    CERT_E_EXPIRED:  if bIsWin9xMe then
                       result := SignExpired
                     else
                       result := SignExpired;

    CERT_E_VALIDITYPERIODNESTING:    result := SignInvalid;
    CERT_E_ROLE:                     result := SignInvalid;

    //TRUST_E_FAIL:
    //CERT_E_REVOCATION_FAILURE:
    //CERT_E_UNTRUSTEDCA:
    //CRYPT_E_NO_REVOCATION_CHECK:

    else                          result := SignFailed;
  end;

  if not(hCatalogContext = 0) then
  begin
    WTrustData.dwStateAction := WTD_STATEACTION_AUTO_CACHE_FLUSH;
    WinVerifyTrust(INVALID_HANDLE_VALUE, @WINTRUST_ACTION_GENERIC_VERIFY_V2, @WTrustData);

    WTrustData.dwStateAction := WTD_STATEACTION_CLOSE;
    WinVerifyTrust(INVALID_HANDLE_VALUE, @WINTRUST_ACTION_GENERIC_VERIFY_V2, @WTrustData);
  end;

finally
  if hCatAdminContext <> 0 then
  begin
    if hCatalogContext <> 0 then
      CryptCATAdminReleaseCatalogContext(hCatAdminContext, hCatalogContext, 0);

    CryptCATAdminReleaseContext(hCatAdminContext, 0);
  end;

  FreeLibrary(hLibWintrust);

  if bIsWin9xMe then
    FreeLibrary(hLibCryptFunz);
end;
end;

end;

end.
