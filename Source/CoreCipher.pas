{ ****************************************************************************** }
{ * Core cipher Library ,writen by QQ 600585@qq.com                            * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

(*
  update history
  2017-11-26
  fixed fastMD5,THashMD5 calculate x64 and x86,ARM platform more than 4G memory Support QQ600585
  change name TMD5Class as TCipherMD5
  Added global DefaultParallelDepth

  2017-12-6
  added supported hash elf64

  2017-12-7
  added System default key

  2018-5-16
  remove pasmp

  2018-9
  fixed rc6 with arm Linux
*)

unit CoreCipher;

{ core cipher engine. create by qq600585 }
{$INCLUDE zDefine.inc}

interface

uses
  Types, SysUtils, Math, TypInfo,
{$IFDEF FastMD5}
  Fast_MD5,
{$ENDIF}
  CoreClasses, UnicodeMixedLib, MemoryStream64, PascalStrings, ListEngine;

const
  { largest structure that can be created }
  MaxStructSize = MaxInt; { 2G }
  cIntSize = 4;
  cKeyDWORDSize = 4;
  cKey2DWORDSize = 8;
  cKey64Size = 8;
  cKey128Size = 16;
  cKey192Size = 24;
  cKey256Size = 32;

type
  PDWORD = ^DWORD;

  { general structures }
  PDWordArray = ^TDWordArray;
  TDWordArray = array [0 .. MaxStructSize div SizeOf(DWORD) - 1] of DWORD;

  TCCByteArray = array [0 .. MaxStructSize div SizeOf(Byte) - 1] of Byte;
  PCCByteArray = ^TCCByteArray;

  TInt32 = packed record
    case Byte of
      1: (Lo: Word;
          Hi: Word);
      2: (LoLo: Byte;
          LoHi: Byte;
          HiLo: Byte;
          HiHi: Byte);
      3: (i: Integer);
      4: (u: DWORD);
  end;

  TInt64 = packed record
    case Byte of
      0: (Lo: Integer;
          Hi: Integer);
      1: (LoLo: Word;
          LoHi: Word;
          HiLo: Word;
          HiHi: Word);
      2: (LoLoLo: Byte;
          LoLoHi: Byte;
          LoHiLo: Byte;
          LoHiHi: Byte;
          HiLoLo: Byte;
          HiLoHi: Byte;
          HiHiLo: Byte;
          HiHiHi: Byte);
      3: (i: Int64);
      4: (u: UInt64);
  end;

  { encryption key types }
type
  PKey64 = ^TKey64; { !!.03 }
  TKey64 = array [0 .. 7] of Byte;

  PKey128 = ^TKey128; { !!.03 }
  TKey128 = array [0 .. 15] of Byte;

  PKey256 = ^TKey256; { !!.03 }
  TKey256 = array [0 .. 31] of Byte;

  { encryption block types }
  PLBCBlock = ^TLBCBlock;
  TLBCBlock = array [0 .. 3] of DWORD; { LBC block }

  PDESBlock = ^TDESBlock;
  TDESBlock = array [0 .. 7] of Byte; { DES block }

  PLQCBlock = ^TLQCBlock;
  TLQCBlock = array [0 .. 1] of DWORD; { Quick Cipher,no LBC key generate }

  PBFBlock = ^TBFBlock;
  TBFBlock = array [0 .. 1] of DWORD; { BlowFish }

  PXXTEABlock = ^TXXTEABlock;
  TXXTEABlock = array [0 .. 63] of Byte; { XXTEA }

  TDesConverter = packed record
    case Byte of
      0: (Bytes: array [0 .. 7] of Byte);
      1: (DWords: array [0 .. 1] of DWORD)
  end;

  P128Bit = ^T128Bit;
  T128Bit = array [0 .. 3] of DWORD;

  P256Bit = ^T256Bit;
  T256Bit = array [0 .. 7] of DWORD;

  TTransformOutput = array [0 .. 3] of DWORD;
  TTransformInput = array [0 .. 15] of DWORD;

  { context type constants }
const
  BFRounds = 16; { 16 blowfish rounds }

  { block cipher context types }
type
  { Blowfish }
  PBFContext = ^TBFContext;

  TBFContext = packed record
    PBox: array [0 .. (BFRounds + 1)] of DWORD;
    SBox: array [0 .. 3, 0 .. 255] of DWORD;
  end;

  { DES }
  PDESContext = ^TDESContext;

  TDESContext = packed record
    TransformedKey: array [0 .. 31] of DWORD;
    Encrypt: Boolean;
  end;

  { 3 DES }
  PTripleDESContext = ^TTripleDESContext;
  TTripleDESContext = array [0 .. 1] of TDESContext;

  PTripleDESContext3Key = ^TTripleDESContext3Key;
  TTripleDESContext3Key = array [0 .. 2] of TDESContext; { !!.01 }

  { LBC Cipher context }
  PLBCContext = ^TLBCContext;

  TLBCContext = packed record
    Encrypt: Boolean;
    Dummy: array [0 .. 2] of Byte; { filler }
    Rounds: Integer;
    case Byte of
      0: (SubKeys64: array [0 .. 15] of TKey64);
      1: (SubKeysInts: array [0 .. 3, 0 .. 7] of DWORD);
  end;

  { LSC stream cipher }
  PLSCContext = ^TLSCContext;

  TLSCContext = packed record
    index: Integer;
    Accumulator: Integer;
    SBox: array [0 .. 255] of Byte;
  end;

  { random number stream ciphers }
  PRNG32Context = ^TRNG32Context;
  TRNG32Context = array [0 .. 3] of Byte;

  PRNG64Context = ^TRNG64Context;
  TRNG64Context = array [0 .. 7] of Byte;

  { message digest blocks }
  PMD5Digest = ^TMD5Digest;
  TMD5Digest = TMD5; { 128 bits - MD5 }
  TMD5Key = TMD5Digest;

  PSHA1Digest = ^TSHA1Digest;
  TSHA1Digest = array [0 .. 19] of Byte; { 160 bits - SHA-1 }
  TSHA1Key = TSHA1Digest;

  { message digest context types }
  TLMDContext = packed record
    DigestIndex: Integer;
    Digest: array [0 .. 255] of Byte;
    KeyIndex: Integer;
    case Byte of
      0: (KeyInts: array [0 .. 3] of DWORD);
      1: (key: TKey128);
  end;

  PMD5Context = ^TMD5Context;

  TMD5Context = packed record       { MD5 }
    Count: array [0 .. 1] of DWORD; { number of bits handled mod 2^64 }
    State: TTransformOutput;        { scratch buffer }
    Buf: array [0 .. 63] of Byte;   { input buffer }
  end;

  TSHA1Context = packed record { SHA-1 }
    sdHi: DWORD;
    sdLo: DWORD;
    sdIndex: DWORD;
    sdHash: array [0 .. 4] of DWORD;
    sdBuf: array [0 .. 63] of Byte;
  end;

type
  { key style and auto Encrypt }
  TCipherStyle = (csNone,
    csDES64, csDES128, csDES192,
    csBlowfish, csLBC, csLQC, csRNG32, csRNG64, csLSC, csTwoFish,
    csXXTea512, csRC6);

  TCipherStyles = set of TCipherStyle;
  TCipherStyleArray = array of TCipherStyle;
  TCipherKeyStyle = (cksNone, cksKey64, cks3Key64, cksKey128, cksKey256, cks2IntKey, cksIntKey, ckyDynamicKey);
  PCipherKeyBuffer = ^TCipherKeyBuffer;
  TCipherKeyBuffer = TBytes;
  THashStyle = (hsNone, hsFastMD5, hsMD5, hsSHA1, hs256, hs128, hs64, hs32, hs16, hsELF, hsELF64, hsMix128, hsCRC16, hsCRC32);
  THashStyles = set of THashStyle;

type
  TCipher = class(TCoreClassObject)
  public
    const
    CAllHash: THashStyles =
      [hsNone, hsFastMD5, hsMD5, hsSHA1, hs256, hs128, hs64, hs32, hs16, hsELF, hsELF64, hsMix128, hsCRC16, hsCRC32];

    CHashName: array [THashStyle] of SystemString =
      ('None', 'FastMD5', 'MD5', 'SHA1', '256', '128', '64', '32', '16', 'ELF', 'ELF64', 'Mix128', 'CRC16', 'CRC32');

    CCipherStyleName: array [TCipherStyle] of SystemString =
      ('None',
      'DES64', 'DES128', 'DES192',
      'Blowfish', 'LBC', 'LQC', 'RNG32', 'RNG64', 'LSC', 'TwoFish',
      'XXTea512', 'RC6');

    cCipherKeyStyle: array [TCipherStyle] of TCipherKeyStyle =
      (
      cksNone,       // csNone
      cksKey64,      // csDES64
      cksKey128,     // csDES128
      cks3Key64,     // csDES192
      cksKey128,     // csBlowfish
      cksKey128,     // csLBC
      cksKey128,     // csLQC
      cksIntKey,     // csRNG32
      cks2IntKey,    // csRNG64
      ckyDynamicKey, // csLSC
      ckyDynamicKey, // csTwoFish
      cksKey128,     // csXXTea512
      ckyDynamicKey  // csRC6
      );
  public
    class function AllCipher: TCipherStyleArray;

    class function NameToHashStyle(n: SystemString; var hash: THashStyle): Boolean;

    class function BuffToString(buff: Pointer; Size: NativeInt): TPascalString; overload;
    class function StringToBuff(const Hex: TPascalString; var Buf; BufSize: Cardinal): Boolean; overload;

    class procedure HashToString(hash: Pointer; Size: NativeInt; var output: TPascalString); overload;
    class procedure HashToString(hash: TSHA1Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TMD5Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TBytes; var output: TPascalString); overload;
    class procedure HashToString(hash: TBytes; var output: SystemString); overload;

    class function CompareHash(h1, h2: TSHA1Digest): Boolean; overload;
    class function CompareHash(h1, h2: TMD5Digest): Boolean; overload;
    class function CompareHash(h1, h2: Pointer; Size: Word): Boolean; overload;
    class function CompareHash(h1, h2: TBytes): Boolean; overload;

    class function CompareKey(k1, k2: TCipherKeyBuffer): Boolean; overload;

    class function GenerateSha1Hash(sour: Pointer; Size: NativeInt): TSHA1Digest;
    class function GenerateMD5Hash(sour: Pointer; Size: NativeInt): TMD5Digest;
    class procedure GenerateHash(sour: Pointer; Size: NativeInt; OutHash: Pointer; HashSize: NativeInt);

    class procedure GenerateHashByte(hs: THashStyle; sour: Pointer; Size: NativeInt; var output: TBytes);

    class procedure GenerateSha1HashString(sour: Pointer; Size: NativeInt; var output: TPascalString);
    class procedure GenerateMD5HashString(sour: Pointer; Size: NativeInt; var output: TPascalString);
    class procedure GenerateHashString(sour: Pointer; Size, HashSize: NativeInt; var output: TPascalString);

    class function BufferToHex(const Buf; BufSize: Cardinal): TPascalString;
    class function HexToBuffer(const Hex: TPascalString; var Buf; BufSize: Cardinal): Boolean;

    class procedure GenerateNoneKey(var output: TCipherKeyBuffer);
    class procedure GenerateKey64(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey64(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey128(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey128(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey256(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey256(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure Generate3Key64(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure Generate3Key64(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure Generate2IntKey(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure Generate2IntKey(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateIntKey(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure GenerateIntKey(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateBytesKey(const s: TPascalString; KeySize: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateBytesKey(sour: Pointer; Size, KeySize: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey64(const k: TDESKey; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey128(const k1, k2: TKey64; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k: TKey64; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k1, k2, k3: TKey64; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k: TKey128; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k: TKey256; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k1, k2: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k: TDESKey; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const key: PByte; Size: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(cs: TCipherStyle; buffPtr: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(cs: TCipherStyle; s: TPascalString; var output: TCipherKeyBuffer); overload;

    class function GetKeyStyle(const p: PCipherKeyBuffer): TCipherKeyStyle; overload;

    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey64): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k1, k2, k3: TKey64): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey128): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey256): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k1, k2: DWORD): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: DWORD): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TDESKey): Boolean; overload;

    class function GetBytesKey(const KeyBuffPtr: PCipherKeyBuffer; var key: TBytes): Boolean; overload;

    class procedure EncryptTail(TailPtr: Pointer; TailSize: NativeInt);
    class function OLDDES(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;

    class function DES64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function DES128(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function DES192(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function Blowfish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function LBC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function LQC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function RNG32(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
    class function RNG64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
    class function LSC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
    class function TwoFish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function XXTea512(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function RC6(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;

    class procedure BlockCBC(sour: Pointer; Size: NativeInt; boxBuff: Pointer; boxSiz: NativeInt);
    class function EncryptBuffer(cs: TCipherStyle; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;

    class function EncryptBufferCBC(cs: TCipherStyle; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
  end;

{$IFDEF parallel}

  TParallelCipher = class(TCoreClassObject)
  private type
    TParallelCipherFunc = procedure(Job, buff, key: Pointer; Size: NativeInt) of object;

    PParallelCipherJobData = ^TParallelCipherJobData;

    TParallelCipherJobData = packed record
      cipherFunc: TParallelCipherFunc;
      KeyBuffer: Pointer;
      OriginBuffer: Pointer;
      BlockLen: NativeInt;
      TotalBlock: NativeInt;
      CompletedBlock: NativeInt;
      Encrypt: Boolean;
    end;
  protected
    procedure DES64_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure DES128_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure DES192_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure Blowfish_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure LBC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure LQC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure TwoFish_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure XXTea512_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure RC6_Parallel(Job, buff, key: Pointer; Size: NativeInt);

    procedure BlockCBC_Parallel(Job, buff, key: Pointer; Size: NativeInt);

    procedure ParallelCipherCall(const JobData: PParallelCipherJobData; const FromIndex, ToIndex: Integer);
    procedure RunParallel(const JobData: PParallelCipherJobData; const Total, Depth: Integer);
  public
    ParallelDepth: Integer;

    constructor Create;
    destructor Destroy; override;

    function DES64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function DES128(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function DES192(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function Blowfish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function LBC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function LQC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function TwoFish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function XXTea512(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function RC6(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;

    procedure BlockCBC(sour: Pointer; Size: NativeInt; boxBuff: Pointer; boxSiz: NativeInt);

    function EncryptBuffer(cs: TCipherStyle; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;

    function EncryptBufferCBC(cs: TCipherStyle; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
  end;

{$ENDIF}


var
  { system default cbc refrence }
  SystemCBC: TBytes;
{$IFDEF parallel}
  { system default parallel depth }
  DefaultParallelDepth: Integer;
{$ENDIF}
  { system default key and cipherStyle }
  DefaultKey: TCipherKeyBuffer;
  DefaultCipherStyle: TCipherStyle = TCipherStyle.csBlowfish;

procedure InitSysCBCAndDefaultKey(rand: Int64);

function SequEncryptWithDirect(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptWithDirect(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;

{$IFDEF parallel}
function SequEncryptWithParallel(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptWithParallel(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;
{$ENDIF}

function SequEncrypt(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncrypt(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncrypt(sour: Pointer; Size: NativeInt; Encrypt, ProcessTail: Boolean): Boolean; overload;

function SequEncryptCBCWithDirect(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptCBCWithDirect(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;

{$IFDEF parallel}
function SequEncryptCBCWithParallel(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptCBCWithParallel(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;
{$ENDIF}

function SequEncryptCBC(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptCBC(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean; overload;

function GenerateSequHash(hssArry: THashStyles; sour: Pointer; Size: NativeInt): TPascalString; overload;
procedure GenerateSequHash(hssArry: THashStyles; sour: Pointer; Size: NativeInt; output: TListPascalString); overload;
procedure GenerateSequHash(hssArry: THashStyles; sour: Pointer; Size: NativeInt; output: TCoreClassStream); overload;

function CompareSequHash(HashVL: THashStringList; sour: Pointer; Size: NativeInt): Boolean; overload;
function CompareSequHash(hashData: TPascalString; sour: Pointer; Size: NativeInt): Boolean; overload;
function CompareSequHash(hashData: TListPascalString; sour: Pointer; Size: NativeInt): Boolean; overload;
function CompareSequHash(hashData: TCoreClassStream; sour: Pointer; Size: NativeInt): Boolean; overload;

function GeneratePasswordHash(hssArry: THashStyles; passwd: TPascalString): TPascalString;
function ComparePasswordHash(passwd, hashBuff: TPascalString): Boolean;

function GeneratePassword(const ca: TCipherStyleArray; passwd: TPascalString): TPascalString; overload;
function ComparePassword(const ca: TCipherStyleArray; passwd, passwdDataSource: TPascalString): Boolean; overload;

function GeneratePassword(const cs: TCipherStyle; passwd: TPascalString): TPascalString; overload;
function ComparePassword(const cs: TCipherStyle; passwd, passwdDataSource: TPascalString): Boolean; overload;

procedure TestCoreCipher;

type
  { Blowfish Cipher }
  TBlowfish = class(TCoreClassObject)
  public
    class procedure EncryptBF(const Context: TBFContext; var Block: TBFBlock; Encrypt: Boolean);
    class procedure InitEncryptBF(key: TKey128; var Context: TBFContext);
  end;

  { DES Cipher }
  TDES = class(TCoreClassObject)
  strict private
    class procedure JoinBlock(const L, r: DWORD; var Block: TDESBlock);
    class procedure SplitBlock(const Block: TDESBlock; var L, r: DWORD);
  private
  public
    class procedure EncryptDES(const Context: TDESContext; var Block: TDESBlock);
    class procedure EncryptTripleDES(const Context: TTripleDESContext; var Block: TDESBlock);
    class procedure EncryptTripleDES3Key(const Context: TTripleDESContext3Key; var Block: TDESBlock);
    class procedure InitEncryptDES(const key: TKey64; var Context: TDESContext; Encrypt: Boolean);
    class procedure InitEncryptTripleDES(const key: TKey128; var Context: TTripleDESContext; Encrypt: Boolean);
    class procedure InitEncryptTripleDES3Key(const Key1, Key2, Key3: TKey64; var Context: TTripleDESContext3Key; Encrypt: Boolean);
    class procedure ShrinkDESKey(var key: TKey64);
  end;

  { SHA1 Cipher }
  TSHA1 = class(TCoreClassObject)
  strict private
    class procedure SHA1Clear(var Context: TSHA1Context);
    class procedure SHA1Hash(var Context: TSHA1Context);
    class function SHA1SwapByteOrder(n: DWORD): DWORD;
    class procedure SHA1UpdateLen(var Context: TSHA1Context; Len: DWORD);
  public
    class procedure FinalizeSHA1(var Context: TSHA1Context; var Digest: TSHA1Digest);
    class procedure HashSHA1(var Digest: TSHA1Digest; const Buf; BufSize: Integer);
    class procedure InitSHA1(var Context: TSHA1Context);
    class procedure ByteBuffHashSHA1(var Digest: TSHA1Digest; const ABytes: TBytes);
    class procedure UpdateSHA1(var Context: TSHA1Context; const Buf; BufSize: Integer);
  end;

  { LBC Cipher }
  TLBC = class(TCoreClassObject)
  public
    class procedure EncryptLBC(const Context: TLBCContext; var Block: TLBCBlock);
    class procedure EncryptLQC(const key: TKey128; var Block: TLQCBlock; Encrypt: Boolean);
    class procedure InitEncryptLBC(const key: TKey128; var Context: TLBCContext; Rounds: Integer; Encrypt: Boolean);
  end;

  { MD5 Cipher }
  TCipherMD5 = class(TCoreClassObject)
  public
    class procedure FinalizeMD5(var Context: TMD5Context; var Digest: TMD5Digest);
    class procedure GenerateMD5Key(var key: TKey128; const ABytes: TBytes);
    class procedure HashMD5(var Digest: TMD5Digest; const Buf; BufSize: NativeInt);
    class procedure InitMD5(var Context: TMD5Context);
    class procedure ByteBuffHashMD5(var Digest: TMD5Digest; const ABytes: TBytes);
    class procedure UpdateMD5(var Context: TMD5Context; const Buf; BufSize: NativeInt);
  end;

  { Cipher message digest }
  TLMD = class(TCoreClassObject)
  public
    class procedure FinalizeLMD(var Context: TLMDContext; var Digest; DigestSize: Integer);
    class procedure GenerateLMDKey(var key; KeySize: Integer; const ABytes: TBytes);
    class procedure HashLMD(var Digest; DigestSize: Integer; const Buf; BufSize: NativeInt);
    class procedure InitLMD(var Context: TLMDContext);
    class procedure ByteBuffHashLMD(var Digest; DigestSize: Integer; const ABytes: TBytes);
    class procedure UpdateLMD(var Context: TLMDContext; const Buf; BufSize: NativeInt);
  end;

  { Random Number Cipher }
  TRNG = class(TCoreClassObject)
  public
    class procedure EncryptRNG32(var Context: TRNG32Context; var Buf; BufSize: Integer);
    class procedure EncryptRNG64(var Context: TRNG64Context; var Buf; BufSize: Integer);
    class procedure InitEncryptRNG32(key: DWORD; var Context: TRNG32Context);
    class procedure InitEncryptRNG64(KeyHi, KeyLo: DWORD; var Context: TRNG64Context);
  end;

  { LSC Stream Cipher }
  TLSC = class(TCoreClassObject)
  public
    class procedure EncryptLSC(var Context: TLSCContext; var Buf; BufSize: Integer);
    class procedure InitEncryptLSC(const key; KeySize: Integer; var Context: TLSCContext);
  end;

  { Miscellaneous hash algorithms }
  { Misc public utilities }
  TMISC = class(TCoreClassObject)
  private
    class procedure Mix128(var x: T128Bit); static;
    class function Ran0Prim(var Seed: Integer; IA, IQ, IR: Integer): Integer; static;
    class function Random64(var Seed: TInt64): Integer; static;
    class procedure Transform(var OutputBuffer: TTransformOutput; var InBuf: TTransformInput); static;
  public
    class procedure GenerateRandomKey(var key; KeySize: Integer); static;
    class procedure HashELF(var Digest: DWORD; const Buf; BufSize: nativeUInt); static;
    class procedure HashELF64(var Digest: Int64; const Buf; BufSize: nativeUInt); static;
    class procedure HashMix128(var Digest: DWORD; const Buf; BufSize: nativeUInt); static;
    class function Ran01(var Seed: Integer): Integer; static;
    class function Ran02(var Seed: Integer): Integer; static;
    class function Ran03(var Seed: Integer): Integer; static;
    class function Random32Byte(var Seed: Integer): Byte; static;
    class function Random64Byte(var Seed: TInt64): Byte; static;
    class function RolX(i, c: DWORD): DWORD; static;
    class procedure ByteBuffHashELF(var Digest: DWORD; const ABytes: TBytes); static;
    class procedure ByteBuffHashMix128(var Digest: DWORD; const ABytes: TBytes); static;
    class procedure XorMem(var Mem1; const Mem2; Count: NativeInt); static;
  end;

  // Twofish
const
  DCPTF_OUTPUTWHITEN = 4;
  DCPTF_NUMROUNDS = 16;
  DCPTF_ROUNDSUBKEYS = (DCPTF_OUTPUTWHITEN + 4);

type
  PDCPTFSubKeys = ^TDCPTFSubKeys;
  TDCPTFSubKeys = array [0 .. DCPTF_ROUNDSUBKEYS + DCPTF_NUMROUNDS * 2 - 1] of DWORD;

  PDCPTFSBox = ^TDCPTFSBox;
  TDCPTFSBox = array [0 .. 3, 0 .. 512 - 1] of DWORD;

  TDCPTF2048 = array [0 .. 255] of Byte;
  TDCPTFp8x8 = array [0 .. 1] of TDCPTF2048;

  PTwoFishContext = ^TTwoFishContext;

  TTwoFishContext = packed record
    SubKeys: TDCPTFSubKeys;
    SBox: TDCPTFSBox;
  end;

procedure DCP_twofish_InitKey(const key; Size: Cardinal; var SubKeys: TDCPTFSubKeys; var SBox: TDCPTFSBox);
procedure DCP_twofish_EncryptECB(var SubKeys: TDCPTFSubKeys; var SBox: TDCPTFSBox; const InData: T128Bit; var OutData: T128Bit);
procedure DCP_twofish_DecryptECB(var SubKeys: TDCPTFSubKeys; var SBox: TDCPTFSBox; const InData: T128Bit; var OutData: T128Bit);

procedure XXTEAEncrypt(var key: TKey128; var Block: TXXTEABlock);
procedure XXTEADecrypt(var key: TKey128; var Block: TXXTEABlock);

const
  cRC6_NumRounds = 20; { number of rounds must be between 16-24 }

type
  PRC6Key = ^TRC6Key;
  TRC6Key = array [0 .. ((cRC6_NumRounds * 2) + 3)] of DWORD;

  PRC6Block = ^TRC6Block;
  TRC6Block = array [0 .. 15] of Byte;

  TRC6 = class(TCoreClassObject)
  public
    class function LRot32(x, c: DWORD): DWORD;
    class function RRot32(x, c: DWORD): DWORD;
    class procedure InitKey(buff: Pointer; Size: Integer; var KeyContext: TRC6Key);
    class procedure Encrypt(var KeyContext: TRC6Key; var Data: TRC6Block);
    class procedure Decrypt(var KeyContext: TRC6Key; var Data: TRC6Block);
  end;

implementation

uses DoStatusIO,
{$IFDEF parallel}
{$IFDEF FPC}
  mtprocs,
{$ELSE parallel}
  Threading,
{$ENDIF FPC}
{$ENDIF parallel}
  SyncObjs;

const
  SInvalidFileFormat: SystemString = 'Invalid file format';

  { -Blowfish lookup tables }

  bf_P: array [0 .. (BFRounds + 1)] of DWORD = (
    $243F6A88, $85A308D3, $13198A2E, $03707344,
    $A4093822, $299F31D0, $082EFA98, $EC4E6C89,
    $452821E6, $38D01377, $BE5466CF, $34E90C6C,
    $C0AC29B7, $C97C50DD, $3F84D5B5, $B5470917,
    $9216D5D9, $8979FB1B);

  bf_S: array [0 .. 3, 0 .. 255] of DWORD =
    (
    ($D1310BA6, $98DFB5AC, $2FFD72DB, $D01ADFB7,
    $B8E1AFED, $6A267E96, $BA7C9045, $F12C7F99,
    $24A19947, $B3916CF7, $0801F2E2, $858EFC16,
    $636920D8, $71574E69, $A458FEA3, $F4933D7E,

    $0D95748F, $728EB658, $718BCD58, $82154AEE,
    $7B54A41D, $C25A59B5, $9C30D539, $2AF26013,
    $C5D1B023, $286085F0, $CA417918, $B8DB38EF,
    $8E79DCB0, $603A180E, $6C9E0E8B, $B01E8A3E,

    $D71577C1, $BD314B27, $78AF2FDA, $55605C60,
    $E65525F3, $AA55AB94, $57489862, $63E81440,
    $55CA396A, $2AAB10B6, $B4CC5C34, $1141E8CE,
    $A15486AF, $7C72E993, $B3EE1411, $636FBC2A,

    $2BA9C55D, $741831F6, $CE5C3E16, $9B87931E,
    $AFD6BA33, $6C24CF5C, $7A325381, $28958677,
    $3B8F4898, $6B4BB9AF, $C4BFE81B, $66282193,
    $61D809CC, $FB21A991, $487CAC60, $5DEC8032,

    $EF845D5D, $E98575B1, $DC262302, $EB651B88,
    $23893E81, $D396ACC5, $0F6D6FF3, $83F44239,
    $2E0B4482, $A4842004, $69C8F04A, $9E1F9B5E,
    $21C66842, $F6E96C9A, $670C9C61, $ABD388F0,

    $6A51A0D2, $D8542F68, $960FA728, $AB5133A3,
    $6EEF0B6C, $137A3BE4, $BA3BF050, $7EFB2A98,
    $A1F1651D, $39AF0176, $66CA593E, $82430E88,
    $8CEE8619, $456F9FB4, $7D84A5C3, $3B8B5EBE,

    $E06F75D8, $85C12073, $401A449F, $56C16AA6,
    $4ED3AA62, $363F7706, $1BFEDF72, $429B023D,
    $37D0D724, $D00A1248, $DB0FEAD3, $49F1C09B,
    $075372C9, $80991B7B, $25D479D8, $F6E8DEF7,

    $E3FE501A, $B6794C3B, $976CE0BD, $04C006BA,
    $C1A94FB6, $409F60C4, $5E5C9EC2, $196A2463,
    $68FB6FAF, $3E6C53B5, $1339B2EB, $3B52EC6F,
    $6DFC511F, $9B30952C, $CC814544, $AF5EBD09,

    $BEE3D004, $DE334AFD, $660F2807, $192E4BB3,
    $C0CBA857, $45C8740F, $D20B5F39, $B9D3FBDB,
    $5579C0BD, $1A60320A, $D6A100C6, $402C7279,
    $679F25FE, $FB1FA3CC, $8EA5E9F8, $DB3222F8,

    $3C7516DF, $FD616B15, $2F501EC8, $AD0552AB,
    $323DB5FA, $FD238760, $53317B48, $3E00DF82,
    $9E5C57BB, $CA6F8CA0, $1A87562E, $DF1769DB,
    $D542A8F6, $287EFFC3, $AC6732C6, $8C4F5573,

    $695B27B0, $BBCA58C8, $E1FFA35D, $B8F011A0,
    $10FA3D98, $FD2183B8, $4AFCB56C, $2DD1D35B,
    $9A53E479, $B6F84565, $D28E49BC, $4BFB9790,
    $E1DDF2DA, $A4CB7E33, $62FB1341, $CEE4C6E8,

    $EF20CADA, $36774C01, $D07E9EFE, $2BF11FB4,
    $95DBDA4D, $AE909198, $EAAD8E71, $6B93D5A0,
    $D08ED1D0, $AFC725E0, $8E3C5B2F, $8E7594B7,
    $8FF6E2FB, $F2122B64, $8888B812, $900DF01C,

    $4FAD5EA0, $688FC31C, $D1CFF191, $B3A8C1AD,
    $2F2F2218, $BE0E1777, $EA752DFE, $8B021FA1,
    $E5A0CC0F, $B56F74E8, $18ACF3D6, $CE89E299,
    $B4A84FE0, $FD13E0B7, $7CC43B81, $D2ADA8D9,

    $165FA266, $80957705, $93CC7314, $211A1477,
    $E6AD2065, $77B5FA86, $C75442F5, $FB9D35CF,
    $EBCDAF0C, $7B3E89A0, $D6411BD3, $AE1E7E49,
    $00250E2D, $2071B35E, $226800BB, $57B8E0AF,

    $2464369B, $F009B91E, $5563911D, $59DFA6AA,
    $78C14389, $D95A537F, $207D5BA2, $02E5B9C5,
    $83260376, $6295CFA9, $11C81968, $4E734A41,
    $B3472DCA, $7B14A94A, $1B510052, $9A532915,

    $D60F573F, $BC9BC6E4, $2B60A476, $81E67400,
    $08BA6FB5, $571BE91F, $F296EC6B, $2A0DD915,
    $B6636521, $E7B9F9B6, $FF34052E, $C5855664,
    $53B02D5D, $A99F8FA1, $08BA4799, $6E85076A),
    { SECOND 256 }
    ($4B7A70E9, $B5B32944, $DB75092E, $C4192623,
    $AD6EA6B0, $49A7DF7D, $9CEE60B8, $8FEDB266,
    $ECAA8C71, $699A17FF, $5664526C, $C2B19EE1,
    $193602A5, $75094C29, $A0591340, $E4183A3E,

    $3F54989A, $5B429D65, $6B8FE4D6, $99F73FD6,
    $A1D29C07, $EFE830F5, $4D2D38E6, $F0255DC1,
    $4CDD2086, $8470EB26, $6382E9C6, $021ECC5E,
    $09686B3F, $3EBAEFC9, $3C971814, $6B6A70A1,

    $687F3584, $52A0E286, $B79C5305, $AA500737,
    $3E07841C, $7FDEAE5C, $8E7D44EC, $5716F2B8,
    $B03ADA37, $F0500C0D, $F01C1F04, $0200B3FF,
    $AE0CF51A, $3CB574B2, $25837A58, $DC0921BD,

    $D19113F9, $7CA92FF6, $94324773, $22F54701,
    $3AE5E581, $37C2DADC, $C8B57634, $9AF3DDA7,
    $A9446146, $0FD0030E, $ECC8C73E, $A4751E41,
    $E238CD99, $3BEA0E2F, $3280BBA1, $183EB331,

    $4E548B38, $4F6DB908, $6F420D03, $F60A04BF,
    $2CB81290, $24977C79, $5679B072, $BCAF89AF,
    $DE9A771F, $D9930810, $B38BAE12, $DCCF3F2E,
    $5512721F, $2E6B7124, $501ADDE6, $9F84CD87,

    $7A584718, $7408DA17, $BC9F9ABC, $E94B7D8C,
    $EC7AEC3A, $DB851DFA, $63094366, $C464C3D2,
    $EF1C1847, $3215D908, $DD433B37, $24C2BA16,
    $12A14D43, $2A65C451, $50940002, $133AE4DD,

    $71DFF89E, $10314E55, $81AC77D6, $5F11199B,
    $043556F1, $D7A3C76B, $3C11183B, $5924A509,
    $F28FE6ED, $97F1FBFA, $9EBABF2C, $1E153C6E,
    $86E34570, $EAE96FB1, $860E5E0A, $5A3E2AB3,

    $771FE71C, $4E3D06FA, $2965DCB9, $99E71D0F,
    $803E89D6, $5266C825, $2E4CC978, $9C10B36A,
    $C6150EBA, $94E2EA78, $A5FC3C53, $1E0A2DF4,
    $F2F74EA7, $361D2B3D, $1939260F, $19C27960,

    $5223A708, $F71312B6, $EBADFE6E, $EAC31F66,
    $E3BC4595, $A67BC883, $B17F37D1, $018CFF28,
    $C332DDEF, $BE6C5AA5, $65582185, $68AB9802,
    $EECEA50F, $DB2F953B, $2AEF7DAD, $5B6E2F84,

    $1521B628, $29076170, $ECDD4775, $619F1510,
    $13CCA830, $EB61BD96, $0334FE1E, $AA0363CF,
    $B5735C90, $4C70A239, $D59E9E0B, $CBAADE14,
    $EECC86BC, $60622CA7, $9CAB5CAB, $B2F3846E,

    $648B1EAF, $19BDF0CA, $A02369B9, $655ABB50,
    $40685A32, $3C2AB4B3, $319EE9D5, $C021B8F7,
    $9B540B19, $875FA099, $95F7997E, $623D7DA8,
    $F837889A, $97E32D77, $11ED935F, $16681281,

    $0E358829, $C7E61FD6, $96DEDFA1, $7858BA99,
    $57F584A5, $1B227263, $9B83C3FF, $1AC24696,
    $CDB30AEB, $532E3054, $8FD948E4, $6DBC3128,
    $58EBF2EF, $34C6FFEA, $FE28ED61, $EE7C3C73,

    $5D4A14D9, $E864B7E3, $42105D14, $203E13E0,
    $45EEE2B6, $A3AAABEA, $DB6C4F15, $FACB4FD0,
    $C742F442, $EF6ABBB5, $654F3B1D, $41CD2105,
    $D81E799E, $86854DC7, $E44B476A, $3D816250,

    $CF62A1F2, $5B8D2646, $FC8883A0, $C1C7B6A3,
    $7F1524C3, $69CB7492, $47848A0B, $5692B285,
    $095BBF00, $AD19489D, $1462B174, $23820E00,
    $58428D2A, $0C55F5EA, $1DADF43E, $233F7061,

    $3372F092, $8D937E41, $D65FECF1, $6C223BDB,
    $7CDE3759, $CBEE7460, $4085F2A7, $CE77326E,
    $A6078084, $19F8509E, $E8EFD855, $61D99735,
    $A969A7AA, $C50C06C2, $5A04ABFC, $800BCADC,

    $9E447A2E, $C3453484, $FDD56705, $0E1E9EC9,
    $DB73DBD3, $105588CD, $675FDA79, $E3674340,
    $C5C43465, $713E38D8, $3D28F89E, $F16DFF20,
    $153E21E7, $8FB03D4A, $E6E39F2B, $DB83ADF7),
    { THIRD 256 }
    ($E93D5A68, $948140F7, $F64C261C, $94692934,
    $411520F7, $7602D4F7, $BCF46B2E, $D4A20068,
    $D4082471, $3320F46A, $43B7D4B7, $500061AF,
    $1E39F62E, $97244546, $14214F74, $BF8B8840,

    $4D95FC1D, $96B591AF, $70F4DDD3, $66A02F45,
    $BFBC09EC, $03BD9785, $7FAC6DD0, $31CB8504,
    $96EB27B3, $55FD3941, $DA2547E6, $ABCA0A9A,
    $28507825, $530429F4, $0A2C86DA, $E9B66DFB,

    $68DC1462, $D7486900, $680EC0A4, $27A18DEE,
    $4F3FFEA2, $E887AD8C, $B58CE006, $7AF4D6B6,
    $AACE1E7C, $D3375FEC, $CE78A399, $406B2A42,
    $20FE9E35, $D9F385B9, $EE39D7AB, $3B124E8B,

    $1DC9FAF7, $4B6D1856, $26A36631, $EAE397B2,
    $3A6EFA74, $DD5B4332, $6841E7F7, $CA7820FB,
    $FB0AF54E, $D8FEB397, $454056AC, $BA489527,
    $55533A3A, $20838D87, $FE6BA9B7, $D096954B,

    $55A867BC, $A1159A58, $CCA92963, $99E1DB33,
    $A62A4A56, $3F3125F9, $5EF47E1C, $9029317C,
    $FDF8E802, $04272F70, $80BB155C, $05282CE3,
    $95C11548, $E4C66D22, $48C1133F, $C70F86DC,

    $07F9C9EE, $41041F0F, $404779A4, $5D886E17,
    $325F51EB, $D59BC0D1, $F2BCC18F, $41113564,
    $257B7834, $602A9C60, $DFF8E8A3, $1F636C1B,
    $0E12B4C2, $02E1329E, $AF664FD1, $CAD18115,

    $6B2395E0, $333E92E1, $3B240B62, $EEBEB922,
    $85B2A20E, $E6BA0D99, $DE720C8C, $2DA2F728,
    $D0127845, $95B794FD, $647D0862, $E7CCF5F0,
    $5449A36F, $877D48FA, $C39DFD27, $F33E8D1E,

    $0A476341, $992EFF74, $3A6F6EAB, $F4F8FD37,
    $A812DC60, $A1EBDDF8, $991BE14C, $DB6E6B0D,
    $C67B5510, $6D672C37, $2765D43B, $DCD0E804,
    $F1290DC7, $CC00FFA3, $B5390F92, $690FED0B,

    $667B9FFB, $CEDB7D9C, $A091CF0B, $D9155EA3,
    $BB132F88, $515BAD24, $7B9479BF, $763BD6EB,
    $37392EB3, $CC115979, $8026E297, $F42E312D,
    $6842ADA7, $C66A2B3B, $12754CCC, $782EF11C,

    $6A124237, $B79251E7, $06A1BBE6, $4BFB6350,
    $1A6B1018, $11CAEDFA, $3D25BDD8, $E2E1C3C9,
    $44421659, $0A121386, $D90CEC6E, $D5ABEA2A,
    $64AF674E, $DA86A85F, $BEBFE988, $64E4C3FE,

    $9DBC8057, $F0F7C086, $60787BF8, $6003604D,
    $D1FD8346, $F6381FB0, $7745AE04, $D736FCCC,
    $83426B33, $F01EAB71, $B0804187, $3C005E5F,
    $77A057BE, $BDE8AE24, $55464299, $BF582E61,

    $4E58F48F, $F2DDFDA2, $F474EF38, $8789BDC2,
    $5366F9C3, $C8B38E74, $B475F255, $46FCD9B9,
    $7AEB2661, $8B1DDF84, $846A0E79, $915F95E2,
    $466E598E, $20B45770, $8CD55591, $C902DE4C,

    $B90BACE1, $BB8205D0, $11A86248, $7574A99E,
    $B77F19B6, $E0A9DC09, $662D09A1, $C4324633,
    $E85A1F02, $09F0BE8C, $4A99A025, $1D6EFE10,
    $1AB93D1D, $0BA5A4DF, $A186F20F, $2868F169,

    $DCB7DA83, $573906FE, $A1E2CE9B, $4FCD7F52,
    $50115E01, $A70683FA, $A002B5C4, $0DE6D027,
    $9AF88C27, $773F8641, $C3604C06, $61A806B5,
    $F0177A28, $C0F586E0, $006058AA, $30DC7D62,

    $11E69ED7, $2338EA63, $53C2DD94, $C2C21634,
    $BBCBEE56, $90BCB6DE, $EBFC7DA1, $CE591D76,
    $6F05E409, $4B7C0188, $39720A3D, $7C927C24,
    $86E3725F, $724D9DB9, $1AC15BB4, $D39EB8FC,

    $ED545578, $08FCA5B5, $D83D7CD3, $4DAD0FC4,
    $1E50EF5E, $B161E6F8, $A28514D9, $6C51133C,
    $6FD5C7E7, $56E14EC4, $362ABFCE, $DDC6C837,
    $D79A3234, $92638212, $670EFA8E, $406000E0),
    { FOURTH 256 }
    ($3A39CE37, $D3FAF5CF, $ABC27737, $5AC52D1B,
    $5CB0679E, $4FA33742, $D3822740, $99BC9BBE,
    $D5118E9D, $BF0F7315, $D62D1C7E, $C700C47B,
    $B78C1B6B, $21A19045, $B26EB1BE, $6A366EB4,

    $5748AB2F, $BC946E79, $C6A376D2, $6549C2C8,
    $530FF8EE, $468DDE7D, $D5730A1D, $4CD04DC6,
    $2939BBDB, $A9BA4650, $AC9526E8, $BE5EE304,
    $A1FAD5F0, $6A2D519A, $63EF8CE2, $9A86EE22,

    $C089C2B8, $43242EF6, $A51E03AA, $9CF2D0A4,
    $83C061BA, $9BE96A4D, $8FE51550, $BA645BD6,
    $2826A2F9, $A73A3AE1, $4BA99586, $EF5562E9,
    $C72FEFD3, $F752F7DA, $3F046F69, $77FA0A59,

    $80E4A915, $87B08601, $9B09E6AD, $3B3EE593,
    $E990FD5A, $9E34D797, $2CF0B7D9, $022B8B51,
    $96D5AC3A, $017DA67D, $D1CF3ED6, $7C7D2D28,
    $1F9F25CF, $ADF2B89B, $5AD6B472, $5A88F54C,

    $E029AC71, $E019A5E6, $47B0ACFD, $ED93FA9B,
    $E8D3C48D, $283B57CC, $F8D56629, $79132E28,
    $785F0191, $ED756055, $F7960E44, $E3D35E8C,
    $15056DD4, $88F46DBA, $03A16125, $0564F0BD,

    $C3EB9E15, $3C9057A2, $97271AEC, $A93A072A,
    $1B3F6D9B, $1E6321F5, $F59C66FB, $26DCF319,
    $7533D928, $B155FDF5, $03563482, $8ABA3CBB,
    $28517711, $C20AD9F8, $ABCC5167, $CCAD925F,

    $4DE81751, $3830DC8E, $379D5862, $9320F991,
    $EA7A90C2, $FB3E7BCE, $5121CE64, $774FBE32,
    $A8B6E37E, $C3293D46, $48DE5369, $6413E680,
    $A2AE0810, $DD6DB224, $69852DFD, $09072166,

    $B39A460A, $6445C0DD, $586CDECF, $1C20C8AE,
    $5BBEF7DD, $1B588D40, $CCD2017F, $6BB4E3BB,
    $DDA26A7E, $3A59FF45, $3E350A44, $BCB4CDD5,
    $72EACEA8, $FA6484BB, $8D6612AE, $BF3C6F47,

    $D29BE463, $542F5D9E, $AEC2771B, $F64E6370,
    $740E0D8D, $E75B1357, $F8721671, $AF537D5D,
    $4040CB08, $4EB4E2CC, $34D2466A, $0115AF84,
    $E1B00428, $95983A1D, $06B89FB4, $CE6EA048,

    $6F3F3B82, $3520AB82, $011A1D4B, $277227F8,
    $611560B1, $E7933FDC, $BB3A792B, $344525BD,
    $A08839E1, $51CE794B, $2F32C9B7, $A01FBAC9,
    $E01CC87E, $BCC7D1F6, $CF0111C3, $A1E8AAC7,

    $1A908749, $D44FBD9A, $D0DADECB, $D50ADA38,
    $0339C32A, $C6913667, $8DF9317C, $E0B12B4F,
    $F79E59B7, $43F5BB3A, $F2D519FF, $27D9459C,
    $BF97222C, $15E6FC2A, $0F91FC71, $9B941525,

    $FAE59361, $CEB69CEB, $C2A86459, $12BAA8D1,
    $B6C1075E, $E3056A0C, $10D25065, $CB03A442,
    $E0EC6E0E, $1698DB3B, $4C98A0BE, $3278E964,
    $9F1F9532, $E0D392DF, $D3A0342B, $8971F21E,

    $1B0A7441, $4BA3348C, $C5BE7120, $C37632D8,
    $DF359F8D, $9B992F2E, $E60B6F47, $0FE3F11D,
    $E54CDA54, $1EDAD891, $CE6279CF, $CD3E7E6F,
    $1618B166, $FD2C1D05, $848FD2C5, $F6FB2299,

    $F523F357, $A6327623, $93A83531, $56CCCD02,
    $ACF08162, $5A75EBB5, $6E163697, $88D273CC,
    $DE966292, $81B949D0, $4C50901B, $71C65614,
    $E6C6C7BD, $327A140A, $45E1D006, $C3F27B9A,

    $C9AA53FD, $62A80F00, $BB25BFE2, $35BDD2F6,
    $71126905, $B2040222, $B6CBCF7C, $CD769C2B,
    $53113EC0, $1640E3D3, $38ABBD60, $2547ADF0,
    $BA38209C, $F746CE76, $77AFA1C5, $20756060,

    $85CBFE4E, $8AE88DD8, $7AAAF9B0, $4CF9AA7E,
    $1948C25C, $02FB8A8C, $01C36AE4, $D6EBE1F9,
    $90D4F869, $A65CDEA0, $3F09252D, $C208E69F,
    $B74E6132, $CE77E25B, $578FDFE3, $3AC372E6)
    );

  { first 2048 bits of Pi in hexadecimal, low to high, without the leading "3" }
  Pi2048: array [0 .. 255] of Byte = (
    $24, $3F, $6A, $88, $85, $A3, $08, $D3, $13, $19, $8A, $2E, $03, $70, $73, $44,
    $A4, $09, $38, $22, $29, $9F, $31, $D0, $08, $2E, $FA, $98, $EC, $4E, $6C, $89,
    $45, $28, $21, $E6, $38, $D0, $13, $77, $BE, $54, $66, $CF, $34, $E9, $0C, $6C,
    $C0, $AC, $29, $B7, $C9, $7C, $50, $DD, $3F, $84, $D5, $B5, $B5, $47, $09, $17,
    $92, $16, $D5, $D9, $89, $79, $FB, $1B, $D1, $31, $0B, $A6, $98, $DF, $B5, $AC,
    $2F, $FD, $72, $DB, $D0, $1A, $DF, $B7, $B8, $E1, $AF, $ED, $6A, $26, $7E, $96,
    $BA, $7C, $90, $45, $F1, $2C, $7F, $99, $24, $A1, $99, $47, $B3, $91, $6C, $F7,
    $08, $01, $F2, $E2, $85, $8E, $FC, $16, $63, $69, $20, $D8, $71, $57, $4E, $69,
    $A4, $58, $FE, $A3, $F4, $93, $3D, $7E, $0D, $95, $74, $8F, $72, $8E, $B6, $58,
    $71, $8B, $CD, $58, $82, $15, $4A, $EE, $7B, $54, $A4, $1D, $C2, $5A, $59, $B5,
    $9C, $30, $D5, $39, $2A, $F2, $60, $13, $C5, $D1, $B0, $23, $28, $60, $85, $F0,
    $CA, $41, $79, $18, $B8, $DB, $38, $EF, $8E, $79, $DC, $B0, $60, $3A, $18, $0E,
    $6C, $9E, $0E, $8B, $B0, $1E, $8A, $3E, $D7, $15, $77, $C1, $BD, $31, $4B, $27,
    $78, $AF, $2F, $DA, $55, $60, $5C, $60, $E6, $55, $25, $F3, $AA, $55, $AB, $94,
    $57, $48, $98, $62, $63, $E8, $14, $40, $55, $CA, $39, $6A, $2A, $AB, $10, $B6,
    $B4, $CC, $5C, $34, $11, $41, $E8, $CE, $A1, $54, $86, $AF, $7C, $72, $E9, $93);

  BCSalts: array [0 .. 3] of DWORD =
    ($55555555, $AAAAAAAA, $33333333, $CCCCCCCC);

  { SHA-1 constants }
  { 5 magic numbers }
  SHA1_A = DWORD($67452301);
  SHA1_B = DWORD($EFCDAB89);
  SHA1_C = DWORD($98BADCFE);
  SHA1_D = DWORD($10325476);
  SHA1_E = DWORD($C3D2E1F0);
  { four rounds consts }
  SHA1_K1 = DWORD($5A827999);
  SHA1_K2 = DWORD($6ED9EBA1);
  SHA1_K3 = DWORD($8F1BBCDC);
  SHA1_K4 = DWORD($CA62C1D6);
  { Maskes used in byte swap }
  LBMASK_HI = DWORD($FF0000);
  LBMASK_LO = DWORD($FF00);

  INPUTWHITEN = 0;
  RS_GF_FDBK = $14D;
  MDS_GF_FDBK = $169;
  SK_STEP = $02020202;
  SK_BUMP = $01010101;
  SK_ROTL = 9;

  DCPTF_p8x8: TDCPTFp8x8 = (
    (
    $A9, $67, $B3, $E8, $04, $FD, $A3, $76,
    $9A, $92, $80, $78, $E4, $DD, $D1, $38,
    $0D, $C6, $35, $98, $18, $F7, $EC, $6C,
    $43, $75, $37, $26, $FA, $13, $94, $48,
    $F2, $D0, $8B, $30, $84, $54, $DF, $23,
    $19, $5B, $3D, $59, $F3, $AE, $A2, $82,
    $63, $01, $83, $2E, $D9, $51, $9B, $7C,
    $A6, $EB, $A5, $BE, $16, $0C, $E3, $61,
    $C0, $8C, $3A, $F5, $73, $2C, $25, $0B,
    $BB, $4E, $89, $6B, $53, $6A, $B4, $F1,
    $E1, $E6, $BD, $45, $E2, $F4, $B6, $66,
    $CC, $95, $03, $56, $D4, $1C, $1E, $D7,
    $FB, $C3, $8E, $B5, $E9, $CF, $BF, $BA,
    $EA, $77, $39, $AF, $33, $C9, $62, $71,
    $81, $79, $09, $AD, $24, $CD, $F9, $D8,
    $E5, $C5, $B9, $4D, $44, $08, $86, $E7,
    $A1, $1D, $AA, $ED, $06, $70, $B2, $D2,
    $41, $7B, $A0, $11, $31, $C2, $27, $90,
    $20, $F6, $60, $FF, $96, $5C, $B1, $AB,
    $9E, $9C, $52, $1B, $5F, $93, $0A, $EF,
    $91, $85, $49, $EE, $2D, $4F, $8F, $3B,
    $47, $87, $6D, $46, $D6, $3E, $69, $64,
    $2A, $CE, $CB, $2F, $FC, $97, $05, $7A,
    $AC, $7F, $D5, $1A, $4B, $0E, $A7, $5A,
    $28, $14, $3F, $29, $88, $3C, $4C, $02,
    $B8, $DA, $B0, $17, $55, $1F, $8A, $7D,
    $57, $C7, $8D, $74, $B7, $C4, $9F, $72,
    $7E, $15, $22, $12, $58, $07, $99, $34,
    $6E, $50, $DE, $68, $65, $BC, $DB, $F8,
    $C8, $A8, $2B, $40, $DC, $FE, $32, $A4,
    $CA, $10, $21, $F0, $D3, $5D, $0F, $00,
    $6F, $9D, $36, $42, $4A, $5E, $C1, $E0),
    (
    $75, $F3, $C6, $F4, $DB, $7B, $FB, $C8,
    $4A, $D3, $E6, $6B, $45, $7D, $E8, $4B,
    $D6, $32, $D8, $FD, $37, $71, $F1, $E1,
    $30, $0F, $F8, $1B, $87, $FA, $06, $3F,
    $5E, $BA, $AE, $5B, $8A, $00, $BC, $9D,
    $6D, $C1, $B1, $0E, $80, $5D, $D2, $D5,
    $A0, $84, $07, $14, $B5, $90, $2C, $A3,
    $B2, $73, $4C, $54, $92, $74, $36, $51,
    $38, $B0, $BD, $5A, $FC, $60, $62, $96,
    $6C, $42, $F7, $10, $7C, $28, $27, $8C,
    $13, $95, $9C, $C7, $24, $46, $3B, $70,
    $CA, $E3, $85, $CB, $11, $D0, $93, $B8,
    $A6, $83, $20, $FF, $9F, $77, $C3, $CC,
    $03, $6F, $08, $BF, $40, $E7, $2B, $E2,
    $79, $0C, $AA, $82, $41, $3A, $EA, $B9,
    $E4, $9A, $A4, $97, $7E, $DA, $7A, $17,
    $66, $94, $A1, $1D, $3D, $F0, $DE, $B3,
    $0B, $72, $A7, $1C, $EF, $D1, $53, $3E,
    $8F, $33, $26, $5F, $EC, $76, $2A, $49,
    $81, $88, $EE, $21, $C4, $1A, $EB, $D9,
    $C5, $39, $99, $CD, $AD, $31, $8B, $01,
    $18, $23, $DD, $1F, $4E, $2D, $F9, $48,
    $4F, $F2, $65, $8E, $78, $5C, $58, $19,
    $8D, $E5, $98, $57, $67, $7F, $05, $64,
    $AF, $63, $B6, $FE, $F5, $B7, $3C, $A5,
    $CE, $E9, $68, $44, $E0, $4D, $43, $69,
    $29, $2E, $AC, $15, $59, $A8, $0A, $9E,
    $6E, $47, $DF, $34, $35, $6A, $CF, $DC,
    $22, $C9, $C0, $9B, $89, $D4, $ED, $AB,
    $12, $A2, $0D, $52, $BB, $02, $2F, $A9,
    $D7, $61, $1E, $B4, $50, $04, $F6, $C2,
    $16, $25, $86, $56, $55, $09, $BE, $91));

type
  TBlock2048 = array [0 .. 255] of Byte;

  TBCHalfBlock = array [0 .. 1] of Integer;

  TBFBlockEx = packed record
    Xl: array [0 .. 3] of Byte;
    Xr: array [0 .. 3] of Byte;
  end;

var
  { twofish local var structures }
  twofish_SBox: TDCPTFSBox;

class function TCipher.AllCipher: TCipherStyleArray;
var
  cs: TCipherStyle;
begin
  SetLength(Result, Integer(high(TCipherStyle)) + 1);
  for cs := low(TCipherStyle) to high(TCipherStyle) do
      Result[Integer(cs)] := cs;
end;

class function TCipher.NameToHashStyle(n: SystemString; var hash: THashStyle): Boolean;
var
  h: THashStyle;
begin
  Result := False;
  for h := low(THashStyle) to high(THashStyle) do
    if SameText(CHashName[h], n) then
      begin
        hash := h;
        Result := True;
        Exit;
      end;
end;

class function TCipher.BuffToString(buff: Pointer; Size: NativeInt): TPascalString;
begin
  HashToString(buff, Size, Result);
end;

class function TCipher.StringToBuff(const Hex: TPascalString; var Buf; BufSize: Cardinal): Boolean;
begin
  Result := HexToBuffer(Hex, Buf, BufSize);
end;

class procedure TCipher.HashToString(hash: Pointer; Size: NativeInt; var output: TPascalString);
const
  HexArr: array [0 .. 15] of U_Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  output.Len := Size * 2;
  for i := 0 to Size - 1 do
    begin
      output.buff[i * 2] := HexArr[(PByte(nativeUInt(hash) + i)^ shr 4) and $0F];
      output.buff[i * 2 + 1] := HexArr[PByte(nativeUInt(hash) + i)^ and $0F];
    end;
end;

class procedure TCipher.HashToString(hash: TSHA1Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TMD5Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TBytes; var output: TPascalString);
begin
  HashToString(@hash[0], length(hash), output);
end;

class procedure TCipher.HashToString(hash: TBytes; var output: SystemString);
var
  s: TPascalString;
begin
  if length(hash) > 0 then
    begin
      HashToString(@hash[0], length(hash), s);
      output := s.Text;
    end
  else
      output := '';
end;

class function TCipher.CompareHash(h1, h2: TSHA1Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: TMD5Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: Pointer; Size: Word): Boolean;
begin
  Result := CompareMemory(h1, h2, Size);
end;

class function TCipher.CompareHash(h1, h2: TBytes): Boolean;
begin
  if length(h1) = 0 then
      Result := length(h2) = 0
  else
      Result := (length(h1) = length(h2)) and (CompareMemory(@h1[0], @h2[0], length(h1)));
end;

class function TCipher.CompareKey(k1, k2: TCipherKeyBuffer): Boolean;
begin
  if length(k1) = 0 then
      Result := length(k2) = 0
  else
      Result := (length(k1) = length(k2)) and (CompareMemory(@k1[0], @k2[0], length(k1)));
end;

class function TCipher.GenerateSha1Hash(sour: Pointer; Size: NativeInt): TSHA1Digest;
begin
  TSHA1.HashSHA1(Result, sour^, Size);
end;

class function TCipher.GenerateMD5Hash(sour: Pointer; Size: NativeInt): TMD5Digest;
begin
{$IF Defined(FastMD5) and (Defined(WIN32) or Defined(WIN64))}
  Result := FastMD5(sour, Size);
{$ELSE}
  TCipherMD5.HashMD5(Result, sour^, Size);
{$IFEND}
end;

class procedure TCipher.GenerateHash(sour: Pointer; Size: NativeInt; OutHash: Pointer; HashSize: NativeInt);
begin
  TLMD.HashLMD(OutHash^, HashSize, sour^, Size);
end;

class procedure TCipher.GenerateHashByte(hs: THashStyle; sour: Pointer; Size: NativeInt; var output: TBytes);
var
  swBuff: TBytes;
begin
  if Size <= 0 then
    begin
      SetLength(output, 0);
      Exit;
    end;

  if Size < 6 then
    begin
      SetLength(output, 16);
      PMD5(@output[0])^ := umlMD5(PByte(sour), Size);
      Exit;
    end;

  case hs of
    hsNone:
      begin
        SetLength(output, 0);
      end;
    hsFastMD5:
      begin
        SetLength(output, 16);
        PMD5(@output[0])^ := umlMD5(PByte(sour), DWORD(Size));
      end;
    hsMD5, hs16:
      begin
        SetLength(output, 16);
        TCipherMD5.HashMD5(PMD5Digest(@output[0])^, sour^, Size);
      end;
    hsSHA1:
      begin
        SetLength(output, 20);
        TSHA1.HashSHA1(PSHA1Digest(@output[0])^, sour^, Size);
      end;
    hs256:
      begin
        SetLength(output, 256);
        TLMD.HashLMD((@output[0])^, 256, sour^, Size);
      end;
    hs128:
      begin
        SetLength(output, 128);
        TLMD.HashLMD((@output[0])^, 128, sour^, Size);
      end;
    hs64:
      begin
        SetLength(output, 64);
        TLMD.HashLMD((@output[0])^, 64, sour^, Size);
      end;
    hs32:
      begin
        SetLength(output, 32);
        TLMD.HashLMD((@output[0])^, 32, sour^, Size);
      end;
    hsELF:
      begin
        SetLength(output, 4);
        TMISC.HashELF(PDWORD(@output[0])^, sour^, Size);
      end;
    hsELF64:
      begin
        SetLength(output, 8);
        TMISC.HashELF64(PInt64(@output[0])^, sour^, Size);
      end;
    hsMix128:
      begin
        SetLength(output, 4);

        if Size < 16 then
          begin
            SetLength(swBuff, 16);
            TLMD.HashLMD(swBuff[0], 16, sour^, Size);
            TMISC.HashMix128(PDWORD(@output[0])^, swBuff[0], 16);
          end
        else
            TMISC.HashMix128(PDWORD(@output[0])^, sour^, Size);
      end;
    hsCRC16:
      begin
        SetLength(output, 2);
        PWORD(@output[0])^ := umlCRC16(PByte(sour), Size);
      end;
    hsCRC32:
      begin
        SetLength(output, 4);
        PCardinal(@output[0])^ := umlCRC32(PByte(sour), Size);
      end;
  end;
end;

class procedure TCipher.GenerateSha1HashString(sour: Pointer; Size: NativeInt; var output: TPascalString);
var
  v: TSHA1Digest;
begin
  v := GenerateSha1Hash(sour, Size);
  HashToString(v, output);
end;

class procedure TCipher.GenerateMD5HashString(sour: Pointer; Size: NativeInt; var output: TPascalString);
var
  v: TMD5Digest;
begin
  v := GenerateMD5Hash(sour, Size);
  HashToString(v, output);
end;

class procedure TCipher.GenerateHashString(sour: Pointer; Size, HashSize: NativeInt; var output: TPascalString);
var
  v: TBytes;
begin
  SetLength(v, HashSize);
  GenerateHash(sour, Size, @v[0], HashSize);
  HashToString(@v[0], HashSize, output);
end;

class function TCipher.BufferToHex(const Buf; BufSize: Cardinal): TPascalString;
begin
  Result := BuffToString(@Buf, BufSize);
end;

class function TCipher.HexToBuffer(const Hex: TPascalString; var Buf; BufSize: Cardinal): Boolean;
var
  i, c: Integer;
  filStr: TPascalString;
  Count: Integer;
  cChar: Char;
begin
  Result := False;
  filStr.Text := '';
  for cChar in Hex.buff do
    if CharIn(cChar, [c0to9, cLoAtoF, cHiAtoF]) then
        filStr.Append(cChar);

  FillPtrByte(@Buf, BufSize, 0);
  Count := Min(filStr.Len div 2, BufSize);

  for i := 0 to Count - 1 do
    begin
      val('$' + filStr[i * 2 + 1] + filStr[i * 2 + 2], TCCByteArray(Buf)[i], c);
      if c <> 0 then
          Exit;
    end;

  Result := True;
end;

class procedure TCipher.GenerateNoneKey(var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size);
  output[0] := Byte(TCipherKeyStyle.cksNone);
end;

class procedure TCipher.GenerateKey64(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey64Size);
  output[0] := Byte(TCipherKeyStyle.cksKey64);
  TLMD.GenerateLMDKey((@output[1])^, cKey64Size, s.Bytes);
end;

class procedure TCipher.GenerateKey64(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey64Size);
  output[0] := Byte(TCipherKeyStyle.cksKey64);
  TLMD.HashLMD((@output[1])^, cKey64Size, sour^, Size);
end;

class procedure TCipher.GenerateKey128(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey128Size);
  output[0] := Byte(TCipherKeyStyle.cksKey128);
  TLMD.GenerateLMDKey((@output[1])^, cKey128Size, s.Bytes);
end;

class procedure TCipher.GenerateKey128(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey128Size);
  output[0] := Byte(TCipherKeyStyle.cksKey128);
  TLMD.HashLMD((@output[1])^, cKey128Size, sour^, Size);
end;

class procedure TCipher.GenerateKey256(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey256Size);
  output[0] := Byte(TCipherKeyStyle.cksKey256);
  TLMD.GenerateLMDKey((@output[1])^, cKey256Size, s.Bytes);
end;

class procedure TCipher.GenerateKey256(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey256Size);
  output[0] := Byte(TCipherKeyStyle.cksKey256);
  TLMD.HashLMD((@output[1])^, cKey256Size, sour^, Size);
end;

class procedure TCipher.Generate3Key64(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey192Size);
  output[0] := Byte(TCipherKeyStyle.cks3Key64);
  TLMD.GenerateLMDKey((@output[1])^, cKey192Size, s.Bytes);
end;

class procedure TCipher.Generate3Key64(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey192Size);
  output[0] := Byte(TCipherKeyStyle.cks3Key64);
  TLMD.HashLMD((@output[1])^, cKey192Size, sour^, Size);
end;

class procedure TCipher.Generate2IntKey(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey2DWORDSize);
  output[0] := Byte(TCipherKeyStyle.cks2IntKey);
  TLMD.GenerateLMDKey((@output[1])^, cKey2DWORDSize, s.Bytes);
end;

class procedure TCipher.Generate2IntKey(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey2DWORDSize);
  output[0] := Byte(TCipherKeyStyle.cks2IntKey);
  TLMD.HashLMD((@output[1])^, cKey2DWORDSize, sour^, Size);
end;

class procedure TCipher.GenerateIntKey(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKeyDWORDSize);
  output[0] := Byte(TCipherKeyStyle.cksIntKey);
  TLMD.GenerateLMDKey((@output[1])^, cKeyDWORDSize, s.Bytes);
end;

class procedure TCipher.GenerateIntKey(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKeyDWORDSize);
  output[0] := Byte(TCipherKeyStyle.cksIntKey);
  TLMD.HashLMD((@output[1])^, cKeyDWORDSize, sour^, Size);
end;

class procedure TCipher.GenerateBytesKey(const s: TPascalString; KeySize: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cIntSize + KeySize);
  output[0] := Byte(TCipherKeyStyle.ckyDynamicKey);
  PDWORD(@output[1])^ := KeySize;
  TLMD.GenerateLMDKey((@output[1 + cIntSize])^, KeySize, s.Bytes);
end;

class procedure TCipher.GenerateBytesKey(sour: Pointer; Size, KeySize: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cIntSize + KeySize);
  output[0] := Byte(TCipherKeyStyle.ckyDynamicKey);
  PDWORD(@output[1])^ := KeySize;
  TLMD.HashLMD((@output[1 + cIntSize])^, KeySize, sour^, Size);
end;

class procedure TCipher.GenerateKey64(const k: TDESKey; var output: TCipherKeyBuffer);
begin
  GenerateKey(k, output);
end;

class procedure TCipher.GenerateKey128(const k1, k2: TKey64; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey128Size);
  output[0] := Byte(TCipherKeyStyle.cksKey128);
  PKey64(@output[1])^ := k1;
  PKey64(@output[1 + cKey64Size])^ := k2;
end;

class procedure TCipher.GenerateKey(const k: TKey64; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey64Size);
  output[0] := Byte(TCipherKeyStyle.cksKey64);
  PKey64(@output[1])^ := k;
end;

class procedure TCipher.GenerateKey(const k1, k2, k3: TKey64; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey192Size);
  output[0] := Byte(TCipherKeyStyle.cks3Key64);
  PKey64(@output[1])^ := k1;
  PKey64(@output[1 + cKey64Size])^ := k2;
  PKey64(@output[1 + cKey64Size + cKey64Size])^ := k3;
end;

class procedure TCipher.GenerateKey(const k: TKey128; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey128Size);
  output[0] := Byte(TCipherKeyStyle.cksKey128);
  PKey128(@output[1])^ := k;
end;

class procedure TCipher.GenerateKey(const k: TKey256; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey256Size);
  output[0] := Byte(TCipherKeyStyle.cksKey256);
  PKey256(@output[1])^ := k;
end;

class procedure TCipher.GenerateKey(const k1, k2: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey2DWORDSize);
  output[0] := Byte(TCipherKeyStyle.cks2IntKey);
  PInteger(@output[1])^ := k1;
  PInteger(@output[1 + cKeyDWORDSize])^ := k2;
end;

class procedure TCipher.GenerateKey(const k: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKeyDWORDSize);
  output[0] := Byte(TCipherKeyStyle.cksIntKey);
  PInteger(@output[1])^ := k;
end;

class procedure TCipher.GenerateKey(const k: TDESKey; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey64Size);
  output[0] := Byte(TCipherKeyStyle.cksKey64);
  PDESKey(@output[1])^ := k;
end;

class procedure TCipher.GenerateKey(const key: PByte; Size: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cIntSize + Size);
  output[0] := Byte(TCipherKeyStyle.ckyDynamicKey);
  PInteger(@output[1])^ := Size;
  CopyPtr(key, @output[1 + cIntSize], Size);
end;

class procedure TCipher.GenerateKey(cs: TCipherStyle; buffPtr: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  case cCipherKeyStyle[cs] of
    cksNone: GenerateNoneKey(output);
    cksKey64: GenerateKey64(buffPtr, Size, output);
    cks3Key64: Generate3Key64(buffPtr, Size, output);
    cksKey128: GenerateKey128(buffPtr, Size, output);
    cks2IntKey: Generate2IntKey(buffPtr, Size, output);
    cksIntKey: GenerateIntKey(buffPtr, Size, output);
    ckyDynamicKey: GenerateBytesKey(buffPtr, Size, 32, output);
  end;
end;

class procedure TCipher.GenerateKey(cs: TCipherStyle; s: TPascalString; var output: TCipherKeyBuffer);
var
  buff: TBytes;
  key: TBytes;
begin
  buff := s.Bytes;
  SetLength(key, 128);
  TCipher.GenerateHash(@buff[0], SizeOf(buff), @key[0], 128);
  GenerateKey(cs, @key[0], 128, output);
end;

class function TCipher.GetKeyStyle(const p: PCipherKeyBuffer): TCipherKeyStyle;
begin
  Result := TCipherKeyStyle(p^[0]);
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey64): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cksKey64;
  if not Result then
      Exit;
  k := PKey64(@KeyBuffPtr^[1])^
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k1, k2, k3: TKey64): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cks3Key64;
  if not Result then
      Exit;
  k1 := PKey64(@KeyBuffPtr^[1])^;
  k2 := PKey64(@KeyBuffPtr^[1 + cKey64Size])^;
  k3 := PKey64(@KeyBuffPtr^[1 + cKey64Size + cKey64Size])^;
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey128): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cksKey128;
  if not Result then
      Exit;
  k := PKey128(@KeyBuffPtr^[1])^
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey256): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cksKey256;
  if not Result then
      Exit;
  k := PKey256(@KeyBuffPtr^[1])^
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k1, k2: DWORD): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cks2IntKey;
  if not Result then
      Exit;
  k1 := PDWORD(@KeyBuffPtr^[1])^;
  k2 := PDWORD(@KeyBuffPtr^[1 + cKeyDWORDSize])^;
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: DWORD): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cksIntKey;
  if not Result then
      Exit;
  k := PDWORD(@KeyBuffPtr^[1])^;
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TDESKey): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cksKey64;
  if not Result then
      Exit;
  k := PDESKey(@KeyBuffPtr^[1])^
end;

class function TCipher.GetBytesKey(const KeyBuffPtr: PCipherKeyBuffer; var key: TBytes): Boolean;
var
  siz: Integer;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.ckyDynamicKey;
  if not Result then
      Exit;
  siz := PInteger(@KeyBuffPtr^[1])^;
  SetLength(key, siz);
  CopyPtr(@KeyBuffPtr^[1 + cIntSize], @key[0], siz);
end;

class procedure TCipher.EncryptTail(TailPtr: Pointer; TailSize: NativeInt);
begin
  BlockCBC(TailPtr, TailSize, @SystemCBC[0], length(SystemCBC));
end;

class function TCipher.OLDDES(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TDESKey;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      p := 0;
      repeat
        umlDES(PDESKey(nativeUInt(sour) + p)^, PDESKey(nativeUInt(sour) + p)^, k, Encrypt);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);

  Result := True;
end;

class function TCipher.DES64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey64;
  d: TDESContext;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;
      TDES.InitEncryptDES(k, d, Encrypt);

      p := 0;
      repeat
        TDES.EncryptDES(d, PDESBlock(nativeUInt(sour) + p)^);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);

  Result := True;
end;

class function TCipher.DES128(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  d: TTripleDESContext;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;
      TDES.InitEncryptTripleDES(k, d, Encrypt);

      p := 0;
      repeat
        TDES.EncryptTripleDES(d, PDESBlock(nativeUInt(sour) + p)^);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);

  Result := True;
end;

class function TCipher.DES192(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k1, k2, k3: TKey64;
  d: TTripleDESContext3Key;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k1, k2, k3) then
          Exit;
      TDES.InitEncryptTripleDES3Key(k1, k2, k3, d, Encrypt);

      p := 0;
      repeat
        TDES.EncryptTripleDES3Key(d, PDESBlock(nativeUInt(sour) + p)^);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.Blowfish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  d: TBFContext;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;
      TBlowfish.InitEncryptBF(k, d);

      p := 0;
      repeat
        TBlowfish.EncryptBF(d, PBFBlock(nativeUInt(sour) + p)^, Encrypt);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);

  Result := True;
end;

class function TCipher.LBC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  d: TLBCContext;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;
      TLBC.InitEncryptLBC(k, d, 16, Encrypt);

      p := 0;
      repeat
        TLBC.EncryptLBC(d, PLBCBlock(nativeUInt(sour) + p)^);
        p := p + 16;
      until p + 16 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.LQC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      p := 0;
      repeat
        TLBC.EncryptLQC(k, PLQCBlock(nativeUInt(sour) + p)^, Encrypt);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.RNG32(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
var
  k: DWORD;
  d: TRNG32Context;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if not GetKey(KeyBuff, k) then
      Exit;

  TRNG.InitEncryptRNG32(k, d);

  TRNG.EncryptRNG32(d, sour^, Size);

  Result := True;
end;

class function TCipher.RNG64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
var
  k1, k2: DWORD;
  d: TRNG64Context;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if not GetKey(KeyBuff, k1, k2) then
      Exit;

  TRNG.InitEncryptRNG64(k1, k2, d);

  TRNG.EncryptRNG64(d, sour^, Size);

  Result := True;
end;

class function TCipher.LSC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
var
  k: TBytes;
  k255: TBytes;
  d: TLSCContext;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if not GetBytesKey(KeyBuff, k) then
      Exit;

  if length(k) > 255 then
    begin
      SetLength(k255, 255);
      TLMD.GenerateLMDKey((@k255[0])^, 255, k);
    end
  else
      k255 := k;

  TLSC.InitEncryptLSC((@k255[0])^, length(k255), d);

  TLSC.EncryptLSC(d, sour^, Size);

  Result := True;
end;

class function TCipher.TwoFish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k, k256: TBytes;
  SubKeys: TDCPTFSubKeys;
  SBox: TDCPTFSBox;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if not GetBytesKey(KeyBuff, k) then
      Exit;

  if Size >= 16 then
    begin
      SetLength(k256, 256);
      TLMD.GenerateLMDKey((@k256[0])^, 256, k);

      DCP_twofish_InitKey((@k256[0])^, 256, SubKeys, SBox);

      if Encrypt then
        begin
          p := 0;
          repeat
            DCP_twofish_EncryptECB(SubKeys, SBox, P128Bit(nativeUInt(sour) + p)^, P128Bit(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end
      else
        begin
          p := 0;
          repeat
            DCP_twofish_DecryptECB(SubKeys, SBox, P128Bit(nativeUInt(sour) + p)^, P128Bit(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.XXTea512(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 64 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      if Encrypt then
        begin
          p := 0;
          repeat
            XXTEAEncrypt(k, PXXTEABlock(nativeUInt(sour) + p)^);
            p := p + 64;
          until p + 64 > Size;
        end
      else
        begin
          p := 0;
          repeat
            XXTEADecrypt(k, PXXTEABlock(nativeUInt(sour) + p)^);
            p := p + 64;
          until p + 64 > Size;
        end;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.RC6(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k, k256: TBytes;
  d: TRC6Key;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if Size >= 16 then
    begin
      if not GetBytesKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 256);
      TLMD.GenerateLMDKey((@k256[0])^, 256, k);

      TRC6.InitKey(@k256[0], 256, d);

      if Encrypt then
        begin
          p := 0;
          repeat
            TRC6.Encrypt(d, PRC6Block(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end
      else
        begin
          p := 0;
          repeat
            TRC6.Decrypt(d, PRC6Block(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class procedure TCipher.BlockCBC(sour: Pointer; Size: NativeInt; boxBuff: Pointer; boxSiz: NativeInt);
var
  p: nativeUInt;
begin
  if Size = 0 then
      Exit;
  if boxSiz >= Size then
    begin
      TMISC.XorMem(sour^, boxBuff^, Size);
    end
  else
    begin
      p := 0;
      repeat
        TMISC.XorMem(Pointer(nativeUInt(sour) + p)^, boxBuff^, boxSiz);
        p := p + boxSiz;
      until p + boxSiz > Size;

      BlockCBC(Pointer(nativeUInt(sour) + p), Size - p, boxBuff, boxSiz);
    end;
end;

class function TCipher.EncryptBuffer(cs: TCipherStyle; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
  Result := False;
  case cs of
    csNone: Result := True;
    csDES64: Result := DES64(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csDES128: Result := DES128(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csDES192: Result := DES192(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csBlowfish: Result := Blowfish(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csLBC: Result := LBC(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csLQC: Result := LQC(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRNG32: Result := RNG32(sour, Size, KeyBuff);
    csRNG64: Result := RNG64(sour, Size, KeyBuff);
    csLSC: Result := LSC(sour, Size, KeyBuff);
    csTwoFish: Result := TwoFish(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csXXTea512: Result := XXTea512(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRC6: Result := RC6(sour, Size, KeyBuff, Encrypt, ProcessTail);
  end;
end;

class function TCipher.EncryptBufferCBC(cs: TCipherStyle; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
  if cs = TCipherStyle.csNone then
    begin
      Result := True;
      Exit;
    end;

  if Encrypt then
    begin
      Result := EncryptBuffer(cs, sour, Size, KeyBuff, Encrypt, ProcessTail);
      if Result then
          BlockCBC(sour, Size, @SystemCBC[0], length(SystemCBC));
    end
  else
    begin
      BlockCBC(sour, Size, @SystemCBC[0], length(SystemCBC));
      Result := EncryptBuffer(cs, sour, Size, KeyBuff, Encrypt, ProcessTail);
    end;
end;

{$IFDEF parallel}


procedure TParallelCipher.DES64_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TDES.EncryptDES(PDESContext(key)^, PDESBlock(nativeUInt(buff) + p)^);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.DES128_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TDES.EncryptTripleDES(PTripleDESContext(key)^, PDESBlock(nativeUInt(buff) + p)^);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.DES192_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TDES.EncryptTripleDES3Key(PTripleDESContext3Key(key)^, PDESBlock(nativeUInt(buff) + p)^);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.Blowfish_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TBlowfish.EncryptBF(PBFContext(key)^, PBFBlock(nativeUInt(buff) + p)^, PParallelCipherJobData(Job)^.Encrypt);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.LBC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TLBC.EncryptLBC(PLBCContext(key)^, PLBCBlock(nativeUInt(buff) + p)^);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.LQC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TLBC.EncryptLQC(PKey128(key)^, PLQCBlock(nativeUInt(buff) + p)^, PParallelCipherJobData(Job)^.Encrypt);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.TwoFish_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  if PParallelCipherJobData(Job)^.Encrypt then
    begin
      p := 0;
      repeat
        DCP_twofish_EncryptECB(
          PTwoFishContext(key)^.SubKeys,
          PTwoFishContext(key)^.SBox,
          P128Bit(nativeUInt(buff) + p)^, P128Bit(nativeUInt(buff) + p)^);

        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end
  else
    begin
      p := 0;
      repeat
        DCP_twofish_DecryptECB(PTwoFishContext(key)^.SubKeys,
          PTwoFishContext(key)^.SBox,
          P128Bit(nativeUInt(buff) + p)^, P128Bit(nativeUInt(buff) + p)^);

        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end;
end;

procedure TParallelCipher.XXTea512_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  if PParallelCipherJobData(Job)^.Encrypt then
    begin
      p := 0;
      repeat
        XXTEAEncrypt(PKey128(key)^, PXXTEABlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end
  else
    begin
      p := 0;
      repeat
        XXTEADecrypt(PKey128(key)^, PXXTEABlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end;
end;

procedure TParallelCipher.RC6_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  if PParallelCipherJobData(Job)^.Encrypt then
    begin
      p := 0;
      repeat
        TRC6.Encrypt(PRC6Key(key)^, PRC6Block(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end
  else
    begin
      p := 0;
      repeat
        TRC6.Decrypt(PRC6Key(key)^, PRC6Block(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end;
end;

procedure TParallelCipher.BlockCBC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
begin
  TCipher.BlockCBC(buff, Size, key, PParallelCipherJobData(Job)^.BlockLen);
end;

procedure TParallelCipher.ParallelCipherCall(const JobData: PParallelCipherJobData; const FromIndex, ToIndex: Integer);
var
  newBuffPtr: Pointer;
  newBuffSiz: nativeUInt;
begin
  newBuffPtr := Pointer(nativeUInt(JobData^.OriginBuffer) + (FromIndex * JobData^.BlockLen));
  try
      newBuffSiz := (ToIndex - FromIndex) * JobData^.BlockLen;
  except
  end;

  try
      JobData^.cipherFunc(JobData, newBuffPtr, JobData^.KeyBuffer, newBuffSiz);
  except
  end;

  inc(JobData^.CompletedBlock, (ToIndex - FromIndex));
end;

procedure TParallelCipher.RunParallel(const JobData: PParallelCipherJobData; const Total, Depth: Integer);
var
  StepTotal, stepW: Integer;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    w: Integer;
  begin
    w := stepW * pass;
    if w + stepW <= Total then
        ParallelCipherCall(JobData, w, w + stepW)
    else
        ParallelCipherCall(JobData, w, Total);
  end;
{$ENDIF FPC}


begin
  if Total <= 0 then
      Exit;
  if (Depth <= 0) or (Total < Depth) then
    begin
      ParallelCipherCall(JobData, 0, Total);
      Exit;
    end;
  stepW := Total div Depth;
  StepTotal := Total div stepW;
  if Total mod stepW > 0 then
      inc(StepTotal);

{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, StepTotal - 1);
{$ELSE FPC}
  TParallel.for(0, StepTotal - 1, procedure(pass: Integer)
    var
      w: Integer;
    begin
      w := stepW * pass;
      if w + stepW <= Total then
          ParallelCipherCall(JobData, w, w + stepW)
      else
          ParallelCipherCall(JobData, w, Total);
    end);
{$ENDIF FPC}
end;

constructor TParallelCipher.Create;
begin
  inherited Create;
  ParallelDepth := DefaultParallelDepth;
end;

destructor TParallelCipher.Destroy;
begin
  inherited Destroy;
end;

function TParallelCipher.DES64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey64;
  Context: TDESContext;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      TDES.InitEncryptDES(k, Context, Encrypt);

{$IFDEF FPC}
      JobData.cipherFunc := @DES64_Parallel;
{$ELSE}
      JobData.cipherFunc := DES64_Parallel;
{$ENDIF}
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      try
          RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);
      except
      end;
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.DES128(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  Context: TTripleDESContext;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      TDES.InitEncryptTripleDES(k, Context, Encrypt);

{$IFDEF FPC}
      JobData.cipherFunc := @DES128_Parallel;
{$ELSE}
      JobData.cipherFunc := DES128_Parallel;
{$ENDIF}
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.DES192(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k1, k2, k3: TKey64;
  Context: TTripleDESContext3Key;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k1, k2, k3) then
          Exit;

      TDES.InitEncryptTripleDES3Key(k1, k2, k3, Context, Encrypt);

{$IFDEF FPC}
      JobData.cipherFunc := @DES192_Parallel;
{$ELSE}
      JobData.cipherFunc := DES192_Parallel;
{$ENDIF}
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.Blowfish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  Context: TBFContext;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      TBlowfish.InitEncryptBF(k, Context);

{$IFDEF FPC}
      JobData.cipherFunc := @Blowfish_Parallel;
{$ELSE}
      JobData.cipherFunc := Blowfish_Parallel;
{$ENDIF}
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.LBC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  Context: TLBCContext;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      TLBC.InitEncryptLBC(k, Context, 16, Encrypt);

{$IFDEF FPC}
      JobData.cipherFunc := @LBC_Parallel;
{$ELSE}
      JobData.cipherFunc := LBC_Parallel;
{$ENDIF}
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 16;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.LQC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

{$IFDEF FPC}
      JobData.cipherFunc := @LQC_Parallel;
{$ELSE}
      JobData.cipherFunc := LQC_Parallel;
{$ENDIF}
      JobData.KeyBuffer := @k;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.TwoFish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k, k256: TBytes;
  Context: TTwoFishContext;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin

      if not TCipher.GetBytesKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 256);
      TLMD.GenerateLMDKey((@k256[0])^, 256, k);

      DCP_twofish_InitKey((@k256[0])^, 256, Context.SubKeys, Context.SBox);

{$IFDEF FPC}
      JobData.cipherFunc := @TwoFish_Parallel;
{$ELSE}
      JobData.cipherFunc := TwoFish_Parallel;
{$ENDIF}
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 16;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.XXTea512(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 64 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

{$IFDEF FPC}
      JobData.cipherFunc := @XXTea512_Parallel;
{$ELSE}
      JobData.cipherFunc := XXTea512_Parallel;
{$ENDIF}
      JobData.KeyBuffer := @k;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 64;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.RC6(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k, k256: TBytes;
  Context: TRC6Key;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not TCipher.GetBytesKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 256);
      TLMD.GenerateLMDKey((@k256[0])^, 256, k);

      TRC6.InitKey(@k256[0], 256, Context);

{$IFDEF FPC}
      JobData.cipherFunc := @RC6_Parallel;
{$ELSE}
      JobData.cipherFunc := RC6_Parallel;
{$ENDIF}
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 16;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

procedure TParallelCipher.BlockCBC(sour: Pointer; Size: NativeInt; boxBuff: Pointer; boxSiz: NativeInt);
var
  JobData: TParallelCipherJobData;
  tailSiz: NativeInt;
begin
  if Size <= 0 then
      Exit;
  if boxSiz <= 0 then
      Exit;

  if boxSiz >= Size then
    begin
      TCipher.BlockCBC(sour, Size, boxBuff, boxSiz);
      Exit;
    end;

{$IFDEF FPC}
  JobData.cipherFunc := @BlockCBC_Parallel;
{$ELSE}
  JobData.cipherFunc := BlockCBC_Parallel;
{$ENDIF}
  JobData.KeyBuffer := boxBuff;
  JobData.OriginBuffer := sour;
  JobData.BlockLen := boxSiz;
  JobData.TotalBlock := Size div JobData.BlockLen;
  JobData.CompletedBlock := 0;
  JobData.Encrypt := True;

  RunParallel(@JobData, JobData.TotalBlock, ParallelDepth);

  tailSiz := Size mod JobData.BlockLen;

  if tailSiz > 0 then
      TCipher.BlockCBC(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz, boxBuff, boxSiz);
end;

function TParallelCipher.EncryptBuffer(cs: TCipherStyle; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
  Result := False;
  case cs of
    csNone: Result := True;
    csDES64: Result := DES64(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csDES128: Result := DES128(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csDES192: Result := DES192(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csBlowfish: Result := Blowfish(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csLBC: Result := LBC(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csLQC: Result := LQC(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRNG32: Result := TCipher.RNG32(sour, Size, KeyBuff);
    csRNG64: Result := TCipher.RNG64(sour, Size, KeyBuff);
    csLSC: Result := TCipher.LSC(sour, Size, KeyBuff);
    csTwoFish: Result := TwoFish(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csXXTea512: Result := XXTea512(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRC6: Result := RC6(sour, Size, KeyBuff, Encrypt, ProcessTail);
  end;
end;

function TParallelCipher.EncryptBufferCBC(cs: TCipherStyle; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
  if cs = TCipherStyle.csNone then
    begin
      Result := True;
      Exit;
    end;

  if Encrypt then
    begin
      Result := EncryptBuffer(cs, sour, Size, KeyBuff, Encrypt, ProcessTail);
      if Result then
          BlockCBC(sour, Size, @SystemCBC[0], length(SystemCBC));
    end
  else
    begin
      BlockCBC(sour, Size, @SystemCBC[0], length(SystemCBC));
      Result := EncryptBuffer(cs, sour, Size, KeyBuff, Encrypt, ProcessTail);
    end;
end;

{$ENDIF}


procedure InitSysCBCAndDefaultKey(rand: Int64);
var
  i: Integer;
  Seed: TInt64;
begin
{$IFDEF parallel}
  { system default parallel depth }
  DefaultParallelDepth := CPUCount * 2;
{$ENDIF}
  SetLength(SystemCBC, 1024 * 1024);
  Seed.i := rand;
  for i := 0 to (length(SystemCBC) div 4) - 1 do
      PInteger(@SystemCBC[i * 4])^ := TMISC.Random64(Seed);

  TCipher.GenerateKey(DefaultCipherStyle, @rand, SizeOf(rand), DefaultKey);
end;

function SequEncryptWithDirect(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TCipherKeyBuffer;
begin
  TCipher.GenerateKey(cs, @key[0], length(key), k);
  Result := TCipher.EncryptBuffer(cs, sour, Size, @k, Encrypt, ProcessTail);
end;

function SequEncryptWithDirect(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Encrypt then
    begin
      for i := low(ca) to high(ca) do
          Result := Result and SequEncryptWithDirect(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end
  else
    begin
      for i := high(ca) downto low(ca) do
          Result := Result and SequEncryptWithDirect(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end;
end;

{$IFDEF parallel}


function SequEncryptWithParallel(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TCipherKeyBuffer;
  Parallel: TParallelCipher;
begin
  TCipher.GenerateKey(cs, @key[0], length(key), k);

  Parallel := TParallelCipher.Create;
  Parallel.ParallelDepth := DefaultParallelDepth;
  Result := Parallel.EncryptBuffer(cs, sour, Size, @k, Encrypt, ProcessTail);
  DisposeObject(Parallel);
end;

function SequEncryptWithParallel(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Encrypt then
    begin
      for i := low(ca) to high(ca) do
          Result := Result and SequEncryptWithParallel(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end
  else
    begin
      for i := high(ca) downto low(ca) do
          Result := Result and SequEncryptWithParallel(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end;
end;
{$ENDIF}


function SequEncrypt(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
begin
{$IFDEF parallel}
  if Size > 1024 then
      Result := SequEncryptWithParallel(ca, sour, Size, key, Encrypt, ProcessTail)
  else
{$ENDIF}
      Result := SequEncryptWithDirect(ca, sour, Size, key, Encrypt, ProcessTail);
end;

function SequEncrypt(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
begin
{$IFDEF parallel}
  if Size > 1024 then
      Result := SequEncryptWithParallel(cs, sour, Size, key, Encrypt, ProcessTail)
  else
{$ENDIF}
      Result := SequEncryptWithDirect(cs, sour, Size, key, Encrypt, ProcessTail);
end;

function SequEncrypt(sour: Pointer; Size: NativeInt; Encrypt, ProcessTail: Boolean): Boolean;
begin
  Result := SequEncrypt(DefaultCipherStyle, sour, Size, DefaultKey, Encrypt, ProcessTail);
end;

function SequEncryptCBCWithDirect(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TCipherKeyBuffer;
begin
  TCipher.GenerateKey(cs, @key[0], length(key), k);
  Result := TCipher.EncryptBufferCBC(cs, sour, Size, @k, Encrypt, ProcessTail);
end;

function SequEncryptCBCWithDirect(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Encrypt then
    begin
      for i := low(ca) to high(ca) do
          Result := Result and SequEncryptCBCWithDirect(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end
  else
    begin
      for i := high(ca) downto low(ca) do
          Result := Result and SequEncryptCBCWithDirect(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end;
end;

{$IFDEF parallel}


function SequEncryptCBCWithParallel(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TCipherKeyBuffer;
  Parallel: TParallelCipher;
begin
  TCipher.GenerateKey(cs, @key[0], length(key), k);

  Parallel := TParallelCipher.Create;
  Parallel.ParallelDepth := DefaultParallelDepth;
  Result := Parallel.EncryptBufferCBC(cs, sour, Size, @k, Encrypt, ProcessTail);
  DisposeObject(Parallel);
end;

function SequEncryptCBCWithParallel(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Encrypt then
    begin
      for i := low(ca) to high(ca) do
          Result := Result and SequEncryptCBCWithParallel(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end
  else
    begin
      for i := high(ca) downto low(ca) do
          Result := Result and SequEncryptCBCWithParallel(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end;
end;
{$ENDIF}


function SequEncryptCBC(const ca: TCipherStyleArray; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
begin
{$IFDEF parallel}
  if Size > 1024 then
      Result := SequEncryptCBCWithParallel(ca, sour, Size, key, Encrypt, ProcessTail)
  else
{$ENDIF}
      Result := SequEncryptCBCWithDirect(ca, sour, Size, key, Encrypt, ProcessTail);
end;

function SequEncryptCBC(const cs: TCipherStyle; sour: Pointer; Size: NativeInt; var key: TBytes; Encrypt, ProcessTail: Boolean): Boolean;
begin
{$IFDEF parallel}
  if Size > 1024 then
      Result := SequEncryptCBCWithParallel(cs, sour, Size, key, Encrypt, ProcessTail)
  else
{$ENDIF}
      Result := SequEncryptCBCWithDirect(cs, sour, Size, key, Encrypt, ProcessTail);
end;

function GenerateSequHash(hssArry: THashStyles; sour: Pointer; Size: NativeInt): TPascalString;
var
  h: THashStyle;
  vl: THashStringList;
  hBuff: TBytes;
  n: SystemString;
begin
  vl := THashStringList.Create;
  for h in hssArry do
    begin
      TCipher.GenerateHashByte(h, sour, Size, hBuff);
      n := '';
      TCipher.HashToString(hBuff, n);
      vl[TCipher.CHashName[h]] := '(' + n + ')';
    end;

  Result.Text := vl.AsText;

  DisposeObject([vl]);
end;

procedure GenerateSequHash(hssArry: THashStyles; sour: Pointer; Size: NativeInt; output: TListPascalString);
var
  h: THashStyle;
  vl: THashStringList;
  hBuff: TBytes;
  n: SystemString;
begin
  vl := THashStringList.Create;
  for h in hssArry do
    begin
      TCipher.GenerateHashByte(h, sour, Size, hBuff);
      n := '';
      TCipher.HashToString(hBuff, n);
      vl[TCipher.CHashName[h]] := '(' + n + ')';
    end;

  vl.ExportAsStrings(output);

  DisposeObject([vl]);
end;

procedure GenerateSequHash(hssArry: THashStyles; sour: Pointer; Size: NativeInt; output: TCoreClassStream);
var
  h: THashStyle;
  vl: THashStringList;
  hBuff: TBytes;
  n: SystemString;
begin
  vl := THashStringList.Create;
  for h in hssArry do
    begin
      TCipher.GenerateHashByte(h, sour, Size, hBuff);
      n := '';
      TCipher.HashToString(hBuff, n);
      vl[TCipher.CHashName[h]] := '(' + n + ')';
    end;

  vl.SaveToStream(output);

  DisposeObject([vl]);
end;

function CompareSequHash(HashVL: THashStringList; sour: Pointer; Size: NativeInt): Boolean;
var
  ns: TListString;
  i: Integer;
  sourHash, destHash: TBytes;
  hName: SystemString;
  hValue: TPascalString;
  h: THashStyle;
begin
  Result := True;

  ns := TListString.Create;
  HashVL.GetNameList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      hName := ns[i];
      hValue := umlTrimSpace(HashVL.GetDefaultValue(hName, ''));
      if TCipher.NameToHashStyle(hName, h) and ((hValue.Len >= 2) and (hValue.First = '(') and (hValue.Last = ')')) then
        begin
          hValue.DeleteFirst;
          hValue.DeleteLast;

          TCipher.GenerateHashByte(h, sour, Size, sourHash);
          SetLength(destHash, length(sourHash));
          if length(destHash) > 0 then
            begin
              Result := Result and TCipher.HexToBuffer(hValue, destHash[0], length(destHash));
              Result := Result and TCipher.CompareHash(sourHash, destHash);
            end;
        end;
      if not Result then
          Break;
    end;

  DisposeObject([ns]);
end;

function CompareSequHash(hashData: TPascalString; sour: Pointer; Size: NativeInt): Boolean;
var
  vl: THashStringList;
begin
  vl := THashStringList.Create;
  vl.AsText := hashData.Text;
  Result := CompareSequHash(vl, sour, Size);
  DisposeObject(vl);
end;

function CompareSequHash(hashData: TListPascalString; sour: Pointer; Size: NativeInt): Boolean;
var
  vl: THashStringList;
begin
  vl := THashStringList.Create;
  vl.ImportFromStrings(hashData);
  Result := CompareSequHash(vl, sour, Size);
  DisposeObject(vl);
end;

function CompareSequHash(hashData: TCoreClassStream; sour: Pointer; Size: NativeInt): Boolean;
var
  vl: THashStringList;
begin
  vl := THashStringList.Create;
  vl.LoadFromStream(hashData);
  Result := CompareSequHash(vl, sour, Size);
  DisposeObject(vl);
end;

function GeneratePasswordHash(hssArry: THashStyles; passwd: TPascalString): TPascalString;
var
  buff: TBytes;
  m64: TMemoryStream64;
begin
  buff := passwd.Bytes;
  m64 := TMemoryStream64.Create;
  GenerateSequHash(hssArry, @buff[0], length(buff), m64);
  umlEncodeStreamBASE64(m64, Result);
  DisposeObject(m64);
  Result := '(' + Result + ')';
end;

function ComparePasswordHash(passwd, hashBuff: TPascalString): Boolean;
var
  buff: TBytes;
  m64: TMemoryStream64;
begin
  Result := False;
  hashBuff := umlTrimSpace(hashBuff);
  if (hashBuff.Len > 2) and (hashBuff.First = '(') and (hashBuff.Last = ')') then
    begin
      hashBuff.DeleteFirst;
      hashBuff.DeleteLast;
      buff := passwd.Bytes;
      m64 := TMemoryStream64.Create;
      umlDecodeStreamBASE64(hashBuff, m64);
      Result := CompareSequHash(m64, @buff[0], length(buff));
      DisposeObject(m64);
    end;
end;

function GeneratePassword(const ca: TCipherStyleArray; passwd: TPascalString): TPascalString;
var
  KeyBuff: TBytes;
  buff: TBytes;
begin
  KeyBuff := passwd.Bytes;
  buff := passwd.Bytes;
  SequEncryptCBC(ca, @buff[0], length(buff), KeyBuff, True, False);
  umlBase64EncodeBytes(buff, Result);
  Result := '(' + Result + ')';
end;

function ComparePassword(const ca: TCipherStyleArray; passwd, passwdDataSource: TPascalString): Boolean;
var
  refBuff: TBytes;
  KeyBuff: TBytes;
begin
  Result := False;
  passwdDataSource := umlTrimSpace(passwdDataSource);
  if (passwdDataSource.Len > 2) and (passwdDataSource.First = '(') and (passwdDataSource.Last = ')') then
    begin
      passwdDataSource.DeleteFirst;
      passwdDataSource.DeleteLast;
      umlBase64DecodeBytes(passwdDataSource, refBuff);
      KeyBuff := passwd.Bytes;
      SequEncryptCBC(ca, @refBuff[0], length(refBuff), KeyBuff, False, False);
      Result := TCipher.CompareKey(refBuff, KeyBuff);
    end;
end;

function GeneratePassword(const cs: TCipherStyle; passwd: TPascalString): TPascalString; overload;
var
  KeyBuff: TBytes;
  buff: TBytes;
begin
  KeyBuff := passwd.Bytes;
  buff := passwd.Bytes;
  SequEncryptCBC(cs, @buff[0], length(buff), KeyBuff, True, False);
  umlBase64EncodeBytes(buff, Result);
  Result := '(' + Result + ')';
end;

function ComparePassword(const cs: TCipherStyle; passwd, passwdDataSource: TPascalString): Boolean; overload;
var
  refBuff: TBytes;
  KeyBuff: TBytes;
begin
  Result := False;
  passwdDataSource := umlTrimSpace(passwdDataSource);
  if (passwdDataSource.Len > 2) and (passwdDataSource.First = '(') and (passwdDataSource.Last = ')') then
    begin
      passwdDataSource.DeleteFirst;
      passwdDataSource.DeleteLast;
      umlBase64DecodeBytes(passwdDataSource, refBuff);
      KeyBuff := passwd.Bytes;
      SequEncryptCBC(cs, @refBuff[0], length(refBuff), KeyBuff, False, False);
      Result := TCipher.CompareKey(refBuff, KeyBuff);
    end;
end;

procedure TestCoreCipher;
var
  buffer: TBytes;
  sour, dest: TMemoryStream64;
  k: TCipherKeyBuffer;
  cs: TCipherStyle;
  sourHash: TSHA1Digest;
  d: Cardinal;

  hs: THashStyle;
  hByte: TBytes;

{$IFDEF parallel}
  Parallel: TParallelCipher;
{$ENDIF}
  ps: TListPascalString;

  s: TPascalString;
begin
  sour := TMemoryStream64.Create;
  sour.Size := Int64(1024 * 1024 + 9);

  FillPtrByte(sour.Memory, sour.Size, $7F);
  DoStatus(umlStreamMD5String(sour).Text);
  DoStatus(umlMD5String(sour.Memory, sour.Size).Text);

  DisposeObject(sour);

  IDEOutput := True;

  DoStatus('Generate and verify password test');

  DoStatus('verify short password');
  s := GeneratePasswordHash(TCipher.CAllHash, '1');
  if not ComparePasswordHash('1', s) then
      DoStatus('PasswordHash failed!');
  if ComparePasswordHash('11', s) then
      DoStatus('PasswordHash failed!');

  DoStatus('verify long password');
  s := GeneratePasswordHash(TCipher.CAllHash, 'hello world 123456');
  if not ComparePasswordHash('hello world 123456', s) then
      DoStatus('PasswordHash failed!');
  if ComparePasswordHash('111 hello world 123456', s) then
      DoStatus('PasswordHash failed!');

  DoStatus('verify full chiher style password');
  s := GeneratePassword(TCipher.AllCipher, 'hello world');
  if not ComparePassword(TCipher.AllCipher, 'hello world', s) then
      DoStatus('Password cipher test failed! cipher: %s', ['']);
  if ComparePassword(TCipher.AllCipher, 'hello_world', s) then
      DoStatus('Password cipher test failed! cipher: %s', ['']);

  for cs in TCipher.AllCipher do
    begin
      DoStatus('verify %s chiher style password', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs))]);
      s := GeneratePassword(TCipher.AllCipher, 'hello world');
      if not ComparePassword(TCipher.AllCipher, 'hello world', s) then
          DoStatus('Password cipher test failed! cipher: %s', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs))]);
      if ComparePassword(TCipher.AllCipher, 'hello_world', s) then
          DoStatus('Password cipher test failed! cipher: %s', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs))]);
    end;

  // hash and Sequence Encrypt
  SetLength(buffer, 1024 * 1024);
  FillPtrByte(@buffer[0], length(buffer), 99);

  ps := TListPascalString.Create;

  DoStatus('Generate Sequence Hash');
  GenerateSequHash(TCipher.CAllHash, @buffer[0], length(buffer), ps);
  // DoStatus(ps.Text);

  if not CompareSequHash(ps, @buffer[0], length(buffer)) then
      DoStatus('hash compare failed!');

  DoStatus('test Sequence Encrypt');
  k := TPascalString('hello world').Bytes;
  if not SequEncryptWithDirect(TCipher.AllCipher, @buffer[0], length(buffer), k, True, True) then
      DoStatus('SequEncrypt failed!');
  if not SequEncryptWithDirect(TCipher.AllCipher, @buffer[0], length(buffer), k, False, True) then
      DoStatus('SequEncrypt failed!');

  DoStatus('verify Sequence Encrypt');
  if not CompareSequHash(ps, @buffer[0], length(buffer)) then
      DoStatus('hash compare failed!');

  // cipher Encrypt performance
  SetLength(buffer, 1024 * 1024 * 1 + 99);
  FillPtrByte(@buffer[0], length(buffer), $7F);

  sour := TMemoryStream64.Create;
  dest := TMemoryStream64.Create;
  sour.write(buffer[0], high(buffer));

  dest.Clear;
  sour.Position := 0;
  dest.CopyFrom(sour, sour.Size);
  sour.Position := 0;
  dest.Position := 0;

  sourHash := TCipher.GenerateSha1Hash(sour.Memory, sour.Size);

{$IFDEF parallel}
  DoStatus(#13#10'Parallel cipher performance test');

  for cs in TCipher.AllCipher do
    begin
      TCipher.GenerateKey(cs, 'hello world', k);
      Parallel := TParallelCipher.Create;

      dest.Clear;
      sour.Position := 0;
      dest.CopyFrom(sour, sour.Size);
      sour.Position := 0;
      dest.Position := 0;

      d := GetTimeTick;

      if not Parallel.EncryptBufferCBC(cs, dest.Memory, dest.Size, @k, True, True) then
          DoStatus('%s: parallel encode failed', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs))]);
      if not Parallel.EncryptBufferCBC(cs, dest.Memory, dest.Size, @k, False, True) then
          DoStatus('%s: parallel decode failed', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs))]);
      DoStatus('%s - parallel performance:%dms', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs)), GetTimeTick - d]);

      if not TCipher.CompareHash(TCipher.GenerateSha1Hash(dest.Memory, dest.Size), sourHash) then
          DoStatus('%s parallel hash error!', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs))]);

      DisposeObject(Parallel);
    end;
{$ENDIF}
  DoStatus(#13#10'normal cipher performance test');

  for cs in TCipher.AllCipher do
    begin
      TCipher.GenerateKey(cs, 'hello world', k);

      dest.Clear;
      sour.Position := 0;
      dest.CopyFrom(sour, sour.Size);
      sour.Position := 0;
      dest.Position := 0;

      d := GetTimeTick;
      if not TCipher.EncryptBufferCBC(cs, dest.Memory, dest.Size, @k, True, True) then
          DoStatus('%s: encode failed', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs))]);
      if not TCipher.EncryptBufferCBC(cs, dest.Memory, dest.Size, @k, False, True) then
          DoStatus('%s: decode failed', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs))]);
      DoStatus('%s - normal performance:%dms', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs)), GetTimeTick - d]);
      if not TCipher.CompareHash(TCipher.GenerateSha1Hash(dest.Memory, dest.Size), sourHash) then
          DoStatus('%s hash error!', [GetEnumName(TypeInfo(TCipherStyle), Integer(cs))]);
    end;

  // hash performance
  DoStatus(#13#10'hash performance test');
  dest.Clear;
  sour.Position := 0;
  dest.CopyFrom(sour, sour.Size);
  sour.Position := 0;
  dest.Position := 0;

  for hs := low(THashStyle) to high(THashStyle) do
    begin
      d := GetTimeTick;
      TCipher.GenerateHashByte(hs, dest.Memory, dest.Size, hByte);
      DoStatus('%s - performance:%dms', [GetEnumName(TypeInfo(THashStyle), Integer(hs)), Round((GetTimeTick - d))]);
    end;

  DoStatus(#13#10'all test done!');
  DisposeObject([ps, sour, dest]);
end;

{ TBlowfish }

class procedure TBlowfish.EncryptBF(const Context: TBFContext; var Block: TBFBlock; Encrypt: Boolean);
var
  i: Integer;
  TmpBlock: TBFBlockEx; { !!.01 }
begin
  CopyPtr(@Block, @TmpBlock, SizeOf(TmpBlock)); { !!.01 }
  if Encrypt then begin
      Block[0] := Block[0] xor Context.PBox[0];

      { 16 Rounds to go (8 double rounds to avoid swaps) }
      i := 1;
      repeat
        { first half round }
        Block[1] := Block[1] xor Context.PBox[i] xor (((
          Context.SBox[0, TmpBlock.Xl[3]] + Context.SBox[1, TmpBlock.Xl[2]])
          xor Context.SBox[2, TmpBlock.Xl[1]]) + Context.SBox[3, TmpBlock.Xl[0]]);
        { second half round }
        Block[0] := Block[0] xor Context.PBox[i + 1] xor (((
          Context.SBox[0, TmpBlock.Xr[3]] + Context.SBox[1, TmpBlock.Xr[2]])
          xor Context.SBox[2, TmpBlock.Xr[1]]) + Context.SBox[3, TmpBlock.Xr[0]]);
        inc(i, 2);
      until i > BFRounds;
      Block[1] := Block[1] xor Context.PBox[(BFRounds + 1)];
    end
  else begin
      Block[1] := Block[1] xor Context.PBox[(BFRounds + 1)];

      { 16 Rounds to go (8 double rounds to avoid swaps) }
      i := BFRounds;
      repeat
        { first half round }
        Block[0] := Block[0] xor Context.PBox[i] xor (((
          Context.SBox[0, TmpBlock.Xr[3]] + Context.SBox[1, TmpBlock.Xr[2]])
          xor Context.SBox[2, TmpBlock.Xr[1]]) + Context.SBox[3, TmpBlock.Xr[0]]);
        { second half round }
        Block[1] := Block[1] xor Context.PBox[i - 1] xor (((
          Context.SBox[0, TmpBlock.Xl[3]] + Context.SBox[1, TmpBlock.Xl[2]])
          xor Context.SBox[2, TmpBlock.Xl[1]]) + Context.SBox[3, TmpBlock.Xl[0]]);
        dec(i, 2);
      until i < 1;
      Block[0] := Block[0] xor Context.PBox[0];
    end;
end;

class procedure TBlowfish.InitEncryptBF(key: TKey128; var Context: TBFContext);
var
  i: Integer;
  j: Integer;
  k: Integer;
  Data: DWORD;
  Block: TBFBlock;
begin
  { initialize PArray }
  CopyPtr(@bf_P, @Context.PBox, SizeOf(Context.PBox));
  { initialize SBox }
  CopyPtr(@bf_S, @Context.SBox, SizeOf(Context.SBox));

  { update PArray with the key bits }
  j := 0;
  for i := 0 to (BFRounds + 1) do begin
      Data := 0;
      for k := 0 to 3 do begin
          Data := (Data shl 8) or key[j];
          inc(j);
          if j >= SizeOf(key) then
              j := 0;
        end;
      Context.PBox[i] := Context.PBox[i] xor Data;
    end;

  { encrypt an all-zero SystemString using the Blowfish algorithm and }
  { replace the elements of the P-array with the output of this process }

  Block[0] := 0;
  Block[1] := 0;
  i := 0;
  repeat
    EncryptBF(Context, Block, True);
    Context.PBox[i] := Block[0];
    Context.PBox[i + 1] := Block[1];
    inc(i, 2);
  until i > BFRounds + 1;

  { continue the process, replacing the elements of the four S-boxes in }
  { order, with the output of the continuously changing Blowfish algorithm }

  for j := 0 to 3 do begin
      i := 0;
      repeat
        EncryptBF(Context, Block, True);
        Context.SBox[j, i] := Block[0];
        Context.SBox[j, i + 1] := Block[1];
        inc(i, 2);
      until i > 255;
    end;

  { in total, 521 iterations are required to generate all required subkeys. }
end;

{ TDES }

class procedure TDES.EncryptDES(const Context: TDESContext; var Block: TDESBlock);
const
  SPBox: array [0 .. 7, 0 .. 63] of DWORD =
    (($01010400, $00000000, $00010000, $01010404, $01010004, $00010404, $00000004, $00010000,
    $00000400, $01010400, $01010404, $00000400, $01000404, $01010004, $01000000, $00000004,
    $00000404, $01000400, $01000400, $00010400, $00010400, $01010000, $01010000, $01000404,
    $00010004, $01000004, $01000004, $00010004, $00000000, $00000404, $00010404, $01000000,
    $00010000, $01010404, $00000004, $01010000, $01010400, $01000000, $01000000, $00000400,
    $01010004, $00010000, $00010400, $01000004, $00000400, $00000004, $01000404, $00010404,
    $01010404, $00010004, $01010000, $01000404, $01000004, $00000404, $00010404, $01010400,
    $00000404, $01000400, $01000400, $00000000, $00010004, $00010400, $00000000, $01010004),
    ($80108020, $80008000, $00008000, $00108020, $00100000, $00000020, $80100020, $80008020,
    $80000020, $80108020, $80108000, $80000000, $80008000, $00100000, $00000020, $80100020,
    $00108000, $00100020, $80008020, $00000000, $80000000, $00008000, $00108020, $80100000,
    $00100020, $80000020, $00000000, $00108000, $00008020, $80108000, $80100000, $00008020,
    $00000000, $00108020, $80100020, $00100000, $80008020, $80100000, $80108000, $00008000,
    $80100000, $80008000, $00000020, $80108020, $00108020, $00000020, $00008000, $80000000,
    $00008020, $80108000, $00100000, $80000020, $00100020, $80008020, $80000020, $00100020,
    $00108000, $00000000, $80008000, $00008020, $80000000, $80100020, $80108020, $00108000),
    ($00000208, $08020200, $00000000, $08020008, $08000200, $00000000, $00020208, $08000200,
    $00020008, $08000008, $08000008, $00020000, $08020208, $00020008, $08020000, $00000208,
    $08000000, $00000008, $08020200, $00000200, $00020200, $08020000, $08020008, $00020208,
    $08000208, $00020200, $00020000, $08000208, $00000008, $08020208, $00000200, $08000000,
    $08020200, $08000000, $00020008, $00000208, $00020000, $08020200, $08000200, $00000000,
    $00000200, $00020008, $08020208, $08000200, $08000008, $00000200, $00000000, $08020008,
    $08000208, $00020000, $08000000, $08020208, $00000008, $00020208, $00020200, $08000008,
    $08020000, $08000208, $00000208, $08020000, $00020208, $00000008, $08020008, $00020200),
    ($00802001, $00002081, $00002081, $00000080, $00802080, $00800081, $00800001, $00002001,
    $00000000, $00802000, $00802000, $00802081, $00000081, $00000000, $00800080, $00800001,
    $00000001, $00002000, $00800000, $00802001, $00000080, $00800000, $00002001, $00002080,
    $00800081, $00000001, $00002080, $00800080, $00002000, $00802080, $00802081, $00000081,
    $00800080, $00800001, $00802000, $00802081, $00000081, $00000000, $00000000, $00802000,
    $00002080, $00800080, $00800081, $00000001, $00802001, $00002081, $00002081, $00000080,
    $00802081, $00000081, $00000001, $00002000, $00800001, $00002001, $00802080, $00800081,
    $00002001, $00002080, $00800000, $00802001, $00000080, $00800000, $00002000, $00802080),
    ($00000100, $02080100, $02080000, $42000100, $00080000, $00000100, $40000000, $02080000,
    $40080100, $00080000, $02000100, $40080100, $42000100, $42080000, $00080100, $40000000,
    $02000000, $40080000, $40080000, $00000000, $40000100, $42080100, $42080100, $02000100,
    $42080000, $40000100, $00000000, $42000000, $02080100, $02000000, $42000000, $00080100,
    $00080000, $42000100, $00000100, $02000000, $40000000, $02080000, $42000100, $40080100,
    $02000100, $40000000, $42080000, $02080100, $40080100, $00000100, $02000000, $42080000,
    $42080100, $00080100, $42000000, $42080100, $02080000, $00000000, $40080000, $42000000,
    $00080100, $02000100, $40000100, $00080000, $00000000, $40080000, $02080100, $40000100),
    ($20000010, $20400000, $00004000, $20404010, $20400000, $00000010, $20404010, $00400000,
    $20004000, $00404010, $00400000, $20000010, $00400010, $20004000, $20000000, $00004010,
    $00000000, $00400010, $20004010, $00004000, $00404000, $20004010, $00000010, $20400010,
    $20400010, $00000000, $00404010, $20404000, $00004010, $00404000, $20404000, $20000000,
    $20004000, $00000010, $20400010, $00404000, $20404010, $00400000, $00004010, $20000010,
    $00400000, $20004000, $20000000, $00004010, $20000010, $20404010, $00404000, $20400000,
    $00404010, $20404000, $00000000, $20400010, $00000010, $00004000, $20400000, $00404010,
    $00004000, $00400010, $20004010, $00000000, $20404000, $20000000, $00400010, $20004010),
    ($00200000, $04200002, $04000802, $00000000, $00000800, $04000802, $00200802, $04200800,
    $04200802, $00200000, $00000000, $04000002, $00000002, $04000000, $04200002, $00000802,
    $04000800, $00200802, $00200002, $04000800, $04000002, $04200000, $04200800, $00200002,
    $04200000, $00000800, $00000802, $04200802, $00200800, $00000002, $04000000, $00200800,
    $04000000, $00200800, $00200000, $04000802, $04000802, $04200002, $04200002, $00000002,
    $00200002, $04000000, $04000800, $00200000, $04200800, $00000802, $00200802, $04200800,
    $00000802, $04000002, $04200802, $04200000, $00200800, $00000000, $00000002, $04200802,
    $00000000, $00200802, $04200000, $00000800, $04000002, $04000800, $00000800, $00200002),
    ($10001040, $00001000, $00040000, $10041040, $10000000, $10001040, $00000040, $10000000,
    $00040040, $10040000, $10041040, $00041000, $10041000, $00041040, $00001000, $00000040,
    $10040000, $10000040, $10001000, $00001040, $00041000, $00040040, $10040040, $10041000,
    $00001040, $00000000, $00000000, $10040040, $10000040, $10001000, $00041040, $00040000,
    $00041040, $00040000, $10041000, $00001000, $00000040, $10040040, $00001000, $00041040,
    $10001000, $00000040, $10000040, $10040000, $10040040, $10000000, $00040000, $10001040,
    $00000000, $10041040, $00040040, $10000040, $10040000, $10001000, $10001040, $00000000,
    $10041040, $00041000, $00041000, $00001040, $00001040, $00040040, $10000000, $10041000));
var
  i, L, r, Work: DWORD;
  CPtr: PDWORD;

  procedure IPerm(var L, r: DWORD);
  var
    Work: DWORD;
  begin
    Work := ((L shr 4) xor r) and $0F0F0F0F;
    r := r xor Work;
    L := L xor Work shl 4;

    Work := ((L shr 16) xor r) and $0000FFFF;
    r := r xor Work;
    L := L xor Work shl 16;

    Work := ((r shr 2) xor L) and $33333333;
    L := L xor Work;
    r := r xor Work shl 2;

    Work := ((r shr 8) xor L) and $00FF00FF;
    L := L xor Work;
    r := r xor Work shl 8;

    r := (r shl 1) or (r shr 31);
    Work := (L xor r) and $AAAAAAAA;
    L := L xor Work;
    r := r xor Work;
    L := (L shl 1) or (L shr 31);
  end;

  procedure FPerm(var L, r: DWORD);
  var
    Work: DWORD;
  begin
    L := L;

    r := (r shl 31) or (r shr 1);
    Work := (L xor r) and $AAAAAAAA;
    L := L xor Work;
    r := r xor Work;
    L := (L shr 1) or (L shl 31);

    Work := ((L shr 8) xor r) and $00FF00FF;
    r := r xor Work;
    L := L xor Work shl 8;

    Work := ((L shr 2) xor r) and $33333333;
    r := r xor Work;
    L := L xor Work shl 2;

    Work := ((r shr 16) xor L) and $0000FFFF;
    L := L xor Work;
    r := r xor Work shl 16;

    Work := ((r shr 4) xor L) and $0F0F0F0F;
    L := L xor Work;
    r := r xor Work shl 4;
  end;

begin
  SplitBlock(Block, L, r);
  IPerm(L, r);

  CPtr := @Context;
  for i := 0 to 7 do begin
      Work := (((r shr 4) or (r shl 28)) xor CPtr^);
      inc(CPtr);
      L := L xor SPBox[6, Work and $3F];
      L := L xor SPBox[4, Work shr 8 and $3F];
      L := L xor SPBox[2, Work shr 16 and $3F];
      L := L xor SPBox[0, Work shr 24 and $3F];

      Work := (r xor CPtr^);
      inc(CPtr);
      L := L xor SPBox[7, Work and $3F];
      L := L xor SPBox[5, Work shr 8 and $3F];
      L := L xor SPBox[3, Work shr 16 and $3F];
      L := L xor SPBox[1, Work shr 24 and $3F];

      Work := (((L shr 4) or (L shl 28)) xor CPtr^);
      inc(CPtr);
      r := r xor SPBox[6, Work and $3F];
      r := r xor SPBox[4, Work shr 8 and $3F];
      r := r xor SPBox[2, Work shr 16 and $3F];
      r := r xor SPBox[0, Work shr 24 and $3F];

      Work := (L xor CPtr^);
      inc(CPtr);
      r := r xor SPBox[7, Work and $3F];
      r := r xor SPBox[5, Work shr 8 and $3F];
      r := r xor SPBox[3, Work shr 16 and $3F];
      r := r xor SPBox[1, Work shr 24 and $3F];
    end;

  FPerm(L, r);
  JoinBlock(L, r, Block);
end;

class procedure TDES.EncryptTripleDES(const Context: TTripleDESContext; var Block: TDESBlock);
begin
  EncryptDES(Context[0], Block);
  EncryptDES(Context[1], Block);
  EncryptDES(Context[0], Block);
end;

{ !!.01 }
class procedure TDES.EncryptTripleDES3Key(const Context: TTripleDESContext3Key; var Block: TDESBlock);
begin
  EncryptDES(Context[2], Block);
  EncryptDES(Context[1], Block);
  EncryptDES(Context[0], Block);
end;

class procedure TDES.InitEncryptDES(const key: TKey64; var Context: TDESContext; Encrypt: Boolean);
const
  PC1: array [0 .. 55] of Byte =
    (56, 48, 40, 32, 24, 16, 8, 0, 57, 49, 41, 33, 25, 17, 9, 1, 58, 50, 42, 34, 26,
    18, 10, 2, 59, 51, 43, 35, 62, 54, 46, 38, 30, 22, 14, 6, 61, 53, 45, 37, 29, 21,
    13, 5, 60, 52, 44, 36, 28, 20, 12, 4, 27, 19, 11, 3);
  PC2: array [0 .. 47] of Byte =
    (13, 16, 10, 23, 0, 4, 2, 27, 14, 5, 20, 9, 22, 18, 11, 3, 25, 7,
    15, 6, 26, 19, 12, 1, 40, 51, 30, 36, 46, 54, 29, 39, 50, 44, 32, 47,
    43, 48, 38, 55, 33, 52, 45, 41, 49, 35, 28, 31);
  CTotRot: array [0 .. 15] of Byte = (1, 2, 4, 6, 8, 10, 12, 14, 15, 17, 19, 21, 23, 25, 27, 28);
  CBitMask: array [0 .. 7] of Byte = (128, 64, 32, 16, 8, 4, 2, 1);
var
  PC1M: array [0 .. 55] of Byte;
  PC1R: array [0 .. 55] of Byte;
  KS: array [0 .. 7] of Byte;
  i, j, L, M: Integer;
begin
  { convert PC1 to bits of key }
  for j := 0 to 55 do begin
      L := PC1[j];
      M := L mod 8;
      PC1M[j] := Ord((key[L div 8] and CBitMask[M]) <> 0);
    end;

  { key chunk for each iteration }
  for i := 0 to 15 do begin
      { rotate PC1 the right amount }
      for j := 0 to 27 do begin
          L := j + CTotRot[i];
          if (L < 28) then begin
              PC1R[j] := PC1M[L];
              PC1R[j + 28] := PC1M[L + 28];
            end
          else begin
              PC1R[j] := PC1M[L - 28];
              PC1R[j + 28] := PC1M[L];
            end;
        end;

      { select bits individually }
      FillPtrByte(@KS, SizeOf(KS), 0);
      for j := 0 to 47 do
        if Boolean(PC1R[PC2[j]]) then begin
            L := j div 6;
            KS[L] := KS[L] or CBitMask[j mod 6] shr 2;
          end;

      { now convert to odd/even interleaved form for use in F }
      if Encrypt then begin
          Context.TransformedKey[i * 2] := (Integer(KS[0]) shl 24) or (Integer(KS[2]) shl 16) or
            (Integer(KS[4]) shl 8) or (Integer(KS[6]));
          Context.TransformedKey[i * 2 + 1] := (Integer(KS[1]) shl 24) or (Integer(KS[3]) shl 16) or
            (Integer(KS[5]) shl 8) or (Integer(KS[7]));
        end
      else begin
          Context.TransformedKey[31 - (i * 2 + 1)] := (Integer(KS[0]) shl 24) or (Integer(KS[2]) shl 16) or
            (Integer(KS[4]) shl 8) or (Integer(KS[6]));
          Context.TransformedKey[31 - (i * 2)] := (Integer(KS[1]) shl 24) or (Integer(KS[3]) shl 16) or
            (Integer(KS[5]) shl 8) or (Integer(KS[7]));
        end;
    end;

  Context.Encrypt := Encrypt;
end;

class procedure TDES.InitEncryptTripleDES(const key: TKey128; var Context: TTripleDESContext; Encrypt: Boolean);
var
  KeyArray: array [0 .. 1] of TKey64;
begin
  CopyPtr(@key, @KeyArray, SizeOf(KeyArray)); { !!.01 }
  if Encrypt then begin
      InitEncryptDES(KeyArray[0], Context[0], True);
      InitEncryptDES(KeyArray[1], Context[1], False);
    end
  else begin
      InitEncryptDES(KeyArray[0], Context[0], False);
      InitEncryptDES(KeyArray[1], Context[1], True);
    end;
end;

{ !!.01 }
class procedure TDES.InitEncryptTripleDES3Key(const Key1, Key2, Key3: TKey64; var Context: TTripleDESContext3Key; Encrypt: Boolean);
begin
  if Encrypt then begin
      InitEncryptDES(Key1, Context[0], True);
      InitEncryptDES(Key2, Context[1], False);
      InitEncryptDES(Key3, Context[2], True);
    end
  else begin
      InitEncryptDES(Key1, Context[2], False);
      InitEncryptDES(Key2, Context[1], True);
      InitEncryptDES(Key3, Context[0], False);
    end;
end;

class procedure TDES.JoinBlock(const L, r: DWORD; var Block: TDESBlock);
var
  Temp: TDesConverter;
  i: Integer;
begin
  Temp.DWords[0] := DWORD(L);
  Temp.DWords[1] := DWORD(r);
  for i := low(Block) to high(Block) do
      Block[i] := Temp.Bytes[7 - i];
end;

class procedure TDES.ShrinkDESKey(var key: TKey64);
const
  SK1: TKey64 = ($C4, $08, $B0, $54, $0B, $A1, $E0, $AE);
  SK2: TKey64 = ($EF, $2C, $04, $1C, $E6, $38, $2F, $E6);
var
  i: Integer;
  Work1: TKey64;
  Work2: TKey64;
  Context: TDESContext;
begin
  { step #1 zero the parity bits - 8, 16, 24, ..., 64 }
  for i := 0 to 7 do
      Work1[i] := key[i] and $FE;

  { step #2 encrypt output of #1 with SK1 and xor with output of #1 }
  InitEncryptDES(SK1, Context, True);
  Work2 := Work1; { make copy }
  EncryptDES(Context, TDESBlock(Work2));
  for i := 0 to 7 do
      Work1[i] := Work1[i] xor Work2[i];

  { step #3 zero bits 1,2,3,4,8,16,17,18,19,20,24,32,33,34,35,36,40,48,49,50,51,52,56,64 }
  TInt64(Work1).Lo := TInt64(Work1).Lo and $F101F101;
  TInt64(Work1).Hi := TInt64(Work1).Hi and $F101F101;

  { step #4 encrypt output of #3 with SK2 }
  InitEncryptDES(SK2, Context, True);
  EncryptDES(Context, TDESBlock(Work1));

  key := Work1;
end;

class procedure TDES.SplitBlock(const Block: TDESBlock; var L, r: DWORD);
var
  Temp: TDesConverter;
  i: Integer;
begin
  for i := low(Block) to high(Block) do
      Temp.Bytes[7 - i] := Block[i];
  L := Temp.DWords[1];
  r := Temp.DWords[0];
end;

{ TSHA1 }

class procedure TSHA1.FinalizeSHA1(var Context: TSHA1Context; var Digest: TSHA1Digest);
begin
  with Context do begin
      sdBuf[sdIndex] := $80;

      if sdIndex >= 56 then
          SHA1Hash(Context);

      PDWORD(@sdBuf[56])^ := SHA1SwapByteOrder(sdHi);
      PDWORD(@sdBuf[60])^ := SHA1SwapByteOrder(sdLo);

      SHA1Hash(Context);

      sdHash[0] := SHA1SwapByteOrder(sdHash[0]);
      sdHash[1] := SHA1SwapByteOrder(sdHash[1]);
      sdHash[2] := SHA1SwapByteOrder(sdHash[2]);
      sdHash[3] := SHA1SwapByteOrder(sdHash[3]);
      sdHash[4] := SHA1SwapByteOrder(sdHash[4]);

      CopyPtr(@sdHash, @Digest, SizeOf(Digest));
      SHA1Clear(Context);
    end;
end;

class procedure TSHA1.HashSHA1(var Digest: TSHA1Digest; const Buf; BufSize: Integer);
var
  Context: TSHA1Context;
begin
  InitSHA1(Context);
  UpdateSHA1(Context, Buf, BufSize);
  FinalizeSHA1(Context, Digest);
end;

class procedure TSHA1.InitSHA1(var Context: TSHA1Context);
begin
  SHA1Clear(Context);
  Context.sdHash[0] := SHA1_A;
  Context.sdHash[1] := SHA1_B;
  Context.sdHash[2] := SHA1_C;
  Context.sdHash[3] := SHA1_D;
  Context.sdHash[4] := SHA1_E;
end;

class procedure TSHA1.SHA1Clear(var Context: TSHA1Context);
begin
  FillPtrByte(@Context, SizeOf(Context), $00);
end;

class procedure TSHA1.SHA1Hash(var Context: TSHA1Context);
var
  a: DWORD;
  b: DWORD;
  c: DWORD;
  d: DWORD;
  E: DWORD;

  x: DWORD;
  w: array [0 .. 79] of DWORD;

  i: Integer;
begin
  with Context do begin
      sdIndex := 0;
      CopyPtr(@sdBuf, @w, SizeOf(w));

      // W := Mt, for t = 0 to 15 : Mt is M sub t
      for i := 0 to 15 do
          w[i] := SHA1SwapByteOrder(w[i]);

      // Transform Message block from 16 32 bit words to 80 32 bit words
      // Wt, = ( Wt-3 xor Wt-8 xor Wt-13 xor Wt-16 ) rolL 1 : Wt is W sub t
      for i := 16 to 79 do
          w[i] := TMISC.RolX(w[i - 3] xor w[i - 8] xor w[i - 14] xor w[i - 16], 1);

      a := sdHash[0];
      b := sdHash[1];
      c := sdHash[2];
      d := sdHash[3];
      E := sdHash[4];

      // the four rounds
      for i := 0 to 19 do begin
          x := TMISC.RolX(a, 5) + (d xor (b and (c xor d))) + E + w[i] + SHA1_K1;
          E := d;
          d := c;
          c := TMISC.RolX(b, 30);
          b := a;
          a := x;
        end;

      for i := 20 to 39 do begin
          x := TMISC.RolX(a, 5) + (b xor c xor d) + E + w[i] + SHA1_K2;
          E := d;
          d := c;
          c := TMISC.RolX(b, 30);
          b := a;
          a := x;
        end;

      for i := 40 to 59 do begin
          x := TMISC.RolX(a, 5) + ((b and c) or (d and (b or c))) + E + w[i] + SHA1_K3;
          E := d;
          d := c;
          c := TMISC.RolX(b, 30);
          b := a;
          a := x;
        end;

      for i := 60 to 79 do
        begin
          x := TMISC.RolX(a, 5) + (b xor c xor d) + E + w[i] + SHA1_K4;
          E := d;
          d := c;
          c := TMISC.RolX(b, 30);
          b := a;
          a := x;
        end;

      sdHash[0] := sdHash[0] + a;
      sdHash[1] := sdHash[1] + b;
      sdHash[2] := sdHash[2] + c;
      sdHash[3] := sdHash[3] + d;
      sdHash[4] := sdHash[4] + E;

      FillPtrByte(@w, SizeOf(w), $00);
      FillPtrByte(@sdBuf, SizeOf(sdBuf), $00);
    end;
end;

class function TSHA1.SHA1SwapByteOrder(n: DWORD): DWORD;
begin
  n := (n shr 24) or ((n shr 8) and LBMASK_LO) or ((n shl 8) and LBMASK_HI) or (n shl 24);
  Result := n;
end;

class procedure TSHA1.SHA1UpdateLen(var Context: TSHA1Context; Len: DWORD);
begin
  inc(Context.sdLo, (Len shl 3));
  if Context.sdLo < (Len shl 3) then
      inc(Context.sdHi);
  inc(Context.sdHi, Len shr 29);
end;

class procedure TSHA1.ByteBuffHashSHA1(var Digest: TSHA1Digest; const ABytes: TBytes);
begin
  HashSHA1(Digest, ABytes[0], length(ABytes));
end;

class procedure TSHA1.UpdateSHA1(var Context: TSHA1Context; const Buf; BufSize: Integer);
var
  PBuf: PByte;
begin
  with Context do begin
      SHA1UpdateLen(Context, BufSize);
      PBuf := @Buf;
      while BufSize > 0 do begin
          if (SizeOf(sdBuf) - sdIndex) <= DWORD(BufSize) then begin
              CopyPtr(PBuf, @sdBuf[sdIndex], SizeOf(sdBuf) - sdIndex);
              dec(BufSize, SizeOf(sdBuf) - sdIndex);
              inc(PBuf, SizeOf(sdBuf) - sdIndex);
              SHA1Hash(Context);
            end
          else begin
              CopyPtr(PBuf, @sdBuf[sdIndex], BufSize);
              inc(sdIndex, BufSize);
              BufSize := 0;
            end;
        end;
    end;
end;

{ TLBC }

class procedure TLBC.EncryptLBC(const Context: TLBCContext; var Block: TLBCBlock);
var
  Blocks: array [0 .. 1] of TBCHalfBlock; { !!.01 }
  Work: TBCHalfBlock;
  Right: TBCHalfBlock;
  Left: TBCHalfBlock;
  AA, BB: Integer;
  CC, DD: Integer;
  r, t: Integer;
begin
  CopyPtr(@Block, @Blocks, SizeOf(Blocks)); { !!.01 }
  Right := Blocks[0];
  Left := Blocks[1];

  for r := 0 to Context.Rounds - 1 do begin
      { transform the right side }
      AA := Right[0];
      BB := TBCHalfBlock(Context.SubKeys64[r])[0];
      CC := Right[1];
      DD := TBCHalfBlock(Context.SubKeys64[r])[1];

      { mix it once... }
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 7);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 13);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (CC shr 17);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 9);
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 3);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 7);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (DD shr 15);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 11);

      { swap sets... }
      t := AA;
      AA := CC;
      CC := t;
      t := BB;
      BB := DD;
      DD := t;

      { mix it twice }
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 7);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 13);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (CC shr 17);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 9);
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 3);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 7);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (DD shr 15);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 11);

      Work[0] := Left[0] xor AA xor BB;
      Work[1] := Left[1] xor CC xor DD;

      Left := Right;
      Right := Work;
    end;

  Blocks[0] := Left;
  Blocks[1] := Right;
  CopyPtr(@Blocks, @Block, SizeOf(Block)); { !!.01 }
end;

class procedure TLBC.EncryptLQC(const key: TKey128; var Block: TLQCBlock; Encrypt: Boolean);
const
  CKeyBox: array [False .. True, 0 .. 3, 0 .. 2] of Integer =
    (((0, 3, 1), (2, 1, 3), (1, 0, 2), (3, 2, 0)),
    ((3, 2, 0), (1, 0, 2), (2, 1, 3), (0, 3, 1)));
var
  KeyInts: array [0 .. 3] of Integer; { !!.01 }
  Blocks: array [0 .. 1] of Integer;  { !!.01 }
  Work: Integer;
  Right: Integer;
  Left: Integer;
  r: Integer;
  AA, BB: Integer;
  CC, DD: Integer;
begin
  CopyPtr(@key, @KeyInts, SizeOf(KeyInts)); { !!.01 }
  CopyPtr(@Block, @Blocks, SizeOf(Blocks)); { !!.01 }
  Right := Blocks[0];
  Left := Blocks[1];

  for r := 0 to 3 do begin
      { transform the right side }
      AA := Right;
      BB := KeyInts[CKeyBox[Encrypt, r, 0]];
      CC := KeyInts[CKeyBox[Encrypt, r, 1]];
      DD := KeyInts[CKeyBox[Encrypt, r, 2]];

      { commented code does not affect results - removed for speed }
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 7);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 13);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (CC shr 17);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 9);
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 3);
      BB := BB + AA; { AA := AA + BB; }
      BB := BB xor (BB shl 7);
      CC := CC + BB; { BB := BB + CC; }
      CC := CC xor (DD shr 15);
      DD := DD + CC; { CC := CC + DD; }
      DD := DD xor (DD shl 11);

      Work := Left xor DD;
      Left := Right;
      Right := Work;
    end;

  Blocks[0] := Left;
  Blocks[1] := Right;
  CopyPtr(@Blocks, @Block, SizeOf(Block)); { !!.01 }
end;

class procedure TLBC.InitEncryptLBC(const key: TKey128; var Context: TLBCContext; Rounds: Integer; Encrypt: Boolean);
type
  TDCPTFSubKeys = packed record
    case Byte of
      0: (SubKeys64: array [0 .. 15] of TKey64);
      1: (SubKeysInts: array [0 .. 3, 0 .. 7] of DWORD);
  end;
var
  KeyArray: PDWordArray;
  AA, BB: DWORD;
  CC, DD: DWORD;
  EE, FF: DWORD;
  GG, HH: DWORD;
  i, r: Integer;
  Temp: TDCPTFSubKeys;
begin
  KeyArray := @key;
  Context.Encrypt := Encrypt;
  Context.Rounds := Max(4, Min(16, Rounds));

  { fill subkeys by propagating seed }
  for i := 0 to 3 do begin
      { interleave the key with the salt }

      AA := KeyArray^[0];
      BB := BCSalts[i];
      CC := KeyArray^[1];
      DD := BCSalts[i];
      EE := KeyArray^[2];
      FF := BCSalts[i];
      GG := KeyArray^[3];
      HH := BCSalts[i];

      { mix all the bits around for 8 rounds }
      { achieves avalanche and eliminates funnels }
      for r := 0 to 7 do begin
          AA := AA xor (BB shl 11);
          DD := DD + AA;
          BB := BB + CC;
          BB := BB xor (CC shr 2);
          EE := EE + BB;
          CC := CC + DD;
          CC := CC xor (DD shl 8);
          FF := FF + CC;
          DD := DD + EE;
          DD := DD xor (EE shr 16);
          GG := GG + DD;
          EE := EE + FF;
          EE := EE xor (FF shl 10);
          HH := HH + EE;
          FF := FF + GG;
          FF := FF xor (GG shr 4);
          AA := AA + FF;
          GG := GG + HH;
          GG := GG xor (HH shl 8);
          BB := BB + GG;
          HH := HH + AA;
          HH := HH xor (AA shr 9);
          CC := CC + HH;
          AA := AA + BB;
        end;

      { assign value to subkey }
      Context.SubKeysInts[i, 0] := AA;
      Context.SubKeysInts[i, 1] := BB;
      Context.SubKeysInts[i, 2] := CC;
      Context.SubKeysInts[i, 3] := DD;
      Context.SubKeysInts[i, 4] := EE;
      Context.SubKeysInts[i, 5] := FF;
      Context.SubKeysInts[i, 6] := GG;
      Context.SubKeysInts[i, 7] := HH;
    end;

  { reverse subkeys if decrypting - easier for EncryptLBC routine }
  if not Encrypt then begin
      for i := 0 to Context.Rounds - 1 do
          Temp.SubKeys64[(Context.Rounds - 1) - i] := Context.SubKeys64[i];
      for i := 0 to Context.Rounds - 1 do
          Context.SubKeys64[i] := Temp.SubKeys64[i];
    end;
end;

{ TCipherMD5 }

class procedure TCipherMD5.FinalizeMD5(var Context: TMD5Context; var Digest: TMD5Digest);
const
  Padding: array [0 .. 63] of Byte = (
    $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);
var
  InBuf: TTransformInput;
  MDI: Integer;
  i: Word;
  II: Word;
  PadLen: Word;
begin
  { save number of bits }
  InBuf[14] := Context.Count[0];
  InBuf[15] := Context.Count[1];
  { compute number of bytes mod 64 }
  MDI := (Context.Count[0] shr 3) and $3F;
  { pad out to 56 mod 64 }
  if (MDI < 56) then
      PadLen := 56 - MDI
  else
      PadLen := 120 - MDI;
  UpdateMD5(Context, Padding, PadLen);

  CopyPtr(@Context, @Context, SizeOf(Context)); { !!.01 }

  { append length in bits and transform }
  II := 0;
  for i := 0 to 13 do begin
      InBuf[i] :=
        (Integer(Context.Buf[II + 3]) shl 24) or
        (Integer(Context.Buf[II + 2]) shl 16) or
        (Integer(Context.Buf[II + 1]) shl 8) or
        Integer(Context.Buf[II]);
      inc(II, 4);
    end;
  TMISC.Transform(Context.State, InBuf);
  { store buffer in digest }
  II := 0;
  for i := 0 to 3 do begin
      Digest[II] := Byte(Context.State[i] and $FF);
      Digest[II + 1] := Byte((Context.State[i] shr 8) and $FF);
      Digest[II + 2] := Byte((Context.State[i] shr 16) and $FF);
      Digest[II + 3] := Byte((Context.State[i] shr 24) and $FF);
      inc(II, 4);
    end;
end;

class procedure TCipherMD5.GenerateMD5Key(var key: TKey128; const ABytes: TBytes);
var
  d: TMD5Digest;
begin
  HashMD5(d, ABytes[0], length(ABytes));
end;

class procedure TCipherMD5.HashMD5(var Digest: TMD5Digest; const Buf; BufSize: NativeInt);
var
  Context: TMD5Context;
begin
  FillPtrByte(@Context, SizeOf(Context), $00);
  InitMD5(Context);
  UpdateMD5(Context, Buf, BufSize);
  FinalizeMD5(Context, Digest);
end;

class procedure TCipherMD5.InitMD5(var Context: TMD5Context);
begin
  Context.Count[0] := 0;
  Context.Count[1] := 0;

  { load magic initialization constants }
  Context.State[0] := $67452301;
  Context.State[1] := $EFCDAB89;
  Context.State[2] := $98BADCFE;
  Context.State[3] := $10325476;
end;

class procedure TCipherMD5.ByteBuffHashMD5(var Digest: TMD5Digest; const ABytes: TBytes);
begin
  HashMD5(Digest, ABytes[0], length(ABytes));
end;

class procedure TCipherMD5.UpdateMD5(var Context: TMD5Context; const Buf; BufSize: NativeInt);
var
  InBuf: TTransformInput;
  BufOfs: DWORD;
  MDI: DWORD;
  i: DWORD;
begin
  // { compute number of bytes mod 64 }
  MDI := (Context.Count[0] shr 3) and $3F;

  // { update number of bits }
  if BufSize shl 3 < 0 then
      inc(Context.Count[1]);

  inc(Context.Count[0], BufSize shl 3);
  inc(Context.Count[1], BufSize shr 29);

  { add new byte acters to buffer }
  BufOfs := 0;
  while (BufSize > 0) do
    begin
      dec(BufSize);
      Context.Buf[MDI] := TCCByteArray(Buf)[BufOfs]; { !!.01 }
      inc(MDI);
      inc(BufOfs);
      if (MDI = $40) then
        begin
          for i := 0 to 15 do
              InBuf[i] := PDWORD(@Context.Buf[i * 4])^;
          TMISC.Transform(Context.State, InBuf);
          MDI := 0;
        end;
    end;
end;

{ TRNG }

class procedure TRNG.EncryptRNG32(var Context: TRNG32Context; var Buf; BufSize: Integer);
var
  i: Integer;
begin
  for i := 0 to BufSize - 1 do
      TCCByteArray(Buf)[i] := TCCByteArray(Buf)[i] xor { !!.01 }
      TMISC.Random32Byte(Integer(Context));
end;

class procedure TRNG.EncryptRNG64(var Context: TRNG64Context; var Buf; BufSize: Integer);
var
  i: Integer;
begin
  for i := 0 to BufSize - 1 do
      TCCByteArray(Buf)[i] := TCCByteArray(Buf)[i] xor { !!.01 }
      TMISC.Random64Byte(TInt64(Context));
end;

class procedure TRNG.InitEncryptRNG32(key: DWORD; var Context: TRNG32Context);
begin
  DWORD(Context) := key;
end;

class procedure TRNG.InitEncryptRNG64(KeyHi, KeyLo: DWORD; var Context: TRNG64Context);
begin
  TInt64(Context).Lo := Integer(KeyLo);
  TInt64(Context).Hi := Integer(KeyHi);
end;

{ TLMD }

class procedure TLMD.FinalizeLMD(var Context: TLMDContext; var Digest; DigestSize: Integer);
const
  Padding: array [0 .. 7] of Byte = (1, 0, 0, 0, 0, 0, 0, 0);
var
  BCContext: TLBCContext;
  i: Integer;
begin
  { pad with "1", followed by as many "0"s as needed to fill the block }
  UpdateLMD(Context, Padding, SizeOf(Padding) - Context.KeyIndex);

  { mix context using block cipher }
  TLBC.InitEncryptLBC(Context.key, BCContext, 8, True);
  for i := 0 to (SizeOf(Context.Digest) div SizeOf(TLBCBlock)) - 1 do
      TLBC.EncryptLBC(BCContext, PLBCBlock(@Context.Digest[i * SizeOf(TLBCBlock)])^);

  { return Digest of requested DigestSize }
  { max digest is 2048-bit, although it could be greater if Pi2048 was larger }
  if DigestSize > SizeOf(Context.Digest) then
      FillPtrByte(@Digest, DigestSize, 0);
  CopyPtr(@Context.Digest, @Digest, Min(SizeOf(Context.Digest), DigestSize));
end;

class procedure TLMD.GenerateLMDKey(var key; KeySize: Integer; const ABytes: TBytes);
begin
  HashLMD(key, KeySize, ABytes[0], length(ABytes));
end;

class procedure TLMD.HashLMD(var Digest; DigestSize: Integer; const Buf; BufSize: NativeInt);
var
  Context: TLMDContext;
begin
  InitLMD(Context);
  UpdateLMD(Context, Buf, BufSize);
  FinalizeLMD(Context, Digest, DigestSize);
end;

class procedure TLMD.InitLMD(var Context: TLMDContext);
begin
  Context.DigestIndex := 0;
  TBlock2048(Context.Digest) := TBlock2048(Pi2048);

  Context.KeyIndex := 0;
  Context.KeyInts[0] := $55555555;
  Context.KeyInts[1] := $55555555;
  Context.KeyInts[2] := $55555555;
  Context.KeyInts[3] := $55555555;
end;

class procedure TLMD.ByteBuffHashLMD(var Digest; DigestSize: Integer; const ABytes: TBytes);
begin
  HashLMD(Digest, DigestSize, ABytes[0], length(ABytes));
end;

class procedure TLMD.UpdateLMD(var Context: TLMDContext; const Buf; BufSize: NativeInt);
var
  AA, BB, CC, DD: DWORD;
  i, r: NativeInt;
begin
  for i := 0 to BufSize - 1 do
    with Context do begin
        { update Digest }
        Digest[DigestIndex] := Digest[DigestIndex] xor
          TCCByteArray(Buf)[i]; { !!.01 }
        DigestIndex := DigestIndex + 1;
        if (DigestIndex = SizeOf(Digest)) then
            DigestIndex := 0;

        { update BlockKey }
        key[KeyIndex] := key[KeyIndex] xor TCCByteArray(Buf)[i]; { !!.01 }
        KeyIndex := KeyIndex + 1;
        if (KeyIndex = SizeOf(key) div 2) then begin
            AA := KeyInts[3];
            BB := KeyInts[2];
            CC := KeyInts[1];
            DD := KeyInts[0];

            { mix all the bits around for 4 rounds }
            { achieves avalanche and eliminates funnels }
            for r := 0 to 3 do
              begin
                inc(AA, DD);
                inc(DD, AA);
                AA := AA xor (AA shr 7);
                inc(BB, AA);
                inc(AA, BB);
                BB := BB xor (BB shl 13);
                inc(CC, BB);
                inc(BB, CC);
                CC := CC xor (CC shr 17);
                inc(DD, CC);
                inc(CC, DD);
                DD := DD xor (DD shl 9);
                inc(AA, DD);
                inc(DD, AA);
                AA := AA xor (AA shr 3);
                inc(BB, AA);
                inc(AA, BB);
                BB := BB xor (BB shl 7);
                inc(CC, BB);
                inc(BB, CC);
                CC := CC xor (DD shr 15);
                inc(DD, CC);
                inc(CC, DD);
                DD := DD xor (DD shl 11);
              end;

            KeyInts[0] := AA;
            KeyInts[1] := BB;
            KeyInts[2] := CC;
            KeyInts[3] := DD;

            KeyIndex := 0;
          end;
      end;
end;

{ TLSC }

class procedure TLSC.EncryptLSC(var Context: TLSCContext; var Buf; BufSize: Integer);
var
  L, y, x: Integer;
  i, a: Integer;
begin
  i := Context.index;
  a := Context.Accumulator;

  for L := 0 to BufSize - 1 do begin
      i := i + 1;

      x := Context.SBox[Byte(i)];
      y := Context.SBox[Byte(x)] + x;
      Context.SBox[Byte(i)] := Context.SBox[Byte(y)];
      Context.SBox[Byte(y)] := x;

      a := a + Context.SBox[Byte(Byte(y shr 8) + Byte(y))];
      TCCByteArray(Buf)[L] := TCCByteArray(Buf)[L] xor Byte(a); { !!.01 }
    end;

  Context.index := i;
  Context.Accumulator := a;
end;

class procedure TLSC.InitEncryptLSC(const key; KeySize: Integer; var Context: TLSCContext);
var
  r, i, a: Integer;
  x: Byte;
begin
  { initialize SBox }
  for i := 0 to 255 do
      Context.SBox[i] := i;

  a := 0;
  for r := 0 to 2 do { 3 rounds - "A" accumulates }
    for i := 0 to 255 do begin
        a := a + Context.SBox[i] + TCCByteArray(key)[i mod KeySize]; { !!.01 }
        x := Context.SBox[i];
        Context.SBox[i] := Context.SBox[Byte(a)];
        Context.SBox[Byte(a)] := x;
      end;

  Context.index := 0;
  Context.Accumulator := a;
end;

{ TMISC }

class procedure TMISC.GenerateRandomKey(var key; KeySize: Integer);
var
  i: Integer;
begin
  Randomize;
  for i := 0 to KeySize - 1 do
      TCCByteArray(key)[i] := System.Random(256); { !!.01 }
end;

class procedure TMISC.HashELF(var Digest: DWORD; const Buf; BufSize: nativeUInt);
var
  i: nativeUInt;
  x: DWORD;
begin
  Digest := 0;
  for i := 0 to BufSize - 1 do begin
      Digest := (Digest shl 4) + TCCByteArray(Buf)[i]; { !!.01 }
      x := Digest and $F0000000;
      if (x <> 0) then
          Digest := Digest xor (x shr 24);
      Digest := Digest and (not x);
    end;
end;

class procedure TMISC.HashELF64(var Digest: Int64; const Buf; BufSize: nativeUInt);
var
  i: nativeUInt;
  x: Int64;
begin
  Digest := 0;
  for i := 0 to BufSize - 1 do begin
      Digest := (Digest shl 4) + TCCByteArray(Buf)[i]; { !!.01 }
      x := Digest and Int64($F000000000000000);
      if (x <> 0) then
          Digest := Digest xor (x shr 56);
      Digest := Digest and (not x);
    end;
end;

class procedure TMISC.HashMix128(var Digest: DWORD; const Buf; BufSize: nativeUInt);
type
  T128BitArray = array [0 .. MaxStructSize div SizeOf(T128Bit) - 1] of T128Bit;
var
  Temp: T128Bit;
  PTemp: PCCByteArray;
  i, L: NativeInt;
begin
  Temp[0] := $243F6A88; { first 16 bytes of Pi in binary }
  Temp[1] := $93F40317;
  Temp[2] := $0C110496;
  Temp[3] := $C709C289;

  L := BufSize div SizeOf(T128Bit);
  for i := 0 to L - 1 do begin
      Temp[0] := Temp[0] + T128BitArray(Buf)[i][0]; { !!.01 }
      Temp[1] := Temp[1] + T128BitArray(Buf)[i][1]; { !!.01 }
      Temp[2] := Temp[2] + T128BitArray(Buf)[i][2]; { !!.01 }
      Temp[3] := Temp[3] + T128BitArray(Buf)[i][3]; { !!.01 }
      Mix128(Temp);
    end;

  PTemp := @Temp;
  if (BufSize > L * SizeOf(T128Bit)) then
    begin
      for i := 0 to (BufSize - L * SizeOf(T128Bit)) - 1 do
          inc(PTemp^[i], PCCByteArray(@Buf)^[(L * SizeOf(T128Bit)) + i]); { !!.01 }
      Mix128(Temp);
    end;

  Digest := Temp[3];
end;

class procedure TMISC.Mix128(var x: T128Bit);
var
  AA, BB, CC, DD: DWORD;
begin
  AA := x[0];
  BB := x[1];
  CC := x[2];
  DD := x[3];

  AA := AA + DD;
  DD := DD + AA;
  AA := AA xor (AA shr 7);
  BB := BB + AA;
  AA := AA + BB;
  BB := BB xor (BB shl 13);
  CC := CC + BB;
  BB := BB + CC;
  CC := CC xor (CC shr 17);
  DD := DD + CC;
  CC := CC + DD;
  DD := DD xor (DD shl 9);
  AA := AA + DD;
  DD := DD + AA;
  AA := AA xor (AA shr 3);
  BB := BB + AA;
  AA := AA + BB;
  BB := BB xor (BB shl 7);
  CC := CC + BB;
  BB := BB + CC;
  CC := CC xor (DD shr 15);
  DD := DD + CC;
  CC := CC + DD;
  DD := DD xor (DD shl 11);

  x[0] := AA;
  x[1] := BB;
  x[2] := CC;
  x[3] := DD;
end;

class function TMISC.Ran01(var Seed: Integer): Integer;
begin
  Result := Ran0Prim(Seed, 16807, 127773, 2836);
end;

class function TMISC.Ran02(var Seed: Integer): Integer;
begin
  Result := Ran0Prim(Seed, 48271, 44488, 3399);
end;

class function TMISC.Ran03(var Seed: Integer): Integer;
begin
  Result := Ran0Prim(Seed, 69621, 30845, 23902);
end;

class function TMISC.Ran0Prim(var Seed: Integer; IA, IQ, IR: Integer): Integer;
const
  IM = 2147483647;
  MA = 123459876;
var
  i, k: Integer;
begin
  { XORing with mask avoids seeds of zero }
  i := Seed xor MA;
  k := i div IQ;
  i := (IA * (i - (k * IQ))) - (IR * k);
  if i < 0 then
      i := i + IM;
  Result := i xor MA;
  Seed := Result;
end;

class function TMISC.Random32Byte(var Seed: Integer): Byte;
var
  L: Integer;
  r: TInt32;
begin
  L := Ran01(Seed);
  r := TInt32(L);
  Result := r.LoLo xor r.LoHi xor r.HiLo xor r.HiHi;
end;

class function TMISC.Random64(var Seed: TInt64): Integer;
begin
  Ran01(Seed.Lo);
  Ran01(Seed.Hi);
  Result := Seed.Lo xor Seed.Hi;
end;

class function TMISC.Random64Byte(var Seed: TInt64): Byte;
var
  L: Integer;
  r: TInt32;
begin
  L := Random64(Seed);
  r := TInt32(L);
  Result := r.LoLo xor r.LoHi xor r.HiLo xor r.HiHi;
end;

class function TMISC.RolX(i, c: DWORD): DWORD;
begin
  Result := (i shl (c and 31)) or (i shr (32 - (c and 31)));
end;

class procedure TMISC.ByteBuffHashELF(var Digest: DWORD; const ABytes: TBytes);
begin
  HashELF(Digest, ABytes[0], length(ABytes));
end;

class procedure TMISC.ByteBuffHashMix128(var Digest: DWORD; const ABytes: TBytes);
begin
  HashMix128(Digest, ABytes[0], length(ABytes));
end;

class procedure TMISC.Transform(var OutputBuffer: TTransformOutput; var InBuf: TTransformInput);
const
  S11 = 7;
  S12 = 12;
  S13 = 17;
  S14 = 22;
  S21 = 5;
  S22 = 9;
  S23 = 14;
  S24 = 20;
  S31 = 4;
  S32 = 11;
  S33 = 16;
  S34 = 23;
  S41 = 6;
  S42 = 10;
  S43 = 15;
  S44 = 21;
var
  a: DWORD;
  b: DWORD;
  c: DWORD;
  d: DWORD;

  procedure FF(var a: DWORD; const b, c, d, x, s, AC: DWORD);
  begin
    a := RolX(a + ((b and c) or (not b and d)) + x + AC, s) + b;
  end;

  procedure GG(var a: DWORD; const b, c, d, x, s, AC: DWORD);
  begin
    a := RolX(a + ((b and d) or (c and not d)) + x + AC, s) + b;
  end;

  procedure HH(var a: DWORD; const b, c, d, x, s, AC: DWORD);
  begin
    a := RolX(a + (b xor c xor d) + x + AC, s) + b;
  end;

  procedure II(var a: DWORD; const b, c, d, x, s, AC: DWORD);
  begin
    a := RolX(a + (c xor (b or not d)) + x + AC, s) + b;
  end;

begin
  a := OutputBuffer[0];
  b := OutputBuffer[1];
  c := OutputBuffer[2];
  d := OutputBuffer[3];

  { round 1 }
  FF(a, b, c, d, InBuf[0], S11, $D76AA478);  { 1 }
  FF(d, a, b, c, InBuf[1], S12, $E8C7B756);  { 2 }
  FF(c, d, a, b, InBuf[2], S13, $242070DB);  { 3 }
  FF(b, c, d, a, InBuf[3], S14, $C1BDCEEE);  { 4 }
  FF(a, b, c, d, InBuf[4], S11, $F57C0FAF);  { 5 }
  FF(d, a, b, c, InBuf[5], S12, $4787C62A);  { 6 }
  FF(c, d, a, b, InBuf[6], S13, $A8304613);  { 7 }
  FF(b, c, d, a, InBuf[7], S14, $FD469501);  { 8 }
  FF(a, b, c, d, InBuf[8], S11, $698098D8);  { 9 }
  FF(d, a, b, c, InBuf[9], S12, $8B44F7AF);  { 10 }
  FF(c, d, a, b, InBuf[10], S13, $FFFF5BB1); { 11 }
  FF(b, c, d, a, InBuf[11], S14, $895CD7BE); { 12 }
  FF(a, b, c, d, InBuf[12], S11, $6B901122); { 13 }
  FF(d, a, b, c, InBuf[13], S12, $FD987193); { 14 }
  FF(c, d, a, b, InBuf[14], S13, $A679438E); { 15 }
  FF(b, c, d, a, InBuf[15], S14, $49B40821); { 16 }

  { round 2 }
  GG(a, b, c, d, InBuf[1], S21, $F61E2562);  { 17 }
  GG(d, a, b, c, InBuf[6], S22, $C040B340);  { 18 }
  GG(c, d, a, b, InBuf[11], S23, $265E5A51); { 19 }
  GG(b, c, d, a, InBuf[0], S24, $E9B6C7AA);  { 20 }
  GG(a, b, c, d, InBuf[5], S21, $D62F105D);  { 21 }
  GG(d, a, b, c, InBuf[10], S22, $02441453); { 22 }
  GG(c, d, a, b, InBuf[15], S23, $D8A1E681); { 23 }
  GG(b, c, d, a, InBuf[4], S24, $E7D3FBC8);  { 24 }
  GG(a, b, c, d, InBuf[9], S21, $21E1CDE6);  { 25 }
  GG(d, a, b, c, InBuf[14], S22, $C33707D6); { 26 }
  GG(c, d, a, b, InBuf[3], S23, $F4D50D87);  { 27 }
  GG(b, c, d, a, InBuf[8], S24, $455A14ED);  { 28 }
  GG(a, b, c, d, InBuf[13], S21, $A9E3E905); { 29 }
  GG(d, a, b, c, InBuf[2], S22, $FCEFA3F8);  { 30 }
  GG(c, d, a, b, InBuf[7], S23, $676F02D9);  { 31 }
  GG(b, c, d, a, InBuf[12], S24, $8D2A4C8A); { 32 }

  { round 3 }
  HH(a, b, c, d, InBuf[5], S31, $FFFA3942);  { 33 }
  HH(d, a, b, c, InBuf[8], S32, $8771F681);  { 34 }
  HH(c, d, a, b, InBuf[11], S33, $6D9D6122); { 35 }
  HH(b, c, d, a, InBuf[14], S34, $FDE5380C); { 36 }
  HH(a, b, c, d, InBuf[1], S31, $A4BEEA44);  { 37 }
  HH(d, a, b, c, InBuf[4], S32, $4BDECFA9);  { 38 }
  HH(c, d, a, b, InBuf[7], S33, $F6BB4B60);  { 39 }
  HH(b, c, d, a, InBuf[10], S34, $BEBFBC70); { 40 }
  HH(a, b, c, d, InBuf[13], S31, $289B7EC6); { 41 }
  HH(d, a, b, c, InBuf[0], S32, $EAA127FA);  { 42 }
  HH(c, d, a, b, InBuf[3], S33, $D4EF3085);  { 43 }
  HH(b, c, d, a, InBuf[6], S34, $4881D05);   { 44 }
  HH(a, b, c, d, InBuf[9], S31, $D9D4D039);  { 45 }
  HH(d, a, b, c, InBuf[12], S32, $E6DB99E5); { 46 }
  HH(c, d, a, b, InBuf[15], S33, $1FA27CF8); { 47 }
  HH(b, c, d, a, InBuf[2], S34, $C4AC5665);  { 48 }

  { round 4 }
  II(a, b, c, d, InBuf[0], S41, $F4292244);  { 49 }
  II(d, a, b, c, InBuf[7], S42, $432AFF97);  { 50 }
  II(c, d, a, b, InBuf[14], S43, $AB9423A7); { 51 }
  II(b, c, d, a, InBuf[5], S44, $FC93A039);  { 52 }
  II(a, b, c, d, InBuf[12], S41, $655B59C3); { 53 }
  II(d, a, b, c, InBuf[3], S42, $8F0CCC92);  { 54 }
  II(c, d, a, b, InBuf[10], S43, $FFEFF47D); { 55 }
  II(b, c, d, a, InBuf[1], S44, $85845DD1);  { 56 }
  II(a, b, c, d, InBuf[8], S41, $6FA87E4F);  { 57 }
  II(d, a, b, c, InBuf[15], S42, $FE2CE6E0); { 58 }
  II(c, d, a, b, InBuf[6], S43, $A3014314);  { 59 }
  II(b, c, d, a, InBuf[13], S44, $4E0811A1); { 60 }
  II(a, b, c, d, InBuf[4], S41, $F7537E82);  { 61 }
  II(d, a, b, c, InBuf[11], S42, $BD3AF235); { 62 }
  II(c, d, a, b, InBuf[2], S43, $2AD7D2BB);  { 63 }
  II(b, c, d, a, InBuf[9], S44, $EB86D391);  { 64 }

  inc(OutputBuffer[0], a);
  inc(OutputBuffer[1], b);
  inc(OutputBuffer[2], c);
  inc(OutputBuffer[3], d);
end;

class procedure TMISC.XorMem(var Mem1; const Mem2; Count: NativeInt);
var
  siz: Byte;
  i: Integer;
  p1, p2: nativeUInt;
begin
  p1 := nativeUInt(@Mem1);
  p2 := nativeUInt(@Mem2);
  siz := SizeOf(nativeUInt);

  for i := 1 to Count div siz do
    begin
      PNativeUInt(p1)^ := PNativeUInt(p1)^ xor PNativeUInt(p2)^;
      p1 := p1 + siz;
      p2 := p2 + siz;
    end;
  for i := 1 to Count mod siz do
    begin
      PByte(p1)^ := PByte(p1)^ xor PByte(p2)^;
      p1 := p1 + 1;
      p2 := p2 + 1;
    end;
end;

procedure DCP_twofish_InitKey(const key; Size: Cardinal; var SubKeys: TDCPTFSubKeys; var SBox: TDCPTFSBox);
  function RS_MDS_Encode(lK0, lK1: DWORD): DWORD;
  var
    lR, nJ, lG2, lG3: DWORD;
    BB: Byte;
  begin
    lR := lK1;
    for nJ := 0 to 3 do
      begin
        BB := lR shr 24;
        if (BB and $80) <> 0 then
            lG2 := ((BB shl 1) xor RS_GF_FDBK) and $FF
        else
            lG2 := (BB shl 1) and $FF;
        if (BB and 1) <> 0 then
            lG3 := ((BB shr 1) and $7F) xor (RS_GF_FDBK shr 1) xor lG2
        else
            lG3 := ((BB shr 1) and $7F) xor lG2;
        lR := (lR shl 8) xor (lG3 shl 24) xor (lG2 shl 16) xor (lG3 shl 8) xor BB;
      end;
    lR := lR xor lK0;
    for nJ := 0 to 3 do
      begin
        BB := lR shr 24;
        if (BB and $80) <> 0 then
            lG2 := ((BB shl 1) xor RS_GF_FDBK) and $FF
        else
            lG2 := (BB shl 1) and $FF;
        if (BB and 1) <> 0 then
            lG3 := ((BB shr 1) and $7F) xor (RS_GF_FDBK shr 1) xor lG2
        else
            lG3 := ((BB shr 1) and $7F) xor lG2;
        lR := (lR shl 8) xor (lG3 shl 24) xor (lG2 shl 16) xor (lG3 shl 8) xor BB;
      end;
    Result := lR;
  end;

  function f32(x: DWORD; const K32: T128Bit; Len: DWORD): DWORD;
  var
    t0, t1, t2, t3: DWORD;
  begin
    t0 := x and $FF;
    t1 := (x shr 8) and $FF;
    t2 := (x shr 16) and $FF;
    t3 := x shr 24;
    if Len = 256 then
      begin
        t0 := DCPTF_p8x8[1, t0] xor ((K32[3]) and $FF);
        t1 := DCPTF_p8x8[0, t1] xor ((K32[3] shr 8) and $FF);
        t2 := DCPTF_p8x8[0, t2] xor ((K32[3] shr 16) and $FF);
        t3 := DCPTF_p8x8[1, t3] xor ((K32[3] shr 24));
      end;
    if Len >= 192 then
      begin
        t0 := DCPTF_p8x8[1, t0] xor ((K32[2]) and $FF);
        t1 := DCPTF_p8x8[1, t1] xor ((K32[2] shr 8) and $FF);
        t2 := DCPTF_p8x8[0, t2] xor ((K32[2] shr 16) and $FF);
        t3 := DCPTF_p8x8[0, t3] xor ((K32[2] shr 24));
      end;
    Result := twofish_SBox[0, DCPTF_p8x8[0, DCPTF_p8x8[0, t0] xor ((K32[1]) and $FF)] xor ((K32[0]) and $FF)] xor
      twofish_SBox[1, DCPTF_p8x8[0, DCPTF_p8x8[1, t1] xor ((K32[1] shr 8) and $FF)] xor ((K32[0] shr 8) and $FF)] xor
      twofish_SBox[2, DCPTF_p8x8[1, DCPTF_p8x8[0, t2] xor ((K32[1] shr 16) and $FF)] xor ((K32[0] shr 16) and $FF)] xor
      twofish_SBox[3, DCPTF_p8x8[1, DCPTF_p8x8[1, t3] xor ((K32[1] shr 24))] xor ((K32[0] shr 24))];
  end;

  procedure Xor256(var Dst: TDCPTF2048; const Src: TDCPTF2048; v: Byte);
  var
    i, j: DWORD;
    PDst, PSrc: PDWORD;
  begin
    j := v * $01010101;
    PDst := @Dst;
    PSrc := @Src;
    for i := 0 to 63 do
      begin
        PDst^ := PSrc^ xor j;
        inc(PSrc);
        inc(PDst)
      end
  end;

const
  subkeyCnt = DCPTF_ROUNDSUBKEYS + 2 * DCPTF_NUMROUNDS;
var
  key32: T256Bit;
  k32e, k32o, sboxkeys: T128Bit;
  k64Cnt, i, j, a, b, q: DWORD;
  L0, L1: TDCPTF2048;
begin
  FillPtrByte(@key32, SizeOf(key32), 0);
  CopyPtr(@key, @key32, Size div 8);
  if Size <= 128 then { pad the key to either 128bit, 192bit or 256bit }
      Size := 128
  else if Size <= 192 then
      Size := 192
  else
      Size := 128;
  k64Cnt := Size div 64;
  j := k64Cnt - 1;
  for i := 0 to j do
    begin
      k32e[i] := key32[2 * i];
      k32o[i] := key32[2 * i + 1];
      sboxkeys[j] := RS_MDS_Encode(k32e[i], k32o[i]);
      dec(j);
    end;
  q := 0;
  for i := 0 to ((subkeyCnt div 2) - 1) do
    begin
      a := f32(q, k32e, Size);
      b := f32(q + SK_BUMP, k32o, Size);
      b := (b shl 8) or (b shr 24);
      SubKeys[2 * i] := a + b;
      b := a + 2 * b;
      SubKeys[2 * i + 1] := (b shl SK_ROTL) or (b shr (32 - SK_ROTL));
      inc(q, SK_STEP);
    end;
  case Size of
    128: begin
        Xor256(L0, DCPTF_p8x8[0], (sboxkeys[1] and $FF));
        a := (sboxkeys[0] and $FF);
        i := 0;
        while i < 256 do
          begin
            SBox[0 and 2, 2 * i + (0 and 1)] := twofish_SBox[0, DCPTF_p8x8[0, L0[i]] xor a];
            SBox[0 and 2, 2 * i + (0 and 1) + 2] := twofish_SBox[0, DCPTF_p8x8[0, L0[i + 1]] xor a];
            inc(i, 2);
          end;
        Xor256(L0, DCPTF_p8x8[1], (sboxkeys[1] shr 8) and $FF);
        a := (sboxkeys[0] shr 8) and $FF;
        i := 0;
        while i < 256 do
          begin
            SBox[1 and 2, 2 * i + (1 and 1)] := twofish_SBox[1, DCPTF_p8x8[0, L0[i]] xor a];
            SBox[1 and 2, 2 * i + (1 and 1) + 2] := twofish_SBox[1, DCPTF_p8x8[0, L0[i + 1]] xor a];
            inc(i, 2);
          end;
        Xor256(L0, DCPTF_p8x8[0], (sboxkeys[1] shr 16) and $FF);
        a := (sboxkeys[0] shr 16) and $FF;
        i := 0;
        while i < 256 do
          begin
            SBox[2 and 2, 2 * i + (2 and 1)] := twofish_SBox[2, DCPTF_p8x8[1, L0[i]] xor a];
            SBox[2 and 2, 2 * i + (2 and 1) + 2] := twofish_SBox[2, DCPTF_p8x8[1, L0[i + 1]] xor a];
            inc(i, 2);
          end;
        Xor256(L0, DCPTF_p8x8[1], (sboxkeys[1] shr 24));
        a := (sboxkeys[0] shr 24);
        i := 0;
        while i < 256 do
          begin
            SBox[3 and 2, 2 * i + (3 and 1)] := twofish_SBox[3, DCPTF_p8x8[1, L0[i]] xor a];
            SBox[3 and 2, 2 * i + (3 and 1) + 2] := twofish_SBox[3, DCPTF_p8x8[1, L0[i + 1]] xor a];
            inc(i, 2);
          end;
      end;
    192: begin
        Xor256(L0, DCPTF_p8x8[1], sboxkeys[2] and $FF);
        a := sboxkeys[0] and $FF;
        b := sboxkeys[1] and $FF;
        i := 0;
        while i < 256 do
          begin
            SBox[0 and 2, 2 * i + (0 and 1)] := twofish_SBox[0, DCPTF_p8x8[0, DCPTF_p8x8[0, L0[i]] xor b] xor a];
            SBox[0 and 2, 2 * i + (0 and 1) + 2] := twofish_SBox[0, DCPTF_p8x8[0, DCPTF_p8x8[0, L0[i + 1]] xor b] xor a];
            inc(i, 2);
          end;
        Xor256(L0, DCPTF_p8x8[1], (sboxkeys[2] shr 8) and $FF);
        a := (sboxkeys[0] shr 8) and $FF;
        b := (sboxkeys[1] shr 8) and $FF;
        i := 0;
        while i < 256 do
          begin
            SBox[1 and 2, 2 * i + (1 and 1)] := twofish_SBox[1, DCPTF_p8x8[0, DCPTF_p8x8[1, L0[i]] xor b] xor a];
            SBox[1 and 2, 2 * i + (1 and 1) + 2] := twofish_SBox[1, DCPTF_p8x8[0, DCPTF_p8x8[1, L0[i + 1]] xor b] xor a];
            inc(i, 2);
          end;
        Xor256(L0, DCPTF_p8x8[0], (sboxkeys[2] shr 16) and $FF);
        a := (sboxkeys[0] shr 16) and $FF;
        b := (sboxkeys[1] shr 16) and $FF;
        i := 0;
        while i < 256 do
          begin
            SBox[2 and 2, 2 * i + (2 and 1)] := twofish_SBox[2, DCPTF_p8x8[1, DCPTF_p8x8[0, L0[i]] xor b] xor a];
            SBox[2 and 2, 2 * i + (2 and 1) + 2] := twofish_SBox[2, DCPTF_p8x8[1, DCPTF_p8x8[0, L0[i + 1]] xor b] xor a];
            inc(i, 2);
          end;
        Xor256(L0, DCPTF_p8x8[0], (sboxkeys[2] shr 24));
        a := (sboxkeys[0] shr 24);
        b := (sboxkeys[1] shr 24);
        i := 0;
        while i < 256 do
          begin
            SBox[3 and 2, 2 * i + (3 and 1)] := twofish_SBox[3, DCPTF_p8x8[1, DCPTF_p8x8[1, L0[i]] xor b] xor a];
            SBox[3 and 2, 2 * i + (3 and 1) + 2] := twofish_SBox[3, DCPTF_p8x8[1, DCPTF_p8x8[1, L0[i + 1]] xor b] xor a];
            inc(i, 2);
          end;
      end;
    256: begin
        Xor256(L1, DCPTF_p8x8[1], (sboxkeys[3]) and $FF);
        i := 0;
        while i < 256 do
          begin
            L0[i] := DCPTF_p8x8[1, L1[i]];
            L0[i + 1] := DCPTF_p8x8[1, L1[i + 1]];
            inc(i, 2);
          end;
        Xor256(L0, L0, (sboxkeys[2]) and $FF);
        a := (sboxkeys[0]) and $FF;
        b := (sboxkeys[1]) and $FF;
        i := 0;
        while i < 256 do
          begin
            try
                SBox[0 and 2, 2 * i + (0 and 1)] := twofish_SBox[0, DCPTF_p8x8[0, DCPTF_p8x8[0, L0[i]] xor b] xor a];
            except
            end;
            SBox[0 and 2, 2 * i + (0 and 1) + 2] := twofish_SBox[0, DCPTF_p8x8[0, DCPTF_p8x8[0, L0[i + 1]] xor b] xor a];
            inc(i, 2);
          end;
        Xor256(L1, DCPTF_p8x8[0], (sboxkeys[3] shr 8) and $FF);
        i := 0;
        while i < 256 do
          begin
            L0[i] := DCPTF_p8x8[1, L1[i]];
            L0[i + 1] := DCPTF_p8x8[1, L1[i + 1]];
            inc(i, 2);
          end;
        Xor256(L0, L0, (sboxkeys[2] shr 8) and $FF);
        a := (sboxkeys[0] shr 8) and $FF;
        b := (sboxkeys[1] shr 8) and $FF;
        i := 0;
        while i < 256 do
          begin
            SBox[1 and 2, 2 * i + (1 and 1)] := twofish_SBox[1, DCPTF_p8x8[0, DCPTF_p8x8[1, L0[i]] xor b] xor a];
            SBox[1 and 2, 2 * i + (1 and 1) + 2] := twofish_SBox[1, DCPTF_p8x8[0, DCPTF_p8x8[1, L0[i + 1]] xor b] xor a];
            inc(i, 2);
          end;

        Xor256(L1, DCPTF_p8x8[0], (sboxkeys[3] shr 16) and $FF);
        i := 0;
        while i < 256 do
          begin
            L0[i] := DCPTF_p8x8[0, L1[i]];
            L0[i + 1] := DCPTF_p8x8[0, L1[i + 1]];
            inc(i, 2);
          end;
        Xor256(L0, L0, (sboxkeys[2] shr 16) and $FF);
        a := (sboxkeys[0] shr 16) and $FF;
        b := (sboxkeys[1] shr 16) and $FF;
        i := 0;
        while i < 256 do
          begin
            SBox[2 and 2, 2 * i + (2 and 1)] := twofish_SBox[2, DCPTF_p8x8[1, DCPTF_p8x8[0, L0[i]] xor b] xor a];
            SBox[2 and 2, 2 * i + (2 and 1) + 2] := twofish_SBox[2, DCPTF_p8x8[1, DCPTF_p8x8[0, L0[i + 1]] xor b] xor a];
            inc(i, 2);
          end;
        Xor256(L1, DCPTF_p8x8[1], (sboxkeys[3] shr 24));
        i := 0;
        while i < 256 do
          begin
            L0[i] := DCPTF_p8x8[0, L1[i]];
            L0[i + 1] := DCPTF_p8x8[0, L1[i + 1]];
            inc(i, 2);
          end;
        Xor256(L0, L0, (sboxkeys[2] shr 24));
        a := (sboxkeys[0] shr 24);
        b := (sboxkeys[1] shr 24);
        i := 0;
        while i < 256 do
          begin
            SBox[3 and 2, 2 * i + (3 and 1)] := twofish_SBox[3, DCPTF_p8x8[1, DCPTF_p8x8[1, L0[i]] xor b] xor a];
            SBox[3 and 2, 2 * i + (3 and 1) + 2] := twofish_SBox[3, DCPTF_p8x8[1, DCPTF_p8x8[1, L0[i + 1]] xor b] xor a];
            inc(i, 2);
          end;
      end;
  end;
end;

procedure DCP_twofish_EncryptECB(var SubKeys: TDCPTFSubKeys; var SBox: TDCPTFSBox; const InData: T128Bit; var OutData: T128Bit);
var
  i: Cardinal;
  t0, t1: DWORD;
  x: T128Bit;
  k: Integer;
begin
  for k := 0 to 3 do
      x[k] := InData[k] xor SubKeys[INPUTWHITEN + k];
  i := 0;
  while i <= DCPTF_NUMROUNDS - 2 do
    begin
      t0 := SBox[0, (x[0] shl 1) and $1FE] xor SBox[0, ((x[0] shr 7) and $1FE) + 1]
        xor SBox[2, (x[0] shr 15) and $1FE] xor SBox[2, ((x[0] shr 23) and $1FE) + 1];

      t1 := SBox[0, ((x[1] shr 23) and $1FE)] xor SBox[0, ((x[1] shl 1) and $1FE) + 1]
        xor SBox[2, ((x[1] shr 7) and $1FE)] xor SBox[2, ((x[1] shr 15) and $1FE) + 1];

      x[3] := (x[3] shl 1) or (x[3] shr 31);
      x[2] := x[2] xor (t0 + t1 + SubKeys[DCPTF_ROUNDSUBKEYS + 2 * i]);
      x[3] := x[3] xor (t0 + 2 * t1 + SubKeys[DCPTF_ROUNDSUBKEYS + 2 * i + 1]);
      x[2] := (x[2] shr 1) or (x[2] shl 31);

      t0 := SBox[0, (x[2] shl 1) and $1FE] xor SBox[0, ((x[2] shr 7) and $1FE) + 1]
        xor SBox[2, ((x[2] shr 15) and $1FE)] xor SBox[2, ((x[2] shr 23) and $1FE) + 1];

      t1 := SBox[0, ((x[3] shr 23) and $1FE)] xor SBox[0, ((x[3] shl 1) and $1FE) + 1]
        xor SBox[2, ((x[3] shr 7) and $1FE)] xor SBox[2, ((x[3] shr 15) and $1FE) + 1];

      x[1] := (x[1] shl 1) or (x[1] shr 31);
      x[0] := x[0] xor (t0 + t1 + SubKeys[DCPTF_ROUNDSUBKEYS + 2 * (i + 1)]);
      x[1] := x[1] xor (t0 + 2 * t1 + SubKeys[DCPTF_ROUNDSUBKEYS + 2 * (i + 1) + 1]);
      x[0] := (x[0] shr 1) or (x[0] shl 31);
      inc(i, 2);
    end;
  OutData[0] := x[2] xor SubKeys[DCPTF_OUTPUTWHITEN];
  OutData[1] := x[3] xor SubKeys[DCPTF_OUTPUTWHITEN + 1];
  OutData[2] := x[0] xor SubKeys[DCPTF_OUTPUTWHITEN + 2];
  OutData[3] := x[1] xor SubKeys[DCPTF_OUTPUTWHITEN + 3];
end;

procedure DCP_twofish_DecryptECB(var SubKeys: TDCPTFSubKeys; var SBox: TDCPTFSBox; const InData: T128Bit; var OutData: T128Bit);
var
  i, k: Integer;
  t0, t1: DWORD;
  x: T128Bit;
begin
  x[2] := InData[0] xor SubKeys[DCPTF_OUTPUTWHITEN];
  x[3] := InData[1] xor SubKeys[DCPTF_OUTPUTWHITEN + 1];
  x[0] := InData[2] xor SubKeys[DCPTF_OUTPUTWHITEN + 2];
  x[1] := InData[3] xor SubKeys[DCPTF_OUTPUTWHITEN + 3];

  i := DCPTF_NUMROUNDS - 2;

  while i >= 0 do
    begin
      t0 := SBox[0, (x[2] shl 1) and $1FE] xor SBox[0, ((x[2] shr 7) and $1FE) + 1]
        xor SBox[2, ((x[2] shr 15) and $1FE)] xor SBox[2, ((x[2] shr 23) and $1FE) + 1];

      t1 := SBox[0, ((x[3] shr 23) and $1FE)] xor SBox[0, ((x[3] shl 1) and $1FE) + 1]
        xor SBox[2, ((x[3] shr 7) and $1FE)] xor SBox[2, ((x[3] shr 15) and $1FE) + 1];

      x[0] := (x[0] shl 1) or (x[0] shr 31);
      x[0] := x[0] xor (t0 + t1 + SubKeys[DCPTF_ROUNDSUBKEYS + 2 * (i + 1)]);
      x[1] := x[1] xor (t0 + 2 * t1 + SubKeys[DCPTF_ROUNDSUBKEYS + 2 * (i + 1) + 1]);
      x[1] := (x[1] shr 1) or (x[1] shl 31);

      t0 := SBox[0, (x[0] shl 1) and $1FE] xor SBox[0, ((x[0] shr 7) and $1FE) + 1]
        xor SBox[2, (x[0] shr 15) and $1FE] xor SBox[2, ((x[0] shr 23) and $1FE) + 1];

      t1 := SBox[0, ((x[1] shr 23) and $1FE)] xor SBox[0, ((x[1] shl 1) and $1FE) + 1]
        xor SBox[2, ((x[1] shr 7) and $1FE)] xor SBox[2, ((x[1] shr 15) and $1FE) + 1];

      x[2] := (x[2] shl 1) or (x[2] shr 31);
      x[2] := x[2] xor (t0 + t1 + SubKeys[DCPTF_ROUNDSUBKEYS + 2 * i]);
      x[3] := x[3] xor (t0 + 2 * t1 + SubKeys[DCPTF_ROUNDSUBKEYS + 2 * i + 1]);
      x[3] := (x[3] shr 1) or (x[3] shl 31);

      dec(i, 2);
    end;
  for k := 0 to 3 do
      OutData[k] := x[k] xor SubKeys[INPUTWHITEN + k]
end;

procedure DCP_towfish_Precomp;
  function LFSR1(const x: DWORD): DWORD;
  begin
    if (x and 1) <> 0 then
        Result := (x shr 1) xor (MDS_GF_FDBK div 2)
    else
        Result := (x shr 1);
  end;

  function LFSR2(const x: DWORD): DWORD;
  begin
    if (x and 2) <> 0 then
      if (x and 1) <> 0 then
          Result := (x shr 2) xor (MDS_GF_FDBK div 2) xor (MDS_GF_FDBK div 4)
      else
          Result := (x shr 2) xor (MDS_GF_FDBK div 2)
    else
      if (x and 1) <> 0 then
        Result := (x shr 2) xor (MDS_GF_FDBK div 4)
    else
        Result := (x shr 2);
  end;

  function Mul_X(const x: DWORD): DWORD;
  begin
    Result := x xor LFSR2(x);
  end;

  function Mul_Y(const x: DWORD): DWORD;
  begin
    Result := x xor LFSR1(x) xor LFSR2(x);
  end;

var
  m1, mx, my: array [0 .. 1] of DWORD;
  nI: Cardinal;
begin
  for nI := 0 to 255 do
    begin
      m1[0] := DCPTF_p8x8[0, nI];
      mx[0] := Mul_X(m1[0]);
      my[0] := Mul_Y(m1[0]);
      m1[1] := DCPTF_p8x8[1, nI];
      mx[1] := Mul_X(m1[1]);
      my[1] := Mul_Y(m1[1]);
      twofish_SBox[0, nI] := (m1[1] shl 0) or
        (mx[1] shl 8) or
        (my[1] shl 16) or
        (my[1] shl 24);
      twofish_SBox[1, nI] := (my[0] shl 0) or
        (my[0] shl 8) or
        (mx[0] shl 16) or
        (m1[0] shl 24);
      twofish_SBox[2, nI] := (mx[1] shl 0) or
        (my[1] shl 8) or
        (m1[1] shl 16) or
        (my[1] shl 24);
      twofish_SBox[3, nI] := (mx[0] shl 0) or
        (m1[0] shl 8) or
        (my[0] shl 16) or
        (mx[0] shl 24);
    end;
end;

function XXTeaMX(Sum, y, z, p, E: DWORD; const k: PDWordArray): DWORD;
begin
  Result := (((z shr 5) xor (y shl 2)) + ((y shr 3) xor (z shl 4))) xor ((Sum xor y) + (k^[p and 3 xor E] xor z));
end;

procedure XXTEAEncrypt(var key: TKey128; var Block: TXXTEABlock);
const
  XXTeaDelta = $9E3779B9;
var
  k, v: PDWordArray;
  n, z, y, Sum, E, p, q: DWORD;
begin
  n := 64 div 4 - 1;
  k := PDWordArray(@key[0]);
  v := PDWordArray(@Block[0]);
  z := v^[n];
  Sum := 0;
  q := 6 + 52 div (n + 1);
  repeat
    inc(Sum, XXTeaDelta);
    E := (Sum shr 2) and 3;
    for p := 0 to n - 1 do begin
        y := v^[p + 1];
        inc(v^[p], XXTeaMX(Sum, y, z, p, E, k));
        z := v^[p];
      end;
    p := n;
    y := v^[0];
    inc(v^[p], XXTeaMX(Sum, y, z, p, E, k));
    z := v^[p];
    dec(q);
  until q = 0;
end;

procedure XXTEADecrypt(var key: TKey128; var Block: TXXTEABlock);
const
  XXTeaDelta = $9E3779B9;
var
  k, v: PDWordArray;
  n, z, y, Sum, E, p, q: DWORD;
begin
  n := 64 div 4 - 1;
  k := PDWordArray(@key[0]);
  v := PDWordArray(@Block[0]);

  y := v^[0];
  q := 6 + 52 div (n + 1);
  Sum := q * XXTeaDelta;
  while (Sum <> 0) do
    begin
      E := (Sum shr 2) and 3;
      for p := n downto 1 do begin
          z := v^[p - 1];
          dec(v^[p], XXTeaMX(Sum, y, z, p, E, k));
          y := v^[p];
        end;
      p := 0;
      z := v^[n];
      dec(v^[0], XXTeaMX(Sum, y, z, p, E, k));
      y := v^[0];
      dec(Sum, XXTeaDelta);
    end;
end;

class function TRC6.LRot32(x, c: DWORD): DWORD;
begin
  LRot32 := ROL32(x, Byte(c));
end;

class function TRC6.RRot32(x, c: DWORD): DWORD;
begin
  RRot32 := ROR32(x, Byte(c))
end;

class procedure TRC6.InitKey(buff: Pointer; Size: Integer; var KeyContext: TRC6Key);
const
  cRC6_sBox: array [0 .. 51] of DWORD = (
    $B7E15163, $5618CB1C, $F45044D5, $9287BE8E, $30BF3847, $CEF6B200,
    $6D2E2BB9, $0B65A572, $A99D1F2B, $47D498E4, $E60C129D, $84438C56,
    $227B060F, $C0B27FC8, $5EE9F981, $FD21733A, $9B58ECF3, $399066AC,
    $D7C7E065, $75FF5A1E, $1436D3D7, $B26E4D90, $50A5C749, $EEDD4102,
    $8D14BABB, $2B4C3474, $C983AE2D, $67BB27E6, $05F2A19F, $A42A1B58,
    $42619511, $E0990ECA, $7ED08883, $1D08023C, $BB3F7BF5, $5976F5AE,
    $F7AE6F67, $95E5E920, $341D62D9, $D254DC92, $708C564B, $0EC3D004,
    $ACFB49BD, $4B32C376, $E96A3D2F, $87A1B6E8, $25D930A1, $C410AA5A,
    $62482413, $007F9DCC, $9EB71785, $3CEE913E);
var
  xKeyD: array [0 .. 63] of DWORD;
  i, j, k, xKeyLen, a, b: DWORD;
begin
  Size := Size div 8;
  CopyPtr(buff, @xKeyD, Size);
  xKeyLen := Size div 4;
  if (Size mod 4) <> 0 then
      inc(xKeyLen);
  CopyPtr(@cRC6_sBox, @KeyContext, ((cRC6_NumRounds * 2) + 4) * 4);
  i := 0;
  j := 0;
  a := 0;
  b := 0;
  if xKeyLen > ((cRC6_NumRounds * 2) + 4) then
      k := xKeyLen * 3
  else
      k := ((cRC6_NumRounds * 2) + 4) * 3;

  for k := 1 to k do
    begin
      a := LRot32(KeyContext[i] + a + b, 3);
      KeyContext[i] := a;
      b := LRot32(xKeyD[j] + a + b, a + b);
      xKeyD[j] := b;
      i := (i + 1) mod ((cRC6_NumRounds * 2) + 4);
      j := (j + 1) mod xKeyLen;
    end;
end;

class procedure TRC6.Encrypt(var KeyContext: TRC6Key; var Data: TRC6Block);
var
  x0, x1, x2, x3: PDWORD;
  u, t, i: DWORD;
begin
  x0 := PDWORD(@Data[0]);
  x1 := PDWORD(@Data[4]);
  x2 := PDWORD(@Data[8]);
  x3 := PDWORD(@Data[12]);

  x1^ := x1^ + KeyContext[0];
  x3^ := x3^ + KeyContext[1];
  for i := 1 to cRC6_NumRounds do
    begin
      t := LRot32(x1^ * (2 * x1^ + 1), 5);
      u := LRot32(x3^ * (2 * x3^ + 1), 5);
      x0^ := LRot32(x0^ xor t, u) + KeyContext[2 * i];
      x2^ := LRot32(x2^ xor u, t) + KeyContext[2 * i + 1];
      t := x0^;
      x0^ := x1^;
      x1^ := x2^;
      x2^ := x3^;
      x3^ := t;
    end;
  x0^ := x0^ + KeyContext[(2 * cRC6_NumRounds) + 2];
  x2^ := x2^ + KeyContext[(2 * cRC6_NumRounds) + 3];
end;

class procedure TRC6.Decrypt(var KeyContext: TRC6Key; var Data: TRC6Block);
var
  x0, x1, x2, x3: PDWORD;
  u, t, i: DWORD;
begin
  x0 := PDWORD(@Data[0]);
  x1 := PDWORD(@Data[4]);
  x2 := PDWORD(@Data[8]);
  x3 := PDWORD(@Data[12]);

  x2^ := x2^ - KeyContext[(2 * cRC6_NumRounds) + 3];
  x0^ := x0^ - KeyContext[(2 * cRC6_NumRounds) + 2];
  for i := cRC6_NumRounds downto 1 do
    begin
      t := x0^;
      x0^ := x3^;
      x3^ := x2^;
      x2^ := x1^;
      x1^ := t;
      u := LRot32(x3^ * (2 * x3^ + 1), 5);
      t := LRot32(x1^ * (2 * x1^ + 1), 5);
      x2^ := RRot32(x2^ - KeyContext[2 * i + 1], t) xor u;
      x0^ := RRot32(x0^ - KeyContext[2 * i], u) xor t;
    end;
  x3^ := x3^ - KeyContext[1];
  x1^ := x1^ - KeyContext[0];
end;

initialization

InitSysCBCAndDefaultKey(Int64($F0F0F0F0F0F00F0F));
DCP_towfish_Precomp;

finalization

SetLength(SystemCBC, 0);

end.
