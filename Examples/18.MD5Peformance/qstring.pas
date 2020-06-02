unit qstring;
{$INCLUDE 'qdac.inc'}

interface

{$REGION 'History'}
{
  本源码来自QDAC项目，版权归swish(QQ:109867294)所有。
  (1)、使用许可及限制
  您可以自由复制、分发、修改本源码，但您的修改应该反馈给作者，并允许作者在必要时，
  合并到本项目中以供使用，合并后的源码同样遵循QDAC版权声明限制。
  您的产品的关于中，应包含以下的版本声明:
  本产品使用的JSON解析器来自QDAC项目中的QJSON，版权归作者所有。
  (2)、技术支持
  有技术问题，您可以加入QDAC官方QQ群250530692共同探讨。
  (3)、赞助
  您可以自由使用本源码而不需要支付任何费用。如果您觉得本源码对您有帮助，您可以赞
  助本项目（非强制），以使作者不为生活所迫，有更多的精力为您呈现更好的作品：
  赞助方式：
  支付宝： guansonghuan@sina.com 姓名：管耸寰
  建设银行：
  户名：管耸寰
  账号：4367 4209 4324 0179 731
  开户行：建设银行长春团风储蓄所
}

{ 修订日志
  2017.1.23
  ==========
  * 修正了 UrlEncode编码时没有对特殊转义字符计算空间的问题
  + 增加 UrlDecode 函数来解析Url的各个组成部分

  2016.11.24
  ==========
  * 优化了函数 HTMLUnescape 的效率

  2016.11.12
  ==========
  + DeleteSideCharsW 用于删除两边的特定字符

  2016.9.29
  ==========
  - 删除 RightStrCountW 函数
  + 增加 RightPosW 函数，来计算字符串出现在右侧的起始位置（阿木建议）

  2016.9.21
  ==========
  * 修正了 HashOf 在计算长度为0的数据时出现AV错误的问题（QQ报告）

  2016.8.23
  ==========
  * 修正了DecodeChineseId身份证号中包含小写 x 时检查出错的问题
  + 增加 JavaEscape 和 JavaUnescape 来转义/反转 Java 字符串
  + 增加 IsOctChar 来检查指定的字符是否是8进制允许的字符

  2016.7.16
  ==========
  + 增加 FreeAndNilObject 函数来替代 FreeAndNil

  2016.6.13
  ==========
  + 增加 MemComp 函数用于比较两块内存块的值，不同于 BinaryCmp，它不要求两个内存块等长

  2016.6.3
  ==========
  + 增加 IsHumanName/IsChineseName/IsNoChineseName/IsChineseAddr/IsNoChineseAddr 函数

  2016.5.27
  ==========
  * 修正了身份证号检查在移动平台中由于字符串索引调整而引起的问题

  2016.5.24
  ==========
  * 修正了TQStringCatHelperW.SetDest 设置地址时未处理结束边界的问题

  2016.3.21
  ==========
  * 修正了 StrIStrA 算法错误（阿木报告）
  + 加入了 PosA 函数（阿木建议）
  + QStringA 加入 UpperCase/LowerCase函数
  2016.2.26
  ==========
  + 增加 IsEmailAddr 来检查电子邮箱格式
  + 增加IsChineseMobile 来检查手机号码格式

  2016.2.25
  ==========
  + 增加 IsChineseIdNo 函数来判断身份证号的有效性（规则验证，非联网验证）
  + 增加 DecodeChineseId 来解析身份证号的各个组成部分内容

  2015.11.22
  ==========
  * 修正了PosW 函数 AStartPos 返回时没有考虑和一个重载丢失AStartPos参数的问题（阿木报告）

  2015.11.18
  ==========
  * 修正了MemScan忘记减小len_s计数的问题（TTT报告）
  2015.11.10
  ==========
  + 新增货币金额汉语大写函数 CapMoney
  + 新增简繁转换函数 SimpleChineseToTraditional 和 TraditionalChineseToSimple

  2015.9.26
  =========
  * 修正了ParseDateTime在解析 "n-nxxx" 这样的内容时错误返回true的问题（马铃薯报告）
  2015.9.2
  =========
  * 修正了 AnsiEncode 在 2007 上由于编译器的Bug而出错的问题（星空报告）

  2015.8.28
  =========
  * 合并 InternalInsert 的代码到 Insert
  * 修正了 Clear 后没有收缩时，重新插入元素时页面异常增长的问题（冰晰空气报告）
  * 修正了 Pack 函数在 Clear 后收缩时出错的问题（蝈蝈报告）
  + 加入了批量插入模式的 Insert 函数重载
  + 加入批量添加模式的 Add 函数重载
  2015.8.27
  =========
  * 修正了 TQPagedList.Pack 方法收缩结果不正确的问题（冰晰空气报告）

  2015.8.26
  =========
  * 修正了 TQPagedList.Insert 插入位置小于0时出错的问题(冰晰空气报告）
  * 优化了 TQPagedList.Add 的代码，改成直接调用InternalInsert(冰晰空气建议）

  2015.8.11
  =========
  * 修正了 HTMLUnescape 反转义特殊字符时出错的问题（不得闲报告）
  2015.7.15
  =========
  * 根据 qsl 的哈希测试结果，将 HashOf 函数算法改为 BKDR 并在Windows平台下使用汇编
  以提升效率（感谢qsl）
  2015.6.6
  =========
  * 修正了 Utf8Decode 时对0x10000+范围的字符解码时移位错误（感谢qsl）
  2015.5.29
  =========
  * 修正了 TQPagedStream 在低版本的IDE中无法编译的问题

  2015.5.25
  =========
  + TQPagedStream 加入，以在写入大内存流数据时替代TMemoryStream

  2015.5.22
  =========
  * 修正了 ParseNumeric 和 ParseInt 时，对溢出表示范围的数值处理方式
  2015.5.21
  =========
  * 修正了 StringReplaceWithW 在 AWithTag 为 True 时结果不正确的问题（麦子仲肥报告）
  * 修正了 StringReplaceWithW 在替换的结果字符串为空时，访问无效地址的问题（麦子仲肥报告）
  2015.5.20
  =========
  + StringReplaceW 函数的一个重载，用于替换字符串中的某一部分为特定字符，一般用于部分敏感内容的隐藏
  * 移除一个Hint
  2015.5.8
  =========
  * 修改 TQPtr 的实现，将匿名释放事件和普通的事件使用同一个变量
  + DecodeText 函数从内存中直接检测编码并返回Unicode编码的字符串

  2015.4.17
  =========
  * 优化了UTFEncode是占用内存过多的问题
  2015.4.8
  =========
  * 修正了ParseNumeric 在解析 -0.xx 这样的数字符号出错的问题（感谢YZ报告）
  2015.4.1
  =========
  * 修正了TQStringCatHelper记算需要的缓冲区长度时判断错误的问题（感谢etarelecca报告）
  2015.3.9
  =========
  * 修改 NaturalCompareW 函数，增加是否忽略掉空白字符选项，在忽略时A 10 和 A10 将被视为一致的结果
  2015.3.5
  =========
  + 新增 PosW（等价于系统的Pos） 和按自然规则排序函数 NaturalCompareW
  2015.2.9
  =========
  + 新增 FilterCharW 和 FilterNoNumberW 两个函数

  2015.1.27
  =========
  * 修正了TQPtr.Bind 几个函数由于代码没有保存的问题

  2015.1.26
  ==========
  + TQPtr 新增几个 Bind 函数的重载
  + 加入全局变量 IsFMXApp 来检测当前是否是 FMX 类应用程序

  2014.11.10
  =========
  * 修正了XE3编译时TSystemTimes定义无效的问题

  2014.11.5
  =========
  * QStringA的From函数修改返回值类型并加入一个重载
  + QStringA加入Cat函数
  + CharCodeA/CharCodeU/CharCodeW来获得指定位置的字符编码值

  2014.9.26
  =========
  * 将TThreadId类型定义由QWorker移入本单元
  2014.9.11
  =========
  * 修正了LoadTextA/LoadTextW加载带有BOM头的空的Utf8流时出错的问题
  2014.8.20
  =========
  + StringReplaceWithW函数，用于替换一块标签中的内容（天地弦）
  2014.8.15
  =========
  * 清理并验证了TQBytesCatHelper引起的2007编译无法通过的问题(秋风起逸以凉报告并验证)
  + PQStringA类型定义

  2014.8.14
  =========
  * 修正了TQBytesCatHelper.NeedSize函数在Delphi2007下无法编译的问题(音儿小白报告并提供修改)
  2014.8.5
  ========
  * BinToHex加入ALowerCase参数，以支持使用小写的十六进制表示方式
  2014.8.1
  =========
  + 新增函数SameCharsA/U/W计算相同的字符数，EndWithA/U/W判断是符以指定的字符串结尾
  2014.7.17
  =========
  + 新增BinaryCmp函数，用于等价于C中的memcmp函数
  2014.7.16
  =========
  + 新增MemScan函数用于在指定的内存区域中查找指定的字节序列
  2014.7.12
  =========
  * 修正了DecodeLineU中递归调用自己的错误(音儿小白报告)
  * 修正了CharCountU检查字符宽度时对双字节Utf8编码的检测错误
  2014.7.10
  =========
  + 新增以下函数：StringReplicateW,NameOfW,ValueOfW,IndexOfNameW,IndexOfValueW

  2014.6.26
  =========
  * 加入HPPEMIT默认链接本单元(麦子仲肥 建议)
  2014.6.21
  ==========
  * 修正了C++ Builder中编译的问题
  2014.6.19
  ==========
  * 修正了QuotedStr对于长度为0的字符串编码出错的问题
}
{$ENDREGION 'History'}

uses Classes, SysUtils, Types{$IF RTLVersion>=21},
  Rtti{$IFEND >=XE10}{$IFNDEF MSWINDOWS},
  SyncObjs{$ENDIF}
{$IFDEF MSWINDOWS}
    , Windows
{$ENDIF}
{$IFDEF POSIX}
    , Posix.String_
{$ENDIF}
{$IFDEF ANDROID}
    , Androidapi.Log
{$ENDIF}
{$IFDEF IOS}
    , iOSApi.Foundation, Macapi.Helpers, Macapi.ObjectiveC
{$ENDIF}
    ;
{$HPPEMIT '#pragma link "qstring"'}
{$HPPEMIT 'template<class T>'}
{$HPPEMIT 'bool __fastcall HasInterface(IUnknown *AIntf, DelphiInterface<T> &AResult)'}
{$HPPEMIT '{'}
{$HPPEMIT 'T *ATemp;'}
{$HPPEMIT 'if (AIntf&&(AIntf->QueryInterface(__uuidof(T), (void **)&ATemp) == S_OK))'}
{$HPPEMIT '{'}
{$HPPEMIT '  AResult = ATemp;'}
{$HPPEMIT '  ATemp->Release();'}
{$HPPEMIT '  return true;'}
(*$HPPEMIT '}'*)
{$HPPEMIT 'return false;'}
(*$HPPEMIT '}'*)
{$HPPEMIT 'template < class T > bool __fastcall HasInterface(TObject * AObject, DelphiInterface<T> &AResult)'}
{$HPPEMIT '{'}
{$HPPEMIT 'T *ATemp = NULL;'}
{$HPPEMIT 'if (AObject&&(AObject->GetInterface(__uuidof(T), &ATemp)))'}
{$HPPEMIT '{'}
{$HPPEMIT '  AResult = ATemp;'}
{$HPPEMIT '  ATemp->Release();'}
{$HPPEMIT '  return true;'}
(*$HPPEMIT '}'*)
{$HPPEMIT 'return false;'}
(*$HPPEMIT '}'*)
{$HPPEMIT 'template<class T>'}
{$HPPEMIT 'DelphiInterface<T> __fastcall ToInterface(TObject *AObject)'}
{$HPPEMIT '{'}
{$HPPEMIT 'DelphiInterface<T> ARetVal;'}
{$HPPEMIT 'HasInterface(AObject, ARetVal);'}
{$HPPEMIT 'return ARetVal;'}
(*$HPPEMIT '}'*)
{$HPPEMIT 'template<class T>'}
{$HPPEMIT 'DelphiInterface<T> __fastcall ToInterface(IUnknown *AIntf)'}
{$HPPEMIT '{'}
{$HPPEMIT 'DelphiInterface<T> ARetVal;'}
{$HPPEMIT 'HasInterface(AIntf, ARetVal);'}
{$HPPEMIT 'return ARetVal;'}
(*$HPPEMIT '}'*)

const
  MC_NUM = $01; // 显示数字
  MC_UNIT = $02; // 显示单位
  MC_HIDE_ZERO = $04; // 隐藏左侧零值
  MC_MERGE_ZERO = $08; // 合并中间的零值
  MC_END_PATCH = $10; // 仅在不以分结束时加入AEndText指定的值
  MC_READ = MC_NUM or MC_UNIT or MC_HIDE_ZERO or MC_MERGE_ZERO or MC_END_PATCH;
  // 正常读取形式
  MC_PRINT = MC_NUM or MC_HIDE_ZERO; // 正常套打形式

type
{$IFDEF UNICODE}
  QStringW = UnicodeString;
{$ELSE}
  QStringW = WideString;
{$ENDIF UNICODE}
{$IF RTLVersion>=21}
  TValueArray = array of TValue;
{$IFEND >=2010}
{$IF RTLVersion<25}
  IntPtr = Integer;
  IntUPtr = Cardinal;
  UIntPtr = Cardinal;
{$IFEND IntPtr}
  // {$IF RTLVersion<=18}
  // DWORD_PTR = DWORD;
  // ULONGLONG = Int64;
  // TBytes = array of Byte;
  // PPointer = ^Pointer;
  // {$IFEND}
{$IF RTLVersion<22}
  TThreadId = Longword;
{$IFEND}
  PIntPtr = ^IntPtr;
  QCharA = Byte;
  QCharW = WideChar;
  PQCharA = ^QCharA;
  PPQCharA = ^PQCharA;
  PQStringA = ^QStringA;
  PQCharW = PWideChar;
  PPQCharW = ^PQCharW;
  PQStringW = ^QStringW;
  TTextEncoding = (teUnknown, { 未知的编码 }
    teAuto, { 自动检测 }
    teAnsi, { Ansi编码 }
    teUnicode16LE, { Unicode LE 编码 }
    teUnicode16BE, { Unicode BE 编码 }
    teUTF8 { UTF8编码 }
    );
{$HPPEMIT '#define DELPHI_ANON(AType,Code,AVar) \'}
{$HPPEMIT '  class AType##AVar:public TCppInterfacedObject<AType>\'}
(*$HPPEMIT '  {\'*)
{$HPPEMIT '  public:\'}
{$HPPEMIT '    void __fastcall Invoke##Code\'}
(*$HPPEMIT '  } *AVar=new AType##AVar'*)

  // 从A结尾的为Ansi编码支持的函数，以U结尾的是Utf8编码支持的函数，以W结尾的为UCS2
  QStringA = record
  private
    FValue: TBytes;
    function GetChars(AIndex: Integer): QCharA;
    procedure SetChars(AIndex: Integer; const Value: QCharA);
    function GetLength: Integer;
    procedure SetLength(const Value: Integer);
    function GetIsUtf8: Boolean;
    function GetData: PByte;
    procedure SetIsUtf8(const Value: Boolean);
  public
    class operator Implicit(const S: QStringW): QStringA;
    class operator Implicit(const S: QStringA): PQCharA;
    class operator Implicit(const S: QStringA): TBytes;
    class operator Implicit(const ABytes: TBytes): QStringA;
    class operator Implicit(const S: QStringA): QStringW;
    class operator Implicit(const S: PQCharA): QStringA;
{$IFNDEF NEXTGEN}
    class operator Implicit(const S: AnsiString): QStringA;
    class operator Implicit(const S: QStringA): AnsiString;
{$ENDIF}
    class function UpperCase(S: PQCharA): QStringA; overload; static;
    class function LowerCase(S: PQCharA): QStringA; overload; static;
    function UpperCase: QStringA; overload;
    function LowerCase: QStringA; overload;
    // 字符串比较
    function From(p: PQCharA; AOffset, ALen: Integer): PQStringA; overload;
    function From(const S: QStringA; AOffset: Integer = 0): PQStringA; overload;
    function Cat(p: PQCharA; ALen: Integer): PQStringA; overload;
    function Cat(const S: QStringA): PQStringA; overload;
    property Chars[AIndex: Integer]: QCharA read GetChars
      write SetChars; default;
    property Length: Integer read GetLength write SetLength;
    property IsUtf8: Boolean read GetIsUtf8 write SetIsUtf8;
    property Data: PByte read GetData;
  end;

  TQSingleton{$IFDEF UNICODE}<T: class>{$ENDIF} = record
    InitToNull: {$IFDEF UNICODE}T{$ELSE}Pointer{$ENDIF};

  type
{$IFDEF UNICODE}
    TGetInstanceCallback = reference to function: T;
{$ELSE}
    TGetInstanceCallback = function: Pointer;
{$ENDIF}
  function Instance(ACallback: TGetInstanceCallback):
{$IFDEF UNICODE}T{$ELSE}Pointer{$ENDIF};
  end;

  QException = class(Exception)

  end;

  // 字符串拼接类
  TQStringCatHelperW = class
  private
    FValue: array of QCharW;
    FStart, FDest, FLast: PQCharW;
    FBlockSize: Integer;
{$IFDEF DEBUG}
    FAllocTimes: Integer;
{$ENDIF}
    FSize: Integer;
    function GetValue: QStringW;
    function GetPosition: Integer; inline;
    procedure SetPosition(const Value: Integer);
    procedure NeedSize(ASize: Integer);
    function GetChars(AIndex: Integer): QCharW;
    function GetIsEmpty: Boolean; inline;
    procedure SetDest(const Value: PQCharW);
  public
    constructor Create; overload;
    constructor Create(ASize: Integer); overload;
    procedure LoadFromFile(const AFileName: QStringW);
    procedure LoadFromStream(const AStream: TStream);
    procedure IncSize(ADelta: Integer);
    function Cat(p: PQCharW; len: Integer): TQStringCatHelperW; overload;
    function Cat(const S: QStringW): TQStringCatHelperW; overload;
    function Cat(c: QCharW): TQStringCatHelperW; overload;
    function Cat(const V: Int64): TQStringCatHelperW; overload;
    function Cat(const V: Double): TQStringCatHelperW; overload;
    function Cat(const V: Boolean): TQStringCatHelperW; overload;
    function Cat(const V: Currency): TQStringCatHelperW; overload;
    function Cat(const V: TGuid): TQStringCatHelperW; overload;
    function Cat(const V: Variant): TQStringCatHelperW; overload;
    function Replicate(const S: QStringW; count: Integer): TQStringCatHelperW;
    function Back(ALen: Integer): TQStringCatHelperW;
    function BackIf(const S: PQCharW): TQStringCatHelperW;
    procedure TrimRight;
    procedure Reset;
    function EndWith(const S: String; AIgnoreCase: Boolean): Boolean;
    property Value: QStringW read GetValue;
    property Chars[AIndex: Integer]: QCharW read GetChars;
    property Start: PQCharW read FStart;
    property Current: PQCharW read FDest write SetDest;
    property Last: PQCharW read FLast;
    property Position: Integer read GetPosition write SetPosition;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TQBytesCatHelper = class
  private
    FValue: TBytes;
    FStart, FDest: PByte;
    FBlockSize: Integer;
    FSize: Integer;
    function GetBytes(AIndex: Integer): Byte;
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    procedure NeedSize(ASize: Integer);
    procedure SetCapacity(const Value: Integer);
    function GetValue: TBytes;
  public
    constructor Create; overload;
    constructor Create(ASize: Integer); overload;
    function Cat(const V: Byte): TQBytesCatHelper; overload;
    function Cat(const V: Shortint): TQBytesCatHelper; overload;
    function Cat(const V: Word): TQBytesCatHelper; overload;
    function Cat(const V: Smallint): TQBytesCatHelper; overload;
    function Cat(const V: Cardinal): TQBytesCatHelper; overload;
    function Cat(const V: Integer): TQBytesCatHelper; overload;
    function Cat(const V: Int64): TQBytesCatHelper; overload;
{$IFNDEF NEXTGEN}
    function Cat(const V: AnsiChar): TQBytesCatHelper; overload;
    function Cat(const V: AnsiString): TQBytesCatHelper; overload;
{$ENDIF}
    function Cat(const V: QStringA; ACStyle: Boolean = false)
      : TQBytesCatHelper; overload;
    function Cat(const c: QCharW): TQBytesCatHelper; overload;
    function Cat(const S: QStringW): TQBytesCatHelper; overload;
    function Cat(const ABytes: TBytes): TQBytesCatHelper; overload;
    function Cat(const AData: Pointer; const ALen: Integer)
      : TQBytesCatHelper; overload;
    function Cat(const V: Single): TQBytesCatHelper; overload;
    function Cat(const V: Double): TQBytesCatHelper; overload;
    function Cat(const V: Boolean): TQBytesCatHelper; overload;
    function Cat(const V: Currency): TQBytesCatHelper; overload;
    function Cat(const V: TGuid): TQBytesCatHelper; overload;
    function Cat(const V: Variant): TQBytesCatHelper; overload;
    function Replicate(const ABytes: TBytes; ACount: Integer): TQBytesCatHelper;
    function Back(ALen: Integer): TQBytesCatHelper;
    function Insert(AIndex: Cardinal; const AData: Pointer; const ALen: Integer)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Byte): TQBytesCatHelper;
      overload;
    function Insert(AIndex: Cardinal; const V: Shortint)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Word): TQBytesCatHelper;
      overload;
    function Insert(AIndex: Cardinal; const V: Smallint)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Cardinal)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Integer)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Int64)
      : TQBytesCatHelper; overload;
{$IFNDEF NEXTGEN}
    function Insert(AIndex: Cardinal; const V: AnsiChar)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: AnsiString)
      : TQBytesCatHelper; overload;
{$ENDIF}
    function Insert(AIndex: Cardinal; const V: QStringA;
      ACStyle: Boolean = false): TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const c: QCharW)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const S: QStringW)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const ABytes: TBytes)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Single)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Double)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Boolean)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Currency)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: TGuid)
      : TQBytesCatHelper; overload;
    function Insert(AIndex: Cardinal; const V: Variant)
      : TQBytesCatHelper; overload;
    function Delete(AStart, ACount: Cardinal): TQBytesCatHelper;
    procedure Reset;
    property Value: TBytes read GetValue;
    property Bytes[AIndex: Integer]: Byte read GetBytes;
    property Start: PByte read FStart;
    property Current: PByte read FDest;
    property Position: Integer read GetPosition write SetPosition;
    property Capacity: Integer read FSize write SetCapacity;
  end;

  IQPtr = interface(IInterface)
    ['{E5B28F92-CA81-4C01-8766-59FBF4E95F05}']
    function Get: Pointer;
  end;

  TQPtrFreeEvent = procedure(AData: Pointer) of object;
  PQPtrFreeEvent = ^TQPtrFreeEvent;
  TQPtrFreeEventG = procedure(AData: Pointer);
{$IFDEF UNICODE}
  TQPtrFreeEventA = reference to procedure(AData: Pointer);
  PQPtrFreeEventA = ^TQPtrFreeEventA;
{$ENDIF}

  TQPtrFreeEvents = record
    case Integer of
      0:
        (Method: TMethod);
      1:
        (OnFree: {$IFNDEF NEXTGEN}TQPtrFreeEvent{$ELSE}Pointer{$ENDIF});
      2:
        (OnFreeG: TQPtrFreeEventG);
      3:
        (OnFreeA: Pointer);
  end;

  TQPtr = class(TInterfacedObject, IQPtr)
  private
    FObject: Pointer;
    FOnFree: TQPtrFreeEvents;
  public
    constructor Create(AObject: Pointer); overload;
    destructor Destroy; override;
    class function Bind(AObject: TObject): IQPtr; overload;
    class function Bind(AData: Pointer; AOnFree: TQPtrFreeEvent)
      : IQPtr; overload;
    class function Bind(AData: Pointer; AOnFree: TQPtrFreeEventG)
      : IQPtr; overload;
{$IFDEF UNICODE}
    class function Bind(AData: Pointer; AOnFree: TQPtrFreeEventA)
      : IQPtr; overload;
{$ENDIF}
    function Get: Pointer;
  end;
{$IF RTLVersion<=23}

  TDirection = (FromBeginning, FromEnd);
  TPointerList = array of Pointer;
{$ELSE}
  // TDirection = System.Types.TDirection;
{$IFEND}

  // TQPagedList - 分页式列表，将来用于记录列表
  TQListPage = class
  protected
    FStartIndex: Integer; // 起始索引
    FUsedCount: Integer; // 使用的页面计数
    FItems: array of Pointer;
  public
    constructor Create(APageSize: Integer);
    destructor Destroy; override;
  end;

  TQPagedListSortCompare = procedure(p1, p2: Pointer; var AResult: Integer)
    of object;
  TQPagedListSortCompareG = procedure(p1, p2: Pointer; var AResult: Integer);
{$IFDEF UNICODE}
  TQPagedListSortCompareA = reference to procedure(p1, p2: Pointer;
    var AResult: Integer);
  PQPagedListSortCompareA = ^TQPagedListSortCompareA;
{$ENDIF}
  TQPagedList = class;

  TQPagedListEnumerator = class
  private
    FIndex: Integer;
    FList: TQPagedList;
  public
    constructor Create(AList: TQPagedList);
    function GetCurrent: Pointer; inline;
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;

  TQPagedList = class
  protected
    FPages: array of TQListPage; // 页面列表
    FPageSize: Integer; // 每页大小
    FCount: Integer; // 数量
    FLastUsedPage: Integer; // 最后一个正在使用的页面索引(基于0)
    FFirstDirtyPage: Integer; // 首个索引脏的页面
    FOnCompare: TQPagedListSortCompare; // 内容比较函数
    /// <summary> 比较两个指针对应的内容大小 </summary>
    /// <param name="p1">第一个指针</param>
    /// <param name="p2">第二个指针</param>
    /// <returns>返回内容的比较结果,&lt;0代表p1的内容小于p2,相等则为0，反之则&gt;0</returns>
    /// <remarks>仅在排序和查找时使用</remarks>
    function DoCompare(p1, p2: Pointer): Integer; inline;
    /// <summary>获取指定索引的值</summary>
    /// <param name="AIndex">要获取的元素索引</param>
    /// <returns>返回指到的指针，如果索引越界，则抛出异常</returns>
    /// <remarks>属性Items的读函数</remarks>
    function GetItems(AIndex: Integer): Pointer; inline;
    /// <summary>设置指定索引的值</summary>
    /// <param name="AIndex">目标索引位置，如果越界，则抛出异常</param>
    /// <remarks>属性Items的写函数</remarks>
    procedure SetItems(AIndex: Integer; const Value: Pointer); inline;
    /// <summary>删除通知触发函数</summary>
    /// <param name="p">要删除的指针</param>
    /// <remarks>仅在TQPagedList子类才会触发Notify函数</remarks>
    procedure DoDelete(const p: Pointer); inline;
    /// <summary>查找指定索引所在的页索引</summary>
    /// <param name="p">目标索引位置</param>
    /// <returns>返回找到的页号</returns>
    function FindPage(AIndex: Integer): Integer;
    /// <summary>查找指定索引所在的页</summary>
    /// <param name="p">目标索引位置</param>
    /// <returns>返回找到的页对象</returns>
    function GetPage(AIndex: Integer): TQListPage;
    /// <summary>标记从指定的页开始，页头的FStartIndex信息失效</summary>
    /// <param name="APage">失效的页面索引</param>
    /// <remarks>使用失效的页（脏页）内容时，需要先更新脏页索引信息</remarks>
    procedure Dirty(APage: Integer); inline;
    /// <summary>通知指定的指针发生变化</summary>
    /// <param name="Ptr">发生变化的指针数据</param>
    /// <param name="Action">发生的变化类型</param>
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    /// <summary>获取当前列表的容量</summary>
    /// <returns>返回 PageSize*PageCount 的结果</returns>
    /// <remarks>属性Capacity的读函数</remarks>
    function GetCapacity: Integer;
    /// <summary>不做任何事，仅为兼容TList的形式而提供</summary>
    /// <remarks>属性Capacity的写函数</remarks>
    procedure SetCapacity(const Value: Integer);
    /// <summary>将当前所有内容放到一维的指针数组中</summary>
    /// <returns>返回生成的一维动态数组</returns>
    /// <remarks>属性List的读函数</remarks>
    function GetList: TPointerList;
    /// <summary>设置内容大小比较函数</summary>
    /// <param name="Value">新的比较函数</summary>
    /// <remarks>属性OnCompare的写函数，修改它可能触发重新排序</remarks>
    procedure SetOnCompare(const Value: TQPagedListSortCompare);
    /// <summary>在删除或移除元素时，检查最后使用的分页索引</summary>
    procedure CheckLastPage;
    /// <summary>获取当前已经分配的总页数</summary>
    function GetPageCount: Integer;
  public
    /// <summary>
    /// 默认构造函数，由于未指定页面大小，使用默认大小512
    /// </summary>
    constructor Create; overload;
    /// <summary>构造函数</summary>
    /// <param name="APageSize">每页项数，如果小于等于0则使用默认值</param>
    constructor Create(APageSize: Integer); overload;
    /// <summary>析构函数</summary>
    destructor Destroy; override;
{$IF RTLVersion<26}
    /// <summary>拷贝函数，参考TList.Assign</summary>
    procedure Assign(ListA: TList; AOperator: TListAssignOp = laCopy;
      ListB: TList = nil); overload;
{$IFEND}
    /// <summary>拷贝函数，参考TList.Assign</summary>
    procedure Assign(ListA: TQPagedList; AOperator: TListAssignOp = laCopy;
      ListB: TQPagedList = nil); overload;
    /// <summary>添加一个元素</summary>
    /// <param name="p">要添加的元素</param>
    /// <returns>返回新元素的索引值</returns>
    function Add(const p: Pointer): Integer; overload;
    /// <summary>一次性添加多个元素</summary>
    /// <param name="pp">要添加的元素列表指针</param>
    /// <param name="ACount">要添加的元素个数</param>
    /// <returns>返回新元素的索引值</returns>
    procedure BatchAdd(pp: PPointer; ACount: Integer); overload;
    /// <summary>一次性添加多个元素</summary>
    /// <param name="AList">要添加的元素动态数组</param>
    /// <returns>返回新元素的索引值</returns>
    procedure BatchAdd(AList: TPointerList); overload;
    /// <summary>在指定的位置插入一个新元素</summary>
    /// <param name="AIndex">要插入的位置，如果小于等于0，插入起始位置，如果大于等于Count，则插入末尾</param>
    /// <param name="p">要插入的元素</param>
    /// <remarks>如果指定的排序规则，则AIndex参数被忽略</remarks>
    procedure Insert(AIndex: Integer; const p: Pointer); overload;
    /// <summary>在指定的位置批量插入多个新元素</summary>
    /// <param name="AIndex">要插入的位置，如果小于等于0，插入起始位置，如果大于等于Count，则插入末尾</param>
    /// <param name="pp">要插入的元素</param>
    /// <param name="ACount">pp参数指向的元素个数</param>
    /// <remarks>如果指定的排序规则，则AIndex参数被忽略</remarks>
    procedure BatchInsert(AIndex: Integer; pp: PPointer;
      ACount: Integer); overload;
    /// <summary>在指定的位置插入一个新元素</summary>
    /// <param name="AIndex">要插入的位置，如果小于等于0，插入起始位置，如果大于等于Count，则插入末尾</param>
    /// <param name="p">要插入的元素</param>
    /// <remarks>如果指定的排序规则，则AIndex参数被忽略</remarks>
    procedure BatchInsert(AIndex: Integer; const AList: TPointerList); overload;
    /// <summary>交换两个元素的位置</summary>
    /// <param name="AIndex1">第一个元素的位置索引</param>
    /// <param name="AIndex2">第二个元素的位置索引</param>
    procedure Exchange(AIndex1, AIndex2: Integer);
    /// <summary>将指定位置的元素移动到新位置</summary>
    /// <param name="AFrom">起始位置索引</param>
    /// <param name="ATo">目标位置索引</param>
    procedure MoveTo(AFrom, ATo: Integer);
    /// <summary>实际直接调用MoveTo</summary>
    procedure Move(AFrom, ATo: Integer); inline;
    /// <summary>删除指定的元素</summary>
    procedure Delete(AIndex: Integer);
    /// <summary>移除指定的元素</summary>
    procedure Remove(AIndex: Integer); overload;
    /// <summary>移除指定的元素</summary>
    function Remove(Item: Pointer): Integer; overload; inline;
    /// <summary>从指定的方向开始查找并移除元素</summary>
    function RemoveItem(Item: Pointer; Direction: TDirection): Integer;
    /// <summary>查找指定元素的索引</summary>
    /// <param name="p">要查找的元素</param>
    /// <param name="AIdx">找到的元素索引</param>
    /// <returns>如果找到，返回True,否则返回False，AIdx为目标应出现的位置</returns>
    function Find(const p: Pointer; var AIdx: Integer): Boolean;
    /// <summary>清除所有元素</summary>
    /// <remarks>Clear并不收缩页以便后面重用，要收缩页，请调用Pack函数</remarks>
    procedure Clear;
    /// <summary>收缩列表为最少的页数</summary>
    procedure Pack;
    /// <summary>按照OnCompare规定的规则排序</summary>
    /// <remarks>一旦指定OnCompare规则，则添加元素时会自动排序，Insert时指定的索引位置将会被忽略</remarks>
    procedure Sort; overload;
{$IFDEF UNICODE}
    /// <summary>按照AOnCompare参数规定的规则排序</summary>
    /// <remarks>一旦指定OnCompare规则，则添加元素时会自动排序，Insert时指定的索引位置将会被忽略</remarks>
    procedure Sort(AOnCompare: TQPagedListSortCompareA); overload;
{$ENDIF}
    /// <summary>按照AOnCompare参数规定的规则排序</summary>
    /// <remarks>一旦指定OnCompare规则，则添加元素时会自动排序，Insert时指定的索引位置将会被忽略</remarks>
    procedure Sort(AOnCompare: TQPagedListSortCompareG); overload;
    /// <summary> for .. in 支持</summary>
    function GetEnumerator: TQPagedListEnumerator;
    /// <summary>仅为兼容 TList 添加，对 TQPagedList 无意义</summary>
    function Expand: TQPagedList;
    /// <summary>移除指定的项目</summary>
    /// <param name="Item">要移除的值</param>
    function Extract(Item: Pointer): Pointer; inline;
    /// <summary>移除指定的项目</summary>
    /// <param name="Item">要移除的值</param>
    /// <param name="Direction">查找方向</param>
    function ExtractItem(Item: Pointer; Direction: TDirection): Pointer;
    /// <summary>首个元素</summary>
    function First: Pointer; inline;
    /// <summary>最后一个元素</summary>
    function Last: Pointer; inline;
    /// <summary>获取指定元素的首次出现位置</summary>
    /// <param name="Item">要查找的元素</param>
    function IndexOf(Item: Pointer): Integer;
    /// <summary>按指定的方向查找元素首次出现的位置</summary>
    /// <param name="Item">要查找的元素</param>
    /// <param name="Direction">查找方向</param>
    function IndexOfItem(Item: Pointer; Direction: TDirection): Integer;
    /// <summary>元素个数</summary>
    property count: Integer read FCount;
    /// <summary>元素列表</summary>
    property Items[AIndex: Integer]: Pointer read GetItems
      write SetItems; default;
    /// <summary>元素比较规则，如果指定，则元素自动排序</summary>
    property OnCompare: TQPagedListSortCompare read FOnCompare
      write SetOnCompare;
    /// <summary>兼容TList设置，对 TQPagedList 无意义</summary>
    property Capacity: Integer read GetCapacity write SetCapacity;
    /// <summary>获取所有的元素值数组</summary>
    property List: TPointerList read GetList;
    /// <summary>已经分配的页数</summary>
    property PageCount: Integer read GetPageCount;
    /// <summary>当前页大小</summary>
    property PageSize: Integer read FPageSize;
  end;

  /// <summary>
  /// 分页内存流对象，用于按页来存取数据，以优化内存的分配和释放次数，提高运行效率，使用方式同普通的内存流一样，但不提供Memory
  /// 指针（因为它就不是连续的内存块）。不过提供了AsBytes或Bytes[AIndex]的方式来访问指定的内容。
  /// </summary>
  TQPagedStream = class(TStream)
  private
    procedure SetCapacity(Value: Int64);
    function GetBytes(AIndex: Int64): Byte;
    procedure SetBytes(AIndex: Int64; const Value: Byte);
    procedure SetAsBytes(const Value: TBytes);
    function GetAsBytes: TBytes;
  protected
    FPages: array of PByte;
    FPageSize: Integer;
    FSize: Int64;
    FPosition: Int64;
    FCapacity: Int64;
    function ActivePage: Integer; inline;
    function ActiveOffset: Integer; inline;
    procedure PageNeeded(APageIndex: Integer);
    function GetSize: Int64; override;
  public
    constructor Create; overload;
    constructor Create(APageSize: Integer); overload;
    destructor Destroy; override;
    procedure Clear;
    function Read(var Buffer; count: Longint): Longint; overload; override;
    function Read(Buffer: TBytes; Offset, count: Longint): Longint;
{$IF RTLVersion>23} override{$ELSE}reintroduce;
    overload{$IFEND};
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; count: Longint): Longint; overload; override;
    function Write(const Buffer: TBytes; Offset, count: Longint): Longint;
{$IF RTLVersion>23} override{$ELSE}reintroduce;
    overload{$IFEND};
    property Capacity: Int64 read FCapacity write SetCapacity;
    property Bytes[AIndex: Int64]: Byte read GetBytes write SetBytes;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
  end;

  /// <summary>
  /// TQBits用来简化对标志位的访问，可以设置或获取某一位的状态
  /// </summary>
  TQBits = record
  private
    FBits: TBytes;
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function GetIsSet(AIndex: Integer): Boolean;
    procedure SetIsSet(AIndex: Integer; const Value: Boolean);
  public
    property Size: Integer read GetSize write SetSize;
    property IsSet[AIndex: Integer]: Boolean read GetIsSet
      write SetIsSet; default;
    property Bytes: TBytes read FBits write FBits;
  end;

  TQReadOnlyMemoryStream = class(TCustomMemoryStream)
  private
  protected
    constructor Create(); overload;
  public
    constructor Create(AData: Pointer; ASize: Integer); overload;
    function Write(const Buffer; count: Longint): Longint; override;
  end;

  TQFilterCharEvent = procedure(const AChar, AIndex: Cardinal;
    var Accept: Boolean; ATag: Pointer) of object;
{$IFDEF UNICODE}
  TQFilterCharEventA = reference to procedure(const c, AIndex: Cardinal;
    var Accept: Boolean; ATag: Pointer);
{$ENDIF}
  TQNumberType = (nftFloat, nftHexPrec, nftDelphiHex, nftCHex, nftBasicHex,
    nftNegative, nftPositive);
  TQNumberTypes = set of TQNumberType;

  TPasswordStrongLevel = (pslLowest, pslLower, pslNormal, pslHigher,
    pslHighest);
  TPasswordRule = (prIncNumber, prIncLowerCase, prIncUpperCase, prIncChart,
    prIncUnicode, prRepeat, prSimpleOrder);
  TPasswordRules = set of TPasswordRule;

  // 舍入方法：mrmNone - 不舍入，mrmSimple - 四舍五入,mrmBank - 银行家舍入法（四舍六入五成双）
  TMoneyRoundMethod = (mrmNone, mrmSimple, mrmBank);

  /// <summary>
  /// 名称字符类型
  /// </summary>
  TNameCharType = (
    /// <summary>
    /// 汉字
    /// </summary>
    nctChinese,
    /// <summary>
    /// 字母
    /// </summary>
    nctAlpha,
    /// <summary>
    /// 数字
    /// </summary>
    nctNum,
    /// <summary>
    /// 符号
    /// </summary>
    nctSymbol,
    /// <summary>
    /// 空格，注意不包含换行或Tab
    /// </summary>
    nctSpace,
    /// <summary>
    /// 姓名分隔符
    /// </summary>
    nctDot,
    /// <summary>
    /// 私有字符
    /// </summary>
    nctCustom,
    /// <summary>
    /// 其它
    /// </summary>
    nctOther);
  TNameCharSet = set of TNameCharType;

  /// <summary>
  /// 用于 IsHumanName 判断名称是否有效时，用户可以自定义判定特定字符是否允许的全局回调函数，AChar 为要检查的字符，Accept
  /// 为检查结果，AHandled 用于记录用户是否已经判定，如果已经判定，设置为true，默认为false，走默认判定。
  /// </summary>
  TQCustomNameCharTest = procedure(AChar: Cardinal;
    var Accept, AHandled: Boolean);

  /// <summary>
  /// 人类性别
  /// </summary>
  TQHumanSex = (
    /// <summary>
    /// 未知
    /// </summary>
    hsUnknown,
    /// <summary>
    /// 女性
    /// </summary>
    hsFemale,
    /// <summary>
    /// 男性
    /// </summary>
    hsMale);
  TMemCompFunction = function(p1, p2: Pointer; L1, L2: Integer): Integer;
  TCNSpellCallback = function(const p: PQCharW): QCharW;
  // UTF8编码与Unicode编码转换函数，使用自己的实现
function Utf8Decode(p: PQCharA; l: Integer): QStringW; overload;
function Utf8Decode(const p: QStringA): QStringW; overload;
function Utf8Encode(p: PQCharW; l: Integer): QStringA; overload;
function Utf8Encode(const p: QStringW): QStringA; overload;
function Utf8Encode(ps: PQCharW; sl: Integer; pd: PQCharA; dl: Integer)
  : Integer; overload;
// Ansi编码与Unicode编码转换函数，使用系统的TEncoding实现
function AnsiEncode(p: PQCharW; l: Integer): QStringA; overload;
function AnsiEncode(const p: QStringW): QStringA; overload;
function AnsiDecode(p: PQCharA; l: Integer): QStringW; overload;
function AnsiDecode(const p: QStringA): QStringW; overload;
// 取指定的字符串的中文拼音首字母
function CNSpellChars(S: QStringA; AIgnoreEnChars: Boolean): QStringW; overload;
function CNSpellChars(S: QStringW; AIgnoreEnChars: Boolean): QStringW; overload;

// 计算当前字符的长度
function CharSizeA(c: PQCharA): Integer;
function CharSizeU(c: PQCharA): Integer;
function CharSizeW(c: PQCharW): Integer;
// 计算字符数函数，CharCountW重写以包括UCS2扩展区字符处理
function CharCountA(const source: QStringA): Integer;
function CharCountW(const S: QStringW): Integer;
function CharCountU(const source: QStringA): Integer;
// 计算当前字符的Unicode编码
function CharCodeA(c: PQCharA): Cardinal;
function CharCodeU(c: PQCharA): Cardinal;
function CharCodeW(c: PQCharW): Cardinal;
// 检查字符是否在指定的列表中
function CharInA(const c: PQCharA; const List: array of QCharA;
  ACharLen: PInteger = nil): Boolean;
function CharInW(const c: PQCharW; const List: array of QCharW;
  ACharLen: PInteger = nil): Boolean; overload;
function CharInW(const c, List: PQCharW; ACharLen: PInteger = nil)
  : Boolean; overload;
function CharInU(const c: PQCharA; const List: array of QCharA;
  ACharLen: PInteger = nil): Boolean;

// 检查是否是空白字符
function IsSpaceA(const c: PQCharA; ASpaceSize: PInteger = nil): Boolean;
function IsSpaceW(const c: PQCharW; ASpaceSize: PInteger = nil): Boolean;
function IsSpaceU(const c: PQCharA; ASpaceSize: PInteger = nil): Boolean;

// 全角半角转换
function CNFullToHalf(const S: QStringW): QStringW;
function CNHalfToFull(const S: QStringW): QStringW;

// 引号处理
function QuotedStrA(const S: QStringA; const AQuoter: QCharA = $27): QStringA;
function QuotedStrW(const S: QStringW; const AQuoter: QCharW = #$27): QStringW;
function SQLQuoted(const S: QStringW): QStringW;
function DequotedStrA(const S: QStringA; const AQuoter: QCharA = $27): QStringA;
function DequotedStrW(const S: QStringW; const AQuoter: QCharW = #$27)
  : QStringW;

// 跳过列表中的字符
function SkipCharA(var p: PQCharA; const List: array of QCharA): Integer;
function SkipCharU(var p: PQCharA; const List: array of QCharA): Integer;
function SkipCharW(var p: PQCharW; const List: array of QCharA)
  : Integer; overload;
function SkipCharW(var p: PQCharW; const List: PQCharW): Integer; overload;

// 跳过空白字符，对于 Ansi编码，跳过的是#9#10#13#161#161，对于UCS编码，跳过的是#9#10#13#$3000
function SkipSpaceA(var p: PQCharA): Integer;
function SkipSpaceU(var p: PQCharA): Integer;
function SkipSpaceW(var p: PQCharW): Integer;
// 跳过一行,以#10为行结尾
function SkipLineA(var p: PQCharA): Integer;
function SkipLineU(var p: PQCharA): Integer;
function SkipLineW(var p: PQCharW): Integer;
// 跳过直接遇到指定的字符
function SkipUntilA(var p: PQCharA; AExpects: array of QCharA;
  AQuoter: QCharA = 0): Integer;
function SkipUntilU(var p: PQCharA; AExpects: array of QCharA;
  AQuoter: QCharA = 0): Integer;
function SkipUntilW(var p: PQCharW; AExpects: array of QCharW;
  AQuoter: QCharW = #0): Integer; overload;
function SkipUntilW(var p: PQCharW; AExpects: PQCharW; AQuoter: QCharW = #0)
  : Integer; overload;
// 查找字符所在行列号，返回行的起始地址
function StrPosA(Start, Current: PQCharA; var ACol, ARow: Integer): PQCharA;
function StrPosU(Start, Current: PQCharA; var ACol, ARow: Integer): PQCharA;
function StrPosW(Start, Current: PQCharW; var ACol, ARow: Integer): PQCharW;

// 字符串分解
function DecodeTokenA(var p: PQCharA; ADelimiters: array of QCharA;
  AQuoter: QCharA; AIgnoreSpace: Boolean): QStringA;
function DecodeTokenU(var p: PQCharA; ADelimiters: array of QCharA;
  AQuoter: QCharA; AIgnoreSpace: Boolean): QStringA;
function DecodeTokenW(var p: PQCharW; ADelimiters: array of QCharW;
  AQuoter: QCharW; AIgnoreSpace: Boolean; ASkipDelimiters: Boolean = True)
  : QStringW; overload;
function DecodeTokenW(var p: PQCharW; ADelimiters: PQCharW; AQuoter: QCharW;
  AIgnoreSpace: Boolean; ASkipDelimiters: Boolean = True): QStringW; overload;
function DecodeTokenW(var S: QStringW; ADelimiters: PQCharW; AQuoter: QCharW;
  AIgnoreCase, ARemove: Boolean; ASkipDelimiters: Boolean = True)
  : QStringW; overload;
function SplitTokenW(AList: TStrings; p: PQCharW; ADelimiters: PQCharW;
  AQuoter: QCharW; AIgnoreSpace: Boolean): Integer; overload;
function SplitTokenW(AList: TStrings; const S: QStringW; ADelimiters: PQCharW;
  AQuoter: QCharW; AIgnoreSpace: Boolean): Integer; overload;
function StrBeforeW(var source: PQCharW; const ASpliter: QStringW;
  AIgnoreCase, ARemove: Boolean; AMustMatch: Boolean = false)
  : QStringW; overload;
function StrBeforeW(var source: QStringW; const ASpliter: QStringW;
  AIgnoreCase, ARemove: Boolean; AMustMatch: Boolean = false)
  : QStringW; overload;
function SplitByStrW(AList: TStrings; ASource: QStringW;
  const ASpliter: QStringW; AIgnoreCase: Boolean): Integer;
function LeftStrW(const S: QStringW; AMaxCount: Integer; ACheckExt: Boolean)
  : QStringW; overload;
function LeftStrW(var S: QStringW; const ADelimiters: QStringW;
  ARemove: Boolean): QStringW; overload;
function RightStrW(const S: QStringW; AMaxCount: Integer; ACheckExt: Boolean)
  : QStringW; overload;
function RightStrW(var S: QStringW; const ADelimiters: QStringW;
  ARemove: Boolean): QStringW; overload;
function StrBetween(var S: PQCharW; AStartTag, AEndTag: QStringW;
  AIgnoreCase: Boolean): QStringW; overload;
function StrBetweenTimes(const S, ADelimiter: QStringW; AIgnoreCase: Boolean;
  AStartTimes: Integer = 0; AStopTimes: Integer = 1): QStringW;
function TokenWithIndex(var S: PQCharW; AIndex: Integer; ADelimiters: PQCharW;
  AQuoter: QCharW; AIgnoreSapce: Boolean): QStringW;

// 获取一行
function DecodeLineA(var p: PQCharA; ASkipEmpty: Boolean = True;
  AMaxSize: Integer = MaxInt): QStringA;
function DecodeLineU(var p: PQCharA; ASkipEmpty: Boolean = True;
  AMaxSize: Integer = MaxInt): QStringA;
function DecodeLineW(var p: PQCharW; ASkipEmpty: Boolean = True;
  AMaxSize: Integer = MaxInt; AQuoterChar: QCharW = #0): QStringW;
// 大小写转换
function CharUpperA(c: QCharA): QCharA; inline;
function CharUpperW(c: QCharW): QCharW; inline;
function CharLowerA(c: QCharA): QCharA; inline;
function CharLowerW(c: QCharW): QCharW; inline;
function UpperFirstW(const S: QStringW): QStringW;
// 判断是否是以指定的字符串开始
function StartWithA(S, startby: PQCharA; AIgnoreCase: Boolean): Boolean;
function StartWithU(S, startby: PQCharA; AIgnoreCase: Boolean): Boolean;
function StartWithW(S, startby: PQCharW; AIgnoreCase: Boolean): Boolean;
// 判断是否以指定的字符串结尾
function EndWithA(const S, endby: QStringA; AIgnoreCase: Boolean): Boolean;
function EndWithU(const S, endby: QStringA; AIgnoreCase: Boolean): Boolean;
function EndWithW(const S, endby: QStringW; AIgnoreCase: Boolean): Boolean;
// 检查两个字符串从开始算相同的字符数
function SameCharsA(s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
function SameCharsU(s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
function SameCharsW(s1, s2: PQCharW; AIgnoreCase: Boolean): Integer;
// 加载文本
function LoadTextA(const AFileName: String;
  AEncoding: TTextEncoding = teUnknown): QStringA; overload;
function LoadTextA(AStream: TStream; AEncoding: TTextEncoding = teUnknown)
  : QStringA; overload;
function LoadTextU(const AFileName: String;
  AEncoding: TTextEncoding = teUnknown): QStringA; overload;
function LoadTextU(AStream: TStream; AEncoding: TTextEncoding = teUnknown)
  : QStringA; overload;
function LoadTextW(const AFileName: String;
  AEncoding: TTextEncoding = teUnknown): QStringW; overload;
function LoadTextW(AStream: TStream; AEncoding: TTextEncoding = teUnknown)
  : QStringW; overload;
// 检测文本编码并加载文本内容，注意对于没有BOM的文本的检测不是100%，尤其在没有BOM
// 时难以区分Unicode和Ansi编码的字符
function DecodeText(p: Pointer; ASize: Integer;
  AEncoding: TTextEncoding = teUnknown): QStringW;
// 保存文本
procedure SaveTextA(const AFileName: String; const S: QStringA); overload;
procedure SaveTextA(AStream: TStream; const S: QStringA); overload;
procedure SaveTextU(const AFileName: String; const S: QStringA;
  AWriteBom: Boolean = True); overload;
procedure SaveTextU(const AFileName: String; const S: QStringW;
  AWriteBom: Boolean = True); overload;
procedure SaveTextU(AStream: TStream; const S: QStringA;
  AWriteBom: Boolean = True); overload;
procedure SaveTextU(AStream: TStream; const S: QStringW;
  AWriteBom: Boolean = True); overload;
procedure SaveTextW(const AFileName: String; const S: QStringW;
  AWriteBom: Boolean = True); overload;
procedure SaveTextW(AStream: TStream; const S: QStringW;
  AWriteBom: Boolean = True); overload;
procedure SaveTextWBE(AStream: TStream; const S: QStringW;
  AWriteBom: Boolean = True); overload;
// 子串查找
function StrStrA(s1, s2: PQCharA): PQCharA;
function StrIStrA(s1, s2: PQCharA): PQCharA;
function StrStrU(s1, s2: PQCharA): PQCharA;
function StrIStrU(s1, s2: PQCharA): PQCharA;
function StrStrW(s1, s2: PQCharW): PQCharW;
function StrIStrW(s1, s2: PQCharW): PQCharW;

// 字符串的 Like 匹配
function StrLikeX(var S: PQCharW; pat: PQCharW; AIgnoreCase: Boolean): PQCharW;
function StrLikeW(S, pat: PQCharW; AIgnoreCase: Boolean): Boolean; overload;
// 计算子串的起始位置
function PosA(sub, S: PQCharA; AIgnoreCase: Boolean; AStartPos: Integer = 1)
  : Integer; overload;
function PosA(sub, S: QStringA; AIgnoreCase: Boolean; AStartPos: Integer = 1)
  : Integer; overload;
/// <summary>Pos函数的增强版本实现</summary>
/// <param name="sub">要查找的子字符串</param>
/// <param name="S">用来查找的原字符串</param>
/// <param name="AIgnoreCase">是否忽略大小写</param>
/// <param name="AStartPos">起始位置，第一个字符位置为1</param>
/// <returns>找到，返回子串的起始位置，失败，返回0<returns>
function PosW(sub, S: PQCharW; AIgnoreCase: Boolean; AStartPos: Integer = 1)
  : Integer; overload;
/// <param name="sub">要查找的子字符串</param>
/// <param name="S">用来查找的原字符串</param>
/// <param name="AIgnoreCase">是否忽略大小写</param>
/// <param name="AStartPos">起始位置，第一个字符位置为1</param>
/// <returns>找到，返回子串的起始位置，失败，返回0<returns>
function PosW(sub, S: QStringW; AIgnoreCase: Boolean; AStartPos: Integer = 1)
  : Integer; overload;
// 字符串复制
function StrDupX(const S: PQCharW; ACount: Integer): QStringW;
function StrDupW(const S: PQCharW; AOffset: Integer = 0;
  const ACount: Integer = MaxInt): QStringW;
procedure StrCpyW(d: PQCharW; S: PQCharW; ACount: Integer = -1);
// 字符串比较
function StrCmpA(const s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
function StrCmpW(const s1, s2: PQCharW; AIgnoreCase: Boolean): Integer;
function StrNCmpW(const s1, s2: PQCharW; AIgnoreCase: Boolean;
  ALength: Integer): Integer;
function NaturalCompareW(s1, s2: PQCharW; AIgnoreCase: Boolean;
  AIgnoreSpace: Boolean = True): Integer;
// 十六进制相关函数
function IsHexChar(c: QCharW): Boolean; inline;
function IsOctChar(c: QCharW): Boolean; inline;
function HexValue(c: QCharW): Integer; inline;
function HexChar(V: Byte): QCharW; inline;
// 类型转换函数
function TryStrToGuid(const S: QStringW; var AGuid: TGuid): Boolean;
function TryStrToIPV4(const S: QStringW; var AIPV4:
{$IFDEF MSWINDOWS}Integer{$ELSE}Cardinal{$ENDIF}): Boolean;
/// StringReplace 增强
function StringReplaceW(const S, Old, New: QStringW; AFlags: TReplaceFlags)
  : QStringW; overload;
/// <summary>替换指定范围内的字符为指定的字符</summary>
/// <param name="AChar">占位字符</param>
/// <param name="AFrom">开始位置，从0开始</param>
/// <param name="ACount">替换数量</param>
/// <returns>返回替换后的字符串</returns>
function StringReplaceW(const S: QStringW; const AChar: QCharW;
  AFrom, ACount: Integer): QStringW; overload;
/// <summary>使用指定的内容替换AStartTag和EndTag之间的内容</summary>
/// <params>
/// <param name="S">要查找替换的字符串</param>
/// <param name="AStartTag">开始的标签名称</param>
/// <param name="AEndTag">结束的标签名称</param>
/// <param name="AReplaced">替换的结果</param>
/// <param name="AWithTag">是否连同AStartTag和AEndTag标签一起替换掉</param>
/// <param name="AIgnoreCase">比较标签名称时是否忽略大小</param>
/// <param name="AMaxTimes">最大替换次数，默认为1</param>
/// </params>
/// <returns>返回替换后的内容</returns>
function StringReplaceWithW(const S, AStartTag, AEndTag, AReplaced: QStringW;
  AWithTag, AIgnoreCase: Boolean; AMaxTimes: Cardinal = 1): QStringW;
/// 重复指定的字符N次
function StringReplicateW(const S: QStringW; ACount: Integer)
  : QStringW; overload;
function StringReplicateW(const S, AChar: QStringW; AExpectLength: Integer)
  : QStringW; overload;
/// <summary>
/// 将字符串中指定的字符按其在第二个参数中出现的位置替换为第三个参数同样位置的字符
/// </summary>
/// <param name="S">
/// 要查找的字符串
/// </param>
/// <param name="AToReplace">
/// 要替换的字符序列
/// </param>
/// <param name="AReplacement">
/// 同位置替换的字符序列
/// </param>
/// <returns>
/// 返回替换完成的结果
/// </returns>
/// <remarks>
/// AToReplace 和 AReplacement 的字符串长度要求必需保持一致，否则抛出异常。
/// </remarks>
/// <example>
/// translate('Techonthenet.com', 'met', 'ABC') 执行的结果为 TBchonChBnBC.coA
/// </example>
function Translate(const S, AToReplace, AReplacement: QStringW): QStringW;
/// <summary>过滤掉字符串内容中不需要的字符</summary>
/// <param name="S">要过滤的字符串</param>
/// <param name="AcceptChars">允许的字符列表</param>
/// <returns>返回过滤后的结果</returns>
function FilterCharW(const S: QStringW; AcceptChars: QStringW)
  : QStringW; overload;
/// <summary>过滤掉字符串内容中不需要的字符</summary>
/// <param name="S">要过滤的字符串</param>
/// <param name="AOnValidate">用于过滤的回调函数</param>
/// <param name="ATag">用户自定义的附加参数，会传递给AOnValidate事件</param>
/// <returns>返回过滤后的结果</returns>
function FilterCharW(const S: QStringW; AOnValidate: TQFilterCharEvent;
  ATag: Pointer = nil): QStringW; overload;
{$IFDEF UNICODE}
/// <summary>过滤掉字符串内容中不需要的字符</summary>
/// <param name="S">要过滤的字符串</param>
/// <param name="AOnValidate">用于过滤的回调函数</param>
/// <param name="ATag">用户自定义的附加参数，会传递给AOnValidate事件</param>
/// <returns>返回过滤后的结果</returns>
function FilterCharW(const S: QStringW; AOnValidate: TQFilterCharEventA;
  ATag: Pointer = nil): QStringW; overload;
{$ENDIF}
/// <summary>过滤掉所有非数值类型的字符，从而将其格式化一个相对标准的浮点数</summary>
/// <param name="S">要过滤的字符串</param>
/// <param name="Accepts">过滤条件组件</param>
/// <returns>返回过滤后的结果</returns>
/// <remarks>
/// FilterNoNumberW 过滤后的结果可以使用 ParseNumeric 解析为数组，大部分情况下也
/// 可以被StrToFloat解析（但StrToFloat不支持有些特殊格式）

function FilterNoNumberW(const S: QStringW; Accepts: TQNumberTypes): QStringW;
/// <summary>简体中文转换为繁体中文</summary>
/// <param name="S">要转换的字符串</param>
/// <returns>返回转换后的结果</returns>
function SimpleChineseToTraditional(S: QStringW): QStringW;
/// <summary>繁体中文转换为简体中文</summary>
/// <param name="S">要转换的字符串</param>
/// <returns>返回转换后的结果</returns>
function TraditionalChineseToSimple(S: QStringW): QStringW;
/// <summary> 将货币值转换为汉字大写</summary>
/// <param name="AVal">货币值</param>
/// <param name="AFlags">标志位组合，以决定输出结果的格式</param>
/// <param name="ANegText">当货币值为负数时，显示的前缀</param>
/// <param name="AStartText">前导字符串，如“人民币：”</param>
/// <param name="AEndText">后置字符串，如“整”</param>
/// <param name="AGroupNum">组数，每个数字与其单位构成一个数组，AGroupNum指出要求的组数量，不为0时，会忽略标志位中的MC_HIDE_ZERO和MC_MERGE_ZERO</param>
/// <param name="ARoundMethod">金额舍入到分时的算法</param>
/// <param name="AEndDigts">小数点后的位数，-16~4 之间</param>
/// <returns>返回格式化后的字符串</returns>
function CapMoney(AVal: Currency; AFlags: Integer;
  ANegText, AStartText, AEndText: QStringW; AGroupNum: Integer;
  ARoundMethod: TMoneyRoundMethod; AEndDigits: Integer = 2): QStringW;

/// <summary>
/// 判断指定的字符串是否是一个有效的名称
/// </summary>
/// <param name="S">
/// 要判断的字符串
/// </param>
/// <param name="AllowChars">
/// 允许的字符类型
/// </param>
/// <param name="AMinLen">
/// 最小允许的字符数
/// </param>
/// <param name="AMaxLen">
/// 最大允许的字符数
/// </param>
/// <param name="AOnTest">
/// 用户自定义测试规则回调
/// </param>
function IsHumanName(S: QStringW; AllowChars: TNameCharSet;
  AMinLen: Integer = 2; AMaxLen: Integer = 15;
  AOnTest: TQCustomNameCharTest = nil): Boolean;
/// <summary>
/// 判断指定的字符串是否是一个中国人姓名
/// </summary>
function IsChineseName(S: QStringW): Boolean;
/// <summary>
/// 判断指定的字符串是否是一个有效的外国人名
/// </summary>
function IsNoChineseName(S: QStringW): Boolean;
/// <summary>
/// 判断指定的字符串是否是有效的中国地址
/// </summary>
function IsChineseAddr(S: QStringW; AMinLength: Integer = 3): Boolean;
/// <summary>
/// 判断指定的字符串是否是有效的外国地址
/// </summary>
function IsNoChineseAddr(S: QStringW; AMinLength: Integer = 3): Boolean;
/// 查找指定的二进制内容出现的起始位置
function MemScan(S: Pointer; len_s: Integer; sub: Pointer;
  len_sub: Integer): Pointer;
function BinaryCmp(const p1, p2: Pointer; len: Integer): Integer;
{$IFNDEF WIN64}inline; {$ENDIF}
// 下面的函数只能Unicode版本，没有Ansi和UTF-8版本，如果需要，再加入
// 分解名称-值对
function NameOfW(const S: QStringW; ASpliter: QCharW): QStringW;
function ValueOfW(const S: QStringW; ASpliter: QCharW): QStringW;
function IndexOfNameW(AList: TStrings; const AName: QStringW;
  ASpliter: QCharW): Integer;
function IndexOfValueW(AList: TStrings; const AValue: QStringW;
  ASpliter: QCharW): Integer;

function DeleteCharW(const ASource, ADeletes: QStringW): QStringW;
function DeleteSideCharsW(const ASource: QStringW; ADeletes: QStringW;
  AIgnoreCase: Boolean = false): QStringW;
function DeleteRightW(const S, ADelete: QStringW; AIgnoreCase: Boolean = false;
  ACount: Integer = MaxInt): QStringW;
function DeleteLeftW(const S, ADelete: QStringW; AIgnoreCase: Boolean = false;
  ACount: Integer = MaxInt): QStringW;
function ContainsCharW(const S, ACharList: QStringW): Boolean;
function HtmlEscape(const S: QStringW): QStringW;
function HtmlUnescape(const S: QStringW): QStringW;
function JavaEscape(const S: QStringW; ADoEscape: Boolean): QStringW;
function JavaUnescape(const S: QStringW; AStrictEscape: Boolean): QStringW;
function HtmlTrimText(const S: QStringW): QStringW;
function UrlEncode(const ABytes: PByte; l: Integer; ASpacesAsPlus: Boolean)
  : QStringW; overload;
function UrlEncode(const ABytes: TBytes; ASpacesAsPlus: Boolean)
  : QStringW; overload;
function UrlEncode(const S: QStringW; ASpacesAsPlus: Boolean;
  AUtf8Encode: Boolean = True): QStringW; overload;
function UrlDecode(const AUrl: QStringW;
  var AScheme, AHost, ADocument: QStringW; var APort: Word; AParams: TStrings;
  AUtf8Encode: Boolean = True): Boolean;

function LeftStrCount(const S: QStringW; const sub: QStringW;
  AIgnoreCase: Boolean): Integer;
function RightPosW(const S: QStringW; const sub: QStringW;
  AIgnoreCase: Boolean): Integer;
// 下面是一些辅助函数
function ParseInt(var S: PQCharW; var ANum: Int64): Integer;
function ParseHex(var p: PQCharW; var Value: Int64): Integer;
function ParseNumeric(var S: PQCharW; var ANum: Extended): Boolean;
function ParseDateTime(S: PWideChar; var AResult: TDateTime): Boolean;
function ParseWebTime(p: PWideChar; var AResult: TDateTime): Boolean;
function EncodeWebTime(ATime: TDateTime): String;
function RollupSize(ASize: Int64): QStringW;
function RollupTime(ASeconds: Int64; AHideZero: Boolean = True): QStringW;
function DetectTextEncoding(const p: Pointer; l: Integer; var b: Boolean)
  : TTextEncoding;
procedure ExchangeByteOrder(p: PQCharA; l: Integer); overload;
function ExchangeByteOrder(V: Smallint): Smallint; overload; inline;
function ExchangeByteOrder(V: Word): Word; overload; inline;
function ExchangeByteOrder(V: Integer): Integer; overload; inline;
function ExchangeByteOrder(V: Cardinal): Cardinal; overload; inline;
function ExchangeByteOrder(V: Int64): Int64; overload; inline;
function ExchangeByteOrder(V: Single): Single; overload; inline;
function ExchangeByteOrder(V: Double): Double; overload; // inline;

procedure FreeObject(AObject: TObject); inline;
procedure FreeAndNilObject(var AObject); inline;
// 原子操作函数
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer;
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer;
{$IF RTLVersion<24}
// 为与XE6兼容，InterlockedCompareExchange等价
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; inline; overload;
function AtomicCmpExchange(var Target: Pointer; Value: Pointer;
  Comparand: Pointer): Pointer; inline; overload;
// 等价于InterlockedExchanged
function AtomicExchange(var Target: Integer; Value: Integer): Integer;
  inline; overload;
function AtomicExchange(var Target: Pointer; Value: Pointer): Pointer;
  inline; overload;

function AtomicIncrement(var Target: Integer; const Value: Integer = 1)
  : Integer; inline;
function AtomicDecrement(var Target: Integer): Integer; inline;
{$IFEND <XE5}
//
function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean = false)
  : QStringW; overload;
function BinToHex(const ABytes: TBytes; ALowerCase: Boolean = false)
  : QStringW; overload;
function HexToBin(const S: QStringW): TBytes; overload;
procedure HexToBin(const S: QStringW; var AResult: TBytes); overload;
// 计算指定内容的哈希值
function HashOf(p: Pointer; l: Integer): Cardinal;
function NewId: TGuid;
function SameId(const V1, V2: TGuid): Boolean;

/// <summary>计算指定内容的密码强度</summary>
/// <param name="S">密码</param>
/// <returns>返回一个>=0的密码强度值</returns>
function PasswordScale(const S: QStringW): Integer;
/// <summary>将指定的密码强度系数转换为强度等级</summary>
/// <param name="AScale">通过PasswordScale得到的强度等级</param>
/// <returns>返回转换后的强度等级</returns>
function CheckPassword(const AScale: Integer): TPasswordStrongLevel; overload;
/// <summary>计算指定内容的密码的强度等级</summary>
/// <param name="S">密码</param>
/// <returns>返回计算得到的强度等级</returns>
function CheckPassword(const S: QStringW): TPasswordStrongLevel; overload;
/// <summary>检查指定的中国身份证号的有效性</summary>
/// <param name="CardNo">身份证号</param>
/// <returns>号码符合规则，返回true，否则，返回false</returns>
function IsChineseIdNo(CardNo: QStringW): Boolean;
/// <summary>解析指定的中国大陆身份证号的组成部分</summary>
/// <param name="CardNo">身份证号</param>
/// <param name="AreaCode">行政区划代码</param>
/// <param name="Birthday">出生日期</param>
/// <param name="IsFemale">性别，男为true，女为false</param>
/// <returns>身份证号有效，返回true，并通过参数返回各个部分，否则，返回false</returns>
function DecodeChineseId(CardNo: QStringW; var AreaCode: QStringW;
  var Birthday: TDateTime; var IsFemale: Boolean): Boolean;
function AreaCodeOfChineseId(CardNo: QStringW): QStringW;
function AgeOfChineseId(CardNo: QStringW; ACalcDate: TDateTime = 0): Integer;
function BirthdayOfChineseId(CardNo: QStringW): TDateTime;
function SexOfChineseId(CardNo: QStringW): TQHumanSex;
/// <summary>检查指定的字符串是否符合电子邮箱格式</summary>
/// <param name="S">要检查的电子邮箱地址</param>
/// <returns>如果是x@y.z格式，则返回true，否则，返回false</returns>
function IsEmailAddr(S: QStringW): Boolean;
/// <summary>检查是否是中国手机号码格式</summary>
/// <param name="S">要检查的手机号码</param>
/// <returns>如果是11位数字，且是以1打头，则返回true，否则返回false</returns>
function IsChineseMobile(S: QStringW): Boolean;
/// <summary>获取指定的文件的大小</summary>
/// <param name="S">要查询的文件名</param>
/// <returns>成功，返回实际的文件大小，失败，返回-1</returns>
function SizeOfFile(const S: QStringW): Int64;
/// <summary>判断两个事件响应函数是否相等</summary>
/// <param name="Left">第一个事件响应函数</param>
/// <param name="Right">第二个事件响应函数</param>
/// <returns>相等，返回true，不相等，返回False</param>
function MethodEqual(const Left, Right: TMethod): Boolean; inline;
/// <summary>合并两个URL,相当于TURI里的相对路径转换为绝对路径的函数</summary>
/// <param name="ABase">基准路径</param>
/// <param name="ARel">相对路径</param>
/// <returns>
/// 1.如果ARel是一个绝对路径，则直接返回该路径.
/// 2.如果ARel是一个以//开始的绝对路径，则拷贝ABase的协议类型，加上ARel形成新路径
/// 3.如果ARel是一个相对路径，则以ABase的路径为基准，加上ARel形成新路径
/// </returns>
function UrlMerge(const ABase, ARel: QStringW): QStringW;
procedure Debugout(const AMsg: String); overload;
procedure Debugout(const AFmt: String; const AParams: array of const); overload;

var
  JavaFormatUtf8: Boolean;
  IsFMXApp: Boolean;
  MemComp: TMemCompFunction;
  OnFetchCNSpell: TCNSpellCallback;

const
  SLineBreak: PQCharW = {$IFDEF MSWINDOWS}#13#10{$ELSE}#10{$ENDIF};
  DefaultNumberSet = [nftFloat, nftDelphiHex, nftCHex, nftBasicHex, nftHexPrec,
    nftNegative, nftPositive];
  HexChars: array [0 .. 15] of QCharW = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  LowerHexChars: array [0 .. 15] of QCharW = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');

implementation

uses DateUtils, Math, sysconst, Variants
{$IF (RTLVersion>=25) and (not Defined(NEXTGEN))}
    , AnsiStrings
{$IFEND >=XE4}
    ;

resourcestring
  SBadUnicodeChar = '无效的Unicode字符:%d';
  SBadUtf8Char = '无效的UTF8字符:%d';
  SOutOfIndex = '索引越界，值 %d 不在[%d..%d]的范围内。';
  SDayName = '天';
  SHourName = '小时';
  SMinuteName = '分钟';
  SSecondName = '秒';
  SCharNeeded = '当前位置应该是 "%s" ，而不是 "%s"。';
  SRangeEndNeeded = '字符范围边界结束字符未指定。';
  STooSmallCapMoneyGroup = '给定的分组数 %d 小于实际需要的最小货币分组数 %d。';
  SUnsupportNow = '指定的函数 %s 目前不受支持';
  SBadJavaEscape = '无效的 Java 转义序列：%s';
  SBadHexChar = '无效的十六进制字符 %s';
  SStreamReadOnly = '不能在一个只读的数据流上写入数据';
  SMismatchReplacement = '%s 与 %s 的长度不一致';

type
  TGBKCharSpell = record
    SpellChar: QCharW;
    StartChar, EndChar: Word;
  end;

  TStrStrFunction = function(s1, s2: PQCharW): PQCharW;
{$IF RTLVersion>=21}
  TIntArray = TArray<Integer>;
{$ELSE}
  TIntArray = array of Integer;
{$IFEND >=2010}
{$IFDEF MSWINDOWS}
  TMSVCStrStr = function(s1, s2: PQCharA): PQCharA; cdecl;
  TMSVCStrStrW = function(s1, s2: PQCharW): PQCharW; cdecl;
  TMSVCMemCmp = function(s1, s2: Pointer; len: Integer): Integer; cdecl;
{$ENDIF}

var
  // GBK汉字拼音首字母表
  GBKSpells: array [0 .. 22] of TGBKCharSpell = (
    (
      SpellChar: 'A'; StartChar: $B0A1; EndChar: $B0C4;
    ), (SpellChar: 'B'; StartChar: $B0C5; EndChar: $B2C0;
    ), (SpellChar: 'C'; StartChar: $B2C1; EndChar: $B4ED;
    ), (SpellChar: 'D'; StartChar: $B4EE; EndChar: $B6E9;
    ), (SpellChar: 'E'; StartChar: $B6EA; EndChar: $B7A1;
    ), (SpellChar: 'F'; StartChar: $B7A2; EndChar: $B8C0;
    ), (SpellChar: 'G'; StartChar: $B8C1; EndChar: $B9FD;
    ), (SpellChar: 'H'; StartChar: $B9FE; EndChar: $BBF6;
    ), (SpellChar: 'J'; StartChar: $BBF7; EndChar: $BFA5;
    ), (SpellChar: 'K'; StartChar: $BFA6; EndChar: $C0AB;
    ), (SpellChar: 'L'; StartChar: $C0AC; EndChar: $C2E7;
    ), (SpellChar: 'M'; StartChar: $C2E8; EndChar: $C4C2;
    ), (SpellChar: 'N'; StartChar: $C4C3; EndChar: $C5B5;
    ), (SpellChar: 'O'; StartChar: $C5B6; EndChar: $C5BD;
    ), (SpellChar: 'P'; StartChar: $C5BE; EndChar: $C6D9;
    ), (SpellChar: 'Q'; StartChar: $C6DA; EndChar: $C8BA;
    ), (SpellChar: 'R'; StartChar: $C8BB; EndChar: $C8F5;
    ), (SpellChar: 'S'; StartChar: $C8F6; EndChar: $CBF0;
    ), (SpellChar: 'T'; StartChar: $CBFA; EndChar: $CDD9;
    ), (SpellChar: 'W'; StartChar: $CDDA; EndChar: $CEF3;
    ), (SpellChar: 'X'; StartChar: $CEF4; EndChar: $D188;
    ), (SpellChar: 'Y'; StartChar: $D1B9; EndChar: $D4D0;
    ), (SpellChar: 'Z'; StartChar: $D4D1; EndChar: $D7F9;));
{$IFDEF MSWINDOWS}
  hMsvcrtl: HMODULE;
  VCStrStr: TMSVCStrStr;
  VCStrStrW: TMSVCStrStrW;
{$IFDEF WIN64}
  VCMemCmp: TMSVCMemCmp;
{$ENDIF}
{$ENDIF}

const
  HtmlEscapeChars: array [0 .. 185] of QStringW = (QCharW(32), '&nbsp;',
    QCharW(34), '&quot;', QCharW(38), '&amp;', QCharW(39), '&apos;', QCharW(60),
    '&lt;', QCharW(62), '&gt;', QCharW(161), '&iexcl;', QCharW(162), '&cent;',
    QCharW(163), '&pound;', QCharW(164), '&curren;', QCharW(165), '&yen;',
    QCharW(166), '&brvbar;', QCharW(167), '&sect;', QCharW(168), '&uml;',
    QCharW(169), '&copy;', QCharW(170), '&ordf;', QCharW(171), '&laquo;',
    QCharW(172), '&not;', QCharW(173), '&shy;', QCharW(174), '&reg;',
    QCharW(175), '&macr;', QCharW(176), '&deg;', QCharW(177), '&plusmn;',
    QCharW(180), '&acute;', QCharW(181), '&micro;', QCharW(182), '&para;',
    QCharW(183), '&middot;', QCharW(184), '&cedil;', QCharW(186), '&ordm;',
    QCharW(187), '&raquo;', QCharW(191), '&iquest;', QCharW(192), '&Agrave;',
    QCharW(193), '&Aacute;', QCharW(194), '&circ;', QCharW(195), '&Atilde;',
    QCharW(197), '&ring;', QCharW(198), '&AElig;', QCharW(199), '&Ccedil;',
    QCharW(200), '&Egrave;', QCharW(201), '&Eacute;', QCharW(202), '&Ecirc;',
    QCharW(203), '&Euml;', QCharW(204), '&Igrave;', QCharW(205), '&Iacute;',
    QCharW(206), '&Icirc;', QCharW(207), '&Iuml;', QCharW(208), '&ETH;',
    QCharW(209), '&Ntilde;', QCharW(210), '&Ograve;', QCharW(211), '&Oacute;',
    QCharW(212), '&Ocirc;', QCharW(213), '&Otilde;', QCharW(214), '&Ouml;',
    QCharW(215), '&times;', QCharW(216), '&Oslash;', QCharW(217), '&Ugrave;',
    QCharW(218), '&Uacute;', QCharW(219), '&Ucirc;', QCharW(220), '&Uuml;',
    QCharW(221), '&Yacute;', QCharW(222), '&THORN;', QCharW(223), '&szlig;',
    QCharW(224), '&agrave;', QCharW(225), '&aacute;', QCharW(227), '&atilde;',
    QCharW(228), '&auml;', QCharW(229), '&aring;', QCharW(230), '&aelig;',
    QCharW(231), '&ccedil;', QCharW(232), '&egrave;', QCharW(233), '&eacute;',
    QCharW(234), '&ecirc;', QCharW(235), '&euml;', QCharW(236), '&igrave;',
    QCharW(237), '&iacute;', QCharW(238), '&icirc;', QCharW(239), '&iuml;',
    QCharW(240), '&ieth;', QCharW(241), '&ntilde;', QCharW(242), '&ograve;',
    QCharW(243), '&oacute;', QCharW(244), '&ocirc;', QCharW(245), '&otilde;',
    QCharW(246), '&ouml;', QCharW(247), '&divide;', QCharW(248), '&oslash;',
    QCharW(249), '&ugrave;', QCharW(250), '&uacute;', QCharW(251), '&ucirc;',
    QCharW(252), '&uuml;', QCharW(253), '&yacute;', QCharW(254), '&thorn;',
    QCharW(255), '&yuml;');
  // QString函数

function Utf8Decode(const p: QStringA): QStringW;
begin
  if p.IsUtf8 then
    Result := Utf8Decode(PQCharA(p), p.Length)
  else if p.Length > 0 then
    Result := AnsiDecode(p)
  else
    SetLength(Result, 0);
end;

function Utf8Encode(const p: QStringW): QStringA;
var
  l: NativeInt;
begin
  l := Length(p);
  if l > 0 then
    Result := Utf8Encode(PQCharW(p), l)
  else
  begin
    Result.Length := 0;
    Result.FValue[0] := 1;
  end;
end;

function Utf8Decode(p: PQCharA; l: Integer; var AResult: QStringW;
  var ABadAt: PQCharA): Boolean; overload;
var
  ps, pe: PByte;
  pd, pds: PWord;
  c: Cardinal;
  procedure _Utf8Decode;
  begin
    ps := PByte(p);
    pe := ps;
    Inc(pe, l);
    System.SetLength(AResult, l);
    pd := PWord(PQCharW(AResult));
    pds := pd;
    Result := True;
    while IntPtr(ps) < IntPtr(pe) do
    begin
      if (ps^ and $80) <> 0 then
      begin
        if (ps^ and $FC) = $FC then // 4000000+
        begin
          c := (ps^ and $03) shl 30;
          Inc(ps);
          c := c or ((ps^ and $3F) shl 24);
          Inc(ps);
          c := c or ((ps^ and $3F) shl 18);
          Inc(ps);
          c := c or ((ps^ and $3F) shl 12);
          Inc(ps);
          c := c or ((ps^ and $3F) shl 6);
          Inc(ps);
          c := c or (ps^ and $3F);
          Inc(ps);
          c := c - $10000;
          pd^ := $D800 + ((c shr 10) and $3FF);
          Inc(pd);
          pd^ := $DC00 + (c and $3FF);
          Inc(pd);
        end
        else if (ps^ and $F8) = $F8 then // 200000-3FFFFFF
        begin
          c := (ps^ and $07) shl 24;
          Inc(ps);
          c := c or ((ps^ and $3F) shl 18);
          Inc(ps);
          c := c or ((ps^ and $3F) shl 12);
          Inc(ps);
          c := c or ((ps^ and $3F) shl 6);
          Inc(ps);
          c := c or (ps^ and $3F);
          Inc(ps);
          c := c - $10000;
          pd^ := $D800 + ((c shr 10) and $3FF);
          Inc(pd);
          pd^ := $DC00 + (c and $3FF);
          Inc(pd);
        end
        else if (ps^ and $F0) = $F0 then // 10000-1FFFFF
        begin
          c := (ps^ and $0F) shl 18;
          Inc(ps);
          c := c or ((ps^ and $3F) shl 12);
          Inc(ps);
          c := c or ((ps^ and $3F) shl 6);
          Inc(ps);
          c := c or (ps^ and $3F);
          Inc(ps);
          c := c - $10000;
          pd^ := $D800 + ((c shr 10) and $3FF);
          Inc(pd);
          pd^ := $DC00 + (c and $3FF);
          Inc(pd);
        end
        else if (ps^ and $E0) = $E0 then // 800-FFFF
        begin
          c := (ps^ and $1F) shl 12;
          Inc(ps);
          c := c or ((ps^ and $3F) shl 6);
          Inc(ps);
          c := c or (ps^ and $3F);
          Inc(ps);
          pd^ := c;
          Inc(pd);
        end
        else if (ps^ and $C0) = $C0 then // 80-7FF
        begin
          pd^ := (ps^ and $3F) shl 6;
          Inc(ps);
          pd^ := pd^ or (ps^ and $3F);
          Inc(pd);
          Inc(ps);
        end
        else
        begin
          ABadAt := PQCharA(ps);
          Result := false;
          Exit;
        end;
      end
      else
      begin
        pd^ := ps^;
        Inc(ps);
        Inc(pd);
      end;
    end;
    System.SetLength(AResult, (IntPtr(pd) - IntPtr(pds)) shr 1);
  end;

begin
  if l <= 0 then
  begin
    ps := PByte(p);
    while ps^ <> 0 do
      Inc(ps);
    l := IntPtr(ps) - IntPtr(p);
  end;
{$IFDEF MSWINDOWS}
  SetLength(AResult, l);
  SetLength(AResult, MultiByteToWideChar(CP_UTF8, 8, PAnsiChar(p), l,
    PQCharW(AResult), l)); // 8==>MB_ERR_INVALID_CHARS
  Result := Length(AResult) <> 0;
  if not Result then
    _Utf8Decode;
{$ELSE}
  _Utf8Decode
{$ENDIF}
end;

function Utf8Decode(p: PQCharA; l: Integer): QStringW;
var
  ABadChar: PQCharA;
begin
  if not Utf8Decode(p, l, Result, ABadChar) then
    raise Exception.Create(Format(SBadUtf8Char, [ABadChar^]));
end;

function WideCharUtf8Size(c: Integer): Integer;
begin
  if c < $7F then
    Result := 1
  else if c < $7FF then
    Result := 2
  else if c < $FFFF then
    Result := 3
  else if c < $1FFFFF then
    Result := 4
  else if c < $3FFFFFF then
    Result := 5
  else
    Result := 6;
end;

function Utf8BufferSize(p: PQCharW; var l: Integer): Integer;
var
  c: Cardinal;
  T: Integer;
begin
  Result := 0;
  if l < 0 then
  begin
    l := 0;
    while p^ <> #0 do
    begin
      if (p^ >= #$D800) and (p^ <= #$DFFF) then // Unicode 扩展区字符
      begin
        c := (Word(p^) - $D800);
        Inc(p);
        if (p^ >= #$DC00) and (p^ <= #$DFFF) then
        begin
          c := $10000 + (c shl 10) + Word(p^) - $DC00;
          Inc(p);
        end;
        Inc(Result, WideCharUtf8Size(c));
      end
      else
        Inc(Result, WideCharUtf8Size(Word(p^)));
      Inc(p);
      Inc(l);
    end;
  end
  else
  begin
    T := l;
    while T > 0 do
    begin
      if (p^ >= #$D800) and (p^ <= #$DFFF) then // Unicode 扩展区字符
      begin
        c := (Word(p^) - $D800);
        Inc(p);
        if (p^ >= #$DC00) and (p^ <= #$DFFF) then
        begin
          c := $10000 + (c shl 10) + Word(p^) - $DC00;
          Inc(p);
        end;
        Inc(Result, WideCharUtf8Size(c));
      end
      else
        Inc(Result, WideCharUtf8Size(Word(p^)));
      Inc(p);
      Dec(T);
    end;
  end;
end;

function Utf8Encode(ps: PQCharW; sl: Integer; pd: PQCharA; dl: Integer)
  : Integer;
{$IFNDEF MSWINDOWS}
var
  pds: PQCharA;
  c: Cardinal;
{$ENDIF}
begin
  if (ps = nil) or (sl = 0) then
    Result := 0
  else
  begin
{$IFDEF MSWINDOWS}
    // Windows下直接调用操作系统的API
    Result := WideCharToMultiByte(CP_UTF8, 0, ps, sl, PAnsiChar(pd), dl,
      nil, nil);
{$ELSE}
    pds := pd;
    while sl > 0 do
    begin
      c := Cardinal(ps^);
      Inc(ps);
      if (c >= $D800) and (c <= $DFFF) then // Unicode 扩展区字符
      begin
        c := (c - $D800);
        if (ps^ >= #$DC00) and (ps^ <= #$DFFF) then
        begin
          c := $10000 + ((c shl 10) + (Cardinal(ps^) - $DC00));
          Inc(ps);
          Dec(sl);
        end
        else
          raise Exception.Create(Format(SBadUnicodeChar, [IntPtr(ps^)]));
      end;
      Dec(sl);
      if c = $0 then
      begin
        if JavaFormatUtf8 then // 按照Java格式编码，将#$0字符编码为#$C080
        begin
          pd^ := $C0;
          Inc(pd);
          pd^ := $80;
          Inc(pd);
        end
        else
        begin
          pd^ := c;
          Inc(pd);
        end;
      end
      else if c <= $7F then // 1B
      begin
        pd^ := c;
        Inc(pd);
      end
      else if c <= $7FF then // $80-$7FF,2B
      begin
        pd^ := $C0 or (c shr 6);
        Inc(pd);
        pd^ := $80 or (c and $3F);
        Inc(pd);
      end
      else if c <= $FFFF then // $8000 - $FFFF,3B
      begin
        pd^ := $E0 or (c shr 12);
        Inc(pd);
        pd^ := $80 or ((c shr 6) and $3F);
        Inc(pd);
        pd^ := $80 or (c and $3F);
        Inc(pd);
      end
      else if c <= $1FFFFF then // $01 0000-$1F FFFF,4B
      begin
        pd^ := $F0 or (c shr 18); // 1111 0xxx
        Inc(pd);
        pd^ := $80 or ((c shr 12) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 6) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or (c and $3F); // 10 xxxxxx
        Inc(pd);
      end
      else if c <= $3FFFFFF then // $20 0000 - $3FF FFFF,5B
      begin
        pd^ := $F8 or (c shr 24); // 1111 10xx
        Inc(pd);
        pd^ := $F0 or ((c shr 18) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 12) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 6) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or (c and $3F); // 10 xxxxxx
        Inc(pd);
      end
      else if c <= $7FFFFFFF then // $0400 0000-$7FFF FFFF,6B
      begin
        pd^ := $FC or (c shr 30); // 1111 11xx
        Inc(pd);
        pd^ := $F8 or ((c shr 24) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $F0 or ((c shr 18) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 12) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 6) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or (c and $3F); // 10 xxxxxx
        Inc(pd);
      end;
    end;
    pd^ := 0;
    Result := IntPtr(pd) - IntPtr(pds);
{$ENDIF}
  end;
end;

function Utf8Encode(p: PQCharW; l: Integer): QStringA;
begin
  Result.Length := l * 3;
  Result.IsUtf8 := True;
  if l > 0 then
    Result.Length := Utf8Encode(p, l, PQCharA(Result), Result.Length);
end;

function AnsiEncode(p: PQCharW; l: Integer): QStringA;
var
  ps: PQCharW;
begin
  if l <= 0 then
  begin
    ps := p;
    while ps^ <> #0 do
      Inc(ps);
    l := ps - p;
  end;
  if l > 0 then
  begin
{$IFDEF MSWINDOWS}
    Result.Length := WideCharToMultiByte(CP_ACP, 0, p, l, nil, 0, nil, nil);
    WideCharToMultiByte(CP_ACP, 0, p, l, LPSTR(Result.Data), Result.Length,
      nil, nil);
{$ELSE}
    Result.Length := l shl 1;
    Result.FValue[0] := 0;
    Move(p^, PQCharA(Result)^, l shl 1);
    Result := TEncoding.Convert(TEncoding.Unicode, TEncoding.ANSI,
      Result.FValue, 1, l shl 1);
{$ENDIF}
  end
  else
    Result.Length := 0;
end;

function AnsiEncode(const p: QStringW): QStringA;
var
  l: NativeInt;
begin
  l := Length(p);
  if l > 0 then
    Result := AnsiEncode(PQCharW(p), l)
  else
    Result.Length := 0;
end;

function AnsiDecode(p: PQCharA; l: Integer): QStringW;
var
  ps: PQCharA;
{$IFNDEF MSWINDOWS}
  ABytes: TBytes;
{$ENDIF}
begin
  if l <= 0 then
  begin
    ps := p;
    while ps^ <> 0 do
      Inc(ps);
    l := IntPtr(ps) - IntPtr(p);
  end;
  if l > 0 then
  begin
{$IFDEF MSWINDOWS}
    System.SetLength(Result, MultiByteToWideChar(CP_ACP, 0, PAnsiChar(p),
      l, nil, 0));
    MultiByteToWideChar(CP_ACP, 0, PAnsiChar(p), l, PWideChar(Result),
      Length(Result));
{$ELSE}
    System.SetLength(ABytes, l);
    Move(p^, PByte(@ABytes[0])^, l);
    Result := TEncoding.ANSI.GetString(ABytes);
{$ENDIF}
  end
  else
    System.SetLength(Result, 0);
end;

function AnsiDecode(const p: QStringA): QStringW;
begin
  if p.IsUtf8 then
    Result := Utf8Decode(p)
  else if p.Length > 0 then
  begin
{$IFDEF MSWINDOWS}
    Result := AnsiDecode(PQCharA(p), p.Length);
{$ELSE}
    Result := TEncoding.ANSI.GetString(p.FValue, 1, p.Length);
{$ENDIF}
  end
  else
    SetLength(Result, 0);
end;

function SpellOfChar(w: Word): QCharW;
var
  I, l, H: Integer;
begin
  Result := #0;
  l := 0;
  H := 22;
  repeat
    I := (l + H) div 2;
    if w >= GBKSpells[I].StartChar then
    begin
      if w <= GBKSpells[I].EndChar then
      begin
        Result := GBKSpells[I].SpellChar;
        Break;
      end
      else
        l := I + 1;
    end
    else
      H := I - 1;
  until l > H;
end;

function CNSpellChars(S: QStringA; AIgnoreEnChars: Boolean): QStringW;
var
  p: PQCharA;
  pd, pds: PQCharW;

begin
  if S.Length > 0 then
  begin
    p := PQCharA(S);
    System.SetLength(Result, S.Length);
    pd := PQCharW(Result);
    pds := pd;
    while p^ <> 0 do
    begin
      if p^ in [1 .. 127] then
      begin
        if not AIgnoreEnChars then
        begin
          pd^ := QCharW(CharUpperA(p^));
          Inc(pd);
        end;
        Inc(p);
      end
      else
      begin
        pd^ := SpellOfChar(ExchangeByteOrder(PWord(p)^));
        Inc(p, 2);
        if pd^ <> #0 then
          Inc(pd);
      end;
    end;
    System.SetLength(Result, pd - pds);
  end
  else
    System.SetLength(Result, 0);
end;

function CNSpellChars(S: QStringW; AIgnoreEnChars: Boolean): QStringW;
var
  pw, pd: PQCharW;
  T: QStringA;
begin
  pw := PWideChar(S);
  System.SetLength(Result, Length(S));
  pd := PQCharW(Result);
  while pw^ <> #0 do
  begin
    if pw^ < #127 then
    begin
      if not AIgnoreEnChars then
      begin
        pd^ := CharUpperW(pw^);
        Inc(pd);
      end;
    end
    else if (pw^ > #$4E00) and (pw^ <= #$9FA5) then // 汉字区间
    begin
      if Assigned(OnFetchCNSpell) then
      begin
        pd^ := OnFetchCNSpell(pw);
        if pd^ <> #0 then
        begin
          Inc(pd);
          Inc(pw);
          continue;
        end;
      end;
      T := AnsiEncode(pw, 1);
      if T.Length = 2 then
        pd^ := SpellOfChar(ExchangeByteOrder(PWord(PQCharA(T))^));
      if pd^ <> #0 then
        Inc(pd);
    end;
    Inc(pw);
  end;
  SetLength(Result, (IntPtr(pd) - IntPtr(PQCharW(Result))) shr 1);
end;

function CharSizeA(c: PQCharA): Integer;
begin
  { GB18030,兼容GBK和GB2312
    单字节，其值从0到0x7F。
    双字节，第一个字节的值从0x81到0xFE，第二个字节的值从0x40到0xFE（不包括0x7F）。
    四字节，第一个字节的值从0x81到0xFE，第二个字节的值从0x30到0x39，第三个字节从0x81到0xFE，第四个字节从0x30到0x39。
  }
{$IFDEF MSWINDOWS}
  if GetACP = 936 then
{$ELSE}
  if TEncoding.ANSI.CodePage = 936 then
{$ENDIF}
  begin
    Result := 1;
    if (c^ >= $81) and (c^ <= $FE) then
    begin
      Inc(c);
      if (c^ >= $40) and (c^ <= $FE) and (c^ <> $7F) then
        Result := 2
      else if (c^ >= $30) and (c^ <= $39) then
      begin
        Inc(c);
        if (c^ >= $81) and (c^ <= $FE) then
        begin
          Inc(c);
          if (c^ >= $30) and (c^ <= $39) then
            Result := 4;
        end;
      end;
    end;
  end
  else
{$IFDEF QDAC_ANSISTRINGS}
    Result := AnsiStrings.StrCharLength(PAnsiChar(c));
{$ELSE}
{$IFDEF NEXTGEN}
    if TEncoding.ANSI.CodePage = CP_UTF8 then
      Result := CharSizeU(c)
    else if (c^ < 128) or (TEncoding.ANSI.CodePage = 437) then
      Result := 1
    else
      Result := 2;
{$ELSE}
{$IF RTLVersion>=25}
    Result := AnsiStrings.StrCharLength(PAnsiChar(c));
{$ELSE}
    Result := sysutils.StrCharLength(PAnsiChar(c));
{$IFEND}
{$ENDIF}
{$ENDIF !QDAC_ANSISTRINGS}
end;

function CharSizeU(c: PQCharA): Integer;
begin
  if (c^ and $80) = 0 then
    Result := 1
  else
  begin
    if (c^ and $FC) = $FC then // 4000000+
      Result := 6
    else if (c^ and $F8) = $F8 then // 200000-3FFFFFF
      Result := 5
    else if (c^ and $F0) = $F0 then // 10000-1FFFFF
      Result := 4
    else if (c^ and $E0) = $E0 then // 800-FFFF
      Result := 3
    else if (c^ and $C0) = $C0 then // 80-7FF
      Result := 2
    else
      Result := 1;
  end
end;

function CharSizeW(c: PQCharW): Integer;
begin
  if (c[0] >= #$D800) and (c[0] <= #$DBFF) and (c[1] >= #$DC00) and
    (c[1] <= #$DFFF) then
    Result := 2
  else
    Result := 1;
end;

function CharCodeA(c: PQCharA): Cardinal;
var
  T: QStringA;
begin
  T := AnsiDecode(c, CharSizeA(c));
  Result := CharCodeW(PQCharW(T));
end;

function CharCodeU(c: PQCharA): Cardinal;
begin
  if (c^ and $80) <> 0 then
  begin
    if (c^ and $FC) = $FC then // 4000000+
    begin
      Result := (c^ and $03) shl 30;
      Inc(c);
      Result := Result or ((c^ and $3F) shl 24);
      Inc(c);
      Result := Result or ((c^ and $3F) shl 18);
      Inc(c);
      Result := Result or ((c^ and $3F) shl 12);
      Inc(c);
      Result := Result or ((c^ and $3F) shl 6);
      Inc(c);
      Result := Result or (c^ and $3F);
    end
    else if (c^ and $F8) = $F8 then // 200000-3FFFFFF
    begin
      Result := (c^ and $07) shl 24;
      Inc(c);
      Result := Result or ((c^ and $3F) shl 18);
      Inc(c);
      Result := Result or ((c^ and $3F) shl 12);
      Inc(c);
      Result := Result or ((c^ and $3F) shl 6);
      Inc(c);
      Result := Result or (c^ and $3F);
    end
    else if (c^ and $F0) = $F0 then // 10000-1FFFFF
    begin
      Result := (c^ and $0F) shr 18;
      Inc(c);
      Result := Result or ((c^ and $3F) shl 12);
      Inc(c);
      Result := Result or ((c^ and $3F) shl 6);
      Inc(c);
      Result := Result or (c^ and $3F);
    end
    else if (c^ and $E0) = $E0 then // 800-FFFF
    begin
      Result := (c^ and $1F) shl 12;
      Inc(c);
      Result := Result or ((c^ and $3F) shl 6);
      Inc(c);
      Result := Result or (c^ and $3F);
    end
    else if (c^ and $C0) = $C0 then // 80-7FF
    begin
      Result := (c^ and $3F) shl 6;
      Inc(c);
      Result := Result or (c^ and $3F);
    end
    else
      raise Exception.Create(Format(SBadUtf8Char, [IntPtr(c^)]));
  end
  else
    Result := c^;
end;

function CharCodeW(c: PQCharW): Cardinal;
begin
  if (c^ >= #$D800) and (c^ <= #$DFFF) then // Unicode 扩展区字符
  begin
    Result := (Ord(c^) - $D800);
    Inc(c);
    if (c^ >= #$DC00) and (c^ <= #$DFFF) then
    begin
      Result := $10000 + ((Result shl 10) + (Cardinal(Ord(c^)) - $DC00));
    end
    else
      Result := 0
  end
  else
    Result := Ord(c^);
end;

function CharCountA(const source: QStringA): Integer;
var
  p: PQCharA;
  l, ASize: Integer;
begin
  p := PQCharA(source);
  l := source.Length;
  Result := 0;
  while l > 0 do
  begin
    ASize := CharSizeA(p);
    Dec(l, ASize);
    Inc(p, ASize);
    Inc(Result);
  end;
  // Result:=TEncoding.ANSI.GetCharCount(source);
end;

function CharCountW(const S: QStringW): Integer;
var
  p, pe: PWord;
  ALen: Integer;
  procedure CountChar;
  begin
    if (p^ > $D800) and (p^ < $DFFF) then
    begin
      Inc(p);
      if (p^ >= $DC00) and (p^ < $DFFF) then
      begin
        Inc(p);
        Inc(Result);
      end
      else
        Result := -1;
    end
    else
    begin
      Inc(Result);
      Inc(p);
    end;
  end;

begin
  Result := 0;
  p := PWord(S);
  ALen := Length(S);
  pe := PWord(IntPtr(p) + (ALen shl 1));
  while IntPtr(p) < IntPtr(pe) do
    CountChar;
end;

function CharCountU(const source: QStringA): Integer;
var
  p, pe: PQCharA;
  procedure CountChar;
  begin
    if (p^ and $80) = 0 then
    begin
      Inc(Result);
      Inc(p);
    end
    else if (p^ and $FC) = $FC then
    begin
      Inc(Result);
      Inc(p, 6);
    end
    else if (p^ and $F8) = $F8 then
    begin
      Inc(Result);
      Inc(p, 5);
    end
    else if (p^ and $F0) = $F0 then
    begin
      Inc(Result);
      Inc(p, 4);
    end
    else if (p^ and $E0) = $E0 then
    begin
      Inc(Result);
      Inc(p, 3);
    end
    else if (p^ and $C0) = $C0 then
    begin
      Inc(Result);
      Inc(p, 2);
    end
    else
      Result := -1;
  end;

begin
  Result := 0;
  p := PQCharA(source);
  pe := PQCharA(IntPtr(p) + source.Length);
  while (IntPtr(p) < IntPtr(pe)) and (Result >= 0) do
    CountChar;
end;

procedure CalcCharLengthA(var Lens: TIntArray; const List: array of QCharA);
var
  I, l: Integer;
begin
  I := Low(List);
  System.SetLength(Lens, Length(List));
  while I <= High(List) do
  begin
    l := CharSizeA(@List[I]);
    Lens[I] := l;
    Inc(I, l);
  end;
end;

function CharInA(const c: PQCharA; const List: array of QCharA;
  ACharLen: PInteger): Boolean;
var
  I, count: Integer;
  Lens: TIntArray;
begin
  count := High(List) + 1;
  Result := false;
  CalcCharLengthA(Lens, List);
  I := Low(List);
  while I < count do
  begin
    if CompareMem(c, @List[I], Lens[I]) then
    begin
      if ACharLen <> nil then
        ACharLen^ := Lens[I];
      Result := True;
      Break;
    end
    else
      Inc(I, Lens[I]);
  end;
end;

procedure CalcCharLengthW(var Lens: TIntArray; const List: array of QCharW);
var
  I, l: Integer;
begin
  I := Low(List);
  System.SetLength(Lens, Length(List));
  while I <= High(List) do
  begin
    l := CharSizeW(@List[I]);
    Lens[I] := l;
    Inc(I, l);
  end;
end;

function CharInW(const c: PQCharW; const List: array of QCharW;
  ACharLen: PInteger): Boolean;
var
  I, count: Integer;
  Lens: TIntArray;
begin
  count := High(List) + 1;
  Result := false;
  CalcCharLengthW(Lens, List);
  I := Low(List);
  while I < count do
  begin
    if c^ = List[I] then
    begin
      if Lens[I] = 2 then
      begin
        Result := c[1] = List[I + 1];
        if Assigned(ACharLen) and Result then
          ACharLen^ := 2;
        if Result then
          Break;
      end
      else
      begin
        Result := True;
        if Assigned(ACharLen) then
          ACharLen^ := 1;
        Break;
      end;
    end;
    Inc(I, Lens[I]);
  end;
end;

function CharInW(const c, List: PQCharW; ACharLen: PInteger): Boolean;
var
  p: PQCharW;
begin
  Result := false;
  p := List;
  while p^ <> #0 do
  begin
    if p^ = c^ then
    begin
      if (p[0] >= #$D800) and (p[0] <= #$DBFF) then
      begin
        // (p[1] >= #$DC00) and (p[1] <= #$DFFF)
        if p[1] = c[1] then
        begin
          Result := True;
          if ACharLen <> nil then
            ACharLen^ := 2;
          Break;
        end;
      end
      else
      begin
        Result := True;
        if ACharLen <> nil then
          ACharLen^ := 1;
        Break;
      end;
    end;
    Inc(p);
  end;
end;

procedure CalcCharLengthU(var Lens: TIntArray; const List: array of QCharA);
var
  I, l: Integer;
begin
  I := Low(List);
  System.SetLength(Lens, Length(List));
  while I <= High(List) do
  begin
    l := CharSizeU(@List[I]);
    Lens[I] := l;
    Inc(I, l);
  end;
end;

function CharInU(const c: PQCharA; const List: array of QCharA;
  ACharLen: PInteger): Boolean;
var
  I, count: Integer;
  Lens: TIntArray;
begin
  count := High(List) + 1;
  Result := false;
  CalcCharLengthU(Lens, List);
  I := Low(List);
  while I < count do
  begin
    if CompareMem(c, @List[I], Lens[I]) then
    begin
      if ACharLen <> nil then
        ACharLen^ := Lens[I];
      Result := True;
      Break;
    end
    else
      Inc(I, Lens[I]);
  end;
end;

function IsSpaceA(const c: PQCharA; ASpaceSize: PInteger): Boolean;
begin
  if c^ in [9, 10, 13, 32] then
  begin
    Result := True;
    if Assigned(ASpaceSize) then
      ASpaceSize^ := 1;
  end
  else if (c^ = 161) and (PQCharA(IntPtr(c) + 1)^ = 161) then
  begin
    Result := True;
    if Assigned(ASpaceSize) then
      ASpaceSize^ := 2;
  end
  else
    Result := false;
end;

function IsSpaceW(const c: PQCharW; ASpaceSize: PInteger): Boolean;
begin
  Result := (c^ = #9) or (c^ = #10) or (c^ = #13) or (c^ = #32) or
    (c^ = #$3000);
  if Result and Assigned(ASpaceSize) then
    ASpaceSize^ := 1;
end;

function IsSpaceU(const c: PQCharA; ASpaceSize: PInteger): Boolean;
begin
  // 全角空格$3000的UTF-8编码是227,128,128
  if c^ in [9, 10, 13, 32] then
  begin
    Result := True;
    if Assigned(ASpaceSize) then
      ASpaceSize^ := 1;
  end
  else if (c^ = 227) and (PQCharA(IntPtr(c) + 1)^ = 128) and
    (PQCharA(IntPtr(c) + 2)^ = 128) then
  begin
    Result := True;
    if Assigned(ASpaceSize) then
      ASpaceSize^ := 3;
  end
  else
    Result := false;
end;

function CNFullToHalf(const S: QStringW): QStringW;
var
  p, pd: PWord;
  l: Integer;
begin
  l := Length(S);
  if l > 0 then
  begin
    System.SetLength(Result, l);
    p := PWord(PQCharW(S));
    pd := PWord(PQCharW(Result));
    while l > 0 do
    begin
      if (p^ = $3000) then // 全角空格'　'
        pd^ := $20
      else if (p^ >= $FF01) and (p^ <= $FF5E) then
        pd^ := $21 + (p^ - $FF01)
      else
        pd^ := p^;
      Dec(l);
      Inc(p);
      Inc(pd);
    end;
  end
  else
    System.SetLength(Result, 0);
end;

function CNHalfToFull(const S: QStringW): QStringW;
var
  p, pd: PWord;
  l: Integer;
begin
  l := Length(S);
  if l > 0 then
  begin
    System.SetLength(Result, l);
    p := PWord(PQCharW(S));
    pd := PWord(PQCharW(Result));
    while l > 0 do
    begin
      if p^ = $20 then // 全角空格'　'
        pd^ := $3000
      else if (p^ >= $21) and (p^ <= $7E) then
        pd^ := $FF01 + (p^ - $21)
      else
        pd^ := p^;
      Dec(l);
      Inc(p);
      Inc(pd);
    end;
  end
  else
    System.SetLength(Result, 0);
end;

function QuotedStrA(const S: QStringA; const AQuoter: QCharA): QStringA;
var
  p, pe, pd, pds: PQCharA;
begin
  p := PQCharA(S);
  Result.Length := S.Length shl 1;
  pe := p;
  Inc(pe, S.Length);
  pd := PQCharA(Result);
  pds := pd;
  pd^ := AQuoter;
  Inc(pd);
  while IntPtr(p) < IntPtr(pe) do
  begin
    if p^ = AQuoter then
    begin
      pd^ := AQuoter;
      Inc(pd);
      pd^ := AQuoter;
    end
    else
      pd^ := p^;
    Inc(pd);
    Inc(p);
  end;
  pd^ := AQuoter;
  Result.Length := IntPtr(pd) - IntPtr(pds) + 1;
end;

function QuotedStrW(const S: QStringW; const AQuoter: QCharW): QStringW;
var
  p, pe, pd, pds: PQCharW;
  l: Integer;
begin
  if AQuoter <> #0 then
  begin
    l := System.Length(S);
    p := PQCharW(S);
    SetLength(Result, (l + 1) shl 1);
    pe := p;
    Inc(pe, l);
    pd := PQCharW(Result);
    pds := pd;
    pd^ := AQuoter;
    Inc(pd);
    while IntPtr(p) < IntPtr(pe) do
    begin
      if p^ = AQuoter then
      begin
        pd^ := AQuoter;
        Inc(pd);
        pd^ := AQuoter;
      end
      else
        pd^ := p^;
      Inc(pd);
      Inc(p);
    end;
    pd^ := AQuoter;
    SetLength(Result, ((IntPtr(pd) - IntPtr(pds)) shr 1) + 1);
  end
  else
    Result := S;
end;

function SQLQuoted(const S: QStringW): QStringW;
begin
  Result := QuotedStrW(S);
end;

function DequotedStrA(const S: QStringA; const AQuoter: QCharA): QStringA;
var
  p, pe, pd, pds: PQCharA;
begin
  if (S.Length > 0) and (S[0] = AQuoter) and (S[S.Length - 1] = AQuoter) then
  begin
    p := PQCharA(S);
    pe := p;
    Inc(pe, S.Length);
    Inc(p);
    Result.Length := S.Length;
    pd := PQCharA(Result);
    pds := pd;
    while IntPtr(p) < IntPtr(pe) do
    begin
      if p^ = AQuoter then
      begin
        Inc(p);
        if p^ = AQuoter then
        begin
          pd^ := AQuoter;
        end
        else if IntPtr(p) < IntPtr(pe) then // 后面不是单引号,错误的字符串，直接拷贝内容
        begin
          pd^ := AQuoter;
          Inc(pd);
          pd^ := p^;
        end
        else
          Break;
      end
      else
        pd^ := p^;
      Inc(p);
      Inc(pd);
    end;
    Result.Length := IntPtr(pd) - IntPtr(pds);
  end
  else
    Result := S;
end;

function DequotedStrW(const S: QStringW; const AQuoter: QCharW): QStringW;
var
  p, pe, pd, pds: PQCharW;
begin
  if (Length(S) > 0) and (PQCharW(S)[0] = AQuoter) and
    (PQCharW(S)[Length(S) - 1] = AQuoter) then
  begin
    p := PQCharW(S);
    pe := p;
    Inc(pe, Length(S));
    Inc(p);
    SetLength(Result, Length(S));
    pd := PQCharW(Result);
    pds := pd;
    while IntPtr(p) < IntPtr(pe) do
    begin
      if p^ = AQuoter then
      begin
        Inc(p);
        if p^ = AQuoter then
        begin
          pd^ := AQuoter;
        end
        else if IntPtr(p) < IntPtr(pe) then // 后面不是单引号,错误的字符串，直接拷贝内容
        begin
          pd^ := AQuoter;
          Inc(pd);
          pd^ := p^;
        end
        else
          Break;
      end
      else
        pd^ := p^;
      Inc(p);
      Inc(pd);
    end;
    SetLength(Result, (IntPtr(pd) - IntPtr(pds)) shr 1);
  end
  else
    Result := S;
end;

function SkipCharA(var p: PQCharA; const List: array of QCharA): Integer;
var
  I, count: Integer;
  Lens: TIntArray;
  AFound: Boolean;
  ps: PQCharA;
begin
  count := High(List) + 1;
  Result := 0;
  if count > 0 then
  begin
    CalcCharLengthA(Lens, List);
    ps := p;
    while p^ <> 0 do
    begin
      I := Low(List);
      AFound := false;
      while I < count do
      begin
        if CompareMem(p, @List[I], Lens[I]) then
        begin
          AFound := True;
          Inc(p, Lens[I]);
          Break;
        end
        else
          Inc(I, Lens[I]);
      end;
      if not AFound then
      begin
        Result := IntPtr(p) - IntPtr(ps);
        Break;
      end;
    end;
  end;
end;

function SkipCharU(var p: PQCharA; const List: array of QCharA): Integer;
var
  I, count: Integer;
  Lens: TIntArray;
  AFound: Boolean;
  ps: PQCharA;
begin
  count := High(List) + 1;
  Result := 0;
  if count > 0 then
  begin
    CalcCharLengthU(Lens, List);
    ps := p;
    while p^ <> 0 do
    begin
      I := Low(List);
      AFound := false;
      while I < count do
      begin
        if CompareMem(p, @List[I], Lens[I]) then
        begin
          AFound := True;
          Inc(p, Lens[I]);
          Break;
        end
        else
          Inc(I, Lens[I]);
      end;
      if not AFound then
      begin
        Result := IntPtr(p) - IntPtr(ps);
        Break;
      end;
    end;
  end;
end;

function SkipCharW(var p: PQCharW; const List: array of QCharA): Integer;
var
  I, count: Integer;
  Lens: TIntArray;
  AFound: Boolean;
  ps: PQCharW;
begin
  count := High(List) + 1;
  Result := 0;
  if count > 0 then
  begin
    CalcCharLengthA(Lens, List);
    ps := p;
    while p^ <> #0 do
    begin
      I := Low(List);
      AFound := false;
      while I < count do
      begin
        if CompareMem(p, @List[I], Lens[I] shl 1) then
        begin
          AFound := True;
          Break;
        end
        else
          Inc(I, Lens[I]);
      end;
      if AFound then
        Inc(p)
      else
      begin
        Result := IntPtr(p) - IntPtr(ps);
        Break;
      end;
    end;
  end;
end;

function SkipCharW(var p: PQCharW; const List: PQCharW): Integer;
var
  l: Integer;
  ps: PQCharW;
begin
  Result := 0;
  if (List <> nil) and (List^ <> #0) then
  begin
    ps := p;
    while p^ <> #0 do
    begin
      if CharInW(p, List, @l) then
        Inc(p, l)
      else
      begin
        Result := IntPtr(p) - IntPtr(ps);
        Break;
      end;
    end;
  end;
end;

function SkipSpaceA(var p: PQCharA): Integer;
var
  ps: PQCharA;
  l: Integer;
begin
  ps := p;
  while p^ <> 0 do
  begin
    if IsSpaceA(p, @l) then
      Inc(p, l)
    else
      Break;
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

function SkipSpaceU(var p: PQCharA): Integer;
var
  ps: PQCharA;
  l: Integer;
begin
  ps := p;
  while p^ <> 0 do
  begin
    if IsSpaceU(p, @l) then
      Inc(p, l)
    else
      Break;
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

function SkipSpaceW(var p: PQCharW): Integer;
var
  ps: PQCharW;
  l: Integer;
begin
  ps := p;
  while p^ <> #0 do
  begin
    if IsSpaceW(p, @l) then
      Inc(p, l)
    else
      Break;
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

// 跳过一行,以#10为行结尾
function SkipLineA(var p: PQCharA): Integer;
var
  ps: PQCharA;
begin
  ps := p;
  while p^ <> 0 do
  begin
    if p^ = 10 then
    begin
      Inc(p);
      Break;
    end
    else
      Inc(p);
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

function SkipLineU(var p: PQCharA): Integer;
begin
  Result := SkipLineA(p);
end;

function SkipLineW(var p: PQCharW): Integer;
var
  ps: PQCharW;
begin
  ps := p;
  while p^ <> #0 do
  begin
    if p^ = #10 then
    begin
      Inc(p);
      Break;
    end
    else
      Inc(p);
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

function StrPosA(Start, Current: PQCharA; var ACol, ARow: Integer): PQCharA;
begin
  ACol := 1;
  ARow := 1;
  Result := Start;
  while IntPtr(Start) < IntPtr(Current) do
  begin
    if Start^ = 10 then
    begin
      Inc(ARow);
      ACol := 1;
      Inc(Start);
      Result := Start;
    end
    else
    begin
      Inc(Start, CharSizeA(Start));
      Inc(ACol);
    end;
  end;
end;

function StrPosU(Start, Current: PQCharA; var ACol, ARow: Integer): PQCharA;
begin
  ACol := 1;
  ARow := 1;
  Result := Start;
  while IntPtr(Start) < IntPtr(Current) do
  begin
    if Start^ = 10 then
    begin
      Inc(ARow);
      ACol := 1;
      Inc(Start);
      Result := Start;
    end
    else
    begin
      Inc(Start, CharSizeU(Start));
      Inc(ACol);
    end;
  end;
end;

function StrPosW(Start, Current: PQCharW; var ACol, ARow: Integer): PQCharW;
begin
  ACol := 1;
  ARow := 1;
  Result := Start;
  while Start < Current do
  begin
    if Start^ = #10 then
    begin
      Inc(ARow);
      ACol := 1;
      Inc(Start);
      Result := Start;
    end
    else
    begin
      Inc(Start, CharSizeW(Start));
      Inc(ACol);
    end;
  end;
end;

function DecodeTokenA(var p: PQCharA; ADelimiters: array of QCharA;
  AQuoter: QCharA; AIgnoreSpace: Boolean): QStringA;
var
  S: PQCharA;
  l: Integer;
begin
  if AIgnoreSpace then
    SkipSpaceA(p);
  S := p;
  while p^ <> 0 do
  begin
    if p^ = AQuoter then // 引用的内容不拆分
    begin
      Inc(p);
      while p^ <> 0 do
      begin
        if p^ = $5C then
        begin
          Inc(p);
          if p^ <> 0 then
            Inc(p);
        end
        else if p^ = AQuoter then
        begin
          Inc(p);
          if p^ = AQuoter then
            Inc(p)
          else
            Break;
        end
        else
          Inc(p);
      end;
    end
    else if CharInA(p, ADelimiters, @l) then
      Break
    else // \",\',"",''分别解析转义
      Inc(p);
  end;
  l := IntPtr(p) - IntPtr(S);
  Result.Length := l;
  Move(S^, PQCharA(Result)^, l);
  while CharInA(p, ADelimiters, @l) do
    Inc(p, l);
end;

function DecodeTokenU(var p: PQCharA; ADelimiters: array of QCharA;
  AQuoter: QCharA; AIgnoreSpace: Boolean): QStringA;
var
  S: PQCharA;
  l: Integer;
begin
  if AIgnoreSpace then
    SkipSpaceU(p);
  S := p;
  while p^ <> 0 do
  begin
    if p^ = AQuoter then // 引用的内容不拆分
    begin
      Inc(p);
      while p^ <> 0 do
      begin
        if p^ = $5C then
        begin
          Inc(p);
          if p^ <> 0 then
            Inc(p);
        end
        else if p^ = AQuoter then
        begin
          Inc(p);
          if p^ = AQuoter then
            Inc(p)
          else
            Break;
        end
        else
          Inc(p);
      end;
    end
    else if CharInU(p, ADelimiters, @l) then
      Break
    else // \",\',"",''分别解析转义
      Inc(p);
  end;
  l := IntPtr(p) - IntPtr(S);
  Result.Length := l;
  Move(S^, PQCharA(Result)^, l);
  while CharInU(p, ADelimiters, @l) do
    Inc(p, l);
end;

function DecodeTokenW(var p: PQCharW; ADelimiters: array of QCharW;
  AQuoter: QCharW; AIgnoreSpace: Boolean; ASkipDelimiters: Boolean): QStringW;
var
  S: PQCharW;
  l: Integer;
begin
  if AIgnoreSpace then
    SkipSpaceW(p);
  S := p;
  while p^ <> #0 do
  begin
    if p^ = AQuoter then // 引用的内容不拆分
    begin
      Inc(p);
      while p^ <> #0 do
      begin
        if p^ = #$5C then
        begin
          Inc(p);
          if p^ <> #0 then
            Inc(p);
        end
        else if p^ = AQuoter then
        begin
          Inc(p);
          if p^ = AQuoter then
            Inc(p)
          else
            Break;
        end
        else
          Inc(p);
      end;
    end
    else if CharInW(p, ADelimiters, @l) then
      Break
    else // \",\',"",''分别解析转义
      Inc(p);
  end;
  l := p - S;
  SetLength(Result, l);
  Move(S^, PQCharW(Result)^, l shl 1);
  if ASkipDelimiters then
  begin
    while CharInW(p, ADelimiters, @l) do
      Inc(p, l);
  end;
  if AIgnoreSpace then
    SkipSpaceW(p);
end;

function DecodeTokenW(var p: PQCharW; ADelimiters: PQCharW; AQuoter: QCharW;
  AIgnoreSpace: Boolean; ASkipDelimiters: Boolean): QStringW;
var
  S: PQCharW;
  l: Integer;
begin
  if AIgnoreSpace then
    SkipSpaceW(p);
  S := p;
  while p^ <> #0 do
  begin
    if p^ = AQuoter then // 引用的内容不拆分
    begin
      Inc(p);
      while p^ <> #0 do
      begin
        if p^ = #$5C then
        begin
          Inc(p);
          if p^ <> #0 then
            Inc(p);
        end
        else if p^ = AQuoter then
        begin
          Inc(p);
          if p^ = AQuoter then
            Inc(p)
          else
            Break;
        end
        else
          Inc(p);
      end;
    end
    else if CharInW(p, ADelimiters, @l) then
      Break
    else // \",\',"",''分别解析转义
      Inc(p);
  end;
  l := p - S;
  SetLength(Result, l);
  Move(S^, PQCharW(Result)^, l shl 1);
  if ASkipDelimiters then
  begin
    while CharInW(p, ADelimiters, @l) do
      Inc(p, l);
  end;
  if AIgnoreSpace then
    SkipSpaceW(p);
end;

function DecodeTokenW(var S: QStringW; ADelimiters: PQCharW; AQuoter: QCharW;
  AIgnoreCase, ARemove, ASkipDelimiters: Boolean): QStringW;
var
  p: PQCharW;
begin
  p := PQCharW(S);
  Result := DecodeTokenW(p, ADelimiters, AQuoter, AIgnoreCase, ASkipDelimiters);
  if ARemove then
    S := StrDupX(p, Length(S) - (p - PQCharW(S)));
end;

function SplitTokenW(AList: TStrings; p: PQCharW; ADelimiters: PQCharW;
  AQuoter: QCharW; AIgnoreSpace: Boolean): Integer;
begin
  Result := 0;
  AList.BeginUpdate;
  try
    while p^ <> #0 do
    begin
      AList.Add(DecodeTokenW(p, ADelimiters, AQuoter, AIgnoreSpace, True));
      Inc(Result);
    end;
  finally
    AList.EndUpdate;
  end;
end;

function SplitTokenW(AList: TStrings; const S: QStringW; ADelimiters: PQCharW;
  AQuoter: QCharW; AIgnoreSpace: Boolean): Integer;
begin
  Result := SplitTokenW(AList, PQCharW(S), ADelimiters, AQuoter, AIgnoreSpace);
end;

function StrBeforeW(var source: PQCharW; const ASpliter: QStringW;
  AIgnoreCase, ARemove: Boolean; AMustMatch: Boolean = false): QStringW;
var
  pe: PQCharW;
  len: Integer;
begin
  if Assigned(source) then
  begin
    if AIgnoreCase then
      pe := StrIStrW(source, PQCharW(ASpliter))
    else
      pe := StrStrW(source, PQCharW(ASpliter));
    if Assigned(pe) then
    begin
      len := (IntPtr(pe) - IntPtr(source)) shr 1;
      Result := StrDupX(source, len);
      if ARemove then
      begin
        Inc(pe, Length(ASpliter));
        source := pe;
      end;
    end
    else if not AMustMatch then
    begin
      Result := source;
      if ARemove then
        source := nil;
    end
    else
      SetLength(Result, 0);
  end
  else
    SetLength(Result, 0);
end;

function StrBeforeW(var source: QStringW; const ASpliter: QStringW;
  AIgnoreCase, ARemove: Boolean; AMustMatch: Boolean): QStringW;
var
  p, pe: PQCharW;
  len: Integer;
begin
  p := PQCharW(source);
  if AIgnoreCase then
    pe := StrIStrW(p, PQCharW(ASpliter))
  else
    pe := StrStrW(p, PQCharW(ASpliter));
  if Assigned(pe) then
  begin
    len := (IntPtr(pe) - IntPtr(p)) shr 1;
    Result := StrDupX(p, len);
    if ARemove then
    begin
      Inc(pe, Length(ASpliter));
      len := Length(source) - len - Length(ASpliter);
      Move(pe^, p^, len shl 1);
      SetLength(source, len);
    end;
  end
  else if not AMustMatch then
  begin
    Result := source;
    if ARemove then
      SetLength(source, 0);
  end
  else
    SetLength(Result, 0);
end;

function SplitByStrW(AList: TStrings; ASource: QStringW;
  const ASpliter: QStringW; AIgnoreCase: Boolean): Integer;
var
  p: PQCharW;
begin
  if Length(ASource) > 0 then
  begin
    p := PQCharW(ASource);
    Result := 0;
    AList.BeginUpdate;
    try
      while Assigned(p) do
      begin
        AList.Add(StrBeforeW(p, ASpliter, AIgnoreCase, True, false));
        Inc(Result);
      end;
    finally
      AList.EndUpdate;
    end;
  end
  else
    Result := 0;
end;

function UpperFirstW(const S: QStringW): QStringW;
var
  p, pd: PQCharW;
begin
  if Length(S) > 0 then
  begin
    p := PQCharW(S);
    SetLength(Result, Length(S));
    pd := PQCharW(Result);
    pd^ := CharUpperW(p^);
    Inc(p);
    Inc(pd);
    while p^ <> #0 do
    begin
      pd^ := CharLowerW(p^);
      Inc(p);
      Inc(pd);
    end;
  end
  else
    Result := S;
end;

function DecodeLineA(var p: PQCharA; ASkipEmpty: Boolean; AMaxSize: Integer)
  : QStringA;
var
  ps: PQCharA;
begin
  ps := p;
  while p^ <> 0 do
  begin
    if ((p^ = 13) and (PQCharA(IntPtr(p) + 1)^ = 10)) or (p^ = 10) then
    begin
      if ps = p then
      begin
        if ASkipEmpty then
        begin
          if p^ = 13 then
            Inc(p, 2)
          else
            Inc(p);
          ps := p;
        end
        else
        begin
          Result.Length := 0;
          Exit;
        end;
      end
      else
      begin
        Result.Length := IntPtr(p) - IntPtr(ps);
        Move(ps^, PQCharA(Result)^, IntPtr(p) - IntPtr(ps));
        if p^ = 13 then
          Inc(p, 2)
        else
          Inc(p);
        Exit;
      end;
    end
    else
      Inc(p);
  end;
  if ps = p then
    Result.Length := 0
  else
  begin
    Result.Length := IntPtr(p) - IntPtr(ps);
    Move(ps^, PQCharA(Result)^, IntPtr(p) - IntPtr(ps));
  end;
  if Result.Length > AMaxSize then
  begin
    Move(Result.FValue[Result.Length - AMaxSize + 3], Result.FValue[4],
      AMaxSize - 3);
    Result.FValue[1] := $2E; // ...
    Result.FValue[2] := $2E;
    Result.FValue[3] := $2E;
  end;
end;

function DecodeLineU(var p: PQCharA; ASkipEmpty: Boolean; AMaxSize: Integer)
  : QStringA;
begin
  Result := DecodeLineA(p, ASkipEmpty, MaxInt);
  if Result.Length > 0 then
  begin
    Result.FValue[0] := 1;
    if Result.Length > AMaxSize then
    begin
      Move(Result.FValue[Result.Length - AMaxSize + 3], Result.FValue[4],
        AMaxSize - 3);
      Result.FValue[1] := $2E; // ...
      Result.FValue[2] := $2E;
      Result.FValue[3] := $2E;
    end;
  end;
end;

function DecodeLineW(var p: PQCharW; ASkipEmpty: Boolean; AMaxSize: Integer;
  AQuoterChar: QCharW): QStringW;
var
  ps: PQCharW;
begin
  ps := p;
  while p^ <> #0 do
  begin
    if p^ = AQuoterChar then
    begin
      Inc(p);
      while p^ <> #0 do
      begin
        if p^ = #$5C then
        begin
          Inc(p);
          if p^ <> #0 then
            Inc(p);
        end
        else if p^ = AQuoterChar then
        begin
          Inc(p);
          if p^ = AQuoterChar then
            Inc(p)
          else
            Break;
        end
        else
          Inc(p);
      end;
    end;
    if ((p[0] = #13) and (p[1] = #10)) or (p[0] = #10) then
    begin
      if ps = p then
      begin
        if ASkipEmpty then
        begin
          if p^ = #13 then
            Inc(p, 2)
          else
            Inc(p);
          ps := p;
        end
        else
        begin
          SetLength(Result, 0);
          Exit;
        end;
      end
      else
      begin
        SetLength(Result, p - ps);
        Move(ps^, PQCharW(Result)^, IntPtr(p) - IntPtr(ps));
        if p^ = #13 then
          Inc(p, 2)
        else
          Inc(p);
        Exit;
      end;
    end
    else
      Inc(p);
  end;
  if ps = p then
    SetLength(Result, 0)
  else
  begin
    SetLength(Result, p - ps);
    Move(ps^, PQCharW(Result)^, IntPtr(p) - IntPtr(ps));
  end;
  if Length(Result) > AMaxSize then
    Result := '...' + RightStrW(Result, AMaxSize - 3, True);
end;

function LeftStrW(const S: QStringW; AMaxCount: Integer; ACheckExt: Boolean)
  : QStringW;
var
  ps, p: PQCharW;
  l: Integer;
begin
  l := Length(S);
  if AMaxCount > l then
    Result := S
  else if AMaxCount > 0 then
  begin
    ps := PQCharW(S);
    if ACheckExt then
    begin
      p := ps;
      while (p^ <> #0) and (AMaxCount > 0) do
      begin
        if (p^ >= #$D800) and (p^ <= #$DBFF) then
        begin
          Inc(p);
          if (p^ >= #$DC00) and (p^ <= #$DFFF) then
            Inc(p);
          // else 无效的扩展区字符，仍然循环保留
        end
        else
          Inc(p);
        Dec(AMaxCount);
      end;
      l := p - ps;
      SetLength(Result, l);
      Move(ps^, PQCharW(Result)^, l shl 1);
    end
    else
    begin
      SetLength(Result, AMaxCount);
      Move(ps^, PQCharW(Result)^, AMaxCount shl 1);
    end;
  end
  else
    SetLength(Result, 0);
end;

function LeftStrW(var S: QStringW; const ADelimiters: QStringW;
  ARemove: Boolean): QStringW;
begin
  Result := DecodeTokenW(S, PQCharW(ADelimiters), QCharW(#0), false, ARemove);
end;

function RightStrW(const S: QStringW; AMaxCount: Integer; ACheckExt: Boolean)
  : QStringW;
var
  ps, p: PQCharW;
  l: Integer;
begin
  l := Length(S);
  if AMaxCount > l then
    Result := S
  else if AMaxCount > 0 then
  begin
    ps := PQCharW(S);
    if ACheckExt then
    begin
      p := ps + l - 1;
      while (p > ps) and (AMaxCount > 0) do
      begin
        if (p^ >= #$DC00) and (p^ <= #$DFFF) then
        begin
          Dec(p);
          if (p^ >= #$D800) and (p^ <= #$DBFF) then
            Dec(p)
            // else 无效的扩展区字符，仍然循环保留
        end
        else
          Dec(p);
        Dec(AMaxCount);
      end;
      Inc(p);
      l := l - (p - ps);
      SetLength(Result, l);
      Move(p^, PQCharW(Result)^, l shl 1);
    end
    else
    begin
      Inc(ps, l - AMaxCount);
      SetLength(Result, AMaxCount);
      Move(ps^, PQCharW(Result)^, AMaxCount shl 1);
    end;
  end
  else
    SetLength(Result, 0);
end;

function RightStrW(var S: QStringW; const ADelimiters: QStringW;
  ARemove: Boolean): QStringW;
var
  ps, pe, pd: PQCharW;
begin
  ps := PQCharW(S);
  pe := PQCharW(IntPtr(ps) + (Length(S) shl 1));
  pd := PQCharW(ADelimiters);
  while pe > ps do
  begin
    Dec(pe);
    if CharInW(pe, pd) then
    begin
      Inc(pe);
      Result := StrDupX(pe, (IntPtr(ps) + (Length(S) shl 1) -
        IntPtr(pe)) shr 1);
      if ARemove then
        S := StrDupX(ps, (IntPtr(pe) - IntPtr(ps)) shr 1);
      Exit;
    end;
  end;
  Result := S;
  if ARemove then
    SetLength(S, 0);
end;

function StrBetween(var S: PQCharW; AStartTag, AEndTag: QStringW;
  AIgnoreCase: Boolean): QStringW;
var
  ps, pe: PQCharW;
  l: Integer;
begin
  if AIgnoreCase then
  begin
    ps := StrIStrW(S, PQCharW(AStartTag));
    if ps <> nil then
    begin
      Inc(ps, Length(AStartTag));
      pe := StrIStrW(ps, PQCharW(AEndTag));
      if pe <> nil then
      begin
        l := pe - ps;
        SetLength(Result, l);
        Move(ps^, PQCharW(Result)^, l shl 1);
        Inc(pe, Length(AEndTag));
        S := pe;
      end
      else
      begin
        SetLength(Result, 0);
        while S^ <> #0 do
          Inc(S);
      end;
    end
    else
    begin
      SetLength(Result, 0);
      while S^ <> #0 do
        Inc(S);
    end;
  end
  else
  begin
    ps := StrStrW(S, PQCharW(AStartTag));
    if ps <> nil then
    begin
      Inc(ps, Length(AStartTag));
      pe := StrStrW(ps, PQCharW(AEndTag));
      if pe <> nil then
      begin
        l := pe - ps;
        SetLength(Result, l);
        Move(ps^, PQCharW(Result)^, l shl 1);
        Inc(pe, Length(AEndTag));
        S := pe;
      end
      else
      begin
        SetLength(Result, 0);
        while S^ <> #0 do
          Inc(S);
      end
    end
    else
    begin
      SetLength(Result, 0);
      while S^ <> #0 do
        Inc(S);
    end;
  end;
end;

function StrStrX(s1, s2: PQCharW; l1, l2: Integer;
  AIgnoreCase: Boolean): PQCharW;
var
  p1, p2: PQCharW;
begin
  Result := nil;
  while (s1^ <> #0) and (l1 >= l2) do
  begin
    if (s1^ = s2^) or
      (AIgnoreCase and (QString.CharUpperW(s1^) = QString.CharUpperW(s2^))) then
    begin
      p1 := s1;
      p2 := s2;
      repeat
        Inc(p1);
        Inc(p2);
        if p2^ = #0 then
        begin
          Result := s1;
          Exit;
        end
        else if (p1^ = p2^) or
          (AIgnoreCase and (QString.CharUpperW(p1^) = QString.CharUpperW(p2^)))
        then
          continue
        else
          Break;
      until 1 > 2;
    end;
    Inc(s1);
  end;
end;

function StrStrRX(s1, s2: PQCharW; l1, l2: Integer;
  AIgnoreCase: Boolean): PQCharW;
var
  ps1, ps2, p1, p2: PQCharW;
begin
  Result := nil;
  ps1 := s1 - 1;
  ps2 := s2 - 1;
  s1 := s1 - l1;
  s2 := s2 - l2;
  while (ps1 >= s1) and (l1 >= l2) do
  begin
    if (ps1^ = ps2^) or
      (AIgnoreCase and (QString.CharUpperW(ps1^) = QString.CharUpperW(ps2^)))
    then
    begin
      p1 := ps1;
      p2 := ps2;
      while (p2 > s2) do
      begin
        Dec(p1);
        Dec(p2);
        if (p1^ = p2^) or
          (AIgnoreCase and (QString.CharUpperW(p1^) = QString.CharUpperW(p2^)))
        then
          continue
        else
          Break;
      end;
      if p2 = s2 then
      begin
        Result := ps1;
        Exit;
      end;
    end;
    Dec(ps1);
  end;
end;

function StrBetweenTimes(const S, ADelimiter: QStringW; AIgnoreCase: Boolean;
  AStartTimes: Integer = 0; AStopTimes: Integer = 1): QStringW;
var
  p, ps, pl, pd: PQCharW;
  AStep, L1, L2, ATimes: Integer;
begin
  ps := PQCharW(S);
  pd := PQCharW(ADelimiter);
  L1 := Length(S);
  L2 := Length(ADelimiter);
  ATimes := 0;
  if AStopTimes > 0 then
  begin
    // 查找起始位置
    p := ps;
    while ATimes < AStartTimes do
    begin
      pl := p;
      p := StrStrX(p, pd, L1, L2, AIgnoreCase);
      if Assigned(p) then
      begin
        Inc(p, L2);
        Dec(L1, (IntPtr(p) - IntPtr(pl)) shr 1);
        Inc(ATimes);
      end
      else
        Break;
    end;
    // 查找结束位置
    ps := p;
    while ATimes < AStopTimes do
    begin
      pl := p;
      p := StrStrX(p, pd, L1, L2, AIgnoreCase);
      if Assigned(p) then
      begin
        Inc(p, Length(ADelimiter));
        Dec(L1, (IntPtr(p) - IntPtr(pl)) shr 1);
        Inc(ATimes);
      end
      else
        Break;
    end;
    if Assigned(p) then
      Result := StrDupX(ps, (IntPtr(p) - IntPtr(ps)) shr 1 - L2)
    else
      Result := S;
  end
  else if AStopTimes < 0 then // 从右向左找
  begin
    p := ps + L1;
    while ATimes > AStartTimes do
    begin
      pl := p;
      p := StrStrRX(p, pd + L2, L1, L2, AIgnoreCase);
      if Assigned(p) then
      begin
        Dec(L1, (IntPtr(pl) - IntPtr(p)) shr 1);
        Dec(ATimes);
      end
      else
        Break;
    end;
    ps := p;
    while ATimes > AStopTimes do
    begin
      pl := p;
      p := StrStrRX(p, pd + L2, L1, L2, AIgnoreCase);
      if Assigned(p) then
      begin
        Dec(L1, (IntPtr(pl) - IntPtr(p)) shr 1);
        Dec(ATimes);
      end
      else
        Break;
    end;
    if Assigned(p) then
      Result := StrDupX(p + L2, (IntPtr(ps) - IntPtr(p)) shr 1 - L2)
    else
      Result := S;
  end;
end;

function TokenWithIndex(var S: PQCharW; AIndex: Integer; ADelimiters: PQCharW;
  AQuoter: QCharW; AIgnoreSapce: Boolean): QStringW;
begin
  SetLength(Result, 0);
  while (AIndex >= 0) and (S^ <> #0) do
  begin
    if AIndex <> 0 then
      DecodeTokenW(S, ADelimiters, AQuoter, AIgnoreSapce)
    else
    begin
      Result := DecodeTokenW(S, ADelimiters, AQuoter, AIgnoreSapce);
      Break;
    end;
    Dec(AIndex);
  end;
end;

function SkipUntilA(var p: PQCharA; AExpects: array of QCharA;
  AQuoter: QCharA): Integer;
var
  ps: PQCharA;
begin
  ps := p;
  while p^ <> 0 do
  begin
    if (p^ = AQuoter) then
    begin
      Inc(p);
      while p^ <> 0 do
      begin
        if p^ = $5C then
        begin
          Inc(p);
          if p^ <> 0 then
            Inc(p);
        end
        else if p^ = AQuoter then
        begin
          Inc(p);
          if p^ = AQuoter then
            Inc(p)
          else
            Break;
        end
        else
          Inc(p);
      end;
    end
    else if CharInA(p, AExpects) then
      Break
    else
      Inc(p, CharSizeA(p));
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

function SkipUntilU(var p: PQCharA; AExpects: array of QCharA;
  AQuoter: QCharA): Integer;
var
  ps: PQCharA;
begin
  ps := p;
  while p^ <> 0 do
  begin
    if (p^ = AQuoter) then
    begin
      Inc(p);
      while p^ <> 0 do
      begin
        if p^ = $5C then
        begin
          Inc(p);
          if p^ <> 0 then
            Inc(p);
        end
        else if p^ = AQuoter then
        begin
          Inc(p);
          if p^ = AQuoter then
            Inc(p)
          else
            Break;
        end
        else
          Inc(p);
      end;
    end
    else if CharInU(p, AExpects) then
      Break
    else
      Inc(p, CharSizeU(p));
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

function SkipUntilW(var p: PQCharW; AExpects: array of QCharW;
  AQuoter: QCharW): Integer;
var
  ps: PQCharW;
begin
  ps := p;
  while p^ <> #0 do
  begin
    if (p^ = AQuoter) then
    begin
      Inc(p);
      while p^ <> #0 do
      begin
        if p^ = #$5C then
        begin
          Inc(p);
          if p^ <> #0 then
            Inc(p);
        end
        else if p^ = AQuoter then
        begin
          Inc(p);
          if p^ = AQuoter then
            Inc(p)
          else
            Break;
        end
        else
          Inc(p);
      end;
    end
    else if CharInW(p, AExpects) then
      Break
    else
      Inc(p, CharSizeW(p));
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

function SkipUntilW(var p: PQCharW; AExpects: PQCharW; AQuoter: QCharW)
  : Integer;
var
  ps: PQCharW;
begin
  ps := p;
  while p^ <> #0 do
  begin
    if (p^ = AQuoter) then
    begin
      Inc(p);
      while p^ <> #0 do
      begin
        if p^ = #$5C then
        begin
          Inc(p);
          if p^ <> #0 then
            Inc(p);
        end
        else if p^ = AQuoter then
        begin
          Inc(p);
          if p^ = AQuoter then
            Inc(p)
          else
            Break;
        end
        else
          Inc(p);
      end;
    end
    else if CharInW(p, AExpects) then
      Break
    else
      Inc(p, CharSizeW(p));
  end;
  Result := (IntPtr(p) - IntPtr(ps)) shr 1;
end;

function CharUpperA(c: QCharA): QCharA;
begin
  if (c >= $61) and (c <= $7A) then
    Result := c xor $20
  else
    Result := c;
end;

function CharUpperW(c: QCharW): QCharW;
begin
  if (c >= #$61) and (c <= #$7A) then
    Result := QCharW(PWord(@c)^ xor $20)
  else
    Result := c;
end;

function CharLowerA(c: QCharA): QCharA;
begin
  if (c >= Ord('A')) and (c <= Ord('Z')) then
    Result := Ord('a') + Ord(c) - Ord('A')
  else
    Result := c;
end;

function CharLowerW(c: QCharW): QCharW;
begin
  if (c >= 'A') and (c <= 'Z') then
    Result := QCharW(Ord('a') + Ord(c) - Ord('A'))
  else
    Result := c;
end;

function StartWithA(S, startby: PQCharA; AIgnoreCase: Boolean): Boolean;
begin
  while (S^ <> 0) and (startby^ <> 0) do
  begin
    if AIgnoreCase then
    begin
      if CharUpperA(S^) <> CharUpperA(startby^) then
        Break;
    end
    else if S^ <> startby^ then
      Break;
    Inc(S);
    Inc(startby);
  end;
  Result := (startby^ = 0);
end;

function StartWithU(S, startby: PQCharA; AIgnoreCase: Boolean): Boolean;
begin
  Result := StartWithA(S, startby, AIgnoreCase);
end;

function StartWithW(S, startby: PQCharW; AIgnoreCase: Boolean): Boolean;
begin
  if AIgnoreCase then
  begin
    while (S^ <> #0) and (startby^ <> #0) do
    begin
      if CharUpperW(S^) <> CharUpperW(startby^) then
        Break;
      Inc(S);
      Inc(startby);
    end;
  end
  else
  begin
    while (S^ <> #0) and (S^ = startby^) do
    begin
      Inc(S);
      Inc(startby);
    end;
  end;
  Result := (startby^ = #0);
end;

function EndWithA(const S, endby: QStringA; AIgnoreCase: Boolean): Boolean;
var
  p: PQCharA;
begin
  if S.Length < endby.Length then
    Result := false
  else
  begin
    p := PQCharA(S);
    Inc(p, S.Length - endby.Length);
    if AIgnoreCase then
      Result := (StrIStrA(p, PQCharA(endby)) = p)
    else
      Result := (StrStrA(p, PQCharA(endby)) = p);
  end;
end;

function EndWithU(const S, endby: QStringA; AIgnoreCase: Boolean): Boolean;
begin
  Result := EndWithA(S, endby, AIgnoreCase);
end;

function EndWithW(const S, endby: QStringW; AIgnoreCase: Boolean): Boolean;
var
  p: PQCharW;
begin
  if System.Length(S) < System.Length(endby) then
    Result := false
  else
  begin
    p := PQCharW(S);
    Inc(p, System.Length(S) - System.Length(endby));
    if AIgnoreCase then
      Result := (StrIStrW(p, PQCharW(endby)) = p)
    else
      Result := (StrStrW(p, PQCharW(endby)) = p);
  end;
end;

function SameCharsA(s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
begin
  Result := 0;
  if (s1 <> nil) and (s2 <> nil) then
  begin
    if AIgnoreCase then
    begin
      while (s1^ <> 0) and (s2^ <> 0) and
        ((s1^ = s2^) or (CharUpperA(s1^) = CharUpperA(s2^))) do
      begin
        Inc(Result);
        Inc(s1);
        Inc(s2);
      end;
    end
    else
    begin
      while (s1^ <> 0) and (s2^ <> 0) and (s1^ = s2^) do
      begin
        Inc(Result);
        Inc(s1);
        Inc(s2);
      end;
    end;
  end;
end;

function SameCharsU(s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
  function CompareSubSeq: Integer;
  var
    ACharSize1, ACharSize2: Integer;
  begin
    ACharSize1 := CharSizeU(s1) - 1;
    ACharSize2 := CharSizeU(s2) - 1;
    Result := 0;
    if ACharSize1 = ACharSize2 then
    begin
      Inc(s1);
      Inc(s2);
      while (ACharSize1 > 0) and (s1^ = s2^) do
      begin
        Inc(s1);
        Inc(s2);
      end;
      if ACharSize1 = 0 then
        Result := ACharSize2 + 1;
    end;
  end;

var
  ACharSize: Integer;
begin
  Result := 0;
  if (s1 <> nil) and (s2 <> nil) then
  begin
    if AIgnoreCase then
    begin
      while (s1^ <> 0) and (s2^ <> 0) and
        ((s1^ = s2^) or (CharUpperA(s1^) = CharUpperA(s2^))) do
      begin
        ACharSize := CompareSubSeq;
        if ACharSize <> 0 then
        begin
          Inc(Result);
          Inc(s1, ACharSize);
          Inc(s2, ACharSize);
        end
        else
          Break;
      end;
    end
    else
    begin
      while (s1^ <> 0) and (s2^ <> 0) and (s1^ = s2^) do
      begin
        ACharSize := CompareSubSeq;
        if ACharSize <> 0 then
        begin
          Inc(Result);
          Inc(s1, ACharSize);
          Inc(s2, ACharSize);
        end
        else
          Break;
      end;
    end;
  end;
end;

function SameCharsW(s1, s2: PQCharW; AIgnoreCase: Boolean): Integer;
begin
  Result := 0;
  if (s1 <> nil) and (s2 <> nil) then
  begin
    if AIgnoreCase then
    begin
      while (s1^ <> #0) and (s2^ <> #0) and
        ((s1^ = s2^) or (CharUpperW(s1^) = CharUpperW(s2^))) do
      begin
        Inc(Result);
        Inc(s1);
        Inc(s2);
      end;
    end
    else
    begin
      while (s1^ <> #0) and (s2^ <> #0) and (s1^ = s2^) do
      begin
        Inc(Result);
        Inc(s1);
        Inc(s2);
      end;
    end;
  end;
end;

function DetectTextEncoding(const p: Pointer; l: Integer; var b: Boolean)
  : TTextEncoding;
var
  pAnsi: PByte;
  pWide: PWideChar;
  I, AUtf8CharSize: Integer;
const
  NoUtf8Char: array [0 .. 3] of Byte = ($C1, $AA, $CD, $A8); // ANSI编码的联通
  function IsUtf8Order(var ACharSize: Integer): Boolean;
  var
    I: Integer;
    ps: PByte;
  const
    Utf8Masks: array [0 .. 4] of Byte = ($C0, $E0, $F0, $F8, $FC);
  begin
    ps := pAnsi;
    ACharSize := CharSizeU(PQCharA(ps));
    Result := false;
    if ACharSize > 1 then
    begin
      I := ACharSize - 2;
      if ((Utf8Masks[I] and ps^) = Utf8Masks[I]) then
      begin
        Inc(ps);
        Result := True;
        for I := 1 to ACharSize - 1 do
        begin
          if (ps^ and $80) <> $80 then
          begin
            Result := false;
            Break;
          end;
          Inc(ps);
        end;
      end;
    end;
  end;

begin
  Result := teAnsi;
  b := false;
  if l >= 2 then
  begin
    pAnsi := PByte(p);
    pWide := PWideChar(p);
    b := True;
    if pWide^ = #$FEFF then
      Result := teUnicode16LE
    else if pWide^ = #$FFFE then
      Result := teUnicode16BE
    else if l >= 3 then
    begin
      if (pAnsi^ = $EF) and (PByte(IntPtr(pAnsi) + 1)^ = $BB) and
        (PByte(IntPtr(pAnsi) + 2)^ = $BF) then // UTF-8编码
        Result := teUTF8
      else // 检测字符中是否有符合UFT-8编码规则的字符，11...
      begin
        b := false;
        Result := teUnknown; // 假设为UTF8编码，然后检测是否有不符合UTF-8编码的序列
        I := 0;
        Dec(l, 2);
        while I <= l do
        begin
          if (pAnsi^ and $80) <> 0 then // 高位为1
          begin
            if (l - I >= 4) then
            begin
              if CompareMem(pAnsi, @NoUtf8Char[0], 4) then
              // 联通？是则忽略掉，不做UTF-8编码的判断依据
              begin
                Inc(pAnsi, 4);
                Inc(I, 4);
                Result := teAnsi;
                continue;
              end;
            end;
            if IsUtf8Order(AUtf8CharSize) then
            begin
              Inc(pAnsi, AUtf8CharSize);
              Result := teUTF8;
              Break;
            end
            else
            begin
              Result := teAnsi;
              Break;
            end;
          end
          else
          begin
            if pAnsi^ = 0 then // 00 xx (xx<128) 高位在前，是BE编码
            begin
              if PByte(IntPtr(pAnsi) + 1)^ < 128 then
              begin
                Result := teUnicode16BE;
                Break;
              end;
            end
            else if PByte(IntPtr(pAnsi) + 1)^ = 0 then // xx 00 低位在前，是LE编码
            begin
              Result := teUnicode16LE;
              Break;
            end;
            Inc(pAnsi);
            Inc(I);
          end;
          if Result = teUnknown then
            Result := teAnsi;
        end;
      end;
    end;
  end;
end;

function LoadTextA(const AFileName: String; AEncoding: TTextEncoding): QStringA;
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadTextA(AStream, AEncoding);
  finally
    AStream.Free;
  end;
end;

procedure ExchangeByteOrder(p: PQCharA; l: Integer);
var
  pe: PQCharA;
  c: QCharA;
begin
  pe := p;
  Inc(pe, l);
  while IntPtr(p) < IntPtr(pe) do
  begin
    c := p^;
    p^ := PQCharA(IntPtr(p) + 1)^;
    PQCharA(IntPtr(p) + 1)^ := c;
    Inc(p, 2);
  end;
end;

function ExchangeByteOrder(V: Smallint): Smallint;
var
  pv: array [0 .. 1] of Byte absolute V;
  pd: array [0 .. 1] of Byte absolute Result;
begin
  pd[0] := pv[1];
  pd[1] := pv[0];
end;

function ExchangeByteOrder(V: Word): Word;
var
  pv: array [0 .. 1] of Byte absolute V;
  pd: array [0 .. 1] of Byte absolute Result;
begin
  pd[0] := pv[1];
  pd[1] := pv[0];
end;

function ExchangeByteOrder(V: Integer): Integer;
var
  pv: array [0 .. 3] of Byte absolute V;
  pd: array [0 .. 3] of Byte absolute Result;
begin
  pd[0] := pv[3];
  pd[1] := pv[2];
  pd[2] := pv[1];
  pd[3] := pv[0];
end;

function ExchangeByteOrder(V: Cardinal): Cardinal;
var
  pv: array [0 .. 3] of Byte absolute V;
  pd: array [0 .. 3] of Byte absolute Result;
begin
  pd[0] := pv[3];
  pd[1] := pv[2];
  pd[2] := pv[1];
  pd[3] := pv[0];
end;

function ExchangeByteOrder(V: Int64): Int64;
var
  pv: array [0 .. 7] of Byte absolute V;
  pd: array [0 .. 7] of Byte absolute Result;
begin
  pd[0] := pv[7];
  pd[1] := pv[6];
  pd[2] := pv[5];
  pd[3] := pv[4];
  pd[4] := pv[3];
  pd[5] := pv[2];
  pd[6] := pv[1];
  pd[7] := pv[0];
end;

function ExchangeByteOrder(V: Single): Single;
var
  pv: array [0 .. 3] of Byte absolute V;
  pd: array [0 .. 3] of Byte absolute Result;
begin
  pd[0] := pv[3];
  pd[1] := pv[2];
  pd[2] := pv[1];
  pd[3] := pv[0];
end;

function ExchangeByteOrder(V: Double): Double;
var
  pv: array [0 .. 7] of Byte absolute V;
  pd: array [0 .. 7] of Byte absolute Result;
begin
  pd[0] := pv[7];
  pd[1] := pv[6];
  pd[2] := pv[5];
  pd[3] := pv[4];
  pd[4] := pv[3];
  pd[5] := pv[2];
  pd[6] := pv[1];
  pd[7] := pv[0];
end;

function LoadTextA(AStream: TStream; AEncoding: TTextEncoding): QStringA;
var
  ASize: Integer;
  ABuffer: TBytes;
  ABomExists: Boolean;
begin
  ASize := AStream.Size - AStream.Position;
  if ASize > 0 then
  begin
    SetLength(ABuffer, ASize);
    AStream.ReadBuffer((@ABuffer[0])^, ASize);
    if AEncoding in [teUnknown, teAuto] then
      AEncoding := DetectTextEncoding(@ABuffer[0], ASize, ABomExists)
    else if ASize >= 2 then
    begin
      case AEncoding of
        teUnicode16LE:
          ABomExists := (ABuffer[0] = $FF) and (ABuffer[1] = $FE);
        teUnicode16BE:
          ABomExists := (ABuffer[1] = $FE) and (ABuffer[1] = $FF);
        teUTF8:
          begin
            if ASize >= 3 then
              ABomExists := (ABuffer[0] = $EF) and (ABuffer[1] = $BB) and
                (ABuffer[2] = $BF)
            else
              ABomExists := false;
          end;
      end;
    end
    else
      ABomExists := false;
    if AEncoding = teAnsi then
      Result := ABuffer
    else if AEncoding = teUTF8 then
    begin
      if ABomExists then
      begin
        if ASize > 3 then
          Result := AnsiEncode(Utf8Decode(@ABuffer[3], ASize - 3))
        else
          Result.Length := 0;
      end
      else
        Result := AnsiEncode(Utf8Decode(@ABuffer[0], ASize));
    end
    else
    begin
      if AEncoding = teUnicode16BE then
        ExchangeByteOrder(@ABuffer[0], ASize);
      if ABomExists then
        Result := AnsiEncode(PQCharW(@ABuffer[2]), (ASize - 2) shr 1)
      else
        Result := AnsiEncode(PQCharW(@ABuffer[0]), ASize shr 1);
    end;
  end
  else
    Result.Length := 0;
end;

function LoadTextU(const AFileName: String; AEncoding: TTextEncoding): QStringA;
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadTextU(AStream, AEncoding);
  finally
    AStream.Free;
  end;
end;

function LoadTextU(AStream: TStream; AEncoding: TTextEncoding): QStringA;
var
  ASize: Integer;
  ABuffer: TBytes;
  ABomExists: Boolean;
begin
  ASize := AStream.Size - AStream.Position;
  if ASize > 0 then
  begin
    SetLength(ABuffer, ASize);
    AStream.ReadBuffer((@ABuffer[0])^, ASize);
    if AEncoding in [teUnknown, teAuto] then
      AEncoding := DetectTextEncoding(@ABuffer[0], ASize, ABomExists)
    else if ASize >= 2 then
    begin
      case AEncoding of
        teUnicode16LE:
          ABomExists := (ABuffer[0] = $FF) and (ABuffer[1] = $FE);
        teUnicode16BE:
          ABomExists := (ABuffer[1] = $FE) and (ABuffer[1] = $FF);
        teUTF8:
          begin
            if ASize > 3 then
              ABomExists := (ABuffer[0] = $EF) and (ABuffer[1] = $BB) and
                (ABuffer[2] = $BF)
            else
              ABomExists := false;
          end;
      end;
    end
    else
      ABomExists := false;
    if AEncoding = teAnsi then
      Result := qstring.Utf8Encode(AnsiDecode(@ABuffer[0], ASize))
    else if AEncoding = teUTF8 then
    begin
      if ABomExists then
      begin
        Dec(ASize, 3);
        Result.From(@ABuffer[0], 3, ASize);
      end
      else
        Result := ABuffer;
      if ASize > 0 then
        Result.FValue[0] := 1; // UTF-8
    end
    else
    begin
      if AEncoding = teUnicode16BE then
        ExchangeByteOrder(@ABuffer[0], ASize);
      if ABomExists then
        Result := qstring.Utf8Encode(PQCharW(@ABuffer[2]), (ASize - 2) shr 1)
      else
        Result := qstring.Utf8Encode(PQCharW(@ABuffer[0]), ASize shr 1);
    end;
  end
  else
  begin
    Result.Length := 0;
    Result.FValue[0] := 1;
  end;
end;

function LoadTextW(const AFileName: String; AEncoding: TTextEncoding): QStringW;
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadTextW(AStream, AEncoding);
  finally
    AStream.Free;
  end;
end;

function DecodeText(p: Pointer; ASize: Integer; AEncoding: TTextEncoding)
  : QStringW;
var
  ABomExists: Boolean;
  pb: PByte;
  pe: PQCharA;
  function ByteOf(AOffset: Integer): Byte;
  begin
    Result := PByte(IntPtr(pb) + AOffset)^;
  end;

begin
  pb := p;
  if ASize >= 2 then
  begin
    // 不管是否指定编码，强制检测BOM头，避免由于编码指定不符造成问题
    if (ByteOf(0) = $FF) and (ByteOf(1) = $FE) then
    begin
      AEncoding := teUnicode16LE;
      Inc(pb, 2);
      Dec(ASize, 2);
    end
    else if (ByteOf(0) = $FE) and (ByteOf(1) = $FF) then
    begin
      AEncoding := teUnicode16BE;
      Inc(pb, 2);
      Dec(ASize, 2);
    end
    else if (ASize > 2) and (ByteOf(0) = $EF) and (ByteOf(1) = $BB) and
      (ByteOf(2) = $BF) then
    begin
      AEncoding := teUTF8;
      Inc(pb, 3);
      Dec(ASize, 3);
    end
    else if AEncoding in [teUnknown, teAuto] then // No BOM
      AEncoding := DetectTextEncoding(pb, ASize, ABomExists);
    if AEncoding = teAnsi then
      Result := AnsiDecode(PQCharA(pb), ASize)
    else if AEncoding = teUTF8 then
    begin
      if not Utf8Decode(PQCharA(pb), ASize, Result, pe) then
        Result := AnsiDecode(PQCharA(pb), ASize);
    end
    else
    begin
      if AEncoding = teUnicode16BE then
        ExchangeByteOrder(PQCharA(pb), ASize);
      SetLength(Result, ASize shr 1);
      Move(pb^, PQCharW(Result)^, ASize);
    end;
  end
  else if ASize > 0 then
    Result := WideChar(pb^)
  else
    SetLength(Result, 0);
end;

function LoadTextW(AStream: TStream; AEncoding: TTextEncoding)
  : QStringW; overload;
var
  ASize: Integer;
  ABuffer: TBytes;
begin
  ASize := AStream.Size - AStream.Position;
  if ASize > 0 then
  begin
    SetLength(ABuffer, ASize);
    AStream.ReadBuffer((@ABuffer[0])^, ASize);
    Result := DecodeText(@ABuffer[0], ASize, AEncoding);
  end
  else
    SetLength(Result, 0);
end;

procedure SaveTextA(const AFileName: String; const S: QStringA);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveTextA(AStream, S);
  finally
    AStream.Free;
  end;
end;

procedure SaveTextA(AStream: TStream; const S: QStringA);
  procedure Utf8Save;
  var
    T: QStringA;
  begin
    T := AnsiEncode(Utf8Decode(S));
    AStream.WriteBuffer(PQCharA(T)^, T.Length);
  end;

begin
  if not S.IsUtf8 then
    AStream.WriteBuffer(PQCharA(S)^, S.Length)
  else
    Utf8Save;
end;

procedure SaveTextU(const AFileName: String; const S: QStringA;
  AWriteBom: Boolean);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveTextU(AStream, S, AWriteBom);
  finally
    AStream.Free;
  end;
end;

procedure SaveTextU(const AFileName: String; const S: QStringW;
  AWriteBom: Boolean); overload;
begin
  SaveTextU(AFileName, qstring.Utf8Encode(S), AWriteBom);
end;

procedure SaveTextU(AStream: TStream; const S: QStringA; AWriteBom: Boolean);
  procedure WriteBom;
  var
    ABom: TBytes;
  begin
    SetLength(ABom, 3);
    ABom[0] := $EF;
    ABom[1] := $BB;
    ABom[2] := $BF;
    AStream.WriteBuffer(ABom[0], 3);
  end;
  procedure SaveAnsi;
  var
    T: QStringA;
  begin
    T := qstring.Utf8Encode(AnsiDecode(S));
    AStream.WriteBuffer(PQCharA(T)^, T.Length);
  end;

begin
  if AWriteBom then
    WriteBom;
  if S.IsUtf8 then
    AStream.WriteBuffer(PQCharA(S)^, S.Length)
  else
    SaveAnsi;
end;

procedure SaveTextU(AStream: TStream; const S: QStringW;
  AWriteBom: Boolean); overload;
begin
  SaveTextU(AStream, qstring.Utf8Encode(S), AWriteBom);
end;

procedure SaveTextW(const AFileName: String; const S: QStringW;
  AWriteBom: Boolean);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveTextW(AStream, S, AWriteBom);
  finally
    AStream.Free;
  end;
end;

procedure SaveTextW(AStream: TStream; const S: QStringW; AWriteBom: Boolean);
  procedure WriteBom;
  var
    bom: Word;
  begin
    bom := $FEFF;
    AStream.WriteBuffer(bom, 2);
  end;

begin
  if AWriteBom then
    WriteBom;
  AStream.WriteBuffer(PQCharW(S)^, System.Length(S) shl 1);
end;

procedure SaveTextWBE(AStream: TStream; const S: QStringW; AWriteBom: Boolean);
var
  pw, pe: PWord;
  w: Word;
  ABuilder: TQStringCatHelperW;
begin
  pw := PWord(PQCharW(S));
  pe := pw;
  Inc(pe, Length(S));
  ABuilder := TQStringCatHelperW.Create(IntPtr(pe) - IntPtr(pw));
  try
    while IntPtr(pw) < IntPtr(pe) do
    begin
      w := (pw^ shr 8) or (pw^ shl 8);
      ABuilder.Cat(@w, 1);
      Inc(pw);
    end;
    if AWriteBom then
      AStream.WriteBuffer(#$FE#$FF, 2);
    AStream.WriteBuffer(ABuilder.FStart^, Length(S) shl 1);
  finally
    FreeObject(ABuilder);
  end;
end;

function StrStrA(s1, s2: PQCharA): PQCharA;
  function DoSearch: PQCharA;
  var
    ps1, ps2: PQCharA;
  begin
    ps1 := s1;
    ps2 := s2;
    Inc(ps1);
    Inc(ps2);
    while ps2^ <> 0 do
    begin
      if ps1^ = ps2^ then
      begin
        Inc(ps1);
        Inc(ps2);
      end
      else
        Break;
    end;
    if ps2^ = 0 then
      Result := s1
    else
      Result := nil;
  end;

begin
{$IFDEF MSWINDOWS}
  if Assigned(VCStrStr) then
  begin
    Result := VCStrStr(s1, s2);
    Exit;
  end;
{$ENDIF}
  Result := nil;
  if (s1 <> nil) and (s2 <> nil) then
  begin
    while s1^ <> 0 do
    begin
      if s1^ = s2^ then
      begin
        Result := DoSearch;
        if Result <> nil then
          Exit;
      end;
      Inc(s1);
    end;
  end;
end;

function StrIStrA(s1, s2: PQCharA): PQCharA;
var
  ws2: QStringA;
  function DoSearch: PQCharA;
  var
    ps1, ps2: PQCharA;
  begin
    ps1 := s1;
    ps2 := PQCharA(ws2);
    Inc(ps1);
    Inc(ps2);
    while ps2^ <> 0 do
    begin
      if CharUpperA(ps1^) = ps2^ then
      begin
        Inc(ps1);
        Inc(ps2);
      end
      else
        Break;
    end;
    if ps2^ = 0 then
      Result := s1
    else
      Result := nil;
  end;

begin
  Result := nil;
  if (s1 <> nil) and (s2 <> nil) then
  begin
    ws2 := QStringA.UpperCase(s2);
    while s1^ <> 0 do
    begin
      if s1^ = s2^ then
      begin
        Result := DoSearch;
        if Result <> nil then
          Exit;
      end;
      Inc(s1);
    end;
  end;
end;

function StrStrU(s1, s2: PQCharA): PQCharA;
begin
  Result := StrStrA(s1, s2);
end;

function StrIStrU(s1, s2: PQCharA): PQCharA;
begin
  Result := StrIStrA(s1, s2);
end;

function StrStrW(s1, s2: PQCharW): PQCharW;
var
  I: Integer;
begin
{$IFDEF MSWINDOWS}
  if Assigned(VCStrStrW) then
  begin
    Result := VCStrStrW(s1, s2);
    Exit;
  end;
{$ENDIF}
  if (s2 = nil) or (s2^ = #0) then
    Result := s1
  else
  begin
    Result := nil;
    while s1^ <> #0 do
    begin
      if s1^ = s2^ then
      begin
        I := 1;
        while s2[I] <> #0 do
        begin
          if s1[I] = s2[I] then
            Inc(I)
          else
            Break;
        end;
        if s2[I] = #0 then
        begin
          Result := s1;
          Break;
        end;
      end;
      Inc(s1);
    end;
  end;
end;

function StrIStrW(s1, s2: PQCharW): PQCharW;
var
  I: Integer;
  ws2: QStringW;
begin
  Result := nil;
  if (s1 = nil) or (s2 = nil) then
    Exit;
  ws2 := UpperCase(s2);
  s2 := PWideChar(ws2);
  while s1^ <> #0 do
  begin
    if CharUpperW(s1^) = s2^ then
    begin
      I := 1;
      while s2[I] <> #0 do
      begin
        if CharUpperW(s1[I]) = s2[I] then
          Inc(I)
        else
          Break;
      end;
      if s2[I] = #0 then
      begin
        Result := s1;
        Break;
      end;
    end;
    Inc(s1);
  end;
end;

function PosA(sub, S: PQCharA; AIgnoreCase: Boolean;
  AStartPos: Integer): Integer;
begin
  if AStartPos > 0 then
    Inc(S, AStartPos - 1);
  if AIgnoreCase then
    sub := StrIStrA(S, sub)
  else
    sub := StrStrA(S, sub);
  if Assigned(sub) then
    Result := (IntPtr(sub) - IntPtr(S)) + AStartPos
  else
    Result := 0;
end;

function PosA(sub, S: QStringA; AIgnoreCase: Boolean;
  AStartPos: Integer): Integer;
begin
  Result := PosA(PQCharA(sub), PQCharA(S), AIgnoreCase, AStartPos);
end;

function PosW(sub, S: PQCharW; AIgnoreCase: Boolean;
  AStartPos: Integer): Integer;
begin
  if AStartPos > 0 then
    Inc(S, AStartPos - 1);
  if AIgnoreCase then
    sub := StrIStrW(S, sub)
  else
    sub := StrStrW(S, sub);
  if Assigned(sub) then
    Result := ((IntPtr(sub) - IntPtr(S)) shr 1) + AStartPos
  else
    Result := 0;
end;

function PosW(sub, S: QStringW; AIgnoreCase: Boolean; AStartPos: Integer)
  : Integer; overload;
begin
  Result := PosW(PQCharW(sub), PQCharW(S), AIgnoreCase, AStartPos);
end;

function StrDupX(const S: PQCharW; ACount: Integer): QStringW;
begin
  SetLength(Result, ACount);
  Move(S^, PQCharW(Result)^, ACount shl 1);
end;

function StrDupW(const S: PQCharW; AOffset: Integer; const ACount: Integer)
  : QStringW;
var
  c, ACharSize: Integer;
  p, pds, pd: PQCharW;
begin
  c := 0;
  p := S + AOffset;
  SetLength(Result, 16384);
  pd := PQCharW(Result);
  pds := pd;
  while (p^ <> #0) and (c < ACount) do
  begin
    ACharSize := CharSizeW(p);
    AOffset := pd - pds;
    if AOffset + ACharSize = Length(Result) then
    begin
      SetLength(Result, Length(Result) shl 1);
      pds := PQCharW(Result);
      pd := pds + AOffset;
    end;
    Inc(c);
    pd^ := p^;
    if ACharSize = 2 then
      pd[1] := p[1];
    Inc(pd, ACharSize);
    Inc(p, ACharSize);
  end;
  SetLength(Result, pd - pds);
end;

function StrCmpA(const s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
var
  p1, p2: PQCharA;
  c1, c2: QCharA;
begin
  p1 := s1;
  p2 := s2;
  if AIgnoreCase then
  begin
    while (p1^ <> 0) and (p2^ <> 0) do
    begin
      if p1^ <> p2^ then
      begin
        if (p1^ >= Ord('a')) and (p1^ <= Ord('z')) then
          c1 := p1^ xor $20
        else
          c1 := p1^;
        if (p2^ >= Ord('a')) and (p2^ <= Ord('z')) then
          c2 := p2^ xor $20
        else
          c2 := p2^;
        Result := Ord(c1) - Ord(c2);
        if Result <> 0 then
          Exit;
      end;
      Inc(p1);
      Inc(p2);
    end;
    Result := Ord(p1^) - Ord(p2^);
  end
  else
  begin
    while (p1^ <> 0) and (p2^ <> 0) do
    begin
      Result := p1^ - p2^;
      if Result <> 0 then
        Exit;
      Inc(p1);
      Inc(p2);
    end;
    Result := Ord(p1^) - Ord(p2^);
  end;
end;

function StrCmpW(const s1, s2: PQCharW; AIgnoreCase: Boolean): Integer;
var
  p1, p2: PQCharW;
  c1, c2: QCharW;
begin
  p1 := s1;
  p2 := s2;
  if AIgnoreCase then
  begin
    while (p1^ <> #0) and (p2^ <> #0) do
    begin
      if p1^ <> p2^ then
      begin
        if (p1^ >= 'a') and (p1^ <= 'z') then
          c1 := WideChar(Word(p1^) xor $20)
        else
          c1 := p1^;
        if (p2^ >= 'a') and (p2^ <= 'z') then
          c2 := WideChar(Word(p2^) xor $20)
        else
          c2 := p2^;
        Result := Ord(c1) - Ord(c2);
        if Result <> 0 then
          Exit;
      end;
      Inc(p1);
      Inc(p2);
    end;
    Result := Ord(p1^) - Ord(p2^);
  end
  else
  begin
    while (p1^ <> #0) and (p2^ <> #0) do
    begin
      if p1^ <> p2^ then
      begin
        Result := Ord(p1^) - Ord(p2^);
        if Result <> 0 then
          Exit;
      end;
      Inc(p1);
      Inc(p2);
    end;
    Result := Ord(p1^) - Ord(p2^);
  end;
end;

function StrNCmpW(const s1, s2: PQCharW; AIgnoreCase: Boolean;
  ALength: Integer): Integer;
var
  p1, p2: PQCharW;
  c1, c2: QCharW;
begin
  p1 := s1;
  p2 := s2;
  if AIgnoreCase then
  begin
    while ALength > 0 do
    begin
      if p1^ <> p2^ then
      begin
        if (p1^ >= 'a') and (p1^ <= 'z') then
          c1 := WideChar(Word(p1^) xor $20)
        else
          c1 := p1^;
        if (p2^ >= 'a') and (p2^ <= 'z') then
          c2 := WideChar(Word(p2^) xor $20)
        else
          c2 := p2^;
        Result := Ord(c1) - Ord(c2);
        if Result <> 0 then
          Exit;
      end;
      Inc(p1);
      Inc(p2);
      Dec(ALength);
    end;
  end
  else
  begin
    while ALength > 0 do
    begin
      if p1^ <> p2^ then
      begin
        Result := Ord(p1^) - Ord(p2^);
        if Result <> 0 then
          Exit;
      end;
      Inc(p1);
      Inc(p2);
      Dec(ALength);
    end;
  end;
  if ALength = 0 then
    Result := 0
  else
    Result := Ord(p1^) - Ord(p2^);
end;

/// <summary>使用自然语言规则比较字符串</summary>
/// <param name="s1">第一个要比较的字符串</param>
/// <param name="s2">第二个要比较的字符串</param>
/// <param name="AIgnoreCase">比较时是否忽略大小写</param>
/// <param name="AIgnoreSpace">比较时是否忽略空白字符</param>
/// <remarks>本比较考虑中文全角的情况，认为中文全角符号和对应的半角符号是相等的值</remarks>
function NaturalCompareW(s1, s2: PQCharW;
  AIgnoreCase, AIgnoreSpace: Boolean): Integer;
var
  N1, N2: Int64;
  L1, L2: Integer;
  c1, c2: QCharW;
  function FetchNumeric(p: PQCharW; var AResult: Int64;
    var ALen: Integer): Boolean;
  var
    ps: PQCharW;
  const
    Full0: WideChar = #65296; // 全角0
    Full9: WideChar = #65305; // 全角9
  begin
    AResult := 0;
    ps := p;
    while p^ <> #0 do
    begin
      while IsSpaceW(p) do
        Inc(p);
      if (p^ >= '0') and (p^ <= '9') then // 半角数字
        AResult := AResult * 10 + Ord(p^) - Ord('0')
      else if (p^ >= Full0) and (p^ <= Full9) then // 全角数字
        AResult := AResult * 10 + Ord(p^) - Ord(Full0)
      else
        Break;
      Inc(p);
    end;
    Result := ps <> p;
    ALen := (IntPtr(p) - IntPtr(ps)) shr 1;
  end;
  function FullToHalfChar(c: Word): QCharW;
  begin
    if (c = $3000) then // 全角空格'　'
      Result := QCharW($20)
    else if (c >= $FF01) and (c <= $FF5E) then
      Result := QCharW($21 + (c - $FF01))
    else
      Result := QCharW(c);
  end;
  function CompareChar: Integer;
  begin
    if AIgnoreCase then
    begin
      c1 := CharUpperW(FullToHalfChar(Ord(s1^)));
      c2 := CharUpperW(FullToHalfChar(Ord(s2^)));
    end
    else
    begin
      c1 := FullToHalfChar(Ord(s1^));
      c2 := FullToHalfChar(Ord(s2^));
    end;
    Result := Ord(c1) - Ord(c2);
  end;

begin
  if Assigned(s1) then
  begin
    if not Assigned(s2) then
    begin
      Result := 1;
      Exit;
    end;
    while (s1^ <> #0) and (s2^ <> #0) do
    begin
      if s1^ <> s2^ then
      begin
        while IsSpaceW(s1) do
          Inc(s1);
        while IsSpaceW(s1) do
          Inc(s2);
        // 检查是否是数字
        L1 := 0;
        L2 := 0;
        if FetchNumeric(s1, N1, L1) and FetchNumeric(s2, N2, L2) then
        begin
          Result := N1 - N2;
          if Result <> 0 then
            Exit
          else
          begin
            Inc(s1, L1);
            Inc(s2, L2);
          end;
        end
        else
        begin
          Result := CompareChar;
          if Result = 0 then
          begin
            Inc(s1);
            Inc(s2);
          end
          else
            Exit;
        end;
      end
      else // 如果想等，即使是数字，肯定也是相等的
      begin
        Inc(s1);
        Inc(s2);
      end;
    end;
    Result := CompareChar;
  end
  else if Assigned(s2) then
    Result := -1
  else
    Result := 0;
end;

function IsHexChar(c: QCharW): Boolean; inline;
begin
  Result := ((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'f')) or
    ((c >= 'A') and (c <= 'F'));
end;

function IsOctChar(c: WideChar): Boolean; inline;
begin
  Result := (c >= '0') and (c <= '7');
end;

function HexValue(c: QCharW): Integer;
begin
  if (c >= '0') and (c <= '9') then
    Result := Ord(c) - Ord('0')
  else if (c >= 'a') and (c <= 'f') then
    Result := 10 + Ord(c) - Ord('a')
  else if (c >= 'A') and (c <= 'F') then
    Result := 10 + Ord(c) - Ord('A')
  else
    Result := -1;
end;

function HexChar(V: Byte): QCharW;
begin
  Result := HexChars[V];
end;

function TryStrToGuid(const S: QStringW; var AGuid: TGuid): Boolean;
var
  p, ps: PQCharW;
  l: Int64;
begin
  l := Length(S);
  p := PWideChar(S);
  if (l = 38) or (l = 36) then
  begin
    // {0BCBAAFF-15E6-451D-A8E8-0D98AC48C364}
    ps := p;
    if p^ = '{' then
      Inc(p);
    if (ParseHex(p, l) <> 8) or (p^ <> '-') then
    begin
      Result := false;
      Exit;
    end;
    AGuid.D1 := l;
    Inc(p);
    if (ParseHex(p, l) <> 4) or (p^ <> '-') then
    begin
      Result := false;
      Exit;
    end;
    AGuid.D2 := l;
    Inc(p);
    if (ParseHex(p, l) <> 4) or (p^ <> '-') then
    begin
      Result := false;
      Exit;
    end;
    AGuid.D3 := l;
    Inc(p);
    // 0102-030405060708
    // 剩下的16个字符
    l := 0;
    while IsHexChar(p[0]) do
    begin
      if IsHexChar(p[1]) then
      begin
        AGuid.D4[l] := (HexValue(p[0]) shl 4) + HexValue(p[1]);
        Inc(l);
        Inc(p, 2);
      end
      else
      begin
        Result := false;
        Exit;
      end;
    end;
    if (l <> 2) or (p^ <> '-') then
    begin
      Result := false;
      Exit;
    end;
    Inc(p);
    while IsHexChar(p[0]) do
    begin
      if IsHexChar(p[1]) then
      begin
        AGuid.D4[l] := (HexValue(p[0]) shl 4) + HexValue(p[1]);
        Inc(l);
        Inc(p, 2);
      end
      else
      begin
        Result := false;
        Exit;
      end;
    end;
    if (l = 8) then
    begin
      if ps^ = '{' then
        Result := (p[0] = '}') and (p[1] = #0)
      else
        Result := (p[0] = #0);
    end
    else
      Result := false;
  end
  else
    Result := false;
end;

function TryStrToIPV4(const S: QStringW; var AIPV4:
{$IFDEF MSWINDOWS}Integer{$ELSE}Cardinal{$ENDIF}): Boolean;
var
  p: PQCharW;
  dc: Integer;
  pd: PByte;
begin
  dc := 0;
  AIPV4 := 0;
  p := PQCharW(S);
  pd := PByte(@AIPV4);
  while p^ <> #0 do
  begin
    if (p^ >= '0') and (p^ <= '9') then
      pd^ := pd^ * 10 + Ord(p^) - Ord('0')
    else if p^ = '.' then
    begin
      Inc(dc);
      if dc > 3 then
        Break;
      Inc(pd);
    end
    else
      Break;
    Inc(p);
  end;
  Result := (dc = 3) and (p^ = #0);
end;

procedure InlineMove(var d, S: Pointer; l: Integer); inline;
begin
  if l >= 16 then
  begin
    Move(S^, d^, l);
    Inc(PByte(d), l);
    Inc(PByte(S), l);
  end
  else
  begin
    if l >= 8 then
    begin
      PInt64(d)^ := PInt64(S)^;
      Inc(PInt64(d));
      Inc(PInt64(S));
      Dec(l, 8);
    end;
    if l >= 4 then
    begin
      PInteger(d)^ := PInteger(S)^;
      Inc(PInteger(d));
      Inc(PInteger(S));
      Dec(l, 4);
    end;
    if l >= 2 then
    begin
      PSmallint(d)^ := PSmallint(S)^;
      Inc(PSmallint(d));
      Inc(PSmallint(S));
      Dec(l, 2);
    end;
    if l = 1 then
    begin
      PByte(d)^ := PByte(S)^;
      Inc(PByte(d));
      Inc(PByte(S));
    end;
  end;
end;

function StringReplaceWX(const S, Old, New: QStringW; AFlags: TReplaceFlags)
  : QStringW;
var
  ps, pse, pr, pd, po, pn, pns: PQCharW;
  LO, LN, LS, I, ACount: Integer;
  SS, SOld: QStringW;
  AFounds: array of Integer;
  AReplaceOnce: Boolean;
begin
  LO := Length(Old);
  LS := Length(S);
  if (LO = 0) or (LS = 0) or (Old = New) then
  begin
    Result := S;
    Exit;
  end;
  LN := Length(New);
  if rfIgnoreCase in AFlags then
  begin
    SOld := UpperCase(Old);
    if SOld = Old then // 大小写一致，不需要额外进行大写转换
      SS := S
    else
    begin
      SS := UpperCase(S);
      LS := Length(SS);
    end;
  end
  else
  begin
    SOld := Old;
    SS := S;
  end;
  ps := Pointer(SS);
  pn := ps;
  po := Pointer(SOld);
  ACount := 0;
  AReplaceOnce := not(rfReplaceAll in AFlags);
  repeat
    pr := StrStrW(pn, po);
    if Assigned(pr) then
    begin
      if ACount = 0 then
        SetLength(AFounds, 32)
      else
        SetLength(AFounds, ACount shl 1);
      AFounds[ACount] := IntPtr(pr) - IntPtr(pn);
      Inc(ACount);
      pn := pr;
      Inc(pn, LO);
    end
    else
      Break;
  until AReplaceOnce or (pn^ = #0);
  if ACount = 0 then // 没有找到需要替换的内容，直接返回原始字符串
    Result := S
  else
  begin
    // 计算需要分配的目标内存
    SetLength(Result, LS + (LN - LO) * ACount);
    pd := Pointer(Result);
    pn := Pointer(New);
    pns := pn;
    ps := Pointer(S);
    pse := ps;
    Inc(pse, LS);
    LN := LN shl 1;
    for I := 0 to ACount - 1 do
    begin
      InlineMove(Pointer(pd), Pointer(ps), AFounds[I]);
      InlineMove(Pointer(pd), Pointer(pn), LN);
      Inc(ps, LO);
      pn := pns;
    end;
    InlineMove(Pointer(pd), Pointer(ps), IntPtr(pse) - IntPtr(ps));
  end;
end;

function StringReplaceW(const S, Old, New: QStringW; AFlags: TReplaceFlags)
  : QStringW;
{$IF RTLVersion>30}// Berlin 开始直接使用系统自带的替换函数
begin
  Result := StringReplace(S, Old, New, AFlags);
end;
{$ELSE}

var
  ps, pse, pds, pr, pd, po, pn: PQCharW;
  l, LO, LN, LS, LR: Integer;
  AReplaceOnce: Boolean;
begin
  LO := Length(Old);
  LN := Length(New);
  if LO = LN then
  begin
    if Old = New then
    begin
      Result := S;
      Exit;
    end;
  end;
  LS := Length(S);
  if (LO > 0) and (LS >= LO) then
  begin
    AReplaceOnce := not(rfReplaceAll in AFlags);
    // LO=LN，则不变LR=LS，假设全替换，也不过是原长度
    // LO<LN，则LR=LS+(LS*LN)/LO，假设全替换的长度
    // LO>LN，则LR=LS，假设一次都不替换，也不过是原长度
    if LO >= LN then
      LR := LS
    else if AReplaceOnce then
      LR := LS + (LN - LO)
    else
      LR := LS + 1 + LS * LN div LO;
    SetLength(Result, LR);
    ps := PQCharW(S);
    pse := ps + LS;
    pd := PQCharW(Result);
    pds := pd;
    po := PQCharW(Old);
    pn := PQCharW(New);
    repeat
      if rfIgnoreCase in AFlags then
        pr := StrIStrW(ps, po)
      else
        pr := StrStrW(ps, po);
      if pr <> nil then
      begin
        l := IntPtr(pr) - IntPtr(ps);
        Move(ps^, pd^, l);
        Inc(pd, l shr 1);
        Inc(pr, LO);
        Move(pn^, pd^, LN shl 1);
        Inc(pd, LN);
        ps := pr;
      end;
    until (pr = nil) or AReplaceOnce;
    // 将剩余部分合并到目标
    l := IntPtr(pse) - IntPtr(ps);
    Move(ps^, pd^, l);
    Inc(pd, l shr 1);
    SetLength(Result, pd - pds);
  end
  else
    Result := S;
end;
{$IFEND}

function StringReplaceW(const S: QStringW; const AChar: QCharW;
  AFrom, ACount: Integer): QStringW;
var
  p, pd: PQCharW;
  l: Integer;
begin
  l := Length(S);
  SetLength(Result, l);
  if (l > 0) and (l > AFrom + 1) then
  begin
    p := PQCharW(S);
    pd := PQCharW(Result);
    while (p^ <> #0) and (AFrom > 0) do
    begin
      pd^ := p^;
      if (p^ > #$D800) and (p^ <= #$DFFF) then
      begin
        Inc(pd);
        Inc(p);
        pd^ := p^;
      end;
      Inc(p);
      Inc(pd);
      Dec(AFrom);
    end;
    while (p^ <> #0) and (ACount > 0) do
    begin
      pd^ := AChar;
      if (p^ > #$D800) and (p^ <= #$DFFF) then
        Inc(p);
      Inc(p);
      Inc(pd);
      Dec(ACount);
    end;
    while p^ <> #0 do
    begin
      pd^ := p^;
      Inc(p);
      Inc(pd);
    end;
  end;
end;

function StringReplaceWithW(const S, AStartTag, AEndTag, AReplaced: QStringW;
  AWithTag, AIgnoreCase: Boolean; AMaxTimes: Cardinal): QStringW;
var
  po, pe, pws, pwe, pd, pStart, pEnd, pReplaced: PQCharW;
  l, dl, LS, LE, LR: Integer;
  StrStrFunc: TStrStrFunction;
begin
  l := Length(S);
  LS := Length(AStartTag);
  LE := Length(AEndTag);
  if (l >= LS + LE) and (AMaxTimes > 0) then
  begin
    LR := Length(AReplaced);
    po := PQCharW(S);
    pe := po + l;
    pStart := PQCharW(AStartTag);
    pEnd := PQCharW(AEndTag);
    pReplaced := PQCharW(AReplaced);
    if LR > l then
      SetLength(Result, l * LR) // 最糟糕的情况，每个都被替换为目标,当然这不可能
    else
      SetLength(Result, l);
    pd := PQCharW(Result);
    if AIgnoreCase then
      StrStrFunc := StrIStrW
    else
      StrStrFunc := StrStrW;
    repeat
      pws := StrStrFunc(po, pStart);
      if pws = nil then
      begin
        dl := (pe - po);
        Move(po^, pd^, dl shl 1);
        SetLength(Result, pd - PQCharW(Result) + dl);
        Exit;
      end
      else
      begin
        pwe := StrStrFunc(pws + LS, pEnd);
        if pwe = nil then
        // 没找到结尾
        begin
          dl := pe - po;
          Move(po^, pd^, dl shl 1);
          SetLength(Result, pd - PQCharW(Result) + dl);
          Exit;
        end
        else
        begin
          dl := pws - po;
          if AWithTag then
          begin
            Move(po^, pd^, (LS + dl) shl 1);
            Inc(pd, LS + dl);
            Move(pReplaced^, pd^, LR shl 1);
            Inc(pd, LR);
            Move(pwe^, pd^, LE shl 1);
            Inc(pd, LE);
          end
          else
          begin
            Move(po^, pd^, dl shl 1);
            Inc(pd, dl);
            Move(pReplaced^, pd^, LR shl 1);
            Inc(pd, LR);
          end;
          po := pwe + LE;
          Dec(AMaxTimes);
        end;
      end;
    until (AMaxTimes = 0) and (IntPtr(po) < IntPtr(pe));
    if IntPtr(po) < IntPtr(pe) then
    begin
      dl := pe - po;
      Move(po^, pd^, dl shl 1);
      Inc(pd, dl);
      SetLength(Result, pd - PQCharW(Result));
    end;
  end
  else
    Result := S;
end;

function StringReplicateW(const S: QStringW; ACount: Integer): QStringW;
var
  l: Integer;
  p, ps, pd: PQCharW;
begin
  l := Length(S);
  if (l > 0) and (ACount > 0) then
  begin
    SetLength(Result, ACount * l);
    ps := PQCharW(S);
    pd := PQCharW(Result);
    for l := 0 to ACount - 1 do
    begin
      p := ps;
      while p^ <> #0 do
      begin
        pd^ := p^;
        Inc(pd);
        Inc(p);
      end;
    end;
  end
  else
    SetLength(Result, 0);
end;

function StringReplicateW(const S, AChar: QStringW; AExpectLength: Integer)
  : QStringW;
begin
  if Length(S) < AExpectLength then
    Result := S + StringReplicateW(AChar, AExpectLength - Length(S))
  else
    Result := S;
end;

function Translate(const S, AToReplace, AReplacement: QStringW): QStringW;
var
  I: Integer;
  pd, pp, pr, ps: PQCharW;

  function CharIndex(c: QCharW): Integer;
  var
    pt: PQCharW;
  begin
    pt := pp;
    Result := -1;
    while pt^ <> #0 do
    begin
      if pt^ <> c then
        Inc(pt)
      else
      begin
        Result := (IntPtr(pt) - IntPtr(pp));
        Break;
      end;
    end;
  end;

begin
  if Length(AToReplace) <> Length(AReplacement) then
    raise Exception.CreateFmt(SMismatchReplacement, [AToReplace, AReplacement]);
  SetLength(Result, Length(S));
  pd := PQCharW(Result);
  ps := PQCharW(S);
  pp := PQCharW(AToReplace);
  pr := PQCharW(AReplacement);
  while ps^ <> #0 do
  begin
    I := CharIndex(ps^);
    if I <> -1 then
      pd^ := PQCharW(IntPtr(pr) + I)^
    else
      pd^ := ps^;
    Inc(ps);
    Inc(pd);
  end;
end;

function FilterCharW(const S: QStringW; AcceptChars: QStringW)
  : QStringW; overload;
var
  ps, pd, pc, pds: PQCharW;
  l: Integer;
begin
  SetLength(Result, Length(S));
  if Length(S) > 0 then
  begin
    ps := PQCharW(S);
    pd := PQCharW(Result);
    pds := pd;
    pc := PQCharW(AcceptChars);
    while ps^ <> #0 do
    begin
      if CharInW(ps, pc, @l) then
      begin
        pd^ := ps^;
        Inc(ps);
        Inc(pd);
        if l > 1 then
        begin
          pd^ := ps^;
          Inc(ps);
          Inc(pd);
        end;
      end
      else
        Inc(ps);
    end;
    SetLength(Result, (IntPtr(pd) - IntPtr(pds)) shr 1);
  end;
end;

function FilterCharW(const S: QStringW; AOnValidate: TQFilterCharEvent;
  ATag: Pointer): QStringW; overload;
var
  ps, pd, pds: PQCharW;
  l, I: Integer;
  Accept: Boolean;
begin
  if (Length(S) > 0) and Assigned(AOnValidate) then
  begin
    SetLength(Result, Length(S));
    ps := PQCharW(S);
    pd := PQCharW(Result);
    pds := pd;
    I := 0;
    while ps^ <> #0 do
    begin
      Accept := True;
      if CharSizeW(ps) = 2 then
      begin
        l := Ord(ps^);
        Inc(ps);
        l := (l shl 16) or Ord(ps^);
        AOnValidate(l, I, Accept, ATag);
      end
      else
        AOnValidate(Ord(ps^), I, Accept, ATag);
      if Accept then
      begin
        pd^ := ps^;
        Inc(pd);
      end;
      Inc(ps);
      Inc(I);
    end;
    SetLength(Result, (IntPtr(pd) - IntPtr(pds)) shr 1);
  end
  else
    SetLength(Result, 0);
end;
{$IFDEF UNICODE}

function FilterCharW(const S: QStringW; AOnValidate: TQFilterCharEventA;
  ATag: Pointer): QStringW; overload;
var
  ps, pd, pds: PQCharW;
  l, I: Integer;
  Accept: Boolean;
begin
  if (Length(S) > 0) and Assigned(AOnValidate) then
  begin
    SetLength(Result, Length(S));
    ps := PQCharW(S);
    pd := PQCharW(Result);
    pds := pd;
    I := 0;
    while ps^ <> #0 do
    begin
      Accept := True;
      if CharSizeW(ps) = 2 then
      begin
        l := Ord(ps^);
        Inc(ps);
        l := (l shl 16) or Ord(ps^);
        AOnValidate(l, I, Accept, ATag);
      end
      else
        AOnValidate(Ord(ps^), I, Accept, ATag);
      Inc(I);
      if Accept then
      begin
        pd^ := ps^;
        Inc(pd);
      end;
      Inc(ps);
    end;
    SetLength(Result, (IntPtr(pd) - IntPtr(pds)) shr 1);
  end
  else
    SetLength(Result, 0);
end;
{$ENDIF}

function FilterNoNumberW(const S: QStringW; Accepts: TQNumberTypes): QStringW;
var
  p, pd, pds: PQCharW;
  d, e: Integer;
  AIsHex: Boolean;
  procedure NegPosCheck;
  begin
    if ((p^ = '+') and (nftPositive in Accepts)) or
      ((p^ = '-') and (nftNegative in Accepts)) then
    begin
      pd^ := p^;
      Inc(p);
      Inc(pd);
    end;
  end;

begin
  SetLength(Result, Length(S));
  p := PQCharW(S);
  pd := PQCharW(Result);
  pds := pd;
  AIsHex := false;
  NegPosCheck;
  if nftHexPrec in Accepts then // Check Hex prec
  begin
    if (p^ = '0') and (nftCHex in Accepts) then // C Style
    begin
      Inc(p);
      if (p^ = 'x') or (p^ = 'X') then
      begin
        pd^ := '0';
        Inc(pd);
        pd^ := p^;
        Inc(pd);
        Inc(p);
        AIsHex := True;
      end
      else
        Dec(p);
    end
    else if (p^ = '$') and (nftDelphiHex in Accepts) then
    begin
      pd^ := p^;
      Inc(p);
      Inc(pd);
      AIsHex := True;
    end
    else if (p^ = '&') and (nftBasicHex in Accepts) then
    begin
      Inc(p);
      if Ord(p^) in [Ord('h'), Ord('H')] then
      begin
        pd^ := '&';
        Inc(pd);
        pd^ := p^;
        Inc(pd);
        Inc(p);
        AIsHex := True;
      end
      else
        Dec(p);
    end;
  end;
  d := 0;
  e := 0;
  while p^ <> #0 do
  begin
    if Ord(p^) in [Ord('0') .. Ord('9')] then
    begin
      pd^ := p^;
      Inc(pd);
    end
    else if (p^ = '.') and (not AIsHex) then
    begin
      Inc(d);
      if (d = 1) and (nftFloat in Accepts) then
      begin
        pd^ := p^;
        Inc(pd);
      end;
    end
    else if (Ord(p^) in [Ord('e'), Ord('E')]) and (not AIsHex) then
    begin
      Inc(e);
      if (e = 1) and (nftFloat in Accepts) then
      begin
        if d <= 1 then
        begin
          pd^ := p^;
          Inc(pd);
          d := 0;
          NegPosCheck;
        end;
      end;
    end
    else if AIsHex and ((Ord(p^) in [Ord('a') .. Ord('f')]) or
      (Ord(p^) in [Ord('A') .. Ord('F')])) then
    begin
      pd^ := p^;
      Inc(pd);
    end;
    Inc(p);
  end;
  SetLength(Result, (IntPtr(pd) - IntPtr(pds)) shr 1);
end;

function MemScan(S: Pointer; len_s: Integer; sub: Pointer;
  len_sub: Integer): Pointer;
var
  pb_s, pb_sub, pc_sub, pc_s: PByte;
  remain: Integer;
begin
  if len_s > len_sub then
  begin
    pb_s := S;
    pb_sub := sub;
    Result := nil;
    while len_s >= len_sub do
    begin
      if pb_s^ = pb_sub^ then
      begin
        remain := len_sub - 1;
        pc_sub := pb_sub;
        pc_s := pb_s;
        Inc(pc_s);
        Inc(pc_sub);
        if BinaryCmp(pc_s, pc_sub, remain) = 0 then
        begin
          Result := pb_s;
          Break;
        end;
      end;
      Inc(pb_s);
      Dec(len_s);
    end;
  end
  else if len_s = len_sub then
  begin
    if CompareMem(S, sub, len_s) then
      Result := S
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function MemCompPascal(p1, p2: Pointer; L1, L2: Integer): Integer;
var
  l: Integer;
  ps1: PByte absolute p1;
  ps2: PByte absolute p2;
begin
  if L1 > L2 then
    l := L2
  else
    l := L1;
  while l > 4 do
  begin
    if PInteger(ps1)^ = PInteger(ps2)^ then
    begin
      Inc(ps1, 4);
      Inc(ps2, 4);
    end
    else
      Break;
  end;
  if l > 0 then
  begin
    Result := ps1^ - ps2^;
    if (Result = 0) and (l > 1) then
    begin
      Inc(ps1);
      Inc(ps2);
      Result := ps1^ - ps2^;
      if (Result = 0) and (L1 > 2) then
      begin
        Inc(ps1);
        Inc(ps2);
        Result := ps1^ - ps2^;
        if (Result = 0) and (L1 > 3) then
        begin
          Inc(ps1);
          Inc(ps2);
          Result := ps1^ - ps2^;
        end;
      end;
    end;
  end
  else // ==0
    Result := L1 - L2;
end;
{$IFDEF WIN32}

function MemCompAsm(p1, p2: Pointer; L1, L2: Integer): Integer;
label AssignL1, AdjustEnd, DoComp, ByBytes, ByLen, PopReg, C4, C3, c2, c1;
// EAX Temp 1
// EBX Temp 2
// ECX Min(L1,L2)
// EDX EndOf(P1)
// ESI P1
// EDI P2
begin
  asm
    push esi
    push edi
    push ebx
    mov esi,P1
    mov edi,p2
    mov eax,L1
    mov ebx,L2
    cmp eax,ebx
    jle AssignL1
    mov ecx,ebx
    jmp AdjustEnd

    AssignL1:// L1<=L2
    mov ecx,eax
    jmp AdjustEnd

    // 调整edx的值为需要比较的结束位置
    AdjustEnd:
    mov edx,esi
    add edx,ecx
    and edx,$FFFFFFFC
    and ecx,3

    DoComp:
    cmp esi,edx
    jge ByBytes
    mov eax,[esi]
    mov ebx,[edi]
    cmp eax,ebx
    jnz C4
    add esi,4
    add edi,4
    jmp DoComp

    C4: // 剩下>=4个字节时，直接减最近的4个字节
    bswap eax
    bswap ebx
    sub eax,ebx
    jmp PopReg

    ByBytes:
    cmp ecx,0// 没有可比的内容了，谁长就是谁大
    je ByLen
    cmp ecx,1// 剩下1个字节
    je C1
    cmp ecx,2// 剩下2个字节
    je C2
    // 剩下3个字节
    C3:
    xor eax,eax// eax清零
    xor ebx,ebx// ebx清零
    mov ax,WORD PTR [esi]
    mov bx,WORD PTR [edi]
    add esi,2
    add edi,2
    cmp eax,ebx
    je C1// 剩下一个需要比较的字节，跳到C1
    bswap eax
    bswap ebx
    sub eax,ebx
    jmp PopReg
    // 剩下两个字节
    C2:
    xor eax,eax// eax清零
    xor ebx,ebx// ebx清零
    mov ax,WORD PTR [esi]
    mov bx,WORD PTR [edi]
    cmp eax,ebx
    je ByLen// 能比较的都相等，看长度了
    bswap eax
    bswap ebx
    shr eax,16
    shr ebx,16
    sub eax,ebx
    jmp PopReg

    // 剩下一个字节
    C1:
    xor eax,eax// eax清零
    xor ebx,ebx// ebx清零
    mov al, BYTE PTR [esi]
    mov bl, BYTE PTR [edi]
    cmp eax,ebx
    je ByLen// 能比较的都相等，看长度了
    sub eax,ebx
    jmp PopReg;

    // 按长度比较
    ByLen:
    mov eax,L1
    sub eax,L2

    // 恢复保存的edi和esi值
    PopReg:
    pop ebx
    pop edi
    pop esi
    mov Result,eax
    // lea ebx,[ebp-10]
  end;
end;
{$ENDIF}

function BinaryCmp(const p1, p2: Pointer; len: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
{$IFDEF WIN32}
  Result := MemComp(p1, p2, len, len);
{$ELSE}
  if Assigned(VCMemCmp) then
    Result := VCMemCmp(p1, p2, len)
  else
    Result := MemComp(p1, p2, len, len)
{$ENDIF}
{$ELSE}
    Result := memcmp(p1, p2, len);
{$ENDIF}
end;

procedure SkipHex(var S: PQCharW);
begin
  while ((S^ >= '0') and (S^ <= '9')) or ((S^ >= 'a') and (S^ <= 'f')) or
    ((S^ >= 'A') and (S^ <= 'F')) do
    Inc(S);
end;

procedure SkipDec(var S: PQCharW);
begin
  while (S^ >= '0') and (S^ <= '9') do
    Inc(S);
end;

function ParseHex(var p: PQCharW; var Value: Int64): Integer;
var
  ps: PQCharW;
begin
  Value := 0;
  ps := p;
  while IsHexChar(p^) do
  begin
    Value := (Value shl 4) + HexValue(p^);
    Inc(p);
  end;
  Result := p - ps;
end;

function LeftStrCount(const S: QStringW; const sub: QStringW;
  AIgnoreCase: Boolean): Integer;
var
  ps, psub: PQCharW;
  l: Integer;
begin
  l := Length(sub);
  Result := 0;
  if (l > 0) and (Length(S) >= l) then
  begin
    ps := PQCharW(S);
    psub := PQCharW(sub);
    if AIgnoreCase then
    begin
      repeat
        ps := StrIStrW(ps, psub);
        if ps <> nil then
        begin
          Inc(Result);
          Inc(ps, l);
        end;
      until ps = nil;
    end
    else
    begin
      repeat
        ps := StrStrW(ps, psub);
        if ps <> nil then
        begin
          Inc(Result);
          Inc(ps, l);
        end;
      until ps = nil;
    end;
  end;
end;

function RightPosW(const S: QStringW; const sub: QStringW;
  AIgnoreCase: Boolean): Integer;
var
  ps, pe, psub, psube, pc, pt: PQCharW;
  LS, lsub: Integer;
begin
  lsub := Length(sub);
  LS := Length(S);
  Result := 0;
  if LS >= lsub then
  begin
    ps := Pointer(S);
    pe := ps + LS - 1;
    psub := Pointer(sub);
    psube := psub + lsub - 1;
    if AIgnoreCase then
    begin
      while pe - ps >= lsub - 1 do
      begin
        if (pe^ = psube^) or (CharUpperW(pe^) = CharUpperW(psube^)) then
        begin
          pt := psube - 1;
          pc := pe - 1;
          while (pt >= psub) and
            ((pc^ = pt^) or (CharUpperW(pc^) = CharUpperW(pt^))) do
          begin
            Dec(pt);
            Dec(pc);
          end;
          if pt < psub then
          begin
            Dec(pe, lsub);
            Result := pe - ps + 2;
            Exit;
          end;
        end;
        Dec(pe);
      end;
    end
    else
    begin
      while pe - ps >= lsub - 1 do
      begin
        if pe^ = psube^ then
        begin
          pt := psube - 1;
          pc := pe - 1;
          while (pt >= psub) and (pc^ = pt^) do
          begin
            Dec(pt);
            Dec(pc);
          end;
          if pt < psub then
          begin
            Dec(pe, lsub);
            Result := pe - ps + 2;
            Exit;
          end;
        end;
        Dec(pe);
      end;
    end;
  end;
end;

function ParseInt(var S: PQCharW; var ANum: Int64): Integer;
var
  ps: PQCharW;
  ANeg: Boolean;
begin
  ps := S;
  // 跳过16进制开始字符
  if S[0] = '$' then
  begin
    Inc(S);
    Result := ParseHex(S, ANum);
  end
  else if (S[0] = '0') and ((S[1] = 'x') or (S[1] = 'X')) then
  begin
    Inc(S, 2);
    Result := ParseHex(S, ANum);
  end
  else
  begin
    if (S^ = '-') then
    begin
      ANeg := True;
      Inc(S);
    end
    else
    begin
      ANeg := false;
      if S^ = '+' then
        Inc(S);
    end;
    ANum := 0;
    while (S^ >= '0') and (S^ <= '9') do
    begin
      ANum := ANum * 10 + Ord(S^) - Ord('0');
      if ANum < 0 then // 溢出？
      begin
        Result := 0;
        S := ps;
        Exit;
      end;
      Inc(S);
    end;
    if ANeg then
      ANum := -ANum;
    Result := S - ps;
  end;
end;

function ParseNumeric(var S: PQCharW; var ANum: Extended): Boolean;
var
  ps: PQCharW;
  function ParseHexInt: Boolean;
  var
    iVal: Int64;
  begin
    iVal := 0;
    while IsHexChar(S^) do
    begin
      iVal := (iVal shl 4) + HexValue(S^);
      Inc(S);
    end;
    Result := (S <> ps);
    ANum := iVal;
  end;

  function ParseDec: Boolean;
  var
    ACount: Integer;
    iVal: Int64;
    APow: Extended;
    ANeg: Boolean;
  begin
    try
      ANeg := S^ = '-';
      if ANeg then
        Inc(S);
      Result := ParseInt(S, iVal) > 0;
      if not Result then
        Exit;
      if ANeg then
        ANum := -iVal
      else
        ANum := iVal;
      if S^ = '.' then // 小数部分
      begin
        Inc(S);
        ACount := ParseInt(S, iVal);
        if ACount > 0 then
        begin
          if (ANum < 0) or ANeg then
            ANum := ANum - iVal / IntPower(10, ACount)
          else
            ANum := ANum + iVal / IntPower(10, ACount);
        end;
      end;
      if (S^ = 'e') or (S^ = 'E') then
      begin
        Inc(S);
        if ParseNumeric(S, APow) then
        begin
          ANum := ANum * Power(10, APow);

        end;
      end;
      Result := (S <> ps);
    except
      on e: EOverflow do
        Result := false;
    end;
  end;

begin
  ps := S;
  if (S^ = '$') or (S^ = '&') then
  begin
    Inc(S);
    Result := ParseHexInt;
    Exit;
  end
  else if (S[0] = '0') and ((S[1] = 'x') or (S[1] = 'X')) then
  begin
    Inc(S, 2);
    Result := ParseHexInt;
    Exit;
  end
  else
    Result := ParseDec;
  if not Result then
    S := ps;
end;

function NameOfW(const S: QStringW; ASpliter: QCharW): QStringW;
var
  p: PQCharW;
begin
  p := PQCharW(S);
  Result := DecodeTokenW(p, [ASpliter], WideChar(0), false);
end;

function ValueOfW(const S: QStringW; ASpliter: QCharW): QStringW;
var
  p: PQCharW;
  l: Integer;
begin
  p := PQCharW(S);
  if p^ = ASpliter then
  begin
    l := Length(S);
    Dec(l);
    SetLength(Result, l);
    Inc(p);
    Move(p^, PQCharW(Result)^, l shl 1);
  end
  else
  begin
    DecodeTokenW(p, [ASpliter], WideChar(0), false);
    if p^ <> #0 then
      Result := p
    else
      Result := S;
  end;
end;

function IndexOfNameW(AList: TStrings; const AName: QStringW;
  ASpliter: QCharW): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to AList.count - 1 do
  begin
    if NameOfW(AList[I], ASpliter) = AName then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function IndexOfValueW(AList: TStrings; const AValue: QStringW;
  ASpliter: QCharW): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to AList.count - 1 do
  begin
    if ValueOfW(AList[I], ASpliter) = AValue then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function DeleteCharW(const ASource, ADeletes: QStringW): QStringW;
var
  ps, pd: PQCharW;
  l, ACharLen: Integer;
begin
  l := Length(ASource);
  if (l > 0) and (Length(ADeletes) > 0) then
  begin
    SetLength(Result, l);
    ps := PQCharW(ASource);
    pd := PQCharW(Result);
    while l > 0 do
    begin
      if not CharInW(ps, PQCharW(ADeletes), @ACharLen) then
      begin
        pd^ := ps^;
        Inc(pd);
        ACharLen := CharSizeW(ps);
      end;
      Inc(ps, ACharLen);
      Dec(l, ACharLen);
    end;
    SetLength(Result, pd - PQCharW(Result));
  end
  else
    Result := ASource;
end;

function DeleteSideCharsW(const ASource: QStringW; ADeletes: QStringW;
  AIgnoreCase: Boolean): QStringW;
var
  ps, pd, pe: PQCharW;
  ATemp: QStringW;
begin
  if Length(ADeletes) = 0 then
    Result := ASource
  else
  begin
    if AIgnoreCase then
    begin
      ATemp := UpperCase(ASource);
      ADeletes := UpperCase(ADeletes);
      ps := PQCharW(ATemp);
      pe := ps + Length(ATemp);
    end
    else
    begin
      ps := PQCharW(ASource);
      pe := ps + Length(ASource);
    end;
    pd := PQCharW(ADeletes);
    while ps < pe do
    begin
      if CharInW(ps, pd) then
        Inc(ps, CharSizeW(ps))
      else
        Break;
    end;
    while pe > ps do
    begin
      Dec(pe);
      if (pe^ >= #$DB00) and (pe^ <= #$DFFF) then
        Dec(pe);
      if not CharInW(pe, pd) then
      begin
        Inc(pe, CharSizeW(pe));
        Break;
      end;
    end;
    Result := StrDupX(ps, pe - ps);
  end;
end;

function DeleteRightW(const S, ADelete: QStringW; AIgnoreCase: Boolean = false;
  ACount: Integer = MaxInt): QStringW;
var
  ps, pd, pe: PQCharW;
  LS, LD: Integer;
begin
  LS := Length(S);
  LD := Length(ADelete);
  if LS < LD then
    Result := S
  else if LD > 0 then
  begin
    pe := PQCharW(S) + Length(S);
    pd := PQCharW(ADelete);
    if AIgnoreCase then
    begin
      while LS >= LD do
      begin
        ps := pe - LD;
        if StrIStrW(ps, pd) = ps then
        begin
          pe := ps;
          Dec(LS, LD);
        end
        else
          Break;
      end;
    end
    else
    begin
      while LS >= LD do
      begin
        ps := pe - LD;
        if CompareMem(ps, pd, LD shl 1) then
        begin
          pe := ps;
          Dec(LS, LD);
        end
        else
          Break;
      end;
    end;
    SetLength(Result, LS);
    if LS > 0 then
      Move(PWideChar(S)^, PQCharW(Result)^, LS shl 1);
  end
  else
    Result := S;
end;

function DeleteLeftW(const S, ADelete: QStringW; AIgnoreCase: Boolean = false;
  ACount: Integer = MaxInt): QStringW;
var
  ps, pd: PQCharW;
  LS, LD: Integer;
begin
  LS := Length(S);
  LD := Length(ADelete);
  if LS < LD then
    Result := S
  else
  begin
    ps := PQCharW(S);
    pd := PQCharW(ADelete);
    if AIgnoreCase then
    begin
      while LS >= LD do
      begin
        if StartWithW(ps, pd, True) then
        begin
          Inc(ps, LD);
          Dec(LS, LD);
        end
        else
          Break;
      end;
    end
    else
    begin
      while LS >= LD do
      begin
        if CompareMem(ps, pd, LD shl 1) then
        begin
          Inc(ps, LD);
          Dec(LS, LD);
        end
        else
          Break;
      end;
    end;
    SetLength(Result, LS);
    if LS > 0 then
      Move(ps^, PQCharW(Result)^, LS shl 1);
  end;
end;

function ContainsCharW(const S, ACharList: QStringW): Boolean;
var
  ps: PQCharW;
  l: Integer;
begin
  l := Length(S);
  Result := false;
  if (l > 0) then
  begin
    if Length(ACharList) > 0 then
    begin
      ps := PQCharW(S);
      while l > 0 do
      begin
        if CharInW(ps, PQCharW(ACharList)) then
        begin
          Result := True;
          Break;
        end;
        Inc(ps);
        Dec(l);
      end;
    end;
  end;
end;

procedure StrCpyW(d: PQCharW; S: PQCharW; ACount: Integer);
begin
  while (S^ <> #0) and (ACount <> 0) do
  begin
    d^ := S^;
    Inc(d);
    Inc(S);
    Dec(ACount);
  end;
end;

function JavaEscape(const S: QStringW; ADoEscape: Boolean): QStringW;
var
  ASize: Integer;
  p, ps, pd: PWideChar;
begin
  ASize := Length(S);
  if ASize > 0 then
  begin
    p := Pointer(S);
    ps := p;
    while p^ <> #0 do
    begin
      if p^ < ' ' then // 控制字符
      begin
        if (p^ >= #7) and (p^ <= #13) then
          Inc(ASize);
      end
      else if p^ >= '~' then // 非可打印字符，转义的话使用 \uxxxx
      begin
        if ADoEscape then
          Inc(ASize, 5);
      end
      else
      begin
        if (p^ = '\') or (p^ = '''') or (p^ = '"') then // \->\\
          Inc(ASize);
      end;
      Inc(p);
    end;
    if ASize = Length(S) then
      Result := S
    else
    begin
      SetLength(Result, ASize);
      pd := Pointer(Result);
      p := ps;
      while p^ <> #0 do
      begin
        if p^ < ' ' then // 控制字符
        begin
          case p^ of
            #7:
              begin
                PInteger(pd)^ := $0061005C; // \a
                Inc(pd, 2);
              end;
            #8:
              begin
                PInteger(pd)^ := $0062005C; // \b
                Inc(pd, 2);
              end;
            #9:
              begin
                PInteger(pd)^ := $0074005C; // \t
                Inc(pd, 2);
              end;
            #10:
              begin
                PInteger(pd)^ := $006E005C; // \n
                Inc(pd, 2);
              end;
            #11:
              begin
                PInteger(pd)^ := $0076005C; // \v
                Inc(pd, 2);
              end;
            #12:
              begin
                PInteger(pd)^ := $0066005C; // \f
                Inc(pd, 2);
              end;
            #13:
              begin
                PInteger(pd)^ := $0072005C; // \r
                Inc(pd, 2);
              end
          else
            begin
              pd^ := p^;
              Inc(pd);
            end;
          end;
        end
        else if p^ >= '~' then // 非可打印字符，转义的话使用 \uxxxx
        begin
          if ADoEscape then // \uxxxx
          begin
            PInteger(pd)^ := $0075005C; // \u
            Inc(pd, 2);
            pd^ := LowerHexChars[(Ord(p^) shr 12) and $0F];
            Inc(pd);
            pd^ := LowerHexChars[(Ord(p^) shr 8) and $0F];
            Inc(pd);
            pd^ := LowerHexChars[(Ord(p^) shr 4) and $0F];
            Inc(pd);
            pd^ := LowerHexChars[Ord(p^) and $0F];
            Inc(pd);
          end
          else
          begin
            pd^ := p^;
            Inc(pd);
          end;
        end
        else
        begin
          case p^ of
            '\':
              begin
                PInteger(pd)^ := $005C005C; // \\
                Inc(pd, 2);
              end;
            '''':
              begin
                PInteger(pd)^ := $0027005C; // \'
                Inc(pd, 2);
              end;
            '"':
              begin
                PInteger(pd)^ := $0022005C; // \"
                Inc(pd, 2);
              end
          else
            begin
              pd^ := p^;
              Inc(pd);
            end;
          end;
        end;
        Inc(p);
      end;
    end;
  end
  else
    SetLength(Result, 0);
end;

function JavaUnescape(const S: QStringW; AStrictEscape: Boolean): QStringW;
var
  ps, p, pd: PWideChar;
  ASize: Integer;
begin
  ASize := Length(S);
  if ASize > 0 then
  begin
    p := Pointer(S);
    ps := p;
    while p^ <> #0 do
    begin
      if p^ = '\' then
      begin
        Inc(p);
        case p^ of
          'a', 'b', 'f', 'n', 'r', 't', 'v', '''', '"', '\', '?':
            begin
              Dec(ASize);
              Inc(p);
            end;
          'x': // \xNN
            begin
              Dec(ASize, 2);
              Inc(p, 3);
            end;
          'u': // \uxxxx
            begin
              if IsHexChar(p[1]) and IsHexChar(p[2]) and IsHexChar(p[3]) and
                IsHexChar(p[4]) then
              begin
                Dec(ASize, 5);
                Inc(p, 5);
              end
              else
                raise Exception.CreateFmt(SBadJavaEscape, [Copy(p, 0, 6)]);
            end;
          'U': // \Uxxxxxxxx
            begin
              if IsHexChar(p[1]) and IsHexChar(p[2]) and IsHexChar(p[3]) and
                IsHexChar(p[4]) and IsHexChar(p[5]) and IsHexChar(p[6]) and
                IsHexChar(p[7]) and IsHexChar(p[8]) then
              begin
                Dec(ASize, 9);
                Inc(p, 9);
              end
              else
                raise Exception.CreateFmt(SBadJavaEscape, [Copy(p, 0, 10)]);
            end
        else
          begin
            if IsOctChar(p[0]) then
            begin
              if IsOctChar(p[1]) then
              begin
                if IsOctChar(p[2]) then
                begin
                  Dec(ASize, 3);
                  Inc(p, 3);
                end
                else
                begin
                  Dec(ASize, 2);
                  Inc(p, 2);
                end;
              end
              else
              begin
                Dec(ASize, 1);
                Inc(p, 1);
              end;
            end
            else if AStrictEscape then
            begin
              raise Exception.CreateFmt(SBadJavaEscape, [Copy(p, 0, 6)]);
            end;
          end;
        end;
      end
      else
        Inc(p);
    end;
    if Length(S) = ASize then // 尺寸相等，没有需要处理的转义
      Result := S
    else
    begin
      SetLength(Result, ASize);
      pd := Pointer(Result);
      p := ps;
      while p^ <> #0 do
      begin
        if p^ = '\' then
        begin
          Inc(p);
          case p^ of
            'a':
              pd^ := #7;
            'b':
              pd^ := #8;
            'f':
              pd^ := #12;
            'n':
              pd^ := #10;
            'r':
              pd^ := #13;
            't':
              pd^ := #9;
            'v':
              pd^ := #11;
            '''':
              pd^ := '''';
            '"':
              pd^ := '"';
            '\':
              pd^ := '\';
            '?':
              pd^ := '?';
            'x': // \xNN
              begin
                pd^ := WideChar((HexValue(p[1]) shl 4) or HexValue(p[2]));
                Inc(p, 2);
              end;
            'u': // \uxxxx
              begin
                pd^ := WideChar((HexValue(p[1]) shl 12) or
                  (HexValue(p[2]) shl 8) or (HexValue(p[3]) shl 4) or
                  HexValue(p[4]));
                Inc(p, 4);
              end;
            'U': // \Uxxxxxxxx
              begin
                pd^ := WideChar((HexValue(p[1]) shl 12) or
                  (HexValue(p[2]) shl 8) or (HexValue(p[3]) shl 4) or
                  HexValue(p[4]));
                Inc(pd);
                pd^ := WideChar((HexValue(p[5]) shl 12) or
                  (HexValue(p[6]) shl 8) or (HexValue(p[7]) shl 4) or
                  HexValue(p[8]));
                Inc(p, 8);
              end
          else
            begin
              if IsOctChar(p[0]) then
              begin
                ASize := HexValue(p[0]);
                if IsOctChar(p[1]) then
                begin
                  ASize := (ASize shl 3) + HexValue(p[1]);
                  if IsOctChar(p[2]) then
                  begin
                    pd^ := WideChar((ASize shl 3) + HexValue(p[2]));
                    Inc(p, 2);
                  end
                  else
                  begin
                    pd^ := WideChar(ASize);
                    Inc(p);
                  end;
                end
                else
                  pd^ := WideChar(ASize);
              end
              else
                pd^ := p^;
            end;
          end;
          Inc(pd);
        end
        else
        begin
          pd^ := p^;
          Inc(pd);
        end;
        Inc(p);
      end;
    end;
  end
  else
    SetLength(Result, 0);
end;

function HtmlEscape(const S: QStringW): QStringW;
var
  p, pd: PQCharW;
  AFound: Boolean;
  I: Integer;
begin
  if Length(S) > 0 then
  begin
    System.SetLength(Result, Length(S) shl 3); // 转义串最长不超过8个字符，长度*8肯定够了
    p := PWideChar(S);
    pd := PWideChar(Result);
    while p^ <> #0 do
    begin
      AFound := false;
      for I := 0 to 92 do
      begin
        if HtmlEscapeChars[I shl 1] = p^ then
        begin
          AFound := True;
          StrCpyW(pd, PQCharW(HtmlEscapeChars[(I shl 1) + 1]));
          Inc(pd, Length(HtmlEscapeChars[(I shl 1) + 1]));
          Break;
        end;
      end;
      // end for
      if not AFound then
      begin
        pd^ := p^;
        Inc(pd);
      end; // end if
      Inc(p);
    end; // end while
    SetLength(Result, pd - PQCharW(Result));
  end // end if
  else
    Result := '';
end;

type
  THTMLEscapeHashItem = record
    Hash: Integer;
    Char: Word;
    Next: Byte;
  end;

function UnescapeHtmlChar(var p: PQCharW): QCharW;
const
  HtmlUnescapeTable: array [0 .. 94] of THTMLEscapeHashItem =
    ((Hash: 1667591796; Char: 162; Next: 255), // 0:0:&cent;
    (Hash: 1768257635; Char: 161; Next: 10), // 1:15:&iexcl;
    (Hash: 1886352750; Char: 163; Next: 34), // 2:55:&pound;
    (Hash: 1869900140; Char: 245; Next: 255), // 3:3:&otilde;
    (Hash: 1853122924; Char: 241; Next: 255), // 4:4:&ntilde;
    (Hash: 1936226560; Char: 173; Next: 66), // 5:54:&shy;
    (Hash: 1684367104; Char: 176; Next: 64), // 6:31:&deg;
    (Hash: 1769043301; Char: 191; Next: 255), // 7:78:&iquest;
    (Hash: 1096901493; Char: 193; Next: 255), // 8:79:&Aacute;
    (Hash: 1095068777; Char: 198; Next: 12), // 9:81:&AElig;
    (Hash: 1130587492; Char: 199; Next: 84), // 10:15:&Ccedil;
    (Hash: 1651668578; Char: 166; Next: 89), // 11:11:&brvbar;
    (Hash: 1164142962; Char: 202; Next: 255), // 12:81:&Ecirc;
    (Hash: 1634562048; Char: 38; Next: 18), // 13:13:&amp;
    (Hash: 1231516257; Char: 204; Next: 255), // 14:86:&Igrave;
    (Hash: 1851945840; Char: 32; Next: 1), // 15:15:&nbsp;
    (Hash: 1231119221; Char: 205; Next: 24), // 16:71:&Iacute;
    (Hash: 1919248128; Char: 174; Next: 40), // 17:17:&reg;
    (Hash: 1163151360; Char: 208; Next: 255), // 18:13:&ETH;
    (Hash: 1316252012; Char: 209; Next: 255), // 19:36:&Ntilde;
    (Hash: 1869835361; Char: 248; Next: 255), // 20:20:&oslash;
    (Hash: 1869966700; Char: 246; Next: 255), // 21:21:&ouml;
    (Hash: 1919512167; Char: 197; Next: 255), // 22:22:&ring;
    (Hash: 1633908084; Char: 180; Next: 85), // 23:23:&acute;
    (Hash: 1331915122; Char: 212; Next: 255), // 24:71:&Ocirc;
    (Hash: 1918988661; Char: 187; Next: 255), // 25:25:&raquo;
    (Hash: 1333029228; Char: 213; Next: 41), // 26:35:&Otilde;
    (Hash: 1769303404; Char: 239; Next: 76), // 27:27:&iuml;
    (Hash: 1953066341; Char: 215; Next: 255), // 28:53:&times;
    (Hash: 1432445813; Char: 218; Next: 255), // 29:59:&Uacute;
    (Hash: 1432578418; Char: 219; Next: 255), // 30:65:&Ucirc;
    (Hash: 1818325365; Char: 171; Next: 6), // 31:31:&laquo;
    (Hash: 1835098994; Char: 175; Next: 88), // 32:32:&macr;
    (Hash: 1868653429; Char: 243; Next: 82), // 33:33:&oacute;
    (Hash: 1499554677; Char: 221; Next: 255), // 34:55:&Yacute;
    (Hash: 1835623282; Char: 181; Next: 26), // 35:35:&micro;
    (Hash: 1970105344; Char: 168; Next: 19), // 36:36:&uml;
    (Hash: 1937402985; Char: 223; Next: 67), // 37:63:&szlig;
    (Hash: 1633772405; Char: 225; Next: 255), // 38:47:&aacute;
    (Hash: 1767990133; Char: 237; Next: 70), // 39:39:&iacute;
    (Hash: 1635019116; Char: 227; Next: 255), // 40:17:&atilde;
    (Hash: 1635085676; Char: 228; Next: 255), // 41:35:&auml;
    (Hash: 1969713761; Char: 249; Next: 255), // 42:42:&ugrave;
    (Hash: 1700881269; Char: 233; Next: 255), // 43:43:&eacute;
    (Hash: 1667458404; Char: 231; Next: 255), // 44:80:&ccedil;
    (Hash: 1768122738; Char: 238; Next: 255), // 45:45:&icirc;
    (Hash: 1701278305; Char: 232; Next: 255), // 46:58:&egrave;
    (Hash: 1433759084; Char: 220; Next: 38), // 47:47:&Uuml;
    (Hash: 1667589225; Char: 184; Next: 68), // 48:48:&cedil;
    (Hash: 1098148204; Char: 195; Next: 51), // 49:49:&Atilde;
    (Hash: 1869767782; Char: 170; Next: 255), // 50:50:&ordf;
    (Hash: 1701013874; Char: 234; Next: 72), // 51:49:&ecirc;
    (Hash: 1332964449; Char: 216; Next: 255), // 52:52:&Oslash;
    (Hash: 1333095788; Char: 214; Next: 28), // 53:53:&Ouml;
    (Hash: 1903521652; Char: 34; Next: 5), // 54:54:&quot;
    (Hash: 1634758515; Char: 39; Next: 2), // 55:55:&apos;
    (Hash: 2036690432; Char: 165; Next: 255), // 56:56:&yen;
    (Hash: 1869767789; Char: 186; Next: 255), // 57:57:&ordm;
    (Hash: 1668641394; Char: 164; Next: 46), // 58:58:&curren;
    (Hash: 1232432492; Char: 207; Next: 29), // 59:59:&Iuml;
    (Hash: 1668247673; Char: 169; Next: 255), // 60:60:&copy;
    (Hash: 1634036841; Char: 230; Next: 255), // 61:61:&aelig;
    (Hash: 1634169441; Char: 224; Next: 255), // 62:62:&agrave;
    (Hash: 1165323628; Char: 203; Next: 37), // 63:63:&Euml;
    (Hash: 1702194540; Char: 235; Next: 255), // 64:31:&euml;
    (Hash: 1331782517; Char: 211; Next: 30), // 65:65:&Oacute;
    (Hash: 1768387169; Char: 236; Next: 255), // 66:54:&igrave;
    (Hash: 1768256616; Char: 240; Next: 255), // 67:63:&ieth;
    (Hash: 1869050465; Char: 242; Next: 255), // 68:48:&ograve;
    (Hash: 1885434465; Char: 182; Next: 255), // 69:69:&para;
    (Hash: 1868786034; Char: 244; Next: 255), // 70:39:&ocirc;
    (Hash: 1886156147; Char: 177; Next: 16), // 71:71:&plusmn;
    (Hash: 1684633193; Char: 247; Next: 255), // 72:49:&divide;
    (Hash: 1414025042; Char: 222; Next: 255), // 73:73:&THORN;
    (Hash: 1432842849; Char: 217; Next: 255), // 74:74:&Ugrave;
    (Hash: 1164010357; Char: 201; Next: 255), // 75:75:&Eacute;
    (Hash: 1969316725; Char: 250; Next: 255), // 76:27:&uacute;
    (Hash: 1231251826; Char: 206; Next: 255), // 77:77:&Icirc;
    (Hash: 1936024436; Char: 167; Next: 7), // 78:78:&sect;
    (Hash: 1852797952; Char: 172; Next: 8), // 79:79:&not;
    (Hash: 1332179553; Char: 210; Next: 44), // 80:80:&Ograve;
    (Hash: 1819541504; Char: 60; Next: 9), // 81:81:&lt;
    (Hash: 1969449330; Char: 251; Next: 255), // 82:33:&ucirc;
    (Hash: 1835623524; Char: 183; Next: 255), // 83:83:&middot;
    (Hash: 1970629996; Char: 252; Next: 255), // 84:15:&uuml;
    (Hash: 2036425589; Char: 253; Next: 255), // 85:23:&yacute;
    (Hash: 1735655424; Char: 62; Next: 14), // 86:86:&gt;
    (Hash: 1667854947; Char: 194; Next: 255), // 87:87:&circ;
    (Hash: 1953001330; Char: 254; Next: 255), // 88:32:&thorn;
    (Hash: 2037738860; Char: 255; Next: 255), // 89:11:&yuml;
    (Hash: 1164407393; Char: 200; Next: 255), // 90:90:&Egrave;
    (Hash: 1634888046; Char: 229; Next: 255), // 91:91:&aring;
    (Hash: 0; Char: 0; Next: 255), // 92:Not Used
    (Hash: 0; Char: 0; Next: 255), // 93:Not Used
    (Hash: 1097298529; Char: 192; Next: 255) // 94:94:&Agrave;
    );
  function HashOfEscape: Integer;
  var
    c: Integer;
    R: array [0 .. 3] of Byte absolute Result;
  begin
    Inc(p); // Skip #
    c := 3;
    Result := 0;
    while (p^ <> #0) and (c >= 0) do
    begin
      if p^ = ';' then
        Exit;
      R[c] := Ord(p^);
      Inc(p);
      Dec(c);
    end;
    while p^ <> #0 do
    begin
      if p^ = ';' then
        Exit
      else
        Inc(p);
    end;
    Result := 0;
  end;

var
  AHash, ANext: Integer;
begin
  AHash := HashOfEscape;
  Result := #0;
  if AHash <> 0 then
  begin
    ANext := AHash mod 97;
    while ANext <> 255 do
    begin
      if HtmlUnescapeTable[ANext].Hash = AHash then
      begin
        Result := QCharW(HtmlUnescapeTable[ANext].Char);
        Break;
      end
      else
        ANext := HtmlUnescapeTable[ANext].Next;
    end;
  end;
end;

function HtmlUnescape(const S: QStringW): QStringW;
var
  p, pd, ps: PQCharW;
  l: Integer;
begin
  if Length(S) > 0 then
  begin
    System.SetLength(Result, Length(S));
    p := PQCharW(S);
    pd := PQCharW(Result);
    while p^ <> #0 do
    begin
      if p^ = '&' then
      begin
        if p[1] = '#' then
        begin
          ps := p;
          Inc(p, 2);
          l := 0;
          if (p^ = 'x') or (p^ = 'X') then
          begin
            Inc(p);
            while IsHexChar(p^) do
            begin
              l := l shl 4 + HexValue(p^);
              Inc(p);
            end;
          end
          else
          begin
            while (p^ >= '0') and (p^ <= '9') do
            begin
              l := l * 10 + Ord(p^) - Ord('0');
              Inc(p);
            end;
          end;
          if p^ = ';' then
          begin
            pd^ := QCharW(l);
            Inc(pd);
          end
          else
          begin
            pd^ := ps^;
            Inc(pd);
            p := ps;
          end;
        end
        else
        begin
          pd^ := UnescapeHtmlChar(p);
          if pd^ = #0 then
            pd^ := p^;
          Inc(pd);
        end; // end else
      end // end else
      else
      begin
        pd^ := p^;
        Inc(pd);
      end;
      Inc(p);
    end; // end while
    SetLength(Result, pd - PWideChar(Result));
  end // end if
  else
    Result := '';
end;

function HtmlTrimText(const S: QStringW): QStringW;
var
  ps, pe: PQCharW;
  l: Integer;
begin
  if Length(S) > 0 then
  begin
    ps := PQCharW(S);
    pe := ps + System.Length(S) - 1;
    while IsSpaceW(ps) do
      Inc(ps);
    while IsSpaceW(pe) do
      Dec(pe);
    l := pe - ps + 1;
    SetLength(Result, l);
    Move(ps^, PQCharW(Result)^, l shl 1);
  end
  else
    Result := '';
end;

function UrlEncode(const ABytes: PByte; l: Integer; ASpacesAsPlus: Boolean)
  : QStringW; overload;
const
  SafeChars: array [33 .. 127] of Byte = ( //
    0, 0, 0, 0, 0, 0, 0, 0, //
    0, 0, 0, 0, 1, 1, 0, 1, //
    1, 1, 1, 1, 1, 1, 1, 1, //
    1, 0, 0, 0, 0, 0, 0, 0, //
    1, 1, 1, 1, 1, 1, 1, 1, //
    1, 1, 1, 1, 1, 1, 1, 1, //
    1, 1, 1, 1, 1, 1, 1, 1, //
    1, 1, 0, 0, 0, 0, 1, 0, //
    1, 1, 1, 1, 1, 1, 1, 1, //
    1, 1, 1, 1, 1, 1, 1, 1, //
    1, 1, 1, 1, 1, 1, 1, 1, //
    1, 1, 0, 0, 0, 1, 0);
  HexChars: array [0 .. 15] of QCharW = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  c: Integer;
  ps, pe: PByte;
  pd: PQCharW;
begin
  // 计算实际的长度
  c := 0;
  ps := PByte(ABytes);
  pe := PByte(IntPtr(ps) + l);
  while IntPtr(ps) < IntPtr(pe) do
  begin
    if (ps^ = Ord('%')) and (IntPtr(pe) - IntPtr(ps) > 2) and
      (PByte(IntPtr(ps) + 1)^ in [Ord('a') .. Ord('f'), Ord('A') .. Ord('F'),
      Ord('0') .. Ord('9')]) and
      (PByte(IntPtr(ps) + 2)^ in [Ord('a') .. Ord('f'), Ord('A') .. Ord('F'),
      Ord('0') .. Ord('9')]) then // 原来就是%xx?     %09%32
      Inc(ps, 3)
    else
    begin
      if (ps^ < 32) or (ps^ > 127) or (SafeChars[ps^] = 0) then
        Inc(c);
      Inc(ps);
    end;
  end;
  SetLength(Result, l + (c shl 1));
  pd := PQCharW(Result);
  ps := ABytes;
  while IntPtr(ps) < IntPtr(pe) do
  begin
    if (ps^ = Ord('%')) and (IntPtr(pe) - IntPtr(ps) > 2) and
      (PByte(IntPtr(ps) + 1)^ in [Ord('a') .. Ord('f'), Ord('A') .. Ord('F'),
      Ord('0') .. Ord('9')]) and
      (PByte(IntPtr(ps) + 2)^ in [Ord('a') .. Ord('f'), Ord('A') .. Ord('F'),
      Ord('0') .. Ord('9')]) then // 原来就是%xx?
    begin
      pd^ := '%';
      Inc(pd);
      Inc(ps);
      pd^ := QCharW(ps^);
      Inc(pd);
      Inc(ps);
      pd^ := QCharW(ps^);
      Inc(pd);
      Inc(ps);
    end
    else
    begin
      if (ps^ in [33 .. 127]) and (SafeChars[ps^] <> 0) then
        pd^ := QCharW(ps^)
      else if (ps^ = 32) and ASpacesAsPlus then
        pd^ := '+'
      else
      begin
        pd^ := '%';
        Inc(pd);
        pd^ := HexChars[(ps^ shr 4) and $0F];
        Inc(pd);
        pd^ := HexChars[ps^ and $0F];
      end;
      Inc(pd);
      Inc(ps);
    end;
  end;
end;

function UrlEncode(const ABytes: TBytes; ASpacesAsPlus: Boolean)
  : QStringW; overload;
begin
  if Length(ABytes) > 0 then
    Result := UrlEncode(@ABytes[0], Length(ABytes), ASpacesAsPlus)
  else
    SetLength(Result, 0);
end;

function UrlEncode(const S: QStringW; ASpacesAsPlus, AUtf8Encode: Boolean)
  : QStringW; overload;
var
  ABytes: QStringA;
begin
  if Length(S) > 0 then
  begin
    if AUtf8Encode then
      ABytes := qstring.Utf8Encode(S)
    else
      ABytes := AnsiEncode(S);
    Result := UrlEncode(PByte(PQCharA(ABytes)), ABytes.Length, ASpacesAsPlus);
  end
  else
    Result := S;
end;

function UrlDecode(const AUrl: QStringW;
  var AScheme, AHost, ADocument: QStringW; var APort: Word; AParams: TStrings;
  AUtf8Encode: Boolean): Boolean;
var
  p, ps: PQCharW;
  V, N: QStringW;
  iV: Int64;
  function DecodeUrlStr(const S: QStringW; var AResult: QStringW): Boolean;
  var
    pd, pds, pb: PQCharA;
    ps: PQCharW;
    ADoUnescape: Boolean;
    ABuf: TBytes;

  begin
    Result := True;
    ps := PQCharW(S);
    ADoUnescape := false;
    while ps^ <> #0 do
    begin
      if ps^ = '%' then
      begin
        ADoUnescape := True;
        Break;
      end
      else
        Inc(ps);
    end;
    if ADoUnescape then
    begin
      SetLength(ABuf, Length(S));
      pd := PQCharA(@ABuf[0]);
      ps := PQCharW(S);
      pds := pd;
      while ps^ <> #0 do
      begin
        if ps^ = '%' then
        begin
          Inc(ps);
          if IsHexChar(ps^) then
          begin
            pd^ := HexValue(ps^) shl 4;
            Inc(ps);
            if IsHexChar(ps^) then
            begin
              pd^ := pd^ or HexValue(ps^);
              Inc(ps);
            end
            else
            begin
              Result := false;
              Break;
            end;
          end
          else
          begin
            Result := false;
            Break;
          end;
        end
        else
        begin
          pd^ := QCharA(ps^);
          Inc(ps);
        end;
        Inc(pd);
      end;
      if AUtf8Encode then
      begin
        if not Utf8Decode(pds, IntPtr(pd) - IntPtr(pds), AResult, pb) then
          AResult := AnsiDecode(pds, IntPtr(pd) - IntPtr(pds));
      end
      else
        AResult := AnsiDecode(pds, IntPtr(pd) - IntPtr(pds));
    end
    else
      AResult := S;
  end;

const
  HostEnd: PQCharW = ':/';
  DocEnd: PQCharW = '?';
  ParamDelimiter: PQCharW = '&';
begin
  Result := True;
  p := PQCharW(AUrl);
  SkipSpaceW(p);
  ps := p;
  p := StrStrW(ps, '://');
  if p <> nil then
  begin
    AScheme := StrDupX(ps, p - ps);
    Inc(p, 3);
    ps := p;
  end
  else
    AScheme := '';
  p := ps;
  SkipUntilW(p, HostEnd);
  AHost := StrDupX(ps, p - ps);
  if p^ = ':' then
  begin
    if ParseInt(p, iV) <> 0 then
    begin
      APort := Word(iV);
      if p^ = '/' then
        Inc(p);
    end
    else
      APort := 0;
  end
  else // 未指定端口
  begin
    APort := 0;
    if p^ = '/' then
      Inc(p);
  end;
  if Assigned(AParams) then
  begin
    ps := p;
    SkipUntilW(p, DocEnd);
    if p^ = '?' then
    begin
      ADocument := StrDupX(ps, p - ps);
      Inc(p);
      AParams.BeginUpdate;
      try
        while p^ <> #0 do
        begin
          V := DecodeTokenW(p, ParamDelimiter, QCharW(0), false, True);
          if DecodeUrlStr(NameOfW(V, '='), N) and
            DecodeUrlStr(ValueOfW(V, '='), V) then
            AParams.Add(N + '=' + V)
          else
          begin
            Result := false;
            Break;
          end;
        end;
      finally
        AParams.EndUpdate;
      end;
    end
    else
    begin
      ADocument := ps;
      AParams.Clear;
    end;
  end;
end;

// 下面是一些辅助函数
function ParseDateTime(S: PWideChar; var AResult: TDateTime): Boolean;
var
  Y, M, d, H, N, Sec, MS: Cardinal;
  AQuoter: WideChar;
  ADate: TDateTime;
  function ParseNum(var N: Cardinal): Boolean;
  var
    neg: Boolean;
    ps: PQCharW;
  begin
    N := 0;
    ps := S;
    if S^ = '-' then
    begin
      neg := True;
      Inc(S);
    end
    else
      neg := false;
    while S^ <> #0 do
    begin
      if (S^ >= '0') and (S^ <= '9') then
      begin
        N := N * 10 + Ord(S^) - 48;
        if N >= 10000 then
        begin
          Result := false;
          Exit;
        end;
        Inc(S);
      end
      else
        Break;
    end;
    if neg then
      N := -N;
    Result := ps <> S;
  end;

begin
  if (S^ = '"') or (S^ = '''') then
  begin
    AQuoter := S^;
    Inc(S);
  end
  else
    AQuoter := #0;
  Result := ParseNum(Y);
  if not Result then
    Exit;
  if (S^ = '-') or (S^ = '/') then
  begin
    Inc(S);
    Result := ParseNum(M);
    if (not Result) or ((S^ <> '-') and (S^ <> '/')) then
    begin
      Result := false;
      Exit;
    end;
    Inc(S);
    Result := ParseNum(d);
    if (not Result) or ((S^ <> 'T') and (S^ <> ' ') and (S^ <> #0)) then
    begin
      Result := false;
      Exit;
    end;
    if S^ <> #0 then
      Inc(S);
    if d > 31 then // D -> Y
    begin
      if M > 12 then // M/D/Y M -> D, D->Y, Y->M
        Result := TryEncodeDate(d, Y, M, ADate)
      else // D/M/Y
        Result := TryEncodeDate(d, M, Y, ADate);
    end
    else
      Result := TryEncodeDate(Y, M, d, ADate);
    if not Result then
      Exit;
    SkipSpaceW(S);
    if S^ <> #0 then
    begin
      if not ParseNum(H) then // 没跟时间值
      begin
        AResult := ADate;
        Exit;
      end;
      if S^ <> ':' then
      begin
        if H in [0 .. 23] then
          AResult := ADate + EncodeTime(H, 0, 0, 0)
        else
          Result := false;
        Exit;
      end;
      Inc(S);
    end
    else
    begin
      AResult := ADate;
      Exit;
    end;
  end
  else if S^ = ':' then
  begin
    ADate := 0;
    H := Y;
    Inc(S);
  end
  else
  begin
    Result := false;
    Exit;
  end;
  if H > 23 then
  begin
    Result := false;
    Exit;
  end;
  if not ParseNum(N) then
  begin
    if AQuoter <> #0 then
    begin
      if S^ = AQuoter then
        AResult := ADate + EncodeTime(H, 0, 0, 0)
      else
        Result := false;
    end
    else
      AResult := ADate + EncodeTime(H, 0, 0, 0);
    Exit;
  end
  else if N > 59 then
  begin
    Result := false;
    Exit;
  end;
  Sec := 0;
  MS := 0;
  if S^ = ':' then
  begin
    Inc(S);
    if not ParseNum(Sec) then
    begin
      if AQuoter <> #0 then
      begin
        if S^ = AQuoter then
          AResult := ADate + EncodeTime(H, N, 0, 0)
        else
          Result := false;
      end
      else
        AResult := ADate + EncodeTime(H, N, 0, 0);
      Exit;
    end
    else if Sec > 59 then
    begin
      Result := false;
      Exit;
    end;
    if S^ = '.' then
    begin
      Inc(S);
      if not ParseNum(MS) then
      begin
        if AQuoter <> #0 then
        begin
          if AQuoter = S^ then
            AResult := ADate + EncodeTime(H, N, Sec, 0)
          else
            Result := false;
        end
        else
          AResult := ADate + EncodeTime(H, N, Sec, 0);
        Exit;
      end
      else if MS >= 1000 then // 超过1000是以微秒为单位计时的，转换为毫秒
      begin
        while MS >= 1000 do
          MS := MS div 10;
      end;
      if AQuoter <> #0 then
      begin
        if AQuoter = S^ then
          AResult := ADate + EncodeTime(H, N, Sec, MS)
        else
          Result := false;
        Exit;
      end
      else
        AResult := ADate + EncodeTime(H, N, Sec, MS);
    end
    else
    begin
      if AQuoter <> #0 then
      begin
        if AQuoter = S^ then
          AResult := ADate + EncodeTime(H, N, Sec, 0)
        else
          Result := false;
      end
      else
        AResult := ADate + EncodeTime(H, N, Sec, 0)
    end;
  end
  else
  begin
    if AQuoter <> #0 then
    begin
      if AQuoter = S^ then
        AResult := ADate + EncodeTime(H, N, 0, 0)
      else
        Result := false;
    end
    else
      AResult := ADate + EncodeTime(H, N, 0, 0);
  end;
end;

function ParseWebTime(p: PWideChar; var AResult: TDateTime): Boolean;
var
  I: Integer;
  Y, M, d, H, N, S: Integer;
  AFound: Boolean;
const
  MonthNames: array [0 .. 11] of QStringW = ('Jan', 'Feb', 'Mar', 'Apr', 'May',
    'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  WeekNames: array [0 .. 6] of QStringW = ('Mon', 'Tue', 'Wed', 'Thu', 'Fri',
    'Sat', 'Sun');
  Comma: PWideChar = ',';
  Digits: PWideChar = '0123456789';
  // Web 日期格式：星期, 日 月 年 时:分:秒 GMT
begin
  // 跳过星期，这个可以直接通过日期计算出来，不需要
  SkipSpaceW(p);
  AFound := false;
  for I := 0 to High(WeekNames) do
  begin
    if StartWithW(p, PQCharW(WeekNames[I]), true) then
    begin
      Inc(p, Length(WeekNames[I]));
      SkipSpaceW(p);
      AFound := True;
      Break;
    end;
  end;
  if (not AFound) or (p^ <> Comma) then
  begin
    Result := False;
    Exit;
  end;
  Inc(p);
  SkipSpaceW(p);
  d := 0;
  // 日期
  while (p^ >= '0') and (p^ <= '9') do
  begin
    d := d * 10 + Ord(p^) - Ord('0');
    Inc(p);
  end;
  if (not IsSpaceW(p)) or (d < 1) or (d > 31) then
  begin
    Result := false;
    Exit;
  end;
  SkipSpaceW(p);
  M := 0;
  for I := 0 to 11 do
  begin
    if StartWithW(p, PWideChar(MonthNames[I]), True) then
    begin
      M := I + 1;
      Inc(p, Length(MonthNames[I]));
      Break;
    end;
  end;
  if (not IsSpaceW(p)) or (M < 1) or (M > 12) then
  begin
    Result := false;
    Exit;
  end;
  SkipSpaceW(p);
  Y := 0;
  while (p^ >= '0') and (p^ <= '9') do
  begin
    Y := Y * 10 + Ord(p^) - Ord('0');
    Inc(p);
  end;
  if not IsSpaceW(p) then
  begin
    Result := False;
    Exit;
  end;
  SkipSpaceW(p);
  H := 0;
  while (p^ >= '0') and (p^ <= '9') do
  begin
    H := H * 10 + Ord(p^) - Ord('0');
    Inc(p);
  end;
  if p^ <> ':' then
  begin
    Result := False;
    Exit;
  end;
  Inc(p);
  N := 0;
  while (p^ >= '0') and (p^ <= '9') do
  begin
    N := N * 10 + Ord(p^) - Ord('0');
    Inc(p);
  end;
  if p^ <> ':' then
  begin
    Result := False;
    Exit;
  end;
  Inc(p);
  S := 0;
  while (p^ >= '0') and (p^ <= '9') do
  begin
    S := S * 10 + Ord(p^) - Ord('0');
    Inc(p);
  end;
  SkipSpaceW(p);
  if StartWithW(p, 'GMT', true) then
    Inc(p, 3);
  Result := TryEncodeDateTime(Y, M, d, H, N, S, 0, AResult);
end;

function EncodeWebTime(ATime: TDateTime): String;
var
  Y, M, d, H, N, S, MS: Word;
const
  DefShortMonthNames: array [1 .. 12] of String = ('Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  DefShortDayNames: array [1 .. 7] of String = ('Sun', 'Mon', 'Tue', 'Wed',
    'Thu', 'Fri', 'Sat');
begin
  DecodeDateTime(ATime, Y, M, d, H, N, S, MS);
  Result := DefShortDayNames[DayOfWeek(ATime)] + ',' + IntToStr(d) + ' ' +
    DefShortMonthNames[M] + ' ' + IntToStr(Y) + ' ' + IntToStr(H) + ':' +
    IntToStr(N) + ':' + IntToStr(S);
end;

function RollupSize(ASize: Int64): QStringW;
var
  AIdx, R1, s1: Int64;
  AIsNeg: Boolean;
const
  Units: array [0 .. 3] of QStringW = ('GB', 'MB', 'KB', 'B');
begin
  AIsNeg := (ASize < 0);
  AIdx := 3;
  R1 := 0;
  if AIsNeg then
    ASize := -ASize;
  Result := '';
  while (AIdx >= 0) do
  begin
    s1 := ASize mod 1024;
    ASize := ASize shr 10;
    if (ASize = 0) or (AIdx = 0) then
    begin
      R1 := R1 * 100 div 1024;
      if R1 > 0 then
      begin
        if R1 >= 10 then
          Result := IntToStr(s1) + '.' + IntToStr(R1) + Units[AIdx]
        else
          Result := IntToStr(s1) + '.' + '0' + IntToStr(R1) + Units[AIdx];
      end
      else
        Result := IntToStr(s1) + Units[AIdx];
      Break;
    end;
    R1 := s1;
    Dec(AIdx);
  end;
  if AIsNeg then
    Result := '-' + Result;
end;

function RollupTime(ASeconds: Int64; AHideZero: Boolean): QStringW;
var
  H, N, d: Integer;
begin
  if ASeconds = 0 then
  begin
    if AHideZero then
      Result := ''
    else
      Result := '0' + SSecondName;
  end
  else
  begin
    Result := '';
    d := ASeconds div 86400;
    ASeconds := ASeconds mod 86400;
    H := ASeconds div 3600;
    ASeconds := ASeconds mod 3600;
    N := ASeconds div 60;
    ASeconds := ASeconds mod 60;
    if d > 0 then
      Result := IntToStr(d) + SDayName
    else
      Result := '';
    if H > 0 then
      Result := Result + IntToStr(H) + SHourName;
    if N > 0 then
      Result := Result + IntToStr(N) + SMinuteName;
    if ASeconds > 0 then
      Result := Result + IntToStr(ASeconds) + SSecondName;
  end;
end;
{ QStringA }

function QStringA.From(p: PQCharA; AOffset, ALen: Integer): PQStringA;
begin
  SetLength(ALen);
  Inc(p, AOffset);
  Move(p^, PQCharA(@FValue[1])^, ALen);
  Result := @Self;
end;

function QStringA.From(const S: QStringA; AOffset: Integer): PQStringA;
begin
  Result := From(PQCharA(S), AOffset, S.Length);
end;

function QStringA.GetChars(AIndex: Integer): QCharA;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise Exception.CreateFmt(SOutOfIndex, [AIndex, 0, Length - 1]);
  Result := FValue[AIndex + 1];
end;

function QStringA.GetData: PByte;
begin
  Result := @FValue[1];
end;

class operator QStringA.Implicit(const S: QStringW): QStringA;
begin
  Result := qstring.AnsiEncode(S);
end;

class operator QStringA.Implicit(const S: QStringA): PQCharA;
begin
  Result := PQCharA(@S.FValue[1]);
end;

function QStringA.GetIsUtf8: Boolean;
begin
  if System.Length(FValue) > 0 then
    Result := (FValue[0] = 1)
  else
    Result := false;
end;

function QStringA.GetLength: Integer;
begin
  // QStringA.FValue[0]存贮编码类型，0-ANSI,1-UTF8，末尾存贮字符串的\0结束符
  Result := System.Length(FValue);
  if Result >= 2 then
    Dec(Result, 2)
  else
    Result := 0;
end;

class operator QStringA.Implicit(const S: QStringA): TBytes;
var
  l: Integer;
begin
  l := System.Length(S.FValue) - 1;
  System.SetLength(Result, l);
  if l > 0 then
    Move(S.FValue[1], Result[0], l);
end;

procedure QStringA.SetChars(AIndex: Integer; const Value: QCharA);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise Exception.CreateFmt(SOutOfIndex, [AIndex, 0, Length - 1]);
  FValue[AIndex + 1] := Value;
end;

procedure QStringA.SetIsUtf8(const Value: Boolean);
begin
  if System.Length(FValue) = 0 then
    System.SetLength(FValue, 1);
  FValue[0] := Byte(Value);
end;

procedure QStringA.SetLength(const Value: Integer);
begin
  if Value < 0 then
  begin
    if System.Length(FValue) > 0 then
      System.SetLength(FValue, 1)
    else
    begin
      System.SetLength(FValue, 1);
      FValue[0] := 0; // ANSI
    end;
  end
  else
  begin
    System.SetLength(FValue, Value + 2);
    FValue[Value + 1] := 0;
  end;
end;

class function QStringA.UpperCase(S: PQCharA): QStringA;
var
  l: Integer;
  ps: PQCharA;
begin
  if Assigned(S) then
  begin
    ps := S;
    while S^ <> 0 do
    begin
      Inc(S)
    end;
    l := IntPtr(S) - IntPtr(ps);
    Result.SetLength(l);
    S := ps;
    ps := PQCharA(Result);
    while S^ <> 0 do
    begin
      ps^ := CharUpperA(S^);
      Inc(ps);
      Inc(S);
    end;
  end
  else
    Result.SetLength(0);
end;

function QStringA.UpperCase: QStringA;
var
  l: Integer;
  pd, ps: PQCharA;
begin
  l := System.Length(FValue);
  System.SetLength(Result.FValue, l);
  if l > 0 then
  begin
    Result.FValue[0] := FValue[0];
    Dec(l);
    pd := PQCharA(Result);
    ps := PQCharA(Self);
    while l > 0 do
    begin
      pd^ := CharUpperA(ps^);
      Inc(pd);
      Inc(ps);
      Dec(l);
    end;
  end;
end;

class operator QStringA.Implicit(const ABytes: TBytes): QStringA;
var
  l: Integer;
begin
  l := System.Length(ABytes);
  Result.Length := l;
  if l > 0 then
    Move(ABytes[0], Result.FValue[1], l);
end;

class operator QStringA.Implicit(const S: QStringA): QStringW;
begin
  Result := AnsiDecode(S);
end;

function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): QStringW;
const
  B2HConvert: array [0 .. 15] of QCharW = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  B2HConvertL: array [0 .. 15] of QCharW = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  pd: PQCharW;
  pb: PByte;
begin
  SetLength(Result, l shl 1);
  pd := PQCharW(Result);
  pb := p;
  if ALowerCase then
  begin
    while l > 0 do
    begin
      pd^ := B2HConvertL[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvertL[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end
  else
  begin
    while l > 0 do
    begin
      pd^ := B2HConvert[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvert[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end;
end;

function BinToHex(const ABytes: TBytes; ALowerCase: Boolean): QStringW;
begin
  Result := BinToHex(@ABytes[0], Length(ABytes), ALowerCase);
end;

procedure HexToBin(const S: QStringW; var AResult: TBytes);
var
  l: Integer;
  p, ps: PQCharW;
  pd: PByte;
begin
  l := System.Length(S);
  SetLength(AResult, l shr 1);
  p := PQCharW(S);
  ps := p;
  pd := @AResult[0];
  while p - ps < l do
  begin
    if IsHexChar(p[0]) and IsHexChar(p[1]) then
    begin
      pd^ := (HexValue(p[0]) shl 4) + HexValue(p[1]);
      Inc(pd);
      Inc(p, 2);
    end
    else
    begin
      SetLength(AResult, 0);
      Exit;
    end;
  end;
end;

function HexToBin(const S: QStringW): TBytes;
begin
  HexToBin(S, Result);
end;

procedure FreeObject(AObject: TObject);
begin
{$IFDEF AUTOREFCOUNT}
  AObject.DisposeOf;
{$ELSE}
  AObject.Free;
{$ENDIF}
end;

procedure FreeAndNilObject(var AObject);
begin
  FreeObject(TObject(AObject));
  Pointer(AObject) := nil;
end;

function HashOf(p: Pointer; l: Integer): Cardinal;
{$IFDEF WIN32}
label A00, A01;
begin
  asm
    push ebx
    mov eax,l
    mov ebx,0
    cmp eax,ebx
    jz A01
    xor    eax, eax
    mov    edx, p
    mov    ebx,edx
    add    ebx,l
    A00:
    imul   eax,131
    movzx  ecx, BYTE ptr [edx]
    inc    edx
    add    eax, ecx
    cmp   ebx, edx
    jne    A00
    A01:
    pop ebx
    mov Result,eax
  end;
{$ELSE}
var
  pe: PByte;
  ps: PByte absolute p;
const
  seed = 131;
  // 31 131 1313 13131 131313 etc..
begin
  pe := p;
  Inc(pe, l);
  Result := 0;
  while IntPtr(ps) < IntPtr(pe) do
  begin
    Result := Result * seed + ps^;
    Inc(ps);
  end;
  Result := Result and $7FFFFFFF;
{$ENDIF}
end;

class operator QStringA.Implicit(const S: PQCharA): QStringA;
var
  p: PQCharA;
begin
  if S <> nil then
  begin
    p := S;
    while p^ <> 0 do
      Inc(p);
    Result.Length := IntPtr(p) - IntPtr(S);
    Move(S^, PQCharA(Result)^, Result.Length);
  end
  else
    Result.Length := 0;
end;
{$IFNDEF NEXTGEN}

class operator QStringA.Implicit(const S: AnsiString): QStringA;
begin
  Result.From(PQCharA(S), 0, System.Length(S));
end;

class operator QStringA.Implicit(const S: QStringA): AnsiString;
begin
  System.SetLength(Result, S.Length);
  if S.Length > 0 then
    Move(PQCharA(S)^, PAnsiChar(Result)^, S.Length);
end;

{$ENDIF}

class function QStringA.LowerCase(S: PQCharA): QStringA;
var
  l: Integer;
  ps: PQCharA;
begin
  if Assigned(S) then
  begin
    ps := S;
    while S^ <> 0 do
    begin
      Inc(S)
    end;
    l := IntPtr(S) - IntPtr(ps);
    Result.SetLength(l);
    S := ps;
    ps := PQCharA(Result);
    while S^ <> 0 do
    begin
      ps^ := CharLowerA(S^);
      Inc(ps);
      Inc(S);
    end;
  end
  else
    Result.SetLength(0);
end;

function QStringA.LowerCase: QStringA;
var
  l: Integer;
  pd, ps: PQCharA;
begin
  l := System.Length(FValue);
  System.SetLength(Result.FValue, l);
  if l > 0 then
  begin
    Result.FValue[0] := FValue[0];
    Dec(l);
    pd := PQCharA(Result);
    ps := PQCharA(Self);
    while l > 0 do
    begin
      pd^ := CharLowerA(ps^);
      Inc(pd);
      Inc(ps);
      Dec(l);
    end;
  end;
end;

function QStringA.Cat(p: PQCharA; ALen: Integer): PQStringA;
var
  l: Integer;
begin
  l := Length;
  SetLength(l + ALen);
  Move(p^, FValue[1 + l], ALen);
  Result := @Self;
end;

function QStringA.Cat(const S: QStringA): PQStringA;
begin
  Result := Cat(PQCharA(S), S.Length);
end;

{ TQStringCatHelperW }

function TQStringCatHelperW.Back(ALen: Integer): TQStringCatHelperW;
begin
  Result := Self;
  Dec(FDest, ALen);
  if FDest < FStart then
    FDest := FStart;
end;

function TQStringCatHelperW.BackIf(const S: PQCharW): TQStringCatHelperW;
var
  ps: PQCharW;
begin
  Result := Self;
  ps := FStart;
  while FDest > ps do
  begin
    if (FDest[-1] >= #$DC00) and (FDest[-1] <= #$DFFF) then
    begin
      if CharInW(FDest - 2, S) then
        Dec(FDest, 2)
      else
        Break;
    end
    else if CharInW(FDest - 1, S) then
      Dec(FDest)
    else
      Break;
  end;
end;

function TQStringCatHelperW.Cat(const S: QStringW): TQStringCatHelperW;
begin
  Result := Cat(PQCharW(S), Length(S));
end;

function TQStringCatHelperW.Cat(p: PQCharW; len: Integer): TQStringCatHelperW;
begin
  Result := Self;
  if len < 0 then
  begin
    while p^ <> #0 do
    begin
      if Position >= FSize then
        NeedSize(FSize + FBlockSize);
      FDest^ := p^;
      Inc(p);
      Inc(FDest);
    end;
  end
  else
  begin
    NeedSize(-len);
    Move(p^, FDest^, len shl 1);
    Inc(FDest, len);
  end;
end;

function TQStringCatHelperW.Cat(c: QCharW): TQStringCatHelperW;
begin
  if Position >= FSize then
    NeedSize(-1);
  FDest^ := c;
  Inc(FDest);
  Result := Self;
end;

function TQStringCatHelperW.Cat(const V: Double): TQStringCatHelperW;
begin
  Result := Cat(FloatToStr(V));
end;

function TQStringCatHelperW.Cat(const V: Int64): TQStringCatHelperW;
begin
  Result := Cat(IntToStr(V));
end;

function TQStringCatHelperW.Cat(const V: Boolean): TQStringCatHelperW;
begin
  Result := Cat(BoolToStr(V, True));
end;

function TQStringCatHelperW.Cat(const V: TGuid): TQStringCatHelperW;
begin
  Result := Cat(GuidToString(V));
end;

function TQStringCatHelperW.Cat(const V: Currency): TQStringCatHelperW;
begin
  Result := Cat(CurrToStr(V));
end;

constructor TQStringCatHelperW.Create(ASize: Integer);
begin
  inherited Create;
  if ASize < 8192 then
    ASize := 8192
  else if (ASize and $3FF) <> 0 then
    ASize := ((ASize shr 10) + 1) shr 1;
  FBlockSize := ASize;
  NeedSize(FBlockSize);
end;

function TQStringCatHelperW.EndWith(const S: String;
  AIgnoreCase: Boolean): Boolean;
var
  p: PQCharW;
begin
  p := FDest;
  Dec(p, Length(S));
  if p >= FStart then
    Result := StrNCmpW(p, PQCharW(S), AIgnoreCase, Length(S)) = 0
  else
    Result := False;
end;

constructor TQStringCatHelperW.Create;
begin
  inherited Create;
  FBlockSize := 8192;
  NeedSize(FBlockSize);
end;

function TQStringCatHelperW.GetChars(AIndex: Integer): QCharW;
begin
  Result := FStart[AIndex];
end;

function TQStringCatHelperW.GetIsEmpty: Boolean;
begin
  Result := FDest <> FStart;
end;

function TQStringCatHelperW.GetPosition: Integer;
begin
  Result := FDest - FStart;
end;

function TQStringCatHelperW.GetValue: QStringW;
var
  l: Integer;
begin
  l := Position;
  SetLength(Result, l);
  Move(FStart^, PQCharW(Result)^, l shl 1);
end;

procedure TQStringCatHelperW.IncSize(ADelta: Integer);
begin
  NeedSize(-ADelta);
end;

procedure TQStringCatHelperW.LoadFromFile(const AFileName: QStringW);
begin
  Reset;
  Cat(LoadTextW(AFileName));
end;

procedure TQStringCatHelperW.LoadFromStream(const AStream: TStream);
begin
  Reset;
  Cat(LoadTextW(AStream));
end;

procedure TQStringCatHelperW.NeedSize(ASize: Integer);
var
  Offset: Integer;
begin
  Offset := FDest - FStart;
  if ASize < 0 then
    ASize := Offset - ASize;
  if ASize > FSize then
  begin
{$IFDEF DEBUG}
    Inc(FAllocTimes);
{$ENDIF}
    FSize := ((ASize + FBlockSize) div FBlockSize) * FBlockSize;
    SetLength(FValue, FSize);
    FStart := PQCharW(@FValue[0]);
    FDest := FStart + Offset;
    FLast := FStart + FSize;
  end;
end;

function TQStringCatHelperW.Replicate(const S: QStringW; count: Integer)
  : TQStringCatHelperW;
var
  ps: PQCharW;
  l: Integer;
begin
  Result := Self;
  if count > 0 then
  begin
    ps := PQCharW(S);
    l := Length(S);
    while count > 0 do
    begin
      Cat(ps, l);
      Dec(count);
    end;
  end;
end;

procedure TQStringCatHelperW.Reset;
begin
  FDest := FStart;
end;

procedure TQStringCatHelperW.SetDest(const Value: PQCharW);
begin
  if Value < FStart then
    FDest := FStart
  else if Value > FLast then
    FDest := FLast
  else
    FDest := Value;
end;

procedure TQStringCatHelperW.SetPosition(const Value: Integer);
begin
  if Value <= 0 then
    FDest := FStart
  else if Value > Length(FValue) then
  begin
    NeedSize(Value);
    FDest := FStart + Value;
  end
  else
    FDest := FStart + Value;
end;

procedure TQStringCatHelperW.TrimRight;
var
  pd: PQCharW;
begin
  pd := FDest;
  Dec(pd);
  while FStart < pd do
  begin
    if IsSpaceW(pd) then
      Dec(pd)
    else
      Break;
  end;
  Inc(pd);
  FDest := pd;
end;

function TQStringCatHelperW.Cat(const V: Variant): TQStringCatHelperW;
begin
  Result := Cat(VarToStr(V));
end;

{ TQPtr }

class function TQPtr.Bind(AObject: TObject): IQPtr;
begin
  Result := TQPtr.Create(AObject);
end;

class function TQPtr.Bind(AData: Pointer; AOnFree: TQPtrFreeEventG): IQPtr;
var
  ATemp: TQPtr;
begin
  ATemp := TQPtr.Create(AData);
  ATemp.FOnFree.Method.Data := nil;
  ATemp.FOnFree.OnFreeG := AOnFree;
  Result := ATemp;
end;

class function TQPtr.Bind(AData: Pointer; AOnFree: TQPtrFreeEvent): IQPtr;
var
  ATemp: TQPtr;
begin
  ATemp := TQPtr.Create(AData);
{$IFDEF NEXTGEN}
  PQPtrFreeEvent(@ATemp.FOnFree.OnFree)^ := AOnFree;
{$ELSE}
  ATemp.FOnFree.OnFree := AOnFree;
{$ENDIF}
  Result := ATemp;
end;

{$IFDEF UNICODE}

class function TQPtr.Bind(AData: Pointer; AOnFree: TQPtrFreeEventA): IQPtr;
var
  ATemp: TQPtr;
begin
  ATemp := TQPtr.Create(AData);
  ATemp.FOnFree.Method.Data := Pointer(-1);
  PQPtrFreeEventA(@ATemp.FOnFree.OnFreeA)^ := AOnFree;
  Result := ATemp;
end;
{$ENDIF}

constructor TQPtr.Create(AObject: Pointer);
begin
  inherited Create;
  FObject := AObject;
end;

destructor TQPtr.Destroy;
begin
  if Assigned(FObject) then
  begin
    if FOnFree.Method.Code <> nil then
    begin
      if FOnFree.Method.Data = nil then
        FOnFree.OnFreeG(FObject)
{$IFDEF UNICODE}
      else if FOnFree.Method.Data = Pointer(-1) then
        TQPtrFreeEventA(FOnFree.OnFreeA)(FObject)
{$ENDIF}
      else
{$IFDEF NEXTGEN}
      begin
        PQPtrFreeEvent(FOnFree.OnFree)^(FObject);
        PQPtrFreeEvent(FOnFree.OnFree)^ := nil;
      end;
{$ELSE}
      FOnFree.OnFree(FObject);
{$ENDIF}
    end
    else
      FreeAndNil(FObject);
  end;
  inherited;
end;

function TQPtr.Get: Pointer;
begin
  Result := FObject;
end;

// 兼容2007版的原子操作接口
{$IF RTLVersion<24}

function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; inline;
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedCompareExchange(Target, Value, Comparand);
{$ELSE}
  Result := TInterlocked.CompareExchange(Target, Value, Comparand);
{$ENDIF}
end;

function AtomicCmpExchange(var Target: Pointer; Value: Pointer;
  Comparand: Pointer): Pointer; inline;
begin
{$IFDEF MSWINDOWS}
  Result := Pointer(InterlockedCompareExchange(PInteger(@Target)^,
    Integer(Value), Integer(Comparand)));
{$ELSE}
  Result := TInterlocked.CompareExchange(Target, Value, Comparand);
{$ENDIF}
end;

function AtomicIncrement(var Target: Integer; const Value: Integer)
  : Integer; inline;
begin
{$IFDEF MSWINDOWS}
  if Value = 1 then
    Result := InterlockedIncrement(Target)
  else if Value = -1 then
    Result := InterlockedDecrement(Target)
  else
    Result := InterlockedExchangeAdd(Target, Value);
{$ELSE}
  if Value = 1 then
    Result := TInterlocked.Increment(Target)
  else if Value = -1 then
    Result := TInterlocked.Decrement(Target)
  else
    Result := TInterlocked.Add(Target, Value);
{$ENDIF}
end;

function AtomicDecrement(var Target: Integer): Integer; inline;
begin
  // Result := InterlockedDecrement(Target);
  Result := AtomicIncrement(Target, -1);
end;

function AtomicExchange(var Target: Integer; Value: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedExchange(Target, Value);
{$ELSE}
  Result := TInterlocked.Exchange(Target, Value);
{$ENDIF}
end;

function AtomicExchange(var Target: Pointer; Value: Pointer): Pointer;
begin
{$IFDEF MSWINDOWS}
{$IF RTLVersion>19}
  Result := InterlockedExchangePointer(Target, Value);
{$ELSE}
  Result := Pointer(IntPtr(InterlockedExchange(IntPtr(Target), IntPtr(Value))));
{$IFEND}
{$ELSE}
  Result := TInterlocked.Exchange(Target, Value);
{$ENDIF}
end;
{$IFEND <XE5}

// 位与，返回原值
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  I: Integer;
begin
  repeat
    Result := Dest;
    I := Result and AMask;
  until AtomicCmpExchange(Dest, I, Result) = Result;
end;

// 位或，返回原值
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  I: Integer;
begin
  repeat
    Result := Dest;
    I := Result or AMask;
  until AtomicCmpExchange(Dest, I, Result) = Result;
end;

{ TQBytesCatHelper }

function TQBytesCatHelper.Back(ALen: Integer): TQBytesCatHelper;
begin
  Result := Self;
  Dec(FDest, ALen);
  if IntPtr(FDest) < IntPtr(FStart) then
    FDest := FStart;
end;

function TQBytesCatHelper.Cat(const V: Double): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Double));
end;

function TQBytesCatHelper.Cat(const V: Currency): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Currency));
end;

function TQBytesCatHelper.Cat(const V: Boolean): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Boolean));
end;

function TQBytesCatHelper.Cat(const S: QStringW): TQBytesCatHelper;
begin
  Result := Cat(PQCharW(S), System.Length(S) shl 1);
end;

function TQBytesCatHelper.Cat(const V: Byte): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Byte));
end;

function TQBytesCatHelper.Cat(const V: Int64): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Int64));
end;

function TQBytesCatHelper.Cat(const c: QCharW): TQBytesCatHelper;
begin
  Result := Cat(@c, SizeOf(QCharW));
end;

function TQBytesCatHelper.Cat(const V: Variant): TQBytesCatHelper;
begin
  // ??这是一个有问题的实现，临时先这样，回头抽空改
  Result := Cat(@V, SizeOf(Variant));
end;

function TQBytesCatHelper.Cat(const V: QStringA; ACStyle: Boolean)
  : TQBytesCatHelper;
begin
  if ACStyle then
    Result := Cat(PQCharA(V), V.Length + 1)
  else
    Result := Cat(PQCharA(V), V.Length);
end;

{$IFNDEF NEXTGEN}

function TQBytesCatHelper.Cat(const V: AnsiChar): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(AnsiChar));
end;

function TQBytesCatHelper.Cat(const V: AnsiString): TQBytesCatHelper;
begin
  Result := Cat(PAnsiChar(V), System.Length(V));
end;
{$ENDIF}

function TQBytesCatHelper.Cat(const V: Single): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Single));
end;

function TQBytesCatHelper.Cat(const V: Cardinal): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Cardinal));
end;

function TQBytesCatHelper.Cat(const V: Smallint): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Smallint));
end;

function TQBytesCatHelper.Cat(const V: Word): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Word));
end;

function TQBytesCatHelper.Cat(const V: Shortint): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Shortint));
end;

function TQBytesCatHelper.Cat(const V: Integer): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(Integer));
end;

function TQBytesCatHelper.Cat(const ABytes: TBytes): TQBytesCatHelper;
begin
  if Length(ABytes) > 0 then
    Result := Cat(@ABytes[0], Length(ABytes))
  else
    Result := Self;
end;

function TQBytesCatHelper.Cat(const AData: Pointer; const ALen: Integer)
  : TQBytesCatHelper;
begin
  Result := Self;
  NeedSize(-ALen);
  Move(AData^, FDest^, ALen);
  Inc(FDest, ALen);
end;

function TQBytesCatHelper.Cat(const V: TGuid): TQBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(TGuid));
end;

constructor TQBytesCatHelper.Create(ASize: Integer);
begin
  inherited Create;
  FBlockSize := ASize;
  NeedSize(FBlockSize);
end;

function TQBytesCatHelper.Delete(AStart, ACount: Cardinal): TQBytesCatHelper;
begin
  Result := Self;
  if AStart < Cardinal(Position) then
  begin
    if Cardinal(Position) - AStart < ACount then
      FDest := PByte(UIntPtr(FStart) + AStart)
    else
    begin
      Move(PByte(UIntPtr(FStart) + AStart + ACount)^,
        PByte(UIntPtr(FStart) + AStart)^, ACount);
      Dec(FDest, ACount);
    end;
  end;
end;

constructor TQBytesCatHelper.Create;
begin
  inherited Create;
  FBlockSize := 8192;
  NeedSize(FBlockSize);
end;

function TQBytesCatHelper.GetBytes(AIndex: Integer): Byte;
begin
  Result := FValue[AIndex];
end;

function TQBytesCatHelper.GetPosition: Integer;
begin
  Result := IntPtr(FDest) - IntPtr(FStart);
end;

function TQBytesCatHelper.GetValue: TBytes;
var
  ALen: Integer;
begin
  ALen := Position;
  SetLength(Result, ALen);
  if ALen > 0 then
    Move(FValue[0], Result[0], ALen);
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Int64)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, SizeOf(Int64));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Integer)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, SizeOf(Integer));
end;
{$IFNDEF NEXTGEN}

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: AnsiString)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, PAnsiChar(V), Length(V));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: AnsiChar)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, 1);
end;
{$ENDIF}

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Cardinal)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, SizeOf(Cardinal));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Shortint)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, SizeOf(Shortint));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Byte)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, 1);
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Smallint)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, SizeOf(Smallint));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Word)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, SizeOf(Word));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: QStringA;
  ACStyle: Boolean): TQBytesCatHelper;
begin
  if ACStyle then
    Result := Insert(AIndex, PQCharA(V), V.Length + 1)
  else
    Result := Insert(AIndex, PQCharA(V), V.Length);
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Currency)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, SizeOf(Currency));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Boolean)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, SizeOf(Boolean));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Variant)
  : TQBytesCatHelper;
begin
  // ??这是一个有问题的实现，临时先这样，回头抽空改
  Result := Insert(AIndex, @V, sizeof(Variant));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: TGuid)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, sizeof(V));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Double)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, sizeof(V));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const S: QStringW)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, PQCharW(S), Length(S) shl 1);
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const c: QCharW)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @c, SizeOf(c));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const V: Single)
  : TQBytesCatHelper;
begin
  Result := Insert(AIndex, @V, SizeOf(V));
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const ABytes: TBytes)
  : TQBytesCatHelper;
begin
  if Length(ABytes) > 0 then
    Result := Insert(AIndex, @ABytes[0], Length(ABytes))
  else
    Result := Self;
end;

function TQBytesCatHelper.Insert(AIndex: Cardinal; const AData: Pointer;
  const ALen: Integer): TQBytesCatHelper;
begin
  if AIndex >= Cardinal(Position) then
    Result := Cat(AData, ALen)
  else
  begin
    NeedSize(-ALen);
    Move(PByte(UIntPtr(FStart) + AIndex)^,
      PByte(IntPtr(FStart) + ALen + Integer(AIndex))^, ALen);
    Move(AData^, PByte(UIntPtr(FStart) + AIndex)^, ALen);
    Inc(FDest, ALen);
    Result := Self;
  end;
end;

procedure TQBytesCatHelper.NeedSize(ASize: Integer);
var
  Offset: Integer;
begin
  Offset := IntPtr(FDest) - IntPtr(FStart);
  if ASize < 0 then
    ASize := Offset - ASize;
  if ASize > FSize then
  begin
    FSize := ((ASize + FBlockSize) div FBlockSize) * FBlockSize;
    SetLength(FValue, FSize);
    FStart := @FValue[0];
    FDest := PByte(IntPtr(FStart) + Offset);
  end;
end;

function TQBytesCatHelper.Replicate(const ABytes: TBytes; ACount: Integer)
  : TQBytesCatHelper;
var
  l: Integer;
begin
  Result := Self;
  l := Length(ABytes);
  if l > 0 then
  begin
    NeedSize(-l * ACount);
    while ACount > 0 do
    begin
      Move(ABytes[0], FDest^, l);
      Inc(FDest, l);
      Dec(ACount);
    end;
  end;
end;

procedure TQBytesCatHelper.Reset;
begin
  FDest := FStart;
end;

procedure TQBytesCatHelper.SetCapacity(const Value: Integer);
begin
  if FSize <> Value then
    NeedSize(Value);
end;

procedure TQBytesCatHelper.SetPosition(const Value: Integer);
begin
  if Value <= 0 then
    FDest := FStart
  else if Value > Length(FValue) then
  begin
    NeedSize(Value);
    FDest := Pointer(IntPtr(FStart) + Value);
  end
  else
    FDest := Pointer(IntPtr(FStart) + Value);
end;

function NewId: TGuid;
begin
  CreateGUID(Result);
end;

function SameId(const V1, V2: TGuid): Boolean;
var
  I1: array [0 .. 1] of Int64 absolute V1;
  I2: array [0 .. 1] of Int64 absolute V2;
begin
  Result := (I1[0] = I2[0]) and (I1[1] = I2[1]);
end;

function StrLikeX(var S: PQCharW; pat: PQCharW; AIgnoreCase: Boolean): PQCharW;
const
  CHAR_DIGITS = -1;
  CHAR_NODIGITS = -2;
  CHAR_SPACES = -3;
  CHAR_NOSPACES = -4;
var
  Accept: Boolean;
  ACharCode, AEndCode: Integer;
  AToken: QStringW;
  ps, pt, os: PQCharW;
  // >0 正常的字符编码
  // <0 特殊范围
  function Unescape(var T: PQCharW): Integer;
  begin
    if T^ = '\' then
    begin
      Inc(T);
      case T^ of
        'b':
          begin
            Inc(T);
            Result := 7;
          end;
        'd':
          begin
            Inc(T);
            Result := CHAR_DIGITS;
          end;
        'D':
          begin
            Inc(T);
            Result := CHAR_NODIGITS;
          end;
        'r':
          begin
            Inc(T);
            Result := 13;
          end;
        'n':
          begin
            Inc(T);
            Result := 10;
          end;
        't':
          begin
            Inc(T);
            Result := 9;
          end;
        'f': // \f
          begin
            Inc(T);
            Result := 12;
          end;
        'v': // \v
          begin
            Inc(T);
            Result := 11;
          end;
        's': // 空白字符
          begin
            Inc(T);
            Result := CHAR_SPACES;
          end;
        'S': // 非空白
          begin
            Inc(T);
            Result := CHAR_NOSPACES;
          end;
        'u': // Unicode字符
          begin
            if IsHexChar(T[1]) and IsHexChar(T[2]) and IsHexChar(T[3]) and
              IsHexChar(T[4]) then
            begin
              Result := (HexValue(T[1]) shl 12) or (HexValue(T[2]) shl 8) or
                (HexValue(T[3]) shl 4) or HexValue(T[4]);
              Inc(T, 5);
            end
            else
              raise Exception.CreateFmt(SCharNeeded,
                ['0-9A-Fa-f', StrDupW(T, 0, 4)]);
          end
      else
        begin
          Inc(T);
          Result := Ord(S^);
        end;
      end;
    end
    else
    begin
      Result := Ord(T^);
    end
  end;

  function IsDigit: Boolean;
  begin
    Result := ((S^ >= '0') and (S^ <= '9')) or
      ((S^ >= #65296) and (S^ <= #65305));
  end;
  function IsMatch(AStart, AEnd: Integer): Boolean;
  var
    ACode: Integer;
  begin
    case AStart of
      CHAR_DIGITS:
        Result := IsDigit;
      CHAR_NODIGITS:
        Result := not IsDigit;
      CHAR_SPACES:
        Result := IsSpaceW(S);
      CHAR_NOSPACES:
        Result := not IsSpaceW(S)
    else
      begin
        ACode := Ord(S^);
        Result := (ACode >= AStart) and (ACode <= AEnd);
        if (not Result) and AIgnoreCase then
        begin
          ACode := Ord(CharUpperW(S^));
          AStart := Ord(CharUpperW(QCharW(AStart)));
          AEnd := Ord(CharUpperW(QCharW(AEnd)));
          Result := (ACode >= AStart) and (ACode <= AEnd);
        end;
        // 如果是扩展区字符，需要两个连续的转义
        if Result and ((ACode >= $D800) and (ACode <= $DFFF)) then
        begin
          Inc(S);
          if pat^ = '\' then
          begin
            ACode := Unescape(pat);
            Result := Ord(S^) = ACode;
          end
          else
            Result := false;
        end;
      end;
    end;
  end;

  function IsIn: Boolean;
  const
    SetEndChar: PQCharW = ']';
  begin
    Result := false;
    while (pat^ <> #0) and (pat^ <> ']') do
    begin
      ACharCode := Unescape(pat);
      if pat^ = '-' then // a-z这种范围
      begin
        Inc(pat);
        if pat^ <> ']' then
          AEndCode := Unescape(pat)
        else
        begin
          raise Exception.Create(SRangeEndNeeded);
        end;
      end
      else
        AEndCode := ACharCode;
      Result := IsMatch(ACharCode, AEndCode);
      if Result then // 在其中的话，忽略掉后面的判定
      begin
        Inc(S);
        SkipUntilW(pat, SetEndChar);
        if pat^ <> ']' then
          raise Exception.CreateFmt(SCharNeeded, [']']);
      end
      else
        Inc(pat);
    end;
  end;

begin
  // SQL Like 语法：
  // _ 代表一个字符
  // % * 代表任意字符
  // [字符列表] 列表中任意字符
  // [^字符列表]/[!字符列表] 非列表中任意字符
  // 以下为QDAC扩展
  // \ 转义
  // \d 数字（全角和半角）
  // \D 非数字（含全角）
  // \s 空白字符
  // \S 非空白字符
  os := S;
  Result := nil;
  while (pat^ <> #0) and (S^ <> #0) do
  begin
    case pat^ of
      '_':
        begin
          Inc(S, CharSizeW(S));
          Inc(pat);
        end;
      '[': // 字符列表
        begin
          Inc(pat);
          if (pat^ = '!') or (pat^ = '^') then
          begin
            Inc(pat);
            Accept := not IsIn;
          end
          else
            Accept := IsIn;
          if pat^ = ']' then
          begin
            Inc(pat);
          end;
          if not Accept then
            Exit;
        end;
      '\':
        begin
          ACharCode := Unescape(pat);
          if not IsMatch(ACharCode, ACharCode) then
            Exit
          else
            Inc(S);
        end;
      '*', '%':
        begin
          Inc(pat);
          // 匹配任意长度的任意字符
          if pat^ = #0 then
          begin
            Result := os;
            while S^ <> #0 do
              Inc(S);
            Exit;
          end
          else
          begin
            // 多个连续的*或%号视为
            while (pat^ = '%') or (pat^ = '*') do
              Inc(pat);
            ps := pat;
            // 找到下一个任意匹配规则边界
            while (pat^ <> #0) and (pat^ <> '*') do
              Inc(pat);
            // 匹配子串及剩余部分
            AToken := StrDupX(ps, (IntPtr(pat) - IntPtr(ps)) shr 1);
            repeat
              pt := S;
              ps := StrLikeX(S, PQCharW(AToken), AIgnoreCase);
              if ps <> nil then
              begin
                if (pat^ <> #0) and (StrLikeX(S, pat, AIgnoreCase) = nil) then
                begin
                  Inc(pt);
                  S := pt;
                end
                else
                begin
                  Result := os;
                  while S^ <> #0 do
                    Inc(S);
                  Exit;
                end;
              end
              else
              begin
                Inc(pt);
                S := pt;
              end;
            until (S^ = #0);
            // 如果上面没有匹配到，说明失败了
            Exit;
          end;
        end
    else // 普通字符的比较
      begin
        if not IsMatch(Ord(pat^), Ord(pat^)) then
          Exit;
        Inc(S);
        Inc(pat);
      end;
    end;
  end;
  if (pat^ = '%') or (pat^ = '*') then // 模式匹配
    Inc(pat);
  if pat^ = #0 then
    Result := os;
end;

function StrLikeW(S, pat: PQCharW; AIgnoreCase: Boolean): Boolean;
var
  ps: PQCharW;
begin
  ps := S;
  Result := (StrLikeX(S, pat, AIgnoreCase) = ps) and (S^ = #0);
end;

{ TQPagedList }

function TQPagedList.Add(const p: Pointer): Integer;
begin
  Result := FCount;
  Insert(Result, p);
end;
{$IF RTLVersion<26}

procedure TQPagedList.Assign(ListA: TList; AOperator: TListAssignOp;
  ListB: TList);
var
  I: Integer;
  LTemp: TQPagedList;
  LSource: TList;
begin
  // ListB given?
  if ListB <> nil then
  begin
    LSource := ListB;
    Assign(ListA);
  end
  else
    LSource := ListA;

  // on with the show
  case AOperator of

    // 12345, 346 = 346 : only those in the new list
    laCopy:
      begin
        Clear;
        for I := 0 to LSource.count - 1 do
          Add(LSource[I]);
      end;

    // 12345, 346 = 34 : intersection of the two lists
    laAnd:
      for I := count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) = -1 then
          Delete(I);

    // 12345, 346 = 123456 : union of the two lists
    laOr:
      for I := 0 to LSource.count - 1 do
        if IndexOf(LSource[I]) = -1 then
          Add(LSource[I]);

    // 12345, 346 = 1256 : only those not in both lists
    laXor:
      begin
        LTemp := TQPagedList.Create; // Temp holder of 4 byte values
        try
          for I := 0 to LSource.count - 1 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          for I := count - 1 downto 0 do
            if LSource.IndexOf(Items[I]) <> -1 then
              Delete(I);
          I := count + LTemp.count;
          if Capacity < I then
            Capacity := I;
          for I := 0 to LTemp.count - 1 do
            Add(LTemp[I]);
        finally
          LTemp.Free;
        end;
      end;

    // 12345, 346 = 125 : only those unique to source
    laSrcUnique:
      for I := count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) <> -1 then
          Delete(I);

    // 12345, 346 = 6 : only those unique to dest
    laDestUnique:
      begin
        LTemp := TQPagedList.Create;
        try
          for I := LSource.count - 1 downto 0 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          Assign(LTemp);
        finally
          LTemp.Free;
        end;
      end;
  end;
end;
{$IFEND}

procedure TQPagedList.BatchAdd(pp: PPointer; ACount: Integer);
begin
  BatchInsert(FCount, pp, ACount);
end;

procedure TQPagedList.BatchAdd(AList: TPointerList);
begin
  if Length(AList) > 0 then
    BatchInsert(FCount, @AList[0], Length(AList));
end;

procedure TQPagedList.Assign(ListA: TQPagedList; AOperator: TListAssignOp;
  ListB: TQPagedList);
var
  I: Integer;
  LTemp, LSource: TQPagedList;
begin
  // ListB given?
  if ListB <> nil then
  begin
    LSource := ListB;
    Assign(ListA);
  end
  else
    LSource := ListA;
  case AOperator of
    // 12345, 346 = 346 : only those in the new list
    laCopy:
      begin
        Clear;
        for I := 0 to LSource.count - 1 do
          Add(LSource[I]);
      end;
    // 12345, 346 = 34 : intersection of the two lists
    laAnd:
      for I := count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) = -1 then
          Delete(I);
    // 12345, 346 = 123456 : union of the two lists
    laOr:
      for I := 0 to LSource.count - 1 do
        if IndexOf(LSource[I]) = -1 then
          Add(LSource[I]);
    // 12345, 346 = 1256 : only those not in both lists
    laXor:
      begin
        LTemp := TQPagedList.Create; // Temp holder of 4 byte values
        try
          for I := 0 to LSource.count - 1 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          for I := count - 1 downto 0 do
            if LSource.IndexOf(Items[I]) <> -1 then
              Delete(I);
          I := count + LTemp.count;
          if Capacity < I then
            Capacity := I;
          for I := 0 to LTemp.count - 1 do
            Add(LTemp[I]);
        finally
          LTemp.Free;
        end;
      end;

    // 12345, 346 = 125 : only those unique to source
    laSrcUnique:
      for I := count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) <> -1 then
          Delete(I);

    // 12345, 346 = 6 : only those unique to dest
    laDestUnique:
      begin
        LTemp := TQPagedList.Create;
        try
          for I := LSource.count - 1 downto 0 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          Assign(LTemp);
        finally
          LTemp.Free;
        end;
      end;
  end;
end;

procedure TQPagedList.CheckLastPage;
begin
  while (FLastUsedPage > 0) and (FPages[FLastUsedPage].FUsedCount = 0) do
    Dec(FLastUsedPage);
end;

procedure TQPagedList.Clear;
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to High(FPages) do
  begin
    for J := 0 to FPages[I].FUsedCount - 1 do
      DoDelete(FPages[I].FItems[J]);
    FPages[I].FUsedCount := 0;
  end;
  FFirstDirtyPage := 1;
  if Length(FPages) > 0 then
  begin
    FLastUsedPage := 0;
    FPages[0].FUsedCount := 0;
  end
  else
    FLastUsedPage := -1;
  FCount := 0;
end;

procedure TQPagedList.Pack;
var
  ASource, ADest, AStartMove, AToMove, APageCount: Integer;
  ADestPage, ASourcePage: TQListPage;
  procedure PackPages(AStartPage: Integer);
  var
    I: Integer;
  begin
    if AStartPage < APageCount then
    begin
      I := AStartPage;
      while I < APageCount do
      begin
        FreeAndNil(FPages[I]);
        Inc(I);
      end;
      SetLength(FPages, AStartPage);
      FLastUsedPage := AStartPage - 1;
      FFirstDirtyPage := AStartPage + 1;
    end;
  end;

  procedure NextDest;
  var
    APriorPage: TQListPage;
    ANewDest: Integer;
  begin
    ANewDest := ADest;
    repeat
      ADestPage := FPages[ANewDest];
      if ADestPage.FUsedCount = FPageSize then
        Inc(ANewDest)
      else
      begin
        if (ADest <> ANewDest) then
        begin
          ADest := ANewDest;
          if ASource <> ADest then
          begin
            APriorPage := FPages[ADest - 1];
            ADestPage.FStartIndex := APriorPage.FStartIndex +
              APriorPage.FUsedCount;
          end;
        end;
        Break;
      end;
    until ADest = ASource;
  end;

  function NextSource: Boolean;
  begin
    Inc(ASource);
    Result := false;
    while ASource <= FLastUsedPage do
    begin
      ASourcePage := FPages[ASource];
      if (ASourcePage.FUsedCount > 0) then
      begin
        Result := True;
        Break;
      end
      else
        Inc(ASource);
    end;
  end;

  procedure CleanPages;
  var
    I: Integer;
  begin
    I := FFirstDirtyPage;
    while I < APageCount do
    begin
      FPages[I].FStartIndex := FPages[I - 1].FStartIndex + FPages[I - 1]
        .FUsedCount;
      Inc(I);
    end;
  end;

begin
  APageCount := Length(FPages);
  if count > 0 then
  begin
    ADest := 0;
    ASource := 0;
    CleanPages;
    while NextSource do
    begin
      AStartMove := 0;
      NextDest;
      if ADestPage <> ASourcePage then
      begin
        while (ADestPage <> ASourcePage) do
        begin
          AToMove := ASourcePage.FUsedCount - AStartMove;
          if AToMove > FPageSize - ADestPage.FUsedCount then
            AToMove := FPageSize - ADestPage.FUsedCount;
          System.Move(ASourcePage.FItems[AStartMove],
            ADestPage.FItems[ADestPage.FUsedCount], AToMove * SizeOf(Pointer));
          Inc(AStartMove, AToMove);
          Inc(ADestPage.FUsedCount, AToMove);
          if ASourcePage.FUsedCount = AStartMove then
          begin
            ASourcePage.FStartIndex := ADestPage.FStartIndex +
              ADestPage.FUsedCount;
            ASourcePage.FUsedCount := 0;
            Break;
          end;
          if ADestPage.FUsedCount = FPageSize then
          begin
            System.Move(ASourcePage.FItems[AStartMove], ASourcePage.FItems[0],
              (ASourcePage.FUsedCount - AStartMove) * SizeOf(Pointer));
            Dec(ASourcePage.FUsedCount, AStartMove);
            Inc(ASourcePage.FStartIndex, AStartMove);
            AStartMove := 0;
            NextDest;
          end;
        end;
      end;
    end;
    if ADestPage.FUsedCount = 0 then
      PackPages(ADest)
    else
      PackPages(ADest + 1);
  end
  else
    PackPages(0);
end;

constructor TQPagedList.Create(APageSize: Integer);
begin
  inherited Create;
  if APageSize <= 0 then
    APageSize := 512;
  FPageSize := APageSize;
  FLastUsedPage := -1;
  FFirstDirtyPage := 1;
end;

constructor TQPagedList.Create;
begin
  Create(512);
end;

procedure TQPagedList.Delete(AIndex: Integer);
var
  APage: Integer;
  ATemp: TQListPage;
begin
  APage := FindPage(AIndex);
  if APage >= 0 then
  begin
    ATemp := FPages[APage];
    Dec(AIndex, ATemp.FStartIndex);
    DoDelete(ATemp.FItems[AIndex]);
    System.Move(ATemp.FItems[AIndex + 1], ATemp.FItems[AIndex],
      SizeOf(Pointer) * (ATemp.FUsedCount - AIndex - 1));
    Dec(ATemp.FUsedCount);
    CheckLastPage;
    Dec(FCount);
    Dirty(APage + 1);
  end;
end;

destructor TQPagedList.Destroy;
var
  I: Integer;
begin
  Clear;
  for I := 0 to High(FPages) do
    FreeObject(FPages[I]);
{$IFDEF UNICODE}
  if (TMethod(FOnCompare).Code <> nil) and
    (TMethod(FOnCompare).Data = Pointer(-1)) then
    TQPagedListSortCompareA(TMethod(FOnCompare).Code) := nil;
{$ENDIF}
  inherited;
end;

procedure TQPagedList.Dirty(APage: Integer);
begin
  if APage < FFirstDirtyPage then
    FFirstDirtyPage := APage;
end;

function TQPagedList.DoCompare(p1, p2: Pointer): Integer;
begin
  case IntPtr(TMethod(FOnCompare).Data) of
    0: // 全局函数
      TQPagedListSortCompareG(TMethod(FOnCompare).Code)(p1, p2, Result);
{$IFDEF UNICODE}
    -1: // 匿名函数
      TQPagedListSortCompareA(TMethod(FOnCompare).Code)(p1, p2, Result)
{$ENDIF}
  else
    FOnCompare(p1, p2, Result);
  end;
end;

procedure TQPagedList.DoDelete(const p: Pointer);
begin
  if (p <> nil) and (ClassType <> TQPagedList) then
    Notify(p, lnDeleted);
end;

procedure TQPagedList.Exchange(AIndex1, AIndex2: Integer);
var
  p1, p2: TQListPage;
  T: Pointer;
begin
  p1 := GetPage(AIndex1);
  p2 := GetPage(AIndex2);
  if (p1 <> nil) and (p2 <> nil) then
  begin
    Dec(AIndex1, p1.FStartIndex);
    Dec(AIndex2, p2.FStartIndex);
    T := p1.FItems[AIndex1];
    p1.FItems[AIndex1] := p2.FItems[AIndex2];
    p2.FItems[AIndex2] := T;
  end;
end;

function TQPagedList.Expand: TQPagedList;
begin
  // 这个函数只是为兼容TList接口保留，TQPagedList不需要
  Result := Self;
end;

function TQPagedList.Extract(Item: Pointer): Pointer;
begin
  Result := ExtractItem(Item, FromBeginning);
end;

function TQPagedList.ExtractItem(Item: Pointer; Direction: TDirection): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOfItem(Item, Direction);
  if I >= 0 then
  begin
    Result := Item;
    Remove(I);
    if ClassType <> TQPagedList then
      Notify(Result, lnExtracted);
  end;
end;

function TQPagedList.Find(const p: Pointer; var AIdx: Integer): Boolean;
var
  l, H, I, c: Integer;
begin
  Result := false;
  l := 0;
  H := FCount - 1;
  while l <= H do
  begin
    I := (l + H) shr 1;
    c := DoCompare(Items[I], p);
    if c < 0 then
      l := I + 1
    else
    begin
      H := I - 1;
      if c = 0 then
        Result := True;
    end;
  end;
  AIdx := l;
end;

function TQPagedList.FindPage(AIndex: Integer): Integer;
var
  l, H, I, AMax, c: Integer;
  ATemp: TQListPage;
begin
  c := Length(FPages);
  ATemp := FPages[FFirstDirtyPage - 1];
  if (FFirstDirtyPage < c) and (AIndex >= ATemp.FStartIndex + ATemp.FUsedCount)
  then
  begin
    I := FFirstDirtyPage;
    while I < c do
    begin
      ATemp := FPages[I - 1];
      FPages[I].FStartIndex := ATemp.FStartIndex + ATemp.FUsedCount;
      if FPages[I].FStartIndex > AIndex then
      begin
        Result := I - 1;
        FFirstDirtyPage := I + 1;
        Exit;
      end
      else if FPages[I].FStartIndex = AIndex then
      begin
        Result := I;
        FFirstDirtyPage := I + 1;
        Exit;
      end;
      Inc(I);
    end;
    H := c - 1;
  end
  else
    H := FFirstDirtyPage - 1;
  l := AIndex div FPageSize;
  while l <= H do
  begin
    I := (l + H) shr 1;
    ATemp := FPages[I];
    AMax := ATemp.FStartIndex + ATemp.FUsedCount - 1; // 最大的索引号
    if AIndex > AMax then
      l := I + 1
    else
    begin
      H := I - 1;
      if (AIndex >= ATemp.FStartIndex) and (AIndex <= AMax) then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
  Result := -1;
end;

function TQPagedList.First: Pointer;
begin
  Result := Items[0];
end;

function TQPagedList.GetCapacity: Integer;
begin
  Result := Length(FPages) * FPageSize;
end;

function TQPagedList.GetEnumerator: TQPagedListEnumerator;
begin
  Result := TQPagedListEnumerator.Create(Self);
end;

function TQPagedList.GetItems(AIndex: Integer): Pointer;
var
  APage: TQListPage;
begin
  APage := GetPage(AIndex);
  if APage <> nil then
  begin
    Dec(AIndex, APage.FStartIndex);
    Result := APage.FItems[AIndex];
  end
  else
    raise Exception.Create('索引越界:' + IntToStr(AIndex));
end;

function TQPagedList.GetList: TPointerList;
var
  I, K: Integer;
  APage: TQListPage;
begin
  SetLength(Result, count);
  K := 0;
  for I := 0 to High(FPages) do
  begin
    APage := FPages[I];
    if APage.FUsedCount > 0 then
    begin
      System.Move(APage.FItems[0], Result[K],
        APage.FUsedCount * SizeOf(Pointer));
      Inc(K, APage.FUsedCount);
    end;
  end;
end;

function TQPagedList.GetPage(AIndex: Integer): TQListPage;
var
  l, H, I, AMax, c: Integer;
  ATemp: TQListPage;
begin
  Result := nil;
  c := Length(FPages);
  ATemp := FPages[FFirstDirtyPage - 1];
  if (FFirstDirtyPage < c) and (AIndex >= ATemp.FStartIndex + ATemp.FUsedCount)
  then
  begin
    I := FFirstDirtyPage;
    while I < c do
    begin
      ATemp := FPages[I - 1];
      FPages[I].FStartIndex := ATemp.FStartIndex + ATemp.FUsedCount;
      if FPages[I].FStartIndex > AIndex then
      begin
        Result := ATemp;
        FFirstDirtyPage := I + 1;
        Exit;
      end
      else if FPages[I].FStartIndex = AIndex then
      begin
        Result := FPages[I];
        FFirstDirtyPage := I + 1;
        Exit;
      end;
      Inc(I);
    end;
    H := c - 1;
  end
  else
    H := FFirstDirtyPage - 1;
  // 最坏的情况来说，假设每页都填满，最低索引位置为AIndex div Page
  l := AIndex div FPageSize;
  while l <= H do
  begin
    I := (l + H) shr 1;
    ATemp := FPages[I];
    AMax := ATemp.FStartIndex + ATemp.FUsedCount - 1;
    // 最大的索引号
    if AIndex > AMax then
      l := I + 1
    else
    begin
      H := I - 1;
      if (AIndex >= ATemp.FStartIndex) and (AIndex <= AMax) then
      begin
        Result := ATemp;
        Exit;
      end;
    end;
  end;
end;

function TQPagedList.GetPageCount: Integer;
begin
  Result := Length(FPages);
end;

function TQPagedList.IndexOf(Item: Pointer): Integer;
var
  I, J: Integer;
begin
  if TMethod(FOnCompare).Code <> nil then
  begin
    if not Find(Item, Result) then
      Result := -1;
  end
  else
  begin
    Result := -1;
    for I := 0 to High(FPages) do
    begin
      for J := 0 to FPages[I].FUsedCount do
      begin
        if FPages[I].FItems[J] = Item then
        begin
          Result := FPages[I].FStartIndex + J;
          Exit;
        end;
      end;
    end;
  end;
end;

function TQPagedList.IndexOfItem(Item: Pointer; Direction: TDirection): Integer;
var
  I, J: Integer;
begin
  if Direction = FromBeginning then
    Result := IndexOf(Item)
  else
  begin
    Result := -1;
    for I := High(FPages) downto 0 do
    begin
      for J := FPages[I].FUsedCount - 1 downto 0 do
      begin
        if FPages[I].FItems[J] = Item then
        begin
          Result := FPages[I].FStartIndex + J;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TQPagedList.BatchInsert(AIndex: Integer; pp: PPointer;
  ACount: Integer);
var
  APage, ANeedPages, APageCount, AOffset, ARemain, AToMove: Integer;
  ASourcePage, ADestPage: TQListPage;
  ASplitNeeded: Boolean;
  procedure SplitPage;
  begin
    ADestPage := FPages[APage];
    ADestPage.FStartIndex := ASourcePage.FStartIndex;
    ADestPage.FUsedCount := AIndex - ASourcePage.FStartIndex;
    System.Move(ASourcePage.FItems[0], ADestPage.FItems[0],
      ADestPage.FUsedCount * SizeOf(Pointer));
    Dec(ASourcePage.FUsedCount, ADestPage.FUsedCount);
    System.Move(ASourcePage.FItems[ADestPage.FUsedCount], ASourcePage.FItems[0],
      ASourcePage.FUsedCount * SizeOf(Pointer));
    Inc(APage);
    Dec(ANeedPages);
  end;
  procedure NewPages;
  var
    ATempList: TPointerList;
    AEmptyPages: Integer;
  begin
    APageCount := Length(FPages);
    // 计算需要的页数
    ANeedPages := ACount div FPageSize;
    if (ACount mod FPageSize) <> 0 then
      Inc(ANeedPages);
    AEmptyPages := APageCount;
    if FLastUsedPage >= 0 then
    begin
      if FPages[FLastUsedPage].FUsedCount = 0 then
        Dec(FLastUsedPage);
      Dec(AEmptyPages, FLastUsedPage + 1);
    end;
    if AIndex = 0 then
      APage := 0
    else if AIndex = FCount then
      APage := FLastUsedPage + 1
    else
    begin
      APage := FindPage(AIndex);
      ASourcePage := FPages[APage];
      ASplitNeeded := AIndex > ASourcePage.FStartIndex;
      if ASplitNeeded then
        // 如果插入的位置是页中间，需要额外一页来存贮前面的元素
        Inc(ANeedPages);
    end;
    if AEmptyPages >= ANeedPages then // 空闲的页足够的话
    begin
      if FCount = 0 then // 没有任何记录，此时AIndex=0,直接预留页就可以了
      begin
        FLastUsedPage := ANeedPages - 1;
        FFirstDirtyPage := ANeedPages;
      end
      else
      begin
        SetLength(ATempList, ANeedPages);
        System.Move(ATempList[0], FPages[APage], ANeedPages * SizeOf(Pointer));
        System.Move(FPages[APage], FPages[APage + ANeedPages],
          (FLastUsedPage - APage + 1) * SizeOf(Pointer));
        System.Move(ATempList[0], FPages[APage], ANeedPages * SizeOf(Pointer));
        if ASplitNeeded then
          SplitPage;
        Inc(FLastUsedPage, ANeedPages);
      end;
      Exit;
    end
    else // 空闲页不足
    begin
      SetLength(FPages, APageCount + ANeedPages - AEmptyPages);
      if FLastUsedPage >= APage then
      begin
        SetLength(ATempList, AEmptyPages);
        if AEmptyPages > 0 then
          System.Move(FPages[FLastUsedPage + 1], ATempList[0],
            AEmptyPages * SizeOf(Pointer));
        System.Move(FPages[APage], FPages[APage + ANeedPages],
          (FLastUsedPage - APage + 1) * SizeOf(Pointer));
        if AEmptyPages > 0 then
          System.Move(ATempList[0], FPages[APage],
            AEmptyPages * SizeOf(Pointer));
      end;
      AOffset := APage + AEmptyPages;
      AToMove := ANeedPages - AEmptyPages;
      while AToMove > 0 do
      begin
        FPages[AOffset] := TQListPage.Create(FPageSize);
        Inc(AOffset);
        Dec(AToMove);
      end;
      if ASplitNeeded then
        SplitPage;
      FLastUsedPage := High(FPages);
    end;
  end;

begin
  if AIndex < 0 then
    AIndex := 0
  else if AIndex > FCount then
    AIndex := FCount;
  NewPages;
  ARemain := ACount;
  while ARemain > 0 do
  begin
    ADestPage := FPages[APage];
    ADestPage.FStartIndex := AIndex;
    AToMove := ARemain;
    if AToMove >= FPageSize then
      AToMove := FPageSize;
    System.Move(pp^, ADestPage.FItems[0], AToMove * SizeOf(Pointer));
    ADestPage.FUsedCount := AToMove;
    Inc(pp, AToMove);
    Inc(AIndex, AToMove);
    Dec(ARemain, AToMove);
    Inc(APage);
  end;
  Inc(FCount, ACount);
  Dirty(APage + ANeedPages);
end;

procedure TQPagedList.BatchInsert(AIndex: Integer; const AList: TPointerList);
begin
  if Length(AList) > 0 then
    BatchInsert(AIndex, @AList[0], Length(AList));
end;

procedure TQPagedList.Insert(AIndex: Integer; const p: Pointer);
var
  APage, ANewPage, AMoved: Integer;
  ADestPage, ATemp: TQListPage;
begin
  if AIndex < 0 then
    AIndex := 0;
  if TMethod(FOnCompare).Code <> nil then
    Find(p, AIndex);
  if AIndex >= count then // 插入末尾
  begin
    APage := FLastUsedPage;
    if (APage < 0) or (FPages[APage].FUsedCount = FPageSize) then
    begin
      Inc(APage);
      if APage >= Length(FPages) then
      begin
        SetLength(FPages, Length(FPages) + 1);
        ADestPage := TQListPage.Create(FPageSize);
        ADestPage.FStartIndex := count;
        FPages[APage] := ADestPage;
      end
      else
        ADestPage := FPages[APage];
      Inc(FLastUsedPage);
      if APage = 0 then
        FFirstDirtyPage := 1;
    end
    else
      ADestPage := FPages[APage];
    ADestPage.FItems[ADestPage.FUsedCount] := p;
    Inc(ADestPage.FUsedCount);
  end
  else if AIndex <= 0 then // 插入最前面
  begin
    ADestPage := FPages[0];
    if ADestPage.FUsedCount < FPageSize then
    begin
      System.Move(ADestPage.FItems[0], ADestPage.FItems[1],
        ADestPage.FUsedCount * SizeOf(Pointer));
      ADestPage.FItems[0] := p;
      Inc(ADestPage.FUsedCount);
    end
    else // 当前页满了，需要分配新页
    begin
      if FLastUsedPage < High(FPages) then
      begin
        Inc(FLastUsedPage);
        ADestPage := FPages[FLastUsedPage];
        System.Move(FPages[0], FPages[1], SizeOf(TQListPage) * FLastUsedPage);
        FPages[0] := ADestPage;
        ADestPage.FStartIndex := 0;
      end
      else
      begin
        ANewPage := Length(FPages);
        SetLength(FPages, ANewPage + 1);
        FLastUsedPage := ANewPage;
        System.Move(FPages[0], FPages[1], SizeOf(TQListPage) * FLastUsedPage);
        FPages[0] := TQListPage.Create(FPageSize);
        ADestPage := FPages[0];
      end;
      ADestPage.FUsedCount := 1;
      ADestPage.FItems[0] := p;
    end;
    Dirty(1);
  end
  else
  // 插入中间
  begin
    APage := FindPage(AIndex);
    ADestPage := FPages[APage];
    if (ADestPage.FUsedCount = FPageSize) then // 目标页已满
    begin
      ANewPage := APage + 1;
      if (FLastUsedPage = APage) or (FPages[ANewPage].FUsedCount = FPageSize)
      then
      // 下一页也满了
      begin
        Inc(FLastUsedPage);
        if FLastUsedPage = Length(FPages) then
        begin
          SetLength(FPages, FLastUsedPage + 1);
          System.Move(FPages[ANewPage], FPages[ANewPage + 1],
            SizeOf(TQListPage) * (FLastUsedPage - ANewPage));
          ATemp := TQListPage.Create(FPageSize);;
          FPages[ANewPage] := ATemp;
        end
        else if ANewPage = FLastUsedPage then
          ATemp := FPages[ANewPage]
        else
        begin
          ATemp := FPages[FLastUsedPage];
          System.Move(FPages[ANewPage], FPages[ANewPage + 1],
            SizeOf(TQListPage) * (FLastUsedPage - ANewPage));
          FPages[ANewPage] := ATemp;
        end;
        ATemp.FStartIndex := AIndex + 1;
        Dec(AIndex, ADestPage.FStartIndex);
        AMoved := ADestPage.FUsedCount - AIndex;
        System.Move(ADestPage.FItems[AIndex], ATemp.FItems[0],
          AMoved * SizeOf(Pointer));
        Dec(ADestPage.FUsedCount, AMoved - 1);
        ATemp.FUsedCount := AMoved;
        ADestPage.FItems[AIndex] := p;
        Dirty(ANewPage + 1);
      end
      else // 将当前页的内容移入下一页
      begin
        ATemp := FPages[ANewPage];
        System.Move(ATemp.FItems[0], ATemp.FItems[1],
          ATemp.FUsedCount * SizeOf(Pointer));
        ATemp.FItems[0] := ADestPage.FItems[FPageSize - 1];
        Inc(ATemp.FUsedCount);
        Dirty(ANewPage);
        Dec(AIndex, ADestPage.FStartIndex);
        AMoved := ADestPage.FUsedCount - AIndex - 1;
        System.Move(ADestPage.FItems[AIndex], ADestPage.FItems[AIndex + 1],
          AMoved * SizeOf(Pointer));
        ADestPage.FItems[AIndex] := p;
      end;
    end
    else
    begin
      Dec(AIndex, ADestPage.FStartIndex);
      if AIndex < ADestPage.FUsedCount then
      begin
        AMoved := (ADestPage.FUsedCount - AIndex);
        System.Move(ADestPage.FItems[AIndex], ADestPage.FItems[AIndex + 1],
          AMoved * SizeOf(TQListPage));
      end;
      ADestPage.FItems[AIndex] := p;
      Inc(ADestPage.FUsedCount);
      Dirty(APage + 1);
    end;
  end;
  Inc(FCount);
  if (p <> nil) and (ClassType <> TQPagedList) then
    Notify(p, lnAdded);
end;

function TQPagedList.Last: Pointer;
begin
  Result := Items[count - 1];
end;

procedure TQPagedList.Move(AFrom, ATo: Integer);
begin
  MoveTo(AFrom, ATo);
end;

procedure TQPagedList.MoveTo(AFrom, ATo: Integer);
var
  ATemp: Pointer;
begin
  if AFrom <> ATo then
  begin
    ATemp := Items[AFrom];
    Remove(AFrom);
    Insert(ATo, ATemp);
  end;
end;

procedure TQPagedList.Notify(Ptr: Pointer; Action: TListNotification);
begin

end;

function TQPagedList.Remove(Item: Pointer): Integer;
begin
  Result := RemoveItem(Item, FromBeginning);
end;

procedure TQPagedList.Remove(AIndex: Integer);
var
  APage: Integer;
begin
  APage := FindPage(AIndex);
  if APage >= 0 then
  begin
    Dec(AIndex, FPages[APage].FStartIndex);
    System.Move(FPages[APage].FItems[AIndex + 1], FPages[APage].FItems[AIndex],
      SizeOf(Pointer) * (FPages[APage].FUsedCount - AIndex - 1));
    Dec(FPages[APage].FUsedCount);
    CheckLastPage;
    Assert(FPages[APage].FUsedCount >= 0);
    Dirty(APage + 1);
  end;
end;
{$IFDEF UNICODE}

procedure TQPagedList.Sort(AOnCompare: TQPagedListSortCompareA);
begin
  TMethod(FOnCompare).Code := nil;
  PQPagedListSortCompareA(@TMethod(FOnCompare).Code)^ := AOnCompare;
  TMethod(FOnCompare).Data := Pointer(-1);
  Sort;
end;
{$ENDIF}

procedure TQPagedList.SetCapacity(const Value: Integer);
begin
  // 仅为兼容保留，实际不做任何事情
end;

procedure TQPagedList.SetItems(AIndex: Integer; const Value: Pointer);
var
  APage: TQListPage;
begin
  APage := GetPage(AIndex);
  if APage <> nil then
  begin
    Dec(AIndex, APage.FStartIndex);
    APage.FItems[AIndex] := Value;
  end
  else
    raise Exception.Create('索引越界:' + IntToStr(AIndex));
end;

procedure TQPagedList.SetOnCompare(const Value: TQPagedListSortCompare);
begin
  if (TMethod(FOnCompare).Code <> TMethod(Value).Code) or
    (TMethod(FOnCompare).Data <> TMethod(Value).Data) then
  begin
    FOnCompare := Value;
    if Assigned(Value) then
      Sort;
  end;
end;

procedure TQPagedList.Sort(AOnCompare: TQPagedListSortCompareG);
begin
  TMethod(FOnCompare).Code := @AOnCompare;
  TMethod(FOnCompare).Data := nil;
  Sort;
end;

procedure TQPagedList.Sort;
  procedure QuickSort(l, R: Integer);
  var
    I, J, p: Integer;
  begin
    repeat
      I := l;
      J := R;
      p := (l + R) shr 1;
      repeat
        while DoCompare(Items[I], Items[p]) < 0 do
          Inc(I);
        while DoCompare(Items[J], Items[p]) > 0 do
          Dec(J);
        if I <= J then
        begin
          if I <> J then
            Exchange(I, J);
          if p = I then
            p := J
          else if p = J then
            p := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if l < J then
        QuickSort(l, J);
      l := I;
    until I >= R;
  end;

begin
  if TMethod(FOnCompare).Code = nil then
    raise Exception.Create('未指定排序规则');
  if count > 0 then
    QuickSort(0, count - 1);
end;

function TQPagedList.RemoveItem(Item: Pointer; Direction: TDirection): Integer;
begin
  Result := IndexOfItem(Item, Direction);
  if Result > 0 then
    Remove(Result);
end;

{ TQListPage }

constructor TQListPage.Create(APageSize: Integer);
begin
  SetLength(FItems, APageSize);
  // OutputDebugString(PChar(IntToHex(IntPtr(Self), 8) + ' Created'));
end;

destructor TQListPage.Destroy;
begin
  // OutputDebugString(PChar(IntToHex(IntPtr(Self), 8) + ' Freed'));
  inherited;
end;

{ TQPagedListEnumerator }

constructor TQPagedListEnumerator.Create(AList: TQPagedList);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TQPagedListEnumerator.GetCurrent: Pointer;
begin
  Result := FList[FIndex];
end;

function TQPagedListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.count - 1;
  if Result then
    Inc(FIndex);
end;

{ TQPagedStream }
constructor TQPagedStream.Create;
begin
  Create(8192);
end;

function TQPagedStream.ActiveOffset: Integer;
begin
  Result := FPosition mod FPageSize;
end;

function TQPagedStream.ActivePage: Integer;
begin
  Result := FPosition div FPageSize;
end;

procedure TQPagedStream.Clear;
var
  I: Integer;
begin
  for I := 0 to High(FPages) do
    FreeMem(FPages[I]);
  SetLength(FPages, 0);
  FSize := 0;
  FPosition := 0;
end;

constructor TQPagedStream.Create(APageSize: Integer);
begin
  inherited Create;
  if APageSize <= 0 then
    APageSize := 8192;
  FPageSize := APageSize;
end;

destructor TQPagedStream.Destroy;
begin
  Clear;
  inherited;
end;

function TQPagedStream.GetAsBytes: TBytes;
begin
  if Size > 0 then
  begin
    SetLength(Result, FSize);
    FPosition := 0;
    Read(Result[0], FSize);
  end
  else
    SetLength(Result, 0);
end;

function TQPagedStream.GetBytes(AIndex: Int64): Byte;
begin
  if AIndex + 1 > FSize then
    Result := 0
  else
    Result := PByte(IntPtr(FPages[AIndex div FPageSize]) +
      (AIndex mod FPageSize))^;
end;

function TQPagedStream.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TQPagedStream.LoadFromFile(const FileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(AStream);
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TQPagedStream.LoadFromStream(Stream: TStream);
var
  ACount: Int64;
begin
  ACount := Stream.Size - Stream.Position;
  Capacity := ACount;
  CopyFrom(Stream, ACount);
end;

procedure TQPagedStream.PageNeeded(APageIndex: Integer);
begin
  if High(FPages) < APageIndex then
    Capacity := (APageIndex + 1) * FPageSize - 1;
end;

function TQPagedStream.Read(var Buffer; count: Longint): Longint;
var
  ACanRead: Int64;
  pBuf: PByte;
  APage, APageSpace, APageOffset, AToRead: Integer;
begin
  ACanRead := FSize - FPosition;
  Result := 0;
  if ACanRead >= count then
  begin
    if ACanRead < count then
      count := ACanRead;
    pBuf := @Buffer;
    while count > 0 do
    begin
      APage := ActivePage;
      APageOffset := ActiveOffset;
      APageSpace := FPageSize - ActiveOffset;
      if count > APageSpace then
        AToRead := APageSpace
      else
        AToRead := count;
      Dec(count, AToRead);
      Move(PByte(IntPtr(FPages[APage]) + APageOffset)^, pBuf^, AToRead);
      Inc(pBuf, AToRead);
      Inc(Result, AToRead);
      Inc(FPosition, AToRead);
    end;
  end;
end;

function TQPagedStream.Read(Buffer: TBytes; Offset, count: Longint): Longint;
begin
  if count > 0 then
    Result := Read(Buffer[Offset], count)
  else
    Result := 0;
end;

procedure TQPagedStream.SaveToFile(const FileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TQPagedStream.SaveToStream(Stream: TStream);
begin
  Stream.CopyFrom(Self, 0);
end;

function TQPagedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      Result := Offset;
    soCurrent:
      Result := FPosition + Offset;
    soEnd:
      Result := FSize - Offset
  else
    Result := 0;
  end;
  if Result > FSize then
    Result := FSize
  else if Result < 0 then
    Result := 0;
  FPosition := Result;
end;

procedure TQPagedStream.SetSize(const NewSize: Int64);
begin
  Capacity := NewSize;
end;

procedure TQPagedStream.SetAsBytes(const Value: TBytes);
begin
  Size := Length(Value);
  if Size > 0 then
    WriteBuffer(Value[0], Size);
end;

procedure TQPagedStream.SetBytes(AIndex: Int64; const Value: Byte);
begin
  if FSize < AIndex + 1 then
    Size := AIndex + 1;
  PByte(IntPtr(FPages[AIndex div FPageSize]) + (AIndex mod FPageSize))^
    := Value;
end;

procedure TQPagedStream.SetCapacity(Value: Int64);
var
  APageNum: Int64;
  I: Integer;
begin
  if Value < 0 then
    Value := 0;
  APageNum := (Value div FPageSize);
  if (Value mod FPageSize) <> 0 then
    Inc(APageNum);
  if FCapacity <> APageNum * FPageSize then
  begin
    FCapacity := APageNum * FPageSize;
    if Length(FPages) > APageNum then
    begin
      I := High(FPages);
      while I >= APageNum do
      begin
        FreeMem(FPages[I]);
        Dec(I);
      end;
      SetLength(FPages, APageNum);
    end
    else
    begin
      I := Length(FPages);
      SetLength(FPages, APageNum);
      while I < APageNum do
      begin
        GetMem(FPages[I], FPageSize);
        Inc(I);
      end;
    end;
  end;
end;

procedure TQPagedStream.SetSize(NewSize: Longint);
begin
  Capacity := NewSize;
end;

function TQPagedStream.Write(const Buffer: TBytes;
  Offset, count: Longint): Longint;
begin
  if count > 0 then
    Result := Write(Buffer[Offset], count)
  else
    Result := 0;
end;

function TQPagedStream.Write(const Buffer; count: Longint): Longint;
var
  ADest: PByte;
  APageIndex, APageOffset, APageSpace: Integer;
  AOffset: Int64;
  pBuf: PByte;
begin
  Result := 0;
  if count > 0 then
  begin
    AOffset := FPosition + count;
    PageNeeded(AOffset div FPageSize);
    APageIndex := ActivePage;
    APageOffset := ActiveOffset;
    APageSpace := FPageSize - APageOffset;
    pBuf := @Buffer;
    while count > 0 do
    begin
      ADest := PByte(IntPtr(FPages[APageIndex]) + APageOffset);
      if APageSpace < count then
      begin
        Move(pBuf^, ADest^, APageSpace);
        Inc(APageIndex);
        Dec(count, APageSpace);
        Inc(Result, APageSpace);
        Inc(pBuf, APageSpace);
        APageOffset := 0;
        APageSpace := FPageSize;
      end
      else
      begin
        Move(pBuf^, ADest^, count);
        Inc(Result, count);
        Break;
      end;
    end;
    Inc(FPosition, Result);
    if FSize < FPosition then
      FSize := FPosition;
  end;
end;

const
  PR_ORDERED = 9; // 如果存在顺序数列时，如12345，此时每个重复字符减小的权值
  PR_REPEAT = 5; // 如果存在重复的数列时，如aaaa，此时每次重复减少的权值
  PR_CHARTYPE = 20; // 每增加一个不同类型的字符时，增加的权值
  PR_LENGTH = 10; // 每增加一个字符时，增加的权值
  PR_CHART = 20; // 包含非数字和字母的控制字符时，额外增加的权值
  PR_UNICODE = 40; // 包含Unicode字符时，额外增加的权值

function PasswordScale(const S: QStringW): Integer;
var
  p: PQCharW;
  ARules: TPasswordRules;
  AMaxOrder, AMaxRepeat, ACharTypes: Integer;
  function RepeatCount: Integer;
  var
    T: PQCharW;
  begin
    T := p;
    Inc(T);
    Result := 0;
    while T^ = p^ do
    begin
      Inc(Result);
      Inc(T);
    end;
    if Result > AMaxRepeat then
      AMaxRepeat := Result;
  end;

  function OrderCount: Integer;
  var
    T, tl: PQCharW;
    AStep: Integer;
  begin
    T := p;
    tl := p;
    Inc(T);
    AStep := Ord(T^) - Ord(p^);
    Result := 0;
    while Ord(T^) - Ord(tl^) = AStep do
    begin
      Inc(Result);
      tl := T;
      Inc(T);
    end;
    if Result > AMaxOrder then
      AMaxOrder := Result;
  end;

begin
  if LowerCase(S) = 'password' then
    Result := 0
  else
  begin
    Result := Length(S) * PR_LENGTH;
    p := PQCharW(S);
    ARules := [];
    AMaxOrder := 0;
    AMaxRepeat := 0;
    while p^ <> #0 do
    begin
      if (p^ >= '0') and (p^ <= '9') then
        ARules := ARules + [prIncNumber]
      else if (p^ >= 'a') and (p^ <= 'z') then
        ARules := ARules + [prIncLowerCase]
      else if (p^ >= 'A') and (p^ <= 'Z') then
        ARules := ARules + [prIncUpperCase]
      else if p^ > #$7F then
        ARules := ARules + [prIncUnicode]
      else
        ARules := ARules + [prIncChart];
      if RepeatCount > 2 then
        ARules := ARules + [prRepeat];
      if OrderCount > 2 then
        ARules := ARules + [prSimpleOrder];
      Inc(p);
    end;
    if prSimpleOrder in ARules then
      Result := Result - AMaxOrder * PR_ORDERED;
    if prRepeat in ARules then
      Result := Result - AMaxRepeat * PR_REPEAT;
    ACharTypes := 0;
    if prIncNumber in ARules then
      Inc(ACharTypes);
    if prIncLowerCase in ARules then
      Inc(ACharTypes);
    if prIncUpperCase in ARules then
      Inc(ACharTypes);
    if prIncChart in ARules then
    begin
      Inc(ACharTypes);
      Result := Result + PR_CHART;
    end;
    if prIncUnicode in ARules then
    begin
      Inc(ACharTypes);
      Result := Result + PR_UNICODE;
    end;
    Result := Result + (ACharTypes - 1) * PR_CHARTYPE;
    // 密码强度的取值范围：<0
    if Result < 0 then
      Result := 0;
  end;
end;

function CheckPassword(const AScale: Integer): TPasswordStrongLevel; overload;
begin
  if AScale < 60 then
    Result := pslLowest
  else if AScale < 100 then
    Result := pslLower
  else if AScale < 150 then
    Result := pslNormal
  else if AScale < 200 then
    Result := pslHigher
  else
    Result := pslHighest;
end;

function CheckPassword(const S: QStringW): TPasswordStrongLevel; overload;
begin
  Result := CheckPassword(PasswordScale(S));
end;

function SimpleChineseToTraditional(S: QStringW): QStringW;
begin
{$IFDEF MSWINDOWS}
  SetLength(Result, Length(S) shl 1);
  SetLength(Result, LCMapStringW(2052, LCMAP_TRADITIONAL_CHINESE, PWideChar(S),
    Length(S), PWideChar(Result), Length(Result)));
{$ELSE}
  raise Exception.CreateFmt(SUnsupportNow, ['SimpleChineseToTraditional']);
{$ENDIF}
end;

function TraditionalChineseToSimple(S: QStringW): QStringW;
begin
{$IFDEF MSWINDOWS}
  SetLength(Result, Length(S) shl 1);
  SetLength(Result, LCMapStringW(2052, LCMAP_SIMPLIFIED_CHINESE, PWideChar(S),
    Length(S), PWideChar(Result), Length(Result)));
{$ELSE}
  raise Exception.CreateFmt(SUnsupportNow, ['TraditionalChineseToSimple']);
{$ENDIF}
end;

function MoneyRound(const AVal: Currency; ARoundMethod: TMoneyRoundMethod)
  : Currency;
var
  V, R: Int64;
begin
  if ARoundMethod = mrmNone then
  begin
    Result := AVal;
    Exit;
  end;
  V := PInt64(@AVal)^;
  R := V mod 100;
  if ARoundMethod = mrmSimple then
  begin
    if R >= 50 then
      PInt64(@Result)^ := ((V div 100) + 1) * 100
    else
      PInt64(@Result)^ := (V div 100) * 100;
  end
  else
  begin
    {
      四舍六入五考虑，
      五后非零就进一，
      五后皆零看奇偶，
      五前为偶应舍去，
      五前为奇要进一。 }
    if R > 50 then // 六入
      PInt64(@Result)^ := ((V div 100) + 1) * 100
    else if R = 50 then //
    begin
      if (((V div 100)) and $1) <> 0 then // 奇数
        PInt64(@Result)^ := ((V div 100) + 1) * 100
      else
        PInt64(@Result)^ := (V div 100) * 100;
    end
    else
      PInt64(@Result)^ := (V div 100) * 100;
  end;
end;
{
  将货币值转换为汉字大写
  Parameters
  AVal : 货币值
  AFlags : 标志位组合，以决定输出结果的格式
  ANegText : 当货币值为负数时，显示的前缀
  AStartText : 前导字符串，如“人民币：”
  AEndText : 后置字符串，如“整”
  AGroupNum : 组数，每个数字与其单位构成一个数组，AGroupNum指出要求的组数量，
  不为0时，会忽略标志位中的MC_HIDE_ZERO和MC_MERGE_ZERO
  ARoundMethod : 金额舍入到分时的算法
  AEndDigts : 小数点后的位数，-16~4 之间
  Returns
  返回格式化后的字符串
  Examples
  CapMoney(1.235,MC_READ,L"",L"",L"",0)=壹元贰角肆分
  CapMoney(1.235,MC_READ,L"",L"",L"",4)=零拾壹元贰角肆分
  CapMoney(100.24,MC_READ,L"",L"",L"",4)=壹佰元零贰角肆分
  CapMoney(-10012.235,MC_READ,L"负",L"￥",L"",0)=￥负壹万零壹拾贰元贰角肆分
  CapMoney(101005,MC_READ,L"负",L"",L"",0)=壹拾万壹仟零伍元
}

function CapMoney(AVal: Currency; AFlags: Integer;
  ANegText, AStartText, AEndText: QStringW; AGroupNum: Integer;
  ARoundMethod: TMoneyRoundMethod; AEndDigits: Integer = 2): QStringW;
const
  Nums: array [0 .. 9] of WideChar = (#$96F6 { 零 } , #$58F9 { 壹 } ,
    #$8D30 { 贰 } , #$53C1 { 叁 } , #$8086 { 肆 } , #$4F0D { 伍 } , #$9646 { 陆 } ,
    #$67D2 { 柒 } , #$634C { 捌 } , #$7396 { 玖 } );
  Units: array [0 .. 19] of WideChar = (#$94B1 { 钱 } , #$5398 { 厘 } ,
    #$5206 { 分 } , #$89D2 { 角 } , #$5706 { 圆 } , #$62FE { 拾 } , #$4F70 { 佰 } ,
    #$4EDF { 仟 } , #$4E07 { 万 } , #$62FE { 拾 } , #$4F70 { 佰 } , #$4EDF { 仟 } ,
    #$4EBF { 亿 } , #$62FE { 拾 } , #$4F70 { 佰 } , #$4EDF { 仟 } , #$4E07 { 万 } ,
    #$5146 { 兆 } , #$62FE { 拾 } , #$4F70 { 佰 } );
var
  R, V: Int64;
  I: Integer;
  ATemp: QStringW;
  pd, pe, p, pu: PWideChar;

begin
  AVal := MoneyRound(AVal, ARoundMethod);
  { -922,337,203,685,477.5808 ~ 922,337,203,685,477.5807 }
  V := PInt64(@AVal)^; // 精确到分
  if V < 0 then
    V := -V
  else
    SetLength(ANegText, 0);
  if (AFlags and MC_END_PATCH) <> 0 then // 如果要包含AEndText，则必需包含单位
    AFlags := AFlags or MC_UNIT;
  if AGroupNum > 0 then // 如果要分组
  begin
    AFlags := AFlags and (not(MC_MERGE_ZERO or MC_HIDE_ZERO));
    if AGroupNum > 20 then
      AGroupNum := 20;
  end;
  if AEndDigits < -16 then
    AEndDigits := -16
  else if AEndDigits > 4 then
    AEndDigits := 4;
  SetLength(ATemp, 40); // 最大长度为40
  pd := PWideChar(ATemp) + 39;
  // 计算实际的分组数量，注意此时包含钱和厘，最后会根据实际的显示需要截断
  I := 0;
  while V > 0 do
  begin
    R := V mod 10;
    V := V div 10;
    pd^ := Units[I];
    Dec(pd);
    pd^ := Nums[R];
    Dec(pd);
    Inc(I);
  end;
  if AGroupNum > 0 then
  begin
    if I > AGroupNum then
      raise Exception.CreateFmt(STooSmallCapMoneyGroup, [AGroupNum, I]);
    while AGroupNum > I do
    // 要求的分组数多
    begin
      pd^ := Units[I];
      Dec(pd);
      pd^ := Nums[0];
      Dec(pd);
      Inc(I);
    end;
  end;
  Inc(pd);
  if (AFlags and MC_HIDE_ZERO) <> 0 then // 如果隐藏左侧的零值，则跳过
  begin
    while pd^ <> #0 do
    begin
      if pd^ = Nums[0] then
        Inc(pd, 2)
      else
        Break;
    end;
  end;

  SetLength(Result, PWideChar(ATemp) + 40 - pd);
  p := PWideChar(Result);
  pe := PWideChar(ATemp) + 32 + AEndDigits * 2;
  while pd < pe do
  begin
    if (AFlags and MC_NUM) <> 0 then
    begin
      p^ := pd^;
      Inc(p);
      if ((AFlags and MC_MERGE_ZERO) <> 0) and (pd^ = Nums[0]) then
      begin
        pu := pd;
        Inc(pu);
        while pd^ = Nums[0] do
          Inc(pd, 2);
        if pd^ = #0 then
        begin
          Dec(p);
          if (AFlags and MC_UNIT) <> 0 then
          begin
            if pu^ = Units[4] then
            begin
              p^ := Units[4];
              Inc(p);
            end;
          end;
        end;
        continue;
      end;
    end;
    Inc(pd);
    if (AFlags and MC_UNIT) <> 0 then
    begin
      p^ := pd^;
      Inc(p);
    end;
    Inc(pd);
  end;
  SetLength(Result, p - PWideChar(Result));
  if (AFlags and MC_UNIT) <> 0 then
  begin
    if Length(Result) = 0 then
      Result := '零圆';
    if (AFlags and MC_END_PATCH) <> 0 then
    begin
      if PWideChar(Result)[Length(Result) - 1] = Units[2] then // 分
        Result := AStartText + ANegText + Result
      else
        Result := AStartText + ANegText + Result + AEndText;
    end
    else
      Result := AStartText + ANegText + Result;
  end
  else
  begin
    if Length(Result) = 0 then
      Result := '零';
    Result := AStartText + ANegText + Result;
  end;
end;

function IsHumanName(S: QStringW; AllowChars: TNameCharSet; AMinLen: Integer;
  AMaxLen: Integer; AOnTest: TQCustomNameCharTest): Boolean;
var
  p: PWideChar;
  c: Integer;
  AHandled: Boolean;
begin
  S := Trim(CNFullToHalf(S)); // 将全角转换为半角
  c := CharCountW(S);
  Result := (c >= AMinLen) and (c <= AMaxLen);
  // 姓名的字符数（注意不是字节数）应在最小和最大长度之间
  if not Result then
    Exit;
  p := PWideChar(S);
  while Result and (p^ <> #0) do
  begin
    if Assigned(AOnTest) then
    begin
      AHandled := false;
      AOnTest(qstring.CharCodeW(p), Result, AHandled);
      if AHandled then
      begin
        if (p^ >= #$D800) and (p^ <= #$DBFF) then
          Inc(p, 2)
        else
          Inc(p);
        continue;
      end;
    end;
    if (p^ >= '0') and (p^ <= '9') then
      // 全角或半角数字
      Result := nctNum in AllowChars
    else if ((p^ >= 'a') and (p^ <= 'z')) or ((p^ >= 'A') and (p^ <= 'Z')) then
      // 全角或半角英文
      Result := nctAlpha in AllowChars
    else if p^ = ' ' then // 空格
      Result := nctSpace in AllowChars
    else if p^ = '·' then // 少数民族或外国人姓名分隔符
      Result := nctDot in AllowChars
    else if p^ < #$80 then // 0-127 之间
      Result := nctSymbol in AllowChars
    else // 剩下的是找汉字的区间了
    begin
      // 2400-4DBF CJK统一表意符号扩展
      // 4DC0-4DFF 易经六十四卦符号
      if (p^ >= #$2400) and (p^ <= #$4DFF) then
        Result := nctSymbol in AllowChars
      else if (p^ >= #$4E00) and (p^ <= #$9FA5) then
        // CJK 统一表意符号
        Result := nctChinese in AllowChars
      else if (p^ >= #$E000) and (p^ <= #$F8FF) then // 自定义字符区
        Result := nctCustom in AllowChars
      else if (p^ >= #$D800) and (p^ <= #$DBFF) then // CJK 扩展字符区
      begin
        Result := nctChinese in AllowChars;
        Inc(p);
      end
      else if (p^ >= #$F900) and (p^ <= #$FAFF) then // CJK兼容象形文字
        Result := nctChinese in AllowChars
      else
        Result := nctOther in AllowChars;
    end;
    Inc(p);
  end;
end;

function IsChineseName(S: QStringW): Boolean;
begin
  Result := IsHumanName(S, [nctChinese, nctDot], 2, 50);
end;

function IsNoChineseName(S: QStringW): Boolean;
begin
  Result := IsHumanName(S, [nctAlpha, nctDot, nctSpace, nctOther], 2, 50);
end;

procedure AddrCharTest(AChar: Cardinal; var Accept, AHandled: Boolean);
begin
  if AChar = Ord('-') then
  begin
    Accept := True;
    AHandled := True;
  end;
end;

function IsChineseAddr(S: QStringW; AMinLength: Integer): Boolean;
begin
  Result := IsHumanName(S, [nctChinese, nctAlpha, nctSymbol, nctDot, nctSpace,
    nctNum], AMinLength, 128, AddrCharTest);
end;

function IsNoChineseAddr(S: QStringW; AMinLength: Integer): Boolean;
begin
  Result := IsHumanName(S, [nctAlpha, nctDot, nctSymbol, nctSpace, nctNum],
    AMinLength, 128, AddrCharTest);
end;

{ TQBits }

function TQBits.GetIsSet(AIndex: Integer): Boolean;
begin
  if (AIndex < 0) or (AIndex >= Size) then
    Result := false
  else
    Result := (FBits[AIndex shr 3] and ($80 shr (AIndex and $7))) <> 0;
end;

function TQBits.GetSize: Integer;
begin
  Result := Length(FBits) shl 3;
end;

procedure TQBits.SetIsSet(AIndex: Integer; const Value: Boolean);
var
  AByteIdx: Integer;
begin
  if (AIndex < 0) or (AIndex >= Size) then
    raise QException.CreateFmt(SOutOfIndex, [AIndex, 0, Size - 1]);
  AByteIdx := AIndex shr 3;
  if Value then
    FBits[AByteIdx] := FBits[AByteIdx] or ($80 shr (AIndex and $7))
  else
    FBits[AByteIdx] := FBits[AByteIdx] and (not($80 shr (AIndex and $7)));
end;

procedure TQBits.SetSize(const Value: Integer);
begin
  if (Value and $7) <> 0 then
    SetLength(FBits, (Value shr 3) + 1)
  else
    SetLength(FBits, Value shr 3);
end;

function CheckChineseId18(CardNo: QStringW): QCharW;
var
  Sum, Idx: Integer;
const
  Weight: array [1 .. 17] of Integer = (7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10,
    5, 8, 4, 2);
  Checksums: array [0 .. 10] of WideChar = ('1', '0', 'X', '9', '8', '7', '6',
    '5', '4', '3', '2');
begin
  if (Length(CardNo) >= 17) then
  begin
    Sum := 0;
    for Idx := 1 to 17 do
      Sum := Sum + (Ord(CardNo[Idx{$IFDEF NEXTGEN} - 1{$ENDIF}]) - Ord('0')) *
        Weight[Idx];
    Result := Checksums[Sum mod 11];
  end
  else
    Result := #0;
end;

function ChineseId15To18(CardNo: QStringW): QStringW;
begin
  if (Length(CardNo) <> 15) then
    raise Exception.Create('15位转18位过程要求身份证号必需为15位。');
  CardNo := LeftStrW(CardNo, 6, false) + '19' + RightStrW(CardNo, 9, false);
  Result := CardNo + CheckChineseId18(CardNo);
end;

function DecodeChineseId(CardNo: QStringW; var AreaCode: QStringW;
  var Birthday: TDateTime; var IsFemale: Boolean): Boolean;
var
  len: Integer;
  Y, M, d: Integer;
  p: PQCharW;
begin
  len := Length(CardNo);
  Result := false;
  if (len in [15, 18]) then
  // 长度检查
  begin
    if (Length(CardNo) = 15) then
      CardNo := ChineseId15To18(CardNo);
    if CheckChineseId18(CardNo) <>
      CharUpperW(CardNo[{$IFDEF NEXTGEN}17{$ELSE}18{$ENDIF}]) then // 身份证号校验码检查
      Exit;
    p := PQCharW(CardNo);
    AreaCode := StrDupX(p, 6);
    Inc(p, 6);
    if not TryStrToInt(StrDupX(p, 4), Y) then // 年
      Exit;
    Inc(p, 4);
    if not TryStrToInt(StrDupX(p, 2), M) then // 月
      Exit;
    Inc(p, 2);
    if not TryStrToInt(StrDupX(p, 2), d) then // 日
      Exit;
    Inc(p, 2);
    if not TryEncodeDate(Y, M, d, Birthday) then
      Exit;
    if Birthday > Now then
      Exit;
    if TryStrToInt(StrDupX(p, 3), Y) then
    begin
      Result := True;
      if (Y mod 2) = 0 then
        IsFemale := True
      else
        IsFemale := false;
    end
  end
  else
    Result := false;
end;

function AreaCodeOfChineseId(CardNo: QStringW): QStringW;
var
  ABirthday: TDateTime;
  AIsFemale: Boolean;
begin
  if not DecodeChineseId(CardNo, Result, ABirthday, AIsFemale) then
    SetLength(Result, 0);
end;

function AgeOfChineseId(CardNo: QStringW; ACalcDate: TDateTime): Integer;
var
  ACode: QStringW;
  AIsFemale: Boolean;
  ABirthday: TDateTime;
begin
  if DecodeChineseId(CardNo, ACode, ABirthday, AIsFemale) then
  begin
    if IsZero(ACalcDate) then
      Result := YearsBetween(Now, ABirthday)
    else
      Result := YearsBetween(ACalcDate, ABirthday);
  end
  else
    Result := -1;
end;

function BirthdayOfChineseId(CardNo: QStringW): TDateTime;
var
  ACode: QStringW;
  AIsFemale: Boolean;
begin
  if not DecodeChineseId(CardNo, ACode, Result, AIsFemale) then
    Result := 0;
end;

function SexOfChineseId(CardNo: QStringW): TQHumanSex;
var
  ACode: QStringW;
  AIsFemale: Boolean;
  ABirthday: TDateTime;
begin
  if DecodeChineseId(CardNo, ACode, ABirthday, AIsFemale) then
  begin
    if AIsFemale then
      Result := hsFemale
    else
      Result := hsMale;
  end
  else
    Result := hsUnknown;
end;

function IsChineseIdNo(CardNo: QStringW): Boolean;
var
  AreaCode: QStringW;
  Birthday: TDateTime;
  IsFemale: Boolean;
begin
  Result := DecodeChineseId(CardNo, AreaCode, Birthday, IsFemale);
end;

function IsEmailAddr(S: QStringW): Boolean;
var
  p: PQCharW;
  At: Integer;
  Dot: Integer;
begin
  p := PQCharW(S);
  At := 0;
  Dot := 0;
  while p^ <> #0 do
  begin
    if p^ = '@' then
      Inc(At)
    else if p^ = '.' then
      Inc(Dot);
    Inc(p);
  end;
  Result := (At = 1) and (Dot > 0);
end;

function IsChineseMobile(S: QStringW): Boolean;
var
  p: PQCharW;
begin
  if (Length(S) = 14) and StartWithW(PQCharW(S), '+86', false) then
    S := RightStrW(S, 11, false);
  if (Length(S) = 11) then
  begin
    p := PQCharW(S);
    if p^ = '1' then // 中国手机号以1打头，
    begin
      Result := True;
      while p^ <> #0 do
      begin
        if ((p^ >= '0') and (p^ <= '9')) or (p^ = '-') or (p^ = ' ') then
          Inc(p)
        else
        begin
          Result := false;
          Exit;
        end;
      end;
    end
    else
      Result := false;
  end
  else
    Result := false;
end;

function SizeOfFile(const S: QStringW): Int64;
var
  sr: TSearchRec;
begin
  if FindFirst(S, 0, sr) = 0 then
  begin
    Result := sr.Size;
    sysutils.FindClose(sr);
  end
  else
    Result := -1;
end;

function MethodEqual(const Left, Right: TMethod): Boolean;
begin
  Result := (Left.Data = Right.Data) and (Left.Code = Right.Code);
end;

function MethodAssigned(var AMethod): Boolean;
begin
  Result := Assigned(TMethod(AMethod).Code);
end;

function UrlMerge(const ABase, ARel: QStringW): QStringW;
var
  p, pBase, pRel: PQCharW;
  ASchema: QStringW;
  function BasePath: String;
  var
    LP, sp: PQCharW;
  begin
    p := pBase;
    if StartWithW(p, 'http://', True) then
      ASchema := StrDupX(p, 7)
    else if StartWithW(p, 'https://', True) then
      ASchema := StrDupX(p, 8);
    Inc(p, Length(ASchema));
    LP := p;
    sp := p;
    while p^ <> #0 do
    begin
      if p^ = '/' then
        LP := p;
      Inc(p);
    end;
    if LP = sp then
      Result := ABase + '/'
    else
      Result := StrDupX(pBase, ((IntPtr(LP) - IntPtr(pBase)) shr 1) + 1);
  end;

  function RootPath: String;
  begin
    p := pBase;
    if StartWithW(p, 'http://', True) then
      ASchema := StrDupX(p, 7)
    else if StartWithW(p, 'https://', True) then
      ASchema := StrDupX(p, 8);
    Inc(p, Length(ASchema));
    while p^ <> #0 do
    begin
      if p^ = '/' then
      begin
        Result := StrDupX(pBase, ((IntPtr(p) - IntPtr(pBase)) shr 1) + 1);
        Exit;
      end;
      Inc(p);
    end;
    Result := ABase + '/'
  end;

begin
  pRel := PQCharW(ARel);
  pBase := PQCharW(ABase);
  if StartWithW(pRel, 'http', True) then // 绝对路径？
  begin
    p := pRel;
    Inc(pRel, 4);
    if (pRel^ = 's') or (pRel^ = 'S') then
      // https?
      Inc(pRel);
    if StartWithW(pRel, '://', false) then // 使用的绝对路径，直接返回
    begin
      Result := ARel;
      Exit;
    end;
  end
  else if StartWithW(pRel, '//', True) then // 基于ABase相同的协议的绝对路径
  begin
    if StartWithW(pBase, 'https', True) then
      Result := 'https:' + ARel
    else
      Result := 'http:' + ARel;
  end
  else
  begin
    if pRel^ = '/' then
      Result := RootPath + StrDupW(pRel, 1)
    else
      Result := BasePath + ARel;
  end;
end;

procedure Debugout(const AMsg: String);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AMsg));
{$ENDIF}
{$IFDEF ANDROID}
  __android_log_write(ANDROID_LOG_DEBUG, 'debug',
    Pointer(PQCharA(qstring.Utf8Encode(AMsg))));
{$ENDIF}
{$IFDEF IOS}
  NSLog(((StrToNSStr(AMsg)) as ILocalObject).GetObjectID);
{$ENDIF}
{$IFDEF OSX}
  WriteLn(ErrOutput, AMsg);
{$ENDIF}
end;

procedure Debugout(const AFmt: String; const AParams: array of const);
begin
  Debugout(Format(AFmt, AParams));
end;

{ TQSingleton<T> }

function TQSingleton{$IFDEF UNICODE}<T>{$ENDIF}.Instance
  (ACallback: TGetInstanceCallback):
{$IFDEF UNICODE}T{$ELSE}Pointer{$ENDIF};
begin
  if not Assigned(InitToNull) then
  begin
    GlobalNameSpace.BeginWrite;
    try
      if not Assigned(InitToNull) then
        InitToNull := ACallback;
    finally
      GlobalNameSpace.EndWrite;
    end;
  end;
  Result := InitToNull;
end;

{ TQReadOnlyMemoryStream }

constructor TQReadOnlyMemoryStream.Create(AData: Pointer; ASize: Integer);
begin
  inherited Create;
  SetPointer(AData, ASize);
end;

constructor TQReadOnlyMemoryStream.Create;
begin
  inherited;
  // 这个构造函数变成保护的，以避免外部访问
end;

function TQReadOnlyMemoryStream.Write(const Buffer; count: Longint): Longint;
begin
  raise EStreamError.Create(SStreamReadOnly);
end;

initialization

{$IFDEF WIN32}
  MemComp := MemCompAsm;
{$ELSE}
  MemComp := MemCompPascal;
{$ENDIF}
{$IFDEF MSWINDOWS}
hMsvcrtl := LoadLibrary('msvcrt.dll');
if hMsvcrtl <> 0 then
begin
  VCStrStr := TMSVCStrStr(GetProcAddress(hMsvcrtl, 'strstr'));
  VCStrStrW := TMSVCStrStrW(GetProcAddress(hMsvcrtl, 'wcsstr'));
{$IFDEF WIN64}
  VCMemCmp := TMSVCMemCmp(GetProcAddress(hMsvcrtl, 'memcmp'));
{$ENDIF}
end
else
begin
  VCStrStr := nil;
  VCStrStrW := nil;
{$IFDEF WIN64}
  VCMemCmp := nil;
{$ENDIF}
end;
{$ENDIF}
IsFMXApp := GetClass('TFmxObject') <> nil;

finalization

{$IFDEF MSWINDOWS}
if hMsvcrtl <> 0 then
  FreeLibrary(hMsvcrtl);
{$ENDIF}

end.
 
