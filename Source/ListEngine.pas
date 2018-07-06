{ ****************************************************************************** }
{ * hash Library,Writen by QQ 600585@qq.com                                    * }
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
  "String" define change as "SystemString"

  2017-12-5
  added support int64 hash object : TInt64HashObjectList
  added support pointer-NativeUInt hash : TPointerHashNativeUIntList

  2018-4-17
  added support big StringList with TListString and TListPascalString
*)

unit ListEngine;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Classes, Variants, CoreClasses, PascalStrings;

type
  TCounter = nativeUInt;

  TListBuffer = packed array of TCoreClassList;
  PListBuffer = ^TListBuffer;

  THashObjectList  = class;
  THashVariantList = class;

  PHashListData = ^THashListData;

  THashListData = packed record
    qHash: THash;
    LowerCaseName, OriginName: SystemString;
    Data: Pointer;
    ID: TCounter;
    Prev, Next: PHashListData;
  end;

  TOnPtr = procedure(p: Pointer) of object;

  TListString       = class;
  TListPascalString = class;

  THashList = class(TCoreClassObject)
  private
    FListBuffer: TListBuffer;
    FAutoFreeData: Boolean;
    FCount: nativeInt;
    FIDCounter: TCounter;
    FIgnoreCase: Boolean;
    FAccessOptimization: Boolean;
    FOnFreePtr: TOnPtr;

    FFirst: PHashListData;
    FLast: PHashListData;

    FMaxNameLen, FMinNameLen: nativeInt;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
    function GetKeyData(const Name: SystemString): PHashListData;
    function GetKeyValue(const Name: SystemString): Pointer;

    procedure RebuildIDCounter;

    procedure DoAdd(p: PHashListData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DoDelete(p: PHashListData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DefaultDataFreeProc(p: Pointer);

    procedure DoDataFreeProc(p: Pointer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    constructor Create; overload;
    constructor Create(hashBlockCount: Integer); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GetNameList(var output: TArrayPascalString); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    procedure GetListData(OutputList: TCoreClassList);
    procedure Delete(const Name: SystemString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Add(const Name: SystemString; _CustomData: Pointer; const overwrite: Boolean); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Add(const Name: SystemString; _CustomData: Pointer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetValue(const Name: SystemString; const _CustomData: Pointer);
    function Find(const Name: SystemString): Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(const Name: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetHashBlockCount(cnt: Integer);

    property FirstPtr: PHashListData read FFirst;
    property LastPtr: PHashListData read FLast;

    function First: Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Last: Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetNext(const Name: SystemString): Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPrev(const Name: SystemString): Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ListBuffer: PListBuffer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: nativeInt read FCount write FCount;

    property KeyValue[const Name: SystemString]: Pointer read GetKeyValue write SetValue; default;
    property NameValue[const Name: SystemString]: Pointer read GetKeyValue write SetValue;

    property KeyData[const Name: SystemString]: PHashListData read GetKeyData;
    property NameData[const Name: SystemString]: PHashListData read GetKeyData;

    property OnFreePtr: TOnPtr read FOnFreePtr write FOnFreePtr;

    property MaxKeyLen: nativeInt read FMaxNameLen;
    property MinKeyLen: nativeInt read FMinNameLen;
    property MaxNameLen: nativeInt read FMaxNameLen;
    property MinNameLen: nativeInt read FMinNameLen;
  end;

  PInt64HashListObjectStruct = ^TInt64HashListObjectStruct;

  TInt64HashListObjectStruct = packed record
    qHash: THash;
    i64: Int64;
    Data: TCoreClassObject;
    ID: TCounter;
    Prev, Next: PInt64HashListObjectStruct;
  end;

  TObjectFreeProc = procedure(Obj: TCoreClassObject) of object;

  TInt64HashObjectList = class(TCoreClassObject)
  private
    FListBuffer: TListBuffer;
    FCount: nativeInt;
    FIDCounter: TCounter;
    FAccessOptimization: Boolean;
    FAutoFreeData: Boolean;
    FFirst: PInt64HashListObjectStruct;
    FLast: PInt64HashListObjectStruct;
    FOnObjectFreeProc: TObjectFreeProc;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
    function Geti64Data(i64: Int64): PInt64HashListObjectStruct;
    function Geti64Val(i64: Int64): TCoreClassObject;

    procedure RebuildIDCounter;

    procedure DoAdd(p: PInt64HashListObjectStruct); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DoDelete(p: PInt64HashListObjectStruct); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DefaultObjectFreeProc(Obj: TCoreClassObject);
    procedure DoDataFreeProc(Obj: TCoreClassObject); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    constructor Create; overload;
    constructor Create(hashBlockCount: Integer); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GetListData(OutputList: TCoreClassList);
    procedure Delete(i64: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(i64: Int64; _CustomData: TCoreClassObject; const overwrite: Boolean): PInt64HashListObjectStruct; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetValue(i64: Int64; _CustomData: TCoreClassObject);
    function Exists(i64: Int64): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetHashBlockCount(cnt: Integer);

    procedure DeleteFirst;
    procedure DeleteLast;

    property FirstPtr: PInt64HashListObjectStruct read FFirst;
    property LastPtr: PInt64HashListObjectStruct read FLast;

    function First: TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Last: TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetNext(i64: Int64): TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPrev(i64: Int64): TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ListBuffer: PListBuffer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: nativeInt read FCount write FCount;
    property i64Val[i64: Int64]: TCoreClassObject read Geti64Val write SetValue; default;
    property i64Data[i64: Int64]: PInt64HashListObjectStruct read Geti64Data;
    property OnObjectFreeProc: TObjectFreeProc read FOnObjectFreeProc write FOnObjectFreeProc;
  end;

  PInt64HashListPointerStruct = ^TInt64HashListPointerStruct;

  TInt64HashListPointerStruct = packed record
    qHash: THash;
    i64: Int64;
    Data: Pointer;
    ID: TCounter;
    Prev, Next: PInt64HashListPointerStruct;
  end;

  TInt64HashPointerList = class(TCoreClassObject)
  private
    FListBuffer: TListBuffer;
    FCount: nativeInt;
    FIDCounter: TCounter;
    FAccessOptimization: Boolean;
    FAutoFreeData: Boolean;
    FFirst: PInt64HashListPointerStruct;
    FLast: PInt64HashListPointerStruct;
    FOnFreePtr: TOnPtr;
    FOnAddPtr: TOnPtr;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
    function Geti64Data(i64: Int64): PInt64HashListPointerStruct;
    function Geti64Val(i64: Int64): Pointer;

    procedure RebuildIDCounter;

    procedure DoAdd(p: PInt64HashListPointerStruct); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DoDelete(p: PInt64HashListPointerStruct); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DefaultDataFreeProc(p: Pointer);
    procedure DoDataFreeProc(p: Pointer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DoAddDataNotifyProc(p: Pointer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    constructor Create; overload;
    constructor Create(hashBlockCount: Integer); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GetListData(OutputList: TCoreClassList);
    procedure Delete(i64: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(i64: Int64; _CustomData: Pointer; const overwrite: Boolean): PInt64HashListPointerStruct; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetValue(i64: Int64; _CustomData: Pointer);
    function Exists(i64: Int64): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetHashBlockCount(cnt: Integer);

    property FirstPtr: PInt64HashListPointerStruct read FFirst;
    property LastPtr: PInt64HashListPointerStruct read FLast;

    function First: Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Last: Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetNext(i64: Int64): Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPrev(i64: Int64): Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ListBuffer: PListBuffer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: nativeInt read FCount write FCount;
    property i64Val[i64: Int64]: Pointer read Geti64Val write SetValue; default;
    property i64Data[i64: Int64]: PInt64HashListPointerStruct read Geti64Data;
    property OnFreePtr: TOnPtr read FOnFreePtr write FOnFreePtr;
    property OnAddPtr: TOnPtr read FOnAddPtr write FOnAddPtr;
  end;

  PUInt32HashListObjectStruct = ^TUInt32HashListObjectStruct;

  TUInt32HashListObjectStruct = packed record
    qHash: THash;
    u32: UInt32;
    Data: TCoreClassObject;
    ID: TCounter;
    Prev, Next: PUInt32HashListObjectStruct;
  end;

  TUInt32HashObjectListLoopCall             = procedure(u32: UInt32; Obj: TCoreClassObject);
  TUInt32HashObjectListLoopMethod           = procedure(u32: UInt32; Obj: TCoreClassObject) of object;
{$IFNDEF FPC} TUInt32HashObjectListLoopProc = reference to procedure(u32: UInt32; Obj: TCoreClassObject); {$ENDIF}

  TUInt32HashObjectList = class(TCoreClassObject)
  private
    FListBuffer: TListBuffer;
    FCount: nativeInt;
    FIDCounter: TCounter;
    FAccessOptimization: Boolean;
    FAutoFreeData: Boolean;
    FFirst: PUInt32HashListObjectStruct;
    FLast: PUInt32HashListObjectStruct;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
    function Getu32Data(u32: UInt32): PUInt32HashListObjectStruct;
    function Getu32Val(u32: UInt32): TCoreClassObject;

    procedure RebuildIDCounter;

    procedure DoAdd(p: PUInt32HashListObjectStruct); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DoDelete(p: PUInt32HashListObjectStruct); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DoDataFreeProc(Obj: TCoreClassObject); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    constructor Create; overload;
    constructor Create(hashBlockCount: Integer); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GetListData(OutputList: TCoreClassList);
    procedure Delete(u32: UInt32); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(u32: UInt32; _CustomData: TCoreClassObject; const overwrite: Boolean): PUInt32HashListObjectStruct; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetValue(u32: UInt32; _CustomData: TCoreClassObject);
    function Exists(u32: UInt32): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetHashBlockCount(cnt: Integer);

    property FirstPtr: PUInt32HashListObjectStruct read FFirst;
    property LastPtr: PUInt32HashListObjectStruct read FLast;

    function First: TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Last: TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetNext(u32: UInt32): TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPrev(u32: UInt32): TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ListBuffer: PListBuffer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Progress(OnProgress: TUInt32HashObjectListLoopCall); overload;
    procedure Progress(OnProgress: TUInt32HashObjectListLoopMethod); overload;
{$IFNDEF FPC} procedure Progress(OnProgress: TUInt32HashObjectListLoopProc); overload; {$ENDIF}
    //
    function ExistsObject(Obj: TCoreClassObject): Boolean;

    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: nativeInt read FCount write FCount;
    property u32Val[u32: UInt32]: TCoreClassObject read Getu32Val write SetValue; default;
    property u32Data[u32: UInt32]: PUInt32HashListObjectStruct read Getu32Data;
  end;

  PPointerHashListNativeUIntStruct = ^TPointerHashListNativeUIntStruct;

  TPointerHashListNativeUIntStruct = packed record
    qHash: THash;
    NPtr: Pointer;
    Data: nativeUInt;
    ID: TCounter;
    Prev, Next: PPointerHashListNativeUIntStruct;
  end;

  TPointerHashNativeUIntListLoopCall             = procedure(NPtr: Pointer; uData: nativeUInt);
  TPointerHashNativeUIntListLoopMethod           = procedure(NPtr: Pointer; uData: nativeUInt) of object;
{$IFNDEF FPC} TPointerHashNativeUIntListLoopProc = reference to procedure(NPtr: Pointer; uData: nativeUInt); {$ENDIF}

  TPointerHashNativeUIntList = class(TCoreClassObject)
  public
    const
    NullValue = 0;
  private
    FListBuffer: TListBuffer;
    FCount: nativeInt;
    FIDCounter: TCounter;
    FAccessOptimization: Boolean;
    FFirst: PPointerHashListNativeUIntStruct;
    FLast: PPointerHashListNativeUIntStruct;
    FTotal: UInt64;
    FMinimizePtr, FMaximumPtr: Pointer;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
    function GetNPtrData(NPtr: Pointer): PPointerHashListNativeUIntStruct;
    function GetNPtrVal(NPtr: Pointer): nativeUInt;

    procedure RebuildIDCounter;

    procedure DoAdd(p: PPointerHashListNativeUIntStruct); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DoDelete(p: PPointerHashListNativeUIntStruct); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    constructor Create; overload;
    constructor Create(hashBlockCount: Integer); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure FastClear;
    procedure GetListData(OutputList: TCoreClassList);
    function Delete(NPtr: Pointer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(NPtr: Pointer; _CustomData: nativeUInt; const overwrite: Boolean): PPointerHashListNativeUIntStruct; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetValue(NPtr: Pointer; _CustomData: nativeUInt);
    function Exists(NPtr: Pointer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetHashBlockCount(cnt: Integer);

    property FirstPtr: PPointerHashListNativeUIntStruct read FFirst;
    property LastPtr: PPointerHashListNativeUIntStruct read FLast;

    function First: nativeUInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Last: nativeUInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetNext(NPtr: Pointer): nativeUInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPrev(NPtr: Pointer): nativeUInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ListBuffer: PListBuffer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Progress(OnProgress: TPointerHashNativeUIntListLoopCall); overload;
    procedure Progress(OnProgress: TPointerHashNativeUIntListLoopMethod); overload;
{$IFNDEF FPC} procedure Progress(OnProgress: TPointerHashNativeUIntListLoopProc); overload; {$ENDIF}
    //
    function ExistsNaviveUInt(Obj: nativeUInt): Boolean;

    procedure PrintHashReport;

    property Total: UInt64 read FTotal;
    property MinimizePtr: Pointer read FMinimizePtr;
    property MaximumPtr: Pointer read FMaximumPtr;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: nativeInt read FCount write FCount;
    property NPtrVal[NPtr: Pointer]: nativeUInt read GetNPtrVal write SetValue; default;
    property NPtrData[NPtr: Pointer]: PPointerHashListNativeUIntStruct read GetNPtrData;
  end;

  THashObjectChangeEvent = procedure(Sender: THashObjectList; Name: SystemString; _OLD, _New: TCoreClassObject) of object;

  THashObjectListData = packed record
    Obj: TCoreClassObject;
    OnChnage: THashObjectChangeEvent;
  end;

  PHashObjectListData = ^THashObjectListData;

  THashObjectListLoopCall             = procedure(const Name: PSystemString; Obj: TCoreClassObject);
  THashObjectListLoopMethod           = procedure(const Name: PSystemString; Obj: TCoreClassObject) of object;
{$IFNDEF FPC} THashObjectListLoopProc = reference to procedure(const Name: PSystemString; Obj: TCoreClassObject); {$ENDIF}

  THashObjectList = class(TCoreClassObject)
  private
    FAutoFreeObject: Boolean;
    FHashList: THashList;
    FIncremental: nativeInt;

    function GetCount: nativeInt;

    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);

    function GetKeyValue(const Name: SystemString): TCoreClassObject;
    procedure SetKeyValue(const Name: SystemString; const Value: TCoreClassObject);

    function GetOnChange(const Name: SystemString): THashObjectChangeEvent;
    procedure SetOnChange(const Name: SystemString; const AValue: THashObjectChangeEvent);

    function GetAccessOptimization: Boolean;
    procedure SetAccessOptimization(const Value: Boolean);

    procedure DefaultDataFreeProc(p: Pointer);
  protected
  public
    constructor Create(_AutoFreeObject: Boolean); overload;
    constructor Create(_AutoFreeObject: Boolean; MaxHashBlock: Integer); overload;
    destructor Destroy; override;

    procedure Assign(sour: THashObjectList);

    procedure Progress(OnProgress: THashObjectListLoopCall); overload;
    procedure Progress(OnProgress: THashObjectListLoopMethod); overload;
{$IFNDEF FPC} procedure Progress(OnProgress: THashObjectListLoopProc); overload; {$ENDIF}
    //
    procedure Clear;
    procedure GetNameList(OutputList: TCoreClassStrings); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    procedure GetListData(OutputList: TCoreClassStrings); overload;
    procedure GetListData(OutputList: TListString); overload;
    procedure GetListData(OutputList: TListPascalString); overload;
    procedure GetAsList(OutputList: TCoreClassListForObj);
    function GetObjAsName(Obj: TCoreClassObject): SystemString;
    procedure Delete(const Name: SystemString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(const Name: SystemString; _Object: TCoreClassObject): TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FastAdd(const Name: SystemString; _Object: TCoreClassObject): TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Find(const Name: SystemString): TCoreClassObject; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(const Name: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ExistsObject(Obj: TCoreClassObject): Boolean;
    procedure CopyFrom(const Source: THashObjectList);
    function ReName(_OLDName, _NewName: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function MakeName: SystemString;
    function MakeRefName(RefrenceName: SystemString): SystemString;

    property AccessOptimization: Boolean read GetAccessOptimization write SetAccessOptimization;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property AutoFreeObject: Boolean read FAutoFreeObject write FAutoFreeObject;
    property Count: nativeInt read GetCount;

    property KeyValue[const Name: SystemString]: TCoreClassObject read GetKeyValue write SetKeyValue; default;
    property NameValue[const Name: SystemString]: TCoreClassObject read GetKeyValue write SetKeyValue;
    property OnChange[const Name: SystemString]: THashObjectChangeEvent read GetOnChange write SetOnChange;
    property HashList: THashList read FHashList;
  end;

  THashStringList = class;

  THashStringChangeEvent = procedure(Sender: THashStringList; Name: SystemString; _OLD, _New: SystemString) of object;

  THashStringListData = packed record
    v: SystemString;
    OnChnage: THashStringChangeEvent;
  end;

  PHashStringListData = ^THashStringListData;

  THashStringListLoopCall             = procedure(Sender: THashStringList; Name: PSystemString; const v: SystemString);
  THashStringListLoopMethod           = procedure(Sender: THashStringList; Name: PSystemString; const v: SystemString) of object;
{$IFNDEF FPC} THashStringListLoopProc = reference to procedure(Sender: THashStringList; Name: PSystemString; const v: SystemString); {$ENDIF}

  THashStringList = class(TCoreClassObject)
  private
    FHashList: THashList;
    FAutoUpdateDefaultValue: Boolean;
    FOnValueChangeNotify: THashStringChangeEvent;

    function GetCount: nativeInt;

    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);

    function GetKeyValue(const Name: SystemString): SystemString;
    procedure SetKeyValue(const Name: SystemString; const Value: SystemString);

    function GetOnChange(const Name: SystemString): THashStringChangeEvent;
    procedure SetOnChange(const Name: SystemString; const AValue: THashStringChangeEvent);

    function GetAccessOptimization: Boolean;
    procedure SetAccessOptimization(const Value: Boolean);

    procedure DefaultDataFreeProc(p: Pointer);
  protected
  public
    constructor Create; overload;
    constructor Create(MaxHashBlock: Integer); overload;
    destructor Destroy; override;
    //
    procedure Assign(sour: THashStringList);
    //
    procedure Progress(OnProgress: THashStringListLoopCall); overload;
    procedure Progress(OnProgress: THashStringListLoopMethod); overload;
{$IFNDEF FPC} procedure Progress(OnProgress: THashStringListLoopProc); overload; {$ENDIF}
    //
    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure GetNameList(OutputList: TCoreClassStrings); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    //
    procedure Delete(const Name: SystemString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(const Name: SystemString; v: SystemString): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FastAdd(const Name: SystemString; v: SystemString): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Find(const Name: SystemString): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FindValue(const AValue: SystemString): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(const Name: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure CopyFrom(const Source: THashStringList);
    function IncValue(const Name: SystemString; v: SystemString): SystemString; overload;
    procedure IncValue(const vl: THashStringList); overload;

    function GetDefaultValue(const Name: SystemString; AValue: SystemString): SystemString;
    procedure SetDefaultValue(const Name: SystemString; AValue: SystemString);

    function ReplaceMacro(const AText, HeadToken, TailToken: SystemString; var output: SystemString): Boolean;

    property AutoUpdateDefaultValue: Boolean read FAutoUpdateDefaultValue write FAutoUpdateDefaultValue;
    property AccessOptimization: Boolean read GetAccessOptimization write SetAccessOptimization;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property Count: nativeInt read GetCount;

    property KeyValue[const Name: SystemString]: SystemString read GetKeyValue write SetKeyValue; default;
    property NameValue[const Name: SystemString]: SystemString read GetKeyValue write SetKeyValue;

    property OnChange[const Name: SystemString]: THashStringChangeEvent read GetOnChange write SetOnChange;
    property OnValueChangeNotify: THashStringChangeEvent read FOnValueChangeNotify write FOnValueChangeNotify;
    property HashList: THashList read FHashList;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromFile(fileName: SystemString);
    procedure SaveToFile(fileName: SystemString);
    procedure ExportAsStrings(output: TListPascalString);
    procedure ImportFromStrings(output: TListPascalString);
    function GetAsText: SystemString;
    procedure SetAsText(const Value: SystemString);
    property AsText: SystemString read GetAsText write SetAsText;
  end;

  THashStringTextStream = class(TCoreClassObject)
  private
    FStringList: THashStringList;

    function GetKeyValue(AName: SystemString): SystemString;
    procedure SetKeyValue(AName: SystemString; const Value: SystemString);
  public
    constructor Create(_VList: THashStringList);
    destructor Destroy; override;
    procedure Clear;

    function VToStr(const v: SystemString): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function StrToV(const s: SystemString): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DataImport(TextList: TListPascalString);
    procedure DataExport(TextList: TListPascalString);
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromFile(fileName: SystemString);
    procedure SaveToFile(fileName: SystemString);

    procedure LoadFromText(AText: SystemString);
    procedure SaveToText(var AText: SystemString);
    function Text: SystemString;

    property StringList: THashStringList read FStringList write FStringList;
  end;

  THashVariantChangeEvent = procedure(Sender: THashVariantList; Name: SystemString; _OLD, _New: Variant) of object;

  THashVariantListData = packed record
    v: Variant;
    OnChnage: THashVariantChangeEvent;
  end;

  PHashVariantListData = ^THashVariantListData;

  THashVariantListLoopCall             = procedure(Sender: THashVariantList; Name: PSystemString; const v: Variant);
  THashVariantListLoopMethod           = procedure(Sender: THashVariantList; Name: PSystemString; const v: Variant) of object;
{$IFNDEF FPC} THashVariantListLoopProc = reference to procedure(Sender: THashVariantList; Name: PSystemString; const v: Variant); {$ENDIF}

  THashVariantList = class(TCoreClassObject)
  private
    FHashList: THashList;
    FAutoUpdateDefaultValue: Boolean;
    FOnValueChangeNotify: THashVariantChangeEvent;

    function GetCount: nativeInt;

    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);

    function GetKeyValue(const Name: SystemString): Variant;
    procedure SetKeyValue(const Name: SystemString; const Value: Variant);

    function GetOnChange(const Name: SystemString): THashVariantChangeEvent;
    procedure SetOnChange(const Name: SystemString; const AValue: THashVariantChangeEvent);

    function GetAccessOptimization: Boolean;
    procedure SetAccessOptimization(const Value: Boolean);

    procedure DefaultDataFreeProc(p: Pointer);

    function GetI64(const Name: SystemString): Int64;
    procedure SetI64(const Name: SystemString; const Value: Int64);
    function GetI32(const Name: SystemString): Integer;
    procedure SetI32(const Name: SystemString; const Value: Integer);
    function GetF(const Name: SystemString): Double;
    procedure SetF(const Name: SystemString; const Value: Double);
    function GetS(const Name: SystemString): SystemString;
    procedure SetS(const Name, Value: SystemString);
  protected
  public
    constructor Create; overload;
    constructor Create(MaxHashBlock: Integer); overload;
    destructor Destroy; override;
    //
    procedure Assign(sour: THashVariantList);
    //
    procedure Progress(OnProgress: THashVariantListLoopCall); overload;
    procedure Progress(OnProgress: THashVariantListLoopMethod); overload;
{$IFNDEF FPC} procedure Progress(OnProgress: THashVariantListLoopProc); overload; {$ENDIF}
    //
    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure GetNameList(OutputList: TCoreClassStrings); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    //
    procedure Delete(const Name: SystemString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(const Name: SystemString; v: Variant): Variant; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FastAdd(const Name: SystemString; v: Variant): Variant; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Find(const Name: SystemString): Variant; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FindValue(const AValue: Variant): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(const Name: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure CopyFrom(const Source: THashVariantList);
    function GetType(const Name: SystemString): Word; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function IncValue(const Name: SystemString; v: Variant): Variant; overload;
    procedure IncValue(const vl: THashVariantList); overload;

    function SetMax(const Name: SystemString; v: Variant): Variant; overload;
    procedure SetMax(const vl: THashVariantList); overload;

    function SetMin(const Name: SystemString; v: Variant): Variant; overload;
    procedure SetMin(const vl: THashVariantList); overload;

    function GetDefaultValue(const Name: SystemString; AValue: Variant): Variant;
    procedure SetDefaultValue(const Name: SystemString; AValue: Variant);

    function ReplaceMacro(const AText, HeadToken, TailToken: SystemString; var output: SystemString): Boolean;

    property AutoUpdateDefaultValue: Boolean read FAutoUpdateDefaultValue write FAutoUpdateDefaultValue;
    property AccessOptimization: Boolean read GetAccessOptimization write SetAccessOptimization;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property Count: nativeInt read GetCount;

    property i64[const Name: SystemString]: Int64 read GetI64 write SetI64;
    property i32[const Name: SystemString]: Integer read GetI32 write SetI32;
    property F[const Name: SystemString]: Double read GetF write SetF;
    property s[const Name: SystemString]: SystemString read GetS write SetS;

    property KeyValue[const Name: SystemString]: Variant read GetKeyValue write SetKeyValue; default;
    property NameValue[const Name: SystemString]: Variant read GetKeyValue write SetKeyValue;

    property OnChange[const Name: SystemString]: THashVariantChangeEvent read GetOnChange write SetOnChange;
    property OnValueChangeNotify: THashVariantChangeEvent read FOnValueChangeNotify write FOnValueChangeNotify;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromFile(fileName: SystemString);
    procedure SaveToFile(fileName: SystemString);
    procedure ExportAsStrings(output: TListPascalString); overload;
    procedure ExportAsStrings(output: TCoreClassStrings); overload;
    procedure ImportFromStrings(output: TListPascalString); overload;
    procedure ImportFromStrings(output: TCoreClassStrings); overload;
    function GetAsText: SystemString;
    procedure SetAsText(const Value: SystemString);
    property AsText: SystemString read GetAsText write SetAsText;

    property HashList: THashList read FHashList;
  end;

  THashVariantTextStream = class(TCoreClassObject)
  private
    FVariantList: THashVariantList;

    function GetKeyValue(AName: SystemString): Variant;
    procedure SetKeyValue(AName: SystemString; const Value: Variant);
  public
    constructor Create(_VList: THashVariantList);
    destructor Destroy; override;
    procedure Clear;

    class function VToStr(const v: Variant): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function StrToV(const s: SystemString): Variant; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DataImport(TextList: TListPascalString); overload;
    procedure DataImport(TextList: TCoreClassStrings); overload;
    procedure DataExport(TextList: TListPascalString); overload;
    procedure DataExport(TextList: TCoreClassStrings); overload;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromFile(fileName: SystemString);
    procedure SaveToFile(fileName: SystemString);

    procedure LoadFromText(AText: SystemString);
    procedure SaveToText(var AText: SystemString); overload;
    function Text: SystemString;

    function GetValue(AName: SystemString; v: Variant): Variant;

    property NameValue[AName: SystemString]: Variant read GetKeyValue write SetKeyValue; default;
    property VariantList: THashVariantList read FVariantList write FVariantList;
  end;

  TListCardinalData = packed record
    Data: Cardinal;
  end;

  PListCardinalData = ^TListCardinalData;

  TListCardinal = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(idx: Integer): Cardinal;
    procedure SetItems(idx: Integer; Value: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Cardinal): Integer;
    procedure AddArray(const Value: array of Cardinal);
    function Delete(idx: Integer): Integer;
    function DeleteCardinal(Value: Cardinal): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Cardinal): Integer;
    procedure Assign(SameObj: TListCardinal);

    property Items[idx: Integer]: Cardinal read GetItems write SetItems; default;
  end;

  TListInt64Data = packed record
    Data: Int64;
  end;

  PListInt64Data = ^TListInt64Data;

  TListInt64 = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(idx: Integer): Int64;
    procedure SetItems(idx: Integer; Value: Int64);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Int64): Integer;
    procedure AddArray(const Value: array of Int64);
    function Delete(idx: Integer): Integer;
    function DeleteInt64(Value: Int64): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Int64): Integer;
    procedure Assign(SameObj: TListInt64);

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    property Items[idx: Integer]: Int64 read GetItems write SetItems; default;
    property List: TCoreClassList read FList;
  end;

  TListNativeIntData = packed record
    Data: nativeInt;
  end;

  PListNativeIntData = ^TListNativeIntData;

  TListNativeInt = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(idx: Integer): nativeInt;
    procedure SetItems(idx: Integer; Value: nativeInt);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: nativeInt): Integer;
    procedure AddArray(const Value: array of nativeInt);
    function Delete(idx: Integer): Integer;
    function DeleteNativeInt(Value: nativeInt): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: nativeInt): Integer;
    procedure Assign(SameObj: TListNativeInt);

    property Items[idx: Integer]: nativeInt read GetItems write SetItems; default;
  end;

  TListIntegerData = packed record
    Data: Integer;
  end;

  PListIntegerData = ^TListIntegerData;

  TListInteger = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(idx: Integer): Integer;
    procedure SetItems(idx: Integer; Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Integer): Integer;
    procedure AddArray(const Value: array of Integer);
    function Delete(idx: Integer): Integer;
    function DeleteInteger(Value: Integer): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Integer): Integer;
    procedure Assign(SameObj: TListInteger);

    property Items[idx: Integer]: Integer read GetItems write SetItems; default;
  end;

  TListDoubleData = packed record
    Data: Double;
  end;

  PListDoubleData = ^TListDoubleData;

  TListDouble = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(idx: Integer): Double;
    procedure SetItems(idx: Integer; Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Double): Integer;
    procedure AddArray(const Value: array of Double);
    function Delete(idx: Integer): Integer;
    procedure Clear;
    function Count: Integer;
    procedure Assign(SameObj: TListDouble);

    property Items[idx: Integer]: Double read GetItems write SetItems; default;
  end;

  TListPointerData = packed record
    Data: Pointer;
  end;

  PListPointerData = ^TListPointerData;

  TListPointer = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(idx: Integer): Pointer;
    procedure SetItems(idx: Integer; Value: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Pointer): Integer;
    function Delete(idx: Integer): Integer;
    function DeletePointer(Value: Pointer): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Pointer): Integer;
    procedure Assign(SameObj: TListPointer);

    property Items[idx: Integer]: Pointer read GetItems write SetItems; default;
  end;

  TPointerList = TListPointer;

  TListStringData = packed record
    Data: SystemString;
    Obj: TCoreClassObject;
    hash: THash;
  end;

  PListStringData = ^TListStringData;

  TListString = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(idx: Integer): SystemString;
    procedure SetItems(idx: Integer; Value: SystemString);

    function GetObjects(idx: Integer): TCoreClassObject;
    procedure SetObjects(idx: Integer; Value: TCoreClassObject);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: SystemString): Integer; overload;
    function Add(Value: SystemString; Obj: TCoreClassObject): Integer; overload;
    function Delete(idx: Integer): Integer;
    function DeleteString(Value: SystemString): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: SystemString): Integer;
    procedure Assign(SameObj: TListString);

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromFile(fn: SystemString);
    procedure SaveToFile(fn: SystemString);

    property Items[idx: Integer]: SystemString read GetItems write SetItems; default;
    property Objects[idx: Integer]: TCoreClassObject read GetObjects write SetObjects;
  end;

  TListPascalStringData = packed record
    Data: TPascalString;
    Obj: TCoreClassObject;
    hash: THash;
  end;

  PListPascalStringData = ^TListPascalStringData;

  TListPascalString = class(TCoreClassObject)
  private
    FList: TCoreClassList;
    function GetText: SystemString;
    procedure SetText(const Value: SystemString);
  protected
    function GetItems(idx: Integer): TPascalString;
    procedure SetItems(idx: Integer; Value: TPascalString);

    function GetItems_PPascalString(idx: Integer): PPascalString;

    function GetObjects(idx: Integer): TCoreClassObject;
    procedure SetObjects(idx: Integer; Value: TCoreClassObject);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: SystemString): Integer; overload;
    function Add(Value: TPascalString): Integer; overload;
    function Add(Value: SystemString; Obj: TCoreClassObject): Integer; overload;
    function Add(Value: TPascalString; Obj: TCoreClassObject): Integer; overload;
    function Append(Value: SystemString): Integer; overload;
    function Delete(idx: Integer): Integer;
    function DeletePascalString(Value: TPascalString): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: TPascalString): Integer;

    procedure Assign(SameObj: TListPascalString); overload;
    procedure Assign(sour: TCoreClassStrings); overload;
    procedure AssignTo(dest: TCoreClassStrings); overload;

    procedure AddStrings(sour: TListPascalString); overload;
    procedure AddStrings(sour: TCoreClassStrings); overload;

    procedure FillTo(var output: TArrayPascalString); overload;
    procedure FillFrom(const InData: TArrayPascalString);

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromFile(fn: SystemString);
    procedure SaveToFile(fn: SystemString);

    property Text: SystemString read GetText write SetText;

    property Items[idx: Integer]: TPascalString read GetItems write SetItems; default;
    property Items_PPascalString[idx: Integer]: PPascalString read GetItems_PPascalString;
    property Objects[idx: Integer]: TCoreClassObject read GetObjects write SetObjects;

    property List: TCoreClassList read FList;
  end;

  TListVariantData = packed record
    Data: Variant;
  end;

  PListVariantData = ^TListVariantData;

  TListVariant = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(idx: Integer): Variant;
    procedure SetItems(idx: Integer; Value: Variant);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Variant): Integer;
    function Delete(idx: Integer): Integer;
    function DeleteVariant(Value: Variant): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Variant): Integer;
    procedure Assign(SameObj: TListVariant);

    property Items[idx: Integer]: Variant read GetItems write SetItems; default;
  end;

  TVariantToDataListData = packed record
    ID: Variant;
    Data: Pointer;
  end;

  PVariantToDataListData = ^TVariantToDataListData;

  TVariantToDataList = class(TCoreClassObject)
  private
    FList: TCoreClassList;
    FAutoFreeData: Boolean;
    FOnFreePtr: TOnPtr;
  protected
    function GetItems(ID: Variant): Pointer;
    procedure SetItems(ID: Variant; Value: Pointer);
    procedure DefaultDataFreeProc(p: Pointer);
    procedure DoDataFreeProc(p: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(ID: Variant; Data: Pointer): Boolean;
    function Delete(ID: Variant): Boolean;
    procedure Clear;
    function Exists(ID: Variant): Boolean;
    procedure GetList(_To: TListVariant);
    function Count: Integer;

{$IFNDEF FPC} property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData; {$ENDIF}
    property Items[ID: Variant]: Pointer read GetItems write SetItems; default;
    property OnFreePtr: TOnPtr read FOnFreePtr write FOnFreePtr;
  end;

  TVariantToVariantListData = packed record
    v: Variant;
  end;

  PVariantToVariantListData = ^TVariantToVariantListData;

  TVariantToVariantList = class(TCoreClassObject)
  private
    FList: TVariantToDataList;
  protected
    function GetItems(ID: Variant): Variant;
    procedure SetItems(ID: Variant; Value: Variant);
    procedure DefaultDataFreeProc(p: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(ID, AValue: Variant): Boolean;
    function Delete(ID: Variant): Boolean;
    procedure Clear;
    function Exists(ID: Variant): Boolean;
    procedure GetList(_To: TListVariant);
    procedure GetValueList(_To: TListVariant);
    function Count: Integer;
    procedure Assign(SameObj: TVariantToVariantList);

    property Items[ID: Variant]: Variant read GetItems write SetItems; default;
  end;

  TVariantToObjectListData = packed record
    Obj: TCoreClassObject;
  end;

  PVariantToObjectListData = ^TVariantToObjectListData;

  TVariantToObjectList = class(TCoreClassObject)
  private
    FList: TVariantToDataList;
  protected
    function GetItems(ID: Variant): TCoreClassObject;
    procedure SetItems(ID: Variant; Value: TCoreClassObject);
    procedure DefaultDataFreeProc(p: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(ID: Variant; Obj: TCoreClassObject): Boolean;
    function Delete(ID: Variant): Boolean;
    procedure Clear;
    function Exists(ID: Variant): Boolean;
    procedure GetList(_To: TListVariant);
    function Count: Integer;
    procedure Assign(SameObj: TVariantToObjectList);

    property Items[ID: Variant]: TCoreClassObject read GetItems write SetItems; default;
  end;

  TBackcalls            = class;
  TBackcallNotifyCall   = procedure(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
  TBackcallNotifyMethod = procedure(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant) of object;

{$IFNDEF FPC} TBackcallNotifyProc = reference to procedure(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant); {$ENDIF}
  PBackcallData                   = ^TBackcallData;

  TBackcallData = packed record
    FlagObject: TCoreClassObject;
    NotifyCall: TBackcallNotifyCall;
    NotifyMethod: TBackcallNotifyMethod;
{$IFNDEF FPC} NotifyProc: TBackcallNotifyProc; {$ENDIF}
    procedure Init;
  end;

  TBackcalls = class(TCoreClassObject)
  private
    FList: TCoreClassList;
    FVariantList: THashVariantList;
    FObjectList: THashObjectList;
    FOwner: TCoreClassObject;

    function GetVariantList: THashVariantList;
    function GetObjectList: THashObjectList;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterBackcall(AFlagObject: TCoreClassObject; ANotifyCall: TBackcallNotifyCall); overload;
    procedure RegisterBackcall(AFlagObject: TCoreClassObject; ANotifyMethod: TBackcallNotifyMethod); overload;
{$IFNDEF FPC} procedure RegisterBackcall(AFlagObject: TCoreClassObject; ANotifyProc: TBackcallNotifyProc); overload; {$ENDIF}
    procedure UnRegisterBackcall(AFlagObject: TCoreClassObject);

    procedure Clear;

    procedure ExecuteBackcall(TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);

    property VariantList: THashVariantList read GetVariantList;
    property ObjectList: THashObjectList read GetObjectList;
    property Owner: TCoreClassObject read FOwner write FOwner;
  end;

  // fast hash support
function MakeHash(var s: SystemString): THash; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeHash(var s: TPascalString): THash; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeHash(var i64: Int64): THash; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeHash(var c32: Cardinal): THash; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeHash(var p: Pointer): THash; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

implementation

uses Math,
{$IFDEF FPC}
  streamex,
{$ENDIF FPC}
  MemoryStream64, DoStatusIO, UnicodeMixedLib;

function MakeHash(var s: SystemString): THash;
begin
  Result := FastHashSystemString(@s);
  Result := umlCRC32(@Result, SizeOf(THash));
end;

function MakeHash(var s: TPascalString): THash;
begin
  Result := FastHashPascalString(@s);
  Result := umlCRC32(@Result, SizeOf(THash));
end;

function MakeHash(var i64: Int64): THash;
begin
  Result := umlCRC32(@i64, C_Int64_Size);
end;

function MakeHash(var c32: Cardinal): THash;
begin
  Result := umlCRC32(@c32, C_Cardinal_Size);
end;

function MakeHash(var p: Pointer): THash;
begin
  Result := umlCRC32(@p, C_Pointer_Size);
end;

function THashList.GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
var
  i: Integer;
begin
  i := hash mod length(FListBuffer);

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCoreClassList.Create;
  Result := FListBuffer[i];
end;

function THashList.GetKeyData(const Name: SystemString): PHashListData;
var
  lName: SystemString;
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PHashListData;
begin
  Result := nil;
  if FIgnoreCase then
      lName := LowerCase(Name)
  else
      lName := Name;
  newhash := MakeHash(lName);
  lst := GetListTable(newhash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PHashListData(lst[i]);
        if (newhash = pData^.qHash) and (lName = pData^.LowerCaseName) then
          begin
            Result := pData;

            if (FAccessOptimization) and (pData^.ID < FIDCounter - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDCounter;
                DoAdd(pData);

                if FIDCounter > FIDCounter + 1 then
                  // rebuild idcounter
                    RebuildIDCounter
                else
                    Inc(FIDCounter);
              end;

            Exit;
          end;
      end;
end;

function THashList.GetKeyValue(const Name: SystemString): Pointer;
var
  p: PHashListData;
begin
  p := GetKeyData(Name);
  if p <> nil then
      Result := p^.Data
  else
      Result := nil;
end;

procedure THashList.RebuildIDCounter;
var
  i: Integer;
  p: PHashListData;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      Inc(i);
      p := p^.Next;
    end;

  FIDCounter := i + 1;
end;

procedure THashList.DoAdd(p: PHashListData);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure THashList.DoDelete(p: PHashListData);
var
  FP, np: PHashListData;
begin
  FP := p^.Prev;
  np := p^.Next;

  if p = FFirst then
      FFirst := np;
  if p = FLast then
      FLast := FP;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
      Exit;
    end;

  FP^.Next := np;
  np^.Prev := FP;

  p^.Prev := nil;
  p^.Next := nil;
end;

procedure THashList.DefaultDataFreeProc(p: Pointer);
begin
{$IFDEF FPC}
{$ELSE}
  Dispose(p);
{$ENDIF}
end;

procedure THashList.DoDataFreeProc(p: Pointer);
begin
  if p <> nil then
      FOnFreePtr(p);
end;

constructor THashList.Create;
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAutoFreeData := False;
  FIgnoreCase := True;
  FAccessOptimization := False;

  FOnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FFirst := nil;
  FLast := nil;
  FMaxNameLen := -1;
  FMinNameLen := -1;

  SetLength(FListBuffer, 0);
  SetHashBlockCount(64);
end;

constructor THashList.Create(hashBlockCount: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAutoFreeData := False;
  FIgnoreCase := True;
  FAccessOptimization := False;

  FOnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FFirst := nil;
  FLast := nil;
  FMaxNameLen := -1;
  FMinNameLen := -1;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(hashBlockCount);
end;

destructor THashList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure THashList.Clear;
var
  i: Integer;
  J: Integer;
  lst: TCoreClassList;
  pData: PHashListData;
begin
  FCount := 0;
  FIDCounter := 0;
  FFirst := nil;
  FLast := nil;
  FMaxNameLen := -1;
  FMinNameLen := -1;
  if length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for J := lst.Count - 1 downto 0 do
                begin
                  pData := lst.Items[J];
                  try
                    if (FAutoFreeData) and (pData^.Data <> nil) then
                        DoDataFreeProc(pData^.Data);
                    Dispose(pData);
                  except
                  end;
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure THashList.GetNameList(var output: TArrayPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  SetLength(output, Count);
  if FCount > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          output[i] := p^.OriginName;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.GetNameList(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList.Add(p^.OriginName);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.GetNameList(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList.Add(p^.OriginName);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.GetListData(OutputList: TCoreClassList);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.Delete(const Name: SystemString);
var
  newhash: THash;
  i: Integer;
  lName: SystemString;
  lst: TCoreClassList;
  _ItemData: PHashListData;
begin
  if FCount = 0 then
      Exit;
  if FIgnoreCase then
      lName := LowerCase(Name)
  else
      lName := Name;
  newhash := MakeHash(lName);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst.Items[i];
          if (newhash = _ItemData^.qHash) and (lName = _ItemData^.LowerCaseName) then
            begin
              DoDelete(_ItemData);
              if (FAutoFreeData) and (_ItemData^.Data <> nil) then
                begin
                  try
                    DoDataFreeProc(_ItemData^.Data);
                    _ItemData^.Data := nil;
                  except
                  end;
                end;
              Dispose(_ItemData);
              lst.Delete(i);
              Dec(FCount);
            end
          else
              Inc(i);
        end;
    end;

  if FCount = 0 then
    begin
      FIDCounter := 1;
      FMaxNameLen := -1;
      FMinNameLen := -1;
    end;
end;

procedure THashList.Add(const Name: SystemString; _CustomData: Pointer; const overwrite: Boolean);
var
  newhash: THash;
  L: nativeInt;
  lst: TCoreClassList;
  i: Integer;
  lName: SystemString;
  pData: PHashListData;
begin
  if FIgnoreCase then
      lName := LowerCase(Name)
  else
      lName := Name;
  newhash := MakeHash(lName);

  L := length(lName);
  if Count > 0 then
    begin
      if L > FMaxNameLen then
          FMaxNameLen := L;
      if L < FMinNameLen then
          FMinNameLen := L;
    end
  else
    begin
      FMaxNameLen := L;
      FMinNameLen := L;
    end;

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) and (overwrite) then
    begin
      for i := 0 to lst.Count - 1 do
        begin
          pData := PHashListData(lst.Items[i]);
          if (newhash = pData^.qHash) and (lName = pData^.LowerCaseName) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> _CustomData) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := _CustomData;

              DoAdd(pData);

              if (pData^.ID < FIDCounter - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDCounter;

                  if FIDCounter > FIDCounter + 1 then
                    // rebuild idcounter
                      RebuildIDCounter
                  else
                      Inc(FIDCounter);
                end;

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := newhash;
  pData^.LowerCaseName := lName;
  pData^.OriginName := Name;
  pData^.Data := _CustomData;
  pData^.ID := FIDCounter;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Inc(FCount);

  DoAdd(pData);

  if FIDCounter > FIDCounter + 1 then
    // rebuild idcounter
      RebuildIDCounter
  else
      Inc(FIDCounter);

end;

procedure THashList.Add(const Name: SystemString; _CustomData: Pointer);
begin
  Add(Name, _CustomData, True);
end;

procedure THashList.SetValue(const Name: SystemString; const _CustomData: Pointer);
var
  newhash: THash;
  L: nativeInt;
  lst: TCoreClassList;
  i: Integer;
  lName: SystemString;
  pData: PHashListData;
begin
  if FIgnoreCase then
      lName := LowerCase(Name)
  else
      lName := Name;
  newhash := MakeHash(lName);

  L := length(lName);
  if Count > 0 then
    begin
      if L > FMaxNameLen then
          FMaxNameLen := L;
      if L < FMinNameLen then
          FMinNameLen := L;
    end
  else
    begin
      FMaxNameLen := L;
      FMinNameLen := L;
    end;

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) then
    begin
      for i := 0 to lst.Count - 1 do
        begin
          pData := PHashListData(lst.Items[i]);
          if (newhash = pData^.qHash) and (lName = pData^.LowerCaseName) then
            begin
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> _CustomData) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := _CustomData;
            end;
        end;
    end;
end;

function THashList.Find(const Name: SystemString): Pointer;
var
  i: Integer;
  J: Integer;
  lst: TCoreClassList;
  pData: PHashListData;
begin
  Result := nil;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for J := lst.Count - 1 downto 0 do
                begin
                  pData := PHashListData(lst.Items[J]);
                  if (umlMultipleMatch(True, Name, pData^.OriginName)) then
                    begin
                      Result := pData^.Data;
                      Exit;
                    end;
                end;
            end;
        end;
    end;
end;

function THashList.Exists(const Name: SystemString): Boolean;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PHashListData;
  lName: SystemString;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  if FIgnoreCase then
      lName := LowerCase(Name)
  else
      lName := Name;
  newhash := MakeHash(lName);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PHashListData(lst.Items[i]);
            if (newhash = pData^.qHash) and (lName = pData^.LowerCaseName) then
                Exit(True);
          end;
    end;
end;

procedure THashList.SetHashBlockCount(cnt: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, cnt);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function THashList.First: Pointer;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

function THashList.Last: Pointer;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := nil;
end;

function THashList.GetNext(const Name: SystemString): Pointer;
var
  p: PHashListData;
begin
  Result := nil;
  p := GetKeyData(Name);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function THashList.GetPrev(const Name: SystemString): Pointer;
var
  p: PHashListData;
begin
  Result := nil;
  p := GetKeyData(Name);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function THashList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure THashList.PrintHashReport;
var
  i: nativeInt;
  L: TCoreClassList;
  Total: nativeInt;
  usaged, aMax, aMin: nativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  Total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          Inc(usaged);
          Total := Total + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, Total, aMax, aMin]));
end;

function TInt64HashObjectList.GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
var
  i: Integer;
begin
  i := hash mod length(FListBuffer);

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCoreClassList.Create;
  Result := FListBuffer[i];
end;

function TInt64HashObjectList.Geti64Data(i64: Int64): PInt64HashListObjectStruct;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PInt64HashListObjectStruct;
begin
  Result := nil;
  newhash := MakeHash(i64);
  lst := GetListTable(newhash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PInt64HashListObjectStruct(lst[i]);
        if (newhash = pData^.qHash) and (i64 = pData^.i64) then
          begin
            Result := pData;
            if (FAccessOptimization) and (pData^.ID < FIDCounter - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDCounter;
                DoAdd(pData);

                if FIDCounter > FIDCounter + 1 then
                  // rebuild idcounter
                    RebuildIDCounter
                else
                    Inc(FIDCounter);
              end;
            Exit;
          end;
      end;
end;

function TInt64HashObjectList.Geti64Val(i64: Int64): TCoreClassObject;
var
  p: PInt64HashListObjectStruct;
begin
  p := Geti64Data(i64);
  if p <> nil then
      Result := p^.Data
  else
      Result := nil;
end;

procedure TInt64HashObjectList.RebuildIDCounter;
var
  i: Integer;
  p: PInt64HashListObjectStruct;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      Inc(i);
      p := p^.Next;
    end;

  FIDCounter := i + 1;
end;

procedure TInt64HashObjectList.DoAdd(p: PInt64HashListObjectStruct);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure TInt64HashObjectList.DoDelete(p: PInt64HashListObjectStruct);
var
  FP, np: PInt64HashListObjectStruct;
begin
  FP := p^.Prev;
  np := p^.Next;

  if p = FFirst then
      FFirst := np;
  if p = FLast then
      FLast := FP;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
      Exit;
    end;

  FP^.Next := np;
  np^.Prev := FP;

  p^.Prev := nil;
  p^.Next := nil;
end;

procedure TInt64HashObjectList.DefaultObjectFreeProc(Obj: TCoreClassObject);
begin
  DisposeObject(Obj);
end;

procedure TInt64HashObjectList.DoDataFreeProc(Obj: TCoreClassObject);
begin
  if Obj <> nil then
      FOnObjectFreeProc(Obj);
end;

constructor TInt64HashObjectList.Create;
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FOnObjectFreeProc := {$IFDEF FPC}@{$ENDIF FPC}DefaultObjectFreeProc;
  FFirst := nil;
  FLast := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(256);
end;

constructor TInt64HashObjectList.Create(hashBlockCount: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FOnObjectFreeProc := {$IFDEF FPC}@{$ENDIF FPC}DefaultObjectFreeProc;
  FFirst := nil;
  FLast := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(hashBlockCount);
end;

destructor TInt64HashObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TInt64HashObjectList.Clear;
var
  i: Integer;
  J: Integer;
  lst: TCoreClassList;
  pData: PInt64HashListObjectStruct;
begin
  FCount := 0;
  FIDCounter := 0;
  FFirst := nil;
  FLast := nil;

  if length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for J := lst.Count - 1 downto 0 do
                begin
                  pData := lst.Items[J];
                  try
                    if (FAutoFreeData) and (pData^.Data <> nil) then
                        DoDataFreeProc(pData^.Data);
                    Dispose(pData);
                  except
                  end;
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure TInt64HashObjectList.GetListData(OutputList: TCoreClassList);
var
  i: Integer;
  p: PInt64HashListObjectStruct;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashObjectList.Delete(i64: Int64);
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  _ItemData: PInt64HashListObjectStruct;
begin
  if FCount = 0 then
      Exit;
  newhash := MakeHash(i64);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst.Items[i];
          if (newhash = _ItemData^.qHash) and (i64 = _ItemData^.i64) then
            begin
              DoDelete(_ItemData);
              if (FAutoFreeData) and (_ItemData^.Data <> nil) then
                begin
                  try
                    DoDataFreeProc(_ItemData^.Data);
                    _ItemData^.Data := nil;
                  except
                  end;
                end;
              Dispose(_ItemData);
              lst.Delete(i);
              Dec(FCount);
            end
          else
              Inc(i);
        end;
    end;

  if FCount = 0 then
      FIDCounter := 1;
end;

function TInt64HashObjectList.Add(i64: Int64; _CustomData: TCoreClassObject; const overwrite: Boolean): PInt64HashListObjectStruct;
var
  newhash: THash;
  lst: TCoreClassList;
  i: Integer;
  pData: PInt64HashListObjectStruct;
begin
  newhash := MakeHash(i64);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) and (overwrite) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListObjectStruct(lst.Items[i]);
          if (newhash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> _CustomData) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := _CustomData;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDCounter - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDCounter;

                  if FIDCounter > FIDCounter + 1 then
                    // rebuild idcounter
                      RebuildIDCounter
                  else
                      Inc(FIDCounter);
                end;

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := newhash;
  pData^.i64 := i64;
  pData^.Data := _CustomData;
  pData^.ID := FIDCounter;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  Inc(FCount);
  DoAdd(pData);

  if FIDCounter > FIDCounter + 1 then
    // rebuild idcounter
      RebuildIDCounter
  else
      Inc(FIDCounter);
end;

procedure TInt64HashObjectList.SetValue(i64: Int64; _CustomData: TCoreClassObject);
var
  newhash: THash;
  lst: TCoreClassList;
  i: Integer;
  pData: PInt64HashListObjectStruct;
begin
  newhash := MakeHash(i64);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListObjectStruct(lst.Items[i]);
          if (newhash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> _CustomData) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := _CustomData;
            end;
        end;
    end;
end;

function TInt64HashObjectList.Exists(i64: Int64): Boolean;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PInt64HashListObjectStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  newhash := MakeHash(i64);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PInt64HashListObjectStruct(lst.Items[i]);
            if (newhash = pData^.qHash) and (i64 = pData^.i64) then
                Exit(True);
          end;
    end;
end;

procedure TInt64HashObjectList.SetHashBlockCount(cnt: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, cnt);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

procedure TInt64HashObjectList.DeleteFirst;
begin
  if FFirst <> nil then
      Delete(FFirst^.i64);
end;

procedure TInt64HashObjectList.DeleteLast;
begin
  if FLast <> nil then
      Delete(FLast^.i64);
end;

function TInt64HashObjectList.First: TCoreClassObject;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

function TInt64HashObjectList.Last: TCoreClassObject;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := nil;
end;

function TInt64HashObjectList.GetNext(i64: Int64): TCoreClassObject;
var
  p: PInt64HashListObjectStruct;
begin
  Result := nil;
  p := Geti64Data(i64);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function TInt64HashObjectList.GetPrev(i64: Int64): TCoreClassObject;
var
  p: PInt64HashListObjectStruct;
begin
  Result := nil;
  p := Geti64Data(i64);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function TInt64HashObjectList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TInt64HashObjectList.PrintHashReport;
var
  i: nativeInt;
  L: TCoreClassList;
  Total: nativeInt;
  usaged, aMax, aMin: nativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  Total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          Inc(usaged);
          Total := Total + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, Total, aMax, aMin]));
end;

function TInt64HashPointerList.GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
var
  i: Integer;
begin
  i := hash mod length(FListBuffer);

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCoreClassList.Create;
  Result := FListBuffer[i];
end;

function TInt64HashPointerList.Geti64Data(i64: Int64): PInt64HashListPointerStruct;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PInt64HashListPointerStruct;
begin
  Result := nil;
  newhash := MakeHash(i64);
  lst := GetListTable(newhash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PInt64HashListPointerStruct(lst[i]);
        if (newhash = pData^.qHash) and (i64 = pData^.i64) then
          begin
            Result := pData;
            if (FAccessOptimization) and (pData^.ID < FIDCounter - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDCounter;
                DoAdd(pData);

                if FIDCounter > FIDCounter + 1 then
                  // rebuild idcounter
                    RebuildIDCounter
                else
                    Inc(FIDCounter);
              end;
            Exit;
          end;
      end;
end;

function TInt64HashPointerList.Geti64Val(i64: Int64): Pointer;
var
  p: PInt64HashListPointerStruct;
begin
  p := Geti64Data(i64);
  if p <> nil then
      Result := p^.Data
  else
      Result := nil;
end;

procedure TInt64HashPointerList.RebuildIDCounter;
var
  i: Integer;
  p: PInt64HashListPointerStruct;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      Inc(i);
      p := p^.Next;
    end;

  FIDCounter := i + 1;
end;

procedure TInt64HashPointerList.DoAdd(p: PInt64HashListPointerStruct);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure TInt64HashPointerList.DoDelete(p: PInt64HashListPointerStruct);
var
  FP, np: PInt64HashListPointerStruct;
begin
  FP := p^.Prev;
  np := p^.Next;

  if p = FFirst then
      FFirst := np;
  if p = FLast then
      FLast := FP;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
      Exit;
    end;

  FP^.Next := np;
  np^.Prev := FP;

  p^.Prev := nil;
  p^.Next := nil;
end;

procedure TInt64HashPointerList.DefaultDataFreeProc(p: Pointer);
begin
{$IFDEF FPC}
{$ELSE}
  Dispose(p);
{$ENDIF}
end;

procedure TInt64HashPointerList.DoDataFreeProc(p: Pointer);
begin
  if p <> nil then
      FOnFreePtr(p);
end;

procedure TInt64HashPointerList.DoAddDataNotifyProc(p: Pointer);
begin
  if Assigned(FOnAddPtr) then
      FOnAddPtr(p);
end;

constructor TInt64HashPointerList.Create;
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FFirst := nil;
  FLast := nil;
  FOnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FOnAddPtr := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(256);
end;

constructor TInt64HashPointerList.Create(hashBlockCount: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FFirst := nil;
  FLast := nil;
  FOnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FOnAddPtr := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(hashBlockCount);
end;

destructor TInt64HashPointerList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TInt64HashPointerList.Clear;
var
  i: Integer;
  J: Integer;
  lst: TCoreClassList;
  pData: PInt64HashListPointerStruct;
begin
  FCount := 0;
  FIDCounter := 0;
  FFirst := nil;
  FLast := nil;

  if length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for J := lst.Count - 1 downto 0 do
                begin
                  pData := lst.Items[J];
                  try
                    if (FAutoFreeData) and (pData^.Data <> nil) then
                        DoDataFreeProc(pData^.Data);
                    Dispose(pData);
                  except
                  end;
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure TInt64HashPointerList.GetListData(OutputList: TCoreClassList);
var
  i: Integer;
  p: PInt64HashListPointerStruct;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashPointerList.Delete(i64: Int64);
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  _ItemData: PInt64HashListPointerStruct;
begin
  if FCount = 0 then
      Exit;
  newhash := MakeHash(i64);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst.Items[i];
          if (newhash = _ItemData^.qHash) and (i64 = _ItemData^.i64) then
            begin
              DoDelete(_ItemData);
              if (FAutoFreeData) and (_ItemData^.Data <> nil) then
                begin
                  try
                    DoDataFreeProc(_ItemData^.Data);
                    _ItemData^.Data := nil;
                  except
                  end;
                end;
              Dispose(_ItemData);
              lst.Delete(i);
              Dec(FCount);
            end
          else
              Inc(i);
        end;
    end;

  if FCount = 0 then
      FIDCounter := 1;
end;

function TInt64HashPointerList.Add(i64: Int64; _CustomData: Pointer; const overwrite: Boolean): PInt64HashListPointerStruct;
var
  newhash: THash;
  lst: TCoreClassList;
  i: Integer;
  pData: PInt64HashListPointerStruct;
begin
  newhash := MakeHash(i64);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) and (overwrite) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListPointerStruct(lst.Items[i]);
          if (newhash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> _CustomData) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := _CustomData;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDCounter - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDCounter;

                  if FIDCounter > FIDCounter + 1 then
                    // rebuild idcounter
                      RebuildIDCounter
                  else
                      Inc(FIDCounter);
                end;

              DoAddDataNotifyProc(_CustomData);

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := newhash;
  pData^.i64 := i64;
  pData^.Data := _CustomData;
  pData^.ID := FIDCounter;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  Inc(FCount);
  DoAdd(pData);

  if FIDCounter > FIDCounter + 1 then
    // rebuild idcounter
      RebuildIDCounter
  else
      Inc(FIDCounter);

  DoAddDataNotifyProc(_CustomData);
end;

procedure TInt64HashPointerList.SetValue(i64: Int64; _CustomData: Pointer);
var
  newhash: THash;
  lst: TCoreClassList;
  i: Integer;
  pData: PInt64HashListPointerStruct;
begin
  newhash := MakeHash(i64);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListPointerStruct(lst.Items[i]);
          if (newhash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> _CustomData) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := _CustomData;
              DoAddDataNotifyProc(pData^.Data);
            end;
        end;
    end;
end;

function TInt64HashPointerList.Exists(i64: Int64): Boolean;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PInt64HashListPointerStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  newhash := MakeHash(i64);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PInt64HashListPointerStruct(lst.Items[i]);
            if (newhash = pData^.qHash) and (i64 = pData^.i64) then
                Exit(True);
          end;
    end;
end;

procedure TInt64HashPointerList.SetHashBlockCount(cnt: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, cnt);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function TInt64HashPointerList.First: Pointer;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

function TInt64HashPointerList.Last: Pointer;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := nil;
end;

function TInt64HashPointerList.GetNext(i64: Int64): Pointer;
var
  p: PInt64HashListPointerStruct;
begin
  Result := nil;
  p := Geti64Data(i64);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function TInt64HashPointerList.GetPrev(i64: Int64): Pointer;
var
  p: PInt64HashListPointerStruct;
begin
  Result := nil;
  p := Geti64Data(i64);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function TInt64HashPointerList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TInt64HashPointerList.PrintHashReport;
var
  i: nativeInt;
  L: TCoreClassList;
  Total: nativeInt;
  usaged, aMax, aMin: nativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  Total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          Inc(usaged);
          Total := Total + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, Total, aMax, aMin]));
end;

function TUInt32HashObjectList.GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
var
  i: Integer;
begin
  i := hash mod length(FListBuffer);

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCoreClassList.Create;
  Result := FListBuffer[i];
end;

function TUInt32HashObjectList.Getu32Data(u32: UInt32): PUInt32HashListObjectStruct;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PUInt32HashListObjectStruct;
begin
  Result := nil;
  newhash := MakeHash(u32);
  lst := GetListTable(newhash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PUInt32HashListObjectStruct(lst[i]);
        if (newhash = pData^.qHash) and (u32 = pData^.u32) then
          begin
            Result := pData;
            if (FAccessOptimization) and (pData^.ID < FIDCounter - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDCounter;
                DoAdd(pData);

                if FIDCounter > FIDCounter + 1 then
                  // rebuild idcounter
                    RebuildIDCounter
                else
                    Inc(FIDCounter);
              end;
            Exit;
          end;
      end;
end;

function TUInt32HashObjectList.Getu32Val(u32: UInt32): TCoreClassObject;
var
  p: PUInt32HashListObjectStruct;
begin
  p := Getu32Data(u32);
  if p <> nil then
      Result := p^.Data
  else
      Result := nil;
end;

procedure TUInt32HashObjectList.RebuildIDCounter;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      Inc(i);
      p := p^.Next;
    end;

  FIDCounter := i + 1;
end;

procedure TUInt32HashObjectList.DoAdd(p: PUInt32HashListObjectStruct);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure TUInt32HashObjectList.DoDelete(p: PUInt32HashListObjectStruct);
var
  FP, np: PUInt32HashListObjectStruct;
begin
  FP := p^.Prev;
  np := p^.Next;

  if p = FFirst then
      FFirst := np;
  if p = FLast then
      FLast := FP;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
      Exit;
    end;

  FP^.Next := np;
  np^.Prev := FP;

  p^.Prev := nil;
  p^.Next := nil;
end;

procedure TUInt32HashObjectList.DoDataFreeProc(Obj: TCoreClassObject);
begin
  DisposeObject(Obj);
end;

constructor TUInt32HashObjectList.Create;
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FFirst := nil;
  FLast := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(256);
end;

constructor TUInt32HashObjectList.Create(hashBlockCount: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FFirst := nil;
  FLast := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(hashBlockCount);
end;

destructor TUInt32HashObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TUInt32HashObjectList.Clear;
var
  i: Integer;
  J: Integer;
  lst: TCoreClassList;
  pData: PUInt32HashListObjectStruct;
begin
  FCount := 0;
  FIDCounter := 0;
  FFirst := nil;
  FLast := nil;

  if length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for J := lst.Count - 1 downto 0 do
                begin
                  pData := lst.Items[J];
                  try
                    if (FAutoFreeData) and (pData^.Data <> nil) then
                        DoDataFreeProc(pData^.Data);
                    Dispose(pData);
                  except
                  end;
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure TUInt32HashObjectList.GetListData(OutputList: TCoreClassList);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashObjectList.Delete(u32: UInt32);
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  _ItemData: PUInt32HashListObjectStruct;
begin
  if FCount = 0 then
      Exit;
  newhash := MakeHash(u32);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst.Items[i];
          if (newhash = _ItemData^.qHash) and (u32 = _ItemData^.u32) then
            begin
              DoDelete(_ItemData);
              if (FAutoFreeData) and (_ItemData^.Data <> nil) then
                begin
                  try
                    DoDataFreeProc(_ItemData^.Data);
                    _ItemData^.Data := nil;
                  except
                  end;
                end;
              Dispose(_ItemData);
              lst.Delete(i);
              Dec(FCount);
            end
          else
              Inc(i);
        end;
    end;

  if FCount = 0 then
      FIDCounter := 1;
end;

function TUInt32HashObjectList.Add(u32: UInt32; _CustomData: TCoreClassObject; const overwrite: Boolean): PUInt32HashListObjectStruct;
var
  newhash: THash;
  lst: TCoreClassList;
  i: Integer;
  pData: PUInt32HashListObjectStruct;
begin
  newhash := MakeHash(u32);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) and (overwrite) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PUInt32HashListObjectStruct(lst.Items[i]);
          if (newhash = pData^.qHash) and (u32 = pData^.u32) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> _CustomData) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := _CustomData;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDCounter - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDCounter;

                  if FIDCounter > FIDCounter + 1 then
                    // rebuild idcounter
                      RebuildIDCounter
                  else
                      Inc(FIDCounter);
                end;

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := newhash;
  pData^.u32 := u32;
  pData^.Data := _CustomData;
  pData^.ID := FIDCounter;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  Inc(FCount);
  DoAdd(pData);

  if FIDCounter > FIDCounter + 1 then
    // rebuild idcounter
      RebuildIDCounter
  else
      Inc(FIDCounter);
end;

procedure TUInt32HashObjectList.SetValue(u32: UInt32; _CustomData: TCoreClassObject);
var
  newhash: THash;
  lst: TCoreClassList;
  i: Integer;
  pData: PUInt32HashListObjectStruct;
begin
  newhash := MakeHash(u32);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PUInt32HashListObjectStruct(lst.Items[i]);
          if (newhash = pData^.qHash) and (u32 = pData^.u32) then
            begin
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> _CustomData) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := _CustomData;
            end;
        end;
    end;
end;

function TUInt32HashObjectList.Exists(u32: UInt32): Boolean;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PUInt32HashListObjectStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  newhash := MakeHash(u32);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PUInt32HashListObjectStruct(lst.Items[i]);
            if (newhash = pData^.qHash) and (u32 = pData^.u32) then
                Exit(True);
          end;
    end;
end;

procedure TUInt32HashObjectList.SetHashBlockCount(cnt: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, cnt);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function TUInt32HashObjectList.First: TCoreClassObject;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

function TUInt32HashObjectList.Last: TCoreClassObject;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := nil;
end;

function TUInt32HashObjectList.GetNext(u32: UInt32): TCoreClassObject;
var
  p: PUInt32HashListObjectStruct;
begin
  Result := nil;
  p := Getu32Data(u32);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function TUInt32HashObjectList.GetPrev(u32: UInt32): TCoreClassObject;
var
  p: PUInt32HashListObjectStruct;
begin
  Result := nil;
  p := Getu32Data(u32);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function TUInt32HashObjectList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TUInt32HashObjectList.Progress(OnProgress: TUInt32HashObjectListLoopCall);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.u32, p^.Data);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashObjectList.Progress(OnProgress: TUInt32HashObjectListLoopMethod);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.u32, p^.Data);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

{$IFNDEF FPC}


procedure TUInt32HashObjectList.Progress(OnProgress: TUInt32HashObjectListLoopProc);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.u32, p^.Data);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;
{$ENDIF}


function TUInt32HashObjectList.ExistsObject(Obj: TCoreClassObject): Boolean;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  Result := False;
  if (FCount > 0) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          if p^.Data = Obj then
            begin
              Result := True;
              Exit;
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashObjectList.PrintHashReport;
var
  i: nativeInt;
  L: TCoreClassList;
  Total: nativeInt;
  usaged, aMax, aMin: nativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  Total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          Inc(usaged);
          Total := Total + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, Total, aMax, aMin]));
end;

function TPointerHashNativeUIntList.GetListTable(hash: THash; AutoCreate: Boolean): TCoreClassList;
var
  i: Integer;
begin
  i := hash mod length(FListBuffer);

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCoreClassList.Create;
  Result := FListBuffer[i];
end;

function TPointerHashNativeUIntList.GetNPtrData(NPtr: Pointer): PPointerHashListNativeUIntStruct;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PPointerHashListNativeUIntStruct;
begin
  Result := nil;
  newhash := MakeHash(NPtr);
  lst := GetListTable(newhash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PPointerHashListNativeUIntStruct(lst[i]);
        if (newhash = pData^.qHash) and (NPtr = pData^.NPtr) then
          begin
            Result := pData;
            if (FAccessOptimization) and (pData^.ID < FIDCounter - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDCounter;
                DoAdd(pData);

                if FIDCounter > FIDCounter + 1 then
                  // rebuild idcounter
                    RebuildIDCounter
                else
                    Inc(FIDCounter);
              end;
            Exit;
          end;
      end;
end;

function TPointerHashNativeUIntList.GetNPtrVal(NPtr: Pointer): nativeUInt;
var
  p: PPointerHashListNativeUIntStruct;
begin
  p := GetNPtrData(NPtr);
  if p <> nil then
      Result := p^.Data
  else
      Result := NullValue;
end;

procedure TPointerHashNativeUIntList.RebuildIDCounter;
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      Inc(i);
      p := p^.Next;
    end;

  FIDCounter := i + 1;
end;

procedure TPointerHashNativeUIntList.DoAdd(p: PPointerHashListNativeUIntStruct);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure TPointerHashNativeUIntList.DoDelete(p: PPointerHashListNativeUIntStruct);
var
  FP, np: PPointerHashListNativeUIntStruct;
begin
  FP := p^.Prev;
  np := p^.Next;

  if p = FFirst then
      FFirst := np;
  if p = FLast then
      FLast := FP;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
      Exit;
    end;

  FP^.Next := np;
  np^.Prev := FP;

  p^.Prev := nil;
  p^.Next := nil;
end;

constructor TPointerHashNativeUIntList.Create;
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAccessOptimization := False;
  FFirst := nil;
  FLast := nil;
  FTotal := 0;
  FMinimizePtr := nil;
  FMaximumPtr := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(256);
end;

constructor TPointerHashNativeUIntList.Create(hashBlockCount: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAccessOptimization := False;
  FFirst := nil;
  FLast := nil;
  FTotal := 0;
  FMinimizePtr := nil;
  FMaximumPtr := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(hashBlockCount);
end;

destructor TPointerHashNativeUIntList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TPointerHashNativeUIntList.Clear;
var
  i: Integer;
  J: Integer;
  lst: TCoreClassList;
  pData: PPointerHashListNativeUIntStruct;
begin
  FCount := 0;
  FIDCounter := 0;
  FFirst := nil;
  FLast := nil;
  FTotal := 0;
  FMinimizePtr := nil;
  FMaximumPtr := nil;

  if length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for J := lst.Count - 1 downto 0 do
                begin
                  pData := lst.Items[J];
                  Dispose(pData);
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure TPointerHashNativeUIntList.FastClear;
var
  i: Integer;
  J: Integer;
  lst: TCoreClassList;
  pData: PPointerHashListNativeUIntStruct;
begin
  FCount := 0;
  FIDCounter := 0;
  FFirst := nil;
  FLast := nil;
  FTotal := 0;
  FMinimizePtr := nil;
  FMaximumPtr := nil;

  if length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for J := lst.Count - 1 downto 0 do
                begin
                  pData := lst.Items[J];
                  Dispose(pData);
                end;
              lst.Clear;
            end;
        end;
    end;
end;

procedure TPointerHashNativeUIntList.GetListData(OutputList: TCoreClassList);
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

function TPointerHashNativeUIntList.Delete(NPtr: Pointer): Boolean;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  _ItemData: PPointerHashListNativeUIntStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  newhash := MakeHash(NPtr);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst.Items[i];
          if (newhash = _ItemData^.qHash) and (NPtr = _ItemData^.NPtr) then
            begin
              Dec(FTotal, _ItemData^.Data);
              DoDelete(_ItemData);
              Dispose(_ItemData);
              lst.Delete(i);
              Dec(FCount);
              Result := True;
            end
          else
              Inc(i);
        end;
    end;

  if FCount = 0 then
    begin
      FIDCounter := 1;
      FTotal := 0;
      FMinimizePtr := nil;
      FMaximumPtr := nil;
    end;
end;

function TPointerHashNativeUIntList.Add(NPtr: Pointer; _CustomData: nativeUInt; const overwrite: Boolean): PPointerHashListNativeUIntStruct;
var
  newhash: THash;
  lst: TCoreClassList;
  i: Integer;
  pData: PPointerHashListNativeUIntStruct;
begin
  newhash := MakeHash(NPtr);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) and (overwrite) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PPointerHashListNativeUIntStruct(lst.Items[i]);
          if (newhash = pData^.qHash) and (NPtr = pData^.NPtr) then
            begin
              Dec(FTotal, pData^.Data);
              DoDelete(pData);
              pData^.Data := _CustomData;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDCounter - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDCounter;

                  if FIDCounter > FIDCounter + 1 then
                    // rebuild idcounter
                      RebuildIDCounter
                  else
                      Inc(FIDCounter);
                end;

              Inc(FTotal, pData^.Data);
              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := newhash;
  pData^.NPtr := NPtr;
  pData^.Data := _CustomData;
  pData^.ID := FIDCounter;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  Inc(FCount);
  DoAdd(pData);

  if FIDCounter > FIDCounter + 1 then
    // rebuild idcounter
      RebuildIDCounter
  else
      Inc(FIDCounter);

  Inc(FTotal, pData^.Data);

  if (nativeUInt(NPtr) < nativeUInt(FMinimizePtr)) or (FMinimizePtr = nil) then
      FMinimizePtr := NPtr;
  if (nativeUInt(NPtr) > nativeUInt(FMaximumPtr)) or (FMaximumPtr = nil) then
      FMaximumPtr := NPtr;
end;

procedure TPointerHashNativeUIntList.SetValue(NPtr: Pointer; _CustomData: nativeUInt);
var
  newhash: THash;
  lst: TCoreClassList;
  i: Integer;
  pData: PPointerHashListNativeUIntStruct;
begin
  newhash := MakeHash(NPtr);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PPointerHashListNativeUIntStruct(lst.Items[i]);
          if (newhash = pData^.qHash) and (NPtr = pData^.NPtr) then
            begin
              Dec(FTotal, pData^.Data);
              pData^.Data := _CustomData;
              Inc(FTotal, pData^.Data);
            end;
        end;
    end;
end;

function TPointerHashNativeUIntList.Exists(NPtr: Pointer): Boolean;
var
  newhash: THash;
  i: Integer;
  lst: TCoreClassList;
  pData: PPointerHashListNativeUIntStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  newhash := MakeHash(NPtr);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PPointerHashListNativeUIntStruct(lst.Items[i]);
            if (newhash = pData^.qHash) and (NPtr = pData^.NPtr) then
                Exit(True);
          end;
    end;
end;

procedure TPointerHashNativeUIntList.SetHashBlockCount(cnt: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, cnt);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function TPointerHashNativeUIntList.First: nativeUInt;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := NullValue;
end;

function TPointerHashNativeUIntList.Last: nativeUInt;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := NullValue;
end;

function TPointerHashNativeUIntList.GetNext(NPtr: Pointer): nativeUInt;
var
  p: PPointerHashListNativeUIntStruct;
begin
  Result := NullValue;
  p := GetNPtrData(NPtr);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function TPointerHashNativeUIntList.GetPrev(NPtr: Pointer): nativeUInt;
var
  p: PPointerHashListNativeUIntStruct;
begin
  Result := NullValue;
  p := GetNPtrData(NPtr);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function TPointerHashNativeUIntList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TPointerHashNativeUIntList.Progress(OnProgress: TPointerHashNativeUIntListLoopCall);
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.NPtr, p^.Data);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TPointerHashNativeUIntList.Progress(OnProgress: TPointerHashNativeUIntListLoopMethod);
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.NPtr, p^.Data);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

{$IFNDEF FPC}


procedure TPointerHashNativeUIntList.Progress(OnProgress: TPointerHashNativeUIntListLoopProc);
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.NPtr, p^.Data);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;
{$ENDIF}


function TPointerHashNativeUIntList.ExistsNaviveUInt(Obj: nativeUInt): Boolean;
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  Result := False;
  if (FCount > 0) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          if p^.Data = Obj then
            begin
              Result := True;
              Exit;
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TPointerHashNativeUIntList.PrintHashReport;
var
  i: nativeInt;
  L: TCoreClassList;
  T: nativeInt;
  usaged, aMax, aMin: nativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  T := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          Inc(usaged);
          T := T + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, T, aMax, aMin]));
end;

function THashObjectList.GetCount: nativeInt;
begin
  Result := FHashList.Count;
end;

function THashObjectList.GetIgnoreCase: Boolean;
begin
  Result := IgnoreCase;
end;

procedure THashObjectList.SetIgnoreCase(const Value: Boolean);
begin
  FHashList.IgnoreCase := Value;
end;

function THashObjectList.GetKeyValue(const Name: SystemString): TCoreClassObject;
var
  pObjData: PHashObjectListData;
begin
  if Name = '' then
    begin
      Result := nil;
      Exit;
    end;
  pObjData := FHashList.NameValue[Name];
  if pObjData <> nil then
      Result := pObjData^.Obj
  else
      Result := nil;
end;

procedure THashObjectList.SetKeyValue(const Name: SystemString; const Value: TCoreClassObject);
begin
  Add(Name, Value);
end;

function THashObjectList.GetOnChange(const Name: SystemString): THashObjectChangeEvent;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.NameValue[Name];
  if pObjData <> nil then
      Result := pObjData^.OnChnage
  else
      Result := nil;
end;

procedure THashObjectList.SetOnChange(const Name: SystemString; const AValue: THashObjectChangeEvent);
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.NameValue[Name];
  if pObjData = nil then
    begin
      new(pObjData);
      pObjData^.OnChnage := AValue;
      pObjData^.Obj := nil;
      FHashList.Add(Name, pObjData, False);
    end
  else
      pObjData^.OnChnage := AValue;
end;

function THashObjectList.GetAccessOptimization: Boolean;
begin
  Result := FHashList.AccessOptimization;
end;

procedure THashObjectList.SetAccessOptimization(const Value: Boolean);
begin
  FHashList.AccessOptimization := Value;
end;

procedure THashObjectList.DefaultDataFreeProc(p: Pointer);
begin
  Dispose(PHashObjectListData(p));
end;

constructor THashObjectList.Create(_AutoFreeObject: Boolean);
begin
  inherited Create;
  FHashList := THashList.Create;
  FHashList.FAutoFreeData := True;

  FHashList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FAutoFreeObject := _AutoFreeObject;
  FIncremental := 0;
end;

constructor THashObjectList.Create(_AutoFreeObject: Boolean; MaxHashBlock: Integer);
begin
  inherited Create;
  FHashList := THashList.Create(MaxHashBlock);
  FHashList.FAutoFreeData := True;

  FHashList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FAutoFreeObject := _AutoFreeObject;
  FIncremental := 0;
end;

destructor THashObjectList.Destroy;
begin
  Clear;
  DisposeObject(FHashList);
  inherited Destroy;
end;

procedure THashObjectList.Assign(sour: THashObjectList);
var
  i: Integer;
  p: PHashListData;
begin
  Clear;
  if sour.HashList.Count > 0 then
    begin
      i := 0;
      p := sour.HashList.FirstPtr;
      while i < sour.HashList.Count do
        begin
          FastAdd(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.Progress(OnProgress: THashObjectListLoopCall);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(@p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.Progress(OnProgress: THashObjectListLoopMethod);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(@p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

{$IFNDEF FPC}


procedure THashObjectList.Progress(OnProgress: THashObjectListLoopProc);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(@p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;
{$ENDIF}


procedure THashObjectList.Clear;
var
  lst: TCoreClassList;
  pObjData: PHashObjectListData;
  i: Integer;
begin
  if AutoFreeObject then
    begin
      lst := TCoreClassList.Create;
      FHashList.GetListData(lst);
      if lst.Count > 0 then
        for i := 0 to lst.Count - 1 do
          with PHashListData(lst[i])^ do
            begin
              pObjData := Data;
              if pObjData <> nil then
                if pObjData^.Obj <> nil then
                  begin
                    try
                        DisposeObject(pObjData^.Obj);
                    except
                    end;
                  end;
            end;
      DisposeObject(lst);
    end;
  FHashList.Clear;
  FIncremental := 0;
end;

procedure THashObjectList.GetNameList(OutputList: TCoreClassStrings);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.AddObject(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetNameList(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetNameList(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetListData(OutputList: TCoreClassStrings);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.AddObject(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetListData(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetListData(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetAsList(OutputList: TCoreClassListForObj);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(PHashObjectListData(p^.Data)^.Obj);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

function THashObjectList.GetObjAsName(Obj: TCoreClassObject): SystemString;
var
  i: Integer;
  p: PHashListData;
begin
  Result := '';
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          if PHashObjectListData(p^.Data)^.Obj = Obj then
            begin
              Result := p^.OriginName;
              Exit;
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.Delete(const Name: SystemString);
var
  pObjData: PHashObjectListData;
begin
  if AutoFreeObject then
    begin
      pObjData := FHashList.NameValue[Name];
      if pObjData <> nil then
        begin
          if pObjData^.Obj <> nil then
            begin
              try
                DisposeObject(pObjData^.Obj);
                pObjData^.Obj := nil;
              except
              end;
            end;
        end;
    end;
  FHashList.Delete(Name);
end;

function THashObjectList.Add(const Name: SystemString; _Object: TCoreClassObject): TCoreClassObject;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.NameValue[Name];
  if pObjData <> nil then
    begin
      try
        if Assigned(pObjData^.OnChnage) then
            pObjData^.OnChnage(Self, Name, pObjData^.Obj, _Object);
      except
      end;

      if (FAutoFreeObject) and (pObjData^.Obj <> nil) then
        begin
          try
            DisposeObject(pObjData^.Obj);
            pObjData^.Obj := nil;
          except
          end;
        end;
    end
  else
    begin
      new(pObjData);
      pObjData^.OnChnage := nil;
      FHashList.Add(Name, pObjData, False);
    end;

  pObjData^.Obj := _Object;
  Result := _Object;
end;

function THashObjectList.FastAdd(const Name: SystemString; _Object: TCoreClassObject): TCoreClassObject;
var
  pObjData: PHashObjectListData;
begin
  new(pObjData);
  pObjData^.OnChnage := nil;
  FHashList.Add(Name, pObjData, False);

  pObjData^.Obj := _Object;
  Result := _Object;
end;

function THashObjectList.Find(const Name: SystemString): TCoreClassObject;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.Find(Name);
  if pObjData <> nil then
      Result := pObjData^.Obj
  else
      Result := nil;
end;

function THashObjectList.Exists(const Name: SystemString): Boolean;
var
  pObjData: PHashObjectListData;
begin
  if Name <> '' then
    begin
      pObjData := FHashList.NameValue[Name];
      if pObjData <> nil then
          Result := pObjData^.Obj <> nil
      else
          Result := False;
    end
  else
      Result := False;
end;

function THashObjectList.ExistsObject(Obj: TCoreClassObject): Boolean;
var
  lst: TCoreClassList;
  i: Integer;
begin
  Result := False;
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            if PHashObjectListData(Data)^.Obj = Obj then
              begin
                Result := True;
                Break;
              end;
          end;
      end;
  DisposeObject(lst);
end;

procedure THashObjectList.CopyFrom(const Source: THashObjectList);
var
  lst: TCoreClassList;
  pObjData: PHashObjectListData;
  i: Integer;
begin
  lst := TCoreClassList.Create;
  Source.HashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          if Data <> nil then
            begin
              pObjData := Data;
              NameValue[OriginName] := pObjData^.Obj;
            end;
      end;
  DisposeObject(lst);
end;

function THashObjectList.ReName(_OLDName, _NewName: SystemString): Boolean;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.NameValue[_OLDName];
  Result := (_OLDName <> _NewName) and (pObjData <> nil) and (FHashList.NameValue[_NewName] = nil);
  if Result then
    begin
      Add(_NewName, pObjData^.Obj);
      FHashList.Delete(_OLDName);
    end;
end;

function THashObjectList.MakeName: SystemString;
begin
  repeat
    Inc(FIncremental);
    Result := IntToStr(FIncremental);
  until not Exists(Result);
end;

function THashObjectList.MakeRefName(RefrenceName: SystemString): SystemString;
begin
  Result := RefrenceName;
  if not Exists(Result) then
      Exit;

  repeat
    Inc(FIncremental);
    Result := RefrenceName + IntToStr(FIncremental);
  until not Exists(Result);
end;

function THashStringList.GetCount: nativeInt;
begin
  Result := FHashList.Count;
end;

function THashStringList.GetIgnoreCase: Boolean;
begin
  Result := IgnoreCase;
end;

procedure THashStringList.SetIgnoreCase(const Value: Boolean);
begin
  FHashList.IgnoreCase := Value;
end;

function THashStringList.GetKeyValue(const Name: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  if Name = '' then
    begin
      Result := Null;
      Exit;
    end;
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
      Result := pVarData^.v
  else
      Result := Null;
end;

procedure THashStringList.SetKeyValue(const Name: SystemString; const Value: SystemString);
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];

  if pVarData = nil then
    begin
      new(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(Name, pVarData, False);
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, Name, Null, Value);
    end
  else
    begin
      if Assigned(pVarData^.OnChnage) then
        begin
          try
              pVarData^.OnChnage(Self, Name, pVarData^.v, Value);
          except
          end;
        end;
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, Name, pVarData^.v, Value);
    end;
  pVarData^.v := Value;
end;

function THashStringList.GetOnChange(const Name: SystemString): THashStringChangeEvent;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
      Result := pVarData^.OnChnage
  else
      Result := nil;
end;

procedure THashStringList.SetOnChange(const Name: SystemString; const AValue: THashStringChangeEvent);
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData = nil then
    begin
      new(pVarData);
      pVarData^.v := Null;
      pVarData^.OnChnage := AValue;
      FHashList.Add(Name, pVarData, False);
    end
  else
      pVarData^.OnChnage := AValue;
end;

function THashStringList.GetAccessOptimization: Boolean;
begin
  Result := FHashList.AccessOptimization;
end;

procedure THashStringList.SetAccessOptimization(const Value: Boolean);
begin
  FHashList.AccessOptimization := Value;
end;

procedure THashStringList.DefaultDataFreeProc(p: Pointer);
begin
  Dispose(PHashStringListData(p));
end;

constructor THashStringList.Create;
begin
  inherited Create;
  FHashList := THashList.Create;
  FHashList.FAutoFreeData := True;

  FHashList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FAutoUpdateDefaultValue := False;
  FOnValueChangeNotify := nil;
end;

constructor THashStringList.Create(MaxHashBlock: Integer);
begin
  inherited Create;
  FHashList := THashList.Create(MaxHashBlock);
  FHashList.FAutoFreeData := True;

  FHashList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FAutoUpdateDefaultValue := False;
  FOnValueChangeNotify := nil;
end;

destructor THashStringList.Destroy;
begin
  DisposeObject(FHashList);
  inherited Destroy;
end;

procedure THashStringList.Assign(sour: THashStringList);
var
  i: Integer;
  p: PHashListData;
begin
  Clear;
  if sour.HashList.Count > 0 then
    begin
      i := 0;
      p := sour.HashList.FirstPtr;
      while i < sour.HashList.Count do
        begin
          FastAdd(p^.OriginName, PHashStringListData(p^.Data)^.v);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.Progress(OnProgress: THashStringListLoopCall);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashStringListData(p^.Data)^.v);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.Progress(OnProgress: THashStringListLoopMethod);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashStringListData(p^.Data)^.v);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

{$IFNDEF FPC}


procedure THashStringList.Progress(OnProgress: THashStringListLoopProc);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashStringListData(p^.Data)^.v);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

{$ENDIF}


procedure THashStringList.Clear;
begin
  FHashList.Clear;
end;

procedure THashStringList.GetNameList(OutputList: TCoreClassStrings);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.GetNameList(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.GetNameList(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.Delete(const Name: SystemString);
begin
  FHashList.Delete(Name);
end;

function THashStringList.Add(const Name: SystemString; v: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, Name, pVarData^.v, v);
      except
      end;
    end
  else
    begin
      new(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(Name, pVarData, True);
    end;

  pVarData^.v := v;
  Result := v;
end;

function THashStringList.FastAdd(const Name: SystemString; v: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  new(pVarData);
  pVarData^.OnChnage := nil;
  FHashList.Add(Name, pVarData, False);

  pVarData^.v := v;
  Result := v;
end;

function THashStringList.Find(const Name: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.Find(Name);
  if pVarData <> nil then
      Result := pVarData^.v
  else
      Result := Null;
end;

function THashStringList.FindValue(const AValue: SystemString): SystemString;
var
  i: Integer;
  lst: TCoreClassList;
  pVarData: PHashStringListData;
begin
  Result := '';
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        pVarData := PHashListData(lst[i])^.Data;
        if umlSameVarValue(AValue, pVarData^.v) then
          begin
            Result := PHashListData(lst[i])^.OriginName;
            Break;
          end;
      end;
  DisposeObject(lst);
end;

function THashStringList.Exists(const Name: SystemString): Boolean;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData = nil then
      Result := False
  else
      Result := not VarIsEmpty(pVarData^.v);
end;

procedure THashStringList.CopyFrom(const Source: THashStringList);
var
  lst: TCoreClassList;
  pVarData: PHashStringListData;
  i: Integer;
begin
  lst := TCoreClassList.Create;
  Source.HashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pVarData := Data;
            NameValue[OriginName] := pVarData^.v;
          end;
      end;
  DisposeObject(lst);
end;

function THashStringList.IncValue(const Name: SystemString; v: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      if pVarData^.v <> '' then
          Result := pVarData^.v + ',' + v;

      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, Name, pVarData^.v, Result);
      except
      end;

      pVarData^.v := Result;
    end
  else
    begin
      Result := v;

      new(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.v := Result;
      FHashList.Add(Name, pVarData, True);
    end;
end;

procedure THashStringList.IncValue(const vl: THashStringList);
var
  lst: TCoreClassList;
  i: Integer;
  p: PHashListData;
begin
  lst := TCoreClassList.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      IncValue(p^.OriginName, PHashStringListData(p^.Data)^.v);
    end;
  DisposeObject(lst);
end;

function THashStringList.GetDefaultValue(const Name: SystemString; AValue: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  try
    if Name = '' then
      begin
        Result := AValue;
        Exit;
      end;
    pVarData := FHashList.NameValue[Name];
    if pVarData <> nil then
      begin
        if (VarIsNull(pVarData^.v)) or (VarIsEmpty(pVarData^.v)) or ((VarIsStr(pVarData^.v)) and (VarToStr(pVarData^.v) = '')) then
          begin
            Result := AValue;
            if FAutoUpdateDefaultValue then
                SetKeyValue(Name, AValue);
          end
        else
          begin
            Result := pVarData^.v;
          end;
      end
    else
      begin
        Result := AValue;
        if FAutoUpdateDefaultValue then
            SetKeyValue(Name, AValue);
      end;
  except
      Result := AValue;
  end;
end;

procedure THashStringList.SetDefaultValue(const Name: SystemString; AValue: SystemString);
begin
  SetKeyValue(Name, AValue);
end;

function THashStringList.ReplaceMacro(const AText, HeadToken, TailToken: SystemString; var output: SystemString): Boolean;
var
  sour: U_String;
  h, T: U_String;
  bPos, ePos: Integer;
  KeyText: SystemString;
  i: Integer;
begin
  output := '';
  sour.Text := AText;
  h.Text := HeadToken;
  T.Text := TailToken;
  Result := True;

  i := 1;

  while i <= sour.Len do
    begin
      if sour.ComparePos(i, h) then
        begin
          bPos := i;
          ePos := sour.GetPos(T, i + h.Len);
          if ePos > 0 then
            begin
              KeyText := sour.Copy(bPos + h.Len, ePos - (bPos + h.Len)).Text;

              if Exists(KeyText) then
                begin
                  output := output + GetKeyValue(KeyText);
                  i := ePos + T.Len;
                  Continue;
                end
              else
                begin
                  Result := False;
                end;
            end;
        end;

      output := output + sour[i];
      Inc(i);
    end;
end;

procedure THashStringList.LoadFromStream(stream: TCoreClassStream);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.LoadFromStream(stream);
  DisposeObject(VT);
end;

procedure THashStringList.SaveToStream(stream: TCoreClassStream);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.SaveToStream(stream);
  DisposeObject(VT);
end;

procedure THashStringList.LoadFromFile(fileName: SystemString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.LoadFromFile(fileName);
  DisposeObject(VT);
end;

procedure THashStringList.SaveToFile(fileName: SystemString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.SaveToFile(fileName);
  DisposeObject(VT);
end;

procedure THashStringList.ExportAsStrings(output: TListPascalString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.DataExport(output);
  DisposeObject(VT);
end;

procedure THashStringList.ImportFromStrings(output: TListPascalString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.DataImport(output);
  DisposeObject(VT);
end;

function THashStringList.GetAsText: SystemString;
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.SaveToText(Result);
  DisposeObject(VT);
end;

procedure THashStringList.SetAsText(const Value: SystemString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.LoadFromText(Value);
  DisposeObject(VT);
end;

function THashStringTextStream.GetKeyValue(AName: SystemString): SystemString;
begin
  if FStringList <> nil then
      Result := FStringList[AName]
  else
      Result := Null;
end;

procedure THashStringTextStream.SetKeyValue(AName: SystemString; const Value: SystemString);
begin
  if FStringList <> nil then
      FStringList[AName] := Value;
end;

constructor THashStringTextStream.Create(_VList: THashStringList);
begin
  inherited Create;
  FStringList := _VList;
end;

destructor THashStringTextStream.Destroy;
begin
  inherited Destroy;
end;

procedure THashStringTextStream.Clear;
begin
  if FStringList <> nil then
      FStringList.Clear;
end;

function THashStringTextStream.VToStr(const v: SystemString): SystemString;
var
  b64: TPascalString;
begin
  if umlExistsLimitChar(v, #10#13#9#8#0) then
    begin
      umlEncodeLineBASE64(v, b64);
      Result := '___base64:' + b64.Text;
    end
  else
      Result := v;
end;

function THashStringTextStream.StrToV(const s: SystemString): SystemString;
var
  n, b64: U_String;
begin
  n := umlTrimSpace(s);
  try
    if n.ComparePos(1, '___base64:') then
      begin
        n := umlDeleteFirstStr(n, ':').Text;
        umlDecodeLineBASE64(n, b64);
        Result := b64.Text;
      end
    else
      begin
        Result := n.Text;
      end;
  except
      Result := n.Text;
  end;
end;

procedure THashStringTextStream.DataImport(TextList: TListPascalString);
var
  i: Integer;
  n: TPascalString;
  TextName, TextValue: TPascalString;
begin
  if FStringList = nil then
      Exit;
  if TextList.Count > 0 then
    for i := 0 to TextList.Count - 1 do
      begin
        n := TextList[i].TrimChar(#32);

        if ((n.Exists(':')) or (n.Exists('='))) and (not CharIn(n.First, [':', '='])) then
          begin
            TextName := umlGetFirstStr_M(n, ':=');
            if TextName.Len > 0 then
              begin
                TextValue := umlDeleteFirstStr_M(n, ':=');
                FStringList[TextName.Text] := StrToV(TextValue.Text);
              end
            else
                FStringList[n.Text] := '';
          end
        else
          begin
            FStringList[n.Text] := '';
          end;
      end;
end;

procedure THashStringTextStream.DataExport(TextList: TListPascalString);
var
  i: Integer;
  vl: TCoreClassList;
  TextValue: SystemString;
begin
  if FStringList = nil then
      Exit;
  vl := TCoreClassList.Create;
  FStringList.HashList.GetListData(vl);
  if vl.Count > 0 then
    for i := 0 to vl.Count - 1 do
      begin
        TextValue := VToStr(PHashStringListData(PHashListData(vl[i])^.Data)^.v);

        if TextValue <> '' then
            TextList.Add((PHashListData(vl[i])^.OriginName + '=' + TextValue))
        else
            TextList.Add(PHashListData(vl[i])^.OriginName);
      end;
  DisposeObject(vl);
end;

procedure THashStringTextStream.LoadFromStream(stream: TCoreClassStream);
var
  n: TListPascalString;
begin
  if FStringList = nil then
      Exit;
  n := TListPascalString.Create;
  n.LoadFromStream(stream);
  DataImport(n);
  DisposeObject(n);
end;

procedure THashStringTextStream.SaveToStream(stream: TCoreClassStream);
var
  n: TListPascalString;
begin
  if FStringList = nil then
      Exit;
  n := TListPascalString.Create;
  DataExport(n);
  n.SaveToStream(stream);
  DisposeObject(n);
end;

procedure THashStringTextStream.LoadFromFile(fileName: SystemString);
var
  ns: TCoreClassStream;
begin
  ns := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashStringTextStream.SaveToFile(fileName: SystemString);
var
  ns: TCoreClassStream;
begin
  ns := TCoreClassFileStream.Create(fileName, fmCreate);
  try
      SaveToStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashStringTextStream.LoadFromText(AText: SystemString);
var
  n: TListPascalString;
begin
  if FStringList = nil then
      Exit;
  n := TListPascalString.Create;
  n.Text := AText;
  DataImport(n);
  DisposeObject(n);
end;

procedure THashStringTextStream.SaveToText(var AText: SystemString);
var
  n: TListPascalString;
begin
  if FStringList = nil then
      Exit;
  n := TListPascalString.Create;
  DataExport(n);
  AText := n.Text;
  DisposeObject(n);
end;

function THashStringTextStream.Text: SystemString;
begin
  SaveToText(Result);
end;

function THashVariantList.GetCount: nativeInt;
begin
  Result := FHashList.Count;
end;

function THashVariantList.GetIgnoreCase: Boolean;
begin
  Result := IgnoreCase;
end;

procedure THashVariantList.SetIgnoreCase(const Value: Boolean);
begin
  FHashList.IgnoreCase := Value;
end;

function THashVariantList.GetKeyValue(const Name: SystemString): Variant;
var
  pVarData: PHashVariantListData;
begin
  if Name = '' then
    begin
      Result := Null;
      Exit;
    end;
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
      Result := pVarData^.v
  else
      Result := Null;
end;

procedure THashVariantList.SetKeyValue(const Name: SystemString; const Value: Variant);
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];

  if pVarData = nil then
    begin
      new(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(Name, pVarData, False);
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, Name, Null, Value);
    end
  else
    begin
      if Assigned(pVarData^.OnChnage) then
        begin
          try
              pVarData^.OnChnage(Self, Name, pVarData^.v, Value);
          except
          end;
        end;
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, Name, pVarData^.v, Value);
    end;
  pVarData^.v := Value;
end;

function THashVariantList.GetOnChange(const Name: SystemString): THashVariantChangeEvent;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
      Result := pVarData^.OnChnage
  else
      Result := nil;
end;

procedure THashVariantList.SetOnChange(const Name: SystemString; const AValue: THashVariantChangeEvent);
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData = nil then
    begin
      new(pVarData);
      pVarData^.v := Null;
      pVarData^.OnChnage := AValue;
      FHashList.Add(Name, pVarData, False);
    end
  else
      pVarData^.OnChnage := AValue;
end;

function THashVariantList.GetAccessOptimization: Boolean;
begin
  Result := FHashList.AccessOptimization;
end;

procedure THashVariantList.SetAccessOptimization(const Value: Boolean);
begin
  FHashList.AccessOptimization := Value;
end;

procedure THashVariantList.DefaultDataFreeProc(p: Pointer);
begin
  Dispose(PHashVariantListData(p));
end;

function THashVariantList.GetI64(const Name: SystemString): Int64;
var
  v: Variant;
begin
  v := GetDefaultValue(Name, 0);
  if VarIsOrdinal(v) then
      Result := v
  else
      Result := 0;
end;

procedure THashVariantList.SetI64(const Name: SystemString; const Value: Int64);
begin
  SetDefaultValue(Name, Value);
end;

function THashVariantList.GetI32(const Name: SystemString): Integer;
var
  v: Variant;
begin
  v := GetDefaultValue(Name, 0);
  if VarIsOrdinal(v) then
      Result := v
  else
      Result := 0;
end;

procedure THashVariantList.SetI32(const Name: SystemString; const Value: Integer);
begin
  SetDefaultValue(Name, Value);
end;

function THashVariantList.GetF(const Name: SystemString): Double;
var
  v: Variant;
begin
  v := GetDefaultValue(Name, 0);
  if VarIsFloat(v) then
      Result := v
  else
      Result := 0;
end;

procedure THashVariantList.SetF(const Name: SystemString; const Value: Double);
begin
  SetDefaultValue(Name, Value);
end;

function THashVariantList.GetS(const Name: SystemString): SystemString;
begin
  Result := VarToStr(GetDefaultValue(Name, ''));
end;

procedure THashVariantList.SetS(const Name, Value: SystemString);
begin
  SetDefaultValue(Name, Value);
end;

constructor THashVariantList.Create;
begin
  inherited Create;
  FHashList := THashList.Create;
  FHashList.FAutoFreeData := True;

  FHashList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FAutoUpdateDefaultValue := False;
  FOnValueChangeNotify := nil;
end;

constructor THashVariantList.Create(MaxHashBlock: Integer);
begin
  inherited Create;
  FHashList := THashList.Create(MaxHashBlock);
  FHashList.FAutoFreeData := True;

  FHashList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FAutoUpdateDefaultValue := False;
  FOnValueChangeNotify := nil;
end;

destructor THashVariantList.Destroy;
begin
  DisposeObject(FHashList);
  inherited Destroy;
end;

procedure THashVariantList.Assign(sour: THashVariantList);
var
  i: Integer;
  p: PHashListData;
begin
  Clear;
  if sour.HashList.Count > 0 then
    begin
      i := 0;
      p := sour.HashList.FirstPtr;
      while i < sour.HashList.Count do
        begin
          FastAdd(p^.OriginName, PHashVariantListData(p^.Data)^.v);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.Progress(OnProgress: THashVariantListLoopCall);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashVariantListData(p^.Data)^.v);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.Progress(OnProgress: THashVariantListLoopMethod);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashVariantListData(p^.Data)^.v);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

{$IFNDEF FPC}


procedure THashVariantList.Progress(OnProgress: THashVariantListLoopProc);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashVariantListData(p^.Data)^.v);
          except
          end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

{$ENDIF}


procedure THashVariantList.Clear;
begin
  FHashList.Clear;
end;

procedure THashVariantList.GetNameList(OutputList: TCoreClassStrings);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.GetNameList(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.GetNameList(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.Delete(const Name: SystemString);
begin
  FHashList.Delete(Name);
end;

function THashVariantList.Add(const Name: SystemString; v: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, Name, pVarData^.v, v);
      except
      end;
    end
  else
    begin
      new(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(Name, pVarData, True);
    end;

  pVarData^.v := v;
  Result := v;
end;

function THashVariantList.FastAdd(const Name: SystemString; v: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  new(pVarData);
  pVarData^.OnChnage := nil;
  FHashList.Add(Name, pVarData, False);

  pVarData^.v := v;
  Result := v;
end;

function THashVariantList.Find(const Name: SystemString): Variant;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Find(Name);
  if pVarData <> nil then
      Result := pVarData^.v
  else
      Result := Null;
end;

function THashVariantList.FindValue(const AValue: Variant): SystemString;
var
  i: Integer;
  lst: TCoreClassList;
  pVarData: PHashVariantListData;
begin
  Result := '';
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        pVarData := PHashListData(lst[i])^.Data;
        if umlSameVarValue(AValue, pVarData^.v) then
          begin
            Result := PHashListData(lst[i])^.OriginName;
            Break;
          end;
      end;
  DisposeObject(lst);
end;

function THashVariantList.Exists(const Name: SystemString): Boolean;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData = nil then
      Result := False
  else
      Result := not VarIsEmpty(pVarData^.v);
end;

procedure THashVariantList.CopyFrom(const Source: THashVariantList);
var
  lst: TCoreClassList;
  pVarData: PHashVariantListData;
  i: Integer;
begin
  lst := TCoreClassList.Create;
  Source.HashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pVarData := Data;
            NameValue[OriginName] := pVarData^.v;
          end;
      end;
  DisposeObject(lst);
end;

function THashVariantList.GetType(const Name: SystemString): Word;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Find(Name);
  if pVarData = nil then
      Result := varEmpty
  else
      Result := VarType(pVarData^.v);
end;

function THashVariantList.IncValue(const Name: SystemString; v: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      if VarIsStr(pVarData^.v) and VarIsStr(v) then
        begin
          if VarToStr(pVarData^.v) <> '' then
              Result := VarToStr(pVarData^.v) + ',' + VarToStr(v)
          else
              Result := VarToStr(pVarData^.v) + VarToStr(v);
        end
      else
        begin
          try
              Result := pVarData^.v + v;
          except
              Result := VarToStr(pVarData^.v) + VarToStr(v);
          end;
        end;

      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, Name, pVarData^.v, Result);
      except
      end;

      pVarData^.v := Result;
    end
  else
    begin
      Result := v;

      new(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.v := Result;
      FHashList.Add(Name, pVarData, True);
    end;
end;

procedure THashVariantList.IncValue(const vl: THashVariantList);
var
  lst: TCoreClassList;
  i: Integer;
  p: PHashListData;
begin
  lst := TCoreClassList.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      IncValue(p^.OriginName, PHashVariantListData(p^.Data)^.v);
    end;
  DisposeObject(lst);
end;

function THashVariantList.SetMax(const Name: SystemString; v: Variant): Variant;
var
  pVarData: PHashVariantListData;
  R: Boolean;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      try
          R := v > pVarData^.v;
      except
          R := True;
      end;

      if R then
        begin
          Result := v;
          try
            if Assigned(pVarData^.OnChnage) then
                pVarData^.OnChnage(Self, Name, pVarData^.v, Result);
          except
          end;

          pVarData^.v := Result;
        end;
    end
  else
    begin
      Result := v;

      new(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.v := Result;
      FHashList.Add(Name, pVarData, True);
    end;
end;

procedure THashVariantList.SetMax(const vl: THashVariantList);
var
  lst: TCoreClassList;
  i: Integer;
  p: PHashListData;
begin
  lst := TCoreClassList.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      SetMax(p^.OriginName, PHashVariantListData(p^.Data)^.v);
    end;
  DisposeObject(lst);
end;

function THashVariantList.SetMin(const Name: SystemString; v: Variant): Variant;
var
  pVarData: PHashVariantListData;
  R: Boolean;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      try
          R := v < pVarData^.v;
      except
          R := True;
      end;

      if R then
        begin
          Result := v;
          try
            if Assigned(pVarData^.OnChnage) then
                pVarData^.OnChnage(Self, Name, pVarData^.v, Result);
          except
          end;

          pVarData^.v := Result;
        end;
    end
  else
    begin
      Result := v;

      new(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.v := Result;
      FHashList.Add(Name, pVarData, True);
    end;
end;

procedure THashVariantList.SetMin(const vl: THashVariantList);
var
  lst: TCoreClassList;
  i: Integer;
  p: PHashListData;
begin
  lst := TCoreClassList.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      SetMin(p^.OriginName, PHashVariantListData(p^.Data)^.v);
    end;
  DisposeObject(lst);
end;

function THashVariantList.GetDefaultValue(const Name: SystemString; AValue: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  try
    if Name = '' then
      begin
        Result := AValue;
        Exit;
      end;
    pVarData := FHashList.NameValue[Name];
    if pVarData <> nil then
      begin
        if (VarIsNull(pVarData^.v)) or (VarIsEmpty(pVarData^.v)) or ((VarIsStr(pVarData^.v)) and (VarToStr(pVarData^.v) = '')) then
          begin
            Result := AValue;
            if FAutoUpdateDefaultValue then
                SetKeyValue(Name, AValue);
          end
        else
          begin
            Result := pVarData^.v;
          end;
      end
    else
      begin
        Result := AValue;
        if FAutoUpdateDefaultValue then
            SetKeyValue(Name, AValue);
      end;
  except
      Result := AValue;
  end;
end;

procedure THashVariantList.SetDefaultValue(const Name: SystemString; AValue: Variant);
begin
  SetKeyValue(Name, AValue);
end;

function THashVariantList.ReplaceMacro(const AText, HeadToken, TailToken: SystemString; var output: SystemString): Boolean;
var
  sour: U_String;
  h, T: U_String;
  bPos, ePos: Integer;
  KeyText: SystemString;
  i: Integer;
begin
  output := '';
  sour.Text := AText;
  h.Text := HeadToken;
  T.Text := TailToken;
  Result := True;

  i := 1;

  while i <= sour.Len do
    begin
      if sour.ComparePos(i, h) then
        begin
          bPos := i;
          ePos := sour.GetPos(T, i + h.Len);
          if ePos > 0 then
            begin
              KeyText := sour.Copy(bPos + h.Len, ePos - (bPos + h.Len)).Text;

              if Exists(KeyText) then
                begin
                  output := output + VarToStr(GetKeyValue(KeyText));
                  i := ePos + T.Len;
                  Continue;
                end
              else
                begin
                  Result := False;
                end;
            end;
        end;

      output := output + sour[i];
      Inc(i);
    end;
end;

procedure THashVariantList.LoadFromStream(stream: TCoreClassStream);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.LoadFromStream(stream);
  DisposeObject(VT);
end;

procedure THashVariantList.SaveToStream(stream: TCoreClassStream);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.SaveToStream(stream);
  DisposeObject(VT);
end;

procedure THashVariantList.LoadFromFile(fileName: SystemString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.LoadFromFile(fileName);
  DisposeObject(VT);
end;

procedure THashVariantList.SaveToFile(fileName: SystemString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.SaveToFile(fileName);
  DisposeObject(VT);
end;

procedure THashVariantList.ExportAsStrings(output: TListPascalString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.DataExport(output);
  DisposeObject(VT);
end;

procedure THashVariantList.ExportAsStrings(output: TCoreClassStrings);
var
  ns: TListPascalString;
begin
  ns := TListPascalString.Create;
  ExportAsStrings(ns);
  ns.AssignTo(output);
  DisposeObject(ns);
end;

procedure THashVariantList.ImportFromStrings(output: TListPascalString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.DataImport(output);
  DisposeObject(VT);
end;

procedure THashVariantList.ImportFromStrings(output: TCoreClassStrings);
var
  ns: TListPascalString;
begin
  ns := TListPascalString.Create;
  ns.Assign(output);
  ImportFromStrings(ns);
  DisposeObject(ns);
end;

function THashVariantList.GetAsText: SystemString;
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.SaveToText(Result);
  DisposeObject(VT);
end;

procedure THashVariantList.SetAsText(const Value: SystemString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.LoadFromText(Value);
  DisposeObject(VT);
end;

function THashVariantTextStream.GetKeyValue(AName: SystemString): Variant;
begin
  if FVariantList <> nil then
      Result := FVariantList[AName]
  else
      Result := Null;
end;

procedure THashVariantTextStream.SetKeyValue(AName: SystemString; const Value: Variant);
begin
  if FVariantList <> nil then
      FVariantList[AName] := Value;
end;

constructor THashVariantTextStream.Create(_VList: THashVariantList);
begin
  inherited Create;
  FVariantList := _VList;
end;

destructor THashVariantTextStream.Destroy;
begin
  inherited Destroy;
end;

procedure THashVariantTextStream.Clear;
begin
  if FVariantList <> nil then
      FVariantList.Clear;
end;

class function THashVariantTextStream.VToStr(const v: Variant): SystemString;
var
  n, b64: U_String;
begin
  try
    case VarType(v) of
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord:
        begin
          Result := IntToStr(v);
        end;
      varInt64:
        begin
          Result := IntToStr(Int64(v));
        end;
      varUInt64:
        begin
{$IFDEF FPC}
          Result := IntToStr(UInt64(v));
{$ELSE}
          Result := UIntToStr(UInt64(v));
{$ENDIF}
        end;
      varSingle, varDouble, varCurrency, varDate:
        begin
          Result := FloatToStr(v);
        end;
      varOleStr, varString, varUString:
        begin
          n.Text := VarToStr(v);

          if umlExistsLimitChar(n, #10#13#9#8#0) then
            begin
              umlEncodeLineBASE64(n, b64);
              Result := '___base64:' + b64.Text;
            end
          else
              Result := n.Text;
        end;
      varBoolean:
        begin
          Result := BoolToStr(v, True);
        end;
      else
        Result := VarToStr(v);
    end;
  except
    try
        Result := VarToStr(v);
    except
        Result := '';
    end;
  end;
end;

class function THashVariantTextStream.StrToV(const s: SystemString): Variant;
var
  n, b64: U_String;
begin
  n := umlTrimSpace(s);
  try
    if n.ComparePos(1, '___base64:') then
      begin
        n := umlDeleteFirstStr(n, ':').Text;
        umlDecodeLineBASE64(n, b64);
        Result := b64.Text;
      end
    else
      begin
        case umlGetNumTextType(n) of
          ntBool: Result := StrToBool(n.Text);
          ntInt: Result := StrToInt(n.Text);
          ntInt64: Result := StrToInt64(n.Text);
{$IFDEF FPC}
          ntUInt64: Result := StrToQWord(n.Text);
{$ELSE}
          ntUInt64: Result := StrToUInt64(n.Text);
{$ENDIF}
          ntWord: Result := StrToInt(n.Text);
          ntByte: Result := StrToInt(n.Text);
          ntSmallInt: Result := StrToInt(n.Text);
          ntShortInt: Result := StrToInt(n.Text);
          ntUInt: Result := StrToInt(n.Text);
          ntSingle: Result := StrToFloat(n.Text);
          ntDouble: Result := StrToFloat(n.Text);
          ntCurrency: Result := StrToFloat(n.Text);
          else Result := n.Text;
        end;
      end;
  except
      Result := n.Text;
  end;
end;

procedure THashVariantTextStream.DataImport(TextList: TListPascalString);
var
  i: Integer;
  n: TPascalString;
  TextName, TextValue: TPascalString;
begin
  if FVariantList = nil then
      Exit;
  if TextList.Count > 0 then
    for i := 0 to TextList.Count - 1 do
      begin
        n := TextList[i].TrimChar(#32);

        if ((n.Exists(':')) or (n.Exists('='))) and (not CharIn(n.First, [':', '='])) then
          begin
            TextName := umlGetFirstStr_M(n, ':=');
            if TextName.Len > 0 then
              begin
                TextValue := umlDeleteFirstStr_M(n, ':=');
                FVariantList[TextName.Text] := StrToV(TextValue.Text);
              end
            else
                FVariantList[n.Text] := '';
          end
        else
          begin
            FVariantList[n.Text] := '';
          end;
      end;
end;

procedure THashVariantTextStream.DataImport(TextList: TCoreClassStrings);
var
  ns: TListPascalString;
begin
  ns := TListPascalString.Create;
  ns.Assign(TextList);
  DataImport(ns);
  DisposeObject(ns);
end;

procedure THashVariantTextStream.DataExport(TextList: TListPascalString);
var
  i: Integer;
  vl: TCoreClassList;
  TextValue: SystemString;
begin
  if FVariantList = nil then
      Exit;
  vl := TCoreClassList.Create;
  FVariantList.HashList.GetListData(vl);
  if vl.Count > 0 then
    for i := 0 to vl.Count - 1 do
      begin
        TextValue := VToStr(PHashVariantListData(PHashListData(vl[i])^.Data)^.v);

        if TextValue <> '' then
            TextList.Add((PHashListData(vl[i])^.OriginName + '=' + TextValue))
        else
            TextList.Add(PHashListData(vl[i])^.OriginName);
      end;
  DisposeObject(vl);
end;

procedure THashVariantTextStream.DataExport(TextList: TCoreClassStrings);
var
  ns: TListPascalString;
begin
  ns := TListPascalString.Create;
  DataExport(ns);
  ns.AssignTo(TextList);
  DisposeObject(ns);
end;

procedure THashVariantTextStream.LoadFromStream(stream: TCoreClassStream);
var
  n: TListPascalString;
begin
  if FVariantList = nil then
      Exit;
  n := TListPascalString.Create;
  n.LoadFromStream(stream);
  DataImport(n);
  DisposeObject(n);
end;

procedure THashVariantTextStream.SaveToStream(stream: TCoreClassStream);
var
  n: TListPascalString;
begin
  if FVariantList = nil then
      Exit;
  n := TListPascalString.Create;
  DataExport(n);
  n.SaveToStream(stream);
  DisposeObject(n);
end;

procedure THashVariantTextStream.LoadFromFile(fileName: SystemString);
var
  ns: TCoreClassStream;
begin
  ns := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashVariantTextStream.SaveToFile(fileName: SystemString);
var
  ns: TCoreClassStream;
begin
  ns := TCoreClassFileStream.Create(fileName, fmCreate);
  try
      SaveToStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashVariantTextStream.LoadFromText(AText: SystemString);
var
  n: TListPascalString;
begin
  if FVariantList = nil then
      Exit;
  n := TListPascalString.Create;
  n.Text := AText;
  DataImport(n);
  DisposeObject(n);
end;

procedure THashVariantTextStream.SaveToText(var AText: SystemString);
var
  n: TListPascalString;
begin
  if FVariantList = nil then
      Exit;
  n := TListPascalString.Create;
  DataExport(n);
  AText := n.Text;
  DisposeObject(n);
end;

function THashVariantTextStream.Text: SystemString;
begin
  SaveToText(Result);
end;

function THashVariantTextStream.GetValue(AName: SystemString; v: Variant): Variant;
begin
  Result := NameValue[AName];
  if VarIsNull(Result) then
    begin
      NameValue[AName] := v;
      Result := v;
    end;
end;

function TListCardinal.GetItems(idx: Integer): Cardinal;
begin
  with PListCardinalData(FList[idx])^ do
      Result := Data;
end;

procedure TListCardinal.SetItems(idx: Integer; Value: Cardinal);
begin
  with PListCardinalData(FList[idx])^ do
      Data := Value;
end;

constructor TListCardinal.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TListCardinal.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListCardinal.Add(Value: Cardinal): Integer;
var
  p: PListCardinalData;
begin
  new(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

procedure TListCardinal.AddArray(const Value: array of Cardinal);
var
  i: Integer;
begin
  for i := 0 to length(Value) - 1 do
      Add(Value[i]);
end;

function TListCardinal.Delete(idx: Integer): Integer;
var
  p: PListCardinalData;
begin
  p := FList[idx];
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListCardinal.DeleteCardinal(Value: Cardinal): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    begin
      if Items[i] = Value then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListCardinal.Clear;
var
  i: Integer;
  p: PListCardinalData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PListCardinalData(FList[i]);
      Dispose(p);
    end;
  FList.Clear;
end;

function TListCardinal.Count: Integer;
begin
  Result := FList.Count;
end;

function TListCardinal.ExistsValue(Value: Cardinal): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
    if Items[i] = Value then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListCardinal.Assign(SameObj: TListCardinal);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

function TListInt64.GetItems(idx: Integer): Int64;
begin
  with PListInt64Data(FList[idx])^ do
      Result := Data;
end;

procedure TListInt64.SetItems(idx: Integer; Value: Int64);
begin
  with PListInt64Data(FList[idx])^ do
      Data := Value;
end;

constructor TListInt64.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TListInt64.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListInt64.Add(Value: Int64): Integer;
var
  p: PListInt64Data;
begin
  new(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

procedure TListInt64.AddArray(const Value: array of Int64);
var
  i: Integer;
begin
  for i := 0 to length(Value) - 1 do
      Add(Value[i]);
end;

function TListInt64.Delete(idx: Integer): Integer;
var
  p: PListInt64Data;
begin
  p := FList[idx];
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListInt64.DeleteInt64(Value: Int64): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    begin
      if Items[i] = Value then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListInt64.Clear;
var
  i: Integer;
  p: PListInt64Data;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PListInt64Data(FList[i]);
      Dispose(p);
    end;
  FList.Clear;
end;

function TListInt64.Count: Integer;
begin
  Result := FList.Count;
end;

function TListInt64.ExistsValue(Value: Int64): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
    if Items[i] = Value then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListInt64.Assign(SameObj: TListInt64);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

procedure TListInt64.SaveToStream(stream: TCoreClassStream);
var
  i: Integer;
  C: Integer;
begin
  C := FList.Count;
  stream.write(C, C_Integer_Size);
  for i := 0 to FList.Count - 1 do
      stream.write(PListInt64Data(FList[i])^.Data, C_Int64_Size);
end;

procedure TListInt64.LoadFromStream(stream: TCoreClassStream);
var
  i: Integer;
  C: Integer;
  v: Int64;
begin
  stream.read(C, C_Integer_Size);
  for i := 0 to C - 1 do
    begin
      stream.read(v, C_Int64_Size);
      Add(v);
    end;
end;

function TListNativeInt.GetItems(idx: Integer): nativeInt;
begin
  with PListNativeIntData(FList[idx])^ do
      Result := Data;
end;

procedure TListNativeInt.SetItems(idx: Integer; Value: nativeInt);
begin
  with PListNativeIntData(FList[idx])^ do
      Data := Value;
end;

constructor TListNativeInt.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TListNativeInt.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListNativeInt.Add(Value: nativeInt): Integer;
var
  p: PListNativeIntData;
begin
  new(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

procedure TListNativeInt.AddArray(const Value: array of nativeInt);
var
  i: Integer;
begin
  for i := 0 to length(Value) - 1 do
      Add(Value[i]);
end;

function TListNativeInt.Delete(idx: Integer): Integer;
var
  p: PListNativeIntData;
begin
  p := FList[idx];
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListNativeInt.DeleteNativeInt(Value: nativeInt): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    begin
      if Items[i] = Value then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListNativeInt.Clear;
var
  i: Integer;
  p: PListNativeIntData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PListNativeIntData(FList[i]);
      Dispose(p);
    end;
  FList.Clear;
end;

function TListNativeInt.Count: Integer;
begin
  Result := FList.Count;
end;

function TListNativeInt.ExistsValue(Value: nativeInt): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
    if Items[i] = Value then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListNativeInt.Assign(SameObj: TListNativeInt);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

function TListInteger.GetItems(idx: Integer): Integer;
begin
  with PListIntegerData(FList[idx])^ do
      Result := Data;
end;

procedure TListInteger.SetItems(idx: Integer; Value: Integer);
begin
  with PListIntegerData(FList[idx])^ do
      Data := Value;
end;

constructor TListInteger.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TListInteger.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListInteger.Add(Value: Integer): Integer;
var
  p: PListIntegerData;
begin
  new(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

procedure TListInteger.AddArray(const Value: array of Integer);
var
  i: Integer;
begin
  for i := 0 to length(Value) - 1 do
      Add(Value[i]);
end;

function TListInteger.Delete(idx: Integer): Integer;
var
  p: PListIntegerData;
begin
  p := FList[idx];
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListInteger.DeleteInteger(Value: Integer): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    begin
      if Items[i] = Value then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListInteger.Clear;
var
  i: Integer;
  p: PListIntegerData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PListIntegerData(FList[i]);
      Dispose(p);
    end;
  FList.Clear;
end;

function TListInteger.Count: Integer;
begin
  Result := FList.Count;
end;

function TListInteger.ExistsValue(Value: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
    if Items[i] = Value then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListInteger.Assign(SameObj: TListInteger);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

function TListDouble.GetItems(idx: Integer): Double;
begin
  with PListDoubleData(FList[idx])^ do
      Result := Data;
end;

procedure TListDouble.SetItems(idx: Integer; Value: Double);
begin
  with PListDoubleData(FList[idx])^ do
      Data := Value;
end;

constructor TListDouble.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TListDouble.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListDouble.Add(Value: Double): Integer;
var
  p: PListDoubleData;
begin
  new(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

procedure TListDouble.AddArray(const Value: array of Double);
var
  i: Integer;
begin
  for i := 0 to length(Value) - 1 do
      Add(Value[i]);
end;

function TListDouble.Delete(idx: Integer): Integer;
var
  p: PListDoubleData;
begin
  p := FList[idx];
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

procedure TListDouble.Clear;
var
  i: Integer;
  p: PListDoubleData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PListDoubleData(FList[i]);
      Dispose(p);
    end;
  FList.Clear;
end;

function TListDouble.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TListDouble.Assign(SameObj: TListDouble);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

function TListPointer.GetItems(idx: Integer): Pointer;
begin
  with PListPointerData(FList[idx])^ do
      Result := Data;
end;

procedure TListPointer.SetItems(idx: Integer; Value: Pointer);
begin
  with PListPointerData(FList[idx])^ do
      Data := Value;
end;

constructor TListPointer.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TListPointer.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListPointer.Add(Value: Pointer): Integer;
var
  p: PListPointerData;
begin
  new(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

function TListPointer.Delete(idx: Integer): Integer;
var
  p: PListPointerData;
begin
  p := FList[idx];
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListPointer.DeletePointer(Value: Pointer): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    begin
      if Items[i] = Value then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListPointer.Clear;
var
  i: Integer;
  p: PListPointerData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PListPointerData(FList[i]);
      Dispose(p);
    end;
  FList.Clear;
end;

function TListPointer.Count: Integer;
begin
  Result := FList.Count;
end;

function TListPointer.ExistsValue(Value: Pointer): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
    if Items[i] = Value then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListPointer.Assign(SameObj: TListPointer);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

function TListString.GetItems(idx: Integer): SystemString;
begin
  Result := PListStringData(FList[idx])^.Data;
end;

procedure TListString.SetItems(idx: Integer; Value: SystemString);
begin
  with PListStringData(FList[idx])^ do
    begin
      Data := Value;
      hash := MakeHash(Value);
    end;
end;

function TListString.GetObjects(idx: Integer): TCoreClassObject;
begin
  Result := PListStringData(FList[idx])^.Obj;
end;

procedure TListString.SetObjects(idx: Integer; Value: TCoreClassObject);
begin
  PListStringData(FList[idx])^.Obj := Value;
end;

constructor TListString.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TListString.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListString.Add(Value: SystemString): Integer;
var
  p: PListStringData;
begin
  new(p);
  p^.Data := Value;
  p^.Obj := nil;
  p^.hash := MakeHash(Value);
  Result := FList.Add(p);
end;

function TListString.Add(Value: SystemString; Obj: TCoreClassObject): Integer;
var
  p: PListStringData;
begin
  new(p);
  p^.Data := Value;
  p^.Obj := Obj;
  p^.hash := MakeHash(Value);
  Result := FList.Add(p);
end;

function TListString.Delete(idx: Integer): Integer;
var
  p: PListStringData;
begin
  p := FList[idx];
  p^.Data := '';
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListString.DeleteString(Value: SystemString): Integer;
var
  i: Integer;
  h: THash;
begin
  i := 0;
  h := MakeHash(Value);

  while i < Count do
    begin
      if (PListStringData(FList[i])^.hash = h) and (SameText(PListStringData(FList[i])^.Data, Value)) then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListString.Clear;
var
  i: Integer;
  p: PListStringData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PListStringData(FList[i]);
      p^.Data := '';
      Dispose(p);
    end;
  FList.Clear;
end;

function TListString.Count: Integer;
begin
  Result := FList.Count;
end;

function TListString.ExistsValue(Value: SystemString): Integer;
var
  i: Integer;
  h: THash;
begin
  h := MakeHash(Value);

  Result := -1;

  for i := 0 to Count - 1 do
    if (PListStringData(FList[i])^.hash = h) and (SameText(PListStringData(FList[i])^.Data, Value)) then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListString.Assign(SameObj: TListString);
var
  i: Integer;
  p1, p2: PListStringData;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
    begin
      p2 := PListStringData(SameObj.FList[i]);
      new(p1);
      p1^ := p2^;
      FList.Add(p1);
    end;
end;

procedure TListString.LoadFromStream(stream: TCoreClassStream);
var
  bp: Int64;
  R: TStreamReader;
begin
  Clear;
  bp := stream.Position;
{$IFDEF FPC}
  R := TStreamReader.Create(stream);
  while not R.Eof do
      Add(R.ReadLine);
{$ELSE FPC}
  R := TStreamReader.Create(stream, TEncoding.UTF8);
  try
    while not R.EndOfStream do
        Add(R.ReadLine);
  except
    Clear;
    DisposeObject(R);
    stream.Position := bp;
    R := TStreamReader.Create(stream, TEncoding.ANSI);
    while not R.EndOfStream do
        Add(R.ReadLine);
  end;
{$ENDIF FPC}
  DisposeObject(R);
end;

procedure TListString.SaveToStream(stream: TCoreClassStream);
var
  i: Integer;
  n: TPascalString;
  b: TBytes;
begin
  for i := 0 to FList.Count - 1 do
    begin
      n.Text := PListStringData(FList[i])^.Data + #13#10;
      b := n.Bytes;
      stream.write(b[0], length(b));
      n := '';
    end;
end;

procedure TListString.LoadFromFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TListString.SaveToFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

function TListPascalString.GetText: SystemString;
var
  i: Integer;
begin
  Result := '';
  if Count > 0 then
    begin
      Result := Items[0];
      for i := 1 to Count - 1 do
          Result := Result + #13#10 + Items[i];
    end;
end;

procedure TListPascalString.SetText(const Value: SystemString);
var
  n: TPascalString;
  b: TBytes;
  m64: TMemoryStream64;
begin
  n.Text := Value;
  b := n.Bytes;
  n := '';
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(@b[0], length(b));
  LoadFromStream(m64);
  DisposeObject(m64);
  SetLength(b, 0);
end;

function TListPascalString.GetItems(idx: Integer): TPascalString;
begin
  Result := PListPascalStringData(FList[idx])^.Data;
end;

procedure TListPascalString.SetItems(idx: Integer; Value: TPascalString);
begin
  with PListPascalStringData(FList[idx])^ do
    begin
      Data := Value;
      hash := MakeHash(Value);
    end;
end;

function TListPascalString.GetItems_PPascalString(idx: Integer): PPascalString;
begin
  Result := @(PListPascalStringData(FList[idx])^.Data);
end;

function TListPascalString.GetObjects(idx: Integer): TCoreClassObject;
begin
  Result := PListPascalStringData(FList[idx])^.Obj;
end;

procedure TListPascalString.SetObjects(idx: Integer; Value: TCoreClassObject);
begin
  PListPascalStringData(FList[idx])^.Obj := Value;
end;

constructor TListPascalString.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TListPascalString.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListPascalString.Add(Value: SystemString): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data.Text := Value;
  p^.Obj := nil;
  p^.hash := MakeHash(p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Add(Value: TPascalString): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data := Value;
  p^.Obj := nil;
  p^.hash := MakeHash(p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Add(Value: SystemString; Obj: TCoreClassObject): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data.Text := Value;
  p^.Obj := Obj;
  p^.hash := MakeHash(p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Add(Value: TPascalString; Obj: TCoreClassObject): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data := Value;
  p^.Obj := Obj;
  p^.hash := MakeHash(p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Append(Value: SystemString): Integer;
begin
  Result := Add(Value);
end;

function TListPascalString.Delete(idx: Integer): Integer;
var
  p: PListPascalStringData;
begin
  p := FList[idx];
  p^.Data := '';
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListPascalString.DeletePascalString(Value: TPascalString): Integer;
var
  i: Integer;
  h: THash;
begin
  i := 0;
  h := MakeHash(Value);
  while i < FList.Count do
    begin
      if (PListPascalStringData(FList[i])^.hash = h) and (PListPascalStringData(FList[i])^.Data.Same(Value)) then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListPascalString.Clear;
var
  i: Integer;
  p: PListPascalStringData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PListPascalStringData(FList[i]);
      p^.Data := '';
      Dispose(p);
    end;
  FList.Clear;
end;

function TListPascalString.Count: Integer;
begin
  Result := FList.Count;
end;

function TListPascalString.ExistsValue(Value: TPascalString): Integer;
var
  i: Integer;
  h: THash;
begin
  h := MakeHash(Value);
  Result := -1;

  for i := 0 to FList.Count - 1 do
    if (PListPascalStringData(FList[i])^.hash = h) and (PListPascalStringData(FList[i])^.Data.Same(Value)) then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListPascalString.Assign(SameObj: TListPascalString);
var
  i: Integer;
  p1, p2: PListPascalStringData;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
    begin
      p2 := PListPascalStringData(SameObj.FList[i]);
      new(p1);
      p1^ := p2^;
      FList.Add(p1);
    end;
end;

procedure TListPascalString.Assign(sour: TCoreClassStrings);
var
  i: Integer;
begin
  Clear;
  for i := 0 to sour.Count - 1 do
      Add(sour[i], sour.Objects[i]);
end;

procedure TListPascalString.AssignTo(dest: TCoreClassStrings);
var
  i: Integer;
begin
  dest.Clear;
  for i := 0 to Count - 1 do
      dest.AddObject(Items[i], Objects[i]);
end;

procedure TListPascalString.AddStrings(sour: TListPascalString);
var
  i: Integer;
begin
  for i := 0 to sour.Count - 1 do
      Add(sour[i]);
end;

procedure TListPascalString.AddStrings(sour: TCoreClassStrings);
var
  i: Integer;
begin
  for i := 0 to sour.Count - 1 do
      Add(sour[i]);
end;

procedure TListPascalString.FillTo(var output: TArrayPascalString);
var
  i: Integer;
begin
  SetLength(output, Count);
  for i := 0 to Count - 1 do
      output[i] := Items[i];
end;

procedure TListPascalString.FillFrom(const InData: TArrayPascalString);
var
  i: Integer;
begin
  Clear;
  for i := 0 to length(InData) - 1 do
      Add(InData[i]);
end;

procedure TListPascalString.LoadFromStream(stream: TCoreClassStream);
var
  bp: Int64;
  R: TStreamReader;
begin
  Clear;
  bp := stream.Position;
{$IFDEF FPC}
  R := TStreamReader.Create(stream);
  while not R.Eof do
      Add(R.ReadLine);
{$ELSE FPC}
  R := TStreamReader.Create(stream, TEncoding.UTF8);
  try
    while not R.EndOfStream do
        Add(R.ReadLine);
  except
    Clear;
    DisposeObject(R);
    stream.Position := bp;
    R := TStreamReader.Create(stream, TEncoding.ANSI);
    while not R.EndOfStream do
        Add(R.ReadLine);
  end;
{$ENDIF FPC}
  DisposeObject(R);
end;

procedure TListPascalString.SaveToStream(stream: TCoreClassStream);
var
  i: Integer;
  n: TPascalString;
  b: TBytes;
begin
  for i := 0 to FList.Count - 1 do
    begin
      n := PListPascalStringData(FList[i])^.Data.Text + #13#10;
      b := n.Bytes;
      stream.write(b[0], length(b));
      n := '';
    end;
end;

procedure TListPascalString.LoadFromFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TListPascalString.SaveToFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

function TListVariant.GetItems(idx: Integer): Variant;
begin
  with PListVariantData(FList[idx])^ do
      Result := Data;
end;

procedure TListVariant.SetItems(idx: Integer; Value: Variant);
begin
  with PListVariantData(FList[idx])^ do
      Data := Value;
end;

constructor TListVariant.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TListVariant.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListVariant.Add(Value: Variant): Integer;
var
  p: PListVariantData;
begin
  new(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

function TListVariant.Delete(idx: Integer): Integer;
var
  p: PListVariantData;
begin
  p := FList[idx];
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListVariant.DeleteVariant(Value: Variant): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    begin
      if umlSameVarValue(Items[i], Value) then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListVariant.Clear;
begin
  while Count > 0 do
      Delete(0);
end;

function TListVariant.Count: Integer;
begin
  Result := FList.Count;
end;

function TListVariant.ExistsValue(Value: Variant): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
    if umlSameVarValue(Items[i], Value) then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListVariant.Assign(SameObj: TListVariant);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

function TVariantToDataList.GetItems(ID: Variant): Pointer;
var
  i: Integer;
  p: PVariantToDataListData;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if umlSameVarValue(p^.ID, ID) then
        begin
          Result := p^.Data;
          Break;
        end;
    end;
end;

procedure TVariantToDataList.SetItems(ID: Variant; Value: Pointer);
var
  i: Integer;
  p: PVariantToDataListData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if umlSameVarValue(p^.ID, ID) then
        begin
          p^.Data := Value;
          Exit;
        end;
    end;

  new(p);
  p^.ID := ID;
  p^.Data := Value;
  FList.Add(p);
end;

procedure TVariantToDataList.DefaultDataFreeProc(p: Pointer);
begin
{$IFDEF FPC}
{$ELSE}
  Dispose(p);
{$ENDIF}
end;

procedure TVariantToDataList.DoDataFreeProc(p: Pointer);
begin
  FOnFreePtr(p);
end;

constructor TVariantToDataList.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
  FAutoFreeData := True;
  FOnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
end;

destructor TVariantToDataList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TVariantToDataList.Add(ID: Variant; Data: Pointer): Boolean;
var
  p: PVariantToDataListData;
begin
  if not Exists(ID) then
    begin
      new(p);
      p^.ID := ID;
      p^.Data := Data;
      FList.Add(p);
      Result := True;
    end
  else
      Result := False;
end;

function TVariantToDataList.Delete(ID: Variant): Boolean;
var
  i: Integer;
  p: PVariantToDataListData;
begin
  Result := False;
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if umlSameVarValue(p^.ID, ID) then
        begin
          try
            if (FAutoFreeData) and (p^.Data <> nil) then
                DoDataFreeProc(p^.Data);
            Dispose(p);
          except
          end;
          FList.Delete(i);
          Result := True;
        end
      else
          Inc(i);
    end;
end;

procedure TVariantToDataList.Clear;
var
  p: PVariantToDataListData;
begin
  while FList.Count > 0 do
    begin
      p := FList[0];
      try
        if (FAutoFreeData) and (p^.Data <> nil) then
            DoDataFreeProc(p^.Data);
        Dispose(p);
      except
      end;
      FList.Delete(0);
    end;
end;

function TVariantToDataList.Exists(ID: Variant): Boolean;
var
  i: Integer;
  p: PVariantToDataListData;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if umlSameVarValue(p^.ID, ID) then
        begin
          Result := True;
          Break;
        end;
    end;
end;

procedure TVariantToDataList.GetList(_To: TListVariant);
var
  i: Integer;
  p: PVariantToDataListData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      _To.Add(p^.ID);
    end;
end;

function TVariantToDataList.Count: Integer;
begin
  Result := FList.Count;
end;

function TVariantToVariantList.GetItems(ID: Variant): Variant;
var
  p: PVariantToVariantListData;
begin
  p := FList.Items[ID];
  if p <> nil then
      Result := p^.v
  else
      Result := Null;
end;

procedure TVariantToVariantList.SetItems(ID: Variant; Value: Variant);
var
  p: PVariantToVariantListData;
begin
  p := FList.Items[ID];
  if p <> nil then
      p^.v := Value
  else
      Add(ID, Value);
end;

procedure TVariantToVariantList.DefaultDataFreeProc(p: Pointer);
begin
  Dispose(PVariantToVariantListData(p));
end;

constructor TVariantToVariantList.Create;
begin
  inherited Create;
  FList := TVariantToDataList.Create;
  FList.FAutoFreeData := True;
  FList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
end;

destructor TVariantToVariantList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TVariantToVariantList.Add(ID, AValue: Variant): Boolean;
var
  p: PVariantToVariantListData;
begin
  if FList.Exists(ID) then
    begin
      p := FList[ID];
    end
  else
    begin
      new(p);
      FList[ID] := p;
    end;

  p^.v := AValue;

  Result := True;
end;

function TVariantToVariantList.Delete(ID: Variant): Boolean;
begin
  Result := FList.Delete(ID);
end;

procedure TVariantToVariantList.Clear;
begin
  FList.Clear;
end;

function TVariantToVariantList.Exists(ID: Variant): Boolean;
begin
  Result := FList.Exists(ID);
end;

procedure TVariantToVariantList.GetList(_To: TListVariant);
begin
  FList.GetList(_To);
end;

procedure TVariantToVariantList.GetValueList(_To: TListVariant);
var
  i: Integer;
  pVarData: PVariantToDataListData;
  pToValueData: PVariantToVariantListData;
begin
  for i := 0 to FList.FList.Count - 1 do
    begin
      pVarData := FList.FList[i];
      pToValueData := pVarData^.Data;
      _To.Add(pToValueData^.v);
    end;
end;

function TVariantToVariantList.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TVariantToVariantList.Assign(SameObj: TVariantToVariantList);
var
  _To: TListVariant;
  i: Integer;
begin
  Clear;
  _To := TListVariant.Create;
  SameObj.GetList(_To);
  for i := 0 to _To.Count - 1 do
      Items[_To[i]] := SameObj[_To[i]];
  DisposeObject(_To);
end;

function TVariantToObjectList.GetItems(ID: Variant): TCoreClassObject;
var
  p: PVariantToObjectListData;
begin
  p := FList.Items[ID];
  if p <> nil then
      Result := p^.Obj
  else
      Result := nil;
end;

procedure TVariantToObjectList.SetItems(ID: Variant; Value: TCoreClassObject);
var
  p: PVariantToObjectListData;
begin
  p := FList.Items[ID];
  if p <> nil then
      p^.Obj := Value
  else
      Add(ID, Value);
end;

procedure TVariantToObjectList.DefaultDataFreeProc(p: Pointer);
begin

end;

constructor TVariantToObjectList.Create;
begin
  inherited Create;
  FList := TVariantToDataList.Create;
  FList.FAutoFreeData := True;
  FList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
end;

destructor TVariantToObjectList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TVariantToObjectList.Add(ID: Variant; Obj: TCoreClassObject): Boolean;
var
  p: PVariantToObjectListData;
begin
  if FList.Exists(ID) then
    begin
      p := FList[ID];
    end
  else
    begin
      new(p);
      FList[ID] := p;
    end;

  p^.Obj := Obj;

  Result := True;
end;

function TVariantToObjectList.Delete(ID: Variant): Boolean;
begin
  Result := FList.Delete(ID);
end;

procedure TVariantToObjectList.Clear;
begin
  FList.Clear;
end;

function TVariantToObjectList.Exists(ID: Variant): Boolean;
begin
  Result := FList.Exists(ID);
end;

procedure TVariantToObjectList.GetList(_To: TListVariant);
begin
  FList.GetList(_To);
end;

function TVariantToObjectList.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TVariantToObjectList.Assign(SameObj: TVariantToObjectList);
var
  _To: TListVariant;
  i: Integer;
begin
  Clear;
  _To := TListVariant.Create;
  SameObj.GetList(_To);
  for i := 0 to _To.Count - 1 do
      Items[_To[i]] := SameObj[_To[i]];
  DisposeObject(_To);
end;

procedure TBackcallData.Init;
begin
  FlagObject := nil;
  NotifyCall := nil;
  NotifyMethod := nil;
{$IFNDEF FPC}
  NotifyProc := nil;
{$ENDIF}
end;

function TBackcalls.GetVariantList: THashVariantList;
begin
  if FVariantList = nil then
      FVariantList := THashVariantList.Create;
  Result := FVariantList;
end;

function TBackcalls.GetObjectList: THashObjectList;
begin
  if FObjectList = nil then
      FObjectList := THashObjectList.Create(False);
  Result := FObjectList;
end;

constructor TBackcalls.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
  FVariantList := nil;
  FObjectList := nil;
  FOwner := nil;
end;

destructor TBackcalls.Destroy;
begin
  if FVariantList <> nil then
      DisposeObject(FVariantList);
  if FObjectList <> nil then
      DisposeObject(FObjectList);
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TBackcalls.RegisterBackcall(AFlagObject: TCoreClassObject; ANotifyCall: TBackcallNotifyCall);
var
  p: PBackcallData;
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if PBackcallData(FList[i])^.FlagObject = AFlagObject then
        Exit;

  new(p);
  p^.Init;
  p^.FlagObject := AFlagObject;
  p^.NotifyCall := ANotifyCall;
  FList.Add(p);
end;

procedure TBackcalls.RegisterBackcall(AFlagObject: TCoreClassObject; ANotifyMethod: TBackcallNotifyMethod);
var
  p: PBackcallData;
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if PBackcallData(FList[i])^.FlagObject = AFlagObject then
        Exit;

  new(p);
  p^.Init;
  p^.FlagObject := AFlagObject;
  p^.NotifyMethod := ANotifyMethod;
  FList.Add(p);
end;

{$IFNDEF FPC}


procedure TBackcalls.RegisterBackcall(AFlagObject: TCoreClassObject; ANotifyProc: TBackcallNotifyProc);
var
  p: PBackcallData;
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if PBackcallData(FList[i])^.FlagObject = AFlagObject then
        Exit;

  new(p);
  p^.Init;
  p^.FlagObject := AFlagObject;
  p^.NotifyProc := ANotifyProc;
  FList.Add(p);
end;
{$ENDIF}


procedure TBackcalls.UnRegisterBackcall(AFlagObject: TCoreClassObject);
var
  i: Integer;
begin
  i := 0;
  while i < FList.Count do
    begin
      if PBackcallData(FList[i])^.FlagObject = AFlagObject then
        begin
          Dispose(PBackcallData(FList[i]));
          FList.Delete(i);
        end
      else
          Inc(i);
    end;
end;

procedure TBackcalls.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PBackcallData(FList[i]));
  FList.Clear;
end;

procedure TBackcalls.ExecuteBackcall(TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
var
  i: Integer;
  p: PBackcallData;
begin
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if Assigned(p^.NotifyCall) then
        begin
          try
              p^.NotifyCall(Self, TriggerObject, Param1, Param2, Param3);
          except
          end;
        end;
      if Assigned(p^.NotifyMethod) then
        begin
          try
              p^.NotifyMethod(Self, TriggerObject, Param1, Param2, Param3);
          except
          end;
        end;
{$IFNDEF FPC}
      if Assigned(p^.NotifyProc) then
        begin
          try
              p^.NotifyProc(Self, TriggerObject, Param1, Param2, Param3);
          except
          end;
        end;
{$ENDIF}
      if (i >= 0) and (i < FList.Count) and (FList[i] = p) then
          Inc(i);
    end;
end;

end. 
 
