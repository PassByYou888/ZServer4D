{ ****************************************************************************** }
{ * hash List Library,Wrten by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }

(*
  update history
  2017-11-26
  "String" define change as "SystemString"

  2017-12-5
  added support int64 hash object : TInt64HashList
*)

unit ListEngine;

{$I zDefine.inc}

interface

uses Sysutils, Variants, CoreClasses, Math, PascalStrings;

type
  TCounter = Integer;

  TListBuffer = array of TCoreClassList;
  PListBuffer = ^TListBuffer;

  THashObjectList  = class;
  THashVariantList = class;

  THashListData = record
    qHash: THash;
    LowerCaseName, OriginName: SystemString;
    Data: Pointer;
    id: TCounter;
    OwnerList: TCoreClassList;
  end;

  PHashListData = ^THashListData;

  TDataFreeProc = procedure(p: Pointer) of object;

  THashList = class(TCoreClassObject)
  private
    FListBuffer        : TListBuffer;
    FAutoFreeData      : Boolean;
    FCount             : Integer;
    FIDCounter         : TCounter;
    FIgnoreCase        : Boolean;
    FAccessOptimization: Boolean;
    FOnDataFreeProc    : TDataFreeProc;

    function GetListTable(Hash: THash; AutoCreate: Boolean): TCoreClassList;
    function GetNames(Name: SystemString): Pointer;

    procedure DefaultDataFreeProc(p: Pointer);
  protected
    procedure DoDataFreeProc(p: Pointer);
  public
    constructor Create; overload;
    constructor Create(hashBlockCount: Integer); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GetListData(OutputList: TCoreClassList);
    procedure FastGetListData(OutputList: TCoreClassList);
    procedure Delete(Name: SystemString); inline;
    procedure Add(Name: SystemString; _CustomData: Pointer; overwrite: Boolean = True); inline;
    function Find(Name: SystemString): Pointer; inline;
    function Exists(Name: SystemString): Boolean; inline;
    procedure SetHashBlockCount(cnt: Integer);
    function First: PHashListData; inline;
    function Last: PHashListData; inline;
    procedure DeleteFirst(c: Integer);
    procedure DeleteLast(c: Integer);

    function ListBuffer: PListBuffer; inline;

    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: Integer read FCount write FCount;
    property Names[name: SystemString]: Pointer read GetNames; default;
    property OnDataFreeProc: TDataFreeProc read FOnDataFreeProc write FOnDataFreeProc;
  end;

  TInt64HashListData = record
    qHash: THash;
    i64: Int64;
    Data: TCoreClassObject;
    id: TCounter;
    OwnerList: TCoreClassList;
  end;

  PInt64HashListData = ^TInt64HashListData;

  TInt64HashList = class(TCoreClassObject)
  private
    FListBuffer        : TListBuffer;
    FAutoFreeData      : Boolean;
    FCount             : Integer;
    FIDCounter         : TCounter;
    FMaxCache          : TCounter;
    FMinCache          : TCounter;
    FAccessOptimization: Boolean;

    function GetListTable(Hash: THash; AutoCreate: Boolean): TCoreClassList;
    function Geti64s(i64: Int64): TCoreClassObject;

    procedure DoDataFreeProc(obj: TCoreClassObject); inline;
  public
    constructor Create; overload;
    constructor Create(hashBlockCount: Integer); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GetListData(OutputList: TCoreClassList);
    procedure FastGetListData(OutputList: TCoreClassList);
    procedure Delete(i64: Int64); inline;
    function Add(i64: Int64; _CustomData: TCoreClassObject; overwrite: Boolean = True): PInt64HashListData; inline;
    function Find(i64: Int64): TCoreClassObject; inline;
    function Exists(i64: Int64): Boolean; inline;
    procedure SetHashBlockCount(cnt: Integer);
    function First: PInt64HashListData; inline;
    function Last: PInt64HashListData; inline;
    procedure DeleteFirst(c: Integer);
    procedure DeleteLast(c: Integer);

    procedure Recache;

    function ListBuffer: PListBuffer; inline;

    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property Count: Integer read FCount write FCount;
    property i64s[i64: Int64]: TCoreClassObject read Geti64s; default;
    property MaxCache: TCounter read FMaxCache write FMaxCache;
    property MinCache: TCounter read FMinCache write FMinCache;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
  end;

  TListString       = class;
  TListPascalString = class;

  THashObjectChangeEvent = procedure(Sender: THashObjectList; Name: SystemString; _OLD, _New: TCoreClassObject) of object;

  THashObjectListData = record
    obj: TCoreClassObject;
    OnChnage: THashObjectChangeEvent;
  end;

  PHashObjectListData = ^THashObjectListData;

  THashObjectList = class(TCoreClassObject)
  private
    FAutoFreeObject: Boolean;
    FHashList      : THashList;
    FIncremental   : NativeInt;

    function GetCount: Integer;

    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);

    function GetNames(Name: SystemString): TCoreClassObject;
    procedure SetNames(Name: SystemString; const Value: TCoreClassObject);

    function GetOnChange(Name: SystemString): THashObjectChangeEvent;
    procedure SetOnChange(Name: SystemString; const aValue: THashObjectChangeEvent);

    procedure DefaultDataFreeProc(p: Pointer);
  protected
  public
    constructor Create(_AutoFreeObject: Boolean); overload;
    constructor Create(_AutoFreeObject: Boolean; MaxHashBlock: Integer); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GetNameList(OutputList: TCoreClassStrings); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    procedure GetListData(OutputList: TCoreClassStrings);
    procedure GetAsList(OutputList: TCoreClassListForObj);
    procedure FastGetAsList(OutputList: TCoreClassListForObj);
    function GetObjAsName(obj: TCoreClassObject): SystemString;
    procedure Delete(Name: SystemString); inline;
    function Add(Name: SystemString; _Object: TCoreClassObject): TCoreClassObject; inline;
    function Find(Name: SystemString): TCoreClassObject; inline;
    function Exists(Name: SystemString): Boolean; inline;
    function ExistsObject(obj: TCoreClassObject): Boolean;
    procedure CopyFrom(Source: THashObjectList);
    function ReName(_OLDName, _NewName: SystemString): Boolean; inline;
    function MakeName: SystemString;
    function MakeRefName(RefrenceName: SystemString): SystemString;

    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property AutoFreeObject: Boolean read FAutoFreeObject write FAutoFreeObject;
    property Count: Integer read GetCount;
    property Names[name: SystemString]: TCoreClassObject read GetNames write SetNames; default;
    property OnChange[name: SystemString]: THashObjectChangeEvent read GetOnChange write SetOnChange;
    // no script interface
    property HashList: THashList read FHashList;
  end;

  THashVariantChangeEvent = procedure(Sender: THashVariantList; Name: SystemString; _OLD, _New: Variant) of object;

  THashVariantListData = record
    V: Variant;
    OnChnage: THashVariantChangeEvent;
  end;

  PHashVariantListData = ^THashVariantListData;

  THashVariantList = class(TCoreClassObject)
  private
    FHashList              : THashList;
    FAutoUpdateDefaultValue: Boolean;
    FOnValueChangeNotify   : THashVariantChangeEvent;

    function GetCount: Integer;

    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);

    function GetNames(Name: SystemString): Variant;
    procedure SetNames(Name: SystemString; const Value: Variant);

    function GetOnChange(Name: SystemString): THashVariantChangeEvent;
    procedure SetOnChange(Name: SystemString; const aValue: THashVariantChangeEvent);

    procedure DefaultDataFreeProc(p: Pointer);
  protected
  public
    constructor Create; overload;
    constructor Create(MaxHashBlock: Integer); overload;
    destructor Destroy; override;
    procedure Clear; inline;
    procedure GetNameList(OutputList: TCoreClassStrings); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    procedure Delete(Name: SystemString); inline;
    function Add(Name: SystemString; V: Variant): Variant; inline;
    function FastAdd(Name: SystemString; V: Variant): Variant; inline;
    function Find(Name: SystemString): Variant; inline;
    function FindValue(aValue: Variant): SystemString; inline;
    function Exists(Name: SystemString): Boolean; inline;
    procedure CopyFrom(Source: THashVariantList);
    function GetType(Name: SystemString): Word; inline;

    function IncValue(Name: SystemString; V: Variant): Variant; overload;
    procedure IncValue(vl: THashVariantList); overload;

    function SetMax(Name: SystemString; V: Variant): Variant; overload;
    procedure SetMax(vl: THashVariantList); overload;

    function SetMin(Name: SystemString; V: Variant): Variant; overload;
    procedure SetMin(vl: THashVariantList); overload;

    function GetDefaultValue(Name: SystemString; aValue: Variant): Variant;
    procedure SetDefaultValue(Name: SystemString; aValue: Variant);

    function ReplaceMacro(const AText, HeadFlag, TailFlag: SystemString; var Output: SystemString): Boolean;

    property AutoUpdateDefaultValue: Boolean read FAutoUpdateDefaultValue write FAutoUpdateDefaultValue;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property Count: Integer read GetCount;
    property Names[name: SystemString]: Variant read GetNames write SetNames; default;
    property OnChange[name: SystemString]: THashVariantChangeEvent read GetOnChange write SetOnChange;
    property OnValueChangeNotify: THashVariantChangeEvent read FOnValueChangeNotify write FOnValueChangeNotify;

    procedure LoadFromStream(Stream: TCoreClassStream);
    procedure SaveToStream(Stream: TCoreClassStream);
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToFile(FileName: SystemString);
    procedure ExportAsStrings(Output: TCoreClassStrings);
    procedure ImportFromStrings(Output: TCoreClassStrings);
    function GetAsText: SystemString;
    procedure SetAsText(const Value: SystemString);
    property AsText: SystemString read GetAsText write SetAsText;

    // no script interface
    property HashList: THashList read FHashList;
  end;

  THashVariantTextStream = class(TCoreClassObject)
  private
    FVariantList: THashVariantList;

    function GetNames(aName: SystemString): Variant;
    procedure SetNames(aName: SystemString; const Value: Variant);
  public
    constructor Create(_VList: THashVariantList);
    destructor Destroy; override;
    procedure Clear;

    class function VToStr(V: Variant): SystemString; inline;
    class function StrToV(s: SystemString): Variant; inline;

    procedure DataImport(TextList: TCoreClassStrings);
    procedure DataExport(TextList: TCoreClassStrings);
    procedure LoadFromStream(Stream: TCoreClassStream);
    procedure SaveToStream(Stream: TCoreClassStream);
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromText(AText: SystemString);

    procedure SaveToText(var AText: SystemString); overload;
    function Text: SystemString;

    function GetValue(aName: SystemString; V: Variant): Variant;

    property Names[aName: SystemString]: Variant read GetNames write SetNames; default;
    property VariantList: THashVariantList read FVariantList write FVariantList;
  end;

  TListCardinalData = record
    Data: Cardinal;
  end;

  PListCardinalData = ^TListCardinalData;

  TListCardinal = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): Cardinal;
    procedure SetItems(Idx: Integer; Value: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Cardinal): Integer;
    function Delete(Idx: Integer): Integer;
    function DeleteCardinal(Value: Cardinal): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Cardinal): Integer;
    procedure Assign(SameObj: TListCardinal);

    property Items[Idx: Integer]: Cardinal read GetItems write SetItems; default;
  end;

  TListInt64Data = record
    Data: Int64;
  end;

  PListInt64Data = ^TListInt64Data;

  TListInt64 = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): Int64;
    procedure SetItems(Idx: Integer; Value: Int64);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Int64): Integer;
    function Delete(Idx: Integer): Integer;
    function DeleteInt64(Value: Int64): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Int64): Integer;
    procedure Assign(SameObj: TListInt64);

    procedure SaveToStream(Stream: TCoreClassStream);
    procedure LoadFromStream(Stream: TCoreClassStream);

    property Items[Idx: Integer]: Int64 read GetItems write SetItems; default;
  end;

  TListNativeIntData = record
    Data: NativeInt;
  end;

  PListNativeIntData = ^TListNativeIntData;

  TListNativeInt = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): NativeInt;
    procedure SetItems(Idx: Integer; Value: NativeInt);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: NativeInt): Integer;
    function Delete(Idx: Integer): Integer;
    function DeleteNativeInt(Value: NativeInt): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: NativeInt): Integer;
    procedure Assign(SameObj: TListNativeInt);

    property Items[Idx: Integer]: NativeInt read GetItems write SetItems; default;
  end;

  TListIntegerData = record
    Data: Integer;
  end;

  PListIntegerData = ^TListIntegerData;

  TListInteger = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): Integer;
    procedure SetItems(Idx: Integer; Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Integer): Integer;
    function Delete(Idx: Integer): Integer;
    function DeleteInteger(Value: Integer): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Integer): Integer;
    procedure Assign(SameObj: TListInteger);

    property Items[Idx: Integer]: Integer read GetItems write SetItems; default;
  end;

  TListDoubleData = record
    Data: Double;
  end;

  PListDoubleData = ^TListDoubleData;

  TListDouble = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): Double;
    procedure SetItems(Idx: Integer; Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Double): Integer;
    function Delete(Idx: Integer): Integer;
    function DeleteDouble(Value: Double): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Double): Integer;
    procedure Assign(SameObj: TListDouble);

    property Items[Idx: Integer]: Double read GetItems write SetItems; default;
  end;

  TListPointerData = record
    Data: Pointer;
  end;

  PListPointerData = ^TListPointerData;

  TListPointer = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): Pointer;
    procedure SetItems(Idx: Integer; Value: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Pointer): Integer;
    function Delete(Idx: Integer): Integer;
    function DeletePointer(Value: Pointer): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Pointer): Integer;
    procedure Assign(SameObj: TListPointer);

    property Items[Idx: Integer]: Pointer read GetItems write SetItems; default;
  end;

  TPointerList = TListPointer;

  TListStringData = record
    Data: SystemString;
    Hash: THash;
  end;

  PListStringData = ^TListStringData;

  TListString = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): SystemString;
    procedure SetItems(Idx: Integer; Value: SystemString);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: SystemString): Integer;
    function Delete(Idx: Integer): Integer;
    function DeleteString(Value: SystemString): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: SystemString): Integer;
    procedure Assign(SameObj: TListString);

    property Items[Idx: Integer]: SystemString read GetItems write SetItems; default;
  end;

  TListPascalStringData = record
    Data: TPascalString;
    Hash: THash;
  end;

  PListPascalStringData = ^TListPascalStringData;

  TListPascalString = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): TPascalString;
    procedure SetItems(Idx: Integer; Value: TPascalString);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: TPascalString): Integer;
    function Delete(Idx: Integer): Integer;
    function DeletePascalString(Value: TPascalString): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: TPascalString): Integer;
    procedure Assign(SameObj: TListPascalString);

    property Items[Idx: Integer]: TPascalString read GetItems write SetItems; default;
  end;

  TListVariantData = record
    Data: Variant;
  end;

  PListVariantData = ^TListVariantData;

  TListVariant = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): Variant;
    procedure SetItems(Idx: Integer; Value: Variant);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: Variant): Integer;
    function Delete(Idx: Integer): Integer;
    function DeleteVariant(Value: Variant): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: Variant): Integer;
    procedure Assign(SameObj: TListVariant);

    property Items[Idx: Integer]: Variant read GetItems write SetItems; default;
  end;

  TVariantToDataListData = record
    id: Variant;
    Data: Pointer;
  end;

  PVariantToDataListData = ^TVariantToDataListData;

  TVariantToDataList = class(TCoreClassObject)
  private
    FList          : TCoreClassList;
    FAutoFreeData  : Boolean;
    FOnDataFreeProc: TDataFreeProc;
  protected
    function GetItems(id: Variant): Pointer;
    procedure SetItems(id: Variant; Value: Pointer);
    procedure DefaultDataFreeProc(p: Pointer);
    procedure DoDataFreeProc(p: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(id: Variant; Data: Pointer): Boolean;
    function Delete(id: Variant): Boolean;
    procedure Clear;
    function Exists(id: Variant): Boolean;
    procedure GetList(_To: TListVariant);
    function Count: Integer;

    {$IFNDEF FPC}
    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    {$ENDIF}
    property Items[id: Variant]: Pointer read GetItems write SetItems; default;
    property OnDataFreeProc: TDataFreeProc read FOnDataFreeProc write FOnDataFreeProc;
  end;

  TVariantToVariantListData = record
    V: Variant;
  end;

  PVariantToVariantListData = ^TVariantToVariantListData;

  TVariantToVariantList = class(TCoreClassObject)
  private
    FList: TVariantToDataList;
  protected
    function GetItems(id: Variant): Variant;
    procedure SetItems(id: Variant; Value: Variant);
    procedure DefaultDataFreeProc(p: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(id, aValue: Variant): Boolean;
    function Delete(id: Variant): Boolean;
    procedure Clear;
    function Exists(id: Variant): Boolean;
    procedure GetList(_To: TListVariant);
    procedure GetValueList(_To: TListVariant);
    function Count: Integer;
    procedure Assign(SameObj: TVariantToVariantList);

    property Items[id: Variant]: Variant read GetItems write SetItems; default;
  end;

  TVariantToObjectListData = record
    obj: TCoreClassObject;
  end;

  PVariantToObjectListData = ^TVariantToObjectListData;

  TVariantToObjectList = class(TCoreClassObject)
  private
    FList: TVariantToDataList;
  protected
    function GetItems(id: Variant): TCoreClassObject;
    procedure SetItems(id: Variant; Value: TCoreClassObject);
    procedure DefaultDataFreeProc(p: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(id: Variant; obj: TCoreClassObject): Boolean;
    function Delete(id: Variant): Boolean;
    procedure Clear;
    function Exists(id: Variant): Boolean;
    procedure GetList(_To: TListVariant);
    function Count: Integer;
    procedure Assign(SameObj: TVariantToObjectList);

    property Items[id: Variant]: TCoreClassObject read GetItems write SetItems; default;
  end;

  TBackcalls            = class;
  TBackcallNotifyCall   = procedure(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
  TBackcallNotifyMethod = procedure(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant) of object;

  {$IFNDEF FPC}
  TBackcallNotifyProc = reference to procedure(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
  {$ENDIF}
  PBackcallData = ^TBackcallData;

  TBackcallData = record
    FlagObject: TCoreClassObject;
    NotifyCall: TBackcallNotifyCall;
    NotifyMethod: TBackcallNotifyMethod;
    {$IFNDEF FPC}
    NotifyProc: TBackcallNotifyProc;
    {$ENDIF}
    procedure Init;
  end;

  TBackcalls = class(TCoreClassObject)
  private
    FList       : TCoreClassList;
    FVariantList: THashVariantList;
    FObjectList : THashObjectList;
    FOwner      : TCoreClassObject;

    function GetVariantList: THashVariantList;
    function GetObjectList: THashObjectList;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterBackcall(AFlagObject: TCoreClassObject; ANotifyCall: TBackcallNotifyCall); overload;
    procedure RegisterBackcall(AFlagObject: TCoreClassObject; ANotifyMethod: TBackcallNotifyMethod); overload;
    {$IFNDEF FPC}
    procedure RegisterBackcall(AFlagObject: TCoreClassObject; ANotifyProc: TBackcallNotifyProc); overload;
    {$ENDIF}
    procedure UnRegisterBackcall(AFlagObject: TCoreClassObject);

    procedure Clear;

    procedure ExecuteBackcall(TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);

    property VariantList: THashVariantList read GetVariantList;
    property ObjectList: THashObjectList read GetObjectList;
    property Owner: TCoreClassObject read FOwner write FOwner;
  end;

function LoadSectionTextAsHashObjectList(AText: SystemString): THashObjectList;
function LoadSectionTextAsObjectList(AText: SystemString): TCoreClassStrings;
function SaveObjectListAsSectionText(aSectionList: TCoreClassStrings): SystemString;
function SaveHashObjectListAsSectionText(aSectionList: THashObjectList): SystemString;
procedure FreeAndNilSectionList(var V: TCoreClassStrings);

implementation

uses DoStatusIO, UnicodeMixedLib;

function MakeHash(var s: SystemString): THash; inline; overload;
begin
  Result := FastHashSystemString(@s);
end;

function MakeHash(var s: TPascalString): THash; inline; overload;
begin
  Result := FastHashPascalString(@s);
end;

function MakeHash(const i64: Int64): THash; inline; overload;
begin
  Result := umlCRC32(@i64, umlInt64Length);
end;

function LoadSectionTextAsObjectList(AText: SystemString): TCoreClassStrings;
  procedure AddDataSection(Name: SystemString; sLst: TCoreClassStrings);
  begin
    while (sLst.Count > 0) and (sLst[sLst.Count - 1] = '') do
        sLst.Delete(sLst.Count - 1);
    Result.AddObject(name, sLst);
  end;

var
  TextList    : TCoreClassStrings;
  i           : Integer;
  flag        : Boolean;
  _NewSection : SystemString;
  _NewTextList: TCoreClassStrings;
begin
  Result := TCoreClassStringList.Create;
  TextList := TCoreClassStringList.Create;
  TextList.Text := AText;
  _NewTextList := nil;
  _NewSection := '';
  flag := False;
  if TextList.Count > 0 then
    begin
      for i := 0 to TextList.Count - 1 do
        begin
          if umlMultipleMatch(False, '[*]', umlTrimChar(TextList[i], ' ')) then
            begin
              if flag then
                  AddDataSection(_NewSection, _NewTextList);
              _NewTextList := TCoreClassStringList.Create;
              if TextList[i] <> '[]' then
                  _NewSection := umlGetFirstStr(TextList[i], '[]').Text
              else
                  _NewSection := '';
              flag := True;
            end
          else if flag then
            begin
              _NewTextList.Append(TextList[i]);
            end;
        end;
      if flag then
          AddDataSection(_NewSection, _NewTextList);
    end;
  DisposeObject(TextList);
end;

function LoadSectionTextAsHashObjectList(AText: SystemString): THashObjectList;
var
  i   : Integer;
  sLst: TCoreClassStrings;
begin
  Result := THashObjectList.Create(True);
  sLst := LoadSectionTextAsObjectList(AText);
  if sLst.Count > 0 then
    for i := 0 to sLst.Count - 1 do
        Result[sLst[i]] := sLst.Objects[i];
  DisposeObject(sLst);
end;

function SaveObjectListAsSectionText(aSectionList: TCoreClassStrings): SystemString;
var
  i  : Integer;
  obj: TCoreClassStrings;
begin
  Result := '';
  if aSectionList.Count > 0 then
    for i := 0 to aSectionList.Count - 1 do
      begin
        Result := Result + Format('[%s]'#13#10, [aSectionList[i]]);
        if aSectionList.Objects[i] is TCoreClassStrings then
          begin
            obj := TCoreClassStrings(aSectionList.Objects[i]);
            Result := Result + obj.Text + #13#10;
          end;
      end;
end;

function SaveHashObjectListAsSectionText(aSectionList: THashObjectList): SystemString;
var
  sLst: TCoreClassStrings;
begin
  sLst := TCoreClassStringList.Create;
  aSectionList.GetListData(sLst);
  Result := SaveObjectListAsSectionText(sLst);
  DisposeObject(sLst);
end;

procedure FreeAndNilSectionList(var V: TCoreClassStrings);
var
  i: Integer;
begin
  if V.Count > 0 then
    for i := 0 to V.Count - 1 do
      begin
        try
            DisposeObject(V.Objects[i]);
        except
        end;
      end;

  DisposeObject(V);
  V := nil;
end;

function THashList.GetListTable(Hash: THash; AutoCreate: Boolean): TCoreClassList;
var
  i: Integer;
begin
  i := Hash mod Length(FListBuffer);

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCoreClassList.Create;
  Result := FListBuffer[i];
end;

function THashList.GetNames(Name: SystemString): Pointer;
var
  lName  : SystemString;
  newhash: THash;
  i, j   : Integer;
  lst, nl: TCoreClassList;
  pData  : PHashListData;
begin
  Result := nil;
  if FIgnoreCase then
      lName := LowerCase(name)
  else
      lName := name;
  newhash := MakeHash(lName);
  lst := GetListTable(newhash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PHashListData(lst[i]);
        if (newhash = pData^.qHash) and (lName = pData^.LowerCaseName) then
          begin
            Result := pData^.Data;

            if (FAccessOptimization) and (pData^.id < FIDCounter - 1) then
              begin
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.id := FIDCounter;

                if FIDCounter > FIDCounter + 1 then
                  begin
                    // rebuild idcounter
                    nl := TCoreClassList.Create;
                    GetListData(nl);
                    for j := 0 to nl.Count - 1 do
                        PHashListData(nl[j])^.id := j + 1;
                    FIDCounter := nl.Count + 1;
                    DisposeObject(nl);
                  end
                else
                    Inc(FIDCounter);
              end;

            Exit;
          end;
      end;
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
  FOnDataFreeProc(p);
end;

constructor THashList.Create;
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAutoFreeData := False;
  FIgnoreCase := True;
  FAccessOptimization := False;

  {$IFDEF FPC}
  FOnDataFreeProc := @DefaultDataFreeProc;
  {$ELSE}
  FOnDataFreeProc := DefaultDataFreeProc;
  {$ENDIF}
  SetLength(FListBuffer, 0);
  SetHashBlockCount(10);
end;

constructor THashList.Create(hashBlockCount: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FAutoFreeData := False;
  FIgnoreCase := True;
  FAccessOptimization := False;

  {$IFDEF FPC}
  FOnDataFreeProc := @DefaultDataFreeProc;
  {$ELSE}
  FOnDataFreeProc := DefaultDataFreeProc;
  {$ENDIF}
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
  i    : Integer;
  j    : Integer;
  lst  : TCoreClassList;
  pData: PHashListData;
begin
  FCount := 0;
  FIDCounter := 0;
  if Length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := lst.Items[j];
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

procedure THashList.GetListData(OutputList: TCoreClassList);

  function Comp(const A, B: TCounter): ShortInt; inline;
  begin
    if A = B then
        Result := 0
    else if A < B then
        Result := -1
    else
        Result := 1;
  end;

  function SortCompare(Item1, Item2: Pointer): ShortInt; inline;
  begin
    Result := Comp(PHashListData(Item1)^.id, PHashListData(Item2)^.id);
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer);
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while SortCompare(SortList[i], p) < 0 do
            Inc(i);
        while SortCompare(SortList[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i  : Integer;
  j  : Integer;
  lst: TCoreClassList;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Capacity := Count;
      for i := low(FListBuffer) to high(FListBuffer) do
        if FListBuffer[i] <> nil then
          begin
            lst := FListBuffer[i];
            if lst.Count > 0 then
              begin
                for j := lst.Count - 1 downto 0 do
                    OutputList.Add(lst.Items[j]);
              end;
          end;

      if OutputList.Count > 1 then
          QuickSortList(OutputList.ListData^, 0, OutputList.Count - 1);
    end;
end;

procedure THashList.FastGetListData(OutputList: TCoreClassList);
var
  i  : Integer;
  j  : Integer;
  lst: TCoreClassList;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Capacity := Count;
      for i := low(FListBuffer) to high(FListBuffer) do
        if FListBuffer[i] <> nil then
          begin
            lst := FListBuffer[i];
            if lst.Count > 0 then
              begin
                for j := lst.Count - 1 downto 0 do
                    OutputList.Add(lst.Items[j]);
              end;
          end;
    end;
end;

procedure THashList.Delete(Name: SystemString);
var
  newhash  : THash;
  i        : Integer;
  lName    : SystemString;
  lst      : TCoreClassList;
  _ItemData: PHashListData;
begin
  if FIgnoreCase then
      lName := LowerCase(name)
  else
      lName := name;
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
end;

procedure THashList.Add(Name: SystemString; _CustomData: Pointer; overwrite: Boolean = True);
var
  newhash: THash;
  lst, nl: TCoreClassList;
  i, j   : Integer;
  lName  : SystemString;
  pData  : PHashListData;
begin
  if FIgnoreCase then
      lName := LowerCase(name)
  else
      lName := name;
  newhash := MakeHash(lName);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) and (overwrite) then
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

              if (pData^.id < FIDCounter - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.id := FIDCounter;

                  if FIDCounter > FIDCounter + 1 then
                    begin
                      // rebuild idcounter
                      nl := TCoreClassList.Create;
                      GetListData(nl);
                      for j := 0 to nl.Count - 1 do
                          PHashListData(nl[j])^.id := j + 1;
                      FIDCounter := nl.Count + 1;
                      DisposeObject(nl);
                    end
                  else
                      Inc(FIDCounter);
                end;

              Exit;
            end;
        end;
    end;

  New(pData);
  pData^.qHash := newhash;
  pData^.LowerCaseName := lName;
  pData^.OriginName := name;
  pData^.Data := _CustomData;
  pData^.id := FIDCounter;
  pData^.OwnerList := lst;
  lst.Add(pData);
  Inc(FCount);

  if FIDCounter > FIDCounter + 1 then
    begin
      // rebuild idcounter
      nl := TCoreClassList.Create;
      GetListData(nl);
      for j := 0 to nl.Count - 1 do
          PHashListData(nl[j])^.id := j + 1;
      FIDCounter := nl.Count + 1;
      DisposeObject(nl);
    end
  else
      Inc(FIDCounter);
end;

function THashList.Find(Name: SystemString): Pointer;
var
  i    : Integer;
  j    : Integer;
  lst  : TCoreClassList;
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
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := PHashListData(lst.Items[j]);
                  if (umlMultipleMatch(True, name, pData^.OriginName)) then
                    begin
                      Result := pData^.Data;
                      Exit;
                    end;
                end;
            end;
        end;
    end;
end;

function THashList.Exists(Name: SystemString): Boolean;
var
  newhash: THash;
  i      : Integer;
  lst    : TCoreClassList;
  pData  : PHashListData;
  lName  : SystemString;
begin
  Result := False;
  if FIgnoreCase then
      lName := LowerCase(name)
  else
      lName := name;
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

function THashList.First: PHashListData;
var
  i : Integer;
  mv: TCounter;
begin
  mv := FIDCounter;
  Result := nil;
  for i := 0 to Length(FListBuffer) - 1 do
    if (FListBuffer[i] <> nil) and (FListBuffer[i].Count > 0) then
      if PHashListData(FListBuffer[i][0])^.id <= mv then
        begin
          Result := FListBuffer[i][0];
          mv := Result^.id;
        end;
end;

function THashList.Last: PHashListData;
var
  i  : Integer;
  mv : TCounter;
  lst: TCoreClassList;
begin
  mv := 0;
  Result := nil;
  for i := 0 to Length(FListBuffer) - 1 do
    begin
      lst := FListBuffer[i];
      if (lst <> nil) and (lst.Count > 0) then
        if PHashListData(lst[lst.Count - 1])^.id >= mv then
          begin
            Result := lst[0];
            mv := Result^.id;
          end;
    end;
end;

procedure THashList.DeleteFirst(c: Integer);
var
  p: PHashListData;
begin
  while c > 0 do
    begin
      p := First;

      if (p <> nil) and (p^.OwnerList[0] = p) then
        begin
          p^.OwnerList.Delete(0);
          if (FAutoFreeData) and (p^.Data <> nil) then
            begin
              try
                DoDataFreeProc(p^.Data);
                p^.Data := nil;
              except
              end;
            end;
          Dispose(p);
          Dec(FCount);
        end;
      Dec(c);
    end;
end;

procedure THashList.DeleteLast(c: Integer);
var
  p: PHashListData;
begin
  while c > 0 do
    begin
      p := Last;

      if (p <> nil) and (p^.OwnerList[p^.OwnerList.Count - 1] = p) then
        begin
          p^.OwnerList.Delete(p^.OwnerList.Count - 1);
          if (FAutoFreeData) and (p^.Data <> nil) then
            begin
              try
                DoDataFreeProc(p^.Data);
                p^.Data := nil;
              except
              end;
            end;
          Dispose(p);
          Dec(FCount);
        end;
      Dec(c);
    end;
end;

function THashList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure THashList.PrintHashReport;
var
  i                 : NativeInt;
  L                 : TCoreClassList;
  total             : NativeInt;
  usaged, aMax, aMin: NativeInt;
  inited            : Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          Inc(usaged);
          total := total + L.Count;
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
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, total, aMax, aMin]));
end;

function TInt64HashList.GetListTable(Hash: THash; AutoCreate: Boolean): TCoreClassList;
var
  i: Integer;
begin
  i := Hash mod Length(FListBuffer);

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCoreClassList.Create;
  Result := FListBuffer[i];
end;

function TInt64HashList.Geti64s(i64: Int64): TCoreClassObject;
var
  newhash: THash;
  i, j   : Integer;
  lst, nl: TCoreClassList;
  pData  : PInt64HashListData;
begin
  Result := nil;
  newhash := MakeHash(i64);
  lst := GetListTable(newhash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PInt64HashListData(lst[i]);
        if (newhash = pData^.qHash) and (i64 = pData^.i64) then
          begin
            Result := pData^.Data;

            if (FAccessOptimization) and (pData^.id < FIDCounter - 1) then
              begin
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.id := FIDCounter;

                if FIDCounter > FIDCounter + 1 then
                  begin
                    // rebuild idcounter
                    nl := TCoreClassList.Create;
                    GetListData(nl);
                    for j := 0 to nl.Count - 1 do
                        PInt64HashListData(nl[j])^.id := j + 1;
                    FIDCounter := nl.Count + 1;
                    DisposeObject(nl);
                  end
                else
                    Inc(FIDCounter);
              end;

            Exit;
          end;
      end;
end;

procedure TInt64HashList.DoDataFreeProc(obj: TCoreClassObject);
begin
  DisposeObject(obj);
end;

constructor TInt64HashList.Create;
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FMaxCache := 10000;
  FMinCache := 1000;
  FAccessOptimization := False;
  FAutoFreeData := False;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(128);
end;

constructor TInt64HashList.Create(hashBlockCount: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDCounter := 0;
  FMaxCache := 10000;
  FMinCache := 1000;
  FAccessOptimization := False;
  FAutoFreeData := False;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(hashBlockCount);
end;

destructor TInt64HashList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TInt64HashList.Clear;
var
  i    : Integer;
  j    : Integer;
  lst  : TCoreClassList;
  pData: PInt64HashListData;
begin
  FCount := 0;
  FIDCounter := 0;
  if Length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := lst.Items[j];
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

procedure TInt64HashList.GetListData(OutputList: TCoreClassList);

  function Comp(const A, B: TCounter): ShortInt; inline;
  begin
    if A = B then
        Result := 0
    else if A < B then
        Result := -1
    else
        Result := 1;
  end;

  function SortCompare(Item1, Item2: Pointer): ShortInt; inline;
  begin
    Result := Comp(PInt64HashListData(Item1)^.id, PInt64HashListData(Item2)^.id);
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer);
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while SortCompare(SortList[i], p) < 0 do
            Inc(i);
        while SortCompare(SortList[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i  : Integer;
  j  : Integer;
  lst: TCoreClassList;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Capacity := Count;
      for i := low(FListBuffer) to high(FListBuffer) do
        if FListBuffer[i] <> nil then
          begin
            lst := FListBuffer[i];
            if lst.Count > 0 then
              for j := lst.Count - 1 downto 0 do
                  OutputList.Add(lst.Items[j]);
          end;

      if OutputList.Count > 1 then
          QuickSortList(OutputList.ListData^, 0, OutputList.Count - 1);
    end;
end;

procedure TInt64HashList.FastGetListData(OutputList: TCoreClassList);
var
  i  : Integer;
  j  : Integer;
  lst: TCoreClassList;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Capacity := Count;
      for i := low(FListBuffer) to high(FListBuffer) do
        if FListBuffer[i] <> nil then
          begin
            lst := FListBuffer[i];
            if lst.Count > 0 then
              for j := lst.Count - 1 downto 0 do
                  OutputList.Add(lst.Items[j]);
          end;
    end;
end;

procedure TInt64HashList.Delete(i64: Int64);
var
  newhash  : THash;
  i        : Integer;
  lst      : TCoreClassList;
  _ItemData: PInt64HashListData;
begin
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
end;

function TInt64HashList.Add(i64: Int64; _CustomData: TCoreClassObject; overwrite: Boolean = True): PInt64HashListData;
var
  newhash: THash;
  lst, nl: TCoreClassList;
  i, j   : Integer;
  pData  : PInt64HashListData;
begin
  newhash := MakeHash(i64);

  lst := GetListTable(newhash, True);
  if (lst.Count > 0) and (overwrite) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListData(lst.Items[i]);
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
              Result := pData;

              if (pData^.id < FIDCounter - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.id := FIDCounter;

                  if FIDCounter > FIDCounter + 1 then
                    begin
                      // rebuild idcounter
                      nl := TCoreClassList.Create;
                      GetListData(nl);
                      for j := 0 to nl.Count - 1 do
                          PInt64HashListData(nl[j])^.id := j + 1;
                      FIDCounter := nl.Count + 1;
                      DisposeObject(nl);
                    end
                  else
                      Inc(FIDCounter);
                end;

              Exit;
            end;
        end;
    end;

  New(pData);
  pData^.qHash := newhash;
  pData^.i64 := i64;
  pData^.Data := _CustomData;
  pData^.id := FIDCounter;
  pData^.OwnerList := lst;

  lst.Add(pData);
  Result := pData;
  Inc(FCount);

  if FIDCounter > FIDCounter + 1 then
    begin
      // rebuild idcounter
      nl := TCoreClassList.Create;
      GetListData(nl);
      for j := 0 to nl.Count - 1 do
          PInt64HashListData(nl[j])^.id := j + 1;
      FIDCounter := nl.Count + 1;
      DisposeObject(nl);
    end
  else
      Inc(FIDCounter);
end;

function TInt64HashList.Find(i64: Int64): TCoreClassObject;
var
  i    : Integer;
  j    : Integer;
  lst  : TCoreClassList;
  pData: PInt64HashListData;
begin
  Result := nil;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := PInt64HashListData(lst.Items[j]);
                  if i64 = pData^.i64 then
                    begin
                      Result := pData^.Data;
                      Exit;
                    end;
                end;
            end;
        end;
    end;
end;

function TInt64HashList.Exists(i64: Int64): Boolean;
var
  newhash: THash;
  i      : Integer;
  lst    : TCoreClassList;
  pData  : PInt64HashListData;
begin
  Result := False;
  newhash := MakeHash(i64);
  lst := GetListTable(newhash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PInt64HashListData(lst.Items[i]);
            if (newhash = pData^.qHash) and (i64 = pData^.i64) then
                Exit(True);
          end;
    end;
end;

procedure TInt64HashList.SetHashBlockCount(cnt: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, cnt);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function TInt64HashList.First: PInt64HashListData;
var
  i  : Integer;
  mv : TCounter;
  lst: TCoreClassList;
begin
  mv := FIDCounter;
  Result := nil;
  for i := 0 to Length(FListBuffer) - 1 do
    begin
      lst := FListBuffer[i];
      if (lst <> nil) and (lst.Count > 0) then
        if PInt64HashListData(lst[0])^.id <= mv then
          begin
            Result := lst[0];
            mv := Result^.id;
          end;
    end;
end;

function TInt64HashList.Last: PInt64HashListData;
var
  i  : Integer;
  mv : TCounter;
  lst: TCoreClassList;
begin
  mv := 0;
  Result := nil;
  for i := 0 to Length(FListBuffer) - 1 do
    begin
      lst := FListBuffer[i];
      if (lst <> nil) and (lst.Count > 0) then
        if PInt64HashListData(lst[lst.Count - 1])^.id >= mv then
          begin
            Result := lst[0];
            mv := Result^.id;
          end;
    end;
end;

procedure TInt64HashList.DeleteFirst(c: Integer);
var
  p: PInt64HashListData;
begin
  while c > 0 do
    begin
      p := First;

      if (p <> nil) and (p^.OwnerList[0] = p) then
        begin
          p^.OwnerList.Delete(0);
          if (FAutoFreeData) and (p^.Data <> nil) then
            begin
              try
                DoDataFreeProc(p^.Data);
                p^.Data := nil;
              except
              end;
            end;
          Dispose(p);
          Dec(FCount);
        end;
      Dec(c);
    end;
end;

procedure TInt64HashList.DeleteLast(c: Integer);
var
  p: PInt64HashListData;
begin
  while c > 0 do
    begin
      p := Last;

      if (p <> nil) and (p^.OwnerList[p^.OwnerList.Count - 1] = p) then
        begin
          p^.OwnerList.Delete(p^.OwnerList.Count - 1);
          if (FAutoFreeData) and (p^.Data <> nil) then
            begin
              try
                DoDataFreeProc(p^.Data);
                p^.Data := nil;
              except
              end;
            end;
          Dispose(p);
          Dec(FCount);
        end;
      Dec(c);
    end;
end;

procedure TInt64HashList.Recache;
begin
  if FCount > FMaxCache then
      Clear;
end;

function TInt64HashList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TInt64HashList.PrintHashReport;
var
  i                 : NativeInt;
  L                 : TCoreClassList;
  total             : NativeInt;
  usaged, aMax, aMin: NativeInt;
  inited            : Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          Inc(usaged);
          total := total + L.Count;
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
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, total, aMax, aMin]));
end;

function THashObjectList.GetCount: Integer;
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

function THashObjectList.GetNames(Name: SystemString): TCoreClassObject;
var
  pObjData: PHashObjectListData;
begin
  if name = '' then
    begin
      Result := nil;
      Exit;
    end;
  pObjData := FHashList.Names[name];
  if pObjData <> nil then
      Result := pObjData^.obj
  else
      Result := nil;
end;

procedure THashObjectList.SetNames(Name: SystemString; const Value: TCoreClassObject);
begin
  Add(name, Value);
end;

function THashObjectList.GetOnChange(Name: SystemString): THashObjectChangeEvent;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.Names[name];
  if pObjData <> nil then
      Result := pObjData^.OnChnage
  else
      Result := nil;
end;

procedure THashObjectList.SetOnChange(Name: SystemString; const aValue: THashObjectChangeEvent);
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.Names[name];
  if pObjData = nil then
    begin
      New(pObjData);
      pObjData^.OnChnage := aValue;
      pObjData^.obj := nil;
      FHashList.Add(name, pObjData, False);
    end
  else
      pObjData^.OnChnage := aValue;
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

  {$IFDEF FPC}
  FHashList.OnDataFreeProc := @DefaultDataFreeProc;
  {$ELSE}
  FHashList.OnDataFreeProc := DefaultDataFreeProc;
  {$ENDIF}
  FAutoFreeObject := _AutoFreeObject;
  FIncremental := 0;
end;

constructor THashObjectList.Create(_AutoFreeObject: Boolean; MaxHashBlock: Integer);
begin
  inherited Create;
  FHashList := THashList.Create(MaxHashBlock);
  FHashList.FAutoFreeData := True;

  {$IFDEF FPC}
  FHashList.OnDataFreeProc := @DefaultDataFreeProc;
  {$ELSE}
  FHashList.OnDataFreeProc := DefaultDataFreeProc;
  {$ENDIF}
  FAutoFreeObject := _AutoFreeObject;
  FIncremental := 0;
end;

destructor THashObjectList.Destroy;
begin
  Clear;
  DisposeObject(FHashList);
  inherited Destroy;
end;

procedure THashObjectList.Clear;
var
  lst     : TCoreClassList;
  pObjData: PHashObjectListData;
  i       : Integer;
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
                if pObjData^.obj <> nil then
                  begin
                    try
                        DisposeObject(pObjData^.obj);
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
  lst     : TCoreClassList;
  pObjData: PHashObjectListData;
  i       : Integer;
begin
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  OutputList.Clear;
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pObjData := Data;
            OutputList.Add(OriginName);
          end;
      end;
  DisposeObject(lst);
end;

procedure THashObjectList.GetNameList(OutputList: TListString);
var
  lst     : TCoreClassList;
  pObjData: PHashObjectListData;
  i       : Integer;
begin
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  OutputList.Clear;
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pObjData := Data;
            OutputList.Add(OriginName);
          end;
      end;
  DisposeObject(lst);
end;

procedure THashObjectList.GetNameList(OutputList: TListPascalString);
var
  lst     : TCoreClassList;
  pObjData: PHashObjectListData;
  i       : Integer;
begin
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  OutputList.Clear;
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pObjData := Data;
            OutputList.Add(OriginName);
          end;
      end;
  DisposeObject(lst);
end;

procedure THashObjectList.GetListData(OutputList: TCoreClassStrings);
var
  lst     : TCoreClassList;
  pObjData: PHashObjectListData;
  i       : Integer;
begin
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  OutputList.Clear;
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pObjData := Data;
            OutputList.AddObject(OriginName, pObjData^.obj);
          end;
      end;
  DisposeObject(lst);
end;

procedure THashObjectList.GetAsList(OutputList: TCoreClassListForObj);
var
  lst     : TCoreClassList;
  pObjData: PHashObjectListData;
  i       : Integer;
begin
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  OutputList.Clear;
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pObjData := Data;
            OutputList.Add(pObjData^.obj);
          end;
      end;
  DisposeObject(lst);
end;

procedure THashObjectList.FastGetAsList(OutputList: TCoreClassListForObj);
var
  lst     : TCoreClassList;
  pObjData: PHashObjectListData;
  i       : Integer;
begin
  lst := TCoreClassList.Create;
  FHashList.FastGetListData(lst);
  OutputList.Clear;
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pObjData := Data;
            OutputList.Add(pObjData^.obj);
          end;
      end;
  DisposeObject(lst);
end;

function THashObjectList.GetObjAsName(obj: TCoreClassObject): SystemString;
var
  lst: TCoreClassList;
  i  : Integer;
begin
  Result := '';
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            if PHashObjectListData(Data)^.obj = obj then
              begin
                Result := OriginName;
                Break;
              end;
          end;
      end;
  DisposeObject(lst);
end;

procedure THashObjectList.Delete(Name: SystemString);
var
  pObjData: PHashObjectListData;
begin
  if AutoFreeObject then
    begin
      pObjData := FHashList.Names[name];
      if pObjData <> nil then
        begin
          if pObjData^.obj <> nil then
            begin
              try
                DisposeObject(pObjData^.obj);
                pObjData^.obj := nil;
              except
              end;
            end;
        end;
    end;
  FHashList.Delete(name);
end;

function THashObjectList.Add(Name: SystemString; _Object: TCoreClassObject): TCoreClassObject;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.Names[name];
  if pObjData <> nil then
    begin
      try
        if Assigned(pObjData^.OnChnage) then
            pObjData^.OnChnage(Self, name, pObjData^.obj, _Object);
      except
      end;

      if (FAutoFreeObject) and (pObjData^.obj <> nil) then
        begin
          try
            DisposeObject(pObjData^.obj);
            pObjData^.obj := nil;
          except
          end;
        end;
    end
  else
    begin
      New(pObjData);
      pObjData^.OnChnage := nil;
      FHashList.Add(name, pObjData, False);
    end;

  pObjData^.obj := _Object;
  Result := _Object;
end;

function THashObjectList.Find(Name: SystemString): TCoreClassObject;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.Find(name);
  if pObjData <> nil then
      Result := pObjData^.obj
  else
      Result := nil;
end;

function THashObjectList.Exists(Name: SystemString): Boolean;
var
  pObjData: PHashObjectListData;
begin
  if name <> '' then
    begin
      pObjData := FHashList.Names[name];
      if pObjData <> nil then
          Result := pObjData^.obj <> nil
      else
          Result := False;
    end
  else
      Result := False;
end;

function THashObjectList.ExistsObject(obj: TCoreClassObject): Boolean;
var
  lst: TCoreClassList;
  i  : Integer;
begin
  Result := False;
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            if PHashObjectListData(Data)^.obj = obj then
              begin
                Result := True;
                Break;
              end;
          end;
      end;
  DisposeObject(lst);
end;

procedure THashObjectList.CopyFrom(Source: THashObjectList);
var
  lst     : TCoreClassList;
  pObjData: PHashObjectListData;
  i       : Integer;
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
              Names[OriginName] := pObjData^.obj;
            end;
      end;
  DisposeObject(lst);
end;

function THashObjectList.ReName(_OLDName, _NewName: SystemString): Boolean;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.Names[_OLDName];
  Result := (_OLDName <> _NewName) and (pObjData <> nil) and (FHashList.Names[_NewName] = nil);
  if Result then
    begin
      Add(_NewName, pObjData^.obj);
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

function THashVariantList.GetCount: Integer;
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

function THashVariantList.GetNames(Name: SystemString): Variant;
var
  pVarData: PHashVariantListData;
begin
  if name = '' then
    begin
      Result := Null;
      Exit;
    end;
  pVarData := FHashList.Names[name];
  if pVarData <> nil then
      Result := pVarData^.V
  else
      Result := Null;
end;

procedure THashVariantList.SetNames(Name: SystemString; const Value: Variant);
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Names[name];

  if pVarData = nil then
    begin
      New(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(name, pVarData, False);
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, name, Null, Value);
    end
  else
    begin
      if Assigned(pVarData^.OnChnage) then
        begin
          try
              pVarData^.OnChnage(Self, name, pVarData^.V, Value);
          except
          end;
        end;
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, name, pVarData^.V, Value);
    end;
  pVarData^.V := Value;
end;

function THashVariantList.GetOnChange(Name: SystemString): THashVariantChangeEvent;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Names[name];
  if pVarData <> nil then
      Result := pVarData^.OnChnage
  else
      Result := nil;
end;

procedure THashVariantList.SetOnChange(Name: SystemString; const aValue: THashVariantChangeEvent);
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Names[name];
  if pVarData = nil then
    begin
      New(pVarData);
      pVarData^.V := Null;
      pVarData^.OnChnage := aValue;
      FHashList.Add(name, pVarData, False);
    end
  else
      pVarData^.OnChnage := aValue;
end;

procedure THashVariantList.DefaultDataFreeProc(p: Pointer);
begin
  Dispose(PHashVariantListData(p));
end;

constructor THashVariantList.Create;
begin
  inherited Create;
  FHashList := THashList.Create;
  FHashList.FAutoFreeData := True;

  {$IFDEF FPC}
  FHashList.OnDataFreeProc := @DefaultDataFreeProc;
  {$ELSE}
  FHashList.OnDataFreeProc := DefaultDataFreeProc;
  {$ENDIF}
  FAutoUpdateDefaultValue := False;
  FOnValueChangeNotify := nil;
end;

constructor THashVariantList.Create(MaxHashBlock: Integer);
begin
  inherited Create;
  FHashList := THashList.Create(MaxHashBlock);
  FHashList.FAutoFreeData := True;

  {$IFDEF FPC}
  FHashList.OnDataFreeProc := @DefaultDataFreeProc;
  {$ELSE}
  FHashList.OnDataFreeProc := DefaultDataFreeProc;
  {$ENDIF}
  FAutoUpdateDefaultValue := False;
  FOnValueChangeNotify := nil;
end;

destructor THashVariantList.Destroy;
begin
  DisposeObject(FHashList);
  inherited Destroy;
end;

procedure THashVariantList.Clear;
begin
  FHashList.Clear;
end;

procedure THashVariantList.GetNameList(OutputList: TCoreClassStrings);
var
  i  : Integer;
  lst: TCoreClassList;
begin
  OutputList.Clear;
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
        OutputList.Append(PHashListData(lst[i])^.OriginName);
  DisposeObject(lst);
end;

procedure THashVariantList.GetNameList(OutputList: TListString);
var
  i  : Integer;
  lst: TCoreClassList;
begin
  OutputList.Clear;
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
        OutputList.Add(PHashListData(lst[i])^.OriginName);
  DisposeObject(lst);
end;

procedure THashVariantList.GetNameList(OutputList: TListPascalString);
var
  i  : Integer;
  lst: TCoreClassList;
begin
  OutputList.Clear;
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
        OutputList.Add(PHashListData(lst[i])^.OriginName);
  DisposeObject(lst);
end;

procedure THashVariantList.Delete(Name: SystemString);
begin
  FHashList.Delete(name);
end;

function THashVariantList.Add(Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Names[name];
  if pVarData <> nil then
    begin
      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, name, pVarData^.V, V);
      except
      end;
    end
  else
    begin
      New(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(name, pVarData, True);
    end;

  pVarData^.V := V;
  Result := V;
end;

function THashVariantList.FastAdd(Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  New(pVarData);
  pVarData^.OnChnage := nil;
  FHashList.Add(name, pVarData, False);

  pVarData^.V := V;
  Result := V;
end;

function THashVariantList.Find(Name: SystemString): Variant;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Find(name);
  if pVarData <> nil then
      Result := pVarData^.V
  else
      Result := Null;
end;

function THashVariantList.FindValue(aValue: Variant): SystemString;
var
  i       : Integer;
  lst     : TCoreClassList;
  pVarData: PHashVariantListData;
begin
  Result := '';
  lst := TCoreClassList.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        pVarData := PHashListData(lst[i])^.Data;
        if umlSameVarValue(aValue, pVarData^.V) then
          begin
            Result := PHashListData(lst[i])^.OriginName;
            Break;
          end;
      end;
  DisposeObject(lst);
end;

function THashVariantList.Exists(Name: SystemString): Boolean;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Names[name];
  if pVarData = nil then
      Result := False
  else
      Result := not VarIsEmpty(pVarData^.V);
end;

procedure THashVariantList.CopyFrom(Source: THashVariantList);
var
  lst     : TCoreClassList;
  pVarData: PHashVariantListData;
  i       : Integer;
begin
  lst := TCoreClassList.Create;
  Source.HashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pVarData := Data;
            Names[OriginName] := pVarData^.V;
          end;
      end;
  DisposeObject(lst);
end;

function THashVariantList.GetType(Name: SystemString): Word;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Find(name);
  if pVarData = nil then
      Result := varEmpty
  else
      Result := VarType(pVarData^.V);
end;

function THashVariantList.IncValue(Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Names[name];
  if pVarData <> nil then
    begin
      Result := pVarData^.V + V;

      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, name, pVarData^.V, Result);
      except
      end;

      pVarData^.V := Result;
    end
  else
    begin
      Result := V;

      New(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.V := Result;
      FHashList.Add(name, pVarData, True);
    end;
end;

procedure THashVariantList.IncValue(vl: THashVariantList);
var
  lst: TCoreClassList;
  i  : Integer;
  p  : PHashListData;
begin
  lst := TCoreClassList.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      IncValue(p^.OriginName, PHashVariantListData(p^.Data)^.V);
    end;
  DisposeObject(lst);
end;

function THashVariantList.SetMax(Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
  r       : Boolean;
begin
  pVarData := FHashList.Names[name];
  if pVarData <> nil then
    begin
      try
          r := V > pVarData^.V;
      except
          r := True;
      end;

      if r then
        begin
          Result := V;
          try
            if Assigned(pVarData^.OnChnage) then
                pVarData^.OnChnage(Self, name, pVarData^.V, Result);
          except
          end;

          pVarData^.V := Result;
        end;
    end
  else
    begin
      Result := V;

      New(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.V := Result;
      FHashList.Add(name, pVarData, True);
    end;
end;

procedure THashVariantList.SetMax(vl: THashVariantList);
var
  lst: TCoreClassList;
  i  : Integer;
  p  : PHashListData;
begin
  lst := TCoreClassList.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      SetMax(p^.OriginName, PHashVariantListData(p^.Data)^.V);
    end;
  DisposeObject(lst);
end;

function THashVariantList.SetMin(Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
  r       : Boolean;
begin
  pVarData := FHashList.Names[name];
  if pVarData <> nil then
    begin
      try
          r := V < pVarData^.V;
      except
          r := True;
      end;

      if r then
        begin
          Result := V;
          try
            if Assigned(pVarData^.OnChnage) then
                pVarData^.OnChnage(Self, name, pVarData^.V, Result);
          except
          end;

          pVarData^.V := Result;
        end;
    end
  else
    begin
      Result := V;

      New(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.V := Result;
      FHashList.Add(name, pVarData, True);
    end;
end;

procedure THashVariantList.SetMin(vl: THashVariantList);
var
  lst: TCoreClassList;
  i  : Integer;
  p  : PHashListData;
begin
  lst := TCoreClassList.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      SetMin(p^.OriginName, PHashVariantListData(p^.Data)^.V);
    end;
  DisposeObject(lst);
end;

function THashVariantList.GetDefaultValue(Name: SystemString; aValue: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  try
    if name = '' then
      begin
        Result := aValue;
        Exit;
      end;
    pVarData := FHashList.Names[name];
    if pVarData <> nil then
      begin
        if VarIsNULL(pVarData^.V) then
          begin
            Result := aValue;
            if FAutoUpdateDefaultValue then
                SetNames(name, aValue);
          end
        else
          begin
            Result := pVarData^.V;
          end;
      end
    else
      begin
        Result := aValue;
        if FAutoUpdateDefaultValue then
            SetNames(name, aValue);
      end;
  except
      Result := aValue;
  end;
end;

procedure THashVariantList.SetDefaultValue(Name: SystemString; aValue: Variant);
begin
  SetNames(name, aValue);
end;

function THashVariantList.ReplaceMacro(const AText, HeadFlag, TailFlag: SystemString; var Output: SystemString): Boolean;
var
  Sour      : umlString;
  hf, tf    : umlString;
  bPos, ePos: Integer;
  KeyText   : SystemString;
  i         : Integer;
begin
  Output := '';
  Sour.Text := AText;
  hf.Text := HeadFlag;
  tf.Text := TailFlag;
  Result := True;

  i := 1;

  while i <= Sour.Len do
    begin
      if Sour.ComparePos(i, hf) then
        begin
          bPos := i;
          ePos := Sour.GetPos(tf, i + hf.Len);
          if ePos > 0 then
            begin
              KeyText := Sour.copy(bPos + hf.Len, ePos - (bPos + hf.Len)).Text;

              if Exists(KeyText) then
                begin
                  Output := Output + VarToStr(GetNames(KeyText));
                  i := ePos + tf.Len;
                  Continue;
                end
              else
                begin
                  Result := False;
                end;
            end;
        end;

      Output := Output + Sour[i];
      Inc(i);
    end;
end;

procedure THashVariantList.LoadFromStream(Stream: TCoreClassStream);
var
  vt: THashVariantTextStream;
begin
  vt := THashVariantTextStream.Create(Self);
  vt.LoadFromStream(Stream);
  DisposeObject(vt);
end;

procedure THashVariantList.SaveToStream(Stream: TCoreClassStream);
var
  vt: THashVariantTextStream;
begin
  vt := THashVariantTextStream.Create(Self);
  vt.SaveToStream(Stream);
  DisposeObject(vt);
end;

procedure THashVariantList.LoadFromFile(FileName: SystemString);
var
  vt: THashVariantTextStream;
begin
  vt := THashVariantTextStream.Create(Self);
  vt.LoadFromFile(FileName);
  DisposeObject(vt);
end;

procedure THashVariantList.SaveToFile(FileName: SystemString);
var
  vt: THashVariantTextStream;
begin
  vt := THashVariantTextStream.Create(Self);
  vt.SaveToFile(FileName);
  DisposeObject(vt);
end;

procedure THashVariantList.ExportAsStrings(Output: TCoreClassStrings);
var
  vt: THashVariantTextStream;
begin
  vt := THashVariantTextStream.Create(Self);
  vt.DataExport(Output);
  DisposeObject(vt);
end;

procedure THashVariantList.ImportFromStrings(Output: TCoreClassStrings);
var
  vt: THashVariantTextStream;
begin
  vt := THashVariantTextStream.Create(Self);
  vt.DataImport(Output);
  DisposeObject(vt);
end;

function THashVariantList.GetAsText: SystemString;
var
  vt: THashVariantTextStream;
begin
  vt := THashVariantTextStream.Create(Self);
  vt.SaveToText(Result);
  DisposeObject(vt);
end;

procedure THashVariantList.SetAsText(const Value: SystemString);
var
  vt: THashVariantTextStream;
begin
  vt := THashVariantTextStream.Create(Self);
  vt.LoadFromText(Value);
  DisposeObject(vt);
end;

function THashVariantTextStream.GetNames(aName: SystemString): Variant;
begin
  if FVariantList <> nil then
      Result := FVariantList[aName]
  else
      Result := Null;
end;

procedure THashVariantTextStream.SetNames(aName: SystemString; const Value: Variant);
begin
  if FVariantList <> nil then
      FVariantList[aName] := Value;
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

class function THashVariantTextStream.VToStr(V: Variant): SystemString;
var
  n, b64: umlString;
begin
  try
    case VarType(V) of
      varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord:
        begin
          Result := IntToStr(V);
        end;
      varInt64:
        begin
          Result := IntToStr(Int64(V));
        end;
      varUInt64:
        begin
          {$IFDEF FPC}
          Result := IntToStr(UInt64(V));
          {$ELSE}
          Result := UIntToStr(UInt64(V));
          {$ENDIF}
        end;
      varSingle, varDouble, varCurrency, varDate:
        begin
          Result := FloatToStr(V);
        end;
      varOleStr, varString, varUString:
        begin
          n.Text := VarToStr(V);

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
          Result := BoolToStr(V, True);
        end;
      else
        Result := VarToStr(V);
    end;
  except
    try
        Result := VarToStr(V);
    except
        Result := '';
    end;
  end;
end;

class function THashVariantTextStream.StrToV(s: SystemString): Variant;
var
  n, b64: umlString;
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

procedure THashVariantTextStream.DataImport(TextList: TCoreClassStrings);
var
  i                  : Integer;
  n                  : TPascalString;
  TextName, TextValue: TPascalString;
begin
  if FVariantList = nil then
      Exit;
  if TextList.Count > 0 then
    for i := 0 to TextList.Count - 1 do
      begin
        n := umlTrimSpace(TextList[i]);

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

procedure THashVariantTextStream.DataExport(TextList: TCoreClassStrings);
var
  i        : Integer;
  vl       : TCoreClassList;
  TextValue: SystemString;
begin
  if FVariantList = nil then
      Exit;
  vl := TCoreClassList.Create;
  FVariantList.HashList.GetListData(vl);
  if vl.Count > 0 then
    for i := 0 to vl.Count - 1 do
      begin
        TextValue := VToStr(PHashVariantListData(PHashListData(vl[i])^.Data)^.V);

        if TextValue <> '' then
            TextList.Add((PHashListData(vl[i])^.OriginName + '=' + TextValue))
        else
            TextList.Add(PHashListData(vl[i])^.OriginName);
      end;
  DisposeObject(vl);
end;

procedure THashVariantTextStream.LoadFromStream(Stream: TCoreClassStream);
var
  n: TCoreClassStrings;
begin
  if FVariantList = nil then
      Exit;
  n := TCoreClassStringList.Create;
  {$IFDEF FPC}
  n.LoadFromStream(Stream);
  {$ELSE}
  n.LoadFromStream(Stream, TEncoding.UTF8);
  {$ENDIF}
  DataImport(n);
  DisposeObject(n);
end;

procedure THashVariantTextStream.SaveToStream(Stream: TCoreClassStream);
var
  n: TCoreClassStrings;
begin
  if FVariantList = nil then
      Exit;
  n := TCoreClassStringList.Create;
  DataExport(n);
  {$IFDEF FPC}
  n.SaveToStream(Stream);
  {$ELSE}
  n.SaveToStream(Stream, TEncoding.UTF8);
  {$ENDIF}
  DisposeObject(n);
end;

procedure THashVariantTextStream.LoadFromFile(FileName: SystemString);
var
  ns: TCoreClassStream;
begin
  ns := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashVariantTextStream.SaveToFile(FileName: SystemString);
var
  ns: TCoreClassStream;
begin
  ns := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashVariantTextStream.LoadFromText(AText: SystemString);
var
  n: TCoreClassStrings;
begin
  if FVariantList = nil then
      Exit;
  n := TCoreClassStringList.Create;
  n.Text := AText;
  DataImport(n);
  DisposeObject(n);
end;

procedure THashVariantTextStream.SaveToText(var AText: SystemString);
var
  n: TCoreClassStrings;
begin
  if FVariantList = nil then
      Exit;
  n := TCoreClassStringList.Create;
  DataExport(n);
  AText := n.Text;
  DisposeObject(n);
end;

function THashVariantTextStream.Text: SystemString;
begin
  SaveToText(Result);
end;

function THashVariantTextStream.GetValue(aName: SystemString; V: Variant): Variant;
begin
  Result := Names[aName];
  if VarIsNULL(Result) then
    begin
      Names[aName] := V;
      Result := V;
    end;
end;

function TListCardinal.GetItems(Idx: Integer): Cardinal;
begin
  with PListCardinalData(FList[Idx])^ do
      Result := Data;
end;

procedure TListCardinal.SetItems(Idx: Integer; Value: Cardinal);
begin
  with PListCardinalData(FList[Idx])^ do
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
  New(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

function TListCardinal.Delete(Idx: Integer): Integer;
var
  p: PListCardinalData;
begin
  p := FList[Idx];
  Dispose(p);
  FList.Delete(Idx);
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
begin
  while Count > 0 do
      Delete(0);
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

function TListInt64.GetItems(Idx: Integer): Int64;
begin
  with PListInt64Data(FList[Idx])^ do
      Result := Data;
end;

procedure TListInt64.SetItems(Idx: Integer; Value: Int64);
begin
  with PListInt64Data(FList[Idx])^ do
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
  New(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

function TListInt64.Delete(Idx: Integer): Integer;
var
  p: PListInt64Data;
begin
  p := FList[Idx];
  Dispose(p);
  FList.Delete(Idx);
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
begin
  while Count > 0 do
      Delete(0);
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

procedure TListInt64.SaveToStream(Stream: TCoreClassStream);
var
  i: Integer;
  c: Integer;
begin
  c := FList.Count;
  Stream.Write(c, umlIntegerLength);
  for i := 0 to FList.Count - 1 do
      Stream.Write(PListInt64Data(FList[i])^.Data, umlInt64Length);
end;

procedure TListInt64.LoadFromStream(Stream: TCoreClassStream);
var
  i: Integer;
  c: Integer;
  V: Int64;
begin
  Stream.Read(c, umlIntegerLength);
  for i := 0 to c - 1 do
    begin
      Stream.Read(V, umlInt64Length);
      Add(V);
    end;
end;

function TListNativeInt.GetItems(Idx: Integer): NativeInt;
begin
  with PListNativeIntData(FList[Idx])^ do
      Result := Data;
end;

procedure TListNativeInt.SetItems(Idx: Integer; Value: NativeInt);
begin
  with PListNativeIntData(FList[Idx])^ do
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

function TListNativeInt.Add(Value: NativeInt): Integer;
var
  p: PListNativeIntData;
begin
  New(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

function TListNativeInt.Delete(Idx: Integer): Integer;
var
  p: PListNativeIntData;
begin
  p := FList[Idx];
  Dispose(p);
  FList.Delete(Idx);
  Result := Count;
end;

function TListNativeInt.DeleteNativeInt(Value: NativeInt): Integer;
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
begin
  while Count > 0 do
      Delete(0);
end;

function TListNativeInt.Count: Integer;
begin
  Result := FList.Count;
end;

function TListNativeInt.ExistsValue(Value: NativeInt): Integer;
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

function TListInteger.GetItems(Idx: Integer): Integer;
begin
  with PListIntegerData(FList[Idx])^ do
      Result := Data;
end;

procedure TListInteger.SetItems(Idx: Integer; Value: Integer);
begin
  with PListIntegerData(FList[Idx])^ do
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
  New(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

function TListInteger.Delete(Idx: Integer): Integer;
var
  p: PListIntegerData;
begin
  p := FList[Idx];
  Dispose(p);
  FList.Delete(Idx);
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
begin
  while Count > 0 do
      Delete(0);
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

function TListDouble.GetItems(Idx: Integer): Double;
begin
  with PListDoubleData(FList[Idx])^ do
      Result := Data;
end;

procedure TListDouble.SetItems(Idx: Integer; Value: Double);
begin
  with PListDoubleData(FList[Idx])^ do
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
  New(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

function TListDouble.Delete(Idx: Integer): Integer;
var
  p: PListDoubleData;
begin
  p := FList[Idx];
  Dispose(p);
  FList.Delete(Idx);
  Result := Count;
end;

function TListDouble.DeleteDouble(Value: Double): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    begin
      if Math.SameValue(Items[i], Value) then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListDouble.Clear;
begin
  while Count > 0 do
      Delete(0);
end;

function TListDouble.Count: Integer;
begin
  Result := FList.Count;
end;

function TListDouble.ExistsValue(Value: Double): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
    if Math.SameValue(Items[i], Value) then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListDouble.Assign(SameObj: TListDouble);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

function TListPointer.GetItems(Idx: Integer): Pointer;
begin
  with PListPointerData(FList[Idx])^ do
      Result := Data;
end;

procedure TListPointer.SetItems(Idx: Integer; Value: Pointer);
begin
  with PListPointerData(FList[Idx])^ do
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
  New(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

function TListPointer.Delete(Idx: Integer): Integer;
var
  p: PListPointerData;
begin
  p := FList[Idx];
  Dispose(p);
  FList.Delete(Idx);
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
begin
  while Count > 0 do
      Delete(0);
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

function TListString.GetItems(Idx: Integer): SystemString;
begin
  with PListStringData(FList[Idx])^ do
      Result := Data;
end;

procedure TListString.SetItems(Idx: Integer; Value: SystemString);
begin
  with PListStringData(FList[Idx])^ do
    begin
      Data := Value;
      Hash := MakeHash(Value);
    end;
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
  New(p);
  p^.Data := Value;
  p^.Hash := MakeHash(Value);
  Result := FList.Add(p);
end;

function TListString.Delete(Idx: Integer): Integer;
var
  p: PListStringData;
begin
  p := FList[Idx];
  Dispose(p);
  FList.Delete(Idx);
  Result := Count;
end;

function TListString.DeleteString(Value: SystemString): Integer;
var
  i: Integer;
  h: THash;
  p: PListStringData;
begin
  i := 0;
  h := MakeHash(Value);

  while i < Count do
    begin
      if (PListStringData(FList[i])^.Hash = h) and (SameText(PListStringData(FList[i])^.Data, Value)) then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListString.Clear;
begin
  while Count > 0 do
      Delete(0);
end;

function TListString.Count: Integer;
begin
  Result := FList.Count;
end;

function TListString.ExistsValue(Value: SystemString): Integer;
var
  i: Integer;
  h: THash;
  p: PListStringData;
begin
  h := MakeHash(Value);

  Result := -1;

  for i := 0 to Count - 1 do
    if (PListStringData(FList[i])^.Hash = h) and (SameText(PListStringData(FList[i])^.Data, Value)) then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListString.Assign(SameObj: TListString);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

function TListPascalString.GetItems(Idx: Integer): TPascalString;
begin
  with PListPascalStringData(FList[Idx])^ do
      Result := Data;
end;

procedure TListPascalString.SetItems(Idx: Integer; Value: TPascalString);
begin
  with PListPascalStringData(FList[Idx])^ do
    begin
      Data := Value;
      Hash := MakeHash(Value);
    end;
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

function TListPascalString.Add(Value: TPascalString): Integer;
var
  p: PListPascalStringData;
begin
  New(p);
  p^.Data := Value;
  p^.Hash := MakeHash(Value);
  Result := FList.Add(p);
end;

function TListPascalString.Delete(Idx: Integer): Integer;
var
  p: PListPascalStringData;
begin
  p := FList[Idx];
  Dispose(p);
  FList.Delete(Idx);
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
      if (PListPascalStringData(FList[i])^.Hash = h) and (PListPascalStringData(FList[i])^.Data.Same(Value)) then
          Delete(i)
      else
          Inc(i);
    end;
  Result := Count;
end;

procedure TListPascalString.Clear;
begin
  while Count > 0 do
      Delete(0);
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
    if (PListPascalStringData(FList[i])^.Hash = h) and (PListPascalStringData(FList[i])^.Data.Same(Value)) then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListPascalString.Assign(SameObj: TListPascalString);
var
  i: Integer;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
      Add(SameObj[i]);
end;

function TListVariant.GetItems(Idx: Integer): Variant;
begin
  with PListVariantData(FList[Idx])^ do
      Result := Data;
end;

procedure TListVariant.SetItems(Idx: Integer; Value: Variant);
begin
  with PListVariantData(FList[Idx])^ do
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
  New(p);
  p^.Data := Value;
  Result := FList.Add(p);
end;

function TListVariant.Delete(Idx: Integer): Integer;
var
  p: PListVariantData;
begin
  p := FList[Idx];
  Dispose(p);
  FList.Delete(Idx);
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

function TVariantToDataList.GetItems(id: Variant): Pointer;
var
  i: Integer;
  p: PVariantToDataListData;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if umlSameVarValue(p^.id, id) then
        begin
          Result := p^.Data;
          Break;
        end;
    end;
end;

procedure TVariantToDataList.SetItems(id: Variant; Value: Pointer);
var
  i: Integer;
  p: PVariantToDataListData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if umlSameVarValue(p^.id, id) then
        begin
          p^.Data := Value;
          Exit;
        end;
    end;

  New(p);
  p^.id := id;
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
  FOnDataFreeProc(p);
end;

constructor TVariantToDataList.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
  FAutoFreeData := True;
  {$IFDEF FPC}
  FOnDataFreeProc := @DefaultDataFreeProc;
  {$ELSE}
  FOnDataFreeProc := DefaultDataFreeProc;
  {$ENDIF}
end;

destructor TVariantToDataList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TVariantToDataList.Add(id: Variant; Data: Pointer): Boolean;
var
  p: PVariantToDataListData;
begin
  if not Exists(id) then
    begin
      New(p);
      p^.id := id;
      p^.Data := Data;
      FList.Add(p);
      Result := True;
    end
  else
      Result := False;
end;

function TVariantToDataList.Delete(id: Variant): Boolean;
var
  i: Integer;
  p: PVariantToDataListData;
begin
  Result := False;
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if umlSameVarValue(p^.id, id) then
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

function TVariantToDataList.Exists(id: Variant): Boolean;
var
  i: Integer;
  p: PVariantToDataListData;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if umlSameVarValue(p^.id, id) then
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
      _To.Add(p^.id);
    end;
end;

function TVariantToDataList.Count: Integer;
begin
  Result := FList.Count;
end;

function TVariantToVariantList.GetItems(id: Variant): Variant;
var
  p: PVariantToVariantListData;
begin
  p := FList.Items[id];
  if p <> nil then
      Result := p^.V
  else
      Result := Null;
end;

procedure TVariantToVariantList.SetItems(id: Variant; Value: Variant);
var
  p: PVariantToVariantListData;
begin
  p := FList.Items[id];
  if p <> nil then
      p^.V := Value
  else
      Add(id, Value);
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
  {$IFDEF FPC}
  FList.OnDataFreeProc := @DefaultDataFreeProc;
  {$ELSE}
  FList.OnDataFreeProc := DefaultDataFreeProc;
  {$ENDIF}
end;

destructor TVariantToVariantList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TVariantToVariantList.Add(id, aValue: Variant): Boolean;
var
  p: PVariantToVariantListData;
begin
  if FList.Exists(id) then
    begin
      p := FList[id];
    end
  else
    begin
      New(p);
      FList[id] := p;
    end;

  p^.V := aValue;

  Result := True;
end;

function TVariantToVariantList.Delete(id: Variant): Boolean;
begin
  Result := FList.Delete(id);
end;

procedure TVariantToVariantList.Clear;
begin
  FList.Clear;
end;

function TVariantToVariantList.Exists(id: Variant): Boolean;
begin
  Result := FList.Exists(id);
end;

procedure TVariantToVariantList.GetList(_To: TListVariant);
begin
  FList.GetList(_To);
end;

procedure TVariantToVariantList.GetValueList(_To: TListVariant);
var
  i           : Integer;
  pVarData    : PVariantToDataListData;
  pToValueData: PVariantToVariantListData;
begin
  for i := 0 to FList.FList.Count - 1 do
    begin
      pVarData := FList.FList[i];
      pToValueData := pVarData^.Data;
      _To.Add(pToValueData^.V);
    end;
end;

function TVariantToVariantList.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TVariantToVariantList.Assign(SameObj: TVariantToVariantList);
var
  _To: TListVariant;
  i  : Integer;
begin
  Clear;
  _To := TListVariant.Create;
  SameObj.GetList(_To);
  for i := 0 to _To.Count - 1 do
      Items[_To[i]] := SameObj[_To[i]];
  DisposeObject(_To);
end;

function TVariantToObjectList.GetItems(id: Variant): TCoreClassObject;
var
  p: PVariantToObjectListData;
begin
  p := FList.Items[id];
  if p <> nil then
      Result := p^.obj
  else
      Result := nil;
end;

procedure TVariantToObjectList.SetItems(id: Variant; Value: TCoreClassObject);
var
  p: PVariantToObjectListData;
begin
  p := FList.Items[id];
  if p <> nil then
      p^.obj := Value
  else
      Add(id, Value);
end;

procedure TVariantToObjectList.DefaultDataFreeProc(p: Pointer);
begin

end;

constructor TVariantToObjectList.Create;
begin
  inherited Create;
  FList := TVariantToDataList.Create;
  FList.FAutoFreeData := True;
  {$IFDEF FPC}
  FList.OnDataFreeProc := @DefaultDataFreeProc;
  {$ELSE}
  FList.OnDataFreeProc := DefaultDataFreeProc;
  {$ENDIF}
end;

destructor TVariantToObjectList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TVariantToObjectList.Add(id: Variant; obj: TCoreClassObject): Boolean;
var
  p: PVariantToObjectListData;
begin
  if FList.Exists(id) then
    begin
      p := FList[id];
    end
  else
    begin
      New(p);
      FList[id] := p;
    end;

  p^.obj := obj;

  Result := True;
end;

function TVariantToObjectList.Delete(id: Variant): Boolean;
begin
  Result := FList.Delete(id);
end;

procedure TVariantToObjectList.Clear;
begin
  FList.Clear;
end;

function TVariantToObjectList.Exists(id: Variant): Boolean;
begin
  Result := FList.Exists(id);
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
  i  : Integer;
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

  New(p);
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

  New(p);
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

  New(p);
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
