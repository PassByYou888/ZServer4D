{ ****************************************************************************** }
{ * json object library for delphi/objfpc                                      * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }

unit ZJson;

{$INCLUDE zDefine.inc}

interface

uses SysUtils,
  CoreClasses, PascalStrings,
{$IFDEF DELPHI}
  delphi_JsonDataObjects,
{$ELSE DELPHI}
  fpjson, jsonparser, jsonscanner,
{$ENDIF DELPHI}
  DoStatusIO,
  UnicodeMixedLib,
  MemoryStream64;

type
  TZ_JsonObject = class;

{$IFDEF DELPHI}
  TZ_Instance_JsonArray = TJsonArray;
  TZ_Instance_JsonObject = TJsonObject;
{$ELSE DELPHI}
  TZ_Instance_JsonArray = TJsonArray;
  TZ_Instance_JsonObject = TJsonObject;
{$ENDIF DELPHI}

  TZ_JsonBase = class
  protected
    FParent: TZ_JsonBase;
    FList: TCoreClassObjectList;
  public
    property Parent: TZ_JsonBase read FParent;
    constructor Create(Parent_: TZ_JsonBase); virtual;
    destructor Destroy; override;
  end;

  TZ_JsonArray = class(TZ_JsonBase)
  private
    FInstance: TZ_Instance_JsonArray;
  public
    property Instance: TZ_Instance_JsonArray read FInstance;
    constructor Create(Parent_: TZ_JsonBase); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Delete(Index: Integer);

    procedure Add(const v_: string); overload;
    procedure Add(const v_: TPascalString); overload;
    procedure Add(const v_: Integer); overload;
    procedure Add(const v_: Int64); overload;
    procedure Add(const v_: UInt64); overload;
    procedure AddF(const v_: Double); overload;
    procedure Add(const v_: TDateTime); overload;
    procedure Add(const v_: Boolean); overload;
    function AddArray: TZ_JsonArray;
    function AddObject: TZ_JsonObject; overload;

    procedure Insert(Index: Integer; const v_: string); overload;
    procedure Insert(Index: Integer; const v_: Integer); overload;
    procedure Insert(Index: Integer; const v_: Int64); overload;
    procedure Insert(Index: Integer; const v_: UInt64); overload;
    procedure Insert(Index: Integer; const v_: Double); overload;
    procedure Insert(Index: Integer; const v_: TDateTime); overload;
    procedure Insert(Index: Integer; const v_: Boolean); overload;
    function InsertArray(Index: Integer): TZ_JsonArray;
    function InsertObject(Index: Integer): TZ_JsonObject; overload;

    function GetString(Index: Integer): string;
    procedure SetString(Index: Integer; const Value: string);
    function GetInt(Index: Integer): Integer;
    procedure SetInt(Index: Integer; const Value: Integer);
    function GetLong(Index: Integer): Int64;
    procedure SetLong(Index: Integer; const Value: Int64);
    function GetULong(Index: Integer): UInt64;
    procedure SetULong(Index: Integer; const Value: UInt64);
    function GetFloat(Index: Integer): Double;
    procedure SetFloat(Index: Integer; const Value: Double);
    function GetDateTime(Index: Integer): TDateTime;
    procedure SetDateTime(Index: Integer; const Value: TDateTime);
    function GetBool(Index: Integer): Boolean;
    procedure SetBool(Index: Integer; const Value: Boolean);
    function GetArray(Index: Integer): TZ_JsonArray;
    function GetObject(Index: Integer): TZ_JsonObject;

    property S[Index: Integer]: string read GetString write SetString;
    property I[Index: Integer]: Integer read GetInt write SetInt;
    property I32[Index: Integer]: Integer read GetInt write SetInt;
    property L[Index: Integer]: Int64 read GetLong write SetLong;
    property I64[Index: Integer]: Int64 read GetLong write SetLong;
    property U[Index: Integer]: UInt64 read GetULong write SetULong;
    property U64[Index: Integer]: UInt64 read GetULong write SetULong;
    property F[Index: Integer]: Double read GetFloat write SetFloat;
    property D[Index: Integer]: TDateTime read GetDateTime write SetDateTime;
    property B[Index: Integer]: Boolean read GetBool write SetBool;
    property A[Index: Integer]: TZ_JsonArray read GetArray;
    property O[Index: Integer]: TZ_JsonObject read GetObject;

    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  TZ_JsonObject = class(TZ_JsonBase)
  private
    FInstance: TZ_Instance_JsonObject;
  public
    property Instance: TZ_Instance_JsonObject read FInstance;

    constructor Create(Parent_: TZ_JsonBase); overload; override;
    constructor Create(); overload;
    destructor Destroy; override;

    procedure Clear;
    function IndexOf(const Name: string): Integer;

    function GetString(const Name: string): string;
    procedure SetString(const Name, Value: string);
    function GetInt(const Name: string): Integer;
    procedure SetInt(const Name: string; const Value: Integer);
    function GetLong(const Name: string): Int64;
    procedure SetLong(const Name: string; const Value: Int64);
    function GetULong(const Name: string): UInt64;
    procedure SetULong(const Name: string; const Value: UInt64);
    function GetFloat(const Name: string): Double;
    procedure SetFloat(const Name: string; const Value: Double);
    function GetDateTime(const Name: string): TDateTime;
    procedure SetDateTime(const Name: string; const Value: TDateTime);
    function GetBool(const Name: string): Boolean;
    procedure SetBool(const Name: string; const Value: Boolean);
    function GetArray(const Name: string): TZ_JsonArray;
    function GetObject(const Name: string): TZ_JsonObject;

    property S[const Name: string]: string read GetString write SetString;
    property I[const Name: string]: Integer read GetInt write SetInt;
    property I32[const Name: string]: Integer read GetInt write SetInt;
    property L[const Name: string]: Int64 read GetLong write SetLong;
    property I64[const Name: string]: Int64 read GetLong write SetLong;
    property U[const Name: string]: UInt64 read GetULong write SetULong;
    property U64[const Name: string]: UInt64 read GetULong write SetULong;
    property F[const Name: string]: Double read GetFloat write SetFloat;
    property D[const Name: string]: TDateTime read GetDateTime write SetDateTime;
    property B[const Name: string]: Boolean read GetBool write SetBool;
    property A[const Name: string]: TZ_JsonArray read GetArray;
    property O[const Name: string]: TZ_JsonObject read GetObject;

    function GetName(Index: Integer): string;
    property Names[Index: Integer]: string read GetName; default;
    function GetCount: Integer;
    property Count: Integer read GetCount;

    procedure SaveToStream(stream: TCoreClassStream; Formated_: Boolean); overload;
    procedure SaveToStream(stream: TCoreClassStream); overload;
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToLines(L_: TCoreClassStrings);
    procedure LoadFromLines(L_: TCoreClassStrings);

    procedure ParseText(Text_: TPascalString);

    function ToJSONString(Formated_: Boolean): TPascalString; overload;
    function ToJSONString: TPascalString; overload;
    property ToJson: TPascalString read ToJSONString;
    class procedure Test;
  end;

implementation

{$IFDEF DELPHI}
{$INCLUDE ZJson_delphi.inc}
{$ELSE DELPHI}
{$INCLUDE ZJson_fpc.inc}
{$ENDIF DELPHI}


constructor TZ_JsonBase.Create(Parent_: TZ_JsonBase);
begin
  inherited Create;
  FParent := Parent_;
  if FParent <> nil then
      FParent.FList.Add(self);

  FList := TCoreClassObjectList.Create;
  FList.AutoFreeObj := True;
end;

destructor TZ_JsonBase.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

constructor TZ_JsonArray.Create(Parent_: TZ_JsonBase);
begin
  inherited Create(Parent_);
end;

destructor TZ_JsonArray.Destroy;
begin
  inherited Destroy;
end;

constructor TZ_JsonObject.Create(Parent_: TZ_JsonBase);
begin
  inherited Create(Parent_);
  if Parent = nil then
      FInstance := TZ_Instance_JsonObject.Create;
end;

constructor TZ_JsonObject.Create;
begin
  Create(nil);
end;

destructor TZ_JsonObject.Destroy;
begin
  if Parent = nil then
      FInstance.Free;
  inherited Destroy;
end;

procedure TZ_JsonObject.SaveToStream(stream: TCoreClassStream);
begin
  SaveToStream(stream, True);
end;

procedure TZ_JsonObject.SaveToLines(L_: TCoreClassStrings);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  SaveToStream(m64);
  m64.Position := 0;
  L_.LoadFromStream(m64);
  m64.Free;
end;

procedure TZ_JsonObject.LoadFromLines(L_: TCoreClassStrings);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
{$IFDEF FPC}
  L_.SaveToStream(m64);
{$ELSE}
  L_.SaveToStream(m64, TEncoding.UTF8);
{$ENDIF}
  m64.Position := 0;
  LoadFromStream(m64);
  m64.Free;
end;

procedure TZ_JsonObject.ParseText(Text_: TPascalString);
var
  buff: TBytes;
  m64: TMS64;
begin
  buff := Text_.Bytes;
  m64 := TMS64.Create;
  if length(buff) > 0 then
      m64.SetPointerWithProtectedMode(@buff[0], length(buff));
  LoadFromStream(m64);
  m64.Free;
  SetLength(buff, 0);
end;

function TZ_JsonObject.ToJSONString(Formated_: Boolean): TPascalString;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  SaveToStream(m64, Formated_);
  Result.Bytes := m64.ToBytes;
  m64.Free;
end;

function TZ_JsonObject.ToJSONString: TPascalString;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  SaveToStream(m64, True);
  Result.Bytes := m64.ToBytes;
  m64.Free;
end;

class procedure TZ_JsonObject.Test;
var
  js: TZ_JsonObject;
  ii: Integer;
  m64: TMS64;
begin
  js := TZ_JsonObject.Create();
  js.S['abc'] := '123';
  DoStatus(js.S['abc']);

  for ii := 1 to 3 do
      js.A['arry'].Add(ii);

  for ii := 0 to js.A['arry'].Count - 1 do
    begin
      DoStatus(js.A['arry'].I[ii]);
    end;

  js.A['arry'].AddObject.S['tt'] := 'inobj';

  js.O['obj'].S['fff'] := '999';

  DoStatus(js.ToJSONString(True));

  m64 := TMS64.Create;
  js.SaveToStream(m64);
  m64.Position := 0;
  js.LoadFromStream(m64);
  DoStatus(js.ToJSONString(True));
  js.Free;
end;

initialization

end.
