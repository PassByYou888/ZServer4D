{ ****************************************************************************** }
{ * ZDB 2.0 automated fragment for Hash Variant support, create by.qq600585    * }
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
unit ZDB2_HV;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  PascalStrings, UnicodeMixedLib, DoStatusIO, MemoryStream64,
  DataFrameEngine, ZDB2_Core, CoreCipher, ListEngine;

type
  TZDB2_List_HashVariant = class;

  TZDB2_HashVariant = class
  private
    FTimeOut: TTimeTick;
    FAlive: TTimeTick;
    FID: Integer;
    FCoreSpace: TZDB2_Core_Space;
    FData: THashVariantList;
    FIsChanged: Boolean;
    procedure DoHashVariantChange(Sender: THashVariantList; Name_: SystemString; OLD_, New_: Variant);
  public
    constructor Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure Load;
    procedure Save;
    procedure RecycleMemory;
    procedure Remove;
    function GetData: THashVariantList;
    property Data: THashVariantList read GetData;
    property ID: Integer read FID;
  end;

  TZDB2_HashVariant_Class = class of TZDB2_HashVariant;

  TZDB2_List_HashVariant_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TZDB2_HashVariant>;

  TOnCreate_ZDB2_HashVariant = procedure(Sender: TZDB2_List_HashVariant; Obj: TZDB2_HashVariant) of object;

  TZDB2_List_HashVariant = class(TZDB2_List_HashVariant_Decl)
  private
    procedure DoNoSpace(Siz_: Int64; var retry: Boolean);
    function GetAutoFreeStream: Boolean;
    procedure SetAutoFreeStream(const Value: Boolean);
  public
    HashVariant_Class: TZDB2_HashVariant_Class;
    TimeOut: TTimeTick;
    DeltaSpace: Int64;
    BlockSize: Word;
    IOHnd: TIOHnd;
    CoreSpace: TZDB2_Core_Space;
    OnCreateClass: TOnCreate_ZDB2_HashVariant;
    constructor Create(HashVariant_Class_: TZDB2_HashVariant_Class; OnCreateClass_: TOnCreate_ZDB2_HashVariant; TimeOut_: TTimeTick;
      Stream_: TCoreClassStream; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;
    property AutoFreeStream: Boolean read GetAutoFreeStream write SetAutoFreeStream;
    procedure Remove(Obj: TZDB2_HashVariant; RemoveData_: Boolean);
    procedure Delete(Index: Integer; RemoveData_: Boolean);
    procedure Clear(RemoveData_: Boolean);
    function NewDataFrom(ID_: Integer): TZDB2_HashVariant; overload;
    function NewData: TZDB2_HashVariant; overload;
    procedure Flush;
    procedure Progress;

    class procedure Test;
  end;

implementation

procedure TZDB2_HashVariant.DoHashVariantChange(Sender: THashVariantList; Name_: SystemString; OLD_, New_: Variant);
begin
  FIsChanged := True;
end;

constructor TZDB2_HashVariant.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
begin
  inherited Create;
  FTimeOut := 5 * 1000;
  FAlive := GetTimeTick;
  FID := ID_;
  FCoreSpace := CoreSpace_;
  FData := nil;
  FIsChanged := False;
end;

destructor TZDB2_HashVariant.Destroy;
begin
  Save;
  inherited Destroy;
end;

procedure TZDB2_HashVariant.Progress;
begin
  if GetTimeTick - FAlive > FTimeOut then
      Save;
end;

procedure TZDB2_HashVariant.Load;
var
  m64: TZDB2_Mem;
begin
  if FID < 0 then
      exit;
  m64 := TZDB2_Mem.Create;

  if FCoreSpace.ReadData(m64, FID) then
    begin
      try
        FData.LoadFromStream(m64.Stream64);
        FIsChanged := False;
      except
      end;
    end
  else
      FData.Clear;

  DisposeObject(m64);
end;

procedure TZDB2_HashVariant.Save;
var
  m64: TMS64;
  old_ID: Integer;
begin
  if FData = nil then
      exit;
  m64 := TMS64.Create;
  try
    if FIsChanged or (FID < 0) then
      begin
        FData.SaveToStream(m64);
        old_ID := FID;
        FCoreSpace.WriteData(m64.Mem64, FID, False);
        if old_ID >= 0 then
            FCoreSpace.RemoveData(old_ID, False);
      end;
  except
  end;
  DisposeObject(m64);
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_HashVariant.RecycleMemory;
begin
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_HashVariant.Remove;
begin
  if FID >= 0 then
      FCoreSpace.RemoveData(FID, False);
  DisposeObjectAndNil(FData);
  FID := -1;
end;

function TZDB2_HashVariant.GetData: THashVariantList;
begin
  if FData = nil then
    begin
      FData := THashVariantList.CustomCreate(64);
      FData.OnValueChangeNotify := {$IFDEF FPC}@{$ENDIF FPC}DoHashVariantChange;
      Load;
      FIsChanged := False;
    end;
  Result := FData;
  FAlive := GetTimeTick;
end;

procedure TZDB2_List_HashVariant.DoNoSpace(Siz_: Int64; var retry: Boolean);
begin
  retry := CoreSpace.AppendSpace(DeltaSpace, BlockSize);
end;

function TZDB2_List_HashVariant.GetAutoFreeStream: Boolean;
begin
  Result := IOHnd.AutoFree;
end;

procedure TZDB2_List_HashVariant.SetAutoFreeStream(const Value: Boolean);
begin
  IOHnd.AutoFree := Value;
end;

constructor TZDB2_List_HashVariant.Create(HashVariant_Class_: TZDB2_HashVariant_Class; OnCreateClass_: TOnCreate_ZDB2_HashVariant; TimeOut_: TTimeTick;
  Stream_: TCoreClassStream; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
var
  buff: TZDB2_BlockHandle;
  ID_: Integer;
  tmp: TZDB2_HashVariant;
begin
  inherited Create;
  HashVariant_Class := HashVariant_Class_;
  TimeOut := TimeOut_;
  DeltaSpace := DeltaSpace_;
  BlockSize := BlockSize_;
  InitIOHnd(IOHnd);
  umlFileCreateAsStream(Stream_, IOHnd);
  CoreSpace := TZDB2_Core_Space.Create(@IOHnd);
  CoreSpace.Cipher := Cipher_;
  CoreSpace.Mode := smBigData;
  CoreSpace.AutoCloseIOHnd := True;
  CoreSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoNoSpace;
  if umlFileSize(IOHnd) > 0 then
    begin
      if not CoreSpace.Open then
          RaiseInfo('error.');
    end;
  OnCreateClass := OnCreateClass_;
  buff := CoreSpace.BuildTableID;
  for ID_ in buff do
      NewDataFrom(ID_);
  SetLength(buff, 0);
end;

destructor TZDB2_List_HashVariant.Destroy;
begin
  Flush;
  Clear(False);
  DisposeObjectAndNil(CoreSpace);
  inherited Destroy;
end;

procedure TZDB2_List_HashVariant.Remove(Obj: TZDB2_HashVariant; RemoveData_: Boolean);
begin
  if RemoveData_ then
      Obj.Remove;
  DisposeObject(Obj);
  inherited Remove(Obj);
end;

procedure TZDB2_List_HashVariant.Delete(Index: Integer; RemoveData_: Boolean);
begin
  if (index >= 0) and (index < Count) then
    begin
      if RemoveData_ then
          Items[index].Remove;
      DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TZDB2_List_HashVariant.Clear(RemoveData_: Boolean);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      if RemoveData_ then
          Items[i].Remove;
      DisposeObject(Items[i]);
    end;
  inherited Clear;
end;

function TZDB2_List_HashVariant.NewDataFrom(ID_: Integer): TZDB2_HashVariant;
begin
  Result := HashVariant_Class.Create(CoreSpace, ID_);
  Result.FTimeOut := TimeOut;
  Add(Result);
end;

function TZDB2_List_HashVariant.NewData: TZDB2_HashVariant;
begin
  Result := NewDataFrom(-1);
end;

procedure TZDB2_List_HashVariant.Flush;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Save;
  CoreSpace.Save;
end;

procedure TZDB2_List_HashVariant.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

class procedure TZDB2_List_HashVariant.Test;
var
  Cipher_: TZDB2_Cipher;
  m64: TMS64;
  i: Integer;
  tmp: TZDB2_HashVariant;
  L: TZDB2_List_HashVariant;
  tk: TTimeTick;
begin
  TCompute.Sleep(5000);
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world', 1, True, True);
  m64 := TMS64.CustomCreate(16 * 1024 * 1024);

  tk := GetTimeTick;
  with TZDB2_List_HashVariant.Create(TZDB2_HashVariant, nil, 5000, m64, 64 * 1048576, 200, Cipher_) do
    begin
      AutoFreeStream := False;
      for i := 1 to 20000 do
        begin
          tmp := NewData();
          tmp.Data['a'] := 'abcdefg';
          tmp.Save;
        end;
      DoStatus('build %d of HashVariant,time:%dms', [Count, GetTimeTick - tk]);
      Free;
    end;

  tk := GetTimeTick;
  L := TZDB2_List_HashVariant.Create(TZDB2_HashVariant, nil, 5000, m64, 64 * 1048576, 200, Cipher_);
  for i := 0 to L.Count - 1 do
    begin
      if L[i].Data['a'] <> 'abcdefg' then
          DoStatus('%s - test error.', [L.ClassName]);
    end;
  DoStatus('load %d of HashVariant,time:%dms', [L.Count, GetTimeTick - tk]);
  L.Free;

  DisposeObject(m64);
  DisposeObject(Cipher_);
end;

end.
