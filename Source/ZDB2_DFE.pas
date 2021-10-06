{ ****************************************************************************** }
{ * ZDB 2.0 automated fragment for DataFrameEngine support, create by.qq600585 * }
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
unit ZDB2_DFE;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  PascalStrings, UnicodeMixedLib, DoStatusIO, MemoryStream64,
  DataFrameEngine, ZDB2_Core, CoreCipher, ListEngine;

type
  TZDB2_List_DFE = class;

  TZDB2_DFE = class
  private
    FTimeOut: TTimeTick;
    FAlive: TTimeTick;
    FID: Integer;
    FCoreSpace: TZDB2_Core_Space;
    FData: TDFE;
  public
    constructor Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure Load;
    procedure Save;
    procedure RecycleMemory;
    procedure Remove;
    function GetData: TDFE;
    property Data: TDFE read GetData;
    property ID: Integer read FID;
  end;

  TZDB2_DFE_Class = class of TZDB2_DFE;

  TZDB2_List_DFE_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TZDB2_DFE>;

  TOnCreate_ZDB2_DFE = procedure(Sender: TZDB2_List_DFE; Obj: TZDB2_DFE) of object;

  TZDB2_List_DFE = class(TZDB2_List_DFE_Decl)
  private
    procedure DoNoSpace(Siz_: Int64; var retry: Boolean);
    function GetAutoFreeStream: Boolean;
    procedure SetAutoFreeStream(const Value: Boolean);
  public
    DFE_Class: TZDB2_DFE_Class;
    TimeOut: TTimeTick;
    DeltaSpace: Int64;
    BlockSize: Word;
    IOHnd: TIOHnd;
    CoreSpace: TZDB2_Core_Space;
    OnCreateClass: TOnCreate_ZDB2_DFE;
    constructor Create(DFE_Class_: TZDB2_DFE_Class; OnCreateClass_: TOnCreate_ZDB2_DFE; TimeOut_: TTimeTick;
      Stream_: TCoreClassStream; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;
    property AutoFreeStream: Boolean read GetAutoFreeStream write SetAutoFreeStream;
    procedure Remove(Obj: TZDB2_DFE; RemoveData_: Boolean);
    procedure Delete(Index: Integer; RemoveData_: Boolean);
    procedure Clear(RemoveData_: Boolean);
    function NewDataFrom(ID_: Integer): TZDB2_DFE; overload;
    function NewData: TZDB2_DFE; overload;
    procedure Flush;
    procedure Progress;

    class procedure Test;
  end;

implementation

constructor TZDB2_DFE.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
begin
  inherited Create;
  FTimeOut := 5 * 1000;
  FAlive := GetTimeTick;
  FID := ID_;
  FCoreSpace := CoreSpace_;
  FData := nil;
end;

destructor TZDB2_DFE.Destroy;
begin
  Save;
  inherited Destroy;
end;

procedure TZDB2_DFE.Progress;
begin
  if GetTimeTick - FAlive > FTimeOut then
      Save;
end;

procedure TZDB2_DFE.Load;
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
        FData.IsChanged := False;
      except
      end;
    end
  else
      FData.Clear;

  DisposeObject(m64);
end;

procedure TZDB2_DFE.Save;
var
  m64: TMS64;
  old_ID: Integer;
begin
  if FData = nil then
      exit;
  m64 := TMS64.Create;
  try
    if FData.IsChanged or (FID < 0) then
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

procedure TZDB2_DFE.RecycleMemory;
begin
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_DFE.Remove;
begin
  if FID >= 0 then
      FCoreSpace.RemoveData(FID, False);
  DisposeObjectAndNil(FData);
  FID := -1;
end;

function TZDB2_DFE.GetData: TDFE;
begin
  if FData = nil then
    begin
      FData := TDFE.Create;
      Load;
      FData.IsChanged := False;
    end;
  Result := FData;
  FAlive := GetTimeTick;
end;

procedure TZDB2_List_DFE.DoNoSpace(Siz_: Int64; var retry: Boolean);
begin
  retry := CoreSpace.AppendSpace(DeltaSpace, BlockSize);
end;

function TZDB2_List_DFE.GetAutoFreeStream: Boolean;
begin
  Result := IOHnd.AutoFree;
end;

procedure TZDB2_List_DFE.SetAutoFreeStream(const Value: Boolean);
begin
  IOHnd.AutoFree := Value;
end;

constructor TZDB2_List_DFE.Create(DFE_Class_: TZDB2_DFE_Class; OnCreateClass_: TOnCreate_ZDB2_DFE; TimeOut_: TTimeTick;
  Stream_: TCoreClassStream; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
var
  buff: TZDB2_BlockHandle;
  ID_: Integer;
  tmp: TZDB2_DFE;
begin
  inherited Create;
  DFE_Class := DFE_Class_;
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

destructor TZDB2_List_DFE.Destroy;
begin
  Flush;
  Clear(False);
  DisposeObjectAndNil(CoreSpace);
  inherited Destroy;
end;

procedure TZDB2_List_DFE.Remove(Obj: TZDB2_DFE; RemoveData_: Boolean);
begin
  if RemoveData_ then
      Obj.Remove;
  DisposeObject(Obj);
  inherited Remove(Obj);
end;

procedure TZDB2_List_DFE.Delete(Index: Integer; RemoveData_: Boolean);
begin
  if (index >= 0) and (index < Count) then
    begin
      if RemoveData_ then
          Items[index].Remove;
      DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TZDB2_List_DFE.Clear(RemoveData_: Boolean);
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

function TZDB2_List_DFE.NewDataFrom(ID_: Integer): TZDB2_DFE;
begin
  Result := DFE_Class.Create(CoreSpace, ID_);
  Result.FTimeOut := TimeOut;
  Add(Result);
end;

function TZDB2_List_DFE.NewData: TZDB2_DFE;
begin
  Result := NewDataFrom(-1);
end;

procedure TZDB2_List_DFE.Flush;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Save;
  CoreSpace.Save;
end;

procedure TZDB2_List_DFE.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

class procedure TZDB2_List_DFE.Test;
var
  Cipher_: TZDB2_Cipher;
  m64: TMS64;
  i: Integer;
  tmp: TZDB2_DFE;
  L: TZDB2_List_DFE;
  tk: TTimeTick;
begin
  TCompute.Sleep(5000);
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world', 1, True, True);
  m64 := TMS64.CustomCreate(16 * 1024 * 1024);

  tk := GetTimeTick;
  with TZDB2_List_DFE.Create(TZDB2_DFE, nil, 5000, m64, 64 * 1048576, 200, Cipher_) do
    begin
      AutoFreeStream := False;
      for i := 1 to 20000 do
        begin
          tmp := NewData();
          tmp.Data.WriteString('abcdefg');
          tmp.Save;
        end;
      DoStatus('build %d of DFE,time:%dms', [Count, GetTimeTick - tk]);
      Free;
    end;

  tk := GetTimeTick;
  L := TZDB2_List_DFE.Create(TZDB2_DFE, nil, 5000, m64, 64 * 1048576, 200, Cipher_);
  for i := 0 to L.Count - 1 do
    begin
      if L[i].Data.ReadString(0) <> 'abcdefg' then
          DoStatus('%s - test error.', [L.ClassName]);
    end;
  DoStatus('load %d of DFE,time:%dms', [L.Count, GetTimeTick - tk]);
  L.Free;

  DisposeObject(m64);
  DisposeObject(Cipher_);
end;

end.
