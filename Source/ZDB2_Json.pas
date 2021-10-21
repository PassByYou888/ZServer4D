{ ****************************************************************************** }
{ * ZDB 2.0 automated fragment for Json support, create by.qq600585            * }
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
unit ZDB2_Json;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  PascalStrings, UnicodeMixedLib, DoStatusIO, MemoryStream64,
  DataFrameEngine, ZDB2_Core, CoreCipher, ZJson;

type
  TZDB2_List_Json = class;

  TZDB2_Json = class
  private
    FTimeOut: TTimeTick;
    FAlive: TTimeTick;
    FID: Integer;
    FCoreSpace: TZDB2_Core_Space;
    FData: TZJ;
    FData_MD5: TMD5;
  public
    constructor Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure Load;
    procedure Save;
    procedure RecycleMemory;
    procedure Remove;
    function GetData: TZJ;
    property Data: TZJ read GetData;
    property ID: Integer read FID;
    property Data_MD5: TMD5 read FData_MD5;
  end;

  TZDB2_Json_Class = class of TZDB2_Json;

  TZDB2_List_Json_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TZDB2_Json>;

  TOnCreate_ZDB2_Json = procedure(Sender: TZDB2_List_Json; Obj: TZDB2_Json) of object;

  TZDB2_List_Json = class(TZDB2_List_Json_Decl)
  private type
    THead_ = packed record
      Identifier: Word;
      ID: Integer;
    end;

    PHead_ = ^THead_;
  private
    procedure DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
    function GetAutoFreeStream: Boolean;
    procedure SetAutoFreeStream(const Value: Boolean);
  public
    Json_Class: TZDB2_Json_Class;
    TimeOut: TTimeTick;
    DeltaSpace: Int64;
    BlockSize: Word;
    IOHnd: TIOHnd;
    CoreSpace: TZDB2_Core_Space;
    OnCreateClass: TOnCreate_ZDB2_Json;
    constructor Create(Json_Class_: TZDB2_Json_Class; OnCreateClass_: TOnCreate_ZDB2_Json; TimeOut_: TTimeTick;
      Stream_: TCoreClassStream; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;
    property AutoFreeStream: Boolean read GetAutoFreeStream write SetAutoFreeStream;
    procedure Remove(Obj: TZDB2_Json; RemoveData_: Boolean);
    procedure Delete(Index: Integer; RemoveData_: Boolean);
    procedure Clear(RemoveData_: Boolean);
    function NewDataFrom(ID_: Integer): TZDB2_Json; overload;
    function NewData: TZDB2_Json; overload;
    procedure Flush;
    procedure ExtractTo(Stream_: TCoreClassStream);
    procedure Progress;

    class procedure Test;
  end;

implementation

constructor TZDB2_Json.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
begin
  inherited Create;
  FTimeOut := 5 * 1000;
  FAlive := GetTimeTick;
  FID := ID_;
  FCoreSpace := CoreSpace_;
  FData := nil;
  FData_MD5 := NullMD5;
end;

destructor TZDB2_Json.Destroy;
begin
  Save;
  inherited Destroy;
end;

procedure TZDB2_Json.Progress;
begin
  if GetTimeTick - FAlive > FTimeOut then
      Save;
end;

procedure TZDB2_Json.Load;
var
  m64: TZDB2_Mem;
begin
  FData_MD5 := NullMD5;
  if FID < 0 then
      exit;
  m64 := TZDB2_Mem.Create;

  if FCoreSpace.ReadData(m64, FID) then
    begin
      FData_MD5 := umlMD5(m64.Memory, m64.Size);
      try
          FData.LoadFromStream(m64.Stream64);
      except
      end;
    end
  else
      FData.Clear;

  DisposeObject(m64);
end;

procedure TZDB2_Json.Save;
var
  m64: TMS64;
  tmp_md5: TMD5;
  old_ID: Integer;
begin
  if FData = nil then
      exit;
  m64 := TMS64.Create;
  try
    FData.SaveToStream(m64, False);
    tmp_md5 := umlMD5(m64.Memory, m64.Size);
    if umlMD5Compare(FData_MD5, NullMD5) or (not umlMD5Compare(tmp_md5, FData_MD5)) or (FID < 0) then
      begin
        old_ID := FID;
        FCoreSpace.WriteData(m64.Mem64, FID, False);
        FData_MD5 := tmp_md5;
        if old_ID >= 0 then
            FCoreSpace.RemoveData(old_ID, False);
      end;
  except
  end;
  DisposeObject(m64);
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_Json.RecycleMemory;
begin
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_Json.Remove;
begin
  if FID >= 0 then
      FCoreSpace.RemoveData(FID, False);
  DisposeObjectAndNil(FData);
  FID := -1;
  FData_MD5 := NullMD5;
end;

function TZDB2_Json.GetData: TZJ;
begin
  if FData = nil then
    begin
      FData := TZJ.Create;
      Load;
    end;
  Result := FData;
  FAlive := GetTimeTick;
end;

procedure TZDB2_List_Json.DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
begin
  retry := Trigger.AppendSpace(DeltaSpace, BlockSize);
end;

function TZDB2_List_Json.GetAutoFreeStream: Boolean;
begin
  Result := IOHnd.AutoFree;
end;

procedure TZDB2_List_Json.SetAutoFreeStream(const Value: Boolean);
begin
  IOHnd.AutoFree := Value;
end;

constructor TZDB2_List_Json.Create(Json_Class_: TZDB2_Json_Class; OnCreateClass_: TOnCreate_ZDB2_Json; TimeOut_: TTimeTick;
  Stream_: TCoreClassStream; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
var
  buff: TZDB2_BlockHandle;
  ID_: Integer;
  m64: TMem64;
begin
  inherited Create;
  Json_Class := Json_Class_;
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
  if CoreSpace.BlockCount = 0 then
      exit;

  if (PHead_(@CoreSpace.UserCustomHeader^[0])^.Identifier = $FFFF) and
    CoreSpace.Check(PHead_(@CoreSpace.UserCustomHeader^[0])^.ID) then
    begin
      m64 := TMem64.Create;
      CoreSpace.ReadData(m64, PHead_(@CoreSpace.UserCustomHeader^[0])^.ID);
      SetLength(buff, m64.Size shr 2);
      if length(buff) > 0 then
          CopyPtr(m64.Memory, @buff[0], length(buff) shl 2);
      DisposeObject(m64);
      CoreSpace.RemoveData(PHead_(@CoreSpace.UserCustomHeader^[0])^.ID, False);
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(THead_), 0);
    end
  else
      buff := CoreSpace.BuildTableID;

  for ID_ in buff do
      NewDataFrom(ID_);
  SetLength(buff, 0);
end;

destructor TZDB2_List_Json.Destroy;
begin
  Flush;
  Clear(False);
  DisposeObjectAndNil(CoreSpace);
  inherited Destroy;
end;

procedure TZDB2_List_Json.Remove(Obj: TZDB2_Json; RemoveData_: Boolean);
begin
  if RemoveData_ then
      Obj.Remove;
  DisposeObject(Obj);
  inherited Remove(Obj);
end;

procedure TZDB2_List_Json.Delete(Index: Integer; RemoveData_: Boolean);
begin
  if (index >= 0) and (index < Count) then
    begin
      if RemoveData_ then
          Items[index].Remove;
      DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TZDB2_List_Json.Clear(RemoveData_: Boolean);
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

function TZDB2_List_Json.NewDataFrom(ID_: Integer): TZDB2_Json;
begin
  Result := Json_Class.Create(CoreSpace, ID_);
  Result.FTimeOut := TimeOut;
  Add(Result);
  if Assigned(OnCreateClass) then
      OnCreateClass(self, Result);
end;

function TZDB2_List_Json.NewData: TZDB2_Json;
begin
  Result := NewDataFrom(-1);
end;

procedure TZDB2_List_Json.Flush;
var
  buff: TZDB2_BlockHandle;
  m64: TMem64;
  i: Integer;
begin
  if Count > 0 then
    begin
      SetLength(buff, Count);
      for i := 0 to Count - 1 do
        begin
          Items[i].Save;
          buff[i] := Items[i].FID;
        end;
      m64 := TMem64.Create;
      m64.Mapping(@buff[0], length(buff) shl 2);
      PHead_(@CoreSpace.UserCustomHeader^[0])^.Identifier := $FFFF;
      CoreSpace.WriteData(m64, PHead_(@CoreSpace.UserCustomHeader^[0])^.ID, False);
      DisposeObject(m64);
      SetLength(buff, 0);
    end
  else
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(THead_), 0);
  CoreSpace.Save;
end;

procedure TZDB2_List_Json.ExtractTo(Stream_: TCoreClassStream);
var
  TmpIOHnd: TIOHnd;
  TmpSpace: TZDB2_Core_Space;
  buff: TZDB2_BlockHandle;
  i: Integer;
  m64: TMem64;
begin
  Flush;
  InitIOHnd(TmpIOHnd);
  umlFileCreateAsStream(Stream_, TmpIOHnd);
  TmpSpace := TZDB2_Core_Space.Create(@TmpIOHnd);
  TmpSpace.Cipher := CoreSpace.Cipher;
  TmpSpace.Mode := smBigData;
  TmpSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoNoSpace;
  TmpSpace.BuildSpace(CoreSpace.State^.Physics, BlockSize);

  if Count > 0 then
    begin
      SetLength(buff, Count);
      for i := 0 to Count - 1 do
        begin
          m64 := TMem64.Create;
          if CoreSpace.ReadData(m64, Items[i].FID) then
            if not TmpSpace.WriteData(m64, buff[i], False) then
                RaiseInfo('error');
          DisposeObject(m64);
        end;
      m64 := TMem64.Create;
      m64.Mapping(@buff[0], length(buff) shl 2);
      PHead_(@TmpSpace.UserCustomHeader^[0])^.Identifier := $FFFF;
      CoreSpace.WriteData(m64, PHead_(@TmpSpace.UserCustomHeader^[0])^.ID, False);
      DisposeObject(m64);
      SetLength(buff, 0);
    end
  else
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(THead_), 0);
  TmpSpace.Save;
  DisposeObject(TmpSpace);
end;

procedure TZDB2_List_Json.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

class procedure TZDB2_List_Json.Test;
var
  Cipher_: TZDB2_Cipher;
  M64_1, M64_2: TMS64;
  i: Integer;
  tmp: TZDB2_Json;
  L: TZDB2_List_Json;
  tk: TTimeTick;
begin
  TCompute.Sleep(5000);
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world', 1, True, True);
  M64_1 := TMS64.CustomCreate(16 * 1024 * 1024);
  M64_2 := TMS64.CustomCreate(16 * 1024 * 1024);

  tk := GetTimeTick;
  with TZDB2_List_Json.Create(TZDB2_Json, nil, 5000, M64_1, 64 * 1048576, 200, Cipher_) do
    begin
      AutoFreeStream := False;
      for i := 0 to 20000 do
        begin
          tmp := NewData();
          tmp.Data.S['a'] := 'abcdefg';
          tmp.Save;
        end;
      DoStatus('build %d of json,time:%dms', [Count, GetTimeTick - tk]);
      Free;
    end;

  tk := GetTimeTick;
  L := TZDB2_List_Json.Create(TZDB2_Json, nil, 5000, M64_1, 64 * 1048576, 200, Cipher_);
  for i := 0 to L.Count - 1 do
    begin
      if L[i].Data.S['a'] <> 'abcdefg' then
          DoStatus('%s - test error.', [L.ClassName]);
    end;
  DoStatus('load %d of json,time:%dms', [L.Count, GetTimeTick - tk]);
  L.ExtractTo(M64_2);
  L.Free;

  tk := GetTimeTick;
  L := TZDB2_List_Json.Create(TZDB2_Json, nil, 5000, M64_2, 64 * 1048576, 200, Cipher_);
  for i := 0 to L.Count - 1 do
    begin
      if L[i].Data.S['a'] <> 'abcdefg' then
          DoStatus('%s - test error.', [L.ClassName]);
    end;
  DoStatus('load %d extract stream of json,time:%dms', [L.Count, GetTimeTick - tk]);
  L.Free;

  DisposeObject(M64_1);
  DisposeObject(M64_2);
  DisposeObject(Cipher_);
end;

end.
