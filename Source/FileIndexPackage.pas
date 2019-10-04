{ ****************************************************************************** }
{ * file index Package                                                         * }
{ * written by QQ 600585@qq.com                                                * }
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
(*
  update history
*)
unit FileIndexPackage;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib,
  ObjectData, ObjectDataManager, ItemStream, MemoryStream64, DoStatusIO, CoreCipher;

type
  TFileIndexInfo = record
    Name, Description: U_String;
    Size: Int64;
    CreateTime, ModificationTime: Double;
    UserProperty: Cardinal;
    Decrypt_MD5, Encrypt_MD5: TMD5;
    procedure Init();
    function Read(eng: TObjectDataManager; fPos: Int64): Boolean; overload;
    function Read(eng: TObjectDataManager; DBPath, DBItem: U_String): Boolean; overload;
    function Read(eng: TObjectDataManager; var hnd: TItemHandle): Boolean; overload;
    procedure Write(eng: TObjectDataManager; var hnd: TItemHandle);
  end;

  TBuildStreamCall = procedure(sourstream, deststream: TMemoryStream64);

function CheckIndexPackage(sour: TObjectDataManager; dataOutputPath: U_String): Boolean; overload;
function CheckIndexPackage(sourDB, dataOutputPath: U_String): Boolean; overload;

procedure BuildIndexPackage(sour, dest: TObjectDataManager; OnBuildStream: TBuildStreamCall; dataOutputPath: U_String); overload;
procedure BuildIndexPackage(sour, dest: TObjectDataManager; dataOutputPath: U_String); overload;
procedure BuildIndexPackage(sourDB, destDB, dataOutputPath: U_String); overload;

procedure ParallelCompressStream_Call(sourstream, deststream: TMemoryStream64);
procedure ParallelDecompressStream_Call(sourstream, deststream: TMemoryStream64);

implementation

procedure TFileIndexInfo.Init;
begin
  Name := '';
  Description := '';
  Size := 0;
  CreateTime := 0;
  ModificationTime := 0;
  UserProperty := 0;
  Decrypt_MD5 := NullMD5;
  Encrypt_MD5 := NullMD5;
end;

function TFileIndexInfo.Read(eng: TObjectDataManager; fPos: Int64): Boolean;
var
  itmHnd: TItemHandle;
begin
  if eng.ItemFastOpen(fPos, itmHnd) then
    begin
      Result := read(eng, itmHnd);
      eng.ItemClose(itmHnd);
    end
  else
      Result := False;
end;

function TFileIndexInfo.Read(eng: TObjectDataManager; DBPath, DBItem: U_String): Boolean;
var
  itmHnd: TItemHandle;
begin
  if eng.ItemOpen(DBPath, DBItem, itmHnd) then
    begin
      Result := read(eng, itmHnd);
      eng.ItemClose(itmHnd);
    end
  else
      Result := False;
end;

function TFileIndexInfo.Read(eng: TObjectDataManager; var hnd: TItemHandle): Boolean;
var
  m64: TMemoryStream64;
begin
  Result := False;
  m64 := TMemoryStream64.Create;
  eng.ItemReadToStream(hnd, m64);
  m64.Position := 0;
  if m64.ReadUInt16() = $FFFF then
    begin
      Name := m64.ReadString();
      Description := m64.ReadString();
      Size := m64.ReadInt64();
      CreateTime := m64.ReadDouble();
      ModificationTime := m64.ReadDouble();
      UserProperty := m64.ReadUInt32();
      Decrypt_MD5 := m64.ReadMD5();
      Encrypt_MD5 := m64.ReadMD5();
      Result := True;
    end;
  disposeObject(m64);
end;

procedure TFileIndexInfo.Write(eng: TObjectDataManager; var hnd: TItemHandle);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.WriteUInt16($FFFF);
  m64.WriteString(Name);
  m64.WriteString(Description);
  m64.WriteInt64(Size);
  m64.WriteDouble(CreateTime);
  m64.WriteDouble(ModificationTime);
  m64.WriteUInt32(UserProperty);
  m64.WriteMD5(Decrypt_MD5);
  m64.WriteMD5(Encrypt_MD5);
  eng.ItemWriteFromStream(hnd, m64);
  disposeObject(m64);
end;

function CheckIndexPackage(sour: TObjectDataManager; dataOutputPath: U_String): Boolean;
var
  srHnd: TItemRecursionSearch;
  itmHnd: TItemHandle;
  info: TFileIndexInfo;
  fn: U_String;
  error_: Integer;
  m64: TMemoryStream64;
begin
  error_ := 0;
  if sour.RecursionSearchFirst('/', '*', srHnd) then
    begin
      repeat
        if srHnd.ReturnHeader.ID = DB_Header_Item_ID then
          begin
            sour.ItemFastOpen(srHnd.ReturnHeader.CurrentHeader, itmHnd);
            info.Read(sour, itmHnd);
            sour.ItemClose(itmHnd);

            fn := umlCombineFileName(dataOutputPath, umlMD52String(info.Decrypt_MD5));
            if not umlFileExists(fn) then
              begin
                DoStatus('no file %s', [fn.Text]);
                Inc(error_);
              end
            else
              begin
                m64 := TMemoryStream64.Create;
                m64.LoadFromFile(fn);
                if not umlMD5Compare(umlStreamMD5(m64), info.Encrypt_MD5) then
                  begin
                    DoStatus('md5 verify error: %s', [fn.Text]);
                    Inc(error_);
                  end;
                disposeObject(m64);
              end;
          end;
      until not sour.RecursionSearchNext(srHnd);
    end;
  Result := error_ = 0;
end;

function CheckIndexPackage(sourDB, dataOutputPath: U_String): Boolean;
var
  sour: TObjectDataManagerOfCache;
begin
  sour := TObjectDataManagerOfCache.Open(sourDB, DBMarshal.ID, True);
  Result := CheckIndexPackage(sour, dataOutputPath);
  disposeObject(sour);
end;

procedure BuildIndexPackage(sour, dest: TObjectDataManager; OnBuildStream: TBuildStreamCall; dataOutputPath: U_String); overload;
var
  srHnd: TItemRecursionSearch;
  ph: U_String;
  itmHnd: TItemHandle;
  sourstream: TMemoryStream64;
  deststream: TMemoryStream64;
  info: TFileIndexInfo;
  fn: U_String;
begin
  umlCreateDirectory(dataOutputPath);

  if sour.RecursionSearchFirst('/', '*', srHnd) then
    begin
      ph := '/';
      repeat
        if srHnd.ReturnHeader.ID = DB_Header_Field_ID then
          begin
            ph := sour.GetFieldPath(srHnd.CurrentField.RHeader.CurrentHeader);
            dest.CreateField(ph, '');
          end
        else if srHnd.ReturnHeader.ID = DB_Header_Item_ID then
          begin
            sourstream := TMemoryStream64.Create;

            sour.ItemFastOpen(srHnd.ReturnHeader.CurrentHeader, itmHnd);
            sour.ItemReadToStream(itmHnd, sourstream);
            info.Name := itmHnd.Name;
            info.Description := itmHnd.Description;
            info.Size := itmHnd.Item.Size;
            info.CreateTime := itmHnd.CreateTime;
            info.ModificationTime := itmHnd.ModificationTime;
            info.UserProperty := itmHnd.Item.RHeader.UserProperty;
            info.Decrypt_MD5 := umlStreamMD5(sourstream);
            sour.ItemClose(itmHnd);

            fn := umlCombineFileName(dataOutputPath, umlMD52String(info.Decrypt_MD5));
            deststream := TMemoryStream64.Create;
            if not umlFileExists(fn) then
              begin
                OnBuildStream(sourstream, deststream);
                deststream.SaveToFile(fn);
                DoStatus('compression %s %s -> %s', [info.Name.Text, umlSizeToStr(info.Size).Text, umlSizeToStr(deststream.Size).Text]);
              end
            else
              begin
                deststream.LoadFromFile(fn);
              end;
            info.Encrypt_MD5 := umlStreamMD5(deststream);
            disposeObject(deststream);
            disposeObject(sourstream);

            dest.ItemCreate(ph, info.Name, info.Description, itmHnd);
            info.Write(dest, itmHnd);
            dest.ItemClose(itmHnd);
          end;

      until not sour.RecursionSearchNext(srHnd);
    end;
  dest.UpdateIO;
end;

procedure BuildIndexPackage(sour, dest: TObjectDataManager; dataOutputPath: U_String);
begin
  BuildIndexPackage(sour, dest, {$IFDEF FPC}@{$ENDIF FPC}ParallelCompressStream_Call, dataOutputPath);
end;

procedure BuildIndexPackage(sourDB, destDB, dataOutputPath: U_String);
var
  sour, dest: TObjectDataManagerOfCache;
begin
  sour := TObjectDataManagerOfCache.Open(sourDB, DBMarshal.ID, True);
  dest := TObjectDataManagerOfCache.CreateNew(sour.Handle^.FixedStringL, destDB, DBMarshal.ID);
  dest.OverWriteItem := False;
  BuildIndexPackage(sour, dest, dataOutputPath);
  disposeObject(sour);
  disposeObject(dest);
end;

procedure ParallelCompressStream_Call(sourstream, deststream: TMemoryStream64);
begin
  ParallelCompressStream(sourstream, deststream);
end;

procedure ParallelDecompressStream_Call(sourstream, deststream: TMemoryStream64);
begin
  ParallelDecompressStream(sourstream, deststream);
end;

initialization

end.
