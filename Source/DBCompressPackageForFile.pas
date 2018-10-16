{ ****************************************************************************** }
{ * DB Package Compress                                                        * }
{ * written by QQ 600585@qq.com                                                * }
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
*)

unit DBCompressPackageForFile;

{$INCLUDE zDefine.inc}

interface

uses ObjectData, ObjectDataManager, UnicodeMixedLib, CoreClasses, ItemStream,
  DoStatusIO, ListEngine, TextDataEngine, PascalStrings;

procedure BeginImportStreamToDB(dbEng: TObjectDataManager; md5List: THashStringList);
procedure ImportStreamToDB(md5List: THashStringList; stream: TCoreClassStream; FileName: SystemString; dbEng: TObjectDataManager);
procedure EndImportStreamToDB(dbEng: TObjectDataManager; md5List: THashStringList);

procedure BatchImportPathToDB(InitDir, Filter: SystemString; dbEng: TObjectDataManager);
procedure BatchImportPathToDBFile(InitDir, Filter, dbFile: SystemString);
procedure BatchImportPathToDBStream(InitDir, Filter: SystemString; DBStream: TCoreClassStream);

function ExtractFileInDB(dbEng: TObjectDataManager; FieldPos: Int64; FileName: SystemString; ExtractToStream: TCoreClassStream): Boolean; overload;
function ExtractFileInDB(dbEng: TObjectDataManager; DBPath, FileName: SystemString; ExtractToStream: TCoreClassStream): Boolean; overload;
function ExtractFileInDB(dbFileName, DBPath, FileName: SystemString; ExtractToStream: TCoreClassStream): Boolean; overload;
function ExtractFileInDB(DBStream: TCoreClassStream; DBPath, FileName: SystemString; ExtractToStream: TCoreClassStream): Boolean; overload;
function ExistsFileInDB(DBStream: TCoreClassStream; DBPath, FileName: SystemString): Boolean; overload;
function ExistsFileInDB(dbFileName, DBPath, FileName: SystemString): Boolean; overload;

procedure ExtractDBToPath(dbEng: TObjectDataManager; ExtractToDir: SystemString; OutputFileList: TCoreClassStrings); overload;
procedure ExtractDBToPath(dbEng: TObjectDataManager; ExtractToDir: SystemString); overload;
procedure ExtractDBFileToPath(dbFile, ExtractToDir: SystemString);

function VerifyFileInDB(dbEng: TObjectDataManager): Integer;
function VerifyFileInDBStream(DBStream: TCoreClassStream): Integer;
function VerifyFileInDBFile(dbFile: SystemString): Integer;

var
  DBPackageMD5VerifyFileName: SystemString = '______md5info.txt';
  DBPackageFileCompressed: Boolean = True;

implementation

uses MemoryStream64;

procedure EncryptStream(sour, GenPack: TCoreClassStream);
begin
  if DBPackageFileCompressed then
      MaxCompressStream(sour, GenPack)
  else
      GenPack.CopyFrom(sour, sour.Size);
end;

procedure DecryptStream(sour, UnPackTo: TCoreClassStream);
begin
  if DBPackageFileCompressed then
      DecompressStream(sour, UnPackTo)
  else
      UnPackTo.CopyFrom(sour, sour.Size);
end;

procedure BeginImportStreamToDB(dbEng: TObjectDataManager; md5List: THashStringList);
var
  hashTextStream: THashStringTextStream;
  srHnd: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  md5List.Clear;
  if dbEng.ItemFastFindLast(dbEng.RootField, DBPackageMD5VerifyFileName, srHnd) then
    begin
      hashTextStream := THashStringTextStream.Create(md5List);
      if dbEng.ItemFastOpen(srHnd.HeaderPOS, itmHnd) then
        begin
          itmStream := TItemStream.Create(dbEng, itmHnd);
          hashTextStream.LoadFromStream(itmStream);
          DisposeObject(itmStream);
        end;
      DisposeObject(hashTextStream);
      dbEng.FastDelete(dbEng.RootField, srHnd.HeaderPOS);
    end;
end;

procedure ImportStreamToDB(md5List: THashStringList; stream: TCoreClassStream; FileName: SystemString; dbEng: TObjectDataManager);
var
  FieldPos: Int64;
  srHnd: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
  md5: SystemString;
begin
  FieldPos := dbEng.RootField;

  if md5List.Exists(FileName) then
    if dbEng.ItemFastFindFirst(FieldPos, FileName, srHnd) then
        dbEng.FastDelete(FieldPos, srHnd.HeaderPOS);

  if dbEng.ItemFastCreate(FieldPos, FileName, '', itmHnd) then
    begin
      try
        itmStream := TItemStream.Create(dbEng, itmHnd);

        stream.Position := 0;
        md5 := umlStreamMD5Char(stream).Text;
        md5List.Add(itmHnd.Name, md5);

        stream.Position := 0;

        EncryptStream(stream, itmStream);

        DoStatus('%s %s ->  %s compressed:%d%%',
          [itmHnd.Name.Text,
          umlSizeToStr(stream.Size).Text,
          umlSizeToStr(itmStream.Size).Text,
          Round(itmStream.Size / stream.Size * 100)]
          );

        itmStream.UpdateHandle;
        DisposeObject(itmStream);
      except
      end;
    end;
end;

procedure EndImportStreamToDB(dbEng: TObjectDataManager; md5List: THashStringList);
var
  hashTextStream: THashStringTextStream;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  hashTextStream := THashStringTextStream.Create(md5List);
  if dbEng.ItemFastCreate(dbEng.RootField, DBPackageMD5VerifyFileName, '', itmHnd) then
    begin
      try
        itmStream := TItemStream.Create(dbEng, itmHnd);
        hashTextStream.SaveToStream(itmStream);
        itmStream.UpdateHandle;
        DisposeObject(itmStream);
      except
      end;
    end;
  DisposeObject(hashTextStream);

  dbEng.Update;
end;

procedure BatchImportPathToDB(InitDir, Filter: SystemString; dbEng: TObjectDataManager);

  procedure AddPath(APath: SystemString; aFieldPos: Int64);
  var
    fAry: U_StringArray;
    n, suffixn: SystemString;
    fs: TCoreClassFileStream;
    itmHnd: TItemHandle;
    itmStream: TItemStream;
    fPos: Int64;
    md5: SystemString;
    md5List: THashStringList;
    hashTextStream: THashStringTextStream;
    srHnd: TItemSearch;
  begin
    md5List := THashStringList.Create;

    if dbEng.ItemFastFindLast(aFieldPos, DBPackageMD5VerifyFileName, srHnd) then
      begin
        hashTextStream := THashStringTextStream.Create(md5List);
        if dbEng.ItemFastOpen(srHnd.HeaderPOS, itmHnd) then
          begin
            itmStream := TItemStream.Create(dbEng, itmHnd);
            hashTextStream.LoadFromStream(itmStream);
            DisposeObject(itmStream);
          end;
        DisposeObject(hashTextStream);
        dbEng.FastDelete(aFieldPos, srHnd.HeaderPOS);
      end;

    fAry := umlGetFileListWithFullPath(APath);
    for n in fAry do
      begin
        suffixn := umlGetFileName(n).Text;
        if umlMultipleMatch(Filter, suffixn) then
          begin
            if md5List.Exists(suffixn) then
              if dbEng.ItemFastFindFirst(aFieldPos, suffixn, srHnd) then
                  dbEng.FastDelete(aFieldPos, srHnd.HeaderPOS);

            if dbEng.ItemFastCreate(aFieldPos, suffixn, '', itmHnd) then
              begin
                try
                  fs := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
                  itmStream := TItemStream.Create(dbEng, itmHnd);

                  fs.Position := 0;
                  md5 := umlStreamMD5Char(fs).Text;
                  md5List.Add(itmHnd.Name, md5);

                  fs.Position := 0;
                  EncryptStream(fs, itmStream);

                  DoStatus('%s %s ->  %s compressed:%d%%',
                    [itmHnd.Name.Text,
                    umlSizeToStr(fs.Size).Text,
                    umlSizeToStr(itmStream.Size).Text,
                    Round(itmStream.Size / fs.Size * 100)]
                    );

                  itmStream.UpdateHandle;
                  DisposeObject(itmStream);
                  DisposeObject(fs);
                except
                end;
              end;
          end;
      end;

    hashTextStream := THashStringTextStream.Create(md5List);

    if dbEng.ItemFastCreate(aFieldPos, DBPackageMD5VerifyFileName, '', itmHnd) then
      begin
        try
          itmStream := TItemStream.Create(dbEng, itmHnd);
          hashTextStream.SaveToStream(itmStream);
          itmStream.UpdateHandle;
          DisposeObject(itmStream);
        except
        end;
      end;
    DisposeObject(hashTextStream);
    DisposeObject(md5List);

    fAry := umlGetDirListWithFullPath(APath);
    for n in fAry do
      begin
        suffixn := umlGetLastStr(n, '/\').Text;
        if dbEng.FastFieldCreate(aFieldPos, suffixn, '', fPos) then
            AddPath(n, fPos);
      end;
  end;

begin
  if dbEng <> nil then
    begin
      AddPath(InitDir, dbEng.RootField);
      dbEng.Update;
    end;
end;

procedure BatchImportPathToDBFile(InitDir, Filter, dbFile: SystemString);
var
  dbEng: TObjectDataManager;
begin
  dbEng := ObjectDataMarshal.NewDB(dbFile, False);
  BatchImportPathToDB(InitDir, Filter, dbEng);
  dbEng.Update;
  ObjectDataMarshal.CloseDB(dbEng);
end;

procedure BatchImportPathToDBStream(InitDir, Filter: SystemString; DBStream: TCoreClassStream);
var
  dbEng: TObjectDataManager;
begin
  dbEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, False, True, False);
  BatchImportPathToDB(InitDir, Filter, dbEng);
  dbEng.Update;
  DisposeObject(dbEng);
  DBStream.Position := 0;
end;

function ExtractFileInDB(dbEng: TObjectDataManager; FieldPos: Int64; FileName: SystemString; ExtractToStream: TCoreClassStream): Boolean;
var
  itmSrHnd: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  Result := False;
  if dbEng.ItemFastFindFirst(FieldPos, FileName, itmSrHnd) then
    begin
      if dbEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
        begin
          itmStream := TItemStream.Create(dbEng, itmHnd);

          if itmHnd.Name.Same(DBPackageMD5VerifyFileName) then
              ExtractToStream.CopyFrom(itmStream, itmStream.Size)
          else
              DecryptStream(itmStream, ExtractToStream);

          DoStatus('extract %s %s done!', [FileName, umlSizeToStr(ExtractToStream.Size).Text]);

          DisposeObject(itmStream);
          Result := True;
        end;
    end;
end;

function ExtractFileInDB(dbEng: TObjectDataManager; DBPath, FileName: SystemString; ExtractToStream: TCoreClassStream): Boolean;
var
  FieldPos: Int64;
begin
  Result := dbEng.GetPathField(DBPath, FieldPos);
  Result := Result and ExtractFileInDB(dbEng, FieldPos, FileName, ExtractToStream);
end;

function ExtractFileInDB(dbFileName, DBPath, FileName: SystemString; ExtractToStream: TCoreClassStream): Boolean;
var
  dbEng: TObjectDataManager;
begin
  if not umlFileExists(dbFileName) then
      Exit(False);
  dbEng := ObjectDataMarshal.Open(dbFileName, True);

  Result := ExtractFileInDB(dbEng, DBPath, FileName, ExtractToStream);

  ObjectDataMarshal.CloseDB(dbEng);
end;

function ExtractFileInDB(DBStream: TCoreClassStream; DBPath, FileName: SystemString; ExtractToStream: TCoreClassStream): Boolean;
var
  dbEng: TObjectDataManager;
begin
  DBStream.Position := 0;
  dbEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, True, False, False);

  Result := ExtractFileInDB(dbEng, DBPath, FileName, ExtractToStream);

  DisposeObject(dbEng);
  DBStream.Position := 0;
end;

function ExistsFileInDB(DBStream: TCoreClassStream; DBPath, FileName: SystemString): Boolean;
var
  dbEng: TObjectDataManager;
begin
  dbEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, True, False, False);

  Result := dbEng.ItemExists(DBPath, FileName);

  DisposeObject(dbEng);
end;

function ExistsFileInDB(dbFileName, DBPath, FileName: SystemString): Boolean;
var
  dbEng: TObjectDataManager;
begin
  if not umlFileExists(dbFileName) then
      Exit(False);
  dbEng := ObjectDataMarshal.Open(dbFileName, True);

  Result := dbEng.ItemExists(DBPath, FileName);

  ObjectDataMarshal.CloseDB(dbEng);
end;

procedure ExtractDBToPath(dbEng: TObjectDataManager; ExtractToDir: SystemString; OutputFileList: TCoreClassStrings);
  procedure ExportTo(AField: Int64; ToDir: SystemString);
  var
    itmSrHnd: TItemSearch;
    FieldSrHnd: TFieldSearch;
    fn: SystemString;
    fs: TCoreClassFileStream;

    itmHnd: TItemHandle;
    itmStream: TItemStream;
  begin
    umlCreateDirectory(ToDir);

    if dbEng.ItemFastFindFirst(AField, '*', itmSrHnd) then
      begin
        repeat
          try
            if not itmSrHnd.Name.Same(DBPackageMD5VerifyFileName) then
              if dbEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
                begin
                  fn := umlCombineFileName(ToDir, itmSrHnd.Name).Text;
                  fs := TCoreClassFileStream.Create(fn, fmCreate);
                  itmStream := TItemStream.Create(dbEng, itmHnd);

                  DecryptStream(itmStream, fs);
                  DoStatus('extract %s %s ok!', [fn, umlSizeToStr(fs.Size).Text]);

                  DisposeObject(fs);
                  DisposeObject(itmStream);
                  if OutputFileList <> nil then
                      OutputFileList.Add(fn);
                end;
          except
          end;
        until not dbEng.ItemFastFindNext(itmSrHnd);
      end;

    if dbEng.FieldFastFindFirst(AField, '*', FieldSrHnd) then
      begin
        repeat
            ExportTo(FieldSrHnd.HeaderPOS, umlCombineFileName(ToDir, FieldSrHnd.Name).Text);
        until not dbEng.FieldFastFindNext(FieldSrHnd);
      end;
  end;

begin
  if dbEng <> nil then
      ExportTo(dbEng.RootField, ExtractToDir);
end;

procedure ExtractDBToPath(dbEng: TObjectDataManager; ExtractToDir: SystemString);
begin
  ExtractDBToPath(dbEng, ExtractToDir, nil);
end;

procedure ExtractDBFileToPath(dbFile, ExtractToDir: SystemString);
var
  dbEng: TObjectDataManager;
begin
  if not umlFileExists(dbFile) then
      Exit;
  dbEng := ObjectDataMarshal.Open(dbFile, True);
  ExtractDBToPath(dbEng, ExtractToDir);
  ObjectDataMarshal.CloseDB(dbEng);
end;

function VerifyFileInDB(dbEng: TObjectDataManager): Integer;

var
  MD5Success, MD5Failed: Integer;

  procedure VerifyField(AField: Int64);
  var
    itmSrHnd: TItemSearch;
    FieldSrHnd: TFieldSearch;
    itmHnd: TItemHandle;
    itmStream: TItemStream;
    ms: TMemoryStream64;

    md5: TPascalString;
    md5List: THashStringList;
    hashTextStream: THashStringTextStream;
  begin
    md5List := THashStringList.Create;

    hashTextStream := THashStringTextStream.Create(md5List);
    if dbEng.ItemFastFindLast(AField, DBPackageMD5VerifyFileName, itmSrHnd) then
      begin
        if dbEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
          begin
            itmStream := TItemStream.Create(dbEng, itmHnd);
            hashTextStream.LoadFromStream(itmStream);
            DisposeObject(itmStream);
          end;
      end;
    DisposeObject(hashTextStream);

    if dbEng.ItemFastFindFirst(AField, '*', itmSrHnd) then
      begin
        repeat
          try
            if not itmSrHnd.Name.Same(DBPackageMD5VerifyFileName) then
              if dbEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
                begin
                  itmStream := TItemStream.Create(dbEng, itmHnd);

                  ms := TMemoryStream64.Create;
                  DecryptStream(itmStream, ms);
                  ms.Position := 0;
                  md5 := umlStreamMD5String(ms);
                  if not md5.Same(md5List[itmHnd.Name]) then
                    begin
                      DoStatus('%s verify failed!', [itmHnd.Name.Text]);
                      inc(MD5Failed);
                    end
                  else
                    begin
                      inc(MD5Success);
                    end;
                  DisposeObject(ms);

                  DisposeObject(itmStream);
                end;
          except
          end;
        until not dbEng.ItemFastFindNext(itmSrHnd);
      end;

    if dbEng.FieldFastFindFirst(AField, '*', FieldSrHnd) then
      begin
        repeat
            VerifyField(FieldSrHnd.HeaderPOS);
        until not dbEng.FieldFastFindNext(FieldSrHnd);
      end;

    DisposeObject(md5List);
  end;

begin
  Result := -1;
  if dbEng <> nil then
    begin
      MD5Success := 0;
      MD5Failed := 0;
      VerifyField(dbEng.RootField);
      Result := MD5Success;
      if MD5Failed > 0 then
          Result := -MD5Failed;
    end;
end;

function VerifyFileInDBStream(DBStream: TCoreClassStream): Integer;
var
  dbEng: TObjectDataManager;
begin
  Result := -1;
  DBStream.Position := 0;
  dbEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, True, False, False);
  if not dbEng.isAbort then
      Result := VerifyFileInDB(dbEng);
  DisposeObject(dbEng);
end;

function VerifyFileInDBFile(dbFile: SystemString): Integer;
var
  dbEng: TObjectDataManager;
begin
  Result := -1;
  if not umlFileExists(dbFile) then
      Exit;
  dbEng := ObjectDataMarshal.Open(dbFile, True);
  Result := VerifyFileInDB(dbEng);
  ObjectDataMarshal.CloseDB(dbEng);
end;

end.
