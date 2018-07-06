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

procedure BeginImportStreamToDB(DBEng: TObjectDataManager; md5List: THashStringList);
procedure ImportStreamToDB(md5List: THashStringList; stream: TCoreClassStream; fileName: SystemString; DBEng: TObjectDataManager);
procedure EndImportStreamToDB(DBEng: TObjectDataManager; md5List: THashStringList);

procedure BatchImportPathToDB(InitDir, Filter: SystemString; DBEng: TObjectDataManager);
procedure BatchImportPathToDBFile(InitDir, Filter, dbFile: SystemString);
procedure BatchImportPathToDBStream(InitDir, Filter: SystemString; DBStream: TCoreClassStream);

function ExtractFileInDB(DBEng: TObjectDataManager; FieldPos: Int64; fileName: SystemString; ExtractToStream: TCoreClassStream): Boolean; overload;
function ExtractFileInDB(DBEng: TObjectDataManager; DBPath, fileName: SystemString; ExtractToStream: TCoreClassStream): Boolean; overload;
function ExtractFileInDB(dbFileName, DBPath, fileName: SystemString; ExtractToStream: TCoreClassStream): Boolean; overload;
function ExtractFileInDB(DBStream: TCoreClassStream; DBPath, fileName: SystemString; ExtractToStream: TCoreClassStream): Boolean; overload;
function ExistsFileInDB(DBStream: TCoreClassStream; DBPath, fileName: SystemString): Boolean; overload;
function ExistsFileInDB(dbFileName, DBPath, fileName: SystemString): Boolean; overload;

procedure ExtractDBToPath(DBEng: TObjectDataManager; ExtractToDir: SystemString; OutputFileList: TCoreClassStrings); overload;
procedure ExtractDBToPath(DBEng: TObjectDataManager; ExtractToDir: SystemString); overload;
procedure ExtractDBFileToPath(dbFile, ExtractToDir: SystemString);

function VerifyFileInDB(DBEng: TObjectDataManager): Integer;
function VerifyFileInDBStream(DBStream: TCoreClassStream): Integer;
function VerifyFileInDBFile(dbFile: SystemString): Integer;

var
  DBPackageMD5VerifyFileName: SystemString = '______md5info.txt';
  DBPackageFileCompressed: Boolean         = True;

implementation

uses MemoryStream64;

procedure PackStream(sour, GenPack: TCoreClassStream);
begin
  if DBPackageFileCompressed then
      MaxCompressStream(sour, GenPack)
  else
      GenPack.CopyFrom(sour, sour.Size);
end;

procedure UnPackStream(sour, UnPackTo: TCoreClassStream);
begin
  if DBPackageFileCompressed then
      DecompressStream(sour, UnPackTo)
  else
      UnPackTo.CopyFrom(sour, sour.Size);
end;

procedure BeginImportStreamToDB(DBEng: TObjectDataManager; md5List: THashStringList);
var
  hashTextStream: THashStringTextStream;
  srHnd: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  md5List.Clear;
  if DBEng.ItemFastFindLast(DBEng.RootField, DBPackageMD5VerifyFileName, srHnd) then
    begin
      hashTextStream := THashStringTextStream.Create(md5List);
      if DBEng.ItemFastOpen(srHnd.HeaderPOS, itmHnd) then
        begin
          itmStream := TItemStream.Create(DBEng, itmHnd);
          hashTextStream.LoadFromStream(itmStream);
          DisposeObject(itmStream);
        end;
      DisposeObject(hashTextStream);
      DBEng.FastDelete(DBEng.RootField, srHnd.HeaderPOS);
    end;
end;

procedure ImportStreamToDB(md5List: THashStringList; stream: TCoreClassStream; fileName: SystemString; DBEng: TObjectDataManager);
var
  FieldPos: Int64;
  srHnd: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
  md5: SystemString;
begin
  FieldPos := DBEng.RootField;

  if md5List.Exists(fileName) then
    if DBEng.ItemFastFindFirst(FieldPos, fileName, srHnd) then
        DBEng.FastDelete(FieldPos, srHnd.HeaderPOS);

  if DBEng.ItemFastCreate(FieldPos, fileName, '', itmHnd) then
    begin
      try
        itmStream := TItemStream.Create(DBEng, itmHnd);

        stream.Position := 0;
        md5 := umlStreamMD5Char(stream).Text;
        md5List.Add(itmHnd.Name, md5);

        stream.Position := 0;

        PackStream(stream, itmStream);

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

procedure EndImportStreamToDB(DBEng: TObjectDataManager; md5List: THashStringList);
var
  hashTextStream: THashStringTextStream;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  hashTextStream := THashStringTextStream.Create(md5List);
  if DBEng.ItemFastCreate(DBEng.RootField, DBPackageMD5VerifyFileName, '', itmHnd) then
    begin
      try
        itmStream := TItemStream.Create(DBEng, itmHnd);
        hashTextStream.SaveToStream(itmStream);
        itmStream.UpdateHandle;
        DisposeObject(itmStream);
      except
      end;
    end;
  DisposeObject(hashTextStream);

  DBEng.Update;
end;

procedure BatchImportPathToDB(InitDir, Filter: SystemString; DBEng: TObjectDataManager);

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

    if DBEng.ItemFastFindLast(aFieldPos, DBPackageMD5VerifyFileName, srHnd) then
      begin
        hashTextStream := THashStringTextStream.Create(md5List);
        if DBEng.ItemFastOpen(srHnd.HeaderPOS, itmHnd) then
          begin
            itmStream := TItemStream.Create(DBEng, itmHnd);
            hashTextStream.LoadFromStream(itmStream);
            DisposeObject(itmStream);
          end;
        DisposeObject(hashTextStream);
        DBEng.FastDelete(aFieldPos, srHnd.HeaderPOS);
      end;

    fAry := umlGetFileListWithFullPath(APath);
    for n in fAry do
      begin
        suffixn := umlGetFileName(n).Text;
        if umlMultipleMatch(Filter, suffixn) then
          begin
            if md5List.Exists(suffixn) then
              if DBEng.ItemFastFindFirst(aFieldPos, suffixn, srHnd) then
                  DBEng.FastDelete(aFieldPos, srHnd.HeaderPOS);

            if DBEng.ItemFastCreate(aFieldPos, suffixn, '', itmHnd) then
              begin
                try
                  fs := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
                  itmStream := TItemStream.Create(DBEng, itmHnd);

                  fs.Position := 0;
                  md5 := umlStreamMD5Char(fs).Text;
                  md5List.Add(itmHnd.Name, md5);

                  fs.Position := 0;
                  PackStream(fs, itmStream);

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

    if DBEng.ItemFastCreate(aFieldPos, DBPackageMD5VerifyFileName, '', itmHnd) then
      begin
        try
          itmStream := TItemStream.Create(DBEng, itmHnd);
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
        if DBEng.FastFieldCreate(aFieldPos, suffixn, '', fPos) then
            AddPath(n, fPos);
      end;
  end;

begin
  if DBEng <> nil then
    begin
      AddPath(InitDir, DBEng.RootField);
      DBEng.Update;
    end;
end;

procedure BatchImportPathToDBFile(InitDir, Filter, dbFile: SystemString);
var
  DBEng: TObjectDataManager;
begin
  DBEng := ObjectDataMarshal.NewDB(dbFile, False);
  BatchImportPathToDB(InitDir, Filter, DBEng);
  DBEng.Update;
  ObjectDataMarshal.CloseDB(DBEng);
end;

procedure BatchImportPathToDBStream(InitDir, Filter: SystemString; DBStream: TCoreClassStream);
var
  DBEng: TObjectDataManager;
begin
  DBEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, False, True, False);
  BatchImportPathToDB(InitDir, Filter, DBEng);
  DBEng.Update;
  DisposeObject(DBEng);
  DBStream.Position := 0;
end;

function ExtractFileInDB(DBEng: TObjectDataManager; FieldPos: Int64; fileName: SystemString; ExtractToStream: TCoreClassStream): Boolean;
var
  itmSrHnd: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  Result := False;
  if DBEng.ItemFastFindFirst(FieldPos, fileName, itmSrHnd) then
    begin
      if DBEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
        begin
          itmStream := TItemStream.Create(DBEng, itmHnd);

          if itmHnd.Name.Same(DBPackageMD5VerifyFileName) then
              ExtractToStream.CopyFrom(itmStream, itmStream.Size)
          else
              UnPackStream(itmStream, ExtractToStream);

          DoStatus('extract %s %s done!', [fileName, umlSizeToStr(ExtractToStream.Size).Text]);

          DisposeObject(itmStream);
          Result := True;
        end;
    end;
end;

function ExtractFileInDB(DBEng: TObjectDataManager; DBPath, fileName: SystemString; ExtractToStream: TCoreClassStream): Boolean;
var
  FieldPos: Int64;
begin
  Result := DBEng.GetPathField(DBPath, FieldPos);
  Result := Result and ExtractFileInDB(DBEng, FieldPos, fileName, ExtractToStream);
end;

function ExtractFileInDB(dbFileName, DBPath, fileName: SystemString; ExtractToStream: TCoreClassStream): Boolean;
var
  DBEng: TObjectDataManager;
begin
  if not umlFileExists(dbFileName) then
      Exit(False);
  DBEng := ObjectDataMarshal.Open(dbFileName, True);

  Result := ExtractFileInDB(DBEng, DBPath, fileName, ExtractToStream);

  ObjectDataMarshal.CloseDB(DBEng);
end;

function ExtractFileInDB(DBStream: TCoreClassStream; DBPath, fileName: SystemString; ExtractToStream: TCoreClassStream): Boolean;
var
  DBEng: TObjectDataManager;
begin
  DBStream.Position := 0;
  DBEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, True, False, False);

  Result := ExtractFileInDB(DBEng, DBPath, fileName, ExtractToStream);

  DisposeObject(DBEng);
  DBStream.Position := 0;
end;

function ExistsFileInDB(DBStream: TCoreClassStream; DBPath, fileName: SystemString): Boolean;
var
  DBEng: TObjectDataManager;
begin
  DBEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, True, False, False);

  Result := DBEng.ItemExists(DBPath, fileName);

  DisposeObject(DBEng);
end;

function ExistsFileInDB(dbFileName, DBPath, fileName: SystemString): Boolean;
var
  DBEng: TObjectDataManager;
begin
  if not umlFileExists(dbFileName) then
      Exit(False);
  DBEng := ObjectDataMarshal.Open(dbFileName, True);

  Result := DBEng.ItemExists(DBPath, fileName);

  ObjectDataMarshal.CloseDB(DBEng);
end;

procedure ExtractDBToPath(DBEng: TObjectDataManager; ExtractToDir: SystemString; OutputFileList: TCoreClassStrings);
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

    if DBEng.ItemFastFindFirst(AField, '*', itmSrHnd) then
      begin
        repeat
          try
            if not itmSrHnd.Name.Same(DBPackageMD5VerifyFileName) then
              if DBEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
                begin
                  fn := umlCombineFileName(ToDir, itmSrHnd.Name).Text;
                  fs := TCoreClassFileStream.Create(fn, fmCreate);
                  itmStream := TItemStream.Create(DBEng, itmHnd);

                  UnPackStream(itmStream, fs);
                  DoStatus('extract %s %s ok!', [fn, umlSizeToStr(fs.Size).Text]);

                  DisposeObject(fs);
                  DisposeObject(itmStream);
                  if OutputFileList <> nil then
                      OutputFileList.Add(fn);
                end;
          except
          end;
        until not DBEng.ItemFastFindNext(itmSrHnd);
      end;

    if DBEng.FieldFastFindFirst(AField, '*', FieldSrHnd) then
      begin
        repeat
            ExportTo(FieldSrHnd.HeaderPOS, umlCombineFileName(ToDir, FieldSrHnd.Name).Text);
        until not DBEng.FieldFastFindNext(FieldSrHnd);
      end;
  end;

begin
  if DBEng <> nil then
      ExportTo(DBEng.RootField, ExtractToDir);
end;

procedure ExtractDBToPath(DBEng: TObjectDataManager; ExtractToDir: SystemString);
begin
  ExtractDBToPath(DBEng, ExtractToDir, nil);
end;

procedure ExtractDBFileToPath(dbFile, ExtractToDir: SystemString);
var
  DBEng: TObjectDataManager;
begin
  if not umlFileExists(dbFile) then
      Exit;
  DBEng := ObjectDataMarshal.Open(dbFile, True);
  ExtractDBToPath(DBEng, ExtractToDir);
  ObjectDataMarshal.CloseDB(DBEng);
end;

function VerifyFileInDB(DBEng: TObjectDataManager): Integer;

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
    if DBEng.ItemFastFindLast(AField, DBPackageMD5VerifyFileName, itmSrHnd) then
      begin
        if DBEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
          begin
            itmStream := TItemStream.Create(DBEng, itmHnd);
            hashTextStream.LoadFromStream(itmStream);
            DisposeObject(itmStream);
          end;
      end;
    DisposeObject(hashTextStream);

    if DBEng.ItemFastFindFirst(AField, '*', itmSrHnd) then
      begin
        repeat
          try
            if not itmSrHnd.Name.Same(DBPackageMD5VerifyFileName) then
              if DBEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
                begin
                  itmStream := TItemStream.Create(DBEng, itmHnd);

                  ms := TMemoryStream64.Create;
                  UnPackStream(itmStream, ms);
                  ms.Position := 0;
                  md5 := umlStreamMD5String(ms);
                  if not md5.Same(md5List[itmHnd.Name]) then
                    begin
                      DoStatus('%s verify failed!', [itmHnd.Name.Text]);
                      Inc(MD5Failed);
                    end
                  else
                    begin
                      Inc(MD5Success);
                    end;
                  DisposeObject(ms);

                  DisposeObject(itmStream);
                end;
          except
          end;
        until not DBEng.ItemFastFindNext(itmSrHnd);
      end;

    if DBEng.FieldFastFindFirst(AField, '*', FieldSrHnd) then
      begin
        repeat
            VerifyField(FieldSrHnd.HeaderPOS);
        until not DBEng.FieldFastFindNext(FieldSrHnd);
      end;

    DisposeObject(md5List);
  end;

begin
  Result := -1;
  if DBEng <> nil then
    begin
      MD5Success := 0;
      MD5Failed := 0;
      VerifyField(DBEng.RootField);
      Result := MD5Success;
      if MD5Failed > 0 then
          Result := -MD5Failed;
    end;
end;

function VerifyFileInDBStream(DBStream: TCoreClassStream): Integer;
var
  DBEng: TObjectDataManager;
begin
  Result := -1;
  DBStream.Position := 0;
  DBEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, True, False, False);
  if not DBEng.isAbort then
      Result := VerifyFileInDB(DBEng);
  DisposeObject(DBEng);
end;

function VerifyFileInDBFile(dbFile: SystemString): Integer;
var
  DBEng: TObjectDataManager;
begin
  Result := -1;
  if not umlFileExists(dbFile) then
      Exit;
  DBEng := ObjectDataMarshal.Open(dbFile, True);
  Result := VerifyFileInDB(DBEng);
  ObjectDataMarshal.CloseDB(DBEng);
end;

end. 
 
