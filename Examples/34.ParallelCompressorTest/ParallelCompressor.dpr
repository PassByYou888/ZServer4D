program ParallelCompressor;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils, TypInfo,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  MemoryStream64;

procedure RunCompressorTest(buff_siz: Integer);

var
  DataBuff: TMemoryStream64;
  DataBuff_MD5: TMD5;

  procedure makeDataBuff;
  var
    i: Integer;
  begin
    DataBuff := TMemoryStream64.CustomCreate(1024);
    for i := 1 to buff_siz shr 2 do
        DataBuff.WriteInt32(umlRandom);
    DataBuff.Position := 0;
    DataBuff_MD5 := umlStreamMD5(DataBuff);
  end;

  procedure RunSelectCompressor;
  var
    i: Integer;
    scm: TSelectCompressionMethod;
    sour, dest: TMemoryStream64;
    tk: TTimeTick;
  begin
    for scm in [scmNone, scmZLIB, scmZLIB_Fast, scmZLIB_Max, scmDeflate, scmBRRC] do
      begin
        DoStatusNoLn(umlSizeToStr(buff_siz) + ' ');
        DoStatusNoLn('Select: ' + GetEnumName(TypeInfo(TSelectCompressionMethod), Integer(scm)));

        sour := TMemoryStream64.CustomCreate(1024);
        sour.LoadFromStream(DataBuff);
        dest := TMemoryStream64.CustomCreate(1024);

        tk := GetTimeTick;
        sour.Position := 0;
        dest.Position := 0;
        SelectCompressStream(scm, sour, dest);

        sour.Clear;
        sour.Position := 0;
        dest.Position := 0;
        SelectDecompressStream(dest, sour);

        if umlCompareMD5(umlStreamMD5(sour), DataBuff_MD5) then
            DoStatusNoLn(' ok. done time: %dms', [GetTimeTick - tk])
        else
            DoStatusNoLn(' failed.', []);

        DoStatusNoLn;

        DisposeObject(sour);
        DisposeObject(dest);
      end;
  end;

  procedure RunParallelCompressor;
  var
    i: Integer;
    scm: TSelectCompressionMethod;
    sour, dest: TMemoryStream64;
    tk: TTimeTick;
  begin
    for scm in [scmNone, scmZLIB, scmZLIB_Fast, scmZLIB_Max, scmDeflate, scmBRRC] do
      begin
        DoStatusNoLn(umlSizeToStr(buff_siz) + ' ');
        DoStatusNoLn('parallel: ' + GetEnumName(TypeInfo(TSelectCompressionMethod), Integer(scm)));

        sour := TMemoryStream64.CustomCreate(1024);
        sour.LoadFromStream(DataBuff);
        dest := TMemoryStream64.CustomCreate(1024);

        tk := GetTimeTick;
        sour.Position := 0;
        dest.Position := 0;
        ParallelCompressStream(scm, sour, dest);

        sour.Clear;
        sour.Position := 0;
        dest.Position := 0;
        ParallelDecompressStream(dest, sour);

        if umlCompareMD5(umlStreamMD5(sour), DataBuff_MD5) then
            DoStatusNoLn(' ok. done time: %dms', [GetTimeTick - tk])
        else
            DoStatusNoLn(' failed.', []);

        DoStatusNoLn;

        DisposeObject(sour);
        DisposeObject(dest);
      end;
  end;

begin
  makeDataBuff;
  RunParallelCompressor;
  RunSelectCompressor;
  DisposeObject(DataBuff);
  DoStatus('');
end;

begin
  RunCompressorTest(1024 * 1024 * 2);
  RunCompressorTest(1024 * 1024 * 2);

  RunCompressorTest(1024 * 1024 * 4);
  RunCompressorTest(1024 * 1024 * 4);

  RunCompressorTest(1024 * 1024 * 8);
  RunCompressorTest(1024 * 1024 * 8);

  DoStatus('finish.');
  readln;

end.
