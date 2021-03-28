{ ****************************************************************************** }
{ * ZDB 2.0 file support, create by.qq600585                                   * }
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
unit ZDB2_FileEncoder;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  PascalStrings, UnicodeMixedLib, DoStatusIO, MemoryStream64, ListEngine,
  DataFrameEngine, ZDB2_Core, ZIOThread, CoreCipher;

type
  TZDB2_File_IntegerList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Integer>;

  TZDB2_FI = class
  public
    FileName: U_String;
    FileMD5: TMD5;
    FimeTime: TDateTime;
    Size: Int64;
    Compressed: Int64;
    OwnerPath: U_String;
    HandleArray: TZDB2_File_IntegerList;

    constructor Create();
    destructor Destroy; override;
    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);
  end;

  TZDB2_FIL_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TZDB2_FI>;

  TZDB2_FIL = class(TZDB2_FIL_Decl)
  public
    procedure Clean;
  end;

  TZDB2_FE_IO = class(TIOData)
  public
    Source: TMemoryStream64;
    Dest: TMemoryStream64;
    CM: TSelectCompressionMethod;
    constructor Create; override;
    destructor Destroy; override;
    procedure Process; override;
  end;

  TZDB2_File_OnProgress = procedure(State_: SystemString; Total, Current1, Current2: Int64) of object;

  TZDB2_File_Encoder = class
  private
    FCore: TZDB2_Core_Space;
    FPlace: TZDB2_SpacePlan;
    FIOThread: TIO_Thread;
    FEncoderFiles: TZDB2_FIL;
    FMaxQueue: Integer;
    FProgressInfo: SystemString;
    FOnProgress: TZDB2_File_OnProgress;
    FAborted: Boolean;
  public
    constructor Create(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCoreClassStream; ThNum_: Integer); overload;
    constructor CreateFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String; ThNum_: Integer); overload;
    constructor Create(ZDB2_Stream: TCoreClassStream; ThNum_: Integer); overload;
    constructor CreateFile(ZDB2_FileName: U_String; ThNum_: Integer); overload;
    destructor Destroy; override;
    function EncodeFromStream(stream: TCoreClassStream; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word): TZDB2_FI;
    function EncodeFromFile(FileName, OwnerPath: U_String; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word): TZDB2_FI;
    procedure EncodeFromDirectory(Directory_: U_String; IncludeSub: Boolean; OwnerPath_: U_String; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word);
    function Flush: Int64;
    property MaxQueue: Integer read FMaxQueue write FMaxQueue;
    property OnProgress: TZDB2_File_OnProgress read FOnProgress write FOnProgress;
    property Aborted: Boolean read FAborted write FAborted;
    property Core: TZDB2_Core_Space read FCore;

    class procedure Test;
  end;

  TZDB2_FD_IO = class(TIOData)
  public
    Source: TMemoryStream64;
    Dest: TMemoryStream64;
    constructor Create; override;
    destructor Destroy; override;
    procedure Process; override;
  end;

  TZDB2_File_Decoder = class
  private
    FCore: TZDB2_Core_Space;
    FIOThread: TIO_Thread;
    FDecoderFiles: TZDB2_FIL;
    FMaxQueue: Integer;
    FFileLog: TPascalStringList;
    FProgressInfo: SystemString;
    FOnProgress: TZDB2_File_OnProgress;
    FAborted: Boolean;
  public
    class function Check(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCoreClassStream): Boolean; overload;
    class function CheckFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String): Boolean; overload;
    class function Check(ZDB2_Stream: TCoreClassStream): Boolean; overload;
    class function CheckFile(ZDB2_FileName: U_String): Boolean; overload;
    constructor Create(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCoreClassStream; ThNum_: Integer); overload;
    constructor CreateFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String; ThNum_: Integer); overload;
    constructor Create(ZDB2_Stream: TCoreClassStream; ThNum_: Integer); overload;
    constructor CreateFile(ZDB2_FileName: U_String; ThNum_: Integer); overload;
    destructor Destroy; override;
    function CheckFileInfo(FileInfo_: TZDB2_FI): Boolean;
    function DecodeToStream(source_: TZDB2_FI; Dest_: TCoreClassStream): Boolean;
    function DecodeToDirectory(source_: TZDB2_FI; DestDirectory_: U_String): Boolean;
    property Files: TZDB2_FIL read FDecoderFiles;
    property MaxQueue: Integer read FMaxQueue write FMaxQueue;
    property FileLog: TPascalStringList read FFileLog;
    property OnProgress: TZDB2_File_OnProgress read FOnProgress write FOnProgress;
    property Aborted: Boolean read FAborted write FAborted;
    property Core: TZDB2_Core_Space read FCore;

    class procedure Test;
  end;

implementation

constructor TZDB2_FI.Create;
begin
  inherited Create;
  FileName := '';
  FileMD5 := NullMD5;
  FimeTime := umlNow();
  OwnerPath := '';
  Size := 0;
  Compressed := 0;
  HandleArray := TZDB2_File_IntegerList.Create;
end;

destructor TZDB2_FI.Destroy;
begin
  FileName := '';
  OwnerPath := '';
  DisposeObject(HandleArray);
  inherited Destroy;
end;

procedure TZDB2_FI.SaveToStream(stream: TMemoryStream64);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(FileName);
  d.WriteMD5(FileMD5);
  d.WriteDouble(FimeTime);
  d.WriteInt64(Size);
  d.WriteInt64(Compressed);
  d.WriteString(OwnerPath);
  with d.WriteArrayInteger do
    for i := 0 to HandleArray.Count - 1 do
        Add(HandleArray[i]);
  d.EncodeTo(stream, True, False);
  DisposeObject(d);
end;

procedure TZDB2_FI.LoadFromStream(stream: TMemoryStream64);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  d.DecodeFrom(stream, True);
  FileName := d.Reader.ReadString;
  FileMD5 := d.Reader.ReadMD5;
  FimeTime := d.Reader.ReadDouble;
  Size := d.Reader.ReadInt64;
  Compressed := d.Reader.ReadInt64;
  OwnerPath := d.Reader.ReadString;
  with d.Reader.ReadArrayInteger do
    for i := 0 to Count - 1 do
        HandleArray.Add(Buffer[i]);
  DisposeObject(d);
end;

procedure TZDB2_FIL.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(items[i]);
  inherited Clear;
end;

constructor TZDB2_FE_IO.Create;
begin
  inherited Create;
  Source := TMemoryStream64.Create;
  Dest := TMemoryStream64.Create;
  CM := TSelectCompressionMethod.scmZLIB;
end;

destructor TZDB2_FE_IO.Destroy;
begin
  DisposeObject(Source);
  DisposeObject(Dest);
  inherited Destroy;
end;

procedure TZDB2_FE_IO.Process;
begin
  Source.Position := 0;
  Dest.Clear;
  if Source.Size < 128 then
      CM := TSelectCompressionMethod.scmNone;
  SelectCompressStream(CM, Source, Dest);
end;

constructor TZDB2_File_Encoder.Create(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCoreClassStream; ThNum_: Integer);
var
  P: PIOHnd;
begin
  inherited Create;
  new(P);
  InitIOHnd(P^);
  if not umlFileCreateAsStream(ZDB2_Stream, P^) then
      RaiseInfo('create stream error.');
  FCore := TZDB2_Core_Space.Create(P);
  FCore.Cipher := Cipher_;
  FCore.AutoCloseIOHnd := True;
  FCore.AutoFreeIOHnd := True;
  FPlace := TZDB2_SpacePlan.Create(FCore);

  FIOThread := TIO_Thread.Create(ThNum_);
  FEncoderFiles := TZDB2_FIL.Create;
  FMaxQueue := ThNum_ * 5;
  FProgressInfo := '';
  FOnProgress := nil;
  FAborted := False;
end;

constructor TZDB2_File_Encoder.CreateFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String; ThNum_: Integer);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(ZDB2_FileName, fmCreate);
  Create(Cipher_, fs, ThNum_);
  FCore.Space_IOHnd^.AutoFree := True;
end;

constructor TZDB2_File_Encoder.Create(ZDB2_Stream: TCoreClassStream; ThNum_: Integer);
begin
  Create(nil, ZDB2_Stream, ThNum_);
end;

constructor TZDB2_File_Encoder.CreateFile(ZDB2_FileName: U_String; ThNum_: Integer);
begin
  CreateFile(nil, ZDB2_FileName, ThNum_);
end;

destructor TZDB2_File_Encoder.Destroy;
begin
  DisposeObject(FIOThread);
  FEncoderFiles.Clean;
  DisposeObject(FEncoderFiles);
  DisposeObject(FPlace);
  DisposeObject(FCore);
  inherited Destroy;
end;

function TZDB2_File_Encoder.EncodeFromStream(stream: TCoreClassStream; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word): TZDB2_FI;
var
  Activted: TAtomBool;

{$IFDEF FPC}
  procedure FPC_ThRun_;
  var
    Total_: Int64;
    thIOData_: TZDB2_FE_IO;
  begin
    Total_ := stream.Size;
    stream.Position := 0;

    while (Total_ > 0) and (not FAborted) do
      begin
        thIOData_ := TZDB2_FE_IO.Create;
        thIOData_.CM := CM;
        if Total_ > chunkSize_ then
          begin
            thIOData_.Source.Size := chunkSize_;
            dec(Total_, chunkSize_);
          end
        else
          begin
            thIOData_.Source.Size := Total_;
            Total_ := 0;
          end;
        if stream.Read(thIOData_.Source.Memory^, thIOData_.Source.Size) <> thIOData_.Source.Size then
            break;
        FIOThread.EnQueue(thIOData_);
        while FIOThread.Count > FMaxQueue do
            TCompute.Sleep(1);
      end;

    FIOThread.Wait();
    Activted.V := False;
  end;
{$ENDIF FPC}


var
  FileInfo: TZDB2_FI;
  ioData: TZDB2_FE_IO;
  id: Integer;
  CompleteSize_: Int64;
begin
  FileInfo := TZDB2_FI.Create;
  FileInfo.Size := stream.Size;
  FileInfo.Compressed := 0;
  FileInfo.FileMD5 := umlStreamMD5(stream);

  if FileInfo.Size = 0 then
    begin
      FEncoderFiles.Add(FileInfo);
      exit;
    end;

  Activted := TAtomBool.Create(True);
  CompleteSize_ := 0;

{$IFDEF FPC}
  TCompute.RunP_NP(@FPC_ThRun_);
{$ELSE FPC}
  TCompute.RunP_NP(procedure
    var
      Total_: Int64;
      thIOData_: TZDB2_FE_IO;
    begin
      Total_ := stream.Size;
      stream.Position := 0;

      while (Total_ > 0) and (not FAborted) do
        begin
          thIOData_ := TZDB2_FE_IO.Create;
          thIOData_.CM := CM;
          if Total_ > chunkSize_ then
            begin
              thIOData_.Source.Size := chunkSize_;
              dec(Total_, chunkSize_);
            end
          else
            begin
              thIOData_.Source.Size := Total_;
              Total_ := 0;
            end;
          if stream.Read(thIOData_.Source.Memory^, thIOData_.Source.Size) <> thIOData_.Source.Size then
              break;
          FIOThread.EnQueue(thIOData_);
          while FIOThread.Count > FMaxQueue do
              TCompute.Sleep(1);
        end;

      FIOThread.Wait();
      Activted.V := False;
    end);
{$ENDIF FPC}
  CompleteSize_ := 0;
  while Activted.V do
    begin
      ioData := TZDB2_FE_IO(FIOThread.DeQueue(False));
      if ioData <> nil then
        begin
          inc(CompleteSize_, ioData.Source.Size);
          inc(FileInfo.Compressed, ioData.Dest.Size);
          if FPlace.WriteStream(ioData.Dest, BlockSize_, id) then
              FileInfo.HandleArray.Add(id)
          else
              DoStatus('TZDB2_File_Encoder error.');
          DisposeObject(ioData);
          if Assigned(FOnProgress) then
              FOnProgress(FProgressInfo + PFormat('(%s/%s compress:%s)',
              [umlSizeToStr(FileInfo.Size).Text, umlSizeToStr(CompleteSize_).Text, umlSizeToStr(FileInfo.Compressed).Text]),
              FileInfo.Size, CompleteSize_, FileInfo.Compressed);
        end
      else
          TCompute.Sleep(1);
    end;

  DisposeObject(Activted);
  FEncoderFiles.Add(FileInfo);
  Result := FileInfo;
end;

function TZDB2_File_Encoder.EncodeFromFile(FileName, OwnerPath: U_String; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word): TZDB2_FI;
var
  fs: TCoreClassFileStream;
  prefix: SystemString;
begin
  Result := nil;
  if not umlFileExists(FileName) then
      exit;
  FProgressInfo := umlCombineFileName(OwnerPath, umlGetFileName(FileName));
  fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  Result := EncodeFromStream(fs, chunkSize_, CM, BlockSize_);
  Result.FileName := umlGetFileName(FileName);
  Result.OwnerPath := OwnerPath;
  Result.FimeTime := umlGetFileTime(FileName);

  if (FCore.Space_IOHnd^.Handle is TCoreClassFileStream) then
      prefix := umlGetFileName(TCoreClassFileStream(FCore.Space_IOHnd^.Handle).FileName)
  else if (FCore.Space_IOHnd^.Handle is TReliableFileStream) then
      prefix := umlGetFileName(TReliableFileStream(FCore.Space_IOHnd^.Handle).FileName)
  else
      prefix := 'encode';

  DoStatus('%s %s %s->%s ratio:%d%%',
    [
    prefix,
    FProgressInfo,
    umlSizeToStr(Result.Size).Text,
    umlSizeToStr(Result.Compressed).Text,
    100 - umlPercentageToInt64(Result.Size, Result.Compressed)]);
end;

procedure TZDB2_File_Encoder.EncodeFromDirectory(Directory_: U_String; IncludeSub: Boolean; OwnerPath_: U_String; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word);
var
  fAry: U_StringArray;
  n: SystemString;
begin
  fAry := umlGetFileListWithFullPath(Directory_);
  for n in fAry do
    if not FAborted then
        EncodeFromFile(n, OwnerPath_, chunkSize_, CM, BlockSize_)
    else
        exit;

  if IncludeSub then
    begin
      fAry := umlGetDirListWithFullPath(Directory_);
      for n in fAry do
        if not FAborted then
            EncodeFromDirectory(n, IncludeSub, umlCombineWinPath(OwnerPath_, umlGetLastStr(n, '\/')), chunkSize_, CM, BlockSize_)
        else
            exit;
    end;
end;

function TZDB2_File_Encoder.Flush: Int64;
var
  i: Integer;
  d: TDFE;
  m64: TMemoryStream64;
  FileInfo_ID: Integer;
begin
  Result := 0;
  d := TDFE.Create;
  for i := 0 to FEncoderFiles.Count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      FEncoderFiles[i].SaveToStream(m64);
      inc(Result, FEncoderFiles[i].Size);
      d.WriteStream(m64);
      DisposeObject(m64);
    end;
  m64 := TMemoryStream64.Create;
  d.EncodeAsZLib(m64, False);
  if not FPlace.WriteStream(m64, 1024, FileInfo_ID) then
      RaiseInfo('flush error.');
  DisposeObject(m64);
  DisposeObject(d);
  FPlace.Flush;
  PInteger(@FCore.UserCustomHeader^[$F0])^ := FileInfo_ID;
  FCore.Save;
  FEncoderFiles.Clean;
end;

class procedure TZDB2_File_Encoder.Test;
var
  zdb_stream: TMemoryStream64;
  en: TZDB2_File_Encoder;
  tmp: TMemoryStream64;
  i: Integer;
begin
  zdb_stream := TMemoryStream64.CustomCreate(1024 * 1024 * 8);
  en := TZDB2_File_Encoder.Create(zdb_stream, 8);

  for i := 0 to 20 do
    begin
      tmp := TMemoryStream64.Create;
      tmp.Size := umlRandomRange(16 * 1024 * 1024, 64 * 1024 * 1024);
      MT19937Rand32(MaxInt, tmp.Memory, tmp.Size div 4);
      en.EncodeFromStream(tmp, 512 * 1024, TSelectCompressionMethod.scmZLIB, 8192);
      DisposeObject(tmp);
    end;
  en.Flush;

  DisposeObject(en);
  DisposeObject(zdb_stream);
end;

constructor TZDB2_FD_IO.Create;
begin
  inherited Create;
  Source := TMemoryStream64.Create;
  Dest := TMemoryStream64.Create;
end;

destructor TZDB2_FD_IO.Destroy;
begin
  DisposeObject(Source);
  DisposeObject(Dest);
  inherited Destroy;
end;

procedure TZDB2_FD_IO.Process;
begin
  SelectDecompressStream(Source, Dest);
end;

class function TZDB2_File_Decoder.Check(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCoreClassStream): Boolean;
var
  ioHnd: TIOHnd;
  tmp: TZDB2_Core_Space;
  id: Integer;
  mem: TZDB2_Mem;
  d: TDFE;
begin
  Result := False;
  InitIOHnd(ioHnd);
  if umlFileOpenAsStream('', ZDB2_Stream, ioHnd, True) then
    begin
      tmp := TZDB2_Core_Space.Create(@ioHnd);
      tmp.Cipher := Cipher_;
      if tmp.Open then
        begin
          id := PInteger(@tmp.UserCustomHeader^[$F0])^;
          if tmp.Check(id) then
            begin
              mem := TZDB2_Mem.Create.Create;
              if tmp.ReadData(mem, id) then
                begin
                  d := TDFE.Create;
                  try
                    d.LoadFromStream(mem.Stream64);
                    Result := d.Count >= 0;
                  except
                      Result := False;
                  end;
                  DisposeObject(d);
                end;
              DisposeObject(mem);
            end;
        end;
      DisposeObject(tmp);
    end;
end;

class function TZDB2_File_Decoder.CheckFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String): Boolean;
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(ZDB2_FileName, fmOpenRead or fmShareDenyNone);
  try
      Result := Check(Cipher_, fs);
  finally
      DisposeObject(fs);
  end;
end;

class function TZDB2_File_Decoder.Check(ZDB2_Stream: TCoreClassStream): Boolean;
begin
  Result := TZDB2_File_Decoder.Check(nil, ZDB2_Stream);
end;

class function TZDB2_File_Decoder.CheckFile(ZDB2_FileName: U_String): Boolean;
begin
  Result := TZDB2_File_Decoder.CheckFile(nil, ZDB2_FileName);
end;

constructor TZDB2_File_Decoder.Create(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCoreClassStream; ThNum_: Integer);
var
  P: PIOHnd;
  mem: TZDB2_Mem;
  d: TDFE;
  i: Integer;
  m64: TMemoryStream64;
  fi: TZDB2_FI;
begin
  inherited Create;
  new(P);
  InitIOHnd(P^);
  if not umlFileOpenAsStream('', ZDB2_Stream, P^, True) then
      RaiseInfo('create stream error.');
  FCore := TZDB2_Core_Space.Create(P);
  FCore.Cipher := Cipher_;
  FCore.AutoCloseIOHnd := True;
  FCore.AutoFreeIOHnd := True;
  FCore.Open;

  FIOThread := TIO_Thread.Create(ThNum_);
  FMaxQueue := ThNum_ * 5;

  FDecoderFiles := TZDB2_FIL.Create;

  mem := TZDB2_Mem.Create.Create;
  if FCore.ReadData(mem, PInteger(@FCore.UserCustomHeader^[$F0])^) then
    begin
      d := TDFE.Create;
      d.LoadFromStream(mem.Stream64);
      while d.Reader.NotEnd do
        begin
          m64 := TMemoryStream64.Create;
          d.Reader.ReadStream(m64);
          m64.Position := 0;
          fi := TZDB2_FI.Create;
          fi.LoadFromStream(m64);
          FDecoderFiles.Add(fi);
          DisposeObject(m64);
        end;
      DisposeObject(d);
    end;
  DisposeObject(mem);

  FFileLog := TPascalStringList.Create;
  FProgressInfo := '';
  FOnProgress := nil;
  FAborted := False;
end;

constructor TZDB2_File_Decoder.CreateFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String; ThNum_: Integer);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(ZDB2_FileName, fmOpenRead or fmShareDenyNone);
  Create(Cipher_, fs, ThNum_);
  FCore.Space_IOHnd^.AutoFree := True;
end;

constructor TZDB2_File_Decoder.Create(ZDB2_Stream: TCoreClassStream; ThNum_: Integer);
begin
  Create(nil, ZDB2_Stream, ThNum_);
end;

constructor TZDB2_File_Decoder.CreateFile(ZDB2_FileName: U_String; ThNum_: Integer);
begin
  CreateFile(nil, ZDB2_FileName, ThNum_);
end;

destructor TZDB2_File_Decoder.Destroy;
begin
  FIOThread.ThEnd;
  DisposeObject(FIOThread);
  FDecoderFiles.Clean;
  DisposeObject(FDecoderFiles);
  DisposeObject(FFileLog);
  DisposeObject(FCore);
  inherited Destroy;
end;

function TZDB2_File_Decoder.CheckFileInfo(FileInfo_: TZDB2_FI): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FileInfo_.HandleArray.Count - 1 do
      Result := Result and FCore.Check(FileInfo_.HandleArray[i]);
end;

function TZDB2_File_Decoder.DecodeToStream(source_: TZDB2_FI; Dest_: TCoreClassStream): Boolean;
var
  Activted: TAtomBool;

{$IFDEF FPC}
  procedure FPC_ThRun_;
  var
    i: Integer;
    thIOData_: TZDB2_FD_IO;
  begin
    for i := 0 to source_.HandleArray.Count - 1 do
      begin
        thIOData_ := TZDB2_FD_IO.Create;
        FCore.ReadStream(thIOData_.Source, source_.HandleArray[i]);
        thIOData_.Source.Position := 0;
        FIOThread.EnQueue(thIOData_);
        while FIOThread.Count > FMaxQueue do
            TCompute.Sleep(1);
        if FAborted then
            break;
      end;

    FIOThread.Wait();
    Activted.V := False;
  end;
{$ENDIF FPC}


var
  compSiz_: Int64;
  ioData: TZDB2_FD_IO;
begin
  Activted := TAtomBool.Create(True);

{$IFDEF FPC}
  TCompute.RunP_NP(@FPC_ThRun_);
{$ELSE FPC}
  TCompute.RunP_NP(procedure
    var
      i: Integer;
      thIOData_: TZDB2_FD_IO;
    begin
      for i := 0 to source_.HandleArray.Count - 1 do
        begin
          thIOData_ := TZDB2_FD_IO.Create;
          FCore.ReadStream(thIOData_.Source, source_.HandleArray[i]);
          thIOData_.Source.Position := 0;
          FIOThread.EnQueue(thIOData_);
          while FIOThread.Count > FMaxQueue do
              TCompute.Sleep(1);
          if FAborted then
              break;
        end;

      FIOThread.Wait();
      Activted.V := False;
    end);
{$ENDIF FPC}
  compSiz_ := 0;
  while Activted.V do
    begin
      ioData := TZDB2_FD_IO(FIOThread.DeQueue(False));
      if ioData <> nil then
        begin
          inc(compSiz_, ioData.Source.Size);
          Dest_.Write(ioData.Dest.Memory^, ioData.Dest.Size);
          DisposeObject(ioData);

          if Assigned(FOnProgress) then
              FOnProgress(FProgressInfo + PFormat(' %s -> %s',
              [umlSizeToStr(Dest_.Size).Text, umlSizeToStr(source_.Size).Text]), source_.Size, Dest_.Size, compSiz_);
        end
      else
          TCompute.Sleep(1);
    end;

  DisposeObject(Activted);
  Result := True;
end;

function TZDB2_File_Decoder.DecodeToDirectory(source_: TZDB2_FI; DestDirectory_: U_String): Boolean;
var
  path_, fn: U_String;
  fs: TCoreClassFileStream;
begin
  Result := False;
  if source_.FileName.L = 0 then
      exit;
  if not CheckFileInfo(source_) then
    begin
      DoStatus('ZDB2 data error: %s', [source_.FileName.Text]);
      exit;
    end;
  path_ := umlCombinePath(DestDirectory_, source_.OwnerPath);
  if not umlDirectoryExists(path_) then
    begin
      umlCreateDirectory(path_);
      if not umlDirectoryExists(path_) then
        begin
          DoStatus('illegal directory %s', [path_.Text]);
          exit;
        end;
    end;
  fn := umlCombineFileName(path_, source_.FileName);

  try
    fs := TCoreClassFileStream.Create(fn, fmCreate);
    try
      FProgressInfo := umlGetFileName(fn);
      Result := DecodeToStream(source_, fs);
    finally
      DisposeObject(fs);
      umlSetFileTime(fn, source_.FimeTime);
      DoStatus('decode %s %s -> %s ratio:%d%%',
        [FProgressInfo, umlSizeToStr(source_.Compressed).Text, umlSizeToStr(source_.Size).Text, 100 - umlPercentageToInt64(source_.Size, source_.Compressed)]);
      FFileLog.Add(fn);
    end;
  except
    DoStatus('illegal file %s', [fn.Text]);
    exit;
  end;
end;

class procedure TZDB2_File_Decoder.Test;
var
  Cipher_: TZDB2_Cipher;
  zdb_stream: TMemoryStream64;
  en: TZDB2_File_Encoder;
  de: TZDB2_File_Decoder;
  tmp: TMemoryStream64;
  i: Integer;
  fi: TZDB2_FI;
begin
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRC6, 'hello world.', 1, False, True);
  zdb_stream := TMemoryStream64.CustomCreate(1024 * 1024 * 8);

  en := TZDB2_File_Encoder.Create(Cipher_, zdb_stream, 4);
  for i := 0 to 10 do
    begin
      tmp := TMemoryStream64.Create;
      tmp.Size := umlRandomRange(16 * 1024, 64 * 1024);
      MT19937Rand32(MaxInt, tmp.Memory, tmp.Size div 4);
      fi := en.EncodeFromStream(tmp, 8192, TSelectCompressionMethod.scmZLIB_Max, 1024);
      DisposeObject(tmp);
    end;
  en.Flush;
  DisposeObject(en);

  if TZDB2_File_Decoder.Check(Cipher_, zdb_stream) then
      DoStatus('TZDB2_File_Decoder check ok.')
  else
      DoStatus('TZDB2_File_Decoder check error.');

  de := TZDB2_File_Decoder.Create(Cipher_, zdb_stream, 4);
  for i := 0 to de.Files.Count - 1 do
    begin
      tmp := TMemoryStream64.Create;
      fi := de.Files[i];
      if de.DecodeToStream(fi, tmp) then
        begin
          if umlCompareMD5(umlStreamMD5(tmp), fi.FileMD5) then
              DoStatus('TZDB2_File_Decoder md5 ok.')
          else
              DoStatus('TZDB2_File_Decoder md5 error.');
        end
      else
          DoStatus('TZDB2_File_Decoder error.');
      DisposeObject(tmp);
    end;
  DisposeObject(de);

  DisposeObject(zdb_stream);
  DisposeObject(Cipher_);
end;

initialization

// TZDB2_File_Encoder.Test;
// TZDB2_File_Decoder.Test;

finalization

end.
