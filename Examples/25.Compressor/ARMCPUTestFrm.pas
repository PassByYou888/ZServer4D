unit ARMCPUTestFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  CoreClasses, CoreCompress, MemoryStream64, DoStatusIO, UnicodeMixedLib, PascalStrings,
  FMX.Layouts;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    DeflateCompressorButton: TButton;
    BRRCCompressorButton: TButton;
    ZLibCompressorButton: TButton;
    GlobalLayout: TLayout;
    MHTestButton: TButton;
    procedure DeflateCompressorButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BRRCCompressorButtonClick(Sender: TObject);
    procedure ZLibCompressorButtonClick(Sender: TObject);
    procedure MHTestButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoStatusNear(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses MH;

procedure TForm1.BRRCCompressorButtonClick(Sender: TObject);
var
  sour           : TMemoryStream64;
  dest           : TMemoryStream64;
  i              : Integer;
  d, cTick, dTick: TTimeTick;
  c              : TCompressor;
  cmd5, dmd5     : TMD5;
begin
  GlobalLayout.Enabled := False;
  DoStatus('');

  sour := TMemoryStream64.Create;
  sour.Size := 999991;
  RandSeed := Random(maxInt);
  for i := 0 to sour.Size div 4 do
      PInteger(NativeUInt(sour.Memory) + i * 4)^ := Random($FFFF);

  cmd5 := umlStreamMD5(sour);

  dest := TMemoryStream64.Create;
  c := TCompressorBRRC.Create;
  d := GetTimeTick;
  c.CompressStream(sour, 0, sour.Size, dest);
  cTick := GetTimeTick - d;
  DoStatus('BBRC (compress) sour:%s dest:%s', [umlSizetoStr(sour.Size).Text, umlSizetoStr(dest.Size).Text]);
  sour.Clear;
  dest.Position := 0;
  d := GetTimeTick;
  c.DecompressStream(dest, sour);
  dTick := GetTimeTick - d;
  dmd5 := umlStreamMD5(sour);
  DisposeObject(c);

  DoStatus('BBRC sour md5:%s', [umlMD5ToStr(cmd5).Text]);
  DoStatus('BBRC dest md5:%s', [umlMD5ToStr(dmd5).Text]);

  if umlMD5Compare(cmd5, dmd5) then
      DoStatus('BBRC md5 compare success!', [])
  else
      DoStatus('BBRC md5 compare failed!', []);

  DoStatus('BBRC encrypt:%dms decrypt:%dms', [cTick, dTick]);

  DisposeObject([sour, dest]);
  GlobalLayout.Enabled := True;
end;

procedure TForm1.DeflateCompressorButtonClick(Sender: TObject);
var
  sour           : TMemoryStream64;
  dest           : TMemoryStream64;
  i              : Integer;
  d, cTick, dTick: TTimeTick;
  c              : TCompressor;
  cmd5, dmd5     : TMD5;
begin
  GlobalLayout.Enabled := False;
  DoStatus('');

  sour := TMemoryStream64.Create;
  sour.Size := 999991;
  RandSeed := Random(maxInt);
  for i := 0 to sour.Size div 4 do
      PInteger(NativeUInt(sour.Memory) + i * 4)^ := Random($FFFF);

  cmd5 := umlStreamMD5(sour);

  dest := TMemoryStream64.Create;
  c := TCompressorDeflate.Create;
  d := GetTimeTick;
  c.CompressStream(sour, 0, sour.Size, dest);
  cTick := GetTimeTick - d;
  DoStatus('Deflate (compress) sour:%s dest:%s', [umlSizetoStr(sour.Size).Text, umlSizetoStr(dest.Size).Text]);
  sour.Clear;
  dest.Position := 0;
  d := GetTimeTick;
  c.DecompressStream(dest, sour);
  dTick := GetTimeTick - d;
  dmd5 := umlStreamMD5(sour);
  DisposeObject(c);

  DoStatus('Deflate sour md5:%s', [umlMD5ToStr(cmd5).Text]);
  DoStatus('Deflate dest md5:%s', [umlMD5ToStr(dmd5).Text]);

  if umlMD5Compare(cmd5, dmd5) then
      DoStatus('Deflate md5 compare success!', [])
  else
      DoStatus('Deflate md5 compare failed!', []);

  DoStatus('Deflate encrypt:%dms decrypt:%dms', [cTick, dTick]);

  DisposeObject([sour, dest]);
  GlobalLayout.Enabled := True;
end;

procedure TForm1.DoStatusNear(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
  Application.ProcessMessages;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusNear);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
end;

procedure TForm1.MHTestButtonClick(Sender: TObject);
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    s2: string;
    s3: TPascalString;
    obj: TObject;
  end;

var
  p: PMyRec;
begin
  MH.BeginMemoryHook_1;
  new(p);
  p^.s1 := #7#8#9;
  p^.s2 := '中文';
  p^.s3.Text := #1#2#3;
  p^.obj := TObject.Create;
  MH.EndMemoryHook_1;

  // 这里我们会发现泄漏
  DoStatus('TMyRec总分分配了 %d 次内存，占用 %d 字节空间，', [MH.GetHookPtrList_1.Count, MH.GetHookMemorySize_1]);

  MH.GetHookPtrList_1.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      DoStatus('泄漏的地址:0x%s', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2)]);
      DoStatus(NPtr, uData, 80);

      // 现在我们可以直接释放该地址
      FreeMem(NPtr);

      DoStatus('已成功释放 地址:0x%s 占用了 %d 字节内存', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2), uData]);
    end);
end;

procedure TForm1.ZLibCompressorButtonClick(Sender: TObject);
var
  sour           : TMemoryStream64;
  dest           : TMemoryStream64;
  i              : Integer;
  d, cTick, dTick: TTimeTick;
  cmd5, dmd5     : TMD5;
begin
  GlobalLayout.Enabled := False;
  DoStatus('');

  sour := TMemoryStream64.Create;
  sour.Size := 999991;
  RandSeed := Random(maxInt);
  for i := 0 to sour.Size div 4 do
      PInteger(NativeUInt(sour.Memory) + i * 4)^ := Random($FFFF);

  cmd5 := umlStreamMD5(sour);

  dest := TMemoryStream64.Create;
  d := GetTimeTick;
  CompressStream(sour, dest);
  cTick := GetTimeTick - d;
  DoStatus('ZLib (compress) sour:%s dest:%s', [umlSizetoStr(sour.Size).Text, umlSizetoStr(dest.Size).Text]);
  sour.Clear;
  dest.Position := 0;
  d := GetTimeTick;
  DecompressStream(dest, sour);
  dTick := GetTimeTick - d;
  dmd5 := umlStreamMD5(sour);

  DoStatus('ZLib sour md5:%s', [umlMD5ToStr(cmd5).Text]);
  DoStatus('ZLib dest md5:%s', [umlMD5ToStr(dmd5).Text]);

  if umlMD5Compare(cmd5, dmd5) then
      DoStatus('ZLib md5 compare success!', [])
  else
      DoStatus('ZLib md5 compare failed!', []);

  DoStatus('ZLib encrypt:%dms decrypt:%dms', [cTick, dTick]);

  DisposeObject([sour, dest]);
  GlobalLayout.Enabled := True;
end;

end.
