unit FilePackageWithZDBMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ObjectDataManagerFrameUnit, MemoryStream64, LibraryManager, ObjectDataManager,

  UnicodeMixedLib, CoreClasses, DoStatusIO, PascalStrings, CoreCipher;

type
  TFilePackageWithZDBMainForm = class(TForm, IMemoryStream64ReadWriteTrigger)
    TopPanel: TPanel;
    NewButton: TButton;
    OpenButton: TButton;
    SaveButton: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Bevel1: TBevel;
    SaveAsButton: TButton;
    Memo: TMemo;
    MD5Edit: TMemo;
    CacheStateMemo: TMemo;
    Timer: TTimer;
    Bevel2: TBevel;
    RecalcMD5Button: TButton;
    CompressAsButton: TButton;
    Bevel3: TBevel;
    SaveAsCompressedDialog: TSaveDialog;
    Splitter1: TSplitter;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NewButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure RecalcMD5ButtonClick(Sender: TObject);
    procedure CompressAsButtonClick(Sender: TObject);
  private
    { Private declarations }
    FDBEng     : TObjectDataManagerOfCache;
    FDBManFrame: TObjectDataManagerFrame;

    FTotalRead, FTotalWrite: Int64;

    FOpenFile: U_String;
    procedure DoStatusNear(AText: SystemString; const ID: Integer);
    procedure TriggerWrite64(Count: Int64);
    procedure TriggerRead64(Count: Int64);
  public
    { Public declarations }
  end;

var
  FilePackageWithZDBMainForm: TFilePackageWithZDBMainForm;

implementation

{$R *.dfm}


procedure TFilePackageWithZDBMainForm.FormCreate(Sender: TObject);
begin
  FTotalRead := 0;
  FTotalWrite := 0;
  AddDoStatusHook(Self, DoStatusNear);

  FDBEng := TObjectDataManagerOfCache.CreateAsStream(TMemoryStream64OfReadWriteTrigger.Create(Self), '', ObjectDataMarshal.ID, False, True, True);

  FDBManFrame := TObjectDataManagerFrame.Create(Self);
  FDBManFrame.Parent := Self;
  FDBManFrame.Align := alClient;

  FDBManFrame.ResourceData := FDBEng;

  FOpenFile := '';

  FDBEng.Update;
  FDBEng.StreamEngine.Position := 0;

  MD5Edit.Text := umlStreamMD5String(FDBEng.StreamEngine);
end;

procedure TFilePackageWithZDBMainForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  DisposeObject(FDBManFrame);
  DisposeObject(FDBEng);
end;

procedure TFilePackageWithZDBMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TFilePackageWithZDBMainForm.CompressAsButtonClick(Sender: TObject);
var
  m64, C64: TMemoryStream64;
  fn      : string;
begin
  if not SaveAsCompressedDialog.Execute then
      Exit;

  fn := SaveAsCompressedDialog.FileName;

  m64 := TMemoryStream64.Create;
  C64 := TMemoryStream64.Create;
  try
    FDBEng.SaveToStream(m64);
    m64.Position := 0;
    MD5Edit.Text := umlStreamMD5String(m64);

    m64.Position := 0;
    MaxCompressStream(m64, C64);

    C64.SaveToFile(fn);

    DoStatus('save as Compressed DB:%s (source:%s compressed:%s)', [fn, umlSizeToStr(m64.Size).Text, umlSizeToStr(C64.Size).Text]);
  finally
      DisposeObject([m64, C64]);
  end;
end;

procedure TFilePackageWithZDBMainForm.DoStatusNear(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
end;

procedure TFilePackageWithZDBMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFilePackageWithZDBMainForm.NewButtonClick(Sender: TObject);
begin
  FTotalRead := 0;
  FTotalWrite := 0;
  FDBManFrame.ResourceData := nil;
  DisposeObject(FDBEng);
  FDBEng := TObjectDataManagerOfCache.CreateAsStream(TMemoryStream64OfReadWriteTrigger.Create(Self), '', ObjectDataMarshal.ID, False, True, True);
  FDBManFrame.ResourceData := FDBEng;
  FOpenFile := '';

  FDBEng.Update;
  FDBEng.StreamEngine.Position := 0;
  MD5Edit.Text := umlStreamMD5String(FDBEng.StreamEngine);
  DoStatus('new DB');
end;

procedure TFilePackageWithZDBMainForm.OpenButtonClick(Sender: TObject);
var
  m64, C64: TMemoryStream64;
begin
  if not OpenDialog.Execute then
      Exit;

  FOpenFile := OpenDialog.FileName;

  FDBManFrame.ResourceData := nil;

  DisposeObject(FDBEng);

  if umlMultipleMatch(True, '*.OXC', FOpenFile) then
    begin
      C64 := TMemoryStream64.Create;
      m64 := TMemoryStream64OfReadWriteTrigger.Create(Self);
      try
        C64.LoadFromFile(FOpenFile);
        C64.Position := 0;
        DecompressStream(C64, m64);
        m64.Position := 0;
        FOpenFile := umlChangeFileExt(FOpenFile, '.OX').Text;
      except
        DisposeObject(C64);
        C64 := nil;
        m64.Clear;
        m64.LoadFromFile(FOpenFile);
        m64.Position := 0;
      end;
      DisposeObject(C64);
    end
  else
    begin
      m64 := TMemoryStream64OfReadWriteTrigger.Create(Self);
      m64.LoadFromFile(FOpenFile);
      m64.Position := 0;
    end;

  MD5Edit.Text := umlStreamMD5String(m64);
  FTotalRead := 0;
  FTotalWrite := 0;

  m64.Position := 0;
  FDBEng := TObjectDataManagerOfCache.CreateAsStream(m64, '', ObjectDataMarshal.ID, False, False, True);
  FDBManFrame.ResourceData := FDBEng;

  DoStatus('open DB:%s', [FOpenFile.Text]);
end;

procedure TFilePackageWithZDBMainForm.RecalcMD5ButtonClick(Sender: TObject);
begin
  MD5Edit.Text := umlStreamMD5String(FDBEng.StreamEngine);
  DoStatus('recalc md5:%s', [MD5Edit.Text]);
end;

procedure TFilePackageWithZDBMainForm.SaveButtonClick(Sender: TObject);
var
  stream: TFileStream;
begin
  if FOpenFile = '' then
    if not SaveDialog.Execute then
        Exit;

  if FOpenFile = '' then
    begin
      FOpenFile := SaveDialog.FileName;
    end;

  stream := TFileStream.Create(FOpenFile, fmCreate);
  try
    FDBEng.SaveToStream(stream);

    stream.Position := 0;
    MD5Edit.Text := umlStreamMD5String(stream);

  finally
      DisposeObject(stream);
  end;
  DoStatus('save DB:%s', [FOpenFile.Text]);
end;

procedure TFilePackageWithZDBMainForm.TimerTimer(Sender: TObject);
begin
  CacheStateMemo.Text := Format('cached header:%d field:%d item:%d block:%d' + #13#10 + 'File Size:%s IO Read:%s IO Write:%s',
    [FDBEng.HeaderCache.Count, FDBEng.FieldCache.Count, FDBEng.ItemCache.Count, FDBEng.ItemBlockCache.Count,
    umlSizeToStr(FDBEng.Size).Text, umlSizeToStr(FTotalRead).Text, umlSizeToStr(FTotalWrite).Text]);
end;

procedure TFilePackageWithZDBMainForm.TriggerRead64(Count: Int64);
begin
  Inc(FTotalRead, Count);
end;

procedure TFilePackageWithZDBMainForm.TriggerWrite64(Count: Int64);
begin
  Inc(FTotalWrite, Count);
end;

procedure TFilePackageWithZDBMainForm.SaveAsButtonClick(Sender: TObject);
var
  stream: TFileStream;
begin
  if not SaveDialog.Execute then
      Exit;

  FOpenFile := SaveDialog.FileName;

  stream := TFileStream.Create(FOpenFile, fmCreate);
  try
    FDBEng.SaveToStream(stream);

    stream.Position := 0;
    MD5Edit.Text := umlStreamMD5String(stream);
  finally
      DisposeObject(stream);
  end;
  DoStatus('save DB:%s', [FOpenFile.Text]);
end;

end. 
