unit FilePackageWithZDBMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  ObjectDataManagerFrameUnit, MemoryStream64, ObjectDataHashField, ObjectDataManager,
  UnicodeMixedLib, CoreClasses, DoStatusIO, PascalStrings, FileIndexPackage;

type
  TFilePackageWithZDBMainForm = class(TForm, IMemoryStream64ReadWriteTrigger)
    TopPanel: TPanel;
    NewButton: TButton;
    OpenButton: TButton;
    SaveButton: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SaveAsButton: TButton;
    Memo: TMemo;
    MD5Edit: TMemo;
    CacheStateMemo: TMemo;
    Timer: TTimer;
    RecalcMD5Button: TButton;
    CompressAsButton: TButton;
    Bevel3: TBevel;
    SaveAsCompressedDialog: TSaveDialog;
    Splitter1: TSplitter;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel7: TBevel;
    BuildIndexPackageButton: TButton;
    Bevel8: TBevel;
    NewCustomButton: TButton;
    ParallelCompressAsButton: TButton;
    SaveAsParallelCompressedDialog: TSaveDialog;
    procedure BuildIndexPackageButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NewButtonClick(Sender: TObject);
    procedure NewCustomButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure RecalcMD5ButtonClick(Sender: TObject);
    procedure CompressAsButtonClick(Sender: TObject);
    procedure ParallelCompressAsButtonClick(Sender: TObject);
  private
    FDBEng: TObjectDataManager;
    FDBManFrame: TObjectDataManagerFrame;

    FTotalRead, FTotalWrite: Int64;

    FOpenFile: U_String;
    procedure DoStatusNear(AText: SystemString; const ID: Integer);
    procedure TriggerWrite64(Count: Int64);
    procedure TriggerRead64(Count: Int64);
  public
    procedure OpenFile(fileName: SystemString);
  end;

var
  FilePackageWithZDBMainForm: TFilePackageWithZDBMainForm;

implementation

{$R *.dfm}


uses BuildIndexPackageOptFrm, NewDBOptFrm;

procedure TFilePackageWithZDBMainForm.BuildIndexPackageButtonClick(Sender: TObject);
var
  destDB: TObjectDataManager;
begin
  if BuildIndexPackageOptForm.ShowModal <> mrOk then
      exit;
  destDB := TObjectDataManager.CreateNew(FDBEng.Handle^.FixedStringL, BuildIndexPackageOptForm.DestDBEdit.Text, DBMarshal.ID);
  destDB.OverWriteItem := False;
  BuildIndexPackage(FDBEng, destDB, ParallelCompressStream_Call, BuildIndexPackageOptForm.DataPathEdit.Text);
  if CheckIndexPackage(destDB, BuildIndexPackageOptForm.DataPathEdit.Text) then
      DoStatus('check index package: no error.');
  disposeObject(destDB);
  if messageDlg(Format('Do you want to open the "%s" file?', [BuildIndexPackageOptForm.DestDBEdit.Text]), mtInformation, [mbYes, mbNO], 0) <> mrYes then
      exit;
  OpenFile(BuildIndexPackageOptForm.DestDBEdit.Text);
end;

procedure TFilePackageWithZDBMainForm.FormCreate(Sender: TObject);
begin
  FTotalRead := 0;
  FTotalWrite := 0;
  AddDoStatusHook(Self, DoStatusNear);

  FDBEng := nil;

  FDBManFrame := TObjectDataManagerFrame.Create(Self);
  FDBManFrame.Parent := Self;
  FDBManFrame.Align := alClient;
  FDBManFrame.ResourceData := nil;
  FOpenFile := '';
  MD5Edit.Text := '';

  NewButtonClick(NewButton);
end;

procedure TFilePackageWithZDBMainForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  disposeObject(FDBManFrame);
  disposeObject(FDBEng);
end;

procedure TFilePackageWithZDBMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
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
  disposeObject(FDBEng);
  FDBEng := TObjectDataManager.CreateAsStream(TMemoryStream64OfReadWriteTrigger.Create(Self), '', ObjectDataMarshal.ID, False, True, True);
  FDBManFrame.ResourceData := FDBEng;
  FOpenFile := '';

  FDBEng.UpdateIO;
  FDBEng.StreamEngine.Position := 0;
  MD5Edit.Text := umlStreamMD5String(FDBEng.StreamEngine);
  DoStatus('new DB. [fixed string size: %d]', [FDBEng.Handle^.FixedStringL]);
end;

procedure TFilePackageWithZDBMainForm.NewCustomButtonClick(Sender: TObject);
var
  l: Integer;
begin
  if NewDBOptForm.ShowModal <> mrOk then
      exit;
  l := umlClamp(umlStrToInt(NewDBOptForm.FixedStringEdit.Text, 65), 10, $FF);

  FTotalRead := 0;
  FTotalWrite := 0;
  FDBManFrame.ResourceData := nil;
  disposeObject(FDBEng);
  FDBEng := TObjectDataManager.CreateAsStream(Byte(l), TMemoryStream64OfReadWriteTrigger.Create(Self), '', ObjectDataMarshal.ID, False, True, True);
  FDBManFrame.ResourceData := FDBEng;
  FOpenFile := '';

  FDBEng.UpdateIO;
  FDBEng.StreamEngine.Position := 0;
  MD5Edit.Text := umlStreamMD5String(FDBEng.StreamEngine);
  DoStatus('new DB. [fixed string size: %d]', [FDBEng.Handle^.FixedStringL]);
end;

procedure TFilePackageWithZDBMainForm.OpenButtonClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
      exit;

  OpenFile(OpenDialog.fileName);
end;

procedure TFilePackageWithZDBMainForm.SaveButtonClick(Sender: TObject);
var
  stream: TFileStream;
begin
  if FOpenFile = '' then
    if not SaveDialog.Execute then
        exit;

  if FOpenFile = '' then
    begin
      FOpenFile := SaveDialog.fileName;
    end;

  stream := TFileStream.Create(FOpenFile, fmCreate);
  try
    FDBEng.SaveToStream(stream);

    stream.Position := 0;
    MD5Edit.Text := umlStreamMD5String(stream);

  finally
      disposeObject(stream);
  end;
  DoStatus('save %s', [FOpenFile.Text]);

  Caption := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);
  Application.Title := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);
end;

procedure TFilePackageWithZDBMainForm.SaveAsButtonClick(Sender: TObject);
var
  stream: TFileStream;
begin
  if not SaveDialog.Execute then
      exit;

  FOpenFile := SaveDialog.fileName;

  stream := TFileStream.Create(FOpenFile, fmCreate);
  try
    FDBEng.SaveToStream(stream);

    stream.Position := 0;
    MD5Edit.Text := umlStreamMD5String(stream);
  finally
      disposeObject(stream);
  end;
  DoStatus('save %s', [FOpenFile.Text]);

  Caption := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);
  Application.Title := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);
end;

procedure TFilePackageWithZDBMainForm.TimerTimer(Sender: TObject);
begin
  CacheStateMemo.Text := Format('File Size:%s IO Read:%s IO Write:%s',
    [umlSizeToStr(FDBEng.Size).Text, umlSizeToStr(FTotalRead).Text, umlSizeToStr(FTotalWrite).Text]);
end;

procedure TFilePackageWithZDBMainForm.RecalcMD5ButtonClick(Sender: TObject);
begin
  MD5Edit.Text := umlStreamMD5String(FDBEng.StreamEngine);
  DoStatus('recalc md5:%s', [MD5Edit.Text]);
end;

procedure TFilePackageWithZDBMainForm.CompressAsButtonClick(Sender: TObject);
var
  m64, C64: TMemoryStream64;
  fn: string;
begin
  SaveAsCompressedDialog.fileName := umlChangeFileExt(FOpenFile, SaveAsCompressedDialog.DefaultExt);
  if not SaveAsCompressedDialog.Execute then
      exit;

  fn := SaveAsCompressedDialog.fileName;

  m64 := TMemoryStream64.Create;
  C64 := TMemoryStream64.Create;
  try
    FDBEng.SaveToStream(m64);
    m64.Position := 0;
    MD5Edit.Text := umlStreamMD5String(m64);

    m64.Position := 0;
    MaxCompressStream(m64, C64);

    C64.SaveToFile(fn);

    DoStatus('save as Compressed %s (source:%s compressed:%s)', [fn, umlSizeToStr(m64.Size).Text, umlSizeToStr(C64.Size).Text]);
  finally
      disposeObject([m64, C64]);
  end;
end;

procedure TFilePackageWithZDBMainForm.ParallelCompressAsButtonClick(Sender: TObject);
var
  m64, C64: TMemoryStream64;
  fn: string;
begin
  SaveAsParallelCompressedDialog.fileName := umlChangeFileExt(FOpenFile, SaveAsParallelCompressedDialog.DefaultExt);
  if not SaveAsParallelCompressedDialog.Execute then
      exit;

  fn := SaveAsParallelCompressedDialog.fileName;

  m64 := TMemoryStream64.Create;
  C64 := TMemoryStream64.Create;
  try
    FDBEng.SaveToStream(m64);
    m64.Position := 0;
    MD5Edit.Text := umlStreamMD5String(m64);

    m64.Position := 0;
    ParallelCompressStream(TSelectCompressionMethod.scmZLIB_Max, m64, C64);

    C64.SaveToFile(fn);

    DoStatus('save as Compressed %s (source:%s compressed:%s)', [fn, umlSizeToStr(m64.Size).Text, umlSizeToStr(C64.Size).Text]);
  finally
      disposeObject([m64, C64]);
  end;
end;

procedure TFilePackageWithZDBMainForm.DoStatusNear(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
end;

procedure TFilePackageWithZDBMainForm.TriggerWrite64(Count: Int64);
begin
  Inc(FTotalWrite, Count);
end;

procedure TFilePackageWithZDBMainForm.TriggerRead64(Count: Int64);
begin
  Inc(FTotalRead, Count);
end;

procedure TFilePackageWithZDBMainForm.OpenFile(fileName: SystemString);
var
  m64, C64: TMemoryStream64;
begin
  FOpenFile := fileName;

  FDBManFrame.ResourceData := nil;

  disposeObject(FDBEng);

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
        disposeObject(C64);
        C64 := nil;
        m64.Clear;
        m64.LoadFromFile(FOpenFile);
        m64.Position := 0;
      end;
      disposeObject(C64);
    end
  else if umlMultipleMatch(True, '*.OXP', FOpenFile) then
    begin
      C64 := TMemoryStream64.Create;
      m64 := TMemoryStream64OfReadWriteTrigger.Create(Self);
      try
        C64.LoadFromFile(FOpenFile);
        C64.Position := 0;
        ParallelDecompressStream(C64, m64);
        m64.Position := 0;
        FOpenFile := umlChangeFileExt(FOpenFile, '.OX').Text;
      except
        disposeObject(C64);
        C64 := nil;
        m64.Clear;
        m64.LoadFromFile(FOpenFile);
        m64.Position := 0;
      end;
      disposeObject(C64);
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
  FDBEng := TObjectDataManager.CreateAsStream(m64, '', ObjectDataMarshal.ID, False, False, True);
  FDBManFrame.ResourceData := FDBEng;

  DoStatus('open %s [fixed string size: %d]', [FOpenFile.Text, FDBEng.Handle^.FixedStringL]);

  Caption := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);
  Application.Title := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);

  BuildIndexPackageOptForm.DestDBEdit.Text := umlChangeFileExt(fileName, '') + '_index.OX';
  BuildIndexPackageOptForm.DataPathEdit.Text := umlCombinePath(umlGetFilePath(fileName), 'DataCache\');
end;

end.
