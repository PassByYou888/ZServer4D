unit BenchmarkForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BenchmarkClassUnit, Math, CheckLst, Grids, Buttons,
  ExtCtrls, ComCtrls, Clipbrd, MMValidation, ToolWin, ImgList, Menus;

  {$I FASTCODE_MM.INC}

const
  WM_POSTPROCESSING = WM_USER + 1;

type
  {The benchmark form}
  TfBenchmark = class(TForm)
    gbBenchmarks: TGroupBox;
    bRunSelectedBenchmark: TBitBtn;
    bRunAllCheckedBenchmarks: TBitBtn;
    mBenchmarkDescription: TMemo;
    ValidateButton: TBitBtn;
    BitBtn1: TBitBtn;
    RunTimeEdit: TEdit;
    ExtraValidateButton: TBitBtn;
    bGraph: TBitBtn;
    PageControl1: TPageControl;
    TabSheetBenchmarkResults: TTabSheet;
    ListViewResults: TListView;
    TabSheetProgress: TTabSheet;
    TabSheetValidation: TTabSheet;
    MemoValidation: TMemo;
    mResults: TMemo;
    ToolBar1: TToolBar;
    ToolButtonCopyResults: TToolButton;
    TabSheetScores: TTabSheet;
    MemoScores: TMemo;
    ImageList1: TImageList;
    ToolButtonDeleteResults: TToolButton;
    btnCopySummary: TBitBtn;
    bRenameMM: TToolButton;
    ToolButton1: TToolButton;
    ListViewBenchmarks: TListView;
    TabSheetCPU: TTabSheet;
    MemoCPU: TMemo;
    PopupMenuBenchmarks: TPopupMenu;
    PopupClearAllCheckMarks: TMenuItem;
    PopupSelectAllCheckMarks: TMenuItem;
    N1: TMenuItem;
    PopupCheckAllDefaultBenchmarks: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCopySummaryClick(Sender: TObject);
    procedure ValidateButtonClick(Sender: TObject);
    procedure bRunSelectedBenchmarkClick(Sender: TObject);
    procedure bRunAllCheckedBenchmarksClick(Sender: TObject);
    procedure bGraphClick(Sender: TObject);
    procedure ToolButtonCopyResultsClick(Sender: TObject);
    procedure ToolButtonDeleteResultsClick(Sender: TObject);
    procedure ListViewResultsCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure bRenameMMClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ListViewBenchmarksSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
    procedure PopupClearAllCheckMarksClick(Sender: TObject);
    procedure PopupSelectAllCheckMarksClick(Sender: TObject);
    procedure PopupCheckAllDefaultBenchmarksClick(Sender: TObject);
  private
    FRanBenchmarkCount: Integer;
    FMMValidation: TMMValidation;
    FValidationHasBeenRun: Boolean;
    FValidationFailures: string;
    FExtraValidationHasBeenRun: Boolean;
    FExtraValidationFailures: string;
    FBenchmarkHasBeenRun: Boolean;
    FXMLResultList: TStringList;

    procedure LoadXMLResults;
    procedure SaveXMLResults;
    procedure AddValidationToLog;
    procedure UpdateValidationInLog;
    procedure AddBenchmark;
    procedure UpdateBenchmark;
    procedure SaveSummary;
    procedure ValidationProgress(const CurrentValidation, Failed: string);
    procedure WMPOSTPROCESSING(var msg: TMessage); message WM_POSTPROCESSING;

    {Runs a benchmark and returns its relative speed}
    procedure RunBenchmark(ABenchmarkClass: TFastcodeMMBenchmarkClass);

    procedure InitResultsDisplay;
    procedure AddResultsToDisplay(const BenchName, MMName, TimeRan: String;
      Ticks, Peak: Cardinal; CurrentSession: string= 'T'; InitialLoad: Boolean = False);
    procedure LoadResultsToDisplay;
    procedure SaveResults;

    procedure CalculateScores;
  public
    CSVResultsFileName: AnsiString;
  end;

var
  fBenchmark: TfBenchmark;

const
  CSV_RESULT_PREFIX   = 'MMBench';
  SUMMARY_FILE_PREFIX = 'BVSummary';
  XML_RESULT_FILENAME = 'MMChallenge.xml';

  //Column indices for the ListView
  LVCOL_BENCH     = 0;
  LVCOL_MM        = 1;
  LVCOL_TIME      = 2;
  LVCOL_TICKS     = 3;
  LVCOL_MEM       = 4;

  //ListView Subitem Indices
  LVSI_MM        = 0;
  LVSI_TIME      = 1;
  LVSI_TICKS     = 2;
  LVSI_MEM       = 3;

  //Order of columns in the Results File
  RESULTS_BENCH   = 0;
  RESULTS_MM      = 1;
  RESULTS_TICKS   = 2;
  RESULTS_MEM     = 3;
  RESULTS_TIME    = 4;

{
PassValidations indicates whether the MM passes all normal validations
FastCodeQualityLabel indicates whether the MM passes the normal AND the extra validations
If you execute a validation run and the results do not match the hardcoded values,
you'll get a message that you should change the source code.
}

var
  MemoryManager_Name  : string  = 'DelphiInternal';
  PassValidations     : boolean = True;
  FastCodeQualityLabel: Boolean = False;
  DllExtension        : string;

implementation

uses
  FragmentationTestUnit, NexusDBBenchmarkUnit, ReallocMemBenchmark,
  DownsizeTestUnit, ReplayBenchmarkUnit, WildThreadsBenchmarkUnit,
  GraphsForm, BlockSizeSpreadBenchmark, SmallDownsizeBenchmark,
  SmallUpsizeBenchmark, RawPerformanceSingleThread, RawPerformanceMultiThread,
  AddressSpaceCreepBenchmark, LargeBlockSpreadBenchmark,
  StringThreadTestUnit, ArrayUpsizeSingleThread, DoubleFPBenchmark1Unit,
  DoubleFPBenchmark2Unit, DoubleFPBenchmark3Unit, SingleFPBenchmark1Unit,
  MoveBenchmark1Unit, MoveBenchmark2Unit,
  AddressSpaceCreepBenchmarkLarge, LinkedListBenchmark, RenameMMForm,
  BenchmarkUtilities, GeneralFunctions, SystemInfoUnit, DateUtils;
  //muMessageDialogHook;

{$R *.dfm}

procedure InitVars;
begin
{$IFDEF MM_BUCKETMM}
  {Robert Houdart's BucketMM}
  MemoryManager_Name = 'BucketMM';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'BUC';
{$ENDIF}
{$IFDEF MM_BUCKETMM_ASM}
  {Robert Houdart's BucketBasmMM}
  MemoryManager_Name = 'BucketMM_Asm';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'BCA';
{$ENDIF}
{$IFDEF MM_DKCIA32MM}
  {Dennis Kjaer Christensen Slowcode challenge entry v0.12}
  MemoryManager_Name = 'DKCIA32MM';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'DKC';
{$ENDIF}
{$IFDEF MM_EWCMM}
  {Eric Carman's EWCMM}
  MemoryManager_Name = 'EWCMM';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'EWC';
{$ENDIF}
{$IFDEF MM_FASTMM2}
  {Pierre le Riche's FastMM v2.xx}
  MemoryManager_Name = 'FastMM2';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'FA2';
{$ENDIF}
{$IFDEF MM_FASTMM3}
  {Pierre le Riche's FastMM v3.xx}
  MemoryManager_Name = 'FastMM3';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'FA3';
{$ENDIF}
{$IFDEF MM_FASTMM4}
  {Pierre le Riche's FastMM v4.xx}
  MemoryManager_Name := 'FastMM4';
  PassValidations := True;
  FastCodeQualityLabel := True;
  DllExtension := 'FA4';
{$ENDIF}
{$IFDEF MM_FASTMM4_16}
  {Pierre le Riche's FastMM v4.xx}
  MemoryManager_Name = 'FastMM4_16';
  PassValidations = True;
  FastCodeQualityLabel = True;
  DllExtension = 'FA4_16';
{$ENDIF}
{$IFDEF MM_HEAPMM}
  {Vladimir Kladov's HeapMM}
  { disabled... identical to WINMEM }
  MemoryManager_Name = 'HeapMM';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'HPM';
{$ENDIF}
{$IFDEF MM_LOCALHEAP}
  {Carsten Zeumer's LocalHeapMM (Uses the windows heap)}
  { disabled... identical to WINMEM }
  MemoryManager_Name = 'LocalHeapMM';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'LHM';
{$ENDIF}
{$IFDEF MM_MULTIMM}
  {Robert Lee's HPMM}
  MemoryManager_Name = 'MultiMM';
  PassValidations = False;
  FastCodeQualityLabel = False;
  DllExtension = 'MMM';
{$ENDIF}
{$IFDEF MM_NEXUSMM}
  {NexusDB Memory Manager}
  MemoryManager_Name = 'NexusMM';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'NEX';
{$ENDIF}
{$IFDEF MM_PSDMM}
  {PSDMemoryManager v1.0}
  MemoryManager_Name = 'PSDMM';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'PSD';
{$ENDIF}
{$IFDEF MM_QMEMORY}
  {Andrew Driazgov's QMemory}
  MemoryManager_Name = 'QMemory';
  PassValidations = False;
  FastCodeQualityLabel = False;
  DllExtension = 'QMM';
{$ENDIF}
{$IFDEF MM_RECYCLERMM}
  {Eric Grange's RecyclerMM}
  MemoryManager_Name = 'RecyclerMM';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'REC';
{$ENDIF}
{$IFDEF MM_RTLMM}
  { Borland Delphi RTL Memory Manager }
  MemoryManager_Name = 'RTLMM';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'RTL';
{$ENDIF}
{$IFDEF MM_TOPMM}
  {Ivo Top's TopMM}
  MemoryManager_Name := 'TopMM';
  PassValidations := True; // warning: in its current version TopMM fails the DLL Validation
  FastCodeQualityLabel := False;
  DllExtension := 'TOP';
{$ENDIF}
{$IFDEF MM_WINMEM}
  {Mike Lischke's WinMem (Uses the windows heap)}
  MemoryManager_Name = 'WinMem';
  PassValidations = True;
  FastCodeQualityLabel = False;
  DllExtension = 'WIN';
{$ENDIF}

{$IFDEF MM_SCALEMM}
  MemoryManager_Name   := 'ScaleMem';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'SCALE';
{$ENDIF}
{$IFDEF MM_SCALEMM2}
  MemoryManager_Name   := 'ScaleMem2';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'SCALE2';
{$ENDIF}
{$IFDEF MM_SCALEMM3}
  MemoryManager_Name   := 'ScaleMem3';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'SCALE3';
{$ENDIF}
{$IFDEF MM_QMM}
  MemoryManager_Name   := 'QMM';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'QMM';
{$ENDIF}
{$IFDEF MM_SapMM}
  MemoryManager_Name   := 'SapMM';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'SapMM';
{$ENDIF}

{$IFDEF MM_MSVCRTMM}
  MemoryManager_Name   := 'MSVCRTMem';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'MSVCRT';
{$ENDIF}
{$IFDEF MM_D2010MM}
  MemoryManager_Name   := 'D2010Mem';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'D2010';
{$ENDIF}
{$IFDEF MM_NedMallocMM}
  MemoryManager_Name   := 'NedMallocMM';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'NedMalloc';
{$ENDIF}
{$IFDEF MM_TCMallocMM}
  MemoryManager_Name   := 'TCMallocMM';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'TCMalloc';
{$ENDIF}
{$IFDEF MM_JEMALLOCMM}
  MemoryManager_Name   := 'JeMallocMM';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'JeMalloc';
{$ENDIF}
{$IFDEF MM_JEMALLOCFFMM}
  MemoryManager_Name   := 'JeMallocFFMM';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'JeMallocFF';
{$ENDIF}
{$IFDEF MM_HOARDMM}
  MemoryManager_Name   := 'HoardMem';
  PassValidations      := True;
  FastCodeQualityLabel := False;
  DllExtension         := 'Hoard';
{$ENDIF}
end;


{Disables the window ghosting feature for the calling graphical user interface
 (GUI) process. Window ghosting is a Windows Manager feature that lets the user
 minimize, move, or close the main window of an application that is not
 responding. (This "feature" causes problems with form z-order and also
 modal forms not showing as modal after long periods of non-responsiveness)}
procedure DisableProcessWindowsGhosting;
var
  DisableProcessWindowsGhostingProc: procedure;
begin
  DisableProcessWindowsGhostingProc := GetProcAddress(
    GetModuleHandle('user32.dll'),
    'DisableProcessWindowsGhosting');
  if Assigned(DisableProcessWindowsGhostingProc) then
    DisableProcessWindowsGhostingProc;
end;

procedure TfBenchmark.FormCreate(Sender: TObject);

  //From FastcodeBenchmarkTool091 - Per Dennis C. Suggestion
  procedure ShowCPUInfo;
  var
   CPUID : TCPUID;
   I : Integer;
  begin
    MemoCPU.Lines.Clear;
    for I := Low(CPUID) to High(CPUID) do CPUID[I] := -1;
      if IsCPUID_Available then
        begin
          CPUID	:= GetCPUID;
          MemoCPU.Lines.Add('Processor Type:    ' + IntToStr(CPUID[1] shr 12 and 3));
          MemoCPU.Lines.Add('Family:            ' + IntToStr(CPUID[1] shr 8 and $f));
          MemoCPU.Lines.Add('Model:             ' + IntToStr(CPUID[1] shr 4 and $f));
          MemoCPU.Lines.Add('Stepping:          ' + IntToStr(CPUID[1] and $f));
          MemoCPU.Lines.Add('Name:              ' + DetectCPUType(Integer(CPUID[1] shr 8 and $f),
                                                                  Integer(CPUID[1] shr 4 and $f)));
          MemoCPU.Lines.Add('Frequency:         ' + IntToStr(GetCPUFrequencyMHz) + ' MHz');
          MemoCPU.Lines.Add('Vendor:            ' + GetCPUVendor);
        end;
  end;

var
  i: integer;
  Item: TListItem;
  LWeight: Double;
  CopiedExeFileName: string;
  CurrentFileName: string;
  t: TDateTime;
begin
  Caption := Format('%s %s - %s   %s', [Caption, GetFormattedVersion, GetMMName, GetCompilerName]);
  // ShowCPUInfo;
  MemoCPU.Lines.Clear;
  MemoCPU.Lines.Add(SystemInfoCPU);
  MemoCPU.Lines.Add('');
  MemoCPU.Lines.Add(SystemInfoWindows);

  fGraphs := TfGraphs.Create(Self);

  CSVResultsFileName := Format('%s%s_%s.csv',
    [ExtractFilePath(GetModuleName(HInstance)), CSV_RESULT_PREFIX, GetCompilerAbbr]);

  FValidationHasBeenRun := False;
  FValidationFailures := '';
  FExtraValidationHasBeenRun := False;
  FExtraValidationFailures := '';
  FBenchmarkHasBeenRun := False;
  FXMLResultList := TStringList.Create;
  LoadXMLResults;

  // make a copy of the application's Exe for later use
  //Skip copy if this is the MM specific exe.
  {
  if Pos('_' + GetMMName, GetModuleName(HInstance)) = 0 then
  begin
    CopiedExeFileName := Format('%s_%s_%s.exe',
      [ChangeFileExt(Application.ExeName, ''), GetCompilerAbbr, GetMMName]);
    CopyFile(PChar(GetModuleName(HInstance)), PChar(CopiedExeFileName), False);
  end;
  }

  {List the benchmarks}
  for i := 0 to high(Benchmarks) do
  begin
    Item := ListViewBenchmarks.Items.Add;
    Item.Data := Pointer(i);
    Item.Checked := Benchmarks[i].RunByDefault;
    Item.Caption := Benchmarks[i].GetBenchmarkName;
    LWeight := Benchmarks[i].GetSpeedWeight;
    Item.SubItems.Add(BenchmarkCategoryNames[Benchmarks[i].GetCategory]);
    Item.SubItems.Add(FormatFloat('0.## %', LWeight * 100));
    Item.SubItems.Add(FormatFloat('0.## %', 100 - LWeight * 100));
    Item.SubItems.Add(FormatFloat('0.## %', Benchmarks[i].GetBenchmarkWeight * 100));
    Item.SubItems.Add(FormatFloat('0.## %', GlobalBenchmarkWeights[i] * 100));
  end;

  if ListViewBenchmarks.Items.Count > 0 then
  begin
    //Select the first benchmark
    ListViewBenchmarks.Items[0].Selected := True;
    ListViewBenchmarks.Items[0].Focused  := True;
    //Set the benchmark description.
    ListViewBenchmarksSelectItem(nil, ListViewBenchmarks.Selected,
                                      ListViewBenchmarks.Selected <> nil);
  end;

  InitResultsDisplay;

  FMMValidation := TMMValidation.Create(Self);
  FMMValidation.OnProgress := ValidationProgress;
  MemoValidation.Lines.Clear;

  PageControl1.ActivePage := TabSheetBenchmarkResults;

  if FastCodeQualityLabel then
  begin
    ExtraValidateButton.Font.Color := clGreen;
    ExtraValidateButton.Caption := 'FastCode Quality Label';
  end
  else
  begin
    ExtraValidateButton.Font.Color := clRed;
    ExtraValidateButton.Caption := 'No FastCode Quality Label';
  end;
  if PassValidations then
  begin
    ValidateButton.Font.Color := clGreen;
    ValidateButton.Caption := 'Passes Validations';
  end
  else
  begin
    ValidateButton.Font.Color := clRed;
    ValidateButton.Caption := 'Does not Pass Validations';
  end;

  if ParamCount > 0 then
    PostMessage(Handle, WM_POSTPROCESSING, 0, 0);

  //t := Now;
  //Halt(0);
  //TStringThreadTest.CreateBenchmark.RunBenchmark;
  //MessageDlg(format('%4.3f',[DateUtils.MilliSecondSpan(now,t)]), mtWarning, [mbOK], 0);
end;

procedure TfBenchmark.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FRanBenchmarkCount > 0 then
    SaveResults;
  FreeAndNil(FXMLResultList);
end;

procedure TfBenchmark.bRunSelectedBenchmarkClick(Sender: TObject);
var
  StartTime, RunTime : TDateTime;

begin
  StartTime := Time;
  Screen.Cursor := crHourglass;
  RuntimeEdit.Text := 'Running';
  RuntimeEdit.Color := clLime;

  if ListViewBenchmarks.Selected = nil then
    Exit;

  bRunSelectedBenchmark.Caption := 'Running';
  Enabled := False;
  Application.ProcessMessages;
  Enabled := True;

  RunBenchmark(Benchmarks[Integer(ListViewBenchmarks.Selected.Data)]);
  if FRanBenchmarkCount > 0 then
    SaveResults;

  CalculateScores;
  SaveSummary;

  bRunSelectedBenchmark.Caption := 'Run Selected Benchmark';
  Enabled := False;
  Application.ProcessMessages;
  Enabled := True;

  Screen.Cursor := crDefault;
  RuntimeEdit.Color := clWindow;
  RunTime := Time - StartTime;
  RunTimeEdit.Text := 'Benchmark Time: ' + TimeToStr(RunTime);
end;

procedure TfBenchmark.RunBenchmark(ABenchmarkClass: TFastcodeMMBenchmarkClass);
var
  LBenchmark: TFastcodeMMBenchmark;
  LStartTicks, LUsedTicks: Int64;
begin
  PageControl1.ActivePage := TabSheetProgress;

  mResults.Lines.Add(FormatDateTime('HH:nn:ss', time) + ' Running Benchmark: ' + ABenchmarkClass.GetBenchmarkName + '...');
  {Create the benchmark}
  LBenchmark := ABenchmarkClass.CreateBenchmark;
  try
    if LBenchmark.CanRunBenchmark then
    begin
      {Do the getmem test}
      LStartTicks := GetCPUTicks;
      LBenchmark.RunBenchmark;
      LUsedTicks := GetCPUTicks - LStartTicks;
      {Add a line}

      mResults.Lines[mResults.Lines.Count - 1] := FormatDateTime('HH:nn:ss', time) + ' '
        + Format('%-45s  MTicks = %6d    Mem = %7d',
        [ ABenchmarkClass.GetBenchmarkName, LUsedTicks shr 20, LBenchmark.PeakAddressSpaceUsage ]);
      Enabled := False;
      Application.ProcessMessages;
      Enabled := True;

      AddResultsToDisplay(ABenchmarkClass.GetBenchmarkName,
                          MemoryManager_Name,
                          FormatDateTime('YYYYMMDD HH:NN:SS.ZZZ', Now),
                          LUsedTicks shr 20,
                          LBenchmark.PeakAddressSpaceUsage);
      if not FBenchmarkHasBeenRun then
      begin
        AddBenchmark;
        FBenchmarkHasBeenRun := True;
      end;
      UpdateBenchmark;
    end
    else
      begin
        mResults.Lines[mResults.Lines.Count - 1] := ABenchmarkClass.GetBenchmarkName +
          ': Skipped';
        Enabled := False;
        Application.ProcessMessages;
        Enabled := True;
      end;
  finally
    {Free the benchmark}
    LBenchmark.Free;
  end;
end;

procedure TfBenchmark.bRunAllCheckedBenchmarksClick(Sender: TObject);
var
 i: integer;
 StartTime, RunTime : TDateTime;

begin
  StartTime := Time;
  Screen.Cursor := crHourglass;
  RuntimeEdit.Text := 'Running';
  RuntimeEdit.Color := clLime;
  bRunAllCheckedBenchmarks.Caption := 'Running';
  Enabled := False;
  Application.ProcessMessages;
  Enabled := True;
  mResults.Lines.Add('***Running All Checked Benchmarks***');
  for i := 0 to high(BenchMarks) do
  begin
    {Must this benchmark be run?}
    if ListViewBenchmarks.Items[i].Checked then
    begin
      {Show progress in checkboxlist}
      ListViewBenchmarks.Items[i].Selected := True;
      ListViewBenchmarks.Items[i].Focused  := True;
      ListViewBenchmarks.Selected.MakeVisible(False);
      ListViewBenchmarksSelectItem(nil, ListViewBenchmarks.Selected,
                                        ListViewBenchmarks.Selected <> nil);
      Enabled := False;
      Application.ProcessMessages;
      Enabled := True;
      {Run the benchmark}
      RunBenchmark(BenchMarks[i]);
      {Wait one second}
      Sleep(1000);
    end;
  end;
  mResults.Lines.Add('***All Checked Benchmarks Done***');
  bRunAllCheckedBenchmarks.Caption := 'Run All Checked Benchmarks';
  if FRanBenchmarkCount > 0 then
    SaveResults;
  CalculateScores;
  SaveSummary;
  Screen.Cursor := crDefault;
  RuntimeEdit.Color := clWindow;
  RunTime := Time - StartTime;
  RunTimeEdit.Text := 'Benchmark Time: ' + TimeToStr(RunTime);
end;

procedure TfBenchmark.ValidateButtonClick(Sender: TObject);
var
 StartTime, RunTime : TDateTime;
 FailedValidation, OriginalCaption: string;

begin
 StartTime := Time;
 Screen.Cursor := crHourglass;
 try
   OriginalCaption := (Sender as TBitBtn).Caption;
   TBitBtn(Sender).Caption := 'Running';
   RuntimeEdit.Text := 'Running';
   RuntimeEdit.Color := clLime;
   MemoValidation.Color := clLime;
   MemoValidation.Lines.Clear;
   PageControl1.ActivePage := TabSheetValidation;
    Enabled := False;
    Application.ProcessMessages;
    Enabled := True;
   // execute Validation
   if not (FValidationHasBeenRun or FExtraValidationHasBeenRun)then
     AddValidationToLog;
   if Sender = ExtraValidateButton then
   begin
     FailedValidation := FMMValidation.ExtraValidate;
     FExtraValidationFailures := FailedValidation;
     FExtraValidationHasBeenRun := True;
     // Application.ProcessMessages;
     UpdateValidationInLog;
   end
   else
   begin
     FailedValidation := FMMValidation.Validate;
     FValidationFailures := FailedValidation;
     FValidationHasBeenRun := True;
     // Application.ProcessMessages;
     UpdateValidationInLog;
   end;

   // show result
   if FailedValidation = '' then
   begin
     MemoValidation.Color := clGreen;
   end
   else
   begin
     MemoValidation.Color := clRed;
     MemoValidation.Lines.Add('Failed Validations: ' + FailedValidation);
   end;
   RunTime := Time - StartTime;
   RunTimeEdit.Text := 'Validation Time: ' + TimeToStr(RunTime);

   // check FastCode Quality Label
//   if Sender = ExtraValidateButton then begin
//     if FastCodeQualityLabel xor (PassValidations and (FailedValidation = '')) then
//       ShowMessage(Format('Constant FastCodeQualityLabel for "%s" should be updated in the source code !',
//         [MemoryManager_Name]));
//   end
//   else begin
//     if PassValidations xor (FailedValidation = '') then
//       ShowMessage(Format('Constant PassValidations for "%s" should be updated in the source code !',
//         [MemoryManager_Name]));
//   end;
 finally
   Screen.Cursor := crDefault;
   RuntimeEdit.Color := clWindow;
   (Sender as TBitBtn).Caption := OriginalCaption;
 end;
end;

procedure TfBenchmark.bGraphClick(Sender: TObject);
begin
  fGraphs.LoadResults(CSVResultsFileName);
  fGraphs.BuildGraphs;
  fGraphs.Show;
end;

procedure TfBenchmark.ValidationProgress(const CurrentValidation, Failed: string);
begin
 if Failed = '' then
 begin
   MemoValidation.Color := clGreen;
   RunTimeEdit.Text := Format('Running %s...', [CurrentValidation]);
   MemoValidation.Lines.Add(Format('Running %s...', [CurrentValidation]))
 end
 else
 begin
   MemoValidation.Color := clRed;
   RunTimeEdit.Text := Format('Running %s... - Failed: %s', [CurrentValidation, Failed]);
   MemoValidation.Lines.Add(Format('Running %s... - Failed: %s', [CurrentValidation, Failed]));
 end;
 Enabled := False;
 Application.ProcessMessages;
 Enabled := True;
 Sleep(250);  // slow down a bit so you can see what's happening
end;

procedure TfBenchmark.InitResultsDisplay;
begin
  ListViewResults.Items.BeginUpdate;
  try
    ListViewResults.Items.Clear;
  finally
    ListViewResults.Items.EndUpdate;
  end;

  FRanBenchmarkCount := 0;
  LoadResultsToDisplay;

  ListViewResults.Column[LVCOL_BENCH].Width := -2;
  ListViewResults.Column[LVCOL_MM].Width    := -2;
  ListViewResults.Column[LVCOL_TIME].Width  := -2;
  ListViewResults.Column[LVCOL_TICKS].Width := -2;
  ListViewResults.Column[LVCOL_MEM].Width   := 120;

  CalculateScores;
end;

procedure TfBenchmark.AddResultsToDisplay(const BenchName, MMName, TimeRan: String;
  Ticks, Peak: Cardinal; CurrentSession: string = 'T'; InitialLoad: Boolean = False);
var
  Item: TListItem;
begin
  Inc(FRanBenchmarkCount);

  Item := ListViewResults.Items.Add;
  Item.Caption := BenchName;
  Item.SubItems.Add(MMName);
  Item.SubItems.Add(TimeRan);
  Item.SubItems.Add(IntToStr(Ticks));
  Item.SubItems.Add(IntToStr(Peak));
  Item.SubItems.Add(CurrentSession);

//  if not InitialLoad then
//    ListViewResults.AlphaSort;
end;

procedure TfBenchmark.LoadResultsToDisplay;
var
  CSV, Bench: TStringList;
  l: Integer;
  BenchName, MMName, TimeRan: string;
  Ticks, Peak: Integer;
begin
  if not FileExists(CSVResultsFileName) then
    Exit;

  CSV := TStringList.Create;
  Bench := TStringList.Create;
  try
    Bench.Delimiter := ';';

    CSV.LoadFromFile(CSVResultsFileName);

    ListViewResults.Items.BeginUpdate;
    try
      for l := 0 to CSV.Count - 1 do
      begin
        Bench.DelimitedText   := CSV[l];
        if Bench.Count < 4 then
          Continue;

        BenchName := Bench[RESULTS_BENCH];
        if Trim(BenchName) = '' then
          Continue;

        MMName    := Bench[RESULTS_MM];
        if Bench.Count > RESULTS_TIME then  // Available from B&V 0.25 on
          TimeRan   := Bench[RESULTS_TIME]
        else
          TimeRan   := '';
        Ticks     := StrToInt(Bench[RESULTS_TICKS]);
        Peak      := StrToInt(Bench[RESULTS_MEM]);

        AddResultsToDisplay(BenchName, MMName, TimeRan, Ticks, Peak, 'F');
      end;
    finally
      ListViewResults.Items.EndUpdate;
    end;

    // ListViewResults.AlphaSort;

    if ListViewResults.Items.Count > 0 then
    begin
      ListViewResults.Items[0].Selected := True;
      ListViewResults.Items[0].Focused  := True;
    end;
  finally
    Bench.Free;
    CSV.Free;
  end;
end;

procedure TfBenchmark.SaveResults;
var
  CSV, Bench: TStringList;
  i:          Integer;
begin
  CSV := TStringList.Create;

  Bench := TStringList.Create;
  try
    Bench.Delimiter := ';';

    for i := 0 to ListViewResults.Items.Count - 1 do
      begin
        Bench.Clear;

        Bench.Add(ListViewResults.Items[i].Caption);
        Bench.Add(ListViewResults.Items[i].SubItems[LVSI_MM]);
        Bench.Add(ListViewResults.Items[i].SubItems[LVSI_TICKS]);
        Bench.Add(ListViewResults.Items[i].SubItems[LVSI_MEM]);
        Bench.Add(ListViewResults.Items[i].SubItems[LVSI_TIME]);

        CSV.Add(Bench.DelimitedText);
      end;

    CSV.SaveToFile(CSVResultsFileName);
  finally
    Bench.Free;
    CSV.Free;
  end;
end;

procedure TfBenchmark.ToolButtonCopyResultsClick(Sender: TObject);
var
  iRow: Integer;
  iCol: Integer;
  StringList: TStringList;
  s: string;
  Item: TListItem;
begin
  //The tab-delimited data dropped to the clipboard can be pasted into
  // Excel and will generally auto-separate itself into columns.
  StringList := TStringList.Create;
  try
    //Header
    s := '';
    for iCol := 0 to ListViewResults.Columns.Count - 1 do
      s := s + #9 + ListViewResults.Column[iCol].Caption;
    Delete(s, 1, 1); // delete initial #9 character
    StringList.Add(s);

    //Body
    for iRow := 0 to ListViewResults.Items.Count - 1 do begin
      Item := ListViewResults.Items[iRow];
      s := Item.Caption;
      for iCol := 0 to Item.SubItems.Count - 1 do
        s := s + #9 + Item.SubItems[iCol];
      StringList.Add(s);
    end;

    Clipboard.AsText := StringList.Text;
  finally
    StringList.Free;
  end;
end;

procedure TfBenchmark.CalculateScores;
begin
  if not FileExists(CSVResultsFileName) then
    begin
      MemoScores.Lines.Clear;
      Exit;
    end;

  fGraphs.LoadResults(CSVResultsFileName);

  {load score in memo}
  MemoScores.Lines.BeginUpdate;
  try
    MemoScores.Lines.Clear;
    fGraphs.ShowSummary(MemoScores.Lines);
  finally
    MemoScores.Lines.EndUpdate;
  end;
end;

procedure TfBenchmark.ToolButtonDeleteResultsClick(Sender: TObject);
begin
//  if (Application.MessageBox('Are you sure you want to delete all results?',
//    'Confirm Results Clear', MB_ICONQUESTION or MB_YesNo or MB_DefButton2) = mrYes) then
  begin
    ListViewResults.Items.BeginUpdate;
    try
      ListViewResults.Items.Clear;
    finally
      ListViewResults.Items.EndUpdate;
    end;
    DeleteFile(CSVResultsFileName);
    FRanBenchmarkCount := 0;
    CalculateScores;
  end;
end;

procedure TfBenchmark.ListViewResultsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  //Sort list by Benchmark Name, MM Name, Time Ran
  Compare := AnsiCompareText(Item1.Caption, Item2.Caption);
  if Compare = 0 then
    Compare := AnsiCompareText(Item1.SubItems[0], Item2.SubItems[0]);
  if Compare = 0 then
    Compare := AnsiCompareText(Item1.SubItems[1], Item2.SubItems[1]);
end;

procedure TfBenchmark.btnCopySummaryClick(Sender: TObject);
var
  StringList: TStringList;
begin
  if not FileExists(CSVResultsFileName) then
    Exit;
  fGraphs.LoadResults(CSVResultsFileName);
  StringList := TStringList.Create;
  try
    fGraphs.ExportTabbedSummary(StringList);
    Clipboard.AsText := StringList.Text;
  finally
    StringList.Free;
  end;
end;

procedure TfBenchmark.bRenameMMClick(Sender: TObject);
var
  LInd: integer;
  LOldName, LNewName: String;
begin
  if ListViewResults.ItemIndex >= 0 then
  begin
    Application.CreateForm(TfRenameMM, fRenameMM);
    try
      LOldName := ListViewResults.Items[ListViewResults.ItemIndex].SubItems[0];
      fRenameMM.eMMName.Text := LOldName;
      if (fRenameMM.ShowModal = mrOK) and (fRenameMM.eMMName.Text <> '') then
      begin
        LNewName := fRenameMM.eMMName.Text;
        for LInd := 0 to ListViewResults.Items.Count - 1 do
        begin
          if ListViewResults.Items[LInd].SubItems[0] = LOldName then
            ListViewResults.Items[LInd].SubItems[0] := LNewName;
        end;
        SaveResults;
        CalculateScores;
      end;
    finally
      FreeAndNil(fRenameMM);
    end;
  end;
end;

procedure TfBenchmark.ToolButton1Click(Sender: TObject);
var
  LMMName: string;
  LInd: integer;
begin
  if ListViewResults.ItemIndex < 0 then
    exit;
  LMMName := ListViewResults.Items[ListViewResults.ItemIndex].SubItems[0];
//  if (Application.MessageBox(PChar('Are you sure you want to delete results for ' + LMMName + '?'),
//    'Confirm Results Delete', MB_ICONQUESTION or MB_YesNo or MB_DefButton2) = mrYes) then
  begin
    for LInd := ListViewResults.Items.Count - 1 downto 0 do
    begin
      if ListViewResults.Items[LInd].SubItems[0] = LMMName then
      begin
        ListViewResults.Items[LInd].Delete;
        Dec(FRanBenchmarkCount);
      end;
    end;
    SaveResults;
    CalculateScores;
  end;
end;

procedure TfBenchmark.ListViewBenchmarksSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  //Set the benchmark description
  if (Item <> nil) and Selected then
    mBenchmarkDescription.Text :=
      Benchmarks[Integer(Item.Data)].GetBenchmarkDescription;
end;

procedure TfBenchmark.PopupClearAllCheckMarksClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListViewBenchmarks.Items.Count - 1 do
    ListViewBenchmarks.Items[i].Checked := False;
end;

procedure TfBenchmark.PopupSelectAllCheckMarksClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListViewBenchmarks.Items.Count - 1 do
    ListViewBenchmarks.Items[i].Checked := True;
end;

procedure TfBenchmark.PopupCheckAllDefaultBenchmarksClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListViewBenchmarks.Items.Count - 1 do
    ListViewBenchmarks.Items[i].Checked := Benchmarks[i].RunByDefault;
end;

// ----------------------------------------------------------------------------
procedure TfBenchmark.LoadXMLResults;
var
  InsertionPoint: Integer;
begin
  if FileExists(XML_RESULT_FILENAME) then
  begin
    FXMLResultList.LoadFromFile(XML_RESULT_FILENAME);

    InsertionPoint := FXMLResultList.IndexOf('</mmbench>');
    if InsertionPoint = -1 then
    begin
      InsertionPoint := FXMLResultList.Count - 1;
      FXMLResultList.Insert(InsertionPoint, '<mmbench>');
      FXMLResultList.Insert(InsertionPoint+1, '</mmbench>');
    end;
  end
  else
  begin
    FXMLResultList.Add('<mmchallenge>');
    FXMLResultList.Add('<mmbench>');
    FXMLResultList.Add('</mmbench>');
    FXMLResultList.Add('</mmchallenge>');
  end;
end;

// ----------------------------------------------------------------------------
procedure TfBenchmark.SaveXMLResults;
begin
  FXMLResultList.SaveToFile(XML_RESULT_FILENAME);
end;

// ----------------------------------------------------------------------------
procedure TfBenchmark.AddValidationToLog;
var
  InsertionPoint: Integer;
  BenchmarksIndex: Integer;
begin
  InsertionPoint := FXMLResultList.IndexOf('</validations>');
  if InsertionPoint = -1 then
  begin
    BenchmarksIndex := FXMLResultList.IndexOf('<benchmarks>');
    if BenchmarksIndex > -1 then
      InsertionPoint := BenchmarksIndex
    else
      InsertionPoint := FXMLResultList.Count - 1;
    FXMLResultList.Insert(InsertionPoint-1, '<validations>');
    FXMLResultList.Insert(InsertionPoint, '</validations>');
  end;

  FXMLResultList.Insert(InsertionPoint,
    Format('<validation version="%s" compiler="%s" MM="%s">', [GetFormattedVersion, GetCompilerAbbr, GetMMName]));
  // FXMLResultList.Insert(InsertionPoint, '<validation compiler="' + GetCompilerName + '" MM="' + GetMMName + '" >');
  FXMLResultList.Insert(InsertionPoint+1, Format('<cpu>%s</cpu>', [SystemInfoCPU]));
  FXMLResultList.Insert(InsertionPoint+2, Format('<os>%s</os>', [SystemInfoWindows]));
  FXMLResultList.Insert(InsertionPoint+3, '<result> </result>');
  FXMLResultList.Insert(InsertionPoint+4, '<extraresult> </extraresult>');
  FXMLResultList.Insert(InsertionPoint+5, '</validation>');
end;

// ----------------------------------------------------------------------------
procedure TfBenchmark.UpdateValidationInLog;
var
  s: string;
  ResultIndex: Integer;
begin
  s := '';
  ResultIndex := FXMLResultList.IndexOf('</validations>') - 3;

  if FValidationHasBeenRun then
  begin
    if FValidationFailures = '' then
      s := 'PASS'
    else
      s := 'FAILED: ' + FValidationFailures;
    FXMLResultList[ResultIndex] := Format('<result>%s</result>', [s]);
  end;

  if FExtraValidationHasBeenRun then
  begin
    s := FExtraValidationFailures;
    if FExtraValidationFailures = '' then
      s := 'PASS'
    else
      s := 'FAILED: ' + FExtraValidationFailures;
    FXMLResultList[ResultIndex+1] := Format('<extraresult>%s</extraresult>', [s]);
  end;

  SaveXMLResults;
end;

// ----------------------------------------------------------------------------
procedure TfBenchmark.AddBenchmark;
var
  InsertionPoint: Integer;
begin
  InsertionPoint := FXMLResultList.IndexOf('</benchmarks>');
  if InsertionPoint = -1 then
  begin
    InsertionPoint := FXMLResultList.Count - 1;
    FXMLResultList.Insert(InsertionPoint-1, '<benchmarks>');
    FXMLResultList.Insert(InsertionPoint, '</benchmarks>');
  end;

  FXMLResultList.Insert(InsertionPoint,
    Format('<benchmark version="%s" compiler="%s" MM="%s">', [GetFormattedVersion, GetCompilerAbbr, GetMMName]));
  // FXMLResultList.Insert(InsertionPoint, '<benchmark compiler="' + GetCompilerName + '" MM="' + GetMMName + '" >');
  FXMLResultList.Insert(InsertionPoint+1, Format('<cpu>%s</cpu>', [SystemInfoCPU]));
  FXMLResultList.Insert(InsertionPoint+2, Format('<os>%s</os>', [SystemInfoWindows]));
  FXMLResultList.Insert(InsertionPoint+3, '<result> </result>');
  FXMLResultList.Insert(InsertionPoint+4, '</benchmark>');
end;

// ----------------------------------------------------------------------------
procedure TfBenchmark.UpdateBenchmark;
var
  i: Integer;
  s: string;
  Item: TListItem;
  ResultIndex: Integer;
begin
  ResultIndex := FXMLResultList.IndexOf('</benchmarks>')-1;

  while SameText(Copy(FXMLResultList[ResultIndex-1], 1, 7), '<result') do
  begin
    FXMLResultList.Delete(ResultIndex-1);
    Dec(ResultIndex);
  end;

  for i := ListViewResults.Items.Count -1 downto 0 do
  begin
    Item := ListViewResults.Items[i];
    if SameText('T', Item.SubItems[4]) then
    begin
      s := Format('<result name="%s" time="%s" mticks="%s" mem="%s" />',
        [Item.Caption, Item.SubItems[1], Item.SubItems[2], Item.SubItems[3]]);
      FXMLResultList.Insert(ResultIndex, s);
    end;
  end;
  SaveXMLResults;
end;

// ----------------------------------------------------------------------------
procedure TfBenchmark.WMPOSTPROCESSING(var msg: TMessage);
begin
  if FindCmdLineSwitch('C', ['-', '/'], True) then
  begin
    // clear CSV results;
    ToolButtonDeleteResults.Click;
  end;

  if FindCmdLineSwitch('B', ['-', '/'], True) then
  begin
    // run all benchmarks
    Show;
    Enabled := False;
    Application.ProcessMessages;
    Enabled := True;
    bRunAllCheckedBenchmarks.Click;
    Halt;
  end;

  if FindCmdLineSwitch('V', ['-', '/'], True) then
  begin
    // perform all validations
    Show;
    Enabled := False;
    Application.ProcessMessages;
    Enabled := True;
    ValidateButton.Click;
    ExtraValidateButton.Click;
    Halt;
  end;

  if FindCmdLineSwitch('EV', ['-', '/'], True) then
  begin
    // perform only extra validations
    Show;
    Enabled := False;
    Application.ProcessMessages;
    Enabled := True;
    ExtraValidateButton.Click;
    Halt;
  end;
end;

// ----------------------------------------------------------------------------
procedure TfBenchmark.SaveSummary;
var
  FileName: string;
  F: TextFile;
  i: Integer;
begin
  FileName := Format('%s%s_%s.txt',
    [ExtractFilePath(GetModuleName(HInstance)), SUMMARY_FILE_PREFIX, GetCompilerAbbr]);

  if FileExists(FileName) then
    DeleteFile(FileName);

  AssignFile(F, FileName);
  Rewrite(F);
  try
    Writeln(F, 'Summary report for Memory Manager challenge ' +
      GetFormattedVersion + FormatDateTime('  yyyy-mmm-dd hh:nn:ss', NOW));
    Writeln(F, '');
    Writeln(F, 'Compiler: ' + GetCompilerName);
    Writeln(F, SystemInfoCPU);
    Writeln(F, SystemInfoWindows);
    Writeln(F, '');
    Writeln(F, '');

    for i := 0 to MemoScores.Lines.Count - 1 do
      Writeln(F, MemoScores.Lines[i]);
  finally
    CloseFile(F);
  end;
end;

initialization
  InitVars;

  {We want the main form repainted while it's busy running}
  DisableProcessWindowsGhosting;

  //muMessageDialogHook.LoadHook;

end.
