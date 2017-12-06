unit GraphsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, TeEngine, Series, TeeProcs, Chart,
  ComCtrls, StrUtils, Math, BenchmarkClassUnit;

const
  {The number of kilobytes to add to the usage scores to "smooth out" the
  influence of benchmarks that allocate very little memory. Many MMs have a
  virtualalloc granularity of 1MB or more, and sometimes it is pure luck
  whether they use one block less or one block more. The effect of this on
  benchmarks that use very little memory can be dramatic. Adding a fixed number
  to the memory usage alleviates this.}
  MemoryScoreAdder = 20000;

type

  {Record holding a result}
  TBenchmarkResult = record
    {The number of benchmarks run}
    NumBenches: integer;
    {The results}
    case Integer of
      1: (SpeedScore, MemoryScore, CombinedScore: double);
      2: (Values: array[0..2] of Double)
  end;

  {Benchmark groups}
  TBenchmarkGroupResult = record
    {The description for the group}
    GroupDescription: string;
    {The categories of benchmarks inside this group}
    Categories: TBenchmarkCategorySet;
    {If this group is restricted to a single benchmark, the benchmark class}
    SingleBenchmarkClass: TFastcodeMMBenchmarkClass;
    {The results for all MMs in this group}
    MMScores: array of TBenchmarkResult;
  end;
  TBenchmarkGroupResults = array of TBenchmarkGroupResult;

  {The graphs form}
  TfGraphs = class(TForm)
    pBottom: TPanel;
    bbClose: TBitBtn;
    cbMemoryManager: TComboBox;
    cbResults: TComboBox;
    lGraph: TLabel;
    lResults: TLabel;
    Chart: TChart;
    SaveDialog: TSaveDialog;
    bSaveResults: TBitBtn;
    Label1: TLabel;
    cbBenchmarks: TComboBox;
    cbResultType: TComboBox;
    procedure GraphOptionChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bSaveResultsClick(Sender: TObject);
  private
    FData: TStringList;
    {The names of the memory managers}
    FMMNames: TStringList;
    {The names of the benchmarks}
    FBenchmarkNames: TStringList;
    {The benchmark class for each benchmark}
    FBenchmarkClasses: array of TFastcodeMMBenchmarkClass;
    {The global benchmark weight for each benchmark}
    FBenchmarkWeights: array of double;
    {The benchmark results in [mm][benchmark] order}
    FResults: array of array of TBenchmarkResult;
    {The benchmark results, per group}
    FGroupResults: TBenchmarkGroupResults;
public
    procedure BuildGraphs;
    procedure LoadResults(const AResultFile: string);
    procedure ShowSummary(Strings: TStrings);
    procedure ExportTabbedSummary(Strings: TStrings);
  end;

var
  fGraphs: TfGraphs;

implementation

uses BenchmarkForm, IniFiles;

{$R *.dfm}

//Added for D6 - Perhaps an ifdef for delphi version is needed?
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

procedure TfGraphs.BuildGraphs;
var
  LMMInd, LBenchmarkInd, LResultInd: integer;
  LSeries: THorizBarSeries;
  LResultName: string;
  LResult: Double;
  LGroup: TBenchmarkGroupResult;
  LShowSummary: Boolean;
begin
  {Clear current results}
  Chart.Legend.Visible := True;
  Chart.SeriesList.Clear;
  {Get the benchmark group}
  LGroup := FGroupResults[cbBenchmarks.ItemIndex];
  {Show summary or detail?}
  LShowSummary := (cbResultType.ItemIndex = 0) and (LGroup.Categories <> []);
  {Step through all the memory managers}
  for LMMind := 0 to FMMNames.Count - 1 do
  begin
    {Show this MM?}
    if (cbMemoryManager.ItemIndex = 0)
      or (cbMemoryManager.Text = FMMNames[LMMind]) then
    begin
      LSeries := THorizBarSeries.Create(Chart);
      LSeries.BarStyle := bsRectangle;
      LSeries.ParentChart := Chart;
      LSeries.ShowInLegend := True;
      LSeries.Marks.Visible := False;
      LSeries.Title := FMMNames[LMMind];
      {Step through all the benchmarks}
      for LBenchmarkInd := 0 to FBenchmarkNames.Count - 1 do
      begin
        {Show this benchmark?}
          if (LShowSummary and (LBenchmarkInd = 0)) //summary is index 0
            or ((not LShowSummary) and (FBenchmarkClasses[LBenchmarkInd] = LGroup.SingleBenchmarkClass))
            or ((not LShowSummary) and (FBenchmarkClasses[LBenchmarkInd].GetCategory in LGroup.Categories)) then
        begin
          {Step through all the results}
          for LResultInd := 0 to 2 do
          begin
            {Show this result?}
            if (LResultInd = cbResults.ItemIndex) or (cbResults.ItemIndex > 2) then
            begin
              {Add the result}
              if LShowSummary then
              begin
                {Average result}
                LResultName := 'Summary: ' + LGroup.GroupDescription;
                case LResultInd of
                  0: LResult := LGroup.MMScores[LMMInd].SpeedScore;
                  1: LResult := LGroup.MMScores[LMMInd].MemoryScore;
                else
                  LResult := LGroup.MMScores[LMMInd].CombinedScore;
                end;
              end
              else
              begin
                LResultName := FBenchmarkNames[LBenchmarkInd];
                {Get benchmark result}
                LResult := FResults[LMMInd][LBenchmarkInd].Values[LResultInd];
              end;
              if cbResults.ItemIndex > 2 then
                LResultName := LResultName + ' (' + cbResults.Items[LResultInd] + ')';
              {Add the series}
              LSeries.Add(LResult, LResultName, LSeries.SeriesColor);
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TfGraphs.GraphOptionChange(Sender: TObject);
begin
  BuildGraphs;
end;

procedure TfGraphs.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfGraphs.FormCreate(Sender: TObject);
begin
  FData := TStringList.Create;
  FMMNames := TStringList.Create;
  FMMNames.Sorted := True;
  FMMNames.Duplicates := dupIgnore;
  FBenchmarkNames := TStringList.Create;
  FBenchmarkNames.Sorted := True;
  FBenchmarkNames.Duplicates := dupIgnore;
end;

procedure TfGraphs.FormDestroy(Sender: TObject);
begin
  FData.Free;
  FMMNames.Free;
  FBenchmarkNames.Free;
end;

procedure TfGraphs.LoadResults(const AResultFile: string);
var
  LCat: TBenchmarkCategory;
  LInd, LInd2, LMMInd, LBenchInd, LScore, LOldBenchIndex, LGroupInd: integer;
  LTempStr, LBenchName, LMMName: string;
  LResult: TBenchmarkResult;
  LTotalScore, LTotalSpeedScore, LTotalMemoryScore, LTotalWeight, LCombinedScore, LWeight: Double;
  LMaxPerf, LMaxSpeedPerf, LMaxMemoryPerf: Double;

  function DecodeResult(const AResultString: string;
    var AMMName, ABenchmarkName: string; var AResult: TBenchmarkResult): boolean;
  var
    LDelimPos, LDelimPos2: integer;
  begin
    Result := False;
    LDelimPos := Pos(';', LTempStr);
    if LDelimPos > 1 then
    begin
      {Get the benchmark name}
      ABenchmarkName := Copy(LTempStr, 1, LDelimPos - 1);
      if ABenchmarkName[1] = '"' then
      begin
        Delete(ABenchmarkName, 1, 1);
        Delete(ABenchmarkName, length(ABenchmarkName), 1);
      end;
      Inc(LDelimPos);
      LDelimPos2 := PosEx(';', LTempStr, LDelimPos);
      if LDelimPos2 > LDelimPos then
      begin
        {Get the mm name}
        AMMName := Copy(LTempStr, LDelimPos, LDelimPos2 - LDelimPos);
        LDelimPos := LDelimPos2 + 1;
        {Get the ticks}
        LDelimPos2 := PosEx(';', LTempStr, LDelimPos);
        if LDelimPos2 > LDelimPos then
        begin
          LScore := StrToInt(Copy(LTempStr, LDelimPos, LDelimPos2 - LDelimPos));
          if LScore > 0 then
            AResult.SpeedScore := 1 / LScore
          else
            AResult.SpeedScore := 0;
          LDelimPos := LDelimPos2 + 1;
          {Get the peak memory}
          LDelimPos2 := PosEx(';', LTempStr, LDelimPos);
          if LDelimPos2 < 1 then
            LDelimPos2 := 9999;
          if LDelimPos2 > LDelimPos then
          begin
            LScore := StrToInt(Copy(LTempStr, LDelimPos, LDelimPos2 - LDelimPos));
            if LScore > 0 then
              AResult.MemoryScore := 1 / (LScore + MemoryScoreAdder)
            else
              AResult.MemoryScore := 0;
            Result := True;
          end;
        end;
      end;
    end;
  end;

  procedure AddResultGroup(
    const AGroupDescription: string;
    ACategories: TBenchmarkCategorySet;
    ASingleBenchmarkClass: TFastcodeMMBenchmarkClass = nil);
  var
    LInd, i: integer;
  begin
    LInd := length(FGroupResults);
    SetLength(FGroupResults, LInd + 1);
    FGroupResults[LInd].GroupDescription := AGroupDescription;
    FGroupResults[LInd].Categories := ACategories;
    FGroupResults[LInd].SingleBenchmarkClass := ASingleBenchmarkClass;
    SetLength(FGroupResults[LInd].MMScores, FMMNames.Count);
    {Clear the initial results}
    for i := 0 to FMMNames.Count - 1 do
    begin
      FGroupResults[LInd].MMScores[i].NumBenches := 0;
      FGroupResults[LInd].MMScores[i].SpeedScore := 0;
      FGroupResults[LInd].MMScores[i].MemoryScore := 0;
      FGroupResults[LInd].MMScores[i].CombinedScore := 0;
    end;
  end;

  function IndexOfBenchmark(Name: String): Integer;
  begin
    for Result := 0 to fBenchmark.ListViewBenchmarks.Items.Count - 1 do
      begin
        if CompareText(fBenchmark.ListViewBenchmarks.Items[Result].Caption, Name) = 0 then
          Exit;
      end;
    Result := -1;
  end;

begin
  {Load the data file}
  if FileExists(AResultFile) then
    FData.LoadFromFile(AResultFile)
  else
    FData.Clear;
  {Get all the benchmarks and contestants}
  FMMNames.Clear;
  FBenchmarkNames.Clear;
  for LInd := 0 to FData.Count - 1 do
  begin
    LTempStr := FData[LInd];
    if DecodeResult(LTempStr, LMMName, LBenchName, LResult) then
    begin
      FMMNames.Add(LMMName);
      FBenchmarkNames.Add(LBenchName);
    end;
  end;
  {Build the list of benchmark result groups}
  SetLength(FGroupResults, 0);
  AddResultGroup('All', AllBenchmarkCategories);
  AddResultGroup('Categories: All Single Thread', [bmSingleThreadRealloc, bmSingleThreadAllocAndFree, bmSingleThreadReplay]);
  AddResultGroup('Categories: All Multithread', [bmMultiThreadRealloc, bmMultiThreadAllocAndFree, bmMultiThreadReplay]);
  AddResultGroup('Categories: All Realloc', [bmSingleThreadRealloc, bmMultiThreadRealloc]);
  AddResultGroup('Categories: All Alloc and Free', [bmSingleThreadAllocAndFree, bmMultiThreadAllocAndFree]);
  AddResultGroup('Categories: All Replays', [bmSingleThreadReplay, bmMultiThreadReplay]);
  AddResultGroup('Categories: All Memory Access Speed', [bmMemoryAccessSpeed]);
  for LCat := low(TBenchmarkCategory) to high(TBenchmarkCategory) do
  begin
    AddResultGroup('Category: ' + BenchmarkCategoryNames[LCat], [LCat]);
  end;
  for LInd := 0 to high(Benchmarks) do
  begin
    AddResultGroup('Benchmark: ' + Benchmarks[LInd].GetBenchmarkName, [], Benchmarks[LInd]);
  end;
  {Set the benchmark name combo}
  LOldBenchIndex := Max(0, cbBenchmarks.ItemIndex);
  cbBenchmarks.Items.Clear;
  for LInd := 0 to high(FGroupResults) do
  begin
    cbBenchmarks.Items.Add(FGroupResults[LInd].GroupDescription);
  end;
  {Get the class for each benchmark}
  SetLength(FBenchmarkClasses, FBenchmarkNames.Count);
  SetLength(FBenchmarkWeights, FBenchmarkNames.Count);
  for LInd := 0 to FBenchmarkNames.Count - 1 do
  begin
    LBenchInd := IndexOfBenchmark(FBenchmarkNames[LInd]);
//    LBenchInd := fBenchmark.lbBenchmarks.Items.IndexOf(FBenchmarkNames[LInd]);
    if LBenchInd >= 0 then
    begin
      FBenchmarkClasses[LInd] := Benchmarks[LBenchInd];
      FBenchmarkWeights[LInd] := GlobalBenchmarkWeights[LBenchInd];
    end
    else
    begin
      FBenchmarkClasses[LInd] := TFastcodeMMBenchmark;
      FBenchmarkWeights[LInd] := 0;
    end;
  end;
  {Add the contestants to the graphs}
  while cbMemoryManager.Items.Count > 1 do
  begin
    cbMemoryManager.Items.Delete(1);
  end;
  for LInd := 0 to FMMNames.Count - 1 do
  begin
    cbMemoryManager.Items.Add(FMMNames[LInd]);
  end;
  {Reset the results}
  Setlength(FResults, FMMNames.Count);
  for LInd := 0 to FMMNames.Count - 1 do
  begin
    Setlength(FResults[LInd], FBenchmarkNames.Count);
    for LInd2 := 0 to FBenchmarkNames.Count - 1 do
      FillChar(FResults[LInd][LInd2], SizeOf(TBenchmarkResult), 0);
  end;
  {Read the results}
  for LInd := 0 to FData.Count - 1 do
  begin
    LTempStr := FData[LInd];
    if DecodeResult(LTempStr, LMMName, LBenchName, LResult) then
    begin
      LMMInd := FMMNames.IndexOf(LMMName);
      LBenchInd := FBenchmarkNames.IndexOf(LBenchName);
      with FResults[LMMInd][LBenchInd] do
      begin
        SpeedScore := SpeedScore + LResult.SpeedScore;
        MemoryScore := MemoryScore + LResult.MemoryScore;
        Inc(NumBenches);
      end;
    end;
  end;
  {Take the average score if a benchmark was run more than once for the same MM}
  for LMMInd := 0 to FMMNames.Count - 1 do
  begin
    for LBenchInd := 0 to FBenchmarkNames.Count - 1 do
    begin
      with FResults[LMMInd][LBenchInd] do
      begin
        if NumBenches > 1 then
        begin
          SpeedScore := SpeedScore / NumBenches;
          MemoryScore := MemoryScore / NumBenches;
        end;
      end;
    end;
  end;
  {The maximum memory and speed score for each benchmark must be 100}
  for LBenchInd := 0 to FBenchmarkNames.Count - 1 do
  begin
     OutputDebugString(PChar(IntToStr(LBenchInd)));
    {Get the maximum value for the benchmark}
    LResult.SpeedScore := 0;
    LResult.MemoryScore := 0;
    for LMMInd := 0 to FMMNames.Count - 1 do
    begin
      if FResults[LMMInd][LBenchInd].NumBenches > 0 then
      begin
        LResult.SpeedScore := Max(LResult.SpeedScore, FResults[LMMInd][LBenchInd].SpeedScore);
        LResult.MemoryScore := Max(LResult.MemoryScore, FResults[LMMInd][LBenchInd].MemoryScore);
      end;
    end;
    {Divide all scores by the maximum}
    for LMMInd := 0 to FMMNames.Count - 1 do
    begin
      {Scale the results for each MM so that the best for the benchmark is 100}
      if LResult.SpeedScore = 0 then
        FResults[LMMInd][LBenchInd].SpeedScore := 0
      else
        FResults[LMMInd][LBenchInd].SpeedScore := FResults[LMMInd][LBenchInd].SpeedScore / LResult.SpeedScore * 100;
      if LResult.MemoryScore = 0 then
        FResults[LMMInd][LBenchInd].MemoryScore := 0
      else
        FResults[LMMInd][LBenchInd].MemoryScore := FResults[LMMInd][LBenchInd].MemoryScore / LResult.MemoryScore * 100;
      {Calculate the relative performance for this benchmark and MM}
      FResults[LMMInd][LBenchInd].CombinedScore :=
        FResults[LMMInd][LBenchInd].SpeedScore * FBenchmarkClasses[LBenchInd].GetSpeedWeight
        + FResults[LMMInd][LBenchInd].MemoryScore * (1 - FBenchmarkClasses[LBenchInd].GetSpeedWeight);
    end;
  end;
  {Calculate the performance of each MM for each group}
  for LGroupInd := 0 to high(FGroupResults) do
  begin
    {Reset maximums}
    LMaxPerf := 1;
    LMaxSpeedPerf := 1;
    LMaxMemoryPerf := 1;
    {Step through all the MMs}
    for LMMInd := 0 to FMMNames.Count - 1 do
    begin
      LTotalScore := 0;
      LTotalSpeedScore := 0;
      LTotalMemoryScore := 0;
      LTotalWeight := 0;
      for LBenchInd := 0 to FBenchmarkNames.Count - 1 do
      begin
        {Is the bench in the group?}
        if ((FBenchmarkClasses[LBenchInd].GetCategory in FGroupResults[LGroupInd].Categories)
            or (FBenchmarkClasses[LBenchInd] = FGroupResults[LGroupInd].SingleBenchmarkClass))
          and (FResults[LMMInd][LBenchInd].NumBenches > 0) then
        begin
          LCombinedScore := FResults[LMMInd][LBenchInd].CombinedScore;
          LWeight := FBenchmarkWeights[LBenchInd];
          LTotalScore := LTotalScore + LCombinedScore * LWeight;
          LTotalSpeedScore := LTotalSpeedScore + FResults[LMMInd][LBenchInd].SpeedScore * LWeight;
          LTotalMemoryScore := LTotalMemoryScore + FResults[LMMInd][LBenchInd].MemoryScore * LWeight;
          LTotalWeight := LTotalWeight + LWeight;
        end;
      end;
      if LTotalWeight = 0 then  // avoid potential divide by zero
        LTotalWeight := 1;      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      {Store the results}
      FGroupResults[LGroupInd].MMScores[LMMInd].CombinedScore := LTotalScore / LTotalWeight;
      FGroupResults[LGroupInd].MMScores[LMMInd].SpeedScore := LTotalSpeedScore / LTotalWeight;
      FGroupResults[LGroupInd].MMScores[LMMInd].MemoryScore := LTotalMemoryScore / LTotalWeight;
      {Keep the top-scoring value}
      LMaxPerf := Max(LMaxPerf, FGroupResults[LGroupInd].MMScores[LMMInd].CombinedScore);
      LMaxSpeedPerf := Max(LMaxSpeedPerf, FGroupResults[LGroupInd].MMScores[LMMInd].SpeedScore);
      LMaxMemoryPerf := Max(LMaxMemoryPerf, FGroupResults[LGroupInd].MMScores[LMMInd].MemoryScore);
    end;
    {Scale the scores of all MMs so that the winning MM has a score of 100}
    for LMMInd := 0 to FMMNames.Count - 1 do
    begin
     if LMaxPerf <> 0 then
      FGroupResults[LGroupInd].MMScores[LMMInd].CombinedScore := FGroupResults[LGroupInd].MMScores[LMMInd].CombinedScore / LMaxPerf * 100
     else
      raise Exception.Create('Fix this division by 0');
     if LMaxSpeedPerf <> 0 then
      FGroupResults[LGroupInd].MMScores[LMMInd].SpeedScore := FGroupResults[LGroupInd].MMScores[LMMInd].SpeedScore / LMaxSpeedPerf * 100
     else
      raise Exception.Create('Fix this division by 0');
     if LMaxMemoryPerf <> 0 then
      FGroupResults[LGroupInd].MMScores[LMMInd].MemoryScore := FGroupResults[LGroupInd].MMScores[LMMInd].MemoryScore / LMaxMemoryPerf * 100
     else
      raise Exception.Create('Fix this division by 0');
    end;
  end;
  {Select a benchmark}
  cbBenchmarks.ItemIndex := LOldBenchIndex;
  {Select all MMs}
  cbMemoryManager.ItemIndex := 0;
end;

procedure TfGraphs.bSaveResultsClick(Sender: TObject);
var
  LLines: TStringList;
  LMMInd, LBenchInd: integer;
begin
  if SaveDialog.Execute then
  begin
    LLines := TStringList.Create;
    LLines.Add('Fastcode MM Challenge Benchmark Results: ' + formatdatetime('d mmm yyyy HH:nn', now));
    LLines.Add('');
    try
      for LBenchInd := 0 to FBenchmarkNames.Count - 1 do
      begin
        LLines.Add(Format('Benchmark: %s (Global Weight = %6.2f %%)',
          [ FBenchmarkNames[LBenchInd], 100 *  FBenchmarkWeights[LBenchInd] ]));
        for LMMInd := 0 to FMMNames.Count - 1 do
        begin
          if FResults[LMMInd][LBenchInd].NumBenches > 0 then
          begin
            LLines.Add(Format('  %-11s : Speed Score =%6.1f, Memory Score =%6.1f, Combined Score =%6.1f',
              [ FMMNames[LMMInd], FResults[LMMInd][LBenchInd].SpeedScore, FResults[LMMInd][LBenchInd].MemoryScore,
                FResults[LMMInd][LBenchInd].CombinedScore]));
          end
          else
          begin
            LLines.Add(Format('  %-11s : Not Run', [ FMMNames[LMMInd] ]));
          end;
        end;
        LLines.Add('');
      end;
      ShowSummary(LLines);
      {Save it}
      LLines.SaveToFile(SaveDialog.FileName);
    finally
      LLines.Free;
    end;
  end;
end;

procedure TfGraphs.ShowSummary(Strings: TStrings);
var
  LMMInd: integer;
begin
  Strings.Add('Average Total Performance: (Scaled so that the winner = 100%)');
  for LMMInd := 0 to FMMNames.Count - 1 do
  begin
    Strings.Add(Format('  %-11s : %6.1f', [ FMMNames[LMMInd], FGroupResults[0].MMScores[LMMInd].CombinedScore]));
  end;
  Strings.Add('');
  Strings.Add('Average Speed Performance: (Scaled so that the winner = 100%)');
  for LMMInd := 0 to FMMNames.Count - 1 do
  begin
    Strings.Add(Format('  %-11s : %6.1f', [ FMMNames[LMMInd], FGroupResults[0].MMScores[LMMInd].SpeedScore]));
  end;
  Strings.Add('');
  Strings.Add('Average Memory Performance: (Scaled so that the winner = 100%)');
  for LMMInd := 0 to FMMNames.Count - 1 do
  begin
    Strings.Add(Format('  %-11s : %6.1f', [ FMMNames[LMMInd], FGroupResults[0].MMScores[LMMInd].MemoryScore ]));
  end;
end;

procedure TfGraphs.ExportTabbedSummary(Strings: TStrings);
var
  LMMInd: integer;
begin
  Strings.Add('Average Total Performance: (Scaled so that the winner = 100%)');
  for LMMInd := 0 to FMMNames.Count - 1 do
    Strings.Add(Format('%s'#9'%.1f', [ FMMNames[LMMInd], FGroupResults[0].MMScores[LMMInd].CombinedScore ]));
  Strings.Add('');
  Strings.Add('Average Speed Performance: (Scaled so that the winner = 100%)');
  for LMMInd := 0 to FMMNames.Count - 1 do
    Strings.Add(Format('%s'#9'%.1f', [ FMMNames[LMMInd], FGroupResults[0].MMScores[LMMInd].SpeedScore ]));
  Strings.Add('');
  Strings.Add('Average Memory Performance: (Scaled so that the winner = 100%)');
  for LMMInd := 0 to FMMNames.Count - 1 do
    Strings.Add(Format('%s'#9'%.1f', [ FMMNames[LMMInd], FGroupResults[0].MMScores[LMMInd].MemoryScore ]));
end;


end.
