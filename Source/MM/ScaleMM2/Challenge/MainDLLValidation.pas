unit MainDLLValidation;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls;

const
  WM_POSTPROCESSING = WM_USER + 1;

type
  TForm1 = class(TForm)
    btnValidate: TButton;
    ListView1: TListView;
    StatusBar1: TStatusBar;
    btnValidateAll: TButton;

    procedure btnValidateAllClick(Sender: TObject);
    procedure btnValidateSelectedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FResultList: TStringList;

    procedure FindDLLs;
    function GetMMFromFileName(const AFileName: string): string;
    function GetStatus(const AStatusString: string): string;
    procedure LoadResults;
    procedure SaveResults;
    procedure StatusText(const Msg: string);
    procedure ValidateAll;
    procedure ValidateCurrent;
    function ValidateDLL(const AFileName: string): string;
    procedure WMPOSTPROCESSING(var msg: TMessage); message WM_POSTPROCESSING;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  BenchmarkUtilities, GeneralFunctions, SystemInfoUnit;

{$R *.DFM}

type
  TExportedMethod = function: Boolean;

const
  DLL_BASENAME = 'MMUsageDll';
  DLL_FUNCTIONNAME = 'UseSomeMemory';
  RESULT_FILENAME = 'MMChallenge.xml';

// TForm1
// ============================================================================
procedure TForm1.btnValidateAllClick(Sender: TObject);
begin
  ValidateAll;
end;

// ----------------------------------------------------------------------------
procedure TForm1.btnValidateSelectedClick(Sender: TObject);
begin
  ValidateCurrent;
end;

// ----------------------------------------------------------------------------
procedure TForm1.FindDLLs;
var
  SearchPattern: string;
  FilePath: string;
  sr: TSearchRec;
begin
  SearchPattern := Format('%s_%s_*', [DLL_BASENAME, GetCompilerAbbr]);
  FilePath := ExtractFilePath(ParamStr(0));
  ListView1.Items.Clear;

  if FindFirst(SearchPattern, (faReadOnly + faHidden + faAnyFile), sr) = 0 then
  begin
    try
      repeat
        with ListView1.Items.Add do begin
          Caption := sr.Name;
          SubItems.Add('');
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
      ListView1.AlphaSort;
    end;
  end; // if
end;

// ----------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  PostMessage(Handle, WM_POSTPROCESSING, 0, 0)
end;

// ----------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FResultList);
end;

// ----------------------------------------------------------------------------
function TForm1.GetMMFromFileName(const AFileName: string): string;
var
  i: Integer;
begin
  Result := ChangeFileExt(AFileName, '');
  i := pos('_', result);
  Result := Copy(Result, i+1, Length(Result));
  i := pos('_', result);
  Result := Copy(Result, i+1, Length(Result));
end;

// ----------------------------------------------------------------------------
function TForm1.GetStatus(const AStatusString: string): string;
begin
  Result := AStatusString;

  if (Result = '') or (SameText(Result, 'Validating...')) then
    Exit;

  if SameText(Result, 'OK') then
    Result := 'PASS'
  else
    Result := 'FAIL';
end;

// ----------------------------------------------------------------------------
procedure TForm1.LoadResults;
var
  InsertionPoint: Integer;
begin
  if FileExists(RESULT_FILENAME) then
  begin
    FResultList.LoadFromFile(RESULT_FILENAME);

    InsertionPoint := FResultList.IndexOf('</mmvalidatedll>');
    if InsertionPoint = -1 then
    begin
      InsertionPoint := FResultList.Count - 2;
      FResultList.Insert(InsertionPoint-1, '<mmvalidatedll>');
      FResultList.Insert(InsertionPoint, '</mmvalidatedll>');
    end;
  end
  else
  begin
    FResultList.Add('<mmchallenge>');
    FResultList.Add('<mmvalidatedll>');
    FResultList.Add('</mmvalidatedll>');
    FResultList.Add('</mmchallenge>');
    InsertionPoint := 2;
  end;
  FResultList.Insert(InsertionPoint,
    Format('<validation version="%s" compiler="%s" >', [GetFormattedVersion, GetCompilerAbbr]));
  // FResultList.Insert(InsertionPoint, '<validation compiler="' + GetCompilerName + '" >');
  FResultList.Insert(InsertionPoint+1, Format('<cpu>%s</cpu>', [SystemInfoCPU]));
  FResultList.Insert(InsertionPoint+2, Format('<os>%s</os>', [SystemInfoWindows]));
  FResultList.Insert(InsertionPoint+3, '<result />');
  FResultList.Insert(InsertionPoint+4, '</validation>');
end;

// ----------------------------------------------------------------------------
procedure TForm1.SaveResults;
var
  i: integer;
  s: string;
  Item: TListItem;
  ResultLineNo: Integer;
  ResultString: string;
begin
  ResultLineNo := FResultList.IndexOf('</mmvalidatedll>') - 2;

  ResultString := '<result';

  for i := 0 to ListView1.Items.Count - 1 do
  begin
    Item := ListView1.Items[i];
    s := format(' %s="%s"',
      [GetMMFromFileName(Item.Caption), GetStatus(Item.SubItems[0])]);
    ResultString := ResultString + s;
  end;

  ResultString := ResultString + ' />';

  FResultList[ResultLineNo] := ResultString;
  FResultList.SaveToFile(RESULT_FILENAME);
end;

// ----------------------------------------------------------------------------
procedure TForm1.StatusText(const Msg: string);
begin
  StatusBar1.SimpleText := Msg;
  Application.ProcessMessages;
end;

// ----------------------------------------------------------------------------
procedure TForm1.ValidateAll;
var
  i: integer;
begin
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    ListView1.Items[i].Selected := True;
    ValidateCurrent;
  end;
end;

// ----------------------------------------------------------------------------
procedure TForm1.ValidateCurrent;
var
  Item: TListItem;
begin
  Item := ListView1.Selected;
  if Item <> nil then
  begin
    Item.SubItems[0] := 'Validating...';
    SaveResults;
    Item.SubItems[0] := ValidateDLL(Item.Caption);
    SaveResults;
  end;
end;

// ----------------------------------------------------------------------------
function TForm1.ValidateDLL(const AFileName: string): string;
const
  ITERATIONS = 100;
var
  i: integer;
  Handle: integer;
  ExportedMethod: TExportedMethod;
  DLLMethodResult: Boolean;
begin
  if not FileExists(AFileName) then
  begin
    Result := 'Can''t find file: ' + AFileName;
    Exit;
  end;

  try
    for i := 1 to ITERATIONS do begin
      Handle := LoadLibrary(PChar(AFileName));
      if Handle = 0 then
        Raise exception.Create('Can''t Load DLL ' + AFileName);
      try
        ExportedMethod := GetProcAddress(Handle, DLL_FUNCTIONNAME);
        if @ExportedMethod = nil then
          Raise exception.Create('Can''t find method: ' + DLL_FUNCTIONNAME);

        StatusText(Format('Validating %s: %d of %d', [AFileName, i, ITERATIONS]));
        DLLMethodResult := ExportedMethod;
        if not DLLMethodResult then
          Raise exception.Create('Exported method failed');
      finally
        if not FreeLibrary(Handle) then
          Raise exception.Create('Error unloading DLL');
      end; // try .. finally
      Sleep(100);
    end; // for i
    Result := 'OK';
  except
    on e: Exception do
      Result := e.Message;
  end;
end;

// ----------------------------------------------------------------------------
procedure TForm1.WMPOSTPROCESSING(var msg: TMessage);
begin
  Caption := Format('%s %s - %s', [Caption, GetFormattedVersion, GetCompilerName]);

  FindDLLs;
  FResultList := TStringList.Create;
  LoadResults;

  if FindCmdLineSwitch('V', ['-', '/'], True) then
  begin
    ValidateAll;
    Close;
  end;
end;

end.

