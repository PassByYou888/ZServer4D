unit ConvFileFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.IOUtils, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  PascalStrings, UnicodeMixedLib, CoreClasses;

type
  TConvFileForm = class(TForm)
    FilesMemo: TMemo;
    Label1: TLabel;
    AddFIlesButton: TButton;
    OpenDialog: TOpenDialog;
    UsedDESCheckBox: TCheckBox;
    ExecuteConvertButton: TButton;
    SaveDialog: TSaveDialog;
    UsedZCompressCheckBox: TCheckBox;
    LineLenEdit: TLabeledEdit;
    procedure AddFIlesButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ExecuteConvertButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FCodes: TCoreClassStringList;
  end;

var
  ConvFileForm: TConvFileForm;

implementation

{$R *.dfm}


uses DESSourData, MemoryStream64;

function ConvertBinaryToPascalSource(SourFileList, OutputCodes: TCoreClassStrings; PascalFileName: string; MaxLineLength: Word; UseRandomDESKey, UseCompress: Boolean): Boolean;
var
  DESSour: TCoreClassStringList;

  function KillFileExt(n: string): string;
  begin
    Result := umlDeleteLastStr(n, '.').Text;
  end;

type
  TFileData = record
    DefName: string;
    FullName: string;
    PrefixFileName: string;
    Stream: TStream;
    DesKey: TDESKey;
    MD5: TMD5;
  end;

  PFileData = ^TFileData;

  function CheckFiles: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to SourFileList.Count - 1 do
      begin
        if not TFile.Exists(SourFileList[i]) then
          begin
            MessageDlg(Format('file no exists:%s', [TPath.GetFileName(SourFileList[i])]), mtError, [mbYes], 0);
            exit;
          end;
      end;
    Result := True;
  end;

  procedure BuildPackageEntry(defUt: string);
  var
    FileList: TCoreClassList;

    function ExistsDefName(n: string): Boolean;
    var
      i: Integer;
      p: PFileData;
    begin
      for i := 0 to FileList.Count - 1 do
        begin
          p := FileList[i];
          if SameText(n, p^.DefName) then
              exit(True);
        end;
      Result := False;
    end;

    function MakeDefName(n: umlString): string;
    var
      i: Integer;
    begin
      Result := n;
      for i := 1 to umlGetLength(n) do
        begin
          if CharIn(Result[i], '.":;/\|<>?*%' + '''') then
              Result[i] := '_';
        end;

      if not ExistsDefName(Result) then
          exit;

      i := 1;

      repeat
        Inc(i);
        Result := n + IntToStr(i);
      until not ExistsDefName(Result);
    end;

    procedure BuildStream(p: PFileData);
    var
      i: Integer;
      fs: TCoreClassFileStream;
      KeyStream: TMemoryStream64;
    begin
      fs := TCoreClassFileStream.Create(p^.FullName, fmOpenRead or fmShareDenyWrite);
      p^.MD5 := umlStreamMD5(fs);
      fs.Position := 0;

      p^.Stream := TMemoryStream64.Create;
      if UseCompress then
          MaxCompressStream(fs, p^.Stream)
      else
          p^.Stream.CopyFrom(fs, fs.Size);
      DisposeObject(fs);

      p^.Stream.Position := 0;

      if UseRandomDESKey then
        begin
          for i := 0 to 7 do
              p^.DesKey[i] := umlRandomRange(0, $FF);
          KeyStream := TMemoryStream64.Create;
          umlDES(p^.Stream, KeyStream, p^.DesKey, True);
          DisposeObject(p^.Stream);
          p^.Stream := KeyStream;
        end
      else
        begin
          for i := 0 to 7 do
              p^.DesKey[i] := 0;
        end;
    end;

  var
    buff: TBytes;
    Line: string;
    i, j: Integer;
    p: PFileData;
    TempStream: TStream;
  begin
    FileList := TCoreClassList.Create;

    OutputCodes.Add(Format('unit %s;', [defUt]));
    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('interface', []));
    OutputCodes.Add(Format('', []));
    OutputCodes.Add('uses SysUtils, Classes;');
    OutputCodes.Add(Format('', []));

    for i := 0 to SourFileList.Count - 1 do
      begin
        New(p);
        p^.FullName := SourFileList[i];
        p^.PrefixFileName := KillFileExt(TPath.GetFileName(p^.FullName));
        p^.DefName := MakeDefName(p^.PrefixFileName);

        BuildStream(p);
        OutputCodes.Add(Format('// %s Origin MD5:%s', [TPath.GetFileName(p^.FullName), umlMD52Str(p^.MD5).Text]));
        OutputCodes.Add(Format('procedure Get_%s_Stream(Output: TStream);', [p^.DefName]));
        FileList.Add(p);
      end;
    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('implementation', []));
    OutputCodes.Add(Format('', []));
    if UseCompress then
        OutputCodes.Add('uses ZLib;');
    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('type', []));

    for i := 0 to FileList.Count - 1 do
      begin
        p := FileList[i];
        OutputCodes.Add(Format('  T_%s_PackageBuffer = array [0..%d] of byte;', [p^.DefName, p^.Stream.Size - 1]));
      end;

    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('const', []));
    for i := 0 to FileList.Count - 1 do
      begin
        p := FileList[i];
        OutputCodes.Add(Format('  // compiled %s package', [TPath.GetFileName(p^.FullName)]));
        OutputCodes.Add(Format('  C_%sPackageBuffer : T_%s_PackageBuffer = (', [p^.DefName, p^.DefName]));

        SetLength(buff, p^.Stream.Size);
        p^.Stream.Position := 0;
        p^.Stream.ReadBuffer(buff[0], p^.Stream.Size);
        Line := '';
        for j := low(buff) to high(buff) do
          begin
            if Line <> '' then
                Line := Line + ',' + Format('$%s', [IntToHex(buff[j], 2)])
            else
                Line := Format('$%s', [IntToHex(buff[j], 2)]);
            if Length(Line) > MaxLineLength then
              begin
                if j < high(buff) then
                    OutputCodes.Add('   ' + Line + ',')
                else
                    OutputCodes.Add('   ' + Line + ');');
                Line := '';
              end;
          end;
        if Line <> '' then
          begin
            OutputCodes.Add('   ' + Line + ');');
            Line := '';
          end;
        OutputCodes.Add(Format('', []));
      end;

    OutputCodes.Add(Format('', []));

    if UseCompress or UseRandomDESKey then
      begin
        OutputCodes.Add(Format('', []));
        OutputCodes.Add(Format('procedure DisposeObject(const obj: TObject);', []));
        OutputCodes.Add(Format('begin', []));
        OutputCodes.Add(Format('  if obj <> nil then', []));
        OutputCodes.Add(Format('    begin', []));
        OutputCodes.Add(Format('      try', []));
        OutputCodes.Add(Format('{$IFDEF AUTOREFCOUNT}', []));
        OutputCodes.Add(Format('          obj.DisposeOf;', []));
        OutputCodes.Add(Format('{$ELSE}', []));
        OutputCodes.Add(Format('        obj.Free;', []));
        OutputCodes.Add(Format('{$ENDIF}', []));
        OutputCodes.Add(Format('      except', []));
        OutputCodes.Add(Format('      end;', []));
        OutputCodes.Add(Format('    end;', []));
        OutputCodes.Add(Format('end;', []));
        OutputCodes.Add(Format('', []));
      end;

    if UseRandomDESKey then
      begin
        OutputCodes.AddStrings(DESSour);
        OutputCodes.Add(Format('', []));
      end;

    if UseCompress then
      begin
        OutputCodes.Add(Format('function DecompressStream(Sour, DeTo: TStream): Boolean;', []));
        OutputCodes.Add(Format('var', []));
        OutputCodes.Add(Format('  DC: TZDecompressionStream;', []));
        OutputCodes.Add(Format('  DeSize: Int64;', []));
        OutputCodes.Add(Format('begin', []));
        OutputCodes.Add(Format('  Result := False;', []));
        OutputCodes.Add(Format('  Sour.ReadBuffer(DeSize, 8);', []));
        OutputCodes.Add(Format('  if DeSize > 0 then', []));
        OutputCodes.Add(Format('    begin', []));
        OutputCodes.Add(Format('      DC := TZDecompressionStream.Create(Sour);', []));
        OutputCodes.Add(Format('      Result := DeTo.CopyFrom(DC, DeSize) = DeSize;', []));
        OutputCodes.Add(Format('      DisposeObject(DC);', []));
        OutputCodes.Add(Format('    end;', []));
        OutputCodes.Add(Format('end;', []));
        OutputCodes.Add(Format('', []));
      end;

    for i := 0 to FileList.Count - 1 do
      begin
        p := FileList[i];
        OutputCodes.Add(Format('procedure Get_%s_Stream(Output: TStream);', [p^.DefName]));
        if UseRandomDESKey then
            OutputCodes.Add(Format('const DESKey_%s: TDESKey = (%d, %d, %d, %d, %d, %d, %d, %d);',
            [p^.DefName, p^.DesKey[0], p^.DesKey[1], p^.DesKey[2], p^.DesKey[3], p^.DesKey[4], p^.DesKey[5], p^.DesKey[6], p^.DesKey[7]]));
        OutputCodes.Add(Format('var', []));
        if UseCompress or UseRandomDESKey then
          begin
            OutputCodes.Add(Format('  Source: TMemoryStream;', []));
            OutputCodes.Add(Format('  PrepareSource: TMemoryStream;', []));
          end;
        OutputCodes.Add(Format('  Buff: T_%s_PackageBuffer;', [p^.DefName]));
        OutputCodes.Add(Format('begin', []));
        OutputCodes.Add(Format('  Buff := C_%sPackageBuffer;', [p^.DefName]));

        if (not UseRandomDESKey) and (not UseCompress) then
          begin
            OutputCodes.Add(Format('  Output.WriteBuffer(Buff[0], %d);', [p^.Stream.Size]));
            OutputCodes.Add(Format('  Output.Position := 0;', []));
          end
        else
          begin
            OutputCodes.Add(Format('  Source := TMemoryStream.Create;', []));
            OutputCodes.Add(Format('  Source.WriteBuffer(Buff[0], %d);', [p^.Stream.Size]));
            OutputCodes.Add(Format('  Source.Position := 0;', []));

            if UseCompress then
              begin
                if UseRandomDESKey then
                  begin
                    OutputCodes.Add(Format('  PrepareSource := TMemoryStream.Create;', []));
                    OutputCodes.Add(Format('  DES_Stream(Source, PrepareSource, DESKey_%s, False);', [p^.DefName]));
                    OutputCodes.Add(Format('  DisposeObject(Source);', []));
                    OutputCodes.Add(Format('  Source := PrepareSource;', []));
                    OutputCodes.Add(Format('  Source.Position := 0;', []));
                  end;
                OutputCodes.Add(Format('  DecompressStream(Source, Output);', []));
                OutputCodes.Add(Format('  DisposeObject(Source);', []));
                OutputCodes.Add(Format('  Output.Position := 0;', []));
              end
            else if UseRandomDESKey then
              begin
                OutputCodes.Add(Format('  DES_Stream(Source, Output, DESKey_%s, False);', [p^.DefName]));
                OutputCodes.Add(Format('  DisposeObject(Source);', []));
                OutputCodes.Add(Format('  Output.Position := 0;', []));
              end;
          end;

        OutputCodes.Add(Format('end;', []));
        OutputCodes.Add(Format('', []));
      end;

    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('(*', []));
    OutputCodes.Add(Format('type TGetStreamProc = procedure(Output: TStream);', []));
    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('procedure RegisterFileStream(MD5Text:string; OnProc: TGetStreamProc; FileName:string);', []));
    OutputCodes.Add(Format('begin', []));
    OutputCodes.Add(Format('end;', []));
    OutputCodes.Add(Format('', []));
    OutputCodes.Add(Format('initialization', []));
    for i := 0 to FileList.Count - 1 do
      begin
        p := FileList[i];
        OutputCodes.Add(Format('  RegisterFileStream(' + '''' + '%s' + '''' + ', Get_%s_Stream, ' + '''' + '%s' + '''' + ');',
          [umlMD52Str(p^.MD5).Text, p^.DefName, TPath.GetFileName(p^.FullName)]));
      end;
    OutputCodes.Add(Format('*)', []));
    OutputCodes.Add(Format('end.', []));
    OutputCodes.Add(Format('', []));

    for i := 0 to FileList.Count - 1 do
      begin
        p := FileList[i];
        DisposeObject(p^.Stream);
        Dispose(p);
      end;
    DisposeObject(FileList);
  end;

var
  tmpStream: TMemoryStream64;
begin
  Result := False;
  OutputCodes.Clear;
  if not CheckFiles then
      exit;

  DESSour := TCoreClassStringList.Create;
  tmpStream := TMemoryStream64.Create;
  Get_desSour_Stream(tmpStream);
  DESSour.LoadFromStream(tmpStream);
  DisposeObject(tmpStream);

  BuildPackageEntry(KillFileExt(TPath.GetFileName(PascalFileName)));

  DisposeObject(DESSour);

  Result := True;
end;

procedure TConvFileForm.AddFIlesButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if not OpenDialog.Execute then
      exit;
  for i := 0 to OpenDialog.Files.Count - 1 do
      FilesMemo.Lines.Add(OpenDialog.Files[i]);
end;

procedure TConvFileForm.ExecuteConvertButtonClick(Sender: TObject);
begin
  if not SaveDialog.Execute then
      exit;

  if ConvertBinaryToPascalSource(FilesMemo.Lines, FCodes, SaveDialog.FileName,
    umlStrToInt(LineLenEdit.Text, 200), UsedDESCheckBox.Checked, UsedZCompressCheckBox.Checked) then
    begin
      FCodes.SaveToFile(SaveDialog.FileName);
      ShowMessage('Pascal source Finished!');
    end;
end;

procedure TConvFileForm.FormCreate(Sender: TObject);
begin
  FCodes := TCoreClassStringList.Create;
end;

end.
