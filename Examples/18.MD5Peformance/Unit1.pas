unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FlyUtils.CnMD5,
  FlyUtils.CnXXX.Common, IDGlobal, IdHashMessageDigest, IdHash, System.Hash,
  SynCrypto, Fast_MD5, qdigest;

type
  TForm1 = class(TForm)
    btn1: TButton;
    edt1: TEdit;
    edt2: TEdit;
    edt3: TEdit;
    edt4: TEdit;
    dlgOpen1: TOpenDialog;
    btn2: TButton;
    edt5: TEdit;
    edt6: TEdit;
    edt7: TEdit;
    lbl1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    function FastMD5DigestToHex(aMD5: Fast_MD5.TMD5): String;
    function FastMD5File(AFileName: string): String;
    function FastMD5Stream(Stream: TStream): String;
    function FastMD5String(Str: string; StrEncoding: TEncoding = nil): String;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TEdit then
      TEdit(Controls[i]).Clear;
{$IFDEF WIN64}
  lbl1.Caption := 'Win64±‡“Î';
{$ELSE}
  lbl1.Caption := 'Win32±‡“Î';
{$ENDIF}

end;

procedure TForm1.btn1Click(Sender: TObject);
var
  sFile: string;
  iSize: Integer;
  lFileStream:
    TMemoryStream;
  //TFileStream;
  //TBufferedFilestream;
  t: Cardinal;
  sMD5: string;
  IndyMD5: TIdHashMessageDigest5;
  SysMd5: THashMD5;
  B: TBytes;
  morMD5: SynCrypto.TMD5;
  morHash: SynCrypto.TMD5Digest;
begin

  if dlgOpen1.Execute then
  begin
    sFile := dlgOpen1.FileName;
    //lFileStream := TFileStream.Create(sFile, fmOpenRead or fmShareDenyWrite);
    lFileStream := TMemoryStream.Create;
    t := GetTickCount;
    lFileStream.LoadFromFile(sFile);
    //lFileStream := TBufferedFilestream.Create(sFile, fmOpenRead or
    //fmShareDenyWrite);

    iSize := lFileStream.Size;
    edt1.Text := Format('Œƒº˛¥Û–°:%sMB  ‘ÿ»Î∫ƒ ±:%d∫¡√Î',
      [IntToStr(iSize div (1024 * 1024)), GetTickCount - t]);

    edt1.Repaint;

    //=============================================================
    //Indy
    //=============================================================
    IndyMD5 := TIdHashMessageDigest5.Create;
    t := GetTickCount;
    lFileStream.Position := 0;
    sMD5 := IndyMD5.HashStreamAsHex(lFileStream);
    edt2.Text := 'Indy MD5∫ƒ ±£∫' + IntToStr(GetTickCount - t) + '∫¡√Î  MD5:' + sMD5;
    IndyMD5.Free;
    edt2.Repaint;

    //=============================================================
    //FlyUtils.CnMD5
    //=============================================================
    t := GetTickCount;
    lFileStream.Position := 0;
    sMD5 := MD5StreamToHex(lFileStream, iSize);
    edt3.Text := 'Fly MD5∫ƒ ±£∫' + IntToStr(GetTickCount - t) + '∫¡√Î  MD5:' + sMD5;
    edt3.Repaint;

    //=============================================================
    //FastMD5
    //=============================================================
    t := GetTickCount;
    lFileStream.Position := 0;
    sMD5 := FastMD5Stream(lFileStream);
    edt4.Text := 'Fast MD5∫ƒ ±£∫' + IntToStr(GetTickCount - t) + '∫¡√Î  MD5:' + sMD5;
    edt4.Repaint;

    //=============================================================
    //Qdac
    //=============================================================
    t := GetTickCount;
    lFileStream.Position := 0;
    sMD5 := DigestToString(MD5Hash(lFileStream));
    edt5.Text := 'Qdac MD5∫ƒ ±£∫' + IntToStr(GetTickCount - t) + '∫¡√Î  MD5:' + sMD5;
    edt5.Repaint;

    //=============================================================
    //Delphi System.hash
    //=============================================================
    SetLength(B, iSize);
    lFileStream.Position := 0;
    lFileStream.Read(B, iSize);
    lFileStream.Free;

    t := GetTickCount;
    SysMd5 := THashMD5.Create;
    SysMd5.Update(B, iSize);
    sMD5 := UpperCase(SysMd5.HashAsString);
    edt6.Text := 'System MD5∫ƒ ±£∫' + IntToStr(GetTickCount - t) +
      '∫¡√Î  MD5:' + sMD5;
    edt6.Repaint;

    //=============================================================
    //mormot
    //=============================================================
    t := GetTickCount;
    morMD5.Init;
    morMD5.Full(@B[0], iSize, morHash);
    sMD5 := UpperCase(SynCrypto.MD5DigestToString(morHash));
    edt7.Text := 'Mormot MD5∫ƒ ±£∫' + IntToStr(GetTickCount - t) +
      '∫¡√Î  MD5:' + sMD5;
    morMD5.Finalize;
    edt7.Repaint;

    SetLength(B, 0);

    //=============================================================

  end;

end;

procedure TForm1.btn2Click(Sender: TObject);
var
  sMD5: string;
  sText: string;
  IndyMD5: TIdHashMessageDigest5;
  SysMd5: THashMD5;
  morMD5: SynCrypto.TMD5;
  morHash: SynCrypto.TMD5Digest;
  B: TBytes;
begin
  sText := edt1.Text;

  //=============================================================
  //Indy
  //=============================================================
  IndyMD5 := TIdHashMessageDigest5.Create;
  sMD5 := IndyMD5.HashStringAsHex(sText, IndyTextEncoding_UTF8);
  IndyMD5.Free;
  edt2.Text := 'Indy MD5: ' + sMD5;

  //=============================================================
  //FlyUtils.CnMD5
  //=============================================================
  sMD5 := MD5StringToHex(sText);
  edt3.Text := 'FlyUtils MD5: ' + sMD5;

  //=============================================================
  //FastMD5
  //=============================================================
  sMD5 := FastMD5String(sText);
  edt4.Text := 'Fast MD5: ' + sMD5;

  //=============================================================
  //Qdac
  //=============================================================
  sMD5 := DigestToString(MD5Hash(sText));
  edt5.Text := 'Qdac MD5: ' + sMD5;

  //=============================================================
  //Delphi System.hash
  //=============================================================
  B := TEncoding.UTF8.GetBytes(sText);

  SysMd5 := THashMD5.Create;
  SysMd5.Update(B, Length(B));
  sMD5 := UpperCase(SysMd5.HashAsString);
  edt6.Text := 'System MD5: ' + sMD5;

  //=============================================================
  //mormot
  //=============================================================
  morMD5.Init;
  morMD5.Full(@B[0], Length(B), morHash);
  sMD5 := UpperCase(SynCrypto.MD5DigestToString(morHash));
  morMD5.Finalize;
  edt7.Text := 'Mormot MD5: ' + sMD5;
end;

function TForm1.FastMD5File(AFileName: string): String;
var
  Stream: TFileStream;
begin
  Result := '';
  if not FileExists(AFileName) then
    Exit;
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := FastMD5Stream(Stream);
  finally
    Stream.Free;
  end;
end;

function TForm1.FastMD5Stream(Stream: TStream): String;
begin
  Result := '';
  if Stream = nil then
    Exit;
  Result := FastMD5DigestToHex(FastMD5(Stream, 0, Stream.Size));
end;

function TForm1.FastMD5String(Str: string; StrEncoding: TEncoding = nil):
  String;
var
  B: TBytes;
begin
  if StrEncoding = nil then
    B := TEncoding.UTF8.GetBytes(Str)
  else
    B := StrEncoding.GetBytes(Str);
  Result := FastMD5DigestToHex(FastMD5(PBYTE(B), Length(B)));
end;

function TForm1.FastMD5DigestToHex(aMD5: Fast_MD5.TMD5): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to 15 do
    Result := Result + inttohex(Ord(aMD5[i]), 2);
end;

end.
 
