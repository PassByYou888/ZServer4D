unit Unit1;

{
测试程序需要 mORMot库，请自行下载
https://github.com/synopse/mORMot
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FlyUtils.CnMD5,
  FlyUtils.CnXXX.Common, IdGlobal, IdHashMessageDigest, IdHash, System.Hash,
  SynCrypto;

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
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    {Private declarations}
  public
    {Public declarations}
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


uses Fast_MD5, qdigest;

procedure TForm1.btn1Click(Sender: TObject);
var
  sFile: string;
  lFileStream: TMemoryStream; //TFileStream;
  t: Cardinal;
  sMD5: string;
  md5: TIdHashMessageDigest5;
  SysMd5: THashMD5;
  B: TBytes;
  morMD5: SynCrypto.TMD5;
  morHash: SynCrypto.TMD5Digest;
begin
  md5 := TIdHashMessageDigest5.Create;
  if dlgOpen1.Execute then
  begin
    sFile := dlgOpen1.FileName;
    //lFileStream := TFileStream.Create(sFile, fmOpenRead or fmShareDenyWrite);
    lFileStream := TMemoryStream.Create;
    lFileStream.LoadFromFile(sFile);
    edt1.Text := '文件大小：' + IntToStr(lFileStream.Size div (1024 * 1024)) + 'MB';

    //=============================================================
    //Indy
    //=============================================================
    t := GetTickCount;
    lFileStream.Position := 0;
    sMD5 := md5.HashStreamAsHex(lFileStream);
    edt2.Text := 'IndyMD5耗时：' + IntToStr(GetTickCount - t) + '毫秒  MD5:' + sMD5;
    md5.Free;

    //=============================================================
    //FlyUtils.CnMD5
    //=============================================================
    t := GetTickCount;
    lFileStream.Position := 0;
    sMD5 := MD5StreamToHex(lFileStream, lFileStream.Size);
    edt3.Text := 'FlyMD5耗时：' + IntToStr(GetTickCount - t) + '毫秒  MD5:' + sMD5;

    //=============================================================
    //FastMD5
    //=============================================================
    t := GetTickCount;
    lFileStream.Position := 0;
    sMD5 := FastMD5Stream(lFileStream);
    edt4.Text := 'FastMD5耗时：' + IntToStr(GetTickCount - t) + '毫秒  MD5:' + sMD5;

    //=============================================================
    //Qdac
    //=============================================================
    t := GetTickCount;
    lFileStream.Position := 0;
    sMD5 := DigestToString(MD5Hash(lFileStream));
    edt5.Text := 'QdacMD5耗时：' + IntToStr(GetTickCount - t) + '毫秒  MD5:' + sMD5;

    //=============================================================
    //Delphi System.hash
    //=============================================================
    SetLength(B, lFileStream.Size);
    lFileStream.Position := 0;
    lFileStream.Read(B, lFileStream.Size);

    t := GetTickCount;
    SysMd5 := THashMD5.Create;
    SysMd5.Update(B, lFileStream.Size);
    edt6.Text := 'SystemMD5耗时：' + IntToStr(GetTickCount - t) +
      '毫秒  MD5:' + sMD5;

    //=============================================================
    //mormot
    //=============================================================
    t := GetTickCount;
    morMD5.Init;
    morMD5.Full(@B[0], lFileStream.Size, morHash);
    sMD5 := UpperCase(SynCrypto.MD5DigestToString(morHash));
    edt7.Text := 'mormotMD5耗时：' + IntToStr(GetTickCount - t) +
      '毫秒  MD5:' + sMD5;
    morMD5.Finalize;

    //=============================================================

    lFileStream.Free;

  end;

end;

procedure TForm1.btn2Click(Sender: TObject);
var
  md5: TIdHashMessageDigest5;
begin
  md5 := TIdHashMessageDigest5.Create;
  edt2.Text := FastMD5String(edt1.Text);
  edt3.Text := MD5StringToHex(edt1.Text);
  edt4.Text := md5.HashStringAsHex(edt1.Text, IndyUTF8Encoding);
  md5.Free;
end;

end.
