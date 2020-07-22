unit CopyPtrSpeedFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  CoreClasses, DoStatusIO, PascalStrings, UnicodeMixedLib, Vcl.ExtCtrls;

type
  TCopyPtrSpeedForm = class(TForm)
    Memo1: TMemo;
    Button8GReadWrite: TButton;
    doStatusTimer: TTimer;
    procedure Button8GReadWriteClick(Sender: TObject);
    procedure doStatusTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure DoStatus_Bcakcall(Text_: SystemString; const ID: Integer);
  public
  end;

var
  CopyPtrSpeedForm: TCopyPtrSpeedForm;

implementation

{$R *.dfm}


procedure TCopyPtrSpeedForm.Button8GReadWriteClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    var
      siz: NativeInt;
      p: Pointer;
      tk: TTimeTick;

      copyStepSiz: NativeInt;
      copySour: PByte;
      copyDest: PByte;
      copydone: NativeInt;
    begin
      DoStatus('Alloc memory.');
      tk := GetTimeTick;
      siz := NativeInt(1024 * 1024 * 1024) * 8;
      p := GetMemory(siz);
      DoStatus('Alloc memory done. time:%dms', [GetTimeTick - tk]);
      DoStatus('');

      DoStatus('Init Memory.');
      FillPtr(p, siz, 0);
      DoStatus('');

      DoStatus('test FillPtr api.');
      tk := GetTimeTick;
      FillPtr(p, siz, 1);
      DoStatus('done. time:%dms', [GetTimeTick - tk]);
      DoStatus('');

      DoStatus('test FillChar api.');
      tk := GetTimeTick;
      FillChar(p^, siz, 2);
      DoStatus('done. time:%dms', [GetTimeTick - tk]);
      DoStatus('');

      copyStepSiz := 128 * 1024 * 1024;
      copySour := GetMemory(copyStepSiz);
      FillPtr(copySour, copyStepSiz, 3);
      copydone := 0;
      copyDest := p;
      tk := GetTimeTick;
      DoStatus('test CopyPtr api.');
      while copydone < siz do
        begin
          copyPtr(copySour, copyDest, copyStepSiz);
          inc(copydone, copyStepSiz);
          inc(copyDest, copyStepSiz);
        end;
      DoStatus('done. time:%dms', [GetTimeTick - tk]);
      FreeMemory(copySour);
      DoStatus('');

      copyStepSiz := 128 * 1024 * 1024;
      copySour := GetMemory(copyStepSiz);
      FillPtr(copySour, copyStepSiz, 4);
      copydone := 0;
      copyDest := p;
      tk := GetTimeTick;
      DoStatus('test move api.');
      while copydone < siz do
        begin
          move(copySour^, copyDest^, copyStepSiz);
          inc(copydone, copyStepSiz);
          inc(copyDest, copyStepSiz);
        end;
      DoStatus('done. time:%dms', [GetTimeTick - tk]);
      FreeMemory(copySour);
      DoStatus('');

      FreeMemory(p);

      DoStatus('all done.');
    end);
end;

procedure TCopyPtrSpeedForm.doStatusTimerTimer(Sender: TObject);
begin
  DoStatus();
  TCompute.ProgressPost;
end;

procedure TCopyPtrSpeedForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatus_Bcakcall);
  StatusThreadID := False;
end;

procedure TCopyPtrSpeedForm.DoStatus_Bcakcall(Text_: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(Text_);
end;

end.
