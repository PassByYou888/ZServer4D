unit MHMainFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  DoStatusIO, PascalStrings, CoreClasses, UnicodeMixedLib, ListEngine;

type
  TMHMainForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  end;

var
  MHMainForm: TMHMainForm;

implementation

{$R *.dfm}


uses MH_1, MH_2, MH_3, MH;

procedure TMHMainForm.Button1Click(Sender: TObject);

  procedure leakproc(x, m: Integer);
  begin
    GetMemory(x);
    if x > m then
        leakproc(x - 1, m);
  end;

begin
  MH.BeginMemoryHook_1;
  leakproc(100, 98);
  MH.EndMemoryHook_1;

  {  We'll find a leak here  }
  DoStatus('The leakproc function allocated %d bytes of memory', [MH.GetHookMemorySize_1]);

  MH.GetHookPtrList_1.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      DoStatus('Leaked address: 0x %s', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2)]);
      DoStatus(NPtr, uData, 80);

      {  Now we can release the address directly  }
      Dispose(NPtr);

      DoStatus('Successfully freed address: 0x %s occupied %d bytes of memory', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2), uData]);
    end);
end;

procedure TMHMainForm.Button2Click(Sender: TObject);
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
  p^.s2 := #$20#$20#$20#$20#$20#$20#$20#$20#$20#$20#$20#$20;
  p^.s3.Text := #1#2#3#4#5#6;
  p^.obj := TObject.Create;
  MH.EndMemoryHook_1;

  {  We'll find a leak here  }
  DoStatus('Tmyrec allocated %d times of memory and occupied %d bytes of space,', [MH.GetHookPtrList_1.Count, MH.GetHookMemorySize_1]);

  MH.GetHookPtrList_1.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      DoStatus('Leaked address: 0x %s', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2)]);
      DoStatus(NPtr, uData, 80);

      {  Now we can release the address directly  }
      FreeMem(NPtr);

      DoStatus('Successfully freed address: 0x %s occupied %d bytes of memory', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2), uData]);
    end);
end;

procedure TMHMainForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText)
end;

procedure TMHMainForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
end;

procedure TMHMainForm.Button3Click(Sender: TObject);
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    p: PMyRec;
  end;

var
  p: PMyRec;
  i: Integer;
begin
  {  1 million times of repeated hooking and repeated release  }
  {  This scenario can be used to count your program overhead and record memory consumption  }
  for i := 0 to 100 * 10000 do
    begin
      MH_2.BeginMemoryHook(4);
      new(p);
      p^.s1 := '12345';
      new(p^.p);
      p^.p^.s1 := '54321';
      MH_2.EndMemoryHook;

      MH_2.GetHookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
        begin
          {  Now we can release the address  }
          FreeMem(NPtr);
        end);
    end;
end;

procedure TMHMainForm.Button4Click(Sender: TObject);
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    p: PMyRec;
  end;

var
  p: PMyRec;
  i: Integer;
  hl: TPointerHashNativeUIntList;
begin
  {  2 million mass recording memory applications, and finally released at one time  }
  {  This scenario can be used to release leaked memory in batches  }

  {  We built 200000 hash arrays for storage  }
  {  The larger the parameter of beginmemoryhook, the better the performance of high-frequency recording for mass storage, but also the more memory consumption  }
  MH_3.BeginMemoryHook(200000);

  for i := 0 to 200 * 10000 do
    begin
      new(p);
      new(p^.p);
      {  Simulate string assignment and trigger realloc call at high frequency  }
      p^.s1 := '111111111111111';
      p^.s1 := '1111111111111111111111111111111111';
      p^.s1 := '11111111111111111111111111111111111111111111111111111111111111';
      p^.p^.s1 := '1';
      p^.p^.s1 := '11111111111111111111';
      p^.p^.s1 := '1111111111111111111111111111111111111';
      p^.p^.s1 := '11111111111111111111111111111111111111111111111111111111111111111111111111';

      if i mod 99999 = 0 then
        begin
          {  Here is the iteration call. We don't record the MH_ 3. Set memoryhooked to false  }
          MH_3.GetMemoryHooked.V := False;
          Button1Click(nil);
          Application.ProcessMessages;
          {  Continue recording memory requests  }
          MH_3.GetMemoryHooked.V := True;
        end;
    end;
  MH_3.EndMemoryHook;

  DoStatus('Total memory allocation %d times, occupying %s space, address span: %s', [MH_3.GetHookPtrList.Count, umlSizeToStr(MH_3.GetHookMemorySize).Text,
    umlSizeToStr(NativeUInt(MH_3.GetHookMemoryMaximumPtr) - NativeUInt(MH_3.GetHookMemoryMinimizePtr)).Text]);

  MH_3.GetHookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      {  Now we can release the address  }
      FreeMem(NPtr);
    end);
  MH_3.GetHookPtrList.PrintHashReport;
  MH_3.GetHookPtrList.SetHashBlockCount(0);
end;

procedure TMHMainForm.Button5Click(Sender: TObject);

var
  s: string;
  sptr: PString;
begin
  MH_1.BeginMemoryHook(16);

  Memo1.Lines.Add('123'); {  Because there is no reference before and after, realloc and GetMem here will not be recorded  }
  s := '12345';           {  Because the s string has been initialized at the beginning of the call and there is no previous and subsequent reference, the realloc here will not be recorded  }

  new(sptr); {  The GetMem address of sptr will be recorded here  }
  sptr^ := '123';
  sptr^ := '123456789'; {  When realloc to sptr occurs, MH will look for the previous and subsequent text. If the realloc recording conditions are met, MH will record it and release it later  }

  {  MH supports control creation and release  }
  {  MH does not support the release of the tform window, because the tform window will register global parameters. After MH releases the tform, some callbacks will report an error if there is no address  }
  TButton.Create(Self).Free;

  MH_1.EndMemoryHook;

  MH_1.GetHookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      {  Now we can release the address  }
      DoStatus(NPtr, uData, 80);
      FreeMem(NPtr);
    end);

  MH_1.GetHookPtrList.SetHashBlockCount(0);
end;

end.

