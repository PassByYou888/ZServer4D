unit MMValidation;

interface

uses
  Windows, SysUtils, Classes, Dialogs, Math;

type
  TValidateFunction = function: Boolean of object;
  TValidateProgressEvent = procedure(const CurrentValidation, Failed: string) of object;

  TMMValidation = class(TComponent)
  private
    FOnProgress: TValidateProgressEvent;
    FFailedList: string;
    function Validate1: Boolean;
    function Validate2: Boolean;
    function Validate3: Boolean;
    function Validate4: Boolean;
    function Validate5: Boolean;
    function Validate6: Boolean;
    function Validate7: Boolean;
    function Validate8: Boolean;
    function Validate9: Boolean;
    function Validate10: Boolean;
    function Validate11: Boolean;
    function Validate12: Boolean;
    function Validate13: Boolean;
    function Validate14: Boolean;
    function Validate15: boolean;
    function Validate16: boolean;
    function Validate17: Boolean;
    function Validate18: Boolean;
    function Validate19: Boolean;
    function Validate20: Boolean;
    function Validate21: Boolean;
    function Validate22: Boolean;
    function Validate24: Boolean;
    function Validate25: Boolean;
    function Validate26: Boolean;
    function Validate27: boolean;
    function Validate28: boolean;
  private
    procedure DoValidate(ValidateFunction: TValidateFunction; const ValidationName: string);
  public
    property OnProgress: TValidateProgressEvent read FOnProgress write FOnProgress;  // to show progress of validation

    function Validate: string;      // returns list of failed validations
    function ExtraValidate: string; // returns list of failed validations
  end;

TValidate28Thread = class(TThread)
public
  procedure Execute; override;
end;

implementation

uses
  BenchmarkForm, BenchmarkUtilities;

function TMMValidation.Validate: string;
begin
  FFailedList := '';

  DoValidate(Validate1, 'Validate1');
  DoValidate(Validate2, 'Validate2');
  DoValidate(Validate3, 'Validate3');
  DoValidate(Validate4, 'Validate4');
  DoValidate(Validate5, 'Validate5');
  DoValidate(Validate6, 'Validate6');
  DoValidate(Validate7, 'Validate7');
  DoValidate(Validate8, 'Validate8');
  DoValidate(Validate9, 'Validate9');
  DoValidate(Validate10, 'Validate10');
  DoValidate(Validate11, 'Validate11');
  DoValidate(Validate12, 'Validate12');
  DoValidate(Validate14, 'Validate14');
  DoValidate(Validate17, 'Validate17');
  DoValidate(Validate19, 'Validate19');
  DoValidate(Validate21, 'Validate21');
  DoValidate(Validate22, 'Validate22');
  DoValidate(Validate24, 'Validate24');
  DoValidate(Validate25, 'Validate25');
  DoValidate(Validate26, 'Validate26');
  DoValidate(Validate28, 'Validate28');

  // Validate13 is really very nasty by provoking an EOutofMemory
  // We execute it at the end, then give the system 10 seconds to relax...
  DoValidate(Validate13, 'Validate13');
  Sleep(10000);

  Result := FFailedList;
end;

function TMMValidation.ExtraValidate: string;
begin
  FFailedList := '';

  DoValidate(Validate15, 'Validate15');
  DoValidate(Validate16, 'Validate16');
  DoValidate(Validate18, 'Validate18');
  DoValidate(Validate20, 'Validate20');
  DoValidate(Validate27, 'Validate27');

  Result := FFailedList;
end;

procedure TMMValidation.DoValidate(ValidateFunction: TValidateFunction; const ValidationName: string);
var
  Passed: Boolean;
begin
  if Assigned(FOnProgress) then
    FOnProgress(ValidationName, FFailedList);

  Passed := True;
  try
    Passed := ValidateFunction;
  except
    Passed := False;
  end;

  if not Passed then
    FFailedList := FFailedList + ValidationName + ';';
end;

type
  TExportedMethod = procedure;

(**************************************************************************************************)
// all Validation functions below
(**************************************************************************************************)

function TMMValidation.Validate1 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATE : Cardinal = 1;//1 byte

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
   SetLength(SomeArray, BYTESTOALLOCATE);
 except
  Result := False;
 end;
end;

function TMMValidation.Validate2 : Boolean;
var
 SomeArray1, SomeArray2 : array of Byte;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATE : Cardinal = 1;//1 byte

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
   begin
    SetLength(SomeArray1, BYTESTOALLOCATE);
    SetLength(SomeArray2, BYTESTOALLOCATE);
    //Validate that pointers do not overlap
    if @SomeArray1[0] = @SomeArray2[0] then
     Result := False;
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate3 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATEMAX : Cardinal = 2;//2 byte

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATEMAX then
   begin
    //Allocate 1 byte
    SetLength(SomeArray, 1);
    //Fill it
    SomeArray[0] := 0;
    //Expand array
    SetLength(SomeArray, 2);
    //Validate that data in first allocation is preserved by realloc/expand
    if SomeArray[0] <> 0 then
     Result := False;
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate4 : Boolean;
var
 I : Cardinal;
 MemoryStatus : TMemoryStatus;
 pMem : Pointer;
 pChr : PAnsiChar;
const
 BYTESTOALLOCATEMAX : Cardinal = 32000;//32 KB

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATEMAX then
   begin
    pMem := AllocMem(BYTESTOALLOCATEMAX);
    pChr := pMem;
    for I := 0 to BYTESTOALLOCATEMAX-1 do
     begin
      if pChr^ <> #0 then
       begin
        Result := False;
        Break;
       end;
      Inc(PChr);
     end;
    FreeMem(pMem);
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate5 : Boolean;
var
 I : Cardinal;
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATE : Cardinal = 64000;//64kB byte
 NOOFITERATIONS : Cardinal = 100000;

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  for I := 1 to NOOFITERATIONS do
   begin
    if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
     //Allocate
     SetLength(SomeArray, BYTESTOALLOCATE);
    //Free
    SetLength(SomeArray, 0);
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate6 : Boolean;
var
 I : Cardinal;
 pMem : Pointer;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATE : Cardinal = 40000;//40kB byte
 NOOFITERATIONS : Cardinal = 200000;

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  for I := 1 to NOOFITERATIONS do
   begin
    if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
     begin
      //Allocate
      GetMem(pMem, BYTESTOALLOCATE);
      //Free
      FreeMem(pMem);
     end;
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate7 : Boolean;
var
 I1, I2, I3, J1, J2 : Cardinal;
 pMem : Pointer;
 PointerArray : array[1..10] of Pointer;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATE : Cardinal = 1;//1 byte

begin
 for I1 := 1 to Length(PointerArray) do
  PointerArray[I1] := nil;
 try
  Result := True;
  for I2 := 1 to Length(PointerArray) do
   begin
    GlobalMemoryStatus(MemoryStatus);
    if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
     begin
      //Allocate all pointers
      GetMem(pMem, BYTESTOALLOCATE);
      PointerArray[I2] := pMem;
     end;
   end;
  for J1 := 1 to Length(PointerArray)-1 do
   for J2 := J1+1 to Length(PointerArray) do
    begin
     //No pointers can overlap = be closer than BYTESTOALLOCATE
     if Cardinal(Abs(Integer(PointerArray[J1]) - Integer(PointerArray[J2]))) < BYTESTOALLOCATE then
      begin
       Result := False;
       Break;
      end;
    end;
  //Free all pointers
  for I3 := 1 to Length(PointerArray) do
   begin
    pMem := PointerArray[I3];
    FreeMem(pMem);
    PointerArray[I3] := nil;
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate8 : Boolean;
var
 I1, I2, I3, J1, J2 : Cardinal;
 pMem : Pointer;
 PointerArray : array[1..10] of Pointer;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATE : Cardinal = 2;//2 byte

begin
 for I1 := 1 to Length(PointerArray) do
  PointerArray[I1] := nil;
 try
  Result := True;
  for I2 := 1 to Length(PointerArray) do
   begin
    GlobalMemoryStatus(MemoryStatus);
    if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
     begin
      //Allocate all pointers
      GetMem(pMem, BYTESTOALLOCATE);
      PointerArray[I2] := pMem;
     end;
   end;
  for J1 := 1 to Length(PointerArray)-1 do
   for J2 := J1+1 to Length(PointerArray) do
    begin
     //No pointers can overlap = be closer than BYTESTOALLOCATE
     if Cardinal(Abs(Integer(PointerArray[J1]) - Integer(PointerArray[J2]))) < BYTESTOALLOCATE then
      begin
       Result := False;
       Break;
      end;
    end;
  //Free all pointers
  for I3 := 1 to Length(PointerArray) do
   begin
    pMem := PointerArray[I3];
    FreeMem(pMem);
    PointerArray[I3] := nil;
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate9 : Boolean;
var
 I1, I2, I3, J1, J2, BytesToAllocate : Cardinal;
 pMem : Pointer;
 PointerArray : array[1..2000] of Pointer;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATEMAX : Cardinal = 3000;//3000 byte

begin
 Result := True;
 try
  for BytesToAllocate := 1 to BYTESTOALLOCATEMAX do
   begin
    for I1 := 1 to Length(PointerArray) do
     PointerArray[I1] := nil;
    for I2 := 1 to Length(PointerArray) do
     begin
      GlobalMemoryStatus(MemoryStatus);
      if MemoryStatus.dwAvailVirtual >= BytesToAllocate then
       begin
        //Allocate all pointers
        GetMem(pMem, BytesToAllocate);
        PointerArray[I2] := pMem;
       end;
     end;
    for J1 := 1 to Length(PointerArray)-1 do
     for J2 := J1+1 to Length(PointerArray) do
      begin
       //No pointers can overlap = be closer than BytesToAllocate
       if Cardinal(Abs(Integer(PointerArray[J1]) - Integer(PointerArray[J2]))) < BytesToAllocate then
        begin
         Result := False;
         Exit;
        end;
      end;
    //Free all pointers
    for I3 := 1 to Length(PointerArray) do
     begin
      pMem := PointerArray[I3];
      FreeMem(pMem);
      PointerArray[I3] := nil;
     end;
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate10 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
 Address: Pointer;
 AddressCard : Cardinal;
const
 BYTESTOALLOCATE : Cardinal = 1000000;//1 MB

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
   begin
    SetLength(SomeArray, BYTESTOALLOCATE);
    Address := @SomeArray[0];
    AddressCard := Cardinal(Address);
    //Is allocation 4 byte aligned?
    if AddressCard mod 4 <> 0 then
     Result := False;
   end;
 except
  Result := False;
 end;
end;

//Validating small to medium upsize of one array combined with many small strings resize

function TMMValidation.Validate11 : Boolean;
var
 I, J, K, L, NoOfStrings  : Cardinal;
 SomeArray : array of Cardinal;
 StringArray : array[1..517] of Ansistring;
 TempS : AnsiString;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATEMAX : Integer = 320001;

begin
 Result := True;
 try
   NoOfStrings := Length(StringArray);
   for I := 0 to BYTESTOALLOCATEMAX-1 do
   begin
    //Clear a string sometimes
    if I mod 5 = 0 then
     StringArray[Random(NoOfStrings) + 1] := '';
    //Grow a string with an 'A'
    StringArray[(I mod NoOfStrings) + 1] := StringArray[(I mod Cardinal(Length(StringArray)))+1] + 'A';
    //Grow SomeArray if place for it
    GlobalMemoryStatus(MemoryStatus);
    if MemoryStatus.dwAvailVirtual >= I+1 then
     SetLength(SomeArray, I+1);
    SomeArray[I] := I;
    //Validate that SomeArray data are not changed
    for J := 0 to I do
     begin
      if SomeArray[J] <> J then
       begin
        Result := False;
        Exit;
       end;
     end;
    //Validate that all strings contains 'A's and nothing else
    for K := 1 to Length(StringArray) do
     begin
      TempS := StringArray[K];
      for L := 1 to Length(TempS) do
       begin
        if TempS[L] <> 'A' then
         begin
          Result := False;
          Exit;
         end;
       end;
     end;
   end;
 except
  Result := False;
 end;
end;

//Creates a square 2 dimensional array
//Grows it from 1x1 = 1 byte to BYTESTOALLOCATEMAX bytes

function TMMValidation.Validate12 : Boolean;
var
 SomeArray : array of array of Cardinal;
 MemoryStatus : TMemoryStatus;
 I1, I2, I3, I4, I5, IMax : Cardinal;
const
 BYTESTOALLOCATEMAX : Integer = 10000000;//10 MB

begin
 Result := True;
 GlobalMemoryStatus(MemoryStatus);
 IMax := Trunc(Sqrt(BYTESTOALLOCATEMAX));
 try
  //Grow array
  for I1 := 2 to IMax do
   begin
    if MemoryStatus.dwAvailVirtual >= I1*I1 then
     begin
      SetLength(SomeArray, I1, I1);
      //Old data preserved? (0's in first iteration)
      for I2 := 0 to I1-2 do
       for I3 := 0 to I1-2 do
        begin
         if SomeArray[I2,I3] <> (I2 * I3) mod 256 then
          begin
           Result := False;
           Exit;
          end;
        end;
      //Fill it (again)
      for I4 := 0 to I1-1 do
       for I5 := 0 to I1-1 do
        SomeArray[I4,I5] := (I4 * I5) mod 256;
     end
    else
     //Stop test if no more memory available
     Exit;
   end;
 except
  Result := False;
 end;
end;

// stress test... pushing the MM to an "Out of Memory" by allocating 16 kB pointers
// see whether we get an EOutOfMemory (OK) or something else (WRONG)

function TMMValidation.Validate13: Boolean;
var
 Pointers : array[0..200000] of Pointer;
 n : integer;

begin
 n := 0;
 try
  repeat
   //Allocate 16 kB pointer
   GetMem(Pointers[n], 16384);
   PAnsiChar(Pointers[n])[4] := 'A';
   Inc(n);
  until n > High(Pointers);
  Result := True;  // no exception at all, and using more than 3 GB...interesting !
 except
  on E: EOutOfMemory do  // that's the right exception...
   Result := True
  else // all other exceptions are wrong...
   Result := False;
 end;
 //Release memory
 while n > 0 do
  begin
   Dec(n);
   FreeMem(Pointers[n]);
  end;
end;

function TMMValidation.Validate14 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
 BytesToAllocate, I, StartAddress : Cardinal;
const
 BYTESTOALLOCATEMIN : Cardinal = 10 * 1024;//10 Kbyte
 BYTESTOALLOCATEMAX : Cardinal = 100 * 1024;//100 Kbyte

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  for BytesToAllocate := BYTESTOALLOCATEMIN to BYTESTOALLOCATEMAX do
   begin
    if MemoryStatus.dwAvailVirtual >= BytesToAllocate then
     begin
      //Allocate
      SetLength(SomeArray, BytesToAllocate);
      //4 byte alignment?
      StartAddress := Cardinal(@SomeArray[8]);
      if StartAddress mod 4 <> 0 then
       begin
        Result := False;
        Exit;
       end;
      for I := 0 to BytesToAllocate-1 do
       SomeArray[I] := 255;
      //Free
      SetLength(SomeArray, 0);
     end;
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate15: boolean;
var
 Ptr: Pointer;

begin
 {Allocate a block}
 GetMem(Ptr, 10);
 {Free it}
 FreeMem(Ptr);
 {Try to free it again and see whether the MM catches the error}
 try
  FreeMem(Ptr);
  Result := False;
 except
  Result := True;
 end;
end;

function TMMValidation.Validate16: boolean;
var
  i: integer;
  t1: pointer;
  t2: pointer;
const
  TRY_TIMES = 1000;
  SIZE_TO_ALLOCATE = 128;
begin
  i := TRY_TIMES;
  while (i <> 0) do
  begin
    dec(i);
    try
      GetMem(t1, SIZE_TO_ALLOCATE);
      FreeMem(t1);
      FreeMem(t1);
      Result := False;
      exit;
    except
    end;

    GetMem(t1, SIZE_TO_ALLOCATE);
    GetMem(t2, SIZE_TO_ALLOCATE);
    if (t1 = t2) then
    begin
      result := false;
      exit;
    end;

    FreeMem(t1);
    FreeMem(t2);
  end;

  result := true;
end;

function TMMValidation.Validate17 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATE : Cardinal = 256000000;//256 Mbyte

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
   SetLength(SomeArray, BYTESTOALLOCATE);
 except
  Result := False;
 end;
end;

//Grow dynamic array with STEPSIZE from MIN to MAX and validate that it is 16 byte aligned

function TMMValidation.Validate18 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
 BytesToAllocate, I, StartAddress : Cardinal;
const
 BYTESTOALLOCATESTEPSIZE : Cardinal = 11;
 BYTESTOALLOCATEMIN : Cardinal = 16;//
 BYTESTOALLOCATEMAX : Cardinal = 100000000;//100 Mbyte

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  BytesToAllocate := BYTESTOALLOCATEMIN;
  while BytesToAllocate <= BYTESTOALLOCATEMAX do
   begin
    if MemoryStatus.dwAvailVirtual >= BytesToAllocate then
     begin
      //Allocate
      SetLength(SomeArray, BytesToAllocate);
      //16 byte alignment?
      //Add 8 byte offset to compensate for refcount and size data
      StartAddress := Cardinal(@SomeArray[8]);
      if StartAddress mod 16 <> 0 then
       begin
        Result := False;
        Exit;
       end;
      for I := 0 to BytesToAllocate-1 do
       SomeArray[I] := 255;
      //Free
      SetLength(SomeArray, 0);
      BytesToAllocate := BytesToAllocate * BYTESTOALLOCATESTEPSIZE;
     end;
   end;
 except
  Result := False;
 end;
end;

//Allocates 100000 pointers up to BYTESTOALLOCATEMAX per pointer
//Validate 4 byte alignment, unique pointers, non overlapping blocks,
//read and write to block with 4 byte granularity, but does not write past the end.
//Does not write to the last 0-3 bytes of a block

function TMMValidation.Validate19 : Boolean;
var
 I1, I2, I3, J1, J2, J3, BytesToAllocate, RunNo, IntegersToFill, BigBlockSize : Cardinal;
 pMem : Pointer;
 PointerArray : array[1..100000] of Pointer;
 MemoryStatus : TMemoryStatus;
 pInt : pInteger;
 RefData, MemBlockSize : Integer;
const
 BYTESTOALLOCATEMAX : Cardinal = 2500;//2.5 Kbyte per pointer max
 RUNNOMAX : Cardinal = 125;
 BIGBLOCKGROWRATE : Cardinal = 1024*1024;//1 MB extra per run

begin
 Result := True;
 try
  for RunNo := 1 to RUNNOMAX do
   begin
    //Initialize pointer array - not really needed
    for I1 := 1 to Length(PointerArray) do
     PointerArray[I1] := nil;
    for I2 := 1 to Length(PointerArray) do
     begin
      if I2 = 1 then
       begin
        BigBlockSize := RunNo * BIGBLOCKGROWRATE + 4;     //Always allocate >= 4 byte
        BytesToAllocate := BigBlockSize//Allocate one big block that grows linearily
       end
      else
       begin
        BytesToAllocate := Random(BYTESTOALLOCATEMAX-1)+4;//Always allocate >= 4 byte
       end;
      if BytesToAllocate < 4 then
       raise Exception.Create('To small block allocated');
      GlobalMemoryStatus(MemoryStatus);
      if MemoryStatus.dwAvailVirtual >= BytesToAllocate then
       begin
        //Allocate all pointers
        GetMem(pMem, BytesToAllocate);
        if Integer(pMem) mod 4 <> 0 then
         begin
          //Pointer not 4 byte aligned
          Result := False;
          Exit;
         end;
        PointerArray[I2] := pMem;
        //Fill allocated memory with unique data = Pointer
        pInt := pMem;
        IntegersToFill := (BytesToAllocate) div 4;
        if IntegersToFill > 0 then
         begin
          //But write size in integers in first 4 bytes
          pInt^ := IntegersToFill;
          Inc(pInt);
          //Does not write to all bytes in block if block size is not multiple of 4
          for J1 := 1 to IntegersToFill-1 do
           begin
            //Write with 4 byte granularity
            pInt^ := Integer(pMem);
            Inc(pInt);
           end;
         end;
       end;
     end;
    //No mem overwritten
    for J2 := 1 to Length(PointerArray) do
     begin
      pInt := PointerArray[J2];
      RefData := Integer(PointerArray[J2]);
      MemBlockSize := pInt^;//Size in first 4 bytes
      Inc(pInt);
      for J3 := 1 to MemBlockSize-1 do
       begin
        if pInt^ <> RefData then
         begin
          //Memory block is overwritten
          Result := False;
          Exit;
         end;
        Inc(pInt);
       end;
     end;
    //Free all pointers
    for I3 := 1 to Length(PointerArray) do
     begin
      pMem := PointerArray[I3];
      FreeMem(pMem);
      PointerArray[I3] := nil;
     end;
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate20 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATE : Cardinal = 1024*1024*1024;//1G

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
   SetLength(SomeArray, BYTESTOALLOCATE);
 except
  Result := False;
 end;
end;

function TMMValidation.Validate21 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
 I1, I2, I3, SmallAllocSize, BigAllocSize : Cardinal;
const
 SMALLALLOCSIZEMIN : Cardinal = 1;//1 byte
 SMALLALLOCSIZEMAX : Cardinal = 100;//100 byte
 BIGALLOCSIZEMIN : Cardinal = 1024*1024;//1 Mbyte
 BIGALLOCSIZEMAX : Cardinal = 1024*1024 + 100;//1 Mbyte + 100 b
 FILLBYTE : Byte = 224;

begin
 Result := True;
 try
 for SmallAllocSize := SMALLALLOCSIZEMIN to SMALLALLOCSIZEMAX do
  for BigAllocSize := BIGALLOCSIZEMIN to BIGALLOCSIZEMAX do
   begin
    GlobalMemoryStatus(MemoryStatus);
    if MemoryStatus.dwAvailVirtual >= BigAllocSize+SmallAllocSize then
     begin
      SetLength(SomeArray, SmallAllocSize);
      for I1 := 0 to SmallAllocSize-1 do
       SomeArray[I1] := FILLBYTE;
      SetLength(SomeArray, BigAllocSize);
      for I2 := 0 to SmallAllocSize-1 do
       begin
        if SomeArray[I2] <> FILLBYTE then
         begin
          Result := False;
          Exit;
         end;
       end;
      SetLength(SomeArray, SmallAllocSize);
      for I3 := 0 to SmallAllocSize-1 do
       begin
        if SomeArray[I3] <> FILLBYTE then
         begin
          Result := False;
          Exit;
         end;
       end;
     end;
   end;
 except
  Result := False;
 end;
end;

//Grow dynamic array with STEPSIZE from MIN to MAX and validate that it is 4 byte aligned

function TMMValidation.Validate22 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
 BytesToAllocate, StartAddress, I2, I3, OldBytesToAllocate : Cardinal;
const
 BYTESTOALLOCATESTEPSIZE : Cardinal = 2;
 BYTESTOALLOCATEMIN : Cardinal = 5;//5 byte
 BYTESTOALLOCATEMAX : Cardinal = 50*1024*1024;//50 Mbyte
 FILLBYTE : Byte = 255;

begin
 try
  Result := True;
  OldBytesToAllocate := 1;
  //Allocate
  SetLength(SomeArray, 1);
  SomeArray[0] := FILLBYTE;
  BytesToAllocate := BYTESTOALLOCATEMIN;
  while BytesToAllocate <= BYTESTOALLOCATEMAX do
   begin
    GlobalMemoryStatus(MemoryStatus);
    if MemoryStatus.dwAvailVirtual >= BytesToAllocate then
     begin
      //Reallocate
      SetLength(SomeArray, BytesToAllocate);
      //Old data preserved?
      for I3 := 0 to OldBytesToAllocate-1 do
       begin
        if SomeArray[I3] <> FILLBYTE then
         begin
          Result := False;
          Exit;
         end;
       end;
      //4 byte alignment?
      //No need to add offset to compensate for refcount and size data
      StartAddress := Cardinal(@SomeArray[0]);
      if StartAddress mod 4 <> 0 then
       begin
        Result := False;
        Exit;
       end;
      //Fill newly allocated space
      for I2 := OldBytesToAllocate to BytesToAllocate-1 do
       SomeArray[I2] := FILLBYTE;
      OldBytesToAllocate := BytesToAllocate;
      BytesToAllocate := BytesToAllocate * BYTESTOALLOCATESTEPSIZE;
     end;
   end;
  //Free
  SetLength(SomeArray, 0);
 except
  Result := False;
 end;
end;

//Allocate NOOFPOINTERS starting at 1 byte size and growing with
//ALLOCGROWSTEPSIZE per pointer and then free's them all.
//Validates that all allocated memory is freed after SLEEPTIMEAFTERFREE seconds.

//NOT ACCEPTED BY COMMUNITY
{
function TMMValidation.Validate23 : Boolean;
var
 MemoryStatus : TMemoryStatus;
 PointerArray : array of Pointer;
 I, IntitialFreeMemory, EndFreeMemory : Integer;
 AllocSize : Integer;
 AllocSizeFP : Double;
const
 NOOFPOINTERS : Integer = 1000000;
 ALLOCGROWSTEPSIZE : Double = 0.001;
 SLEEPTIMEAFTERFREE : Integer = 10;//Seconds to free
 SLACK : Integer = 32*1024*1024;

begin
 try
  Result := True;
  GlobalMemoryStatus(MemoryStatus);
  IntitialFreeMemory := MemoryStatus.dwAvailVirtual;
  //Allocate
  SetLength(PointerArray, NOOFPOINTERS);
  AllocSizeFP := 1;
  for I:= 0 to Length(PointerArray)-1 do
   begin
    AllocSizeFP := AllocSizeFP + ALLOCGROWSTEPSIZE;
    AllocSize := Round(AllocSizeFP);
    GetMem(PointerArray[I], AllocSize);
   end;
  //Free
  for I:= 0 to Length(PointerArray)-1 do
   FreeMem(PointerArray[I]);
  SetLength(PointerArray, 0);
  //Give a little time to free
  Sleep(SLEEPTIMEAFTERFREE*1000);
  GlobalMemoryStatus(MemoryStatus);
  EndFreeMemory := MemoryStatus.dwAvailVirtual;
  //Did everything get freed?
  if IntitialFreeMemory - EndFreeMemory > SLACK then
   Result := False;
 except
  Result := False;
 end;
 //More free than when we started !!!!!!!!!!!!!
 if EndFreeMemory > IntitialFreeMemory then
  raise Exception.Create('More free memory after running test !!!');
end;
}

function TMMValidation.Validate24 : Boolean;
var
 MemoryStatus : TMemoryStatus;
 PointerArray : array of Pointer;
 I, FreeMemoryRun1, FreeMemoryLastRun : Integer;
 AllocSize, RunNo : Integer;
 AllocSizeFP : Double;
const
 NOOFPOINTERS : Integer = 1000000;
 ALLOCGROWSTEPSIZE : Double = 0.001;
 SLEEPTIMEAFTERFREE : Integer = 10;//Seconds to free
 SLACK : Integer = 100*1024;

begin
 try
  FreeMemoryRun1 := 0;//For compiler 
  for RunNo := 1 to 2 do
   begin
    Result := True;
    //Allocate
    SetLength(PointerArray, NOOFPOINTERS);
    AllocSizeFP := 1;
    for I:= 0 to Length(PointerArray)-1 do
     begin
      AllocSizeFP := AllocSizeFP + ALLOCGROWSTEPSIZE;
      AllocSize := Round(AllocSizeFP);
      GetMem(PointerArray[I], AllocSize);
     end;
    //Free
    for I:= 0 to Length(PointerArray)-1 do
     FreeMem(PointerArray[I]);
    SetLength(PointerArray, 0);
    //Give a little time to free
    Sleep(SLEEPTIMEAFTERFREE*1000);
    GlobalMemoryStatus(MemoryStatus);
    if RunNo = 1 then
     FreeMemoryRun1 := MemoryStatus.dwAvailVirtual
    else
     begin
      FreeMemoryLastRun := MemoryStatus.dwAvailVirtual;
      //Did memory usage increase?
      if FreeMemoryLastRun < (FreeMemoryRun1 - SLACK) then
       Result := False;
     end;
   end;
 except
  Result := False;
 end;
end;

//Based on Validate11 but validating big array downsize combined with many small strings resize

function TMMValidation.Validate26 : Boolean;
var
 I, J, K, L, I2, NoOfStrings  : Cardinal;
 SomeArray : array of Cardinal;
 StringArray : array[1..10000] of Ansistring;
 TempS : AnsiString;
 MemoryStatus : TMemoryStatus;
 SomeArraySize : Cardinal;
 SomeArraySizeFP : Double;
const
 NOOFRUNS : Integer = 2000;
 SOMEARRAYMAXSIZE : Integer = 50*1024*1024;
 SHRINKFACTOR : Double = 0.95;
begin
 Result := True;
 GlobalMemoryStatus(MemoryStatus);
 try
  SomeArraySize := SOMEARRAYMAXSIZE;
  SomeArraySizeFP := SomeArraySize;
  if MemoryStatus.dwAvailVirtual >= SomeArraySize then
   SetLength(SomeArray, SomeArraySize);
  //Fill SomeArray
  for I2 := 0 to SomeArraySize-1 do
   SomeArray[I2] := I2;
  for I := 0 to NOOFRUNS-1 do
   begin
    NoOfStrings := Length(StringArray);
    //Clear a string sometimes
    if I mod 5 = 0 then
     StringArray[Random(NoOfStrings) + 1] := '';
    //Grow a string with an 'A'
    StringArray[(I mod NoOfStrings) + 1] := StringArray[(I mod Cardinal(Length(StringArray)))+1] + 'A';
    //Shrink SomeArray
    SomeArraySizeFP := SomeArraySizeFP * SHRINKFACTOR;
    SomeArraySize := Round(SomeArraySizeFP);
    //SomeArray length always > 0
    if SomeArraySize <= 0 then
     SomeArraySize := 1;
    SetLength(SomeArray, SomeArraySize);
    //Validate that SomeArray data are not changed
    for J := 0 to SomeArraySize-1 do
     begin
      if SomeArray[J] <> J then
       begin
        Result := False;
        Exit;
       end;
     end;
    //Validate that all strings contains 'A's and nothing else
    for K := 1 to Length(StringArray) do
     begin
      TempS := StringArray[K];
      for L := 1 to Length(TempS) do
       begin
        if TempS[L] <> 'A' then
         begin
          Result := False;
          Exit;
         end;
       end;
     end;
   end;
 except
  Result := False;
 end;
end;

function TMMValidation.Validate27 : Boolean;
const
  ITERATIONS = 100;
  DLL_FUNCTIONNAME = 'LeakSomeMemory';
  DLL_BASENAME = 'MMUsageDLL';
var
  Handle: integer;
  LInitialUsage: Cardinal;
  ExportedMethod: TExportedMethod;
  LFileName: string;
begin
  {Get the initial memory usage}
  LInitialUsage := GetAddressSpaceUsed;
  {Default to fail}
  Result := False;
  {Build the DLL filename}
  LFileName := Format('%s_%s_%s.dll', [DLL_BASENAME, GetCompilerAbbr, GetMMName]);
  {Load the DLL}
  Handle := LoadLibrary(PChar(LFileName));
  if Handle = 0 then
  begin
    // ShowMessage('Cannot Load DLL ' + LFileName);
    Exit;
  end;
  {Get the exported procedure}
  ExportedMethod := GetProcAddress(Handle, DLL_FUNCTIONNAME);
  if @ExportedMethod = nil then
  begin
    // ShowMessage('Cannot Find ' + DLL_FUNCTIONNAME);
    FreeLibrary(Handle);
    Exit;
  end;
  {Run the exported method}
  ExportedMethod;
  {Free the library}
  FreeLibrary(Handle);
  {Was the memory leak in the DLL corrected? Allow maximum increase of 256K}
  if GetAddressSpaceUsed - LInitialUsage < 4 then
    Result := True;
end;

//****************************************************************************
//Multithreadvalidation
//****************************************************************************

type

  TMultiThreadValidateThread = class(TThread)
  public
   function GetPassed : Boolean;
   procedure Execute; override;
  private
   Passed : Boolean;
   function MultiThreadValidate1 : Boolean;
   function MultiThreadValidate2 : Boolean;
   function MultiThreadValidate3 : Boolean;
   function MultiThreadValidate4 : Boolean;
   function MultiThreadValidate5 : Boolean;
   function MultiThreadValidate6 : Boolean;
  end;

function TMultiThreadValidateThread.GetPassed : Boolean;
begin
 Result := Passed;
end;

function TMultiThreadValidateThread.MultiThreadValidate1 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
const
 BYTESTOALLOCATE : Cardinal = 100*1024*1024;//100 MB

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATE then
   SetLength(SomeArray, BYTESTOALLOCATE);
 except
  Result := False;
 end;
end;

function TMultiThreadValidateThread.MultiThreadValidate2 : Boolean;
var
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
 RunNo : Cardinal;
 SomeArraySize : Integer;
const
 BYTESTOALLOCATEMAX : Cardinal = 100000;//
 NOOFRUNS : Integer = 10000;
 STEPSIZE : Integer = 1024;

begin
 GlobalMemoryStatus(MemoryStatus);
 try
  Result := True;
  if MemoryStatus.dwAvailVirtual >= BYTESTOALLOCATEMAX then
   SetLength(SomeArray, BYTESTOALLOCATEMAX);
  for RunNo := 1 to NOOFRUNS do
   begin
    SomeArraySize := BYTESTOALLOCATEMAX - RunNo * Trunc(BYTESTOALLOCATEMAX / NOOFRUNS);
    //Downsize
    SetLength(SomeArray, SomeArraySize);
    //Upsize
    SetLength(SomeArray, SomeArraySize + STEPSIZE);
   end;
 except
  Result := False;
 end;
end;

//Reallocate one dynamic array

function TMultiThreadValidateThread.MultiThreadValidate3 : Boolean;
var
 Size, RunNo : Cardinal;
 SomeArray : array of Byte;
 MemoryStatus : TMemoryStatus;
const
 MAXSIZE : Cardinal = 10000;
 RUNNOMAX : Integer = 10;

begin
 try
  Result := True;
  for RunNo := 1 to RUNNOMAX do
   begin
    GlobalMemoryStatus(MemoryStatus);
    Size := Random(MAXSIZE);
    if MemoryStatus.dwAvailVirtual >= Size then
     SetLength(SomeArray, Size);
   end;
  //"Free"
  SetLength(SomeArray, 0);
 except
  Result := False;
 end;
end;

function TMultiThreadValidateThread.MultiThreadValidate4 : Boolean;
var
 Size, StringNo, I : Cardinal;
 StringArray : array of AnsiString;
 MemoryStatus : TMemoryStatus;
const
 MAXSIZE : Integer = 25;
 NOOFSTRINGS : Integer = 1000000;

begin
 try
  Result := True;
  SetLength(StringArray, NOOFSTRINGS);
  for StringNo := 0 to NOOFSTRINGS-1 do
   begin
    GlobalMemoryStatus(MemoryStatus);
    Size := Random(MAXSIZE);
    if MemoryStatus.dwAvailVirtual >= Size then
     begin
      SetLength(StringArray[StringNo], Size);
      //Fill string
      for I := 1 to Size do
       StringArray[StringNo][I] := 'A';
     end
   end;
  //Validate string content for all strings
  for StringNo := 0 to NOOFSTRINGS-1 do
   begin
    for I := 1 to Length(StringArray[StringNo]) do
     begin
      if StringArray[StringNo][I] <> 'A' then
       begin
        Result := False;
        Break;
       end;
     end;
   end;
  //"Free" all strings
  for StringNo := 0 to NOOFSTRINGS-1 do
   SetLength(StringArray[StringNo], 0);
  SetLength(StringArray, 0);
 except
  Result := False;
 end;
end;

function TMultiThreadValidateThread.MultiThreadValidate5 : Boolean;
var
 Size : Cardinal;
 StringNo, I, RunNo : Cardinal;
 StringArray : array of AnsiString;
 MemoryStatus : TMemoryStatus;
const
 MAXRUNNO : Integer = 200;
 MINSIZE  : Cardinal = 10;
 MAXSIZE : Cardinal = 1000;
 NOOFSTRINGS : Integer = 10000;

begin
 try
  Result := True;
  SetLength(StringArray, NOOFSTRINGS);
  for RunNo := 1 to MAXRUNNO do
   begin
    for StringNo := 0 to NOOFSTRINGS-1 do
     begin
      GlobalMemoryStatus(MemoryStatus);
      Size := Cardinal(Random(MAXSIZE - MINSIZE)) + MINSIZE;
      if MemoryStatus.dwAvailVirtual >= Size then
       begin
        SetLength(StringArray[StringNo], Size);
        //Fill string up to MINSIZE in round 1
        if RunNo = 1 then
         begin
          for I := 1 to MINSIZE do
           StringArray[StringNo][I] := 'A';
         end
        else
         begin
          //Fill string from MINSIZE to Size in remaining rounds
          if Size > MINSIZE + 1 then
           begin
            for I := MINSIZE + 1 to Size do
             StringArray[StringNo][I] := 'X';
           end;
         end;
       end
     end;
    //Validate string content in low part up to MINSIZE for all strings
    for StringNo := 0 to NOOFSTRINGS-1 do
     begin
      for I := 1 to MINSIZE do
       begin
        if StringArray[StringNo][I] <> 'A' then
         begin
          Result := False;
          Break;
         end;
       end;
     end;
   end;
  //"Free" all strings
  for StringNo := 0 to NOOFSTRINGS-1 do
   SetLength(StringArray[StringNo], 0);
  //"Free" StringArray
  SetLength(StringArray, 0);
 except
  Result := False;
 end;
end;

function TMultiThreadValidateThread.MultiThreadValidate6 : Boolean;
var
 Size, StringNo, I, RunNo : Cardinal;
 StringArray : array of AnsiString;
 MemoryStatus : TMemoryStatus;
const
 MAXRUNNO : Integer = 20;
 MINSIZE  : Cardinal = 1024;
 MAXSIZE : Cardinal = 10*1024*1024;
 NOOFSTRINGS : Integer = 20;

begin
 try
  Result := True;
  SetLength(StringArray, NOOFSTRINGS);
  for RunNo := 1 to MAXRUNNO do
   begin
    for StringNo := 0 to NOOFSTRINGS-1 do
     begin
      GlobalMemoryStatus(MemoryStatus);
      Size := Cardinal(Random(MAXSIZE - MINSIZE)) + MINSIZE;
      if MemoryStatus.dwAvailVirtual >= Size then
       begin
        SetLength(StringArray[StringNo], Size);
        //Fill string up to MINSIZE in round 1
        if RunNo = 1 then
         begin
          for I := 1 to MINSIZE do
           StringArray[StringNo][I] := 'A';
         end
        else
         begin
          //Fill string from MINSIZE to Size in remaining rounds
          if Size > MINSIZE + 1 then
           begin
            for I := MINSIZE + 1 to Size do
             StringArray[StringNo][I] := 'X';
           end;  
         end;
       end
     end;
    //Validate string content in low part up to MINSIZE for all strings
    for StringNo := 0 to NOOFSTRINGS-1 do
     begin
      for I := 1 to MINSIZE do
       begin
        if StringArray[StringNo][I] <> 'A' then
         begin
          Result := False;
          Break;
         end;
       end;
     end;
   end;
  //"Free" all strings
  for StringNo := 0 to NOOFSTRINGS-1 do
   SetLength(StringArray[StringNo], 0);
  //"Free" StringArray
  SetLength(StringArray, 0);
 except
  Result := False;
 end;
end;

procedure TMultiThreadValidateThread.Execute;
begin
 Passed := MultiThreadValidate1
       and MultiThreadValidate2
       and MultiThreadValidate3
       and MultiThreadValidate4
       and MultiThreadValidate5
       and MultiThreadValidate6;
end;

function TMMValidation.Validate25 : Boolean;
var
 MultiThreadValidateThread1 : TMultiThreadValidateThread;
 MultiThreadValidateThread2 : TMultiThreadValidateThread;
 MultiThreadValidateThread3 : TMultiThreadValidateThread;
 MultiThreadValidateThread4 : TMultiThreadValidateThread;
 Result1, Result2, Result3, Result4 : Boolean;

begin
 MultiThreadValidateThread1 := TMultiThreadValidateThread.Create(True);
 MultiThreadValidateThread2 := TMultiThreadValidateThread.Create(True);
 MultiThreadValidateThread3 := TMultiThreadValidateThread.Create(True);
 MultiThreadValidateThread4 := TMultiThreadValidateThread.Create(True);
 MultiThreadValidateThread1.FreeOnTerminate := False;
 MultiThreadValidateThread2.FreeOnTerminate := False;
 MultiThreadValidateThread3.FreeOnTerminate := False;
 MultiThreadValidateThread4.FreeOnTerminate := False;
 MultiThreadValidateThread1.Priority := tpLowest;
 MultiThreadValidateThread2.Priority := tpLower;
 MultiThreadValidateThread3.Priority := tpNormal;
 MultiThreadValidateThread4.Priority := tpHighest;
 MultiThreadValidateThread1.Resume;
 Sleep(5000);
 MultiThreadValidateThread2.Resume;
 Sleep(5000);
 MultiThreadValidateThread3.Resume;
 Sleep(5000);
 MultiThreadValidateThread4.Resume;
 MultiThreadValidateThread1.WaitFor;
 MultiThreadValidateThread2.WaitFor;
 MultiThreadValidateThread3.WaitFor;
 MultiThreadValidateThread4.WaitFor;
 Result1 := MultiThreadValidateThread1.GetPassed;
 Result2 := MultiThreadValidateThread2.GetPassed;
 Result3 := MultiThreadValidateThread3.GetPassed;
 Result4 := MultiThreadValidateThread4.GetPassed;
 MultiThreadValidateThread1.Free;
 MultiThreadValidateThread2.Free;
 MultiThreadValidateThread3.Free;
 MultiThreadValidateThread4.Free;
 Result := Result1 and Result2 and Result3 and Result4;
end;

function TMMValidation.Validate28: boolean;
const
  ThreadCount = 6;
var
  LThreads: array[0..ThreadCount - 1] of TValidate28Thread;
  LInd: integer;
begin
  Result := True;
  {Create the threads}
  for LInd := 0 to ThreadCount - 1 do
  begin
    LThreads[LInd] := TValidate28Thread.Create(False);
  end;
  {Wait for all threads to finish}
  for LInd := 0 to ThreadCount - 1 do
  begin
    LThreads[LInd].WaitFor;
    Result := Result and Boolean(LThreads[LInd].ReturnValue);
    LThreads[LInd].Free;
  end;
end;

{ TValidate28Thread }

procedure TValidate28Thread.Execute;
const
  NumStrings = 256;
  TestCount = 5000;
  MaxLength = 256 * 1024;
var
  LStrings: array[0..NumStrings - 1] of String;
  i, LTestNo, LIndex, LOldSize, LNewSize: integer;
  LPCheck: PAnsiChar;
begin
  {All ok so far}
  ReturnValue := ord(True);
  {Run the loop}
  for LTestNo := 1 to TestCount do
  begin
    {Reallocate a string}
    LIndex := Random(NumStrings);
    LOldSize := Length(LStrings[LIndex]);
    LNewSize := Random(MaxLength);
    SetLength(LStrings[LIndex], LNewSize);
    {Check that the contents was unchanged}
    LPCheck := Pointer(LStrings[LIndex]);
    for i := 1 to Min(LOldSize, LNewSize) do
    begin
      if LPCheck^ <> ansichar(LIndex) then
      begin
        ReturnValue := ord(False);
        exit;
      end;
      Inc(LPCheck);
    end;
    {Fill the newly allocated area}
    FillChar(PAnsiChar(Integer(LStrings[LIndex]) + LOldSize)^, LNewSize - LOldSize, Byte(LIndex));
  end;
end;

end.
