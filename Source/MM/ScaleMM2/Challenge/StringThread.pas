{****************************************************************************************

   StringThread usede by StringTestBenchMark & ManyThreadsTestBenchMark

   By Ivo Tops for FastCode Memory Manager BenchMark & Validation

****************************************************************************************}
unit StringThread;

interface

uses
   Classes, windows, sysutils;

const
   cRandomSizes = False;

type
   TStringThread = class(TThread)
   private
     FStringItems: Integer;
     FValidate: Boolean;
     FIterations: Integer;
     FSize:Integer;
   protected
     procedure StringAction;
   public
     constructor Create(AIterations: Integer; AItems: Integer; 
       AItemSize:Integer;AValidate: Boolean); reintroduce;
     procedure Execute; override;
   end;

type TLargeByteArray = array[0..MaxInt - 1] of Byte;

procedure FillPattern(const Dest: Pointer; const Size: Integer; const
StartChar: Byte);
function CheckPattern(const Dest: Pointer; const Size: Integer; const
StartChar: Byte): Boolean;

implementation

uses StringThreadTestUnit;

constructor TStringThread.Create(AIterations: Integer; AItems: Integer;
AItemSize:Integer;AValidate: Boolean);
begin
   inherited Create(False);
   FreeOnTerminate := True;
   IncRunningThreads;
   FStringItems := AItems;
   FValidate := AValidate;
   FIterations := AIterations;
   FSize:=AItemSize;
end;

procedure TStringThread.Execute;
var I: Integer;
begin
   try
     for I := 0 to FIterations - 1 do StringAction;
   except
    // Notify TestUnit we had a failure
    NotifyThreadError;
   end;
   DecRunningThreads;
end;

procedure TStringThread.StringAction;
var I: Integer;
   B1, B2: Integer;
   FCB: Byte;
   FillLen: Integer;
   A, B: array of string;
begin
   SetLength(A, FStringItems);
   SetLength(B, FStringItems);
   if cRandomSizes then
   begin
     B1 := Random(FSize) + 1;
     B2 := Random(FSize) + 1;
   end else
   begin
     B1 := FSize;
     B2 := FSize div 2;
   end;
   for I := 0 to FStringItems - 1 do
   begin
     SetLength(A[I], B1);
     if FValidate then
     begin
       FCB := Byte((I mod 250) + 1);
       FillPattern(PAnsiChar(A[I]), B1, FCB);
     end;
   end;
   // Reference counter, no copy
   for I := FStringItems - 1 downto 0 do
     B[I] := A[I];
   // Copy resizing
   for I := 0 to FStringItems - 1 do
     SetLength(B[I], B2);
   // Validate and CleanUp
   for I := FStringItems - 1 downto 0 do
   begin
     if FValidate then
     begin
       FCB := Byte((I mod 250) + 1);
       FillLen := length(A[I]);
       if not CheckPattern(PAnsiChar(A[I]), FillLen, FCB) then
       begin
        NotifyValidationError;
        Exit;
       end;
       if length(B[I]) < FillLen then FillLen := Length(B[I]);
       if not CheckPattern(PAnsiChar(B[I]), FillLen, FCB) then
             begin
        NotifyValidationError;
        Exit;
       end;
     end;
     B[I] := EmptyStr; // Cleanup
     A[I] := EmptyStr;
   end;
end;

// Fill Memory with a Pattern
procedure FillPattern(const Dest: Pointer; const Size: Integer; const
StartChar: Byte);
var I: Integer;
   PC: Byte;
begin
     // Write a three byte pattern starting with the byte passed
   PC := 0;
   for I := 0 to Size - 1 do
   begin
     TLargeByteArray(Dest^)[I] := StartChar + PC;
     Inc(PC);
     if PC = 3 then PC := 0;
   end;
end;

// Check memory for correct Pattern
function CheckPattern(const Dest: Pointer; const Size: Integer; const 
StartChar: Byte): Boolean;
var I: Integer;
   PC: Byte;
begin
     // Check a three byte pattern starting with the byte passed
   Result := True;
   PC := 0;
   for I := 0 to Size - 1 do
   begin
     if TLargeByteArray(Dest^)[I] <> StartChar + PC then
     begin
       Result := False;
       Break;
     end;
     Inc(PC);
     if PC = 3 then PC := 0;
   end;
end;

end.
