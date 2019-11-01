procedure FPCParallelFor(const OnFor: TFPCParallelForProcedure; const b, e: NativeInt);
var
  pass: NativeInt;
begin
  for pass := b to e do
      OnFor(pass);
end;
