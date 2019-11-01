(*
  paper: Mersenne Twister: A 623-dimensionallyequidistributed uniformpseudorandom number generator
  post by 2002

  reference material
  https://baike.baidu.com/item/%E6%A2%85%E6%A3%AE%E7%B4%A0%E6%95%B0
  https://baike.baidu.com/item/%E6%A2%85%E6%A3%AE%E6%97%8B%E8%BD%AC%E7%AE%97%E6%B3%95
  https://www.cnblogs.com/lfri/p/11461695.html
  https://en.wikipedia.org/wiki/Mersenne_twister
*)

{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
const
  MT19937N = 624;
  MT19937M = 397;

type
  TMT19937Core = record
    MT: array [0 .. MT19937N - 1] of Integer; // the array for the state vector
    MTI: Integer;
    InternalRndSeed, InternalOldRndSeed: Cardinal;
    ThreadID: TThreadID;
    LastActivtedTime: TTimeTick;
    Busy: Boolean;
    Instance: Integer;
    procedure BuildMT(Seed_: Cardinal);
    function GenRand_MT19937(): Integer;
    procedure Init(ThreadID_: TThreadID; LastActivtedTime_: TTimeTick);
  end;

  PMD19937Core = ^TMT19937Core;

  TMT19937List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PMD19937Core>;

  TMT19937List = class(TMT19937List_Decl)
  end;

  { Initializing the array with a seed }
procedure TMT19937Core.BuildMT(Seed_: Cardinal);
var
  i: Integer;
begin
  MT[0] := Integer(Seed_);
  for i := 1 to MT19937N - 1 do
    begin
      MT[i] := 1812433253 * (MT[i - 1] xor (MT[i - 1] shr 30)) + i;
      { See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. }
      { In the previous versions, MSBs of the seed affect }
      { only MSBs of the array mt[]. }
      { 2002/01/09 modified by Makoto Matsumoto }
    end;
  MTI := MT19937N;
end;

function TMT19937Core.GenRand_MT19937(): Integer;
const
  Mag01: array [0 .. 1] of Integer = (0, Integer($9908B0DF));
  MT19937UPPER_MASK = Integer($80000000); // most significant r/w bits
  MT19937LOWER_MASK = Integer($7FFFFFFF); // least significant r bits
  TEMPERING_MASK_B = Integer($9D2C5680);
  TEMPERING_MASK_C = Integer($EFC60000);
var
  th: TCoreClassThread;
  Y, K: Integer;
begin
  if InternalRndSeed <> InternalOldRndSeed then
      MTI := MT19937N + 1;

  { generate MT19937N longints at one time }
  if (MTI >= MT19937N) then
    begin
      { if BuildMT() has not been called }
      if MTI = (MT19937N + 1) then
        begin
          { default initial seed is used }
          BuildMT(InternalRndSeed);

          { hack: InternalRndSeed is not used more than once in this algorithm. Most }
          { user changes are re-initialising reandseed with the value it had }
          { at the start -> with the "not", we will detect this change. }
          { Detecting other changes is not useful, since the generated }
          { numbers will be different anyway. }
          InternalRndSeed := not(InternalRndSeed);
          InternalOldRndSeed := InternalRndSeed;
        end;

      for K := 0 to MT19937N - MT19937M - 1 do
        begin
          Y := (MT[K] and MT19937UPPER_MASK) or (MT[K + 1] and MT19937LOWER_MASK);
          MT[K] := MT[K + MT19937M] xor (Y shr 1) xor Mag01[Y and $00000001];
        end;

      for K := MT19937N - MT19937M to MT19937N - 2 do
        begin
          Y := (MT[K] and MT19937UPPER_MASK) or (MT[K + 1] and MT19937LOWER_MASK);
          MT[K] := MT[K + (MT19937M - MT19937N)] xor (Y shr 1) xor Mag01[Y and $00000001];
        end;

      Y := (MT[MT19937N - 1] and MT19937UPPER_MASK) or (MT[0] and MT19937LOWER_MASK);
      MT[MT19937N - 1] := MT[MT19937M - 1] xor (Y shr 1) xor Mag01[Y and $00000001];
      MTI := 0;
    end;

  Y := MT[MTI];
  inc(MTI);
  Y := Y xor (Y shr 11);
  Y := Y xor (Y shl 7) and TEMPERING_MASK_B;
  Y := Y xor (Y shl 15) and TEMPERING_MASK_C;
  Y := Y xor (Y shr 18);
  Result := Y;
end;

procedure TMT19937Core.Init(ThreadID_: TThreadID; LastActivtedTime_: TTimeTick);
begin
  InternalRndSeed := 0;
  InternalOldRndSeed := 0;
  BuildMT(0);
  ThreadID := ThreadID_;
  LastActivtedTime := LastActivtedTime_;
  Busy := False;
  Instance := 0;
end;

var
  MT19937InternalCritical: TCritical;
  MT19937POOL: TMT19937List;

function MT19937(): PMD19937Core;
var
  tk: TTimeTick;
  thID: TThreadID;
  i: Integer;
  p: PMD19937Core;
begin
  MT19937InternalCritical.Acquire;
  try
    Result := nil;
    tk := GetTimeTick;
    thID := TCoreClassThread.CurrentThread.ThreadID;
    i := 0;
    while i < MT19937POOL.Count do
      begin
        p := MT19937POOL[i];
        if p^.ThreadID = thID then
          begin
            if i > 0 then
                MT19937POOL.Exchange(0, i);
            p^.LastActivtedTime := tk;
            Result := p;
            inc(i);
          end
        else if (not p^.Busy) and (p^.Instance <= 0) and (tk - p^.LastActivtedTime > MT19937LifeCycle) then
          begin
            dispose(p);
            MT19937POOL.Delete(i);
          end
        else
            inc(i);
      end;
    if Result = nil then
      begin
        New(p);
        p^.Init(thID, tk);
        MT19937POOL.Add(p);
        Result := p;
      end;
  finally
      MT19937InternalCritical.Release;
  end;
end;

{$IFDEF DELPHI}


function DelphiRandom32Proc: UInt32;
begin
  Result := UInt32(MT19937()^.GenRand_MT19937());
end;

procedure DelphiRandomizeProc(NewSeed: UInt64);
begin
  MT19937()^.InternalRndSeed := Cardinal(NewSeed);
end;

procedure MT19937Install();
begin
  Random32Proc := DelphiRandom32Proc;
  RandomizeProc := DelphiRandomizeProc;
end;

{$ENDIF DELPHI}


procedure InitMT19937Rand;
begin
  MT19937InternalCritical := TCritical.Create;
  MT19937POOL := TMT19937List.Create;

{$IFDEF DELPHI}
{$IFDEF InstallMT19937CoreToDelphi}
  MT19937Install();
{$ENDIF InstallMT19937CoreToDelphi}
{$ENDIF DELPHI}
end;

procedure FreeMT19937Rand;
var
  i: Integer;
begin
  for i := 0 to MT19937POOL.Count - 1 do
      dispose(MT19937POOL[i]);
  DisposeObject(MT19937POOL);
  MT19937POOL := nil;
  MT19937InternalCritical.Free;
  MT19937InternalCritical := nil;
end;

function MT19937InstanceNum(): Integer;
begin
  MT19937();
  MT19937InternalCritical.Acquire;
  Result := MT19937POOL.Count;
  MT19937InternalCritical.Release;
end;

procedure SetMT19937Seed(seed: Integer);
begin
  with MT19937()^ do
    begin
      InternalRndSeed := seed;
      InternalOldRndSeed := seed;
      BuildMT(seed);
    end;
end;

function GetMT19937Seed(): Integer;
begin
  Result := MT19937()^.InternalRndSeed;
end;

procedure MT19937Randomize();
begin
  MT19937()^.InternalRndSeed := GetTimeTick;
end;

function MT19937Rand32(L: Integer): Integer;
begin
  { otherwise we can return values = L (JM) }
  if (L < 0) then
      inc(L);
  Result := Integer((Int64(Cardinal(MT19937()^.GenRand_MT19937())) * L) shr 32);
end;

procedure MT19937Rand32(L: Integer; dest: PInteger; num: NativeInt);
begin
  { otherwise we can return values = L (JM) }
  if (L < 0) then
      inc(L);

  with MT19937()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := Integer((Int64(Cardinal(GenRand_MT19937())) * L) shr 32);
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

function MT19937Rand64(L: Int64): Int64;
begin
  { always call random, so the random generator cycles (TP-compatible) (JM) }
  with MT19937()^ do
      Result := Int64((UInt64(Cardinal(GenRand_MT19937())) or ((UInt64(Cardinal(GenRand_MT19937())) shl 32))) and $7FFFFFFFFFFFFFFF);
  if (L <> 0) then
      Result := Result mod L
  else
      Result := 0;
end;

procedure MT19937Rand64(L: Int64; dest: PInt64; num: NativeInt);
begin
  with MT19937()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := Int64((UInt64(Cardinal(GenRand_MT19937())) or ((UInt64(Cardinal(GenRand_MT19937())) shl 32))) and $7FFFFFFFFFFFFFFF);
            if (dest^ <> 0) then
                dest^ := dest^ mod L
            else
                dest^ := 0;
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

function MT19937RandE: Extended;
const
  f = Extended(1.0) / (Int64(1) shl 32);
begin
  Result := f * Cardinal(MT19937()^.GenRand_MT19937());
end;

procedure MT19937RandE(dest: PExtended; num: NativeInt);
const
  f = Extended(1.0) / (Int64(1) shl 32);
begin
  with MT19937()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := f * Cardinal(GenRand_MT19937());
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

function MT19937RandF: Single;
const
  f = Single(1.0) / (Int64(1) shl 32);
begin
  Result := f * Cardinal(MT19937()^.GenRand_MT19937());
end;

procedure MT19937RandF(dest: PSingle; num: NativeInt);
const
  f = Single(1.0) / (Int64(1) shl 32);
begin
  with MT19937()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := f * Cardinal(GenRand_MT19937());
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

function MT19937RandD: Double;
const
  f = Double(1.0) / (Int64(1) shl 32);
begin
  Result := f * Cardinal(MT19937()^.GenRand_MT19937());
end;

procedure MT19937RandD(dest: PDouble; num: NativeInt);
const
  f = Double(1.0) / (Int64(1) shl 32);
begin
  with MT19937()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := f * Cardinal(GenRand_MT19937());
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

{ ****************************************************************************** }
{ * TMT19937 classes                                                           * }
{ ****************************************************************************** }
function TMT19937Random.GetSeed: Integer;
begin
  with PMD19937Core(FRndInstance)^ do
      Result := InternalRndSeed;
end;

procedure TMT19937Random.SetSeed(const Value: Integer);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      InternalRndSeed := Value;
      InternalOldRndSeed := Value;
      BuildMT(Value);
    end;
end;

constructor TMT19937Random.Create;
begin
  inherited Create;
  FRndInstance := MT19937();
  AtomInc(PMD19937Core(FRndInstance)^.Instance);
end;

destructor TMT19937Random.Destroy;
begin
  AtomDec(PMD19937Core(FRndInstance)^.Instance);
  inherited Destroy;
end;

procedure TMT19937Random.Rndmize;
begin
  with PMD19937Core(FRndInstance)^ do
      InternalRndSeed := GetTimeTick;
end;

function TMT19937Random.Rand32(L: Integer): Integer;
begin
  { otherwise we can return values = L (JM) }
  if (L < 0) then
      inc(L);
  with PMD19937Core(FRndInstance)^ do
      Result := Integer((Int64(Cardinal(GenRand_MT19937())) * L) shr 32);
end;

procedure TMT19937Random.Rand32(L: Integer; dest: PInteger; num: NativeInt);
begin
  { otherwise we can return values = L (JM) }
  if (L < 0) then
      inc(L);

  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := Integer((Int64(Cardinal(GenRand_MT19937())) * L) shr 32);
          dec(num);
          inc(dest);
        end;
    end;
end;

function TMT19937Random.Rand64(L: Int64): Int64;
begin
  { always call random, so the random generator cycles (TP-compatible) (JM) }
  with PMD19937Core(FRndInstance)^ do
      Result := Int64((UInt64(Cardinal(GenRand_MT19937())) or ((UInt64(Cardinal(GenRand_MT19937())) shl 32))) and $7FFFFFFFFFFFFFFF);
  if (L <> 0) then
      Result := Result mod L
  else
      Result := 0;
end;

procedure TMT19937Random.Rand64(L: Int64; dest: PInt64; num: NativeInt);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := Int64((UInt64(Cardinal(GenRand_MT19937())) or ((UInt64(Cardinal(GenRand_MT19937())) shl 32))) and $7FFFFFFFFFFFFFFF);
          if (dest^ <> 0) then
              dest^ := dest^ mod L
          else
              dest^ := 0;
          dec(num);
          inc(dest);
        end;
    end;
end;

function TMT19937Random.RandE: Extended;
const
  f = Extended(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
      Result := f * Cardinal(GenRand_MT19937());
end;

procedure TMT19937Random.RandE(dest: PExtended; num: NativeInt);
const
  f = Extended(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := f * Cardinal(GenRand_MT19937());
          dec(num);
          inc(dest);
        end;
    end;
end;

function TMT19937Random.RandF: Single;
const
  f = Single(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
      Result := f * Cardinal(GenRand_MT19937());
end;

procedure TMT19937Random.RandF(dest: PSingle; num: NativeInt);
const
  f = Single(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := f * Cardinal(GenRand_MT19937());
          dec(num);
          inc(dest);
        end;
    end;
end;

function TMT19937Random.RandD: Double;
const
  f = Double(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
      Result := f * Cardinal(GenRand_MT19937());
end;

procedure TMT19937Random.RandD(dest: PDouble; num: NativeInt);
const
  f = Double(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := f * Cardinal(GenRand_MT19937());
          dec(num);
          inc(dest);
        end;
    end;
end;
