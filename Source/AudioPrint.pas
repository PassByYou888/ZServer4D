{ ****************************************************************************** }
{ * Audio Fingerprint support                                                  * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit AudioPrint;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, DoStatusIO, UnicodeMixedLib, LearnTypes, Learn;

type
  TInt16 = SmallInt;
  TUInt32 = Cardinal;
  TU32Vec = array of TUInt32;
  TI16Vec = array of TInt16;
  TBytes = array of Byte;
  TAudioprintAlgorithm = (Audioprint_ALGORITHM_TEST1, Audioprint_ALGORITHM_TEST2, Audioprint_ALGORITHM_TEST3, Audioprint_ALGORITHM_TEST4);

  TAudioprint = class(TInterfacedObject)
  private type
    TAudioprintContext = record
      algorithm: TAudioprintAlgorithm;
      Engine: TObject;
      fingerprint: TU32Vec;
    end;
  public
    ctx: TAudioprintContext;

    constructor Create;
    destructor Destroy; override;

    function New(algorithm: TAudioprintAlgorithm): boolean;
    function GetAlgorithm: TAudioprintAlgorithm;
    function SetOption(const Name: TPascalString; Value: TLInt): boolean;
    function Start(Sample_Rate: TLInt; NumChannels: TLInt): boolean;
    function Feed(Data: TI16Vec; len: TLInt): boolean;
    function Finish: boolean;
    function GetFingerprint(out fingerprint: TPascalString): boolean;
    function GetRawFingerprint(out fingerprint: TU32Vec; out Size: TLInt): boolean;

    procedure EnocdeFingerprint(RawFP: TU32Vec; algorithm: TLInt; var EncodedFP: TPascalString; var EncodedSize: TLInt; Base64: boolean);
    procedure DecodeFingerprint(encoded: TPascalString; var uncompressed: TU32Vec; var algorithm: TLInt; Base64: boolean);
  end;

implementation

uses Math;

const
  FILTER_SHIFT = 15;
  WINDOW_TYPE = 9;

  kMinSampleRate = 1000;
  kMaxBufferSize = 1024 * 16;

  // Resampler configuration
  kResampleFilterLength = 16;
  kResamplePhaseCount = 10;
  kResampleLinear = 0;
  kResampleCutoff = 0.8;

  NUM_BANDS = 12;

  cSAMPLE_RATE = 11025;
  cFRAME_SIZE = 4096;
  cOVERLAP = cFRAME_SIZE - cFRAME_SIZE div 3;
  cMIN_FREQ = 28;
  cMAX_FREQ = 3520;

  Math_Pi: TLFloat = 3.1415926535897931;
  TwoMath_Pi: TLFloat = 6.2831853071795862;

  CODES: array [0 .. 3] of Byte = (0, 1, 3, 2);

  kMaxNormalValue = 7;
  kNormalBits = 3;
  kExceptionBits = 5;
  kChromaFilterSize = 5;

  kSilenceWindow = 55; // 5 ms as 11025 Hz

function FreqToOctave(freq, base: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := log10(freq / base) / log10(2.0); // logarithmus dualis
end;

function IndexToFreq(i, frame_size, Sample_Rate: TLInt): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := i * Sample_Rate / frame_size;
end;

function FreqToIndex(freq: TLFloat; frame_size, Sample_Rate: TLInt): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := round(frame_size * freq / Sample_Rate);
end;

procedure NormalizeVector(var vector: TLVec; norm: TLFloat; Threshold: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  i: TLInt;
begin
  if (norm < Threshold) then
      LSetVec(vector, 0.0)
  else
    for i := 0 to Length(vector) - 1 do
        LDiv(vector[i], norm);
end;

function EuclideanNorm(vector: TLVec): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  squares, Value: TLFloat;
  i: TLInt;
begin
  squares := 0;
  for i := 0 to Length(vector) - 1 do
    begin
      Value := vector[i];
      squares := squares + Value * Value;
    end;
  if squares > 0 then
      Result := Sqrt(squares)
  else
      Result := 0;
end;

procedure PrepareHammingWindow(var Data: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  i, n: TLInt;
  scale: TLFloat;
begin
  n := Length(Data);
  scale := TwoMath_Pi / (n - 1);
  for i := 0 to n - 1 do
      Data[i] := 0.54 - 0.46 * cos(scale * i);
end;

procedure ApplyWindow(var Data: TLVec; window: TLVec; Size: TLInt; scale: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  i: TLInt;
begin
  i := 0;
  while (i < Size) do
    begin
      Data[i] := Data[i] * window[i] * scale;
      Inc(i);
    end;
end;

type
  TAudioConsumer = class(TCoreClassObject)
    procedure Consume(Input: TI16Vec; AOffset: TLInt; ALength: TLInt); virtual; abstract;
  end;

  TAudioBuffer = class(TAudioConsumer)
    FData: TI16Vec;
    constructor Create;
    destructor Destroy; override;
    procedure Consume(Input: TI16Vec; AOffset: TLInt; ALength: TLInt); override;
    function Data: TI16Vec;
    procedure Reset;
  end;

  TAVResampleContext = record
    rFilterBank: TI16Vec;
    rFilterLength: TLInt;
    rIdealDstIncr: TLInt;
    rDstIncr: TLInt;
    rIndex: TLInt;
    rFrac: TLInt;
    rSrcIncr: TLInt;
    rCompensationDistance: TLInt;
    rPhaseShift: TLInt;
    rPhaseMask: TLInt;
    rLinear: TLInt;

    procedure Clear;

    procedure Init(out_rate, in_rate, filter_size, APhaseShift: TLInt; ALinear: TLInt; cutoff: TLFloat);
    function Resample(dst: TI16Vec; src: TI16Vec; var consumed: TLInt; src_size: TLInt; dst_size: TLInt; update_ctx: TLInt): TLInt;
  end;

  PAVResampleContext = ^TAVResampleContext;

  TBitStringWriter = class(TCoreClassObject)
    FValue: TPascalString;
    FBuffer: TUInt32;
    FBufferSize: TLInt;
    constructor Create;
    destructor Destroy; override;
    procedure Flush;
    procedure Write(x: TUInt32; bits: TLInt);
  end;

  TBitStringReader = class(TCoreClassObject)
    FValue: TPascalString;
    FValueIter: TLInt;
    FBuffer: TUInt32;
    FBufferSize: TLInt;
    FEOF: boolean;
    function GetAvailableBits: TLInt;
    property EOF: boolean read FEOF;
    property AvailableBits: TLInt read GetAvailableBits;
    constructor Create(Input: TPascalString);
    destructor Destroy; override;
    procedure Reset;
    function Read(bits: TLInt): TUInt32;
  end;

  TCPImage = class(TCoreClassObject)
    FColumns: TLInt;
    FRows: TLInt;
    FData: TLMatrix;
    property NumRows: TLInt read FRows;
    property NumColumns: TLInt read FColumns;
    constructor Create(columns: TLInt; rows: TLInt = 0);
    destructor Destroy; override;
    procedure AddRow(Row: TLVec);
    function GetData(Row, Column: TLInt): TLFloat;
    procedure SetData(Row, Column: TLInt; Value: TLFloat);
  end;

  TIntegralImage = class(TCoreClassObject)
    FImage: TCPImage;
    procedure Transform;
    { Construct the integral image. Note that will modify the original image in-place, so it will not be usable afterwards. }
    constructor Create(Image: TCPImage);
    destructor Destroy; override;
    function Area(x1, y1, x2, y2: TLInt): TLFloat;
    function NumColumns: TLInt;
    function NumRows: TLInt;
    function GetData(Row, Column: TLInt): TLFloat;
  end;

  TComparator = function(a, b: TLFloat): TLFloat;

  TFilter = class(TCoreClassObject)
    FType: TLInt;
    FY: TLInt;
    FHeight: TLInt;
    FWidth: TLInt;
    constructor Create(AType: TLInt; Y: TLInt; Height: TLInt; Width: TLInt);
    function Apply(Image: TIntegralImage; offset: TLInt): TLFloat;
  end;

  TQuantizer = class(TCoreClassObject)
    FT0, FT1, FT2: TLFloat;
    constructor Create(t0: TLFloat = 0.0; t1: TLFloat = 0.0; t2: TLFloat = 0.0);
    function Quantize(Value: TLFloat): TLInt;
  end;

  TClassifier = class(TCoreClassObject)
    FFilter: TFilter;
    FQuantizer: TQuantizer;
    constructor Create(Filter: TFilter = nil; Quantizer: TQuantizer = nil);
    destructor Destroy; override;
    function Classify(Image: TIntegralImage; offset: TLInt): TLInt;
  end;

  TClassifierArray = array of TClassifier;

  TCombinedBuffer = class(TCoreClassObject)
    FOffset: TLInt;
    FBuffer1, FBuffer2: TI16Vec;
    FSize1, FSize2: TLInt;
    constructor Create(const Buffer1: TI16Vec; Size1: TLInt; const Buffer2: TI16Vec; Size2: TLInt);
    destructor Destroy; override;
    function Size: TLInt;
    function GetValue(i: TLInt): TInt16;
    function Shift(Value: TLInt): TLInt;
    function Read(var Buffer: TI16Vec; offset: TLInt; Count: TLInt): TLInt;
    procedure Flush(var Buffer: TI16Vec);
  end;

  TFFTFrame = class(TCoreClassObject)
    FSize: TLInt;
    Data: TLVec;
    constructor Create(Size: TLInt);
    destructor Destroy; override;
    function Magnitude(i: TLInt): TLFloat;
    function Energy(i: TLInt): TLFloat;
  end;

  TFFTFrameConsumer = class(TCoreClassObject)
    procedure Consume(frame: TFFTFrame); virtual; abstract;
  end;

  TFFTLib = class(TCoreClassObject)
    FWindow: TLVec;
    FFrameSize: TLInt;
    FFrameSizeH: TLInt;
    constructor Create(frame_size: TLInt; window: TLVec);
    destructor Destroy; override;
    procedure ComputeFrame(Input: TI16Vec; var frame: TFFTFrame); virtual; abstract;
  end;

  TFFT = class(TAudioConsumer)
    FBufferOffset: TLInt;
    FWindow: TLVec;
    FFFTBuffer: TI16Vec;
    FFrame: TFFTFrame;
    FFrameSize: TLInt;
    FIncrement: TLInt;
    FLib: TFFTLib;
    FConsumer: TFFTFrameConsumer;
    constructor Create(frame_size: TLInt; overlap: TLInt; consumer: TFFTFrameConsumer);
    destructor Destroy; override;
    procedure Reset;
    procedure Consume(Input: TI16Vec; AOffset: TLInt; lLength: TLInt); override;
    function overlap: TLInt;
  end;

  TFFTLomont = class(TFFTLib)
    FInput: TLVec;
    ForwardCos: TLVec;
    ForwardSin: TLVec;
    procedure ComputeTable(Size: TLInt);
    procedure FFT(var Data: TLVec);
    procedure RealFFT(var Data: TLVec);
    constructor Create(frame_size: TLInt; window: TLVec);
    destructor Destroy; override;
    procedure ComputeFrame(Input: TI16Vec; var frame: TFFTFrame); override;
  end;

  TFeatureVectorConsumer = class(TCoreClassObject)
    constructor Create;
    destructor Destroy; override;
    procedure Consume(var features: TLVec); virtual; abstract;
  end;

  TImageBuilder = class(TFeatureVectorConsumer)
    FImage: TCPImage;
    constructor Create(Image: TCPImage = nil);
    destructor Destroy; override;
    procedure Reset(Image: TCPImage);
    procedure Consume(var features: TLVec); override;
  end;

  TChromaFilter = class(TFeatureVectorConsumer)
    FBufferOffset: TLInt;
    FBufferSize: TLInt;
    FConsumer: TFeatureVectorConsumer;
    FCoefficients: TLVec;
    FLength: TLInt;
    FBuffer: TLMatrix;
    FResult: TLVec;
    constructor Create(coefficients: TLVec; ALength: TLInt; consumer: TFeatureVectorConsumer);
    destructor Destroy; override;
    procedure Reset;
    procedure Consume(var features: TLVec); override;
  end;

  TChromaNormalizer = class(TFeatureVectorConsumer)
    FConsumer: TFeatureVectorConsumer;
    constructor Create(consumer: TFeatureVectorConsumer);
    destructor Destroy; override;
    procedure Reset;
    procedure Consume(var features: TLVec); override;
  end;

  TChromaResampler = class(TFeatureVectorConsumer)
    FResult: TLVec;
    FConsumer: TFeatureVectorConsumer;
    FIteration: TLInt;
    FFactor: TLInt;
    constructor Create(factor: TLInt; consumer: TFeatureVectorConsumer);
    destructor Destroy; override;
    procedure Reset;
    procedure Consume(var features: TLVec); override;
  end;

  TFingerprintCalculator = class(TCoreClassObject)
    FMaxFilterWidth: TLInt;
    FClassifiers: TClassifierArray;
    constructor Create(classifiers: TClassifierArray);
    destructor Destroy; override;
    function Calculate(Image: TCPImage): TU32Vec;
    function CalculateSubfingerprint(Image: TIntegralImage; offset: TLInt): TUInt32;
  end;

  TFingerprintCompressor = class(TCoreClassObject)
    FResult: TPascalString;
    FBits: TBytes;
    procedure WriteNormalBits();
    procedure WriteExceptionBits();
    procedure ProcessSubfingerprint(x: TUInt32);
    constructor Create;
    function Compress(var fingerprint: TU32Vec; algorithm: TLInt = 0): TPascalString;
  end;

  TFingerprintDecompressor = class(TCoreClassObject)
    FResult: TU32Vec;
    FBits: TBytes;
    function ReadNormalBits(reader: TBitStringReader): boolean;
    function ReadExceptionBits(reader: TBitStringReader): boolean;
    procedure UnpackBits();
    constructor Create;
    function Decompress(fingerprint: TPascalString; var algorithm: TLInt): TU32Vec;
  end;

  TFingerprinterConfiguration = class(TCoreClassObject)
    FNumClassifiers: TLInt;
    FClassifiers: TClassifierArray;
    FNumFilterCoefficients: TLInt;
    FFilterCoefficients: TLVec;
    FInterpolate: boolean;
    FRemoveSilence: boolean;
    FSilenceThreshold: TLInt;
    kChromaFilterCoefficients: TLVec;
    constructor Create;
    destructor Destroy; override;
    procedure SetClassifiers(classifiers: TClassifierArray);
    procedure SetFilterCoefficients(FilterCoefficients: TLVec);
  end;

  // Trained on a randomly selected test data
  TFingerprinterConfigurationTest1 = class(TFingerprinterConfiguration)
    constructor Create;
    destructor Destroy; override;
  end;

  // Trained on 60k pairs based on eMusic samples (mp3)
  TFingerprinterConfigurationTest2 = class(TFingerprinterConfiguration)
    constructor Create;
    destructor Destroy; override;
  end;

  // Trained on 60k pairs based on eMusic samples with interpolation enabled (mp3)
  TFingerprinterConfigurationTest3 = class(TFingerprinterConfiguration)
    constructor Create;
    destructor Destroy; override;
  end;

  // Same as v2, but trims leading silence
  TFingerprinterConfigurationTest4 = class(TFingerprinterConfigurationTest2)
    constructor Create;
    destructor Destroy; override;
  end;

  TMovingAverage = class(TCoreClassObject)
    FBuffer: TI16Vec;
    FSize: TLInt;
    FOffset: TLInt;
    FSum: TLInt;
    FCount: TLInt;
    constructor Create(Size: TLInt);
    destructor Destroy; override;
    procedure AddValue(const x: TInt16);
    function GetAverage: TInt16;
  end;

  TSilenceRemover = class(TAudioConsumer)
    FThreshold: TLInt;
    FStart: boolean;
    FConsumer: TAudioConsumer;
    FAverage: TMovingAverage;
    constructor Create(consumer: TAudioConsumer; Threshold: TLInt = 0);
    destructor Destroy; override;
    function Reset(Sample_Rate, NumChannels: TLInt): boolean;
    procedure Flush;
    procedure Consume(Input: TI16Vec; AOffset: TLInt; Length: TLInt); override;
  end;

  TAudioProcessor = class(TAudioConsumer)
    FBufferOffset: TLInt;
    FBufferSize: TLInt;
    FTargetSampleRate: TLInt;
    FNumChannels: TLInt;

    FResampleCTX: PAVResampleContext;
    FConsumer: TAudioConsumer;

    FBuffer: TI16Vec;
    FResampleBuffer: TI16Vec;

    procedure Resample;
    function Load(Input: TI16Vec; offset: TLInt; ALength: TLInt): TLInt;
    procedure LoadMono(Input: TI16Vec; offset: TLInt; ALength: TLInt);
    procedure LoadStereo(Input: TI16Vec; offset: TLInt; ALength: TLInt);
    procedure LoadMultiChannel(Input: TI16Vec; offset: TLInt; ALength: TLInt);

    constructor Create(SampleRate: TLInt; consumer: TAudioConsumer);
    destructor Destroy; override;
    function Reset(SampleRate, NumChannels: TLInt): boolean;
    procedure Flush;
    procedure Consume(Input: TI16Vec; AOffset: TLInt; ALength: TLInt); override;
  end;

  TChroma = class(TFFTFrameConsumer)
    FInterpolate: boolean;
    FMinIndex: TLInt;
    FMaxIndex: TLInt;

    FNotes: TBytes;
    FNotesFrac: TLVec;
    FFeatures: TLVec;

    FConsumer: TFeatureVectorConsumer;
    procedure PrepareNotes(min_freq, max_freq, frame_size, Sample_Rate: TLInt);

    constructor Create(min_freq, max_freq, frame_size, Sample_Rate: TLInt; consumer: TFeatureVectorConsumer);
    destructor Destroy; override;
    procedure Reset;
    procedure Consume(frame: TFFTFrame); override;
  end;

  TFingerprinter = class(TAudioConsumer)
    FSilenceRemover: TSilenceRemover;
    FImage: TCPImage;
    FImageBuilder: TImageBuilder;
    FChroma: TChroma;
    FChromaNormalizer: TChromaNormalizer;
    FChromaFilter: TChromaFilter;
    FFFT: TFFT;
    FAudioProcessor: TAudioProcessor;
    FFingerprintCalculator: TFingerprintCalculator;
    FConfig: TFingerprinterConfiguration;

    constructor Create(config: TFingerprinterConfiguration = nil);
    destructor Destroy; override;
    function SetOption(const Name: TPascalString; Value: TLInt): boolean;
    function Start(Sample_Rate, NumChannels: TLInt): boolean;
    function Finish: TU32Vec;
    procedure Consume(Input: TI16Vec; AOffset: TLInt; len: TLInt); override;
  end;

function CompressFingerprint(Data: TU32Vec; algorithm: TLInt = 0): TPascalString; forward;
function DecompressFingerprint(Data: TPascalString; var algorithm: TLInt): TU32Vec; forward;
function CreateFingerprinterConfiguration(algorithm: TAudioprintAlgorithm): TFingerprinterConfiguration; forward;

constructor TAudioBuffer.Create;
begin
  inherited Create;
  SetLength(FData, 0);
end;

destructor TAudioBuffer.Destroy;
begin
  SetLength(FData, 0);
  inherited Destroy;
end;

procedure TAudioBuffer.Consume(Input: TI16Vec; AOffset: TLInt; ALength: TLInt);
var
  i, n: TLInt;
begin
  n := Length(FData);
  SetLength(FData, n + ALength - AOffset);
  for i := AOffset to ALength - 1 do
    begin
      FData[n + i - AOffset] := Input[i];
    end;
end;

function TAudioBuffer.Data: TI16Vec;
begin
  Result := FData;
end;

procedure TAudioBuffer.Reset;
begin
  SetLength(FData, 0);
end;

function BuildFilter(var Filter: TI16Vec; factor: TLFloat; tap_count: TLInt; phase_count, scale, _Type: TLInt): boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ 0th order modified bessel function of the first kind. }

  function bessel(x: TLFloat): TLFloat; inline;
  var
    v, lastv, t, lx: TLFloat;
    i: TLInt;
  begin
    v := 1;
    lastv := 0;
    t := 1;

    lx := x * x / 4;
    i := 1;
    while v <> lastv do
      begin
        lastv := v;
        t := t * lx / (i * i);
        v := v + t;
        Inc(i);
      end;
    Result := v;
  end;

  function Clip16(a: TLInt; amin, amax: TInt16): TInt16; inline;
  begin
    if (a < amin) then
        Result := amin
    else if (a > amax) then
        Result := amax
    else
        Result := TInt16(a);
  end;

var
  ph, i, center: TLInt;
  x, Y, w: TLFloat;
  tab: array of TLFloat;
  norm: TLFloat;
  d: TLFloat;
  ex: TLInt;
begin

  SetLength(tab, tap_count);
  center := (tap_count - 1) div 2;

  // if upsampling, only need to interpolate, no filter
  if (factor > 1.0) then
      factor := 1.0;

  for ph := 0 to phase_count - 1 do
    begin
      norm := 0;
      for i := 0 to tap_count - 1 do
        begin
          x := Math_Pi * ((i - center) - ph / phase_count) * factor;
          if (x = 0) then
              Y := 1.0
          else
              Y := Sin(x) / x;
          case (_Type) of

            0:
              begin
                d := -0.5; // first order derivative = -0.5
                x := Abs(((i - center) - ph / phase_count) * factor);
                if (x < 1.0) then
                    Y := 1 - 3 * x * x + 2 * x * x * x + d * (-x * x + x * x * x)
                else
                    Y := d * (-4 + 8 * x - 5 * x * x + x * x * x);
              end;

            1:
              begin
                w := 2.0 * x / (factor * tap_count) + Math_Pi;
                Y := Y * (0.3635819 - 0.4891775 * cos(w) + 0.1365995 * cos(2 * w) - 0.0106411 * cos(3 * w));
              end;
            else
              begin
                w := 2.0 * x / (factor * tap_count * Math_Pi);
                Y := Y * (bessel(_Type * Sqrt(Max(1 - w * w, 0))));
              end;
          end;

          tab[i] := Y;
          norm := norm + Y;
        end;

      // normalize so that an uniform color remains the same
      for i := 0 to tap_count - 1 do
        begin
          ex := floor(tab[i] * scale / norm + 0.50000);
          Filter[ph * tap_count + i] := Clip16(ex, -32768, 32767);
        end;
    end;

  Result := True;
end;

procedure TAVResampleContext.Clear;
begin
  SetLength(rFilterBank, 0);

  rFilterLength := 0;
  rIdealDstIncr := 0;
  rDstIncr := 0;
  rIndex := 0;
  rFrac := 0;
  rSrcIncr := 0;
  rCompensationDistance := 0;
  rPhaseShift := 0;
  rPhaseMask := 0;
  rLinear := 0;
end;

procedure TAVResampleContext.Init(out_rate, in_rate, filter_size, APhaseShift: TLInt; ALinear: TLInt; cutoff: TLFloat);
var
  factor: TLFloat;
  phase_count: TLInt;
  r: boolean;
begin
  factor := min(out_rate * cutoff / in_rate, 1.0);
  phase_count := 1 shl APhaseShift;

  rPhaseShift := APhaseShift;
  rPhaseMask := phase_count - 1;
  rLinear := ALinear;

  rFilterLength := Max(ceil(filter_size / factor), 1);
  SetLength(rFilterBank, rFilterLength * (phase_count + 1));
  begin
    r := BuildFilter(rFilterBank, factor, rFilterLength, phase_count, (1 shl FILTER_SHIFT), WINDOW_TYPE);
    if (r) then
      begin
        CopyPtr(@rFilterBank[0], @rFilterBank[rFilterLength * phase_count + 1], (rFilterLength - 1) * sizeof(TInt16));
        rFilterBank[rFilterLength * phase_count] := rFilterBank[rFilterLength - 1];

        rSrcIncr := out_rate;
        rDstIncr := in_rate * phase_count;
        rIdealDstIncr := rDstIncr;
        rIndex := -phase_count * ((rFilterLength - 1) div 2);
      end;
  end;
end;

function TAVResampleContext.Resample(dst: TI16Vec; src: TI16Vec; var consumed: TLInt; src_size: TLInt; dst_size: TLInt; update_ctx: TLInt): TLInt;
var
  dst_index, i: TLInt;
  lIndex, lFrac, ldst_incr_frac, lDst_incr, lCompensationDistance: TLInt;

  lIndex2, lIncr: int64;

  lFilterOffset: TLInt;
  lSampleIndex, lVal, lV2: TLInt;
  lr: TLInt;
  lTempSrcIdx: TLInt;
begin
  lIndex := self.rIndex;
  lFrac := self.rFrac;
  ldst_incr_frac := self.rDstIncr mod self.rSrcIncr;
  lDst_incr := self.rDstIncr div self.rSrcIncr;
  lCompensationDistance := self.rCompensationDistance;

  if (lCompensationDistance = 0) and (self.rFilterLength = 1) and (self.rPhaseShift = 0) then
    begin
      lIndex2 := int64(lIndex) shl 32; // pascal is pain
      lIncr := (int64(1) shl 32) * self.rDstIncr div self.rSrcIncr;
      dst_size := min(dst_size, (src_size - 1 - lIndex) * self.rSrcIncr div self.rDstIncr);

      dst_index := 0;
      while dst_index < dst_size do
        begin
          dst[dst_index] := src[lIndex2 shr 32];
          Inc(lIndex2, lIncr);
          Inc(dst_index);
        end;

      Inc(lFrac, dst_size * ldst_incr_frac);
      Inc(lIndex, dst_size * lDst_incr + lFrac div self.rSrcIncr);
      lFrac := lFrac mod self.rSrcIncr;
    end
  else
    begin
      for dst_index := 0 to dst_size - 1 do
        begin
          lFilterOffset := self.rFilterLength * (lIndex and self.rPhaseMask);
          lr := Integer(int64(lIndex) shr self.rPhaseShift);

          lSampleIndex := lr;
          lVal := 0;

          if (lSampleIndex < 0) then
            begin
              for i := 0 to self.rFilterLength - 1 do
                begin
                  lTempSrcIdx := Abs(lSampleIndex + i) mod src_size;
                  Inc(lVal, (src[lTempSrcIdx] * self.rFilterBank[lFilterOffset + i]));
                end;
            end
          else if (lSampleIndex + self.rFilterLength > src_size) then
            begin
              break;
            end
          else if (self.rLinear = 1) then
            begin
              lV2 := 0;
              for i := 0 to self.rFilterLength - 1 do
                begin
                  Inc(lVal, src[lSampleIndex + i] * self.rFilterBank[lFilterOffset + i]);
                  Inc(lV2, src[lSampleIndex + i] * self.rFilterBank[lFilterOffset + i + self.rFilterLength]);
                end;
              Inc(lVal, ((lV2 - lVal) * (lFrac div self.rSrcIncr)));
            end
          else
            begin
              for i := 0 to self.rFilterLength - 1 do
                begin
                  Inc(lVal, (src[lSampleIndex + i] * self.rFilterBank[lFilterOffset + i]));
                end;
            end;

          lVal := (lVal + (1 shl (FILTER_SHIFT - 1))) shr FILTER_SHIFT;

          if (lVal + 32768) > 65535 then // Pascal is pain
              dst[dst_index] := (lVal shr 31) xor 32767
          else
              dst[dst_index] := lVal;

          Inc(lFrac, ldst_incr_frac);
          Inc(lIndex, lDst_incr);
          if (lFrac >= self.rSrcIncr) then
            begin
              dec(lFrac, self.rSrcIncr);
              Inc(lIndex);
            end;

          if (dst_index + 1 = lCompensationDistance) then
            begin
              lCompensationDistance := 0;
              ldst_incr_frac := self.rIdealDstIncr mod self.rSrcIncr;
              lDst_incr := self.rIdealDstIncr div self.rSrcIncr;
            end;
        end;

    end;
  consumed := Max(lIndex, 0) shr self.rPhaseShift;
  if (lIndex >= 0) then
      lIndex := lIndex and self.rPhaseMask;

  if (lCompensationDistance <> 0) then
    begin
      lCompensationDistance := lCompensationDistance - dst_index;
    end;
  if (update_ctx = 1) then
    begin
      self.rFrac := lFrac;
      self.rIndex := lIndex;
      self.rDstIncr := ldst_incr_frac + self.rSrcIncr * lDst_incr;
      self.rCompensationDistance := lCompensationDistance;
    end;

  Result := dst_index;
end;

constructor TBitStringWriter.Create;
begin
  inherited Create;
  FBuffer := 0;
  FBufferSize := 0;
  FValue := '';
end;

destructor TBitStringWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TBitStringWriter.Flush;
begin
  while (FBufferSize > 0) do
    begin
      FValue.Append(chr(FBuffer and 255));
      FBuffer := FBuffer shr 8;
      FBufferSize := FBufferSize - 8;
    end;
  FBufferSize := 0;
end;

procedure TBitStringWriter.Write(x: TUInt32; bits: TLInt);
begin
  FBuffer := FBuffer or (x shl FBufferSize);
  FBufferSize := FBufferSize + bits;
  while (FBufferSize >= 8) do
    begin
      FValue.Append(chr(FBuffer and 255));
      FBuffer := FBuffer shr 8;
      FBufferSize := FBufferSize - 8;
    end;
end;

function TBitStringReader.GetAvailableBits: TLInt;
begin
  if FEOF then
      Result := 0
  else
    begin
      Result := FBufferSize + 8 * (FValue.len - FValueIter + 1);
    end;
end;

constructor TBitStringReader.Create(Input: TPascalString);
begin
  inherited Create;
  FValue := Input;
  FBuffer := 0;
  FBufferSize := 0;
  FEOF := False;
  FValueIter := 1;
end;

destructor TBitStringReader.Destroy;
begin
  inherited Destroy;
end;

procedure TBitStringReader.Reset;
begin
  FBuffer := 0;
  FBufferSize := 0;
end;

function TBitStringReader.Read(bits: TLInt): TUInt32;
var
  lValueByte: Byte;
begin
  if (FBufferSize < bits) then
    begin
      if (FValueIter <= FValue.len) then
        begin
          lValueByte := Ord(FValue[FValueIter]);
          FBuffer := FBuffer or (lValueByte shl FBufferSize);
          Inc(FValueIter);
          FBufferSize := FBufferSize + 8;
        end
      else
        begin
          FEOF := True;
        end;
    end;

  Result := FBuffer and ((1 shl bits) - 1);
  FBuffer := FBuffer shr bits;
  FBufferSize := FBufferSize - bits;

  if (FBufferSize <= 0) and (FValueIter > FValue.len) then
    begin
      FEOF := True;
    end;
end;

constructor TCPImage.Create(columns: TLInt; rows: TLInt = 0);
var
  i: TLInt;
begin
  inherited Create;
  FColumns := columns; // 12 columns 0...x rows (variabel);
  FRows := rows;
  // dim the array correctly
  SetLength(FData, FRows);
  if FRows > 0 then
    begin
      for i := 0 to FRows - 1 do
        begin
          SetLength(FData[i], FColumns);
        end;
    end;
end;

destructor TCPImage.Destroy;
var
  i: TLInt;
begin
  for i := 0 to FRows - 1 do
    begin
      SetLength(FData[i], 0);
    end;
  SetLength(FData, 0);
  inherited Destroy;
end;

procedure TCPImage.AddRow(Row: TLVec);
var
  i: TLInt;
begin
  // add a row and copy the values
  Inc(FRows);
  SetLength(FData, FRows);
  SetLength(FData[FRows - 1], FColumns);

  for i := 0 to FColumns - 1 do
    begin
      FData[FRows - 1, i] := Row[i];
    end;
end;

function TCPImage.GetData(Row, Column: TLInt): TLFloat;
begin
  Result := FData[Row][Column];
end;

procedure TCPImage.SetData(Row, Column: TLInt; Value: TLFloat);
begin
  FData[Row][Column] := Value;
end;

procedure TIntegralImage.Transform;
var
  lColumns, lRows: TLInt;
  m, n: TLInt;
begin
  // in Pascal you know what you're doing
  lColumns := FImage.NumColumns;
  lRows := FImage.NumRows;
  for m := 1 to lColumns - 1 do
    begin
      // First column - add value on top
      FImage.SetData(0, m, FImage.GetData(0, m) + FImage.GetData(0, m - 1));
    end;
  for n := 1 to lRows - 1 do
    begin
      // First row - add value on left
      FImage.SetData(n, 0, FImage.GetData(n, 0) + FImage.GetData(n - 1, 0));
      for m := 1 to lColumns - 1 do
        begin
          FImage.SetData(n, m, FImage.GetData(n, m) + FImage.GetData(n - 1, m) + FImage.GetData(n, m - 1) - FImage.GetData(n - 1, m - 1));
        end;
    end;
end;

constructor TIntegralImage.Create(Image: TCPImage);
begin
  inherited Create;
  FImage := Image;
  Transform;
end;

destructor TIntegralImage.Destroy;
begin
  FImage := nil;
  inherited Destroy;
end;

function TIntegralImage.GetData(Row, Column: TLInt): TLFloat;
begin
  Result := FImage.GetData(Row, Column);
end;

function TIntegralImage.Area(x1, y1, x2, y2: TLInt): TLFloat;
var
  lArea: TLFloat;
begin
  if (x2 < x1) or (y2 < y1) then
    begin
      Result := 0.0;
      Exit;
    end;
  lArea := FImage.GetData(x2, y2);
  if (x1 > 0) then
    begin
      lArea := lArea - FImage.GetData(x1 - 1, y2);
      if (y1 > 0) then
        begin
          lArea := lArea + FImage.GetData(x1 - 1, y1 - 1);
        end;
    end;
  if (y1 > 0) then
    begin
      lArea := lArea - FImage.GetData(x2, y1 - 1);
    end;
  Result := lArea;
end;

function TIntegralImage.NumColumns: TLInt;
begin
  Result := FImage.NumColumns;
end;

function TIntegralImage.NumRows: TLInt;
begin
  Result := FImage.NumRows;
end;

function Subtract(a, b: TLFloat): TLFloat;
begin
  Result := a - b;
end;

function SubtractLog(a, b: TLFloat): TLFloat;
var
  r: TLFloat;
begin
  r := ln(1.0 + a) - ln(1.0 + b);
  // assert(!IsNaN(r));
  Result := r;
end;

// oooooooooooooooo
// oooooooooooooooo
// oooooooooooooooo
// oooooooooooooooo
function Filter0(Image: TIntegralImage; x, Y, w, h: TLInt; cmp: TComparator): TLFloat;
var
  a, b: TLFloat;
begin
  a := Image.Area(x, Y, x + w - 1, Y + h - 1);
  b := 0;
  Result := cmp(a, b);
end;

// ................
// ................
// oooooooooooooooo
// oooooooooooooooo
function Filter1(Image: TIntegralImage; x, Y, w, h: TLInt; cmp: TComparator): TLFloat;
var
  a, b: TLFloat;
  h_2: TLInt;
begin
  h_2 := h div 2;

  a := Image.Area(x, Y + h_2, x + w - 1, Y + h - 1);
  b := Image.Area(x, Y, x + w - 1, Y + h_2 - 1);

  Result := cmp(a, b);
end;

// .......ooooooooo
// .......ooooooooo
// .......ooooooooo
// .......ooooooooo
function Filter2(Image: TIntegralImage; x, Y, w, h: TLInt; cmp: TComparator): TLFloat;
var
  a, b: TLFloat;
  w_2: TLInt;
begin
  w_2 := w div 2;

  a := Image.Area(x + w_2, Y, x + w - 1, Y + h - 1);
  b := Image.Area(x, Y, x + w_2 - 1, Y + h - 1);

  Result := cmp(a, b);
end;

// .......ooooooooo
// .......ooooooooo
// ooooooo.........
// ooooooo.........
function Filter3(Image: TIntegralImage; x, Y, w, h: TLInt; cmp: TComparator): TLFloat;
var
  a, b: TLFloat;
  w_2, h_2: TLInt;
begin
  w_2 := w div 2;
  h_2 := h div 2;

  a := Image.Area(x, Y + h_2, x + w_2 - 1, Y + h - 1) + Image.Area(x + w_2, Y, x + w - 1, Y + h_2 - 1);
  b := Image.Area(x, Y, x + w_2 - 1, Y + h_2 - 1) + Image.Area(x + w_2, Y + h_2, x + w - 1, Y + h - 1);

  Result := cmp(a, b);
end;

// ................
// oooooooooooooooo
// ................
function Filter4(Image: TIntegralImage; x, Y, w, h: TLInt; cmp: TComparator): TLFloat;
var
  a, b: TLFloat;
  h_3: TLInt;
begin
  h_3 := h div 3;

  a := Image.Area(x, Y + h_3, x + w - 1, Y + 2 * h_3 - 1);
  b := Image.Area(x, Y, x + w - 1, Y + h_3 - 1) + Image.Area(x, Y + 2 * h_3, x + w - 1, Y + h - 1);

  Result := cmp(a, b);
end;

// .....oooooo.....
// .....oooooo.....
// .....oooooo.....
// .....oooooo.....
function Filter5(Image: TIntegralImage; x, Y, w, h: TLInt; cmp: TComparator): TLFloat;
var
  a, b: TLFloat;
  w_3: TLInt;
begin
  w_3 := w div 3;

  a := Image.Area(x + w_3, Y, x + 2 * w_3 - 1, Y + h - 1);
  b := Image.Area(x, Y, x + w_3 - 1, Y + h - 1) + Image.Area(x + 2 * w_3, Y, x + w - 1, Y + h - 1);

  Result := cmp(a, b);
end;

constructor TFilter.Create(AType: TLInt; Y: TLInt; Height: TLInt; Width: TLInt);
begin
  inherited Create;
  FType := AType;
  FY := Y;
  FHeight := Height;
  FWidth := Width;
end;

function TFilter.Apply(Image: TIntegralImage; offset: TLInt): TLFloat;
begin
  case (FType) of
    0: Result := Filter0(Image, offset, FY, FWidth, FHeight, {$IFDEF FPC}@{$ENDIF FPC}SubtractLog);
    1: Result := Filter1(Image, offset, FY, FWidth, FHeight, {$IFDEF FPC}@{$ENDIF FPC}SubtractLog);
    2: Result := Filter2(Image, offset, FY, FWidth, FHeight, {$IFDEF FPC}@{$ENDIF FPC}SubtractLog);
    3: Result := Filter3(Image, offset, FY, FWidth, FHeight, {$IFDEF FPC}@{$ENDIF FPC}SubtractLog);
    4: Result := Filter4(Image, offset, FY, FWidth, FHeight, {$IFDEF FPC}@{$ENDIF FPC}SubtractLog);
    5: Result := Filter5(Image, offset, FY, FWidth, FHeight, {$IFDEF FPC}@{$ENDIF FPC}SubtractLog);
    else
      Result := 0.0;
  end;
end;

constructor TQuantizer.Create(t0: TLFloat; t1: TLFloat; t2: TLFloat);
begin
  inherited Create;
  FT0 := t0;
  FT1 := t1;
  FT2 := t2;
end;

function TQuantizer.Quantize(Value: TLFloat): TLInt;
begin
  if (Value < FT1) then
    begin
      if (Value < FT0) then
          Result := 0
      else
          Result := 1;
    end
  else
    begin
      if (Value < FT2) then
          Result := 2
      else
          Result := 3;
    end;
end;

constructor TClassifier.Create(Filter: TFilter; Quantizer: TQuantizer);
begin
  inherited Create;
  if Filter <> nil then
      FFilter := Filter
  else
      FFilter := TFilter.Create(0, 0, 0, 0);
  if Quantizer <> nil then
      FQuantizer := Quantizer
  else
      FQuantizer := TQuantizer.Create;
end;

destructor TClassifier.Destroy;
begin
  DisposeObject(FFilter);
  DisposeObject(FQuantizer);
  inherited Destroy;
end;

function TClassifier.Classify(Image: TIntegralImage; offset: TLInt): TLInt;
var
  Value: TLFloat;
begin
  Value := FFilter.Apply(Image, offset);
  Result := FQuantizer.Quantize(Value);
end;

constructor TCombinedBuffer.Create(const Buffer1: TI16Vec; Size1: TLInt; const Buffer2: TI16Vec; Size2: TLInt);
begin
  inherited Create;
  FOffset := 0;
  FBuffer1 := Buffer1;
  FBuffer2 := Buffer2;
  FSize1 := Size1;
  FSize2 := Size2;
end;

destructor TCombinedBuffer.Destroy;
begin
  inherited Destroy;
end;

// Gets the size of the combined buffer.

function TCombinedBuffer.Size: TLInt;
begin
  Result := FSize1 + FSize2 - FOffset;
end;

// Gets the element at given position.

function TCombinedBuffer.GetValue(i: TLInt): TInt16;
var
  k: TLInt;
begin
  k := i + FOffset;
  if k < FSize1 then
      Result := FBuffer1[k]
  else
    begin
      k := k - FSize1;
      Result := FBuffer2[k];
    end;
end;

// Shift the buffer offset.

function TCombinedBuffer.Shift(Value: TLInt): TLInt;
begin
  FOffset := FOffset + Value;
  Result := FOffset;
end;

function TCombinedBuffer.Read(var Buffer: TI16Vec; offset, Count: TLInt): TLInt;
var
  n: TLInt;
  pos: TLInt;
  lSplit: TLInt;
begin
  pos := FOffset + offset;
  if (pos < FSize1) and (pos + Count > FSize1) then
    begin
      { read from first and seconde buffer }
      // Number of shorts to be read from first buffer
      lSplit := FSize1 - pos;
      // Number of shorts to be read from second buffer
      n := Math.min(Count - lSplit, FSize2);
      // Copy from both buffers
      CopyPtr(@FBuffer1[pos], @Buffer[0], lSplit * sizeof(TInt16));
      CopyPtr(@FBuffer2[0], @Buffer[lSplit], n * sizeof(TInt16));
      // Correct total length
      n := n + lSplit;
    end
  else
    begin
      if pos > FSize1 then
        begin
          { read from seconde buffer }
          pos := pos - FSize1;
          // Number of shorts to be read from second buffer
          n := Math.min(Count, FSize2 - pos);
          // Read from second buffer
          CopyPtr(@FBuffer2[pos], @Buffer[0], n * sizeof(TInt16));
        end
      else
        begin
          n := Math.min(Count, FSize1 - pos); // here not safe in C++ and C#
          CopyPtr(@FBuffer1[pos], @Buffer[0], n * sizeof(TInt16));
        end;
    end;
  Result := n;
end;

// Read all remaining values from the buffer.

procedure TCombinedBuffer.Flush(var Buffer: TI16Vec);
var
  lSize: TLInt;
begin
  // Read the whole buffer (offset will be taken care of).
  lSize := Size;
  if lSize > 0 then
    begin
      Read(Buffer, 0, lSize);
    end;
end;

constructor TFFTFrame.Create(Size: TLInt);
begin
  inherited Create;
  FSize := Size;
  SetLength(Data, FSize);
end;

destructor TFFTFrame.Destroy;
begin
  SetLength(Data, 0);
  inherited Destroy;
end;

function TFFTFrame.Magnitude(i: TLInt): TLFloat;
begin
  Result := Sqrt(Data[i]);
end;

function TFFTFrame.Energy(i: TLInt): TLFloat;
begin
  Result := Data[i];
end;

constructor TFFT.Create(frame_size: TLInt; overlap: TLInt; consumer: TFFTFrameConsumer);
var
  i: TLInt;
begin
  inherited Create;
  SetLength(FWindow, frame_size);
  FBufferOffset := 0;
  SetLength(FFFTBuffer, frame_size);

  FFrame := TFFTFrame.Create(frame_size);
  FFrameSize := frame_size;
  FIncrement := frame_size - overlap;
  FConsumer := consumer;

  PrepareHammingWindow(FWindow);
  for i := 0 to frame_size - 1 do
    begin
      FWindow[i] := FWindow[i] / $7FFF;
    end;
  FLib := TFFTLomont.Create(frame_size, FWindow);
end;

destructor TFFT.Destroy;
begin
  DisposeObject(FFrame);
  DisposeObject(FLib);
  SetLength(FFFTBuffer, 0);
  SetLength(FWindow, 0);
  inherited Destroy;
end;

function TFFT.overlap: TLInt;
begin
  Result := FFrameSize - FIncrement;
end;

procedure TFFT.Reset;
begin
  FBufferOffset := 0;
end;

procedure TFFT.Consume(Input: TI16Vec; AOffset: TLInt; lLength: TLInt);
var
  lCombinedBuffer: TCombinedBuffer;
  lBuffer: TI16Vec;
begin
  // Special case, just pre-filling the buffer
  if (FBufferOffset + lLength < FFrameSize) then
    begin
      CopyPtr(@Input[0], @FFFTBuffer[FBufferOffset], lLength * sizeof(TInt16));
      FBufferOffset := FBufferOffset + lLength;
      Exit;
    end;
  // Apply FFT on the available data
  lCombinedBuffer := TCombinedBuffer.Create(FFFTBuffer, FBufferOffset, Input, lLength);

  while (lCombinedBuffer.Size >= FFrameSize) do
    begin
      SetLength(lBuffer, FFrameSize);
      lCombinedBuffer.Read(lBuffer, 0, FFrameSize);

      FLib.ComputeFrame(lBuffer, FFrame);
      FConsumer.Consume(FFrame);
      lCombinedBuffer.Shift(FIncrement);
      SetLength(lBuffer, 0);
    end;
  // Copy the remaining input data to the internal buffer
  lCombinedBuffer.Flush(FFFTBuffer);
  FBufferOffset := lCombinedBuffer.Size();
  DisposeObject(lCombinedBuffer);
end;

constructor TFFTLib.Create(frame_size: TLInt; window: TLVec);
begin
  inherited Create;
end;

destructor TFFTLib.Destroy;
begin
  inherited Destroy;
end;

// Call this with the size before using the FFT
// Fills in tables for speed
procedure TFFTLomont.ComputeTable(Size: TLInt);
var
  i, n, mmax, istep, m: TLInt;
  theta, wr, wpr, wpi, wi, t: TLFloat;
begin
  SetLength(ForwardCos, Size);
  SetLength(ForwardSin, Size);

  // forward pass
  i := 0;
  n := Size;
  mmax := 1;
  while (n > mmax) do
    begin
      istep := 2 * mmax;
      theta := Math_Pi / mmax;
      wr := 1;
      wi := 0;
      wpr := cos(theta);
      wpi := Sin(theta);
      m := 0;
      while m < istep do
        begin
          ForwardCos[i] := wr;
          ForwardSin[i] := wi;
          Inc(i);

          t := wr;
          wr := wr * wpr - wi * wpi;
          wi := wi * wpr + t * wpi;
          m := m + 2;
        end;
      mmax := istep;
    end;
end;

constructor TFFTLomont.Create(frame_size: TLInt; window: TLVec);
begin
  inherited Create(frame_size, window);
  FFrameSize := frame_size;
  FFrameSizeH := frame_size div 2;
  FWindow := window;
  SetLength(FInput, frame_size);
  ComputeTable(frame_size);
end;

destructor TFFTLomont.Destroy;
begin
  SetLength(FInput, 0);
  SetLength(FWindow, 0);
  SetLength(ForwardCos, 0);
  SetLength(ForwardSin, 0);
  inherited Destroy;
end;


// Compute the forward or inverse FFT of data, which is
// complex valued items, stored in alternating real and
// imaginary real numbers. The length must be a power of 2.

procedure TFFTLomont.FFT(var Data: TLVec);
var
  n, j, top, k, h: TLInt;
  t: TLFloat;
  mmax, tptr, istep, m: TLInt;
  wr, wi, tempr, tempi: TLFloat;
begin
  n := Length(Data);
  // check all are valid
  // checks n is a power of 2 in 2's complement format
  if ((n and (n - 1)) <> 0) then
    begin
      // throw new Exception("data length " + n + " in FFT is not a power of 2");
      Exit;
    end;
  n := n div 2;

  // bit reverse the indices. This is exercise 5 in section 7.2.1.1 of Knuth's TAOCP
  // the idea is a binary counter in k and one with bits reversed in j
  // see also Alan H. Karp, "Bit Reversals on Uniprocessors", SIAM Review, vol. 38, #1, 1--26, March (1996)
  // nn = number of samples, 2* this is length of data?
  j := 0;
  k := 0;         // Knuth R1: initialize
  top := n div 2; // this is Knuth's 2^(n-1)
  while (True) do
    begin
      // Knuth R2: swap
      // swap j+1 and k+2^(n-1) - both have two entries
      t := Data[j + 2];
      Data[j + 2] := Data[k + n];
      Data[k + n] := t;
      t := Data[j + 3];
      Data[j + 3] := Data[k + n + 1];
      Data[k + n + 1] := t;
      if (j > k) then
        begin // swap two more
          // j and k
          t := Data[j];
          Data[j] := Data[k];
          Data[k] := t;
          t := Data[j + 1];
          Data[j + 1] := Data[k + 1];
          Data[k + 1] := t;
          // j + top + 1 and k+top + 1
          t := Data[j + n + 2];
          Data[j + n + 2] := Data[k + n + 2];
          Data[k + n + 2] := t;
          t := Data[j + n + 3];
          Data[j + n + 3] := Data[k + n + 3];
          Data[k + n + 3] := t;
        end;
      // Knuth R3: advance k
      k := k + 4;
      if (k >= n) then
          break;
      // Knuth R4: advance j
      h := top;
      while (j >= h) do
        begin
          j := j - h;
          h := h div 2;
        end;
      j := j + h;
    end; // bit reverse loop

  // do transform by doing float transforms, then doubles, fours, etc.
  mmax := 1;
  tptr := 0;
  while (n > mmax) do
    begin
      istep := 2 * mmax;
      m := 0;
      while m < istep do
        begin
          wr := ForwardCos[tptr];
          wi := ForwardSin[tptr];
          Inc(tptr);
          k := m;
          while (k < 2 * n) do
            begin
              j := k + istep;
              tempr := wr * Data[j] - wi * Data[j + 1];
              tempi := wi * Data[j] + wr * Data[j + 1];
              Data[j] := Data[k] - tempr;
              Data[j + 1] := Data[k + 1] - tempi;
              Data[k] := Data[k] + tempr;
              Data[k + 1] := Data[k + 1] + tempi;

              k := k + 2 * istep;
            end;
          Inc(m, 2);
        end;
      mmax := istep;
    end;
end;

// Computes the real FFT.

procedure TFFTLomont.RealFFT(var Data: TLVec);
var
  n, j, k: TLInt;
  temp, theta, wpr, wpi, wjr, wji: TLFloat;
  tnr, tni, tjr, tji: TLFloat;
  a, b, c, d, e, f: TLFloat;
begin
  FFT(Data); // do packed FFT

  n := Length(Data); // number of real input points, which is 1/2 the complex length

  theta := 2 * Math_Pi / n;
  wpr := cos(theta);
  wpi := Sin(theta);
  wjr := wpr;
  wji := wpi;
  for j := 1 to (n div 4) do
    begin

      k := n div 2 - j;
      tnr := Data[2 * k];
      tni := Data[2 * k + 1];
      tjr := Data[2 * j];
      tji := Data[2 * j + 1];

      e := (tjr + tnr);
      f := (tji - tni);
      a := (tjr - tnr) * wji;
      d := (tji + tni) * wji;
      b := (tji + tni) * wjr;
      c := (tjr - tnr) * wjr;

      // compute entry y[j]
      Data[2 * j] := 0.5 * (e + (a + b));
      Data[2 * j + 1] := 0.5 * (f - (c - d));

      // compute entry y[k]
      Data[2 * k] := 0.5 * (e - (a + b));
      Data[2 * k + 1] := 0.5 * ((-c + d) - f);

      temp := wjr;
      wjr := wjr * wpr - wji * wpi;
      wji := temp * wpi + wji * wpr;
    end;

  // compute final y0 and y_{N/2} ones, place into data[0] and data[1]
  temp := Data[0];
  Data[0] := Data[0] + Data[1];
  Data[1] := temp - Data[1];
end;

procedure TFFTLomont.ComputeFrame(Input: TI16Vec; var frame: TFFTFrame);
var
  i: TLInt;
begin
  for i := 0 to FFrameSize - 1 do
    begin
      FInput[i] := Input[i] * FWindow[i] * 1.0;
    end;

  RealFFT(FInput);

  // FInput will now contain the FFT values
  // r0, r(n/2), r1, i1, r2, i2 ...

  // Compute energy
  frame.Data[0] := FInput[0] * FInput[0];
  frame.Data[FFrameSizeH] := FInput[1] * FInput[1];

  for i := 1 to FFrameSizeH - 1 do
    begin
      frame.Data[i] := FInput[2 * i] * FInput[2 * i] + FInput[2 * i + 1] * FInput[2 * i + 1];
    end;

end;

constructor TChromaFilter.Create(coefficients: TLVec; ALength: TLInt; consumer: TFeatureVectorConsumer);
begin
  inherited Create;
  SetLength(FResult, 12);
  FCoefficients := coefficients;
  FLength := ALength;
  SetLength(FBuffer, 8);

  FBufferOffset := 0;
  FBufferSize := 1;
  FConsumer := consumer;
end;

destructor TChromaFilter.Destroy;
begin
  SetLength(FResult, 0);
  FConsumer := nil;
  SetLength(FCoefficients, 0);
  inherited Destroy;
end;

procedure TChromaFilter.Reset;
begin
  FBufferSize := 1;
  FBufferOffset := 0;
end;

procedure TChromaFilter.Consume(var features: TLVec);
var
  offset: TLInt;
  i, j: TLInt;
begin
  SetLength(FBuffer[FBufferOffset], Length(features));
  for i := 0 to Length(features) - 1 do
    begin
      FBuffer[FBufferOffset, i] := features[i];
    end;
  FBufferOffset := (FBufferOffset + 1) mod 8;
  if (FBufferSize >= FLength) then
    begin
      offset := (FBufferOffset + 8 - FLength) mod 8;
      for i := 0 to Length(FResult) - 1 do
        begin
          FResult[i] := 0.0;
        end;
      for i := 0 to 11 do
        begin
          for j := 0 to FLength - 1 do
            begin

              FResult[i] := FResult[i] + FBuffer[(offset + j) mod 8, i] * FCoefficients[j];
            end;
        end;
      FConsumer.Consume(FResult);
    end
  else
    begin
      Inc(FBufferSize);
    end;
end;

constructor TFeatureVectorConsumer.Create;
begin
  inherited Create;
end;

destructor TFeatureVectorConsumer.Destroy;
begin
  inherited Destroy;
end;

constructor TImageBuilder.Create(Image: TCPImage);
begin
  inherited Create;
  FImage := Image;
end;

destructor TImageBuilder.Destroy;
begin
  FImage := nil;
  inherited Destroy;
end;

procedure TImageBuilder.Reset(Image: TCPImage);
begin
  FImage := Image;
end;

procedure TImageBuilder.Consume(var features: TLVec);
begin
  FImage.AddRow(features);
end;

constructor TChromaNormalizer.Create(consumer: TFeatureVectorConsumer);
begin
  inherited Create;
  FConsumer := consumer;
end;

destructor TChromaNormalizer.Destroy;
begin
  FConsumer := nil;
  inherited Destroy;
end;

procedure TChromaNormalizer.Reset;
begin
end;

procedure TChromaNormalizer.Consume(var features: TLVec);
var
  norm: TLFloat;
begin
  norm := EuclideanNorm(features);
  NormalizeVector(features, norm, 0.01);
  FConsumer.Consume(features);
end;

constructor TChromaResampler.Create(factor: TLInt; consumer: TFeatureVectorConsumer);
begin
  inherited Create;
  SetLength(FResult, 12);
  Reset;
  FFactor := factor;
  FConsumer := consumer;
end;

destructor TChromaResampler.Destroy;
begin
  SetLength(FResult, 0);
  inherited Destroy;
end;

procedure TChromaResampler.Reset;
var
  i: TLInt;
begin
  FIteration := 0;
  for i := 0 to Length(FResult) - 1 do
    begin
      FResult[i] := 0.0;
    end;
end;

procedure TChromaResampler.Consume(var features: TLVec);
var
  i: TLInt;
begin
  for i := 0 to 11 do
      LAdd(FResult[i], features[i]);

  Inc(FIteration);
  if (FIteration = FFactor) then
    begin
      for i := 0 to 11 do
          LDiv(FResult[i], FFactor);
      FConsumer.Consume(FResult);
      Reset();
    end;
end;

constructor TFingerprintCalculator.Create(classifiers: TClassifierArray);
var
  i: TLInt;
  n: TLInt;
begin
  inherited Create;
  FClassifiers := classifiers;
  n := Length(FClassifiers);
  FMaxFilterWidth := 0;
  for i := 0 to n - 1 do
    begin
      FMaxFilterWidth := Math.Max(FMaxFilterWidth, FClassifiers[i].FFilter.FWidth);
    end;
end;

destructor TFingerprintCalculator.Destroy;
begin
  inherited Destroy;
end;

function TFingerprintCalculator.Calculate(Image: TCPImage): TU32Vec;
var
  lLength: TLInt;
  i: TLInt;
  lIntegralImage: TIntegralImage;
begin
  lLength := Image.NumRows - FMaxFilterWidth + 1;
  if (lLength <= 0) then
    begin
      Result := nil;
      Exit;
    end;
  lIntegralImage := TIntegralImage.Create(Image);
  SetLength(Result, lLength);
  for i := 0 to lLength - 1 do
    begin
      Result[i] := CalculateSubfingerprint(lIntegralImage, i);
    end;
  DisposeObject(lIntegralImage);
end;

function TFingerprintCalculator.CalculateSubfingerprint(Image: TIntegralImage; offset: TLInt): TUInt32;
var
  bits: TUInt32;
  i: TLInt;
  n: TLInt;
begin
  bits := 0;
  n := Length(FClassifiers);
  for i := 0 to n - 1 do
    begin
      bits := (bits shl 2) or CODES[TClassifierArray(FClassifiers)[i].Classify(Image, offset)];
    end;
  Result := bits;
end;

function CompressFingerprint(Data: TU32Vec; algorithm: TLInt = 0): TPascalString;
var
  compressor: TFingerprintCompressor;
begin
  compressor := TFingerprintCompressor.Create;
  try
      Result := compressor.Compress(Data, algorithm);
  finally
      DisposeObject(compressor);
  end;
end;

function DecompressFingerprint(Data: TPascalString; var algorithm: TLInt): TU32Vec;
var
  decompressor: TFingerprintDecompressor;
begin
  decompressor := TFingerprintDecompressor.Create;
  try
      Result := decompressor.Decompress(Data, algorithm);
  finally
      DisposeObject(decompressor);
  end;
end;

constructor TFingerprintCompressor.Create;
begin
  inherited Create;
end;

procedure TFingerprintCompressor.ProcessSubfingerprint(x: TUInt32);
var
  bit, last_bit, n: TLInt;
begin
  bit := 1;
  last_bit := 0;
  while (x <> 0) do
    begin
      if ((x and 1) <> 0) then
        begin
          n := Length(FBits);
          SetLength(FBits, n + 1);
          FBits[n] := (bit - last_bit);
          last_bit := bit;
        end;
      x := x shr 1;
      Inc(bit);
    end;
  n := Length(FBits);
  SetLength(FBits, n + 1);
  FBits[n] := 0;
end;

procedure TFingerprintCompressor.WriteExceptionBits;
var
  writer: TBitStringWriter;
  i: TLInt;
begin
  writer := TBitStringWriter.Create;
  for i := 0 to Length(FBits) - 1 do
    begin
      if (FBits[i] >= kMaxNormalValue) then
        begin
          writer.Write(FBits[i] - kMaxNormalValue, kExceptionBits);
        end;
    end;
  writer.Flush();
  FResult.Append(writer.FValue);
  DisposeObject(writer);
end;

procedure TFingerprintCompressor.WriteNormalBits;
var
  writer: TBitStringWriter;
  i: TLInt;
begin
  writer := TBitStringWriter.Create;
  for i := 0 to Length(FBits) - 1 do
    begin
      writer.Write(Math.min(FBits[i], kMaxNormalValue), kNormalBits);
    end;
  writer.Flush();
  FResult.Append(writer.FValue);
  DisposeObject(writer);
end;

function TFingerprintCompressor.Compress(var fingerprint: TU32Vec; algorithm: TLInt = 0): TPascalString;
var
  i, n: TLInt;
begin
  n := Length(fingerprint);
  if n > 0 then
    begin
      ProcessSubfingerprint(fingerprint[0]);
      for i := 1 to n - 1 do
        begin
          ProcessSubfingerprint(fingerprint[i] xor fingerprint[i - 1]);
        end;
    end;
  FResult := chr(algorithm and 255);
  FResult.Append(chr((n shr 16) and 255));
  FResult.Append(chr((n shr 8) and 255));
  FResult.Append(chr(n and 255));

  WriteNormalBits();
  WriteExceptionBits();
  Result := FResult;
end;

constructor TFingerprintDecompressor.Create;
begin
  inherited Create;
end;

function TFingerprintDecompressor.Decompress(fingerprint: TPascalString; var algorithm: TLInt): TU32Vec;
var
  n: TLInt;
  reader: TBitStringReader;
begin
  SetLength(Result, 0);
  if fingerprint.len < 4 then
    begin
      DoStatus('FingerprintDecompressor::Decompress() -- Invalid fingerprint (shorter than 4 bytes)');
      Exit;
    end;
  if algorithm <> 0 then
      algorithm := Ord(fingerprint[1]);

  n := (Ord(fingerprint[2]) shl 16) or (Ord(fingerprint[3]) shl 8) or (Ord(fingerprint[4]));
  reader := TBitStringReader.Create(fingerprint);

  reader.Read(8);
  reader.Read(8);
  reader.Read(8);
  reader.Read(8);

  if (reader.AvailableBits < n * kNormalBits) then
    begin
      DoStatus('FingerprintDecompressor::Decompress() -- Invalid fingerprint (too short)');
      DisposeObject(reader);
      Exit;
    end;

  SetLength(FResult, n);

  reader.Reset();
  if (not ReadNormalBits(reader)) then
    begin
      DisposeObject(reader);
      Exit;
    end;

  reader.Reset();
  if (not ReadExceptionBits(reader)) then
    begin
      DisposeObject(reader);
      Exit;
    end;

  UnpackBits();
  Result := FResult;

end;

function TFingerprintDecompressor.ReadExceptionBits(reader: TBitStringReader): boolean;
var
  i: TLInt;
begin
  for i := 0 to Length(FBits) - 1 do
    begin
      if (FBits[i] = kMaxNormalValue) then
        begin
          if (reader.EOF) then
            begin
              DoStatus('FingerprintDecompressor.ReadExceptionBits() -- Invalid fingerprint (reached EOF while reading exception bits)');
              Result := False;
              Exit;
            end;
          FBits[i] := FBits[i] + reader.Read(kExceptionBits);
        end;
    end;
  Result := True;
end;

function TFingerprintDecompressor.ReadNormalBits(reader: TBitStringReader): boolean;
var
  i, bit, n: TLInt;
begin
  i := 0;
  while (i < Length(FResult)) do
    begin
      bit := reader.Read(kNormalBits);
      if (bit = 0) then
          Inc(i);
      n := Length(FBits);
      SetLength(FBits, n + 1);
      FBits[n] := bit;
    end;
  Result := True;
end;

procedure TFingerprintDecompressor.UnpackBits;
var
  i, last_bit, j, bit: TLInt;
  Value: TUInt32;
begin
  i := 0;
  last_bit := 0;
  Value := 0;

  for j := 0 to Length(FBits) - 1 do
    begin
      bit := FBits[j];
      if (bit = 0) then
        begin
          if (i > 0) then
              FResult[i] := Value xor FResult[i - 1]
          else
              FResult[i] := Value;
          Value := 0;
          last_bit := 0;
          Inc(i);
          continue;
        end;
      bit := bit + last_bit;
      last_bit := bit;
      Value := Value or (1 shl (bit - 1));
    end;
end;

function CreateFingerprinterConfiguration(algorithm: TAudioprintAlgorithm): TFingerprinterConfiguration;
begin
  case (algorithm) of
    Audioprint_ALGORITHM_TEST1: Result := TFingerprinterConfigurationTest1.Create;
    Audioprint_ALGORITHM_TEST2: Result := TFingerprinterConfigurationTest2.Create;
    Audioprint_ALGORITHM_TEST3: Result := TFingerprinterConfigurationTest3.Create;
    Audioprint_ALGORITHM_TEST4: Result := TFingerprinterConfigurationTest4.Create;
    else
      Result := nil;
  end;
end;

constructor TFingerprinterConfigurationTest4.Create;
begin
  inherited Create;
  FRemoveSilence := True;
  FSilenceThreshold := 50;
end;

destructor TFingerprinterConfigurationTest4.Destroy;
begin
  inherited Destroy;
end;

constructor TFingerprinterConfigurationTest3.Create;
var
  kClassifiersTest: TClassifierArray;
begin
  inherited Create;
  SetLength(kClassifiersTest, 16);
  kClassifiersTest[0] := TClassifier.Create(TFilter.Create(0, 4, 3, 15), TQuantizer.Create(1.98215, 2.35817, 2.63523));
  kClassifiersTest[1] := TClassifier.Create(TFilter.Create(4, 4, 6, 15), TQuantizer.Create(-1.03809, -0.651211, -0.282167));
  kClassifiersTest[2] := TClassifier.Create(TFilter.Create(1, 0, 4, 16), TQuantizer.Create(-0.298702, 0.119262, 0.558497));
  kClassifiersTest[3] := TClassifier.Create(TFilter.Create(3, 8, 2, 12), TQuantizer.Create(-0.105439, 0.0153946, 0.135898));
  kClassifiersTest[4] := TClassifier.Create(TFilter.Create(3, 4, 4, 8), TQuantizer.Create(-0.142891, 0.0258736, 0.200632));
  kClassifiersTest[5] := TClassifier.Create(TFilter.Create(4, 0, 3, 5), TQuantizer.Create(-0.826319, -0.590612, -0.368214));
  kClassifiersTest[6] := TClassifier.Create(TFilter.Create(1, 2, 2, 9), TQuantizer.Create(-0.557409, -0.233035, 0.0534525));
  kClassifiersTest[7] := TClassifier.Create(TFilter.Create(2, 7, 3, 4), TQuantizer.Create(-0.0646826, 0.00620476, 0.0784847));
  kClassifiersTest[8] := TClassifier.Create(TFilter.Create(2, 6, 2, 16), TQuantizer.Create(-0.192387, -0.029699, 0.215855));
  kClassifiersTest[9] := TClassifier.Create(TFilter.Create(2, 1, 3, 2), TQuantizer.Create(-0.0397818, -0.00568076, 0.0292026));
  kClassifiersTest[10] := TClassifier.Create(TFilter.Create(5, 10, 1, 15), TQuantizer.Create(-0.53823, -0.369934, -0.190235));
  kClassifiersTest[11] := TClassifier.Create(TFilter.Create(3, 6, 2, 10), TQuantizer.Create(-0.124877, 0.0296483, 0.139239));
  kClassifiersTest[12] := TClassifier.Create(TFilter.Create(2, 1, 1, 14), TQuantizer.Create(-0.101475, 0.0225617, 0.231971));
  kClassifiersTest[13] := TClassifier.Create(TFilter.Create(3, 5, 6, 4), TQuantizer.Create(-0.0799915, -0.00729616, 0.063262));
  kClassifiersTest[14] := TClassifier.Create(TFilter.Create(1, 9, 2, 12), TQuantizer.Create(-0.272556, 0.019424, 0.302559));
  kClassifiersTest[15] := TClassifier.Create(TFilter.Create(3, 4, 2, 14), TQuantizer.Create(-0.164292, -0.0321188, 0.0846339));

  SetClassifiers(kClassifiersTest);
  SetFilterCoefficients(kChromaFilterCoefficients);
  FInterpolate := True;
end;

destructor TFingerprinterConfigurationTest3.Destroy;
begin
  inherited Destroy;
end;

constructor TFingerprinterConfigurationTest2.Create;
var
  kClassifiersTest: TClassifierArray;
begin
  inherited Create;

  SetLength(kClassifiersTest, 16);
  kClassifiersTest[0] := TClassifier.Create(TFilter.Create(0, 4, 3, 15), TQuantizer.Create(1.98215, 2.35817, 2.63523));
  kClassifiersTest[1] := TClassifier.Create(TFilter.Create(4, 4, 6, 15), TQuantizer.Create(-1.03809, -0.651211, -0.282167));
  kClassifiersTest[2] := TClassifier.Create(TFilter.Create(1, 0, 4, 16), TQuantizer.Create(-0.298702, 0.119262, 0.558497));
  kClassifiersTest[3] := TClassifier.Create(TFilter.Create(3, 8, 2, 12), TQuantizer.Create(-0.105439, 0.0153946, 0.135898));
  kClassifiersTest[4] := TClassifier.Create(TFilter.Create(3, 4, 4, 8), TQuantizer.Create(-0.142891, 0.0258736, 0.200632));
  kClassifiersTest[5] := TClassifier.Create(TFilter.Create(4, 0, 3, 5), TQuantizer.Create(-0.826319, -0.590612, -0.368214));
  kClassifiersTest[6] := TClassifier.Create(TFilter.Create(1, 2, 2, 9), TQuantizer.Create(-0.557409, -0.233035, 0.0534525));
  kClassifiersTest[7] := TClassifier.Create(TFilter.Create(2, 7, 3, 4), TQuantizer.Create(-0.0646826, 0.00620476, 0.0784847));
  kClassifiersTest[8] := TClassifier.Create(TFilter.Create(2, 6, 2, 16), TQuantizer.Create(-0.192387, -0.029699, 0.215855));
  kClassifiersTest[9] := TClassifier.Create(TFilter.Create(2, 1, 3, 2), TQuantizer.Create(-0.0397818, -0.00568076, 0.0292026));
  kClassifiersTest[10] := TClassifier.Create(TFilter.Create(5, 10, 1, 15), TQuantizer.Create(-0.53823, -0.369934, -0.190235));
  kClassifiersTest[11] := TClassifier.Create(TFilter.Create(3, 6, 2, 10), TQuantizer.Create(-0.124877, 0.0296483, 0.139239));
  kClassifiersTest[12] := TClassifier.Create(TFilter.Create(2, 1, 1, 14), TQuantizer.Create(-0.101475, 0.0225617, 0.231971));
  kClassifiersTest[13] := TClassifier.Create(TFilter.Create(3, 5, 6, 4), TQuantizer.Create(-0.0799915, -0.00729616, 0.063262));
  kClassifiersTest[14] := TClassifier.Create(TFilter.Create(1, 9, 2, 12), TQuantizer.Create(-0.272556, 0.019424, 0.302559));
  kClassifiersTest[15] := TClassifier.Create(TFilter.Create(3, 4, 2, 14), TQuantizer.Create(-0.164292, -0.0321188, 0.0846339));

  SetClassifiers(kClassifiersTest);
  SetFilterCoefficients(kChromaFilterCoefficients);
  FInterpolate := False;
end;

destructor TFingerprinterConfigurationTest2.Destroy;
begin
  inherited Destroy;
end;

constructor TFingerprinterConfigurationTest1.Create;
var
  kClassifiersTest: TClassifierArray;
begin
  inherited Create;
  SetLength(kClassifiersTest, 16);
  kClassifiersTest[0] := TClassifier.Create(TFilter.Create(0, 0, 3, 15), TQuantizer.Create(2.10543, 2.45354, 2.69414));
  kClassifiersTest[1] := TClassifier.Create(TFilter.Create(1, 0, 4, 14), TQuantizer.Create(-0.345922, 0.0463746, 0.446251));
  kClassifiersTest[2] := TClassifier.Create(TFilter.Create(1, 4, 4, 11), TQuantizer.Create(-0.392132, 0.0291077, 0.443391));
  kClassifiersTest[3] := TClassifier.Create(TFilter.Create(3, 0, 4, 14), TQuantizer.Create(-0.192851, 0.00583535, 0.204053));
  kClassifiersTest[4] := TClassifier.Create(TFilter.Create(2, 8, 2, 4), TQuantizer.Create(-0.0771619, -0.00991999, 0.0575406));
  kClassifiersTest[5] := TClassifier.Create(TFilter.Create(5, 6, 2, 15), TQuantizer.Create(-0.710437, -0.518954, -0.330402));
  kClassifiersTest[6] := TClassifier.Create(TFilter.Create(1, 9, 2, 16), TQuantizer.Create(-0.353724, -0.0189719, 0.289768));
  kClassifiersTest[7] := TClassifier.Create(TFilter.Create(3, 4, 2, 10), TQuantizer.Create(-0.128418, -0.0285697, 0.0591791));
  kClassifiersTest[8] := TClassifier.Create(TFilter.Create(3, 9, 2, 16), TQuantizer.Create(-0.139052, -0.0228468, 0.0879723));
  kClassifiersTest[9] := TClassifier.Create(TFilter.Create(2, 1, 3, 6), TQuantizer.Create(-0.133562, 0.00669205, 0.155012));
  kClassifiersTest[10] := TClassifier.Create(TFilter.Create(3, 3, 6, 2), TQuantizer.Create(-0.0267, 0.00804829, 0.0459773));
  kClassifiersTest[11] := TClassifier.Create(TFilter.Create(2, 8, 1, 10), TQuantizer.Create(-0.0972417, 0.0152227, 0.129003));
  kClassifiersTest[12] := TClassifier.Create(TFilter.Create(3, 4, 4, 14), TQuantizer.Create(-0.141434, 0.00374515, 0.149935));
  kClassifiersTest[13] := TClassifier.Create(TFilter.Create(5, 4, 2, 15), TQuantizer.Create(-0.64035, -0.466999, -0.285493));
  kClassifiersTest[14] := TClassifier.Create(TFilter.Create(5, 9, 2, 3), TQuantizer.Create(-0.322792, -0.254258, -0.174278));
  kClassifiersTest[15] := TClassifier.Create(TFilter.Create(2, 1, 8, 4), TQuantizer.Create(-0.0741375, -0.00590933, 0.0600357));

  SetClassifiers(kClassifiersTest);
  SetFilterCoefficients(kChromaFilterCoefficients);
  FInterpolate := False;
end;

destructor TFingerprinterConfigurationTest1.Destroy;
begin
  inherited Destroy;
end;

constructor TFingerprinterConfiguration.Create;
begin
  FNumClassifiers := 0;
  FClassifiers := nil;
  FRemoveSilence := False;
  FSilenceThreshold := 0;

  SetLength(kChromaFilterCoefficients, kChromaFilterSize);

  kChromaFilterCoefficients[0] := 0.25;
  kChromaFilterCoefficients[1] := 0.75;
  kChromaFilterCoefficients[2] := 1.0;
  kChromaFilterCoefficients[3] := 0.75;
  kChromaFilterCoefficients[4] := 0.25;
end;

destructor TFingerprinterConfiguration.Destroy;
var
  i: TLInt;
begin
  for i := 0 to Length(FClassifiers) - 1 do
    begin
      DisposeObject(FClassifiers[i]);
    end;
  inherited Destroy;
end;

procedure TFingerprinterConfiguration.SetClassifiers(classifiers: TClassifierArray);
begin
  FClassifiers := classifiers;
  FNumClassifiers := Length(classifiers);
end;

procedure TFingerprinterConfiguration.SetFilterCoefficients(FilterCoefficients: TLVec);
begin
  FFilterCoefficients := FilterCoefficients;
  FNumFilterCoefficients := Length(FilterCoefficients);
end;

constructor TMovingAverage.Create(Size: TLInt);
var
  i: TLInt;
begin
  FSize := Size;
  FOffset := 0;
  FSum := 0;
  FCount := 0;

  SetLength(FBuffer, FSize);
  for i := 0 to FSize - 1 do
    begin
      FBuffer[i] := 0;
    end;
end;

destructor TMovingAverage.Destroy;
begin
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

procedure TMovingAverage.AddValue(const x: TInt16);
begin
  FSum := FSum + x;
  FSum := FSum - FBuffer[FOffset];
  if (FCount < FSize) then
    begin
      Inc(FCount);
    end;
  FBuffer[FOffset] := x;
  FOffset := (FOffset + 1) mod FSize;
end;

function TMovingAverage.GetAverage: TInt16;
begin
  if (FCount = 0) then
    begin
      Result := 0;
      Exit;
    end;
  Result := FSum div FCount;
end;

constructor TSilenceRemover.Create(consumer: TAudioConsumer; Threshold: TLInt);
begin
  FStart := True;
  FThreshold := Threshold;
  FAverage := TMovingAverage.Create(kSilenceWindow);
  FConsumer := consumer;
end;

destructor TSilenceRemover.Destroy;
begin
  DisposeObject(FAverage);
  inherited Destroy;
end;

function TSilenceRemover.Reset(Sample_Rate, NumChannels: TLInt): boolean;
begin
  if NumChannels <> 1 then
    begin
      Result := False;
      Exit;
    end;
  FStart := True;
  Result := True;
end;

procedure TSilenceRemover.Flush;
begin
end;

procedure TSilenceRemover.Consume(Input: TI16Vec; AOffset: TLInt; Length: TLInt);
var
  offset, n: TLInt;
begin
  offset := 0;
  n := Length;
  if (FStart) then
    begin
      while (n > 0) do
        begin
          FAverage.AddValue(Abs(Input[offset]));
          if (FAverage.GetAverage() > FThreshold) then
            begin
              FStart := False;
              break;
            end;
          Inc(offset);
          dec(n);
        end;
    end;
  if (n > 0) then
    begin
      FConsumer.Consume(Input, offset, Length);
    end;
end;

procedure TAudioProcessor.Resample;
var
  consumed, lLength: TLInt;
  remaining: TLInt;
begin
  // coding C++ is a pain
  if (FResampleCTX = nil) then
    begin
      FConsumer.Consume(FBuffer, 0, FBufferOffset);
      FBufferOffset := 0;
      Exit;
    end;
  consumed := 0;
  lLength := FResampleCTX^.Resample(FResampleBuffer, FBuffer, consumed, FBufferOffset, kMaxBufferSize, 1);
  if (lLength > kMaxBufferSize) then
    begin
      DoStatus('Audioprint::AudioProcessor::Resample() -- Resampling overwrote output buffer.');
      lLength := kMaxBufferSize;
    end;
  FConsumer.Consume(FResampleBuffer, 0, lLength); // do the FFT now
  remaining := FBufferOffset - consumed;
  if (remaining > 0) then
    begin
      CopyPtr(@FBuffer[consumed], @FBuffer[0], remaining * sizeof(TInt16));
    end
  else if (remaining < 0) then
    begin
      DoStatus('Audioprint::AudioProcessor::Resample() -- Resampling overread input buffer.');
      remaining := 0;
    end;
  FBufferOffset := remaining;
end;

function TAudioProcessor.Load(Input: TI16Vec; offset: TLInt; ALength: TLInt): TLInt;
var
  lLength: TLInt;
begin
  lLength := Math.min(ALength, FBufferSize - FBufferOffset);

  case (FNumChannels) of
    1:
      LoadMono(Input, offset, lLength);
    2:
      LoadStereo(Input, offset, lLength);
    else
      LoadMultiChannel(Input, offset, lLength);

  end;
  FBufferOffset := FBufferOffset + lLength;
  Result := lLength;
end;

procedure TAudioProcessor.LoadMono(Input: TI16Vec; offset: TLInt; ALength: TLInt);
var
  i: TLInt;
begin
  i := FBufferOffset;
  while ALength > 0 do
    begin
      FBuffer[i] := Input[offset];
      Inc(offset);
      Inc(i);
      dec(ALength);
    end;

end;

procedure TAudioProcessor.LoadStereo(Input: TI16Vec; offset: TLInt; ALength: TLInt);
var
  i: TLInt;
begin
  i := FBufferOffset;
  while (ALength > 0) do
    begin
      FBuffer[i] := (Input[offset] + Input[offset + 1]) div 2;
      Inc(i);
      Inc(offset, 2);
      dec(ALength);
    end;

end;

procedure TAudioProcessor.LoadMultiChannel(Input: TI16Vec; offset: TLInt; ALength: TLInt);
var
  sum: TLInt;
  i, j: TLInt;
begin
  i := FBufferOffset;
  while ALength > 0 do
    begin
      sum := 0;
      for j := 0 to FNumChannels - 1 do
        begin
          sum := sum + Input[offset];
          Inc(offset);
        end;
      FBuffer[i] := sum div FNumChannels;
      Inc(i);
      dec(ALength);
    end;
end;

constructor TAudioProcessor.Create(SampleRate: TLInt; consumer: TAudioConsumer);
begin
  FBufferSize := kMaxBufferSize;
  FTargetSampleRate := SampleRate;
  FConsumer := consumer;
  FResampleCTX := nil;
  SetLength(FBuffer, kMaxBufferSize);
  FBufferOffset := 0;
  SetLength(FResampleBuffer, kMaxBufferSize);
end;

destructor TAudioProcessor.Destroy;
begin
  if FResampleCTX <> nil then
      Dispose(FResampleCTX);
  SetLength(FBuffer, 0);
  SetLength(FResampleBuffer, 0);
  inherited Destroy;
end;

function TAudioProcessor.Reset(SampleRate, NumChannels: TLInt): boolean;
begin
  if (NumChannels <= 0) then
    begin
      DoStatus('Audioprint::AudioProcessor::Reset() -- No audio channels.');
      Result := False;
      Exit;
    end;

  if (SampleRate <= kMinSampleRate) then
    begin
      DoStatus('Audioprint::AudioProcessor::Reset() -- Sample rate less than 0 1,      kMinSampleRate, sample_rate');
      Result := False;
      Exit;
    end;

  FBufferOffset := 0;

  if (FResampleCTX <> nil) then
    begin
      Dispose(FResampleCTX);
      FResampleCTX := nil;
    end;

  if (SampleRate <> FTargetSampleRate) then
    begin
      FResampleCTX := New(PAVResampleContext);
      FResampleCTX^.Clear;
      FResampleCTX^.Init(FTargetSampleRate, SampleRate, kResampleFilterLength, kResamplePhaseCount, kResampleLinear, kResampleCutoff);
    end;

  FNumChannels := NumChannels;
  Result := True;
end;

procedure TAudioProcessor.Flush;
begin
  if (FBufferOffset <> 0) then
      Resample();
end;

procedure TAudioProcessor.Consume(Input: TI16Vec; AOffset: TLInt; ALength: TLInt);
var
  offset, consumed: TLInt;
  lLength: TLInt;
begin
  lLength := ALength div FNumChannels;
  offset := 0;
  while (lLength > 0) do
    begin
      consumed := Load(Input, offset, lLength);
      offset := offset + consumed * FNumChannels;
      lLength := lLength - consumed;
      if (FBufferSize = FBufferOffset) then
        begin
          Resample();
          if (FBufferSize = FBufferOffset) then
            begin
              // DEBUG("Audioprint::AudioProcessor::Consume() -- Resampling failed?");
              Exit;
            end;
        end;
    end;
end;

procedure TChroma.PrepareNotes(min_freq, max_freq, frame_size, Sample_Rate: TLInt);
var
  i: TLInt;
  freq, octave, note: TLFloat;
  cn: Byte;
begin
  FMinIndex := Math.Max(1, FreqToIndex(min_freq, frame_size, Sample_Rate));
  FMaxIndex := Math.min(frame_size div 2, FreqToIndex(max_freq, frame_size, Sample_Rate));
  for i := FMinIndex to FMaxIndex - 1 do
    begin
      freq := IndexToFreq(i, frame_size, Sample_Rate);
      octave := FreqToOctave(freq, 440.0 / 16.0);
      note := NUM_BANDS * (octave - floor(octave));
      cn := Byte(trunc(note));
      FNotes[i] := cn;
      FNotesFrac[i] := note - FNotes[i];
    end;
end;

constructor TChroma.Create(min_freq, max_freq, frame_size, Sample_Rate: TLInt; consumer: TFeatureVectorConsumer);
begin
  FInterpolate := False;
  SetLength(FNotes, frame_size);
  SetLength(FNotesFrac, frame_size);
  SetLength(FFeatures, NUM_BANDS);
  FConsumer := consumer;
  PrepareNotes(min_freq, max_freq, frame_size, Sample_Rate);
end;

destructor TChroma.Destroy;
begin
  SetLength(FNotes, 0);
  SetLength(FNotesFrac, 0);
  SetLength(FFeatures, 0);
  FConsumer := nil;
  inherited Destroy;
end;

procedure TChroma.Reset;
begin
end;

procedure TChroma.Consume(frame: TFFTFrame);
var
  lNote, i, lNote2, n: TLInt;
  lEnergy, a: TLFloat;
begin
  n := Length(FFeatures);
  for i := 0 to n - 1 do
    begin
      FFeatures[i] := 0.0;
    end;

  for i := FMinIndex to FMaxIndex - 1 do
    begin
      lNote := FNotes[i];
      lEnergy := frame.Energy(i);
      if (FInterpolate) then
        begin
          lNote2 := lNote;
          a := 1.0;
          if (FNotesFrac[i] < 0.5) then
            begin
              lNote2 := (lNote + NUM_BANDS - 1) mod NUM_BANDS;
              a := 0.5 + FNotesFrac[i];
            end;
          if (FNotesFrac[i] > 0.5) then
            begin
              lNote2 := (lNote + 1) mod NUM_BANDS;
              a := 1.5 - FNotesFrac[i];
            end;
          FFeatures[lNote] := FFeatures[lNote] + lEnergy * a;
          FFeatures[lNote2] := FFeatures[lNote2] + lEnergy * (1.0 - a);
        end
      else
        begin
          FFeatures[lNote] := FFeatures[lNote] + lEnergy;
        end;
    end;
  FConsumer.Consume(FFeatures);
end;

constructor TFingerprinter.Create(config: TFingerprinterConfiguration);
begin
  FImage := TCPImage.Create(12, 0);
  if (config = nil) then
      config := TFingerprinterConfigurationTest1.Create;

  FImageBuilder := TImageBuilder.Create(FImage);
  FChromaNormalizer := TChromaNormalizer.Create(FImageBuilder);
  FChromaFilter := TChromaFilter.Create(config.FFilterCoefficients, config.FNumFilterCoefficients, FChromaNormalizer);
  FChroma := TChroma.Create(cMIN_FREQ, cMAX_FREQ, cFRAME_SIZE, cSAMPLE_RATE, FChromaFilter);
  FFFT := TFFT.Create(cFRAME_SIZE, cOVERLAP, FChroma);
  if (config.FRemoveSilence) then
    begin
      FSilenceRemover := TSilenceRemover.Create(FFFT);
      FSilenceRemover.FThreshold := config.FSilenceThreshold;
      FAudioProcessor := TAudioProcessor.Create(cSAMPLE_RATE, FSilenceRemover);
    end
  else
    begin
      FSilenceRemover := nil;
      FAudioProcessor := TAudioProcessor.Create(cSAMPLE_RATE, FFFT);
    end;
  FFingerprintCalculator := TFingerprintCalculator.Create(config.FClassifiers);
  FConfig := config;
end;

destructor TFingerprinter.Destroy;
begin
  DisposeObject(FConfig);
  DisposeObject(FFingerprintCalculator);
  DisposeObject(FAudioProcessor);
  if (FSilenceRemover <> nil) then
      DisposeObject(FSilenceRemover);
  DisposeObject(FFFT);
  DisposeObject(FChroma);
  DisposeObject(FChromaFilter);
  DisposeObject(FChromaNormalizer);
  DisposeObject(FImageBuilder);
  DisposeObject(FImage);

  inherited Destroy;
end;

function TFingerprinter.SetOption(const Name: TPascalString; Value: TLInt): boolean;
begin
  Result := False;
  if Name.Same('silence_threshold') then
    begin
      if (FSilenceRemover <> nil) then
        begin
          FSilenceRemover.FThreshold := Value;
          Result := True;
        end;
    end;
end;

function TFingerprinter.Start(Sample_Rate, NumChannels: TLInt): boolean;
begin
  if (not FAudioProcessor.Reset(Sample_Rate, NumChannels)) then
    begin
      // FIXME save error message somewhere
      Result := False;
      Exit;
    end;
  FFFT.Reset();
  FChroma.Reset();
  FChromaFilter.Reset();
  FChromaNormalizer.Reset();
  if FImage <> nil then
      DisposeObject(FImage);
  FImage := TCPImage.Create(12);
  FImageBuilder.Reset(FImage);
  Result := True;
end;

function TFingerprinter.Finish: TU32Vec;
begin
  FAudioProcessor.Flush();
  Result := FFingerprintCalculator.Calculate(FImage);
end;

procedure TFingerprinter.Consume(Input: TI16Vec; AOffset: TLInt; len: TLInt);
begin
  FAudioProcessor.Consume(Input, AOffset, len);
end;

constructor TAudioprint.Create;
begin
  inherited Create;
end;

destructor TAudioprint.Destroy;
begin
  SetLength(ctx.fingerprint, 0);
  if ctx.Engine <> nil then
      DisposeObject(ctx.Engine);
  inherited Destroy;
end;

function TAudioprint.New(algorithm: TAudioprintAlgorithm): boolean;
begin
  SetLength(ctx.fingerprint, 0);
  ctx.algorithm := algorithm;
  if ctx.Engine <> nil then
      DisposeObject(ctx.Engine);
  ctx.Engine := TFingerprinter.Create(CreateFingerprinterConfiguration(ctx.algorithm));
  Result := ctx.Engine <> nil;
end;

function TAudioprint.GetAlgorithm: TAudioprintAlgorithm;
begin
  Result := ctx.algorithm;
end;

function TAudioprint.SetOption(const Name: TPascalString; Value: TLInt): boolean;
begin
  Result := TFingerprinter(ctx.Engine).SetOption(Name, Value);
end;

function TAudioprint.Start(Sample_Rate: TLInt; NumChannels: TLInt): boolean;
begin
  Result := TFingerprinter(ctx.Engine).Start(Sample_Rate, NumChannels);
end;

function TAudioprint.Feed(Data: TI16Vec; len: TLInt): boolean;
begin
  // data: raw audio data, should point to an array of 16-bit signed integers in native byte-order
  TFingerprinter(ctx.Engine).Consume(Data, 0, len);
  Result := True;
end;

function TAudioprint.Finish: boolean;
begin
  ctx.fingerprint := TFingerprinter(ctx.Engine).Finish;
  Result := True;
end;

function TAudioprint.GetFingerprint(out fingerprint: TPascalString): boolean;
begin
  umlEncodeLineBASE64(CompressFingerprint(ctx.fingerprint, Ord(ctx.algorithm)), fingerprint);
  Result := True;
end;

function TAudioprint.GetRawFingerprint(out fingerprint: TU32Vec; out Size: TLInt): boolean;
begin
  fingerprint := ctx.fingerprint;
  Size := Length(ctx.fingerprint);
  Result := True;
end;

procedure TAudioprint.EnocdeFingerprint(RawFP: TU32Vec; algorithm: TLInt;
  var EncodedFP: TPascalString; var EncodedSize: TLInt; Base64: boolean);
var
  compressed: TPascalString;
begin
  compressed := CompressFingerprint(RawFP, algorithm);
  if not Base64 then
    begin
      EncodedFP := compressed;
      EncodedSize := EncodedFP.len;
    end
  else
    begin
      umlEncodeLineBASE64(compressed, EncodedFP);
      EncodedSize := EncodedFP.len;
    end;
end;

procedure TAudioprint.DecodeFingerprint(encoded: TPascalString; var uncompressed: TU32Vec;
  var algorithm: TLInt; Base64: boolean);
var
  lCompressed: TPascalString;
begin
  if Base64 then
      umlDecodeLineBASE64(encoded, lCompressed)
  else
      lCompressed := encoded;
  uncompressed := DecompressFingerprint(lCompressed, algorithm);
end;

end.
