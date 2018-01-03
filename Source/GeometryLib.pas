{ ****************************************************************************** }
{ * geometry 3D library writen by QQ 600585@qq.com                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }

unit GeometryLib;

{$I zDefine.inc}

interface

type
  THalfFloat = type Word;
  PHalfFloat = ^THalfFloat;

  TVector2d  = array [0 .. 1] of double;
  TVector2f  = array [0 .. 1] of single;
  TVector2h  = array [0 .. 1] of THalfFloat;
  TVector2i  = array [0 .. 1] of Integer;
  TVector2ui = array [0 .. 1] of Cardinal;
  TVector2s  = array [0 .. 1] of smallint;
  TVector2b  = array [0 .. 1] of byte;
  TVector2sb = array [0 .. 1] of ShortInt;
  TVector2e  = array [0 .. 1] of Extended;
  TVector2w  = array [0 .. 1] of Word;
  TVector2p  = array [0 .. 1] of Pointer;

  TVector3d  = array [0 .. 2] of double;
  TVector3f  = array [0 .. 2] of single;
  TVector3h  = array [0 .. 2] of THalfFloat;
  TVector3i  = array [0 .. 2] of Integer;
  TVector3ui = array [0 .. 2] of Cardinal;
  TVector3s  = array [0 .. 2] of smallint;
  TVector3b  = array [0 .. 2] of byte;
  TVector3sb = array [0 .. 2] of ShortInt;
  TVector3e  = array [0 .. 2] of Extended;
  TVector3w  = array [0 .. 2] of Word;
  TVector3p  = array [0 .. 2] of Pointer;

  TVector4d  = array [0 .. 3] of double;
  TVector4f  = array [0 .. 3] of single;
  TVector4h  = array [0 .. 3] of THalfFloat;
  TVector4i  = array [0 .. 3] of Integer;
  TVector4ui = array [0 .. 3] of Cardinal;
  TVector4s  = array [0 .. 3] of smallint;
  TVector4b  = array [0 .. 3] of byte;
  TVector4sb = array [0 .. 3] of ShortInt;
  TVector4e  = array [0 .. 3] of Extended;
  TVector4w  = array [0 .. 3] of Word;
  TVector4p  = array [0 .. 3] of Pointer;

  TMatrix2d = array [0 .. 1] of TVector2d;
  TMatrix2f = array [0 .. 1] of TVector2f;
  TMatrix2i = array [0 .. 1] of TVector2i;
  TMatrix2s = array [0 .. 1] of TVector2s;
  TMatrix2b = array [0 .. 1] of TVector2b;
  TMatrix2e = array [0 .. 1] of TVector2e;
  TMatrix2w = array [0 .. 1] of TVector2w;
  TMatrix2p = array [0 .. 1] of TVector2p;

  TMatrix3d = array [0 .. 2] of TVector3d;
  TMatrix3f = array [0 .. 2] of TVector3f;
  TMatrix3i = array [0 .. 2] of TVector3i;
  TMatrix3s = array [0 .. 2] of TVector3s;
  TMatrix3b = array [0 .. 2] of TVector3b;
  TMatrix3e = array [0 .. 2] of TVector3e;
  TMatrix3w = array [0 .. 2] of TVector3w;
  TMatrix3p = array [0 .. 2] of TVector3p;

  TMatrix4d = array [0 .. 3] of TVector4d;
  TMatrix4f = array [0 .. 3] of TVector4f;
  TMatrix4i = array [0 .. 3] of TVector4i;
  TMatrix4s = array [0 .. 3] of TVector4s;
  TMatrix4b = array [0 .. 3] of TVector4b;
  TMatrix4e = array [0 .. 3] of TVector4e;
  TMatrix4w = array [0 .. 3] of TVector4w;
  TMatrix4p = array [0 .. 3] of TVector4p;

const
  cMaxArray     = (MaxInt shr 4);
  cColinearBias = 1E-8;

type
  // data types needed for 3D graphics calculation,
  // included are 'C like' aliases for each type (to be
  // conformal with OpenGL types)
  PFloat = PSingle;

  PTexPoint = ^TTexPoint;

  TTexPoint = packed record
    S, T: single;
  end;

  // types to specify continous streams of a specific type
  // switch off range checking to access values beyond the limits
  PByteVector     = ^TByteVector;
  PByteArray      = PByteVector;
  TByteVector     = array [0 .. cMaxArray] of byte;
  PWordVector     = ^TWordVector;
  TWordVector     = array [0 .. cMaxArray] of Word;
  PIntegerVector  = ^TIntegerVector;
  PIntegerArray   = PIntegerVector;
  TIntegerVector  = array [0 .. cMaxArray] of Integer;
  PFloatVector    = ^TFloatVector;
  PFloatArray     = PFloatVector;
  PSingleArray    = PFloatArray;
  TFloatVector    = array [0 .. cMaxArray] of single;
  TSingleArray    = array of single;
  PDoubleVector   = ^TDoubleVector;
  PDoubleArray    = PDoubleVector;
  TDoubleVector   = array [0 .. cMaxArray] of double;
  PPointerVector  = ^TPointerVector;
  PPointerArray   = PPointerVector;
  TPointerVector  = array [0 .. cMaxArray] of Pointer;
  PCardinalVector = ^TCardinalVector;
  PCardinalArray  = PCardinalVector;
  TCardinalVector = array [0 .. cMaxArray] of Cardinal;

  // common vector and matrix types with predefined limits
  // indices correspond like: x -> 0
  // y -> 1
  // z -> 2
  // w -> 3

  PHomogeneousByteVector = ^THomogeneousByteVector;
  THomogeneousByteVector = TVector4b;
  PHomogeneousWordVector = ^THomogeneousWordVector;
  THomogeneousWordVector = TVector4w;
  PHomogeneousIntVector  = ^THomogeneousIntVector;
  THomogeneousIntVector  = TVector4i;
  PHomogeneousFltVector  = ^THomogeneousFltVector;
  THomogeneousFltVector  = TVector4f;
  PHomogeneousDblVector  = ^THomogeneousDblVector;
  THomogeneousDblVector  = TVector4d;
  PHomogeneousExtVector  = ^THomogeneousExtVector;
  THomogeneousExtVector  = TVector4e;
  PHomogeneousPtrVector  = ^THomogeneousPtrVector;
  THomogeneousPtrVector  = TVector4p;
  PAffineByteVector      = ^TAffineByteVector;
  TAffineByteVector      = TVector3b;
  PAffineWordVector      = ^TAffineWordVector;
  TAffineWordVector      = TVector3w;
  PAffineIntVector       = ^TAffineIntVector;
  TAffineIntVector       = TVector3i;
  PAffineFltVector       = ^TAffineFltVector;
  TAffineFltVector       = TVector3f;
  PAffineDblVector       = ^TAffineDblVector;
  TAffineDblVector       = TVector3d;
  PAffineExtVector       = ^TAffineExtVector;
  TAffineExtVector       = TVector3e;
  PAffinePtrVector       = ^TAffinePtrVector;
  TAffinePtrVector       = TVector3p;
  PVector2f              = ^TVector2f;

  // some simplified names
  PVector            = ^TVector;
  TVector            = THomogeneousFltVector;
  PHomogeneousVector = ^THomogeneousVector;
  THomogeneousVector = THomogeneousFltVector;
  PAffineVector      = ^TAffineVector;
  TAffineVector      = TVector3f;
  PVertex            = ^TVertex;
  TVertex            = TAffineVector;
  // arrays of vectors
  PAffineVectorArray = ^TAffineVectorArray;
  TAffineVectorArray = array [0 .. MaxInt shr 4] of TAffineVector;
  PVectorArray       = ^TVectorArray;
  TVectorArray       = array [0 .. MaxInt shr 5] of TVector;
  PTexPointArray     = ^TTexPointArray;
  TTexPointArray     = array [0 .. MaxInt shr 4] of TTexPoint;
  // matrices
  THomogeneousByteMatrix = TMatrix4b;
  THomogeneousWordMatrix = array [0 .. 3] of THomogeneousWordVector;
  THomogeneousIntMatrix  = TMatrix4i;
  THomogeneousFltMatrix  = TMatrix4f;
  THomogeneousDblMatrix  = TMatrix4d;
  THomogeneousExtMatrix  = array [0 .. 3] of THomogeneousExtVector;
  TAffineByteMatrix      = TMatrix3b;
  TAffineWordMatrix      = array [0 .. 2] of TAffineWordVector;
  TAffineIntMatrix       = TMatrix3i;
  TAffineFltMatrix       = TMatrix3f;
  TAffineDblMatrix       = TMatrix3d;
  TAffineExtMatrix       = array [0 .. 2] of TAffineExtVector;

  // some simplified names
  PMatrix            = ^TMatrix;
  TMatrix            = THomogeneousFltMatrix;
  TMatrixArray       = array [0 .. MaxInt shr 7] of TMatrix;
  PMatrixArray       = ^TMatrixArray;
  PHomogeneousMatrix = ^THomogeneousMatrix;
  THomogeneousMatrix = THomogeneousFltMatrix;
  PAffineMatrix      = ^TAffineMatrix;
  TAffineMatrix      = TAffineFltMatrix;

  { : A plane equation.<p>
    Defined by its equation A.x+B.y+C.z+D<p>, a plane can be mapped to the
    homogeneous space coordinates, and this is what we are doing here.<br>
    The typename is just here for easing up data manipulation. }
  THmgPlane       = TVector;
  TDoubleHmgPlane = THomogeneousDblVector;

  // q = ([x, y, z], w)
  PQuaternion = ^TQuaternion;

  TQuaternion = record
    ImagPart: TAffineVector;
    RealPart: single;
  end;

  PQuaternionArray = ^TQuaternionArray;
  TQuaternionArray = array [0 .. MaxInt shr 5] of TQuaternion;

  TRectangle = record
    Left, Top, Width, Height: Integer;
  end;

  TFrustum = record
    pLeft, pTop, pRight, pBottom, pNear, pFar: THmgPlane;
  end;

  TTransType = (ttScaleX, ttScaleY, ttScaleZ,
    ttShearXY, ttShearXZ, ttShearYZ,
    ttRotateX, ttRotateY, ttRotateZ,
    ttTranslateX, ttTranslateY, ttTranslateZ,
    ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW);

  // used to describe a sequence of transformations in following order:
  // [Sx][Sy][Sz][ShearXY][ShearXZ][ShearZY][Rx][Ry][Rz][Tx][Ty][Tz][P(x,y,z,w)]
  // constants are declared for easier access (see MatrixDecompose below)
  TTransformations = array [TTransType] of single;

  TPackedRotationMatrix = array [0 .. 2] of smallint;

const
  // useful constants

  // TexPoints (2D space)
  XTexPoint: TTexPoint    = (S: 1; T: 0);
  YTexPoint: TTexPoint    = (S: 0; T: 1);
  XYTexPoint: TTexPoint   = (S: 1; T: 1);
  NullTexPoint: TTexPoint = (S: 0; T: 0);
  MidTexPoint: TTexPoint  = (S: 0.5; T: 0.5);

  // standard vectors
  XVector: TAffineVector      = (1, 0, 0);
  YVector: TAffineVector      = (0, 1, 0);
  ZVector: TAffineVector      = (0, 0, 1);
  XYVector: TAffineVector     = (1, 1, 0);
  XZVector: TAffineVector     = (1, 0, 1);
  YZVector: TAffineVector     = (0, 1, 1);
  XYZVector: TAffineVector    = (1, 1, 1);
  NullVector: TAffineVector   = (0, 0, 0);
  MinusXVector: TAffineVector = (-1, 0, 0);
  MinusYVector: TAffineVector = (0, -1, 0);
  MinusZVector: TAffineVector = (0, 0, -1);
  // standard homogeneous vectors
  XHmgVector: THomogeneousVector  = (1, 0, 0, 0);
  YHmgVector: THomogeneousVector  = (0, 1, 0, 0);
  ZHmgVector: THomogeneousVector  = (0, 0, 1, 0);
  WHmgVector: THomogeneousVector  = (0, 0, 0, 1);
  XYHmgVector: THomogeneousVector = (1, 1, 0, 0);
  YZHmgVector: THomogeneousVector = (0, 1, 1, 0);
  XZHmgVector: THomogeneousVector = (1, 0, 1, 0);

  XYZHmgVector: THomogeneousVector  = (1, 1, 1, 0);
  XYZWHmgVector: THomogeneousVector = (1, 1, 1, 1);
  NullHmgVector: THomogeneousVector = (0, 0, 0, 0);
  // standard homogeneous points
  XHmgPoint: THomogeneousVector    = (1, 0, 0, 1);
  YHmgPoint: THomogeneousVector    = (0, 1, 0, 1);
  ZHmgPoint: THomogeneousVector    = (0, 0, 1, 1);
  WHmgPoint: THomogeneousVector    = (0, 0, 0, 1);
  NullHmgPoint: THomogeneousVector = (0, 0, 0, 1);

  IdentityMatrix: TAffineMatrix = ((1, 0, 0),
    (0, 1, 0),
    (0, 0, 1));
  IdentityHmgMatrix: TMatrix = ((1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1));
  IdentityHmgDblMatrix: THomogeneousDblMatrix = ((1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1));
  EmptyMatrix: TAffineMatrix = ((0, 0, 0),
    (0, 0, 0),
    (0, 0, 0));
  EmptyHmgMatrix: TMatrix = ((0, 0, 0, 0),
    (0, 0, 0, 0),
    (0, 0, 0, 0),
    (0, 0, 0, 0));

  // Quaternions

  IdentityQuaternion: TQuaternion = (ImagPart: (0, 0, 0); RealPart: 1);

  // some very small numbers
  EPSILON: single  = 1E-40;
  EPSILON2: single = 1E-30;

  // ------------------------------------------------------------------------------
  // Vector functions
  // ------------------------------------------------------------------------------

function TexPointMake(const S, T: single): TTexPoint; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function AffineVectorMake(const x, y, z: single): TAffineVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function AffineVectorMake(const v: TVector): TAffineVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure SetAffineVector(out v: TAffineVector; const x, y, z: single); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure SetVector(out v: TAffineVector; const x, y, z: single); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure SetVector(out v: TAffineVector; const vSrc: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure SetVector(out v: TAffineVector; const vSrc: TAffineVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure SetVector(out v: TAffineDblVector; const vSrc: TAffineVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure SetVector(out v: TAffineDblVector; const vSrc: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorMake(const v: TAffineVector; w: single = 0): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorMake(const x, y, z: single; w: single = 0): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function PointMake(const x, y, z: single): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function PointMake(const v: TAffineVector): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function PointMake(const v: TVector): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure SetVector(out v: TVector; const x, y, z: single; w: single = 0); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure SetVector(out v: TVector; const av: TAffineVector; w: single = 0); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure SetVector(out v: TVector; const vSrc: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure MakePoint(out v: TVector; const x, y, z: single); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure MakePoint(out v: TVector; const av: TAffineVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure MakePoint(out v: TVector; const av: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure MakeVector(out v: TAffineVector; const x, y, z: single); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure MakeVector(out v: TVector; const x, y, z: single); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure MakeVector(out v: TVector; const av: TAffineVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure MakeVector(out v: TVector; const av: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure RstVector(var v: TAffineVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure RstVector(var v: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// 2
function VectorEquals(const V1, V2: TVector2f): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector2i): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector2d): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector2s): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector2b): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// 3
// function VectorEquals(const V1, V2: TVector3f): Boolean; overload; //declared further
function VectorEquals(const V1, V2: TVector3i): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector3d): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector3s): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector3b): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// 4
// function VectorEquals(const V1, V2: TVector4f): Boolean; overload; //declared further
function VectorEquals(const V1, V2: TVector4i): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector4d): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector4s): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorEquals(const V1, V2: TVector4b): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// 3x3
function MatrixEquals(const Matrix1, Matrix2: TMatrix3f): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix3i): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix3d): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix3s): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix3b): Boolean; overload;

// 4x4
function MatrixEquals(const Matrix1, Matrix2: TMatrix4f): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix4i): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix4d): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix4s): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix4b): Boolean; overload;

// 2x
function Vector2fMake(const x, y: single): TVector2f; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2iMake(const x, y: Integer): TVector2i; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2sMake(const x, y: smallint): TVector2s; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2dMake(const x, y: double): TVector2d; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2bMake(const x, y: byte): TVector2b; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function Vector2fMake(const Vector: TVector3f): TVector2f; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2iMake(const Vector: TVector3i): TVector2i; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2sMake(const Vector: TVector3s): TVector2s; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2dMake(const Vector: TVector3d): TVector2d; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2bMake(const Vector: TVector3b): TVector2b; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function Vector2fMake(const Vector: TVector4f): TVector2f; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2iMake(const Vector: TVector4i): TVector2i; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2sMake(const Vector: TVector4s): TVector2s; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2dMake(const Vector: TVector4d): TVector2d; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector2bMake(const Vector: TVector4b): TVector2b; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// 3x
function Vector3fMake(const x: single; const y: single = 0; const z: single = 0): TVector3f; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3iMake(const x: Integer; const y: Integer = 0; const z: Integer = 0): TVector3i; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3sMake(const x: smallint; const y: smallint = 0; const z: smallint = 0): TVector3s; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3dMake(const x: double; const y: double = 0; const z: double = 0): TVector3d; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3bMake(const x: byte; const y: byte = 0; const z: byte = 0): TVector3b; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function Vector3fMake(const Vector: TVector2f; const z: single = 0): TVector3f; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3iMake(const Vector: TVector2i; const z: Integer = 0): TVector3i; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3sMake(const Vector: TVector2s; const z: smallint = 0): TVector3s; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3dMake(const Vector: TVector2d; const z: double = 0): TVector3d; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3bMake(const Vector: TVector2b; const z: byte = 0): TVector3b; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function Vector3fMake(const Vector: TVector4f): TVector3f; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3iMake(const Vector: TVector4i): TVector3i; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3sMake(const Vector: TVector4s): TVector3s; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3dMake(const Vector: TVector4d): TVector3d; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector3bMake(const Vector: TVector4b): TVector3b; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// 4x
function Vector4fMake(const x: single; const y: single = 0; const z: single = 0; const w: single = 0): TVector4f; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4iMake(const x: Integer; const y: Integer = 0; const z: Integer = 0; const w: Integer = 0): TVector4i; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4sMake(const x: smallint; const y: smallint = 0; const z: smallint = 0; const w: smallint = 0): TVector4s; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4dMake(const x: double; const y: double = 0; const z: double = 0; const w: double = 0): TVector4d; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4bMake(const x: byte; const y: byte = 0; const z: byte = 0; const w: byte = 0): TVector4b; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function Vector4fMake(const Vector: TVector3f; const w: single = 0): TVector4f; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4iMake(const Vector: TVector3i; const w: Integer = 0): TVector4i; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4sMake(const Vector: TVector3s; const w: smallint = 0): TVector4s; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4dMake(const Vector: TVector3d; const w: double = 0): TVector4d; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4bMake(const Vector: TVector3b; const w: byte = 0): TVector4b; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function Vector4fMake(const Vector: TVector2f; const z: single = 0; const w: single = 0): TVector4f; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4iMake(const Vector: TVector2i; const z: Integer = 0; const w: Integer = 0): TVector4i; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4sMake(const Vector: TVector2s; const z: smallint = 0; const w: smallint = 0): TVector4s; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4dMake(const Vector: TVector2d; const z: double = 0; const w: double = 0): TVector4d; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Vector4bMake(const Vector: TVector2b; const z: byte = 0; const w: byte = 0): TVector4b; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// : Vector comparison functions:
// ComparedVector
// 3f
function VectorMoreThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;

function VectorLessThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
// 4f
function VectorMoreThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;

function VectorLessThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
// 3i
function VectorMoreThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;

function VectorLessThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
// 4i
function VectorMoreThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;

function VectorLessThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;

// 3s
function VectorMoreThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;

function VectorLessThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
// 4s
function VectorMoreThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;

function VectorLessThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;

// ComparedNumber
// 3f
function VectorMoreThen(const SourceVector: TVector3f; const ComparedNumber: single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector3f; const ComparedNumber: single): Boolean; overload;

function VectorLessThen(const SourceVector: TVector3f; const ComparedNumber: single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector3f; const ComparedNumber: single): Boolean; overload;
// 4f
function VectorMoreThen(const SourceVector: TVector4f; const ComparedNumber: single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector4f; const ComparedNumber: single): Boolean; overload;

function VectorLessThen(const SourceVector: TVector4f; const ComparedNumber: single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector4f; const ComparedNumber: single): Boolean; overload;
// 3i
function VectorMoreThen(const SourceVector: TVector3i; const ComparedNumber: single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector3i; const ComparedNumber: single): Boolean; overload;

function VectorLessThen(const SourceVector: TVector3i; const ComparedNumber: single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector3i; const ComparedNumber: single): Boolean; overload;
// 4i
function VectorMoreThen(const SourceVector: TVector4i; const ComparedNumber: single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector4i; const ComparedNumber: single): Boolean; overload;

function VectorLessThen(const SourceVector: TVector4i; const ComparedNumber: single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector4i; const ComparedNumber: single): Boolean; overload;
// 3s
function VectorMoreThen(const SourceVector: TVector3s; const ComparedNumber: single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector3s; const ComparedNumber: single): Boolean; overload;

function VectorLessThen(const SourceVector: TVector3s; const ComparedNumber: single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector3s; const ComparedNumber: single): Boolean; overload;
// 4s
function VectorMoreThen(const SourceVector: TVector4s; const ComparedNumber: single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector4s; const ComparedNumber: single): Boolean; overload;

function VectorLessThen(const SourceVector: TVector4s; const ComparedNumber: single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector4s; const ComparedNumber: single): Boolean; overload;

function VectorAdd(const V1, V2: TVector2f): TVector2f; overload;
// : Returns the sum of two affine vectors
function VectorAdd(const V1, V2: TAffineVector): TAffineVector; overload;
// : Adds two vectors and places result in vr
procedure VectorAdd(const V1, V2: TAffineVector; var vr: TAffineVector); overload;
procedure VectorAdd(const V1, V2: TAffineVector; vr: PAffineVector); overload;
// : Returns the sum of two homogeneous vectors
function VectorAdd(const V1, V2: TVector): TVector; overload;
procedure VectorAdd(const V1, V2: TVector; var vr: TVector); overload;
// : Sums up f to each component of the vector
function VectorAdd(const v: TAffineVector; const f: single): TAffineVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Sums up f to each component of the vector
function VectorAdd(const v: TVector; const f: single): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Adds V2 to V1, result is placed in V1
procedure AddVector(var V1: TAffineVector; const V2: TAffineVector); overload;
// : Adds V2 to V1, result is placed in V1
procedure AddVector(var V1: TAffineVector; const V2: TVector); overload;
// : Adds V2 to V1, result is placed in V1
procedure AddVector(var V1: TVector; const V2: TVector); overload;
// : Sums up f to each component of the vector
procedure AddVector(var v: TAffineVector; const f: single); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Sums up f to each component of the vector
procedure AddVector(var v: TVector; const f: single); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// : Adds V2 to V1, result is placed in V1. W coordinate is always 1.
procedure AddPoint(var V1: TVector; const V2: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// : Returns the sum of two homogeneous vectors. W coordinate is always 1.
function PointAdd(var V1: TVector; const V2: TVector): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// : Adds delta to nb texpoints in src and places result in dest
procedure TexPointArrayAdd(const src: PTexPointArray; const delta: TTexPoint;
  const nb: Integer;
  dest: PTexPointArray); overload;
procedure TexPointArrayScaleAndAdd(const src: PTexPointArray; const delta: TTexPoint;
  const nb: Integer; const scale: TTexPoint;
  dest: PTexPointArray); overload;
// : Adds delta to nb vectors in src and places result in dest
procedure VectorArrayAdd(const src: PAffineVectorArray; const delta: TAffineVector;
  const nb: Integer;
  dest: PAffineVectorArray); overload;

// : Returns V1-V2
function VectorSubtract(const V1, V2: TVector2f): TVector2f; overload;
// : Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1: TVector2f; const V2: TVector2f); overload;

// : Returns V1-V2
function VectorSubtract(const V1, V2: TAffineVector): TAffineVector; overload;
// : Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1, V2: TAffineVector; var result: TAffineVector); overload;
// : Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1, V2: TAffineVector; var result: TVector); overload;
// : Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1: TVector; V2: TAffineVector; var result: TVector); overload;
// : Returns V1-V2
function VectorSubtract(const V1, V2: TVector): TVector; overload;
// : Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1, V2: TVector; var result: TVector); overload;
// : Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1, V2: TVector; var result: TAffineVector); overload;
function VectorSubtract(const V1: TAffineVector; delta: single): TAffineVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorSubtract(const V1: TVector; delta: single): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1: TAffineVector; const V2: TAffineVector); overload;
// : Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1: TVector; const V2: TVector); overload;

// : Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr: TAffineVector; const v: TAffineVector; var f: single); overload;
procedure CombineVector(var vr: TAffineVector; const v: TAffineVector; pf: PFloat); overload;
// : Makes a linear combination of two texpoints
function TexPointCombine(const t1, t2: TTexPoint; f1, f2: single): TTexPoint; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Makes a linear combination of two vectors and return the result
function VectorCombine(const V1, V2: TAffineVector; const f1, f2: single): TAffineVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Makes a linear combination of three vectors and return the result
function VectorCombine3(const V1, V2, V3: TAffineVector; const f1, f2, F3: single): TAffineVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure VectorCombine3(const V1, V2, V3: TAffineVector; const f1, f2, F3: single; var vr: TAffineVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// : Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr: TVector; const v: TVector; var f: single); overload;
// : Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr: TVector; const v: TAffineVector; var f: single); overload;
// : Makes a linear combination of two vectors and return the result
function VectorCombine(const V1, V2: TVector; const f1, f2: single): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Makes a linear combination of two vectors and return the result
function VectorCombine(const V1: TVector; const V2: TAffineVector; const f1, f2: single): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Makes a linear combination of two vectors and place result in vr
procedure VectorCombine(const V1: TVector; const V2: TAffineVector; const f1, f2: single; var vr: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Makes a linear combination of two vectors and place result in vr
procedure VectorCombine(const V1, V2: TVector; const f1, f2: single; var vr: TVector); overload;
// : Makes a linear combination of two vectors and place result in vr, F1=1.0
procedure VectorCombine(const V1, V2: TVector; const f2: single; var vr: TVector); overload;
// : Makes a linear combination of three vectors and return the result
function VectorCombine3(const V1, V2, V3: TVector; const f1, f2, F3: single): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Makes a linear combination of three vectors and return the result
procedure VectorCombine3(const V1, V2, V3: TVector; const f1, f2, F3: single; var vr: TVector); overload;

{ : Calculates the dot product between V1 and V2.<p>
  Result:=V1[X] * V2[X] + V1[Y] * V2[Y] }
function VectorDotProduct(const V1, V2: TVector2f): single; overload;
{ : Calculates the dot product between V1 and V2.<p>
  Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] }
function VectorDotProduct(const V1, V2: TAffineVector): single; overload;
{ : Calculates the dot product between V1 and V2.<p>
  Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] }
function VectorDotProduct(const V1, V2: TVector): single; overload;
{ : Calculates the dot product between V1 and V2.<p>
  Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] }
function VectorDotProduct(const V1: TVector; const V2: TAffineVector): single; overload;

{ : Projects p on the line defined by o and direction.<p>
  Performs VectorDotProduct(VectorSubtract(p, origin), direction), which,
  if direction is normalized, computes the distance between origin and the
  projection of p on the (origin, direction) line. }
function PointProject(const p, origin, direction: TAffineVector): single; overload;
function PointProject(const p, origin, direction: TVector): single; overload;

// : Calculates the cross product between vector 1 and 2
function VectorCrossProduct(const V1, V2: TAffineVector): TAffineVector; overload;
// : Calculates the cross product between vector 1 and 2
function VectorCrossProduct(const V1, V2: TVector): TVector; overload;
// : Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const V1, V2: TVector; var vr: TVector); overload;
// : Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const V1, V2: TAffineVector; var vr: TVector); overload;
// : Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const V1, V2: TVector; var vr: TAffineVector); overload;
// : Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const V1, V2: TAffineVector; var vr: TAffineVector); overload;

// : Calculates linear interpolation between start and stop at point t
function Lerp(const start, stop, T: single): single; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Calculates angular interpolation between start and stop at point t
function AngleLerp(start, stop, T: single): single; {$IFDEF INLINE_ASM}inline; {$ENDIF}
{ : This is used for interpolating between 2 matrices. The result
  is used to reposition the model parts each frame. }
function MatrixLerp(const m1, m2: TMatrix; const delta: single): TMatrix;

{ : Calculates the angular distance between two angles in radians.<p>
  Result is in the [0; PI] range. }
function DistanceBetweenAngles(angle1, angle2: single): single;

// : Calculates linear interpolation between texpoint1 and texpoint2 at point t
function TexPointLerp(const t1, t2: TTexPoint; T: single): TTexPoint; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Calculates linear interpolation between vector1 and vector2 at point t
function VectorLerp(const V1, V2: TAffineVector; T: single): TAffineVector; overload;
// : Calculates linear interpolation between vector1 and vector2 at point t, places result in vr
procedure VectorLerp(const V1, V2: TAffineVector; T: single; var vr: TAffineVector); overload;
// : Calculates linear interpolation between vector1 and vector2 at point t
function VectorLerp(const V1, V2: TVector; T: single): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Calculates linear interpolation between vector1 and vector2 at point t, places result in vr
procedure VectorLerp(const V1, V2: TVector; T: single; var vr: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function VectorAngleLerp(const V1, V2: TAffineVector; T: single): TAffineVector; overload;
function VectorAngleCombine(const V1, V2: TAffineVector; f: single): TAffineVector; overload;

// : Calculates linear interpolation between vector arrays
procedure VectorArrayLerp(const src1, src2: PVectorArray; T: single; n: Integer; dest: PVectorArray); overload;
procedure VectorArrayLerp(const src1, src2: PAffineVectorArray; T: single; n: Integer; dest: PAffineVectorArray); overload;
procedure VectorArrayLerp(const src1, src2: PTexPointArray; T: single; n: Integer; dest: PTexPointArray); overload;

type
  TGLInterpolationType = (itLinear, itPower, itSin, itSinAlt, itTan, itLn, itExp);

  { : There functions that do the same as "Lerp", but add some distortions. }
function InterpolatePower(const start, stop, delta: single; const DistortionDegree: single): single;
function InterpolateLn(const start, stop, delta: single; const DistortionDegree: single): single;
function InterpolateExp(const start, stop, delta: single; const DistortionDegree: single): single;

{ : Only valid where Delta belongs to [0..1] }
function InterpolateSin(const start, stop, delta: single): single;
function InterpolateTan(const start, stop, delta: single): single;

{ : "Alt" functions are valid everywhere }
function InterpolateSinAlt(const start, stop, delta: single): single;

function InterpolateCombinedFastPower(const OriginalStart, OriginalStop, OriginalCurrent: single; const TargetStart, TargetStop: single; const DistortionDegree: single): single;
function InterpolateCombinedSafe(const OriginalStart, OriginalStop, OriginalCurrent: single; const TargetStart, TargetStop: single; const DistortionDegree: single; const InterpolationType: TGLInterpolationType): single;
function InterpolateCombinedFast(const OriginalStart, OriginalStop, OriginalCurrent: single; const TargetStart, TargetStop: single; const DistortionDegree: single; const InterpolationType: TGLInterpolationType): single;
function InterpolateCombined(const start, stop, delta: single; const DistortionDegree: single; const InterpolationType: TGLInterpolationType): single;

{ : Calculates the length of a vector following the equation sqrt(x*x+y*y). }
function VectorLength(const x, y: single): single; overload;
{ : Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z). }
function VectorLength(const x, y, z: single): single; overload;
// : Calculates the length of a vector following the equation sqrt(x*x+y*y).
function VectorLength(const v: TVector2f): single; overload;
// : Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z).
function VectorLength(const v: TAffineVector): single; overload;
// : Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z+w*w).
function VectorLength(const v: TVector): single; overload;
{ : Calculates the length of a vector following the equation: sqrt(x*x+y*y+...).<p>
  Note: The parameter of this function is declared as open array. Thus
  there's no restriction about the number of the components of the vector. }
function VectorLength(const v: array of single): single; overload;

{ : Calculates norm of a vector which is defined as norm = x * x + y * y<p>
  Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(const x, y: single): single; overload;
{ : Calculates norm of a vector which is defined as norm = x*x + y*y + z*z<p>
  Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(const v: TAffineVector): single; overload;
{ : Calculates norm of a vector which is defined as norm = x*x + y*y + z*z<p>
  Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(const v: TVector): single; overload;
{ : Calculates norm of a vector which is defined as norm = v[0]*v[0] + ...<p>
  Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(var v: array of single): single; overload;

// : Transforms a vector to unit length
procedure NormalizeVector(var v: TVector2f); overload;
// : Returns the vector transformed to unit length
// : Transforms a vector to unit length
procedure NormalizeVector(var v: TAffineVector); overload;
// : Transforms a vector to unit length
procedure NormalizeVector(var v: TVector); overload;
// : Returns the vector transformed to unit length
function VectorNormalize(const v: TVector2f): TVector2f; overload;
// : Returns the vector transformed to unit length
function VectorNormalize(const v: TAffineVector): TAffineVector; overload;
// : Returns the vector transformed to unit length (w component dropped)
function VectorNormalize(const v: TVector): TVector; overload;

// : Transforms vectors to unit length
procedure NormalizeVectorArray(list: PAffineVectorArray; n: Integer); overload;

{ : Calculates the cosine of the angle between Vector1 and Vector2.<p>
  Result = DotProduct(V1, V2) / (Length(V1) * Length(V2)) }
function VectorAngleCosine(const V1, V2: TAffineVector): single; overload;

{ : Calculates the cosine of the angle between Vector1 and Vector2.<p>
  Result = DotProduct(V1, V2) / (Length(V1) * Length(V2)) }
function VectorAngleCosine(const V1, V2: TVector): single; overload;

// : Negates the vector
function VectorNegate(const v: TAffineVector): TAffineVector; overload;
function VectorNegate(const v: TVector): TVector; overload;

// : Negates the vector
procedure NegateVector(var v: TAffineVector); overload;
// : Negates the vector
procedure NegateVector(var v: TVector); overload;
// : Negates the vector
procedure NegateVector(var v: array of single); overload;

// : Scales given vector by a factor
procedure ScaleVector(var v: TVector2f; factor: single); overload;
// : Scales given vector by a factor
procedure ScaleVector(var v: TAffineVector; factor: single); overload;
{ : Scales given vector by another vector.<p>
  v[x]:=v[x]*factor[x], v[y]:=v[y]*factor[y] etc. }
procedure ScaleVector(var v: TAffineVector; const factor: TAffineVector); overload;
// : Scales given vector by a factor
procedure ScaleVector(var v: TVector; factor: single); overload;
{ : Scales given vector by another vector.<p>
  v[x]:=v[x]*factor[x], v[y]:=v[y]*factor[y] etc. }
procedure ScaleVector(var v: TVector; const factor: TVector); overload;

// : Returns a vector scaled by a factor
function VectorScale(const v: TVector2f; factor: single): TVector2f; overload;
// : Returns a vector scaled by a factor
function VectorScale(const v: TAffineVector; factor: single): TAffineVector; overload;
// : Scales a vector by a factor and places result in vr
procedure VectorScale(const v: TAffineVector; factor: single; var vr: TAffineVector); overload;
// : Returns a vector scaled by a factor
function VectorScale(const v: TVector; factor: single): TVector; overload;
// : Scales a vector by a factor and places result in vr
procedure VectorScale(const v: TVector; factor: single; var vr: TVector); overload;
// : Scales a vector by a factor and places result in vr
procedure VectorScale(const v: TVector; factor: single; var vr: TAffineVector); overload;
// : Scales given vector by another vector
function VectorScale(const v: TAffineVector; const factor: TAffineVector): TAffineVector; overload;
// : RScales given vector by another vector
function VectorScale(const v: TVector; const factor: TVector): TVector; overload;

{ : Divides given vector by another vector.<p>
  v[x]:=v[x]/divider[x], v[y]:=v[y]/divider[y] etc. }
procedure DivideVector(var v: TVector; const divider: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure DivideVector(var v: TAffineVector; const divider: TAffineVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function VectorDivide(const v: TVector; const divider: TVector): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function VectorDivide(const v: TAffineVector; const divider: TAffineVector): TAffineVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// : True if all components are equal.
function TexpointEquals(const p1, p2: TTexPoint): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : True if all components are equal.
// : True if all components are equal.
function VectorEquals(const V1, V2: TVector): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : True if all components are equal.
function VectorEquals(const V1, V2: TAffineVector): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : True if X, Y and Z components are equal.
function AffineVectorEquals(const V1, V2: TVector): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : True if x=y=z=0, w ignored
function VectorIsNull(const v: TVector): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : True if x=y=z=0, w ignored
function VectorIsNull(const v: TAffineVector): Boolean; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

{ : Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y]), also know as "Norm1".<p> }
function VectorSpacing(const V1, V2: TTexPoint): single; overload;
{ : Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y])+..., also know as "Norm1".<p> }
function VectorSpacing(const V1, V2: TAffineVector): single; overload;
{ : Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y])+..., also know as "Norm1".<p> }
function VectorSpacing(const V1, V2: TVector): single; overload;

{ : Calculates distance between two vectors.<p>
  ie. sqrt(sqr(v1[x]-v2[x])+...) }
function VectorDistance(const V1, V2: TAffineVector): single; overload;
{ : Calculates distance between two vectors.<p>
  ie. sqrt(sqr(v1[x]-v2[x])+...) (w component ignored) }
function VectorDistance(const V1, V2: TVector): single; overload;

{ : Calculates the "Norm 2" between two vectors.<p>
  ie. sqr(v1[x]-v2[x])+... }
function VectorDistance2(const V1, V2: TAffineVector): single; overload;
{ : Calculates the "Norm 2" between two vectors.<p>
  ie. sqr(v1[x]-v2[x])+... (w component ignored) }
function VectorDistance2(const V1, V2: TVector): single; overload;

{ : Calculates a vector perpendicular to N.<p>
  N is assumed to be of unit length, subtract out any component parallel to N }
function VectorPerpendicular(const v, n: TAffineVector): TAffineVector;
// : Reflects vector V against N (assumes N is normalized)
function VectorReflect(const v, n: TAffineVector): TAffineVector;
// : Rotates Vector about Axis with Angle radians
procedure RotateVector(var Vector: TVector; const axis: TAffineVector; angle: single); overload;
// : Rotates Vector about Axis with Angle radians
procedure RotateVector(var Vector: TVector; const axis: TVector; angle: single); overload;

// : Rotate given vector around the Y axis (alpha is in rad)
procedure RotateVectorAroundY(var v: TAffineVector; alpha: single);
// : Returns given vector rotated around the X axis (alpha is in rad)
function VectorRotateAroundX(const v: TAffineVector; alpha: single): TAffineVector; overload;
// : Returns given vector rotated around the Y axis (alpha is in rad)
function VectorRotateAroundY(const v: TAffineVector; alpha: single): TAffineVector; overload;
// : Returns given vector rotated around the Y axis in vr (alpha is in rad)
procedure VectorRotateAroundY(const v: TAffineVector; alpha: single; var vr: TAffineVector); overload;
// : Returns given vector rotated around the Z axis (alpha is in rad)
function VectorRotateAroundZ(const v: TAffineVector; alpha: single): TAffineVector; overload;

// : Vector components are replaced by their Abs() value. }
procedure AbsVector(var v: TVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Vector components are replaced by their Abs() value. }
procedure AbsVector(var v: TAffineVector); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Returns a vector with components replaced by their Abs value. }
function VectorAbs(const v: TVector): TVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
// : Returns a vector with components replaced by their Abs value. }
function VectorAbs(const v: TAffineVector): TAffineVector; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

// : Returns true if both vector are colinear
function IsColinear(const V1, V2: TVector2f): Boolean; overload;
// : Returns true if both vector are colinear
function IsColinear(const V1, V2: TAffineVector): Boolean; overload;
// : Returns true if both vector are colinear
function IsColinear(const V1, V2: TVector): Boolean; overload;
// ------------------------------------------------------------------------------
// Matrix functions
// ------------------------------------------------------------------------------

procedure SetMatrix(var dest: THomogeneousDblMatrix; const src: TMatrix); overload;
procedure SetMatrix(var dest: TAffineMatrix; const src: TMatrix); overload;
procedure SetMatrix(var dest: TMatrix; const src: TAffineMatrix); overload;

procedure SetMatrixRow(var dest: TMatrix; rowNb: Integer; const aRow: TVector); overload;

// : Creates scale matrix
function CreateScaleMatrix(const v: TAffineVector): TMatrix; overload;
// : Creates scale matrix
function CreateScaleMatrix(const v: TVector): TMatrix; overload;
// : Creates translation matrix
function CreateTranslationMatrix(const v: TAffineVector): TMatrix; overload;
// : Creates translation matrix
function CreateTranslationMatrix(const v: TVector): TMatrix; overload;
{ : Creates a scale+translation matrix.<p>
  Scale is applied BEFORE applying offset }
function CreateScaleAndTranslationMatrix(const scale, offset: TVector): TMatrix; overload;
// : Creates matrix for rotation about x-axis (angle in rad)
function CreateRotationMatrixX(const sine, cosine: single): TMatrix; overload;
function CreateRotationMatrixX(const angle: single): TMatrix; overload;
// : Creates matrix for rotation about y-axis (angle in rad)
function CreateRotationMatrixY(const sine, cosine: single): TMatrix; overload;
function CreateRotationMatrixY(const angle: single): TMatrix; overload;
// : Creates matrix for rotation about z-axis (angle in rad)
function CreateRotationMatrixZ(const sine, cosine: single): TMatrix; overload;
function CreateRotationMatrixZ(const angle: single): TMatrix; overload;
// : Creates a rotation matrix along the given Axis by the given Angle in radians.
function CreateRotationMatrix(const anAxis: TAffineVector; angle: single): TMatrix; overload;
function CreateRotationMatrix(const anAxis: TVector; angle: single): TMatrix; overload;
// : Creates a rotation matrix along the given Axis by the given Angle in radians.
function CreateAffineRotationMatrix(const anAxis: TAffineVector; angle: single): TAffineMatrix;

// : Multiplies two 3x3 matrices
function MatrixMultiply(const m1, m2: TAffineMatrix): TAffineMatrix; overload;
// : Multiplies two 4x4 matrices
function MatrixMultiply(const m1, m2: TMatrix): TMatrix; overload;
// : Multiplies M1 by M2 and places result in MResult
procedure MatrixMultiply(const m1, m2: TMatrix; var MResult: TMatrix); overload;

// : Transforms a homogeneous vector by multiplying it with a matrix
function VectorTransform(const v: TVector; const M: TMatrix): TVector; overload;
// : Transforms a homogeneous vector by multiplying it with a matrix
function VectorTransform(const v: TVector; const M: TAffineMatrix): TVector; overload;
// : Transforms an affine vector by multiplying it with a matrix
function VectorTransform(const v: TAffineVector; const M: TMatrix): TAffineVector; overload;
// : Transforms an affine vector by multiplying it with a matrix
function VectorTransform(const v: TAffineVector; const M: TAffineMatrix): TAffineVector; overload;

// : Determinant of a 3x3 matrix
function MatrixDeterminant(const M: TAffineMatrix): single; overload;
// : Determinant of a 4x4 matrix
function MatrixDeterminant(const M: TMatrix): single; overload;

{ : Adjoint of a 4x4 matrix.<p>
  used in the computation of the inverse of a 4x4 matrix }
procedure AdjointMatrix(var M: TMatrix); overload;
{ : Adjoint of a 3x3 matrix.<p>
  used in the computation of the inverse of a 3x3 matrix }
procedure AdjointMatrix(var M: TAffineMatrix); overload;

// : Multiplies all elements of a 3x3 matrix with a factor
procedure ScaleMatrix(var M: TAffineMatrix; const factor: single); overload;
// : Multiplies all elements of a 4x4 matrix with a factor
procedure ScaleMatrix(var M: TMatrix; const factor: single); overload;

// : Adds the translation vector into the matrix
procedure TranslateMatrix(var M: TMatrix; const v: TAffineVector); overload;
procedure TranslateMatrix(var M: TMatrix; const v: TVector); overload;

{ : Normalize the matrix and remove the translation component.<p>
  The resulting matrix is an orthonormal matrix (Y direction preserved, then Z) }
procedure NormalizeMatrix(var M: TMatrix);

// : Computes transpose of 3x3 matrix
procedure TransposeMatrix(var M: TAffineMatrix); overload;
// : Computes transpose of 4x4 matrix
procedure TransposeMatrix(var M: TMatrix); overload;

// : Finds the inverse of a 4x4 matrix
procedure InvertMatrix(var M: TMatrix); overload;
function MatrixInvert(const M: TMatrix): TMatrix; overload;

// : Finds the inverse of a 3x3 matrix;
procedure InvertMatrix(var M: TAffineMatrix); overload;
function MatrixInvert(const M: TAffineMatrix): TAffineMatrix; overload;

{ : Finds the inverse of an angle preserving matrix.<p>
  Angle preserving matrices can combine translation, rotation and isotropic
  scaling, other matrices won't be properly inverted by this function. }
function AnglePreservingMatrixInvert(const mat: TMatrix): TMatrix;

{ : Decompose a non-degenerated 4x4 transformation matrix into the sequence of transformations that produced it.<p>
  Modified by ml then eg, original Author: Spencer W. Thomas, University of Michigan<p>
  The coefficient of each transformation is returned in the corresponding
  element of the vector Tran.<p>
  Returns true upon success, false if the matrix is singular. }
function MatrixDecompose(const M: TMatrix; var Tran: TTransformations): Boolean;

function CreateLookAtMatrix(const eye, center, normUp: TVector): TMatrix;
function CreateMatrixFromFrustum(Left, Right, Bottom, Top, ZNear, ZFar: single): TMatrix;
function CreatePerspectiveMatrix(FOV, Aspect, ZNear, ZFar: single): TMatrix;
function CreateOrthoMatrix(Left, Right, Bottom, Top, ZNear, ZFar: single): TMatrix;
function CreatePickMatrix(x, y, deltax, deltay: single; const viewport: TVector4i): TMatrix;
function Project(objectVector: TVector; const ViewProjMatrix: TMatrix; const viewport: TVector4i; out WindowVector: TVector): Boolean;
function UnProject(WindowVector: TVector; ViewProjMatrix: TMatrix; const viewport: TVector4i; out objectVector: TVector): Boolean;
// ------------------------------------------------------------------------------
// Plane functions
// ------------------------------------------------------------------------------

// : Computes the parameters of a plane defined by three points.
function PlaneMake(const p1, p2, p3: TAffineVector): THmgPlane; overload;
function PlaneMake(const p1, p2, p3: TVector): THmgPlane; overload;
// : Computes the parameters of a plane defined by a point and a normal.
function PlaneMake(const point, normal: TAffineVector): THmgPlane; overload;
function PlaneMake(const point, normal: TVector): THmgPlane; overload;
// : Converts from single to double representation
procedure SetPlane(var dest: TDoubleHmgPlane; const src: THmgPlane);

// : Normalize a plane so that point evaluation = plane distance. }
procedure NormalizePlane(var plane: THmgPlane);

{ : Calculates the cross-product between the plane normal and plane to point vector.<p>
  This functions gives an hint as to were the point is, if the point is in the
  half-space pointed by the vector, result is positive.<p>
  This function performs an homogeneous space dot-product. }
function PlaneEvaluatePoint(const plane: THmgPlane; const point: TAffineVector): single; overload;
function PlaneEvaluatePoint(const plane: THmgPlane; const point: TVector): single; overload;

{ : Calculate the normal of a plane defined by three points. }
function CalcPlaneNormal(const p1, p2, p3: TAffineVector): TAffineVector; overload;
procedure CalcPlaneNormal(const p1, p2, p3: TAffineVector; var vr: TAffineVector); overload;
procedure CalcPlaneNormal(const p1, p2, p3: TVector; var vr: TAffineVector); overload;

{ : Returns true if point is in the half-space defined by a plane with normal.<p>
  The plane itself is not considered to be in the tested halfspace. }
function PointIsInHalfSpace(const point, planePoint, planeNormal: TVector): Boolean; overload;
function PointIsInHalfSpace(const point, planePoint, planeNormal: TAffineVector): Boolean; overload;
function PointIsInHalfSpace(const point: TAffineVector; plane: THmgPlane): Boolean; overload;

{ : Computes algebraic distance between point and plane.<p>
  Value will be positive if the point is in the halfspace pointed by the normal,
  negative on the other side. }
function PointPlaneDistance(const point, planePoint, planeNormal: TVector): single; overload;
function PointPlaneDistance(const point, planePoint, planeNormal: TAffineVector): single; overload;
function PointPlaneDistance(const point: TAffineVector; plane: THmgPlane): single; overload;

{ : Computes point to plane projection. Plane and direction have to be normalized }
function PointPlaneOrthoProjection(const point: TAffineVector; const plane: THmgPlane; var inter: TAffineVector; bothface: Boolean = True): Boolean;
function PointPlaneProjection(const point, direction: TAffineVector; const plane: THmgPlane; var inter: TAffineVector; bothface: Boolean = True): Boolean;

{ : Computes segment / plane intersection return false if there isn't an intersection }
function SegmentPlaneIntersection(const ptA, ptB: TAffineVector; const plane: THmgPlane; var inter: TAffineVector): Boolean;

{ : Computes point to triangle projection. Direction has to be normalized }
function PointTriangleOrthoProjection(const point, ptA, ptB, ptC: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;
function PointTriangleProjection(const point, direction, ptA, ptB, ptC: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;

{ : Returns true if line intersect ABC triangle. }
function IsLineIntersectTriangle(const point, direction, ptA, ptB, ptC: TAffineVector): Boolean;

{ : Computes point to Quad projection. Direction has to be normalized. Quad have to be flat and convex }
function PointQuadOrthoProjection(const point, ptA, ptB, ptC, ptD: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;
function PointQuadProjection(const point, direction, ptA, ptB, ptC, ptD: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;

{ : Returns true if line intersect ABCD quad. Quad have to be flat and convex }
function IsLineIntersectQuad(const point, direction, ptA, ptB, ptC, ptD: TAffineVector): Boolean;

{ : Computes point to disk projection. Direction has to be normalized }
function PointDiskOrthoProjection(const point, center, up: TAffineVector; const radius: single; var inter: TAffineVector; bothface: Boolean = True): Boolean;
function PointDiskProjection(const point, direction, center, up: TAffineVector; const radius: single; var inter: TAffineVector; bothface: Boolean = True): Boolean;

{ : Computes closest point on a segment (a segment is a limited line). }
function PointSegmentClosestPoint(const point, segmentStart, segmentStop: TAffineVector): TAffineVector; overload;
function PointSegmentClosestPoint(const point, segmentStart, segmentStop: TVector): TVector; overload;

{ : Computes algebraic distance between segment and line (a segment is a limited line). }
function PointSegmentDistance(const point, segmentStart, segmentStop: TAffineVector): single;

{ : Computes closest point on a line. }
function PointLineClosestPoint(const point, linePoint, lineDirection: TAffineVector): TAffineVector;

{ : Computes algebraic distance between point and line. }
function PointLineDistance(const point, linePoint, lineDirection: TAffineVector): single;

{ : Computes the closest points (2) given two segments. }
procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start, S1Stop: TAffineVector; var Segment0Closest, Segment1Closest: TAffineVector);

{ : Computes the closest distance between two segments. }
function SegmentSegmentDistance(const S0Start, S0Stop, S1Start, S1Stop: TAffineVector): single;

{ : Computes the closest distance between two lines. }
function LineLineDistance(const linePt0, lineDir0, linePt1, lineDir1: TAffineVector): single;

// ------------------------------------------------------------------------------
// Quaternion functions
// ------------------------------------------------------------------------------

type
  TEulerOrder = (eulXYZ, eulXZY, eulYXZ, eulYZX, eulZXY, eulZYX);

  // : Creates a quaternion from the given values
function QuaternionMake(const Imag: array of single; Real: single): TQuaternion;
// : Returns the conjugate of a quaternion
function QuaternionConjugate(const Q: TQuaternion): TQuaternion;
// : Returns the magnitude of the quaternion
function QuaternionMagnitude(const Q: TQuaternion): single;
// : Normalizes the given quaternion
procedure NormalizeQuaternion(var Q: TQuaternion);

// : Constructs a unit quaternion from two points on unit sphere
function QuaternionFromPoints(const V1, V2: TAffineVector): TQuaternion;
// : Converts a unit quaternion into two points on a unit sphere
procedure QuaternionToPoints(const Q: TQuaternion; var ArcFrom, ArcTo: TAffineVector);
// : Constructs a unit quaternion from a rotation matrix
function QuaternionFromMatrix(const mat: TMatrix): TQuaternion;
{ : Constructs a rotation matrix from (possibly non-unit) quaternion.<p>
  Assumes matrix is used to multiply column vector on the left:<br>
  vnew = mat vold.<p>
  Works correctly for right-handed coordinate system and right-handed rotations. }
function QuaternionToMatrix(quat: TQuaternion): TMatrix;
{ : Constructs an affine rotation matrix from (possibly non-unit) quaternion.<p> }
function QuaternionToAffineMatrix(quat: TQuaternion): TAffineMatrix;
// : Constructs quaternion from angle (in deg) and axis
function QuaternionFromAngleAxis(const angle: single; const axis: TAffineVector): TQuaternion;
// : Constructs quaternion from Euler angles
function QuaternionFromRollPitchYaw(const r, p, y: single): TQuaternion;
// : Constructs quaternion from Euler angles in arbitrary order (angles in degrees)
function QuaternionFromEuler(const x, y, z: single; eulerOrder: TEulerOrder): TQuaternion;

{ : Returns quaternion product qL * qR.<p>
  Note: order is important!<p>
  To combine rotations, use the product QuaternionMuliply(qSecond, qFirst),
  which gives the effect of rotating by qFirst then qSecond. }
function QuaternionMultiply(const qL, qR: TQuaternion): TQuaternion;

{ : Spherical linear interpolation of unit quaternions with spins.<p>
  QStart, QEnd - start and end unit quaternions<br>
  t            - interpolation parameter (0 to 1)<br>
  Spin         - number of extra spin rotations to involve<br> }
function QuaternionSlerp(const QStart, QEnd: TQuaternion; Spin: Integer; T: single): TQuaternion; overload;
function QuaternionSlerp(const source, dest: TQuaternion; const T: single): TQuaternion; overload;

// ------------------------------------------------------------------------------
// Logarithmic and exponential functions
// ------------------------------------------------------------------------------

{ : Return ln(1 + X),  accurate for X near 0. }
function LnXP1(x: Extended): Extended;
{ : Log base 10 of X }
function Log10(x: Extended): Extended;
{ : Log base 2 of X }
function Log2(x: Extended): Extended; overload;
{ : Log base 2 of X }
function Log2(x: single): single; overload;
{ : Log base N of X }
function LogN(Base, x: Extended): Extended;
{ : Raise base to an integer. }
function IntPower(Base: Extended; Exponent: Integer): Extended;
{ : Raise base to any power.<p>
  For fractional exponents, or |exponents| > MaxInt, base must be > 0. }
function Power(const Base, Exponent: single): single; overload;
{ : Raise base to an integer. }
function Power(Base: single; Exponent: Integer): single; overload;
function Power(Base: single; Exponent: Int64): single; overload;

// ------------------------------------------------------------------------------
// Trigonometric functions
// ------------------------------------------------------------------------------

function DegToRad(const Degrees: Extended): Extended; overload;
function DegToRad(const Degrees: single): single; overload;
function RadToDeg(const Radians: Extended): Extended; overload;
function RadToDeg(const Radians: single): single; overload;

// : Normalize to an angle in the [-PI; +PI] range
function NormalizeAngle(angle: single): single;
// : Normalize to an angle in the [-180; 180] range
function NormalizeDegAngle(angle: single): single;

// : Calculates sine and cosine from the given angle Theta
procedure SinCos(const Theta: Extended; out Sin, Cos: Extended); overload;
// : Calculates sine and cosine from the given angle Theta
procedure SinCos(const Theta: single; out Sin, Cos: single); overload;
{ : Calculates sine and cosine from the given angle Theta and Radius.<p>
  sin and cos values calculated from theta are multiplicated by radius. }
procedure SinCos(const Theta, radius: double; out Sin, Cos: Extended); overload;
{ : Calculates sine and cosine from the given angle Theta and Radius.<p>
  sin and cos values calculated from theta are multiplicated by radius. }
procedure SinCos(const Theta, radius: single; out Sin, Cos: single); overload;

{ : Fills up the two given dynamic arrays with sin cos values.<p>
  start and stop angles must be given in degrees, the number of steps is
  determined by the length of the given arrays. }
procedure PrepareSinCosCache(var S, c: array of single;
  startAngle, stopAngle: single);

function ArcCos(const x: Extended): Extended; overload;
function ArcCos(const x: single): single; overload;
function ArcSin(const x: Extended): Extended; overload;
function ArcSin(const x: single): single; overload;
function ArcTan2(const y, x: Extended): Extended; overload;
function ArcTan2(const y, x: single): single; overload;
{ : Fast ArcTan2 approximation, about 0.07 rads accuracy. }
function FastArcTan2(y, x: single): single;
function Tan(const x: Extended): Extended; overload;
function Tan(const x: single): single; overload;
function CoTan(const x: Extended): Extended; overload;
function CoTan(const x: single): single; overload;

// ------------------------------------------------------------------------------
// Hyperbolic Trigonometric functions
// ------------------------------------------------------------------------------

function Sinh(const x: single): single; overload;
function Sinh(const x: double): double; overload;
function Cosh(const x: single): single; overload;
function Cosh(const x: double): double; overload;

// ------------------------------------------------------------------------------
// Miscellanious math functions
// ------------------------------------------------------------------------------

{ : Computes 1/Sqrt(v).<p> }
function RSqrt(v: single): single;
{ : Computes 1/Sqrt(Sqr(x)+Sqr(y)). }
function RLength(x, y: single): single;
{ : Computes an integer sqrt approximation.<p> }
function ISqrt(i: Integer): Integer;
{ : Computes an integer length Result:=Sqrt(x*x+y*y). }
function ILength(x, y: Integer): Integer; overload;
function ILength(x, y, z: Integer): Integer; overload;

{ : Generates a random point on the unit sphere.<p>
  Point repartition is correctly isotropic with no privilegied direction. }
procedure RandomPointOnSphere(var p: TAffineVector);

{ : Rounds the floating point value to the closest integer.<p>
  Behaves like Round but returns a floating point value like Int. }
function RoundInt(v: single): single; overload;
function RoundInt(v: Extended): Extended; overload;

function Trunc(x: Extended): Int64;
function Round(x: Extended): Int64;
function Frac(x: Extended): Extended;

function Ceil(v: single): Integer; overload;
function Ceil64(v: Extended): Int64; overload;
function Floor(v: single): Integer; overload;
function Floor64(v: Extended): Int64; overload;

{ : Multiples i by s and returns the rounded result.<p> }
function ScaleAndRound(i: Integer; var S: single): Integer;

{ : Returns the sign of the x value using the (-1, 0, +1) convention }
function Sign(x: single): Integer;
function SignStrict(x: single): Integer;

{ : Returns True if x is in [a; b] }
function IsInRange(const x, a, b: single): Boolean; overload;
function IsInRange(const x, a, b: double): Boolean; overload;

{ : Returns True if p is in the cube defined by d. }
function IsInCube(const p, d: TAffineVector): Boolean; overload;
function IsInCube(const p, d: TVector): Boolean; overload;

{ : Returns the minimum value of the array. }
function MinFloat(values: PSingleArray; nbItems: Integer): single; overload;
function MinFloat(values: PDoubleArray; nbItems: Integer): double; overload;
{ : Returns the minimum of given values. }
function MinFloat(const V1, V2: single): single; overload;
function MinFloat(const v: array of single): single; overload;
function MinFloat(const V1, V2: double): double; overload;
function MinFloat(const V1, V2, V3: single): single; overload;
function MinFloat(const V1, V2, V3: double): double; overload;
{ : Returns the maximum value of the array. }
function MaxFloat(values: PSingleArray; nbItems: Integer): single; overload;
function MaxFloat(values: PDoubleArray; nbItems: Integer): double; overload;
function MaxFloat(const v: array of single): single; overload;
{ : Returns the maximum of given values. }
function MaxFloat(const V1, V2: single): single; overload;
function MaxFloat(const V1, V2: double): double; overload;
function MaxFloat(const V1, V2, V3: single): single; overload;
function MaxFloat(const V1, V2, V3: double): double; overload;

function MinInteger(const V1, V2: Integer): Integer; overload;
function MinInteger(const V1, V2: Cardinal): Cardinal; overload;
function MinInteger(const V1, V2, V3: Integer): Integer; overload;
function MinInteger(const V1, V2, V3: Cardinal): Cardinal; overload;

function MaxInteger(const V1, V2: Integer): Integer; overload;
function MaxInteger(const V1, V2: Cardinal): Cardinal; overload;
function MaxInteger(const V1, V2, V3: Integer): Integer; overload;
function MaxInteger(const V1, V2, V3: Cardinal): Cardinal; overload;

function ClampInteger(const value, min, max: Integer): Integer; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function ClampInteger(const value, min, max: Cardinal): Cardinal; overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

{ : Computes the triangle's area. }
function TriangleArea(const p1, p2, p3: TAffineVector): single; overload;
{ : Computes the polygons's area.<p>
  Points must be coplanar. Polygon needs not be convex. }
function PolygonArea(const p: PAffineVectorArray; nSides: Integer): single; overload;
{ : Computes a 2D triangle's signed area.<p>
  Only X and Y coordinates are used, Z is ignored. }
function TriangleSignedArea(const p1, p2, p3: TAffineVector): single; overload;
{ : Computes a 2D polygon's signed area.<p>
  Only X and Y coordinates are used, Z is ignored. Polygon needs not be convex. }
function PolygonSignedArea(const p: PAffineVectorArray; nSides: Integer): single; overload;

{ : Multiplies values in the array by factor.<p>
  This function is especially efficient for large arrays, it is not recommended
  for arrays that have less than 10 items.<br>
  Expected performance is 4 to 5 times that of a Deliph-compiled loop on AMD
  CPUs, and 2 to 3 when 3DNow! isn't available. }
procedure ScaleFloatArray(values: PSingleArray; nb: Integer;
  var factor: single); overload;
procedure ScaleFloatArray(var values: TSingleArray;
  factor: single); overload;

{ : Adds delta to values in the array.<p>
  Array size must be a multiple of four. }
procedure OffsetFloatArray(values: PSingleArray; nb: Integer;
  var delta: single); overload;
procedure OffsetFloatArray(var values: array of single;
  delta: single); overload;
procedure OffsetFloatArray(valuesDest, valuesDelta: PSingleArray; nb: Integer); overload;

{ : Returns the max of the X, Y and Z components of a vector (W is ignored). }
function MaxXYZComponent(const v: TVector): single; overload;
function MaxXYZComponent(const v: TAffineVector): single; overload;
{ : Returns the min of the X, Y and Z components of a vector (W is ignored). }
function MinXYZComponent(const v: TVector): single; overload;
function MinXYZComponent(const v: TAffineVector): single; overload;
{ : Returns the max of the Abs(X), Abs(Y) and Abs(Z) components of a vector (W is ignored). }
function MaxAbsXYZComponent(v: TVector): single;
{ : Returns the min of the Abs(X), Abs(Y) and Abs(Z) components of a vector (W is ignored). }
function MinAbsXYZComponent(v: TVector): single;
{ : Replace components of v with the max of v or v1 component.<p>
  Maximum is computed per component. }
procedure MaxVector(var v: TVector; const V1: TVector); overload;
procedure MaxVector(var v: TAffineVector; const V1: TAffineVector); overload;
{ : Replace components of v with the min of v or v1 component.<p>
  Minimum is computed per component. }
procedure MinVector(var v: TVector; const V1: TVector); overload;
procedure MinVector(var v: TAffineVector; const V1: TAffineVector); overload;

{ : Sorts given array in ascending order.<p>
  NOTE : current implementation is a slow bubble sort... }
procedure SortArrayAscending(var a: array of Extended);

{ : Clamps aValue in the aMin-aMax interval.<p> }
function ClampValue(const aValue, aMin, aMax: single): single; overload;
{ : Clamps aValue in the aMin-INF interval.<p> }
function ClampValue(const aValue, aMin: single): single; overload;


// --------------------- Unstandardized functions after these lines
// --------------------- Unstandardized functions after these lines
// --------------------- Unstandardized functions after these lines
// --------------------- Unstandardized functions after these lines
// --------------------- Unstandardized functions after these lines

// mixed functions

{ : Turn a triplet of rotations about x, y, and z (in that order) into an equivalent rotation around a single axis (all in radians).<p> }
function ConvertRotation(const Angles: TAffineVector): TVector;

// miscellaneous functions

function MakeAffineDblVector(var v: array of double): TAffineDblVector;
function MakeDblVector(var v: array of double): THomogeneousDblVector;
function VectorAffineDblToFlt(const v: TAffineDblVector): TAffineVector;
function VectorDblToFlt(const v: THomogeneousDblVector): THomogeneousVector;
function VectorAffineFltToDbl(const v: TAffineVector): TAffineDblVector;
function VectorFltToDbl(const v: TVector): THomogeneousDblVector;

function PointInPolygon(var xp, yp: array of single; x, y: single): Boolean;

procedure DivMod(Dividend: Integer; Divisor: Word; var result, Remainder: Word);

// coordinate system manipulation functions

// : Rotates the given coordinate system (represented by the matrix) around its Y-axis
function Turn(const Matrix: TMatrix; angle: single): TMatrix; overload;
// : Rotates the given coordinate system (represented by the matrix) around MasterUp
function Turn(const Matrix: TMatrix; const MasterUp: TAffineVector; angle: single): TMatrix; overload;
// : Rotates the given coordinate system (represented by the matrix) around its X-axis
function Pitch(const Matrix: TMatrix; angle: single): TMatrix; overload;
// : Rotates the given coordinate system (represented by the matrix) around MasterRight
function Pitch(const Matrix: TMatrix; const MasterRight: TAffineVector; angle: single): TMatrix; overload;
// : Rotates the given coordinate system (represented by the matrix) around its Z-axis
function Roll(const Matrix: TMatrix; angle: single): TMatrix; overload;
// : Rotates the given coordinate system (represented by the matrix) around MasterDirection
function Roll(const Matrix: TMatrix; const MasterDirection: TAffineVector; angle: single): TMatrix; overload;

// intersection functions

{ : Compute the intersection point "res" of a line with a plane.<p>
  Return value:<ul>
  <li>0 : no intersection, line parallel to plane
  <li>1 : res is valid
  <li>-1 : line is inside plane
  </ul><br>
  Adapted from:<br>
  E.Hartmann, Computeruntersttzte Darstellende Geometrie, B.G. Teubner Stuttgart 1988 }
function IntersectLinePlane(const point, direction: TVector;
  const plane: THmgPlane;
  intersectPoint: PVector = nil): Integer; overload;

{ : Compute intersection between a triangle and a box.<p>
  Returns True if an intersection was found. }
function IntersectTriangleBox(
  const p1, p2, p3, aMinExtent, aMaxExtent: TAffineVector): Boolean;

{ : Compute intersection between a Sphere and a box.<p>
  Up, Direction and Right must be normalized!
  Use CubDepht, CubeHeight and CubeWidth to scale TGLCube. }
function IntersectSphereBox(
  const SpherePos: TVector;
  const SphereRadius: single;
  const BoxMatrix: TMatrix;
  const BoxScale: TAffineVector
  ; intersectPoint: PAffineVector = nil
  ; normal: PAffineVector = nil
  ; depth: PSingle = nil
  ): Boolean;

{ : Compute intersection between a ray and a plane.<p>
  Returns True if an intersection was found, the intersection point is placed
  in intersectPoint is the reference is not nil. }
function RayCastPlaneIntersect(const rayStart, rayVector: TVector;
  const planePoint, planeNormal: TVector;
  intersectPoint: PVector = nil): Boolean; overload;
function RayCastPlaneXZIntersect(const rayStart, rayVector: TVector;
  const planeY: single;
  intersectPoint: PVector = nil): Boolean; overload;

{ : Compute intersection between a ray and a triangle. }
function RayCastTriangleIntersect(const rayStart, rayVector: TVector;
  const p1, p2, p3: TAffineVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean; overload;
{ : Compute the min distance a ray will pass to a point.<p> }
function RayCastMinDistToPoint(const rayStart, rayVector: TVector;
  const point: TVector): single;
{ : Determines if a ray will intersect with a given sphere.<p> }
function RayCastIntersectsSphere(const rayStart, rayVector: TVector;
  const sphereCenter: TVector;
  const SphereRadius: single): Boolean; overload;
{ : Calculates the intersections between a sphere and a ray.<p>
  Returns 0 if no intersection is found (i1 and i2 untouched), 1 if one
  intersection was found (i1 defined, i2 untouched), and 2 is two intersections
  were found (i1 and i2 defined). }
function RayCastSphereIntersect(const rayStart, rayVector: TVector;
  const sphereCenter: TVector;
  const SphereRadius: single;
  var i1, i2: TVector): Integer; overload;
{ : Compute intersection between a ray and a box.<p>
  Returns True if an intersection was found, the intersection point is
  placed in intersectPoint if the reference is not nil. }
function RayCastBoxIntersect(
  const rayStart, rayVector, aMinExtent, aMaxExtent: TAffineVector;
  intersectPoint: PAffineVector = nil): Boolean;

// Some 2d intersection functions.

{ : Determine if 2 rectanges intersect. }
function RectanglesIntersect(const ACenterOfRect1, ACenterOfRect2,
  ASizeOfRect1, ASizeOfRect2: TVector2f): Boolean;

{ : Determine if BigRect completely contains SmallRect. }
function RectangleContains(const ACenterOfBigRect1, ACenterOfSmallRect2,
  ASizeOfBigRect1, ASizeOfSmallRect2: TVector2f; const AEps: single = 0.0): Boolean;

{ : Computes the visible radius of a sphere in a perspective projection.<p>
  This radius can be used for occlusion culling (cone extrusion) or 2D
  intersection testing. }
function SphereVisibleRadius(distance, radius: single): single;

{ : Extracts a TFrustum for combined modelview and projection matrices. }
function ExtractFrustumFromModelViewProjection(const modelViewProj: TMatrix): TFrustum;

// : Determines if volume is clipped or not
function IsVolumeClipped(const objPos: TAffineVector; const objRadius: single;
  const Frustum: TFrustum): Boolean; overload;
function IsVolumeClipped(const objPos: TVector; const objRadius: single;
  const Frustum: TFrustum): Boolean; overload;
function IsVolumeClipped(const min, max: TAffineVector;
  const Frustum: TFrustum): Boolean; overload;

// misc funcs

{ : Creates a parallel projection matrix.<p>
  Transformed points will projected on the plane along the specified direction. }
function MakeParallelProjectionMatrix(const plane: THmgPlane;
  const dir: TVector): TMatrix;

{ : Creates a shadow projection matrix.<p>
  Shadows will be projected onto the plane defined by planePoint and planeNormal,
  from lightPos. }
function MakeShadowMatrix(const planePoint, planeNormal, lightPos: TVector): TMatrix;

{ : Builds a reflection matrix for the given plane.<p>
  Reflection matrix allow implementing planar reflectors in OpenGL (mirrors). }
function MakeReflectionMatrix(const planePoint, planeNormal: TAffineVector): TMatrix;

{ : Packs an homogeneous rotation matrix to 6 bytes.<p>
  The 6:64 (or 6:36) compression ratio is achieved by computing the quaternion
  associated to the matrix and storing its Imaginary components at 16 bits
  precision each.<br>
  Deviation is typically below 0.01% and around 0.1% in worst case situations.<p>
  Note: quaternion conversion is faster and more robust than an angle decomposition. }
function PackRotationMatrix(const mat: TMatrix): TPackedRotationMatrix;
{ : Restores a packed rotation matrix.<p>
  See PackRotationMatrix. }
function UnPackRotationMatrix(const packedMatrix: TPackedRotationMatrix): TMatrix;

{ : Calculates the barycentric coordinates for the point p on the triangle
  defined by the vertices v1, v2 and v3. That is, solves
  p = u * v1 + v * v2 + (1-u-v) * v3
  for u,v.
  Returns true if the point is inside the triangle, false otherwise.<p>
  NOTE: This function assumes that the point lies on the plane defined by the triangle.
  If this is not the case, the function will not work correctly! }
function BarycentricCoordinates(const V1, V2, V3, p: TAffineVector; var u, v: single): Boolean;

{ : Extracted from Camera.MoveAroundTarget(pitch, turn). }
function MoveObjectAround(const AMovingObjectPosition, AMovingObjectUp, ATargetPosition: TVector;
  pitchDelta, turnDelta: single): TVector;

{ : Calcualtes Angle between 2 Vectors: (A-CenterPoint) and (B-CenterPoint). In radians. }
function AngleBetweenVectors(const a, b, ACenterPoint: TVector): single; overload;
function AngleBetweenVectors(const a, b, ACenterPoint: TAffineVector): single; overload;

{ : AOriginalPosition - Object initial position.
  ACenter - some point, from which is should be distanced.

  ADistance + AFromCenterSpot - distance, which object should keep from ACenter
  or
  ADistance + not AFromCenterSpot - distance, which object should shift from his current position away from center.
}
function ShiftObjectFromCenter(const AOriginalPosition: TVector;
  const ACenter: TVector; const ADistance: single; const AFromCenterSpot: Boolean): TVector; overload;
function ShiftObjectFromCenter(const AOriginalPosition: TAffineVector;
  const ACenter: TAffineVector; const ADistance: single; const AFromCenterSpot: Boolean): TAffineVector; overload;

const
  cPI: single       = 3.141592654;
  cPIdiv180: single = 0.017453292;
  c180divPI: single = 57.29577951;
  c2PI: single      = 6.283185307;
  cPIdiv2: single   = 1.570796326;
  cPIdiv4: single   = 0.785398163;
  c3PIdiv2: single  = 4.71238898;
  c3PIdiv4: single  = 2.35619449;
  cInv2PI: single   = 1 / 6.283185307;
  cInv360: single   = 1 / 360;
  c180: single      = 180;
  c360: single      = 360;
  cOneHalf: single  = 0.5;
  cLn10: single     = 2.302585093;

  // Ranges of the IEEE floating point types, including denormals
  // with Math.pas compatible name
  MinSingle   = 1.5E-45;
  MaxSingle   = 3.4E+38;
  MinDouble   = 5.0E-324;
  MaxDouble   = 1.7E+308;
  MinExtended = 3.4E-4932;
  MaxExtended = 1.1E+4932;
  MinComp     = -9.223372036854775807E+18;
  MaxComp     = 9.223372036854775807E+18;

  // --------------------------------------------------------------
  // --------------------------------------------------------------
  // --------------------------------------------------------------
implementation

// --------------------------------------------------------------
// --------------------------------------------------------------
// --------------------------------------------------------------

uses Math;

const
  // to be used as descriptive indices
  x = 0;
  y = 1;
  z = 2;
  w = 3;

  cZero: single       = 0.0;
  cOne: single        = 1.0;
  cOneDotFive: single = 0.5;

  // ------------------------------------------------------------------------------
  // ----------------- vector functions -------------------------------------------
  // ------------------------------------------------------------------------------

  // TexPointMake
  //
function TexPointMake(const S, T: single): TTexPoint;
begin
  result.S := S;
  result.T := T;
end;

// AffineVectorMake
//
function AffineVectorMake(const x, y, z: single): TAffineVector; overload;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

// AffineVectorMake
//
function AffineVectorMake(const v: TVector): TAffineVector;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
end;

// SetAffineVector
//
procedure SetAffineVector(out v: TAffineVector; const x, y, z: single); overload;
begin
  v[0] := x;
  v[1] := y;
  v[2] := z;
end;

// SetVector (affine)
//
procedure SetVector(out v: TAffineVector; const x, y, z: single);
begin
  v[0] := x;
  v[1] := y;
  v[2] := z;
end;

// SetVector (affine-hmg)
//
procedure SetVector(out v: TAffineVector; const vSrc: TVector);
begin
  v[0] := vSrc[0];
  v[1] := vSrc[1];
  v[2] := vSrc[2];
end;

// SetVector (affine-affine)
//
procedure SetVector(out v: TAffineVector; const vSrc: TAffineVector);
begin
  v[0] := vSrc[0];
  v[1] := vSrc[1];
  v[2] := vSrc[2];
end;

// SetVector (affine double - affine single)
//
procedure SetVector(out v: TAffineDblVector; const vSrc: TAffineVector);
begin
  v[0] := vSrc[0];
  v[1] := vSrc[1];
  v[2] := vSrc[2];
end;

// SetVector (affine double - hmg single)
//
procedure SetVector(out v: TAffineDblVector; const vSrc: TVector);
begin
  v[0] := vSrc[0];
  v[1] := vSrc[1];
  v[2] := vSrc[2];
end;

// VectorMake
//
function VectorMake(const v: TAffineVector; w: single = 0): TVector;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := w;
end;

// VectorMake
//
function VectorMake(const x, y, z: single; w: single = 0): TVector;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
  result[3] := w;
end;

// PointMake (xyz)
//
function PointMake(const x, y, z: single): TVector; overload;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
  result[3] := 1;
end;

// PointMake (affine)
//
function PointMake(const v: TAffineVector): TVector; overload;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := 1;
end;

// PointMake (hmg)
//
function PointMake(const v: TVector): TVector; overload;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := 1;
end;

// SetVector
//
procedure SetVector(out v: TVector; const x, y, z: single; w: single = 0);
begin
  v[0] := x;
  v[1] := y;
  v[2] := z;
  v[3] := w;
end;

// SetVector
//
procedure SetVector(out v: TVector; const av: TAffineVector; w: single = 0);
begin
  v[0] := av[0];
  v[1] := av[1];
  v[2] := av[2];
  v[3] := w;
end;

// SetVector
//
procedure SetVector(out v: TVector; const vSrc: TVector);
begin
  // faster than memcpy, move or ':=' on the TVector...
  v[0] := vSrc[0];
  v[1] := vSrc[1];
  v[2] := vSrc[2];
  v[3] := vSrc[3];
end;

// MakePoint
//
procedure MakePoint(out v: TVector; const x, y, z: single);
begin
  v[0] := x;
  v[1] := y;
  v[2] := z;
  v[3] := 1;
end;

// MakePoint
//
procedure MakePoint(out v: TVector; const av: TAffineVector);
begin
  v[0] := av[0];
  v[1] := av[1];
  v[2] := av[2];
  v[3] := 1;
end;

// MakePoint
//
procedure MakePoint(out v: TVector; const av: TVector);
begin
  v[0] := av[0];
  v[1] := av[1];
  v[2] := av[2];
  v[3] := 1;
end;

// MakeVector
//
procedure MakeVector(out v: TAffineVector; const x, y, z: single); overload;
begin
  v[0] := x;
  v[1] := y;
  v[2] := z;
end;

// MakeVector
//
procedure MakeVector(out v: TVector; const x, y, z: single);
begin
  v[0] := x;
  v[1] := y;
  v[2] := z;
  v[3] := 0;
end;

// MakeVector
//
procedure MakeVector(out v: TVector; const av: TAffineVector);
begin
  v[0] := av[0];
  v[1] := av[1];
  v[2] := av[2];
  v[3] := 0;
end;

// MakeVector
//
procedure MakeVector(out v: TVector; const av: TVector);
begin
  v[0] := av[0];
  v[1] := av[1];
  v[2] := av[2];
  v[3] := 0;
end;

// RstVector (affine)
//
procedure RstVector(var v: TAffineVector);
begin
  v[0] := 0;
  v[1] := 0;
  v[2] := 0;
end;

// RstVector (hmg)
//
procedure RstVector(var v: TVector);
begin
  v[0] := 0;
  v[1] := 0;
  v[2] := 0;
  v[3] := 0;
end;

// VectorAdd (func)
//
function VectorAdd(const V1, V2: TVector2f): TVector2f;
begin
  result[0] := V1[0] + V2[0];
  result[1] := V1[1] + V2[1];
end;

// VectorAdd (func, affine)
//
function VectorAdd(const V1, V2: TAffineVector): TAffineVector;
begin
  result[0] := V1[0] + V2[0];
  result[1] := V1[1] + V2[1];
  result[2] := V1[2] + V2[2];
end;

// VectorAdd (proc, affine)
//
procedure VectorAdd(const V1, V2: TAffineVector; var vr: TAffineVector); overload;
begin
  vr[0] := V1[0] + V2[0];
  vr[1] := V1[1] + V2[1];
  vr[2] := V1[2] + V2[2];
end;

// VectorAdd (proc, affine)
//
procedure VectorAdd(const V1, V2: TAffineVector; vr: PAffineVector); overload;
begin
  vr^[0] := V1[0] + V2[0];
  vr^[1] := V1[1] + V2[1];
  vr^[2] := V1[2] + V2[2];
end;

// VectorAdd (hmg)
//
function VectorAdd(const V1, V2: TVector): TVector;
begin
  result[0] := V1[0] + V2[0];
  result[1] := V1[1] + V2[1];
  result[2] := V1[2] + V2[2];
  result[3] := V1[3] + V2[3];
end;

// VectorAdd (hmg, proc)
//
procedure VectorAdd(const V1, V2: TVector; var vr: TVector);
begin
  vr[0] := V1[0] + V2[0];
  vr[1] := V1[1] + V2[1];
  vr[2] := V1[2] + V2[2];
  vr[3] := V1[3] + V2[3];
end;

// VectorAdd (affine, single)
//
function VectorAdd(const v: TAffineVector; const f: single): TAffineVector;
begin
  result[0] := v[0] + f;
  result[1] := v[1] + f;
  result[2] := v[2] + f;
end;

// VectorAdd (hmg, single)
//
function VectorAdd(const v: TVector; const f: single): TVector;
begin
  result[0] := v[0] + f;
  result[1] := v[1] + f;
  result[2] := v[2] + f;
  result[3] := v[3] + f;
end;

// PointAdd (hmg, W = 1)
//
function PointAdd(var V1: TVector; const V2: TVector): TVector;
begin
  result[0] := V1[0] + V2[0];
  result[1] := V1[1] + V2[1];
  result[2] := V1[2] + V2[2];
  result[3] := 1;
end;

// AddVector (affine)
//
procedure AddVector(var V1: TAffineVector; const V2: TAffineVector);
begin
  V1[0] := V1[0] + V2[0];
  V1[1] := V1[1] + V2[1];
  V1[2] := V1[2] + V2[2];
end;

// AddVector (affine)
//
procedure AddVector(var V1: TAffineVector; const V2: TVector);
begin
  V1[0] := V1[0] + V2[0];
  V1[1] := V1[1] + V2[1];
  V1[2] := V1[2] + V2[2];
end;

// AddVector (hmg)
//
procedure AddVector(var V1: TVector; const V2: TVector);
begin
  V1[0] := V1[0] + V2[0];
  V1[1] := V1[1] + V2[1];
  V1[2] := V1[2] + V2[2];
  V1[3] := V1[3] + V2[3];
end;

// AddVector (affine)
//
procedure AddVector(var v: TAffineVector; const f: single);
begin
  v[0] := v[0] + f;
  v[1] := v[1] + f;
  v[2] := v[2] + f;
end;

// AddVector (hmg)
//
procedure AddVector(var v: TVector; const f: single);
begin
  v[0] := v[0] + f;
  v[1] := v[1] + f;
  v[2] := v[2] + f;
  v[3] := v[3] + f;
end;

// AddPoint (hmg, W = 1)
//
procedure AddPoint(var V1: TVector; const V2: TVector);
begin
  V1[0] := V1[0] + V2[0];
  V1[1] := V1[1] + V2[1];
  V1[2] := V1[2] + V2[2];
  V1[3] := 1;
end;

// TexPointArrayAdd
//
procedure TexPointArrayAdd(const src: PTexPointArray; const delta: TTexPoint;
  const nb: Integer;
  dest: PTexPointArray); overload;
var
  i: Integer;
begin
  for i := 0 to nb - 1 do begin
      dest^[i].S := src^[i].S + delta.S;
      dest^[i].T := src^[i].T + delta.T;
    end;
end;

// TexPointArrayScaleAndAdd
//
procedure TexPointArrayScaleAndAdd(const src: PTexPointArray; const delta: TTexPoint;
  const nb: Integer; const scale: TTexPoint;
  dest: PTexPointArray); overload;
var
  i: Integer;
begin
  for i := 0 to nb - 1 do begin
      dest^[i].S := src^[i].S * scale.S + delta.S;
      dest^[i].T := src^[i].T * scale.T + delta.T;
    end;
end;

// VectorArrayAdd
//
procedure VectorArrayAdd(const src: PAffineVectorArray; const delta: TAffineVector;
  const nb: Integer; dest: PAffineVectorArray);
var
  i: Integer;
begin
  for i := 0 to nb - 1 do begin
      dest^[i][0] := src^[i][0] + delta[0];
      dest^[i][1] := src^[i][1] + delta[1];
      dest^[i][2] := src^[i][2] + delta[2];
    end;
end;

// VectorSubtract (func, affine)
//
function VectorSubtract(const V1, V2: TAffineVector): TAffineVector;
begin
  result[0] := V1[0] - V2[0];
  result[1] := V1[1] - V2[1];
  result[2] := V1[2] - V2[2];
end;

// VectorSubtract (func, 2f)
//
function VectorSubtract(const V1, V2: TVector2f): TVector2f;
begin
  result[0] := V1[0] - V2[0];
  result[1] := V1[1] - V2[1];
end;

// VectorSubtract (proc, affine)
//
procedure VectorSubtract(const V1, V2: TAffineVector; var result: TAffineVector);
begin
  result[0] := V1[0] - V2[0];
  result[1] := V1[1] - V2[1];
  result[2] := V1[2] - V2[2];
end;

// VectorSubtract (proc, affine-hmg)
//
procedure VectorSubtract(const V1, V2: TAffineVector; var result: TVector);
begin
  result[0] := V1[0] - V2[0];
  result[1] := V1[1] - V2[1];
  result[2] := V1[2] - V2[2];
  result[3] := 0;
end;

// VectorSubtract
//
procedure VectorSubtract(const V1: TVector; V2: TAffineVector; var result: TVector);
begin
  result[0] := V1[0] - V2[0];
  result[1] := V1[1] - V2[1];
  result[2] := V1[2] - V2[2];
  result[3] := V1[0];
end;

// VectorSubtract (hmg)
//
function VectorSubtract(const V1, V2: TVector): TVector;
begin
  result[0] := V1[0] - V2[0];
  result[1] := V1[1] - V2[1];
  result[2] := V1[2] - V2[2];
  result[3] := V1[3] - V2[3];
end;

// VectorSubtract (proc, hmg)
//
procedure VectorSubtract(const V1, V2: TVector; var result: TVector);
begin
  result[0] := V1[0] - V2[0];
  result[1] := V1[1] - V2[1];
  result[2] := V1[2] - V2[2];
  result[3] := V1[3] - V2[3];
end;

// VectorSubtract (proc, affine)
//
procedure VectorSubtract(const V1, V2: TVector; var result: TAffineVector); overload;
begin
  result[0] := V1[0] - V2[0];
  result[1] := V1[1] - V2[1];
  result[2] := V1[2] - V2[2];
end;

// VectorSubtract (affine, single)
//
function VectorSubtract(const V1: TAffineVector; delta: single): TAffineVector;
begin
  result[0] := V1[0] - delta;
  result[1] := V1[1] - delta;
  result[2] := V1[2] - delta;
end;

// VectorSubtract (hmg, single)
//
function VectorSubtract(const V1: TVector; delta: single): TVector;
begin
  result[0] := V1[0] - delta;
  result[1] := V1[1] - delta;
  result[2] := V1[2] - delta;
  result[3] := V1[3] - delta;
end;

// SubtractVector (affine)
//
procedure SubtractVector(var V1: TAffineVector; const V2: TAffineVector);
begin
  V1[0] := V1[0] - V2[0];
  V1[1] := V1[1] - V2[1];
  V1[2] := V1[2] - V2[2];
end;

// SubtractVector (2f)
//
procedure SubtractVector(var V1: TVector2f; const V2: TVector2f);
begin
  V1[0] := V1[0] - V2[0];
  V1[1] := V1[1] - V2[1];
end;

// SubtractVector (hmg)
//
procedure SubtractVector(var V1: TVector; const V2: TVector);
begin
  V1[0] := V1[0] - V2[0];
  V1[1] := V1[1] - V2[1];
  V1[2] := V1[2] - V2[2];
  V1[3] := V1[3] - V2[3];
end;

// CombineVector (var)
//
procedure CombineVector(var vr: TAffineVector; const v: TAffineVector; var f: single);
begin
  vr[0] := vr[0] + v[0] * f;
  vr[1] := vr[1] + v[1] * f;
  vr[2] := vr[2] + v[2] * f;
end;

// CombineVector (pointer)
//
procedure CombineVector(var vr: TAffineVector; const v: TAffineVector; pf: PFloat);
begin
  vr[0] := vr[0] + v[0] * pf^;
  vr[1] := vr[1] + v[1] * pf^;
  vr[2] := vr[2] + v[2] * pf^;
end;

// TexPointCombine
//
function TexPointCombine(const t1, t2: TTexPoint; f1, f2: single): TTexPoint;
begin
  result.S := (f1 * t1.S) + (f2 * t2.S);
  result.T := (f1 * t1.T) + (f2 * t2.T);
end;

// VectorCombine
//
function VectorCombine(const V1, V2: TAffineVector; const f1, f2: single): TAffineVector;
begin
  result[x] := (f1 * V1[x]) + (f2 * V2[x]);
  result[y] := (f1 * V1[y]) + (f2 * V2[y]);
  result[z] := (f1 * V1[z]) + (f2 * V2[z]);
end;

// VectorCombine3 (func)
//
function VectorCombine3(const V1, V2, V3: TAffineVector; const f1, f2, F3: single): TAffineVector;
begin
  result[x] := (f1 * V1[x]) + (f2 * V2[x]) + (F3 * V3[x]);
  result[y] := (f1 * V1[y]) + (f2 * V2[y]) + (F3 * V3[y]);
  result[z] := (f1 * V1[z]) + (f2 * V2[z]) + (F3 * V3[z]);
end;

// VectorCombine3 (vector)
//
procedure VectorCombine3(const V1, V2, V3: TAffineVector; const f1, f2, F3: single; var vr: TAffineVector);
begin
  vr[x] := (f1 * V1[x]) + (f2 * V2[x]) + (F3 * V3[x]);
  vr[y] := (f1 * V1[y]) + (f2 * V2[y]) + (F3 * V3[y]);
  vr[z] := (f1 * V1[z]) + (f2 * V2[z]) + (F3 * V3[z]);
end;

// CombineVector
//
procedure CombineVector(var vr: TVector; const v: TVector; var f: single); overload;
begin
  vr[0] := vr[0] + v[0] * f;
  vr[1] := vr[1] + v[1] * f;
  vr[2] := vr[2] + v[2] * f;
  vr[3] := vr[3] + v[3] * f;
end;

// CombineVector
//
procedure CombineVector(var vr: TVector; const v: TAffineVector; var f: single); overload;
begin
  vr[0] := vr[0] + v[0] * f;
  vr[1] := vr[1] + v[1] * f;
  vr[2] := vr[2] + v[2] * f;
end;

// VectorCombine
//
function VectorCombine(const V1, V2: TVector; const f1, f2: single): TVector;
begin
  result[x] := (f1 * V1[x]) + (f2 * V2[x]);
  result[y] := (f1 * V1[y]) + (f2 * V2[y]);
  result[z] := (f1 * V1[z]) + (f2 * V2[z]);
  result[w] := (f1 * V1[w]) + (f2 * V2[w]);
end;

// VectorCombine
//
function VectorCombine(const V1: TVector; const V2: TAffineVector; const f1, f2: single): TVector; overload;
begin
  result[x] := (f1 * V1[x]) + (f2 * V2[x]);
  result[y] := (f1 * V1[y]) + (f2 * V2[y]);
  result[z] := (f1 * V1[z]) + (f2 * V2[z]);
  result[w] := f1 * V1[w];
end;

// VectorCombine
//
procedure VectorCombine(const V1, V2: TVector; const f1, f2: single; var vr: TVector); overload;
begin
  vr[0] := (f1 * V1[0]) + (f2 * V2[0]);
  vr[1] := (f1 * V1[1]) + (f2 * V2[1]);
  vr[2] := (f1 * V1[2]) + (f2 * V2[2]);
  vr[3] := (f1 * V1[3]) + (f2 * V2[3]);
end;

// VectorCombine (F1=1.0)
//
procedure VectorCombine(const V1, V2: TVector; const f2: single; var vr: TVector); overload;
begin
  vr[0] := V1[0] + (f2 * V2[0]);
  vr[1] := V1[1] + (f2 * V2[1]);
  vr[2] := V1[2] + (f2 * V2[2]);
  vr[3] := V1[3] + (f2 * V2[3]);
end;

// VectorCombine
//
procedure VectorCombine(const V1: TVector; const V2: TAffineVector; const f1, f2: single; var vr: TVector);
begin
  vr[x] := (f1 * V1[x]) + (f2 * V2[x]);
  vr[y] := (f1 * V1[y]) + (f2 * V2[y]);
  vr[z] := (f1 * V1[z]) + (f2 * V2[z]);
  vr[w] := f1 * V1[w];
end;

// VectorCombine3
//
function VectorCombine3(const V1, V2, V3: TVector; const f1, f2, F3: single): TVector;
begin
  result[x] := (f1 * V1[x]) + (f2 * V2[x]) + (F3 * V3[x]);
  result[y] := (f1 * V1[y]) + (f2 * V2[y]) + (F3 * V3[y]);
  result[z] := (f1 * V1[z]) + (f2 * V2[z]) + (F3 * V3[z]);
  result[w] := (f1 * V1[w]) + (f2 * V2[w]) + (F3 * V3[w]);
end;

// VectorCombine3
//
procedure VectorCombine3(const V1, V2, V3: TVector; const f1, f2, F3: single; var vr: TVector);
begin
  vr[x] := (f1 * V1[x]) + (f2 * V2[x]) + (F3 * V3[x]);
  vr[y] := (f1 * V1[y]) + (f2 * V2[y]) + (F3 * V3[y]);
  vr[z] := (f1 * V1[z]) + (f2 * V2[z]) + (F3 * V3[z]);
  vr[w] := (f1 * V1[w]) + (f2 * V2[w]) + (F3 * V3[w]);
end;

// VectorDotProduct (2f)
//
function VectorDotProduct(const V1, V2: TVector2f): single;
begin
  result := V1[0] * V2[0] + V1[1] * V2[1];
end;

// VectorDotProduct (affine)
//
function VectorDotProduct(const V1, V2: TAffineVector): single;
begin
  result := V1[0] * V2[0] + V1[1] * V2[1] + V1[2] * V2[2];
end;

// VectorDotProduct (hmg)
//
function VectorDotProduct(const V1, V2: TVector): single;
begin
  result := V1[0] * V2[0] + V1[1] * V2[1] + V1[2] * V2[2] + V1[3] * V2[3];
end;

// VectorDotProduct
//
function VectorDotProduct(const V1: TVector; const V2: TAffineVector): single;
begin
  result := V1[0] * V2[0] + V1[1] * V2[1] + V1[2] * V2[2];
end;

// PointProject (affine)
//
function PointProject(const p, origin, direction: TAffineVector): single;
begin
  result := direction[0] * (p[0] - origin[0])
    + direction[1] * (p[1] - origin[1])
    + direction[2] * (p[2] - origin[2]);
end;

// PointProject (vector)
//
function PointProject(const p, origin, direction: TVector): single;
begin
  result := direction[0] * (p[0] - origin[0])
    + direction[1] * (p[1] - origin[1])
    + direction[2] * (p[2] - origin[2]);
end;

// VectorCrossProduct
//
function VectorCrossProduct(const V1, V2: TAffineVector): TAffineVector;
begin
  result[x] := V1[y] * V2[z] - V1[z] * V2[y];
  result[y] := V1[z] * V2[x] - V1[x] * V2[z];
  result[z] := V1[x] * V2[y] - V1[y] * V2[x];
end;

// VectorCrossProduct
//
function VectorCrossProduct(const V1, V2: TVector): TVector;
begin
  result[x] := V1[y] * V2[z] - V1[z] * V2[y];
  result[y] := V1[z] * V2[x] - V1[x] * V2[z];
  result[z] := V1[x] * V2[y] - V1[y] * V2[x];
  result[w] := 0;
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const V1, V2: TVector; var vr: TVector);
begin
  vr[x] := V1[y] * V2[z] - V1[z] * V2[y];
  vr[y] := V1[z] * V2[x] - V1[x] * V2[z];
  vr[z] := V1[x] * V2[y] - V1[y] * V2[x];
  vr[w] := 0;
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const V1, V2: TAffineVector; var vr: TVector); overload;
begin
  vr[x] := V1[y] * V2[z] - V1[z] * V2[y];
  vr[y] := V1[z] * V2[x] - V1[x] * V2[z];
  vr[z] := V1[x] * V2[y] - V1[y] * V2[x];
  vr[w] := 0;
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const V1, V2: TVector; var vr: TAffineVector); overload;
begin
  vr[x] := V1[y] * V2[z] - V1[z] * V2[y];
  vr[y] := V1[z] * V2[x] - V1[x] * V2[z];
  vr[z] := V1[x] * V2[y] - V1[y] * V2[x];
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const V1, V2: TAffineVector; var vr: TAffineVector); overload;
begin
  vr[x] := V1[y] * V2[z] - V1[z] * V2[y];
  vr[y] := V1[z] * V2[x] - V1[x] * V2[z];
  vr[z] := V1[x] * V2[y] - V1[y] * V2[x];
end;

// Lerp
//
function Lerp(const start, stop, T: single): single;
begin
  result := start + (stop - start) * T;
end;

// Angle Lerp
//
function AngleLerp(start, stop, T: single): single;
var
  d: single;
begin
  start := NormalizeAngle(start);
  stop := NormalizeAngle(stop);
  d := stop - start;
  if d > PI then begin
      // positive d, angle on opposite side, becomes negative i.e. changes direction
      d := -d - c2PI;
    end
  else if d < -PI then begin
      // negative d, angle on opposite side, becomes positive i.e. changes direction
      d := d + c2PI;
    end;
  result := start + d * T;
end;

// DistanceBetweenAngles
//
function DistanceBetweenAngles(angle1, angle2: single): single;
begin
  angle1 := NormalizeAngle(angle1);
  angle2 := NormalizeAngle(angle2);
  result := Abs(angle2 - angle1);
  if result > PI then
      result := c2PI - result;
end;

// TexPointLerp
//
function TexPointLerp(const t1, t2: TTexPoint; T: single): TTexPoint; overload;
begin
  result.S := t1.S + (t2.S - t1.S) * T;
  result.T := t1.T + (t2.T - t1.T) * T;
end;

// VectorAffineLerp
//
function VectorLerp(const V1, V2: TAffineVector; T: single): TAffineVector;
begin
  result[x] := V1[x] + (V2[x] - V1[x]) * T;
  result[y] := V1[y] + (V2[y] - V1[y]) * T;
  result[z] := V1[z] + (V2[z] - V1[z]) * T;
end;

// VectorLerp
//
procedure VectorLerp(const V1, V2: TAffineVector; T: single; var vr: TAffineVector);
begin
  vr[x] := V1[x] + (V2[x] - V1[x]) * T;
  vr[y] := V1[y] + (V2[y] - V1[y]) * T;
  vr[z] := V1[z] + (V2[z] - V1[z]) * T;
end;

// VectorLerp
//
function VectorLerp(const V1, V2: TVector; T: single): TVector;
begin
  result[x] := V1[x] + (V2[x] - V1[x]) * T;
  result[y] := V1[y] + (V2[y] - V1[y]) * T;
  result[z] := V1[z] + (V2[z] - V1[z]) * T;
  result[w] := V1[w] + (V2[w] - V1[w]) * T;
end;

// VectorLerp
//
procedure VectorLerp(const V1, V2: TVector; T: single; var vr: TVector);
begin
  vr[x] := V1[x] + (V2[x] - V1[x]) * T;
  vr[y] := V1[y] + (V2[y] - V1[y]) * T;
  vr[z] := V1[z] + (V2[z] - V1[z]) * T;
  vr[w] := V1[w] + (V2[w] - V1[w]) * T;
end;

// VectorAngleLerp
//
function VectorAngleLerp(const V1, V2: TAffineVector; T: single): TAffineVector;
var
  q1, q2, qR: TQuaternion;
  M         : TMatrix;
  Tran      : TTransformations;
begin
  if VectorEquals(V1, V2) then begin
      result := V1;
    end
  else begin
      q1 := QuaternionFromEuler(GeometryLib.RadToDeg(V1[0]), GeometryLib.RadToDeg(V1[1]), GeometryLib.RadToDeg(V1[2]), eulZYX);
      q2 := QuaternionFromEuler(GeometryLib.RadToDeg(V2[0]), GeometryLib.RadToDeg(V2[1]), GeometryLib.RadToDeg(V2[2]), eulZYX);
      qR := QuaternionSlerp(q1, q2, T);
      M := QuaternionToMatrix(qR);
      MatrixDecompose(M, Tran);
      result[0] := Tran[ttRotateX];
      result[1] := Tran[ttRotateY];
      result[2] := Tran[ttRotateZ];
    end;
end;

// VectorAngleCombine
//
function VectorAngleCombine(const V1, V2: TAffineVector; f: single): TAffineVector;
begin
  result := VectorCombine(V1, V2, 1, f);
end;

// VectorArrayLerp (hmg)
//
procedure VectorArrayLerp(const src1, src2: PVectorArray; T: single; n: Integer; dest: PVectorArray);
var
  i: Integer;
begin
  for i := 0 to n - 1 do begin
      dest^[i][0] := src1^[i][0] + (src2^[i][0] - src1^[i][0]) * T;
      dest^[i][1] := src1^[i][1] + (src2^[i][1] - src1^[i][1]) * T;
      dest^[i][2] := src1^[i][2] + (src2^[i][2] - src1^[i][2]) * T;
      dest^[i][3] := src1^[i][3] + (src2^[i][3] - src1^[i][3]) * T;
    end;
end;

// VectorArrayLerp (affine)
//
procedure VectorArrayLerp(const src1, src2: PAffineVectorArray; T: single; n: Integer; dest: PAffineVectorArray);
var
  i: Integer;
begin
  for i := 0 to n - 1 do begin
      dest^[i][0] := src1^[i][0] + (src2^[i][0] - src1^[i][0]) * T;
      dest^[i][1] := src1^[i][1] + (src2^[i][1] - src1^[i][1]) * T;
      dest^[i][2] := src1^[i][2] + (src2^[i][2] - src1^[i][2]) * T;
    end;
end;

procedure VectorArrayLerp(const src1, src2: PTexPointArray; T: single; n: Integer; dest: PTexPointArray);
var
  i: Integer;
begin
  for i := 0 to n - 1 do begin
      dest^[i].S := src1^[i].S + (src2^[i].S - src1^[i].S) * T;
      dest^[i].T := src1^[i].T + (src2^[i].T - src1^[i].T) * T;
    end;
end;

// InterpolateCombined
//
function InterpolateCombined(const start, stop, delta: single; const DistortionDegree: single; const InterpolationType: TGLInterpolationType): single;
begin
  case InterpolationType of
    itLinear: result := Lerp(start, stop, delta);
    itPower: result := InterpolatePower(start, stop, delta, DistortionDegree);
    itSin: result := InterpolateSin(start, stop, delta);
    itSinAlt: result := InterpolateSinAlt(start, stop, delta);
    itTan: result := InterpolateTan(start, stop, delta);
    itLn: result := InterpolateLn(start, stop, delta, DistortionDegree);
    itExp: result := InterpolateExp(start, stop, delta, DistortionDegree);
    else
      begin
        result := -1;
        Assert(False);
      end;
  end;
end;

// InterpolateCombinedFastPower
//
function InterpolateCombinedFastPower(const OriginalStart, OriginalStop, OriginalCurrent: single; const TargetStart, TargetStop: single; const DistortionDegree: single): single;
begin
  result := InterpolatePower(TargetStart, TargetStop, (OriginalCurrent - OriginalStart) / (OriginalStop - OriginalStart), DistortionDegree);
end;

// InterpolateCombinedSafe
//
function InterpolateCombinedSafe(const OriginalStart, OriginalStop, OriginalCurrent: single; const TargetStart, TargetStop: single; const DistortionDegree: single; const InterpolationType: TGLInterpolationType): single;
var
  ChangeDelta: single;
begin
  if OriginalStop = OriginalStart then
      result := TargetStart
  else
    begin
      ChangeDelta := (OriginalCurrent - OriginalStart) / (OriginalStop - OriginalStart);
      result := InterpolateCombined(TargetStart, TargetStop, ChangeDelta, DistortionDegree, InterpolationType);
    end;
end;

// InterpolateCombinedFast
//
function InterpolateCombinedFast(const OriginalStart, OriginalStop, OriginalCurrent: single; const TargetStart, TargetStop: single; const DistortionDegree: single; const InterpolationType: TGLInterpolationType): single;
var
  ChangeDelta: single;
begin
  ChangeDelta := (OriginalCurrent - OriginalStart) / (OriginalStop - OriginalStart);
  result := InterpolateCombined(TargetStart, TargetStop, ChangeDelta, DistortionDegree, InterpolationType);
end;

// InterpolateLn
//
function InterpolateLn(const start, stop, delta: single; const DistortionDegree: single): single;
begin
  result := (stop - start) * Ln(1 + delta * DistortionDegree) / Ln(1 + DistortionDegree) + start;
end;

// InterpolateExp
//
function InterpolateExp(const start, stop, delta: single; const DistortionDegree: single): single;
begin
  result := (stop - start) * Exp(-DistortionDegree * (1 - delta)) + start;
end;

// InterpolateSinAlt
//
function InterpolateSinAlt(const start, stop, delta: single): single;
begin
  result := (stop - start) * delta * Sin(delta * PI / 2) + start;
end;

// InterpolateSin
//
function InterpolateSin(const start, stop, delta: single): single;
begin
  result := (stop - start) * Sin(delta * PI / 2) + start;
end;

// InterpolateTan
//
function InterpolateTan(const start, stop, delta: single): single;
begin
  result := (stop - start) * GeometryLib.Tan(delta * PI / 4) + start;
end;

// InterpolatePower
//
function InterpolatePower(const start, stop, delta: single; const DistortionDegree: single): single;
begin
  if (Round(DistortionDegree) <> DistortionDegree) and (delta < 0) then
      result := (stop - start) * GeometryLib.Power(delta, Round(DistortionDegree)) + start
  else
      result := (stop - start) * GeometryLib.Power(delta, DistortionDegree) + start;
end;

// MatrixLerp
//
function MatrixLerp(const m1, m2: TMatrix; const delta: single): TMatrix;
var
  i, J: Integer;
begin
  for J := 0 to 3 do
    for i := 0 to 3 do
        result[i][J] := m1[i][J] + (m2[i][J] - m1[i][J]) * delta;
end;

// VectorLength (array)
//
function VectorLength(const v: array of single): single;
var
  i: Integer;
begin
  result := 0;
  for i := low(v) to high(v) do
      result := result + Sqr(v[i]);
  result := Sqrt(result);
end;

// VectorLength  (x, y)
//
function VectorLength(const x, y: single): single;
begin
  result := Sqrt(x * x + y * y);
end;

// VectorLength (x, y, z)
//
function VectorLength(const x, y, z: single): single;
begin
  result := Sqrt(x * x + y * y + z * z);
end;

// VectorLength
//
function VectorLength(const v: TVector2f): single;
begin
  result := Sqrt(VectorNorm(v[0], v[1]));
end;

// VectorLength
//
function VectorLength(const v: TAffineVector): single;
begin
  result := Sqrt(VectorNorm(v));
end;

// VectorLength
//
function VectorLength(const v: TVector): single;
begin
  result := Sqrt(VectorNorm(v));
end;

// VectorNorm
//
function VectorNorm(const x, y: single): single;
begin
  result := Sqr(x) + Sqr(y);
end;

// VectorNorm (affine)
//
function VectorNorm(const v: TAffineVector): single;
begin
  result := v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
end;

// VectorNorm (hmg)
//
function VectorNorm(const v: TVector): single;
begin
  result := v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
end;

// VectorNorm
//
function VectorNorm(var v: array of single): single;
var
  i: Integer;
begin
  result := 0;
  for i := low(v) to high(v) do
      result := result + v[i] * v[i];
end;

// NormalizeVector (2f)
//
procedure NormalizeVector(var v: TVector2f);
var
  invLen: single;
  vn    : single;
begin
  vn := VectorNorm(v);
  if vn > 0 then begin
      invLen := RSqrt(vn);
      v[0] := v[0] * invLen;
      v[1] := v[1] * invLen;
    end;
end;

// NormalizeVector (affine)
//
procedure NormalizeVector(var v: TAffineVector);
var
  invLen: single;
  vn    : single;
begin
  vn := VectorNorm(v);
  if vn > 0 then begin
      invLen := RSqrt(vn);
      v[0] := v[0] * invLen;
      v[1] := v[1] * invLen;
      v[2] := v[2] * invLen;
    end;
end;

// VectorNormalize
//
function VectorNormalize(const v: TVector2f): TVector2f;
var
  invLen: single;
  vn    : single;
begin
  vn := VectorNorm(v[0], v[1]);
  if vn = 0 then
      result := v
  else begin
      invLen := RSqrt(vn);
      result[0] := v[0] * invLen;
      result[1] := v[1] * invLen;
    end;
end;

// VectorNormalize
//
function VectorNormalize(const v: TAffineVector): TAffineVector;
var
  invLen: single;
  vn    : single;
begin
  vn := VectorNorm(v);
  if vn = 0 then
      SetVector(result, v)
  else begin
      invLen := RSqrt(vn);
      result[0] := v[0] * invLen;
      result[1] := v[1] * invLen;
      result[2] := v[2] * invLen;
    end;
end;

// NormalizeVectorArray
//
procedure NormalizeVectorArray(list: PAffineVectorArray; n: Integer);
var
  i: Integer;
begin
  for i := 0 to n - 1 do
      NormalizeVector(list^[i]);
end;

// NormalizeVector (hmg)
//
procedure NormalizeVector(var v: TVector);
var
  invLen: single;
  vn    : single;
begin
  vn := VectorNorm(v);
  if vn > 0 then begin
      invLen := RSqrt(vn);
      v[0] := v[0] * invLen;
      v[1] := v[1] * invLen;
      v[2] := v[2] * invLen;
    end;
  v[3] := 0;
end;

// VectorNormalize (hmg, func)
//
function VectorNormalize(const v: TVector): TVector;
var
  invLen: single;
  vn    : single;
begin
  vn := VectorNorm(v);
  if vn = 0 then
      SetVector(result, v)
  else begin
      invLen := RSqrt(vn);
      result[0] := v[0] * invLen;
      result[1] := v[1] * invLen;
      result[2] := v[2] * invLen;
    end;
  result[3] := 0;
end;

// VectorAngleCosine
//
function VectorAngleCosine(const V1, V2: TAffineVector): single;
begin
  result := VectorDotProduct(V1, V2) / (VectorLength(V1) * VectorLength(V2));
end;

// VectorAngleCosine
//
function VectorAngleCosine(const V1, V2: TVector): single;
begin
  result := VectorDotProduct(V1, V2) / (VectorLength(V1) * VectorLength(V2));
end;

// VectorNegate (affine)
//
function VectorNegate(const v: TAffineVector): TAffineVector;
begin
  result[0] := -v[0];
  result[1] := -v[1];
  result[2] := -v[2];
end;

// VectorNegate (hmg)
//
function VectorNegate(const v: TVector): TVector;
begin
  result[0] := -v[0];
  result[1] := -v[1];
  result[2] := -v[2];
  result[3] := -v[3];
end;

// NegateVector
//
procedure NegateVector(var v: TAffineVector);
begin
  v[0] := -v[0];
  v[1] := -v[1];
  v[2] := -v[2];
end;

// NegateVector
//
procedure NegateVector(var v: TVector);
begin
  v[0] := -v[0];
  v[1] := -v[1];
  v[2] := -v[2];
  v[3] := -v[3];
end;

// NegateVector
//
procedure NegateVector(var v: array of single);
var
  i: Integer;
begin
  for i := low(v) to high(v) do
      v[i] := -v[i];
end;

// ScaleVector (2f)
//
procedure ScaleVector(var v: TVector2f; factor: single);
begin
  v[0] := v[0] * factor;
  v[1] := v[1] * factor;
end;

// ScaleVector (affine)
//
procedure ScaleVector(var v: TAffineVector; factor: single);
begin
  v[0] := v[0] * factor;
  v[1] := v[1] * factor;
  v[2] := v[2] * factor;
end;

// ScaleVector (hmg)
//
procedure ScaleVector(var v: TVector; factor: single);
begin
  v[0] := v[0] * factor;
  v[1] := v[1] * factor;
  v[2] := v[2] * factor;
  v[3] := v[3] * factor;
end;

// ScaleVector (affine vector)
//
procedure ScaleVector(var v: TAffineVector; const factor: TAffineVector);
begin
  v[0] := v[0] * factor[0];
  v[1] := v[1] * factor[1];
  v[2] := v[2] * factor[2];
end;

// ScaleVector (hmg vector)
//
procedure ScaleVector(var v: TVector; const factor: TVector);
begin
  v[0] := v[0] * factor[0];
  v[1] := v[1] * factor[1];
  v[2] := v[2] * factor[2];
  v[3] := v[3] * factor[3];
end;

// VectorScale (2f)
//
function VectorScale(const v: TVector2f; factor: single): TVector2f;
begin
  result[0] := v[0] * factor;
  result[1] := v[1] * factor;
end;

// VectorScale (affine)
//
function VectorScale(const v: TAffineVector; factor: single): TAffineVector;
begin
  result[0] := v[0] * factor;
  result[1] := v[1] * factor;
  result[2] := v[2] * factor;
end;

// VectorScale (proc, affine)
//
procedure VectorScale(const v: TAffineVector; factor: single; var vr: TAffineVector);
begin
  vr[0] := v[0] * factor;
  vr[1] := v[1] * factor;
  vr[2] := v[2] * factor;
end;

// VectorScale (hmg)
//
function VectorScale(const v: TVector; factor: single): TVector;
begin
  result[0] := v[0] * factor;
  result[1] := v[1] * factor;
  result[2] := v[2] * factor;
  result[3] := v[3] * factor;
end;

// VectorScale (proc, hmg)
//
procedure VectorScale(const v: TVector; factor: single; var vr: TVector);
begin
  vr[0] := v[0] * factor;
  vr[1] := v[1] * factor;
  vr[2] := v[2] * factor;
  vr[3] := v[3] * factor;
end;

// VectorScale (proc, hmg-affine)
//
procedure VectorScale(const v: TVector; factor: single; var vr: TAffineVector);
begin
  vr[0] := v[0] * factor;
  vr[1] := v[1] * factor;
  vr[2] := v[2] * factor;
end;

// VectorScale (func, affine)
//
function VectorScale(const v: TAffineVector; const factor: TAffineVector): TAffineVector;
begin
  result[0] := v[0] * factor[0];
  result[1] := v[1] * factor[1];
  result[2] := v[2] * factor[2];
end;

// VectorScale (func, hmg)
//
function VectorScale(const v: TVector; const factor: TVector): TVector;
begin
  result[0] := v[0] * factor[0];
  result[1] := v[1] * factor[1];
  result[2] := v[2] * factor[2];
  result[3] := v[3] * factor[3];
end;

// DivideVector
//
procedure DivideVector(var v: TVector; const divider: TVector);
begin
  v[0] := v[0] / divider[0];
  v[1] := v[1] / divider[1];
  v[2] := v[2] / divider[2];
  v[3] := v[3] / divider[3];
end;

// DivideVector
//
procedure DivideVector(var v: TAffineVector; const divider: TAffineVector); overload;
begin
  v[0] := v[0] / divider[0];
  v[1] := v[1] / divider[1];
  v[2] := v[2] / divider[2];
end;

// VectorDivide
//
function VectorDivide(const v: TVector; const divider: TVector): TVector; overload;
begin
  result[0] := v[0] / divider[0];
  result[1] := v[1] / divider[1];
  result[2] := v[2] / divider[2];
  result[3] := v[3] / divider[3];
end;

// VectorDivide
//
function VectorDivide(const v: TAffineVector; const divider: TAffineVector): TAffineVector; overload;
begin
  result[0] := v[0] / divider[0];
  result[1] := v[1] / divider[1];
  result[2] := v[2] / divider[2];
end;

// TexpointEquals
//
function TexpointEquals(const p1, p2: TTexPoint): Boolean;
begin
  result := (p1.S = p2.S) and (p1.T = p2.T);
end;

// VectorEquals (hmg vector)
//
function VectorEquals(const V1, V2: TVector): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]) and (V1[3] = V2[3]);
end;

// VectorEquals (affine vector)
//
function VectorEquals(const V1, V2: TAffineVector): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]);
end;

// AffineVectorEquals (hmg vector)
//
function AffineVectorEquals(const V1, V2: TVector): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]);
end;

// VectorIsNull (hmg)
//
function VectorIsNull(const v: TVector): Boolean;
begin
  result := ((v[0] = 0) and (v[1] = 0) and (v[2] = 0));
end;

// VectorIsNull (affine)
//
function VectorIsNull(const v: TAffineVector): Boolean; overload;
begin
  result := ((v[0] = 0) and (v[1] = 0) and (v[2] = 0));
end;

// VectorSpacing (texpoint)
//
function VectorSpacing(const V1, V2: TTexPoint): single; overload;
begin
  result := Abs(V2.S - V1.S) + Abs(V2.T - V1.T);
end;

// VectorSpacing (affine)
//
function VectorSpacing(const V1, V2: TAffineVector): single;
begin
  result := Abs(V2[0] - V1[0]) + Abs(V2[1] - V1[1]) + Abs(V2[2] - V1[2]);
end;

// VectorSpacing (Hmg)
//
function VectorSpacing(const V1, V2: TVector): single;
begin
  result := Abs(V2[0] - V1[0]) + Abs(V2[1] - V1[1]) + Abs(V2[2] - V1[2]) + Abs(V2[3] - V1[3]);
end;

// VectorDistance (affine)
//
function VectorDistance(const V1, V2: TAffineVector): single;
begin
  result := Sqrt(Sqr(V2[0] - V1[0]) + Sqr(V2[1] - V1[1]) + Sqr(V2[2] - V1[2]));
end;

// VectorDistance (hmg)
//
function VectorDistance(const V1, V2: TVector): single;
begin
  result := Sqrt(Sqr(V2[0] - V1[0]) + Sqr(V2[1] - V1[1]) + Sqr(V2[2] - V1[2]));
end;

// VectorDistance2 (affine)
//
function VectorDistance2(const V1, V2: TAffineVector): single;
begin
  result := Sqr(V2[0] - V1[0]) + Sqr(V2[1] - V1[1]) + Sqr(V2[2] - V1[2]);
end;

// VectorDistance2 (hmg)
//
function VectorDistance2(const V1, V2: TVector): single;
begin
  result := Sqr(V2[0] - V1[0]) + Sqr(V2[1] - V1[1]) + Sqr(V2[2] - V1[2]);
end;

// VectorPerpendicular
//
function VectorPerpendicular(const v, n: TAffineVector): TAffineVector;
var
  dot: single;
begin
  dot := VectorDotProduct(v, n);
  result[x] := v[x] - dot * n[x];
  result[y] := v[y] - dot * n[y];
  result[z] := v[z] - dot * n[z];
end;

// VectorReflect
//
function VectorReflect(const v, n: TAffineVector): TAffineVector;
begin
  result := VectorCombine(v, n, 1, -2 * VectorDotProduct(v, n));
end;

// RotateVector
//
procedure RotateVector(var Vector: TVector; const axis: TAffineVector; angle: single);
var
  rotMatrix: TMatrix4f;
begin
  rotMatrix := CreateRotationMatrix(axis, angle);
  Vector := VectorTransform(Vector, rotMatrix);
end;

// RotateVector
//
procedure RotateVector(var Vector: TVector; const axis: TVector; angle: single); overload;
var
  rotMatrix: TMatrix4f;
begin
  rotMatrix := CreateRotationMatrix(PAffineVector(@axis)^, angle);
  Vector := VectorTransform(Vector, rotMatrix);
end;

// RotateVectorAroundY
//
procedure RotateVectorAroundY(var v: TAffineVector; alpha: single);
var
  c, S, v0: single;
begin
  GeometryLib.SinCos(alpha, S, c);
  v0 := v[0];
  v[0] := c * v0 + S * v[2];
  v[2] := c * v[2] - S * v0;
end;

// VectorRotateAroundX (func)
//
function VectorRotateAroundX(const v: TAffineVector; alpha: single): TAffineVector;
var
  c, S: single;
begin
  GeometryLib.SinCos(alpha, S, c);
  result[0] := v[0];
  result[1] := c * v[1] + S * v[2];
  result[2] := c * v[2] - S * v[1];
end;

// VectorRotateAroundY (func)
//
function VectorRotateAroundY(const v: TAffineVector; alpha: single): TAffineVector;
var
  c, S: single;
begin
  GeometryLib.SinCos(alpha, S, c);
  result[1] := v[1];
  result[0] := c * v[0] + S * v[2];
  result[2] := c * v[2] - S * v[0];
end;

// VectorRotateAroundY (proc)
//
procedure VectorRotateAroundY(const v: TAffineVector; alpha: single; var vr: TAffineVector);
var
  c, S: single;
begin
  GeometryLib.SinCos(alpha, S, c);
  vr[1] := v[1];
  vr[0] := c * v[0] + S * v[2];
  vr[2] := c * v[2] - S * v[0];
end;

// VectorRotateAroundZ (func)
//
function VectorRotateAroundZ(const v: TAffineVector; alpha: single): TAffineVector;
var
  c, S: single;
begin
  GeometryLib.SinCos(alpha, S, c);
  result[0] := c * v[0] + S * v[1];
  result[1] := c * v[1] - S * v[0];
  result[2] := v[2];
end;

// AbsVector (hmg)
//
procedure AbsVector(var v: TVector);
begin
  v[0] := Abs(v[0]);
  v[1] := Abs(v[1]);
  v[2] := Abs(v[2]);
  v[3] := Abs(v[3]);
end;

// AbsVector (affine)
//
procedure AbsVector(var v: TAffineVector);
begin
  v[0] := Abs(v[0]);
  v[1] := Abs(v[1]);
  v[2] := Abs(v[2]);
end;

// VectorAbs (hmg)
//
function VectorAbs(const v: TVector): TVector;
begin
  result[0] := Abs(v[0]);
  result[1] := Abs(v[1]);
  result[2] := Abs(v[2]);
  result[3] := Abs(v[3]);
end;

// VectorAbs (affine)
//
function VectorAbs(const v: TAffineVector): TAffineVector;
begin
  result[0] := Abs(v[0]);
  result[1] := Abs(v[1]);
  result[2] := Abs(v[2]);
end;

// IsColinear (2f)
//
function IsColinear(const V1, V2: TVector2f): Boolean; overload;
var
  a, b, c: single;
begin
  a := VectorDotProduct(V1, V1);
  b := VectorDotProduct(V1, V2);
  c := VectorDotProduct(V2, V2);
  result := (a * c - b * b) < cColinearBias;
end;

// IsColinear (affine)
//
function IsColinear(const V1, V2: TAffineVector): Boolean; overload;
var
  a, b, c: single;
begin
  a := VectorDotProduct(V1, V1);
  b := VectorDotProduct(V1, V2);
  c := VectorDotProduct(V2, V2);
  result := (a * c - b * b) < cColinearBias;
end;

// IsColinear (hmg)
//
function IsColinear(const V1, V2: TVector): Boolean; overload;
var
  a, b, c: single;
begin
  a := VectorDotProduct(V1, V1);
  b := VectorDotProduct(V1, V2);
  c := VectorDotProduct(V2, V2);
  result := (a * c - b * b) < cColinearBias;
end;

// SetMatrix (single->double)
//
procedure SetMatrix(var dest: THomogeneousDblMatrix; const src: TMatrix);
var
  i: Integer;
begin
  for i := x to w do begin
      dest[i, x] := src[i, x];
      dest[i, y] := src[i, y];
      dest[i, z] := src[i, z];
      dest[i, w] := src[i, w];
    end;
end;

// SetMatrix (hmg->affine)
//
procedure SetMatrix(var dest: TAffineMatrix; const src: TMatrix);
begin
  dest[0, 0] := src[0, 0];
  dest[0, 1] := src[0, 1];
  dest[0, 2] := src[0, 2];
  dest[1, 0] := src[1, 0];
  dest[1, 1] := src[1, 1];
  dest[1, 2] := src[1, 2];
  dest[2, 0] := src[2, 0];
  dest[2, 1] := src[2, 1];
  dest[2, 2] := src[2, 2];
end;

// SetMatrix (affine->hmg)
//
procedure SetMatrix(var dest: TMatrix; const src: TAffineMatrix);
begin
  dest[0, 0] := src[0, 0];
  dest[0, 1] := src[0, 1];
  dest[0, 2] := src[0, 2];
  dest[0, 3] := 0;
  dest[1, 0] := src[1, 0];
  dest[1, 1] := src[1, 1];
  dest[1, 2] := src[1, 2];
  dest[1, 3] := 0;
  dest[2, 0] := src[2, 0];
  dest[2, 1] := src[2, 1];
  dest[2, 2] := src[2, 2];
  dest[2, 3] := 0;
  dest[3, 0] := 0;
  dest[3, 1] := 0;
  dest[3, 2] := 0;
  dest[3, 3] := 1;
end;

// SetMatrixRow
//
procedure SetMatrixRow(var dest: TMatrix; rowNb: Integer; const aRow: TVector);
begin
  dest[0, rowNb] := aRow[0];
  dest[1, rowNb] := aRow[1];
  dest[2, rowNb] := aRow[2];
  dest[3, rowNb] := aRow[3];
end;

// CreateScaleMatrix (affine)
//
function CreateScaleMatrix(const v: TAffineVector): TMatrix;
begin
  result := IdentityHmgMatrix;
  result[x, x] := v[x];
  result[y, y] := v[y];
  result[z, z] := v[z];
end;

// CreateScaleMatrix (Hmg)
//
function CreateScaleMatrix(const v: TVector): TMatrix;
begin
  result := IdentityHmgMatrix;
  result[x, x] := v[x];
  result[y, y] := v[y];
  result[z, z] := v[z];
end;

// CreateTranslationMatrix (affine)
//
function CreateTranslationMatrix(const v: TAffineVector): TMatrix;
begin
  result := IdentityHmgMatrix;
  result[w, x] := v[x];
  result[w, y] := v[y];
  result[w, z] := v[z];
end;

// CreateTranslationMatrix (hmg)
//
function CreateTranslationMatrix(const v: TVector): TMatrix;
begin
  result := IdentityHmgMatrix;
  result[w, x] := v[x];
  result[w, y] := v[y];
  result[w, z] := v[z];
end;

// CreateScaleAndTranslationMatrix
//
function CreateScaleAndTranslationMatrix(const scale, offset: TVector): TMatrix;
begin
  result := IdentityHmgMatrix;
  result[x, x] := scale[x];
  result[w, x] := offset[x];
  result[y, y] := scale[y];
  result[w, y] := offset[y];
  result[z, z] := scale[z];
  result[w, z] := offset[z];
end;

// CreateRotationMatrixX
//
function CreateRotationMatrixX(const sine, cosine: single): TMatrix;
begin
  result := EmptyHmgMatrix;
  result[x, x] := 1;
  result[y, y] := cosine;
  result[y, z] := sine;
  result[z, y] := -sine;
  result[z, z] := cosine;
  result[w, w] := 1;
end;

// CreateRotationMatrixX
//
function CreateRotationMatrixX(const angle: single): TMatrix;
var
  S, c: single;
begin
  GeometryLib.SinCos(angle, S, c);
  result := CreateRotationMatrixX(S, c);
end;

// CreateRotationMatrixY
//
function CreateRotationMatrixY(const sine, cosine: single): TMatrix;
begin
  result := EmptyHmgMatrix;
  result[x, x] := cosine;
  result[x, z] := -sine;
  result[y, y] := 1;
  result[z, x] := sine;
  result[z, z] := cosine;
  result[w, w] := 1;
end;

// CreateRotationMatrixY
//
function CreateRotationMatrixY(const angle: single): TMatrix;
var
  S, c: single;
begin
  GeometryLib.SinCos(angle, S, c);
  result := CreateRotationMatrixY(S, c);
end;

// CreateRotationMatrixZ
//
function CreateRotationMatrixZ(const sine, cosine: single): TMatrix;
begin
  result := EmptyHmgMatrix;
  result[x, x] := cosine;
  result[x, y] := sine;
  result[y, x] := -sine;
  result[y, y] := cosine;
  result[z, z] := 1;
  result[w, w] := 1;
end;

// CreateRotationMatrixZ
//
function CreateRotationMatrixZ(const angle: single): TMatrix;
var
  S, c: single;
begin
  GeometryLib.SinCos(angle, S, c);
  result := CreateRotationMatrixZ(S, c);
end;

// CreateRotationMatrix (affine)
//
function CreateRotationMatrix(const anAxis: TAffineVector; angle: single): TMatrix;
var
  axis                          : TAffineVector;
  cosine, sine, one_minus_cosine: single;
begin
  GeometryLib.SinCos(angle, sine, cosine);
  one_minus_cosine := 1 - cosine;
  axis := VectorNormalize(anAxis);

  result[x, x] := (one_minus_cosine * axis[0] * axis[0]) + cosine;
  result[x, y] := (one_minus_cosine * axis[0] * axis[1]) - (axis[2] * sine);
  result[x, z] := (one_minus_cosine * axis[2] * axis[0]) + (axis[1] * sine);
  result[x, w] := 0;

  result[y, x] := (one_minus_cosine * axis[0] * axis[1]) + (axis[2] * sine);
  result[y, y] := (one_minus_cosine * axis[1] * axis[1]) + cosine;
  result[y, z] := (one_minus_cosine * axis[1] * axis[2]) - (axis[0] * sine);
  result[y, w] := 0;

  result[z, x] := (one_minus_cosine * axis[2] * axis[0]) - (axis[1] * sine);
  result[z, y] := (one_minus_cosine * axis[1] * axis[2]) + (axis[0] * sine);
  result[z, z] := (one_minus_cosine * axis[2] * axis[2]) + cosine;
  result[z, w] := 0;

  result[w, x] := 0;
  result[w, y] := 0;
  result[w, z] := 0;
  result[w, w] := 1;
end;

// CreateRotationMatrix (hmg)
//
function CreateRotationMatrix(const anAxis: TVector; angle: single): TMatrix;
begin
  result := CreateRotationMatrix(PAffineVector(@anAxis)^, angle);
end;

// CreateAffineRotationMatrix
//
function CreateAffineRotationMatrix(const anAxis: TAffineVector; angle: single): TAffineMatrix;
var
  axis                          : TAffineVector;
  cosine, sine, one_minus_cosine: single;
begin
  GeometryLib.SinCos(angle, sine, cosine);
  one_minus_cosine := 1 - cosine;
  axis := VectorNormalize(anAxis);

  result[x, x] := (one_minus_cosine * Sqr(axis[0])) + cosine;
  result[x, y] := (one_minus_cosine * axis[0] * axis[1]) - (axis[2] * sine);
  result[x, z] := (one_minus_cosine * axis[2] * axis[0]) + (axis[1] * sine);

  result[y, x] := (one_minus_cosine * axis[0] * axis[1]) + (axis[2] * sine);
  result[y, y] := (one_minus_cosine * Sqr(axis[1])) + cosine;
  result[y, z] := (one_minus_cosine * axis[1] * axis[2]) - (axis[0] * sine);

  result[z, x] := (one_minus_cosine * axis[2] * axis[0]) - (axis[1] * sine);
  result[z, y] := (one_minus_cosine * axis[1] * axis[2]) + (axis[0] * sine);
  result[z, z] := (one_minus_cosine * Sqr(axis[2])) + cosine;
end;

// MatrixMultiply (3x3 func)
//
function MatrixMultiply(const m1, m2: TAffineMatrix): TAffineMatrix;
begin
  result[x, x] := m1[x, x] * m2[x, x] + m1[x, y] * m2[y, x] + m1[x, z] * m2[z, x];
  result[x, y] := m1[x, x] * m2[x, y] + m1[x, y] * m2[y, y] + m1[x, z] * m2[z, y];
  result[x, z] := m1[x, x] * m2[x, z] + m1[x, y] * m2[y, z] + m1[x, z] * m2[z, z];
  result[y, x] := m1[y, x] * m2[x, x] + m1[y, y] * m2[y, x] + m1[y, z] * m2[z, x];
  result[y, y] := m1[y, x] * m2[x, y] + m1[y, y] * m2[y, y] + m1[y, z] * m2[z, y];
  result[y, z] := m1[y, x] * m2[x, z] + m1[y, y] * m2[y, z] + m1[y, z] * m2[z, z];
  result[z, x] := m1[z, x] * m2[x, x] + m1[z, y] * m2[y, x] + m1[z, z] * m2[z, x];
  result[z, y] := m1[z, x] * m2[x, y] + m1[z, y] * m2[y, y] + m1[z, z] * m2[z, y];
  result[z, z] := m1[z, x] * m2[x, z] + m1[z, y] * m2[y, z] + m1[z, z] * m2[z, z];
end;

// MatrixMultiply (4x4, func)
//
function MatrixMultiply(const m1, m2: TMatrix): TMatrix;
begin
  result[x, x] := m1[x, x] * m2[x, x] + m1[x, y] * m2[y, x] + m1[x, z] * m2[z, x] + m1[x, w] * m2[w, x];
  result[x, y] := m1[x, x] * m2[x, y] + m1[x, y] * m2[y, y] + m1[x, z] * m2[z, y] + m1[x, w] * m2[w, y];
  result[x, z] := m1[x, x] * m2[x, z] + m1[x, y] * m2[y, z] + m1[x, z] * m2[z, z] + m1[x, w] * m2[w, z];
  result[x, w] := m1[x, x] * m2[x, w] + m1[x, y] * m2[y, w] + m1[x, z] * m2[z, w] + m1[x, w] * m2[w, w];
  result[y, x] := m1[y, x] * m2[x, x] + m1[y, y] * m2[y, x] + m1[y, z] * m2[z, x] + m1[y, w] * m2[w, x];
  result[y, y] := m1[y, x] * m2[x, y] + m1[y, y] * m2[y, y] + m1[y, z] * m2[z, y] + m1[y, w] * m2[w, y];
  result[y, z] := m1[y, x] * m2[x, z] + m1[y, y] * m2[y, z] + m1[y, z] * m2[z, z] + m1[y, w] * m2[w, z];
  result[y, w] := m1[y, x] * m2[x, w] + m1[y, y] * m2[y, w] + m1[y, z] * m2[z, w] + m1[y, w] * m2[w, w];
  result[z, x] := m1[z, x] * m2[x, x] + m1[z, y] * m2[y, x] + m1[z, z] * m2[z, x] + m1[z, w] * m2[w, x];
  result[z, y] := m1[z, x] * m2[x, y] + m1[z, y] * m2[y, y] + m1[z, z] * m2[z, y] + m1[z, w] * m2[w, y];
  result[z, z] := m1[z, x] * m2[x, z] + m1[z, y] * m2[y, z] + m1[z, z] * m2[z, z] + m1[z, w] * m2[w, z];
  result[z, w] := m1[z, x] * m2[x, w] + m1[z, y] * m2[y, w] + m1[z, z] * m2[z, w] + m1[z, w] * m2[w, w];
  result[w, x] := m1[w, x] * m2[x, x] + m1[w, y] * m2[y, x] + m1[w, z] * m2[z, x] + m1[w, w] * m2[w, x];
  result[w, y] := m1[w, x] * m2[x, y] + m1[w, y] * m2[y, y] + m1[w, z] * m2[z, y] + m1[w, w] * m2[w, y];
  result[w, z] := m1[w, x] * m2[x, z] + m1[w, y] * m2[y, z] + m1[w, z] * m2[z, z] + m1[w, w] * m2[w, z];
  result[w, w] := m1[w, x] * m2[x, w] + m1[w, y] * m2[y, w] + m1[w, z] * m2[z, w] + m1[w, w] * m2[w, w];
end;

// MatrixMultiply (4x4, proc)
//
procedure MatrixMultiply(const m1, m2: TMatrix; var MResult: TMatrix);
begin
  MResult := MatrixMultiply(m1, m2);
end;

// VectorTransform
//
function VectorTransform(const v: TVector; const M: TMatrix): TVector;
begin
  result[x] := v[x] * M[x, x] + v[y] * M[y, x] + v[z] * M[z, x] + v[w] * M[w, x];
  result[y] := v[x] * M[x, y] + v[y] * M[y, y] + v[z] * M[z, y] + v[w] * M[w, y];
  result[z] := v[x] * M[x, z] + v[y] * M[y, z] + v[z] * M[z, z] + v[w] * M[w, z];
  result[w] := v[x] * M[x, w] + v[y] * M[y, w] + v[z] * M[z, w] + v[w] * M[w, w];
end;

// VectorTransform
//
function VectorTransform(const v: TVector; const M: TAffineMatrix): TVector;
begin
  result[x] := v[x] * M[x, x] + v[y] * M[y, x] + v[z] * M[z, x];
  result[y] := v[x] * M[x, y] + v[y] * M[y, y] + v[z] * M[z, y];
  result[z] := v[x] * M[x, z] + v[y] * M[y, z] + v[z] * M[z, z];
  result[w] := v[w];
end;

// VectorTransform
//
function VectorTransform(const v: TAffineVector; const M: TMatrix): TAffineVector;
begin
  result[x] := v[x] * M[x, x] + v[y] * M[y, x] + v[z] * M[z, x] + M[w, x];
  result[y] := v[x] * M[x, y] + v[y] * M[y, y] + v[z] * M[z, y] + M[w, y];
  result[z] := v[x] * M[x, z] + v[y] * M[y, z] + v[z] * M[z, z] + M[w, z];
end;

// VectorTransform
//
function VectorTransform(const v: TAffineVector; const M: TAffineMatrix): TAffineVector;
begin
  result[x] := v[x] * M[x, x] + v[y] * M[y, x] + v[z] * M[z, x];
  result[y] := v[x] * M[x, y] + v[y] * M[y, y] + v[z] * M[z, y];
  result[z] := v[x] * M[x, z] + v[y] * M[y, z] + v[z] * M[z, z];
end;

// MatrixDeterminant (affine)
//
function MatrixDeterminant(const M: TAffineMatrix): single;
begin
  result := M[x, x] * (M[y, y] * M[z, z] - M[z, y] * M[y, z])
    - M[x, y] * (M[y, x] * M[z, z] - M[z, x] * M[y, z])
    + M[x, z] * (M[y, x] * M[z, y] - M[z, x] * M[y, y]);
end;

// MatrixDetInternal
//
function MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: single): single;
// internal version for the determinant of a 3x3 matrix
begin
  result := a1 * (b2 * c3 - b3 * c2)
    - b1 * (a2 * c3 - a3 * c2)
    + c1 * (a2 * b3 - a3 * b2);
end;

// MatrixDeterminant (hmg)
//
function MatrixDeterminant(const M: TMatrix): single;
begin
  result := M[x, x] * MatrixDetInternal(M[y, y], M[z, y], M[w, y], M[y, z], M[z, z], M[w, z], M[y, w], M[z, w], M[w, w])
    - M[x, y] * MatrixDetInternal(M[y, x], M[z, x], M[w, x], M[y, z], M[z, z], M[w, z], M[y, w], M[z, w], M[w, w])
    + M[x, z] * MatrixDetInternal(M[y, x], M[z, x], M[w, x], M[y, y], M[z, y], M[w, y], M[y, w], M[z, w], M[w, w])
    - M[x, w] * MatrixDetInternal(M[y, x], M[z, x], M[w, x], M[y, y], M[z, y], M[w, y], M[y, z], M[z, z], M[w, z]);
end;

// AdjointMatrix
//
procedure AdjointMatrix(var M: TMatrix);
var
  a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4: single;
begin
  a1 := M[x, x];
  b1 := M[x, y];
  c1 := M[x, z];
  d1 := M[x, w];
  a2 := M[y, x];
  b2 := M[y, y];
  c2 := M[y, z];
  d2 := M[y, w];
  a3 := M[z, x];
  b3 := M[z, y];
  c3 := M[z, z];
  d3 := M[z, w];
  a4 := M[w, x];
  b4 := M[w, y];
  c4 := M[w, z];
  d4 := M[w, w];

  // row column labeling reversed since we transpose rows & columns
  M[x, x] := MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
  M[y, x] := -MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
  M[z, x] := MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
  M[w, x] := -MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

  M[x, y] := -MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
  M[y, y] := MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
  M[z, y] := -MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
  M[w, y] := MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

  M[x, z] := MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
  M[y, z] := -MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
  M[z, z] := MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
  M[w, z] := -MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

  M[x, w] := -MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
  M[y, w] := MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
  M[z, w] := -MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
  M[w, w] := MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

// AdjointMatrix (affine)
//
procedure AdjointMatrix(var M: TAffineMatrix);
var
  a1, a2, a3,
    b1, b2, b3,
    c1, c2, c3: single;
begin
  a1 := M[x, x];
  a2 := M[x, y];
  a3 := M[x, z];
  b1 := M[y, x];
  b2 := M[y, y];
  b3 := M[y, z];
  c1 := M[z, x];
  c2 := M[z, y];
  c3 := M[z, z];
  M[x, x] := (b2 * c3 - c2 * b3);
  M[y, x] := -(b1 * c3 - c1 * b3);
  M[z, x] := (b1 * c2 - c1 * b2);

  M[x, y] := -(a2 * c3 - c2 * a3);
  M[y, y] := (a1 * c3 - c1 * a3);
  M[z, y] := -(a1 * c2 - c1 * a2);

  M[x, z] := (a2 * b3 - b2 * a3);
  M[y, z] := -(a1 * b3 - b1 * a3);
  M[z, z] := (a1 * b2 - b1 * a2);
end;

// ScaleMatrix (affine)
//
procedure ScaleMatrix(var M: TAffineMatrix; const factor: single);
var
  i: Integer;
begin
  for i := 0 to 2 do begin
      M[i, 0] := M[i, 0] * factor;
      M[i, 1] := M[i, 1] * factor;
      M[i, 2] := M[i, 2] * factor;
    end;
end;

// ScaleMatrix (hmg)
//
procedure ScaleMatrix(var M: TMatrix; const factor: single);
var
  i: Integer;
begin
  for i := 0 to 3 do begin
      M[i, 0] := M[i, 0] * factor;
      M[i, 1] := M[i, 1] * factor;
      M[i, 2] := M[i, 2] * factor;
      M[i, 3] := M[i, 3] * factor;
    end;
end;

// TranslateMatrix (affine vec)
//
procedure TranslateMatrix(var M: TMatrix; const v: TAffineVector);
begin
  M[3][0] := M[3][0] + v[0];
  M[3][1] := M[3][1] + v[1];
  M[3][2] := M[3][2] + v[2];
end;

// TranslateMatrix
//
procedure TranslateMatrix(var M: TMatrix; const v: TVector);
begin
  M[3][0] := M[3][0] + v[0];
  M[3][1] := M[3][1] + v[1];
  M[3][2] := M[3][2] + v[2];
end;

// NormalizeMatrix
//
procedure NormalizeMatrix(var M: TMatrix);
begin
  M[0][3] := 0;
  NormalizeVector(M[0]);
  M[1][3] := 0;
  NormalizeVector(M[1]);
  M[2] := VectorCrossProduct(M[0], M[1]);
  M[0] := VectorCrossProduct(M[1], M[2]);
  M[3] := WHmgVector;
end;

// TransposeMatrix
//
procedure TransposeMatrix(var M: TAffineMatrix);
var
  f: single;
begin
  f := M[0, 1];
  M[0, 1] := M[1, 0];
  M[1, 0] := f;
  f := M[0, 2];
  M[0, 2] := M[2, 0];
  M[2, 0] := f;
  f := M[1, 2];
  M[1, 2] := M[2, 1];
  M[2, 1] := f;
end;

// TransposeMatrix
//
procedure TransposeMatrix(var M: TMatrix);
var
  f: single;
begin
  f := M[0, 1];
  M[0, 1] := M[1, 0];
  M[1, 0] := f;
  f := M[0, 2];
  M[0, 2] := M[2, 0];
  M[2, 0] := f;
  f := M[0, 3];
  M[0, 3] := M[3, 0];
  M[3, 0] := f;
  f := M[1, 2];
  M[1, 2] := M[2, 1];
  M[2, 1] := f;
  f := M[1, 3];
  M[1, 3] := M[3, 1];
  M[3, 1] := f;
  f := M[2, 3];
  M[2, 3] := M[3, 2];
  M[3, 2] := f;
end;

// InvertMatrix
//
procedure InvertMatrix(var M: TMatrix);
var
  det: single;
begin
  det := MatrixDeterminant(M);
  if Abs(det) < EPSILON then
      M := IdentityHmgMatrix
  else begin
      AdjointMatrix(M);
      ScaleMatrix(M, 1 / det);
    end;
end;

// MatrixInvert
//
function MatrixInvert(const M: TMatrix): TMatrix;
begin
  result := M;
  InvertMatrix(result);
end;

// InvertMatrix (affine)
//
procedure InvertMatrix(var M: TAffineMatrix);
var
  det: single;
begin
  det := MatrixDeterminant(M);
  if Abs(det) < EPSILON then
      M := IdentityMatrix
  else begin
      AdjointMatrix(M);
      ScaleMatrix(M, 1 / det);
    end;
end;

// MatrixInvert (affine)
//
function MatrixInvert(const M: TAffineMatrix): TAffineMatrix;
begin
  result := M;
  InvertMatrix(result);
end;

// transpose_scale_m33
//
procedure transpose_scale_m33(const src: TMatrix; var dest: TMatrix; var scale: single);
begin
  dest[0][0] := scale * src[0][0];
  dest[1][0] := scale * src[0][1];
  dest[2][0] := scale * src[0][2];
  dest[0][1] := scale * src[1][0];
  dest[1][1] := scale * src[1][1];
  dest[2][1] := scale * src[1][2];
  dest[0][2] := scale * src[2][0];
  dest[1][2] := scale * src[2][1];
  dest[2][2] := scale * src[2][2];
end;

// AnglePreservingMatrixInvert
//
function AnglePreservingMatrixInvert(const mat: TMatrix): TMatrix;
var
  scale: single;
begin
  scale := VectorNorm(mat[0]);

  // Is the submatrix A singular?
  if Abs(scale) < EPSILON then begin
      // Matrix M has no inverse
      result := IdentityHmgMatrix;
      Exit;
    end
  else begin
      // Calculate the inverse of the square of the isotropic scale factor
      scale := 1.0 / scale;
    end;

  // Fill in last row while CPU is busy with the division
  result[0][3] := 0.0;
  result[1][3] := 0.0;
  result[2][3] := 0.0;
  result[3][3] := 1.0;

  // Transpose and scale the 3 by 3 upper-left submatrix
  transpose_scale_m33(mat, result, scale);

  // Calculate -(transpose(A) / s*s) C
  result[3][0] := -(result[0][0] * mat[3][0]
    + result[1][0] * mat[3][1]
    + result[2][0] * mat[3][2]);
  result[3][1] := -(result[0][1] * mat[3][0]
    + result[1][1] * mat[3][1]
    + result[2][1] * mat[3][2]);
  result[3][2] := -(result[0][2] * mat[3][0]
    + result[1][2] * mat[3][1]
    + result[2][2] * mat[3][2]);
end;

// MatrixDecompose
//
function MatrixDecompose(const M: TMatrix; var Tran: TTransformations): Boolean;
var
  i, J                 : Integer;
  LocMat, pmat, invpmat: TMatrix;
  prhs, psol           : TVector;
  row0, row1, row2     : TAffineVector;
  f                    : single;
begin
  result := False;
  LocMat := M;
  // normalize the matrix
  if LocMat[w, w] = 0 then
      Exit;
  for i := 0 to 3 do
    for J := 0 to 3 do
        LocMat[i, J] := LocMat[i, J] / LocMat[w, w];

  // pmat is used to solve for perspective, but it also provides
  // an easy way to test for singularity of the upper 3x3 component.

  pmat := LocMat;
  for i := 0 to 2 do
      pmat[i, w] := 0;
  pmat[w, w] := 1;

  if MatrixDeterminant(pmat) = 0 then
      Exit;

  // First, isolate perspective.  This is the messiest.
  if (LocMat[x, w] <> 0) or (LocMat[y, w] <> 0) or (LocMat[z, w] <> 0) then begin
      // prhs is the right hand side of the equation.
      prhs[x] := LocMat[x, w];
      prhs[y] := LocMat[y, w];
      prhs[z] := LocMat[z, w];
      prhs[w] := LocMat[w, w];

      // Solve the equation by inverting pmat and multiplying
      // prhs by the inverse.  (This is the easiest way, not
      // necessarily the best.)

      invpmat := pmat;
      InvertMatrix(invpmat);
      TransposeMatrix(invpmat);
      psol := VectorTransform(prhs, invpmat);

      // stuff the answer away
      Tran[ttPerspectiveX] := psol[x];
      Tran[ttPerspectiveY] := psol[y];
      Tran[ttPerspectiveZ] := psol[z];
      Tran[ttPerspectiveW] := psol[w];

      // clear the perspective partition
      LocMat[x, w] := 0;
      LocMat[y, w] := 0;
      LocMat[z, w] := 0;
      LocMat[w, w] := 1;
    end
  else begin
      // no perspective
      Tran[ttPerspectiveX] := 0;
      Tran[ttPerspectiveY] := 0;
      Tran[ttPerspectiveZ] := 0;
      Tran[ttPerspectiveW] := 0;
    end;

  // next take care of translation (easy)
  for i := 0 to 2 do begin
      Tran[TTransType(Ord(ttTranslateX) + i)] := LocMat[w, i];
      LocMat[w, i] := 0;
    end;

  // now get scale and shear
  SetVector(row0, LocMat[0]);
  SetVector(row1, LocMat[1]);
  SetVector(row2, LocMat[2]);

  // compute X scale factor and normalize first row
  Tran[ttScaleX] := VectorNorm(row0);
  ScaleVector(row0, RSqrt(Tran[ttScaleX]));

  // compute XY shear factor and make 2nd row orthogonal to 1st
  Tran[ttShearXY] := VectorDotProduct(row0, row1);
  f := -Tran[ttShearXY];
  CombineVector(row1, row0, f);

  // now, compute Y scale and normalize 2nd row
  Tran[ttScaleY] := VectorNorm(row1);
  ScaleVector(row1, RSqrt(Tran[ttScaleY]));
  Tran[ttShearXY] := Tran[ttShearXY] / Tran[ttScaleY];

  // compute XZ and YZ shears, orthogonalize 3rd row
  Tran[ttShearXZ] := VectorDotProduct(row0, row2);
  f := -Tran[ttShearXZ];
  CombineVector(row2, row0, f);
  Tran[ttShearYZ] := VectorDotProduct(row1, row2);
  f := -Tran[ttShearYZ];
  CombineVector(row2, row1, f);

  // next, get Z scale and normalize 3rd row
  Tran[ttScaleZ] := VectorNorm(row2);
  ScaleVector(row2, RSqrt(Tran[ttScaleZ]));
  Tran[ttShearXZ] := Tran[ttShearXZ] / Tran[ttScaleZ];
  Tran[ttShearYZ] := Tran[ttShearYZ] / Tran[ttScaleZ];

  // At this point, the matrix (in rows[]) is orthonormal.
  // Check for a coordinate system flip.  If the determinant
  // is -1, then negate the matrix and the scaling factors.
  if VectorDotProduct(row0, VectorCrossProduct(row1, row2)) < 0 then begin
      for i := 0 to 2 do
          Tran[TTransType(Ord(ttScaleX) + i)] := -Tran[TTransType(Ord(ttScaleX) + i)];
      NegateVector(row0);
      NegateVector(row1);
      NegateVector(row2);
    end;

  // now, get the rotations out, as described in the gem
  Tran[ttRotateY] := GeometryLib.ArcSin(-row0[z]);
  if Cos(Tran[ttRotateY]) <> 0 then begin
      Tran[ttRotateX] := GeometryLib.ArcTan2(row1[z], row2[z]);
      Tran[ttRotateZ] := GeometryLib.ArcTan2(row0[y], row0[x]);
    end
  else begin
      Tran[ttRotateX] := GeometryLib.ArcTan2(row1[x], row1[y]);
      Tran[ttRotateZ] := 0;
    end;
  // All done!
  result := True;
end;

function CreateLookAtMatrix(const eye, center, normUp: TVector): TMatrix;
var
  XAxis, YAxis, ZAxis, negEye: TVector;
begin
  ZAxis := VectorSubtract(center, eye);
  NormalizeVector(ZAxis);
  XAxis := VectorCrossProduct(ZAxis, normUp);
  NormalizeVector(XAxis);
  YAxis := VectorCrossProduct(XAxis, ZAxis);
  result[0] := XAxis;
  result[1] := YAxis;
  result[2] := ZAxis;
  NegateVector(result[2]);
  result[3] := NullHmgPoint;
  TransposeMatrix(result);
  negEye := eye;
  NegateVector(negEye);
  negEye[3] := 1;
  negEye := VectorTransform(negEye, result);
  result[3] := negEye;
end;

function CreateMatrixFromFrustum(Left, Right, Bottom, Top, ZNear, ZFar: single): TMatrix;
begin
  result[0][0] := 2 * ZNear / (Right - Left);
  result[0][1] := 0;
  result[0][2] := 0;
  result[0][3] := 0;

  result[1][0] := 0;
  result[1][1] := 2 * ZNear / (Top - Bottom);
  result[1][2] := 0;
  result[1][3] := 0;

  result[2][0] := (Right + Left) / (Right - Left);
  result[2][1] := (Top + Bottom) / (Top - Bottom);
  result[2][2] := -(ZFar + ZNear) / (ZFar - ZNear);
  result[2][3] := -1;

  result[3][0] := 0;
  result[3][1] := 0;
  result[3][2] := -2 * ZFar * ZNear / (ZFar - ZNear);
  result[3][3] := 0;
end;

function CreatePerspectiveMatrix(FOV, Aspect, ZNear, ZFar: single): TMatrix;
var
  x, y: single;
begin
  FOV := MinFloat(179.9, MaxFloat(0, FOV));
  y := ZNear * GeometryLib.Tan(GeometryLib.DegToRad(FOV) * 0.5);
  x := y * Aspect;
  result := CreateMatrixFromFrustum(-x, x, -y, y, ZNear, ZFar);
end;

function CreateOrthoMatrix(Left, Right, Bottom, Top, ZNear, ZFar: single): TMatrix;
begin
  result[0][0] := 2 / (Right - Left);
  result[0][1] := 0;
  result[0][2] := 0;
  result[0][3] := 0;

  result[1][0] := 0;
  result[1][1] := 2 / (Top - Bottom);
  result[1][2] := 0;
  result[1][3] := 0;

  result[2][0] := 0;
  result[2][1] := 0;
  result[2][2] := -2 / (ZFar - ZNear);
  result[2][3] := 0;

  result[3][0] := (Left + Right) / (Left - Right);
  result[3][1] := (Bottom + Top) / (Bottom - Top);
  result[3][2] := (ZNear + ZFar) / (ZNear - ZFar);
  result[3][3] := 1;
end;

function CreatePickMatrix(x, y, deltax, deltay: single; const viewport: TVector4i): TMatrix;
begin
  if (deltax <= 0) or (deltay <= 0) then
    begin
      result := IdentityHmgMatrix;
      Exit;
    end;
  // Translate and scale the picked region to the entire window
  result := CreateTranslationMatrix(AffineVectorMake(
    (viewport[2] - 2 * (x - viewport[0])) / deltax,
    (viewport[3] - 2 * (y - viewport[1])) / deltay,
    0.0));
  result[0][0] := viewport[2] / deltax;
  result[1][1] := viewport[3] / deltay;
end;

function Project(
  objectVector: TVector;
  const ViewProjMatrix: TMatrix;
  const viewport: TVector4i;
  out WindowVector: TVector): Boolean;
begin
  result := False;
  objectVector[3] := 1.0;
  WindowVector := VectorTransform(objectVector, ViewProjMatrix);
  if WindowVector[3] = 0.0 then
      Exit;
  WindowVector[0] := WindowVector[0] / WindowVector[3];
  WindowVector[1] := WindowVector[1] / WindowVector[3];
  WindowVector[2] := WindowVector[2] / WindowVector[3];
  // Map x, y and z to range 0-1
  WindowVector[0] := WindowVector[0] * 0.5 + 0.5;
  WindowVector[1] := WindowVector[1] * 0.5 + 0.5;
  WindowVector[2] := WindowVector[2] * 0.5 + 0.5;

  // Map x,y to viewport
  WindowVector[0] := WindowVector[0] * viewport[2] + viewport[0];
  WindowVector[1] := WindowVector[1] * viewport[3] + viewport[1];
  result := True;
end;

function UnProject(
  WindowVector: TVector;
  ViewProjMatrix: TMatrix;
  const viewport: TVector4i;
  out objectVector: TVector): Boolean;
begin
  result := False;
  InvertMatrix(ViewProjMatrix);
  WindowVector[3] := 1.0;
  // Map x and y from window coordinates
  WindowVector[0] := (WindowVector[0] - viewport[0]) / viewport[2];
  WindowVector[1] := (WindowVector[1] - viewport[1]) / viewport[3];
  // Map to range -1 to 1
  WindowVector[0] := WindowVector[0] * 2 - 1;
  WindowVector[1] := WindowVector[1] * 2 - 1;
  WindowVector[2] := WindowVector[2] * 2 - 1;
  objectVector := VectorTransform(WindowVector, ViewProjMatrix);
  if objectVector[3] = 0.0 then
      Exit;
  objectVector[0] := objectVector[0] / objectVector[3];
  objectVector[1] := objectVector[1] / objectVector[3];
  objectVector[2] := objectVector[2] / objectVector[3];
  result := True;
end;

// CalcPlaneNormal (func, affine)
//
function CalcPlaneNormal(const p1, p2, p3: TAffineVector): TAffineVector;
var
  V1, V2: TAffineVector;
begin
  VectorSubtract(p2, p1, V1);
  VectorSubtract(p3, p1, V2);
  VectorCrossProduct(V1, V2, result);
  NormalizeVector(result);
end;

// CalcPlaneNormal (proc, affine)
//
procedure CalcPlaneNormal(const p1, p2, p3: TAffineVector; var vr: TAffineVector);
var
  V1, V2: TAffineVector;
begin
  VectorSubtract(p2, p1, V1);
  VectorSubtract(p3, p1, V2);
  VectorCrossProduct(V1, V2, vr);
  NormalizeVector(vr);
end;

// CalcPlaneNormal (proc, hmg)
//
procedure CalcPlaneNormal(const p1, p2, p3: TVector; var vr: TAffineVector); overload;
var
  V1, V2: TVector;
begin
  VectorSubtract(p2, p1, V1);
  VectorSubtract(p3, p1, V2);
  VectorCrossProduct(V1, V2, vr);
  NormalizeVector(vr);
end;

// PlaneMake (point + normal, affine)
//
function PlaneMake(const point, normal: TAffineVector): THmgPlane;
begin
  PAffineVector(@result)^ := normal;
  result[3] := -VectorDotProduct(point, normal);
end;

// PlaneMake (point + normal, hmg)
//
function PlaneMake(const point, normal: TVector): THmgPlane;
begin
  PAffineVector(@result)^ := PAffineVector(@normal)^;
  result[3] := -VectorDotProduct(PAffineVector(@point)^, PAffineVector(@normal)^);
end;

// PlaneMake (3 points, affine)
//
function PlaneMake(const p1, p2, p3: TAffineVector): THmgPlane;
begin
  CalcPlaneNormal(p1, p2, p3, PAffineVector(@result)^);
  result[3] := -VectorDotProduct(p1, PAffineVector(@result)^);
end;

// PlaneMake (3 points, hmg)
//
function PlaneMake(const p1, p2, p3: TVector): THmgPlane;
begin
  CalcPlaneNormal(p1, p2, p3, PAffineVector(@result)^);
  result[3] := -VectorDotProduct(p1, PAffineVector(@result)^);
end;

// SetPlane
//
procedure SetPlane(var dest: TDoubleHmgPlane; const src: THmgPlane);
begin
  dest[0] := src[0];
  dest[1] := src[1];
  dest[2] := src[2];
  dest[3] := src[3];
end;

// NormalizePlane
//
procedure NormalizePlane(var plane: THmgPlane);
var
  n: single;
begin
  n := RSqrt(plane[0] * plane[0] + plane[1] * plane[1] + plane[2] * plane[2]);
  ScaleVector(plane, n);
end;

// PlaneEvaluatePoint (affine)
//
function PlaneEvaluatePoint(const plane: THmgPlane; const point: TAffineVector): single;
begin
  result := plane[0] * point[0] + plane[1] * point[1] + plane[2] * point[2] + plane[3];
end;

// PlaneEvaluatePoint (hmg)
//
function PlaneEvaluatePoint(const plane: THmgPlane; const point: TVector): single;
begin
  result := plane[0] * point[0] + plane[1] * point[1] + plane[2] * point[2] + plane[3];
end;

// PointIsInHalfSpace
//
function PointIsInHalfSpace(const point, planePoint, planeNormal: TVector): Boolean;
begin
  result := (PointPlaneDistance(point, planePoint, planeNormal) > 0); // 44
end;

// PointIsInHalfSpace
//
function PointIsInHalfSpace(const point, planePoint, planeNormal: TAffineVector): Boolean;
begin
  result := (PointPlaneDistance(point, planePoint, planeNormal) > 0);
end;

// PointIsInHalfSpace
//
function PointIsInHalfSpace(const point: TAffineVector; plane: THmgPlane): Boolean;
begin
  result := (PointPlaneDistance(point, plane) > 0);
end;

// PointPlaneDistance
//
function PointPlaneDistance(const point, planePoint, planeNormal: TVector): single;
begin
  result := (point[0] - planePoint[0]) * planeNormal[0]
    + (point[1] - planePoint[1]) * planeNormal[1]
    + (point[2] - planePoint[2]) * planeNormal[2];
end;

// PointPlaneDistance
//
function PointPlaneDistance(const point, planePoint, planeNormal: TAffineVector): single;
begin
  result := (point[0] - planePoint[0]) * planeNormal[0]
    + (point[1] - planePoint[1]) * planeNormal[1]
    + (point[2] - planePoint[2]) * planeNormal[2];
end;

// PointPlaneDistance
//
function PointPlaneDistance(const point: TAffineVector; plane: THmgPlane): single;
begin
  result := PlaneEvaluatePoint(plane, point);
end;

// PointPlaneOrthoProjection
//
function PointPlaneOrthoProjection(const point: TAffineVector; const plane: THmgPlane;
  var inter: TAffineVector; bothface: Boolean = True): Boolean;
var
  h     : single;
  normal: TAffineVector;
begin
  result := False;

  h := PointPlaneDistance(point, plane);

  if (not bothface) and (h < 0) then
      Exit;

  normal := Vector3fMake(plane);
  inter := VectorAdd(point, VectorScale(normal, -h));
  result := True;
end;

// PointPlaneProjection
//
function PointPlaneProjection(const point, direction: TAffineVector; const plane: THmgPlane;
  var inter: TAffineVector; bothface: Boolean = True): Boolean;
var
  h, dot: single;
  normal: TAffineVector;
begin
  result := False;

  normal := Vector3fMake(plane);
  dot := VectorDotProduct(VectorNormalize(direction), normal);

  if (not bothface) and (dot > 0) then
      Exit;

  if Abs(dot) >= 0.000000001 then begin
      h := PointPlaneDistance(point, plane);
      inter := VectorAdd(point, VectorScale(direction, -h / dot));
      result := True;
    end;
end;

// SegmentPlaneIntersection
//
function SegmentPlaneIntersection(const ptA, ptB: TAffineVector; const plane: THmgPlane; var inter: TAffineVector): Boolean;
var
  hA, hB, dot      : single;
  normal, direction: TVector3f;
begin
  result := False;
  hA := PointPlaneDistance(ptA, plane);
  hB := PointPlaneDistance(ptB, plane);
  if hA * hB <= 0 then
    begin
      normal := Vector3fMake(plane);
      direction := VectorNormalize(VectorSubtract(ptB, ptA));
      dot := VectorDotProduct(direction, normal);
      if Abs(dot) >= 0.000000001 then begin
          inter := VectorAdd(ptA, VectorScale(direction, -hA / dot));
          result := True;
        end;
    end;
end;

// PointTriangleOrthoProjection
//
function PointTriangleOrthoProjection(const point, ptA, ptB, ptC: TAffineVector;
  var inter: TAffineVector; bothface: Boolean = True): Boolean;
var
  plane: THmgPlane;
begin
  result := False;

  plane := PlaneMake(ptA, ptB, ptC);
  if not IsLineIntersectTriangle(point, Vector3fMake(plane), ptA, ptB, ptC) then
      Exit;

  result := PointPlaneOrthoProjection(point, plane, inter, bothface);
end;

// PointTriangleProjection
//
function PointTriangleProjection(const point, direction, ptA, ptB, ptC: TAffineVector;
  var inter: TAffineVector; bothface: Boolean = True): Boolean;
var
  plane: THmgPlane;
begin
  result := False;

  if not IsLineIntersectTriangle(point, direction, ptA, ptB, ptC) then
      Exit;

  plane := PlaneMake(ptA, ptB, ptC);
  result := PointPlaneProjection(point, direction, plane, inter, bothface);
end;

// IsLineIntersectTriangle
//
function IsLineIntersectTriangle(const point, direction, ptA, ptB, ptC: TAffineVector): Boolean;
var
  PA, PB, PC               : TAffineVector;
  crossAB, crossBC, crossCA: TAffineVector;
begin
  result := False;

  PA := VectorSubtract(ptA, point);
  PB := VectorSubtract(ptB, point);
  PC := VectorSubtract(ptC, point);

  crossAB := VectorCrossProduct(PA, PB);
  crossBC := VectorCrossProduct(PB, PC);

  if VectorDotProduct(crossAB, direction) > 0 then
    begin
      if VectorDotProduct(crossBC, direction) > 0 then
        begin
          crossCA := VectorCrossProduct(PC, PA);
          if VectorDotProduct(crossCA, direction) > 0 then
              result := True;
        end;
    end
  else
    if VectorDotProduct(crossBC, direction) < 0 then
    begin
      crossCA := VectorCrossProduct(PC, PA);
      if VectorDotProduct(crossCA, direction) < 0 then
          result := True;
    end
end;

// PointQuadOrthoProjection
//
function PointQuadOrthoProjection(const point, ptA, ptB, ptC, ptD: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;
var
  plane: THmgPlane;
begin
  result := False;

  plane := PlaneMake(ptA, ptB, ptC);
  if not IsLineIntersectQuad(point, Vector3fMake(plane), ptA, ptB, ptC, ptD) then
      Exit;

  result := PointPlaneOrthoProjection(point, plane, inter, bothface);
end;

// PointQuadProjection
//
function PointQuadProjection(const point, direction, ptA, ptB, ptC, ptD: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;
var
  plane: THmgPlane;
begin
  result := False;

  if not IsLineIntersectQuad(point, direction, ptA, ptB, ptC, ptD) then
      Exit;

  plane := PlaneMake(ptA, ptB, ptC);
  result := PointPlaneProjection(point, direction, plane, inter, bothface);
end;

// IsLineIntersectQuad
//
function IsLineIntersectQuad(const point, direction, ptA, ptB, ptC, ptD: TAffineVector): Boolean;
var
  PA, PB, PC, PD                    : TAffineVector;
  crossAB, crossBC, crossCD, crossDA: TAffineVector;
begin
  result := False;

  PA := VectorSubtract(ptA, point);
  PB := VectorSubtract(ptB, point);
  PC := VectorSubtract(ptC, point);
  PD := VectorSubtract(ptD, point);

  crossAB := VectorCrossProduct(PA, PB);
  crossBC := VectorCrossProduct(PB, PC);

  if VectorDotProduct(crossAB, direction) > 0 then
    begin
      if VectorDotProduct(crossBC, direction) > 0 then
        begin
          crossCD := VectorCrossProduct(PC, PD);
          if VectorDotProduct(crossCD, direction) > 0 then
            begin
              crossDA := VectorCrossProduct(PD, PA);
              if VectorDotProduct(crossDA, direction) > 0 then
                  result := True;
            end;
        end;
    end
  else
    if VectorDotProduct(crossBC, direction) < 0 then
    begin
      crossCD := VectorCrossProduct(PC, PD);
      if VectorDotProduct(crossCD, direction) < 0 then
        begin
          crossDA := VectorCrossProduct(PD, PA);
          if VectorDotProduct(crossDA, direction) < 0 then
              result := True;
        end;
    end
end;

// PointDiskOrthoProjection
//
function PointDiskOrthoProjection(const point, center, up: TAffineVector; const radius: single; var inter: TAffineVector; bothface: Boolean = True): Boolean;
begin
  if PointPlaneOrthoProjection(point, PlaneMake(center, up), inter, bothface) then
      result := (VectorDistance2(inter, center) <= radius * radius)
  else
      result := False;
end;

// PointDiskProjection
//
function PointDiskProjection(const point, direction, center, up: TAffineVector; const radius: single; var inter: TAffineVector; bothface: Boolean = True): Boolean;
begin
  if PointPlaneProjection(point, direction, PlaneMake(center, up), inter, bothface) then
      result := VectorDistance2(inter, center) <= radius * radius
  else
      result := False;
end;

// PointLineClosestPoint
//
function PointLineClosestPoint(const point, linePoint, lineDirection: TAffineVector): TAffineVector;
var
  w        : TAffineVector;
  c1, c2, b: single;
begin
  w := VectorSubtract(point, linePoint);

  c1 := VectorDotProduct(w, lineDirection);
  c2 := VectorDotProduct(lineDirection, lineDirection);
  b := c1 / c2;

  VectorAdd(linePoint, VectorScale(lineDirection, b), result);
end;

// PointLineDistance
//
function PointLineDistance(const point, linePoint, lineDirection: TAffineVector): single;
var
  PB: TAffineVector;
begin
  PB := PointLineClosestPoint(point, linePoint, lineDirection);
  result := VectorDistance(point, PB);
end;

// PointSegmentClosestPoint
//
function PointSegmentClosestPoint(const point, segmentStart, segmentStop: TVector): TVector;
var
  w, lineDirection: TVector;
  c1, c2, b       : single;
begin
  lineDirection := VectorSubtract(segmentStop, segmentStart);
  w := VectorSubtract(point, segmentStart);

  c1 := VectorDotProduct(w, lineDirection);
  c2 := VectorDotProduct(lineDirection, lineDirection);
  b := ClampValue(c1 / c2, 0, 1);

  VectorAdd(segmentStart, VectorScale(lineDirection, b), result);
end;

// PointSegmentClosestPoint
//
function PointSegmentClosestPoint(const point, segmentStart, segmentStop: TAffineVector): TAffineVector;
var
  w, lineDirection: TAffineVector;
  c1, c2, b       : single;
begin
  lineDirection := VectorSubtract(segmentStop, segmentStart);
  w := VectorSubtract(point, segmentStart);

  c1 := VectorDotProduct(w, lineDirection);
  c2 := VectorDotProduct(lineDirection, lineDirection);
  b := ClampValue(c1 / c2, 0, 1);

  VectorAdd(segmentStart, VectorScale(lineDirection, b), result);
end;

// PointSegmentDistance
//
function PointSegmentDistance(const point, segmentStart, segmentStop: TAffineVector): single;
var
  PB: TAffineVector;
begin
  PB := PointSegmentClosestPoint(point, segmentStart, segmentStop);
  result := VectorDistance(point, PB);
end;

// http://geometryalgorithms.com/Archive/algorithm_0104/algorithm_0104B.htm
// SegmentSegmentClosestPoint
//
procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start, S1Stop: TAffineVector; var Segment0Closest, Segment1Closest: TAffineVector);
const
  cSMALL_NUM = 0.000000001;
var
  u, v, w                                           : TAffineVector;
  a, b, c, smalld, e, largeD, sc, sn, sD, tc, tN, tD: single;
begin
  VectorSubtract(S0Stop, S0Start, u);
  VectorSubtract(S1Stop, S1Start, v);
  VectorSubtract(S0Start, S1Start, w);

  a := VectorDotProduct(u, u);
  b := VectorDotProduct(u, v);
  c := VectorDotProduct(v, v);
  smalld := VectorDotProduct(u, w);
  e := VectorDotProduct(v, w);
  largeD := a * c - b * b;

  sD := largeD;
  tD := largeD;

  if largeD < cSMALL_NUM then
    begin
      sn := 0.0;
      sD := 1.0;
      tN := e;
      tD := c;
    end
  else
    begin
      sn := (b * e - c * smalld);
      tN := (a * e - b * smalld);
      if (sn < 0.0) then
        begin
          sn := 0.0;
          tN := e;
          tD := c;
        end
      else if (sn > sD) then
        begin
          sn := sD;
          tN := e + b;
          tD := c;
        end;
    end;

  if (tN < 0.0) then
    begin
      tN := 0.0;
      // recompute sc for this edge
      if (-smalld < 0.0) then
          sn := 0.0
      else if (-smalld > a) then
          sn := sD
      else
        begin
          sn := -smalld;
          sD := a;
        end;
    end
  else if (tN > tD) then
    begin
      tN := tD;
      // recompute sc for this edge
      if ((-smalld + b) < 0.0) then
          sn := 0
      else if ((-smalld + b) > a) then
          sn := sD
      else
        begin
          sn := (-smalld + b);
          sD := a;
        end;
    end;

  // finally do the division to get sc and tc
  // sc := (abs(sN) < SMALL_NUM ? 0.0 : sN / sD);
  if Abs(sn) < cSMALL_NUM then
      sc := 0
  else
      sc := sn / sD;

  // tc := (abs(tN) < SMALL_NUM ? 0.0 : tN / tD);
  if Abs(tN) < cSMALL_NUM then
      tc := 0
  else
      tc := tN / tD;

  // get the difference of the two closest points
  // Vector   dP = w + (sc * u) - (tc * v);  // = S0(sc) - S1(tc)

  Segment0Closest := VectorAdd(S0Start, VectorScale(u, sc));
  Segment1Closest := VectorAdd(S1Start, VectorScale(v, tc));
end;

// SegmentSegmentDistance
//
function SegmentSegmentDistance(const S0Start, S0Stop, S1Start, S1Stop: TAffineVector): single;
var
  Pb0, PB1: TAffineVector;
begin
  SegmentSegmentClosestPoint(S0Start, S0Stop, S1Start, S1Stop, Pb0, PB1);
  result := VectorDistance(Pb0, PB1);
end;

// LineLineDistance
//
function LineLineDistance(const linePt0, lineDir0, linePt1, lineDir1: TAffineVector): single;
const
  cBIAS = 0.000000001;
var
  det: single;
begin
  det := Abs((linePt1[0] - linePt0[0]) * (lineDir0[1] * lineDir1[2] - lineDir1[1] * lineDir0[2]) -
    (linePt1[1] - linePt0[1]) * (lineDir0[0] * lineDir1[2] - lineDir1[0] * lineDir0[2]) +
    (linePt1[2] - linePt0[2]) * (lineDir0[0] * lineDir1[1] - lineDir1[0] * lineDir0[1]));
  if det < cBIAS then
      result := PointLineDistance(linePt0, linePt1, lineDir1)
  else
      result := det / VectorLength(VectorCrossProduct(lineDir0, lineDir1));
end;

// QuaternionMake
//
function QuaternionMake(const Imag: array of single; Real: single): TQuaternion;
var
  n: Integer;
begin
  n := Length(Imag);
  if n >= 1 then
      result.ImagPart[0] := Imag[0];
  if n >= 2 then
      result.ImagPart[1] := Imag[1];
  if n >= 3 then
      result.ImagPart[2] := Imag[2];
  result.RealPart := Real;
end;

// QuaternionConjugate
//
function QuaternionConjugate(const Q: TQuaternion): TQuaternion;
begin
  result.ImagPart[0] := -Q.ImagPart[0];
  result.ImagPart[1] := -Q.ImagPart[1];
  result.ImagPart[2] := -Q.ImagPart[2];
  result.RealPart := Q.RealPart;
end;

// QuaternionMagnitude
//
function QuaternionMagnitude(const Q: TQuaternion): single;
begin
  result := Sqrt(VectorNorm(Q.ImagPart) + Sqr(Q.RealPart));
end;

// NormalizeQuaternion
//
procedure NormalizeQuaternion(var Q: TQuaternion);
var
  M, f: single;
begin
  M := QuaternionMagnitude(Q);
  if M > EPSILON2 then begin
      f := 1 / M;
      ScaleVector(Q.ImagPart, f);
      Q.RealPart := Q.RealPart * f;
    end
  else
      Q := IdentityQuaternion;
end;

// QuaternionFromPoints
//
function QuaternionFromPoints(const V1, V2: TAffineVector): TQuaternion;
begin
  result.ImagPart := VectorCrossProduct(V1, V2);
  result.RealPart := Sqrt((VectorDotProduct(V1, V2) + 1) / 2);
end;

// QuaternionFromMatrix
//
function QuaternionFromMatrix(const mat: TMatrix): TQuaternion;
// the matrix must be a rotation matrix!
var
  traceMat, S, invS: double;
begin
  traceMat := 1 + mat[0, 0] + mat[1, 1] + mat[2, 2];
  if traceMat > EPSILON2 then begin
      S := Sqrt(traceMat) * 2;
      invS := 1 / S;
      result.ImagPart[0] := (mat[1, 2] - mat[2, 1]) * invS;
      result.ImagPart[1] := (mat[2, 0] - mat[0, 2]) * invS;
      result.ImagPart[2] := (mat[0, 1] - mat[1, 0]) * invS;
      result.RealPart := 0.25 * S;
    end
  else if (mat[0, 0] > mat[1, 1]) and (mat[0, 0] > mat[2, 2]) then begin // Row 0:
      S := Sqrt(MaxFloat(EPSILON2, cOne + mat[0, 0] - mat[1, 1] - mat[2, 2])) * 2;
      invS := 1 / S;
      result.ImagPart[0] := 0.25 * S;
      result.ImagPart[1] := (mat[0, 1] + mat[1, 0]) * invS;
      result.ImagPart[2] := (mat[2, 0] + mat[0, 2]) * invS;
      result.RealPart := (mat[1, 2] - mat[2, 1]) * invS;
    end
  else if (mat[1, 1] > mat[2, 2]) then begin // Row 1:
      S := Sqrt(MaxFloat(EPSILON2, cOne + mat[1, 1] - mat[0, 0] - mat[2, 2])) * 2;
      invS := 1 / S;
      result.ImagPart[0] := (mat[0, 1] + mat[1, 0]) * invS;
      result.ImagPart[1] := 0.25 * S;
      result.ImagPart[2] := (mat[1, 2] + mat[2, 1]) * invS;
      result.RealPart := (mat[2, 0] - mat[0, 2]) * invS;
    end
  else begin // Row 2:
      S := Sqrt(MaxFloat(EPSILON2, cOne + mat[2, 2] - mat[0, 0] - mat[1, 1])) * 2;
      invS := 1 / S;
      result.ImagPart[0] := (mat[2, 0] + mat[0, 2]) * invS;
      result.ImagPart[1] := (mat[1, 2] + mat[2, 1]) * invS;
      result.ImagPart[2] := 0.25 * S;
      result.RealPart := (mat[0, 1] - mat[1, 0]) * invS;
    end;
  NormalizeQuaternion(result);
end;

// QuaternionMultiply
//
function QuaternionMultiply(const qL, qR: TQuaternion): TQuaternion;
var
  Temp: TQuaternion;
begin
  Temp.RealPart := qL.RealPart * qR.RealPart - qL.ImagPart[x] * qR.ImagPart[x]
    - qL.ImagPart[y] * qR.ImagPart[y] - qL.ImagPart[z] * qR.ImagPart[z];
  Temp.ImagPart[x] := qL.RealPart * qR.ImagPart[x] + qL.ImagPart[x] * qR.RealPart
    + qL.ImagPart[y] * qR.ImagPart[z] - qL.ImagPart[z] * qR.ImagPart[y];
  Temp.ImagPart[y] := qL.RealPart * qR.ImagPart[y] + qL.ImagPart[y] * qR.RealPart
    + qL.ImagPart[z] * qR.ImagPart[x] - qL.ImagPart[x] * qR.ImagPart[z];
  Temp.ImagPart[z] := qL.RealPart * qR.ImagPart[z] + qL.ImagPart[z] * qR.RealPart
    + qL.ImagPart[x] * qR.ImagPart[y] - qL.ImagPart[y] * qR.ImagPart[x];
  result := Temp;
end;

// QuaternionToMatrix
//
function QuaternionToMatrix(quat: TQuaternion): TMatrix;
var
  w, x, y, z, xx, xy, xz, xw, yy, yz, yw, zz, zw: single;
begin
  NormalizeQuaternion(quat);
  w := quat.RealPart;
  x := quat.ImagPart[0];
  y := quat.ImagPart[1];
  z := quat.ImagPart[2];
  xx := x * x;
  xy := x * y;
  xz := x * z;
  xw := x * w;
  yy := y * y;
  yz := y * z;
  yw := y * w;
  zz := z * z;
  zw := z * w;
  result[0, 0] := 1 - 2 * (yy + zz);
  result[1, 0] := 2 * (xy - zw);
  result[2, 0] := 2 * (xz + yw);
  result[3, 0] := 0;
  result[0, 1] := 2 * (xy + zw);
  result[1, 1] := 1 - 2 * (xx + zz);
  result[2, 1] := 2 * (yz - xw);
  result[3, 1] := 0;
  result[0, 2] := 2 * (xz - yw);
  result[1, 2] := 2 * (yz + xw);
  result[2, 2] := 1 - 2 * (xx + yy);
  result[3, 2] := 0;
  result[0, 3] := 0;
  result[1, 3] := 0;
  result[2, 3] := 0;
  result[3, 3] := 1;
end;

// QuaternionToAffineMatrix
//
function QuaternionToAffineMatrix(quat: TQuaternion): TAffineMatrix;
var
  w, x, y, z, xx, xy, xz, xw, yy, yz, yw, zz, zw: single;
begin
  NormalizeQuaternion(quat);
  w := quat.RealPart;
  x := quat.ImagPart[0];
  y := quat.ImagPart[1];
  z := quat.ImagPart[2];
  xx := x * x;
  xy := x * y;
  xz := x * z;
  xw := x * w;
  yy := y * y;
  yz := y * z;
  yw := y * w;
  zz := z * z;
  zw := z * w;
  result[0, 0] := 1 - 2 * (yy + zz);
  result[1, 0] := 2 * (xy - zw);
  result[2, 0] := 2 * (xz + yw);
  result[0, 1] := 2 * (xy + zw);
  result[1, 1] := 1 - 2 * (xx + zz);
  result[2, 1] := 2 * (yz - xw);
  result[0, 2] := 2 * (xz - yw);
  result[1, 2] := 2 * (yz + xw);
  result[2, 2] := 1 - 2 * (xx + yy);
end;

// QuaternionFromAngleAxis
//
function QuaternionFromAngleAxis(const angle: single; const axis: TAffineVector): TQuaternion;
var
  f, S, c: single;
begin
  GeometryLib.SinCos(GeometryLib.DegToRad(angle * cOneDotFive), S, c);
  result.RealPart := c;
  f := S / VectorLength(axis);
  result.ImagPart[0] := axis[0] * f;
  result.ImagPart[1] := axis[1] * f;
  result.ImagPart[2] := axis[2] * f;
end;

// QuaternionFromRollPitchYaw
//
function QuaternionFromRollPitchYaw(const r, p, y: single): TQuaternion;
var
  qp, qy: TQuaternion;
begin
  result := QuaternionFromAngleAxis(r, ZVector);
  qp := QuaternionFromAngleAxis(p, XVector);
  qy := QuaternionFromAngleAxis(y, YVector);

  result := QuaternionMultiply(qp, result);
  result := QuaternionMultiply(qy, result);
end;

// QuaternionFromEuler
//
function QuaternionFromEuler(const x, y, z: single; eulerOrder: TEulerOrder): TQuaternion;
// input angles in degrees
var
  gimbalLock  : Boolean;
  quat1, quat2: TQuaternion;

  function EulerToQuat(const x, y, z: single; eulerOrder: TEulerOrder): TQuaternion;
  const
    cOrder: array [low(TEulerOrder) .. high(TEulerOrder)] of array [1 .. 3] of byte =
      ((1, 2, 3), (1, 3, 2), (2, 1, 3), // eulXYZ, eulXZY, eulYXZ,
      (3, 1, 2), (2, 3, 1), (3, 2, 1)); // eulYZX, eulZXY, eulZYX
  var
    Q: array [1 .. 3] of TQuaternion;
  begin
    Q[cOrder[eulerOrder][1]] := QuaternionFromAngleAxis(x, XVector);
    Q[cOrder[eulerOrder][2]] := QuaternionFromAngleAxis(y, YVector);
    Q[cOrder[eulerOrder][3]] := QuaternionFromAngleAxis(z, ZVector);
    result := QuaternionMultiply(Q[2], Q[3]);
    result := QuaternionMultiply(Q[1], result);
  end;

const
  SMALL_ANGLE = 0.001;
begin
  NormalizeDegAngle(x);
  NormalizeDegAngle(y);
  NormalizeDegAngle(z);
  case eulerOrder of
    eulXYZ, eulZYX: gimbalLock := Abs(Abs(y) - 90.0) <= EPSILON2; // cos(Y) = 0;
    eulYXZ, eulZXY: gimbalLock := Abs(Abs(x) - 90.0) <= EPSILON2; // cos(X) = 0;
    eulXZY, eulYZX: gimbalLock := Abs(Abs(z) - 90.0) <= EPSILON2; // cos(Z) = 0;
    else
      Assert(False);
      gimbalLock := False;
  end;
  if gimbalLock then begin
      case eulerOrder of
        eulXYZ, eulZYX: quat1 := EulerToQuat(x, y - SMALL_ANGLE, z, eulerOrder);
        eulYXZ, eulZXY: quat1 := EulerToQuat(x - SMALL_ANGLE, y, z, eulerOrder);
        eulXZY, eulYZX: quat1 := EulerToQuat(x, y, z - SMALL_ANGLE, eulerOrder);
      end;
      case eulerOrder of
        eulXYZ, eulZYX: quat2 := EulerToQuat(x, y + SMALL_ANGLE, z, eulerOrder);
        eulYXZ, eulZXY: quat2 := EulerToQuat(x + SMALL_ANGLE, y, z, eulerOrder);
        eulXZY, eulYZX: quat2 := EulerToQuat(x, y, z + SMALL_ANGLE, eulerOrder);
      end;
      result := QuaternionSlerp(quat1, quat2, 0.5);
    end
  else begin
      result := EulerToQuat(x, y, z, eulerOrder);
    end;
end;

// QuaternionToPoints
//
procedure QuaternionToPoints(const Q: TQuaternion; var ArcFrom, ArcTo: TAffineVector);
var
  S, invS: single;
begin
  S := Q.ImagPart[x] * Q.ImagPart[x] + Q.ImagPart[y] * Q.ImagPart[y];
  if S = 0 then
      SetAffineVector(ArcFrom, 0, 1, 0)
  else begin
      invS := RSqrt(S);
      SetAffineVector(ArcFrom, -Q.ImagPart[y] * invS, Q.ImagPart[x] * invS, 0);
    end;
  ArcTo[x] := Q.RealPart * ArcFrom[x] - Q.ImagPart[z] * ArcFrom[y];
  ArcTo[y] := Q.RealPart * ArcFrom[y] + Q.ImagPart[z] * ArcFrom[x];
  ArcTo[z] := Q.ImagPart[x] * ArcFrom[y] - Q.ImagPart[y] * ArcFrom[x];
  if Q.RealPart < 0 then
      SetAffineVector(ArcFrom, -ArcFrom[x], -ArcFrom[y], 0);
end;

// LnXP1
//
function LnXP1(x: Extended): Extended;
begin
  result := Math.LnXP1(x);
end;

// Log10
//
function Log10(x: Extended): Extended;
begin
  result := Math.Log10(x);
end;

// Log2
//
function Log2(x: Extended): Extended;
begin
  result := Math.Log2(x);
end;

// Log2
//
function Log2(x: single): single;
begin
  result := Math.Log2(x);
end;

// LogN
//
function LogN(Base, x: Extended): Extended;
begin
  result := Math.LogN(Base, x);
end;

// IntPower
//
function IntPower(Base: Extended; Exponent: Integer): Extended;
begin
  result := Math.IntPower(Base, Exponent);
end;

// Power
//
function Power(const Base, Exponent: single): single;
begin
  if Exponent = cZero then
      result := cOne
  else if (Base = cZero) and (Exponent > cZero) then
      result := cZero
  else if RoundInt(Exponent) = Exponent then
      result := Power(Base, Integer(Round(Exponent)))
  else
      result := Exp(Exponent * Ln(Base));
end;

// Power (int exponent)
//
function Power(Base: single; Exponent: Integer): single;
begin
  result := Math.Power(Base, Exponent);
end;

function Power(Base: single; Exponent: Int64): single;
begin
  result := Math.Power(Base, Exponent);
end;

// DegToRad (extended)
//
function DegToRad(const Degrees: Extended): Extended;
begin
  result := Degrees * (PI / 180);
end;

// DegToRad (single)
//
function DegToRad(const Degrees: single): single;
// Result:=Degrees * cPIdiv180;
// don't laugh, Delphi's compiler manages to make a nightmare of this one
// with pushs, pops, etc. in its default compile... (this one is twice faster !)
begin
  result := Degrees * cPIdiv180;
end;

// RadToDeg (extended)
//
function RadToDeg(const Radians: Extended): Extended;
begin
  result := Radians * (180 / PI);
end;

// RadToDeg (single)
//
function RadToDeg(const Radians: single): single;
// Result:=Radians * c180divPI;
// don't laugh, Delphi's compiler manages to make a nightmare of this one
// with pushs, pops, etc. in its default compile... (this one is twice faster !)
begin
  result := Radians * c180divPI;
end;

// NormalizeAngle
//
function NormalizeAngle(angle: single): single;
begin
  result := angle - Int(angle * cInv2PI) * c2PI;
  if result > PI then
      result := result - 2 * PI
  else if result < -PI then
      result := result + 2 * PI;
end;

// NormalizeDegAngle
//
function NormalizeDegAngle(angle: single): single;
begin
  result := angle - Int(angle * cInv360) * c360;
  if result > c180 then
      result := result - c360
  else if result < -c180 then
      result := result + c360;
end;

// SinCos (Extended)
//
procedure SinCos(const Theta: Extended; out Sin, Cos: Extended);
begin
  Math.SinCos(Theta, Sin, Cos);
end;

// SinCos (Single)
//
procedure SinCos(const Theta: single; out Sin, Cos: single);
var
  S, c: Extended;
begin
  Math.SinCos(Theta, S, c);
  Sin := S;
  Cos := c;
end;

// SinCos (Extended w radius)
//
procedure SinCos(const Theta, radius: double; out Sin, Cos: Extended);
var
  S, c: Extended;
begin
  Math.SinCos(Theta, S, c);
  Sin := S * radius;
  Cos := c * radius;
end;

// SinCos (Single w radius)
//
procedure SinCos(const Theta, radius: single; out Sin, Cos: single);
var
  S, c: Extended;
begin
  Math.SinCos(Theta, S, c);
  Sin := S * radius;
  Cos := c * radius;
end;

// PrepareSinCosCache
//
procedure PrepareSinCosCache(var S, c: array of single;
  startAngle, stopAngle: single);
var
  i             : Integer;
  d, alpha, beta: single;
begin
  Assert((high(S) = high(c)) and (low(S) = low(c)));
  stopAngle := stopAngle + 1E-5;
  if high(S) > low(S) then
      d := cPIdiv180 * (stopAngle - startAngle) / (high(S) - low(S))
  else
      d := 0;

  if high(S) - low(S) < 1000 then begin
      // Fast computation (approx 5.5x)
      alpha := 2 * Sqr(Sin(d * 0.5));
      beta := Sin(d);
      GeometryLib.SinCos(startAngle * cPIdiv180, S[low(S)], c[low(S)]);
      for i := low(S) to high(S) - 1 do begin
          // Make use of the incremental formulae:
          // cos (theta+delta) = cos(theta) - [alpha*cos(theta) + beta*sin(theta)]
          // sin (theta+delta) = sin(theta) - [alpha*sin(theta) - beta*cos(theta)]
          c[i + 1] := c[i] - alpha * c[i] - beta * S[i];
          S[i + 1] := S[i] - alpha * S[i] + beta * c[i];
        end;
    end
  else begin
      // Slower, but maintains precision when steps are small
      startAngle := startAngle * cPIdiv180;
      for i := low(S) to high(S) do
          GeometryLib.SinCos((i - low(S)) * d + startAngle, S[i], c[i]);
    end;
end;

// ArcCos (Extended)
//
function ArcCos(const x: Extended): Extended;
begin
  result := GeometryLib.ArcTan2(Sqrt(1 - Sqr(x)), x);
end;

// ArcCos (Single)
//
function ArcCos(const x: single): single;
// Result:=ArcTan2(Sqrt(c1 - X * X), X);
begin
  {$IFDEF FPC}
  if Abs(x) > 1.0 then
      result := Math.ArcCos(Sign(x))
  else
    {$ENDIF}
      result := Math.ArcCos(x);
end;

// ArcSin (Extended)
//
function ArcSin(const x: Extended): Extended;
begin
  result := GeometryLib.ArcTan2(x, Sqrt(1 - Sqr(x)))
end;

// ArcSin (Single)
//
function ArcSin(const x: single): single;
// Result:=ArcTan2(X, Sqrt(1 - X * X))
begin
  result := Math.ArcSin(x);
end;

// ArcTan2 (Extended)
//
function ArcTan2(const y, x: Extended): Extended;
begin
  result := Math.ArcTan2(y, x);
end;

// ArcTan2 (Single)
//
function ArcTan2(const y, x: single): single;
begin
  result := Math.ArcTan2(y, x);
end;

// FastArcTan2
//
function FastArcTan2(y, x: single): single;
// accuracy of about 0.07 rads
const
  cEpsilon: single = 1E-10;
var
  abs_y: single;
begin
  abs_y := Abs(y) + cEpsilon; // prevent 0/0 condition
  if y < 0 then begin
      if x >= 0 then
          result := cPIdiv4 * (x - abs_y) / (x + abs_y) - cPIdiv4
      else
          result := cPIdiv4 * (x + abs_y) / (abs_y - x) - c3PIdiv4;
    end
  else begin
      if x >= 0 then
          result := cPIdiv4 - cPIdiv4 * (x - abs_y) / (x + abs_y)
      else
          result := c3PIdiv4 - cPIdiv4 * (x + abs_y) / (abs_y - x);
    end;
end;

// Tan (Extended)
//
function Tan(const x: Extended): Extended;
begin
  result := Math.Tan(x);
end;

// Tan (Single)
//
function Tan(const x: single): single;
begin
  result := Math.Tan(x);
end;

// CoTan (Extended)
//
function CoTan(const x: Extended): Extended;
begin
  result := Math.CoTan(x);
end;

// CoTan (Single)
//
function CoTan(const x: single): single;
begin
  result := Math.CoTan(x);
end;

// Sinh
//
function Sinh(const x: single): single;
begin
  result := 0.5 * (Exp(x) - Exp(-x));
end;

// Sinh
//
function Sinh(const x: double): double;
begin
  result := 0.5 * (Exp(x) - Exp(-x));
end;

// Cosh
//
function Cosh(const x: single): single;
begin
  result := 0.5 * (Exp(x) + Exp(-x));
end;

// Cosh
//
function Cosh(const x: double): double;
begin
  result := 0.5 * (Exp(x) + Exp(-x));
end;

// RSqrt
//
function RSqrt(v: single): single;
begin
  result := 1 / Sqrt(v);
end;

// ISqrt
//
function ISqrt(i: Integer): Integer;
begin
  result := Round(Sqrt(i));
end;

// ILength
//
function ILength(x, y: Integer): Integer;
begin
  result := Round(Sqrt(x * x + y * y));
end;

// ILength
//
function ILength(x, y, z: Integer): Integer;
begin
  result := Round(Sqrt(x * x + y * y + z * z));
end;

// RLength
//
function RLength(x, y: single): single;
begin
  result := 1 / Sqrt(x * x + y * y);
end;

// RandomPointOnSphere
//
procedure RandomPointOnSphere(var p: TAffineVector);
var
  T, w: single;
begin
  p[2] := 2 * Random - 1;
  T := 2 * PI * Random;
  w := Sqrt(1 - p[2] * p[2]);
  GeometryLib.SinCos(T, w, p[1], p[0]);
end;

// RoundInt (single)
//
function RoundInt(v: single): single;
begin
  result := Int(v + cOneDotFive);
end;

// RoundInt (extended)
//
function RoundInt(v: Extended): Extended;
begin
  result := Int(v + 0.5);
end;

function Trunc(x: Extended): Int64;
begin
  result := System.Trunc(x);
end;

function Round(x: Extended): Int64;
begin
  result := System.Round(x);
end;

function Frac(x: Extended): Extended;
begin
  result := System.Frac(x);
end;

// Ceil64 (Extended)
//
function Ceil64(v: Extended): Int64; overload;
begin
  if Frac(v) > 0 then
      result := Trunc(v) + 1
  else
      result := Trunc(v);
end;

// Ceil (Single)
//
function Ceil(v: single): Integer; overload;
begin
  if Frac(v) > 0 then
      result := Trunc(v) + 1
  else
      result := Trunc(v);
end;

// Floor64 (Extended)
//
function Floor64(v: Extended): Int64; overload;
begin
  if v < 0 then
      result := Trunc(v) - 1
  else
      result := Trunc(v);
end;

// Floor (Single)
//
function Floor(v: single): Integer; overload;
begin
  if v < 0 then
      result := Trunc(v) - 1
  else
      result := Trunc(v);
end;

// Sign
//
function Sign(x: single): Integer;
begin
  if x < 0 then
      result := -1
  else if x > 0 then
      result := 1
  else
      result := 0;
end;

// SignStrict
//
function SignStrict(x: single): Integer;
begin
  if x < 0 then
      result := -1
  else
      result := 1
end;

// ScaleAndRound
//
function ScaleAndRound(i: Integer; var S: single): Integer;
begin
  result := Round(i * S);
end;

// IsInRange (single)
//
function IsInRange(const x, a, b: single): Boolean;
begin
  if a < b then
      result := (a <= x) and (x <= b)
  else
      result := (b <= x) and (x <= a);
end;

// IsInRange (double)
//
function IsInRange(const x, a, b: double): Boolean;
begin
  if a < b then
      result := (a <= x) and (x <= b)
  else
      result := (b <= x) and (x <= a);
end;

// IsInCube (affine)
//
function IsInCube(const p, d: TAffineVector): Boolean; overload;
begin
  result := ((p[0] >= -d[0]) and (p[0] <= d[0]))
    and ((p[1] >= -d[1]) and (p[1] <= d[1]))
    and ((p[2] >= -d[2]) and (p[2] <= d[2]));
end;

// IsInCube (hmg)
//
function IsInCube(const p, d: TVector): Boolean; overload;
begin
  result := ((p[0] >= -d[0]) and (p[0] <= d[0]))
    and ((p[1] >= -d[1]) and (p[1] <= d[1]))
    and ((p[2] >= -d[2]) and (p[2] <= d[2]));
end;

// MinFloat (single)
//
function MinFloat(values: PSingleArray; nbItems: Integer): single;
var
  i, k: Integer;
begin
  if nbItems > 0 then begin
      k := 0;
      for i := 1 to nbItems - 1 do
        if values^[i] < values^[k] then
            k := i;
      result := values^[k];
    end
  else
      result := 0;
end;

// MinFloat (double)
//
function MinFloat(values: PDoubleArray; nbItems: Integer): double;
var
  i, k: Integer;
begin
  if nbItems > 0 then begin
      k := 0;
      for i := 1 to nbItems - 1 do
        if values^[i] < values^[k] then
            k := i;
      result := values^[k];
    end
  else
      result := 0;
end;

// MinFloat (array)
//
function MinFloat(const v: array of single): single;
var
  i: Integer;
begin
  if Length(v) > 0 then begin
      result := v[0];
      for i := 1 to high(v) do
        if v[i] < result then
            result := v[i];
    end
  else
      result := 0;
end;

// MinFloat (single 2)
//
function MinFloat(const V1, V2: single): single;
begin
  if V1 < V2 then
      result := V1
  else
      result := V2;
end;

// MinFloat (double 2)
//
function MinFloat(const V1, V2: double): double;
begin
  if V1 < V2 then
      result := V1
  else
      result := V2;
end;

// MinFloat
//
function MinFloat(const V1, V2, V3: single): single;
begin
  if V1 <= V2 then
    if V1 <= V3 then
        result := V1
    else if V3 <= V2 then
        result := V3
    else
        result := V2
  else if V2 <= V3 then
      result := V2
  else if V3 <= V1 then
      result := V3
  else
      result := V1;
end;

// MinFloat (double)
//
function MinFloat(const V1, V2, V3: double): double;
begin
  if V1 <= V2 then
    if V1 <= V3 then
        result := V1
    else if V3 <= V2 then
        result := V3
    else
        result := V2
  else if V2 <= V3 then
      result := V2
  else if V3 <= V1 then
      result := V3
  else
      result := V1;
end;

// MaxFloat (single)
//
function MaxFloat(values: PSingleArray; nbItems: Integer): single; overload;
var
  i, k: Integer;
begin
  if nbItems > 0 then begin
      k := 0;
      for i := 1 to nbItems - 1 do
        if values^[i] > values^[k] then
            k := i;
      result := values^[k];
    end
  else
      result := 0;
end;

// MaxFloat (double)
//
function MaxFloat(values: PDoubleArray; nbItems: Integer): double; overload;
var
  i, k: Integer;
begin
  if nbItems > 0 then begin
      k := 0;
      for i := 1 to nbItems - 1 do
        if values^[i] > values^[k] then
            k := i;
      result := values^[k];
    end
  else
      result := 0;
end;

// MaxFloat
//
function MaxFloat(const v: array of single): single;
var
  i: Integer;
begin
  if Length(v) > 0 then begin
      result := v[0];
      for i := 1 to high(v) do
        if v[i] > result then
            result := v[i];
    end
  else
      result := 0;
end;

// MaxFloat
//
function MaxFloat(const V1, V2: single): single;
begin
  if V1 > V2 then
      result := V1
  else
      result := V2;
end;

// MaxFloat
//
function MaxFloat(const V1, V2: double): double;
begin
  if V1 > V2 then
      result := V1
  else
      result := V2;
end;

// MaxFloat
//
function MaxFloat(const V1, V2, V3: single): single;
begin
  if V1 >= V2 then
    if V1 >= V3 then
        result := V1
    else if V3 >= V2 then
        result := V3
    else
        result := V2
  else if V2 >= V3 then
      result := V2
  else if V3 >= V1 then
      result := V3
  else
      result := V1;
end;

// MaxFloat
//
function MaxFloat(const V1, V2, V3: double): double;
begin
  if V1 >= V2 then
    if V1 >= V3 then
        result := V1
    else if V3 >= V2 then
        result := V3
    else
        result := V2
  else if V2 >= V3 then
      result := V2
  else if V3 >= V1 then
      result := V3
  else
      result := V1;
end;

// MinInteger (2 int)
//
function MinInteger(const V1, V2: Integer): Integer;
begin
  if V1 < V2 then
      result := V1
  else
      result := V2;
end;

// MinInteger (2 card)
//
function MinInteger(const V1, V2: Cardinal): Cardinal;
begin
  if V1 < V2 then
      result := V1
  else
      result := V2;
end;

// MinInteger
//
function MinInteger(const V1, V2, V3: Integer): Integer;
begin
  if V1 <= V2 then
    if V1 <= V3 then
        result := V1
    else if V3 <= V2 then
        result := V3
    else
        result := V2
  else if V2 <= V3 then
      result := V2
  else if V3 <= V1 then
      result := V3
  else
      result := V1;
end;

// MinInteger
//
function MinInteger(const V1, V2, V3: Cardinal): Cardinal;
begin
  if V1 <= V2 then
    if V1 <= V3 then
        result := V1
    else if V3 <= V2 then
        result := V3
    else
        result := V2
  else if V2 <= V3 then
      result := V2
  else if V3 <= V1 then
      result := V3
  else
      result := V1;
end;

// MaxInteger (2 int)
//
function MaxInteger(const V1, V2: Integer): Integer;
begin
  if V1 > V2 then
      result := V1
  else
      result := V2;
end;

// MaxInteger (2 card)
//
function MaxInteger(const V1, V2: Cardinal): Cardinal;
begin
  if V1 > V2 then
      result := V1
  else
      result := V2;
end;

// MaxInteger
//
function MaxInteger(const V1, V2, V3: Integer): Integer;
begin
  if V1 >= V2 then
    if V1 >= V3 then
        result := V1
    else if V3 >= V2 then
        result := V3
    else
        result := V2
  else if V2 >= V3 then
      result := V2
  else if V3 >= V1 then
      result := V3
  else
      result := V1;
end;

// MaxInteger
//
function MaxInteger(const V1, V2, V3: Cardinal): Cardinal;
begin
  if V1 >= V2 then
    if V1 >= V3 then
        result := V1
    else if V3 >= V2 then
        result := V3
    else
        result := V2
  else if V2 >= V3 then
      result := V2
  else if V3 >= V1 then
      result := V3
  else
      result := V1;
end;

function ClampInteger(const value, min, max: Integer): Integer;
begin
  result := MinInteger(MaxInteger(value, min), max);
end;

function ClampInteger(const value, min, max: Cardinal): Cardinal;
begin
  result := MinInteger(MaxInteger(value, min), max);
end;

// TriangleArea
//
function TriangleArea(const p1, p2, p3: TAffineVector): single;
begin
  result := 0.5 * VectorLength(VectorCrossProduct(VectorSubtract(p2, p1),
    VectorSubtract(p3, p1)));
end;

// PolygonArea
//
function PolygonArea(const p: PAffineVectorArray; nSides: Integer): single;
var
  r         : TAffineVector;
  i         : Integer;
  p1, p2, p3: PAffineVector;
begin
  result := 0;
  if nSides > 2 then begin
      RstVector(r);
      p1 := @p[0];
      p2 := @p[1];
      for i := 2 to nSides - 1 do begin
          p3 := @p[i];
          AddVector(r, VectorCrossProduct(VectorSubtract(p2^, p1^),
            VectorSubtract(p3^, p1^)));
          p2 := p3;
        end;
      result := VectorLength(r) * 0.5;
    end;
end;

// TriangleSignedArea
//
function TriangleSignedArea(const p1, p2, p3: TAffineVector): single;
begin
  result := 0.5 * ((p2[0] - p1[0]) * (p3[1] - p1[1])
    - (p3[0] - p1[0]) * (p2[1] - p1[1]));
end;

// PolygonSignedArea
//
function PolygonSignedArea(const p: PAffineVectorArray; nSides: Integer): single;
var
  i         : Integer;
  p1, p2, p3: PAffineVector;
begin
  result := 0;
  if nSides > 2 then begin
      p1 := @(p^[0]);
      p2 := @(p^[1]);
      for i := 2 to nSides - 1 do begin
          p3 := @(p^[i]);
          result := result + (p2^[0] - p1^[0]) * (p3^[1] - p1^[1])
            - (p3^[0] - p1^[0]) * (p2^[1] - p1^[1]);
          p2 := p3;
        end;
      result := result * 0.5;
    end;
end;

// ScaleFloatArray (raw)
//
procedure ScaleFloatArray(values: PSingleArray; nb: Integer;
  var factor: single);
var
  i: Integer;
begin
  for i := 0 to nb - 1 do
      values^[i] := values^[i] * factor;
end;

// ScaleFloatArray (array)
//
procedure ScaleFloatArray(var values: TSingleArray;
  factor: single);
begin
  if Length(values) > 0 then
      ScaleFloatArray(@values[0], Length(values), factor);
end;

// OffsetFloatArray (raw)
//
procedure OffsetFloatArray(values: PSingleArray; nb: Integer;
  var delta: single);
var
  i: Integer;
begin
  for i := 0 to nb - 1 do
      values^[i] := values^[i] + delta;
end;

// ScaleFloatArray (array)
//
procedure OffsetFloatArray(var values: array of single;
  delta: single);
begin
  if Length(values) > 0 then
      ScaleFloatArray(@values[0], Length(values), delta);
end;

// OffsetFloatArray (raw, raw)
//
procedure OffsetFloatArray(valuesDest, valuesDelta: PSingleArray; nb: Integer);
var
  i: Integer;
begin
  for i := 0 to nb - 1 do
      valuesDest^[i] := valuesDest^[i] + valuesDelta^[i];
end;

// MaxXYZComponent
//
function MaxXYZComponent(const v: TVector): single; overload;
begin
  result := MaxFloat(v[0], v[1], v[2]);
end;

// MaxXYZComponent
//
function MaxXYZComponent(const v: TAffineVector): single; overload;
begin
  result := MaxFloat(v[0], v[1], v[2]);
end;

// MinXYZComponent
//
function MinXYZComponent(const v: TVector): single; overload;
begin
  if v[0] <= v[1] then
    if v[0] <= v[2] then
        result := v[0]
    else if v[2] <= v[1] then
        result := v[2]
    else
        result := v[1]
  else if v[1] <= v[2] then
      result := v[1]
  else if v[2] <= v[0] then
      result := v[2]
  else
      result := v[0];
end;

// MinXYZComponent
//
function MinXYZComponent(const v: TAffineVector): single; overload;
begin
  result := MinFloat(v[0], v[1], v[2]);
end;

// MaxAbsXYZComponent
//
function MaxAbsXYZComponent(v: TVector): single;
begin
  AbsVector(v);
  result := MaxXYZComponent(v);
end;

// MinAbsXYZComponent
//
function MinAbsXYZComponent(v: TVector): single;
begin
  AbsVector(v);
  result := MinXYZComponent(v);
end;

// MaxVector (hmg)
//
procedure MaxVector(var v: TVector; const V1: TVector);
begin
  if V1[0] > v[0] then
      v[0] := V1[0];
  if V1[1] > v[1] then
      v[1] := V1[1];
  if V1[2] > v[2] then
      v[2] := V1[2];
  if V1[3] > v[3] then
      v[3] := V1[3];
end;

// MaxVector (affine)
//
procedure MaxVector(var v: TAffineVector; const V1: TAffineVector); overload;
begin
  if V1[0] > v[0] then
      v[0] := V1[0];
  if V1[1] > v[1] then
      v[1] := V1[1];
  if V1[2] > v[2] then
      v[2] := V1[2];
end;

// MinVector (hmg)
//
procedure MinVector(var v: TVector; const V1: TVector);
begin
  if V1[0] < v[0] then
      v[0] := V1[0];
  if V1[1] < v[1] then
      v[1] := V1[1];
  if V1[2] < v[2] then
      v[2] := V1[2];
  if V1[3] < v[3] then
      v[3] := V1[3];
end;

// MinVector (affine)
//
procedure MinVector(var v: TAffineVector; const V1: TAffineVector);
begin
  if V1[0] < v[0] then
      v[0] := V1[0];
  if V1[1] < v[1] then
      v[1] := V1[1];
  if V1[2] < v[2] then
      v[2] := V1[2];
end;

// SortArrayAscending (extended)
//
procedure SortArrayAscending(var a: array of Extended);
var
  i, J, M: Integer;
  buf    : Extended;
begin
  for i := low(a) to high(a) - 1 do begin
      M := i;
      for J := i + 1 to high(a) do
        if a[J] < a[M] then
            M := J;
      if M <> i then begin
          buf := a[M];
          a[M] := a[i];
          a[i] := buf;
        end;
    end;
end;

// ClampValue (min-max)
//
function ClampValue(const aValue, aMin, aMax: single): single;
begin
  if aValue < aMin then
      result := aMin
  else if aValue > aMax then
      result := aMax
  else
      result := aValue;
end;

// ClampValue (min-)
//
function ClampValue(const aValue, aMin: single): single;
begin
  if aValue < aMin then
      result := aMin
  else
      result := aValue;
end;

// MakeAffineDblVector
//
function MakeAffineDblVector(var v: array of double): TAffineDblVector;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
end;

// MakeDblVector
//
function MakeDblVector(var v: array of double): THomogeneousDblVector;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := v[3];
end;

// PointInPolygon
//
function PointInPolygon(var xp, yp: array of single; x, y: single): Boolean;
// The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
// with some minor modifications for speed.  It returns 1 for strictly
// interior points, 0 for strictly exterior, and 0 or 1 for points on
// the boundary.
var
  i, J: Integer;
begin
  result := False;
  if high(xp) = high(yp) then begin
      J := high(xp);
      for i := 0 to high(xp) do begin
          if ((((yp[i] <= y) and (y < yp[J])) or ((yp[J] <= y) and (y < yp[i])))
            and (x < (xp[J] - xp[i]) * (y - yp[i]) / (yp[J] - yp[i]) + xp[i])) then
              result := not result;
          J := i;
        end;
    end;
end;

// DivMod
//
procedure DivMod(Dividend: Integer; Divisor: Word; var result, Remainder: Word);
begin
  result := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
end;

// ConvertRotation
//
function ConvertRotation(const Angles: TAffineVector): TVector;

{ Rotation of the Angle t about the axis (X, Y, Z) is given by:

  | X^2 + (1-X^2) Cos(t),    XY(1-Cos(t))  +  Z Sin(t), XZ(1-Cos(t))-Y Sin(t) |
  M = | XY(1-Cos(t))-Z Sin(t), Y^2 + (1-Y^2) Cos(t),      YZ(1-Cos(t)) + X Sin(t) |
  | XZ(1-Cos(t)) + Y Sin(t), YZ(1-Cos(t))-X Sin(t),   Z^2 + (1-Z^2) Cos(t)    |

  Rotation about the three axes (Angles a1, a2, a3) can be represented as
  the product of the individual rotation matrices:

  | 1  0       0       | | Cos(a2) 0 -Sin(a2) | |  Cos(a3) Sin(a3) 0 |
  | 0  Cos(a1) Sin(a1) | * | 0       1  0       | * | -Sin(a3) Cos(a3) 0 |
  | 0 -Sin(a1) Cos(a1) | | Sin(a2) 0  Cos(a2) | |  0       0       1 |
  Mx                       My                     Mz

  We now want to solve for X, Y, Z, and t given 9 equations in 4 unknowns.
  Using the diagonal elements of the two matrices, we get:

  X^2 + (1-X^2) Cos(t) = M[0][0]
  Y^2 + (1-Y^2) Cos(t) = M[1][1]
  Z^2 + (1-Z^2) Cos(t) = M[2][2]

  Adding the three equations, we get:

  X^2  +  Y^2  +  Z^2 - (M[0][0]  +  M[1][1]  +  M[2][2]) =
  - (3 - X^2 - Y^2 - Z^2) Cos(t)

  Since (X^2  +  Y^2  +  Z^2) = 1, we can rewrite as:

  Cos(t) = (1 - (M[0][0]  +  M[1][1]  +  M[2][2])) / 2

  Solving for t, we get:

  t = Acos(((M[0][0]  +  M[1][1]  +  M[2][2]) - 1) / 2)

  We can substitute t into the equations for X^2, Y^2, and Z^2 above
  to get the values for X, Y, and Z.  To find the proper signs we note
  that:

  2 X Sin(t) = M[1][2] - M[2][1]
  2 Y Sin(t) = M[2][0] - M[0][2]
  2 Z Sin(t) = M[0][1] - M[1][0]
}
var
  Axis1, Axis2                 : TVector3f;
  M, m1, m2                    : TMatrix;
  cost, cost1, sint, s1, s2, s3: single;
  i                            : Integer;
begin
  // see if we are only rotating about a single Axis
  if Abs(Angles[x]) < EPSILON then begin
      if Abs(Angles[y]) < EPSILON then begin
          SetVector(result, 0, 0, 1, Angles[z]);
          Exit;
        end
      else if Abs(Angles[z]) < EPSILON then begin
          SetVector(result, 0, 1, 0, Angles[y]);
          Exit;
        end
    end
  else if (Abs(Angles[y]) < EPSILON) and (Abs(Angles[z]) < EPSILON) then begin
      SetVector(result, 1, 0, 0, Angles[x]);
      Exit;
    end;

  // make the rotation matrix
  Axis1 := XVector;
  M := CreateRotationMatrix(Axis1, Angles[x]);

  Axis2 := YVector;
  m2 := CreateRotationMatrix(Axis2, Angles[y]);
  m1 := MatrixMultiply(M, m2);

  Axis2 := ZVector;
  m2 := CreateRotationMatrix(Axis2, Angles[z]);
  M := MatrixMultiply(m1, m2);

  cost := ((M[x, x] + M[y, y] + M[z, z]) - 1) / 2;
  if cost < -1 then
      cost := -1
  else if cost > 1 - EPSILON then begin
      // Bad Angle - this would cause a crash
      SetVector(result, XHmgVector);
      Exit;
    end;

  cost1 := 1 - cost;
  SetVector(result, Sqrt((M[x, x] - cost) / cost1),
    Sqrt((M[y, y] - cost) / cost1),
    Sqrt((M[z, z] - cost) / cost1),
    GeometryLib.ArcCos(cost));

  sint := 2 * Sqrt(1 - cost * cost); // This is actually 2 Sin(t)

  // Determine the proper signs
  for i := 0 to 7 do begin
      if (i and 1) > 1 then
          s1 := -1
      else
          s1 := 1;
      if (i and 2) > 1 then
          s2 := -1
      else
          s2 := 1;
      if (i and 4) > 1 then
          s3 := -1
      else
          s3 := 1;
      if (Abs(s1 * result[x] * sint - M[y, z] + M[z, y]) < EPSILON2)
        and (Abs(s2 * result[y] * sint - M[z, x] + M[x, z]) < EPSILON2)
        and (Abs(s3 * result[z] * sint - M[x, y] + M[y, x]) < EPSILON2) then begin
          // We found the right combination of signs
          result[x] := result[x] * s1;
          result[y] := result[y] * s2;
          result[z] := result[z] * s3;
          Exit;
        end;
    end;
end;

// QuaternionSlerp
//
function QuaternionSlerp(const QStart, QEnd: TQuaternion; Spin: Integer; T: single): TQuaternion;
var
  beta,           // complementary interp parameter
  Theta,          // Angle between A and B
  sint, cost,     // sine, cosine of theta
  phi  : single;  // theta plus spins
  bflip: Boolean; // use negativ t?
begin
  // cosine theta
  cost := VectorAngleCosine(QStart.ImagPart, QEnd.ImagPart);

  // if QEnd is on opposite hemisphere from QStart, use -QEnd instead
  if cost < 0 then begin
      cost := -cost;
      bflip := True;
    end
  else
      bflip := False;

  // if QEnd is (within precision limits) the same as QStart,
  // just linear interpolate between QStart and QEnd.
  // Can't do spins, since we don't know what direction to spin.

  if (1 - cost) < EPSILON then
      beta := 1 - T
  else begin
      // normal case
      Theta := GeometryLib.ArcCos(cost);
      phi := Theta + Spin * PI;
      sint := Sin(Theta);
      beta := Sin(Theta - T * phi) / sint;
      T := Sin(T * phi) / sint;
    end;

  if bflip then
      T := -T;

  // interpolate
  result.ImagPart[x] := beta * QStart.ImagPart[x] + T * QEnd.ImagPart[x];
  result.ImagPart[y] := beta * QStart.ImagPart[y] + T * QEnd.ImagPart[y];
  result.ImagPart[z] := beta * QStart.ImagPart[z] + T * QEnd.ImagPart[z];
  result.RealPart := beta * QStart.RealPart + T * QEnd.RealPart;
end;

// QuaternionSlerp
//
function QuaternionSlerp(const source, dest: TQuaternion; const T: single): TQuaternion;
var
  to1                                : array [0 .. 4] of single;
  omega, cosom, sinom, scale0, scale1: Extended;
  // t goes from 0 to 1
  // absolute rotations
begin
  // calc cosine
  cosom := source.ImagPart[0] * dest.ImagPart[0]
    + source.ImagPart[1] * dest.ImagPart[1]
    + source.ImagPart[2] * dest.ImagPart[2]
    + source.RealPart * dest.RealPart;
  // adjust signs (if necessary)
  if cosom < 0 then begin
      cosom := -cosom;
      to1[0] := -dest.ImagPart[0];
      to1[1] := -dest.ImagPart[1];
      to1[2] := -dest.ImagPart[2];
      to1[3] := -dest.RealPart;
    end
  else begin
      to1[0] := dest.ImagPart[0];
      to1[1] := dest.ImagPart[1];
      to1[2] := dest.ImagPart[2];
      to1[3] := dest.RealPart;
    end;
  // calculate coefficients
  if ((1.0 - cosom) > EPSILON2) then begin // standard case (slerp)
      omega := GeometryLib.ArcCos(cosom);
      sinom := 1 / Sin(omega);
      scale0 := Sin((1.0 - T) * omega) * sinom;
      scale1 := Sin(T * omega) * sinom;
    end
  else begin // "from" and "to" quaternions are very close
      // ... so we can do a linear interpolation
      scale0 := 1.0 - T;
      scale1 := T;
    end;
  // calculate final values
  result.ImagPart[0] := scale0 * source.ImagPart[0] + scale1 * to1[0];
  result.ImagPart[1] := scale0 * source.ImagPart[1] + scale1 * to1[1];
  result.ImagPart[2] := scale0 * source.ImagPart[2] + scale1 * to1[2];
  result.RealPart := scale0 * source.RealPart + scale1 * to1[3];
  NormalizeQuaternion(result);
end;

// VectorDblToFlt
//
function VectorDblToFlt(const v: THomogeneousDblVector): THomogeneousVector;
// converts a vector containing double sized values into a vector with single sized values
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := v[3];
end;

// VectorAffineDblToFlt
//
function VectorAffineDblToFlt(const v: TAffineDblVector): TAffineVector;
// converts a vector containing double sized values into a vector with single sized values
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
end;

// VectorAffineFltToDbl
//
function VectorAffineFltToDbl(const v: TAffineVector): TAffineDblVector;
// converts a vector containing single sized values into a vector with double sized values
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
end;

// VectorFltToDbl
//
function VectorFltToDbl(const v: TVector): THomogeneousDblVector;
// converts a vector containing single sized values into a vector with double sized values
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := v[3];
end;

// ----------------- coordinate system manipulation functions -----------------------------------------------------------

// Turn (Y axis)
//
function Turn(const Matrix: TMatrix; angle: single): TMatrix;
begin
  result := MatrixMultiply(Matrix, CreateRotationMatrix(AffineVectorMake(Matrix[1][0], Matrix[1][1], Matrix[1][2]), angle));
end;

// Turn (direction)
//
function Turn(const Matrix: TMatrix; const MasterUp: TAffineVector; angle: single): TMatrix;
begin
  result := MatrixMultiply(Matrix, CreateRotationMatrix(MasterUp, angle));
end;

// Pitch (X axis)
//
function Pitch(const Matrix: TMatrix; angle: single): TMatrix;
begin
  result := MatrixMultiply(Matrix, CreateRotationMatrix(AffineVectorMake(Matrix[0][0], Matrix[0][1], Matrix[0][2]), angle));
end;

// Pitch (direction)
//
function Pitch(const Matrix: TMatrix; const MasterRight: TAffineVector; angle: single): TMatrix; overload;
begin
  result := MatrixMultiply(Matrix, CreateRotationMatrix(MasterRight, angle));
end;

// Roll (Z axis)
//
function Roll(const Matrix: TMatrix; angle: single): TMatrix;
begin
  result := MatrixMultiply(Matrix, CreateRotationMatrix(AffineVectorMake(Matrix[2][0], Matrix[2][1], Matrix[2][2]), angle));
end;

// Roll (direction)
//
function Roll(const Matrix: TMatrix; const MasterDirection: TAffineVector; angle: single): TMatrix; overload;
begin
  result := MatrixMultiply(Matrix, CreateRotationMatrix(MasterDirection, angle));
end;

// RayCastPlaneIntersect (plane defined by point+normal)
//
function RayCastPlaneIntersect(const rayStart, rayVector: TVector;
  const planePoint, planeNormal: TVector;
  intersectPoint: PVector = nil): Boolean;
var
  sp  : TVector;
  T, d: single;
begin
  d := VectorDotProduct(rayVector, planeNormal);
  result := ((d > EPSILON2) or (d < -EPSILON2));
  if result and Assigned(intersectPoint) then begin
      VectorSubtract(planePoint, rayStart, sp);
      d := 1 / d; // will keep one FPU unit busy during dot product calculation
      T := VectorDotProduct(sp, planeNormal) * d;
      if T > 0 then
          VectorCombine(rayStart, rayVector, T, intersectPoint^)
      else
          result := False;
    end;
end;

// RayCastPlaneXZIntersect
//
function RayCastPlaneXZIntersect(const rayStart, rayVector: TVector;
  const planeY: single;
  intersectPoint: PVector = nil): Boolean;
var
  T: single;
begin
  if rayVector[1] = 0 then
      result := False
  else begin
      T := (rayStart[1] - planeY) / rayVector[1];
      if T < 0 then begin
          if Assigned(intersectPoint) then
              VectorCombine(rayStart, rayVector, T, intersectPoint^);
          result := True;
        end
      else
          result := False;
    end;
end;

// RayCastTriangleIntersect
//
function RayCastTriangleIntersect(const rayStart, rayVector: TVector;
  const p1, p2, p3: TAffineVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  pvec                : TAffineVector;
  V1, V2, qvec, tvec  : TVector;
  T, u, v, det, invDet: single;
begin
  VectorSubtract(p2, p1, V1);
  VectorSubtract(p3, p1, V2);
  VectorCrossProduct(rayVector, V2, pvec);
  det := VectorDotProduct(V1, pvec);
  if ((det < EPSILON2) and (det > -EPSILON2)) then begin // vector is parallel to triangle's plane
      result := False;
      Exit;
    end;
  invDet := cOne / det;
  VectorSubtract(rayStart, p1, tvec);
  u := VectorDotProduct(tvec, pvec) * invDet;
  if (u < 0) or (u > 1) then
      result := False
  else begin
      qvec := VectorCrossProduct(tvec, V1);
      v := VectorDotProduct(rayVector, qvec) * invDet;
      result := (v >= 0) and (u + v <= 1);
      if result then begin
          T := VectorDotProduct(V2, qvec) * invDet;
          if T > 0 then begin
              if intersectPoint <> nil then
                  VectorCombine(rayStart, rayVector, T, intersectPoint^);
              if intersectNormal <> nil then
                  VectorCrossProduct(V1, V2, intersectNormal^);
            end
          else
              result := False;
        end;
    end;
end;

// RayCastMinDistToPoint
//
function RayCastMinDistToPoint(const rayStart, rayVector: TVector;
  const point: TVector): single;
var
  proj: single;
begin
  proj := PointProject(point, rayStart, rayVector);
  if proj <= 0 then
      proj := 0; // rays don't go backward!
  result := VectorDistance(point, VectorCombine(rayStart, rayVector, 1, proj));
end;

// RayCastIntersectsSphere
//
function RayCastIntersectsSphere(const rayStart, rayVector: TVector;
  const sphereCenter: TVector;
  const SphereRadius: single): Boolean;
var
  proj: single;
begin
  proj := PointProject(sphereCenter, rayStart, rayVector);
  if proj <= 0 then
      proj := 0; // rays don't go backward!
  result := (VectorDistance2(sphereCenter, VectorCombine(rayStart, rayVector, 1, proj)) <= Sqr(SphereRadius));
end;

// RayCastSphereIntersect
//
function RayCastSphereIntersect(const rayStart, rayVector: TVector;
  const sphereCenter: TVector;
  const SphereRadius: single;
  var i1, i2: TVector): Integer;
var
  proj, d2 : single;
  id2      : Integer;
  projPoint: TVector;
begin
  proj := PointProject(sphereCenter, rayStart, rayVector);
  VectorCombine(rayStart, rayVector, proj, projPoint);
  d2 := SphereRadius * SphereRadius - VectorDistance2(sphereCenter, projPoint);
  id2 := PInteger(@d2)^;
  if id2 >= 0 then begin
      if id2 = 0 then begin
          if PInteger(@proj)^ > 0 then begin
              VectorCombine(rayStart, rayVector, proj, i1);
              result := 1;
              Exit;
            end;
        end
      else if id2 > 0 then begin
          d2 := Sqrt(d2);
          if proj >= d2 then begin
              VectorCombine(rayStart, rayVector, proj - d2, i1);
              VectorCombine(rayStart, rayVector, proj + d2, i2);
              result := 2;
              Exit;
            end
          else if proj + d2 >= 0 then begin
              VectorCombine(rayStart, rayVector, proj + d2, i1);
              result := 1;
              Exit;
            end;
        end;
    end;
  result := 0;
end;

// RayCastBoxIntersect
//
function RayCastBoxIntersect(
  const rayStart, rayVector, aMinExtent, aMaxExtent: TAffineVector;
  intersectPoint: PAffineVector = nil): Boolean;
var
  i, planeInd           : Integer;
  ResAFV, MaxDist, plane: TAffineVector;
  isMiddle              : array [0 .. 2] of Boolean;
begin
  // Find plane.
  result := True;
  for i := 0 to 2 do
    if rayStart[i] < aMinExtent[i] then begin
        plane[i] := aMinExtent[i];
        isMiddle[i] := False;
        result := False;
      end
    else if rayStart[i] > aMaxExtent[i] then begin
        plane[i] := aMaxExtent[i];
        isMiddle[i] := False;
        result := False;
      end
    else begin
        isMiddle[i] := True;
      end;
  if result then begin
      // rayStart inside box.
      if intersectPoint <> nil
      then
          intersectPoint^ := rayStart;
    end
  else begin
      // Distance to plane.
      planeInd := 0;
      for i := 0 to 2 do
        if isMiddle[i]
          or (rayVector[i] = 0)
        then
            MaxDist[i] := -1
        else begin
            MaxDist[i] := (plane[i] - rayStart[i]) / rayVector[i];
            if MaxDist[i] > 0 then begin
                if MaxDist[planeInd] < MaxDist[i]
                then
                    planeInd := i;
                result := True;
              end;
          end;
      // Inside box ?
      if result then begin
          for i := 0 to 2 do
            if planeInd = i
            then
                ResAFV[i] := plane[i]
            else begin
                ResAFV[i] := rayStart[i] + MaxDist[planeInd] * rayVector[i];
                result := (ResAFV[i] >= aMinExtent[i])
                  and (ResAFV[i] <= aMaxExtent[i]);
                if not result then
                    Exit;
              end;
          if intersectPoint <> nil
          then
              intersectPoint^ := ResAFV;
        end;
    end;
end;

// SphereVisibleRadius
//
function SphereVisibleRadius(distance, radius: single): single;
var
  d2, r2, ir, tr: single;
begin
  d2 := distance * distance;
  r2 := radius * radius;
  ir := Sqrt(d2 - r2);
  tr := (d2 + r2 - Sqr(ir)) / (2 * ir);

  result := Sqrt(r2 + Sqr(tr));
end;

// IntersectLinePlane
//
function IntersectLinePlane(const point, direction: TVector;
  const plane: THmgPlane;
  intersectPoint: PVector = nil): Integer;
var
  a, b: Extended;
  T   : single;
begin
  a := VectorDotProduct(plane, direction); // direction projected to plane normal
  b := PlaneEvaluatePoint(plane, point);   // distance to plane
  if a = 0 then begin                      // direction is parallel to plane
      if b = 0 then
          result := -1 // line is inside plane
      else
          result := 0; // line is outside plane
    end
  else begin
      if Assigned(intersectPoint) then begin
          T := -b / a; // parameter of intersection
          intersectPoint^ := point;
          // calculate intersection = p + t*d
          CombineVector(intersectPoint^, direction, T);
        end;
      result := 1;
    end;
end;

// TriangleBoxIntersect
//
function IntersectTriangleBox(
  const p1, p2, p3, aMinExtent, aMaxExtent: TAffineVector): Boolean;
var
  RayDir, iPoint: TAffineVector;
  BoxDiagPt, BoxDiagPt2,
    BoxDiagDir, iPnt: TVector;
begin
  // Triangle edge (p2, p1) - Box intersection
  VectorSubtract(p2, p1, RayDir);
  result := RayCastBoxIntersect(p1, RayDir, aMinExtent, aMaxExtent, @iPoint);
  if result then
      result := VectorNorm(VectorSubtract(p1, iPoint))
      < VectorNorm(VectorSubtract(p1, p2));
  if result then
      Exit;

  // Triangle edge (p3, p1) - Box intersection
  VectorSubtract(p3, p1, RayDir);
  result := RayCastBoxIntersect(p1, RayDir, aMinExtent, aMaxExtent, @iPoint);
  if result then
      result := VectorNorm(VectorSubtract(p1, iPoint))
      < VectorNorm(VectorSubtract(p1, p3));
  if result then
      Exit;

  // Triangle edge (p2, p3) - Box intersection
  VectorSubtract(p2, p3, RayDir);
  result := RayCastBoxIntersect(p3, RayDir, aMinExtent, aMaxExtent, @iPoint);
  if result then
      result := VectorNorm(VectorSubtract(p3, iPoint))
      < VectorNorm(VectorSubtract(p3, p2));
  if result then
      Exit;

  // Triangle - Box diagonal 1 intersection
  BoxDiagPt := VectorMake(aMinExtent);
  VectorSubtract(aMaxExtent, aMinExtent, BoxDiagDir);
  result := RayCastTriangleIntersect(BoxDiagPt, BoxDiagDir, p1, p2, p3, @iPnt);
  if result then
      result := VectorNorm(VectorSubtract(BoxDiagPt, iPnt))
      < VectorNorm(VectorSubtract(aMaxExtent, aMinExtent));
  if result then
      Exit;

  // Triangle - Box diagonal 2 intersection
  BoxDiagPt := VectorMake(aMinExtent[0], aMinExtent[1], aMaxExtent[2]);
  BoxDiagPt2 := VectorMake(aMaxExtent[0], aMaxExtent[1], aMinExtent[2]);
  VectorSubtract(BoxDiagPt2, BoxDiagPt, BoxDiagDir);
  result := RayCastTriangleIntersect(BoxDiagPt, BoxDiagDir, p1, p2, p3, @iPnt);
  if result then
      result := VectorNorm(VectorSubtract(BoxDiagPt, iPnt))
      < VectorNorm(VectorSubtract(BoxDiagPt, BoxDiagPt2));
  if result then
      Exit;

  // Triangle - Box diagonal 3 intersection
  BoxDiagPt := VectorMake(aMinExtent[0], aMaxExtent[1], aMinExtent[2]);
  BoxDiagPt2 := VectorMake(aMaxExtent[0], aMinExtent[1], aMaxExtent[2]);
  VectorSubtract(BoxDiagPt, BoxDiagPt, BoxDiagDir);
  result := RayCastTriangleIntersect(BoxDiagPt, BoxDiagDir, p1, p2, p3, @iPnt);
  if result then
      result := VectorLength(VectorSubtract(BoxDiagPt, iPnt))
      < VectorLength(VectorSubtract(BoxDiagPt, BoxDiagPt));
  if result then
      Exit;

  // Triangle - Box diagonal 4 intersection
  BoxDiagPt := VectorMake(aMaxExtent[0], aMinExtent[1], aMinExtent[2]);
  BoxDiagPt2 := VectorMake(aMinExtent[0], aMaxExtent[1], aMaxExtent[2]);
  VectorSubtract(BoxDiagPt, BoxDiagPt, BoxDiagDir);
  result := RayCastTriangleIntersect(BoxDiagPt, BoxDiagDir, p1, p2, p3, @iPnt);
  if result then
      result := VectorLength(VectorSubtract(BoxDiagPt, iPnt))
      < VectorLength(VectorSubtract(BoxDiagPt, BoxDiagPt));
end;

// IntersectSphereBox
//
function IntersectSphereBox(
  const SpherePos: TVector;
  const SphereRadius: single;
  const BoxMatrix: TMatrix; // Up Direction and Right must be normalized!
  // Use CubDepht, CubeHeight and CubeWidth
  // for scale TGLCube.
  const BoxScale: TAffineVector
  ; intersectPoint: PAffineVector = nil
  ; normal: PAffineVector = nil
  ; depth: PSingle = nil
  ): Boolean;

  function dDOTByColumn(const v: TAffineVector; const M: TMatrix;
    const aColumn: Integer): single;
  begin
    result := v[0] * M[0, aColumn]
      + v[1] * M[1, aColumn]
      + v[2] * M[2, aColumn];
  end;

  function dDotByRow(const v: TAffineVector;
    const M: TMatrix; const aRow: Integer): single;
  begin
    // Equal with: Result := VectorDotProduct(v, AffineVectorMake(m[aRow]));
    result := v[0] * M[aRow, 0]
      + v[1] * M[aRow, 1]
      + v[2] * M[aRow, 2];
  end;

  function dDotMatrByColumn(const v: TAffineVector;
    const M: TMatrix): TAffineVector;
  begin
    result[0] := dDOTByColumn(v, M, 0);
    result[1] := dDOTByColumn(v, M, 1);
    result[2] := dDOTByColumn(v, M, 2);
  end;

  function dDotMatrByRow(const v: TAffineVector;
    const M: TMatrix): TAffineVector;
  begin
    result[0] := dDotByRow(v, M, 0);
    result[1] := dDotByRow(v, M, 1);
    result[2] := dDotByRow(v, M, 2);
  end;

var
  tmp, l, T, p, Q, r: TAffineVector;
  FaceDistance,
    MinDistance, Depth1  : single;
  mini, i                : Integer;
  isSphereCenterInsideBox: Boolean;
begin
  // this is easy. get the sphere center `p' relative to the box, and then clip
  // that to the boundary of the box (call that point `q'). if q is on the
  // boundary of the box and |p-q| is <= sphere radius, they touch.
  // if q is inside the box, the sphere is inside the box, so set a contact
  // normal to push the sphere to the closest box face.

  p[0] := SpherePos[0] - BoxMatrix[3, 0];
  p[1] := SpherePos[1] - BoxMatrix[3, 1];
  p[2] := SpherePos[2] - BoxMatrix[3, 2];

  isSphereCenterInsideBox := True;
  for i := 0 to 2 do begin
      l[i] := 0.5 * BoxScale[i];
      T[i] := dDotByRow(p, BoxMatrix, i);
      if T[i] < -l[i] then begin
          T[i] := -l[i];
          isSphereCenterInsideBox := False;
        end
      else if T[i] > l[i] then begin
          T[i] := l[i];
          isSphereCenterInsideBox := False;
        end;
    end;

  if isSphereCenterInsideBox then begin

      MinDistance := l[0] - Abs(T[0]);
      mini := 0;
      for i := 1 to 2 do begin
          FaceDistance := l[i] - Abs(T[i]);
          if FaceDistance < MinDistance then begin
              MinDistance := FaceDistance;
              mini := i;
            end;
        end;

      if intersectPoint <> nil then
          intersectPoint^ := AffineVectorMake(SpherePos);

      if normal <> nil then begin
          tmp := NullVector;
          if T[mini] > 0 then
              tmp[mini] := 1
          else
              tmp[mini] := -1;
          normal^ := dDotMatrByRow(tmp, BoxMatrix);
        end;

      if depth <> nil then
          depth^ := MinDistance + SphereRadius;

      result := True;
    end
  else begin
      Q := dDotMatrByColumn(T, BoxMatrix);
      r := VectorSubtract(p, Q);
      Depth1 := SphereRadius - VectorLength(r);
      if Depth1 < 0 then begin
          result := False;
        end
      else begin
          if intersectPoint <> nil then
              intersectPoint^ := VectorAdd(Q, AffineVectorMake(BoxMatrix[3]));
          if normal <> nil then begin
              normal^ := VectorNormalize(r);
            end;
          if depth <> nil then
              depth^ := Depth1;
          result := True;
        end;
    end;
end;

// ExtractFrustumFromModelViewProjection
//
function ExtractFrustumFromModelViewProjection(const modelViewProj: TMatrix): TFrustum;
begin
  with result do begin
      // extract left plane
      pLeft[0] := modelViewProj[0][3] + modelViewProj[0][0];
      pLeft[1] := modelViewProj[1][3] + modelViewProj[1][0];
      pLeft[2] := modelViewProj[2][3] + modelViewProj[2][0];
      pLeft[3] := modelViewProj[3][3] + modelViewProj[3][0];
      NormalizePlane(pLeft);
      // extract top plane
      pTop[0] := modelViewProj[0][3] - modelViewProj[0][1];
      pTop[1] := modelViewProj[1][3] - modelViewProj[1][1];
      pTop[2] := modelViewProj[2][3] - modelViewProj[2][1];
      pTop[3] := modelViewProj[3][3] - modelViewProj[3][1];
      NormalizePlane(pTop);
      // extract right plane
      pRight[0] := modelViewProj[0][3] - modelViewProj[0][0];
      pRight[1] := modelViewProj[1][3] - modelViewProj[1][0];
      pRight[2] := modelViewProj[2][3] - modelViewProj[2][0];
      pRight[3] := modelViewProj[3][3] - modelViewProj[3][0];
      NormalizePlane(pRight);
      // extract bottom plane
      pBottom[0] := modelViewProj[0][3] + modelViewProj[0][1];
      pBottom[1] := modelViewProj[1][3] + modelViewProj[1][1];
      pBottom[2] := modelViewProj[2][3] + modelViewProj[2][1];
      pBottom[3] := modelViewProj[3][3] + modelViewProj[3][1];
      NormalizePlane(pBottom);
      // extract far plane
      pFar[0] := modelViewProj[0][3] - modelViewProj[0][2];
      pFar[1] := modelViewProj[1][3] - modelViewProj[1][2];
      pFar[2] := modelViewProj[2][3] - modelViewProj[2][2];
      pFar[3] := modelViewProj[3][3] - modelViewProj[3][2];
      NormalizePlane(pFar);
      // extract near plane
      pNear[0] := modelViewProj[0][3] + modelViewProj[0][2];
      pNear[1] := modelViewProj[1][3] + modelViewProj[1][2];
      pNear[2] := modelViewProj[2][3] + modelViewProj[2][2];
      pNear[3] := modelViewProj[3][3] + modelViewProj[3][2];
      NormalizePlane(pNear);
    end;
end;

// IsVolumeClipped
//
function IsVolumeClipped(const objPos: TAffineVector; const objRadius: single;
  const Frustum: TFrustum): Boolean;
var
  negRadius: single;
begin
  negRadius := -objRadius;
  result := (PlaneEvaluatePoint(Frustum.pLeft, objPos) < negRadius)
    or (PlaneEvaluatePoint(Frustum.pTop, objPos) < negRadius)
    or (PlaneEvaluatePoint(Frustum.pRight, objPos) < negRadius)
    or (PlaneEvaluatePoint(Frustum.pBottom, objPos) < negRadius)
    or (PlaneEvaluatePoint(Frustum.pNear, objPos) < negRadius)
    or (PlaneEvaluatePoint(Frustum.pFar, objPos) < negRadius);
end;

// IsVolumeClipped
//
function IsVolumeClipped(const objPos: TVector; const objRadius: single;
  const Frustum: TFrustum): Boolean;
begin
  result := IsVolumeClipped(PAffineVector(@objPos)^, objRadius, Frustum);
end;

// IsVolumeClipped
//
function IsVolumeClipped(const min, max: TAffineVector;
  const Frustum: TFrustum): Boolean;
begin
  // change box to sphere
  result := IsVolumeClipped(VectorScale(VectorAdd(min, max), 0.5),
    VectorDistance(min, max) * 0.5, Frustum);
end;

// MakeParallelProjectionMatrix
//
function MakeParallelProjectionMatrix(const plane: THmgPlane;
  const dir: TVector): TMatrix;
// Based on material from a course by William D. Shoaff (www.cs.fit.edu)
var
  dot, invDot: single;
begin
  dot := plane[0] * dir[0] + plane[1] * dir[1] + plane[2] * dir[2];
  if Abs(dot) < 1E-5 then begin
      result := IdentityHmgMatrix;
      Exit;
    end;
  invDot := 1 / dot;

  result[0][0] := (plane[1] * dir[1] + plane[2] * dir[2]) * invDot;
  result[1][0] := (-plane[1] * dir[0]) * invDot;
  result[2][0] := (-plane[2] * dir[0]) * invDot;
  result[3][0] := (-plane[3] * dir[0]) * invDot;

  result[0][1] := (-plane[0] * dir[1]) * invDot;
  result[1][1] := (plane[0] * dir[0] + plane[2] * dir[2]) * invDot;
  result[2][1] := (-plane[2] * dir[1]) * invDot;
  result[3][1] := (-plane[3] * dir[1]) * invDot;

  result[0][2] := (-plane[0] * dir[2]) * invDot;
  result[1][2] := (-plane[1] * dir[2]) * invDot;
  result[2][2] := (plane[0] * dir[0] + plane[1] * dir[1]) * invDot;
  result[3][2] := (-plane[3] * dir[2]) * invDot;

  result[0][3] := 0;
  result[1][3] := 0;
  result[2][3] := 0;
  result[3][3] := 1;
end;

// MakeShadowMatrix
//
function MakeShadowMatrix(const planePoint, planeNormal, lightPos: TVector): TMatrix;
var
  planeNormal3, dot: single;
begin
  // Find the last coefficient by back substitutions
  planeNormal3 := -(planeNormal[0] * planePoint[0]
    + planeNormal[1] * planePoint[1]
    + planeNormal[2] * planePoint[2]);
  // Dot product of plane and light position
  dot := planeNormal[0] * lightPos[0]
    + planeNormal[1] * lightPos[1]
    + planeNormal[2] * lightPos[2]
    + planeNormal3 * lightPos[3];
  // Now do the projection
  // First column
  result[0][0] := dot - lightPos[0] * planeNormal[0];
  result[1][0] := -lightPos[0] * planeNormal[1];
  result[2][0] := -lightPos[0] * planeNormal[2];
  result[3][0] := -lightPos[0] * planeNormal3;
  // Second column
  result[0][1] := -lightPos[1] * planeNormal[0];
  result[1][1] := dot - lightPos[1] * planeNormal[1];
  result[2][1] := -lightPos[1] * planeNormal[2];
  result[3][1] := -lightPos[1] * planeNormal3;
  // Third Column
  result[0][2] := -lightPos[2] * planeNormal[0];
  result[1][2] := -lightPos[2] * planeNormal[1];
  result[2][2] := dot - lightPos[2] * planeNormal[2];
  result[3][2] := -lightPos[2] * planeNormal3;
  // Fourth Column
  result[0][3] := -lightPos[3] * planeNormal[0];
  result[1][3] := -lightPos[3] * planeNormal[1];
  result[2][3] := -lightPos[3] * planeNormal[2];
  result[3][3] := dot - lightPos[3] * planeNormal3;
end;

// MakeReflectionMatrix
//
function MakeReflectionMatrix(const planePoint, planeNormal: TAffineVector): TMatrix;
var
  pv2: single;
begin
  // Precalcs
  pv2 := 2 * VectorDotProduct(planePoint, planeNormal);
  // 1st column
  result[0][0] := 1 - 2 * Sqr(planeNormal[0]);
  result[0][1] := -2 * planeNormal[0] * planeNormal[1];
  result[0][2] := -2 * planeNormal[0] * planeNormal[2];
  result[0][3] := 0;
  // 2nd column
  result[1][0] := -2 * planeNormal[1] * planeNormal[0];
  result[1][1] := 1 - 2 * Sqr(planeNormal[1]);
  result[1][2] := -2 * planeNormal[1] * planeNormal[2];
  result[1][3] := 0;
  // 3rd column
  result[2][0] := -2 * planeNormal[2] * planeNormal[0];
  result[2][1] := -2 * planeNormal[2] * planeNormal[1];
  result[2][2] := 1 - 2 * Sqr(planeNormal[2]);
  result[2][3] := 0;
  // 4th column
  result[3][0] := pv2 * planeNormal[0];
  result[3][1] := pv2 * planeNormal[1];
  result[3][2] := pv2 * planeNormal[2];
  result[3][3] := 1;
end;

// PackRotationMatrix
//
function PackRotationMatrix(const mat: TMatrix): TPackedRotationMatrix;
var
  Q: TQuaternion;
const
  cFact: single = 32767;
begin
  Q := QuaternionFromMatrix(mat);
  NormalizeQuaternion(Q);
  if Q.RealPart < 0 then begin
      result[0] := Round(-Q.ImagPart[0] * cFact);
      result[1] := Round(-Q.ImagPart[1] * cFact);
      result[2] := Round(-Q.ImagPart[2] * cFact);
    end
  else begin
      result[0] := Round(Q.ImagPart[0] * cFact);
      result[1] := Round(Q.ImagPart[1] * cFact);
      result[2] := Round(Q.ImagPart[2] * cFact);
    end;
end;

// UnPackRotationMatrix
//
function UnPackRotationMatrix(const packedMatrix: TPackedRotationMatrix): TMatrix;
var
  Q: TQuaternion;
const
  cFact: single = 1 / 32767;
begin
  Q.ImagPart[0] := packedMatrix[0] * cFact;
  Q.ImagPart[1] := packedMatrix[1] * cFact;
  Q.ImagPart[2] := packedMatrix[2] * cFact;
  Q.RealPart := 1 - VectorNorm(Q.ImagPart);
  if Q.RealPart < 0 then
      Q.RealPart := 0
  else
      Q.RealPart := Sqrt(Q.RealPart);
  result := QuaternionToMatrix(Q);
end;

// BarycentricCoordinates
//
function BarycentricCoordinates(const V1, V2, V3, p: TAffineVector; var u, v: single): Boolean;
var
  a1, a2       : Integer;
  n, e1, e2, pt: TAffineVector;
begin
  // calculate edges
  VectorSubtract(V1, V3, e1);
  VectorSubtract(V2, V3, e2);

  // calculate p relative to v3
  VectorSubtract(p, V3, pt);

  // find the dominant axis
  n := VectorCrossProduct(e1, e2);
  AbsVector(n);
  a1 := 0;
  if n[1] > n[a1] then
      a1 := 1;
  if n[2] > n[a1] then
      a1 := 2;

  // use dominant axis for projection
  case a1 of
    0: begin
        a1 := 1;
        a2 := 2;
      end;
    1: begin
        a1 := 0;
        a2 := 2;
      end;
    else // 2:
      a1 := 0;
      a2 := 1;
  end;

  // solve for u and v
  u := (pt[a2] * e2[a1] - pt[a1] * e2[a2]) / (e1[a2] * e2[a1] - e1[a1] * e2[a2]);
  v := (pt[a2] * e1[a1] - pt[a1] * e1[a2]) / (e2[a2] * e1[a1] - e2[a1] * e1[a2]);

  result := (u >= 0) and (v >= 0) and (u + v <= 1);
end;

{ ***************************************************************************** }

// VectorMake functions
// 2x
function Vector2fMake(const x, y: single): TVector2f;
begin
  result[0] := x;
  result[1] := y;
end;

function Vector2iMake(const x, y: Integer): TVector2i;
begin
  result[0] := x;
  result[1] := y;
end;

function Vector2sMake(const x, y: smallint): TVector2s;
begin
  result[0] := x;
  result[1] := y;
end;

function Vector2dMake(const x, y: double): TVector2d;
begin
  result[0] := x;
  result[1] := y;
end;

function Vector2bMake(const x, y: byte): TVector2b;
begin
  result[0] := x;
  result[1] := y;
end;

// **************

function Vector2fMake(const Vector: TVector3f): TVector2f;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

function Vector2iMake(const Vector: TVector3i): TVector2i;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

function Vector2sMake(const Vector: TVector3s): TVector2s;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

function Vector2dMake(const Vector: TVector3d): TVector2d;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

function Vector2bMake(const Vector: TVector3b): TVector2b;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

// **********

function Vector2fMake(const Vector: TVector4f): TVector2f;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

function Vector2iMake(const Vector: TVector4i): TVector2i;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

function Vector2sMake(const Vector: TVector4s): TVector2s;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

function Vector2dMake(const Vector: TVector4d): TVector2d;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

function Vector2bMake(const Vector: TVector4b): TVector2b;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
end;

{ ***************************************************************************** }

// 3x
function Vector3fMake(const x, y, z: single): TVector3f;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

function Vector3iMake(const x, y, z: Integer): TVector3i;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

function Vector3sMake(const x, y, z: smallint): TVector3s;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

function Vector3dMake(const x, y, z: double): TVector3d;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

function Vector3bMake(const x, y, z: byte): TVector3b;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

// *******

function Vector3fMake(const Vector: TVector2f; const z: single): TVector3f;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
end;

function Vector3iMake(const Vector: TVector2i; const z: Integer): TVector3i;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
end;

function Vector3sMake(const Vector: TVector2s; const z: smallint): TVector3s;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
end;

function Vector3dMake(const Vector: TVector2d; const z: double): TVector3d;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
end;

function Vector3bMake(const Vector: TVector2b; const z: byte): TVector3b;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
end;

// *******

function Vector3fMake(const Vector: TVector4f): TVector3f;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
end;

function Vector3iMake(const Vector: TVector4i): TVector3i;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
end;

function Vector3sMake(const Vector: TVector4s): TVector3s;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
end;

function Vector3dMake(const Vector: TVector4d): TVector3d;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
end;

function Vector3bMake(const Vector: TVector4b): TVector3b;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
end;

{ ***************************************************************************** }

// 4x
function Vector4fMake(const x, y, z, w: single): TVector4f;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
  result[3] := w;
end;

function Vector4iMake(const x, y, z, w: Integer): TVector4i;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
  result[3] := w;
end;

function Vector4sMake(const x, y, z, w: smallint): TVector4s;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
  result[3] := w;
end;

function Vector4dMake(const x, y, z, w: double): TVector4d;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
  result[3] := w;
end;

function Vector4bMake(const x, y, z, w: byte): TVector4b;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
  result[3] := w;
end;

// ********

function Vector4fMake(const Vector: TVector3f; const w: single): TVector4f;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
  result[3] := w;
end;

function Vector4iMake(const Vector: TVector3i; const w: Integer): TVector4i;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
  result[3] := w;
end;

function Vector4sMake(const Vector: TVector3s; const w: smallint): TVector4s;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
  result[3] := w;
end;

function Vector4dMake(const Vector: TVector3d; const w: double): TVector4d;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
  result[3] := w;
end;

function Vector4bMake(const Vector: TVector3b; const w: byte): TVector4b;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
  result[3] := w;
end;

// *******

function Vector4fMake(const Vector: TVector2f; const z: single; const w: single): TVector4f;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
  result[3] := w;
end;

function Vector4iMake(const Vector: TVector2i; const z: Integer; const w: Integer): TVector4i;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
  result[3] := w;
end;

function Vector4sMake(const Vector: TVector2s; const z: smallint; const w: smallint): TVector4s;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
  result[3] := w;
end;

function Vector4dMake(const Vector: TVector2d; const z: double; const w: double): TVector4d;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
  result[3] := w;
end;

function Vector4bMake(const Vector: TVector2b; const z: byte; const w: byte): TVector4b;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := z;
  result[3] := w;
end;

{ ***************************************************************************** }

// 2
function VectorEquals(const V1, V2: TVector2f): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]);
end;

function VectorEquals(const V1, V2: TVector2i): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]);
end;

function VectorEquals(const V1, V2: TVector2d): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]);
end;

function VectorEquals(const V1, V2: TVector2s): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]);
end;

function VectorEquals(const V1, V2: TVector2b): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]);
end;

{ ***************************************************************************** }

// 3
function VectorEquals(const V1, V2: TVector3i): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]);
end;

function VectorEquals(const V1, V2: TVector3d): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]);
end;

function VectorEquals(const V1, V2: TVector3s): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]);
end;

function VectorEquals(const V1, V2: TVector3b): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]);
end;

{ ***************************************************************************** }

// 4
function VectorEquals(const V1, V2: TVector4i): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]) and (V1[3] = V2[3]);
end;

function VectorEquals(const V1, V2: TVector4d): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]) and (V1[3] = V2[3]);
end;

function VectorEquals(const V1, V2: TVector4s): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]) and (V1[3] = V2[3]);
end;

function VectorEquals(const V1, V2: TVector4b): Boolean;
begin
  result := (V1[0] = V2[0]) and (V1[1] = V2[1]) and (V1[2] = V2[2]) and (V1[3] = V2[3]);
end;

{ ***************************************************************************** }

// 3x3f
function MatrixEquals(const Matrix1, Matrix2: TMatrix3f): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]);
end;

// 3x3i
function MatrixEquals(const Matrix1, Matrix2: TMatrix3i): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]);
end;

// 3x3d
function MatrixEquals(const Matrix1, Matrix2: TMatrix3d): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]);
end;

// 3x3s
function MatrixEquals(const Matrix1, Matrix2: TMatrix3s): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]);
end;

// 3x3b
function MatrixEquals(const Matrix1, Matrix2: TMatrix3b): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]);
end;

{ ***************************************************************************** }

// 4x4f
function MatrixEquals(const Matrix1, Matrix2: TMatrix4f): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]) and
    VectorEquals(Matrix1[3], Matrix2[3]);
end;

// 4x4i
function MatrixEquals(const Matrix1, Matrix2: TMatrix4i): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]) and
    VectorEquals(Matrix1[3], Matrix2[3]);
end;

// 4x4d
function MatrixEquals(const Matrix1, Matrix2: TMatrix4d): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]) and
    VectorEquals(Matrix1[3], Matrix2[3]);
end;

// 4x4s
function MatrixEquals(const Matrix1, Matrix2: TMatrix4s): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]) and
    VectorEquals(Matrix1[3], Matrix2[3]);
end;

// 4x4b
function MatrixEquals(const Matrix1, Matrix2: TMatrix4b): Boolean;
begin
  result := VectorEquals(Matrix1[0], Matrix2[0]) and
    VectorEquals(Matrix1[1], Matrix2[1]) and
    VectorEquals(Matrix1[2], Matrix2[2]) and
    VectorEquals(Matrix1[3], Matrix2[3]);
end;

{ ***************************************************************************** }

// Vector comparison functions:
// 3f
function VectorMoreThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedVector[0]) and
    (SourceVector[1] > ComparedVector[1]) and
    (SourceVector[2] > ComparedVector[2]);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedVector[0]) and
    (SourceVector[1] >= ComparedVector[1]) and
    (SourceVector[2] >= ComparedVector[2]);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedVector[0]) and
    (SourceVector[1] < ComparedVector[1]) and
    (SourceVector[2] < ComparedVector[2]);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedVector[0]) and
    (SourceVector[1] <= ComparedVector[1]) and
    (SourceVector[2] <= ComparedVector[2]);
end;

// 4f
function VectorMoreThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedVector[0]) and
    (SourceVector[1] > ComparedVector[1]) and
    (SourceVector[2] > ComparedVector[2]) and
    (SourceVector[3] > ComparedVector[3]);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedVector[0]) and
    (SourceVector[1] >= ComparedVector[1]) and
    (SourceVector[2] >= ComparedVector[2]) and
    (SourceVector[3] >= ComparedVector[3]);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedVector[0]) and
    (SourceVector[1] < ComparedVector[1]) and
    (SourceVector[2] < ComparedVector[2]) and
    (SourceVector[3] < ComparedVector[3]);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedVector[0]) and
    (SourceVector[1] <= ComparedVector[1]) and
    (SourceVector[2] <= ComparedVector[2]) and
    (SourceVector[3] <= ComparedVector[3]);
end;

// 3i
// Vector comparison functions:
function VectorMoreThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedVector[0]) and
    (SourceVector[1] > ComparedVector[1]) and
    (SourceVector[2] > ComparedVector[2]);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedVector[0]) and
    (SourceVector[1] >= ComparedVector[1]) and
    (SourceVector[2] >= ComparedVector[2]);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedVector[0]) and
    (SourceVector[1] < ComparedVector[1]) and
    (SourceVector[2] < ComparedVector[2]);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedVector[0]) and
    (SourceVector[1] <= ComparedVector[1]) and
    (SourceVector[2] <= ComparedVector[2]);
end;

// 4i
function VectorMoreThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedVector[0]) and
    (SourceVector[1] > ComparedVector[1]) and
    (SourceVector[2] > ComparedVector[2]) and
    (SourceVector[3] > ComparedVector[3]);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedVector[0]) and
    (SourceVector[1] >= ComparedVector[1]) and
    (SourceVector[2] >= ComparedVector[2]) and
    (SourceVector[3] >= ComparedVector[3]);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedVector[0]) and
    (SourceVector[1] < ComparedVector[1]) and
    (SourceVector[2] < ComparedVector[2]) and
    (SourceVector[3] < ComparedVector[3]);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedVector[0]) and
    (SourceVector[1] <= ComparedVector[1]) and
    (SourceVector[2] <= ComparedVector[2]) and
    (SourceVector[3] <= ComparedVector[3]);
end;

// 3s
// Vector comparison functions:
function VectorMoreThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedVector[0]) and
    (SourceVector[1] > ComparedVector[1]) and
    (SourceVector[2] > ComparedVector[2]);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedVector[0]) and
    (SourceVector[1] >= ComparedVector[1]) and
    (SourceVector[2] >= ComparedVector[2]);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedVector[0]) and
    (SourceVector[1] < ComparedVector[1]) and
    (SourceVector[2] < ComparedVector[2]);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedVector[0]) and
    (SourceVector[1] <= ComparedVector[1]) and
    (SourceVector[2] <= ComparedVector[2]);
end;

// 4s
function VectorMoreThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedVector[0]) and
    (SourceVector[1] > ComparedVector[1]) and
    (SourceVector[2] > ComparedVector[2]) and
    (SourceVector[3] > ComparedVector[3]);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedVector[0]) and
    (SourceVector[1] >= ComparedVector[1]) and
    (SourceVector[2] >= ComparedVector[2]) and
    (SourceVector[3] >= ComparedVector[3]);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedVector[0]) and
    (SourceVector[1] < ComparedVector[1]) and
    (SourceVector[2] < ComparedVector[2]) and
    (SourceVector[3] < ComparedVector[3]);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedVector[0]) and
    (SourceVector[1] <= ComparedVector[1]) and
    (SourceVector[2] <= ComparedVector[2]) and
    (SourceVector[3] <= ComparedVector[3]);
end;

// ComparedNumber
// 3f
function VectorMoreThen(const SourceVector: TVector3f; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedNumber) and
    (SourceVector[1] > ComparedNumber) and
    (SourceVector[2] > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector3f; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedNumber) and
    (SourceVector[1] >= ComparedNumber) and
    (SourceVector[2] >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector3f; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedNumber) and
    (SourceVector[1] < ComparedNumber) and
    (SourceVector[2] < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector3f; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedNumber) and
    (SourceVector[1] <= ComparedNumber) and
    (SourceVector[2] <= ComparedNumber);
end;

// 4f
function VectorMoreThen(const SourceVector: TVector4f; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedNumber) and
    (SourceVector[1] > ComparedNumber) and
    (SourceVector[2] > ComparedNumber) and
    (SourceVector[3] > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector4f; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedNumber) and
    (SourceVector[1] >= ComparedNumber) and
    (SourceVector[2] >= ComparedNumber) and
    (SourceVector[3] >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector4f; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedNumber) and
    (SourceVector[1] < ComparedNumber) and
    (SourceVector[2] < ComparedNumber) and
    (SourceVector[3] < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector4f; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedNumber) and
    (SourceVector[1] <= ComparedNumber) and
    (SourceVector[2] <= ComparedNumber) and
    (SourceVector[3] <= ComparedNumber);
end;

// 3i
function VectorMoreThen(const SourceVector: TVector3i; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedNumber) and
    (SourceVector[1] > ComparedNumber) and
    (SourceVector[2] > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector3i; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedNumber) and
    (SourceVector[1] >= ComparedNumber) and
    (SourceVector[2] >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector3i; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedNumber) and
    (SourceVector[1] < ComparedNumber) and
    (SourceVector[2] < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector3i; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedNumber) and
    (SourceVector[1] <= ComparedNumber) and
    (SourceVector[2] <= ComparedNumber);
end;

// 4i
function VectorMoreThen(const SourceVector: TVector4i; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedNumber) and
    (SourceVector[1] > ComparedNumber) and
    (SourceVector[2] > ComparedNumber) and
    (SourceVector[3] > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector4i; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedNumber) and
    (SourceVector[1] >= ComparedNumber) and
    (SourceVector[2] >= ComparedNumber) and
    (SourceVector[3] >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector4i; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedNumber) and
    (SourceVector[1] < ComparedNumber) and
    (SourceVector[2] < ComparedNumber) and
    (SourceVector[3] < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector4i; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedNumber) and
    (SourceVector[1] <= ComparedNumber) and
    (SourceVector[2] <= ComparedNumber) and
    (SourceVector[3] <= ComparedNumber);
end;

// 3s
function VectorMoreThen(const SourceVector: TVector3s; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedNumber) and
    (SourceVector[1] > ComparedNumber) and
    (SourceVector[2] > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector3s; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedNumber) and
    (SourceVector[1] >= ComparedNumber) and
    (SourceVector[2] >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector3s; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedNumber) and
    (SourceVector[1] < ComparedNumber) and
    (SourceVector[2] < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector3s; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedNumber) and
    (SourceVector[1] <= ComparedNumber) and
    (SourceVector[2] <= ComparedNumber);
end;

// 4s
function VectorMoreThen(const SourceVector: TVector4s; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] > ComparedNumber) and
    (SourceVector[1] > ComparedNumber) and
    (SourceVector[2] > ComparedNumber) and
    (SourceVector[3] > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector4s; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] >= ComparedNumber) and
    (SourceVector[1] >= ComparedNumber) and
    (SourceVector[2] >= ComparedNumber) and
    (SourceVector[3] >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector4s; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] < ComparedNumber) and
    (SourceVector[1] < ComparedNumber) and
    (SourceVector[2] < ComparedNumber) and
    (SourceVector[3] < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector4s; const ComparedNumber: single): Boolean; overload;
begin
  result := (SourceVector[0] <= ComparedNumber) and
    (SourceVector[1] <= ComparedNumber) and
    (SourceVector[2] <= ComparedNumber) and
    (SourceVector[3] <= ComparedNumber);
end;

{ : Determine if 2 rectanges intersect. }
function RectanglesIntersect(const ACenterOfRect1, ACenterOfRect2, ASizeOfRect1, ASizeOfRect2: TVector2f): Boolean;
begin
  result := (Abs(ACenterOfRect1[0] - ACenterOfRect2[0]) < (ASizeOfRect1[0] + ASizeOfRect2[0]) / 2) and
    (Abs(ACenterOfRect1[1] - ACenterOfRect2[1]) < (ASizeOfRect1[1] + ASizeOfRect2[1]) / 2);
end;

{ : Determine if BigRect completely contains SmallRect. }
function RectangleContains(const ACenterOfBigRect1, ACenterOfSmallRect2,
  ASizeOfBigRect1, ASizeOfSmallRect2: TVector2f; const AEps: single = 0.0): Boolean;
begin
  result := (Abs(ACenterOfBigRect1[0] - ACenterOfSmallRect2[0]) + ASizeOfSmallRect2[0] / 2 - ASizeOfBigRect1[0] / 2 < AEps) and
    (Abs(ACenterOfBigRect1[1] - ACenterOfSmallRect2[1]) + ASizeOfSmallRect2[1] / 2 - ASizeOfBigRect1[1] / 2 < AEps);
end;

function MoveObjectAround(const AMovingObjectPosition, AMovingObjectUp, ATargetPosition: TVector;
  pitchDelta, turnDelta: single): TVector;
var
  originalT2C, normalT2C, normalCameraRight: TVector;
  pitchNow, dist                           : single;
begin
  // normalT2C points away from the direction the camera is looking
  originalT2C := VectorSubtract(AMovingObjectPosition,
    ATargetPosition);
  SetVector(normalT2C, originalT2C);
  dist := VectorLength(normalT2C);
  NormalizeVector(normalT2C);
  // normalRight points to the camera's right
  // the camera is pitching around this axis.
  normalCameraRight := VectorCrossProduct(AMovingObjectUp, normalT2C);
  if VectorLength(normalCameraRight) < 0.001 then
      SetVector(normalCameraRight, XVector) // arbitrary vector
  else
      NormalizeVector(normalCameraRight);
  // calculate the current pitch.
  // 0 is looking down and PI is looking up
  pitchNow := GeometryLib.ArcCos(VectorDotProduct(AMovingObjectUp, normalT2C));
  pitchNow := ClampValue(pitchNow + GeometryLib.DegToRad(pitchDelta), 0 + 0.025, PI -
    0.025);
  // create a new vector pointing up and then rotate it down
  // into the new position
  SetVector(normalT2C, AMovingObjectUp);
  RotateVector(normalT2C, normalCameraRight, -pitchNow);
  RotateVector(normalT2C, AMovingObjectUp, -GeometryLib.DegToRad(turnDelta));
  ScaleVector(normalT2C, dist);
  result := VectorAdd(AMovingObjectPosition, VectorSubtract(normalT2C,
    originalT2C));
end;

{ : Calcualtes Angle between 2 Vectors: (A-CenterPoint) and (B-CenterPoint). In radians. }
function AngleBetweenVectors(const a, b, ACenterPoint: TVector): single;
begin
  result := GeometryLib.ArcCos(VectorAngleCosine(
    VectorNormalize(VectorSubtract(a, ACenterPoint)),
    VectorNormalize(VectorSubtract(b, ACenterPoint))));
end;

{ : Calcualtes Angle between 2 Vectors: (A-CenterPoint) and (B-CenterPoint). In radians. }
function AngleBetweenVectors(const a, b, ACenterPoint: TAffineVector): single;
begin
  result := GeometryLib.ArcCos(VectorAngleCosine(
    VectorNormalize(VectorSubtract(a, ACenterPoint)),
    VectorNormalize(VectorSubtract(b, ACenterPoint))));
end;

{ : AOriginalPosition - Object initial position.
  ACenter - some point, from which is should be distanced.

  ADistance + AFromCenterSpot - distance, which object should keep from ACenter
  or
  ADistance + not AFromCenterSpot - distance, which object should shift from his current position away from center.
}
function ShiftObjectFromCenter(const AOriginalPosition: TVector;
  const ACenter: TVector; const ADistance: single; const AFromCenterSpot: Boolean): TVector;
var
  lDirection: TVector;
begin
  lDirection := VectorNormalize(VectorSubtract(AOriginalPosition, ACenter));
  if AFromCenterSpot then
      result := VectorAdd(ACenter, VectorScale(lDirection, ADistance))
  else
      result := VectorAdd(AOriginalPosition, VectorScale(lDirection, ADistance))
end;

{ : AOriginalPosition - Object initial position.
  ACenter - some point, from which is should be distanced.

  ADistance + AFromCenterSpot - distance, which object should keep from ACenter
  or
  ADistance + not AFromCenterSpot - distance, which object should shift from his current position away from center.
}
function ShiftObjectFromCenter(const AOriginalPosition: TAffineVector;
  const ACenter: TAffineVector; const ADistance: single; const AFromCenterSpot: Boolean): TAffineVector;
var
  lDirection: TAffineVector;
begin
  lDirection := VectorNormalize(VectorSubtract(AOriginalPosition, ACenter));
  if AFromCenterSpot then
      result := VectorAdd(ACenter, VectorScale(lDirection, ADistance))
  else
      result := VectorAdd(AOriginalPosition, VectorScale(lDirection, ADistance))
end;

// --------------------------------------------------------------
// --------------------------------------------------------------
// --------------------------------------------------------------
initialization

// --------------------------------------------------------------
// --------------------------------------------------------------
// --------------------------------------------------------------

// SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

end.
