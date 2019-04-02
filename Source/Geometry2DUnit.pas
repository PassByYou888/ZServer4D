{ ****************************************************************************** }
{ * geometry 2D library writen by QQ 600585@qq.com                             * }
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

unit Geometry2DUnit;

{$INCLUDE zDefine.inc}

interface

uses Classes, SysUtils, Math, Types, CoreClasses, MemoryStream64;

type
  TGeoFloat = Single;
  TGeoInt = Integer;
  TVec2 = array [0 .. 1] of TGeoFloat;
  PVec2 = ^TVec2;
  T2DPoint = TVec2;
  P2DPoint = PVec2;
  TPoint2 = T2DPoint;

  TArrayVec2 = array of TVec2;
  PArrayVec2 = ^TArrayVec2;

  TArray2DPoint = TArrayVec2;
  PArray2DPoint = PArrayVec2;

  TRectV2 = array [0 .. 1] of TVec2;
  PRectV2 = ^TRectV2;
  TRect2 = TRectV2;
  TRect2D = TRectV2;

  TVert2 = record
    Render: TVec2;
    Sampler: TVec2;
  end;

  PVert2 = ^TVert2;

  TTriangle = array [0 .. 2] of TVert2;
  PTriangle = ^TTriangle;

  TGeoFloatArray = array of TGeoFloat;
  PGeoFloatArray = ^TGeoFloatArray;

{$IFDEF FPC}

  TPointf = record
    x: TGeoFloat;
    y: TGeoFloat;
  end;

  PPointf = ^TPointf;

  TRectf = record
    case Integer of
      0:
        (Left, Top, Right, Bottom: TGeoFloat);
      1:
        (TopLeft, BottomRight: TPointf);
  end;

  PRectf = ^TRectf;

function Pointf(x, y: TGeoFloat): TPointf;
function Rectf(Left, Top, Right, Bottom: TGeoFloat): TRectf;
{$ENDIF}


const
  XPoint: T2DPoint = (1, 0);
  YPoint: T2DPoint = (0, 1);
  NULLPoint: T2DPoint = (0, 0);
  NULLVec2: T2DPoint = (0, 0);
  ZeroPoint: T2DPoint = (0, 0);
  ZeroVec2: T2DPoint = (0, 0);
  NULLRect: TRectV2 = ((0, 0), (0, 0));
  ZeroRect: TRectV2 = ((0, 0), (0, 0));
  NULLRectV2: TRectV2 = ((0, 0), (0, 0));
  ZeroRectV2: TRectV2 = ((0, 0), (0, 0));

const
  RightHandSide = -1;
  LeftHandSide = +1;
  CollinearOrientation = 0;
  AboveOrientation = +1;
  BelowOrientation = -1;
  CoplanarOrientation = 0;

function fabs(const v: Single): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function fabs(const v: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Range(const v, minv, maxv: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Clamp(const v, minv, maxv: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MaxF(const v1, v2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MinF(const v1, v2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeVec2(const x, y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeVec2(const x, y: Integer): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakePoint(const x, y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakePoint(const x, y: Integer): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakePoint(const pt: TVec2): TPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Point2Point(const pt: TVec2): TPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Point2Pointf(const pt: TVec2): TPointf; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointMake(const x, y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointMake(const pt: TPoint): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointMake(const pt: TPointf): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Make2DPoint(const x, y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Make2DPoint(const x, y: Integer): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Make2DPoint(const pt: TPoint): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Make2DPoint(const pt: TPointf): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function vec2(const f: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function vec2(const x, y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function vec2(const x, y: Integer): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function vec2(const x, y: Int64): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function vec2(const pt: TPoint): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function vec2(const pt: TPointf): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function RoundVec2(const v: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function MakePointf(const pt: TVec2): TPointf; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function IsZero(const v: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function IsZero(const pt: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function IsZero(const r: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function IsNan(const pt: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function IsNan(const x, y: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function HypotX(const x, y: Extended): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function PointNorm(const v: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointNegate(const v: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Vec2Norm(const v: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Negate(const v: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function vec2Inv(const v: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure SetVec2(var v: TVec2; const vSrc: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Vec2Add(const v1, v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Add(const v1: TVec2; v2: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Add(const v1: TVec2; x, y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Add(const v1: TGeoFloat; v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Add(const v1: TArrayVec2; v2: TVec2): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Add(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Vec2Sub(const v1, v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Sub(const v1: TVec2; v2: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Sub(const v1: TGeoFloat; v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Sub(const v1: TArrayVec2; v2: TVec2): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Sub(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Vec2Mul(const v1, v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1, v2: TVec2; const v3: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1, v2: TVec2; const v3, v4: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1, v2, v3: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1, v2, v3, v4: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1: TVec2; const v2: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1: TVec2; const v2, v3: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1: TVec2; const v2, v3, v4: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1: TGeoFloat; const v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1: TArrayVec2; v2: TVec2): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Mul(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Vec2Div(const v1: TVec2; const v2: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Div(const v1, v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Div(const v1: TGeoFloat; const v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function PointNormalize(const v: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Normalize(const v: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function PointLength(const v: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Length(const v: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

procedure PointScale(var v: TVec2; factor: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointDotProduct(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Distance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Distance(const x1, y1, z1, x2, y2, z2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointDistance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointDistance(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Distance(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointLayDistance(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function SqrDistance(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointLerp(const v1, v2: TVec2; t: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointLerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Lerp(const v1, v2: TVec2; t: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2LerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure SwapPoint(var v1, v2: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Pow(v: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Pow(const v, n: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MiddleVec2(const pt1, pt2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function IsEqual(const Val1, Val2, Epsilon: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function IsEqual(const Val1, Val2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function IsEqual(const Val1, Val2: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function IsEqual(const Val1, Val2: TVec2; Epsilon: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function IsEqual(const Val1, Val2: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function NotEqual(const Val1, Val2, Epsilon: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function NotEqual(const Val1, Val2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function NotEqual(const Val1, Val2: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function LessThanOrEqual(const Val1, Val2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function GreaterThanOrEqual(const Val1, Val2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function GetEquilateralTriangleCen(pt1, pt2: TVec2): TVec2; overload;

procedure Rotate(RotAng: TGeoFloat; const x, y: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Rotate(const RotAng: TGeoFloat; const Point: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function NormalizeDegAngle(const Angle: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

// axis to pt angle
function PointAngle(const axis, pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Angle(const axis, pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
// null point to pt angle
function PointAngle(const pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Angle(const pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function AngleDistance(const s, a: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointRotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointRotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Rotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Vec2Rotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function CircleInCircle(const cp1, cp2: TVec2; const r1, r2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function CircleInRect(const cp: TVec2; const radius: TGeoFloat; r: TRectV2): Boolean;
function PointInRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointInRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointInRect(const pt: TVec2; const r: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointInRect(const Px, Py: TGeoFloat; const r: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectToRectIntersect(const r1, r2: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectToRectIntersect(const r1, r2: TRect): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectToRectIntersect(const r1, r2: TRectf): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectWithinRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectWithinRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectWithinRect(const r1, r2: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectWithinRect(const r1, r2: TRect): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function MakeRectV2(const x, y, radius: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRectV2(const p1, p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRectV2(const x, y: TGeoFloat; const p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRectV2(const r: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRectV2(const r: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function RectV2(const x, y, radius: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectV2(const p1, p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectV2(const p1, p2: TPointf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectV2(const x, y: TGeoFloat; const p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectV2(const r: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectV2(const r: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectV2(const r: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function MakeRect(const x, y, radius: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRect(const x1, y1, x2, y2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRect(const p1, p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRect(const r: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRect(const r: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Rect2Rect(const r: TRectV2): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Rect2Rect(const r: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function RectMake(const x, y, radius: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectMake(const x1, y1, x2, y2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectMake(const p1, p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectMake(const r: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectMake(const r: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function RectAdd(const r: TRectV2; pt: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectAdd(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectSub(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectSub(const r: TRectV2; pt: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectMul(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectMul(const r1: TRectV2; v2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectMul(const r1: TRectV2; r2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectDiv(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectDiv(const r1: TRectV2; f2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectOffset(const r: TRectV2; Offset: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectSizeLerp(const r: TRectV2; const rSizeLerp: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectCenScale(const r: TRectV2; const rSizeScale: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectEndge(const r: TRectV2; const endge: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectEndge(const r: TRectV2; const endge: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectCentre(const r: TRectV2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectCentre(const r: TRect): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectCentre(const r: TRectf): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Vec2TransformToDest(const sour, dest: TRectV2; sour_sub: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectTransformToDest(const sour, dest, sour_sub: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectTransformToDest(const sour, dest: TRectV2; const sour_sub: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectTransformToDest(const sour, dest: TRectV2; const sour_sub: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function RectScaleSpace(const r: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RectScaleSpace(const r: TRect; const SS_width, SS_height: Integer): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function CalibrationRectInRect(const r, Area: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function CalibrationRectInRect(const r, Area: TRect): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

procedure FixRect(var Left, Top, Right, Bottom: Integer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FixRect(var Left, Top, Right, Bottom: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FixRect(r: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FixRect(r: TRect): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FixedRect(var Left, Top, Right, Bottom: Integer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FixedRect(var Left, Top, Right, Bottom: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FixedRect(r: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FixedRect(r: TRect): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ForwardRect(var Left, Top, Right, Bottom: Integer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ForwardRect(var Left, Top, Right, Bottom: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ForwardRect(r: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ForwardRect(r: TRect): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MakeRect(const r: TRectV2): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MakeRectf(const r: TRectV2): TRectf; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function RectWidth(const r: TRectV2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectHeight(const r: TRectV2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectWidth(const r: TRect): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectHeight(const r: TRect): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectWidth(const r: TRectf): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectHeight(const r: TRectf): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectArea(const r: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectSize(const r: TRectV2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectFit(const r, b: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectFit(const width, height: TGeoFloat; const b: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BoundRect(const buff: TArrayVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function BoundRect(const p1, p2, p3: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function BoundRect(const p1, p2, p3, p4: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function BoundRect(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function BuffCentroid(const buff: TArrayVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function BuffCentroid(const p1, p2, p3, p4: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function FastRamerDouglasPeucker(var Points: TArrayVec2; Epsilon: TGeoFloat): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FastVertexReduction(Points: TArrayVec2; Epsilon: TGeoFloat; var output: TArrayVec2);

function Clip(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out Cx1, Cy1, Cx2, Cy2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Clip(const r1, r2: TRectV2; out r3: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Orientation(const x1, y1, x2, y2, Px, Py: TGeoFloat): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Orientation(const x1, y1, z1, x2, y2, z2, x3, y3, z3, Px, Py, Pz: TGeoFloat): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Coplanar(const x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function SimpleIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function SimpleIntersect(const Point1, Point2, Point3, Point4: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out ix, iy: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Intersect(const pt1, pt2, pt3, pt4: TVec2; out pt: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Intersect(const pt1, pt2, pt3, pt4: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function PointInCircle(const pt, cp: TVec2; radius: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function PointInTriangle(const Px, Py, x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure BuildSinCosCache(const oSin, oCos: PGeoFloatArray; const b, E: TGeoFloat);

procedure ClosestPointOnSegmentFromPoint(const x1, y1, x2, y2, Px, Py: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function ClosestPointOnSegmentFromPoint(const lb, le, pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function MinimumDistanceFromPointToLine(const Px, Py, x1, y1, x2, y2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function Quadrant(const Angle: TGeoFloat): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

procedure ProjectPoint(const Srcx, Srcy, Dstx, Dsty, Dist: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint(const Srcx, Srcy, Srcz, Dstx, Dsty, Dstz, Dist: TGeoFloat; out Nx, Ny, Nz: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint(const Px, Py, Angle, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint0(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint45(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint90(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint135(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint180(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint225(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint270(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
procedure ProjectPoint315(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function ProjectPoint0(const Point: TVec2; const Distance: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function ProjectPoint45(const Point: TVec2; const Distance: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function ProjectPoint90(const Point: TVec2; const Distance: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function ProjectPoint135(const Point: TVec2; const Distance: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function ProjectPoint180(const Point: TVec2; const Distance: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function ProjectPoint225(const Point: TVec2; const Distance: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function ProjectPoint270(const Point: TVec2; const Distance: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function ProjectPoint315(const Point: TVec2; const Distance: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function GetCicleRadiusInPolyEndge(r: TGeoFloat; PolySlices: Integer): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure Circle2LineIntersectionPoint(const lb, le, cp: TVec2; const radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: Integer; out pt1, pt2: TVec2);

procedure Circle2CircleIntersectionPoint(const cp1, cp2: TVec2; const r1, r2: TGeoFloat; out Point1, Point2: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

// circle collision Detect
function Detect_Circle2Circle(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function CircleCollision(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function Detect_Circle2CirclePoint(const p1, p2: TVec2; const r1, r2: TGeoFloat; out op1, op2: TVec2): Boolean;
// circle 2 line collision
function Detect_Circle2Line(const cp: TVec2; const r: TGeoFloat; const lb, le: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

type
  TVec2List = class;
  TPoly = class;
  T2DLineList = class;

  TVec2List = class(TCoreClassPersistent)
  private
    FList: TCoreClassList;
    FUserData: Pointer;
    FUserObject: TCoreClassObject;

    function GetPoints(index: Integer): PVec2;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const x, y: TGeoFloat); overload;
    procedure Add(const pt: TVec2); overload;
    procedure Add(pt: TPoint); overload;
    procedure Add(pt: TPointf); overload;
    procedure Add(v2l: TVec2List); overload;
    procedure Add(r: TRectV2); overload;
    procedure Add(r: TRect); overload;
    procedure Add(r: TRectf); overload;
    procedure AddSubdivision(nbCount: Integer; pt: TVec2); overload;
    procedure AddSubdivisionWithDistance(avgDist: TGeoFloat; pt: TVec2); overload;
    procedure Insert(idx: Integer; x, y: TGeoFloat); overload;
    procedure Delete(idx: Integer); overload;
    procedure Remove(p: PVec2);
    procedure Clear; overload;
    function Count: Integer; overload;
    procedure FixedSameError;

    procedure Assign(Source: TCoreClassPersistent); override;
    procedure AssignFromArrayV2(arry: TArrayVec2);
    function BuildArray: TArrayVec2;

    procedure SaveToStream(stream: TMemoryStream64); overload;
    procedure LoadFromStream(stream: TMemoryStream64); overload;

    function BoundRect: TRectV2; overload;
    function CircleRadius(ACentroid: TVec2): TGeoFloat; overload;
    function Centroid: TVec2; overload;

    function PointInHere(pt: TVec2): Boolean; overload;

    procedure RotateAngle(axis: TVec2; Angle: TGeoFloat); overload;

    procedure Scale(axis: TVec2; Scale: TGeoFloat); overload;

    procedure ConvexHull(output: TVec2List); overload;
    procedure ConvexHull; overload;

    procedure SplineSmooth(output: TVec2List; DetailLevel: TGeoFloat); overload;
    procedure SplineSmooth(DetailLevel: TGeoFloat); overload;

    procedure ExtractToBuff(var output: TArrayVec2); overload;
    procedure GiveListDataFromBuff(output: TArrayVec2); overload;
    procedure VertexReduction(Epsilon: TGeoFloat); overload;

    function Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean; OutputPoint: TVec2List): Boolean; overload;
    function Line2NearIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: Integer; out IntersectPt: TVec2): Boolean; overload;

    procedure SortOfNear(const pt: TVec2); overload;
    procedure SortOfFar(const pt: TVec2); overload;

    procedure Reverse; overload;

    procedure AddCirclePoint(aCount: Cardinal; axis: TVec2; ADist: TGeoFloat);
    procedure AddRectangle(r: TRectV2);

    function GetMinimumFromPointToLine(const pt: TVec2; const ClosedMode: Boolean; out lb, le: Integer): TVec2; overload;
    function GetMinimumFromPointToLine(const pt: TVec2; const ClosedMode: Boolean): TVec2; overload;
    function GetMinimumFromPointToLine(const pt: TVec2; const ExpandDist: TGeoFloat): TVec2; overload;
    procedure CutLineBeginPtToIdx(const pt: TVec2; const toidx: Integer);

    procedure Transform(x, y: TGeoFloat); overload;
    procedure Mul(x, y: TGeoFloat); overload;
    procedure FDiv(x, y: TGeoFloat); overload;

    property Items[index: Integer]: PVec2 read GetPoints;
    property Points[index: Integer]: PVec2 read GetPoints; default;
    function First: PVec2;
    function Last: PVec2;

    procedure ExpandDistanceAsList(ExpandDist: TGeoFloat; output: TVec2List);
    procedure ExpandConvexHullAsList(ExpandDist: TGeoFloat; output: TVec2List);

    function GetExpands(idx: Integer; ExpandDist: TGeoFloat): TVec2;
    property Expands[idx: Integer; ExpandDist: TGeoFloat]: TVec2 read GetExpands;

    property UserData: Pointer read FUserData write FUserData;
    property UserObject: TCoreClassObject read FUserObject write FUserObject;
  end;

  T2DPointList = TVec2List;

  PPolyPoint = ^TPolyPoint;

  TPolyPoint = record
    Owner: TPoly;
    Angle: TGeoFloat;
    Dist: TGeoFloat;
  end;

  TExpandMode = (emConvex, emConcave);

  TPoly = class(TCoreClassPersistent)
  private
    FList: TCoreClassList;
    FScale: TGeoFloat;
    FAngle: TGeoFloat;
    FMaxRadius: TGeoFloat;
    FPosition: TVec2;
    FExpandMode: TExpandMode;

    FUserDataObject: TCoreClassObject;
    FUserData: Pointer;

    function GetPoly(index: Integer): PPolyPoint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset; overload;

    procedure Assign(Source: TCoreClassPersistent); override;

    procedure AddPoint(pt: TVec2); overload;
    procedure AddPoint(x, y: TGeoFloat); overload;
    procedure Add(AAngle, ADist: TGeoFloat); overload;
    procedure Insert(idx: Integer; Angle, Dist: TGeoFloat); overload;
    procedure Delete(idx: Integer); overload;
    procedure Clear; overload;
    function Count: Integer; overload;
    procedure CopyPoly(pl: TPoly; AReversed: Boolean);
    procedure CopyExpandPoly(pl: TPoly; AReversed: Boolean; Dist: TGeoFloat);
    procedure Reverse;
    function ScaleBeforeDistance: TGeoFloat;
    function ScaleAfterDistance: TGeoFloat;

    procedure FixedSameError;

    { * auto build opt from convex hull point * }
    procedure ConvexHullFromPoint(AFrom: TVec2List); overload;
    procedure RebuildPoly(pl: TVec2List); overload;
    procedure RebuildPoly; overload;
    procedure RebuildPoly(AScale: TGeoFloat; AAngle: TGeoFloat; AExpandMode: TExpandMode; APosition: TVec2); overload;

    function BoundRect: TRectV2; overload;
    function Centroid: TVec2; overload;

    { * fast line intersect * }
    function PointInHere(pt: TVec2): Boolean; overload;
    function LineNearIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean;
      out idx1, idx2: Integer; out IntersectPt: TVec2): Boolean; overload;
    function LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean; overload;
    function GetMinimumFromPointToPoly(const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: Integer): TVec2; overload;

    { * expand intersect * }
    function PointInHere(AExpandDistance: TGeoFloat; pt: TVec2): Boolean; overload;
    function LineNearIntersect(AExpandDistance: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean;
      out idx1, idx2: Integer; out IntersectPt: TVec2): Boolean; overload;
    function LineIntersect(AExpandDistance: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean; overload;

    function GetMinimumFromPointToPoly(AExpandDistance: TGeoFloat; const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: Integer): TVec2; overload;

    function Collision2Circle(cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean): Boolean; overload;
    function Collision2Circle(cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: T2DLineList): Boolean; overload;
    function Collision2Circle(AExpandDistance: TGeoFloat; cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: T2DLineList): Boolean; overload;

    function PolyIntersect(APoly: TPoly): Boolean;

    function LerpToEndge(pt: TVec2; AProjDistance, AExpandDistance: TGeoFloat; FromIdx, toidx: Integer): TVec2;

    property Scale: TGeoFloat read FScale write FScale;
    property Angle: TGeoFloat read FAngle write FAngle;
    property Position: TVec2 read FPosition write FPosition;
    property Poly[index: Integer]: PPolyPoint read GetPoly;
    property MaxRadius: TGeoFloat read FMaxRadius;
    property ExpandMode: TExpandMode read FExpandMode write FExpandMode;

    function GetPoint(idx: Integer): TVec2;
    procedure SetPoint(idx: Integer; Value: TVec2);
    property Points[idx: Integer]: TVec2 read GetPoint write SetPoint; default;

    function GetExpands(idx: Integer; ExpandDist: TGeoFloat): TVec2;
    property Expands[idx: Integer; ExpandDist: TGeoFloat]: TVec2 read GetExpands;

    procedure SaveToStream(stream: TMemoryStream64); overload;
    procedure LoadFromStream(stream: TMemoryStream64); overload;

    property UserDataObject: TCoreClassObject read FUserDataObject write FUserDataObject;
    property UserData: Pointer read FUserData write FUserData;
  end;

  T2DLine = record
    buff: array [0 .. 1] of TVec2;
    Poly: TPoly;
    PolyIndex: array [0 .. 1] of Integer;
    index: Integer;
  public
    procedure SetLocation(const lb, le: TVec2);
    function ExpandPoly(ExpandDist: TGeoFloat): T2DLine;
    function length: TGeoFloat;
    function MinimumDistance(const pt: TVec2): TGeoFloat; overload;
    function MinimumDistance(ExpandDist: TGeoFloat; const pt: TVec2): TGeoFloat; overload;
    function ClosestPointFromLine(const pt: TVec2): TVec2; overload;
    function ClosestPointFromLine(ExpandDist: TGeoFloat; const pt: TVec2): TVec2; overload;
    function MiddlePoint: TVec2;
  end;

  P2DLine = ^T2DLine;

  T2DLineList = class(TCoreClassPersistent)
  private
    FList: TCoreClassList;
    FUserData: Pointer;
    FUserObject: TCoreClassObject;
    function GetItems(index: Integer): P2DLine;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TCoreClassPersistent); override;

    property Items[index: Integer]: P2DLine read GetItems; default;
    function Add(v: T2DLine): Integer; overload;
    function Add(lb, le: TVec2): Integer; overload;
    function Add(lb, le: TVec2; idx1, idx2: Integer; Poly: TPoly): Integer; overload;
    function Count: Integer;
    procedure Clear;
    procedure Delete(index: Integer);

    function NearLine(const ExpandDist: TGeoFloat; const pt: TVec2): P2DLine;
    function FarLine(const ExpandDist: TGeoFloat; const pt: TVec2): P2DLine;

    procedure SortOfNear(const pt: TVec2); overload;
    procedure SortOfFar(const pt: TVec2); overload;

    property UserData: Pointer read FUserData write FUserData;
    property UserObject: TCoreClassObject read FUserObject write FUserObject;
  end;

  P2DCircle = ^T2DCircle;

  T2DCircle = record
    Position: TVec2;
    radius: TGeoFloat;
    UserData: TCoreClassObject;
  end;

  T2DCircleList = class(TCoreClassPersistent)
  private
    FList: TCoreClassList;
    function GetItems(index: Integer): P2DCircle;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TCoreClassPersistent); override;

    property Items[index: Integer]: P2DCircle read GetItems; default;
    function Add(const v: T2DCircle): Integer; overload;
    function Add(const Position: TVec2; const radius: TGeoFloat; const UserData: TCoreClassObject): Integer; overload;
    function Count: Integer;
    procedure Clear;
    procedure Delete(index: Integer);

    procedure SortOfMinRadius;
    procedure SortOfMaxRadius;
  end;

  TRectV2List = class(TCoreClassPersistent)
  private
    FList: TCoreClassList;
    function GetItems(index: Integer): PRectV2;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TCoreClassPersistent); override;

    property Items[index: Integer]: PRectV2 read GetItems; default;
    function Add(const v: TRectV2): Integer; overload;
    function Count: Integer;
    procedure Clear;
    procedure Delete(index: Integer);
  end;

  TV2Rect4 = record
    LeftTop: TVec2;
    RightTop: TVec2;
    RightBottom: TVec2;
    LeftBottom: TVec2;
  public
    function IsZero: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Rotation(Angle: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Rotation(axis: TVec2; Angle: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ScaleToRect(Area: TRectV2; endge: TGeoFloat): TV2Rect4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(v: TVec2): TV2Rect4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Sub(v: TVec2): TV2Rect4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Mul(v: TVec2): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Mul(v: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Mul(x, y: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FDiv(v: TVec2): TV2Rect4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function MoveTo(Position: TVec2): TV2Rect4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function BoundRect: TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function BoundRectf: TRectf; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Centroid: TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Transform(v2: TVec2): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Transform(x, y: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Expands(Dist: TGeoFloat): TV2Rect4;
    class function Init(r: TRectV2; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(r: TRectf; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(r: TRect; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(width, height, Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function InitZero: TV2Rect4; static;
  end;

  TRectPackData = record
    Rect: TRectV2;
    error: Boolean;
    Data1: Pointer;
    Data2: TCoreClassObject;
  end;

  PRectPackData = ^TRectPackData;

  TRectPacking = class(TCoreClassPersistent)
  private
    FList: TCoreClassList;
    function Pack(width, height: TGeoFloat; var x, y: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetItems(const index: Integer): PRectPackData;
  public
    MaxWidth, MaxHeight: TGeoFloat;
    Margins: TGeoFloat;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const x, y, width, height: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Add(Data1: Pointer; Data2: TCoreClassObject; x, y, width, height: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Add(Data1: Pointer; Data2: TCoreClassObject; r: TRectV2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Add(Data1: Pointer; Data2: TCoreClassObject; width, height: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Data1Exists(const Data1: Pointer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Data2Exists(const Data2: TCoreClassObject): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Count: Integer;
    property Items[const index: Integer]: PRectPackData read GetItems; default;

    procedure Build(SpaceWidth, SpaceHeight: TGeoFloat); overload;
    procedure Build; overload;
  end;

implementation

uses Geometry3DUnit;

const
  // Epsilon
  Epsilon = 1.0E-12;
  Zero = 0.0;
  PIDiv180 = 0.017453292519943295769236907684886;

procedure T2DLine.SetLocation(const lb, le: TVec2);
begin
  buff[0] := lb;
  buff[1] := le;
end;

function T2DLine.ExpandPoly(ExpandDist: TGeoFloat): T2DLine;
begin
  Result := Self;
  if Poly <> nil then
    begin
      Result.buff[0] := Poly.Expands[PolyIndex[0], ExpandDist];
      Result.buff[1] := Poly.Expands[PolyIndex[1], ExpandDist];
    end;
end;

function T2DLine.length: TGeoFloat;
begin
  Result := PointDistance(buff[0], buff[1]);
end;

function T2DLine.MinimumDistance(const pt: TVec2): TGeoFloat;
begin
  Result := PointDistance(pt, ClosestPointFromLine(pt));
end;

function T2DLine.MinimumDistance(ExpandDist: TGeoFloat; const pt: TVec2): TGeoFloat;
begin
  Result := PointDistance(pt, ClosestPointFromLine(ExpandDist, pt));
end;

function T2DLine.ClosestPointFromLine(const pt: TVec2): TVec2;
begin
  Result := ClosestPointOnSegmentFromPoint(buff[0], buff[1], pt);
end;

function T2DLine.ClosestPointFromLine(ExpandDist: TGeoFloat; const pt: TVec2): TVec2;
var
  E: T2DLine;
begin
  E := ExpandPoly(ExpandDist);
  Result := ClosestPointOnSegmentFromPoint(E.buff[0], E.buff[1], pt);
end;

function T2DLine.MiddlePoint: TVec2;
begin
  Result := MiddleVec2(buff[0], buff[1]);
end;

{$IFDEF FPC}


function Pointf(x, y: TGeoFloat): TPointf;
begin
  Result.x := x;
  Result.y := y;
end;

function Rectf(Left, Top, Right, Bottom: TGeoFloat): TRectf;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

{$ENDIF}


function fabs(const v: Single): Single;
begin
  if v < 0 then
      Result := -v
  else
      Result := v;
end;

function fabs(const v: Double): Double;
begin
  if v < 0 then
      Result := -v
  else
      Result := v;
end;

function Range(const v, minv, maxv: TGeoFloat): TGeoFloat;
begin
  if v < minv then
      Result := minv
  else if v > maxv then
      Result := maxv
  else
      Result := v;
end;

function Clamp(const v, minv, maxv: TGeoFloat): TGeoFloat;
begin
  if v < minv then
      Result := minv
  else if v > maxv then
      Result := maxv
  else
      Result := v;
end;

function MaxF(const v1, v2: TGeoFloat): TGeoFloat;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function MinF(const v1, v2: TGeoFloat): TGeoFloat;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function MakeVec2(const x, y: TGeoFloat): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function MakeVec2(const x, y: Integer): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function MakePoint(const x, y: TGeoFloat): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function MakePoint(const x, y: Integer): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function MakePoint(const pt: TVec2): TPoint;
begin
  Result.x := Round(pt[0]);
  Result.y := Round(pt[1]);
end;

function Point2Point(const pt: TVec2): TPoint;
begin
  Result.x := Round(pt[0]);
  Result.y := Round(pt[1]);
end;

function Point2Pointf(const pt: TVec2): TPointf;
begin
  Result.x := pt[0];
  Result.y := pt[1];
end;

function PointMake(const x, y: TGeoFloat): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function PointMake(const pt: TPoint): TVec2;
begin
  Result[0] := pt.x;
  Result[1] := pt.y;
end;

function PointMake(const pt: TPointf): TVec2;
begin
  Result[0] := pt.x;
  Result[1] := pt.y;
end;

function Make2DPoint(const x, y: TGeoFloat): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function Make2DPoint(const x, y: Integer): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function Make2DPoint(const pt: TPoint): TVec2;
begin
  Result[0] := pt.x;
  Result[1] := pt.y;
end;

function Make2DPoint(const pt: TPointf): TVec2;
begin
  Result[0] := pt.x;
  Result[1] := pt.y;
end;

function vec2(const f: TGeoFloat): TVec2;
begin
  Result[0] := f;
  Result[1] := f;
end;

function vec2(const x, y: TGeoFloat): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function vec2(const x, y: Integer): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function vec2(const x, y: Int64): TVec2;
begin
  Result[0] := x;
  Result[1] := y;
end;

function vec2(const pt: TPoint): TVec2;
begin
  Result[0] := pt.x;
  Result[1] := pt.y;
end;

function vec2(const pt: TPointf): TVec2;
begin
  Result[0] := pt.x;
  Result[1] := pt.y;
end;

function RoundVec2(const v: TVec2): TVec2;
begin
  Result[0] := Round(v[0]);
  Result[1] := Round(v[1]);
end;

function MakePointf(const pt: TVec2): TPointf;
begin
  Result.x := pt[0];
  Result.y := pt[1];
end;

function IsZero(const v: TGeoFloat): Boolean;
begin
  Result := IsEqual(v, 0, Epsilon);
end;

function IsZero(const pt: TVec2): Boolean;
begin
  Result := IsEqual(pt[0], 0, Epsilon) and IsEqual(pt[1], 0, Epsilon);
end;

function IsZero(const r: TRectV2): Boolean;
begin
  Result := IsZero(r[0]) and IsZero(r[1]);
end;

function IsNan(const pt: TVec2): Boolean;
begin
  Result := Math.IsNan(pt[0]) or Math.IsNan(pt[1]);
end;

function IsNan(const x, y: TGeoFloat): Boolean;
begin
  Result := Math.IsNan(x) or Math.IsNan(y);
end;

function HypotX(const x, y: Extended): TGeoFloat;
{
  formula: Sqrt(X*X + Y*Y)
  implemented as: |Y|*Sqrt(1+Sqr(X/Y)), |X| < |Y| for greater precision
}
var
  Temp, TempX, TempY: Extended;
begin
  TempX := fabs(x);
  TempY := fabs(y);
  if TempX > TempY then
    begin
      Temp := TempX;
      TempX := TempY;
      TempY := Temp;
    end;
  if TempX = 0 then
      Result := TempY
  else // TempY > TempX, TempX <> 0, so TempY > 0
      Result := TempY * Sqrt(1 + Sqr(TempX / TempY));
end;

function PointNorm(const v: TVec2): TGeoFloat;
begin
  Result := v[0] * v[0] + v[1] * v[1];
end;

function PointNegate(const v: TVec2): TVec2;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
end;

function Vec2Norm(const v: TVec2): TGeoFloat;
begin
  Result := v[0] * v[0] + v[1] * v[1];
end;

function Vec2Negate(const v: TVec2): TVec2;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
end;

function vec2Inv(const v: TVec2): TVec2;
begin
  Result[0] := v[1];
  Result[1] := v[0];
end;

procedure SetVec2(var v: TVec2; const vSrc: TVec2);
begin
  v[0] := vSrc[0];
  v[1] := vSrc[1];
end;

function Vec2Add(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] + v2[0];
  Result[1] := v1[1] + v2[1];
end;

function Vec2Add(const v1: TVec2; v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + v2;
  Result[1] := v1[1] + v2;
end;

function Vec2Add(const v1: TVec2; x, y: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + x;
  Result[1] := v1[1] + y;
end;

function Vec2Add(const v1: TGeoFloat; v2: TVec2): TVec2;
begin
  Result[0] := v1 + v2[0];
  Result[1] := v1 + v2[1];
end;

function Vec2Add(const v1: TArrayVec2; v2: TVec2): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Add(v1[i], v2);
end;

function Vec2Add(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Add(v1[i], v2);
end;

function Vec2Sub(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] - v2[0];
  Result[1] := v1[1] - v2[1];
end;

function Vec2Sub(const v1: TVec2; v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] - v2;
  Result[1] := v1[1] - v2;
end;

function Vec2Sub(const v1: TGeoFloat; v2: TVec2): TVec2;
begin
  Result[0] := v1 - v2[0];
  Result[1] := v1 - v2[1];
end;

function Vec2Sub(const v1: TArrayVec2; v2: TVec2): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Sub(v1[i], v2);
end;

function Vec2Sub(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Sub(v1[i], v2);
end;

function Vec2Mul(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] * v2[0];
  Result[1] := v1[1] * v2[1];
end;

function Vec2Mul(const v1, v2: TVec2; const v3: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3;
  Result[1] := v1[1] * v2[1] * v3;
end;

function Vec2Mul(const v1, v2: TVec2; const v3, v4: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3 * v4;
  Result[1] := v1[1] * v2[1] * v3 * v4;
end;

function Vec2Mul(const v1, v2, v3: TVec2): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3[0];
  Result[1] := v1[1] * v2[1] * v3[1];
end;

function Vec2Mul(const v1, v2, v3, v4: TVec2): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3[0] * v4[0];
  Result[1] := v1[1] * v2[1] * v3[1] * v4[1];
end;

function Vec2Mul(const v1: TVec2; const v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2;
  Result[1] := v1[1] * v2;
end;

function Vec2Mul(const v1: TVec2; const v2, v3: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2 * v3;
  Result[1] := v1[1] * v2 * v3;
end;

function Vec2Mul(const v1: TVec2; const v2, v3, v4: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2 * v3 * v4;
  Result[1] := v1[1] * v2 * v3 * v4;
end;

function Vec2Mul(const v1: TGeoFloat; const v2: TVec2): TVec2;
begin
  Result[0] := v1 * v2[0];
  Result[1] := v1 * v2[1];
end;

function Vec2Mul(const v1: TArrayVec2; v2: TVec2): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Mul(v1[i], v2);
end;

function Vec2Mul(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Mul(v1[i], v2);
end;

function Vec2Div(const v1: TVec2; const v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] / v2;
  Result[1] := v1[1] / v2;
end;

function Vec2Div(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] / v2[0];
  Result[1] := v1[1] / v2[1];
end;

function Vec2Div(const v1: TGeoFloat; const v2: TVec2): TVec2;
begin
  Result[0] := v1 / v2[0];
  Result[1] := v1 / v2[1];
end;

function PointNormalize(const v: TVec2): TVec2;
var
  InvLen: TGeoFloat;
  vn: TGeoFloat;
begin
  vn := PointNorm(v);
  if vn = 0 then
      SetVec2(Result, v)
  else
    begin
      InvLen := 1 / Sqrt(vn);
      Result[0] := v[0] * InvLen;
      Result[1] := v[1] * InvLen;
    end;
end;

function Vec2Normalize(const v: TVec2): TVec2;
var
  InvLen: TGeoFloat;
  vn: TGeoFloat;
begin
  vn := PointNorm(v);
  if vn = 0 then
      SetVec2(Result, v)
  else
    begin
      InvLen := 1 / Sqrt(vn);
      Result[0] := v[0] * InvLen;
      Result[1] := v[1] * InvLen;
    end;
end;

function PointLength(const v: TVec2): TGeoFloat;
begin
  Result := Sqrt(PointNorm(v));
end;

function Vec2Length(const v: TVec2): TGeoFloat;
begin
  Result := Sqrt(Vec2Norm(v));
end;

procedure PointScale(var v: TVec2; factor: TGeoFloat);
begin
  v[0] := v[0] * factor;
  v[1] := v[1] * factor;
end;

function PointDotProduct(const v1, v2: TVec2): TGeoFloat;
begin
  Result := v1[0] * v2[0] + v1[1] * v2[1];
end;

function Distance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat;
begin
  Result := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
end;

function Distance(const x1, y1, z1, x2, y2, z2: TGeoFloat): TGeoFloat;
begin
  Result := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) + (z2 - z1) * (z2 - z1));
end;

function PointDistance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat;
begin
  Result := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
end;

function PointDistance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Sqrt((v2[0] - v1[0]) * (v2[0] - v1[0]) + (v2[1] - v1[1]) * (v2[1] - v1[1]));
end;

function Vec2Distance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Sqrt((v2[0] - v1[0]) * (v2[0] - v1[0]) + (v2[1] - v1[1]) * (v2[1] - v1[1]));
end;

function PointLayDistance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Pow(v2[0] - v1[0]) + Pow(v2[1] - v1[1]);
end;

function SqrDistance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Sqr(v2[0] - v1[0]) + Sqr(v2[1] - v1[1]);
end;

function PointLerp(const v1, v2: TVec2; t: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + (v2[0] - v1[0]) * t;
  Result[1] := v1[1] + (v2[1] - v1[1]) * t;
end;

function PointLerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2;
var
  dx: TGeoFloat;
  dy: TGeoFloat;
  k: Double;
begin
  dx := dest[0] - sour[0];
  dy := dest[1] - sour[1];
  if ((dx <> 0) or (dy <> 0)) and (d <> 0) then
    begin
      k := d / Sqrt(dx * dx + dy * dy);
      Result[0] := sour[0] + k * dx;
      Result[1] := sour[1] + k * dy;
    end
  else
    begin
      Result := sour;
    end;
end;

function Vec2Lerp(const v1, v2: TVec2; t: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + (v2[0] - v1[0]) * t;
  Result[1] := v1[1] + (v2[1] - v1[1]) * t;
end;

function Vec2LerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2;
var
  dx: TGeoFloat;
  dy: TGeoFloat;
  k: Double;
begin
  dx := dest[0] - sour[0];
  dy := dest[1] - sour[1];
  if ((dx <> 0) or (dy <> 0)) and (d <> 0) then
    begin
      k := d / Sqrt(dx * dx + dy * dy);
      Result[0] := sour[0] + k * dx;
      Result[1] := sour[1] + k * dy;
    end
  else
    begin
      Result := sour;
    end;
end;

procedure SwapPoint(var v1, v2: TVec2);
var
  v: TVec2;
begin
  v := v1;
  v1 := v2;
  v2 := v;
end;

function Pow(v: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
begin
  Result := v * v;
end;

function Pow(const v, n: TGeoFloat): TGeoFloat;
begin
  Result := Math.Power(v, n);
end;

function MiddleVec2(const pt1, pt2: TVec2): TVec2;
begin
  Result[0] := (pt1[0] + pt2[0]) * 0.5;
  Result[1] := (pt1[1] + pt2[1]) * 0.5;
end;

function IsEqual(const Val1, Val2, Epsilon: TGeoFloat): Boolean;
var
  Diff: TGeoFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon <= Diff) and (Diff <= Epsilon)) = (fabs(Diff) <= Epsilon), 'Error - Illogical error in equality Detect. (IsEqual)');
  Result := ((-Epsilon <= Diff) and (Diff <= Epsilon));
end;

function IsEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := IsEqual(Val1, Val2, Epsilon);
end;

function IsEqual(const Val1, Val2: TVec2): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0]) and IsEqual(Val1[1], Val2[1]);
end;

function IsEqual(const Val1, Val2: TVec2; Epsilon: TGeoFloat): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0], Epsilon) and IsEqual(Val1[1], Val2[1], Epsilon);
end;

function IsEqual(const Val1, Val2: TRectV2): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0]) and IsEqual(Val1[1], Val2[1]);
end;

function NotEqual(const Val1, Val2, Epsilon: TGeoFloat): Boolean;
var
  Diff: TGeoFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon > Diff) or (Diff > Epsilon)) = (fabs(Val1 - Val2) > Epsilon), 'Error - Illogical error in equality Detect. (NotEqual)');
  Result := ((-Epsilon > Diff) or (Diff > Epsilon));
end;

function NotEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := NotEqual(Val1, Val2, Epsilon);
end;

function NotEqual(const Val1, Val2: TVec2): Boolean;
begin
  Result := NotEqual(Val1[0], Val2[0]) or NotEqual(Val1[1], Val2[1]);
end;

function LessThanOrEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := (Val1 < Val2) or IsEqual(Val1, Val2);
end;

function GreaterThanOrEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := (Val1 > Val2) or IsEqual(Val1, Val2);
end;

function GetEquilateralTriangleCen(pt1, pt2: TVec2): TVec2;
const
  Sin60: TGeoFloat = 0.86602540378443864676372317075294;
  Cos60: TGeoFloat = 0.50000000000000000000000000000000;
var
  b, E, pt: TVec2;
begin
  b := pt1;
  E := pt2;
  E[0] := E[0] - b[0];
  E[1] := E[1] - b[1];
  pt[0] := ((E[0] * Cos60) - (E[1] * Sin60)) + b[0];
  pt[1] := ((E[1] * Cos60) + (E[0] * Sin60)) + b[1];
  Assert(Intersect(pt1, MiddleVec2(pt2, pt), pt2, MiddleVec2(pt1, pt), Result));
end;

procedure Rotate(RotAng: TGeoFloat; const x, y: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  SinVal: TGeoFloat;
  CosVal: TGeoFloat;
begin
  RotAng := RotAng * PIDiv180;
  SinVal := Sin(RotAng);
  CosVal := Cos(RotAng);
  Nx := (x * CosVal) - (y * SinVal);
  Ny := (y * CosVal) + (x * SinVal);
end;

function Rotate(const RotAng: TGeoFloat; const Point: TVec2): TVec2;
begin
  Rotate(RotAng, Point[0], Point[1], Result[0], Result[1]);
end;

function NormalizeDegAngle(const Angle: TGeoFloat): TGeoFloat;
begin
  Result := Angle - Int(Angle * (1 / 360)) * 360;
  if Result > 180 then
      Result := Result - 360
  else if Result < -180 then
      Result := Result + 360;
end;

function PointAngle(const axis, pt: TVec2): TGeoFloat;
begin
  Result := NormalizeDegAngle(RadToDeg(ArcTan2(axis[1] - pt[1], axis[0] - pt[0])));
end;

function Vec2Angle(const axis, pt: TVec2): TGeoFloat;
begin
  Result := NormalizeDegAngle(RadToDeg(ArcTan2(axis[1] - pt[1], axis[0] - pt[0])));
end;

function PointAngle(const pt: TVec2): TGeoFloat;
begin
  Result := PointAngle(NULLPoint, pt);
end;

function Vec2Angle(const pt: TVec2): TGeoFloat;
begin
  Result := Vec2Angle(NULLPoint, pt);
end;

function AngleDistance(const s, a: TGeoFloat): TGeoFloat;
begin
  Result := fabs(s - a);
  if Result > 180 then
      Result := 360 - Result;
end;

function PointRotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2;
begin
  Result[0] := axis[0] - (Cos(DegToRad(Angle)) * Dist);
  Result[1] := axis[1] - (Sin(DegToRad(Angle)) * Dist);
end;

function PointRotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2;
begin
  Result := PointRotation(axis, PointDistance(axis, pt), Angle);
end;

function Vec2Rotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2;
begin
  Result[0] := axis[0] - (Cos(DegToRad(Angle)) * Dist);
  Result[1] := axis[1] - (Sin(DegToRad(Angle)) * Dist);
end;

function Vec2Rotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2;
begin
  Result := Vec2Rotation(axis, Vec2Distance(axis, pt), Angle);
end;

function CircleInCircle(const cp1, cp2: TVec2; const r1, r2: TGeoFloat): Boolean;
begin
  Result := (r2 - (PointDistance(cp1, cp2) + r1) >= Zero);
end;

function CircleInRect(const cp: TVec2; const radius: TGeoFloat; r: TRectV2): Boolean;
begin
  FixRect(r[0][0], r[0][1], r[1][0], r[1][1]);
  Result := PointInRect(cp, MakeRect(Vec2Sub(r[0], radius), Vec2Add(r[1], radius)));
end;

function PointInRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function PointInRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function PointInRect(const pt: TVec2; const r: TRectV2): Boolean;
begin
  Result := PointInRect(pt[0], pt[1], r[0][0], r[0][1], r[1][0], r[1][1]);
end;

function PointInRect(const Px, Py: TGeoFloat; const r: TRectV2): Boolean;
begin
  Result := PointInRect(Px, Py, r[0][0], r[0][1], r[1][0], r[1][1]);
end;

function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := (x1 <= x4) and (x2 >= x3) and (y1 <= y4) and (y2 >= y3);
end;

function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean;
begin
  Result := (x1 <= x4) and (x2 >= x3) and (y1 <= y4) and (y2 >= y3);
end;

function RectToRectIntersect(const r1, r2: TRectV2): Boolean;
begin
  Result := RectToRectIntersect(r1[0][0], r1[0][1], r1[1][0], r1[1][1], r2[0][0], r2[0][1], r2[1][0], r2[1][1]);
end;

function RectToRectIntersect(const r1, r2: TRect): Boolean;
begin
  Result := RectToRectIntersect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function RectToRectIntersect(const r1, r2: TRectf): Boolean;
begin
  Result := RectToRectIntersect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function RectWithinRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectWithinRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectWithinRect(const r1, r2: TRectV2): Boolean;
begin
  Result := RectWithinRect(r1[0][0], r1[0][1], r1[1][0], r1[1][1], r2[0][0], r2[0][1], r2[1][0], r2[1][1]);
end;

function RectWithinRect(const r1, r2: TRect): Boolean;
begin
  Result := RectWithinRect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function MakeRectV2(const x, y, radius: TGeoFloat): TRectV2;
begin
  Result[0][0] := x - radius;
  Result[0][1] := y - radius;
  Result[1][0] := x + radius;
  Result[1][1] := y + radius;
end;

function MakeRectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0][0] := x1;
  Result[0][1] := y1;
  Result[1][0] := x2;
  Result[1][1] := y2;
end;

function MakeRectV2(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function MakeRectV2(const x, y: TGeoFloat; const p2: TVec2): TRectV2;
begin
  Result[0] := PointMake(x, y);
  Result[1] := p2;
end;

function MakeRectV2(const r: TRect): TRectV2;
begin
  Result[0][0] := r.Left;
  Result[0][1] := r.Top;
  Result[1][0] := r.Right;
  Result[1][1] := r.Bottom;
end;

function MakeRectV2(const r: TRectf): TRectV2;
begin
  Result[0][0] := r.Left;
  Result[0][1] := r.Top;
  Result[1][0] := r.Right;
  Result[1][1] := r.Bottom;
end;

function RectV2(const x, y, radius: TGeoFloat): TRectV2;
begin
  Result[0][0] := x - radius;
  Result[0][1] := y - radius;
  Result[1][0] := x + radius;
  Result[1][1] := y + radius;
end;

function RectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0][0] := x1;
  Result[0][1] := y1;
  Result[1][0] := x2;
  Result[1][1] := y2;
end;

function RectV2(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function RectV2(const p1, p2: TPointf): TRectV2;
begin
  Result[0] := vec2(p1);
  Result[1] := vec2(p2);
end;

function RectV2(const x, y: TGeoFloat; const p2: TVec2): TRectV2;
begin
  Result[0] := PointMake(x, y);
  Result[1] := p2;
end;

function RectV2(const r: TRect): TRectV2;
begin
  Result[0][0] := r.Left;
  Result[0][1] := r.Top;
  Result[1][0] := r.Right;
  Result[1][1] := r.Bottom;
end;

function RectV2(const r: TRectf): TRectV2;
begin
  Result[0][0] := r.Left;
  Result[0][1] := r.Top;
  Result[1][0] := r.Right;
  Result[1][1] := r.Bottom;
end;

function RectV2(const r: TRectV2): TRectV2;
begin
  Result := FixedRect(r);
end;

function MakeRect(const x, y, radius: TGeoFloat): TRectV2;
begin
  Result[0][0] := x - radius;
  Result[0][1] := y - radius;
  Result[1][0] := x + radius;
  Result[1][1] := y + radius;
end;

function MakeRect(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0][0] := x1;
  Result[0][1] := y1;
  Result[1][0] := x2;
  Result[1][1] := y2;
end;

function MakeRect(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function MakeRect(const r: TRect): TRectV2;
begin
  Result[0][0] := r.Left;
  Result[0][1] := r.Top;
  Result[1][0] := r.Right;
  Result[1][1] := r.Bottom;
end;

function MakeRect(const r: TRectf): TRectV2;
begin
  Result[0][0] := r.Left;
  Result[0][1] := r.Top;
  Result[1][0] := r.Right;
  Result[1][1] := r.Bottom;
end;

function Rect2Rect(const r: TRectV2): TRect;
begin
  Result.Left := Round(r[0][0]);
  Result.Top := Round(r[0][1]);
  Result.Right := Round(r[1][0]);
  Result.Bottom := Round(r[1][1]);
end;

function Rect2Rect(const r: TRect): TRectV2;
begin
  Result[0][0] := r.Left;
  Result[0][1] := r.Top;
  Result[1][0] := r.Right;
  Result[1][1] := r.Bottom;
end;

function RectMake(const x, y, radius: TGeoFloat): TRectV2;
begin
  Result[0][0] := x - radius;
  Result[0][1] := y - radius;
  Result[1][0] := x + radius;
  Result[1][1] := y + radius;
end;

function RectMake(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0][0] := x1;
  Result[0][1] := y1;
  Result[1][0] := x2;
  Result[1][1] := y2;
end;

function RectMake(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function RectMake(const r: TRect): TRectV2;
begin
  Result[0][0] := r.Left;
  Result[0][1] := r.Top;
  Result[1][0] := r.Right;
  Result[1][1] := r.Bottom;
end;

function RectMake(const r: TRectf): TRectV2;
begin
  Result[0][0] := r.Left;
  Result[0][1] := r.Top;
  Result[1][0] := r.Right;
  Result[1][1] := r.Bottom;
end;

function RectAdd(const r: TRectV2; pt: TVec2): TRectV2;
begin
  Result[0] := Vec2Add(r[0], pt);
  Result[1] := Vec2Add(r[1], pt);
end;

function RectAdd(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Add(r1[0], r2[0]);
  Result[1] := Vec2Add(r1[1], r2[1]);
end;

function RectSub(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Sub(r1[0], r2[0]);
  Result[1] := Vec2Sub(r1[1], r2[1]);
end;

function RectSub(const r: TRectV2; pt: TVec2): TRectV2;
begin
  Result[0] := Vec2Sub(r[0], pt);
  Result[1] := Vec2Sub(r[1], pt);
end;

function RectMul(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Mul(r1[0], r2[0]);
  Result[1] := Vec2Mul(r1[1], r2[1]);
end;

function RectMul(const r1: TRectV2; v2: TVec2): TRectV2;
begin
  Result[0] := Vec2Mul(r1[0], v2[0]);
  Result[1] := Vec2Mul(r1[1], v2[1]);
end;

function RectMul(const r1: TRectV2; r2: TGeoFloat): TRectV2;
begin
  Result[0] := Vec2Mul(r1[0], r2);
  Result[1] := Vec2Mul(r1[1], r2);
end;

function RectDiv(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Div(r1[0], r2[0]);
  Result[1] := Vec2Div(r1[1], r2[1]);
end;

function RectDiv(const r1: TRectV2; f2: TGeoFloat): TRectV2;
begin
  Result[0] := Vec2Div(r1[0], f2);
  Result[1] := Vec2Div(r1[1], f2);
end;

function RectOffset(const r: TRectV2; Offset: TVec2): TRectV2;
begin
  Result[0] := Vec2Add(r[0], Offset);
  Result[1] := Vec2Add(r[1], Offset);
end;

function RectSizeLerp(const r: TRectV2; const rSizeLerp: TGeoFloat): TRectV2;
begin
  Result[0] := r[0];
  Result[1] := PointLerp(r[0], r[1], rSizeLerp);
end;

function RectCenScale(const r: TRectV2; const rSizeScale: TGeoFloat): TRectV2;
var
  cen, siz: TVec2;
begin
  cen := PointLerp(r[0], r[1], 0.5);
  siz := Vec2Mul(RectSize(r), rSizeScale);
  Result[0] := Vec2Sub(cen, Vec2Mul(siz, 0.5));
  Result[1] := Vec2Add(cen, Vec2Mul(siz, 0.5));
end;

function RectEndge(const r: TRectV2; const endge: TGeoFloat): TRectV2;
begin
  Result[0][0] := r[0][0] - endge;
  Result[0][1] := r[0][1] - endge;
  Result[1][0] := r[1][0] + endge;
  Result[1][1] := r[1][1] + endge;
end;

function RectEndge(const r: TRectV2; const endge: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
begin
  Result[0][0] := r[0][0] - endge[0];
  Result[0][1] := r[0][1] - endge[1];
  Result[1][0] := r[1][0] + endge[0];
  Result[1][1] := r[1][1] + endge[1];
end;

function RectCentre(const r: TRectV2): TVec2;
begin
  Result := PointLerp(r[0], r[1], 0.5);
end;

function RectCentre(const r: TRect): TVec2;
begin
  Result := RectCentre(RectV2(r));
end;

function RectCentre(const r: TRectf): TVec2;
begin
  Result := RectCentre(RectV2(r));
end;

function Vec2TransformToDest(const sour, dest: TRectV2; sour_sub: TVec2): TVec2;
var
  sw, sh, dw, dh: TGeoFloat;
begin
  sw := RectWidth(sour);
  sh := RectHeight(sour);
  dw := RectWidth(dest);
  dh := RectHeight(dest);
  Result := Vec2Add(Vec2Mul(sour_sub, vec2(dw / sw, dh / sh)), dest[0]);
end;

function RectTransformToDest(const sour, dest, sour_sub: TRectV2): TRectV2;
var
  sw, sh, dw, dh: TGeoFloat;
begin
  sw := RectWidth(sour);
  sh := RectHeight(sour);
  dw := RectWidth(dest);
  dh := RectHeight(dest);
  Result := RectAdd(RectMul(sour_sub, vec2(dw / sw, dh / sh)), dest[0]);
end;

function RectTransformToDest(const sour, dest: TRectV2; const sour_sub: TRect): TRectV2;
begin
  Result := RectTransformToDest(sour, dest, RectV2(sour_sub));
end;

function RectTransformToDest(const sour, dest: TRectV2; const sour_sub: TRectf): TRectV2;
begin
  Result := RectTransformToDest(sour, dest, RectV2(sour_sub));
end;

function RectScaleSpace(const r: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2;
var
  k: TGeoFloat;
  w, h, nw, nh: TGeoFloat;
  d: TVec2;
begin
  k := SS_width / SS_height;
  Result := ForwardRect(r);
  w := RectWidth(Result);
  h := RectHeight(Result);

  if w < h then
    begin
      nw := h * k;
      nh := h;
    end
  else
    begin
      nw := w;
      nh := w * k;
    end;

  d[0] := (nw - w) * 0.5;
  d[1] := (nh - h) * 0.5;
  Result[0] := Vec2Sub(Result[0], d);
  Result[1] := Vec2Add(Result[1], d);

  Result := FixRect(Result);
end;

function RectScaleSpace(const r: TRect; const SS_width, SS_height: Integer): TRect;
begin
  Result := MakeRect(RectScaleSpace(RectV2(r), SS_width, SS_height));
end;

function CalibrationRectInRect(const r, Area: TRectV2): TRectV2;
var
  nr: TRectV2;
begin
  nr := ForwardRect(r);

  if nr[0, 0] < Area[0, 0] then
      nr := RectOffset(nr, vec2(Area[0, 0] - nr[0, 0], 0));
  if nr[0, 1] < Area[0, 1] then
      nr := RectOffset(nr, vec2(0, Area[0, 1] - nr[0, 1]));
  if nr[1, 0] > Area[1, 0] then
      nr := RectOffset(nr, vec2(Area[1, 0] - nr[1, 0], 0));
  if nr[1, 1] > Area[1, 1] then
      nr := RectOffset(nr, vec2(0, Area[1, 1] - nr[1, 1]));

  Result := nr;
end;

function CalibrationRectInRect(const r, Area: TRect): TRect;
begin
  Result := MakeRect(CalibrationRectInRect(RectV2(r), RectV2(Area)));
end;

procedure FixRect(var Left, Top, Right, Bottom: Integer);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

procedure FixRect(var Left, Top, Right, Bottom: TGeoFloat);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

function FixRect(r: TRectV2): TRectV2;
begin
  Result := r;
  FixRect(Result[0][0], Result[0][1], Result[1][0], Result[1][1]);
end;

function FixRect(r: TRect): TRect;
begin
  Result := r;
  FixRect(Result.Left, Result.Top, Result.Right, Result.Bottom);
end;

procedure FixedRect(var Left, Top, Right, Bottom: Integer);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

procedure FixedRect(var Left, Top, Right, Bottom: TGeoFloat);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

function FixedRect(r: TRectV2): TRectV2;
begin
  Result := r;
  FixedRect(Result[0][0], Result[0][1], Result[1][0], Result[1][1]);
end;

function FixedRect(r: TRect): TRect;
begin
  Result := r;
  FixedRect(Result.Left, Result.Top, Result.Right, Result.Bottom);
end;

procedure ForwardRect(var Left, Top, Right, Bottom: Integer);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

procedure ForwardRect(var Left, Top, Right, Bottom: TGeoFloat);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

function ForwardRect(r: TRectV2): TRectV2;
begin
  Result := r;
  ForwardRect(Result[0][0], Result[0][1], Result[1][0], Result[1][1]);
end;

function ForwardRect(r: TRect): TRect;
begin
  Result := r;
  ForwardRect(Result.Left, Result.Top, Result.Right, Result.Bottom);
end;

function MakeRect(const r: TRectV2): TRect;
begin
  Result.Left := Round(r[0][0]);
  Result.Top := Round(r[0][1]);
  Result.Right := Round(r[1][0]);
  Result.Bottom := Round(r[1][1]);
end;

function MakeRectf(const r: TRectV2): TRectf;
begin
  Result.Left := r[0][0];
  Result.Top := r[0][1];
  Result.Right := r[1][0];
  Result.Bottom := r[1][1];
end;

function RectWidth(const r: TRectV2): TGeoFloat;
begin
  if r[1][0] > r[0][0] then
      Result := r[1][0] - r[0][0]
  else
      Result := r[0][0] - r[1][0];
end;

function RectHeight(const r: TRectV2): TGeoFloat;
begin
  if r[1][1] > r[0][1] then
      Result := r[1][1] - r[0][1]
  else
      Result := r[0][1] - r[1][1];
end;

function RectWidth(const r: TRect): TGeoInt;
begin
  if r.Right > r.Left then
      Result := r.Right - r.Left
  else
      Result := r.Left - r.Right;
end;

function RectHeight(const r: TRect): TGeoInt;
begin
  if r.Bottom > r.Top then
      Result := r.Bottom - r.Top
  else
      Result := r.Top - r.Bottom;
end;

function RectWidth(const r: TRectf): TGeoFloat;
begin
  if r.Right > r.Left then
      Result := r.Right - r.Left
  else
      Result := r.Left - r.Right;
end;

function RectHeight(const r: TRectf): TGeoFloat;
begin
  if r.Bottom > r.Top then
      Result := r.Bottom - r.Top
  else
      Result := r.Top - r.Bottom;
end;

function RectArea(const r: TRectV2): TGeoFloat;
begin
  Result := RectWidth(r) * RectHeight(r);
end;

function RectSize(const r: TRectV2): TVec2;
var
  n: TRectV2;
begin
  n := FixRect(r);
  Result := Vec2Sub(n[1], n[0]);
end;

function RectFit(const r, b: TRectV2): TRectV2;
var
  k, kw, kh: TGeoFloat;
  rs, bs, siz, pt: TVec2;
begin
  rs := RectSize(r);
  bs := RectSize(b);

  kw := rs[0] / bs[0];
  kh := rs[1] / bs[1];

  if kw > kh then
      k := kw
  else
      k := kh;

  siz := Vec2Div(rs, k);
  pt := Vec2Mul(Vec2Sub(bs, siz), 0.5);
  Result[0] := Vec2Add(b[0], pt);
  Result[1] := Vec2Add(Result[0], siz);
end;

function RectFit(const width, height: TGeoFloat; const b: TRectV2): TRectV2;
begin
  Result := RectFit(MakeRectV2(0, 0, width, height), b);
end;

function BoundRect(const buff: TArrayVec2): TRectV2;
var
  t: TVec2;
  MaxX: TGeoFloat;
  MaxY: TGeoFloat;
  MinX: TGeoFloat;
  MinY: TGeoFloat;
  i: Integer;
begin
  Result := MakeRectV2(Zero, Zero, Zero, Zero);
  if length(buff) < 2 then
      Exit;
  t := buff[0];
  MinX := t[0];
  MaxX := t[0];
  MinY := t[1];
  MaxY := t[1];

  for i := 1 to length(buff) - 1 do
    begin
      t := buff[i];
      if t[0] < MinX then
          MinX := t[0]
      else if t[0] > MaxX then
          MaxX := t[0];
      if t[1] < MinY then
          MinY := t[1]
      else if t[1] > MaxY then
          MaxY := t[1];
    end;
  Result := MakeRectV2(MinX, MinY, MaxX, MaxY);
end;

function BoundRect(const p1, p2, p3: TVec2): TRectV2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 3);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  Result := BoundRect(buff);
end;

function BoundRect(const p1, p2, p3, p4: TVec2): TRectV2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 4);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  buff[3] := p4;
  Result := BoundRect(buff);
end;

function BoundRect(const r1, r2: TRectV2): TRectV2;
begin
  Result := BoundRect(r1[0], r1[1], r2[0], r2[1]);
end;

function BuffCentroid(const buff: TArrayVec2): TVec2;
var
  i, Count: Integer;
  asum: TGeoFloat;
  term: TGeoFloat;

  t1, t2: TVec2;
begin
  Result := NULLPoint;
  Count := length(buff);

  if Count < 3 then
      Exit;

  asum := Zero;
  t2 := buff[Count - 1];

  for i := 0 to Count - 1 do
    begin
      t1 := buff[i];

      term := ((t2[0] * t1[1]) - (t2[1] * t1[0]));
      asum := asum + term;
      Result[0] := Result[0] + (t2[0] + t1[0]) * term;
      Result[1] := Result[1] + (t2[1] + t1[1]) * term;
      t2 := t1;
    end;

  if NotEqual(asum, Zero) then
    begin
      Result[0] := Result[0] / (3.0 * asum);
      Result[1] := Result[1] / (3.0 * asum);
    end;
end;

function BuffCentroid(const p1, p2, p3, p4: TVec2): TVec2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 4);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  buff[3] := p4;
  Result := BuffCentroid(buff);
end;

function FastRamerDouglasPeucker(var Points: TArrayVec2; Epsilon: TGeoFloat): Integer;
var
  i: Integer;
  Range: array of Integer;
  FirstIndex: Integer;
  LastIndex: Integer;
  LastPoint: TVec2;
  FirstLastDelta: TVec2;
  DeltaMaxIndex: Integer;
  Delta: TGeoFloat;
  DeltaMax: TGeoFloat;
begin
  Result := length(Points);
  if Result < 3 then
      Exit;
  FirstIndex := 0;
  LastIndex := Result - 1;
  SetLength(Range, Result);
  Range[0] := LastIndex;
  Range[LastIndex] := -1;
  Result := 0;

  repeat
    if LastIndex - FirstIndex > 1 then
      begin
        // find the point with the maximum distance
        DeltaMax := 0;
        DeltaMaxIndex := 0;
        LastPoint := Points[LastIndex];
        FirstLastDelta := Vec2Sub(Points[FirstIndex], LastPoint);
        for i := FirstIndex + 1 to LastIndex - 1 do
          begin
            Delta := fabs((Points[i][0] - LastPoint[0]) * FirstLastDelta[1] - (Points[i][1] - LastPoint[1]) * FirstLastDelta[0]);
            if Delta > DeltaMax then
              begin
                DeltaMaxIndex := i;
                DeltaMax := Delta;
              end;
          end;

        // if max distance is greater than epsilon, split ranges
        if DeltaMax >= Epsilon * HypotX(FirstLastDelta[0], FirstLastDelta[1]) then
          begin
            Range[FirstIndex] := DeltaMaxIndex;
            Range[DeltaMaxIndex] := LastIndex;
            LastIndex := DeltaMaxIndex;
            Continue;
          end;
      end;

    // Include First and Last points only
    if Result <> FirstIndex then
        Points[Result] := Points[FirstIndex];
    inc(Result);
    if Result <> LastIndex then
        Points[Result] := Points[LastIndex];

    // Next range
    FirstIndex := Range[FirstIndex];
    LastIndex := Range[FirstIndex];

  until LastIndex < 0;
  inc(Result);
end;

procedure FastVertexReduction(Points: TArrayVec2; Epsilon: TGeoFloat; var output: TArrayVec2);

  procedure FilterPoints;
  var
    index: Integer;
    Count: Integer;
    SqrEpsilon: TGeoFloat;
  begin
    SqrEpsilon := Sqr(Epsilon);
    output := Points;
    Count := 1;
    for index := 1 to high(output) do
      begin
        if SqrDistance(output[Count - 1], Points[index]) > SqrEpsilon then
          begin
            if Count <> index then
                output[Count] := Points[index];
            inc(Count);
          end;
      end;
    SetLength(output, Count);
  end;

var
  Count: Integer;
begin
  FilterPoints;

  Count := FastRamerDouglasPeucker(output, Epsilon);
  SetLength(output, Count);
end;

function Clip(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out Cx1, Cy1, Cx2, Cy2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
begin
  if RectToRectIntersect(x1, y1, x2, y2, x3, y3, x4, y4) then
    begin
      Result := True;
      if x1 < x3 then
          Cx1 := x3
      else
          Cx1 := x1;

      if x2 > x4 then
          Cx2 := x4
      else
          Cx2 := x2;

      if y1 < y3 then
          Cy1 := y3
      else
          Cy1 := y1;

      if y2 > y4 then
          Cy2 := y4
      else
          Cy2 := y2;
    end
  else
      Result := False;
end;

function Clip(const r1, r2: TRectV2; out r3: TRectV2): Boolean;
begin
  Result := Clip(
    r1[0][0], r1[0][1], r1[1][0], r1[1][1],
    r2[0][0], r2[0][1], r2[1][0], r2[1][1],
    r3[0][0], r3[0][1], r3[1][0], r3[1][1]);
end;

function Orientation(const x1, y1, x2, y2, Px, Py: TGeoFloat): Integer;
var
  Orin: TGeoFloat;
begin
  (* Determinant of the 3 points *)
  Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);

  if Orin > Zero then
      Result := LeftHandSide (* Orientaion is to the left-hand side *)
  else if Orin < Zero then
      Result := RightHandSide (* Orientaion is to the right-hand side *)
  else
      Result := CollinearOrientation; (* Orientaion is neutral aka collinear *)
end;

function Orientation(const x1, y1, z1, x2, y2, z2, x3, y3, z3, Px, Py, Pz: TGeoFloat): Integer;
var
  Px1: TGeoFloat;
  Px2: TGeoFloat;
  Px3: TGeoFloat;
  Py1: TGeoFloat;
  Py2: TGeoFloat;
  Py3: TGeoFloat;
  Pz1: TGeoFloat;
  Pz2: TGeoFloat;
  Pz3: TGeoFloat;
  Orin: TGeoFloat;
begin
  Px1 := x1 - Px;
  Px2 := x2 - Px;
  Px3 := x3 - Px;

  Py1 := y1 - Py;
  Py2 := y2 - Py;
  Py3 := y3 - Py;

  Pz1 := z1 - Pz;
  Pz2 := z2 - Pz;
  Pz3 := z3 - Pz;

  Orin := Px1 * (Py2 * Pz3 - Pz2 * Py3) +
    Px2 * (Py3 * Pz1 - Pz3 * Py1) +
    Px3 * (Py1 * Pz2 - Pz1 * Py2);

  if Orin < Zero then
      Result := BelowOrientation (* Orientaion is below plane *)
  else if Orin > Zero then
      Result := AboveOrientation (* Orientaion is above plane *)
  else
      Result := CoplanarOrientation; (* Orientaion is coplanar to plane if Result is 0 *)
end;

function Coplanar(const x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4: TGeoFloat): Boolean;
begin
  Result := (Orientation(x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4) = CoplanarOrientation);
end;

function SimpleIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := (
    ((Orientation(x1, y1, x2, y2, x3, y3) * Orientation(x1, y1, x2, y2, x4, y4)) <= 0) and
    ((Orientation(x3, y3, x4, y4, x1, y1) * Orientation(x3, y3, x4, y4, x2, y2)) <= 0)
    );
end;

function SimpleIntersect(const Point1, Point2, Point3, Point4: TVec2): Boolean;
begin
  Result := SimpleIntersect(Point1[0], Point1[1], Point2[0], Point2[1], Point3[0], Point3[1], Point4[0], Point4[1]);
end;

function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
var
  UpperX: TGeoFloat;
  UpperY: TGeoFloat;
  LowerX: TGeoFloat;
  LowerY: TGeoFloat;
  Ax: TGeoFloat;
  Bx: TGeoFloat;
  Cx: TGeoFloat;
  Ay: TGeoFloat;
  By: TGeoFloat;
  Cy: TGeoFloat;
  d: TGeoFloat;
  f: TGeoFloat;
  E: TGeoFloat;
begin
  Result := False;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < Zero then
    begin
      LowerX := x2;
      UpperX := x1;
    end
  else
    begin
      UpperX := x2;
      LowerX := x1;
    end;

  if Bx > Zero then
    begin
      if (UpperX < x4) or (x3 < LowerX) then
          Exit;
    end
  else if (UpperX < x3) or (x4 < LowerX) then
      Exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < Zero then
    begin
      LowerY := y2;
      UpperY := y1;
    end
  else
    begin
      UpperY := y2;
      LowerY := y1;
    end;

  if By > Zero then
    begin
      if (UpperY < y4) or (y3 < LowerY) then
          Exit;
    end
  else if (UpperY < y3) or (y4 < LowerY) then
      Exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d := (By * Cx) - (Bx * Cy);
  f := (Ay * Bx) - (Ax * By);

  if f > Zero then
    begin
      if (d < Zero) or (d > f) then
          Exit;
    end
  else if (d > Zero) or (d < f) then
      Exit;

  E := (Ax * Cy) - (Ay * Cx);

  if f > Zero then
    begin
      if (E < Zero) or (E > f) then
          Exit;
    end
  else if (E > Zero) or (E < f) then
      Exit;

  Result := True;
end;

function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out ix, iy: TGeoFloat): Boolean;
var
  UpperX: TGeoFloat;
  UpperY: TGeoFloat;
  LowerX: TGeoFloat;
  LowerY: TGeoFloat;
  Ax: TGeoFloat;
  Bx: TGeoFloat;
  Cx: TGeoFloat;
  Ay: TGeoFloat;
  By: TGeoFloat;
  Cy: TGeoFloat;
  d: TGeoFloat;
  f: TGeoFloat;
  E: TGeoFloat;
  Ratio: TGeoFloat;
begin
  Result := False;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < Zero then
    begin
      LowerX := x2;
      UpperX := x1;
    end
  else
    begin
      UpperX := x2;
      LowerX := x1;
    end;

  if Bx > Zero then
    begin
      if (UpperX < x4) or (x3 < LowerX) then
          Exit;
    end
  else if (UpperX < x3) or (x4 < LowerX) then
      Exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < Zero then
    begin
      LowerY := y2;
      UpperY := y1;
    end
  else
    begin
      UpperY := y2;
      LowerY := y1;
    end;

  if By > Zero then
    begin
      if (UpperY < y4) or (y3 < LowerY) then
          Exit;
    end
  else if (UpperY < y3) or (y4 < LowerY) then
      Exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d := (By * Cx) - (Bx * Cy);
  f := (Ay * Bx) - (Ax * By);

  if f > Zero then
    begin
      if (d < Zero) or (d > f) then
          Exit;
    end
  else if (d > Zero) or (d < f) then
      Exit;

  E := (Ax * Cy) - (Ay * Cx);

  if f > Zero then
    begin
      if (E < Zero) or (E > f) then
          Exit;
    end
  else if (E > Zero) or (E < f) then
      Exit;

  Result := True;

  (*
    From IntersectionPoint Routine

    dx1 := x2 - x1; ->  Ax
    dx2 := x4 - x3; -> -Bx
    dx3 := x1 - x3; ->  Cx

    dy1 := y2 - y1; ->  Ay
    dy2 := y1 - y3; ->  Cy
    dy3 := y4 - y3; -> -By
  *)

  Ratio := (Ax * -By) - (Ay * -Bx);

  if NotEqual(Ratio, Zero) then
    begin
      Ratio := ((Cy * -Bx) - (Cx * -By)) / Ratio;
      ix := x1 + (Ratio * Ax);
      iy := y1 + (Ratio * Ay);
    end
  else
    begin
      if IsEqual((Ax * -Cy), (-Cx * Ay)) then
        begin
          ix := x3;
          iy := y3;
        end
      else
        begin
          ix := x4;
          iy := y4;
        end;
    end;
end;

function Intersect(const pt1, pt2, pt3, pt4: TVec2; out pt: TVec2): Boolean;
begin
  Result := Intersect(pt1[0], pt1[1], pt2[0], pt2[1], pt3[0], pt3[1], pt4[0], pt4[1], pt[0], pt[1]);
end;

function Intersect(const pt1, pt2, pt3, pt4: TVec2): Boolean;
begin
  Result := Intersect(pt1[0], pt1[1], pt2[0], pt2[1], pt3[0], pt3[1], pt4[0], pt4[1]);
end;

function PointInCircle(const pt, cp: TVec2; radius: TGeoFloat): Boolean;
begin
  Result := (PointLayDistance(pt, cp) <= (radius * radius));
end;

function PointInTriangle(const Px, Py, x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean;
var
  Or1, Or2, Or3: Integer;
begin
  Or1 := Orientation(x1, y1, x2, y2, Px, Py);
  Or2 := Orientation(x2, y2, x3, y3, Px, Py);

  if (Or1 * Or2) = -1 then
      Result := False
  else
    begin
      Or3 := Orientation(x3, y3, x1, y1, Px, Py);
      if (Or1 = Or3) or (Or3 = 0) then
          Result := True
      else if Or1 = 0 then
          Result := (Or2 * Or3) >= 0
      else if Or2 = 0 then
          Result := (Or1 * Or3) >= 0
      else
          Result := False;
    end;
end;

procedure BuildSinCosCache(const oSin, oCos: PGeoFloatArray; const b, E: TGeoFloat);
var
  i: Integer;
  startAngle, stopAngle, d, alpha, beta: TGeoFloat;
begin
  startAngle := b;
  stopAngle := E + 1E-5;
  if high(oSin^) > low(oSin^) then
      d := PIDiv180 * (stopAngle - startAngle) / (high(oSin^) - low(oSin^))
  else
      d := 0;

  if high(oSin^) - low(oSin^) < 1000 then
    begin
      // Fast computation (approx 5.5x)
      alpha := 2 * Sqr(Sin(d * 0.5));
      beta := Sin(d);
      SinCos(startAngle * PIDiv180, oSin^[low(oSin^)], oCos^[low(oSin^)]);
      for i := low(oSin^) to high(oSin^) - 1 do
        begin
          // Make use of the incremental formulae:
          // cos (theta+delta) = cos(theta) - [alpha*cos(theta) + beta*sin(theta)]
          // sin (theta+delta) = sin(theta) - [alpha*sin(theta) - beta*cos(theta)]
          oCos^[i + 1] := oCos^[i] - alpha * oCos^[i] - beta * oSin^[i];
          oSin^[i + 1] := oSin^[i] - alpha * oSin^[i] + beta * oCos^[i];
        end;
    end
  else
    begin
      // Slower, but maintains precision when steps are small
      startAngle := startAngle * PIDiv180;
      for i := low(oSin^) to high(oSin^) do
          SinCos((i - low(oSin^)) * d + startAngle, oSin^[i], oCos^[i]);
    end;
end;

procedure ClosestPointOnSegmentFromPoint(const x1, y1, x2, y2, Px, Py: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  Vx: TGeoFloat;
  Vy: TGeoFloat;
  Wx: TGeoFloat;
  Wy: TGeoFloat;
  c1: TGeoFloat;
  c2: TGeoFloat;
  Ratio: TGeoFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Wx := Px - x1;
  Wy := Py - y1;

  c1 := Vx * Wx + Vy * Wy;

  if c1 <= 0.0 then
    begin
      Nx := x1;
      Ny := y1;
      Exit;
    end;

  c2 := Vx * Vx + Vy * Vy;

  if c2 <= c1 then
    begin
      Nx := x2;
      Ny := y2;
      Exit;
    end;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;

function ClosestPointOnSegmentFromPoint(const lb, le, pt: TVec2): TVec2;
begin
  ClosestPointOnSegmentFromPoint(lb[0], lb[1], le[0], le[1], pt[0], pt[1], Result[0], Result[1]);
end;

function MinimumDistanceFromPointToLine(const Px, Py, x1, y1, x2, y2: TGeoFloat): TGeoFloat;
var
  Nx: TGeoFloat;
  Ny: TGeoFloat;
begin
  ClosestPointOnSegmentFromPoint(x1, y1, x2, y2, Px, Py, Nx, Ny);
  Result := Distance(Px, Py, Nx, Ny);
end;

function Quadrant(const Angle: TGeoFloat): Integer;
begin
  Result := 0;
  if (Angle >= 0.0) and (Angle < 90.0) then
      Result := 1
  else if (Angle >= 90.0) and (Angle < 180.0) then
      Result := 2
  else if (Angle >= 180.0) and (Angle < 270.0) then
      Result := 3
  else if (Angle >= 270.0) and (Angle < 360.0) then
      Result := 4
  else if Angle = 360.0 then
      Result := 1;
end;

procedure ProjectPoint(const Srcx, Srcy, Dstx, Dsty, Dist: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  DistRatio: TGeoFloat;
begin
  DistRatio := Dist / Distance(Srcx, Srcy, Dstx, Dsty);
  Nx := Srcx + DistRatio * (Dstx - Srcx);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
end;

procedure ProjectPoint(const Srcx, Srcy, Srcz, Dstx, Dsty, Dstz, Dist: TGeoFloat; out Nx, Ny, Nz: TGeoFloat);
var
  DistRatio: TGeoFloat;
begin
  DistRatio := Dist / Distance(Srcx, Srcy, Srcz, Dstx, Dsty, Dstz);
  Nx := Srcx + DistRatio * (Dstx - Srcx);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
  Nz := Srcz + DistRatio * (Dstz - Srcz);
end;
(* End of Project Point 3D *)

procedure ProjectPoint(const Px, Py, Angle, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  dx: TGeoFloat;
  dy: TGeoFloat;
begin
  dx := Zero;
  dy := Zero;
  case Quadrant(Angle) of
    1:
      begin
        dx := Cos(Angle * PIDiv180) * Distance;
        dy := Sin(Angle * PIDiv180) * Distance;
      end;
    2:
      begin
        dx := Sin((Angle - 90.0) * PIDiv180) * Distance * -1.0;
        dy := Cos((Angle - 90.0) * PIDiv180) * Distance;
      end;
    3:
      begin
        dx := Cos((Angle - 180.0) * PIDiv180) * Distance * -1.0;
        dy := Sin((Angle - 180.0) * PIDiv180) * Distance * -1.0;
      end;
    4:
      begin
        dx := Sin((Angle - 270.0) * PIDiv180) * Distance;
        dy := Cos((Angle - 270.0) * PIDiv180) * Distance * -1.0;
      end;
  end;
  Nx := Px + dx;
  Ny := Py + dy;
end;

procedure ProjectPoint0(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
begin
  Nx := Px + Distance;
  Ny := Py;
end;

procedure ProjectPoint45(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
begin
  Nx := Px + 0.70710678118654752440084436210485 * Distance;
  Ny := Py + 0.70710678118654752440084436210485 * Distance;
end;

procedure ProjectPoint90(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
begin
  Nx := Px;
  Ny := Py + Distance;
end;

procedure ProjectPoint135(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
begin
  Nx := Px - 0.70710678118654752440084436210485 * Distance;
  Ny := Py + 0.70710678118654752440084436210485 * Distance;
end;

procedure ProjectPoint180(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
begin
  Nx := Px - Distance;
  Ny := Py;
end;

procedure ProjectPoint225(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
begin
  Nx := Px - 0.70710678118654752440084436210485 * Distance;
  Ny := Py - 0.70710678118654752440084436210485 * Distance;
end;

procedure ProjectPoint270(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
begin
  Nx := Px;
  Ny := Py - Distance;
end;

procedure ProjectPoint315(const Px, Py, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
begin
  Nx := Px + 0.70710678118654752440084436210485 * Distance;
  Ny := Py - 0.70710678118654752440084436210485 * Distance;
end;

function ProjectPoint0(const Point: TVec2; const Distance: TGeoFloat): TVec2;
begin
  ProjectPoint0(Point[0], Point[1], Distance, Result[0], Result[1]);
end;

function ProjectPoint45(const Point: TVec2; const Distance: TGeoFloat): TVec2;
begin
  ProjectPoint45(Point[0], Point[1], Distance, Result[0], Result[1]);
end;

function ProjectPoint90(const Point: TVec2; const Distance: TGeoFloat): TVec2;
begin
  ProjectPoint90(Point[0], Point[1], Distance, Result[0], Result[1]);
end;

function ProjectPoint135(const Point: TVec2; const Distance: TGeoFloat): TVec2;
begin
  ProjectPoint135(Point[0], Point[1], Distance, Result[0], Result[1]);
end;

function ProjectPoint180(const Point: TVec2; const Distance: TGeoFloat): TVec2;
begin
  ProjectPoint180(Point[0], Point[1], Distance, Result[0], Result[1]);
end;

function ProjectPoint225(const Point: TVec2; const Distance: TGeoFloat): TVec2;
begin
  ProjectPoint225(Point[0], Point[1], Distance, Result[0], Result[1]);
end;

function ProjectPoint270(const Point: TVec2; const Distance: TGeoFloat): TVec2;
begin
  ProjectPoint270(Point[0], Point[1], Distance, Result[0], Result[1]);
end;

function ProjectPoint315(const Point: TVec2; const Distance: TGeoFloat): TVec2;
begin
  ProjectPoint315(Point[0], Point[1], Distance, Result[0], Result[1]);
end;

function GetCicleRadiusInPolyEndge(r: TGeoFloat; PolySlices: Integer): TGeoFloat;
begin
  Result := r / Sin((180 - 360 / PolySlices) * 0.5 / 180 * pi);
end;

procedure Circle2LineIntersectionPoint(const lb, le, cp: TVec2; const radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: Integer; out pt1, pt2: TVec2);
var
  Px: TGeoFloat;
  Py: TGeoFloat;
  S1In: Boolean;
  s2In: Boolean;
  h: TGeoFloat;
  a: TGeoFloat;
begin
  ICnt := 0;

  S1In := PointInCircle(lb, cp, radius);
  s2In := PointInCircle(le, cp, radius);

  if S1In and s2In then
    begin
      ICnt := 2;
      pt1 := lb;
      pt2 := le;
      pt1in := True;
      pt2in := True;
      Exit;
    end;

  if S1In or s2In then
    begin
      pt1in := True;
      pt2in := False;
      ICnt := 2;
      ClosestPointOnSegmentFromPoint(lb[0], lb[1], le[0], le[1], cp[0], cp[1], Px, Py);
      if S1In then
        begin
          h := Distance(Px, Py, cp[0], cp[1]);
          a := Sqrt((radius * radius) - (h * h));
          pt1 := lb;
          ProjectPoint(Px, Py, le[0], le[1], a, pt2[0], pt2[1]);
        end
      else if s2In then
        begin
          h := Distance(Px, Py, cp[0], cp[1]);
          a := Sqrt((radius * radius) - (h * h));
          pt1 := le;
          ProjectPoint(Px, Py, lb[0], lb[1], a, pt2[0], pt2[1]);
        end;
      Exit;
    end;

  pt1in := False;
  pt2in := False;

  ClosestPointOnSegmentFromPoint(lb[0], lb[1], le[0], le[1], cp[0], cp[1], Px, Py);

  if (IsEqual(lb[0], Px) and IsEqual(lb[1], Py)) or (IsEqual(le[0], Px) and IsEqual(le[1], Py)) then
      Exit
  else
    begin
      h := Distance(Px, Py, cp[0], cp[1]);
      if h > radius then
          Exit
      else if IsEqual(h, radius) then
        begin
          ICnt := 1;
          pt1[0] := Px;
          pt1[1] := Py;
          Exit;
        end
      else if IsEqual(h, Zero) then
        begin
          ICnt := 2;
          ProjectPoint(cp[0], cp[1], lb[0], lb[1], radius, pt1[0], pt1[1]);
          ProjectPoint(cp[0], cp[1], le[0], le[1], radius, pt2[0], pt2[1]);
          Exit;
        end
      else
        begin
          ICnt := 2;
          a := Sqrt((radius * radius) - (h * h));
          ProjectPoint(Px, Py, lb[0], lb[1], a, pt1[0], pt1[1]);
          ProjectPoint(Px, Py, le[0], le[1], a, pt2[0], pt2[1]);
          Exit;
        end;
    end;
end;

procedure Circle2CircleIntersectionPoint(const cp1, cp2: TVec2; const r1, r2: TGeoFloat; out Point1, Point2: TVec2);
var
  Dist: TGeoFloat;
  a: TGeoFloat;
  h: TGeoFloat;
  RatioA: TGeoFloat;
  RatioH: TGeoFloat;
  dx: TGeoFloat;
  dy: TGeoFloat;
  Phi: TVec2;
  r1Sqr: TGeoFloat;
  r2Sqr: TGeoFloat;
  dstSqr: TGeoFloat;
begin
  Dist := Distance(cp1[0], cp1[1], cp2[0], cp2[1]);

  dstSqr := Dist * Dist;
  r1Sqr := r1 * r1;
  r2Sqr := r2 * r2;

  a := (dstSqr - r2Sqr + r1Sqr) / (2 * Dist);
  h := Sqrt(r1Sqr - (a * a));

  RatioA := a / Dist;
  RatioH := h / Dist;

  dx := cp2[0] - cp1[0];
  dy := cp2[1] - cp1[1];

  Phi[0] := cp1[0] + (RatioA * dx);
  Phi[1] := cp1[1] + (RatioA * dy);

  dx := dx * RatioH;
  dy := dy * RatioH;

  Point1[0] := Phi[0] + dy;
  Point1[1] := Phi[1] - dx;

  Point2[0] := Phi[0] - dy;
  Point2[1] := Phi[1] + dx;
end;

function Detect_Circle2Circle(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean;
begin
  // return point disace < sum
  Result := PointDistance(p1, p2) <= r1 + r2;
end;

function CircleCollision(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean;
begin
  // return point disace < sum
  Result := PointDistance(p1, p2) <= r1 + r2;
end;

function Detect_Circle2CirclePoint(const p1, p2: TVec2; const r1, r2: TGeoFloat; out op1, op2: TVec2): Boolean;
var
  Dist: TGeoFloat;
  a: TGeoFloat;
  h: TGeoFloat;
  RatioA: TGeoFloat;
  RatioH: TGeoFloat;
  dx: TGeoFloat;
  dy: TGeoFloat;
  Phi: TVec2;
  r1Sqr: TGeoFloat;
  r2Sqr: TGeoFloat;
  dstSqr: TGeoFloat;
begin
  Dist := Distance(p1[0], p1[1], p2[0], p2[1]);
  Result := Dist <= r1 + r2;
  if Result then
    begin
      dstSqr := Dist * Dist;
      r1Sqr := r1 * r1;
      r2Sqr := r2 * r2;

      a := (dstSqr - r2Sqr + r1Sqr) / (2 * Dist);
      h := Sqrt(r1Sqr - (a * a));

      RatioA := a / Dist;
      RatioH := h / Dist;

      dx := p2[0] - p1[0];
      dy := p2[1] - p1[1];

      Phi[0] := p1[0] + (RatioA * dx);
      Phi[1] := p1[1] + (RatioA * dy);

      dx := dx * RatioH;
      dy := dy * RatioH;

      op1[0] := Phi[0] + dy;
      op1[1] := Phi[1] - dx;

      op2[0] := Phi[0] - dy;
      op2[1] := Phi[1] + dx;
    end;
end;

// circle 2 line collision

function Detect_Circle2Line(const cp: TVec2; const r: TGeoFloat; const lb, le: TVec2): Boolean;
var
  lineCen, v1, v2: TVec2;
begin
  lineCen := PointLerp(lb, le, 0.5);
  if Detect_Circle2Circle(cp, lineCen, r, PointDistance(lb, le) * 0.5) then
    begin
      v1 := Vec2Sub(lb, cp);
      v2 := Vec2Sub(le, cp);
      Result := GreaterThanOrEqual(((r * r) * PointLayDistance(v1, v2) - Sqr(v1[0] * v2[1] - v1[1] * v2[0])), Zero);
    end
  else
      Result := False;
end;

function TVec2List.GetPoints(index: Integer): PVec2;
begin
  Result := FList[index];
end;

constructor TVec2List.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
  FUserData := nil;
  FUserObject := nil;
end;

destructor TVec2List.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TVec2List.Add(const x, y: TGeoFloat);
var
  p: PVec2;
begin
  new(p);
  p^ := PointMake(x, y);
  FList.Add(p);
end;

procedure TVec2List.Add(const pt: TVec2);
var
  p: PVec2;
begin
  new(p);
  p^ := pt;
  FList.Add(p);
end;

procedure TVec2List.Add(pt: TPoint);
var
  p: PVec2;
begin
  new(p);
  p^ := vec2(pt);
  FList.Add(p);
end;

procedure TVec2List.Add(pt: TPointf);
var
  p: PVec2;
begin
  new(p);
  p^ := vec2(pt);
  FList.Add(p);
end;

procedure TVec2List.Add(v2l: TVec2List);
var
  i: Integer;
begin
  for i := 0 to v2l.Count - 1 do
      Add(v2l[i]^);
end;

procedure TVec2List.Add(r: TRectV2);
begin
  Add(r[0][0], r[0][1]);
  Add(r[1][0], r[0][1]);
  Add(r[1][0], r[1][1]);
  Add(r[0][0], r[1][1]);
end;

procedure TVec2List.Add(r: TRect);
begin
  Add(RectV2(r));
end;

procedure TVec2List.Add(r: TRectf);
begin
  Add(RectV2(r));
end;

procedure TVec2List.AddSubdivision(nbCount: Integer; pt: TVec2);
var
  lpt: PVec2;
  i: Integer;
  t: Double;
begin
  if Count > 0 then
    begin
      lpt := FList.Last;
      t := 1.0 / nbCount;
      for i := 1 to nbCount do
          Add(PointLerp(lpt^, pt, t * i));
    end
  else
      Add(pt);
end;

procedure TVec2List.AddSubdivisionWithDistance(avgDist: TGeoFloat; pt: TVec2);
var
  lpt: PVec2;
  i, nbCount: Integer;
  t: Double;
begin
  if (Count > 0) and (PointDistance(PVec2(FList.Last)^, pt) > avgDist) then
    begin
      lpt := FList.Last;
      nbCount := Trunc(PointDistance(PVec2(FList.Last)^, pt) / avgDist);
      t := 1.0 / nbCount;
      for i := 1 to nbCount do
          Add(PointLerp(lpt^, pt, t * i));
    end;
  Add(pt);
end;

procedure TVec2List.Insert(idx: Integer; x, y: TGeoFloat);
var
  p: PVec2;
begin
  new(p);
  p^ := PointMake(x, y);
  FList.Insert(idx, p);
end;

procedure TVec2List.Delete(idx: Integer);
begin
  Dispose(PVec2(FList[idx]));
  FList.Delete(idx);
end;

procedure TVec2List.Remove(p: PVec2);
var
  i: Integer;
begin
  i := 0;
  while i < FList.Count do
    begin
      if FList[i] = p then
        begin
          Dispose(PVec2(FList[i]));
          FList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TVec2List.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PVec2(FList[i]));
  FList.Clear;
end;

function TVec2List.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TVec2List.FixedSameError;
var
  L, p: PVec2;
  i: Integer;
begin
  if Count < 2 then
      Exit;

  L := PVec2(FList[0]);
  p := PVec2(FList[Count - 1]);
  while (Count >= 2) and (IsEqual(p^, L^)) do
    begin
      Delete(Count - 1);
      p := PVec2(FList[Count - 1]);
    end;

  if Count < 2 then
      Exit;

  L := PVec2(FList[0]);
  i := 1;
  while i < Count do
    begin
      p := PVec2(FList[i]);
      if IsEqual(p^, L^) then
          Delete(i)
      else
        begin
          L := p;
          inc(i);
        end;
    end;
end;

procedure TVec2List.Assign(Source: TCoreClassPersistent);
var
  i: Integer;
begin
  if Source is TVec2List then
    begin
      Clear;
      for i := 0 to TVec2List(Source).Count - 1 do
          Add(TVec2List(Source)[i]^);
    end
  else if Source is TPoly then
    begin
      Clear;
      for i := 0 to TPoly(Source).Count - 1 do
          Add(TPoly(Source).Points[i]);
    end;
end;

procedure TVec2List.AssignFromArrayV2(arry: TArrayVec2);
var
  i: Integer;
begin
  Clear;
  for i := low(arry) to high(arry) do
      Add(arry[i]);
end;

function TVec2List.BuildArray: TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i]^;
end;

procedure TVec2List.SaveToStream(stream: TMemoryStream64);
var
  i: Integer;
  p: PVec2;
begin
  stream.WriteInt32(Count);
  for i := 0 to Count - 1 do
    begin
      p := GetPoints(i);
      stream.WriteSingle(p^[0]);
      stream.WriteSingle(p^[1]);
    end;
end;

procedure TVec2List.LoadFromStream(stream: TMemoryStream64);
var
  c: Integer;
  i: Integer;
  v: TVec2;
begin
  Clear;
  c := stream.ReadInt32;
  for i := 0 to c - 1 do
    begin
      v[0] := stream.ReadSingle;
      v[1] := stream.ReadSingle;
      Add(v);
    end;
end;

function TVec2List.BoundRect: TRectV2;
var
  p: PVec2;
  MaxX: TGeoFloat;
  MaxY: TGeoFloat;
  MinX: TGeoFloat;
  MinY: TGeoFloat;
  i: Integer;
begin
  Result := MakeRectV2(Zero, Zero, Zero, Zero);
  if Count < 2 then
      Exit;
  p := Items[0];
  MinX := p^[0];
  MaxX := p^[0];
  MinY := p^[1];
  MaxY := p^[1];

  for i := 1 to Count - 1 do
    begin
      p := Items[i];
      if p^[0] < MinX then
          MinX := p^[0]
      else if p^[0] > MaxX then
          MaxX := p^[0];
      if p^[1] < MinY then
          MinY := p^[1]
      else if p^[1] > MaxY then
          MaxY := p^[1];
    end;
  Result := MakeRectV2(MinX, MinY, MaxX, MaxY);
end;

function TVec2List.CircleRadius(ACentroid: TVec2): TGeoFloat;
var
  i: Integer;
  LayLen: TGeoFloat;
  LayDist: TGeoFloat;
begin
  Result := 0;
  if Count < 3 then
      Exit;
  LayLen := -1;
  for i := 0 to Count - 1 do
    begin
      LayDist := PointLayDistance(ACentroid, Items[i]^);
      if LayDist > LayLen then
          LayLen := LayDist;
    end;
  Result := Sqrt(LayLen);
end;

function TVec2List.Centroid: TVec2;
var
  i: Integer;
  asum: TGeoFloat;
  term: TGeoFloat;

  p1, p2: PVec2;
begin
  Result := NULLPoint;

  if Count < 3 then
      Exit;

  asum := Zero;
  p2 := Items[Count - 1];

  for i := 0 to Count - 1 do
    begin
      p1 := Items[i];

      term := ((p2^[0] * p1^[1]) - (p2^[1] * p1^[0]));
      asum := asum + term;
      Result[0] := Result[0] + (p2^[0] + p1^[0]) * term;
      Result[1] := Result[1] + (p2^[1] + p1^[1]) * term;
      p2 := p1;
    end;

  if NotEqual(asum, Zero) then
    begin
      Result[0] := Result[0] / (3.0 * asum);
      Result[1] := Result[1] / (3.0 * asum);
    end;
end;

function TVec2List.PointInHere(pt: TVec2): Boolean;
var
  i: Integer;
  pi, pj: PVec2;
begin
  Result := False;
  if Count < 3 then
      Exit;
  pj := Items[Count - 1];
  for i := 0 to Count - 1 do
    begin
      pi := Items[i];
      if ((pi^[1] <= pt[1]) and (pt[1] < pj^[1])) or  // an upward crossing
        ((pj^[1] <= pt[1]) and (pt[1] < pi^[1])) then // a downward crossing
        begin
          (* compute the edge-ray intersect @ the x-coordinate *)
          if (pt[0] - pi^[0] < ((pj^[0] - pi^[0]) * (pt[1] - pi^[1]) / (pj^[1] - pi^[1]))) then
              Result := not Result;
        end;
      pj := pi;
    end;
end;

procedure TVec2List.RotateAngle(axis: TVec2; Angle: TGeoFloat);
var
  i: Integer;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      p^ := PointRotation(axis, p^, PointAngle(axis, p^) + Angle);
    end;
end;

procedure TVec2List.Scale(axis: TVec2; Scale: TGeoFloat);
var
  i: Integer;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      p^ := PointRotation(axis, PointDistance(axis, p^) * Scale, PointAngle(axis, p^));
    end;
end;

procedure TVec2List.ConvexHull(output: TVec2List);

const
  RightHandSide = -1;
  LeftHandSide = +1;
  CounterClockwise = +1;
  CollinearOrientation = 0;

type
  T2DHullPoint = record
    x: TGeoFloat;
    y: TGeoFloat;
    Ang: TGeoFloat;
  end;

  TCompareResult = (eGreaterThan, eLessThan, eEqual);

var
  Point: array of T2DHullPoint;
  Stack: array of T2DHullPoint;
  StackHeadPosition: Integer;
  Anchor: T2DHullPoint;

  function CartesianAngle(const x, y: TGeoFloat): TGeoFloat;
  const
    _180DivPI = 57.295779513082320876798154814105000;
  begin
    if (x > Zero) and (y > Zero) then
        Result := (ArcTan(y / x) * _180DivPI)
    else if (x < Zero) and (y > Zero) then
        Result := (ArcTan(-x / y) * _180DivPI) + 90.0
    else if (x < Zero) and (y < Zero) then
        Result := (ArcTan(y / x) * _180DivPI) + 180.0
    else if (x > Zero) and (y < Zero) then
        Result := (ArcTan(-x / y) * _180DivPI) + 270.0
    else if (x = Zero) and (y > Zero) then
        Result := 90.0
    else if (x < Zero) and (y = Zero) then
        Result := 180.0
    else if (x = Zero) and (y < Zero) then
        Result := 270.0
    else
        Result := Zero;
  end;

  procedure Swap(i, j: Integer; var Point: array of T2DHullPoint);
  var
    Temp: T2DHullPoint;
  begin
    Temp := Point[i];
    Point[i] := Point[j];
    Point[j] := Temp;
  end;

  function hEqual(const p1, p2: T2DHullPoint): Boolean;
  begin
    Result := IsEqual(p1.x, p2.x) and IsEqual(p1.y, p2.y);
  end;

  function CompareAngles(const p1, p2: T2DHullPoint): TCompareResult;
  begin
    if p1.Ang < p2.Ang then
        Result := eLessThan
    else if p1.Ang > p2.Ang then
        Result := eGreaterThan
    else if hEqual(p1, p2) then
        Result := eEqual
    else if Distance(Anchor.x, Anchor.y, p1.x, p1.y) < Distance(Anchor.x, Anchor.y, p2.x, p2.y) then
        Result := eLessThan
    else
        Result := eGreaterThan;
  end;

  procedure RQSort(Left, Right: Integer; var Point: array of T2DHullPoint);
  var
    i: Integer;
    j: Integer;
    Middle: Integer;
    Pivot: T2DHullPoint;
  begin
    repeat
      i := Left;
      j := Right;
      Middle := (Left + Right) div 2;
      (* Median of 3 Pivot Selection *)
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      if CompareAngles(Point[Right], Point[Middle]) = eLessThan then
          Swap(Right, Middle, Point);
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      Pivot := Point[Right];
      repeat
        while CompareAngles(Point[i], Pivot) = eLessThan do
            inc(i);
        while CompareAngles(Point[j], Pivot) = eGreaterThan do
            dec(j);
        if i <= j then
          begin
            Swap(i, j, Point);
            inc(i);
            dec(j);
          end;
      until i > j;
      if Left < j then
          RQSort(Left, j, Point);
      Left := i;
    until i >= Right;
  end;

  procedure Push(Pnt: T2DHullPoint);
  begin
    inc(StackHeadPosition);
    Stack[StackHeadPosition] := Pnt;
  end;

  function Pop: Boolean;
  begin
    Result := False;
    if StackHeadPosition >= 0 then
      begin
        Result := True;
        dec(StackHeadPosition);
      end;
  end;

  function Head: T2DHullPoint;
  begin
    Assert((StackHeadPosition >= 0) and (StackHeadPosition < length(Stack)), 'Invalid stack-head position.');
    Result := Stack[StackHeadPosition];
  end;

  function PreHead: T2DHullPoint;
  begin
    Assert(((StackHeadPosition - 1) >= 0) and ((StackHeadPosition - 1) < length(Stack)), 'Invalid pre stack-head position.');
    Result := Stack[StackHeadPosition - 1];
  end;

  function PreHeadExist: Boolean;
  begin
    Result := (StackHeadPosition > 0);
  end;

  function Orientation(p1, p2, p3: T2DHullPoint): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Orientation2(const x1, y1, x2, y2, Px, Py: TGeoFloat): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    var
      Orin: TGeoFloat;
    begin
      (* Determinant of the 3 points *)
      Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);
      if Orin > Zero then
          Result := LeftHandSide (* Orientaion is to the left-hand side *)
      else if Orin < Zero then
          Result := RightHandSide (* Orientaion is to the right-hand side *)
      else
          Result := CollinearOrientation; (* Orientaion is neutral aka collinear *)
    end;

  begin
    Result := Orientation2(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
  end;

  procedure GrahamScan;
  var
    i: Integer;
    Orin: Integer;
  begin
    Push(Point[0]);
    Push(Point[1]);
    i := 2;
    while i < length(Point) do
      begin
        if PreHeadExist then
          begin
            Orin := Orientation(PreHead, Head, Point[i]);
            if Orin = CounterClockwise then
              begin
                Push(Point[i]);
                inc(i);
              end
            else
                Pop;
          end
        else
          begin
            Push(Point[i]);
            inc(i);
          end;
      end;
  end;

var
  i: Integer;
  j: Integer;
  p: PVec2;
begin
  if Count <= 3 then
    begin
      for i := 0 to Count - 1 do
          output.Add(Items[i]^);
      Exit;
    end;
  StackHeadPosition := -1;

  try
    SetLength(Point, Count);
    SetLength(Stack, Count);
    j := 0;
    for i := 0 to Count - 1 do
      begin
        p := Items[i];
        Point[i].x := p^[0];
        Point[i].y := p^[1];
        Point[i].Ang := 0.0;
        if Point[i].y < Point[j].y then
            j := i
        else if Point[i].y = Point[j].y then
          if Point[i].x < Point[j].x then
              j := i;
      end;

    Swap(0, j, Point);
    Point[0].Ang := 0;
    Anchor := Point[0];
    (* Calculate angle of the vertex ([ith point]-[anchorpoint]-[most left point]) *)
    for i := 1 to length(Point) - 1 do
        Point[i].Ang := CartesianAngle(Point[i].x - Anchor.x, Point[i].y - Anchor.y);
    (* Sort points in ascending order according to their angles *)
    RQSort(1, length(Point) - 1, Point);
    GrahamScan;
    (* output list *)
    for i := 0 to StackHeadPosition do
        output.Add(Stack[i].x, Stack[i].y);
  finally
    (* Final clean-up *)
    Finalize(Stack);
    Finalize(Point);
  end;
end;

procedure TVec2List.ConvexHull;
var
  nl: TVec2List;
  L: TCoreClassList;
begin
  nl := TVec2List.Create;
  ConvexHull(nl);
  L := FList;
  FList := nl.FList;
  nl.FList := L;
  DisposeObject(nl);
end;

procedure TVec2List.SplineSmooth(output: TVec2List; DetailLevel: TGeoFloat);
type
  TSplineMatrix = array of array [0 .. 4] of TGeoFloat;
  TSplineVector = array [0 .. (MaxInt div SizeOf(TGeoFloat)) - 1] of TGeoFloat;
  PSplineVector = ^TSplineVector;

  procedure VECCholeskyTriDiagResol(const b: array of TGeoFloat; const nb: Integer; var Result: array of TGeoFloat);
  var
    y, LDiag, LssDiag: array of TGeoFloat;
    i, k, Debut, Fin: Integer;
  begin
    Debut := 0;
    Fin := nb - 1;
    SetLength(LDiag, nb);
    SetLength(LssDiag, nb - 1);
    LDiag[Debut] := 1.4142135;
    LssDiag[Debut] := 1.0 / 1.4142135;
    for k := Debut + 1 to Fin - 1 do
      begin
        LDiag[k] := Sqrt(4 - LssDiag[k - 1] * LssDiag[k - 1]);
        LssDiag[k] := 1.0 / LDiag[k];
      end;
    LDiag[Fin] := Sqrt(2 - LssDiag[Fin - 1] * LssDiag[Fin - 1]);
    SetLength(y, nb);
    y[Debut] := b[Debut] / LDiag[Debut];
    for i := Debut + 1 to Fin do
        y[i] := (b[i] - y[i - 1] * LssDiag[i - 1]) / LDiag[i];
    Result[Fin] := y[Fin] / LDiag[Fin];
    for i := Fin - 1 downto Debut do
        Result[i] := (y[i] - Result[i + 1] * LssDiag[i]) / LDiag[i];
  end;

  procedure MATRIX_InterpolationHermite(const ordonnees: PSplineVector; const nb: Integer; var Result: TSplineMatrix); inline;
  var
    a, b, c, d: TGeoFloat;
    i, n: Integer;
    bb, deriv: array of TGeoFloat;
  begin
    Result := nil;
    if Assigned(ordonnees) and (nb > 0) then
      begin
        n := nb - 1;
        SetLength(bb, nb);
        bb[0] := 3 * (ordonnees^[1] - ordonnees^[0]);
        bb[n] := 3 * (ordonnees^[n] - ordonnees^[n - 1]);
        for i := 1 to n - 1 do
            bb[i] := 3 * (ordonnees^[i + 1] - ordonnees^[i - 1]);
        SetLength(deriv, nb);
        VECCholeskyTriDiagResol(bb, nb, deriv);
        SetLength(Result, n);
        for i := 0 to n - 1 do
          begin
            a := ordonnees^[i];
            b := deriv[i];
            c := 3 * (ordonnees^[i + 1] - ordonnees^[i]) - 2 * deriv[i] - deriv[i + 1];
            d := -2 * (ordonnees^[i + 1] - ordonnees^[i]) + deriv[i] + deriv[i + 1];
            Result[i][3] := a + i * (i * (c - i * d) - b);
            Result[i][2] := b + i * (3 * i * d - 2 * c);
            Result[i][1] := c - 3 * i * d;
            Result[i][0] := d;
          end;
      end;
  end;

  function MATRIX_ValeurSpline(const spline: TSplineMatrix; const x: TGeoFloat; const nb: Integer): TGeoFloat;
  var
    i: Integer;
  begin
    if length(spline) > 0 then
      begin
        if x <= 0 then
            i := 0
        else if x > nb - 1 then
            i := nb - 1
        else
            i := Trunc(x);
        if i = (nb - 1) then
            dec(i);
        Result := ((spline[i][0] * x + spline[i][1]) * x + spline[i][2]) * x + spline[i][3];
      end
    else
        Result := 0;
  end;

var
  nl: TVec2List;
  matX, matY: TSplineMatrix;
  FNb: Integer;
  i: Integer;
  xa, ya: PSplineVector;
  x, y: TGeoFloat;
  p: PVec2;
begin
  if Count < 3 then
    begin
      output.Assign(Self);
      Exit;
    end;

  GetMem(xa, SizeOf(TGeoFloat) * Count);
  GetMem(ya, SizeOf(TGeoFloat) * Count);

  for i := 0 to Count - 1 do
    begin
      p := GetPoints(i);
      xa^[i] := p^[0];
      ya^[i] := p^[1];
    end;

  MATRIX_InterpolationHermite(xa, Count, matX);
  MATRIX_InterpolationHermite(ya, Count, matY);

  FNb := Count;

  for i := 0 to Round((FNb - 1) * DetailLevel) do
    begin
      x := MATRIX_ValeurSpline(matX, i * (1.0 / DetailLevel), FNb);
      y := MATRIX_ValeurSpline(matY, i * (1.0 / DetailLevel), FNb);
      output.Add(x, y);
    end;

  FreeMem(xa);
  FreeMem(ya);
end;

procedure TVec2List.SplineSmooth(DetailLevel: TGeoFloat);
var
  nl: TVec2List;
  L: TCoreClassList;
begin
  nl := TVec2List.Create;
  SplineSmooth(nl, DetailLevel);
  L := FList;
  FList := nl.FList;
  nl.FList := L;
  DisposeObject(nl);
end;

procedure TVec2List.ExtractToBuff(var output: TArrayVec2);
var
  i: Integer;
begin
  SetLength(output, Count);
  for i := 0 to Count - 1 do
      output[i] := Items[i]^;
end;

procedure TVec2List.GiveListDataFromBuff(output: TArrayVec2);
var
  i: Integer;
begin
  Clear;
  for i := low(output) to high(output) do
      Add(output[i]);
end;

procedure TVec2List.VertexReduction(Epsilon: TGeoFloat);
var
  buff, output: TArrayVec2;
begin
  ExtractToBuff(buff);
  FastVertexReduction(buff, Epsilon, output);
  GiveListDataFromBuff(output);
end;

function TVec2List.Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean; OutputPoint: TVec2List): Boolean;
var
  i: Integer;
  p1, p2: PVec2;
  ox, oy: TGeoFloat;
begin
  Result := False;
  if FList.Count > 1 then
    begin
      p1 := FList[0];
      for i := 1 to FList.Count - 1 do
        begin
          p2 := FList[i];
          if OutputPoint <> nil then
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
                begin
                  OutputPoint.Add(ox, oy);
                  Result := True;
                end;
            end
          else
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
                  Result := True;
            end;
          p1 := p2;
        end;
      if ClosedPolyMode then
        begin
          p2 := FList[0];
          if OutputPoint <> nil then
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
                begin
                  OutputPoint.Add(ox, oy);
                  Result := True;
                end;
            end
          else
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
                  Result := True;
            end;
        end;
    end;
end;

function TVec2List.Line2NearIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: Integer; out IntersectPt: TVec2): Boolean;
var
  i: Integer;
  p1, p2: PVec2;
  ox, oy: TGeoFloat;
  d, d2: TGeoFloat;
begin
  Result := False;
  if FList.Count > 1 then
    begin
      p1 := FList[0];
      d := 0.0;
      for i := 1 to FList.Count - 1 do
        begin
          p2 := FList[i];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
            begin
              d2 := PointDistance(lb, PointMake(ox, oy));
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := PointMake(ox, oy);
                  d := d2;
                  idx1 := i - 1;
                  idx2 := i;
                  Result := True;
                end;
            end;
          p1 := p2;
        end;
      if ClosedPolyMode then
        begin
          p2 := FList[0];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
            begin
              d2 := PointDistance(lb, PointMake(ox, oy));
              if (d = 0) or (d2 < d) then
                begin
                  IntersectPt := PointMake(ox, oy);
                  // d := d2;
                  idx1 := FList.Count - 1;
                  idx2 := 0;
                  Result := True;
                end;
            end;
        end;
    end;
end;

procedure TVec2List.SortOfNear(const pt: TVec2);

  function ListSortCompare(Item1, Item2: Pointer): Integer;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := PointDistance(PVec2(Item1)^, pt);
    d2 := PointDistance(PVec2(Item2)^, pt);
    Result := CompareValue(d1, d2);
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer);
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

begin
  if Count > 1 then
      QuickSortList(FList.ListData^, 0, Count - 1);
end;

procedure TVec2List.SortOfFar(const pt: TVec2);

  function ListSortCompare(Item1, Item2: Pointer): Integer;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := PointDistance(PVec2(Item1)^, pt);
    d2 := PointDistance(PVec2(Item2)^, pt);
    Result := CompareValue(d2, d1);
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer);
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

begin
  if Count > 1 then
      QuickSortList(FList.ListData^, 0, Count - 1);
end;

procedure TVec2List.Reverse;
var
  NewList: TCoreClassList;
  i, c: Integer;
begin
  NewList := TCoreClassList.Create;
  c := Count - 1;
  NewList.Count := c + 1;
  for i := c downto 0 do
      NewList[c - i] := FList[i];
  DisposeObject(FList);
  FList := NewList;
end;

procedure TVec2List.AddCirclePoint(aCount: Cardinal; axis: TVec2; ADist: TGeoFloat);
var
  i: Integer;
begin
  for i := 0 to aCount - 1 do
      Add(PointRotation(axis, ADist, 360 / aCount * i));
end;

procedure TVec2List.AddRectangle(r: TRectV2);
begin
  Add(r[0][0], r[0][1]);
  Add(r[1][0], r[0][1]);
  Add(r[1][0], r[1][1]);
  Add(r[0][0], r[1][1]);
end;

function TVec2List.GetMinimumFromPointToLine(const pt: TVec2; const ClosedMode: Boolean; out lb, le: Integer): TVec2;
var
  i: Integer;
  pt1, pt2: PVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Items[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Items[i];

          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);

          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
              lb := i - 1;
              le := i;
            end;

          pt1 := pt2;
        end;
      if ClosedMode then
        begin
          pt2 := Items[0];
          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              lb := FList.Count - 1;
              le := 0;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Items[0]^;
          lb := 0;
          le := 0;
        end
      else
        begin
          Result := NULLPoint;
          lb := -1;
          le := -1;
        end;
    end;
end;

function TVec2List.GetMinimumFromPointToLine(const pt: TVec2; const ClosedMode: Boolean): TVec2;
var
  i: Integer;
  pt1, pt2: PVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Items[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Items[i];

          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);

          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
            end;

          pt1 := pt2;
        end;
      if ClosedMode then
        begin
          pt2 := Items[0];
          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Items[0]^;
        end
      else
        begin
          Result := NULLPoint;
        end;
    end;
end;

function TVec2List.GetMinimumFromPointToLine(const pt: TVec2; const ExpandDist: TGeoFloat): TVec2;
var
  i: Integer;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Expands[0, ExpandDist];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, ExpandDist];

          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);

          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
            end;

          pt1 := pt2;
        end;

      pt2 := Expands[0, ExpandDist];
      opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
      d2 := PointDistance(pt, opt);
      if (d = 0.0) or (d2 < d) then
        begin
          Result := opt;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Items[0]^;
        end
      else
        begin
          Result := NULLPoint;
        end;
    end;
end;

procedure TVec2List.CutLineBeginPtToIdx(const pt: TVec2; const toidx: Integer);
var
  i: Integer;
begin
  for i := 0 to toidx - 2 do
      Delete(0);
  Items[0]^ := pt;
end;

procedure TVec2List.Transform(x, y: TGeoFloat);
var
  i: Integer;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      p^[0] := p^[0] + x;
      p^[1] := p^[1] + y;
    end;
end;

procedure TVec2List.Mul(x, y: TGeoFloat);
var
  i: Integer;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      p^[0] := p^[0] * x;
      p^[1] := p^[1] * y;
    end;
end;

procedure TVec2List.FDiv(x, y: TGeoFloat);
var
  i: Integer;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      p^[0] := p^[0] / x;
      p^[1] := p^[1] / y;
    end;
end;

function TVec2List.First: PVec2;
begin
  if Count > 0 then
      Result := Items[0]
  else
      Result := nil;
end;

function TVec2List.Last: PVec2;
begin
  if Count > 0 then
      Result := Items[Count - 1]
  else
      Result := nil;
end;

procedure TVec2List.ExpandDistanceAsList(ExpandDist: TGeoFloat; output: TVec2List);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      output.Add(GetExpands(i, ExpandDist));
end;

procedure TVec2List.ExpandConvexHullAsList(ExpandDist: TGeoFloat; output: TVec2List);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  ConvexHull(pl);
  pl.ExpandDistanceAsList(ExpandDist, output);
  DisposeObject(pl);
end;

function TVec2List.GetExpands(idx: Integer; ExpandDist: TGeoFloat): TVec2;
var
  lpt, pt, rpt: TVec2;
  ln, rn: TVec2;
  dx, dy, f, r: TGeoFloat;
  Cx, Cy: TGeoFloat;
begin
  if (ExpandDist = 0) or (Count < 2) then
    begin
      Result := Items[idx]^;
      Exit;
    end;

  if idx > 0 then
      lpt := Items[idx - 1]^
  else
      lpt := Items[Count - 1]^;
  if idx + 1 < Count then
      rpt := Items[idx + 1]^
  else
      rpt := Items[0]^;
  pt := Items[idx]^;

  // normal : left to
  dx := (pt[0] - lpt[0]);
  dy := (pt[1] - lpt[1]);
  f := 1.0 / HypotX(dx, dy);
  ln[0] := (dy * f);
  ln[1] := -(dx * f);

  // normal : right to
  dx := (rpt[0] - pt[0]);
  dy := (rpt[1] - pt[1]);
  f := 1.0 / HypotX(dx, dy);
  rn[0] := (dy * f);
  rn[1] := -(dx * f);

  // compute the expand edge
  dx := (ln[0] + rn[0]);
  dy := (ln[1] + rn[1]);
  r := (ln[0] * dx) + (ln[1] * dy);
  if r = 0 then
      r := 1;
  Cx := (dx * ExpandDist / r);
  Cy := (dy * ExpandDist / r);

  Result[0] := pt[0] + Cx;
  Result[1] := pt[1] + Cy;
end;

function TPoly.GetPoly(index: Integer): PPolyPoint;
begin
  Result := FList[index];
end;

constructor TPoly.Create;
begin
  inherited Create;
  FMaxRadius := 0;
  FList := TCoreClassList.Create;
  FPosition := PointMake(0, 0);
  FScale := 1.0;
  FAngle := 0;
  FExpandMode := emConvex;
  FUserDataObject := nil;
  FUserData := nil;
end;

destructor TPoly.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TPoly.Reset;
begin
  FPosition := PointMake(0, 0);
  FMaxRadius := 0;
  FScale := 1.0;
  FAngle := 0;
  Clear;
end;

procedure TPoly.Assign(Source: TCoreClassPersistent);
var
  i: Integer;
  p, p2: PPolyPoint;
begin
  if Source is TPoly then
    begin
      Clear;

      FScale := TPoly(Source).FScale;
      FAngle := TPoly(Source).FAngle;
      FMaxRadius := TPoly(Source).FMaxRadius;
      FPosition := TPoly(Source).FPosition;
      FExpandMode := TPoly(Source).FExpandMode;

      for i := 0 to TPoly(Source).FList.Count - 1 do
        begin
          new(p);
          p2 := TPoly(Source).Poly[i];
          p^.Owner := Self;
          p^.Angle := p2^.Angle;
          p^.Dist := p2^.Dist;
          FList.Add(p);
        end;
    end
  else if Source is TVec2List then
    begin
      RebuildPoly(TVec2List(Source));
    end;
end;

procedure TPoly.AddPoint(pt: TVec2);
begin
  AddPoint(pt[0], pt[1]);
end;

procedure TPoly.AddPoint(x, y: TGeoFloat);
var
  pt: TVec2;
begin
  pt := PointMake(x, y);
  Add(PointAngle(FPosition, pt), PointDistance(FPosition, pt));
end;

procedure TPoly.Add(AAngle, ADist: TGeoFloat);
var
  p: PPolyPoint;
begin
  if ADist > FMaxRadius then
      FMaxRadius := ADist;
  new(p);
  p^.Owner := Self;
  p^.Angle := AAngle - FAngle;
  p^.Dist := ADist / FScale;
  FList.Add(p);
end;

procedure TPoly.Insert(idx: Integer; Angle, Dist: TGeoFloat);
var
  p: PPolyPoint;
begin
  new(p);
  p^.Owner := Self;
  p^.Angle := Angle;
  p^.Dist := Dist;
  FList.Insert(idx, p);
end;

procedure TPoly.Delete(idx: Integer);
begin
  Dispose(PPolyPoint(FList[idx]));
  FList.Delete(idx);
end;

procedure TPoly.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PPolyPoint(FList[i]));
  FList.Clear;
end;

function TPoly.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TPoly.CopyPoly(pl: TPoly; AReversed: Boolean);
  procedure _Append(a, d: TGeoFloat);
  var
    p: PPolyPoint;
  begin
    if d > FMaxRadius then
        FMaxRadius := d;
    new(p);
    p^.Owner := Self;
    p^.Angle := a;
    p^.Dist := d;
    FList.Add(p);
  end;

var
  i: Integer;
begin
  Clear;
  FScale := pl.FScale;
  FAngle := pl.FAngle;
  FPosition := pl.FPosition;
  FMaxRadius := 0;
  if AReversed then
    begin
      for i := pl.Count - 1 downto 0 do
        with pl.Poly[i]^ do
            _Append(Angle, Dist);
    end
  else
    begin
      for i := 0 to pl.Count - 1 do
        with pl.Poly[i]^ do
            _Append(Angle, Dist);
    end;
end;

procedure TPoly.CopyExpandPoly(pl: TPoly; AReversed: Boolean; Dist: TGeoFloat);
var
  i: Integer;
begin
  Clear;
  FScale := pl.FScale;
  FAngle := pl.FAngle;
  FPosition := pl.FPosition;
  FMaxRadius := 0;
  if AReversed then
    begin
      for i := pl.Count - 1 downto 0 do
          AddPoint(pl.Expands[i, Dist]);
    end
  else
    for i := 0 to pl.Count - 1 do
        AddPoint(pl.Expands[i, Dist]);
end;

procedure TPoly.Reverse;
var
  NewList: TCoreClassList;
  i, c: Integer;
begin
  NewList := TCoreClassList.Create;
  c := Count - 1;
  NewList.Count := c + 1;
  for i := c downto 0 do
      NewList[c - i] := FList[i];
  DisposeObject(FList);
  FList := NewList;
end;

function TPoly.ScaleBeforeDistance: TGeoFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + PPolyPoint(FList[i])^.Dist;
end;

function TPoly.ScaleAfterDistance: TGeoFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + PPolyPoint(FList[i])^.Dist * FScale;
end;

procedure TPoly.FixedSameError;
var
  L, p: PPolyPoint;
  i: Integer;
begin
  if Count < 2 then
      Exit;

  L := PPolyPoint(FList[0]);
  p := PPolyPoint(FList[Count - 1]);
  while (Count >= 2) and (IsEqual(p^.Angle, L^.Angle)) and (IsEqual(p^.Dist, L^.Dist)) do
    begin
      Delete(Count - 1);
      p := PPolyPoint(FList[Count - 1]);
    end;

  if Count < 2 then
      Exit;

  L := PPolyPoint(FList[0]);
  i := 1;
  while i < Count do
    begin
      p := PPolyPoint(FList[i]);
      if (IsEqual(p^.Angle, L^.Angle)) and (IsEqual(p^.Dist, L^.Dist)) then
          Delete(i)
      else
        begin
          L := p;
          inc(i);
        end;
    end;
end;

procedure TPoly.ConvexHullFromPoint(AFrom: TVec2List);

const
  RightHandSide = -1;
  LeftHandSide = +1;
  CounterClockwise = +1;
  CollinearOrientation = 0;

type
  T2DHullPoint = record
    x: TGeoFloat;
    y: TGeoFloat;
    Ang: TGeoFloat;
  end;

  TCompareResult = (eGreaterThan, eLessThan, eEqual);

var
  Point: array of T2DHullPoint;
  Stack: array of T2DHullPoint;
  StackHeadPosition: Integer;
  Anchor: T2DHullPoint;

  function CartesianAngle(const x, y: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  const
    _180DivPI = 57.295779513082320876798154814105000;
  begin
    if (x > Zero) and (y > Zero) then
        Result := (ArcTan(y / x) * _180DivPI)
    else if (x < Zero) and (y > Zero) then
        Result := (ArcTan(-x / y) * _180DivPI) + 90.0
    else if (x < Zero) and (y < Zero) then
        Result := (ArcTan(y / x) * _180DivPI) + 180.0
    else if (x > Zero) and (y < Zero) then
        Result := (ArcTan(-x / y) * _180DivPI) + 270.0
    else if (x = Zero) and (y > Zero) then
        Result := 90.0
    else if (x < Zero) and (y = Zero) then
        Result := 180.0
    else if (x = Zero) and (y < Zero) then
        Result := 270.0
    else
        Result := Zero;
  end;

  procedure Swap(i, j: Integer; var Point: array of T2DHullPoint);
  var
    Temp: T2DHullPoint;
  begin
    Temp := Point[i];
    Point[i] := Point[j];
    Point[j] := Temp;
  end;

  function CompareAngles(const p1, p2: T2DHullPoint): TCompareResult;
    function hEqual(const p1, p2: T2DHullPoint): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    begin
      Result := IsEqual(p1.x, p2.x) and IsEqual(p1.y, p2.y);
    end;

  begin
    if p1.Ang < p2.Ang then
        Result := eLessThan
    else if p1.Ang > p2.Ang then
        Result := eGreaterThan
    else if hEqual(p1, p2) then
        Result := eEqual
    else if Distance(Anchor.x, Anchor.y, p1.x, p1.y) < Distance(Anchor.x, Anchor.y, p2.x, p2.y) then
        Result := eLessThan
    else
        Result := eGreaterThan;
  end;

  procedure RQSort(Left, Right: Integer; var Point: array of T2DHullPoint);
  var
    i: Integer;
    j: Integer;
    Middle: Integer;
    Pivot: T2DHullPoint;
  begin
    repeat
      i := Left;
      j := Right;
      Middle := (Left + Right) div 2;
      (* Median of 3 Pivot Selection *)
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      if CompareAngles(Point[Right], Point[Middle]) = eLessThan then
          Swap(Right, Middle, Point);
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      Pivot := Point[Right];
      repeat
        while CompareAngles(Point[i], Pivot) = eLessThan do
            inc(i);
        while CompareAngles(Point[j], Pivot) = eGreaterThan do
            dec(j);
        if i <= j then
          begin
            Swap(i, j, Point);
            inc(i);
            dec(j);
          end;
      until i > j;
      if Left < j then
          RQSort(Left, j, Point);
      Left := i;
    until i >= Right;
  end;

  procedure Push(Pnt: T2DHullPoint);
  begin
    inc(StackHeadPosition);
    Stack[StackHeadPosition] := Pnt;
  end;

  function Pop: Boolean;
  begin
    Result := False;
    if StackHeadPosition >= 0 then
      begin
        Result := True;
        dec(StackHeadPosition);
      end;
  end;

  function Head: T2DHullPoint;
  begin
    Assert((StackHeadPosition >= 0) and (StackHeadPosition < length(Stack)), 'Invalid stack-head position.');
    Result := Stack[StackHeadPosition];
  end;

  function PreHead: T2DHullPoint;
  begin
    Assert(((StackHeadPosition - 1) >= 0) and ((StackHeadPosition - 1) < length(Stack)), 'Invalid pre stack-head position.');
    Result := Stack[StackHeadPosition - 1];
  end;

  function PreHeadExist: Boolean;
  begin
    Result := (StackHeadPosition > 0);
  end;

  function Orientation(p1, p2, p3: T2DHullPoint): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Orientation2(const x1, y1, x2, y2, Px, Py: TGeoFloat): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    var
      Orin: TGeoFloat;
    begin
      (* Determinant of the 3 points *)
      Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);
      if Orin > Zero then
          Result := LeftHandSide (* Orientaion is to the left-hand side *)
      else if Orin < Zero then
          Result := RightHandSide (* Orientaion is to the right-hand side *)
      else
          Result := CollinearOrientation; (* Orientaion is neutral aka collinear *)
    end;

  begin
    Result := Orientation2(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
  end;

  procedure GrahamScan;
  var
    i: Integer;
    Orin: Integer;
  begin
    Push(Point[0]);
    Push(Point[1]);
    i := 2;
    while i < length(Point) do
      begin
        if PreHeadExist then
          begin
            Orin := Orientation(PreHead, Head, Point[i]);
            if Orin = CounterClockwise then
              begin
                Push(Point[i]);
                inc(i);
              end
            else
                Pop;
          end
        else
          begin
            Push(Point[i]);
            inc(i);
          end;
      end;
  end;

  function CalcCentroid: TVec2;
  var
    i: Integer;
    j: Integer;
    asum: TGeoFloat;
    term: TGeoFloat;
  begin
    Result := NULLPoint;

    asum := Zero;
    j := StackHeadPosition;

    for i := 0 to StackHeadPosition do
      begin
        term := ((Stack[j].x * Stack[i].y) - (Stack[j].y * Stack[i].x));
        asum := asum + term;
        Result[0] := Result[0] + (Stack[j].x + Stack[i].x) * term;
        Result[1] := Result[1] + (Stack[j].y + Stack[i].y) * term;
        j := i;
      end;

    if NotEqual(asum, Zero) then
      begin
        Result[0] := Result[0] / (3.0 * asum);
        Result[1] := Result[1] / (3.0 * asum);
      end;
  end;

var
  i: Integer;
  j: Integer;
  pt: TVec2;
begin
  if AFrom.Count <= 3 then
      Exit;

  StackHeadPosition := -1;

  try
    SetLength(Point, AFrom.Count);
    SetLength(Stack, AFrom.Count);
    j := 0;
    for i := 0 to AFrom.Count - 1 do
      begin
        pt := AFrom[i]^;
        Point[i].x := pt[0];
        Point[i].y := pt[1];
        Point[i].Ang := 0.0;
        if Point[i].y < Point[j].y then
            j := i
        else if Point[i].y = Point[j].y then
          if Point[i].x < Point[j].x then
              j := i;
      end;

    Swap(0, j, Point);
    Point[0].Ang := 0;
    Anchor := Point[0];
    (* Calculate angle of the vertex ([ith point]-[anchorpoint]-[most left point]) *)
    for i := 1 to length(Point) - 1 do
        Point[i].Ang := CartesianAngle(Point[i].x - Anchor.x, Point[i].y - Anchor.y);
    (* Sort points in ascending order according to their angles *)
    RQSort(1, length(Point) - 1, Point);
    GrahamScan;

    { * make Circle * }
    FPosition := CalcCentroid;
    FMaxRadius := 0;

    { * rebuild opt * }
    FScale := 1.0;
    FAngle := 0;

    { * clear * }
    Clear;

    (* output list to self *)
    for i := 0 to StackHeadPosition do
        AddPoint(Stack[i].x, Stack[i].y);
  finally
    (* Final clean-up *)
    Finalize(Stack);
    Finalize(Point);
  end;
  RebuildPoly;
end;

procedure TPoly.RebuildPoly(pl: TVec2List);
var
  i: Integer;
  Ply: TPoly;
begin
  { * rebuild opt * }
  FPosition := pl.Centroid;
  FMaxRadius := 0;
  FScale := 1.0;
  FAngle := 0;

  { * rebuild poly * }
  Clear;
  for i := 0 to pl.Count - 1 do
      AddPoint(pl[i]^);

  Ply := TPoly.Create;
  with Ply do
    begin
      CopyExpandPoly(Self, False, 1);
      if (Self.FExpandMode = emConvex) and (Self.ScaleBeforeDistance > ScaleBeforeDistance) then
          Self.Reverse
      else if (Self.FExpandMode = emConcave) and (Self.ScaleBeforeDistance < ScaleBeforeDistance) then
          Self.Reverse;
    end;
  DisposeObject(Ply);
end;

procedure TPoly.RebuildPoly;
var
  pl: TVec2List;
  i: Integer;
begin
  pl := TVec2List.Create;
  for i := 0 to Count - 1 do
      pl.Add(GetPoint(i));
  RebuildPoly(pl);
  DisposeObject(pl);
end;

procedure TPoly.RebuildPoly(AScale, AAngle: TGeoFloat; AExpandMode: TExpandMode; APosition: TVec2);
var
  pl: TVec2List;
  i: Integer;
begin
  pl := TVec2List.Create;
  for i := 0 to Count - 1 do
      pl.Add(GetPoint(i));
  Scale := AScale;
  Angle := AAngle;
  ExpandMode := AExpandMode;
  Position := APosition;
  RebuildPoly(pl);
  DisposeObject(pl);
end;

function TPoly.BoundRect: TRectV2;
var
  p: TVec2;
  MaxX: TGeoFloat;
  MaxY: TGeoFloat;
  MinX: TGeoFloat;
  MinY: TGeoFloat;
  i: Integer;
begin
  Result := MakeRectV2(Zero, Zero, Zero, Zero);
  if Count < 2 then
      Exit;
  p := Points[0];
  MinX := p[0];
  MaxX := p[0];
  MinY := p[1];
  MaxY := p[1];

  for i := 1 to Count - 1 do
    begin
      p := Points[i];
      if p[0] < MinX then
          MinX := p[0]
      else if p[0] > MaxX then
          MaxX := p[0];
      if p[1] < MinY then
          MinY := p[1]
      else if p[1] > MaxY then
          MaxY := p[1];
    end;
  Result := MakeRectV2(MinX, MinY, MaxX, MaxY);
end;

function TPoly.Centroid: TVec2;
var
  i: Integer;
  asum: TGeoFloat;
  term: TGeoFloat;

  pt1, pt2: TVec2;
begin
  Result := NULLPoint;

  if Count < 3 then
      Exit;

  asum := Zero;
  pt2 := Points[Count - 1];

  for i := 0 to Count - 1 do
    begin
      pt1 := Points[i];

      term := ((pt2[0] * pt1[1]) - (pt2[1] * pt1[0]));
      asum := asum + term;
      Result[0] := Result[0] + (pt2[0] + pt1[0]) * term;
      Result[1] := Result[1] + (pt2[1] + pt1[1]) * term;
      pt2 := pt1;
    end;

  if NotEqual(asum, Zero) then
    begin
      Result[0] := Result[0] / (3.0 * asum);
      Result[1] := Result[1] / (3.0 * asum);
    end;
end;

function TPoly.PointInHere(pt: TVec2): Boolean;
var
  i: Integer;
  pi, pj: TVec2;
begin
  Result := False;
  if Count < 3 then
      Exit;
  if not PointInCircle(pt, FPosition, FMaxRadius * FScale) then
      Exit;
  pj := GetPoint(Count - 1);
  for i := 0 to Count - 1 do
    begin
      pi := GetPoint(i);
      if ((pi[1] <= pt[1]) and (pt[1] < pj[1])) or  // an upward crossing
        ((pj[1] <= pt[1]) and (pt[1] < pi[1])) then // a downward crossing
        begin
          (* compute the edge-ray intersect @ the x-coordinate *)
          if (pt[0] - pi[0] < ((pj[0] - pi[0]) * (pt[1] - pi[1]) / (pj[1] - pi[1]))) then
              Result := not Result;
        end;
      pj := pi;
    end;
end;

function TPoly.LineNearIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: Integer; out IntersectPt: TVec2): Boolean;
var
  i: Integer;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale, lb, le) then
      Exit;

  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  d := d2;
                  idx1 := i - 1;
                  idx2 := i;
                  Result := True;
                end;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode then
        begin
          pt2 := Points[0];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  // d := d2;
                  idx1 := FList.Count - 1;
                  idx2 := 0;
                  Result := True;
                end;
            end;
        end;
    end;
end;

function TPoly.LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean;
var
  i: Integer;
  pt1, pt2: TVec2;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale, lb, le) then
      Exit;

  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];
          if SimpleIntersect(lb, le, pt1, pt2) then
            begin
              Result := True;
              Exit;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode then
        begin
          pt2 := Points[0];
          if SimpleIntersect(lb, le, pt1, pt2) then
              Result := True;
        end;
    end;
end;

function TPoly.GetMinimumFromPointToPoly(const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: Integer): TVec2;
var
  i: Integer;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];

          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);

          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
              lb := i - 1;
              le := i;
            end;

          pt1 := pt2;
        end;
      if ClosedPolyMode then
        begin
          pt2 := Points[0];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              lb := FList.Count - 1;
              le := 0;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0];
          lb := 0;
          le := 0;
        end
      else
        begin
          Result := NULLPoint;
          lb := -1;
          le := -1;
        end;
    end;
end;

function TPoly.PointInHere(AExpandDistance: TGeoFloat; pt: TVec2): Boolean;
var
  i: Integer;
  pi, pj: TVec2;
begin
  Result := False;
  if Count < 3 then
      Exit;
  if not PointInCircle(pt, FPosition, FMaxRadius * FScale + AExpandDistance) then
      Exit;
  pj := Expands[Count - 1, AExpandDistance];
  for i := 0 to Count - 1 do
    begin
      pi := Expands[i, AExpandDistance];
      if ((pi[1] <= pt[1]) and (pt[1] < pj[1])) or  // an upward crossing
        ((pj[1] <= pt[1]) and (pt[1] < pi[1])) then // a downward crossing
        begin
          (* compute the edge-ray intersect @ the x-coordinate *)
          if ((pt[0] - pi[0]) < ((pj[0] - pi[0]) * (pt[1] - pi[1]) / (pj[1] - pi[1]))) then
              Result := not Result;
        end;
      pj := pi;
    end;
end;

function TPoly.LineNearIntersect(AExpandDistance: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: Integer; out IntersectPt: TVec2): Boolean;
var
  i: Integer;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale + AExpandDistance, lb, le) then
      Exit;

  if FList.Count > 1 then
    begin
      pt1 := Expands[0, AExpandDistance];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, AExpandDistance];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  d := d2;
                  idx1 := i - 1;
                  idx2 := i;
                  Result := True;
                end;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode then
        begin
          pt2 := Expands[0, AExpandDistance];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  // d := d2;
                  idx1 := FList.Count - 1;
                  idx2 := 0;
                  Result := True;
                end;
            end;
        end;
    end;
end;

function TPoly.LineIntersect(AExpandDistance: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean;
var
  i: Integer;
  pt1, pt2: TVec2;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale + AExpandDistance, lb, le) then
      Exit;

  if FList.Count > 1 then
    begin
      pt1 := Expands[0, AExpandDistance];
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, AExpandDistance];
          if SimpleIntersect(lb, le, pt1, pt2) then
            begin
              Result := True;
              Exit;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode then
        begin
          pt2 := Expands[0, AExpandDistance];
          if SimpleIntersect(lb, le, pt1, pt2) then
              Result := True;
        end;
    end;
end;

function TPoly.GetMinimumFromPointToPoly(AExpandDistance: TGeoFloat; const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: Integer): TVec2;
var
  i: Integer;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Expands[0, AExpandDistance];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, AExpandDistance];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
              lb := i - 1;
              le := i;
            end;

          pt1 := pt2;
        end;
      if ClosedPolyMode then
        begin
          pt2 := Expands[0, AExpandDistance];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              lb := FList.Count - 1;
              le := 0;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0];
          lb := 0;
          le := 0;
        end
      else
        begin
          Result := NULLPoint;
          lb := -1;
          le := -1;
        end;
    end;
end;

function TPoly.Collision2Circle(cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean): Boolean;
var
  i: Integer;
  curpt, destpt: TVec2;
begin
  if (Detect_Circle2Circle(FPosition, cp, FMaxRadius * FScale, r)) and (Count > 0) then
    begin
      Result := True;
      curpt := Points[0];
      for i := 1 to Count - 1 do
        begin
          destpt := Points[i];
          if Detect_Circle2Line(cp, r, curpt, destpt) then
              Exit;
          curpt := destpt;
        end;
      if ClosedPolyMode then
        if Detect_Circle2Line(cp, r, curpt, Points[0]) then
            Exit;
    end;
  Result := False;
end;

function TPoly.Collision2Circle(cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: T2DLineList): Boolean;
var
  i: Integer;
  curpt, destpt: TVec2;
begin
  Result := False;
  if (Detect_Circle2Circle(FPosition, cp, FMaxRadius * FScale, r)) and (Count > 0) then
    begin
      curpt := Points[0];
      for i := 1 to Count - 1 do
        begin
          destpt := Points[i];
          if Detect_Circle2Line(cp, r, curpt, destpt) then
            begin
              OutputLine.Add(curpt, destpt, i - 1, i, Self);
              Result := True;
            end;
          curpt := destpt;
        end;
      if ClosedPolyMode then
        if Detect_Circle2Line(cp, r, curpt, Points[0]) then
          begin
            OutputLine.Add(curpt, Points[0], Count - 1, 0, Self);
            Result := True;
          end;
    end;
end;

function TPoly.Collision2Circle(AExpandDistance: TGeoFloat; cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: T2DLineList): Boolean;
var
  i: Integer;
  curpt, destpt: TVec2;
begin
  Result := False;
  if (Detect_Circle2Circle(FPosition, cp, FMaxRadius * FScale + AExpandDistance, r)) and (Count > 0) then
    begin
      curpt := Expands[0, AExpandDistance];
      for i := 1 to Count - 1 do
        begin
          destpt := Expands[i, AExpandDistance];
          if Detect_Circle2Line(cp, r, curpt, destpt) then
            begin
              OutputLine.Add(curpt, destpt, i - 1, i, Self);
              Result := True;
            end;
          curpt := destpt;
        end;
      if ClosedPolyMode then
        if Detect_Circle2Line(cp, r, curpt, Expands[0, AExpandDistance]) then
          begin
            OutputLine.Add(curpt, Expands[0, AExpandDistance], Count - 1, 0, Self);
            Result := True;
          end;
    end;
end;

function TPoly.PolyIntersect(APoly: TPoly): Boolean;
var
  i: Integer;
begin
  Result := Detect_Circle2Circle(Position, APoly.Position, MaxRadius * FScale, APoly.MaxRadius * APoly.Scale);
  if not Result then
      Exit;

  for i := 0 to Count - 1 do
    if APoly.PointInHere(Points[i]) then
        Exit;

  for i := 0 to APoly.Count - 1 do
    if PointInHere(APoly.Points[i]) then
        Exit;

  Result := False;
end;

function TPoly.LerpToEndge(pt: TVec2; AProjDistance, AExpandDistance: TGeoFloat; FromIdx, toidx: Integer): TVec2;
  function NextIndexStep(CurIdx: Integer; curDir: ShortInt): Integer;
  begin
    if curDir < 0 then
      begin
        if CurIdx = 0 then
            Result := Count - 1
        else if CurIdx > 0 then
            Result := CurIdx - 1
        else
            Result := Count + CurIdx - 1;
      end
    else
      begin
        if CurIdx = Count - 1 then
            Result := 0
        else if CurIdx < Count - 1 then
            Result := CurIdx + 1
        else
            Result := CurIdx - Count;
      end;
    if (Result < 0) or (Result >= Count) then
        Result := -1;
  end;

var
  idxDir: ShortInt;
  ToPt: TVec2;
  d: TGeoFloat;
begin
  Result := pt;
  if Count <= 1 then
      Exit;

  if (FromIdx = Count - 1) and (toidx = 0) then
      idxDir := 1
  else if (FromIdx = 0) and (toidx = Count - 1) then
      idxDir := -1
  else if toidx < FromIdx then
      idxDir := -1
  else
      idxDir := 1;

  while True do
    begin
      ToPt := Expands[toidx, AExpandDistance];
      d := PointDistance(pt, ToPt);

      if AProjDistance < d then
        begin
          Result := PointLerpTo(pt, ToPt, AProjDistance);
          Exit;
        end;

      if d > 0 then
        begin
          pt := PointLerpTo(pt, ToPt, d);
          AProjDistance := AProjDistance - d;
        end;
      toidx := NextIndexStep(toidx, idxDir);
    end;
end;

function TPoly.GetPoint(idx: Integer): TVec2;
var
  p: PPolyPoint;
begin
  p := GetPoly(idx);
  Result := PointRotation(FPosition, p^.Dist * FScale, p^.Angle + FAngle);
end;

procedure TPoly.SetPoint(idx: Integer; Value: TVec2);
var
  p: PPolyPoint;
begin
  p := GetPoly(idx);
  p^.Angle := PointAngle(FPosition, Value) - FAngle;
  p^.Dist := PointDistance(FPosition, Value);
  if p^.Dist > FMaxRadius then
      FMaxRadius := p^.Dist;
  p^.Dist := p^.Dist / FScale;
end;

function TPoly.GetExpands(idx: Integer; ExpandDist: TGeoFloat): TVec2;
var
  lpt, pt, rpt: TVec2;
  ln, rn: TVec2;
  dx, dy, f, r: TGeoFloat;
  Cx, Cy: TGeoFloat;
begin
  if (ExpandDist = 0) or (Count < 2) then
    begin
      Result := Points[idx];
      Exit;
    end;

  if idx > 0 then
      lpt := Points[idx - 1]
  else
      lpt := Points[Count - 1];
  if idx + 1 < Count then
      rpt := Points[idx + 1]
  else
      rpt := Points[0];
  pt := Points[idx];

  // normal : left to
  dx := (pt[0] - lpt[0]);
  dy := (pt[1] - lpt[1]);
  f := 1.0 / HypotX(dx, dy);
  ln[0] := (dy * f);
  ln[1] := -(dx * f);

  // normal : right to
  dx := (rpt[0] - pt[0]);
  dy := (rpt[1] - pt[1]);
  f := 1.0 / HypotX(dx, dy);
  rn[0] := (dy * f);
  rn[1] := -(dx * f);

  // compute the expand edge
  dx := (ln[0] + rn[0]);
  dy := (ln[1] + rn[1]);
  r := (ln[0] * dx) + (ln[1] * dy);
  if r = 0 then
      r := 1;
  Cx := (dx * ExpandDist / r);
  Cy := (dy * ExpandDist / r);

  if FExpandMode = emConcave then
    begin
      Result[0] := pt[0] - Cx;
      Result[1] := pt[1] - Cy;
    end
  else
    begin
      Result[0] := pt[0] + Cx;
      Result[1] := pt[1] + Cy;
    end;
end;

procedure TPoly.SaveToStream(stream: TMemoryStream64);
var
  i: Integer;
  p: PPolyPoint;
begin
  stream.WriteSingle(FScale);
  stream.WriteSingle(FAngle);
  stream.WriteSingle(FPosition[0]);
  stream.WriteSingle(FPosition[1]);
  stream.WriteInt32(Count);
  for i := 0 to Count - 1 do
    begin
      p := GetPoly(i);
      stream.WriteSingle(p^.Angle);
      stream.WriteSingle(p^.Dist);
    end;
end;

procedure TPoly.LoadFromStream(stream: TMemoryStream64);
var
  c: Integer;
  i: Integer;
  p: PPolyPoint;
begin
  Clear;
  FScale := stream.ReadSingle;
  FAngle := stream.ReadSingle;
  FPosition[0] := stream.ReadSingle;
  FPosition[1] := stream.ReadSingle;
  FMaxRadius := 0;
  c := stream.ReadInt32;
  for i := 0 to c - 1 do
    begin
      new(p);
      p^.Owner := Self;
      p^.Angle := stream.ReadSingle;
      p^.Dist := stream.ReadSingle;
      FList.Add(p);

      if p^.Dist > FMaxRadius then
          FMaxRadius := p^.Dist;
    end;
end;

function T2DLineList.GetItems(index: Integer): P2DLine;
begin
  Result := FList[index];
end;

constructor T2DLineList.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
  FUserData := nil;
  FUserObject := nil;
end;

destructor T2DLineList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure T2DLineList.Assign(Source: TCoreClassPersistent);
var
  i: Integer;
begin
  if Source is T2DLineList then
    begin
      Clear;
      for i := 0 to T2DLineList(Source).Count - 1 do
          Add(T2DLineList(Source)[i]^);
    end;
end;

function T2DLineList.Add(v: T2DLine): Integer;
var
  p: P2DLine;
begin
  new(p);
  p^ := v;
  Result := FList.Add(p);
  p^.index := Result;
end;

function T2DLineList.Add(lb, le: TVec2): Integer;
var
  p: P2DLine;
begin
  new(p);
  p^.buff[0] := lb;
  p^.buff[1] := le;
  p^.PolyIndex[0] := -1;
  p^.PolyIndex[1] := -1;
  p^.Poly := nil;
  Result := FList.Add(p);
  p^.index := Result;
end;

function T2DLineList.Add(lb, le: TVec2; idx1, idx2: Integer; Poly: TPoly): Integer;
var
  p: P2DLine;
begin
  new(p);
  p^.buff[0] := lb;
  p^.buff[1] := le;
  p^.PolyIndex[0] := idx1;
  p^.PolyIndex[1] := idx2;
  p^.Poly := Poly;
  Result := FList.Add(p);
  p^.index := Result;
end;

function T2DLineList.Count: Integer;
begin
  Result := FList.Count;
end;

procedure T2DLineList.Delete(index: Integer);
var
  p: P2DLine;
  i: Integer;
begin
  p := FList[index];
  Dispose(p);
  FList.Delete(index);
  for i := index to Count - 1 do
      Items[i]^.index := i;
end;

procedure T2DLineList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(P2DLine(FList[i]));
  FList.Clear;
end;

function T2DLineList.NearLine(const ExpandDist: TGeoFloat; const pt: TVec2): P2DLine;
var
  d, d2: TGeoFloat;
  L: P2DLine;
  i: Integer;
begin
  Result := nil;
  if Count = 1 then
    begin
      Result := Items[0];
    end
  else if Count > 1 then
    begin
      L := Items[0];
      if ExpandDist = 0 then
          d := L^.MinimumDistance(pt)
      else
          d := L^.MinimumDistance(ExpandDist, pt);
      Result := L;

      for i := 1 to Count - 1 do
        begin
          L := Items[i];

          if ExpandDist = 0 then
              d2 := L^.MinimumDistance(pt)
          else
              d2 := L^.MinimumDistance(ExpandDist, pt);

          if d2 < d then
            begin
              Result := L;
              d := d2;
            end;
        end;
    end;
end;

function T2DLineList.FarLine(const ExpandDist: TGeoFloat; const pt: TVec2): P2DLine;
var
  d, d2: TGeoFloat;
  L: P2DLine;
  i: Integer;
begin
  Result := nil;
  if Count > 0 then
    begin
      L := Items[0];
      if ExpandDist = 0 then
          d := L^.MinimumDistance(pt)
      else
          d := L^.MinimumDistance(ExpandDist, pt);
      Result := L;

      for i := 1 to Count - 1 do
        begin
          L := Items[i];

          if ExpandDist = 0 then
              d2 := L^.MinimumDistance(pt)
          else
              d2 := L^.MinimumDistance(ExpandDist, pt);

          if d2 > d then
            begin
              Result := L;
              d := d2;
            end;
        end;
    end;
end;

procedure T2DLineList.SortOfNear(const pt: TVec2);

  function ListSortCompare(Item1, Item2: Pointer): Integer;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := P2DLine(Item1)^.MinimumDistance(pt);
    d2 := P2DLine(Item2)^.MinimumDistance(pt);
    Result := CompareValue(d1, d2);
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i: Integer;
begin
  if Count > 1 then
      QuickSortList(FList.ListData^, 0, Count - 1);
  for i := 0 to Count - 1 do
      Items[i]^.index := i;
end;

procedure T2DLineList.SortOfFar(const pt: TVec2);

  function ListSortCompare(Item1, Item2: Pointer): Integer;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := P2DLine(Item1)^.MinimumDistance(pt);
    d2 := P2DLine(Item2)^.MinimumDistance(pt);
    Result := CompareValue(d2, d1);
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer);
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i: Integer;
begin
  if Count > 1 then
      QuickSortList(FList.ListData^, 0, Count - 1);
  for i := 0 to Count - 1 do
      Items[i]^.index := i;
end;

function T2DCircleList.GetItems(index: Integer): P2DCircle;
begin
  Result := FList[index];
end;

constructor T2DCircleList.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor T2DCircleList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure T2DCircleList.Assign(Source: TCoreClassPersistent);
var
  i: Integer;
begin
  if Source is T2DCircleList then
    begin
      Clear;
      for i := 0 to T2DCircleList(Source).Count - 1 do
          Add(T2DCircleList(Source)[i]^);
    end;
end;

function T2DCircleList.Add(const v: T2DCircle): Integer;
var
  p: P2DCircle;
begin
  new(p);
  p^ := v;
  Result := FList.Add(p);
end;

function T2DCircleList.Add(const Position: TVec2; const radius: TGeoFloat; const UserData: TCoreClassObject): Integer;
var
  p: P2DCircle;
begin
  new(p);
  p^.Position := Position;
  p^.radius := radius;
  p^.UserData := UserData;
  Result := FList.Add(p);
end;

function T2DCircleList.Count: Integer;
begin
  Result := FList.Count;
end;

procedure T2DCircleList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(P2DCircle(FList[i]));
  FList.Clear;
end;

procedure T2DCircleList.Delete(index: Integer);
var
  p: P2DCircle;
begin
  p := FList[index];
  Dispose(p);
  FList.Delete(index);
end;

procedure T2DCircleList.SortOfMinRadius;

  function ListSortCompare(Item1, Item2: Pointer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    d1, d2: TGeoFloat;
  begin
    d1 := P2DCircle(Item1)^.radius;
    d2 := P2DCircle(Item2)^.radius;
    Result := CompareValue(d1, d2);
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

begin
  if Count > 1 then
      QuickSortList(FList.ListData^, 0, Count - 1);
end;

procedure T2DCircleList.SortOfMaxRadius;

  function ListSortCompare(Item1, Item2: Pointer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    d1, d2: TGeoFloat;
  begin
    d1 := P2DCircle(Item1)^.radius;
    d2 := P2DCircle(Item2)^.radius;
    Result := CompareValue(d2, d1);
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

begin
  if Count > 1 then
      QuickSortList(FList.ListData^, 0, Count - 1);
end;

function TRectV2List.GetItems(index: Integer): PRectV2;
begin
  Result := FList[index];
end;

constructor TRectV2List.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TRectV2List.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TRectV2List.Assign(Source: TCoreClassPersistent);
var
  i: Integer;
begin
  if Source is TRectV2List then
    begin
      Clear;
      for i := 0 to TRectV2List(Source).Count - 1 do
          Add(TRectV2List(Source)[i]^);
    end;
end;

function TRectV2List.Add(const v: TRectV2): Integer;
var
  p: PRectV2;
begin
  new(p);
  p^ := v;
  Result := FList.Add(p);
end;

function TRectV2List.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TRectV2List.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(PRectV2(FList[i]));
  FList.Clear;
end;

procedure TRectV2List.Delete(index: Integer);
var
  p: PRectV2;
begin
  p := FList[index];
  Dispose(p);
  FList.Delete(index);
end;

function TV2Rect4.IsZero: Boolean;
begin
  Result :=
    Geometry2DUnit.IsZero(LeftTop) and
    Geometry2DUnit.IsZero(RightTop) and
    Geometry2DUnit.IsZero(RightBottom) and
    Geometry2DUnit.IsZero(LeftBottom);
end;

function TV2Rect4.Rotation(Angle: TGeoFloat): TV2Rect4;
var
  axis: TVec2;
begin
  axis := Centroid;
  Result.LeftTop := PointRotation(axis, LeftTop, PointAngle(axis, LeftTop) + Angle);
  Result.RightTop := PointRotation(axis, RightTop, PointAngle(axis, RightTop) + Angle);
  Result.RightBottom := PointRotation(axis, RightBottom, PointAngle(axis, RightBottom) + Angle);
  Result.LeftBottom := PointRotation(axis, LeftBottom, PointAngle(axis, LeftBottom) + Angle);
end;

function TV2Rect4.Rotation(axis: TVec2; Angle: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := PointRotation(axis, LeftTop, PointAngle(axis, LeftTop) + Angle);
  Result.RightTop := PointRotation(axis, RightTop, PointAngle(axis, RightTop) + Angle);
  Result.RightBottom := PointRotation(axis, RightBottom, PointAngle(axis, RightBottom) + Angle);
  Result.LeftBottom := PointRotation(axis, LeftBottom, PointAngle(axis, LeftBottom) + Angle);
end;

function TV2Rect4.ScaleToRect(Area: TRectV2; endge: TGeoFloat): TV2Rect4;
var
  a: TRectV2;
begin
  a := BoundRect;
  Result := Mul((RectWidth(Area) - endge) / RectWidth(a), (RectHeight(Area) - endge) / RectHeight(a));
  Result := Result.Transform(Vec2Sub(RectCentre(Area), Result.Centroid));
end;

function TV2Rect4.Add(v: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Add(LeftTop, v);
  Result.RightTop := Vec2Add(RightTop, v);
  Result.RightBottom := Vec2Add(RightBottom, v);
  Result.LeftBottom := Vec2Add(LeftBottom, v);
end;

function TV2Rect4.Sub(v: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Sub(LeftTop, v);
  Result.RightTop := Vec2Sub(RightTop, v);
  Result.RightBottom := Vec2Sub(RightBottom, v);
  Result.LeftBottom := Vec2Sub(LeftBottom, v);
end;

function TV2Rect4.Mul(v: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Mul(LeftTop, v);
  Result.RightTop := Vec2Mul(RightTop, v);
  Result.RightBottom := Vec2Mul(RightBottom, v);
  Result.LeftBottom := Vec2Mul(LeftBottom, v);
end;

function TV2Rect4.Mul(v: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Mul(LeftTop, v);
  Result.RightTop := Vec2Mul(RightTop, v);
  Result.RightBottom := Vec2Mul(RightBottom, v);
  Result.LeftBottom := Vec2Mul(LeftBottom, v);
end;

function TV2Rect4.Mul(x, y: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Mul(LeftTop, x, y);
  Result.RightTop := Vec2Mul(RightTop, x, y);
  Result.RightBottom := Vec2Mul(RightBottom, x, y);
  Result.LeftBottom := Vec2Mul(LeftBottom, x, y);
end;

function TV2Rect4.FDiv(v: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Div(LeftTop, v);
  Result.RightTop := Vec2Div(RightTop, v);
  Result.RightBottom := Vec2Div(RightBottom, v);
  Result.LeftBottom := Vec2Div(LeftBottom, v);
end;

function TV2Rect4.MoveTo(Position: TVec2): TV2Rect4;
begin
  Result := Init(Position, PointDistance(LeftTop, RightTop), PointDistance(LeftBottom, RightBottom), 0);
end;

function TV2Rect4.BoundRect: TRectV2;
begin
  Result := Geometry2DUnit.BoundRect(LeftTop, RightTop, RightBottom, LeftBottom);
end;

function TV2Rect4.BoundRectf: TRectf;
begin
  Result := MakeRectf(BoundRect);
end;

function TV2Rect4.Centroid: TVec2;
begin
  Result := Geometry2DUnit.BuffCentroid(LeftTop, RightTop, RightBottom, LeftBottom);
end;

function TV2Rect4.Transform(v2: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Add(LeftTop, v2);
  Result.RightTop := Vec2Add(RightTop, v2);
  Result.RightBottom := Vec2Add(RightBottom, v2);
  Result.LeftBottom := Vec2Add(LeftBottom, v2);
end;

function TV2Rect4.Transform(x, y: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Add(LeftTop, x, y);
  Result.RightTop := Vec2Add(RightTop, x, y);
  Result.RightBottom := Vec2Add(RightBottom, x, y);
  Result.LeftBottom := Vec2Add(LeftBottom, x, y);
end;

function TV2Rect4.Expands(Dist: TGeoFloat): TV2Rect4;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  vl.Add(LeftTop);
  vl.Add(RightTop);
  vl.Add(RightBottom);
  vl.Add(LeftBottom);
  Result.LeftTop := vl.Expands[0, Dist];
  Result.RightTop := vl.Expands[1, Dist];
  Result.RightBottom := vl.Expands[2, Dist];
  Result.LeftBottom := vl.Expands[3, Dist];
  DisposeObject(vl);
end;

class function TV2Rect4.Init(r: TRectV2; Ang: TGeoFloat): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := PointMake(r[0][0], r[0][1]);
      RightTop := PointMake(r[1][0], r[0][1]);
      RightBottom := PointMake(r[1][0], r[1][1]);
      LeftBottom := PointMake(r[0][0], r[1][1]);
    end;
  if Ang <> 0 then
      Result := Result.Rotation(Ang);
end;

class function TV2Rect4.Init(r: TRectf; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(r), Ang);
end;

class function TV2Rect4.Init(r: TRect; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(r), Ang);
end;

class function TV2Rect4.Init(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4;
var
  r: TRectV2;
begin
  r[0][0] := CenPos[0] - width * 0.5;
  r[0][1] := CenPos[1] - height * 0.5;
  r[1][0] := CenPos[0] + width * 0.5;
  r[1][1] := CenPos[1] + height * 0.5;
  Result := Init(r, Ang);
end;

class function TV2Rect4.Init(width, height, Ang: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(0, 0, width, height), Ang);
end;

class function TV2Rect4.InitZero: TV2Rect4;
begin
  with Result do
    begin
      LeftTop := NULLPoint;
      RightTop := NULLPoint;
      RightBottom := NULLPoint;
      LeftBottom := NULLPoint;
    end;
end;

function TRectPacking.Pack(width, height: TGeoFloat; var x, y: TGeoFloat): Boolean;
var
  i: Integer;
  p: PRectPackData;
  r, b: TGeoFloat;
begin
  MaxWidth := Max(MaxWidth, width);
  MaxHeight := Max(MaxHeight, height);

  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if (width <= RectWidth(p^.Rect)) and (height <= RectHeight(p^.Rect)) then
        begin
          FList.Delete(i);
          x := p^.Rect[0][0];
          y := p^.Rect[0][1];
          r := x + width;
          b := y + height;
          MaxWidth := Max(MaxWidth, Max(width, r));
          MaxHeight := Max(MaxHeight, Max(height, b));
          Add(x, b, width, p^.Rect[1][1] - b);
          Add(r, y, p^.Rect[1][0] - r, height);
          Add(r, b, p^.Rect[1][0] - r, p^.Rect[1][1] - b);
          Result := True;
          Dispose(p);
          Exit;
        end;
      inc(i);
    end;
  x := 0;
  y := 0;
  Result := False;
end;

function TRectPacking.GetItems(const index: Integer): PRectPackData;
begin
  Result := PRectPackData(FList[index]);
end;

constructor TRectPacking.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
  MaxWidth := 0;
  MaxHeight := 0;
  Margins := 2;
end;

destructor TRectPacking.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited;
end;

procedure TRectPacking.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PRectPackData(FList[i]));
  FList.Clear;
end;

function TRectPacking.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TRectPacking.Add(const x, y, width, height: TGeoFloat);
var
  p: PRectPackData;
begin
  new(p);
  p^.Rect := FixRect(MakeRectV2(x, y, x + width, y + height));
  p^.error := True;
  p^.Data1 := nil;
  p^.Data2 := nil;
  FList.Add(p);
end;

procedure TRectPacking.Add(Data1: Pointer; Data2: TCoreClassObject; x, y, width, height: TGeoFloat);
var
  p: PRectPackData;
begin
  new(p);
  p^.Rect := FixRect(MakeRectV2(0, 0, width, height));
  p^.error := True;
  p^.Data1 := Data1;
  p^.Data2 := Data2;
  FList.Add(p);
end;

procedure TRectPacking.Add(Data1: Pointer; Data2: TCoreClassObject; r: TRectV2);
begin
  Add(Data1, Data2, 0, 0, RectWidth(r), RectHeight(r));
end;

procedure TRectPacking.Add(Data1: Pointer; Data2: TCoreClassObject; width, height: TGeoFloat);
begin
  Add(Data1, Data2, 0, 0, width, height);
end;

function TRectPacking.Data1Exists(const Data1: Pointer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FList.Count - 1 do
    if (PRectPackData(FList[i])^.Data1 = Data1) then
        Exit;
  Result := False;
end;

function TRectPacking.Data2Exists(const Data2: TCoreClassObject): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FList.Count - 1 do
    if (PRectPackData(FList[i])^.Data2 = Data2) then
        Exit;
  Result := False;
end;

procedure TRectPacking.Build(SpaceWidth, SpaceHeight: TGeoFloat);

  function ListSortCompare(Left, Right: Pointer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    Result := CompareValue(RectArea(PRectPackData(Right)^.Rect), RectArea(PRectPackData(Left)^.Rect));
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  newLst: TRectPacking;
  p: PRectPackData;
  i: Integer;
  x, y, w, h: TGeoFloat;
begin
  if FList.Count > 1 then
      QuickSortList(FList.ListData^, 0, Count - 1);

  newLst := TRectPacking.Create;
  newLst.Add(0, 0, SpaceWidth, SpaceHeight);
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];

      x := p^.Rect[0][0];
      y := p^.Rect[0][1];

      w := RectWidth(p^.Rect);
      h := RectHeight(p^.Rect);

      p^.error := not newLst.Pack(w + Margins, h + Margins, x, y);

      if not p^.error then
          p^.Rect := MakeRectV2(x, y, x + w, y + h);
    end;

  MaxWidth := newLst.MaxWidth;
  MaxHeight := newLst.MaxHeight;

  DisposeObject(newLst);
end;

procedure TRectPacking.Build;
var
  i: Integer;
  p: PRectPackData;
  w, h: TGeoFloat;
begin
  w := 0;
  h := 0;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      w := w + RectWidth(p^.Rect) + Margins + 10;
      h := h + RectHeight(p^.Rect) + Margins + 10;
    end;
  Build(w, h);
end;

initialization

finalization

end.
