{ ****************************************************************************** }
{ * geometry 3D Advance library writen by QQ 600585@qq.com                     * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ ****************************************************************************** }

unit Geometry3DUnit;

interface

{$I zDefine.inc}


uses Types, SysUtils,
  GeometryLib, Geometry2DUnit, PascalStrings, UnicodeMixedLib;

type
  TMat4 = TMatrix;
  TVec4 = TVector;
  TVec3 = TAffineVector;

  TMatrix4 = packed record
    Link: TMat4;
  public
    {$IFNDEF FPC}
    class operator Equal(const Lhs, Rhs: TMatrix4): Boolean;
    class operator NotEqual(const Lhs, Rhs: TMatrix4): Boolean;
    class operator Multiply(const Lhs, Rhs: TMatrix4): TMatrix4;
    class operator Implicit(Value: Single): TMatrix4;
    class operator Implicit(Value: TMat4): TMatrix4;
    {$ENDIF}
    function Swap: TMatrix4;
    function Lerp(m: TMatrix4; Delta: Single): TMatrix4;
    function AffineMatrix: TAffineMatrix;
    function Invert: TMatrix4;
    function Translate(v: TVec3): TMatrix4;
    function Normalize: TMatrix4;
    function Transpose: TMatrix4;
    function AnglePreservingInvert: TMatrix4;
    function Determinant: Single;
    function Adjoint: TMatrix4;
    function Pitch(Angle: Single): TMatrix4;
    function Roll(Angle: Single): TMatrix4;
    function Turn(Angle: Single): TMatrix4;
  end;

  TVector4 = packed record
    Link: TVec4;
  private
    function GetVec3: TVec3;
    procedure SetVec3(const Value: TVec3);
    function GetVec2: TVec2;
    procedure SetVec2(const Value: TVec2);
    function GetLinkValue(index: Integer): Single;
    procedure SetLinkValue(index: Integer; const Value: Single);
  public
    property Vec2                     : TVec2 read GetVec2 write SetVec2;
    property Vec3                     : TVec3 read GetVec3 write SetVec3;
    property XYZ                      : TVec3 read GetVec3 write SetVec3;
    property RGB                      : TVec3 read GetVec3 write SetVec3;
    property Vec4                     : TVec4 read Link write Link;
    property RGBA                     : TVec4 read Link write Link;
    property COLOR                    : TVec4 read Link write Link;
    property LinkValue[index: Integer]: Single read GetLinkValue write SetLinkValue; default;

    {$IFNDEF FPC}
    class operator Equal(const Lhs, Rhs: TVector4): Boolean;
    class operator NotEqual(const Lhs, Rhs: TVector4): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TVector4): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TVector4): Boolean;
    class operator LessThan(const Lhs, Rhs: TVector4): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TVector4): Boolean;

    class operator Add(const Lhs, Rhs: TVector4): TVector4;
    class operator Add(const Lhs: TVector4; const Rhs: Single): TVector4;
    class operator Add(const Lhs: Single; const Rhs: TVector4): TVector4;

    class operator Subtract(const Lhs, Rhs: TVector4): TVector4;
    class operator Subtract(const Lhs: TVector4; const Rhs: Single): TVector4;
    class operator Subtract(const Lhs: Single; const Rhs: TVector4): TVector4;

    class operator Multiply(const Lhs, Rhs: TVector4): TVector4;
    class operator Multiply(const Lhs: TVector4; const Rhs: Single): TVector4;
    class operator Multiply(const Lhs: Single; const Rhs: TVector4): TVector4;

    class operator Multiply(const Lhs: TVector4; const Rhs: TMatrix4): TVector4;
    class operator Multiply(const Lhs: TMatrix4; const Rhs: TVector4): TVector4;

    class operator Multiply(const Lhs: TVector4; const Rhs: TMat4): TVector4;
    class operator Multiply(const Lhs: TMat4; const Rhs: TVector4): TVector4;

    class operator Multiply(const Lhs: TVector4; const Rhs: TAffineMatrix): TVector4;
    class operator Multiply(const Lhs: TAffineMatrix; const Rhs: TVector4): TVector4;

    class operator Divide(const Lhs, Rhs: TVector4): TVector4;
    class operator Divide(const Lhs: TVector4; const Rhs: Single): TVector4;
    class operator Divide(const Lhs: Single; const Rhs: TVector4): TVector4;

    class operator Implicit(Value: Single): TVector4;
    class operator Implicit(Value: TVec4): TVector4;
    class operator Implicit(Value: TVec3): TVector4;
    class operator Implicit(Value: T2DPoint): TVector4;

    class operator Explicit(Value: TVector4): TVec4;
    class operator Explicit(Value: TVector4): TVec3;
    class operator Explicit(Value: TVector4): T2DPoint;
    {$ENDIF}
    procedure SetRGBA(const r, g, b, a: Single); overload;
    procedure SetLocation(const fx, fy, fz, fw: Single); overload;
    procedure SetLocation(const fx, fy, fz: Single); overload;
    function Distance4D(const v2: TVector4): Single;
    function Distance3D(const v2: TVector4): Single;
    function Distance2D(const v2: TVector4): Single;
    function Lerp(const v2: TVector4; const t: Single): TVector4;
    function LerpDistance(const v2: TVector4; const d: Single): TVector4;
    function Norm: Single;
    function Length: Single;
    function Normalize: TVector4;
    function Cross(const v2: TVector4): TVector4; overload;
    function Cross(const v2: TVec3): TVector4; overload;
    function Cross(const v2: TVec4): TVector4; overload;
  end;

  TVector3 = packed record
    Link: TVec3;
  private
    function GetVec2: TVec2;
    procedure SetVec2(const Value: TVec2);
    function GetLinkValue(index: Integer): Single;
    procedure SetLinkValue(index: Integer; const Value: Single);
  public
    property Vec2                     : TVec2 read GetVec2 write SetVec2;
    property Vec3                     : TVec3 read Link write Link;
    property XYZ                      : TVec3 read Link write Link;
    property COLOR                    : TVec3 read Link write Link;
    property RGB                      : TVec3 read Link write Link;
    property LinkValue[index: Integer]: Single read GetLinkValue write SetLinkValue; default;

    {$IFNDEF FPC}
    class operator Equal(const Lhs, Rhs: TVector3): Boolean;
    class operator NotEqual(const Lhs, Rhs: TVector3): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TVector3): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TVector3): Boolean;
    class operator LessThan(const Lhs, Rhs: TVector3): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TVector3): Boolean;

    class operator Add(const Lhs, Rhs: TVector3): TVector3;
    class operator Add(const Lhs: TVector3; const Rhs: Single): TVector3;
    class operator Add(const Lhs: Single; const Rhs: TVector3): TVector3;

    class operator Subtract(const Lhs, Rhs: TVector3): TVector3;
    class operator Subtract(const Lhs: TVector3; const Rhs: Single): TVector3;
    class operator Subtract(const Lhs: Single; const Rhs: TVector3): TVector3;

    class operator Multiply(const Lhs, Rhs: TVector3): TVector3;
    class operator Multiply(const Lhs: TVector3; const Rhs: Single): TVector3;
    class operator Multiply(const Lhs: Single; const Rhs: TVector3): TVector3;

    class operator Multiply(const Lhs: TVector3; const Rhs: TMatrix4): TVector3;
    class operator Multiply(const Lhs: TMatrix4; const Rhs: TVector3): TVector3;

    class operator Multiply(const Lhs: TVector3; const Rhs: TMat4): TVector3;
    class operator Multiply(const Lhs: TMat4; const Rhs: TVector3): TVector3;

    class operator Multiply(const Lhs: TVector3; const Rhs: TAffineMatrix): TVector3;
    class operator Multiply(const Lhs: TAffineMatrix; const Rhs: TVector3): TVector3;

    class operator Divide(const Lhs, Rhs: TVector3): TVector3;
    class operator Divide(const Lhs: TVector3; const Rhs: Single): TVector3;
    class operator Divide(const Lhs: Single; const Rhs: TVector3): TVector3;

    class operator Implicit(Value: Single): TVector3;
    class operator Implicit(Value: TVec4): TVector3;
    class operator Implicit(Value: TVec3): TVector3;
    class operator Implicit(Value: T2DPoint): TVector3;

    class operator Explicit(Value: TVector3): TVec4;
    class operator Explicit(Value: TVector3): TVec3;
    class operator Explicit(Value: TVector3): T2DPoint;
    {$ENDIF}
    procedure SetLocation(const fx, fy, fz: Single); overload;
    function Distance3D(const v2: TVector3): Single;
    function Distance2D(const v2: TVector3): Single;
    function Lerp(const v2: TVector3; const t: Single): TVector3;
    function LerpDistance(const v2: TVector3; const d: Single): TVector3;
    function Norm: Single;
    function Length: Single;
    function Normalize: TVector3;
    function Cross(const v2: TVector3): TVector3;

    function Vec4(fw: Single): TVector4; overload;
    function Vec4: TVector4; overload;
  end;

  TAABB = packed record
    min, max: TAffineVector;
  public
    { : Resize the AABB if necessary to include p. }
    procedure Include(const p: TVector3);
    { : Make an AABB that is formed by sweeping a sphere (or AABB) from Start to Dest }
    procedure FromSweep(const Start, Dest: TVector3; const Radius: Single);
    { : Returns the intersection AABB of two AABBs.<p>
      If the AABBs don't intersect, will return a degenerated AABB (plane, line or point). }
    function Intersection(const aabb2: TAABB): TAABB;
    { : Adds delta to min and max of the AABB. }
    procedure Offset(const Delta: TVector3);
    { : Checks if a point "p" is inside an AABB }
    function PointIn(const p: TVector3): Boolean;
  end;

function Vector4(X, Y, Z, W: Single): TVector4; overload;
function Vector4(X, Y, Z: Single): TVector4; overload;
function Vector4(v: TVec3): TVector4; overload;
function Vector4(v: TVec4): TVector4; overload;

function Vector3(X, Y, Z: Single): TVector3; overload;
function Vector3(v: TVec3): TVector3; overload;
function Vector3(v: TVec4): TVector3; overload;

function Vec3(const X, Y, Z: Single): TVec3; overload;
function Vec3(const v: TVec4): TVec3; overload;
function Vec3(const v: TVector3): TVec3; overload;
function Vec3(const v: TVec2): TVec3; overload;
function Vec3(const v: TVec2; Z: Single): TVec3; overload;

function Vec4(const X, Y, Z: Single): TVec4; overload;
function Vec4(const X, Y, Z, W: Single): TVec4; overload;
function Vec4(const v: TVec3): TVec4; overload;
function Vec4(const v: TVec3; const Z: Single): TVec4; overload;
function Vec4(const v: TVector3): TVec4; overload;

function Vec2(const X, Y: Single): TVec2; overload;
function Vec2(const v: TVec3): TVec2; overload;
function Vec2(const v: TVec4): TVec2; overload;
function Vec2(const v: TVector3): TVec2; overload;
function Vec2(const v: TVector4): TVec2; overload;

function VecToStr(const v: TVec2): SystemString; overload;
function VecToStr(const v: TVec3): SystemString; overload;
function VecToStr(const v: TVec4): SystemString; overload;
function VecToStr(const v: TVector3): SystemString; overload;
function VecToStr(const v: TVector4): SystemString; overload;
function RectToStr(const v: T2DRect): SystemString; overload;

function StrToVec2(const s: SystemString): TVec2;
function StrToVec3(const s: SystemString): TVec3;
function StrToVec4(const s: SystemString): TVec4;
function StrToVector3(const s: SystemString): TVector3;
function StrToVector4(const s: SystemString): TVector4;
function StrToRect(const s: SystemString): T2DRect;

function BounceVector(Current: TVector4; DeltaDistance: Single; BeginVector, EndVector: TVector4; var EndFlag: Boolean): TVector4; overload;
function BounceVector(Current: TVector3; DeltaDistance: Single; BeginVector, EndVector: TVector3; var EndFlag: Boolean): TVector3; overload;
function BounceVector(Current: TVec2; DeltaDistance: Single; BeginVector, EndVector: TVec2; var EndFlag: Boolean): TVec2; overload;
function BounceFloat(const CurrentVal, DeltaVal, StartVal, OverVal: TGeoFloat; var EndFlag: Boolean): TGeoFloat;

function GetMin(const arry: array of TGeoFloat): TGeoFloat; overload;
function GetMin(const arry: array of Integer): Integer; overload;
function GetMax(const arry: array of TGeoFloat): TGeoFloat; overload;
function GetMax(const arry: array of Integer): Integer; overload;

function FinalAngle(a: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CalcAngle(v1, v2: T2DPoint): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AngleDistance(sour, Dest: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SmoothAngle(sour, Dest, Delta: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AngleEqual(a1, a2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function distance(v1, v2: T2DPoint): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function distance(v1, v2: T2DRect): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function MovementLerp(s, d, Lerp: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MovementLerp(s, d: T2DPoint; Lerp: TGeoFloat): T2DPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MovementLerp(s, d: T2DRect; Lerp: TGeoFloat): T2DRect; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function MovementDistance(s, d: T2DPoint; dt: TGeoFloat): T2DPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MovementDistance(s, d: T2DRect; dt: TGeoFloat): T2DRect; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function MovementDistance(sour, Dest: TVector4; distance: Single): TVector4; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MovementDistance(sour, Dest: TVector3; distance: Single): TVector3; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function MovementDistanceDeltaTime(s, d: T2DPoint; ASpeed: TGeoFloat): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function MovementDistanceDeltaTime(s, d: T2DRect; ASpeed: TGeoFloat): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function AngleRollDistanceDeltaTime(s, d: TGeoFloat; ARollSpeed: TGeoFloat): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

{$IFDEF FPC}

operator := (const s: Single)r: TMatrix4;
operator := (const s: TMatrix4)r: TMat4;
operator := (const s: TMat4)r: TMatrix4;

operator := (const s: Single)r: TVector4;
operator := (const s: TVector4)r: TVec4;
operator := (const s: TVec4)r: TVector4;

operator := (const s: Single)r: TVector3;
operator := (const s: TVector3)r: TVec3;
operator := (const s: TVec3)r: TVector3;

operator * (const a: TMatrix4; const b: TMatrix4): TMatrix4;

operator * (const a: Single; const b: TVector4): TVector4;
operator * (const a: TVector4; const b: Single): TVector4;
operator * (const a: TVector4; const b: TVector4): TVector4;

operator * (const a: Single; const b: TVector3): TVector3;
operator * (const a: TVector3; const b: Single): TVector3;
operator * (const a: TVector3; const b: TVector3): TVector3;

operator / (const a: Single; const b: TVector4): TVector4;
operator / (const a: TVector4; const b: Single): TVector4;
operator / (const a: TVector4; const b: TVector4): TVector4;

operator / (const a: Single; const b: TVector3): TVector3;
operator / (const a: TVector3; const b: Single): TVector3;
operator / (const a: TVector3; const b: TVector3): TVector3;

operator + (const a: Single; const b: TVector4): TVector4;
operator + (const a: TVector4; const b: Single): TVector4;
operator + (const a: TVector4; const b: TVector4): TVector4;

operator + (const a: Single; const b: TVector3): TVector3;
operator + (const a: TVector3; const b: Single): TVector3;
operator + (const a: TVector3; const b: TVector3): TVector3;

operator - (const a: Single; const b: TVector4): TVector4;
operator - (const a: TVector4; const b: Single): TVector4;
operator - (const a: TVector4; const b: TVector4): TVector4;

operator - (const a: Single; const b: TVector3): TVector3;
operator - (const a: TVector3; const b: Single): TVector3;
operator - (const a: TVector3; const b: TVector3): TVector3;

{$ENDIF}

implementation

function Vector4(X, Y, Z, W: Single): TVector4;
begin
  Result.Link[0] := X;
  Result.Link[1] := Y;
  Result.Link[2] := Z;
  Result.Link[3] := W;
end;

function Vector4(X, Y, Z: Single): TVector4;
begin
  Result.Link[0] := X;
  Result.Link[1] := Y;
  Result.Link[2] := Z;
  Result.Link[3] := 0;
end;

function Vector4(v: TVec3): TVector4;
begin
  Result.Link[0] := v[0];
  Result.Link[1] := v[1];
  Result.Link[2] := v[2];
  Result.Link[3] := 0;
end;

function Vector4(v: TVec4): TVector4;
begin
  Result.Link := v;
end;

function Vector3(X, Y, Z: Single): TVector3;
begin
  Result.Link[0] := X;
  Result.Link[1] := Y;
  Result.Link[2] := Z;
end;

function Vector3(v: TVec3): TVector3;
begin
  Result.Link := v;
end;

function Vector3(v: TVec4): TVector3;
begin
  Result.Link[0] := v[0];
  Result.Link[1] := v[1];
  Result.Link[2] := v[2];
end;

function Vec3(const X, Y, Z: Single): TVec3;
begin
  Result := AffineVectorMake(X, Y, Z);
end;

function Vec3(const v: TVec4): TVec3;
begin
  Result[0] := v[0];
  Result[1] := v[1];
  Result[2] := v[2];
end;

function Vec3(const v: TVector3): TVec3;
begin
  Result := v.Link;
end;

function Vec3(const v: TVec2): TVec3;
begin
  Result[0] := v[0];
  Result[1] := v[1];
  Result[2] := 0;
end;

function Vec3(const v: TVec2; Z: Single): TVec3;
begin
  Result[0] := v[0];
  Result[1] := v[1];
  Result[2] := Z;
end;

function Vec4(const X, Y, Z: Single): TVec4;
begin
  Result := VectorMake(X, Y, Z, 0);
end;

function Vec4(const X, Y, Z, W: Single): TVec4;
begin
  Result := VectorMake(X, Y, Z, W);
end;

function Vec4(const v: TVec3): TVec4;
begin
  Result := VectorMake(v);
end;

function Vec4(const v: TVec3; const Z: Single): TVec4;
begin
  Result := VectorMake(v, Z);
end;

function Vec4(const v: TVector3): TVec4;
begin
  Result := VectorMake(v.Link);
end;

function Vec2(const X, Y: Single): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function Vec2(const v: TVec3): TVec2;
begin
  Result := Vec2(v[0], v[1]);
end;

function Vec2(const v: TVec4): TVec2;
begin
  Result := Vec2(v[0], v[1]);
end;

function Vec2(const v: TVector3): TVec2;
begin
  Result[0] := v.Link[0];
  Result[1] := v.Link[1];
end;

function Vec2(const v: TVector4): TVec2;
begin
  Result[0] := v.Link[0];
  Result[1] := v.Link[1];
end;

function VecToStr(const v: TVec2): SystemString;
begin
  Result := Format('%g,%g', [v[0], v[1]]);
end;

function VecToStr(const v: TVec3): SystemString;
begin
  Result := Format('%g,%g,%g', [v[0], v[1], v[2]]);
end;

function VecToStr(const v: TVec4): SystemString;
begin
  Result := Format('%g,%g,%g,%g', [v[0], v[1], v[2], v[3]]);
end;

function VecToStr(const v: TVector3): SystemString;
begin
  Result := VecToStr(v.Link);
end;

function VecToStr(const v: TVector4): SystemString;
begin
  Result := VecToStr(v.Link);
end;

function RectToStr(const v: T2DRect): SystemString;
begin
  Result := Format('%g,%g,%g,%g', [v[0][0], v[0][1], v[1][0], v[1][1]]);
end;

function StrToVec2(const s: SystemString): TVec2;
var
  v, v1, v2: umlString;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');

  Result[0] := umlStrToFloat(v1, 0);
  Result[1] := umlStrToFloat(v2, 0);
end;

function StrToVec3(const s: SystemString): TVec3;
var
  v, v1, v2, v3: umlString;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');

  Result[0] := umlStrToFloat(v1, 0);
  Result[1] := umlStrToFloat(v2, 0);
  Result[2] := umlStrToFloat(v3, 0);
end;

function StrToVec4(const s: SystemString): TVec4;
var
  v, v1, v2, v3, v4: umlString;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v4 := umlGetFirstStr(v, ',: ');

  Result[0] := umlStrToFloat(v1, 0);
  Result[1] := umlStrToFloat(v2, 0);
  Result[2] := umlStrToFloat(v3, 0);
  Result[3] := umlStrToFloat(v4, 0);
end;

function StrToVector3(const s: SystemString): TVector3;
var
  v, v1, v2, v3: umlString;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');

  Result.Link[0] := umlStrToFloat(v1, 0);
  Result.Link[1] := umlStrToFloat(v2, 0);
  Result.Link[2] := umlStrToFloat(v3, 0);
end;

function StrToVector4(const s: SystemString): TVector4;
var
  v, v1, v2, v3, v4: umlString;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v4 := umlGetFirstStr(v, ',: ');

  Result.Link[0] := umlStrToFloat(v1, 0);
  Result.Link[1] := umlStrToFloat(v2, 0);
  Result.Link[2] := umlStrToFloat(v3, 0);
  Result.Link[3] := umlStrToFloat(v4, 0);
end;

function StrToRect(const s: SystemString): T2DRect;
var
  v, v1, v2, v3, v4: umlString;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v4 := umlGetFirstStr(v, ',: ');

  Result[0][0] := umlStrToFloat(v1, 0);
  Result[0][1] := umlStrToFloat(v2, 0);
  Result[1][0] := umlStrToFloat(v3, 0);
  Result[1][1] := umlStrToFloat(v4, 0);
end;

function BounceVector(Current: TVector4; DeltaDistance: Single; BeginVector, EndVector: TVector4; var EndFlag: Boolean): TVector4;
  function ToVector: TVector4;
  begin
    if EndFlag then
        Result := EndVector
    else
        Result := BeginVector;
  end;

var
  k: Single;
begin
  k := Current.Distance4D(ToVector);
  if k >= DeltaDistance then
      Result := MovementDistance(Current, ToVector, DeltaDistance)
  else
    begin
      Result := ToVector;
      EndFlag := not EndFlag;
      Result := MovementDistance(Result, ToVector, DeltaDistance - k);
    end;
end;

function BounceVector(Current: TVector3; DeltaDistance: Single; BeginVector, EndVector: TVector3; var EndFlag: Boolean): TVector3;
  function ToVector: TVector3;
  begin
    if EndFlag then
        Result := EndVector
    else
        Result := BeginVector;
  end;

var
  k: Single;
begin
  k := Current.Distance3D(ToVector);
  if k >= DeltaDistance then
      Result := MovementDistance(Current, ToVector, DeltaDistance)
  else
    begin
      Result := ToVector;
      EndFlag := not EndFlag;
      Result := MovementDistance(Result, ToVector, DeltaDistance - k);
    end;
end;

function BounceVector(Current: TVec2; DeltaDistance: Single; BeginVector, EndVector: TVec2; var EndFlag: Boolean): TVec2;
  function ToVector: TVec2;
  begin
    if EndFlag then
        Result := EndVector
    else
        Result := BeginVector;
  end;

var
  k: Single;
begin
  k := PointDistance(Current, ToVector);
  if k >= DeltaDistance then
      Result := PointLerpTo(Current, ToVector, DeltaDistance)
  else
    begin
      Result := ToVector;
      EndFlag := not EndFlag;
      Result := PointLerpTo(Result, ToVector, DeltaDistance - k);
    end;
end;

function BounceFloat(const CurrentVal, DeltaVal, StartVal, OverVal: TGeoFloat; var EndFlag: Boolean): TGeoFloat;
  function IfOut(Cur, Delta, Dest: Single): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    if Cur > Dest then
        Result := Cur - Delta < Dest
    else
        Result := Cur + Delta > Dest;
  end;

  function GetOutValue(Cur, Delta, Dest: Single): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    if IfOut(Cur, Delta, Dest) then
      begin
        if Cur > Dest then
            Result := Dest - (Cur - Delta)
        else
            Result := Cur + Delta - Dest;
      end
    else
        Result := 0;
  end;

  function GetDeltaValue(Cur, Delta, Dest: Single): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    if Cur > Dest then
        Result := Cur - Delta
    else
        Result := Cur + Delta;
  end;

begin
  if (DeltaVal > 0) and (StartVal <> OverVal) then
    begin
      if EndFlag then
        begin
          if IfOut(CurrentVal, DeltaVal, OverVal) then
            begin
              EndFlag := False;
              Result := umlProcessCycleValue(OverVal, GetOutValue(CurrentVal, DeltaVal, OverVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, OverVal);
        end
      else
        begin
          if IfOut(CurrentVal, DeltaVal, StartVal) then
            begin
              EndFlag := True;
              Result := umlProcessCycleValue(StartVal, GetOutValue(CurrentVal, DeltaVal, StartVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, StartVal);
        end
    end
  else
      Result := CurrentVal;
end;

function GetMin(const arry: array of TGeoFloat): TGeoFloat;
var
  I: Integer;
begin
  Result := arry[low(arry)];
  for I := low(arry) + 1 to high(arry) do
    if Result > arry[I] then
        Result := arry[I];
end;

function GetMin(const arry: array of Integer): Integer;
var
  I: Integer;
begin
  Result := arry[low(arry)];
  for I := low(arry) + 1 to high(arry) do
    if Result > arry[I] then
        Result := arry[I];
end;

function GetMax(const arry: array of TGeoFloat): TGeoFloat;
var
  I: Integer;
begin
  Result := arry[low(arry)];
  for I := low(arry) + 1 to high(arry) do
    if Result < arry[I] then
        Result := arry[I];
end;

function GetMax(const arry: array of Integer): Integer;
var
  I: Integer;
begin
  Result := arry[low(arry)];
  for I := low(arry) + 1 to high(arry) do
    if Result < arry[I] then
        Result := arry[I];
end;

function FinalAngle(a: TGeoFloat): TGeoFloat;
begin
  Result := NormalizeDegAngle((-a - 90) + 180);
end;

function CalcAngle(v1, v2: T2DPoint): TGeoFloat;
begin
  if IsEqual(v1, v2) then
      Result := 0
  else
      Result := RadToDeg(ArcTan2(v1[0] - v2[0], v1[1] - v2[1]));
end;

function AngleDistance(sour, Dest: TGeoFloat): TGeoFloat;
begin
  Result := Abs(sour - Dest);
  if Result > 180.0 then
      Result := 360.0 - Result;
end;

function SmoothAngle(sour, Dest, Delta: TGeoFloat): TGeoFloat;
var
  a1, a2: TGeoFloat;
begin
  if sour <> Dest then
    begin
      if sour >= 0 then
        begin
          a1 := sour + Delta;
          a2 := sour - Delta;
        end
      else
        begin
          a1 := sour + -Delta;
          a2 := sour + Delta;
        end;

      if AngleDistance(Dest, a1) >= AngleDistance(Dest, a2) then
        begin
          if AngleDistance(Dest, a2) > Delta then
              Result := a2
          else
              Result := Dest;
        end
      else if AngleDistance(Dest, a1) > Delta then
          Result := a1
      else
          Result := Dest;
    end
  else
      Result := Dest;
end;

function AngleEqual(a1, a2: TGeoFloat): Boolean;
begin
  Result := AngleDistance(a1, a2) < 0.01;
end;

function distance(v1, v2: T2DPoint): TGeoFloat;
begin
  Result := PointDistance(v1, v2);
end;

function distance(v1, v2: T2DRect): TGeoFloat;
var
  d1, d2: TGeoFloat;
begin
  d1 := PointDistance(v1[0], v2[0]);
  d2 := PointDistance(v1[1], v2[1]);
  if d1 >= d2 then
      Result := d1
  else
      Result := d2;
end;

function MovementLerp(s, d, Lerp: TGeoFloat): TGeoFloat;
begin
  if Lerp < 1.0 then
      Result := s + Lerp * (d - s)
  else
      Result := d;
end;

function MovementLerp(s, d: T2DPoint; Lerp: TGeoFloat): T2DPoint;
begin
  if Lerp < 1.0 then
    begin
      Result[0] := s[0] + Lerp * (d[0] - s[0]);
      Result[1] := s[1] + Lerp * (d[1] - s[1]);
    end
  else
      Result := d;
end;

function MovementLerp(s, d: T2DRect; Lerp: TGeoFloat): T2DRect;
begin
  if Lerp < 1.0 then
    begin
      Result[0] := MovementLerp(s[0], d[0], Lerp);
      Result[1] := MovementLerp(s[1], d[1], Lerp);
    end
  else
      Result := d;
end;

function MovementDistance(s, d: T2DPoint; dt: TGeoFloat): T2DPoint;
var
  k: Double;
begin
  k := dt / sqrt((d[0] - s[0]) * (d[0] - s[0]) + (d[1] - s[1]) * (d[1] - s[1]));
  Result[0] := s[0] + k * (d[0] - s[0]);
  Result[1] := s[1] + k * (d[1] - s[1]);
end;

function MovementDistance(s, d: T2DRect; dt: TGeoFloat): T2DRect;
begin
  if distance(s[0], d[0]) > dt then
      Result[0] := MovementDistance(s[0], d[0], dt)
  else
      Result[0] := d[0];

  if distance(s[1], d[1]) > dt then
      Result[1] := MovementDistance(s[1], d[1], dt)
  else
      Result[1] := d[1];
end;

function MovementDistance(sour, Dest: TVector4; distance: Single): TVector4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  k: Single;
begin
  // calc distance
  k := distance / sqrt((Dest[0] - sour[0]) * (Dest[0] - sour[0]) + (Dest[1] - sour[1]) * (Dest[1] - sour[1]) + (Dest[2] - sour[2]) * (Dest[2] - sour[2]) + (Dest[3] - sour[3]) *
    (Dest[3] - sour[3]));
  // done
  Result[0] := sour[0] + k * (Dest[0] - sour[0]);
  Result[1] := sour[1] + k * (Dest[1] - sour[1]);
  Result[2] := sour[2] + k * (Dest[2] - sour[2]);
  Result[3] := sour[3] + k * (Dest[3] - sour[3]);
end;

function MovementDistance(sour, Dest: TVector3; distance: Single): TVector3; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  k: Single;
begin
  // calc distance
  k := distance / sqrt((Dest[0] - sour[0]) * (Dest[0] - sour[0]) + (Dest[1] - sour[1]) * (Dest[1] - sour[1]) + (Dest[2] - sour[2]) * (Dest[2] - sour[2]));
  // done
  Result[0] := sour[0] + k * (Dest[0] - sour[0]);
  Result[1] := sour[1] + k * (Dest[1] - sour[1]);
  Result[2] := sour[2] + k * (Dest[2] - sour[2]);
end;

function MovementDistanceDeltaTime(s, d: T2DPoint; ASpeed: TGeoFloat): Double;
begin
  Result := distance(s, d) / ASpeed;
end;

function MovementDistanceDeltaTime(s, d: T2DRect; ASpeed: TGeoFloat): Double;
var
  d1, d2: Double;
begin
  d1 := MovementDistanceDeltaTime(s[0], d[0], ASpeed);
  d2 := MovementDistanceDeltaTime(s[1], d[1], ASpeed);
  if d1 > d2 then
      Result := d1
  else
      Result := d2;
end;

function AngleRollDistanceDeltaTime(s, d: TGeoFloat; ARollSpeed: TGeoFloat): Double;
begin
  Result := AngleDistance(s, d) / ARollSpeed;
end;

{$IFNDEF FPC}


class operator TMatrix4.Equal(const Lhs, Rhs: TMatrix4): Boolean;
begin
  Result := VectorEquals(Lhs.Link[0], Rhs.Link[0]) and VectorEquals(Lhs.Link[1], Rhs.Link[1]) and VectorEquals(Lhs.Link[2], Rhs.Link[2]) and VectorEquals(Lhs.Link[3], Rhs.Link[3]);
end;

class operator TMatrix4.NotEqual(const Lhs, Rhs: TMatrix4): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

class operator TMatrix4.Multiply(const Lhs, Rhs: TMatrix4): TMatrix4;
begin
  Result.Link := GeometryLib.MatrixMultiply(Lhs.Link, Rhs.Link);
end;

class operator TMatrix4.Implicit(Value: Single): TMatrix4;
var
  I, j: Integer;
begin
  for I := 0 to 3 do
    for j := 0 to 3 do
        Result.Link[I, j] := Value;
end;

class operator TMatrix4.Implicit(Value: TMat4): TMatrix4;
begin
  Result.Link := Value;
end;

{$ENDIF}


function TMatrix4.Swap: TMatrix4;
var
  I, j: Integer;
begin
  for I := 0 to 3 do
    for j := 0 to 3 do
        Result.Link[j, I] := Link[I, j];
end;

function TMatrix4.Lerp(m: TMatrix4; Delta: Single): TMatrix4;
var
  I, j: Integer;
begin
  for j := 0 to 3 do
    for I := 0 to 3 do
        Result.Link[I][j] := Link[I][j] + (m.Link[I][j] - Link[I][j]) * Delta;
end;

function TMatrix4.AffineMatrix: TAffineMatrix;
begin
  Result[0, 0] := Link[0, 0];
  Result[0, 1] := Link[0, 1];
  Result[0, 2] := Link[0, 2];
  Result[1, 0] := Link[1, 0];
  Result[1, 1] := Link[1, 1];
  Result[1, 2] := Link[1, 2];
  Result[2, 0] := Link[2, 0];
  Result[2, 1] := Link[2, 1];
  Result[2, 2] := Link[2, 2];
end;

function TMatrix4.Invert: TMatrix4;
var
  det: Single;
begin
  Result.Link := Link;
  det := GeometryLib.MatrixDeterminant(Result.Link);
  if Abs(det) < EPSILON then
      Result.Link := GeometryLib.IdentityHmgMatrix
  else
    begin
      GeometryLib.AdjointMatrix(Result.Link);
      GeometryLib.ScaleMatrix(Result.Link, 1 / det);
    end;
end;

function TMatrix4.Translate(v: TVec3): TMatrix4;
begin
  Result.Link := Link;
  GeometryLib.TranslateMatrix(Result.Link, v);
end;

function TMatrix4.Normalize: TMatrix4;
begin
  Result.Link := Link;
  GeometryLib.NormalizeMatrix(Result.Link);
end;

function TMatrix4.Transpose: TMatrix4;
begin
  Result.Link := Link;
  GeometryLib.TransposeMatrix(Result.Link);
end;

function TMatrix4.AnglePreservingInvert: TMatrix4;
begin
  Result.Link := GeometryLib.AnglePreservingMatrixInvert(Link);
end;

function TMatrix4.Determinant: Single;
begin
  Result := GeometryLib.MatrixDeterminant(Link);
end;

function TMatrix4.Adjoint: TMatrix4;
begin
  Result.Link := Link;
  GeometryLib.AdjointMatrix(Result.Link);
end;

function TMatrix4.Pitch(Angle: Single): TMatrix4;
begin
  Result.Link := GeometryLib.Pitch(Link, Angle);
end;

function TMatrix4.Roll(Angle: Single): TMatrix4;
begin
  Result.Link := GeometryLib.Roll(Link, Angle);
end;

function TMatrix4.Turn(Angle: Single): TMatrix4;
begin
  Result.Link := GeometryLib.Turn(Link, Angle);
end;

function TVector4.GetVec3: TVec3;
begin
  Result := AffineVectorMake(Link);
end;

procedure TVector4.SetVec3(const Value: TVec3);
begin
  Link := VectorMake(Value);
end;

function TVector4.GetVec2: TVec2;
begin
  Result[0] := Link[0];
  Result[1] := Link[1];
end;

procedure TVector4.SetVec2(const Value: TVec2);
begin
  Link[0] := Value[0];
  Link[1] := Value[1];
end;

function TVector4.GetLinkValue(index: Integer): Single;
begin
  Result := Link[index];
end;

procedure TVector4.SetLinkValue(index: Integer; const Value: Single);
begin
  Link[index] := Value;
end;

{$IFNDEF FPC}


class operator TVector4.Equal(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Link[0] = Rhs.Link[0]) and (Lhs.Link[1] = Rhs.Link[1]) and (Lhs.Link[2] = Rhs.Link[2]) and (Lhs.Link[3] = Rhs.Link[3]);
end;

class operator TVector4.NotEqual(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Link[0] <> Rhs.Link[0]) or (Lhs.Link[1] <> Rhs.Link[1]) or (Lhs.Link[2] <> Rhs.Link[2]) or (Lhs.Link[3] <> Rhs.Link[3]);
end;

class operator TVector4.GreaterThan(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Link[0] > Rhs.Link[0]) and (Lhs.Link[1] > Rhs.Link[1]) and (Lhs.Link[2] > Rhs.Link[2]) and (Lhs.Link[3] > Rhs.Link[3]);
end;

class operator TVector4.GreaterThanOrEqual(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Link[0] >= Rhs.Link[0]) and (Lhs.Link[1] >= Rhs.Link[1]) and (Lhs.Link[2] >= Rhs.Link[2]) and (Lhs.Link[3] >= Rhs.Link[3]);
end;

class operator TVector4.LessThan(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Link[0] < Rhs.Link[0]) and (Lhs.Link[1] < Rhs.Link[1]) and (Lhs.Link[2] < Rhs.Link[2]) and (Lhs.Link[3] < Rhs.Link[3]);
end;

class operator TVector4.LessThanOrEqual(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Link[0] <= Rhs.Link[0]) and (Lhs.Link[1] <= Rhs.Link[1]) and (Lhs.Link[2] <= Rhs.Link[2]) and (Lhs.Link[3] <= Rhs.Link[3]);
end;

class operator TVector4.Add(const Lhs, Rhs: TVector4): TVector4;
begin
  Result.Link[0] := Lhs.Link[0] + Rhs.Link[0];
  Result.Link[1] := Lhs.Link[1] + Rhs.Link[1];
  Result.Link[2] := Lhs.Link[2] + Rhs.Link[2];
  Result.Link[3] := Lhs.Link[3] + Rhs.Link[3];
end;

class operator TVector4.Add(const Lhs: TVector4; const Rhs: Single): TVector4;
begin
  Result.Link[0] := Lhs.Link[0] + Rhs;
  Result.Link[1] := Lhs.Link[1] + Rhs;
  Result.Link[2] := Lhs.Link[2] + Rhs;
  Result.Link[3] := Lhs.Link[3] + Rhs;
end;

class operator TVector4.Add(const Lhs: Single; const Rhs: TVector4): TVector4;
begin
  Result.Link[0] := Lhs + Rhs.Link[0];
  Result.Link[1] := Lhs + Rhs.Link[1];
  Result.Link[2] := Lhs + Rhs.Link[2];
  Result.Link[3] := Lhs + Rhs.Link[3];
end;

class operator TVector4.Subtract(const Lhs, Rhs: TVector4): TVector4;
begin
  Result.Link[0] := Lhs.Link[0] - Rhs.Link[0];
  Result.Link[1] := Lhs.Link[1] - Rhs.Link[1];
  Result.Link[2] := Lhs.Link[2] - Rhs.Link[2];
  Result.Link[3] := Lhs.Link[3] - Rhs.Link[3];
end;

class operator TVector4.Subtract(const Lhs: TVector4; const Rhs: Single): TVector4;
begin
  Result.Link[0] := Lhs.Link[0] - Rhs;
  Result.Link[1] := Lhs.Link[1] - Rhs;
  Result.Link[2] := Lhs.Link[2] - Rhs;
  Result.Link[3] := Lhs.Link[3] - Rhs;
end;

class operator TVector4.Subtract(const Lhs: Single; const Rhs: TVector4): TVector4;
begin
  Result.Link[0] := Lhs - Rhs.Link[0];
  Result.Link[1] := Lhs - Rhs.Link[1];
  Result.Link[2] := Lhs - Rhs.Link[2];
  Result.Link[3] := Lhs - Rhs.Link[3];
end;

class operator TVector4.Multiply(const Lhs, Rhs: TVector4): TVector4;
begin
  Result.Link[0] := Lhs.Link[0] * Rhs.Link[0];
  Result.Link[1] := Lhs.Link[1] * Rhs.Link[1];
  Result.Link[2] := Lhs.Link[2] * Rhs.Link[2];
  Result.Link[3] := Lhs.Link[2] * Rhs.Link[3];
end;

class operator TVector4.Multiply(const Lhs: TVector4; const Rhs: Single): TVector4;
begin
  Result.Link[0] := Lhs.Link[0] * Rhs;
  Result.Link[1] := Lhs.Link[1] * Rhs;
  Result.Link[2] := Lhs.Link[2] * Rhs;
  Result.Link[3] := Lhs.Link[3] * Rhs;
end;

class operator TVector4.Multiply(const Lhs: Single; const Rhs: TVector4): TVector4;
begin
  Result.Link[0] := Lhs * Rhs.Link[0];
  Result.Link[1] := Lhs * Rhs.Link[1];
  Result.Link[2] := Lhs * Rhs.Link[2];
  Result.Link[3] := Lhs * Rhs.Link[3];
end;

class operator TVector4.Multiply(const Lhs: TVector4; const Rhs: TMatrix4): TVector4;
begin
  Result.Link := GeometryLib.VectorTransform(Lhs.Link, Rhs.Link);
end;

class operator TVector4.Multiply(const Lhs: TMatrix4; const Rhs: TVector4): TVector4;
begin
  Result.Link := VectorTransform(Rhs.Link, Lhs.Link);
end;

class operator TVector4.Multiply(const Lhs: TVector4; const Rhs: TMat4): TVector4;
begin
  Result.Link := GeometryLib.VectorTransform(Lhs.Link, Rhs);
end;

class operator TVector4.Multiply(const Lhs: TMat4; const Rhs: TVector4): TVector4;
begin
  Result.Link := GeometryLib.VectorTransform(Rhs.Link, Lhs);
end;

class operator TVector4.Multiply(const Lhs: TVector4; const Rhs: TAffineMatrix): TVector4;
begin
  Result.Link := GeometryLib.VectorTransform(Lhs.Link, Rhs);
end;

class operator TVector4.Multiply(const Lhs: TAffineMatrix; const Rhs: TVector4): TVector4;
begin
  Result.Link := GeometryLib.VectorTransform(Rhs.Link, Lhs);
end;

class operator TVector4.Divide(const Lhs, Rhs: TVector4): TVector4;
begin
  Result.Link[0] := Lhs.Link[0] / Rhs.Link[0];
  Result.Link[1] := Lhs.Link[1] / Rhs.Link[1];
  Result.Link[2] := Lhs.Link[2] / Rhs.Link[2];
  Result.Link[3] := Lhs.Link[3] / Rhs.Link[3];
end;

class operator TVector4.Divide(const Lhs: TVector4; const Rhs: Single): TVector4;
begin
  Result.Link[0] := Lhs.Link[0] / Rhs;
  Result.Link[1] := Lhs.Link[1] / Rhs;
  Result.Link[2] := Lhs.Link[2] / Rhs;
  Result.Link[3] := Lhs.Link[3] / Rhs;
end;

class operator TVector4.Divide(const Lhs: Single; const Rhs: TVector4): TVector4;
begin
  Result.Link[0] := Lhs / Rhs.Link[0];
  Result.Link[1] := Lhs / Rhs.Link[1];
  Result.Link[2] := Lhs / Rhs.Link[2];
  Result.Link[3] := Lhs / Rhs.Link[3];
end;

class operator TVector4.Implicit(Value: Single): TVector4;
begin
  Result.Link[0] := Value;
  Result.Link[1] := Value;
  Result.Link[2] := Value;
  Result.Link[3] := Value;
end;

class operator TVector4.Implicit(Value: TVec4): TVector4;
begin
  Result.Link := Value;
end;

class operator TVector4.Implicit(Value: TVec3): TVector4;
begin
  Result.Link := VectorMake(Value);
end;

class operator TVector4.Implicit(Value: T2DPoint): TVector4;
begin
  Result.Link := VectorMake(Value[0], Value[1], 0, 0);
end;

class operator TVector4.Explicit(Value: TVector4): TVec4;
begin
  Result := Value.Link;
end;

class operator TVector4.Explicit(Value: TVector4): TVec3;
begin
  Result := AffineVectorMake(Value.Link);
end;

class operator TVector4.Explicit(Value: TVector4): T2DPoint;
begin
  Result[0] := Value.Link[0];
  Result[1] := Value.Link[1];
end;

{$ENDIF}


procedure TVector4.SetRGBA(const r, g, b, a: Single);
begin
  Link[0] := r;
  Link[1] := g;
  Link[2] := b;
  Link[3] := a;
end;

procedure TVector4.SetLocation(const fx, fy, fz, fw: Single);
begin
  Link[0] := fx;
  Link[1] := fy;
  Link[2] := fz;
  Link[3] := fw;
end;

procedure TVector4.SetLocation(const fx, fy, fz: Single);
begin
  Link[0] := fx;
  Link[1] := fy;
  Link[2] := fz;
end;

function TVector4.Distance4D(const v2: TVector4): Single;
begin
  Result := sqrt(Sqr(v2.Link[0] - Link[0]) + Sqr(v2.Link[1] - Link[1]) + Sqr(v2.Link[2] - Link[2]) + Sqr(v2.Link[3] - Link[3]));
end;

function TVector4.Distance3D(const v2: TVector4): Single;
begin
  Result := sqrt(Sqr(v2.Link[0] - Link[0]) + Sqr(v2.Link[1] - Link[1]) + Sqr(v2.Link[2] - Link[2]));
end;

function TVector4.Distance2D(const v2: TVector4): Single;
begin
  Result := sqrt(Sqr(v2.Link[0] - Link[0]) + Sqr(v2.Link[1] - Link[1]));
end;

function TVector4.Lerp(const v2: TVector4; const t: Single): TVector4;
begin
  Result.Link[0] := Link[0] + (v2.Link[0] - Link[0]) * t;
  Result.Link[1] := Link[1] + (v2.Link[1] - Link[1]) * t;
  Result.Link[2] := Link[2] + (v2.Link[2] - Link[2]) * t;
  Result.Link[3] := Link[3] + (v2.Link[3] - Link[3]) * t;
end;

function TVector4.LerpDistance(const v2: TVector4; const d: Single): TVector4;
var
  k: Double;
begin
  k := d / sqrt((v2.Link[0] - Link[0]) * (v2.Link[0] - Link[0]) + (v2.Link[1] - Link[1]) * (v2.Link[1] - Link[1]) + (v2.Link[2] - Link[2]) * (v2.Link[2] - Link[2]) +
    (v2.Link[3] - Link[3]) * (v2.Link[3] - Link[3]));
  Result.Link[0] := Link[0] + k * (v2.Link[0] - Link[0]);
  Result.Link[1] := Link[1] + k * (v2.Link[1] - Link[1]);
  Result.Link[2] := Link[2] + k * (v2.Link[2] - Link[2]);
  Result.Link[3] := Link[3] + k * (v2.Link[3] - Link[3]);
end;

function TVector4.Norm: Single;
begin
  Result := Link[0] * Link[0] + Link[1] * Link[1] + Link[2] * Link[2] + Link[3] * Link[3];
end;

function TVector4.Length: Single;
begin
  Result := sqrt(Norm);
end;

function TVector4.Normalize: TVector4;
var
  invLen: Single;
  vn    : Single;
begin
  vn := Norm;
  if vn = 0 then
      Result := Self
  else
    begin
      invLen := RSqrt(vn);
      Result.Link[0] := Link[0] * invLen;
      Result.Link[1] := Link[1] * invLen;
      Result.Link[2] := Link[2] * invLen;
      Result.Link[3] := Link[3] * invLen;
    end;
end;

function TVector4.Cross(const v2: TVector4): TVector4;
begin
  Result.Link[0] := Link[1] * v2.Link[2] - Link[2] * v2.Link[1];
  Result.Link[1] := Link[2] * v2.Link[0] - Link[0] * v2.Link[2];
  Result.Link[2] := Link[0] * v2.Link[1] - Link[1] * v2.Link[0];
  Result.Link[3] := 0;
end;

function TVector4.Cross(const v2: TVec3): TVector4;
begin
  Result.Link[0] := Link[1] * v2[2] - Link[2] * v2[1];
  Result.Link[1] := Link[2] * v2[0] - Link[0] * v2[2];
  Result.Link[2] := Link[0] * v2[1] - Link[1] * v2[0];
  Result.Link[3] := 0;
end;

function TVector4.Cross(const v2: TVec4): TVector4;
begin
  Result.Link[0] := Link[1] * v2[2] - Link[2] * v2[1];
  Result.Link[1] := Link[2] * v2[0] - Link[0] * v2[2];
  Result.Link[2] := Link[0] * v2[1] - Link[1] * v2[0];
  Result.Link[3] := 0;
end;

function TVector3.GetVec2: TVec2;
begin
  Result[0] := Link[0];
  Result[1] := Link[1];
end;

procedure TVector3.SetVec2(const Value: TVec2);
begin
  Link[0] := Value[0];
  Link[1] := Value[1];
end;

function TVector3.GetLinkValue(index: Integer): Single;
begin
  Result := Link[index];
end;

procedure TVector3.SetLinkValue(index: Integer; const Value: Single);
begin
  Link[index] := Value;
end;

{$IFNDEF FPC}


class operator TVector3.Equal(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Link[0] = Rhs.Link[0]) and (Lhs.Link[1] = Rhs.Link[1]) and (Lhs.Link[2] = Rhs.Link[2]);
end;

class operator TVector3.NotEqual(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Link[0] <> Rhs.Link[0]) or (Lhs.Link[1] <> Rhs.Link[1]) or (Lhs.Link[2] <> Rhs.Link[2]);
end;

class operator TVector3.GreaterThan(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Link[0] > Rhs.Link[0]) and (Lhs.Link[1] > Rhs.Link[1]) and (Lhs.Link[2] > Rhs.Link[2]);
end;

class operator TVector3.GreaterThanOrEqual(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Link[0] >= Rhs.Link[0]) and (Lhs.Link[1] >= Rhs.Link[1]) and (Lhs.Link[2] >= Rhs.Link[2]);
end;

class operator TVector3.LessThan(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Link[0] < Rhs.Link[0]) and (Lhs.Link[1] < Rhs.Link[1]) and (Lhs.Link[2] < Rhs.Link[2]);
end;

class operator TVector3.LessThanOrEqual(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Link[0] <= Rhs.Link[0]) and (Lhs.Link[1] <= Rhs.Link[1]) and (Lhs.Link[2] <= Rhs.Link[2]);
end;

class operator TVector3.Add(const Lhs, Rhs: TVector3): TVector3;
begin
  Result.Link[0] := Lhs.Link[0] + Rhs.Link[0];
  Result.Link[1] := Lhs.Link[1] + Rhs.Link[1];
  Result.Link[2] := Lhs.Link[2] + Rhs.Link[2];
end;

class operator TVector3.Add(const Lhs: TVector3; const Rhs: Single): TVector3;
begin
  Result.Link[0] := Lhs.Link[0] + Rhs;
  Result.Link[1] := Lhs.Link[1] + Rhs;
  Result.Link[2] := Lhs.Link[2] + Rhs;
end;

class operator TVector3.Add(const Lhs: Single; const Rhs: TVector3): TVector3;
begin
  Result.Link[0] := Lhs + Rhs.Link[0];
  Result.Link[1] := Lhs + Rhs.Link[1];
  Result.Link[2] := Lhs + Rhs.Link[2];
end;

class operator TVector3.Subtract(const Lhs, Rhs: TVector3): TVector3;
begin
  Result.Link[0] := Lhs.Link[0] - Rhs.Link[0];
  Result.Link[1] := Lhs.Link[1] - Rhs.Link[1];
  Result.Link[2] := Lhs.Link[2] - Rhs.Link[2];
end;

class operator TVector3.Subtract(const Lhs: TVector3; const Rhs: Single): TVector3;
begin
  Result.Link[0] := Lhs.Link[0] - Rhs;
  Result.Link[1] := Lhs.Link[1] - Rhs;
  Result.Link[2] := Lhs.Link[2] - Rhs;
end;

class operator TVector3.Subtract(const Lhs: Single; const Rhs: TVector3): TVector3;
begin
  Result.Link[0] := Lhs - Rhs.Link[0];
  Result.Link[1] := Lhs - Rhs.Link[1];
  Result.Link[2] := Lhs - Rhs.Link[2];
end;

class operator TVector3.Multiply(const Lhs, Rhs: TVector3): TVector3;
begin
  Result.Link[0] := Lhs.Link[0] * Rhs.Link[0];
  Result.Link[1] := Lhs.Link[1] * Rhs.Link[1];
  Result.Link[2] := Lhs.Link[2] * Rhs.Link[2];
end;

class operator TVector3.Multiply(const Lhs: TVector3; const Rhs: Single): TVector3;
begin
  Result.Link[0] := Lhs.Link[0] * Rhs;
  Result.Link[1] := Lhs.Link[1] * Rhs;
  Result.Link[2] := Lhs.Link[2] * Rhs;
end;

class operator TVector3.Multiply(const Lhs: Single; const Rhs: TVector3): TVector3;
begin
  Result.Link[0] := Lhs * Rhs.Link[0];
  Result.Link[1] := Lhs * Rhs.Link[1];
  Result.Link[2] := Lhs * Rhs.Link[2];
end;

class operator TVector3.Multiply(const Lhs: TVector3; const Rhs: TMatrix4): TVector3;
begin
  Result.Link := GeometryLib.VectorTransform(Lhs.Link, Rhs.Link);
end;

class operator TVector3.Multiply(const Lhs: TMatrix4; const Rhs: TVector3): TVector3;
begin
  Result.Link := GeometryLib.VectorTransform(Rhs.Link, Lhs.Link);
end;

class operator TVector3.Multiply(const Lhs: TVector3; const Rhs: TMat4): TVector3;
begin
  Result.Link := GeometryLib.VectorTransform(Lhs.Link, Rhs);
end;

class operator TVector3.Multiply(const Lhs: TMat4; const Rhs: TVector3): TVector3;
begin
  Result.Link := GeometryLib.VectorTransform(Rhs.Link, Lhs);
end;

class operator TVector3.Multiply(const Lhs: TVector3; const Rhs: TAffineMatrix): TVector3;
begin
  Result.Link := GeometryLib.VectorTransform(Lhs.Link, Rhs);
end;

class operator TVector3.Multiply(const Lhs: TAffineMatrix; const Rhs: TVector3): TVector3;
begin
  Result.Link := GeometryLib.VectorTransform(Rhs.Link, Lhs);
end;

class operator TVector3.Divide(const Lhs, Rhs: TVector3): TVector3;
begin
  Result.Link[0] := Lhs.Link[0] / Rhs.Link[0];
  Result.Link[1] := Lhs.Link[1] / Rhs.Link[1];
  Result.Link[2] := Lhs.Link[2] / Rhs.Link[2];
end;

class operator TVector3.Divide(const Lhs: TVector3; const Rhs: Single): TVector3;
begin
  Result.Link[0] := Lhs.Link[0] / Rhs;
  Result.Link[1] := Lhs.Link[1] / Rhs;
  Result.Link[2] := Lhs.Link[2] / Rhs;
end;

class operator TVector3.Divide(const Lhs: Single; const Rhs: TVector3): TVector3;
begin
  Result.Link[0] := Lhs / Rhs.Link[0];
  Result.Link[1] := Lhs / Rhs.Link[1];
  Result.Link[2] := Lhs / Rhs.Link[2];
end;

class operator TVector3.Implicit(Value: Single): TVector3;
begin
  Result.Link[0] := Value;
  Result.Link[1] := Value;
  Result.Link[2] := Value;
end;

class operator TVector3.Implicit(Value: TVec4): TVector3;
begin
  Result.Link := AffineVectorMake(Value);
end;

class operator TVector3.Implicit(Value: TVec3): TVector3;
begin
  Result.Link := Value;
end;

class operator TVector3.Implicit(Value: T2DPoint): TVector3;
begin
  Result.Link := AffineVectorMake(Value[0], Value[1], 0);
end;

class operator TVector3.Explicit(Value: TVector3): TVec4;
begin
  Result := VectorMake(Value.Link);
end;

class operator TVector3.Explicit(Value: TVector3): TVec3;
begin
  Result := Value.Link;
end;

class operator TVector3.Explicit(Value: TVector3): T2DPoint;
begin
  Result[0] := Value.Link[0];
  Result[1] := Value.Link[1];
end;

{$ENDIF}


procedure TVector3.SetLocation(const fx, fy, fz: Single);
begin
  Link[0] := fx;
  Link[1] := fy;
  Link[2] := fz;
end;

function TVector3.Distance3D(const v2: TVector3): Single;
begin
  Result := sqrt(Sqr(v2.Link[0] - Link[0]) + Sqr(v2.Link[1] - Link[1]) + Sqr(v2.Link[2] - Link[2]));
end;

function TVector3.Distance2D(const v2: TVector3): Single;
begin
  Result := sqrt(Sqr(v2.Link[0] - Link[0]) + Sqr(v2.Link[1] - Link[1]));
end;

function TVector3.Lerp(const v2: TVector3; const t: Single): TVector3;
begin
  Result.Link[0] := Link[0] + (v2.Link[0] - Link[0]) * t;
  Result.Link[1] := Link[1] + (v2.Link[1] - Link[1]) * t;
  Result.Link[2] := Link[2] + (v2.Link[2] - Link[2]) * t;
end;

function TVector3.LerpDistance(const v2: TVector3; const d: Single): TVector3;
var
  k: Double;
begin
  k := d / sqrt((v2.Link[0] - Link[0]) * (v2.Link[0] - Link[0]) + (v2.Link[1] - Link[1]) * (v2.Link[1] - Link[1]) + (v2.Link[2] - Link[2]) * (v2.Link[2] - Link[2]));
  Result.Link[0] := Link[0] + k * (v2.Link[0] - Link[0]);
  Result.Link[1] := Link[1] + k * (v2.Link[1] - Link[1]);
  Result.Link[2] := Link[2] + k * (v2.Link[2] - Link[2]);
end;

function TVector3.Norm: Single;
begin
  Result := Link[0] * Link[0] + Link[1] * Link[1] + Link[2] * Link[2];
end;

function TVector3.Length: Single;
begin
  Result := sqrt(Norm);
end;

function TVector3.Normalize: TVector3;
var
  invLen: Single;
  vn    : Single;
begin
  vn := Norm;
  if vn = 0 then
      Result := Self
  else
    begin
      invLen := RSqrt(vn);
      Result.Link[0] := Link[0] * invLen;
      Result.Link[1] := Link[1] * invLen;
      Result.Link[2] := Link[2] * invLen;
    end;
end;

function TVector3.Cross(const v2: TVector3): TVector3;
begin
  Result.Link[0] := Link[1] * v2.Link[2] - Link[2] * v2.Link[1];
  Result.Link[1] := Link[2] * v2.Link[0] - Link[0] * v2.Link[2];
  Result.Link[2] := Link[0] * v2.Link[1] - Link[1] * v2.Link[0];
end;

function TVector3.Vec4(fw: Single): TVector4;
begin
  Result.SetLocation(Link[0], Link[1], Link[2], fw);
end;

function TVector3.Vec4: TVector4;
begin
  Result.SetLocation(Link[0], Link[1], Link[2], 0);
end;

{ TAABB }

procedure TAABB.Include(const p: TVector3);
begin
  if p.Link[0] < min[0] then
      min[0] := p.Link[0];
  if p.Link[0] > max[0] then
      max[0] := p.Link[0];

  if p.Link[1] < min[1] then
      min[1] := p.Link[1];
  if p.Link[1] > max[1] then
      max[1] := p.Link[1];

  if p.Link[2] < min[2] then
      min[2] := p.Link[2];
  if p.Link[2] > max[2] then
      max[2] := p.Link[2];
end;

procedure TAABB.FromSweep(const Start, Dest: TVector3; const Radius: Single);
begin
  if Start.Link[0] < Dest.Link[0] then
    begin
      min[0] := Start.Link[0] - Radius;
      max[0] := Dest.Link[0] + Radius;
    end
  else
    begin
      min[0] := Dest.Link[0] - Radius;
      max[0] := Start.Link[0] + Radius;
    end;

  if Start.Link[1] < Dest.Link[1] then
    begin
      min[1] := Start.Link[1] - Radius;
      max[1] := Dest.Link[1] + Radius;
    end
  else
    begin
      min[1] := Dest.Link[1] - Radius;
      max[1] := Start.Link[1] + Radius;
    end;

  if Start.Link[2] < Dest.Link[2] then
    begin
      min[2] := Start.Link[2] - Radius;
      max[2] := Dest.Link[2] + Radius;
    end
  else
    begin
      min[2] := Dest.Link[2] - Radius;
      max[2] := Start.Link[2] + Radius;
    end;
end;

function TAABB.Intersection(const aabb2: TAABB): TAABB;
var
  I: Integer;
begin
  for I := 0 to 2 do
    begin
      Result.min[I] := MaxFloat(min[I], aabb2.min[I]);
      Result.max[I] := MinFloat(max[I], aabb2.max[I]);
    end;
end;

procedure TAABB.Offset(const Delta: TVector3);
begin
  AddVector(min, Delta.Link);
  AddVector(max, Delta.Link);
end;

function TAABB.PointIn(const p: TVector3): Boolean;
begin
  Result := (p.Link[0] <= max[0]) and (p.Link[0] >= min[0])
    and (p.Link[1] <= max[1]) and (p.Link[1] >= min[1])
    and (p.Link[2] <= max[2]) and (p.Link[2] >= min[2]);
end;

{$IFDEF FPC}


operator := (const s: Single)r: TMatrix4;
var
  I, j: Integer;
begin
  for I := 0 to 3 do
    for j := 0 to 3 do
        r.Link[I, j] := s;
end;

operator := (const s: TMatrix4)r: TMat4;
begin
  r := s.Link;
end;

operator := (const s: TMat4)r: TMatrix4;
begin
  r.Link := s;
end;

operator := (const s: Single)r: TVector4;
begin
  r.Link[0] := s;
  r.Link[1] := s;
  r.Link[2] := s;
  r.Link[3] := s;
end;

operator := (const s: TVector4)r: TVec4;
begin
  r := s.Link;
end;

operator := (const s: TVec4)r: TVector4;
begin
  r.Link := s;
end;

operator := (const s: Single)r: TVector3;
begin
  r.Link[0] := s;
  r.Link[1] := s;
  r.Link[2] := s;
end;

operator := (const s: TVector3)r: TVec3;
begin
  r := s.Link;
end;

operator := (const s: TVec3)r: TVector3;
begin
  r.Link := s;
end;

operator * (const a: TMatrix4; const b: TMatrix4): TMatrix4;
begin
  Result := GeometryLib.MatrixMultiply(a, b);
end;

operator * (const a: Single; const b: TVector4): TVector4;
begin
  Result.Link[0] := a * b.Link[0];
  Result.Link[1] := a * b.Link[1];
  Result.Link[2] := a * b.Link[2];
  Result.Link[3] := a * b.Link[3];
end;

operator * (const a: TVector4; const b: Single): TVector4;
begin
  Result.Link[0] := a.Link[0] * b;
  Result.Link[1] := a.Link[1] * b;
  Result.Link[2] := a.Link[2] * b;
  Result.Link[3] := a.Link[3] * b;
end;

operator * (const a: TVector4; const b: TVector4): TVector4;
begin
  Result.Link[0] := a.Link[0] * b.Link[0];
  Result.Link[1] := a.Link[1] * b.Link[1];
  Result.Link[2] := a.Link[2] * b.Link[2];
  Result.Link[3] := a.Link[3] * b.Link[3];
end;

operator * (const a: Single; const b: TVector3): TVector3;
begin
  Result.Link[0] := a * b.Link[0];
  Result.Link[1] := a * b.Link[1];
  Result.Link[2] := a * b.Link[2];
end;

operator * (const a: TVector3; const b: Single): TVector3;
begin
  Result.Link[0] := a.Link[0] * b;
  Result.Link[1] := a.Link[1] * b;
  Result.Link[2] := a.Link[2] * b;
end;

operator * (const a: TVector3; const b: TVector3): TVector3;
begin
  Result.Link[0] := a.Link[0] * b.Link[0];
  Result.Link[1] := a.Link[1] * b.Link[1];
  Result.Link[2] := a.Link[2] * b.Link[2];
end;

operator / (const a: Single; const b: TVector4): TVector4;
begin
  Result.Link[0] := a / b.Link[0];
  Result.Link[1] := a / b.Link[1];
  Result.Link[2] := a / b.Link[2];
  Result.Link[3] := a / b.Link[3];
end;

operator / (const a: TVector4; const b: Single): TVector4;
begin
  Result.Link[0] := a.Link[0] / b;
  Result.Link[1] := a.Link[1] / b;
  Result.Link[2] := a.Link[2] / b;
  Result.Link[3] := a.Link[3] / b;
end;

operator / (const a: TVector4; const b: TVector4): TVector4;
begin
  Result.Link[0] := a.Link[0] / b.Link[0];
  Result.Link[1] := a.Link[1] / b.Link[1];
  Result.Link[2] := a.Link[2] / b.Link[2];
  Result.Link[3] := a.Link[3] / b.Link[3];
end;

operator / (const a: Single; const b: TVector3): TVector3;
begin
  Result.Link[0] := a / b.Link[0];
  Result.Link[1] := a / b.Link[1];
  Result.Link[2] := a / b.Link[2];
end;

operator / (const a: TVector3; const b: Single): TVector3;
begin
  Result.Link[0] := a.Link[0] / b;
  Result.Link[1] := a.Link[1] / b;
  Result.Link[2] := a.Link[2] / b;
end;

operator / (const a: TVector3; const b: TVector3): TVector3;
begin
  Result.Link[0] := a.Link[0] / b.Link[0];
  Result.Link[1] := a.Link[1] / b.Link[1];
  Result.Link[2] := a.Link[2] / b.Link[2];
end;

operator + (const a: Single; const b: TVector4): TVector4;
begin
  Result.Link[0] := a + b.Link[0];
  Result.Link[1] := a + b.Link[1];
  Result.Link[2] := a + b.Link[2];
  Result.Link[3] := a + b.Link[3];
end;

operator + (const a: TVector4; const b: Single): TVector4;
begin
  Result.Link[0] := a.Link[0] + b;
  Result.Link[1] := a.Link[1] + b;
  Result.Link[2] := a.Link[2] + b;
  Result.Link[3] := a.Link[3] + b;
end;

operator + (const a: TVector4; const b: TVector4): TVector4;
begin
  Result.Link[0] := a.Link[0] + b.Link[0];
  Result.Link[1] := a.Link[1] + b.Link[1];
  Result.Link[2] := a.Link[2] + b.Link[2];
  Result.Link[3] := a.Link[3] + b.Link[3];
end;

operator + (const a: Single; const b: TVector3): TVector3;
begin
  Result.Link[0] := a + b.Link[0];
  Result.Link[1] := a + b.Link[1];
  Result.Link[2] := a + b.Link[2];
end;

operator + (const a: TVector3; const b: Single): TVector3;
begin
  Result.Link[0] := a.Link[0] + b;
  Result.Link[1] := a.Link[1] + b;
  Result.Link[2] := a.Link[2] + b;
end;

operator + (const a: TVector3; const b: TVector3): TVector3;
begin
  Result.Link[0] := a.Link[0] + b.Link[0];
  Result.Link[1] := a.Link[1] + b.Link[1];
  Result.Link[2] := a.Link[2] + b.Link[2];
end;

operator - (const a: Single; const b: TVector4): TVector4;
begin
  Result.Link[0] := a - b.Link[0];
  Result.Link[1] := a - b.Link[1];
  Result.Link[2] := a - b.Link[2];
  Result.Link[3] := a - b.Link[3];
end;

operator - (const a: TVector4; const b: Single): TVector4;
begin
  Result.Link[0] := a.Link[0] - b;
  Result.Link[1] := a.Link[1] - b;
  Result.Link[2] := a.Link[2] - b;
  Result.Link[3] := a.Link[3] - b;
end;

operator - (const a: TVector4; const b: TVector4): TVector4;
begin
  Result.Link[0] := a.Link[0] - b.Link[0];
  Result.Link[1] := a.Link[1] - b.Link[1];
  Result.Link[2] := a.Link[2] - b.Link[2];
  Result.Link[3] := a.Link[3] - b.Link[3];
end;

operator - (const a: Single; const b: TVector3): TVector3;
begin
  Result.Link[0] := a - b.Link[0];
  Result.Link[1] := a - b.Link[1];
  Result.Link[2] := a - b.Link[2];
end;

operator - (const a: TVector3; const b: Single): TVector3;
begin
  Result.Link[0] := a.Link[0] - b;
  Result.Link[1] := a.Link[1] - b;
  Result.Link[2] := a.Link[2] - b;
end;

operator - (const a: TVector3; const b: TVector3): TVector3;
begin
  Result.Link[0] := a.Link[0] - b.Link[0];
  Result.Link[1] := a.Link[1] - b.Link[1];
  Result.Link[2] := a.Link[2] - b.Link[2];
end;

{$ENDIF}


end.
