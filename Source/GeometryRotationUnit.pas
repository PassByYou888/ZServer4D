{ ****************************************************************************** }
{ * geometry Rotation imp writen by QQ 600585@qq.com                           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }

unit GeometryRotationUnit;

{$I zDefine.inc}

{
  create by,passbyyou
}

interface

uses
  GeometryLib;

procedure NormalizeMat(var m: TMatrix);
procedure MulRMat(var m: TMatrix; const ScaleXYZ: TAffineVector);

procedure DecodeOrderAngle(lvec, uvec, dvec: TAffineVector; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure DecodeOrderAngle(lvec, uvec, dvec: TVector; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure DecodeOrderAngle(uvec, dvec: TVector; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure DecodeOrderAngle(m: TMatrix; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function ComputePitch(uvec, dvec: TAffineVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ComputePitch(uvec, dvec: TVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ComputePitch(m: TMatrix): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function ComputeTurn(uvec, dvec: TAffineVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ComputeTurn(uvec, dvec: TVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ComputeTurn(m: TMatrix): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function ComputeRoll(uvec, dvec: TAffineVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ComputeRoll(uvec, dvec: TVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ComputeRoll(m: TMatrix): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure StepPitch(var uvec, dvec: TVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure StepPitch(uvec, dvec: TVector; angle: Single; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure StepPitch(m: TMatrix; angle: Single; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure StepPitch(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure StepTurn(var uvec, dvec: TVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure StepTurn(uvec, dvec: TVector; angle: Single; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure StepTurn(m: TMatrix; angle: Single; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure StepTurn(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure StepRoll(var uvec, dvec: TVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure StepRoll(uvec, dvec: TVector; angle: Single; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure StepRoll(m: TMatrix; angle: Single; var p, t, r: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure StepRoll(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SetPitch(var uvec, dvec: TAffineVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SetPitch(var uvec, dvec: TVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SetPitch(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SetTurn(var uvec, dvec: TAffineVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SetTurn(var uvec, dvec: TVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SetTurn(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SetRoll(var uvec, dvec: TAffineVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SetRoll(var uvec, dvec: TVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SetRoll(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}


implementation

procedure NormalizeMat(var m: TMatrix);
begin
  NormalizeVector(m[0]);
  NormalizeVector(m[1]);
  NormalizeVector(m[2]);
end;

procedure MulRMat(var m: TMatrix; const ScaleXYZ: TAffineVector);
begin
  m[0][0] := m[0][0] * ScaleXYZ[0];
  m[0][0] := m[0][1] * ScaleXYZ[0];
  m[0][0] := m[0][2] * ScaleXYZ[0];
  m[0][0] := m[0][3] * ScaleXYZ[0];

  m[1][0] := m[1][0] * ScaleXYZ[1];
  m[1][0] := m[1][1] * ScaleXYZ[1];
  m[1][0] := m[1][2] * ScaleXYZ[1];
  m[1][0] := m[1][3] * ScaleXYZ[1];

  m[2][0] := m[2][0] * ScaleXYZ[2];
  m[2][0] := m[2][1] * ScaleXYZ[2];
  m[2][0] := m[2][2] * ScaleXYZ[2];
  m[2][0] := m[2][3] * ScaleXYZ[2];
end;

procedure DecodeOrderAngle(lvec, uvec, dvec: TAffineVector; var p, t, r: Single);
var
  sinTurn, cosTurn, sinPitch, cosPitch, sinRoll, cosRoll: Single;
begin
  sinTurn := -dvec[0];
  cosTurn := sqrt(1 - sinTurn * sinTurn);

  if (abs(cosTurn) > EPSILON) then
    begin
      sinPitch := dvec[1] / cosTurn;
      cosPitch := dvec[2] / cosTurn;
      sinRoll := uvec[0] / cosTurn;
      cosRoll := lvec[0] / cosTurn;
    end
  else
    begin
      sinPitch := -uvec[2];
      cosPitch := uvec[1];
      sinRoll := 0;
      cosRoll := 1;
    end;

  p := RadToDeg(ArcTan2(sinPitch, cosPitch));
  t := -RadToDeg(ArcTan2(sinTurn, cosTurn));
  r := -RadToDeg(ArcTan2(sinRoll, cosRoll));
end;

procedure DecodeOrderAngle(lvec, uvec, dvec: TVector; var p, t, r: Single);
begin
  DecodeOrderAngle(PAffineVector(@lvec)^, PAffineVector(@uvec)^, PAffineVector(@dvec)^, p, t, r);
end;

procedure DecodeOrderAngle(uvec, dvec: TVector; var p, t, r: Single);
var
  lvec: TVector;
begin
  lvec := VectorCrossProduct(uvec, dvec);
  NormalizeVector(lvec);
  DecodeOrderAngle(lvec, uvec, dvec, p, t, r);
end;

procedure DecodeOrderAngle(m: TMatrix; var p, t, r: Single);
begin
  DecodeOrderAngle(m[0], m[1], m[2], p, t, r);
end;

function ComputePitch(uvec, dvec: TAffineVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  sinTurn, cosTurn, sinPitch, cosPitch: Single;
begin
  sinTurn := -dvec[0];
  cosTurn := sqrt(1 - sinTurn * sinTurn);
  sinPitch := dvec[1] / cosTurn;
  cosPitch := dvec[2] / cosTurn;
  Result := RadToDeg(ArcTan2(sinPitch, cosPitch));
end;

function ComputePitch(uvec, dvec: TVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := ComputePitch(PAffineVector(@uvec)^, PAffineVector(@dvec)^);
end;

function ComputePitch(m: TMatrix): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := ComputePitch(m[1], m[2]);
end;

function ComputeTurn(uvec, dvec: TAffineVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  sinTurn, cosTurn: Single;
begin
  sinTurn := -dvec[0];
  cosTurn := sqrt(1 - sinTurn * sinTurn);

  Result := -RadToDeg(ArcTan2(sinTurn, cosTurn));
end;

function ComputeTurn(uvec, dvec: TVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := ComputeTurn(PAffineVector(@uvec)^, PAffineVector(@dvec)^);
end;

function ComputeTurn(m: TMatrix): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := ComputeTurn(m[1], m[2]);
end;

function ComputeRoll(uvec, dvec: TAffineVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  lvec                              : TAffineVector;
  sinTurn, cosTurn, sinRoll, cosRoll: Single;
begin
  lvec := VectorCrossProduct(uvec, dvec);

  sinTurn := -dvec[0];
  cosTurn := sqrt(1 - sinTurn * sinTurn);

  sinRoll := uvec[0] / cosTurn;
  cosRoll := lvec[0] / cosTurn;

  Result := -RadToDeg(ArcTan2(sinRoll, cosRoll));
end;

function ComputeRoll(uvec, dvec: TVector): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := ComputeRoll(PAffineVector(@uvec)^, PAffineVector(@dvec)^);
end;

function ComputeRoll(m: TMatrix): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := ComputeRoll(m[1], m[2]);
end;

procedure StepPitch(var uvec, dvec: TVector; angle: Single);
var
  rvec: TVector;
begin
  angle := -DegToRad(angle);
  rvec := VectorCrossProduct(dvec, uvec);

  RotateVector(uvec, rvec, angle);
  NormalizeVector(uvec);

  RotateVector(dvec, rvec, angle);
  NormalizeVector(dvec);
end;

procedure StepPitch(uvec, dvec: TVector; angle: Single; var p, t, r: Single);
begin
  StepPitch(uvec, dvec, angle);
  DecodeOrderAngle(uvec, dvec, p, t, r);
end;

procedure StepPitch(m: TMatrix; angle: Single; var p, t, r: Single);
begin
  StepPitch(m, XYZVector, angle);
  DecodeOrderAngle(m, p, t, r);
end;

procedure StepPitch(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single);
begin
  NormalizeMat(m);
  StepPitch(m[1], m[2], angle);
  // change left vector
  m[0] := VectorCrossProduct(m[1], m[2]);
  MulRMat(m, ScaleXYZ);
end;

procedure StepTurn(var uvec, dvec: TVector; angle: Single);
var
  uvec2: TAffineVector;
begin
  angle := -DegToRad(angle);
  uvec2 := AffineVectorMake(uvec);

  RotateVector(uvec, uvec2, angle);
  NormalizeVector(uvec);

  RotateVector(dvec, uvec2, angle);
  NormalizeVector(dvec);
end;

procedure StepTurn(uvec, dvec: TVector; angle: Single; var p, t, r: Single);
begin
  StepTurn(uvec, dvec, angle);
  DecodeOrderAngle(uvec, dvec, p, t, r);
end;

procedure StepTurn(m: TMatrix; angle: Single; var p, t, r: Single);
begin
  StepTurn(m, XYZVector, angle);
  DecodeOrderAngle(m, p, t, r);
end;

procedure StepTurn(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single);
begin
  NormalizeMat(m);
  StepTurn(m[1], m[2], angle);
  // change left vector
  m[0] := VectorCrossProduct(m[1], m[2]);
  MulRMat(m, ScaleXYZ);
end;

procedure StepRoll(var uvec, dvec: TVector; angle: Single);
var
  dvec2: TVector;
begin
  angle := -DegToRad(angle);
  dvec2 := dvec;

  RotateVector(uvec, dvec2, angle);
  NormalizeVector(uvec);

  RotateVector(dvec, dvec2, angle);
  NormalizeVector(dvec);
end;

procedure StepRoll(uvec, dvec: TVector; angle: Single; var p, t, r: Single);
begin
  StepRoll(uvec, dvec, angle);
  DecodeOrderAngle(uvec, dvec, p, t, r);
end;

procedure StepRoll(m: TMatrix; angle: Single; var p, t, r: Single);
begin
  StepRoll(m, XYZVector, angle);
  DecodeOrderAngle(m, p, t, r);
end;

procedure StepRoll(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single);
begin
  NormalizeMat(m);
  StepRoll(m[1], m[2], angle);
  // change left vector
  m[0] := VectorCrossProduct(m[1], m[2]);
  MulRMat(m, ScaleXYZ);
end;

procedure SetPitch(var uvec, dvec: TAffineVector; angle: Single);
var
  rvec     : TAffineVector;
  diff     : Single;
  rotMatrix: TMatrix;
begin
  rvec := VectorCrossProduct(dvec, uvec);
  diff := DegToRad(ComputePitch(uvec, dvec) - angle);
  rotMatrix := CreateRotationMatrix(rvec, diff);

  uvec := VectorTransform(uvec, rotMatrix);
  NormalizeVector(uvec);

  dvec := VectorTransform(dvec, rotMatrix);
  NormalizeVector(dvec);
end;

procedure SetPitch(var uvec, dvec: TVector; angle: Single);
begin
  SetPitch(PAffineVector(@uvec)^, PAffineVector(@dvec)^, angle);
end;

procedure SetPitch(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single);
begin
  NormalizeMat(m);
  SetPitch(m[1], m[2], angle);
  // change left vector
  m[0] := VectorCrossProduct(m[1], m[2]);
  MulRMat(m, ScaleXYZ);
end;

procedure SetTurn(var uvec, dvec: TAffineVector; angle: Single);
var
  rvec     : TAffineVector;
  diff     : Single;
  rotMatrix: TMatrix;
begin
  rvec := VectorCrossProduct(dvec, uvec);
  diff := DegToRad(ComputeTurn(uvec, dvec) - angle);

  rotMatrix := CreateRotationMatrix(uvec, diff);

  uvec := VectorTransform(uvec, rotMatrix);
  NormalizeVector(uvec);

  dvec := VectorTransform(dvec, rotMatrix);
  NormalizeVector(dvec);
end;

procedure SetTurn(var uvec, dvec: TVector; angle: Single);
begin
  SetTurn(PAffineVector(@uvec)^, PAffineVector(@dvec)^, angle);
end;

procedure SetTurn(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single);
begin
  NormalizeMat(m);
  SetTurn(m[1], m[2], angle);
  // change left vector
  m[0] := VectorCrossProduct(m[1], m[2]);
  MulRMat(m, ScaleXYZ);
end;

procedure SetRoll(var uvec, dvec: TAffineVector; angle: Single);
var
  rvec     : TAffineVector;
  diff     : Single;
  rotMatrix: TMatrix;
begin
  rvec := VectorCrossProduct(dvec, uvec);
  diff := DegToRad(ComputeRoll(uvec, dvec) - angle);

  rotMatrix := CreateRotationMatrix(dvec, diff);

  uvec := VectorTransform(uvec, rotMatrix);
  NormalizeVector(uvec);

  dvec := VectorTransform(dvec, rotMatrix);
  NormalizeVector(dvec);
end;

procedure SetRoll(var uvec, dvec: TVector; angle: Single);
begin
  SetRoll(PAffineVector(@uvec)^, PAffineVector(@dvec)^, angle);
end;

procedure SetRoll(var m: TMatrix; ScaleXYZ: TAffineVector; angle: Single);
begin
  NormalizeMat(m);
  SetRoll(m[1], m[2], angle);
  // change left vector
  m[0] := VectorCrossProduct(m[1], m[2]);
  MulRMat(m, ScaleXYZ);
end;

end.
