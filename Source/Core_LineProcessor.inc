{$IFDEF RangeCheck}{$R-}{$ENDIF}
{$IFDEF OverflowCheck}{$Q-}{$ENDIF}


procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.VertLine(X, y1, y2: NativeInt);
var
  i, nh, nl: NativeInt;
  p: PT_;
begin
  if (X < 0) or (X >= Width_) then
      Exit;

  if y1 < 0 then
      y1 := 0;
  if y1 >= Height_ then
      y1 := Height_ - 1;

  if y2 < 0 then
      y2 := 0;
  if y2 >= Height_ then
      y2 := Height_ - 1;

  if y2 < y1 then
      Swap(y1, y2);

  p := @Data_^[X + y1 * Width_];
  i := y2 - y1 + 1;
  nh := i shr 2;
  nl := i and $03;
  for i := 0 to nh - 1 do
    begin
      Process(p, Value_);
      inc(p, Width_);
      Process(p, Value_);
      inc(p, Width_);
      Process(p, Value_);
      inc(p, Width_);
      Process(p, Value_);
      inc(p, Width_);
    end;
  for i := 0 to nl - 1 do
    begin
      Process(p, Value_);
      inc(p, Width_);
    end;
end;

procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.HorzLine(x1, Y, x2: NativeInt);
var
  i: NativeInt;
  p: PT_;
begin
  if (Y < 0) or (Y >= Height_) then
      Exit;

  if x1 < 0 then
      x1 := 0;
  if x1 >= Width_ then
      x1 := Width_ - 1;

  if x2 < 0 then
      x2 := 0;
  if x2 >= Width_ then
      x2 := Width_ - 1;

  if x1 > x2 then
      Swap(x1, x2);

  p := @Data_^[x1 + Y * Width_];

  for i := x1 to x2 do
    begin
      Process(p, Value_);
      inc(p);
    end;
end;

constructor TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create(data: Pointer; width, height: NativeInt; Value: T_; L: Boolean);
begin
  inherited Create;
  Data_ := PTArry_(data);
  width := Width_;
  height := Height_;
  Value_ := Value;
  L_ := L;
end;

procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.Line(x1, y1, x2, y2: NativeInt);
var
  dy, dx, SY, SX, i, Delta: NativeInt;
  pi, pl: NativeInt;
begin
  if (x1 = x2) and (y1 = y2) then
    begin
      Process(@Data_^[x1 + y1 * Width_], Value_);
      Exit;
    end;

  dx := x2 - x1;
  dy := y2 - y1;

  if dx > 0 then
      SX := 1
  else if dx < 0 then
    begin
      dx := -dx;
      SX := -1;
    end
  else // Dx = 0
    begin
      if dy > 0 then
          VertLine(x1, y1, y2 - 1)
      else if dy < 0 then
          VertLine(x1, y2 + 1, y1);
      if L_ then
          Process(@Data_^[x2 + y2 * Width_], Value_);
      Exit;
    end;

  if dy > 0 then
      SY := 1
  else if dy < 0 then
    begin
      dy := -dy;
      SY := -1;
    end
  else // Dy = 0
    begin
      if x2 > x1 then
          HorzLine(x1, y1, x2 - 1)
      else
          HorzLine(x2 + 1, y1, x1);
      if L_ then
          Process(@Data_^[x2 + y2 * Width_], Value_);
      Exit;
    end;

  pi := x1 + y1 * Width_;
  SY := SY * Width_;
  pl := Width_ * Height_;

  if dx > dy then
    begin
      Delta := dx shr 1;
      for i := 0 to dx - 1 do
        begin
          if (pi >= 0) and (pi < pl) then
              Process(@Data_^[pi], Value_);

          inc(pi, SX);
          inc(Delta, dy);
          if Delta >= dx then
            begin
              inc(pi, SY);
              dec(Delta, dx);
            end;
        end;
    end
  else // Dx < Dy
    begin
      Delta := dy shr 1;
      for i := 0 to dy - 1 do
        begin
          if (pi >= 0) and (pi < pl) then
              Process(@Data_^[pi], Value_);

          inc(pi, SY);
          inc(Delta, dx);
          if Delta >= dy then
            begin
              inc(pi, SX);
              dec(Delta, dy);
            end;
        end;
    end;
  if (L_) and (pi >= 0) and (pi < pl) then
      Process(@Data_^[pi], Value_);
end;

procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.FillBox(x1, y1, x2, y2: NativeInt);
var
  i: Integer;
begin
  if y1 > y2 then
      Swap(y1, y2);
  for i := y1 to y2 do
      HorzLine(x1, i, x2);
end;

procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.Process(vp: PT_; v: T_);
begin
  vp^ := v;
end;
{$IFDEF RangeCheck}{$R+}{$ENDIF}
{$IFDEF OverflowCheck}{$Q+}{$ENDIF}
