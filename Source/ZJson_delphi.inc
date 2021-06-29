{ ****************************************************************************** }
{ * json object library for delphi/objfpc                                      * }
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

procedure TZ_JsonArray.Clear;
begin
  FInstance.Clear;
end;

procedure TZ_JsonArray.Delete(Index: Integer);
begin
  FInstance.Delete(index);
end;

procedure TZ_JsonArray.Add(const v_: string);
begin
  FInstance.Add(v_);
end;

procedure TZ_JsonArray.Add(const v_: TPascalString);
begin
  FInstance.Add(v_.Text);
end;

procedure TZ_JsonArray.Add(const v_: Integer);
begin
  FInstance.Add(v_);
end;

procedure TZ_JsonArray.Add(const v_: Int64);
begin
  FInstance.Add(v_);
end;

procedure TZ_JsonArray.Add(const v_: UInt64);
begin
  FInstance.Add(v_);
end;

procedure TZ_JsonArray.AddF(const v_: Double);
begin
  FInstance.Add(v_);
end;

procedure TZ_JsonArray.Add(const v_: TDateTime);
begin
  FInstance.Add(umlDateTimeToStr(v_).Text);
end;

procedure TZ_JsonArray.Add(const v_: Boolean);
begin
  FInstance.Add(v_);
end;

function TZ_JsonArray.AddArray: TZ_JsonArray;
begin
  Result := TZ_JsonArray.Create(self);
  Result.FInstance := FInstance.AddArray;
end;

function TZ_JsonArray.AddObject: TZ_JsonObject;
begin
  Result := TZ_JsonObject.Create(self);
  Result.FInstance := FInstance.AddObject;
end;

procedure TZ_JsonArray.Insert(Index: Integer; const v_: string);
begin
  FInstance.Insert(index, v_);
end;

procedure TZ_JsonArray.Insert(Index: Integer; const v_: Integer);
begin
  FInstance.Insert(index, v_);
end;

procedure TZ_JsonArray.Insert(Index: Integer; const v_: Int64);
begin
  FInstance.Insert(index, v_);
end;

procedure TZ_JsonArray.Insert(Index: Integer; const v_: UInt64);
begin
  FInstance.Insert(index, v_);
end;

procedure TZ_JsonArray.Insert(Index: Integer; const v_: Double);
begin
  FInstance.Insert(index, v_);
end;

procedure TZ_JsonArray.Insert(Index: Integer; const v_: TDateTime);
begin
  FInstance.Insert(index, umlDateTimeToStr(v_).Text);
end;

procedure TZ_JsonArray.Insert(Index: Integer; const v_: Boolean);
begin
  FInstance.Insert(index, v_);
end;

function TZ_JsonArray.InsertArray(Index: Integer): TZ_JsonArray;
begin
  Result := TZ_JsonArray.Create(self);
  Result.FInstance := FInstance.InsertArray(index);
end;

function TZ_JsonArray.InsertObject(Index: Integer): TZ_JsonObject;
begin
  Result := TZ_JsonObject.Create(self);
  Result.FInstance := FInstance.InsertObject(index);
end;

function TZ_JsonArray.GetString(Index: Integer): string;
begin
  Result := FInstance.S[index];
end;

procedure TZ_JsonArray.SetString(Index: Integer; const Value: string);
begin
  FInstance.S[index] := Value;
end;

function TZ_JsonArray.GetInt(Index: Integer): Integer;
begin
  Result := FInstance.I[index];
end;

procedure TZ_JsonArray.SetInt(Index: Integer; const Value: Integer);
begin
  FInstance.I[index] := Value;
end;

function TZ_JsonArray.GetLong(Index: Integer): Int64;
begin
  Result := FInstance.I64[index];
end;

procedure TZ_JsonArray.SetLong(Index: Integer; const Value: Int64);
begin
  FInstance.I64[index] := Value;
end;

function TZ_JsonArray.GetULong(Index: Integer): UInt64;
begin
  Result := FInstance.U64[index];
end;

procedure TZ_JsonArray.SetULong(Index: Integer; const Value: UInt64);
begin
  FInstance.U64[index] := Value;
end;

function TZ_JsonArray.GetFloat(Index: Integer): Double;
begin
  Result := FInstance.F[index];
end;

procedure TZ_JsonArray.SetFloat(Index: Integer; const Value: Double);
begin
  FInstance.F[index] := Value;
end;

function TZ_JsonArray.GetDateTime(Index: Integer): TDateTime;
begin
  Result := umlStrToDateTime(FInstance.S[index]);
end;

procedure TZ_JsonArray.SetDateTime(Index: Integer; const Value: TDateTime);
begin
  FInstance.S[index] := umlDateTimeToStr(Value).Text;
end;

function TZ_JsonArray.GetBool(Index: Integer): Boolean;
begin
  Result := FInstance.B[index];
end;

procedure TZ_JsonArray.SetBool(Index: Integer; const Value: Boolean);
begin
  FInstance.B[index] := Value;
end;

function TZ_JsonArray.GetArray(Index: Integer): TZ_JsonArray;
var
  arry: TZ_Instance_JsonArray;
  j: Integer;
begin
  arry := FInstance.A[index];
  for j := FList.Count - 1 downto 0 do
    if (FList[j] is TZ_JsonArray) and (TZ_JsonArray(FList[j]).FInstance = arry) then
      begin
        Result := TZ_JsonArray(FList[j]);
        exit;
      end;
  Result := TZ_JsonArray.Create(self);
  Result.FInstance := arry;
end;

function TZ_JsonArray.GetObject(Index: Integer): TZ_JsonObject;
var
  Obj_: TZ_Instance_JsonObject;
  j: Integer;
begin
  Obj_ := FInstance.O[Index];
  for j := FList.Count - 1 downto 0 do
    if (FList[j] is TZ_JsonObject) and (TZ_JsonObject(FList[j]).FInstance = Obj_) then
      begin
        Result := TZ_JsonObject(FList[j]);
        exit;
      end;
  Result := TZ_JsonObject.Create(self);
  Result.FInstance := Obj_;
end;

function TZ_JsonArray.GetCount: Integer;
begin
  Result := FInstance.Count;
end;

procedure TZ_JsonObject.Clear;
begin
  FInstance.Clear;
end;

function TZ_JsonObject.IndexOf(const Name: string): Integer;
begin
  Result := FInstance.IndexOf(Name);
end;

function TZ_JsonObject.GetString(const Name: string): string;
begin
  Result := FInstance.S[Name];
end;

procedure TZ_JsonObject.SetString(const Name, Value: string);
begin
  FInstance.S[Name] := Value;
end;

function TZ_JsonObject.GetInt(const Name: string): Integer;
begin
  Result := FInstance.I[Name];
end;

procedure TZ_JsonObject.SetInt(const Name: string; const Value: Integer);
begin
  FInstance.I[Name] := Value;
end;

function TZ_JsonObject.GetLong(const Name: string): Int64;
begin
  Result := FInstance.I64[Name];
end;

procedure TZ_JsonObject.SetLong(const Name: string; const Value: Int64);
begin
  FInstance.I64[Name] := Value;
end;

function TZ_JsonObject.GetULong(const Name: string): UInt64;
begin
  Result := FInstance.U[Name];
end;

procedure TZ_JsonObject.SetULong(const Name: string; const Value: UInt64);
begin
  FInstance.U[Name] := Value;
end;

function TZ_JsonObject.GetFloat(const Name: string): Double;
begin
  Result := FInstance.F[Name];
end;

procedure TZ_JsonObject.SetFloat(const Name: string; const Value: Double);
begin
  FInstance.F[Name] := Value;
end;

function TZ_JsonObject.GetDateTime(const Name: string): TDateTime;
begin
  Result := umlStrToDateTime(FInstance.S[Name]);
end;

procedure TZ_JsonObject.SetDateTime(const Name: string; const Value: TDateTime);
begin
  FInstance.S[Name] := umlDateTimeToStr(Value).Text;
end;

function TZ_JsonObject.GetBool(const Name: string): Boolean;
begin
  Result := FInstance.B[Name];
end;

procedure TZ_JsonObject.SetBool(const Name: string; const Value: Boolean);
begin
  FInstance.B[Name] := Value;
end;

function TZ_JsonObject.GetArray(const Name: string): TZ_JsonArray;
var
  arry: TZ_Instance_JsonArray;
  j: Integer;
begin
  arry := FInstance.A[Name];
  for j := FList.Count - 1 downto 0 do
    if (FList[j] is TZ_JsonArray) and (TZ_JsonArray(FList[j]).FInstance = arry) then
      begin
        Result := TZ_JsonArray(FList[j]);
        exit;
      end;
  Result := TZ_JsonArray.Create(self);
  Result.FInstance := arry;
end;

function TZ_JsonObject.GetObject(const Name: string): TZ_JsonObject;
var
  Obj_: TZ_Instance_JsonObject;
  j: Integer;
begin
  Obj_ := FInstance.O[Name];
  for j := FList.Count - 1 downto 0 do
    if (FList[j] is TZ_JsonObject) and (TZ_JsonObject(FList[j]).FInstance = Obj_) then
      begin
        Result := TZ_JsonObject(FList[j]);
        exit;
      end;
  Result := TZ_JsonObject.Create(self);
  Result.FInstance := Obj_;
end;

function TZ_JsonObject.GetName(Index: Integer): string;
begin
  Result := FInstance.Names[Index];
end;

function TZ_JsonObject.GetCount: Integer;
begin
  Result := FInstance.Count;
end;

procedure TZ_JsonObject.SaveToStream(stream: TCoreClassStream; Formated_: Boolean);
begin
  FInstance.SaveToStream(stream, not Formated_, TEncoding.UTF8, True);
end;

procedure TZ_JsonObject.LoadFromStream(stream: TCoreClassStream);
begin
  FInstance.LoadFromStream(stream, TEncoding.UTF8, True);
end;
