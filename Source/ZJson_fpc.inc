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
  Result.FInstance := TZ_Instance_JsonArray.Create;
  FInstance.Add(Result.FInstance);
end;

function TZ_JsonArray.AddObject: TZ_JsonObject;
begin
  Result := TZ_JsonObject.Create(self);
  Result.FInstance := TZ_Instance_JsonObject.Create;
  FInstance.Add(Result.FInstance);
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
  Result.FInstance := TZ_Instance_JsonArray.Create;
  FInstance.Insert(index, Result.FInstance);
end;

function TZ_JsonArray.InsertObject(Index: Integer): TZ_JsonObject;
begin
  Result := TZ_JsonObject.Create(self);
  Result.FInstance := TZ_Instance_JsonObject.Create;
  FInstance.Insert(index, Result.FInstance);
end;

function TZ_JsonArray.GetString(Index: Integer): string;
begin
  Result := FInstance.Strings[index];
end;

procedure TZ_JsonArray.SetString(Index: Integer; const Value: string);
begin
  FInstance.Strings[index] := Value;
end;

function TZ_JsonArray.GetInt(Index: Integer): Integer;
begin
  Result := FInstance.Integers[index];
end;

procedure TZ_JsonArray.SetInt(Index: Integer; const Value: Integer);
begin
  FInstance.Integers[index] := Value;
end;

function TZ_JsonArray.GetLong(Index: Integer): Int64;
begin
  Result := FInstance.Int64s[index];
end;

procedure TZ_JsonArray.SetLong(Index: Integer; const Value: Int64);
begin
  FInstance.Int64s[index] := Value;
end;

function TZ_JsonArray.GetULong(Index: Integer): UInt64;
begin
  Result := FInstance.QWords[index];
end;

procedure TZ_JsonArray.SetULong(Index: Integer; const Value: UInt64);
begin
  FInstance.QWords[index] := Value;
end;

function TZ_JsonArray.GetFloat(Index: Integer): Double;
begin
  Result := FInstance.Floats[index];
end;

procedure TZ_JsonArray.SetFloat(Index: Integer; const Value: Double);
begin
  FInstance.Floats[index] := Value;
end;

function TZ_JsonArray.GetDateTime(Index: Integer): TDateTime;
begin
  Result := umlStrToDateTime(FInstance.Strings[index]);
end;

procedure TZ_JsonArray.SetDateTime(Index: Integer; const Value: TDateTime);
begin
  FInstance.Strings[index] := umlDateTimeToStr(Value).Text;
end;

function TZ_JsonArray.GetBool(Index: Integer): Boolean;
begin
  Result := FInstance.Booleans[index];
end;

procedure TZ_JsonArray.SetBool(Index: Integer; const Value: Boolean);
begin
  FInstance.Booleans[index] := Value;
end;

function TZ_JsonArray.GetArray(Index: Integer): TZ_JsonArray;
var
  arry: TZ_Instance_JsonArray;
  j: Integer;
begin
  arry := FInstance.Arrays[index];
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
  Obj_ := FInstance.Objects[Index];
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
  Result := FInstance.IndexOfName(Name);
end;

function TZ_JsonObject.GetString(const Name: string): string;
begin
  Result := FInstance.Strings[Name];
end;

procedure TZ_JsonObject.SetString(const Name, Value: string);
begin
  FInstance.Strings[Name] := Value;
end;

function TZ_JsonObject.GetInt(const Name: string): Integer;
begin
  Result := FInstance.Integers[Name];
end;

procedure TZ_JsonObject.SetInt(const Name: string; const Value: Integer);
begin
  FInstance.Integers[Name] := Value;
end;

function TZ_JsonObject.GetLong(const Name: string): Int64;
begin
  Result := FInstance.Int64s[Name];
end;

procedure TZ_JsonObject.SetLong(const Name: string; const Value: Int64);
begin
  FInstance.Int64s[Name] := Value;
end;

function TZ_JsonObject.GetULong(const Name: string): UInt64;
begin
  Result := FInstance.QWords[Name];
end;

procedure TZ_JsonObject.SetULong(const Name: string; const Value: UInt64);
begin
  FInstance.QWords[Name] := Value;
end;

function TZ_JsonObject.GetFloat(const Name: string): Double;
begin
  Result := FInstance.Floats[Name];
end;

procedure TZ_JsonObject.SetFloat(const Name: string; const Value: Double);
begin
  FInstance.Floats[Name] := Value;
end;

function TZ_JsonObject.GetDateTime(const Name: string): TDateTime;
begin
  Result := umlStrToDateTime(FInstance.Strings[Name]);
end;

procedure TZ_JsonObject.SetDateTime(const Name: string; const Value: TDateTime);
begin
  FInstance.Strings[Name] := umlDateTimeToStr(Value).Text;
end;

function TZ_JsonObject.GetBool(const Name: string): Boolean;
begin
  Result := FInstance.Booleans[Name];
end;

procedure TZ_JsonObject.SetBool(const Name: string; const Value: Boolean);
begin
  FInstance.Booleans[Name] := Value;
end;

function TZ_JsonObject.GetArray(const Name: string): TZ_JsonArray;
var
  arry: TZ_Instance_JsonArray;
  j: Integer;
begin
  if FInstance.Find(Name, arry) then
    begin
      for j := FList.Count - 1 downto 0 do
        if (FList[j] is TZ_JsonArray) and (TZ_JsonArray(FList[j]).FInstance = arry) then
          begin
            Result := TZ_JsonArray(FList[j]);
            exit;
          end;
    end;

  arry := TZ_Instance_JsonArray.Create();
  FInstance.Arrays[Name] := arry;
  Result := TZ_JsonArray.Create(self);
  Result.FInstance := arry;
end;

function TZ_JsonObject.GetObject(const Name: string): TZ_JsonObject;
var
  Obj_: TZ_Instance_JsonObject;
  j: Integer;
begin
  if FInstance.Find(Name, Obj_) then
    begin
      for j := FList.Count - 1 downto 0 do
        if (FList[j] is TZ_JsonObject) and (TZ_JsonObject(FList[j]).FInstance = Obj_) then
          begin
            Result := TZ_JsonObject(FList[j]);
            exit;
          end;
    end;

  Obj_ := TZ_Instance_JsonObject.Create();
  FInstance.Objects[Name] := Obj_;
  Result := TZ_JsonObject.Create(self);
  Result.FInstance := Obj_;
end;

function TZ_JsonObject.GetName(Index: Integer): string;
begin
  Result := FInstance.Names[index];
end;

function TZ_JsonObject.GetCount: Integer;
begin
  Result := FInstance.Count;
end;

procedure TZ_JsonObject.SaveToStream(stream: TCoreClassStream; Formated_: Boolean);
var
  s_: TPascalString;
  buff: TBytes;
begin
  if Formated_ then
      s_.Text := FInstance.FormatJSON()
  else
      s_.Text := FInstance.AsJSON;
  buff := s_.Bytes;
  s_ := '';
  stream.Write(buff[0], length(buff));
  SetLength(buff, 0);
end;

procedure TZ_JsonObject.LoadFromStream(stream: TCoreClassStream);
Var
  P: TJSONParser;
  j: TJSONData;
begin
  DisposeObjectAndNil(FInstance);
  P := TJSONParser.Create(stream, [joUTF8]);
  try
    j := P.Parse;
    if j is TZ_Instance_JsonObject then
        FInstance := TZ_Instance_JsonObject(j)
  except
  end;
  FreeAndNil(P);
end;
