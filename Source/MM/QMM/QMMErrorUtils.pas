unit QMMErrorUtils;

interface

implementation

{$ifdef debug}

{$i QMM.inc}
uses
  Windows, QMM;

function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean): Boolean; overload;
asm
{$ifdef win32}
  lock cmpxchg [ecx], dl
{$else}
.noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;

type
  PSpin = ^TSpin;
  TSpin = record
    inited: Boolean;
    lock: Boolean;
    spin: Cardinal;
  end;

procedure spin_init(v: PSpin; spin_count: Integer);
begin
  if v.inited then exit;
  v.lock := false;
  v.spin := spin_count;
  v.inited := true;
end;

procedure spin_uninit(v: PSpin);
begin
  if not v.inited then exit;
  fillchar(v, sizeof(v), 0);
end;

procedure spin_lock(v: PSpin);

  procedure do_pause;
  asm
    pause
  end;

var
  loop: Cardinal;
  locked: Boolean;
begin
  loop := 0;
  repeat
    locked := lock_cmp_exchange(false, true, v.lock);
    if not locked then
      break;
    inc(loop);
    if loop < v.spin then
      do_pause()
    else
      SwitchToThread;
  until false;
end;

procedure spin_unlock(v: PSpin);
begin
  v.lock := false;
end;


// code from FastMM4.pas.DetectClassInstance
function detect_class(mem: Pointer): TClass;
const
  MEM_PAGE_PROTECTs =
    PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE or
    PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY;
var
  mem_info: TMemoryBasicInformation;

  function valid_VMT(address: Pointer): Boolean;
  begin
    if (MSIZE(address) > 65535) and (MSIZE(address) and 3 = 0) then
    begin
      if (MADDR(mem_info.BaseAddress) > MADDR(address))
        or ((MADDR(mem_info.BaseAddress) + mem_info.RegionSize) < (MADDR(address) + 4)) then
      begin
        mem_info.RegionSize := 0;
        VirtualQuery(address, mem_info, sizeof(mem_info));
      end;
      result := (mem_info.RegionSize >= 4)
        and (mem_info.State = MEM_COMMIT)
        and (mem_info.Protect and MEM_PAGE_PROTECTs <> 0)
        and (mem_info.Protect and PAGE_GUARD = 0);
    end else
      result := false;
  end;

  function valid_class(p: Pointer; depth: Integer): Boolean;
  var
    parent: PPointer;
  begin
    if (depth < 255) and valid_VMT(MADDR(p) + vmtSelfPtr) and
      valid_VMT(MADDR(p) + vmtParent) then
    begin
      parent := PPointer(MADDR(p) + vmtParent)^;
      result := (PPointer(MADDR(p) + vmtSelfPtr)^ = p) and
        ((parent = nil) or (valid_VMT(parent) and valid_class(parent^, depth + 1)));
    end else
      result := false;
  end;

begin
  fillchar(mem_info, sizeof(mem_info), #0);
  mem_info.RegionSize := 0;
  result := PPointer(mem)^;
  if not valid_class(result, 0) then
    result := nil;
end;

procedure get_file(path: PAnsiChar; size: Integer; suffix: PAnsichar);
var
  val, flen: Integer;
begin
  fillchar(path[0], size, #0);
  val := GetModuleFileNameA(HInstance, path, size);
  path[val] := #0;
  flen := lstrlenA(suffix);
  lstrcatA(path, suffix);
  path[val + flen + 1] := #0;
end;


  function last_delimiter(name: PAnsiChar; len: Integer): Integer;
  begin
    result := len - 1;
    while result > 0 do
    begin
      if name[result] in ['.', '/', '\'] then exit;
      Dec(Result);
    end;
  end;

var
  inited: Boolean = false;
  instance_name, instance_path, app_name: array [0..255] of AnsiChar;

procedure init_log_path();
var
  code: Cardinal;
  val, start: Integer;
begin
  if inited then
  begin
    code := GetFileAttributesA(instance_path);
    if not ((code <> INVALID_FILE_ATTRIBUTES) and (FILE_ATTRIBUTE_DIRECTORY and code <> 0)) then
      if not CreateDirectoryA(instance_path, nil) then
      begin
        instance_path := instance_name;
        val := last_delimiter(instance_path, lstrlenA(instance_path));
        instance_path[val] := #0;
      end;
    exit;
  end;

  fillchar(instance_name[0], sizeof(instance_name), #0);
  val := GetModuleFileNameA(HInstance, instance_name, sizeof(instance_name));

  app_name := instance_name;
  val := last_delimiter(app_name, val);
  start := last_delimiter(app_name, val);
  move(app_name[start + 1], app_name[0], val - start);
  app_name[val - start - 1] := #0;

  instance_path := instance_name;
  val := last_delimiter(app_name, val);
  instance_path[val] := #0;
  lstrcatA(instance_path, '\mem-error\');
  code := GetFileAttributesA(instance_path);
  if not ((code <> INVALID_FILE_ATTRIBUTES) and (FILE_ATTRIBUTE_DIRECTORY and code <> 0)) then
    if not CreateDirectoryA(instance_path, nil) then
    begin
      instance_path := instance_name;
      instance_path[val] := #0;
    end;

  inited := true;
end;

procedure to_hex(src: PByte; src_len: Integer; dest: MADDR; dest_len: Integer);
const
  TABLEs: PAnsiChar = '0123456789ABCDEF';
var
  index: Integer;
begin
  if dest_len < src_len shl 2 then
    src_len := dest_len shr 2;
  fillchar(dest^, dest_len, #0);
  index := 0;
  while src_len > 0 do
  begin
    inc(index);
    (dest + 0)^ := TABLEs[src^ shr $4];
    (dest + 1)^ := TABLEs[src^ and $f];
    if index and $f = 0 then
    begin
      (dest + 2)^ := #13;
      (dest + 3)^ := #10;
      inc(dest, 4);
    end else
    begin
     (dest + 2)^ := #32;
      inc(dest, 3);
    end;
    inc(src);
    dec(src_len);
  end;
  dest^ := #13;
  (dest + 1)^ := #10;
end;

function format_buffer(buffer, format_string: PAnsiChar;
  const args: array of const): Integer;
var
  i: Integer;
  params: array [0..31] of MSIZE;
begin
  if Length(args) > 0 then
  begin
    for i := low(args) to high(args) do
    begin
      case args[i].VType of
        vtInteger:
          params[i] := args[i].VInteger;
        vtInt64:
          params[i] := args[i].VInt64^;
        vtPointer:
          params[i] := MSIZE(args[i].VPointer);
      else
      {$ifdef win32}
        params[i] := args[i].VInteger;
      {$else}
        params[i] := MSIZE(args[i].VPointer);
      {$endif}
      end;
    end;
    result := wvsprintfA(buffer, format_string, @params);
  end else
  begin
    fillchar(buffer, sizeof(buffer), #0);
    lstrcatA(buffer, format_string);
    result := lstrlenA(buffer);
  end;
end;

procedure write_memory_error_to_file(msg: PAnsiChar; size: Integer;
  data_buffer: PAnsiChar; data_size: Integer);
var
  val: Integer;
  handle: THandle;
  tid, bytes: Cardinal;
  log_filename: array [0..255] of AnsiChar;
begin
  init_log_path();

  fillchar(log_filename, sizeof(log_filename), 0);
  lstrcatA(log_filename, instance_path);
  val := lstrlenA(log_filename);
  tid := GetCurrentThreadId();
  format_buffer(log_filename + val, '%s.threadid=%d.txt', [@app_name[0], tid]);

  handle := CreateFileA(log_filename, GENERIC_WRITE,
      FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if handle = INVALID_HANDLE_VALUE then exit;

  SetFilePointer(handle, 0, nil, FILE_END);
  WriteFile(handle, msg^, size, bytes, nil);
  WriteFile(handle, data_buffer^, data_size, bytes, nil);
  CloseHandle(handle);
end;

type
  PDebugMem = ^TDebugMem;
  TDebugMem = record
    ori_size: MSIZE;
    first_tag: Cardinal;
  {$ifdef TRACE_STACK}
    stack_trace: TStackTrace;
  {$endif}
    last_tag: PCardinal;
  end;

const
  SOPNames: array [TDebugMemOP] of AnsiString = ('Memory.Get', 'Memory.Realloc', 'Memory.Free');

procedure do_memory_error_proc(op: TDebugMemOP; address: Pointer; mem_size: MSIZE);
const
  SErrorFormat = '[%.4d-%.2d-%.2d %.2d:%.2d:%.2d]memory error:'#13#10 +
    '"%s", address: %p, mem size: %d, instance data type: %s'#13#10
  {$ifdef TRACE_STACK}
    + 'stack trace: $%X -> $%X -> $%X -> $%X -> $%X -> $%X -> $%X -> $%X'#13#10
  {$endif}
    + 'hex data:'#13#10;
var
{$ifdef TRACE_STACK}
  trace: ^TStackTrace;
{$endif}
  val: Integer;
  mem_class: TClass;
  time: TSystemTime;
  pname: PAnsiChar;
  cname: array [0..255] of AnsiChar;
  buffer, data_buffer: array [0..1023] of AnsiChar;
begin
  GetLocalTime(time);
{$ifdef TRACE_STACK}
  trace := @PDebugMem(PAnsiChar(address) - sizeof(TDebugMem)).stack_trace;
{$endif}
  mem_class := detect_class(address);

  fillchar(cname[0], sizeof(cname), 0);
  if mem_class <> nil then
  begin
    pname := Pointer(PPointer(MSIZE(address^) + vmtClassName)^);
    move((pname + 1)^, cname[0], Byte(pname^));
  end else
  begin
    cname := 'unknow class';
  end;
  to_hex(address, mem_size, data_buffer, sizeof(data_buffer));
  val := format_buffer(buffer, SErrorFormat,
  [
    time.wYear, time.wMonth, time.wDay, time.wHour, time.wMinute, time.wSecond,
    SOPNames[op], address, mem_size, cname
  {$ifdef TRACE_STACK}
    , trace^[0], trace^[1], trace^[2], trace^[3], trace^[4], trace^[5],
       trace^[6], trace^[7]
  {$endif}
  ]);


  write_memory_error_to_file(buffer, val, data_buffer, lstrlenA(data_buffer));
end;

initialization
  QMM.on_memory_error_proc := do_memory_error_proc;
{$endif}

end.
