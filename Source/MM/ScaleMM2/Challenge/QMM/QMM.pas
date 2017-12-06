
{ ***********************************************************************

QIU Memory Manager 1.0 for Delphi

Description:
	a simple and compact MM for Delphi/XE

Homepage:
  https://code.google.com/p/qiumm/
  by qiusonglin (qiusonglin.hex@gmail.com)

Usage:
 - place this unit as the very first unit under the "uses" section in your
   project's .dpr file.

Other:
 - important: test only by D7,D2010
 - NOT test on the WIN64
 - without professional software testing, please use caution
 - support multithread, allocate memory for each thread manager.
 - don't support between DLL and APP shared memory device (next version will...)

Support:
 If you have trouble using QMM, you are welcome to drop me an e-mail at the
 address above.

License:
  Released under Mozilla Public License 1.1

  If you find QMM useful or you would like to support further development,
  a donation would be much appreciated.
  My PayPal account is: qiusonglin.hex@gmail.com

Change log:
  QMM.change.log

2013.11.26

 *********************************************************************** }
unit QMM;

interface

uses Windows;

{$i QMM.inc}

type
{$if CompilerVersion < 22}
  MSIZE = Integer;
{$else}
  MSIZE = NativeInt;
{$ifend}
  MADDR = PAnsiChar;

  TMemoryStatus = record
    // allocate size = total_alloc - total_free
    total_alloc: Int64;
    total_free: Int64;
    // small allocate size = small_alloc - small_free
    small_alloc: Int64;
    small_free: Int64;
    // medium allocate size = medium_alloc - medium_free
    medium_alloc: Int64;
    medium_free: Int64;
    // large allocate size = large_alloc - large_free
    large_alloc: Int64;
    large_free: Int64;
  end;

{$ifdef debug}
function debug_memory_get(size: MSIZE): Pointer;
function debug_memory_alloc(size: MSIZE): Pointer;
function debug_memory_realloc(p: Pointer; size: MSIZE): Pointer;
function debug_memory_free(p: Pointer): Integer;
{$endif}

function memory_get(size: MSIZE): Pointer;
function memory_alloc(size: MSIZE): Pointer;
function memory_realloc(p: Pointer; size: MSIZE): Pointer;
function memory_free(p: Pointer): Integer;

function memory_register_leak(p: Pointer): Boolean;
function memory_unregister_leak(p: Pointer): Boolean;

function memory_status: TMemoryStatus;

{$ifdef debug}

type
  TDebugMemOP = (opGet, opRealloc, opFree);

var
  //
  // memory manager event callbacks on DEBUG
  // note: the callbacks is not threadsafe.
  //
  // the memory operations out of bounds, will trigger the event
  on_memory_error_proc: procedure(op: TDebugMemOP; address: Pointer; size: MSIZE);
  // after memory_get
  on_notify_get_proc: procedure(ptr: Pointer; size: MSIZE);
  // after memory_realloc
  on_notify_realloc_proc: procedure(old_ptr: Pointer; old_size: MSIZE;
    new_ptr: Pointer; new_size: MSIZE);
  // after memory_free
  on_notify_free_proc: procedure(ptr: Pointer; size: MSIZE);

{$if CompilerVersion < 18}
var
  // report memory leak
  ReportMemoryLeaksOnShutdown: Boolean = false;
{$ifend}

{$endif}

implementation

{$ifdef fastcode}
uses FastcodeFillCharUnit, FastMove;
{$endif}

type
  PSpinLock = ^TSpinLock;
  TSpinLock = record
    lock: Integer;
    ref_cnt: Integer;
    thread_id: Cardinal;
  end;

procedure spinlock_init(lock: PSpinLock);
{$ifdef has_inline} inline; {$endif}
begin
  fillchar(lock^, sizeof(lock^), #0);
end;

procedure spinlock_lock(lock: PSpinLock; timeout: Integer = 0);
var
  spin: Integer;
  curr_thread_id: Cardinal;
begin
  asm mfence end;

  if (lock.lock = 1) then
  begin
    curr_thread_id := GetCurrentThreadId;
    if (lock.thread_id = curr_thread_id) then
      InterlockedIncrement(lock.ref_cnt)
    else
    begin
      spin := 0;
      if timeout = 0 then
        timeout := $7FFFFFFF;
      while timeout > 0 do
      begin
        asm mfence end;
        if InterlockedExchange(lock.lock, 1) = 0 then
        begin
          lock.ref_cnt := 1;
          lock.thread_id := GetCurrentThreadId;
          break;
        end;

        if spin < 4 then
        else
        if spin < 16 then
        asm pause end
        else
        if spin < 32 then
          sleep(0)
        else
          sleep(1);
        inc(spin);

        dec(timeout);
      end;
    end;
  end else
  begin
    spin := 0;
    if timeout = 0 then
      timeout := $7FFFFFFF;
    while timeout > 0 do
    begin
      asm mfence end;
      if InterlockedExchange(lock.lock, 1) = 0 then
      begin
        lock.ref_cnt := 1;
        lock.thread_id := GetCurrentThreadId;
        break;
      end;

      if spin < 4 then
      else
      if spin < 16 then
      asm pause end
      else
      if spin < 32 then
        sleep(0)
      else
        sleep(1);
      inc(spin);
      dec(timeout);
    end;
  end;
end;

procedure spinlock_unlock(lock: PSpinLock);
begin
  asm mfence end;
  if InterlockedDecrement(lock.ref_cnt) <= 0 then
    lock.lock := 0;
end;

function roundup_pow_of_two(size: MSIZE): MSIZE;
{$ifdef has_inline} inline; {$endif}
var
  l_bit: Integer;
begin
  if (size and (size - 1)) = 0 then
    result := size
  else
  begin
    l_bit := 0;
    while size > 0 do
    begin
      size := size shr 1;
      inc(l_bit);
    end;
    result := 1 shl l_bit;
  end;
end;

function virtual_alloc(var size: MSIZE): Pointer;
{$ifdef has_inline} inline; {$endif}
begin
  size := roundup_pow_of_two(size);
  result := VirtualAlloc(nil, size, MEM_COMMIT, PAGE_READWRITE);
  if result = nil then
    System.Error(reOutOfMemory);
end;

procedure virtual_free(data: Pointer);
{$ifdef has_inline} inline; {$endif}
begin
  VirtualFree(data, 0, MEM_RELEASE);
end;

{$ifopt c+}
function assert(condition: Boolean; step: Integer = 0): Boolean;
begin
  result := condition;
  if not result then
  asm
    int 3;   // breakpoint
  end;
end;
{$endif}

type
  PPLinkData = ^PLinkData;
  PLinkData = ^TLinkData;
  TLinkData = record
    data: Pointer;
    next: Pointer;
  end;

const
  FLAG_BIT            = 4;

  FLAG_NONE           = $0;
  FLAG_USED           = $1;
  FLAG_LINK           = $2;
  FLAG_WAITFORFREE    = $4;
  FLAG_MASK           = FLAG_USED or FLAG_LINK or FLAG_WAITFORFREE;

  FLAG_BLOCK_MINI     = Integer($10000000);
  FLAG_BLOCK_SMALL    = Integer($20000000);
  FLAG_BLOCK_MEDIUM   = Integer($40000000);
  FLAG_BLOCK_LARGE    = Integer($80000000);
  FLAG_BLOCK_MASK     = Integer($F0000000);
  FLAG_SIZE           = Integer($0FFFFFF0);

const
  MIN_MEM_SIZE        = $20;
  MEM_PAGE_SIZE       = 1024 * 4;

  MAX_SIZE_MINI       = 256;
  MAX_SIZE_SMALL      = 1024 * 2;
  MAX_SIZE_MEDIUM     = 1024 * 128;

  BIT_MINI_SIZE       = 5;
  BIT_SMALL_SIZE      = 7;
  SIZE_BLOCK          = 1024 * 1024;

  MAX_BUFFER_LARGE    = (MEM_PAGE_SIZE div 12) - 1;  // 8 = sizeof(TMemLarge)

  SIZE_HASH_LEAK      = 4096;
  SIZE_HASH_LINK      = 256;

type
  PPMem = ^PMem;
  PMem = ^TMem;
  PMemBlock = ^TMemBlock;
  PMemItem = PMem;
  PPMemItems = ^PMemItems;
  PMemItems = ^TMemItems;
  PMemItemsBlock = ^TMemItemsBlock;

  PMemItemBuffer = ^TMemItemBuffer;
  TMemItemBuffer = record
    next, prev: PMemItemBuffer;
    owner: PMemItems;
    item_count: Integer;
    //idle_mem: PMem;
    idle_count: Integer;
    data: array [0..0] of Byte;
  end;

  TMemItems = record
    item_size: Integer;
    idle_item: PMem;
    first_buffer: PMemItemBuffer;
    items_buffer: PMemItemBuffer;
  end;

  TMemItemsBlock = record
    bflag: MSIZE;
    count, min, max, bits, step: Integer;
    lists: array [0..0] of PMemItems;
  end;

  PFreeItemBuffer = ^TFreeItemBuffer;
  TFreeItemBuffer = record
    next: PFreeItemBuffer;
    items: array [0..((MEM_PAGE_SIZE * 10) div sizeof(TLinkData)) - 2] of TLinkData;
  end;

  TBlockType = (btSmall, btMedium, btLarge);

  TMemBlock = record
    owner: Pointer;
    btype: TBlockType;
    bflag: MSIZE;
    src_ptr: Pointer;
    end_ptr: Pointer;
    src_len: MSIZE;
    curr_mem: PMem;
    next, prev: PMemBlock;
  end;

  PMemLarge = ^TMemLarge;
  TMemLarge = record
    keep: LongBool;
    size: MSIZE;
    prev: PMem;
  end;

  PMemLargeBuffer = ^TMemLargeBuffer;
  TMemLargeBuffer = record
    next: PMemLargeBuffer;
    items: array [0..MAX_BUFFER_LARGE - 1] of TMemLarge;
  end;

  PMemoryLeak = PLinkData;
  TMemoryLeak = TLinkData;

  PMemLink = ^TMemLink;
  TMemLink = record
    case Byte of
      // for add_link/del_link
      $0: (link_next, link_prev: PMem);
      // for mem_item_XXX
      $1: (item_next, item_prev: PMem; item_owner: PMemItemBuffer);
      // for reg/unreg leak
      $2: (leak_next, leak_prev: PMem; reg_leak: MSIZE);
      // for all
      $F: (next, prev, data: Pointer);
  end;

  TMem = record
    flag: MSIZE;
    owner: PMemBlock;
    link: TMemLink;
    case Byte of
      0: (prev: PMem);
      1: (items: PMemItems);
      2: (large: PMemLarge);
  end;

const
  SIZE_HEADER = sizeof(TMem);

const
  HASH_SIZE_THREAD_MGR  = $FF + 1;

  PER_THREAD_BUFFER_COUNT = 64;

  MAX_BLOCK_COUNT = 8;

type
  PMemManager = ^TMemManager;
  PThreadMemory = ^TThreadMemory;

  TThreadMemory = object
  private
    initialized: Boolean;
    active: Boolean;
    owner: PMemManager;
    lock: TSpinLock;
    status: TMemoryStatus;
    item_idle: PLinkData;
    item_buffer: PFreeItemBuffer;
    block_buffer: PLinkData;
    block_buffer_count: Integer;
    function pop_idle: PLinkData;
    {$ifdef has_inline} inline; {$endif}
    procedure push_idle(item: PLinkData);
    {$ifdef has_inline} inline; {$endif}
    function create_block(btype: TBlockType): Pointer;
    procedure release_block(block: PMemBlock);
  private
    block_links: array [TBlockType] of PMem;
    procedure add_link(mem: PMem);
    {$ifdef has_inline} inline; {$endif}
    function del_link(mem: PMem): Boolean;
    {$ifdef has_inline} inline; {$endif}
    procedure update_link(old_mem, new_mem: PMem);
    {$ifdef has_inline} inline; {$endif}
  private
    mini_block: PMemItemsBlock;
    small_block: PMemItemsBlock;
    function create_item_buffer(items: PMemItems; item_size: MSIZE): PMemItemBuffer;
    function mem_item_get(block: PMemItemsBlock; size: MSIZE): Pointer;
    {$ifdef has_inline} inline; {$endif}
    function mem_item_realloc(p: PMem; new_size: MSIZE): Pointer;
    {$ifdef has_inline} inline; {$endif}
    function mem_item_free(p: PMem): Integer;
    {$ifdef has_inline} inline; {$endif}
  private
    medium_block: PMemBlock;
    function medium_get_idle(var size: MSIZE): Pointer;
    function medium_mem_get(size: MSIZE): Pointer;
    function medium_mem_realloc(p: Pointer; new_size: MSIZE): Pointer;
    function medium_mem_free(p: Pointer): Integer;
  private
    large_block: PMemBlock;
    large_hdr_idle: PLinkData;
    large_hdr_buffer: PMemLargeBuffer;
    function pop_large_hdr: PMemLarge;
    {$ifdef has_inline} inline; {$endif}
    procedure push_large_hdr(hdr: PMemLarge);
    {$ifdef has_inline} inline; {$endif}
    function large_mem_get(size: MSIZE): Pointer;
    function large_mem_free(p: Pointer): Integer;
    function large_mem_realloc(p: Pointer; new_size: MSIZE): Pointer;
    function large_get_idle(var size: MSIZE): Pointer;
  public
    free_list: Integer;
    other_thread_free_lists: PLinkData;
    procedure do_freemem_from_other_thread;
    procedure freemem_by_other_thread(p: Pointer);
  public
    leak_hash: array [0..SIZE_HASH_LEAK - 1] of Pointer;
    function is_register_leak(address: Pointer): Boolean;
    {$ifdef has_inline} inline; {$endif}
    function register_memory_leak(address: Pointer): Boolean;
    {$ifdef has_inline} inline; {$endif}
    function unregister_memory_leak(address: Pointer): Boolean;
    {$ifdef has_inline} inline; {$endif}
  public
    function memory_get(size: MSIZE): Pointer;
    function memory_realloc(p: Pointer; new_size: MSIZE): Pointer;
    function memory_free(p: Pointer): Integer;
  public
    thread_id: Cardinal;
    //thread_handle: THandle;
    next_thread_memory: PThreadMemory;
    procedure initialize(owner_: PMemManager);
    procedure uninitialize;
    procedure reactive(thread_id_: Cardinal);
    procedure deactive;
  end;

  PJump = ^TJump;
  TJump = packed record
    OpCode  : Byte;
    Distance: Integer;
  end;

  TMemManager = object
  private
    lock: TSpinlock;
    link_idle: PLinkData;
    link_buffer: PLinkData;
    mem_idle: PLinkData;
    mem_buffer: PLinkData;
    main_mgr: PThreadMemory;
    mem_mgrs: array [0..HASH_SIZE_THREAD_MGR - 1] of PThreadMemory;
    function pop_link: PLinkData;
    {$ifdef has_inline} inline; {$endif}
    procedure push_link(data: PLinkData);
    {$ifdef has_inline} inline; {$endif}
    procedure create_link_buffer;
    function pop_thread_memory: PThreadMemory;
    {$ifdef has_inline} inline; {$endif}
    procedure push_thread_memory(thread_memory: PThreadMemory);
    {$ifdef has_inline} inline; {$endif}
    procedure create_thread_memory_buffer;
  public
    leak_list: PLinkData;
    function is_register_leak(address: Pointer): Boolean;
    function register_leak(address: Pointer): Boolean;
    function unregister_leak(address: Pointer): Boolean;
  public
    initialized: Boolean;
    procedure initialize;
    procedure uninitialize;
  public
    block_lock: TSpinLock;
    block_buffer: PLinkData;
    block_buffer_count: Integer;
    function create_block(var size: MSIZE): Pointer;
    {$ifdef has_inline} inline; {$endif}
    procedure release_block(block: PMemBlock);
    {$ifdef has_inline} inline; {$endif}
  public
    thread_count: Integer;
    function get_thread_memory(): PThreadMemory;
    {$ifdef has_inline} inline; {$endif}
    function create_thread_memory(thread_id: Cardinal): PThreadMemory;
    procedure release_thread_memory(thread_id: Cardinal);
  end;


{$ifdef debug}
function mcheck_large_hdr(mem, link: PMem): Boolean;
var
  hdr: PMemLarge;
begin
  result := true;
  hdr := mem.large;
  while link <> nil do
  begin
    if (mem <> link) and (link.large = hdr) then
    begin
      result := assert(false, 0);
      break;
    end;
    link := link.link.link_next;
  end;
end;

function mcheck_large(step: Integer; block: PMemBlock; link: PMem): Boolean;
var
  end_mem: MADDR;
  prev, curr, next: PMem;
  size, curr_size, prev_size: MSIZE;
begin
  result := true;
  if block.btype <> btLarge then exit;

  result := assert(block.src_len = SIZE_BLOCK - sizeof(TMemBlock) - SIZE_HEADER);
  if not result then exit;

  size := 0;
  curr := block.src_ptr;
  end_mem := block.end_ptr;
  while result and (MADDR(curr) < MADDR(end_mem)) do
  begin
    prev := curr.large.prev;
    curr_size := curr.large.size;
    size := size + SIZE_HEADER + curr_size;
    assert(size <= block.src_len + SIZE_HEADER);
    next := Pointer(MADDR(curr) + SIZE_HEADER + curr_size);
    if prev <> nil then
    begin
      prev_size := prev.large.size;
      result := assert(MADDR(curr) = MADDR(prev) + SIZE_HEADER + prev_size, step);
      if not result then
      begin
        assert(false);
        break;
      end;
    end;

    if (MADDR(next) < MADDR(end_mem)) and assert(next.large <> nil) and (next.large.prev <> nil) then
    begin
      assert(next.large.prev = curr);
    end;
    curr := next;
  end;
  if result then
    assert(size = (block.src_len + SIZE_HEADER));

  size := 0;
  curr := link;
  while curr <> nil do
  begin
    size := size + SIZE_HEADER + curr.large.size;
    assert(size <= SIZE_BLOCK);
    curr := curr.link.link_next;
  end;
end;

function mcheck_block(step: Integer; block: PMemBlock): Boolean;
var
  end_mem: MADDR;
  size, prev_size: MSIZE;
  prev, curr, next: PMem;
begin
  result := true;
  if (block = nil) then exit;
  if (block.btype = btLarge) then
  begin
    exit;
  end;

  result := assert(block.src_len = SIZE_BLOCK - sizeof(TMemBlock) - SIZE_HEADER);
  if not result then exit;
  curr := block.src_ptr;
  end_mem := block.end_ptr;
  while result and (MADDR(curr) < MADDR(end_mem)) do
  begin
    prev := curr.prev;
    size := curr.flag and FLAG_SIZE shr FLAG_BIT;
    next := Pointer(MADDR(curr) + SIZE_HEADER + size);
    if prev <> nil then
    begin
      prev_size := prev.flag and FLAG_SIZE shr FLAG_BIT;
      result := assert(MADDR(curr) = MADDR(prev) + SIZE_HEADER + prev_size, step);
      if not result then break;
    end;
    if MADDR(next) < MADDR(end_mem) then
    begin
      result :=
        assert(MADDR(next.prev) < MADDR(next)) and
        assert((next.prev = curr) and
        (MADDR(next) = MADDR(curr) + SIZE_HEADER + size), step);
      if not result then
        break;
    end;
    curr := next;
  end;
  result := result and assert(block.src_len = SIZE_BLOCK - sizeof(TMemBlock) - SIZE_HEADER);
  result := result and assert(block.end_ptr = Pointer(MADDR(block) + block.src_len + sizeof(TMemBlock) + SIZE_HEADER));
  if not result then
    assert(false, 100);
end;

procedure mprint(format: PChar; const argv: array of const);
var
  i: Integer;
  params: array [0..31] of Cardinal;
  buffer: array [0..127] of Char;
begin
  for i := low(argv) to high(argv) do
    params[i] := argv[i].VInteger;
  buffer[wvsprintf(buffer, format, @params)] := #0;
  OutputDebugString(buffer);
end;
{$endif}

{ TThreadMemory }

function _align_mem_size(size: Integer): Integer;
{$ifdef has_inline} inline; {$endif}
begin
  if size <= MIN_MEM_SIZE then
    result := MIN_MEM_SIZE
  else
    result := (size + MIN_MEM_SIZE - 1) and -MIN_MEM_SIZE;
end;

procedure TThreadMemory.initialize(owner_: PMemManager);

  function create_mem_items_block(block_flag: MSIZE;
    min, max, bits: Integer): PMemItemsBlock;
  var
    items: PMemItems;
    i, size, item_count, item_step: MSIZE;
  begin
    item_step := 1 shl bits;
    item_count := ((max - min) div item_step) + 1;
    size := sizeof(TMemItemsBlock) + item_count * sizeof(Pointer) +
      item_count * sizeof(TMemItems);
    result := virtual_alloc(size);
    fillchar(result^, size, #0);
    result.bflag := block_flag;
    result.count := item_count;
    result.min := min;
    result.max := max;
    result.bits := bits;
    result.step := item_step;
    items := Pointer(MADDR(result) + sizeof(TMemItemsBlock) +
      item_count * sizeof(Pointer));
    for i := 0 to item_count - 1 do
    begin
      items.item_size := min + item_step * i;
      result.lists[i] := items;
      inc(items);
    end;
  end;

begin
  if initialized then exit;
  fillchar(self, sizeof(self), #0);
  initialized := true;
  owner := owner_;

  mini_block := create_mem_items_block(FLAG_BLOCK_MINI, 0, MAX_SIZE_MINI, BIT_MINI_SIZE);
  small_block := create_mem_items_block(FLAG_BLOCK_SMALL, MAX_SIZE_MINI, MAX_SIZE_SMALL, BIT_SMALL_SIZE);
  create_block(btMedium);
  // wait for get large
  //create_block(btLarge);
  push_idle(pop_idle);
end;

procedure TThreadMemory.uninitialize;

  procedure free_block(var block: PMemBlock); overload;
  var
    next: PMemBlock;
  begin
    while block <> nil do
    begin
      next := block.next;
      virtual_free(block);
      block := next;
    end;
  end;

  procedure free_block(var block: PLinkData); overload;
  var
    link: PLinkData;
  begin
    while block <> nil do
    begin
      link := block.next;
      virtual_free(block.data);
      block := link;
    end;
  end;

var
  next: PFreeItemBuffer;
begin
  if not initialized then exit;

  virtual_free(mini_block);
  virtual_free(small_block);
  free_block(medium_block);
  free_block(large_block);
  free_block(block_buffer);
  while item_buffer <> nil do
  begin
    next := item_buffer.next;
    virtual_free(item_buffer);
    item_buffer := next;
  end;
  fillchar(self, sizeof(self), #0);
end;

procedure TThreadMemory.reactive(thread_id_: Cardinal);
begin
  thread_id := thread_id_;
  if other_thread_free_lists <> nil then
    do_freemem_from_other_thread;
  //thread_handle := GetCurrentThread;
  active := true;
end;

procedure TThreadMemory.deactive;
var
  next: PLinkData;
begin
  active := false;
  thread_id := 0;
  //thread_handle := 0;
  while block_buffer <> nil do
  begin
    next := block_buffer.next;
    owner.release_block(block_buffer.data);
    push_idle(block_buffer);
    block_buffer := next;
  end;
  block_buffer_count := 0;
  if other_thread_free_lists = nil then
    do_freemem_from_other_thread;
end;

procedure TThreadMemory.freemem_by_other_thread(p: Pointer);
var
  item: PLinkData;
begin
  spinlock_lock(@lock);
  try
    with PMem(MADDR(p) - SIZE_HEADER)^ do
      flag := flag or FLAG_WAITFORFREE;
    item := pop_idle;
    item.data := p;
    item.next := other_thread_free_lists;
    other_thread_free_lists := item;
    free_list := 1;
  finally
    spinlock_unlock(@lock);
  end;
end;

procedure TThreadMemory.do_freemem_from_other_thread;
var
  item, next: PLinkData;
begin
  if other_thread_free_lists = nil then exit;

  spinlock_lock(@lock);
  try
    item := other_thread_free_lists;
    other_thread_free_lists := nil;
  finally
    spinlock_unlock(@lock);
  end;

  while item <> nil do
  begin
    next := item.next;
    memory_free(item.data);
    push_idle(item);
    item := next;
  end;
  free_list := 0;
end;

function TThreadMemory.is_register_leak(address: Pointer): Boolean;
begin
  result := PMem(MADDR(address) - SIZE_HEADER).link.reg_leak = 1;
end;

function TThreadMemory.register_memory_leak(address: Pointer): Boolean;
var
  leak: PMem;
  index: Integer;
begin
  result := true;
  leak := Pointer(MADDR(address) - SIZE_HEADER);
  index := MSIZE(leak) and (SIZE_HASH_LEAK - 1);
  leak.link.reg_leak := 1;
  leak.link.leak_next := leak_hash[index];
  if leak_hash[index] <> nil then
    PMem(leak_hash[index]).link.leak_prev := leak;
  leak_hash[index] := leak;
end;

function TThreadMemory.unregister_memory_leak(address: Pointer): Boolean;
var
  leak, next, prev: PMem;
begin
  leak := Pointer(MADDR(address) - SIZE_HEADER);
  result := leak.link.reg_leak = 1;
  if not result then exit;
  next := leak.link.leak_next;
  prev := leak.link.leak_prev;
  if prev = nil then
  begin
    leak_hash[MSIZE(leak) and (SIZE_HASH_LEAK - 1)] := next;
    if next <> nil then
      next.link.leak_prev := nil;
  end else
  begin
    prev.link.leak_next := next;
    if next <> nil then
      next.link.leak_prev := prev;
  end;
  leak.link.reg_leak := 0;
  leak.link.leak_next := nil;
  leak.link.leak_prev := nil;
end;

procedure TThreadMemory.add_link(mem: PMem);
var
  link_ptr: PPMem;
  mem_size: MSIZE;
begin
{$ifopt c+}
  if mem.flag > 0 then
    assert(mem.flag and FLAG_LINK <> FLAG_LINK);
{$endif}
  with mem^, mem.link do
  begin
    mem_size := flag and FLAG_SIZE shr FLAG_BIT;
    flag := (owner.bflag or FLAG_LINK) + (mem_size shl FLAG_BIT);
    link_ptr := @block_links[owner.btype];
    link_prev := nil;
    link_next := link_ptr^;
    if link_ptr^ <> nil then
      link_ptr^.link.link_prev := mem;
    link_ptr^ := mem;
  end;
end;

function TThreadMemory.del_link(mem: PMem): Boolean;
begin
  result := true;
{$ifdef debug}
  if mem.flag < 0 then
    mcheck_large_hdr(mem, self.block_links[btLarge]);
{$endif}
  with mem^, mem.link do
  begin
    flag := flag and not FLAG_LINK;
    if link_prev = nil then
    begin
      block_links[owner.btype] := link_next;
      if link_next <> nil then
        link_next.link.link_prev := nil;
    end else
    begin
      link_prev.link.link_next := link_next;
      if link_next <> nil then
        link_next.link.link_prev := link_prev;
    end;
    link_next := nil;
    link_prev := nil;
  end;
end;

procedure TThreadMemory.update_link(old_mem, new_mem: PMem);
begin
  del_link(old_mem);
  add_link(new_mem);
end;

function TThreadMemory.create_item_buffer(items: PMemItems;
  item_size: MSIZE): PMemItemBuffer;
var
  mem_size: MSIZE;
  //idle: PLinkData;
  block: PMemBlock;
  item, prev: PMemItem;
  item_step, item_count: Integer;
begin
  if item_size < MAX_SIZE_MINI then
    item_count := 16
  else
    item_count := 4;
  item_step := item_size + SIZE_HEADER;
  mem_size := sizeof(TMemItemBuffer) + item_step * item_count;
  result := Self.medium_mem_get(mem_size);
  register_memory_leak(result);
  //fillchar(result^, mem_size, #0);
  result.owner := items;
  result.item_count := item_count;
  result.idle_count := item_count;
  item := @result.data[0];
  // items.idle_item is null, so...
  prev := nil;
  block := PMem(MADDR(result) - SIZE_HEADER).owner;
  while item_count > 0 do
  begin
    item.flag := 0;
    item.owner := block;
    item.items := items;
    item.link.item_next := prev;
    item.link.item_prev := nil;
    item.link.item_owner := result;
    if prev <> nil then
      prev.link.item_prev := item;
    items.idle_item := item;
    prev := item;
    item := Pointer(MADDR(item) + item_step);
    dec(item_count);
  end;
end;

function TThreadMemory.mem_item_get(block: PMemItemsBlock; size: MSIZE): Pointer;
var
  //idle: PLinkData;
  curr: PMemItem;
  items: PMemItems;
  buffer: PMemItemBuffer;
begin
  with block^ do
  begin
    inc(size, step);
    if size > max then
      size := max;
    items := lists[(size - min) shr bits];
    with items^ do
    begin
      if idle_item = nil then
      begin
        buffer := create_item_buffer(items, size shr bits shl bits);
        buffer.next := items_buffer;
        buffer.prev := nil;
        if items_buffer <> nil then
          items_buffer.prev := buffer;
        items_buffer := buffer;
        if first_buffer = nil then
          first_buffer := buffer;
      end;
      curr := idle_item;
      idle_item := curr.link.item_next;
      if idle_item <> nil then
        idle_item.link.item_prev := nil;
      dec(curr.link.item_owner.idle_count);
      curr.flag := (bflag or FLAG_USED) + (item_size shl FLAG_BIT);
      curr.link.next := nil;
      curr.link.prev := nil;
      result := MADDR(curr) + SIZE_HEADER;
      with status do
      begin
        total_alloc := total_alloc + item_size;
        small_alloc := small_alloc + item_size;
      end;
    end;
  end;
end;

function TThreadMemory.mem_item_realloc(p: PMem; new_size: MSIZE): Pointer;
var
  curr: PMemItem;
  old_size: MSIZE;
begin
  curr := p;
  old_size := curr.flag and FLAG_SIZE shr FLAG_BIT;
{$ifopt c+}
  assert(new_size > old_size);
{$endif}
  result := memory_get(new_size);
  move((MADDR(curr) + SIZE_HEADER)^, result^, old_size);
  mem_item_free(curr);
end;

function TThreadMemory.mem_item_free(p: PMem): Integer;
var
  item: PMemItem;
  buffer: PMemItemBuffer;
  i, count, item_step: Integer;
begin
  result := 0;
  item := p;
  item.flag := 0;
  with item.items^ do
  begin
    item.link.item_next := idle_item;
    if idle_item <> nil then
      idle_item.link.item_prev := item;
    idle_item := item;

    buffer := item.link.item_owner;
    inc(buffer.idle_count);
    if (first_buffer <> buffer) and (buffer.item_count = buffer.idle_count) then
    begin
      i := 0;
      count := buffer.item_count;
      item_step := item.items.item_size + SIZE_HEADER;
      item := Pointer(MADDR(@buffer.data[0]));
      while i < count do
      begin
        with item^.link do
        begin
          if item_prev = nil then
          begin
            item.items.idle_item := item_next;
            if item_next <> nil then
              item_next.link.item_prev := nil;
          end else
          begin
            item_prev.link.item_next := item_next;
            if item_next <> nil then
              item_next.link.item_prev := item_prev;
          end;
          //item_next := nil;
          //item_prev := nil;
        end;
        inc(i);
        item := Pointer(MADDR(item) + item_step);
      end;
      if buffer.prev = nil then
      begin
        items_buffer := buffer.next;
        if items_buffer <> nil then
          items_buffer.prev := nil;
      end else
      begin
        buffer.prev.next := buffer.next;
        if buffer.next <> nil then
          buffer.next.prev := buffer.prev;
      end;
      unregister_memory_leak(buffer);
      self.memory_free(buffer);
    end;
    with status do
    begin
      total_free := total_free + item_size;
      small_free := small_free + item_size;
    end;
  end;
end;

function TThreadMemory.medium_get_idle(var size: MSIZE): Pointer;
var
  block: PMemBlock;
  curr, next, next_next: PMem;
  curr_size, next_size: MSIZE;
begin
  result := nil;
  curr := block_links[btMedium];
  if curr = nil then exit;
  while curr <> nil do
  begin
  {$ifopt c+}
    assert(curr.flag and FLAG_LINK = FLAG_LINK);
  {$endif}
    curr_size := curr.flag and FLAG_SIZE shr FLAG_BIT;
    if curr_size >= size then
    begin
      del_link(curr);
      block := curr.owner;
      next_size := curr_size - SIZE_HEADER - size;
      if next_size >= MAX_SIZE_SMALL then
      begin
        curr.flag := (FLAG_BLOCK_MEDIUM or FLAG_USED) + (size shl FLAG_BIT);
        next := Pointer(MADDR(curr) + SIZE_HEADER + size);
        next.flag := (FLAG_BLOCK_MEDIUM) + (next_size shl FLAG_BIT);
        next.owner := block;
        next.prev := curr;
        next_next := Pointer(MADDR(next) + SIZE_HEADER + next_size);
        if MADDR(next_next) < MADDR(block.end_ptr) then
          next_next.prev := next;
        add_link(next);
      end else
      begin
        size := curr_size;
        curr.flag := (FLAG_BLOCK_MEDIUM or FLAG_USED) + (size shl FLAG_BIT);
      end;
      curr.link.next := nil;
      curr.link.prev := nil;
      curr.link.data := nil;
      result := MADDR(curr) + SIZE_HEADER;
      break;
    end;
    curr := curr.link.link_next;
  end;
end;

function TThreadMemory.medium_mem_get(size: MSIZE): Pointer;
var
  block: PMemBlock;
  curr, next, prev: PMem;
  curr_size, next_size, prev_size: MSIZE;
begin
{$ifdef debug}
  mcheck_block(30, medium_block);
{$endif}
  result := medium_get_idle(size);
  block := medium_block;
  while result = nil do
  begin
    if (block = nil) or (block.curr_mem = nil) then
      block := create_block(btMedium);
    curr := block.curr_mem;
    curr_size := curr.flag and FLAG_SIZE shr FLAG_BIT;
    if curr_size >= size then
    begin
      next_size := curr_size - SIZE_HEADER - size;
      if next_size >= MAX_SIZE_SMALL then
      begin
        curr.flag := (FLAG_BLOCK_MEDIUM or FLAG_USED) + (size shl FLAG_BIT);
        next := Pointer(MADDR(curr) + SIZE_HEADER + size);
        next.flag := (FLAG_BLOCK_MEDIUM or FLAG_NONE) + (next_size shl FLAG_BIT);
        next.owner := block;
        next.prev := curr;
        block.curr_mem := next;
      end else
      begin
        size := curr_size;
        curr.flag := (FLAG_BLOCK_MEDIUM or FLAG_USED) + (size shl FLAG_BIT);
        block.curr_mem := nil;
      end;
      curr.link.next := nil;
      curr.link.prev := nil;
      curr.link.data := nil;
      result := MADDR(curr) + SIZE_HEADER;
      break;
    end else
    begin
      if curr_size >= MAX_SIZE_SMALL then
      begin
        add_link(curr);
      end else
      begin
        // merge curr to prev
        prev := curr.prev;
        prev_size := prev.flag and FLAG_SIZE shr FLAG_BIT;
        prev.flag := (FLAG_BLOCK_MEDIUM or (prev.flag and FLAG_MASK)) +
          ((prev_size + SIZE_HEADER + curr_size) shl FLAG_BIT);
      end;
      block.curr_mem := nil;
      block := create_block(btMedium);
    end;
  end;
  with status do
  begin
    total_alloc := total_alloc + size;
    medium_alloc := medium_alloc + size;
  end;
  {$ifdef debug}
    mcheck_block(42, block);
  {$endif}
end;

function TThreadMemory.medium_mem_realloc(p: Pointer; new_size: MSIZE): Pointer;
var
  old_size: MSIZE;
  block: PMemBlock;

  function do_resize(curr, next: PMem; var resize_size: MSIZE): Boolean;
  var
    new_next, next_next: PMem;
    calc_size, curr_size, next_flag, next_size, remain_size: MSIZE;
  begin
    result := false;
    curr_size := old_size;
    calc_size := old_size;
    while not result and (MADDR(next) < MADDR(block.end_ptr)) do
    begin
      next_flag := next.flag and FLAG_MASK;
      next_size := next.flag and FLAG_SIZE shr FLAG_BIT;
      next_next := Pointer(MADDR(next) + SIZE_HEADER + next_size);
      case next_flag of
        FLAG_USED:
          break;
        FLAG_NONE:
        begin
          calc_size := calc_size + SIZE_HEADER + next_size;
          result := calc_size >= new_size;
          if result then
          begin
            remain_size := calc_size - SIZE_HEADER - new_size;
            if remain_size >= MAX_SIZE_SMALL then
            begin
              curr.flag := (FLAG_BLOCK_MEDIUM or FLAG_USED) + (new_size shl FLAG_BIT);
              new_next := Pointer(MADDR(curr) + SIZE_HEADER + new_size);
              new_next.flag := (FLAG_BLOCK_MEDIUM or FLAG_NONE) + (remain_size shl FLAG_BIT);
              new_next.owner := block;
              new_next.prev := curr;
              block.curr_mem := new_next;
            end else
            begin
              new_size := calc_size;
              curr.flag := (FLAG_BLOCK_MEDIUM or FLAG_USED) + (new_size shl FLAG_BIT);
              block.curr_mem := nil;
            end;
          end;
          resize_size := new_size - old_size;
          break;
        end;
        FLAG_LINK:
        begin
          if MADDR(next_next) < MADDR(block.end_ptr) then
            next_next.prev := curr;

          del_link(next);
          curr_size := curr_size + SIZE_HEADER + next_size;
          curr.flag := (FLAG_BLOCK_MEDIUM or FLAG_USED) + (curr_size shl FLAG_BIT);
          calc_size := calc_size + SIZE_HEADER + next_size;
          result := calc_size >= new_size;
          if result then
          begin
            remain_size := calc_size - SIZE_HEADER - new_size;
            if remain_size >= MAX_SIZE_SMALL then
            begin
              curr.flag := (FLAG_BLOCK_MEDIUM or FLAG_USED) + (new_size shl FLAG_BIT);
              new_next := Pointer(MADDR(curr) + SIZE_HEADER + new_size);
              new_next.flag := (FLAG_BLOCK_MEDIUM) + (remain_size shl FLAG_BIT);
              new_next.owner := block;
              new_next.prev := curr;
              if MADDR(next_next) < MADDR(block.end_ptr) then
                next_next.prev := new_next;
              add_link(new_next);
            end else
            begin
              new_size := curr_size;
              curr.flag := (FLAG_BLOCK_MEDIUM or FLAG_USED) + (new_size shl FLAG_BIT);
            end;
            resize_size := new_size - old_size;
            break;
          end;
        end;
      end;
      next.flag := 0;
      next.prev := nil;
      next.owner := nil;
      next := next_next;
    end;
  end;

var
  resize: MSIZE;
  curr, next: PMem;
  can_resize: Boolean;
begin
  curr := p;
  block := curr.owner;
  old_size := curr.flag and FLAG_SIZE shr FLAG_BIT;
  next := Pointer(MADDR(curr) + SIZE_HEADER + old_size);
{$ifdef debug}
  mcheck_block(30, block);
{$endif}
  can_resize :=
    (MADDR(next) < MADDR(block.end_ptr)) and
    ((next.flag and FLAG_MASK) in [FLAG_NONE, FLAG_LINK]) and
    (old_size + SIZE_HEADER + (next.flag and FLAG_SIZE shr FLAG_BIT) >= new_size);
  if can_resize and do_resize(curr, next, resize) then
  begin
    result := MADDR(curr) + SIZE_HEADER;
    with status do
    begin
      total_alloc := total_alloc + resize;
      medium_alloc := medium_alloc + resize;
    end;
  end else
  begin
    result := memory_get(new_size);
    move((MADDR(curr) + SIZE_HEADER)^, result^, old_size);
    medium_mem_free(curr);
  end;
{$ifdef debug}
  mcheck_block(31, block);
{$endif}
end;

function TThreadMemory.medium_mem_free(p: Pointer): Integer;
var
  free_size: MSIZE;

  function medium_merge_block(curr: PMem): Boolean;
  var
    block: PMemBlock;
    prev, next, next_next: PMem;
    curr_size, next_size, prev_size, flag: MSIZE;
  begin
    result := false;
    block := curr.owner;
    curr_size := curr.flag and FLAG_SIZE shr FLAG_BIT;
    free_size := curr_size;
    next := Pointer(MADDR(curr) + SIZE_HEADER + curr_size);
    prev := curr.prev;

    if MADDR(next) < MADDR(block.end_ptr) then
    begin
      flag := next.flag and FLAG_MASK;
      if flag in [FLAG_NONE, FLAG_LINK] then
      begin
        if flag = FLAG_LINK then
          del_link(next);
        next_size := next.flag and FLAG_SIZE shr FLAG_BIT;
        next_next := Pointer(MADDR(next) + SIZE_HEADER + next_size);
        if MADDR(next_next) < MADDR(block.end_ptr) then
          next_next.prev := curr;
        curr_size := curr_size + SIZE_HEADER + next_size;
        curr.flag := (FLAG_BLOCK_MEDIUM) + (curr_size shl FLAG_BIT);
        if flag = FLAG_LINK then
        begin
          add_link(curr);
        end else
        if block.curr_mem = next then
          block.curr_mem := curr
        else
        begin
        {$ifopt c+}
          // error, never happen
          assert(false);
          assert(flag = FLAG_NONE);
        {$endif}
          add_link(curr);
        end;
        result := true;
      end;
    end;

    if (prev <> nil) and (prev.flag and FLAG_LINK = FLAG_LINK) then
    begin
      if result then
        curr_size := curr.flag and FLAG_SIZE shr FLAG_BIT;
      next := Pointer(MADDR(curr) + SIZE_HEADER + curr_size);
      if MADDR(next) < MADDR(block.end_ptr) then
        next.prev := prev;
      prev_size := (prev.flag and FLAG_SIZE shr FLAG_BIT) + SIZE_HEADER + curr_size;
      prev.flag := (FLAG_BLOCK_MEDIUM or FLAG_LINK) + (prev_size shl FLAG_BIT);
      if curr.flag and FLAG_LINK = FLAG_LINK then
      begin
        del_link(curr);
      end
      else
      if block.curr_mem = curr then
      begin
        del_link(prev);
        block.curr_mem := prev;
      end else
      begin
        // nothing to do
        // already merge curr to prev
      end;
      result := true;
    end;

    if result then
    begin
      curr := block.src_ptr;
      if curr.flag and FLAG_LINK = FLAG_LINK then
      begin
        curr_size := curr.flag and FLAG_SIZE shr FLAG_BIT;
        if curr_size >= block.src_len then
        begin
          //assert(curr_size = block.src_len, 1000);
          release_block(block);
        end;
      end;
    end;
  end;

var
  curr: PMem;
{$ifdef debug}
  block: PMemBlock;
{$endif}
begin
  result := 0;
  curr := p;
{$ifdef debug}
  block := curr.owner;
  //mcheck_block(20, block);
{$endif}
  if not medium_merge_block(curr) then
  begin
    add_link(curr);
  end;
  with status do
  begin
    total_free := total_free + free_size;
    medium_free := medium_free + free_size;
  end;
{$ifdef debug}
  mcheck_block(21, block);
{$endif}
end;

function TThreadMemory.pop_large_hdr: PMemLarge;
var
  hdr: PMemLarge;
  item: PLinkData;
  size, count: MSIZE;
  buffer: PMemLargeBuffer;
begin
  if large_hdr_idle = nil then
  begin
    size := sizeof(TMemLargeBuffer);
    buffer := virtual_alloc(size);
    buffer.next := large_hdr_buffer;
    large_hdr_buffer := buffer;
    hdr := @buffer.items[0];
    count := (size - sizeof(Pointer)) div sizeof(TMemLarge);
    //count := MAX_BUFFER_LARGE;
    while count > 0 do
    begin
      item := pop_idle();
      item.data := hdr;
      item.next := large_hdr_idle;
      large_hdr_idle := item;
      inc(hdr);
      dec(count);
    end;
  end;
  item := large_hdr_idle;
  large_hdr_idle := item.next;
  result := item.data;
  push_idle(item);
end;

procedure TThreadMemory.push_large_hdr(hdr: PMemLarge);
var
  item: PLinkdata;
begin
  item := pop_idle;
  item.data := hdr;
  item.next := large_hdr_idle;
  large_hdr_idle := item;
end;

function TThreadMemory.large_get_idle(var size: MSIZE): Pointer;
var
  block: PMemBlock;
  large: PMemLarge;
  curr, next, next_next: PMem;
  curr_size, next_size: MSIZE;
begin
  result := nil;
{$ifdef debug}
  mcheck_large(1, large_block, block_links[btLarge]);
{$endif}
  curr := block_links[btLarge];
  while curr <> nil do
  begin
    curr_size := curr.large.size;
    if curr_size >= size then
    begin
      block := curr.owner;
      next_size := curr_size - SIZE_HEADER - size;
      if next_size <= MAX_SIZE_MEDIUM then
      begin
        // del on link list
        del_link(curr);
      end else
      begin
        large := curr.large;
        large.size := size;
        next := Pointer(MADDR(curr) + SIZE_HEADER + size);
        next.flag := FLAG_BLOCK_LARGE;
        next.owner := block;
        next.large := pop_large_hdr;
        large := next.large;
        large.keep := true;
        large.size := next_size;
        large.prev := curr;
        next_next := Pointer(MADDR(next) + SIZE_HEADER + next_size);
        if MADDR(next_next) < MADDR(block.end_ptr) then
          next_next.large.prev := next;
        update_link(curr, next);
      end;
      curr.flag := FLAG_BLOCK_LARGE or FLAG_USED;
      result := MADDR(curr) + SIZE_HEADER;
      size := curr.large.size;
      break;
    end;
    curr := curr.link.link_next;
  end;
{$ifdef debug}
  mcheck_large(2, large_block, block_links[btLarge]);
{$endif}
end;

function TThreadMemory.large_mem_get(size: MSIZE): Pointer;
var
  curr, next: PMem;
  curr_size, next_size: MSIZE;
begin
{$ifdef debug}
  mcheck_large(20, large_block, block_links[btLarge]);
{$endif}
  result := large_get_idle(size);
{$ifopt c+}
  if result <> nil then
  begin
    curr := Pointer(MADDR(result) - SIZE_HEADER);
    if curr.large.keep then
      assert((MADDR(result) > MADDR(large_block)) and
        (MADDR(result) < MADDR(large_block.end_ptr)));
  end;
{$endif}
  if result = nil then
  begin
    curr := large_block.curr_mem;
    if curr <> nil then
      curr_size := curr.large.size
    else
      curr_size := 0;
    if (size > curr_size) or (size > SIZE_BLOCK) then
    begin
      size := size + SIZE_HEADER;
      curr := virtual_alloc(size);
      size := size - SIZE_HEADER;
      curr.flag := FLAG_BLOCK_LARGE or FLAG_USED;
      curr.owner := large_block;
      curr.large := pop_large_hdr;
      curr.large.keep := false;
      curr.large.size := size;
      curr.large.prev := nil;
    end else
    begin
      next_size := curr_size - SIZE_HEADER - size;
      if next_size < MAX_SIZE_MEDIUM then
      begin
        curr.flag := FLAG_BLOCK_LARGE or FLAG_USED;
        curr.large.size := curr_size;
        large_block.curr_mem := nil;
      end else
      begin
        curr.flag := FLAG_BLOCK_LARGE or FLAG_USED;
        curr.large.size := size;
        next := Pointer(MADDR(curr) + SIZE_HEADER + size);
        next.flag := FLAG_BLOCK_LARGE or FLAG_NONE;
        next.owner := large_block;
        next.large := pop_large_hdr;
        next.large.keep := true;
        next.large.size := next_size;
        next.large.prev := curr;
        large_block.curr_mem := next;
      end;
    end;
    result := MADDR(curr) + SIZE_HEADER;
  end;
  with status do
  begin
    total_alloc := total_alloc + size;
    large_alloc := large_alloc + size;
  end;
{$ifdef debug}
  mcheck_large(29, large_block, block_links[btLarge]);
{$endif}
end;

function TThreadMemory.large_mem_realloc(p: Pointer; new_size: MSIZE): Pointer;
var
  can_resize: Boolean;
  curr, next, new_next, next_next: PMem;
  next_size, next_flag, merge_size, resize_size: MSIZE;
begin
{$ifdef debug}
  mcheck_large(30, large_block, block_links[btLarge]);
{$endif}
  curr := p;
  next := Pointer(MADDR(curr) + SIZE_HEADER + curr.large.size);
  can_resize := (curr.large.keep) and
    (MADDR(next) < MADDR(large_block.end_ptr)) and
    ((next.flag and FLAG_MASK) in [FLAG_NONE, FLAG_LINK]) and
    ((curr.large.size + SIZE_HEADER + next.large.size) >= new_size);
  if can_resize then
  begin
    next_flag := next.flag;
    next_size := next.large.size;
    next_next := Pointer(MADDR(next) + SIZE_HEADER + next_size);
    if MADDR(next_next) >= MADDR(large_block.end_ptr) then
      next_next := nil;

    merge_size := curr.large.size + SIZE_HEADER + next_size;
    if (merge_size - SIZE_HEADER - new_size) >= MAX_SIZE_MEDIUM then
    begin
      resize_size := new_size - curr.large.size;
      curr.large.size := new_size;
      next_size := merge_size - SIZE_HEADER - new_size;

      new_next := Pointer(MADDR(curr) + SIZE_HEADER + new_size);
      new_next.flag := next_flag;
      new_next.owner := large_block;
      new_next.large := pop_large_hdr;
      new_next.large.keep := true;
      new_next.large.size := next_size;
      new_next.large.prev := curr;

      if next_next <> nil then
        next_next.large.prev := new_next;
      if next_flag and FLAG_MASK = FLAG_NONE then
      begin
        if large_block.curr_mem <> nil then
          push_large_hdr(large_block.curr_mem.large);
        large_block.curr_mem := new_next;
      end else
      begin
        push_large_hdr(next.large);
        update_link(next, new_next);
      end;
    end else
    begin
      resize_size := SIZE_HEADER + next_size;
      curr.large.size := merge_size;
      push_large_hdr(next.large);
      if next_flag and FLAG_MASK = FLAG_LINK then
        del_link(next)
      else
        large_block.curr_mem := nil;
      if next_next <> nil then
        next_next.large.prev := curr;
    end;
    result := MADDR(curr) + SIZE_HEADER;
    with status do
    begin
      total_alloc := total_alloc + resize_size;
      large_alloc := large_alloc + resize_size;
    end;
  end else
  begin
    // check: if not mem_keep then virtual_realloc(next)
    result := large_mem_get(new_size);
    move((MADDR(curr) + SIZE_HEADER)^, result^, curr.large.size);
    large_mem_free(curr);
  end;
{$ifdef debug}
  mcheck_large(31, large_block, block_links[btLarge]);
{$endif}
end;

function TThreadMemory.large_mem_free(p: Pointer): Integer;

  procedure large_merge_block(curr: PMem);
  var
    prev, next, next_next: PMem;
    curr_size, next_size, next_flag: MSIZE;
  begin
    next := Pointer(MADDR(curr) + SIZE_HEADER + curr.large.size);
    if MADDR(next) < MADDR(large_block.end_ptr) then
    begin
    {$ifopt c+}
      assert(curr = next.large.prev, 1);
    {$endif}
      next_flag := next.flag and FLAG_MASK;
      if next_flag in [FLAG_NONE, FLAG_LINK] then
      begin
        next_size := next.large.size;
        next_next := Pointer(MADDR(next) + SIZE_HEADER + next_size);
        if MADDR(next_next) < MADDR(large_block.end_ptr) then
          next_next.large.prev := curr;
        curr.large.size := curr.large.size + SIZE_HEADER + next_size;
        push_large_hdr(next.large);
        del_link(next);
        if large_block.curr_mem = next then
        begin
        {$ifopt c+}
          assert(next_flag = FLAG_NONE);
        {$endif}
          large_block.curr_mem := curr;
        end;
      end;
    end;

    prev := curr.large.prev;
    if prev <> nil then
    begin
    {$ifopt c+}
      assert((MADDR(prev) + SIZE_HEADER + prev.large.size) = MADDR(curr), 2);
    {$endif}
      if prev.flag and FLAG_LINK = FLAG_LINK then
      begin
        curr_size := curr.large.size;
        next := Pointer(MADDR(curr) + SIZE_HEADER + curr_size);
        if MADDR(next) < MADDR(large_block.end_ptr) then
          next.large.prev := prev;
        push_large_hdr(curr.large);
        del_link(curr);
        prev.large.size := prev.large.size + SIZE_HEADER + curr_size;
        if large_block.curr_mem = curr then
        begin
          del_link(prev);
          large_block.curr_mem := prev;
        end;
      end;
    end;
  end;

var
  curr: PMem;
  free_size: MSIZE;
begin
{$ifdef debug}
  mcheck_large(40, large_block, block_links[btLarge]);
{$endif}
  result := 0;
  curr := p;
  assert(curr.flag and FLAG_BLOCK_LARGE = FLAG_BLOCK_LARGE);
  free_size := curr.large.size;
  if curr.large.keep then
  begin
    add_link(curr);
    large_merge_block(curr);
  end else
  begin
    push_large_hdr(curr.large);
    virtual_free(curr);
  end;
  with status do
  begin
    total_free := total_free + free_size;
    large_free := large_free + free_size;
  end;
{$ifdef debug}
  mcheck_large(41, large_block, block_links[btLarge]);
{$endif}
end;

function TThreadMemory.pop_idle: PLinkData;
var
  size: MSIZE;
  item: PLinkData;
  item_count: Integer;
  buffer: PFreeItemBuffer;
begin
  if item_idle = nil then
  begin
    size := sizeof(TFreeItemBuffer);
    buffer := virtual_alloc(size);
    buffer.next := item_buffer;
    item_buffer := buffer;
    item := @buffer.items[0];
    item_count := (size - sizeof(Pointer)) div sizeof(TLinkData);
    while item_count > 0 do
    begin
      item.next := item_idle;
      item_idle := item;
      inc(item);
      dec(item_count);
    end;
  end;
  result := item_idle;
  item_idle := item_idle.next;
end;

procedure TThreadMemory.push_idle(item: PLinkData);
begin
  item.next := item_idle;
  item_idle := item;
end;

procedure TThreadMemory.release_block(block: PMemBlock);
var
  mem: PMem;
  mem_size: MSIZE;
  idle: PLinkData;
  block_ptr: ^PMemBlock;
begin
  assert(block <> nil);
  mem := block.src_ptr;
  case mem.flag and FLAG_BLOCK_MASK of
    FLAG_BLOCK_MEDIUM:
    begin
      mem_size := mem.flag and FLAG_SIZE shr FLAG_BIT;
      if mem_size < block.src_len then exit;
      if mem.flag and FLAG_LINK = FLAG_LINK then
        del_link(mem);
      block_ptr := @medium_block;
    end;
    FLAG_BLOCK_LARGE:
    begin
      if mem.large.size < block.src_len then exit;
      push_large_hdr(mem.large);
      if mem.flag and FLAG_LINK = FLAG_LINK then
        del_link(mem);
      block_ptr := @large_block;
    end;
  else
    exit;
  end;

  block.curr_mem := block.src_ptr;
  block.curr_mem.prev := nil;
  block.curr_mem.flag := 0;
  if block.prev = nil then
  begin
    block_ptr^ := block.next;
    if (block.next <> nil) then
      block.next.prev := nil;
  end else
  begin
    assert((block <> nil) and( block.prev <> nil));
    block.prev.next := block.next;
    if block.next <> nil then
      block.next.prev := block.prev;
  end;

  if block_buffer_count < MAX_BLOCK_COUNT then
  begin
    idle := pop_idle;
    idle.data := block;
    idle.next := block_buffer;
    block_buffer := idle;
    inc(block_buffer_count);
  end else
    owner.release_block(block);
end;

function TThreadMemory.create_block(btype: TBlockType): Pointer;
var
  mem: PMem;
  idle: PLinkData;
  new_block: PMemBlock;
  block_ptr: ^PMemBlock;
  size, block_flag: MSIZE;
begin
  result := nil;
  if not initialized then exit;
  case btype of
    btMedium:
    begin
      block_flag := FLAG_BLOCK_MEDIUM;
      block_ptr := @medium_block;
    end;
    btLarge:
    begin
      block_flag := FLAG_BLOCK_LARGE;
      block_ptr := @large_block;
    end;
  else
    exit;
  end;

  if block_buffer <> nil then
  begin
    idle := block_buffer;
    block_buffer := idle.next;
    result := idle.data;
    push_idle(idle);
    dec(block_buffer_count);
    size := SIZE_BLOCK;
  end else
    result := owner.create_block(size);
  new_block := result;
  new_block.owner := @self;
  new_block.src_ptr := Pointer(MADDR(new_block) + sizeof(TMemBlock));
  new_block.end_ptr := Pointer(MADDR(new_block) + size);
  new_block.src_len := size - sizeof(TMemBlock) - SIZE_HEADER;
  new_block.btype := btype;
  new_block.bflag := block_flag;
  new_block.curr_mem := new_block.src_ptr;
  mem := new_block.src_ptr;
  mem.owner := new_block;
  if btype <> btLarge then
  begin
    mem.prev := nil;
    mem.flag := (block_flag) + (new_block.src_len shl FLAG_BIT);
  end else
  begin
    mem.flag := block_flag;
    mem.large := pop_large_hdr;
    mem.large.keep := true;
    mem.large.size := new_block.src_len;
    mem.large.prev := nil;
  end;
  if block_ptr^ <> nil then
    block_ptr^.prev := new_block;
  new_block.next := block_ptr^;
  block_ptr^ := new_block;
end;

function TThreadMemory.memory_get(size: MSIZE): Pointer;
begin
  if other_thread_free_lists <> nil then
    do_freemem_from_other_thread;
  size := _align_mem_size(size);
{$ifopt c+}
  assert(size > 0);
{$endif}
  if size < MAX_SIZE_MINI then
  begin
    result := mem_item_get(mini_block, size shl 1);
  end else
  if size < MAX_SIZE_SMALL then
  begin
    result := mem_item_get(small_block, size + (size shr 2));
  end else
  if size < MAX_SIZE_MEDIUM then
  begin
    result := medium_mem_get(size + (size shr 2));
  end else
  begin
    if large_block = nil then
      large_block := create_block(btLarge);
    result := large_mem_get(size + MAX_SIZE_SMALL);
  end;
{$ifdef debug}
  //mcheck_block(1, PMem(MADDR(result) - SIZE_HEADER).owner);
{$endif}
end;

function TThreadMemory.memory_realloc(p: Pointer; new_size: MSIZE): Pointer;
var
  curr: PMem;
begin
  if other_thread_free_lists <> nil then
    do_freemem_from_other_thread;

  curr := Pointer(MADDR(p) - SIZE_HEADER);
  if curr.flag and FLAG_USED = FLAG_USED then
  begin
    new_size := _align_mem_size(new_size);
    case curr.flag and FLAG_BLOCK_MASK of
      FLAG_BLOCK_MINI:
        result := mem_item_realloc(curr, new_size shl 1);
      FLAG_BLOCK_SMALL:
        result := mem_item_realloc(curr, new_size + (new_size shr 1));
      FLAG_BLOCK_MEDIUM:
        result := medium_mem_realloc(curr, new_size + (new_size shr 2));
      FLAG_BLOCK_LARGE:
        result := large_mem_realloc(curr, new_size + MAX_SIZE_SMALL);
    else
      result := p;
    end;
  end else
  begin
    result := p;
  {$ifdef debug}
    mprint('realloc.invalid pointer: %.8X, flag: %.8X, size: $%.8X',
      [Integer(curr), curr.flag, curr.flag and FLAG_SIZE shr FLAG_BIT]);
  {$endif}
    System.Error(reInvalidPtr);
  end;

{$ifdef debug}
  mcheck_block(100, PMem(MADDR(result) - SIZE_HEADER).owner);
{$endif}
end;

function TThreadMemory.memory_free(p: Pointer): Integer;
var
  curr: PMem;
begin
  result := 0;
  if other_thread_free_lists <> nil then
    do_freemem_from_other_thread;

  curr := Pointer(MADDR(p) - SIZE_HEADER);
  if curr.flag and FLAG_USED = FLAG_USED then
  begin
    case curr.flag and FLAG_BLOCK_MASK of
      FLAG_BLOCK_MINI, FLAG_BLOCK_SMALL:
        result := mem_item_free(curr);
      FLAG_BLOCK_MEDIUM:
        result := medium_mem_free(curr);
      FLAG_BLOCK_LARGE:
        result := large_mem_free(curr);
    end;
  end else
  begin
  {$ifdef debug}
    mprint('free.invalid pointer: %.8X, flag: %.8X, size: $%.8X',
      [Integer(curr), curr.flag, curr.flag and FLAG_SIZE shr FLAG_BIT]);
  {$endif}
    System.Error(reInvalidPtr);
  end;
end;

{ TMemManager }

var
  mem_mgr: TMemManager;

{$ifdef tls_mode}
var
  tls_index: Cardinal;
{$else}
threadvar
  local_thread_memory: PThreadMemory;
{$endif}

{$ifdef has_thread_exit}
var
  prev_end_thread: procedure(code: Integer);

procedure qmm_end_thread(code: Integer);
begin
  mem_mgr.release_thread_memory(GetCurrentThreadId);
  if assigned(prev_end_thread) then
    prev_end_thread(code);
end;

{$else}

var
  old_api_thread_exit: TJump;
  old_sys_thread_exit: TJump;

procedure api_thread_exit(code: Integer); stdcall;
begin
  mem_mgr.release_thread_memory(GetCurrentThreadId);
  // nothing to do
end;

procedure sys_thread_exit(code: Integer);
begin
  mem_mgr.release_thread_memory(GetCurrentThreadId);
  // nothing to do
end;

function _replace_function(src, dest: Pointer; var old_jump: TJump): Boolean;
const
  OP_REPLACE = $E9;
var
  jump: PJump;
  old_protect: Cardinal;
begin
  result := VirtualProtect(src, sizeof(TJump), PAGE_EXECUTE_READWRITE, old_protect);
  if not result then exit;

  jump := PJump(src);
  if jump.OpCode <> OP_REPLACE then
  begin
    old_jump := jump^;
    // jmp <Displacement>        jmp -$00001234
    jump^.OpCode   := OP_REPLACE;
    jump^.Distance := MADDR(dest) - MADDR(src) - sizeof(TJump);
    FlushInstructionCache(GetCurrentProcess, src, sizeof(TJump));
  end;
  VirtualProtect(src, sizeof(TJump), old_protect, old_protect);
end;

procedure _restore_function(func: Pointer; var old_jump: TJump);
var
  old_protect: Cardinal;
begin
  if old_jump.OpCode = 0 then exit;
  if VirtualProtect(func, sizeof(TJump), PAGE_EXECUTE_READWRITE, old_protect) then
  begin
    PJump(func)^ := old_jump;
    fillchar(old_jump, sizeof(TJump), #0);
    FlushInstructionCache(GetCurrentProcess, func, SizeOf(TJump));
  end;
  VirtualProtect(func, sizeof(TJump), old_protect, old_protect);
end;
{$endif}

procedure TMemManager.initialize;
begin
  fillchar(self, sizeof(self), #0);
  spinlock_init(@lock);
  spinlock_init(@block_lock);
{$ifdef tls_mode}
  tls_index := Windows.TlsAlloc;
{$endif}

{$ifndef has_thread_exit}
  _replace_function(@Windows.ExitThread, @api_thread_exit, old_api_thread_exit);
  _replace_function(@System.EndThread, @sys_thread_exit, old_sys_thread_exit);
{$else}
  prev_end_thread := SystemThreadEndProc;
  SystemThreadEndProc := qmm_end_thread;
{$endif}
  main_mgr := create_thread_memory(MainThreadID);
  initialized := true;
end;

procedure TMemManager.uninitialize;
var
  count: Integer;
  next: PLinkData;
  thread_memory: PThreadMemory;
begin
  initialized := false;
  while mem_buffer <> nil do
  begin
    next := mem_buffer.next;
    count := roundup_pow_of_two(PER_THREAD_BUFFER_COUNT * sizeof(TThreadMemory)) div sizeof(TThreadMemory);
    thread_memory := mem_buffer.data;
    while count > 0 do
    begin
      if thread_memory.initialized then
        thread_memory.uninitialize;
      inc(thread_memory);
      dec(count);
    end;
    virtual_free(mem_buffer.data);
    mem_buffer := next;
  end;
  while block_buffer <> nil do
  begin
    next := block_buffer.next;
    virtual_free(block_buffer.data);
    block_buffer := next;
  end;
  while link_buffer <> nil do
  begin
    next := link_buffer.next;
    virtual_free(link_buffer.data);
    link_buffer := next;
  end;
{$ifdef tls_mode}
  tlsFree(tls_index);
{$endif}
{$ifndef has_thread_exit}
  _restore_function(@Windows.ExitThread, old_api_thread_exit);
  _restore_function(@System.EndThread, old_sys_thread_exit);
{$else}
  SystemThreadEndProc := prev_end_thread;
{$endif}
  fillchar(self, sizeof(self), #0);
end;

procedure TMemManager.release_block(block: PMemBlock);
var
  link: PLinkData;
begin
  if block_buffer_count < MAX_BLOCK_COUNT then
  begin
    spinlock_lock(@block_lock);
    link := pop_link;
    link.data := block;
    link.next := block_buffer;
    block_buffer := link;
    inc(block_buffer_count);
    spinlock_unlock(@block_lock);
  end else
  begin
    virtual_free(block);
  end;
end;

function TMemManager.create_block(var size: MSIZE): Pointer;
var
  link: PLinkData;
begin
  size := SIZE_BLOCK;
  if block_buffer <> nil then
  begin
    spinlock_lock(@block_lock);
    link := block_buffer;
    block_buffer := link.next;
    result := link.data;
    push_link(link);
    dec(block_buffer_count);
    spinlock_unlock(@block_lock);
  end else
  begin
    result := virtual_alloc(size);
  end;
end;

procedure TMemManager.create_link_buffer;
var
  addr: Pointer;
  link: PLinkData;
  size, count: MSIZE;
begin
  size := MEM_PAGE_SIZE;
  addr := virtual_alloc(size);
  link := addr;
  count := size div sizeof(TLinkData);
  while count > 0 do
  begin
    link.next := link_idle;
    link_idle := link;
    inc(link);
    dec(count);
  end;
  link := link_idle;
  link_idle := link.next;
  link.data := addr;
  link.next := link_buffer;
  link_buffer := link;
end;

function TMemManager.pop_link: PLinkData;
begin
  if link_idle = nil then
    create_link_buffer;
  result := link_idle;
  link_idle := result.next;
end;

procedure TMemManager.push_link(data: PLinkData);
begin
  data.next := link_idle;
  link_idle := data;
end;

procedure TMemManager.create_thread_memory_buffer;
var
  addr: Pointer;
  link: PLinkData;
  size, count: MSIZE;
  thread: PThreadMemory;
begin
  size := PER_THREAD_BUFFER_COUNT * sizeof(TThreadMemory);
  addr := virtual_alloc(size);
  link := pop_link;
  link.data := addr;
  link.next := mem_buffer;
  mem_buffer := link;

  count := size div sizeof(TThreadMemory);
  thread := addr;
  while count > 0 do
  begin
    link := pop_link;
    link.data := thread;
    link.next := mem_idle;
    mem_idle := link;
    inc(thread);
    dec(count);
  end;
end;

function TMemManager.pop_thread_memory: PThreadMemory;
var
  link: PLinkData;
begin
  spinlock_lock(@lock);
  if mem_idle = nil then
    create_thread_memory_buffer;
  link := mem_idle;
  mem_idle := link.next;
  result := link.data;
  push_link(link);
  spinlock_unlock(@lock);
end;

procedure TMemManager.push_thread_memory(thread_memory: PThreadMemory);
var
  link: PLinkData;
begin
  spinlock_lock(@lock);
  link := pop_link;
  link.data := thread_memory;
  link.next := mem_idle;
  mem_idle := link;
  spinlock_unlock(@lock);
end;

function TMemManager.create_thread_memory(thread_id: Cardinal): PThreadMemory;
var
  is_exists: Boolean;
  mem_ptr: ^PThreadMemory;
  thread_memory: PThreadMemory;
begin
  result := nil;
  if not initialized then exit;
  mem_ptr := @mem_mgrs[thread_id and (HASH_SIZE_THREAD_MGR - 1)];
  is_exists := false;
  if mem_ptr^ <> nil then
  begin
    thread_memory := mem_ptr^;
    while thread_memory <> nil do
    begin
      is_exists := thread_memory.thread_id = thread_id;
      if is_exists then
      begin
        result := thread_memory;
        break;
      end;
      thread_memory := thread_memory.next_thread_memory;
    end;
  end;

  if not is_exists then
  begin
    spinlock_lock(@lock);
    inc(thread_count);
    result := pop_thread_memory;
    result.next_thread_memory := mem_ptr^;
    mem_ptr^ := result;
    spinlock_unlock(@lock);

    if not result.initialized then
      result.initialize(@self);
    result.reactive(thread_id);
  {$ifdef tls_mode}
    tlsSetValue(tls_index, result);
  {$else}
    local_thread_memory := result;
  {$endif}
  end;
end;

procedure TMemManager.release_thread_memory(thread_id: Cardinal);
var
  mem_ptr: ^PThreadMemory;
  thread_memory, prev_thread_memory: PThreadMemory;
begin
  if not initialized then exit;

  spinlock_lock(@lock);
  try
    mem_ptr := @mem_mgrs[thread_id and (HASH_SIZE_THREAD_MGR - 1)];
    thread_memory := mem_ptr^;
    prev_thread_memory := nil;
    while thread_memory <> nil do
    begin
      if thread_memory.thread_id = thread_id then
      begin
        dec(thread_count);
        if prev_thread_memory = nil then
          mem_ptr^ := thread_memory.next_thread_memory
        else
          prev_thread_memory.next_thread_memory := thread_memory.next_thread_memory;
        thread_memory.next_thread_memory := nil;
        thread_memory.deactive();
        push_thread_memory(thread_memory);
        break;
      end;
      prev_thread_memory := thread_memory;
      thread_memory := thread_memory.next_thread_memory;
    end;
  {$ifdef tls_mode}
    tlsSetValue(tls_index, nil);
  {$else}
    local_thread_memory := nil;
  {$endif}
    // if only main thread, then ...
    if thread_count = 1 then
      System.IsMultiThread := false;
  finally
    spinlock_unlock(@lock);
  end;
end;

function TMemManager.get_thread_memory(): PThreadMemory;
begin
{$ifdef tls_mode}
  result := tlsGetValue(tls_index);
{$else}
  result := local_thread_memory;
{$endif}
  if (result = nil) then
    result := create_thread_memory(GetCurrentThreadId);
end;

function TMemManager.is_register_leak(address: Pointer): Boolean;
var
  link: PLinkData;
begin
  result := false;
  link := leak_list;
  while not result and (link <> nil) do
  begin
    if link.data = address then
    begin
      result := true;
      break;
    end;
    link := link.next;
  end;
end;

function TMemManager.register_leak(address: Pointer): Boolean;
var
  link: PLinkData;
begin
  result := true;
  spinlock_lock(@lock);
  try
    link := pop_link;
    link.data := address;
    link.next := leak_list;
    leak_list := link;
  finally
    spinlock_unlock(@lock);
  end;
end;

function TMemManager.unregister_leak(address: Pointer): Boolean;
var
  link, prev: PLinkData;
begin
  result := false;
  spinlock_lock(@lock);
  try
    link := leak_list;
    prev := nil;
    while not result and (link <> nil) do
    begin
      if link.data = address then
      begin
        if prev = nil then
          leak_list := link.next
        else
          prev.next := link.next;
        push_link(link);
        result := true;
        break;
      end;
      prev := link;
      link := link.next;
    end;
  finally
    spinlock_lock(@lock);
  end;
end;

function memory_get(size: MSIZE): Pointer;
begin
  result := mem_mgr.get_thread_memory.memory_get(size);
end;

function memory_alloc(size: MSIZE): Pointer;
begin
  result := memory_get(size);
  if result <> nil then
    fillchar(result^, size, #0);
end;

function memory_realloc(p: Pointer; size: MSIZE): Pointer;
var
  curr: PMem;
  old_size: MSIZE;
  curr_thread: PThreadMemory;
begin
  if size > 0 then
  begin
    if p <> nil then
    begin
      result := p;
      curr := Pointer(MADDR(p) - SIZE_HEADER);
      if curr.flag > 0 then
      begin
        old_size := curr.flag and FLAG_SIZE shr FLAG_BIT;
        if size <= old_size then exit;
      end
      else
      begin
        old_size := curr.large.size;
        if size <= old_size then exit;
      end;
    {$ifopt c+}
      assert((curr.owner <> nil) and (curr.owner.owner <> nil));
    {$endif}
      curr_thread := mem_mgr.get_thread_memory;
      if curr_thread = curr.owner.owner then
        result := curr_thread.memory_realloc(p, size)
      else
      begin
        result := curr_thread.memory_get(size);
        if old_size > size then
          old_size := size;
        move(p^, result^, old_size);
        PThreadMemory(curr.owner.owner).freemem_by_other_thread(p);
      end;
    end else
    begin
      result := mem_mgr.get_thread_memory.memory_get(size);
    end;
  end else
  begin
    if p <> nil then
    begin
      curr := Pointer(MADDR(p) - SIZE_HEADER);
      curr_thread := mem_mgr.get_thread_memory;
      if curr_thread = curr.owner.owner then
        curr_thread.memory_free(p)
      else
        PThreadMemory(curr.owner.owner).freemem_by_other_thread(p);
    end;
    result := nil;
  end;
end;

function memory_free(p: Pointer): Integer;
var
  curr: PMem;
  curr_thread: PThreadMemory;
begin
  result := 0;
  if p = nil then exit;
  curr_thread := mem_mgr.get_thread_memory;
  curr := Pointer(MADDR(p) - SIZE_HEADER);
{$ifopt c+}
  assert((curr.owner <> nil) and (curr.owner.owner <> nil));
{$endif}
  if curr_thread = curr.owner.owner then
    curr_thread.memory_free(p)
  else
  begin
    PThreadMemory(curr.owner.owner).freemem_by_other_thread(p);
  {$ifdef debug}
    // for debug_memory_free
    result := 1;
  {$endif}
  end;
end;

{$ifdef debug}
type
  PDebugMem = ^TDebugMem;
  TDebugMem = record
    ori_size: MSIZE;
    first_tag: Cardinal;
    last_tag: PCardinal;
  end;

const
  TAG_FIRST = $FEEBFCDF;
  TAG_LAST =  $EBEFFEFB;

function debug_memory_get(size: MSIZE): Pointer;
var
  mem: PDebugMem;
begin
  mem := memory_get(size + sizeof(TDebugMem) + sizeof(Cardinal));
{$ifopt c+}
  assert(mem <> nil);
{$endif}
  mem.ori_size := size;
  mem.first_tag := TAG_FIRST;
  mem.last_tag := Pointer(MADDR(mem) + sizeof(TDebugMem) + size);
  mem.last_tag^ := TAG_LAST;
  result := MADDR(mem) + sizeof(TDebugMem);
  if assigned(on_notify_get_proc) then
    on_notify_get_proc(result, size);
end;

function debug_memory_alloc(size: MSIZE): Pointer;
begin
  result := debug_memory_get(size);
  if result <> nil then
    fillchar(result^, size, #0);
end;

function debug_memory_check(op: TDebugMemOP; mem: PDebugMem): Boolean;
begin
  result := (mem = nil) or ((mem.first_tag = TAG_FIRST) and
    (MADDR(mem.last_tag) = MADDR(mem) + sizeof(TDebugMem) + mem.ori_size) and
    (mem.last_tag^ = TAG_LAST));
  if not result then
  begin
    //memory_log();
    if not assigned(on_memory_error_proc) then
    begin
      sleep(0);
      asm int 3 end;
    end else
      on_memory_error_proc(op, MADDR(mem) + sizeof(TDebugMem), mem.ori_size);
  end;
end;

function debug_memory_realloc(p: Pointer; size: MSIZE): Pointer;
var
  ori_size: MSIZE;
  ori_mem, new_mem: PDebugMem;
  last_tag: Pointer;
begin
  if p <> nil then
  begin
    ori_mem := Pointer(MADDR(p) - sizeof(TDebugMem));
    ori_size := ori_mem.ori_size;
    debug_memory_check(opRealloc, ori_mem);
    last_tag := ori_mem.last_tag;
    new_mem := memory_realloc(ori_mem, size + sizeof(TDebugMem) + sizeof(Cardinal));
    if (last_tag <> new_mem.last_tag) or (new_mem.first_tag <> TAG_FIRST) then
    begin
      assert(false);
    end;
  end else
  begin
    ori_size := 0;
    new_mem := memory_get(size + sizeof(TDebugMem) + sizeof(Cardinal));
  end;

  new_mem.ori_size := size;
  new_mem.first_tag := TAG_FIRST;
  new_mem.last_tag := Pointer(MADDR(new_mem)+ sizeof(TDebugMem) + size);
  new_mem.last_tag^ := TAG_LAST;
  result := MADDR(new_mem) + sizeof(TDebugMem);
  if assigned(on_notify_realloc_proc) then
    on_notify_realloc_proc(p, ori_size, result, size);
end;

function debug_memory_free(p: Pointer): Integer;
var
  curr: PMem;
  mem: PDebugMem;
  mem_keep: Boolean;
  size: MSIZE;
begin
  result := 0;
  if p = nil then exit;
  mem := Pointer(MADDR(p) - sizeof(TDebugMem));
  debug_memory_check(opFree, mem);
  curr := PMem(MADDR(mem) - SIZE_HEADER);
  mem_keep := (curr.flag and FLAG_BLOCK_MASK <> FLAG_BLOCK_LARGE);
  size := mem.ori_size;
  if (memory_free(mem) = 0) and mem_keep then
  begin
    mem.ori_size := 0;
    mem.first_tag := 0;
    mem.last_tag^ := 0;
  end;
  if assigned(on_notify_free_proc) then
    on_notify_free_proc(p, size);
end;
{$endif}

function memory_register_leak(p: Pointer): Boolean;
begin
  result := mem_mgr.register_leak(p);
end;

function memory_unregister_leak(p: Pointer): Boolean;
begin
  result := mem_mgr.unregister_leak(p);
end;

function memory_status: TMemoryStatus;
var
  i: Integer;
  link: PLinkData;
  thread_memory: PThreadMemory;
begin
  fillchar(result, sizeof(result), #0);
  for i := 0 to HASH_SIZE_THREAD_MGR - 1 do
  begin
    thread_memory := mem_mgr.mem_mgrs[i];
    while thread_memory <> nil do
    begin
      if thread_memory.initialized then
        with thread_memory.status do
        begin
          result.total_alloc := result.total_alloc + total_alloc;
          result.total_free := result.total_free + total_free;
          result.small_alloc := result.small_alloc + small_alloc;
          result.small_free := result.small_free + small_free;
          result.medium_alloc := result.medium_alloc + medium_alloc;
          result.medium_free := result.medium_free + medium_free;
          result.large_alloc := result.large_alloc + large_alloc;
          result.large_free := result.large_free + large_free;
          //inc(result.small_block_count, small_block_count);
          //inc(result.medium_block_count, medium_block_count);
          //inc(result.large_block_count, large_block_count);
        end;
      thread_memory := thread_memory.next_thread_memory;
    end;
  end;
  link := mem_mgr.mem_idle;
  while link <> nil do
  begin
    thread_memory := link.data;
    if not thread_memory.initialized then
      break;
    with thread_memory.status do
    begin
      result.total_alloc := result.total_alloc + total_alloc;
      result.total_free := result.total_free + total_free;
      result.small_alloc := result.small_alloc + small_alloc;
      result.small_free := result.small_free + small_free;
      result.medium_alloc := result.medium_alloc + medium_alloc;
      result.medium_free := result.medium_free + medium_free;
      result.large_alloc := result.large_alloc + large_alloc;
      result.large_free := result.large_free + large_free;
      //inc(result.small_block_count, small_block_count);
      //inc(result.medium_block_count, medium_block_count);
      //inc(result.large_block_count, large_block_count);
    end;
    link := link.next;
  end;
end;

{$ifdef debug}
type
  TStringType = (stUnknow, stAnsi, stUnicode);
  PStrRec = ^StrRec;
  StrRec = packed record
  {$ifdef WIN64}
    _padding: Integer;
  {$endif}
  {$if CompilerVersion >= 20}
    code: Word;
    char_size: Word;
  {$ifend}
    ref_cnt: Integer;
    len: Integer;
  end;

// code from FastMM4.pas.DetectStringData
function detect_string_type(mem: Pointer; mem_size: Integer): TStringType;
var
  rec: PStrRec;
  char_size, str_len: Integer;
begin
  result := stUnknow;
  rec := mem;
  if (rec.ref_cnt > 255) or (rec.len <= 0) then exit;
{$if CompilerVersion >= 20}
  char_size := rec.char_size;
  if not (char_size in [1, 2]) then exit;
{$else}
  char_size := 1;
{$ifend}
  str_len := rec.len;
  if str_len > ((mem_size - sizeof(StrRec)) div char_size) then exit;
  if char_size = 1 then
  begin
    if (PAnsiChar(mem) + sizeof(StrRec) + str_len)^ = #0 then
      result := stAnsi
  end else
  begin
    if PWideChar(MADDR(mem) + sizeof(StrRec) + str_len * char_size)^ = #0 then
      result := stUnicode;
  end;
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
end;

procedure get_file(path: PAnsiChar; size: Integer; filename: PAnsichar);
var
  val, flen: Integer;
begin
  fillchar(path[0], size, #0);
  val := GetModuleFileNameA(HInstance, path, size) - 4;
  while val > 0 do
  begin
    dec(val);
    if path[val] in ['\', '/'] then
      break;
  end;
  path[val + 1] := #0;
  flen := lstrlenA(filename);
  lstrcatA(path, filename);
  path[val + flen + 1] := #0;
end;

procedure report_memory_leak_to_file();
var
  log_file: THandle;
  log_val, log_len: Integer;
  log_buf: array [0..$FFFF] of AnsiChar;

  procedure report_file_open;
  var
    report_file: array [0..MAX_PATH - 1] of AnsiChar;
  begin
    if log_file = INVALID_HANDLE_VALUE then exit;
    if log_file = 0 then
    begin
      get_file(report_file, MAX_PATH, 'memory.leak.txt');
      log_file := CreateFileA(report_file, GENERIC_WRITE,
        FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      if log_file = INVALID_HANDLE_VALUE then exit;
      SetFilePointer(log_file, 0, nil, FILE_END);
    end;
  end;

  procedure report_write(buf: Pointer; size: Integer);
  var
    bytes: Cardinal;
  begin
    report_file_open;
    if ((log_val + size + 2) > log_len) or (size = 0)  then
    begin
      WriteFile(log_file, log_buf[0], log_val, bytes, nil);
      log_val := 0;;
    end;
    if size > 0 then
    begin
      move(buf^, (log_buf + log_val)^, size);
      inc(log_val, size);
      if not ((log_buf + log_val - 1)^ in [#13, #10]) then
      begin
        (log_buf + log_val)^ := #13;
        (log_buf + log_val + 1)^ := #10;
        inc(log_val, 2);
      end;
    end;
  end;

  procedure report_format(format: PAnsiChar;
    const argv: array of const);
  var
    i, buf_val: Integer;
    params: array [0..31] of Cardinal;
    buffer: array [0..1023] of AnsiChar;
  begin
    if Length(argv) > 0 then
    begin
      for i := low(argv) to high(argv) do
        params[i] := argv[i].VInteger;
      buf_val := wvsprintfA(buffer, format, @params);
    end else
    begin
      fillchar(buffer, sizeof(buffer), #0);
      lstrcatA(buffer, format);
      buf_val := lstrlenA(buffer);
    end;
    report_write(@buffer, buf_val);
  end;

var
  is_log_start, is_log_threadid: Boolean;
  curr_thread_id, last_thread_id: Cardinal;
  leak_index: Integer;

  procedure report_leak(block_index: Integer; mem: Pointer; mem_size: MSIZE);
  var
    ptr: PAnsiChar;
    str_rec: PStrRec;
    instance_class: TClass;
    curr_mem: Pointer;
    debug_mem: PDebugMem;
    buffer: array [0..1023] of AnsiChar;
  begin
    if not is_log_start then
    begin
      is_log_start := true;
      fillchar(buffer[0], 80, '=');
      buffer[80] := #0;
      report_format('%s'#13#10'This application has leaked memory(excluding expected leaks registered):',
        [Integer(@buffer[0])]);
      MessageBoxA(0, '!!The app has leaked memory, please see "memory.leak.txt"!!!', 'leaked', MB_ICONWARNING);
    end;
    if not is_log_threadid then
    begin
      if last_thread_id <> 0 then
      begin
        fillchar(buffer[0], 80, '-');
        buffer[80] := #0;
        report_format('%s', [Integer(@buffer[0])]);
      end;
      is_log_threadid := true;
      report_format('----------------main thread id: %d, current thread id: %.5d------------------',
        [MainThreadId, curr_thread_id]);
    end;

    inc(leak_index);
    report_format('block: %d, leak %d:', [block_index, leak_index]);
    debug_mem := mem;
    curr_mem := MADDR(debug_mem) + sizeof(TDebugMem);
    instance_class := detect_class(curr_mem);
    if instance_class <> nil then
    begin
      fillchar(buffer[0], sizeof(buffer), #0);
      ptr := Pointer(PPointer(Integer(curr_mem^) + vmtClassName)^);
      move((ptr + 1)^, buffer[0], Byte(ptr^));
      report_format('Class Name: %s, instance address: $%.8X, instance size: %d, mem size: %d', [
        Integer(@buffer[0]), Integer(curr_mem),
        PInteger(Integer(curr_mem^) + vmtInstanceSize)^, debug_mem.ori_size]);
    end else
    begin
      case detect_string_type(curr_mem, debug_mem.ori_size) of
        stUnknow:
        begin
          to_hex(curr_mem, debug_mem.ori_size, buffer, sizeof(buffer));
          report_format('unknow data: $%.8X, size: %d, data: '#13#10'%s', [
            Integer(curr_mem), debug_mem.ori_size, Integer(@buffer[0])]);
        end;
        stAnsi:
        begin
          str_rec := curr_mem;
          ptr := MADDR(curr_mem) + sizeof(StrRec);
          report_format('AnsiString: $%.8X, string len: %d, string:'#13#10'%s', [
            Integer(ptr), str_rec.len, Integer(ptr)]);
        end;
        stUnicode:
        begin
          str_rec := curr_mem;
          ptr := MADDR(curr_mem) + sizeof(StrRec);
          buffer[WideCharToMultiByte(CP_ACP, 0, PWideChar(ptr),
            str_rec.len, buffer, sizeof(buffer), nil, nil)] := #0;
          report_format('UnicodeString: $%.8X, string len: %d, string:'#13#10'%s', [
            Integer(ptr), str_rec.len, Integer(@buffer)]);
        end;
      end;
    end;
  end;

  procedure report_mem_items_block(block: PMemItemsBlock);
  var
    item_buf: PMemItemBuffer;
    item: PMemItem;
    items: PMemItems;
    debug_mem: PDebugMem;
    used_flag: MSIZE;
    i, item_step, item_count: Integer;
  begin
    // used: FLAG_USED
    // waitfor free: (FLAG_USED OR FLAG_WAITFORFREE)
    used_flag := FLAG_USED;
    for i := 0 to block.count - 1 do
    begin
      items := block.lists[i];
      item_step := items.item_size + SIZE_HEADER;
      item_buf := items.items_buffer;
      while item_buf <> nil do
      begin
        if item_buf.item_count <> item_buf.idle_count then
        begin
          item_count := item_buf.item_count;
          item := @item_buf.data[0];
          while item_count > 0 do
          begin
            if item.flag and FLAG_MASK = used_flag then
            begin
              debug_mem := Pointer(MADDR(item) + SIZE_HEADER);
              //owner := item.owner.owner;
              if not mem_mgr.is_register_leak(MADDR(debug_mem) + sizeof(TDebugMem)) then
              begin
              {$ifopt c+}
                //assert(debug_mem.first_tag = TAG_FIRST);
                //assert((debug_mem.last_tag <> nil) and (debug_mem.last_tag^ = TAG_FIRST));
                //assert(debug_mem.ori_size > MIN_MEM_SIZE);
              {$endif}
                report_leak(0, debug_mem, items.item_size);
              end;
            end;
            item := Pointer(MADDR(item) + item_step);
            dec(item_count);
          end;
        end;
        item_buf := item_buf.next;
      end;
    end;
  end;

  procedure report_block(block: PMemBlock);
  var
    owner: PThreadMemory;
    debug_mem: PDebugMem;
    start, curr, next: PMem;
    used_flag, used_len, curr_size: MSIZE;
  begin
    // used: FLAG_USED
    // waitfor free: (FLAG_USED OR FLAG_WAITFORFREE)
    used_flag := FLAG_USED;
    while (block <> nil) do
    begin
      start := block.src_ptr;
      curr := start;
      if block.curr_mem = nil then
        used_len := block.src_len
      else
        used_len := MADDR(block.curr_mem) - MADDR(start);
      while ((MADDR(curr) - MADDR(start)) <= used_len) do
      begin
        if curr.flag and FLAG_BLOCK_MASK = FLAG_BLOCK_LARGE then
          curr_size := curr.large.size
        else
          curr_size := curr.flag and FLAG_SIZE shr FLAG_BIT;
        next := Pointer(MADDR(curr) + SIZE_HEADER + curr_size);
        if curr.flag and FLAG_MASK = used_flag then
        begin
          debug_mem := Pointer(MADDR(curr) + SIZE_HEADER);
          owner := curr.owner.owner;
          if not (owner.is_register_leak(debug_mem) or
            mem_mgr.is_register_leak(MADDR(debug_mem) + sizeof(TDebugMem))) then
          begin
          {$ifopt c+}
            //assert(debug_mem.first_tag = TAG_FIRST);
            //assert((debug_mem.last_tag <> nil) and (debug_mem.last_tag^ = TAG_FIRST));
            //assert(debug_mem.ori_size > MIN_MEM_SIZE);
          {$endif}
            report_leak(ord(block.btype) + 1, debug_mem, curr_size);
          end;
        end;
        curr := next;
      end;
      block := block.next;
    end;
  end;

var
  i: Integer;
  thread_memory: PThreadMemory;
  buffer: array [0..MAX_PATH - 1] of AnsiChar;
begin
  log_file := 0;
  is_log_start := false;
  is_log_threadid := false;
  last_thread_id := 0;
  curr_thread_id := 0;
  try
    log_val := 0;
    log_len := sizeof(log_buf);
    for i := 0 to HASH_SIZE_THREAD_MGR - 1 do
    begin
      thread_memory := mem_mgr.mem_mgrs[i];
      while thread_memory <> nil do
      begin
        if thread_memory.initialized then
        begin
          if last_thread_id <> thread_memory.thread_id then
          begin
            leak_index := 0;
            if is_log_threadid and (curr_thread_id <> 0) then
              report_format('-------------------------------------------------------------------------------', []);
            is_log_threadid := false;
            last_thread_id := curr_thread_id;
            curr_thread_id := thread_memory.thread_id;
          end;
          report_mem_items_block(thread_memory.mini_block);
          report_mem_items_block(thread_memory.small_block);
          //report_block(thread_memory.small_block);
          report_block(thread_memory.medium_block);
          report_block(thread_memory.large_block);
          if is_log_threadid and (curr_thread_id <> 0) then
            report_format('--------------------------------------------------------------------------------', []);
        end;
        thread_memory := thread_memory.next_thread_memory;
      end;
    end;
    if is_log_start then
    begin
      fillchar(buffer[0], 77, '=');
      buffer[77] := #0;
      report_format('%sEND', [Integer(@buffer[0])]);
    end;
    if log_val > 0 then
      report_write(nil, 0);
  finally
    if (log_file <> 0) and (log_file <> INVALID_HANDLE_VALUE) then
      CloseHandle(log_file);
  end;
end;
{$endif}
{ init/uninit }

type
  PMM = ^TMM;
  TMM = System.{$ifdef has_mm_ex} TMemoryManagerEx {$else} TMemoryManager {$endif};

var
  old_mm, new_mm: TMM;

  is_qmm_set: Boolean = false;
  share_map_handle: THandle = 0;

//
//code from FastMM4
//
type
  PShareMessage = ^TShareMessage;
  TShareMessage = packed record
    mm_size: Cardinal;
    mm: Pointer;
  end;

procedure do_share_memmgr(var mm: TMM);
var
  pid: Cardinal;
  share: PShareMessage;
{$ifdef debug}
  out_string,
{$endif}
  share_name: array [0..63] of AnsiChar;
begin
  pid := GetCurrentProcessId;
  fillchar(share_name, sizeof(share_name), #0);
  wvsprintfA(share_name, 'share_qiu_mm_pid_%.8x', @pid);
  share_map_handle := OpenFileMappingA(FILE_MAP_READ, false, share_name);
  if share_map_handle = 0 then
  begin
    if not system.IsLibrary then
    begin
      share_map_handle := CreateFileMappingA(INVALID_HANDLE_VALUE, nil,
        PAGE_READWRITE, 0, sizeof(TShareMessage), share_name);
      share := MapViewOfFile(share_map_handle, FILE_MAP_WRITE, 0, 0, 0);
      share.mm_size := sizeof(TMM);
      share.mm := @mm;
      UnmapViewOfFile(share);
    {$ifdef debug}
      OutputDebugStringA('create share memory manager succed');
    {$endif}
    end else
    begin
    {$ifdef debug}
      OutputDebugStringA('DLL: cann''t open share map');
    {$endif}
    end;
  end else
  begin
    share := MapViewOfFile(share_map_handle, FILE_MAP_READ, 0, 0, 0);
    if share.mm_size = sizeof(TMM) then
    begin
      mm := PMM(share.mm)^;
    {$ifdef debug}
      OutputDebugStringA('read share memory manager succed');
    {$endif}
    end else
    begin
    {$ifdef debug}
      wvsprintfA(out_string, 'share mm fail: mm_size: %d <> sizeof(TMM)', @share.mm_size);
      OutputDebugStringA(out_string);
    {$endif}
    end;
    UnmapViewOfFile(share);
    CloseHandle(share_map_handle);
    share_map_handle := 0;
  end;
end;

procedure initialize_memory_manager;

  function get_allocated_size(): Int64;
{$ifdef has_mm_ex}
  var
    i: Integer;
    state: TMemoryManagerState;
  begin
    GetMemoryManagerState(state);
    result := state.TotalAllocatedMediumBlockSize + state.TotalAllocatedLargeBlockSize;
    for i := 0 to NumSmallBlockTypes - 1 do
      with state.SmallBlockTypeStates[i] do
        result := result + UseableBlockSize * AllocatedBlockCount;
  end;
{$else}
  begin
  {$warn symbol_platform off}
    result := GetHeapStatus.TotalAllocated;
  {$warn symbol_platform on}
  end;
{$endif}

begin
  mem_mgr.initialize;

  new_mm.GetMem := @{$ifndef debug}memory_get{$else}debug_memory_get{$endif};
  new_mm.FreeMem := @{$ifndef debug}memory_free{$else}debug_memory_free{$endif};
  new_mm.ReallocMem := @{$ifndef debug}memory_realloc{$else}debug_memory_realloc{$endif};
{$ifdef has_mm_ex}
  new_mm.AllocMem := @{$ifndef debug}memory_alloc{$else}debug_memory_alloc{$endif};
  new_mm.RegisterExpectedMemoryLeak := @memory_register_leak;
  new_mm.UnRegisterExpectedMemoryLeak := @memory_unregister_leak;
{$endif}

  do_share_memmgr(new_mm);

  if (is_qmm_set) or (System.IsMemoryManagerSet) or
    (get_allocated_size() > 0) then
    exit;

  GetMemoryManager(old_mm);
  SetMemoryManager(new_mm);
  is_qmm_set := true;
end;

procedure finalize_memory_manager;
begin
{$ifdef debug}
  if ReportMemoryLeaksOnShutdown then
    report_memory_leak_to_file();
{$endif}

  if is_qmm_set then
  begin
    SetMemoryManager(old_mm);
    is_qmm_set := false;
  end;

  if share_map_handle <> 0 then
  begin
    CloseHandle(share_map_handle);
    share_map_handle := 0;
  end;
  mem_mgr.uninitialize;
end;

initialization
  initialize_memory_manager;
finalization
  finalize_memory_manager;

end.
