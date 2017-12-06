{******************************************************************************/
* LocalHeapMM
* Local Heap Memory Manager
* (c) copyright 2003 by Carsten Zeumer
* last change 03/20/2003
*
*
* this code is compeltely free
* you may change, redistribute use or even sell (is some i willing to pay) it.
*
* oh.. and there is NO warranty of any kind.
*
* if you have comments or questions try this: carsten@task4ce.net
*
******************************************************************************/}

{
Abstract:
  Since Delphi offers you to substitute its default memory manager by some
  custom memory manager it come in handy to do so.
  This Memory Manager usese the Windows Local Heap Memory functions.
  In debug mode it additionally verboses all memory manipualtions and keeps
  track of allocated memory.
  The advantage of the Windows Heaps is, you can set an initial size and a
  maximum size. This can reduce memmory fragmentation and can come in handy if
  you need to simmulate low memory conditions.

Limitations:
  This Memory manager should not be used in DLLs which export data that is not
  freed by them selves (like strings!)

  A delphi memory manager only hooks into the GetMem, FreeMem and ReallocMem
  functions.
  Those functions are used by all VCL components to allocate memory for its
  private use. (including construction etc.)
   (this is why the statistics window is not using any VCL at all. it would
    end up in a endelss recursion)
  It does NOT hook into all other memory allocations! Most likely you will
  see that the actual amount of memory used by you program will differ from
  the heap size you set. this is beacuse of:
   - vcl components using different mechanism to allocate memory
      (i do not know of any examples, but you can never know!)
   - use of OLE Strings (BSTR) or Variants which are of the VT_BSTR type.
     BSTRs are allocated and freed by the SysAllocString and SysFreeString
     functions. Those functions are exported by the OLEAUT32.dll. This dll
     additionally caches strings (so it is possible that a BSTR you allocated
     and freed is still in memory!! un freed BSTR are REALY hard to track down!)
   - a call to an API that allocates memory on its own behalf
   - any use of a window class not 100% programmed in delphi (i.E. the listviews
     in debug mode allocate their own memory to hold its items and state)
   - external COM objects you use
   - if you use the MSXML components it will be even worse COmBSTR ;)


QUICK intstructions:
  * Make the LocalHeapMM the first Unit used (IMPORTANT!!)
      (go to project->view source and add it as first unit)
  * define/undefine DEBUG to enable debug mode
  * adjust the constanst to you needs:
        INITIAL_HEAP_SIZE  = Initial Size of the heap
        CONST MAX_HEAP_SIZE = maximum size of the heap. use a value -1 for
                inifite growth
        CONST CLEANUP_INTERVAL = try to do a compact memory all n-times.

}
unit LocalHeapMM;

interface

//$DEFINE DEBUG} // change to have a clean release build!

uses
{$IFDEF MSWINDOWS}
Windows,
Messages,
{$ENDIF}
SysConst;

Procedure TRACE(sMsg: String);
Function DumpMemStatus():String;

function LHGetMem(Size: Integer): Pointer;
function LHFreeMem(P: Pointer): Integer;
function LHReallocMem(P: Pointer; Size: Integer): Pointer;

implementation
uses
{$IFDEF DEBUG}
CommCtrl,
{$endif}
SysUtils;

//$R *.res}

const
  LocalHeapMemoryManager: TMemoryManager = (
  GetMem: LHGetMem;
  FreeMem: LHFreeMem;
  ReallocMem: LHReallocMem);


//CONST INITIAL_HEAP_SIZE = 1024*100; // 100KB Initial Size
//CONST MAX_HEAP_SIZE = INITIAL_HEAP_SIZE; // Fixed size!
//CONST CLEANUP_INTERVALL=1000; // all x operatiosn compact memory
CONST INITIAL_HEAP_SIZE = 1024*200; // 200KB Initial Size
CONST MAX_HEAP_SIZE = 0; // Fixed size!
CONST CLEANUP_INTERVALL=1000; // all x operatiosn compact memory



Var hHeap : THandle ;
Var OldMemMgr : TMemoryManager;
var iNextCleanUp : Integer; 
var szStatusLine : ShortString;

var WM_DUMP : cardinal;


{$IFDEF DEBUG}


  CONST IDD_MEMSTATUS = 101;

  CONST IDC_DETAILS = 1000;
  CONST IDC_MEM_OPS   = 1001;
  CONST IDC_MEM_ALLOCATIONS = 1002;
  CONST IDC_MEM_FREES = 1003;
  CONST IDC_MEM_INUSE= 1004;
  CONST IDC_MEM_OPENALLOCATIONS = 1005;
  CONST IDC_DETAILS_LIST = 1006;
  CONST IDC_TRACE_LIST = 1007;
  CONST IDC_DETAILS_REGION = 1008;



  type THeapStats = Record
        iOperations       : Integer;
        iTotalAllocations : Integer;
        iTotalFrees       : Integer;
        iOpenAllocations  : Integer;
        iMemAllocated     : Integer;
  End;
  var HeapStats : THeapStats;
  var hStatusWin : THANDLE;

  function memStatDlgProc(hWnd : THANDLE; uMessage : Longword ; wParam : Word; lParam : Longint):Longint; stdcall;
  var bShowDetails : Longint;
      hListView    : THandle;
      hTraceView    : THandle;
      hRegionView    : THandle;
      column       :  TLVColumn;
      item         :  TLVItem;
      heapEntry    : TProcessHeapEntry;
      localString  : ShortString;
      i : Cardinal ;
      c : char;
      function getObjectData(p : pointer): String;
      var       dummy : TObject;
        pSelf : Pointer;
      Begin
        dummy := TObject(heapEntry.lpData);
        pSelf := PPointer(Integer(dummy) - vmtSelfPtr)^;

        //                    if (heapEntry.cbData >  vmtSelfPtr
        Try
           localString :=  PShortString(PPointer(Integer(pSelf) + vmtClassName)^)^;
            item.pszText:=pchar(Integer(Pointer(@localString))+1);
        Except

        End;
        result:='';
      End;
  Begin
        result:=0;
        if (uMessage = WM_INITDIALOG) then
        Begin
          bShowDetails:=0;
          SetWindowPos(hWnd,HWND_TOPMOST,0,0,632,160,SWP_NOMOVE + SWP_SHOWWINDOW);
// debug debug ;)
//          bShowDetails:=1;
//          SetWindowPos(hWnd,HWND_TOPMOST,0,0,500,600,SWP_NOMOVE + SWP_SHOWWINDOW);
// /debug
          // init the ListView...
          hListView:=GetDlgItem(hWnd, IDC_DETAILS_LIST);
          column.mask:=LVCF_TEXT + LVCF_WIDTH + LVCF_FMT;
          column.fmt := LVCFMT_RIGHT;
          column.cx:=50;
          column.pszText:='Region';
          ListView_InsertColumn(hListView, 0,column);
          column.cx:=70;
          column.pszText:='Addr';
          ListView_InsertColumn(hListView, 1,column);
          column.cx:=70;
          column.pszText:='Size';
          ListView_InsertColumn(hListView, 2,column);
          column.pszText:='Overhead';
          ListView_InsertColumn(hListView, 3,column);
          column.pszText:='Occupied';
          ListView_InsertColumn(hListView, 4,column);

          column.fmt := LVCFMT_LEFT;
          column.cx:=320;
          column.pszText:='Data (paritaly)';
          ListView_InsertColumn(hListView, 5,column);

          hTraceView:=GetDlgItem(hWnd, IDC_TRACE_LIST);
          column.mask:=LVCF_TEXT + LVCF_WIDTH + LVCF_FMT;
          column.fmt := LVCFMT_RIGHT;
          column.cx:=80;
          column.pszText:='#';
          ListView_InsertColumn(hTraceView, 0,column);
          column.cx:=80;
          column.fmt := LVCFMT_LEFT;
          column.pszText:='Op';
          ListView_InsertColumn(hTraceView, 1,column);
          column.fmt := LVCFMT_RIGHT;
          column.cx:=80;
          column.pszText:='Size';
          ListView_InsertColumn(hTraceView, 2,column);
          column.fmt := LVCFMT_RIGHT;
          column.cx:=90;
          column.pszText:='MemInUse';
          ListView_InsertColumn(hTraceView, 3,column);


          hRegionView:=GetDlgItem(hWnd, IDC_DETAILS_REGION);
          column.mask:=LVCF_TEXT + LVCF_WIDTH + LVCF_FMT;
          column.cx:=50;
          column.pszText:='Region';
          ListView_InsertColumn(hRegionView, 0,column);
          column.cx:=70;
          column.pszText:='Addr';
          ListView_InsertColumn(hRegionView, 1,column);
          column.cx:=70;
          column.pszText:='Size';
          ListView_InsertColumn(hRegionView, 2,column);
          column.pszText:='Overhead';
          ListView_InsertColumn(hRegionView, 3,column);
          column.pszText:='Occupied';
          ListView_InsertColumn(hRegionView, 4,column);

          column.pszText:='Committed';
          ListView_InsertColumn(hRegionView, 5,column);
          column.pszText:='Uncommitted';
          ListView_InsertColumn(hRegionView, 6,column);

          SetWindowLong(hWnd, GWL_USERDATA,bShowDetails);
          result:=1;
        end;

        if (uMessage = WM_COMMAND) and (wParam = IDC_DETAILS)  then
        Begin
          bShowDetails := GetWindowLong(hWnd, GWL_USERDATA);
          if (bShowDetails = 0) then
          Begin
                SetWindowPos(hWnd,HWND_TOPMOST,0,0,632,505,SWP_NOMOVE + SWP_SHOWWINDOW);
                bShowDetails:=1;
                SetWindowLong( hWnd, GWL_USERDATA,bShowDetails);
                SendMessage(hWnd,WM_DUMP,0,0);
          end
          else
          Begin
                SetWindowPos(hWnd,HWND_TOPMOST,0,0,632,160,SWP_NOMOVE + SWP_SHOWWINDOW);

                bShowDetails:=0;
                hListView:=GetDlgItem(hWnd, IDC_DETAILS_LIST);
                ListView_DeleteAllItems(hListView);
                SetWindowLong( hWnd, GWL_USERDATA,bShowDetails);
          End;
         result:=1;
        End;
        if (uMessage =  WM_DUMP) then
        Begin
            bShowDetails := GetWindowLong( hWnd, GWL_USERDATA);

            SetDlgItemInt(hWnd,  IDC_MEM_OPS, HeapStats.iOperations,false);
            SetDlgItemInt(hWnd,  IDC_MEM_ALLOCATIONS, HeapStats.iTotalAllocations,false);
            SetDlgItemInt(hWnd,  IDC_MEM_FREES, HeapStats.iTotalFrees,false);
            SetDlgItemInt(hWnd,  IDC_MEM_OPENALLOCATIONS, HeapStats.iOpenAllocations,false);
            SetDlgItemInt(hWnd,  IDC_MEM_INUSE, HeapStats.iMemAllocated,false);
            if wparam <> 0 then
            Begin
              hTraceView:=GetDlgItem(hWnd, IDC_TRACE_LIST);
              ZeroMemory(@item,sizeof(TLVItem));
              item.mask := LVIF_TEXT;
              item.iItem:=0;
              item.iSubItem:=0;
              ZeroMemory(@localString,255);
              localString:='';
              Str(HeapStats.iOperations, localString);
              item.pszText:=pchar(Integer(Pointer(@localString))+1);
              ListView_InsertItem(hTraceView,item);

              item.iSubItem:=1;
              case wparam of
                 0 : item.pszText:='Dump';
                 1 : item.pszText:='GetMem';
                 2 : item.pszText:='FreeMem';
                 3 : item.pszText:='ReallocMem';
                 4 : item.pszText:='Compact Heap';
              End;
              ListView_SetItem(hTraceView,item);

              item.iSubItem:=2;
              ZeroMemory(@localString,255);
              Str(lParam, localString);
              item.pszText:=pchar(Integer(Pointer(@localString))+1);
              ListView_SetItem(hTraceView,item);

              item.iSubItem:=3;
              ZeroMemory(@localString,255);
              Str(HeapStats.iMemAllocated, localString);
              item.pszText:=pchar(Integer(Pointer(@localString))+1);
              ListView_SetItem(hTraceView,item);
              while (ListView_GetItemCount(hTraceView) > 500) do
              Begin
                ListView_DeleteItem(hTraceView,500);
              End;

            end;

            // extended stats?
            if (bShowDetails <> 0) AND (hHeap <> 0) then
            Begin

             hListView:=GetDlgItem(hWnd, IDC_DETAILS_LIST);
             hRegionView:=GetDlgItem(hWnd, IDC_DETAILS_REGION);

             ListView_DeleteAllItems(hListView);
             ListView_DeleteAllItems(hRegionView);
             if (HeapLock(hHeap)) then
             Try
                // WALK THE REGIONS
                ZeroMemory(@heapEntry,sizeof(TProcessHeapEntry));
                ZeroMemory(@item,sizeof(TLVItem));
                item.mask := LVIF_TEXT;
                item.iItem:=0;
                heapEntry.lpData:=nil;
                heapEntry.wFlags:= PROCESS_HEAP_REGION;
                while (HeapWalk(hHeap,heapEntry)  ) do
                Begin
                 // only show regions
                  if ( (heapEntry.wFlags OR PROCESS_HEAP_REGION) = heapEntry.wFlags ) then
                   begin
                    // use old mmanager.... avoid recursion...
                    item.iSubItem:=0;
                    ZeroMemory(@localString,255);
                    localString:='';
                    Str(heapEntry.iRegionIndex, localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_InsertItem(hRegionView,item);

                    item.iSubItem:=1;
                    ZeroMemory(@localString,255);
                    Str(Integer(heapEntry.lpData),localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hRegionView,item);

                    item.iSubItem:=2;
                    ZeroMemory(@localString,255);
                    Str(heapEntry.dwCommittedSize + heapEntry.dwUncommittedSize,localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hRegionView,item);

                    item.iSubItem:=3;
                    ZeroMemory(@localString,255);
                    Str(heapEntry.cbOverhead, localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hRegionView,item);

                    item.iSubItem:=4;
                    ZeroMemory(@localString,255);
                    Str(heapEntry.cbData+heapEntry.cbOverhead, localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hRegionView,item);

                    item.iSubItem:=5;
                    ZeroMemory(@localString,255);
                    Str(heapEntry.dwCommittedSize, localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hRegionView,item);

                    item.iSubItem:=6;
                    ZeroMemory(@localString,255);
                    Str(heapEntry.dwUnCommittedSize, localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hRegionView,item);


                    Inc(item.iItem);
                   end;
                End;

             // NOW WALK THE ITEMS
                ZeroMemory(@heapEntry,sizeof(TProcessHeapEntry));
                ZeroMemory(@item,sizeof(TLVItem));
                item.mask := LVIF_TEXT;
                item.iItem:=0;
                heapEntry.lpData:=nil;
                heapEntry.wFlags:= PROCESS_HEAP_ENTRY_BUSY;
                while (HeapWalk(hHeap,heapEntry)  ) do
                Begin
                 // only show USED memory blocks
                  if ( (heapEntry.wFlags OR PROCESS_HEAP_ENTRY_BUSY) = heapEntry.wFlags ) then
                   begin
                    // use old mmanager.... avoid recursion...
                    item.iSubItem:=0;
                    ZeroMemory(@localString,255);
                    localString:='';
                    Str(heapEntry.iRegionIndex, localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_InsertItem(hListView,item);

                    item.iSubItem:=1;
                    ZeroMemory(@localString,255);
                    Str(Integer(heapEntry.lpData),localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hListView,item);

                    item.iSubItem:=2;
                    ZeroMemory(@localString,255);
                    Str(heapEntry.cbData,localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hListView,item);

                    item.iSubItem:=3;
                    ZeroMemory(@localString,255);
                    Str(heapEntry.cbOverhead, localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hListView,item);

                    item.iSubItem:=4;
                    ZeroMemory(@localString,255);
                    Str(heapEntry.cbData+heapEntry.cbOverhead, localString);
                    item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    ListView_SetItem(hListView,item);

                    item.iSubItem:=5;
                    ZeroMemory(@localString,255);
                    i:=0;
                    item.pszText:=nil;

{                    if (heapEntry.cbData >= 4) then
                            getObjectData(heapEntry.lpData);
 }
                    if ( item.pszText=nil) then
                    BEgin
                      while ( (i < 255) AND (i < heapEntry.cbData))  do
                      Begin
                            c:=PChar(Cardinal(heapEntry.lpData)+i)^;
                            if (Ord(c) = 0) then
                                    c:='·';
                            localString[i+1]:=c;
                            inc(i);
                      End;
                      item.pszText:=pchar(Integer(Pointer(@localString))+1);
                    end;
                    ListView_SetItem(hListView,item);
                    Inc(item.iItem);
                   end;
                End;

             finally
              HeapUnlock(hHeap);
             end;

            end;
          result:=1;
        End;

  End;

   Procedure StartDebugWin();
   Begin
       hStatusWin := CreateDialog(MainInstance,MakeIntResource(IDD_MEMSTATUS), 0, @memStatDlgProc);
       if (hStatusWin=0) then
       Begin
        WriteLN(GetLastError());
       End;
       ShowWindow(hStatusWin,SW_SHOW);
   End;


  Function DumpMemStatus():String;
  Begin
     DumpMemStatus:='';
    with HeapStats do
    Begin
        szStatusLine:='Allocs: '+IntToStr(iTotalAllocations) + #9 +
        ' Free: '+IntToStr(iTotalFrees) + #9 +
        ' Open: '+IntToStr(iOpenAllocations) + #9  +
        ' MemInUse: '+IntToStr(iMemAllocated) ;
      TRACE(szStatusLine);
      DumpMemStatus:=szStatusLine;
    end;
  End;

  {$IFDEF CONSOLE}
  Procedure TRACE(sMsg: String);
  Begin
        WriteLn(sMsg);
        OutputDebugString(pchar(sMsg));
  End;
  {$ELSE}
  Procedure TRACE(sMsg: String);
  Begin
        OutputDebugString(pchar(sMsg));
  End;
  {$ENDIF}
{$ELSE}
  Procedure  TRACE(sMsg: String);
  Begin
  End;
  Function DumpMemStatus():String;
  Begin
        DumpMemStatus:='Retail version!';
  End;
{$ENDIF}


function LHGetMem(Size: Integer): Pointer;
var pResult : Pointer;
Begin
  if (hHeap = 0) then
        raise Exception.Create('Illegal local Heap');

  pResult := HeapAlloc(hHeap, 0, Size);
  if (pResult=nil) then
  Begin
   // try to compact memory
   HeapCompact(hHeap,0);
{$IFDEF DEBUG}
   SendMessage(hStatusWin,WM_DUMP,4,0);
{$ENDIF}
   pResult := HeapAlloc(hHeap, 0, Size);
  end;

{$IFDEF DEBUG}
  with HeapStats do
  Begin
        InterlockedIncrement(iOperations);
        InterlockedIncrement(iTotalAllocations);
        InterlockedIncrement(iOpenAllocations);
        InterlockedExchangeAdd(IMemAllocated,Size);
  End;
  SendMessage(hStatusWin,WM_DUMP,1,Size);
{$ENDIF}
  // need cleanup?
  InterlockedIncrement(iNextCleanUp);
  if  (iNextCleanUp >= CLEANUP_INTERVALL) then
  Begin
        iNextCleanUp:=0;
        HeapCompact(hHeap,0);
  {$IFDEF DEBUG}
        SendMessage(hStatusWin,WM_DUMP,4,0);
  {$ENDIF}
  End;

  {$IFDEF DEBUG}
        //initialize new mem block..
        FillMemory(pResult,Size,64);
  {$ENDIF}

  Result:= pResult;
End;

function LHFreeMem(P: Pointer): Integer;
{$IFDEF DEBUG}
Var Size : Integer;
{$ENDIF}
Begin
  Result:=0;
  if (hHeap = 0) then
        raise Exception.Create('Illegal local Heap');
  if (p=nil) then
   Begin
       {$IFDEF DEBUG}
      with HeapStats do
      Begin
            InterlockedIncrement(iOperations);
      End;
      SendMessage(hStatusWin,WM_DUMP,2,-1);
    {$ENDIF}
    exit;
   End;

{$IFDEF DEBUG}
    Size:=HeapSize(hHeap,0,p);
    if (Size=-1) then
        raise Exception.Create('HeapSize Failed');{$ENDIF}

  if (NOT HeapFree(hHeap,0,p) ) then
        raise Exception.Create('HeapFree Failed');

{$IFDEF DEBUG}
  with HeapStats do
  Begin
        InterlockedIncrement(iOperations);
        InterlockedIncrement(iTotalFrees);
        InterlockedDecrement(iOpenAllocations);
        InterlockedExchangeAdd(IMemAllocated,-1*Size);
  End;
  SendMessage(hStatusWin,WM_DUMP,2,Size);
{$ENDIF}
  // need cleanup?
  InterlockedIncrement(iNextCleanUp);
  if  (iNextCleanUp >= CLEANUP_INTERVALL) then
  Begin
        iNextCleanUp:=0;
        HeapCompact(hHeap,0);
{$IFDEF DEBUG}
        SendMessage(hStatusWin,WM_DUMP,4,0);
{$ENDIF}
  End;

End;

function LHReallocMem(P: Pointer; Size: Integer): Pointer;
var pResult : Pointer ;
{$IFDEF DEBUG}
Var OldSize: Integer;
{$ENDIF}
Begin
  if (hHeap = 0) then
        raise Exception.Create('Illegal local Heap');

{$IFDEF DEBUG}
    if (p <> NIL) then
    begin
      OldSize:=HeapSize(hHeap,0,p);
      if (OldSize=-1) then
          raise Exception.Create('HeapSize Failed');
      with HeapStats do
      Begin
            InterlockedIncrement(iTotalFrees);
            InterlockedDecrement(iOpenAllocations);
            InterlockedExchangeAdd(IMemAllocated,-1*OldSize);
      End;
    end;
{$ENDIF}

  pResult := HeapReAlloc(hHeap,0,p,Size);

  // need cleanup?
  InterlockedIncrement(iNextCleanUp) ;
  if  (iNextCleanUp >= CLEANUP_INTERVALL) then
  Begin
        iNextCleanUp:=0;
        HeapCompact(hHeap,0);
{$IFDEF DEBUG}
        SendMessage(hStatusWin,WM_DUMP,4,0);
{$ENDIF}
  End;

{$IFDEF DEBUG}
  with HeapStats do
  Begin
        InterlockedIncrement(iTotalAllocations);
        InterlockedIncrement(iOpenAllocations);
        InterlockedExchangeAdd(IMemAllocated,Size);
  End;
   SendMessage(hStatusWin,WM_DUMP,3,Size);
{$ENDIF}

  Result:= pResult;
End;

procedure InitMemoryManager;
begin
  WM_DUMP := RegisterWindowMessage('DumpMemory');
   hHeap :=0;
   iNextCleanUp := 0;

   if IsMemoryManagerSet then
        raise Exception.Create('Custom memory manager a%lready set!');

  hHeap := HeapCreate(0, INITIAL_HEAP_SIZE, MAX_HEAP_SIZE);
  if (hHeap = 0) then
        raise Exception.Create('Could not create Local heap');

  GetMemoryManager(OldMemMgr);
  SetMemoryManager(LocalHeapMemoryManager);
{$IFDEF DEBUG}
  with HeapStats do
  Begin
        iOperations       :=0;
        iTotalAllocations := 0;
        iTotalFrees       := 0;
        iOpenAllocations  := 0;
        iMemAllocated     := 0;
  End;

 StartDebugWin();

{$ENDIF}
  TRACE('LocalHeapMemoryManager initialized');
end;

procedure DoneMemoryManager;
{$IFDEF DEBUG}
var sMsg : String;
{$ENDIF}
Begin
  if (hHeap<>0) Then
  Begin
    SetMemoryManager(OldMemMgr);
{$IFDEF DEBUG}
    if (HeapStats.iOpenAllocations > 0) then
    Begin
       sMsg := 'You have ' + IntToStr(HeapStats.iOpenAllocations) +' open allocations!';
        MessageBox(0,Pchar( sMsg),'Uiuiui!',MB_ICONEXCLAMATION + MB_OK);
    End;

    if (NOT HeapValidate(hHeap,0,nil)) then
    Begin
      HeapDestroy(hHeap);
      raise Exception.Create('Heap Validation returned an error');
    End;
{$ENDIF}

    if (NOT HeapDestroy(hHeap) ) then
        raise Exception.Create('HeapDestroy failed');
    DumpMemStatus() ;
    TRACE('LocalHeapMemoryManager deinitialized');
 {$IFDEF DEBUG}
    DestroyWindow(hStatusWin);
 {$ENDIF}
   end;

End;

Initialization
  InitMemoryManager;

Finalization
  DoneMemoryManager();

  //HeapCompact
end.
