
QIU Memory Manager 1.14 for Delphi

Description:
  a simple and compact MM for Delphi/XE

Homepage:
  https://code.google.com/p/qiumm/
  by qiusonglin (qiusonglin.hex@gmail.com)

Usage:
 - place this unit as the very first unit under the "uses" section in your
   project's .dpr file.

Other:
 - Note: test on D7+D2010+XE4+XE6 (WIN32+WIN64), other Delphi versions of your own testing, 
   WIN64 testing is not comprehensive, there might be a problem.
 - support multithread, allocate memory for each thread manager.
 - more, plz see qmm readme.txt & change.log

Support:
 If you have trouble using QMM, you are welcome to drop me an e-mail at the
 address above.

License:
  Released under Mozilla Public License 1.1

  If you find QMM useful or you would like to support further development,
  a donation would be much appreciated.
  My PayPal account is: qiusonglin.hex@gmail.com


This archive contains:

--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
QMM.inc - QMM Optional Features (QMM.Inc):

##!!!deprecated!!! fastcode - The use of fastcode code, instead of the system function: fillchar and move.

debug/release      -- with application config

debug.TRACE_STACK  -- from fastmm code
        with debug, if defined(trace_stack), report leak will:

if not defined(debug) and not defined(release)
   QMM define release for self(default on Delphi7-D2007) 
   
--------------------------------
leak X:
stack trace: $4097E4 -> $4D372C -> $4D3250 -> $4D3FE1 -> $4D4379 -> $0 -> $0 -> $0
unknow data: $3C0492C, size: 53, data: 
--------------------------------
  you can trace memleak with address
        
--------------------------------------------------------------------------------------------------------------------------
QMM.pas - The replacement memory manager (to speed up your applications)

1: Constant:
   StackTraceDepth = 8;
   -- the constant is for debug.TRACE_STACK, tracking the number of address
   suffix_mem_check = sizeof(Pointer) * 2;
   -- the constant is for debug.memory check out bound, You can use it to check the
      maximum length of the boundary shape.
   -- nothing.
   
2: Variable:
  on_memory_error_proc: procedure(op: TDebugMemOP; address: Pointer; size: MSIZE);  
  -- When the errors of memory, it will trigger the event callback.
		(this variable, wrote a simple tool unit(QMMErrorUtils.pas) to save memory error)

  int3_when_memory_error: Boolean = true;
  -- ok, not what the variables, only when the memory error, will appear, if false, 
    don't interrupt your program, just to continue running the program.
  
  on_notify_get_proc,  on_notify_realloc_proc,  on_notify_free_proc
  -- ...
  
3: new unit: QMMErrorUtils.pas
   This unit is a tools unit, in order to do out of memory error checking simple writing.
   You can use the two constants:
		StackTraceDepth & suffix_mem_check
	To configure the unit to check your program, please refer to the demo\mem-error

--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------

NOTE: 
  1: when System.ReportMemoryLeaksOnShutdown(debug&replease) is true,  QMM will 
  		report leak on app shut down. 
  2: when uses QMMErrorUtils.pas, The QMM will immediately report errors in memory to file
        file path: YourApp\mem-error\YourApp.threadId.txt

--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
2014.09.15  by qiusonglin 