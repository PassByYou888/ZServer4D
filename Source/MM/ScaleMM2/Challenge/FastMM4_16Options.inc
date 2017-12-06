{

Fast Memory Manager: Options Include File

Set the default options for FastMM here.

}

{Set the following option to do extensive checking of all memory blocks. All
 blocks are padded with both a header and trailer that are used to verify the
 integrity of the heap. Freed blocks are also cleared to to ensure that they
 cannot be reused after being freed. This option slows down memory operations
 dramatically and should only be used to debug an application that is
 overwriting memory or reusing freed pointers. Setting this option
 automatically enables CheckHeapForCorruption and disables ASMVersion.}
{.$define FullDebugMode}

{FastMM always catches attempts to free the same memory block twice, however it
 can also check for corruption of the memory heap (typically due to the user
 program overwriting the bounds of allocated memory). These checks are
 expensive, and this option should thus only be used for debugging purposes.
 If this option is set then the ASMVersion option is automatically disabled.}
{.$define CheckHeapForCorruption}

{Set this option to use the assembly language version which is faster than the
 pascal version. Disable only for debugging purposes. Setting the
 CheckHeapForCorruption option automatically disables this option.}
{$define ASMVersion}

{Enable this define to align all blocks on 16 byte boundaries so aligned SSE
 instructions can be used safely. If this option is disabled then some of the
 smallest block sizes will be 8-byte aligned instead which may result in a
 reduction in memory usage. Medium and large blocks are always 16-byte aligned
 irrespective of this setting.}
{$define Align16Bytes}

{Enable to use faster fixed-size move routines when upsizing small blocks.
 These routines are much faster than the Borland RTL move procedure since they
 are optimized to move a fixed number of bytes. This option may be used
 together with the FastMove library for even better performance.}
{$define UseCustomFixedSizeMoveRoutines}

{Enable this option to use an optimized procedure for moving a memory block of
 an arbitrary size. Disable this option when using the Fastcode move
 ("FastMove") library. Using the Fastcode move library allows your whole
 application to gain from faster move routines, not just the memory manager. It
 is thus recommended that you use the Fastcode move library in conjunction with
 this memory manager and disable this option.}
{$define UseCustomVariableSizeMoveRoutines}

{Set this option to enable the use of MMX instructions. Disabling this option
 will result in a slight performance hit, but will enable compatibility with
 AMD K6, Pentium I and earlier CPUs. MMX is currently only used in the variable
 size move routines, so if UseCustomVariableSizeMoveRoutines is not set then
 this option has no effect.}
{.$define EnableMMX}

{Enable to always assume that the application is multithreaded. Enabling this
 option will cause a significant performance hit with single threaded
 applications. Enable if you are using multi-threaded third party tools that do
 not properly set the IsMultiThread variable. Also set this option if you are
 going to share this memory manager between a single threaded application and a
 multi-threaded DLL.}
{.$define AssumeMultiThreaded}

{Allow sharing of the memory manager between a main application and DLLs that
 were also compiled with FastMM. This allows you to pass dynamic arrays and
 long strings to DLL functions provided both are compiled to use FastMM.
 Sharing will only work if the library that is supposed to share the memory
 manager was compiled with the "AttemptToUseSharedMM" option set. Note that if
 the main application is single threaded and the DLL is multi-threaded that you
 have to set the IsMultiThread variable in the main application to true or it
 will crash when a thread contention occurs. Note that statically linked DLL
 files are initialized before the main application, so the main application may
 well end up sharing a statically loaded DLL's memory manager and not the other
 way around. }
{$define ShareMM}

{Allow sharing of the memory manager by a DLL with other DLLs (or the main
 application if this is a statically loaded DLL) that were also compiled with
 FastMM. Set this option with care in dynamically loaded DLLs, because if the
 DLL that is sharing its MM is unloaded and any other DLL is still sharing the
 MM then the application will crash. This setting is only relevant for DLL
 libraries and requires ShareMM to also be set to have any effect. Sharing will
 only work if the library that is supposed to share the memory manager was
 compiled with the "AttemptToUseSharedMM" option set. Note that if DLLs are
 statically linked then they will be initialized before the main application
 and then the DLL will in fact share its MM with the main application.}
{.$define ShareMMIfLibrary}

{Define this to attempt to share the MM of the main application or other loaded
 DLLs in the same process that were compiled with ShareMM set. When sharing a
 memory manager, memory leaks caused by the sharer will not be freed
 automatically. Take into account that statically linked DLLs are initialized
 before the main application, so set the sharing options accordingly.}
{.$define AttemptToUseSharedMM}

{Set this option to check for user code that uses an interface of a freed
 object. Note that this will disable the checking of blocks modified after
 being freed (the two are not compatible). This option has no effect if
 FullDebugMode is not also enabled.}
{.$define CatchUseOfFreedInterfaces}

{Set this option to enable reporting of memory leaks. Combine it with the two
 options below for further fine-tuning.}
{.$define EnableMemoryLeakReporting}

{Set this option to require the presence of the Delphi IDE to report memory
 leaks. This option has no effect if the option "EnableMemoryLeakReporting"
 is not also set.}
{.$define RequireIDEPresenceForLeakReporting}

{Set this option to require the presence of debug info ($D+ option) in the
 compiled unit to perform memory leak checking. This option has no effect if
 the option "EnableMemoryLeakReporting" is not also set.}
{.$define RequireDebugInfoForLeakReporting}

{Set this option to enable manual control of the memory leak report. When this
 option is set ReportMemoryLeaks variable (default = true) may be changed to
 select whether leak reporting should be done or not. When this option is
 selected then both the variable must be set to true and the other leak
 checking options must be applicable for the leak checking to be done.}
{$define ManualLeakReportingControl}

{Set this option to disable the display of the hint below the memory leak
 message.}
{.$define HideMemoryLeakHintMessage}

{Set this option to log all errors to a text file in the same folder as the
 application. Memory errors (with the FullDebugMode option set) will be
 appended to the log file. Has no effect if "FullDebugMode" is not set.}
{$define LogErrorsToFile}

{Set this option to log all memory leaks to a text file in the same folder as
 the application. Memory leak reports (with the FullDebugMode option set) will
 be appended to the log file. Has no effect if "LogErrorsToFile" and
 "FullDebugMode" are not also set. Note that all leaks are always logged, even
 if they are "expected" leaks registered through AddExpectedMemoryLeaks.}
{.$define LogMemoryLeakDetailToFile}

{Due to QC#14070 ("Delphi IDE attempts to free memory after the shutdown code
 of borlndmm.dll has been called"), FastMM cannot be uninstalled safely when
 used inside a replacement borlndmm.dll for the IDE. Setting this option will
 circumvent this problem by never uninstalling the memory manager.}
{.$define NeverUninstall}

{Set this option to display a debug message whenever FastMM is installed or
 uninstalled.}
{.$define ShowInstallUninstallDebugString}

{Group the options you use for release and debug versions below}
{$ifdef Release}
  {Specify the options you use for release versions below}
  {.$undef FullDebugMode}
  {.$undef CheckHeapForCorruption}
  {.$define ASMVersion}
  {.$undef EnableMemoryLeakReporting}
{$else}
  {Specify the options you use for debugging below}
  {.$define FullDebugMode}
  {.$define EnableMemoryLeakReporting}
{$endif}

