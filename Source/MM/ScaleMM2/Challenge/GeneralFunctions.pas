unit GeneralFunctions;

//Minor modifications:
// Added DetectCPUType from original MainUnit in FastCodeBenchmarkTool091
// Added GetCPUFrequence from original MainUnit in FastCodeBenchmarkTool091

interface

uses
 Windows, SysUtils, Classes;

type

 TVersion = record
  Major: integer;
  Minor: integer;
  Release: integer;
  Build: integer;
 end;

 PVS_FIXEDFILEINFO = ^VS_FIXEDFILEINFO;
 TCPUIDResult = packed record
  EAX: Cardinal;
  EBX: Cardinal;
  ECX: Cardinal;
  EDX: Cardinal;
 end;

 TCPUFeatures = class(TPersistent)
  private
   FSEP: Boolean;
   FMTRR: Boolean;
   FMSR: Boolean;
   FPSE: Boolean;
   FTSC: Boolean;
   FMCE: Boolean;
   FMMX: Boolean;
   FPAT: Boolean;
   FPAE: Boolean;
   FXSR: Boolean;
   FVME: Boolean;
   FPGE: Boolean;
   FCMOV: Boolean;
   FFPU: Boolean;
   FCX8: Boolean;
   FSIMD: Boolean;
   FMCA: Boolean;
   FAPIC: Boolean;
   FDE: Boolean;
   FPSE36: Boolean;
   FSERIAL: Boolean;
   F3DNOW: Boolean;
   FEX3DNOW: Boolean;
   FEXMMX: Boolean;
 published
   property _3DNOW: Boolean read F3DNOW write F3DNOW stored False;
   property EX_3DNOW: Boolean read FEX3DNOW write FEX3DNOW stored False;
   property EX_MMX: Boolean read FEXMMX write FEXMMX stored False;
   property SIMD: Boolean read FSIMD write FSIMD stored False;
   property SERIAL: Boolean read FSERIAL write FSERIAL stored False;
   property XSR: Boolean read FXSR write FXSR stored False;
   property MMX: Boolean read FMMX write FMMX stored False;
   property PSE36: Boolean read FPSE36 write FPSE36 stored False;
   property PAT: Boolean read FPAT write FPAT stored False;
   property CMOV: Boolean read FCMOV write FCMOV stored False;
   property MCA: Boolean read FMCA write FMCA stored False;
   property PGE: Boolean read FPGE write FPGE stored False;
   property MTRR: Boolean read FMTRR write FMTRR stored False;
   property SEP: Boolean read FSEP write FSEP stored False;
   property APIC: Boolean read FAPIC write FAPIC stored False;
   property CX8: Boolean read FCX8 write FCX8 stored False;
   property MCE: Boolean read FMCE write FMCE stored False;
   property PAE: Boolean read FPAE write FPAE stored False;
   property MSR: Boolean read FMSR write FMSR stored False;
   property TSC: Boolean read FTSC write FTSC stored False;
   property PSE: Boolean read FPSE write FPSE stored False;
   property DE: Boolean read FDE write FDE stored False;
   property VME: Boolean read FVME write FVME stored False;
   property FPU: Boolean read FFPU write FFPU stored False;
 end;

 TCPUID	= array[1..4] of Longint;
 TVendor = array [0..11] of char;

 function GetCPUID : TCPUID; assembler; register;
 function IsCPUID_Available : Boolean; register;
 function GetCPUVendor : TVendor; assembler; register;
 function GetCPUFeaturesInfo(FeatureSetIndex : Integer) : Boolean;
 function ExecuteCPUID : TCPUIDResult; assembler;
 function GetFormattedVersion: string;
 function GetModuleVersionDFL(ModuleFileName: string; var Ver: TVersion; Product: Boolean = False): string;
 function DetectCPUType(Family, Model: Integer): String;
 function GetCPUFrequencyMHz : Cardinal;

implementation

//uses MainUnit;

resourcestring
  TEXT_NO_VERSIONINFO = 'No version info';

var
 CPUID_Level : DWORD;
 CPUID : TCPUIDResult;

const
 ID_BIT	=	$200000;
 CPUID_CPUFEATURESET : DWORD = $1;
 CPUID_CPUSIGNATUREEX: DWORD = $80000001;
 SFS_FPU = 0;
 SFS_VME = 1;
 SFS_DE = 2;
 SFS_PSE = 3;
 SFS_TSC = 4;
 SFS_MSR = 5;
 SFS_PAE = 6;
 SFS_MCE = 7;
 SFS_CX8 = 8;
 SFS_APIC = 9;
 SFS_SEP = 11;
 SFS_MTRR = 12;
 SFS_PGE = 13;
 SFS_MCA = 14;
 SFS_CMOV = 15;
 SFS_PAT = 16;
 SFS_PSE36 = 17;
 SFS_SERIAL = 18;
 SFS_MMX = 23;
 SFS_XSR = 24;
 SFS_SIMD = 25;
 EFS_EXMMXA = 22; { AMD Specific }
 EFS_EXMMXC = 24; { Cyrix Specific }
 EFS_3DNOW = 31;
 EFS_EX3DNOW = 30;

function GetCPUID : TCPUID; assembler; register;
asm
  push    ebx         {Save affected register}
  push    edi
  mov     edi,eax     {@Resukt}
  mov     eax,1
  dw      $a20f       {CPUID Command}
  stosd			          {CPUID[1]}
  mov     eax,ebx
  stosd               {CPUID[2]}
  mov     eax,ecx
  stosd               {CPUID[3]}
  mov     eax,edx
  stosd               {CPUID[4]}
  pop     edi					{Restore registers}
  pop     ebx
end;

function IsCPUID_Available : Boolean; register;
asm
	pushfd							{direct access to flags no possible, only via stack}
  pop     eax 				{flags to EAX}
  mov     edx,eax			{save current flags}
  xor     eax,id_bit	{not ID bit}
  push    eax					{onto stack}
  popfd								{from stack to flags, with not ID bit}
  pushfd							{back to stack}
  pop     eax					{get back to EAX}
  xor     eax,edx			{check if ID bit affected}
  jz      @exit				{no, CPUID not availavle}
  mov     al,True			{Result=True}
@exit:
end;

function GetCPUVendor : TVendor; assembler; register;
asm
  push    ebx					{Save affected register}
  push    edi
  mov     edi,eax			{@Result (TVendor)}
  mov     eax,0
  dw      $a20f				{CPUID Command}
  mov     eax,ebx
  xchg		ebx,ecx     {save ECX result}
  mov			ecx,4
@1:
  stosb
  shr     eax,8
  loop    @1
  mov     eax,edx
  mov			ecx,4
@2:
  stosb
  shr     eax,8
  loop    @2
  mov     eax,ebx
  mov			ecx,4
@3:
  stosb
  shr     eax,8
  loop    @3
  pop     edi					{Restore registers}
  pop     ebx
end;

function GetCPUFeaturesInfo(FeatureSetIndex : Integer) : Boolean;
begin
 if (FeatureSetIndex >= 0) and (FeatureSetIndex <21) then
  begin
   CPUID_Level := CPUID_CPUFEATURESET;
   CPUID := ExecuteCPUID;
  end;
 if (FeatureSetIndex >= 21) and (FeatureSetIndex <= 23) then
  begin
   CPUID_Level := CPUID_CPUSIGNATUREEX;
   CPUID := ExecuteCPUID;
  end;
 case FeatureSetIndex of
   0 : Result := ((CPUID.EDX and (1 shl SFS_SIMD)) <> 0); // SIMD
   1 : Result := ((CPUID.EDX and (1 shl SFS_XSR)) <> 0); // XSR
   2 : Result := ((CPUID.EDX and (1 shl SFS_MMX)) <> 0); // MMX
   3 : Result := ((CPUID.EDX and (1 shl SFS_SERIAL)) <> 0); // SERIAL
   4 : Result := ((CPUID.EDX and (1 shl SFS_PSE36)) <> 0); // PSE36
   5 : Result := ((CPUID.EDX and (1 shl SFS_PAT)) <> 0); // PAT
   6 : Result := ((CPUID.EDX and (1 shl SFS_CMOV)) <> 0); // CMOV
   7 : Result := ((CPUID.EDX and (1 shl SFS_MCA)) <> 0); // MCA
   8 : Result := ((CPUID.EDX and (1 shl SFS_PGE)) <> 0); // PGE
   9 : Result := ((CPUID.EDX and (1 shl SFS_MTRR)) <> 0); // MTRR
  10 : Result := ((CPUID.EDX and (1 shl SFS_SEP)) <> 0); // SEP
  11 : Result := ((CPUID.EDX and (1 shl SFS_APIC)) <> 0); // APIC
  12 : Result := ((CPUID.EDX and (1 shl SFS_CX8)) <> 0); // CX8
  13 : Result := ((CPUID.EDX and (1 shl SFS_MCE)) <> 0); // MCE
  14 : Result := ((CPUID.EDX and (1 shl SFS_PAE)) <> 0); // PAE
  15 : Result := ((CPUID.EDX and (1 shl SFS_MSR)) <> 0); // MSR
  16 : Result := ((CPUID.EDX and (1 shl SFS_TSC)) <> 0); // TSC
  17 : Result := ((CPUID.EDX and (1 shl SFS_PSE)) <> 0); // PSE
  18 : Result := ((CPUID.EDX and (1 shl SFS_DE)) <> 0); // DE
  19 : Result := ((CPUID.EDX and (1 shl SFS_VME)) <> 0);// VME
  20 : Result := ((CPUID.EDX and (1 shl SFS_FPU)) <> 0); // FPU
  21 : Result := ((CPUID.EDX and (1 shl EFS_EXMMXA)) <> 0) or ((CPUID.EDX and (1 shl EFS_EXMMXC)) <> 0); // EX_MMX
  22 : Result := ((CPUID.EDX and (1 shl EFS_EX3DNOW)) <> 0); // EX_3DNOW
  23 : Result := ((CPUID.EDX and (1 shl EFS_3DNOW)) <> 0); // _3DNOW
 else
  begin
   Result := False;
//   MessageBox(MainForm.Handle,'Index out of bounds',nil,MB_OK)
  end;
 end;
end;

function ExecuteCPUID : TCPUIDResult; assembler;
asm
  push    ebx
  push    edi
  mov     edi, eax
  mov     eax, CPUID_LEVEL
  dw      $A20F
  stosd
  mov     eax, ebx
  stosd
  mov     eax, ecx
  stosd
  mov     eax, edx
  stosd
  pop     edi
  pop     ebx
end;

function GetFormattedVersion: string;
var
  ver: TVersion;
begin
  GetModuleVersionDFL(GetModuleName(HInstance), ver);
  Result := Format('%d.%0.2d', [ver.Major, ver.Minor])
end;

function GetModuleVersionDFL(ModuleFileName: string; var Ver: TVersion; Product: Boolean = False): string;
var
 VersionBufferLength: Integer;
 PVersionBuffer     : Pointer;
 Dummy              : Dword;
 PFixedFileInfo     : PVS_FIXEDFILEINFO;
 ModuleVersionLength: Dword;
 VerW1, VerW2       : DWord;
begin
  Ver.Major := 0;
  Ver.Minor := 0;
  Ver.Release := 0;
  Ver.Build := 0;
  VersionBufferLength:= GetFileVersionInfoSize(PChar(ModuleFileName),Dummy);
  PVersionBuffer:= AllocMem(VersionBufferLength);
  if (PVersionBuffer <> nil) then
   begin
    if (GetFileVersionInfo(PChar(ModuleFileName), VersionBufferLength,VersionBufferLength, PVersionBuffer)) then
     begin
      if (VerQueryValue(PVersionBuffer, '\', Pointer(PFixedFileInfo),ModuleVersionLength)) then
       begin
        if Product then
         begin
          VerW1 := PFixedFileInfo^.dwProductVersionMS;
          VerW2 := PFixedFileInfo^.dwProductVersionLS;
         end
        else
         begin
          VerW1 := PFixedFileInfo^.dwFileVersionMS;
          VerW2 := PFixedFileInfo^.dwFileVersionLS;
         end;
        Ver.Major :=   ((VerW1) and $FFFF0000) shr 16;
        Ver.Minor :=   ((VerW1) and $0000FFFF);
        Ver.Release := ((VerW2) and $FFFF0000) shr 16;
        Ver.Build :=   ((VerW2) and $0000FFFF);
        Result := Format('%d.%d.%d.%d', [Ver.Major, Ver.Minor, Ver.Release,Ver.Build]);
       end;
     end
    else
     begin
      Result:= TEXT_NO_VERSIONINFO;
     end;
    FreeMem(PVersionBuffer);
   end
  else
   begin
    Result:= TEXT_NO_VERSIONINFO;
   end;
end;

function DetectCPUType(Family, Model: Integer): String;
var
 P2Array : array[0..5] of Integer; // Family : 6
 P3Array : array[0..5] of Integer; // Family : 6
 P4Array : array[0..5] of Integer; // Family : 15
 XPArray : array[0..5] of Integer; // Family : 6
 PrescottArray : array[0..5] of Integer; // Family : 15
 OpteronArray : array[0..5] of Integer; // Family : 15
 PentiumMArray : array[0..5] of Integer; // Family : 6
 AthlonArray : array[0..5] of Integer; // Family : 6
 Athlon64Array : array[0..5] of Integer; // Family : 15
 CPUDetected : Boolean;
 I : Integer;
 Vendor: String;

begin
 P2Array[0] := 5; //0101
 P3Array[0] := 7; // 0111
 P3Array[1] := 8; // 1000
 P3Array[2] := 10; // 1010
 P3Array[3] := 11; // 1011
 P4Array[0] := 0; // 0000
 P4Array[1] := 1; // 0001
 P4Array[2] := 2; // 0010
 XPArray[0] := 6; // 0110
 XPArray[1] := 8; // 1000
 XPArray[2] := 10; // 1010
 PrescottArray[0] := 3; // 0011
 OpteronArray[0] := 5; // 0101
 AthlonArray[0] := 4; // 0100
 Athlon64Array[0] := 4; // 0100
 CPUDetected := False;
 Vendor := GetCPUVendor;
 begin
  if Vendor = 'GenuineIntel' then
   begin
    // Intel processors detection
    if Family = 6 then
    begin
     for I := 0 to 5 do
     begin
      if Model = P3Array[I] then
       begin
        Result := 'Pentium 3';
        Exit;
       end;
     end;
    end;
    if Family = 6 then
    begin
     for I := 0 to 5 do
     begin
      if Model = P2Array[I] then
       begin
        Result := 'Pentium 2';
        Exit;
       end;
     end;
    end;
    if Family = 6 then
    begin
     for I := 0 to 5 do
     begin
      if Model = PentiumMArray[I] then
       begin
        Result := 'Pentium M';
        Exit;
       end;
     end;
    end;
    if Family = 15 then
    begin
     for I := 0 to 5 do
     begin
      if Model = P4Array[I] then
       begin
        Result := 'P4 Northwood';
        Exit;
       end;
     end;
    end;
    if Family = 15 then
    begin
     for I := 0 to 5 do
     begin
      if Model = PrescottArray[I] then
       begin
        Result := 'P4 Prescott';
        Exit;
       end;
     end;
    end;
   end;
  if Vendor = 'AuthenticAMD' then
   begin
    // AMD processor detection
    if Family = 6 then
    begin
     for I := 0 to 5 do
     begin
      if Model = XPArray[I] then
       begin
        Result := 'Athlon XP';
        Exit;
       end;
     end;
    end;
    if Family = 6 then
    begin
     for I := 0 to 5 do
     begin
      if Model = AthlonArray[I] then
       begin
        Result := 'Athlon';
        Exit;
       end;
     end;
    end;
    if Family = 15 then
    begin
     for I := 0 to 5 do
     begin
      if Model = Athlon64Array[I] then
       begin
        Result := 'Athlon 64';
        Exit;
       end;
     end;
    end;
    if Family = 15 then
    begin
     for I := 0 to 5 do
     begin
      if Model = OpteronArray[I] then
       begin
        Result := 'Opteron';
        Exit;
       end;
     end;
    end;
   end;
 end;
 if CPUDetected = False then
  begin
   Result := 'Not detected';
   Exit;
  end;
end;

function GetCPUFrequencyMHz : Cardinal;

  function RDTSC : Int64;
  asm
   rdtsc
  end;

var
 RDTSCCountStart, RDTSCCountEnd, RDTSCTicks, lpFrequency, lpPerformanceCount,
 StartCount, EndCount, NoOfTicks : Int64;
 I1, I2 : Cardinal;
 J : Int64;
 Succes : Boolean;
 RunTimeSec : Double;
const
 NOOFRUNS : Cardinal = 1000;

begin
 Succes := QueryPerformanceFrequency(lpFrequency);
 if not Succes then
  raise Exception.Create('QueryPerformanceFrequency failed');
 Succes := QueryPerformanceCounter(lpPerformanceCount);
 if Succes then
  StartCount := lpPerformanceCount
 else
  raise Exception.Create('QueryPerformanceCounter failed');
 RDTSCCountStart := RDTSC;
 //Do something for a while
 J := 0;
 for I1 := 0 to NOOFRUNS do
  begin
   for I2 := 0 to NOOFRUNS do
    begin
     J := J + 1;
    end;
  end;
 RDTSCCountEnd := RDTSC;
 Succes := QueryPerformanceCounter(lpPerformanceCount);
 if Succes then
  EndCount := lpPerformanceCount
 else
  raise Exception.Create('QueryPerformanceCounter failed');
 NoOfTicks := EndCount - StartCount;
 if NoOfTicks < 0 then
  raise Exception.Create('Performance counter wrapped around');
 RunTimeSec := NoOfTicks / lpFrequency;
 RDTSCTicks := RDTSCCountEnd - RDTSCCountStart;
 if RDTSCTicks < 0 then
  raise Exception.Create('Time stamp counter counter wrapped around');
 Result := Round(RDTSCTicks / RunTimeSec / 1000000);
end;

end.
