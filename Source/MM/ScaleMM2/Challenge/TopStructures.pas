{****************************************************************************************

  TOPMEMORY v1.1 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2004 Ivo Tops, Topsoftware
            Based on some Concepts and Code from Robert Lee's MultiMM

  Free for use in Alex Applications on condition that bugfixes will be reported
  to support@fileprocessor.com

****************************************************************************************}
unit TopStructures;

interface

const
  cMaxAppBlocks = 255; // MaxNumber of Appblocks in an OSBlock limited by using Byte for some structures

type
  TPointerArray = array[0..MaxInt div (SizeOf(Pointer)) - 1] of Pointer;
  PPointerArray = ^TPointerArray;

type
  TIntegerArray = array[0..MaxInt div (SizeOf(Integer)) - 1] of Integer;
  PIntegerArray = ^TIntegerArray;

type
  TBoolArray = array[0..MaxInt div (SizeOf(Boolean)) - 1] of Boolean;
  PBoolArray = ^TBoolArray;

type
  TByteArray = array[0..MaxInt div (SizeOf(Byte)) - 1] of Byte;
  PByteArray = ^TByteArray;

{$IFDEF TOPDEBUG}
procedure DumpAllocStructureToDisk;
procedure MemAssert(const S: string);

var
  Log: string;
{$ENDIF}

implementation

uses windows
{$IFDEF TOPDEBUG}
  , TopPointerLookupList
  , TopInstall
{$ENDIF};

{$IFDEF TOPDEBUG}
procedure MemAssert(const S: string);
// Set BreakPoint here and compile with DEBUGINFO
// Assertions just give weird errors in a memmanager
var
  F: Textfile;
begin
  TopMM.Lock;
  AssignFile(F, 'TOPMMDEBUG.ERR');
  ReWrite(F);
  Writeln(F, S);
  CloseFile(F);
  Sleep(1000);
  Halt(0);
end;
{$ENDIF}

{$IFDEF TOPDEBUG}
procedure DumpAllocStructureToDisk;
var
  S: string;
  F: Textfile;
begin
  S := TopMM.GlobalPLL.GetInfo;
  AssignFile(F, 'TOPMMSTRUCT.LOG');
  ReWrite(F);
  Writeln(F, S);
  CloseFile(F);
end;
{$ENDIF}

end.

