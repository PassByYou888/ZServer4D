
{$ifdef win64}
function cmp_mem(p1, p2: Pointer; len: Integer): Boolean;
label
  cmp_small, cmp_succ, cmp_fail;
var
  q1, q2: PInt64;
begin
  if len <= 0 then
  begin
    result := true;
    exit;
  end;
  q1 := p1;
  q2 := p2;
  if len < 8 then
    goto cmp_small;
  repeat
    if q1^ = q2^ then
    begin
      dec(len, 8);
      if len = 0 then
        goto cmp_succ
      else
      if len < 8 then
        goto cmp_small;
      inc(q1);
      inc(q2);
    end else
      goto cmp_fail;
  until false;

cmp_small:
  if len >= 4 then
  begin
    result := PInteger(q1)^ = PInteger(q2)^;
    if not result then
      goto cmp_fail;
    dec(len, 4);
    if len >= 2 then
    begin
      inc(PAnsiChar(q1), 4);
      inc(PAnsiChar(q2), 4);
      result := PWord(q1)^ = PWord(q2)^;
      if not result then
        goto cmp_fail
      else
      dec(len, 2);
      if len >= 1 then
      begin
        inc(PAnsiChar(q1), 2);
        inc(PAnsiChar(q2), 2);
        result := PByte(q1)^ = PByte(q2)^;
      end else
        goto cmp_succ;
    end else
    if len >= 1 then
      result := PByte(q1)^ = PByte(q2)^
    else
      goto cmp_succ;
  end else
  if len >= 2 then
  begin
    result := PWord(q1)^ = PWord(q2)^;
    if not result then
      goto cmp_fail;
    dec(len, 2);
    if len >= 1 then
    begin
      inc(PAnsiChar(q1), 2);
      inc(PAnsiChar(q2), 2);
      result := PByte(q1)^ = PByte(q2)^;
    end else
      goto cmp_succ;
  end else
  if len >= 1 then
    result := PByte(q1)^ = PByte(q2)^
  else
    goto cmp_succ;

  exit;

cmp_succ:
  result := true;
  exit;
cmp_fail:
  result := false;
end;

{$else}
(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function CompareMem is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)

function cmp_mem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
   add   eax, ecx
   add   edx, ecx
   xor   ecx, -1
   add   eax, -8
   add   edx, -8
   add   ecx, 9
   push  ebx
   jg    @Dword
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   lea   ebx, [eax+ecx]
   add   ecx, 4
   and   ebx, 3
   sub   ecx, ebx
   jg    @Dword
@DwordLoop:
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   mov   ebx, [eax+ecx+4]
   cmp   ebx, [edx+ecx+4]
   jne   @Ret0
   add   ecx, 8
   jg    @Dword
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   mov   ebx, [eax+ecx+4]
   cmp   ebx, [edx+ecx+4]
   jne   @Ret0
   add   ecx, 8
   jle   @DwordLoop
@Dword:
   cmp   ecx, 4
   jg    @Word
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   add   ecx, 4
@Word:
   cmp   ecx, 6
   jg    @Byte
   movzx ebx, word ptr [eax+ecx]
   cmp   bx, [edx+ecx]
   jne   @Ret0
   add   ecx, 2
@Byte:
   cmp   ecx, 7
   jg    @Ret1
   movzx ebx, byte ptr [eax+7]
   cmp   bl, [edx+7]
   jne   @Ret0
@Ret1:
   mov   eax, 1
   pop   ebx
   ret
@Ret0:
   xor   eax, eax
   pop   ebx
end;
{$endif}