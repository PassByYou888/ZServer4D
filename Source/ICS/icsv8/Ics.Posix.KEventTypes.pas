{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
Creation:     October 30, 2011
Description:  kevent types
Version:      0.9 Beta
EMail:        <arno.garrels@gmx.de>
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2011-2011 by Arno Garrels, Berlin, Germany,
              <arno.garrels@gmx.de>

              This software is freeware and provided 'as-is', without any
              express or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of this
              software.

              The following restrictions apply:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.


History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Ics.Posix.KEventTypes;

{.$DEFINE HAS_EVFILT_NETDEV}

interface
{$IFDEF POSIX}
  {$HPPEMIT '#include <event.h>' } // ToBeChecked

type  { should be defined in types.h }
  uintptr_t = NativeUInt;
  {$EXTERNALSYM uintptr_t}
  int16_t   = SmallInt;
  {$EXTERNALSYM int16_t}
  uint16_t  = Word;
  {$EXTERNALSYM uint16_t}
  uint32_t  = LongWord;
  {$EXTERNALSYM uint32_t}
  intptr_t  = NativeInt;
  {$EXTERNALSYM intptr_t}
  int64_t   = Int64;
  {$EXTERNALSYM int64_t}
  uint64_t  = UInt64;
  {$EXTERNALSYM uint64_t}

const
  EVFILT_READ     = -1;
  {$EXTERNALSYM EVFILT_READ}
  EVFILT_WRITE    = -2;
  {$EXTERNALSYM EVFILT_WRITE}
  EVFILT_AIO      = -3;  { attached to aio requests }
  {$EXTERNALSYM EVFILT_AIO}
  EVFILT_VNODE    = -4;  { attached to vnodes }
  {$EXTERNALSYM EVFILT_VNODE}
  EVFILT_PROC     = -5;  { attached to struct proc }
  {$EXTERNALSYM EVFILT_PROC}
  EVFILT_SIGNAL   = -6;  { attached to struct proc }
  {$EXTERNALSYM EVFILT_SIGNAL}
  EVFILT_TIMER    = -7;  { timers }
  {$EXTERNALSYM EVFILT_TIMER}
{$IFNDEF HAS_EVFILT_NETDEV}
  EVFILT_MACHPORT = -8;  { Mach portsets } // MacOS
  {$EXTERNALSYM EVFILT_MACHPORT}
{$ELSE}
  EVFILT_NETDEV   = -8; { network devices }
  {$EXTERNALSYM EVFILT_NETDEV}
{$ENDIF HAS_EVFILT_NETDEV}
  EVFILT_FS       = -9;  { filesystem events }
  {$EXTERNALSYM EVFILT_FS}
  EVFILT_USER     = -10; { User events }
  {$EXTERNALSYM EVFILT_USER}
  EVFILT_SESSION  = -11; { Audit session events }
  {$EXTERNALSYM EVFILT_SESSION}

  EVFILT_SYSCOUNT = 11;
  {$EXTERNALSYM EVFILT_SYSCOUNT}
  EVFILT_THREADMARKER = EVFILT_SYSCOUNT; { Internal use only }
  {$EXTERNALSYM EVFILT_THREADMARKER}

  { actions }
  EV_ADD          = $0001;  { add event to kq }
  {$EXTERNALSYM EV_ADD}
  EV_DELETE       = $0002;  { delete event from kq  }
  {$EXTERNALSYM EV_DELETE}
  EV_ENABLE       = $0004;  { enable event  }
  {$EXTERNALSYM EV_ENABLE}
  EV_DISABLE      = $0008;  { disable event (not reported)  }
  {$EXTERNALSYM EV_DISABLE}
  EV_RECEIPT      = $0040;  { force EV_ERROR on success, data == 0 }
  {$EXTERNALSYM EV_RECEIPT}

{ flags  }
  EV_ONESHOT      = $0010;  { only report one occurrence  }
  {$EXTERNALSYM EV_ONESHOT}
  EV_CLEAR        = $0020;  { clear event state after reporting  }
  {$EXTERNALSYM EV_CLEAR}
  EV_DISPATCH     = $0080;  { disable event after reporting }
  {$EXTERNALSYM EV_DISPATCH}

  EV_SYSFLAGS     = $F000;  { reserved by system  }
  {$EXTERNALSYM EV_SYSFLAGS}
  EV_FLAG0        = $1000;  { filter-specific flag }
  {$EXTERNALSYM EV_FLAG0}
  EV_FLAG1        = $2000;  { filter-specific flag  }
  {$EXTERNALSYM EV_FLAG1}

{ returned values  }
  EV_EOF          = $8000;  { EOF detected  }
  {$EXTERNALSYM EV_EOF}
  EV_ERROR        = $4000;  { error, data contains errno  }
  {$EXTERNALSYM EV_ERROR}

 {*
 * Filter specific flags for EVFILT_READ
 *
 * The default behavior for EVFILT_READ is to make the "read" determination
 * relative to the current file descriptor read pointer. The EV_POLL
 * flag indicates the determination should be made via poll(2) semantics
 * (which always returns true for regular files - regardless of the amount
 * of unread data in the file).
 *
 * On input, EV_OOBAND specifies that only OOB data should be looked for.
 * The returned data count is the number of bytes beyond the current OOB marker.
 *
 * On output, EV_OOBAND indicates that OOB data is present
 * If it was not specified as an input parameter, then the data count is the
 * number of bytes before the current OOB marker. If at the marker, the
 * data count indicates the number of bytes available after it.  In either
 * case, it's the amount of data one could expect to receive next.
 *}
  EV_POLL         = EV_FLAG0;
  {$EXTERNALSYM EV_POLL}
  EV_OOBAND       = EV_FLAG1;
  {$EXTERNALSYM EV_OOBAND}

{*
 * data/hint fflags for EVFILT_USER, shared with userspace
 *}

{*
 * On input, NOTE_TRIGGER causes the event to be triggered for output.
 *}
  NOTE_TRIGGER    = $01000000;
  {$EXTERNALSYM NOTE_TRIGGER}
  EV_TRIGGER      = $0100; // deprecated--for backwards compatibility only
  {$EXTERNALSYM EV_TRIGGER}

{*
 * On input, the top two bits of fflags specifies how the lower twenty four
 * bits should be applied to the stored value of fflags.
 *
 * On output, the top two bits will always be set to NOTE_FFNOP and the
 * remaining twenty four bits will contain the stored fflags value.
 *}
  NOTE_FFNOP      = $00000000;              { ignore input fflags }
  {$EXTERNALSYM NOTE_FFNOP}
  NOTE_FFAND      = $40000000;              { and fflags }
  {$EXTERNALSYM NOTE_FFAND}
  NOTE_FFOR       = $80000000;              { or fflags }
  {$EXTERNALSYM NOTE_FFOR}
  NOTE_FFCOPY     = $c0000000;              { copy fflags }
  {$EXTERNALSYM NOTE_FFCOPY}
  NOTE_FFCTRLMASK = $c0000000;              { mask for operations }
  {$EXTERNALSYM NOTE_FFCTRLMASK}
  NOTE_FFLAGSMASK = $00ffffff;
  {$EXTERNALSYM NOTE_FFLAGSMASK}

{ data/hint flags for EVFILT_READ|WRITE, shared with userspace   }
  NOTE_LOWAT      = $00000001;  { low water mark  }
  {$EXTERNALSYM NOTE_LOWAT}

{ data/hint flags for EVFILT_VNODE, shared with userspace  }
  NOTE_DELETE     = $00000001;  { vnode was removed  }
  {$EXTERNALSYM NOTE_DELETE}
  NOTE_WRITE      = $00000002;  { data contents changed  }
  {$EXTERNALSYM NOTE_WRITE}
  NOTE_EXTEND     = $00000004;  { size increased  }
  {$EXTERNALSYM NOTE_EXTEND}
  NOTE_ATTRIB     = $00000008;  { attributes changed  }
  {$EXTERNALSYM NOTE_ATTRIB}
  NOTE_LINK       = $00000010;  { link count changed  }
  {$EXTERNALSYM NOTE_LINK}
  NOTE_RENAME     = $00000020;  { vnode was renamed  }
  {$EXTERNALSYM NOTE_RENAME}
  NOTE_REVOKE     = $00000040;  { vnode access was revoked  }
  {$EXTERNALSYM NOTE_REVOKE}
  NOTE_NONE       = $00000080;  { No specific vnode event: to test for EVFILT_READ activation }
  {$EXTERNALSYM NOTE_NONE}

{ data/hint flags for EVFILT_PROC, shared with userspace   }
  NOTE_EXIT       = $80000000;  { process exited  }
  {$EXTERNALSYM NOTE_EXIT}
  NOTE_FORK       = $40000000;  { process forked  }
  {$EXTERNALSYM NOTE_FORK}
  NOTE_EXEC       = $20000000;  { process exec'd  }
  {$EXTERNALSYM NOTE_EXEC}
  NOTE_PCTRLMASK  = $f0000000;  { mask for hint bits  }
  {$EXTERNALSYM NOTE_PCTRLMASK}
  NOTE_PDATAMASK  = $000fffff;  { mask for pid  }
  {$EXTERNALSYM NOTE_PDATAMASK}
  //NOTE_PCTRLMASK  (~NOTE_PDATAMASK) { ?? }

{*
 * data/hint fflags for EVFILT_TIMER, shared with userspace.
 * The default is a (repeating) interval timer with the data
 * specifying the timeout interval in milliseconds.
 *
 * All timeouts are implicitly EV_CLEAR events.
 *}
  NOTE_SECONDS    = $00000001;    { data is seconds         }
  {$EXTERNALSYM NOTE_SECONDS}
  NOTE_USECONDS   = $00000002;    { data is microseconds    }
  {$EXTERNALSYM NOTE_USECONDS}
  NOTE_NSECONDS   = $00000004;    { data is nanoseconds     }
  {$EXTERNALSYM NOTE_NSECONDS}
  NOTE_ABSOLUTE   = $00000008;    { absolute timeout        }
  {$EXTERNALSYM NOTE_ABSOLUTE}
      { ... implicit EV_ONESHOT }

{*
 * data/hint fflags for EVFILT_MACHPORT, shared with userspace.
 *
 * Only portsets are support at this time.
 *
 * The fflags field can optionally contain the MACH_RCV_MSG, MACH_RCV_LARGE,
 * and related trailer receive options as defined in <mach/message.h>.
 * The presence of these flags directs the kevent64() call to attempt to receive
 * the message during kevent delivery, rather than just indicate that a message exists.
 * On setup, The ext[0] field contains the receive buffer pointer and ext[1] contains
 * the receive buffer length.  Upon event delivery, the actual received message size
 * is returned in ext[1].  As with mach_msg(), the buffer must be large enough to
 * receive the message and the requested (or default) message trailers.  In addition,
 * the fflags field contains the return code normally returned by mach_msg().
 *
 * If no message receipt options were provided in the fflags field on setup, no
 * message is received by this call. Instead, on output, the data field simply
 * contains the name of the actual port detected with a message waiting.
 *}

{*
 * data/hint fflags for EVFILT_SESSION, shared with userspace.
 *
 * The kevent ident field should be set to AU_SESSION_ANY_ASID if interested
 * in events for any session.
 *
 * NOTE_AS_UPDATE may be going away since struct auditinfo_addr may become
 * immutable once initially set.
 *}
  NOTE_AS_START   = $00000001;  { start of new session }
  {$EXTERNALSYM NOTE_AS_START}
  NOTE_AS_END     = $00000002;  { start of new session }
  {$EXTERNALSYM NOTE_AS_END}
  NOTE_AS_ERR     = $00000004;  { error tracking new session }
  {$EXTERNALSYM NOTE_AS_ERR}
  NOTE_AS_CLOSE   = $00000008;  { currently unsupported }
  {$EXTERNALSYM NOTE_AS_CLOSE}
  NOTE_AS_UPDATE  = $00000010;  { session data updated }
  {$EXTERNALSYM NOTE_AS_UPDATE}

{*
 * Kevent ident value for any session.
 *}
  AS_ANY_ASID     = $FFFFFFFF;
  {$EXTERNALSYM AS_ANY_ASID}

{*
 * DEPRECATED!!!!!!!!!
 * NOTE_TRACK, NOTE_TRACKERR, and NOTE_CHILD are no longer supported as of 10.5
 *}
{ additional flags for EVFILT_PROC }
  NOTE_TRACK      = $00000001;  { follow across forks  }
  {$EXTERNALSYM NOTE_TRACK}
  NOTE_TRACKERR   = $00000002;  { could not track child  }
  {$EXTERNALSYM NOTE_TRACKERR}
  NOTE_CHILD      = $00000004;  { am a child process  }
  {$EXTERNALSYM NOTE_CHILD}

{$IFDEF HAS_EVFILT_NETDEV}
{ data/hint flags for EVFILT_NETDEV, shared with userspace  }
  NOTE_LINKUP     = $0001;  { link is up  }
  NOTE_LINKDOWN   = $0002;  { link is down  }
  NOTE_LINKINV    = $0004;  { link state is invalid  }
{$ENDIF HAS_EVFILT_NETDEV}

type
  //#pragma pack(4)
  {$A4}
  kevent = record
    Ident  : uintptr_t;    { identifier for this event }
    Filter : int16_t;      { filter for event }
    Flags  : uint16_t;     { action flags for kqueue }
    FFlags : uint32_t;     { filter flag value }
    Data   : intptr_t;     { filter data value }
    uData  : Pointer;      { opaque user data identifier }
  end;
  {$EXTERNALSYM kevent}
  TKEvent = kevent;
  PKEvent = ^TKEvent;

//#pragma pack() //Sets the alignment to the one that was in effect when compilation started
  {$A8} // shouldn't matter
  kevent64_s = record
    ident   : uint64_t;                   { identifier for this event }
    filter  : int16_t;                    { filter for event }
    flags   : uint16_t;                   { general flags }
    fflags  : uint32_t;                   { filter-specific flags }
    data    : int64_t;                    { filter-specific data }
    udata   : uint64_t;                   { opaque user data identifier }
    ext     : array [0..1] of uint64_t;   { filter-specific extensions }
  end;
  {$EXTERNALSYM kevent64_s}
  TKEvent64_s = kevent64_s;
  PKEvent64_s = ^TKEvent64_s;

  au_sentry = record                    { Audit session entry }
  end;
  {$EXTERNALSYM au_sentry}

procedure EV_SET(kevp: PKEvent; const aIdent: UIntPtr; const aFilter: Int16;
                 const aFlags: UInt16; const aFFlags: UInt32;
                 const aData: IntPtr; const auData: Pointer); inline;
{$EXTERNALSYM EV_SET}

procedure EV_SET64(kevp: PKEvent64_s; const aIdent: UInt64; const aFilter: Int16;
                   const aFlags: UInt16; const aFFlags: UInt32; const aData: Int64;
                   const auData: UInt64; ext0, ext1: UInt64); inline;
{$EXTERNALSYM EV_SET64}
{$ENDIF POSIX}

implementation

{$IFDEF POSIX}
procedure EV_SET(kevp: PKEvent; const aIdent: UIntPtr; const aFilter: Int16;
                 const aFlags: UInt16; const aFFlags: UInt32;
                 const aData: IntPtr; const auData: Pointer);
begin
  kevp^.Ident  := aIdent;
  kevp^.Filter := aFilter;
  kevp^.Flags  := aFlags;
  kevp^.FFlags := aFFlags;
  kevp^.Data   := aData;
  kevp^.uData  := auData;
end;

procedure EV_SET64(kevp: PKEvent64_s; const aIdent: UInt64; const aFilter: Int16;
                 const aFlags: UInt16; const aFFlags: UInt32; const aData: Int64;
                 const auData: UInt64; ext0, ext1: UInt64);
begin
  kevp^.Ident  := aIdent;
  kevp^.Filter := aFilter;
  kevp^.Flags  := aFlags;
  kevp^.FFlags := aFFlags;
  kevp^.Data   := aData;
  kevp^.uData  := auData;
  kevp^.ext[0] := Ext0;
  kevp^.ext[1] := Ext1;
end;
{$ENDIF POSIX}
end.

