{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
Creation:     October 30, 2011
Description:  kevent API
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
unit Ics.Posix.KEventApi;

interface
{$IFDEF POSIX}
uses
  Posix.Base, Posix.Time,
  Ics.Posix.KEventTypes;

function kqueue: Integer; cdecl; external libc name _PU + 'kqueue';
{$EXTERNALSYM kqueue}

function kevent(kq: Integer; ChangeList: PKEvent; nChanged: Integer;
                EventList: PKevent; nEvents: Integer; Timeout: PTimeSpec): Integer; cdecl;
                external libc name _PU + 'kevent';
{$EXTERNALSYM kevent}

function kevent64(kq: Integer; ChangeList: PKEvent64_s; nChanged: Integer;
                EventList: PKEvent64_s; nEvents: Integer; Timeout: PTimeSpec): Integer; cdecl;
                external libc name _PU + 'kevent64';
{$EXTERNALSYM kevent64}
{$ENDIF POSIX}
implementation

end.
