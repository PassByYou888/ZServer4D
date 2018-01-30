
## What is this

implementation of the MD5 high performance algorithm with delphi


## platform and architecture

processor architecture: x86,x64

supports platform: Win32,Win64


compatible: https and openssl

file support: greater than 4G



## about author
fastMD5 assembler algorithm by Maxim Masiutin

https://github.com/maximmasiutin/MD5_Transform-x64



## Advantages and Tech details
The main advantage of this 64-bit version is that
it loads 64 bytes of hashed message into 8 64-bit registers
(RBP, R8, R9, R10, R11, R12, R13, R14) at the beginning,
to avoid excessive memory load operations
througout the routine.

To operate with 32-bit values store in higher bits
of a 64-bit register (bits 32-63) uses "Ror" by 32;
8 macro variables (M1-M8) are used to keep record
or corrent state of whether the register has been
Ror'ed or not.

It also has an ability to use Lea instruction instead
of two sequental Adds (uncomment UseLea=1), but it is
slower on Skylake processors. Also, Intel in the
Optimization Reference Maual discourages us of
Lea as a replacement of two adds, since it is slower
on the Atom processors.

MD5_Transform-x64 is released under a dual license,
and you may choose to use it under either the
Mozilla Public License 2.0 (MPL 2.1, available from
https://www.mozilla.org/en-US/MPL/2.0/) or the
GNU Lesser General Public License Version 3,
dated 29 June 2007 (LGPL 3, available from
https://www.gnu.org/licenses/lgpl.html).

MD5_Transform-x64 is based
on the following code by Peter Sawatzki.

The original notice by Peter Sawatzki follows.






## Usage delphi

```Delphi

FastMD5(@buff[0], length(buff));

```


delphi implementation by 600585@qq.com

https://github.com/PassByYou888/FastMD5/



2018-1-27
