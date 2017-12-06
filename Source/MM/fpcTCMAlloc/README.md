# fpc-tcmalloc
A fast freepascal memory manager using [TCMalloc](http://goog-perftools.sourceforge.net/doc/tcmalloc.html).

To build, first run ``build-tcmalloc.sh`` with root permissions (it'll have to install TCMalloc on your system).
Then, in every project where you want to use TCMalloc as the allocator, make sure the unit ``fpc_tcmalloc`` is the first unit included in your program. That way, no other units can allocate memory before this unit initializes.

This should work on every UNIX-like system. For windows, you'll have to build TCMalloc/gperftools yourself. For more information, see their [README_windows.txt](gperftools/README_windows.txt) file.

## LICENSE
Both the TCMalloc code included in the gperftools subrepository and my additions are licensed under the BSD-3-Clause license. See [the COPYING file of gperftools](gperftools/COPYING) and [the LICENSE file](LICENSE).

