The words "host" and "target" have their usual compiler-jargon meanings.

The word "platform" means a specific operating system and cpu
architecture combination.  Some cpu architectures have both 32- and
64-bit variants.  The PowerPC architecture looks almost the same
except for word size.  On x86, the 64-bit variant extends the 32-bit
variant with new instructions and more registers with new names.
The 32- and 64-bit ARM architectures are totally different.

CCL is written in itself.  Thus, when porting to a new platform, we
start by defining the target backend in a host CCL.

The backend files are found in directories like ccl:compiler;ARM; (for
32-bit ARM) and ccl:compiler;X86; (for x86).

When adding a new cpu architecture, we have to define an architecture
description (see ccl:compiler;**;*-arch.lisp for examples).  This
description contains information about the target cpu and the data
tagging scheme to be used (including things like a mapping between
keywords that name vector types and the actual 8-bit subtags used to
implement them).

The backend struct contains some additional platform-specific data
such as some FFI details, fasl extension, etc.

At some point, the cross-dumper will need to know about the target.
Update the xload-target-backend definitions in ccl:xload;x*xfasload.lisp.

In order to cross-compile anything, ensure tha the backend structure
for the target is on the list of known backends.  Ensure that
(find-backend :darwinarm64) (or :linuxriscv32 or whatever) finds the
backend structure.

Defining the backend should associate a "foreign type data" (FTD)
structure with it.  Install the standard foreign types in that
ftd via install-standard-foreign-types.

There are also a couple of interfaces that describe some of the details
of ff-calls and callbacks.  Most of the details have to do with how
structs are returned and passed by value.

At some point, you'll need to create interface databases (.cdb files)
for the target and make those available on the host.  The ccl-ffigen
tool can help with this.

Update the "module" definitions in ccl:lib;compile-ccl.lisp.
Ensure that things like target-xload-modules define the right things
for the target.

There are some functions (possibly conditionalized out) with names
like target-xcompile-lambda that are supposed to compile a lambda
expression using the specified backend and disassemble the result.
This should work for a wide variety of functions before trying to
write fasl files.

Functions are represented in platform-specific ways.  Due to operating
system limitations, on darwinarm64 functions will need to have
a code-vector slot (containing read-only instructions), and this will
need to be a MAP_JIT region.

Some level-1 files (l1-boot*.lisp, level-1.lisp) define the order
in which fasl files get loaded.  Conditionalize things so that the
right compiler backend and other platform-specific files get loaded.
There's some junk in l1-boot-1.lisp about decoding an integer that
encodes os/word-size/cpu platform information.  We're getting close
to being out of space in that integer, so this might need updating
sooner or later.

Once all the infrastructure is in place, cross-compiling is not too
bad.

First, ensure the desried backend is loaded, perhaps by writing a
little .lisp file to do it for you.

Then, load the backend-specific parts of the compiler into the host.

Finally, (cross-compile-ccl :<target>) will write fasls, and
(cross-xload-level-0 :<target>) will dump a boot image.



### Tagging

Best to stick to low tags, although TBI hightags are highly tempting.

;;; It is very tempting to rely on the arm64 TBI feature (where the
;;; top 8 bits of addresses are ignored) to use a hightag scheme.
;;; There are two main reasons I think it is better (i.e., lower risk,
;;; less effort) to stick to a lowtag scheme.
;;;
;;; The first is uncertainty over the future availability of the
;;; feature.  As of today, macOS and Linux (at least) enable the TBI
;;; feature.  But I think there is long-term risk that memory safety
;;; features like ARM's Memory Tagging Extension and Apple's Memory
;;; Integrity Enforcement will become widely adopted, and they are
;;; incompatible with the TBI feature.  A lowtag scheme doesn't rely
;;; on any special hardware or operating system support.
;;;
;;; The other reason is consistency with the other ports.  There is a
;;; lot of existing logic within the implementation of CCL that works
;;; a lowtag representation.  Reimplementing and separately
;;; maintaining that logic for hightags seems like effort that would
;;; be better avoided.


Future memory safety tricks might imperil TBI. I'm thinking of ARM's
Memory Tagging Extension, and Apple's enhanced Memory Integrity
Enforcement.

https://security.apple.com/blog/memory-integrity-enforcement/

## General considerations

CCL allocates lisp objects in memory on double-node (dnode)
boundaries.

On a 32-bit platform, a dnode is 8 bytes (64 bits) long.  That means
that the low three bits of an address are redundant: we only really
need to the upper 29 bits to know the address of a dnode-aligned
object.

On a 64-bit platform, a dnode is 16 bytes (128 bits) long, and the low
four bits are redundant in a similar way: the upper 60 bits are sufficient
to address a dnode-aligned object.

We call the redundant low 3 or 4 bits the "tag bits."

Tagging considerations:
 * It's important to quickly recognize fixnums.
 * It's important to quickly recognize lists (for car/cdr);  it's also
   desirable to quickly recognize cons cells.
 * It's desirable for vectorp, arrayp, and specific-array-type-p to be
   fast.  We need at least 12 immediate CL vector types:
     * {signed,unsigned}-byte {8,16,32,64}
     * single-float, double-float
     * bit
     * at least one character type
   As node types, we need:
     * simple-array
     * vector-header
     * array-header
   
When defining subtags, certain ports use order as a shortcut.  For instance,
if CL array types are in orde


## x86-64 tags

## x86-64 uvector subtags

The subtags are ordered for the benefit of arrayp, vectorp
also ivector-typecode-p, gvector-typecode-p

SINGLE-FLOAT                          1
SYMBOL                            10101
RATIO                             10110
BIGNUM                            11001
MACPTR                            11010
CATCH-FRAME                      100101
COMPLEX                          100110
DOUBLE-FLOAT                     101001
HASH-VECTOR                      110101
STRUCT                           110110
XCODE-VECTOR                     111001
POOL                            1000101
ISTRUCT                         1000110
COMPLEX-SINGLE-FLOAT            1001001
POPULATION                      1010101
VALUE-CELL                      1010110
COMPLEX-DOUBLE-FLOAT            1011001
PACKAGE                         1100101
SLOT-VECTOR                     1110101
LOCK                            1110110
BASIC-STREAM                   10000101
INSTANCE                       10000110
FUNCTION                       10010101
COMPLEX-DOUBLE-FLOAT-VECTOR    10010111
MIN-CL-IVECTOR-SUBTAG          10010111
ARRAY-HEADER                   10100101
VECTOR-HEADER                  10100110
SIGNED-16-BIT-VECTOR           10100111
SIMPLE-VECTOR                  10110110
UNSIGNED-16-BIT-VECTOR         10110111
COMPLEX-SINGLE-FLOAT-VECTOR    10111010
SIMPLE-STRING                  11001001
FIXNUM-VECTOR                  11001010
SIGNED-8-BIT-VECTOR            11010111
SIGNED-32-BIT-VECTOR           11011001
SIGNED-64-BIT-VECTOR           11011010
UNSIGNED-8-BIT-VECTOR          11100111
UNSIGNED-32-BIT-VECTOR         11101001
UNSIGNED-64-BIT-VECTOR         11101010
BIT-VECTOR                     11110111
SINGLE-FLOAT-VECTOR            11111001
DOUBLE-FLOAT-VECTOR            11111010

Note that the 64-bit subtags end in #b1010

32-bit: #b1001 (fulltag-immheader-1)
64-bit: #b1010 (fulltag-immheader-2)

nodeheader-0 (#b0101): (10 values)
      2:SYMBOL                            10101
      6:CATCH-FRAME                      100101
      9:HASH-VECTOR                      110101
     12:POOL                            1000101
     15:POPULATION                      1010101
     18:PACKAGE                         1100101
     19:SLOT-VECTOR                     1110101
     21:BASIC-STREAM                   10000101
     23:FUNCTION                       10010101
     26:ARRAY-HEADER                   10100101

nodeheader-1 (#b0110): (9 values)
      3:RATIO                             10110
      7:COMPLEX                          100110
     10:STRUCT                           110110
     13:ISTRUCT                         1000110
     16:VALUE-CELL                      1010110
     20:LOCK                            1110110
     22:INSTANCE                       10000110
     27:VECTOR-HEADER                  10100110
     29:SIMPLE-VECTOR                  10110110

## arm64 tags

suppose fulltag-misc is #b0100

Then a tagged uvector pointer will be something like

#x100000004

The elements of the uvector start at

#x100000004 - 4 + 8 => address #x100000008, which is past the uvector header.

On arm64, do we want tags for symbols and functions?

typecode returns an object's lisptag or the subtag, if the object is a miscobj.
