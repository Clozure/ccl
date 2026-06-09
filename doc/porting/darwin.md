# Darwin (macOS)

On arm64, macOS prevents us from having a static area at a fixed address.
On x86-64, we used the `pagezero_size` linker option to reserve some low
memory, but this no longer works.

> From https://forums.developer.apple.com/forums/thread/655950
> 
> "Modifying pagezero_size isn't a supportable option in the arm64
> environment. arm64 code must be in an ASLR binary, which using a
> custom pagezero_size is incompatible with. An ASLR binary encodes
> signed pointers using a large random size along with the expected
> page zero size, and this combination is going to extend beyond the
> range of values covered in the lower 32-bits. Further, even if
> that did work, 32-bit pointers are completely incompatible with
> the arm64e architecture, which is available as a preview
> technology.

On an arm64 Mac, building with something like
`cc -Wl,-pagezero_size,0x4000 -g foo.c`, seems to work, but it produces
a binary that won't run: "error: Malformed Mach-o file" is what
the debugger prints out.

On an Intel Mac, that same `cc -Wl,-pagezero_size,0x4000 -g foo.c`
does produce a working binary.

On other ports, `nil` is basically a really popular constant, and it
happens to be a pointer to a fixed address in low-ish memory.  But it
looks like we're going to have to keep `nil` in a register (rnil).  This means
we will access kernel globals and `nil`-relative symbols as offsets from rnil.

## ARM ABI
Official ARM documentation: https://github.com/ARM-software/abi-aa

As permitted by the standard 64-bit ARM ABI, Apple reserves register x18.
(Linux apparently doesn't.)

The architectural stack pointer SP must be 16-byte aligned whenever it is
used to access memory.  This is hardware-enforced.
As a matter of ABI policy, SP must be 16-byte aligned at public
C function boundaries.

For example, here is a way to lose:
```
str x1, [sp, #-8]! ;OK, but sp now has only 8 byte alignment...
str x0, [sp, #-8]! ;... so this subsequent store fails
```

## MAP_JIT and the W^X policy
On arm64, macOS enforces a policy called W^X.  This means that a memory
region can be either writable or executable, but never both at the same
time.

Apple recognizes that this policy affects dynamic languages.  They
document their accomodation for this at
https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon.

In short, you are supposed to call `mmap` with the `MAP_JIT` flags.
Threads can then call `pthread_jit_write_protect_np` to enable and
disable write access.  Note that this operates on a per-thread basis.
Officially, only a single `MAP_JIT` region is supported.

CCL has traditionally managed a single dynamic memory area that contains
code and other data.  I don't see how we can keep 