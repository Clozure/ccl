# Tagging

CCL allocates Lisp objects in memory on double-node (dnode)
boundaries.  A dnode is two machine words.

On a 32-bit platform, a dnode is 8 bytes (64 bits) long.  That means
that the low three bits of an address are redundant: we only really
need to the upper 29 bits to know the address of a dnode-aligned
object.

On a 64-bit platform, a dnode is 16 bytes (128 bits) long. The low
four bits are thus redundant: the upper 60 bits are sufficient
to address a dnode-aligned object.

We call a reference (pointer) to a Lisp object a "node".

The extra 3 or 4 bits at the bottom of a Lisp node can be used
to encode at least some information about the node's type; at the
same time, the upper 29 or 60 bits represent either some immediate
value (i.e., some value stored directly in the node), or else a
dnode-aligned memory address.

We call the low 3 or 4 bits of a node that node's "tag bits."  The
conventions used to encode type information in those tag bits are
called a "tagging scheme."

A built-in primitive called `typecode` returns an object's subtag
if it a miscobj; otherwise it returns the object's "lisptag".

extract-typecode: if the reference is to a miscobj, return its subtag;
otherwise return its 3-bit tag.
extract-typecode-fixnum: same, but box return value

ivector-typecode-p: is the reference in question an ivector?
gvector-typecode-p: is the reference in question a gvector?

## 64-bit ARM
