/*
	This little trick is used on Darwin/x8632 to ensure that
	memory under 16MB is otherwise occupied, and will therefore
	not contain any lisp objects.

	This will overshoot the 16MB mark by the size of the lisp
	kernel, but since that's only a couple hundred kilobytes, it
	hardly seems worth getting excited about.
*/
	.zerofill __PAD, __pad,	lowmem_pad, 0x01000000

