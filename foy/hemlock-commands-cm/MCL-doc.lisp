#|
; Copyright 1987-1988 Coral Software Corp.
; Copyright 1989-1994 Apple Computer, Inc.
; Copyright 1995-2001 Digitool, Inc.

; MCL has been released as an open source application, subject to the GLGPL License.
|#

* 
"&rest numbers"
"[Function / Variable]"
"as a function, multiplies all the numbers, returning the product. As a variable, bound to the last
value returned by the read loop."

**  
NIL "[Variable]"
"bound to the second to last value returned by the read loop."

***  
NIL "[Variable]"
"bound to the third to last value returned by the read loop."

*COMPILE-PRINT* 
NIL "[Variable]"
"holds the default value for the :print keyword argument to compile. If the value of this variable is t,
then compile prints the value of each expression compiled to *standard-output*. The default value is
nil."

*COMPILE-VERBOSE* 
NIL "[Variable]"
"holds the default value for the :verbose keyword argument to compile If true, then compile defaults to
printing the name of each file compiled. If nil, then compile defaults to not printing the names of files
compiled."

*DEBUG-IO*  
NIL "[Variable]"
"the stream used for input and output when operating inside a break loop."

*DEFAULT-PATHNAME-DEFAULTS* 
NIL "[Variable]" 
"the default pathname used to supply missing components when pathnames are merged."

*ERROR-OUTPUT*  
NIL "[Variable]"
"the stream to which error messages are sent."

*FEATURES*  
NIL "[Variable]"
"a list of features present in the current lisp environment. You can add features to this list as you
bring tools into your environment. The features should be represented as keywords. This variable is
used by the #+ and #- reader macros."

*LOAD-PRINT*  
NIL "[Variable]"
"holds the default value for the :print keyword argument to load. If the value of this variable is t, then
load prints the value of each expression loaded to *standard-output*. The default value is nil."

*LOAD-TRUENAME*  
NIL "[Variable]"
"a variable bound to the true name of the file being loaded. Its initial value is nil."

*LOAD-VERBOSE*  
NIL "[Variable]"
"holds the default value for the :verbose keyword argument to load. If true, then load defaults to
printing the name of each file loaded. If nil, then load defaults to not printing the names of files loaded."

*MACROEXPAND-HOOK*  
NIL "[Variable]"
"used by macroexpand-1. Its initial value is funcall."

#|
*MODULES*  
NIL "[Variable]"
"holds a list of the names of the modules that have been loaded into Macintosh Common Lisp. This list is
used by the functions require and provide. This variable has been removed from Common Lisp and is
provided in the CCL: package."
|#

*PACKAGE*  
NIL "[Variable]"
"at any point, this variable is bound to the current package. The functions load and compile-file rebind
*package* to its current value. The forthcoming ANSI Common Lisp will use the package name
common-lisp-user instead of user."

*PRINT-ARRAY*  
NIL "[Variable]"
"controls whether arrays are printed readably. If the value of this variable is nil, the contents of
arrays other than strings are never printed."

*PRINT-BASE*  
NIL "[Variable]"
"specifies the radix to use for printing rational numbers. The default is base 10. Note that this is the
radix for printing; it doesn’t affect the radix for reading numbers."

*PRINT-CASE*  
NIL "[Variable]"
"controls the case used for printing symbols. The value of *print-case* should be :upcase (the
default), :downcase, or :capitalize."

*PRINT-CIRCLE*  
NIL "[Variable]"
"Controls the printing of data objects which may be circular, such as arrays and lists. If
*print-circle* is true, then the printer checks for circular structures (which makes printing
slower). The default is nil."

*PRINT-ESCAPE*  
NIL "[Variable]"
"controls the printing of escape characters. If *print-escape* is true (the default), then escape
characters are printed (which increases the chances that objects will print readably)."

*PRINT-GENSYM*  
NIL "[Variable]"
"controls the printing of uninterned symbols. If true, uninterned symbols are printed with the #:
reader macro; they will not be interned when read into Lisp. If nil, they are not printed with the
reader macro, and they will be interned when read back in."

*PRINT-LENGTH*  
NIL "[Variable]"
"controls how many elements of a list or array are printed. If nil, then the entire data structure is
printed."

*PRINT-LEVEL*  
NIL "[Variable]"
"controls how many levels of a list or array are printed. If nil, then all levels are printed."

*PRINT-LINES*  
NIL "[Variable]"
"When the value of this Common Lisp variable is other than nil, it is a limit on the number of output
lines produced when an object is pretty printed."

*PRINT-PRETTY*  
NIL "[Variable]"
"controls the look of printed expressions. If true, then extra whitespace is inserted to make the
printed representation of forms more readable. For a description of user-controlled pretty-printing
in Common Lisp, see Common Lisp: The Language, 2d edition, Chapter 27."

*PRINT-RADIX*  
NIL "[Variable]"
"controls the printing of radix specifiers for rational numbers. If true, then rational numbers are
printed with radix specifiers. This ensures that the numbers may be read back into Lisp. The default
is nil."

*PRINT-READABLY*  
NIL "[Variable]"
"If this Common Lisp variable true, objects are printed out such that the printed representation can
be read back into Lisp to create a similar object. If false, objects may be printed more tersely."

*PRINT-RIGHT-MARGIN*  
NIL "[Variable]"
"If non-nil, this Common Lisp variable specifies the right margin to use when the pretty printer is
making layout decisions."

*QUERY-IO*  
NIL "[Variable]"
"the stream used for asking questions of, and receiving answer from, the user."

*RANDOM-STATE*  
NIL "[Variable]"
"the default random state object, used by random when it is not explicitly passed a random state."

*READ-BASE*  
NIL "[Variable]"
"specifies the radix used when reading rational numbers. Note that this does not affect the radix used
when printing numbers."

*READ-DEFAULT-FLOAT-FORMAT*  
NIL "[Constant]"
"the default floating-point type in which to read floating-point numbers."

*READ-SUPPRESS*  
NIL "[Variable]"
"if true, most read operations are suppressed, preventing the reading of Lisp data objects. This
variable is most often used by the reader macros #+ and #-."

*READTABLE*  
NIL "[Variable]"
"holds the default readtable used by read operations."

*STANDARD-INPUT*  
NIL "[Variable]"
"the stream from which the top level read-eval-print loop gets its input. This is the default input
stream used by functions such as read and read-char."

*STANDARD-OUTPUT*  
NIL "[Variable]"
"the stream to which the top level read-eval-print loop sends its output. This is also the default
output stream for functions such as print, write, etc."

*TERMINAL-IO*  
NIL "[Variable]"
"the stream which is used for interacting with the user. *terminal-io* is bound to a stream that reads
from the Listener and other Fred windows, and from forms which have been set up with
eval-enqueue. It prints to the *standard-output* or one of the other system output streams (such as
*error-output*)."

*TRACE-OUTPUT*  
NIL "[Variable]"
"the stream to which the output of trace is sent. *trace-output* is initially bound to the same stream
as *terminal-io*, but may be rebound to redirect the output of trace."

+ 
"&rest numbers"
"[Function / Variable]"
"as a function, adds all the arguments and returns the sum. As a variable, bound to the last form read
by the read loop."

++  
NIL "[Variable]"
"bound to the second to last form read by the read loop."

+++  
NIL "[Variable]"
"bound to the third to last form read by the read loop."

- 
"number &rest more-numbers"
"[Function / Variable]"
"as a function subtracts each argument, from left to right, from the result of the previous
subtraction, and returns the final difference. As a variable, bound to the form currently being
executed by the read loop."

/ 
"number &rest more-numbers"
"[Function / Variable]"
"as a function divides each argument, from left to right, into the result of the previous division, and
returns the final quotient. As a variable, bound to a list containing the multiple values last returned
by the read loop."

//  
NIL "[Variable]"
"bound to a list of the second to last set of multiple values returned by the read loop."

///  
NIL "[Variable]"
"bound to a list of the third to last set of multiple values returned by the read loop."

/= 
"number &rest more-numbers"
"[Function]"
"returns true if none of the arguments are numerically equal; otherwise returns nil."

1+ 
"number"
"[Function]"
"returns the result of adding 1 to number."

1- 
"number"
"[Function]"
"returns the result of subtracting 1 from number."

< 
"number &rest more-numbers"
"[Function]"
"returns true if each argument is less than the one following it; otherwise returns nil."

<= 
"number &rest more-numbers"
"[Function]"
"returns true if each argument is less than or equal to the one following it; otherwise returns nil."

= 
"number &rest more-numbers"
"[Function]"
"returns true if all the arguments are numerically equal; otherwise returns nil."

> 
"number &rest more-numbers"
"[Function]"
"returns true if each argument is greater than the one following it; otherwise returns nil."

>= 
"number &rest more-numbers"
"[Function]"
"returns true if each argument is greater than or equal to the one following it; otherwise returns nil."

ABORT 
"&optional condition"
"[Function]"
"transfers control to the restart named abort. If no such restart exists, an error is signaled. If
condition is not nil, only restarts associated with condition are considered."

ABS 
"number"
"[Function]"
"returns the absolute value of number."

ACONS 
"key datum alist"
"[Function]"
"creates a cons with key in the car and datum in the cdr, conses this onto the front of alist, and returns
the resulting list. alist is not destructively modified."

ACOS 
"radians"
"[Function]"
"returns the arc cosine of radians, a number in radians."

ACOSH 
"radians"
"[Function]"
"returns the hyperbolic arc cosine of radians, a number in radians."

ADJOIN 
"item list &key :test :test-not :key"
"[Function]"
"adds item to list if it is not already a member of list, and returns the resulting list. list is not
destructively modified."

ADJUST-ARRAY 
"array new-dimensions &key :element-type :initial-element :initial-contents
:fill-pointer :displaced-to :displaced-index-offset"
"[Function]"
"returns an array of the same type and rank as array, with the specified new-dimensions. This
function may either alter the given array or create and return a new one."

ADJUSTABLE-ARRAY-P 
"array"
"[Function]"
"returns true if array is adjustable, and nil if it is not."

ALPHA-CHAR-P 
"char"
"[Function]"
"returns true if char is an alphabetic character, otherwise false. char must be a character."

ALPHANUMERICP 
"char"
"[Function]"
"returns true if char is an alphabetic or numeric character, otherwise returns nil. char must be a
character."

AND 
"{form}*"
"[Macro]"
"evaluates each form sequentially. If and reaches a form that returns nil, it returns nil without
evaluating any more forms. If it reaches the last form, it returns that form's value."

APPEND 
"&rest lists"
"[Function]"
"concatenates the top-level elements of lists, in effect splicing them together. The lists are not
modified. Returns the resulting concatenated list."

APPLY 
"function first-arg &rest more-args"
"[Function]"
"invokes function, giving it first-arg and more-args as arguments. The value returned by function is
returned. The last argument to apply should be a list; the elements of this list are passed as
individual arguments to function. The type of function can be only symbol or function."

APROPOS 
"string-or-symbol &optional package"
"[Function]"
"finds all interned symbols whose print names contain string-or-symbol as a substring and prints
the name, function definition, and global value of each symbol. The printing is sent to
*standard-output*. If package is specified, only the given package is searched. apropos returns no
values."

APROPOS-LIST 
"string-or-symbol &optional package"
"[Function]"
"returns a list of all available symbols whose print names contain string-or-symbol as a substring.
If package is specified, only the given package is searched."

AREF 
"array &rest subscripts"
"[Function]"
"returns the element of array specified by subscripts. aref can be used with setf to modify an array."

ARRAY-DIMENSION 
"array dimension"
"[Function]"
"returns the length of dimension of array. Vector fill-pointers are ignored (i.e. the total size,
including inactive elements, is returned)."

#|
ARRAY-DIMENSION-LIMIT  
NIL "[Constant]"
"The maximum allowable number of elements in a single dimension of an array. This value must be a
fixnum. Its value in Macintosh Common Lisp is 4194304."
|#

ARRAY-DIMENSIONS 
"array"
"[Function]"
"returns a list whose elements are the dimensions of array."

ARRAY-ELEMENT-TYPE 
"array"
"[Function]"
"returns a type specifier which describes what data types an element of array may have."

ARRAY-HAS-FILL-POINTER-P 
"array"
"[Function]"
"returns t if array is a vector with a fill pointer. Returns nil if array is not a vector or if array does
not have a fill pointer."

ARRAY-IN-BOUNDS-P 
"array &rest subscripts"
"[Function]"
"returns true if subscripts are all legal subscripts for array."

ARRAY-RANK 
"array"
"[Function]"
"returns the rank (number of dimensions) of array."

#|
ARRAY-RANK-LIMIT  
NIL "[Constant]"
"a positive integer that is the upper exclusive bound on the rank (number of dimensions) in an array.
Its value in Macintosh Common Lisp is 8192."
|#

ARRAY-ROW-MAJOR-INDEX 
"array &rest subscripts"
"[Function]"
"given an array and a valid set of subscripts, returns a single number indicating the position of the
accessed element based on row-major ordering. This function ignores fill-pointers."

ARRAY-TOTAL-SIZE 
"array"
"[Function]"
"returns the total size of array. This is the product of the sizes of all the dimensions."

#|
ARRAY-TOTAL-SIZE-LIMIT  
NIL "[Constant]"
"a positive integer that is the upper exclusive bound on the total number of elements in an array. Its
value in Macintosh Common Lisp is 4194304."
|#

ARRAYP 
"object"
"[Function]"
"returns true if data-object is an array, false if it is not."

ASH 
"integer count"
"[Function]"
"Shifts integer arithmetically left or right by count bits, depending on the sign of count. Bits shifted
off the right are lost."

ASIN 
"radians"
"[Function]"
"returns the arc sine of radians, a number in radians."

ASINH 
"radians"
"[Function]"
"returns the hyperbolic arc sine of radians, a number in radians."

ASSERT 
"test-form [({place}*) [string {arg}*]]"
"[Macro]"
"signals a continuable error if the value of test-form is nil. If the user continues, the values of some
variables can be changed, and the assert will start over, evaluating the test-form again. assert
returns nil."

ASSOC 
"indicator a-list &key :test :key :test-not"
"[Function]"
"searches a-list for the first pair whose car matches indicator. Returns the pair, or nil if the search
fails."

ASSOC-IF 
"predicate a-list &key :key"
"[Function]"
"searches a-list for the first pair matching :key whose car satisfies predicate. Returns the pair, or
nil if the search fails."

ASSOC-IF-NOT 
"predicate a-list &key :key"
"[Function]"
"searches a-list for the first pair matching :key whose car does not satisfy predicate. Returns the
pair, or nil if the search fails."

ATAN 
"y &optional x"
"[Function]"
"returns the arc tangent of y, either the y-component of a number in radians or the complete number,
encoded as a point."

ATANH 
"radians"
"[Function]"
"returns the hyperbolic arc tangent of radians, a number in radians."

ATOM 
"data-object"
"[Function]"
"returns true if object is not a cons; otherwise returns false. In general , atom is true of anything that
is not a list. The one exception is the empty list, which is both a list and an atom."

BIT 
"bit-array &rest subscripts"
"[Function]"
"returns the value of the the bit in bit-array specified by the subscripts."

BIT-AND 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"performs a logical AND of the bits in bit-array-1 and bit-array-2, storing the result into
result-bit-array. If result-bit-array is not specified, a new bit-array is created to hold the result.
If result-bit-array is t, the result is stored into bit-array-1. If result-bit-array is a bit array,
the result is destructively placed into that array."

BIT-ANDC1 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"performs a logical AND of the bits in the complement of bit-array-1 and the bits in bit-array-2,
storing the result into result-bit-array. If result-bit-array is not specified, a new bit-array is
created to hold the result. If result-bit-array is t, the result is stored into bit-array-1. If
result-bit-array is a bit array, the result is destructively placed into that array."

BIT-ANDC2 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"performs a logical AND of the bits in bit-array-1 and the bits in the complement of bit-array-2,
storing the result into result-bit-array. If result-bit-array is not specified, a new bit-array is
created to hold the result. If result-bit-array is t, the result is stored into bit-array-1. If
result-bit-array is a bit array, the result is destructively placed into that array."

BIT-EQV 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"compares the bits in bit-array-1 and bit-array-2; if both are 1s or both are 0s, a 1 is stored into
result-bit-array (otherwise a 0 is stored into result-bit-array). If result-bit-array is not
specified, a new bit-array is created to hold the result. If result-bit-array is t, the result is stored
into bit-array-1. If result-bit-array is a bit array, the result is destructively placed into that
array."

BIT-IOR 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"performs a logical inclusive OR of the bits in bit-array-1 and bit-array-2, storing the result into
result-bit-array. If result-bit-array is not specified, a new bit-array is created to hold the result.
If result-bit-array is t, the result is stored into bit-array-1. If result-bit-array is a bit array,
the result is destructively placed into that array."

BIT-NAND 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"performs a logical NOT-AND of the bits in bit-array-1 and bit-array-2, storing the result into
result-bit-array. If result-bit-array is not specified, a new bit-array is created to hold the result.
If result-bit-array is t, the result is stored into bit-array-1. If result-bit-array is a bit array,
the result is destructively placed into that array."

BIT-NOR 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"performs a logical NOT-OR of the bits in bit-array-1 and bit-array-2, storing the result into
result-bit-array. If result-bit-array is not specified, a new bit-array is created to hold the result.
If result-bit-array is t, the result is stored into bit-array-1. If result-bit-array is a bit array,
the result is destructively placed into that array."

BIT-NOT 
"source-bit-array &optional result-bit-array"
"[Function]"
"stores the contents of source-bit-array, with all the bits inverted, into result-bit-array. If
result-bit-array is not specified, a new bit array is created. If result-bit-array is t, the result is
stored in source-bit-array. If result-bit-array is a bit array, the result is destructively placed
into that array."

BIT-ORC1 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"performs a logical OR of the bits in the complement of bit-array-1 and the bits of bit-array-2,
storing the result into result-bit-array. If result-bit-array is not specified, a new bit-array is
created to hold the result. If result-bit-array is t, the result is stored into bit-array-1. If
result-bit-array is a bit array, the result is destructively placed into that array."

BIT-ORC2 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"performs a logical OR of the bits in bit-array-1 and the bits in the complement of bit-array-2,
storing the result into result-bit-array. If result-bit-array is not specified, a new bit-array is
created to hold the result. If result-bit-array is t, the result is stored into bit-array-1. If
result-bit-array is a bit array, the result is destructively placed into that array."

BIT-VECTOR-P 
"thing"
"[Function]"
"returns true if thing is a bit-vector."

BIT-XOR 
"bit-array-1 bit-array-2 &optional result-bit-array"
"[Function]"
"performs a logical exclusive OR of the bits in bit-array-1 and bit-array-2, storing the result into
result-bit-array. If result-bit-array is not specified, a new bit-array is created to hold the result.
If result-bit-array is t, the result is stored into bit-array-1. If result-bit-array is a bit array,
the result is destructively placed into that array."

BLOCK 
"name {form} *"
"[Special Form]"
"establishes a lexical construct named name in which the forms are evaluated sequentially. The block
may be exited at any time by calling return-from with the argument name. Block returns the value
of the last form, or the value passed to return-from."

BOOLE 
"op integer-1 integer-2"
"[Function]"
"applies the logical operation indicated by op to integer-1 and integer-2."

BOOLE-1  
NIL "[Constant]"
"Constant used by the boole function. boole will return 1 for each bit in the first integer argument
which is 1."

BOOLE-2  
NIL "[Constant]"
"Constant used by the boole function. boole will return 1 for each bit in the second integer argument
which is 1."

BOOLE-AND  
NIL "[Constant]"
"Constant used by the boole function. boole will return the logical and of the two integer arguments."

BOOLE-ANDC1  
NIL "[Constant]"
"Constant used by the boole function. boole will return the and complement of the first integer
argument with the second integer argument."

BOOLE-ANDC2  
NIL "[Constant]"
"Constant used by the boole function. boole will return the and complement of the second integer
argument with the first integer argument."

BOOLE-C1  
NIL "[Constant]"
"Constant used by the boole function. boole will return the complement of the first integer argument."

BOOLE-C2  
NIL "[Constant]"
"Constant used by the boole function. boole will return the complement of the second integer argument."

BOOLE-CLR  
NIL "[Constant]"
"Constant used by the boole function. boole will return 0."

BOOLE-EQV  
NIL "[Constant]"
"Constant used by the boole function. boole will return the exclusive nor of the integer arguments."

BOOLE-IOR  
NIL "[Constant]"
"Constant used by the boole function. boole will return the inclusive or of the integer arguments."

BOOLE-NAND  
NIL "[Constant]"
"Constant used by the boole function. boole will return the not-and of the integer arguments."

BOOLE-NOR  
NIL "[Constant]"
"Constant used by the boole function. boole will return the not-or of the integer arguments."

BOOLE-ORC1  
NIL "[Constant]"
"Constant used by the boole function. boole will return the or complement of the first integer
argument with the second integer argument."

BOOLE-ORC2  
NIL "[Constant]"
"Constant used by the boole function. boole will return the or complement of the second integer
argument with the first integer argument."

BOOLE-SET  
NIL "[Constant]"
"Constant used by the boole function. boole will return 1."

BOOLE-XOR  
NIL "[Constant]"
"Constant used by the boole function. boole will return exclusive or of the integer arguments."

BOTH-CASE-P 
"char"
"[Function]"
"returns true if char has both a lowercase and and uppercase version. char must be a character."

BOUNDP 
"symbol"
"[Function]"
"returns true if the dynamic (special) variable named by symbol has a value binding, otherwise
returns nil. boundp does not check for lexical bindings."

BREAK 
"&optional format-string &rest arguments"
"[Function]"
"Prints the message specified by format-string and arguments, sets up a break catch, and enters a
break loop. The program will resume when the expression (continue) is evaluated (at which point
break will return nil)."

BUTLAST 
"list &optional num"
"[Function]"
"copies all of list except the last num elements, and returns the new list. num defaults to 1. If list is
shorter than num, the empty list is returned. list is not modified."

BYTE 
"size position"
"[Function]"
"constructs and returns a byte specifier from size and position."

BYTE-POSITION 
"bytespec"
"[Function]"
"returns the position component of bytespec."

BYTE-SIZE 
"bytespec"
"[Function]"
"returns the size component of bytespec."

CAAAAR 
"list"
"[Function]"
"returns the fourth car of list."

CAAADR 
"list"
"[Function]"
"returns the car of the car of the car of the cdr of list."

CAAAR 
"list"
"[Function]"
"returns the third car of list."

CAADAR 
"list"
"[Function]"
"returns the car of the car of the cdr of the car of list."

CAADDR 
"list"
"[Function]"
"returns the car of the car of the cdr of the cdr of list."

CAADR 
"list"
"[Function]"
"returns the car of the car of the cdr of list."

CAAR 
"list"
"[Function]"
"returns the car of the car of list."

CADAAR 
"list"
"[Function]"
"returns the car of the cdr of the car of the car of list."

CADADR 
"list"
"[Function]"
"returns the car of the cdr of the car of the cdr of list."

CADAR 
"list"
"[Function]"
"returns the car of the cdr of the car of list."

CADDAR 
"list"
"[Function]"
"returns the car of the cdr of the cdr of the car of list."

CADDDR 
"list"
"[Function]"
"returns the car of the cdr of the cdr of the cdr of list."

CADDR 
"list"
"[Function]"
"returns the car of the cdr of the cdr of list."

CADR 
"list"
"[Function]"
"returns the car of the cdr of list. This is the second element of list."

CALL-ARGUMENTS-LIMIT  
NIL "[Constant]"
"a positive integer that is the upper exclusive bound on the number of arguments that may be passed to
a function."

CALL-NEXT-METHOD 
"&rest args"
"[Function]"
"can be used within the body of a method defined by a method-defining form to call the next method.
The type of method combination used determines which methods can invoke this function and what
behavior results."

CAR 
"list"
"[Function]"
"returns the first element of list."

CASE 
"keyform {({({key }*) | key } {form}* ) }*"
"[Macro]"
"evaluates keyform, then evaluates as an implicit progn the forms whose keys match the value of
keyform. Returns the last form evaluated. keyform is evaluated, but the keys are not. case permits a
final case, otherwise or t, that handles all keys not otherwise covered."

CATCH 
"tag {form}*"
"[Special Form]"
"sets up a catch called tag and executes forms sequentially. At any time during the execution of the
forms a throw to tag will immediately cause catch to return the thrown value. If no throw occurs, the
value of the last body-form is returned."

CCASE 
"keyform {({( {key }*) | key } {form}* ) }*"
"[Macro]"
"gets the value of keyform (which must be a place acceptable to setf), and then executes the first set of
forms whose corresponding keys are eql to the keyform value. I A continuable error is signalled if
there is no match, allowing the user to place a new value in the keyform place."

CDAAAR 
"list"
"[Function]"
"returns the cdr of the car of the car of the car of list."

CDAADR 
"list"
"[Function]"
"returns the cdr of the car of the car of the cdr of list."

CDAAR 
"list"
"[Function]"
"returns the cdr of the car of the car of list."

CDADAR 
"list"
"[Function]"
"returns the cdr of the car of the cdr of the car of list."

CDADDR 
"list"
"[Function]"
"returns the cdr of the car of the cdr of the cdr of list."

CDADR 
"list"
"[Function]"
"returns the cdr of the car of the cdr of list."

CDAR 
"list"
"[Function]"
"returns the cdr of the car of list."

CDDAAR 
"list"
"[Function]"
"returns the cdr of the cdr of the car of the car of list."

CDDADR 
"list"
"[Function]"
"returns the cdr of the cdr of the car of the cdr of list."

CDDAR 
"list"
"[Function]"
"returns the cdr of the cdr of the car of list."

CDDDAR 
"list"
"[Function]"
"returns the cdr of the cdr of the cdr of the car of list."

CDDDDR 
"list"
"[Function]"
"returns the fourth cdr of list."

CDDDR 
"list"
"[Function]"
"returns the third cdr of list."

CDDR 
"list"
"[Function]"
"returns the cdr of the cdr of list."

CDR 
"list"
"[Function]"
"returns all of list but the first element."

CEILING 
"number &optional divisor"
"[Function]"
"converts number to an integer by rounding up. That is, it returns the smallest integer which is not
smaller than number. The remainder is returned as a second value. When divisor is specified, ceiling
first divides divisor into number, and then applies ceiling to the result."

CERROR 
"continue-format-string datum &rest args"
"[Function]"
"invokes the signal facility on a condition. If the condition is not handled, (invoke-debugger condition)
is executed. While signaling is going on, it is possible to return from cerrror by invoking continue.
cerror returns nil."

CHANGE-CLASS 
"instance new-class"
"[Generic function]"
"changes the class of an instance to a new class. This function destructively modifies and returns the
instance."

CHAR 
"string index"
"[Function]"
"returns as a character object the character in string in the position indicated by index. char may be
used with setf."

CHAR-CODE 
"character"
"[Function]"
"returns the integer ASCII value of character, a character object."

CHAR-CODE-LIMIT  
NIL "[Constant]"
"a non-negative integer that is the upper exclusive bound on values produced by the char-code
function."

CHAR-DOWNCASE 
"char"
"[Function]"
"returns a character which is the lowercase equivalent of char."

#|
CHAR-EQUAL 
"char &rest more-chars"
"[Function]"
"returns true if all the characters are equal, otherwise false. In Macintosh Common Lisp, case, font,
and bits attributes are ignored; that is, Control-A is char-equal to a."

CHAR-GREATERP 
"char &rest more-chars"
"[Function]"
"returns true if each character is greater than the one to its right, otherwise false.In Macintosh
Common Lisp, case, font, and bits attributes are ignored."

CHAR-INT 
"char"
"[Function]"
"returns an integer encoding char, including bit and font information. In Macintosh Common Lisp, this
function is equal to char-code."

CHAR-LESSP 
"char &rest more-chars"
"[Function]"
"returns true if each character is less than the one to its right, otherwise false. In Macintosh Common
Lisp, case, font, and bits attributes are ignored."

CHAR-NAME 
"character"
"[Function]"
"returns the standard name of character as a string, or nil if character has no standard name."

CHAR-NOT-EQUAL 
"char &rest more-chars"
"[Function]"
"returns true if none of the characters are equal, otherwise false. In Macintosh Common Lisp, case,
font, and bits attributes are ignored; that is, (char-not-equal #\d #\Control-D) is false."

CHAR-NOT-GREATERP 
"char &rest more-chars"
"[Function]"
"returns true if no character is greater than the one to its right, otherwise false. In Macintosh
Common Lisp, case, font, and bits attributes are ignored."

CHAR-NOT-LESSP 
"char &rest more-chars"
"[Function]"
"returns true if no character is less than the one to its right, otherwise false. In Macintosh Common
Lisp, case, font, and bits attributes are ignored."
|#

CHAR-UPCASE 
"char"
"[Function]"
"returns a character which is the uppercase equivalent of char."

CHAR/= 
"char &rest more-chars"
"[Function]"
"returns true if none of the characters are equal, otherwise false. The comparison is case sensitive."

CHAR< 
"character &rest characters"
"[Function]"
"returns true if the ASCII value of character is less than the ASCII value of any of the other
characters, otherwise false. Because ASCII values are compared, case is significant."

CHAR<= 
"char &rest more-chars"
"[Function]"
"returns true if each character is less than or equal to the character to its right, otherwise false. The
comparison is case sensitive."

CHAR= 
"character &rest characters"
"[Function]"
"returns true if all the characters are equal. Case is significant, so that characters of different cases
will never be considered equal."

CHAR> 
"character &rest characters"
"[Function]"
"returns true if the ASCII value of character is greater than the ASCII value of any of the other
characters, otherwise false. Because ASCII values are compared, case is significant."

CHAR>= 
"char &rest more-chars"
"[Function]"
"returns true if each character is greater than or equal to the one to its right, otherwise false. The
comparison is case sensitive."

CHARACTER 
"thing"
"[Function]"
"coerces thing to a character if possible. thing must be a character, positive integer less than
char-code-limit, a string of length one, or a symbol whose print-name is a string of length one."

CHARACTERP 
"object"
"[Function]"
"returns true if object is a character; otherwise returns false."

CHECK-TYPE 
"place typespec &optional string"
"[Macro]"
"signals an error if the value place is not of the type typespec. string, if present, provides a
description of typespec that can appear in an error message."

CLEAR-INPUT 
"&optional input-stream"
"[Function]"
"clears any buffered input associated with input-stream. Returns nil."

CLEAR-OUTPUT 
"&optional output-stream"
"[Function]"
"flushes any pending output to output-stream and returns nil. The output is simply cleared, not sent."

CLOSE 
"stream &key :abort"
"[Function]"
"closes stream so that it can no longer be used for input or output. Returns nil."

CLRHASH 
"hash-table"
"[Function]"
"removes all the entries from the hash table and returns the hash table."

CODE-CHAR 
"code"
"[Function]"
"creates and returns a character object corresponding to the ASCII value code, a non-negative integer.
Signals an error if code is outside the range of supported ASCII values."

COERCE 
"thing type-specifier"
"[Function]"
"converts thing to an \"equivalent\" object of type type-specifier. Coercions can take place between
numeric types as long as these would not involve loss of accuracy, between sequence types, from some
strings, symbols, and numbers to characters, and from a symbol or lambda-function to a function.
Returns the new object. If the coercion cannot take place, an error is signaled."

COMPILE 
"name &optional definition"
"[Function]"
"compiles the function name, using the definition supplied by definition . If definition is supplied, it
should be a lambda-expression or a function to be compiled; if not, the symbol-function of the
symbol is extracted and compiled. The resulting compiled code becomes the function definition of
name, and name is returned. name may be a symbol or a list whose car is setf."

COMPILE-FILE 
"filename &key :output-file :verbose :print :load :features :save-local-symbols
:save-doc-strings :save-definitions"
"[Function]"
"produces a compiled version of the file filename. Compiled files (also called fasl files) can be loaded
much more quickly than source code files. The default values of :verbose and :print are the values of
*compile-verbose* and *compile-print*. The default value of :save-local-symbols is the value of
*fasl-save-local-symbols*; of :save-doc-strings, the value of *fasl-save-doc-strings*;; and of
save-definitions, the value of *fasl-save-definitions*. The default value of :output-file is the input
file with the file type *.fasl-pathname*."

COMPILED-FUNCTION-P 
"thing"
"[Function]"
"returns true if thing is any compiled code object, otherwise false."

COMPLEX 
"realpart &optional imagpart"
"[Function]"
"creates a complex number from realpart and imagpart."

COMPLEXP 
"object"
"[Function]"
"returns true if object is a complex number; otherwise returns false."

CONCATENATE 
"result-type &rest sequences"
"[Function]"
"copies sequences in order into a new sequence of type result-type."

COND 
"{(test {form}* )}*"
"[Macro]"
"consists of a series of clauses which are tested sequentially. If a test is true, cond evaluates the
corresponding forms and returns the last form's value. If the test returns nil, cond proceeds to the
next test / form clause. If all tests fail, cond returns nil."

CONJUGATE 
"number"
"[Function]"
"returns the complex conjugate of number."

CONS 
"x list-or-thing"
"[Function]"
"allocates a new cons cell whose car is x and whose cdr is list-or-thing."

CONSP 
"object"
"[Function]"
"returns true if object is a cons, otherwise returns false. consp of the empty list returns false. (See
also listp which returns true on the empty list.)"

CONSTANTP 
"object"
"[Function]"
"returns true if object is a constant. Constants include self-evaluating objects such as numbers,
characters, bit-vectors, strings, and keywords, and all constant symbols defined with defconstant,
such as nil and t. In addition, a list whose car is quote, such as (quote foo), is considered to be a
constant."

CONTINUE 
"&optional condition"
"[Function]"
"resumes execution of the code suspended by the most recent call to break or cerror. If there have
been no calls to break or cerror, continue simply returns to the top level. If condition is present, the
restart for condition is invoked."

COPY-ALIST 
"a-list"
"[Function]"
"returns a copy of a-list. The top level of list structure is copied; in addition, any element of a-list
that is a cons is also copied. Used for copying association lists."

COPY-LIST 
"list"
"[Function]"
"returns a list that is equal to but not eq to list."

COPY-READTABLE 
"&optional from-readtable to-readtable"
"[Function]"
"copies from-readtable to to-readtable. from-readtable defaults to the current readtable; if it is
supplied as nil, the standard. Common Lisp readtable is used. If to-readtable is supplied, it is
destructively modified to hold the result; otherwise a new readtable is created."

COPY-SEQ 
"sequence"
"[Function]"
"returns a copy of sequence. The result is equalp but not eq to the argument."

COPY-SYMBOL 
"symbol &optional copy-props"
"[Function]"
"returns a uninterned symbol whose print-name is string-=, but not eq, to that of symbol. If
copy-props is non-nil, then the contents of the value, function, and property cells are copied to the
new symbol (the property list is actually duplicated). If copy-props is nil (the default), they are
not copied."

COPY-TREE 
"tree"
"[Function]"
"if tree is not a cons, it is returned directly. Otherwise, a copy of tree is returned, with all conses,
stopping only when non-conses are encountered. Used for copying trees of conses. This function does
not preserve circularities and sharing of substructures."

COS 
"radians"
"[Function]"
"returns the cosine of radians, a number in radians."

COSH 
"radians"
"[Function]"
"returns the hyperbolic cosine of radians, a number in radians."

COUNT 
"item sequence &key :start :end :from-end :key :test :test-not"
"[Function]"
"returns the number of elements of sequence that match item using the given test function; returns nil
if no element matches."

COUNT-IF 
"test sequence &key :from-end :start :end :key"
"[Function]"
"returns the number of elements in the given range of sequence that satisfy test."

COUNT-IF-NOT 
"test sequence &key :from-end :start :end :key"
"[Function]"
"returns the number of elements in the given range of sequence which do not satisfy test."

CTYPECASE 
"keyform {(type {form}* )}*"
"[Macro]"
"evaluates keyform, then evaluates as an implicit progn the forms whose type matches the value of
keyform. Returns the last form evaluated. keyform is evaluated, but the type is not. ctypecase does
not permit an otherwise or t clause. If no clause is satisfied, ctypecase signals a continuable
type-error."

DECF 
"place &optional delta"
"[Function]"
"decrements the value in place by delta (which defaults to 1)."

DECLAIM 
"{declaration-spec}*"
"[Special Form]"
"provides a declaration that is executable and may be used anywhere proclaim may be called, but each
declaration-spec is not evaluated."

DECLARE 
"{declaration-spec}*"
"[Special Form]"
"provides a declaration within executable code. The declare form is not executed; it is limited to
appearing within the bodies of lambda-expressions, certain generic functions, and some special
forms, and must always appear at the beginning of the body."

DECODE-FLOAT 
"float"
"[Function]"
"returns three values describing the value of float: a new floating-point number representing the
mantissa, an integer representing the exponent, and a floating-point number indicating the sign
(+1.0 or -1.0)."

DECODE-UNIVERSAL-TIME 
"universal-time &optional time-zone"
"[Function]"
"returns nine values giving the decoded time equivalents of time-code. The values are: second, minute,
hour, date, month, year, day of week, daylight-savings-time-p, and time-zone."

DEFCONSTANT 
"name value &optional documentation"
"[Macro]"
"proclaims name to be constant with the value that is the result of evaluatingvalue. Returns name.
Documentation may be provided as a string."

DEFINE-MODIFY-MACRO 
"name lambda-list function [doc-string]"
"[Macro]"
"defines a read-modify-write macro named name. These macros set a location to a new value based on
the old value. (incf and decf are examples of read-modify-write macros.)"

DEFINE-SETF-METHOD 
"access-function lambda-list [[{declaration}* | doc-string]] {form}*"
"[Macro]"
"defines how to setf a generalized variable that is accessed by access-function, which should be a
function or macro."

DEFMACRO 
"symbol lambda-list {declaration | doc-string}* {form}*"
"[Macro]"
"constructs a global macro definition, binds it to symbol, marks symbol as a macro, and returns
symbol. defmacro is the macro equivalent of defun."

DEFMETHOD 
"function-name {method-qualifier}* specialized-lambda-list [[{declaration}* |
doc-string]] {form}*"
"[Macro]"
"defines a method on a generic function."

DEFPACKAGE 
"defined-package-name {option}*"
"[Macro]"
"Creates a new package, or modifies an existing one, whose name is defined-package-name (a string or
symbol) and returns defined-package-name. The standard options are :size, :nicknames, :shadow,
:shadowing-import-from, :use, :import-from, :intern, and :export."

DEFPARAMETER 
"variable-name initial-value &optional documentation"
"[Macro]"
"proclaims variable-name to be a special variable, sets it to the value of evaluating initial-value, a
form, and returns variable-name. Documentation may be provided as a string."

DEFSETF 
"access-function {update-function [doc-string] | lambda-list (store-variable)
[[{declaration}* | doc-string]] {form}*}"
"[Macro]"
"defines how to setf a generalized variable that is accessed by access-function, which should be a
function or macro."

DEFSTRUCT 
"name-and-options [doc-string] {slot-description}*"
"[Macro]"
"defines a new structure, according to name-and-options, with slots described by the
slot-descriptions."

DEFTYPE 
"symbol lambda-list [[{declaration }* | doc-string]] {form}*"
"[Macro]"
"defines the type symbol, to be expanded according to the lambda-list and forms, and returns symbol."

DEFUN 
"symbol lambda-list {declaration | doc-string}* {form}*"
"[Macro]"
"defines a function with the name symbol. Once a function is defined, it may be used just like the
functions which are built into the system. defun returns symbol."

DEFVAR 
"variable-name &optional initial-value documentation"
"[Macro]"
"proclaims variable-name to be a special variable, optionally sets it to the value of initial-value, and
returns variable-name. If initial-value is given, variable-name is initialized to the result of
evaluating it unless variable-name already has a value. If initial-name is not used, it is not
evaluated. The macro defvar only has an effect the first time it is called on a symbol. Documentation
may be provided as a string."

DELETE 
"item sequence &key :count :start :end :from-end :test :test-not :key"
"[Function]"
"returns a sequence equivalent to sequence with occurrences of item removed. The original sequence
may be modified. (This is the destructive counterpart of remove.)"

DELETE-DUPLICATES 
"sequence &key :start :end :from-end :test :test-not :key"
"[Function]"
"returns a sequence equivalent to sequence except that all duplicate elements have been removed. The
original sequence may be modified. The non-destructive version of this function is
remove-duplicates."

#|
DELETE-FILE 
"file-or-dir &key :if-does-not-exist"
"[Function]"
"deletes file-or-dir. Returns the pathname of the file if it was successfully deleted, or nil if the file
does not exist and the 0value of :if-does-not-exist is nil. If the file does not exist and the value of
:if-does-not-exist is t, Macintosh Common Lisp signals an error."
|#

DELETE-IF 
"test sequence &key :from-end :start :end :count :key"
"[Function]"
"returns a sequence equivalent to sequence except that elements that pass test are removed. The
original sequence may be destroyed by the operation."

DELETE-IF-NOT 
"test sequence &key :from-end :start :end :count :key"
"[Function]"
"returns a sequence equivalent to sequence except that elements that fail to pass test are removed. The
original sequence may be destroyed by the operation."

DENOMINATOR 
"rational"
"[Function]"
"returns the denominator of the canonical reduced form of rational."

DEPOSIT-FIELD 
"source-integer bytespec destination-integer"
"[Function]"
"returns a number that is the same as destination-integer, except that the byte specified by bytespec
has been replaced by the corresponding byte from source-integer."

DESCRIBE 
"thing &optional stream"
"[Function]"
"prints information about thing to stream, which defaults to the value of *standard-output*. stream
may also be nil (meaning *standard-output*) or t (meaning *terminal-io*)."

DIGIT-CHAR 
"weight &optional radix"
"[Function]"
"if possible, returns a character corresponding to weight in the given radix. If it is not possible to
return such a character, returns nil. radix defaults to 10."

DIGIT-CHAR-P 
"char &optional radix"
"[Function]"
"if char is a legal digit in the base given by radix (default 10), then returns the numeric value of the
digit. If it is not a legal digit, returns nil. char must be a character."

DIRECTORY 
"pathname &key :directories :files :directory-pathnames :test :resolve-aliases"
"[Function]"
"returns a list of the truenames of all files or folders that match pathname, using * as a wildcard
character. directory must accept logical pathnames but does not return them."

DIRECTORY-NAMESTRING 
"pathname"
"[Function]"
"returns the directory component of pathname, as a string."

DISASSEMBLE 
"name-or-compiled-function"
"[Function]"
"prints out a disassembly of name-or-compiled-function, which should be a compiled function object,
a symbol, a lambda expression, or a list whose car is setf."

DO 
"({var | (var [init [step]])}*) (end-test {result}*) {declaration}* {tag | statement}*"
"[Macro]"
"at the beginning of each iteration, evaluates all the init forms (before any var is bound), then binds
each var to the value of its init . Then evaluates end-test; if the result is nil, execution proceeds with
the body of the form. If the result is non-nil, the result forms are evaluated as an implicit progn and
the value of the last result form is returned. At the beginning of the second and subsequent iterations,
all step forms are evaluated, then all variables are updated."

DO* 
"({(var [init-val [update]])}*) (end-test {result}*) {decl}* {tag | body-form}*"
"[Macro]"
"sequentially evaluates each init form and binds each var to the value of its init , then evaluates
end-test. If the result is nil, execution proceeds with the body of the form. If the result is non-nil,
the result forms are evaluated as an implicit progn and the value of the last result form is returned.
At the beginning of the second and subsequent iterations, the first step form is evaluated and its value
assigned to the first var, then the second step form is evaluated and its value assigned, and so on."

DO-ALL-SYMBOLS 
"(var [result-form]) {declaration}* {tag | form}*"
"[Macro]"
"iterates over all the symbols accessible in any package, binding var to each of them in turn, and
evaluating the forms. Some symbols may be processed more than once. When done, evaluates
result-form and returns its value."

DO-EXTERNAL-SYMBOLS 
"(var [package [result-form]]) {declaration}* {tag | form}*"
"[Macro]"
"iterates over all the external symbols of package, binding var to each of them in turn, and evaluating
the forms. When done, evaluates result-form and returns its value. package may be either a package
object or a package name."

DO-SYMBOLS 
"(var [package [result-form]]) {declaration}* {tag | form}*"
"[Macro]"
"iterates over all the symbols accessible in package, binding var to each of them in turn, and
evaluating the forms. When done, evaluates result-form and returns its value."

DOCUMENTATION 
"symbol &optional doc-type"
"[Generic Function]"
"returns the documentation string of doc-type for symbol. Documentation strings may be specified
when functions, variables, macros, etc. are defined. The documentation strings are only retained if
*save-doc-strings* is true when the definition occurs. doc-type may be function, variable,
structure, type, or setf."

DOLIST 
"(var listform [resultform]) {declaration}* {tag | statement}*"
"[Macro]"
"evaluates listform, which produces a list, and executes the body once for every element in the list. On
each iteration, var is bound to successive elements of the list. Upon completion, resultform is
evaluated, and the value is returned. If resultform is omitted, the result is nil."

DOTIMES 
"(var countform [resultform]) {declaration}* {tag | statement}*"
"[Macro]"
"executes forms countform times. On successive executions, var is bound to the integers between zero
and countform. Upon completion, resultform is evaluated, and the value is returned. If resultform is
omitted, the result is nil."

DOUBLE-FLOAT-EPSILON  
NIL "[Constant]"
"The smallest positive floating point number e such that (not (= (float 1 e) (+ (float 1 e) e)))."

DOUBLE-FLOAT-NEGATIVE-EPSILON  
NIL "[Constant]"
"The smallest negative floating point number e such that (not (= (float 1 e) (- (float 1 e) e)))."

DPB 
"source-integer. bytespec destination-integer"
"[Function]"
"returns a number that is the same as destination-integer., except that the byte specified by bytespec
is replaced with the appropriate number of low bits from source-integer."

DRIBBLE 
"&optional pathname"
"[Function]"
"sends input/output from an interactive session in the Listener to the file pathname, creating a
readable record of the session."

ECASE 
"keyform {({( {key }*) | key } {form}* ) }*"
"[Macro]"
"evaluates keyform, and then executes the first set of forms whose corresponding keys ares eql to the
keyform. An error is signalled if there is no match."

ED 
"&optional pathname"
"[Function]"
"opens an editor window to the file specified by pathname, or a new editor window if pathname is not
specified. pathname may be a logical pathname."

EIGHTH 
"list"
"[Function]"
"returns the eighth element of list, using one-based addressing."

ELT 
"sequence index"
"[Function]"
"returns the element of sequence specified by index, a non-negative integer less than the length of
sequence. Zero-based indexing is used."

ENCODE-UNIVERSAL-TIME 
"second minute hour date month year &optional time-zone"
"[Function]"
"returns the time indicated by the arguments in encoded format."

ENDP 
"list"
"[Function]"
"returns t if list is nil; returns nil if list is a cons; errors if list is not a list."

ENOUGH-NAMESTRING 
"pathname &optional defaults"
"[Function]"
"returns a string form of pathname containing just enough information to distinguish it uniquely from
defaults (which defaults to the value of *default-pathname-defaults*)."

EQ 
"object1 object2"
"[Function]"
"returns true if and only if object1 and object2 are the same object. eq is the fastest and strictest test
for equality. (eq works by testing whether object1 and object2 address the same location in
memory.) Things that print the same are not necessarily eq, numbers with the same value need not
be eq, and two similar lists are usually not eq."

EQL 
"object1 object2"
"[Function]"
"returns true if object1 and object2 are eq, or if they are numbers of the same type with the same
value, or if they are character objects that represent the same character."

EQUAL 
"object1 object2"
"[Function]"
"returns true when object1 and object2 are structurally similar. A rough rule of thumb is that
objects are equal when their printed representation is the same. equal is case sensitive when
comparing strings and characters."

EQUALP 
"object1 object2"
"[Function]"
"returns true if object1 and object2 are equal; if they are characters and satisfy char-equal; if they
are numbers with the same numeric value (even if they are of different types); or if they have
components that are all equalp. Special rules apply to arrays, hash tables, and structures; see the
full Common Lisp specification."

ERROR 
"datum &rest args"
"[Function]"
"invokes the signal facility on a condition. If the condition is not handled, (invoke-debugger condition)
is executed."

ETYPECASE 
"keyform {(type {form}* )}*"
"[Macro]"
"evaluates keyform, then evaluates as an implicit progn the forms whose type matches the value of
keyform. Returns the last form evaluated. keyform is evaluated, but the type is not. etypecase does
not permit an otherwise or t clause. If no clause is satisfied, etypecase signals a non-continuable
type-error."

EVAL 
"form"
"[Function]"
"evaluates form and returns the value returned by form. The evaluation is performed in the current
dynamic environment and a null lexical environment."

EVAL-WHEN 
"({situation}*) {form}*"
"[Special Form]"
"specifies when form is to be executed; if it is executed, processes the body of its form as an implicit
progn. situation must be one of :compile-toplevel, :load-toplevel, or :execute."

EVENP 
"integer"
"[Function]"
"returns true if integer is even (evenly divisible by two); otherwise returns nil."

EVERY 
"predicate sequence &rest more-sequences"
"[Function]"
"predicate is applied to the elements of sequence with index 0, then to those with index 1, and so on,
until the end of the shortest sequence is reached. As soon as predicate returns a nil value, nil is
returned; otherwise a non-nil value is returned. That is, every is true if every invocation of
predicate on sequence returns true."

EXP 
"number"
"[Function]"
"returns e raised to the power number, where e is the base of the natural logarithms."

EXPORT 
"symbols &optional package"
"[Function]"
"makes symbols (which should be a symbol or list of symbols) accessible as external symbols in
package (which defaults to *package*), and returns t."

EXPT 
"base-number power-number"
"[Function]"
"returns base-number raised to power-number."

FBOUNDP 
"symbol"
"[Function]"
"returns true if symbol has a global function binding, otherwise returns nil."

FCEILING 
"number &optional divisor"
"[Function]"
"rounds number upward to a floating-point number. The remainder is returned as a second value.
When divisor is specified, fceiling first divides divisor into number, then rounds the result upward."

FFLOOR 
"number &optional divisor"
"[Function]"
"rounds number downward to a floating-point number. The remainder of the operation is returned as a
second value. When divisor is specified, ffloor first divides divisor into number, then rounds the
result downward."

FIFTH 
"list"
"[Function]"
"returns the fifth element of list, using one-based addressing."

#|
FILE-AUTHOR 
"file"
"[Function]"
"attempts to determine and return the author of file. In Macintosh Common Lisp, this returns the
empty string for all files."
|#

FILE-LENGTH 
"file-stream &optional new-length"
"[Function]"
"returns the length of the file associated with file-stream. If new-length is supplied, the function sets
the file size and returns new-pos. If new-length is set to less than the current file position the file is
truncated and the position is set to the new length."

FILE-NAMESTRING 
"pathname"
"[Function]"
"returns the filename portion of pathname, in string format."

FILE-POSITION 
"file-stream &optional new-position"
"[Function]"
"returns or sets the current position within a random access file. If new-position is given, the
position is set and the new position is returned. If new-position is not given, the current position is
returned. Stream input or output operations will occur at this position in the file."

FILE-STRING-LENGTH 
"file-stream object"
"[Function]"
"returns a non-negative integer that is the difference between what the file-position of file-stream
will be after and before writing object to file-stream. If this difference cannot be determined,
returns nil."

FILE-WRITE-DATE 
"file"
"[Function]"
"returns the time when file was last modified as an integer in Universal Time format. If this cannot be
determined, returns nil."

FILL 
"sequence item &key :start :end"
"[Function]"
"destructively replaces elements of sequence with item. Returns the destructively modified sequence."

FILL-POINTER 
"vector"
"[Function]"
"returns the fill pointer of vector. If vector does not have a fill pointer, an error is returned."

FIND 
"item sequence &key :from-end :test :test-not :start :end :key"
"[Function]"
"returns the first element in the specified portion of sequence that matches item according to test, or
nil if no element matches."

FIND-ALL-SYMBOLS 
"string-or-symbol"
"[Function]"
"returns a list of all symbols in any package whose print-name is the same as string-or-symbol. The
search is case-sensitive. If the argument is a symbol, its print name supplies the string to be
searched for."

FIND-CLASS 
"symbol &optional errorp environment"
"[Function]"
"returns the class object named by the given signal in the given environment. If there is no such class
and the errorp argument is nil, find-class returns nil. The default value of errorp is t, which means
that if there is no such class, find-class returns an error."

FIND-IF 
"test sequence &key :key :start :end :from-end"
"[Function]"
"returns the first element in the specified portion of sequence that satisfies test."

FIND-IF-NOT 
"test sequence &key :from-end :start :end :key"
"[Function]"
"returns the first element in the specified portion of sequence that fails to satisfy test."

FIND-PACKAGE 
"package"
"[Function]"
"returns the package with package as its name or nickname, or nil if no such package exists. If package
is a symbol, its print-name is used. If package is a package object, the package is returned."

FIND-SYMBOL 
"string &optional package"
"[Function]"
"searches for the symbol named by string in package (a package object or a package name). Returns
the symbol if it is found, otherwise returns nil."

FINISH-OUTPUT 
"&optional output-stream"
"[Function]"
"attempts to ensure that any output to output-stream that has been buffered reaches its destination.
When the output is complete, returns nil."

FIRST 
"list"
"[Function]"
"returns the car of list, using one-based addressing."

FLET 
"({(name lambda-list {declaration | doc-string}* {form}*)}*) {flet-body-form}*"
"[Special Form]"
"creates local function definitions which can be accessed by the flet-body-forms. Within the body of
the flet, if there are global functions with the same names as the local function definitions, the local
definitions are used instead of the global. The local definition can refer to the global definition."

FLOAT 
"number &optional other"
"[Function]"
"converts number, any non-complex number, to a floating-point number. If other is given, it should
be a floating-point number. number is converted to a float of the same type."

FLOAT-SIGN 
"float1 &optional float2"
"[Function]"
"returns a floating-point number with the same sign as float1 and the same absolute value as float2
(which defaults to 1.0)."

FLOATP 
"object"
"[Function]"
"returns true if object is a floating point number; otherwise returns false."

FLOOR 
"number &optional divisor"
"[Function]"
"converts number to an integer by rounding down. That is, it returns the largest integer which is not
larger than number. A second value returned is the remainder of the operation. When divisor is
specified, floor first divides divisor into number, and then applies floor to the result."

FMAKUNBOUND 
"symbol"
"[Function]"
"causes the global function definition of symbol to become unbound (have no value). symbol may be a
symbol or a list whose car is setf. fmakunbound returns symbol."

FORCE-OUTPUT 
"&optional output-stream"
"[Function]"
"tells output-stream to immediately process all pending buffered output, and returns nil (without
waiting for completion or acknowledgment)."

FORMAT 
"destination control-string &rest args"
"[Function]"
"generates output from control-string and args, and sends it to destination, which should be a stream,
t, or nil. If destination is nil, format returns a stream of type string-stream holding the output.
Otherwise it sends the output to destination (*standard-output* if destination is t) and returns nil."

FOURTH 
"list"
"[Function]"
"returns the fourth element (cadddr) of list, using one-based addressing."

FRESH-LINE 
"&optional output-stream"
"[Function]"
"writes a newline character to output-stream if and only if output-stream is not already at the start
of a new line. Returns t if it wrote a newline or nil if it did not."

FROUND 
"number &optional divisor"
"[Function]"
"returns as a floating-point number the integer nearest to number. If number is halfway between two
integers (for example 3.5), fround rounds up to the next integer and expresses it as a floating-point
number. fround returns a second value, which is the remainder of the rounding operation. When
divisor is present, fround first divides divisor into number, then rounds up the result."

FTRUNCATE 
"number &optional divisor"
"[Function]"
"returns two values: the integer part of number (i.e. number with the fractional part removed),
expressed as a floating-point number, and the fractional part. When divisor is present, truncate
divides divisor into number first and then truncates the result."

FUNCALL 
"function &rest arguments"
"[Function]"
"invokes function, passing it arguments as arguments. Because funcall is a function, function is
evaluated. The type of function can be only symbol or function. The value returned by the function
call is returned."

FUNCTION 
"function-indicator"
"[Special Form]"
"returns the function object associated with function-indicator. This function object is the piece of
code that would be executed if function-indicator was in the car of a list. function is usually
abbreviated by the reader macro #'."

FUNCTIONP 
"object"
"[Function]"
"returns true if object could be a function, otherwise returns false. However, functionp is always
false of symbols and lists, including lambda-lists."

GCD 
"&rest integers"
"[Function]"
"returns the greatest common denominator of its arguments."

GENSYM 
"&optional string-or-number"
"[Function]"
"creates and returns a unique uninterned symbol. If string-or-number is given, it will be used in the
name of the new symbol."

GENTEMP 
"&optional prefix package"
"[Function]"
"creates and returns a new symbol interned in package (which defaults to *package*), guaranteeing
that the symbol will be a new one not already in package."

GET 
"symbol property &optional default"
"[Function]"
"searches the property list of symbol for property (using eq to test). Returns the property value if
found; otherwise returns default-value if specified, or nil if no default-value is specified. get may be
combined with setf to add or change a property."

GET-DECODED-TIME  
NIL
"[Function]"
"returns 9 values giving the current time in Decoded Time format. The 9 values are seconds, minutes,
hours, date, month, year, day-of-week, daylight-saving-time-p, and time-zone."

GET-DISPATCH-MACRO-CHARACTER 
"disp-char sub-char &optional readtable"
"[Function]"
"returns the function associated with sub-char under disp-char in readtable (which defaults to
*readtable*)."

#|
GET-INTERNAL-REAL-TIME  
NIL
"[Function]"
"returns an integer representing, in Internal Time format, the amount of time since your Macintosh
computer has been turned on."

GET-INTERNAL-RUN-TIME  
NIL
"[Function]"
"returns an integer representing, in Internal Time format, the amount of time since your Macintosh
computer has been turned on during which Macintosh Common Lisp computation took place."
|#

GET-MACRO-CHARACTER 
"char &optional readtable"
"[Function]"
"returns two values, the function associated with char in readtable (which defaults to *readtable*),
and a second value that is the non-terminating-p flag."

GET-OUTPUT-STREAM-STRING 
"string-output-stream"
"[Function]"
"returns a string containing all characters written to string-output-stream and resets the stream."

GET-PROPERTIES 
"plist indicator-list"
"[Function]"
"searches plist for any one of the properties from indicator-list. Returns three values: the first found
property, its value, and the portion of plist that has yet to be searched. If no property is found, all
three values are nil."

GET-SETF-METHOD 
"form &optional environment"
"[Function]"
"returns five values constituting the setf method for form in environment."

GET-SETF-METHOD-MULTIPLE-VALUE 
"form &optional environment"
"[Function]"
"returns five values constituting the setf method for form in environment. Used (instead of
get-setf-method) when multiple values may be stored into a generalized variable."

GET-UNIVERSAL-TIME  
NIL
"[Function]"
"returns the current time as a single integer in Universal Time format. This integer can be decoded
with decode-universal-time."

GETF 
"place indicator &optional default"
"[Function]"
"searches the property list stored in place for an indicator eq to indicator. Returns the corresponding
value if indicator matches; otherwise returns default, if specified, or nil. place may be computed
from a generalized variable acceptable to setf."

GETHASH 
"key hash-table &optional default"
"[Function]"
"returns the value of key in hash-table, or default if key is not entered in hash-table. default is nil if
not specified. This function can be used with setf to enter a value into a hash table."

GO 
"tag"
"[Special Form]"
"transfers control to the position in a tagbody referred to by tag."

GRAPHIC-CHAR-P 
"char"
"[Function]"
"returns true if char is a printing character (as opposed to formatting or control character),
otherwise false. char must be a character."

HANDLER-BIND 
"({(typespec handler)}*) {form}*"
"[Macro]"
"executes body in a dynamic context where the given handler bindings are in effect. Each typespec may
be any type specifier. Each handler should evaluate to a function to be used to handle conditions of the
given type(s) during execution of form. The function should take a single argument, the condition
being signaled."

HASH-TABLE-COUNT 
"hash-table"
"[Function]"
"returns the number of entries in hash-table."

HASH-TABLE-P 
"thing"
"[Function]"
"returns true if thing is a hash table, otherwise false."

HOST-NAMESTRING 
"pathname"
"[Function]"
"returns the host portion of pathname, in the form of a string."

IDENTITY 
"thing"
"[Function]"
"returns thing, unaltered."

IF 
"testform thenform [elseform]"
"[Special Form]"
"evaluates testform. If the result is true, evaluates thenform and returns the result; if the result is
nil, evaluates elseform and returns the result."

IMAGPART 
"number"
"[Function]"
"returns the imaginary part of number."

IMPORT 
"symbols &optional package"
"[Function]"
"imports symbols (which should be a symbol or list of symbols) into package (which defaults to
*package*), so that they can referenced without the qualifying colon syntax."

IN-PACKAGE 
"package-name"
"[Macro]"
"sets *package* to the package whose name is package-name. An error is signaled if the package does
not exist. package-name must be a symbol or string; if it is a symbol, the symbol's print-name is
used."

INCF 
"place &optional delta"
"[Function]"
"increases the value in place (which can be any setf-able location) by delta (which defaults to 1)."

INITIALIZE-INSTANCE 
"instance &rest initargs"
"[Generic Function]"
"called by make-instance to initialize a newly created instance."

INPUT-STREAM-P 
"thing"
"[Function]"
"returns true if stream is a stream which can handle input operations, otherwise returns nil."

INSPECT 
"thing"
"[Function]"
"inspects thing, any Lisp data object."

INTEGER-DECODE-FLOAT 
"float"
"[Function]"
"returns three values: the significand scaled to be an integer, the exponent, and the sign of float."

INTEGERP 
"object"
"[Function]"
"returns true if object is an integer; otherwise returns false."

INTERN 
"string &optional package"
"[Function]"
"searches package (a package object or package name, defaulting to the current package) and all
inherited packages for the symbol named by string. If not found, it creates and interns such a symbol.
Returns two values, the found or new symbol, and a Boolean value indicating whether the symbol
already existed (t indicates that it existed, nil that it was created)."

#|
INTERNAL-TIME-UNITS-PER-SECOND  
NIL "[Constant]"
"an integer that is the number of Macintosh Common Lisp internal time units per second."
|#

INTERSECTION 
"list1 list2 &key :test :test-not :key"
"[Function]"
"returns the intersection of list1 and list2, that is, a list of those elements which are in both list1 and
list2. If either list has duplicate entries, the redundant entries may or may not appear in the result.
list1 and list2 are not modified."

ISQRT 
"integer"
"[Function]"
"returns the integer square root of integer."

KEYWORDP 
"thing"
"[Function]"
"returns true if thing is a symbol in the keyword package. Every symbol that is a keyword is written
with a leading colon and always evaluates to itself."

LABELS 
"({(name lambda-list {declaration | doc-string}* {function-body-form}*)}*)
{labels-body-form}*"
"[Special Form]"
"creates local function definitions whose scope encompasses both the body and the function definitions
themselves. If there are global functions with the same names, the local definitions take precedence
within the body of the labels. That is, labels can be used to define mutually recursive functions, while
flet cannot; but a local function definition with flet can refer to the global definition, while one with
labels cannot."

LAMBDA 
"lambda-list {declarations}* {form}*"
"[Special Form]"
"indicates a function with parameters specified by lambda-list and body specified by forms."

#|
LAMBDA-LIST-KEYWORDS  
NIL "[Constant]"
"a list of all the lambda-list keywords used in Macintosh Common Lisp, including the additional ones
used only by defmacro."
|#

LAMBDA-PARAMETERS-LIMIT  
NIL "[Constant]"
"a positive integer that is the upper exclusive bound on the number of distinct parameter names that
may appear in a single lambda list."

LAST 
"list &optional count"
"[Function]"
"returns the last count conses of list."

LCM 
"&rest integers"
"[Function]"
"returns the least common multiple of its arguments."

LDB 
"bytespec integer"
"[Function]"
"returns the byte of integer specified by bytespec."

LDB-TEST 
"bytespec integer"
"[Function]"
"returns true if any of the bits in the specified bytespec of integer are 1's."

LDIFF 
"list sublist"
"[Function]"
"returns a new list containing the portion of list prior to sublist, which should be a cons appearing in
list. If sublist does not appear in list, a copy of the entire list is returned."

#|
LEAST-NEGATIVE-DOUBLE-FLOAT  
NIL "[Constant]"
"the negative double-float floating-point number closest in value, but not equal to, zero in Macintosh
Common Lisp."

LEAST-NEGATIVE-LONG-FLOAT  
NIL "[Constant]"
"the negative long-format floating-point number closest in value, but not equal to, zero in Macintosh
Common Lisp."

LEAST-NEGATIVE-SHORT-FLOAT  
NIL "[Constant]"
"the negative short-format floating-point number closest in value, but not equal to, zero in Macintosh
Common Lisp."

LEAST-NEGATIVE-SINGLE-FLOAT  
NIL "[Constant]"
"The negative floating-point number closest in value, but not equal to, zero in Macintosh Common Lisp."

LEAST-POSITIVE-DOUBLE-FLOAT  
NIL "[Constant]"
"the positive double-float floating-point number closest in value, but not equal to, zero in Macintosh
Common Lisp."

LEAST-POSITIVE-LONG-FLOAT  
NIL "[Constant]"
"the positive long-format floating-point number closest in value, but not equal to, zero in Macintosh
Common Lisp."

LEAST-POSITIVE-SHORT-FLOAT  
NIL "[Constant]"
"the positive short-format floating-point number closest in value, but not equal to, zero in Macintosh
Common Lisp."

LEAST-POSITIVE-SINGLE-FLOAT  
NIL "[Constant]"
"the negative floating-point number closest in value, but not equal to, zero in Macintosh Common Lisp."
|#

LENGTH 
"sequence"
"[Function]"
"returns the number of elements in sequence."

LET 
"({variable | (variable value) }*) {declaration}* {form}*"
"[Special Form]"
"creates a binding for each variable (in parallel) and evaluates forms in the resulting environment.
Returns the value of the last form."

LET* 
"({variable | (variable value) }*) {declaration}* {form}*"
"[Special Form]"
"creates a binding for each variable (sequentially) and evaluates forms in the resulting environment.
This sequential binding allows the expression for the value of a variable to refer to variables bound
earlier in the let* form. Returns the value of the last form."

#|
LISP-IMPLEMENTATION-TYPE  
NIL
"[Function]"
"returns the string \"Macintosh Common Lisp\"."

LISP-IMPLEMENTATION-VERSION  
NIL
"[Function]"
"returns the version of Macintosh Common Lisp."
|#

LIST 
"&rest arguments"
"[Function]"
"constructs and returns a list containing arguments as its elements."

LIST* 
"object &rest more-objects"
"[Function]"
"constructs and returns a list containing object and more-objects. Unlike list, list* places the last
more-objects in the final cdr of the list. If the last argument to list* is an atom, this will result in a
dotted list; if the last argument is a list, it will have the effect of appending the other arguments to
this list."

LIST-ALL-PACKAGES  
NIL
"[Function]"
"returns a list of all packages that currently exist in the Lisp system."

LIST-LENGTH 
"list"
"[Function]"
"returns the length of list as an integer, or nil if list is a circular list."

LISTEN 
"&optional input-stream"
"[Function]"
"returns true if a character is immediately available from input-stream. Returns nil if a character is
not available or the stream is at end-of-file."

LISTP 
"object"
"[Function]"
"returns true if object is a cons or the empty list; otherwise returns false. listp returns true on the
empty list. This is the only difference between listp and consp. listp does not check whether the list is
terminated by nil or is a dotted list."

#|
LOAD 
"filename &key :verbose :print :if-does-not-exist :foreign-files :system-libraries
:unreferenced-lib-names"
"[Function]"
"loads the file named by filename into the Macintosh Common Lisp environment. This is equivalent to
opening an editor buffer to the file and evaluating all the forms in the buffer (which is equivalent to
typing the forms into the Listener)."
|#

LOCALLY 
"{declaration}* {form}*"
"[Special form]"
"executes forms with the declarations in effect. When a locally form appears at top level, the forms in
its body are processed as top-level forms. This function may be used to wrap declarations around a
defun or defmacro."

LOG 
"number &optional base"
"[Function]"
"returns the logarithm of number in base."

LOGAND 
"&rest integers"
"[Function]"
"returns the bit-wise logical “and” of its arguments. If no argument is given, then the result is -1."

LOGANDC1 
"integer1 integer2"
"[Function]"
"returns (logand (lognot integer1) integer2)."

LOGANDC2 
"integer1 integer2"
"[Function]"
"returns (logand integer1 (lognot integer2))."

LOGBITP 
"index integer"
"[Function]"
"returns true if the bit in integer whose index is index is a one-bit; return nil if it is a zero bit. The
rightmost bit of integer is bit 0."

LOGCOUNT 
"integer"
"[Function]"
"returns the number of “on” bits in integer. If integer is positive, then the one bits in its binary
representation are counted; if integer is negative, then the zero bits in the two’s-complement
representation are counted."

LOGEQV 
"&rest integers"
"[Function]"
"returns the logical-exclusive-nor of the integers."

LOGIOR 
"&rest integers"
"[Function]"
"returns the bit-wise logical inclusive “or” of its arguments. If no argument is given, then the result
is 0."

LOGNAND 
"integer1 integer2"
"[Function]"
"returns (lognot (logand integer1 integer2))."

LOGNOR 
"integer1 integer2"
"[Function]"
"returns the logical nor of integer1 and integer2. Each bit that is a 0 in both integer1 and integer2
will be a 1 in the result."

LOGNOT 
"integer"
"[Function]"
"returns the bit-wise logical “not” of its argument. Every bit of the result is the complement of the
corresponding bit in the argument."

LOGORC1 
"integer1 integer2"
"[Function]"
"returns (logior (lognot integer1) integer2)."

LOGORC2 
"integer1 integer2"
"[Function]"
"returns (logior integer1 (lognot integer2))."

LOGTEST 
"integer1 integer2"
"[Function]"
"returns true if any of the 1 bits in integer1 are also 1 bits in integer2."

LOGXOR 
"&rest integers"
"[Function]"
"returns the bit-wise logical exclusive “or” of its arguments. If no argument is given, then the result
is 0."

LONG-FLOAT-EPSILON  
NIL "[Constant]"
"the smallest positive floating point number e such that (not (= (float 1 e) (+ (float 1 e) e)))."

LONG-FLOAT-NEGATIVE-EPSILON  
NIL "[Constant]"
"The smallest negative floating point number e such that (not (= (float 1 e) (- (float 1 e) e)))."

LONG-SITE-NAME  
NIL
"[Function]"
"returns a string giving the full name of the site at which the Lisp is running. This function should be
redefined by the user."

LOOP 
"{form}*"
"[Macro]"
"repeatedly evaluates+ forms. loop will continue until explicitly terminated by a throw, return, or
go. A set of extensions to loop is documented in Common Lisp: The Language, chapter 26."


LOOP-FINISH 
"()"
"[macro]"
"Causes the iteration to terminate \"normally\", the same as implicit
termination by an iteration driving clause, or by use of WHILE or
UNTIL -- the epilogue code (if any) will be run, and any implicitly
collected result will be returned as the value of the LOOP."

LOWER-CASE-P 
"char"
"[Function]"
"returns true if char is a lowercase character; otherwise returns false. char must be a character."

#|
MACHINE-INSTANCE  
NIL
"[Function]"
"returns the name of the machine running Macintosh Common Lisp; this might be a local nickname or
a serial number. This function should be redefined by the user."

MACHINE-TYPE  
NIL
"[Function]"
"returns the type of Macintosh computer running Macintosh Common Lisp."

MACHINE-VERSION  
NIL
"[Function]"
"returns a string that identifies the version of the current hardware running Macintosh Common Lisp."
|#

MACRO-FUNCTION 
"symbol &optional environment"
"[Function]"
"returns the macro expansion function of the global macro definition associated with symbol. If
symbol does not name a macro, returns nil."

MACROEXPAND 
"form &optional environment"
"[Function]"
"expands form repeatedly within environment until it is no longer a macro call, and returns the
expansion and a second value, t if form was a macro call and nil if it was not."

MACROEXPAND-1 
"form &optional environment"
"[Function]"
"returns the result of expanding form oncewithin environment . Returns the expansion and a second
value, t if the form was a macro call and nil if it was not."

MACROLET 
"({(name lambda-list {declaration | doc-string}* {form}*)}*) {form}*"
"[Special Form]"
"constructs one or more local macro definitions. macrolet is equivalent to flet except that it generates
macro definitions rather than functions. the value of the last form is returned."

MAKE-ARRAY 
"dimensions &key :element-type :initial-element :initial-contents :adjustable
:fill-pointer :displaced-to :displaced-index-offset"
"[Function]"
"constructs and returns an array. dimensions should be an integer or a list of integers."

MAKE-BROADCAST-STREAM 
"&rest stream"
"[Function]"
"returns an output stream which sends its output to all the given streams and returns the result of
performing the operation on the last stream; all other values are discarded."

MAKE-CONCATENATED-STREAM 
"&rest streams"
"[Function]"
"returns an input stream that reads from streams in sequence; when one stream is at end-of-file, the
function takes input from the next."

MAKE-DISPATCH-MACRO-CHARACTER 
"char &optional non-terminating-p readtable"
"[Function]"
"makes the character char a dispatching macro character in readtable (which defaults to *readtable*).
If non-terminating-p is nil (the default), char is a terminating macro character; otherwise it is
non-terminating and may be embedded within extended tokens."

MAKE-ECHO-STREAM 
"input-stream output-stream"
"[Function]"
"returns a stream that gets input from input-stream and sends output to output-stream. In addition,
all input received is echoed to output-stream."

MAKE-HASH-TABLE 
"&key :test :size :rehash-size :rehash-threshold :weak :hash-function"
"[Function]"
"creates and returns a new hash table. :test should be one of the values #`eq, #`eql, #`equal, or
#`equalp, or one of the symbols eq, eql, equal or equalp."

MAKE-INSTANCE 
"class &rest initargs"
"[Generic Function]"
"creates a new instance of the given class."

MAKE-LIST 
"size &key :initial-element"
"[Function]"
"returns a list containing size elements, each of which is intialized to :initial-element. size should be
a non-negative integer; the default value of :initial-element is nil."

MAKE-LOAD-FORM 
"object"
"[Generic Function]"
"returns two values, a form that, when evaluated at load time, returns an object that is equivalent to
object, and a form that, when evaluated at load time, performs further initialization of object. See
Common Lisp: The Language, pages 659-662."

MAKE-LOAD-FORM-SAVING-SLOTS 
"object &optional slots"
"[Generic Function]"
"returns two values suitable for return from a make-load-form method. The first argument is the
object; the optional second argument is a list of the names of slots to preserve. It defaults to all of the
local slots."

MAKE-PACKAGE 
"package-name &key :nicknames :use :internal-size :external-size"
"[Function]"
"creates and returns a package named package-name, which may be either a string or symbol. If it is a
symbol, the symbol's print-name is used. The list :nicknames contains strings that serve as
alternative names; :use is a list of packages whose external symbols are inherited by the new
package. The default value of :use is the value of the variable *make-package-use-defaults* which is
initially (\"COMMON-LISP\" \"CCL\")."

MAKE-PATHNAME 
"&key :host :device :directory :name :type :version :defaults :case"
"[Function]"
"creates and returns a pathname with components based on the arguments."

MAKE-RANDOM-STATE 
"&optional state"
"[Function]"
"returns a new random state object, based on state (which defaults to *random-state*). If state is nil
or omitted, the current random state object is copied; if it is t, a completely fresh random state is
generated."

MAKE-SEQUENCE 
"type size &key :initial-element"
"[Function]"
"creates and returns a sequence of type with length size."

MAKE-STRING 
"size &key :initial-element :element-type"
"[Function]"
"returns a simple string of length size, each character of which is initialized to :initial-element. The
:element-type argument names the type of the elements of the stream; its default is character."

MAKE-STRING-INPUT-STREAM 
"string &optional start end"
"[Function]"
"creates and returns an input stream of type string-stream that reads from string."

MAKE-STRING-OUTPUT-STREAM 
"&key :element-type"
"[Function]"
"creates and returns an output stream that accumulates all output given it in a string-stream for the
benefit of get-output-stream-string. The :element-type argument specifies what characters must be
accepted by the created stream; if the argument is omitted the stream must accept all characters."

MAKE-SYMBOL 
"print-name"
"[Function]"
"creates and returns an uninterned symbol with name print-name."

MAKE-SYNONYM-STREAM 
"symbol"
"[Function]"
"returns a synonym stream associated with symbol. Any operations performed on the stream will act
on the stream that is (at that point) bound to symbol. symbol may be bound to a new stream, and the
operations will act on the new stream."

MAKE-TWO-WAY-STREAM 
"input-stream output-stream"
"[Function]"
"returns a bidirectional stream that receives input from input-stream and sends output to
output-stream."

MAKUNBOUND 
"symbol"
"[Function]"
"causes the dynamic (special) variable symbol to become unbound (have no value). makunbound
returns symbol."

MAP 
"result-type function sequence &rest more-sequences"
"[Function]"
"applies function to the first element of each sequence, then the second element of each, and so on until
the end of the shortest sequence is reached. The results of the function calls are collected in a
sequence of type result-type. If the result-type is nil, the results are not collected and nil is
returned."

MAPC 
"function list &rest more-lists"
"[Function]"
"applies function to the elements of list and more-lists. The results are not stored. list is returned. If
the lists are not all the same length, the iteration terminates when the shortest list runs out.
function can be only of type symbol or function."

MAPCAN 
"function list &rest more-lists"
"[Function]"
"applies function to the car of list and more-lists, then to the cadr, and so on. The results, which must
be lists, are combined using nconc. If the lists are not all the same length, the iteration terminates
when the shortest list runs out. function can be only of type symbol or function."

MAPCAR 
"function list &rest more-lists"
"[Function]"
"applies function to the car of list and more-lists, then to the cadr, and so on. The results are collected
into a list, which is returned. If the lists are not all the same length, the iteration terminates when
the shortest list runs out. function can be only of type symbol or function."

MAPCON 
"function list &rest more-lists"
"[Function]"
"applies function first to list and more-lists, and then to successive cdrs of list and more-lists. The
results, which must be lists, are combined using nconc. If the lists are not all the same length, the
iteration terminates when the shortest list runs out. function can be only of type symbol or function."

MAPHASH 
"function hash-table"
"[Function]"
"calls function for each entry in hash-table, passing as arguments both the key and the value. Entries
should not be added or removed while maphash is in progress. maphash returns nil."

MAPL 
"function list &rest more-lists"
"[Function]"
"applies function first to list and more-lists, and then to successive cdrs of list and more-lists. The
results are not stored (i.e. the operation is only for side-effect). list is returned. If the lists are not
all the same length, the iteration terminates when the shortest list runs out. function can be only of
type symbol or function."

MAPLIST 
"function list &rest more-lists"
"[Function]"
"applies function first to list and more-lists, and then to successive cdrs of list and more-lists. The
results are collected into a list, which is returned. If the lists are not all the same length, the
iteration terminates when the shortest list runs out. function can be only of type symbol or function."

MASK-FIELD 
"bytespec integer"
"[Function]"
"returns an integer all of whose bits are zero but the byte specified in bytespec; that byte is the same
as the one at bytespec pin integer."

MAX 
"number &rest more-numbers"
"[Function]"
"returns the largest of numbers."

MEMBER 
"item list &key :test :test-not :key"
"[Function]"
"searches list for a top-level element that matches item. If a match is successful, member returns the
rest of the list starting with the element that matched item; otherwise returns nil."

MEMBER-IF 
"test list &key :key"
"[Function]"
"searches list for the first top-level element satisfying test. If one is found, the rest of the list
(beginning with that element) is returned. If none are found, nil is returned."

MEMBER-IF-NOT 
"test list &key :key"
"[Function]"
"searches list for the first top-level element that fails to satisfy test. If such an element is found, the
rest of the list (beginning with that element) is returned, otherwise nil is returned."

MERGE 
"result-type sequence1 sequence2 predicate &key :key"
"[Function]"
"destructively merges sequence1 and sequence2 into a new sequence of type result-type. result-type
must be a subtype of sequence."

MERGE-PATHNAMES 
"pathname &optional defaults default-version"
"[Function]"
"creates a new pathname resulting from merging pathname with defaults (which defaults to
*default-pathname-defaults*). pathname may be a pathname, a string, or a stream that is or was
open to a file; if defaults is a logical pathname, pathname may be a logical pathname namestring."

MIN 
"number &rest more-numbers"
"[Function]"
"returns the smallest of numbers."

MINUSP 
"number"
"[Function]"
"returns true if number is strictly less than zero; otherwise returns nil. number must be a
non-complex number."

MISMATCH 
"sequence1 sequence2 &key :from-end :test :test-not :key :start1 :start2 :end1 :end2"
"[Function]"
"compares the elements of the specified portions of sequence1 and sequence2. If two elements do not
match, the index within sequence1 of the leftmost position at which the elements fail to match is
returned. If all elements match, nil is returned."

MOD 
"number divisor"
"[Function]"
"returns the root of number modulo divisor. The result will have the same sign as divisor."

#|
MOST-NEGATIVE-DOUBLE-FLOAT  
NIL "[Constant]"
"the double-float floating-point number closest in value, but not equal to, minus infinity in Macintosh
Common Lisp."

MOST-NEGATIVE-FIXNUM  
NIL "[Constant]"
"The fixnum closest to minus infinity in Macintosh Common Lisp."

MOST-NEGATIVE-LONG-FLOAT  
NIL "[Constant]"
"the long-float floating-point number closest in value, but not equal to, minus infinity in Macintosh
Common Lisp."

MOST-NEGATIVE-SHORT-FLOAT  
NIL "[Constant]"
"the short-float floating-point number closest in value, but not equal to, minus infinity in Macintosh
Common Lisp."

MOST-NEGATIVE-SINGLE-FLOAT  
NIL "[Constant]"
"the floating-point number closest in value, but not equal to, minus infinity in Macintosh Common
Lisp."

MOST-POSITIVE-DOUBLE-FLOAT  
NIL "[Constant]"
"the double-float floating-point number closest in value, but not equal to, infinity in Macintosh
Common Lisp."

MOST-POSITIVE-FIXNUM  
NIL "[Constant]"
"the fixnum closest in value, but not equal to, infinity in Macintosh Common Lisp."

MOST-POSITIVE-LONG-FLOAT  
NIL "[Constant]"
"the long-float floating-point number closest in value, but not equal to, infinity in Macintosh Common
Lisp."

MOST-POSITIVE-SHORT-FLOAT  
NIL "[Constant]"
"the short-float floating-point number closest in value, but not equal to, infinity in Macintosh
Common Lisp."

MOST-POSITIVE-SINGLE-FLOAT  
NIL "[Constant]"
"the floating-point number closest in value, but not equal to, infinity in Macintosh Common Lisp."
|#

MULTIPLE-VALUE-BIND 
"({var}*) values-form {declaration}* {form}*"
"[Macro]"
"evaluates values-form, and binds the multiple values returned to the vars. The forms are evaluated
in the resulting environment. The value of the last form is returned."

MULTIPLE-VALUE-CALL 
"function {form}*"
"[Special Form]"
"calls function, passing as arguments all the multiple values returned by forms. The first argument
(function) is evaluated."

MULTIPLE-VALUE-LIST 
"form"
"[Macro]"
"collects the multiple values returned by form, and returns them in a list."

MULTIPLE-VALUE-PROG1 
"form {more-forms}*"
"[Special Form]"
"evaluates form, and saves the values it returns. Then evaluates more-forms, discarding their
returned values. When done, returns the values returned by form."

MULTIPLE-VALUE-SETQ 
"({var}*) form"
"[Macro]"
"calls form, and uses the returned multiple values to set (not bind) the vars."

MULTIPLE-VALUES-LIMIT  
NIL "[Constant]"
"a positive integer that is the upper exclusive bound on the number of values that may be returned
from a function."

NAME-CHAR 
"name"
"[Function]"
"returns the character with name name, or nil if there is no such character."

NAMESTRING 
"pathname"
"[Function]"
"returns a string representation of pathname, which may be a pathname, a string, or a stream that is
or was open to a file."

NBUTLAST 
"list &optional num"
"[Function]"
"destructively modifies list to remove the last num elements. num defaults to 1. If list is shorter than
num, the empty list is returned and list is not modified. (Therefore one normally writes (setq a
(nbutlast a)) rather than (nbutlast a).)"

NCONC 
"&rest lists-or-thing"
"[Function]"
"concatenates lists destructively and returns the resulting list. The lists are not copied, but are
destructively altered in place. nconc is the destructive equivalent of append."

#|
NIL  
NIL "[Constant]"
"the false value in Common Lisp. nil is a symbol, a constant, the Boolean false, a data type, a logical
operator, an object of type null, and the usual terminator of a list. It is also equivalent to the empty
list. It is no longer equivalent to Pascal null, which in Macintosh Common Lisp is now equivalent to
the macptr %null-ptr."
|#

NINTERSECTION 
"list1 list2 &key :test :test-not :key"
"[Function]"
"returns the intersection of list1 and list2, that is, a list of those elements which are in both list1 and
list2. If either list has duplicate entries, the redundant entries may or may not appear in the result.
list1 and list2 may be modified by the process. (nintersection is the destructive form of
intersection.)"

NINTH 
"list"
"[Function]"
"returns the ninth element of list, using one-based addressing."

NOT 
"object"
"[Function]"
"returns true if object is nil; otherwise returns false. It inverts its argument considered as a Boolean
value."

NOTANY 
"predicate sequence &rest more-sequences"
"[Function]"
"applies predicate to succesive elements of sequence and more-sequences until a call returns non-nil.
If a call returns non-nil, notany returns false. If no call returns true, notany returns non-nil.
predicate must take as many arguments as there are sequences."

NOTEVERY 
"predicate sequence &rest more-sequences"
"[Function]"
"applies predicate to succesive elements of sequence and more-sequences until a call returns nil. If a
call returns nil, notevery immediately returns a non-nil value. If all calls return true, notevery
returns nil. predicate must take as many arguments as there are sequences."

NRECONC 
"list-1 list-2"
"[Function]"
"reverses list-1, and places its elements (reversed) at the head of list-2. list-1 is destructively
modified. This function has exactly the same side-effect behavior as (nconc (nreverse x) y) but is
potentially more efficient."

NREVERSE 
"sequence"
"[Function]"
"returns a sequence with the elements of sequence reversed. The original sequence may be modified."

NSET-DIFFERENCE 
"list1 list2 &key :test :test-not :key"
"[Function]"
"returns a list of elements in list1 that are not elements of list2. list1 and list2 may be modified by
the process. (nset-difference is the destructive form of set-difference.)"

NSET-EXCLUSIVE-OR 
"list1 list2 &key :test :test-not :key"
"[Function]"
"returns a list containing those elements that are in list1 or in list2 but are not in both. list1 and
list2 may be modified by the process. (nset-exclusive-or is the destructive form of
set-exclusive-or.)"

NSTRING-CAPITALIZE 
"string &key :start :end"
"[Function]"
"destructively modifies the given portion of string, capitalizing all words."

NSTRING-DOWNCASE 
"string &key :start :end"
"[Function]"
"destructively modifies the given portion of string, converting all uppercase characters to lowercase."

NSTRING-UPCASE 
"string &key :start :end"
"[Function]"
"destructively modifies the given portion of string, converting all lowercase characters to uppercase."

NSUBLIS 
"a-list tree &key :test :test-not :key"
"[Function]"
"destructively modifies tree, replacing elements that appear as keys in a-list with the corresponding
value from a-list. In effect, nsublis can perform several nsubst operations simultaneously."

NSUBST 
"new old tree &key :test :test-not :key"
"[Function]"
"destructively modifies tree, replacing occurrences of old with new."

NSUBST-IF 
"new test tree &key :key"
"[Function]"
"destructively modifies tree, replacing elements that satisfy test with new."

NSUBST-IF-NOT 
"new test tree &key :key"
"[Function]"
"destructively modifies tree, replacing elements that don't satisfy test with new."

NSUBSTITUTE 
"new-item old-item sequence &key :start :end :from-end :count :test :test-not :key"
"[Function]"
"returns a sequence equivalent to sequence except that occurrences of old-item within a given
subsequence are replaced with new-item. The :count argument, if given, limits the number of
substitutions which take place. The original sequence may be modified. This is the destructive
equivalent of substitute."

NSUBSTITUTE-IF 
"new-item test sequence &key :start :end :from-end :count :key"
"[Function]"
"returns a sequence equivalent to sequence except that elements which satisfy test within the given
subsequence are replaced with new-item. The :count argument, if given, limits the number of
substitutions which take place. The original sequence may be modified. This is the destructive
equivalent of substitute-if."

NSUBSTITUTE-IF-NOT "new-item test sequence &key :start :end :from-end :count :key"
"[Function]"
"returns a sequence equivalent to sequence except that elements which do not satisfy test within the
given subsequence are replaced with new-item. The :count argument, if given, limits the number of
substitutions which take place. The original sequence may be modified. This is the destructive
equivalent of substitute-if-not."

NTH 
"n list"
"[Function]"
"returns the nth element of list (where the car of list is the \"zeroth\" element )."

NTHCDR 
"n list"
"[Function]"
"performs the cdr operation n times on list and returns the result."

NULL 
"thing"
"[Function]"
"returns true if thing is the empty list (), otherwise returns nil. This is equivalent to the function
not, except that null is normally used to check for the empty list and not to invert. The programmer
can express intent by choice of function name."

NUMBERP 
"object"
"[Function]"
"returns true if object is a number; otherwise returns false. More specific numeric data type tests
include integerp, rationalp, floatp, and complexp."

NUMERATOR 
"rational"
"[Function]"
"returns the numerator of the canonical reduced form of rational."

NUNION 
"list1 list2 &key :test :test-not :key"
"[Function]"
"returns a list containing the union of the elements list1 and list2. Any element that is contained in
list1 or list2 will be contained in the result list. list1 and list2 may be modified by the process.
(nunion is the destructive form of union.)"

ODDP 
"integer"
"[Function]"
"returns true if integer is odd (not evenly divisible by two); otherwise returns nil."

OPEN 
"filename &key :direction :element-type :if-exists :if-does-not-exist :external-format :fork
:mac-file-creator"
"[Function]"
"opens a stream to the file specified by filename, which may be a string, a pathname, a logical
pathname, or a stream."

OR 
"{form}*"
"[Macro]"
"evaluates each form sequentially. If or reaches a form that returns non-nil, it returns the value of
that form without evaluating any more forms. If it reaches the last form, it returns that form's value."

OUTPUT-STREAM-P 
"thing"
"[Function]"
"returns true if thing is a stream which can handle output, otherwise returns nil."

PACKAGE-NAME 
"package"
"[Function]"
"returns the name of package as a string, or nil if applied to a deleted package."

PACKAGE-NICKNAMES 
"package"
"[Function]"
"returns a list of the nickname strings of package. This will not include the primary name."

PACKAGE-SHADOWING-SYMBOLS 
"package"
"[Function]"
"returns a list of symbols which have been declared (by shadow or shadowing-import) as shadowing
symbols in package."

PACKAGE-USE-LIST 
"package"
"[Function]"
"returns a list of packages used by package."

PACKAGE-USED-BY-LIST 
"package"
"[Function]"
"returns a list of all the packages that use package."

PACKAGEP 
"thing"
"[Function]"
"returns true if thing is a package, otherwise false."

PAIRLIS 
"keys data &optional a-list"
"[Function]"
"creates an a-list associated matching elements from the lists keys and data. This a-list is appended to
a-list. It is an error if the lists keys and data are not the same length."

PARSE-INTEGER 
"string &key :start :end :radix :junk-allowed"
"[Function]"
"reads and returns an integer from the indicated portion of string. Returns a second value that is the
index into the string of the delimiter that terminated the parse, or the index beyond the substring if
the parse terminated at the end of the substring. An error is signaled if the substring cannot be
parsed as an integer."

PARSE-NAMESTRING 
"thing &optional host defaults &key :start :end :junk-allowed"
"[Function]"
"parses thing to a pathname. thing is usually a string, but may be a logical pathname, pathname, or
stream. The host and defaults arguments are used only to determine pathname syntax, not for
supplying default pathname components."

PATHNAME 
"thing"
"[Function]"
"coerces thing to a pathname, which it returns. thing should be a pathname, string, or stream."

PATHNAME-DEVICE 
"pathname &key :case"
"[Function]"
"returns the device component of pathname."

PATHNAME-DIRECTORY 
"pathname &key :case"
"[Function]"
"returns the directory component of pathname."

PATHNAME-HOST 
"pathname &key :case"
"[Function]"
"returns the host component of pathname."

PATHNAME-NAME 
"pathname &key :case"
"[Function]"
"returns the name component of pathname."

PATHNAME-TYPE 
"pathname &key :case"
"[Function]"
"returns the type component of pathname."

PATHNAME-VERSION 
"pathname"
"[Function]"
"returns the version component of pathname."

PATHNAMEP 
"thing"
"[Function]"
"returns true if thing is a pathname, otherwise false."

PEEK-CHAR 
"&optional peek-type input-stream eof-error-p eof-value recursive-p"
"[Function]"
"returns the next character of stream according to peek-type, and leaves the character in the input
stream. peek-type may be nil (return next character), t (skip whitespace, then return next
character) or a character (advance to first occurrence of the character)."

PHASE 
"number"
"[Function]"
"returns the angle part of the polar representation of number as a complex number. The return value
is in radians."

PI  
NIL "[Constant]"
"The best possible approximation of pi in floating-point format."

PLUSP 
"number"
"[Function]"
"returns true if number is strictly greater than zero; otherwise returns nil. number must be a
non-complex number."

POP 
"place"
"[Macro]"
"returns the car of the contents of place , which can be any generalized variable containing a list and
acceptable as a generalized variable to setf. Sets place to point to the cdr of its previous contents. "

POSITION 
"item sequence &key :start :end :from-end :key :test :test-not"
"[Function]"
"returns the index of the first element of sequence that match item using the given test function;
returns nil if no element matches."

POSITION-IF 
"test sequence &key :from-end :start :end :key"
"[Function]"
"returns the position of the first element in the given range of sequence that satisfies test, or nil if no
element satisfies the test."

POSITION-IF-NOT 
"test sequence &key :from-end :start :end :key"
"[Function]"
"returns the position of the first element in the given range of sequence that does not satisfy test, or
nil if all the elements satisfy test."

PPRINT 
"object &optional output-stream"
"[Function]"
"outputs a newline character and the pretty-print representation of object to output-stream. pprint
returns no value. Common Lisp user-controlled pretty-printing is described in Common Lisp: The
Language, 2d edition, Chapter 27."

PRIN1 
"object &optional output-stream"
"[Function]"
"outputs the printed representation of data-object to output-stream, using escape characters as
appropriate. Returns object."

PRIN1-TO-STRING 
"thing"
"[Function]"
"thing is printed, as if by prin1, but the output is collected in a string, which is returned."

PRINC 
"object &optional output-stream"
"[Function]"
"outputs the printed representation of object to output-stream, without any escape characters.
Returns object."

PRINC-TO-STRING 
"thing"
"[Function]"
"thing is printed, as if by princ, but the output is collected in a string, which is returned."

PRINT 
"object &optional output-stream"
"[Function]"
"outputs the printed representation of data object to output-stream, preceded by a newline and
followed by a space. Returns object."

PROBE-FILE 
"pathname"
"[Function]"
"if pathname corresponds to an existing file or folder, returns its true name. If pathname does not
correspond to an existing file or folder, returns nil."

PROCLAIM 
"declaration-spec"
"[Function]"
"provides a global declaration (called a proclamation) or a declaration that is computed by a program.
proclaim returns nil."

PROG 
"({var | (var [init])}*) {declaration}* {tag | statement}*"
"[Macro]"
"binds the vars to the values of the inits in parallel (or to nil for vars with no corresponding init),
and then executes the statements. The entire prog form is implicitly surrounded by a block named nil
(so that return may be used at any time to exit from the construct), and the body is a tagbody. prog
returns nil."

PROG* 
"({var | (var [init])}*) {declaration}* {tag | statement}*"
"[Macro]"
"binds the vars to the values of the inits in sequence (or to nil for vars with no corresponding init),
and then executes the statements. The entire prog* form is implicitly surrounded by a block named
nil (so that return may be used at any time to exit from the construct), and the body is a tagbody.
prog* returns nil."

PROG1 
"{form}*"
"[Macro]"
"evaluates each form in order, left to right. The first form is evaluated and its value is stored; the
other forms are evaluated, usually for their side effects; the value of the first form is then returned."

PROG2 
"{form}*"
"[Macro]"
"evaluates each form in order, left to right. The value of the second form is stored and returned."

PROGN 
"{form}*"
"[Special Form]"
"evaluates each form in order, left to right. The values of all forms but the last are discarded; the
value of the last form is returned."

PROGV 
"symbols values {form}*"
"[Special Form]"
"binds one or more dynamic variables in the list symbols to the values in the list values. With these
bindings in effect, the forms are executed. Both symbols and values are computed quantities, rather
than stated explicitly."

PROVIDE 
"module"
"[Function]"
"adds a new module name to the list of modules maintained in the variable *modules*, indicating that
the module module has been provided. provide is no longer part of the Common Lisp standard."

PSETF 
"{place newvalue}*"
"[Macro]"
"sets the contents of places to the corresponding newvalues. The assignments are done in parallel."

PSETQ 
"{variable form}*"
"[Macro]"
"sets the value of the current binding of each variable to the result of evaluating the corresponding
form. The assignmentes are performed in parallel Returns nil."

PUSH 
"item place"
"[Macro]"
"conses item onto the list contained in place , which can be any generalized variable containing a list
and acceptable as a generalized variable to setf. Stores the resulting list in place and returns the new
contents of place."

PUSHNEW 
"item place &key :test :test-not :key"
"[Macro]"
"pushes item onto the list in place, if the list does not already contain item (as determined by :test).
The modified list is returned."

QUOTE 
"object"
"[Special Form]"
"returns object , which may be any object, without evaluating it."

RANDOM 
"number &optional state"
"[Function]"
"returns a pseudo random number between zero (inclusive) and number (exclusive). state is an
object of type random-state and defaults to the value of the variable *random-state*."

RANDOM-STATE-P 
"thing"
"[Function]"
"returns true if thing is a random state, otherwise false."

RASSOC 
"value a-list &key :test :test-not :key"
"[Function]"
"searches a-list for the first pair whose cdr matches value. Returns the pair, or nil if the search fails."

RASSOC-IF 
"predicate a-list &key :key"
"[Function]"
"searches a-list for the first pair matching :key whose cdr satisfies predicate. Returns the pair, or
nil if the search fails."

RASSOC-IF-NOT 
"predicate a-list &key :key"
"[Function]"
"searches a-list for the first pair matching :key whose cdr does not satisfy predicate. Returns the
pair, or nil if the search fails."

RATIONAL 
"number"
"[Function]"
"returns the rational representation of number, any non-complex number. With floating-point
numbers, rational assumes that number is completely accurate and returns a rational number
mathematically equal to the precise value of the floating-point number. Compare rationalize, which
returns the best available approximation of number that keeps numerator and denominator small."

RATIONALIZE 
"number"
"[Function]"
"returns the rational representation of number, any non-complex number. With floating-point
numbers, rationalize assumes that number is accurate only to the precision of the floating-point
representation and returns the best available approximation of number, keeping numerator and
denominator small. Compare rational, which returns the precise value."

RATIONALP 
"object"
"[Function]"
"returns true if object is a rational number; otherwise returns false. A rational number is any
number expressible as the ratio of two integers."

READ 
"&optional input-stream eof-error-p eof-value recursivep"
"[Function]"
"reads the printed representation of a single object from input-stream, builds a corresponding
object, and returns the object."

READ-BYTE 
"binary-input-stream &optional eof-errorp eof-value"
"[Function]"
"reads one byte from binary-input-stream and returns it in the form of an integer."

READ-CHAR 
"&optional input-stream eof-error-p eof-value recursive-p"
"[Function]"
"reads one character from input-stream, and returns the corresponding character object."

READ-CHAR-NO-HANG 
"&optional input-stream eof-errorp eof-value recursive-p"
"[Function]"
"reads and returns a character from input-stream if one is immediately available, otherwise
immediately returns nil."

READ-DELIMITED-LIST 
"char &optional stream recursive-p"
"[Function]"
"reads objects from stream, ignoring whitespace and comments, until an occurrence of the character
char is reached, then returns a list of all the objects it has read so far. char should not be a
whitespace character."

READ-FROM-STRING 
"string &optional eof-error-p eof-value &key :start :end :preserve-whitespace"
"[Function]"
"reads and returns an expression, taking input from string. A second value returned indicates the
index of the first character in string not read."

READ-LINE 
"&optional input-stream eof-error-p eof-value recursive-p"
"[Function]"
"reads a line of text terminated by a newline or end of file from input-stream and returns two values,
the line as a character string and a Boolean value, t if the line was terminated by an end-of-file and
nil if it was terminated by a newline."

READ-PRESERVING-WHITESPACE 
"&optional input-stream eof-errorp eof-value recursive-p"
"[Function]"
"performs the same operation as read, except that if recursive-p is nil or omitted, delimiting
whitespace following a token is not discarded, but is retained at the head of the stream, where it can
be read."

READTABLEP 
"thing"
"[Function]"
"returns t if thing is a readtable, nil if not."

REALP 
"object"
"[Function]"
"returns true if object is a real number; otherwise returns false."

REALPART 
"number"
"[Function]"
"returns the real part of number."

REDUCE 
"function sequence &key :start :end :from-end :initial-value :key"
"[Function]"
"combines the elements of sequence according to function and returns the result. (For example, a list
of numbers can be combined by adding all the numbers together.)"

REM 
"number divisor"
"[Function]"
"returns the remainder of number divided by divisor. The result will have the same sign as number."

REMF 
"place property-name"
"[Macro]"
"removes the property with an indicator eq to indicator from the property list stored in place. The
property indicator and its value are removed by destructively splicing the property list. Returns
true if the property is found; otherwise returns nil. place can be any form acceptable to setf."

REMHASH 
"key hash-table"
"[Function]"
"removes the entry for key from the hash-table hash-table. Returns t if there was such an entry,
otherwise nil."

REMOVE 
"item sequence &key :count :start :end :from-end :test :test-not :key"
"[Function]"
"returns a new sequence equivalent to sequence with occurrences of item removed. The original
sequence is not modified. (The destructive counterpart of remove is delete.)"

REMOVE-DUPLICATES 
"sequence &key :start :end :from-end :test :test-not :key"
"[Function]"
"returns a sequence equivalent to sequence except that all duplicate elements have been removed. The
original sequence is not modified. The destructive version of this function is delete-duplicates."

REMOVE-IF 
"test sequence &key :from-end :start :end :count :key"
"[Function]"
"returns a sequence equivalent to sequence except that elements in the given range that pass test are
removed. The original sequence is not modified."

REMOVE-IF-NOT 
"test sequence &key :from-end :start :end :count :key"
"[Function]"
"returns a sequence equivalent to sequence except that elements in the given range that fail test are
removed. The original sequence is not modified."

REMPROP 
"symbol property-name"
"[Function]"
"removes property-name and its value from the property list of symbol. Returns true if the property
was found and nil if the property was not found."

RENAME-FILE 
"old-pathname new-pathname &key :if-exists"
"[Function]"
"renames the last component of the specified old-pathname to the result of merging new-pathname
with old-pathname. The :if-exists argument has the same meaning as for copy-file."

RENAME-PACKAGE 
"package new-name &optional new-nicknames"
"[Function]"
"the old name and nicknames of package are replaced by new-name and new-nicknames. new-name
should be a string, a symbol (the symbol's print-name is used), or a package object. new-nicknames
should be a string, a symbol, or a list of strings and symbols. package is returned."

REPLACE 
"destination-sequence source-sequence &key :start1 :end1 :start2 :end2"
"[Function]"
"destructively replaces elements in the specified portion of destination-sequence with the elements of
the specified portion of source-sequence. Returns the modified destination-sequence."

REQUIRE 
"module &optional pathname"
"[Function]"
"attempts to load the files in module if they have not already been loaded. require is no longer part of
the Common Lisp standard."

REST 
"list"
"[Function]"
"returns the cdr of list. rest can be used with setf to set the cdr of a list."

RETURN 
"[result-form]"
"[Macro]"
"used to return from a block or from constructs such as do and progn. Returns from a block named nil,
and the block as a whole returns the value of result-form ). If result-form is not supplied, nil is
returned."

RETURN-FROM 
"name [result-form]"
"[Special Form]"
"used to return from a block or from constructs such as do and progn. The function exits from the
innermost block named name, and the block as a whole returns the value of result-form. If
result-form is not supplied, nil is returned."

REVAPPEND 
"list thing"
"[Function]"
"appends the reverse of list to thing. This is equivalent to (append (reverse list) thing) but is
potentially more efficient."

REVERSE 
"sequence"
"[Function]"
"returns a new sequence with the elements of sequence reversed. The original sequence is not modified."

ROOM 
"&optional detailedp"
"[Function]"
"prints information on the amount of space available in the Lisp operating system."

ROTATEF 
"&rest places"
"[Function]"
"rotates the contents of all the places to the left. The contents of the leftmost place is put into the
rightmost place."

ROUND 
"number &optional divisor"
"[Function]"
"returns the integer nearest to number. If number is halfway between two integers (for example
3.5), round converts number to the nearest integer divisible by 2. round returns a second value,
which is the remainder of the rounding operation. When there is a second argument, round first
divides divisor into number, and then applies round to the result."

RPLACA 
"cons object"
"[Function]"
"destructively alters cons so that its car points to object. Returns the modified cons."

RPLACD 
"cons object"
"[Function]"
"destructively alters cons so that its cdr points to object. Returns the modified cons."

SBIT 
"simple-bit-array &rest subscripts"
"[Function]"
"returns the bit in simple-bit-array indicated by the subscripts. This function can be used with setf
to destructively replace a bit-array element."

SCHAR 
"string index"
"[Function]"
"returns the indexth character in string, which must be a simple string. This function can be used
with setf to set a character in a simple string. Indexing is zero-origin."

SEARCH 
"sequence1 sequence2 &key :start1 :end1 :start2 :end2 :from-end :key :test :test-not"
"[Function]"
"searches sequence2 for a subsequence whose elements match those of sequence1. If successful,
returns an index into sequence2 indicating the leftmost element of this subsequence. If not successful,
returns nil."

SECOND 
"list"
"[Function]"
"returns the cadr of list, using one-based addressing."

SET 
"symbol value"
"[Function]"
"assigns value to the result of evaluating symbol, the name of a dynamic (special) variable. Returns
symbol. The function set cannot alter a local (lexically bound) variable."

SET-DIFFERENCE 
"list1 list2 &key :test :test-not :key"
"[Function]"
"returns the as list of elements of list1 that are not elements of list2. list1 and list2 are not modified."

SET-DISPATCH-MACRO-CHARACTER 
"disp-char sub-char function &optional readtable"
"[Function]"
"causes function to be called when disp-char followed by sub-char is read. readtable is the current
readtable."

SET-EXCLUSIVE-OR 
"list1 list2 &key :test :test-not :key"
"[Function]"
"returns a list containing those elements that are in list1 or in list2 but are not in both. list1 and
list2 are not modified."

SET-MACRO-CHARACTER 
"char function &optional non-terminating-p readtable"
"[Function]"
"sets char to be a macro character in readtable (which defaults to *readtable*). When read
encounters char, function is invoked."

SET-SYNTAX-FROM-CHAR 
"to-char from-char &optional to-readtable from-readtable"
"[Function]"
"sets the syntax of to-char in to-readtable (which defaults to *readtable*) to be equal to the syntax of
from-char in from-readtable (which defaults to nil, meaning to use the syntax from the standard
Common Lisp readtable)."

SETF 
"{place newvalue}*"
"[Macro]"
"stores the result of evaluating newvalue into the location that results from examining place. If
multiple place-newvalue pairs are specified, they are processed sequentially. setf returns the last
newvalue."

SETQ 
"{variable form}*"
"[Special Form]"
"sets the value of the current binding of each variable to the result of evaluating the corresponding
form. The assignments are performed sequentially. Returns the value of the last variable."

SEVENTH 
"list"
"[Function]"
"returns the seventh element of list, using one-based addressing."

SHADOW 
"symbols &optional package"
"[Function]"
"searches package for a symbol with the print-name of symbol. If package does not own such a symbol
(inherited symbols do not count), a new internal symbol is created in the package and placed on the
shadowing symbols list. symbols should be a symbol, a string, or a list of symbols and/or strings.
shadow returns t."

SHADOWING-IMPORT 
"symbols &optional package"
"[Function]"
"imports symbols (which should be a symbol or list of symbols) into package, a package object or
package name. This function does not error if the importation causes a conflict with symbols already
accessible in package. The symbols are placed in package's shadowing-symbols list."

SHIFTF 
"{place}+ newvalue"
"[Macro]"
"the contents of all the places are shifted to the place to the left. newvalue is shifted into the rightmost
placo, and the original value of the leftmost place is returned."

SHORT-FLOAT-EPSILON  
NIL "[Constant]"
"The smallest positive floating point number e such that (not (= (float 1 e) (+ (float 1 e) e)))."

SHORT-FLOAT-NEGATIVE-EPSILON  
NIL "[Constant]"
"The smallest negative floating point number e such that (not (= (float 1 e) (- (float 1 e) e)))."

SHORT-SITE-NAME  
NIL
"[Function]"
"returns the short form of the name of the site in which the Lisp is running. This function should be
redefined by the user to return an appropriate value."

SIGNUM 
"number"
"[Function]"
"returns an indication of the sign of number. This will be -1, 1, or 0 for rational numbers, -1.0,
1.0, or 0.0 for floating point numbers. For a complex number z, (signum z) is a complex number of
the same phase but with unit magnitude unless z is a complex zero, in which case the result is z."

SIMPLE-BIT-VECTOR-P 
"thing"
"[Function]"
"returns true if thing is a simple-bit-vector."

SIMPLE-STRING-P 
"thing"
"[Function]"
"returns true if thing is a simple-string."

SIMPLE-VECTOR-P 
"thing"
"[Function]"
"returns true if thing is a simple-vector."

SIN 
"radians"
"[Function]"
"returns the sine of radians, a number in radians."

SINGLE-FLOAT-EPSILON  
NIL "[Constant]"
"The smallest positive floating point number e such that (not (= (float 1 e) (+ (float 1 e) e)))."

SINGLE-FLOAT-NEGATIVE-EPSILON  
NIL "[Constant]"
"The smallest negative floating point number e such that (not (= (float 1 e) (- (float 1 e) e)))."

SINH 
"radians"
"[Function]"
"returns the hyperbolic sine of radians, a number in radians."

SIXTH 
"list"
"[Function]"
"returns the sixth element of list, using one-based addressing."

SLEEP 
"seconds"
"[Function]"
"pauses for seconds seconds."

SLOT-VALUE 
"object slot-name"
"[Function]"
"returns the value contained in the slot slot-name of the given object."

#|
SOFTWARE-TYPE  
NIL
"[Function]"
"returns a string identifying the operating system software. In Macintosh Common Lisp, this is the
type of Macintosh currently running."

SOFTWARE-VERSION  
NIL
"[Function]"
"returns a string identifying version information of the operating system software. In Macintosh
Common Lisp, this includes the Macintosh ROM version and the operating system file version."
|#

SOME 
"predicate sequence &rest more-sequences"
"[Function]"
"predicate is applied to the elements of sequence with index 0, then to those with index 1, and so on,
until the end of the shortest sequence is reached. As soon as predicate returns a non-nil value, that
value is returned. nil is returned if there is no true value. That is, some is true if some invocation of
predicate on sequence returns true."

SORT 
"sequence predicate &key :key"
"[Function]"
"destructively sorts the elements of sequence into an order determined by predicate."

SPECIAL-FORM-P 
"symbol"
"[Function]"
"returns true if symbol names a special form; otherwise returns nil. This is the general mechanism
for seeing if something is a special form."

SQRT 
"number"
"[Function]"
"returns the principal square root of number."

STABLE-SORT 
"sequence predicate &key :key"
"[Function]"
"destructively sorts the elements of sequence into an order determined by predicate. Elements
considered equal by predicate stay in their original order. (This function is similar to sort, but is
guaranteed to be stable.)"

STANDARD-CHAR-P 
"char"
"[Function]"
"returns true if char is a standard character, otherwise false. char must be a character."

STEP 
"form"
"[Macro]"
"Evaluates form expression by expression, under user control. Calls to compiled functions within
form are treated as a single step if the definition was not saved. The stepping is performed in an
empty lexical environment."

STREAM 
NIL
"[Class name]"
"the class from which all other streams inherit. This is an abstract class. It should not be directly
instantiated, but instead used for the creation of new subclasses."

STREAM-ELEMENT-TYPE 
"stream"
"[Function]"
"returns a type indicator, describing what types of objects may be read from or written to stream."

STREAMP 
"thing"
"[Function]"
"returns true if thing is a stream, otherwise false."

STRING 
"object"
"[Function]"
"creates a string from object and returns it. Signals an error if object cannot be transformed into a
string. string can only convert characters and symbols. (Use format to convert numbers.)"

STRING-CAPITALIZE 
"string &key :start :end"
"[Function]"
"returns a string equivalent to string except that all words in the given range have been capitalized."

STRING-DOWNCASE 
"string &key :start :end"
"[Function]"
"returns a string equivalent to string except that all uppercase characters in the given range have
been converted to lowercase."

STRING-EQUAL 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"returns true if the specified portions of string1 and string2 are equal, ignoring case. Returns nil if
the strings are not equal. The keywords :start and :end allow comparison of substrings."

STRING-GREATERP 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"compares the specified portions of string1 and string2, using ASCII alphabetical order and ignoring
case. Returns true if the specified portion of string1 is greater than the specified portion of string2,
otherwise returns nil. If the result is true, it will be an integer index into string1 indicating the
first different character. The keywords :start and :end allow comparison of substrings."

STRING-LEFT-TRIM 
"char-bag string"
"[Function]"
"returns a substring of string with all characters in char-bag removed from the start."

STRING-LESSP 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"compares the specified portions of string1 and string2, using ASCII alphabetical order and ignoring
case. Returns true if the portion of string1 is less than the portion of string2, otherwise returns nil.
If the result is true, it will be an integer index into string1 indicating the first different character.
The keywords :start and :end allow comparison of substrings."

STRING-NOT-EQUAL 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"returns true of the specified portions of string1 and string2 are not equal. Character case is ignored.
If the strings are equal, nil is returned. If a true result is returned, it is the index into string1 of the
first non-matching character."

STRING-NOT-GREATERP 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"returns true of the specified portion of string1 is not greater than the specified portion of string2.
Character case is ignored."

STRING-NOT-LESSP 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"returns true of the specified portion of string1 is not less than the specified portion of string2.
Character case is ignored."

STRING-RIGHT-TRIM 
"char-bag string"
"[Function]"
"returns a substring of string with all characters in char-bag removed from the end."

STRING-TRIM 
"char-bag string"
"[Function]"
"returns a substring of string with all characters in char-bag removed from the beginning and end."

STRING-UPCASE 
"string &key :start :end"
"[Function]"
"returns a string equivalent to string except that all lowercase characters in the given range have
been converted to uppercase."

STRING/= 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"returns true of the specified portions of string1 and string2 are not equal. Character case is
significant. If the strings are equal, nil is returned. If a true result is returned, it is the index into
string1 of the first non-matching character."

STRING< 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"compares the specified portions of string1 and string2, using ASCII alphabetical order and treating
case as significant. Returns true if the portion of string1 is less than the portion of string2,
otherwise returns nil. If the result is true, it will be an integer index into string1 indicating the
first different character. The keywords :start and :end allow comparison of substrings."

STRING<= 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"returns true of the specified portion of string1 is less than or equal to the specified portion of
string2. Character case is significant."

STRING= 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"returns true if the specified portions of string1 and string2 are equal, treating case as significant.
The keywords :start and :end allow comparison of substrings."

STRING> 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"compares the specified portions of string1 and string2, using ASCII alphabetical order and treating
case as significant. Returns true if the portion of string1 is greater than the portion of string2,
otherwise returns nil. If the result is true it will be an integer index into string1 indicating the first
different character. The keywords :start and :end allow comparison of substrings."

STRING>= 
"string1 string2 &key :start1 :end1 :start2 :end2"
"[Function]"
"returns true of the specified portion of string1 is greater than or equal to the specified portion of
string2. Character case is significant."

STRINGP 
"object"
"[Function]"
"returns true if object is a string; otherwise, returns false."

SUBLIS 
"a-list tree &key :test :test-not :key"
"[Function]"
"creates a new tree based on tree, except that elements that appear as keys in a-list are replaced with
the corresponding value from a-list. The original tree is not modified, but the new tree may share
list structure with it. In effect, sublis can perform several subst operations simultaneously."

SUBSEQ 
"sequence start &optional end"
"[Function]"
"returns a new sequence which contains the elements of the portion of sequence specified by start and
end. subseq may be used with setf to destructively replace a portion of a sequence."

SUBSETP 
"list1 list2 &key :test :test-not :key"
"[Function]"
"returns true if every element of list1 matches some element of list2."

SUBST 
"new old tree &key :test :test-not :key"
"[Function]"
"creates a new tree based on tree, except that occurrences of old have been replaced with new. The
original tree is not modified, but the new tree may share list structure with it."

SUBST-IF 
"new test tree &key :key"
"[Function]"
"creates a new tree based on tree, except that elements that satisfy test have been replaced with new.
The original tree is not modified, but the new tree may share list structure with it."

SUBST-IF-NOT 
"new test tree &key :key"
"[Function]"
"creates a new tree based on tree, except that elements that don't satisfy test have been replaced with
new. The original tree is not modified, but the new tree may share list structure with it."

SUBSTITUTE 
"new-item old-item sequence &key :start :end :from-end :count :test :test-not :key"
"[Function]"
"returns a new sequence equivalent to sequence except that occurrences of old-item within a given
subsequence are replaced with new-item. The :count argument, if given, limits the number of
substitutions which take place. The original sequence is not modified."

SUBSTITUTE-IF 
"new-item test sequence &key :start :end :from-end :count :key"
"[Function]"
"returns a sequence equivalent to sequence except that elements that satisfy test within the given
subsequence are replaced with new-item. The :count argument, if given, limits the number of
substitutions that take place. The original sequence is not modified."

SUBSTITUTE-IF-NOT 
"new-item test sequence &key :start :end :from-end :count :key"
"[Function]"
"returns a sequence equivalent to sequence except that elements that do not satisfy test within the
given subsequence are replaced with new-item. The :count argument, if given, limits the number of
substitutions that take place. The original sequence is not modified."

SUBTYPEP 
"type-1 type-2"
"[Function]"
"returns true if type-1 is definitely a subtype of type-2. If the result is nil, however, type1 may or
may not be a subtype of type2. A second value is returned indicating the certainty of the result (t
indicates that type1 definitely is or is not a subtype of type2; nil indicates that the result is
uncertain). subtypep is not permitted to return a second value of nil unless one or both of its
arguments involve satisfies, and, or, not, or member. When one or both of its arguments involve
values or the list form of the function type specifier, subtypep returns an error."

SVREF 
"simple-vector index"
"[Function]"
"returns the element of simple-vector indicated by index."

SXHASH 
"thing"
"[Function]"
"computes a hash code for thing and returns it as a non-negative fixnum."

SYMBOL-FUNCTION 
"symbol"
"[Function]"
"returns the current global function definition named bysymbol. If the symbol has no function
binding, symbol-function signals an error."

SYMBOL-NAME 
"symbol"
"[Function]"
"returns the print name of symbol as a string."

SYMBOL-PACKAGE 
"symbol"
"[Function]"
"returns the home package of symbol."

SYMBOL-PLIST 
"symbol"
"[Function]"
"returns the property list of symbol."

SYMBOL-VALUE 
"symbol"
"[Function]"
"returns the current value of the special variable named by symbol. An error is signalled if symbol is
unbound."

SYMBOLP 
"object"
"[Function]"
"returns true if object is a symbol; otherwise returns false."

T  
NIL "[Constant]"
"the general truth value in Common Lisp. t is a constant, a class, a stream, and a type."

TAGBODY 
"{tag | statement}*"
"[Special Form]"
"A tagbody consists of a mixture of tags and forms. The tags indicate positions in the tagbody. During
execution, the statements are evaluated sequentially, except that (go tag ) may redirect execution to
the position of any tag. If the end of the body is reached, tagbody returns nil."

TAILP 
"sublist list"
"[Function]"
"returns true if and only if there exists an integer n such that (eql sublist (nthcdr n list)). list may
be a dotted list."

TAN 
"radians"
"[Function]"
"returns the tangent of radians, a number in radians."

TANH 
"radians"
"[Function]"
"returns the hyperbolic tangent of radians, a number in radians."

TENTH 
"list"
"[Function]"
"returns the tenth element of list, using one-based addressing."

TERPRI 
"&optional output-stream"
"[Function]"
"writes a newline character to output-stream and returns nil."

THE 
"type form"
"[Special Form]"
"instructs the compiler that form is of type type. This information can be used by the compiler for
performing optimizations."

THIRD 
"list"
"[Function]"
"returns the third element (caddr) of list, using one-based addressing."

THROW 
"tag result"
"[Special Form]"
"causes the dynamically active catch named tag to immediately return the value of result. This
involves exiting any processes begun from the point of the catch."

TIME 
"form"
"[Macro]"
"Executes form, prints the amount of time used in execution (with a special note on garbage collection
time, if any), and returns the value returned by form. time is useful for testing and optimizing code."

TRACE 
"{symbol | (symbol {option [modifier] }*) }*"
"[Macro]"
"causes the function named by symbol to be traced. Whenever the function is called, information can
be printed or other options can be performed. modifier specifies that actions can be performed
:before or :after the function is traced; the modifier :step specifies that the function is stepped when
it is run. Functions that are compiled in-line cannot be traced."

TREE-EQUAL 
"x y &key :test :test-not"
"[Function]"
"returns true of x and y are equivalent trees, that is, if they have the same shape and the leaves are
equal. It is true for atoms if they are equal according to the test function (by default eql), and it is
true for conses if both the car and cdr are tree-equal."

TRUENAME 
"pathname"
"[Function]"
"returns the true name of pathname. This is the name of the pathname as it is actually represented by
the file system. An error is signalled if pathname does not indicate an actual file or directory."

TRUNCATE 
"number &optional divisor"
"[Function]"
"returns two values: the integer part of number (i.e. number with the fractional part removed), and
the fractional part. When there is a second argument, truncate divides divisor into number first, and
then applies truncate to the result."

TYPE-OF 
"thing"
"[Function]"
"returns a type of which thing is a member. Various constraints are now placed on type-of; see
Common Lisp: The Language, p. 65-67, for clarification."

TYPECASE 
"keyform {(type {form}* )}*"
"[Macro]"
"evaluates keyform, then evaluates as an implicit progn the forms whose type matches the value of
keyform. Returns the last form evaluated. keyform is evaluated, but the type is not. typecase permits
a final type, otherwise or t, that handles all types not otherwise covered."

TYPEP 
"thing type"
"[Function]"
"returns true if thing is of type type; otherwise returns nil."

UNEXPORT 
"symbols &optional package"
"[Function]"
"makes symbols (which should be a symbol or list of symbols) become internal symbols in package
(which defaults to *package*), and returns t. It is an error to unexport symbols from the keyword
package."

UNINTERN 
"symbol &optional package"
"[Function]"
"deletes symbol from the package package. unintern returns true if it removes symbol and nil if the
symbol was not interned in the first place."

UNION 
"list1 list2 &key :test :test-not :key"
"[Function]"
"returns a list containing the union of the elements list1 and list2. Any element that is contained in
list1 or list2 will be contained in the result list. If there are duplicate entries, only one will appear
in the result. list1 and list2 are not modified."

UNLESS 
"testform {thenform}*"
"[Macro]"
"evaluates testform. If the result is non-nil, then no thenforms are evaluated and unless returns nil.
If the result is nil, evaluates thenforms as an implicit progn, sequentially from left to right, and
returns the value of the last thenform."

UNREAD-CHAR 
"character &optional input-stream"
"[Function]"
"puts character , the most recently read charadcter, back onto the front of input-stream so that it
will be read again as the next input character. Returns nil."

UNTRACE 
"{symbol}*"
"[Macro]"
"stops each function named by symbol from being traced. Notices will not be printed when the function
enters or returns."

UNUSE-PACKAGE 
"packages-to-unuse &optional package-unusing"
"[Function]"
"removes packages-to-unuse to the use-list of package-unusing. packages-to-unuse should be a
package, package-name, or list of packages and package-names. package-unusing may be a package
name or package object."

UNWIND-PROTECT 
"protected-form {cleanup-form}*"
"[Special Form]"
"executes protected-form and the cleanup-forms. The cleanup-forms are guaranteed to be executed,
even if there is a non-local exit during the execution of body-forms. unwind-protect returns the
value of protected-form if the exit is normal."

UPPER-CASE-P 
"char"
"[Function]"
"returns true if char is an uppercase character; otherwise returns false. char must be a character."

USE-PACKAGE 
"packages-to-use &optional package-using"
"[Function]"
"adds packages-to-use to the use-list of package-using. The external symbols of packages-to-use will
be directly accessible in package-using. packages-to-use should be a package, package-name, or list
of packages and package-names. package-using may be a package name or package object."

#|
USER-HOMEDIR-PATHNAME 
"&optional host"
"[Function]"
"returns the user’s home directory. This is the expanded form of the \"home:\" logical host. When
Macintosh Common Lisp files run on the Macintosh, the host argument is ignored."
|#

VALUES 
"&rest things"
"[Function]"
"returns things in order as multiple values."

VALUES-LIST 
"list"
"[Function]"
"returns the elements of list as multiple values."

VECTOR 
"&rest objects"
"[Function]"
"creates a simple vector (a non-adjustable one-dimensional array without a fill-pointer) whose
elements are objects."

VECTOR-POP 
"vector"
"[Function]"
"returns the element of vector indicated by vector’s fill pointer and decrements the fill pointer.
vector must be a one-dimensional array that has a fill pointer."

VECTOR-PUSH 
"new-element vector"
"[Function]"
"stores new-element in vector at the location indicated by vector’s fill pointer and increments the fill
pointer by one. Returns the previous value of the fill pointer, or nil if the fill pointer does not
designate an element of vector. vector must be a one-dimensional array that has a fill pointer."

VECTOR-PUSH-EXTEND 
"new-element vector &optional extension"
"[Function]"
"stores new-element in vector at the location indicated by the fill pointer. If the fill pointer is already
at the end of vector, vector-push-extend increases the size of vector by an amount given by
extension. This function is equivalent to vector-push except that it increases the size of the vector if
it is already full (provided the vector is adjustable). vector must be a one-dimensional array that
has a fill pointer."

VECTORP 
"object"
"[Function]"
"returns true if object is a vector; otherwise returns false. A vector is a one-dimensional array."

WARN 
"datum &rest args"
"[Function]"
"warns about a situation by signaling a condition of type warning."

WHEN 
"testform {thenform}*"
"[Macro]"
"evaluates testform. If the result is nil, returns nil without evaluating any thenform. If the result is
true, evaluates thenforms as an implicit progn, sequentially from left to right, and returns the value
of the last thenform."

WITH-INPUT-FROM-STRING 
"(var string {keyword value}*) {declaration}* {form}*"
"[Macro]"
"executes forms with var bound to an input stream that reads characters from string and returns the
results from the last form of the body. The keyword options are :index, :start, and :end. The stream
created by this macro is always of type string-stream."

WITH-OPEN-FILE 
"(stream filename {option}*) {declaration}* {form}*"
"[Macro]"
"evaluates the forms with a stream stream that reads or writes to filename, and returns the value of
the last form. The file is closed when the body of the with-open-file exits, even if the exit is through
an error, throw, or return."

WITH-OPEN-STREAM 
"(variable stream) {declaration}* {form}*"
"[Macro]"
"evaluates the forms with stream open and bound to variable. The stream is guaranteed to be closed
when with-open-stream exits, even if the exit is abnormal (as through a throw). The stream created
by with-open-stream is always of type file-stream."

WITH-OUTPUT-TO-STRING 
"(var [string [:element-type type]]) {declaration}* {form}*"
"[Macro]"
"executes forms with var bound to a string output stream. If string is supplied, it must be adjustable
and have a fill-pointer. The value of the last form is returned. If nil is supplied instead of string, the
:element-type keyword may be used to specify what characters must be accepted by the created
stream. The stream created by this macro is always of type string-stream."

WITH-PACKAGE-ITERATOR 
"(mname package-list {symbol-type}*) {form}*"
"[Macro]"
"mname is defined as if by macrolet with forms as its lexical scope, such that each invocation of
(mname) returns a symbol. Successive invocations eventually deliver all the symbols matching
symbol-types from the packages that are the elements of package-list, which is evaluated exactly
once. Each invocation of mname returns nil if there are no more symbols to be processed in the
current package. If symbols remain to be processed, the function returns four values: t, the symbol,
a keyword indicating its accessibility, and the package from which it was accessed."

WRITE 
"object &key :stream :escape :radix :base :circle :pretty :level :length :case :gensym :array
:readably :right-margin :miser-width :lines :pprint-dispatch :simple-bit-vector :simple-vector
:string-length :structure"
"[Function]"
"writes the printed representation of object to the stream specified by :stream and returns object. The
other keyword arguments specify values used to control the printed representation; each defaults to
the value of its corresponding global variable."

WRITE-BYTE 
"integer binary-output-stream"
"[Function]"
"writes one byte, the value of integer, to binary-output-stream. It is an error if integer is not of the
type specified as the :element-type argument for binary-output-stream."

WRITE-CHAR 
"character &optional output-stream"
"[Function]"
"writes character to output-stream, and returns character."

WRITE-LINE 
"string &optional output-stream &key :start :end"
"[Function]"
"sends the specified portion of string to output-stream (which defaults to *standard-output*),
followed by a newline, and returns string."

WRITE-STRING 
"string &optional stream &key :start :end"
"[Function]"
"sends the specified portion of string to stream (which defaults to *standard-output*), and returns
string."

WRITE-TO-STRING 
"thing &key :escape :radix :base :circle :pretty :level :length :case :gensym
:array :readably :right-margin :miser-width :lines :pprint-dispatch :simple-bit-vector
:simple-vector :string-length :structure"
"[Function]"
"thing is printed, as if by write, but the output is collected in a string, which is returned."

Y-OR-N-P 
"&optional format-string &rest format-args"
"[Function]"
"prints a message from format-string and format-args, followed by (y or n), and waits for the user
to type y or n. Returns T if the user typed y, or nil if the user typed n."

YES-OR-NO-P 
"&optional format-string &rest args"
"[Function]"
"prints a message from format-string and args, followed by (yes or no), and waits for the user to
type yes or no followed by a carriage return. Returns t if the user typed yes, nil if the user typed no."

ZEROP 
"number"
"[Function]"
"returns true if number is zero (either the integer zero, a floating-point zero, or a complex zero);
otherwise returns nil. (zerop -0.0) is always true."
