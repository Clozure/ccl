@make[Manual] @comment{-*- Dictionary: /afs/cs/project/clisp/docs/hem/hem; Mode: spell; Package: Hemlock; Log: /usr/lisp/scribe/hem/hem-docs.log -*-}
@Device[postscript]
@style(FontFamily = TimesRoman)
@Style(Spacing = 1.2 lines)
@Style(StringMax = 5000)
@style(Hyphenation = On)
@style(Date="March 1952")
@use(database "/afs/cs/project/clisp/docs/database/")
@Style [DoubleSided]
@Libraryfile[ArpaCredit]
@Libraryfile[Hem]
@Libraryfile[Spice]
@Libraryfile[Uttir]

@String(ReportTitle "Hemlock Command Implementor's Manual")

@comment<
@begin[TitlePage]
@begin[TitleBox]
>
@blankspace(1.3inches)
@heading[Hemlock Command Implementor's Manual]

@center[
@b<Bill Chiles>
@b<Rob MacLachlan>

@b<@value[date]>

@b<CMU-CS-89-134-R1>
]
@comment<@end[TitleBox]>
@blankspace(2lines)
@begin[Center]
School of Computer Science
Carnegie Mellon University
Pittsburgh, PA 15213
@end[Center]

@blankspace(2lines)
@begin[Center]
This is a revised version of Technical Report CMU-CS-87-159.
@end[Center]
@heading[Abstract]
@begin(Text, indent 0)
This document describes how to write commands for the @Hemlock text editor, as
of version M3.2.  @Hemlock is a customizable, extensible text editor whose
initial command set closely resembles that of ITS/TOPS-20 @Emacs.  @Hemlock is
written in the CMU Common Lisp and has been ported to other implementations.
@end(Text)

@blankspace(0.5in)
@begin[ResearchCredit]
@arpacredit[Contract=Basic87-90]
@end[ResearchCredit]
@comment<@end[TitlePage]>


@commandstring(dash = "@Y[M]")


@Tabclear

@chapter(Introduction)

 @hemlock is a text editor which follows in the tradition of editors
such as EMACS and the Lisp Machine editor ZWEI.  In its basic form,
@hemlock has almost the same command set as EMACS, and similar
features such as multiple buffers and windows, extended commands,
and built in documentation.

Both user extensions and the original commands are written in Lisp,
therefore a command implementor will have a working knowledge of this
language.  Users not familiar with Lisp need not despair however.  Many
users of Multics EMACS, another text editor written in Lisp, came to learn
Lisp simply for the purpose of writing their own editor extensions, and
found, to their surprise, that it was really pretty easy to write simple
commands.

This document describes the Common Lisp functions, macros and data structures
that are used to implement new commands.  The basic editor consists of a set of
Lisp utility functions for manipulating buffers and the other data structures
of the editor as well as handling the display.  All user level commands are
written in terms of these functions.  To find out how to define commands see
chapter @ref[commands].

@chapter(Representation of Text)
@index (Lines)
@section(Lines)
In @hemlock all text is in some @i[line].  Text is broken into lines wherever
it contains a newline character; newline characters are never stored, but are
assumed to exist between every pair of lines.  The implicit newline character
is treated as a single character by the text primitives.

@defun[fun {linep}, args {@i[line]}]
This function returns @true if @i[line] is a @f[line] object, otherwise @nil.
@enddefun

@defun[fun {line-string}, args {@i[line]}]
Given a @i(line), this function returns as a simple string the characters in
the line.  This is @f[setf]'able to set the @f[line-string] to any string that
does not contain newline characters.  It is an error to destructively modify
the result of @f[line-string] or to destructively modify any string after the
@f[line-string] of some line has been set to that string.
@enddefun

@defun[fun {line-previous}, args {@i[line]}]
@defun1[fun {line-next}, args {@i[line]}]
Given a @i(line), @f[line-previous] returns the previous line or @nil if there
is no previous line.  Similarly, @f[line-next] returns the line following
@i[line] or @nil.
@enddefun

@defun[fun {line-buffer}, args {@i[line]}]
This function returns the buffer which contains this @i(line).  Since a
line may not be associated with any buffer, in which case @f[line-buffer]
returns @nil.
@enddefun

@defun[fun {line-length}, args {@i[line]}]
This function returns the number of characters in the @i(line).  This excludes
the newline character at the end.
@enddefun

@defun[fun {line-character}, args {@i[line] @i[index]}]
This function returns the character at position @i[index] within @i[line].  It
is an error for @i[index] to be greater than the length of the line or less
than zero.  If @i[index] is equal to the length of the line, this returns a
@f[#\newline] character.
@enddefun

@defun[fun {line-plist}, args {@i[line]}]
This function returns the property-list for @i[line].  @f[setf], @f[getf],
@f[putf] and @f[remf] can be used to change properties.  This is typically used
in conjunction with @f[line-signature] to cache information about the line's
contents.
@enddefun

@defun[fun {line-signature}, args {@i[line]}]
This function returns an object that serves as a signature for a @i[line]'s
contents.  It is guaranteed that any modification of text on the line will
result in the signature changing so that it is not @f[eql] to any previous
value.  The signature may change even when the text remains unmodified, but
this does not happen often.
@enddefun


@section(Marks)
@label[marks]
@index (Marks)
A mark indicates a specific position within the text represented by a line and
a character position within that line.  Although a mark is sometimes loosely
referred to as pointing to some character, it in fact points between
characters.  If the @f[charpos] is zero, the previous character is the newline
character separating the previous line from the mark's @f[line].  If the
charpos is equal to the number of characters in the line, the next character is
the newline character separating the current line from the next.  If the mark's
line has no previous line, a mark with @f[charpos] of zero has no previous
character; if the mark's line has no next line, a mark with @f[charpos] equal
to the length of the line has no next character.

This section discusses the very basic operations involving marks, but a lot of
@hemlock programming is built on altering some text at a mark.  For more
extended uses of marks see chapter @ref[doing-stuff].


@subsection(Kinds of Marks)
@index (Permanent marks)
@index (Temporary marks)
A mark may have one of two lifetimes: @i[temporary] or @i[permanent].
Permanent marks remain valid after arbitrary operations on the text; temporary
marks do not.  Temporary marks are used because less bookkeeping overhead is
involved in their creation and use.  If a temporary mark is used after the text
it points to has been modified results will be unpredictable.  Permanent marks
continue to point between the same two characters regardless of insertions and
deletions made before or after them.

There are two different kinds of permanent marks which differ only in their
behavior when text is inserted @i(at the position of the mark); text is
inserted to the left of a @i[left-inserting] mark and to the right of
@i[right-inserting] mark.


@subsection(Mark Functions)
@defun[fun {markp}, args {@i[mark]}]
This function returns @true if @i[mark] is a @f[mark] object, otherwise @nil.
@enddefun

@defun[fun {mark-line}, args {@i[mark]}]
This function returns the line to which @i(mark) points.
@enddefun

@defun[fun {mark-charpos}, args {@i[mark]}]
This function returns the character position of the character after @i(mark).
If @i[mark]'s line has no next line, this returns the length of the line as
usual; however, there is actually is no character after the mark.
@enddefun

@defun[fun {mark-kind}, args {@i[mark]}]
This function returns one of @kwd[right-inserting], @kwd[left-inserting] or
@kwd[temporary] depending on the mark's kind.  A corresponding @f[setf] form
changes the mark's kind.
@enddefun

@defun[fun {previous-character}, args {@i[mark]}]
@defun1[fun {next-character}, args {@i[mark]}]
This function returns the character immediately before (after) the position of
the @i[mark], or @nil if there is no previous (next) character.  These
characters may be set with @f[setf] when they exist; the @f[setf] methods for
these forms signal errors when there is no previous or next character.
@enddefun


@subsection(Making Marks)
@defun[fun {mark}, args {@i[line] @i[charpos] @optional @i[kind]}]
This function returns a mark object that points to the @i(charpos)'th character
of the @i(line).  @i(Kind) is the kind of mark to create, one of
@kwd[temporary], @kwd[left-inserting], or @kwd[right-inserting].  The default
is @kwd[temporary].
@enddefun

@defun[fun {copy-mark}, args {@i[mark] @optional @i[kind]}]
This function returns a new mark pointing to the same position and of the same
kind, or of kind @i[kind] if it is supplied.
@enddefun

@defun[fun {delete-mark}, args {@i[mark]}]
This function deletes @i(mark).  Delete any permanent marks when you are
finished using it.
@enddefun

@Defmac[Fun {with-mark}, Args 
        {(@Mstar<(@i[mark] @i[pos] @mopt[@i(kind)])>) @Mstar<@i[form]>}]
 This macro binds to each variable @i[mark] a mark of kind @i[kind], which
defaults to @kwd[temporary], pointing to the same position as the mark @i[pos].
On exit from the scope the mark is deleted.  The value of the last @i[form] is
the value returned.
@enddefmac


@subsection(Moving Marks)
@index(Moving marks)
These functions destructively modify marks to point to new positions.  Other
sections of this document describe mark moving routines specific to higher
level text forms than characters and lines, such as words, sentences,
paragraphs, Lisp forms, etc.

@defun[fun {move-to-position}, args {@i[mark] @i[charpos] @optional @i[line]}]
This function changes the @i(mark) to point to the given character position on
the line @i(line).  @i(Line) defaults to @i[mark]'s line.
@enddefun

@defun[fun {move-mark}, args {@i[mark] @i[new-position]}]
This function moves @i[mark] to the same position as the mark @i[new-position]
and returns it.
@enddefun

@defun[fun {line-start}, args {@i[mark] @optional @i[line]}]
@defun1[fun {line-end}, args {@i[mark] @optional @i[line]}]
This function changes @i[mark] to point to the beginning or the end of @i(line)
and returns it.  @i[Line] defaults to @i[mark]'s line.
@enddefun

@defun[fun {buffer-start}, args {@i[mark] @optional @i[buffer]}]
@defun1[fun {buffer-end}, args {@i[mark] @optional @i[buffer]}]
These functions change @i[mark] to point to the beginning or end of @i[buffer],
which defaults to the buffer @i[mark] currently points into.  If @i[buffer] is
unsupplied, then it is an error for @i[mark] to be disassociated from any
buffer.
@enddefun

@defun[fun {mark-before}, args {@i[mark]}]
@defun1[fun {mark-after}, args {@i[mark]}]
These functions change @i[mark] to point one character before or after the
current position.  If there is no character before/after the current position,
then they return @nil and leave @i[mark] unmodified.
@enddefun

@defun[fun {character-offset}, args {@i[mark] @i[n]}]
This function changes @i[mark] to point @i[n] characters after (@i[n] before if
@i[n] is negative) the current position.  If there are less than @i[n]
characters after (before) the @i[mark], then this returns @nil and @i[mark] is
unmodified.
@enddefun

@defun[fun {line-offset}, args {@i[mark] @i[n] @optional @i[charpos]}]
This function changes @i[mark] to point @i[n] lines after (@i[n] before if
@i[n] is negative) the current position.  The character position of the
resulting mark is
@lisp
(min (line-length @i(resulting-line)) (mark-charpos @i(mark)))
@endlisp
if @i[charpos] is unspecified, or
@lisp
(min (line-length @i(resulting-line)) @i(charpos))
@endlisp
if it is.  As with @funref(character-offset), if there are not @i[n] lines then
@nil is returned and @i[mark] is not modified.
@enddefun


@section(Regions)
@index (Regions)
A region is simply a pair of marks: a starting mark and an ending mark.
The text in a region consists of the characters following the starting
mark and preceding the ending mark (keep in mind that a mark points between
characters on a line, not at them).

By modifying the starting or ending mark in a region it is possible to
produce regions with a start and end which are out of order or even in
different buffers.  The use of such regions is undefined and may
result in arbitrarily bad behavior.


@subsection(Region Functions)
@defun[fun {region}, args {@i[start] @i[end]}]
This function returns a region constructed from the marks @i[start] and
@i[end].  It is an error for the marks to point to non-contiguous lines or for
@i(start) to come after @i(end).
@enddefun

@defun[fun {regionp}, args {@i[region]}]
This function returns @true if @i[region] is a @f[region] object, otherwise
@nil.
@enddefun

@defun[fun {make-empty-region}]
This function returns a region with start and end marks pointing to the start
of one empty line.  The start mark is a @kwd[right-inserting] mark, and the end
is a @kwd[left-inserting] mark.
@enddefun

@defun[fun {copy-region}, args {@i[region]}]
This function returns a region containing a copy of the text in the specified
@i[region].  The resulting region is completely disjoint from @i[region] with
respect to data references @dash marks, lines, text, etc.
@enddefun

@defun[fun {region-to-string}, args {@i[region]}]
@defun1[fun {string-to-region}, args {@i[string]}]
These functions coerce regions to Lisp strings and vice versa.  Within the
string, lines are delimited by newline characters.
@enddefun

@defun[fun {line-to-region}, args {@i[line]}]
This function returns a region containing all the characters on @i[line].  The
first mark is @kwd[right-inserting] and the last is @kwd[left-inserting].
@enddefun

@defun[fun {region-start}, args {@i[region]}]
@defun1[fun {region-end}, args {@i[region]}]
This function returns the start or end mark of @i(region).
@enddefun

@defun[fun {region-bounds}, args {@i[region]}]
This function returns as multiple-values the starting and ending marks of
@i[region].
@enddefun

@defun[fun {set-region-bounds}, args {@i[region] @i[start] @i[end]}]
This function sets the start and end of region to @i[start] and @i[end].  It is
an error for @i[start] to be after or in a different buffer from @i[end].
@enddefun

@index(Counting lines and characters)
@defun[fun {count-lines}, args {@i[region]}]
This function returns the number of lines in the @i(region), first and last
lines inclusive.  A newline is associated with the line it follows, thus a
region containing some number of non-newline characters followed by one newline
is one line, but if a newline were added at the beginning, it would be two
lines.
@enddefun

@defun[fun {count-characters}, args {@i[region]}]
This function returns the number of characters in a given @i(region).  This
counts line breaks as one character.
@enddefun

@defun[fun {check-region-query-size}, args {@i[region]}]
@defhvar1[var {Region Query Size}, val {30}]
@f[check-region-query-size] counts the lines in @i[region], and if their number
exceeds the @hid[Region Query Size] threshold, it prompts the user for
confirmation.  This should be used in commands that perform destructive
operations and are not undoable.  If the user responds negatively, then this
signals an editor-error, aborting whatever command was in progress.
@enddefun



@chapter(Buffers)
@index (Buffers)
@label[buffers]
A buffer is an environment within @hemlock consisting of:
@begin(enumerate)
A name.

A piece of text.

A current focus of attention, the point.

An associated file (optional).

A write protect flag.

Some variables (page @pageref[variables]).

Some key bindings (page @pageref[key-bindings]).

Some collection of modes (page @pageref[modes]).

Some windows in which it is displayed (page @pageref[windows]).

A list of modeline fields (optional).
@end(enumerate)


@section (The Current Buffer)
@index (Current buffer)
@defun[fun {current-buffer}]
@defhvar1[var {Set Buffer Hook}]
@defhvar1[var {After Set Buffer Hook}]
@f[current-buffer] returns the current buffer object.  Usually this is the
buffer that @funref[current-window] is displaying.  This value may be changed
with @f[setf], and the @f[setf] method invokes @hid[Set Buffer Hook] before the
change occurs with the new value.  After the change occurs, the method invokes
@hid[After Set Buffer Hook] with the old value.
@enddefun

@defun[fun {current-point}]
This function returns the @f[buffer-point] of the current buffer.
This is such a common idiom in commands that it is defined despite
its trivial implementation.
@enddefun

@defun[fun {current-mark}]
@defun1[fun {pop-buffer-mark}]
@defun1[fun {push-buffer-mark}, args {@i[mark] @optional @i[activate-region]}]
@index(Buffer mark stack)
@index(Mark stack)
@label(mark-stack)
@f[current-mark] returns the top of the current buffer's mark stack.  There
always is at least one mark at the beginning of the buffer's region, and all
marks returned are right-inserting.

@f[pop-buffer-mark] pops the current buffer's mark stack, returning the mark.
If the stack becomes empty, this pushes a new mark on the stack pointing to the
buffer's start.  This always deactivates the current region (see section
@ref[active-regions]).

@f[push-buffer-mark] pushes @i[mark] into the current buffer's mark stack,
ensuring that the mark is right-inserting.  If @i[mark] does not point into the
current buffer, this signals an error.  Optionally, the current region is made
active, but this never deactivates the current region (see section
@ref[active-regions]).  @i[Mark] is returned.
@enddefun

@defvar[var {buffer-list}]
This variable holds a list of all the buffer objects made with @f[make-buffer].
@enddefvar

@defvar[var {buffer-names}]
This variable holds a @f[string-table] (page @pageref(string-tables)) of all the
names of the buffers in @var[buffer-list].  The values of the entries are the
corresponding buffer objects.
@enddefvar

@defvar[var {buffer-history}]
This is a list of buffer objects ordered from those most recently selected to
those selected farthest in the past.  When someone makes a buffer, an element
of @hid[Make Buffer Hook] adds this buffer to the end of this list.  When
someone deletes a buffer, an element of @hid[Delete Buffer Hook] removes the
buffer from this list.  Each buffer occurs in this list exactly once, but it
never contains the @var[echo-area-buffer].
@enddefvar

@defun[fun {change-to-buffer}, args {@i[buffer]}]
This switches to @i[buffer] in the @f[current-window] maintaining
@f[buffer-history].
@enddefun

@defun[fun {previous-buffer}]
This returns the first buffer from @var[buffer-history] that is not the
@f[current-buffer].  If none can be found, then this returns @nil.
@enddefun


@section(Buffer Functions)
@defun[fun {make-buffer}, args {@i[name]}, keys {[modes][modeline-fields][delete-hook]}]
@defhvar1[var {Make Buffer Hook}]
@defhvar1[var {Default Modeline Fields}]
@f[make-buffer] creates and returns a buffer with the given @i(name).  If a
buffer named @i[name] already exists, @nil is returned.  @i[Modes] is a list of
modes which should be in effect in the buffer, major mode first, followed by
any minor modes.  If this is omitted then the buffer is created with the list
of modes contained in @hvarref[Default Modes].  @i[Modeline-fields] is a list
of modeline-field objects (see section @ref[modelines]) which may be @nil.
@f[delete-hook] is a list of delete hooks specific to this buffer, and
@f[delete-buffer] invokes these along with @hid[Delete Buffer Hook].

Buffers created with @f[make-buffer] are entered into the list
@var[buffer-list], and their names are inserted into the
string-table @var[buffer-names].  When a buffer is created the hook
@hid[Make Buffer Hook] is invoked with the new buffer.
@enddefun

@defun[fun {bufferp}, args {@i[buffer]}]
Returns @true if @i[buffer] is a @f[buffer] object, otherwise @nil.
@enddefun

@defun[fun {buffer-name}, args {@i[buffer]}]
@defhvar1[var {Buffer Name Hook}]
@f[buffer-name] returns the name, which is a string, of the given @i(buffer).
The corresponding @f[setf] method invokes @hid[Buffer Name Hook] with
@i[buffer] and the new name and then sets the buffer's name.  When the user
supplies a name for which a buffer already exists, the @f[setf] method signals
an error.
@enddefun

@defun[fun {buffer-region}, args {@i[buffer]}]
Returns the @i[buffer]'s region.  This can be set with @f[setf].  Note, this
returns the region that contains all the text in a buffer, not the
@funref[current-region].
@enddefun

@defun[fun {buffer-pathname}, args {@i[buffer]}]
@defhvar1[var {Buffer Pathname Hook}]
@f[buffer-pathname] returns the pathname of the file associated with
the given @i(buffer), or nil if it has no associated file.  This is
the truename of the file as of the most recent time it was read or
written.  There is a @f[setf] form to change the pathname.  When the
pathname is changed the hook @hid[Buffer Pathname Hook] is invoked
with the buffer and new value.
@enddefun

@defun[fun {buffer-write-date}, args {@i[buffer]}]
Returns the write date for the file associated with the buffer in universal
time format.  When this the @f[buffer-pathname] is set, use @f[setf] to set
this to the corresponding write date, or to @nil if the date is unknown or
there is no file.
@enddefun

@defun[fun {buffer-point}, args {@i[buffer]}]
Returns the mark which is the current location within @i[buffer].  To
move the point, use @f[move-mark] or @funref[move-to-position] rather
than setting @f[buffer-point] with @f[setf].
@enddefun

@defun[fun {buffer-mark}, args {@i[buffer]}]
@index(Buffer mark stack)
@index(Mark stack)
This function returns the top of @i[buffer]'s mark stack.  There always
is at least one mark at the beginning of @i[buffer]'s region, and all marks
returned are right-inserting.
@enddefun

@defun[fun {buffer-start-mark}, args {@i[buffer]}]
@defun1[fun {buffer-end-mark}, args {@i[buffer]}]
These functions return the start and end marks of @i[buffer]'s region:
@Begin[ProgramExample]
(buffer-start-mark buffer)  <==>
  (region-start (buffer-region buffer))
and
(buffer-end-mark buffer)  <==>
  (region-end (buffer-region buffer))
@End[ProgramExample]
@enddefun

@defun[fun {buffer-writable}, args {@i[buffer]}]
@defhvar1[var "Buffer Writable Hook"]
This function returns @true if you can modify the @i(buffer), @nil if you
cannot.  If a buffer is not writable, then any attempt to alter text in the
buffer results in an error.  There is a @f[setf] method to change this value.

The @f[setf] method invokes the functions in @hid[Buffer Writable Hook] on the
buffer and new value before storing the new value.
@enddefun

@defun[fun {buffer-modified}, args {@i[buffer]}]
@defhvar1[var "Buffer Modified Hook"]
@f[buffer-modified] returns @true if the @i[buffer] has been modified, @nil if
it hasn't.  This attribute is set whenever a text-altering operation is
performed on a buffer.  There is a @f[setf] method to change this value.

The @f[setf] method invokes the functions in @hid[Buffer Modified Hook] with
the buffer whenever the value of the modified flag changes.
@enddefun

@defmac[fun {with-writable-buffer}, args {(@i[buffer]) @rest @i[forms]}]
This macro executes @i[forms] with @i[buffer]'s writable status set.  After
@i[forms] execute, this resets the @i[buffer]'s writable and modified status.
@enddefmac

@defun[fun {buffer-signature}, args {@i[buffer]}]
This function returns an arbitrary number which reflects the buffer's current
@i[signature].  The result is @f[eql] to a previous result if and only if the
buffer has not been modified between the calls.
@enddefun

@defun[fun {buffer-variables}, args {@i[buffer]}]
This function returns a string-table (page @pageref[string-tables]) containing
the names of the buffer's local variables.  See chapter @ref[variables].
@enddefun

@defun[fun {buffer-modes}, args {@i[buffer]}]
This function returns the list of the names of the modes active in @i[buffer].
The major mode is first, followed by any minor modes.  See chapter @ref[modes].
@enddefun

@defun[fun {buffer-windows}, args {@i[buffer]}]
This function returns the list of all the windows in which the buffer may be
displayed.  This list may include windows which are not currently visible.  See
page @pageref[windows] for a discussion of windows.
@enddefun

@defun[fun {buffer-delete-hook}, args {@i[buffer]}]
This function returns the list of buffer specific functions @f[delete-buffer]
invokes when deleting a buffer.  This is @f[setf]'able.
@enddefun

@defun[fun {delete-buffer}, args {@i[buffer]}]
@defhvar1[var {Delete Buffer Hook}]
@f[delete-buffer] removes @i[buffer] from @varref[buffer-list] and its name
from @varref[buffer-names].  Before @i[buffer] is deleted, this invokes the
functions on @i[buffer] returned by @f[buffer-delete-hook] and those found in
@hid[Delete Buffer Hook].  If @i[buffer] is the @f[current-buffer], or if it is
displayed in any windows, then this function signals an error.
@enddefun

@defun[fun {delete-buffer-if-possible}, args {@i[buffer]}]
This uses @f[delete-buffer] to delete @i[buffer] if at all possible.  If
@i[buffer] is the @f[current-buffer], then this sets the @f[current-buffer] to
the first distinct buffer in @f[buffer-history].  If @i[buffer] is displayed in
any windows, then this makes each window display the same distinct buffer.
@enddefun


@section(Modelines)
@index(Modelines)
@label(modelines)

A Buffer may specify a modeline, a line of text which is displayed across the
bottom of a window to indicate status information.  Modelines are described as
a list of @f[modeline-field] objects which have individual update functions and
are optionally fixed-width.  These have an @f[eql] name for convenience in
referencing and updating, but the name must be unique for all created
modeline-field objects.  When creating a modeline-field with a specified width,
the result of the update function is either truncated or padded on the right to
meet the constraint.  All modeline-field functions must return simple strings
with standard characters, and these take a buffer and a window as arguments.
Modeline-field objects are typically shared amongst, or aliased by, different
buffers' modeline fields lists.  These lists are unique allowing fields to
behave the same wherever they occur, but different buffers may display these
fields in different arrangements.

Whenever one of the following changes occurs, all of a buffer's modeline fields
are updated:
@Begin[Itemize]
A buffer's major mode is set.

One of a buffer's minor modes is turned on or off.

A buffer is renamed.

A buffer's pathname changes.

A buffer's modified status changes.

A window's buffer is changed.
@End[Itemize]

The policy is that whenever one of these changes occurs, it is guaranteed that
the modeline will be updated before the next trip through redisplay.
Furthermore, since the system cannot know what modeline-field objects the
user has added whose update functions rely on these values, or how he has
changed @hid[Default Modeline Fields], we must update all the fields.  When any
but the last occurs, the modeline-field update function is invoked once for
each window into the buffer.  When a window's buffer changes, each
modeline-field update function is invoked once; other windows' modeline
fields should not be affected due to a given window's buffer changing.

The user should note that modelines can be updated at any time, so update
functions should be careful to avoid needless delays (for example, waiting for
a local area network to determine information).

@defun[fun {make-modeline-field}, keys {[name][width][function]}]
@defun1[fun {modeline-field-p}, args @i(modeline-field)]
@defun1[fun {modeline-field-name}, args @i(modeline-field)]
@f[make-modeline-field] returns a modeline-field object with @i[name],
@i[width], and @i[function].  @i[Width] defaults to @nil meaning that the field
is variable width; otherwise, the programmer must supply this as a positive
integer.  @i[Function] must take a buffer and window as arguments and return a
@f[simple-string] containing only standard characters.  If @i[name] already
names a modeline-field object, then this signals an error.

@f[modeline-field-name] returns the name field of a modeline-field object.  If
this is set with @f[setf], and the new name already names a modeline-field,
then the @f[setf] method signals an error.

@f[modeline-field-p] returns @true or @nil, depending on whether its argument
is a @f[modeline-field] object.
@enddefun

@defun[fun {modeline-field}, args {@i[name]}]
This returns the modeline-field object named @i[name].  If none exists, this
returns nil.
@enddefun

@defun[fun {modeline-field-function}, args {@i[modeline-field]}]
Returns the function called when updating the @i[modeline-field].  When this is
set with @f[setf], the @f[setf] method updates @i[modeline-field] for all
windows on all buffers that contain the given field, so the next trip through
redisplay will reflect the change.  All modeline-field functions must return
simple strings with standard characters, and they take a buffer and a window
as arguments.
@enddefun

@defun[fun {modeline-field-width}, args {@i[modeline-field]}]
Returns the width to which @i[modeline-field] is constrained, or @nil
indicating that it is variable width.  When this is set with @f[setf], the
@f[setf] method updates all modeline-fields for all windows on all buffers that
contain the given field, so the next trip through redisplay will reflect the
change.  All the fields for any such modeline display must be updated, which is
not the case when setting a modeline-field's function.
@enddefun

@defun[fun {buffer-modeline-fields}, args {@i[buffer]}]
Returns a copy of the list of @i[buffer]'s modeline-field objects.  This list
can be destructively modified without affecting display of @i[buffer]'s
modeline, but modifying any particular field's components (for example, width
or function) causes the changes to be reflected the next trip through redisplay
in every modeline display that uses the modified modeline-field.  When this is
set with @f[setf], @f[update-modeline-fields] is called for each window into
@i[buffer].
@enddefun

@defun[fun {buffer-modeline-field-p}, args {@i[buffer] @i[field]}]
If @i[field], a modeline-field or the name of one, is in buffer's list of
modeline-field objects, it is returned; otherwise, this returns nil.
@enddefun

@defun[fun {update-modeline-fields}, args {@i[buffer] @i[window]}]
This invokes each modeline-field object's function from @i[buffer]'s list,
passing @i[buffer] and @i[window].  The results are collected regarding each
modeline-field object's width as appropriate, and the window is marked so
the next trip through redisplay will reflect the changes.  If window does not
display modelines, then no computation occurs.
@enddefun

@defun[fun {update-modeline-field}, args {@i[buffer] @i[window] @i[field-or-name]}]
This invokes the modeline-field object's function for @i[field-or-name], which
is a modeline-field object or the name of one for @i[buffer].  This passes
@i[buffer] and @i[window] to the update function.  The result is applied to the
@i[window]'s modeline display using the modeline-field object's width, and the
window is marked so the next trip through redisplay will reflect the changes.
If the window does not display modelines, then no computation occurs.  If
@i[field-or-name] is not found in @i[buffer]'s list of modeline-field objects,
then this signals an error.  See @f[buffer-modeline-field-p] above.
@enddefun



@chapter(Altering and Searching Text)
@label[doing-stuff]

@section(Altering Text)
@index(Altering text)
@index(Inserting)
@index(Deleting)
A note on marks and text alteration: @kwd[temporary] marks are invalid
after any change has been made to the text the mark points to; it is an
error to use a temporary mark after such a change has been made.  If
text is deleted which has permanent marks pointing into it then they
are left pointing to the position where the text was.

@defun[fun {insert-character}, args {@i[mark] @i[character]}]
@defun1[fun {insert-string}, args {@i[mark] @i[string]}]
@defun1[fun {insert-region}, args {@i[mark] @i[region]}]
Inserts @i[character], @i[string] or @i[region] at @i[mark].
@f[insert-character] signals an error if @i[character] is not
@f[string-char-p].  If @i[string] or @i[region] is empty, and @i[mark] is in
some buffer, then @hemlock leaves @f[buffer-modified] of @i[mark]'s buffer
unaffected.
@enddefun

@defun[fun {ninsert-region}, args {@i[mark] @i[region]}]
Like @f[insert-region], inserts the @i[region] at the @i[mark]'s position,
destroying the source region.  This must be used with caution, since if anyone
else can refer to the source region bad things will happen.  In particular, one
should make sure the region is not linked into any existing buffer.  If
@i[region] is empty, and @i[mark] is in some buffer, then @hemlock leaves
@f[buffer-modified] of @i[mark]'s buffer unaffected.
@enddefun

@defun[fun {delete-characters}, args {@i[mark] @i[n]}]
This deletes @i[n] characters after the @i[mark] (or -@i[n] before if @i[n] is
negative).  If @i[n] characters after (or -@i[n] before) the @i[mark] do not
exist, then this returns @nil; otherwise, it returns @true.  If @i[n] is zero,
and @i[mark] is in some buffer, then @hemlock leaves @f[buffer-modified] of
@i[mark]'s buffer unaffected.
@enddefun

@defun[fun {delete-region}, args {@i[region]}]
This deletes @i[region].  This is faster than @f[delete-and-save-region]
(below) because no lines are copied.  If @i[region] is empty and contained in
some buffer's @f[buffer-region], then @hemlock leaves @f[buffer-modified] of
the buffer unaffected.
@enddefun

@defun[fun {delete-and-save-region}, args {@i[region]}]
This deletes @i[region] and returns a region containing the original
@i[region]'s text.  If @i[region] is empty and contained in some buffer's
@f[buffer-region], then @hemlock leaves @f[buffer-modified] of the buffer
unaffected.  In this case, this returns a distinct empty region.
@enddefun

@defun[fun {filter-region}, args {@i[function] @i[region]}]
Destructively modifies @i[region] by replacing the text
of each line with the result of the application of @i[function] to a
string containing that text.  @i[Function] must obey the following
restrictions:
@begin[enumerate]
The argument may not be destructively modified.

The return value may not contain newline characters.

The return value may not be destructively modified after it is
returned from @i[function].
@end[enumerate]
The strings are passed in order, and are always simple strings.

Using this function, a region could be uppercased by doing:
@lisp
(filter-region #'string-upcase region)
@endlisp
@enddefun


@section(Text Predicates)
@defun[fun {start-line-p}, args {@i[mark]}]
Returns @true if the @i(mark) points before the first character in a line,
@nil otherwise.
@enddefun

@defun[fun {end-line-p}, args {@i[mark]}]
Returns @true if the @i(mark) points after the last character in a line and
before the newline, @nil otherwise.
@enddefun

@defun[fun {empty-line-p}, args {@i[mark]}]
Return @true of the line which @i[mark] points to contains no characters.
@enddefun

@defun[fun {blank-line-p}, args {@i[line]}]
Returns @true if @i[line] contains only characters with a
@hid[Whitespace] attribute of 1.  See chapter @ref[character-attributes] for
discussion of character attributes.
@enddefun

@defun[fun {blank-before-p}, args {@i[mark]}]
@defun1[fun {blank-after-p}, args {@i[mark]}]
These functions test if all the characters preceding or following
@i[mark] on the line it is on have a @hid[Whitespace] attribute of @f[1].
@enddefun

@defun[fun {same-line-p}, args {@i[mark1] @i[mark2]}]
Returns @true if @i(mark1) and @i(mark2) point to the same line, or @nil
otherwise;  That is,
@example[(same-line-p a b) <==> (eq (mark-line a) (mark-line b))]
@enddefun

@defun[fun {mark<}, funlabel {mark-LSS}, args {@i[mark1] @i[mark2]}]
@defun1[fun {mark<=}, funlabel {mark-LEQ}, args {@i[mark1] @i[mark2]}]
@defun1[fun {mark=}, funlabel {mark-EQL}, args {@i[mark1] @i[mark2]}]
@defun1[fun {mark/=}, funlabel {mark-NEQ}, args {@i[mark1] @i[mark2]}]
@defun1[fun {mark>=}, funlabel {mark-GEQ}, args {@i[mark1] @i[mark2]}]
@defun1[fun {mark>}, funlabel {mark-GTR}, args {@i[mark1] @i[mark2]}]
These predicates test the relative ordering of two marks in a piece of
text, that is a mark is @f[mark>] another if it points to a position
after it.  If the marks point into different, non-connected pieces of
text, such as different buffers, then it is an error to test their
ordering; for such marks @f[mark=] is always false and @f[mark/=] is
always true.
@enddefun

@defun[fun {line<}, funlabel {line-LSS}, args {@i[line1] @i[line2]}]
@defun1[fun {line<=}, funlabel {line-LEQ}, args {@i[line1] @i[line2]}]
@defun1[fun {line>=}, funlabel {line-GEQ}, args {@i[line1] @i[line2]}]
@defun1[fun {line>}, funlabel {line-GTR}, args {@i[line1] @i[line2]}]
These predicates test the ordering of @i[line1] and @i[line2].  If the
lines are in unconnected pieces of text it is an error to test their
ordering.
@enddefun

@defun[fun {lines-related}, args {@i[line1] @i[line2]}]
This function returns @true if @i[line1] and @i[line2] are in the same
piece of text, or @nil otherwise.
@enddefun

@defun[fun {first-line-p}, args {@i[mark]}]
@defun1[fun {last-line-p}, args {@i[mark]}]
@f[first-line-p] returns @true if there is no line before the line
@i[mark] is on, and @nil otherwise.  @i[Last-line-p] similarly tests
tests whether there is no line after @i[mark].
@enddefun


@section(Kill Ring)
@index(Kill ring)
@label(kill-ring)

@defvar[var {kill-ring}]
This is a ring (see section @ref[rings]) of regions deleted from buffers.
Some commands save affected regions on the kill ring before performing
modifications.  You should consider making the command undoable (see section
@ref[undo]), but this is a simple way of achieving a less satisfactory means
for the user to recover.
@enddefvar

@defun[fun {kill-region}, args {@i[region] @i[current-type]}]
This kills @i[region] saving it in @var[kill-ring].  @i[Current-type] is either
@kwd[kill-forward] or @kwd[kill-backward].  When the @funref[last-command-type]
is one of these, this adds @i[region] to the beginning or end, respectively, of
the top of @var[kill-ring].  The result of calling this is undoable using the
command @hid[Undo] (see the @i[Hemlock User's Manual]).  This sets
@f[last-command-type] to @i[current-type], and it interacts with
@f[kill-characters].
@enddefun

@defun[fun {kill-characters}, args {@i[mark] @i[count]}]
@defhvar1[var {Character Deletion Threshold}, val {5}]
@f[kill-characters] kills @i[count] characters after @i[mark] if @i[count] is
positive, otherwise before @i[mark] if @i[count] is negative.  When @i[count]
is greater than or equal to @hid[Character Deletion Threshold], the killed
characters are saved on @var[kill-ring].  This may be called multiple times
contiguously (that is, without @funref[last-command-type] being set) to
accumulate an effective count for purposes of comparison with the threshold.

This sets @f[last-command-type], and it interacts with @f[kill-region].  When
this adds a new region to @var[kill-ring], it sets @f[last-command-type] to
@kwd[kill-forward] (if @i[count] is positive) or @kwd[kill-backward] (if
@i[count] is negative).  When @f[last-command-type] is @kwd[kill-forward] or
@kwd[kill-backward], this adds the killed characters to the beginning (if
@i[count] is negative) or the end (if @i[count] is positive) of the top of
@var[kill-ring], and it sets @f[last-command-type] as if it added a new region
to @var[kill-ring].  When the kill ring is unaffected, this sets
@f[last-command-type] to @kwd[char-kill-forward] or @kwd[char-kill-backward]
depending on whether @i[count] is positive or negative, respectively.

This returns mark if it deletes characters.  If there are not @i[count]
characters in the appropriate direction, this returns nil.
@enddefun


@section(Active Regions)
@index(Active regions)
@label(active-regions)

Every buffer has a mark stack (page @pageref[mark-stack]) and a mark known as
the point where most text altering nominally occurs.  Between the top of the
mark stack, the @f[current-mark], and the @f[current-buffer]'s point, the
@f[current-point], is what is known as the @f[current-region].  Certain
commands signal errors when the user tries to operate on the @f[current-region]
without its having been activated.  If the user turns off this feature, then
the @f[current-region] is effectively always active.

When writing a command that marks a region of text, the programmer should make
sure to activate the region.  This typically occurs naturally from the
primitives that you use to mark regions, but sometimes you must explicitly
activate the region.  These commands should be written this way, so they do not
require the user to separately mark an area and then activate it.  Commands
that modify regions do not have to worry about deactivating the region since
modifying a buffer automatically deactivates the region.  Commands that insert
text often activate the region ephemerally; that is, the region is active for
the immediately following command, allowing the user wants to delete the region
inserted, fill it, or whatever.

Once a marking command makes the region active, it remains active until:
@begin[itemize]
a command uses the region,

a command modifies the buffer,

a command changes the current window or buffer,

a command signals an editor-error,

or the user types @binding[C-g].
@end[itemize]

@defhvar[var "Active Regions Enabled", val {t}]
When this variable is non-@nil, some primitives signal an editor-error if
the region is not active.  This may be set to @nil for more traditional @emacs
region semantics.
@enddefhvar

@defvar[var {ephemerally-active-command-types}]
This is a list of command types (see section @ref[command-types]), and its
initial value is the list of @kwd[ephemerally-active] and @kwd[unkill].  When
the previous command's type is one of these, the @f[current-region] is active
for the currently executing command only, regardless of whether it does
something to deactivate the region.  However, the current command may activate
the region for future commands.  @kwd[ephemerally-active] is a default command
type that may be used to ephemerally activate the region, and @kwd[unkill] is
the type used by two commands, @hid[Un-kill] and @hid[Rotate Kill Ring] (what
users typically think of as @binding[C-y] and @binding[M-y]).
@enddefvar

@defun[fun {activate-region}]
This makes the @f[current-region] active.
@enddefun

@defun[fun {deactivate-region}]
After invoking this the @f[current-region] is no longer active.
@enddefun

@defun[fun {region-active-p}]
Returns whether the @f[current-region] is active, including ephemerally.  This
ignores @hid[Active Regions Enabled].
@enddefun

@defun[fun {check-region-active}]
This signals an editor-error when active regions are enabled, and the
@f[current-region] is not active.
@enddefun

@defun[fun {current-region},
       args {@optional @i[error-if-not-active] @i[deactivate-region]}]
This returns a region formed with @f[current-mark] and @f[current-point],
optionally signaling an editor-error if the current region is not active.
@i[Error-if-not-active] defaults to @true.  Each call returns a distinct region
object.  Depending on @i[deactivate-region] (defaults to @true), fetching the
current region deactivates it.  @hemlock primitives are free to modify text
regardless of whether the region is active, so a command that checks for this
can deactivate the region whenever it is convenient.
@enddefun


@section(Searching and Replacing)
@index(Searching)
@index(Replacing)

Before using any of these functions to do a character search, look at character
attributes (page @pageref[character-attributes]).  They provide a facility
similar to the syntax table in real EMACS.  Syntax tables are a powerful,
general, and efficient mechanism for assigning meanings to characters in
various modes.

@defcon[var {search-char-code-limit}]
An exclusive upper limit for the char-code of characters given to the searching
functions.  The result of searches for characters with a char-code greater than
or equal to this limit is ill-defined, but it is @i[not] an error to do such
searches.
@enddefcon

@defun[fun {new-search-pattern},
args {@i[kind] @i[direction] @i[pattern] @optional @i[result-search-pattern]}] 

Returns a @i[search-pattern] object which can be given to the @f[find-pattern]
and @f[replace-pattern] functions.  A search-pattern is a specification of a
particular sort of search to do.  @i[direction] is either @kwd[forward] or
@kwd[backward], indicating the direction to search in.  @i[kind] specifies the
kind of search pattern to make, and @i[pattern] is a thing which specifies what
to search for.

The interpretation of @i[pattern] depends on the @i[kind] of pattern being
made.  Currently defined kinds of search pattern are:
@begin(description)
@kwd[string-insensitive]@\Does a case-insensitive string search,
@i[pattern] being the string to search for.

@kwd[string-sensitive]@\Does a case-sensitive string search for
@i[pattern].

@kwd[character]@\Finds an occurrence of the character @i[pattern].
This is case sensitive.

@kwd[not-character]@\Find a character which is not the character
@i[pattern].

@kwd[test]@\Finds a character which satisfies the function @i[pattern].
This function may not be applied an any particular fashion, so it
should depend only on what its argument is, and should have no
side-effects.

@kwd[test-not]@\Similar to as @kwd[test], except it finds a character that
fails the test.

@kwd[any]@\Finds a character that is in the string @i[pattern].

@kwd[not-any]@\Finds a character that is not in the string @i[pattern].
@end(description)

@i[result-search-pattern], if supplied, is a search-pattern to
destructively modify to produce the new pattern.  Where reasonable
this should be supplied, since some kinds of search patterns may
involve large data structures.
@enddefun

@defun[fun {search-pattern-p}, args {@i[search-pattern]}]
Returns @true if @i[search-pattern] is a @f[search-pattern] object, otherwise
@nil.
@enddefun

@defun[fun {get-search-pattern}, args {@i[string] @i[direction]}]
@defvar1[var {last-search-pattern}]
@defvar1[var {last-search-string}]
@f[get-search-pattern] interfaces to a default search string and pattern that
search and replacing commands can use.  These commands then share a default
when prompting for what to search or replace, and save on consing a search
pattern each time they execute.  This uses @hid[Default Search Kind] (see the
@i[Hemlock User's Manual]) when updating the pattern object.  This returns the
pattern, so you probably don't need to refer to @var[last-search-pattern], but
@var[last-search-string] is useful when prompting.
@enddefun

@defun[fun {find-pattern}, args {@i[mark] @i[search-pattern]}]
Find the next match of @i[search-pattern] starting at @i[mark].  If a
match is found then @i[mark] is altered to point before the matched text
and the number of characters matched is returned.  If no match is
found then @nil is returned and @i[mark] is not modified.
@enddefun

@defun[fun {replace-pattern}, args
        {@i[mark] @i[search-pattern] @i[replacement] @optional @i[n]}]
Replace @i[n] matches of @i[search-pattern] with the string
@i[replacement] starting at @i[mark].  If @i[n] is @nil (the default)
then replace all matches.  A mark pointing before the last replacement
done is returned.
@enddefun



@Chapter(The Current Environment)
@label(current-environment)
@index(Current environment)

@section(Different Scopes)
    In @hemlock the values of @i[variables] (page @pageref[variables]),
@i[key-bindings] (page @pageref(key-bindings)) and
@i[character-attributes] (page @pageref[character-attributes]) may
depend on the @funref(current-buffer) and the modes
active in it.  There are three possible scopes for
@hemlock values:
@begin(description)
@i[buffer local]@\The value is present only if the buffer it is local
to is the @f[current-buffer].

@i[mode local]@\The value is present only when the mode it is local to
is active in the @f[current-buffer].

@i[global]@\The value is always present unless shadowed by a buffer or
mode local value.
@end(description)


@section(Shadowing)
    It is possible for there to be a conflict between different values
for the same thing in different scopes.  For example, there be might a
global binding for a given variable and also a local binding in the
current buffer.  Whenever there is a conflict shadowing occurs,
permitting only one of the values to be visible in the current
environment.

    The process of resolving such a conflict can be described as a
search down a list of places where the value might be defined, returning
the first value found.  The order for the search is as follows:
@begin(enumerate)
Local values in the current buffer.

Mode local values in the minor modes of the current buffer, in order
from the highest precedence mode to the lowest precedence mode.  The
order of minor modes with equal precedences is undefined.

Mode local values in the current buffer's major mode.

Global values.
@end(enumerate)



@chapter(Hemlock Variables)
@index (Hemlock variables)
@label(variables)
@hemlock implements a system of variables separate from normal Lisp variables
for the following reasons:
@begin(enumerate)
@hemlock has different scoping rules which are useful in an editor.  @hemlock
variables can be local to a @i(buffer) (page @pageref[buffers]) or a @i(mode)
(page @pageref[modes]).

@hemlock variables have @i(hooks) (page @pageref[hooks]), lists of functions
called when someone sets the variable.  See @f[variable-value] for the
arguments @hemlock passes to these hook functions.

There is a database of variable names and documentation which makes it easier
to find out what variables exist and what their values mean.
@end(enumerate)


@section(Variable Names)
To the user, a variable name is a case insensitive string.  This
string is referred to as the @i[string name] of the variable.  A
string name is conventionally composed of words separated by spaces.

In Lisp code a variable name is a symbol.  The name of this symbol is
created by replacing any spaces in the string name with hyphens.  This
symbol name is always interned in the @hemlock package and referring
to a symbol with the same name in the wrong package is an error.

@defvar[var {global-variable-names}]
This variable holds a string-table of the names of all the global @hemlock
variables.  The value of each entry is the symbol name of the variable.
@enddefvar

@defun[fun {current-variable-tables}]
This function returns a list of variable tables currently established,
globally, in the @f[current-buffer], and by the modes of the
@f[current-buffer].  This list is suitable for use with
@f[prompt-for-variable].
@enddefun


@section(Variable Functions)
In the following descriptions @i[name] is the symbol name of the variable.

@defun[fun {defhvar}, args {@i[string-name] @i[documentation]},
	keys {[mode][buffer][hooks][value]}]
 This function defines a @hemlock variable.  Functions that take a variable
name signal an error when the variable is undefined.
@begin(description)
@i[string-name]@\The string name of the variable to define.

@i[documentation]@\The documentation string for the variable.

@multiple{
@kwd[mode],
@kwd[buffer]}@\
 If @i[buffer] is supplied, the variable is local to that buffer.  If @i[mode]
is supplied, it is local to that mode.  If neither is supplied, it is global.

@kwd[value]@\
 This is the initial value for the variable, which defaults to @nil.

@kwd[hooks]@\
 This is the initial list of functions to call when someone sets the variable's
value.  These functions execute before @hemlock establishes the new value.  See
@f[variable-value] for the arguments passed to the hook functions.
@end(description)
If a variable with the same name already exists in the same place, then
@f[defhvar] sets its hooks and value from @i[hooks] and @i[value] if the user
supplies these keywords.
@enddefun

@defun[fun {variable-value}, args {@i[name] @optional @i[kind] @i[where]}]
This function returns the value of a @hemlock variable in some place.
The following values for @i[kind] are defined:
@begin[description]
@kwd[current]@\
 Return the value present in the current environment, taking into consideration
any mode or buffer local variables.  This is the default.

@kwd[global]@\
 Return the global value.

@kwd[mode]@\
 Return the value in the mode named @i[where].

@kwd[buffer]@\
 Return the value in the buffer @i[where].
@end[description]
When set with @f[setf], @hemlock sets the value of the specified variable and
invokes the functions in its hook list with @i[name], @i[kind], @i[where], and
the new value.
@enddefun

@defun[fun {variable-documentation}, args
	{@i[name] @optional @i[kind] @i[where]}] 
@defun1[fun {variable-hooks}, args
        {@i[name] @optional @i[kind] @i[where]}]
@defun1[fun {variable-name}, args
	{@i[name] @optional @i[kind] @i[where]}]
These function return the documentation, hooks and string name of a
@hemlock variable.  The @i[kind] and @i[where] arguments are the same
as for @f[variable-value].  The documentation and hook list may be set
using @f[setf].
@enddefun

@defun[fun {string-to-variable}, args {@i[string]}]
This function converts a string into the corresponding variable symbol
name.  @i[String] need not be the name of an actual @hemlock variable.
@enddefun

@defmac[fun {value}, args {@i[name]}] 
@defmac1[fun {setv}, args {@i[name] @i[new-value]}]
These macros get and set the current value of the @hemlock variable
@i[name].  @i[Name] is not evaluated.  There is a @f[setf] form for
@f[value].
@enddefmac

@Defmac[Fun {hlet}, Args {(@Mstar<(@i[var] @i[value])>) @Mstar<@i[form]>}]
This macro is very similar to @f[let] in effect; within its scope each
of the @hemlock variables @i[var] have the respective @i[value]s, but
after the scope is exited by any means the binding is removed.  This
does not cause any hooks to be invoked.  The value of the last
@i[form] is returned.
@enddefmac

@defun[fun {hemlock-bound-p}, args {@i[name] @optional @i[kind] @i[where]}]
Returns @true if @i[name] is defined as a @hemlock variable in the
place specified by @i[kind] and @i[where], or @nil otherwise.
@enddefun

@defun[fun {delete-variable}, args {@i(name) @optional @i[kind] @i[where]}]
@defhvar1[var {Delete Variable Hook}]
@f[delete-variable] makes the @hemlock variable @i[name] no longer
defined in the specified place.  @i[Kind] and @i[where] have the same
meanings as they do for @f[variable-value], except that @kwd[current]
is not available, and the default for @i[kind] is @kwd[global]

An error will be signaled if no such variable exists.  The hook,
@hid[Delete Variable Hook] is invoked with the same arguments before the
variable is deleted.
@enddefun


@section(Hooks)
@index(Hooks)
@label[hooks]
@hemlock actions such as setting variables, changing buffers, changing windows,
turning modes on and off, etc., often have hooks associated with them.  A hook
is a list of functions called before the system performs the action.  The
manual describes the object specific hooks with the rest of the operations
defined on these objects.

Often hooks are stored in @hemlock variables, @hid[Delete Buffer Hook] and
@hid[Set Window Hook] for example.  This leads to a minor point of confusion
because these variables have hooks that the system executes when someone
changes their values.  These hook functions @hemlock invokes when someone sets
a variable are an example of a hook stored in an object instead of a @hemlock
variable.  These are all hooks for editor activity, but @hemlock keeps them in
different kinds of locations.  This is why some of the routines in this section
have a special interpretation of the hook @i[place] argument.

@defmac[fun {add-hook}, args {@i[place] @i[hook-fun]}]
@defmac1[fun {remove-hook}, args {@i[place] @i[hook-fun]}]
These macros add or remove a hook function in some @i[place].  If @i[hook-fun]
already exists in @i[place], this call has no effect.  If @i[place] is a
symbol, then it is a @hemlock variable; otherwise, it is a generalized variable
or storage location.  Here are two examples:
@Begin[ProgramExample]
(add-hook delete-buffer-hook 'remove-buffer-from-menu)

(add-hook (variable-hooks 'check-mail-interval)
          'reschedule-mail-check)
@End[ProgramExample]
@enddefmac

@defmac[fun {invoke-hook}, args {@i[place] @rest @i[args]}]
This macro calls all the functions in @i[place].  If @i[place] is a symbol,
then it is a @hemlock variable; otherwise, it is a generalized variable.
@enddefun



@chapter(Commands)
@index (Commands)
@label[commands]


@section(Introduction)
The way that the user tells @hemlock to do something is by invoking a
@i(command).  Commands have three attributes:
@begin(description)
@i[name]@\A command's name provides a way to refer to it.  Command
names are usually capitalized words separated by spaces, such as 
@hid[Forward Word].

@i[documentation]@\The documentation for a command is used by
on-line help facilities.

@i[function]@\A command is implemented by a Lisp function, which is callable
from Lisp.
@end(description)

@defvar[var {command-names}]
Holds a string-table (page @pageref[string-tables]) associating
command names to command objects.  Whenever a new command is defined
it is entered in this table.
@enddefvar


@subsection(Defining Commands)

@defmac[fun {defcommand}, args 
{@^@mgroup<@i[command-name] @MOR (@i[command-name] @i[function-name])> @i[lambda-list]
@\@i[command-doc] @i[function-doc] @mstar<@i[form]>}]

Defines a command named @i[name].  @f[defcommand] creates a function to
implement the command from the @i[lambda-list] and @i[form]'s supplied.  The
@i[lambda-list] must specify one required argument, see section
@ref[invoking-commands-as-functions], which by convention is typically named
@f[p].  If the caller does not specify @i[function-name], @f[defcommand]
creates the command name by replacing all spaces with hyphens and appending
"@f[-command]".  @i[Function-doc] becomes the documentation for the function
and should primarily describe issues involved in calling the command as a
function, such as what any additional arguments are.  @i[Command-doc] becomes
the command documentation for the command.  @enddefmac

@defun[fun {make-command}, args 
	{@i[name] @i[documentation] @i[function]}] 
Defines a new command named @i[name], with command documentation
@I[documentation] and function @i[function].  The command in entered
in the string-table @varref[command-names], with the command object as
its value.  Normally command implementors will use the @f[defcommand]
macro, but this permits access to the command definition mechanism at
a lower level, which is occasionally useful.
@enddefun

@defun[fun {commandp}, args {@i[command]}]
Returns @true if @i[command] is a @f[command] object, otherwise @nil.
@enddefun

@defun[fun {command-documentation}, args {@i[command]}]
@defun1[fun {command-function}, args {@i[command]}]
@defun1[fun {command-name}, args {@i[command]}]
Returns the documentation, function, or name for @i[command].  These
may be set with @f[setf].
@enddefun


@subsection(Command Documentation)
@i[Command documentation] is a description of what the command does
when it is invoked as an extended command or from a key.  Command
documentation may be either a string or a function.  If the
documentation is a string then the first line should briefly summarize
the command, with remaining lines filling the details.  Example:
@lisp
(defcommand "Forward Character" (p)
  "Move the point forward one character.
   With prefix argument move that many characters, with negative
   argument go backwards."
  "Move the point of the current buffer forward p characters."
   . . .)
@endlisp

Command documentation may also be a function of one argument.  The
function is called with either @kwd[short] or @kwd[full], indicating
that the function should return a short documentation string or do
something to document the command fully.


@section(The Command Interpreter)
@index[Interpreter, command]
@index[Invocation, command]
@index[Command interpreter]

The @i[command interpreter] is a function which reads key-events (see section
@ref[key-events-intro]) from the keyboard and dispatches to different commands
on the basis of what the user types.  When the command interpreter executes a
command, we say it @i[invokes] the command.  The command interpreter also
provides facilities for communication between commands contiguously running
commands, such as a last command type register.  It also takes care of
resetting communication mechanisms, clearing the echo area, displaying partial
keys typed slowly by the user, etc.

@defvar[var {invoke-hook}]
This variable contains a function the command interpreter calls when it wants
to invoke a command.  The function receives the command and the prefix argument
as arguments.  The initial value is a function which simply funcalls the
@f[command-function] of the command with the supplied prefix argument.  This is
useful for implementing keyboard macros and similar things.
@enddefhvar

@defhvar[var "Command Abort Hook"]
The command interpreter invokes the function in this variable whenever someone
aborts a command (for example, if someone called @f[editor-error]).
@enddefhvar

When @hemlock initially starts the command interpreter is in control, but
commands may read from the keyboard themselves and assign whatever
interpretation they will to the key-events read.  Commands may call the command
interpreter recursively using the function @funref[recursive-edit].


@subsection(Editor Input)
@label[key-events-intro]
@index[key-events]

The canonical representation of editor input is a key-event structure.  Users
can bind commands to keys (see section @ref[key-bindings]), which are non-zero
length sequences of key-events.  A key-event consists of an identifying token
known as a @i[keysym] and a field of bits representing modifiers.  Users define
keysyms, integers between 0 and 65535 inclusively, by supplying names that
reflect the legends on their keyboard's keys.  Users define modifier names
similarly, but the system chooses the bit and mask for recognizing the
modifier.  You can use keysym and modifier names to textually specify
key-events and Hemlock keys in a @f[#k] syntax.  The following are some
examples:
@begin[programexample]
   #k"C-u"
   #k"Control-u"
   #k"c-m-z"
   #k"control-x meta-d"
   #k"a"
   #k"A"
   #k"Linefeed"
@end[programexample]
This is convenient for use within code and in init files containing
@f[bind-key] calls.

The @f[#k] syntax is delimited by double quotes, but the system parses the
contents rather than reading it as a Common Lisp string.  Within the double
quotes, spaces separate multiple key-events.  A single key-event optionally
starts with modifier names terminated by hyphens.  Modifier names are
alphabetic sequences of characters which the system uses case-insensitively.
Following modifiers is a keysym name, which is case-insensitive if it consists
of multiple characters, but if the name consists of only a single character,
then it is case-sensitive.

You can escape special characters @dash hyphen, double quote, open angle
bracket, close angle bracket, and space @dash with a backslash, and you can
specify a backslash by using two contiguously.  You can use angle brackets to
enclose a keysym name with many special characters in it.  Between angle
brackets appearing in a keysym name position, there are only two special
characters, the closing angle bracket and backslash.

For more information on key-events see section @ref[key-events].



@subsection(Binding Commands to Keys)
@label[Key-Bindings]
@Index[Key Bindings]

The command interpreter determines which command to invoke on the basis of
@i[key bindings].  A key binding is an association between a command and a
sequence of key-events (see section @ref[key-events-intro].  A sequence of
key-events is called a @i[key] and is represented by a single key-event or a
sequence (list or vector) of key-events.

Since key bindings may be local to a mode or buffer, the current environment
(page @pageref[current-environment]) determines the set of key bindings in
effect at any given time.  When the command interpreter tries to find the
binding for a key, it first checks if there is a local binding in the
@w[@funref[current-buffer]], then if there is a binding in each of the minor
modes and the major mode for the current buffer @w[(page @pageref[modes])], and
finally checks to see if there is a global binding.  If no binding is found,
then the command interpreter beeps or flashes the screen to indicate this.

@defun[fun {bind-key}, args
        {@i(name) @i(key) @optional @i[kind] @i[where]}]
 This function associates command @i[name] and @i[key] in some environment.
@i[Key] is either a key-event or a sequence of key-events.  There are three
possible values of @i[kind]:
@begin(description)
@kwd[global]@\
 The default, make a global key binding.

@kwd[mode]@\
 Make a mode specific key binding in the mode whose name is @i[where].

@kwd[buffer]@\
 Make a binding which is local to buffer @i[where].
@end(description)

This processes @i[key] for key translations before establishing the binding.
See section @ref[key-trans].

If the key is some prefix of a key binding which already exists in the
specified place, then the new one will override the old one, effectively
deleting it.

@f[ext:do-alpha-key-events] is useful for setting up bindings in certain new
modes.
@enddefun

@defun[fun {command-bindings}, args {@i[command]}]
This function returns a list of the places where @i[command] is bound.  A place
is specified as a list of the key (always a vector), the kind of binding, and
where (either the mode or buffer to which the binding is local, or @nil if it
is a global).
@enddefun

@defun[fun {delete-key-binding}, args {@i[key] @optional @i[kind] @i[where]}]
This function removes the binding of @i[key] in some place.  @i[Key] is either
a key-event or a sequence of key-events.  @i[kind] is the kind of binding to
delete, one of @kwd[global] (the default), @kwd[mode] or @kwd[buffer].  If
@i[kind] is @kwd[mode], @i[where] is the mode name, and if @i[kind] is
@kwd[buffer], then @i[where] is the buffer.

This function signals an error if @i[key] is unbound.

This processes @i[key] for key translations before deleting the binding.  See
section @ref[key-trans].
@enddefun

@defun[fun {get-command}, args {@i[key] @optional @i[kind] @i[where]}]
This function returns the command bound to @i[key], returning @nil if it is
unbound.  @i[Key] is either a key-event or a sequence of key-events.  If
@i[key] is an initial subsequence of some keys, then this returns the keyword
@kwd[prefix].  There are four cases of @i[kind]:
@begin(description)
@kwd[current]@\
 Return the current binding of @i[key] using the current buffer's search list.
If there are any transparent key bindings for @i[key], then they are returned
in a list as a second value.

@kwd[global]@\
 Return the global binding of @i[key].  This is the default.

@kwd[mode]@\
 Return the binding of @i[key] in the mode named @i[where].

@kwd[buffer]@\
 Return the binding of @i[key] local to the buffer @i[where].
@end(description)

This processes @i[key] for key translations before looking for any binding.
See section @ref[key-trans].
@enddefun

@defun[fun {map-bindings}, Args {@i[function] @i[kind] @optional @i[where]}]
This function maps over the key bindings in some place.  For each binding, this
passes @i[function] the key and the command bound to it.  @i[Kind] and
@i[where] are the same as in @f[bind-key].  The key is not guaranteed to remain
valid after a given iteration.
@enddefmac


@subsection[Key Translation]
@index[bit-prefix keys]
@index[key translation]
@index[translating keys]
@label[key-trans]
Key translation is a process that the command interpreter applies to keys
before doing anything else.  There are two kinds of key translations:
substitution and bit-prefix.  In either case, the command interpreter
translates a key when a specified key-event sequence appears in a key.

In a substitution translation, the system replaces the matched subsequence with
another key-event sequence.  Key translation is not recursively applied to the
substituted key-events.

In a bit-prefix translation, the system removes the matched subsequence and
effectively sets the specified bits in the next key-event in the key.

While translating a key, if the system encounters an incomplete final
subsequence of key-events, it aborts the translation process.  This happens
when those last key-events form a prefix of some translation.  It also happens
when they translate to a bit-prefix, but there is no following key-event to
which the system can apply the indicated modifier.  If there is a binding for
this partially untranslated key, then the command interpreter will invoke that
command; otherwise, it will wait for the user to type more key-events.

@defun[fun {key-translation}, args {@i[key]}]
This form is @f[setf]'able and allows users to register key translations that
the command interpreter will use as users type key-events.

This function returns the key translation for @i[key], returning @nil if there
is none.  @i[Key] is either a key-event or a sequence of key-events.  If
@i[key] is a prefix of a translation, then this returns @kwd[prefix].

A key translation is either a key or modifier specification.  The bits
translations have a list form: @w<@f[(:bits {]@i[bit-name]@f[}*)]>.

Whenever @i[key] appears as a subsequence of a key argument to the binding
manipulation functions, that portion will be replaced with the translation.
@enddefun



@subsection[Transparent Key Bindings]
@label[transparent-key-bindings]
@index[Transparent key bindings]

Key bindings local to a mode may be @i[transparent].  A transparent key
binding does not shadow less local key bindings, but rather indicates that
the bound command should be invoked before the first normal key binding.
Transparent key bindings are primarily useful for implementing minor modes
such as auto fill and word abbreviation.  There may be several transparent
key bindings for a given key, in which case all of the commands bound are
invoked in the order they were found.  If there no normal key binding for a
key typed, then the command interpreter acts as though the key is unbound
even if there are transparent key bindings.

The @kwd[transparent-p] argument to @funref[defmode] determines whether the
key bindings in a mode are transparent or not.


@subsection (Interactive)
@index (Keyboard macro vs. interactive)
@index (Interactive vs. keyboard macro)
@Hemlock supports keyboard macros.  A user may enter a mode where the editor
records his actions, and when the user exits this mode, the command @hid[Last
Keyboard Macro] plays back the actions.  Some commands behave differently when
invoked as part of the definition of a keyboard macro.  For example, when used
in a keyboard macro, a command that @f[message]'s useless user confirmation
will slow down the repeated invocations of @hid[Last Keyboard Macro] because
the command will pause on each execution to make sure the user sees the
message.  This can be eliminated with the use of @f[interactive].  As another
example, some commands conditionally signal an editor-error versus simply
beeping the device depending on whether it executes on behalf of the user or a
keyboard macro.

@defun[fun {interactive}]
This returns @true when the user invoked the command directly.
@enddefun


@section(Command Types)
@index(Command types)
@label(command-types)
In many editors the behavior of a command depends on the kind of command
invoked before it.  @hemlock provides a mechanism to support this known as
@i(command type).

@defun[fun {last-command-type}]
This returns the command type of the last command invoked.  If this is set with
@f[setf], the supplied value becomes the value of @f[last-command-type] until
the next command completes.  If the previous command did not set
@f[last-command-type], then its value is @nil.  Normally a command type is a
keyword.  The command type is not cleared after a command is invoked due to a
transparent key binding.
@enddefun


@section(Command Arguments)
@label[invoking-commands-as-functions]
There are three ways in which a command may be invoked: It may be bound to a
key which has been typed, it may be invoked as an extended command, or it may
be called as a Lisp function.  Ideally commands should be written in such a way
that they will behave sensibly no matter which way they are invoked.  The
functions which implement commands must obey certain conventions about argument
passing if the command is to function properly.


@subsection(The Prefix Argument)
@index(Prefix arguments)
Whenever a command is invoked it is passed as its first argument what
is known as the @i[prefix argument].  The prefix argument is always
either an integer or @nil.  When a command uses this value it is
usually as a repeat count, or some conceptually similar function.

@defun[fun {prefix-argument}]
This function returns the current value of the prefix argument.  When
set with @f[setf], the new value becomes the prefix argument for the
next command.
@enddefun

If the prefix argument is not set by the previous command then the
prefix argument for a command is @nil.  The prefix argument is not cleared
after a command is invoked due to a transparent key binding.


@subsection(Lisp Arguments)
It is often desirable to call commands from Lisp code, in which case
arguments which would otherwise be prompted for are passed as optional
arguments following the prefix argument.  A command should prompt for
any arguments not supplied.


@section(Recursive Edits)
@index(Recursive edits)
@defmac[fun {use-buffer}, args {@i[buffer] @mstar<@i[form]>}]
The effect of this is similar to setting the current-buffer to @i[buffer]
during the evaluation of @i[forms].  There are restrictions placed on what the
code can expect about its environment.  In particular, the value of any global
binding of a @hemlock variable which is also a mode local variable of some mode
is ill-defined; if the variable has a global binding it will be bound, but the
value may not be the global value.  It is also impossible to nest
@f[use-buffer]'s in different buffers.  The reason for using @f[use-buffer] is
that it may be significantly faster than changing @f[current-buffer] to
@i[buffer] and back.
@enddefmac

@defun[fun {recursive-edit}, args {@optional @i[handle-abort]}]
@defhvar1[var {Enter Recursive Edit Hook}]
@index[aborting]
@f[recursive-edit] invokes the command interpreter.  The command interpreter
will read from the keyboard and invoke commands until it is terminated with
either @f[exit-recursive-edit] or @f[abort-recursive-edit].

Normally, an editor-error or @bf[C-g] aborts the command in progress and
returns control to the top-level command loop.  If @f[recursive-edit] is used
with @i[handle-abort] true, then @f[editor-error] or @bf[C-g] will only abort
back to the recursive command loop.

Before the command interpreter is entered the hook
@hid[Enter Recursive Edit Hook] is invoked.
@enddefun

@defun[fun {in-recursive-edit}]
This returns whether the calling point is dynamically within a recursive edit
context.
@enddefun

@defun[fun {exit-recursive-edit}, args {@optional @i[values-list]}]
@defhvar1[var {Exit Recursive Edit Hook}]
@f[exit-recursive-edit] exits a recursive edit returning as multiple values
each element of @i[values-list], which defaults to @nil.  This invokes
@hid[Exit Recursive Edit Hook] after exiting the command interpreter.  If no
recursive edit is in progress, then this signals an error.
@enddefun

@defun[fun {abort-recursive-edit}, args {@rest @i[args]}]
@defhvar1[var {Abort Recursive Edit Hook}]
@f[abort-recursive-edit] terminates a recursive edit by applying
@funref[editor-error] to @i[args] after exiting the command interpreter.  This
invokes @hid[Abort Recursive Edit Hook] with @i[args] before aborting the
recursive edit .  If no recursive edit is in progress, then this signals an
error.
@enddefun



@Chapter(Modes)
@label[modes]
@index (Modes)
A mode is a collection of @hemlock values which may be present in the current
environment @w<(page @pageref(current-environment))> depending on the editing
task at hand.  Examples of typical modes are @hid[Lisp], for editing Lisp code,
and @hid[Echo Area], for prompting in the echo area.


@section(Mode Hooks)
  When a mode is added to or removed from a buffer, its @i[mode hook]
is invoked.  The hook functions take two arguments, the buffer
involved and @true if the mode is being added or @nil if it is being
removed. 

Mode hooks are typically used to make a mode do something additional to
what it usually does.  One might, for example, make a text mode hook
that turned on auto-fill mode when you entered.


@section(Major and Minor Modes)
There are two kinds of modes, @i[major] modes and @i[minor] modes.  A buffer
always has exactly one major mode, but it may have any number of minor modes.
Major modes may have mode character attributes while minor modes may not.

A major mode is usually used to change the environment in some major way, such
as to install special commands for editing some language.  Minor modes
generally change some small attribute of the environment, such as whether lines
are automatically broken when they get too long.  A minor mode should work
regardless of what major mode and minor modes are in effect.

@defhvar[var {Default Modes}, val {("Fundamental" "Save")}]
This variable contains a list of mode names which are instantiated in a
buffer when no other information is available.
@enddefhvar

@defvar[var {mode-names}]
Holds a string-table of the names of all the modes.
@enddefvar

@defcom[com "Illegal"]
This is a useful command to bind in modes that wish to shadow global bindings
by making them effectively illegal.  Also, although less likely, minor modes
may shadow major mode bindings with this.  This command calls @f[editor-error].
@enddefcom


@section(Mode Functions)

@defun[fun {defmode}, args {@i[name]},
        keys {[setup-function][cleanup-function][major-p]},
        morekeys {[precedence][transparent-p][documentation]}]
This function defines a new mode named @i[name], and enters it in
@varref[mode-names].  If @i[major-p] is supplied and is not @nil
then the mode is a major mode; otherwise it is a minor mode.

@i[Setup-function] and @i[cleanup-function] are functions which are
invoked with the buffer affected, after the mode is turned on, and
before it is turned off, respectively.  These functions typically are
used to make buffer-local key or variable bindings and to remove them
when the mode is turned off.

@i[Precedence] is only meaningful for a minor mode.  The precedence of a
minor mode determines the order in which it in a buffer's list of modes.
When searching for values in the current environment, minor modes are
searched in order, so the precedence of a minor mode determines which value
is found when there are several definitions.

@i[Transparent-p] determines whether key bindings local to the defined mode
are transparent.  Transparent key bindings are invoked in addition to the
first normal key binding found rather than shadowing less local key bindings.

@i[Documentation] is some introductory text about the mode.  Commands such as
@hid[Describe Mode] use this.
@enddefun

@defun[fun {mode-documentation}, args {@i[name]}]
This function returns the documentation for the mode named @i[name].
@enddefun

@defun[fun {buffer-major-mode}, args {@i[buffer]}]
@defhvar1[var {Buffer Major Mode Hook}]
@f[buffer-major-mode] returns the name of @i[buffer]'s major mode.
The major mode may be changed with @f[setf]; then
 @hid[Buffer Major Mode Hook] is invoked with
@i[buffer] and the new mode.
@enddefun

@defun[fun {buffer-minor-mode}, args {@i[buffer] @i[name]}]
@defhvar1[var {Buffer Minor Mode Hook}]
@f[buffer-minor-mode] returns @true if the minor mode @i[name] is active
in @i[buffer], @nil otherwise.  A minor mode may be turned on or off
by using @f[setf]; then @hid[Buffer Minor Mode Hook] is
invoked with @i[buffer], @i[name] and the new value.
@enddefun

@defun[fun {mode-variables}, args {@i[name]}]
Returns the string-table of mode local variables.
@enddefun

@defun[fun {mode-major-p}, args {@i[name]}]
Returns @true if @i[name] is the name of a major mode, or @nil if
it is the name of a minor mode.  It is an error for @i[name] not to be
the name of a mode.
@enddefun



@chapter(Character Attributes)
@label(character-attributes)
@index(Character attributes)
@index(Syntax tables)

@section(Introduction)
Character attributes provide a global database of information about characters.
This facility is similar to, but more general than, the @i[syntax tables] of
other editors such as @f[EMACS].  For example, you should use character
attributes for commands that need information regarding whether a character is
@i[whitespace] or not.  Use character attributes for these reasons:
@begin(enumerate)
If this information is all in one place, then it is easy the change the
behavior of the editor by changing the syntax table, much easier than it would
be if character constants were wired into commands.

This centralization of information avoids needless duplication of effort.

The syntax table primitives are probably faster than anything that can be
written above the primitive level.
@end(enumerate)

Note that an essential part of the character attribute scheme is that
@i[character attributes are global and are there for the user to change.]
Information about characters which is internal to some set of commands (and
which the user should not know about) should not be maintained as a character
attribute.  For such uses various character searching abilities are provided by
the function @funref[find-pattern].

@defcon[var {syntax-char-code-limit}]
The exclusive upper bound on character codes which are significant in
the character attribute functions.  Font and bits are always ignored.
@enddefcon


@section(Character Attribute Names)

As for @hemlock variables, character attributes have a user visible
string name, but are referred to in Lisp code as a symbol.  The string
name, which is typically composed of capitalized words separated by
spaces, is translated into a keyword by replacing all spaces with
hyphens and interning this string in the keyword package.  The
attribute named @hid[Ada Syntax] would thus become @kwd[ada-syntax].

@defvar[var {character-attribute-names}]
Whenever a character attribute is defined, its name is entered in
this string table (page @pageref[string-tables]), with the
corresponding keyword as the value.
@enddefvar


@section(Character Attribute Functions)

@defun[fun {defattribute}, args 
	{@i[name] @i[documentation] @optional @i[type] @i[initial-value]}]
 This function defines a new character attribute with @i[name], a
simple-string.  Character attribute operations take attribute arguments as a
keyword whose name is @i[name] uppercased with spaces replaced by hyphens.

@i[Documentation] describes the uses of the character attribute.

@i[Type], which defaults to @w<@f[(mod 2)]>, specifies what type the values of
the character attribute are.  Values of a character attribute may be of any
type which may be specified to @f[make-array].  @i[Initial-value] (default
@f[0]) is the value which all characters will initially have for this
attribute.
@enddefun

@defun[fun {character-attribute-name}, args {@i[attribute]}]
@defun1[fun {character-attribute-documentation}, args {@i[attribute]}]
These functions return the name or documentation for @i[attribute].
@enddefun

@defun[fun {character-attribute}, args	{@i[attribute] @i[character]}]
@defhvar1[var {Character Attribute Hook}]
@f[character-attribute] returns the value of @i[attribute] for @i[character].
This signals an error if @i[attribute] is undefined.

@f[setf] will set a character's attributes.  This @f[setf] method invokes the
functions in @hid[Character Attribute Hook] on the attribute and character
before it makes the change.

If @i[character] is @nil, then the value of the attribute for the beginning or
end of the buffer can be accessed or set.  The buffer beginning and end thus
become a sort of fictitious character, which simplifies the use of character
attributes in many cases.
@enddefun

@defun[fun {character-attribute-p}, args {@i[symbol]}]
This function returns @true if @i[symbol] is the name of a character attribute,
@nil otherwise.
@enddefun

@defun[fun {shadow-attribute}, args 
{@i[attribute] @i[character] @i[value] @i[mode]}]
@defhvar1[var {Shadow Attribute Hook}]
This function establishes @i[value] as the value of @i[character]'s
@i[attribute] attribute when in the mode @i[mode].  @i[Mode] must be the name
of a major mode.  @hid[Shadow Attribute Hook] is invoked with the same
arguments when this function is called.  If the value for an attribute is set
while the value is shadowed, then only the shadowed value is affected, not the
global one.
@enddefun

@defun[fun {unshadow-attribute}, args {@i[attribute] @i[character] @i[mode]}]
@defhvar1[var {Unshadow Attribute Hook}]
Make the value of @i[attribute] for @i[character] no longer be shadowed in
@i[mode].  @hid[Unshadow Attribute Hook] is invoked with the same arguments
when this function is called.
@enddefun

@defun[fun {find-attribute},
	args {@i[mark] @i[attribute] @optional @i[test]}]
@defun1[fun {reverse-find-attribute},
	args {@i[mark] @i[attribute] @optional @i[test]}]
 These functions find the next (or previous) character with some value for the
character attribute @i[attribute] starting at @i[mark].  They pass @i[Test] one
argument, the value of @i[attribute] for the character tested.  If the test
succeeds, then these routines modify @i[mark] to point before (after for
@f[reverse-find-attribute]) the character which satisfied the test.  If no
characters satisfy the test, then these return @nil, and @i[mark] remains
unmodified.  @i[Test] defaults to @f[not zerop].  There is no guarantee that
the test is applied in any particular fashion, so it should have no side
effects and depend only on its argument.
@enddefun


@section(Character Attribute Hooks)

It is often useful to use the character attribute mechanism as an abstract
interface to other information about characters which in fact is stored
elsewhere.  For example, some implementation of @hemlock might decide to define
a @hid[Print Representation] attribute which controls how a character is
displayed on the screen.

To make this easy to do, each attribute has a list of hook functions
which are invoked with the attribute, character and new value whenever
the current value changes for any reason.

@defun[fun {character-attribute-hooks}, args {@i[attribute]}]
Return the current hook list for @i[attribute].  This may be set with
@f[setf].  The @f[add-hook] and @macref[remove-hook] macros should
be used to manipulate these lists.
@enddefun


@section (System Defined Character Attributes)
@label(sys-def-chars)
These are predefined in @hemlock:
@begin[description]
@hid[Whitespace]@\
A value of @f[1] indicates the character is whitespace.

@hid[Word Delimiter]@\
A value of @f[1] indicates the character separates words (see section
@ref[text-functions]).

@hid[Digit]@\
A value of @f[1] indicates the character is a base ten digit.  This may be
shadowed in modes or buffers to mean something else.

@hid[Space]@\
This is like @hid[Whitespace], but it should not include @binding[Newline].
@hemlock uses this primarily for handling indentation on a line.

@hid[Sentence Terminator]@\
A value of @f[1] indicates these characters terminate sentences (see section
@ref[text-functions]).

@hid[Sentence Closing Char]@\
A value of @f[1] indicates these delimiting characters, such as @binding["]
or @binding[)], may follow a @hid[Sentence Terminator] (see section
@ref[text-functions]).

@hid[Paragraph Delimiter]@\
A value of @f[1] indicates these characters delimit paragraphs when they begin
a line (see section @ref[text-functions]).

@hid[Page Delimiter]@\
A value of @f[1] indicates this character separates logical pages (see section
@ref[logical-pages]) when it begins a line.

@hid[Scribe Syntax]@\
This uses the following symbol values:
@begin[multiple]
@begin[description]
@nil@\These characters have no interesting properties.

@kwd[escape]@\This is @binding[@@] for the Scribe formatting language.

@kwd[open-paren]@\These characters begin delimited text.

@kwd[close-paren]@\These characters end delimited text.

@kwd[space]@\These characters can terminate the name of a formatting command.

@kwd[newline]@\These characters can terminate the name of a formatting command.
@end[description]
@end[multiple]


@hid[Lisp Syntax]@\
This uses symbol values from the following:
@begin[multiple]
@begin[description]
@nil@\These characters have no interesting properties.

@kwd[space]@\These characters act like whitespace and should not include
@binding[Newline].

@kwd[newline]@\This is the @binding[Newline] character.

@kwd[open-paren]@\This is @binding[(] character.

@kwd[close-paren]@\This is @binding[)] character.

@kwd[prefix]@\This is a character that is a part of any form it precedes @dash
for example, the single quote, @binding['].

@kwd[string-quote]@\This is the character that quotes a string literal,
@binding["].@comment["]

@kwd[char-quote]@\This is the character that escapes a single character,
@binding[\].

@kwd[comment]@\This is the character that makes a comment with the rest of the
line, @binding[;].

@kwd[constituent]@\These characters are constitute symbol names.
@end[description]
@end[multiple]

@end[description]



@chapter (Controlling the Display)
@section (Windows)
@tag[windows]
@index(Windows)
@index(modelines)

A window is a mechanism for displaying part of a buffer on some physical
device.  A window is a way to view a buffer but is not synonymous with one; a
buffer may be viewed in any number of windows.  A window may have a
@i[modeline] which is a line of text displayed across the bottom of a window to
indicate status information, typically related to the buffer displayed.


@section (The Current Window)
@index (Current window)
@defun[fun {current-window}, args {}]
@defhvar1[var {Set Window Hook}]
@f[current-window] returns the window in which the cursor is currently
displayed.  The cursor always tracks the buffer-point of the corresponding
buffer.  If the point is moved to a position which would be off the screen the
recentering process is invoked.  Recentering shifts the starting point of the
window so that the point is once again displayed.  The current window may be
changed with @f[setf].  Before the current window is changed, the hook @hid[Set
Window Hook] is invoked with the new value.
@enddefun

@defvar[var {window-list}]
Holds a list of all the window objects made with @funref[make-window].
@enddefvar


@section(Window Functions)

@defun[fun {make-window}, args {@i[mark]},
	keys {[modelinep][window][ask-user]},
	morekeys {[x][y][width][height]},
	morekeys {[proportion]}]
@defhvar1[var {Default Window Width}]
@defhvar1[var {Default Window Height}]
@defhvar1[var {Make Window Hook}]

@comment[NOTE, we purposefully do not document the font-family or device
	 arguments since we don't officially support fonts or devices.]

@f[make-window] returns a window displaying text starting at @i[mark], which
must point into a buffer.  If it could not make a window on the device, it
returns nil.  The default action is to make the new window a proportion of the
@f[current-window]'s height to make room for the new window.

@i[Modelinep] specifies whether the window should display buffer modelines.

@i[Window] is a device dependent window to be used with the Hemlock window.
The device may not support this argument.  @i[Window] becomes the parent window
for a new group of windows that behave in a stack orientation as windows do on
the terminal.

If @i[ask-user] is non-@nil, @hemlock prompts the user for the missing
dimensions (@i[x], @i[y], @i[width], and @i[height]) to make a new group of
windows, as with the @i[window] argument.  The device may not support this
argument.  Non-null values other than @f[t] may have device dependent meanings.
@i[X] and @i[y] are in pixel units, but @i[width] and @i[height] are characters
units.  @hid[Default Window Width] and @hid[Default Window Height] are the
default values for the @i[width] and @i[height] arguments.

@i[Proportion] determines what proportion of the @f[current-window]'s height
the new window will use.  The @f[current-window] retains whatever space left
after accommodating the new one.  The default is to split the window in half.

This invokes @hid[Make Window Hook] with the new window.
@enddefun

@defun[fun {windowp}, args {@i[window]}]
This function returns @true if @i[window] is a @f[window] object, otherwise
@nil.
@enddefun

@defun[fun {delete-window}, args {@i[window]}]
@defhvar1[var {Delete Window Hook}]
@f[delete-window] makes @i[window] go away, first invoking @hid[Delete Window
Hook] with @i[window].
@enddefun

@defun[fun {window-buffer}, args {@i[window]}]
@defhvar1[var {Window Buffer Hook}]
@f[window-buffer] returns the buffer from which the window displays
text.  This may be changed with @f[setf], in which case the hook
@hid[Window Buffer Hook] is invoked beforehand with the window and the
new buffer.
@enddefun

@defun[fun {window-display-start}, args {@i[window]}]
@defun1[fun {window-display-end}, args {@i[window]}] 
@f[window-display-start] returns the mark that points before the first
character displayed in @i[window].  Note that if @i[window] is the current
window, then moving the start may not prove much, since recentering may move it
back to approximately where it was originally.

@f[window-display-end] is similar, but points after the last character
displayed.  Moving the end is meaningless, since redisplay always moves it to
after the last character.
@enddefun

@defun[fun {window-display-recentering}, args {@i[window]}]
This function returns whether redisplay will ensure the buffer's point of
@i[window]'s buffer is visible after redisplay.  This is @f[setf]'able, and
changing @i[window]'s buffer sets this to @nil via @hid[Window Buffer Hook].
@enddefun

@defun[fun {window-point}, args {@i[window]}]
This function returns as a mark the position in the buffer where the cursor is
displayed.  This may be set with @f[setf].  If @i[window] is the current
window, then setting the point will have little effect; it is forced to track
the buffer point.  When the window is not current, the window point is the
position that the buffer point will be moved to when the window becomes
current.
@enddefun

@defun[fun {center-window}, args {@i[window] @i[mark]}]
This function attempts to adjust window's display start so the that @i[mark] is
vertically centered within the window.
@enddefun

@defun[fun {scroll-window}, args {@i[window] @i[n]}]
This function scrolls the window down @i[n] display lines; if @i[n] is negative
scroll up.  Leave the cursor at the same text position unless we scroll it off
the screen, in which case the cursor is moved to the end of the window closest
to its old position.
@enddefun

@defun[fun {displayed-p}, args {@i[mark] @i[window]}]
Returns @true if either the character before or the character after @i[mark]
is being displayed in @i[window], or @nil otherwise.  
@enddefun

@defun[fun {window-height}, args {@i[window]}]
@defun1[fun {window-width}, args {@i[window]}]
Height or width of the area of the window used for displaying the
buffer, in character positions.  These values may be changed with
@f[setf], but the setting attempt may fail, in which case nothing is done.
@enddefun

@defun[fun {next-window}, args {@i[window]}]
@defun1[fun {previous-window}, args {@i[window]}]
Return the next or previous window of @i[window].  The exact meaning of next
and previous depends on the device displaying the window.  It should be
possible to cycle through all the windows displayed on a device using either
next or previous (implying that these functions wrap around.)
@enddefun


@section(Cursor Positions)
@index(Cursor positions)
A cursor position is an absolute position within a window's coordinate
system.  The origin is in the upper-left-hand corner and the unit
is character positions.

@defun[fun {mark-to-cursorpos}, args {@i[mark] @i[window]}]
Returns as multiple values the @f[X] and @f[Y] position on which
@i[mark] is being displayed in @i[window], or @nil if it is not within the
bounds displayed.
@enddefun

@defun[fun {cursorpos-to-mark}, args {@i[X] @i[Y] @i[window]}]
Returns as a mark the text position which corresponds to the given
(@i[X], @i[Y]) position within window, or @nil if that
position does not correspond to any text within @i[window].
@enddefun

@defun[fun {last-key-event-cursorpos}]
Interprets mouse input.  It returns as multiple values the (@i[X], @i[Y])
position and the window where the pointing device was the last time some key
event happened.  If the information is unavailable, this returns @nil.
@enddefun

@defun[fun {mark-column}, args {@i[mark]}]
This function returns the @i[X] position at which @i[mark] would be displayed,
supposing its line was displayed on an infinitely wide screen.  This takes into
consideration strange characters such as tabs.
@enddefun

@defun[fun {move-to-column}, args {@i[mark] @i[column] @optional @i[line]}]
This function is analogous to @funref[move-to-position], except that
it moves @i[mark] to the position on @i[line] which corresponds to the
specified @i[column].  @i[Line] defaults to the line that @i[mark] is
currently on.  If the line would not reach to the specified column,
then @nil is returned and @i[mark] is not modified.  Note that since a
character may be displayed on more than one column on the screen,
several different values of @i[column] may cause @i[mark] to be moved
to the same position.
@enddefun

@defun[fun {show-mark}, args {@i[mark] @i[window] @i[time]}]
This function highlights the position of @i[mark] within @i[window] for
@i[time] seconds, possibly by moving the cursor there.  The wait may be aborted
if there is pending input.  If @i[mark] is positioned outside the text
displayed by @i[window], then this returns @nil, otherwise @true.
@enddefun


@section(Redisplay)
Redisplay translates changes in the internal representation of text into
changes on the screen.  Ideally this process finds the minimal transformation
to make the screen correspond to the text in order to maximize the speed of
redisplay.

@defun[fun {redisplay}]
@defhvar1[var "Redisplay Hook"]
@f[redisplay] executes the redisplay process, and @hemlock typically invokes
this whenever it looks for input.  The redisplay process frequently checks for
input, and if it detects any, it aborts.  The return value is interpreted as
follows:
@begin[description]
@false@\No update was needed.

@true@\Update was needed, and completed successfully.

@kwd[editor-input]@\Update is needed, but was aborted due to pending input.
@end[description]

This function invokes the functions in @hid[Redisplay Hook] on the current
window after computing screen transformations but before executing them.  After
invoking the hook, this recomputes the redisplay and then executes it on the
current window.

For the current window and any window with @f[window-display-recentering] set,
@f[redisplay] ensures the buffer's point for the window's buffer is visible
after redisplay.
@enddefun

@defun[fun {redisplay-all}]
This causes all editor windows to be completely redisplayed.  For the current
window and any window with @f[window-display-recentering] set, this ensures the
buffer's point for the window's buffer is visible after redisplay.  The return
values are the same as for redisplay, except that @false is never returned.
@enddefun

@defun[fun {editor-finish-output}, args {@i[window]}]
This makes sure the editor is synchronized with respect to redisplay output to
@i[window].  This may do nothing on some devices.
@enddefun



@chapter(Logical Key-Events)
@label[logical-key-events]
@index[Logical key-events]


@section[Introduction]
Some primitives such as @funref[prompt-for-key] and commands such as EMACS
query replace read key-events directly from the keyboard instead of using the
command interpreter.  To encourage consistency between these commands and to
make them portable and easy to customize, there is a mechanism for defining
@i[logical key-events].

A logical key-event is a keyword which stands for some set of key-events.  The
system globally interprets these key-events as indicators a particular action.
For example, the @kwd[help] logical key-event represents the set of key-events
that request help in a given @hemlock implementation.  This mapping is a
many-to-many mapping, not one-to-one, so a given logical key-event may have
multiple corresponding actual key-events.  Also, any key-event may represent
different logical key-events.


@section[Logical Key-Event Functions]

@defvar[var {logical-key-event-names}]
This variable holds a string-table mapping all logical key-event names to the
keyword identifying the logical key-event.
@enddefvar

@defun[fun {define-logical-key-event}, args {@i[string-name] @i[documentation]}]
 This function defines a new logical key-event with name @i[string-name], a
simple-string.  Logical key-event operations take logical key-events arguments
as a keyword whose name is @i[string-name] uppercased with spaces replaced by
hyphens.

@i[Documentation] describes the action indicated by the logical key-event.
@enddefun

@defun[fun {logical-key-event-key-events}, args {@i[keyword]}]
This function returns the list of key-events representing the logical key-event
@i[keyword].
@enddefun

@defun[fun {logical-key-event-name}, args {@i[keyword]}]
@defun1[fun {logical-key-event-documentation}, args {@i[keyword]}]
These functions return the string name and documentation given to
@f[define-logical-key-event] for logical key-event @i[keyword].
@enddefun

@defun[fun {logical-key-event-p}, args {@i[key-event] @i[keyword]}]
This function returns @f[t] if @i[key-event] is the logical key-event
@i[keyword].  This is @f[setf]'able establishing or disestablishing key-events
as particular logical key-events.  It is a error for @i[keyword] to be an
undefined logical key-event.
@enddefun


@section[System Defined Logical Key-Events]
There are many default logical key-events, some of which are used by functions
documented in this manual.  If a command wants to read a single key-event
command that fits one of these descriptions then the key-event read should be
compared to the corresponding logical key-event instead of explicitly
mentioning the particular key-event in the code.  In many cases you can use the
@macref[command-case] macro.  It makes logical key-events easy to use and takes
care of prompting and displaying help messages.

@begin[description]
@kwd[yes]@\
 Indicates the prompter should take the action under consideration.

@kwd[no]@\
 Indicates the prompter should NOT take the action under consideration.

@kwd[do-all]@\
 Indicates the prompter should repeat the action under consideration as many
times as possible.

@kwd[do-once]@\
 Indicates the prompter should execute the action under consideration once and
then exit.

@kwd[exit]@\
 Indicates the prompter should terminate its activity in a normal fashion.

@kwd[abort]@\
 Indicates the prompter should terminate its activity without performing any
closing actions of convenience, for example.

@kwd[keep]@\
 Indicates the prompter should preserve something.

@kwd[help]@\
 Indicates the prompter should display some help information.

@kwd[confirm]@\
 Indicates the prompter should take any input provided or use the default if
the user entered nothing.

@kwd[quote]@\
 Indicates the prompter should take the following key-event as itself without
any sort of command interpretation.

@kwd[recursive-edit]@\
 Indicates the prompter should enter a recursive edit in the current context.

@kwd[cancel]@\
 Indicates the prompter should cancel the effect of a previous key-event input.

@kwd[forward-search]@\
 Indicates the prompter should search forward in the current context.

@kwd[backward-search]@\
 Indicates the prompter should search backward in the current context.
@end[description]

@blankspace(1 line)
Define a new logical key-event whenever:
@begin[enumerate]
The key-event concerned represents a general class of actions, and
several commands may want to take a similar action of this type.

The exact key-event a command implementor chooses may generate violent taste
disputes among users, and then the users can trivially change the command in
their init files.

You are using @f[command-case] which prevents implementors from specifying
non-standard characters for dispatching in otherwise possibly portable code, 
and you can define and set the logical key-event in a site dependent file where
you can mention implementation dependent characters.
@end[enumerate]



@chapter(The Echo Area)

@hemlock provides a number of facilities for displaying information and
prompting the user for it.  Most of these work through a small window displayed
at the bottom of the screen.  This is called the echo area and is supported by
a buffer and a window.  This buffer's modeline (see section @ref[modelines]) is
referred to as the status line, which, unlike other buffers' modelines, is used
to show general status about the editor, Lisp, or world.

@defhvar[var {Default Status Line Fields}]
This is the initial list of modeline-field objects stored in the echo area
buffer.
@enddefhvar

@defhvar[var "Echo Area Height", val {3}]
This variable determines the initial height in lines of the echo area window. 
@enddefhvar


@section(Echo Area Functions)
It is considered poor taste to perform text operations on the echo area buffer
to display messages; the @f[message] function should be used instead.  A
command must use this function or set @funref[buffer-modified] for the
@hid[Echo Area] buffer to @nil to cause @hemlock to leave text in the echo area
after the command's execution.

@defun[fun {clear-echo-area}]
Clears the echo area.
@enddefun

@defun[fun {message}, args {@i[control-string] @rest @i[format-arguments]}]
@defun1[fun {loud-message}, args {@i[control-string] @rest @i[format-arguments]}]
@defhvar1[var {Message Pause}, val {0.5}]
Displays a message in the echo area.  The message is always displayed on a
fresh line.  @f[message] pauses for @hid[Message Pause] seconds before
returning to assure that messages are not displayed too briefly to be seen.
Because of this, @f[message] is the best way to display text in the echo area.

@f[loud-message] is like @f[message], but it first clears the echo area and
beeps.
@enddefun

@defvar[var {echo-area-window}]
@defvar1[var {echo-area-buffer}]
@f[echo-area-buffer] contains the buffer object for the echo area, which is
named @hid[Echo Area].  This buffer is usually in @hid[Echo Area] mode.
@f[echo-area-window] contains a window displaying @f[echo-area-buffer].  Its
modeline is the status line, see the beginning of this chapter.
@enddefvar

@defvar[var {echo-area-stream}]
@index (Echo area)
This is a buffered @hemlock output stream
(@pageref[make-hemlock-output-stream-fun]) which inserts text written to it at
the point of the echo area buffer.  Since this stream is buffered a
@f[force-output] must be done when output is complete to assure that it is
displayed.
@enddefvar


@section(Prompting Functions)
@index(Prompting functions)
Most of the prompting functions accept the following keyword arguments:
@begin(description)
@kwd[must-exist] @\If @kwd[must-exist] has a non-@nil value then the
user is prompted until a valid response is obtained.  If
@kwd[must-exist] is @nil then return as a string whatever is input.
The default is @true.

@kwd[default] @\If null input is given when the user is prompted 
then this value is returned.  If no default is given then
some input must be given before anything interesting will happen.

@kwd[default-string] @\If a @kwd[default] is given then this is a
string to be printed to indicate what the default is.  The default is
some representation of the value for @kwd[default], for example for a
buffer it is the name of the buffer.

@kwd[prompt] @\This is the prompt string to display.

@kwd[help] @\@multiple{
This is similar to @kwd[prompt], except that it is displayed when
the help command is typed during input.  @comment{If there is some known number
of options as in keyword parses, then they may be displayed, depending
on the setting of @hvarref[Help Show Options].}

This may also be a function.  When called with no arguments, it should either
return a string which is the help text or perform some action to help the user,
returning @Nil.}
@end(description)

@defun[fun {prompt-for-buffer}, keys {[prompt][help][must-exist][default]},
	morekeys {[default-string]}] 
Prompts with completion for a buffer name and returns the corresponding buffer.
If @i[must-exist] is @nil, then it returns the input string if it is not a
buffer name.  This refuses to accept the empty string as input when
@kwd[default] and @kwd[default-string] are @nil.  @kwd[default-string] may be
used to supply a default buffer name when @kwd[default] is @nil, but when
@kwd[must-exist] is non-@nil, it must name an already existing buffer.
@enddefun

@defmac[fun {command-case}, Args {(@mstar<@i[key] @i[value]>) @Mstar<(@Mgroup"(@MSTAR'@i[tag]') @MOR @i[tag]" @i[help] @MSTAR'@i[form]')>}] 
 This macro is analogous to the Common Lisp @f[case] macro.  Commands such as
@hid[Query Replace] use this to get a key-event, translate it to a character,
and then to dispatch on the character to some case.  In addition to character
dispatching, this supports logical key-events @w<(page
@pageref[logical-key-events])> by using the input key-event directly without
translating it to a character.  Since the description of this macro is rather
complex, first consider the following example:
@lisp
(defcommand "Save All Buffers" (p)
  "Give the User a chance to save each modified buffer."
  "Give the User a chance to save each modified buffer."
  (dolist (b *buffer-list*)
    (select-buffer-command () b)
    (when (buffer-modified b)
      (command-case (:prompt "Save this buffer: [Y] "
		     :help "Save buffer, or do something else:")
	((:yes :confirm)
	 "Save this buffer and go on to the next."
	 (save-file-command () b))
	(:no "Skip saving this buffer, and go on to the next.")
	(:recursive-edit
	 "Go into a recursive edit in this buffer."
	 (do-recursive-edit) (reprompt))
	((:exit #\p) "Punt this silly loop."
	 (return nil))))))
@endlisp

@f[command-case] prompts for a key-event and then executes the code in the
first branch with a logical key-event or a character (called @i[tags]) matching
the input.  Each character must be a standard-character, one that satisfies the
Common Lisp @f[standard-char-p] predicate, and the dispatching mechanism
compares the input key-event to any character tags by mapping the key-event to
a character with @f[ext:key-event-char].  If the tag is a logical key-event,
then the search for an appropriate case compares the key-event read with the
tag using @f[logical-key-event-p].

All uses of @f[command-case] have two default cases, @kwd[help] and
@kwd[abort].  You can override these easily by specifying your own branches
that include these logical key-event tags.  The @kwd[help] branch displays in a
pop-up window the a description of the valid responses using the variously
specified help strings.  The @kwd[abort] branch signals an editor-error.

The @i[key]/@i[value] arguments control the prompting.  The following are valid
values:
@begin[description]
@kwd[help]@\
 The default @kwd[help] case displays this string in a pop-up window.  In
addition it formats a description of the valid input including each case's
@i[help] string.

@kwd[prompt]@\
 This is the prompt used when reading the key-event.

@kwd[change-window]@\
 If this is non-nil (the default), then the echo area window becomes the
current window while the prompting mechanism reads a key-event.  Sometimes it
is desirable to maintain the current window since it may be easier for users to
answer the question if they can see where the current point is.

@kwd[bind]@\
 This specifies a variable to which the prompting mechanism binds the input
key-event.  Any case may reference this variable.  If you wish to know what
character corresponds to the key-event, use @f[ext:key-event-char].
@end(description)

Instead of specifying a tag or list of tags, you may use @true.  This becomes
the default branch, and its forms execute if no other branch is taken,
including the default @kwd[help] and @kwd[abort] cases.  This option has no
@i[help] string, and the default @kwd[help] case does not describe the default
branch.  Every @f[command-case] has a default branch; if none is specified, the
macro includes one that @f[system:beep]'s and @f[reprompt]'s (see below).

Within the body of @f[command-case], there is a defined @f[reprompt] macro.
It causes the prompting mechanism and dispatching mechanism to immediately
repeat without further execution in the current branch.
@enddefmac


@defun[fun {prompt-for-key-event}, keys {[prompt][change-window]}]
This function prompts for a key-event returning immediately when the user types
the next key-event.  @macref[command-case] is more useful for most purposes.
When appropriate, use logical key-events @w<(page
@pageref[logical-key-events])>.
@enddefun

@defun[fun {prompt-for-key}, keys {[prompt][help][must-exist][default]},
	morekeys {[default-string]}]
 This function prompts for a @i[key], a vector of key-events, suitable for
passing to any of the functions that manipulate key bindings @w<(page
@pageref[key-bindings])>.  If @i[must-exist] is true, then the key must be
bound in the current environment, and the command currently bound is returned
as the second value.
@enddefun

@defun[fun {prompt-for-file}, keys {[prompt][help][must-exist][default]},
	morekeys {[default-string]}]
 This function prompts for an acceptable filename in some system dependent
fashion.  "Acceptable" means that it is a legal filename, and it exists if
@i[must-exist] is non-@nil.  @f[prompt-for-file] returns a Common Lisp
pathname.

If the file exists as entered, then this returns it, otherwise it is merged
with @i[default] as by @f[merge-pathnames].
@enddefun

@defun[fun {prompt-for-integer}, keys {[prompt][help][must-exist][default]},
	morekeys {[default-string]}] 
 This function prompts for a possibly signed integer.  If @i[must-exist] is
@nil, then @f[prompt-for-integer] returns the input as a string if it is not a
valid integer.
@enddefun

@defun[fun {prompt-for-keyword}, args {@i[string-tables]},
	keys {[prompt][help][must-exist]},
	morekeys {[default][default-string]}]
 This function prompts for a keyword with completion, using the string tables
in the list @i[string-tables].  If @I[must-exist] is non-@nil, then the result
must be an unambiguous prefix of a string in one of the @i[string-tables], and
the returns the complete string even if only a prefix of the full string was
typed.  In addition, this returns the value of the corresponding entry in the
string table as the second value.

If @i[must-exist] is @nil, then this function returns the string exactly as
entered.  The difference between @f[prompt-for-keyword] with @i[must-exist]
@nil, and @f[prompt-for-string], is the user may complete the input using the
@hid<Complete Parse> and @hid<Complete Field> commands.
@enddefun

@defun[fun {prompt-for-expression},
	keys {[prompt][help][must-exist][default]},
	morekeys {[default-string]}]
 This function reads a Lisp expression.  If @i[must-exist] is @nil, and a read
error occurs, then this returns the string typed.
@enddefun

@defun[fun {prompt-for-string}, keys 
{[prompt][help][default][default-string]}]
 This function prompts for a string; this cannot fail.
@enddefun

@defun[fun {prompt-for-variable}, keys {[prompt][help][must-exist][default]},
	morekeys {[default-string]}]
 This function prompts for a variable name.  If @i[must-exist] is non-@nil,
then the string must be a variable @i[defined in the current environment], in
which case the symbol name of the variable found is returned as the second
value.
@enddefun

@defun[fun {prompt-for-y-or-n}, keys {[prompt][help][must-exist][default]},
	morekeys {[default-string]}]
 This prompts for @binding[y], @binding[Y], @binding[n], or @binding[N],
returning @true or @nil without waiting for confirmation.  When the user types
a confirmation key, this returns @i[default] if it is supplied.  If
@i[must-exist] is @nil, this returns whatever key-event the user first types;
however, if the user types one of the above key-events, this returns @true or
@nil.  This is analogous to the Common Lisp function @f[y-or-n-p].
@enddefun

@defun[fun {prompt-for-yes-or-no}, keys {[prompt][help][must-exist][default]},
	morekeys {[default-string]}]
 This function is to @f[prompt-for-y-or-n] as @f[yes-or-no-p] is to
@f[y-or-n-p].  "Yes" or "No" must be typed out in full and
confirmation must be given.
@enddefun


@section(Control of Parsing Behavior)

@defhvar[var {Beep On Ambiguity}, val {@true}]
If this variable is true, then an attempt to complete a parse which is
ambiguous will result in a "beep".
@enddefhvar


@begin(comment)
@hemlock provides for limited control of parsing routine behaviour The
character attribute @hid[Parse Field Separator] is a boolean attribute, a value
of @f[1] indicating that the character is a field separator recognized by the
@hid<Complete Field> command.
@end(comment)

@begin(comment)
@defhvar[var {Help Show Options}]
During a keyword or similar parse, typing the help command may cause a
list of options to be displayed.  If displaying the help would take up
more lines than the value of this variable then confirmation will be
asked for before they will be displayed.
@enddefhvar
@end(comment)



@section(Defining New Prompting Functions)
Prompting functions are implemented as a recursive edit in the
@hid[Echo Area] buffer.  Completion, help, and other parsing features
are implemented by commands which are bound in @hid[Echo Area Mode].

A prompting function passes information down into the recursive edit
by binding a collection of special variables.

@defvar[var {parse-verification-function}]
The system binds this to a function that @comref[Confirm Parse] calls.  It does
most of the work when parsing prompted input.  @comref[Confirm Parse] passes
one argument, which is the string that was in @var<parse-input-region> when the
user invokes the command.  The function should return a list of values which
are to be the result of the recursive edit, or @nil indicating that the parse
failed.  In order to return zero values, a non-@nil second value may be
returned along with a @nil first value.
@enddefvar

@defvar[var {parse-string-tables}]
This is the list of @f[string-table]s, if any, that pertain to this parse.
@enddefvar

@defvar[var {parse-value-must-exist}]
This is bound to the value of the @kwd[must-exist] argument, and is
referred to by the verification function, and possibly some of the
commands.
@enddefvar

@defvar[var {parse-default}]
When prompting the user, this is bound to a string representing the default
object, the value supplied as the @kwd[default] argument.  @hid<Confirm Parse>
supplies this to the parse verification function when the
@var<parse-input-region> is empty.
@enddefvar

@defvar[var {parse-default-string}]
When prompting the user, if @var[parse-default] is @nil, @hemlock displays this
string as a representation of the default object; for example, when prompting
for a buffer, this variable would be bound to the buffer name.
@enddefvar

@defvar[var {parse-type}]
The kind of parse in progress, one of @kwd[file], @kwd[keyword] or
@kwd[string].  This tells the completion commands how to do completion, with
@kwd[string] disabling completion.
@enddefvar

@defvar[var {parse-prompt}]
The prompt being used for the current parse.
@enddefvar

@defvar[var {parse-help}]
The help string or function being used for the current parse.
@enddefvar

@defvar[var {parse-starting-mark}]
This variable holds a mark in the @varref[echo-area-buffer] which
is the position at which the parse began.
@enddefvar

@defvar[var {parse-input-region}]
This variable holds a region with @var[parse-starting-mark] as its
start and the end of the echo-area buffer as its end.  When
@hid[Confirm Parse] is called, the text in this region is the text
that will be parsed.
@enddefvar


@section(Some Echo Area Commands)

These are some of the @hid[Echo Area] commands that coordinate with the
prompting routines.  @Hemlock binds other commands specific to the @hid[Echo
Area], but they are uninteresting to mention here, such as deleting to the
beginning of the line or deleting backwards a word.

@defcom[com {Help On Parse},
	stuff (bound to @bf[Home, C-_] in @hid[Echo Area] mode)]
Display the help text for the parse currently in progress.
@enddefcom

@defcom[com {Complete Keyword},
	stuff (bound to @bf[Escape] in @hid[Echo Area] mode)] 
This attempts to complete the current region as a keyword in
@var[string-tables].  It signals an editor-error if the input is ambiguous
or incorrect.
@enddefcom

@defcom[com {Complete Field},
	stuff (bound to @bf[Space] in @hid[Echo Area] mode)]
Similar to @hid[Complete Keyword], but only attempts to complete up to and
including the first character in the keyword with a non-zero
@kwd[parse-field-separator] attribute.  If
there is no field separator then attempt to complete the entire keyword.
If it is not a keyword parse then just self-insert.
@enddefcom

@defcom[com {Confirm Parse},
	stuff (bound to @bf[Return] in @hid[Echo Area] mode)]
If @var[string-tables] is non-@nil find the string in the region in
them.  Call @var[parse-verification-function] with the current input.
If it returns a non-@nil value then that is returned as the value of
the parse.  A parse may return a @nil value if the verification
function returns a non-@nil second value.
@enddefcom



@chapter (Files)
@index (Files)
This chapter discusses ways to read and write files at various levels @dash at
marks, into regions, and into buffers.  This also treats automatic mechanisms
that affect the state of buffers in which files are read.

@section (File Options and Type Hooks)
@index (File options)
@index (Type hooks)
@index (File type hooks)
The user specifies file options with a special syntax on the first line of a
file.  If the first line contains the string "@f[-*-]", then @hemlock
interprets the text between the first such occurrence and the second, which
must be contained in one line , as a list of @w{"@f<@i[option]: @i[value]>"}
pairs separated by semicolons.  The following is a typical example:
@begin[programexample]
;;; -*- Mode: Lisp, Editor; Package: Hemlock -*-
@end[programexample]
See the @i[Hemlock User's Manual] for more details and predefined options.

File type hooks are executed when @hemlock reads a file into a buffer based on
the type of the pathname.  When the user specifies a @hid[Mode] file option
that turns on a major mode, @hemlock ignores type hooks.  This mechanism is
mostly used as a simple means for turning on some appropriate default major
mode.

@defmac[fun {define-file-option}, args
{@i[name] (@i[buffer] @i[value]) @mstar<@i[declaration]> @mstar<@i[form]>}]
This defines a new file option with the string name @i[name].  @i[Buffer] and
@i[value] specify variable names for the buffer and the option value string,
and @i[form]'s are evaluated with these bound.
@enddefmac

@defmac[fun {define-file-type-hook}, args 
{@i[type-list] (@i[buffer] @i[type]) @mstar<@i[declaration]> @mstar<@i[form]>}]

This defines some code that @f[process-file-options] (below) executes when the
file options fail to set a major mode.  This associates each type, a
@f[simple-string], in @i[type-list] with a routine that binds @i[buffer] to the
buffer the file is in and @i[type] to the type of the pathname.
@enddefmac

@defun[fun {process-file-options}, args {@i[buffer] @optional @i[pathname]}]
This checks for file options in buffer and invokes handlers if there are any.
@i[Pathname] defaults to @i[buffer]'s pathname but may be @nil.  If there is no
@hid[Mode] file option that specifies a major mode, and @i[pathname] has a
type, then this tries to invoke the appropriate file type hook.
@f[read-buffer-file] calls this.
@enddefun


@section (Pathnames and Buffers)
There is no good way to uniquely identify buffer names and pathnames.  However,
@hemlock has one way of mapping pathnames to buffer names that should be used
for consistency among customizations and primitives.  Independent of this,
@hemlock provides a means for consistently generating prompting defaults when
asking the user for pathnames.

@defun[fun {pathname-to-buffer-name}, args {@i[pathname]}]
This function returns a string of the form "@f[file-namestring]
@f[directory-namestring]".
@enddefun

@defhvar[var "Pathname Defaults", val {(pathname "gazonk.del")}]
@defhvar1[var "Last Resort Pathname Defaults Function"]
@defhvar1[var "Last Resort Pathname Defaults", val {(pathname "gazonk")}]
These variables control the computation of default pathnames when needed for
promting the user.  @hid[Pathname Defaults] is a @i[sticky] default.
See the @i[Hemlock User's Manual] for more details.
@enddefhvar

@defun[fun {buffer-default-pathname}, args {@i[buffer]}]
This returns @hid[Buffer Pathname] if it is bound.  If it is not bound, and
@i[buffer]'s name is composed solely of alphnumeric characters, then return a
pathname formed from @i[buffer]'s name.  If @i[buffer]'s name has other
characters in it, then return the value of @hid[Last Resort Pathname Defaults
Function] called on @i[buffer].
@enddefun

@section (File Groups)
@index (File groups)
File groups provide a simple way of collecting the files that compose a system
and naming that collection.  @Hemlock supports commands for searching,
replacing, and compiling groups.

@defvar[var {active-file-group}]
This is the list of files that constitute the currently selected file group.
If this is @nil, then there is no current group.
@enddefvar

@defmac[fun {do-active-group}, args {@mstar<@i[form]>}]
@defhvar1[var "Group Find File", val {nil}]
@defhvar1[var "Group Save File Confirm", val {t}]
@f[do-active-group] iterates over @var[active-file-group] executing the forms
once for each file.  While the forms are executing, the file is in the current
buffer, and the point is at the beginning.  If there is no active group, this
signals an editor-error.

This reads each file into its own buffer using @f[find-file-buffer].  Since
unwanted buffers may consume large amounts of memory, @hid[Group Find File]
controls whether to delete the buffer after executing the forms.  When the
variable is false, this deletes the buffer if it did not previously exist;
however, regardless of this variable, if the user leaves the buffer modified,
the buffer persists after the forms have completed.  Whenever this processes a
buffer that already existed, it saves the location of the buffer's point before
and restores it afterwards.  

After processing a buffer, if it is modified, @f[do-active-group] tries to save
it.  If @hid[Group Save File Confirm] is non-@nil, it asks for confirmation.
@enddefmac


@section (File Reading and Writing)
Common Lisp pathnames are used by the file primitives.  For probing, checking
write dates, and so forth, all of the Common Lisp file functions are available.

@defun[fun {read-file}, args {@i[pathname] @i[mark]}]
This inserts the file named by @i[pathname] at @i[mark].
@enddefun

@defun[fun {write-file}, args {@i[region] @i[pathname]},
	keys {[keep-backup][access][append]}]
@defhvar1[var {Keep Backup Files}, val {@nil}]
This function writes the contents of @i[region] to the file named by
@i[pathname].  This writes @i[region] using a stream as if it were opened with
@kwd[if-exists] supplied as @kwd[rename-and-delete].

When @i[keep-backup], which defaults to the value of @hid[Keep Backup Files],
is non-@nil, this opens the stream as if @kwd[if-exists] were @kwd[rename].  If
@i[append] is non-@nil, this writes the file as if it were opened with
@kwd[if-exists] supplied as @kwd[append].

This signals an error if both @i[append] and @i[keep-backup] are supplied as
non-@nil.

@i[Access] is an implementation dependent value that is suitable for setting
@i[pathname]'s access or protection bits.
@enddefun


@defun[fun {write-buffer-file}, args {@i[buffer] @i[pathname]}]
@defhvar1[var {Write File Hook}]
@defhvar1[var {Add Newline at EOF on Writing File}, val {@kwd[ask-user]}]
@f[write-buffer-file] writes @i[buffer] to the file named by @i[pathname]
including the following:
@begin[itemize]
It assumes pathname is somehow related to @i[buffer]'s pathname: if the
@i[buffer]'s write date is not the same as @i[pathname]'s, then this prompts
the user for confirmation before overwriting the file.

It consults @hid[Add Newline at EOF on Writing File] (see @i[Hemlock User's
Manual] for possible values) and interacts with the user if necessary.

It sets @hid[Pathname Defaults], and after using @f[write-file], marks
@i[buffer] unmodified.

It updates @i[Buffer]'s pathname and write date.

It renames the buffer according to the new pathname if possible.

It invokes @hid[Write File Hook].
@end[itemize]

@hid[Write File Hook] is a list of functions that take the newly written buffer
as an argument.
@enddefun


@defun[fun {read-buffer-file}, args {@i[pathname] @i[buffer]}]
@defhvar1[var {Read File Hook}]
@f[read-buffer-file] deletes @i[buffer]'s region and uses @f[read-file] to read
@i[pathname] into it, including the following:
@begin[itemize]
It sets @i[buffer]'s write date to the file's write date if the file exists;
otherwise, it @f[message]'s that this is a new file and sets @i[buffer]'s write
date to @nil.

It moves @i[buffer]'s point to the beginning.

It sets @i[buffer]'s unmodified status.

It sets @i[buffer]'s pathname to the result of probing @i[pathname] if the file
exists; otherwise, this function sets @i[buffer]'s pathname to the result of
merging @i[pathname] with @f[default-directory].

It sets @hid[Pathname Defaults] to the result of the previous item.

It processes the file options.

It invokes @hid[Read File Hook].
@end[itemize]

@hid[Read File Hook] is a list functions that take two arguments @dash the
buffer read into and whether the file existed, @true if so.
@enddefun


@defun[fun {find-file-buffer}, args {@i[pathname]}]
This returns a buffer assoicated with the @i[pathname], reading the file into a
new buffer if necessary.  This returns a second value indicating whether a new
buffer was created, @true if so.  If the file has already been read, this
checks to see if the file has been modified on disk since it was read, giving
the user various recovery options.  This is the basis of the @hid[Find File]
command.
@enddefun



@chapter (Hemlock's Lisp Environment)

@index (Lisp environment)
This chapter is sort of a catch all for any functions and variables
which concern @hemlock's interaction with the outside world.

@section(Entering and Leaving the Editor)

@defun[fun {ed}, args {@optional @i[x]}]
@defhvar1[var "Entry Hook"]
@f[ed] enters the editor.  It is basically as specified in Common Lisp.  If
@i[x] is supplied and is a symbol, the definition of @i[x] is put into a
buffer, and that buffer is selected.  If @i[x] is a pathname, the file
specified by @i[x] is visited in a new buffer.  If @i[x] is not supplied or
@nil, the editor is entered in the same state as when last exited.
	
The @hid[Entry Hook] is invoked each time the editor is entered.
@enddefhvar

@defun[fun {exit-hemlock}, args {@optional @i[value]}]
@defhvar1[var {Exit Hook}]
@f[exit-hemlock] leaves @hemlock and return to Lisp; @i[value] is the
value to return, which defaults to @true.  The hook 
@hvarref[Exit Hook] is invoked before this is done.
@enddefun

@defun[fun {pause-hemlock}]
@f[pause-hemlock] suspends the editor process and returns control to the shell.
When the process is resumed, it will still be running @hemlock.
@enddefun


@section(Keyboard Input)
@index(I/O)
@index[keyboard input]
@index[input, keyboard]

Keyboard input interacts with a number of other parts of the editor.  Since the
command loop works by reading from the keyboard, keyboard input is the initial
cause of everything that happens.  Also, @hemlock redisplays in the low-level
input loop when there is no available input from the user.


@defvar[var {editor-input}]
@defvar1[var {real-editor-input}]
@defhvar1[var "Input Hook"]
@defhvar1[var "Abort Hook"]
@index[aborting]
@var[editor-input] is an object on which @hemlock's I/O routines operate.  You
can get input, clear input, return input, and listen for input.  Input appears
as key-events.

@var[real-editor-input] holds the initial value of @var[editor-input].  This is
useful for reading from the user when @var[editor-input] is rebound (such as
within a keyboard macro.)

@Hemlock invokes the functions in @hid[Input Hook] each time someone reads a
key-event from @var[real-editor-input].  These take no arguments.
@enddefvar

@defun[fun {get-key-event}, args {@i[editor-input] @optional @i[ignore-abort-attempts-p]}]
This function returns a key-event as soon as it is available on
@i[editor-input].  @i[Editor-input] is either @var[editor-input] or
@var[real-editor-input].  @i[Ignore-abort-attempts-p] indicates whether
@binding[C-g] and @binding[C-G] throw to the editor's top-level command loop;
when this is non-nil, this function returns those key-events when the user
types them.  Otherwise, it aborts the editor's current state, returning to the
command loop.

When the user aborts, @Hemlock invokes the functions in @hid[Abort Hook].
These functions take no arguments.  When aborting, @Hemlock ignores the
@hid[Input Hook].
@enddefun


@defun[fun {unget-key-event}, args {@i[key-event] @i[editor-input]}]
This function returns @i[key-event] to @i[editor-input], so the next invocation
of @f[get-key-event] will return @i[key-event].  If @i[key-event] is
@f[#k"C-g"] or @f[#k"C-G"], then whether @f[get-key-event] returns it depends
on that function's second argument.  @i[Editor-input] is either
@var[editor-input] or @var[real-editor-input].
@enddefun

@defun[fun {clear-editor-input}, args {@i[editor-input]}]
This function flushes any pending input on @i[editor-input].  @i[Editor-input]
is either @var[editor-input] or @var[real-editor-input].
@enddefun

@defun[fun {listen-editor-input}, args {@i[editor-input]}]
This function returns whether there is any input available on @i[editor-input].
@i[Editor-input] is either @var[editor-input] or @var[real-editor-input].
@enddefun

@defun[fun {editor-sleep}, args {@i[time]}]
Return either after @i[time] seconds have elapsed or when input is available on
@var[editor-input].
@enddefun

@defvar[var {key-event-history}]
This is a @hemlock ring buffer (see page @pageref[rings]) that holds the last
60 key-events read from the keyboard.
@enddefvar

@defvar[var {last-key-event-typed}]
Commands use this variable to realize the last key-event the user typed to
invoke the commands.  Before @hemlock ever reads any input, the value is @nil.
This variable usually holds the last key-event read from the keyboard, but it
is also maintained within keyboard macros allowing commands to behave the same
on each repetition as they did in the recording invocation.
@enddefvar

@defvar[var {input-transcript}]
If this is non-@nil then it should be an adjustable vector with a fill-pointer.
When it is non-@nil, @hemlock pushes all input read onto this vector.
@enddefvar



@section(Hemlock Streams)
It is possible to create streams which output to or get input from a buffer.
This mechanism is quite powerful and permits easy interfacing of @hemlock to
Lisp.

@defun[fun {make-hemlock-output-stream}, args 
	{@i[mark] @optional @i[buffered]}]
@defun1[fun {hemlock-output-stream-p}, args {@i[object]}]
@f[make-hemlock-output-stream] returns a stream that inserts at the permanent
mark @i[mark] all output directed to it.  @i[Buffered] controls whether the
stream is buffered or not, and its valid values are the following keywords:
@begin[description]
@kwd[none]@\No buffering is done.  This is the default.

@kwd[line]@\The buffer is flushed whenever a newline is written or
when it is explicitly done with @f[force-output].

@kwd[full]@\The screen is only brought up to date when it is
explicitly done with @f[force-output]
@end[description]

@f[hemlock-output-stream-p] returns @true if @i[object] is a
@f[hemlock-output-stream] object.
@enddefun

@defun[fun {make-hemlock-region-stream}, args {@i[region]}]
@defun1[fun {hemlock-region-stream-p}, args {@i[object]}]
@f[make-hemlock-region-stream] returns a stream from which the text in
@i[region] can be read.  @f[hemlock-region-stream-p] returns @true if
@i[object] is a @f[hemlock-region-stream] object.
@enddefun

@defmac[fun {with-input-from-region}, args
{(@i[var] @i[region]) @mstar<@i[declaration]> @mstar<@i[form]>}]
While evaluating @i[form]s, binds @i[var] to a stream which returns input
from @i[region].
@enddefmac

@defmac[fun {with-output-to-mark}, args
{(@i[var] @i[mark] @mopt<@i"buffered">) @mstar<@i[declaration]> @mstar<@i[form]>}]
 During the evaluation of the @i[form]s, binds @i[var] to a stream which
inserts output at the permanent @i[mark].  @i[Buffered] has the same meaning as
for @f[make-hemlock-output-stream].
@enddefmac

@defmac[fun {with-pop-up-display}, args {(@i[var] @key @i[height name]) @mstar<@i[declaration]> @mstar<@i[form]>}]
@defvar1[var {random-typeout-buffers}]
 This macro executes @i[forms] in a context with @i[var] bound to a stream.
@Hemlock collects output to this stream and tries to pop up a display of the
appropriate height containing all the output.  When @i[height] is supplied,
@Hemlock creates the pop-up display immediately, forcing output on line breaks.
The system saves the output in a buffer named @i[name], which defaults to
@hid[Random Typeout].  When the window is the incorrect height, the display
mechanism will scroll the window with more-style prompting.  This is useful
for displaying information of temporary interest.

When a buffer with name @i[name] already exists and was not previously created
by @f[with-pop-up-display], @Hemlock signals an error.

@var[random-typeout-buffers] is an association list mapping random typeout
buffers to the streams that operate on the buffers.
@enddefmac


@section (Interface to the Error System)
The error system interface is minimal.  There is a simple editor-error
condition which is a subtype of error and a convenient means for signaling
them.  @Hemlock also provides a standard handler for error conditions while in
the editor.

@defun[fun {editor-error-format-string}, args {@i[condition]}]
@defun1[fun {editor-error-format-arguments}, args {@i[condition]}]
Handlers for editor-error conditions can access the condition object with
these.
@enddefun

@defun[fun {editor-error}, args {@rest @i[args]}]
This function is called to signal minor errors within Hemlock; these are errors
that a normal user could encounter in the course of editing such as a search
failing or an attempt to delete past the end of the buffer.  This function
@f[signal]'s an editor-error condition formed from @i[args], which are @nil or
a @f[format] string possibly followed by @f[format] arguments.  @Hemlock
invokes commands in a dynamic context with an editor-error condition handler
bound.  This default handler beeps or flashes (or both) the display.  If the
condition passed to the handler has a non-@nil string slot, the handler also
invokes @f[message] on it.  The command in progress is always aborted, and this
function never returns.
@enddefun

@defmac[fun {handle-lisp-errors}, args {@mstar<@i[form]>}]
Within the body of this macro any Lisp errors that occur are handled in some
fashion more gracefully than simply dumping the user in the debugger.  This
macro should be wrapped around code which may get an error due to some action
of the user @dash for example, evaluating code fragments on the behalf of and
supplied by the user.  Using this in a command allows the established handler
to shadow the default editor-error handler, so commands should take care to
signal user errors (calls to @f[editor-errors]) outside of this context.
@enddefmac


@section (Definition Editing)
@index (Definition editing)
@hemlock provides commands for finding the definition of a function, macro, or
command and placing the user at the definition in a buffer.  This, of course,
is implementation dependent, and if an implementation does not associate a
source file with a routine, or if @hemlock cannot get at the information, then
these commands do not work.  If the Lisp system does not store an absolute
pathname, independent of the machine on which the maintainer built the system,
then users need a way of translating a source pathname to one that will be able
to locate the source.

@defun[fun {add-definition-dir-translation}, args {@i[dir1] @i[dir2]}]
This maps directory pathname @i[dir1] to @i[dir2].  Successive invocations
using the same @i[dir1] push into a translation list.  When @hemlock seeks a
definition source file, and it has a translation, then it tries the
translations in order.  This is useful if your sources are on various machines,
some of which may be down.  When @hemlock tries to find a translation, it first
looks for translations of longer directory pathnames, finding more specific
translations before shorter, more general ones.
@enddefun

@defun[fun {delete-definition-dir-translation}, args {@i[dir]}]
This deletes the mapping of @i[dir] to all directories to which it has been
mapped.
@enddefun


@section (Event Scheduling)
@index (Event scheduling)
@index (Scheduling events)
The mechanism described in this chapter is only operative when the Lisp process
is actually running inside of @hemlock, within the @f[ed] function.  The
designers intended its use to be associated with the editor, such as with
auto-saving files, reminding the user, etc.

@defun[fun {schedule-event}, args {@i[time] @i[function] @optional @i[repeat]}]
This causes @hemlock to call @i[function] after @i[time] seconds have passed,
optionally repeating every @i[time] seconds.  @i[Repeat] defaults to @true.
This is a rough mechanism since commands can take an arbitrary amount of time
to run; @hemlock invokes @i[function] at the first possible moment after
@i[time] has elapsed.  @i[Function] takes the time in seconds that has elapsed
since the last time it was called (or since it was scheduled for the first
invocation).
@enddefun

@defun[fun {remove-scheduled-event}, args {@i[function]}]
This removes @i[function] from the scheduling queue.  @i[Function] does not
have to be in the queue.
@enddefun


@section (Miscellaneous)

@defun[fun {in-lisp}, args {@mstar<@i[form]>}]
@index[Evaluating Lisp code]
This evaluates @i[form]'s inside @f[handle-lisp-errors].  It also binds
@var[package] to the package named by @hid[Current Package] if it is non-@nil.
Use this when evaluating Lisp code on behalf of the user.
@enddefun

@defmac[fun {do-alpha-chars}, args {(@i[var] @i[kind] [@i[result]]) @mstar<@i[form]>}]
This iterates over alphabetic characters in Common Lisp binding @i[var] to each
character in order as specified under character relations in @i[Common Lisp the
Language].  @i[Kind] is one of @kwd[lower], @kwd[upper], or @kwd[both].  When
the user supplies @kwd[both], lowercase characters are processed first.
@enddefmac



@chapter (High-Level Text Primitives)
This chapter discusses primitives that operate on higher level text forms than
characters and words.  For English text, there are functions that know about
sentence and paragraph structures, and for Lisp sources, there are functions
that understand this language.  This chapter also describes mechanisms for
organizing file sections into @i[logical pages] and for formatting text forms.


@section (Indenting Text)
@index (Indenting)
@label(indenting)

@defhvar[var "Indent Function", val {tab-to-tab-stop}]
The value of this variable determines how indentation is done, and it is a
function which is passed a mark as its argument.  The function should indent
the line that the mark points to.  The function may move the mark around on
the line.  The mark will be @f[:left-inserting].  The default simply inserts a
@binding[tab] character at the mark.  A function for @hid[Lisp] mode probably
moves the mark to the beginning of the line, deletes horizontal whitespace, and
computes some appropriate indentation for Lisp code.
@enddefhvar

@defhvar[var "Indent with Tabs", val {indent-using-tabs}]
@defhvar1[var "Spaces per Tab", val {8}]
@hid[Indent with Tabs] holds a function that takes a mark and a number of
spaces.  The function will insert a maximum number of tabs and a minimum number
of spaces at mark to move the specified number of columns.  The default
definition uses @hid[Spaces per Tab] to determine the size of a tab.  @i[Note,]
@hid[Spaces per Tab] @i[is not used everywhere in @hemlock yet, so changing
this variable could have unexpected results.]
@enddefhvar

@defun[fun {indent-region}, args {@i[region]}]
@defun1[fun {indent-region-for-commands}, args {@i[region]}]
@f[indent-region] invokes the value of @hid[Indent Function] on every line of
region.  @f[indent-region-for-commands] uses @f[indent-region] but first saves
the region for the @hid[Undo] command.
@enddefun

@defun[fun {delete-horizontal-space}, args {@i[mark]}]
This deletes all characters with a @hid[Space] attribute (see section
@ref[sys-def-chars]) of @f[1].
@enddefun


@section (Lisp Text Buffers)
@index (Lisp text functions)
@hemlock bases its Lisp primitives on parsing a block of the buffer and
annotating lines as to what kind of Lisp syntax occurs on the line or what kind
of form a mark might be in (for example, string, comment, list, etc.).  These
do not work well if the block of parsed forms is exceeded when moving marks
around these forms, but the block that gets parsed is somewhat programmable.

There is also a notion of a @i[top level form] which this documentation often
uses synonymously with @i[defun], meaning a Lisp form occurring in a source
file delimited by parentheses with the opening parenthesis at the beginning of
some line.  The names of the functions include this inconsistency.

@defun[fun {pre-command-parse-check}, args {@i[mark] @i[for-sure]}]
@defhvar1[var {Parse Start Function}, val {start-of-parse-block}]
@defhvar1[var {Parse End Function}, val {end-of-parse-block}]
@defhvar1[var {Minimum Lines Parsed}, val {50}]
@defhvar1[var {Maximum Lines Parsed}, val {500}]
@defhvar1[var {Defun Parse Goal}, val {2}]
@f[pre-command-parse-check] calls @hid[Parse Start Function] and @hid[Parse End
Function] on @i[mark] to get two marks.  It then parses all the lines between
the marks including the complete lines they point into.  When @i[for-sure] is
non-@nil, this parses the area regardless of any cached information about the
lines.  Every command that uses the following routines calls this before doing
so.

The default values of the start and end variables use @hid[Minimum Lines
Parsed], @hid[Maximum Lines Parsed], and @hid[Defun Parse Goal] to determine
how big a region to parse.  These two functions always include at least the
minimum number of lines before and after the mark passed to them.  They try to
include @hid[Defun Parse Goal] number of top level forms before and after the
mark passed them, but these functions never return marks that include more than
the maximum number of lines before or after the mark passed to them.
@enddefun

@defun[fun {form-offset}, args {@i[mark] @i[count]}]
This tries to move @i[mark] @i[count] forms forward if positive or -@i[count]
forms backwards if negative.  @i[Mark] is always moved.  If there were enough
forms in the appropriate direction, this returns @i[mark], otherwise nil.
@enddefun

@defun[fun {top-level-offset}, args {@i[mark] @i[count]}]
This tries to move @i[mark] @i[count] top level forms forward if positive or
-@i[count] top level forms backwards if negative.  If there were enough top
level forms in the appropriate direction, this returns @i[mark], otherwise nil.
@i[Mark] is moved only if this is successful.
@enddefun

@defun[fun {mark-top-level-form}, args {@i[mark1] @i[mark2]}]
This moves @i[mark1] and @i[mark2] to the beginning and end, respectively, of
the current or next top level form.  @i[Mark1] is used as a reference to start
looking.  The marks may be altered even if unsuccessful.  If successful, return
@i[mark2], else nil.  @i[Mark2] is left at the beginning of the line following
the top level form if possible, but if the last line has text after the closing
parenthesis, this leaves the mark immediately after the form.
@enddefun

@defun[fun {defun-region}, args {@i[mark]}]
This returns a region around the current or next defun with respect to
@i[mark].  @i[Mark] is not used to form the region.  If there is no appropriate
top level form, this signals an editor-error.  This calls
@f[pre-command-parse-check] first.
@enddefun

@defun[fun {inside-defun-p}, args {@i[mark]}]
@defun1[fun {start-defun-p}, args {@i[mark]}]
These return, respectively, whether @i[mark] is inside a top level form or at
the beginning of a line immediately before a character whose @hid[Lisp Syntax]
(see section @ref[sys-def-chars]) value is @kwd[opening-paren].
@enddefun

@defun[fun {forward-up-list}, args {@i[mark]}]
@defun1[fun {backward-up-list}, args {@i[mark]}]
Respectively, these move @i[mark] immediately past a character whose @hid[Lisp
Syntax] (see section @ref[sys-def-chars]) value is @kwd[closing-paren] or
immediately before a character whose @hid[Lisp Syntax] value is
@kwd[opening-paren].
@enddefun

@defun[fun {valid-spot}, args {@i[mark] @i[forwardp]}]
This returns @true or @nil depending on whether the character indicated by
@i[mark] is a valid spot.  When @i[forwardp] is set, use the character after
mark and vice versa.  Valid spots exclude commented text, inside strings, and
character quoting.
@enddefun

@defun[fun {defindent}, args {@i[name] @i[count]}]
This defines the function with @i[name] to have @i[count] special arguments.
@f[indent-for-lisp], the value of @hid[Indent Function] (see section
@ref[indenting]) in @hid[Lisp] mode, uses this to specially indent these
arguments.  For example, @f[do] has two, @f[with-open-file] has one, etc.
There are many of these defined by the system including definitions for special
@hemlock forms.  @i[Name] is a simple-string, case insensitive and purely
textual (that is, not read by the Lisp reader); therefore, @f["with-a-mumble"]
is distinct from @f["mumble:with-a-mumble"].
@enddefun


@section (English Text Buffers)
@index (English text functions)
@label(text-functions)
This section describes some routines that understand basic English language
forms.

@defun[fun {word-offset}, args {@i[mark] @i[count]}]
This moves @i[mark] @i[count] words forward (if positive) or backwards (if
negative).  If @i[mark] is in the middle of a word, that counts as one.  If
there were @i[count] (-@i[count] if negative) words in the appropriate
direction, this returns @i[mark], otherwise nil.  This always moves @i[mark].
A word lies between two characters whose @hid[Word Delimiter] attribute value
is @f[1] (see section @ref[sys-def-chars]).
@enddefun

@defun[fun {sentence-offset}, args {@i[mark] @i[count]}]
This moves @i[mark] @i[count] sentences forward (if positive) or backwards (if
negative).  If @i[mark] is in the middle of a sentence, that counts as one.  If
there were @i[count] (-@i[count] if negative) sentences in the appropriate
direction, this returns @i[mark], otherwise nil.  This always moves @i[mark].

A sentence ends with a character whose @hid[Sentence Terminator] attribute is
@f[1] followed by two spaces, a newline, or the end of the buffer.  The
terminating character is optionally followed by any number of characters whose
@hid[Sentence Closing Char] attribute is @f[1].  A sentence begins after a
previous sentence ends, at the beginning of a paragraph, or at the beginning of
the buffer.
@enddefun

@defun[fun {paragraph-offset}, args {@i[mark] @i[count] @optional @i[prefix]}]
@defhvar1[var {Paragraph Delimiter Function}, var {default-para-delim-function}]
This moves @i[mark] @i[count] paragraphs forward (if positive) or backwards (if
negative).  If @i[mark] is in the middle of a paragraph, that counts as one.
If there were @i[count] (-@i[count] if negative) paragraphs in the appropriate
direction, this returns @i[mark], otherwise nil.  This only moves @i[mark] if
there were enough paragraphs.

@hid[Paragraph Delimiter Function] holds a function that takes a mark,
typically at the beginning of a line, and returns whether or not the current
line should break the paragraph.  @f[default-para-delim-function] returns @true
if the next character, the first on the line, has a @hid[Paragraph Delimiter]
attribute value of @f[1].  This is typically a space, for an indented
paragraph, or a newline, for a block style.  Some modes require a more
complicated determinant; for example, @hid[Scribe] modes adds some characters
to the set and special cases certain formatting commands.

@i[Prefix] defaults to @hid[Fill Prefix] (see section @ref[filling]), and the
right prefix is necessary to correctly skip paragraphs.  If @i[prefix] is
non-@nil, and a line begins with @i[prefix], then the scanning process skips
the prefix before invoking the @hid[Paragraph Delimiter Function].
Note, when scanning for paragraph bounds, and @i[prefix] is non-@nil, lines are
potentially part of the paragraph regardless of whether they contain the prefix;
only the result of invoking the delimiter function matters.

The programmer should be aware of an idiom for finding the end of the current
paragraph.  Assume @f[paragraphp] is the result of moving @f[mark] one
paragraph, then the following correctly determines whether there actually is a
current paragraph:
@begin[programexample]
(or paragraphp
    (and (last-line-p mark)
         (end-line-p mark)
	 (not (blank-line-p (mark-line mark)))))
@end[programexample]
In this example @f[mark] is at the end of the last paragraph in the buffer, and
there is no last newline character in the buffer.  @f[paragraph-offset] would
have returned @nil since it could not skip any paragraphs since @f[mark] was at
the end of the current and last paragraph.  However, you still have found a
current paragraph on which to operate.  @f[mark-paragraph] understands this
problem.
@enddefun

@defun[fun {mark-paragraph}, args {@f[mark1] @f[mark2]}]
This marks the next or current paragraph, setting @i[mark1] to the beginning
and @i[mark2] to the end.  This uses @hid[Fill Prefix] (see section
@ref[filling]).  @i[Mark1] is always on the first line of the paragraph,
regardless of whether the previous line is blank.  @i[Mark2] is typically at
the beginning of the line after the line the paragraph ends on, this returns
@i[mark2] on success.  If this cannot find a paragraph, then the marks are left
unmoved, and @nil is returned.
@enddefun


@section (Logical Pages)
@index (Logical pages)
@index (Page functions)
@label(logical-pages)
Logical pages are a way of dividing a file into coarse divisions.  This is
analogous to dividing a paper into sections, and @hemlock provides primitives
for moving between the pages of a file and listing a directory of the page
titles.  Pages are separated by @hid[Page Delimiter] characters (see section
@ref[sys-def-chars]) that appear at the beginning of a line.

@defun[fun {goto-page}, args {@i[mark] @i[n]}]
This moves @i[mark] to the absolute page numbered @i[n].  If there are less
than @i[n] pages, it signals an editor-error.  If it returns, it returns
@i[mark].  @hemlock numbers pages starting with one for the page delimited by
the beginning of the buffer and the first @hid[Page Delimiter] (or the end of
the buffer).
@enddefun

@defun[fun {page-offset}, args {@i[mark] @i[n]}]
This moves mark forward @i[n] (-@i[n] backwards, if @i[n] is negative)
@hid[Page Delimiter] characters that are in the zero'th line position.  If a
@hid[Page Delimiter] is the immediately next character after mark (or before
mark, if @i[n] is negative), then skip it before starting.  This always moves
@i[mark], and if there were enough pages to move over, it returns @i[mark];
otherwise, it returns @nil.
@enddefun

@defun[fun {page-directory}, args {@i[buffer]}]
This returns a list of each first non-blank line in @i[buffer] that follows a
@hid[Page Delimiter] character that is in the zero'th line position.  This
includes the first line of the @i[buffer] as the first page title.  If a page
is empty, then its title is the empty string.
@enddefun

@defun[fun {display-page-directory}, args {@i[stream] @i[directory]}]
This writes the list of strings, @i[directory], to @i[stream], enumerating them
in a field three wide.  The number and string are separated by two spaces, and
the first line contains headings for the page numbers and title strings.
@enddefun


@section (Filling)
@index (filling)
@label(filling)
Filling is an operation on text that breaks long lines at word boundaries
before a given column and merges shorter lines together in an attempt to make
each line roughly the specified length.  This is different from justification
which tries to add whitespace in awkward places to make each line exactly the
same length.  @Hemlock's filling optionally inserts a specified string at the
beginning of each line.  Also, it eliminates extra whitespace between lines and
words, but it knows two spaces follow sentences (see section
@ref[text-functions]).

@defhvar[var "Fill Column", val {75}]
@defhvar1[var "Fill Prefix", val {nil}]
These variables hold the default values of the prefix and column arguments to
@hemlock's filling primitives.  If @hid[Fill Prefix] is @nil, then there is no
fill prefix.
@enddefhvar

@defun[fun {fill-region}, args {@i[region] @optional @i[prefix] @i[column]}]
This deletes any blank lines in region and fills it according to prefix and
column.  @i[Prefix] and @i[column] default to @hid[Fill Prefix] and @hid[Fill
Column].
@enddefun

@defun[fun {fill-region-by-paragraphs},
	args {@i[region] @optional @i[prefix] @i[column]}]
This finds paragraphs (see section @ref[text-functions]) within region and
fills them with @f[fill-region].  This ignores blank lines between paragraphs.
@i[Prefix] and @i[column] default to @hid[Fill Prefix] and @hid[Fill Column].
@enddefun



@chapter (Utilities)
@index (Utilities)
This chapter describes a number of utilities for manipulating some types of
objects @hemlock uses to record information.  String-tables are used to store
names of variables, commands, modes, and buffers.  Ring lists can be used to
provide a kill ring, recent command history, or other user-visible features.


@section(String-table Functions)
@index (String-tables)
@label(string-tables)

String tables are similar to Common Lisp hash tables in that they associate a
value with an object.  There are a few useful differences: in a string table
the key is always a case insensitive string, and primitives are provided to
facilitate keyword completion and recognition.  Any type of string may be added
to a string table, but the string table functions always return
@f[simple-string]'s.

A string entry in one of these tables may be thought of as being separated into
fields or keywords.  The interface provides keyword completion and recognition
which is primarily used to implement some @hid[Echo Area] commands.  These
routines perform a prefix match on a field-by-field basis allowing the
ambiguous specification of earlier fields while going on to enter later fields.
While string tables may use any @f[string-char] as a separator, the use of
characters other than @binding[space] may make the @hid[Echo Area] commands
fail or work unexpectedly.

@defun[fun {make-string-table}, keys {[separator][initial-contents]}]
This function creates an empty string table that uses @i[separator] as the
character, which must be a @f[string-char], that distinguishes fields.
@i[Initial-contents] specifies an initial set of strings and their values in
the form of a dotted @f[a-list], for example:
@Begin[ProgramExample]
'(("Global" . t) ("Mode" . t) ("Buffer" . t))
@End[ProgramExample]
@enddefun

@defun[fun {string-table-p}, args {@i[string-table]}]
This function returns @true if @i[string-table] is a @f[string-table] object,
otherwise @nil.
@enddefun

@defun[fun {string-table-separator}, args {@i[string-table]}]
This function returns the separator character given to @f[make-string-table].
@enddefun

@defun[fun {delete-string}, args {@i[string] @i[table]}]
@defun1[fun {clrstring}, args {@i[table]}]
@f[delete-string] removes any entry for @i[string] from the @f[string-table]
@i[table], returning @true if there was an entry.  @f[clrstring] removes all
entries from @i[table].
@enddefun

@defun[fun {getstring}, args {@i[string] @i[table]}]
This function returns as multiple values, first the value corresponding to the
string if it is found and @nil if it isn't, and second @true if it is found and
@nil if it isn't.

This may be set with @f[setf] to add a new entry or to store a new value for a
string.  It is an error to try to insert a string with more than one
field separator character occurring contiguously.
@enddefun

@defun[fun {complete-string}, args {@i[string] @i[tables]}]
This function completes @i[string] as far as possible over the list of
@i[tables], returning five values.  It is an error for @i[tables] to have
different separator characters.  The five return values are as follows:
@begin[itemize]
The maximal completion of the string or @nil if there is none.

An indication of the usefulness of the returned string:
@begin[description]
@kwd[none]@\
There is no completion of @i[string].

@kwd[complete]@\
The completion is a valid entry, but other valid completions exist too.  This
occurs when the supplied string is an entry as well as initial substring of
another entry.

@kwd[unique]@\
The completion is a valid entry and unique.

@kwd[ambiguous]@\
The completion is invalid; @f[get-string] would return @nil and @nil if given
the returned string.
@end[description]

The value of the string when the completion is @kwd[unique] or @kwd[complete],
otherwise @nil.

An index, or nil, into the completion returned, indicating where the addition
of a single field to @i[string] ends.  The command @hid[Complete Field] uses
this when the completion contains the addition to @i[string] of more than one
field.

An index to the separator following the first ambiguous field when the
completion is @kwd[ambiguous] or @kwd[complete], otherwise @nil.
@end[itemize]
@enddefun

@defun[fun {find-ambiguous}, args {@i[string] @i[table]}]
@defun1[fun {find-containing}, args {@i[string] @i[table]}]
@f[find-ambiguous] returns a list in alphabetical order of all the
strings in @i[table] matching @i[string].  This considers an entry as matching
if each field in @i[string], taken in order, is an initial substring of the
entry's fields; entry may have fields remaining.
 
@f[find-containing] is similar, but it ignores the order of the fields in
@i[string], returning all strings in @i[table] matching any permutation of the
fields in @i[string].
@enddefun

@defmac[fun {do-strings}, args {(@i[string-var] @i[value-var] @i[table] @MOPT<@i[result]>) @mstar<@i[declaration]> @mstar<@i[tag] @MOR @i[statement]>}]
This macro iterates over the strings in @i[table] in alphabetical order.  On
each iteration, it binds @i[string-var] to an entry's string and @i[value-var]
to an entry's value.
@enddefmac


@section (Ring Functions)
@index (Rings)
@label[rings]
There are various purposes in an editor for which a ring of values can be used,
so @hemlock provides a general ring buffer type.  It is used for maintaining a
ring of killed regions (see section @ref[kill-ring]), a ring of marks (see
section @ref[mark-stack]), or a ring of command strings which various modes and
commands maintain as a history mechanism.

@defun[fun {make-ring}, args {@i[length] @optional @i[delete-function]}]
Makes an empty ring object capable of holding up to @i[length] Lisp objects.
@i[Delete-function] is a function that each object is passed to before it falls
off the end.  @i[Length] must be greater than zero.
@enddefun

@defun[fun {ringp}, args {@i[ring]}]
Returns @true if @i[ring] is a @f[ring] object, otherwise @nil.
@enddefun

@defun[fun {ring-length}, args {@i[ring]}]
Returns as multiple-values the number of elements which @i[ring]
currently holds and the maximum number of elements which it may hold.
@enddefun

@defun[fun {ring-ref}, args {@i[ring] @i[index]}]
Returns the @i[index]'th item in the @i[ring], where zero is the index
of the most recently pushed.  This may be set with @f[setf].
@enddefun

@defun[fun {ring-push}, args {@i[object] @i[ring]}]
Pushes @i[object] into @i[ring], possibly causing the oldest item to
go away.
@enddefun

@defun[fun {ring-pop}, args {@i[ring]}]
Removes the most recently pushed object from @i[ring] and returns it.
If the ring contains no elements then an error is signalled.
@enddefun

@defun[fun {rotate-ring}, args {@i[ring] @i[offset]}]
With a positive @i[offset], rotates @i[ring] forward that many times.
In a forward rotation the index of each element is reduced by one,
except the one which initially had a zero index, which is made the
last element.  A negative offset rotates the ring the other way.
@enddefun


@section (Undoing commands)
@index (Undo functions)
@label(undo)

@defun[fun {save-for-undo}, args {@i[name] @i[method] @optional @i[cleanup] @i[method-undo] @i[buffer]}]
This saves information to undo a command.  @i[Name] is a string to display when
prompting the user for confirmation when he invokes the @hid[Undo] command (for
example, @f["kill"] or @f["Fill Paragraph"]).  @i[Method] is the function to
invoke to undo the effect of the command.  @i[Method-undo] is a function that
undoes the undo function, or effectively re-establishes the state immediately
after invoking the command.  If there is any existing undo information, this
invokes the @i[cleanup] function; typically @i[method] closes over or uses
permanent marks into a buffer, and the @i[cleanup] function should delete such
references.  @i[Buffer] defaults to the @f[current-buffer], and the @hid[Undo]
command only invokes undo methods when they were saved for the buffer that is
current when the user invokes @hid[Undo].
@enddefun

@defun[fun {make-region-undo}, args {@i[kind] @i[name] @i[region] @optional @i[mark-or-region]}]
This handles three common cases that commands fall into when setting up undo
methods, including cleanup and method-undo functions (see @f[save-for-undo]).
These cases are indicated by the @i[kind] argument:
@begin[description]
@kwd[twiddle]@\
Use this kind when a command modifies a region, and the undo information
indicates how to swap between two regions @dash the one before any modification
occurs and the resulting region.  @i[Region] is the resulting region, and it
has permanent marks into the buffer.  @i[Mark-or-region] is a region without
marks into the buffer (for example, the result of @f[copy-region]).  As a
result of calling this, a first invocation of @hid[Undo] deletes @i[region],
saving it, and inserts @i[mark-or-region] where @i[region] used to be.  The
undo method sets up for a second invocation of @hid[Undo] that will undo the
effect of the undo; that is, after two calls, the buffer is exactly as it was
after invoking the command.  This activity is repeatable any number of times.
This establishes a cleanup method that deletes the two permanent marks into the
buffer used to locate the modified region.

@kwd[insert]@\
Use this kind when a command has deleted a region, and the undo information
indicates how to re-insert the region.  @i[Region] is the deleted and saved
region, and it does not contain marks into any buffer.  @i[Mark-or-region] is a
permanent mark into the buffer where the undo method should insert @i[region].
As a result of calling this, a first invocation of @hid[Undo] inserts
@i[region] at @i[mark-or-region] and forms a region around the inserted text
with permanent marks into the buffer.  This allows a second invocation of
@hid[Undo] to undo the effect of the undo; that is, after two calls, the buffer
is exactly as it was after invoking the command.  This activity is repeatable
any number of times.  This establishes a cleanup method that deletes either the
permanent mark into the buffer or the two permanent marks of the region,
depending on how many times the user used @hid[Undo].

@kwd[delete]@\
Use this kind when a command has inserted a block of text, and the undo
information indicates how to delete the region.  @i[Region] has permanent marks
into the buffer and surrounds the inserted text.  Leave @i[Mark-or-region]
unspecified.  As a result of calling this, a first invocation of @hid[Undo]
deletes @i[region], saving it, and establishes a permanent mark into the buffer
to remember where the @i[region] was.  This allows a second invocation of
@hid[Undo] to undo the effect of the undo; that is, after two calls, the buffer
is exactly as it was after invoking the command.  This activity is repeatable
any number of times.  This establishes a cleanup method that deletes either the
permanent mark into the buffer or the two permanent marks of the region,
depending on how many times the user used @hid[Undo].
@end[description]

@blankspace(1 line)
@i[Name] in all cases is an appropriate string indicating what the command did.
This is used by @hid[Undo] when prompting the user for confirmation before
calling the undo method.  The string used by @hid[Undo] alternates between this
argument and something to indicate that the user is undoing an undo.
@enddefun



@chapter (Miscellaneous)
This chapter is somewhat of a catch-all for comments and features that don't
fit well anywhere else.


@section (Generic Pointer Up)
@hid[Generic Pointer Up] is a @hemlock command bound to mouse up-clicks.  It
invokes a function supplied with the interface described in this section.  This
command allows different commands to be bound to the same down-click in various
modes with one command bound to the corresponding up-click.

@defun[fun {supply-generic-pointer-up-function}, args {@i[function]}]
@index[Generic Pointer Up]
This function supplies a function that @hid[Generic Pointer Up] invokes the
next time it executes.
@enddefun


@section (Using View Mode)
@hid[View] mode supports scrolling through files automatically terminating the
buffer at end-of-file as well as commands for quitting the mode and popping
back to the buffer that spawned the @hid[View] mode buffer.  Modes such as
@hid[Dired] and @hid[Lisp-Lib] use this to view files and description of
library entries.

Modes that want similar commands should use @f[view-file-command] to view a
file and get a handle on the view buffer.  To allow the @hid[View Return] and
@hid[View Quit] commands to return to the originating buffer, you must set the
variable @hid[View Return Function] in the viewing buffer to a function that
knows how to do this.  Furthermore, since you now have a reference to the
originating buffer, you must add a buffer local delete hook to it that will
clear the view return function's reference.  This needs to happen for two
reasons in case the user deletes the originating buffer:
@Begin[Enumerate]
You don't want the return function to go to a non-existing, invalid buffer.

Since the viewing buffer still exists, its @hid[View Return Function] buffer
local variable still exists.  This means the function still references the
deleted originating buffer, and garbage collection cannot reclaim the memory
locked down by the deleted buffer.
@End[Enumerate]

The following is a piece of code that could implement part of @hid[Dired View
File] that uses two closures to accomplish that described above:
@Begin[ProgramExample]
(let* ((dired-buf (current-buffer))
       (buffer (view-file-command nil pathname)))
  (push #'(lambda (buffer)
	    (declare (ignore buffer))
	    (setf dired-buf nil))
	(buffer-delete-hook dired-buf))
  (setf (variable-value 'view-return-function :buffer buffer)
	#'(lambda ()
	    (if dired-buf
		(change-to-buffer dired-buf)
		(dired-from-buffer-pathname-command nil)))))
@End[ProgramExample]

The @hid[Dired] buffer's delete hook clears the return function's reference to
the @hid[Dired] buffer.  The return function tests the variable to see if it
still holds a buffer when the function executes.



@comment[@chapter (Auxiliary Systems)]
@include(aux-sys)
