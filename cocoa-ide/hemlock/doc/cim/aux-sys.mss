@comment{-*- Dictionary: /usr/lisp/scribe/hem/hem; Mode: spell; Package: Hemlock; Log: /usr/lisp/scribe/hem/hem-docs.log -*-}
@chapter (Auxiliary Systems)
This chapter describes utilities that some implementations of @hemlock may
leave unprovided or unsupported.


@section[Key-events]
@index(I/O)
@index[keyboard input]
@index[input, keyboard]
@index[mouse input]
@index[input, mouse]
@label[key-events]

These routines are defined in the @f["EXTENSIONS"] package since other projects
have often used @hemlock's input translations for interfacing to CLX.


@subsection[Introduction]

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



@subsection[Interface]

All of the following routines and variables are exported from the "EXTENSIONS"
("EXT") package.


@defun[fun {define-keysym}, args {@i[keysym] @i[preferred-name] @rest @i[other-names]}]
This function establishes a mapping from @i[preferred-name] to @i[keysym] for
purposes of @f[#k] syntax.  @i[Other-names] also map to @i[keysym], but the
system uses @i[preferred-name] when printing key-events.  The names are
case-insensitive simple-strings; however, if the string contains a single
character, then it is used case-sensitively.  Redefining a keysym or re-using
names has undefined effects.

You can use this to define unused keysyms, but primarily this defines keysyms
defined in the @i[X Window System Protocol, MIT X Consortium Standard, X
Version 11, Release 4].  @f[translate-key-event] uses this knowledge to
determine what keysyms are modifier keysyms and what keysym stand for
alphabetic key-events.
@enddefun


@defun[fun {define-mouse-keysym}, args {@i[button] @i[keysym] @i[name] @i[shifted-bit] @i[event-key]}]
This function defines @i[keysym] named @i[name] for key-events representing the
X @i[button] cross the X @i[event-key] (@kwd[button-press] or
@kwd[button-release]).  @i[Shifted-bit] is a defined modifier name that
@f[translate-mouse-key-event] sets in the key-event it returns whenever the X
shift bit is set in an incoming event.

Note, by default, there are distinct keysyms for each button distinguishing
whether the user pressed or released the button.

@i[Keysym] should be an one unspecified in @i[X Window System Protocol, MIT X
Consortium Standard, X Version 11, Release 4].
@enddefun


@defun[fun {name-keysym}, args {@i[name]}]
This function returns the keysym named @i[name].  If @i[name] is unknown, this
returns @nil.
@enddefun

@defun[fun {keysym-names}, args {@i[keysym]}]
This function returns the list of all names for @i[keysym].  If @i[keysym] is
undefined, this returns @nil.
@enddefun

@defun[fun {keysym-preferred-name}, args {@i[keysym]}]
This returns the preferred name for @i[keysym], how it is typically printed.
If @i[keysym] is undefined, this returns @nil.
@enddefun

@defun[fun {define-key-event-modifier}, args {@i[long-name] @i[short-name]}]
This establishes @i[long-name] and @i[short-name] as modifier names for
purposes of specifying key-events in @f[#k] syntax.  The names are
case-insensitive simple-strings.  If either name is already defined, this
signals an error.

The system defines the following default modifiers (first the long name,
then the short name):
@begin[Itemize]
@f["Hyper"], @f["H"]

@f["Super"], @f["S"]

@f["Meta"], @f["M"]

@f["Control"], @f["C"]

@f["Shift"], @f["Shift"]

@f["Lock"], @f["Lock"]
@end[Itemize]
@enddefun


@defvar[var {all-modifier-names}]
This variable holds all the defined modifier names.
@enddefvar


@defun[fun {define-clx-modifier}, args {@i[clx-mask] @i[modifier-name]}]
This function establishes a mapping from @i[clx-mask] to a defined key-event
@i[modifier-name].  @f[translate-key-event] and @f[translate-mouse-key-event]
can only return key-events with bits defined by this routine.

The system defines the following default mappings between CLX modifiers and
key-event modifiers:
@begin[Itemize]
@f[(xlib:make-state-mask :mod-1)    -->  "Meta"]

@f[(xlib:make-state-mask :control)  -->  "Control"]

@f[(xlib:make-state-mask :lock)     -->  "Lock"]

@f[(xlib:make-state-mask :shift)    -->  "Shift"]
@end[Itemize]
@enddefun


@defun[fun {make-key-event-bits}, args {@rest @i[modifier-names]}]
This function returns bits suitable for @f[make-key-event] from the supplied
@i[modifier-names].  If any name is undefined, this signals an error.
@enddefun

@defun[fun {key-event-modifier-mask}, args {@i[modifier-name]}]
This function returns a mask for @i[modifier-name].  This mask is suitable for
use with @f[key-event-bits].  If @i[modifier-name] is undefined, this signals
an error.
@enddefun

@defun[fun {key-event-bits-modifiers}, args {@i[bits]}]
This returns a list of key-event modifier names, one for each modifier
set in @i[bits].
@enddefun


@defun[fun {translate-key-event}, args {@i[display] @i[scan-code] @i[bits]}]
This function translates the X @i[scan-code] and X @i[bits] to a key-event.
First this maps @i[scan-code] to an X keysym using @f[xlib:keycode->keysym]
looking at @i[bits] and supplying index as @f[1] if the X shift bit is on,
@f[0] otherwise.

If the resulting keysym is undefined, and it is not a modifier keysym,
then this signals an error.  If the keysym is a modifier key, then this
returns @nil.

If these conditions are satisfied
@begin[Itemize]
The keysym is defined.

The X shift bit is off.

The X lock bit is on.

The X keysym represents a lowercase letter.
@end[Itemize]
then this maps the @i[scan-code] again supplying index as @f[1] this time,
treating the X lock bit as a caps-lock bit.  If this results in an undefined
keysym, this signals an error.  Otherwise, this makes a key-event with the
keysym and bits formed by mapping the X bits to key-event bits.

Otherwise, this makes a key-event with the keysym and bits formed by
mapping the X bits to key-event bits.
@enddefun


@defun[fun {translate-mouse-key-event}, args {@i[scan-code] @i[bits] @i[event-key]}]
This function translates the X button code, @i[scan-code], and modifier bits,
@i[bits], for the X @i[event-key] into a key-event.  See
@f[define-mouse-keysym].
@enddefun

@defun[fun {make-key-event}, args {@i[object] @i[bits]}]
This function returns a key-event described by @i[object] with @i[bits].
@i[Object] is one of keysym, string, or key-event.  When @i[object] is a
key-event, this uses @f[key-event-keysym].  You can form @i[bits] with
@f[make-key-event-bits] or @f[key-event-modifier-mask].
@enddefun

@defun[fun {key-event-p}, args {@i[object]}]
This function returns whether @i[object] is a key-event.
@enddefun

@defun[fun {key-event-bits}, args {@i[key-event]}]
This function returns the bits field of a @i[key-event].
@enddefun

@defun[fun {key-event-keysym}, args {@i[key-event]}]
This function returns the keysym field of a @i[key-event].
@enddefun

@defun[fun {char-key-event}, args {@i[character]}]
This function returns the key-event associated with @i[character].  You can
associate a key-event with a character by @f[setf]'ing this form.
@enddefun

@defun[fun {key-event-char}, args {@i[key-event]}]
This function returns the character associated with @i[key-event].  You can
associate a character with a key-event by @f[setf]'ing this form.  The system
defaultly translates key-events in some implementation dependent way for text
insertion; for example, under an ASCII system, the key-event @f[#k"C-h"], as
well as @f[#k"backspace"] would map to the Common Lisp character that causes a
backspace.
@enddefun

@defun[fun {key-event-bit-p}, args {@i[key-event] @i[bit-name]}]
This function returns whether @i[key-event] has the bit set named by
@i[bit-name].  This signals an error if @i[bit-name] is undefined.
@enddefun

@defmac[fun {do-alpha-key-events}, args
{(@i[var] @i[kind] @optional @i[result]) @mstar<@i[form]>}]
 This macro evaluates each @i[form] with @i[var] bound to a key-event
representing an alphabetic character.  @i[Kind] is one of @kwd[lower],
@kwd[upper], or @kwd[both], and this binds @i[var] to each key-event in order
as specified in @i[X Window System Protocol, MIT X Consortium Standard, X
Version 11, Release 4].  When @kwd[both] is specified, this processes lowercase
letters first.
@enddefmac

@defun[fun {print-pretty-key}, args {@i[key] @optional @i[stream] @i[long-names-p]}]
This prints @i[key], a key-event or vector of key-events, in a user-expected
fashion to @i[stream].  @i[Long-names-p] indicates whether modifiers should
print with their long or short name.  @i[Stream] defaults to
@var[standard-output].
@enddefun

@defun[fun {print-pretty-key-event}, args {@i[key-event] @optional @i[stream] @i[long-names-p]}]
This prints @i[key-event] to @i[stream] in a user-expected fashion.
@i[Long-names-p] indicates whether modifier names should appear using the long
name or short name.  @i[Stream] defaults to @var[standard-output].
@enddefun



@section (CLX Interface)

@subsection (Graphics Window Hooks)
This section describes a few hooks used by Hemlock's internals to handle
graphics windows that manifest Hemlock windows.  Some heavy users of Hemlock as
a tool have needed these in the past, but typically functions that replace the
default values of these hooks must be written in the "@f[HEMLOCK-INTERNALS]"
package.  All of these symbols are internal to this package.

If you need this level of control for your application, consult the current
implementation for code fragments that will be useful in correctly writing your
own window hook functions.

@defvar[var {create-window-hook}]
This holds a function that @Hemlock calls when @f[make-window] executes under
CLX.  @Hemlock passes the CLX display and the following arguments from
@f[make-window]: starting mark, ask-user, x, y, width, height, and modelinep.
The function returns a CLX window or nil indicating one could not be made.
@enddefvar

@defvar[var {delete-window-hook}]
This holds a function that @hemlock calls when @f[delete-window] executes under
CLX.  @hemlock passes the CLX window and the @hemlock window to this function.
@enddefvar

@defvar[var {random-typeout-hook}]
This holds a function that @hemlock calls when random typeout occurs under CLX.
@hemlock passes it a @hemlock device, a pre-existing CLX window or @nil, and
the number of pixels needed to display the number of lines requested in the
@f[with-pop-up-display] form.  It should return a window, and if a new window
is created, then a CLX gcontext must be the second value.
@enddefvar

@defvar[var {create-initial-windows-hook}]
This holds a function that @hemlock calls when it initializes the screen
manager and makes the first windows, typically windows for the @hid[Main] and
@hid[Echo Area] buffers.  @hemlock passes the function a @hemlock device.
@enddefvar


@subsection (Entering and Leaving Windows)

@defhvar[var "Enter Window Hook"]
When the mouse enters an editor window, @hemlock invokes the functions in this
hook.  These functions take a @Hemlock window as an argument.
@enddefhvar

@defhvar[var "Exit Window Hook"]
When the mouse exits an editor window, @hemlock invokes the functions in this
hook.  These functions take a @Hemlock window as an argument.
@enddefhvar


@subsection (How to Lose Up-Events)
Often the only useful activity user's design for the mouse is to click on
something.  @Hemlock sees a character representing the down event, but what do
you do with the up event character that you know must follow?  Having the
command eat it would be tasteless, and would inhibit later customizations that
make use of it, possibly adding on to the down click command's functionality.
Bind the corresponding up character to the command described here.

@defcom[com "Do Nothing"]
This does nothing as many times as you tell it.
@enddefcom


@section (Slave Lisps)
@index (Slave lisp interface functions)
Some implementations of @hemlock feature the ability to manage multiple slave
Lisps, each connected to one editor Lisp.  The routines discussed here spawn
slaves, send evaluation and compilation requests, return the current server,
etc.  This is very powerful because without it you can lose your editing state
when code you are developing causes a fatal error in Lisp.

The routines described in this section are best suited for creating editor
commands that interact with slave Lisps, but in the past users implemented
several independent Lisps as nodes communicating via these functions.  There is
a better level on which to write such code that avoids the extra effort these
routines take for the editor's sake.  See the @i[CMU Common Lisp User's Manual]
for the @f[remote] and @f[wire] packages.


@subsection (The Current Slave)
There is a slave-information structure that these return which is suitable for
passing to the routines described in the following subsections.

@defun[fun {create-slave}, args {@optional @i[name]}]
This creates a slave that tries to connect to the editor.  When the slave
connects to the editor, this returns a slave-information structure, and the
interactive buffer is the buffer named @i[name].  This generates a name if
@i[name] is @nil.  In case the slave never connects, this will eventually
timeout and signal an editor-error.
@enddefun

@defun[fun {get-current-eval-server}, args {@optional @i[errorp]}]
@defhvar1[var {Current Eval Server}]
This returns the server-information for the @hid[Current Eval Server] after
making sure it is valid.  Of course, a slave Lisp can die at anytime.  If this
variable is @nil, and @i[errorp] is non-@nil, then this signals an
editor-error; otherwise, it tries to make a new slave.  If there is no current
eval server, then this tries to make a new slave, prompting the user based on a
few variables (see the @i[Hemlock User's Manual]).
@enddefun

@defun[fun {get-current-compile-server}]
@defhvar1[var {Current Compile Server}]
This returns the server-information for the @hid[Current Compile Server] after
making sure it is valid.  This may return nil.  Since multiple slaves may
exist, it is convenient to use one for developing code and one for compiling
files.  The compilation commands that use slave Lisps prefer to use the current
compile server but will fall back on the current eval server when necessary.
Typically, users only have separate compile servers when the slave Lisp can
live on a separate workstation to save cycles on the editor machine, and the
@hemlock commands only use this for compiling files.
@enddefun


@subsection (Asynchronous Operation Queuing)
The routines in this section queue requests with an eval server.  Requests are
always satisfied in order, but these do not wait for notification that the
operation actually happened.  Because of this, the user can continue editing
while his evaluation or compilation occurs.  Note, these usually execute in the
slave immediately, but if the interactive buffer connected to the slave is
waiting for a form to return a value, the operation requested must wait until
the slave is free again.

@defun[fun {string-eval}, args {@i[string]}, keys {[server][package][context]}]
@defun1[fun {region-eval}, args {@i[region]}, keys {[server][package][context]}]
@defun1[fun {region-compile}, args {@i[region]}, keys {[server][package]}]
@f[string-eval] queues the evaluation of the form read from @i[string] on eval
server @i[server].  @i[Server] defaults to the result of
@f[get-current-server], and @i[string] is a simple-string.  The evaluation
occurs with @var[package] bound in the slave to the package named by
@i[package], which defaults to @hid[Current Package] or the empty string; the
empty string indicates that the slave should evaluate the form in its current
package.  The slave reads the form in @i[string] within this context as well.
@i[Context] is a string to use when reporting start and end notifications in
the @hid[Echo Area] buffer; it defaults to the concatenation of @f["evaluation
of "] and @i[string].

@f[region-eval] is the same as @f[string-eval], but @i[context] defaults
differently.  If the user leaves this unsupplied, then it becomes a string
involving part of the first line of region.

@f[region-compile] is the same as the above.  @i[Server] defaults the same; it
does not default to @f[get-current-compile-server] since this compiles the
region into the slave Lisp's environment, to affect what you are currently
working on.
@enddefun

@defun[fun {file-compile}, args {@i[file]},
			   keys {[output-file][error-file][load][server]},
			   morekeys {[package]}]
@defhvar1[var {Remote Compile File}, val {nil}]
This compiles @i[file] in a slave Lisp.  When @i[output-file] is @true (the
default), this uses a temporary output file that is publicly writable in case
the client is on another machine, which allows for file systems that do not
permit remote write access.  This renames the temporary file to the appropriate
binary name or deletes it after compilation.  Setting @hid[Remote Compile File]
to @nil, inhibits this.  If @i[output-file] is non-@nil and not @true, then it
is the name of the binary file to write.  The compilation occurs with
@var[package] bound in the slave to the package named by @i[package], which
defaults to @hid[Current Package] or the empty string; the empty string
indicates that the slave should evaluate the form in its current package.
@i[Error-file] is the file in which to record compiler output, and a @nil value
inhibits this file's creation.  @i[Load] indicates whether to load the
resulting binary file, defaults to @nil.  @i[Server] defaults to
@f[get-current-compile-server], but if this returns nil, then @i[server]
defaults to @f[get-current-server].
@enddefun

@subsection (Synchronous Operation Queuing)
The routines in this section queue requests with an eval server and wait for
confirmation that the evaluation actually occurred.  Because of this, the user
cannot continue editing while the slave executes the request.  Note, these
usually execute in the slave immediately, but if the interactive buffer
connected to the slave is waiting for a form to return a value, the operation
requested must wait until the slave is free again.

@defun[fun {eval-form-in-server},
       args {@i[server-info] @i[string] @optional @i[package]}]
 This function queues the evaluation of a form in the server associated with
@i[server-info] and waits for the results.  The server @f[read]'s the form from
@i[string] with @var[package] bound to the package named by @i[package].  This
returns the results from the slave Lisp in a list of string values.  You can
@f[read] from the strings or simply display them depending on the @f[print]'ing
of the evaluation results.

@i[Package] defaults to @hid[Current Package].  If this is @nil, the server
uses the value of @var[package] in the server.

While the slave executes the form, it binds @var[terminal-io] to a stream that
signals errors when read from and dumps output to a bit-bucket.  This prevents
the editor and slave from dead locking by waiting for each other to reply.
@enddefun

@defun[fun {eval-form-in-server-1},
       args {@i[server-info] @i[string] @optional @i[package]}]
 This function calls @f[eval-form-in-server] and @f[read]'s the result in the
first string it returns.  This result must be @f[read]'able in the editor's
Lisp.
@enddefun


@section (Spelling)
@index (Spelling checking)
@hemlock supports spelling checking and correcting commands based on the ITS
Ispell dictionary.  These commands use the following routines which include
adding and deleting entries, reading the Ispell dictionary in a compiled binary
format, reading user dictionary files in a text format, and checking and
correcting possible spellings.

@defun[fun {maybe-read-spell-dictionary}, package {spell}]
This reads the default binary Ispell dictionary.  Users must call this before
the following routines will work.
@enddefun

@defun[fun {spell-read-dictionary}, package {spell}, args {@i[filename]}]
This adds entries to the dictionary from the lines in the file @i[filename].
Dictionary files contain line oriented records like the following:
@begin[programexample]
entry1/flag1/flag2
entry2
entry3/flag1
@end[programexample]
The flags are the Ispell flags indicating which endings are appropriate for the
given entry root, but these are unnecessary for user dictionary files.  You can
consult Ispell documentation if you want to know more about them.
@enddefun

@defun[fun {spell-add-entry}, package {spell},
       args {@i[line] @optional @i[word-end]}]
This takes a line from a dictionary file, and adds the entry described by
@i[line] to the dictionary.  @i[Word-end] defaults to the position of the first
slash character or the length of the line.  @i[Line] is destructively modified.
@enddefun

@defun[fun {spell-remove-entry}, package {spell}, args {@i[entry]}]
This removes entry, a simple-string, from the dictionary, so it will be an
unknown word.  This destructively modifies @i[entry].  If it is a root word,
then all words derived with @i[entry] and its flags will also be deleted.  If
@i[entry] is a word derived from some root word, then the root and any words
derived from it remain known words.
@enddefun

@defun[fun {correct-spelling}, package {spell}, args {@i[word]}]
This checks the spelling of @i[word] and outputs the results.  If this finds
@i[word] is correctly spelled due to some appropriate suffix on a root, it
generates output indicating this.  If this finds @i[word] as a root entry, it
simply outputs that it found @i[word].  If this cannot find @i[word] at all,
then it outputs possibly correct close spellings.  This writes to
@var[standard-output], and it calls @f[maybe-read-spell-dictionary] before
attempting any lookups.
@enddefun

@defun[fun {spell-try-word}, package {spell}, args {@i[word] @i[word-len]}]
@defcon1[var {max-entry-length}, val {31}]
This returns an index into the dictionary if it finds @i[word] or an
appropriate root.  @i[Word-len] must be inclusively in the range 2 through
@f[max-entry-length], and it is the length of @i[word].  @i[Word] must be
uppercase.  This returns a second value indicating whether it found @i[word]
due to a suffix flag, @nil if @i[word] is a root entry.
@enddefun

@defun[fun {spell-root-word}, package {spell}, args {@i[index]}]
This returns a copy of the root word at dictionary entry @i[index].  This index
is the same as returned by @f[spell-try-word].
@enddefun

@defun[fun {spell-collect-close-words}, package {spell}, args {@i[word]}]
This returns a list of words correctly spelled that are @i[close] to @i[word].
@i[Word] must be uppercase, and its length must be inclusively in the range 2
through @f[max-entry-length].  Close words are determined by the Ispell rules:
@begin[enumerate]
Two adjacent letters can be transposed to form a correct spelling.

One letter can be changed to form a correct spelling.

One letter can be added to form a correct spelling.

One letter can be removed to form a correct spelling.
@end[enumerate]
@enddefun

@defun[fun {spell-root-flags}, package {spell}, args {@i[index]}]
This returns a list of suffix flags as capital letters that apply to the
dictionary root entry at @i[index].  This index is the same as returned by
@f[spell-try-word].
@enddefun


@section (File Utilities)
Some implementations of @hemlock provide extensive directory editing commands,
@hid[Dired], including a single wildcard feature.  An asterisk denotes a
wildcard.

@defun[fun {copy-file}, package {dired},
       args {@i[spec1] @i[spec2]}, keys {[update][clobber][directory]}]
 This function copies @i[spec1] to @i[spec2].  It accepts a single wildcard in
the filename portion of the specification, and it accepts directories.  This
copies files maintaining the source's write date.

If @i[spec1] and @i[spec2] are both directories, this recursively copies the
files and subdirectory structure of @i[spec1]; if @i[spec2] is in the
subdirectory structure of @i[spec1], the recursion will not descend into it.
Use @f["/spec1/*"] to copy only the files from @i[spec1] to directory
@i[spec2].

If @i[spec2] is a directory, and @i[spec1] is a file, then this copies
@i[spec1] into @i[spec2] with the same @f[pathname-name].

When @kwd[update] is non-@nil, then the copying process only copies files if the
source is newer than the destination.

When @kwd[update] and @kwd[clobber] are @nil, and the destination exists, the
copying process stops and asks the user whether the destination should be
overwritten.

When the user supplies @kwd[directory], it is a list of pathnames, directories
excluded, and @i[spec1] is a pattern containing one wildcard.  This then copies
each of the pathnames whose @f[pathname-name] matches the pattern.  @i[Spec2]
is either a directory or a pathname whose @f[pathname-name] contains a
wildcard.
@enddefun

@defun[fun {rename-file}, package {dired},
       args {@i[spec1] @i[spec2]}, keys {[clobber][directory]}]
 This function renames @i[spec1] to @i[spec2].  It accepts a single wildcard in
the filename portion of the specification, and @i[spec2] may be a directory
with the destination specification resulting in the merging of @i[spec2] with
@i[spec1].  If @kwd[clobber] is @nil, and @i[spec2] exists, then this asks the
user to confirm the renaming.  When renaming a directory, end the specification
without the trailing slash.

When the user supplies @kwd[directory], it is a list of pathnames, directories
excluded, and @i[spec1] is a pattern containing one wildcard.  This then copies
each of the pathnames whose @f[pathname-name] matches the pattern.  @i[Spec2]
is either a directory or a pathname whose @f[pathname-name] contains a
wildcard.
@enddefun

@defun[fun {delete-file}, package {dired},
       args {@i[spec]}, keys {[recursive][clobber]}]
 This function deletes @i[spec].  It accepts a single wildcard in the filename
portion of the specification, and it asks for confirmation on each file if
@kwd[clobber] is @nil.  If @kwd[recursive] is non-@nil, then @i[spec] may be a
directory to recursively delete the entirety of the directory and its
subdirectory structure.  An empty directory may be specified without
@kwd[recursive] being non-@nil.  Specify directories with the trailing
slash.
@enddefun

@defun[fun {find-file}, package {dired},
       args {@i[name] @optional @i[directory] @i[find-all]}]
 This function finds the file with @f[file-namestring] @i[name], recursively
looking in @i[directory].  If @i[find-all] is non-@nil (defaults to @nil), then
this continues searching even after finding a first occurrence of file.
@i[Name] may contain a single wildcard, which causes @i[find-all] to default to
@true instead of @nil.
@enddefun

@defun[fun {make-directory}, package {dired}, args {@i[name]}]
This function creates the directory with @i[name].  If it already exists, this
signals an error.
@enddefun

@defun[fun {pathnames-from-pattern}, package {dired},
       args {@i[pattern] @i[files]}]
This function returns a list of pathnames from the list @i[files] whose
@f[file-namestring]'s match @i[pattern].  @i[Pattern] must be a non-empty
string and contain only one asterisk.  @i[Files] contains no directories.
@enddefun

@defvar[var {update-default}, package {dired}]
@defvar1[var {clobber-default}, package {dired}]
@defvar1[var {recursive-default}, package {dired}]
These are the default values for the keyword arguments above with corresponding
names.  These default to @nil, @true, and @nil respectively.
@enddefvar

@defvar[var {report-function}, package {dired}]
@defvar1[var {error-function}, package {dired}]
@defvar1[var {yesp-function}, package {dired}]
These are the function the above routines call to report progress, signal
errors, and prompt for @i[yes] or @i[no].  These all take format strings and
arguments.
@enddefvar


@defun[fun {merge-relative-pathnames}, args {@i[pathname] @i[default-directory]}]
This function merges @i[pathname] with @i[default-directory].  If @i[pathname]
is not absolute, this assumes it is relative to @i[default-directory].  The
result is always a directory pathname.
@enddefun

@defun[fun {directoryp}, args {@i[pathname]}]
This function returns whether @i[pathname] names a directory: it has no name
and no type fields.
@enddefun


@section (Beeping)

@defun[fun {hemlock-beep}]
@Hemlock binds @f[system:*beep-function*] to this function to beep the device.
It is different for different devices.
@enddefun

@defhvar[var "Bell Style", val {:border-flash}]
@defhvar1[var "Beep Border Width", val {20}]
@hid[Bell Style] determines what @var[hemlock-beep] does in @hemlock under CLX.
Acceptable values are @kwd[border-flash], @kwd[feep],
@kwd[border-flash-and-feep], @kwd[flash], @kwd[flash-and-feep], and @nil (do
nothing).

@hid[Beep Border Width] is the width in pixels of the border flashed by border
flash beep styles.
@enddefhvar
