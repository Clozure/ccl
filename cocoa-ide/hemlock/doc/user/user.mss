@Make[Manual] @comment{-*- Dictionary: /afs/cs/project/clisp/scribe/hem/hem; Mode: spell; Package: Hemlock -*-}
@Device[postscript]
@Style(Spacing = 1.2 lines)
@Style(StringMax = 5000)
@Use(Database "/afs/cs/project/clisp/docs/database/")
@Style(FontFamily=TimesRoman)
@Style(Date="March 1952")
@style(DOUBLESIDED)
@Libraryfile[ArpaCredit]
@libraryfile[hem]
@Libraryfile[Spice]
@Libraryfile[Uttir]

@String(REPORTTITLE "Hemlock User's Manual")

@comment<@begin[TitlePage]
@begin[TitleBox]
>
@blankspace(1.3inches)
@heading[Hemlock User's Manual]

@center[@b<Bill Chiles>
@b<Robert A. MacLachlan>


@b<@value[date]>

@b<CMU-CS-89-133-R1>
]
@comment<@end[TitleBox]>
@blankspace(2lines)
@begin[Center]
School of Computer Science
Carnegie Mellon University
Pittsburgh, PA 15213
@end[Center]
@blankspace[2lines]

@begin[Center]
This is a revised version of Technical Report CMU-CS-87-158.
@end[Center]

@heading<Abstract>
@begin(Text, indent 0)
This document describes the @Hemlock text editor, version M3.2.  @Hemlock is a
customizable, extensible text editor whose initial command set closely
resembles that of ITS/TOPS-20 @Emacs.  @Hemlock is written in CMU Common Lisp
and has been ported to other implementations.
@end(Text)

@begin[ResearchCredit]
@ArpaCredit[Contract=Basic87-90]
@end[ResearchCredit]
@comment<@end[TitlePage]>


@commandstring(mh = "@f1(MH)")
@commandstring(dash = "@Y[M]")

@comment[This tabclear is necessary since the definition macros don't
	 take care of the own tabbing needs]
@tabclear


@comment[@chap (Introduction)]
@include(intro)


@comment[@chap (Basic Commands)]
@include(commands)



@chap[Files, Buffers, and Windows]

@section[Introduction]

@index[files]
@index[buffers]
@index[windows]
@hemlock provides three different abstractions which are used in combination to
solve the text-editing problem, while other editors tend to mash these ideas
together into two or even one.
@begin[description]
File@\A file provides permanent storage of text.  @hemlock has commands
to read files into buffers and write buffers out into files.

Buffer@\A buffer provides temporary storage of text and a capability to
edit it.  A buffer may or may not have a file associated with it; if it
does, the text in the buffer need bear no particular relation to the text
in the file.  In addition, text in a buffer may be displayed in any number
of windows, or may not be displayed at all.

Window@\A window displays some portion of a buffer on the screen.  There
may be any number of windows on the screen, each of which may display any
position in any buffer.  It is thus possible, and often useful, to have
several windows displaying different places in the same buffer.
@end[description]


@section[Buffers]
In addition to some text, a buffer has several other user-visible attributes:
@begin[description]
A name@\
A buffer is identified by its name, which allows it to be selected, destroyed,
or otherwise manipulated.

A collection of modes@\
The modes present in a buffer alter the set of commands available and
otherwise alter the behavior of the editor.  For details see page
@pageref[modes].

A modification flag @\
This flag is set whenever the text in a buffer is modified.  It is often
useful to know whether a buffer has been changed, since if it has it should
probably be saved in its associated file eventually.

A write-protect flag @\
If this flag is true, then any attempt to modify the buffer will result in an
error.
@end[description]

@defcom[com "Select Buffer", bind (C-x b)]
This command prompts for the name of a existing buffer and makes that buffer
the @i[current buffer].  The newly selected buffer is displayed in the
current window, and editing commands now edit the text in that buffer.
Each buffer has its own point, thus the point will be in the place it was
the last time the buffer was selected.  When prompting for the buffer, the
default is the buffer that was selected before the current one.
@enddefcom

@defcom[com "Select Previous Buffer", bind (C-M-l)]
@defcom1[com "Circulate Buffers", bind (C-M-L)]
With no prefix argument, @hid[Select Previous Buffer] selects the buffer that
has been selected most recently, similar to @binding[C-x b Return].  If given a
prefix argument, then it does the same thing as @hid[Circulate Buffers].

@hid[Circulate Buffers] moves back into successively earlier buffers in the
buffer history.  If the previous command was not @hid[Circulate Buffers] or
@hid[Select Previous Buffer], then it does the same thing as
@hid[Select Previous Buffer], otherwise it moves to the next most recent
buffer.  The original buffer at the start of the excursion is made the previous
buffer, so @hid[Select Previous Buffer] will always take you back to where you
started.

These commands are generally used together.  Often @hid[Select Previous Buffer]
will take you where you want to go.  If you don't end up there, then using
@hid[Circulate Buffers] will do the trick.
@enddefcom

@defcom[com "Create Buffer", bind (C-x M-b)]
This command is very similar to @hid[Select Buffer], but the buffer need not
already exist.  If the buffer does not exist, a new empty buffer is created
with the specified name.
@enddefcom

@defcom[com "Kill Buffer", bind (C-x k)]
This command is used to make a buffer go away.  There is no way to restore
a buffer that has been accidentally deleted, so the user is given a chance
to save the hapless buffer if it has been modified.  This command is poorly
named, since it has nothing to do with killing text.
@enddefcom

@defcom[com "List Buffers", bind (C-x C-b)]
This command displays a list of all existing buffers in a pop-up window.  A
"@f[*]" is displayed before the name of each modified buffer.  A buffer with no
associated file is represented by the buffer name followed by the number of
lines in the buffer.  A buffer with an associated file are is represented by
the name and type of the file, a space, and the device and directory.  If the
buffer name doesn't match the associated file, then the buffer name is also
displayed.  When given a prefix argument, this command lists only the modified
buffers.
@enddefcom

@defcom[com "Buffer Not Modified", bind (M-~)]
This command resets the current buffer's modification flag @dash @i[it does not
save any changes].  This is primarily useful in cases where a user accidentally
modifies a buffer and then undoes the change.  Resetting the modified flag
indicates that the buffer has no changes that need to be written out.
@enddefcom

@defcom[com "Check Buffer Modified", bind (C-x ~)]
This command displays a message indicating whether the current buffer is modified.
@enddefcom

@defcom[com "Set Buffer Read-Only"]
This command changes the flag that allows the current buffer to be modified.
If a buffer is read-only, any attempt to modify it will result in an error.  The
buffer may be made writable again by repeating this command.
@enddefcom

@defcom[com "Set Buffer Writable"]
This command ensures the current buffer is modifiable.
@enddefcom

@defcom[com "Insert Buffer"]
This command prompts for the name of a buffer and inserts its contents at the
point, pushing a buffer mark before inserting.  The buffer inserted is
unaffected.
@enddefcom  

@defcom[com "Rename Buffer"]
This command prompts for a new name for the current buffer, which defaults
to a name derived from the associated filename.
@enddefcom


@section[Files]
@index[files]
These commands either read a file into the current buffer or write it out to
some file.  Various other bookkeeping operations are performed as well.

@defcom[com "Find File", bind (C-x C-f)]
This is the command normally used to get a file into @hemlock.  It prompts
for the name of a file, and if that file has already been read in, selects
that buffer; otherwise, it reads file into a new buffer whose name is
derived from the name of the file.  If the file does not exist, then the
buffer is left empty, and @w<"@f[(New File)]"> is displayed in the echo area;
the file may then be created by saving the buffer.

The buffer name created is in the form @w<"@i[name] @i[type] @i[directory]">.
This means that the filename "@f[/sys/emacs/teco.mid]" has
@w<"@f[Teco Mid /Sys/Emacs/]"> as its the corresponding buffer name.  The
reason for rearranging the fields in this fashion is that it facilitates
recognition since the components most likely to differ are placed first.  If
the buffer cannot be created because it already exists, but has another file in
it (an unlikely occurrence), then the user is prompted for the buffer to use,
as by @hid[Create Buffer].

@hid[Find File] takes special action if the file has been modified on disk
since it was read into @hemlock.  This usually happens when several people are
simultaneously editing a file, an unhealthy circumstance.  If the buffer is
unmodified, @hid[Find File] just asks for confirmation before reading in the
new version.  If the buffer is modified, then @hid[Find File] beeps and prompts
for a single key-event to indicate what action to take.  It recognizes
the following key-events:
@begin[description]
@binding[Return, Space, y]@\
 Prompt for a file in which to save the current buffer and then read in the
file found to be modified on disk.

@binding[Delete, Backspace, n]@\
 Forego reading the file.

@binding[r]@\
 Read the file found to be modified on disk into the buffer containing the
earlier version with modifications.  This loses all changes you had in the
buffer.
@end[description]
@enddefcom

@defcom[com "Save File", bind (C-x C-s)]
This command writes the current buffer out to its associated file and
resets the buffer modification flag.  If there is no associated file, then
the user is prompted for a file, which is made the associated file.  If
the buffer is not modified, then the user is asked whether to actually
write it or not.

If the file has been modified on disk since the last time it was read,
@hid[Save File] prompts for confirmation before overwriting the file.
@enddefcom

@defcom[com "Save All Files", bind (C-x C-m)]
@defcom1[com "Save All Files and Exit", bind (C-x M-z)]
@defhvar1[var "Save All Files Confirm", val {t}]
@hid[Save All Files] does a @hid[Save File] on all buffers which have an
associated file.  @hid[Save All Files and Exit] does the same thing and then
exits @hemlock.

When @hid[Save All Files Confirm] is true, these commands will ask for
confirmation before saving a file.
@enddefcom

@defcom[com "Visit File", bind (C-x C-v)]
This command prompts for a file and reads it into the current buffer,
setting the associated filename.  Since the old contents of the buffer are
destroyed, the user is given a chance to save the buffer if it is modified.
As for @hid[Find File], the file need not actually exist.  This command warns
if some other buffer also contains the file.
@enddefcom

@defcom[com "Write File", bind (C-x C-w)] This command prompts for a file
and writes the current buffer out to it, changing the associated filename
and resetting the modification flag.  When the buffer's associated file is
specified this command does the same thing as @hid[Save File].  @enddefcom

@defcom[com "Backup File"]
This command is similar to @hid[Write File], but it neither sets the
associated filename nor clears the modification flag.  This is useful for
saving the current state somewhere else, perhaps on a reliable machine.

Since @hid[Backup File] doesn't update the write date for the buffer,
@hid[Find File] and @hid[Save File] will get all upset if you back up
a buffer on any file that has been read into @hemlock.
@enddefcom

@defcom[com "Revert File"]
@defhvar1[var "Revert File Confirm", val {t}]
This command replaces the text in the current buffer with the contents of the
associated file or the checkpoint file for that file, whichever is more recent.
The point is put in approximately the same place that it was before the file
was read.  If the original file is reverted to, then clear the modified flag,
otherwise leave it set.  If a prefix argument is specified, then always revert
to the original file, ignoring any checkpoint file.

If the buffer is modified and @hid[Revert File Confirm] is true, then the user
is asked for confirmation.
@enddefcom

@defcom[com "Insert File", bind (C-x C-r)]
This command prompts for a file and inserts it at the point, pushing a buffer
mark before inserting.
@enddefcom

@defcom[com "Write Region"]
This command prompts for a file and writes the text in the region out to it.
@enddefcom

@defhvar[var "Add Newline at EOF on Writing File", val {:ask-user}]
This variable controls whether some file writing commands add a newline at the
end of the file if the last line is non-empty.
@begin[description]
@f[:ask-user]@\Ask the user whether to add a newline.

@f[t]@\Automatically add a newline and inform the user.

@nil@\Never add a newline and do not ask.
@end[description]
Some programs will lose the text on the last line or get an
error when the last line does not have a newline at the end.
@enddefhvar

@defhvar[var "Keep Backup Files", val {nil}]
Whenever a file is written by @hid[Save File] and similar commands, the old
file is renamed by appending "@f[.BAK]" to the name, ensuring that some version
of the file will survive a system crash during the write.  If set to true, this
backup file will not deleted even when the write successfully completes.
@enddefhvar


@subsection[Auto Save Mode]

@hid[Save] mode protects against loss of work in system crashes by periodically
saving modified buffers in checkpoint files.

@defcom[com "Auto Save Mode"]
This command turns on @hid[Save] mode if it is not on, and turns off when it is
on.  @hid[Save] mode is on by default.
@enddefcom

@defhvar[var "Auto Save Checkpoint Frequency", val {120}]
@defhvar1[var "Auto Save Key Count Threshold", val {256}]
These variables determine how often modified buffers in @hid[Save] mode will be
checkpointed.  Checkpointing is done after
@hid[Auto Save Checkpoint Frequency] seconds, or after
@hid[Auto Save Key Count Threshold] keystrokes that modify the buffer
(whichever comes first).  Either kind of checkpointing may be disabled by
setting the corresponding variable to @nil.
@enddefhvar

@defhvar[var "Auto Save Cleanup Checkpoints", val {t}]
If this variable is true, then any checkpoint file for a buffer will be deleted
when the buffer is successfully saved in its associated file.
@enddefhvar

@defhvar[var "Auto Save Filename Pattern", val {"~A~A.CKP"}]
@defhvar1[var "Auto Save Pathname Hook", val {make-unique-save-pathname}]
These variables determine the naming of checkpoint files.
@hid[Auto Save Filename Pattern] is a format string used to name the checkpoint
files for buffers with associated files.  Format is called with two arguments:
the directory and file namestrings of the associated file.

@hid[Auto Save Pathname Hook] is a function called by @hid[Save] mode to get a
checkpoint pathname when there is no pathname associated with a buffer.  It
should take a buffer as its argument and return either a pathname or @nil.  If
a pathname is returned, then it is used as the name of the checkpoint file.  If
the function returns @nil, or if the hook variable is @nil, then @hid[Save]
mode is turned off in the buffer.  The default value for this variable returns
a pathname in the default directory of the form "@w<@f[save-]@i[number]>",
where @i[number] is a number used to make the file unique.
@enddefhvar


@subsection[Filename Defaulting and Merging]
@index[merging, filename]
@index[defaulting, filename]
@index[filename defaulting]
@label[merging]
@index[pathnames]
When @hemlock prompts for the name of a file, it always offers a default.
Except for a few commands that have their own defaults, filename defaults are
computed in a standard way.  If it exists, the associated file for the current
buffer is used as the default, otherwise a more complex mechanism creates a
default.

@defhvar[var "Pathname Defaults", val {(pathname "gazonk.del")}]
@defhvar1[var "Last Resort Pathname Defaults Function"]
@defhvar1[var "Last Resort Pathname Defaults", val {(pathname "gazonk")}]
These variables control the computation of default filename defaults when the
current buffer has no associated file.

@hid[Pathname Defaults] holds a "sticky" filename default.  Commands that
prompt for files set this to the file specified, and the value is used as a
basis for filename defaults.  It is undesirable to offer the unmodified value
as a default, since it is usually the name of an existing file that we don't
want to overwrite.  If the current buffer's name is all alphanumeric, then the
default is computed by substituting the buffer name for the the name portion of
@hid[Pathname Defaults].  Otherwise, the default is computed by calling
@hid[Last Resort Pathname Defaults Function] with the buffer as an argument.

The default value of @hid[Last Resort Pathname Defaults Function] merges 
@hid[Last Resort Pathname Defaults] with @hid[Pathname Defaults].
Unlike @hid[Pathname Defaults], @hid[Last Resort Pathname Defaults] is not
modified by file commands, so setting it to a silly name ensures that real
files aren't inappropriately offered as defaults.
@enddefhvar

When a default is present in the prompt for a file, @hemlock @i[merges] the
given input with the default filename.  The semantics of merging, described in
the Common Lisp manual, is somewhat involved, but @hemlock has a few rules it
uses:
@begin[enumerate]
If @hemlock can find the user's input as a file on the @f["default:"] search
list, then it forgoes merging with the displayed default.  Basically, the
system favors the files in your current working directory over those found by
merging with the defaults offered in the prompt.

Merging comes in two flavors, just merge with the displayed default's directory
or just merge with the displayed default's @f[file-namestring].  If the user
only responds with a directory specification, without any name or type
information, then @hemlock merges the default's @f[file-namestring].  If the
user responds with any name or type information, then @hemlock only merges with
the default's directory.  Specifying relative directories in this second
situation coordinates with the displayed defaults, not the current working
directory.
@end[enumerate]


@subsection[Type Hooks and File Options]
@index[mode comment]
@index[type hooks]
When a file is read either by @hid[Find File] or @hid[Visit File], @hemlock
attempts to guess the correct mode in which to put the buffer, based on the
file's @i[type] (the part of the filename after the last dot).  Any default
action may be overridden by specifying the mode in the file's @i[file
options].@index[modes]@index[package]

@label[file-options]@index[file options] 
The user specifies file options with a special syntax on the first line of a
file.  If the first line contains the string "@f[-*-]", then @hemlock
interprets the text between the first such occurrence and the second, which
must be contained in one line , as a list of @w{"@f<@i[option]: @i[value]>"}
pairs separated by semicolons.  The following is a typical example:
@begin[programexample]
;;; -*- Mode: Lisp, Editor; Package: Hemlock -*-
@end[programexample]

These options are currently defined:
@begin[description]
Dictionary@\The argument is the filename of a spelling dictionary associated
with this file.  The handler for this option merges the argument with the
name of this file.  See @comref[Set Buffer Spelling Dictionary].

Log@\The argument is the name of the change log file associated with this file
(see page @pageref[log-files]).  The handler for this option merges the
argument with the name of this file.

Mode@\The argument is a comma-separated list of the names of modes to turn on
in the buffer that the file is read into.

Package@\The argument is the name of the package to be used for reading code in
the file.  This is only meaningful for Lisp code (see page
@pageref[lisp-package].)

Editor@\The handler for this option ignores its argument and turns on
@hid[Editor] mode (see @comref[Editor Mode]).

@end[description]
If the option list contains no "@f[:]" then the entire string is used as
the name of the major mode for the buffer.

@defcom[com "Process File Options"]
This command processes the file options in the current buffer as described
above.  This is useful when the options have been changed or when a file is
created.
@enddefcom


@section[Windows]
@index[windows]

@hemlock windows display a portion of a buffer's text.  See the section on
@i[window groups], @ref[groups], for a discussion of managing windows on bitmap
device.

@defcom[com "New Window", bind (C-x C-n)]
This command prompts users for a new window which they can place anywhere on
the screen.  This window is in its own group.  This only works with bitmap
devices.
@enddefcom

@defcom[com "Split Window", bind (C-x 2)]
This command splits the current window roughly in half to make two windows.  If
the current window is too small to be split, the command signals a user error.
@enddefcom

@defcom[com "Next Window", bind (C-x n)]
@defcom1[com "Previous Window", bind (C-x p)]
These commands make the next or previous window the new current window, often
changing the current buffer in the process.  When a window is created, it is
arbitrarily made the next window of the current window.  The location of the
next window is, in general, unrelated to that of the current window.
@enddefcom

@defcom[com "Delete Window", bind (C-x C-d, C-x d)]
@defcom1[com "Delete Next Window", bind (C-x 1)]
@hid[Delete Window] makes the current window go away, making the next window
current.  @hid[Delete Next Window] deletes the next window, leaving the current
window unaffected.

On bitmap devices, if there is only one window in the group, either command
deletes the group, making some window in another group the current window.  If
there are no other groups, they signal a user error.
@enddefcom

@defcom[com "Go to One Window"]
This command deletes all window groups leaving one with the @hid[Default
Initial Window X], @hid[Default Initial Window Y], @hid[Default Initial Window
Width], and @hid[Default Initial Window Height].  This remaining window
retains the contents of the current window.
@enddefcom

@defcom[com "Line to Top of Window", bind (M-!)]
@defcom1[com "Line to Center of Window", bind (M-#)]
@index[scrolling]@hid[Line to Top of Window] scrolls the current window up
until the current line is at the top of the screen.

@hid[Line to Center of Window] attempts to scroll the current window so that
the current line is vertically centered.
@enddefcom

@defcom[com "Scroll Next Window Down", bind (C-M-v)]
@defcom1[com "Scroll Next Window Up", bind (C-M-V)]
These commands are the same as @hid[Scroll Window Up] and
@hid[Scroll Window Down] except that they operate on the next window.
@enddefcom

@defcom[com "Refresh Screen", bind {C-l}]
This command refreshes all windows, which is useful if the screen got trashed,
centering the current window about the current line.  When the user supplies a
positive argument, it scrolls that line to the top of the window.  When the
argument is negative, the line that far from the bottom of the window is moved
to the bottom of the window.  In either case when an argument is supplied, this
command only refreshes the current window.
@enddefcom


@chap[Editing Documents]
@index[documents, editing]
Although @hemlock is not dedicated to editing documents as word processing
systems are, it provides a number of commands for this purpose.  If @hemlock is
used in conjunction with a text-formatting program, then its lack of complex
formatting commands is no liability.


@defcom[com "Text Mode"]
This commands puts the current buffer into "Text" mode.
@enddefcom


@section[Sentence Commands]
@index[sentence commands]
A sentence is defined as a sequence of characters ending with a period,
question mark or exclamation point, followed by either two spaces or a newline.
A sentence may also be terminated by the end of a paragraph.  Any number of
closing delimiters, such as brackets or quotes, may be between the punctuation
and the whitespace.  This somewhat complex definition of a sentence is used so
that periods in abbreviations are not misinterpreted as sentence ends.

@defcom[com "Forward Sentence", bind {M-a}]
@defcom1[com "Backward Sentence", bind {M-e}]
@index[motion, sentence]@hid[Forward Sentence] moves the point forward
past the next sentence end. @hid[Backward Sentence] moves to the beginning
of the current sentence. A prefix argument may be used as a repeat count.
@enddefcom

@defcom[com "Forward Kill Sentence", bind {M-k}]
@defcom1[com "Backward Kill Sentence", bind (C-x Delete, C-x Backspace)]
@index[killing, sentence]@hid[Forward Kill Sentence] kills text from the
point through to the end of the current sentence.  @hid[Backward Kill Sentence]
kills from the point to the beginning of the current sentence.  A
prefix argument may be used as a repeat count.
@enddefcom

@defcom[com "Mark Sentence"]
This command puts the point at the beginning and the mark at the end of the
next or current sentence.
@enddefcom


@section[Paragraph Commands]

@index[paragraph commands]A paragraph may be delimited by a blank line or a
line beginning with "@f[']" or "@f[.]", in which case the delimiting line is
not part of the paragraph.  Other characters may be paragraph delimiters in
some modes.  A line with at least one leading whitespace character may also
introduce a paragraph and is considered to be part of the paragraph.  Any
fill-prefix which is present on a line is disregarded for the purpose of
locating a paragraph boundary.

@defcom[com "Forward Paragraph", bind (@bf<M-]>)]
@defcom1[com "Backward Paragraph", bind (M-[)]
@index[motion, paragraph]@index[paragraph, motion]@hid[Forward Paragraph]
moves to the end of the current or next paragraph. @hid[Backward Paragraph]
moves to the beginning of the current or previous paragraph.  A prefix
argument may be used as a repeat count.
@enddefcom

@defcom[com "Mark Paragraph", bind {M-h}]
This command puts the point at the beginning and the mark at the end of the
current paragraph.
@enddefcom

@defhvar[var "Paragraph Delimiter Function", val {default-para-delim-function}]
This variable holds a function that takes a mark as its argument and returns
true when the line it points to should break the paragraph.
@enddefhvar

@section[Filling]

@index[filling]@index[formatting]Filling is a coarse text-formatting
process which attempts to make all the lines roughly the same length, but
doesn't vary the amount of space between words.  Editing text may leave
lines with all sorts of strange lengths; filling this text will return it
to a moderately aesthetic form.

@defcom[com "Set Fill Column", bind (C-x f)]
This command sets the fill column to the column that the point is currently at,
or the one specified by the absolute value of prefix argument, if it is
supplied.  The fill column is the column past which no text is permitted to
extend.
@enddefcom

@defcom[com "Set Fill Prefix", bind (C-x .)]
This command sets the fill prefix to the text from the beginning of the
current line to the point.  The fill-prefix is a string which filling commands
leave at the beginning of every line filled.  This feature is useful for
filling indented text or comments.
@enddefcom

@defhvar[var "Fill Column", val {75}]
@defhvar1[var "Fill Prefix", val {nil}]
These variables hold the value of the fill prefix and fill column, thus
setting these variables will change the way filling is done.  If
@hid[Fill Prefix] is @nil, then there is no fill prefix.
@enddefcom

@defcom[com "Fill Paragraph", bind {M-q}]
@index[paragraph, filling]This command fills the text in the current or next
paragraph.  The point is not moved.
@enddefcom

@defcom[com "Fill Region", bind {M-g}]
@index[region, filling]This command fills the text in the region.  Since
filling can mangle a large quantity of text, this command asks for confirmation
before filling a large region (see @hid[Region Query Size].)
@enddefcom


@defcom[com "Auto Fill Mode"]
@index[modes, auto fill]This command turns on or off the @hid[Fill]
minor mode in the current buffer.  When in @hid[Fill] mode, @bf[Space],
@bf[Return] and @bf[Linefeed] are rebound to commands that check whether
the point is past the fill column and fill the current line if it is.
This enables typing text without having to break the lines manually.

If a prefix argument is supplied, then instead of toggling, the sign
determines whether @hid[Fill] mode is turned off; a positive argument
argument turns in on, and a negative one turns it off.
@enddefcom

@defcom[com "Auto Fill Linefeed", stuff (bound to @bf[Linefeed] in @hid[Fill] mode)]
@defcom1[com "Auto Fill Return", stuff (bound to @bf[Return] in @hid[Fill] mode)]
@hid[Auto Fill Linefeed] fills the current line if it needs it and then goes to
a new line and inserts the fill prefix.  @hid[Auto Fill Return] is similar, but
does not insert the fill prefix on the new line.
@enddefcom

@defcom[com "Auto Fill Space", stuff (bound to @bf[Space] in @hid[Fill] mode)]
If no prefix argument is supplied, this command inserts a space and
fills the current line if it extends past the fill column.  If the argument is
zero, then it fills the line if needed, but does not insert a space.  If the
argument is positive, then that many spaces are inserted without filling.
@enddefcom

@defhvar[var "Auto Fill Space Indent", val {nil}]
This variable determines how lines are broken by the auto fill commands.  If it
is true, new lines are created using the @hid[Indent New Comment Line] command,
otherwise the @hid[New Line] command is used.  Language modes should define
this variable to be true so that auto fill mode can be used on code.
@enddefhvar


@section[Scribe Mode]

@hid[Scribe] mode provides a number of facilities useful for editing Scribe
documents.  It is also sufficiently parameterizable to be adapted to other
similar syntaxes.

@defcom[com "Scribe Mode"]
@index[modes, scribe]This command puts the current buffer in @hid[Scribe] mode.
Except for special Scribe commands, the only difference between @hid[Scribe]
mode and @hid[Text] mode is that the rules for determining paragraph breaks are
different.  In @hid[Scribe] mode, paragraphs delimited by Scribe commands are
normally placed on their own line, in addition to the normal paragraph breaks.
The main reason for doing this is that it prevents @hid[Fill Paragraph] from
mashing these commands into the body of a paragraph.
@enddefcom

@defcom[com "Insert Scribe Directive", stuff (@bf[C-h] in @hid[Scribe] mode)]
This command prompts for a key-event to determine which Scribe directive to
insert.  Directives are inserted differently depending on their kind:
@begin[description]
@i[environment]@\
The current or next paragraph is enclosed in a begin-end pair:
@f<@@begin[@i{directive}]> @i[paragraph] @f<@@end[@i{directive}]>.  If the
current region is active, then this command encloses the region instead of the
paragraph it would otherwise chose.

@i[command]@\
The previous word is enclosed by @f<@@@i[directive][@i[word]]>.  If the
previous word is already enclosed by a use of the same command, then the
beginning of the command is extended backward by one word.
@end[description]

Typing @bf[Home] or @bf[C-_] to this command's prompt will display a list of
all the defined key-events on which it dispatches.
@enddefcom

@defcom[com "Add Scribe Directive"]
This command adds to the database of directives recognized by the 
@hid[Insert Scribe Directive] command.  It prompts for the directive's name,
the kind of directive (environment or command) and the key-event on which to
dispatch.
@enddefcom

@defcom[com "Add Scribe Paragraph Delimiter"]
@defcom1[com "List Scribe Paragraph Delimiters"]
@hid[Add Scribe Paragraph Delimiter] prompts for a string to add to the list of
formatting commands that delimit paragraphs in @hid[Scribe] mode.  If the user
supplies a prefix argument, then this command removes the string as a
delimiter.

@hid[List Scribe Paragraph Delimiters] displays in a pop-up window the Scribe
commands that delimit paragraphs.
@enddefcom

@defhvar[var "Escape Character", val {#\@@}]
@defhvar1[var "Close Paren Character", val {#\]}]
@defhvar1[var "Open Paren Character", val {#\[}]
These variables determine the characters used when a Scribe directive is
inserted.
@enddefhvar

@defcom[com "Scribe Insert Bracket"]
@defhvar1[var "Scribe Bracket Table"]
@hid[Scribe Insert Bracket] inserts a bracket (@bf[>], @bf[}], @bf[)], or
@bf<]>), that caused its invocation, and then shows the matching bracket.

@hid[Scribe Bracket Table] holds a @f[simple-vector] indexed by character
codes.  If a character is a bracket, then the entry for its @f[char-code]
should be the opposite bracket.  If a character is not a bracket, then the
entry should be @nil.
@enddefcom


@section[Spelling Correction]
@index[spelling correction]
@hemlock has a spelling correction facility based on the dictionary for the ITS
spell program.  This dictionary is fairly small, having only 45,000 word or so,
which means it fits on your disk, but it also means that many reasonably common
words are not in the dictionary.  A correct spelling for a misspelled word will
be found if the word is in the dictionary and is only erroneous in that it has
a wrong character, a missing character, an extra character or a transposition
of two characters.


@defcom[com "Check Word Spelling", bind (M-$)]
This command looks up the previous or current word in the dictionary and
attempts to correct the spelling if it is misspelled.  There are four possible
results of invoking this command:
@begin[enumerate]
This command displays the message "@f[Found it.]" in the echo area.  This means
it found the word in the dictionary exactly as given.

This command displays the message "@f[Found it because of @i[word].]", where
@i[word] is some other word with the same root but a different ending.  The
word is no less correct than if the first message is given, but an additional
piece of useless information is supplied to make you feel like you are using a
computer.

The command prompts with "@f[Correction choice:]" in the echo area and lists
possible correct spellings associated with numbers in a pop-up display.  Typing
a number selects the corresponding correction, and the command replaces the
erroneous word, preserving case as though by @hid[Query Replace].  Typing
anything else rejects all the choices.

This commands displays the message "@f[Word not found.]".  The word is not in
the dictionary and possibly spelled correctly anyway.  Furthermore, no
similarly spelled words were found to offer as possible corrections.  If this
happens, it is worth trying some alternate spellings since one of them might
be close enough to some known words that this command could display.
@end[enumerate]
@enddefcom

@defcom[com "Correct Buffer Spelling"]
This command scans the entire buffer looking for misspelled words and offers to
correct them.  It creates a window into the @hid[Spell Corrections] buffer, and
in this buffer it maintains a log of any actions taken by the user.  When this
finds an unknown word, it prompts for a key-event.  The user has the following
options:
@begin[description]
@bf[a]@\
 Ignore this word.  If the command finds the word again, it will prompt again.

@bf[i]@\
 Insert this word in the dictionary.

@bf[c]@\
 Choose one of the corrections displayed in the @hid[Spell Corrections] window
by specifying the correction number.  If the same misspelling is encountered
again, then the command will make the same correction automatically, leaving a
note in the log window.

@bf[r]@\
 Prompt for a word to use instead of the misspelled one, remembering the
correction as with @bf[c].

@binding[C-r]@\
 Go into a recursive edit at the current position, and resume checking when the
recursive edit is exited.
@end[description]
After this command completes, it deletes the log window leaving the buffer
around for future reference.
@enddefcom

@defhvar[var "Spell Ignore Uppercase", val {nil}]
@index[case sensitivity]
If this variable is true, then @hid[Auto Check Word Spelling] and @hid[Correct
Buffer Spelling] will ignore unknown words that are all uppercase.  This is
useful for acronyms and cryptic formatter directives.
@enddefhvar

@defcom[com "Add Word to Spelling Dictionary", bind (C-x $)]
This command adds the previous or current word to the spelling dictionary.
@enddefcom

@defcom[com "Remove Word from Spelling Dictionary"]
This command prompts for a word to remove from the spelling dictionary.  Due to
the dictionary representation, removal of a word in the initial spelling
dictionary will remove all words with the same root.  The user is asked for
confirmation before removing a root word with valid suffix flags.
@enddefcom

@defcom[com "List Incremental Spelling Insertions"]
This command displays the incremental spelling insertions for the current
buffer's associated spelling dictionary file.
@enddefcom

@defcom[com "Read Spelling Dictionary"]
This command adds some words from a file to the spelling dictionary.  The
format of the file is a list of words, one on each line.
@enddefcom

@defcom[com "Save Incremental Spelling Insertions"]
This command appends incremental dictionary insertions to a file.  Any words
added to the dictionary since the last time this was done will be appended to
the file.  Except for @hid[Augment Spelling Dictionary], all the commands that
add words to the dictionary put their insertions in this list.  The file is
prompted for unless @hid[Set Buffer Spelling Dictionary] has been executed in
the buffer.
@enddefcom

@defcom[com "Set Buffer Spelling Dictionary"]
This command Prompts for the dictionary file to associate with the current
buffer.  If the specified dictionary file has not been read for any other
buffer, then it is read.  Incremental spelling insertions from this buffer
can be appended to this file with @hid[Save Incremental Spelling
Insertions].  If a buffer has an associated spelling dictionary, then
saving the buffer's associated file also saves any incremental dictionary
insertions.  The @w<"@f[Dictionary: ]@i[file]"> file option may also be
used to specify the dictionary for a buffer (see section
@ref[file-options]).
@enddefcom

@defhvar[var "Default User Spelling Dictionary", val {nil}]
This variable holds the pathname of a dictionary to read the first time
@hid[Spell] mode is entered in a given editing session.  When
@hid[Set Buffer Spelling Dictionary] or the "@f[dictionary]" file option is
used to specify a dictionary, this default one is read also.  It defaults to
nil.
@enddefhvar


@subsection[Auto Spell Mode]
@hid[Auto Spell Mode] checks the spelling of each word as it is typed.
When an unknown word is typed the user is notified and allowed to take a
number of actions to correct the word.

@defcom[com "Auto Spell Mode"]
This command turns @hid[Spell] mode on or off in the current buffer.
@enddefcom

@defcom[com "Auto Check Word Spelling",
	stuff (bound to word delimiters in @hid[Spell] mode)]
@defhvar1[var "Check Word Spelling Beep", val {t}]
@defhvar1[var "Correct Unique Spelling Immediately", val {t}]
This command checks the spelling of the word before the point, doing nothing if
the word is in the dictionary.  If the word is misspelled but has a known
correction previously supplied by the user, then this command corrects the
spelling.  If there is no correction, then this displays a message in the echo
area indicating the word is unknown.  An unknown word detected by this command
may be corrected using the @hid[Correct Last Misspelled Word] command.  This
command executes in addition to others bound to the same key; for example, if
@hid[Fill] mode is on, any of its commands bound to the same keys as this
command also run.

If @hid[Check Word Spelling Beep] is true, then this command will beep when an
unknown word is found.  If @hid[Correct Unique Spelling Immediately] is true,
then this command will immediately attempt to correct any unknown word,
automatically making the correction if there is only one possible.
@enddefhvar

@defcom[com "Undo Last Spelling Correction", bind (C-x a)]
@defhvar1[var "Spelling Un-Correct Prompt for Insert", val {nil}]
@hid[Undo Last Spelling Correction] undoes the last incremental spelling
correction.  The "correction" is replaced with the old word, and the old word
is inserted in the dictionary.  Any automatic replacement for the old word is
eliminated.  When @hid[Spelling Un-Correct Prompt for Insert] is true, the user
is asked to confirm the insertion into the dictionary.
@enddefcom

@defcom[com "Correct Last Misspelled Word", bind (M-:)]
This command places the cursor after the last misspelled word detected by the
@hid[Auto Check Word Spelling] command and then prompts for a key-event on
which it dispatches:
@begin[description]
@bf[c]@\
 Display possible corrections in a pop-up window, and prompt for the one to
make according to the corresponding displayed digit or letter.

@i[any digit]@\
 Similar to @bf[c] @i[digit], but immediately makes the correction, dispensing
with display of the possible corrections.  This is shorter, but only works when
there are less than ten corrections.

@bf[i]@\
 Insert the word in the dictionary.

@bf[r]@\
 Replace the word with another.

@binding[Backspace, Delete, n]@\
 Skip this word and try again on the next most recently misspelled word.

@binding[C-r]@\
 Enter a recursive edit at the word, exiting the command when the recursive
edit is exited.

@binding[Escape]@\
 Exit and forget about this word.
@end[description]
As in @hid[Correct Buffer Spelling], the @bf[c] and @bf[r] commands add the
correction to the known corrections.
@enddefcom



@chap[Managing Large Systems]

@hemlock provides three tools which help to manage large systems:
@begin[enumerate]
File groups, which provide several commands that operate on all the files
in a possibly large collection, instead of merely on a single buffer.

A source comparison facility with semi-automatic merging, which can be used
to compare and merge divergent versions of a source file.

A change log facility, which maintains a single file containing a record of the
edits done on a system.
@end[enumerate]


@section[File Groups]

@index[file groups]@index[searching, group]@index[replacing, group]
A file group is a set of files, upon which various editing operations can be
performed.  The files in a group are specified by a file in the following
format:
@begin[itemize]
Any line which begins with one "@f[@@]" is ignored.

Any line which does not begin with an "@f[@@]" is the name of a file in the
group.

A line which begins with "@f[@@@@]" specifies another file having this
syntax, which is recursively examined to find more files in the group.
@end[itemize]
This syntax is used for historical reasons.  Although any number of file groups
may be read into @hemlock, there is only one @i[active group], which is the
file group implicitly used by all of the file group commands.  
Page @pageref[compile-group-command] describes the @hid[Compile Group] command.

@defcom[com "Select Group"]
This command prompts for the name of a file group to make the active group.
If the name entered is not the name of a group whose definition has been
read, then the user is prompted for the name of a file to read the group
definition from.  The name of the default pathname is the name of the
group, and the type is "@f[upd]".
@enddefcom

@defcom[com "Group Query Replace"]
This command prompts for target and replacement strings and then executes an
interactive string replace on each file in the active group.  This reads in
each file as if @hid[Find File] were used and processes it as if @hid[Query
Replace] were executing.
@enddefcom

@defcom[com "Group Replace"]
This is like @hid[Group Query Replace] except that it executes a
non-interactive replacement, similar to @hid[Replace String].
@enddefcom

@defcom[com "Group Search"]
This command prompts for a string and then searches for it in each file in the
active group.  This reads in each file as if @hid[Find File] were used.  When
it finds an occurrence, it prompts the user for a key-event indicating what
action to take.  The following commands are defined:
@begin[description]
@binding[Escape, Space, y]@\
 Exit @hid[Group Search].

@binding[Delete, Backspace, n]@\
 Continue searching for the next occurrence of the string.

@binding[!]@\
 Continue the search at the beginning of the next file, skipping the remainder
of the current file.

@binding[C-r]@\
 Go into a recursive edit at the current location, and continue the search when
it is exited.
@end[description]
@enddefcom

@defhvar[var "Group Find File", val {nil}]
The group searching and replacing commands read each file into its own buffer
using @hid[Find File].  Since this may result in large amounts of memory being
consumed by unwanted buffers, this variable controls whether to delete the
buffer after processing it.  When this variable is false, the default, the
commands delete the buffer if it did not previously exist; however, regardless
of this variable, if the user leaves the buffer modified, the commands will not
delete it.
@enddefhvar

@defhvar[var "Group Save File Confirm", val {t}]
If this variable is true, the group searching and replacing commands ask for
confirmation before saving any modified file.  The commands attempt to save
each file processed before going on to the next one in the group.
@enddefhvar


@section[Source Comparison]
@index[buffer, merging]
@index[buffer, comparison]
@index[source comparison]

These commands can be used to find exactly how the text in two buffers differs,
and to generate a new version that combines features of both versions.

@defhvar[var "Source Compare Default Destination", val {"Differences"}]
This is a sticky default buffer name to offer when comparison commands prompt
for a buffer in which to insert the results.
@enddefhvar

@defcom[com "Compare Buffers"]
This command prompts for three buffers and then does a buffer comparison.
The first two buffers must exist, as they are the buffers to be compared.
The last buffer, which is created if it does not exist, is the buffer to
which output is directed.  The output buffer is selected during the
comparison so that its progress can be monitored.  There are various variables
that control exactly how the comparison is done.

If a prefix argument is specified, then only only the lines in the the regions
of the two buffers are compared.
@enddefcom

@defcom[com "Buffer Changes"]
This command compares the contents of the current buffer with the disk version
of the associated file.  It reads the file into the buffer 
@hid[Buffer Changes File], and generates the comparison in the buffer
@hid[Buffer Changes Result].  As with @hid[Compare Buffers], the output buffer
is displayed in the current window.
@enddefcom

@defcom[com "Merge Buffers"]
This command functions in a very similar fashion to @hid[Compare Buffers], the
difference being that a version which is a combination of the two buffers being
compared is generated in the output buffer.  This copies text that is identical
in the two comparison buffers to the output buffer.  When it encounters a
difference, it displays the two differing sections in the output buffer and
prompts the user for a key-event indicating what action to take.  The following
commands are defined:
@begin[description]
@bf[1]@\
 Use the first version of the text.

@bf[2]@\
 Use the second version.

@bf[b]@\
 Insert the string @w<"@f[**** MERGE LOSSAGE ****]"> followed by both versions.
This is useful if the differing sections are too complex, or it is unclear
which is the correct version.  If you cannot make the decision conveniently at
this point, you can later search for the marking string above.

@binding[C-r]@\
 Do a recursive edit and ask again when the edit is exited.
@end[description]
@enddefcom


@defhvar[var "Source Compare Ignore Case", val {nil}]
@index[case sensitivity]
If this variable is non-@nil, @hid[Compare Buffers] and @hid[Merge Buffers]
will do comparisons case-insensitively.
@enddefhvar

@defhvar[var "Source Compare Ignore Indentation", val {nil}] 
If this variable is non-@nil, @hid[Compare Buffers] and @hid[Merge Buffers]
ignore initial whitespace when comparing lines.
@enddefhvar

@defhvar[var "Source Compare Ignore Extra Newlines", val {t}]
If this variable is true, @hid[Compare Buffers] and @hid[Merge Buffers]
will treat all groups of newlines as if they were a single newline.
@enddefhvar

@defhvar[var "Source Compare Number of Lines", val {3}]
This variable controls the number of lines @hid[Compare Buffers] and
@hid[Merge Buffers] will compare when resynchronizing after a difference
has been encountered.
@enddefhvar


@section[Change Logs]
@label[log-files]
@index[edit history]
@index[change log]

The @hemlock change log facility encourages the recording of changes to a
system by making it easy to do so.  The change log is kept in a separate file
so that it doesn't clutter up the source code.  The name of the log for a file
is specified by the @f[Log] file option (see page @pageref[file-options].)

@defcom[com "Log Change"]
@defhvar1[var "Log Entry Template"]
@hid[Log Change] makes a new entry in the change log associated with the file.
Any changes in the current buffer are saved, and the associated log file is
read into its own buffer.  The name of the log file is determined by merging
the name specified in the @f[Log] option with the current buffer's file name,
so it is not usually necessary to put the full name there.  After inserting a
template for the log entry at the beginning of the buffer, the command enters a
recursive edit (see page @pageref[recursive-edits]) so that the text of the
entry may be filled in.  When the user exits the recursive edit, the log file
is saved.

The variable @hid[Log Entry Template] determines the format of the change log
entry.  Its value is a @clisp @f[format] control string.  The format string is
passed three string arguments: the full name of the file, the creation date for
the file and the name of the file author.  If the creation date is not
available, the current date is used.  If the author is not available then @nil
is passed.  If there is an @f[@@] in the template, then it is deleted and the
point is left at that position.
@enddefcom



@comment[@chap (Special Modes)]
@include(special-modes)



@chap[Editing Programs]


@section[Comment Manipulation]
@index[comment manipulation]
@hemlock has commenting commands which can be used in almost any language.  The
behavior of these commands is determined by several @hemlock variables which
language modes should define appropriately.

@defcom[com "Indent for Comment", bind (M-;)]
@index[indentation, comment]@label[comment-indentation]
This is the most basic commenting command.  If there is already a comment on
the current line, then this moves the point to the start of the comment.  If
there no comment, this creates an empty one.

This command normally indents the comment to start at @hid[Comment Column].
The comment indents differently in the following cases:
@begin[enumerate]
If the comment currently starts at the beginning of the line, or if the last
character in the @hid[Comment Start] appears three times, then the comment
remains unmoved.

If the last character in the @hid[Comment Start] appears two times, then the
comment is indented like a line of code.

If text on the line prevents the comment occurring in the desired position,
this places the comment at the end of the line, separated from the text by a
space.
@end[enumerate]
Although the rules about replication in the comment start are oriented toward
Lisp commenting styles, you can exploit these properties in other languages.

When given a prefix argument, this command indents any existing comment on that
many consecutive lines.  This is useful for fixing up the indentation of a
group of comments.
@enddefcom

@defcom[com "Indent New Comment Line", bind {M-j, M-Linefeed}]
This commend ends the current comment and starts a new comment on a blank line,
indenting the comment the same way that @hid[Indent for Comment] does.
When not in a comment, this command is the same as @hid[Indent New Line].
@enddefcom

@defcom[com "Up Comment Line", bind {M-p}]
@defcom1[com "Down Comment Line", bind {M-n}]
These commands are similar to @hid[Previous Line] or @hid[Next Line]
followed by @hid[Indent for Comment].  Any empty comment on the current line is
deleted before moving to the new line.
@enddefcom

@defcom[com "Kill Comment", bind (C-M-;)]
This command kills any comment on the current line.  When given a prefix
argument, it kills comments on that many consecutive lines.  @hid[Undo] will
restore the unmodified text.
@enddefcom

@defcom[com "Set Comment Column", bind (C-x ;)]
This command sets the comment column to its prefix argument.  If used without a
prefix argument, it sets the comment column to the column the point is at.
@enddefcom

@defhvar[var "Comment Start", val {nil}]
@defhvar1[var "Comment End", val {nil}]
@defhvar1[var "Comment Begin", val {nil}]
@defhvar1[var "Comment Column", val {0}]
These variables determine the behavior of the comment commands.
@begin[description]
@hid[Comment Start]@\The string which indicates the start of a comment.  If
this is @nil, then there is no defined comment syntax.

@hid[Comment End]@\The string which ends a comment.  If this is @nil, then
the comment is terminated by the end of the line.

@hid[Comment Begin]@\The string inserted to begin a new comment.

@hid[Comment Column]@\The column that normal comments start at.
@end[description]
@enddefcom


@section[Indentation]
@label[indentation]
@index[indentation]
Nearly all programming languages have conventions for indentation or leading
whitespace at the beginning of lines.  The @hemlock indentation facility is
integrated into the command set so that it interacts well with other features
such as filling and commenting.

@defcom[com "Indent", bind (Tab, C-i)]
This command indents the current line.  With a prefix argument, indents that
many lines and moves down.  Exactly what constitutes indentation depends on the
current mode (see @hid[Indent Function]).
@enddefcom

@defcom[com "Indent New Line", bind (Linefeed)]
This command starts a new indented line.  Deletes any whitespace before the
point and inserts indentation on a blank line.  The effect of this is similar
to @binding[Return] followed by @binding[Tab].  The prefix argument is passed
to @hid[New Line], which is used to insert the blank line.
@enddefcom

@defcom[com "Indent Region", bind (C-M-\)]
This command indents every line in the region.  It may be undone with
@hid[Undo].
@enddefcom

@defcom[com "Back to Indentation", bind {M-m, C-M-m}]
@index[motion, indentation]
This command moves point to the first non-whitespace character on the current
line.
@enddefcom

@defcom[com "Delete Indentation", bind (M-^, C-M-^)]
@hid[Delete Indentation] joins the current line with the previous one, deleting
excess whitespace.  This operation is the inverse of the @bf[Linefeed] command
in most modes.  Usually this leaves one space between the two joined lines, but
there are several exceptions.

The non-whitespace immediately surrounding the deleted line break determine the
amount of space inserted.
@begin[enumerate]
If the preceding character is an "@f[(]" or the following character is a
"@f[)]", then this inserts no space.

If the preceding character is a newline, then this inserts no space.  This will
happen if the previous line was blank.

If the preceding character is a sentence terminator, then this inserts two
spaces.
@end[enumerate]

When given a prefix argument, this command joins the current and next lines,
rather than the previous and current lines.
@enddefcom

@defcom[com "Quote Tab", bind (M-Tab)]
This command inserts a tab character.
@enddefcom

@defcom[com "Indent Rigidly", bind (C-x Tab, C-x C-i)]
This command changes the indentation of all the lines in the region.  Each
line is moved to the right by the number of spaces specified by the prefix
argument, which defaults to eight.  A negative prefix argument moves lines
left.
@enddefcom

@defcom[com "Center Line"]
This indents the current line so that it is centered between the left margin
and @hvarref[Fill Column].  If a prefix argument is supplied, then it is used
as the width instead of @hid[Fill Column].
@enddefcom

@defhvar[var "Indent Function", val {tab-to-tab-stop}]
The value of this variable determines how indentation is done, and it is a
function which is passed a mark as its argument.  The function should indent
the line which the mark points to.  The function may move the mark around on
the line.  The mark will be @f[:left-inserting].  The default simply inserts a
tab character at the mark.
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


@section[Language Modes]

@hemlock@comment{}'s language modes are currently fairly crude, but probably
provide better programming support than most non-extensible editors.

@defcom[com "Pascal Mode"]
@index[indentation, pascal]@index[modes, pascal]This command sets the current
buffer's major mode to @hid[Pascal].  @hid[Pascal] mode borrows parenthesis
matching from Scribe mode and indents lines under the previous line.
@enddefcom


@chap[Editing Lisp]
@index[lisp, editing]
@hemlock provides a large number of powerful commands for editing Lisp code.
It is possible for a text editor to provide a much higher level of support for
editing Lisp than ordinary programming languages, since its syntax is much
simpler.


@section[Lisp Mode]
@index[lisp mode]
@index[modes, lisp]
@hid[Lisp] mode is a major mode used for editing Lisp code.  Although most
Lisp specific commands are globally bound, @hid[Lisp] mode is necessary to
enable Lisp indentation, commenting, and parenthesis-matching.  Whenever the
user or some @hemlock mechanism turns on @hid[Lisp] mode, the mode's setup
includes locally setting @hid[Current Package] (see section @ref[lisp-package])
in that buffer if its value is non-existent there; the value used is
@f["USER"].

@defcom[com "Lisp Mode"]
This command sets the major mode of the current buffer to @hid[Lisp].
@enddefcom


@section[Form Manipulation]
@index[form manipulation]
These commands manipulate Lisp forms, the printed representations of Lisp
objects.  A form is either an expression balanced with respect to parentheses
or an atom such as a symbol or string.

@defcom[com "Forward Form", bind (C-M-f)]
@defcom1[com "Backward Form", bind (C-M-b)]
@index[motion, form]@hid[Forward Form] moves to the end of the current or
next form, while @hid[Backward Form] moves to the beginning of the current
or previous form.  A prefix argument is treated as a repeat count.
@enddefcom

@defcom[com "Forward Kill Form", bind (C-M-k)]
@defcom1[com "Backward Kill Form", bind (C-M-Delete, C-M-Backspace)]
@index[killing, form]@hid[Forward Kill Form] kills text from the point to
the end of the current form.  If at the end of a list, but inside the close
parenthesis, then kill the close parenthesis.  @hid[Backward Kill Form] is
the same, except it goes in the other direction.  A prefix argument is
treated as a repeat count.
@enddefcom

@defcom[com "Mark Form", bind (C-M-@@)]
This command sets the mark at the end of the current or next form.
@enddefcom

@defcom[com "Transpose Forms", bind (C-M-t)]
This command transposes the forms before and after the point and moves
forward.  A prefix argument is treated as a repeat count.  If the prefix
argument is negative, then the point is moved backward after the
transposition is done, reversing the effect of the equivalent positive
argument.
@enddefcom

@defcom[com "Insert ()", bind {M-(}]
This command inserts an open and a close parenthesis, leaving the point
inside the open parenthesis.  If a prefix argument is supplied, then the
close parenthesis is put at the end of the form that many forms from the
point.
@enddefcom

@defcom[com "Extract Form"]
This command replaces the current containing list with the next form.  The
entire affected area is pushed onto the kill ring.  If an argument is supplied,
that many upward levels of list nesting is replaced by the next form.  This is
similar to @hid[Extract List], but this command is more generally useful since
it works on any kind of form; it is also more intuitive since it operates on
the next form as many @hid[Lisp] mode commands do.
@enddefcom


@section[List Manipulation]

@index[list manipulation]List commands are similar to form commands, but
they only pay attention to lists, ignoring any atomic objects that may
appear.  These commands are useful because they can skip over many symbols
and move up and down in the list structure.

@defcom[com "Forward List", bind (C-M-n)]
@defcom1[com "Backward List", bind (C-M-p)]
@index[motion, list]@hid[Forward List] moves the point to immediately
after the end of the next list at the current level of list structure.  If
there is not another list at the current level, then it moves up past
the end of the containing list.
@hid[Backward List] is identical, except that it moves backward and leaves
the point at the beginning of the list.  The prefix argument is used as a
repeat count.
@enddefcom

@defcom[com "Forward Up List", bind {C-M-@bf<)>}]
@defcom1[com "Backward Up List", bind (C-M-@bf<(>, C-M-u)]
@hid[Forward Up List] moves to after the end of the enclosing list.
@hid[Backward Up List] moves to the beginning.  The prefix argument is used
as a repeat count.
@enddefcom

@defcom[com "Down List", bind (C-M-d)]
This command moves to just after the beginning of the next list.  The
prefix argument is used as a repeat count.
@enddefcom

@defcom[com "Extract List", bind (C-M-x)]
This command "extracts" the current list from the list which contains it.
The outer list is deleted, leaving behind the current list.  The entire
affected area is pushed on the kill ring, so that this possibly catastrophic
operation can be undone.  The prefix argument is used as a repeat count.
@enddefcom



@section[Defun Manipulation]

@index[defun manipulation]A @i[defun] is a list whose open parenthesis is
against the left margin.  It is called this because an occurrence of the
@f[defun] top level form usually satisfies this definition, but
other top level forms such as a @f[defstruct] and @f[defmacro] work just as
well.

@defcom[com "End of Defun", bind (@bf<C-M-e, C-M-]>)]
@defcom1[com "Beginning of Defun", bind (C-M-a, C-M-[)]
@index[motion, defun]@hid[End of Defun] moves to the end of the current
or next defun. @hid[Beginning of Defun] moves to the beginning of the
current or previous defun.  @hid[End of Defun] will not work if the
parentheses are not balanced.
@enddefcom

@defcom[com "Mark Defun", bind (C-M-h)]
This command puts the point at the beginning and the mark at the end of the
current or next defun.
@enddefcom



@section[Indentation]

@index[indentation, lisp]
One of the most important features provided by @hid[Lisp] mode is automatic
indentation of Lisp code.  Since unindented Lisp is unreadable, poorly indented
Lisp is hard to manage, and inconsistently indented Lisp is subtly misleading.
See section @ref[indentation] for a description of the general-purpose
indentation commands.  @hid[Lisp] mode uses these indentation rules:
@begin[itemize]
If in a semicolon (@f[;]) comment, then use the standard comment indentation
rules.  See page @pageref[comment-indentation].

If in a quoted string, then indent to the column one greater than the column
containing the opening double quote.  This is exactly what you want in function
documentation strings and wrapping @f[error] strings.

If there is no enclosing list, then use no indentation.

If enclosing list resembles a call to a known macro or special-form, then the
first few arguments are given greater indentation and the first body form is
indented two spaces.  If the first special argument is on the same line as the
beginning of the form, then following special arguments will be indented to the
start of the first special argument, otherwise all special arguments are
indented four spaces.

If the previous form starts on its own line, then the indentation is copied
from that form.  This rule allows the default indentation to be overridden:
once a form has been manually indented to the user's satisfaction, subsequent
forms will be indented in the same way.

If the enclosing list has some arguments on the same line as the form start,
then subsequent arguments will be indented to the start of the first argument.

If the enclosing list has no argument on the same line as the form start, then
arguments will be indented one space.
@end[itemize]


@defcom[com "Indent Form", bind (C-M-q)]
This command indents all the lines in the current form, leaving the point
unmoved.  This is undo-able.
@enddefcom

@defcom[com "Fill Lisp Comment Paragraph",
	stuff <bound to @bf[M-q] in @hid[Lisp] mode>]
@defhvar1[var "Fill Lisp Comment Paragraph Confirm", val {t}]
This fills a flushleft or indented Lisp comment.  This also fills Lisp string
literals using the proper indentation as a filling prefix.  When invoked
outside of a comment or string, this tries to fill all contiguous lines
beginning with the same initial, non-empty blankspace.  When filling a comment,
the current line is used to determine a fill prefix by taking all the initial
whitespace on the line, the semicolons, and any whitespace following the
semicolons.

When invoked outside of a comment or string, this command prompts for
confirmation before filling.  It is useful to use this for filling long
@f[export] lists or other indented text or symbols, but since this is a less
common use, this command tries to make sure that is what you wanted.  Setting
@hid[Fill Lisp Comment Paragraph Confirm] to @nil inhibits the confirmation
prompt.
@enddefcom

@defcom[com "Defindent", bind (C-M-#)]
This command prompts for the number of special arguments to associate with
the symbol at the beginning of the current or containing list.
@enddefcom

@defhvar[var "Indent Defanything", val {2}]
This is the number of special arguments implicitly assumed to be supplied in
calls to functions whose names begin with "@f[def]".  If set to @nil, this
feature is disabled.
@enddefhvar

@defcom[com "Move Over )", bind {M-)}]
This command moves past the next close parenthesis and then does the equivalent
of @hid[Indent New Line].
@enddefcom       


@section[Parenthesis Matching]

@index[parenthesis matching]Another very important facility provided by
@hid[Lisp] mode is @i[parenthesis matching].  Two different styles of
parenthesis matching are supported: highlighting and pausing.

@defhvar[var "Highlight Open Parens", val {t}]
@defhvar1[var "Open Paren Highlighting Font", val {nil}]
When @hid[Highlight Open Parens] is true, and a close paren is immediately
before the point, then @hemlock displays the matching open paren in @hid[Open
Paren Highlighting Font].

@hid[Open Paren Highlighting Font] is the string name of the font used for
paren highlighting.  Only the "@f[(]" character is used in this font.  If null,
then a reasonable default is chosen.  The highlighting font is read at
initialization time, so this variable must be set before the editor is first
entered to have any effect.
@enddefhvar

@defcom[com "Lisp Insert )", stuff <bound to @bf[)] in @hid[Lisp] mode>]
@defhvar1[var "Paren Pause Period", val {0.5}]
This command inserts a close parenthesis and then attempts to display the
matching open parenthesis by placing the cursor on top of it for
@hid[Paren Pause Period] seconds.  If there is no matching parenthesis then
beep.  If the matching parenthesis is off the top of the screen, then the line
on which it appears is displayed in the echo area.  Paren pausing may be
disabled by setting @hid[Paren Pause Period] to @nil. 
@enddefcom

The initial values shown for @hid[Highlight Open Parens] and @hid[Paren Pause
Period] are only approximately correct.  Since paren highlighting is only
meaningful in Lisp mode, @hid[Highlight Open Parens] is false globally, and
has a mode-local value of @true in Lisp mode.  It it redundant to do both
kinds of paren matching, so there is also a binding of @hid[Paren Pause Period]
to @false in Lisp mode.

Paren highlighting is only supported under @windows, so the above defaults are
conditional on the device type.  If @hemlock is started on a terminal, the
initialization code makes Lisp mode bindings of @false and @f[0.5] for
@hid[Highlight Open Parens] and @hid[Paren Pause Period].  Since these
alternate default bindings are made at initialization time, the only way to
affect them is to use the @f[after-editor-initializations] macro.


@section[Parsing Lisp]
Lisp mode has a fairly complete knowledge of Lisp syntax, but since it does
not use the reader, and must work incrementally, it can be confused by legal
constructs.  Lisp mode totally ignores the read-table, so user-defined read
macros have no effect on the editor.  In some cases, the values the @hid[Lisp
Syntax] character attribute can be changed to get a similar effect.

Lisp commands consistently treat semicolon (@f[;]) style comments as
whitespace when parsing, so a Lisp command used in a comment will affect the
next (or previous) form outside of the comment.  Since @f[#| ... |#] comments
are not recognized, they can used to comment out code, while still allowing
Lisp editing commands to be used.

Strings are parsed similarly to symbols.  When within a string, the next form
is after the end of the string, and the previous form is the beginning of the
string.


@defhvar[var "Defun Parse Goal", val {2}]
@defhvar1[var "Maximum Lines Parsed", val {500}]
@defhvar1[var "Minimum Lines Parsed", val {50}]
In order to save time, Lisp mode does not parse the entire buffer every time
a Lisp command is used.  Instead, it uses a heuristic to guess the region of
the buffer that is likely to be interesting.  These variables control the
heuristic.

Normally, parsing begins and ends on defun boundaries (an open parenthesis at
the beginning of a line).  @hid[Defun Parse Goal] specifies the number of
defuns before and after the point to parse.  If this parses fewer lines than
@hid[Minimum Lines Parsed], then parsing continues until this lower limit is
reached.  If we cannot find enough defuns within @hid[Maximum Lines Parsed]
lines then we stop on the farthest defun found, or at the point where we
stopped if no defuns were found.

When the heuristic fails, and does not parse enough of the buffer, then
commands usually act as though a syntax error was detected.  If the parse
starts in a bad place (such as in the middle of a string), then Lisp commands
will be totally confused.  Such problems can usually be eliminated by
increasing the values of some of these variables.
@enddefhvar

@defhvar[var "Parse Start Function", val {start-of-parse-block}]
@defhvar1[var "Parse End Function", val {end-of-parse-block}]
These variables determine the region of the buffer parsed.  The values are
functions that take a mark and move it to the start or end of the parse region.
The default values implement the heuristic described above.
@enddefhvar



@comment[@chap(Interacting With Lisp)]
@include(lisp)


@comment[@chap(Mail Interface)]
@include(mail)


@comment[@chap(Netnews Interface)]
@include(netnews)



@chap[System Interface]

@hemlock provides a number of commands that access operating system resources
such as the filesystem and print servers.  These commands offer an alternative
to leaving the editor and using the normal operating system command language
(such as the Unix shell), but they are implementation dependent.  Therefore,
they might not even exist in some implementations.


@section[File Utility Commands]
This section describes some general file operation commands and quick directory
commands. 

See section @ref[dired] for a description @hemlock@comment{}'s directory editing
mechanism, @hid[Dired] mode.

@defcom[com "Copy File"]
This command copies a file, allowing one wildcard in the filename.  It prompts
for source and destination specifications.

If these are both directories, then the copying process is recursive on the
source, and if the destination is in the subdirectory structure of the source,
the recursion excludes this portion of the directory tree.  Use
@f[dir-spec-1/*] to copy only the files in a source directory without
recursively descending into subdirectories.

If the destination specification is a directory, and the source is a file, then
it is copied into the destination with the same filename.

The copying process copies files maintaining the source's write date.

See the description of @hid[Dired Copy File Confirm], page
@pageref[copy-confirm], for controlling user interaction when the destination
exists.
@enddefcom

@defcom[com "Rename File"]
This command renames a file, allowing one wildcard in the filename.  It prompts
for source and destination specifications.

If the destination is a directory, then the renaming process moves file(s)
indicated by the source into the directory with their original filenames.

For Unix-based implementations, if you want to rename a directory, do not
specify the trailing slash in the source specification.
@enddefcom

@defcom[com "Delete File"]
This command prompts for the name of a file and deletes it.
@enddefcom

@defcom[com "Directory", bind (C-x C-d)]
@defcom1[com "Verbose Directory", bind (C-x C-D)]
These commands prompt for a pathname (which may contain wildcards), and display
a directory listing in a pop-up window.  If a prefix argument is supplied, then
normally hidden files such as Unix dot-files will also be displayed.  
@hid[Directory] uses a compact, multiple-column format; 
@hid[Verbose Directory] displays one file on a line, with information about
protection, size, etc.
@enddefcom


@section[Printing]

@defcom[com "Print Region"]
@defcom1[com "Print Buffer"]
@defcom1[com "Print File"]
@hid[Print Region] and @hid[Print Buffer] print the contents of the current
region and the current buffer, respectively.  @hid[Print File] prompts for a
the name of a file and prints that file.  Any error messages will be displayed
in the echo area.
@enddefcom

@defhvar[var "Print Utility", val {"/usr/cs/bin/lpr"}]
@defhvar1[var "Print Utility Switches", val {()}]
@hid[Print Utility] is the program the print commands use to send files to the
printer.  The program should act like @f[lpr]: if a filename is given as an
argument, it should print that file, and if no name appears, standard input
should be assumed.  @hid[Print Utility Switches] is a list of strings
specifying the options to pass to the program.
@enddefhvar


@section[Scribe]
@defcom[com "Scribe Buffer File",
	stuff (bound to @bf[C-x c] in @hid[Scribe] mode)]
@defhvar1[var "Scribe Buffer File Confirm", val {t}]
@defcom1[com "Scribe File"]
@hid[Scribe Buffer File] invokes @hid[Scribe Utility] on the file associated
with the current buffer.  That process's default directory is the directory of
the file.  The process sends its output to the @hid[Scribe Warnings] buffer.
Before doing anything, this asks the user to confirm saving and formatting the
file.  This prompting can be inhibited with "Scribe Buffer File Confirm".

@hid[Scribe File] invokes @hid[Scribe Utility] on a file supplied by the user
in the same manner as describe above.
@enddefcom

@defhvar[var "Scribe Utility", val {"/usr/misc/bin/scribe"}]
@defhvar1[var "Scribe Utility Switches"]
@hid[Scribe Utility] is the program the Scribe commands use to compile the text
formatting.  @hid[Scribe Utility Switches] is a list of strings whose contents
would be contiguous characters, other than space, had the user invoked this
program on a command line outside of @hemlock.  Do not include the name of the
file to compile in this variable; the Scribe commands supply this.
@enddefhvar

@defcom[com "Select Scribe Warnings", bind (Scribe: C-M-C)]
This command makes the @hid[Scribe Warnings] buffer current if it exists.
@enddefcom


@section[Miscellaneous]

@defcom[com "Manual Page"]
This command displays a Unix manual page in a buffer which is in @hid[View]
mode.  When given an argument, this puts the manual page in a pop-up display.
@enddefcom

@defcom[com "Unix Filter Region"]
This command prompts for a UNIX program and then passes the current region to
the program as standard input.  The standard output from the program is used to
replace the region.  This command is undoable.
@enddefcom



@chap[Simple Customization]

@index[customization]@hemlock can be customized and extended to a very
large degree, but in order to do much of this a knowledge of Lisp is
required.  These advanced aspects of customization are discussed in the
@i[Hemlock Command Implementor's Manual], while simpler methods of
customization are discussed here.


@section[Keyboard Macros]
@index[keyboard macros]
Keyboard macros provide a facility to turn a sequence of commands into one
command.

@defcom[com "Define Keyboard Macro", bind {C-x (}]
@defcom1[com "End Keyboard Macro", bind {C-x )}]
@hid[Define Keyboard Macro] starts the definition of a keyboard macro.  The
commands which are invoked up until @hid[End Keyboard Macro] is invoked
become the definition for the keyboard macro, thus replaying the keyboard
macro is synonymous with invoking that sequence of commands.
@enddefcom

@defcom[com "Last Keyboard Macro", bind (C-x e)]
This command is the keyboard macro most recently defined; invoking it will
replay the keyboard macro.  The prefix argument is used as a repeat count.
@enddefcom

@defcom[com "Define Keyboard Macro Key", bind (C-x M-(; )]
@defhvar1[var "Define Keyboard Macro Key Confirm", val {t}]
This command prompts for a key before going into a mode for defining keyboard
macros.  After defining the macro @hemlock binds it to the key.  If the key is
already bound, @hemlock asks for confirmation before clobbering the binding;
this prompting can be inhibited by setting @hid[Define Keyboard Macro Key
Confirm] to @nil.
@enddefcom

@defcom[com "Keyboard Macro Query", bind (C-x q)]
This command conditionalizes the execution of a keyboard macro.  When invoked
during the definition of a macro, it does nothing.  When the macro replays, it
prompts the user for a key-event indicating what action to take.  The following
commands are defined:
@begin[description]
@binding[Escape]@\
 Exit all repetitions of this keyboard macro.  More than one may have been
specified using a prefix argument.

@binding[Space, y]@\
 Proceed with the execution of the keyboard macro.

@binding[Delete, Backspace, n]@\
 Skip the remainder of the keyboard macro and go on to the next repetition, if
any.

@binding[!]@\
 Do all remaining repetitions of the keyboard macro without prompting.

@binding[.]@\
 Complete this repetition of the macro and then exit without doing any of the
remaining repetitions.

@binding[C-r]@\
 Do a recursive edit and then prompt again.
@end[description]
@enddefcom

@defcom[com "Name Keyboard Macro"]
This command prompts for the name of a command and then makes the
definition for that command the same as @hid[Last Keyboard Macro]'s current
definition.  The command which results is not clobbered when another
keyboard macro is defined, so it is possible to keep several keyboard
macros around at once.  The resulting command may also be bound to a key
using @hid[Bind Key], in the same way any other command is.
@enddefcom

Many keyboard macros are not for customization, but rather for one-shot
use, a typical example being performing some operation on each line of a file.
To add "@f[del ]" to the beginning and "@f[.*]" to the end of every line in
in a buffer, one could do this:
@begin[programexample]
C-x ( d e l Space C-e . * C-n C-a C-x ) C-u 9 9 9 C-x e
@end[programexample]
First a keyboard macro is defined which performs the desired operation on
one line, and then the keyboard macro is invoked with a large prefix
argument.  The keyboard macro will not actually execute that many times;
when the end of the buffer is reached the @binding[C-n] will get an error
and abort the execution.


@section[Binding Keys]
@index[key bindings]
@label[binding-keys]

@defcom[com "Bind Key"]
This command prompts for a command, a key and a kind of binding to make,
and then makes the specified binding.  The following kinds of bindings are
allowed:
@begin[description]
@i[buffer]@\Prompts for a buffer and then makes a key binding which is
only present when that buffer is the current buffer.

@i[mode]@\Prompts for the name of a mode and then makes a key binding which
is only in present when that mode is active in the current buffer.

@i[global]@\Makes a global key binding which is in effect when there is
no applicable mode or buffer key binding.  This is the default.
@end[description]
@enddefcom

@defcom[com "Delete Key Binding"]
This command prompts for a key binding the same way that @hid[Bind Key]
does and makes the specified binding go away.
@enddefcom

@section[Hemlock Variables]

@label[vars]@index[variables, hemlock]@index[hemlock variables]A number
of commands use @hemlock variables as flags to control their behavior.  Often
you can get a command to do what you want by setting a variable.  Generally the
default value for a variable is chosen to be the safest value for novice users.

@defcom[com "Set Variable"]
This command prompts for the name of a @hemlock variable and an expression,
then sets the current value of the variable to the result of the evaluation of
the expression.
@enddefcom


@defcom[com "Defhvar"]
Like @hid[Set Variable], this command prompts for the name of a @hemlock
variable and an expression.  Like @hid[Bind Key], this command prompts for a
place: mode, buffer or local.  The result of evaluating the expression is
defined to be the value of the named variable in the specified place.

This command is most useful for making mode or buffer local bindings of
variables.  Redefining a variable in a mode or buffer will create a
customization that takes effect only when in that mode or buffer.

Unlike @hid[Set Variable], the variable name need not be the name of an
existing variable: new variables may be defined.  If the variable is already
defined in the current environment, @hemlock copies the documentation and hooks
to the new definition.
@enddefcom


@section[Init Files]
@index[init files]
@hemlock customizations are normally put in @hemlock@comment{}'s initialization file,
"@f[hemlock-init.lisp]", or when compiled "@f[hemlock-init.fasl]".  When
starting up Lisp, use the @f[-hinit] switch to indicate a particular file.  The
contents of the init file must be Lisp code, but there is a fairly
straightforward correspondence between the basic customization commands and the
equivalent Lisp code.  Rather than describe these functions in depth here, a
brief example follows:
@begin[programexample]
;;; -*- Mode: Lisp; Package: Hemlock -*-

;;; It is necessary to specify that the customizations go in
;;; the hemlock package.
(in-package 'hemlock)

;;; Bind @hid[Kill Previous Word] to @binding[M-h].
(bind-key "Kill Previous Word" '#(#\m-h))
;;;
;;; Bind @hid[Extract List] to @binding[C-M-?] when in @hid[Lisp] mode.
(bind-key "Extract List" '#(#\c-m-?) :mode "Lisp")

;;; Make @binding[C-w] globally unbound.
(delete-key-binding '#(#\c-w))

;;; Make string searches case-sensitive.
(setv string-search-ignore-case nil)
;;;
;;; Make "Query Replace" replace strings literally.
(setv case-replace nil)
@end[programexample]
For a detailed description of these functions, see the @i[Hemlock Command
Implementor's Manual].
