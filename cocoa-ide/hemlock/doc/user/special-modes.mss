@comment{-*- Dictionary: bld:scribe/hem/hem; Mode: spell; Package: Hemlock -*-}
@chap[Special Modes]

@section[Dired Mode]
@label[dired]
@index[directory editing]

@hemlock provides a directory editing mechanism.  The user can flag files and
directories for deletion, undelete flagged files, and with a keystroke read in
files and descend into directories.  In some implementations, it also supports
copying, renaming, and a simple wildcard feature.


@subsection[Inspecting Directories]
@defcom[com "Dired", bind (C-x C-M-d)]
This command prompts for a directory and fills a buffer with a verbose listing
of that directory.  When the prefix argument is supplied, this includes Unix
dot files.  If a dired buffer already exists for the directory, this switches
to the buffer and makes sure it displays dot files if appropriate.
@enddefcom

@defcom[com "Dired with Pattern", bind (C-x C-M-d)]
This command prompts for a directory and a pattern that may contain at most one
wildcard, an asterisk, and it fills a buffer with a verbose listing of the
files in the directory matching the pattern.  When the prefix argument is
supplied, this includes Unix dot files.  If a dired buffer already exists for
this directory, this switches to the buffer and makes sure it displays dot
files if appropriate.
@enddefcom

@defcom[com "Dired from Buffer Pathname"]
This command invokes @hid[Dired] on the directory name of the current buffer's
pathname.
@enddefcom

@defcom[com "Dired Help", bind (Dired: ?)]
This command pops up a help window listing the various @hid[Dired] commands.
@enddefcom

@defcom[com "Dired View File", bind (Dired: Space)]
@defcom1[com "Dired Edit File", bind (Dired: e)]
These command read in the file on the current line with the point.  If the line
describes a directory instead of a file, then this command effectively invokes
@hid[Dired] on the specification.  This associates the file's buffer with the
@hid[Dired] buffer.

@hid[Dired View File] reads in the file as if by @hid[View File], and
@hid[Dired Edit File] as if by @hid[Find File].

@hid[Dired View File] always reads into a newly created buffer, warning if the
file already exists in some buffer.
@enddefcom

@defcom[com "Dired Up Directory", bind (Dired: ^)]
This command invokes @hid[Dired] on the directory up one level from the current
@hid[Dired] buffer.  This is useful for going backwards after repeatedly
invoking @hid[Dired View File] and descending into a series of subdirectories.
Remember, @hid[Dired] only generates directory listings when no buffer contains
a dired for the specified directory.
@enddefcom

@defcom[com "Dired Update Buffer", bind (Dired: H-u)]
This command is useful when the user knows the directory in the current
@hid[Dired] buffer has changed.  @hemlock cannot know the directory structure
has changed, but the user can explicitly update the buffer with this command
instead of having to delete it and invoke @hid[Dired] again.
@enddefcom

@defcom[com "Dired Next File"]
@defcom1[com "Dired Previous File"]
These commands move to next or previous undeleted file.
@enddefcom


@subsection[Deleting Files]
@defcom[com "Dired Delete File and Down Line", bind (Dired: d)]
This command marks for deletion the file on the current line with the point and
moves point down a line.
@enddefcom

@defcom[com "Dired Delete File with Pattern", bind (Dired: D)]
This command prompts for a name pattern that may contain at most one wildcard,
an asterisk, and marks for deletion all the names matching the pattern.
@enddefcom

@defcom[com "Dired Delete File", bind (Dired: C-d)]
This command marks for deletion the file on the current line with the point
without moving the point.
@enddefcom


@subsection[Undeleting Files]
@defcom[com "Dired Undelete File and Down Line", bind (Dired: u)]
This command unmarks for deletion the file on the current line with the point
and moves point down a line.
@enddefcom

@defcom[com "Dired Undelete File with Pattern", bind (Dired: U)]
This command prompts for a name pattern that may contain at most one wildcard,
an asterisk, and unmarks for deletion all the names matching the pattern.
@enddefcom

@defcom[com "Dired Undelete File", bind (Dired: C-u)]
This command unmarks for deletion the file on the current line with the point
without moving the point.
@enddefcom


@subsection[Expunging and Quitting]
@defcom[com "Dired Expunge Files", bind (Dired: !)]
@defhvar1[var "Dired File Expunge Confirm", val {t}]
@defhvar1[var "Dired Directory Expunge Confirm", val {t}]
This command deletes files marked for deletion, asking the user for
confirmation once for all the files flagged.  It recursively deletes any marked
directories, asking the user for confirmation once for all those marked.
@hid[Dired File Expunge Confirm] and @hid[Dired Directory Expunge Confirm] when
set to @nil individually inhibit the confirmation prompting for the appropriate
deleting.
@enddefcom

@defcom[com "Dired Quit", bind (Dired: q)]
This command expunges any marked files or directories as if by @hid[Expunge
Dired Files] before deleting the @hid[Dired] buffer.
@enddefcom


@subsection[Copying Files]
@defcom[com "Dired Copy File", bind (Dired: c)]
This command prompts for a destination specification and copies the file on the
line with the point.  When prompting, the current line's specification is the
default, which provides some convenience in supplying the destination.  The
destination is either a directory specification or a file name, and when it is
the former, the source is copied into the directory under its current file name
and extension.
@enddefcom

@defcom[com "Dired Copy with Wildcard", bind (Dired: C)]
This command prompts for a name pattern that may contain at most one wildcard,
an asterisk, and copies all the names matching the pattern.  When prompting for
a destination, this provides the @hid[Dired] buffer's directory as a default.
The destination is either a directory specification or a file name with a
wildcard.  When it is the former, all the source files are copied into the
directory under their current file names and extensions.  When it is the later,
each sources file's substitution for the wildcard causing it to match the first
pattern replaces the wildcard in the destination pattern; for example, you
might want to copy @f["*.txt"] to @f["*.text"].
@enddefcom

@defhvar[var "Dired Copy File Confirm", val {t}]
@label[copy-confirm]
This variable controls interaction with the user when it is not obvious what
the copying process should do.  This takes one of the following values:
@Begin[Description]
@true@\
When the destination specification exists, the copying process stops and asks
the user if it should overwrite the destination.

@nil@\
The copying process always copies the source file to the destination
specification without interacting with the user.

@kwd[update]@\
When the destination specification exists, and its write date is newer than
the source's write date, then the copying process stops and asks the user if it
should overwrite the destination.
@End[Description]
@enddefhvar


@subsection[Renaming Files]
@defcom[com "Dired Rename File", bind (Dired: r)]
Rename the file or directory under the point
@enddefcom

@defcom[com "Dired Rename with Wildcard", bind (Dired: R)]
Rename files that match a pattern containing ONE wildcard.
@enddefcom

@defhvar[var "Dired Rename File Confirm", val {t}]
When non-nil, @hid[Dired] will query before clobbering an existing file.
@enddefhvar


@section[View Mode]
@hid[View] mode provides for scrolling through a file read-only, terminating
the buffer upon reaching the end.

@defcom[com "View File"]
This command reads a file into a new buffer as if by "Visit File", but
read-only.  Bindings exist for scrolling and backing up in a single key stroke.
@enddefcom

@defcom[com "View Help", bind (View: ?)]
This command shows a help message for @hid[View] mode.
@enddefcom

@defcom[com "View Edit File", bind (View: e)]
This commands makes a buffer in @hid[View] mode a normal editing buffer,
warning if the file exists in another buffer simultaneously.
@enddefcom

@defcom[com "View Scroll Down", bind (View: Space)]
@defhvar1[var "View Scroll Deleting Buffer", val {t}]
This command scrolls the current window down through its buffer.  If the end of
the file is visible, then this deletes the buffer if @hid[View Scroll Deleting
Buffer] is set.  If the buffer is associated with a @hid[Dired] buffer, this
returns there instead of to the previous buffer.
@enddefcom

@defcom[com "View Return", bind (View: ^)]
@defcom1[com "View Quit", bind (View: q)]
These commands invoke a function that returns to the buffer that created the
current buffer in @hid[View] mode.  Sometimes this function does nothing, but
it is useful for returning to @hid[Dired] buffers and similar @hemlock
features.

After invoking the viewing return function if there is one, @hid[View Quit]
deletes the buffer that is current when the user invokes it.
@enddefcom

Also, bound in @hid[View] mode are the following commands:
@Begin[Description]
@binding[backspace], @binding[delete]@\Scrolls the window up.

@binding[<]@\Goes to the beginning of the buffer.

@binding[>]@\Goes to the end of the buffer.
@End[Description]


@section[Process Mode]
@Label[process]
@Index[shells]
@Index[processes]

@hid[Process] mode allows the user to execute a Unix process within a @hemlock
buffer.  These commands and default bindings cater to running Unix shells in
buffers.  For example, @hid[Stop Buffer Subprocess] is bound to @binding[H-z]
to stop the process you are running in the shell instead of binding @hid[Stop
Main Process] to this key which would stop the shell itself.

@defcom[com "Shell", bind (C-M-s)]
@defhvar1[var "Shell Utility", val {"/bin/csh"}]
@defhvar1[var "Shell Utility Switches", val {@nil}]
@defhvar1[var "Current Shell"]
@defhvar1[var "Ask about Old Shells"]
This command executes the process determined by the values of @hid(Shell
Utility) and @hid(Shell Utility Switches) in a new buffer named @f["Shell n"]
where @f["n"] is some distinguishing integer.

@hid[Current Shell] is a @hemlock variable that holds to the current shell
buffer.  When @hid[Shell] is invoked, if there is a @hid[Current Shell], the
command goes to that buffer.

When there is no @hid[Current Shell], but shell buffers do exist, if @hid[Ask
about Old Shells] is set, the @hid[Shell] command prompts for one of them,
setting @hid[Current Shell] to the indicated shell, and goes to the buffer.

Invoking @hid[Shell] with an argument forces the creation of a new shell
buffer.

@hid[Shell Utility] is the string name of the process to execute.

@hid[Shell Utility Switches] is a string containing the default command line
arguments to @hid[Shell Utility].  This is a string since the utility is
typically @f["/bin/csh"], and this string can contain I/O redirection and other
shell directives.
@enddefcom

@defcom[com "Shell Command Line in Buffer"]
This command prompts for a buffer and a shell command line.  It then runs a
shell, giving it the command line, in the buffer.
@enddefcom

@defcom[com "Set Current Shell"]
This command sets the value of @hid[Current Shell].
@enddefcom

@defcom[com "Stop Main Process"]
This command stops the process running in the current buffer by sending a
@f[:SIGTSTP] to that process.  With an argument, stops the process using
@f[:SIGSTOP].
@enddefcom

@defcom[com "Continue Main Process"]
If the process in the current buffer is stopped, this command continues it.
@enddefcom

@defcom[com "Kill Main Process"]
@defhvar1[var "Kill Process Confirm", val {t}]
This command prompts for confirmation and kills the process running in the
current buffer.

Setting this variable to @nil inhibits @hemlock@comment{}'s prompting for confirmation.
@enddefcom

@defcom[com "Stop Buffer Subprocess", stuff (bound to @bf[H-z] in @hid[Process] mode)]
This command stops the foreground subprocess of the process in the current
buffer, similar to the effect of @binding[C-Z] in a shell.
@enddefcom

@defcom[com "Kill Buffer Subprocess"]
This command kills the foreground subprocess of the process in the current
buffer.
@enddefcom

@defcom[com "Interrupt Buffer Subprocess", stuff (bound to  @bf[H-c] in @hid[Process] mode)]
This command interrupts the foreground subprocess of the process in the
current buffer, similar to the effect of @binding[C-C] in a shell.
@enddefcom

@defcom[com "Quit Buffer Subprocess", stuff (bound to @bf[H-\] in @hid[Process] mode)]
This command dumps the core of the foreground subprocess of the processs in
the current buffer, similar to the effect of @binding[C-\] in a shell.
@enddefcom

@defcom[com "Send EOF to Process", stuff (bound to @bf[H-d] in @hid[Process] mode)]
This command sends the end of file character to the process in the current
buffer, similar to the effect of @binding[C-D] in a shell.
@enddefcom

@defcom[com "Confirm Process Input", stuff (bound to @bf[Return] in @hid[Process] mode)]
This command sends the text the user has inserted at the end of a process
buffer to the process in that buffer.  Resulting output is inserted at the end
of the process buffer.
@enddefcom

The user may edit process input using commands that are shared with
@hid[Typescript] mode, see section @ref[typescripts].


@section[Bufed Mode]
@hemlock provides a mechanism for managing buffers as an itemized list.
@hid[Bufed] supports conveniently deleting several buffers at once, saving
them, going to one, etc., all in a key stroke.

@defcom[com "Bufed", bind (C-x C-M-b)]
This command creates a list of buffers in a buffer supporting operations such
as deletion, saving, and selection.  If there already is a @hid[Bufed] buffer,
this just goes to it.
@enddefcom

@defcom[com "Bufed Help"]
This command pops up a display of @hid[Bufed] help.
@enddefcom

@defcom[com "Bufed Delete", bind (Bufed: C-d, C-D, D, d)]
@defhvar1[var "Virtual Buffer Deletion", val {t}]
@defhvar1[var "Bufed Delete Confirm", val {t}]
@hid[Bufed Delete] deletes the buffer on the current line.

When @hid[Virtual Buffer Deletion] is set, this merely flags the buffer for
deletion until @hid[Bufed Expunge] or @hid[Bufed Quit] executes.

Whenever these commands actually delete a buffer, if @hid[Bufed Delete Confirm]
is set, then @hemlock prompts the user for permission; if more than one buffer
is flagged for deletion, this only prompts once.  For each modified buffer,
@hemlock asks to save the buffer before deleting it.
@enddefcom

@defcom[com "Bufed Undelete", bind (Bufed: U, u)]
This command undeletes the buffer on the current line.
@enddefcom

@defcom[com "Bufed Expunge", bind (Bufed: !)]
This command expunges any buffers marked for deletion regarding @hid[Bufed
Delete Confirm].
@enddefcom

@defcom[com "Bufed Quit", bind (Bufed: q)]
This command kills the @hid[Bufed] buffer, expunging any buffers marked for
deletion.
@enddefcom

@defcom[com "Bufed Goto", bind (Bufed: Space)]
This command selects the buffer on the current line, switching to it.
@enddefcom

@defcom[com "Bufed Goto and Quit", bind (Bufed: S-leftdown)]
This command goes to the buffer under the pointer, quitting @hid[Bufed].  It
supplies a function for @hid[Generic Pointer Up] which is a no-op.
@enddefcom

@defcom[com "Bufed Save File", bind (Bufed: s)]
This command saves the buffer on the current line.
@enddefcom


@section[Completion]
This is a minor mode that saves words greater than three characters in length,
allowing later completion of those words.  This is very useful for the often
long identifiers used in Lisp programs.  As you type a word, such as a Lisp
symbol when in @hid[Lisp] mode, and you progress to typing the third letter,
@hemlock displays a possible completion in the status line.  You can then
rotate through the possible completions or type some more letters to narrow
down the possibilities.  If you choose a completion, you can also rotate
through the possibilities in the buffer instead of in the status line.
Choosing a completion or inserting a character that delimits words moves the
word forward in the ring of possible completions, so the next time you enter
its initial characters, @hemlock will prefer it over less recently used
completions.

@defcom[com "Completion Mode"]
This command toggles @hid[Completion] mode in the current buffer.
@enddefcom

@defcom[com "Completion Self Insert"]
This command is like @hid[Self Insert], but it also checks for possible
completions displaying any result in the status line.  This is bound to most of
the key-events with corresponding graphic characters.
@enddefcom

@defcom[com "Completion Complete Word", bind (Completion: End)]
This command selects the currently displayed completion if there is one,
guessing the case of the inserted text as with @hid[Query Replace].  Invoking
this immediately in succession rotates through possible completions in the
buffer.  If there is no currently displayed completion on a first invocation,
this tries to find a completion from text immediately before the point and
displays the completion if found.
@enddefcom

@defcom[com "Completion Rotate Completions", bind (Completion: M-End)]
This command displays the next possible completion in the status line.  If
there is no currently displayed completion, this tries to find a completion
from text immediately before the point and displays the completion if found.
@enddefcom

@defcom[com "List Possible Completions"]
This command lists all the possible completions for the text immediately before
the point in a pop-up display.  Sometimes this is more useful than rotating
through several completions to see if what you want is available.
@enddefcom

@defhvar[var "Completion Bucket Size", val {20}]
Completions are stored in buckets determined by the first three letters of a
word. This variable limits the number of completions saved for each combination
of the first three letters of a word.  If you have many identifier in some
module beginning with the same first three letters, you'll need increase this
variable to accommodate all the names.
@enddefhvar


@defcom[com "Save Completions"]
@defcom1[com "Read Completions"]
@defhvar1[var "Completion Database Filename", val {nil}]
@hid[Save Completions] writes the current completions to the file
@hid[Completion Database Filename].  It writes them, so @hid[Read Completions]
can read them back in preserving the most-recently-used order.  If the user
supplies an argument, then this prompts for a pathname.

@hid[Read Completions] reads completions saved in @hid[Completion Database
Filename].  It moves any current completions to a less-recently-used status,
and it removes any in a given bucket that exceed the limit @hid[Completion
Bucket Size].
@enddefcom

@defcom[com "Parse Buffer for Completions"]
This command passes over the current buffer putting each valid completion word
into the database.  This is a good way of picking up many useful completions
upon visiting a new file for which there are no saved completions.
@enddefcom


@section[CAPS-LOCK Mode]

@hid[CAPS-LOCK] is a minor mode in which @hemlock that inserts all alphabetic
characters as uppercase letters.

@defcom[com "Caps Lock Mode"]
This command toggles @hid[CAPS-LOCK] mode for the current buffer; it is most
useful when bound to a key, so you can enter and leave @hid[CAPS-LOCK] mode
casually.
@enddefcom

@defcom[com "Self Insert Caps Lock"]
This command inserts the uppercase version of the character corresponding to
the last key-event typed.
@enddefcom



@section[Overwrite Mode]

@hid[Overwrite] mode is a minor mode which is useful for creating figures and
tables out of text.  In this mode, typing a key-event with a corresponding
graphic character replaces the character at the point instead of inserting the
character.  @hid[Quoted Insert] can be used to insert characters normally.

@defcom[com "Overwrite Mode"]
This command turns on @hid[Overwrite] mode in the current buffer.  If it is
already on, then it is turned off.  A positive argument turns @hid[Overwrite]
mode on, while zero or a negative argument turns it off.
@enddefcom

@defcom[com "Self Overwrite"]
This command replaces the next character with the character corresponding to
the key-event used to invoke the command.  After replacing the character, this
moves past it.  If the next character is a tab, this first expands the tab into
the appropriate number of spaces, replacing just the next space character.
At the end of the line, it inserts the
character instead of clobbering the newline.

This is bound to key-events with corresponding graphic characters in
@hid[Overwrite] mode.
@enddefcom

@defcom[com "Overwrite Delete Previous Character",
       stuff (bound to @bf[Delete] and @bf[Backspace] in @hid[Overwrite] mode)]
This command replaces the previous character with a space and moves backwards.
This deletes tabs and newlines.
@enddefcom


@section[Word Abbreviation]
@index[word abbreviation]
Word abbreviation provides a way to speed the typing of frequently used words
and phrases.  When in @hid[Abbrev] mode, typing a word delimiter causes the
previous word to be replaced with its @i[expansion] if there is one currently
defined.  The expansion for an abbrev may be any string, so this mode can be
used for abbreviating programming language constructs and other more obscure
uses.  For example, @hid[Abbrev] mode can be used to automatically correct
common spelling mistakes and to enforce consistent capitalization of
identifiers in programs.

@i[Abbrev] is an abbreviation for @i[abbreviation], which is used for
historical reasons.  Obviously the original writer of @hid[Abbrev] mode hated
to type long words and could hardly use @hid[Abbrev] mode while writing
@hid[Abbrev] mode. 

A word abbrev can be either global or local to a major mode.  A global word
abbrev is defined no matter what the current major mode is, while a mode word
abbrev is only defined when its mode is the major mode in the current buffer.
Mode word abbrevs can be used to prevent abbrev expansion in inappropriate
contexts.


@subsection[Basic Commands]

@defcom[com "Abbrev Mode"]
This command turns on @hid[Abbrev] mode in the current buffer.  If @hid[Abbrev]
mode is already on, it is turned off.  @hid[Abbrev] mode must be on for the
automatic expansion of word abbrevs to occur, but the abbreviation commands are
bound globally and may be used at any time.
@enddefcom

@defcom[com "Abbrev Expand Only", 
        stuff (bound to word-delimiters in @hid[Abbrev] mode)]
This is the word abbrev expansion command.  If the word before the point is a
defined word abbrev, then it is replaced with its expansion.  The replacement
is done using the same case-preserving heuristic as is used by
@hid[Query Replace].  This command is globally bound to @binding[M-Space] so
that abbrevs can be expanded when @hid[Abbrev] mode is off.  An undesirable
expansion may be inhibited by using @binding[C-q] to insert the delimiter.
@enddefcom

@defcom[com "Inverse Add Global Word Abbrev", bind (C-x -)]
@defcom1[com "Inverse Add Mode Word Abbrev", bind (C-x C-h, C-x Backspace)]
@hid[Inverse Add Global Word Abbrev] prompts for a string and makes it the
global word abbrev expansion for the word before the point.

@hid[Inverse Add Mode Word Abbrev] is identical to 
@hid[Inverse Add Global Word Abbrev] except that it defines an expansion which
is local to the current major mode.
@enddefcom

@defcom[com "Make Word Abbrev"]
This command defines an arbitrary word abbreviation.  It prompts for the mode,
abbreviation and expansion.  If the mode @f["Global"] is specified, then it
makes a global abbrev.
@enddefcom

@defcom[com "Add Global Word Abbrev", bind (C-x +)]
@defcom1[com "Add Mode Word Abbrev", bind (C-x C-a)]
@hid[Add Global Word Abbrev] prompts for a word and defines it to be a global
word abbreviation.  The prefix argument determines which text is used as the
expansion:
@begin[description]
@i[no prefix argument]@\The word before the point is used as the expansion of
the abbreviation.

@i[zero prefix argument]@\The text in the region is used as the expansion of the
abbreviation.

@i[positive prefix argument]@\That many words before the point are made the
expansion of the abbreviation.

@i[negative prefix argument]@\Do the same thing as 
@hid[Delete Global Word Abbrev] instead of defining an abbreviation.
@end[description]

@hid[Add Mode Word Abbrev] is identical to @hid[Add Global Word Abbrev] except
that it defines or deletes mode word abbrevs in the current major mode.
@enddefcom

@defcom[com "Word Abbrev Prefix Mark", bind (M-")]
This command allows @hid[Abbrev Expand Only] to recognize abbreviations when
they have prefixes attached.  First type the prefix, then use this command.  A
hyphen (@f[-]) will be inserted in the buffer.  Now type the abbreviation and
the word delimiter.  @hid[Abbrev Expand Only] will expand the abbreviation and
remove the hyphen.

Note that there is no need for a suffixing command, since 
@hid[Abbrev Expand Only] may be used explicitly by typing @binding[M-Space].
@enddefcom

@defcom[com "Unexpand Last Word", bind (C-x u)]
This command undoes the last word abbrev expansion.  If repeated, undoes its
own effect.
@enddefcom


@subsection[Word Abbrev Files]
A word abbrev file is a file which holds word abbrev definitions.  Word abbrev
files allow abbrevs to be saved so that they may be used across many editing
sessions.

@defhvar[var "Abbrev Pathname Defaults", val {(pathname "abbrev.defns")}]
This is sticky default for the following commands.  When they prompt for a file
to write, they offer this and set it for the next time one of them executes.
@enddefhvar

@defcom[com "Read Word Abbrev File"]
This command reads in a word abbrev file, adding all the definitions to those
currently defined.  If a definition in the file is different from the current
one, the current definition is replaced.
@enddefcom

@defcom[com "Write Word Abbrev File"]
This command prompts for a file and writes all currently defined word abbrevs
out to it.
@enddefcom

@defcom[com "Append to Word Abbrev File"]
This command prompts for a word abbrev file and appends any new definitions to
it.  An abbrev is new if it has been defined or redefined since the last use of
this command.  Definitions made by reading word abbrev files are not
considered.
@enddefcom


@subsection[Listing Word Abbrevs]
@defcom[com "List Word Abbrevs"]
@defcom1[com "Word Abbrev Apropos"]
@hid[List Word Abbrevs] displays a list of each defined word abbrev, with its
mode and expansion.

@hid[Word Abbrev Apropos] is similar, except that it only displays abbrevs
which contain a specified string, either in the definition, expansion or mode.
@enddefcom

@subsection[Editing Word Abbrevs]
Word abbrev definition lists are edited by editing the text representation
of the definitions.  Word abbrev files may be edited directly, like any other
text file.  The set of abbrevs currently defined in @hemlock may be edited
using the commands described in this section.

The text representation of a word abbrev is fairly simple.  Each definition
begins at the beginning of a line.  Each line has three fields which are
separated by ASCII tab characters.  The fields are the abbreviation, the mode
of the abbreviation and the expansion.  The mode is represented as the mode
name inside of parentheses.  If the abbrev is global, then the mode field is
empty.  The expansion is represented as a quoted string since it may contain
any character.  The string is quoted with double-quotes (@f["]); double-quotes
in the expansion are represented by doubled double-quotes.  The expansion may
contain newline characters, in which case the definition will take up more than
one line.

@defcom[com "Edit Word Abbrevs"]
This command inserts the current word abbrev definitions into the 
@hid[Edit Word Abbrevs] buffer and then enters a recursive edit on the buffer.
When the recursive edit is exited, the definitions in the buffer become the new
current abbrev definitions.
@enddefcom

@defcom[com "Insert Word Abbrevs"]
This command inserts at the point the text representation of the currently
defined word abbrevs.
@enddefcom

@defcom[com "Define Word Abbrevs"]
This command interprets the text of the current buffer as a word abbrev
definition list, adding all the definitions to those currently defined.
@enddefcom


@subsection[Deleting Word Abbrevs]
The user may delete word abbrevs either individually or collectively.
Individual abbrev deletion neutralizes single abbrevs which have outlived their
usefulness; collective deletion provides a clean slate from which to initiate
abbrev definitions.

@defcom[com "Delete All Word Abbrevs"]
This command deletes all word abbrevs which are currently defined.
@enddefcom

@defcom[com "Delete Global Word Abbrev"]
@defcom1[com "Delete Mode Word Abbrev"]
@hid[Delete Global Word Abbrev] prompts for a word abbreviation and deletes its
global definition.  If given a prefix argument, deletes all global abbrev
definitions.

@hid[Delete Mode Word Abbrev] is identical to @hid[Delete Global Word Abbrev]
except that it deletes definitions in the current major mode.
@enddefcom


@section[Lisp Library]
This is an implementation dependent feature.  The Lisp library is a collection
of local hacks that users can submit and share that is maintained by the Lisp
group.  These commands help peruse the catalog or description files and figure
out how to load the entries.

@defcom[com "Lisp Library"]
This command finds all the library entries and lists them in a buffer.  The
following commands describe and load those entries.
@enddefcom

@defcom[com "Describe Library Entry", bind (Lisp-Lib: space)]
@defcom1[com "Describe Pointer Library Entry", bind (Lisp-Lib: leftdown)]
@defcom1[com "Load Library Entry", bind (Lisp-Lib: rightdown)]
@defcom1[com "Load Pointer Library Entry", bind (Lisp-Lib: l)]
@defcom1[com "Editor Load Library Entry"]
@defcom1[com "Editor Load Pointer Library Entry"]
@hid[Load Library Entry] and @hid[Load Pointer Library Entry] load the library
entry indicated by the line on which the point lies or where the user clicked
the pointer, respectively.  These load the entry into the current slave Lisp.

@hid[Editor Load Library Entry] and @hid[Editor Load Pointer Library Entry] are
the same, but they load the entry into the editor Lisp.
@enddefcom

@defcom[com "Exit Lisp Library", bind (Lisp-Lib: q)]
This command deletes the @hid[Lisp Library] buffer.
@enddefcom

@defcom[com "Lisp Library Help", bind (Lisp-Lib: ?)]
This command pops up a help window listing @hid[Lisp-Lib] commands.
@enddefcom
