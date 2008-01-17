@comment{-*- Dictionary: /afs/cs/project/clisp/scribe/hem/hem; Mode: spell; Package: Hemlock -*-}
@chap[Interacting With Lisp]
@index[lisp, interaction with]

Lisp encourages highly interactive programming environments by requiring
decisions about object type and function definition to be postponed until run
time.  @hemlock supports interactive programming in @llisp by providing
incremental redefinition and environment examination commands.  @hemlock also
uses Unix TCP sockets to support multiple Lisp processes, each of which may be
on any machine.


@section[Eval Servers]
@label[eval-servers]
@index[eval servers]

@hemlock runs in the editor process and interacts with other Lisp processes
called @i[eval servers].  A user's Lisp program normally runs in an eval
server process.  The separation between editor and eval server has several
advantages:
@begin[itemize]
The editor is protected from any bad things which may happen while debugging a
Lisp program.

Editing may occur while running a Lisp program.

The eval server may be on a different machine, removing the load from the
editing machine.

Multiple eval servers allow the use of several distinct Lisp environments.
@end[itemize]
Instead of providing an interface to a single Lisp environment, @hemlock
coordinates multiple Lisp environments.


@subsection[The Current Eval Server]
@index[current eval server]
Although @hemlock can be connected to several eval servers simultaneously, one
eval server is designated as the @i[current eval server].  This is the eval
server used to handle evaluation and compilation requests.  Eval servers are
referred to by name so that there is a convenient way to discriminate between
servers when the editor is connected to more than one.  The current eval server
is normally globally specified, but it may also be shadowed locally in specific
buffers.

@defcom[com "Set Eval Server"]
@defcom1[com "Set Buffer Eval Server"]
@defcom1[com "Current Eval Server"]
@hid[Set Eval Server] prompts for the name of an eval server and makes it the
the current eval server.  @hid[Set Buffer Eval Server] is the same except that
is sets the eval server for the current buffer only.  @hid[Current Eval Server]
displays the name of the current eval server in the echo area, taking any
buffer eval server into consideration.  See also @comref[Set Compile Server].
@enddefcom


@subsection[Slaves]
@index[slave buffers]
@index[slaves]
For now, all eval servers are @i[slaves].  A slave is a Lisp process that uses
a typescript (see page @pageref[typescripts]) to run its top-level
@f[read-eval-print] loop in a @hemlock buffer.  We refer to the buffer that a
slave uses for I/O as its @i[interactive] or @i[slave] buffer.  The name of the
interactive buffer is the same as the eval server's name.

@index[background buffers]
@hemlock creates a @i[background] buffer for each eval server.  The background
buffer's name is @w<@hid[Background ]@i{name}>, where @i[name] is the name of
the eval server.  Slaves direct compiler warning output to the background
buffer to avoid cluttering up the interactive buffer.

@hemlock locally sets @hid[Current Eval Server] in interactive and background
buffers to their associated slave.  When in a slave or background buffer, eval
server requests will go to the associated slave, regardless of the global value
of @hid[Current Eval Server].

@defcom[com "Select Slave", bind (C-M-c)]
This command changes the current buffer to the current eval server's
interactive buffer.  If the current eval server is not a slave, then it beeps.
If there is no current eval server, then this creates a slave (see section
@ref[slave-creation]).  If a prefix argument is supplied, then this creates a
new slave regardless of whether there is a current eval server.  This command
is the standard way to create a slave.

The slave buffer is a typescript (see page @pageref[typescripts]) the slave
uses for its top-level @f[read-eval-print] loop.
@enddefcom

@defcom[com "Select Background", bind (C-M-C)]
This command changes the current buffer to the current eval server's background
buffer.  If there is no current eval server, then it beeps.
@enddefcom


@subsection[Slave Creation and Destruction]
@label[slave-creation]
When @hemlock first starts up, there is no current eval server.  If there is no
a current eval server, commands that need to use the current eval server will
create a slave as the current eval server.

If an eval server's Lisp process terminates, then we say the eval server is
dead.  @hemlock displays a message in the echo area, interactive, and
background buffers whenever an eval server dies.  If the user deletes an
interactive or background buffer, the associated eval server effectively
becomes impotent, but @hemlock does not try to kill the process.  If a command
attempts to use a dead eval server, then the command will beep and display a
message.

@defhvar[var "Confirm Slave Creation", val {t}]
If this variable is true, then @hemlock always prompts the user for
confirmation before creating a slave.
@enddefhvar

@defhvar[var "Ask About Old Servers", val {t}]
If this variable is true, and some slave already exists, @hemlock prompts the
user for the name of an existing server when there is no current server,
instead of creating a new one.
@enddefhvar

@defcom[com "Editor Server Name"]
This command echos the editor server's name, the machine and port of the
editor, which is suitable for use with the Lisp processes -slave switch.
See section @ref[slave-switch].
@enddefcom

@defcom[com "Accept Slave Connections"]
This command cause @hemlock to accept slave connections, and it displays the
editor server's name, which is suitable for use with the Lisp processes -slave
switch.  See section @ref[slave-switch].  Supplying an argument causes this
command to inhibit slave connections.
@enddefcom

@defhvar[var "Slave Utility", val {"/usr/misc/.lisp/bin/lisp"}]
@defhvar1[var "Slave Utility Switches"]
A slave is started by running the program @hid[Slave Utility Name] with
arguments specified by the list of strings @hid[Slave Utility Switches].  This
is useful primarily when running customized Lisp systems.  For example,
setting @hid[Slave Utility Switches] to @f[("-core" "my.core")] will cause
"@f[/usr/hqb/my.core]" to be used instead of the default core image.

The @f[-slave] switch and the editor name are always supplied as arguments, and
should remain unspecified in @hid[Slave Utility Switches].
@enddefhvar

@defcom[com "Kill Slave"]
@defcom1[com "Kill Slave and Buffers"]
@hid[Kill Slave] prompts for a slave name, aborts any operations in the slave,
tells the slave to @f[quit], and shuts down the connection to the specified
eval server.  This makes no attempt to assure the eval server actually dies.

@hid[Kill Slave and Buffers] is the same as @hid[Kill Slave], but it also
deletes the interactive and background buffers.
@enddefcom


@subsection[Eval Server Operations]

@label[operations]
@index[eval server operations]@index[operations, eval server]
@hemlock handles requests for compilation or evaluation by queuing an
@i[operation] on the current eval server.  Any number of operations may be
queued, but each eval server can only service one operation at a time.
Information about the progress of operations is displayed in the echo area.

@defcom[com "Abort Operations", bind (C-c a)]
This command aborts all operations on the current eval server, either queued or
in progress.  Any operations already in the @f[Aborted] state will be flushed.
@enddefcom

@defcom[com "List Operations", bind (C-c l)]
This command lists all operations which have not yet completed.  Along with a
description of the operation, the state and eval server is displayed.  The
following states are used:
@begin[description]
@f[Unsent]@\The operation is in local queue in the editor, and hasn't been sent
yet.

@f[Pending]@\The operation has been sent, but has not yet started execution.

@f[Running]@\The operation is currently being processed.

@f[Aborted]@\The operation has been aborted, but the eval server has not yet
indicated termination.
@end[description]
@enddefcom


@section[Typescripts]
@label[typescripts]
@index[typescripts]

Both slave buffers and background buffers are typescripts.  The typescript
protocol allows other processes to do stream-oriented interaction in a @hemlock
buffer similar to that of a terminal.  When there is a typescript in a buffer,
the @hid[Typescript] minor mode is present.  Some of the commands described in
this section are also used by @hid[Eval] mode (page @pageref[eval-mode].)

Typescripts are simple to use.  @hemlock inserts output from the process into
the buffer.  To give the process input, use normal editing to insert the input
at the end of the buffer, and then type @bf[Return] to confirm sending the
input to the process.

@defcom[com "Confirm Typescript Input", 
        stuff (bound to @bf[Return] in @hid[Typescript] mode)]
@defhvar1[var "Unwedge Interactive Input Confirm", val {t}]
This command sends text that has been inserted at the end of the current buffer
to the process reading on the buffer's typescript.  Before sending the text,
@hemlock moves the point to the end of the buffer and inserts a newline.

Input may be edited as much as is desired before it is confirmed; the result
of editing input after it has been confirmed is unpredictable.  For this reason,
it is desirable to postpone confirming of input until it is actually complete.
The @hid[Indent New Line] command is often useful for inserting newlines
without confirming the input.

If the process reading on the buffer's typescript is not waiting for input,
then the text is queued instead of being sent immediately.  Any number of
inputs may be typed ahead in this fashion.  @hemlock makes sure that the inputs
and outputs get interleaved correctly so that when all input has been read, the
buffer looks the same as it would have if the input had not been typed ahead.

If the buffer's point is before the start of the input area, then various
actions can occur.  When set, @hid[Unwedge Interactive Input Confirm] causes
@hemlock to ask the user if it should fix the input buffer which typically
results in ignoring any current input and refreshing the input area at the end
of the buffer.  This also has the effect of throwing the slave Lisp to top
level, which aborts any pending operations or queued input.  This is the only
way to be sure the user is cleanly set up again after messing up the input
region.  When this is @nil, @hemlock simply beeps and tells the user in the
@hid[Echo Area] that the input area is invalid.
@enddefcom

@defcom[com "Kill Interactive Input", 
    stuff (bound to @bf[M-i] in @hid[Typescript] and @hid[Eval] modes)]
This command kills any input that would have been confirmed by @bf[Return].
@enddefcom

@defcom[com "Next Interactive Input",  
        stuff (bound to @bf[M-n] in @hid[Typescript] and @hid[Eval] modes)]
@defcom1[com "Previous Interactive Input",
        stuff (bound to @bf[M-p] in @hid[Typescript] and @hid[Eval] modes)]
@defcom1[com "Search Previous Interactive Input",
	stuff (bound to @bf[M-P] in @hid[Typescript] and @hid[Eval] modes)]
@defhvar1[var "Interactive History Length", val {10}]
@defhvar1[var "Minimum Interactive Input Length", val {2}]
@index[history, typescript]
@Hemlock maintains a history of interactive inputs.  @hid[Next Interactive
Input] and @hid[Previous Interactive Input] step forward and backward in the
history, inserting the current entry in the buffer.  The prefix argument is
used as a repeat count.

@hid[Search Previous Interactive Input] searches backward through the
interactive history using the current input as a search string.  Consecutive
invocations repeat the previous search.

@hid[Interactive History Length] determines the number of entries with which
@hemlock creates the buffer-specific histories.  @Hemlock only adds an input
region to the history if its number of characters exceeds @hid[Minimum
Interactive Input Length].
@enddefcom

@defcom[com "Reenter Interactive Input",
	stuff (bound to @bf[C-Return] in @hid[Typescript] and @hid[Eval] modes)]
 This copies to the end of the buffer the form to the left of the buffer's
point.  When the current region is active, this copies it instead.  This is
sometimes easier to use to get a previous input that is either so far back that
it has fallen off the history or is visible and more readily @i[yanked] than
gotten with successive invocations of the history commands.
@enddefcom

@defcom[com "Interactive Beginning of Line", 
        stuff (bound to @bf[C-a] in @hid[Typescript] and @hid[Eval] modes)]
This command is identical to @hid[Beginning of Line] unless there is no
prefix argument and the point is on the same line as the start of the current
input; then it moves to the beginning of the input.  This is useful since it
skips over any prompt which may be present.
@enddefcom

@defhvar[var "Input Wait Alarm", val {:loud-message}]
@defhvar1[var "Slave GC Alarm", val {:message}]
@hid[Input Wait Alarm] determines what action to take when a slave Lisp goes
into an input wait on a typescript that isn't currently displayed in any
window.  @hid[Slave GC Alarm] determines what action to take when a slave
notifies that it is GC'ing.

The following are legal values:
@begin[description]
@kwd[loud-message]@\Beep and display a message in the echo area indicating
which buffer is waiting for input.

@kwd[message]@\Display a message, but don't beep.

@nil@\Don't do anything.
@end[description]
@enddefhvar

@defcom[com "Typescript Slave BREAK", bind (Typescript: H-b)]
@defcom1[com "Typescript Slave to Top Level", bind (Typescript: H-g)]
@defcom1[com "Typescript Slave Status", bind (Typescript: H-s)]
Some typescripts have associated information which these commands access
allowing @hemlock to control the process which uses the typescript.

@hid[Typescript Slave BREAK] puts the current process in a break loop so that
you can be debug it.  This is similar in effect to an interrupt signal (@f[^C]
or @f[^\] in the editor process).

@hid[Typescript Slave to Top Level] causes the current process to throw to the
top-level @f[read-eval-print] loop.  This is similar in effect to a quit signal
(@f[^\]).

@hid[Typescript Slave Status] causes the current process to print status
information on @var[error-output]:
@lisp
; Used 0:06:03, 3851 faults.  In: SYSTEM:SERVE-EVENT
@endlisp
The message displays the process run-time, the total number of page faults and
the name of the currently running function.   This command is useful for
determining whether the slave is in an infinite loop, waiting for input, or
whatever.
@enddefcom


@section[The Current Package]
@label[lisp-package]
@index[package]
The current package is the package which Lisp interaction commands use.  The
current package is specified on a per-buffer basis, and defaults to "@f[USER]".
If the current package does not exist in the eval server, then it is created.
If evaluation is being done in the editor process and the current package
doesn't exist, then the value of @f[*package*] is used.  The current package is
displayed in the modeline (see section @ref[modelines].)  Normally the package
for each file is specified using the @f[Package] file option (see page
@pageref[file-options].)

When in a slave buffer, the current package is controlled by the value of
@var[package] in that Lisp process.  Modeline display of the current package
is inhibited in this case.

@defcom[com "Set Buffer Package"]
This command prompts for the name of a package to make the local package in the
current buffer.  If the current buffer is a slave, background, or eval buffer,
then this sets the current package in the associated eval server or editor
Lisp.  When in an interactive buffer, do not use @f[in-package]; use this
command instead.
@enddefcom


@section[Compiling and Evaluating Lisp Code]

@index[compilation]@index[evaluation]These commands can greatly speed up
the edit/debug cycle since they enable incremental reevaluation or
recompilation of changed code, avoiding the need to compile and load an
entire file.  

@defcom[com "Evaluate Expression", bind (M-Escape)]
This command prompts for an expression and prints the result of its evaluation
in the echo area.  If an error happens during evaluation, the evaluation is
simply aborted, instead of going into the debugger.  This command doesn't
return until the evaluation is complete.
@enddefcom

@defcom[com "Evaluate Defun", bind (C-x C-e)]
@defcom1[com "Evaluate Region"]
@defcom1[com "Evaluate Buffer"]
These commands evaluate text out of the current buffer, reading the current
defun, the region and the entire buffer, respectively.  The result of the
evaluation of each form is displayed in the echo area.  If the region is
active, then @hid[Evaluate Defun] evaluates the current region, just like 
@hid[Evaluate Region].
@enddefcom

@defcom[com "Macroexpand Expression", bind (C-M)]
This command shows the macroexpansion of the next expression in the null
environment in a pop-up window.  With an argument, it uses @f[macroexpand]
instead of @f[macroexpand-1].
@enddefcom

@defcom[com "Re-evaluate Defvar"]
This command is similar to @hid[Evaluate Defun].  It is used for force the
re-evaluation of a @f[defvar] init form.  If the current top-level form is a
@f[defvar], then it does a @f[makunbound] on the variable, and evaluates the
form.
@enddefcom

@defcom[com "Compile Defun", bind (C-x C-c)]
@defcom1[com "Compile Region"]
These commands compile the text in the current defun and the region,
respectively.  If the region is active, then @hid[Compile Defun] compiles the
current region, just like @hid[Compile Region].
@enddefcom

@defcom[com "Load File"]
@defhvar1[var "Load Pathname Defaults", val {nil}]
This command prompts for a file and loads it into the current eval server using
@f[load].  @hid[Load Pathname Defaults] contains the default pathname for this
command.  This variable is set to the file loaded; if it is @nil, then there is
no default.  This command also uses the @hid[Remote Compile File] variable.
@enddefcom


@section[Compiling Files]
These commands are used to compile source ("@f[.lisp]") files, producing binary
("@f[.fasl]") output files.  Note that unlike the other compiling and evalating
commands, this does not have the effect of placing the definitions in the
environment; to do so, the binary file must be loaded.

@defcom[com "Compile Buffer File", bind (C-x c)]
@defhvar1[var "Compile Buffer File Confirm", val {t}]
This command asks for confirmation, then saves the current buffer (when
modified) and compiles the associated file.  The confirmation prompt indicates
intent to save and compile or just compile.  If the buffer wasn't modified, and
a comparison of the write dates for the source and corresponding binary
("@f[.fasl]") file suggests that recompilation is unnecessary, the confirmation
also indicates this.  A prefix argument overrides this test and forces
recompilation.  Since there is a complete log of output in the background
buffer, the creation of the normal error output ("@f[.err]") file is inhibited.

Setting @hid[Compile Buffer File Confirm] to @nil inhibits confirmation, except
when the binary is up to date and a prefix argument is not supplied.
@enddefcom

@defcom[com "Compile File"]
This command prompts for a file and compiles that file, providing a convenient
way to compile a file that isn't in any buffer.  Unlike 
@hid[Compile Buffer File], this command doesn't do any consistency checks such
as checking whether the source is in a modified buffer or the binary is up to
date.
@enddefcom

@defcom[com "Compile Group"]
@defcom1[com "List Compile Group"]
@label[compile-group-command]@index[group, compilation]@hid[Compile Group] does
a @hid[Save All Files] and then compiles every "@f[.lisp]" file for which the
corresponding "@f[.fasl]" file is older or nonexistent.  The files are compiled
in the order in which they appear in the group definition.  A prefix argument
forces compilation of all "@f[.lisp]" files.

@hid[List Compile Group] lists any files that would be compiled by
@hid[Compile Group].  All Modified files are saved before checking to generate
a consistent list.
@enddefcom 

@defcom[com "Set Compile Server"]
@defcom1[com "Set Buffer Compile Server"]
@defcom1[com "Current Compile Server"]
These commands are analogous to @hid[Set Eval Server], @comref[Set Buffer Eval
Server] and @hid[Current Eval Server], but they determine the eval server used
for file compilation requests.  If the user specifies a compile server, then
the file compilation commands send compilation requests to that server instead
of the current eval server.

Having a separate compile server makes it easy to do compilations in the
background while continuing to interact with your eval server and editor.  The
compile server can also run on a remote machine relieving your active
development machine of the compilation effort.
@enddefcom

@defcom[com "Next Compiler Error", bind (H-n)]
@defcom1[com "Previous Compiler Error", bind (H-p)]
These commands provides a convenient way to inspect compiler errors.  First it
splits the current window if there is only one window present.  @hemlock
positions the current point in the first window at the erroneous source code
for the next (or previous) error.  Then in the second window, it displays the
error beginning at the top of the window.  Given an argument, this command
skips that many errors.
@enddefcom

@defcom[com "Flush Compiler Error Information"]
This command relieves the current eval server of all infomation about errors
encountered while compiling.  This is convenient if you have been compiling a
lot, but you were ignoring errors and warnings.  You don't want to step through
all the old errors, so you can use this command immediately before compiling a
file whose errors you intend to edit.
@enddefcom


@defhvar[var "Remote Compile File", val {nil}]
When true, this variable causes file compilations to be done using the RFS
remote file system mechanism by prepending "@f[/../]@i[host]" to the file being
compiled.  This allows the compile server to be run on a different machine, but
requires that the source be world readable.  If false, commands use source
filenames directly.  Do NOT use this to compile files in AFS.
@enddefhvar


@section[Querying the Environment]
@index[documentation, lisp]
These commands are useful for obtaining various random information from the
Lisp environment.

@defcom[com "Describe Function Call", bind (C-M-A)]
@defcom1[com "Describe Symbol", bind (C-M-S)]
@hid[Describe Function Call] uses the current eval server to describe the
symbol found at the head of the currently enclosing list, displaying the output
in a pop-up window.  @hid[Describe Symbol] is the same except that it describes
the symbol at or before the point.  These commands are primarily useful for
finding the documentation for functions and variables.  If there is no
currently valid eval server, then this command uses the editor Lisp's
environment instead of trying to spawn a slave.
@enddefcom


@section[Editing Definitions]
The Lisp compiler annotates each compiled function object with the source
file that the function was originally defined from.  The definition editing
commands use this information to locate and edit the source for functions
defined in the environment.

@defcom[com "Edit Definition"]
@defcom1[com "Goto Definition", bind (C-M-F)]
@defcom1[com "Edit Command Definition"]
@hid[Edit Definition] prompts for the name of a function, and then uses the
current eval server to find out in which file the function is defined.  If
something other than @f[defun] or @f[defmacro] defined the function, then this
simply reads in the file, without trying to find its definition point within
the file.  If the function is uncompiled, then this looks for it in the current
buffer.  If there is no currently valid eval server, then this command uses the
editor Lisp's environment instead of trying to spawn a slave.

@hid[Goto Definition] edits the definition of the symbol at the beginning of
the current list.

@hid[Edit Command Definition] edits the definition of a @hemlock command.  By
default, this command does a keyword prompt for the command name (as in an
extended command).  If a prefix argument is specified, then instead prompt for
a key and edit the definition of the command bound to that key.
@enddefcom

@defcom[com "Add Definition Directory Translation"]
@defcom1[com "Delete Definition Directory Translation"]
The defining file is recorded as an absolute pathname.  The definition editing
commands have a directory translation mechanism that allow the sources to be
found when they are not in the location where compilation was originally done.
@hid[Add Definition Directory Translation] prompts for two directory
namestrings and causes the first to be mapped to the second.  Longer (more
specific) directory specifications are matched before shorter (more general)
ones.

@hid[Delete Definition Directory Translation] prompts for a directory
namestring and deletes it from the directory translation table.
@enddefcom

@defhvar[var "Editor Definition Info", val {nil}]
When this variable is true, the editor Lisp is used to determine definition
editing information, otherwise the current eval server is used.  This variable
is true in @hid[Eval] and @hid[Editor] modes.
@enddefhvar


@section[Debugging]
These commands manipulate the slave when it is in the debugger and provide
source editing based on the debugger's current frame.  These all affect the
@hid[Current Eval Server].


@subsection[Changing Frames]

@defcom[com "Debug Down", bind (C-M-H-d)]
This command moves down one debugger frame.
@enddefcom

@defcom[com "Debug Up", bind (C-M-H-u)]
This command moves up one debugger frame.
@enddefcom

@defcom[com "Debug Top", bind (C-M-H-t)]
This command moves to the top of the debugging stack.
@enddefcom

@defcom[com "Debug Bottom", bind (C-M-H-b)]
This command moves to the bottom of the debugging stack.
@enddefcom

@defcom[com "Debug Frame", bind (C-M-H-f)]
This command moves to the absolute debugger frame number indicated by the
prefix argument.
@enddefcom


@subsection[Getting out of the Debugger]

@defcom[com "Debug Quit", bind (C-M-H-q)]
This command throws to top level out of the debugger in the @hid[Current Eval
Server].
@enddefcom

@defcom[com "Debug Go", bind (C-M-H-g)]
This command tries the @f[continue] restart in the @hid[Current Eval Server].
@enddefcom

@defcom[com "Debug Abort", bind (C-M-H-a)]
This command executes the ABORT restart in the @hid[Current Eval Server].
@enddefcom

@defcom[com "Debug Restart", bind (C-M-H-r)]
This command executes the restart indicated by the prefix argument in the
@hid[Current Eval Server].  The debugger enumerates the restart cases upon
entering it.
@enddefcom


@subsection[Getting Information]

@defcom[com "Debug Help", bind (C-M-H-h)]
This command in prints the debugger's help text.
@enddefcom

@defcom[com "Debug Error", bind (C-M-H-e)]
This command prints the error condition and restart cases displayed upon
entering the debugger.
@enddefcom

@defcom[com "Debug Backtrace", bind (C-M-H-B)]
This command executes the debugger's @f[backtrace] command.
@enddefcom

@defcom[com "Debug Print", bind (C-M-H-p)]
This command prints the debugger's current frame in the same fashion as the
frame motion commands.
@enddefcom

@defcom[com "Debug Verbose Print", bind (C-M-H-P)]
This command prints the debugger's current frame without elipsis.
@enddefcom

@defcom[com "Debug Source", bind (C-M-H-s)]
This command prints the source form for the debugger's current frame.
@enddefcom

@defcom[com "Debug Verbose Source"]
This command prints the source form for the debugger's current frame with
surrounding forms for context.
@enddefcom

@defcom[com "Debug List Locals", bind (C-M-H-l)]
This prints the local variables for the debugger's current frame.
@enddefcom


@subsection[Editing Sources]

@defcom[com "Debug Edit Source", bind (C-M-H-S)]
This command attempts to place you at the source location of the debugger's
current frame.  Not all debugger frames represent function's that were compiled
with the appropriate debug-info policy.  This beeps with a message if it is
unsuccessful.
@enddefcom


@subsection[Miscellaneous]

@defcom[com "Debug Flush Errors", bind (C-M-H-F)]
This command toggles whether the debugger ignores errors or recursively enters
itself.
@enddefcom




@section[Manipulating the Editor Process]
When developing @hemlock customizations, it is useful to be able to manipulate
the editor Lisp environment from @hemlock.

@defcom[com "Editor Describe", bind (Home t, C-_ t)]
This command prompts for an expression, and then evaluates and describes it
in the editor process.
@enddefcom

@defcom[com "Room"]
Call the @f[room] function in the editor process, displaying information
about allocated storage in a pop-up window.
@enddefcom

@defcom[com "Editor Load File"]
This command is analogous to @comref[Load File], but loads the file into the
editor process.
@enddefcom


@subsection[Editor Mode]
When @hid[Editor] mode is on, alternate versions of the Lisp interaction
commands are bound in place of the eval server based commands.  These commands
manipulate the editor process instead of the current eval server.  Turning on
editor mode in a buffer allows incremental development of code within the
running editor.

@defcom[com "Editor Mode"]
This command turns on @hid[Editor] minor mode in the current buffer.  If it is
already on, it is turned off.  @hid[Editor] mode may also be turned on using
the @f[Mode] file option (see page @pageref[file-options].)
@enddefcom

@defcom[com "Editor Compile Defun",
	stuff (bound to @bf[C-x C-c] in @hid[Editor] mode)]
@defcom1[com "Editor Compile Region"]
@defcom1[com "Editor Evaluate Buffer"]
@defcom1[com "Editor Evaluate Defun",
	stuff (bound to @bf[C-x C-e] in @hid[Editor] mode)]
@defcom1[com "Editor Evaluate Region"]
@defcom1[com "Editor Macroexpand Expression", bind (Editor: C-M)]
@defcom1[com "Editor Re-evaluate Defvar"]
@defcom1[com "Editor Describe Function Call",
	stuff (bound to @bf[C-M-A] in @hid[Editor] mode)]
@defcom1[com "Editor Describe Symbol",
	stuff (bound to @bf[C-M-S] in @hid[Editor] mode)]
These commands are similar to the standard commands, but modify or examine the
Lisp process that @hemlock is running in.  Terminal I/O is done on the
initial window for the editor's Lisp process.  Output is directed to a pop-up
window or the editor's window instead of to the background buffer.
@enddefcom

@defcom[com "Editor Compile Buffer File"]
@defcom1[com "Editor Compile File"]
@defcom1[com "Editor Compile Group"]
In addition to compiling in the editor process, these commands differ from the
eval server versions in that they direct output to the the 
@hid[Compiler Warnings] buffer.
@enddefcom

@defcom[com "Editor Evaluate Expression",
     stuff (bound to @bf[M-Escape] in @hid[Editor] mode and @bf[C-M-Escape])] 
This command prompts for an expression and evaluates it in the editor process.
The results of the evaluation are displayed in the echo area.
@enddefcom


@subsection[Eval Mode]
@label[eval-mode]
@index[modes, eval]@hid[Eval] mode is a minor mode that simulates a @f[read]
@f[eval] @f[print] loop running within the editor process.  Since Lisp
program development is usually done in a separate eval server process (see page
@pageref[eval-servers]), @hid[Eval] mode is used primarily for debugging code
that must run in the editor process.  @hid[Eval] mode shares some commands with
@hid[Typescript] mode: see section @ref[typescripts].

@hid[Eval] mode doesn't completely support terminal I/O: it binds
@var[standard-output] to a stream that inserts into the buffer and
@var[standard-input] to a stream that signals an error for all operations.
@hemlock cannot correctly support the interactive evaluation of forms that read
from the @hid[Eval] interactive buffer.

@defcom[com "Select Eval Buffer"]
This command changes to the @hid[Eval] buffer, creating one if it doesn't
already exist.  The @hid[Eval] buffer is created with @hid[Lisp] as the major
mode and @hid[Eval] and @hid[Editor] as minor modes. 
@enddefcom

@defcom[com "Confirm Eval Input",
        stuff (bound to @bf[Return] in @hid[Eval] mode)]
This command evaluates all the forms between the end of the last output and
the end of the buffer, inserting the results of their evaluation in the buffer.
This beeps if the form is incomplete.  Use @binding[Linefeed] to insert line
breaks in the middle of a form.

This command uses @hid[Unwedge Interactive Input Confirm] in the same way
@hid[Confirm Interactive Input] does.
@enddefcom

@defcom[com "Abort Eval Input", 
        stuff (bound to @bf[M-i] in @hid[Eval] mode)]
This command moves the the end of the buffer and prompts, ignoring any
input already typed in.
@enddefcom


@subsection[Error Handling]
@index[error handling]
When an error happens inside of @hemlock, @hemlock will trap the error and
display the error message in the echo area, possibly along with the
"@f[Internal error:]" prefix.  If you want to debug the error, type @bf[?].
This causes the prompt "@f[Debug:]" to appear in the echo area.  The following
commands are recognized:
@begin[description]
@bf[d]@\Enter a break-loop so that you can use the Lisp debugger.
Proceeding with "@f[go]" will reenter @hemlock and give the "@f[Debug:]"
prompt again.

@bf[e]@\Display the original error message in a pop-up window.

@bf[b]@\Show a stack backtrace in a pop-up window.

@bf[q, Escape]@\Quit from this error to the nearest command loop.

@bf[r]@\Display a list of the restart cases and prompt for the number of a
@f[restart-case] with which to continue.  Restarting may result in prompting in
the window in which Lisp started.
@end[description]

Only errors within the editor process are handled in this way.  Errors during
eval server operations are handled using normal terminal I/O on a typescript in
the eval server's slave buffer or background buffer (see page
@pageref[operations]).  Errors due to interaction in a slave buffer will cause
the debugger to be entered in the slave buffer.


@section[Command Line Switches]
@label[slave-switch]
Two command line switches control the initialization of editor and eval servers
for a Lisp process:
@begin[description]
@f<-edit>@\
@label[edit-switch]
This switch starts up @hemlock.  If there is a non-switch command line word
immediately following the program name, then the system interprets it as a file
to edit.  For example, given
@Begin[ProgramExample]
lisp file.txt -edit
@End[ProgramExample]
Lisp will go immediately into @hemlock finding the file @f[file.txt].

@f<-slave [>@i[name]@f<]>@\
 This switch causes the Lisp process to become a slave of the editor process
@i[name].  An editor Lisp determines @i[name] when it allows connections from
slaves.  Once the editor chooses a name, it keeps the same name until the
editor's Lisp process terminates.  Since the editor can automatically create
slaves on its own machine, this switch is useful primarily for creating slaves
that run on a different machine.  @f[hqb]'s machine is @f[ME.CS.CMU.EDU], and
he wants want to run a slave on @f[SLAVE.CS.CMU.EDU], then he should use the
@hid[Accept Slave Connections] command, telnet to the machine, and invoke Lisp
supplying @f[-slave] and the editor's name.  The command displays the editor's
name.
@end[description]
