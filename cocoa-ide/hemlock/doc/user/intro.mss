@comment{-*- Dictionary: target:scribe/hem/hem; Mode: spell; Package: Hemlock -*-}
@chap[Introduction]

@hemlock is a text editor which follows in the tradition of @emacs
and the Lisp Machine editor ZWEI.  In its basic form, @hemlock has almost
the same command set as ITS/TOPS-20 @emacs@foot[In this document, "Emacs"
refers to this, the original version, rather than to any of the large
numbers of text editors inspired by it which may go by the same name.],
and similar features such as multiple windows and extended commands, as
well as built in documentation features.  The reader should bear in mind
that whenever some powerful feature of @hemlock is described, it has
probably been directly inspired by @emacs.

This manual describes @hemlock@comment{}'s commands and other user visible
features and then goes on to tell how to make simple customizations.  For
complete documentation of the @hemlock primitives with which commands are
written, the @i[Hemlock Command Implementor's Manual] is also available.



@section[The Point and The Cursor]

@index[point]
@index[cursor]
The @i[point] is the current focus of editing activity.  Text typed in by the
user is inserted at the point.  Nearly all commands use the point as a
indication of what text to examine or modify.  Textual positions in @hemlock
are between characters.  This may seem a bit curious at first, but it is
necessary since text must be inserted between characters.  Although the point
points between characters, it is sometimes said to point @i[at] a character, in
which case the character after the point is referred to.

The @i[cursor] is the visible indication of the current focus of attention: a
rectangular blotch under @windows, or the hardware cursor on a terminal.  The
cursor is usually displayed on the character which is immediately after the
point, but it may be displayed in other places.  Wherever the cursor is
displayed it indicates the current focus of attention.  When input is being
prompted for in the echo area, the cursor is displayed where the input is to
go.  Under @windows the cursor is only displayed when @hemlock is waiting
for input.


@section[Notation]

There are a number of notational conventions used in this manual which need
some explanation.


@subsection[Key-events]

@label[key-events]
@index[key-events, notation]
@index[bits, key-event]
@index[modifiers, key-event]
The canonical representation of editor input is a @i[key-event].  When you type
on the keyboard, @hemlock receives key-events.  Key-events have names for their
basic form, and we refer to this name as a @i[keysym].  This manual displays
keysyms in a @bf[Bold] font.  For example, @bf[a] and @bf[b] are the keys that
normally cause the editor to insert the characters @i[a] and @i[b].

Key-events have @i[modifiers] or @i[bits] indicating a special interpretation
of the root key-event.  Although the keyboard places limitations on what
key-events you can actually type, @hemlock understands arbitrary combinations
of the following modifiers: @i[Control], @i[Meta], @i[Super], @i[Hyper],
@i[Shift], and @i[Lock].  This manual represents the bits in a key-event by
prefixing the keysym with combinations of @bf[C-], @bf[M-], @bf[S-], @bf[H-],
@bf[Shift-], and @bf[Lock].  For example, @bf[a] with both the control and meta
bits set appears as @bf[C-M-a].  In general, ignore the shift and lock
modifiers since this manual never talks about keysyms that explicitly have
these bits set; that is, it may talk about the key-event @bf[A], but it would
never mention @bf[Shift-a].  These are actually distinct key-events, but
typical input coercion turns presents @hemlock with the former, not the latter.

Key-event modifiers are totally independent of the keysym.  This may be new to
you if you are used to thinking in terms of ASCII character codes.  For
example, with key-events you can distinctly identify both uppercase and
lowercase keysyms with the control bit set; therefore, @bf[C-a] and @bf[C-A]
may have different meanings to @hemlock.

Some keysyms' names consist of more than a single character, and these usually
correspond to the legend on the keyboard.  For example, some keyboards let you
enter @bf[Home], @bf[Return], @bf[F9], etc.

In addition to a keyboard, you may have a mouse or pointer device.  Key-events
also represent this kind of input.  For example, the down and up transitions of
the @i[left button] correspond to the @bf[Leftdown] and @bf[Leftup] keysyms.

See sections @ref[key-bindings], @ref[using-x], @ref[using-terminals]


@subsection[Commands]

@index[commands]@label[commands]Nearly everything that can be done in
@hemlock is done using a command.  Since there are many things worth
doing, @hemlock provides many commands, currently nearly two hundred.
Most of this manual is a description of what commands exist, how they are
invoked, and what they do.  This is the format of a command's
documentation:

@defcom[com "Sample Command", bind (C-M-q, C-`)]
@begin[quotation, facecode i, leftmargin 8ems, rightmargin 3.5ems,
below 0.8 lines]
This command's name is @hid[Sample Command], and it is bound to
@w(@bf(C-M-q)) and @bf[C-`], meaning that typing either of these will
invoke it.  After this header comes a description of what the command does:
@end[quotation]

This command replaces all occurrences following the point of the string
"@f[Pascal]" with the string "@f[Lisp]".  If a prefix argument is supplied,
then it is interpreted as the maximum number of occurrences to replace.  If
the prefix argument is negative then the replacements are done backwards
from the point.
@comment<
@begin[quotation, facecode i, leftmargin 8ems, rightmargin 3.5ems,
above 0.8 lines, below 0.8 lines]
Toward the end of the description there may be information primarily of
interest to customizers and command implementors.  If you don't understand
this information, don't worry, the writer probably forgot to speak English.
@end[quotation]

@b[Arguments:]
@begin[description]
@i[target]@\The string to replace with "@f[Lisp]".

@i[buffer]@\The buffer to do the replacement in.  If this is @f[:all] then
the replacement is done in all buffers.
@end[description]>
@enddefcom


@subsection[Hemlock Variables]

@index[variables, hemlock]@hemlock variables supply a simple
customization mechanism by permitting commands to be parameterized.  For
details see page @pageref[vars].

@defhvar[var "Sample Variable", val {36}]
@begin[quotation, facecode i, leftmargin 8ems, below 0.8 lines]
The name of this variable is @hid[Sample Variable] and its initial value is
36.
@end[quotation]
this variable sets a lower limit on the number of replacements that be done
by @hid[Sample Command].  If the prefix argument is supplied, and smaller
in absolute value than @hid[Sample Variable], then the user is prompted as
to whether that small a number of occurrences should be replaced, so as to
avoid a possibly disastrous error.
@enddefhvar


@section[Invoking Commands]
@index[invocation, command]
In order to get a command to do its thing, it must be invoked.  The user can do
this two ways, by typing the @i[key] to which the command is @i[bound] or by
using an @i[extended command].  Commonly used commands are invoked via their
key bindings since they are faster to type, while less used commands are
invoked as extended commands since they are easier to remember.


@subsection[Key Bindings]
@index[bindings, key]
@index[key bindings]
@label[key-bindings]
A key is a sequence of key-events (see section @ref[key-events]) typed on the
keyboard, usually only one or two in length.  Sections @ref[using-x] and
@ref[using-terminals] contain information on particular input devices.

When a command is bound to a key, typing the key causes @hemlock to invoke the
command.  When the command completes its job, @hemlock returns to reading
another key, and this continually repeats.

Some commands read key-events interpreting them however each command desires.
When commands do this, key bindings have no effect, but you can usually abort
@hemlock whenever it is waiting for input by typing @binding[C-g] (see section
@ref[aborting]).  You can usually find out what options are available by typing
@binding[C-_] or @binding[Home] (see section @ref[help]).

The user can easily rebind keys to different commands, bind new keys to
commands, or establish bindings for commands never bound before (see section
@ref[binding-keys]).

In addition to the key bindings explicitly listed with each command, there are
some implicit bindings created by using key translations@foot[Key translations
are documented in the @i[Hemlock Command Implementor's Manual].].  These
bindings are not displayed by documentation commands such as @hid[Where Is].
By default, there are only a few key translations.  The modifier-prefix
characters @bf[C-^], @bf[Escape], @bf[C-z], or @bf[C-c] may be used when typing
keys to convert the following key-event to a control, meta, control-meta, or
hyper key-event.  For example, @bf[C-x Escape b] invokes the same commands as
@bf[C-x M-b], and @bf[C-z u] is the same as @bf[C-M-u].  This allows user to
type more interesting keys on limited keyboards that lack control, meta, and
hyper keys.
@index[bit-prefix key-events]


@defhvar[var "Key Echo Delay", val {1.0}]
A key binding may be composed of several key-events, especially when you enter
it using modifier-prefix key-events.  @hemlock provides feedback for partially
entered keys by displaying the typed key-events in the echo area.  In order to
avoid excessive output and clearing of the echo area, this display is delayed
by @hid[Key Echo Delay] seconds.  If this variable is set to @nil, then
@hemlock foregoes displaying initial subsequences of keys.
@enddefhvar


@subsection[Extended Commands]

@index[commands, extended]A command is invoked as an extended command by
typing its name to the @hid[Extended Command] command, which is invoked
using its key binding, @binding[M-x].

@defcom[com "Extended Command", bind {M-x}]
This command prompts in the echo area for the name of a command, and then
invokes that command.  The prefix argument is passed through to the command
invoked.  The command name need not be typed out in full, as long as enough
of its name is supplied to uniquely identify it.  Completion is available
using @binding[Escape] and @binding[Space], and a list of possible completions
is given by @binding[Home] or @binding[C-_].
@enddefcom


@section[The Prefix Argument]

@index[prefix argument]The prefix argument is an integer argument which
may be supplied to a command.  It is known as the prefix argument because
it is specified by invoking some prefix argument setting command
immediately before the command to be given the argument.  The following
statements about the interpretation of the prefix argument are true:
@begin[itemize]
When it is meaningful, most commands interpret the prefix argument as a
repeat count, causing the same effect as invoking the command that many
times.

When it is meaningful, most commands that use the prefix argument interpret
a negative prefix argument as meaning the same thing as a positive
argument, but the action is done in the opposite direction.

Most commands treat the absence of a prefix argument as meaning the same
thing as a prefix argument of one.

Many commands ignore the prefix argument entirely.

Some commands do none of the above.
@end[itemize]
The following commands are used to set the prefix argument:

@defcom[com "Argument Digit", stuff (bound to all control or meta digits)]
Typing a number using this command sets the prefix argument to that number,
for example, typing @binding[M-1 M-2] sets the prefix argument to twelve.
@enddefcom

@defcom[com "Negative Argument", bind {M--}]
This command negates the prefix argument, or if there is none, sets it to
negative one.  For example, typing @binding[M-- M-7] sets the prefix
argument to negative seven.
@enddefcom

@defcom[com "Universal Argument", bind {C-u}]
@defhvar1[var "Universal Argument Default", val {4}]
This command sets the prefix argument or multiplies it by four.  If digits
are typed immediately afterward, they are echoed in the echo area, and the
prefix argument is set to the specified number.  If no digits are typed
then the prefix argument is multiplied by four.  @binding[C-u - 7] sets the
prefix argument to negative seven.  @binding[C-u C-u] sets the prefix
argument to sixteen.  @binding[M-4 M-2 C-u] sets the prefix argument to one
hundred and sixty-eight.  @binding[C-u M-0] sets the prefix argument to
forty.

@hid[Universal Argument Default] determines the default value and multiplier
for the @hid[Universal Argument] command.
@enddefcom


@section[Modes]

@label[modes]@index[modes]A mode provides a way to change @hemlock@comment{}'s
behavior by specifying a modification to current key bindings, values of
variables, and other things.  Modes are typically used to adjust @hemlock
to suit a particular editing task, e.g. @hid[Lisp] mode is used for editing
@llisp code.

Modes in @hemlock are not like modes in most text editors; @hemlock is really a
"modeless" editor.  There are two ways that the @hemlock mode concept differs
from the conventional one:
@begin[enumerate]
Modes do not usually alter the environment in a very big way, i.e. replace
the set of commands bound with another totally disjoint one.  When a mode
redefines what a key does, it is usually redefined to have a slightly
different meaning, rather than a totally different one.  For this reason,
typing a given key does pretty much the same thing no matter what modes are
in effect.  This property is the distinguishing characteristic of a
modeless editor.

Once the modes appropriate for editing a given file have been chosen, they
are seldom, if ever, changed.  One of the advantages of modeless editors is
that time is not wasted changing modes.
@end[enumerate]

@index[major mode]A @i[major mode] is used to make some big change in the
editing environment.  Language modes such as @hid[Pascal] mode are major
modes.  A major mode is usually turned on by invoking the command
@i{mode-name}@hid[ Mode] as an extended command.  There is only one major
mode present at a time.  Turning on a major mode turns off the one that is
currently in effect.

@index[minor mode]A @i[minor mode] is used to make a small change in the
environment, such as automatically breaking lines if they get too long.
Unlike major modes, any number of minor modes may be present at once.
Ideally minor modes should do the "right thing" no matter what major and
minor modes are in effect, but this is may not be the case when key
bindings conflict.

Modes can be envisioned as switches, the major mode corresponding to one big
switch which is thrown into the correct position for the type of editing being
done, and each minor mode corresponding to an on-off switch which controls
whether a certain characteristic is present.

@defcom[com "Fundamental Mode"]
This command puts the current buffer into @hid[Fundamental] mode.
@hid[Fundamental] mode is the most basic major mode: it's the next best thing
to no mode at all.
@enddefcom


@section[Display Conventions]
@index[display conventions]
There are two ways that @hemlock displays information on the screen; one is
normal @i[buffer display], in which the text being edited is shown on the
screen, and the other is a @i[pop-up window].


@subsection[Pop-Up Windows]
@index[pop-up windows]
@index[random typeout]
@label[pop-up]
Some commands print out information that is of little permanent value, and
these commands use a @i[pop-up] window to display the information.  It is known
as a @i[pop-up] window because it temporarily appears on the screen overlaying
text already displayed.  Most commands of this nature can generate their output
quickly, but in case there is a lot of output, or the user wants to repeatedly
refer to the same output while editing, @hemlock saves the output in a buffer.
Different commands may use different buffers to save their output, and we refer
to these as @i[random typeout] buffers.

If the amount of output exceeds the size of the pop-up window, @Hemlock
displays the message @w<"@f[--More--]"> after each window full.  The following
are valid responses to this prompt:
@Begin[Description]
@bf[Space], @bf[y]@\
 Display the next window full of text.

@bf[Delete], @bf[Backspace], @bf[n]@\
 Abort any further output.

@bf[Escape], @bf[!]@\
 Remove the window and continue saving any further output in the buffer.

@bf[k]@\
 This is the same as @bf[!] or @bf[escape], but @hemlock makes a normal window
over the pop-up window.  This only works on bitmap devices.
@End[Description]
Any other input causes the system to abort using the key-event to determine
the next command to execute.

When the output is complete, @hemlock displays the string @w<"@f[--Flush--]">
in the pop-up window's modeline, indicating that the user may flush the
temporary display.  Typing any of the key-events described above removes the
pop-up window, but typing @bf[k] still produces a window suitable for normal
editing.  Any other input also flushes the display, but @hemlock uses the
key-event to determine the next command to invoke.

@defcom[com "Select Random Typeout Buffer", bind {H-t}]
This command makes the most recently used random typeout buffer the current
buffer in the current window.
@enddefcom

Random typeout buffers are always in @hid[Fundamental] mode.


@subsection[Buffer Display]
@index[buffer, display]
@index[display, buffer]

If a line of text is too long to fit within the screen width it is @i[wrapped],
with @hemlock displaying consecutive pieces of the text line on as many screen
lines as needed to hold the text.  @hemlock indicates a wrapped line by placing
a line-wrap character in the last column of each screen line.  Currently, the
line-wrap character is an exclamation point (@f[!]).  It is possible for a line
to wrap off the bottom of the screen or on to the top.

@hemlock wraps screen lines when the line is completely full regardless of the
line-wrap character.  Most editors insert the line-wrap character and wrap a
single character when a screen line would be full if the editor had avoided
wrapping the line.  In this situation, @hemlock would leave the screen line
full.  This means there are always at least two characters on the next screen
line if @hemlock wraps a line of display.  When the cursor is at the end of a
line which is the full width of the screen, it is displayed in the last column,
since it cannot be displayed off the edge.

@hemlock displays most characters as themselves, but it treats some
specially:
@begin[itemize]
Tabs are treated as tabs, with eight character tab-stops.

Characters corresponding to ASCII control characters are printed as
@f[^]@i[char]; for example, a formfeed is @f[^L].

Characters with the most-significant bit on are displayed as
@f[<]@i[hex-code]@f[>]; for example, @f[<E2>].
@end[itemize]
Since a character may be displayed using more than one printing character,
there are some positions on the screen which are in the middle of a character.
When the cursor is on a character with a multiple-character representation,
@hemlock always displays the cursor on the first character.


@subsection[Recentering Windows]
@index[recentering windows]
@index[windows, recentering]

When redisplaying the current window, @hemlock makes sure the current point is
visible.  This is the behavior you see when you are entering text near the
bottom of the window, and suddenly redisplay shifts your position to the
window's center.

Some buffers receive input from streams and other processes, and you might have
windows displaying these.  However, if those windows are not the current
window, the output will run off the bottom of the windows, and you won't be
able to see the output as it appears in the buffers.  You can change to a
window in which you want to track output and invoke the following command to
remedy this situation.

@defcom[com "Track Buffer Point"]
This command makes the current window track the buffer's point.  This means
that each time Hemlock redisplays, it will make sure the buffer's point is
visible in the window.  This is useful for windows that are not current and
that display buffer's that receive output from streams coming from other
processes.
@enddefcom


@subsection[Modelines]
@label[modelines]
@index[modeline]
A modeline is the line displayed at the bottom of each window where @hemlock
shows information about the buffer displayed in that window.  Here is a typical
modeline:
@begin[programexample]
Hemlock USER: (Fundamental Fill)  /usr/slisp/hemlock/user.mss
@end[programexample]
This tells us that the file associated with this buffer is
"@f[/usr/slisp/hemlock/user.mss]", and the @hid[Current Package] for Lisp
interaction commands is the @f["USER"] package.  The modes currently present
are @hid[Fundamental] and @hid[Fill]; the major mode is always displayed first,
followed by any minor modes.  If the buffer has no associated file, then the
buffer name will be present instead:
@begin[programexample]
Hemlock PLAY: (Lisp)  Silly:
@end[programexample]
In this case, the buffer is named @hid[Silly] and is in @hid[Lisp] mode.  The
user has set @hid[Current Package] for this buffer to @f["PLAY"].

@defhvar[var "Maximum Modeline Pathname Length", val {nil}]
This variable controls how much of a pathname @hemlock displays in a modeline.
Some distributed file systems can have very long pathnames which leads to the
more particular information in a pathname running off the end of a modeline.
When set, the system chops off leading directories until the name is less than
the integer value of this variable.  Three dots, @f[...], indicate a truncated
name.  The user can establish this variable buffer locally with the
@hid[Defhvar] command.
@enddefhvar

If the user has modified the buffer since the last time it was read from or
save to a file, then the modeline contains an asterisk (@f[*]) between the
modes list and the file or buffer name:
@begin[programexample]
Hemlock USER: (Fundamental Fill)  * /usr/slisp/hemlock/user.mss
@end[programexample]
This serves as a reminder that the buffer should be saved eventually.

@index[status line]
There is a special modeline known as the @i[status line] which appears as the
@hid[Echo Area]'s modeline.  @Hemlock and user code use this area to display
general information not particular to a buffer @dash recursive edits, whether
you just received mail, etc.


@section[Use with X Windows]
@label[using-x]
@index[X windows, use with]
You should use @hemlock on a workstation with a bitmap display and a windowing
system since @hemlock makes good use of a non-ASCII device, mouse, and the
extra modifier keys typically associated with workstations.  This section
discusses using @hemlock under X windows, the only supported windowing system.


@subsection[Window Groups]
@index[window management]
@label[groups]
@hemlock manages windows under X in groups.  This allows @hemlock to be more
sophisticated in its window management without being rude in the X paradigm of
screen usage.  With window groups, @hemlock can ignore where the groups are,
but within a group, it can maintain the window creation and deletion behavior
users expect in editors without any interference from window managers.

Initially there are two groups, a main window and the @hid[Echo Area].  If you
keep a pop-up display, see section @ref[pop-up], @hemlock puts the window it
creates in its own group.  There are commands for creating new groups.

@hemlock only links windows within a group for purposes of the @hid[Next
Window], @hid[Previous Window], and @hid[Delete Next Window] commands.  To move
between groups, you must use the @hid[Point to Here] command bound to the
mouse.  

Window manager commands can reshape and move groups on the screen.


@subsection[Event Translation]
@index[keyboard use under X]
@index[translation of keys under X]
Each X key event is translated into a canonical input representation, a
key-event.  The X key event consists of a scan-code and modifier bits, and
these translate to an X keysym.  This keysym and the modifier bits map to a
key-event.

If you type a key with a shift key held down, this typically maps to a distinct
X keysym.  For example, the shift of @bf[3] is @bf[#], and these have different
X keysyms.  Some keys map to the same X keysym regardless of the shift bit,
such as @bf[Tab], @bf[Space], @bf[Return], etc.  When the X lock bit is on, the
system treats this as a caps-lock, only mapping keysyms for lowercase letters
to shifted keysyms.

The key-event has a keysym and a field of bits.  The X keysyms map directly to
the key-event keysyms.  There is a distinct mapping for each CLX modifier bit
to a key-event bit.  This tends to eliminate shift and lock modifiers, so
key-events usually only have control, meta, hyper, and super bits on.  Hyper
and super usually get turned on with prefix key-events that set them on the
following key-event, but you can turn certain keys on the keyboard into hyper
and super keys.  See the X manuals and the @i[Hemlock Command Implementor's
Manual] for details.

The system also maps mouse input to key-events.  Each mouse button has distinct
key-event keysyms for whether the user pressed or released it.  For
convenience, @hemlock makes use of an odd property of converting mouse events
to key-events.  If you enter a mouse event with the shift key held down,
@hemlock sees the key-event keysym for the mouse event, but the key-event has
the super bit turned on.  For example, if you press the left button with the
shift key pressed, @hemlock sees @bf[S-Leftdown].

Note that with the two button mouse on the IBM RT PC, the only way to to send
@bf[Middledown] is to press both the left and right buttons simultaneously.
This is awkward, and it often confuses the X server.  For this reason, the
commands bound to the middle button are also bound to the shifted left button,
@bf[S-Leftdown], which is much easier to type.


@subsection[Cut Buffer Commands]
@index[cutting]@index[pasting] These commands allow the X cut buffer to be
used from @hemlock .  Although @hemlock can cut arbitrarily large regions,
a bug in the standard version 10 xterm prevents large regions from being
pasted into an xterm window.

@defcom[com "Region to Cut Buffer", bind {M-Insert}]
@defcom1[com "Insert Cut Buffer", bind {Insert}]
These commands manipulate the X cut buffer.  @hid[Region to Cut Buffer] puts
the text in the region into the cut buffer.  @hid[Insert Cut Buffer] inserts
the contents of the cut buffer at the point.
@enddefcom

@subsection[Redisplay and Screen Management]

These variables control a number of the characteristics of @hemlock bitmap
screen management.

@defhvar[var "Bell Style", val {:border-flash}]
@defhvar1[var "Beep Border Width", val {20}]
@hid[Bell Style] determines what beeps do in @hemlock.  Acceptable values are
@kwd[border-flash], @kwd[feep], @kwd[border-flash-and-feep], @kwd[flash],
@kwd[flash-and-feep], and @nil (do nothing).

@hid[Beep Border Width] is the width in pixels of the border flashed by border
flash beep styles.
@enddefhvar

@defhvar[var "Reverse Video", val {nil}]
If this variable is true, then @hemlock paints white on black in window
bodies, black on white in modelines.
@enddefhvar

@defhvar[var "Thumb Bar Meter", val {t}]
If this variable is true, then windows will be created to be displayed with a
ruler in the bottom border of the window.
@enddefhvar

@defhvar[var "Set Window Autoraise", val {:echo-only}]
When true, changing the current window will automatically raise the new current
window.  If the value is @kwd[echo-only], then only the echo area window will
be raised automatically upon becoming current.
@enddefhvar

@defhvar[var "Default Initial Window Width", val {80}]
@defhvar1[var "Default Initial Window Height", val {24}]
@defhvar1[var "Default Initial Window X"]
@defhvar1[var "Default Initial Window Y"]
@defhvar1[var "Default Window Height", val {24}]
@defhvar1[var "Default Window Width", val {80}]
@index[window placement]
@Hemlock uses the variables with "@hid[Initial]" in their names when it first
starts up to make its first window.  The width and height are specified in
character units, but the x and y are specified in pixels.  The other variables
determine the width and height for interactive window creation, such as making
a window with @comref[New Window].
@enddefhvar

@defhvar[var "Cursor Bitmap File", val {"library:hemlock.cursor"}]
This variable determines where the mouse cursor bitmap is read from when
@hemlock starts up.  The mask is found by merging this name with "@f[.mask]".
This has to be a full pathname for the C routine.
@enddefhvar


@defhvar[var "Default Font"]
This variable holds the string name of the font to be used for normal text
display: buffer text, modelines, random typeout, etc.  The font is loaded at
initialization time, so this variable must be set before entering @hemlock.
When @nil, the display type is used to choose a font.
@enddefhvar


@section[Use With Terminals]
@label[using-terminals]@index[terminals, use with] @hemlock can also be used
with ASCII terminals and terminal emulators.  Capabilities that depend on
@windows (such as mouse commands) are not available, but nearly everything else
can be done.

@subsection[Terminal Initialization]

@index[terminal speed]
@index[speed, terminal]
@index[slow terminals]
@index[incremental redisplay]
For best redisplay performance, it is very important to set the terminal speed:
@lisp
stty 2400
@endlisp
Often when running @hemlock using TTY redisplay, Hemlock will actually be
talking to a PTY whose speed is initialized to infinity.  In reality, the
terminal will be much slower, resulting in @hemlock@comment{}'s output getting way ahead
of the terminal.  This prevents @hemlock from briefly stopping redisplay to
allow the terminal to catch up.  See also @hvarref<Scroll Redraw Ratio>.

The terminal control sequences are obtained from the termcap database using the
normal Unix conventions.  The @f["TERM"] environment variable holds the
terminal type.  The @f["TERMCAP"] environment variable can be used to override
the default termcap database (in @f["/etc/termcap"]).  The size of the terminal
can be altered from the termcap default through the use of:
@lisp
stty rows @i{height} columns @i{width}
@endlisp

@subsection[Terminal Input]
@index[ASCII keyboard translation]
@index[bit-prefix key-events]
@index[prefix key-events]
@index[key-event, prefix]
The most important limitation of a terminal is its input capabilities.  On a
workstation with function keys and independent control, meta, and shift
modifiers, it is possible to type 800 or so distinct single keystrokes.
Although by default, @hemlock uses only a fraction of these combinations, there
are many more than the 128 key-events available in ASCII.

On a terminal, @hemlock attempts to translate ASCII control characters into the
most useful key-event:
@begin[itemize]
On a terminal, control does not compose with shift.  If the control key is down
when you type a letter keys, the terminal always sends one code regardless of
whether the shift key is held.  Since @hemlock primarily binds commands to
key-events with keysyms representing lowercase letters regardless of what bits
are set in the key-event, the system translates the ASCII control codes to a
keysym representing the appropriate lowercase characters.  This keysym then
forms a key-event with the control bit set.  Users can type @bf[C-c] followed
by an uppercase character to form a key-event with a keysym representing an
uppercase character and bits with the control bit set.

On a terminal, some of the named keys generate an ASCII control code.  For
example, @f[Return] usually sends a @f[C-m].  The system translates these ASCII
codes to a key-event with an appropriate keysym instead of the keysym named by
the character which names the ASCII code.  In the above example, typing the
@f[Return] key would generate a key-event with the @bf[Return] keysym and no
bits.  It would NOT translate to a key-event with the @bf[m] keysym and the
control bit.
@end[itemize]

Since terminals have no meta key, you must use the @bf[Escape] and @bf[C-Z]
modifier-prefix key-events to invoke commands bound to key-events with the meta
bit or meta and control bits set.  ASCII terminals cannot generate all
key-events which have the control bit on, so you can use the @bf[C-^]
modifier-prefix.  The @bf[C-c] prefix sets the hyper bit on the next key-event
typed.

When running @hemlock from a terminal @f[^\] is the interrupt key-event.
Typing this will place you in the Lisp debugger.

When using a terminal, pop-up output windows cannot be retained after the
completion of the command.


@subsection[Terminal Redisplay]

Redisplay is substantially different on a terminal.  @Hemlock uses different
algorithms, and different parameters control redisplay and screen management.

Terminal redisplay uses the Unix termcap database to find out how to use a
terminal.  @hemlock is useful with terminals that lack capabilities for
inserting and deleting lines and characters, and some terminal emulators
implement these operations very inefficiently (such as xterm).
If you realize poor performance when scrolling, create a termcap entry that
excludes these capabilities.

@defhvar[var "Scroll Redraw Ratio", val {nil}]
This is a ratio of "inserted" lines to the size of a window.  When this ratio
is exceeded, insert/delete line terminal optimization is aborted, and every
altered line is simply redrawn as efficiently as possible.  For example,
setting this to 1/4 will cause scrolling commands to redraw the entire window
instead of moving the bottom two lines of the window to the top (typically 3/4
of the window is being deleted upward and inserted downward, hence a redraw);
however, commands like @hid[New Line] and @hid[Open Line] will still work
efficiently, inserting a line and moving the rest of the window's text
downward.
@enddefhvar


@section[The Echo Area]

@index[echo area]
@index[prompting]
The echo area is the region which occupies the bottom few lines on the screen.
It is used for two purposes: displaying brief messages to the user and
prompting.

When a command needs some information from the user, it requests it by
displaying a @i[prompt] in the echo area.  The following is a typical prompt:
@begin[programexample]
Select Buffer: [hemlock-init.lisp /usr/foo/]
@end[programexample]
The general format of a prompt is a one or two word description of the input
requested, possibly followed by a @i[default] in brackets.  The default is a
standard response to the prompt that @hemlock uses if you type @bf[Return]
without giving any other input.

There are four general kinds of prompts: @comment<Key prompts?>
@begin[description]
@i[key-event]@\
 The response is a single key-event and no confirming @binding[Return] is
needed.

@i[keyword]@\
 The response is a selection from one of a limited number of choices.
Completion is available using @binding[Space] and @binding[Escape], and you
only need to supply enough of the keyword to distinguish it from any other
choice.  In some cases a keyword prompt accepts unknown input, indicating the
prompter should create a new entry.  If this is the case, then you must enter
the keyword fully specified or completed using @binding[Escape]; this
distinguishes entering an old keyword from making a new keyword which is a
prefix of an old one since the system completes partial input automatically.

@i[file]@\
 The response is the name of a file, which may have to exist.  Unlike other
prompts, the default has some effect even after the user supplies some input:
the system @i[merges] the default with the input filename.  See page
@pageref(merging) for a description of filename merging.  @bf[Escape] and
@bf[Space] complete the input for a file parse.

@i[string]@\
 The response is a string which must satisfy some property, such as being the
name of an existing file.
@end[description]

@index[history, echo area]
These key-events have special meanings when prompting:
@begin[description]
@binding[Return]@\
 Confirm the current parse.  If no input has been entered, then use the
default.  If for some reason the input is unacceptable, @hemlock does two
things:
@Begin[enumerate]
beeps, if the variable @hid[Beep on Ambiguity] set, and

moves the point to the end of the first word requiring disambiguation.
@End[enumerate]
This allows you to add to the input before confirming the it again.

@binding[Home, C-_]@\
 Print some sort of help message.  If the parse is a keyword parse, then print
all the possible completions of the current input in a pop-up window.

@binding[Escape]@\
 Attempt to complete the input to a keyword or file parse as far as possible,
beeping if the result is ambiguous.  When the result is ambiguous, @hemlock
moves the point to the first ambiguous field, which may be the end of the
completed input.

@binding[Space]@\
 In a keyword parse, attempt to complete the input up to the next space.  This
is useful for completing the names of @hemlock commands and similar things
without beeping a lot, and you can continue entering fields while leaving
previous fields ambiguous.  For example, you can invoke @hid[Forward Word] as
an extended command by typing @binding[M-X f Space w Return].  Each time the
user enters space, @Hemlock attempts to complete the current field and all
previous fields.

@binding[C-i, Tab]@\
 In a string or keyword parse, insert the default so that it may be edited.

@binding[C-p]@\
 Retrieve the text of the last string input from a history of echo area inputs.
Repeating this moves to successively earlier inputs.

@binding[C-n]@\
 Go the other way in the echo area history.

@binding[C-q]@\
 Quote the next key-event so that it is not interpreted as a command.
@end[description]

@defhvar[var "Ignore File Types"]
This variable is a list of file types (or extensions), represented as a string
without the dot, e.g. @f["fasl"].  Files having any of the specified types will
be considered nonexistent for completion purposes, making an unambiguous
completion more likely.  The initial value contains most common binary and
output file types.
@enddefhvar


@section[Online Help]

@label[help]
@index[online help]
@index[documentation, hemlock]
@hemlock has a fairly good online documentation facility.  You can get brief
documentation for every command, variable, character attribute, and key
by typing a key.

@defcom[com "Help", bind (Home, C-_)]
This command prompt for a key-event indicating one of a number of other
documentation commands.  The following are valid responses:
@begin[description]
@bf[a]@\
 List commands and other things whose names contain a specified keyword.

@bf[d]@\
 Give the documentation and bindings for a specified command.

@bf[g]@\
 Give the documentation for any @hemlock thing.

@bf[v]@\
 Give the documentation for a @hemlock variable and its values.

@bf[c]@\
 Give the documentation for a command bound to some key.

@bf[l]@\
 List the last sixty key-events typed.

@bf[m]@\
 Give the documentation for a mode followed by a short description of its
mode-specific bindings.

@bf[p]@\
 Give the documentation and bindings for commands that have at least one
binding involving a mouse/pointer key-event.

@bf[w]@\
 List all the key bindings for a specified command.

@bf[t]@\
 Describe a @llisp object.

@binding[q]@\
 Quit without doing anything.

@binding[Home, C-_, ?, h]@\
 List all of the options and what they do.
@end[description]
@enddefcom

@defcom[com "Apropos", bind (Home a, C-_ a)]
This command prints brief documentation for all commands, variables, and
character attributes whose names match the input.  This performs a prefix match
on each supplied word separately, intersecting the names in each word's result.
For example, giving @hid[Apropos] "@f[f m]" causes it to tersely describe
following commands and variables:
@Begin[Itemize]   
@hid[Auto Fill Mode]

@hid[Fundamental Mode]

@hid[Mark Form]

@hid[Default Modeline Fields]

@hid[Fill Mode Hook]

@hid[Fundamental Mode Hook]
@End[Itemize]
Notice @hid[Mark Form] demonstrates that the "@f[f]" words may follow the
"@f[m]" order of the fields does not matter for @hid[Apropos].

The bindings of commands and values of variables are printed with the
documentation.
@enddefcom

@defcom[com "Describe Command", bind (Home d, C-_ d)]
This command prompts for a command and prints its full documentation and all
the keys bound to it.
@enddefcom

@defcom[com "Describe Key", bind (Home c, C-_ c, M-?)]
This command prints full documentation for the command which is bound to
the specified key in the current environment.
@enddefcom

@defcom[com "Describe Mode", bind (Home m, C-_ m)]
This command prints the documentation for a mode followed by a short
description of each of its mode-specific bindings.
@enddefcom

@defcom[com "Show Variable"]
@defcom1[com "Describe and Show Variable"]
@hid[Show Variable] prompts for the name of a variable and displays
the global value of the variable, the value local to the current buffer (if
any), and the value of the variable in all defined modes that have it as a
local variable.  @hid[Describe and Show Variable] displays the variable's
documentation in addition to the values.
@enddefcom

@defcom[com "What Lossage", bind (Home l, C-_ l)]
This command displays the last sixty key-events typed.  This can be useful
if, for example, you are curious what the command was that you typed by
accident.
@enddefcom

@defcom[com "Describe Pointer"]
This command displays the documentation and bindings for commands that have
some binding involving a mouse/pointer key-event.  It will not show the
documentation for the @hid[Illegal] command regardless of whether it has a
pointer binding.
@enddefcom

@defcom[com "Where Is", bind (Home w, C-_ w)]
This command prompts for the name of a command and displays its key
bindings in a pop-up window.  If a key binding is not global, the
environment in which it is available is displayed.
@enddefcom

@defcom[com "Generic Describe", bind (Home g, C-_ g)]
This command prints full documentation for any thing that has
documentation.  It first prompts for the kind of thing to document, the
following options being available:
@begin[description]
@i[attribute]@\Describe a character attribute, given its name.

@i[command]@\Describe a command, given its name.

@i[key]@\Describe a command, given a key to which it is bound.

@i[variable]@\Describe a variable, given its name.  This is the default.
@end[description]
@enddefcom


@section[Entering and Exiting]

@index[entering hemlock]@hemlock is entered by using the @clisp @f[ed]
function.  Simply typing @f[(ed)] will enter @hemlock, leaving you in the state
that you were in when you left it.  If @hemlock has never been entered before
then the current buffer will be @hid[Main].  The @f[-edit] command-line switch
may also be used to enter @hemlock: see page @pageref[edit-switch].

@f[ed] may optionally be given a file name or a symbol argument.  Typing 
@f[(ed @i[filename])] will cause the specified file to be read into @hemlock,
as though by @hid[Find File].  Typing @w<@f[(ed @i[symbol])]> will pretty-print
the definition of the symbol into a buffer whose name is obtained by adding
"@f[Edit ]" to the beginning of the symbol's name.

@defcom[com "Exit Hemlock", bind (C-c, C-x C-z)]
@defcom1[com "Pause Hemlock"]
@index[exiting hemlock]@hid[Exit Hemlock] exits @hemlock, returning @f[t].
@hid[Exit Hemlock] does not by default save modified buffers, or do
anything else that you might think it should do; it simply exits.  At any time
after exiting you may reenter by typing @f[(ed)] to @llisp without losing
anything.  Before you quit from @llisp using @f[(quit)], you should
save any modified files that you want to be saved.

@hid[Pause Hemlock] is similar, but it suspends the @llisp process and returns
control to the shell.  When the process is resumed, it will still be running
@hemlock.
@enddefcom


@section[Helpful Information]

@label[aborting]
@index[aborting]
@index[undoing]
@index[error recovery]
This section contains assorted helpful information which may be useful in
staying out of trouble or getting out of trouble.

@begin[itemize]
It is possible to get some sort of help nearly everywhere by typing
@binding[Home] or @binding[C-_].

Various commands take over the keyboard and insist that you type the key-events
that they want as input.  If you get in such a situation and want to get out,
you can usually do so by typing @bf[C-g] some small number of times.  If this
fails you can try typing @binding[C-x C-z] to exit @hemlock and then "@f[(ed)]"
to re-enter it.

Before you quit, make sure you have saved all your changes.  @binding[C-u C-x
C-b] will display a list of all modified buffers.  If you exit using @bf[C-x
M-z], then @hemlock will save all modified buffers with associated files.

If you lose changes to a file due to a crash or accidental failure to save,
look for backup ("@i[file]@f[.BAK]") or checkpoint ("@i[file]@f[.CKP]") files
in the same directory where the file was.

If the screen changes unexpectedly, you may have accidentally typed an
incorrect command.  Use @binding[Home l] to see what it was.  If you are
not familiar with the command, use @binding[Home c] to see what it is so that
you know what damage has been done.  Many interesting commands can be found
in this fashion.  This is an example of the much-underrated learning
technique known as "Learning by serendipitous malcoordination".  Who would
ever think of looking for a command that deletes all files in the current
directory?

If you accidentally type a "killing" command such as @binding[C-w], you can
get the lost text back using @binding[C-y].  The @hid[Undo] command is also
useful for recovering from this sort of problem.
@end[itemize]

@defhvar[var "Region Query Size", val {30}]
@index[large region]
Various commands ask for confirmation before modifying a region containing more
than this number of lines.  If this is @nil, then these commands refrain from
asking, no matter how large the region is.
@enddefhvar

@defcom[com "Undo"]
This command undoes the last major modification.  Killing commands and some
other commands save information about their modifications, so accidental uses
may be retracted.  This command displays the name of the operation to be undone
and asks for confirmation.  If the affected text has been modified between the
invocations of @hid[Undo] and the command to be undone, then the result may be
somewhat incorrect but useful.  Often @hid[Undo] itself can be undone by
invoking it again.
@enddefcom


@section[Recursive Edits]
@label[recursive-edits]
@index[recursive edits]
Some sophisticated commands, such as @hid[Query Replace], can place you in a
@i[recursive edit].  A recursive edit is simply a recursive invocation of
@hemlock done within a command.  A recursive edit is useful because it allows
arbitrary editing to be done during the execution of a command without losing
any state that the command might have.  When the user exits a recursive edit,
the command that entered it proceeds as though nothing happened.  @Hemlock
notes recursive edits in the @hid[Echo Area] modeline, or status line.  A
counter reflects the number of pending recursive edits.

@defcom[com "Exit Recursive Edit", bind (C-M-z)]
This command exits the current recursive edit, returning @nil.  If invoked when
not in a recursive edit, then this signals an user error.
@enddefcom

@defcom[com "Abort Recursive Edit", bind (@bf<C-]>)]
This command causes the command which invoked the recursive edit to get an
error.  If not in a recursive edit, this signals an user error.
@enddefcom


@section[User Errors]
@index[beeping]
@index[errors, user]
When in the course of editing, @hemlock is unable to do what it thinks you want
to do, then it brings this to your attention by a beep or a screen flash
(possibly accompanied by an explanatory echo area message such as @w<"@f[No
next line.]">.)  Although the exact attention-getting mechanism may vary on the
output device and variable settings, this is always called @i[beeping].

Whatever the circumstances, you had best try something else since @hemlock,
being far more stupid than you, is far more stubborn.  @hemlock is an
extensible editor, so it is always possible to change the command that
complained to do what you wanted it to do.

@section[Internal Errors]

@index[errors, internal]A message of this form may appear in the echo
area, accompanied by a beep:
@begin[programexample]
Internal error:
Wrong type argument, NIL, should have been of type SIMPLE-VECTOR.
@end[programexample]
If the error message is a file related error such as the following, then
you have probably done something illegal which @hemlock did not catch,
but was detected by the file system:
@begin[programexample]
Internal error:
No access to "/lisp2/emacs/teco.mid"
@end[programexample]
Otherwise, you have found a bug.  Try to avoid the behavior that resulted
in the error and report the problem to your system maintainer.  Since @llisp
has fairly robust error recovery mechanisms, probably no damage has been
done.

If a truly abominable error from which @hemlock cannot recover occurs,
then you will be thrown into the @llisp debugger.  At this point it would be
a good idea to save any changes with @f[save-all-buffers] and then start
a new @llisp.

@index[save-all-buffers, function]The @llisp function @f[save-all-buffers] may
be used to save modified buffers in a seriously broken @hemlock.  To use this,
type "@f[(save-all-buffers)]" to the top-level ("@f[* ]") or debugger
("@f<1] >") prompt and confirm saving of each buffer that should be saved.
Since this function will prompt in the "@f[Lisp]" window, it isn't very useful
when called inside of @hemlock.
