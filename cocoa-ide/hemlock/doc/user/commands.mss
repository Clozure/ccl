@comment{-*- Dictionary: hem; Mode: spell; Package: Hemlock -*-}
@chap[Basic Commands]
@section[Motion Commands]

@index[commands, basic]@index[motion]There is a fairly small number of
basic commands for moving around in the buffer.  While there are many other
more complex motion commands, these are by far the most commonly used and
the easiest to learn.

@defcom[com "Forward Character", bind (C-f, Rightarrow)]
@defcom1[com "Backward Character", bind (C-b, Leftarrow)]
@index[character, motion]
@hid[Forward Character] moves the point forward by one character.  If a prefix
argument is supplied, then the point is moved by that many characters.
@hid[Backward Character] is identical, except that it moves the point
backwards.
@enddefcom

@defcom[com "Forward Word", bind {M-f}]
@defcom1[com "Backward Word", bind {M-b}]
@index[word, motion]These commands move the point forward and backward
over words.  The point is always left between the last word and first
non-word character in the direction of motion.  This means that after moving
backward the cursor appears on the first character of the word, while after
moving forward, the cursor appears on the delimiting character.  Supplying
a prefix argument moves the point by that many words.
@enddefcom

@defcom[com "Next Line", bind (C-n, Downarrow)]
@defcom1[com "Previous Line", bind (C-p, Uparrow)]
@defcom1[com "Goto Absolute Line"]
@index[line, motion]
@hid[Next Line] and @hid[Previous Line] move to adjacent lines, while remaining
the same distance within a line.  Note that this motion is by logical lines,
each of which may take up many lines on the screen if it wraps.  If a prefix
argument is supplied, then the point is moved by that many lines.

The position within the line at the start is recorded, and each successive
use of @binding[C-p] or @binding[C-n] attempts to move the point to that
position on the new line.  If it is not possible to move to the recorded
position because the line is shorter, then the point is left at the end of
the line.

@hid[Goto Absolute Line] moves to the indicated line, as if you counted them
starting at the beginning of the buffer with number one.  If the user supplies
a prefix argument, it is the line number; otherwise, @Hemlock prompts the user
for the line.
@enddefcom

@defcom[com "End of Line", bind {C-e}]
@defcom1[com "Beginning of Line", bind {C-a}]
@hid[End of Line] moves the point to the end of the current line, while 
@hid[Beginning of Line] moves to the beginning.  If a prefix argument is
supplied, then the point is moved to the end or beginning of the line that
many lines below the current one.
@enddefcom

@defcom[com "Scroll Window Down", bind {C-v}]
@defcom1[com "Scroll Window Up", bind {M-v}]
@index[scrolling]
@hid[Scroll Window Down] moves forward in the buffer by one screenful of text,
the exact amount being determined by the size of the window.  If a prefix
argument is supplied, then this scrolls the screen that many lines.  When this
action scrolls the line with the point off the screen, it this command moves
the point to the vertical center of the window.  @hid[Scroll Window Up] is
identical to @hid[Scroll Window Down], except that it moves backwards.
@enddefcom

@defhvar[var "Scroll Overlap", val {2}]
This variable is used by @hid[Scroll Window Down] and @hid[Scroll Window Up] to
determine the number of lines by which the new and old screen should overlap.
@enddefhvar

@defcom[com "End of Buffer", bind (M-<)]
@defcom1[com "Beginning of Buffer", bind (@bf[M->])]
These commands are used to conveniently get to the very beginning and end of the
text in a buffer.  Before the point is moved, its position is saved by
pushing it on the mark stack (see page @pageref[marks]).
@enddefcom

@defcom[com "Top of Window", bind (M-,)]
@defcom1[com "Bottom of Window", bind (M-.)]
@index[window, motion]@hid[Top of Window] moves the point to the beginning of
the first line displayed in the current window.  @hid[Bottom of Window] moves
to the beginning of the last line displayed.
@enddefcom


@section[The Mark and The Region]

@label[marks]@index[marks]@index[region]@index[selection]Each buffer has a
distinguished position known as the @i[mark].  The mark initially points to the
beginning of the buffer.  The area between the mark and the point is known as
the @i[region].  Many @hemlock commands which manipulate large pieces of text
use the text in the region.  To use these commands, one must first use some
command to mark the region.

@index[active regions]Although the mark is always pointing somewhere (initially
to the beginning of the buffer), region commands insist that the region be made
@i[active] before it can be used.  This prevents accidental use of a region
command from mysteriously mangling large amounts of text.

@defhvar[var "Active Regions Enabled", val {t}]
When this variable is true, region commands beep unless the region is active.
This may be set to @false for more traditional @emacs region semantics.
@enddefhvar

Once a marking command makes the region active, it remains active until:
@begin[itemize]
a command uses the region,

a command modifies the buffer,

a command changes the current window or buffer,

a command signals an editor error,

or the user types @binding[C-g].
@end[itemize]
Motion commands have the effect of redefining the region, since they move the
point and leave the region active.

@index[ephemerally active regions]Commands that insert a large chunk of
text into the buffer usually set an @i[ephemerally active] region around
the inserted text.  An ephemerally active region is always deactivated by
the next command, regardless of the kind of command.  The ephemerally
active region allows an immediately following region command to manipulate
the inserted text, but doesn't persist annoyingly.  This is also very
useful with active region highlighting, since it visibly marks the inserted
text.


@defhvar[var "Highlight Active Region", val {t}]
@defhvar1[var "Active Region Highlighting Font", val {nil}]
When @hid[Highlight Active Region] is true, @hemlock displays the text in the
region in a different font whenever the region is active.  This provides a
visible indication of what text will be manipulated by a region command.
Active region highlighting is only supported under @windows.

@hid[Active Region Highlighting Font] is the name of the font to use for active
region highlighting.  If unspecified, @hemlock uses an underline font.
@enddefhvar


@defcom[com "Set/Pop Mark", bind (C-@@)]
This command moves the mark to the point (saving the old mark on the mark
stack) and activates the region.  After using this command to mark one end of
the region, use motion commands to move to the other end, then do the region
command.  This is the traditional @emacs marking command; when running under a
windowing system with mouse support, it is usually easier to use the mouse with
the @comref[Point to Here] and @comref[Generic Pointer Up].

For historical reasons, the prefix argument causes this command to do things
that are distinct commands in @Hemlock.  A prefix argument of four does
@hid[Pop and Goto Mark], and a prefix argument of @f[16] does
@hid[Pop Mark].
@enddefcom

@defcom[com "Mark Whole Buffer", bind (C-x h)]
@defcom1[com "Mark to Beginning of Buffer", bind (C-<)]
@defcom1[com "Mark to End of Buffer", bind (C->)]
@hid[Mark Whole Buffer] sets the region around the whole buffer, with the point
at the beginning and the mark at the end.  If a prefix argument is supplied,
then the mark is put at the beginning and the point at the end.  The mark is
pushed on the mark stack beforehand, so popping the stack twice will restore
it.

@hid[Mark to Beginning of Buffer] sets the current region from point to the
beginning of the buffer.

@hid[Mark to End of Buffer] sets the current region from the end of the buffer
to point.
@enddefcom

@defcom[com "Activate Region", bind (C-x C-Space, C-x C-@@)]
This command makes the region active, using whatever the current position of
the mark happens to be.  This is useful primarily when the region is
accidentally deactivated.
@enddefcom


@subsection[The Mark Stack]

@index[mark stack]As was hinted at earlier, each buffer has a @i[mark stack],
providing a history of positions in that buffer.  The current mark is the mark
on the top of the stack; earlier values are recovered by popping the stack.
Since commands that move a long distance save the old position on the mark
stack, the mark stack commands are useful for jumping to interesting places in
a buffer without having to do a search.

@defcom[com "Pop Mark", bind (C-M-Space)]
@defcom1[com "Pop and Goto Mark", bind (M-@@, M-Space)]
@hid[Pop Mark] pops the mark stack, restoring the current mark to the next most
recent value.  @hid[Pop and Goto Mark] also pops the mark stack, but instead of
discarding the current mark, it moves the point to that position.  Both
commands deactivate the region.
@enddefcom

@defcom[com "Exchange Point and Mark", bind (C-x C-x)]
This command interchanges the position of the point and the mark, thus moving
to where the mark was, and leaving the mark where the point was.  This command
can be used to switch between two positions in a buffer, since repeating it
undoes its effect.  The old mark isn't pushed on the mark stack, since it is
saved in the point.
@enddefcom


@subsection[Using The Mouse]

@index[mouse]It can be convenient to use the mouse to point to positions in
text, especially when moving large distances.  @hemlock defines several
commands for using the mouse.  These commands can only be used when running
under @windows (see page @pageref[using-x].)

@defcom[com "Here to Top of Window", bind (Rightdown)]
@defcom1[com "Top Line to Here", bind (Leftdown)]
@index[window, motion]@hid[Here to Top of Window] scrolls the window so as to
move the line which is under the mouse cursor to the top of the window.  This
has the effect of moving forward in the buffer by the distance from the top of
the window to the mouse cursor.  @hid[Top Line to Here] is the inverse
operation, it scrolls backward, moving current the top line underneath the
mouse.

If the mouse is near the left edge of a window, then these commands do smooth
scrolling.  @hid[Here To Top of Window] repeatedly scrolls the window up by one
line until the mouse button is released.  Similarly, @hid[Top Line to Here]
smoothly scrolls down.
@enddefcom

@defcom[com "Point to Here", bind (Middledown, S-Leftdown)]
This command moves the point to the position of the mouse, changing to a
different window if necessary.

When used in a window's modeline, this moves the point of the window's buffer
to the position within the file that is the same percentage, start to end, as
the horizontal position of the mouse within the modeline.  This also makes this
window current if necessary.

This command supplies a function @hid[Generic Pointer Up] invokes if it runs
without any intervening generic pointer up predecessors executing.  If the
position of the pointer is different than the current point when the user
invokes @hid[Generic Pointer Up], then this function pushes a buffer mark at
point and moves point to the pointer's position.  This allows the user to mark
off a region with the mouse.
@enddefcom

@defcom[com "Generic Pointer Up", bind (Middleup, S-Leftup)]
Other commands determine this command's action by supplying functions that
this command invokes.  The following built-in commands supply the following
generic up actions:
@Begin[Description]
@hid[Point to Here]@\
 When the position of the pointer is different than the current point, the
action pushes a buffer mark at point and moves point to the pointer's position.

@hid[Bufed Goto and Quit]@\
 The action is a no-op.
@End[Description]
@enddefcom

@defcom[com "Insert Kill Buffer", bind (S-Rightdown)]
This command is a combination of @hid[Point to Here] and @comref[Un-Kill].  It
moves the point to the mouse location and inserts the most recently killed
text.
@enddefcom


@section[Modification Commands]
@index[commands, modification]

There is a wide variety of basic text-modification commands, but once again the
simplest ones are the most often used.

@subsection[Inserting Characters]
@index[character, insertion]
@index[insertion, character]

In @hemlock, you can insert characters with graphic representations by typing
the corresponding key-event which you normally generate with the obvious
keyboard key.  You can only insert characters whose codes correspond to ASCII
codes.  To insert those without graphic representations, use @hid[Quoted
Insert].

@defcom[com "Self Insert"]
@hid[Self Insert] inserts into the buffer the character corresponding to the
key-event typed to invoke the command.  This command is normally bound to all
such key-events @binding[Space].  If a prefix argument is supplied, then this
inserts the character that many times.
@enddefcom

@defcom[com "New Line", bind (Return)]
This command, which has roughly the same effect as inserting a @bf[Newline],
is used to move onto a new blank line.  If there are at least two blank
lines beneath the current one then @binding[Return] cleans off any
whitespace on the next line and uses it, instead of inserting a newline.
This behavior is desirable when inserting in the middle of text, because
the bottom half of the screen does not scroll down each time @hid[New Line]
is used.
@enddefcom

@defcom[com "Quoted Insert", bind {C-q}]
Many key-events have corresponding ASCII characters, but these key-events are
bound to commands other than @hid[Self Insert].  Sometimes they are otherwise
encumbered such as with @binding[C-g].  @hid[Quoted Insert] prompts for a
key-event, without any command interpretation semantics, and inserts the
corresponding character.  If the appropriate character has some code other than
an ASCII code, this will beep and abort the command.  A common use for this
command is inserting a @bf[Formfeed] by typing @binding[C-q C-l].  If a prefix
argument is supplied, then the character is inserted that many times.
@enddefcom

@defcom[com "Open Line", bind {C-o}]
This command inserts a newline into the buffer without moving the point.
This command may also be given a prefix argument to insert a number of
newlines, thus opening up some room to work in the middle of a screen of
text.  See also @comref[Delete Blank Lines].
@enddefcom


@subsection[Deleting Characters]
@index[deletion, character]
@index[character, deletion]
There are a number of commands for deleting characters as well.

@defhvar[var "Character Deletion Threshold", val {5}]
If more than this many characters are deleted by a character deletion command,
then the deleted text is placed in the kill ring.
@enddefhvar

@defcom[com "Delete Next Character", bind {C-d}]
@defcom1[com "Delete Previous Character", bind (Delete, Backspace)]
@hid[Delete Next Character] deletes the character immediately following the
point, that is, the character which appears under the cursor.  When given a
prefix argument, @binding[C-d] deletes that many characters after the
point.  @hid[Delete Previous Character] is identical, except that it
deletes characters before the point.
@enddefcom

@defcom[com "Delete Previous Character Expanding Tabs"]
@hid[Delete Previous Character Expanding Tabs] is identical to
@hid[Delete Previous Character], except that it treats tabs as the
equivalent number of spaces.  Various language modes that use tabs for
indentation bind @binding[Delete] to this command.
@enddefcom


@subsection[Killing and Deleting]

@index[killing]@index[cutting]@index[pasting]@index[kill ring]@hemlock has many
commands which kill text.  Killing is a variety of deletion which saves the
deleted text for later retrieval.  The killed text is saved in a ring buffer
known as the @i[kill ring].  Killing has two main advantages over deletion:
@begin[enumerate]
If text is accidentally killed, a not uncommon occurrence, then it can be
restored.

Text can be moved from one place to another by killing it and then
restoring it in the new location.
@end[enumerate]

Killing is not the same as deleting.  When a command is said to delete
text, the text is permanently gone and is not pushed on the kill ring.
Commands which delete text generally only delete things of little
importance, such as single characters or whitespace.

@subsection[Kill Ring Manipulation]
@defcom[com "Un-Kill", bind {C-y}]
@index[kill ring, manipulation]This command "yanks" back the most
recently killed piece of text, leaving the mark before the inserted text
and the point after.  If a prefix argument is supplied, then the text that
distance back in the kill ring is yanked.
@enddefcom

@defcom[com "Rotate Kill Ring", bind {M-y}]
This command rotates the kill ring forward, replacing the most recently
yanked text with the next most recent text in the kill ring. @binding[M-y]
may only be used immediately after a use of @binding[C-y] or a previous
use of @binding[M-y].  This command is used to step back through the text
in the kill ring if the desired text was not the most recently killed, and
thus could not be retrieved directly with a @binding[C-y].  If a prefix
argument is supplied, then the kill ring is rotated that many times.
@enddefcom

@defcom[com "Kill Region", bind {C-w}]
@index[region, killing]This command kills the text between the point and
mark, pushing it onto the kill ring.  This command is usually the best way
to move or remove large quantities of text.
@enddefcom

@defcom[com "Save Region", bind {M-w}]
This command pushes the text in the region on the kill ring, but doesn't
actually kill it, giving an effect similar to typing @binding[C-w C-y].
This command is useful for duplicating large pieces of text.
@enddefcom

@subsection[Killing Commands]

@index[commands, killing]Most commands which kill text append into the
kill ring, meaning that consecutive uses of killing commands will insert
all text killed into the top entry in the kill ring.  This allows large
pieces of text to be killed by repeatedly using a killing command.

@defcom[com "Kill Line", bind {C-k}]
@defcom1[com "Backward Kill Line"]
@index[line, killing]@hid[Kill Line] kills the text from the point to the
end of the current line, deleting the line if it is empty.  If a prefix
argument is supplied, then that many lines are killed.  Note that a prefix
argument is not the same as a repeat count.

@hid[Backward Kill Line] is similar, except that it kills from the point to the
beginning of the line.  If it is called at the beginning of the line, it kills
the newline and any trailing whitespace on the previous line.  With a prefix
argument, this command is the same as @hid[Kill Line] with a negated argument.
@enddefcom

@defcom[com "Kill Next Word", bind {M-d}]
@defcom1[com "Kill Previous Word", bind (M-Backspace, M-Delete)]
@index[word, killing]@hid[Kill Next Word] kills from the point to the end
of the current or next word.  If a prefix argument is supplied, then that
many words are killed.  @hid[Kill Previous Word] is identical, except that
it kills backward.
@enddefcom

@subsection[Case Modification Commands]

@index[case modification]@hemlock provides a few case modification
commands, which are often useful for correcting typos.

@defcom[com "Capitalize Word", bind {M-c}]
@defcom1[com "Lowercase Word", bind {M-l}]
@defcom1[com "Uppercase Word", bind {M-u}]
@index[word, case modification]These commands modify the case of the
characters from the point to the end of the current or next word, leaving
the point after the end of the word affected.  A positive prefix argument
modifies that many words, moving forward.  A negative prefix argument
modifies that many words before the point, but leaves the point unmoved.
@enddefcom

@defcom[com "Lowercase Region", bind (C-x C-l)]
@defcom1[com "Uppercase Region", bind (C-x C-u)]
@index[region, case modification]These commands case-fold the text in the
region.  Since these commands can damage large amounts of text, they ask for
confirmation before modifying large regions and can be undone with @hid[Undo].
@enddefcom

@subsection[Transposition Commands]

@index[transposition]@index[commands, transposition]@hemlock provides a
number of transposition commands.  A transposition command swaps the
"things" before and after the point and moves forward one "thing".  Just
how a "thing" is defined depends on the particular transposition command.
Transposition commands, particularly
@hid[Transpose Characters] and @hid[Transpose Words], are useful for
correcting typos.  More obscure transposition commands can be used to amaze
your friends and demonstrate your immense knowledge of exotic @emacs
commands.

To the uninitiated, the behavior of transposition commands may seem mysterious;
this has led some implementors to attempt to improve the definition of
transposition, but right-thinking people will accept no substitutes.  The
@emacs transposition definition used in @hemlock has two useful properties:
@begin[enumerate]
Repeated applications of a transposition command have a useful effect.  The
way to visualize this effect is that each use of the transposition command
drags the previous thing over the next thing.  It is possible to correct
double transpositions easily using @hid[Transpose Characters].

Transposition commands move backward with a negative prefix argument, thus
undoing the effect of the equivalent positive argument.
@end[enumerate]

@defcom[com "Transpose Characters", bind {C-t}]
@index[character, transposition]This command exchanges the characters on
either side of the point and moves forward, unless at the end of a line, in
which case it transposes the previous two characters without moving.
@enddefcom

@defcom[com "Transpose Lines", bind (C-x C-t)]
@index[line, transposition]This command transposes the previous and
current line, moving down to the next line.  With a zero argument, it
transposes the current line and the line the mark is on.
@enddefcom

@defcom[com "Transpose Words", bind {M-t}]
@index[word, transposition]This command transposes the previous word and
the current or next word.
@enddefcom


@defcom[com "Transpose Regions", bind (C-x t)]
This command transposes two regions with endpoints defined by the mark stack
and point.  To use this command, place three marks (in order) at the start and
end of the first region, and at the start of the second region, then place the
point at the end of the second region.  Unlike the other transposition
commands, a second use will simply undo the effect of the first use, and to do
even this, you must reactivate the current region.
@enddefcom


@subsection[Whitespace Manipulation]
These commands change the amount of space between words.  See also the
indentation commands in section @ref[indentation].

@defcom[com "Just One Space", bind (M-|)]
@index[whitespace, manipulation]@index[indentation, manipulation]This
command deletes all whitespace characters before and after the point and then
inserts one space.  If a prefix argument is supplied, then that number of
spaces is inserted.
@enddefcom

@defcom[com "Delete Horizontal Space", bind (M-\)]
This command deletes all blank characters around the point.
@enddefcom

@defcom[com "Delete Blank Lines", bind (C-x C-o)]
This command deletes all blank lines surrounding the current line, leaving the
point on a single blank line.  If the point is already on a single blank line,
then that line is deleted.  If the point is on a non-blank line, then all blank
lines immediately following that line are deleted.  This command is often used
to clean up after @comref[Open Line].
@enddefcom

@section[Filtering]

@i[Filtering] is a simple way to perform a fairly arbitrary transformation
on text.  Filtering text replaces the string in each line with the result
of applying a @llisp function of one argument to that string.  The function must 
neither destructively modify the argument nor the return value.  It is an
error for the function to return a string containing newline characters.

@defcom[com "Filter Region"]
This function prompts for an expression which is evaluated to obtain a
function to be used to filter the text in the region.  For example, to
capitalize all the words in the region one could respond:
@begin[programexample]
Function: #'@comment<>string-capitalize
@end[programexample]
Since the function may be called many times, it should probably be
compiled.  Functions for one-time use can be compiled using the compile
function as in the following example which removes all the semicolons on any line
which contains the string "@f[PASCAL]":
@begin[programexample]
Function: (compile nil '(lambda (s)
			  (if (search "PASCAL" s)
			      (remove #\; s)
			      s)))
@end[programexample]
@enddefcom

@section[Searching and Replacing]
@index[searching]@index[replacing]
Searching for some string known to appear in the text is a commonly used method
of moving long distances in a file.  Replacing occurrences of one pattern with
another is a useful way to make many simple changes to text.  @hemlock provides
powerful commands for doing both of these operations.

@defhvar[var "String Search Ignore Case", val {t}]
@index[case sensitivity]
This variable determines the kind of search done by searching and replacing
commands.  
@enddefhvar

@defcom[com "Incremental Search", bind {C-s}]
@defcom1[com "Reverse Incremental Search", bind {C-r}]
@hid[Incremental Search] searches for an occurrence of a string after the
current point.  It is known as an incremental search because it reads
key-events form the keyboard one at a time and immediately searches for the
pattern of corresponding characters as you type.  This is useful because
it is possible to initially type in a very short pattern and then add more
characters if it turns out that this pattern has too many spurious matches.

This command dispatches on the following key-events as sub-commands:
@begin[description]
@binding[C-s]@\
 Search forward for an occurrence of the current pattern.  This can be used
repeatedly to skip from one occurrence of the pattern to the next, or it can be
used to change the direction of the search if it is currently a reverse search.
If @binding[C-s] is typed when the search string is empty, then a search is
done for the string that was used by the last searching command.

@binding[C-r]@\
 Similar to @binding[C-s], except that it searches backwards.

@binding[Delete, Backspace]@\
 Undoes the effect of the last key-event typed.  If that key-event simply added
to the search pattern, then this removes the character from the pattern, moving
back to the last match found before entering the removed character.  If the
character was a @binding[C-s] or @binding[C-r], then this moves back to the
previous match and possibly reverses the search direction.

@binding[C-g]@\
 If the search is currently failing, meaning that there is no occurrence of the
search pattern in the direction of search, then @binding[C-g] deletes enough
characters off the end of the pattern to make it successful.  If the search
is currently successful, then @binding[C-g] causes the search to be aborted,
leaving the point where it was when the search started.  Aborting the search
inhibits the saving of the current search pattern as the last search string.

@binding[Escape]@\
 Exit at the current position in the text, unless the search string is empty,
in which case a non-incremental string search is entered.

@binding[C-q]@\
 Search for the character corresponding to the next key-event, rather than
treating it as a command.
@end[description]
Any key-event not corresponding to a graphic character, except those just
described, causes the search to exit.  @hemlock then uses the key-event in it
normal command interpretation.

For example, typing @binding[C-a] will exit the search @i[and] go to the
beginning of the current line.  When either of these commands successfully
exits, they push the starting position (before the search) on the mark stack.
If the current region was active when the search started, this foregoes pushing
a mark.
@enddefcom

@defcom[com "Forward Search", bind (M-s)]
@defcom1[com "Reverse Search", bind (M-r)]
These commands do a normal dumb string search, prompting for the search
string in a normal dumb fashion.  One reason for using a non-incremental
search is that it may be faster since it is possible to specify a long
search string from the very start.  Since @hemlock uses the Boyer--Moore
search algorithm, the speed of the search increases with the size of the
search string.
When either of these commands successfully exits, they push the starting
position (before the search) on the mark stack.  This is inhibited when the
current region is active.
@enddefcom

@defcom[com "Query Replace", bind (M-%)]
This command prompts in the echo area for a target string and a replacement
string.  It then searches for an occurrence of the target after the point.
When it finds a match, it prompts for a key-event indicating what action to
take.  The following are valid responses:
@begin[description]
@binding[Space, y]@\
 Replace this occurrence of the target with the replacement string, and search
again.

@binding[Delete, Backspace, n]@\
 Do not replace this occurrence, but continue the search.

@binding[!]@\
 Replace this and all remaining occurrences without prompting again.

@binding[.]@\
 Replace this occurrence and exit.

@binding[C-r]@\
 Go into a recursive edit (see page @pageref[recursive-edits]) at the current
location.  The search will be continued from wherever the point is left when
the recursive edit is exited.  This is useful for handling more complicated
cases where a simple replacement will not achieve the desired effect.

@binding[Escape]@\
 Exit without doing any replacement.

@binding[Home, C-_, ?, h]@\
 Print a list of all the options available.
@end[description]
Any other key-event causes the command to exit, returning the key-event to the
input stream; thus, @hemlock will interpret it normally for a command binding.

When the current region is active, this command uses it instead of the region
from point to the end of the buffer.  This is especially useful when you expect
to use the @binding[!] option.

If the replacement string is all lowercase, then a heuristic is used that
attempts to make the case of the replacement the same as that of the
particular occurrence of the target pattern.  If "@f[foo]" is being
replaced with "@f[bar]" then "@f[Foo]" is replaced with "@f[Bar]" and
"@f[FOO]" with "@f[BAR]".

This command may be undone with @hid[Undo], but its undoing may not be undone.
On a successful exit from this command, the starting position (before the
search) is pushed on the mark stack.
@enddefcom

@defhvar[var "Case Replace", val {t}]
@index[case sensitivity]
If this variable is true then the case preserving heuristic in
@hid[Query Replace] is enabled, otherwise all replacements are done with
the replacement string exactly as specified.
@enddefhvar

@defcom[com "Replace String"]
This command is the same as @hid[Query Replace] except it operates without ever
querying the user before making replacements.  After prompting for a target and
replacement string, it replaces all occurrences of the target string following
the point.  If a prefix argument is specified, then only that many occurrences
are replaced.  When the current region is active, this command uses it instead
of the region from point to the end of the buffer.
@enddefcom

@defcom[com "List Matching Lines"]
This command prompts for a search string and displays in a pop-up window all
the lines containing the string that are after the point.  If a prefix argument
is specified, then this displays that many lines before and after each matching
line.  When the current region is active, this command uses it instead of the
region from point to the end of the buffer.
@enddefcom

@defcom[com "Delete Matching Lines"]
@defcom1[com "Delete Non-Matching Lines"]
@hid[Delete Matching Lines] prompts for a search string and deletes all lines
containing the string that are after the point.  Similarly, @hid[Delete
Non-Matching Lines] deletes all lines following the point that do not contain
the specified string.  When the current region is active, these commands uses
it instead of the region from point to the end of the buffer.
@enddefcom


@section[Page Commands]
@index[page commands]
Another unit of text recognized by @hemlock is the page.  A @i[page] is a piece
of text delimited by formfeeds (@f[^L]'s.)  The first non-blank line after the
page marker is the @i[page title].  The page commands are quite useful when
logically distinct parts of a file are put on separate pages.  See also
@comref[Count Lines Page].  These commands only recognize @f[^L]'s at the
beginning of a lines, so those quoted in string literals do not get in the way.

@defcom[com "Previous Page", bind (C-x @bf<]>)]
@defcom1[com "Next Page", bind (C-x [)]
@hid[Previous Page] moves the point to the previous page delimiter, while
@hid[Next Page] moves to the next one.  Any page delimiters next to the point
are skipped.  The prefix argument is a repeat count.
@enddefcom

@defcom[com "Mark Page", bind (C-x C-p)]
This command puts the point at the beginning of the current page and the mark
at the end.  If given a prefix argument, marks the page that many pages from the
current one.
@enddefcom

@defcom[com "Goto Page"]
This command does various things, depending on the prefix argument:
@begin[description]
@i[no argument]@\goes to the next page.

@i[positive argument]@\goes to an absolute page number, moving that many pages
from the beginning of the file.

@i[zero argument]@\prompts for string and goes to the page with that string in
its title.  Repeated invocations in this manner continue searching from the
point of the last find, and a first search with a particular pattern pushes a
buffer mark.

@i[negative argument]@\moves backward by that many pages, if possible.
@end[description]
@enddefcom

@defcom[com "View Page Directory"]
@defcom1[com "Insert Page Directory"]
@hid[View Page Directory] uses a pop-up window to display the number and title
of each page in the current buffer.  @hid[Insert Page Directory] is the same
except that it inserts the text at the beginning of the buffer.  With a prefix
argument, @hid[Insert Page Directory] inserts at the point.
@enddefcom


@section[Counting Commands]

@defcom[com "Count Words"]
This command counts the number of words from the current point to the end of
the buffer, displaying a message in the echo area.  When the current region is
active, this uses it instead of the region from the point to the end of the
buffer.  Word delimiters are determined by the current major mode.
@enddefcom

@defcom[com "Count Lines"]
This command counts the number of lines from the current point to the end of
the buffer, displaying a message in the echo area.  When the current region is
active, this uses it instead of the region from the point to the end of the
buffer.  
@enddefcom

@defcom[com "Count Lines Page", bind (C-x l)]
This command displays the number of lines in the current page and the number of
lines before and after the point within that page.  If given a prefix argument,
the entire buffer is counted instead of just the current page.
@enddefcom

@defcom[com "Count Occurrences"]
This command prompts for a search string and displays the number of occurrences
of that string in the text from the point to the end of the buffer.  When the
current region is active, this uses it instead of the region from the point to
the end of the buffer.
@enddefcom


@section[Registers]
@index[registers]
Registers allow you to save a text position or chunk of text associated with a
key-event.  This is a convenient way to repeatedly access a commonly-used
location or text fragment.  The concept and key bindings should be familiar to
TECO users.

@defcom[com "Save Position", bind (C-x s)]
@defcom1[com "Jump to Saved Position", bind (C-x j)]
These commands manipulate registers containing textual positions.  
@hid[Save Position] prompts for a register and saves the location of the
current point in that register.  @hid[Jump to Saved Position] prompts for a
register and moves the point to the position saved in that register.  If the
saved position is in a different buffer, then that buffer is made current.
@enddefcom

@defcom[com "Put Register", bind (C-x x)]
@defcom1[com "Get Register", bind (C-x g)]
These commands manipulate registers containing text.  @hid[Put Register]
prompts for a register and puts the text in the current region into the
register.  @hid[Get Register] prompts for a register and inserts the text in
that register at the current point.
@enddefcom

@defcom[com "List Registers"]
@defcom1[com "Kill Register"]
@hid[List Registers] displays a list of all the currently defined registers in
a pop-up window, along with a brief description of their contents.  
@hid[Kill Register] prompts for the name of a register and deletes that
register.
@enddefcom
