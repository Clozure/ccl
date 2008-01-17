@comment{-*- Dictionary: /afs/cs/project/clisp/docs/hem/hem; Mode: spell; Package: Hemlock -*-}

@chap[The Hemlock Netnews Interface]
@section[Introduction to Netnews in Hemlock]


@hemlock provides a facility for reading bulletin boards through the
NetNews Transfer Protocol (NNTP).  You can easily read Netnews, reply to
news posts, post messages, etc.  The news reading interface is consistent
with that of the @hemlock mailer, and most Netnews commands function in the
same manner as their mailer counterparts.

Netnews can be read in one of two different modes.  The first mode, invoked
by the @hid(Netnews) command, allows the user to read new messages in
groups which the user has specified.  This method of reading netnews will
track the highest numbered message in each newsgroup and only show new
messages which have arrived since then.  The @hid(Netnews Browse) command
invokes the other method of reading netnews.  This mode displays a list of
all newsgroups, and the user may choose to read messages in any of them.
By default, the news reader will not track the latest message read when
browsing, and it will always display the last few messages.


@section[Setting Up Netnews]

To start reading bulletin boards from @hemlock you probably need to create a
file containing the newsgroups you want to read.

@defhvar[var "Netnews Group File", val {".hemlock-groups"}]
   When you invoke the @hid(Netnews) command, @hemlock merges the value of
   this variable with your home directory and looks there for a list of
   groups (one per line) to read.
@enddefhvar

@defhvar[var "Netnews Database File", val{".hemlock-netnews"}]
When you invoke the @hid(Netnews) command, @hemlock merges the value of
this variable with your home directory.  This file maintains a pointer to
the highest numbered message read in each group in @hid(Netnews Group
File).
@enddefhvar

@defcom[com "List All Groups"]
   When you invoke this command, @hemlock creates a buffer called
   @hid(Netnews Groups) and inserts the names of all accessible Netnews
   groups into it alphabetically.  You may find this useful if you choose to set
   up your @hid(Netnews Group File) manually.
@enddefcom

@defhvar[var "Netnews NNTP Server", val{"netnews.srv.cs.cmu.edu"}]
This variable stores the host name of the machine which @hemlock will use
as the NNTP server.
@enddefhvar

@defhvar[var "Netnews NNTP Timeout Period", val{30}]
This is the number of seconds @hemlock will wait trying to connect to the
NNTP server.  If a connection is not made within this time period, the
connection will time out and an error will be signalled.
@enddefhvar

@subsection[News-Browse Mode]

   @hid(News-Browse) mode provides an easy method of adding groups to
   your @hid(Netnews Group File).

@defcom[com "Netnews Browse"]
   This command sets up a buffer in @hid{News-Browse} mode with all
   available groups listed one per line.  Groups may be read or added
   to your group file using various commands in this mode.
@enddefcom

@defcom[com "Netnews Browse Add Group To File", stuff (bound to @bf[a] in @hid[News-Browse] mode)]
@defcom1[com "Netnews Browse Pointer Add Group to File"]
@hid(Netnews Browse Add Group to File) adds the group under the point to
your group file, and @hid(Netnews Browse Pointer Add Group To File) adds
the group under the mouse pointer without moving the point.
@enddefcom

@defcom[com "Netnews Browse Read Group", stuff (bound to @bf[space] in @hid[News-Browse] mode)]
@defcom1[com "Netnews Browse Pointer Read Group"]
@hid(Netnews Browse Read Group) and @hid(Netnews Browse Pointer Read Group)
read the group under the cursor and the group under the mouse pointer,
respectively.  These commands neither use nor modify the contents of your
@hid(Netnews Database File); they will always present the last few messages
in the newsgroup, regardless of the last message read.  @hid(Netnews Browse
Pointer Read Group) does not modify the position of the point.
@enddefcom

@defcom[com "Netnews Quit Browse"]
   This command exits @hid(News-Browse) mode.
@enddefcom

The @hid(Next Line) and @hid(Previous Line) commands are conveniently bound to
@bf[n] and @bf[p] in this mode.

@section[Starting Netnews]

Once your @hid(Netnews Group File) is set up, you may begin reading netnews.

@defcom[com "Netnews"]
   This command is the main entry point for reading bulletin boards in
   @hemlock.  Without an argument, the system looks for what bulletin boards to
   read in the value of @hid(Netnews Group File) and reads each of them in
   succession.  @hemlock keeps a pointer to the last message you read in each
   of these groups in your @hid(Netnews Database File).  Bulletin boards may
   be added to your @hid(Netnews Group File) manually or by using
   the @hid(Netnews Browse) facility.  With an argument, @hemlock prompts the 
   user for the name of a bulletin board and reads it.
@enddefcom

@defcom[com "Netnews Look at Group"]
   This command prompts for a group and reads it, ignoring the information
   in your @hid(Netnews Database File).
@enddefcom

When you read a group, @hemlock creates a buffer that contains important
header information for the messages in that group.  There are four fields
in each header, one each for the @i(date), @i(lines), @i(from), and
@i(subject).  The @i(date) field shows when the message was sent, the
@i(lines) field displays how long the message is in lines, the @i(from)
field shows who sent the message, and the @i(subject) field displays the
subject of this message.  If a field for a message is not available, @f(NA)
will appear instead.  You may alter the length of each of these fields by
modifying the following @hemlock variables:

@defhvar[var "Netnews Before Date Field Pad", val 1]
   How many spaces should be inserted before the date in @hid(News-Headers)
   buffers.
@enddefhvar

@defhvar[var "Netnews Date Field Length", val 6]
@defhvar1[var "Netnews Line Field Length", val 3]
@defhvar1[var "Netnews From Field Length", val 20]
@defhvar1[var "Netnews Subject Field Length", val 43]
   These variables control how long the @i(date), @i(line), @i(from), and
   @i(subject) fields should be in @hid{News-Headers} buffers.
@enddefhvar

@defhvar[var "Netnews Field Padding", val 2]
   How many spaces should be left between the Netnews @i(date), @i(from), 
   @i(lines), and @i(subject) fields after padding to the required length.
@enddefhvar

For increased speed, @hemlock only inserts headers for a subset of the
messages in each group.  If you have never read a certain group, and the
value of @hid(Netnews New Group Style) is @f(:from-end) (the default),
@hemlock inserts some number of the last messages in the group, determined
by the value of @hid(Netnews Batch Count).  If the value of @hid(Netnews
New Group Style) is @f(:from-start), @hemlock will insert the first batch
of messages in the group.  If you have read a group before, @hemlock will
insert the batch of messages following the highest numbered message that
you had read previously.

@defhvar[var "Netnews Start Over Threshold", val {350}]
   If the number of new messages in a group exceeds the value of this
   variable and @hid(Netnews New Group Style) is @f(:from-end), @hemlock asks
   if you would like to start reading this group from the end.
@enddefhvar

You may at any time go beyond the messages that are visible using the 
@hid(Netnews Next Line), @hid(Netnews Previous Line),
@hid(Netnews Headers Scroll Window Up), and
@hid(Netnews Headers Scroll Down) commands in @hid(News-Headers) mode,
or the @hid(Netnews Next Article) and @hid(Netnews Previous Article)
commands in @hid(News-Message) mode.

@defhvar[var "Netnews Fetch All Headers", val {nil}]
This variable determines whether Netnews will fetch all headers immediately
upon entering a new group.
@enddefhvar

@defhvar[var "Netnews Batch Count", val {50}]
   This variable determines how many headers the Netnews facility will fetch
   at a time.
@enddefhvar

@defhvar[var "Netnews New Group Style", val {:from-end}]
This variable determines what happens when you read a group that you have
never read before.  When it is @f(:from-start), the @hid(Netnews) command
will read from the beginning of a new group forward.  When it is @f(:from-end),
the default, @hid(Netnews) will read the group from the end backward.
@enddefhvar

@section[Reading Messages]

From a @hid{News-Headers} buffer, you may read messages, reply to messages
via the @hemlock mailer, or reply to messages via post.  Some commands are
also bound to ease getting from one header to another.

@defcom[com "Netnews Show Article", stuff (bound to @bf[space] in @hid{News-Headers} mode)]
@defhvar1[var "Netnews Read Style", val {:multiple}]
@defhvar1[var "Netnews Headers Proportion", val {0.25}]
This command puts the body of the message header under the current point
into a @hid{News-Message} buffer.  If the value of @hid(Netnews Read
Style) is @f(:single), @hemlock changes to the @hid{News-Message}
buffer.  If it is @f(:multiple), then @hemlock splits the current window
into two windows, one for headers and one for message bodies.  The headers
window takes up a proportion of the current window based on the value of
@hid(Netnews Headers Proportion).  If the window displaying the
@hid(News-Headers) buffer has already been split, and the message
currently displayed in the @hid(News-Message) window is the same as the
one under the current point, this command behaves just like @hid(Netnews
Message Scroll Down).
@enddefcom

@defhvar[var "Netnews Message Header Fields", val {nil}]
   When this variable is @nil, all available fields are displayed in the
   header of a message.  Otherwise, this variable should containt a list of
   fields to include in message headers.  If an element of this
   list is an atom, then it should be the string name of a field.  If it is
   a cons, then the car should be the string name of a field, and the cdr
   should be the length to which this field should be limited.  Any string
   name is acceptable, and fields that do not exist are ignored.
@enddefhvar   

@defcom[com "Netnews Show Whole Header", stuff (bound to @bf[w] in @hid{News-Headers} and @hid{News-Message} modes.)]
This command displays the entire header for the message currently being
read.  This is to undo the effects of @hid{Netnews Message Header Fields}
for the current message.
@enddefcom

@defcom[com "Netnews Next Line", stuff (bound to @bf[C-n] and @bf[Downarrow] in @hid{News-Headers} mode)]
@defhvar1[var "Netnews Last Header Style", val {:next-headers}]
This command moves the current point to the next line.  If you are on the
last visible message, and there are more in the current group, headers for
these messages will be inserted.  If you are on the last header and there
are no more messages in this group, then @hemlock will take some action
based on the value of @hid(Netnews Last Header Style).  If the value of
this variable is @f(:feep), @hemlock feeps you indicating there are no
more messages.  If the value is @f(:next-headers), @hemlock reads in the
headers for the next group in your @hid(Netnews Group File).  If the value
is @f(:next-article), @hemlock goes on to the next group and shows you
the first unread message.
@enddefcom
					 
@defcom[com "Netnews Previous Line", stuff (bound to @bf[C-p] and @bf[Uparrow] in @hid{News-Headers} mode)]
This command moves the current point to the previous line.  If you are on
the first visible header, and there are more previous messages, @hemlock
inserts the headers for these messages.
@enddefcom

@defcom[com "Netnews Headers Scroll Window Down", stuff (bound to @bf[C-v] in @hid{News-Headers} mode)]
@defcom1[com "Netnews Headers Scroll Window Up", stuff (bound to @bf[M-v] in @hid{News-Headers} mode)]
   These commands scroll the headers window up or down one screenfull.  If the
   end of the buffer is visible, @hemlock inserts the next batch of headers.
@enddefcom

@defcom[com "Netnews Next Article", stuff (bound to @bf[n] in @hid{News-Message} and @hid{News-Headers} modes)]
@defcom1[com "Netnews Previous Article", stuff (bound to @bf[p] in @hid{News-Message} and @hid{News-Headers} modes)]
   These commands insert the next or previous message into a message buffer.
@enddefcom

@defcom[com "Netnews Message Scroll Down", stuff (bound to @bf[space] in @hid{News-Message} mode)]
@defhvar1[var "Netnews Scroll Show Next Message", val {t}]
If the end of the current message is visible, @hemlock feeps the user if
the value of @hid(Netnews Scroll Show Next Message) is non-@nil, or it
inserts the next message into this message buffer if that variable is @nil.
If the end of the message is not visible, then @hemlock shows the next
screenfull of the current message.
@enddefcom

@defcom[com "Netnews Message Quit", stuff (bound to @bf[q] in @hid{News-Message} mode)]
   This command deletes the current message buffer and makes the associated
   @hid{News-Headers} buffer current.
@enddefcom
 
@defcom[com "Netnews Goto Headers Buffer", stuff (bound to @bf[H-h] in @hid{News-Message} mode)]
   This command, when invoked from a @hid(News-Message) buffer with an
   associated @hid(News-Headers) buffer, places the associated 
   @hid(News-Headers) buffer into the current window.
@enddefcom

@defcom[com "Netnews Message Keep Buffer", stuff (bound to @bf[k] in @hid{News-Message} mode)]
   By default, @hemlock uses one buffer to display all messages in a group,
   one at a time.  This command tells @hemlock to keep the current message
   buffer intact and start reading messages in another buffer.
@enddefcom

@defcom[com "Netnews Select Message Buffer", stuff (bound to @bf[H-m] in @hid{News-Headers} and @hid{Post} modes.)]
   In @hid{News-Headers} mode, this command selects the buffer
   containing the last message read.  In @hid{Post} mode, it selects the
   associated @hid{News-Message} buffer, if there is one.
@enddefcom

@defcom[com "Netnews Append to File", stuff (bound to @bf[a] in @hid{News-Headers} and @hid{News-Message} modes.)]
@defhvar1[var "Netnews Message File", val {"netnews-messages.txt"}]
This command prompts for a file which the current message will be appended
to.  The default file is the value of @hid(Netnews Message File) merged
with your home directory.
@enddefcom

@defcom[com "Netnews Headers File Message", stuff (bound to @bf[o] in @hid{News-Headers} mode)]
This command prompts for a mail folder and files the message under the
point into it.  If the folder does not exist, @hemlock will ask if it should
be created.
@enddefcom

@defcom[com "Netnews Message File Message", stuff (bound to @bf[o] in @hid{News-Message} mode)]
This command prompts for a mail folder and files the current message there.
If the folder does not exist, @hemlock will ask if it should be created.
@enddefcom

@defcom[com "Fetch All Headers", stuff (bound to @bf[f] in @hid{Netnews Headers} mode)]
   In a forward reading @hid(Netnews headers) buffer, this command inserts
   all headers after the last visible one into the headers buffer.  If
   @hemlock is reading this group backward, the system inserts all headers
   before the first visible one into the headers buffer.
@enddefcom

@defcom[com "Netnews Go to Next Group", stuff (bound to @bf[g] in @hid{News-Headers} and @hid{News-Message} modes.)]
This command goes to the next group in your @hid(Netnews Group File).
Before going on, it sets the group pointer in @hid(Netnews Database
Filename) to the last message you read.  With an argument, the command does
not modify the group pointer for the current group.
@enddefcom

@defcom[com "Netnews Quit Starting Here", stuff (bound to @bf[.] in @hid{News-Headers} and @hid{News-Message} modes)]
   This command goes to the next group in your @hid(Netnews Group File), 
   setting the netnews pointer for this group to the message before the one
   under the current point, so the next time you read this group, the message
   indicated by the point will appear first.
@enddefcom

@defcom[com "Netnews Group Punt Messages", stuff (bound to @bf[G] in @hid{News-Headers} mode)]
   This command goes on to the next bulletin board in your group
   file.  Without an argument, the system sets the pointer for the current
   group to the last message.  With an argument, @hemlock sets the
   pointer to the last visible message in the group.
@enddefcom

@defcom[com "Netnews Exit", stuff (bound to @bf[q] in @hid{News-Headers} mode)]
@defhvar1[var "Netnews Exit Confirm", val {t}]
   This command cleans up and deletes the @hid(News-Headers) buffer and
   all associated @hid(News-Message) buffers.  If the value of
   @hid(Netnews Exit Confirm) is @nil, then @hemlock will not prompt before
   exiting.
@enddefcom

@section[Replying to Messages]

The @hemlock Netnews interface also provides an easy way of replying to
messages through the @hemlock Mailer or via @hid{Post} mode.

@defcom[com "Netnews Reply to Sender"]
   When you invoke this command, @hemlock creates a @hid(Draft) buffer and
   tries to fill in the @i(to) and @i(subject) fields of the draft.  For
   the @i(to) field, @hemlock looks at the @i(reply-to) field of the
   message you are replying to, or failing that, the @i(from) field.  If
   the @i(subject) field does not start with @f(Re:), @hemlock inserts this
   string, signifying that this is a reply.
@enddefcom

@defcom[com "Netnews Reply to Sender in Other Window", stuff (bound to @bf[r] in @hid{News-Headers} and @hid{News-Message}.)]
This command splits the current window, placing the message you are
replying to in the top window and a new @hid{Draft} buffer in the bottom
one.  This command fills in the header fields in the same manner as
@hid(Netnews Reply to Sender).
@enddefcom

@defcom[com "Netnews Reply to Group"]
This command creates a @hid{Post} buffer with the @i(newsgroups) field set
to the current group and the @i(subject) field constructed in the same way
as in @hid(Netnews Reply to Sender).
@enddefcom

@defcom[com "Netnews Reply to Group in Other Window", stuff (bound to @bf[R] in @hid{News-Headers} and @hid{News-Message}.)]
   This command splits the current window, placing the message you are
   replying to in the top window and a new @hid{Post} buffer in the bottom
   one.  This command will fill in the header fields in the same manner as
   @hid(Netnews Reply to Group).
@enddefcom

@defcom[com "Netnews Post Message", stuff (bound to @bf[C-x P])]
   This command creates a @hid{Post} buffer.  If you are in a 
   @hid(News-Headers) or @hid{News-Message} buffer, @hemlock fills in the
   @i(newsgroups) field with the current group.
@enddefcom

@defcom[com "Netnews Forward Message", stuff (bound to @bf[f] in @hid{News-Headers} and @hid{News-Message} modes.)]
This command creates a @hid{Post} buffer.  If you are in a @hid{Netnews
Headers} or @hid{News-Message} buffer, @hemlock will put the text of the
current message into the buffer along with lines delimiting the forwarded
message.
@enddefcom

@defcom[com "Netnews Goto Post Buffer", stuff (bound to @bf[H-p] in @hid{News-Message} mode)]
   This command, when invoked in a @hid(News-Message) or @hid(Draft) buffer
   with an associated @hid(News-Headers) buffer, places the associated
   @hid(News-Headers) buffer into the current window.
@enddefcom

@defcom[com "Netnews Goto Draft Buffer", stuff (bound to @bf[H-d] in @hid{News-Message} mode)]
   This command, when invoked in a @hid(News-Message) buffer with an 
   associated @hid(Draft) buffer, places the @hid(Draft) buffer into the 
   current window.
@enddefcom

@section[Posting Messages]

@defcom[com "Netnews Deliver Post", stuff (bound to @bf[H-s] in @hid{Post} mode)]
@defhvar1[var "Netnews Deliver Post Confirm", val "t"]
This command delivers the contents of a @hid(Post) buffer to the NNTP
server.  If @hid(Netnews Deliver Post Confirm) is @f(t), @hemlock will ask for
confirmation before posting the message.  @hemlock feeps you if NNTP does
not accept the message.
@enddefcom

@defcom[com "Netnews Abort Post", stuff (bound to @bf[H-q] in @hid{Post} mode)]
   This command deletes the current @hid(Post) buffer.
@enddefcom


As in the mailer, when replying to a message you can be excerpt sections of
it using @hid(Insert Message Buffer) and @hid(Insert Message Region) in
@hid(Post) and @hid(News-Message) modes, respectively.  You can also use
these commands when replying to a message via mail in a @hid(Draft) buffer.
In all cases, the same binding is used: @bf[H-y].

@newpage
@section[Wallchart]

@tabclear
@tabdivide(5)

@begin[format, spacing 1.5]


@Begin[Center] @b[Global bindings:] @End[Center]

@hid[Netnews Post Message]@\@\@bf[C-x P]


@Begin[Center] @b[News-Headers and News-Message modes bindings:] @End[Center]

@hid[Netnews Next Article]@\@\@\@bf[n]
@hid[Netnews Previous Article]@\@\@bf[p]
@hid[Netnews Go to Next Group]@\@\@bf[g]
@hid[Netnews Group Punt Messages]@\@\@bf[G]
@hid[List All Groups]@\@\@\@bf[l]
@hid[Netnews Append to File]@\@\@bf[a]
@hid[Netnews Forward Message]@\@\@bf[f]
@hid[Netnews Reply to Sender in Other Window]@\@\@bf[r]
@hid[Netnews Reply to Group in Other Window]@\@\@bf[R]
@hid[Netnews Quit Starting Here]@\@\@bf[.]

@Begin[Center] @b[News-Headers mode bindings:] @End[Center]

@hid[Netnews Show Article]@\@\@bf[Space]
@hid[Netnews Previous Line]@\@\@bf[C-p], @bf[Uparrow]
@hid[Netnews Next Line]@\@\@\@bf[C-n], @bf[Downarrow]
@hid[Netnews Headers Scroll Window Down]@\@\@bf[C-v]
@hid[Netnews Headers Scroll Window Up]@\@\@bf[M-v]
@hid[Netnews Select Message Buffer]@\@\@bf[H-m]
@hid[Netnews Exit]@\@\@\@bf[q]
@hid[Netnews Headers File Message]@\@\@bf[o]


@Begin[Center] @b[News-Message mode bindings:] @End[Center]

@hid[Netnews Message Scroll Down]@\@\@bf[Space]
@hid[Scroll Window Up]@\@\@\@bf[Backspace]
@hid[Netnews Goto Headers Buffer]@\@\@bf[H-h], @bf[^]
@hid[Netnews Message Keep Buffer]@\@\@bf[k]
@hid[Netnews Message Quit]@\@\@bf[q]
@hid[Netnews Message File Message]@\@\@bf[o]
@hid[Netnews Goto Post Buffer]@\@\@bf[H-p]
@hid[Netnews Goto Draft Buffer]@\@\@bf[H-d]
@hid[Insert Message Region]@\@\@bf[H-y]


@Begin[Center] @b[Post mode bindings:] @End[Center]

@hid[Netnews Select Message Buffer]@\@\@bf[H-m]
@hid[Netnews Deliver Post]@\@\@bf[H-s]
@hid[Netnews Abort Post]@\@\@\@bf[H-q]
@hid[Insert Message Buffer]@\@\@bf[H-y]


@Begin[Center] @b[News-Browse mode bindings:] @End[Center]

@hid[Netnews Quit Browse]@\@\@bf[q]
@hid[Netnews Browse Add Group To File]@\@\@bf[a]
@hid[Netnews Browse Read Group]@\@\@bf[Space]
@hid[Next Line]@\@\@\@bf[n]
@hid[Previous Line]@\@\@\@bf[p]


@end[format]
@tabclear
