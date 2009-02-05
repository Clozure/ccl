@comment{-*- Dictionary: /afs/cs/project/clisp/scribe/hem/hem; Mode: spell; Package: Hemlock -*-}
@chap[The Mail Interface]
@section[Introduction to Mail in Hemlock]

@index[MH interface]@label[introduction]
@hemlock provides an electronic mail handling facility via an interface to the
public domain @i[Rand MH Message Handling System].  This chapter assumes that
the user is familiar with the basic features and operation of @mh, but it
attempts to make allowances for beginners.  Later sections of this chapter
discuss setting up @mh, profile components and special files for formatting
outgoing mail headers, and backing up protected mail directories on a
workstation.  For more information on @mh, see the @i[Rand MH Message Handling
System Tutorial] and the @i[Rand MH Message Handling System Manual].

The @hemlock interface to @mh provides a means for generating header (@f[scan])
lines for messages and displaying these headers in a @hid[Headers] buffer.
This allows the user to operate on the @i[current message] as indicated by the
position of the cursor in the @hid[Headers] buffer.  The user can read, reply
to, forward, refile, or perform various other operations on the current
message.  A user typically generates a @hid[Headers] buffer with the commands
@hid[Message Headers] or @hid[Incorporate and Read New Mail], and multiple such
buffers may exist simultaneously.

Reading a message places its text in a @hid[Message] buffer.  In a manner
similar to a @hid[Headers] buffer, this allows the user to operate on that
message.  Most @hid[Headers] buffer commands behave the same in a @hid[Message]
buffer.  For example, the @hid[Reply to Message] command has the same effect in
both @hid[Headers] mode and @hid[Message] mode.  It creates a @hid[Draft]
buffer and makes it the current buffer so that the user may type a reply to the
current message.

The @hid[Send Message] command originates outgoing mail.  It generates a
@hid[Draft] buffer in which the user composes a mail message.  Each @hid[Draft]
buffer has an associated pathname, so the user can save the buffer to a file as
necessary.  Invoking @hid[Send Message] in a @hid[Headers] or @hid[Message]
buffer associates the @hid[Draft] buffer with a @hid[Message] buffer.  This
allows the user to easily refer to the message being replied to with the
command @hid[Goto Message Buffer].  After the user composes a draft message, he
can deliver the message by invoking the @hid[Deliver Message] command in the
@hid[Draft] buffer (which deletes both the this buffer and any associated
@hid[Message] buffer), or he can delay this action.  Invoking @hid[Deliver
Message] when not in a @hid[Draft] buffer causes it to prompt for a draft
message ID, allowing previously composed and saved messages to be delivered
(even across distinct Lisp invocations).

@index[virtual message deletion]
The @hemlock mail system provides a mechanism for @i[virtual message deletion].
That is, the @hid[Delete Message] command does not immediately delete a message
but merely flags the message for future deletion.  This allows the user to
undelete the messages with the @hid[Undelete Message] command.  The
@hid[Expunge Messages] command actually removes messages flagged for deletion.
After expunging a deleted message, @hid[Undelete Messages] can no longer
retrieve it.  Commands that read messages by sequencing through a @hid[Headers]
buffer typically ignore those marked for deletion, which makes for more fluid
reading if a first pass has been made to delete uninteresting messages.

After handling messages in a @hid[Headers] buffer, there may be messages
flagged for deletion and possibly multiple @hid[Message] buffers lying around.
There is a variety of commands that help @i[terminate] a mail session.
@hid[Expunge Messages] will flush the messages to be deleted, leaving the
buffer in an updated state.  @hid[Delete Headers Buffer and Message Buffers]
will delete the @hid[Headers] buffer and its corresponding @hid[Message]
buffers.  @hid[Quit Headers] is a combination of these two commands in that it
first expunges messages and then deletes all the appropriate buffers.

One does not have to operate only on messages represented in a @hid[Headers]
buffer.  This is merely the nominal mode of interaction.  There are commands
that prompt for a folder, an @mh message specification (for example, "@f[1 3 6
last]", "@f[1-3 5 6]", "@f[all]", "@f[unseen]"), and possibly a @f[pick]
expression.  @f[Pick] expressions allow messages to be selected based on header
field pattern matching, body text searching, and date comparisons; these can be
specified using either a Unix shell-like/switch notation or a Lisp syntax,
according to one's preference.  See section @ref[scanning] for more details.

A @i[mail-drop] is a file where a Unix-based mail system stores all messages a
user receives.  The user's mail handling program then fetches these from the
mail-drop, allowing the user to operate on them.  Traditionally one locates his
mail-drop and mail directory on a mainframe machine because the information on
mainframes is backed up on magnetic tape at least once per day.  Since @hemlock
only runs under CMU @clisp on workstations, and one's mail directory is not
usually world writable, it is not possible to adhere to a standard arrangement.
Since @mh provides for a remote mail-drop, and CMU's Remote File System has a
feature allowing authentication across a local area network, one can use
@hemlock to fetch his mail from a mainframe mail-drop (where it is backed up
before @hemlock grabs it) and store it on his workstation.  Reading mail on a
workstation is often much faster and more comfortable because typically it is a
single user machine.  Section @ref[backing-up] describes how to back up one's
mail directory from a workstation to a mainframe.


@section[Constraints on MH to use Hemlock's Interface]

@index[constraints for mail interface]@label[constraints]
There are a couple constaints placed on the user of the @hemlock interface to
@mh.  The first is that there must be a draft folder specified in one's @mh
profile to use any command that sends mail.  Also, to read new mail, there must
be an @f[Unseen-Sequence:] component in one's @mh profile.  The default @mh
profile does not specify these components, so they must be added by the user.
The next section of this chapter describes how to add these components.
Another constraint is that @hemlock requires its own @f[scan] line format to
display headers lines in a @hid[Headers] buffer.  See the description of the
variable @hid[MH Scan Line Form] for details.


@section[Setting up MH]

@index[setting up the mail interface]@label[setting-up]
@index[mail profile]@index[MH profile]
Get an @mh default profile and mail directory by executing the @mh @f[folder]
utility in a Unix shell.  When it asks if it should make the "@f[inbox]"
folder, answer "@b[yes]".  This creates a file called "@f[.mh_profile]" in the
user's home directory and a directory named "@f[Mail]".

Edit the "@f[.mh_profile]" file inserting two additional lines.  To send mail
in @hemlock, the user must indicate a draft folder by adding a
@f[Draft-Folder:] line with a draft folder name @dash "@f[drafts]" is a common
name:
@begin[example]
Draft-Folder: drafts
@end[example]

Since the mail-drop exists on a remote machine, the following line must
also be added:
@begin[example]
MailDrop: /../<hostname>/usr/spool/mail/<username>
@end[example]

Since the user's mail-drop is on a separate machine from his mail directory
(and where the user runs @hemlock), it is necessary to issue the following
command from the Unix shell (on the workstation).  This only needs to be done
once.
@begin[programexample]
/usr/cs/etc/rfslink -host <hostname> /usr/spool/mail/<username>
@end[programexample]
Note that @b[<hostname>] is not a full ARPANET domain-style name.  Use an
abbreviated CMU host name (for example, "@b[spice]" not
"@b[spice.cs.cmu.edu]").


@section[Profile Components and Customized Files]

@subsection[Profile Components]

@label[Profile] 
The following are short descriptions about profile components that are either
necessary to using @hemlock@comment{}'s interface to @mh or convenient for using @mh in
general:

@begin[description]
@f[Path:]@\
This specifies the user's mail directory.  It can be either a full pathname or
a pathname relative to the user's home directory.  This component is
@i[necessary] for using @mh.

@f[MailDrop:]@\
This is used to specify one's remote mail-drop.  It is @i[necessary] for
@hemlock only when using a mail-drop other than "@f[/usr/spool/mail/<user>]" on
the local machine.

@f[Folder-Protect:], @f[Msg-Protect:]@\
These are set to 700 and 600 respectively to keep others from reading one's
mail.  At one time the default values were set for public visibility of mail
folders.  Though this is no longer true, these can be set for certainty.  The
700 protection allows only user read, write, and execute (list access for
directories), and 600 allows only user read and write.  These are not necessary
for either @mh or the @hemlock interface.

@f[Unseen-Sequence:]@\
When mail is incorporated, new messages are added to this sequence, and as
these messages are read they are removed from it.  This allows the user at any
time to invoke an @mh program on all the unseen messges of a folder easily.  An
example definition is:
@begin[example]
Unseen-Sequence: unseen
@end[example]
Specifying an unseen-sequence is @i[necessary] to use @hemlock@comment{}'s
interface to @mh.

@f[Alternate-Mailboxes:]@\
This is not necessary for either @mh or the @hemlock interface.  This
component tells @mh which addresses that it should recognize as the user.  This
is used for @f[scan] output formatting when the mail was sent by the user.  It
is also used by @f[repl] when it sets up headers to know who the user is for
inclusion or exclusion from @b[cc]: lists.  This is case sensitive and takes
wildcards.  One example is:
@begin[example]
Alternate-Mailboxes: *FRED*, *Fred*, *fred*
@end[example]

@f[Draft-Folder:]@\
This makes multiple draft creation possible and trivial to use.  Just supply a
folder name (for example, "@f[drafts]").  Specifying a draft-folder is
@i[necessary] to use @hemlock@comment{}'s interface to @mh.

@f[repl: -cc all -nocc me -fcc out-copy]@\
This tells the @f[repl] utility to include everyone but the user in the
@b[cc:] list when replying to mail.  It also makes @f[repl] keep an copy of the
message the user sends.  This is mentioned because one probably wants to reply
to everyone receiving a piece of mail except oneself.  Unlike other utilities
that send mail, @f[repl] stores personal copies of outgoing mail based on a
command line switch.  Other @mh utilities use different mechanisms.  This line
is not necessary to use either @mh or the @hemlock interface.

@f[rmmproc: /usr/cs/bin/rm]@\
This is not necessary to use @hemlock@comment{}'s interface to @mh, but due to
@hemlock@comment{}'s virtual message deletion feature, this causes messages to be deleted
from folder directories in a cleaner fashion when they actually get removed.
Note that setting this makes @f[rmm] more treacherous if used in the Unix
shell.
@end[description]
@;


@subsection[Components Files]
@index[components]
@label[components-files]
@i[Components] files are templates for outgoing mail header fields that specify
position and sometimes values for specified fields.  Example files are shown
for each one discussed here.  These should exist in the user's mail directory.

For originating mail there is a components file named "@f[components]", and it
is used by the @mh utility @f[comp].  An example follows:
@begin[example]
   To: 
   cc: 
   fcc: out-copy
   Subject: 
   --------
@end[example]
This example file differs from the default by including the @f[fcc:] line.
This causes @mh to keep a copy of the outgoing draft message.  Also, though it
isn't visible here, the @f[To:], @f[cc:], and @f[Subject:] lines have a space
at the end.

@index[forwarding components]
The "@f[forwcomps]" components file is a template for the header fields of any
forwarded message.  Though it may be different, our example is the same as the
previous one.  These are distinct files for @mh@comment{}'s purposes, and it is more
flexible since the user might not want to keep copies of forwarded messages.

@index[reply components]
The "@f[replcomps]" components file is a template for the header fields of any
draft message composed when replying to a message.  An example
follows:
@begin[example]
   %(lit)%(formataddr %<{reply-to}%|%<{from}%|%{sender}%>%>)\
   %<(nonnull)%(void(width))%(putaddr To: )\n%>\
   %(lit)%(formataddr{to})%(formataddr{cc})%(formataddr(me))\
   %(formataddr{resent-to})\
   %<(nonnull)%(void(width))%(putaddr cc: )\n%>\
   %<{fcc}Fcc: %{fcc}\n%>\
   %<{subject}Subject: Re: %{subject}\n%>\
   %<{date}In-reply-to: Your message of \
   %<(nodate{date})%{date}%|%(tws{date})%>.%<{message-id}
		%{message-id}%>\n%>\
   --------
@end[example]
This example file differs from the default by including the @b[resent-to:]
field (in addition to the @b[to:] and @b[cc:] fields) of the message being
replied to in the @b[cc:] field of the draft.  This is necessary for replying
to all recipients of a distributed message.  Keeping a copy of the outgoing
draft message works a little differently with reply components.  @mh expects a
switch which the user can put in his profile (see section @ref[Profile] of this
chapter), and using the @mh formatting language, this file tests for the
@f[fcc] value as does the standard file.


@section[Backing up the Mail Directory]
@index[backing up mail directories]
@label[backing-up]
The easiest method of backing up a protected mail directory is to copy it into
an Andrew File System (AFS) directory since these are backed up daily as with
mainframes.  The only problem with this is that the file servers may be down
when one wants to copy his mail directory since, at the time of this writing,
these servers are still under active development; however, they are becoming
more robust daily.  One can read about the current AFS status in the file
@f[/../fac/usr/gripe/doc/vice/status].

Using AFS, one could keep his actual mail directory (not a copy thereof) in his
AFS home directory which eliminates the issue of backing it up.  This is
additionally beneficial if the user does not use the same workstation everyday
(that is, he does not have his own but shares project owned machines).  Two
problems with this arrangement result from the AFS being a distributed file
system.  Besides the chance that the server will be down when the user wants to
read mail, performance degrades since messages must always be referenced across
the local area network.

Facilities' official mechanism for backing up protected directories is called
@f[sup].  This is awkward to use and hard to set up, but a subsection here
describes a particular arrangement suitable for the user's mail directory.


@subsection[Andrew File System]
If the user choses to use AFS, he should get copies of @i[Getting Started with
the Andrew File System] and @i[Protecting AFS files and directories].  To use
AFS, send mail to Gripe requesting an account.  When Gripe replies with a
password, change it to be the same as the account's password on the
workstation.  This causes the user to be authenticated into AFS when he logs
into his workstation (that is, he is automatically logged into his AFS
account).  To change the password, first log into the AFS account:
@begin[programexample]
log <AFS userid>
@end[programexample]
Then issue the @f[vpasswd] command.

All of the example command lines in this section assume the user has
@f[/usr/misc/bin] on his Unix shell @f[PATH] environment variable.

@paragraph[Copy into AFS:]

Make an AFS directory to copy into:
@begin[programexample]
mkdir /afs/cs.cmu.edu/user/<AFS userid>/mail-backup
@end[programexample]

This will be readable by everyone, so protect it with the following:
@begin[programexample]
fs sa /afs/cs.cmu.edu/user/<AFSuserid>/mail-backup System:AnyUser none
@end[programexample]

Once the AFS account and directory to backup into have been established, the
user needs a means to recursively copy his mail directory updating only those
file that have changed and deleting those that no longer exist.  To do this,
issue the following command:
@begin[programexample]
copy -2 -v -R <mail directory> <AFS backup directory>
@end[programexample]
Do not terminate either of these directory specifications with a @f[/].  The
@f[-v] switch causes @f[copy] to output a line for copy and deletion, so this
may be eliminated if the user desires.

@paragraph[Mail Directory Lives in AFS:]

Assuming the AFS account has been established, and the user has followed the
directions in @ref[setting-up], now make an AFS directory to serve as the mail
directory:
@begin[programexample]
mkdir /afs/cs.cmu.edu/user/<AFS userid>/Mail
@end[programexample]

This will be readable by everyone, so protect it with the following:
@begin[programexample]
fs sa /afs/cs.cmu.edu/user/<AFSuserid>/Mail System:AnyUser none
@end[programexample]

Tell @mh where the mail directory is by modifying the profile's
"@f[.mh_profile]" (see section @ref[setting-up]) @f[Path:] component (see
section @ref[Profile]):
@begin[programexample]
Path: /afs/cs.cmu.edu/user/<AFS userid>/Mail
@end[programexample]


@subsection[Sup to a Mainframe]
To use @f[sup] the user must set up a directory named "@f[sup]" on the
workstation in the user's home directory.  This contains different directories
for the various trees that will be backed up, so there will be a "@f[Mail]"
directory.  This directory will contain two files: "@f[crypt]" and "@f[list]".
The "@f[crypt]" file contains one line, terminated with a new line, that
contains a single word @dash an encryption key.  "@f[list]" contains one line,
terminated with a new line, that contains two words @dash "@b[upgrade Mail]".

On the user's mainframe, a file must be created that will be supplied to the
@f[sup] program.  It should contain the following line to backup the mail
directory:

@begin[example]
Mail delete host=<workstation> hostbase=/usr/<user> base=/usr/<user> \
crypt=WordInCryptFile login=<user> password=LoginPasswordOnWorkstation
@end[example]
Warning: @i[This file contains the user's password and should be
protected appropriately.] 

The following Unix shell command issued on the mainframe will backup the
mail directory:

@begin[programexample]
   sup <name of the sup file used in previous paragraph>
@end[programexample]

As a specific example, assume user "@f[fred]" has a workstation called
"@f[fred]", and his mainframe is the "@f[gpa]" machine where he has another
user account named "@f[fred]".  The password on his workstation is
"@f[purple]".  On his workstation, he creates the directory
"@f[/usr/fred/sup/Mail/]" with the two files "@f[crypt]" and "@f[list]".
The file "@f[/usr/fred/sup/Mail/crypt]" contains only the encryption key:
@programexample[steppenwolf]
The file "@f[/usr/fred/sup/Mail/list]" contains the command to upgrade the
"@f[Mail]" directory:
@programexample[upgrade Mail]

On the "@f[gpa]" machine, the file "@f[/usr/fred/supfile]" contains the
following line:
@begin[programexample]
Mail delete host=fred hostbase=/usr/fred base=/usr/fred \
crypt=steppenwolf login=fred password=purple
@end[programexample]
This file is protected on "@f[gpa]", so others cannot see @f[fred's] password
on his workstation.

On the gpa-vax, issuing
@begin[programexample]
   sup /usr/fred/supfile
@end[programexample]
to the Unix shell will update the @mh mail directory from @f[fred's]
workstation deleting any files that exist on the gpa that do not exist on the
workstation.

For a more complete description of the features of @f[sup], see the @i[UNIX
Workstation Owner's Guide] and @i[The SUP Software Upgrade Protocol].

@section[Introduction to Commands and Variables]

@index[mail commands]@index[mail variables]@label[mhcommands]
Unless otherwise specified, any command which prompts for a folder name will
offer the user a default.  Usually this is @mh@comment{}'s idea of the current folder,
but sometimes it is the folder name associated with the current buffer if there
is one.  When prompting for a message, any valid @mh message expression may be
entered (for example, "@f[1 3 6]", "@f[1-3 5 6]", "@f[unseen]", "@f[all]").
Unless otherwise specified, a default will be offered (usually the current
message).

Some commands mention specific @mh utilities, so the user knows how the
@hemlock command affects the state of @mh and what profile components and
special formatting files will be used.  @hemlock runs the @mh utility programs
from a directory indicated by the following variable:

@defhvar[var "MH Utility Pathname", val {"/usr/misc/.mh/bin/"}]
@mh utility names are merged with this pathname to find the executable
files. 
@enddefhvar


@section[Scanning and Picking Messages]
@label[scanning]
As pointed out in the introduction of this chapter, users typically generate
headers or @f[scan] listings of messages with @hid[Message Headers], using
commands that operate on the messages represented by the headers.  @hid[Pick
Headers] (bound to @bf[h] in @hid[Headers] mode) can be used to narrow down (or
further select over) the headers in the buffer.

A @f[pick] expression may be entered using either a Lisp syntax or a Unix
shell-like/switch notation as described in the @mh documentation.  The Lisp
syntax is as follows:

@begin[example]
   <exp>       ::=  {(not <exp>) | (and <exp>*) | (or <exp>*)
		    | (cc <pattern>) | (date <pattern>)
		    | (from <pattern>) | (search <pattern>)
		    | (subject <pattern>) | (to <pattern>)
		    | (-- <component> <pattern>)
		    | (before <date>) | (after <date>)
		    | (datefield <field>)}

   <pattern>   ::=  {<string> | <symbol>}

   <component> ::=  {<string> | <symbol>}

   <date>      ::=  {<string> | <symbol> | <number>}

   <field>     ::=  <string>
@end[example]

Anywhere the user enters a @f[<symbol>], its symbol name is used as a string.
Since @hemlock @f[read]s the expression without evaluating it, single quotes
("@bf[']") are unnecessary.  From the @mh documentation,

@begin[itemize]
   A @f[<pattern>] is a Unix @f[ed] regular expression.  When using a string to
   input these, remember that @f[\] is an escape character in Common Lisp.

   A @f[<component>] is a header field name (for example, @b[reply-to] or
   @b[resent-to]).

   A @f[<date>] is an @i[822]-style specification, a day of the week,
   "@b[today]", "@b[yesterday]", "@b[tomorrow]", or a number indicating @i[n]
   days ago.  The @i[822] standard is basically:
   @begin[example]
   dd mmm yy hh:mm:ss zzz
   @end[example]
   which is a two digit day, three letter month (first letter capitalized), two
   digit year, two digit hour (@f[00] through @f[23]), two digit minute, two
   digit second (this is optional), and a three letter zone (all capitalized).
   For
   example:
   @begin[example]
   21 Mar 88 16:00 EST
   @end[example]
   
   A @f[<field>] is an alternate @f[Date:] field to use with @f[(before
   <date>)] and @f[(after <date>)] such as @f[BB-Posted:] or
   @f[Delivery-Date:].

   Using @f[(before <date>)] and @f[(after <date>)] causes date field parsing,
   while @f[(date <pattern>)] does string pattern matching.
@end[itemize]

Since a @f[<pattern>] may be a symbol or string, it should be noted that the
symbol name is probably all uppercase characters, and @mh will match these
only against upper case.  @mh will match lowercase characters against lower
and upper case.  Some examples are:
@begin[example]
   ;;; All messages to Gripe.
   (to "gripe")

   ;;; All messages to Gripe or about Hemlock.
   (or (to "gripe") (subject "hemlock"))

   ;;; All messages to Gripe with "Hemlock" in the body.
   (and (to "gripe") (search "hemlock"))
@end[example]

Matching of @f[<component>] fields is case sensitive, so this example will
@f[pick] over all messages that have been replied to.
@example[(or (-- "replied" "") (-- "Replied" ""))]


@defhvar[var "MH Scan Line Form", val {"library:mh-scan"}]
This is a pathname of a file containing an @mh format expression used for
header lines.

The header line format must display the message ID as the first non-whitespace
item.  If the user uses the virtual message deletion feature which is on by
default, there must be a space three characters to the right of the message ID.
This location is used on header lines to note that a message is flagged for
deletion.  The second space after the message ID is used for notating answered
or replied-to messages.
@enddefhvar

@defcom[com "Message Headers", bind (C-x r)]
This command prompts for a folder, message (defaulting to "@b[all]"), and an
optional @f[pick] expression.  Typically this will simply be used to generate
headers for an entire folder or sequence, and the @f[pick] expression will not
be used.  A new @hid[Headers] buffer is made, and the output of @f[scan] on the
messages indicated is inserted into the buffer.  The current window is used,
the buffer's point is moved to the first header, and the @hid[Headers] buffer
becomes current.  The current value of the @hemlock @hid[Fill Column] variable
is supplied to @f[scan] as the @f[-width] switch.  The buffer name is set to a
string of the form @w<"@f[Headers <folder> <msgs> <pick expression>]">, so the
modeline will show what is in the buffer.  If no @f[pick] expression was
supplied, none will be shown in the buffer's name.  As described in the
introduction to this section, the expression may be entered using either a Lisp
syntax or a Unix shell-like/switch notation.
@enddefcom

@defhvar[var "MH Lisp Expression", val {t}]
When this is set, @mh expression prompts are read in a Lisp syntax.  Otherwise,
the input is of the form of a Unix shell-like/switch notation as described in
the @mh documentation.
@enddefhvar

@defcom[com "Pick Headers", stuff (bound to @bf[h] in @hid[Headers] mode) ]
This command is only valid in a @hid[Headers] buffer.  It prompts for a
@f[pick] expression, and the messages shown in the buffer are supplied to
@f[pick] with the expression.  The resulting messages are @f[scan]'ed, deleting
the previous contents of the buffer.  The current value of @hid[Fill Column] is
used for the @f[scan]'ing.  The buffer's point is moved to the first header.
The buffer's name is set to a string of the form @w<"@f[Headers <folder> <msgs
picked over> <pick expression>]">, so the modeline will show what is in the
buffer.  As described in the introduction to this section, the expression may
be entered using either a Lisp syntax or a Unix shell-like/switch notation.
@enddefcom

@defcom[com "Headers Help", bind (Headers: ?)]
This command displays documentation on @hid[Headers] mode.
@enddefcom


@section[Reading New Mail]

@index[reading new mail]@label[reading-new-mail]

@defcom[com "Incorporate and Read New Mail", stuff (bound to @bf[C-x i] globally and @bf[i] in @hid[Headers] and @hid[Message] modes) ]
This command incorporates new mail into @hid[New Mail Folder] and creates a
@hid[Headers] buffer with the new messages.  An unseen-sequence must be define
in the user's @mh profile to use this.  Any headers generated due to
@hid[Unseen Headers Message Spec] are inserted as well.  The buffer's point is
positioned on the headers line representing the first unseen message of the
newly incorporated mail.
@enddefcom

@defcom[com "Incorporate New Mail" ]
This command incorporates new mail into @hid[New Mail Folder], displaying
@f[inc] output in a pop-up window.  This is similar to @hid[Incorporate and
Read New Mail] except that no @hid[Headers] buffer is generated.
@enddefcom

@defhvar[var "New Mail Folder", val {"+inbox"}]
This is the folder into which @mh incorporates new mail.
@enddefhvar

@defhvar[var "Unseen Headers Message Spec", val {nil}]
This is an @mh message specification that is suitable for any message prompt.
When incorporating new mail and after expunging messages, @hemlock uses this
specification in addition to the unseen-sequence name that is taken from the
user's @mh profile to generate headers for the unseen @hid[Headers] buffer.
This value is a string.
@enddefhvar

@defhvar[var "Incorporate New Mail Hook", val {nil}]
This is a list of functions which are invoked immediately after new mail is
incorporated.  The functions should take no arguments.
@enddefhvar

@defhvar[var "Store Password", val {nil}]
When this is set, the user is only prompted once for his password, and the
password is stored for future use.
@enddefhvar

@defhvar[var "Authenticate Incorporation", val {nil}]
@defhvar1[var "Authentication User Name", val {nil}]
When @hid[Authenticate Incorporation] is set, incorporating new mail prompts
for a password to access a remote mail-drop.

When incorporating new mail accesses a remote mail-drop, @hid[Authentication
User Name] is the user name supplied for authentication on the remote machine.
If this is @nil, @hemlock uses the local name.
@enddefhvar


@section[Reading Messages]
@index[reading messages]
@label[reading-messages]
This section describes basic commands that show the current, next, and previous
messages, as well as a couple advanced commands.  @hid[Show Message] (bound to
@bf[SPACE] in @hid[Headers] mode) will display the message represented by the
@f[scan] line the @hemlock cursor is on.  Deleted messages are considered
special, and the more conveniently bound commands for viewing the next and
previous messages (@hid[Next Undeleted Message] bound to @bf[n] and
@hid[Previous Undeleted Message] bound to @bf[p], both in @hid[Headers] and
@hid[Message] modes) will ignore them.  @hid[Next Message] and @hid[Previous
Message] (bound to @bf[M-n] and @bf[M-p] in @hid[Headers] and @hid[Message]
modes) may be invoked if reading a message is desired regardless of whether it
has been deleted.


@defcom[com "Show Message", stuff (bound to @bf[SPACE] and @bf[.] in @hid[Headers] mode) ]
 This command, when invoked in a @hid[Headers] buffer, displays the current
message (the message the cursor is on), by replacing any previous message that
has not been preserved with @hid[Keep Message].  The current message is also
removed from the unseen sequence.  The @hid[Message] buffer becomes the current
buffer using the current window.  The buffer's point will be moved to the
beginning of the buffer, and the buffer's name will be set to a string of the
form @w<"@f[Message <folder> <msg-id>]">.

The @hid[Message] buffer is read-only and may not be modified.  The command
@hid[Goto Headers Buffer] issued in the @hid[Message] buffer makes the
associated @hid[Headers] buffer current.

When not in a @hid[Headers] buffer, this command prompts for a folder and
message.  A unique @hid[Message] buffer is obtained, and its name is set to a
string of the form @w<"@f[Message <folder> <msg-id>]">.  The buffer's point is
moved to the beginning of the buffer, and the current window is used to display
the message.

Specifying multiple messages inserts all the messages into the same buffer.  If
the user wishes to show more than one message, it is expected that he will
generate a @hid[headers] buffer with the intended messages, and then use the
message sequencing commands described below.
@enddefcom

@defcom[com "Next Message", stuff (bound to @bf[M-n] in @hid[Headers] and @hid[Message] modes) ]
 This command is only meaningful in a @hid[Headers] buffer or a @hid[Message]
buffer associated with a @hid[Headers] buffer.  In a @hid[Headers] buffer, the
point is moved to the next message, and if there is one, it is shown as
described in the @hid[Show Message] command.

In a @hid[Message] buffer, the message after the currently visible message is
displayed.  This clobbers the buffer's contents.  Note, if the @hid[Message]
buffer is associated with a @hid[Draft] buffer, invoking this command breaks
that association.  Using @hid[Keep Message] preserves the @hid[Message] buffer
and any association with a @hid[Draft] buffer.

The @hid[Message] buffer's name is set as described in the @hid[Show Message]
command.
@enddefcom

@defcom[com "Previous Message", stuff (bound to @bf[M-p] in @hid[Headers] and @hid[Message] modes) ]
 This command is only meaningful in a @hid[Headers] buffer or a @hid[Message]
buffer associated with a @hid[Headers] buffer.  In a @hid[Headers] buffer, the
point is moved to the previous message, and if there is one, it is shown as
described in the @hid[Show Message] command.

In a @hid[Message] buffer, the message before the currently visible message is
displayed.  This clobbers the buffer's contents.  Note, if the @hid[Message]
buffer is associated with a @hid[Draft] buffer, invoking this command breaks
that association.  Using @hid[Keep Message] preserves the @hid[Message] buffer
and any association with a @hid[Draft] buffer.

The @hid[Message] buffer's name is set as described in the @hid[Show Message]
command.
@enddefcom

@defcom[com "Next Undeleted Message", stuff (bound to @bf[n] in @hid[Headers] and @hid[Message] modes) ]
 This command is only meaningful in a @hid[Headers] buffer or a @hid[Message]
buffer associated with a @hid[Headers] buffer.  In a @hid[Headers] buffer, the
point is moved to the next undeleted message, and if there is one, it is shown
as described in the @hid[Show Message] command.

In a @hid[Message] buffer, the first undeleted message after the currently
visible message is displayed.  This clobbers the buffer's contents.  Note, if
the @hid[Message] buffer is associated with a @hid[Draft] buffer, invoking this
command breaks that association.  The @hid[Keep Message] command preserves the
@hid[Message] buffer and any association with a @hid[Draft] buffer.

The @hid[Message] buffer's name is set as described in the @hid[Show Message]
command.
@enddefcom

@defcom[com "Previous Undeleted Message", stuff (bound to @bf[p] in @hid[Headers] and @hid[Message] modes) ]
 This command is only meaningful in a @hid[Headers] buffer or a @hid[Message]
buffer associated with a @hid[Headers] buffer.  In a @hid[Headers] buffer, the
point is moved to the previous undeleted message, and if there is one, it is
shown as described in the @hid[Show Message] command.

In a @hid[Message] buffer, the first undeleted message before the currently
visible message is displayed.  This clobbers the buffer's contents.  Note, if
the @hid[Message] buffer is associated with a @hid[Draft] buffer, invoking this
command breaks that association.  The @hid[Keep Message] command preserves the
@hid[Message] buffer and any association with a @hid[Draft] buffer.

The @hid[Message] buffer's name is set as described in the @hid[Show Message]
command.
@enddefcom

@defcom[com "Scroll Message", stuff (bound to @bf[SPACE] and @bf[C-v] in @hid[Message] mode) ]
@defhvar1[var "Scroll Message Showing Next", val {t}]
 This command scrolls the current window down through the current message.  If
the end of the message is visible and @hid[Scroll Message Showing Next] is not
@nil, then show the next undeleted message.
@enddefcom

@defcom[com "Keep Message" ]
This command can only be invoked in a @hid[Message] buffer.  It causes the
@hid[Message] buffer to continue to exist when the user invokes commands to
view other messages either within the kept @hid[Message] buffer or its
associated @hid[Headers] buffer.  This is useful for getting two messages into
different buffers.  It is also useful for retaining @hid[Message] buffers which
would otherwise be deleted when an associated draft message is delivered.
@enddefcom

@defcom[com "Message Help", bind (Message: ?)]
This command displays documentation on @hid[Message] mode.
@enddefcom


@section[Sending Messages]
@index[sending messages]
@label[sending-messages]
The most useful commands for sending mail are @hid[Send Message] (bound to
@bf[m] and @bf[s] in @hid[Headers] and @hid[Message] modes), @hid[Reply to
Message] (bound to @bf[r] in @hid[Headers] mode), and @hid[Reply to Message in
Other Window] (bound to @bf[r] in @hid[Message] mode).  These commands set up a
@hid[Draft] buffer and associate a @hid[Message] buffer with the draft when
possible.  To actually deliver the message to its recipient(s), use
@hid[Deliver Message] (bound to @bf[H-s] in @hid[Draft] mode).  To abort
sending mail, use @hid[Delete Draft and Buffer] (bound to @bf[H-q] in
@hid[Draft] mode).  If one wants to temporarily stop composing a draft with the
intention of finishing it later, then the @hid[Save File] command (bound to
@bf[C-x C-s]) will save the draft to the user's draft folder.

@hid[Draft] buffers have a special @hemlock minor mode called @hid[Draft] mode.
The major mode of a @hid[Draft] buffer is taken from the @hid[Default Modes]
variable.  The user may wish to arrange that @hid[Text] mode (and possibly
@hid[Fill] mode or @hid[Save] mode) be turned on whenever @hid[Draft] mode is
set.  For a further description of how to manipulate modes in @hemlock see the
@i[Hemlock Command Implementor's Manual].


@defcom[com "Send Message", stuff (bound to @bf[s] and @bf[m] in @hid[Headers] and @hid[Message] modes and @bf[C-x m] globally) ]
 This command, when invoked in a @hid[Headers] buffer, creates a unique
@hid[Draft] buffer and a unique @hid[Message] buffer.  The current message is
inserted in the @hid[Message] buffer, and the @hid[Draft] buffer is displayed
in the current window.  The @hid[Draft] buffer's point is moved to the end of
the line containing @f[To:] if it exists.  The name of the draft message file
is used to produce the buffer's name.  A pathname is associated with the
@hid[Draft] buffer so that @hid[Save File] can be used to incrementally save a
composition before delivering it.  The @f[comp] utility will be used to
allocate a draft message in the user's @mh draft folder and to insert the
proper header components into the draft message.  Both the @hid[Draft] and
@hid[Message] buffers are associated with the @hid[Headers] buffer, and the
@hid[Draft] buffer is associated with the @hid[Message] buffer.

When invoked in a @hid[Message] buffer, a unique @hid[Draft] buffer is created,
and these two buffers are associated.  If the @hid[Message] buffer is
associated with a @hid[Headers] buffer, this association is propagated to the
@hid[Draft] buffer.  Showing other messages while in this @hid[Headers] buffer
will not affect this @hid[Message] buffer.

When not in a @hid[Headers] or @hid[Message] buffer, this command does the same
thing as described in the previous two cases, but there are no @hid[Message] or
@hid[Headers] buffer manipulations.

@hid[Deliver Message] will deliver the draft to its intended recipient(s).

The @hid[Goto Headers Buffer] command, when invoked in a @hid[Draft] or
@hid[Message] buffer, makes the associated @hid[Headers] buffer current.  The
@hid[Goto Message Buffer] command, when invoked in a @hid[Draft] buffer, makes
the associated @hid[Message] buffer current.
@enddefcom

@defcom[com "Reply to Message", stuff (bound to @bf[r] in @hid[Headers] mode) ]
@defcom1[com "Reply to Message in Other Window", stuff (bound to @bf[r] in @hid[Message] mode) ]
@defhvar1[var "Reply to Message Prefix Action"]
 @hid[Reply to Message], when invoked in a @hid[Headers] buffer, creates a
unique @hid[Draft] buffer and a unique @hid[Message] buffer.  The current
message is inserted in the @hid[Message] buffer, and the @hid[Draft] buffer is
displayed in the current window.  The draft components are set up in reply to
the message, and the @hid[Draft] buffer's point is moved to the end of the
buffer.  The name of the draft message file is used to produce the buffer's
name.  A pathname is associated with the @hid[Draft] buffer so that @hid[Save
File] can be used to incrementally save a composition before delivering it.
The @f[repl] utility will be used to allocate a draft message file in the
user's @mh draft folder and to insert the proper header components into the
draft message.  Both the @hid[Draft] and @hid[Message] buffers are associated
with the @hid[Headers] buffer, and the @hid[Draft] buffer is associated with
the @hid[Message] buffer.

When invoked in a @hid[Message] buffer, a unique @hid[Draft] buffer is set up
using the message in the buffer as the associated message.  Any previous
association between the @hid[Message] buffer and a @hid[Draft] buffer is
removed.  Any association of the @hid[Message] buffer with a @hid[Headers]
buffer is propagated to the @hid[Draft] buffer.

When not in a @hid[Headers] buffer or @hid[Message] buffer, this command
prompts for a folder and message to reply to.  This message is inserted into a
unique @hid[Message] buffer, and a unique @hid[Draft] buffer is created as in
the previous two cases.  There is no association of either the @hid[Message]
buffer or the @hid[Draft] buffer with a @hid[Headers] buffer.

When a prefix argument is supplied, @hid[Reply to Message Prefix Action] is
considered with respect to supplying carbon copy switches to @f[repl].  This
variable's value is one of @b[:cc-all], :@b[no-cc-all], or @nil.  See section
@ref[Styles] for examples of how to use this.

@hid[Reply to Message in Other Window] is identical to @hid[Reply to Message],
but the current window is split showing the @hid[Draft] buffer in the new
window.  The split window displays the @hid[Message] buffer.

@hid[Deliver Message] will deliver the draft to its intended recipient(s).

The @hid[Goto Headers Buffer] commmand, when invoked in a @hid[Draft] or
@hid[Message] buffer, makes the associated @hid[Headers] buffer current.  The
@hid[Goto Message Buffer] command, when invoked in a @hid[Draft] buffer, makes
the associated @hid[Message] buffer current.
@enddefcom

@defcom[com "Forward Message", stuff (bound to @bf[f] in @hid[Headers] and @hid[Message] modes) ]
 This command, when invoked in a @hid[Headers] buffer, creates a unique
@hid[Draft] buffer.  The current message is inserted in the draft by using the
@f[forw] utility, and the @hid[Draft] buffer is shown in the current window.
The name of the draft message file is used to produce the buffer's name.  A
pathname is associated with the @hid[Draft] buffer so that @hid[Save File] can
be used to incrementally save a composition before delivering it.  The
@hid[Draft] buffer is associated with the @hid[Headers] buffer, but no
@hid[Message] buffer is created since the message is already a part of the
draft.

When invoked in a @hid[Message] buffer, a unique @hid[Draft] buffer is set up
inserting the message into the @hid[Draft] buffer.  The @hid[Message] buffer is
not associated with the @hid[Draft] buffer because the message is already a
part of the draft.  However, any association of the @hid[Message] buffer with a
@hid[Headers] buffer is propagated to the @hid[Draft] buffer.

When not in a @hid[Headers] buffer or @hid[Message] buffer, this command
prompts for a folder and message to forward.  A @hid[Draft] buffer is created
as described in the previous two cases.

@hid[Deliver Message] will deliver the draft to its intended recipient(s).
@enddefcom

@defcom[com "Deliver Message", stuff (bound to @bf[H-s] and @bf[H-c] in @hid[Draft] mode) ]
@defhvar1[var "Deliver Message Confirm", val {nil}]
 This command, when invoked in a @hid[Draft] buffer, saves the file and uses
the @mh @f[send] utility to deliver the draft.  If the draft is a reply to some
message, then @f[anno] is used to annotate that message with a "@f[replied]"
component.  Any @hid[Headers] buffers containing the replied-to message are
updated with an "@b[A]" placed in the appropriate headers line two characters
after the message ID.  Before doing any of this, confirmation is asked for
based on @hid[Deliver Message Confirm].

When not in a @hid[Draft] buffer, this prompts for a draft message ID and
invokes @f[send] on that draft message to deliver it.  Sending a draft in this
way severs any association that draft may have had with a message being replied
to, so no annotation will occur.
@enddefcom

@defcom[com "Delete Draft and Buffer", stuff (bound to @bf[H-q] in @hid[Draft] mode) ]
This command, when invoked in a @hid[Draft] buffer, deletes the draft message
file and the buffer.  This also deletes any associated message buffer unless
the user preserved it with @hid[Keep Message].
@enddefcom

@defcom[com "Remail Message", stuff (bound to @bf[H-r] in @hid[Headers] and @hid[Message] modes) ]
 This command, when invoked in a @hid[Headers] or @hid[Message] buffer, prompts
for resend @f[To:] and resend @f[Cc:] addresses, remailing the current message.
When invoked in any other kind of buffer, this command prompts for a folder and
message as well.  @mh@comment{}'s @f[dist] sets up a draft folder message which is then
modified.  The above mentioned addresses are inserted on the @f[Resent-To:] and
@f[Resent-Cc:] lines.  Then the message is delivered.

There is no mechanism for annotating messages as having been remailed.
@enddefcom

@defcom[com "Draft Help", bind (Draft: H-?)]
This command displays documentation on @hid[Draft] mode.
@enddefcom


@section[Convenience Commands for Message and Draft Buffers]
@index[message buffer commands]
@index[draft buffer commands]
@index[convenience commands for mail interface]
@label[convenience-coms] 
This section describes how to switch from a @hid[Message] or @hid[Draft] buffer
to its associated @hid[Headers] buffer, or from a @hid[Draft] buffer to its
associated @hid[Message] buffer.  There are also commands for various styles of
inserting text from a @hid[Message] buffer into a @hid[Draft] buffer.

@defcom[com "Goto Headers Buffer", stuff (bound to @bf[^] in @hid[Message] mode and @bf[H-^] in @hid[Draft] mode) ] 
This command, when invoked in a @hid[Message] or @hid[Draft] buffer with an
associated @hid[Headers] buffer, places the associated @hid[Headers] buffer in
the current window.

The cursor is moved to the headers line of the associated message.
@enddefcom

@defcom[com "Goto Message Buffer", stuff (bound to @bf[H-m] in @hid[Draft] mode) ]
This command, when invoked in a @hid[Draft] buffer with an associated
@hid[Message] buffer, places the associated @hid[Message] buffer in the current
window.
@enddefcom

@defcom[com "Insert Message Region", stuff (bound to @bf[H-y] in appropriate modes) ]
@defhvar1[var "Message Insertion Prefix", val {"   "}]
@defhvar1[var "Message Insertion Column", val {75}]
This command, when invoked in a @hid[Message] or @hid[News-Message] (where it
is bound) buffer that has an associated @hid[Draft] or @hid[Post] buffer,
copies the current active region into the @hid[Draft] or @hid[Post] buffer.  It
is filled using @hid[Message Insertion Prefix] (which defaults to three spaces)
and @hid[Message Insertion Column].  If an argument is supplied, the filling is
inhibited.
@enddefcom

@defcom[com "Insert Message Buffer", stuff (bound to @bf[H-y] in appropriate modes) ]
@defhvar1[var "Message Buffer Insertion Prefix", val {"    "}]
This command, when invoked in a @hid[Draft] or @hid[Post] (where it is bound)
buffer with an associated @hid[Message] or @hid[News-Message] buffer, or when
in a @hid[Message] (or @hid[News-Message]) buffer that has an associated
@hid[Draft] buffer, inserts the @hid[Message] buffer into the @hid[Draft] (or
@hid[Post]) buffer.  Each inserted line is modified by prefixing it with
@hid[Message Buffer Insertion Prefix] (which defaults to four spaces) .  If an
argument is supplied, the prefixing is inhibited.
@enddefcom

@defcom[com "Edit Message Buffer", stuff (bound to @bf[e] in @hid[Message] mode) ]
This command puts the current @hid[Message] buffer in @hid[Text] mode and makes
it writable (@hid[Message] buffers are normally read-only).  The pathname of
the file which the message is in is associated with the buffer making saving
possible.  A recursive edit is entered, and the user is allowed to make changes
to the message.  When the recursive edit is exited, if the buffer is modified,
the user is asked if the changes should be saved.  The buffer is marked
unmodified, and the pathname is disassociated from the buffer.  The buffer
otherwise returns to its previous state as a @hid[Message] buffer.  If the
recursive edit is aborted, the user is not asked to save the file, and the
buffer remains changed though it is marked unmodified.
@enddefcom


@section[Deleting Messages]
@index[deleting messages]
@label[deleting]
The main command described in this section is @hid[Headers Delete Message]
(bound to @bf[k] in @hid[Headers] and @hid[Message] modes).  A useful command
for reading new mail is @hid[Delete Message and Show Next] (bound to @bf[d] in
@hid[Message] mode) which deletes the current message and shows the next
undeleted message.

Since messages are by default deleted using a virtual message deletion
mechanism, @hid[Expunge Messages] (bound to @bf[!] in @hid[Headers] mode)
should be mentioned here.  This is described in section @ref[terminating].


@defhvar[var "Virtual Message Deletion", val {t}]
When set, @hid[Delete Message] adds a message to the "@f[hemlockdeleted]"
sequence; otherwise, @f[rmm] is invoked on the message immediately.
@enddefhvar

@defcom[com "Delete Message" ]
This command prompts for a folder, messages, and an optional @f[pick]
expression.  When invoked in a @hid[Headers] buffer of the specified folder,
the prompt for a message specification will default to the those messages in
that @hid[Headers] buffer.

When the variable @hid[Virtual Message Deletion] is set, this command merely
flags the messages for deletion by adding them to the "@f[hemlockdeleted]"
sequence.  Then this updates any @hid[Headers] buffers representing the folder.
It notates each headers line referring to a deleted message with a "@b[D]" in
the third character position after the message ID.

When @hid[Virtual Message Deletion] is not set, @f[rmm] is invoked on the
message, and each headers line referring to the deleted message is deleted from
its buffer
@enddefcom

@defcom[com "Headers Delete Message", stuff (bound to @bf[k] in @hid[Headers] and @hid[Message] modes) ]
This command, when invoked in a @hid[Headers] buffer, deletes the message on
the current line as described in @hid[Delete Message].

When invoked in a @hid[Message] buffer, the message displayed in it is deleted
as described in @hid[Delete Message].
@enddefcom

@defcom[com "Delete Message and Show Next", stuff (bound to @bf[k] in @hid[Headers] and @hid[Message] modes) ]
This command is only valid in a @hid[Headers] buffer or a @hid[Message] buffer
associated with some @hid[Headers] buffer.  The current message is deleted as
with the @hid[Delete Message] command.  Then the next message is shown as with
@hid[Next Undeleted Message].
@enddefcom

@defcom[com "Delete Message and Down Line", stuff (bound to @bf[d] in @hid[Headers mode])]
This command, when invoked in a @hid[Headers] buffer, deletes the message on
the current line.  Then the point is moved to the next non-blank line.
@enddefcom

@defcom[com "Undelete Message" ]
This command is only meaningful when @hid[Virtual Message Deletion] is set.
This prompts for a folder, messages, and an optional @f[pick] expression.  When
in a @hid[Headers] buffer of the specified folder, the messages prompt defaults
to those messages in the buffer.  All @hid[Headers] buffers representing the
folder are updated.  Each headers line referring to an undeleted message is
notated by replacing the "@b[D]" in the third character position after the
message ID with a space.
@enddefcom

@defcom[com "Headers Undelete Message", stuff (bound to @bf[u] in @hid[Headers] and @hid[Message] modes) ]
This command is only meaningful when @hid[Virtual Message Deletion] is set.
When invoked in a @hid[Headers] buffer, the message on the current line is
undeleted as described in @hid[Undelete Message].

When invoked in a @hid[Message] buffer, the message displayed in it is
undeleted as described in @hid[Undelete Message].
@enddefcom


@section[Folder Operations]

@index[folder operations]@label[folder]
@defcom[com "List Folders" ]
This command displays a list of all current mail folders in the user's
top-level mail directory in a @hemlock pop-up window.
@enddefcom

@defcom[com "Create Folder"]
This command prompts for and creates a folder.  If the folder already exists,
an error is signaled.
@enddefcom

@defcom[com "Delete Folder" ]
This command prompts for a folder and uses @f[rmf] to delete it.  Note that no
confirmation is asked for.
@enddefcom


@section[Refiling Messages]

@index[refiling messages]@label[refiling]
@defcom[com "Refile Message" ]
This command prompts for a folder, messages, an optional @f[pick] expression,
and a destination folder.  When invoked in a @hid[Headers] buffer of the
specified folder, the message prompt offers a default of those messages in the
buffer.  If the destination folder does not exist, the user is asked to create
it.  The resulting messages are refiled with the @f[refile] utility.  All
@hid[Headers] buffers for the folder are updated.  Each line referring to a
refiled message is deleted from its buffer.
@enddefcom

@defcom[com "Headers Refile Message", stuff (bound to @bf[o] in @hid[Headers] and @hid[Message] modes) ]
This command, when invoked in a @hid[Headers] buffer, prompts for a destination
folder, refiling the message on the current line with @f[refile].  If the
destination folder does not exist, the user is asked to create it.  Any
@hid[Headers] buffers containing messages for that folder are updated.  Each
headers line referring to the refiled message is deleted from its buffer.

When invoked in a @hid[Message] buffer, that message is refiled as described
above.
@enddefcom


@section[Marking Messages]
@index[marking messages]
@label[marking]
@defcom[com "Mark Message" ]
This command prompts for a folder, message, and sequence and adds (deletes) the
message specification to (from) the sequence.  By default this adds the
message, but if an argument is supplied, this deletes the message.  When
invoked in a @hid[Headers] buffer or @hid[Message] buffer, this only prompts
for a sequence and uses the current message.
@enddefcom


@section[Terminating Headers Buffers]
@label[terminating]
The user never actually @i[exits] the mailer.  He can leave mail buffers lying
around while conducting other editing tasks, selecting them and continuing his
mail handling whenever.  There still is a need for various methods of
terminating or cleaning up @hid[Headers] buffers.  The two most useful commands
in this section are @hid[Expunge Messages] and @hid[Quit Headers].


@defhvar[var "Expunge Messages Confirm", val {t}]
When this is set, @hid[Quit Headers] and @hid[Expunge Messages] will ask for
confirmation before expunging messages and packing the folder's message ID's.
@enddefhvar

@defhvar[var "Temporary Draft Folder", val {nil}]
This is a folder name where @mh @f[fcc:] messages are kept with the intention
that this folder's messages will be deleted and expunged whenever messages from
any folder are expunged (for example, when @hid[Expunge Messages] or @hid[Quit
Headers] is invoked.
@enddefhvar

@defcom[com "Expunge Messages", stuff (bound to @bf[!] in @hid[Headers] mode) ]
This command deletes messages @f[mark]'ed for deletion, and compacts the
folder's message ID's.  If there are messages to expunge, ask the user for
confirmation, displaying the folder name.  This can be inhibited by setting
@hid[Expunge Messages Confirm] to @nil.  When @hid[Temporary Draft Folder] is
not @nil, this command deletes and expunges that folder's messages regardless
of the folder in which the user invokes it, and a negative response to the
request for confirmation inhibits this.

When invoked in a @hid[Headers] buffer, the messages in that folder's
"@f[hemlockdeleted]" sequence are deleted by invoking @f[rmm].  Then the ID's
of the folder's remaining messages are compacted using the @f[folder] utility.
Since headers must be regenerated due to renumbering or reassigning message
ID's, and because @hid[Headers] buffers become inconsistent after messages are
deleted, @hemlock must regenerate all the headers for the folder.  Multiple
@hid[Headers] buffers for the same folder are then collapsed into one buffer,
deleting unnecessary duplicates.  Any @hid[Message] buffers associated with
these @hid[Headers] buffers are deleted.

If there is an unseen @hid[Headers] buffer for the folder, it is handled
separately from the @hid[Headers] buffers described above.  @hemlock tries to
update it by filling it only with remaining unseen message headers.
Additionally, any headers generated due to @hid[Unseen Headers Message Spec]
are inserted.  If there are no headers, unseen or otherwise, the buffer is left
blank.

Any @hid[Draft] buffer set up as a reply to a message in the folder is affected
as well since the associated message has possibly been deleted.  When a draft
of this type is delivered, no message will be annotated as having been replied
to.

When invoked in a @hid[Message] buffer, this uses its corresponding folder as
the folder argument.  The same updating as described above occurs.

In any other type of buffer, a folder is prompted for.
@enddefcom

@defcom[com "Quit Headers", stuff (bound to @bf[q] in @hid[Headers] and @hid[Message] modes) ]
This command affects the current @hid[Headers] buffer.  When there are deleted
messages, ask the user for confirmation on expunging the messages and packing
the folder's message ID's.  This prompting can be inhibited by setting
@hid[Expunge Messages Confirm] to @nil.  After deleting and packing, this
deletes the buffer and all its associated @hid[Message] buffers.

Other @hid[Headers] buffers regarding the same folder are handled as described
in @hid[Expunge Messages], but the buffer this command is invoked in is always
deleted.

When @hid[Temporary Draft Folder] is not @nil, this folder's messages are
deleted and expunged regardless of the folder in which the user invokes this
command.  A negative response to the above mentioned request for confirmation
inhibits this.
@enddefcom

@defcom[com "Delete Headers Buffer and Message Buffers" ]
This command prompts for a @hid[Headers] buffer to delete along with its
associated @hid[Message] buffers.  Any associated @hid[Draft] buffers are left
intact, but their corresponding @hid[Message] buffers will be deleted.  When
invoked in a @hid[Headers] buffer or a @hid[Message] buffer associated with a
@hid[Headers] buffer, that @hid[Headers] buffer is offered as a default.
@enddefcom


@section[Miscellaneous Commands]
@label[miscellaneous mail commands]
@label[miscellaneous]

@defcom[com "List Mail Buffers", stuff (bound to @bf[l] in @hid[Headers] and @hid[Message] modes @bf[H-l] in @hid[Draft] mode) ]
This command shows a list of all mail @hid[Message], @hid[Headers], and
@hid[Draft] buffers.

If a @hid[Message] buffer has an associated @hid[Headers] buffer, it is
displayed to the right of the @hid[Message] buffer's name.

If a @hid[Draft] buffer has an associated @hid[Message] buffer, it is displayed
to the right of the @hid[Draft] buffer's name.  If a @hid[Draft] buffer has no
associated @hid[Message] buffer, but it is associated with a @hid[Headers]
buffer, then the name of the @hid[Headers] buffer is displayed to the right of
the @hid[Draft] buffer.

For each buffer listed, if it is modified, then an asterisk is displayed before
the name of the buffer.
@enddefcom


@section[Styles of Usage]
@index[styles of mail interface usage]
@label[Styles]
This section discusses some styles of usage or ways to make use of some of the
features of @hemlock@comment{}'s interface to @mh that might not be obvious.  In each
case, setting some variables and/or remembering an extra side effect of a
command will lend greater flexibility and functionality to the user.

@subsection[Unseen Headers Message Spec]
The unseen @hid[Headers] buffer by default only shows unseen headers which is
adequate for one folder, simple mail handling.  Some people use their @hid[New
Mail Folder] only for incoming mail, refiling or otherwise dispatching a
message immediately.  Under this mode it is easy to conceive of the user not
having time to respond to a message, but he would like to leave it in this
folder to remind him to take care of it.  Using the @hid[Unseen Headers Message
Spec] variable, the user can cause all the messages the @hid[New Mail Folder] to
be inserted into the unseen @hid[Headers] buffer whenever just unseen headers
would be.  This way he sees all the messages that require immediate attention.

To achieve the above effect, @hid[Unseen Headers Message Spec] should be set to
the string @f["all"].  This variable can be set to any general @mh message
specification (see section @ref[mhcommands] of this chapter), so the user can
include headers of messages other than those that have not been seen without
having to insert all of them.  For example, the user could set the variable to
@f["flagged"] and use the @hid[Mark Message] command to add messages he's
concerned about to the @f["flagged"] sequence.  Then the user would see new
mail and interesting mail in his unseen @hid[Headers] buffer, but he doesn't
have to see everything in his @hid[New Mail Folder].


@subsection[Temporary Draft Folder]
Section @ref[components-files] of this chapter discusses how to make @mh keep
personal copies of outgoing mail.  The method described will cause a copy of
every outgoing message to be saved forever and requires the user to go through
his @f[Fcc:] folder, weeding out those he does not need.  The @hid[Temporary
Draft Folder] variable can name a folder whose messages will be deleted and
expunged whenever any folder's messages are expunged.  By naming this folder in
the @mh profile and components files, copies of outgoing messages can be saved
temporarily.  They will be cleaned up automatically, but the user still has a
time frame in which he can permanently save a copy of an outgoing message.
This folder can be visited with @hid[Message Headers], and messages can be
refiled just like any other folder.


@subsection[Reply to Message Prefix Action]
Depending on the kinds of messages one tends to handle, the user may find
himself usually replying to everyone who receives a certain message, or he may
find that this is only desired occasionally.  In either case, the user
can set up his @mh profile to do one thing by default, using the @hid[Reply
to Message Prefix Action] variable in combination with a prefix argument to the
@hid[Reply to Message] command to get the other effect.

For example, the following line in one's @mh profile will cause @mh to reply to
everyone receiving a certain message (except for the user himself since he
saves personal copies with the @f[-fcc] switch):
@begin[programexample]
repl: -cc all -nocc me -fcc out-copy
@end[programexample]
This user can set @hid[Reply to Message Prefix Action] to be @f[:no-cc-all].
Then whenever he invokes @hid[Reply to Message] with a prefix argument, instead
of replying to everyone, the draft will be set up in reply only to the person
who sent the mail.

As an alternative example, not specifying anything in one's @mh profile and
setting this variable to @f[:cc-all] will have a default effect of replying
only to the sender of a piece of mail.  Then invoking @hid[Reply to Message]
with a prefix argument will cause everyone who received the mail to get a copy
of the reply.  If the user does not want a @f[cc:] copy, then he can add
@f[-nocc me] as a default switch and value in his @mh profile.


@newpage
@section[Wallchart]

@tabclear
@tabdivide(3)

@begin[format, spacing 1.5]

@Begin[Center] @b[Global bindings:] @End[Center]

@hid[Incorporate and Read New Mail]@\@\@bf[C-x i]
@hid[Send Message]@\@\@bf[C-x m]
@hid[Message Headers]@\@\@bf[C-x r]


@Begin[Center] @b[Headers and Message modes bindings:] @End[Center]

@hid[Next Undeleted Message]@\@\@bf[n]
@hid[Previous Undeleted Message]@\@\@bf[p]
@hid[Send Message]@\@\@bf[s], @bf[m]
@hid[Forward Message]@\@\@bf[f]
@hid[Headers Delete Message]@\@\@bf[k]
@hid[Headers Undelete Message]@\@\@bf[u]
@hid[Headers Refile Message]@\@\@bf[o]
@hid[List Mail Buffers]@\@\@bf[l]
@hid[Quit Headers]@\@\@bf[q]
@hid[Incorporate and Read New Mail]@\@\@bf[i]
@hid[Next Message]@\@\@bf[M-n]
@hid[Previous Message]@\@\@bf[M-p]
@hid[Beginning of Buffer]@\@\@bf[<]
@hid[End of Buffer]@\@\@bf[>]


@Begin[Center] @b[Headers mode bindings:] @End[Center]

@hid[Delete Message and Down Line]@\@\@bf[d]
@hid[Pick Headers]@\@\@bf[h]
@hid[Show Message]@\@\@bf[space], @bf[.]
@hid[Reply to Message]@\@\@bf[r]
@hid[Expunge Messages]@\@\@bf[!]


@Begin[Center] @b[Message mode bindings:] @End[Center]

@hid[Delete Message and Show Next]@\@\@bf[d]
@hid[Goto Headers Buffer]@\@\@bf[^]
@hid[Scroll Message]@\@\@bf[space]
@hid[Scroll Message]@\@\@bf[C-v]
@hid[Scroll Window Up]@\@\@bf[backspace], @bf[delete]
@hid[Reply to Message in Other Window]@\@bf[r]
@hid[Edit Message Buffer]@\@\@bf[e]
@hid[Insert Message Region]@\@\@bf[H-y]


@Begin[Center] @b[Draft mode bindings:] @End[Center]

@hid[Goto Headers Buffer]@\@\@bf[H-^]
@hid[Goto Message Buffer]@\@\@bf[H-m]
@hid[Deliver Message]@\@\@bf[H-s], @bf[H-c]
@hid[Insert Message Buffer]@\@\@bf[H-y]
@hid[Delete Draft and Buffer]@\@\@bf[H-q]
@hid[List Mail Buffers]@\@\@bf[H-l]

@end[format]
@tabclear
