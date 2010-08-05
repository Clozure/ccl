# Some environments (gdb mode in XEmacs) interact poorly with
# the readline-based line editing features in some versions of GDB.
set editing off
define pl
 call print_lisp_object($arg0)
end
break Bug

display/i $pc

handle SIGKILL pass nostop noprint
handle SIGILL pass nostop noprint
handle SIGSEGV pass nostop noprint
handle SIGBUS pass nostop noprint
handle SIGFPE pass nostop noprint
handle SIG40 pass nostop noprint
handle SIG41 pass nostop noprint
handle SIG42 pass nostop noprint
handle SIGPWR pass nostop noprint


