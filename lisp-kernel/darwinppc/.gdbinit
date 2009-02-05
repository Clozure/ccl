define pl
call print_lisp_object($arg0)
end

define arg_x
pl $r21
end

define arg_y
pl $r22
end

define arg_z
pl $r23
end

define lw
pl $r16
end

define fname
pl $r17
end


break Bug

display/i $pc

handle SIGKILL pass nostop noprint
handle SIGILL pass nostop noprint
handle SIGSEGV pass nostop noprint
handle SIGBUS pass nostop noprint
handle SIGFPE pass nostop noprint
handle SIGUSR1 pass nostop noprint
handle SIGUSR2 pass nostop noprint
handle SIGEMT pass nostop noprint
# Work around apparent Apple GDB bug
handle SIGTTIN nopass nostop noprint
