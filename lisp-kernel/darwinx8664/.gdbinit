define x86_lisp_string
x/s $arg0-5
end

define x86pname
set $temp=*((long *)((long)($arg0-6)))
x86_lisp_string $temp
end

define gtra
br *$r10
cont
end


define pname
 x86pname $arg0
end

define pl
 call print_lisp_object($arg0)
end

define lw
 pl $r13
end

define clobber_breakpoint
  set *(short *)($pc-2)=0x9090
end

define arg_z
 pl $rsi
end

define arg_y
 pl $rdi
end

define arg_x
 pl $r8
end

define bx
 pl $rbx
end


define lbt
 call plbt_sp($rbp)
end

define ada
 p/x *(all_areas->succ)
end

define lregs
 call debug_lisp_registers($arg0,0,0)
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
# Work around Leopard bug du jour
handle SIGSYS pass nostop noprint
handle SIGQUIT pass nostop noprint

