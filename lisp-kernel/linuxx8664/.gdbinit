define x86_lisp_string
x/s $arg0-5
end

define gtra
br *$r10
cont
end

define x86pname
set $temp=*((long *)((long)($arg0-6)))
x86_lisp_string $temp
end


define pname
 x86pname $arg0
end

define l
 call print_lisp_object($arg0)
end

define lw
 l $r13
end

define clobber_breakpoint
  set *(short *)($pc-2)=0x9090
end

define arg_z
 l $rsi
end

define arg_y
 l $rdi
end

define arg_x
 l $r8
end

define bx
 l $rbx
end

define showlist
  set $l=$arg0
  while $l != 0x200b
   set $car = *((LispObj *)($l+5))
   set $l =  *((LispObj *)($l-3))
   l $car
  end
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
handle SIG40 pass nostop noprint
handle SIG41 pass nostop noprint
handle SIG42 pass nostop noprint
handle SIGPWR pass nostop noprint
handle SIGQUIT pass nostop noprint

