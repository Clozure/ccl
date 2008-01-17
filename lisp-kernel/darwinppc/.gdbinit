define header32
x/x $arg0-6
end

define header64
x/x $arg0-12
end

define lisp_string32
call print_lisp_object($arg0)
end

define lisp_string64
call print_lisp_object($arg0)
end

define pname32
lisp_string (*($arg0-2))
end

# GDB's expression parser seems to have difficulty
# with this unless the temporary is used.
define pname64
set $temp=*((long *)((long)($arg0-4)))
lisp_string64 $temp
end

define ada 
 p *all_areas->succ
end

define _TCR
 p/x *(TCR *) $arg0
end

define tcr32
 _TCR $r13
end

define tcr64
 _TCR $r2
end

define regs32
 p/x ((ExceptionInformation *)$arg0)->uc_mcontext.ss
end

define regs64
 p/x ((ExceptionInformation *)$arg0)->uc_mcontext64.ss
end

define xpGPR32
 p/x ((unsigned long *)&((((ExceptionInformation *)$arg0)->uc_mcontext.ss)))[2+$arg1]
end

define xpGPR64
 p/x ((unsigned long *)&((((ExceptionInformation *)$arg0)->uc_mcontext64.ss)))[2+$arg1]
end

define xpPC32
 p/x ((ExceptionInformation *)$arg0)->uc_mcontext.ss.srr0
end

define xpPC64
 p/x ((ExceptionInformation *)$arg0)->uc_mcontext64.ss.srr0
end

set $ppc64=0

define lisp_string
 if $ppc64
  lisp_string64 $arg0
 else
  lisp_string32 $arg0
 end
end

define pname
 if $ppc64
  pname64 $arg0
 else
  pname32 $arg0
 end
end

define tcr
 if $ppc64
  tcr64
 else
  tcr32
 end
end

define regs
 if $ppc64
  regs64 $arg0
 else
  regs32 $arg0
 end
end

define xpGPR
 if $ppc64
  xpGPR64 $arg0 $arg1
 else
  xpGPR32 $arg0 $arg1
 end
end

define xpPC
 if $ppc64
  xpPC64 $arg0
 else
  xpPC32 $arg0
 end
end

define header
 if $ppc64
  header64 $arg0
 else
  header32 $arg0
 end
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
