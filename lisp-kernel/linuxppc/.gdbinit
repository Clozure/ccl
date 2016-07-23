directory lisp-kernel

define pl
 call print_lisp_object($arg0)
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
 p/x *(((struct pt_regs **)$arg0)[12])
end

define regs64
 p/x * (((ExceptionInformation *)$arg0)->uc_mcontext.regs)
end

define xpGPR
 p/x (((struct pt_regs **)$arg0)[12])->gpr[$arg1]
end

define xpPC
 p/x ((ExceptionInformation *)$arg0)->uc_mcontext.regs->nip
end

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

define lisp
 call print_lisp_object($arg0)
end

set $ppc64=0


break Bug

handle SIGILL pass nostop noprint
handle SIGSEGV pass nostop noprint
handle SIGBUS pass nostop noprint
handle SIGFPE pass nostop noprint
handle SIG40 pass nostop noprint
handle SIG41 pass nostop noprint
handle SIG42 pass nostop noprint
handle SIGPWR pass nostop noprint

display/i $pc
