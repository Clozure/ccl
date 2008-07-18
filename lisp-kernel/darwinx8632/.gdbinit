define pl
  call print_lisp_object($arg0)
end

define showlist
  set $l=$arg0
  while $l != 0x3001
   set $car = *((LispObj *)($l+3))
   set $l =  *((LispObj *)($l-1))
   pl $car
  end
end


define fn
  pl $edi
end

define arg_y
 pl $esi
end

define arg_z
 pl $ebx
end

define offset
 p (int)$pc-$edi
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

