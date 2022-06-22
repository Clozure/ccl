/*
 * Copyright 1994-2009 Clozure Associates
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "lisp.h"
#include "lisp-exceptions.h"
#include "lisp_globals.h"
#include "area.h"
#include "threads.h"
#include <ctype.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdio.h>
#include <sys/stat.h>


#include <fcntl.h>
#ifndef WINDOWS
#include <sys/socket.h>
#include <dlfcn.h>
#endif


FILE *dbgout = NULL, *dbgin=NULL;

typedef enum {
  debug_continue,		/* stay in the repl */
  debug_exit_success,		/* return 0 from lisp_Debugger */
  debug_exit_fail,		/* return non-zero from lisp_Debugger */
  debug_kill
} debug_command_return;

#ifdef VC_REVISION
#define xstr(s) str(s)
#define str(s) #s
char *kernel_vc_revision = xstr(VC_REVISION);
#undef xstr
#undef str
#else
char *kernel_vc_revision = "unknown";
#endif

#ifdef ARM
#ifdef LINUX

/* This stuff is buried in kernel headers.  Why ? */

/* The uc_regspace field of a ucontext can contain coprocessor
   info in structures whose first word is one of these magic
   values; the structure list is terminated by something that's
   not one of these magic values.

   Good thinking! That'll make the mechanism easy to extend!
   (In practice, a word of 0 seems to terminate the structure
   list.)
*/
#define VFP_MAGIC		0x56465001
#define IWMMXT_MAGIC		0x12ef842a
#define CRUNCH_MAGIC		0x5065cf03


#ifndef ANDROID
struct user_vfp {
	unsigned long long fpregs[32];
	unsigned long fpscr;
};
#endif

struct user_vfp *
find_vfp_info(ExceptionInformation *xp)
{
  char *p = (char *)(xp->uc_regspace);
  unsigned *q, magic;

  while (1) {
    q = (unsigned *)p;                        
    magic = *q;
    if (magic == VFP_MAGIC) {
      return (struct user_vfp *)(q+2);
    }
    if ((magic == CRUNCH_MAGIC) ||
        (magic == IWMMXT_MAGIC)) {
      p += q[1];
    }
    else {
      return NULL;
    }
  }
}

#endif
#endif

Boolean
open_debug_output(int fd)
{
  FILE *f = fdopen(fd, "w");
  
  if (f) {
    if (setvbuf(f, NULL, _IONBF, 0) == 0) {
#ifdef WINDOWS
      if (fileno(stdin) < 0) {
        stdin->_file = 0;
      }
#endif
      dbgout = f;
      return true;
    }
    fclose(f);
  }
  return false;
}


typedef debug_command_return (*debug_command) (ExceptionInformation *,
					       siginfo_t *,
					       int);

#define DEBUG_COMMAND_FLAG_REQUIRE_XP 1 /* function  */
#define DEBUG_COMMAND_FLAG_AUX_REGNO  (2 | DEBUG_COMMAND_FLAG_REQUIRE_XP)
#define DEBUG_COMMAND_FLAG_AUX_SPR (4 | DEBUG_COMMAND_FLAG_REQUIRE_XP)
#define DEBUG_COMMAND_REG_FLAGS 7
#define DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY 8
#define DEBUG_COMMAND_FLAG_EXCEPTION_REASON_ARG 16

typedef struct {
  debug_command f;
  char *help_text;
  unsigned flags;
  char *aux_prompt;
  int c;
} debug_command_entry;


extern
debug_command_entry debug_command_entries[];

Boolean lisp_debugger_in_foreign_code = false;

#ifndef WINDOWS
Boolean
stdin_is_dev_null()
{
  struct stat fd0stat, devnullstat;

  if (fstat(fileno(stdin),&fd0stat)) {
    return true;
  }
  if (stat("/dev/null",&devnullstat)) {
    return true;
  }
  return ((fd0stat.st_ino == devnullstat.st_ino) &&
          (fd0stat.st_dev == devnullstat.st_dev));
}
#endif

#ifdef WINDOWS
Boolean
stdin_is_dev_null()
{
  HANDLE stdIn;
  stdIn = GetStdHandle(STD_INPUT_HANDLE);
  return (stdIn == NULL);
}
#endif




char *
foreign_name_and_offset(natural addr, int *delta)
{
#ifndef WINDOWS
  Dl_info info;
#endif
  char *ret = NULL;

  if (delta) {
    *delta = 0;
  }
#ifndef WINDOWS
#ifndef ANDROID
  if (dladdr((void *)addr, &info)) {
    ret = (char *)info.dli_sname;
    if (delta) {
      *delta = ((natural)addr - (natural)info.dli_saddr);
    }
  }
#endif
#endif
  return ret;
}


#if defined(LINUX) || defined(SOLARIS)
#include <stdio_ext.h>
#define fpurge __fpurge
#endif

#ifdef WINDOWS
void
fpurge (FILE* file)
{
}
#endif

void
redirect_debugger_io()
{
#ifndef WINDOWS
  if (1 || !isatty(fileno(dbgin))) {
    int fd = open("/dev/tty", O_RDWR);
    if (fd >=0) {
      dbgin=fdopen(fd,"r");
      open_debug_output(fd);
    }
  }
#endif
}
      
int
readc()
{
  unsigned tries = 1000;
  int c;

  while (tries) {
    c = fgetc(dbgin);
    switch(c) {
    case '\n':
      continue;
    case '\r':
      continue;
    case EOF:
      if (ferror(stdin)) {
	if ((errno == EINTR) || (errno == EIO)) {
	  clearerr(stdin);
	  tries--;
	  continue;
	}
      }
      /* fall through */
    default:
      return c;
    }
  }
  return EOF;
}

#ifdef X8664
#ifdef LINUX
char* Iregnames[] = {"r8 ","r9 ","r10","r11","r12","r13","r14","r15",
		     "rdi","rsi","rbp", "rbx", "rdx", "rax", "rcx","rsp"};
#endif
#ifdef SOLARIS
char* Iregnames[] = {"r15 ","r14 ","r13","r12","r11","r10","r9 ","r8 ",
		     "rdi","rsi","rbp", "rbx", "rdx", "rcx", "rcx","rsp"};
#endif
#ifdef FREEBSD
char* Iregnames[] = {"???", "rdi", "rsi", "rdx", "rcx", "r8 ", "r9 ", "rax",
                     "rbx", "rbp", "r10", "r11", "r12", "r13", "r14", "r15",
                     "???", "???", "???", "???", "???", "???", "???", "rsp"};
#endif
#ifdef DARWIN
char* Iregnames[] = {"rax", "rbx", "rcx", "rdx", "rdi", "rsi",
                     "rbp", "rsp", "r8 ", "r9 ", "r10", "r11", "r12", "r13",
                     "r14", "r15", "rip", "rfl"};
#endif
#ifdef WINDOWS
char* Iregnames[] = {"rax ","rcx ","rdx","rbx","rsp","rrbp","rsi","rdi",
		     "r8","r9","r10", "r11", "r12", "r13", "r14","r15"};
#endif
#endif

#ifdef X8632
#ifdef DARWIN
char *Iregnames[] = {"eax", "ebx", "ecx", "edx", "edi", "esi",
		     "ebp", "???", "efl", "eip"};
#endif
#ifdef LINUX
char *Iregnames[] = {"???", "???", "???", "???",
                     "edi", "esi", "ebp", "esp",
                     "ebx", "edx", "ecx", "eax",
                     "???", "???", "eip", "???", "efl"};
#endif
#ifdef WINDOWS
char *Iregnames[] = {"edi", "esi", "ebx", "edx", "ecx", "eax",
                     "ebp", "eip", "???", "efl", "esp"};
#endif
#ifdef FREEBSD
char *Iregnames[] = {"???", "???", "???", "???", "???"
                     "edi", "esi", "ebp", "ebx", "edx", 
		     "ecx", "eax", "???", "???", "eip",
		     "???", "efl", "esp"};
#endif
#ifdef SOLARIS
char *Iregnames[] = {"???", "???", "???", "???", "???",
                     "edi", "esi", "ebp", "???", "ebx",
                     "edx", "ecx", "eax", "???", "???",
                     "eip", "???", "efl", "esp"};
#endif
#endif

#ifdef X8632
int bit_for_regnum(int r)
{
  switch (r) {
  case REG_EAX: return 1<<0;
  case REG_ECX: return 1<<1;
  case REG_EDX: return 1<<2;
  case REG_EBX: return 1<<3;
  case REG_ESP: return 1<<4;
  case REG_EBP: return 1<<5;
  case REG_ESI: return 1<<6;
  default:                      /* stupid compiler */
  case REG_EDI: return 1<<7;
  }
}
#endif

void
show_lisp_register(ExceptionInformation *xp, char *label, int r)
{

  extern char* print_lisp_object(LispObj);

  LispObj val = xpGPR(xp, r);

#ifdef PPC
  fprintf(dbgout, "r%02d (%s) = %s\n", r, label, print_lisp_object(val));
#endif
#ifdef X8664
  fprintf(dbgout, "%%%s (%s) = %s\n",Iregnames[r], label, print_lisp_object(val));
#endif
#ifdef X8632
  {
    TCR *tcr = get_tcr(false);
    char *s;

    if (r == REG_EDX && (xpGPR(xp, REG_EFL) & EFL_DF))
      s = "marked as unboxed (DF set)";
    else if (tcr && (tcr->node_regs_mask & bit_for_regnum(r)) == 0)
      s = "marked as unboxed (node_regs_mask)";
    else
      s = print_lisp_object(val);

    fprintf(dbgout, "%%%s (%s) = %s\n", Iregnames[r], label, s);
  }
#endif
#ifdef ARM
  fprintf(dbgout, "r%02d (%s) = %s\n", r, label, print_lisp_object(val));
#endif
}

void
describe_siginfo(siginfo_t *info)
{
#if defined(WINDOWS) || defined(FREEBSD) || defined(DARWIN)
  /*
   * It's not surprising that Windows doesn't have this signal stuff.
   * It is somewhat surprising that FreeBSD 6.x lacks the si_code
   * constants.  (Subsequent FreeBSD versions define them, though.)
   *
   * On Darwin, recall that we handle exceptions at the Mach level,
   * and build a "fake" signal context ourselves.  We don't try very
   * hard to translate the Mach exception information to Unix-style
   * information, so avoid printing out possibly-misleading garbage.
   * (bsd/dev/i386/unix_signal.c from the xnu sources is where that
   * happens for Mac OS X's own Mach-exception-to-Unix-signal
   * translation.
   */
#else
  if (info->si_code > 0) {
    if (info->si_signo == SIGSEGV) {
      switch (info->si_code) {
      case SEGV_MAPERR:
	fprintf(dbgout, "address not mapped to object\n");
	break;
      case SEGV_ACCERR:
	fprintf(dbgout, "invalid permissions for mapped object\n");
	break;
      default:
	fprintf(dbgout, "unexpected si_code value: %d\n", info->si_code);
	break;
      }
    } else if (info->si_signo == SIGBUS) {
      switch (info->si_code) {
      case BUS_ADRALN:
	fprintf(dbgout, "invalid address alignment\n");
	break;
      case BUS_ADRERR:
	fprintf(dbgout, "non-existent physical address");
	break;
      case BUS_OBJERR:
	fprintf(dbgout, "object-specific hardware error");
	break;
      default:
	fprintf(dbgout, "unexpected si_code value: %d\n", info->si_code);
      }
    }
  }
#endif
}

void
describe_memfault(ExceptionInformation *xp, siginfo_t *info)
{
#ifdef PPC
  void *addr = (void *)xpDAR(xp);
  natural dsisr = xpDSISR(xp);

  fprintf(dbgout, "%s operation to %s address 0x%lx\n",
	  dsisr & (1<<25) ? "Write" : "Read",
	  dsisr & (1<<27) ? "protected" : "unmapped",
	  addr);
#elif !defined(WINDOWS)
  if (info) {
#ifdef X86
    if ((info->si_signo == SIGNUM_FOR_INTN_TRAP) &&
        IS_MAYBE_INT_TRAP(info, xp)) {
      pc program_counter = (pc)xpPC(xp);

      if ((program_counter != NULL) && 
          (*program_counter == INTN_OPCODE)) {
        fprintf(dbgout, "unhandled int 0x%x instruction\n", program_counter[1]);
      }
      return;
    }
#endif
    fprintf(dbgout, "received signal %d; faulting address: %p\n",
            info->si_signo, info->si_addr);
    describe_siginfo(info);
  }
#endif
}

#ifdef PPC
void
describe_ppc_illegal(ExceptionInformation *xp)
{
  pc where = xpPC(xp);
  opcode the_uuo = *where;
  Boolean described = false;

  if (IS_UUO(the_uuo)) {
    unsigned 
      minor = UUO_MINOR(the_uuo),
      errnum = 0x3ff & (the_uuo >> 16);

    switch(minor) {
    case UUO_INTERR:
      switch (errnum) {
      case error_udf_call:
        fprintf(dbgout, "ERROR: undefined function call: %s\n",
                print_lisp_object(xpGPR(xp,fname)));
        described = true;
        break;
        
      default:
        fprintf(dbgout, "ERROR: lisp error %d\n", errnum);
        described = true;
        break;
      }
      break;
      
    default:
      break;
    }
  }
  if (!described) {
    fprintf(dbgout, "Illegal instruction (0x%08x) at 0x%lx\n",
            the_uuo, where);
  }
}
#endif

#ifdef PPC
void
describe_ppc_trap(ExceptionInformation *xp)
{
  pc where = xpPC(xp);
  opcode the_trap = *where, instr;
  int err_arg2, ra, rs;
  Boolean identified = false;

  if ((the_trap & OP_MASK) == OP(major_opcode_TRI)) {
    /* TWI/TDI.  If the RA field is "nargs", that means that the
       instruction is either a number-of-args check or an
       event-poll.  Otherwise, the trap is some sort of
       typecheck. */

    if (RA_field(the_trap) == nargs) {
      switch (TO_field(the_trap)) {
      case TO_NE:
	if (xpGPR(xp, nargs) < D_field(the_trap)) {
	  fprintf(dbgout, "Too few arguments (no opt/rest)\n");
	} else {
	  fprintf(dbgout, "Too many arguments (no opt/rest)\n");
	}
	identified = true;
	break;
	
      case TO_GT:
	fprintf(dbgout, "Event poll !\n");
	identified = true;
	break;
	
      case TO_HI:
	fprintf(dbgout, "Too many arguments (with opt)\n");
	identified = true;
	break;
	
      case TO_LT:
	fprintf(dbgout, "Too few arguments (with opt/rest/key)\n");
	identified = true;
	break;
	
      default:                /* some weird trap, not ours. */
	identified = false;
	break;
      }
    } else {
      /* A type or boundp trap of some sort. */
      switch (TO_field(the_trap)) {
      case TO_EQ:
	/* Boundp traps are of the form:
	   treqi rX,unbound
	   where some preceding instruction is of the form:
	   lwz/ld rX,symbol.value(rY).
	   The error message should try to say that rY is unbound. */
	
	if (D_field(the_trap) == unbound) {
#ifdef PPC64
	  instr = scan_for_instr(LD_instruction(RA_field(the_trap),
                                                unmasked_register,
                                                offsetof(lispsymbol,vcell)-fulltag_misc),
				 D_RT_IMM_MASK,
				 where);
#else
	  instr = scan_for_instr(LWZ_instruction(RA_field(the_trap),
						 unmasked_register,
						 offsetof(lispsymbol,vcell)-fulltag_misc),
				 D_RT_IMM_MASK,
				 where);
#endif
	  if (instr) {
	    ra = RA_field(instr);
	    if (lisp_reg_p(ra)) {
	      fprintf(dbgout, "Unbound variable: %s\n",
		      print_lisp_object(xpGPR(xp,ra)));
	      identified = true;	
	    }
	  }
	}
	break;
	
      case TO_NE:
	/* A type check.  If the type (the immediate field of the trap
	   instruction) is a header type, an "lbz
	   rX,misc_header_offset(rY)" should precede it, in which case
	   we say that "rY is not of header type <type>."  If the type
	   is not a header type, then rX should have been set by a
	   preceding "clrlwi rX,rY,29/30".  In that case, scan
	   backwards for an RLWINM instruction that set rX and report
	   that rY isn't of the indicated type. */
	err_arg2 = D_field(the_trap);
	if (nodeheader_tag_p(err_arg2) ||
	    immheader_tag_p(err_arg2)) {
	  instr = scan_for_instr(LBZ_instruction(RA_field(the_trap),
						 unmasked_register,
						 misc_subtag_offset),
				 D_RT_IMM_MASK,
				 where);
	  if (instr) {
	    ra = RA_field(instr);
	    if (lisp_reg_p(ra)) {
	      fprintf(dbgout, "value 0x%lX is not of the expected header type 0x%02X\n", xpGPR(xp, ra), err_arg2);
	      identified = true;
	    }
	  }
	} else {		
	  /* Not a header type, look for rlwinm whose RA field matches the_trap's */
	  instr = scan_for_instr((OP(major_opcode_RLWINM) | (the_trap & RA_MASK)),
				 (OP_MASK | RA_MASK),
				 where);
	  if (instr) {
	    rs = RS_field(instr);
	    if (lisp_reg_p(rs)) {
	      fprintf(dbgout, "value 0x%lX is not of the expected type 0x%02X\n",
		      xpGPR(xp, rs), err_arg2);
	      identified = true;
	    }
	  }
	}
	break;
      }
    }
  } else {
    /* a "TW <to>,ra,rb" instruction."
       twltu sp,rN is stack-overflow on SP.
       twgeu rX,rY is subscript out-of-bounds, which was preceded
       by an "lwz rM,misc_header_offset(rN)" instruction.
       rM may or may not be the same as rY, but no other header
       would have been loaded before the trap. */
    switch (TO_field(the_trap)) {
    case TO_LO:
      if (RA_field(the_trap) == sp) {
	fprintf(dbgout, "Stack overflow! Run away! Run away!\n");
	identified = true;
      }
      break;
      
    case (TO_HI|TO_EQ):
      instr = scan_for_instr(OP(major_opcode_LWZ) | (D_MASK & misc_header_offset),
			     (OP_MASK | D_MASK),
			     where);
      if (instr) {
	ra = RA_field(instr);
	if (lisp_reg_p(ra)) {
	  fprintf(dbgout, "Bad index %d for vector %lX length %d\n",
		  unbox_fixnum(xpGPR(xp, RA_field(the_trap))),
		  xpGPR(xp, ra),
		  unbox_fixnum(xpGPR(xp, RB_field(the_trap))));
	  identified = true;
	}
      }
      break;
    }
  }

  if (!identified) {
    fprintf(dbgout, "Unknown trap: 0x%08x\n", the_trap);
  }


}
#endif

#ifdef ARM
void
describe_arm_uuo(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instruction = *program_counter;

  if (IS_UUO(instruction)) {
    unsigned format = UUO_FORMAT(instruction);

    switch(format) {
    case uuo_format_nullary:
    case uuo_format_nullary_error:
      switch UUOA_field(instruction) {
      case 0:
        fprintf(dbgout,"alloc_trap\n");
        break;
      case 1:
        fprintf(dbgout,"wrong number of args (%d) to %s\n",xpGPR(xp,nargs)>>node_shift,
                print_lisp_object(xpGPR(xp,nfn)));
        break;
      case 2:
        fprintf(dbgout,"gc trap\n");
        break;
      case 3:
        fprintf(dbgout,"debug trap\n");
        break;
      case 4:
        fprintf(dbgout,"deferred interrupt\n");
        break;
      case 5:
        fprintf(dbgout,"deferred suspend\n");
        break;
      default:
        break;
      }
      break;

    case uuo_format_unary_error:
      switch (UUO_UNARY_field(instruction)) {
      case 0:
      case 1:
        fprintf(dbgout,"%s is unbound\n", print_lisp_object(xpGPR(xp,UUOA_field(instruction))));
        break;

      default:
        break;
      }
    default:
      break;
    }
  }
}
#endif

char *
area_code_name(int code)
{
  switch (code) {
    case AREA_VOID: return "void";
    case AREA_CSTACK: return "cstack";
    case AREA_VSTACK: return "vstack";
    case AREA_TSTACK: return "tstack";
    case AREA_READONLY: return "readonly";
    case AREA_WATCHED: return "watched";
    case AREA_STATIC_CONS: return "static cons";
    case AREA_MANAGED_STATIC: return "managed static";
    case AREA_STATIC: return "static";
    case AREA_DYNAMIC: return "dynamic";
    default: return "unknown";
  }
}

debug_command_return
debug_memory_areas(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  area *a, *header = all_areas;
  char label[100];

  fprintf(dbgout, "Lisp memory areas:\n");
  fprintf(dbgout, "%20s %20s %20s\n", "code", "low", "high");
  for (a = header->succ; a != header; a = a->succ) {
    snprintf(label, sizeof(label), "%s (%d)", area_code_name(a->code),
	     a->code >> fixnumshift);
    fprintf(dbgout, "%20s %20p %20p\n", label, a->low, a->high);
  }
  return debug_continue;
}

debug_command_return
debug_lisp_registers(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  if (lisp_debugger_in_foreign_code == false) {
#ifdef PPC
    TCR *xpcontext = (TCR *)ptr_from_lispobj(xpGPR(xp, rcontext));

    fprintf(dbgout, "rcontext = 0x%lX ", xpcontext);
    if (!active_tcr_p(xpcontext)) {
      fprintf(dbgout, "(INVALID)\n");
    } else {
      fprintf(dbgout, "\nnargs = %d\n", xpGPR(xp, nargs) >> fixnumshift);
      show_lisp_register(xp, "fn", fn);
      show_lisp_register(xp, "arg_z", arg_z);
      show_lisp_register(xp, "arg_y", arg_y);
      show_lisp_register(xp, "arg_x", arg_x);
      show_lisp_register(xp, "temp0", temp0);
      show_lisp_register(xp, "temp1/next_method_context", temp1);
      show_lisp_register(xp, "temp2/nfn", temp2);
      show_lisp_register(xp, "temp3/fname", temp3);
      /*    show_lisp_register(xp, "new_fn", new_fn); */
      show_lisp_register(xp, "save0", save0);
      show_lisp_register(xp, "save1", save1);
      show_lisp_register(xp, "save2", save2);
      show_lisp_register(xp, "save3", save3);
      show_lisp_register(xp, "save4", save4);
      show_lisp_register(xp, "save5", save5);
      show_lisp_register(xp, "save6", save6);
      show_lisp_register(xp, "save7", save7);
    }
#endif
#ifdef X8664

    show_lisp_register(xp, "arg_z", Iarg_z);
    show_lisp_register(xp, "arg_y", Iarg_y);
    show_lisp_register(xp, "arg_x", Iarg_x);
    fprintf(dbgout,"------\n");
    show_lisp_register(xp, "fn", Ifn);
    fprintf(dbgout,"------\n");
    show_lisp_register(xp, "temp6", Itemp6);
    show_lisp_register(xp, "temp5", Itemp5);
    show_lisp_register(xp, "temp4", Itemp4);
    show_lisp_register(xp, "temp3", Itemp3);
    fprintf(dbgout,"------\n");
    show_lisp_register(xp, "temp0", Itemp0);
    show_lisp_register(xp, "temp1", Itemp1);
    show_lisp_register(xp, "temp2", Itemp2);
    fprintf(dbgout,"------\n");
    if (tag_of(xpGPR(xp,Inargs)) == tag_fixnum) {
      fprintf(dbgout,"%%rcx (nargs) = %ld (maybe)\n", unbox_fixnum(xpGPR(xp,Inargs)&0xffff));
    }
#endif

#ifdef X8632
  show_lisp_register(xp, "arg_z", Iarg_z);
  show_lisp_register(xp, "arg_y", Iarg_y);
  fprintf(dbgout,"------\n");
  show_lisp_register(xp, "fn", Ifn);
  fprintf(dbgout,"------\n");
  show_lisp_register(xp, "temp0", Itemp0);
  show_lisp_register(xp, "temp1", Itemp1);
  fprintf(dbgout,"------\n");
  if (tag_of(xpGPR(xp,Inargs)) == tag_fixnum) {
    fprintf(dbgout,"%%edx (nargs) = %d (maybe)\n", unbox_fixnum(xpGPR(xp,Inargs)));
  }
#endif
#ifdef ARM
    TCR *xpcontext = (TCR *)ptr_from_lispobj(xpGPR(xp, rcontext));
    extern Boolean active_tcr_p(TCR *);

    fprintf(dbgout, "rcontext = 0x%lX ", xpcontext);
    if (!active_tcr_p(xpcontext)) {
      fprintf(dbgout, "(INVALID)\n");
    } else {
      fprintf(dbgout, "\nnargs = %d\n", xpGPR(xp, nargs) >> fixnumshift);
      show_lisp_register(xp, "fn", Rfn);
      show_lisp_register(xp, "arg_z", arg_z);
      show_lisp_register(xp, "arg_y", arg_y);
      show_lisp_register(xp, "arg_x", arg_x);
      show_lisp_register(xp, "temp0", temp0);
      show_lisp_register(xp, "temp1/fname/next_method_context", temp1);
      show_lisp_register(xp, "temp2/nfn", temp2);
    }
#endif
  }
  
  return debug_continue;
}

#ifndef X86
debug_command_return
debug_advance_pc(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  adjust_exception_pc(xp,4);
  return debug_continue;
}
#endif

debug_command_return
debug_identify_exception(ExceptionInformation *xp, siginfo_t *info, int arg)
{
#ifndef X86
  pc program_counter = xpPC(xp);
  opcode instruction = 0;
#endif

  switch (arg) {
#ifdef PPC
  case SIGILL:
  case SIGTRAP:
    instruction = *program_counter;
    if (major_opcode_p(instruction, major_opcode_TRI) ||
	X_opcode_p(instruction,major_opcode_X31,minor_opcode_TR)) {
      describe_ppc_trap(xp);
    } else {
      describe_ppc_illegal(xp);
    }
    break;
#endif

#ifdef ARM  
  case SIGILL:
    instruction = *program_counter;
    if (IS_UUO(instruction)) {
      describe_arm_uuo(xp);
    }
    break;
#endif
  case SIGSEGV:
  case SIGBUS:
    describe_memfault(xp, info);
    break;
  default:
    break;
  }
  return debug_continue;
}

char *
debug_get_string_value(char *prompt)
{
  static char buf[128];
  char *p, *res;

  do {
    fpurge(stdin);
    fprintf(dbgout, "\n %s :",prompt);
    buf[0] = 0;
    res = fgets(buf, sizeof(buf),dbgin);
  } while (0);
  p = strchr(res, '\n');
  if (p) {
    *p = 0;
    return buf;
  }
  return NULL;
}

natural
debug_get_natural_value(char *prompt)
{
  char s[32], *res, *endptr;
  natural val;

  do {
    fpurge(stdin);
    fprintf(dbgout, "\n  %s :", prompt);
    s[0]=0;
    res = fgets(s, 24, dbgin);
    val = strtoul(res,&endptr,0);
  } while (*endptr);
  return val;
}

unsigned
debug_get_u5_value(char *prompt)
{
  char s[32], *res;
  int n;
  unsigned val;

  do {
    fpurge(stdin);
    fprintf(dbgout, "\n  %s :", prompt);
    res = fgets(s, 24, dbgin);
    n = sscanf(res, "%i", &val);
  } while ((n != 1) || (val > 31));
  return val;
}

debug_command_return
debug_show_symbol(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  char *pname = debug_get_string_value("symbol name");
  extern void *plsym(ExceptionInformation *,char*);
  
  if (pname != NULL) {
    plsym(xp, pname);
  }
  return debug_continue;
}

debug_command_return
debug_show_lisp_version(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  extern void *plsym(ExceptionInformation *,char*);

  fprintf(dbgout, "Lisp kernel vc revision: %s\n", kernel_vc_revision);
  if (xp)
    plsym(xp, "*OPENMCL-VERSION*");
  return debug_continue;
}

debug_command_return
debug_thread_info(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  TCR * tcr = get_tcr(false);
  
  if (tcr) {
    area *vs_area = tcr->vs_area, *cs_area;

    if (TCR_AUX(tcr))
      cs_area = TCR_AUX(tcr)->cs_area;

    fprintf(dbgout, "Current Thread Context Record (tcr) = 0x" LISP "\n", tcr);
    fprintf(dbgout, "Control (C) stack area:  low = 0x" LISP ", high = 0x" LISP "\n",
            (cs_area->low), (cs_area->high));
    fprintf(dbgout, "Value (lisp) stack area: low = 0x" LISP ", high = 0x" LISP "\n",
            (natural)(vs_area->low), (natural)vs_area->high);
    if (xp) {
      fprintf(dbgout, "Exception stack pointer = 0x" LISP "\n",
#ifdef PPC
              (natural)(xpGPR(xp,1))
#endif
#ifdef X86
              (natural)(xpGPR(xp,Isp))
#endif           
#ifdef ARM
              (natural)(xpGPR(xp,Rsp))
#endif
              );
    }
  }
  return debug_continue;
}
      

debug_command_return
debug_set_gpr(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  char buf[32];
  natural val;

  sprintf(buf, "value for GPR %d", arg);
  val = debug_get_natural_value(buf);
  xpGPR(xp,arg) = val;
  return debug_continue;
}

debug_command_return
debug_show_registers(ExceptionInformation *xp, siginfo_t *info, int arg)
{


#ifdef PPC
#ifdef PPC64
  int a, b;
  for (a = 0, b = 16; a < 16; a++, b++) {
    fprintf(dbgout,"r%02d = 0x%016lX    r%02d = 0x%016lX\n",
	    a, xpGPR(xp, a),
	    b, xpGPR(xp, b));
  }
  
  fprintf(dbgout, "\n PC = 0x%016lX     LR = 0x%016lX\n",
          xpPC(xp), xpLR(xp));
  fprintf(dbgout, "CTR = 0x%016lX    CCR = 0x%08X\n",
          xpCTR(xp), xpCCR(xp));
  fprintf(dbgout, "XER = 0x%08X            MSR = 0x%016lX\n",
          xpXER(xp), xpMSR(xp));
  fprintf(dbgout,"DAR = 0x%016lX  DSISR = 0x%08X\n",
	  xpDAR(xp), xpDSISR(xp));
#else
  int a, b, c, d;;
  for (a = 0, b = 8, c = 16, d = 24; a < 8; a++, b++, c++, d++) {
    fprintf(dbgout,"r%02d = 0x%08X  r%02d = 0x%08X  r%02d = 0x%08X  r%02d = 0x%08X\n",
	    a, xpGPR(xp, a),
	    b, xpGPR(xp, b),
	    c, xpGPR(xp, c),
	    d, xpGPR(xp, d));
  }
  fprintf(dbgout, "\n PC = 0x%08X   LR = 0x%08X  CTR = 0x%08X  CCR = 0x%08X\n",
	  xpPC(xp), xpLR(xp), xpCTR(xp), xpCCR(xp));
  fprintf(dbgout, "XER = 0x%08X  MSR = 0x%08X  DAR = 0x%08X  DSISR = 0x%08X\n",
	  xpXER(xp), xpMSR(xp), xpDAR(xp), xpDSISR(xp));
#endif
#endif

#ifdef X8664
  fprintf(dbgout,"%%rax = 0x" ZLISP "      %%r8  = 0x" ZLISP "\n", xpGPR(xp,REG_RAX),xpGPR(xp,REG_R8));
  fprintf(dbgout,"%%rcx = 0x" ZLISP "      %%r9  = 0x" ZLISP "\n", xpGPR(xp,REG_RCX),xpGPR(xp,REG_R9));
  fprintf(dbgout,"%%rdx = 0x" ZLISP "      %%r10 = 0x" ZLISP "\n", xpGPR(xp,REG_RDX),xpGPR(xp,REG_R10));
  fprintf(dbgout,"%%rbx = 0x" ZLISP "      %%r11 = 0x" ZLISP "\n", xpGPR(xp,REG_RBX),xpGPR(xp,REG_R11));
  fprintf(dbgout,"%%rsp = 0x" ZLISP "      %%r12 = 0x" ZLISP "\n", xpGPR(xp,REG_RSP),xpGPR(xp,REG_R12));
  fprintf(dbgout,"%%rbp = 0x" ZLISP "      %%r13 = 0x" ZLISP "\n", xpGPR(xp,REG_RBP),xpGPR(xp,REG_R13));
  fprintf(dbgout,"%%rsi = 0x" ZLISP "      %%r14 = 0x" ZLISP "\n", xpGPR(xp,REG_RSI),xpGPR(xp,REG_R14));
  fprintf(dbgout,"%%rdi = 0x" ZLISP "      %%r15 = 0x" ZLISP "\n", xpGPR(xp,REG_RDI),xpGPR(xp,REG_R15));
  fprintf(dbgout,"%%rip = 0x" ZLISP "   %%rflags = 0x%08lx\n",
	  xpGPR(xp, Iip), eflags_register(xp));
#endif

#ifdef X8632
  unsigned short rcs,rds,res,rfs,rgs,rss;
#ifdef DARWIN
  rcs = xp->uc_mcontext->__ss.__cs;
  rds = xp->uc_mcontext->__ss.__ds;
  res = xp->uc_mcontext->__ss.__es;
  rfs = xp->uc_mcontext->__ss.__fs;
  rgs = xp->uc_mcontext->__ss.__gs;
  rss = xp->uc_mcontext->__ss.__ss;
#define DEBUG_SHOW_X86_SEGMENT_REGISTERS
#endif
#ifdef LINUX
  rcs = xp->uc_mcontext.gregs[REG_CS];
  rds = xp->uc_mcontext.gregs[REG_DS];
  res = xp->uc_mcontext.gregs[REG_ES];
  rfs = xp->uc_mcontext.gregs[REG_FS];
  rgs = xp->uc_mcontext.gregs[REG_GS];
  rss = xp->uc_mcontext.gregs[REG_SS];
#define DEBUG_SHOW_X86_SEGMENT_REGISTERS
#endif
#ifdef FREEBSD
  rcs = xp->uc_mcontext.mc_cs;
  rds = xp->uc_mcontext.mc_ds;
  res = xp->uc_mcontext.mc_es;
  rfs = xp->uc_mcontext.mc_fs;
  rgs = xp->uc_mcontext.mc_gs;
  rss = xp->uc_mcontext.mc_ss;
#define DEBUG_SHOW_X86_SEGMENT_REGISTERS
#endif
#ifdef SOLARIS
  rcs = xp->uc_mcontext.gregs[CS];
  rds = xp->uc_mcontext.gregs[DS];
  res = xp->uc_mcontext.gregs[ES];
  rfs = xp->uc_mcontext.gregs[FS];
  rgs = xp->uc_mcontext.gregs[GS];
  rss = xp->uc_mcontext.gregs[SS];
#define DEBUG_SHOW_X86_SEGMENT_REGISTERS
#endif
#ifdef WINDOWS
  rcs = xp->SegCs;
  rds = xp->SegDs;
  res = xp->SegEs;
  rfs = xp->SegFs;
  rgs = xp->SegGs;
  rss = xp->SegSs;
#define DEBUG_SHOW_X86_SEGMENT_REGISTERS
#endif



  fprintf(dbgout, "%%eax = 0x" ZLISP "\n", xpGPR(xp, REG_EAX));
  fprintf(dbgout, "%%ecx = 0x" ZLISP "\n", xpGPR(xp, REG_ECX));
  fprintf(dbgout, "%%edx = 0x" ZLISP "\n", xpGPR(xp, REG_EDX));
  fprintf(dbgout, "%%ebx = 0x" ZLISP "\n", xpGPR(xp, REG_EBX));
  fprintf(dbgout, "%%esp = 0x" ZLISP "\n", xpGPR(xp, REG_ESP));
  fprintf(dbgout, "%%ebp = 0x" ZLISP "\n", xpGPR(xp, REG_EBP));
  fprintf(dbgout, "%%esi = 0x" ZLISP "\n", xpGPR(xp, REG_ESI));
  fprintf(dbgout, "%%edi = 0x" ZLISP "\n", xpGPR(xp, REG_EDI));
  fprintf(dbgout, "%%eip = 0x" ZLISP "\n", xpGPR(xp, REG_EIP));
  fprintf(dbgout, "%%eflags = 0x" ZLISP "\n", xpGPR(xp, REG_EFL));
#ifdef DEBUG_SHOW_X86_SEGMENT_REGISTERS
  fprintf(dbgout,"\n");
  fprintf(dbgout, "%%cs = 0x%04x\n", rcs);
  fprintf(dbgout, "%%ds = 0x%04x\n", rds);
  fprintf(dbgout, "%%ss = 0x%04x\n", rss);
  fprintf(dbgout, "%%es = 0x%04x\n", res);
  fprintf(dbgout, "%%fs = 0x%04x\n", rfs);
  fprintf(dbgout, "%%gs = 0x%04x\n", rgs);

#endif

#endif

#ifdef ARM
  int a, b;
  for (a = 0, b = 8; a < 8; a++, b++) {
    fprintf(dbgout,"r%02d = 0x%08lX    r%02d = 0x%08lX\n",
	    a, xpGPR(xp, a),
	    b, xpGPR(xp, b));
  }
#endif

  return debug_continue;
}


debug_command_return
debug_show_fpu(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  double *dp;
  int *np, i;
#ifdef PPC
  dp = xpFPRvector(xp);
  np = (int *) dp;
  
  for (i = 0; i < 32; i++, np+=2) {
    fprintf(dbgout, "f%02d : 0x%08X%08X (%f)\n", i,  np[0], np[1], *dp++);
  }
  fprintf(dbgout, "FPSCR = %08X\n", xpFPSCR(xp));
#endif
#ifdef X8664
#ifdef LINUX
  struct _libc_xmmreg * xmmp = NULL;
#endif
#ifdef DARWIN
  struct xmm {
    char fpdata[16];
  };
  struct xmm *xmmp = (struct xmm *)(xpFPRvector(xp));
#endif
#ifdef WINDOWS
  struct xmm {
    char fpdata[16];
  };
  struct xmm *xmmp; /* XXX: actually get them */
#endif
#ifdef FREEBSD
  struct xmmacc *xmmp = xpXMMregs(xp);
#endif
#ifdef SOLARIS
  upad128_t *xmmp = xpXMMregs(xp);
#endif
  float *sp;

#ifdef LINUX
  if (xp->uc_mcontext.fpregs)
    xmmp = &(xp->uc_mcontext.fpregs->_xmm[0]);
  else
    /* no fp state, apparently */
    return debug_continue;
#endif

  for (i = 0; i < 16; i++, xmmp++) {
    sp = (float *) xmmp;
    dp = (double *) xmmp;
    np = (int *) xmmp;
    fprintf(dbgout, "f%02d: 0x%08x (%e), 0x%08x%08x (%e)\n", i, *np, (double)(*sp), np[1], np[0], *dp);
  }
  fprintf(dbgout, "mxcsr = 0x%08x\n",
#ifdef LINUX
          xp->uc_mcontext.fpregs->mxcsr
#endif
#ifdef DARWIN
          UC_MCONTEXT(xp)->__fs.__fpu_mxcsr
#endif
#ifdef FREEBSD
          (((struct savefpu *)(&(xp)->uc_mcontext.mc_fpstate))->sv_env.en_mxcsr)
#endif
#ifdef SOLARIS
	  xp->uc_mcontext.fpregs.fp_reg_set.fpchip_state.xstatus
#endif
#ifdef WINDOWS
          *(xpMXCSRptr(xp))
#endif
          );
#endif  
#ifdef X8632
#ifdef DARWIN
  struct xmm {
    char fpdata[8];
  };
  struct xmm *xmmp = (struct xmm *)(xpFPRvector(xp));

  for (i = 0; i < 8; i++, xmmp++) {
    float *sp = (float *)xmmp;
    dp = (double *)xmmp;
    np = (int *)xmmp;
    fprintf(dbgout, "f%1d: 0x%08x (%e), 0x%08x%08x (%e)\n", i, *np,
	    (double)(*sp), np[1], np[0], *dp);
  }
  fprintf(dbgout, "mxcsr = 0x%08x\n", UC_MCONTEXT(xp)->__fs.__fpu_mxcsr);
#endif
#endif
#ifdef ARM
#ifdef LINUX
  struct user_vfp *vfp = find_vfp_info(xp);
  
  if (vfp != NULL) {
    float *fp = (float *)vfp;
    double *dp = (double *)vfp;
    unsigned *up = (unsigned *)vfp;
    unsigned long long *llp = (unsigned long long *)vfp;
    int dn,fn;


    for (dn=0,fn=0;dn<16;dn++) {
      fprintf(dbgout, "s%02d = %10e (0x%08x)        s%02d = %10e (0x%08x)\n",fn,fp[fn],up[fn],fn+1,fp[fn+1],up[fn+1]);
      fn+=2;
      fprintf(dbgout, "d%02d = %10e (0x%015llx)\n",dn,dp[dn],llp[dn]);
    }
    fprintf(dbgout, "FPSCR = 0x%08x\n", vfp->fpscr);
  }
#endif
#endif
  return debug_continue;
}

debug_command_return
debug_kill_process(ExceptionInformation *xp, siginfo_t *info, int arg) {
  return debug_kill;
}

debug_command_return
debug_win(ExceptionInformation *xp, siginfo_t *info, int arg) {
  return debug_exit_success;
}

debug_command_return
debug_lose(ExceptionInformation *xp, siginfo_t *info, int arg) {
  return debug_exit_fail;
}

debug_command_return
debug_help(ExceptionInformation *xp, siginfo_t *info, int arg) {
  debug_command_entry *entry;

  for (entry = debug_command_entries; entry->f; entry++) {
    /* If we have an XP or don't need one, call the function */
    if (xp || !(entry->flags & DEBUG_COMMAND_FLAG_REQUIRE_XP)) {
      fprintf(dbgout, "(%c)  %s\n", entry->c, entry->help_text);
    }
  }
  return debug_continue;
}
	      

  

debug_command_return
debug_backtrace(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  extern LispObj current_stack_pointer();
  extern void plbt_sp(LispObj);
  extern void plbt(ExceptionInformation *);

  if (xp) {
    plbt(xp);
#ifndef X86
  } else {
    plbt_sp(current_stack_pointer());
#endif
  }
  return debug_continue;
}

debug_command_return
debug_thread_reset(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  reset_lisp_process(xp);
  return debug_exit_success;
}


debug_command_entry debug_command_entries[] = 
{
  {debug_set_gpr,
   "Set specified GPR to new value",
   DEBUG_COMMAND_FLAG_AUX_REGNO,
   "GPR to set (0-31) ?",
   'G'},
#ifndef X86
  {debug_advance_pc,
   "Advance the program counter by one instruction (use with caution!)",
   DEBUG_COMMAND_FLAG_REQUIRE_XP | DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY,
   NULL,
   'A'},
  {debug_identify_exception,
   "Describe the current exception in greater detail",
   DEBUG_COMMAND_FLAG_REQUIRE_XP | DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY |
   DEBUG_COMMAND_FLAG_EXCEPTION_REASON_ARG,
   NULL,
   'D'},
#endif
  {debug_show_registers, 
   "Show raw GPR/SPR register values", 
   DEBUG_COMMAND_FLAG_REQUIRE_XP,
   NULL,
   'R'},
  {debug_lisp_registers,
   "Show Lisp values of tagged registers",
   DEBUG_COMMAND_FLAG_REQUIRE_XP,
   NULL,
   'L'},
  {debug_show_fpu,
   "Show FPU registers",
   DEBUG_COMMAND_FLAG_REQUIRE_XP,
   NULL,
   'F'},
  {debug_show_symbol,
   "Find and describe symbol matching specified name",
   0,
   NULL,
   'S'},
  {debug_backtrace,
   "Show backtrace",
   0,
   NULL,
   'B'},
  {debug_thread_info,
   "Show info about current thread",
   0,
   NULL,
   'T'},
  {debug_memory_areas,
   "Show memory areas",
   0,
   NULL,
   'M'},
  {debug_win,
   "Exit from this debugger, asserting that any exception was handled",
   0,
   NULL,
   'X'},
#ifdef DARWIN
  {debug_lose,
   "Propagate the exception to another handler (debugger or OS)",
   DEBUG_COMMAND_FLAG_REQUIRE_XP | DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY,
   NULL,
   'P'},
#endif
#if 0
  {debug_thread_reset,
   "Reset current thread (as if in response to stack overflow)",
   DEBUG_COMMAND_FLAG_REQUIRE_XP,
   NULL,
   'T'},
#endif
  {debug_kill_process,
   "Kill Clozure CL process",
   0,
   NULL,
   'K'},
  {debug_show_lisp_version,
   "Show Subversion revision information",
   0,
   NULL,
   'V'},
  {debug_help,
   "Show this help",
   0,
   NULL,
   '?'},
  /* end-of-table */
  {NULL,
   NULL,
   0,
   NULL,
   0}
};

debug_command_return
apply_debug_command(ExceptionInformation *xp, int c, siginfo_t *info, int why) 
{
  if (c == EOF) {
    return debug_kill;
  } else {
    debug_command_entry *entry;
    debug_command f;
    c = toupper(c);

    for (entry = debug_command_entries; (f = entry->f) != NULL; entry++) {
      if (toupper(entry->c) == c) {
	/* If we have an XP or don't need one, call the function */
	if ((xp || !(entry->flags & DEBUG_COMMAND_FLAG_REQUIRE_XP)) &&
	    ((why > debug_entry_exception) || 
	     !(entry->flags & DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY))) {
	  int arg = 0;
	  if ((entry->flags & DEBUG_COMMAND_REG_FLAGS)
	      == DEBUG_COMMAND_FLAG_AUX_REGNO) {
	    arg = debug_get_u5_value("register number");
	  }
	  if (entry->flags & DEBUG_COMMAND_FLAG_EXCEPTION_REASON_ARG) {
	    arg = why;
	  }
	  return (f)(xp, info, arg);
	}
	break;
      }
    }
    return debug_continue;
  }
}

void
debug_identify_function(ExceptionInformation *xp, siginfo_t *info) 
{
#ifdef PPC
  if (xp) {
    if (active_tcr_p((TCR *)(ptr_from_lispobj(xpGPR(xp, rcontext))))) {
      LispObj f = xpGPR(xp, fn), codev;
      pc where = xpPC(xp);
      
      if (!(codev = register_codevector_contains_pc(f, where))) {
        f = xpGPR(xp, nfn);
        codev =  register_codevector_contains_pc(f, where);
      }
      if (codev) {
        fprintf(dbgout, " While executing: %s\n", print_lisp_object(f));
      }
    } else {
      int disp;
      char *foreign_name;
      natural where = (natural)xpPC(xp);

      fprintf(dbgout, " In foreign code at address 0x" ZLISP "\n", where);
      foreign_name = foreign_name_and_offset(where, &disp);
      if (foreign_name) {
        fprintf(dbgout, "  [%s + %d]\n", foreign_name, disp);
      }
    }
  }
#endif
}

#ifndef WINDOWS
extern pid_t main_thread_pid;
#endif


static Boolean in_postmortem = false;

OSStatus
lisp_Debugger(ExceptionInformation *xp, 
	      siginfo_t *info, 
	      int why, 
              Boolean in_foreign_code,
	      char *message, 
	      ...)
{
  va_list args;
  debug_command_return state = debug_continue;

  if (in_postmortem) {
    /* If we get reentered trying to print crash info, just exit
       as quickly and quietly as possible.  Don't even print a
       message: stdio may be hosed.
    */
#ifdef ANDROID
    _exit(1);
#else
    abort();
#endif
  }
  if (stdin_is_dev_null()) {
    return -1;
  }

  va_start(args,message);
  vfprintf(dbgout, message, args);
  fprintf(dbgout, "\n");
  va_end(args);

  if (threads_initialized) {
    suspend_other_threads(false);
  }

  lisp_debugger_in_foreign_code = in_foreign_code;
  if (in_foreign_code) {    
    char *foreign_name;
    int disp;
    fprintf(dbgout, "Exception occurred while executing foreign code\n");
    foreign_name = foreign_name_and_offset((natural)xpPC(xp), &disp);
    if (foreign_name) {
      fprintf(dbgout, " at %s + %d\n", foreign_name, disp);
    }
  }

  if (xp) {
    if (why > debug_entry_exception) {
      debug_identify_exception(xp, info, why);
    }
    debug_identify_function(xp, info);
  }
  if (lisp_global(BATCH_FLAG)) {
    in_postmortem = true;
#ifdef WINDOWS
    fprintf(dbgout, "Current Process Id %d\n", (int)GetCurrentProcessId());
#else
    fprintf(dbgout, "Main thread pid %d\n", main_thread_pid);
#endif
    debug_thread_info(xp, info, 0);
    if (xp) {
      debug_show_registers(xp, info, 0);
      debug_lisp_registers(xp, info, 0);
      debug_show_fpu(xp, info, 0);
    }
    debug_memory_areas(xp, info, 0);
    debug_show_lisp_version(xp, info, 0);
    debug_backtrace(xp, info, 0);
#ifdef ANDROID
    /* Android crashes when abort() is called */
    _exit(1);
#else
    abort();
#endif
  }

  fprintf(dbgout, "? for help\n");
  while (state == debug_continue) {
#ifdef WINDOWS
    fprintf(dbgout, "[%d] Clozure CL kernel debugger: ", (int)GetCurrentProcessId());
#else
    fprintf(dbgout, "[%d] Clozure CL kernel debugger: ", main_thread_pid);
#endif
    fflush(dbgout);             /* dbgout should be unbuffered, so this shouldn't be necessary.  But it can't hurt ... */
    state = apply_debug_command(xp, readc(), info, why);
  }
  switch (state) {
  case debug_exit_success:
    if (threads_initialized) {
      resume_other_threads(false);
    }
    return 0;
  case debug_exit_fail:
    if (threads_initialized) {
      resume_other_threads(false);
    }
    return -1;
  case debug_kill:
    terminate_lisp();
  default:
    return 0;
  }
}

void
Bug(ExceptionInformation *xp, const char *format, ...)
{
  va_list args;
  char s[512];
 
  va_start(args, format);
  vsnprintf(s, sizeof(s),format, args);
  va_end(args);
  lisp_Debugger(xp, NULL, debug_entry_bug, false, s);

}

void
FBug(ExceptionInformation *xp, const char *format, ...)
{
  va_list args;
  char s[512];
 
  va_start(args, format);
  vsnprintf(s, sizeof(s),format, args);
  va_end(args);
  lisp_Debugger(xp, NULL, debug_entry_bug, true, s);
}

void
lisp_bug(char *string)
{
  Bug(NULL, "Bug in Clozure CL system code:\n%s", string);
}

