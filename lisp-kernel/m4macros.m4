changecom(`/*',`*/')



/*   Copyright (C) 1994-2001 Digitool, Inc  */
/*   Copyright (c) 2009 Clozure Associates */
/*   This file is part of Clozure CL.    */

/*   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public  */
/*   License , known as the LLGPL and distributed with Clozure CL as the  */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,  */
/*   which is distributed with Clozure CL as the file "LGPL".  Where these  */
/*   conflict, the preamble takes precedence.    */

/*   Clozure CL is referenced in the preamble as the "LIBRARY."  */

/*   The LLGPL is also available online at  */
/*   http://opensource.franz.com/preamble.html  */



/*  BSD debugging information (line numbers, etc) is a little different  */
/*  from ELF/SVr4 debugging information.  There are probably lots more  */
/*  differences, but this helps us to distinguish between what LinuxPPC  */
/*  (ELF/SVr4) wants and what Darwin(BSD) wants.  */


define(`BSDstabs',`1')
define(`ELFstabs',`2')
define(`COFFstabs',`3')
undefine(`EABI')
undefine(`POWEROPENABI')
undefine(`rTOC')


ifdef(`DARWIN',`define(`SYSstabs',`BSDstabs')
		define(`DarwinAssembler',`')
                define(`CNamesNeedUnderscores',`')
	        define(`LocalLabelPrefix',`L')
	        define(`StartTextLabel',`Ltext0')
	        define(`EndTextLabel',`Letext')
                ifdef(`PPC',`
		define(`POWEROPENABI',`')')
                ifdef(`X86',`
                define(`SYSCALL_SETS_CARRY_ON_ERROR',`')
		define(`SSE2_MATH_LIB',`')')
')

ifdef(`LINUX',`define(`SYSstabs',`ELFstabs')
	       define(`HaveWeakSymbols',`')
	       define(`LocalLabelPrefix',`.L')
	       define(`StartTextLabel',`.Ltext0')
	       define(`EndTextLabel',`.Letext')
               ifdef(`PPC64',`
               define(`POWEROPENABI',`')
               define(`rTOC',`r2')', `
	       define(`EABI',`')')')

ifdef(`FREEBSD',`define(`SYSstabs',`ELFstabs')
	       define(`HaveWeakSymbols',`')
	       define(`LocalLabelPrefix',`.L')
	       define(`StartTextLabel',`.Ltext0')
	       define(`EndTextLabel',`.Letext')'
                ifdef(`X86',`
                define(`SYSCALL_SETS_CARRY_ON_ERROR',`')')
)

ifdef(`SOLARIS',`define(`SYSstabs',`ELFstabs')
	       define(`HaveWeakSymbols',`')
	       define(`LocalLabelPrefix',`.L')
	       define(`StartTextLabel',`.Ltext0')
	       define(`EndTextLabel',`.Letext')')

ifdef(`WINDOWS',`define(`SYSstabs',`COFFstabs')
        ifdef(`WIN_32',`define(`CNamesNeedUnderscores',`')')
               define(`LocalLabelPrefix',`L')
	       define(`StartTextLabel',`Ltext0')
	       define(`EndTextLabel',`Letext')')


/*  Names exported to (or imported from) C may need leading underscores.  */
/*  Still.  After all these years.  Why ?  */

define(`C',`ifdef(`CNamesNeedUnderscores',``_'$1',`$1')')

define(`_linecounter_',0)

define(`_emit_BSD_source_line_stab',`
ifdef(`X86',`
# __line__ "__file__" 1',`
	.stabd 68,0,$1
')')


/*  We don't really do "weak importing" of symbols from a separate  */
/*  subprims library anymore; if we ever do and the OS supports it,  */
/*  here's how to say that we want it ...  */

define(`WEAK',`ifdef(`HaveWeakSymbols',`
	.weak $1
',`
	.globl $1
')')

define(`_emit_ELF_source_line_stab',`
  define(`_linecounter_',incr(_linecounter_))
	.stabn 68,0,$1,`.LM'_linecounter_`-'__func_name
`.LM'_linecounter_:
')

define(`_emit_COFF_source_line_stab',`
        _emit_ELF_source_line_stab($1)
')


define(`emit_source_line_stab',`
	ifelse(eval(SYSstabs),
             eval(BSDstabs),
  	      `_emit_BSD_source_line_stab($1)',
              eval(SYSstabs),
              eval(ELFstabs),
              `_emit_ELF_source_line_stab($1)',
              `_emit_COFF_source_line_stab($1)')')






/*  Assemble a reference to the high half of a 32-bit constant,  */
/*  possibly adjusted for sign-extension of thw low half.  */


define(`HA',`ifdef(`DARWIN',`ha16($1)',`$1@ha')')

 
/*  Likewise for the low half, and for the high half without  */
/*  concern for sign-extension of the low half.  */

define(`LO',`ifdef(`DARWIN',`lo16($1)',`$1@l')')
define(`HI',`ifdef(`DARWIN',`hi16($1)',`$1@hi')')

/*  Note that m4 macros that could be expanded in the .text segment  */
/*  need to advertise the current line number after they have finished  */
/*  expanding.  That shouldn't be too onerous, if only because there  */
/*  should not be too many of them.  */


define(`N_FUN',36)
define(`N_SO',100)

/*    I wish that there was a less-dumb way of doing this.  */

define(`pwd0',esyscmd(`/bin/pwd'))
define(`__pwd__',substr(pwd0,0,decr(len(pwd0)))`/')

/*   _beginfile() -- gets line/file in synch, generates N_SO for file,  */
/*   starts .text section  */


define(`_beginfile',`
	.stabs "__pwd__",N_SO,0,0,StartTextLabel()
	.stabs "__file__",N_SO,0,0,StartTextLabel()
ifdef(`PPC64',`
ifdef(`DARWIN',`
        .machine ppc64
')')
	.text
StartTextLabel():
# __line__ "__file__"
')

define(`_endfile',`
	.stabs "",N_SO,0,0,EndTextLabel()
EndTextLabel():
# __line__
')

define(`_startfn',`define(`__func_name',$1)
# __line__
	ifelse(eval(SYSstabs),eval(ELFstabs),`
	.type $1,ifdef(`ARM',%function,@function)
')

$1:
ifdef(`WINDOWS',`
	.def	$1;	.scl	2;	.type	32;	.endef
',`
        .stabd 68,0,__line__
')
	.stabs "$1:F1",36,0,__line__,$1
	.set func_start,$1
# __line__ "__file__" 1 ')



define(`_exportfn',`
	.globl $1
	_startfn($1)
ifdef(`PPC64',`
ifdef(`LINUX',`
        .global `.'$1
`.'$1:
')')
# __line__
')


define(`_endfn',`
LocalLabelPrefix`'__func_name`999':
ifdef(`WINDOWS',`
',`
	.stabs "",36,0,0,LocalLabelPrefix`'__func_name`999'-__func_name
	.line __line__
	ifelse(eval(SYSstabs),eval(ELFstabs),`
        .size __func_name,LocalLabelPrefix`'__func_name`999'-__func_name
')
')
	undefine(`__func_name')
')


/* _struct(name,start_offset)  */
/*   This just generates a bunch of assembler equates; m4  */
/*   doesn't remember much of it ..  */

define(`_struct', `define(`__struct_name',$1)
 define(`_struct_org_name', _$1_org) 
 define(`_struct_base_name', _$1_base)
	.set _struct_org_name,$2
	.set _struct_base_name,_struct_org_name
 ifelse($3,`',`
  undefine(`_struct_fixed_size_name')
  ',`
  define(`_struct_fixed_size_name', _$1_fixed_size)
	.set _struct_fixed_size_name,$3
  ')
')

define(`_struct_pad',`
	.set _struct_org_name,_struct_org_name + $1
')
 
define(`_struct_label',`
	.set __struct_name`.'$1, _struct_org_name
')

/*  _field(name,size)   */
define(`_field',`_struct_label($1) _struct_pad($2)')

define(`_halfword', `_field($1, 2)')
define(`_word', `_field($1, 4)')
define(`_dword',`_field($1, 8)')
define(`_node', `_field($1, node_size)')

define(`_ends',`ifdef(`_struct_fixed_size_name',`
	.set  __struct_name`.size',_struct_fixed_size_name
	',`
	.set  __struct_name`.size', _struct_org_name-_struct_base_name
	')
')


/*   Lisp fixed-size objects always have a 1-word header  */
/*   and are always accessed from a "fulltag_misc"-tagged pointer.  */
/*   We also want to define STRUCT_NAME.element_count for each  */
/*   such object.  */


define(`_structf',`
	_struct($1,ifelse($2,`',-misc_bias,$2))
        _node(header)
')

define(`_endstructf',`
	.set __struct_name.`element_count',((_struct_org_name-node_size)-_struct_base_name)/node_size
	_ends
')


define(`__',`emit_source_line_stab(__line__)
	$@
	')

define(`__local_label_counter__',0)
define(`__macro_label_counter__',0)

define(`new_local_labels',
  `define(`__local_label_counter__',incr(__local_label_counter__))')

define(`new_macro_labels',
  `define(`__macro_label_counter__',incr(__macro_label_counter__))')

define(`_local_label',`LocalLabelPrefix()`'$1')

define(`local_label',`_local_label($1`'__local_label_counter__)')

define(`macro_label',`_local_label($1`'__macro_label_counter__)')


/* The Darwin assembler doesn't seem to support .ifdef/.ifndef, but  */
/* does understand .if.    */
/* Note that using M4's own ifdef is certainly possible, but it's  */
/* hard to generate source line information when doing so.  */

  
define(`__ifdef',`ifdef(`$1',`.if 1',`.if 0')')
define(`__ifndef',`ifdef(`$1',`.if 0',`.if 1')')
define(`__else',`.else')
define(`__endif',`.endif')
define(`__if',`.if $1')

define(`equate_if_defined',`ifdef($1,`
`$1' = 1
',`
`$1' = 0
')')

equate_if_defined(`DARWIN')
equate_if_defined(`LINUX')
equate_if_defined(`FREEBSD')
equate_if_defined(`SOLARIS')
equate_if_defined(`WIN_64')
equate_if_defined(`PPC64')
equate_if_defined(`X8664')
equate_if_defined(`WIN_32')
equate_if_defined(`WINDOWS')

equate_if_defined(`HAVE_TLS')

equate_if_defined(`TCR_IN_GPR')

/* Well, so much for that. Maybe this will go away soon ? */
equate_if_defined(`WIN32_ES_HACK')
equate_if_defined(`SYSCALL_SETS_CARRY_ON_ERROR')



/* We use (more-or-less) a PowerOpen C frame, except on LinuxPPC32  */

define(`USE_POWEROPEN_C_FRAME',`')
undefine(`USE_EABI_C_FRAME')

ifdef(`LINUX',`
ifdef(`PPC64',`',`
define(`USE_EABI_C_FRAME',`')
undefine(`USE_POWEROPEN_C_FRAME')
')')




