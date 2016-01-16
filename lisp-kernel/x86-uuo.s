/*
 * Copyright 2005-2009 Clozure Associates
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

define(`uuo_error_too_few_args',`
        int `$'0xc0
')

define(`uuo_error_too_many_args',`
        int `$'0xc1
')

define(`uuo_error_wrong_number_of_args',`
        int `$'0xc2
')


define(`uuo_error_gc_trap',`
        int `$'0xc4
')                        


define(`uuo_error_debug_trap',`
        int `$'0xca
')                        
        
                                        
/* If we're allocating a CONS, the tcr's save_allocptr slot will be */
/* tagged as a cons.  Otherwise, it'll be tagged as fulltag_misc, */
/* and we have to look at the immediate registers to determine what's */
/* being allocated. */
define(`uuo_alloc',`
	int `$'0xc5
')
				
define(`uuo_error_not_callable',`
        int `$'0xc6
')


define(`xuuo',`
	ud2a
	.byte $1
')
	
define(`tlb_too_small',`
	xuuo(1)
')

define(`interrupt_now',`
	xuuo(2)
')		

define(`suspend_now',`
	xuuo(3)
')		

define(`uuo_error_reg_not_fixnum',`
	int `$'0xf0|$1
')	
	
define(`uuo_error_reg_not_list',`
	int `$'0xe0|$1
')

define(`uuo_error_reg_not_tag',`
	int `$'0xd0|$1
	.byte $2
')			

define(`uuo_error_reg_not_type',`
	int `$'0xb0|$1
	.byte $2
')

define(`uuo_error_reg_not_fixnum',`
	int `$'0xf0|$1
')	
		
define(`uuo_error_reg_unbound',`
	int `$'0x90|$1
')	

define(`uuo_error_vector_bounds',`
	int `$'0xc8
	.byte ($1<<4)|($2)
')	

define(`uuo_error_array_bounds',`
	int `$'0xcb
	.byte ($1<<4)|($2)
')	

