/*
 * Copyright 2011 Clozure Associates
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

#ifndef X86_UTILS_H
#define X86_UTILS_H

extern LispObj tra_function(LispObj tra);
extern int tra_offset(LispObj tra);
extern int ptr_in_area(char *p, area* a);
extern area *in_any_consing_area(LispObj thing);

static inline LispObj
function_to_function_vector(LispObj f)
{
#ifdef X8664
  return f - fulltag_function + fulltag_misc;
#else
  return f;
#endif
}

static inline int
tra_p(LispObj thing)
{
#ifdef X8664
  return tag_of(thing) == tag_tra;
#else
  return fulltag_of(thing) == fulltag_tra;
#endif
}

static inline int
functionp(LispObj f)
{
#ifdef X8664
  return fulltag_of(f) == fulltag_function;
#else
  return fulltag_of(f) == fulltag_misc &&
    header_subtag(header_of(f)) == subtag_function;
#endif
}

#endif
