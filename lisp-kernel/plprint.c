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

#include "lispdcmd.h"


void
plprint(ExceptionInformation *xp, LispObj obj)
{
  if (lisp_nil == (LispObj) NULL) {
    fprintf(dbgout,"can't find lisp NIL; lisp process not active process ?\n");
  } else {
    Dprintf("\n%s", print_lisp_object(obj));
  }
}

