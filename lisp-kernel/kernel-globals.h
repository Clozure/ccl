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

#ifndef __kernel_globals__
#define __kernel_globals__
#include "area.h"


extern area *nilreg_area, *tenured_area, *g2_area, *g1_area, *managed_static_area, *readonly_area, *static_cons_area;
extern area *all_areas;
extern int cache_block_size;







#endif /* __kernel_globals__ */
