/*
 * Copyright 1994-2010 Clozure Associates
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

#define SIGNAL_FOR_PROCESS_INTERRUPT SIGPWR
#ifdef ANDROID
#define SIG_SUSPEND_THREAD SIGUSR2
#define SIG_KILL_THREAD SIGXCPU
#else
#define SIG_SUSPEND_THREAD (SIGRTMIN+6)
#define SIG_KILL_THREAD (SIGRTMIN+7)
#endif

#ifdef USE_DTRACE
#include "probes.h"
#endif
