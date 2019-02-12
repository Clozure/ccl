/*
* From https://www.codefull.org/2015/12/systime-h-replacement-for-windows/
* TODO: this project is LGPL, and this function is original from:
* https://code.google.com/p/madp-win/source/browse/src/include/sys/times.h?r=4ef496e2071896b295262c89eb36a8b3d7656bae
* but the link seems dead and can't confirm original license, it's safer
* to find a replacement, like a similar one mingw-w64 project is public
* domain, but has a few include need to be fixed.
*/
#ifndef _TIME_H
#define _TIME_H

#ifdef _WIN32
#include <sys/timeb.h>
#include <sys/types.h>
#include <winsock2.h>

int gettimeofday(struct timeval* t, void* timezone);

// from linux's sys/times.h

//#include <features.h>

#define __need_clock_t
#include <time.h>


/* Structure describing CPU time used by a process and its children.  */
struct tms
{
	clock_t tms_utime;          /* User CPU time.  */
	clock_t tms_stime;          /* System CPU time.  */

	clock_t tms_cutime;         /* User CPU time of dead children.  */
	clock_t tms_cstime;         /* System CPU time of dead children.  */
};

/* Store the CPU time used by this process and all its
dead children (and their dead children) in BUFFER.
Return the elapsed real time, or (clock_t) -1 for errors.
All times are in CLK_TCKths of a second.  */
clock_t times(struct tms *__buffer);

typedef long long suseconds_t;

#endif
#endif