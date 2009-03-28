/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#ifndef JALIBJASM_H
#define JALIBJASM_H

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif


namespace jalib {

typedef volatile long AtomicT;

#if defined(__i386__) || defined(__x86_64__)
/**
 * Thread safe add, returns new value
 */
template<long v> long atomicAdd(AtomicT *p){
  long r;
  asm volatile ("lock; xadd %0, %1" : "=r"(r), "=m"(*p) : "0"(v), "m"(*p) : "memory");
  return r+v;
}

inline void atomicIncrement(AtomicT *p){ atomicAdd<1>(p); }
inline void atomicDecrement(AtomicT *p){ atomicAdd<-1>(p); }
inline long atomicIncrementReturn(AtomicT *p){ return atomicAdd<1>(p); }
inline long atomicDecrementReturn(AtomicT *p){ return atomicAdd<-1>(p); }

/**
 * Break into debugger
 */
inline void Breakpoint(){
  asm volatile ( "int3" );
}


/** 
 * Returns the number of clock cycles that have passed since the machine
 * booted up.
 */
inline uint64_t ClockCyclesSinceBoot()
{
  uint32_t hi, lo;
  asm volatile ("rdtsc" : "=a"(lo), "=d"(hi));
  return ((uint64_t ) lo) | (((uint64_t) hi) << 32);
}

#elif defined(__sparc__)

inline bool
cas(volatile long *m, long old_val, long new_val)
{
	asm volatile("cas [%2], %3, %0\n\t"
			     : "=&r" (new_val)
			     : "0" (new_val), "r" (m), "r" (old_val)
			     : "memory");

	return new_val == old_val;
}

template<long v> long atomicAdd(AtomicT *p)
{
  long new_val, old_val;
  do {
    old_val = *p;
    new_val = old_val + v;
  } while (!cas(p, old_val, new_val));
  
  return new_val;
}

inline void atomicIncrement(AtomicT *p){ atomicAdd<1>(p); }
inline void atomicDecrement(AtomicT *p){ atomicAdd<-1>(p); }
inline long atomicIncrementReturn(AtomicT *p){ return atomicAdd<1>(p); }
inline long atomicDecrementReturn(AtomicT *p){ return atomicAdd<-1>(p); }

/**
 * Break into debugger
 */
inline void Breakpoint(){
  asm volatile ("ta 0x70");
}


/** 
 * Returns the number of clock cycles that have passed since the machine
 * booted up.
 */
inline uint64_t ClockCyclesSinceBoot()
{
  uint64_t t;
  asm volatile ("rd %%tick, %0" : "=r"(t));
  return t;

}

#else

int Need_To_Port_ASM_Functions[-1];
// Port these
template<long v> long atomicAdd(volatile long *p);
inline void Breakpoint();
inline uint64_t ClockCyclesSinceBoot();

#endif
}

#endif 
