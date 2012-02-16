/*****************************************************************************
 *  Copyright (C) 2008-2011 Massachusetts Institute of Technology            *
 *                                                                           *
 *  Permission is hereby granted, free of charge, to any person obtaining    *
 *  a copy of this software and associated documentation files (the          *
 *  "Software"), to deal in the Software without restriction, including      *
 *  without limitation the rights to use, copy, modify, merge, publish,      *
 *  distribute, sublicense, and/or sell copies of the Software, and to       *
 *  permit persons to whom the Software is furnished to do so, subject       *
 *  to the following conditions:                                             *
 *                                                                           *
 *  The above copyright notice and this permission notice shall be included  *
 *  in all copies or substantial portions of the Software.                   *
 *                                                                           *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY                *
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE               *
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      *
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   *
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   *
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    *
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE           *
 *                                                                           *
 *  This source code is part of the PetaBricks project:                      *
 *    http://projects.csail.mit.edu/petabricks/                              *
 *                                                                           *
 *****************************************************************************/

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

#define __PADDING(n, l) char _padding_ ## l [ n ] ;
#define _PADDING(n, l) __PADDING(n, l)
#define PADDING(n) _PADDING(n, __LINE__)

#define USE(x) (void)(x)

#ifdef HAVE_BUILTIN_EXPECT
#define LIKELY(x)       __builtin_expect((x),1)
#define UNLIKELY(x)     __builtin_expect((x),0)
#else
#define LIKELY(x)       (x)
#define UNLIKELY(x)     (x)
#endif

namespace jalib {


typedef volatile long AtomicT;

INLINE ATTRIBUTE(cold) void cold(){}

#if defined(__i386__) || defined(__x86_64__)
/**
 * Thread safe add, returns new value
 */
template<long v> long atomicAdd(AtomicT *p){
  long r;
  asm volatile ("lock; xadd %0, %1" : "=r"(r), "=m"(*p) : "0"(v), "m"(*p) : "memory");
  return r+v;
}

inline void atomicIncrement(AtomicT *p){
  asm volatile ("lock; incl %0" : "=m"(*p) : : "memory");
}

inline void atomicDecrement(AtomicT *p){
  asm volatile ("lock; decl %0" : "=m"(*p) : : "memory");
}

inline long atomicIncrementReturn(AtomicT *p){ return atomicAdd<1>(p); }
inline long atomicDecrementReturn(AtomicT *p){ return atomicAdd<-1>(p); }

/**
 * Break into debugger
 */
inline void Breakpoint(){
  asm volatile ( "int3" );
}

inline void loadFence() {
  asm __volatile__ ("lfence" : : : "memory");
}

inline void memFence() {
  asm __volatile__ ("mfence" : : : "memory");
}

inline void staticMemFence(void)
{
  asm __volatile__ ("":::"memory");
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

/**
 * Raw compare and swap, specialized for each number of bits
 * Returns value of *ptr before CAS
 */
template < int bytes >
long _casBytes(volatile long* ptr, long oldVal, long newVal);
//specialized template for 1 byte CAS
template <>
inline long _casBytes<1>(volatile long* ptr, long oldVal, long newVal){
  long prev;
  asm volatile("cmpxchgb %b1,%2" : "=a"(prev) : "q"(newVal), "m"(*ptr), "0"(oldVal) : "memory");
  return prev;
}
//specialized template for 2 byte CAS
template <>
inline long _casBytes<2>(volatile long* ptr, long oldVal, long newVal){
  long prev;
  asm volatile("cmpxchgw %w1,%2" : "=a"(prev) : "r"(newVal), "m"(*ptr), "0"(oldVal) : "memory");
  return prev;
}
//specialized template for 4 byte CAS
template <>
inline long _casBytes<4>(volatile long* ptr, long oldVal, long newVal){
  long prev;
  asm volatile("cmpxchgl %k1,%2" : "=a"(prev) : "r"(newVal), "m"(*ptr), "0"(oldVal) : "memory");
  return prev;
}
//specialized template for 8 byte CAS
template <>
inline long _casBytes<8>(volatile long* ptr, long oldVal, long newVal){
  long prev;
  asm volatile("cmpxchgq %1,%2" : "=a"(prev) : "r"(newVal), "m"(*ptr), "0"(oldVal) : "memory");
  return prev;
}

/**
 * Properly type checked CAS
 * Returns true if the newVal was put in place
 */
template<typename T>
inline bool compareAndSwap(volatile T* ptr, T oldVal, T newVal){
  return (long)oldVal==_casBytes<sizeof(T)>((volatile long*)ptr,(long)oldVal,(long)newVal);
}

inline long fetchAndStore(long *p, long val)
{
  long ret;
  asm volatile("lock; xchg %0, %1" : "=r"(ret), "=m"(*p) : "0"(val), "m"(*p) : "memory");
  return ret;
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

inline void loadFence() {
  asm __volatile__ ("membar #StoreLoad" : : : "memory");
}

inline void memFence() {
  asm __volatile__ ("membar #StoreLoad" : : : "memory");
}

inline void staticMemFence(void)
{
  asm __volatile__ ("":::"memory");
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
