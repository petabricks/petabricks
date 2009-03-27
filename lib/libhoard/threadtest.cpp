// -*- C++ -*-

/*
  The Hoard Multiprocessor Memory Allocator
  www.hoard.org

  Author: Emery Berger, http://www.cs.umass.edu/~emery
 
  Copyright (c) 1998-2004, The University of Texas at Austin

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/


/**
 * @file threadtest.cpp
 *
 * This program does nothing but generate a number of kernel threads
 * that allocate and free memory, with a variable
 * amount of "work" (i.e. cycle wasting) in between.
 *
 * compile as follows:
 *  g++ -I../../heaplayers -DNDEBUG -O3 -D_REENTRANT=1 threadtest.cpp -lpthread -o threadtest
 *
 **/



#ifndef _REENTRANT
#define _REENTRANT
#endif

#include <iostream>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "timer.h"
#include "fred.h"

int niterations = 50;	// Default number of iterations.
int nobjects = 30000;  // Default number of objects.
int nthreads = 1;	// Default number of threads.
int work = 0;		// Default number of loop iterations.
int size = 1;


class Foo {
public:
  Foo (void)
    : x (14),
      y (29)
    {}

  int x;
  int y;
};


#if defined(_WIN32)
extern "C" void * __cdecl worker (void *)
#else
extern "C" void * worker (void *)
#endif
{
  int i, j;
  Foo ** a;
  a = new Foo * [nobjects / nthreads];

  for (i = 0; i < nobjects / nthreads; i++) {
    a[i] = new Foo[size];
  }

  for (j = 0; j < niterations; j++) {

    for (i = 0; i < (nobjects / nthreads) / 1; i ++) {
      a[i] = new Foo[size];
      for (volatile int d = 0; d < work; d++) {
	volatile int f = 1;
	f = f + f;
	f = f * f;
	f = f + f;
	f = f * f;
      }
      a[i]->x = 1;
      //      assert (a[i]);
    }
    
    for (i = 0; i < (nobjects / nthreads) / 1; i ++) {
      delete [] a[i];
      for (volatile int d = 0; d < work; d++) {
	volatile int f = 1;
	f = f + f;
	f = f * f;
	f = f + f;
	f = f * f;
      }
    }
  }

  delete [] a;

  return NULL;
}

#if defined(__sgi)
#include <ulocks.h>
#endif

int main (int argc, char * argv[])
{
  HL::Fred * threads;
  
  if (argc >= 2) {
    nthreads = atoi(argv[1]);
  }

  if (argc >= 3) {
    niterations = atoi(argv[2]);
  }

  if (argc >= 4) {
    nobjects = atoi(argv[3]);
  }

  if (argc >= 5) {
    work = atoi(argv[4]);
  }

  if (argc >= 6) {
    size = atoi(argv[5]);
  }

  printf ("Running threadtest for %d threads, %d iterations, %d objects, %d work and %d size...\n", nthreads, niterations, nobjects, work, size);

  threads = new HL::Fred[nthreads];
  //  hoardSetConcurrency (nthreads);

  HL::Timer t;
  //Timer t;

  t.start ();

  int i;
  for (i = 0; i < nthreads; i++) {
    threads[i].create (worker, NULL);
  }

  for (i = 0; i < nthreads; i++) {
    threads[i].join();
  }
  t.stop ();

  printf( "Time elapsed = %f\n", (double) t);

  delete [] threads;

  return 0;
}
