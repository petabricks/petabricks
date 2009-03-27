// -*- C++ -*-

#ifndef _COMPUTETHREADSTACKSIZE_H_
#define _COMPUTETHREADSTACKSIZE_H_

// Define a little helper template class that we can
// use to compute the default size of a thread stack in Solaris.
//   ComputeThreadStackSize::VALUE;
//
// It's 1 MB for 32-bits, 2 MB for 64-bit platforms.


#include <stdlib.h>


template <int sizeOfSizeT>
class ComputeThreadStackSizeHelper;

template<>
class ComputeThreadStackSizeHelper<4> {
public:
  enum { VALUE = 1048576 };
};

template<>
class ComputeThreadStackSizeHelper<8> {
public:
  enum { VALUE = 2 * 1048576 };
};

class ComputeThreadStackSize {
public:
  enum { VALUE = ComputeThreadStackSizeHelper<sizeof(size_t)>::VALUE };
  enum { MASK = ~((unsigned long) VALUE - 1UL) };
};


#endif

