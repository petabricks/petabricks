// -*- C++ -*-

/*

  Heap Layers: An Extensible Memory Allocation Infrastructure
  
  Copyright (C) 2000-2004 by Emery Berger
  http://www.cs.umass.edu/~emery
  emery@cs.umass.edu
  
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

#ifndef _ALIGNEDHEAP_H_
#define _ALIGNEDHEAP_H_

#include "sassert.h"

template <int RequestedAlignment,
	  int RequestSize,
	  class SuperHeap>
class AlignedHeap : public SuperHeap {
public:
  AlignedHeap (void)
    : _remaining (0),
      _currentChunk (0),
      _currentPointer (0)
  {}

  enum { Alignment = RequestedAlignment };

  // We always ask for at least this much memory.
  enum { ChunkRequestSize = 65536 };

  inline void * malloc (size_t sz) {
    assert (sz == RequestSize);
    //    fprintf (stderr, "AlignedHeap: request = %d\n", sz);
    do {
      if (_remaining >= sz) {
	char * ptr = _currentPointer;
	_currentPointer += sz;
	_currentPointer = align (_currentPointer);
	_remaining -= (reinterpret_cast<size_t>(_currentPointer) - reinterpret_cast<size_t>(ptr));
	assert (ptr == align(ptr));
	return ptr;
      } else {
	getMoreMemory ();
      }
    } while (1);
  }

  void free (void * ) {
    // Should never be called!
    assert (0);
    abort();
  }

  static size_t getSize (void *) {
    return RequestSize;
  }

private:

  AlignedHeap (const AlignedHeap&);
  AlignedHeap& operator=(const AlignedHeap&);

  void clear (void);

  void getMoreMemory (void) {
    size_t maxSize = ChunkRequestSize;
    _currentChunk = reinterpret_cast<char *>(SuperHeap::malloc (maxSize));
    if (SuperHeap::Alignment % Alignment == 0) {
      _currentPointer = align(_currentChunk);
    } else {
      _currentPointer = _currentChunk;
    }
    _remaining = maxSize;
    _remaining -= (reinterpret_cast<size_t>(_currentPointer) - reinterpret_cast<size_t>(_currentChunk));
  }

  inline static char * align (char * buf) {
    return reinterpret_cast<char *> ((reinterpret_cast<size_t>(buf) + (Alignment-1)) & ~(Alignment-1));
  }

  size_t _remaining;
  char * _currentChunk;
  char * _currentPointer;

  HL::sassert<((Alignment & (Alignment-1)) == 0)> VerifyPowerOfTwo;

  HL::sassert<((ChunkRequestSize >= RequestedAlignment) && (ChunkRequestSize >= RequestSize))> EnsureChunkSizeIsLargeEnough;

};


#endif
