// -*- C++ -*-

/*

  Heap Layers: An Extensible Memory Allocation Infrastructure
  
  Copyright (C) 2000-2006 by Emery Berger
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

#ifndef _HOARD_H_
#define _HOARD_H_

#include "hldefines.h"

#define SUPERBLOCK_SIZE 65536

#define EMPTINESS_CLASSES 8


// Hoard-specific Heap Layers

#include "check.h"
#include "superblock.h"
#include "fixedrequestheap.h"
#include "ownerheader.h"
#include "hoardmanager.h"
#include "segregatedheap.h"
#include "addheaderheap.h"
#include "semilockedheap.h"
#include "alignedheap.h"
#include "threadpoolheap.h"
#include "noheader.h"
#include "addlock.h"
#include "redirectfree.h"
#include "ignoreinvalidfree.h"
#include "addlocksuperblockheader.h"
#include "conformantheap.h"
#include "hoardsuperblock.h"
#include "lockmallocheap.h"
#include "alignedsuperblockheap.h"
#include "alignedmmap.h"
#include "globalheap.h"

// Generic Heap Layers

#include "ansiwrapper.h"
#include "debugheap.h"
#include "spinlock.h"
#include "lockedheap.h"
#include "spinlock.h"
#include "winlock.h"
#include "bins4k.h"
#include "bins8k.h"
#include "bins16k.h"
#include "bins64k.h"
#include "oneheap.h"
#include "freelistheap.h"
#include "nullheap.h"
#include "threadheap.h"
#include "hybridheap.h"
#include "posixlock.h"
#include "spinlock.h"


#if defined(_WIN32)
typedef HL::WinLockType TheLockType;
#else
typedef HL::SpinLockType TheLockType;
#endif


typedef GlobalHeap<SUPERBLOCK_SIZE, EMPTINESS_CLASSES, TheLockType> TheGlobalHeap;

class hoardThresholdFunctionClass {
public:
  inline static bool function (int u, int a, size_t objSize) {
    /*
      Returns 1 iff we've crossed the emptiness threshold:

      U < A - 2S   &&   U < 7/8 A

     */
    bool r = ((8 * u) < (7 * a)) && ((u < a - (2 * SUPERBLOCK_SIZE) / (int) objSize));
    return r;
  }
};


class SmallHeap;

typedef HoardSuperblock<TheLockType, SUPERBLOCK_SIZE, SmallHeap> SmallSuperblockType;

// The heap that manages small objects.
class SmallHeap : 
  public ConformantHeap<
HoardManager<AlignedSuperblockHeap<TheLockType, SUPERBLOCK_SIZE>,
                      TheGlobalHeap,
                      SmallSuperblockType,
                      SUPERBLOCK_SIZE,
                      EMPTINESS_CLASSES,
                      TheLockType,
                      hoardThresholdFunctionClass,
                      SmallHeap> > {
public:

  void * malloc (size_t sz) {
    return SuperHeap::malloc (sz);
  }

  void put (SuperHeap::SuperblockType * s, size_t sz) {
    //    printf ("putting %x\n", (void *) s);
    SuperHeap::put (s, sz);
  }

  SmallSuperblockType * get (size_t sz, SmallHeap * dest) {
    SmallSuperblockType * s = SuperHeap::get (sz, dest);
    // printf ("get %x\n", (void *) s);
    return s;
  }


};

class BigHeap;

typedef HoardSuperblock<TheLockType, SUPERBLOCK_SIZE, BigHeap> BigSuperblockType;


// The heap that manages large objects.
class BigHeap :
  public ConformantHeap<HL::LockedHeap<TheLockType,
                        AddHeaderHeap<BigSuperblockType,
                                      SUPERBLOCK_SIZE,
                                      AlignedMmap<SUPERBLOCK_SIZE> > > >
{
  HL::sassert<(((int) Alignment % SUPERBLOCK_SIZE) == 0)> VerifyAlignment;
};


enum { BigObjectSize = HL::bins<SmallSuperblockType::Header, SUPERBLOCK_SIZE>::BIG_OBJECT };

class PerThreadHoardHeap :
  public RedirectFree<LockMallocHeap<SmallHeap>,
                      SmallSuperblockType,
                      SUPERBLOCK_SIZE> {};

template <int N, int NH>
class HoardHeap :
  public IgnoreInvalidFree<
       HL::ANSIWrapper<
       HL::HybridHeap<BigObjectSize,
       ThreadPoolHeap<N, NH, PerThreadHoardHeap>,
       BigHeap> > >
{
public:

  enum { BIG_OBJECT = BigObjectSize };

  HL::sassert<sizeof(BigSuperblockType::Header) == sizeof(SmallSuperblockType::Header)> ensureSameSizeHeaders;
};


#endif
