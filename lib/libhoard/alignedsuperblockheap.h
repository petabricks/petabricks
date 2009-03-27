// -*- C++ -*-

#ifndef _ALIGNEDSUPERBLOCKHEAP_H_
#define _ALIGNEDSUPERBLOCKHEAP_H_

#include "mmapheap.h"
#include "sassert.h"

#include "conformantheap.h"
#include "lockedheap.h"
#include "fixedrequestheap.h"

// Always requests aligned superblocks.
#include "alignedmmap.h"

template <class TheLockType,
	  size_t SuperblockSize>
class AlignedSuperblockHeapHelper :
  public ConformantHeap<HL::LockedHeap<TheLockType,
				       FixedRequestHeap<SuperblockSize, 
		   AlignedMmap<SuperblockSize> > > > {};


template <class TheLockType,
	  size_t SuperblockSize>
class AlignedSuperblockHeap :
  public AlignedSuperblockHeapHelper<TheLockType, SuperblockSize> {

  HL::sassert<(AlignedSuperblockHeapHelper<TheLockType, SuperblockSize>::Alignment % SuperblockSize == 0)> EnsureProperAlignment;

};

#endif
