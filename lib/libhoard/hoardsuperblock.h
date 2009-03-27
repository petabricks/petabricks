#ifndef _HOARDSUPERBLOCK_H_
#define _HOARDSUPERBLOCK_H_

#include <assert.h>

#include "superblock.h"
#include "addlocksuperblockheader.h"
#include "noheader.h"


template <class TheLockType,
	  int SuperblockSize,
	  class HeapType_>
class HoardSuperblock : public Superblock<AddLockSuperblockHeader<TheLockType, NoHeader, SuperblockSize, HoardSuperblock<TheLockType, SuperblockSize, HeapType_>, HeapType_>, SuperblockSize> {
public:
  typedef HeapType_ HeapType;

  typedef Superblock<AddLockSuperblockHeader<TheLockType, NoHeader, SuperblockSize, HoardSuperblock<TheLockType, SuperblockSize, HeapType_>, HeapType_>, SuperblockSize> SuperHeap;

  typedef typename SuperHeap::Header Header;
  typedef HoardSuperblock<TheLockType, SuperblockSize, HeapType_> SuperblockType;

  HoardSuperblock (size_t sz)
    : SuperHeap (sz)
  {
  }
};

#endif
