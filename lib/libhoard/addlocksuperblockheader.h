#ifndef _ADDLOCKSUPERBLOCKHEADER_H_
#define _ADDLOCKSUPERBLOCKHEADER_H_

#include "addlock.h"
#include "ownerheader.h"


// Put a lock in the superblock header.
template <class TheLockType,
	  class Super,
	  size_t SuperblockSize,
	  class SuperblockType,
	  class HeapType>
class AddLockSuperblockHeader : 
  public OwnerHeader<AddLock<TheLockType, Super>, SuperblockType, HeapType> {
public:
  typedef OwnerHeader<AddLock<TheLockType, Super>, SuperblockType, HeapType> Header;

  inline AddLockSuperblockHeader (size_t sz)
    : Header (sz)
  {}

  // Ensure that this header is padded!
  char _pad[sizeof(double) - (sizeof(Header) % sizeof(double))];
};

#endif
