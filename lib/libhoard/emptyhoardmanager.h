#ifndef _EMPTYHOARDMANAGER_H_
#define _EMPTYHOARDMANAGER_H_

#include "basehoardmanager.h"
#include "sassert.h"

template <class SuperblockType_,
	  size_t SuperblockSize>
class EmptyHoardManager : public BaseHoardManager<SuperblockType_, SuperblockSize> {
public:

  EmptyHoardManager (void)
    : _magic (0x1d2d3d40)
    {}

  int isValid (void) const {
    return (_magic == 0x1d2d3d40);
  }

  typedef SuperblockType_ SuperblockType;

  void free (void *) { abort(); }
  void lock (void) {}
  void unlock (void) {}

  SuperblockType * get (size_t, EmptyHoardManager *) { abort(); return NULL; }
  void put (SuperblockType *, size_t) { abort(); }

#if 0
  void * malloc (size_t) { return NULL; }
#endif

private:

  unsigned long _magic;

  /// Ensure that the superblock is exactly <EM>SuperblockSize</EM> bytes in size.
  HL::sassert<(sizeof(SuperblockType) == SuperblockSize)> verifySize;
};


#endif
