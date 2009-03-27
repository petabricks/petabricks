#include <new>

using namespace std;

#include "hldefines.h"

#include "bins16k.h"
#include "mallocheap.h"
#include "hoardsuperblock.h"
#include "alignedsuperblockheap.h"
#include "spinlock.h"

#include "tlab.h"

// Force the use of atomic operations.
volatile int anyThreadCreated = 1;

using namespace HL;

enum { SUPERBLOCK_SIZE = 16384 };

class NoLock {
public:
  void lock (void) {}
  void unlock (void) {}
};


typedef HoardSuperblock<NoLock, SUPERBLOCK_SIZE, mallocHeap> SType;

enum { NumBins = bins<SType::Header, SUPERBLOCK_SIZE>::NUM_BINS };

class ParentHeap {
public:
  ParentHeap (void)
    : s (NULL)
  {
    //    head.setNext (&head);
    // head.setPrev (&head);
  }

  inline void * malloc (size_t sz) {
    // Get memory from our superblock.
    while (1) {
      if (s == NULL) {
	// Look for a superblock.
	if (1) { // head.getNext() == &head) {
	  // Get another superblock.
	  void * p = ASH.malloc(1);
	  if (p == NULL) {
	    printf ("WHAT?\n");
	    return NULL;
	  }
	  s = new (p) SType (sz);
	} else {
#if 0
	  s = head.getNext();
	  head.setNext (s->getNext());
	  head.getNext()->setPrev (&head);
#endif
	}
	s->setOwner (NULL);
      }
      void * ptr = s->malloc (sz);
      if (ptr != NULL) {
	return ptr;
      }
#if 0
      // This superblock is out of memory.
      // Remove it from the list.
      SType * prev = s->getPrev();
      SType * next = s->getNext();
      prev->setNext (next);
      next->setPrev (prev);
#endif
      s = NULL;
    }
  }

  inline void free (void * ptr) {
    // Free directly to the right superblock.
    SType * s = getSuperblock (ptr);
    s->free (ptr);
    if (0) { //(s->getPrev() == NULL) && (s->getNext() == NULL)) {
#if 0
      // Not on the list yet. Add it.
      s->setPrev (&head);
      s->setNext (head.getNext());
      head.setNext (s);
      head.getNext()->setPrev (s);
#endif
    }
  }

private:

  //  SType head;

  static inline SType * getSuperblock (void * ptr) {
    SType * s = reinterpret_cast<SType *>(reinterpret_cast<size_t>(ptr) & ~(SUPERBLOCK_SIZE-1));
    assert (s->isValidSuperblock());
    return s;
  }
 
  AlignedSuperblockHeap<NoLock, SUPERBLOCK_SIZE> ASH;
  SType * s;
};


#if 1

class ThisHeap : public ThreadLocalAllocationBuffer<NumBins,
		 bins<SType::Header, SUPERBLOCK_SIZE>::getSizeClass,
		 bins<SType::Header, SUPERBLOCK_SIZE>::getClassSize,
		 2000,
		 SType,
		 SUPERBLOCK_SIZE,
		 LockedHeap<SpinLockType, ParentHeap> > {};

#else

class ThisHeap : public LockedHeap<SpinLockType, ParentHeap> {};

#endif

int main (int argc, char * argv[]) {
  ThisHeap h;
  enum { MAGIC_NUMBER = 2000 };

  void * buf[MAGIC_NUMBER];
  int i;
  int j;

  for (j = 0; j < 1000; j++) {
    for (i = 0; i < MAGIC_NUMBER; i++) {
      buf[i] = h.malloc (16);
    }
    for (i = 0; i < MAGIC_NUMBER; i++) {
      h.free (buf[i]);
    }
  }

  return 0;
}
