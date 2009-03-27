// -*- C++ -*-

#ifndef _LEAHEADER_H_
#define _LEAHEADER_H_

// All of the stuff below here is adapted from Doug Lea's allocator,
// the basis of the GNU libc allocator.

class LeaHeader {
public:

  /// Return the size of an object allocated with these headers.
  static inline size_t getSize (void * ptr) {
    if (ptr != 0) {
      LeaHeader * p = mem2chunk(ptr);
      if (p->chunk_is_mmapped())
	return p->chunksize() - 2*sizeof(size_t);
      else if (p->inuse())
	return p->chunksize() - sizeof(size_t);
    }
    return 0;
  }

private:

  size_t _prev_size;
  size_t _size;

  // Allocated space begins here.
  
  LeaHeader * fd;
  LeaHeader * bk;
  
  enum { PREV_INUSE = 0x1 };
  enum { IS_MMAPPED = 0x2 };
  enum { NON_MAIN_ARENA = 0x4 };
  enum { SIZE_BITS = (PREV_INUSE | IS_MMAPPED | NON_MAIN_ARENA) };
  
  enum { OVERHEAD = (sizeof(_prev_size) + sizeof(_size)) };

  static inline LeaHeader * mem2chunk (void * mem) {
    return ((LeaHeader *)((char*) mem - OVERHEAD));
  }

  static inline void * chunk2mem (LeaHeader * p) {
    return ((void *) ((char *) p + OVERHEAD));
  }

  inline size_t chunksize (void) const {
    return (_size & ~(SIZE_BITS));
  }
  
  inline bool prev_inuse (void) const {
    return (_size & PREV_INUSE);
  }

  inline size_t prev_size (void) const {
    return _prev_size;
  }

  inline LeaHeader * prev_chunk (void) const {
    return ((LeaHeader *) (((char*)this) - (prev_size())));
  }

  inline LeaHeader * next_chunk (void) const {
    return (LeaHeader *) (((char*) this)+chunksize());
  }

  inline bool inuse (void) const {
    return next_chunk()->prev_inuse();
  }

  inline bool chunk_is_mmapped (void) const {
    return (_size & IS_MMAPPED);
  }

  inline LeaHeader * back (void) const {
    return bk;
  }

  inline LeaHeader * forward (void) const {
    return fd;
  }

};

#endif
