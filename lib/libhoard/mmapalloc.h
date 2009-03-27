// -*- C++ -*-

#ifndef _MMAPALLOC_H_
#define _MMAPALLOC_H_

#include "mmapwrapper.h"

/**
 * @class MmapAlloc
 * @brief Obtains memory from Mmap but doesn't allow it to be freed.
 * @author Emery Berger <http://www.cs.umass.edu/~emery>
 */

class MmapAlloc {
public:
  void * malloc (size_t sz) {
    void * ptr = HL::MmapWrapper::map (sz);
    if (ptr == 0) {
      fprintf (stderr, "System memory exhausted.\n");
    }
    return ptr;
  }

};

#endif
