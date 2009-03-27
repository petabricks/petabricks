// -*- C++ -*-

#ifndef _BASEHOARDMANAGER_H_
#define _BASEHOARDMANAGER_H_

/**
 * @class BaseHoardManager
 * @brief The top of the hoard manager hierarchy.
 *
 */

template <class SuperblockType_,
          size_t SuperblockSize>
class BaseHoardManager {
public:

  BaseHoardManager (void)
    : _magic (0xedded00d)
  {}

  virtual ~BaseHoardManager (void)
  {}

  inline int isValid (void) const {
    return (_magic == 0xedded00d);
  }

  // Export the superblock type.
  typedef SuperblockType_ SuperblockType;

  /// Free an object.
  inline virtual void free (void *) {}

  /// Lock this memory manager.
  inline virtual void lock (void) {}

  /// Unlock this memory manager.
  inline virtual void unlock (void) {};

  /// Return the size of an object.
  static inline size_t getSize (void * ptr) {
    SuperblockType * s = getSuperblock (ptr);
    assert (s->isValidSuperblock());
    return s->getObjectSize();
  }

  /// @brief Find the superblock corresponding to a pointer via bitmasking.
  /// @note  All superblocks <em>must</em> be naturally aligned, and powers of two.

  static inline SuperblockType * getSuperblock (void * ptr) {
    return reinterpret_cast<SuperblockType *>(reinterpret_cast<size_t>(ptr) & ~(SuperblockSize-1));
  }

private:

  HL::sassert<((SuperblockSize & (SuperblockSize - 1)) == 0)> EnsureSuperblockSizeIsPowerOfTwo;

  const unsigned long _magic;

};

#endif
