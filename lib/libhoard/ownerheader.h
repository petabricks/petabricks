#ifndef _OWNERHEADER_H_
#define _OWNERHEADER_H_

#include <assert.h>

/**
 * @class OwnerHeader
 * @brief Adds ownership management to a superblock.
 * @author Emery Berger <http://www.cs.umass.edu/~emery>
 */

template <class Super, class SuperblockType, class HeapType>
class OwnerHeader : public Super {
public:

  typedef HeapType OwnerType;

  class Header : public Super {};

  OwnerHeader (size_t sz)
    : Super (sz),
    _owner (NULL),
    _prev (NULL),
    _next (NULL)
  {}

  inline HeapType * getOwner (void) const {
    return _owner;
  }

  inline void setOwner (HeapType * o) {
    assert (o != NULL);
    _owner = o;
  }

  inline SuperblockType * getNext (void) const {
    return _next;
  }

  inline SuperblockType * getPrev (void) const {
    return _prev;
  }

  inline void setNext (SuperblockType * f) {
    assert (f != this);
    _next = f;
  }

  inline void setPrev (SuperblockType * f) {
    assert (f != this);
    _prev = f;
  }

private:

  // Disable copying and assignment.

  OwnerHeader (const OwnerHeader&);
  OwnerHeader& operator=(const OwnerHeader&);

  /// The owner of this superblock.
  HeapType * _owner;

  /// The preceding superblock in a linked list.
  SuperblockType * _prev;

  /// The succeeding superblock in a linked list.
  SuperblockType * _next;

};

#endif
