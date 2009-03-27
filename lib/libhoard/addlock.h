// -*- C++ -*-

#ifndef _ADDLOCK_H_
#define _ADDLOCK_H_

/**
 * @class AddLock
 * @brief Adds explicit lock and unlock commands to a superblock.
 */

template <class LockType, class Super>
class AddLock : public Super {
public:

  class Header : public Super {};

  AddLock (size_t sz)
    : Super (sz)
  {}

  virtual ~AddLock (void) {}

  inline void lock (void) {
    _theLock.lock();
  }

  inline void unlock (void) {
    _theLock.unlock();
  }

private:

  /// The lock for this header.
  LockType _theLock;
};


#endif
