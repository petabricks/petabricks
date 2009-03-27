/* -*- C++ -*- */

#ifndef _SEMILOCKEDHEAP_H_
#define _SEMILOCKEDHEAP_H_


/***********************************************
*  Provide a lock mechanism for malloc().      *
*                                              *
*  Provide :                                   *
*    lock()                                    *
*    unlock()                                  *
*                                              *
***********************************************/


template <class Super, class LockType>
class SemiLockedHeap : public Super {
public:

  inline void * malloc (size_t sz) {
    thelock.lock();
    printf ("Lock %x\n", this);
    void * ptr = Super::malloc (sz);
    //printf ("Unlock %x\n",(int)this);
    thelock.unlock();
    return ptr;
  }
  
  inline void lock (void) {
    thelock.lock();
    //printf ("Lock %x\n",(int)this);
  }

  inline void unlock (void) {
    //printf ("Unlock %x\n",(int)this);
    thelock.unlock(); 
  }
 
private:
  LockType thelock;

};

#endif








