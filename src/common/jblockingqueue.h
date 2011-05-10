/*****************************************************************************
 *  Copyright (C) 2008-2011 Massachusetts Institute of Technology            *
 *                                                                           *
 *  Permission is hereby granted, free of charge, to any person obtaining    *
 *  a copy of this software and associated documentation files (the          *
 *  "Software"), to deal in the Software without restriction, including      *
 *  without limitation the rights to use, copy, modify, merge, publish,      *
 *  distribute, sublicense, and/or sell copies of the Software, and to       *
 *  permit persons to whom the Software is furnished to do so, subject       *
 *  to the following conditions:                                             *
 *                                                                           *
 *  The above copyright notice and this permission notice shall be included  *
 *  in all copies or substantial portions of the Software.                   *
 *                                                                           *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY                *
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE               *
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      *
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   *
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   *
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    *
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE           *
 *                                                                           *
 *  This source code is part of the PetaBricks project:                      *
 *    http://projects.csail.mit.edu/petabricks/                              *
 *                                                                           *
 *****************************************************************************/
#ifndef JALIBJBLOCKINGQUEUE_H
#define JALIBJBLOCKINGQUEUE_H

#include "jmutex.h"
#include "jasm.h"

#include <list>
#include <stdint.h>

namespace jalib {

template < typename T > class JBlockingQueue{
  public:
#ifdef QUEUE_STATISTICS
    JBlockingQueue() {
      total = 0;
      count = 0;
      totalEmpty = 0;
      totalFilled = 0;
      t1 = t2 = 0;
    }
#endif


    void push(const T& e) {
      JLOCKSCOPE(_lock); 
#ifdef QUEUE_STATISTICS
      if(_q.empty()) {
	count++;
	t2 = jalib::ClockCyclesSinceBoot();
	if(total != 0) {
	  totalEmpty += (t2 - t1);
	}
      }
      total++;
#endif
      _lock.signal();
      return _q.push_front(e); 
    }


    T pop() {
      JLOCKSCOPE(_lock);
      while(_q.empty()) _lock.wait();
      T t = _q.front();
      _q.pop_front();
#ifdef QUEUE_STATISTICS
      if(_q.empty()) {
	t1 = jalib::ClockCyclesSinceBoot();
	totalFilled += (t1 - t2);
      }
#endif
      return t; 
    }


    T tryPop() {
      // precheck to reduce locking overhead
      if(_q.empty()) return NULL;
      JLOCKSCOPE(_lock);
      if(_q.empty()) return NULL;
      T t = _q.front();
      _q.pop_front();
#ifdef QUEUE_STATISTICS
      if(_q.empty()) {
	t1 = jalib::ClockCyclesSinceBoot();
	totalFilled += (t1 - t2);
      }
#endif
      return t; 
    }


    bool empty() {
      JLOCKSCOPE(_lock);
      return _q.empty();
    }


    size_t size() {
      JLOCKSCOPE(_lock);
      return _q.size();
    }


    void clear() {
      JLOCKSCOPE(_lock);
      _q.clear();
    }


#ifdef QUEUE_STATISTICS
    unsigned int contention() {
      unsigned int temp = count;
      count = 0;
      return temp;
    }


    unsigned int totalEnqueue() {
      unsigned int temp = total;
      total = 0;
      return temp;
    }


    uint64_t totalEmptyTime() {
      uint64_t temp = totalEmpty;
      totalEmpty  = 0;
      return temp;
    }


    uint64_t totalFilledTime() {
      uint64_t temp = totalFilled;
      totalFilled  = 0;
      return temp;      
    }
#endif


  private:
    std::list<T> _q;
    JCondMutex _lock;
#ifdef QUEUE_STATISTICS
    unsigned int total, count;
    uint64_t  totalEmpty, totalFilled;
    uint64_t  t1, t2;
#endif
};

}

#endif
