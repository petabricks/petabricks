/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
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
