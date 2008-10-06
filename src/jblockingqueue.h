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

#include <list>

namespace jalib {

template < typename T > class JBlockingQueue{
  public:
    void push(const T& e) {
      JLOCKSCOPE(_lock); 
      _lock.signal();
      return _q.push_back(e); 
    }
    T pop() {
      JLOCKSCOPE(_lock);
      while(_q.empty()) _lock.wait();
      T t = _q.front();
      _q.pop_front();
      return t; 
    }
    T tryPop() {
      JLOCKSCOPE(_lock);
      if(_q.empty()) return NULL;
      T t = _q.front();
      _q.pop_front();
      return t; 
    }
  private:
    std::list<T> _q;
    JCondMutex _lock;
};

}

#endif
