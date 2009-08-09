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
#ifndef JALIBJTHREAD_H
#define JALIBJTHREAD_H

#include "jblockingqueue.h"

#include <list>

namespace jalib {

class JThread;
class JThreadContext;
typedef JBlockingQueue<JThread*> JThreadQueue;
  
/**
 * User space threading library
 */
class JThread{
  friend class JThreadContext;
  enum ThreadState{
    STOPPED,
    RUNNING,
    SUSPENDED
  };
public:
  JThread();
  virtual ~JThread();
  
  void start();
  
  void yield();
  
  static void launchThreads();
public:
  virtual void run() = 0;

private:
  static JThreadQueue& pendingThreadPool();
  static void* systemThreadMain(void* );
private:
  JThreadContext* _stack;
  ThreadState _state;
};
  
}

#endif
