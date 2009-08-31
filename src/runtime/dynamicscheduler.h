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
#ifndef PETABRICKSDYNAMICSCHEDULER_H
#define PETABRICKSDYNAMICSCHEDULER_H

#include "workerthread.h"

#include <list>
#include <pthread.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

class DynamicScheduler{
public:
  static DynamicScheduler& cpuScheduler();
  static DynamicScheduler& lookupScheduler(DynamicTask::TaskType t);
 
  ///
  /// start worker threads
  void startWorkerThreads(int newWorkers);

  ///
  /// Cancel all pending tasks (including caller)
  void abort();

  ///
  /// Shutdown all threads
  void shutdown();
  
  ///
  /// Exception thrown by aborting threads 
  class AbortException {};

  WorkerThreadPool& pool() { return _pool; }  

  int numThreads() const { return (int)_rawThreads.size(); }
protected:
  std::list<pthread_t> _rawThreads;
  WorkerThreadPool _pool;
};

}

#endif
