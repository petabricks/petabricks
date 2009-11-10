/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
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
