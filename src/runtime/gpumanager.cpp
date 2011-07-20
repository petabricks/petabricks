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

#include "gpumanager.h"
#include "openclutil.h"
#include "dynamicscheduler.h"
#include "gpudynamictask.h"

namespace petabricks {

typedef std::list<GpuDynamicTaskPtr>::iterator gpuTaskIter;

//std::list<GpuDynamicTaskPtr> GpuManager::_pendingtasks;
std::queue<GpuDynamicTaskPtr> GpuManager::_readytasks;
jalib::JMutex  GpuManager::_lock;
bool GpuManager::_shutdown = true;
pthread_t GpuManager::_thread;

extern "C" void *startGpuManager(void* /*arg*/) {
  //try {
    petabricks::GpuManager::mainLoop();
  //}catch(petabricks::DynamicScheduler::CleanExitException e){}
  return NULL;
}

void GpuManager::start() {
  if(!_shutdown) return;
  _shutdown = false;
  std::cerr << "pthead_create~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, 0);
  JASSERT(pthread_create(&_thread, &attr, startGpuManager, NULL) == 0);
  pthread_attr_destroy(&attr);
}

void GpuManager::shutdown() {
  if(_shutdown) return;
  _shutdown = true;
  int rv = pthread_join(_thread, NULL);
  JWARNING(rv==0)(rv).Text("pthread_join failed");
  std::cerr << "pthead_join~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
}

void GpuManager::mainLoop() {
  for(;;){
    // Execute tasks from ready queue
    /*while(!_readytasks.empty()) {
      _readytasks.front()->runWrapper(false);
      _readytasks.pop();
    }

    // Move ready tasks from tasklist to ready queue
    std::list<gpuTaskIter> del;

    for(gpuTaskIter it = _pendingtasks.begin(); it != _pendingtasks.end(); ++it){
      if((*it)->getStatus() == DynamicTask::S_READY) {
        del.push_back(it);
        _readytasks.push(*it);
      }
    }

    _lock.lock();
    for(std::list<gpuTaskIter>::iterator it = del.begin(); it != del.end(); ++it){
      _pendingtasks.erase(*it);
    }
    _lock.unlock();*/

    //TODO: whey do I have to use empty?
    bool empty = _readytasks.empty();
    while(!empty) {
      _readytasks.front()->runWrapper();
      _lock.lock();
      _readytasks.pop();
      _lock.unlock();
      empty = _readytasks.empty();
    }

    if(_shutdown) { 
      break;
    }
  }
}

void GpuManager::addTask(GpuDynamicTaskPtr task) {
  _lock.lock();
  _readytasks.push(task);
  _lock.unlock();
}

}
