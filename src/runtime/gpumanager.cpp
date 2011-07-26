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

std::queue<GpuDynamicTaskPtr> GpuManager::_readytasks;
//std::map<MatrixStorageInfoPtr,cl_mem> GpuManager::_clmems;
jalib::JMutex  GpuManager::_lock;
bool GpuManager::_shutdown = true;
pthread_t GpuManager::_thread;

GpuTaskInfoPtr GpuManager::_currenttaskinfo;
cl_kernel GpuManager::_kernel;
cl_command_queue GpuManager::_queue = OpenCLUtil::getQueue(0);
cl_context GpuManager::_context = OpenCLUtil::getContext();

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

    //TODO: whey do I have to use empty?
    bool empty = _readytasks.empty();
    while(!empty) {
      std::cerr << ">>>>>>>>>>>>>>>>>>>>>>>>>>>> START " << std::endl;
      GpuDynamicTaskPtr task = _readytasks.front();
      int done = true;
      switch(task->tasktype()) {
        case GpuDynamicTask::PREPARE: prepare(task);        break;
        case GpuDynamicTask::COPYIN:  copyin(task);         break;
        case GpuDynamicTask::RUN:     run(task);            break;
        case GpuDynamicTask::COPYOUT: done = copyout(task); break;
      }
      std::cerr << "<<<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE" << std::endl;
      _lock.lock();
      _readytasks.pop();
      if(!done)
        _readytasks.push(task);
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

void GpuManager::prepare(GpuDynamicTaskPtr task) {
  std::cerr << "[PREPARE]" << std::endl;
  _currenttaskinfo = task->taskinfo();
  task->runWrapper();
  std::cerr << "Number of From Matrices = " << _currenttaskinfo->_from.size() << std::endl;
  std::cerr << "Number of To Matrices   = " << _currenttaskinfo->_to.size() << std::endl;

  for(std::vector<MatrixStorageInfoPtr>::iterator i = _currenttaskinfo->_to.begin(); i != _currenttaskinfo->_to.end(); ++i) {
    (*i)->initGpuMem(_context); // clCreateBuffer
  }
}

void GpuManager::copyin(GpuDynamicTaskPtr task) {
  std::cerr << "[COPY IN]" << std::endl;
  _currenttaskinfo = task->taskinfo();
  MatrixStorageInfoPtr storageinfo = task->storageinfo();

  if(storageinfo->initGpuMem(_context)) { // clCreateBuffer
    std::cerr << "copying in... " << &(*storageinfo) << std::endl;
    task->runWrapper(); // clEnqueueWriteBuffer
  }
  else {
    task->completeTaskDeps();
  }
}

void GpuManager::run(GpuDynamicTaskPtr task) {
  std::cerr << "[RUN]" << std::endl;
  _currenttaskinfo = task->taskinfo();
  task->runWrapper();

  //cl_int err;
  for(std::vector<MatrixStorageInfoPtr>::iterator i = _currenttaskinfo->_to.begin(); i != _currenttaskinfo->_to.end(); ++i) {
    (*i)->finishGpuMem(_queue,true); // clEnqueueReadBuffer
  }
  for(std::vector<MatrixStorageInfoPtr>::iterator i = _currenttaskinfo->_from.begin(); i != _currenttaskinfo->_from.end(); ++i) {
    (*i)->finishGpuMem(_queue,false); // clEnqueueReadBuffer
  }
}

bool GpuManager::copyout(GpuDynamicTaskPtr task) {
  cl_int err;
  MatrixStorageInfoPtr storage = task->storageinfo();   
  std::cerr << "[COPY OUT]" << &(*storage) << std::endl;
  if(!storage->isModified()) {
    std::cerr << "is not modified... " << &(*storage) << std::endl;
    task->completeTaskDeps();
    return true;
  }
  if(storage->doneReadBuffer()) {
    std::cerr << "actual copy... " << &(*storage) << std::endl;
    task->runWrapper();
    return true;
  }
  return false;
}

}
