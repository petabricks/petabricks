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

#include "dynamicscheduler.h"
#include "gpudynamictask.h"
#include "workerthread.h"

#include "common/openclutil.h"

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

//#define GPU_TRACE 1

#ifdef HAVE_OPENCL
static bool _useOpenCL() { return true; }
#else
static bool _useOpenCL() { return false; }
#endif

namespace petabricks {

typedef std::list<GpuDynamicTaskPtr>::iterator gpuTaskIter;

std::queue<GpuDynamicTaskPtr> GpuManager::_readytasks;
jalib::JMutex  GpuManager::_lock;
bool GpuManager::_shutdown = true;
pthread_t GpuManager::_thread;

GpuTaskInfoPtr GpuManager::_currenttaskinfo;
cl_kernel GpuManager::_kernel;

#ifdef HAVE_OPENCL
cl_command_queue GpuManager::_queue = OpenCLUtil::getQueue(0);
cl_context GpuManager::_context = OpenCLUtil::getContext();
#else
cl_command_queue GpuManager::_queue = -1;
cl_context GpuManager::_context = -1;
#endif

extern "C" void *startGpuManager(void* /*arg*/) {
  if(!_useOpenCL()) return NULL;
  //try {
  petabricks::WorkerThread::markUtilityThread();
  petabricks::GpuManager::mainLoop();
  //}catch(petabricks::DynamicScheduler::CleanExitException e){}
  return NULL;
}

void GpuManager::start() {
  if(!_useOpenCL()) return;
  if(!_shutdown) return;
  _shutdown = false;
  #ifdef GPU_TRACE
  std::cout << "pthead_create~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  #endif
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, 0);
  JASSERT(pthread_create(&_thread, &attr, startGpuManager, NULL) == 0);
  pthread_attr_destroy(&attr);
}

void GpuManager::shutdown() {
  if(!_useOpenCL()) return;
  if(_shutdown) return;
  _shutdown = true;
  int rv = pthread_join(_thread, NULL);
  JWARNING(rv==0)(rv).Text("pthread_join failed");
  OpenCLUtil::deinit();
  #ifdef GPU_TRACE
  std::cout << "pthead_join~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  #endif
}

void GpuManager::mainLoop() {
  if(!_useOpenCL()) return;
  for(;;){

    //TODO: whey do I have to use empty?
    bool empty = _readytasks.empty();
    while(!empty) {
      #ifdef GPU_TRACE
      std::cout << ">>>>>>>>>>>>>>>>>>>>>>>>>>>> START " << std::endl;
      #endif
      GpuDynamicTaskPtr task = _readytasks.front();
      _currenttaskinfo = task->taskinfo();
      int done = true;
      switch(task->tasktype()) {
        case GpuDynamicTask::PREPARE: prepare(task);        break;
        case GpuDynamicTask::COPYIN:  copyin(task);         break;
        case GpuDynamicTask::RUN:     run(task);            break;
        case GpuDynamicTask::COPYOUT: done = copyout(task); break;
      }
      #ifdef GPU_TRACE
      std::cout << "<<<<<<<<<<<<<<<<<<<<<<<<<<<<< DONE" << std::endl;
      #endif
      _lock.lock();
      _readytasks.pop();
      if(!done)
        _readytasks.push(task);
      empty = _readytasks.empty();
      _lock.unlock();
    }

    if(_shutdown) { 
      break;
    }
  }
}

void GpuManager::addTask(GpuDynamicTaskPtr task) {
  JASSERT(_useOpenCL());
  _lock.lock();
  _readytasks.push(task);
  _lock.unlock();
}

#ifndef HAVE_OPENCL

void GpuManager::prepare(GpuDynamicTaskPtr ) {  
  UNIMPLEMENTED();
}

void GpuManager::copyin(GpuDynamicTaskPtr ) {
  UNIMPLEMENTED();
}

void GpuManager::run(GpuDynamicTaskPtr ) {
  UNIMPLEMENTED();
}

bool GpuManager::copyout(GpuDynamicTaskPtr ) {
  UNIMPLEMENTED();
}


#else

void GpuManager::prepare(GpuDynamicTaskPtr task) {
  JASSERT(_useOpenCL());
  #ifdef GPU_TRACE
  std::cout << "[PREPARE]" << std::endl;
  #endif
  task->runWrapper();
  #ifdef GPU_TRACE
  std::cout << "Number of From Matrices = " << _currenttaskinfo->_from.size() << std::endl;
  std::cout << "Number of To Matrices   = " << _currenttaskinfo->_to.size() << std::endl;
  #endif

  for(std::vector<MatrixStorageInfoPtr>::iterator i = _currenttaskinfo->_to.begin(); i != _currenttaskinfo->_to.end(); ++i) {
    (*i)->initGpuMem(_queue,_context,false); // clCreateBuffer
  }
}

void GpuManager::copyin(GpuDynamicTaskPtr task) {
  JASSERT(_useOpenCL());
  #ifdef GPU_TRACE
  std::cout << "[COPY IN]" << std::endl;
  #endif
  MatrixStorageInfoPtr storageinfo = task->storageinfo();

  if(storageinfo->initGpuMem(_queue,_context,true)) { // clCreateBuffer
    #ifdef GPU_TRACE
    std::cout << "copying in... " << &(*storageinfo) << std::endl;
    #endif
    task->runWrapper(); // clEnqueueWriteBuffer
  }
  else {
    task->completeTaskDeps();
  }
}

void GpuManager::run(GpuDynamicTaskPtr task) {
  JASSERT(_useOpenCL());
  #ifdef GPU_TRACE
  std::cout << "[RUN]" << std::endl;
  #endif
  /*for(std::vector<MatrixStorageInfoPtr>::iterator i = _currenttaskinfo->_from.begin(); i != _currenttaskinfo->_from.end(); ++i) {
    (*i)->check(_queue);
  }*/

  task->run();

  for(std::vector<MatrixStorageInfoPtr>::iterator i = _currenttaskinfo->_to.begin(); i != _currenttaskinfo->_to.end(); ++i) {
    (*i)->finishGpuMem(_queue,_currenttaskinfo->nodeID(), _currenttaskinfo->regionNodeGroupMap(), _currenttaskinfo->gpuCopyOut()); // clEnqueueReadBuffer
  }
  task->completeTaskDeps();
}

bool GpuManager::copyout(GpuDynamicTaskPtr task) {
  JASSERT(_useOpenCL());
  MatrixStorageInfoPtr storage = task->storageinfo();
  #ifdef GPU_TRACE 
  std::cout << "[COPY OUT]" << &(*storage) << std::endl;
  #endif

  CopyoutInfoPtr copyInfo = storage->getCopyoutInfo(_currenttaskinfo->nodeID());
  //TODO: still not totally right
  if(!copyInfo) {
    #ifdef GPU_TRACE
    std::cout << "not done " << _currenttaskinfo->nodeID() << std::endl;
    #endif
    return false;
  }
  if(copyInfo->done()) {
    #ifdef GPU_TRACE
    std::cout << "done " << _currenttaskinfo->nodeID() << std::endl;
    #endif
    task->completeTaskDeps();
    storage->done(_currenttaskinfo->nodeID());
    return true;
  }
  if(copyInfo->complete()) {
    #ifdef GPU_TRACE
    std::cout << "copy out... " << &(*storage) << std::endl;
    std::cout << "done " << _currenttaskinfo->nodeID() << std::endl;
    #endif
    task->setRegions(copyInfo->getBegins(), copyInfo->getEnds(), _currenttaskinfo->nodeID());
    task->runWrapper();
    storage->done(_currenttaskinfo->nodeID());
    return true;
  }
  #ifdef GPU_TRACE
  std::cout << "not done " << _currenttaskinfo->nodeID() << std::endl;
  #endif
  return false;
}

#endif

}

