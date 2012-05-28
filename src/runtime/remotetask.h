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
#ifndef PETABRICKSREMOTETASK_H
#define PETABRICKSREMOTETASK_H

#include "dynamictask.h"
#include "remotehost.h"
#include "remoteobject.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

  class RemoteTask;
  typedef jalib::JRef<RemoteTask> RemoteTaskPtr;

  class RemoteTask : public petabricks::DynamicTask {
  public:
    RemoteTask();

    virtual size_t serialSize() = 0;
    virtual void serialize(char* buf, RemoteHost& host) = 0;
    virtual void unserialize(const char* buf, RemoteHost& host) = 0;
    virtual void migrateRegions(RemoteHost& sender) = 0;
    virtual RemoteHostList getDataHosts() = 0;
    virtual RemoteObjectGenerator generator() = 0;

    void onCompletedRemotely();
    void enqueueLocal();
    void enqueueRemote(RemoteHost& host);

  protected:
    void remoteScheduleTask();

    RemoteHostPtr _sender;
  };



  template<typename T>
  class CallMarkComplete : public DynamicTask {
  public:
    CallMarkComplete(const jalib::JRef<T>& t) : _rtp(t) {}
    DynamicTaskPtr run(){
      _rtp->markComplete();
      return 0;
    }

    bool isNullTask() const { return true; }
  private:
    jalib::JRef<T> _rtp;
  };

  class RemoteTaskSender : public petabricks::RemoteObject {
  public:
    RemoteTaskSender(const RemoteTaskPtr& t) : _task(t) {}

    void onComplete() {
      //JTRACE("remote complete");
      _task->onCompletedRemotely();
    }
  private:
    RemoteTaskPtr _task;
  };


  template<typename T>
  class RemoteTaskReciever : public petabricks::RemoteObject {
  public:
    static RemoteObjectPtr gen() { return new RemoteTaskReciever(); }

    void onRecvInitial(const void* buf, size_t ) {
      //JTRACE("remote create");
      JASSERT(!_task);
      _task = new T(reinterpret_cast<const char*>(buf), *host());
      _task->incRefCount();
      _task->enqueueLocal();
      DynamicTaskPtr t = new CallMarkComplete<RemoteTaskReciever>(this);
      t->dependsOn(_task.asPtr());
      t->enqueue();
    }
  private:
    RemoteTaskPtr _task;
  };


}

#endif
