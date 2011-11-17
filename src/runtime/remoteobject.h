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
#ifndef PETABRICKSREMOTEOBJECT_H
#define PETABRICKSREMOTEOBJECT_H

#include "common/jasm.h"
#include "common/jmutex.h"
#include "common/jrefcounted.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

namespace petabricks {

typedef ptrdiff_t EncodedPtr;

class RemoteObject;
typedef jalib::JRef<RemoteObject> RemoteObjectPtr;
typedef std::vector<RemoteObjectPtr> RemoteObjectList;
typedef RemoteObjectPtr (*RemoteObjectGenerator)(void);

class RemoteHost;
typedef RemoteHost* RemoteHostPtr;
typedef const RemoteHost* ConstRemoteHostPtr;


class RemoteObject : public jalib::JRefCounted, public jalib::JCondMutex {
  friend class RemoteHost;
  enum { FLAG_INITIATOR = 1,
         FLAG_CREATED = 2,
         FLAG_COMPLETE = 4 };
public:
  RemoteObject() : _host(NULL), _flags(0), _lastMsgGen(0), _pendingMessages(0) {}


  void waitUntilCreated() const {
    if(0 == (_flags & FLAG_CREATED)) {
      JLOCKSCOPE(*this);
      waitUntilCreatedMu();
    }
  }
  void waitUntilComplete() const {
    if(0 == (_flags & FLAG_COMPLETE)) {
      JLOCKSCOPE(*this);
      waitUntilCompleteMu();
    }
  }

  //transfer data to remote host and call remote recv
  void send(const void* ptr , size_t len);

  void markComplete() {
    remoteMarkComplete();
    JLOCKSCOPE(*this);
    markCompleteMu();
  }
  void remoteSignal();
  void remoteBroadcast();
  void remoteNotify(int arg = 0);

  int flags() const { return _flags; }


  ConstRemoteHostPtr host() const { return _host; }
  RemoteHostPtr host() { return _host; }

  bool isCreated() const {
    return 0 != (_flags & FLAG_CREATED);
  }
  
  bool isInitiator() const {
    return 0 != (_flags & FLAG_CREATED);
  }
  
  bool isComplete() const {
    return 0 != (_flags & FLAG_COMPLETE);
  }

  int lastMsgGen() const { return _lastMsgGen; }

  int pendingMessages() const { return _pendingMessages; }

  EncodedPtr remoteObj() const { return _remoteObj; }


  bool maybeDeletable(int gen) const {
    return isCreated() && refCount()==1 && lastMsgGen()<gen && pendingMessages()==0;
  }
protected:
  void remoteMarkComplete();

  // these three callbacks get called to handle incoming data
  virtual void* allocRecv(size_t len);
  virtual void onRecv(const void* , size_t s);
  virtual void freeRecv(void* buf, size_t );

  // these three callbacks get called to handle initial incoming data, default to above three
  virtual void* allocRecvInitial(size_t len);
  virtual void onRecvInitial(const void* buf, size_t len);
  virtual void freeRecvInitial(void* buf, size_t len);

  // this method gets called after a connection is established and initial data is sent
  virtual void onCreated();
  virtual void onNotify(int arg);
  virtual void onComplete();

  void setHostMu(RemoteHostPtr v) { _host = v; }
  void markInitiatorMu() { _flags |= FLAG_INITIATOR; }
  void markCreatedMu() { _flags |= FLAG_CREATED;  broadcast(); }
  void markCompleteMu() { _flags |= FLAG_COMPLETE;  broadcast(); }
  void setRemoteObjMu(EncodedPtr v) { _remoteObj = v; }
  void waitUntilCreatedMu() const {
    while(0 == (_flags & FLAG_CREATED) ) wait();
  }
  void waitUntilCompleteMu() const {
    while(0 == (_flags & FLAG_COMPLETE) ) wait();
  }
private:
  RemoteHostPtr _host;
  EncodedPtr _remoteObj;
  int _flags;
  int _lastMsgGen;
  jalib::AtomicT _pendingMessages;
};

} //namespace petabricks

#endif
