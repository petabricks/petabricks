/***************************************************************************
 *  Copyright (C) 2008-2010 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/
#ifndef PETABRICKSREMOTEOBJECT_H
#define PETABRICKSREMOTEOBJECT_H

#include "common/jmutex.h"
#include "common/jrefcounted.h"

#include <stdio.h>
#include <stdlib.h>

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
  enum { FLAG_INITIATOR = 1, FLAG_CREATED = 2, FLAG_COMPLETE = 4};
public:
  RemoteObject() : _host(NULL), _flags(0) {}


  void waitUntilCreated() const {
    if(0 == (_flags & FLAG_CREATED)) {
      JLOCKSCOPE(*this);
      waitUntilCreatedMu();
    }
  }
  void waitUntilComplete() const {
    if(0 == (_flags & FLAG_COMPLETE)) {
      JLOCKSCOPE(*this);
      waitUntilCreatedMu();
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
protected:
  void remoteMarkComplete();

  ConstRemoteHostPtr host() const { return _host; }
  RemoteHostPtr host() { return _host; }

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
  EncodedPtr remoteObj() const { return _remoteObj; }
private:
  RemoteHostPtr _host;
  EncodedPtr _remoteObj;
  int _flags;
};

} //namespace petabricks

#endif
