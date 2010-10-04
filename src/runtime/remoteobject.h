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
public:
  RemoteObject() : _host(NULL), _isInitiator(false) {}

  ConstRemoteHostPtr host() const { return _host; }
  RemoteHostPtr host() { return _host; }
  void setHost(RemoteHostPtr v) { _host = v; }
  void markInitiator() { _isInitiator=true; }
  void setRemoteObj(EncodedPtr v) { _remoteObj = v; }

  virtual void* allocRecv(size_t len);
  virtual void recv(const void* , size_t s);
  virtual void freeRecv(void* buf, size_t );

  virtual void* allocRecvInitial(size_t len);
  virtual void recvInitial(const void* buf, size_t len);
  virtual void freeRecvInitial(void* buf, size_t len);

  virtual void created();

private:
  RemoteHostPtr _host;
  EncodedPtr _remoteObj;
  bool _isInitiator;
};

} //namespace petabricks

#endif
