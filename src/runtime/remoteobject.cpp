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
#include "remoteobject.h"


void* petabricks::RemoteObject::allocRecv(size_t len) {
  return malloc(len);
}

void petabricks::RemoteObject::freeRecv(void* buf, size_t ) {
  free(buf);
}

void petabricks::RemoteObject::recv(const void* , size_t s) {
  JTRACE("recv")(s);
}

void* petabricks::RemoteObject::allocRecvInitial(size_t len) {
  return allocRecv(len);
}

void petabricks::RemoteObject::recvInitial(const void* buf, size_t len) {
  recv(buf,len);
}

void petabricks::RemoteObject::freeRecvInitial(void* buf, size_t len) {
  return freeRecv(buf, len);
}

void petabricks::RemoteObject::created() {
  JTRACE("remote object created")(_isInitiator);
}

