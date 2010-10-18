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
#include "remotehost.h"


void* petabricks::RemoteObject::allocRecv(size_t len) {
  return malloc(len);
}

void petabricks::RemoteObject::freeRecv(void* buf, size_t ) {
  free(buf);
}

void petabricks::RemoteObject::onRecv(const void* , size_t s) {
  JTRACE("recv")(s);
}

void* petabricks::RemoteObject::allocRecvInitial(size_t len) {
  return allocRecv(len);
}

void petabricks::RemoteObject::onRecvInitial(const void* buf, size_t len) {
  JTRACE("recvInitial")(len);
  onRecv(buf,len);
}

void petabricks::RemoteObject::freeRecvInitial(void* buf, size_t len) {
  return freeRecv(buf, len);
}

void petabricks::RemoteObject::onCreated() {
  JTRACE("remote object created")(_flags);
}
void petabricks::RemoteObject::onComplete() {
  JTRACE("complete")(_flags);
}
void petabricks::RemoteObject::onNotify(int arg) {
  JTRACE("notify")(_flags)(arg);
}
void petabricks::RemoteObject::send(const void* p, size_t s) {
  host()->sendData(this, p, s);
}
void petabricks::RemoteObject::remoteSignal() {
  host()->remoteSignal(this);
}
void petabricks::RemoteObject::remoteBroadcast() {
  host()->remoteBroadcast(this);
}
void petabricks::RemoteObject::remoteNotify(int arg){
  host()->remoteNotify(this, arg);
}
void petabricks::RemoteObject::remoteMarkComplete() {
  host()->remoteMarkComplete(this);
}



