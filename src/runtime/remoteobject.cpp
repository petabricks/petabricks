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
#include "remoteobject.h"
#include "remotehost.h"

#include "common/jasm.h"


void* petabricks::RemoteObject::allocRecv(size_t len, int) {
  return new char[len];
}

void petabricks::RemoteObject::freeRecv(void* buf, size_t, int ) {
  delete [] ((char*)buf);
}

void petabricks::RemoteObject::onRecv(const void* , size_t s, int) {
  JTRACE("recv")(s);
}

void* petabricks::RemoteObject::allocRecvInitial(size_t len) {
  return allocRecv(len,0);
}

void petabricks::RemoteObject::onRecvInitial(const void* buf, size_t len) {
  JTRACE("recvInitial")(len);
  onRecv(buf,len,0);
}

void petabricks::RemoteObject::freeRecvInitial(void* buf, size_t len) {
  return freeRecv(buf, len,0);
}

void petabricks::RemoteObject::onCreated() {
  //JTRACE("remote object created")(_flags);
}
void petabricks::RemoteObject::onComplete() {
  JTRACE("complete")(_flags);
}
void petabricks::RemoteObject::onNotify(int arg) {
  JTRACE("notify")(_flags)(arg);
}
void petabricks::RemoteObject::send(const void* p, size_t s, int arg) const {
  //JTRACE("send")(s)(arg);
  host()->sendData(this, p, s, arg);
}
void petabricks::RemoteObject::send(const void* p, size_t s, const void* p2, size_t s2, int arg) const {
  //JTRACE("send")(s2)(arg);
  host()->sendData(this, p, s, p2, s2, arg);
}
void petabricks::RemoteObject::sendMu(const void* p, size_t s, int arg) const {
  //JTRACE("sendmu")(s)(arg);
  unlock();
  host()->sendData(this, p, s, arg);
  lock();
}
void petabricks::RemoteObject::sendMu(const void* p, size_t s, const void* p2, size_t s2, int arg) const {
  //JTRACE("sendmu")(s2)(arg);
  unlock();
  host()->sendData(this, p, s, p2, s2, arg);
  lock();
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

void petabricks::RemoteObject::waitMsgMu() const{
  for(;;) {
    if(pendingMessages() > 0) {
      unlock();
      while(pendingMessages() > 0) jalib::memFence();
      lock();
      return;
    }
    if(host()->recv(this)) {
      return;
    }
  }
}

