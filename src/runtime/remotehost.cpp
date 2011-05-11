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

#include "remotehost.h"

#include "common/jconvert.h"

#include <algorithm>
#include <poll.h>
#include <unistd.h>

namespace _RemoteHostMsgTypes {

  typedef petabricks::EncodedPtr EncodedPtr;

  typedef uint32_t DataLen;
  typedef uint16_t MessageType;
  typedef int16_t ChanNumber;

  extern "C" void token_func() {}

  template<typename T>
  EncodedPtr EncodeTextPtr(T p) {
    return reinterpret_cast<intptr_t>(p)-reinterpret_cast<intptr_t>(&token_func);
  }

  template<typename T>
  T DecodeTextPtr(EncodedPtr p) {
    return reinterpret_cast<T>(p+reinterpret_cast<intptr_t>(&token_func));
  }
  
  template<typename T>
  EncodedPtr EncodeDataPtr(T* p) {
    return reinterpret_cast<EncodedPtr>(p);
  }

  template<typename T>
  T* DecodeDataPtr(EncodedPtr p) {
    return reinterpret_cast<T*>(p);
  }
  
  using petabricks::HostPid;
  struct MessageTypes {
    enum {
      HELLO_CONTROL= 0xf0c0,
      HELLO_DATA,
      REMOTEOBJECT_CREATE,
      REMOTEOBJECT_CREATE_ACK,
      REMOTEOBJECT_DATA,
      REMOTEOBJECT_SIGNAL,
      REMOTEOBJECT_BROADCAST,
      REMOTEOBJECT_NOTIFY,
      REMOTEOBJECT_MARKCOMPLETE,
    };
    static const char* str(int t) {
      switch(t) {
#define  EXPSTR(s) case s: return #s
        EXPSTR(HELLO_CONTROL);
        EXPSTR(HELLO_DATA);
        EXPSTR(REMOTEOBJECT_CREATE);
        EXPSTR(REMOTEOBJECT_CREATE_ACK);
        EXPSTR(REMOTEOBJECT_DATA);
        EXPSTR(REMOTEOBJECT_SIGNAL);
        EXPSTR(REMOTEOBJECT_BROADCAST);
        EXPSTR(REMOTEOBJECT_NOTIFY);
        EXPSTR(REMOTEOBJECT_MARKCOMPLETE);
#undef EXPSTR
        default: return "INVALID";
    }
  }
  };

  struct HelloMessage {
    MessageType type;
    HostPid     id;
    ChanNumber  chan;
    
    friend std::ostream& operator<<(std::ostream& o, const HelloMessage& m) {
      return o << "HelloMessage("
               << MessageTypes::str(m.type) << ", "
               << m.id << ")";
    }
  };

  struct GeneralMessage {
    MessageType type;
    ChanNumber  chan;
    DataLen     len;
    EncodedPtr  srcptr;
    EncodedPtr  dstptr;

    friend std::ostream& operator<<(std::ostream& o, const GeneralMessage& m) {
      return o << "GeneralMessage("
               << MessageTypes::str(m.type) << ", "
               << m.len << " bytes, "
               << std::hex << m.srcptr << " => " << m.dstptr << std::dec << ")";
    }
  };

  void* start_listenLoop(void* arg) {
    ((petabricks::RemoteHostDB*)arg)->listenLoop();
    return NULL;
  }
}
using namespace _RemoteHostMsgTypes;

void petabricks::RemoteHost::accept(jalib::JServerSocket& s) {
  _control.close();
  _control = s.accept();
  JASSERT(_control.isValid());
  for(int i=0; i<REMOTEHOST_DATACHANS; ++i) {
    _data[i].close();
    _data[i] = s.accept();
    JASSERT(_data[i].isValid());
  }
  _lastchan = 1;
  handshake();
}

void petabricks::RemoteHost::connect(const jalib::JSockAddr& a, int p) {
  JASSERT(_control.connect(a, p));
  for(int i=0; i<REMOTEHOST_DATACHANS; ++i) {
    JASSERT(_data[i].connect(a, p));
  }
  _lastchan = 0;
  handshake();
}

void petabricks::RemoteHost::handshake() {
  HostPid self = { gethostid(), getpid() };

  HelloMessage msg = { MessageTypes::HELLO_CONTROL,
                       self,
                       REMOTEHOST_DATACHANS};
  _control.disableNagle();
  _control.writeAll((char*)&msg, sizeof msg);
  _control.readAll((char*)&msg, sizeof msg);
  JASSERT(msg.type == MessageTypes::HELLO_CONTROL && msg.id != self && msg.chan == REMOTEHOST_DATACHANS);
  _id = msg.id;
  
  for(int i=0; i<REMOTEHOST_DATACHANS; ++i) {
    HelloMessage dmsg = { MessageTypes::HELLO_DATA, self, i};
    _data[i].disableNagle();
    JASSERT(_data[i].writeAll((char*)&dmsg, sizeof dmsg) == sizeof dmsg);
    JASSERT(_data[i].readAll((char*)&dmsg, sizeof dmsg) == sizeof dmsg);
    JASSERT(dmsg.type == MessageTypes::HELLO_DATA
        && dmsg.id == _id 
        && dmsg.chan == i);
  }
}


bool petabricks::RemoteHost::recv() {
  GeneralMessage msg;

  if(!_controlmu.trylock()) {
    JTRACE("skipping recv, locked");
    return false;
  }

  ssize_t cnt = _control.tryReadAll((char*)&msg, sizeof msg);
  if(cnt==0) {
    _controlmu.unlock();
    return false;
  }
  if(cnt<0) {
    _controlmu.unlock();
    JASSERT(false)(_id).Text("disconnected");
    return false;
  }
  JASSERT(cnt==sizeof msg)(cnt);

  if(msg.len>0){
    JASSERT(msg.chan>=0 && msg.chan<REMOTEHOST_DATACHANS);
    _datamu[msg.chan].lock();
  }
  _controlmu.unlock();
  RemoteObjectGenerator gen = 0;
  RemoteObjectPtr obj = 0;
  void* buf = 0;

  switch(msg.type) {
  case MessageTypes::REMOTEOBJECT_CREATE: 
    {
      gen = DecodeTextPtr<RemoteObjectGenerator>(msg.dstptr);
      obj = (*gen)();
      JLOCKSCOPE(*obj);
      obj->setHostMu(this);
      obj->setRemoteObjMu(msg.srcptr);
      if(msg.len>0){
        buf = obj->allocRecvInitial(msg.len);
        _data[msg.chan].readAll((char*)buf, msg.len);
        _datamu[msg.chan].unlock();
        obj->onRecvInitial(buf, msg.len);
        obj->freeRecvInitial(buf, msg.len);
      }
      obj->onCreated();
      obj->markCreatedMu();
      { GeneralMessage ackmsg = { MessageTypes::REMOTEOBJECT_CREATE_ACK, 0, 0, EncodeDataPtr(obj.asPtr()), msg.srcptr };
        sendMsg(&ackmsg);
      }
      JLOCKSCOPE(_controlmu);
      _objects.push_back(obj);
      break;
    }
  case MessageTypes::REMOTEOBJECT_CREATE_ACK:
    {
      obj = DecodeDataPtr<RemoteObject>(msg.dstptr);
      JASSERT(msg.len==0);
      JLOCKSCOPE(*obj);
      obj->setRemoteObjMu(msg.srcptr);
      obj->onCreated();
      obj->markCreatedMu();
      break;
    }
  case MessageTypes::REMOTEOBJECT_DATA:
    {
      obj = DecodeDataPtr<RemoteObject>(msg.dstptr);
      JLOCKSCOPE(*obj);
      if(msg.len>0){
        buf = obj->allocRecv(msg.len);
        _data[msg.chan].readAll((char*)buf, msg.len);
        _datamu[msg.chan].unlock();
        obj->onRecv(buf, msg.len);
        obj->freeRecv(buf, msg.len);
      }
      break;
    }
  case MessageTypes::REMOTEOBJECT_SIGNAL:
    {
      obj = DecodeDataPtr<RemoteObject>(msg.dstptr);
      JASSERT(msg.len==0);
      JLOCKSCOPE(*obj);
      obj->signal();
      break;
    }
  case MessageTypes::REMOTEOBJECT_BROADCAST:
    {
      obj = DecodeDataPtr<RemoteObject>(msg.dstptr);
      JASSERT(msg.len==0);
      JLOCKSCOPE(*obj);
      obj->broadcast();
      break;
    }
  case MessageTypes::REMOTEOBJECT_NOTIFY:
    {
      obj = DecodeDataPtr<RemoteObject>(msg.dstptr);
      JASSERT(msg.len==0);
      JLOCKSCOPE(*obj);
      obj->onNotify(msg.srcptr);
      break;
    }
  case MessageTypes::REMOTEOBJECT_MARKCOMPLETE:
    {
      obj = DecodeDataPtr<RemoteObject>(msg.dstptr);
      JASSERT(msg.len==0);
      JLOCKSCOPE(*obj);
      obj->onComplete();
      obj->markCompleteMu();
      break;
    }
  default:
    JASSERT(false);
  }
  return true;
}

void petabricks::RemoteHost::sendMsg(GeneralMessage* msg, const void* data, size_t len) {
  int chan;
  _controlmu.lock();
  if(len>0){
    chan = msg->chan = pickChannel();
  }else{
    chan = msg->chan = 0;
  }
  msg->len = len;
  _control.writeAll((const char*)msg, sizeof(GeneralMessage));
  if(len>0){
    _datamu[chan].lock();
    _controlmu.unlock();
    _data[chan].writeAll((const char*)data, len);
    _datamu[chan].unlock();
  }else{
    _controlmu.unlock();
  }
}

void petabricks::RemoteHost::createRemoteObject(const RemoteObjectPtr& local,
                                                RemoteObjectGenerator remote,
                                                const void* data, size_t len){
  local->markInitiatorMu();
  local->setHostMu(this);
  GeneralMessage msg = { MessageTypes::REMOTEOBJECT_CREATE,
                         0,
                         len,
                         EncodeDataPtr(local.asPtr()),
                         EncodeTextPtr(remote) };
  sendMsg(&msg, data, len);
  JLOCKSCOPE(_controlmu);
  _objects.push_back(local);
}
  
void petabricks::RemoteHost::sendData(const RemoteObject* local, const void* data, size_t len) {
  local->waitUntilCreated();
  GeneralMessage msg = { MessageTypes::REMOTEOBJECT_DATA,
                         0,
                         len,
                         EncodeDataPtr(local),
                         local->remoteObj() };
  sendMsg(&msg, data, len);
}
void petabricks::RemoteHost::remoteSignal(const RemoteObject* local) {
  local->waitUntilCreated();
  GeneralMessage msg = { MessageTypes::REMOTEOBJECT_SIGNAL,
                         0,
                         0,
                         EncodeDataPtr(local),
                         local->remoteObj() };
  sendMsg(&msg);
}
void petabricks::RemoteHost::remoteBroadcast(const RemoteObject* local) {
  local->waitUntilCreated();
  GeneralMessage msg = { MessageTypes::REMOTEOBJECT_BROADCAST,
                         0,
                         0,
                         EncodeDataPtr(local),
                         local->remoteObj() };
  sendMsg(&msg);
}
void petabricks::RemoteHost::remoteMarkComplete(const RemoteObject* local) {
  local->waitUntilCreated();
  GeneralMessage msg = { MessageTypes::REMOTEOBJECT_MARKCOMPLETE,
                         0,
                         0,
                         EncodeDataPtr(local),
                         local->remoteObj() };
  sendMsg(&msg);
}
void petabricks::RemoteHost::remoteNotify(const RemoteObject* local, int arg) {
  local->waitUntilCreated();
  GeneralMessage msg = { MessageTypes::REMOTEOBJECT_NOTIFY,
                         0,
                         0,
                         arg, //pack the arg in the srcptr field
                         local->remoteObj() };
  sendMsg(&msg);
}

petabricks::RemoteHostDB::RemoteHostDB()
  : _port(LISTEN_PORT_FIRST),
    _listener(jalib::JSockAddr::ANY, LISTEN_PORT_FIRST),
    _nfds(0),
    _ready(0),
    _fds(NULL)
{
  while(!_listener.isValid()) {
    JTRACE("trying next port")(_port);
    JASSERT(_port < LISTEN_PORT_FIRST+512)(_port);
    _listener = jalib::JServerSocket(jalib::JSockAddr::ANY, ++_port);
  }
  _listener.enablePortReuse();
  char buf[1024];
  JASSERT(gethostname(buf, sizeof buf) >= 0);
  _host = buf;
}

void petabricks::RemoteHostDB::accept(){
  JLOCKSCOPE(_mu);
  RemoteHostPtr h = new RemoteHost();
  h->accept(_listener);
  _hosts.push_back(h);
  regenPollFds();
}

void petabricks::RemoteHostDB::connect(const char* host, int port){
  JLOCKSCOPE(_mu);
  RemoteHostPtr h = new RemoteHost();
  h->connect(host, port);
  _hosts.push_back(h);
  regenPollFds();
}

void petabricks::RemoteHostDB::remotefork(const char* host, int oargc, const char** oargv) {
  std::string hoststr = this->host();
  std::string portstr = jalib::XToString(this->port());
  const char** argv = new const char*[oargc+32];
  int i=0;
  if(host!=NULL) { 
    argv[i++] = "ssh";
    argv[i++] = host;
  }
  for(; i<oargc; ++i) argv[i] = oargv[i];
  argv[i++] = hoststr.c_str();
  argv[i++] = portstr.c_str();
  argv[i++] = NULL;
  if(fork()==0){
    for(int i=3; i<1024; ++i) close(i);
    execv(argv[0], (char**)argv);
    JASSERT(false);
  }
}

void petabricks::RemoteHostDB::regenPollFds() {
  delete[] _fds;
  _nfds = _hosts.size();
  _fds = new struct pollfd[_nfds];
  struct pollfd *fd;
  RemoteHostList::iterator i;
  for(i=_hosts.begin(), fd=_fds; i!=_hosts.end(); ++i, ++fd) {
    fd->fd = (*i)->fd();
    fd->events = POLLIN;
    fd->revents = 0;
  }
}
void petabricks::RemoteHostDB::listenLoop() {
  JLOCKSCOPE(_mu);
  struct pollfd *fd;
  RemoteHostList::iterator i;
  for(bool workDone = true; true; workDone = false) {

    for(i=_hosts.begin(), fd=_fds; i!=_hosts.end() && _ready>0; ++i, ++fd) {
      JASSERT(0 == (fd->revents & ~POLLIN))
        ((*i)->id()).Text("connection closed");
      if(0 != (fd->revents & POLLIN)) {
        fd->revents = 0;
        --_ready;
        _mu.unlock();
        if((*i)->recv()){
          workDone = true;
          while((*i)->recv()){}
        }
        _mu.lock();
      }
    }

    if(!workDone) {
      _mu.unlock();
      pthread_yield();
      _mu.lock();
    }

    if(_ready == 0) {
      _ready=poll(_fds, _nfds, -1);
    }
    JASSERT(_ready>0);
  }

}

void petabricks::RemoteHostDB::spawnListenThread() {
  pthread_t t;
  JASSERT(0==pthread_create(&t, 0, start_listenLoop, this));
  JASSERT(0==pthread_detach(t));
}

