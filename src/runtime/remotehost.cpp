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

#include "remotehost.h"

#include "common/jconvert.h"

#include <algorithm>
#include <poll.h>

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
    return -reinterpret_cast<EncodedPtr>(p);
  }

  template<typename T>
  T* DecodeDataPtr(EncodedPtr p) {
    return reinterpret_cast<T*>(-p);
  }
  
  using petabricks::HostPid;
  struct MessageTypes {
    enum {
      HELLO_CONTROL= 0xf00d,
      HELLO_DATA,
      REMOTEOBJECT_CREATE,
      REMOTEOBJECT_CREATE_ACK,
      REMOTEOBJECT_DATA,
      REMOTEOBJECT_SIGNAL,
      REMOTEOBJECT_BROADCAST,
      REMOTEOBJECT_NOTIFY,
      REMOTEOBJECT_MARKCOMPLETE,
    };
  };

  struct HelloMessage {
    MessageType type;
    HostPid     id;
    ChanNumber  chan;
  };


  struct GeneralMessage {
    MessageType type;
    ChanNumber  chan;
    DataLen     len;
    EncodedPtr  srcptr;
    EncodedPtr  dstptr;
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
  _control.writeAll((char*)&msg, sizeof msg);
  _control.readAll((char*)&msg, sizeof msg);
  JASSERT(msg.type == MessageTypes::HELLO_CONTROL && msg.id != self && msg.chan == REMOTEHOST_DATACHANS);
  _id = msg.id;
  
  for(int i=0; i<REMOTEHOST_DATACHANS; ++i) {
    HelloMessage dmsg = { MessageTypes::HELLO_DATA, self, i};
    JASSERT(_control.writeAll((char*)&dmsg, sizeof dmsg) == sizeof dmsg);
    JASSERT(_control.readAll((char*)&dmsg, sizeof dmsg) == sizeof dmsg);
    JASSERT(dmsg.type == MessageTypes::HELLO_DATA
        && dmsg.id == _id 
        && dmsg.chan == i);
  }
  JTRACE("connection established")(msg.id);
}


bool petabricks::RemoteHost::recv() {
  GeneralMessage msg;

  if(!_controlmu.trylock()) {
    JTRACE("skipping recv, locked");
    return false;
  }

  ssize_t cnt = _control.tryReadAll((char*)&msg, sizeof msg);
  if(cnt==0) {
    JTRACE("skipping recv, no pending data");
    _controlmu.unlock();
    return false;
  }
  JASSERT(cnt==sizeof msg);

  if(msg.len>0){
    JASSERT(msg.chan>=0 && msg.chan<REMOTEHOST_DATACHANS);
    _datamu[msg.chan].lock();
  }
  _controlmu.unlock();
  RemoteObjectGenerator gen = 0;
  RemoteObjectPtr obj = 0;
  void* buf = 0;

  JTRACE("incoming msg")(msg.type)(msg.len)(msg.chan)(msg.srcptr)(msg.dstptr);

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
  JTRACE("outgoing msg")(msg->type)(msg->len)(msg->chan)(msg->srcptr)(msg->dstptr);
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
  : _host("localhost"),
    _port(LISTEN_PORT_FIRST),
    _listener(jalib::JSockAddr::ANY, LISTEN_PORT_FIRST)
{
  while(!_listener.isValid()) {
    JTRACE("trying next port")(_port);
    JASSERT(_port<LISTEN_PORT_FIRST+512)(_port);
    _listener = jalib::JServerSocket(jalib::JSockAddr::ANY, ++_port);
  }
}

void petabricks::RemoteHostDB::accept(){
  JLOCKSCOPE(_mu);
  RemoteHostPtr h = new RemoteHost();
  h->accept(_listener);
  _hosts.push_back(h);
}

void petabricks::RemoteHostDB::connect(const char* host, int port){
  JLOCKSCOPE(_mu);
  RemoteHostPtr h = new RemoteHost();
  h->connect(host, port);
  _hosts.push_back(h);
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
    execv(argv[0], (char**)argv);
    JASSERT(false);
  }
}

void petabricks::RemoteHostDB::listenLoop() {
  JLOCKSCOPE(_mu);
  RemoteHostList hosts = _hosts;
  RemoteHostList::iterator i;
  std::random_shuffle(hosts.begin(), hosts.end());

  nfds_t nfds = hosts.size();
  struct pollfd *fd;
  struct pollfd *fds = new struct pollfd[nfds];
  for(i=hosts.begin(), fd=fds; i!=hosts.end(); ++i, ++fd) {
    fd->fd = (*i)->fd();
    fd->events = POLLIN | POLLERR | POLLHUP | POLLNVAL;
    fd->revents = 0;
  }

  for(;;) {
    bool workDone = false; 

    for(i=hosts.begin(), fd=fds; i!=hosts.end(); ++i, ++fd) {
      if(0 != (fd->revents & (POLLERR|POLLHUP|POLLNVAL))) {
        JASSERT(false)((*i)->id()).Text("connection closed");
      }
      if(0 != (fd->revents & POLLIN)) {
        _mu.unlock();
        workDone |= (*i)->recv();
        _mu.lock();
      }
    }

    if(!workDone) {
      _mu.unlock();
      pthread_yield();
      _mu.lock();
    }

    JASSERT(poll(fds, nfds, -1) >= 0);
  }

  delete[] fds;
}

void petabricks::RemoteHostDB::spawnListenThread() {
  pthread_t t;
  JASSERT(0==pthread_create(&t, 0, start_listenLoop, this));
  JASSERT(0==pthread_detach(t));
}

