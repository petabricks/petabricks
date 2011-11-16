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

#include "distributedgc.h"

#include "common/jconvert.h"

#include "workerthread.h"


#include <algorithm>
#include <poll.h>
#include <unistd.h>

#include <set>

static bool theListenersShutdown = false;

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
      SETUP_CONNECT,
      SETUP_ACCEPT,
      SETUP_ACK,
      SETUP_END,
      REMOTEOBJECT_CREATE,
      REMOTEOBJECT_CREATE_ACK,
      REMOTEOBJECT_DATA,
      REMOTEOBJECT_SIGNAL,
      REMOTEOBJECT_BROADCAST,
      REMOTEOBJECT_NOTIFY,
      REMOTEOBJECT_MARKCOMPLETE,
      SHUTDOWN_BEGIN,
      SHUTDOWN_ACK,
      SHUTDOWN_END,
    };
    static const char* str(int t) {
      switch(t) {
#define  EXPSTR(s) case s: return #s
        EXPSTR(HELLO_CONTROL);
        EXPSTR(HELLO_DATA);
        EXPSTR(SETUP_CONNECT);
        EXPSTR(SETUP_ACCEPT);
        EXPSTR(SETUP_ACK);
        EXPSTR(SETUP_END);
        EXPSTR(REMOTEOBJECT_CREATE);
        EXPSTR(REMOTEOBJECT_CREATE_ACK);
        EXPSTR(REMOTEOBJECT_DATA);
        EXPSTR(REMOTEOBJECT_SIGNAL);
        EXPSTR(REMOTEOBJECT_BROADCAST);
        EXPSTR(REMOTEOBJECT_NOTIFY);
        EXPSTR(REMOTEOBJECT_MARKCOMPLETE);
        EXPSTR(SHUTDOWN_BEGIN);
        EXPSTR(SHUTDOWN_ACK);
        EXPSTR(SHUTDOWN_END);
#undef EXPSTR
        default: return "INVALID";
    }
  }
  };

  struct HelloMessage {
    uint16_t    type;
    HostPid     id;
    ChanNumber  chan;
    int         port;
    int         roll;

    friend std::ostream& operator<<(std::ostream& o, const HelloMessage& m) {
      return o << "HelloMessage("
               << MessageTypes::str(m.type) << ", "
               << m.id << ")";
    }
  } PACKED;


  struct SetupMessage {
    uint16_t type;
    char     host[1024];
    int      port;
    
    friend std::ostream& operator<<(std::ostream& o, const SetupMessage& m) {
      return o << "SetupMessage("
               << MessageTypes::str(m.type) << ", "
               << m.host << ", "
               << m.port << ")";
    }
  } PACKED;

  struct SetupAckMessage {
    uint16_t type;

    friend std::ostream& operator<<(std::ostream& o, const SetupAckMessage& m) {
      return o << "SetupAckMessage("
               << MessageTypes::str(m.type) << ")";
    }
  } PACKED;


  struct GeneralMessage {
    uint16_t    type;
    uint16_t    chan;
    DataLen     len;
    EncodedPtr  arg;
    EncodedPtr  srcptr;
    EncodedPtr  dstptr;

    friend std::ostream& operator<<(std::ostream& o, const GeneralMessage& m) {
      return o << "GeneralMessage("
               << MessageTypes::str(m.type) << ", "
               << m.len << " bytes, "
               << m.arg << " arg, "
               << std::hex << m.srcptr << " => " << m.dstptr << std::dec << ")";
    }
  } PACKED;

  void* start_listenLoop(void* arg) {
    petabricks::WorkerThread::markUtilityThread();
    ((petabricks::RemoteHostDB*)arg)->listenLoop();
    return NULL;
  }
}
using namespace _RemoteHostMsgTypes;

const HostPid& petabricks::HostPid::self() {
  static HostPid selfObj = { gethostid(), 0 };
  selfObj.pid = getpid();//pid may change with fork()
  return selfObj;
}

void petabricks::RemoteHost::accept(jalib::JServerSocket& s, int listenPort) {
  _control.close();
  _control = s.accept();
  JASSERT(_control.isValid());
  for(int i=0; i<REMOTEHOST_DATACHANS; ++i) {
    _data[i].close();
    _data[i] = s.accept();
    JASSERT(_data[i].isValid());
  }
  _lastchan = 1;
  handshake(listenPort);
}

void petabricks::RemoteHost::connect(const jalib::JSockAddr& a, int p, int listenPort) {
  JASSERT(_control.connect(a, p));
  for(int i=0; i<REMOTEHOST_DATACHANS; ++i) {
    JASSERT(_data[i].connect(a, p));
  }
  _lastchan = 0;
  handshake(listenPort);
}

void petabricks::RemoteHost::handshake(int port) {
  HostPid self = HostPid::self();

  //mix our pid into the random roll since lrand48 is often not seeded (in debugging modes)
  unsigned short xsubi[] = { lrand48()^self.pid , lrand48()^self.pid, lrand48()^self.pid } ;
  for(int i=0; i<16; ++i) nrand48(xsubi);
  int myRoll = nrand48(xsubi) % 9000;

  HelloMessage msg = { MessageTypes::HELLO_CONTROL,
                       self,
                       REMOTEHOST_DATACHANS,
                       port,
                       myRoll };
  _control.disableNagle();
  _control.writeAll((char*)&msg, sizeof msg);
  _control.readAll((char*)&msg, sizeof msg);
  JASSERT(msg.type == MessageTypes::HELLO_CONTROL && msg.id != self && msg.chan == REMOTEHOST_DATACHANS);
  _id = msg.id;
  _remotePort = msg.port;

  if(myRoll!=msg.roll)
    _shouldGc = myRoll < msg.roll;
  else
    _shouldGc = self < _id;

  for(int i=0; i<REMOTEHOST_DATACHANS; ++i) {
    HelloMessage dmsg = { MessageTypes::HELLO_DATA, self, i, port, myRoll};
    _data[i].disableNagle();
    JASSERT(_data[i].writeAll((char*)&dmsg, sizeof dmsg) == sizeof dmsg);
    JASSERT(_data[i].readAll((char*)&dmsg, sizeof dmsg) == sizeof dmsg);
    JASSERT(dmsg.type == MessageTypes::HELLO_DATA
        && dmsg.id == _id
        && dmsg.chan == i);
  }
}

void petabricks::RemoteHost::setupLoop(RemoteHostDB& db) {
  JLOCKSCOPE(_controlmu);

  SetupMessage msg;
  SetupAckMessage ackmsg = { MessageTypes::SETUP_ACK };

  for(msg.type=0; msg.type!=MessageTypes::SETUP_END; ) {
    memset(&msg, 0, sizeof msg);
    _control.readAll((char*)&msg, sizeof msg);

    JTRACE("setup slave")(msg);

    switch(msg.type) {
      case MessageTypes::SETUP_CONNECT:
        db.connect(msg.host, msg.port);
        break;
      case MessageTypes::SETUP_ACCEPT:
        db.accept(msg.host);
        break;
      case MessageTypes::SETUP_END:
        break;
      default:
        UNIMPLEMENTED();
    }

    _control.writeAll((char*)&ackmsg, sizeof ackmsg);
  }
}

void petabricks::RemoteHost::setupRemoteConnection(RemoteHost& a, RemoteHost& b) {
  JLOCKSCOPE(a._controlmu);
  JLOCKSCOPE(b._controlmu);

  SetupMessage amsg;
  amsg.type = MessageTypes::SETUP_CONNECT;
  strncpy(amsg.host, a._connectName.c_str(), sizeof amsg.host);
  JASSERT(amsg.host==a._connectName);
  amsg.port = a._remotePort;

  SetupMessage bmsg;
  bmsg.type = MessageTypes::SETUP_ACCEPT;
  strncpy(bmsg.host, b._connectName.c_str(), sizeof bmsg.host);
  JASSERT(bmsg.host==b._connectName);
  bmsg.port = b._remotePort;

  //a goes to b, b goes to a
  a._control.writeAll((char*)&bmsg, sizeof bmsg);
  b._control.writeAll((char*)&amsg, sizeof amsg);


  SetupAckMessage aack;
  SetupAckMessage back;
  a._control.readAll((char*)&aack, sizeof aack);
  b._control.readAll((char*)&back, sizeof back);
  JASSERT(aack.type == MessageTypes::SETUP_ACK);
  JASSERT(back.type == MessageTypes::SETUP_ACK);
}

void petabricks::RemoteHost::setupEnd() {
  JLOCKSCOPE(_controlmu);

  SetupMessage msg;
  memset(&msg, 0, sizeof msg);
  msg.type = MessageTypes::SETUP_END;
  _control.writeAll((char*)&msg, sizeof msg);

  SetupAckMessage ack;
  _control.readAll((char*)&ack, sizeof ack);
  JASSERT(ack.type == MessageTypes::SETUP_ACK);
}



bool petabricks::RemoteHost::recv() {
  GeneralMessage msg;

  if(!_controlmu.trylock()) {
    //JTRACE("skipping recv, locked");
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
    JASSERT(msg.chan<REMOTEHOST_DATACHANS);
    _datamu[msg.chan].lock();
  }
  RemoteObjectPtr obj = 0;
  void* buf = 0;

  if(msg.dstptr != 0) {
    obj = DecodeDataPtr<RemoteObject>(msg.dstptr);
    jalib::atomicIncrement(&obj->_pendingMessages);
    obj->_lastMsgGen = _currentGen;
  }

  _controlmu.unlock();

  if(obj){
    obj->lock();
  }

  switch(msg.type) {
  case MessageTypes::REMOTEOBJECT_CREATE:
    {
      RemoteObjectGenerator gen;
      gen = DecodeTextPtr<RemoteObjectGenerator>(msg.arg);
      obj = (*gen)();
      obj->_pendingMessages += 1;
      obj->_lastMsgGen = _currentGen;
      obj->lock();
      obj->setHostMu(this);
      obj->setRemoteObjMu(msg.srcptr);
      obj->markCreatedMu();
      if(msg.len>0){
        buf = obj->allocRecvInitial(msg.len);
        _data[msg.chan].readAll((char*)buf, msg.len);
        _datamu[msg.chan].unlock();
        obj->onRecvInitial(buf, msg.len);
        obj->freeRecvInitial(buf, msg.len);
      }
      GeneralMessage ackmsg = { MessageTypes::REMOTEOBJECT_CREATE_ACK,
                                0,
                                0,
                                0,
                                EncodeDataPtr(obj.asPtr()), msg.srcptr };
      sendMsg(&ackmsg);
      obj->onCreated();
      addObject(obj);
      break;
    }
  case MessageTypes::REMOTEOBJECT_CREATE_ACK:
    {
      JASSERT(msg.len==0);
      obj->setRemoteObjMu(msg.srcptr);
      obj->markCreatedMu();
      obj->onCreated();
      break;
    }
  case MessageTypes::REMOTEOBJECT_DATA:
    {
      if(msg.len>0){
        buf = obj->allocRecv(msg.len);
        _data[msg.chan].readAll((char*)buf, msg.len);
        _datamu[msg.chan].unlock();
        obj->onRecv(buf, msg.len);
        obj->freeRecv(buf, msg.len);
      }else{
        char dummy;
        obj->onRecv(&dummy, 0);
      }
      break;
    }
  case MessageTypes::REMOTEOBJECT_SIGNAL:
    {
      JASSERT(msg.len==0);
      obj->signal();
      break;
    }
  case MessageTypes::REMOTEOBJECT_BROADCAST:
    {
      JASSERT(msg.len==0);
      obj->broadcast();
      break;
    }
  case MessageTypes::REMOTEOBJECT_NOTIFY:
    {
      JASSERT(msg.len==0);
      obj->onNotify(msg.arg);
      break;
    }
  case MessageTypes::REMOTEOBJECT_MARKCOMPLETE:
    {
      JASSERT(msg.len==0);
      obj->onComplete();
      obj->markCompleteMu();
      break;
    }
  case MessageTypes::SHUTDOWN_BEGIN:
    {
      JLOCKSCOPE(_controlmu);
      { GeneralMessage ackmsg = { MessageTypes::SHUTDOWN_ACK, 0, 0, 0, 0, 0};
        _control.writeAll((const char*)&ackmsg, sizeof(GeneralMessage));
      }
      _control.readAll((char*)&msg, sizeof msg);
      JASSERT(msg.type==MessageTypes::SHUTDOWN_END);
      JTRACE("slave exit")(HostPid::self());
      _exit(0);
      break;
    }
  case MessageTypes::SHUTDOWN_ACK:
    {
      _isShuttingDown=true;
      RemoteHostDB::onShutdownEvent();
      break;
    }
  case MessageTypes::SHUTDOWN_END:
    {
      JASSERT(false);
      break;
    }
  default:
    JASSERT(false);
  }
  
  if(obj) {
    obj->unlock();
    jalib::atomicDecrement(&obj->_pendingMessages);
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

  if(msg->srcptr != 0) {
    DecodeDataPtr<RemoteObject>(msg->srcptr)->_lastMsgGen = _currentGen;
  }

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
                         EncodeTextPtr(remote),
                         EncodeDataPtr(local.asPtr()),
                         0 };
  sendMsg(&msg, data, len);
  addObject(local);
}

void petabricks::RemoteHost::sendData(const RemoteObject* local, const void* data, size_t len) {
  local->waitUntilCreated();
  GeneralMessage msg = { MessageTypes::REMOTEOBJECT_DATA,
                         0,
                         len,
                         0,
                         EncodeDataPtr(local),
                         local->remoteObj() };
  sendMsg(&msg, data, len);
}
void petabricks::RemoteHost::remoteSignal(const RemoteObject* local) {
  local->waitUntilCreated();
  GeneralMessage msg = { MessageTypes::REMOTEOBJECT_SIGNAL,
                         0,
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
                         arg,
                         0,
                         local->remoteObj() };
  sendMsg(&msg);
}

void petabricks::RemoteHost::shutdownBegin() {
  GeneralMessage msg = { MessageTypes::SHUTDOWN_BEGIN,
                         0,
                         0,
                         0,
                         0,
                         0 };
  sendMsg(&msg);
}

void petabricks::RemoteHost::shutdownEnd() {
  GeneralMessage msg = { MessageTypes::SHUTDOWN_END,
                         0,
                         0,
                         0,
                         0,
                         0 };
  _controlmu.lock();
  _control.writeAll((char*)&msg, sizeof msg);
}

void petabricks::RemoteHost::swapObjects(RemoteObjectList& obj, int& gen) {
  JLOCKSCOPE(_controlmu);
  ++_currentGen;
  gen = _currentGen;
  _objects.swap(obj);
  _gcLastLiveObjCount = _objects.size();
}


void petabricks::RemoteHost::readdObjects(RemoteObjectList& obj) {
  if(obj.empty()) return;
  {
    JLOCKSCOPE(_controlmu);
    if(_objects.empty()) {
      _objects.swap(obj);
    }else{
      _objects.insert(_objects.end(), obj.begin(), obj.end());
    }
    _gcLastLiveObjCount += obj.size();
  }
  obj.clear();
}

petabricks::EncodedPtr petabricks::RemoteHost::asEncoded(RemoteObject* obj) const {
  return EncodeDataPtr(obj);
}


void petabricks::RemoteHost::spawnGcTask() {
  createRemoteObject(DistributedGC::gen(), &DistributedGC::gen);
}

void petabricks::RemoteHost::addObject(const RemoteObjectPtr& obj) {
  _controlmu.lock();
  _objects.push_back(obj);

  if(_shouldGc && _objects.size()-_gcLastLiveObjCount > DISTRIBUTED_GC_FREQ){
    _gcLastLiveObjCount = _objects.size();
    _controlmu.unlock();
    spawnGcTask();
  }else{
    _controlmu.unlock();
  }
}

petabricks::RemoteHostDB::RemoteHostDB()
  : _port(LISTEN_PORT_FIRST),
    _listener(jalib::JSockAddr::ANY, LISTEN_PORT_FIRST),
    _nfds(0),
    _ready(0),
    _fds(NULL)
{
  while(!_listener.isValid()) {
    //    JTRACE("trying next port")(_port);
    JASSERT(_port < LISTEN_PORT_FIRST+512)(_port);
    _listener = jalib::JServerSocket(jalib::JSockAddr::ANY, ++_port);
  }
  _listener.enablePortReuse();
  char buf[1024];
  JASSERT(gethostname(buf, sizeof buf) >= 0);
  _host = buf;
}

void petabricks::RemoteHostDB::accept(const char* host){
  JLOCKSCOPE(_mu);
  RemoteHostPtr h = new RemoteHost(host);
  h->accept(_listener, _port);
  _hosts.push_back(h);
  regenPollFds();
}

void petabricks::RemoteHostDB::connect(const char* host, int port){
  JLOCKSCOPE(_mu);
  RemoteHostPtr h = new RemoteHost(host);
  h->connect(host, port, _port);
  _hosts.push_back(h);
  regenPollFds();
}

void petabricks::RemoteHostDB::remotefork(const char* host, int oargc, const char** oargv, const char* slavehost, const char* slaveport) {
  std::string hoststr = this->host();
  std::string portstr = jalib::XToString(this->port());
  const char** argv = new const char*[oargc+32];
  int i=0;
  if(host!=NULL) {
    argv[i++] = "ssh";
    argv[i++] = host;
  }
  for(; i<oargc; ++i) argv[i] = oargv[i];
  if(slavehost!=NULL)
    argv[i++] = slavehost;
  argv[i++] = hoststr.c_str();
  if(slaveport!=NULL)
    argv[i++] = slaveport;
  argv[i++] = portstr.c_str();
  argv[i++] = NULL;
  if(fork()==0){
    for(int i=3; i<1024; ++i)
      close(i);
    JTRACE("forked child proc");
    execv(argv[0], (char**)argv);
    JASSERT(false);
  }
  delete[] argv;
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
      if(theListenersShutdown) {
        return;
      }
      pthread_yield();
      _mu.lock();
    }

    if(_ready == 0) {
      _ready=poll(_fds, _nfds, -1);
    }
    JASSERT(_ready>0);
  }

}
  
void petabricks::RemoteHostDB::setupConnectAllPairs() {
  for (unsigned int a = 0; a < _hosts.size(); a++) {
    for (unsigned int b = a+1; b < _hosts.size(); b++) {
      RemoteHost::setupRemoteConnection(*host(a), *host(b));
    }
  }
  for (unsigned int a = 0; a < _hosts.size(); a++) {
    host(a)->setupEnd();
  }
}

void petabricks::RemoteHostDB::spawnListenThread() {
  pthread_t t;
  JASSERT(0==pthread_create(&t, 0, start_listenLoop, this));
  JASSERT(0==pthread_detach(t));
}

petabricks::RemoteHostDB& petabricks::RemoteHostDB::instance() {
  static RemoteHostDB db;
  return db;
}

jalib::JCondMutex theShutdownMu;
void petabricks::RemoteHostDB::onShutdownEvent() {
  JLOCKSCOPE(theShutdownMu);
  theShutdownMu.signal();
}
void petabricks::RemoteHostDB::shutdown() {
  JLOCKSCOPE(theShutdownMu);
  RemoteHostList::iterator i;
  for(i=_hosts.begin(); i!=_hosts.end(); ++i) {
    (*i)->shutdownBegin();
  }
  for(i=_hosts.begin(); i!=_hosts.end(); ++i) {
    while(!(*i)->isShuttingDown()){
      theShutdownMu.wait();
    }
  }
  for(i=_hosts.begin(); i!=_hosts.end(); ++i) {
    (*i)->shutdownEnd();
  }
}



