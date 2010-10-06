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


namespace _RemoteHostMsgTypes {

  typedef petabricks::EncodedPtr EncodedPtr;

  typedef uint32_t DataLen;
  typedef int MessageType;
  typedef int ChanNumber;

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
      HELLO_CONTROL= 0xf00d,
      HELLO_DATA,
      CREATE_REMOTEOBJECT,
      CREATE_REMOTEOBJECT_ACK,
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


void petabricks::RemoteHost::unlockAndRecv(jalib::JMutex& selectmu) {
  _controlmu.lock();
  selectmu.unlock();
  GeneralMessage msg;
  _control.readAll((char*)&msg, sizeof msg);
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
  case MessageTypes::CREATE_REMOTEOBJECT: 
    {
      gen = DecodeTextPtr<RemoteObjectGenerator>(msg.dstptr);
      obj = (*gen)();
      JLOCKSCOPE(*obj);
      obj->setHost(this);
      obj->setRemoteObj(msg.srcptr);
      if(msg.len>0){
        buf = obj->allocRecvInitial(msg.len);
        _data[msg.chan].readAll((char*)buf, msg.len);
        _datamu[msg.chan].unlock();
        obj->recvInitial(buf, msg.len);
        obj->freeRecvInitial(buf, msg.len);
      }
      { GeneralMessage ackmsg = { MessageTypes::CREATE_REMOTEOBJECT_ACK, 0, 0, EncodeDataPtr(obj.asPtr()), msg.srcptr };
        sendMsg(&ackmsg);
      }
      obj->created();
      JLOCKSCOPE(_controlmu);
      _objects.push_back(obj);
      break;
    }
  case MessageTypes::CREATE_REMOTEOBJECT_ACK:
    {
      obj = DecodeDataPtr<RemoteObject>(msg.dstptr);
      JASSERT(msg.len==0);
      JLOCKSCOPE(*obj);
      obj->setRemoteObj(msg.srcptr);
      obj->created();
      break;
    }
  default:
    JASSERT(false);
  }
}

void petabricks::RemoteHost::createRemoteObject(const RemoteObjectPtr& local,
                                                RemoteObjectGenerator remote,
                                                const void* data, size_t len){
  local->markInitiator();
  local->setHost(this);
  GeneralMessage msg = { MessageTypes::CREATE_REMOTEOBJECT, 0, len, EncodeDataPtr(local.asPtr()), EncodeTextPtr(remote) };
  sendMsg(&msg, data, len);
  JLOCKSCOPE(_controlmu);
  _objects.push_back(local);
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


