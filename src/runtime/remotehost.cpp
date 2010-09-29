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


namespace {

  typedef ptrdiff_t EncodedPtr;
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
      CREATE_REMOTE_OBJECT,
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

void petabricks::RemoteHost::accept(jalib::JServerSocket& s) {
  _control.close();
  _control = s.accept();
  JASSERT(_control.isValid());
  for(int i=0; i<REMOTEHOST_DATACHANS; ++i) {
    _data[i].close();
    _data[i] = s.accept();
    JASSERT(_data[i].isValid());
  }
  handshake();
}

void petabricks::RemoteHost::connect(const jalib::JSockAddr& a, int p) {
  JASSERT(_control.connect(a, p));
  for(int i=0; i<REMOTEHOST_DATACHANS; ++i) {
    JASSERT(_data[i].connect(a, p));
  }
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
    _lastchan = msg.chan;
  }
  _controlmu.unlock();

  //process msg

  if(msg.len>0){
    _datamu[msg.chan].unlock();
  }
}



