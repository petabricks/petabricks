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
#ifndef PETABRICKSREMOTEHOST_H
#define PETABRICKSREMOTEHOST_H

#include "remoteobject.h"

#include "common/jmutex.h"
#include "common/jrefcounted.h"
#include "common/jsocket.h"

#include <poll.h>
#include <stdint.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <vector>


#define REMOTEHOST_DATACHANS 4

namespace _RemoteHostMsgTypes {
  struct GeneralMessage;  
}

namespace petabricks {

class RemoteHost;
typedef RemoteHost* RemoteHostPtr;
typedef std::vector<RemoteHostPtr> RemoteHostList;

struct HostPid {
  long hostid;
  pid_t pid;

  friend bool operator == (const HostPid& a, const HostPid& b) {
    return a.hostid==b.hostid && a.pid==b.pid;
  }
  friend bool operator != (const HostPid& a, const HostPid& b) {
    return !operator==(a,b);
  }
  friend std::ostream& operator << (std::ostream& o, const HostPid& a) {
    return o << std::hex << a.hostid << '/' << std::dec <<  a.pid;
  }
};


class RemoteHost {
  friend class RemoteHostDB;
public:

  void createRemoteObject(const RemoteObjectPtr& local,
                          RemoteObjectGenerator remote,
                          const void* data = 0, size_t len = 0);

  void sendData(const RemoteObject* local, const void* data, size_t len);
  void remoteSignal(const RemoteObject* local);
  void remoteBroadcast(const RemoteObject* local);
  void remoteMarkComplete(const RemoteObject* local);
  void remoteNotify(const RemoteObject* local, int arg);

  const HostPid& id() const { return _id; }

protected:
  RemoteHost() : _lastchan(0) {}
  void accept(jalib::JServerSocket& s);
  void connect(const jalib::JSockAddr& a, int port);
  bool recv();
  int fd() const { return _control.sockfd(); }
  void handshake();

  void sendMsg(_RemoteHostMsgTypes::GeneralMessage* msg, const void* data = NULL, size_t len = 0);
  int pickChannel() { 
    _lastchan = (_lastchan+2) % REMOTEHOST_DATACHANS;
    return _lastchan;
  }
private:
  HostPid _id;
  jalib::JSocket _control;
  jalib::JMutex _controlmu;
  jalib::JSocket _data[REMOTEHOST_DATACHANS];
  jalib::JMutex _datamu[REMOTEHOST_DATACHANS];
  int _lastchan;
  RemoteObjectList _objects;
};


class RemoteHostDB {
public:
  RemoteHostDB();

  void connect(const char* host, int port);
  void accept();
  void remotefork(const char* host, int argc, const char** argv);

  void listenLoop();
  void spawnListenThread();

  const char* host() const { return _host.c_str(); }
  int port() const { return _port; }

  RemoteHostPtr host(int i) const {
    return _hosts[i];
  }

protected:

  void regenPollFds();
private:
  jalib::JMutex _mu;
  std::string _host;
  int _port;
  jalib::JServerSocket _listener;
  RemoteHostList _hosts;
  nfds_t _nfds;
  int _ready;
  struct pollfd *_fds;
};


} //namespace petabricks

#endif
