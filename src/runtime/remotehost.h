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
#include <set>


#define REMOTEHOST_DATACHANS 4
#define REMOTEHOST_THREADS 2

namespace _RemoteHostMsgTypes {
  struct GeneralMessage;
}

namespace petabricks {

class RemoteHost;
class RemoteHostDB;
typedef RemoteHost* RemoteHostPtr;
typedef std::vector<RemoteHostPtr> RemoteHostList;
typedef std::set<RemoteHostPtr> RemoteHostSet;

struct RemoteHostWeightListItem {
  RemoteHostPtr host;
  double weight;
};
typedef std::vector<RemoteHostWeightListItem> RemoteHostWeightList;


struct HostPid {
  long hostid;
  pid_t pid;

  friend bool operator == (const HostPid& a, const HostPid& b) {
    return a.hostid==b.hostid && a.pid==b.pid;
  }
  friend bool operator != (const HostPid& a, const HostPid& b) {
    return !operator==(a,b);
  }
  friend bool operator < (const HostPid& a, const HostPid& b) {
    if (a.hostid == b.hostid) return a.pid < b.pid;
    return a.hostid < b.hostid;
  }
  friend std::ostream& operator << (std::ostream& o, const HostPid& a) {
    return o << std::hex << a.hostid << '/' << std::dec <<  a.pid;
  }

  static const HostPid& self();
} PACKED;


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

  void shutdownBegin();
  void shutdownEnd();


  //used by GC:
  void swapObjects(RemoteObjectList& obj, int& gen);
  void readdObjects(RemoteObjectList& obj);
  EncodedPtr asEncoded(RemoteObject* obj) const;

  void setupLoop(RemoteHostDB& db);
  static void setupRemoteConnection(RemoteHost& a, RemoteHost& b);
  void setupEnd();
protected:
  RemoteHost(const std::string& connectName)
    : _lastchan(0),
      _isShuttingDown(false),
      _remotePort(-1),
      _connectName(connectName),
      _currentGen(0),
      _gcLastLiveObjCount(0),
      _shouldGc(false)
  {}
  void accept(jalib::JServerSocket& s, int listenPort);
  void connect(const jalib::JSockAddr& a, int port, int listenPort);
  bool recv();
  int fd() const { return _control.sockfd(); }
  void handshake(int port);

  void sendMsg(_RemoteHostMsgTypes::GeneralMessage* msg, const void* data = NULL, size_t len = 0);
  int pickChannel() {
    _lastchan = (_lastchan+2) % REMOTEHOST_DATACHANS;
    return _lastchan;
  }

  bool isShuttingDown() const { return _isShuttingDown; }
  int remotePort() const { return _remotePort; }


  void spawnGcTask();
  void addObject(const RemoteObjectPtr& obj);

private:
  jalib::JMutex _controlmu;
  jalib::JMutex _datamu[REMOTEHOST_DATACHANS];
  jalib::JSocket _control;
  jalib::JSocket _data[REMOTEHOST_DATACHANS];
  HostPid _id;
  int _lastchan;
  RemoteObjectList _objects;
  bool _isShuttingDown;
  int _remotePort;
  std::string _connectName;
  int _currentGen;
  size_t _gcLastLiveObjCount;
  bool _shouldGc;
};


class RemoteHostDB {
public:
  static RemoteHostDB& instance();

  RemoteHostDB();

  void connect(const char* host, int port);
  void accept(const char* fromhost);
  void remotefork(const char* host, int argc, const char** argv, const char* slavehost=NULL, const char* slaveport=NULL);

  void listenLoop();
  void spawnListenThread();

  const char* host() const { return _host.c_str(); }
  int port() const { return _port; }

  RemoteHostPtr host(int i) const {
    return _hosts[i];
  }

  RemoteHostPtr host(HostPid& id) const {
    for (unsigned int i = 0; i < _hosts.size(); i++) {
      RemoteHostPtr host = _hosts[i];
      if (host->id() == id) {
        return host;
      }
    }
    return NULL;
  }

  int size() const {
    return _hosts.size();
  }

  void shutdown();
  static void onShutdownEvent();

  void setupConnectAllPairs();

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
