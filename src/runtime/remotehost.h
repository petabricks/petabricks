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

#include "common/jsocket.h"
#include "common/jmutex.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdint.h>
#include <vector>


#define REMOTEHOST_DATACHANS 2

namespace petabricks {

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
public:
  RemoteHost() : _lastchan(0) {}

  void accept(jalib::JServerSocket& s);
  void connect(const jalib::JSockAddr& a, int port);

  void unlockAndRecv(jalib::JMutex& selectmu);

protected:
  void handshake();

private:
  HostPid _id;
  jalib::JSocket _control;
  jalib::JMutex _controlmu;
  jalib::JSocket _data[REMOTEHOST_DATACHANS];
  jalib::JMutex _datamu[REMOTEHOST_DATACHANS];
  int _lastchan;
};


} //namespace petabricks

#endif
