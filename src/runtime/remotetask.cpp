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
#include "remotetask.h"

#include "petabricksruntime.h"
#include <map>

void petabricks::RemoteTask::enqueueRemote(RemoteHost& host) {
  size_t len = serialSize();
  char* buf = new char[len];
  serialize(buf, host);
  host.createRemoteObject(new RemoteTaskSender(this), generator(), buf, len);
  delete [] buf;
}

petabricks::RemoteTask::RemoteTask() {
  _state = S_REMOTE_NEW;
  _sender = NULL;
}

void petabricks::RemoteTask::onCompletedRemotely() {
  SubRegionCacheManager::incVersion();
  completeTaskDeps(false);
}

void petabricks::RemoteTask::enqueueLocal() {
  //JTRACE("local scheduled");
  { JLOCKSCOPE(_lock);
    _state = S_READY;
  }
  inlineOrEnqueueTask();
}

void petabricks::RemoteTask::remoteScheduleTask() {
  // JTRACE("remote schedule");

  HostPid toHostPid = HostPid::self();
  size_t maxWeight = 0;

  DataHostPidList hosts;
  getDataHosts(hosts);

  // printf("(%d) DataHostPidList\n", getpid());
  // for (unsigned int i = 0; i < hosts.size(); i++) {
  //   printf("  %lx/%d ==> %ld\n", hosts[i].hostPid.hostid, hosts[i].hostPid.pid, hosts[i].weight);
  // }

  if (hosts.size() == 1) {
    toHostPid = hosts[0].hostPid;

  } else if (hosts.size() > 1) {
    std::map<HostPid, double> map;
    for (DataHostPidList::iterator it = hosts.begin(); it < hosts.end(); it++) {
      map[it->hostPid] += it->weight;
      if (map[it->hostPid] > maxWeight) {
        toHostPid = it->hostPid;
        maxWeight = map[it->hostPid];

      } else if ((map[it->hostPid] == maxWeight) && (it->hostPid == HostPid::self())) {
        toHostPid = it->hostPid;
      }
    }
  }

  // JTRACE("force enqueueRemote");
  // RemoteHostPtr toHost = RemoteHostDB::instance().host(0);
  // enqueueRemote(*toHost);
  // return;

  // JTRACE("force enqueueLocal");
  // enqueueLocal();
  // return;

  if (toHostPid == HostPid::self()) {
    enqueueLocal();

  } else {
#ifdef REGIONMATRIX_TEST
    enqueueLocal();
#else
    RemoteHostPtr toHost = RemoteHostDB::instance().host(toHostPid);
    JDEBUGASSERT(toHost != 0)(toHostPid);
    enqueueRemote(*toHost);
    JTRACE("enqueueRemote")(toHostPid);
#endif
  }
}


