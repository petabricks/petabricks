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

#include "distributedgc.h"


petabricks::RemoteObjectPtr petabricks::DistributedGC::gen() {
  return new DistributedGC();
}
    
void petabricks::DistributedGC::onCreated() {
  JTRACE("Areated");
  host()->swapObjects(_objects, _gen);
  JTRACE("Breated");
  remoteNotify(FLUSH_MSGS);
  JTRACE("Created");
}

void petabricks::DistributedGC::onNotify(int stage) {
  if(stage == FLUSH_MSGS) {
    JTRACE("flushed");
    remoteNotify(DO_SCAN);
  }else if(stage == DO_SCAN) {
    std::vector<EncodedPtr> response;
    scan(response);
    if(! response.empty() ) {
      unlock();
      send(&response[0], sizeof(EncodedPtr)*response.size());
      lock();
    }else{
      remoteNotify(ABORT_GC);
      finishup();
    }
    JTRACE("scanned");
  }else if(stage == ABORT_GC) {
    finishup();
    JTRACE("aborted");
  }else UNIMPLEMENTED();
}

void petabricks::DistributedGC::scan(std::vector<EncodedPtr>& response) {
  RemoteObjectList tmp;
  RemoteObjectList::iterator i;
  tmp.swap(_objects);
  for(i=tmp.begin(); i!=tmp.end(); ++i) {
    if(canDeleteLocal(*(*i))){
      response.push_back((*i)->remoteObj());
      _objectsMaybeDead.push_back(*i);
    }else{
      _objects.push_back(*i);
    }
  }
}

void petabricks::DistributedGC::onRecv(const void* buf, size_t s) {
  const EncodedPtr* begin = reinterpret_cast<const EncodedPtr*>(buf);
  const EncodedPtr* end   = begin+(s/sizeof(EncodedPtr));
  std::set<EncodedPtr> remoteDead(begin, end);

  RemoteObjectList::iterator i;
  for(i=_objectsMaybeDead.begin(); i!=_objectsMaybeDead.end(); ++i) {
    if(remoteDead.find(host()->asEncoded(i->asPtr())) == remoteDead.end()) {
      _objects.push_back(*i);
    }else{
#ifdef DEBUG
      JASSERT(canDeleteLocal(*(*i)));
#endif
    }
  }
  JTRACE("DistributedGC deleting objects")(_objectsMaybeDead.size());
  _objectsMaybeDead.clear();

  finishup();
}

void petabricks::DistributedGC::finishup() {
    host()->readdObjects(_objects);
    JASSERT(_objects.empty());
    host()->readdObjects(_objectsMaybeDead);
    JASSERT(_objectsMaybeDead.empty());
    markCompleteMu();
}

bool petabricks::DistributedGC::canDeleteLocal(RemoteObject& obj) const {
  bool deleteme = false;
  if(obj.isCreated() && obj.refCount()==1 && obj.lastMsgGen()<_gen) {
    if(obj.trylock()) {
      deleteme=(obj.isCreated() && obj.refCount()==1 && obj.lastMsgGen()<_gen);
      obj.unlock();
    }
  }
  return deleteme;
}

