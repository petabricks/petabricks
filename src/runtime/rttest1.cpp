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
#include "petabricksruntime.h"

#include "remotehost.h"
#include "distributedgc.h"

using namespace petabricks;

PetabricksRuntime::Main* petabricksMainTransform(){
  return NULL;
}
PetabricksRuntime::Main* petabricksFindTransform(const std::string& ){
  return NULL;
}
void _petabricksInit() {}
void _petabricksCleanup() {}

RemoteObjectPtr gen() {
  class TestRemoteObject : public petabricks::RemoteObject {
  public:
    void onNotify(int argc) {
      JTRACE("notify")(argc);
      if(argc==1) {
        markComplete();
      }
    }
    void onRecv(const void* data, size_t len) {
      JTRACE("recv")((char*)data)(len);
    }
    ~TestRemoteObject() { 
      JTRACE("destructing");
    }
  };
  return new TestRemoteObject();
}


int main(int argc, const char** argv){
  RemoteHostDB hdb;
  RemoteObjectPtr local;
  RemoteObjectPtr local2;
  char testdata[] = "this is a test string";
  if(argc==1){
    hdb.remotefork(NULL, argc, argv);
    hdb.accept("");
    hdb.spawnListenThread();
    hdb.spawnListenThread();

    hdb.host(0)->createRemoteObject(local=gen(), &gen);
    hdb.host(0)->createRemoteObject(local=gen(), &gen);
    hdb.host(0)->createRemoteObject(local=gen(), &gen);
    local->waitUntilCreated();
    local->send(testdata, sizeof testdata);
    local->send(testdata, sizeof testdata);
    local->send(testdata, sizeof testdata);
    local->remoteNotify(0);
    local->remoteSignal();
    local->remoteBroadcast();
    local->remoteNotify(1);
    local->waitUntilComplete();
    JTRACE("complete");

    RemoteObjectPtr gca=DistributedGC::gen();
    RemoteObjectPtr gcb=DistributedGC::gen();
    RemoteObjectPtr gcc=DistributedGC::gen();

    JTRACE("gcA");
    hdb.host(0)->createRemoteObject(gca, &DistributedGC::gen);
    gca->waitUntilComplete();

    JTRACE("gcB");
    hdb.host(0)->createRemoteObject(gcb, &DistributedGC::gen);
    gcb->waitUntilComplete();
    
    gca=0;
    gcb=0;

    JTRACE("gcC");
    hdb.host(0)->createRemoteObject(gcc, &DistributedGC::gen);
    gcc->waitUntilComplete();


    return 0;
  }else{
    JASSERT(argc==3);
    hdb.connect(argv[1], jalib::StringToInt(argv[2]));
    hdb.spawnListenThread();
    hdb.listenLoop();
    return 0;
  }
}


