/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/
#include "petabricksruntime.h"

#include "remotehost.h"

using namespace petabricks;

PetabricksRuntime::Main* petabricksMainTransform(){
  return NULL;
}
PetabricksRuntime::Main* petabricksFindTransform(const std::string& ){
  return NULL;
}

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
    hdb.accept();
    hdb.spawnListenThread();
    hdb.spawnListenThread();

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
    return 0;
  }else{
    JASSERT(argc==3);
    hdb.connect(argv[1], jalib::StringToInt(argv[2]));
    hdb.spawnListenThread();
    hdb.listenLoop();
    return 0;
  }
}


