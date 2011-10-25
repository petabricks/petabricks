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

#include "petabricks.h"

#include "regionmatrix.h"
#include "remotehost.h"

using namespace petabricks;
using namespace petabricks::distributed;

PetabricksRuntime::Main* petabricksMainTransform(){
  return NULL;
}
PetabricksRuntime::Main* petabricksFindTransform(const std::string& ){
  return NULL;
}
void _petabricksInit() {}
void _petabricksCleanup() {}

void print(DataHostPidList list) {
  printf("(%d) DataHostPidList\n", getpid());
  for (unsigned int i = 0; i < list.size(); i++) {
    printf("  %lx/%d ==> %.5g\n", list[i].hostPid.hostid, list[i].hostPid.pid, list[i].weight);
  }
}

RemoteObjectPtr step2();
RemoteObjectPtr step3();
RemoteObjectPtr step4();
RemoteObjectPtr step5();
RemoteObjectPtr step6();

int main(int argc, const char** argv){
  using namespace petabricks::distributed;

  if(argc==1){
    printf("process1 %d\n", getpid());

    RemoteHostDB::instance().remotefork(NULL, argc, argv);
    RemoteHostDB::instance().accept("");
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();

    const char* filename = "testdata/Helmholtz3DB1";
    MatrixRegion3D regionMatrix = MatrixIO(filename,"r").read_distributed<3>();

    IndexT m123[] = {1,2,3};
    IndexT m456[] = {4,5,6};
    regionMatrix = regionMatrix.region(m123, m456);

    MatrixRegion2D slice = regionMatrix.slice(2, 0);
    slice = slice.transposed();

    MatrixIO().write(slice);

    char* buf = new char[slice.serialSize()];
    slice.serialize(buf, *RemoteHostDB::instance().host(0));

    // send buf to process 2
    RemoteObjectPtr local;
    RemoteHostDB::instance().host(0)->createRemoteObject(local=step2(), &step2);
    local->waitUntilCreated();
    local->send(buf, slice.serialSize());

    printf("== step 1 done ==\n");

    RemoteHostDB::instance().listenLoop();
    return 0;
  } else {
    printf("process2 %d\n", getpid());

    JASSERT(argc==3);
    RemoteHostDB::instance().connect(argv[1], jalib::StringToInt(argv[2]));
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().listenLoop();
    return 0;
  }
}

RemoteObjectPtr step2() {
  class TestRemoteObject : public petabricks::RemoteObject {
  public:
    void onRecv(const void* data, size_t /*len*/) {
      printf("== step2 ==\n");
      MatrixRegion2D regionMatrix = MatrixRegion2D();
      regionMatrix.unserialize((char*)data, *host());
      regionMatrix.createRegionHandler(*host());
      MatrixIO().write(regionMatrix);

      IndexT m11[] = {1,1};
      regionMatrix.cell(m11) = 1331;
      MatrixIO().write(regionMatrix);

      printf("== step2 done ==\n");

      char* buf = new char[regionMatrix.serialSize()];
      regionMatrix.serialize(buf, *RemoteHostDB::instance().host(0));
      RemoteObjectPtr local;
      RemoteHostDB::instance().host(0)->createRemoteObject(local=step3(), &step3);
      local->waitUntilCreated();
      local->send(buf, regionMatrix.serialSize());

      printf("== sent matrix to step3 ==\n");
      RemoteHostDB::instance().listenLoop();
    }
  };
  return new TestRemoteObject();
}

RemoteObjectPtr step3() {
  class TestRemoteObject : public petabricks::RemoteObject {
  public:
    void onRecv(const void* data, size_t /*len*/) {
      printf("== step3 ==\n");
      MatrixRegion2D regionMatrix = MatrixRegion2D();
      regionMatrix.unserialize((char*)data, *host());
      regionMatrix.createRegionHandler(*host());
      MatrixIO().write(regionMatrix);

      regionMatrix.updateHandlerChain();
      JASSERT(regionMatrix.isLocal());
      MatrixIO().write(regionMatrix._toLocalRegion());

      printf("== pass updateHandlerChain for ( 1-> 2 -> 1 ==> local ) ==\n");

      char* buf = new char[regionMatrix.serialSize()];
      regionMatrix.serialize(buf, *RemoteHostDB::instance().host(0));
      RemoteObjectPtr local;
      RemoteHostDB::instance().host(0)->createRemoteObject(local=step4(), &step4);
      local->waitUntilCreated();
      local->send(buf, regionMatrix.serialSize());

      printf("== sent matrix to step4 ==\n");
      RemoteHostDB::instance().listenLoop();
    }
  };
  return new TestRemoteObject();
}

RemoteObjectPtr step4() {
  class TestRemoteObject : public petabricks::RemoteObject {
  public:
    void onRecv(const void* data, size_t /*len*/) {
      printf("== step4 ==\n");
      MatrixRegion2D regionMatrix = MatrixRegion2D();
      regionMatrix.unserialize((char*)data, *host());
      regionMatrix.createRegionHandler(*host());

      char* buf = new char[regionMatrix.serialSize()];
      regionMatrix.serialize(buf, *RemoteHostDB::instance().host(0));
      RemoteObjectPtr local;
      RemoteHostDB::instance().host(0)->createRemoteObject(local=step5(), &step5);
      local->waitUntilCreated();
      local->send(buf, regionMatrix.serialSize());

      printf("== sent matrix to step5 ==\n");
      RemoteHostDB::instance().listenLoop();
    }
  };
  return new TestRemoteObject();
}

RemoteObjectPtr step5() {
  class TestRemoteObject : public petabricks::RemoteObject {
  public:
    void onRecv(const void* data, size_t /*len*/) {
      printf("== step5 ==\n");
      MatrixRegion2D regionMatrix = MatrixRegion2D();
      regionMatrix.unserialize((char*)data, *host());
      regionMatrix.createRegionHandler(*host());

      char* buf = new char[regionMatrix.serialSize()];
      regionMatrix.serialize(buf, *RemoteHostDB::instance().host(0));
      RemoteObjectPtr local;
      RemoteHostDB::instance().host(0)->createRemoteObject(local=step6(), &step6);
      local->waitUntilCreated();
      local->send(buf, regionMatrix.serialSize());

      printf("== sent matrix to step6 ==\n");
      RemoteHostDB::instance().listenLoop();
    }
  };
  return new TestRemoteObject();
}

RemoteObjectPtr step6() {
  class TestRemoteObject : public petabricks::RemoteObject {
  public:
    void onRecv(const void* data, size_t /*len*/) {
      printf("== step6 ==\n");
      MatrixRegion2D regionMatrix = MatrixRegion2D();
      regionMatrix.unserialize((char*)data, *host());
      regionMatrix.createRegionHandler(*host());
      MatrixIO().write(regionMatrix);

      print(regionMatrix.dataHosts());
      JASSERT(!regionMatrix.regionHandler()->isHandlerChainUpdated());

      regionMatrix.updateHandlerChain();
      JASSERT(regionMatrix.regionHandler()->isHandlerChainUpdated());
      MatrixIO().write(regionMatrix);

      printf("== pass updateHandlerChain for ( 1-> 2 -> 1 -> 2 ==> 1 -> 2 ) ==\n");

      printf("== done ==\n");
    }
  };
  return new TestRemoteObject();
}

