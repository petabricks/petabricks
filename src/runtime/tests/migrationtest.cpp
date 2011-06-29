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

void print(DataHostList list) {
  printf("(%d) DataHostList\n", getpid());
  for (unsigned int i = 0; i < list.size(); i++) {
    printf("  %lx/%d ==> %.5g\n", list[i].hostPid.hostid, list[i].hostPid.pid, list[i].weight);
  }
}

RemoteObjectPtr gen() {
  class TestRemoteObject : public petabricks::RemoteObject {
  public:
    void onRecv(const void* data, size_t len) {
      JTRACE("recv")((char*)data)(len);
      MatrixRegion2D regionMatrix = MatrixRegion2D();
      regionMatrix.unserialize((char*)data, *host());
      MatrixIO().write(regionMatrix);
      printf("== done ==\n");
    }
  };
  return new TestRemoteObject();
}

int main(int argc, const char** argv){
  using namespace petabricks::distributed;

  const char* filename = "testdata/Helmholtz3DB1";
  RemoteHostDB hdb;

  if(argc==1){
    printf("process1 %d\n", getpid());

    hdb.remotefork(NULL, argc, argv);
    hdb.accept("");
    hdb.spawnListenThread();
    hdb.spawnListenThread();

    MatrixRegion3D regionMatrix = MatrixIO(filename,"r").read_distributed<3>();

    IndexT m123[] = {1,2,3};
    IndexT m456[] = {4,5,6};
    regionMatrix = regionMatrix.region(m123, m456);

    MatrixRegion2D slice = regionMatrix.slice(2, 0);
    slice = slice.transposed();

    MatrixIO().write(slice);

    char* buf = new char[slice.serialSize()];
    slice.serialize(buf, *hdb.host(0));

    // send buf to process 2
    RemoteObjectPtr local;
    hdb.host(0)->createRemoteObject(local=gen(), &gen);
    local->waitUntilCreated();
    local->send(buf, regionMatrix.serialSize());

    printf("proc 1 done\n");

    hdb.listenLoop();
    return 0;
  } else {
    printf("process2 %d\n", getpid());

    JASSERT(argc==3);
    hdb.connect(argv[1], jalib::StringToInt(argv[2]));
    hdb.spawnListenThread();
    hdb.listenLoop();
    return 0;
  }
}

