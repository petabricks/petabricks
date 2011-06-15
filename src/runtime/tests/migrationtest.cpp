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

int main(int argc, const char** argv){
  const char* filename = "testdata/Helmholtz3DB1";
  RemoteHostDB hdb;

  IndexT size[] = {8,9,8};
  //
  // Create a RegionMatrix
  MatrixRegion3D regionMatrix(size);

  if(argc==1){
    printf("process1 %d\n", getpid());

    hdb.remotefork(NULL, argc, argv);
    hdb.accept();
    hdb.spawnListenThread();
    hdb.spawnListenThread();
    hdb.spawnListenThread();
    hdb.spawnListenThread();

    // import data
    regionMatrix.importDataFromFile(filename);
    print(regionMatrix.dataHosts());
    JASSERT(regionMatrix.isLocal()).Text("Must be local.");


    //
    // Test 1 -> 2 -> 1
    //

    regionMatrix.moveToRemoteHost(hdb.host(0), 1);
    printf("completed sending 1\n");

    // Wait until process2 move matrix back
    regionMatrix = MatrixRegion3D(RegionMatrix(3, size, new RegionHandler(3)));
    regionMatrix.updateHandler(2);
    printf("received 2\n");
    print(regionMatrix.dataHosts());

    regionMatrix.updateHandlerChain();
    JASSERT(regionMatrix.isLocal()).Text("Must be local.");

    printf("==== 1 -> 2 -> 1 Done ====\n");

    //
    // Test 1 -> 2 -> 1 -> 2
    //

    regionMatrix.moveToRemoteHost(hdb.host(0), 3);
    regionMatrix = MatrixRegion3D(RegionMatrix(3, size, new RegionHandler(3)));
    regionMatrix.updateHandler(4);
    regionMatrix.moveToRemoteHost(hdb.host(0), 5);

    hdb.listenLoop();
    return 0;
  } else {
    printf("process2 %d\n", getpid());

    JASSERT(argc==3);
    hdb.connect(argv[1], jalib::StringToInt(argv[2]));
    hdb.spawnListenThread();
    hdb.spawnListenThread();
    hdb.spawnListenThread();
    hdb.spawnListenThread();

    //
    // Test 1 -> 2 -> 1
    //

    // Wait until process1 send matrix here
    regionMatrix.updateHandler(1);
    printf("received 1\n");
    print(regionMatrix.dataHosts());

    regionMatrix.updateHandlerChain();
    JASSERT(!regionMatrix.isLocal()).Text("updateHandlerChain should not be able to do anything.");

    // Move it back to process1
    regionMatrix.moveToRemoteHost(hdb.host(0), 2);
    printf("completed sending 2\n");

    //
    // Test 1 -> 2 -> 1 -> 2
    //

    regionMatrix = MatrixRegion3D(RegionMatrix(3, size, new RegionHandler(3)));
    regionMatrix.updateHandler(3);
    regionMatrix.moveToRemoteHost(hdb.host(0), 4);
    regionMatrix = MatrixRegion3D(RegionMatrix(3, size, new RegionHandler(3)));
    regionMatrix.updateHandler(5);

    regionMatrix.updateHandlerChain();


    printf("==== 1 -> 2 -> 1 -> 2 Done ====\n");


    hdb.listenLoop();
    return 0;
  }
}

