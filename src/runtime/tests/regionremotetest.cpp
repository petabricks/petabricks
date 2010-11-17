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

#include "remotehost.h"
#include "regionremote.h"

using namespace petabricks;

PetabricksRuntime::Main* petabricksMainTransform(){
  return NULL;
}
PetabricksRuntime::Main* petabricksFindTransform(const std::string& ){
  return NULL;
}

int main(int argc, const char** argv){
  char* filename = "testdata/Helmholtz3DB1";

  RemoteHostDB hdb;
  RemoteObjectPtr local;

  if(argc==1){
    hdb.remotefork(NULL, argc, argv);
    hdb.accept();
    hdb.spawnListenThread();

    RegionRemote<3>* region = new RegionRemote<3>();

    hdb.host(0)->createRemoteObject
      (local=RegionRemote<3>::genLocal(region),
       &RegionRemote<3>::genRemote, filename, strlen(filename));
    local->waitUntilCreated();
    
    region->setRemoteObject(local);

    MatrixIO* matrixio = new MatrixIO(filename, "r");
    RegionIPtr contiguousRegion = matrixio->readToRegionI();

    int dim = contiguousRegion->dimension();
    
    IndexT* coord = new IndexT[dim];
    memset(coord, 0, (sizeof coord) * dim);
         
    while (true) {
      ElementT v1 = contiguousRegion->readCell(coord);
      ElementT v2 = region->readCell(coord);
      
      if (v1 != v2) {
	printf("%4.8g %4.8g\n", v1, v2);
      }
  
      if (contiguousRegion->incCoord(coord) == -1) {
	break;
      }
    }

    delete(coord);

    region->markComplete();
    printf("complete\n");
    return 0;
  } else {
    JASSERT(argc==3);
    hdb.connect(argv[1], jalib::StringToInt(argv[2]));
    hdb.spawnListenThread();
    hdb.listenLoop();
    return 0;
  }
}

