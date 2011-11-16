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
void _petabricksInit() {}
void _petabricksCleanup() {}

bool compareRegion(RegionIPtr r1, RegionRemote<3>* r2) {
  int dim = r1->dimension();
  
  IndexT* coord = new IndexT[dim];
  memset(coord, 0, (sizeof coord) * dim);
  
  while (true) {
    ElementT v1 = r1->readCell(coord);
    ElementT v2 = r2->readCell(coord);
      
    if (v1 != v2) {
      printf("Error: %4.8g %4.8g\n", v1, v2);
      return false;
    }
    
    if (r1->incCoord(coord) == -1) {
      break;
    }
  }
  
  delete(coord);
  return true;
}

int main(int argc, const char** argv){
  char* filename1 = "testdata/Helmholtz3DZeros";
  char* filename2 = "testdata/Helmholtz3DA";

  RemoteHostDB hdb;
  RemoteObjectPtr local;

  if(argc==1){
    hdb.remotefork(NULL, argc, argv);
    hdb.accept();
    hdb.spawnListenThread();

    RegionRemote<3>* remoteRegion = new RegionRemote<3>();

    hdb.host(0)->createRemoteObject
      (local=RegionRemote<3>::genLocal(remoteRegion),
       &RegionRemote<3>::genRemote, filename1, strlen(filename1));
    local->waitUntilCreated();
    
    remoteRegion->setRemoteObject(local);

    MatrixIO* matrixio = new MatrixIO(filename2, "r");
    RegionIPtr local = matrixio->readToRegionI();
    delete matrixio;
    
    int dim = local->dimension();
    
    IndexT* coord = new IndexT[dim];
    memset(coord, 0, (sizeof coord) * dim);
    
    while (true) {
      ElementT v = local->readCell(coord);
      remoteRegion->writeCell(coord, v);
    
      if (local->incCoord(coord) == -1) {
	break;
      }
    }
    
    delete(coord);

    if (!compareRegion(local, remoteRegion)) {
      remoteRegion->markComplete();
      printf("write failed\n");
      return -1;
    }
    
    remoteRegion->markComplete();
    printf("completed\n");
    return 0;

  } else {
    JASSERT(argc==3);
    hdb.connect(argv[1], jalib::StringToInt(argv[2]));
    hdb.spawnListenThread();
    hdb.listenLoop();
    return 0;
  }
}

