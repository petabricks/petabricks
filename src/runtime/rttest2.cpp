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

/*
  Expected Output:

start
cell 0.57373451
cell 0.51854216
cell 0.92932013
cell 0.63057305
cell 0.57473156
cell  123
SIZE 3 3 3
0.57373451 0.78742994 0.83810736
0.82796525 0.9570284 0.57473156
0.89288054 0.69676226 0.85378168

0.62908688 0.78765918 0.66108902
0.55227836 0.52076837 0.8645779
0.76824321 0.70897682 0.76634859

0.63232559 0.51486057 0.72048933
0.67815482 0.95152374 0.89198544
0.7460758 0.5407854 0.50022465

SIZE 2 2 2
0.52076837 0.8645779
0.70897682 0.76634859

0.95152374 0.89198544
0.5407854 0.50022465

SIZE 2 2
0.52076837 0.8645779
0.70897682 0.76634859

SIZE 2
0.70897682 0.76634859

SIZE 8 8
 123 0.88132748 0.87581202 0.66178823 0.90139657 0.87427579 0.5830398 0.80057525
0.57769847 0.72969966 0.60477567 0.78044858 0.72820768 0.83076063 0.66295958 0.87960024
0.66378681 0.84961301 0.75242225 0.65033857 0.84922928 0.9473998 0.58257957 0.61900267
0.51322678 0.67779715 0.87807468 0.74676728 0.64339147 0.71112322 0.7486735 0.80284584
0.65738401 0.69654637 0.88110737 0.77238595 0.69494902 0.81906444 0.50619642 0.51079598
0.72267932 0.55740811 0.79522263 0.59645541 0.5740906 0.51489046 0.56381163 0.89474906
0.75152776 0.95023563 0.8121331 0.51715425 0.78915763 0.88933146 0.91866404 0.67899394
0.88821071 0.95012511 0.59838045 0.94296641 0.54733715 0.78508079 0.77526635 0.5405974

complete

*/

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
  IndexT m0[] = {0,0,0};
  IndexT m1[] = {1,1,1};
  IndexT m123[] = {1,2,3};
  IndexT m2[] = {2,2,2};
  IndexT m3[] = {3,3,3};
  
  RemoteHostDB hdb;
  RemoteObjectPtr local;

  if(argc==1){
    hdb.remotefork(NULL, argc, argv);
    hdb.accept();
    hdb.spawnListenThread();
    hdb.spawnListenThread();

    RegionRemote<3>* region = new RegionRemote<3>();

    JTRACE("start");
    printf("start\n");
    char* filename = "testdata/Helmholtz3DB1";

    hdb.host(0)->createRemoteObject
      (local=RegionRemote<3>::genLocal(region), &RegionRemote<3>::genRemote, filename, strlen(filename));
    local->waitUntilCreated();
    
    region->setRemoteObject(local);

    // Test read/write
    printf("cell %4.8g\n", region->readCell(m123));
    printf("cell %4.8g\n", region->readCell(m0));
    printf("cell %4.8g\n", region->readCell(m1));
    printf("cell %4.8g\n", region->readCell(m2));
    printf("cell %4.8g\n", region->readCell(m3));

    region->writeCell(m0, 123);
    printf("cell %4.8g\n", region->readCell(m0));


    // Test remote split
    RegionIPtr split3 = region->splitRegion(m123, m3);
    RegionIPtr split2 = split3->splitRegion(m1, m2);
    split3->print();
    split2->print();

    RegionIPtr slice1 = split2->sliceRegion(2, 0);
    slice1->print();

    RegionIPtr slice2 = slice1->sliceRegion(1, 1);
    slice2->print();

    // Test remote slice
    RegionIPtr slice3 = region->sliceRegion(1, 0);
    slice3->print();

    region->markComplete();
    JTRACE("complete");
    printf("complete\n");
    return 0;
  } else {
    JASSERT(argc==3);
    hdb.connect(argv[1], jalib::StringToInt(argv[2]));
    hdb.spawnListenThread();
    hdb.listenLoop();
    return 0;
  }
  

  /*  
  MatrixIO* matrixio = new MatrixIO(argv[1], "r");
  RegionIPtr region = matrixio->readToRegionI();

  RegionIPtr split3 = region->splitRegion(m123, m3);
  RegionIPtr split2 = split3->splitRegion(m1, m2);
  split3->print();
  split2->print();

  RegionIPtr slice1 = split2->sliceRegion(2, 0);
  slice1->print();

  RegionIPtr slice2 = slice1->sliceRegion(1, 1);
  slice2->print();
  */

  return 0;
  
}

