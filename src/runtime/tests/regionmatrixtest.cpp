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

/* Expected Output
before 0.61989714
after    5
RegionMatrix: SIZE 3 3 3
0.57373451 0.78742994 0.83810736
0.82796525 0.9570284 0.57473156
0.89288054 0.69676226 0.85378168

0.62908688 0.78765918 0.66108902
0.55227836 0.52076837 0.8645779
0.76824321 0.70897682 0.76634859

0.63232559 0.51486057 0.72048933
0.67815482 0.95152374 0.89198544
0.7460758 0.5407854 0.50022465

RegionMatrix: SIZE 2 2 2
0.52076837 0.8645779
0.70897682 0.76634859

0.95152374 0.89198544
0.5407854 0.50022465

RegionMatrix: SIZE 2 2
0.52076837 0.8645779
0.70897682 0.76634859

RegionMatrix: SIZE 2
0.70897682 0.76634859

RegionMatrix: SIZE 8 8
0.51854216 0.88132748 0.87581202 0.66178823 0.90139657 0.87427579 0.5830398 0.80057525
0.57769847 0.72969966 0.60477567 0.78044858 0.72820768 0.83076063 0.66295958 0.87960024
0.66378681 0.84961301 0.75242225 0.65033857 0.84922928 0.9473998 0.58257957 0.61900267
0.51322678 0.67779715 0.87807468 0.74676728 0.64339147 0.71112322 0.7486735 0.80284584
0.65738401 0.69654637 0.88110737 0.77238595 0.69494902 0.81906444 0.50619642 0.51079598
0.72267932 0.55740811 0.79522263 0.59645541 0.5740906 0.51489046 0.56381163 0.89474906
0.75152776 0.95023563 0.8121331 0.51715425 0.78915763 0.88933146 0.91866404 0.67899394
0.88821071 0.95012511 0.59838045 0.94296641 0.54733715 0.78508079 0.77526635 0.5405974

completed

cell    5
cell 0.51854216
cell 0.92932013
cell 0.63057305
cell 0.57473156
cell  123

 */

#include "petabricks.h"

#include "regiondataraw.h"
#include "regiondataremote.h"
#include "regiondatasplit.h"
#include "regionmatrix.h"
#include "regionmatrixproxy.h"
#include "remotehost.h"

using namespace petabricks;

PetabricksRuntime::Main* petabricksMainTransform(){
  return NULL;
}
PetabricksRuntime::Main* petabricksFindTransform(const std::string& ){
  return NULL;
}

int main(int argc, const char** argv){
  const char* filename = "testdata/Helmholtz3DB1";

  IndexT m0[] = {0,0,0};
  IndexT m1[] = {1,1,1};
  IndexT m123[] = {1,2,3};
  IndexT m2[] = {2,2,2};
  IndexT m3[] = {3,3,3};
  IndexT m257[] = {2,5,7};

  RemoteHostDB hdb;
  RegionMatrixPtr regionMatrix;

  IndexT size[] = {8,9,8};
  regionMatrix = new RegionMatrix(3, size);

  if(argc==1){
    printf("main %d\n", getpid());

    hdb.remotefork(NULL, argc, argv);
    hdb.accept();
    hdb.spawnListenThread();

    // split data
    regionMatrix->splitData(m2);

    // assign a chunk of data to remote host
    RegionHandlerPtr handler = regionMatrix->getRegionHandler();
    RegionDataSplitPtr regionData = (RegionDataSplit*) handler->acquireRegionData(NULL).asPtr();
    regionData->createPart(0, hdb.host(0));
    handler->releaseRegionData(NULL);

    // import data
    // regionMatrix->allocData();
    regionMatrix->importDataFromFile(filename);

    // regionMatrix->print();

    regionMatrix->acquireRegionData();

    printf("before %4.8g\n", regionMatrix->readCell(m257));
    regionMatrix->writeCell(m257, 5);
    printf("after %4.8g\n", regionMatrix->readCell(m257));

    regionMatrix->releaseRegionData();

    // Test split
    RegionMatrixPtr split3 = regionMatrix->splitRegion(m123, m3);
    RegionMatrixPtr split2 = split3->splitRegion(m1, m2);
    RegionMatrixPtr slice1 = split2->sliceRegion(2, 0);
    RegionMatrixPtr slice2 = slice1->sliceRegion(1, 1);

    split3->print();
    split2->print();
    slice1->print();
    slice2->print();

    // Test slice
    RegionMatrixPtr slice3 = regionMatrix->sliceRegion(1, 0);
    slice3->print();


    ///////////////////////////////////
    // Create remote RegionMetrix

    regionMatrix->moveToRemoteHost(hdb.host(0), 1);

    printf("completed\n");
    hdb.listenLoop();
    return 0;
  } else {
    printf("main2 %d\n", getpid());

    JASSERT(argc==3);
    hdb.connect(argv[1], jalib::StringToInt(argv[2]));
    hdb.spawnListenThread();

    regionMatrix->updateHandler(1);

    regionMatrix->acquireRegionData();

    printf("cell %4.8g\n", regionMatrix->readCell(m257));
    printf("cell %4.8g\n", regionMatrix->readCell(m0));
    printf("cell %4.8g\n", regionMatrix->readCell(m1));
    printf("cell %4.8g\n", regionMatrix->readCell(m2));
    printf("cell %4.8g\n", regionMatrix->readCell(m3));

    regionMatrix->writeCell(m257, 123);
    printf("cell %4.8g\n", regionMatrix->readCell(m257));

    regionMatrix->releaseRegionData();

    // Test split
    RegionMatrixPtr rsplit3 = regionMatrix->splitRegion(m123, m3);
    RegionMatrixPtr rsplit2 = rsplit3->splitRegion(m1, m2);
    RegionMatrixPtr rslice1 = rsplit2->sliceRegion(2, 0);
    RegionMatrixPtr rslice2 = rslice1->sliceRegion(1, 1);

    rsplit3->print();
    rsplit2->print();
    rslice1->print();
    rslice2->print();

    // Test slice
    RegionMatrixPtr rslice3 = regionMatrix->sliceRegion(1, 0);
    rslice3->print();

    printf("completed2\n");

    hdb.listenLoop();
    return 0;
  }
}
