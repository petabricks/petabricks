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
  for (unsigned int i = 0; i < list.size(); i++) {
    printf("%lx/%d ==> %.5g\n", list[i].hostPid.hostid, list[i].hostPid.pid, list[i].weight);
  }
}

void runProcess2(MatrixRegion3D& regionMatrix);

RemoteObjectPtr gen() {
  class TestRemoteObject : public petabricks::RemoteObject {
  public:
    void onRecv(const void* data, size_t /*len*/) {
      printf("== start process 2 ==\n");

      MatrixRegion3D regionMatrix = MatrixRegion3D();
      regionMatrix.unserialize((char*)data, *host());
      regionMatrix.createRegionHandler(*host());
      MatrixIO().write(regionMatrix);

      runProcess2(regionMatrix);
    }
  };
  return new TestRemoteObject();
}

int main(int argc, const char** argv){
  const char* filename = "testdata/Helmholtz3DB1";

  IndexT m1[] = {1,1,1};
  IndexT m123[] = {1,2,3};
  IndexT m456[] = {4,5,6};
  IndexT m2[] = {2,2,2};
  IndexT m3[] = {3,3,3};
  IndexT m234[] = {2,3,4};

  IndexT size[] = {8,9,8};
  //
  // Create a RegionMatrix
  MatrixRegion3D regionMatrix(size);

  if(argc==1){
    printf("main %d\n", getpid());

    RemoteHostDB::instance().remotefork(NULL, argc, argv);
    RemoteHostDB::instance().accept("");
    RemoteHostDB::instance().spawnListenThread();

    // Split the matrix in to multiple parts of size m2
    regionMatrix.splitData(m2);

    // Assign a chunk of data to remote host
    //   - put part 0 in hdb.host(0)
    //   - the other parts are created locally
    regionMatrix.createDataPart(0, RemoteHostDB::instance().host(0));

    // import data
    MatrixRegion3D in = MatrixIO(filename,"r").read_distributed<3>();
    regionMatrix.copyDataFromRegion(in);

    // or, allocate empty matrix
    // regionMatrix->allocData();

    printf("== data should be in 2 hosts ==\n");
    print(regionMatrix.dataHosts());

    CellProxy cell = regionMatrix.cell(m234);
    JASSERT(fabs(cell - 0.52076837) < 0.00000001);
    cell = 5;
    JASSERT(fabs(cell - 5) < 0.00000001);

    // Test split
    MatrixRegion3D split3 = regionMatrix.region(m123, m456);
    printf("== region((1,2,3), (4,5,6)) : will send this to process2 ==\n");
    MatrixIO().write(split3);
    print(split3.dataHosts());

    MatrixRegion3D split2 = split3.region(m1, m3);
    printf("== region((1,1,1), (3,3,3)) ==\n");
    MatrixIO().write(split2);
    print(split2.dataHosts());

    MatrixRegion2D slice1 = split2.slice(2, 0);
    printf("== slice(2,0) ==\n");
    MatrixIO().write(slice1);
    print(slice1.dataHosts());

    MatrixRegion1D slice2 = slice1.slice(1, 1);
    printf("== slice(1,1) ==\n");
    MatrixIO().write(slice2);
    print(slice2.dataHosts());

    // Test slice
    MatrixRegion2D slice3 = regionMatrix.slice(1, 0);
    printf("== slice(1,0) of original matrix ==\n");
    MatrixIO().write(slice3);
    print(slice3.dataHosts());

    ///////////////////////////////////
    // Migrate to process 2

    char* buf = new char[split3.serialSize()];
    split3.serialize(buf, *RemoteHostDB::instance().host(0));

    RemoteObjectPtr local;
    RemoteHostDB::instance().host(0)->createRemoteObject(local=gen(), &gen);
    local->waitUntilCreated();
    local->send(buf, split3.serialSize());

    printf("== sent matrix to process2 ==\n");

    RemoteHostDB::instance().listenLoop();
    return 0;
  } else {
    printf("main2 %d\n", getpid());
    JASSERT(argc==3);
    RemoteHostDB::instance().connect(argv[1], jalib::StringToInt(argv[2]));
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().listenLoop();
    return 0;
  }
}

void runProcess2(MatrixRegion3D& regionMatrix) {
  print(regionMatrix.dataHosts());
  regionMatrix.updateHandlerChain();

  IndexT m0[] = {0,0,0};
  IndexT m1[] = {1,1,1};
  IndexT m2[] = {2,2,2};

  JASSERT(fabs(regionMatrix.cell(m0) - 0.57373451) < 0.00000001);
  JASSERT(fabs(regionMatrix.cell(m1) - 5) < 0.00000001);
  JASSERT(fabs(regionMatrix.cell(m2) - 0.50022465) < 0.00000001);
  regionMatrix.cell(m1) = 123;
  JASSERT(fabs(regionMatrix.cell(m1) - 123) < 0.00000001);

  // Test split
  MatrixRegion3D rsplit = regionMatrix.region(m0, m2);
  printf("== region((0,0,0), (2,2,2)) ==\n");
  MatrixIO().write(rsplit);

  MatrixRegion2D rslice = rsplit.slice(2, 0);
  printf("== slice(2,0) ==\n");
  MatrixIO().write(rslice);

  // Test slice
  MatrixRegion2D rslice3 = regionMatrix.slice(1, 1);
  printf("== slice(1,1) of original matrix ==\n");
  MatrixIO().write(rslice3);
  print(rslice3.dataHosts());

  // localCopy: copy the entire matrix and store it locally
  MatrixRegion2D copy = rslice3.localCopy();
  printf("== local copy of above ==\n");
  MatrixIO().write(copy);
  print(copy.dataHosts());

  // Convert to MatrixRegion
  MatrixIO().write(copy._toLocalRegion());

  printf("== done ==\n");
}
