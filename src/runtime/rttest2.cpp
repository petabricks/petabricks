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

int main(int argc, const char** argv){
  using namespace petabricks::distributed;

  if(argc==1){
    JTRACE("process1");

    RemoteHostDB::instance().remotefork(NULL, argc, argv);
    RemoteHostDB::instance().accept("");
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();
    RemoteHostDB::instance().spawnListenThread();

    const char* filename = "testdata/Rand2Da";
    MatrixRegion2D regionMatrixLocal = MatrixIO(filename,"r").read_distributed<2>();

    IndexT sizes[] = {16, 16};
    IndexT partSizes[] = {8, 8};
    MatrixRegion2D regionMatrix(sizes);
    regionMatrix.splitData(partSizes);
    regionMatrix.createDataPart(0, RemoteHostDB::instance().host(0));

    regionMatrix.copyDataFromRegion(regionMatrixLocal);

    MatrixIO().write(regionMatrixLocal);
    MatrixIO().write(regionMatrix);
    print(regionMatrix.dataHosts());

    regionMatrix.assertEqual(regionMatrixLocal);

    IndexT c1[] = {4, 4};
    IndexT c2[] = {12, 12};

    MatrixRegion2D split = regionMatrix.region(c1, c2);
    MatrixIO().write(split);
    print(split.dataHosts());

    char* buf = new char[split.serialSize()];
    split.serialize(buf, *RemoteHostDB::instance().host(0));

    // send buf to process 2
    RemoteObjectPtr local;
    RemoteHostDB::instance().host(0)->createRemoteObject(local=step2(), &step2);
    local->waitUntilCreated();
    local->send(buf, split.serialSize());

    JTRACE("== step 1 done ==");

    RemoteHostDB::instance().listenLoop();
    return 0;
  } else {
    JTRACE("process2");

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
      JTRACE("== step2 ==");
      MatrixRegion2D regionMatrix = MatrixRegion2D();
      regionMatrix.unserialize((char*)data, *host());
      regionMatrix.createRegionHandler(*host());
      MatrixIO().write(regionMatrix);
      print(regionMatrix.dataHosts());

      // IndexT sizes[] = {8, 8};
      // MatrixRegion2D scratch(sizes);
      // scratch.allocData();
      // regionMatrix.localCopy(scratch);
      // MatrixIO().write(scratch);


      IndexT m11[] = {1,1};
      regionMatrix.cell(m11) = 1331;
      MatrixIO().write(regionMatrix);


      JTRACE("== step2 done ==");
      exit(0);
    }
  };
  return new TestRemoteObject();
}
