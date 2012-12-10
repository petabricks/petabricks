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
    regionMatrix.createDataPart(3, RemoteHostDB::instance().host(0));

    regionMatrix.copyDataFromRegion(regionMatrixLocal);

    MatrixIO().write(regionMatrixLocal);
    MatrixIO().write(regionMatrix);
    regionMatrix.printDataHosts();

    regionMatrix.assertEqual(regionMatrixLocal);

    IndexT c1[] = {4, 4};
    IndexT c2[] = {12, 12};

    MatrixRegion2D split = regionMatrix.region(c1, c2);
    MatrixIO().write(split);
    split.printDataHosts();

    JTRACE("scratch");
    MatrixRegion2D scratch = split.localCopy();
    MatrixIO().write(scratch);
    split.assertEqual(scratch);

    JTRACE("slice");
    MatrixRegion1D slice = split.slice(0, 5);
    MatrixIO().write(slice);
    slice.printDataHosts();
    MatrixRegion1D scratchSlice = slice.localCopy();
    MatrixIO().write(scratchSlice);
    slice.assertEqual(scratchSlice);

    JTRACE("modify");
    scratchSlice.cell(5) = 9779;
    MatrixIO().write(scratchSlice);

    slice.fromScratchRegion(scratchSlice);
    MatrixIO().write(regionMatrix);

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
    void onRecv(const void* data, size_t /*len*/, int) {
      JTRACE("== step2 ==");
      MatrixRegion2D regionMatrix = MatrixRegion2D();
      regionMatrix.unserialize((char*)data, *host());
      MatrixIO().write(regionMatrix);
      regionMatrix.printDataHosts();

      MatrixRegion2D scratch = regionMatrix.localCopy();
      MatrixIO().write(scratch);
      regionMatrix.assertEqual(scratch);

      IndexT m11[] = {1,1};
      JTRACE("modify");
      scratch.cell(m11) = 1331;
      MatrixIO().write(scratch);

      regionMatrix.fromScratchRegion(scratch);
      MatrixIO().write(regionMatrix);
      regionMatrix.assertEqual(scratch);

      JTRACE("== step2 done ==");
      exit(0);
    }
  };
  return new TestRemoteObject();
}
