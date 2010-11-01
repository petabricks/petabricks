#include "regionremoteproxy.h"

#include "matrixio.h"

using namespace petabricks;

petabricks::RegionRemoteProxy::RegionRemoteProxy() {
  MatrixIO* matrixio = new MatrixIO("testdata/Helmholtz3DB1", "r");
  _referenceRegion = matrixio->readToRegionI();
}

void RegionRemoteProxy::onNotify(int argc) {
  JTRACE("notify")(argc);
  if(argc==1) {
    markComplete();
  }
}

void RegionRemoteProxy::onRecv(const void* data, size_t len) {
  //printf("xxx %d", (int*)data);


  JTRACE("recv")(*(int*)data)(len);
}
