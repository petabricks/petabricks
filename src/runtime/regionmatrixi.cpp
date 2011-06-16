#include "regionmatrixi.h"

#include <stdarg.h>

using namespace petabricks;

RegionHandlerPtr RegionMatrixI::getRegionHandler() const {
  return _regionHandler;
}

CellProxy& RegionMatrixI::cell(IndexT x, ...) const {
  IndexT c1[_D];
  va_list ap;
  va_start(ap, x);
  c1[0]=x;
  for(int i=1; i<_D; ++i) c1[i]=va_arg(ap, IndexT);
  va_end(ap);
  return cell(c1);
}

CellProxy& RegionMatrixI::cell(IndexT* coord) const {
  return *(new CellProxy(_regionHandler, coord));
}

