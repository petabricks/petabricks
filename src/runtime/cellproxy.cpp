#include "cellproxy.h"

std::string petabricks::CellProxy::toString() const {
  std::ostringstream os;
  print(os);
  return os.str();
}

std::ostream& operator<< (std::ostream& o, const petabricks::CellProxy& cell){
  cell.print(o);
  return o;
}
