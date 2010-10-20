#include "regioni.h"
#include <string.h>

///
/// increment a raw coord in ascending order
/// return largest dimension incremented or -1 for end
int petabricks::RegionI::incCoord(IndexT* coord) {
  if (_dimension == 0) { 
    return -1;
  }

  int i;
  coord[0]++;
  for (i = 0; i < _dimension - 1; ++i){
    if (coord[i] >= _size[i]){
      coord[i]=0;
      coord[i+1]++;
    } else{
      return i;
    }
  }
  if (coord[_dimension-1] >= _size[_dimension-1]){
    return -1;
  }else{
    return _dimension-1;
  }
}

int petabricks::RegionI::dimension() const {
  return _dimension;
}

petabricks::IndexT
petabricks::RegionI::sizeOfDimension(int d) const {
  return _size[d];
}


void petabricks::RegionI::print() {
  printf("SIZE");
  for (int d = 0; d < _dimension; d++) {
    printf(" %d", _size[d]);
  }
  printf("\n");

  IndexT* coord = new IndexT[_dimension];
  memset(coord, 0, (sizeof coord) * _dimension);

  while (true) {
    printf("%4.8g ", *this->coordToPtr(coord));

    int z = this->incCoord(coord);

    if (z == -1) {
      break;
    }

    while (z > 0) {
      printf("\n");
      z--;
    }
  }

  printf("\n\n");
  delete(coord);
}
