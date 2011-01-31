#include "regiondataraw.h"

using namespace petabricks;

RegionDataRaw::RegionDataRaw(int dimensions, IndexT* size, ElementT* data) {
  init(dimensions, size, data);
}

RegionDataRaw::RegionDataRaw(char* filename) {
  MatrixIO* matrixio = new MatrixIO(filename, "r");
  MatrixReaderScratch o = matrixio->readToMatrixReaderScratch();
  init(o.dimensions, o.sizes, o.storage->data());
}

RegionDataRaw::~RegionDataRaw() {
  delete _data;
  delete _multipliers;
}

void RegionDataRaw::init(int dimensions, IndexT* size, ElementT* data) {
  _D = dimensions;
  
  _size = new IndexT[_D];
  memcpy(_size, size, sizeof(IndexT) * _D);

  int numData = 1;
  for (int i = 0; i < _D; i++) {
    numData *= _size[i];
  }

  _data = new ElementT[numData];
  
  if (data) {
    memcpy(_data, data, sizeof(ElementT) * numData); 
  }

  _multipliers = new IndexT[_D];
  _multipliers[0] = 1;
  for (int i = 1; i < _D; i++) {
    _multipliers[i] = _multipliers[i - 1] * _size[i - 1];
  }
}

ElementT* RegionDataRaw::coordToPtr(const IndexT* coord){
  IndexT offset = 0;
  for(int i = 0; i < _D; i++){
    offset += _multipliers[i] * coord[i];
  }
  return _data + offset;
}

ElementT RegionDataRaw::readCell(const IndexT* coord) {
  return *this->coordToPtr(coord);
}

void RegionDataRaw::writeCell(const IndexT* coord, ElementT value) {
  ElementT* cell = this->coordToPtr(coord);
  *cell = value;
}
