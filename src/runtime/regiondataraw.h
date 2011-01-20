#ifndef PETABRICKSREGIONDATARAW_H
#define PETABRICKSREGIONDATARAW_H

#include "regiondatai.h"

namespace petabricks {
  template <int D> class RegionDataRaw : public RegionDataI<D> {
  public:
    typedef jalib::JRef<RegionDataRaw> RegionDataRawPtr;

  private:
    ElementT* _data;
    IndexT* _multipliers;

  public:
    RegionDataRaw(char* filename);
    RegionDataRaw(IndexT* size, ElementT* data);
    ~RegionDataRaw();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

  private:
    void init(IndexT* size, ElementT* data);
    ElementT* coordToPtr(const IndexT* coord);
  };
}

/* implementation */
using namespace petabricks;

template <int D>
RegionDataRaw<D>::RegionDataRaw(IndexT* size, ElementT* data) {
  init(size, data);
}

template <int D>
RegionDataRaw<D>::RegionDataRaw(char* filename) {
  MatrixIO* matrixio = new MatrixIO(filename, "r");
  MatrixReaderScratch o = matrixio->readToMatrixReaderScratch();
  init(o.sizes, o.storage->data());
}

template <int D>
RegionDataRaw<D>::~RegionDataRaw() {
  delete _data;
  delete _multipliers;
}

template <int D>
void RegionDataRaw<D>::init(IndexT* size, ElementT* data) {
  this->_size = new IndexT[D];
  memcpy(this->_size, size, sizeof(IndexT) * D);

  int numData = 1;
  for (int i = 0; i < D; i++) {
    numData *= this->_size[i];
  }

  _data = new ElementT[numData];
  
  if (data) {
    memcpy(_data, data, sizeof(ElementT) * numData); 
  }

  _multipliers = new IndexT[D];
  _multipliers[0] = 1;
  for (int i = 1; i < D; i++) {
    _multipliers[i] = _multipliers[i - 1] * this->_size[i - 1];
  }
}

template <int D>
ElementT* RegionDataRaw<D>::coordToPtr(const IndexT* coord){
  IndexT offset = 0;
  for(int i = 0; i < D; i++){
    offset += _multipliers[i] * coord[i];
  }
  return _data + offset;
}

template <int D>
ElementT RegionDataRaw<D>::readCell(const IndexT* coord) {
  return *this->coordToPtr(coord);
}

template <int D>
void RegionDataRaw<D>::writeCell(const IndexT* coord, ElementT value) {
  ElementT* cell = this->coordToPtr(coord);
  *cell = value;
}

#endif
