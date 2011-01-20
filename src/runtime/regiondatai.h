#ifndef PETABRICKSREGIONDATAI_H
#define PETABRICKSREGIONDATAI_H

#include "common/jrefcounted.h"

namespace petabricks {
  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;

  template <int D> class RegionDataI : public jalib::JRefCounted {
  
  public:
    typedef jalib::JRef<RegionDataI> RegionDataIPtr;

  protected:
    IndexT* _size;

  public:
    ~RegionDataI();

    virtual ElementT readCell(const IndexT* coord);
    virtual void writeCell(const IndexT* coord, ElementT value);

    // for tests
    int incCoord(IndexT* coord);
    void print();
  };
}

/* implementation */
using namespace petabricks;

template <int D>
RegionDataI<D>::~RegionDataI() {
  delete _size;
}

template <int D>
ElementT RegionDataI<D>::readCell(const IndexT* coord) {
  printf("Subclass must implement this method.\n");
  throw std::exception();
}

template <int D>
void RegionDataI<D>::writeCell(const IndexT* coord, ElementT value) {
  printf("Subclass must implement this method.\n");
  throw std::exception();
}

template <int D>
int RegionDataI<D>::incCoord(IndexT* coord) {
  if (D == 0) { 
    return -1;
  }

  coord[0]++;
  for (int i = 0; i < D - 1; ++i){
    if (coord[i] >= _size[i]){
      coord[i]=0;
      coord[i+1]++;
    } else{
      return i;
    }
  }
  if (coord[D - 1] >= _size[D - 1]){
    return -1;
  }else{
    return D - 1;
  }
}

template <int D>
void RegionDataI<D>::print() {
  printf("RegionData: SIZE");
  for (int d = 0; d < D; d++) {
    printf(" %d", _size[d]);
  }
  printf("\n");

  IndexT* coord = new IndexT[D];
  memset(coord, 0, (sizeof coord) * D);

  while (true) {
    printf("%4.8g ", this->readCell(coord));

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

#endif
