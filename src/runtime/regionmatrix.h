#ifndef PETABRICKSREGIONMATRIX_H
#define PETABRICKSREGIONMATRIX_H

#include <map>
#include <pthread.h>
#include "common/jassert.h"
#include "matrixregion.h"
#include "regiondatai.h"
#include "regionmatrixi.h"
#include "remotehost.h"

namespace petabricks {
  class RegionMatrix;
  typedef jalib::JRef<RegionMatrix> RegionMatrixPtr;

  class RegionMatrix : public RegionMatrixI {
  private:
    static std::map<uint16_t, RegionDataIPtr> movingBuffer;
    static pthread_mutex_t movingBuffer_mux;

  protected:
    IndexT* _size;
    IndexT* _splitOffset;
    int _numSliceDimensions;
    int* _sliceDimensions;
    IndexT* _slicePositions;
    bool _isTransposed;

  public:
    RegionMatrix(int dimensions);
    RegionMatrix(int dimensions, ElementT value);
    RegionMatrix(int dimensions, IndexT* size);

    RegionMatrix(RegionHandlerPtr handler, int dimensions, IndexT* size,
		 IndexT* splitOffset, int numSliceDimensions,
		 int* sliceDimensions, IndexT* slicePositions,
		 bool isTransposed);

    RegionMatrix(const RegionMatrix& that);
    void operator=(const RegionMatrix& that);

    ~RegionMatrix();

    void init(int dimensions, IndexT* size);
    void copy(const RegionMatrix& that);

    void splitData(IndexT* splitSize);
    void createDataPart(int partIndex, RemoteHostPtr host);
    void allocData();
    void importDataFromFile(const char* filename);

    RegionMatrixPtr splitRegion(const IndexT* offset, const IndexT* size) const;
    RegionMatrixPtr sliceRegion(int d, IndexT pos) const;
    void transpose();
    RegionMatrixPtr transposedRegion() const;


    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    IndexT* size() const;
    IndexT size(int i) const;
    bool contains(const IndexT* coord) const;
    bool contains(IndexT x, ...) const;

    void moveToRemoteHost(RemoteHostPtr host, uint16_t movingBufferIndex);
    void updateHandler(uint16_t movingBufferIndex);
    static void addMovingBuffer(RegionDataIPtr remoteData, uint16_t index);
    void removeMovingBuffer(uint16_t index);

    void updateHandlerChain();

    CellProxy& cell(IndexT x, ...) const;
    CellProxy& cell(IndexT* coord) const {
      return *(new CellProxy(_regionHandler, getRegionDataCoord(coord)));
    }
    INLINE CellProxy& cell() const {
      IndexT c1[0];
      return this->cell(c1);
    }

    DataHostList dataHosts() const;

    int incCoord(IndexT* coord) const;
    void print();

  private:
    IndexT* getRegionDataCoord(const IndexT* coord_orig) const;

  };
}

#endif
