#ifndef PETABRICKSREGIONMATRIX_H
#define PETABRICKSREGIONMATRIX_H

#include <map>
#include <pthread.h>
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
    int _D;
    IndexT* _size;
    IndexT* _splitOffset;
    int _numSliceDimensions;
    int* _sliceDimensions;
    IndexT* _slicePositions;

  public:
    RegionMatrix(int dimensions);
    RegionMatrix(int dimensions, IndexT* size);

    //   RegionMatrix(RegionDataIPtr regionData);
    RegionMatrix(RegionHandlerPtr handler, int dimensions, IndexT* size,
		 IndexT* splitOffset, int numSliceDimensions,
		 int* sliceDimensions, IndexT* slicePositions);
   
    RegionMatrix(const RegionMatrix& that); 
    void operator=(const RegionMatrix& that);

    ~RegionMatrix();

    void init(int dimensions, IndexT* size);

    void splitData(IndexT* splitSize);
    void allocData();
    void importDataFromFile(char* filename);

    RegionMatrixPtr splitRegion(IndexT* offset, IndexT* size);
    RegionMatrixPtr sliceRegion(int d, IndexT pos);

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    int dimensions() const;
    IndexT* size() const;
    IndexT size(int i) const;

    void moveToRemoteHost(RemoteHostPtr host, uint16_t movingBufferIndex);
    void updateHandler(uint16_t movingBufferIndex);
    static void addMovingBuffer(RegionDataIPtr remoteData, uint16_t index);
    void removeMovingBuffer(uint16_t index);

    // for tests
    int incCoord(IndexT* coord);
    void print();

  private:
    IndexT* getRegionDataCoord(const IndexT* coord_orig);

  };
}

#endif
