#include "regionmatrix.h"

#include <stdarg.h>
#include <string.h>
#include "regiondata0D.h"
#include "regiondataraw.h"
#include "regiondataremote.h"
#include "regiondatasplit.h"
#include "regionmatrixproxy.h"

using namespace petabricks;

std::map<uint16_t, RegionDataIPtr> RegionMatrix::movingBuffer;
pthread_mutex_t RegionMatrix::movingBuffer_mux = PTHREAD_MUTEX_INITIALIZER;

RegionMatrix::RegionMatrix(int dimensions) {
  // (yod) fix this --> MatrixRegion()
  _D = dimensions;

  _size = 0;
  _splitOffset = 0;
  _numSliceDimensions = 0;
  _sliceDimensions = 0;
  _slicePositions = 0;
  _isTransposed = false;
}

RegionMatrix::RegionMatrix(int dimensions, ElementT value) {
  JASSERT(dimensions == 0)("This constructor is for 0D only");

  _D = dimensions;

  _regionHandler = new RegionHandler(new RegionData0D(value));

  _size = 0;
  _splitOffset = 0;
  _numSliceDimensions = 0;
  _sliceDimensions = 0;
  _slicePositions = 0;
  _isTransposed = false;
}

RegionMatrix::RegionMatrix(int dimensions, IndexT* size) {
  RegionDataIPtr regionData = new RegionDataRaw(dimensions, size);
  init(dimensions, size, new RegionHandler(regionData));
}

RegionMatrix::RegionMatrix(int dimensions, IndexT* size, RegionHandlerPtr handler) {
  init(dimensions, size, handler);
}

void RegionMatrix::init(int dimensions, IndexT* size, RegionHandlerPtr handler) {
  _regionHandler = handler;

  _D = dimensions;
  _size = new IndexT[_D];
  memcpy(_size, size, sizeof(IndexT) * _D);

  _splitOffset = new IndexT[_D];
  memset(_splitOffset, 0, sizeof(IndexT) * _D);

  _numSliceDimensions = 0;
  _sliceDimensions = 0;
  _slicePositions = 0;

  _isTransposed = false;
}

RegionMatrix::RegionMatrix(const RegionMatrix& that) {
  copy(that);
}

void RegionMatrix::operator=(const RegionMatrix& that) {
  copy(that);
}

void RegionMatrix::copy(const RegionMatrix& that) {
  _D = that.dimensions();
  _size = new IndexT[_D];
  memcpy(_size, that._size, sizeof(IndexT) * _D);

  _splitOffset = new IndexT[_D];
  memcpy(_splitOffset, that._splitOffset, sizeof(IndexT) * _D);


  _numSliceDimensions = that._numSliceDimensions;

  if (_numSliceDimensions > 0) {
    _sliceDimensions = new IndexT[_numSliceDimensions];
    memcpy(_sliceDimensions, that._sliceDimensions, sizeof(int) * _numSliceDimensions);

    _slicePositions = new IndexT[_numSliceDimensions];
    memcpy(_slicePositions, that._slicePositions, sizeof(int) * _numSliceDimensions);
  } else {
    _sliceDimensions = 0;
    _slicePositions = 0;
  }

  _isTransposed = that._isTransposed;

  _regionHandler = that.getRegionHandler();
}

/*
// We might want to create a regionMatrix from movingBuffer
RegionMatrix::RegionMatrix(RegionDataIPtr regionData) {
  _regionHandler = new RegionHandler(regionData);

  _D = _regionHandler->dimensions();
  _size = new IndexT[_D];
  memcpy(_size, regionData->size(), sizeof(IndexT) * _D);

  _splitOffset = new IndexT[_D];
  memset(_splitOffset, 0, sizeof(IndexT) * _D);

  _numSliceDimensions = 0;
  _sliceDimensions = 0;
  _slicePositions = 0;
}
*/

//
// Called by split & slice
//
RegionMatrix::RegionMatrix(RegionHandlerPtr handler, int dimensions, IndexT* size,
			   IndexT* splitOffset, int numSliceDimensions,
			   int* sliceDimensions, IndexT* slicePositions,
			   bool isTransposed) {
  _regionHandler = handler;
  _D = dimensions;
  _size = size;
  _splitOffset = splitOffset;
  _numSliceDimensions = numSliceDimensions;
  if (_numSliceDimensions > 0) {
    _sliceDimensions = sliceDimensions;
    _slicePositions = slicePositions;
  }
  _isTransposed = isTransposed;
}

RegionMatrix::~RegionMatrix() {
  delete [] _size;
  delete [] _splitOffset;
  if (_numSliceDimensions > 0) {
    delete [] _sliceDimensions;
    delete [] _slicePositions;
  }
}

void RegionMatrix::splitData(IndexT* splitSize) {
  JASSERT(_regionHandler->type() == RegionDataTypes::REGIONDATARAW);
  RegionDataIPtr newRegionData =
    new RegionDataSplit((RegionDataRaw*)_regionHandler->getRegionData().asPtr(), splitSize);
  _regionHandler->updateRegionData(newRegionData);
}

void RegionMatrix::createDataPart(int partIndex, RemoteHostPtr host) {
  JASSERT(_regionHandler->type() == RegionDataTypes::REGIONDATASPLIT);
  ((RegionDataSplit*)_regionHandler->getRegionData().asPtr())->createPart(partIndex, host);
}

void RegionMatrix::allocData() {
  _regionHandler->allocData();
}

void RegionMatrix::importDataFromFile(const char* filename) {
  // (yod) perf: move the import to regionDataRaw

  this->allocData();

  MatrixIO* matrixio = new MatrixIO(filename, "r");
  MatrixReaderScratch o = matrixio->readToMatrixReaderScratch();
  ElementT* data = o.storage->data();

  IndexT* coord = new IndexT[_D];
  memset(coord, 0, sizeof(IndexT) * _D);

  int i = 0;

  while (true) {
    this->writeCell(coord, data[i]);
    i++;

    int z = this->incCoord(coord);
    if (z == -1) {
      break;
    }
  }

  delete [] coord;
  delete matrixio;
}

ElementT RegionMatrix::readCell(const IndexT* coord) {
  IndexT* rd_coord = this->getRegionDataCoord(coord);
  ElementT elmt = _regionHandler->readCell(rd_coord);
  delete [] rd_coord;
  return elmt;
}

void RegionMatrix::writeCell(const IndexT* coord, ElementT value) {
  IndexT* rd_coord = this->getRegionDataCoord(coord);
  _regionHandler->writeCell(rd_coord, value);
  delete [] rd_coord;
}

IndexT* RegionMatrix::size() const {
  return _size;
}

IndexT RegionMatrix::size(int i) const {
  return _size[i];
}

///
/// true if coord is in bounds
bool RegionMatrix::contains(const IndexT* coord) const {
  for(int i=0; i<_D; ++i)
    if(coord[i]<0 || coord[i]>=size(i))
      return false;
  return true;
}

bool RegionMatrix::contains(IndexT x, ...) const {
  IndexT c1[_D];
  va_list ap;
  va_start(ap, x);
  c1[0]=x;
  for(int i=1; i<_D; ++i) c1[i]=va_arg(ap, IndexT);
  va_end(ap);
  return contains(c1);
}

RegionMatrixPtr RegionMatrix::splitRegion(const IndexT* offset, const IndexT* size) const {
  IndexT* offset_new = this->getRegionDataCoord(offset);

  IndexT* size_copy = new IndexT[_D];
  memcpy(size_copy, size, sizeof(IndexT) * _D);

  int* sliceDimensions = new int[_numSliceDimensions];
  memcpy(sliceDimensions, _sliceDimensions,
	 sizeof(int) * _numSliceDimensions);
  IndexT* slicePositions = new IndexT[_numSliceDimensions];
  memcpy(slicePositions, _slicePositions,
	 sizeof(IndexT) * _numSliceDimensions);

  return new RegionMatrix(_regionHandler, _D, size_copy, offset_new,
			  _numSliceDimensions, sliceDimensions, slicePositions,
			  _isTransposed);
}

RegionMatrixPtr RegionMatrix::sliceRegion(int d, IndexT pos) const {
  if (_isTransposed) {
    d = _D - d - 1;
  }

  int dimensions = _D - 1;
  IndexT* size = new IndexT[dimensions];
  memcpy(size, _size, sizeof(IndexT) * d);
  memcpy(size + d, _size + d + 1, sizeof(IndexT) * (dimensions - d));

  IndexT* offset = new IndexT[dimensions];
  memcpy(offset, _splitOffset, sizeof(IndexT) * d);
  memcpy(offset + d, _splitOffset + d + 1, sizeof(IndexT) * (dimensions - d));

  // maintain ordered array of _sliceDimensions + update d as necessary
  int numSliceDimensions = _numSliceDimensions + 1;
  int* sliceDimensions = new int[numSliceDimensions];
  IndexT* slicePositions = new IndexT[numSliceDimensions];

  if (_numSliceDimensions == 0) {
    sliceDimensions[0] = d;
    slicePositions[0] = pos + _splitOffset[d];
  } else {
    bool isAddedNewD = false;
    for (int i = 0; i < numSliceDimensions; i++) {
      if (isAddedNewD) {
	sliceDimensions[i] = _sliceDimensions[i-1];
	slicePositions[i] = _slicePositions[i-1];
      } else if (d >= _sliceDimensions[i]) {
	sliceDimensions[i] = _sliceDimensions[i];
	slicePositions[i] = _slicePositions[i];
	d++;
      } else {
	sliceDimensions[i] = d;
	slicePositions[i] = pos + _splitOffset[d];
	isAddedNewD = true;
      }
    }
  }

  return new RegionMatrix(_regionHandler, dimensions, size, offset,
			  numSliceDimensions, sliceDimensions, slicePositions,
			  _isTransposed);
}

void RegionMatrix::transpose() {
  _isTransposed = !_isTransposed;
}

RegionMatrixPtr RegionMatrix::transposedRegion() const {
  RegionMatrixPtr transposed = new RegionMatrix(*this);
  transposed->transpose();
  return transposed;
}

void RegionMatrix::moveToRemoteHost(RemoteHostPtr host, uint16_t movingBufferIndex) {
  RegionMatrixProxyPtr proxy =
    new RegionMatrixProxy(this->getRegionHandler());
  RemoteObjectPtr local = proxy->genLocal();

  // InitialMsg
  RegionDataRemoteMessage::InitialMessage* msg = new RegionDataRemoteMessage::InitialMessage();
  msg->dimensions = _D;
  msg->movingBufferIndex = movingBufferIndex;
  memcpy(msg->size, _size, sizeof(msg->size));
  int len = (sizeof msg) + sizeof(msg->size);

  host->createRemoteObject(local, &RegionDataRemote::genRemote, msg, len);
  local->waitUntilCreated();
  JTRACE("done");
}

void RegionMatrix::updateHandler(uint16_t movingBufferIndex) {
  while (!RegionMatrix::movingBuffer[movingBufferIndex]) {
    jalib::memFence();
    sched_yield();
  }

  // Create a new regionHandler. We cannot update the old one because it
  // might be used by another regionmatrixproxy. e.g. 1 -> 2 -> 1
  _regionHandler->updateRegionData(RegionMatrix::movingBuffer[movingBufferIndex]);
  RegionMatrix::movingBuffer[movingBufferIndex] = NULL;
}

void RegionMatrix::addMovingBuffer(RegionDataIPtr remoteData, uint16_t index) {
  pthread_mutex_lock(&RegionMatrix::movingBuffer_mux);
  RegionMatrix::movingBuffer[index] = remoteData;
  pthread_mutex_unlock(&RegionMatrix::movingBuffer_mux);
}

void RegionMatrix::removeMovingBuffer(uint16_t index) {
  pthread_mutex_lock(&RegionMatrix::movingBuffer_mux);
  RegionMatrix::movingBuffer.erase(index);
  pthread_mutex_unlock(&RegionMatrix::movingBuffer_mux);
}

void RegionMatrix::updateHandlerChain() {
  RegionDataIPtr regionData = _regionHandler->getRegionData();
  if (regionData->type() == RegionDataTypes::REGIONDATAREMOTE) {
    RegionDataRemoteMessage::UpdateHandlerChainReplyMessage* reply =
      ((RegionDataRemote*)regionData.asPtr())->updateHandlerChain();
    JTRACE("updatehandler")(reply->dataHost)(reply->numHops)(reply->regionData.asPtr());

    if (reply->dataHost == HostPid::self()) {
      // Data is in the same process. Update handler to point directly to the data.
      _regionHandler->updateRegionData(reply->regionData);
    } else if (reply->numHops > 1) {
      // Multiple network hops to data. Create a direct connection to data.

      // (yod) TODO:
      //this->updateHandler(999);
    }
  }
}

//
// Convert a coord to the one in _regionData
//
IndexT* RegionMatrix::getRegionDataCoord(const IndexT* coord_orig) const {
  IndexT slice_index = 0;
  IndexT split_index = 0;

  IndexT* coord_new = new IndexT[_regionHandler->dimensions()];

  for (int d = 0; d < _regionHandler->dimensions(); d++) {
    if (slice_index < _numSliceDimensions &&
	d == _sliceDimensions[slice_index]) {
      // slice
      if (_isTransposed) {
	coord_new[_D-d] = _slicePositions[slice_index];
      } else {
	coord_new[d] = _slicePositions[slice_index];
      }
      slice_index++;
    } else {
      // split
      int offset = 0;
      if (_splitOffset) {
	offset = _splitOffset[split_index];
      }

      if (_isTransposed) {
	coord_new[_D-d] = coord_orig[split_index] + offset;
      } else {
	coord_new[d] = coord_orig[split_index] + offset;
      }
      split_index++;
    }
  }

  return coord_new;
}

CellProxy& RegionMatrix::cell(IndexT x, ...) const {
  IndexT c1[_D];
  va_list ap;
  va_start(ap, x);
  c1[0]=x;
  for(int i=1; i<_D; ++i) c1[i]=va_arg(ap, IndexT);
  va_end(ap);
  return cell(c1);
}


DataHostList RegionMatrix::dataHosts() const {
  IndexT begin[_D];
  IndexT end[_D];

  memset(begin, 0, sizeof(IndexT) * _D);
  for (int i = 0; i < _D; i++) {
    end[i] = this->size(i) - 1;
  }

  return _regionHandler->hosts(this->getRegionDataCoord(begin), this->getRegionDataCoord(end));
}

int RegionMatrix::incCoord(IndexT* coord) const {
  if (_D == 0) {
    return -1;
  }

  coord[0]++;
  for (int i = 0; i < _D - 1; ++i){
    if (coord[i] >= _size[i]){
      coord[i]=0;
      coord[i+1]++;
    } else{
      return i;
    }
  }
  if (coord[_D - 1] >= _size[_D - 1]){
    return -1;
  }else{
    return _D - 1;
  }
}

void RegionMatrix::print() {
  printf("(%d) RegionMatrix: SIZE", getpid());
  for (int d = 0; d < _D; d++) {
    printf(" %d", _size[d]);
  }
  printf("\n");

  IndexT* coord = new IndexT[_D];
  memset(coord, 0, (sizeof coord) * _D);

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
  delete [] coord;
}
