#include "regiondatai.h"

#include <stdio.h>
#include <string.h>

#include "regionmatrixproxy.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

int RegionDataI::dimensions() {
  return _D;
}

// size() is the size of this part, not the entire region
IndexT* RegionDataI::size() {
  return _size;
}

// Process Remote Messages
void RegionDataI::processReadCellMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  ReadCellMessage* msg = (ReadCellMessage*)base->content();
  ReadCellReplyMessage reply;
  reply.value = this->readCell(msg->coord);
  size_t len = sizeof(ReadCellReplyMessage);
  caller->sendReply(&reply, len, base);
}

void RegionDataI::processReadCellCacheMsg(const BaseMessageHeader*, size_t, IRegionReplyProxy*) {
  JASSERT(false)(_type).Text("must be overrided");
}

void RegionDataI::processWriteCellMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  WriteCellMessage* msg = (WriteCellMessage*)base->content();
  WriteCellReplyMessage reply;
  this->writeCell(msg->coord, msg->value);
  reply.value = msg->value;
  size_t len = sizeof(WriteCellReplyMessage);
  caller->sendReply(&reply, len, base);
}

void RegionDataI::processWriteCellCacheMsg(const BaseMessageHeader*, size_t, IRegionReplyProxy*) {
  JASSERT(false)(_type).Text("must be overrided");
}

void RegionDataI::processGetHostListMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  GetHostListMessage* msg = (GetHostListMessage*)base->content();

  DataHostPidList list = this->hosts(msg->begin, msg->end);
  size_t hosts_array_size = list.size() * sizeof(DataHostPidListItem);
  size_t sz = sizeof(GetHostListReplyMessage) + hosts_array_size;

  char buf[sz];
  GetHostListReplyMessage* reply = (GetHostListReplyMessage*)buf;
  reply->numHosts = list.size();
  memcpy(reply->hosts, &list[0], hosts_array_size);

  caller->sendReply(buf, sz, base);
}

void RegionDataI::processCopyFromMatrixStorageMsg(const BaseMessageHeader*, size_t, IRegionReplyProxy*) {
  UNIMPLEMENTED();
}

void RegionDataI::processCopyToMatrixStorageMsg(const BaseMessageHeader*, size_t, IRegionReplyProxy*) {
  UNIMPLEMENTED();
}

void RegionDataI::processAllocDataMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  AllocDataReplyMessage reply;
  reply.result = this->allocData();
  size_t len = sizeof(AllocDataReplyMessage);
  caller->sendReply(&reply, len, base);
}

void RegionDataI::processRandomizeDataMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  this->randomize();
  RandomizeDataReplyMessage reply;
  size_t len = sizeof(RandomizeDataReplyMessage);
  caller->sendReply(&reply, len, base);
}

void RegionDataI::processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller, RegionDataIPtr regionDataPtr) {
  UpdateHandlerChainMessage* msg = (UpdateHandlerChainMessage*)base->content();

  UpdateHandlerChainReplyMessage reply;
  reply.dataHost = HostPid::self();
  reply.numHops = msg->numHops;
  reply.encodedPtr = reinterpret_cast<EncodedPtr>(regionDataPtr.asPtr());

  size_t len = sizeof(UpdateHandlerChainReplyMessage);
  caller->sendReply(&reply, len, base);
}

int RegionDataI::incCoord(int dimensions, IndexT* size, IndexT* coord) {
  if (dimensions == 0)
    return -1;
  int i;
  coord[0]++;
  for(i = 0; i < dimensions - 1; ++i){
    if(coord[i] >= size[i]){
      coord[i]=0;
      coord[i+1]++;
    }else{
      return i;
    }
  }
  if(coord[dimensions - 1] >= size[dimensions - 1]){
    return -1;
  }else{
    return dimensions - 1;
  }
}

void RegionDataI::toRegionDataCoord(int dimensions, const IndexT* coord, int numSliceDimensions, const IndexT* splitOffset, const int* sliceDimensions, const IndexT* slicePositions, IndexT* newCoord) {
  IndexT sliceIndex = 0;
  IndexT splitIndex = 0;
  for (int d = 0; d < (dimensions + numSliceDimensions); ++d) {
    if (sliceIndex < numSliceDimensions && d == sliceDimensions[sliceIndex]) {
      newCoord[d] = slicePositions[sliceIndex];
      ++sliceIndex;
    } else {
      newCoord[d] = coord[splitIndex] + splitOffset[splitIndex];
      ++splitIndex;
    }
  }
}

IndexT RegionDataI::coordToOffset(int dimensions, const IndexT* coord, const IndexT* multipliers) {
  IndexT offset = 0;
  for(int i = 0; i < dimensions; i++){
    offset += multipliers[i] * coord[i];
  }
  return offset;
}

void RegionDataI::sizeToMultipliers(int dimensions, const IndexT* size, IndexT* multipliers) {
  multipliers[0] = 1;
  for (int i = 1; i < dimensions; i++) {
    multipliers[i] = multipliers[i - 1] * size[i - 1];
  }
}

IndexT RegionDataI::toRegionDataIndex(int dimensions, const IndexT* coord, int numSliceDimensions, const IndexT* splitOffset, const int* sliceDimensions, const IndexT* slicePositions, const IndexT* multipliers) {
  IndexT newCoord[dimensions + numSliceDimensions];
  toRegionDataCoord(dimensions, coord, numSliceDimensions, splitOffset, sliceDimensions, slicePositions, newCoord);
  return coordToOffset(dimensions + numSliceDimensions, newCoord, multipliers);
}


void RegionDataI::print() {
  printf("RegionData: SIZE");
  for (int d = 0; d < _D; d++) {
    printf(" %d", _size[d]);
  }
  printf("\n");

  IndexT coord[_D];
  memset(coord, 0, (sizeof coord) * _D);

  while (true) {
    printf("%4.8g ", this->readCell(coord));

    int z = this->incCoord(_D, _size, coord);

    if (z == -1) {
      break;
    }

    while (z > 0) {
      printf("\n");
      z--;
    }
  }

  printf("\n\n");
}
