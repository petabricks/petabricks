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
void RegionDataI::processReadCellMsg(const BaseMessageHeader* base, size_t, ReadCellReplyMessage& reply, size_t& len, EncodedPtr) {
  ReadCellMessage* msg = (ReadCellMessage*)base->content();
  reply.value = this->readCell(msg->coord);
  len = sizeof(ReadCellReplyMessage);
}

void RegionDataI::processWriteCellMsg(const BaseMessageHeader* base, size_t, WriteCellReplyMessage& reply, size_t& len, EncodedPtr) {
  WriteCellMessage* msg = (WriteCellMessage*)base->content();
  this->writeCell(msg->coord, msg->value);
  reply.value = msg->value;
  len = sizeof(WriteCellReplyMessage);
}

void RegionDataI::processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen, GetHostListReplyMessage&, size_t&, EncodedPtr encodedPtr) {
  GetHostListMessage* msg = (GetHostListMessage*)base->content();

  DataHostList list = this->hosts(msg->begin, msg->end);
  size_t hosts_array_size = list.size() * sizeof(DataHostListItem);
  size_t sz = sizeof(GetHostListReplyMessage) + hosts_array_size;

  char buf[sz];
  GetHostListReplyMessage* reply = (GetHostListReplyMessage*)buf;
  reply->numHosts = list.size();
  memcpy(reply->hosts, &list[0], hosts_array_size);

  RegionMatrixProxy* region = reinterpret_cast<RegionMatrixProxy*>(encodedPtr);
  region->sendReply(buf, sz, base);
}

void RegionDataI::processAllocDataMsg(const BaseMessageHeader*, size_t, AllocDataReplyMessage& reply, size_t& len, EncodedPtr) {
  reply.result = this->allocData();
  len = sizeof(AllocDataReplyMessage);
}

void RegionDataI::processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t, UpdateHandlerChainReplyMessage& reply, size_t& len, EncodedPtr) {
  UpdateHandlerChainMessage* msg = (UpdateHandlerChainMessage*)base->content();

  reply.dataHost = HostPid::self();
  reply.numHops = msg->numHops;
  reply.encodedPtr = reinterpret_cast<EncodedPtr>(this);

  len = sizeof(UpdateHandlerChainReplyMessage);
}

// Printing

int RegionDataI::incCoord(IndexT* coord) {
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
}
