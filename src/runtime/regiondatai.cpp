#include "regiondatai.h"

#include <stdio.h>
#include <string.h>

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
void RegionDataI::processReadCellMsg(ReadCellMessage* msg, ReadCellReplyMessage* reply, int* len) {
  reply->value = this->readCell(msg->coord);
  *len = sizeof(ReadCellReplyMessage);
}

void RegionDataI::processWriteCellMsg(WriteCellMessage* msg, WriteCellReplyMessage* reply, int* len) {
  this->writeCell(msg->coord, msg->value);
  reply->value = msg->value;
  *len = sizeof(WriteCellReplyMessage);
}

void RegionDataI::processGetHostListMsg(GetHostListMessage* msg, GetHostListReplyMessage* reply, int* len) {
  UNIMPLEMENTED();
}

void RegionDataI::processAllocDataMsg(AllocDataMessage* /*msg*/, AllocDataReplyMessage* reply, int* len) {
  reply->result = this->allocData();
  *len = sizeof(AllocDataReplyMessage);
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
