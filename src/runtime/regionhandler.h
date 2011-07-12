#ifndef PETABRICKSREGIONHANDLER_H
#define PETABRICKSREGIONHANDLER_H

#include "common/jassert.h"
#include "common/jrefcounted.h"
#include "regiondatai.h"
#include "remotehost.h"

namespace petabricks {
  using namespace petabricks::RegionDataRemoteMessage;

  class RegionHandler;
  typedef jalib::JRef<RegionHandler> RegionHandlerPtr;

  class RegionHandler : public jalib::JRefCounted {
  private:
    RegionDataIPtr _regionData;

  public:
    RegionHandler(const int dimensions, const IndexT* size);
    RegionHandler(const int dimensions, const IndexT* size, const IndexT* partOffset);
    RegionHandler(const RegionDataIPtr regionData);
    RegionHandler(const EncodedPtr remoteObjPtr);

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    int allocData();

    RegionDataIPtr getRegionData();
    void updateRegionData(RegionDataIPtr regionData);

    DataHostList hosts(IndexT* begin, IndexT* end);

    int dimensions();
    IndexT* size();
    RegionDataType type() const;

    // Migration
    EncodedPtr moveToRemoteHost(RemoteHostPtr host);
    void updateHandlerChain();
    bool isHandlerChainUpdated(); // for testing

    // RegionDataSplit
    void splitData(IndexT* splitSize);
    void createDataPart(int partIndex, RemoteHostPtr host);

    // Process Remote Messages
    void processReadCellMsg(ReadCellMessage* msg, ReadCellReplyMessage* reply, int* len);
    void processWriteCellMsg(WriteCellMessage* msg, WriteCellReplyMessage* reply, int* len);
    void processGetHostListMsg(GetHostListMessage* msg, GetHostListReplyMessage* reply, int* len);
    void processAllocDataMsg(AllocDataMessage* msg, AllocDataReplyMessage* reply, int* len);
  };
}

#endif
