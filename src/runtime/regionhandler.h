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
    void processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, ReadCellReplyMessage& reply, size_t& len, EncodedPtr caller);
    void processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, WriteCellReplyMessage& reply, size_t& len, EncodedPtr caller);
    void processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen,GetHostListReplyMessage& reply, size_t& len, EncodedPtr caller);
    void processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen, AllocDataReplyMessage& reply, size_t& len, EncodedPtr caller);
  };
}

#endif
