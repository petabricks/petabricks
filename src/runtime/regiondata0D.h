#ifndef PETABRICKSREGIONDATA0D_H
#define PETABRICKSREGIONDATA0D_H

#include "common/jassert.h"
#include "regiondatai.h"

namespace petabricks {
  using namespace petabricks::RegionDataRemoteMessage;

  class RegionData0D : public RegionDataI, public jalib::JRefCounted {

  private:
    ElementT* _value;
    bool _shouldDeleteValue;

  public:
    RegionData0D() {
      _D = 0;
      _type = RegionDataTypes::REGIONDATA0D;
      _value = (ElementT*)malloc(sizeof(ElementT));
      _shouldDeleteValue = false;
    }

    RegionData0D(ElementT& value) {
      _D = 0;
      _type = RegionDataTypes::REGIONDATA0D;
      _value = &value;
      _shouldDeleteValue = false;
    }

    ~RegionData0D() {
      if (_shouldDeleteValue) delete _value;
    }

    long refCount() const { return jalib::JRefCounted::refCount(); }
    void incRefCount() const { jalib::JRefCounted::incRefCount(); }
    void decRefCount() const { jalib::JRefCounted::decRefCount(); }

    int allocData() {
      return 0;
    }

    ElementT readCell(const IndexT* /*coord*/) const {
      return *_value;
    }

    void writeCell(const IndexT* /*coord*/, ElementT value) {
      *_value = value;
    }

    void randomize() {
      *_value = MatrixStorage::rand();
    }

    RegionDataIPtr hosts(const IndexT* /*begin*/, const IndexT* /*end*/, DataHostPidList& list) {
      DataHostPidListItem item = {HostPid::self(), 1};
      list.push_back(item);
      return NULL;
    }

    RemoteHostPtr dataHost() { return NULL; }

    void processReadCellCacheMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
      size_t values_sz = sizeof(ElementT);
      size_t sz = sizeof(ReadCellCacheReplyMessage) + values_sz;

      char buf[sz];
      ReadCellCacheReplyMessage* reply = (ReadCellCacheReplyMessage*)buf;

      reply->start = 0;
      reply->end = 0;
      reply->values[0] = readCell(NULL);

      caller->sendReply(buf, sz, base);
    }

    // Used in RegionMatrix::_toLocalRegion()
    ElementT& value0D(const IndexT* /*coord*/) const {
      return *_value;
    }

    void print() {
      printf("%e\n", this->readCell(NULL));
    }
  };

  class ConstRegionData0D : public RegionDataI, public jalib::JRefCounted {

  private:
    ElementT _value;

  public:
    ConstRegionData0D() {
      _D = 0;
      _type = RegionDataTypes::CONSTREGIONDATA0D;
    }

    ConstRegionData0D(ElementT value) {
      _D = 0;
      _type = RegionDataTypes::CONSTREGIONDATA0D;
      _value = value;
    }

    long refCount() const { return jalib::JRefCounted::refCount(); }
    void incRefCount() const { jalib::JRefCounted::incRefCount(); }
    void decRefCount() const { jalib::JRefCounted::decRefCount(); }

    int allocData() {
      return 0;
    }

    ElementT readCell(const IndexT* /*coord*/) const {
      return _value;
    }

    void writeCell(const IndexT* /*coord*/, ElementT /*value*/) {
      JASSERT(false);
    }

    void randomize() {
      _value = MatrixStorage::rand();
    }

    RegionDataIPtr hosts(const IndexT* /*begin*/, const IndexT* /*end*/, DataHostPidList& list) {
      DataHostPidListItem item = {HostPid::self(), 1};
      list.push_back(item);
      return NULL;
    }

    RemoteHostPtr dataHost() { return NULL; }

    void processReadCellCacheMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
      size_t values_sz = sizeof(ElementT);
      size_t sz = sizeof(ReadCellCacheReplyMessage) + values_sz;

      char buf[sz];
      ReadCellCacheReplyMessage* reply = (ReadCellCacheReplyMessage*)buf;

      reply->start = 0;
      reply->end = 0;
      reply->values[0] = readCell(NULL);

      caller->sendReply(buf, sz, base);
    }


    void print() {
      printf("%e\n", this->readCell(NULL));
    }
  };
}

#endif
