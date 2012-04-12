#ifndef PETABRICKSREGIONDATA0D_H
#define PETABRICKSREGIONDATA0D_H

#include "common/jassert.h"
#include "regiondatai.h"

namespace petabricks {
  using namespace petabricks::RegionDataRemoteMessage;

  class RegionData0D : public RegionDataI, public jalib::JRefCounted {

  private:
    ElementT* _value;
    MatrixStoragePtr _storage;

  public:
    RegionData0D() {
      _D = 0;
      _type = RegionDataTypes::REGIONDATA0D;
      _storage = new MatrixStorage(1);
      _value = _storage->data();
    }

    RegionData0D(ElementT& value) {
      _D = 0;
      _type = RegionDataTypes::REGIONDATA0D;
      _value = &value;
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

    RegionDataIPtr copyToScratchMatrixStorage(CopyToMatrixStorageMessage* /*origMetadata*/, size_t /*len*/, MatrixStoragePtr /*scratchStorage*/, RegionMatrixMetadata* /*scratchMetadata*/, const IndexT* /*scratchStorageSize*/) {
      JASSERT(false);
      return NULL;
    }

    void copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* /*origMetadata*/, size_t /*len*/, MatrixStoragePtr /*scratchStorage*/, RegionMatrixMetadata* /*scratchMetadata*/, const IndexT* /*scratchStorageSize*/) {
      JASSERT(false);
    }

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

    ConstRegionData0D(const ElementT value) {
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

    RegionDataIPtr copyToScratchMatrixStorage(CopyToMatrixStorageMessage* /*origMetadata*/, size_t /*len*/, MatrixStoragePtr /*scratchStorage*/, RegionMatrixMetadata* /*scratchMetadata*/, const IndexT* /*scratchStorageSize*/) {
      JASSERT(false);
      return NULL;
    }

    void copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* /*origMetadata*/, size_t /*len*/, MatrixStoragePtr /*scratchStorage*/, RegionMatrixMetadata* /*scratchMetadata*/, const IndexT* /*scratchStorageSize*/) {
      JASSERT(false);
    }

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
