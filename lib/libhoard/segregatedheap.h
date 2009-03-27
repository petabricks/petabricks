/* -*- C++ -*- */

#ifndef _SEGREGATEDHEAP_H_
#define _SEGREGATEDHEAP_H_

#include <assert.h>

/***********************************************
*   A segregated-fits collection of heaps.     *
*   One extra heap is used for objects         *
*   that are "too big".                        *
*                                              *
*  Wait from SizeClassComputer                 *
*    BIG_OBJECT                                *
*    NUM_BINS                                  *
*    int getSizeClass (size_t)                 *
*    size_t getClassSize (int)                 *
*                                              *
***********************************************/

template <class SizeClassComputer,class SuperHeap, class TopHeap>
class SegregatedHeap : public SizeClassComputer {

public:

  enum { Alignment = sizeof(double) };

  inline void * malloc (size_t sz) {
    void * ptr;

    if (sz > SizeClassComputer::BIG_OBJECT) {
      ptr = topheap.malloc (sz);
      return ptr;
    }

    int SizeClass = SizeClassComputer::getSizeClass (sz);
    size_t ssz = SizeClassComputer::getClassSize (SizeClass);
    assert (ssz >= sz);
    
    ptr = myHeaps[SizeClass].malloc (ssz);
    return ptr;
  }

  inline void free (void * ptr) {
    // MUST NOT HAPPEN
    assert (0);
  }

  SuperHeap * getOne (size_t sz) {
    assert (sz <= SizeClassComputer::BIG_OBJECT);

    int SizeClass = SizeClassComputer::getSizeClass (sz);
    return &myHeaps[SizeClass];
  }

private:
  // The little heaps.
  SuperHeap myHeaps[SizeClassComputer::NUM_BINS];
  
  // The big heap.
  TopHeap topheap;
};



class Bins_4096 {
public:

  enum { NUM_BINS = 92 };
  enum { BIG_OBJECT = 3584 };
  
  static const size_t bins[NUM_BINS];

#if USE_OLD_GETSC
  static inline int getSizeClass (const size_t sz) {
  // Find the size class for a given object size
  // (the smallest i such that _sizeTable[i] >= sz).
    int sizeclass = 0;
    while (bins[sizeclass] < sz)
      sizeclass++;
    return sizeclass;
  }
  
  static inline size_t getClassSize (const int sizeclass) {
  assert (sizeclass >= 0);
  assert (sizeclass < NUM_BINS);
  return bins[sizeclass];
  }

#else
  
  static inline int getSizeClass (size_t sz) {
    assert (sz > 0);
    assert (sz <= BIG_OBJECT);
    int sc;
    --sz;
    if (sz <= 512) {
      sc = sz >> 3;
    } else {
      int sz6 = sz >> 6;
      sc = (sz6 <= 32) ?  56 + sz6 : 85 + (sz >> 9);
    }
    assert (sc >= 0);
    assert (sc < NUM_BINS);    
    return sc;
  }
  
  static inline size_t getClassSize (const int i) {
    assert (i >= 0);
    assert (i < NUM_BINS);
#if 0
    size_t sz;
    if (i < 64) {
      sz = (i+1) << 3;
    } else {
      sz = bins[i];
    }
    return sz;
#else
    return bins[i];
#endif
  }
#endif

};

const size_t Bins_4096::bins[] = {8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88,
				  96, 104, 112, 120, 128, 136, 144, 152, 160,
				  168, 176, 184, 192, 200, 208, 216, 224, 232,
				  240, 248, 256, 264, 272, 280, 288, 296, 304,
				  312, 320, 328, 336, 344, 352, 360, 368, 376,
				  384, 392, 400, 408, 416, 424, 432, 440, 448,
				  456, 464, 472, 480, 488, 496, 504, 512, 576,
				  640, 704, 768, 832, 896, 960, 1024, 1088, 1152,
				  1216, 1280, 1344, 1408, 1472, 1536, 1600, 1664,
				  1728, 1792, 1856, 1920, 1984, 2048, 2112, 2560,
				  3072, 3584 };



class Bins_8192 {
public:
  
  enum { BIG_OBJECT = 7680 };
  enum { NUM_BINS = 100 };
  
  static const size_t bins[NUM_BINS];

#if USE_OLD_GETSC
  static inline int getSizeClass (const size_t sz) {
  // Find the size class for a given object size
  // (the smallest i such that _sizeTable[i] >= sz).
    int sizeclass = 0;
    while (bins[sizeclass] < sz)
      sizeclass++;
    return sizeclass;
  }
  
  static inline size_t getClassSize (const int sizeclass) {
  assert (sizeclass >= 0);
  assert (sizeclass < NUM_BINS);
  return bins[sizeclass];
  }

#else

  static inline int getSizeClass (size_t sz) {
    assert (sz > 0);
    assert (sz <= BIG_OBJECT);
    int sc;
    --sz;
    if (sz <= 512) {
      sc = sz >> 3;
    } else {
      int sq = sz >> 6;
      if (sq <= 32)
	sc = 56 + sq;
      else
	sc = 85 + (sq >> 3);
    }
    assert (sc >= 0);
    assert (sc < NUM_BINS);
    return sc;
  }
  
  static inline size_t getClassSize (int i) {
    assert (i >= 0);
    assert (i < NUM_BINS);
    size_t sz;
    if (i < 64) {
      sz = (i+1) << 3;
    } else {
      sz = bins[i];
    }
    return sz;
  }
#endif

};

const size_t Bins_8192::bins[] = {8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88,
				  96, 104, 112, 120, 128, 136, 144, 152, 160,
				  168, 176, 184, 192, 200, 208, 216, 224, 232,
				  240, 248, 256, 264, 272, 280, 288, 296, 304,
				  312, 320, 328, 336, 344, 352, 360, 368, 376,
				  384, 392, 400, 408, 416, 424, 432, 440, 448,
				  456, 464, 472, 480, 488, 496, 504, 512, 576,
				  640, 704, 768, 832, 896, 960, 1024, 1088, 1152,
				  1216, 1280, 1344, 1408, 1472, 1536, 1600, 1664,
				  1728, 1792, 1856, 1920, 1984, 2048, 2112, 2560,
				  3072, 3584, 4096, 4608, 5120, 5632, 6144, 6656, 
				  7168, 7680 };


#endif
