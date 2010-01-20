/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/
#ifndef PETABRICKSHASH_H
#define PETABRICKSHASH_H

#include <stdio.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#if defined(HASH_USE_SHA1) && HASH_USE_SHA1
#  ifdef HAVE_OPENSSL_SHA_H
#    include <openssl/sha.h>
#  endif
#  define HASH_FN         SHA1
#  define HASH_LEN        SHA_DIGEST_LENGTH
#  define HASH_CTX        SHA1_CTX
#  define HASH_CTX_INIT   SHA1_Init
#  define HASH_CTX_UPDATE SHA1_Update
#  define HASH_CTX_FINAL  SHA1_Final
#else
#  ifdef HAVE_OPENSSL_MD5_H
#    include <openssl/md5.h>
#  endif
#  define HASH_FN         MD5
#  define HASH_LEN        MD5_DIGEST_LENGTH
#  define HASH_CTX        MD5_CTX
#  define HASH_CTX_INIT   MD5_Init
#  define HASH_CTX_UPDATE MD5_Update
#  define HASH_CTX_FINAL  MD5_Final
#endif

namespace jalib { 

  class Hash {
  public:
    friend bool operator==(const Hash& a, const Hash& b){
      return memcmp(a._buf, b._buf, HASH_LEN)==0;
    }
    Hash(){
      memset(_buf, 0, sizeof _buf);
    }
    unsigned char* buf() { return _buf; }
    const unsigned char* buf() const { return _buf; }

    void print() const {
      for(int i=0; i<HASH_LEN; ++i)
        printf("%02x", _buf[i]);
    }
  private:
    unsigned char _buf[HASH_LEN];
  };


  class HashGenerator {
  public:
    HashGenerator(){
      init();
    }
    void init(){
      _isFinal=false;
      int rv = HASH_CTX_INIT(&_ctx);
      JASSERT(rv==1);
    }
    void update(const void* data, size_t len){
      int rv = HASH_CTX_UPDATE(&_ctx, data, len);
      JASSERT(rv==1);
    }
    Hash final() {
      JASSERT(!_isFinal);
      Hash t;
      int rv = HASH_CTX_FINAL(t.buf(), &_ctx);
      JASSERT(rv==1);
      _isFinal = true;
      return t;
    }
  private:
    HASH_CTX _ctx;
    bool _isFinal;
  };


}




#endif

