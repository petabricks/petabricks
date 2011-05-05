/*****************************************************************************
 *  Copyright (C) 2008-2011 Massachusetts Institute of Technology            *
 *                                                                           *
 *  Permission is hereby granted, free of charge, to any person obtaining    *
 *  a copy of this software and associated documentation files (the          *
 *  "Software"), to deal in the Software without restriction, including      *
 *  without limitation the rights to use, copy, modify, merge, publish,      *
 *  distribute, sublicense, and/or sell copies of the Software, and to       *
 *  permit persons to whom the Software is furnished to do so, subject       *
 *  to the following conditions:                                             *
 *                                                                           *
 *  The above copyright notice and this permission notice shall be included  *
 *  in all copies or substantial portions of the Software.                   *
 *                                                                           *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY                *
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE               *
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      *
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   *
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   *
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    *
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE           *
 *                                                                           *
 *  This source code is part of the PetaBricks project:                      *
 *    http://projects.csail.mit.edu/petabricks/                              *
 *                                                                           *
 *****************************************************************************/
#ifndef PETABRICKSHASH_H
#define PETABRICKSHASH_H

#include "jassert.h"

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
    friend std::ostream& operator<<(std::ostream& o, const Hash& h) {
      char str[HASH_LEN*2+1] = {0};
      for(int i=0; i<HASH_LEN; ++i)
        sprintf(str+2*i, "%02x", h._buf[i]);
      return o<<"0x"<<str;
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

