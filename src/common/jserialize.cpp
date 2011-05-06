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
#include "jserialize.h"
#include "jassert.h"
#include "stdio.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

jalib::JBinarySerializeWriterRaw::JBinarySerializeWriterRaw ( const std::string& path, int fd )
    : JBinarySerializer ( path )
    , _fd ( fd )
{
  JASSERT (_fd >= 0)(path)(JASSERT_ERRNO).Text("open(path) failed");
}

jalib::JBinarySerializeWriter::JBinarySerializeWriter ( const std::string& path )
  : JBinarySerializeWriterRaw ( path , open ( path.c_str(), O_CREAT|O_WRONLY|O_TRUNC, 0600) )
{}

jalib::JBinarySerializeReaderRaw::JBinarySerializeReaderRaw ( const std::string& path, int fd )
  : JBinarySerializer ( path )
  , _fd ( fd )
{
  JASSERT (_fd >= 0)(path)(JASSERT_ERRNO).Text("open(path) failed");
}

jalib::JBinarySerializeReader::JBinarySerializeReader ( const std::string& path )
  : JBinarySerializeReaderRaw ( path , open ( path.c_str(), O_RDONLY ) )
{}

jalib::JBinarySerializeWriter::~JBinarySerializeWriter()
{
  close ( _fd );
}

jalib::JBinarySerializeReader::~JBinarySerializeReader()
{
  close ( _fd );
}


bool jalib::JBinarySerializeWriterRaw::isReader() {return false;}

bool jalib::JBinarySerializeReaderRaw::isReader() {return true;}


void jalib::JBinarySerializeWriterRaw::readOrWrite ( void* buffer, size_t len )
{
  char* b=(char*)buffer;
  for(ssize_t n; len>0; ){
    n=write(_fd, b, len);
    JASSERT(n>0 && n<=(ssize_t)len)(filename())(n)(len)(JASSERT_ERRNO).Text("write() failed");
    len -= n;
    b += n;
  }
}


void jalib::JBinarySerializeReaderRaw::readOrWrite ( void* buffer, size_t len )
{
  ssize_t retries=len*32;
  char* b=(char*)buffer;
  for(ssize_t n; len>0; ){
    n=read (_fd, b, len);
    JASSERT(n>=0 && n<=(ssize_t)len)(filename())(n)(len)(JASSERT_ERRNO).Text("read() failed");
    len -= n;
    b += n;
    JASSERT(n>0 || retries-->0)(len)
      .Text("still reading 0 bytes, broken pipe?");
  }
}

