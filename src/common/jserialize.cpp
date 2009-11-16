/***************************************************************************
 *   Copyright (C) 2006-2009 by Jason Ansel                                *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
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

