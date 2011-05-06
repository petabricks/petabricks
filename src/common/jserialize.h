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
#ifndef JALIBJSERIALIZE_H
#define JALIBJSERIALIZE_H


#include "jassert.h"

#include <string>
#include <vector>

#define JSERIALIZE_ASSERT_POINT(str) \
    { char versionCheck[] = str;                                        \
    static const std::string correctValue = versionCheck;               \
    o & versionCheck;                                                      \
    JASSERT(versionCheck == correctValue)(versionCheck)(correctValue)(o.filename()) \
            .Text("invalid file format"); }

namespace jalib
{

  class JBinarySerializer
  {
    public:
      virtual ~JBinarySerializer() {}



      virtual void readOrWrite ( void* buffer, size_t len ) = 0;
      virtual bool isReader() = 0;
      bool isWriter() { return ! isReader(); }

      template < typename T >
      void serialize ( T& t ) {readOrWrite ( &t, sizeof ( T ) );}

      template < typename T >
      JBinarySerializer& operator& ( T& t )
      {
        serialize ( t );
        return *this;
      }

      template < typename T >
      void serializeVector ( std::vector<T>& t )
      {
        JBinarySerializer& o = *this;

        JSERIALIZE_ASSERT_POINT ( "std::vector:" );

        //establish the size
        size_t len = t.size();
        serialize ( len );

        //make sure we have correct size
        t.resize ( len );

        //now serialize all the elements
        for ( size_t i=0; i<len; ++i )
        {
          JSERIALIZE_ASSERT_POINT ( "[" );
          serialize ( t[i] );
          JSERIALIZE_ASSERT_POINT ( "]" );
        }

        JSERIALIZE_ASSERT_POINT ( "endvector" );
      }

      const std::string& filename() const {return _filename;}
      JBinarySerializer ( const std::string& filename ) : _filename ( filename ) {}
    private:
      std::string _filename;
  };

  template <>
  inline void JBinarySerializer::serialize<std::string> ( std::string& t )
  {
    size_t len = t.length();
    serialize ( len );
    t.resize ( len,'?' );
    readOrWrite ( &t[0], len );
  }

  template <>
  inline void JBinarySerializer::serialize<std::vector<int> > ( std::vector<int>& t )
  {
    serializeVector<int> ( t );
  }

  class JBinarySerializeWriterRaw : public JBinarySerializer
  {
    public:
      JBinarySerializeWriterRaw ( const std::string& file, int fd );
      void readOrWrite ( void* buffer, size_t len );
      bool isReader();
    protected:
      int _fd;
  };

  class JBinarySerializeWriter : public JBinarySerializeWriterRaw
  {
    public:
      JBinarySerializeWriter ( const std::string& path );
      ~JBinarySerializeWriter();
  };

  class JBinarySerializeReaderRaw : public JBinarySerializer
  {
    public:
      JBinarySerializeReaderRaw ( const std::string& file, int fd );
      void readOrWrite ( void* buffer, size_t len );
      bool isReader();
    protected:
      int _fd;
  };
  
  class JBinarySerializeReader : public JBinarySerializeReaderRaw
  {
    public:
      JBinarySerializeReader ( const std::string& path );
      ~JBinarySerializeReader();
  };


}



#endif
