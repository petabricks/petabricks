/***************************************************************************
 *   Copyright (C) 2006 by Jason Ansel                                     *
 *   jansel@ccs.neu.edu                                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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

#ifndef JCONVERT_H
#define JCONVERT_H

#include <stdlib.h>
#include <sstream>
#include "jassert.h"
#include <string>

namespace jalib
{

  namespace jconvert_internal
  {
    //about 15x faster than sringstream method
    template < typename T, T ( strtoT ) ( const char *, char**, int ) >
    inline T StdLibEC ( const std::string& s, bool strict )
    {
      const char * begin = s.c_str();
      char * end = 0;
      T v = ( *strtoT ) ( begin,&end,10 );
      JASSERT ( end!=0 && end!=begin && ( !strict || *end=='\0' ) ) ( end ) ( begin ) ( strict )
      .Text ( "conversion failed" );
      return v;
    }
    //about 15x faster than sringstream method
    template < typename T, T ( strtoT ) ( const char *, char** ) >
    inline T StdLibEC ( const std::string& s,bool strict )
    {
      const char * begin = s.c_str();
      char * end = 0;
      T v = ( *strtoT ) ( begin,&end );
      JASSERT ( end!=0 && end!=begin && ( !strict || *end=='\0' ) ) ( end ) ( begin ) ( strict )
      .Text ( "conversion failed" );
      return v;
    }

  }

  template<typename X> inline std::string XToString ( const X& x )
  {
    std::ostringstream tmp;
    tmp << x;
    return tmp.str();
  }

  template<typename X> inline X StringToX ( const std::string& s, bool strict=true );
// this is too slow
// {
//     std::istringstream tmp(s);
//     X x;
//     tmp >> x;
//     return x;
// }

  template<> inline std::string StringToX<std::string> ( const std::string& s, bool strict )
  {
    return s;
  }

#define JCONVERT_DECLARE_StringToX(T,TFunc,Function)\
    template<> inline T StringToX<T>(const std::string& s, bool strict){ \
        return jconvert_internal::StdLibEC<TFunc,Function>(s,strict);}

  JCONVERT_DECLARE_StringToX ( short, long, strtol );
  JCONVERT_DECLARE_StringToX ( int, long, strtol );
  JCONVERT_DECLARE_StringToX ( long, long, strtol );
  JCONVERT_DECLARE_StringToX ( unsigned int, unsigned long, strtoul );
  JCONVERT_DECLARE_StringToX ( unsigned long, unsigned long, strtoul );
  JCONVERT_DECLARE_StringToX ( long long, long long, strtoll );
  JCONVERT_DECLARE_StringToX ( unsigned long long, unsigned long long, strtoull );
  JCONVERT_DECLARE_StringToX ( float, float, strtof );
  JCONVERT_DECLARE_StringToX ( double, double, strtod );
  JCONVERT_DECLARE_StringToX ( long double, long double, strtold )


#undef JCONVERT_DECLARE_StringToX

#define StringToInt StringToX<int>
#define StringToDouble StringToX<double>

  template < typename T >
  inline bool Between ( const T& a, const T& b, const T& c )
  {
    return ( a <= b ) && ( b <= c );
  }

  inline bool IsWs(char c) { return c==' '||c=='\n'||c=='\r'||c=='\t';}

  ///
  /// Remove whitespace from both ends of a string, return result
  inline std::string StringTrim(std::string& str){
    std::string::const_iterator b = str.begin();
    std::string::const_iterator e = str.end();
    while(b!=e && IsWs(*b))     ++b;
    while(b<e  && IsWs(*(e-1))) --e;
    return std::string(b,e);
  }

  ///
  /// Call func on each element in list
  template < typename Element, typename List >
  inline void Map(void (Element::*func)(), List& list)
  {
    for( typename List::iterator i=list.begin()
          ; i!=list.end()
          ; ++i)
    {
      Element& obj = *i;
      ((obj).*(func))();
    }
  }
      
  ///
  /// Call func on each element in list
  template < typename Element, typename Arg,  typename List >
  inline void Map(void (Element::*func)(Arg&), Arg& arg , List& list)
  {
    for( typename List::iterator i=list.begin()
          ; i!=list.end()
          ; ++i)
    {
      Element& obj = *i;
      ((obj).*(func))(arg);
    }
  }

  ///
  /// Call func on each element in list
  template < typename Element, typename List >
  inline void ConstMap(void (Element::*func)() const, const List& list){
    for( typename List::const_iterator i=list.begin()
       ; i!=list.end()
       ; ++i)
    {
      const Element& obj = *i;
      ((obj).*(func))();
    }
  }

  ///
  /// Call func on each element in list
  template < typename Element, typename Arg,  typename List >
  inline void ConstMap(void (Element::*func)(Arg&) const, Arg& arg , const List& list)
  {
    for( typename List::const_iterator i=list.begin()
          ; i!=list.end()
          ; ++i)
    {
      const Element& obj = *i;
      ((obj).*(func))(arg);
    }
  }

}//namespace jalib

#endif
