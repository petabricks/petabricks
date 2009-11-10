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
#ifndef JCONVERT_H
#define JCONVERT_H

#include "jassert.h"

#include <algorithm>
#include <limits>
#include <sstream>
#include <stdlib.h>
#include <string>

namespace jalib
{

  namespace jconvert_internal
  {
    //about 15x faster than stringstream method
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
    //about 15x faster than stringstream method
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

  template<> inline std::string StringToX<std::string> ( const std::string& s, bool /*strict*/ )
  {
    return s;
  }

#define JCONVERT_DECLARE_StringToX(T,TFunc,Function)\
    template<> inline T StringToX<T>(const std::string& s, bool strict){ \
        return jconvert_internal::StdLibEC<TFunc,Function>(s,strict);}

  JCONVERT_DECLARE_StringToX ( short, long, strtol )
  JCONVERT_DECLARE_StringToX ( int, long, strtol )
  JCONVERT_DECLARE_StringToX ( long, long, strtol )
  JCONVERT_DECLARE_StringToX ( unsigned int, unsigned long, strtoul )
  JCONVERT_DECLARE_StringToX ( unsigned long, unsigned long, strtoul )
  JCONVERT_DECLARE_StringToX ( long long, long long, strtoll )
  JCONVERT_DECLARE_StringToX ( unsigned long long, unsigned long long, strtoull )
  JCONVERT_DECLARE_StringToX ( float, float, strtof )
  JCONVERT_DECLARE_StringToX ( double, double, strtod )
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
  inline std::string StringTrim(const std::string& str){
    std::string::const_iterator b = str.begin();
    std::string::const_iterator e = str.end();
    while(b!=e && IsWs(*b))     ++b;
    while(b<e  && IsWs(*(e-1))) --e;
    return std::string(b,e);
  }

  inline std::string StringPad(const std::string& str, int n, char ch=' '){
    if((int)str.size()>=n) return str;
    return str+ std::string(n-str.size(), ch);
  }

  ///
  /// Remove whitespace from both ends of a string, return result
  inline void SplitFirst(std::string& left, std::string& right, const std::string& str, char delim){
    std::string::const_iterator b = str.begin();
    std::string::const_iterator m = str.begin();
    std::string::const_iterator e = str.end();
    while(m<e && *m!=delim) ++m;
    left.assign(b,m);
    if(m<e)
      right.assign(m+1,e);
    else
      right="";
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
  template < typename Element, typename Arg1, typename Arg2,  typename List >
  inline void Map(void (Element::*func)(Arg1&, Arg2&), Arg1& arg1, Arg2& arg2 , List& list)
  {
    for( typename List::iterator i=list.begin()
          ; i!=list.end()
          ; ++i)
    {
      Element& obj = *i;
      ((obj).*(func))(arg1, arg2);
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

  inline bool Contains(const char* str, char c) {
    while(*str!=0 && *str!=c) ++str;
    return *str==c;
  }
  inline bool Contains(const std::string& str, char c) {
    return Contains(str.c_str(), c);
  }

  template < typename T >
  inline T minval(){ 
    return std::min<T>(-1.0*std::numeric_limits<T>::max(),//min for floats 
           std::min<T>(std::numeric_limits<T>::min(),
                       0));//min for unsigned
  }
  
  template < typename T >
  inline T maxval(){
    return std::numeric_limits<T>::max();
  }

}//namespace jalib

#endif
