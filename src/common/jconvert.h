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

  inline bool StartsWith(const char* str, const char* v) {
    while(*str!=0 && *v!=0 && *str++ == *v++){}
    return *v==0;
  }
  inline bool StartsWith(const std::string& str, const char* v) {
    return StartsWith(str.c_str(), v);
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

  ///Replaces every occurrence "str" with "withStr" in string "inStr"
  inline std::string replace(const std::string str, const std::string withStr, const std::string inStr) {
    size_t pos;
    std::string result=inStr;
    while((pos=result.find(str)) != std::string::npos) {
      result=result.replace(pos, str.length(), withStr);
    }
    
    return result;
  }

  /**
   * Escape characters that will interfere with xml.
   *
   * @param sSrc The src string to escape.
   * @return sSrc encoded for insertion into xml.
   */
  inline std::string escapeXML( const std::string &sSrc ) {
      std::ostringstream sRet;

      for( std::string::const_iterator iter = sSrc.begin(); iter!=sSrc.end(); iter++ )
      {
           unsigned char c = (unsigned char)*iter;

           switch( c )
           {
               case '&': sRet << "&amp;"; break;
               case '<': sRet << "&lt;"; break;
               case '>': sRet << "&gt;"; break;
               case '"': sRet << "&quot;"; break;
               case '\'': sRet << "&apos;"; break;

               default:
                if ( c<32 || c>127 )
                {
                     sRet << "&#" << (unsigned int)c << ";";
                }
                else
                {
                     sRet << c;
                }
           }
      }

      return sRet.str();
  }

  /**
   * Unescape characters that whould have interfered with xml.
   *
   * @param sSrc The src string to escape.
   * @return sSrc encoded for insertion into xml.
   */
  inline std::string unescapeXML( const std::string &sSrc ) {
    std::string result;
    result = replace("&amp;", "&", sSrc);
    result = replace("&lt;", "<", result);
    result = replace("&gt;", ">", result);
    result = replace("&quot;", "\"", result);
    result = replace("&apos;", "\\", result);
    return result;
  }

}//namespace jalib

#endif
