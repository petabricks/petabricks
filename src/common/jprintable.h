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
#ifndef JALIBJPRINTABLE_H
#define JALIBJPRINTABLE_H

#include <string>
#include <iostream>
#include <sstream>

namespace jalib {

/**
 * Object that can be converted to a string
 */
class JPrintable {
public:
  /// virtual destructor
  virtual ~JPrintable(){}

  /// toString calls print()
  std::string toString() const;

  /// must be implemented by base class
  virtual void print(std::ostream& o) const = 0;

  ///print to stdout
  void print() const { print(std::cout); }


  template < typename T >
  static void printStlList(std::ostream& o, const T& obj){
    printStlList(o, obj.begin(), obj.end());
  }

  template < typename T >
  static void printStlList(std::ostream& o, const T& begin, const T& end, const char* sep = ""){
    for(T i=begin; i!=end; ++i) o << (i!=begin?sep:"") << *i;
  }


  template < typename T >
  static std::string stringStlList(const T& begin, const T& end, const char* sep = ""){
    std::ostringstream o;
    printStlList(o, begin, end, sep);
    return o.str();
  }
};

}

///interop with std c++
std::ostream& operator<< (std::ostream& o, const jalib::JPrintable& p);

#endif
