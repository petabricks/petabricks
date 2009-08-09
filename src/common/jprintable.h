/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
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
#ifndef JALIBJPRINTABLE_H
#define JALIBJPRINTABLE_H

#include <string>
#include <iostream>

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
};

}

///interop with std c++
std::ostream& operator<< (std::ostream& o, const jalib::JPrintable& p);

#endif
