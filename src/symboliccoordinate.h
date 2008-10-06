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
#ifndef HECURASYMBOLICCOORDINATE_H
#define HECURASYMBOLICCOORDINATE_H

#include "jrefcounted.h"

#include <vector>
#include <map>
#include <iostream>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifndef DIMENSIONS
#define DIMENSIONS 2
#endif

namespace hecura {

class                                    CoordPart;
typedef jalib::JRef<const CoordPart>     CoordPartPtrRO;
typedef jalib::JRef<CoordPart>           CoordPartPtrRW;
typedef CoordPartPtrRO                   CoordPartPtr;
class SummationCoordPart;

/**
 * A single part (x, y, or z) of a coordinate.
 * Abstract class, parent of both symbolic values, literals, and aggregate types.
 */
class CoordPart : public jalib::JRefCounted {
public:
  typedef int    TValue;    //type for matrix values
  typedef TValue TMultiple; //type for multiples of CoordParts

  virtual void print(std::ostream& os) const = 0;

  CoordPartPtr normalize() const;
  virtual void normalize(TMultiple mult, SummationCoordPart& symbols, TValue& literals) const = 0;

  virtual bool isSimple() const;
protected:
  CoordPart(){}
  virtual ~CoordPart(){}
};

/**
 * A single part (x, y, or z) of a coordinate.
 * A literal 1,2,3,... value
 */
class LiteralCoordPart : public CoordPart {
public:
  LiteralCoordPart( int v ) : _value(v) {}

  void print(std::ostream& os) const;
  void normalize(TMultiple mult, SummationCoordPart& symbols, TValue& literals) const;
  TValue value() const { return _value; }
private:
  TValue _value;
};

/**
 * A single part (x, y, or z) of a coordinate.
 * A symbolic (unknown) component
 */
class SymbolCoordPart : public CoordPart {
  static std::string makeDisplayName();
public:
  SymbolCoordPart( const char* name = NULL );

  void print(std::ostream& os) const;
  void normalize(TMultiple mult, SummationCoordPart& symbols, TValue& literals) const;

private:
  std::string _displayName;
};

/**
 * A single part (x, y, or z) of a coordinate.
 * A combination of multiple parts
 */
class SummationCoordPart : public CoordPart {
public:
  SummationCoordPart(const CoordPartPtr& e1 , const CoordPartPtr& e2){ 
    add(e1); 
    add(e2); 
  }
  SummationCoordPart(TMultiple mult , const CoordPartPtr& e){ 
    add(e, mult); 
  }
  SummationCoordPart(TMultiple m1, const CoordPartPtr& e1, TMultiple m2, const CoordPartPtr& e2){
    add(e1, m1);
    add(e2, m2);
  }
  SummationCoordPart(){}
  
  void print(std::ostream& os) const;
  void normalize(TMultiple mult, SummationCoordPart& symbols, TValue& literals) const;

  void add(const CoordPartPtr& e, TMultiple count = 1){ _parts[e] += count; }

  bool isSimple() const;
  bool isNormalize() const;
private:
  typedef std::map<CoordPartPtr, TMultiple> TParts;
  TParts _parts;
};


class SymCoord {
public:
    SymCoord();
    virtual ~SymCoord();

    const CoordPartPtr& operator[](int n) const { return _parts[n]; }
    const CoordPartPtr& x() const { return operator[](0); }
#if DIMENSIONS > 1
    const CoordPartPtr& y() const { return operator[](1); }
#endif
#if DIMENSIONS > 2
    const CoordPartPtr& z() const { return operator[](2); }
#endif
private:

private:
  CoordPartPtr _parts[DIMENSIONS];
};

}

std::ostream& operator<< ( std::ostream& os, const hecura::CoordPartPtr& part );
std::ostream& operator<< ( std::ostream& os, const hecura::CoordPart& part );

#endif
