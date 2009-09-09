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
#ifndef PETABRICKSSPATIALDYNAMICTASK_H
#define PETABRICKSSPATIALDYNAMICTASK_H

#include "dynamictask.h"
#include "matrix.h"
#include "string.h"

#include <vector>

namespace petabricks {

template< typename T, int D, DynamicTaskPtr (T::*method)(IndexT begin[D], IndexT end[D])>
class SpatialMethodCallTask : public DynamicTask {
public:
  SpatialMethodCallTask(const jalib::JRef<T>& obj, IndexT begin[D], IndexT end[D])
    : _obj(obj)
  {
    memcpy(_begin, begin, sizeof _begin);
    memcpy(_end,   end,   sizeof _end);
  }
  DynamicTaskPtr run(){
    return ((*_obj).*(method))(_begin, _end);
  }
private:
  jalib::JRef<T> _obj;
  IndexT _begin[D];
  IndexT _end[D];
};

}

#endif
