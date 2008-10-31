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
#ifndef HECURASPATIALDYNAMICTASK_H
#define HECURASPATIALDYNAMICTASK_H

#include "dynamictask.h"
#include "matrix.h"
#include "matrixdependency.h"
#include <vector>

namespace hecura {

class SpatialDynamicTask;
class SpatialTaskList;
typedef jalib::JRef<SpatialDynamicTask> SpatialTaskPtr;
typedef jalib::JRef<SpatialTaskList> SpatialTaskListPtr;


class SpatialDynamicTask : public DynamicTask {
public:
  virtual IndexT beginPos(int dimension) const = 0;
  virtual IndexT endPos(int dimension) const = 0;
  virtual void spatialSplit(SpatialTaskList& output, int dim, int n) = 0;
};

class SpatialTaskList : public std::vector< SpatialTaskPtr > {
public:
  SpatialTaskList(const SpatialTaskPtr& initial) : std::vector< SpatialTaskPtr >(1, initial) {}
  SpatialTaskList() {}

  void spatialSplit(int dim, int n);

  template< int D >
  void dependsOn(const SpatialTaskList& that, DependencyDirection::DirectionT dir[D]){
    for(const_iterator a=begin(); a!=end(); ++a){
      for(const_iterator b=that.begin(); b!=that.end(); ++b){
//         for(int d=0; d<D; ++d){
        (*a)->dependsOn(DynamicTaskPtr(b->asPtr()));
//         }
      }
    }
  }

  void dependsOn(DynamicTaskPtr& b){
    for(const_iterator a=begin(); a!=end(); ++a){
      (*a)->dependsOn(b);
    }
  }
};


}

#endif
