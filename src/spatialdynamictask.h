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
  virtual int dimensions() const = 0;
  virtual void spatialSplit(SpatialTaskList& output, int dim, int n) = 0;
};

class SpatialTaskList : public std::vector< SpatialTaskPtr > {
public:
  ///
  /// Constructor
  SpatialTaskList(const SpatialTaskPtr& initial) : std::vector< SpatialTaskPtr >(1, initial) {}
  SpatialTaskList() {}

  SpatialTaskList& operator=(const SpatialTaskPtr& b){
    #ifdef DEBUG
    JASSERT(empty());
    #endif
    push_back(b);
    return *this;
  }

  ///
  /// Split the task into smaller tasks
  void spatialSplit(int dim, int n);


  void spatialSplit(int n){
    JASSERT(size()==1)(size());;
    // HACK HACK HACK: skip the first dimension unless task is 1D
    if (back()->dimensions() == 1) {
      spatialSplit(0, n);
    } else {
      for(int i=1; i<back()->dimensions(); ++i)
        spatialSplit(i, n);
    }
  }

  ///
  /// Add a directed dependency between tasklists
  template< int D >
  void dependsOn(SpatialTaskList& that, DependencyDirection::DirectionT dir[D]){
    for(iterator a=begin(); a!=end(); ++a){
      for(iterator b=that.begin(); b!=that.end(); ++b){
        if(*a!=*b){
//           for(int d=0; d<D; ++d){
            (*a)->dependsOn(b->asPtr());
//           }
        }
      }
    }
  }

  ///
  /// Add a directed dependency between tasklists
  template< int D >
  void dependsOn(DynamicTaskPtr& that, DependencyDirection::DirectionT dir[D]){
    dependsOn(that);
  }

  ///
  /// Make all tasks in this depend on b
  void dependsOn(const DynamicTaskPtr& b){
    if(b){
      for(const_iterator a=begin(); a!=end(); ++a){
        (*a)->dependsOn(b);
      }
    }
  }

  ///
  /// Make all tasks in this depend on b
  void enqueue(){
    for(const_iterator a=begin(); a!=end(); ++a){
      (*a)->enqueue();
    }
  }

  ///
  /// Return a task depending on all taks in this
  DynamicTaskPtr completionTask(){
    DynamicTaskPtr tmp = new NullDynamicTask();
    for(iterator a=begin(); a!=end(); ++a){
      tmp->dependsOn(a->asPtr());
    }
    tmp->enqueue();
    return tmp;
  }

  operator DynamicTaskPtr () { return completionTask(); };
};


}

#endif
