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
#ifndef PETABRICKSGPUTASKINFO_H
#define PETABRICKSGPUTASKINFO_H

#include <map>

#include "common/jmutex.h"
#include "common/openclutil.h"

#include "matrixstorage.h"


namespace petabricks {

class GpuTaskInfo;
typedef jalib::JRef<GpuTaskInfo> GpuTaskInfoPtr;

class GpuTaskInfo: public jalib::JRefCounted {
public:

  /// constructor
  GpuTaskInfo(int nodeID, RegionNodeGroupMapPtr map, int gpuCopyOut) {
    _nodeID = nodeID;
    _map = map;
    _gpuCopyOut = gpuCopyOut;
  }

  ///
  /// add output matrix storage info
  void addToMatrix(MatrixStorageInfoPtr info) { _to.push_back(info); }

  ///
  /// add input matrix storage info
  void addFromMatrix(MatrixStorageInfoPtr info) { _from.push_back(info); }

  int nodeID() { return _nodeID; }
  int gpuCopyOut() { return _gpuCopyOut; }
  RegionNodeGroupMapPtr regionNodeGroupMap() { return _map; }
  
  void print() {
    std::cout << "GpuTaskInfo " << this << std::endl;
  }

  ///
  /// input matrix storage infomation
  std::vector<MatrixStorageInfoPtr> _from;

  ///
  /// output matrix storage infomation
  std::vector<MatrixStorageInfoPtr> _to;

private:
  ///
  /// opencl command queue that is associated with this task
  cl_command_queue _queue;

  ///
  /// opencl kernel that is associated with this task
  cl_kernel _kernel;

  ///
  /// ID of this task
  int _nodeID;

  int _gpuCopyOut;

  ///
  /// a map from matrix name to a set of task IDs of the tasks that write to the matrix and have to be finished running before copying out the matrix
  RegionNodeGroupMapPtr _map;
};

}

#endif
