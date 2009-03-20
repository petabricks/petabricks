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

#include "matrix.h"
#include "matrixio.h"
#include "dynamictask.h"
#include "spatialdynamictask.h"
#include "petabricksruntime.h"
#include "jtunable.h"
#include "config.h"

#ifdef HAVE_MATH_H
#  include <math.h>
#endif 

#ifdef HAVE_FFTW3_H
#  include <fftw3.h>
#endif

#define SPAWN(taskname, args...) \
  { DynamicTaskPtr _task = spawn_ ## taskname (args , _before); \
    _after->dependsOn(_task);\
    _task->enqueue();\
  }

#define PB_CAT(a,b) _PB_CAT(a,b)
#define _PB_CAT(a,b) __PB_CAT(a,b)
#define __PB_CAT(a,b) a ## b

#define SYNC() \
  { \
    _before = _after; \
    _after = new NullDynamicTask(); \
    _after->dependsOn(_before); \
    _before->enqueue(); \
  }

