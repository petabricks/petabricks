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

#ifndef JALLOC_H
#define JALLOC_H

#include <stdlib.h>

namespace jalib
{

class JAlloc {
public:
  static void* allocate(size_t n);
  static void deallocate(void* ptr, size_t n);
  static void* reallocate(void* ptr, size_t oldn, size_t newn);
};

class JAllocRaw {
public:
  static void* allocate(size_t n);
  static void deallocate(void* ptr, size_t n);
  static void* reallocate(void* ptr, size_t oldn, size_t newn);
};

}

#endif
