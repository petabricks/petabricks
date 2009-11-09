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
#ifndef PETABRICKSPBC_H
#define PETABRICKSPBC_H

#include "config.h"

namespace petabricks
{

#if defined(HAVE_OPENCL)

enum OpenCLMode
{
	E_OPENCL_DISABLED,
	E_OPENCL_ENABLED,
};

extern OpenCLMode theOpenCLMode;

#endif

/*
enum CodeGenerationMode
  {
    E_CGM_STATIC,
    E_CGM_DYNAMIC,
  #if defined(HAVE_OPENCL)
    E_CGM_OPENCL,
  #endif
  };

}
*/

enum RuleFlavor
  {
    //dynamic first for backward compatibility with isStatic (if we missed any more places)
    E_RF_DYNAMIC,
    E_RF_STATIC,
  #if defined(HAVE_OPENCL)
    E_RF_OPENCL,
  #endif
  };

}

#endif
