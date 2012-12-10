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
#ifndef PETABRICKSOPENCLUTIL_H
#define PETABRICKSOPENCLUTIL_H

#include <vector>
#include <string>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef HAVE_OCLUTILS_H
# include <oclUtils.h>
#endif

#ifdef HAVE_SDKCOMMON_HPP
# include <SDKCommon.hpp>
#endif

#ifdef HAVE_SDKAPPLICATION_HPP
# include <SDKApplication.hpp>
#endif

#ifdef HAVE_SDKFILE_HPP
# include <SDKFile.hpp>
#endif

#ifdef HAVE_OPENCL_OPENCL_H
# include <OpenCL/opencl.h>
#endif

#ifdef HAVE_OPENCL_H
# include <opencl.h>
#endif

#ifndef HAVE_OPENCL
//make our function signatures compile without opencl
typedef int cl_command_queue;
typedef int cl_context;
typedef int cl_device_id;
typedef int cl_int;
typedef int cl_kernel;
typedef int cl_mem;
typedef int cl_platform_id;
typedef int cl_program;
typedef int cl_uint;
typedef int cl_ulong;
typedef int cl_event;
#endif

namespace petabricks
{



  struct OpenCLDevice
  {
    OpenCLDevice( cl_device_id _id );

    /** OpenCL-assigned unique identifier for this device. */
    cl_device_id id;
    /** Handle to command queue for this device. */
    cl_command_queue queue;
    /** True iff we're using this device. */
    bool enabled;
    /** Arbitrary string returned by OpenCL driver.  Typically the name of
	the graphics card. */
    std::string name;
    /** Arbitrary string returned by OpenCL driver describing the company
	which produced the GPU. */
    std::string vendor;

    cl_uint max_compute_units;
    cl_uint max_clock_freq;
    size_t max_workitem_size[3];
    size_t max_workgroup_size;
    cl_ulong global_mem_size;
    cl_ulong local_mem_size;
    bool has_queue_outoforder_exec;
    bool has_queue_profiling;
  };

  class OpenCLUtil
  {
  public:
    /** Initializes OpenCL and enumerates devices.  Returns nonzero on error. */
    static int init( );
    static void deinit( );
    static void printDeviceList( bool verbose = true );
    static void printDeviceDetails( const OpenCLDevice& dev_info,
				    bool verbose = true );
    static void printDeviceDetails( unsigned int dev_idx,
				    bool verbose = true );
    static cl_context getContext( );
    static cl_command_queue getQueue( unsigned int dev_idx );
    static cl_int buildProgram( cl_program &program );
    static std::string errorString( cl_int error );

    static void setActiveDevice( unsigned int dev_idx );
    static unsigned int getActiveDevice( );
    static cl_device_id getActiveDeviceID( );

    static cl_platform_id getPlatform();

    static bool buildKernel(cl_program& cprog, cl_kernel& clkern, const char* clsrc);

  private:
    /** Class is a singleton. */
    OpenCLUtil( ) { }
    static void pfn_notify(const char *errinfo, const void *private_info, size_t cb, void *user_data);

    static cl_context context;
    static bool has_init;
    static unsigned int active_device;
    static std::vector<OpenCLDevice> devices;

  };
};

#endif
