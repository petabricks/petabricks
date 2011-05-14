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

#include "config.h"

#if !defined(PETABRICKSOPENCLUTIL_H) && HAVE_OPENCL
#define PETABRICKSOPENCLUTIL_H

#include <vector>
#include <string>
#include <oclUtils.h>
//#include <CL/cl_platform.h>
//#include <CL/cl.h>

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

  private:
    /** Class is a singleton. */
    OpenCLUtil( ) { }

    static cl_context context;
    static bool has_init;
    static unsigned int active_device;
    static std::vector<OpenCLDevice> devices;
  };
};

#endif
