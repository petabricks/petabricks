/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/

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
