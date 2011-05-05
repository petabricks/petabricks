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
#include "openclutil.h"
#include "common/jassert.h"
#include <iostream>

#if HAVE_OPENCL

namespace petabricks
{

bool OpenCLUtil::has_init = false;
std::vector<OpenCLDevice> OpenCLUtil::devices;
cl_context OpenCLUtil::context;
  unsigned int OpenCLUtil::active_device = 0;

int
OpenCLUtil::init( )
{
  #if OPENCL_TRACE
  std::cout << "OpenCLUtil::init() begins...\n";
  #endif

  int err;
  char buf[1024];

  if( true == has_init )
    return 0;

  // Get platform.
  cl_platform_id platform = NULL;
  if( CL_SUCCESS != oclGetPlatformID(&platform) )
    return -1;

  // Get device count.
  cl_uint device_count;
  if( CL_SUCCESS != clGetDeviceIDs( platform, CL_DEVICE_TYPE_GPU, 0, NULL, &device_count ) )
    return -1;

  // Get device IDs.
  cl_device_id* device_ids = new cl_device_id[ device_count ];
  if( CL_SUCCESS != clGetDeviceIDs( platform, CL_DEVICE_TYPE_GPU, device_count, device_ids, &device_count ) )
    return -2;

  // Create context.
  if( (cl_context)0 == ( context = clCreateContext(0, device_count, device_ids, NULL, NULL, &err) ) )
    return -3;
  if( CL_SUCCESS != err )
    return -5;

  #if OPENCL_TRACE
  //std::cout << "Created context: " << context << "\n";
  #endif

  std::cout << "GPU!!!!! device = " << device_count << std::endl;

  // Get device-specific information.
  for( cl_uint i = 0; i < device_count; ++i )
    {
      devices.push_back( OpenCLDevice( device_ids[i] ) );
      #if OPENCL_TRACE
      std::cout << "Loading device ID: " << device_ids[i] << "\n";
      #endif
      OpenCLDevice* dev_info = &devices.back( );

      // Name
      clGetDeviceInfo( device_ids[i], CL_DEVICE_NAME, sizeof(buf), &buf, NULL );
      dev_info->name = std::string( buf );
      clGetDeviceInfo( device_ids[i], CL_DEVICE_VENDOR, sizeof(buf), &buf, NULL );
      dev_info->vendor = std::string( buf );

      // Compute units & frequency
      clGetDeviceInfo( device_ids[i], CL_DEVICE_MAX_COMPUTE_UNITS,
		       sizeof(dev_info->max_compute_units), &dev_info->max_compute_units, NULL );
      clGetDeviceInfo( device_ids[i], CL_DEVICE_MAX_CLOCK_FREQUENCY,
		       sizeof(dev_info->max_clock_freq), &dev_info->max_clock_freq, NULL );

      // TODO(mangpo): do we need this?
      // Work-item and work-group properties
      /*std::cout << "work-item" << std::endl;
      clGetDeviceInfo( device_ids[1], CL_DEVICE_MAX_WORK_ITEM_SIZES,
		       sizeof(dev_info->max_workitem_size), &dev_info->max_workitem_size, NULL );
      std::cout << "work-group" << std::endl;
      clGetDeviceInfo( device_ids[i], CL_DEVICE_MAX_WORK_GROUP_SIZE,
		       sizeof(dev_info->max_workgroup_size), &dev_info->max_workgroup_size, NULL );*/

      // Memory properties
      clGetDeviceInfo( device_ids[i], CL_DEVICE_GLOBAL_MEM_SIZE,
		       sizeof(dev_info->global_mem_size), &dev_info->global_mem_size, NULL );
      clGetDeviceInfo( device_ids[i], CL_DEVICE_LOCAL_MEM_SIZE,
		       sizeof(dev_info->local_mem_size), &dev_info->local_mem_size, NULL );

      // Queue properties
      cl_command_queue_properties queue_props;
      clGetDeviceInfo( device_ids[i], CL_DEVICE_QUEUE_PROPERTIES,
		       sizeof(queue_props), &queue_props, NULL );
      dev_info->has_queue_outoforder_exec = queue_props & CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE;
      dev_info->has_queue_profiling = queue_props & CL_QUEUE_PROFILING_ENABLE;

      // Create queue
      if( (cl_command_queue)0 == ( dev_info->queue =
				clCreateCommandQueue( context, device_ids[i], 0, &err ) ) )
	      return -4;
      if( CL_SUCCESS != err )
	      return -6;

      #if OPENCL_TRACE
      std::cout << "Created command queue: " << dev_info->queue << "\n";
      #endif
    }

  // Clean up.
  delete[] device_ids;

  has_init = true;
  #if OPENCL_TRACE
  std::cout << "OpenCLUtil::init() finishes...\n";
  #endif
  return 0;
}

void
OpenCLUtil::deinit( )
{
  // Release command queues.
  for( std::vector<OpenCLDevice>::iterator it = devices.begin( ); it != devices.end( ); ++it )
    clReleaseCommandQueue( (*it).queue );

  // Release context.
  clReleaseContext( context );
}

cl_context
OpenCLUtil::getContext( )
{
  if( false == has_init )
    init( );

  return context;
}

cl_command_queue
OpenCLUtil::getQueue( unsigned int dev_idx )
{
  if( false == has_init )
    init( );

  return devices.at( dev_idx ).queue;
}

void
OpenCLUtil::printDeviceList( bool verbose )
{
  if( false == has_init )
    init( );

  std::cout << "A total of " << devices.size( ) << " OpenCL device(s) have been enumerated.\n";

  for( std::vector<OpenCLDevice>::iterator it = devices.begin( ); it != devices.end( ); ++it )
    printDeviceDetails( *it, verbose );
}

void
OpenCLUtil::printDeviceDetails( const OpenCLDevice& dev_info, bool verbose )
{
  if( dev_info.enabled )
    std::cout << "\t+ ";
  else
    std::cout << "\t- ";
  std::cout << dev_info.name;
  
  if( true == verbose )
    {
      std::cout << " (" << dev_info.vendor << ")\n";
      std::cout << "\t\tCompute units (max):\t\t" << dev_info.max_compute_units << "\n";
      std::cout << "\t\tClock freq. (max) (MHz):\t" << dev_info.max_clock_freq << "\n";
      std::cout << "\t\tMaximum work-item size(s):\t(" << dev_info.max_workitem_size[0] << ","
		<< dev_info.max_workitem_size[1] << "," << dev_info.max_workitem_size[2] << ")\n";
      std::cout << "\t\tMaximum work-group size:\t" << dev_info.max_workgroup_size << "\n";
      std::cout << "\t\tLocal memory (KiB):\t\t" << ( dev_info.local_mem_size / 1024 ) << "\n";
      std::cout << "\t\tGlobal memory (MiB):\t\t" << ( dev_info.global_mem_size / (1024*1024) ) << "\n";
      std::cout << "\t\tSupports queue profiling?\t"
		<< ( dev_info.has_queue_profiling ? "Yes" : "No" ) << "\n";
      std::cout << "\t\tSupports out-of-order exec.?\t"
		<< ( dev_info.has_queue_outoforder_exec ? "Yes" : "No" ) << "\n";
    }
  else
    std::cout << "\n";
}

void
OpenCLUtil::printDeviceDetails( unsigned int dev_idx, bool verbose )
{
  if( false == has_init )
    {
      std::cout << "OpenCL has not been initialized.\n";
      return;
    }

  if( dev_idx >= devices.size( ) )
    {
      std::cout << "Device index is out-of-range in printDeviceDetails().\n";
      return;
    }

  printDeviceDetails( devices.at( dev_idx ), verbose );
}

cl_int
OpenCLUtil::buildProgram( cl_program &program )
{
  #if OPENCL_TRACE
  std::cout << "OpenCLUtil::buildProgram() begins...\n";
  #endif

  cl_int err, build_err;
  cl_device_id device_id = getActiveDeviceID( );
  build_err = clBuildProgram( program, 1, &device_id, NULL, NULL, NULL );

  // If everything went to plan, we're done.
  if( CL_SUCCESS == build_err )
    return CL_SUCCESS;

  // Otherwise, report the error and try to get more detailed information.
  std::cout << "Error while building program (" << build_err << "): " << OpenCLUtil::errorString( build_err ) << std::endl;

  // If the program failed to build, get more detailed information about what went wrong.
  //  cl_build_status status;
  size_t log_size;
  err = clGetProgramBuildInfo( program, device_id, CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size );
  JASSERT( CL_SUCCESS == err ).Text( "Failed to get build log size." );
  char* build_log = new char[log_size+1];
  err = clGetProgramBuildInfo( program, device_id, CL_PROGRAM_BUILD_LOG, log_size, build_log, NULL );
  JASSERT( CL_SUCCESS == err ).Text( "Failed to get build log." );
  build_log[log_size] = '\0'; // Add null terminator.  /** \todo is this necessary? */

  std::cerr << "OpenCL kernel failed to build.  Build log:\n" << build_log << std::endl;

  return build_err;
}

std::string
OpenCLUtil::errorString( cl_int error )
{
  switch( error )
    {
    case CL_SUCCESS:
      return "Success!";
    case CL_DEVICE_NOT_FOUND:
      return "Device not found.";
    case CL_DEVICE_NOT_AVAILABLE:
      return "Device not available.";
    case CL_COMPILER_NOT_AVAILABLE:
      return "Compiler not available.";
    case CL_MEM_OBJECT_ALLOCATION_FAILURE:
      return "Memory object allocation failure.";
    case CL_OUT_OF_RESOURCES:
      return "Out of resources.";
    case CL_OUT_OF_HOST_MEMORY:
      return "Out of host memory.";
    case CL_PROFILING_INFO_NOT_AVAILABLE:
      return "Profiling information not available.";
    case CL_MEM_COPY_OVERLAP:
      return "Memory copy overlap.";
    case CL_IMAGE_FORMAT_MISMATCH:
      return "Image format mismatch.";
    case CL_IMAGE_FORMAT_NOT_SUPPORTED:
      return "Image format not supported.";
    case CL_BUILD_PROGRAM_FAILURE:
      return "Build program failure.";
    case CL_MAP_FAILURE:
      return "Map failure.";
    case CL_INVALID_VALUE:
      return "Invalid value.";

    case CL_INVALID_KERNEL_NAME:
      return "Invalid kernel name.";
    case CL_INVALID_KERNEL_DEFINITION:
      return "Invalid kernel definition.";

    case CL_INVALID_PROGRAM:
      return "Invalid program.";
    case CL_INVALID_DEVICE:
      return "Invalid device.";
    case CL_INVALID_BINARY:
      return "Invalid binary.";
    case CL_INVALID_BUILD_OPTIONS:
      return "Invalid build options.";
    case CL_INVALID_OPERATION:
      return "Invalid operation.";

    case CL_INVALID_EVENT_WAIT_LIST:
      return "Invalid event wait list.";

    case 1: // Nonstandard error sometimes returned by beta NVIDIA SDL
      return "Nonstandard error; check that symbols are defined and that you're not using double datatype where unsupported.";

    default:
      return "[Unknown error.]";
    }
}

OpenCLDevice::OpenCLDevice( cl_device_id _id )
  : id( _id ), enabled( true )
{
  // intentionally blank
}

void
OpenCLUtil::setActiveDevice( unsigned int dev_idx )
{
  active_device = dev_idx;
}

unsigned int
OpenCLUtil::getActiveDevice( )
{
  return active_device;
}

cl_device_id
OpenCLUtil::getActiveDeviceID( )
{
  return devices.at( active_device ).id;
}

}

#endif
