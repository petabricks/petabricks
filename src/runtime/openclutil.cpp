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

#include "openclutil.h"
#include <iostream>

#if defined(HAVE_OPENCL)

namespace petabricks
{

bool OpenCLUtil::has_init = false;
std::vector<OpenCLDevice> OpenCLUtil::devices;
cl_context OpenCLUtil::context;

int
OpenCLUtil::init( )
{
  char buf[1024];

  if( true == has_init )
    return 0;

  // Create context.
  if( (cl_context)0 == ( context = clCreateContextFromType( 0, CL_DEVICE_TYPE_GPU, NULL, NULL, NULL ) ) )
    return -3;

  // Get device count.
  cl_uint device_count;
  if( CL_SUCCESS != clGetDeviceIDs( NULL, CL_DEVICE_TYPE_GPU, 0, NULL, &device_count ) )
    return -1;

  // Get device IDs.
  cl_device_id* device_ids = new cl_device_id[ device_count ];
  if( CL_SUCCESS != clGetDeviceIDs( NULL, CL_DEVICE_TYPE_GPU, device_count, device_ids, &device_count ) )
    return -2;

  // Get device-specific information.
  for( cl_uint i = 0; i < device_count; ++i )
    {
      devices.push_back( OpenCLDevice( device_ids[i] ) );
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

      // Work-item and work-group properties
      clGetDeviceInfo( device_ids[1], CL_DEVICE_MAX_WORK_ITEM_SIZES,
		       sizeof(dev_info->max_workitem_size), &dev_info->max_workitem_size, NULL );
      clGetDeviceInfo( device_ids[i], CL_DEVICE_MAX_WORK_GROUP_SIZE,
		       sizeof(dev_info->max_workgroup_size), &dev_info->max_workgroup_size, NULL );

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
				   clCreateCommandQueue( context, device_ids[i], 0, NULL ) ) )
	return -4;
    }

  // Clean up.
  delete[] device_ids;

  has_init = true;
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
  return context;
}

void
OpenCLUtil::printDeviceList( bool verbose )
{
  if( false == has_init )
    {
      std::cout << "OpenCL has not been initialized.\n";
      return;
    }

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

OpenCLDevice::OpenCLDevice( cl_device_id _id )
  : id( _id ), enabled( true )
{
  // intentionally blank
}

}

#endif
