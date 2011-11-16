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
#include "common/hash.h"
#include "common/jassert.h"
#include "common/jconvert.h"
#include "common/jfilesystem.h"
#include "common/openclutil.h"

#include <sys/stat.h>
#include <sys/types.h>

#include <iostream>
#include <stdio.h>

namespace petabricks
{

OpenCLDevice::OpenCLDevice( cl_device_id _id )
  : id( _id ), enabled( true )
{
}


#ifndef HAVE_OPENCL

cl_platform_id OpenCLUtil::getPlatform( )
{
  UNIMPLEMENTED();
  return 0;
}

int
OpenCLUtil::init( )
{
  UNIMPLEMENTED();
  return 0;
}

void
OpenCLUtil::deinit( )
{
  UNIMPLEMENTED();
}

cl_context
OpenCLUtil::getContext( )
{
  UNIMPLEMENTED();
  return 0;
}

cl_command_queue
OpenCLUtil::getQueue( unsigned int  )
{
  UNIMPLEMENTED();
  return 0;
}

cl_int
OpenCLUtil::buildProgram( cl_program & )
{
  UNIMPLEMENTED();
  return 0;
}

std::string
OpenCLUtil::errorString( cl_int  )
{
  return "";
}

void
OpenCLUtil::setActiveDevice( unsigned int  )
{
  UNIMPLEMENTED();
}

bool OpenCLUtil::buildKernel(cl_program& , cl_kernel& , const char* ) {
  UNIMPLEMENTED();
  return false;
}

}


#else
//#define OPENCL_TRACE 1


bool OpenCLUtil::has_init = false;
std::vector<OpenCLDevice> OpenCLUtil::devices;
cl_context OpenCLUtil::context;
unsigned int OpenCLUtil::active_device = 0;


void OpenCLUtil::pfn_notify(const char *errinfo, const void* /*private_info*/, size_t /*cb*/, void* /*user_data*/){
#ifdef DEBUG
  std::cerr << "OpenCL Error via pfn_notify: " << errinfo << std::endl;
#endif
  //JASSERT(false).Text("OpenCL Error via pfn_notify.");
}

cl_platform_id OpenCLUtil::getPlatform( )
{
  cl_platform_id platform = NULL;
#ifdef NVIDIA
  // Get platform.
  if( CL_SUCCESS != oclGetPlatformID(&platform) )
    return NULL;
#else
  cl_uint numPlatforms;
  if( CL_SUCCESS != clGetPlatformIDs(0, NULL, &numPlatforms))
    return NULL;

  if (0 < numPlatforms) 
  {
    cl_platform_id* platforms = new cl_platform_id[numPlatforms];
    if(CL_SUCCESS != clGetPlatformIDs(numPlatforms, platforms, NULL))
      return NULL;
    for (int i = 0; i < numPlatforms; ++i) 
    {
      char pbuf[100];
      if(CL_SUCCESS != clGetPlatformInfo(platforms[i], CL_PLATFORM_VENDOR, sizeof(pbuf), pbuf, NULL))
        return NULL;

      platform = platforms[i];
      if (!strcmp(pbuf, "Advanced Micro Devices, Inc.")) 
        break;
    }
    delete[] platforms;
  }

#endif
  return platform;;
}

int
OpenCLUtil::init( )
{
  #if OPENCL_TRACE
  std::cerr << "OpenCLUtil::init() begins...\n";
  #endif

  int err;
  char buf[1024];

  if( true == has_init )
    return 0;

  cl_platform_id platform = getPlatform();

  if(platform == NULL)
    return -1;

  // Get device count.
  cl_uint device_count;
#ifdef MAC
  if( CL_SUCCESS != clGetDeviceIDs( platform, CL_DEVICE_TYPE_GPU, 0, NULL, &device_count ) )
    return -1;

  // Get device IDs.
  cl_device_id* device_ids = new cl_device_id[ device_count ];
  if( CL_SUCCESS != clGetDeviceIDs( platform, CL_DEVICE_TYPE_GPU, device_count, device_ids, &device_count ) )
    return -2;
#else
  if( CL_SUCCESS != clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, 0, NULL, &device_count ) )
    return -1;

  // Get device IDs.
  cl_device_id* device_ids = new cl_device_id[ device_count ];
  if( CL_SUCCESS != clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, device_count, device_ids, &device_count ) )
    return -2;
#endif

  // Create context.
  if( (cl_context)0 == ( context = clCreateContext(0, device_count, device_ids, &pfn_notify, NULL, &err) ) )
    return -3;
  if( CL_SUCCESS != err )
    return -5;

  #if OPENCL_TRACE
  std::cerr << "Created context: " << context << "\n";
  #endif

  // Get device-specific information.
  for( cl_uint i = 0; i < device_count; ++i )
    {
      devices.push_back( OpenCLDevice( device_ids[i] ) );
      #if OPENCL_TRACE
      std::cerr << "Loading device ID: " << device_ids[i] << "\n";
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

      // TODO(mangpo):
      // Work-item and work-group properties
      /*std::cerr << "work-item" << std::endl;
      clGetDeviceInfo( device_ids[1], CL_DEVICE_MAX_WORK_ITEM_SIZES,
		       sizeof(dev_info->max_workitem_size), &dev_info->max_workitem_size, NULL );
      std::cerr << "work-group" << std::endl;
      clGetDeviceInfo( device_ids[i], CL_DEVICE_MAX_WORK_GROUP_SIZE,
		       sizeof(dev_info->max_workgroup_size), &dev_info->max_workgroup_size, NULL );*/

      // Memory properties
      clGetDeviceInfo( device_ids[i], CL_DEVICE_GLOBAL_MEM_SIZE,
		       sizeof(dev_info->global_mem_size), &dev_info->global_mem_size, NULL );
      clGetDeviceInfo( device_ids[i], CL_DEVICE_LOCAL_MEM_SIZE,
		       sizeof(dev_info->local_mem_size), &dev_info->local_mem_size, NULL );

      //std::cout << "local mem size = " << dev_info->local_mem_size << std::endl;
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
      std::cerr << "Created command queue: " << dev_info->queue << "\n";
      #endif
    }

  // Clean up.
  delete[] device_ids;

  has_init = true;
  #if OPENCL_TRACE
  std::cerr << "OpenCLUtil::init() finishes...\n";
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
  #if OPENCL_TRACE
  std::cerr << "OpenCLUtil::deinit()\n";
  #endif
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

#define MAX_DEVICES 16
#define OCL_CACHE_DIR "/tmp/petabricks_ocl_cache"

namespace {

  int do_mkdir() {
    mode_t old = umask(0);
    int mdrv = mkdir(OCL_CACHE_DIR, 0777);
    if(mdrv != 0) {
      JASSERT(errno == EEXIST);
    }
    umask(old);
    return 0;
  }

  std::string srcToCacheFile(const char* src) {
    static int init = do_mkdir();
    JASSERT(init==0);

    jalib::HashGenerator hg;
    hg.update(src, strlen(src));
    std::ostringstream t;
    t << OCL_CACHE_DIR "/" << hg.final();
    return t.str();
  }

}


bool OpenCLUtil::buildKernel(cl_program& clprog, cl_kernel& clkern, const char* clsrc) {
  std::string cachefile = srcToCacheFile(clsrc);
  cl_int err;
  size_t num_devices = 0;
  size_t binSize[MAX_DEVICES];
  unsigned char* binary[MAX_DEVICES];
  memset(binary, 0, sizeof binary);
  memset(binSize, 0, sizeof binSize);

  // Source for kernel.
  cl_context ctx = OpenCLUtil::getContext();

#ifndef MAC
  if(jalib::Filesystem::FileExists(cachefile + "_0")) {
    cl_platform_id platform = getPlatform();
    JASSERT(platform != NULL);
    
    cl_uint device_count;
    JASSERT( CL_SUCCESS == clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, 0, NULL, &device_count ) ).Text("Failed to get device count");
   
    // Get device IDs.
    cl_device_id* device_ids = new cl_device_id[ device_count ];
    JASSERT( CL_SUCCESS == clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, device_count, device_ids, &device_count ) ).Text("Failed to get device IDs");

    num_devices = device_count;
    JASSERT(num_devices < MAX_DEVICES);

    for(int i=0; i<num_devices; ++i) { 
      FILE* binfile = fopen((cachefile+"_"+jalib::XToString(i)).c_str(), "rb");
      JASSERT(binfile!=NULL)(cachefile).Text("failed to open file");
      JASSERT(fread(&binSize[i], sizeof(size_t), 1, binfile)>0);
      binary[i] = new unsigned char[binSize[i]];
      JASSERT(fread(binary[i], sizeof(char), binSize[i], binfile)>0);
      fclose(binfile);
    }
  
    //JTRACE("loading cached opencl")(num_devices)(binSize[0])(binSize[1])(cachefile);
    const unsigned char** binary_c = (const unsigned char**)binary;

    cl_int binary_status, errcode_ret;
    clprog = clCreateProgramWithBinary( ctx, num_devices, device_ids, binSize, binary_c, &binary_status, &errcode_ret);
    //JASSERT( CL_SUCCESS == errcode_ret).Text( "Failed to create program." );

    err = clBuildProgram( clprog, 0, NULL, NULL, NULL, NULL);
    JASSERT( CL_SUCCESS == err ).Text( "Failed to build program." );

    clkern = clCreateKernel( clprog, "kernel_main", &err );
    JASSERT( CL_SUCCESS == err ).Text( "Failed to create kernel." );

    for(int i=0; i<num_devices; ++i) { 
      delete [] (binary[i]);
    }

    //JTRACE("cache hit");

    return true;
  }
#endif

  // Build program.
  clprog = clCreateProgramWithSource( ctx, 1, (const char **)&clsrc, NULL, &err );
  JASSERT( CL_SUCCESS == err ).Text( "Failed to create program." );

  err = clBuildProgram( clprog, 0, NULL, NULL, NULL, NULL);
  JASSERT( CL_SUCCESS == err ).Text( "Failed to build program." );

  // Create kernel.
  clkern = clCreateKernel( clprog, "kernel_main", &err );
  JASSERT( CL_SUCCESS == err ).Text( "Failed to create kernel." );

#ifndef MAC
  err = clGetProgramInfo(clprog, CL_PROGRAM_BINARY_SIZES, sizeof(binSize), binSize, &num_devices);
  JASSERT( CL_SUCCESS == err ).Text( "Failed to extract binary sizes." );
  JASSERT((num_devices % sizeof(size_t)) == 0);
  num_devices /= sizeof(size_t);
  JASSERT(num_devices < MAX_DEVICES);

  for(int i=0; i<num_devices; ++i) { 
    binary[i] = new unsigned char[binSize[i]];
  }
  err = clGetProgramInfo(clprog, CL_PROGRAM_BINARIES, sizeof binary, binary, NULL);
  JASSERT( CL_SUCCESS == err ).Text( "Failed to extract binaries." );
  

  JTRACE("creating cached opencl")(num_devices)(binSize[0])(binSize[1])(cachefile);

  for(int i=0; i<num_devices; ++i) { 
    FILE* binfile = fopen((cachefile+"_"+jalib::XToString(i)).c_str(), "wb");
    JASSERT(binfile!=NULL)(cachefile).Text("failed to open file");
    JASSERT(fwrite(&binSize[i], sizeof(size_t), 1, binfile)>0);
    JASSERT(fwrite(binary[i], sizeof(char), binSize[i], binfile)>0);
    fclose(binfile);
  }
  
  for(int i=0; i<num_devices; ++i) { 
    delete [] (binary[i]);
  }
#endif

  return true;
}

}

#endif
