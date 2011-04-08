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
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_OPENCL

#include "clcodegenerator.h"

namespace petabricks
{

void
CLCodeGenerator::outputStringTo( std::ostream& o )
{
  o << os().str( );
}

void
CLCodeGenerator::outputEscapedStringTo( std::ostream& o )
{
  std::string str = os().str();

  o << "\"";
  for( std::string::const_iterator it = str.begin( ); it != str.end( ); ++it )
      switch( *it )
	{
	case '\\':
	  o << "\\\\";
	  break;
	case '"':
	  o << "\\\"";
	  break;
	case '\n':
	  //o << "\\n";
	  o << "\\n\"\n\"";
	  break;
	case '\t':
	  o << "\\t";
	  break;
	default:
	  o << *it;
	  break;
	}
  o << "\"";
  return;
}

std::string
CLCodeGenerator::outputEscapedString( )
{
  std::stringstream ss;
  outputEscapedStringTo( ss );
  return ss.str( );
}

void
CLCodeGenerator::localMemoryBarrier( )
{
  os() << "barrier( CLK_LOCAL_MEM_FENCE );\n";
}

void
CLCodeGenerator::beginKernel( const std::vector<std::string>& outputs, const std::vector<std::string>& inputs, unsigned int dims )
{
  /*
    // \todo temporarily revised
  JASSERT( dims >= 1 );
  JASSERT( outputs.size( ) > 0 );
  JASSERT( inputs.size( ) > 0 );
  */

  os() << "__kernel void kernel_main( ";

  // The kernel will need a pointer to an appropriate chunk of each input and output matrix
  for( std::vector<std::string>::const_iterator it = outputs.begin( ); it != outputs.end( ); ++it )
  {
    if( it != outputs.begin( ) )
	  os() << ", ";
    os() << "__global " << STRINGIFY(MATRIX_ELEMENT_T) << "* _region_" << *it;
  }
  for( std::vector<std::string>::const_iterator it = inputs.begin( ); it != inputs.end( ); ++it )
    os() << ", __global " << STRINGIFY(MATRIX_ELEMENT_T) << "* _region_" << *it;

  // And we'll need to provide the size of the region that we want the kernel to operate on.  (This is where the 'center' of the rule will be.)
  for( int i = 0; i < (int)dims; ++i )
    os() << ", int dim_d" << i;

  // Finally, we need to provide some of the dimensions of each of the matrices we've passed in, so that we can calculate indices.
  for( std::vector<std::string>::const_iterator it = outputs.begin( ); it != outputs.end( ); ++it )
  {
    for( int i = 0; i < (int)dims-1; ++i )
	    os() << ", int dim_" << *it << "_d" << i;
  }
  for( std::vector<std::string>::const_iterator it = inputs.begin( ); it != inputs.end( ); ++it )
  {
    for( int i = 0; i < (int)dims-1; ++i )
	    os() << ", int dim_" << *it << "_d" << i;
  }

  os() << " ) {\n";
}

void
CLCodeGenerator::endKernel( )
{
  os() << "}\n";
}

}

#endif
