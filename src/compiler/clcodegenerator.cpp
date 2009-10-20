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
CLCodeGenerator::outputEscapedStringTo( std::ostream& o )
{
  std::string str = _os.str( );

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
	  o << "\\\n";
	  break;
	case '\t':
	  o << "\\\t";
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
  _os << "barrier( CLK_LOCAL_MEM_FENCE );\n";
}

void
CLCodeGenerator::beginKernel( const std::vector<std::string>& outputs, const std::vector<std::string>& inputs, unsigned int dims )
{
  _os << "__kernel kernel_main( ) {\n";
}

void
CLCodeGenerator::endKernel( )
{
  _os << "}\n";
}

}

#endif
