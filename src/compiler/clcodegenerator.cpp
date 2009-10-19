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
#ifdef HAVE_OPENCL

#include "clcodegenerator.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks
{

void
CLCodeGenerator::outputEscapedStringTo( std::ostream& o )
{
  std::string str = _os.str( );

  o << "\"";
  for( string::const_iterator it = str.begin( ); it != str.end( ); ++it )
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
  outputEscapedTo( ss );
  return ss.str( );
}

}

#endif
