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
#include "trainingdeps.h"

std::map<std::string, std::vector<std::string> > petabricks::TrainingDeps::_callgraph;

namespace petabricks {

void TrainingDeps::emitRules(std::string& choicename,
                             const std::vector<RulePtr>& sortedRules) {
  _os << "    <rules choicename=\"" << choicename << "\">\n";
  int index = 0;
  for (std::vector<RulePtr>::const_iterator i = sortedRules.begin(),
       e = sortedRules.end(); i != e; ++i) {
    _os << "      <rule index=\"" << index << "\" label=\"" << (*i)->getLabel()
        << "\" />\n";
    ++index;
  }
  _os << "    </rules>\n";
}

}
