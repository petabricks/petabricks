/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef PETABRICKSTRAININGDEPS_H
#define PETABRICKSTRAININGDEPS_H

#include <sstream>
#include <map>
#include <vector>
#include "jassert.h"

namespace petabricks {

class TrainingDeps{
public:
  void addTunable( bool isTunable
                 , const std::string& category
                 , const std::string& name
                 , int initial
                 , int min
                 , int max)
  {
    if(isTunable)
      _os<< "    <tunable";
    else
      _os<< "    <config";

    _os << " name=\""    << name << "\""
        << " type=\""    << category << "\""
        << " initial=\"" << initial << "\""
        << " min=\""     << min << "\""
        << " max=\""     << max << "\""
        << " />\n";
  }

  void beginTransform(const std::string& name, const std::string& instanceName){
    _os << "  <transform ";
    _os << " name=\""         << instanceName << "\"";
    _os << " templateName=\"" << name << "\"";
    _os << " isTemplateInstance=\"" << (name==instanceName ? "no" : "yes") << "\"";
    _os << ">\n";
  }
  void endTransform(const std::string& name, const std::string& instanceName){
    const std::vector<std::string>& calls = _callgraph[name];
    for(size_t i=0; i<calls.size(); ++i)
      _os << "    <calls caller=\"" << name << "\" callee=\"" << calls[i] << "\" />\n";
    _os << "  </transform>\n"; 
  }



  static void addCallgraphEdge(const std::string& caller, const std::string& callee){
    _callgraph[caller].push_back(callee);
  }


  void dumpTo(std::ostream& o){
    o << "<traininginfo>\n";
    o << _os.str();
    o << "</traininginfo>\n";
  }
private:
  std::stringstream _os;
  static std::map<std::string, std::vector<std::string> > _callgraph;
};

}

#endif
