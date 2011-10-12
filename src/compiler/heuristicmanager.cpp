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
#include "heuristicmanager.h"

#include <fstream>
#include <boost/regex.hpp>

petabricks::HeuristicPtr& petabricks::HeuristicManager::getHeuristic(const std::string name) {
  //From cache
  HeuristicMap::iterator found=_heuristicCache.find(name);
  if (found != _heuristicCache.end()) {
    //The heuristic is already in the cache, just return it
    return found->second;
  }
  
  found = _fromFile.find(name);
  if(found != _fromFile.end()) {
    //Found! Store in cache and return
    _heuristicCache[name] = found->second;
    return found->second;
  }
  
  //Best from DB
  HeuristicPtr heuristic = _db.getBestHeuristic(name);
  if(heuristic) {
    //Found! Store in cache and return
    _heuristicCache[name] = heuristic;
    return _heuristicCache[name];
  }
  
  //Use default heuristic
  found = _defaultHeuristics.find(name);
    //Found! Store in cache and return
  if (found != _defaultHeuristics.end()) {
    _heuristicCache[name] = found->second;
    return found->second;
  }
  
  //Should never arrive here! Every heuristic should have a default
  JWARNING("Unable to find this heuristic. Does it have a default?")(name);
  abort();
}

void petabricks::HeuristicManager::loadFromFile(const std::string fileName) {
  //boost::regex heuristicRE("\\s*<heuristic\\s+name=\"(\\w+)\"\\s+formula=\"(\\w+)\"\\s*/>\\s*");
  boost::regex heuristicRE("<heuristic\\s+name=\"(.+)\"\\s+formula=\"(.+)\"\\s*/>");
  
  std::ifstream f(fileName.c_str());
  if (! f.is_open()) {
    std::cerr << "Unable to open the file containing the heuristics: " << fileName << "\n";
    abort();
  }
  
  std::string line;
  while ( f.good() ) {
    boost::cmatch submatch;
    getline (f, line);
    JTRACE("line")(line);
    bool found = boost::regex_search(line.c_str(), submatch, heuristicRE);
    
    if (found) {
      //Add to the list of heuristics
      std::string name = submatch[1];
      std::string formula = jalib::unescapeXML(submatch[2]);
      JTRACE("Matches")(submatch[0])(name)(formula);
      Heuristic* newHeuristic= new Heuristic(formula);
      
      _fromFile[name]=HeuristicPtr(newHeuristic);
    }
    
  }
  
  f.close();
}