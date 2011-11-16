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
#include "tinyxml.h"

petabricks::HeuristicPtr& petabricks::HeuristicManager::getDefaultHeuristic(const std::string name) {
  HeuristicMap::iterator found = _defaultHeuristics.find(name);
  //Found! Store in cache and return
  if (found != _defaultHeuristics.end()) {
    _heuristicCache[name] = found->second;
    return found->second;
  }
  
  //Should never arrive here! Every heuristic should have a default
  JWARNING("The heuristic does not have a default!")(name);
  abort();
}


petabricks::HeuristicPtr& petabricks::HeuristicManager::getHeuristic(const std::string name) {  
  if (_useDefaultHeuristics) {
    return getDefaultHeuristic(name);
  }
  
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
  return getDefaultHeuristic(name);
}

void petabricks::HeuristicManager::loadFromFile(const std::string fileName) {
  TiXmlDocument doc(fileName.c_str());
	doc.LoadFile();
  
  TiXmlHandle docHandle( &doc );
	TiXmlElement* heuristic = docHandle.FirstChildElement("heuristics").FirstChildElement("heuristic").ToElement();
	while (heuristic) {
    std::string name = heuristic->Attribute("name");
    std::string formula = heuristic->Attribute("formula");
    
    JTRACE("heuristic")(name)(formula);
    Heuristic* newHeuristic= new Heuristic(formula);
      
    _fromFile[name]=HeuristicPtr(newHeuristic);
    
    //Next  
    heuristic = heuristic->NextSiblingElement("heuristic");
  }
}