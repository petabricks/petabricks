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
#include "srcpos.h"

#include "jassert.h"

#include <algorithm>

namespace {

  bool cmpFirstSrcPos(const jalib::SrcPosPtr& a, const jalib::SrcPosPtr& b){
    return a->first() < b->first();
  }
  bool cmpAutoTagged(const jalib::SrcPosPtr& a, const jalib::SrcPosPtr& b){
    return a->isAutoTagged() < b->isAutoTagged();
  }
  
  bool cmpNull(const jalib::SrcPosPtr& a, const jalib::SrcPosPtr& b){
    return a->isNull() < b->isNull();
  }

  void _compactSrcPosList(std::vector<jalib::SrcPosPtr>& v){
    if(v.size() <= 1)
      return;

    // drop null 
    std::sort(v.begin(), v.end(), cmpNull);
    while(!v.empty() && v.back()->isNull()){
      v.pop_back();
    }
    if(v.size() <= 1)
      return;
    JASSERT(!v.front()->isNull());

    // drop autotagged if real tags exist
    std::sort(v.begin(), v.end(), cmpAutoTagged);
    if(v.front()->isAutoTagged() != v.back()->isAutoTagged()) {
      JASSERT(v.back()->isAutoTagged());
      while(v.back()->isAutoTagged()){
        v.pop_back();
      }
      if(v.size() <= 1)
        return;
    }
    
    //join into one tag per file 
    std::sort(v.begin(), v.end(), cmpFirstSrcPos);
    std::vector<jalib::SrcPosPtr> old;
    old.swap(v);
    v.push_back(old.front());
    std::vector<jalib::SrcPosPtr>::const_iterator i;
    for(i=old.begin()+1; i!=old.end(); ++i){
      if(v.back()->first().sameFile((*i)->first())){
        if(v.back()->last()<(*i)->last()){
          v.back()=new jalib::SrcPos(v.back()->first(), (*i)->last());
        }
      }else{
        v.push_back(*i);
      }
    }
  }

  std::string stripfilename ( const std::string& str ) {
    const char* begin = str.c_str();
    const char* end = begin+str.length();
    const char* i;
    for(i=begin; i<end; ++i) {
      if ( *i == '/') {
        begin=i+1;
      }
    }
    while(begin<end && end[-1]=='"') --end;
    while(begin<end && begin[0]=='"') ++begin;
    return std::string(begin, end);
  }


}//filelocal

jalib::SrcPosStack& jalib::SrcPosStack::global() {
  static SrcPosStack inst;
  return inst;
}
  
jalib::SrcPosTaggable::SrcPosTaggable() {
  jalib::SrcPosStack& globalStack = jalib::SrcPosStack::global();
  if(!globalStack.empty()){
    tagPosition(globalStack.back()->clone(true));
  }
}

bool jalib::SrcPosTaggable::srcPos(std::ostream& os) const {
  if(!_positions.empty()){
    _compactSrcPosList(_positions);
    _positions.front()->print(os);
    for(size_t i=1; i<_positions.size(); ++i){
      os << ", ";
      _positions[i]->print(os);
    }
    return true;
  }else{
    return false;
  }
}



std::string jalib::SrcPosTaggable::srcPos() const {
  std::ostringstream os;
  if(srcPos(os))
    return os.str();
  else
    return "<unknown>";
}

jalib::SrcPosPtr jalib::SrcPosTaggable::srcPosFirst() const {
  _compactSrcPosList(_positions);
  if(!_positions.empty())
    return _positions.front();
  else
    return 0;
}
  
void jalib::SrcPos::print(std::ostream& o) const {
  o << stripfilename(_first.filename);
  if(_first.filename != _last.filename)
    o << "/" << stripfilename(_last.filename);
  if(_first.line>=0)
    o << ":" << _first.line;
  if(_last.line>=0 && _last.line!=_first.line){
    o << "-" << _last.line;
  } else {
    if(_first.column >= 0)
      o << " (col " << _first.column << "-" << _last.column << ")";
  }
}


const jalib::SrcPosTaggable* _lexicalSrcPos() {
  jalib::SrcPosStack& globalStack = jalib::SrcPosStack::global();
  if(globalStack.empty()){
    return NULL;
  }else{
    // Guess position based on the call stack
    static jalib::SrcPosTaggable theStackTagged;
    theStackTagged.clearPosition();
    theStackTagged.tagPosition(globalStack.back()->clone(true));
    return &theStackTagged;
  }
}


