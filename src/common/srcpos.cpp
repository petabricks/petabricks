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
#include "srcpos.h"

#include <algorithm>

namespace {

  bool cmpSrcPos(const jalib::SrcPosPtr& a, const jalib::SrcPosPtr& b){
    return a->first() < b->first();
  }

  void _compactSrcPosList(std::vector<jalib::SrcPosPtr>& v){
    std::vector<jalib::SrcPosPtr> old;
    old.swap(v);
    v.push_back(old.front());
    std::sort(v.begin(), v.end(), cmpSrcPos);
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

}//filelocal

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
  srcPos(os);
  return os.str();
}
void jalib::SrcPosTaggable::_lexicalSrcPos(std::ostream& o) const {
  if(!srcPos(o))
    ::_lexicalSrcPos(o);//go to global location guessing
}
void _lexicalSrcPos(std::ostream& o) {
  o << "<unknown-location>";
}


