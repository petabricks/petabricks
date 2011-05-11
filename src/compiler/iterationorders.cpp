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
//#define DEBUG

#include "iterationorders.h"

#include "codegenerator.h"
#include "maximawrapper.h"
#include "region.h"
#include "rule.h"

#include "common/jconvert.h"

#include <algorithm>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

namespace petabricks {

/**
 * Helper class used by IterationDefinition::genSplitCode
 */
class SplitRegion : public SimpleRegion {
public:
  void reserve(size_t n) {
    minCoord().reserve(n);
    maxCoord().reserve(n);
    _isFirst.reserve(n);
  }
  void push_back(const FormulaPtr& b,const FormulaPtr& e, bool f){
    minCoord().push_back(b);
    maxCoord().push_back(e);
    _isFirst.push_back(f);
  }
  void pop_back(){
    minCoord().pop_back();
    maxCoord().pop_back();
    _isFirst.pop_back();
  }
  bool isFirst(int d) const { return _isFirst[d]; }
private:
  std::vector<bool> _isFirst;
};

struct SplitRegionCmp {
  DependencyDirection _order;
  SplitRegionCmp(const DependencyDirection& order) : _order(order) {}

  //sort regions be the order specified by the given dependency direction
  bool operator() (const SplitRegion& a, const SplitRegion& b) const {
    JASSERT(a.size()==b.size());
    for(size_t i=0; i<a.size(); ++i){
      if(a.isFirst(i) != b.isFirst(i)){
        if(_order.canIterateForward(i) || !_order.canIterateBackward(i)){
          return a.isFirst(i);//dimension ordered forward 
        }else{
          return b.isFirst(i);//dimension ordered backward
        }
      }
    }
    return false;//equal
  }
};

}

petabricks::IterationDefinition::IterationDefinition(RuleInterface& rule, const DependencyDirection& order, bool isSingleCall)
  : _order(order)
{
  for(size_t i=0; i<_order.size(); ++i){
    _var.push_back(rule.getOffsetVar(i));
    _begin.push_back(rule.getOffsetVar(i,"begin"));
    _end.push_back(rule.getOffsetVar(i,  "end"));
    _step.push_back(rule.getSizeOfRuleIn(i));
  }
  if(isSingleCall){
    _step.clear();
  }
}

void petabricks::IterationDefinition::genLoopBegin(CodeGenerator& o){
  if(isSingleCall()){
    o.write("{");
    for(size_t i=0; i<_var.size(); ++i){
      o.varDecl("const IndexT "+_var[i]->toString()+" = "+_begin[i]->toString());
    }
  }else{
    for(size_t i=0; i<_var.size(); ++i){
      FormulaPtr b=_begin[i];
      FormulaPtr e=_end[i];
      FormulaPtr s=_step[i];
      FormulaPtr v=_var[i];
      //TODO: expand to reorder dimensions
      if(_order.canIterateForward(i) || !_order.canIterateBackward(i)){
        JWARNING(_order.canIterateForward(i))(_order).Text("couldn't find valid iteration order, assuming forward");
        o.beginFor(v->toString(), b, e, s);
      } else {
        o.beginReverseFor(v->toString(), b, e, s);
      }
    }
  }
}

void petabricks::IterationDefinition::genLoopEnd(CodeGenerator& o){
  if(isSingleCall()){
    o.write("}");
  }else{
    for(size_t i=0; i<_var.size(); ++i){
      o.endFor();
    }
  }
}

std::vector<std::string> petabricks::IterationDefinition::args() const {
  std::vector<std::string> rv;
  CoordinateFormula::const_iterator i;
  for(i=_begin.begin(); i!=_begin.end(); ++i) 
    rv.push_back("const IndexT "+(*i)->toString());
  for(i=_end.begin(); i!=_end.end(); ++i) 
    rv.push_back("const IndexT "+(*i)->toString());
  return rv;
}

std::vector<std::string> petabricks::IterationDefinition::argnames() const {
  std::vector<std::string> rv;
  CoordinateFormula::const_iterator i;
  for(i=_begin.begin(); i!=_begin.end(); ++i) 
    rv.push_back((*i)->toString());
  for(i=_end.begin(); i!=_end.end(); ++i) 
    rv.push_back((*i)->toString());
  return rv;
}

std::vector<std::string> petabricks::IterationDefinition::packedargs() const {
  std::string t[] = {
    "IndexT "COORD_BEGIN_STR"["+jalib::XToString(dimensions())+"]",
    "IndexT "COORD_END_STR"["+jalib::XToString(dimensions())+"]"
  };
  return std::vector<std::string>(t, t+2);
}
std::vector<std::string> petabricks::IterationDefinition::packedargnames() const {
  static std::string t[] = {
    COORD_BEGIN_STR,
    COORD_END_STR
  };
  return std::vector<std::string>(t, t+2);
}
void petabricks::IterationDefinition::unpackargs(CodeGenerator& o) const {
  for(size_t i=0; i<_var.size(); ++i){
    o.write("const IndexT "+_begin[i]->toString()+" = "COORD_BEGIN_STR"["+jalib::XToString(i)+"];");
    o.write("const IndexT "+_end[i]->toString()+" = "COORD_END_STR"["+jalib::XToString(i)+"];");
  }
}


void petabricks::IterationDefinition::genSplitCode(CodeGenerator& o, Transform& trans, RuleInterface& rule, bool isStatic) const {
  //create list of subregion
  SplitRegionList regions;
  SplitRegion seed;
  seed.reserve(dimensions());
  fillSplitRegionList(regions, seed);

  //order them correctly
  std::sort(regions.begin(), regions.end(), SplitRegionCmp(_order));
 
  //generate code
  if(!isStatic){ 
    o.write("GroupedDynamicTask<"+jalib::XToString(1<<dimensions())+">* _split_task "
                   "= new GroupedDynamicTask<"+jalib::XToString(1<<dimensions())+">();");
  }

  for(size_t a=0; a<regions.size(); ++a){
    SimpleRegionPtr r= new SimpleRegion(regions[a]);
    if(!isStatic){
      rule.generateCallCode("(*_split_task)["+jalib::XToString(a)+"]", trans, o, r, E_RF_DYNAMIC);
      for(size_t b=0; b<a; ++b){
        if(canDependOn(regions[a], regions[b])){
          JTRACE("adding dep")(regions[a])(regions[b]);
          o.write("(*_split_task)["+jalib::XToString(a)+"]->dependsOn((*_split_task)["+jalib::XToString(b)+"]);");
        }
      }
    }else{
      rule.generateCallCode("", trans, o, r, E_RF_STATIC);
    }
  }
  if(!isStatic) o.write("return petabricks::run_task(_split_task);");
  else          o.write("return NULL;");
}

void petabricks::IterationDefinition::fillSplitRegionList(SplitRegionList& regions, SplitRegion& r) const {
  int d = r.dimensions();
  if(d<dimensions()){
    FormulaPtr begin = _begin[d];
    FormulaPtr mid = new FormulaDivide( new FormulaAdd( _begin[d], _end[d] ), FormulaInteger::two());
    FormulaPtr end = _end[d];
    r.push_back(begin, mid, true);
    fillSplitRegionList(regions, r);
    r.pop_back();
    r.push_back(mid, end, false);
    fillSplitRegionList(regions, r);
    r.pop_back();
  }else{
    regions.push_back(r);
  }
}

bool petabricks::IterationDefinition::canDependOn(const SplitRegion& a, const SplitRegion& b) const {
  JASSERT(a.size()==b.size());
  JASSERT(a.size()==_order.size());
  for(size_t d=0; d<_order.size(); ++d){
    int mask = 0;
    if(a.isFirst(d) && b.isFirst(d)){
      //mask = DependencyDirection::D_LE;
      continue;
    }else if(!a.isFirst(d) && !b.isFirst(d)){
      //mask = DependencyDirection::D_GE;
      continue;
    }else if(a.isFirst(d) && !b.isFirst(d)){
      mask = DependencyDirection::D_GT;
    }else if(!a.isFirst(d) && b.isFirst(d)){
      mask = DependencyDirection::D_LT;
    }

    if( (_order[d] & mask) == 0)
      return false;
  }
  return true;
}

