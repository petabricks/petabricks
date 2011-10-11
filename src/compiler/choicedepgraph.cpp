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
#include "choicedepgraph.h"

#include "codegenerator.h"
#include "transform.h"


void petabricks::ChoiceDepGraphNodeSet::applyRemapping(const petabricks::ChoiceDepGraphNodeRemapping& map){
  if(map.empty())
    return;
  for(ChoiceDepGraphNodeRemapping::const_iterator i=map.begin(); i!=map.end(); ++i){
    ChoiceDepGraphNodeSet::iterator rslt = find(i->first);
    if(rslt!=end()){
      erase(rslt);
      insert(i->second);
    }
  }
}

void petabricks::ScheduleDependencies::applyRemapping(const petabricks::ChoiceDepGraphNodeRemapping& map){
  if(map.empty())
    return;
  ScheduleDependencies orig;
  ScheduleDependencies::iterator i;
  orig.swap(*this);
  for(i=orig.begin();i!=orig.end();++i){
    if(map.find(i->first)==map.end()){
      (*this)[i->first]=i->second;
    }else{
      (*this)[map.find(i->first)->second]=i->second;
    }
  }
}


///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


petabricks::ChoiceDepGraphNode::ChoiceDepGraphNode()
  : _isInput(false)
  , _isOutput(false)
  , _isLast(false)
  , _choiceId(-1)
{
  static jalib::AtomicT i=0;
  _id=jalib::atomicIncrementReturn(&i);
}

std::string petabricks::ChoiceDepGraphNode::getChoicePrefix(Transform& t){
  if(_choiceId<0) _choiceId = t.nextTunerId();
  return t.name() + "_" + jalib::XToString(_choiceId) + "_";
}

int petabricks::ChoiceDepGraphNode::updateIndirectDepends(){
  int c = 0;
  if(_indirectDepends.empty()){  // seed first iteration
    _indirectDepends = directDepends();
    c+=_indirectDepends.size();
  }
  ScheduleDependencies tmp = _indirectDepends;
  for(ScheduleDependencies::iterator i=tmp.begin(); i!=tmp.end(); ++i){
    const ScheduleDependencies& remote = i->first->_indirectDepends;
    for( ScheduleDependencies::const_iterator dep=remote.begin(); dep!=remote.end(); ++dep)
    { //for each dependency
      if(_indirectDepends[dep->first].merge(dep->second))
        ++c;
    }
  }
  return c;
}

void petabricks::ChoiceDepGraphNode::applyRemapping(const ChoiceDepGraphNodeRemapping& map){
  if(_directDependsRemapped.empty()){
    _directDependsRemapped  = _directDependsOriginal;
  }
  _directDependsRemapped.applyRemapping(map);
  _indirectDepends.applyRemapping(map);
}

void petabricks::ChoiceDepGraphNode::resetRemapping(){
  _directDependsRemapped.clear();
  _indirectDepends.clear();
}

void petabricks::ChoiceDepGraphNode::printNode(std::ostream& os) const{
  os << "  " << nodename() << "[label=\"" << nodename() << ": " 
     << *this
     << "\"];\n";
}

void petabricks::ChoiceDepGraphNode::printEdges(std::ostream& os) const{
  const ScheduleDependencies& deps = directDepends();
  for(ScheduleDependencies::const_iterator i=deps.begin(); i!=deps.end(); ++i){
    os << "  " << i->first->nodename() << " -> " << nodename() << "[ label=\"" << i->second.direction << "\"";
    if(directDepends().find(i->first)==directDepends().end()) os << ", style=dashed";
    os << "];\n";
  }
}

petabricks::ChoiceDepGraphNodeSet petabricks::ChoiceDepGraphNode::getStronglyConnectedComponent(){
  /// compute strongly connected component
  ChoiceDepGraphNodeSet s;
  s.insert(this);
  if(indirectDepends().find(this)==indirectDepends().end())
    return s;

  for(ScheduleDependencies::iterator i=indirectDepends().begin(); i!=indirectDepends().end(); ++i){
    if(i->first->indirectDepends().find(this)!=i->first->indirectDepends().end()) //if in a cycle with this
      s.insert(i->first);
  }
  return s;
}

petabricks::ChoiceDepGraphNodeSet petabricks::ChoiceDepGraphNode::getMultioutputComponent(){
  /// compute strongly connected component
  ChoiceDepGraphNodeSet s;
  s.insert(this);
  for(ScheduleDependencies::iterator i=directDepends().begin(); i!=directDepends().end(); ++i){
    if(i->second.direction.isMultioutput()) {
      s.insert(i->first);
    }
  }
  return s;
}

void petabricks::ChoiceDepGraphNode::applyChoiceRemapping(const RuleChoiceAssignment& map) {
  JASSERT(_indirectDepends.empty());
  JASSERT(_directDependsRemapped.empty());
  ScheduleDependencies::iterator i;
  RulePtr choice = map.find(this)->second;
  for(i=_directDependsOriginal.begin();i!=_directDependsOriginal.end();++i){
    if(i->second.contains(choice)){
      _directDependsRemapped[i->first] = i->second;
    }
  }
  
}


///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
  
petabricks::BasicChoiceDepGraphNode::BasicChoiceDepGraphNode(const MatrixDefPtr& m, const SimpleRegionPtr& r, const ChoiceGridPtr& choices)
  : _matrix(m), _region(r), _choices(choices ? choices->rules() : RuleSet())
{}

void petabricks::BasicChoiceDepGraphNode::generateCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor,
                            const RuleChoiceAssignment& choice){
  JASSERT(choice.find(this)!=choice.end());
  RulePtr rule = choice.find(this)->second;
  rule->generateCallCode(nodename(), trans, o, _region, flavor, _regionNodesGroups, id(), _gpuCopyOut);
}


void petabricks::BasicChoiceDepGraphNode::generateCodeForSlice(Transform& trans, CodeGenerator& o, int d, const FormulaPtr& pos, RuleFlavor , const RuleChoiceAssignment& choice){
  JASSERT(choice.find(this)!=choice.end());
  RulePtr rule = choice.find(this)->second;
  
  CoordinateFormula min = _region->minCoord();
  CoordinateFormula max = _region->maxCoord();

  min[d] = pos;
  max[d] = pos->plusOne();

  SimpleRegionPtr t = new SimpleRegion(min,max);

  rule->generateCallCode(nodename(), trans, o, t, RuleFlavor::SEQUENTIAL, _regionNodesGroups, id(), _gpuCopyOut);
  //TODO deps for slice // dynamic version
}

void petabricks::BasicChoiceDepGraphNode::removeDimensionFromRegions(
                                                          MatrixDefPtr matrix, 
                                                          size_t dimension) {
  
  if(this->matrix() == matrix) {
    //Remove dimension from the region directly associated with the node 
    this->region()->removeDimension(dimension);
  }
  
  //Remove dimension from the regions manipulated by the rules of the node
  for(RuleSet::iterator i=_choices.begin(), e=_choices.end(); i!=e; ++i) {
    RulePtr rule = *i;
    rule->removeDimensionFromMatrix(matrix, dimension);
  }
}

void petabricks::BasicChoiceDepGraphNode::fixVersionedRegionsType() {
  //Fix the type of the region directly associated with the node
  //region()->fixTypeIfVersioned();
  
  //Fix the type for the regions manipulated by the rules of the node
  for(RuleSet::iterator i=_choices.begin(), e=_choices.end(); i!=e; ++i) {
    RulePtr rule = *i;
    rule->fixVersionedRegionsType();
  }
}

#ifdef HAVE_OPENCL
petabricks::RegionList petabricks::BasicChoiceDepGraphNode::getFromRegionOnCpu(const RuleChoiceAssignment& choice) const {
  RulePtr rule = choice.find(this)->second;
  if(!rule->isEnabledGpuRule()){
    return rule->getFromRegions();
  }
  else{
    return petabricks::RegionList();
  } 
}

int petabricks::BasicChoiceDepGraphNode::numOutMatrixOnGpu(const RuleChoiceAssignment& choice, MatrixDefPtr matrix){
  if(matrix->name().compare(_matrix->name()) == 0 && choice.find(this)->second->isEnabledGpuRule())
    return 1;
  else
    return 0;
}


bool petabricks::BasicChoiceDepGraphNode::hasOverlappingRegionOnGpu(const RuleChoiceAssignment& choice, RegionPtr region) {
	std::cout << "name: " << region->matrix()->name() << " , " << _matrix->name() << " : " << choice.find(this)->second->isEnabledGpuRule() << std::endl;
	/*if(region->matrix()->name().compare(_matrix->name()) == 0)
		std::cout << "intersect: " << region->hasIntersect(_region) << std::endl;*/
  return (region->matrix()->name().compare(_matrix->name()) == 0) && choice.find(this)->second->isEnabledGpuRule();
}
#endif

///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


petabricks::MetaChoiceDepGraphNode::MetaChoiceDepGraphNode(const ChoiceDepGraphNodeSet& set)
  : _originalNodes(set)
{
  for(ChoiceDepGraphNodeSet::const_iterator i=set.begin(); i!=set.end(); ++i){
    _directDependsOriginal.merge((*i)->directDependsOriginal());
    _directDependsRemapped.merge((*i)->directDependsRemapped());
    _indirectDepends.merge((*i)->indirectDepends());
    if((*i)->isInput()) markInput();
    if((*i)->isOutput()) markOutput();
  }
}

///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


bool petabricks::MultiOutputChoiceDepGraphNode::findValidSchedule(const RuleChoiceAssignment& choice){
  //just check to make sure the schedule is valid
  RulePtr rule = 0;
  for(ChoiceDepGraphNodeSet::const_iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
    JASSERT(choice.find(*i)!=choice.end());
    if(!rule) {
      rule = choice.find(*i)->second;
    }
    if(rule != choice.find(*i)->second) {
      JTRACE("rejecting schedule")(rule)(choice.find(*i)->second);
      return false;
    }
  }
  return true;
}

void petabricks::MultiOutputChoiceDepGraphNode::generateCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor, const RuleChoiceAssignment& choice){
//  const DependencyInformation& selfDep = indirectDepends()[this];
//  if(selfDep.direction.isNone()){

  o.comment("Dual outputs compacted "+nodename());
  RuleSet rules;
  MatrixDefList matrices;
  std::string region;
  RulePtr rule = 0;
  ChoiceDepGraphNode* first = 0;
  //test matching region extents in d
  for(ChoiceDepGraphNodeSet::const_iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
    JASSERT(choice.find(*i)!=choice.end());
    if(first==0) {
      region=(*i)->region()->toString();
      first=*i;
      rule = choice.find(*i)->second;
    } else if(first->region()->dimensions()<(*i)->region()->dimensions()) {
      first=*i;
      JASSERT(rule == choice.find(*i)->second)(rule)(choice.find(*i)->second)
        .Text("expected all output regions to be generated from same rule");
    }
    JWARNING(region==(*i)->region()->toString())(region)((*i)->region()->toString())
      .Text("to(...) regions of differing size not yet supported");
    //TODO: what are these for?
    RuleSet tmp = (*i)->choices();
    rules.insert(tmp.begin(), tmp.end());
    matrices.push_back((*i)->matrix());
  }
  JASSERT(choice.find(first)!=choice.end());
  rule->generateCallCode(nodename(), trans, o, first->region(), flavor, _regionNodesGroups, id(), _gpuCopyOut);
}

#ifdef HAVE_OPENCL
petabricks::RegionList petabricks::MultiOutputChoiceDepGraphNode::getFromRegionOnCpu(const RuleChoiceAssignment& choice) const {
  RulePtr rule = choice.find(*_originalNodes.begin())->second;
  if(!rule->isEnabledGpuRule()){
    return rule->getFromRegions();
  }
  else{
    return petabricks::RegionList();
  } 
}

int petabricks::MultiOutputChoiceDepGraphNode::numOutMatrixOnGpu(const RuleChoiceAssignment& choice, MatrixDefPtr matrix){
  RulePtr rule = choice.find(*_originalNodes.begin())->second;
  if(!rule->isEnabledGpuRule())
    return 0;
  int count = 0;
  for(ChoiceDepGraphNodeSet::const_iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
    count += (*i)->numOutMatrixOnGpu(choice,matrix);
  }
  return count;
}

bool petabricks::MultiOutputChoiceDepGraphNode::hasOverlappingRegionOnGpu(const RuleChoiceAssignment& choice, RegionPtr region) {
  RulePtr rule = choice.find(*_originalNodes.begin())->second;
  if(!rule->isEnabledGpuRule())
    return false;
  for(ChoiceDepGraphNodeSet::const_iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
    if((*i)->hasOverlappingRegionOnGpu(choice,region)) {
      return true;
    }
  }
  return false;
}
#endif

///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////

  
petabricks::SlicedChoiceDepGraphNode::SlicedChoiceDepGraphNode(const ChoiceDepGraphNodeSet& set)
  : MetaChoiceDepGraphNode(set), _dimension(-1), _forward(true)
{
}

bool petabricks::SlicedChoiceDepGraphNode::findValidSchedule(const RuleChoiceAssignment&){
  const DependencyInformation& selfDep = indirectDepends()[this];
  JTRACE("finding valid schedule")(selfDep.direction.size());
  for(int d=(int)selfDep.direction.size()-1; d>=0; --d){
    bool passed=true;
    _begin=0;
    _end=0;

    //test matching region extents in d
    for(ChoiceDepGraphNodeSet::const_iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
      if(!_begin) _begin = (*i)->region()->minCoord()[d];
      if(!_end)   _end   = (*i)->region()->maxCoord()[d];
      if(  _begin->toString() != (*i)->region()->minCoord()[d]->toString()
        || _end->toString()   != (*i)->region()->maxCoord()[d]->toString())
      {
        JTRACE("Can't sliceschedule due to mismatched extents")(d);
        passed=false;
        break;
      }
    }
    if(!passed) continue;
    
    //test direction
    if((selfDep.direction[d] & ~DependencyDirection::D_LT) == 0){
      JTRACE("slicescheduling forward")(d)(*this)(selfDep.direction[d]);
      _forward = true;
    }else if((selfDep.direction[d] & ~DependencyDirection::D_GT) == 0){
      JTRACE("Coscheduling backward")(d)(*this)(selfDep.direction[d]);
      _forward = false;
    }else{
      JTRACE("Can't sliceschedule due to mismatched direction")(d);
      continue;
    }

    _dimension=d;

    return true;
  }
  return false;
}

void petabricks::SlicedChoiceDepGraphNode::generateCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor, const RuleChoiceAssignment& choice){
  bool isStatic = (flavor==RuleFlavor::SEQUENTIAL);
  std::vector<std::string> args;
  args.push_back("const jalib::JRef<"+trans.instClassName()+"> transform");
  std::string taskname= "coscheduled_"+nodename()+"_task";
  CodeGenerator& ot = isStatic ? o : o.forkhelper();
  if(!isStatic) ot.beginFunc("DynamicTaskPtr", taskname);
  std::string varname="coscheduled_"+nodename();

  if(_forward){
    ot.beginFor(varname, _begin, _end, FormulaInteger::one());
  }else{
    ot.beginReverseFor(varname, _begin, _end, FormulaInteger::one());
  }    
  
  for(ChoiceDepGraphNodeSet::iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
    (*i)->generateCodeForSlice(trans, ot, _dimension, new FormulaVariable(varname), flavor, choice);
  }

  ot.endFor();
  if(!isStatic){
    ot.write("return NULL;");
    ot.endFunc();
    std::vector<std::string> args(1, "this");
    o.setcall(nodename(), "new petabricks::MethodCallTask<" + trans.instClassName()
                        + ", &" + trans.instClassName() + "::" + taskname + ">"
        , args);
  }
}

#ifdef HAVE_OPENCL
petabricks::RegionList petabricks::SlicedChoiceDepGraphNode::getFromRegionOnCpu(const RuleChoiceAssignment& choice) const {
  petabricks::RegionList regions;
  for(ChoiceDepGraphNodeSet::iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
    petabricks::RegionList list = (*i)->getFromRegionOnCpu(choice);
    regions.insert(regions.end(), list.begin(), list.end());
  }
  return regions;
}

int petabricks::SlicedChoiceDepGraphNode::numOutMatrixOnGpu(const RuleChoiceAssignment& choice, MatrixDefPtr matrix){
  int count = 0;
  for(ChoiceDepGraphNodeSet::iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
    count += (*i)->numOutMatrixOnGpu(choice, matrix);
  }
  return count;
}

bool petabricks::SlicedChoiceDepGraphNode::hasOverlappingRegionOnGpu(const RuleChoiceAssignment& choice, RegionPtr region) {
  for(ChoiceDepGraphNodeSet::iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
    if((*i)->hasOverlappingRegionOnGpu(choice, region)) {
      return true;
    }
  }
  return false;
}
#endif

void petabricks::ChoiceDepGraphNodeList::removeDimensionFromRegions(MatrixDefPtr matrix, size_t dimension) {
  for(ChoiceDepGraphNodeList::iterator i=begin(), e=end(); i!=e; ++i) {
    BasicChoiceDepGraphNode& basicChoiceDepGraphNode = (*i)->asBasicNode();
    
    basicChoiceDepGraphNode.removeDimensionFromRegions(matrix, dimension);
  }
}

void petabricks::ChoiceDepGraphNodeList::fixVersionedRegionsType() {
  for(ChoiceDepGraphNodeList::iterator i=begin(), e=end(); i!=e; ++i) {
    BasicChoiceDepGraphNode& basicChoiceDepGraphNode = (*i)->asBasicNode();
    
    basicChoiceDepGraphNode.fixVersionedRegionsType();
  }
}
