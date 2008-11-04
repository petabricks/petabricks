/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
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
#include "staticscheduler.h"
#include "transform.h"
#include "jasm.h"

namespace { //file local
void _remapSet(hecura::ScheduleNodeSet& set, const hecura::ScheduleNodeRemapping& map){
  using namespace hecura;
  for(ScheduleNodeRemapping::const_iterator i=map.begin(); i!=map.end(); ++i){
    ScheduleNodeSet::iterator rslt = set.find(i->first);
    if(rslt!=set.end()){
      set.erase(rslt);
      set.insert(i->second);
    }
  }
}
}


hecura::ScheduleNode::ScheduleNode()
  : _isInput(false)
{
  static long i=0;
  _id=jalib::atomicAdd<1>(&i);
}


hecura::StaticScheduler::StaticScheduler(const ChoiceGridMap& cg){
  for(ChoiceGridMap::const_iterator m=cg.begin(); m!=cg.end(); ++m){
    ScheduleNodeList& regions = _matrixToNodes[m->first];
    for(ChoiceGridIndex::const_iterator i=m->second.begin(); i!=m->second.end(); ++i){
      SimpleRegionPtr tmp = new SimpleRegion(i->first);
      regions.push_back(new UnischeduledNode(m->first, tmp, i->second));
      _allNodes.push_back(regions.back());
    }
  }
}

hecura::ScheduleNodeSet hecura::StaticScheduler::lookupNode(const MatrixDefPtr& matrix, const SimpleRegionPtr& region){
  ScheduleNodeSet rv;
  ScheduleNodeList& regions = _matrixToNodes[matrix];
  if(matrix->numDimensions()==0){
    JASSERT(regions.size()==1);
    rv.insert(regions.begin()->asPtr());
  }else{
    for(ScheduleNodeList::iterator i=regions.begin(); i!=regions.end(); ++i){
      if(region->toString() == (*i)->region()->toString()){
        rv.insert(i->asPtr());
        break; //optimization
      }
      if((*i)->region()->hasIntersect(region)){
        rv.insert(i->asPtr());
      }
    }
  }
  JASSERT(rv.size()>0)(matrix)(region).Text("failed to find rule for region");
  return rv;
}

void hecura::StaticScheduler::generateSchedule(){
  #ifdef DEBUG 
//   writeGraphAsPDF("schedule_initial.pdf");
  #endif

  computeIndirectDependencies();

  mergeCoscheduledNodes();

  #ifdef DEBUG 
  writeGraphAsPDF("schedule.pdf");
  #endif

  for( std::set<ScheduleNode*>::const_iterator i=_goals.begin()
      ; i!=_goals.end()
      ; ++i )
  {
    depthFirstSchedule(*i);
  }
}

void hecura::StaticScheduler::computeIndirectDependencies(){
  // this algorithm can be optimized, but since graphs are small it doesn't matter
  for(int c=1; c>0;){ //keep interating until no changes have been made
    c=0;
    for(ScheduleNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i)
      c+=(*i)->updateIndirectDepends();
    if(c>0) JTRACE("Updated indirectDepends")(c);
  }
}

void hecura::StaticScheduler::mergeCoscheduledNodes(){
  ScheduleNodeSet done;
  ScheduleNodeRemapping mapping;
  ScheduleNodeList tmp = _allNodes;
  for(ScheduleNodeList::iterator i=tmp.begin(); i!=tmp.end(); ++i){
    if(done.find(i->asPtr()) == done.end()){
      ScheduleNodeSet set=(*i)->getStronglyConnectedComponent();
      done.insert(set.begin(),set.end());
      if(set.size()>1){
        JTRACE("coscheduling nodes")((*i)->nodename())(set.size());
        ScheduleNode* coscheduled = new CoscheduledNode(set);
        _allNodes.push_back(coscheduled);
        for(ScheduleNodeSet::iterator e=set.begin();e!=set.end(); ++e)
          mapping[*e] = coscheduled;
      }
    }
  }
  applyRemapping(mapping);
}

void hecura::StaticScheduler::applyRemapping(const ScheduleNodeRemapping& m){
  for(ScheduleNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i){
      (*i)->applyRemapping(m);
  }
  _remapSet(_goals, m);
  _remapSet(_generated, m);
  _remapSet(_pending, m);
  ScheduleNodeList tmp;
  _allNodes.swap(tmp);
  _allNodes.reserve(tmp.size());
  for(ScheduleNodeList::iterator i=tmp.begin(); i!=tmp.end(); ++i){
    if(m.find(i->asPtr())==m.end())
      _allNodes.push_back(*i);
    else
      _remappedNodes.push_back(*i);
  }
}

void hecura::StaticScheduler::depthFirstSchedule(ScheduleNode* n){
  if(_generated.find(n)!=_generated.end())
    return;

  JASSERT(_pending.find(n)==_pending.end()).Text("dependency cycle");
  _pending.insert(n);

  for( ScheduleDependencies::const_iterator i=n->directDepends().begin()
      ; i!=n->directDepends().end()
      ; ++i)
  {
    if(i->first != n)
      depthFirstSchedule(i->first);
  }
//   JTRACE("scheduling")(n->matrix()); 
  _schedule.push_back(n);
  _generated.insert(n);
}

void hecura::StaticScheduler::generateCodeSimple(Transform& trans, CodeGenerator& o){
  JASSERT(_schedule.size()>0);
//   JTRACE("codegen")(_schedule.size());
  for(ScheduleNodeList::iterator i=_schedule.begin(); i!=_schedule.end(); ++i){
    (*i)->generateCodeSimple(trans, o);
  }
  o.write("DynamicTaskPtr "+trans.taskname()+" = new NullDynamicTask();");
  for(ScheduleNodeSet::iterator i=_goals.begin(); i!=_goals.end(); ++i)
    o.write(trans.taskname()+"->dependsOn(" + (*i)->nodename() + ".completionTask());");
}

void hecura::UnischeduledNode::generateCodeSimple(Transform& trans, CodeGenerator& o){
  RuleChoicePtr rule = trans.learner().makeRuleChoice(_choices->rules(), _matrix, _region);
  o.write("SpatialTaskList "+nodename()+";");
  rule->generateCodeSimple(nodename(), trans, *this, _region, o);
}

void hecura::ScheduleNode::printDepsAndEnqueue(CodeGenerator& o, const RulePtr& rule, bool useDirections){
  bool printedBeforeDep = false;

  ScheduleDependencies::const_iterator sd = _indirectDepends.find(this);
  if(sd==_indirectDepends.end()){
    o.write(nodename()+".spatialSplit("SPLIT_CHUNK_SIZE");");
  }else{
    //TODO split selfdep tasks too
  }

  for(ScheduleDependencies::const_iterator i=_directDepends.begin();  i!=_directDepends.end(); ++i){
    if(i->first!=this){
      if(i->first->isInput()){
        if(!printedBeforeDep){
         printedBeforeDep=true;
         if(useDirections)
           o.write(nodename()+".dependsOn(_before);");
         else
           o.write(nodename()+"->dependsOn(_before);");
        }
      }else{
        if(useDirections){
          o.write("{"); 
          o.incIndent();
          o.write("DependencyDirection::DirectionT _dir[] = {"+i->second.direction.toCodeStr()+"};");
          o.write(nodename()+".dependsOn<"+jalib::XToString(i->second.direction.size())+">"
                            "("+i->first->nodename()+", _dir);");
          o.decIndent();
          o.write("}"); 
        }else{
          o.write(nodename()+"->dependsOn("+i->first->nodename()+");");
        }
      }
    }
  }
  if(useDirections)
    o.write(nodename()+".enqueue();");
  else
    o.write(nodename()+"->enqueue();");
}

void hecura::UnischeduledNode::generateCodeForSlice(Transform& trans, CodeGenerator& o, int d, const FormulaPtr& pos){
  RuleChoicePtr rule = trans.learner().makeRuleChoice(_choices->rules(), _matrix, _region);
  
  CoordinateFormula min = _region->minCoord();
  CoordinateFormula max = _region->maxCoord();

  min[d] = pos;
  max[d] = pos->plusOne();

  SimpleRegionPtr t = new SimpleRegion(min,max);

  rule->generateCodeSimple("", trans, *this, t, o);
  //TODO deps for slice
}


void hecura::StaticScheduler::writeGraphAsPDF(const char* filename) const{
  std::string schedulerGraph = toString();
  FILE* fd = popen(("dot -Grankdir=LR -Tpdf -o "+std::string(filename)).c_str(), "w");
  fwrite(schedulerGraph.c_str(),1,schedulerGraph.length(),fd);
  pclose(fd);
}

int hecura::ScheduleNode::updateIndirectDepends(){
  int c = 0;
  if(_indirectDepends.empty()){  // seed first iteration
    _indirectDepends = _directDepends;
    c+=_indirectDepends.size();
  }
  ScheduleDependencies tmp = _indirectDepends;
  for(ScheduleDependencies::iterator i=tmp.begin(); i!=tmp.end(); ++i){
    const ScheduleDependencies& remote = i->first->indirectDepends();
    for( ScheduleDependencies::const_iterator dep=remote.begin(); dep!=remote.end(); ++dep)
    { //for each dependency
      if(_indirectDepends[dep->first].merge(dep->second))
        ++c;
    }
  }
  return c;
}

hecura::ScheduleNodeSet hecura::ScheduleNode::getStronglyConnectedComponent(){
  /// compute strongly connected component
  ScheduleNodeSet s;
  s.insert(this);
  if(_indirectDepends.find(this)==_indirectDepends.end())
    return s;

  for(ScheduleDependencies::iterator i=_indirectDepends.begin(); i!=_indirectDepends.end(); ++i){
    if(i->first->indirectDepends().find(this)!=i->first->indirectDepends().end()) //if in a cycle with this
      s.insert(i->first);
  }
  return s;
}

hecura::CoscheduledNode::CoscheduledNode(const ScheduleNodeSet& set)
  : _originalNodes(set)
{
  for(ScheduleNodeSet::const_iterator i=set.begin(); i!=set.end(); ++i){
    _directDepends.merge((*i)->directDepends());
    _indirectDepends.merge((*i)->indirectDepends());
  }
}


void hecura::CoscheduledNode::generateCodeSimple(Transform& trans, CodeGenerator& o){
  const DependencyInformation& selfDep = _indirectDepends[this];

  if(selfDep.direction.isNone()){
    o.write("SpatialTaskList "+nodename()+";");
    o.comment("Dual outputs compacted "+nodename());
    std::string region;
    //test matching region extents in d
    for(ScheduleNodeSet::const_iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
      if(region.empty()) region=(*i)->region()->toString();
      JWARNING(region==(*i)->region()->toString())(region)((*i)->region()->toString())
        .Text("to(...) regions of differing size not yet supported");
    }
    ScheduleNode& first = * * _originalNodes.begin();
    RuleChoicePtr rule = trans.learner().makeRuleChoice(first.choices()->rules(), first.matrix(), first.region());
    rule->generateCodeSimple(nodename(), trans, *this, first.region(), o);
  }else{
    o.write("DynamicTaskPtr "+nodename()+";");
    TaskCodeGenerator& task = o.createTask("coscheduled_"+nodename(), trans.maximalArgList(), "DynamicTask");
    std::string varname="coscheduled_"+nodename();
    task.beginRunFunc();
    for(size_t d=selfDep.direction.size()-1; d>=0; --d){
      bool passed=true;
      FormulaPtr begin,end;
    
      //test matching region extents in d
      for(ScheduleNodeSet::const_iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
        if(!begin) begin = (*i)->region()->minCoord()[d];
        if(!end)   end   = (*i)->region()->maxCoord()[d];
        if(   begin->toString() != (*i)->region()->minCoord()[d]->toString()
          ||  end->toString()   != (*i)->region()->maxCoord()[d]->toString())
        {
          JTRACE("Can't coschedule due to mismatched extents")(d);
          passed=false;
          break;
        }
      }
      if(!passed) continue;
      
      //test direction
      if((selfDep.direction[d]& ~DependencyDirection::D_LE) == 0){
        JTRACE("Coscheduling forward")(d)(*this);
        task.beginFor(varname, begin, end, FormulaInteger::one());
      }else if((selfDep.direction[d]& ~DependencyDirection::D_GE) == 0){
        JTRACE("Coscheduling backward")(d)(*this);
        task.beginReverseFor(varname, begin, end, FormulaInteger::one());
      }else{
        JTRACE("Can't coschedule due to mismatched direction")(d);
        passed=false;
        continue;
      }

      for(ScheduleNodeSet::iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i){
        (*i)->generateCodeForSlice(trans, task, d, new FormulaVariable(varname));
      }
      task.endFor();
      task.write("return NULL;");
      task.endFunc();
      o.setcall(nodename(),"new "+varname+"_task", task.argnames());
      printDepsAndEnqueue(o, NULL, false);
      return;
    }
    JASSERT(false)(*this)(selfDep.direction).Text("Unresolved dependency cycle");
  }
}
