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
#include "scheduler.h"

#include "codegenerator.h"
#include "pbc.h"
#include "transform.h"
#include "maximawrapper.h"
#include "heuristicmanager.h"
#include "common/jasm.h"

#include <cstdio>
#include <algorithm>

static void _setEachTo(petabricks::ChoiceDepGraphNodeRemapping& mapping,
                       const petabricks::ChoiceDepGraphNodeSet& set,
                       petabricks::ChoiceDepGraphNode* node) {
  petabricks::ChoiceDepGraphNodeSet::const_iterator e;
  for(e=set.begin();e!=set.end(); ++e) {
    mapping[*e] = node;
  }
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

void petabricks::Schedule::initialize(const ChoiceDepGraphNodeSet& inputs,
                                      const ChoiceDepGraphNodeSet& intermediates,
                                      const ChoiceDepGraphNodeSet& outputs){
  JASSERT(_schedule.empty());

  SchedulingState state;
  state.generated = inputs;

  for( std::set<ChoiceDepGraphNode*>::const_iterator i=outputs.begin()
    ; i!=outputs.end()
    ; ++i )
  {
    JASSERT((*i)->isOutput());
    depthFirstChoiceDepGraphNode(state, *i);
  }

  ChoiceDepGraphNodeSet::const_iterator i;
  for(i=intermediates.begin(); i!=intermediates.end(); ++i) {
    if(state.generated.find(*i) == state.generated.end()) {
      if( _choiceAssignment[*i] != *(*i)->choices().begin() ) {
        JTRACE("not generated node has unused schedule");
        throw StaticScheduler::CantScheduleException();
      }
    }
  }
}


void petabricks::Schedule::depthFirstChoiceDepGraphNode(SchedulingState& state, ChoiceDepGraphNode* n){
  if(state.generated.find(n)!=state.generated.end())
    return;

  JASSERT(state.pending.find(n)==state.pending.end()).Text("dependency cycle");
  state.pending.insert(n);

  for( ScheduleDependencies::const_iterator i=n->directDepends().begin()
      ; i!=n->directDepends().end()
      ; ++i)
  {
    if(i->first != n) {
      depthFirstChoiceDepGraphNode(state, i->first);
    }
  }
//   JTRACE("scheduling")(n->matrix()); 
  _schedule.push_back(ScheduleEntry(n, n->directDependsRemapped()));
  state.generated.insert(n);
}

void petabricks::Schedule::generateCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor){
  JASSERT(_schedule.size()>0);


#ifdef HAVE_OPENCL

  for(ScheduleT::iterator i=_schedule.begin(); i!=_schedule.end(); ++i){
    i->node().resetRegionNodeGroups();
  }

  // Assign node to perform
  // 1) copy out immediately <setGpuCopyOut> 
  // 2) pend copy out <setPendingGpuCopyOut> 
  //  3) don't copy out at all <do nothing>

  // Check output before inner rules because inner rules can overwrite setPendingGpuCopyOut to setGpuCopyOut
  // becuase we don't know if output will be used on gpu or cpu next (we don't have cross transform analysis)
  MatrixDefList matrices = trans.getToMatrices();
  for(MatrixDefList::iterator it = matrices.begin(); it != matrices.end(); ++it){
    std::vector<int> ids;
    ScheduleT::iterator last = _schedule.end();
    for(ScheduleT::iterator j=_schedule.begin(); j!=_schedule.end(); ++j){
      int overlappingDimensions = j->node().hasOverlappingRegionOnGpu(_choiceAssignment, *it);
      if(overlappingDimensions >= 0){
        // If there is overlapping, might need to copy out
        if(overlappingDimensions == (*it)->numDimensions()) {
          // If same, can be pended the copy out later
          j->node().setPendingGpuCopyOut();
        }
        else {
          // If dimension is not the same, have to copy out immediately
          j->node().setGpuCopyOut();
        }
        last = j;
        ids.push_back(j->node().id());
      }
    }
    if(last != _schedule.end())
      last->node().addGroup((*it)->name(),ids);
  }

  // Check inner rules. If there is overlapping, we eagerly copy out because we know we need the data on cpu for sure.
  for(ScheduleT::iterator i=_schedule.begin(); i!=_schedule.end(); ++i){
    std::set<std::string> matrices;
    RegionList from = i->node().getFromRegionOnCpu(_choiceAssignment);

    for(RegionList::iterator it = from.begin(); it != from.end(); ++it){
      if(matrices.find((*it)->matrix()->name()) == matrices.end()) {
        matrices.insert((*it)->matrix()->name());
        std::vector<int> ids;
        ScheduleT::iterator last = _schedule.end();

        for(ScheduleT::iterator j=_schedule.begin(); j!=i; ++j){
          if(j->node().hasOverlappingRegionOnGpu(_choiceAssignment, *it) >= 0){
            j->node().setGpuCopyOut();
            last = j;
            ids.push_back(j->node().id());
          }
        }
        if(last != _schedule.end())
          last->node().addGroup((*it)->matrix()->name(),ids);
      }
    }
  }

#endif

  o.comment("MARKER 1");
  for(ScheduleT::iterator i=_schedule.begin(); i!=_schedule.end(); ++i){
    if(i!=_schedule.begin() && flavor!=RuleFlavor::SEQUENTIAL)
      o.continuationPoint();

    i->node().generateCode(trans, o, flavor, _choiceAssignment);

    if(flavor!=RuleFlavor::SEQUENTIAL) {
      for(ScheduleDependencies::const_iterator d=i->deps().begin();
          d!=i->deps().end();
          ++d){
        o.write(i->node().nodename()+"->dependsOn("+d->first->nodename()+");");
      }
      o.write(i->node().nodename()+"->enqueue();");
    }
  }
  if(flavor!=RuleFlavor::SEQUENTIAL) {
    o.write("DynamicTaskPtr  _fini = new NullDynamicTask();");
    for(ScheduleT::iterator i=_schedule.begin(); i!=_schedule.end(); ++i){
      if(i->node().isOutput()) {
        o.write("_fini->dependsOn(" + i->node().nodename() + ");");
      }
    }
    for(ScheduleT::iterator i=_schedule.begin(); i!=_schedule.end(); ++i){
      o.write(i->node().nodename()+"=0;");
    }
    o.write("return _fini;");
  }
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////



petabricks::StaticScheduler::StaticScheduler(const ChoiceGridMap& cg, Transform& tx){
  for(ChoiceGridMap::const_iterator m=cg.begin(); m!=cg.end(); ++m){
    ChoiceDepGraphNodeList& regions = _matrixToNodes[m->first];
    for(ChoiceGridIndex::const_iterator i=m->second.begin(); i!=m->second.end(); ++i){
      SimpleRegionPtr tmp = new SimpleRegion(i->first);
      regions.push_back(new BasicChoiceDepGraphNode(m->first, tmp, i->second));
      _allNodes.push_back(regions.back());
    }
  }
  _dbgpath = pbcConfig::theObjDir+"/"+tx.name();
}

petabricks::ChoiceDepGraphNodeSet petabricks::StaticScheduler::lookupNode(const MatrixDefPtr& matrix, const SimpleRegionPtr& region){
  ChoiceDepGraphNodeSet rv;
  ChoiceDepGraphNodeList& regions = _matrixToNodes[matrix];
  if(matrix->numDimensions()==0){
    JASSERT(regions.size()==1);
    rv.insert(regions.begin()->asPtr());
  }else{
    for(ChoiceDepGraphNodeList::iterator i=regions.begin(); i!=regions.end(); ++i){
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

namespace {
  petabricks::RegionList getSameMatrixRegions(
                                  petabricks::RegionList& selfDepRegions, 
                                  const petabricks::MatrixDefPtr& matrix) {
    petabricks::RegionList sameMatrixRegions;
    std::string matrixName = matrix->name();
    
    for(petabricks::RegionList::iterator i=selfDepRegions.begin(), 
                                         e=selfDepRegions.end();
        i != e;
        ++i) {
      petabricks::RegionPtr region = *i;
      
      if(region->matrix()->name() == matrixName) {
        sameMatrixRegions.push_back(region);
      }
    }
    
    return sameMatrixRegions;
  }
  
  /**
   * Does this region access its matrix in the middle? Or just at the end?
   */
  bool accessInTheMiddle(const petabricks::RegionPtr& region, 
                         const size_t dimension) {
    /* If the maxCoord of the region for this dimension is not equal to the size
     * of the dimension, we are accessing some place in the middle of the matrix
     */
    petabricks::FormulaPtr maxCoordDim = region->maxCoord()[dimension];
    petabricks::FormulaPtr dimSize = region->matrix()->getSizeOfDimension(dimension);
    
    return ! petabricks::MaximaWrapper::instance().comparePessimistically(maxCoordDim, "=", dimSize);
  }
  
  void filterAccess(std::vector<size_t>& uselessDimensions,
                                    petabricks::RegionList& regionList) {
    for (petabricks::RegionList::iterator i=regionList.begin(), 
                                          e=regionList.end();
         i != e;
         ++i) {
      petabricks::RegionPtr region = *i;
      
      for(std::vector<size_t>::iterator dimIt=uselessDimensions.begin(),
                                        dimIt_end=uselessDimensions.end();
          dimIt != dimIt_end;
          ++dimIt) {
        const size_t dimension = *dimIt;
        if(dimension >= region->matrix()->numDimensions() || accessInTheMiddle(region, dimension)) {
          uselessDimensions.erase(dimIt);
        }
      }
    }
  }
}

void petabricks::StaticScheduler::filterNonSelfDependentAccesses(
                                          std::vector<size_t>& uselessDimensions,
                                          const MatrixDefPtr matrix) {
  for(rule_iterator i=rule_begin(), e=rule_end(); i!=e; ++i) {
    RulePtr rule = *i;
    
    RegionList selfDepRegions = rule->getNonSelfDependentRegions();
    
    if (selfDepRegions.size()==0) {
      continue;
    }
    
    RegionList sameMatrixRegions = getSameMatrixRegions(selfDepRegions, matrix);
    filterAccess(uselessDimensions, sameMatrixRegions);
    
  }
}

std::vector<size_t> petabricks::StaticScheduler::findUselessDimensions(
                                const DataDependencySet matrixSelfDependencies,
                                const MatrixDefPtr matrix) {
  
  size_t dimensions = (*(matrixSelfDependencies.begin()))->size();
  
  //Verify if this dimension is always 0 or -1 selfdependent for this matrix
  std::vector<DimensionStatus> matrixStatus;
  for(size_t dim=0; dim<dimensions; ++dim) {
    if (matrixSelfDependencies.isDimensionDependencyAlwaysEqualTo(dim, -1)) {
      matrixStatus.push_back(ALWAYS_MINUS1);
      continue;
    }
    
    if (matrixSelfDependencies.isDimensionDependencyAlwaysEqualTo(dim, 0)) {
      matrixStatus.push_back(ALWAYS_ZERO);
      continue;
    }
    
    matrixStatus.push_back(OTHER);
  }
  
  //Detect candidate useless dimensions
  std::vector<size_t> uselessDimensions;
  /* A dimension is useless if it is always involved in a -1 data dependency 
   * on itself, and every other dimension is 0 or -1 data dependend 
  for(size_t dim=0; dim<dimensions; ++dim) {
    if(matrixStatus[dim]==OTHER) {
      //Something is not 0 or -1. No dimension can be removed
      uselessDimensions.clear();
      return uselessDimensions;
    }
    else if(matrixStatus[dim]==ALWAYS_MINUS1) {
      //This dimension can be removed if no dimension is "other" dependent.
      uselessDimensions.push_back(dim);
    }
  }
  */
   
  

  /* A dimension is useless if it is always involved in a -1 data dependency and 
   * every other dimension is 0 data dependent
  for(size_t dim=0; dim<dimensions; ++dim) {
    if(matrixStatus[dim]==ALWAYS_MINUS1) {
      //This dimension can be removed if all the other dimensions 
      //are 0 data dependent
      if (uselessDimensions.size()==0) {
        uselessDimensions.push_back(dim);
      }
      else {
        //More than 1 -1dd dimension. Abort dimension removal.
        uselessDimensions.clear();
        return uselessDimensions;
      }
    }
    else if(matrixStatus[dim]==OTHER) {
      uselessDimensions.clear();
      return uselessDimensions;
    }
  } */
  
  /* The last dimension is useless if it is -1 data dependent and all the others
   * are 0 data dependent  */
  for(size_t dim=0; dim<dimensions-1; ++dim) {
    if(matrixStatus[dim]!=ALWAYS_ZERO)
      return uselessDimensions;
  }
  if (matrixStatus[dimensions-1]==ALWAYS_MINUS1) {
    uselessDimensions.push_back(dimensions-1);
  }
  
  filterNonSelfDependentAccesses(uselessDimensions, matrix);
  return uselessDimensions;
}


void petabricks::StaticScheduler::removeUselessDimensions(std::vector<size_t> uselessDimensions, 
                                                          MatrixDefPtr matrix) {
  for(std::vector<size_t>::iterator i=uselessDimensions.begin(),
                                    e=uselessDimensions.end();
                                    i != e;
                                    ++i) {
    size_t dim=*i;
    matrix->removeDimension(dim); 
    
    _allNodes.removeDimensionFromRegions(matrix, dim);
  }
} 


void petabricks::StaticScheduler::importDataDepsFromRule(
                                          RulePtr& rule, 
                                          MatrixDataDependencyMap& dataDepsMap) {
  DataDependencyVectorMap& dataDepsVectorMap=rule->getDataDependencyVectorMap();
  
  for(DataDependencyVectorMap::iterator i=dataDepsVectorMap.begin(), e=dataDepsVectorMap.end();
      i!=e; ++i) {
    MatrixDefPtr matrix = i->first;
    if(matrix->type() != MatrixDef::T_THROUGH) {
      //We cannot modify this matrix, so we don't care
      continue;
    }
    CoordinateFormula& dataDepVector = i->second;
    DataDependencySet& dataDepSet = dataDepsMap[matrix];
    dataDepSet.insert(&dataDepVector);
  }
}


/**
 * Get the data dependency information about all the matrix that are used
 * as temporary storage ("through" matrixes) to compute the outcome of a
 * transformation 
 */
petabricks::StaticScheduler::MatrixDataDependencyMap 
               petabricks::StaticScheduler::getDataDepsForTemporaryMatrixes () {
  MatrixDataDependencyMap dataDepsMap;
  
  for(rule_iterator i=rule_begin(), e=rule_end(); i != e; ++i) {
    RulePtr rule = *i;
    importDataDepsFromRule(rule, dataDepsMap);
  }
  return dataDepsMap;
}
   
void petabricks::StaticScheduler::removeUselessDimensions() {
  
  const MatrixDataDependencyMap& tempMatrixesDataDeps = getDataDepsForTemporaryMatrixes();
  //For every matrix in the program:
  for(MatrixDataDependencyMap::const_iterator i=tempMatrixesDataDeps.begin(),
                                        e=tempMatrixesDataDeps.end();
                                        i != e;
                                        ++i) {
    MatrixDefPtr matrix = i->first;
    DataDependencySet matrixDependencies = i->second;
    
    std::vector<size_t> uselessDimensions = findUselessDimensions(
                                                          matrixDependencies,
                                                          matrix);
    removeUselessDimensions(uselessDimensions, matrix);
  }
}

void petabricks::StaticScheduler::fixVersionedRegionsType() {
  _allNodes.fixVersionedRegionsType();
}
 

void petabricks::StaticScheduler::generateSchedule(){
  std::string dbgpathorig = _dbgpath;


  for(ChoiceDepGraphNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i){
    if(!(*i)->isInput() && !(*i)->isOutput()) {
      _intermediatesOriginal.insert(i->asPtr());
    }
  }



  for(ChoiceDepGraphNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i){
    _choices.addConsumer(i->asPtr());
  }
  _choices.pruneChoiceSpace();
  JASSERT(_choices.size()<1000)(_choices.size()).Text("WAY TOO MANY CHOICES");
  for(RuleChoiceCollection::iterator choice=_choices.begin(); choice!=_choices.end(); ++choice) {
    _dbgpath = dbgpathorig+".schedule"+jalib::XToString(choice);
    try {
      RuleChoiceAssignment choiceassign = _choices.getAssignment(choice);
      for(ChoiceDepGraphNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i){
        (*i)->resetRemapping();
      }

      #ifdef DEBUG
      writeGraph((_dbgpath+".initial.dot").c_str());
      #endif

      for(ChoiceDepGraphNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i){
        (*i)->applyChoiceRemapping(choiceassign);
      }

      computeIndirectDependencies();
      
      #ifdef DEBUG
      writeGraph((_dbgpath+".remapped.dot").c_str());
      #endif

      removeUselessDimensions();
      fixVersionedRegionsType();
      
      #ifdef DEBUG
      writeGraph((_dbgpath+".noUselessDimensions.dot").c_str());
      #endif

      mergeCoscheduledNodes(choiceassign);

      #ifdef DEBUG
      writeGraph((_dbgpath+".merged.dot").c_str());
      #endif

/*
      //---------------- TEST ----------------------
      HeuristicManager::instance().registerDefault("testHeuristic", "21+2+a");
      HeuristicManager::instance().registerDefault("anotherHeuristic", "5*4*(a+1)");
      HeuristicManager::instance().registerDefault("fromFile", "42");
      Heuristic& test = HeuristicManager::instance().getHeuristic("testHeuristic");
      Heuristic& test2 = HeuristicManager::instance().getHeuristic("anotherHeuristic");
      Heuristic& test3 = HeuristicManager::instance().getHeuristic("fromFile");
      ValueMap values;
      values["a"]=5;
      JTRACE("TestHeuristic")(test.eval(values))(test2.eval(values))(test3.eval(values));
      //--------------------------------------------
*/

      _schedules.push_back(new Schedule(choiceassign,
                                        _inputsRemapped,
                                        _intermediatesRemapped,
                                        _outputsRemapped));
      
      #ifdef DEBUG
      _schedules.back()->writeGraph((_dbgpath+".final.dot").c_str());
      #endif
    } catch(CantScheduleException) {
      _choices.markInvalid(choice);
      JTRACE("SCHEDULING CHOICE FAIL")(choice);
    }
  }
  _dbgpath = dbgpathorig;
  JASSERT(!_schedules.empty())
    .Text("failed to find a legal schedule for nodes");
}



void petabricks::StaticScheduler::computeIndirectDependencies(){
  // this algorithm can be optimized, but since graphs are small it doesn't matter
  for(int c=1; c>0;){ //keep interating until no changes have been made
    c=0;
    for(ChoiceDepGraphNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i)
      c+=(*i)->updateIndirectDepends();
  }
}

void petabricks::StaticScheduler::mergeCoscheduledNodes(const RuleChoiceAssignment& choice){
  ChoiceDepGraphNodeSet done;
  ChoiceDepGraphNodeRemapping mapping;
  ChoiceDepGraphNodeList newMetaNodes;
  for(ChoiceDepGraphNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i){
    if(done.find(i->asPtr()) == done.end()){
      //process nodes one connected component at a time
      ChoiceDepGraphNodePtr meta;
      ChoiceDepGraphNodeSet set=(*i)->getMultioutputComponent();
      if(set.size()>1){
        if(set.overlaps(done) || set!=(*i)->getStronglyConnectedComponent()){
          JTRACE("invalid multioutput schedule");
          throw CantScheduleException();
        }
        done.insert(set.begin(),set.end());
        meta = new MultiOutputChoiceDepGraphNode(set);
      }else{
        set=(*i)->getStronglyConnectedComponent();
        done.insert(set.begin(),set.end());
        if(set.size()>1){
          meta = new SlicedChoiceDepGraphNode(set);
        }
      }
      if(meta){
        _setEachTo(mapping, set, meta.asPtr());
        newMetaNodes.push_back(meta);
        JTRACE("coscheduling nodes")(meta);
      }
    }
  }
  for(ChoiceDepGraphNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i){
    (*i)->applyRemapping(mapping);
  }
  for(ChoiceDepGraphNodeList::iterator i=newMetaNodes.begin(); i!=newMetaNodes.end(); ++i){
    (*i)->applyRemapping(mapping);
  }
  for(ChoiceDepGraphNodeList::iterator i=newMetaNodes.begin(); i!=newMetaNodes.end(); ++i){
    if(!(*i)->findValidSchedule(choice)) {
      throw StaticScheduler::CantScheduleException();
    }
  }
  _inputsRemapped = _inputsOriginal;
  _inputsRemapped.applyRemapping(mapping);
  _intermediatesRemapped = _intermediatesOriginal;
  _intermediatesRemapped.applyRemapping(mapping);
  _outputsRemapped = _outputsOriginal;
  _outputsRemapped.applyRemapping(mapping);
  _metaNodes.insert(_metaNodes.end(), newMetaNodes.begin(), newMetaNodes.end());
}

void petabricks::StaticScheduler::generateCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor) {

  if(flavor!=RuleFlavor::SEQUENTIAL) {
    for(ChoiceDepGraphNodeList::iterator i=_allNodes.begin(); i!=_allNodes.end(); ++i){
      o.addMember("DynamicTaskPtr", (*i)->nodename(), "");
    }
    for(ChoiceDepGraphNodeList::iterator i=_metaNodes.begin(); i!=_metaNodes.end(); ++i){
      o.addMember("DynamicTaskPtr", (*i)->nodename(), "");
    }
  }

  SchedulesT::iterator i;
  CodeGenerator& schedOutput = o.forkhelper();
  int n=0;
  if(_schedules.size()==1) {
    o.beginSwitch("0");
  }else{
    o.beginSwitch(trans.name()+"_selectSchedule("TRANSFORM_N_STR"())");
  }
  for(i=_schedules.begin(); i!=_schedules.end(); ++i,++n) {
    o.beginCase(n);

    if(flavor==RuleFlavor::SEQUENTIAL){
      schedOutput.beginFunc("void", "schedule"+jalib::XToString(n)+"seq");
      o.write("schedule"+jalib::XToString(n)+"seq();");
      o.write("return;");
    } else {
      schedOutput.beginFunc("DynamicTaskPtr", "schedule"+jalib::XToString(n)+"workstealing");
      o.write("return schedule"+jalib::XToString(n)+"workstealing();");
    }
    (*i)->generateCode(trans, schedOutput, flavor);
    schedOutput.endFunc();

    o.endCase();
  }
  o.endSwitch();
  o.write("JASSERT(false).Text(\"invalid schedule\");");
  if(flavor!=RuleFlavor::SEQUENTIAL) {
    o.write("return 0;");
  }

    
}

void petabricks::StaticScheduler::generateGlobalCode(Transform& trans, CodeGenerator& o) {
  std::string prefix = trans.name() + "_" + jalib::XToString(trans.nextTunerId()) + "_";
  if(_schedules.size()>1) {
    o.beginFunc("int", trans.name()+"_selectSchedule", std::vector<std::string>(1,"int _transform_n"));
    _choices.generateDecisionTree(prefix, _schedules.size(), o);
    o.endFunc();
  }
}


void petabricks::StaticScheduler::renderGraph(const char* filename, const char* type) const{
  FILE* fd = popen(("dot -Grankdir=TD -T"+std::string(type)+" -o "+std::string(filename)).c_str(), "w");
  writeGraph(fd);
  pclose(fd);
}

void petabricks::StaticScheduler::writeGraph(const char* filename) const{
  FILE* fd = fopen(std::string(filename).c_str(), "w");
  //writeGraph(fd);
  //fclose(fd);
  JWARNING(fd != NULL)(JASSERT_ERRNO)(filename).Text("failed to open file");
  if(fd != NULL) {
    writeGraph(fd);
    fclose(fd);
  }
}

void petabricks::StaticScheduler::writeGraph(FILE* fd) const{
  std::string schedulerGraph = toString();
  fwrite(schedulerGraph.c_str(),1,schedulerGraph.length(),fd);
}

bool petabricks::StaticScheduler::DataDepSetCompare::operator() (
                                  const CoordinateFormula* a, 
                                  const CoordinateFormula* b) const {
  size_t sizeA = a->size();
  size_t sizeB = b->size();

  if (sizeA != sizeB) {
    return sizeA < sizeB;
  }

  CoordinateFormula::const_iterator iterA = a->begin();
  CoordinateFormula::const_iterator iterB = b->begin();

  while(MaximaWrapper::instance().compare(*iterA, "=", *iterB)) {
    iterA++;
    iterB++;
    
    if(iterA == a->end()) 
      return false;
  }

  return MaximaWrapper::instance().compare(*iterA, "<", *iterB);

}

bool petabricks::StaticScheduler::DataDependencySet::isDimensionDependencyAlwaysEqualTo(size_t dimension, int value) const {
  FormulaPtr valueFormula = new FormulaLiteral<int>(value);
  for(DataDependencySet::const_iterator i=this->begin(), e=this->end();
      i != e; ++i) {
    CoordinateFormula& dependencyVector = **i;
    FormulaPtr dependency = dependencyVector[dimension];
    
    if(! MaximaWrapper::instance().compare(dependency, "=", valueFormula)) {
      return false;
    }
  }
  
  return true;
}
