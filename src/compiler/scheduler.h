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
#ifndef PETABRICKSSCHEDULER_H
#define PETABRICKSSCHEDULER_H

#include "choicedepgraph.h"

#include <fstream>

namespace petabricks {
class ChoiceGridMap;
class StaticScheduler;
class Schedule;
typedef jalib::JRef<StaticScheduler> StaticSchedulerPtr;
typedef jalib::JRef<Schedule> SchedulePtr;

/**
 * A node and a snapshot of its dynamic dependencies
 */
class ScheduleEntry {
public:
  ScheduleEntry(ChoiceDepGraphNode* node, const ScheduleDependencies& deps)
    : _node(node), _deps(deps) 
  {
    filterDeps();
  }

  const ChoiceDepGraphNode& node() const { return _node; }
  ChoiceDepGraphNode& node() { return _node; }

  void printNode(std::ostream& o) const {
    node().printNode(o);
  }
  void printEdge(std::ostream& o) const {
    for(ScheduleDependencies::const_iterator i=_deps.begin(); i!=_deps.end(); ++i){
      if(i->first->isInput()) {
        o << "  input";
      }else{
        o << "  " << i->first->nodename();
      }
      o << " -> "
        << node().nodename()
        << "[ label=\"" << i->second.direction << "\"];\n";
    }
  }
  void filterDeps() {
    ScheduleDependencies tmp;
    for(ScheduleDependencies::const_iterator i=_deps.begin(); i!=_deps.end(); ++i){
      if(i->first->isInput()) {
        continue;
      }
      if(i->first == _node.asPtr()) {
        continue;
      }
      tmp[i->first] = i->second;
    }
    tmp.swap(_deps);
  }

  const ScheduleDependencies& deps() const { return _deps; }
  ScheduleDependencies& deps() { return _deps; }
private:
  ChoiceDepGraphNodePtr _node;
  ScheduleDependencies _deps;
};

/**
 * Contains an order to execute nodes in
 */
class Schedule : public jalib::JRefCounted, public jalib::JPrintable{
  /** Intermediate state used while scheduling */
  struct SchedulingState {
    ChoiceDepGraphNodeSet generated;
    ChoiceDepGraphNodeSet pending;
  };
public:
  Schedule(const RuleChoiceAssignment& choice,
           const ChoiceDepGraphNodeSet& inputs,
           const ChoiceDepGraphNodeSet& intermediates,
           const ChoiceDepGraphNodeSet& outputs)
    : _choiceAssignment(choice)
  {
    initialize(inputs, intermediates, outputs);
  }
  void generateCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor);

  size_t size() const { return _schedule.size(); }

  void print(std::ostream& o) const {
    o << "digraph schedule {\n";
    for(ScheduleT::const_iterator i=_schedule.begin(); i!=_schedule.end(); ++i){
      i->printNode(o);
    }
    for(ScheduleT::const_iterator i=_schedule.begin(); i!=_schedule.end(); ++i){
      i->printEdge(o);
    }
    o << "}\n";
  }

  void writeGraph(const char* filename) {
    std::ofstream of(filename);
    of << *this;
  }
protected:
  void initialize(const ChoiceDepGraphNodeSet& inputs,
                  const ChoiceDepGraphNodeSet& intermediates,
                  const ChoiceDepGraphNodeSet& outputs);
  void depthFirstChoiceDepGraphNode(SchedulingState& state, ChoiceDepGraphNode* n);  
private:
  // the ordering
  typedef std::vector<ScheduleEntry> ScheduleT;
  ScheduleT _schedule;
  RuleChoiceAssignment _choiceAssignment;

  std::map<RuleChoiceConsumer*,bool> _copyAssignment;
  std::map<RuleChoiceConsumer*,int> _numOutOnGpu;
};

namespace {
  void reachFirstValidRuleFromHere(ChoiceDepGraphNodeList::iterator& _nodesIt,
                                   ChoiceDepGraphNodeList::iterator& _nodesEnd,
                                   RuleSet::iterator& _rulesIt) {
    if (_rulesIt != (*_nodesIt)->choices().end()) {
        //The rule is already valid
        return;
      }
      
    //This set of rules is finished! Let's go to the next one
    bool found;
    do {
      found=true;
      _nodesIt++;
      
      if (_nodesIt == _nodesEnd) {
        //No more rules!!
        break;
      }
      
      _rulesIt=(*_nodesIt)->choices().begin();
      
      if (_rulesIt == (*_nodesIt)->choices().end()) {
        found=false;
      }
    } while(!found);
  }
}

/**
 * Create a manage a set of legal schedules
 */
class StaticScheduler : public jalib::JRefCounted,
                        public jalib::JPrintable,
                        public jalib::SrcPosTaggable
{
public:
  class rule_iterator;
  friend class rule_iterator;
  class rule_iterator {
  public:
    rule_iterator(StaticScheduler& scheduler) {
      _nodesEnd = scheduler._allNodes.end();
      _nodesIt = scheduler._allNodes.begin();
      if (_nodesIt == _nodesEnd) {
        //There are no node and therefore no rules
        return;
      } 
      else {
        _rulesIt = (*_nodesIt)->choices().begin();
        reachFirstValidRuleFromHere(_nodesIt, _nodesEnd, _rulesIt);
      }
    }
    
    rule_iterator(ChoiceDepGraphNodeList::iterator nodesIt) 
                 : _nodesIt(nodesIt) {}
  
    void operator++ () {
      if (_nodesIt == _nodesEnd) {
        //The iterator is already at the end. Nothing to do
        return;
      }
      
      _rulesIt++;
      reachFirstValidRuleFromHere(_nodesIt, _nodesEnd, _rulesIt);
    }
    
    bool operator== (rule_iterator& that) {
      return _nodesIt==that._nodesIt && (_nodesIt==_nodesEnd
                                         || _rulesIt==that._rulesIt);
    }
    
    bool operator !=(rule_iterator& that) {
      return ! (*this==that);
    }
    
    RulePtr operator*() {
      return *_rulesIt;
    }
    
  private:
    ChoiceDepGraphNodeList::iterator _nodesIt;
    ChoiceDepGraphNodeList::iterator _nodesEnd;
    RuleSet::iterator _rulesIt;
  };

public:
  rule_iterator rule_begin() {
    return rule_iterator(*this);
  }
    
  rule_iterator rule_end() {
    return rule_iterator(_allNodes.end());
  }
    
public:
  class CantScheduleException {};

  ChoiceDepGraphNodeSet lookupNode(const MatrixDefPtr& matrix, const SimpleRegionPtr& region);

  StaticScheduler(const ChoiceGridMap& cg, Transform&);

  void markInputMatrix(const MatrixDefPtr& matrix){
    SimpleRegionPtr r = new SimpleRegion(matrix);
    ChoiceDepGraphNodeList& regions = _matrixToNodes[matrix];
    JASSERT(regions.size()==0)(matrix).Text("Rules given for input matrix");
    regions.push_back(new BasicChoiceDepGraphNode(matrix, r, NULL));
    _allNodes.push_back(regions.back());
    _inputsOriginal.insert(regions.back().asPtr());
    regions.back()->markInput();
  }

  void markOutputMatrix(const MatrixDefPtr& matrix){
    ChoiceDepGraphNodeList& lst = _matrixToNodes[matrix];
    JASSERT(lst.size()>0);
    for(ChoiceDepGraphNodeList::iterator i=lst.begin(); i!=lst.end(); ++i){
      _outputsOriginal.insert(i->asPtr());
      (*i)->markOutput();
    }
  }

  void renderGraph(const char* filename, const char* type="png") const;
  void writeGraph(const char* filename) const;
  void writeGraph(FILE* fd) const;

  void generateSchedule();

  void computeIndirectDependencies();
  void mergeCoscheduledNodes(const RuleChoiceAssignment& choice);
  
  //const ChoiceDepGraphNodeList& schedule() const { return _schedule; }

  void print(std::ostream& o) const {
    o << "digraph staticScheduler {\n";
    for(ChoiceDepGraphNodeList::const_iterator r=_allNodes.begin(); r!=_allNodes.end(); ++r){
      (*r)->printNode(o);
    }
    for(ChoiceDepGraphNodeList::const_iterator r=_allNodes.begin(); r!=_allNodes.end(); ++r){
      (*r)->printEdges(o);
    }
    o << "}\n";
  }

  void generateCode(Transform& trans, CodeGenerator& o, RuleFlavor type);

  void generateGlobalCode(Transform& trans, CodeGenerator& o);

  int size() const { return _allNodes.size() - _inputsOriginal.size(); }

private:
  
  struct DataDepSetCompare {
    bool operator() (const CoordinateFormula* a, const CoordinateFormula* b) const;
  };
  
  class DataDependencySet : public std::set<CoordinateFormula*,DataDepSetCompare> {
  public:
    /**
     * Return true if the given dimension is equal to value in al the data 
     * dependency vectors in the set
     */
    bool isDimensionDependencyAlwaysEqualTo(size_t dimension, int value) const;
  };
  
  enum DimensionStatus {
    ALWAYS_MINUS1,
    ALWAYS_ZERO,
    OTHER
  };
  
  typedef std::map<MatrixDefPtr, DataDependencySet> MatrixDataDependencyMap;

private:
  void removeUselessDimensions();
  void removeUselessDimensions(std::vector<size_t> uselessDimensions, 
                               MatrixDefPtr matrix);
  void importDataDepsFromRule(RulePtr& rule, 
                              MatrixDataDependencyMap& dataDepsMap);
  std::vector<size_t> findUselessDimensions(
                                    const DataDependencySet matrixDependencies,
                                    const MatrixDefPtr matrix);
                                    
  void filterNonSelfDependentAccesses(std::vector<size_t>& uselessDimensions,
                                      const MatrixDefPtr matrix);
  MatrixDataDependencyMap getDataDepsForTemporaryMatrixes ();
  
  void fixVersionedRegionsType();
private:
  //storage of nodes
  std::map<MatrixDefPtr, ChoiceDepGraphNodeList> _matrixToNodes;
  ChoiceDepGraphNodeList _allNodes;
  ChoiceDepGraphNodeList _metaNodes;

  ChoiceDepGraphNodeSet _inputsOriginal;
  ChoiceDepGraphNodeSet _inputsRemapped;
  ChoiceDepGraphNodeSet _intermediatesOriginal;
  ChoiceDepGraphNodeSet _intermediatesRemapped;
  ChoiceDepGraphNodeSet _outputsOriginal;
  ChoiceDepGraphNodeSet _outputsRemapped;

  //output schedule
  typedef std::vector<SchedulePtr> SchedulesT;
  SchedulesT _schedules;

  RuleChoiceCollection _choices;

  std::string _dbgpath;
};

}

#endif
