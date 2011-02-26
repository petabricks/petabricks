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
  Schedule(const RuleChoiceAssignment& choice, const ChoiceDepGraphNodeSet& inputs, const ChoiceDepGraphNodeSet& outputs)
    : _choiceAssignment(choice)
  {
    initialize(inputs, outputs);
  }
  void generateCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor);

  size_t size() const { return _schedule.size(); }

  void print(std::ostream& o) const {
    o << "digraph {\n";
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
  void initialize(const ChoiceDepGraphNodeSet& inputs, const ChoiceDepGraphNodeSet& outputs);
  void depthFirstChoiceDepGraphNode(SchedulingState& state, ChoiceDepGraphNode* n);  
private:
  // the ordering
  typedef std::vector<ScheduleEntry> ScheduleT;
  ScheduleT _schedule;
  RuleChoiceAssignment _choiceAssignment;
};

/**
 * Create a manage a set of legal schedules
 */
class StaticScheduler : public jalib::JRefCounted,
                        public jalib::JPrintable,
                        public jalib::SrcPosTaggable
{
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
    o << "digraph {\n";
    for(ChoiceDepGraphNodeList::const_iterator r=_allNodes.begin(); r!=_allNodes.end(); ++r){
      (*r)->printNode(o);
    }
    for(ChoiceDepGraphNodeList::const_iterator r=_allNodes.begin(); r!=_allNodes.end(); ++r){
      (*r)->printEdges(o);
    }
    o << "}\n";
  }

  void generateCode(Transform& trans, CodeGenerator& o, RuleFlavor type);

  int size() const { return _allNodes.size() - _inputsOriginal.size(); }
private:
  //storage of nodes
  std::map<MatrixDefPtr, ChoiceDepGraphNodeList> _matrixToNodes;
  ChoiceDepGraphNodeList _allNodes;
  ChoiceDepGraphNodeList _metaNodes;

  ChoiceDepGraphNodeSet _inputsOriginal;
  ChoiceDepGraphNodeSet _inputsRemapped;
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
