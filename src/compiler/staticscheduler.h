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
#ifndef PETABRICKSSTATICSCHEDULER_H
#define PETABRICKSSTATICSCHEDULER_H

#include "choicegrid.h"
#include "matrixdef.h"
#include "matrixdependency.h"
#include "region.h"

#include "common/jrefcounted.h"
#include "common/srcpos.h"

#include <list>
#include <map>
#include <vector>

namespace petabricks {
class Transform;
class CodeGenerator;
class ScheduleNode;
class StaticScheduler;
typedef std::set<ScheduleNode*> ScheduleNodeSet;
typedef std::vector< jalib::JRef<ScheduleNode> > ScheduleNodeList; 
typedef std::map<ScheduleNode*,ScheduleNode*> ScheduleNodeRemapping;
typedef jalib::JRef<StaticScheduler> StaticSchedulerPtr;

struct DependencyInformation {
  RuleSet rules;
  DependencyDirection direction;

  ///
  /// Add that to this, true if information was updates
  bool merge(const DependencyInformation& that){
    size_t nRules = rules.size();
    DependencyDirection oldDir = direction;
    rules.insert(that.rules.begin(), that.rules.end());
    direction.addDirection(that.direction);
    return rules.size()!=nRules || oldDir!=direction;
  }
};

class ScheduleDependencies : public std::map<ScheduleNode*, DependencyInformation>{
public:
  void merge(const ScheduleDependencies& that){
    for(const_iterator i=that.begin(); i!=that.end(); ++i)
      operator[](i->first).merge(i->second);
  }
};

class ScheduleNode : public jalib::JRefCounted, public jalib::JPrintable, public jalib::SrcPosTaggable {
public:
  ///
  /// Constructor
  ScheduleNode();

  ///
  /// Add a dependency edge
  void addDependency(ScheduleNode* n, const RulePtr& r, const DependencyDirection& dir){
    _directDepends[n].rules.insert(r);
    _directDepends[n].direction.addDirection(dir);
  }

  ///
  /// Print this node name in graphviz/dot format
  void printNode(std::ostream& os) const{
    os << "  " << nodename() << "[label=\"" << nodename() << ": " 
       << *this
       << "\"];\n";
  }

  ///
  /// Print this node's edges in graphviz/dot format
  void printEdges(std::ostream& os) const{
//     const ScheduleDependencies& deps = _indirectDepends.empty() ? _directDepends : _indirectDepends;
    const ScheduleDependencies& deps = _directDepends;
    for(ScheduleDependencies::const_iterator i=deps.begin(); i!=deps.end(); ++i){
      os << "  " << i->first->nodename() << " -> " << nodename() << "[ label=\"" << i->second.direction << "\"";
      if(_directDepends.find(i->first)==_directDepends.end()) os << ", style=dashed";
      os << "];\n";
    }
  }

  ///
  /// Name of this node as it appears in graphs
  std::string nodename() const { return "n"+jalib::XToString(_id); }

  void printDepsAndEnqueue(CodeGenerator& o, Transform& trans,  const RulePtr& rule, bool useDirections = true);

  ///
  /// Generate code for executing this node
  virtual void generateCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic) = 0;
  virtual void generateCodeForSlice(Transform& trans, CodeGenerator& o, int dimension, const FormulaPtr& pos, bool isStatic) = 0;


  virtual const MatrixDefPtr&    matrix() const = 0;
  virtual const SimpleRegionPtr& region() const = 0;
  virtual const ChoiceGridPtr& choices() const = 0;

  const ScheduleDependencies& directDepends() const   { return _directDepends; }
  const ScheduleDependencies& indirectDepends() const { return _indirectDepends; }

  ///
  /// Run one iteration to update indirectDepends, return count of changes
  int updateIndirectDepends();

  ScheduleNodeSet getStronglyConnectedComponent();

  void applyRemapping(const ScheduleNodeRemapping& map){
    ScheduleDependencies direct;
    ScheduleDependencies indirect;
    _directDepends.swap(direct);
    _indirectDepends.swap(indirect);
    for(ScheduleDependencies::iterator i=direct.begin();i!=direct.end();++i){
      if(map.find(i->first)==map.end())
        _directDepends[i->first]=i->second;
      else
        _directDepends[map.find(i->first)->second].merge(i->second);
    }
    for(ScheduleDependencies::iterator i=indirect.begin();i!=indirect.end();++i){
      if(map.find(i->first)==map.end())
        _indirectDepends[i->first]=i->second;
      else
        _indirectDepends[map.find(i->first)->second]=i->second;
    }
  }

  bool isInput() const { return _isInput; }
  void markInput() { _isInput = true; }
  void markLast()  { _isLast = true;  }

  std::string getChoicePrefix(Transform& t);
protected:
  int _id;
  bool _isInput;
  bool _isLast;
  ScheduleDependencies _directDepends;
  ScheduleDependencies _indirectDepends;
  int _choiceId;
};

class UnischeduledNode : public ScheduleNode {
public:
  UnischeduledNode(const MatrixDefPtr& m, const SimpleRegionPtr& r, const ChoiceGridPtr& choices)
    : _matrix(m), _region(r), _choices(choices)
  {}

  const MatrixDefPtr&    matrix()    const { return _matrix; }
  const SimpleRegionPtr& region()    const { return _region; }
  const ChoiceGridPtr& choices() const { return _choices; }

  void print(std::ostream& o) const {
    o << _matrix->name() << ".region(" << _region << ")";
  }

  virtual void generateCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic);
  virtual void generateCodeForSlice(Transform& trans, CodeGenerator& o, int dimension, const FormulaPtr& pos, bool isStatic);
private:
  MatrixDefPtr      _matrix;
  SimpleRegionPtr   _region;
  ChoiceGridPtr     _choices;
};

class CoscheduledNode : public ScheduleNode {
public:
  CoscheduledNode(const ScheduleNodeSet& set);

  const MatrixDefPtr&    matrix()  const { JASSERT(false); return MatrixDefPtr::null();    }
  const SimpleRegionPtr& region()  const { JASSERT(false); return SimpleRegionPtr::null(); }
  const ChoiceGridPtr&   choices() const { JASSERT(false); return ChoiceGridPtr::null();   }

  void print(std::ostream& o) const {
    o << "Coscheduled:";
    for(ScheduleNodeSet::const_iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i)
      o << "\\n " << **i;
  }
  void generateCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic);
  void generateCodeForSlice(Transform&, CodeGenerator&, int, const FormulaPtr&, bool){ UNIMPLEMENTED(); }
private:
  ScheduleNodeSet _originalNodes;
  int             _dimension;
};


/**
 * Contains an order to execute nodes in
 */
class Schedule {
  /** Intermediate state used while scheduling */
  struct SchedulingState {
    ScheduleNodeSet generated;
    ScheduleNodeSet pending;
  };
public:
  /** generate _schedule from a given set of inputs and outputs*/
  void initialize(const ScheduleNodeSet& inputs, const ScheduleNodeSet& outputs);
  
  void generateCodeStatic(Transform& trans, CodeGenerator& o);
  void generateCodeDynamic(Transform& trans, CodeGenerator& o);

  size_t size() const { return _schedule.size(); }
protected:
  void depthFirstScheduleNode(SchedulingState& state, ScheduleNode* n);  
private:
  //the ordering
  ScheduleNodeList _schedule;
};

class StaticScheduler : public jalib::JRefCounted, public jalib::JPrintable, public jalib::SrcPosTaggable {
public:
  ScheduleNodeSet lookupNode(const MatrixDefPtr& matrix, const SimpleRegionPtr& region);

  StaticScheduler(const ChoiceGridMap& cg);

  void markInputMatrix(const MatrixDefPtr& matrix){
    SimpleRegionPtr r = new SimpleRegion(matrix);
    ScheduleNodeList& regions = _matrixToNodes[matrix];
    JASSERT(regions.size()==0)(matrix).Text("Rules given for input matrix");
    regions.push_back(new UnischeduledNode(matrix, r, NULL));
    _allNodes.push_back(regions.back());
    _inputs.insert(regions.back().asPtr());
    regions.back()->markInput();
  }
  void markOutputMatrix(const MatrixDefPtr& matrix){
    ScheduleNodeList& lst = _matrixToNodes[matrix];
    JASSERT(lst.size()>0);
    for(ScheduleNodeList::iterator i=lst.begin(); i!=lst.end(); ++i)
      _outputs.insert(i->asPtr());
  }

  void renderGraph(const char* filename, const char* type="png") const;
  void writeGraph(const char* filename) const;
  void writeGraph(FILE* fd) const;

  void generateSchedule();

  void computeIndirectDependencies();
  void mergeCoscheduledNodes();
  
  //const ScheduleNodeList& schedule() const { return _schedule; }

  void print(std::ostream& o) const {
    o << "digraph {\n";
    for(ScheduleNodeList::const_iterator r=_allNodes.begin(); r!=_allNodes.end(); ++r){
      (*r)->printNode(o);
    }
    for(ScheduleNodeList::const_iterator r=_allNodes.begin(); r!=_allNodes.end(); ++r){
      (*r)->printEdges(o);
    }
    o << "}\n";
  }

  void generateCodeStatic(Transform& trans, CodeGenerator& o);
  void generateCodeDynamic(Transform& trans, CodeGenerator& o);
  void applyRemapping(const ScheduleNodeRemapping& m);

  size_t size() const { return _schedule.size(); }
private:
  //storage of nodes
  std::map<MatrixDefPtr, ScheduleNodeList > _matrixToNodes;
  ScheduleNodeList _allNodes;
  ScheduleNodeList _remappedNodes;

  ScheduleNodeSet _inputs;
  ScheduleNodeSet _outputs;

  //output schedule
  Schedule _schedule;
};

}

#endif
