/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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
#ifndef PETABRICKSSTATICSCHEDULER_H
#define PETABRICKSSTATICSCHEDULER_H

#include "matrixdef.h"
#include "region.h"
#include "jrefcounted.h"
#include "choicegrid.h"

#include <vector>
#include <list>
#include <map>

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

class ScheduleNode : public jalib::JRefCounted, public jalib::JPrintable {
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
  void markInput(){ _isInput=true; }
  void markLast(){ _isLast=true; }
protected:
  int _id;
  bool _isInput;
  bool _isLast;
  ScheduleDependencies _directDepends;
  ScheduleDependencies _indirectDepends;
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

  const MatrixDefPtr&    matrix() const { JASSERT(false); return MatrixDefPtr::null(); }
  const SimpleRegionPtr& region() const { JASSERT(false); return SimpleRegionPtr::null(); }
  const ChoiceGridPtr& choices() const  { JASSERT(false); return ChoiceGridPtr::null(); }

  void print(std::ostream& o) const {
    o << "Coscheduled:";
    for(ScheduleNodeSet::const_iterator i=_originalNodes.begin(); i!=_originalNodes.end(); ++i)
      o << "\\n " << **i;
  }
  void generateCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic);
  void generateCodeForSlice(Transform& trans, CodeGenerator& o, int dimension, const FormulaPtr& pos, bool isStatic){ JASSERT(false); }
private:
  ScheduleNodeSet _originalNodes;
  int             _dimension;
};

class StaticScheduler : public jalib::JRefCounted, public jalib::JPrintable {
public:
  ScheduleNodeSet lookupNode(const MatrixDefPtr& matrix, const SimpleRegionPtr& region);

  StaticScheduler(const ChoiceGridMap& cg);

  void markInputMatrix(const MatrixDefPtr& matrix){
    SimpleRegionPtr r = new SimpleRegion(matrix);
    ScheduleNodeList& regions = _matrixToNodes[matrix];
    JASSERT(regions.size()==0)(matrix).Text("Rules given for input matrix");
    regions.push_back(new UnischeduledNode(matrix, r, NULL));
    _allNodes.push_back(regions.back());
    _generated.insert(regions.back().asPtr());
    regions.back()->markInput();
  }
  void markOutputMatrix(const MatrixDefPtr& matrix){
    ScheduleNodeList& lst = _matrixToNodes[matrix];
    JASSERT(lst.size()>0);
    for(ScheduleNodeList::iterator i=lst.begin(); i!=lst.end(); ++i)
      _goals.insert(i->asPtr());
    //TODO make sure no missing spots
  }

  void writeGraphAsPDF(const char* filename) const;

  void generateSchedule();

  void depthFirstSchedule(ScheduleNode* n);

  void computeIndirectDependencies();
  void mergeCoscheduledNodes();
  
  const ScheduleNodeList& schedule() const { return _schedule; }

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

  //intermediate state of depthFirstSchedule
  ScheduleNodeSet _goals;
  ScheduleNodeSet _generated;
  ScheduleNodeSet _pending;

  //output schedule
  ScheduleNodeList _schedule;
};

}

#endif
