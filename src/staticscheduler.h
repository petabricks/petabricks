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
#ifndef HECURASTATICSCHEDULER_H
#define HECURASTATICSCHEDULER_H

#include "matrixdef.h"
#include "region.h"
#include "jrefcounted.h"
#include "choicegrid.h"

#include <vector>
#include <list>
#include <map>

namespace hecura {
class Transform;
class CodeGenerator;
class ScheduleNode;
typedef  std::set<ScheduleNode*> ScheduleNodeSet;
typedef std::vector< jalib::JRef<ScheduleNode> > ScheduleNodeList; 

class ScheduleNode : public jalib::JRefCounted{
public:
  ScheduleNode(const MatrixDefPtr& m, const SimpleRegionPtr& r, const ChoiceGridPtr& choices)
    : _matrix(m), _region(r), _choices(choices)
  {
    static int i=0;
    _id=i++;
  }
  
  void mergeRegion(const SimpleRegionPtr& r){
    _region = _region->regionUnion(r);
  }

  void addDependency(ScheduleNode* n){
    _dependsOn.insert(n);
    //JTRACE("Added dep")(_matrix)(n->_matrix);
  }
  
  const MatrixDefPtr&    matrix()    const { return _matrix; }
  const SimpleRegionPtr& region()    const { return _region; }
  const ScheduleNodeSet& dependsOn() const { return _dependsOn; }

  std::string nodename() const { return "n"+jalib::XToString(_id); }

  void printNode(std::ostream& os) const{
    os << "  " << nodename() << "[label=\"" 
       << _matrix->name() << ".region(" << _region << ")"
       << "\"];\n";
  }
  void printEdges(std::ostream& os) const{
    for(ScheduleNodeSet::const_iterator i=_dependsOn.begin(); i!=_dependsOn.end(); ++i){
      os << "  " << (*i)->nodename() << " -> " << nodename() << "[ label=\"" << "\"];\n";
    }
  }

  void generateCodeSimple(Transform& trans, CodeGenerator& o);
  
private:
  int               _id;
  MatrixDefPtr      _matrix;
  SimpleRegionPtr   _region;
  ScheduleNodeSet   _dependsOn;
  ChoiceGridPtr     _choices;
};

class StaticScheduler : public jalib::JPrintable {
public:
  ScheduleNodeSet lookupNode(const MatrixDefPtr& matrix, const SimpleRegionPtr& region);

  StaticScheduler(const ChoiceGridMap& cg);

  void markInputMatrix(const MatrixDefPtr& matrix){
    SimpleRegionPtr r = new SimpleRegion(matrix);
    ScheduleNodeList& regions = _nodes[matrix];
    JASSERT(regions.size()==0)(matrix).Text("Rules given for input matrix");
    regions.push_back(new ScheduleNode(matrix, r, NULL));
    _generated.insert(regions.back().asPtr());
  }
  void markOutputMatrix(const MatrixDefPtr& matrix){
    ScheduleNodeList& lst = _nodes[matrix];
    JASSERT(lst.size()>0);
    for(ScheduleNodeList::iterator i=lst.begin(); i!=lst.end(); ++i)
      _goals.insert(i->asPtr());
    //TODO make sure no missing spots
  }

  void generateSchedule();

  void generateSchedule(ScheduleNode* n);
  
  const ScheduleNodeList& schedule() const { return _schedule; }

  void print(std::ostream& o) const {
    o << "digraph {\n";
    for( std::map<MatrixDefPtr, ScheduleNodeList >::const_iterator i=_nodes.begin()
       ; i!=_nodes.end()
       ; ++i )
    {
      const ScheduleNodeList& regions = i->second;
      for(ScheduleNodeList::const_iterator r=regions.begin(); r!=regions.end(); ++r){
        (*r)->printNode(o);
      }
    }
    for( std::map<MatrixDefPtr, ScheduleNodeList >::const_iterator i=_nodes.begin()
       ; i!=_nodes.end()
       ; ++i )
    {
      const ScheduleNodeList& regions = i->second;
      for(ScheduleNodeList::const_iterator r=regions.begin(); r!=regions.end(); ++r){
        (*r)->printEdges(o);
      }
    }
    o << "}\n";
  }

  void generateCodeSimple(Transform& trans, CodeGenerator& o);
private:
  std::map<MatrixDefPtr, ScheduleNodeList > _nodes;
  ScheduleNodeSet _generated;
  ScheduleNodeSet _pending;
  ScheduleNodeSet _goals;
  ScheduleNodeList _schedule;
};

}

#endif
