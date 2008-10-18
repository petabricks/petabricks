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

hecura::StaticScheduler::StaticScheduler(const ChoiceGridMap& cg){
  for(ChoiceGridMap::const_iterator m=cg.begin(); m!=cg.end(); ++m){
    ScheduleNodeList& regions = _nodes[m->first];
    for(ChoiceGridIndex::const_iterator i=m->second.begin(); i!=m->second.end(); ++i){
      SimpleRegionPtr tmp = new SimpleRegion(i->first);
//       tmp->offsetMaxBy(FormulaInteger::negOne());
      regions.push_back(new ScheduleNode(m->first, tmp, i->second));
    }
  }
}

hecura::ScheduleNodeSet hecura::StaticScheduler::lookupNode(const MatrixDefPtr& matrix, const SimpleRegionPtr& region){
  ScheduleNodeSet rv;
  ScheduleNodeList& regions = _nodes[matrix];
  for(ScheduleNodeList::iterator i=regions.begin(); i!=regions.end(); ++i){
    if(region->toString() == (*i)->region()->toString()){
      rv.insert(i->asPtr());
//       JTRACE("region lookup break")(region)((*i)->region());
      break; //optimization
    }
    if((*i)->region()->hasIntersect(region)){
      rv.insert(i->asPtr());
    }
  }
//   JTRACE("region lookup COMPLETE")(matrix)(region)(rv.size());
  JASSERT(rv.size()>0)(matrix)(region).Text("failed to find rule for region");
  return rv;
}

void hecura::StaticScheduler::generateSchedule(){
  for( std::set<ScheduleNode*>::const_iterator i=_goals.begin()
      ; i!=_goals.end()
      ; ++i )
  {
    generateSchedule(*i);
  }
}

void hecura::StaticScheduler::generateSchedule(ScheduleNode* n){
  if(_generated.find(n)!=_generated.end())
    return;

  JASSERT(_pending.find(n)==_pending.end()).Text("dependency cycle");
  _pending.insert(n);

  for( ScheduleNodeSet::const_iterator i=n->dependsOn().begin()
      ; i!=n->dependsOn().end()
      ; ++i)
  {
    if(*i != n)
      generateSchedule(*i);
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
}

void hecura::ScheduleNode::generateCodeSimple(Transform& trans, CodeGenerator& o){
  RuleChoicePtr rule = trans.learner().makeRuleChoice(_choices->rules(), _matrix, _region);
  rule->generateCodeSimple(_region, o);
}


