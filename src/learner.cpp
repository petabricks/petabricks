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
#include "learner.h"

hecura::RuleChoicePtr hecura::Learner::makeRuleChoice( const RuleSet& choices
                                                     , const MatrixDefPtr&
                                                     , const SimpleRegionPtr& )
{
  /*
   * This function must build a stack of RuleChoicePtr's from choices.
   * The field RuleChoice::_next forms a linked list of choices.
   * At runtime the first choice for which RuleChoice::_condition holds is used.
   */

  RuleChoicePtr rv = new RuleChoice(*choices.begin()); //the first rule
  return rv;
}
