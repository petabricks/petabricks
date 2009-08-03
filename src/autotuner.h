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
#ifndef PETABRICKSAUTOTUNER_H
#define PETABRICKSAUTOTUNER_H

#include "jtunable.h"
#include "jrefcounted.h"
#include "petabricksruntime.h"
#include <vector>
#include <algorithm>

namespace petabricks {
class Autotuner;
class CandidateAlgorithm;
typedef jalib::JRef<CandidateAlgorithm> CandidateAlgorithmPtr;
typedef std::vector<CandidateAlgorithmPtr> CandidateAlgorithmList;
typedef jalib::JRef<Autotuner> AutotunerPtr;

class CandidateAlgorithm : public jalib::JRefCounted, public jalib::JPrintable {
public:
  CandidateAlgorithm(int l, int a, jalib::JTunable* at, int c, jalib::JTunable* ct, const CandidateAlgorithmPtr& n)
    : _lvl(l), _alg(a), _algTunable(at), _cutoff(c), _cutoffTunable(ct), _nextLevel(n)
  {}
  
  void activate(){
    if(_algTunable)    _algTunable->setValue(_alg);
    if(_cutoffTunable) _cutoffTunable->setValue(_cutoff);
    if(_nextLevel)     _nextLevel->activate();
  }

  void addResult(double d) { _performance.push_back(d); }
  double lastResult() const { JASSERT(!_performance.empty()); return _performance.back(); }

  double lastlastResult() const { JASSERT(!_performance.empty()); return _performance.size()>1 ? _performance[_performance.size()-2] :_performance.back(); }

  void print(std::ostream& o) const {
    if(_cutoffTunable!=0){
      if(_nextLevel) _nextLevel->print(o);
      o << " #" << _cutoff;
    }
    if(_lvl==1)    o << "B" << _alg;
    else           o << " R" << _alg;
  }

  int alg() const { return _alg; }
  int lvl() const { return _lvl; }
  int cutoff() const { return _cutoff; }
  const CandidateAlgorithmPtr& next() const { return _nextLevel; }

  CandidateAlgorithmPtr attemptBirth(PetabricksRuntime& rt, Autotuner& at, double thresh);

  bool isDuplicate(const CandidateAlgorithmPtr& that);
private:
  int                   _lvl;
  int                   _alg;
  jalib::JTunable*      _algTunable;
  int                   _cutoff;
  jalib::JTunable*      _cutoffTunable;
  CandidateAlgorithmPtr _nextLevel;
  std::vector<double>   _performance; 
};

class Autotuner : public jalib::JRefCounted {
public:
  Autotuner(PetabricksRuntime& rt, PetabricksRuntime::Main* m, const std::string& prefix);
  jalib::JTunable* algTunable(int lvl);
  jalib::JTunable* cutoffTunable(int lvl);

  void runAll();

  //void train(int min, int max);

  void trainOnce();

  void printCanidates();

  void removeDuplicates();
  
  PetabricksRuntime::Main* main() const { return _main; }

  static bool isValidAlgChoiceSite(const std::string& prefix);
private:
  PetabricksRuntime&        _runtime;
  PetabricksRuntime::Main*  _main;
  CandidateAlgorithmList    _candidates;
  CandidateAlgorithmPtr     _initialConfig;
  jalib::JTunableReverseMap _tunableMap;
  std::string               _prefix;
  int                       _maxLevels;
};

}

#endif
