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

#include "petabricksruntime.h"

#include "common/jrefcounted.h"
#include "common/jtunable.h"

#include <vector>
#include <algorithm>

namespace petabricks {
class Autotuner;
class CandidateAlgorithm;
typedef jalib::JRef<CandidateAlgorithm> CandidateAlgorithmPtr;
typedef jalib::JRef<const CandidateAlgorithm> ConstCandidateAlgorithmPtr;
typedef std::vector<CandidateAlgorithmPtr> CandidateAlgorithmList;
typedef jalib::JRef<Autotuner> AutotunerPtr;

class CandidateAlgorithm : public jalib::JRefCounted
                         , public jalib::JPrintable
                         , public jalib::JTunableModificationMonitor 
{
public:
  CandidateAlgorithm( int l
                    , int a
                    , jalib::JTunable* at
                    , int c
                    , jalib::JTunable* ct
                    , const ConstCandidateAlgorithmPtr& n
                    , const std::vector<std::string>& extraCutoffs)
    : _lvl(l)
    , _alg(a)
    , _algTunable(at)
    , _cutoff(c)
    , _cutoffTunable(ct)
    , _nextLevel(n)
  {}
  
  void activate() const {
    if(_algTunable)    _algTunable->setValue(_alg);
    if(_cutoffTunable) _cutoffTunable->setValue(_cutoff);
    if(_nextLevel)     _nextLevel->activate();
    _extraConfig.makeActive();
  }

  void addResult(double d) { _performance.push_back(d); }
  double lastResult() const {
    JASSERT(!_performance.empty());
    return _performance.back(); 
  }

  double lastlastResult() const { 
    JASSERT(!_performance.empty()); 
    return _performance.size()>1 ? _performance[_performance.size()-2] :_performance.back(); 
  }

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
  const ConstCandidateAlgorithmPtr& next() const { return _nextLevel; }

  CandidateAlgorithmPtr attemptBirth(PetabricksRuntime& rt, Autotuner& at, double thresh) const;

  bool isDuplicate(const CandidateAlgorithmPtr& that) const{ 
    return isDuplicate(ConstCandidateAlgorithmPtr(that.asPtr()));
  }
  bool isDuplicate(const ConstCandidateAlgorithmPtr& that) const;

  void onTunableModification(jalib::JTunable* tunable, jalib::TunableValue oldVal, jalib::TunableValue newVal){
    JTRACE("user modified tunable")(tunable->name())(oldVal)(newVal);
    _extraConfig[tunable] = newVal;
  }

  double run(PetabricksRuntime& rt, Autotuner& at, double thresh);
private:
  int                          _lvl;
  int                          _alg;
  jalib::JTunable*             _algTunable;
  int                          _cutoff;
  jalib::JTunable*             _cutoffTunable;
  ConstCandidateAlgorithmPtr   _nextLevel;
  std::vector<double>          _performance; 
  jalib::JTunableConfiguration _extraConfig;
  std::vector<std::string>     _unusedCutoffs; 
};

class Autotuner : public jalib::JRefCounted {
public:
  Autotuner(PetabricksRuntime& rt, PetabricksRuntime::Main* m, const std::string& prefix, const std::vector<std::string>& extraCutoffs);
  jalib::JTunable* algTunable(int lvl);
  jalib::JTunable* cutoffTunable(int lvl);

  void runAll();

  //void train(int min, int max);

  void trainOnce();

  void printCanidates();

  void removeDuplicates();
  
  PetabricksRuntime::Main* main() const { return _main; }

  static bool isValidAlgChoiceSite(const std::string& prefix);

  void resetConfig() { _initialConfig->activate(); }
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
