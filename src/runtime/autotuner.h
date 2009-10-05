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

#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>

namespace petabricks {
class Autotuner;
class CandidateAlgorithm;
typedef jalib::JRef<CandidateAlgorithm> CandidateAlgorithmPtr;
typedef jalib::JRef<const CandidateAlgorithm> ConstCandidateAlgorithmPtr;
typedef std::vector<CandidateAlgorithmPtr> CandidateAlgorithmList;
typedef jalib::JRef<Autotuner> AutotunerPtr;
typedef std::vector<jalib::JTunable*> TunableList;
typedef TunableList ExtraCutoffList;
  
#define COL '\t'
#define ROW '\n'

/**
 * Log of autotuner actions
 */
class ATLogger {
public:
  ATLogger(const char* filename)
    : _of(filename)
  {
    JASSERT(_of.is_open())(filename);
    _of.precision(15);
    _of << std::showpoint;
  }
  void addTunable(jalib::JTunable* t){
    if(t!=NULL)
      _tunables.push_back(t);
  }

  void logHeader() {
    _of << "N" << COL;
    _of << "InPopulation" << COL;
    for(TunableList::const_iterator i=_tunables.begin(); i!=_tunables.end(); ++i){
      _of << (*i)->name() << COL;
    }
    _of << "Time" << ROW;
  }
  void logLine(int n, double time, bool inPop) {
    _of << n << COL;
    _of << inPop << COL;
    for(TunableList::const_iterator i=_tunables.begin(); i!=_tunables.end(); ++i){
      _of << (*i)->value() << COL;
    }
    _of << std::showpoint << time << ROW << std::flush;
  }
private:
  TunableList _tunables;
  std::ofstream _of;
};

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
                    , const ExtraCutoffList& unusedCutoffs);
  
  void activate() const;

  void addResult(double d) { _performance.push_back(d); }
  double lastResult() const {
    JASSERT(!_performance.empty());
    return _performance.back(); 
  }

  size_t numResults() const {
    return _performance.size();
  }

  double lastlastResult() const { 
    JASSERT(!_performance.empty()); 
    return _performance.size()>1 ? _performance[_performance.size()-2] :_performance.back(); 
  }

  void print(std::ostream& o) const;

  int lastalg() const {
    if(_algTunable) return _alg; 
    if(_nextLevel) return _nextLevel->lastalg(); 
    return -1;
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

  void onTunableModification(jalib::JTunable* tunable, jalib::TunableValue oldVal, jalib::TunableValue newVal);

  double run(PetabricksRuntime& rt, Autotuner& at, double thresh, bool inPop);
private:
  int                          _lvl;
  int                          _alg;
  jalib::JTunable*             _algTunable;
  int                          _cutoff;
  jalib::JTunable*             _cutoffTunable;
  ConstCandidateAlgorithmPtr   _nextLevel;
  std::vector<double>          _performance; 
  jalib::JTunableConfiguration _extraConfig;
  ExtraCutoffList              _unusedCutoffs; 
};

class Autotuner : public jalib::JRefCounted {
public:
  Autotuner(PetabricksRuntime& rt, PetabricksRuntime::Main* m, const std::string& prefix, const std::vector<std::string>& extraCutoffs, const char* logfile = "/dev/null");
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

  ATLogger& log() { return _log; }

  double lastBestResult() const {
    if(_candidates.empty()) return 0.0;
    return _candidates[0]->lastResult();
  }
private:
  PetabricksRuntime&        _runtime;
  PetabricksRuntime::Main*  _main;
  CandidateAlgorithmList    _candidates;
  CandidateAlgorithmPtr     _initialConfig;
  jalib::JTunableReverseMap _tunableMap;
  std::string               _prefix;
  int                       _maxLevels;
  ATLogger                  _log;
};

}

#endif
