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
#include "autotuner.h"
#include "petabricksruntime.h"
#include "common/jtimer.h"

#include <limits>
#include <algorithm>

JTUNABLE(autotune_alg_slots,                1, 1, 32);
JTUNABLE(autotune_branch_attempts,          1, 1, 32);
JTUNABLE(autotune_improvement_threshold,    98, 10, 100);
JTUNABLE(autotune_cutoff_divisor,           16, 2, 1024);


#define FIRST_DEATH_THRESH 0
#define POP_PERFORMANCE_CUTOFF 1.5

#define MAX_ALGS autotune_alg_slots
#define BIRTH_ATTEMPTS autotune_branch_attempts
#define BIRTH_THRESH  (autotune_improvement_threshold.value()/100.0)

static const int DUP_CUTOFF_THRESH = 1; // how different cutoffs must be to be duplicates

namespace{ //file local
  struct CmpLastPerformance {
    bool operator() (const petabricks::CandidateAlgorithmPtr& a, const petabricks::CandidateAlgorithmPtr& b){
      return a->lastResult() < b->lastResult();
    }
  };

  struct CmpLastLastPerformance {
    bool operator() (const petabricks::CandidateAlgorithmPtr& a, const petabricks::CandidateAlgorithmPtr& b){
      return a->lastlastResult() < b->lastlastResult();
    }
  };

  struct CmpAlgType {
    bool operator() (const petabricks::CandidateAlgorithmPtr& a, const petabricks::CandidateAlgorithmPtr& b){
      if(a->lvl() != b->lvl()) return a->lvl() < b->lvl();
      petabricks::ConstCandidateAlgorithmPtr ta=a.asPtr(), tb=b.asPtr();
      while(ta && tb){
        if(ta->alg() != tb->alg()) return ta->alg() < tb->alg();
        ta=ta->next();
        tb=tb->next();
      }
      return a->cutoff() < b->cutoff();
    }
  };

  std::string _mktname(int lvl, const std::string& prefix, const std::string& type){
    return prefix + "_lvl" + jalib::XToString(lvl) + "_" + type;
  }
}

jalib::JTunable* petabricks::Autotuner::algTunable(int lvl){
  return _tunableMap[_mktname(lvl, _prefix, "rule")];
}
jalib::JTunable* petabricks::Autotuner::cutoffTunable(int lvl){
  return _tunableMap[_mktname(lvl, _prefix, "cutoff")];
}

petabricks::Autotuner::Autotuner(PetabricksRuntime& rt, PetabricksRuntime::Main* m, const std::string& prefix, const std::vector<std::string>& extraCutoffNames, const char* logfile)
  : _runtime(rt)
  , _main(m)
  , _tunableMap(jalib::JTunableManager::instance().getReverseMap())
  , _prefix(prefix)
  , _log(logfile)
{
  using jalib::JTunable;
  _maxLevels=0; 

  ExtraCutoffList extraCutoffs;
  extraCutoffs.reserve(extraCutoffNames.size());
  for(size_t i=0; i<extraCutoffNames.size(); ++i){
    extraCutoffs.push_back(_tunableMap[extraCutoffNames[i]]);
    JASSERT(extraCutoffs.back()!=NULL)(extraCutoffNames[i]);
    _log.addTunable(extraCutoffs.back());
    
    _initialConfig = new CandidateAlgorithm(0, 0, 0, extraCutoffs.back()->max(), extraCutoffs.back(), _initialConfig.asPtr(), extraCutoffs);
  }

  //find numlevels
  for(int lvl=2; true; ++lvl){
    JTunable* rule   = algTunable(lvl);
    JTunable* cutoff = cutoffTunable(lvl);
    if(rule==0 && cutoff==0){
      _maxLevels = lvl-1;
      break;
    }
  }

  JASSERT(algTunable(1)!=NULL || cutoffTunable(2)!=NULL || _prefix=="")(prefix).Text("invalid prefix to autotune");

  //make initialconfig (all level disabled)
  for(int lvl=1; lvl<=_maxLevels; ++lvl){
    JTunable* at   = algTunable(lvl);
    JTunable* ct = cutoffTunable(lvl);
    int a=0, c=std::numeric_limits<int>::max();
    if(ct!=0) c=ct->max();
    _initialConfig = new CandidateAlgorithm(lvl, a, at, c, ct, _initialConfig.asPtr(), extraCutoffs);
    _log.addTunable(at);
    _log.addTunable(ct);
  }

  //add 1 level candidates
  CandidateAlgorithmList lvl1Candidates;
  {
    JTunable* at = algTunable(1);
    JTunable* ct = cutoffTunable(1);
    int a=0, c=1;
    if(at==0){
      lvl1Candidates.push_back(new CandidateAlgorithm(1, a, at, c, ct, NULL, extraCutoffs));
    }else{
      for(a=at->min(); a<=at->max(); ++a){
        lvl1Candidates.push_back(new CandidateAlgorithm(1, a, at, c, ct, NULL, extraCutoffs));
      }
    }
  }

  //add 2 level candidates
  CandidateAlgorithmList lvl2Candidates;
#ifdef TWO_LEVEL_CANDIDATES
  {
    JTunable* at = algTunable(2);
    JTunable* ct = cutoffTunable(2);
    int a=0, c=1;
    if(at==0){
      for(CandidateAlgorithmList::const_iterator i=lvl1Candidates.begin(); i!=lvl1Candidates.end(); ++i)
        lvl2Candidates.push_back(new CandidateAlgorithm(2, a, at, c, ct, i->asPtr(), extraCutoffs));
    }else{
      for(a=at->min(); a<=at->max(); ++a){
        for(CandidateAlgorithmList::const_iterator i=lvl1Candidates.begin(); i!=lvl1Candidates.end(); ++i)
          if(a!=(*i)->alg())
            lvl2Candidates.push_back(new CandidateAlgorithm(2, a, at, c, ct, i->asPtr(), extraCutoffs));
      }
    }
  }
#endif

  //JTRACE("Autotuner constructed")(lvl1Candidates.size())(lvl2Candidates.size());
  _candidates.swap(lvl1Candidates);
  _candidates.insert(_candidates.end(), lvl2Candidates.begin(), lvl2Candidates.end());

  _log.logHeader();
}


void petabricks::Autotuner::runAll(double timelimit){
  for(CandidateAlgorithmList::iterator i=_candidates.begin(); i!=_candidates.end(); ++i){
    double d = (*i)->run(_runtime, *this, timelimit , true);
    std::cout << std::flush;
    timelimit=std::min(timelimit, std::max(d*POP_PERFORMANCE_CUTOFF, 1.0));
  }
}

bool petabricks::Autotuner::trainOnce(double limit){
  std::cout << "BEGIN ITERATION " << _prefix << " / " << _runtime.curSize() <<  " (in " << _main->name() << ")" << std::endl;
  runAll(limit);

  //get the best performance for this round
  std::sort(_candidates.begin(), _candidates.end(), CmpLastPerformance());
  double bestPerf = _candidates[0]->lastResult();
  std::sort(_candidates.begin(), _candidates.end(), CmpLastLastPerformance());


  // add new algorithms -- by last rounds performance
  int numCurrentCandidates = _candidates.size();
  for(int i=0; i<BIRTH_ATTEMPTS && i<numCurrentCandidates; ++i){
    _initialConfig->activate();
    CandidateAlgorithmPtr b=_candidates[i]->attemptBirth(_runtime, *this, bestPerf*BIRTH_THRESH);
    if(b){
      bestPerf = std::min(bestPerf,b->lastResult());
      _candidates.push_back(b);
    }
  }
  
  if(bestPerf > std::numeric_limits<double>::max()/2){
    //test ran too slowly, abort
    _initialConfig->activate();
    _candidates[0]->activate();
    return false;
  }

  removeDuplicates();

  std::ostringstream removed;
  std::sort(_candidates.begin(), _candidates.end(), CmpLastPerformance());
  //kill slowest algorithms
  for(int i=_candidates.size()-1; i>0; --i){
      if(_candidates[i]->lastResult() > std::numeric_limits<double>::max()/2
        || (i>=MAX_ALGS && _candidates[i]->lastResult() > FIRST_DEATH_THRESH )){
        removed << "  REMOVE " << _candidates[i] << ' ' << _candidates[i]->lastResult() << std::endl;
        _candidates.pop_back();
      }else break;
  }

  printCanidates();

  std::cout << removed.str();

  //reset config
  _initialConfig->activate();
  _candidates[0]->activate();
  return true;
}

void petabricks::Autotuner::printCanidates(){
  int slot=0;
  for(CandidateAlgorithmList::iterator i=_candidates.begin(); i!=_candidates.end(); ++i){
    std::cout << "SLOT[" << slot++ << "] ";
    if((*i)->numResults()==1)
      std::cout << "ADD    " ;
    else
      std::cout << "KEEP   " ;
    std::cout << jalib::StringPad((*i)->toString(),20) << " = " << (*i)->lastResult() << std::endl;
  }
}

void petabricks::Autotuner::removeDuplicates(){
  std::sort(_candidates.begin(), _candidates.end(), CmpAlgType());
  //kill duplicates
  for(int i=0; i<(int)_candidates.size()-1; ++i){
    if(_candidates[i]->isDuplicate(_candidates[i+1])){
      if(_candidates[i]->lastResult() > _candidates[i+1]->lastResult()){
        std::cout << "  DUPLICATE " << _candidates[i] << std::endl;
        _candidates.erase(_candidates.begin()+i);
      }else{
        std::cout << "  DUPLICATE " << _candidates[i+1] << std::endl;
        _candidates.erase(_candidates.begin()+i+1);
      }
      --i; //redo this iteration
    }
  }
}

double petabricks::CandidateAlgorithm::run(PetabricksRuntime& rt, Autotuner& autotuner, double thresh, bool inPop){
  jalib::JTime begin = jalib::JTime::now();
  if(inPop)
    std::cout << "  * TEST ";
  else
    std::cout << "  * TRY  ";
  std::cout << *this << " (limit "<<thresh<<") = " << std::flush;
  autotuner.resetConfig();
  activate();
  jalib::JTunable::setModificationCallback(this);
  double d = rt.runTrial(thresh, true);
  jalib::JTunable::setModificationCallback();
  addResult(d);
  autotuner.log().logLine(rt.curSize(), d, inPop);
  std::cout << d << " (cost " << (jalib::JTime::now()-begin) << ")" << std::endl;
  return d;
}

petabricks::CandidateAlgorithmPtr petabricks::CandidateAlgorithm::attemptBirth(PetabricksRuntime& rt, Autotuner& autotuner, double thresh) const {
  CandidateAlgorithmList possible;
  int newCutoff = rt.curSize() * 3 / 4;
  
  //algorithmic candidates
  jalib::JTunable* at = autotuner.algTunable(_lvl+1);
  jalib::JTunable* ct = autotuner.cutoffTunable(_lvl+1);
  if(ct!=0 && newCutoff > 1){
    int amin=0,amax=0;
    if(at!=0){
      amin=at->min();
      amax=at->max();
    }
    for(int a=amin; a<=amax; ++a){
      if(/*_lvl>1 &&*/ a==lastalg()) continue;
      if(at!=0) at->setValue(a);
      possible.push_back(new CandidateAlgorithm(_lvl+1, a, at, newCutoff, ct, this, _unusedCutoffs));
    }
  }

  if(newCutoff>1){
    // candidates from _unusedCutoffs (sequential, blocking, etc)
    for(size_t i=0; i<_unusedCutoffs.size(); ++i){
      ExtraCutoffList remaining = _unusedCutoffs; 
      remaining.erase(remaining.begin()+i);
      possible.push_back(new CandidateAlgorithm(_lvl, -1, NULL, newCutoff, _unusedCutoffs[i], this, remaining));
    }
  }

  newCutoff = rt.curSize()/autotune_cutoff_divisor;
  if(newCutoff>1){
    // candidates from _unusedCutoffs (sequential, blocking, etc), lower cutoff point
    for(size_t i=0; i<_unusedCutoffs.size(); ++i){
      ExtraCutoffList remaining = _unusedCutoffs; 
      remaining.erase(remaining.begin()+i);
      possible.push_back(new CandidateAlgorithm(_lvl, -1, NULL, newCutoff, _unusedCutoffs[i], this, remaining));
    }
  }
      
  if(possible.empty())
    return NULL;

  //run them all
  for(CandidateAlgorithmList::iterator i=possible.begin(); i!=possible.end(); ++i){
    (*i)->run(rt, autotuner, thresh, false);
  }

  //sort by performance
  std::sort(possible.begin(), possible.end(), CmpLastPerformance());


  //see if fastest is good enough
  if(possible[0]->lastResult() > thresh)
    return NULL;

  return possible[0];
}

bool petabricks::CandidateAlgorithm::isDuplicate(const ConstCandidateAlgorithmPtr& that) const{
  if(!that) return false;
  if(_lvl != that->lvl()) return false;
  if(_alg != that->alg()) return false;
  if(std::abs(_cutoff - that->cutoff()) > DUP_CUTOFF_THRESH) return false;
  if(_nextLevel) return _nextLevel->isDuplicate(that->next());
  return true;
}
  
petabricks::CandidateAlgorithm::CandidateAlgorithm( int l
                  , int a
                  , jalib::JTunable* at
                  , int c
                  , jalib::JTunable* ct
                  , const ConstCandidateAlgorithmPtr& n
                  , const ExtraCutoffList& unusedCutoffs)
  : _lvl(l)
  , _alg(a)
  , _algTunable(at)
  , _cutoff(c)
  , _cutoffTunable(ct)
  , _nextLevel(n)
  , _unusedCutoffs(unusedCutoffs)
{}

void petabricks::CandidateAlgorithm::activate() const {
  if(_nextLevel)     _nextLevel->activate();
  if(_algTunable)    _algTunable->setValue(_alg);
  if(_cutoffTunable) _cutoffTunable->setValue(_cutoff);
  _extraConfig.makeActive();
}

std::string _shortenTunableName(const std::string& s){
  if(jalib::Contains(s, '_'))
    return std::string(s.begin()+s.rfind('_')+1, s.end());
  else
    return s;
}

void petabricks::CandidateAlgorithm::print(std::ostream& o) const {
  if(_cutoffTunable!=0){
    if(_nextLevel) _nextLevel->print(o);
    o << " #" << _shortenTunableName(_cutoffTunable->name()) << '=' << _cutoff;
  }
  if(_algTunable){
    o << " " << _shortenTunableName(_algTunable->name()) << "=" << _alg;
  }
  if(_extraConfig.size()>0){
    o << " (*" << _extraConfig.size() << ")";
  }
}

void petabricks::CandidateAlgorithm::onTunableModification(jalib::JTunable* tunable, jalib::TunableValue /*oldVal*/, jalib::TunableValue newVal){
  _extraConfig[tunable] = newVal;
}


