/***************************************************************************
 *   Copyright (C) 2006-2009 by Jason Ansel                                *
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

#include <stdio.h>

#include "jtunable.h"

#include <fstream>
#include <string>
#include <algorithm>

#include "jconvert.h"
#include <math.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
  
jalib::JTunableModificationMonitor* jalib::JTunable::theModCallback = NULL;

jalib::JTunableManager& jalib::JTunableManager::instance(){ 
  static JTunableManager t; 
  return t; 
}

jalib::JTunableConfiguration jalib::JTunableManager::getCurrentConfiguration() const{
  JTunableConfiguration c;
  for(const_iterator i=begin(); i!=end(); ++i)
    c[*i] = (*i)->value();
  return c;
}

jalib::JTunableReverseMap jalib::JTunableManager::getReverseMap() const{
  JTunableReverseMap m;
  for(const_iterator i=begin(); i!=end(); ++i)
    m[(*i)->name()] = *i;
  return m;
}

void jalib::JTunableConfiguration::makeActive() const{
  for(const_iterator i=begin(); i!=end(); ++i){
    i->first->setValue(i->second);
  }
}

jalib::TunableValueMap jalib::JTunableManager::loadRaw(const std::string& filename){
  TunableValueMap rv;
  std::ifstream fp(filename.c_str());
  JASSERT(fp.is_open())(filename).Text("failed to open file");
  std::string line;
  while(getline(fp, line)){
    std::string t,l,r,c; 
    //PARSE PATTERN: <l> = <r> # <c>
    jalib::SplitFirst(t, c, line, '#');
    jalib::SplitFirst(l, r, t,    '=');
    l=jalib::StringTrim(l);
    r=jalib::StringTrim(r);
    c=jalib::StringTrim(c);
    if(!r.empty() && !l.empty()){
      if(StartsWith(c, "double") || StartsWith(c, "float") || Contains(r, '.')) {
        rv[l] = StringToX<double>(r);
      } else {
        rv[l] = StringToX<int>(r);
      }
    }
  }
  return rv;
}

void jalib::JTunableManager::load(const std::string& filename) const{
  const TunableValueMap f = loadRaw(filename);
  const JTunableReverseMap m = getReverseMap();
  TunableValueMap::const_iterator i;
  for(i=f.begin(); i!=f.end(); ++i){
    JTunableReverseMap::const_iterator t=m.find(i->first);
    JWARNING(t!=m.end())(i->first)(i->second).Text("unknown tunable parameter");
    if(t!=m.end()){
      t->second->setValue(i->second);
      t->second->verify();
    }
  }
}

void jalib::JTunableManager::save(const std::string& filename) const{
  std::ofstream of(filename.c_str());
  JASSERT(of.is_open())(filename).Text("failed to open file");
  for(const_iterator i=begin(); i!=end(); ++i){
    of << (*i)->name() << " = " << (*i)->value()
    << "     # " << (*i)->value().typestr() << ", valid range: "<< (*i)->min() << " to " << (*i)->max()
    << "\n";
  }
  of << std::flush;
}

double jalib::JTunableConfiguration::distanceTo(const JTunableConfiguration& /*that*/) const {
  UNIMPLEMENTED();
  return 0;
  /*
  double d=0;
  JASSERT(that.size()==this->size())(*this)(that);
  for(const_iterator a=begin(); a!=end(); ++a){
    const_iterator b=that.find(a->first);
    JASSERT(b!=that.end())(b->first)(*this)(that);
    d+=(a->second-b->second)*(a->second-b->second);
  }
  return sqrt(d);
  */
}


void jalib::JTunableConfiguration::print(std::ostream& o) const{
  for(const_iterator a=begin(); a!=end(); ++a){
    if(a!=begin()) o << ", ";
    o << a->second;
  }
}

void jalib::JTunableManager::reset() const {
    for(const_iterator i=begin(); i!=end(); ++i){
      (*i)->reset();
    }
  }

#ifdef HAVE_LIBGSL
#include <gsl/gsl_siman.h>

namespace { //file local

typedef jalib::JTunableConfiguration Cfg;
jalib::JConfigurationTester* theConfigTester;

#define P_HOP_THRESH 0.8

/* set up parameters for this simulated annealing run */
/* how many points do we try before stepping */
#define N_TRIES 100
/* how many iterations for each T? */
#define ITERS_FIXED_T 100
/* max step size in random walk */
#define STEP_SIZE 60
/* Boltzmann constant */
#define K 1.0
/* initial temperature */
#define T_INITIAL 0.008
/* damping factor for temperature */
#define MU_T 1.003 
#define T_MIN 2.0e-6

gsl_siman_params_t params = {N_TRIES, ITERS_FIXED_T, STEP_SIZE, K, T_INITIAL, MU_T, T_MIN};

// return the energy of a configuration xp.
double testEnergy(void *xp)
{
  Cfg* x = ((Cfg*)xp);
//   double e =  exp(-pow((x-1.0),2.0))*sin(8*x);
//   JTRACE("test energy")(x)(e);
  return theConfigTester->test(*x);
}

// modify the configuration xp using a random step taken from the generator r, up to a maximum distance of step_size.
void step(const gsl_rng * r, void *xp, double step_size)
{
  Cfg& cfg = *((Cfg*)xp);

  for(Cfg::iterator i=cfg.begin(); i!=cfg.end(); ++i){
    jalib::TunableValue& val = i->second;
    jalib::JTunable& tunable = *i->first;

    //make jump
    double rand = gsl_rng_uniform(r); // [0, 1] range
    rand=(rand*2.0)-1.0; // [-1, 1] range
    if(tunable.rangeLength() > step_size){
      val+= jalib::TunableValue(step_size*rand);
    }else{
      //step size is too big to handle this range, use alternate single-step algorithm
      if(rand >  P_HOP_THRESH) val++;
      if(rand < -P_HOP_THRESH) val--;
    }

    //wrap overunderflows
    if(val < tunable.min()) val = tunable.min();
    if(val > tunable.max()) val = tunable.max();
   }

//   double u = gsl_rng_uniform(r);
//   new_x = u * 2 * step_size - step_size + old_x;
//   JTRACE("step")(old_x)(new_x)(u)(step_size);
//   memcpy(xp, &new_x, sizeof(new_x));
}

// return the distance between two configurations xp and yp.
double getDistance(void *xp, void *yp)
{
  Cfg* x = ((Cfg*)xp);
  Cfg* y = ((Cfg*)yp);
  return x->distanceTo(*y);
}

// print the contents of the configuration xp.
void printCfg(void *xp)
{
  Cfg* x = ((Cfg*)xp);
  printf(" (%s)", x->toString().c_str());
}

void cfgCopy(void *source, void *dest){
  Cfg* s = ((Cfg*) source);
  Cfg* d = ((Cfg*) dest);
  *s=*d;
}

void* cfgCreate(void *xp){ 
  Cfg* x = ((Cfg*)xp);
  return new Cfg(*x); 
}

void cfgDestroy(void *xp){
  delete (Cfg*)xp;
}

}

void jalib::JTunableManager::autotune(JConfigurationTester* tester) const{
  theConfigTester = tester;

  const gsl_rng_type * T;
  gsl_rng * r;

  Cfg* initial = new JTunableConfiguration(JTunableManager::getCurrentConfiguration());

  gsl_rng_env_setup();
  T = gsl_rng_default;
  r = gsl_rng_alloc(T);

  gsl_siman_solve(r, initial, testEnergy, step, getDistance, printCfg,
                  cfgCopy, cfgCreate, cfgDestroy,
                  0, params);

  gsl_rng_free (r);

  initial->makeActive();
  delete initial;
}

#else//HAVE_LIBGSL

void jalib::JTunableManager::autotune(JConfigurationTester* /*tester*/) const{
  JASSERT(false).Text("Can't autotune because not compiled with -lgsl, run: 'apt-get install libgsl0-dev' then './configure'");
}

#endif//HAVE_LIBGSL
