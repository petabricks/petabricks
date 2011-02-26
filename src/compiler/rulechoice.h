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
#ifndef PETABRICKSRULECHOICE_H
#define PETABRICKSRULECHOICE_H

#include "formula.h"
#include "rule.h"

namespace petabricks {
class CodeGenerator;
class RuleChoice;
class ScheduleNode;
typedef jalib::JRef<RuleChoice> RuleChoicePtr;

class RuleChoiceConsumer {
public:
  virtual ~RuleChoiceConsumer() {}
  virtual const RuleSet& choices() const = 0;
};

class RuleChoiceAssignment : public std::map<const RuleChoiceConsumer*, RulePtr>{
};

class RuleChoiceCollection {
public:
  typedef int const_iterator;
  typedef const_iterator iterator;
  iterator begin() { return 0; }
  const_iterator begin() const { return 0; }
  iterator end() { return size(); }
  const_iterator end() const { return size(); }

  void addConsumer(const RuleChoiceConsumer* c) {
    _ordering.push_back(c);
  }

  size_t size() const;

  RuleChoiceAssignment getAssignment(size_t choice) const;


  void markInvalid(const iterator& i) { _invalidOrderings.push_back(i); }

  void generateDecisionTree(std::string& prefix, size_t choiceCount, CodeGenerator& o);

private:
  std::vector<const RuleChoiceConsumer*> _ordering;
  std::vector<iterator> _invalidOrderings;
};

}

#endif
