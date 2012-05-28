/*****************************************************************************
 *  Copyright (C) 2008-2011 Massachusetts Institute of Technology            *
 *                                                                           *
 *  Permission is hereby granted, free of charge, to any person obtaining    *
 *  a copy of this software and associated documentation files (the          *
 *  "Software"), to deal in the Software without restriction, including      *
 *  without limitation the rights to use, copy, modify, merge, publish,      *
 *  distribute, sublicense, and/or sell copies of the Software, and to       *
 *  permit persons to whom the Software is furnished to do so, subject       *
 *  to the following conditions:                                             *
 *                                                                           *
 *  The above copyright notice and this permission notice shall be included  *
 *  in all copies or substantial portions of the Software.                   *
 *                                                                           *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY                *
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE               *
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      *
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   *
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   *
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    *
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE           *
 *                                                                           *
 *  This source code is part of the PetaBricks project:                      *
 *    http://projects.csail.mit.edu/petabricks/                              *
 *                                                                           *
 *****************************************************************************/
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
  
  //virtual const MatrixDefPtr&    matrix() const = 0;
  virtual const SimpleRegionPtr& region() const = 0;
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

  void pruneChoiceSpace();

private:
  std::vector<const RuleChoiceConsumer*> _ordering;
  std::vector<iterator> _invalidOrderings;
  std::map<const RuleChoiceConsumer*, const RuleChoiceConsumer*> _combinedChoices;
};

}

#endif
