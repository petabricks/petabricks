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
#ifndef JALIBSRCPOS_H
#define JALIBSRCPOS_H

#include "jprintable.h"
#include "jrefcounted.h"

#include <iostream>
#include <sstream>
#include <string>
#include <vector>


namespace jalib { class SrcPosTaggable; }

//in global scope, so one can run _lexicalSrcPos() anywhere and not get an error
const jalib::SrcPosTaggable* _lexicalSrcPos();

typedef const jalib::SrcPosTaggable* (*_lexicalSrcPosT)();

// Disable lexical source position guessing in a scope
#define DISABLESRCPOS() _lexicalSrcPosT _lexicalSrcPos = ::_lexicalSrcPos

// Query lexical source position
#define SRCPOS() _lexicalSrcPos()

// Register the current source position on the source position stack
#define SRCPOSSCOPE() jalib::SrcPosScopeHelper _srcPosScope(jalib::SrcPosStack::global(), _lexicalSrcPos())

namespace jalib {

class SrcPos;
typedef JRef<const SrcPos> SrcPosPtr;

class SrcPosStack : public std::vector<SrcPosPtr> {
public:
  /** Global stack to represent the source position of the current callframe */
  static SrcPosStack& global();
};

/** A single source position */
struct FileLineCol {
  std::string filename;
  int line;
  int column;
  FileLineCol(const std::string& f="", int l = -1, int c = -1)
    : filename(f), line(l), column(c)
  {}
  bool isFullyDefined() { return filename.length()>0 && line>=0 && column>=0; }
  FileLineCol fileline() { return FileLineCol(filename, line); }

  FileLineCol operator+(int i) const { return FileLineCol(filename,line+i); }
  FileLineCol operator-(int i) const { return FileLineCol(filename,line-i); }
  friend bool operator< (const FileLineCol& a, const FileLineCol& b) {
    if(a.filename!=b.filename)
      return a.filename<b.filename;
    if(a.line!=b.line)
      return a.line<b.line;
    return a.column<b.column;
  }
  bool sameFile(const FileLineCol& b) const { return filename==b.filename; }

  bool isNull() const {
    return filename.empty() && line<=0;
  }
};

/** A range of source positions */
class SrcPos : public JRefCounted, public JPrintable {
public:
  SrcPos(const std::string& file, int line, int col1, int col2, bool isAuto=false)
    : _first(FileLineCol(file,line,col1))
    , _last(FileLineCol(file,line,col2))
    , _isAutotagged(isAuto)
  {}
  SrcPos(const FileLineCol& first, const FileLineCol& last, bool isAuto=false)
    : _first(first), _last(last), _isAutotagged(isAuto)
  {}
  SrcPos(const FileLineCol& first, bool isAuto=false)
    : _first(first), _last(first), _isAutotagged(isAuto)
  {}
  SrcPos(const SrcPos& that, bool isAuto)
    : _first(that.first()), _last(that.last()), _isAutotagged(isAuto)
  {}
  SrcPos(bool isAuto=false)
    : _isAutotagged(isAuto)
  {}

  void print(std::ostream& o) const;

  FileLineCol& first() { return _first; }
  FileLineCol& last() { return _last; }
  const FileLineCol& first() const { return _first; }
  const FileLineCol& last() const { return _last; }

  bool isAutoTagged() const { return _isAutotagged; }
  bool isNull() const { return _first.isNull() && _last.isNull(); }

  SrcPosPtr clone() const            { return new SrcPos(*this); }
  SrcPosPtr clone(bool isAuto) const { return new SrcPos(*this, isAuto); }


private:
  FileLineCol _first;
  FileLineCol _last;
  bool        _isAutotagged;
};


/** Base class for objects that can be tagged with source positions */
class SrcPosTaggable {
public:
  SrcPosTaggable();
  void tagPosition(const SrcPosPtr& p) { if(p) _positions.push_back(p); }
  void clearPosition() { _positions.clear(); }
  bool srcPos(std::ostream& os) const;
  std::string srcPos() const;
  SrcPosPtr srcPosFirst() const;
  const SrcPosTaggable* _lexicalSrcPos() const { return this; }
private:
  mutable std::vector<SrcPosPtr> _positions;
};


/**
 * When constructed push pos onto stack
 * When destructed remove it from the stack
 */
class SrcPosScopeHelper {
public:
  SrcPosScopeHelper(SrcPosStack& stack, const SrcPosTaggable* pos)
    : _stack(stack), _pos(pos ? pos->srcPosFirst() : SrcPosPtr())
  {
    if(_pos){
      _stack.push_back(_pos);  
    }
  }
  ~SrcPosScopeHelper() {
    if(_pos){
      _stack.pop_back();
    }
  }
private:
  SrcPosStack& _stack;
  SrcPosPtr _pos;
};


}//namespace jalib


//these defines are for bison/flex integration
#ifndef SRCPOS_NO_BISON_DEFS
# define YYLTYPE jalib::SrcPos
# define YYLTYPE_IS_DECLARED 1
//logic for combining locations
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
  do{                                                                   \
    (Current) = YYRHSLOC(Rhs,0).last().fileline();                      \
    if(YYID(N)){                                                        \
      for(int i=1; i<=(N) && !(Current).first().isFullyDefined(); ++i)  \
        (Current).first() = YYRHSLOC(Rhs, i).first();                   \
      for(int i=(N); i>=1 && !(Current).last().isFullyDefined(); --i)   \
        (Current).last() = YYRHSLOC(Rhs, i).last();                     \
    }                                                                   \
  }while (YYID(0))
//fill in flex
#define SRCPOS_FILL(lloc, filename, lineno, oldlineno, col)        \
  lloc = jalib::SrcPos(filename, lineno, col, col+strlen(yytext)); \
  if(lineno!=oldlineno) {                                          \
    oldlineno=lineno;                                              \
    col=0;                                                         \
  }                                                                \
  col+=strlen(yytext);
#define YY_LOCATION_PRINT(File, Loc) fprintf(File, "%s", (Loc)->toString().c_str())
#define TAGPOS _tagposhelper
template<typename T>
inline T* _tagposhelper(T* t, const jalib::SrcPos& pos){
  t->tagPosition(pos.clone());
  return t;
}
#endif
#endif

