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

namespace jalib {

class SrcPos;
typedef JRef<const SrcPos> SrcPosPtr;


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
    if(a.filename==b.filename){
      if(a.line==b.line)
        return a.column<b.column;
      else
        return a.line<b.line;
    }
    return a.filename<b.filename;
  }
  bool sameFile(const FileLineCol& b) const { return filename==b.filename; }
};

class SrcPos : public JRefCounted, public JPrintable {
public:
  SrcPos(const FileLineCol& first, const FileLineCol& last, bool isAuto=false)
    : _first(first), _last(last), _isAutotagged(isAuto)
  {}
  SrcPos(const FileLineCol& first, bool isAuto=false)
    : _first(first), _last(first), _isAutotagged(isAuto)
  {}
  SrcPos(bool isAuto=false)
    : _isAutotagged(isAuto)
  {}
  SrcPos(const std::string& file, int line, int col1, int col2, bool isAuto=false)
    : _first(FileLineCol(file,line,col1))
    , _last(FileLineCol(file,line,col2))
    , _isAutotagged(isAuto)
  {}
  void print(std::ostream& o) const {
    o << _first.filename;
    if(_first.filename != _last.filename)
      o << "/" << _last.filename;
    if(_first.line>=0)
      o << ":" << _first.line;
    if(_last.line>=0 && _last.line!=_first.line){
      o << "-" << _last.line;
    } else {
      if(_first.column >= 0)
        o << " (col " << _first.column << "-" << _last.column << ")";
    }
  }

  FileLineCol& first() { return _first; }
  FileLineCol& last() { return _last; }
  const FileLineCol& first() const { return _first; }
  const FileLineCol& last() const { return _last; }

  SrcPosPtr clone() const { return new SrcPos(*this); }
private:
  FileLineCol _first;
  FileLineCol _last;
  bool        _isAutotagged;
};


class SrcPosTaggable {
public:
  void tagPosition(const SrcPosPtr& p) { if(p) _positions.push_back(p); }
  bool srcPos(std::ostream& os) const;
  std::string srcPos() const;
  const SrcPosTaggable* _lexicalSrcPos() const { return this; }
private:
  mutable std::vector<SrcPosPtr> _positions;
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

