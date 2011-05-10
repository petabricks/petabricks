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
#ifndef PETABRICKSMATRIXDEF_H
#define PETABRICKSMATRIXDEF_H

#include "formula.h"

#include "common/jconvert.h"
#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/srcpos.h"

#include <map>
#include <string>
#include <vector>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif


namespace petabricks {
class CodeGenerator;
#if HAVE_OPENCL
class CLCodeGenerator;
#endif
class Transform;
class MatrixDef;
class FreeVars;
typedef jalib::JRef<MatrixDef> MatrixDefPtr;
class MatrixDefList : public std::vector<MatrixDefPtr> , public jalib::JRefCounted, public jalib::SrcPosTaggable {};
class MatrixDefMap : public std::map<std::string, MatrixDefPtr> , public jalib::JRefCounted {};

/**
 * Symbolically defines the size of an input/output matrix
 */
class MatrixDef : public jalib::JRefCounted, public jalib::JPrintable, public jalib::SrcPosTaggable {
public:
  enum Type {
    T_UNKNOWN = 0,
    T_FROM    = 1,
    T_TO      = 2,
    T_THROUGH = 4
  };


  static const MatrixDef& oneD(){
    FormulaList l;
    l.push_back(FormulaInteger::one());
    static MatrixDef tmp("_oneD",l,l);
    return tmp;
  }

  ///
  /// Constructor
  MatrixDef(const char* name, const FormulaList& version, const FormulaList& size);
  
  void print(std::ostream& o) const;

  void initialize(Transform&);
  
  const std::string& name() const { return _name; }

  size_t numDimensions() const { return _size.size(); }
  
  FormulaPtr getSizeOfDimension( size_t n ) const { 
    JASSERT(n < numDimensions())(n)(numDimensions());
    return _size[n]; 
  }

  FormulaPtr width() const  { return getSizeOfDimension(0); }
  FormulaPtr height() const { return getSizeOfDimension(1); }

  FormulaPtr getMaxValueInDimension(size_t n) const {
    return new FormulaSubtract(getSizeOfDimension(n), FormulaInteger::one());
  }

  void exportAssumptions();

  void exportConstants(Transform&);

  std::string matrixTypeName() const{
    return "MatrixRegion"+jalib::XToString(numDimensions())+"D";
  }

  std::string constMatrixTypeName() const{
    return "ConstMatrixRegion"+jalib::XToString(numDimensions())+"D";
  }

  
  std::string sliceTypeName() const{
    return "MatrixRegion"+jalib::XToString(numDimensions()-1)+"D";
  }

  std::string constSliceTypeName() const{
    return "ConstMatrixRegion"+jalib::XToString(numDimensions()-1)+"D";
  }

  
  std::string allocateStr() const;

  void argDeclRW(std::vector<std::string>& args, bool byRef=false) const;
  void argDeclRO(std::vector<std::string>& args, bool byRef=false) const;
  void genAllocTmpCode(CodeGenerator& o);
  void generateCodeSimple(CodeGenerator& o);
  void readFromFileCode(CodeGenerator& o, const std::string& fn);
  void writeToFileCode(CodeGenerator& o, const std::string& fn);
  void varDeclCodeRO(CodeGenerator& o);
  void varDeclCodeRW(CodeGenerator& o);

  void extractDefines(FreeVars& defined, CodeGenerator& o);
#if HAVE_OPENCL
  void extractCLDefines(FreeVars& defined, CLCodeGenerator& clo, unsigned int dims);
#endif
  void verifyDefines(CodeGenerator& o);
  void allocateTemporary(CodeGenerator& o, bool setOnly, bool reallocAllowed);

  void addType(Type t){  _type |= t; }
  bool isAllInput() const { return _type == T_FROM; }
private:
  std::string _name;
  FormulaList _version;
  FormulaList _size;
  int _type;
};
 
}

#endif
