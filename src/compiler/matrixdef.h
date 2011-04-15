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
