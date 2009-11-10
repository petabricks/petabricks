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

#include <map>
#include <string>
#include <vector>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif


namespace petabricks {
class CodeGenerator;
class Transform;
class MatrixDef;
class FreeVars;
typedef jalib::JRef<MatrixDef> MatrixDefPtr;
class MatrixDefList : public std::vector<MatrixDefPtr> , public jalib::JRefCounted {};
class MatrixDefMap : public std::map<std::string, MatrixDefPtr> , public jalib::JRefCounted {};

/**
 * Symbolically defines the size of an input/output matrix
 */
class MatrixDef : public jalib::JRefCounted, public jalib::JPrintable {
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
    #ifdef SHORT_TYPE_NAMES
    return "MatrixRegion"+jalib::XToString(numDimensions())+"D";
    #else
    return "petabricks::MatrixRegion<"+jalib::XToString(numDimensions())+">";
    #endif
  }

  std::string constMatrixTypeName() const{
    #ifdef SHORT_TYPE_NAMES
    return "ConstMatrixRegion"+jalib::XToString(numDimensions())+"D";
    #else
    return "petabricks::MatrixRegion<"+jalib::XToString(numDimensions())+", const MATRIX_ELEMENT_T>";
    #endif
  }

  
  std::string sliceTypeName() const{
    #ifdef SHORT_TYPE_NAMES
    return "MatrixRegion"+jalib::XToString(numDimensions()-1)+"D";
    #else
    return "petabricks::MatrixRegion<"+jalib::XToString(numDimensions()-1)+">";
    #endif
  }

  std::string constSliceTypeName() const{
    #ifdef SHORT_TYPE_NAMES
    return "ConstMatrixRegion"+jalib::XToString(numDimensions()-1)+"D";
    #else
    return "petabricks::MatrixRegion<"+jalib::XToString(numDimensions()-1)+", const MATRIX_ELEMENT_T>";
    #endif
  }

  

  void argDeclRW(std::vector<std::string>& args, bool byRef=false) const;
  void argDeclRO(std::vector<std::string>& args, bool byRef=false) const;
  void genAllocTmpCode(CodeGenerator& o);
  void generateCodeSimple(CodeGenerator& o);
  void readFromFileCode(CodeGenerator& o, const std::string& fn);
  void writeToFileCode(CodeGenerator& o, const std::string& fn);
  void varDeclCodeRO(CodeGenerator& o);
  void varDeclCodeRW(CodeGenerator& o);

  void extractDefines(FreeVars& defined, CodeGenerator& o);
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
