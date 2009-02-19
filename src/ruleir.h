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
#ifndef HECURARULEIR_H
#define HECURARULEIR_H

#include "jrefcounted.h"
#include "jprintable.h"

namespace hecura {

class RIRNode;
class RIRBlock;
class RIRStmt;
class RIRExpr;
typedef jalib::JRef<RIRNode>  RIRNodePtr;
typedef jalib::JRef<RIRBlock> RIRBlockPtr;
typedef jalib::JRef<RIRStmt>  RIRStmtPtr;
typedef jalib::JRef<RIRExpr>  RIRExprPtr;
typedef std::vector<RIRNodePtr>  RIRNodeList;
typedef std::vector<RIRBlockPtr> RIRBlockList;
typedef std::vector<RIRStmtPtr>  RIRStmtList;
typedef std::vector<RIRExprPtr>  RIRExprList;

/**
 * Base class for all Rule IR types
 */
class RIRNode : public jalib::JRefCounted, public jalib::JPrintable {
public:
};

/**
 * Rule IR Expression
 */
class RIRExpr  : public RIRNode {
public:
  RIRExpr(const std::string& str="") :_str(str) {}
  void addSubExpr(const RIRExprPtr& p) { _parts.push_back(p); }
  void print(std::ostream& o) const;
private:
  std::string _str;
  RIRExprList _parts;
};

typedef RIRExpr RIRNilExpr;
typedef RIRExpr RIROpExpr;
typedef RIRExpr RIRLitExpr;
typedef RIRExpr RIRIdentExpr;
typedef RIRExpr RIRChainExpr;
typedef RIRExpr RIRCallExpr;
typedef RIRExpr RIRArgsExpr;
typedef RIRExpr RIRKeywordExpr;

/**
 * Rule IR Statement
 */
class RIRStmt  : public RIRNode {
public:
  void addExpr(const RIRExprPtr& p)   { _exprs.push_back(p); }
protected:
  RIRExprList _exprs;
};

class RIRBasicStmt  : public RIRStmt {
public:
  void print(std::ostream& o) const;
};

class RIRControlStmt  : public RIRStmt {
public:
};

class RIRBlockStmt  : public RIRStmt{
public:
  RIRBlockStmt(const RIRBlockPtr& p) { _block=p; }
  void print(std::ostream& o) const;
private:
  RIRBlockPtr _block;
};

class RIRLoopStmt: public RIRControlStmt{
public:
  RIRLoopStmt(const RIRStmtPtr& p) { _body=p; }
  void print(std::ostream& o) const;
private:
  RIRStmtPtr _body;
};

class RIRIfStmt: public RIRControlStmt{
public:
  RIRIfStmt(const RIRStmtPtr& t, const RIRStmtPtr& e=0) 
    : _then(t)
    , _else(e) 
  {}
  void print(std::ostream& o) const;
private:
  RIRStmtPtr _then;
  RIRStmtPtr _else;
};

typedef RIRBasicStmt RIRReturnStmt;
typedef RIRControlStmt RIRBreakStmt;
typedef RIRControlStmt RIRContinueStmt;

/**
 * Rule IR Basic Block
 */
class RIRBlock : public RIRNode {
public:
  void addStmt(const RIRStmtPtr& p) { _stmts.push_back(p); }
  void print(std::ostream& o) const;
private:
  RIRStmtList _stmts;
};


}

#endif
