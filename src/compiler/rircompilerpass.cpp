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
#include "rircompilerpass.h"
#include "codegenerator.h"
#include "rule.h"
#include "transform.h"

#include "common/jconvert.h"

#include <cstring>

namespace {//file local
  std::string _uniquify(const std::string& prefix) {
    static std::map<std::string, int> idmap;
    return prefix+"_"+jalib::XToString(idmap[prefix]++);
  }
}

void petabricks::GpuRenamePass::before(RIRExprCopyRef& e)
{
  if( RIRNode::EXPR_IDENT == e->type() )
    {
      if( e->isLeaf( "ElementT" ) )
	e = new RIRIdentExpr( STRINGIFY( MATRIX_ELEMENT_T ) );
      else if( e->isLeaf( "IndexT" ) )
	e = new RIRIdentExpr( STRINGIFY( MATRIX_INDEX_T ) );
    }
}

void petabricks::DynamicBodyPrintPass::before(RIRStmtCopyRef& s) {
  switch(s->type()){
  case RIRNode::STMT_BASIC:
  case RIRNode::STMT_RAW:
    if(s->containsLeaf("break")){
      o.comment("break;");
      JASSERT(!_breakTargets.empty());
      o.continueJump(_breakTargets.back());
    }else if(s->containsLeaf("continue")){
      o.comment("continue;");
      JASSERT(!_continueTargets.empty());
      o.continueJump(_continueTargets.back());
    }else if(s->containsLeaf("SYNC")){
      o.comment("SYNC();");
      o.continuationRequired("petabricks::sync_hook(_completion, ");
    }else if(s->containsLeaf("CALL")){
      o.write(s->toString());
      o.comment("sync forced because of CALL");
      o.continuationRequired("petabricks::sync_hook(_completion, ");
    }else if(s->containsLeaf("SPAWN")){
      o.write(s->toString());
      o.continuationPoint();
    }else{
      o.write(s->toString());
    }
    break;
  case RIRNode::STMT_LOOP:
  case RIRNode::STMT_COND:
  case RIRNode::STMT_BLOCK:
  case RIRNode::STMT_SWITCH:
    if(s->containsLeaf("SYNC") || s->containsLeaf("CALL") || s->containsLeaf("SPAWN")
        || s->containsLeaf("break")  || s->containsLeaf("continue") ){
      if(s->type() == RIRNode::STMT_COND){
        o.comment("expanded if statement");
        const RIRIfStmt& stmt = (const RIRIfStmt&)*s;
        std::string jthen = o.nextContName("then_");
        std::string jelse = o.nextContName("else_");
        std::string jafter = o.nextContName("after_");
        if(!stmt.elsePart()) jelse=jafter;
        o.beginIfNot(stmt.condPart()->toString());
        o.continueJump(jelse);
        o.endIf();
        o.continueLabel(jthen);
        stmt.thenPart()->extractBlock()->accept(*this);
        if(stmt.elsePart()){
          o.continueJump(jafter);
          o.continueLabel(jelse);
          stmt.elsePart()->extractBlock()->accept(*this);
        }
        o.continueLabel(jafter);
      }else if(s->type() == RIRNode::STMT_LOOP){
        o.comment("expanded loop statement");
        const RIRLoopStmt& stmt = (const RIRLoopStmt&)*s;
        std::string jbody = o.nextContName("loopbody_");
        std::string jafter = o.nextContName("after_");
        o.write(stmt.declPart()->toString()+";");
        o.continueLabel(jbody);
        o.beginIfNot(stmt.testPart()->toString());
        o.continueJump(jafter);
        o.endIf();
        _breakTargets.push_back(jafter);
        _continueTargets.push_back(jbody);
        stmt.body()->extractBlock()->accept(*this);
        _continueTargets.pop_back();
        _breakTargets.pop_back();
        o.write(stmt.incPart()->toString()+";");
        o.continueJump(jbody);
        o.continueLabel(jafter);
      }else if(s->type() == RIRNode::STMT_BLOCK){
        o.comment("expanded block statement");
        s->extractBlock()->accept(*this);
      }else{
        UNIMPLEMENTED();
      }
    }else{
      o.write(s->toString());
    }
    break;
  default:
    JASSERT(false)(s->typeStr())(s->toString());
  }
}

void petabricks::LiftVardeclPass::before(RIRExprCopyRef& e) {
  if(e->type() == RIRNode::EXPR_IDENT){
    RIRSymbolPtr sym = _scope->lookup(e->toString());
    if(sym && sym->hasReplacement() && sym->replacement()!=e->toString()){
      JTRACE("LIFTVAR - replace")(e->toString())(sym->replacement());
      e = new RIRIdentExpr(sym->replacement());
      before(e);
      return;
    }else if(sym && sym->isType()){
      if(!hasExprBackward() && hasExprForward()
          && (peekExprForward()->type()==RIRNode::EXPR_IDENT || peekExprForward()->isLeaf("*")) ){
        //it smells like a variable declaration...
        std::string type = e->toString();
        for(int nstar=0; peekExprForward()->isLeaf("*"); nstar++){
          type+=popExprForward()->toString();
          if(!hasExprForward()) {
            //whoops, we were supposed to find an IDENT not EOF -- backtrack
            for(; nstar>=0; nstar--) pushExprForward(RIRExpr::parse("*", SRCPOS()));
            return;
          }
        }
        std::string name = peekExprForward()->toString();
        std::string nameExtra = "";
        std::string nameMangled = prefix() + name;
        e = NULL;
        popExprForward(); //scroll past name
        pushExprBackward(new RIRIdentExpr(nameMangled));
        if(hasExprForward()){
          JASSERT(!peekExprForward()->isLeaf(","))(_transform.name())(_rule.id()-_transform.ruleIdOffset())
            .Text("list style initializers not yet supported");
          if(peekExprForward()->isLeaf("[")){
            while(!peekExprForward()->isLeaf("]")){
              nameExtra += popExprForward()->toString();
            }
            nameExtra += popExprForward()->toString();
          }
        }
        _scope->set(name, new RIRSymbol(RIRSymbol::SYM_LOCAL_VAR, nameMangled));
        _scope->set(nameMangled, new RIRSymbol(RIRSymbol::SYM_LOCAL_VAR));
        o.addMember(type, nameMangled+nameExtra, "");
        JTRACE("LIFTVAR - decl")(name)(nameMangled);
      }
    }else{
      //JTRACE("LIFTVAR - unknown")(e->toString())(sym);
    }
  }
}



void petabricks::RuleFlavorSpecializePass::before(RIRExprCopyRef& e) {
  if(e->type() == RIRNode::EXPR_IDENT){
    RIRSymbolPtr sym = _scope->lookup(e->toString());
    if(sym && sym->type() ==  RIRSymbol::SYM_TYPE_MATRIX_GENERIC){
      e = new RIRIdentExpr(_rf.string()+"::"+e->toString());
      JTRACE("specialized type")(e)(e->type() == RIRNode::EXPR_IDENT);
    }
  }
}



void petabricks::ExpansionPass::before(RIRStmtCopyRef& s){
  if(s->type() != RIRNode::STMT_BLOCK && depth()>=2 && parentNode()->isControl()){
    // add {}'s to sloppy ifs() and loops
    RIRBlockCopyRef tmp = new RIRBlock();
    tmp->addStmt(s);
    s=new RIRBlockStmt(tmp);
  }
  if(s->type() == RIRNode::STMT_LOOP && (depth()==1 || !parentNode()->hasAnnotation("outer_for_scope"))){
    // add {}'s around for statements to give them a sub-scope
    RIRBlockCopyRef tmp = new RIRBlock();
    tmp->addStmt(s);
    tmp->addAnnotation("outer_for_scope");
    s=new RIRBlockStmt(tmp);
  }
  if(s->type() == RIRNode::STMT_LOOP && s->hasAnnotation("for_enough")){
    //expand forenough loops
    s->removeAnnotation("for_enough");
    RIRLoopStmt& loop = (RIRLoopStmt&)*s;
    RIRStmtCopyRef t;
    JASSERT(s->numExprs()==5)(s->numExprs());
    RIRExprCopyRef maxExp = s->popExpr();
    RIRExprCopyRef minExp = s->popExpr();
    int minI = jalib::StringToX<int>(minExp->toString());
    int maxI = jalib::StringToX<int>(maxExp->toString());
    JTRACE("for_enough expansion")(minExp)(minI)(maxExp)(maxI);
    std::string config=     _uniquify("forenough_iterations");
    std::string vI=         _uniquify("_forenough_i");
    std::string vCount=     _uniquify("_forenough_count");
    //std::string vIsTraining=_uniquify("_forenough_isTraining");
    _transform.addConfigItem(
        ConfigItem::FLAG_FROMCFG|ConfigItem::FLAG_SIZESPECIFIC|ConfigItem::FLAG_ACCURACY|ConfigItem::FLAG_TUNABLE,
        config, minI, minI, maxI);

    // set the iteration bounds
    loop.declPart() = RIRExpr::parse("int "+vI+" = 0", SRCPOS());
    loop.testPart() = RIRExpr::parse(vI+" < "+config, SRCPOS());
    loop.incPart()  = RIRExpr::parse("++"+vI, SRCPOS());
  }
}

void petabricks::ExpansionPass::before(RIRExprCopyRef& e){
  if(e->type() == RIRNode::EXPR_IDENT){
    RIRSymbolPtr sym = _scope->lookup(e->toString());
    if(sym && sym->isTemplateTransform()){
      if(peekExprForward()->isLeaf("<")){
        //transform calls to templates from:
        //   tmpl<a,b>(c,d)
        //to:
        //   tmpl(a,b,c,d)
        popExprForward();
        RIRExprList tmp;
        tmp.push_back(e);
        tmp.push_back(new RIROpExpr(","));
        e = new RIRIdentExpr("CALL");
        //tmp.push_back(e);
        //tmp.push_back(new RIROpExpr(","));
        while(!peekExprForward()->isLeaf(">")){
          tmp.push_back(popExprForward().asPtr());
        }
        tmp.push_back(new RIROpExpr(","));
        popExprForward();
        JASSERT(!peekExprForward()->isLeaf())(peekExprForward())
          .Text("Expected (...) after template transform");
        RIRExprList::iterator i=peekExprForward()->parts().begin();
        JASSERT((*i)->isLeaf("("))(*i);
        ++i;
        peekExprForward()->parts().insert(i, tmp.begin(), tmp.end());
        JTRACE("handled template")(e)(tmp.size())(peekExprForward()->toString());
      }
    }
    if(sym && sym->isTransform()){
      if(hasExprForward() && peekExprForward()->type() == RIRNode::EXPR_ARGS){
        //transform transform calls from:
        //   Foo(c,d)
        //to:
        //   CALL(Foo,c,d)
        JTRACE("Creating call")(sym);
        peekExprForward()->parts().push_front(e);
        if(hasExprBackward()&&peekExprBackward()->isLeaf("spawn")){
          popExprBackward();
          e = new RIRIdentExpr("SPAWN");
        }else{
          e = new RIRIdentExpr("CALL");
        }
      }
    }
    if(sym && sym->type() == RIRSymbol::SYM_CONFIG_TRANSFORM_LOCAL){
      JTRACE("Expanding config item")(e);
      e = new RIRIdentExpr("TRANSFORM_LOCAL("+e->toString()+")");
    }
    if(sym && sym->isType()){
      if(!hasExprBackward() && hasExprForward()
          && (peekExprForward()->type()==RIRNode::EXPR_IDENT || peekExprForward()->isLeaf("*")) ){
        //transform:
        // int a,b=0,c;
        //into:
        // int a; int b=0; int c;
        for(;;){
          RIRStmtRef stmt = new RIRBasicStmt();
          stmt->addExpr(e);
          //collect everything till the next ,
          while(hasExprForward() && !peekExprForward()->isLeaf(","))
            stmt->addExpr(popExprForward().asPtr());
          if(hasExprForward() && peekExprForward()->isLeaf(",")){
            //add an extra statement behind this
            stmt->accept(*this);
            JTRACE("expanded ,decl")(stmt);
            pushStmtBackward(stmt.asPtr());
            popExprForward();//","
          }else{
            //replace this stmt
            while(stmt->numExprs()>1){
              pushExprForward(stmt->popExpr());
            }
            break;
          }
        }
      }
    }
    if(e->isLeaf("verify_accuracy")){
      std::string var = _uniquify("tmpacc");

      RIRStmtCopyRef s1 = RIRStmt::parse("ElementT "+var+";", SRCPOS());
      s1->accept(*this);
      pushStmtBackward(s1);

      std::vector<std::string> argnames = _transform.normalArgNames();
      std::ostringstream os;
      os << _transform.accuracyMetric() << "(" <<  var << ", transform->";
      jalib::JPrintable::printStlList(os, argnames.begin(), argnames.end(), ", transform->");
      os << ");";
      RIRStmtCopyRef s2 = RIRStmt::parse(os.str(), SRCPOS());
      s2->accept(*this);
      pushStmtBackward(s2);

      if(_transform.isAccuracyInverted())
        e = RIRExpr::parse(var + "*-1 >= ACCURACY_TARGET", SRCPOS());
      else
        e = RIRExpr::parse(var + " >= ACCURACY_TARGET", SRCPOS());
    }
    if(e->isLeaf("sync")){
      e = RIRExpr::parse("SYNC()", SRCPOS());
    }
  }
  if(e->type() == RIRNode::EXPR_KEYWORD){
    if(e->toString() == "return"){
      if(hasExprForward() && !peekExprForward()->parts().empty()){
        peekExprForward()->parts().push_front(new RIROpExpr("("));
        peekExprForward()->parts().push_back(new RIROpExpr(")"));
        e = new RIRIdentExpr("RETURN");
      }else{
        e = new RIRIdentExpr("RETURN_VOID");
      }
    }
  }
}

void petabricks::AnalysisPass::before(RIRExprCopyRef& e){
  if(e->type() == RIRNode::EXPR_IDENT){
    RIRSymbolPtr sym = _scope->lookup(e->toString());
    if(sym && sym->isTransform()){
      TrainingDeps::addCallgraphEdge(_name, e->toString());
      _rule.markRecursive();
    }
  }
}

petabricks::RegionPtr petabricks::OpenClCleanupPass::findMatrix(std::string var)
{
  RegionList from = _rule.getFromRegions();
  RegionList to = _rule.getToRegions();

  for( RegionList::const_iterator i = to.begin(); i != to.end(); ++i )
    if( var == (*i)->name() )
      return (*i);
  for( RegionList::const_iterator i = from.begin(); i != from.end(); ++i )
    if( var == (*i)->name() )
      return (*i);
  return NULL;
}

void petabricks::OpenClCleanupPass::generateAccessor( const RegionPtr& , const FormulaPtr& , const FormulaPtr&  )
{
}

std::vector<std::string> petabricks::OpenClCleanupPass::generateCellIndices(RIRExprList& tokens) {
  std::string s;
  for(RIRExprList::iterator i = tokens.begin(); i != tokens.end(); ++i) {
    s += (*i)->toString();
  }
  std::vector<std::string> indices;
  char* tok = strtok((char*) s.c_str(), ",");
  while(tok != NULL) {
    indices.push_back(tok);
    tok = strtok(NULL, ",");
  }
  return indices;
}

void petabricks::OpenClCleanupPass::before(RIRExprCopyRef& e){
  if(e->type() == RIRNode::EXPR_IDENT){
    RIRSymbolPtr sym = _scope->lookup(e->toString());
    if(sym && sym->isTemplateTransform()){
      throw NotValidSource();
    }
    if(sym && sym->isTransform()){
      throw NotValidSource();
    }
    if(sym && sym->type() == RIRSymbol::SYM_CONFIG_TRANSFORM_LOCAL){
      throw NotValidSource();
    }
    if(e->isLeaf("SPAWN")){
      throw NotValidSource();
    }
    if(e->isLeaf("SYNC")){
      throw NotValidSource();
    }
    if(sym && sym->type() == RIRSymbol::SYM_ARG_REGION){
      if(hasExprForward() && peekExprForward()->isLeaf(".")){
        RIRExprCopyRef regionName = e;
        popExprForward();// burn "."
        RIRExprRef call = popExprForward();
        JASSERT(call->type()==RIRNode::EXPR_CALL && call->parts().size()==2);
        RIRExprCopyRef methodname = call->part(0);
        RIRExprCopyRef args = call->part(1);
        JTRACE("expanding SYM_ARG_REGION")(regionName)(methodname)(args);

	      // Look up matrix region.
	      RegionPtr region = findMatrix(regionName->str());
	      JASSERT( !region.null() ).Text( "No such region exists." );

	      if( "cell" == methodname->str() )
	      {
          // Convert cell to linear index for OpenCL buffer.
          std::vector<std::string> indices = generateCellIndices(call->parts().back()->parts().front()->parts());
          std::vector<std::string>::reverse_iterator i = indices.rbegin();
          FormulaPtr idx_formula;
          switch(region->getRegionType()) {
            case Region::REGION_CELL:
              JASSERT(false).Text("Cannot call cell in cell");
              break;
            case Region::REGION_ROW:
              idx_formula = new FormulaVariable(*i);
              break;
            case Region::REGION_COL:
              idx_formula = new FormulaMultiply( new FormulaVariable("(" + *i + ")"), new FormulaVariable("dim_" + region->name() + "_d0") );
              break;
            case Region::REGION_ALL:
            case Region::REGION_BOX:
              {
                int j = indices.size() - 2;
                idx_formula = new FormulaVariable("(" + *i + ")");
                ++i;
                while(i != indices.rend()) {
                  std::stringstream sizevar;
                  sizevar << "dim_" << region->name() << "_d" << j--;
                  idx_formula = new FormulaAdd( new FormulaVariable(*i), new FormulaMultiply( new FormulaVariable( sizevar.str( ) ), idx_formula ) );
                  ++i;
                }
              }
              break;
            default:
              UNIMPLEMENTED();
          }

          // Use local memory when possible.
          std::string exprstr;
          if(_minCoordOffsets.find(region->matrix()->name()) != _minCoordOffsets.end()) {
            // Use local memory
            std::vector<std::string>::reverse_iterator i = indices.rbegin();
            if(region->dimensions() == 1) {
              exprstr = "buff_" + region->matrix()->name() + "[" + *i + " + x_local]";
            }
            else if(region->dimensions() == 2) {
              std::string y = *i;
              i++;
              std::string x = *i;
              exprstr = "buff_" + region->matrix()->name() + "[" + y + " + y_local]" 
                                                           + "[" + x + " + x_local]";
            }
            else {
              JASSERT(false).Text("Dimension is not 1 or 2. No Local Memory");
            }
          }
          else {
            // Use global memory
            exprstr = "_region_" + region->name() + "[" + idx_formula->toString() + " + idx_" + region->name() + "]";
          }
          e = RIRExpr::parse( exprstr, SRCPOS() );
        }
        else if("count" == methodname->str()) {
          FormulaPtr count_formula = new FormulaInteger(1);
          for(size_t i = 0; i < region->dimensions(); ++i)
            count_formula = new FormulaMultiply(count_formula, new FormulaVariable("dim_" + region->name() + "_d" + jalib::XToString(i)) );
          e = RIRExpr::parse(count_formula->toString(), SRCPOS() );
        }
	      else if( "width" == methodname->str() )
	      {
	          e = RIRExpr::parse( "dim_" + region->matrix()->name() + "_d0", SRCPOS() );
	      }
	      else
	      {
	        JASSERT( false ).Text( "Failed to generate OpenCL kernel: unsupported member function of region." );
	      }
      }
    }
    if(sym && sym->type() == RIRSymbol::SYM_ARG_ELEMENT) {
      RegionPtr region = findMatrix(e->str());
      std::string matrix = region->matrix()->name();
      std::string exprstr;
          if(_minCoordOffsets.find(matrix) != _minCoordOffsets.end()) {
            if(region->dimensions() == 1) {
              FormulaPtr index_x = new FormulaSubtract(region->minCoord().at(0), new FormulaVariable("_r" + jalib::XToString(_id) + "_x"));
              index_x = MAXIMA.normalize(index_x);
              exprstr = "buff_" + matrix + "[" + index_x->toCppString() + " + x_local + " + matrix + "0_minoffset]";
            }
            else if(region->dimensions() == 2) {
              FormulaPtr index_x = new FormulaSubtract(region->minCoord().at(0), new FormulaVariable("_r" + jalib::XToString(_id) + "_x"));
              index_x = MAXIMA.normalize(index_x);
              FormulaPtr index_y = new FormulaSubtract(region->minCoord().at(1), new FormulaVariable("_r" + jalib::XToString(_id) + "_y"));
              index_y = MAXIMA.normalize(index_y);
              exprstr = "buff_" + matrix + "[" + index_y->toCppString() + " + y_local + " + matrix + "1_minoffset]"
                                         + "[" + index_x->toCppString() + " + x_local + " + matrix + "0_minoffset]";
            }
            else {
              JASSERT(false).Text("Dimension is not 1 or 2. No Local Memory");
            }
          }
          else {
            exprstr = "_region_" + e->str() + "[idx_"+e->str()+"]";
          }
      e = RIRExpr::parse( exprstr, SRCPOS() );
    }
  }
}

bool petabricks::OpenClFunctionRejectPass::isFunctionAllowed( const std::string& fn )
{
  /* This is a quick list of functions which are common to the C or C++ and OpenCL C.  Thus, trying to compile rules using these functions
     shouldn't cause a problem. */
  const std::string whitelist[] =
    { "RETURN",
      "abs", "fabs",
      "max", "min",
      "sign", "round",
      "floor", "ceil",
      "log", "exp", "pow",
      "sin", "cos", "tan",
      "acos", "asin", "atan",
      "sqrt",
      "cell", 
      "", };

  const std::string* p = whitelist;
  while( "" != *p )
    if( *(p++) == fn )
      return true;

  return false;
}

bool petabricks::OpenClFunctionRejectPass::isIdentBlacklisted( const std::string& ident )
{
  const std::string blacklist[] =
    { "double",
      "fftw_complex",
      "", };

  const std::string* p = blacklist;
  while( "" != *p )
    if( *(p++) == ident )
      return true;

  return false;
}

void petabricks::OpenClFunctionRejectPass::before(RIRExprCopyRef& e)
{
  if(e->type() == RIRNode::EXPR_CALL)
    {
      if( ( e->parts().size() < 1 ) || !isFunctionAllowed( e->part(0)->str() ) )
	{
	  JTRACE( "Function isn't whitelisted for OpenCL:")(e->part(0)->str());
	  throw NotValidSource();
	}
    }
  else if(e->type() == RIRNode::EXPR_IDENT)
    {
      if( isIdentBlacklisted( e->str() ) )
	{
	  JTRACE( "Identifier is blacklisted for OpeNCL:")(e->str());
	}
    }
}
