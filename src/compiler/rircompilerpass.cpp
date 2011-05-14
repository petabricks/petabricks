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
        o.write(s->extractBlock()->toString()); 
      }else{
        UNIMPLEMENTED();
      }
    }else{
      o.write(s->toString()); 
    }
    break;
  case RIRNode::STMT_SWITCH:
  default:
    UNIMPLEMENTED()(s->typeStr());
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
        std::string type = e->toString();
        while(peekExprForward()->isLeaf("*"))
          type+=popExprForward()->toString();
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

void petabricks::ExpansionPass::before(RIRStmtCopyRef& s){
  if(s->type() != RIRNode::STMT_BLOCK && depth()>=2 && parentNode()->isControl()){
    // add {}'s to sloppy ifs() and loops
    RIRBlockCopyRef tmp = new RIRBlock();
    tmp->addStmt(s);
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
        e = new RIRIdentExpr("PB_RETURN");
      }else{
        e = new RIRIdentExpr("PB_RETURN_VOID");
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

	// Generate list of index expressions.
	/*
	std::cout << "formula bounds:\n";
	for( int i = 0; i < region->dimensions(); ++i )
	  {
	    std::cout << i << ": ";
	    region->minCoord().at( i )->print(std::cout);
	    std::cout << " to ";
	    region->maxCoord().at( i )->print(std::cout);
	    std::cout << std::endl;
	  }
	*/

	if( "cell" == methodname->str() )
	  {
	    std::string xcoord, ycoord;
	    if( petabricks::Region::REGION_ROW == region->getRegionType() )
	      {
		xcoord = args->toString();
		ycoord = region->minCoord().at(1)->toString();
	      }
	    else if( petabricks::Region::REGION_COL == region->getRegionType() )
	      {
		xcoord = region->minCoord().at(0)->toString();
		ycoord = args->toString();
	      }
	    else
	      {
		std::cout << "Failed to generate OpenCL kernel: unsupported region type.";
		throw NotValidSource();
	      }

	    std::string exprstr = region->matrix()->name() + "[(dim_" + region->matrix()->name() + "_d0*" + ycoord + ")+" + xcoord + "]";
	    //std::cout << "expression string: " << exprstr << "\n";
	    e = RIRExpr::parse( exprstr, SRCPOS() );
	    //std::cout << "accessor index: " << e->debugStr() << "\n";
	  }
	else if( "width" == methodname->str() )
	  {
	    e = RIRExpr::parse( "dim_" + region->matrix()->name() + "_d0", SRCPOS() );
	  }
	else
	  {
	    JASSERT( false ).Text( "Failed to generate OpenCL kernel: unsupported member function of region." );
	  }

	// Simplify expressions and produce final call.

	/*
        args->prependSubExpr(methodname);
        args->prependSubExpr(regionName);
        e = new RIRCallExpr();
        e->addSubExpr(new RIRIdentExpr("REGION_METHOD_CALL"));
        e->addSubExpr(args);
	*/
      }
    }
  }
}

bool petabricks::OpenClFunctionRejectPass::isFunctionAllowed( const std::string& fn )
{
  /* This is a quick list of functions which are common to the C or C++ and OpenCL C.  Thus, trying to compile rules using these functions
     shouldn't cause a problem. */
  const std::string whitelist[] =
    { "PB_RETURN",
      "abs", "fabs",
      "max", "min",
      "sign", "round",
      "floor", "ceil",
      "log", "exp", "pow",
      "sin", "cos", "tan",
      "acos", "asin", "atan",
      "sqrt",
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
