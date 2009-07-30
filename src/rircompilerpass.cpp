/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
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
#include "rircompilerpass.h"

#include "codegenerator.h"
#include "rule.h"
#include "transform.h"

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
      if(!hasExprBackward() && hasExprForward() && (peekExprForward()->type()==RIRNode::EXPR_IDENT || peekExprForward()->isLeaf("*")) ){
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
  if(s->type() != RIRNode::STMT_BLOCK 
    && depth()>=2 
    && parentNode()->isControl()){
    // add {}'s to sloppy ifs() and loops
    RIRBlockCopyRef tmp = new RIRBlock();
    tmp->addStmt(s);
    s=new RIRBlockStmt(tmp);
  }
}
  
void petabricks::ExpansionPass::before(RIRExprCopyRef& e){
  if(e->type() == RIRNode::EXPR_IDENT){
    RIRSymbolPtr sym = _scope->lookup(e->toString());
    if(sym && sym->type() == RIRSymbol::SYM_TRANSFORM_TEMPLATE){
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
      if(peekExprForward()->type() == RIRNode::EXPR_ARGS){
        //transform transform calls from:
        //   Foo(c,d)
        //to:
        //   CALL(Foo,c,d)
        JTRACE("Creating call")(sym);
        peekExprForward()->parts().push_front(e);
        e = new RIRIdentExpr("CALL");
      }
    }
    if(sym && sym->type() == RIRSymbol::SYM_CONFIG_TRANSFORM_LOCAL){
      JTRACE("Expanding config item")(e);
      e = new RIRIdentExpr("TRANSFORM_LOCAL("+e->toString()+")");
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



