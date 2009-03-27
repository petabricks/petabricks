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

void petabricks::DynamicBodyPrintPass::before(RIRStmtCopyRef& s) {
  if(s->containsLeaf("SYNC")){
    o.comment("SYNC();");
    o.continuationRequired("petabricks::sync_hook(_completion, ");
  }else if(s->containsLeaf("CALL")){
    o.write(s->toString()); 
    o.comment("SYNC(); // forced because of CALL");
    o.continuationRequired("petabricks::sync_hook(_completion, ");
  }else if(s->containsLeaf("SPAWN")){
    o.write(s->toString()); 
    o.continuationPoint();
  }else{ 
    o.write(s->toString()); 
  }
}


void petabricks::LiftVardeclPass::before(RIRExprCopyRef& e) {
  if(e->type() == RIRNode::EXPR_IDENT){
    RIRSymbolPtr sym = RIRScope::global()->lookup(e->toString());
    if(sym && sym->isType()){
      if(!hasExprBackward() && peekExprForward()->type()==RIRNode::EXPR_IDENT){
        std::string name = peekExprForward()->toString();
        std::string type = e->toString();
        e = NULL;
        pushExprBackward(popExprForward().asPtr());//scroll forward 1
        if(hasExprForward()){
          JASSERT(!peekExprForward()->isLeaf(",")).Text("list style initializers not yet supported");
          if(peekExprForward()->isLeaf("[")){
            while(!peekExprForward()->isLeaf("]")){ 
              name += popExprForward()->toString();
            }
            name += popExprForward()->toString();
          }
        }
        o.addMember(type, name, "");
      }
    }
  }
}
  
void petabricks::ExpansionPass::before(RIRExprCopyRef& e){
  if(e->type() == RIRNode::EXPR_IDENT){
    RIRSymbolPtr sym = _scope->lookup(e->toString());
    if(sym && sym->type() == RIRSymbol::SYM_TRANSFORM_TEMPLATE){
      RIRExprList tmp;
      if(peekExprForward()->isLeaf("<")){
        //transform calls to templates from:
        //   tmpl<a,b>(c,d)
        //to:
        //   tmpl(a,b,c,d)
        popExprForward();
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
    if(sym && sym->type() == RIRSymbol::SYM_TRANSFORM){
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



