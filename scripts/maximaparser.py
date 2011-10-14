#!/usr/bin/python
import ply.lex as lex
import ply.yacc as yacc
from ply.lex import TOKEN
from formula import *

# ----------------------- LEXER --------------------------
WS = r'[ \r\n\t]'

reserved = {"true" : "BOOL_T",
            "false" : "BOOL_F",
            "equal" : "STR_EQUAL",
            "ceiling" : "STR_CEILING",
            "floor" : "STR_FLOOR",
            "and" : "AND",
            "or"  : "OR",
            "if"  : "IF",
            "then" : "THEN",
            "else" : "ELSE"}
            
#Tokens and literals lists, as required by ply.lex
tokens = ["INTEGER", "FLOAT", "IDENT", "LE", "GE", ] + list(reserved.values())
literals = ["=", "<", ">", ",", "*", "/", "(", ")", "[", "]", "\n", "^", "+", "-"] 
  
@TOKEN(WS)
def t_WS(t):
  #Whitespace: do not return anything
  pass

def t_IDENT(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'IDENT')    # Check for reserved words
    return t

def t_error(t):
  print "Unable to recongnize the next token: " + t.value
  exit()


t_INTEGER = r'[0-9]+'
t_FLOAT = r'[0-9]+[.][0-9]+'
t_LE = r'<='
t_GE = r'>='

# -------------------------- PARSER ----------------------------
precedence = (
  ('left', 'AND', 'OR'),
  ('nonassoc', '=', '<', '>', 'LE', 'GE'),
  ('right', 'IF', 'THEN', 'ELSE'),
  ('left', '-', '+'),
  ('left', '*', '/'),
  ('left', '^'),
)

def p_formula_IDENT(p):
  r'Formula : IDENT'
  p[0]=FormulaVariable(p[1])
  
def p_formula_Integer(p):
  r'Formula : Integer'
  p[0]=FormulaInteger(p[1])
  
def p_formula_Bool(p):
  r'Formula : Bool'
  p[0]=FormulaBool(p[1])
  
def p_formula_Float(p):
  r'Formula : Float'
  p[0]=FormulaFloat(p[1])
  
def p_formula_paren(p):
  r"Formula : '(' Formula ')'"
  p[0]=p[2]
       
def p_formula_binop(p):
  r'Formula : FormulaBinop'
  p[0]=p[1]
  
def p_formula_if(p):
  r'Formula : FormulaIf'
  p[0]=p[1]
  
def p_formulaif_then(p):
  r'FormulaIf : IF Formula THEN Formula'
  p[0]=FormulaIf(p[2], p[4])
  
def p_formulaif_then_else(p):
  r'FormulaIf : IF Formula THEN Formula ELSE Formula'
  p[0]=FormulaIf(p[2], p[4], p[6])
  
def p_binop_plus(p):
  r"FormulaBinop : Formula '+' Formula"
  p[0]=FormulaBinop("+",p[1],p[3])
  
def p_binop_minus(p):
  r"FormulaBinop : Formula '-' Formula"
  p[0]=FormulaBinop("-",p[1],p[3])
  
def p_binop_mul(p):
  r"FormulaBinop : Formula '*' Formula"
  p[0]=FormulaBinop("*",p[1],p[3])

def p_binop_div(p):
  r"FormulaBinop : Formula '/' Formula"
  p[0]=FormulaBinop("/",p[1],p[3])

def p_binop_eq(p):
  r"FormulaBinop : Formula '=' Formula"
  p[0]=FormulaBinop("=",p[1],p[3])

def p_binop_lt(p):
  r"FormulaBinop : Formula '<' Formula"
  p[0]=FormulaBinop("<",p[1],p[3])

def p_binop_gt(p):
  r"FormulaBinop : Formula '>' Formula"
  p[0]=FormulaBinop(">",p[1],p[3])

def p_binop_power(p):
  r"FormulaBinop : Formula '^' Formula"
  p[0]=FormulaBinop("^",p[1],p[3])

def p_binop_le(p):
  r"FormulaBinop : Formula LE Formula"
  p[0]=FormulaBinop("<=",p[1],p[3])

def p_binop_ge(p):
  r"FormulaBinop : Formula GE Formula"
  p[0]=FormulaBinop(">=",p[1],p[3])

def p_binop_and(p):
  r"FormulaBinop : Formula AND Formula"
  p[0]=FormulaBinop("and",p[1],p[3])
  
def p_binop_or(p):
  r"FormulaBinop : Formula OR Formula"
  p[0]=FormulaBinop("or",p[1],p[3])
  
def p_binop_strequal(p):
  r"FormulaBinop : STR_EQUAL '(' Formula ',' Formula ')'"
  p[0]=FormulaBinop("=",p[3],p[5])

def p_binop_strfloor(p):
  r"FormulaBinop : STR_FLOOR '(' Formula ')'"
  p[0]=p[3]
  
def p_binop_strceiling(p):
  r"FormulaBinop : STR_CEILING '(' Formula ')'"
  p[0]=p[3]
  
def p_binop_neg(p):
  r"FormulaBinop : '-' Formula"
  p[0]=FormulaBinop("-", FormulaInteger(0), p[2])
  
def p_integer(p):
  r'Integer : INTEGER'
  p[0]=int(p[1])
  
def p_bool_t(p):
  r'Bool : BOOL_T'
  p[0]=True
  
def p_bool_f(p):
  r'Bool : BOOL_F'
  p[0]=False
  
def p_float(p):
  r'Float : FLOAT'
  p[0]=float(p[1])
  
def p_error(p):
  print "Error parsing the next token: " + str(p)
  

#--------------- Init ----------------
#Build the lexer
lex.lex()
#Build the parser
parser = yacc.yacc()


#-------------- External interface ------------
def parse(s):
  return parser.parse(s)

