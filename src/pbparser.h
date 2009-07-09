
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TRANSFORM = 258,
     FROM = 259,
     TO = 260,
     THROUGH = 261,
     LE = 262,
     GE = 263,
     EQ = 264,
     WHERE = 265,
     ROTATABLE = 266,
     PRIMARY = 267,
     SECONDARY = 268,
     PRIORITY = 269,
     MAIN = 270,
     RECURSIVE = 271,
     TESTCASE = 272,
     GENERATOR = 273,
     TEMPLATE = 274,
     TUNABLE = 275,
     CONFIG = 276,
     INTEGER = 277,
     FLOAT = 278,
     IDENT = 279,
     RULEBODY = 280
   };
#endif
/* Tokens.  */
#define TRANSFORM 258
#define FROM 259
#define TO 260
#define THROUGH 261
#define LE 262
#define GE 263
#define EQ 264
#define WHERE 265
#define ROTATABLE 266
#define PRIMARY 267
#define SECONDARY 268
#define PRIORITY 269
#define MAIN 270
#define RECURSIVE 271
#define TESTCASE 272
#define GENERATOR 273
#define TEMPLATE 274
#define TUNABLE 275
#define CONFIG 276
#define INTEGER 277
#define FLOAT 278
#define IDENT 279
#define RULEBODY 280




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 37 "pbparser.ypp"

  int i;
  double d;
  const char* str;
  petabricks::Transform*       transform;
  petabricks::TransformList*   transforms;
  petabricks::MatrixDef*       matrixdef;
  petabricks::MatrixDefList*   matrixdefs;
  petabricks::Rule*            rule;
  petabricks::RuleList*        rules;
  petabricks::Formula*         formula;
  petabricks::FormulaList*     formulas;
  petabricks::Region*          region;
  petabricks::RegionList*      regions;
  petabricks::TestCase*        testcase;
  petabricks::TemplateArg*     templatearg;
  petabricks::TemplateArgList* templateargs;
  struct { const char* str; petabricks::FormulaList* formulas; } str_formulas; 



/* Line 1676 of yacc.c  */
#line 124 "pbparser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE pblval;

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYLTYPE pblloc;

