/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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
     WHERE = 264,
     ROTATABLE = 265,
     PRIMARY = 266,
     SECONDARY = 267,
     PRIORITY = 268,
     MAIN = 269,
     RECURSIVE = 270,
     TESTCASE = 271,
     GENERATOR = 272,
     TEMPLATE = 273,
     INTEGER = 274,
     FLOAT = 275,
     IDENT = 276,
     RULEBODY = 277
   };
#endif
/* Tokens.  */
#define TRANSFORM 258
#define FROM 259
#define TO 260
#define THROUGH 261
#define LE 262
#define GE 263
#define WHERE 264
#define ROTATABLE 265
#define PRIMARY 266
#define SECONDARY 267
#define PRIORITY 268
#define MAIN 269
#define RECURSIVE 270
#define TESTCASE 271
#define GENERATOR 272
#define TEMPLATE 273
#define INTEGER 274
#define FLOAT 275
#define IDENT 276
#define RULEBODY 277




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 37 "pbparser.ypp"
{
  int i;
  double d;
  const char* str;
  hecura::Transform*       transform;
  hecura::TransformList*   transforms;
  hecura::MatrixDef*       matrixdef;
  hecura::MatrixDefList*   matrixdefs;
  hecura::Rule*            rule;
  hecura::RuleList*        rules;
  hecura::Formula*         formula;
  hecura::FormulaList*     formulas;
  hecura::Region*          region;
  hecura::RegionList*      regions;
  hecura::TestCase*        testcase;
  hecura::TemplateArg*     templatearg;
  hecura::TemplateArgList* templateargs;
  struct { const char* str; hecura::FormulaList* formulas; } str_formulas; 
}
/* Line 1489 of yacc.c.  */
#line 113 "pbparser.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
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
