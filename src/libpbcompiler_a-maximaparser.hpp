/* A Bison parser, made by GNU Bison 2.7.12-4996.  */

/* Bison interface for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.
   
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

#ifndef YY_MAXIMA_LIBPBCOMPILER_A_MAXIMAPARSER_HPP_INCLUDED
# define YY_MAXIMA_LIBPBCOMPILER_A_MAXIMAPARSER_HPP_INCLUDED
/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int maximadebug;
#endif

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     IPROMPT = 258,
     OPROMPT = 259,
     LE = 260,
     GE = 261,
     BOOL_T = 262,
     BOOL_F = 263,
     STR_EQUAL = 264,
     STR_FLOOR = 265,
     STR_CEILING = 266,
     IF = 267,
     THEN = 268,
     ELSE = 269,
     IDENT = 270,
     INTEGER = 271,
     FLOAT = 272,
     OR = 273,
     AND = 274,
     NOT = 275
   };
#endif
/* Tokens.  */
#define IPROMPT 258
#define OPROMPT 259
#define LE 260
#define GE 261
#define BOOL_T 262
#define BOOL_F 263
#define STR_EQUAL 264
#define STR_FLOOR 265
#define STR_CEILING 266
#define IF 267
#define THEN 268
#define ELSE 269
#define IDENT 270
#define INTEGER 271
#define FLOAT 272
#define OR 273
#define AND 274
#define NOT 275



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 2053 of yacc.c  */
#line 55 "maximaparser.ypp"

  bool b;
  int i;
  double d;
  const char* str;
  petabricks::Formula*        formula;
  petabricks::FormulaList*    formulas;


/* Line 2053 of yacc.c  */
#line 107 "libpbcompiler_a-maximaparser.hpp"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

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

extern YYSTYPE maximalval;
extern YYLTYPE maximalloc;
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int maximaparse (void *YYPARSE_PARAM);
#else
int maximaparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int maximaparse (petabricks::FormulaListPtr& ret);
#else
int maximaparse ();
#endif
#endif /* ! YYPARSE_PARAM */

#endif /* !YY_MAXIMA_LIBPBCOMPILER_A_MAXIMAPARSER_HPP_INCLUDED  */
