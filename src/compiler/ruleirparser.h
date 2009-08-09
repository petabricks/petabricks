
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
     TOK_FORENOUGH = 258,
     TOK_DEFAULT = 259,
     TOK_CASE = 260,
     TOK_SWITCH = 261,
     TOK_RAW = 262,
     TOK_CONTINUE = 263,
     TOK_BREAK = 264,
     TOK_RETURN = 265,
     TOK_DO = 266,
     TOK_WHILE = 267,
     TOK_FOR = 268,
     TOK_ELSE = 269,
     TOK_THEN = 270,
     TOK_IF = 271,
     TOK_IDENT = 272,
     TOK_LIT = 273,
     TOK_OP = 274
   };
#endif
/* Tokens.  */
#define TOK_FORENOUGH 258
#define TOK_DEFAULT 259
#define TOK_CASE 260
#define TOK_SWITCH 261
#define TOK_RAW 262
#define TOK_CONTINUE 263
#define TOK_BREAK 264
#define TOK_RETURN 265
#define TOK_DO 266
#define TOK_WHILE 267
#define TOK_FOR 268
#define TOK_ELSE 269
#define TOK_THEN 270
#define TOK_IF 271
#define TOK_IDENT 272
#define TOK_LIT 273
#define TOK_OP 274




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 31 "ruleirparser.ypp"

  const char* str;
  petabricks::RIRExpr*  expr;
  petabricks::RIRStmt*  stmt;
  petabricks::RIRBlock* block;



/* Line 1676 of yacc.c  */
#line 99 "libpbcompiler_a-ruleirparser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE ruleirlval;

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

extern YYLTYPE ruleirlloc;

