/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 1

/* Substitute the variable and function names.  */
#define yyparse pbparse
#define yylex   pblex
#define yyerror pberror
#define yylval  pblval
#define yychar  pbchar
#define yydebug pbdebug
#define yynerrs pbnerrs
#define yylloc pblloc

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




/* Copy the first part of user declarations.  */
#line 1 "pbparser.ypp"

#include <stdio.h>
#include <map>
#include "jconvert.h"
#include "transform.h"
#include "rule.h"
#include "formula.h"
#include "matrixdef.h"
#include "region.h"
#include "config.h"
#include "cxxconfig.h"
using namespace petabricks;

extern int pblineno;
extern char* pbtext;
extern int pblex (void);
extern std::string pbfilename;

int yyerror(petabricks::TransformListPtr&, const char* msg){
  JASSERT(false)(pbfilename)(pblineno)(pbtext)(msg).Text("parse error"); return 0;
}

static jalib::JRefPool theRefPool;
#define REFALLOC(args...) (theRefPool.add(new args))

static void clearParserCaches(){
  theRefPool.clear();
}



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 37 "pbparser.ypp"
{
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
}
/* Line 187 of yacc.c.  */
#line 199 "pbparser.cpp"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
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


/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 224 "pbparser.cpp"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
	     && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
    YYLTYPE yyls;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  13
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   186

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  41
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  45
/* YYNRULES -- Number of rules.  */
#define YYNRULES  91
/* YYNRULES -- Number of states.  */
#define YYNSTATES  174

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   277

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      31,    33,    28,    27,    32,    26,    39,    29,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    40,
      24,    23,    25,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    38,     2,     2,
       2,    34,     2,    35,    30,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    36,     2,    37,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    10,    12,    14,    16,    18,
      20,    22,    24,    27,    32,    37,    42,    47,    52,    59,
      64,    71,    73,    77,    79,    82,    86,    88,    92,    98,
     100,   104,   106,   108,   110,   114,   116,   120,   124,   128,
     132,   136,   139,   143,   147,   151,   155,   159,   163,   167,
     169,   172,   175,   178,   184,   188,   192,   197,   202,   204,
     206,   211,   214,   216,   219,   223,   229,   231,   234,   236,
     238,   240,   242,   245,   249,   250,   253,   255,   257,   259,
     261,   264,   266,   269,   271,   275,   277,   281,   283,   285,
     289,   291
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      42,     0,    -1,    81,    -1,    44,    61,    -1,    76,    -1,
      45,    -1,    46,    -1,    47,    -1,    48,    -1,    50,    -1,
      51,    -1,    49,    -1,    44,    14,    -1,    44,     3,    21,
      77,    -1,    44,     4,    82,    77,    -1,    44,     6,    82,
      77,    -1,    44,     5,    82,    77,    -1,    44,    16,    54,
      77,    -1,    44,    18,    73,    53,    74,    77,    -1,    44,
      17,    21,    77,    -1,    21,    31,    78,    32,    78,    33,
      -1,    52,    -1,    53,    32,    52,    -1,    76,    -1,    54,
      21,    -1,    21,    56,    57,    -1,    76,    -1,    24,    58,
      25,    -1,    24,    58,    75,    58,    25,    -1,    76,    -1,
      34,    83,    35,    -1,    21,    -1,    78,    -1,    79,    -1,
      31,    58,    33,    -1,    59,    -1,    58,    27,    58,    -1,
      58,    26,    58,    -1,    58,    28,    58,    -1,    58,    29,
      58,    -1,    58,    30,    58,    -1,    26,    58,    -1,    58,
      23,    58,    -1,    58,    24,    58,    -1,    58,    25,    58,
      -1,    58,     7,    58,    -1,    58,     8,    58,    -1,    36,
      80,    37,    -1,    63,    68,    77,    -1,    64,    -1,    67,
      63,    -1,    10,    63,    -1,    15,    63,    -1,    15,    31,
      58,    33,    63,    -1,    70,    65,    69,    -1,    66,    65,
      69,    -1,     4,    31,    84,    33,    -1,     5,    31,    84,
      33,    -1,    11,    -1,    12,    -1,    13,    31,    78,    33,
      -1,    36,    22,    -1,    76,    -1,    38,    60,    -1,    21,
      56,    71,    -1,    39,    21,    31,    85,    33,    -1,    76,
      -1,    70,    21,    -1,    76,    -1,    24,    -1,    76,    -1,
      25,    -1,    39,    39,    -1,    39,    39,    39,    -1,    -1,
      40,    77,    -1,    76,    -1,    19,    -1,    20,    -1,    62,
      -1,    80,    62,    -1,    43,    -1,    81,    43,    -1,    55,
      -1,    82,    32,    55,    -1,    58,    -1,    83,    32,    58,
      -1,    76,    -1,    72,    -1,    84,    32,    72,    -1,    76,
      -1,    83,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    91,    91,    97,   102,   103,   104,   105,   106,   107,
     108,   109,   111,   112,   113,   114,   115,   116,   117,   118,
     120,   123,   124,   126,   127,   129,   131,   132,   133,   140,
     141,   143,   144,   145,   146,   147,   149,   150,   151,   152,
     153,   154,   156,   157,   158,   159,   160,   163,   165,   167,
     168,   169,   170,   171,   173,   174,   176,   177,   179,   180,
     181,   183,   185,   186,   188,   190,   191,   193,   195,   195,
     196,   196,   198,   198,   199,   200,   200,   201,   202,   204,
     205,   207,   208,   210,   211,   213,   214,   216,   217,   218,
     221,   222
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TRANSFORM", "FROM", "TO", "THROUGH",
  "LE", "GE", "WHERE", "ROTATABLE", "PRIMARY", "SECONDARY", "PRIORITY",
  "MAIN", "RECURSIVE", "TESTCASE", "GENERATOR", "TEMPLATE", "INTEGER",
  "FLOAT", "IDENT", "RULEBODY", "'='", "'<'", "'>'", "'-'", "'+'", "'*'",
  "'/'", "'^'", "'('", "','", "')'", "'['", "']'", "'{'", "'}'", "'WHERE'",
  "'.'", "';'", "$accept", "Start", "Transform", "TransformHeader",
  "TransformName", "TransformFrom", "TransformThrough", "TransformTo",
  "TransformTest", "TransformTemplate", "TransformGenerator",
  "TemplateArg", "TemplateArgs", "TestCase", "MatrixDef", "OptVersion",
  "OptSize", "Formula", "FormulaExpr", "FormulaRelation", "TransformBody",
  "Rule", "RuleHeader", "BaseRuleHeader", "RuleHeaderFrom", "RuleHeaderTo",
  "PriorityFlag", "RuleBody", "OptWhere", "Region", "RegionAccessor",
  "NamedRegion", "OptLT", "OptGT", "Dots", "Nil", "OptSemiCol", "Integer",
  "Float", "RuleList", "TransformList", "MatrixDefList", "FormulaList",
  "NamedRegionList", "OptFormulaList", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,    61,    60,    62,    45,    43,    42,    47,
      94,    40,    44,    41,    91,    93,   123,   125,    87,    46,
      59
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    41,    42,    43,    44,    44,    44,    44,    44,    44,
      44,    44,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    53,    54,    54,    55,    56,    56,    56,    57,
      57,    58,    58,    58,    58,    58,    59,    59,    59,    59,
      59,    59,    60,    60,    60,    60,    60,    61,    62,    63,
      63,    63,    63,    63,    64,    64,    65,    66,    67,    67,
      67,    68,    69,    69,    70,    71,    71,    72,    73,    73,
      74,    74,    75,    75,    76,    77,    77,    78,    79,    80,
      80,    81,    81,    82,    82,    83,    83,    84,    84,    84,
      85,    85
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     4,     4,     4,     4,     4,     6,     4,
       6,     1,     3,     1,     2,     3,     1,     3,     5,     1,
       3,     1,     1,     1,     3,     1,     3,     3,     3,     3,
       3,     2,     3,     3,     3,     3,     3,     3,     3,     1,
       2,     2,     2,     5,     3,     3,     4,     4,     1,     1,
       4,     2,     1,     2,     3,     5,     1,     2,     1,     1,
       1,     1,     2,     3,     0,     2,     1,     1,     1,     1,
       2,     1,     2,     1,     3,     1,     3,     1,     1,     3,
       1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
      74,     0,    81,     0,     5,     6,     7,     8,    11,     9,
      10,     4,    74,     1,     0,     0,     0,     0,    12,    74,
       0,    74,     0,     3,    82,    74,    74,    83,    74,    74,
      74,    74,    23,    74,    69,     0,    68,     0,     0,    58,
      59,     0,     0,    74,    79,     0,    49,     0,     0,     0,
       0,    74,    76,    13,     0,    74,    26,     0,    14,    16,
      15,    24,    17,    19,     0,    21,    74,    74,    51,     0,
       0,    52,    74,     0,    74,     0,    74,    50,    74,    47,
      80,    75,    77,    78,    31,     0,     0,     0,    35,    32,
      33,     0,    25,    29,    84,     0,    71,     0,    74,    70,
       0,    88,    87,     0,     0,     0,     0,    64,    66,    61,
      48,    74,     0,    55,    62,    54,    41,     0,    27,     0,
       0,     0,     0,     0,     0,     0,    85,     0,     0,    22,
      18,    67,     0,    57,    60,     0,     0,     0,     0,    63,
      34,    37,    36,    38,    39,    40,    72,     0,     0,    30,
       0,    89,    53,    74,    56,     0,     0,     0,     0,     0,
      73,    28,    86,     0,    90,    91,     0,    45,    46,    42,
      43,    44,    20,    65
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     3,     4,     5,     6,     7,     8,     9,
      10,    65,    66,    31,    27,    55,    92,   126,    88,   139,
      23,    44,    45,    46,    76,    47,    48,    74,   113,    49,
     107,   101,    35,    98,   125,    52,    53,    89,    90,    50,
      12,    28,   127,   103,   166
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -66
static const yytype_int16 yypact[] =
{
     -66,     3,   -66,    85,   -66,   -66,   -66,   -66,   -66,   -66,
     -66,   -66,     7,   -66,    -8,    -3,    -3,    -3,   -66,   -66,
      -1,     8,   144,   -66,   -66,     2,    13,   -66,   -24,   -24,
     -24,   -11,   -66,     2,   -66,    33,   -66,    28,   144,   -66,
     -66,    34,   117,    13,   -66,    38,   -66,    60,   144,    60,
      23,     2,   -66,   -66,    32,    35,   -66,    -3,   -66,   -66,
     -66,   -66,   -66,   -66,    39,   -66,   -23,    54,   -66,    58,
      32,   -66,    40,    65,     2,    79,    74,   -66,    74,   -66,
     -66,   -66,   -66,   -66,   -66,    32,    32,   141,   -66,   -66,
     -66,    32,   -66,   -66,   -66,    58,   -66,    33,     2,   -66,
      92,   -66,   -66,    16,    81,    67,    94,   -66,   -66,   -66,
     -66,    54,    32,   -66,   -66,   -66,    11,   146,   -66,    32,
      32,    32,    32,    32,    86,    32,    78,   -18,    99,   -66,
     -66,   -66,    54,   -66,   -66,   144,    95,    24,   116,   -66,
     -66,    11,    11,   103,   103,   -66,    96,   156,    32,   -66,
      58,   -66,   -66,    32,   -66,    32,    32,    32,    32,    32,
     -66,   -66,    78,   101,   -66,   104,   114,    78,    78,    78,
      78,    78,   -66,   -66
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -66,   -66,   125,   -66,   -66,   -66,   -66,   -66,   -66,   -66,
     -66,    53,   -66,   -66,   105,   108,   -66,   -39,   -66,   -66,
     -66,   102,   -37,   -66,   109,   -66,   -66,   -66,    82,   -61,
     -66,    29,   -66,   -66,   -66,     0,    -6,   -65,   -66,   -66,
     -66,    45,    10,    66,   -66
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -3
static const yytype_int16 yytable[] =
{
      11,    68,    96,    13,   104,    71,   100,    -2,    57,    97,
      61,    77,    11,    25,   148,    87,    51,   149,    26,    32,
      33,    36,    58,    59,    60,    62,    56,    63,    37,    51,
     128,   105,    34,    38,    39,    40,    41,    54,    42,   121,
     122,   123,    51,    56,    43,    81,   116,   117,   132,   133,
     100,    82,    83,    84,    64,    93,   132,   154,    85,    67,
      79,    29,    30,    86,    75,    69,    99,   102,   110,    91,
      95,   100,   108,   138,    73,    43,   114,    82,   114,   106,
     141,   142,   143,   144,   145,   163,   147,   109,    14,    15,
      16,    17,   130,   119,   120,   121,   122,   123,   152,    18,
     135,    19,    20,    21,   119,   120,   121,   122,   123,   162,
     111,   102,   112,   131,   134,   136,   167,   168,   169,   170,
     171,    22,    37,   155,   156,   146,   153,    38,    39,    40,
      41,   150,    42,   123,   172,   160,   148,    24,    43,   157,
     158,   159,   119,   120,   121,   122,   123,   173,    70,    37,
     129,    72,    80,   164,    38,    39,    40,    41,    78,    42,
     115,   151,    94,   165,     0,    43,   118,   119,   120,   121,
     122,   123,   119,   120,   121,   122,   123,   137,     0,   140,
     124,   161,   119,   120,   121,   122,   123
};

static const yytype_int16 yycheck[] =
{
       0,    38,    25,     0,    69,    42,    67,     0,    32,    32,
      21,    48,    12,    21,    32,    54,    40,    35,    21,    19,
      21,    21,    28,    29,    30,    31,    26,    33,     5,    40,
      95,    70,    24,    10,    11,    12,    13,    24,    15,    28,
      29,    30,    40,    43,    21,    51,    85,    86,    32,    33,
     111,    19,    20,    21,    21,    55,    32,    33,    26,    31,
      37,    16,    17,    31,     4,    31,    66,    67,    74,    34,
      31,   132,    72,   112,    36,    21,    76,    19,    78,    39,
     119,   120,   121,   122,   123,   150,   125,    22,     3,     4,
       5,     6,    98,    26,    27,    28,    29,    30,   135,    14,
      33,    16,    17,    18,    26,    27,    28,    29,    30,   148,
      31,   111,    38,    21,    33,    21,   155,   156,   157,   158,
     159,    36,     5,     7,     8,    39,    31,    10,    11,    12,
      13,    32,    15,    30,    33,    39,    32,    12,    21,    23,
      24,    25,    26,    27,    28,    29,    30,    33,    31,     5,
      97,    43,    50,   153,    10,    11,    12,    13,    49,    15,
      78,   132,    57,   153,    -1,    21,    25,    26,    27,    28,
      29,    30,    26,    27,    28,    29,    30,   111,    -1,    33,
      39,    25,    26,    27,    28,    29,    30
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    76,    81,     0,     3,     4,     5,     6,    14,    16,
      17,    18,    36,    61,    43,    21,    21,    55,    82,    82,
      82,    54,    76,    21,    24,    73,    76,     5,    10,    11,
      12,    13,    15,    21,    62,    63,    64,    66,    67,    70,
      80,    40,    76,    77,    24,    56,    76,    32,    77,    77,
      77,    21,    77,    77,    21,    52,    53,    31,    63,    31,
      31,    63,    56,    36,    68,     4,    65,    63,    65,    37,
      62,    77,    19,    20,    21,    26,    31,    58,    59,    78,
      79,    34,    57,    76,    55,    31,    25,    32,    74,    76,
      70,    72,    76,    84,    78,    58,    39,    71,    76,    22,
      77,    31,    38,    69,    76,    69,    58,    58,    25,    26,
      27,    28,    29,    30,    39,    75,    58,    83,    78,    52,
      77,    21,    32,    33,    33,    33,    21,    84,    58,    60,
      33,    58,    58,    58,    58,    58,    39,    58,    32,    35,
      32,    72,    63,    31,    33,     7,     8,    23,    24,    25,
      39,    25,    58,    78,    76,    83,    85,    58,    58,    58,
      58,    58,    33,    33
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (ret, YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, Location, ret); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, petabricks::TransformListPtr& ret)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, ret)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
    petabricks::TransformListPtr& ret;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (yylocationp);
  YYUSE (ret);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, petabricks::TransformListPtr& ret)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, yylocationp, ret)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
    petabricks::TransformListPtr& ret;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, ret);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, petabricks::TransformListPtr& ret)
#else
static void
yy_reduce_print (yyvsp, yylsp, yyrule, ret)
    YYSTYPE *yyvsp;
    YYLTYPE *yylsp;
    int yyrule;
    petabricks::TransformListPtr& ret;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       , ret);
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, yylsp, Rule, ret); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, petabricks::TransformListPtr& ret)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, yylocationp, ret)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    YYLTYPE *yylocationp;
    petabricks::TransformListPtr& ret;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (ret);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (petabricks::TransformListPtr& ret);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;
/* Location data for the look-ahead symbol.  */
YYLTYPE yylloc;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (petabricks::TransformListPtr& ret)
#else
int
yyparse (ret)
    petabricks::TransformListPtr& ret;
#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;

  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[2];

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
  yylsp = yyls;
#if YYLTYPE_IS_TRIVIAL
  /* Initialize the default location before parsing starts.  */
  yylloc.first_line   = yylloc.last_line   = 1;
  yylloc.first_column = yylloc.last_column = 0;
#endif


  /* User initialization code.  */
#line 60 "pbparser.ypp"
{
  theRefPool.clear();
}
/* Line 1069 of yacc.c.  */
#line 1435 "pbparser.cpp"
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;
	YYLTYPE *yyls1 = yyls;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
	YYSTACK_RELOCATE (yyls);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 91 "pbparser.ypp"
    {
  ret = (yyvsp[(1) - (1)].transforms);
  clearParserCaches();
}
    break;

  case 3:
#line 97 "pbparser.ypp"
    {
   ((yyval.transform)=(yyvsp[(1) - (2)].transform))->setRules(*(yyvsp[(2) - (2)].rules)); 
   JTRACE("parsed Transform")((yyval.transform)->name()); 
}
    break;

  case 4:
#line 102 "pbparser.ypp"
    { (yyval.transform)=REFALLOC(Transform()); }
    break;

  case 5:
#line 103 "pbparser.ypp"
    {(yyval.transform)=(yyvsp[(1) - (1)].transform);}
    break;

  case 6:
#line 104 "pbparser.ypp"
    {(yyval.transform)=(yyvsp[(1) - (1)].transform);}
    break;

  case 7:
#line 105 "pbparser.ypp"
    {(yyval.transform)=(yyvsp[(1) - (1)].transform);}
    break;

  case 8:
#line 106 "pbparser.ypp"
    {(yyval.transform)=(yyvsp[(1) - (1)].transform);}
    break;

  case 9:
#line 107 "pbparser.ypp"
    {(yyval.transform)=(yyvsp[(1) - (1)].transform);}
    break;

  case 10:
#line 108 "pbparser.ypp"
    {(yyval.transform)=(yyvsp[(1) - (1)].transform);}
    break;

  case 11:
#line 109 "pbparser.ypp"
    {(yyval.transform)=(yyvsp[(1) - (1)].transform);}
    break;

  case 12:
#line 111 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (2)].transform))->markMain(); }
    break;

  case 13:
#line 112 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (4)].transform))->setName((yyvsp[(3) - (4)].str));     }
    break;

  case 14:
#line 113 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (4)].transform))->addFrom(*(yyvsp[(3) - (4)].matrixdefs));   }
    break;

  case 15:
#line 114 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (4)].transform))->addThrough(*(yyvsp[(3) - (4)].matrixdefs));}
    break;

  case 16:
#line 115 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (4)].transform))->addTo(*(yyvsp[(3) - (4)].matrixdefs));     }
    break;

  case 17:
#line 116 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (4)].transform))->addTestCase((yyvsp[(3) - (4)].testcase));}
    break;

  case 18:
#line 117 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (6)].transform))->addTemplateArg(*(yyvsp[(4) - (6)].templateargs)); }
    break;

  case 19:
#line 118 "pbparser.ypp"
    { UNIMPLEMENTED(); }
    break;

  case 20:
#line 120 "pbparser.ypp"
    {
  (yyval.templatearg)=REFALLOC(TemplateArg((yyvsp[(1) - (6)].str), (yyvsp[(3) - (6)].i), (yyvsp[(5) - (6)].i)));
}
    break;

  case 21:
#line 123 "pbparser.ypp"
    { ((yyval.templateargs)=REFALLOC(TemplateArgList()))->push_back((yyvsp[(1) - (1)].templatearg)); }
    break;

  case 22:
#line 124 "pbparser.ypp"
    { ((yyval.templateargs)=(yyvsp[(1) - (3)].templateargs))->push_back((yyvsp[(3) - (3)].templatearg)); }
    break;

  case 23:
#line 126 "pbparser.ypp"
    { (yyval.testcase)=REFALLOC(TestCase()); }
    break;

  case 24:
#line 127 "pbparser.ypp"
    { ((yyval.testcase)=(yyvsp[(1) - (2)].testcase))->addMatrix((yyvsp[(2) - (2)].str)); }
    break;

  case 25:
#line 129 "pbparser.ypp"
    { (yyval.matrixdef)=REFALLOC(MatrixDef((yyvsp[(1) - (3)].str),*(yyvsp[(2) - (3)].formulas),*(yyvsp[(3) - (3)].formulas))); }
    break;

  case 26:
#line 131 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList())); }
    break;

  case 27:
#line 132 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(2) - (3)].formula)); }
    break;

  case 28:
#line 134 "pbparser.ypp"
    { 
  (yyval.formulas)=REFALLOC(FormulaList()); 
  (yyval.formulas)->push_back((yyvsp[(2) - (5)].formula));
  (yyval.formulas)->push_back((yyvsp[(4) - (5)].formula));
}
    break;

  case 29:
#line 140 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 30:
#line 141 "pbparser.ypp"
    { (yyval.formulas)=(yyvsp[(2) - (3)].formulas); }
    break;

  case 31:
#line 143 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaVariable((yyvsp[(1) - (1)].str)) ); }
    break;

  case 32:
#line 144 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaInteger( (yyvsp[(1) - (1)].i)) ); }
    break;

  case 33:
#line 145 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaFloat(  (yyvsp[(1) - (1)].d)) );  }
    break;

  case 34:
#line 146 "pbparser.ypp"
    { (yyval.formula)=(yyvsp[(2) - (3)].formula); }
    break;

  case 35:
#line 147 "pbparser.ypp"
    { (yyval.formula)=(yyvsp[(1) - (1)].formula); }
    break;

  case 36:
#line 149 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'+'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 37:
#line 150 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'-'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 38:
#line 151 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'*'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 39:
#line 152 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'/'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 40:
#line 153 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'^'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 41:
#line 154 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'-'>(FormulaInteger::zero(),(yyvsp[(2) - (2)].formula))); }
    break;

  case 42:
#line 156 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'='>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 43:
#line 157 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'<'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 44:
#line 158 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'>'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 45:
#line 159 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaLE((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 46:
#line 160 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaGE((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 47:
#line 163 "pbparser.ypp"
    { (yyval.rules)=(yyvsp[(2) - (3)].rules); }
    break;

  case 48:
#line 165 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(1) - (3)].rule))->setBody((yyvsp[(2) - (3)].str)); JTRACE("parsed Rule"); }
    break;

  case 49:
#line 167 "pbparser.ypp"
    { (yyval.rule)=(yyvsp[(1) - (1)].rule); }
    break;

  case 50:
#line 168 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(2) - (2)].rule))->setPriority((yyvsp[(1) - (2)].i)); }
    break;

  case 51:
#line 169 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(2) - (2)].rule))->addRotations(RuleFlags::ROTATE); }
    break;

  case 52:
#line 170 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(2) - (2)].rule))->markRecursive(); }
    break;

  case 53:
#line 171 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(5) - (5)].rule))->markRecursive((yyvsp[(3) - (5)].formula)); }
    break;

  case 54:
#line 173 "pbparser.ypp"
    { (yyval.rule)=REFALLOC(Rule((yyvsp[(1) - (3)].region),  *(yyvsp[(2) - (3)].regions), *(yyvsp[(3) - (3)].formulas))); }
    break;

  case 55:
#line 174 "pbparser.ypp"
    { (yyval.rule)=REFALLOC(Rule(*(yyvsp[(1) - (3)].regions), *(yyvsp[(2) - (3)].regions), *(yyvsp[(3) - (3)].formulas))); }
    break;

  case 56:
#line 176 "pbparser.ypp"
    { (yyval.regions)=(yyvsp[(3) - (4)].regions); }
    break;

  case 57:
#line 177 "pbparser.ypp"
    { (yyval.regions)=(yyvsp[(3) - (4)].regions); }
    break;

  case 58:
#line 179 "pbparser.ypp"
    {(yyval.i)=RuleFlags::PRIORITY_PRIMARY;}
    break;

  case 59:
#line 180 "pbparser.ypp"
    {(yyval.i)=RuleFlags::PRIORITY_SECONDARY;}
    break;

  case 60:
#line 181 "pbparser.ypp"
    {(yyval.i)=(yyvsp[(3) - (4)].i);}
    break;

  case 61:
#line 183 "pbparser.ypp"
    { (yyval.str)=(yyvsp[(2) - (2)].str); }
    break;

  case 62:
#line 185 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 63:
#line 186 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(2) - (2)].formula)); }
    break;

  case 64:
#line 188 "pbparser.ypp"
    { (yyval.region)=REFALLOC(Region((yyvsp[(1) - (3)].str),*(yyvsp[(2) - (3)].formulas), (yyvsp[(3) - (3)].str_formulas).str,*(yyvsp[(3) - (3)].str_formulas).formulas)); }
    break;

  case 65:
#line 190 "pbparser.ypp"
    { (yyval.str_formulas).str=(yyvsp[(2) - (5)].str); (yyval.str_formulas).formulas=(yyvsp[(4) - (5)].formulas);}
    break;

  case 66:
#line 191 "pbparser.ypp"
    { (yyval.str_formulas).str="all"; (yyval.str_formulas).formulas=REFALLOC(FormulaList());}
    break;

  case 67:
#line 193 "pbparser.ypp"
    { ((yyval.region)=(yyvsp[(1) - (2)].region))->setName((yyvsp[(2) - (2)].str)); }
    break;

  case 77:
#line 201 "pbparser.ypp"
    { (yyval.i)=     jalib::StringToX<int>((yyvsp[(1) - (1)].str)); }
    break;

  case 78:
#line 202 "pbparser.ypp"
    { (yyval.d)=jalib::StringToX<double>((yyvsp[(1) - (1)].str)); }
    break;

  case 79:
#line 204 "pbparser.ypp"
    { ((yyval.rules)=REFALLOC(RuleList()))->push_back((yyvsp[(1) - (1)].rule)); }
    break;

  case 80:
#line 205 "pbparser.ypp"
    { ((yyval.rules)=(yyvsp[(1) - (2)].rules))->push_back((yyvsp[(2) - (2)].rule)); }
    break;

  case 81:
#line 207 "pbparser.ypp"
    { ((yyval.transforms)=REFALLOC(TransformList()))->push_back((yyvsp[(1) - (1)].transform)); }
    break;

  case 82:
#line 208 "pbparser.ypp"
    { ((yyval.transforms)=(yyvsp[(1) - (2)].transforms))->push_back((yyvsp[(2) - (2)].transform)); }
    break;

  case 83:
#line 210 "pbparser.ypp"
    { ((yyval.matrixdefs)=REFALLOC(MatrixDefList()))->push_back((yyvsp[(1) - (1)].matrixdef)); }
    break;

  case 84:
#line 211 "pbparser.ypp"
    { ((yyval.matrixdefs)=(yyvsp[(1) - (3)].matrixdefs))->push_back((yyvsp[(3) - (3)].matrixdef));                      }
    break;

  case 85:
#line 213 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(1) - (1)].formula)); }
    break;

  case 86:
#line 214 "pbparser.ypp"
    { ((yyval.formulas)=(yyvsp[(1) - (3)].formulas))->push_back((yyvsp[(3) - (3)].formula)); }
    break;

  case 87:
#line 216 "pbparser.ypp"
    { ((yyval.regions)=REFALLOC(RegionList())); }
    break;

  case 88:
#line 217 "pbparser.ypp"
    { ((yyval.regions)=REFALLOC(RegionList()))->push_back((yyvsp[(1) - (1)].region)); }
    break;

  case 89:
#line 218 "pbparser.ypp"
    { ((yyval.regions)=(yyvsp[(1) - (3)].regions))->push_back((yyvsp[(3) - (3)].region)); }
    break;

  case 90:
#line 221 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 91:
#line 222 "pbparser.ypp"
    { (yyval.formulas)=(yyvsp[(1) - (1)].formulas); }
    break;


/* Line 1267 of yacc.c.  */
#line 2037 "pbparser.cpp"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (ret, YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (ret, yymsg);
	  }
	else
	  {
	    yyerror (ret, YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }

  yyerror_range[0] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval, &yylloc, ret);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[0] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      yyerror_range[0] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp, ret);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;

  yyerror_range[1] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the look-ahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, (yyerror_range - 1), 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (ret, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, &yylloc, ret);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, yylsp, ret);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 225 "pbparser.ypp"


extern FILE* pbin;

TransformListPtr parsePbFile(const char* filename){
  TransformListPtr ret;
#ifdef RUN_CPP
  pbin = popen((std::string(CPP " ")+filename).c_str(),"r");
#else
  pbin = fopen(filename,"r");
#endif
  JASSERT(pbin!=NULL)(filename)(JASSERT_ERRNO).Text("failed to open file");
  pbparse(ret);
  return ret;
}



