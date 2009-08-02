
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
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
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 1

/* Substitute the variable and function names.  */
#define yyparse         pbparse
#define yylex           pblex
#define yyerror         pberror
#define yylval          pblval
#define yychar          pbchar
#define yydebug         pbdebug
#define yynerrs         pbnerrs
#define yylloc          pblloc

/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "pbparser.ypp"

#include <stdio.h>
#include <map>
#include "jconvert.h"
#include "transform.h"
#include "userrule.h"
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



/* Line 189 of yacc.c  */
#line 113 "pbparser.cpp"

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


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     KW_TRANSFORM = 258,
     KW_FROM = 259,
     KW_TO = 260,
     KW_THROUGH = 261,
     KW_LE = 262,
     KW_GE = 263,
     KW_EQ = 264,
     KW_WHERE = 265,
     KW_ROTATABLE = 266,
     KW_PRIMARY = 267,
     KW_SECONDARY = 268,
     KW_PRIORITY = 269,
     KW_MAIN = 270,
     KW_RECURSIVE = 271,
     KW_GENERATOR = 272,
     KW_TEMPLATE = 273,
     KW_TUNABLE = 274,
     KW_CONFIG = 275,
     KW_PARAM = 276,
     KW_ACCURACYMETRIC = 277,
     KW_ACCURACYBINS = 278,
     KW_ACCURACYVARIABLE = 279,
     TOK_INTEGER = 280,
     TOK_FLOAT = 281,
     TOK_RULEBODY = 282,
     IDENT = 283
   };
#endif
/* Tokens.  */
#define KW_TRANSFORM 258
#define KW_FROM 259
#define KW_TO 260
#define KW_THROUGH 261
#define KW_LE 262
#define KW_GE 263
#define KW_EQ 264
#define KW_WHERE 265
#define KW_ROTATABLE 266
#define KW_PRIMARY 267
#define KW_SECONDARY 268
#define KW_PRIORITY 269
#define KW_MAIN 270
#define KW_RECURSIVE 271
#define KW_GENERATOR 272
#define KW_TEMPLATE 273
#define KW_TUNABLE 274
#define KW_CONFIG 275
#define KW_PARAM 276
#define KW_ACCURACYMETRIC 277
#define KW_ACCURACYBINS 278
#define KW_ACCURACYVARIABLE 279
#define TOK_INTEGER 280
#define TOK_FLOAT 281
#define TOK_RULEBODY 282
#define IDENT 283




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 37 "pbparser.ypp"

  int i;
  double d;
  const char* str;
  petabricks::Transform*       transform;
  petabricks::TransformList*   transforms;
  petabricks::MatrixDef*       matrixdef;
  petabricks::MatrixDefList*   matrixdefs;
  petabricks::UserRule*        rule;
  petabricks::RuleList*        rules;
  petabricks::Formula*         formula;
  petabricks::FormulaList*     formulas;
  petabricks::Region*          region;
  petabricks::RegionList*      regions;
  petabricks::OrderedFreeVars* freevars;
  petabricks::TemplateArg*     templatearg;
  petabricks::TemplateArgList* templateargs;
  petabricks::DoubleList*      doublelist;
  struct { const char* str; petabricks::FormulaList* formulas; } str_formulas; 



/* Line 214 of yacc.c  */
#line 228 "pbparser.cpp"
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


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 253 "pbparser.cpp"

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
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
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
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
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
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  6
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   215

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  46
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  41
/* YYNRULES -- Number of rules.  */
#define YYNRULES  100
/* YYNRULES -- Number of states.  */
#define YYNSTATES  188

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   283

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      38,    39,    34,    33,    40,    32,    45,    35,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    37,
      30,    29,    31,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    41,     2,    42,    36,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    43,     2,    44,     2,     2,     2,     2,
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
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    10,    13,    16,    20,    24,
      28,    32,    36,    42,    46,    50,    54,    58,    64,    71,
      80,    91,    93,    95,    97,   104,   106,   110,   114,   116,
     120,   126,   128,   132,   134,   136,   138,   142,   144,   148,
     152,   156,   160,   164,   167,   171,   175,   179,   183,   187,
     191,   195,   199,   201,   204,   207,   210,   216,   220,   224,
     229,   234,   236,   238,   243,   246,   248,   251,   255,   261,
     263,   266,   268,   270,   272,   274,   277,   281,   282,   285,
     287,   289,   291,   293,   295,   297,   300,   302,   305,   307,
     311,   313,   317,   319,   321,   325,   327,   329,   331,   335,
     337
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      47,     0,    -1,    80,    -1,    49,    59,    -1,    74,    -1,
      49,    37,    -1,    49,    15,    -1,    49,     3,    28,    -1,
      49,    21,    85,    -1,    49,     4,    81,    -1,    49,     6,
      81,    -1,    49,     5,    81,    -1,    49,    18,    71,    52,
      72,    -1,    49,    22,    28,    -1,    49,    23,    86,    -1,
      49,    17,    28,    -1,    49,    50,    28,    -1,    49,    50,
      28,    38,    39,    -1,    49,    50,    28,    38,    76,    39,
      -1,    49,    50,    28,    38,    76,    40,    76,    39,    -1,
      49,    50,    28,    38,    76,    40,    76,    40,    76,    39,
      -1,    20,    -1,    19,    -1,    24,    -1,    28,    38,    76,
      40,    76,    39,    -1,    51,    -1,    52,    40,    51,    -1,
      28,    54,    55,    -1,    74,    -1,    30,    56,    31,    -1,
      30,    56,    73,    56,    31,    -1,    74,    -1,    41,    82,
      42,    -1,    28,    -1,    76,    -1,    77,    -1,    38,    56,
      39,    -1,    57,    -1,    56,    33,    56,    -1,    56,    32,
      56,    -1,    56,    34,    56,    -1,    56,    35,    56,    -1,
      56,    36,    56,    -1,    32,    56,    -1,    56,    29,    56,
      -1,    56,     9,    56,    -1,    56,    30,    56,    -1,    56,
      31,    56,    -1,    56,     7,    56,    -1,    56,     8,    56,
      -1,    43,    79,    44,    -1,    61,    66,    75,    -1,    62,
      -1,    65,    61,    -1,    11,    61,    -1,    16,    61,    -1,
      16,    38,    56,    39,    61,    -1,    68,    63,    67,    -1,
      64,    63,    67,    -1,     4,    38,    83,    39,    -1,     5,
      38,    83,    39,    -1,    12,    -1,    13,    -1,    14,    38,
      76,    39,    -1,    43,    27,    -1,    74,    -1,    10,    58,
      -1,    28,    54,    69,    -1,    45,    28,    38,    84,    39,
      -1,    74,    -1,    68,    28,    -1,    74,    -1,    30,    -1,
      74,    -1,    31,    -1,    45,    45,    -1,    45,    45,    45,
      -1,    -1,    37,    75,    -1,    74,    -1,    25,    -1,    26,
      -1,    77,    -1,    76,    -1,    60,    -1,    79,    60,    -1,
      48,    -1,    80,    48,    -1,    53,    -1,    81,    40,    53,
      -1,    56,    -1,    82,    40,    56,    -1,    74,    -1,    70,
      -1,    83,    40,    70,    -1,    74,    -1,    82,    -1,    28,
      -1,    85,    40,    28,    -1,    78,    -1,    86,    40,    78,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    96,    96,   101,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   115,   116,   117,   119,   121,   123,   125,
     127,   131,   132,   133,   136,   139,   140,   145,   147,   148,
     149,   156,   157,   159,   160,   161,   162,   163,   165,   166,
     167,   168,   169,   170,   172,   173,   174,   175,   176,   177,
     180,   182,   184,   185,   186,   187,   188,   190,   191,   193,
     194,   196,   197,   198,   200,   202,   203,   205,   207,   208,
     210,   212,   212,   213,   213,   215,   215,   216,   217,   217,
     218,   219,   220,   220,   222,   223,   225,   226,   228,   229,
     231,   232,   234,   235,   236,   239,   240,   243,   244,   246,
     247
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "KW_TRANSFORM", "KW_FROM", "KW_TO",
  "KW_THROUGH", "KW_LE", "KW_GE", "KW_EQ", "KW_WHERE", "KW_ROTATABLE",
  "KW_PRIMARY", "KW_SECONDARY", "KW_PRIORITY", "KW_MAIN", "KW_RECURSIVE",
  "KW_GENERATOR", "KW_TEMPLATE", "KW_TUNABLE", "KW_CONFIG", "KW_PARAM",
  "KW_ACCURACYMETRIC", "KW_ACCURACYBINS", "KW_ACCURACYVARIABLE",
  "TOK_INTEGER", "TOK_FLOAT", "TOK_RULEBODY", "IDENT", "'='", "'<'", "'>'",
  "'-'", "'+'", "'*'", "'/'", "'^'", "';'", "'('", "')'", "','", "'['",
  "']'", "'{'", "'}'", "'.'", "$accept", "Start", "Transform",
  "TransformHeader", "ConfigItemType", "TemplateArg", "TemplateArgs",
  "MatrixDef", "OptVersion", "OptSize", "Formula", "FormulaExpr",
  "FormulaRelation", "TransformBody", "Rule", "RuleHeader",
  "BaseRuleHeader", "RuleHeaderFrom", "RuleHeaderTo", "PriorityFlag",
  "RuleBody", "OptWhere", "Region", "RegionAccessor", "NamedRegion",
  "OptLT", "OptGT", "Dots", "Nil", "OptSemiCol", "Integer", "Float",
  "FloatOrInt", "RuleList", "TransformList", "MatrixDefList",
  "FormulaList", "NamedRegionList", "OptFormulaList", "FreeVars",
  "FloatList", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,    61,
      60,    62,    45,    43,    42,    47,    94,    59,    40,    41,
      44,    91,    93,   123,   125,    46
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    46,    47,    48,    49,    49,    49,    49,    49,    49,
      49,    49,    49,    49,    49,    49,    49,    49,    49,    49,
      49,    50,    50,    50,    51,    52,    52,    53,    54,    54,
      54,    55,    55,    56,    56,    56,    56,    56,    57,    57,
      57,    57,    57,    57,    58,    58,    58,    58,    58,    58,
      59,    60,    61,    61,    61,    61,    61,    62,    62,    63,
      64,    65,    65,    65,    66,    67,    67,    68,    69,    69,
      70,    71,    71,    72,    72,    73,    73,    74,    75,    75,
      76,    77,    78,    78,    79,    79,    80,    80,    81,    81,
      82,    82,    83,    83,    83,    84,    84,    85,    85,    86,
      86
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     2,     2,     3,     3,     3,
       3,     3,     5,     3,     3,     3,     3,     5,     6,     8,
      10,     1,     1,     1,     6,     1,     3,     3,     1,     3,
       5,     1,     3,     1,     1,     1,     3,     1,     3,     3,
       3,     3,     3,     2,     3,     3,     3,     3,     3,     3,
       3,     3,     1,     2,     2,     2,     5,     3,     3,     4,
       4,     1,     1,     4,     2,     1,     2,     3,     5,     1,
       2,     1,     1,     1,     1,     2,     3,     0,     2,     1,
       1,     1,     1,     1,     1,     2,     1,     2,     1,     3,
       1,     3,     1,     1,     3,     1,     1,     1,     3,     1,
       3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
      77,     0,    86,     0,     4,    77,     1,     0,     0,     0,
       0,     6,     0,    77,    22,    21,     0,     0,     0,    23,
       5,     0,     0,     3,    87,     7,    77,    88,     9,    11,
      10,    15,    72,     0,    71,    97,     8,    13,    80,    81,
      83,    82,    99,    14,     0,     0,    61,    62,     0,     0,
      77,    84,     0,    52,     0,     0,     0,     0,    16,     0,
      77,    28,     0,     0,    25,    77,     0,     0,    77,    54,
       0,     0,    55,    77,     0,    77,     0,    77,    53,    77,
      50,    85,     0,    33,     0,     0,     0,    37,    34,    35,
       0,    27,    31,    89,     0,    74,     0,    12,    73,    98,
     100,     0,    93,    92,     0,     0,     0,     0,    67,    69,
      64,    77,    79,    51,    77,     0,    58,    65,    57,    17,
       0,    43,     0,    29,     0,     0,     0,     0,     0,     0,
       0,    90,     0,     0,    26,    70,    60,     0,    63,     0,
       0,    78,     0,     0,    66,    18,     0,    36,    39,    38,
      40,    41,    42,    75,     0,     0,    32,     0,    94,    56,
      77,    59,     0,     0,     0,     0,     0,     0,     0,    76,
      30,    91,     0,    95,    96,     0,    48,    49,    45,    44,
      46,    47,    19,     0,    24,    68,     0,    20
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     3,    22,    64,    65,    27,    60,    91,
     131,    87,   144,    23,    51,    52,    53,    77,    54,    55,
      75,   116,    56,   108,   102,    33,    97,   130,     4,   113,
      88,    89,    42,    57,     5,    28,   132,   104,   175,    36,
      43
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -64
static const yytype_int16 yypact[] =
{
     -64,     3,   -64,   100,   -64,    10,   -64,   -15,   -13,   -13,
     -13,   -64,    -7,     2,   -64,   -64,    11,    17,    29,   -64,
     -64,    73,    31,   -64,   -64,   -64,    32,   -64,    -4,    -4,
      -4,   -64,   -64,    38,   -64,   -64,    37,   -64,   -64,   -64,
     -64,   -64,   -64,    48,    57,    73,   -64,   -64,    60,    30,
      32,   -64,    89,   -64,   112,    73,   112,     6,    97,   108,
     106,   -64,   -13,   126,   -64,     0,   147,    29,   148,   -64,
     113,   108,   -64,   132,   151,   142,   143,   170,   -64,   170,
     -64,   -64,     8,   -64,   108,   108,    -8,   -64,   -64,   -64,
     108,   -64,   -64,   -64,   113,   -64,    38,   -64,   -64,   -64,
     -64,   154,   -64,   -64,    74,   144,    58,   156,   -64,   -64,
     -64,   142,   -64,   -64,   148,   108,   -64,   -64,   -64,   -64,
     102,    91,   127,   -64,   108,   108,   108,   108,   108,   140,
     108,    47,   -33,   146,   -64,   -64,   -64,   148,   -64,    73,
     149,   -64,   105,   122,   -64,   -64,   113,   -64,    91,    91,
     152,   152,   -64,   145,   136,   108,   -64,   113,   -64,   -64,
     108,   -64,   108,   108,   108,   108,   108,   108,   110,   -64,
     -64,    47,   150,   -64,   153,   155,    47,    47,    47,    47,
      47,    47,   -64,   113,   -64,   -64,   157,   -64
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -64,   -64,   186,   -64,   -64,    96,   -64,   133,   158,   -64,
     -55,   -64,   -64,   -64,   141,   -43,   -64,   159,   -64,   -64,
     -64,   118,   -63,   -64,    62,   -64,   -64,   -64,   -12,    90,
     -18,   -10,   135,   -64,   -64,   164,    40,    92,   -64,   -64,
     -64
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -3
static const yytype_int16 yytable[] =
{
      40,    34,    69,     6,    86,   101,    72,   155,    41,   156,
      -2,    44,    78,    25,    61,    26,   106,    45,    46,    47,
      48,    31,    49,   123,   124,   125,   126,   127,   128,   121,
     122,    95,    32,    38,    50,    44,    62,   129,    61,    35,
      96,    45,    46,    47,    48,    37,    49,   119,    92,    40,
      80,   101,   105,    98,    38,    39,   103,    41,    50,    58,
     143,   109,    59,   112,   120,   117,    63,   117,    71,   148,
     149,   150,   151,   152,   101,   154,   133,    66,    44,   124,
     125,   126,   127,   128,    45,    46,    47,    48,    67,    49,
     124,   125,   126,   127,   128,    68,   159,   139,    70,   112,
     171,    50,   103,     7,     8,     9,    10,   176,   177,   178,
     179,   180,   181,   136,   137,    11,    76,    12,    13,    14,
      15,    16,    17,    18,    19,   126,   127,   128,   168,   162,
     163,   164,    74,    38,    39,    82,    83,    20,    38,   172,
      84,   145,   146,    21,   161,   137,    85,    90,   173,   182,
     183,   165,   166,   167,   124,   125,   126,   127,   128,   124,
     125,   126,   127,   128,    94,   186,   147,   170,   124,   125,
     126,   127,   128,    29,    30,    99,    50,   107,   110,   111,
     115,   114,   135,   138,   140,   153,   157,   160,   128,   184,
     169,    24,   134,   155,   185,    93,   187,   118,    81,   158,
     174,   141,   100,     0,     0,     0,   142,     0,    73,     0,
       0,     0,     0,     0,     0,    79
};

static const yytype_int16 yycheck[] =
{
      18,    13,    45,     0,    59,    68,    49,    40,    18,    42,
       0,     5,    55,    28,    26,    28,    71,    11,    12,    13,
      14,    28,    16,    31,    32,    33,    34,    35,    36,    84,
      85,    31,    30,    25,    28,     5,    40,    45,    50,    28,
      40,    11,    12,    13,    14,    28,    16,    39,    60,    67,
      44,   114,    70,    65,    25,    26,    68,    67,    28,    28,
     115,    73,    30,    75,    82,    77,    28,    79,    38,   124,
     125,   126,   127,   128,   137,   130,    94,    40,     5,    32,
      33,    34,    35,    36,    11,    12,    13,    14,    40,    16,
      32,    33,    34,    35,    36,    38,   139,    39,    38,   111,
     155,    28,   114,     3,     4,     5,     6,   162,   163,   164,
     165,   166,   167,    39,    40,    15,     4,    17,    18,    19,
      20,    21,    22,    23,    24,    34,    35,    36,   146,     7,
       8,     9,    43,    25,    26,    38,    28,    37,    25,   157,
      32,    39,    40,    43,    39,    40,    38,    41,   160,    39,
      40,    29,    30,    31,    32,    33,    34,    35,    36,    32,
      33,    34,    35,    36,    38,   183,    39,    31,    32,    33,
      34,    35,    36,     9,    10,    28,    28,    45,    27,    37,
      10,    38,    28,    39,    28,    45,    40,    38,    36,    39,
      45,     5,    96,    40,    39,    62,    39,    79,    57,   137,
     160,   111,    67,    -1,    -1,    -1,   114,    -1,    50,    -1,
      -1,    -1,    -1,    -1,    -1,    56
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    47,    48,    49,    74,    80,     0,     3,     4,     5,
       6,    15,    17,    18,    19,    20,    21,    22,    23,    24,
      37,    43,    50,    59,    48,    28,    28,    53,    81,    81,
      81,    28,    30,    71,    74,    28,    85,    28,    25,    26,
      76,    77,    78,    86,     5,    11,    12,    13,    14,    16,
      28,    60,    61,    62,    64,    65,    68,    79,    28,    30,
      54,    74,    40,    28,    51,    52,    40,    40,    38,    61,
      38,    38,    61,    54,    43,    66,     4,    63,    61,    63,
      44,    60,    38,    28,    32,    38,    56,    57,    76,    77,
      41,    55,    74,    53,    38,    31,    40,    72,    74,    28,
      78,    68,    70,    74,    83,    76,    56,    45,    69,    74,
      27,    37,    74,    75,    38,    10,    67,    74,    67,    39,
      76,    56,    56,    31,    32,    33,    34,    35,    36,    45,
      73,    56,    82,    76,    51,    28,    39,    40,    39,    39,
      28,    75,    83,    56,    58,    39,    40,    39,    56,    56,
      56,    56,    56,    45,    56,    40,    42,    40,    70,    61,
      38,    39,     7,     8,     9,    29,    30,    31,    76,    45,
      31,    56,    76,    74,    82,    84,    56,    56,    56,    56,
      56,    56,    39,    40,    39,    39,    76,    39
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
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
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
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       , ret);
      YYFPRINTF (stderr, "\n");
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


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Location data for the lookahead symbol.  */
YYLTYPE yylloc;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

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
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.
       `yyls': related to locations.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[2];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

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
  yylloc.first_column = yylloc.last_column = 1;
#endif

/* User initialization code.  */

/* Line 1242 of yacc.c  */
#line 61 "pbparser.ypp"
{
  theRefPool.clear();
}

/* Line 1242 of yacc.c  */
#line 1497 "pbparser.cpp"

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
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
	YYSTACK_RELOCATE (yyls_alloc, yyls);
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

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
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

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
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

/* Line 1455 of yacc.c  */
#line 96 "pbparser.ypp"
    {
  ret = (yyvsp[(1) - (1)].transforms);
  clearParserCaches();
}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 101 "pbparser.ypp"
    {
   ((yyval.transform)=(yyvsp[(1) - (2)].transform))->setRules(*(yyvsp[(2) - (2)].rules)); 
}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 105 "pbparser.ypp"
    {(yyval.transform)=REFALLOC(Transform());}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 106 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (2)].transform)); }
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 107 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (2)].transform))->markMain(); }
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 108 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->setName((yyvsp[(3) - (3)].str));     }
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 109 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->addParams(*(yyvsp[(3) - (3)].freevars));    }
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 110 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->addFrom(*(yyvsp[(3) - (3)].matrixdefs));   }
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 111 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->addThrough(*(yyvsp[(3) - (3)].matrixdefs));}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 112 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->addTo(*(yyvsp[(3) - (3)].matrixdefs));     }
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 113 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (5)].transform))->addTemplateArg(*(yyvsp[(4) - (5)].templateargs)); }
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 115 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->setAccuracyMetric((yyvsp[(3) - (3)].str));}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 116 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->setAccuracyBins(*(yyvsp[(3) - (3)].doublelist));}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 117 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->setGenerator((yyvsp[(3) - (3)].str));}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 120 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->addConfigItem((yyvsp[(2) - (3)].i), (yyvsp[(3) - (3)].str)); }
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 122 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (5)].transform))->addConfigItem((yyvsp[(2) - (5)].i), (yyvsp[(3) - (5)].str)); }
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 124 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (6)].transform))->addConfigItem((yyvsp[(2) - (6)].i), (yyvsp[(3) - (6)].str), (yyvsp[(5) - (6)].i)); }
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 126 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (8)].transform))->addConfigItem((yyvsp[(2) - (8)].i), (yyvsp[(3) - (8)].str), (yyvsp[(5) - (8)].i), (yyvsp[(7) - (8)].i)); }
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 128 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (10)].transform))->addConfigItem((yyvsp[(2) - (10)].i), (yyvsp[(3) - (10)].str), (yyvsp[(5) - (10)].i), (yyvsp[(7) - (10)].i), (yyvsp[(9) - (10)].i)); }
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 131 "pbparser.ypp"
    { (yyval.i)=ConfigItem::FLAG_USER; }
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 132 "pbparser.ypp"
    { (yyval.i)=ConfigItem::FLAG_USER|ConfigItem::FLAG_TUNABLE; }
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 133 "pbparser.ypp"
    { (yyval.i)=ConfigItem::FLAG_USER|ConfigItem::FLAG_ACCURACY|ConfigItem::FLAG_SIZESPECIFIC; }
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 136 "pbparser.ypp"
    {
  (yyval.templatearg)=REFALLOC(TemplateArg((yyvsp[(1) - (6)].str), (yyvsp[(3) - (6)].i), (yyvsp[(5) - (6)].i)));
}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 139 "pbparser.ypp"
    { ((yyval.templateargs)=REFALLOC(TemplateArgList()))->push_back((yyvsp[(1) - (1)].templatearg)); }
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 140 "pbparser.ypp"
    { ((yyval.templateargs)=(yyvsp[(1) - (3)].templateargs))->push_back((yyvsp[(3) - (3)].templatearg)); }
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 145 "pbparser.ypp"
    { (yyval.matrixdef)=REFALLOC(MatrixDef((yyvsp[(1) - (3)].str),*(yyvsp[(2) - (3)].formulas),*(yyvsp[(3) - (3)].formulas))); }
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 147 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList())); }
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 148 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(2) - (3)].formula)); }
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 150 "pbparser.ypp"
    { 
  (yyval.formulas)=REFALLOC(FormulaList()); 
  (yyval.formulas)->push_back((yyvsp[(2) - (5)].formula));
  (yyval.formulas)->push_back((yyvsp[(4) - (5)].formula));
}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 156 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 157 "pbparser.ypp"
    { (yyval.formulas)=(yyvsp[(2) - (3)].formulas); }
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 159 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaVariable((yyvsp[(1) - (1)].str)) ); }
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 160 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaInteger( (yyvsp[(1) - (1)].i)) ); }
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 161 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaFloat(  (yyvsp[(1) - (1)].d)) );  }
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 162 "pbparser.ypp"
    { (yyval.formula)=(yyvsp[(2) - (3)].formula); }
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 163 "pbparser.ypp"
    { (yyval.formula)=(yyvsp[(1) - (1)].formula); }
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 165 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'+'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 166 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'-'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 167 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'*'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 168 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'/'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 169 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'^'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 170 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'-'>(FormulaInteger::zero(),(yyvsp[(2) - (2)].formula))); }
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 172 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'='>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 173 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'='>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 174 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'<'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 175 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'>'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 176 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaLE((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 177 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaGE((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 180 "pbparser.ypp"
    { (yyval.rules)=(yyvsp[(2) - (3)].rules); }
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 182 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(1) - (3)].rule))->setBody((yyvsp[(2) - (3)].str)); }
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 184 "pbparser.ypp"
    { (yyval.rule)=(yyvsp[(1) - (1)].rule); }
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 185 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(2) - (2)].rule))->setPriority((yyvsp[(1) - (2)].i)); }
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 186 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(2) - (2)].rule))->addRotations(RuleFlags::ROTATE); }
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 187 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(2) - (2)].rule))->markRecursive(); }
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 188 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(5) - (5)].rule))->markRecursive((yyvsp[(3) - (5)].formula)); }
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 190 "pbparser.ypp"
    { (yyval.rule)=REFALLOC(UserRule((yyvsp[(1) - (3)].region),  *(yyvsp[(2) - (3)].regions), *(yyvsp[(3) - (3)].formulas))); }
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 191 "pbparser.ypp"
    { (yyval.rule)=REFALLOC(UserRule(*(yyvsp[(1) - (3)].regions), *(yyvsp[(2) - (3)].regions), *(yyvsp[(3) - (3)].formulas))); }
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 193 "pbparser.ypp"
    { (yyval.regions)=(yyvsp[(3) - (4)].regions); }
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 194 "pbparser.ypp"
    { (yyval.regions)=(yyvsp[(3) - (4)].regions); }
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 196 "pbparser.ypp"
    {(yyval.i)=RuleFlags::PRIORITY_PRIMARY;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 197 "pbparser.ypp"
    {(yyval.i)=RuleFlags::PRIORITY_SECONDARY;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 198 "pbparser.ypp"
    {(yyval.i)=(yyvsp[(3) - (4)].i);}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 200 "pbparser.ypp"
    { (yyval.str)=(yyvsp[(2) - (2)].str); }
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 202 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 203 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(2) - (2)].formula)); }
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 205 "pbparser.ypp"
    { (yyval.region)=REFALLOC(Region((yyvsp[(1) - (3)].str),*(yyvsp[(2) - (3)].formulas), (yyvsp[(3) - (3)].str_formulas).str,*(yyvsp[(3) - (3)].str_formulas).formulas)); }
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 207 "pbparser.ypp"
    { (yyval.str_formulas).str=(yyvsp[(2) - (5)].str); (yyval.str_formulas).formulas=(yyvsp[(4) - (5)].formulas);}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 208 "pbparser.ypp"
    { (yyval.str_formulas).str="all"; (yyval.str_formulas).formulas=REFALLOC(FormulaList());}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 210 "pbparser.ypp"
    { ((yyval.region)=(yyvsp[(1) - (2)].region))->setName((yyvsp[(2) - (2)].str)); }
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 218 "pbparser.ypp"
    { (yyval.i)=jalib::StringToX<int>((yyvsp[(1) - (1)].str)); }
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 219 "pbparser.ypp"
    { (yyval.d)=jalib::StringToX<double>((yyvsp[(1) - (1)].str)); }
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 220 "pbparser.ypp"
    { (yyval.d)=(yyvsp[(1) - (1)].d); }
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 220 "pbparser.ypp"
    { (yyval.d)=(yyvsp[(1) - (1)].i); }
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 222 "pbparser.ypp"
    { ((yyval.rules)=REFALLOC(RuleList()))->push_back((yyvsp[(1) - (1)].rule)); }
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 223 "pbparser.ypp"
    { ((yyval.rules)=(yyvsp[(1) - (2)].rules))->push_back((yyvsp[(2) - (2)].rule)); }
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 225 "pbparser.ypp"
    { ((yyval.transforms)=REFALLOC(TransformList()))->push_back((yyvsp[(1) - (1)].transform)); }
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 226 "pbparser.ypp"
    { ((yyval.transforms)=(yyvsp[(1) - (2)].transforms))->push_back((yyvsp[(2) - (2)].transform)); }
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 228 "pbparser.ypp"
    { ((yyval.matrixdefs)=REFALLOC(MatrixDefList()))->push_back((yyvsp[(1) - (1)].matrixdef)); }
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 229 "pbparser.ypp"
    { ((yyval.matrixdefs)=(yyvsp[(1) - (3)].matrixdefs))->push_back((yyvsp[(3) - (3)].matrixdef));                      }
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 231 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(1) - (1)].formula)); }
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 232 "pbparser.ypp"
    { ((yyval.formulas)=(yyvsp[(1) - (3)].formulas))->push_back((yyvsp[(3) - (3)].formula)); }
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 234 "pbparser.ypp"
    { ((yyval.regions)=REFALLOC(RegionList())); }
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 235 "pbparser.ypp"
    { ((yyval.regions)=REFALLOC(RegionList()))->push_back((yyvsp[(1) - (1)].region)); }
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 236 "pbparser.ypp"
    { ((yyval.regions)=(yyvsp[(1) - (3)].regions))->push_back((yyvsp[(3) - (3)].region)); }
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 239 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 240 "pbparser.ypp"
    { (yyval.formulas)=(yyvsp[(1) - (1)].formulas); }
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 243 "pbparser.ypp"
    { ((yyval.freevars)=REFALLOC(OrderedFreeVars()))->push_back((yyvsp[(1) - (1)].str)); }
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 244 "pbparser.ypp"
    { ((yyval.freevars)=(yyvsp[(1) - (3)].freevars))->push_back((yyvsp[(3) - (3)].str)); }
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 246 "pbparser.ypp"
    { ((yyval.doublelist)=REFALLOC(DoubleList()))->push_back((yyvsp[(1) - (1)].d)); }
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 247 "pbparser.ypp"
    { ((yyval.doublelist)=(yyvsp[(1) - (3)].doublelist))->push_back((yyvsp[(3) - (3)].d)); }
    break;



/* Line 1455 of yacc.c  */
#line 2325 "pbparser.cpp"
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
      /* If just tried and failed to reuse lookahead token after an
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

  /* Else will try to reuse lookahead token after shifting the error
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

  *++yyvsp = yylval;

  yyerror_range[1] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
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

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (ret, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
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



/* Line 1675 of yacc.c  */
#line 249 "pbparser.ypp"


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



