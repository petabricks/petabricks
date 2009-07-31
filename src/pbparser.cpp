
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
     TOK_INTEGER = 279,
     TOK_FLOAT = 280,
     TOK_RULEBODY = 281,
     IDENT = 282
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
#define TOK_INTEGER 279
#define TOK_FLOAT 280
#define TOK_RULEBODY 281
#define IDENT 282




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
  petabricks::TestCase*        testcase;
  petabricks::TemplateArg*     templatearg;
  petabricks::TemplateArgList* templateargs;
  petabricks::DoubleList*      doublelist;
  struct { const char* str; petabricks::FormulaList* formulas; } str_formulas; 



/* Line 214 of yacc.c  */
#line 227 "pbparser.cpp"
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
#line 252 "pbparser.cpp"

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
#define YYLAST   213

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  45
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  40
/* YYNRULES -- Number of rules.  */
#define YYNRULES  98
/* YYNRULES -- Number of states.  */
#define YYNSTATES  195

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   282

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      37,    38,    33,    32,    39,    31,    44,    34,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    36,
      29,    28,    30,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    40,     2,    41,    35,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    42,     2,    43,     2,     2,     2,     2,
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
      25,    26,    27
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    10,    13,    16,    20,    24,
      28,    32,    36,    42,    46,    50,    54,    61,    70,    81,
      88,    97,   108,   115,   117,   121,   125,   127,   131,   137,
     139,   143,   145,   147,   149,   153,   155,   159,   163,   167,
     171,   175,   178,   182,   186,   190,   194,   198,   202,   206,
     210,   212,   215,   218,   221,   227,   231,   235,   240,   245,
     247,   249,   254,   257,   259,   262,   266,   272,   274,   277,
     279,   281,   283,   285,   288,   292,   293,   296,   298,   300,
     302,   304,   306,   308,   311,   313,   316,   318,   322,   324,
     328,   330,   332,   336,   338,   340,   342,   346,   348
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      46,     0,    -1,    78,    -1,    48,    57,    -1,    72,    -1,
      48,    36,    -1,    48,    15,    -1,    48,     3,    27,    -1,
      48,    21,    83,    -1,    48,     4,    79,    -1,    48,     6,
      79,    -1,    48,     5,    79,    -1,    48,    18,    69,    50,
      70,    -1,    48,    22,    27,    -1,    48,    23,    84,    -1,
      48,    17,    27,    -1,    48,    20,    27,    37,    74,    38,
      -1,    48,    20,    27,    37,    74,    39,    74,    38,    -1,
      48,    20,    27,    37,    74,    39,    74,    39,    74,    38,
      -1,    48,    19,    27,    37,    74,    38,    -1,    48,    19,
      27,    37,    74,    39,    74,    38,    -1,    48,    19,    27,
      37,    74,    39,    74,    39,    74,    38,    -1,    27,    37,
      74,    39,    74,    38,    -1,    49,    -1,    50,    39,    49,
      -1,    27,    52,    53,    -1,    72,    -1,    29,    54,    30,
      -1,    29,    54,    71,    54,    30,    -1,    72,    -1,    40,
      80,    41,    -1,    27,    -1,    74,    -1,    75,    -1,    37,
      54,    38,    -1,    55,    -1,    54,    32,    54,    -1,    54,
      31,    54,    -1,    54,    33,    54,    -1,    54,    34,    54,
      -1,    54,    35,    54,    -1,    31,    54,    -1,    54,    28,
      54,    -1,    54,     9,    54,    -1,    54,    29,    54,    -1,
      54,    30,    54,    -1,    54,     7,    54,    -1,    54,     8,
      54,    -1,    42,    77,    43,    -1,    59,    64,    73,    -1,
      60,    -1,    63,    59,    -1,    11,    59,    -1,    16,    59,
      -1,    16,    37,    54,    38,    59,    -1,    66,    61,    65,
      -1,    62,    61,    65,    -1,     4,    37,    81,    38,    -1,
       5,    37,    81,    38,    -1,    12,    -1,    13,    -1,    14,
      37,    74,    38,    -1,    42,    26,    -1,    72,    -1,    10,
      56,    -1,    27,    52,    67,    -1,    44,    27,    37,    82,
      38,    -1,    72,    -1,    66,    27,    -1,    72,    -1,    29,
      -1,    72,    -1,    30,    -1,    44,    44,    -1,    44,    44,
      44,    -1,    -1,    36,    73,    -1,    72,    -1,    24,    -1,
      25,    -1,    75,    -1,    74,    -1,    58,    -1,    77,    58,
      -1,    47,    -1,    78,    47,    -1,    51,    -1,    79,    39,
      51,    -1,    54,    -1,    80,    39,    54,    -1,    72,    -1,
      68,    -1,    81,    39,    68,    -1,    72,    -1,    80,    -1,
      27,    -1,    83,    39,    27,    -1,    76,    -1,    84,    39,
      76,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    98,    98,   103,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   117,   118,   119,   121,   123,   125,   127,
     129,   131,   134,   137,   138,   143,   145,   146,   147,   154,
     155,   157,   158,   159,   160,   161,   163,   164,   165,   166,
     167,   168,   170,   171,   172,   173,   174,   175,   178,   180,
     182,   183,   184,   185,   186,   188,   189,   191,   192,   194,
     195,   196,   198,   200,   201,   203,   205,   206,   208,   210,
     210,   211,   211,   213,   213,   214,   215,   215,   216,   217,
     218,   218,   220,   221,   223,   224,   226,   227,   229,   230,
     232,   233,   234,   237,   238,   241,   242,   244,   245
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
  "KW_ACCURACYMETRIC", "KW_ACCURACYBINS", "TOK_INTEGER", "TOK_FLOAT",
  "TOK_RULEBODY", "IDENT", "'='", "'<'", "'>'", "'-'", "'+'", "'*'", "'/'",
  "'^'", "';'", "'('", "')'", "','", "'['", "']'", "'{'", "'}'", "'.'",
  "$accept", "Start", "Transform", "TransformHeader", "TemplateArg",
  "TemplateArgs", "MatrixDef", "OptVersion", "OptSize", "Formula",
  "FormulaExpr", "FormulaRelation", "TransformBody", "Rule", "RuleHeader",
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
     275,   276,   277,   278,   279,   280,   281,   282,    61,    60,
      62,    45,    43,    42,    47,    94,    59,    40,    41,    44,
      91,    93,   123,   125,    46
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    45,    46,    47,    48,    48,    48,    48,    48,    48,
      48,    48,    48,    48,    48,    48,    48,    48,    48,    48,
      48,    48,    49,    50,    50,    51,    52,    52,    52,    53,
      53,    54,    54,    54,    54,    54,    55,    55,    55,    55,
      55,    55,    56,    56,    56,    56,    56,    56,    57,    58,
      59,    59,    59,    59,    59,    60,    60,    61,    62,    63,
      63,    63,    64,    65,    65,    66,    67,    67,    68,    69,
      69,    70,    70,    71,    71,    72,    73,    73,    74,    75,
      76,    76,    77,    77,    78,    78,    79,    79,    80,    80,
      81,    81,    81,    82,    82,    83,    83,    84,    84
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     2,     2,     3,     3,     3,
       3,     3,     5,     3,     3,     3,     6,     8,    10,     6,
       8,    10,     6,     1,     3,     3,     1,     3,     5,     1,
       3,     1,     1,     1,     3,     1,     3,     3,     3,     3,
       3,     2,     3,     3,     3,     3,     3,     3,     3,     3,
       1,     2,     2,     2,     5,     3,     3,     4,     4,     1,
       1,     4,     2,     1,     2,     3,     5,     1,     2,     1,
       1,     1,     1,     2,     3,     0,     2,     1,     1,     1,
       1,     1,     1,     2,     1,     2,     1,     3,     1,     3,
       1,     1,     3,     1,     1,     1,     3,     1,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
      75,     0,    84,     0,     4,    75,     1,     0,     0,     0,
       0,     6,     0,    75,     0,     0,     0,     0,     0,     5,
       0,     3,    85,     7,    75,    86,     9,    11,    10,    15,
      70,     0,    69,     0,     0,    95,     8,    13,    78,    79,
      81,    80,    97,    14,     0,     0,    59,    60,     0,     0,
      75,    82,     0,    50,     0,     0,     0,     0,     0,    75,
      26,     0,     0,    23,    75,     0,     0,     0,     0,    75,
      52,     0,     0,    53,    75,     0,    75,     0,    75,    51,
      75,    48,    83,    31,     0,     0,     0,    35,    32,    33,
       0,    25,    29,    87,     0,    72,     0,    12,    71,     0,
       0,    96,    98,     0,    91,    90,     0,     0,     0,     0,
      65,    67,    62,    75,    77,    49,    75,     0,    56,    63,
      55,    41,     0,    27,     0,     0,     0,     0,     0,     0,
       0,    88,     0,     0,    24,    19,     0,    16,     0,    68,
      58,     0,    61,     0,     0,    76,     0,     0,    64,    34,
      37,    36,    38,    39,    40,    73,     0,     0,    30,     0,
       0,     0,    92,    54,    75,    57,     0,     0,     0,     0,
       0,     0,    74,    28,    89,     0,    20,     0,    17,     0,
      93,    94,     0,    46,    47,    43,    42,    44,    45,    22,
       0,     0,    66,    21,    18
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     3,    63,    64,    25,    59,    91,   131,
      87,   148,    21,    51,    52,    53,    78,    54,    55,    76,
     118,    56,   110,   104,    31,    97,   130,     4,   115,    88,
      89,    42,    57,     5,    26,   132,   106,   182,    36,    43
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -62
static const yytype_int16 yypact[] =
{
     -62,    27,   -62,    15,   -62,    31,   -62,    19,    38,    38,
      38,   -62,    40,    44,    42,    48,    52,    61,    36,   -62,
      12,   -62,   -62,   -62,    57,   -62,    70,    70,    70,   -62,
     -62,    83,   -62,    75,    76,   -62,    77,   -62,   -62,   -62,
     -62,   -62,   -62,    78,    82,    12,   -62,   -62,    85,    84,
      57,   -62,    73,   -62,   126,    12,   126,   124,   -15,    91,
     -62,    38,    95,   -62,   -26,   109,   109,   107,    36,   112,
     -62,   109,   -15,   -62,    99,   116,   108,   113,   135,   -62,
     135,   -62,   -62,   -62,   -15,   -15,   146,   -62,   -62,   -62,
     -15,   -62,   -62,   -62,   109,   -62,    83,   -62,   -62,   -32,
      24,   -62,   -62,   119,   -62,   -62,    33,   114,   151,   128,
     -62,   -62,   -62,   108,   -62,   -62,   112,   -15,   -62,   -62,
     -62,    10,   160,   -62,   -15,   -15,   -15,   -15,   -15,   118,
     -15,    59,    17,   117,   -62,   -62,   109,   -62,   109,   -62,
     -62,   112,   -62,    12,   120,   -62,    39,   140,   -62,   -62,
      10,    10,   123,   123,   -62,   121,   169,   -15,   -62,   109,
      63,    66,   -62,   -62,   -15,   -62,   -15,   -15,   -15,   -15,
     -15,   -15,   -62,   -62,    59,   115,   -62,   109,   -62,   109,
     -62,   125,   122,    59,    59,    59,    59,    59,    59,   -62,
     149,   150,   -62,   -62,   -62
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -62,   -62,   158,   -62,   100,   -62,   105,   147,   -62,   -43,
     -62,   -62,   -62,   148,   -44,   -62,   152,   -62,   -62,   -62,
     127,   -61,   -62,    65,   -62,   -62,   -62,   -10,    96,   -18,
     -16,   142,   -62,   -62,    98,    47,    97,   -62,   -62,   -62
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -3
static const yytype_int16 yytable[] =
{
      40,    70,    41,    32,    95,    73,   135,   136,   103,    38,
      39,    79,    83,    96,    60,    86,    84,    44,     7,     8,
       9,    10,    85,    45,    46,    47,    48,     6,    49,   108,
      11,    -2,    12,    13,    14,    15,    16,    17,    18,    50,
      60,   121,   122,   126,   127,   128,    23,    99,   100,    92,
      40,    19,    41,   107,    98,   103,   157,    20,   158,   105,
      38,    39,   137,   138,   111,    24,   114,    29,   119,    33,
     119,   140,   141,    30,   147,    34,   133,   165,   141,    35,
     103,   150,   151,   152,   153,   154,    58,   156,    37,    44,
     124,   125,   126,   127,   128,    45,    46,    47,    48,   163,
      49,   176,   177,   114,   178,   179,   105,    27,    28,    61,
      62,    50,    65,    66,   174,    75,    67,    68,   160,    69,
     161,    72,    71,   183,   184,   185,   186,   187,   188,    44,
      77,    90,    94,    38,   101,    45,    46,    47,    48,    50,
      49,   175,   112,   109,   113,   117,   139,   166,   167,   168,
     116,    50,   142,   189,   180,   144,   159,   164,   128,   190,
     192,   191,   155,    22,   157,   172,    93,    81,   169,   170,
     171,   124,   125,   126,   127,   128,   123,   124,   125,   126,
     127,   128,   124,   125,   126,   127,   128,   193,   194,   143,
     129,   124,   125,   126,   127,   128,   134,    74,   149,   173,
     124,   125,   126,   127,   128,    82,   162,   120,    80,   145,
     102,   181,     0,   146
};

static const yytype_int16 yycheck[] =
{
      18,    45,    18,    13,    30,    49,    38,    39,    69,    24,
      25,    55,    27,    39,    24,    58,    31,     5,     3,     4,
       5,     6,    37,    11,    12,    13,    14,     0,    16,    72,
      15,     0,    17,    18,    19,    20,    21,    22,    23,    27,
      50,    84,    85,    33,    34,    35,    27,    65,    66,    59,
      68,    36,    68,    71,    64,   116,    39,    42,    41,    69,
      24,    25,    38,    39,    74,    27,    76,    27,    78,    27,
      80,    38,    39,    29,   117,    27,    94,    38,    39,    27,
     141,   124,   125,   126,   127,   128,    29,   130,    27,     5,
      31,    32,    33,    34,    35,    11,    12,    13,    14,   143,
      16,    38,    39,   113,    38,    39,   116,     9,    10,    39,
      27,    27,    37,    37,   157,    42,    39,    39,   136,    37,
     138,    37,    37,   166,   167,   168,   169,   170,   171,     5,
       4,    40,    37,    24,    27,    11,    12,    13,    14,    27,
      16,   159,    26,    44,    36,    10,    27,     7,     8,     9,
      37,    27,    38,    38,   164,    27,    39,    37,    35,   177,
      38,   179,    44,     5,    39,    44,    61,    43,    28,    29,
      30,    31,    32,    33,    34,    35,    30,    31,    32,    33,
      34,    35,    31,    32,    33,    34,    35,    38,    38,    38,
      44,    31,    32,    33,    34,    35,    96,    50,    38,    30,
      31,    32,    33,    34,    35,    57,   141,    80,    56,   113,
      68,   164,    -1,   116
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    46,    47,    48,    72,    78,     0,     3,     4,     5,
       6,    15,    17,    18,    19,    20,    21,    22,    23,    36,
      42,    57,    47,    27,    27,    51,    79,    79,    79,    27,
      29,    69,    72,    27,    27,    27,    83,    27,    24,    25,
      74,    75,    76,    84,     5,    11,    12,    13,    14,    16,
      27,    58,    59,    60,    62,    63,    66,    77,    29,    52,
      72,    39,    27,    49,    50,    37,    37,    39,    39,    37,
      59,    37,    37,    59,    52,    42,    64,     4,    61,    59,
      61,    43,    58,    27,    31,    37,    54,    55,    74,    75,
      40,    53,    72,    51,    37,    30,    39,    70,    72,    74,
      74,    27,    76,    66,    68,    72,    81,    74,    54,    44,
      67,    72,    26,    36,    72,    73,    37,    10,    65,    72,
      65,    54,    54,    30,    31,    32,    33,    34,    35,    44,
      71,    54,    80,    74,    49,    38,    39,    38,    39,    27,
      38,    39,    38,    38,    27,    73,    81,    54,    56,    38,
      54,    54,    54,    54,    54,    44,    54,    39,    41,    39,
      74,    74,    68,    59,    37,    38,     7,     8,     9,    28,
      29,    30,    44,    30,    54,    74,    38,    39,    38,    39,
      72,    80,    82,    54,    54,    54,    54,    54,    54,    38,
      74,    74,    38,    38,    38
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
#line 62 "pbparser.ypp"
{
  theRefPool.clear();
}

/* Line 1242 of yacc.c  */
#line 1493 "pbparser.cpp"

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
#line 98 "pbparser.ypp"
    {
  ret = (yyvsp[(1) - (1)].transforms);
  clearParserCaches();
}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 103 "pbparser.ypp"
    {
   ((yyval.transform)=(yyvsp[(1) - (2)].transform))->setRules(*(yyvsp[(2) - (2)].rules)); 
}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 107 "pbparser.ypp"
    {(yyval.transform)=REFALLOC(Transform());}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 108 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (2)].transform)); }
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 109 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (2)].transform))->markMain(); }
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 110 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->setName((yyvsp[(3) - (3)].str));     }
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 111 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->addParams(*(yyvsp[(3) - (3)].freevars));    }
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 112 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->addFrom(*(yyvsp[(3) - (3)].matrixdefs));   }
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 113 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->addThrough(*(yyvsp[(3) - (3)].matrixdefs));}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 114 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->addTo(*(yyvsp[(3) - (3)].matrixdefs));     }
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 115 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (5)].transform))->addTemplateArg(*(yyvsp[(4) - (5)].templateargs)); }
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 117 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->setAccuracyMetric((yyvsp[(3) - (3)].str));}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 118 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->setAccuracyBins(*(yyvsp[(3) - (3)].doublelist));}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 119 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (3)].transform))->setGenerator((yyvsp[(3) - (3)].str));}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 122 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (6)].transform))->addConfig((yyvsp[(3) - (6)].str), (yyvsp[(5) - (6)].i)); }
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 124 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (8)].transform))->addConfig((yyvsp[(3) - (8)].str), (yyvsp[(5) - (8)].i), (yyvsp[(7) - (8)].i)); }
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 126 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (10)].transform))->addConfig((yyvsp[(3) - (10)].str), (yyvsp[(5) - (10)].i), (yyvsp[(7) - (10)].i), (yyvsp[(9) - (10)].i)); }
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 128 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (6)].transform))->addTunable((yyvsp[(3) - (6)].str), (yyvsp[(5) - (6)].i)); }
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 130 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (8)].transform))->addTunable((yyvsp[(3) - (8)].str), (yyvsp[(5) - (8)].i), (yyvsp[(7) - (8)].i)); }
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 132 "pbparser.ypp"
    { ((yyval.transform)=(yyvsp[(1) - (10)].transform))->addTunable((yyvsp[(3) - (10)].str), (yyvsp[(5) - (10)].i), (yyvsp[(7) - (10)].i), (yyvsp[(9) - (10)].i)); }
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 134 "pbparser.ypp"
    {
  (yyval.templatearg)=REFALLOC(TemplateArg((yyvsp[(1) - (6)].str), (yyvsp[(3) - (6)].i), (yyvsp[(5) - (6)].i)));
}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 137 "pbparser.ypp"
    { ((yyval.templateargs)=REFALLOC(TemplateArgList()))->push_back((yyvsp[(1) - (1)].templatearg)); }
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 138 "pbparser.ypp"
    { ((yyval.templateargs)=(yyvsp[(1) - (3)].templateargs))->push_back((yyvsp[(3) - (3)].templatearg)); }
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 143 "pbparser.ypp"
    { (yyval.matrixdef)=REFALLOC(MatrixDef((yyvsp[(1) - (3)].str),*(yyvsp[(2) - (3)].formulas),*(yyvsp[(3) - (3)].formulas))); }
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 145 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList())); }
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 146 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(2) - (3)].formula)); }
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 148 "pbparser.ypp"
    { 
  (yyval.formulas)=REFALLOC(FormulaList()); 
  (yyval.formulas)->push_back((yyvsp[(2) - (5)].formula));
  (yyval.formulas)->push_back((yyvsp[(4) - (5)].formula));
}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 154 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 155 "pbparser.ypp"
    { (yyval.formulas)=(yyvsp[(2) - (3)].formulas); }
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 157 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaVariable((yyvsp[(1) - (1)].str)) ); }
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 158 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaInteger( (yyvsp[(1) - (1)].i)) ); }
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 159 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaFloat(  (yyvsp[(1) - (1)].d)) );  }
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 160 "pbparser.ypp"
    { (yyval.formula)=(yyvsp[(2) - (3)].formula); }
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 161 "pbparser.ypp"
    { (yyval.formula)=(yyvsp[(1) - (1)].formula); }
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 163 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'+'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 164 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'-'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 165 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'*'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 166 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'/'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 167 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'^'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 168 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'-'>(FormulaInteger::zero(),(yyvsp[(2) - (2)].formula))); }
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 170 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'='>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 171 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'='>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 172 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'<'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 173 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'>'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 174 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaLE((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 175 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaGE((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 178 "pbparser.ypp"
    { (yyval.rules)=(yyvsp[(2) - (3)].rules); }
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 180 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(1) - (3)].rule))->setBody((yyvsp[(2) - (3)].str)); }
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 182 "pbparser.ypp"
    { (yyval.rule)=(yyvsp[(1) - (1)].rule); }
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 183 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(2) - (2)].rule))->setPriority((yyvsp[(1) - (2)].i)); }
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 184 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(2) - (2)].rule))->addRotations(RuleFlags::ROTATE); }
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 185 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(2) - (2)].rule))->markRecursive(); }
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 186 "pbparser.ypp"
    { ((yyval.rule)=(yyvsp[(5) - (5)].rule))->markRecursive((yyvsp[(3) - (5)].formula)); }
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 188 "pbparser.ypp"
    { (yyval.rule)=REFALLOC(UserRule((yyvsp[(1) - (3)].region),  *(yyvsp[(2) - (3)].regions), *(yyvsp[(3) - (3)].formulas))); }
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 189 "pbparser.ypp"
    { (yyval.rule)=REFALLOC(UserRule(*(yyvsp[(1) - (3)].regions), *(yyvsp[(2) - (3)].regions), *(yyvsp[(3) - (3)].formulas))); }
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 191 "pbparser.ypp"
    { (yyval.regions)=(yyvsp[(3) - (4)].regions); }
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 192 "pbparser.ypp"
    { (yyval.regions)=(yyvsp[(3) - (4)].regions); }
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 194 "pbparser.ypp"
    {(yyval.i)=RuleFlags::PRIORITY_PRIMARY;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 195 "pbparser.ypp"
    {(yyval.i)=RuleFlags::PRIORITY_SECONDARY;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 196 "pbparser.ypp"
    {(yyval.i)=(yyvsp[(3) - (4)].i);}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 198 "pbparser.ypp"
    { (yyval.str)=(yyvsp[(2) - (2)].str); }
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 200 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 201 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(2) - (2)].formula)); }
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 203 "pbparser.ypp"
    { (yyval.region)=REFALLOC(Region((yyvsp[(1) - (3)].str),*(yyvsp[(2) - (3)].formulas), (yyvsp[(3) - (3)].str_formulas).str,*(yyvsp[(3) - (3)].str_formulas).formulas)); }
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 205 "pbparser.ypp"
    { (yyval.str_formulas).str=(yyvsp[(2) - (5)].str); (yyval.str_formulas).formulas=(yyvsp[(4) - (5)].formulas);}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 206 "pbparser.ypp"
    { (yyval.str_formulas).str="all"; (yyval.str_formulas).formulas=REFALLOC(FormulaList());}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 208 "pbparser.ypp"
    { ((yyval.region)=(yyvsp[(1) - (2)].region))->setName((yyvsp[(2) - (2)].str)); }
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 216 "pbparser.ypp"
    { (yyval.i)=jalib::StringToX<int>((yyvsp[(1) - (1)].str)); }
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 217 "pbparser.ypp"
    { (yyval.d)=jalib::StringToX<double>((yyvsp[(1) - (1)].str)); }
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 218 "pbparser.ypp"
    { (yyval.d)=(yyvsp[(1) - (1)].d); }
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 218 "pbparser.ypp"
    { (yyval.d)=(yyvsp[(1) - (1)].i); }
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 220 "pbparser.ypp"
    { ((yyval.rules)=REFALLOC(RuleList()))->push_back((yyvsp[(1) - (1)].rule)); }
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 221 "pbparser.ypp"
    { ((yyval.rules)=(yyvsp[(1) - (2)].rules))->push_back((yyvsp[(2) - (2)].rule)); }
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 223 "pbparser.ypp"
    { ((yyval.transforms)=REFALLOC(TransformList()))->push_back((yyvsp[(1) - (1)].transform)); }
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 224 "pbparser.ypp"
    { ((yyval.transforms)=(yyvsp[(1) - (2)].transforms))->push_back((yyvsp[(2) - (2)].transform)); }
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 226 "pbparser.ypp"
    { ((yyval.matrixdefs)=REFALLOC(MatrixDefList()))->push_back((yyvsp[(1) - (1)].matrixdef)); }
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 227 "pbparser.ypp"
    { ((yyval.matrixdefs)=(yyvsp[(1) - (3)].matrixdefs))->push_back((yyvsp[(3) - (3)].matrixdef));                      }
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 229 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(1) - (1)].formula)); }
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 230 "pbparser.ypp"
    { ((yyval.formulas)=(yyvsp[(1) - (3)].formulas))->push_back((yyvsp[(3) - (3)].formula)); }
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 232 "pbparser.ypp"
    { ((yyval.regions)=REFALLOC(RegionList())); }
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 233 "pbparser.ypp"
    { ((yyval.regions)=REFALLOC(RegionList()))->push_back((yyvsp[(1) - (1)].region)); }
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 234 "pbparser.ypp"
    { ((yyval.regions)=(yyvsp[(1) - (3)].regions))->push_back((yyvsp[(3) - (3)].region)); }
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 237 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 238 "pbparser.ypp"
    { (yyval.formulas)=(yyvsp[(1) - (1)].formulas); }
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 241 "pbparser.ypp"
    { ((yyval.freevars)=REFALLOC(OrderedFreeVars()))->push_back((yyvsp[(1) - (1)].str)); }
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 242 "pbparser.ypp"
    { ((yyval.freevars)=(yyvsp[(1) - (3)].freevars))->push_back((yyvsp[(3) - (3)].str)); }
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 244 "pbparser.ypp"
    { ((yyval.doublelist)=REFALLOC(DoubleList()))->push_back((yyvsp[(1) - (1)].d)); }
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 245 "pbparser.ypp"
    { ((yyval.doublelist)=(yyvsp[(1) - (3)].doublelist))->push_back((yyvsp[(3) - (3)].d)); }
    break;



/* Line 1455 of yacc.c  */
#line 2307 "pbparser.cpp"
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
#line 247 "pbparser.ypp"


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



