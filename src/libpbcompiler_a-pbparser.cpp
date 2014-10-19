/* A Bison parser, made by GNU Bison 2.7.12-4996.  */

/* Bison implementation for Yacc-like parsers in C
   
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
#define YYBISON_VERSION "2.7.12-4996"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


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
/* Line 371 of yacc.c  */
#line 27 "pbparser.ypp"


#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif

#include "formula.h"
#include "matrixdef.h"
#include "pbc.h"
#include "region.h"
#include "transform.h"
#include "userrule.h"

#include "common/jconvert.h"
#include "common/srcpos.h"

#include <stdio.h>
#include <map>

#include "libpbcompiler_a-pbparser.h"

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

using namespace petabricks;

extern char* pbtext;//in pblexer.cpp

int yyerror(petabricks::TransformListPtr&, const char* msg){
  JASSERT(false)(yylloc)(pbtext)(msg).Text("parse error"); 
  return 0;
}

// keep a ref to all created objects so they aren't deleted during parsing
// the ref pool is then cleared after each parse finishes
// this is needed because bison parsing types doesn't support constructors/destructors
static jalib::JRefPool theRefPool;
static void clearParserCaches(){
  theRefPool.clear();
}
void pbparser_addRefPoolItem(jalib::JRefCounted* i){
  theRefPool.add(i);
}
#define REFALLOC(args...) TAGPOS(theRefPool.add(new args), yylloc)

//helper to mark objects with locations
#define P(A) TAGPOS(A, yylloc)


#define YYLEX_PARAM &yylval, &yylloc
extern int pblex(YYSTYPE * yylval_param,YYLTYPE * yylloc_param);

/* Line 371 of yacc.c  */
#line 131 "libpbcompiler_a-pbparser.cpp"

# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_PB_Y_TAB_H_INCLUDED
# define YY_PB_Y_TAB_H_INCLUDED
/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int pbdebug;
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
     KW_AND = 266,
     KW_OR = 267,
     KW_PRIMARY = 268,
     KW_SECONDARY = 269,
     KW_PRIORITY = 270,
     KW_ROTATABLE = 271,
     KW_MAIN = 272,
     KW_RECURSIVE = 273,
     KW_DUPLICATE = 274,
     KW_MEMOIZED = 275,
     KW_GENERATOR = 276,
     KW_TEMPLATE = 277,
     KW_TUNABLE = 278,
     KW_CONFIG = 279,
     KW_PARAM = 280,
     KW_ACCURACYMETRIC = 281,
     KW_ACCURACYBINS = 282,
     KW_ACCURACYVARIABLE = 283,
     KW_RULE = 284,
     KW_INPUTFEATURE = 285,
     TOK_INTEGER = 286,
     TOK_FLOAT = 287,
     TOK_RULEBODY = 288,
     IDENT = 289
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
#define KW_AND 266
#define KW_OR 267
#define KW_PRIMARY 268
#define KW_SECONDARY 269
#define KW_PRIORITY 270
#define KW_ROTATABLE 271
#define KW_MAIN 272
#define KW_RECURSIVE 273
#define KW_DUPLICATE 274
#define KW_MEMOIZED 275
#define KW_GENERATOR 276
#define KW_TEMPLATE 277
#define KW_TUNABLE 278
#define KW_CONFIG 279
#define KW_PARAM 280
#define KW_ACCURACYMETRIC 281
#define KW_ACCURACYBINS 282
#define KW_ACCURACYVARIABLE 283
#define KW_RULE 284
#define KW_INPUTFEATURE 285
#define TOK_INTEGER 286
#define TOK_FLOAT 287
#define TOK_RULEBODY 288
#define IDENT 289



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 387 of yacc.c  */
#line 88 "pbparser.ypp"

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
  petabricks::ConfigItem*      configitem;
  struct { const char* str; petabricks::FormulaList* formulas; } str_formulas; 


/* Line 387 of yacc.c  */
#line 265 "libpbcompiler_a-pbparser.cpp"
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

extern YYSTYPE pblval;
extern YYLTYPE pblloc;
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int pbparse (void *YYPARSE_PARAM);
#else
int pbparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int pbparse (petabricks::TransformListPtr& ret);
#else
int pbparse ();
#endif
#endif /* ! YYPARSE_PARAM */

#endif /* !YY_PB_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

/* Line 390 of yacc.c  */
#line 306 "libpbcompiler_a-pbparser.cpp"

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
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef __attribute__
/* This feature is available in gcc versions 2.5 and later.  */
# if (! defined __GNUC__ || __GNUC__ < 2 \
      || (__GNUC__ == 2 && __GNUC_MINOR__ < 5))
#  define __attribute__(Spec) /* empty */
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif


/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(N) (N)
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
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

# define YYCOPY_NEEDED 1

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

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  6
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   259

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  52
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  45
/* YYNRULES -- Number of rules.  */
#define YYNRULES  119
/* YYNRULES -- Number of states.  */
#define YYNSTATES  232

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   289

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      44,    45,    40,    39,    46,    38,    51,    41,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    43,
      36,    35,    37,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    47,     2,    48,    42,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    49,     2,    50,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    11,    13,    16,    19,    23,
      27,    31,    35,    39,    45,    49,    53,    57,    61,    64,
      67,    69,    73,    78,    85,    94,    99,   106,   115,   118,
     124,   130,   132,   134,   136,   143,   145,   149,   153,   155,
     159,   165,   167,   171,   173,   175,   177,   181,   183,   187,
     191,   195,   199,   203,   206,   210,   214,   218,   222,   226,
     230,   234,   238,   242,   244,   247,   250,   253,   259,   269,
     273,   278,   283,   288,   293,   298,   300,   302,   304,   309,
     312,   314,   317,   319,   323,   327,   333,   335,   338,   343,
     345,   347,   349,   351,   354,   358,   359,   362,   364,   366,
     368,   370,   372,   375,   377,   380,   382,   385,   387,   391,
     393,   397,   399,   401,   405,   407,   409,   411,   415,   417
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      53,     0,    -1,    90,    -1,    55,    67,    -1,    55,    43,
      -1,    84,    -1,    55,    17,    -1,    55,    20,    -1,    55,
       3,    34,    -1,    55,    25,    95,    -1,    55,     4,    91,
      -1,    55,     6,    91,    -1,    55,     5,    91,    -1,    55,
      22,    81,    60,    82,    -1,    55,    26,    34,    -1,    55,
      27,    96,    -1,    55,    21,    34,    -1,    55,    30,    34,
      -1,    55,    56,    -1,    58,    57,    -1,    34,    -1,    34,
      44,    45,    -1,    34,    44,    86,    45,    -1,    34,    44,
      86,    46,    86,    45,    -1,    34,    44,    86,    46,    86,
      46,    86,    45,    -1,    34,    44,    87,    45,    -1,    34,
      44,    87,    46,    87,    45,    -1,    34,    44,    87,    46,
      87,    46,    87,    45,    -1,    34,    57,    -1,    34,    44,
      86,    45,    57,    -1,    34,    44,    87,    45,    57,    -1,
      24,    -1,    23,    -1,    28,    -1,    34,    44,    86,    46,
      86,    45,    -1,    59,    -1,    60,    46,    59,    -1,    34,
      62,    63,    -1,    84,    -1,    36,    64,    37,    -1,    36,
      64,    83,    64,    37,    -1,    84,    -1,    47,    92,    48,
      -1,    34,    -1,    86,    -1,    87,    -1,    44,    64,    45,
      -1,    65,    -1,    64,    39,    64,    -1,    64,    38,    64,
      -1,    64,    40,    64,    -1,    64,    41,    64,    -1,    64,
      42,    64,    -1,    38,    64,    -1,    64,    35,    64,    -1,
      64,     9,    64,    -1,    64,    36,    64,    -1,    64,    37,
      64,    -1,    64,     7,    64,    -1,    64,     8,    64,    -1,
      66,    12,    64,    -1,    49,    89,    50,    -1,    69,    75,
      85,    -1,    70,    -1,    74,    69,    -1,    16,    69,    -1,
      18,    69,    -1,    18,    44,    64,    45,    69,    -1,    19,
      44,    34,    46,    86,    46,    86,    45,    69,    -1,    29,
      34,    69,    -1,    78,    71,    73,    76,    -1,    72,    71,
      73,    76,    -1,     4,    44,    93,    45,    -1,     5,    44,
      93,    45,    -1,     6,    44,    91,    45,    -1,    84,    -1,
      13,    -1,    14,    -1,    15,    44,    86,    45,    -1,    49,
      33,    -1,    84,    -1,    10,    77,    -1,    66,    -1,    77,
      11,    66,    -1,    34,    62,    79,    -1,    51,    34,    44,
      94,    45,    -1,    84,    -1,    78,    34,    -1,    78,    34,
      35,    64,    -1,    84,    -1,    36,    -1,    84,    -1,    37,
      -1,    51,    51,    -1,    51,    51,    51,    -1,    -1,    43,
      85,    -1,    84,    -1,    31,    -1,    32,    -1,    87,    -1,
      86,    -1,    38,    88,    -1,    68,    -1,    89,    68,    -1,
      54,    -1,    90,    54,    -1,    61,    -1,    91,    46,    61,
      -1,    64,    -1,    92,    46,    64,    -1,    84,    -1,    80,
      -1,    93,    46,    80,    -1,    84,    -1,    92,    -1,    34,
      -1,    95,    46,    34,    -1,    88,    -1,    96,    46,    88,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   151,   151,   156,   160,   164,   166,   167,   168,   169,
     170,   171,   172,   173,   175,   176,   177,   178,   179,   181,
     187,   189,   191,   193,   195,   197,   199,   201,   205,   220,
     236,   250,   251,   252,   256,   259,   260,   265,   269,   270,
     271,   278,   279,   281,   282,   283,   284,   285,   287,   288,
     289,   290,   291,   292,   294,   295,   296,   297,   298,   299,
     300,   302,   304,   306,   307,   308,   309,   310,   311,   312,
     314,   315,   317,   318,   319,   320,   322,   323,   324,   326,
     328,   329,   332,   333,   335,   337,   338,   340,   341,   343,
     343,   344,   344,   346,   346,   347,   348,   348,   349,   350,
     351,   351,   351,   353,   354,   356,   357,   359,   360,   362,
     363,   365,   366,   367,   370,   371,   374,   375,   377,   378
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "KW_TRANSFORM", "KW_FROM", "KW_TO",
  "KW_THROUGH", "KW_LE", "KW_GE", "KW_EQ", "KW_WHERE", "KW_AND", "KW_OR",
  "KW_PRIMARY", "KW_SECONDARY", "KW_PRIORITY", "KW_ROTATABLE", "KW_MAIN",
  "KW_RECURSIVE", "KW_DUPLICATE", "KW_MEMOIZED", "KW_GENERATOR",
  "KW_TEMPLATE", "KW_TUNABLE", "KW_CONFIG", "KW_PARAM",
  "KW_ACCURACYMETRIC", "KW_ACCURACYBINS", "KW_ACCURACYVARIABLE", "KW_RULE",
  "KW_INPUTFEATURE", "TOK_INTEGER", "TOK_FLOAT", "TOK_RULEBODY", "IDENT",
  "'='", "'<'", "'>'", "'-'", "'+'", "'*'", "'/'", "'^'", "';'", "'('",
  "')'", "','", "'['", "']'", "'{'", "'}'", "'.'", "$accept", "Start",
  "Transform", "TransformHeader", "ConfigItemLine", "ConfigItem",
  "ConfigItemType", "TemplateArg", "TemplateArgs", "MatrixDef",
  "OptVersion", "OptSize", "Formula", "FormulaExpr", "FormulaRelation",
  "TransformBody", "Rule", "RuleHeader", "BaseRuleHeader",
  "RuleHeaderFrom", "RuleHeaderTo", "RuleHeaderThrough", "PriorityFlag",
  "RuleBody", "OptWhere", "FormulaRelations", "Region", "RegionAccessor",
  "NamedRegion", "OptLT", "OptGT", "Dots", "Nil", "OptSemiCol", "Integer",
  "Float", "FloatOrInt", "RuleList", "TransformList", "MatrixDefList",
  "FormulaList", "NamedRegionList", "OptFormulaList", "FreeVars",
  "FloatList", YY_NULL
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,    61,    60,    62,    45,    43,
      42,    47,    94,    59,    40,    41,    44,    91,    93,   123,
     125,    46
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    52,    53,    54,    54,    55,    55,    55,    55,    55,
      55,    55,    55,    55,    55,    55,    55,    55,    55,    56,
      57,    57,    57,    57,    57,    57,    57,    57,    57,    57,
      57,    58,    58,    58,    59,    60,    60,    61,    62,    62,
      62,    63,    63,    64,    64,    64,    64,    64,    65,    65,
      65,    65,    65,    65,    66,    66,    66,    66,    66,    66,
      66,    67,    68,    69,    69,    69,    69,    69,    69,    69,
      70,    70,    71,    72,    73,    73,    74,    74,    74,    75,
      76,    76,    77,    77,    78,    79,    79,    80,    80,    81,
      81,    82,    82,    83,    83,    84,    85,    85,    86,    87,
      88,    88,    88,    89,    89,    90,    90,    91,    91,    92,
      92,    93,    93,    93,    94,    94,    95,    95,    96,    96
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     2,     1,     2,     2,     3,     3,
       3,     3,     3,     5,     3,     3,     3,     3,     2,     2,
       1,     3,     4,     6,     8,     4,     6,     8,     2,     5,
       5,     1,     1,     1,     6,     1,     3,     3,     1,     3,
       5,     1,     3,     1,     1,     1,     3,     1,     3,     3,
       3,     3,     3,     2,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     1,     2,     2,     2,     5,     9,     3,
       4,     4,     4,     4,     4,     1,     1,     1,     4,     2,
       1,     2,     1,     3,     3,     5,     1,     2,     4,     1,
       1,     1,     1,     2,     3,     0,     2,     1,     1,     1,
       1,     1,     2,     1,     2,     1,     2,     1,     3,     1,
       3,     1,     1,     3,     1,     1,     1,     3,     1,     3
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
      95,     0,   105,     0,     5,    95,     1,     0,     0,     0,
       0,     6,     7,     0,    95,    32,    31,     0,     0,     0,
      33,     0,     4,     0,    18,     0,     3,   106,     8,    95,
     107,    10,    12,    11,    16,    90,     0,    89,   116,     9,
      14,    98,    99,     0,   101,   100,   118,    15,    17,     0,
      76,    77,     0,     0,     0,     0,     0,    95,   103,     0,
      63,     0,     0,     0,     0,    20,    19,     0,    95,    38,
       0,     0,    35,    95,     0,   102,     0,    95,     0,    65,
       0,    66,     0,     0,    95,     0,    95,     0,    95,    64,
      95,    61,   104,     0,    28,    43,     0,     0,     0,    47,
      44,    45,     0,    37,    41,   108,     0,    92,     0,    13,
      91,   117,   119,     0,   112,   111,     0,     0,     0,     0,
      69,     0,    84,    86,    79,    95,    97,    62,    95,     0,
      95,    75,    95,    21,     0,     0,    53,     0,    39,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,    36,
      87,    73,     0,    78,     0,     0,     0,    96,     0,     0,
       0,    71,    80,    70,    22,     0,    25,     0,    46,    49,
      48,    50,    51,    52,    93,     0,     0,    42,     0,     0,
     113,    67,     0,    95,    72,     0,     0,    82,    81,    29,
       0,    30,     0,    94,    40,   110,     0,    88,     0,   114,
     115,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,    23,     0,    26,     0,    34,     0,    85,    58,    59,
      55,    54,    56,    57,    60,    83,     0,     0,     0,    24,
      27,    68
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     3,    24,    66,    25,    72,    73,    30,
      68,   103,   146,    99,   187,    26,    58,    59,    60,    88,
      61,   130,    62,    86,   161,   188,    63,   122,   114,    36,
     109,   145,     4,   127,   100,   101,    46,    64,     5,    31,
     147,   116,   201,    39,    47
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -66
static const yytype_int16 yypact[] =
{
     -66,    51,   -66,   179,   -66,    59,   -66,    28,    30,    30,
      30,   -66,   -66,    42,    37,   -66,   -66,    46,    52,    40,
     -66,    55,   -66,    77,   -66,    60,   -66,   -66,   -66,    61,
     -66,    -6,    -6,    -6,   -66,   -66,    76,   -66,   -66,    78,
     -66,   -66,   -66,    40,   -66,   -66,   -66,    86,   -66,    92,
     -66,   -66,   111,    77,   115,   112,    88,    61,   -66,    96,
     -66,   153,    77,   153,    15,     3,   -66,     4,   114,   -66,
      30,   126,   -66,     6,   124,   -66,    40,   145,   150,   -66,
       4,   -66,   157,    77,   141,   160,   154,   164,   192,   -66,
     192,   -66,   -66,   -22,   -66,   -66,     4,     4,   -24,   -66,
     -66,   -66,     4,   -66,   -66,   -66,   150,   -66,    76,   -66,
     -66,   -66,   -66,   180,   -66,   -66,     0,   168,   101,   169,
     -66,   182,   -66,   -66,   -66,   154,   -66,   -66,   145,   173,
     208,   -66,   208,   -66,     8,    39,    27,   133,   -66,     4,
       4,     4,     4,     4,   170,     4,   148,   -27,   174,   -66,
     184,   -66,   145,   -66,    77,   150,   181,   -66,    63,    30,
       4,   -66,   -66,   -66,    60,   150,    60,   191,   -66,    27,
      27,   185,   185,   -66,   175,    75,     4,   -66,   150,     4,
     -66,   -66,   178,     4,   -66,    81,   203,   217,   219,   -66,
     105,   -66,   107,   -66,   -66,   148,   186,   148,   150,   -66,
     187,   189,   -66,     4,     4,     4,     4,     4,     4,     4,
       4,   -66,   150,   -66,   191,   -66,   190,   -66,   148,   148,
     148,   148,   148,   148,   148,   217,   201,   202,    77,   -66,
     -66,   -66
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -66,   -66,   227,   -66,   -66,   -59,   -66,   128,   -66,   167,
     193,   -66,   -41,   -66,    38,   -66,   188,   -51,   -66,   194,
     -66,   159,   -66,   -66,   119,   -66,   -65,   -66,   102,   -66,
     -66,   -66,    -7,   130,   -18,   -19,   -35,   -66,   -66,    -5,
      70,   131,   -66,   -66,   -66
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -3
static const yytype_int16 yytable[] =
{
      45,    44,    79,    81,    32,    33,    94,    37,    75,    41,
      42,    89,   113,   138,   139,   140,   141,   142,   143,   176,
      49,   177,    69,   133,    45,    44,    98,   144,    50,    51,
      52,    53,   120,    54,    55,    41,    42,    65,    95,   118,
      70,   112,    96,   107,    56,   151,   152,    93,    97,    57,
      69,     6,   108,   164,   165,   136,   137,    45,    44,    -2,
     117,   104,    28,   113,    29,    91,   110,   141,   142,   143,
     115,    41,    42,    35,   135,   134,    34,   123,    43,   126,
      38,   131,    49,   131,   166,   167,    40,   113,   148,    48,
      50,    51,    52,    53,    65,    54,    55,    67,   169,   170,
     171,   172,   173,   181,   175,   189,    56,   191,   184,   152,
      71,    57,   194,   139,   140,   141,   142,   143,   126,   186,
      49,   115,    83,   162,    74,   162,   202,    70,    50,    51,
      52,    53,    76,    54,    55,   195,    77,   182,   197,   139,
     140,   141,   142,   143,    56,    85,   154,   190,   192,    57,
     211,   212,   213,   214,   185,    78,    82,    87,   111,    80,
     196,   102,   218,   219,   220,   221,   222,   223,   224,   186,
     106,   139,   140,   141,   142,   143,   199,   231,   168,    57,
     216,    41,     7,     8,     9,    10,   139,   140,   141,   142,
     143,   119,   121,   124,   226,   227,    11,   125,   129,    12,
      13,    14,    15,    16,    17,    18,    19,    20,   128,    21,
     203,   204,   205,   153,   150,   155,   156,   159,   160,   179,
     178,   174,    22,    42,   198,   183,   193,   143,    23,   209,
     210,   215,    27,   176,   217,   228,   149,   105,   206,   207,
     208,   139,   140,   141,   142,   143,   229,   230,   225,   132,
      84,   163,    92,   200,   180,   157,     0,    90,     0,   158
};

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-66)))

#define yytable_value_is_error(Yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
      19,    19,    53,    54,     9,    10,    65,    14,    43,    31,
      32,    62,    77,    37,    38,    39,    40,    41,    42,    46,
       5,    48,    29,    45,    43,    43,    67,    51,    13,    14,
      15,    16,    83,    18,    19,    31,    32,    34,    34,    80,
      46,    76,    38,    37,    29,    45,    46,    44,    44,    34,
      57,     0,    46,    45,    46,    96,    97,    76,    76,     0,
      78,    68,    34,   128,    34,    50,    73,    40,    41,    42,
      77,    31,    32,    36,    93,    93,    34,    84,    38,    86,
      34,    88,     5,    90,    45,    46,    34,   152,   106,    34,
      13,    14,    15,    16,    34,    18,    19,    36,   139,   140,
     141,   142,   143,   154,   145,   164,    29,   166,    45,    46,
      34,    34,    37,    38,    39,    40,    41,    42,   125,   160,
       5,   128,    34,   130,    46,   132,    45,    46,    13,    14,
      15,    16,    46,    18,    19,   176,    44,   155,   179,    38,
      39,    40,    41,    42,    29,    49,    45,   165,   167,    34,
      45,    46,    45,    46,   159,    44,    44,     4,    34,    44,
     178,    47,   203,   204,   205,   206,   207,   208,   209,   210,
      44,    38,    39,    40,    41,    42,   183,   228,    45,    34,
     198,    31,     3,     4,     5,     6,    38,    39,    40,    41,
      42,    34,    51,    33,   212,   214,    17,    43,     6,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    44,    30,
       7,     8,     9,    45,    34,    46,    34,    44,    10,    35,
      46,    51,    43,    32,    46,    44,    51,    42,    49,    12,
      11,    45,     5,    46,    45,    45,   108,    70,    35,    36,
      37,    38,    39,    40,    41,    42,    45,    45,   210,    90,
      57,   132,    64,   183,   152,   125,    -1,    63,    -1,   128
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    53,    54,    55,    84,    90,     0,     3,     4,     5,
       6,    17,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    30,    43,    49,    56,    58,    67,    54,    34,    34,
      61,    91,    91,    91,    34,    36,    81,    84,    34,    95,
      34,    31,    32,    38,    86,    87,    88,    96,    34,     5,
      13,    14,    15,    16,    18,    19,    29,    34,    68,    69,
      70,    72,    74,    78,    89,    34,    57,    36,    62,    84,
      46,    34,    59,    60,    46,    88,    46,    44,    44,    69,
      44,    69,    44,    34,    62,    49,    75,     4,    71,    69,
      71,    50,    68,    44,    57,    34,    38,    44,    64,    65,
      86,    87,    47,    63,    84,    61,    44,    37,    46,    82,
      84,    34,    88,    78,    80,    84,    93,    86,    64,    34,
      69,    51,    79,    84,    33,    43,    84,    85,    44,     6,
      73,    84,    73,    45,    86,    87,    64,    64,    37,    38,
      39,    40,    41,    42,    51,    83,    64,    92,    86,    59,
      34,    45,    46,    45,    45,    46,    34,    85,    93,    44,
      10,    76,    84,    76,    45,    46,    45,    46,    45,    64,
      64,    64,    64,    64,    51,    64,    46,    48,    46,    35,
      80,    69,    86,    44,    45,    91,    64,    66,    77,    57,
      86,    57,    87,    51,    37,    64,    86,    64,    46,    84,
      92,    94,    45,     7,     8,     9,    35,    36,    37,    12,
      11,    45,    46,    45,    46,    45,    86,    45,    64,    64,
      64,    64,    64,    64,    64,    66,    86,    87,    45,    45,
      45,    69
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
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (ret, YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))

/* Error token number */
#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (YYID (N))                                                     \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (YYID (0))
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

__attribute__((__unused__))
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
#else
static unsigned
yy_location_print_ (yyo, yylocp)
    FILE *yyo;
    YYLTYPE const * const yylocp;
#endif
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += fprintf (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += fprintf (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += fprintf (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += fprintf (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += fprintf (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

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
  FILE *yyo = yyoutput;
  YYUSE (yyo);
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
  YYUSE (yytype);
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

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULL, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULL;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULL, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
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

  YYUSE (yytype);
}




/* The lookahead symbol.  */
int yychar;


#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval YY_INITIAL_VALUE(yyval_default);

/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;


/* Number of syntax errors so far.  */
int yynerrs;


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
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.
       `yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
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
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
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

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

/* User initialization code.  */
/* Line 1570 of yacc.c  */
#line 113 "pbparser.ypp"
{
  theRefPool.clear();
}
/* Line 1570 of yacc.c  */
#line 1655 "libpbcompiler_a-pbparser.cpp"
  yylsp[0] = yylloc;
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
  if (yypact_value_is_default (yyn))
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
      if (yytable_value_is_error (yyn))
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
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
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
/* Line 1787 of yacc.c  */
#line 151 "pbparser.ypp"
    {
  ret = (yyvsp[(1) - (1)].transforms);
  clearParserCaches();
}
    break;

  case 3:
/* Line 1787 of yacc.c  */
#line 156 "pbparser.ypp"
    {
  ((yyval.transform)=(yyvsp[(1) - (2)].transform))->setRules(*(yyvsp[(2) - (2)].rules)); 
}
    break;

  case 4:
/* Line 1787 of yacc.c  */
#line 160 "pbparser.ypp"
    {
  //forward decl syntax 
}
    break;

  case 5:
/* Line 1787 of yacc.c  */
#line 164 "pbparser.ypp"
    {(yyval.transform)=REFALLOC(Transform());}
    break;

  case 6:
/* Line 1787 of yacc.c  */
#line 166 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (2)].transform))->markMain(); }
    break;

  case 7:
/* Line 1787 of yacc.c  */
#line 167 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (2)].transform))->markMemoized();  }
    break;

  case 8:
/* Line 1787 of yacc.c  */
#line 168 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (3)].transform))->setName((yyvsp[(3) - (3)].str));     }
    break;

  case 9:
/* Line 1787 of yacc.c  */
#line 169 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (3)].transform))->addParams(*(yyvsp[(3) - (3)].freevars));  }
    break;

  case 10:
/* Line 1787 of yacc.c  */
#line 170 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (3)].transform))->addFrom(*(yyvsp[(3) - (3)].matrixdefs));    }
    break;

  case 11:
/* Line 1787 of yacc.c  */
#line 171 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (3)].transform))->addThrough(*(yyvsp[(3) - (3)].matrixdefs)); }
    break;

  case 12:
/* Line 1787 of yacc.c  */
#line 172 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (3)].transform))->addTo(*(yyvsp[(3) - (3)].matrixdefs));      }
    break;

  case 13:
/* Line 1787 of yacc.c  */
#line 173 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (5)].transform))->addTemplateArg(*(yyvsp[(4) - (5)].templateargs)); }
    break;

  case 14:
/* Line 1787 of yacc.c  */
#line 175 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (3)].transform))->setAccuracyMetric((yyvsp[(3) - (3)].str));}
    break;

  case 15:
/* Line 1787 of yacc.c  */
#line 176 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (3)].transform))->setAccuracyBins(*(yyvsp[(3) - (3)].doublelist));}
    break;

  case 16:
/* Line 1787 of yacc.c  */
#line 177 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (3)].transform))->setGenerator((yyvsp[(3) - (3)].str));}
    break;

  case 17:
/* Line 1787 of yacc.c  */
#line 178 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (3)].transform))->addInputFeature((yyvsp[(3) - (3)].str));}
    break;

  case 18:
/* Line 1787 of yacc.c  */
#line 179 "pbparser.ypp"
    { P((yyval.transform)=(yyvsp[(1) - (2)].transform))->addConfigItem(*(yyvsp[(2) - (2)].configitem)); }
    break;

  case 19:
/* Line 1787 of yacc.c  */
#line 181 "pbparser.ypp"
    {
  P((yyval.configitem)=(yyvsp[(2) - (2)].configitem));
  (yyval.configitem)->addFlag((yyvsp[(1) - (2)].i));
  (yyval.configitem)->initDefaults();
}
    break;

  case 20:
/* Line 1787 of yacc.c  */
#line 188 "pbparser.ypp"
    { (yyval.configitem)=REFALLOC(ConfigItem(0, (yyvsp[(1) - (1)].str), jalib::TunableValue(), jalib::TunableValue(), jalib::TunableValue())); }
    break;

  case 21:
/* Line 1787 of yacc.c  */
#line 190 "pbparser.ypp"
    { (yyval.configitem)=REFALLOC(ConfigItem(0, (yyvsp[(1) - (3)].str), jalib::TunableValue(), jalib::TunableValue(), jalib::TunableValue())); }
    break;

  case 22:
/* Line 1787 of yacc.c  */
#line 192 "pbparser.ypp"
    { (yyval.configitem)=REFALLOC(ConfigItem(0, (yyvsp[(1) - (4)].str), (yyvsp[(3) - (4)].i), jalib::TunableValue(), jalib::TunableValue())); }
    break;

  case 23:
/* Line 1787 of yacc.c  */
#line 194 "pbparser.ypp"
    { (yyval.configitem)=REFALLOC(ConfigItem(0, (yyvsp[(1) - (6)].str), (yyvsp[(3) - (6)].i), (yyvsp[(5) - (6)].i), jalib::TunableValue())); }
    break;

  case 24:
/* Line 1787 of yacc.c  */
#line 196 "pbparser.ypp"
    { (yyval.configitem)=REFALLOC(ConfigItem(0, (yyvsp[(1) - (8)].str), (yyvsp[(3) - (8)].i), (yyvsp[(5) - (8)].i), (yyvsp[(7) - (8)].i))); }
    break;

  case 25:
/* Line 1787 of yacc.c  */
#line 198 "pbparser.ypp"
    { (yyval.configitem)=REFALLOC(ConfigItem(0, (yyvsp[(1) - (4)].str), (yyvsp[(3) - (4)].d), jalib::TunableValue(), jalib::TunableValue())); }
    break;

  case 26:
/* Line 1787 of yacc.c  */
#line 200 "pbparser.ypp"
    { (yyval.configitem)=REFALLOC(ConfigItem(0, (yyvsp[(1) - (6)].str), (yyvsp[(3) - (6)].d), (yyvsp[(5) - (6)].d), jalib::TunableValue())); }
    break;

  case 27:
/* Line 1787 of yacc.c  */
#line 202 "pbparser.ypp"
    { (yyval.configitem)=REFALLOC(ConfigItem(0, (yyvsp[(1) - (8)].str), (yyvsp[(3) - (8)].d), (yyvsp[(5) - (8)].d), (yyvsp[(7) - (8)].d))); }
    break;

  case 28:
/* Line 1787 of yacc.c  */
#line 205 "pbparser.ypp"
    {
  P((yyval.configitem)=(yyvsp[(2) - (2)].configitem));
  if(strcmp((yyvsp[(1) - (2)].str), "double")==0)
    (yyval.configitem)->addFlag(ConfigItem::FLAG_DOUBLE);
  else if(strcmp((yyvsp[(1) - (2)].str), "float")==0)
    (yyval.configitem)->addFlag(ConfigItem::FLAG_DOUBLE);
  else if(strcmp((yyvsp[(1) - (2)].str), "sizespecific")==0)
    (yyval.configitem)->addFlag(ConfigItem::FLAG_SIZESPECIFIC);
  else if(strcmp((yyvsp[(1) - (2)].str), "accuracyhint")==0)
    (yyval.configitem)->addFlag(ConfigItem::FLAG_ACCURACY);
  else {
    JASSERT(false)((yyvsp[(1) - (2)].str)).Text("unknown keyword");
  }
}
    break;

  case 29:
/* Line 1787 of yacc.c  */
#line 220 "pbparser.ypp"
    {
  P((yyval.configitem)=(yyvsp[(5) - (5)].configitem));
  if(strcmp((yyvsp[(1) - (5)].str), "array")==0){
    (yyval.configitem)->addFlag(ConfigItem::FLAG_ARRAY);
    (yyval.configitem)->setArraySize((yyvsp[(3) - (5)].i));
  }else if(strcmp((yyvsp[(1) - (5)].str), "initial")==0)
    (yyval.configitem)->setInitial((yyvsp[(3) - (5)].i));
  else if(strcmp((yyvsp[(1) - (5)].str), "min")==0)
    (yyval.configitem)->setMin((yyvsp[(3) - (5)].i));
  else if(strcmp((yyvsp[(1) - (5)].str), "max")==0)
    (yyval.configitem)->setMax((yyvsp[(3) - (5)].i));
  else {
    JASSERT(false)((yyvsp[(1) - (5)].str)).Text("unknown keyword");
  }
}
    break;

  case 30:
/* Line 1787 of yacc.c  */
#line 236 "pbparser.ypp"
    {
  P((yyval.configitem)=(yyvsp[(5) - (5)].configitem));
  if(strcmp((yyvsp[(1) - (5)].str), "initial")==0)
    (yyval.configitem)->setInitial((yyvsp[(3) - (5)].d));
  else if(strcmp((yyvsp[(1) - (5)].str), "min")==0)
    (yyval.configitem)->setMin((yyvsp[(3) - (5)].d));
  else if(strcmp((yyvsp[(1) - (5)].str), "max")==0)
    (yyval.configitem)->setMax((yyvsp[(3) - (5)].d));
  else {
    JASSERT(false)((yyvsp[(1) - (5)].str)).Text("unknown keyword");
  }
}
    break;

  case 31:
/* Line 1787 of yacc.c  */
#line 250 "pbparser.ypp"
    { (yyval.i)=ConfigItem::FLAG_FROMCFG|ConfigItem::FLAG_USER; }
    break;

  case 32:
/* Line 1787 of yacc.c  */
#line 251 "pbparser.ypp"
    { (yyval.i)=ConfigItem::FLAG_FROMCFG|ConfigItem::FLAG_USER|ConfigItem::FLAG_TUNABLE; }
    break;

  case 33:
/* Line 1787 of yacc.c  */
#line 252 "pbparser.ypp"
    { (yyval.i)=ConfigItem::FLAG_FROMCFG|ConfigItem::FLAG_USER
                                        |ConfigItem::FLAG_ACCURACY|ConfigItem::FLAG_SIZESPECIFIC
                                        |ConfigItem::FLAG_TUNABLE; }
    break;

  case 34:
/* Line 1787 of yacc.c  */
#line 256 "pbparser.ypp"
    {
  (yyval.templatearg)=REFALLOC(TemplateArg((yyvsp[(1) - (6)].str), (yyvsp[(3) - (6)].i), (yyvsp[(5) - (6)].i)));
}
    break;

  case 35:
/* Line 1787 of yacc.c  */
#line 259 "pbparser.ypp"
    { ((yyval.templateargs)=REFALLOC(TemplateArgList()))->push_back((yyvsp[(1) - (1)].templatearg)); }
    break;

  case 36:
/* Line 1787 of yacc.c  */
#line 260 "pbparser.ypp"
    { P((yyval.templateargs)=(yyvsp[(1) - (3)].templateargs))->push_back((yyvsp[(3) - (3)].templatearg)); }
    break;

  case 37:
/* Line 1787 of yacc.c  */
#line 265 "pbparser.ypp"
    {          
  (yyval.matrixdef)=REFALLOC(MatrixDef((yyvsp[(1) - (3)].str),*(yyvsp[(2) - (3)].formulas),*(yyvsp[(3) - (3)].formulas)));          
}
    break;

  case 38:
/* Line 1787 of yacc.c  */
#line 269 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList())); }
    break;

  case 39:
/* Line 1787 of yacc.c  */
#line 270 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(2) - (3)].formula)); }
    break;

  case 40:
/* Line 1787 of yacc.c  */
#line 272 "pbparser.ypp"
    {                                              
  (yyval.formulas)=REFALLOC(FormulaList()); 
  (yyval.formulas)->push_back((yyvsp[(2) - (5)].formula));
  (yyval.formulas)->push_back((yyvsp[(4) - (5)].formula));
}
    break;

  case 41:
/* Line 1787 of yacc.c  */
#line 278 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 42:
/* Line 1787 of yacc.c  */
#line 279 "pbparser.ypp"
    { P((yyval.formulas)=(yyvsp[(2) - (3)].formulas)); }
    break;

  case 43:
/* Line 1787 of yacc.c  */
#line 281 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaVariable((yyvsp[(1) - (1)].str)) ); }
    break;

  case 44:
/* Line 1787 of yacc.c  */
#line 282 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaInteger( (yyvsp[(1) - (1)].i)) ); }
    break;

  case 45:
/* Line 1787 of yacc.c  */
#line 283 "pbparser.ypp"
    {(yyval.formula)=REFALLOC( FormulaFloat(  (yyvsp[(1) - (1)].d)) );  }
    break;

  case 46:
/* Line 1787 of yacc.c  */
#line 284 "pbparser.ypp"
    { (yyval.formula)=(yyvsp[(2) - (3)].formula); }
    break;

  case 47:
/* Line 1787 of yacc.c  */
#line 285 "pbparser.ypp"
    { (yyval.formula)=(yyvsp[(1) - (1)].formula); }
    break;

  case 48:
/* Line 1787 of yacc.c  */
#line 287 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'+'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 49:
/* Line 1787 of yacc.c  */
#line 288 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'-'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 50:
/* Line 1787 of yacc.c  */
#line 289 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'*'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 51:
/* Line 1787 of yacc.c  */
#line 290 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'/'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 52:
/* Line 1787 of yacc.c  */
#line 291 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'^'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 53:
/* Line 1787 of yacc.c  */
#line 292 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'-'>(FormulaInteger::zero(),(yyvsp[(2) - (2)].formula))); }
    break;

  case 54:
/* Line 1787 of yacc.c  */
#line 294 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'='>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 55:
/* Line 1787 of yacc.c  */
#line 295 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'='>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 56:
/* Line 1787 of yacc.c  */
#line 296 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'<'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 57:
/* Line 1787 of yacc.c  */
#line 297 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaBinop<'>'>((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 58:
/* Line 1787 of yacc.c  */
#line 298 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaLE((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 59:
/* Line 1787 of yacc.c  */
#line 299 "pbparser.ypp"
    { (yyval.formula)=REFALLOC(FormulaGE((yyvsp[(1) - (3)].formula),(yyvsp[(3) - (3)].formula))); }
    break;

  case 60:
/* Line 1787 of yacc.c  */
#line 300 "pbparser.ypp"
    { JASSERT(false).Text("|| in where clauses not yet supported"); }
    break;

  case 61:
/* Line 1787 of yacc.c  */
#line 302 "pbparser.ypp"
    { P((yyval.rules)=(yyvsp[(2) - (3)].rules)); }
    break;

  case 62:
/* Line 1787 of yacc.c  */
#line 304 "pbparser.ypp"
    { P((yyval.rule)=(yyvsp[(1) - (3)].rule))->setBody((yyvsp[(2) - (3)].str), yyloc); }
    break;

  case 63:
/* Line 1787 of yacc.c  */
#line 306 "pbparser.ypp"
    { P((yyval.rule)=(yyvsp[(1) - (1)].rule)); }
    break;

  case 64:
/* Line 1787 of yacc.c  */
#line 307 "pbparser.ypp"
    { P((yyval.rule)=(yyvsp[(2) - (2)].rule))->setPriority((yyvsp[(1) - (2)].i)); }
    break;

  case 65:
/* Line 1787 of yacc.c  */
#line 308 "pbparser.ypp"
    { P((yyval.rule)=(yyvsp[(2) - (2)].rule))->addRotations(RuleFlags::ROTATE); }
    break;

  case 66:
/* Line 1787 of yacc.c  */
#line 309 "pbparser.ypp"
    { P((yyval.rule)=(yyvsp[(2) - (2)].rule))->markRecursive(); }
    break;

  case 67:
/* Line 1787 of yacc.c  */
#line 310 "pbparser.ypp"
    { P((yyval.rule)=(yyvsp[(5) - (5)].rule))->markRecursive((yyvsp[(3) - (5)].formula)); }
    break;

  case 68:
/* Line 1787 of yacc.c  */
#line 311 "pbparser.ypp"
    { P((yyval.rule)=(yyvsp[(9) - (9)].rule))->addDuplicateVar((yyvsp[(3) - (9)].str), (yyvsp[(5) - (9)].i), (yyvsp[(7) - (9)].i)); }
    break;

  case 69:
/* Line 1787 of yacc.c  */
#line 312 "pbparser.ypp"
    { P((yyval.rule)=(yyvsp[(3) - (3)].rule))->setLabel((yyvsp[(2) - (3)].str)); }
    break;

  case 70:
/* Line 1787 of yacc.c  */
#line 314 "pbparser.ypp"
    { (yyval.rule)=REFALLOC(UserRule((yyvsp[(1) - (4)].region),  *(yyvsp[(2) - (4)].regions), *(yyvsp[(3) - (4)].matrixdefs), *(yyvsp[(4) - (4)].formulas))); }
    break;

  case 71:
/* Line 1787 of yacc.c  */
#line 315 "pbparser.ypp"
    { (yyval.rule)=REFALLOC(UserRule(*(yyvsp[(1) - (4)].regions), *(yyvsp[(2) - (4)].regions), *(yyvsp[(3) - (4)].matrixdefs), *(yyvsp[(4) - (4)].formulas))); }
    break;

  case 72:
/* Line 1787 of yacc.c  */
#line 317 "pbparser.ypp"
    { P((yyval.regions)=(yyvsp[(3) - (4)].regions)); }
    break;

  case 73:
/* Line 1787 of yacc.c  */
#line 318 "pbparser.ypp"
    { P((yyval.regions)=(yyvsp[(3) - (4)].regions)); }
    break;

  case 74:
/* Line 1787 of yacc.c  */
#line 319 "pbparser.ypp"
    { P((yyval.matrixdefs)=(yyvsp[(3) - (4)].matrixdefs)); }
    break;

  case 75:
/* Line 1787 of yacc.c  */
#line 320 "pbparser.ypp"
    { (yyval.matrixdefs)=REFALLOC(MatrixDefList()); }
    break;

  case 76:
/* Line 1787 of yacc.c  */
#line 322 "pbparser.ypp"
    {(yyval.i)=RuleFlags::PRIORITY_PRIMARY;}
    break;

  case 77:
/* Line 1787 of yacc.c  */
#line 323 "pbparser.ypp"
    {(yyval.i)=RuleFlags::PRIORITY_SECONDARY;}
    break;

  case 78:
/* Line 1787 of yacc.c  */
#line 324 "pbparser.ypp"
    {(yyval.i)=(yyvsp[(3) - (4)].i);}
    break;

  case 79:
/* Line 1787 of yacc.c  */
#line 326 "pbparser.ypp"
    { ((yyval.str)=(yyvsp[(2) - (2)].str)); }
    break;

  case 80:
/* Line 1787 of yacc.c  */
#line 328 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList())); }
    break;

  case 81:
/* Line 1787 of yacc.c  */
#line 329 "pbparser.ypp"
    { ((yyval.formulas)=(yyvsp[(2) - (2)].formulas)); }
    break;

  case 82:
/* Line 1787 of yacc.c  */
#line 332 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(1) - (1)].formula)); }
    break;

  case 83:
/* Line 1787 of yacc.c  */
#line 333 "pbparser.ypp"
    { ((yyval.formulas)=(yyvsp[(1) - (3)].formulas))->push_back((yyvsp[(3) - (3)].formula)); }
    break;

  case 84:
/* Line 1787 of yacc.c  */
#line 335 "pbparser.ypp"
    { (yyval.region)=REFALLOC(Region((yyvsp[(1) - (3)].str),*(yyvsp[(2) - (3)].formulas), (yyvsp[(3) - (3)].str_formulas).str,*(yyvsp[(3) - (3)].str_formulas).formulas)); }
    break;

  case 85:
/* Line 1787 of yacc.c  */
#line 337 "pbparser.ypp"
    { (yyval.str_formulas).str=(yyvsp[(2) - (5)].str); P((yyval.str_formulas).formulas=(yyvsp[(4) - (5)].formulas));}
    break;

  case 86:
/* Line 1787 of yacc.c  */
#line 338 "pbparser.ypp"
    { (yyval.str_formulas).str="all"; (yyval.str_formulas).formulas=REFALLOC(FormulaList());}
    break;

  case 87:
/* Line 1787 of yacc.c  */
#line 340 "pbparser.ypp"
    { P((yyval.region)=(yyvsp[(1) - (2)].region))->setName((yyvsp[(2) - (2)].str)); }
    break;

  case 88:
/* Line 1787 of yacc.c  */
#line 341 "pbparser.ypp"
    { P((yyval.region)=(yyvsp[(1) - (4)].region))->setName((yyvsp[(2) - (4)].str)); (yyval.region)->setOptionalDefault((yyvsp[(4) - (4)].formula)); }
    break;

  case 98:
/* Line 1787 of yacc.c  */
#line 349 "pbparser.ypp"
    { (yyval.i)=jalib::StringToX<int>((yyvsp[(1) - (1)].str)); }
    break;

  case 99:
/* Line 1787 of yacc.c  */
#line 350 "pbparser.ypp"
    { (yyval.d)=jalib::StringToX<double>((yyvsp[(1) - (1)].str)); }
    break;

  case 100:
/* Line 1787 of yacc.c  */
#line 351 "pbparser.ypp"
    { (yyval.d)=(yyvsp[(1) - (1)].d); }
    break;

  case 101:
/* Line 1787 of yacc.c  */
#line 351 "pbparser.ypp"
    { (yyval.d)=(yyvsp[(1) - (1)].i); }
    break;

  case 102:
/* Line 1787 of yacc.c  */
#line 351 "pbparser.ypp"
    { (yyval.d)=-1*(yyvsp[(2) - (2)].d); }
    break;

  case 103:
/* Line 1787 of yacc.c  */
#line 353 "pbparser.ypp"
    { ((yyval.rules)=REFALLOC(RuleList()))->push_back((yyvsp[(1) - (1)].rule)); }
    break;

  case 104:
/* Line 1787 of yacc.c  */
#line 354 "pbparser.ypp"
    { P((yyval.rules)=(yyvsp[(1) - (2)].rules))->push_back((yyvsp[(2) - (2)].rule)); }
    break;

  case 105:
/* Line 1787 of yacc.c  */
#line 356 "pbparser.ypp"
    { ((yyval.transforms)=REFALLOC(TransformList()))->push_back((yyvsp[(1) - (1)].transform)); }
    break;

  case 106:
/* Line 1787 of yacc.c  */
#line 357 "pbparser.ypp"
    { P((yyval.transforms)=(yyvsp[(1) - (2)].transforms))->push_back((yyvsp[(2) - (2)].transform)); }
    break;

  case 107:
/* Line 1787 of yacc.c  */
#line 359 "pbparser.ypp"
    { ((yyval.matrixdefs)=REFALLOC(MatrixDefList()))->push_back((yyvsp[(1) - (1)].matrixdef)); }
    break;

  case 108:
/* Line 1787 of yacc.c  */
#line 360 "pbparser.ypp"
    { P((yyval.matrixdefs)=(yyvsp[(1) - (3)].matrixdefs))->push_back((yyvsp[(3) - (3)].matrixdef));                      }
    break;

  case 109:
/* Line 1787 of yacc.c  */
#line 362 "pbparser.ypp"
    { ((yyval.formulas)=REFALLOC(FormulaList()))->push_back((yyvsp[(1) - (1)].formula)); }
    break;

  case 110:
/* Line 1787 of yacc.c  */
#line 363 "pbparser.ypp"
    { P((yyval.formulas)=(yyvsp[(1) - (3)].formulas))->push_back((yyvsp[(3) - (3)].formula)); }
    break;

  case 111:
/* Line 1787 of yacc.c  */
#line 365 "pbparser.ypp"
    { ((yyval.regions)=REFALLOC(RegionList())); }
    break;

  case 112:
/* Line 1787 of yacc.c  */
#line 366 "pbparser.ypp"
    { ((yyval.regions)=REFALLOC(RegionList()))->push_back((yyvsp[(1) - (1)].region)); }
    break;

  case 113:
/* Line 1787 of yacc.c  */
#line 367 "pbparser.ypp"
    { P((yyval.regions)=(yyvsp[(1) - (3)].regions))->push_back((yyvsp[(3) - (3)].region)); }
    break;

  case 114:
/* Line 1787 of yacc.c  */
#line 370 "pbparser.ypp"
    { (yyval.formulas)=REFALLOC(FormulaList()); }
    break;

  case 115:
/* Line 1787 of yacc.c  */
#line 371 "pbparser.ypp"
    { P((yyval.formulas)=(yyvsp[(1) - (1)].formulas)); }
    break;

  case 116:
/* Line 1787 of yacc.c  */
#line 374 "pbparser.ypp"
    { ((yyval.freevars)=REFALLOC(OrderedFreeVars()))->push_back((yyvsp[(1) - (1)].str)); }
    break;

  case 117:
/* Line 1787 of yacc.c  */
#line 375 "pbparser.ypp"
    { ((yyval.freevars)=(yyvsp[(1) - (3)].freevars))->push_back((yyvsp[(3) - (3)].str)); }
    break;

  case 118:
/* Line 1787 of yacc.c  */
#line 377 "pbparser.ypp"
    { ((yyval.doublelist)=REFALLOC(DoubleList()))->push_back((yyvsp[(1) - (1)].d)); }
    break;

  case 119:
/* Line 1787 of yacc.c  */
#line 378 "pbparser.ypp"
    { ((yyval.doublelist)=(yyvsp[(1) - (3)].doublelist))->push_back((yyvsp[(3) - (3)].d)); }
    break;


/* Line 1787 of yacc.c  */
#line 2556 "libpbcompiler_a-pbparser.cpp"
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
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
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (ret, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (ret, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

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

  yyerror_range[1] = yylsp[1-yylen];
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
      if (!yypact_value_is_default (yyn))
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

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp, ret);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
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

#if !defined yyoverflow || YYERROR_VERBOSE
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
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, ret);
    }
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


/* Line 2050 of yacc.c  */
#line 380 "pbparser.ypp"


extern FILE* pbin;

TransformListPtr parsePbFile(const char* filename){
  TransformListPtr ret;
  if(pbcConfig::thePbPreprocessor != "") {
    pbin = popen((pbcConfig::thePbPreprocessor+" \""+filename+"\"").c_str(), "r");
  } else {
    pbin = fopen(filename,"r");
  }
  JASSERT(pbin!=NULL)(filename)(JASSERT_ERRNO).Text("failed to open file");
  pbparse(ret);
  return ret;
}


