/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"

#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "symbol.h" 
#include "errormsg.h"
#include "absyn.h"

int yylex(void); /* function prototype */

A_exp absyn_root;

void yyerror(char *s)
{
 EM_error(EM_tokPos, "%s", s);
 exit(1);
}

#line 89 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_HOME_GLASSER_TOYS_TIGER_COMPILER_SRC_COMPILER_REWRITE_TIGER_PARSER_H_INCLUDED
# define YY_YY_HOME_GLASSER_TOYS_TIGER_COMPILER_SRC_COMPILER_REWRITE_TIGER_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    ID = 258,
    STRING = 259,
    INT = 260,
    COMMA = 261,
    COLON = 262,
    SEMICOLON = 263,
    LPAREN = 264,
    RPAREN = 265,
    LBRACK = 266,
    RBRACK = 267,
    LBRACE = 268,
    RBRACE = 269,
    DOT = 270,
    PLUS = 271,
    MINUS = 272,
    TIMES = 273,
    DIVIDE = 274,
    EQ = 275,
    NEQ = 276,
    LT = 277,
    LE = 278,
    GT = 279,
    GE = 280,
    AND = 281,
    OR = 282,
    ASSIGN = 283,
    ARRAY = 284,
    IF = 285,
    THEN = 286,
    ELSE = 287,
    WHILE = 288,
    FOR = 289,
    TO = 290,
    DO = 291,
    LET = 292,
    IN = 293,
    END = 294,
    OF = 295,
    BREAK = 296,
    NIL = 297,
    FUNCTION = 298,
    VAR = 299,
    TYPE = 300,
    EXP = 301,
    FUNDEC = 302,
    UMINUS = 303,
    IFX = 304
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 21 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"

	int pos;
	int ival;
	string sval;
	A_var var;
	A_exp exp;
	/* et cetera */
    A_dec dec;
    A_ty ty;
    A_field field;
    A_fieldList fieldList;
    A_expList expList;
    A_fundec fundec;
    A_fundecList fundecList;
    A_decList decList;
    A_namety namety;
    A_nametyList nametyList;
    A_efield efield;
    A_efieldList efieldList;
	

#line 213 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_HOME_GLASSER_TOYS_TIGER_COMPILER_SRC_COMPILER_REWRITE_TIGER_PARSER_H_INCLUDED  */



#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

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

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
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
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  48
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   235

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  50
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  30
/* YYNRULES -- Number of rules.  */
#define YYNRULES  77
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  153

#define YYUNDEFTOK  2
#define YYMAXUTOK   304


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    86,    86,    94,    95,    96,    97,    98,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   112,   113,   115,
     116,   117,   119,   120,   122,   124,   125,   126,   128,   129,
     130,   132,   134,   135,   137,   138,   140,   141,   145,   146,
     147,   149,   151,   152,   153,   155,   157,   158,   160,   161,
     163,   164,   166,   167,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   186,   193,   194,   196,
     197,   199,   203,   205,   206,   208,   210,   212
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ID", "STRING", "INT", "COMMA", "COLON",
  "SEMICOLON", "LPAREN", "RPAREN", "LBRACK", "RBRACK", "LBRACE", "RBRACE",
  "DOT", "PLUS", "MINUS", "TIMES", "DIVIDE", "EQ", "NEQ", "LT", "LE", "GT",
  "GE", "AND", "OR", "ASSIGN", "ARRAY", "IF", "THEN", "ELSE", "WHILE",
  "FOR", "TO", "DO", "LET", "IN", "END", "OF", "BREAK", "NIL", "FUNCTION",
  "VAR", "TYPE", "EXP", "FUNDEC", "UMINUS", "IFX", "$accept", "program",
  "exp", "valexp", "decs", "dec", "tydecs", "tydec", "ty", "tyfields",
  "tyfield", "vardec", "fundecs", "fundec", "constexp", "lvalue",
  "arrayexp", "seqlist", "exps", "callexp", "funcargs", "opexp", "recexp",
  "recfields", "recfield", "assignexp", "ifexp", "whileexp", "forexp",
  "letexp", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304
};
# endif

#define YYPACT_NINF (-114)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     144,    31,  -114,  -114,    88,     1,     1,   144,    12,    14,
    -114,  -114,     3,  -114,   187,  -114,    -3,  -114,  -114,  -114,
    -114,  -114,  -114,  -114,  -114,  -114,  -114,   129,   144,    10,
    -114,    18,     7,  -114,    26,   171,    24,     8,    36,    59,
      86,    52,    14,  -114,    49,  -114,  -114,    53,  -114,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,   144,    92,   144,  -114,    94,    91,    95,    83,  -114,
      90,   100,   144,  -114,   144,   144,   144,    99,     4,    89,
     144,  -114,  -114,  -114,     2,     2,  -114,  -114,    48,    48,
      48,    48,    48,    48,   210,   199,    98,  -114,  -114,   144,
    -114,    72,   144,  -114,   110,  -114,    82,  -114,    80,   114,
     120,   144,     6,    85,  -114,  -114,   144,  -114,  -114,   144,
     144,   119,   117,   122,   103,  -114,  -114,   114,    96,  -114,
    -114,  -114,  -114,   105,   132,     9,   114,   144,   128,   141,
     144,  -114,   142,   144,  -114,  -114,  -114,  -114,  -114,   130,
    -114,   144,  -114
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    41,    39,    38,     0,     0,     0,     0,     0,     0,
       7,    40,     0,     2,     3,     9,     8,    14,    10,    11,
      12,    13,     4,    16,     5,     6,    15,     0,     0,     0,
      47,    48,     0,    58,     8,     0,     0,     0,     0,     0,
       0,     0,    17,    19,    22,    20,    21,    34,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,    52,     0,     0,     0,    67,
       0,    69,     0,    46,     0,     0,     0,     0,     0,     0,
       0,    18,    23,    35,    54,    55,    56,    57,    59,    60,
      62,    64,    61,    63,    65,    66,     0,    43,    72,     0,
      51,    42,     0,    68,     0,    49,    73,    75,     0,    28,
       0,     0,     0,     0,    44,    53,     0,    71,    70,     0,
       0,     0,     0,    29,     0,    32,    25,    28,     0,    24,
      77,    45,    74,     0,     0,     0,    28,     0,     0,     0,
       0,    31,     0,     0,    30,    33,    26,    27,    76,     0,
      36,     0,    37
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -114,  -114,     0,    -4,   112,  -114,   108,  -114,  -114,  -113,
    -114,  -114,   109,  -114,  -114,    28,  -114,  -114,   -50,  -114,
      56,  -114,  -114,    54,  -114,  -114,  -114,  -114,  -114,  -114
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    12,    31,    14,    41,    42,    43,    44,   129,   122,
     123,    45,    46,    47,    15,    16,    17,    18,    32,    19,
      66,    20,    21,    70,    71,    22,    23,    24,    25,    26
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      13,    33,    35,    48,     1,     2,     3,    36,    61,   126,
       4,   110,    62,    68,   138,    37,   142,    73,     5,   127,
      51,    52,   105,   144,    69,    63,    72,    65,    67,   143,
     113,     6,   111,    34,    34,   128,    76,    61,     9,    77,
      27,    62,    28,    11,    29,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    38,    39,    40,
      75,    96,    78,    98,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,   108,    34,    34,    34,
      34,    34,    34,    34,    34,    34,    34,    34,    34,    79,
      80,     1,     2,     3,    40,    97,    38,     4,    30,    65,
      99,   100,   117,   102,   103,     5,   104,   101,   109,   112,
     114,   125,   116,    68,   119,   120,   131,   121,     6,   132,
     133,     7,     8,   124,   130,     9,   134,   135,   136,    10,
      11,   137,     1,     2,     3,   141,   139,   145,     4,    64,
     148,   140,   146,   150,   147,   149,     5,     1,     2,     3,
     151,   152,    82,     4,    81,   115,    83,     0,   118,     6,
       0,     5,     7,     8,     0,     0,     9,     0,     0,     0,
      10,    11,     0,     0,     6,     0,     0,     7,     8,     0,
       0,     9,     0,     0,     0,    10,    11,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,     0,
       0,     0,    74,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58
};

static const yytype_int16 yycheck[] =
{
       0,     5,     6,     0,     3,     4,     5,     7,    11,     3,
       9,     7,    15,     3,   127,     3,     7,    10,    17,    13,
      18,    19,    72,   136,    14,    28,     8,    27,    28,    20,
      80,    30,    28,     5,     6,    29,    28,    11,    37,     3,
       9,    15,    11,    42,    13,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    43,    44,    45,
      36,    61,     3,    63,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    74,    75,    76,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,     3,
      38,     3,     4,     5,    45,     3,    43,     9,    10,    99,
       6,    10,   102,    20,    14,    17,     6,    12,     9,    20,
      12,   111,    40,     3,    32,    35,   116,     3,    30,   119,
     120,    33,    34,     3,    39,    37,     7,    10,     6,    41,
      42,    28,     3,     4,     5,     3,    40,   137,     9,    10,
     140,    36,    14,   143,     3,     3,    17,     3,     4,     5,
      20,   151,    44,     9,    42,    99,    47,    -1,   104,    30,
      -1,    17,    33,    34,    -1,    -1,    37,    -1,    -1,    -1,
      41,    42,    -1,    -1,    30,    -1,    -1,    33,    34,    -1,
      -1,    37,    -1,    -1,    -1,    41,    42,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    -1,
      -1,    -1,    31,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,     4,     5,     9,    17,    30,    33,    34,    37,
      41,    42,    51,    52,    53,    64,    65,    66,    67,    69,
      71,    72,    75,    76,    77,    78,    79,     9,    11,    13,
      10,    52,    68,    53,    65,    53,    52,     3,    43,    44,
      45,    54,    55,    56,    57,    61,    62,    63,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    11,    15,    28,    10,    52,    70,    52,     3,    14,
      73,    74,     8,    10,    31,    36,    28,     3,     3,     3,
      38,    54,    56,    62,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    53,    53,    53,    52,     3,    52,     6,
      10,    12,    20,    14,     6,    68,    52,    52,    52,     9,
       7,    28,    20,    68,    12,    70,    40,    52,    73,    32,
      35,     3,    59,    60,     3,    52,     3,    13,    29,    58,
      39,    52,    52,    52,     7,    10,     6,    28,    59,    40,
      36,     3,     7,    20,    59,    52,    14,     3,    52,     3,
      52,    20,    52
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    50,    51,    52,    52,    52,    52,    52,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    54,    54,    55,
      55,    55,    56,    56,    57,    58,    58,    58,    59,    59,
      59,    60,    61,    61,    62,    62,    63,    63,    64,    64,
      64,    65,    65,    65,    65,    66,    67,    67,    68,    68,
      69,    69,    70,    70,    71,    71,    71,    71,    71,    71,
      71,    71,    71,    71,    71,    71,    71,    72,    72,    73,
      73,    74,    75,    76,    76,    77,    78,    79
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     2,     4,     1,     3,     3,     0,     1,
       3,     3,     4,     6,     1,     2,     7,     9,     1,     1,
       1,     1,     4,     3,     4,     6,     3,     2,     1,     3,
       3,     4,     1,     3,     3,     3,     3,     3,     2,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     4,     1,
       3,     3,     3,     4,     6,     4,     8,     5
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
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
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

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
#ifndef YYINITDEPTH
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
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
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
static char *
yystpcpy (char *yydest, const char *yysrc)
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
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
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
            else
              goto append;

          append:
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

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
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
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
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
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
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
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
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
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
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
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

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
      yychar = yylex ();
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
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2:
#line 86 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                  {absyn_root=(yyvsp[0].exp);}
#line 1509 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 3:
#line 94 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1515 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 4:
#line 95 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1521 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 5:
#line 96 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1527 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 6:
#line 97 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1533 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 7:
#line 98 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = A_BreakExp(EM_tokPos);}
#line 1539 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 8:
#line 100 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = A_VarExp(EM_tokPos, (yyvsp[0].var));}
#line 1545 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 9:
#line 101 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1551 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 10:
#line 102 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1557 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 11:
#line 103 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1563 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 12:
#line 104 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1569 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 13:
#line 105 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1575 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 14:
#line 106 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1581 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 15:
#line 107 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1587 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 16:
#line 108 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = (yyvsp[0].exp);}
#line 1593 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 17:
#line 112 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.decList) = A_DecList((yyvsp[0].dec), NULL);}
#line 1599 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 18:
#line 113 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.decList) = A_DecList((yyvsp[-1].dec), (yyvsp[0].decList));}
#line 1605 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 19:
#line 115 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.dec) = A_TypeDec(EM_tokPos, (yyvsp[0].nametyList));}
#line 1611 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 20:
#line 116 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.dec) = (yyvsp[0].dec);}
#line 1617 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 21:
#line 117 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.dec) = A_FunctionDec(EM_tokPos, (yyvsp[0].fundecList));}
#line 1623 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 22:
#line 119 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.nametyList) = A_NametyList((yyvsp[0].namety), NULL);}
#line 1629 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 23:
#line 120 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.nametyList) = A_NametyList((yyvsp[-1].namety), (yyvsp[0].nametyList));}
#line 1635 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 24:
#line 122 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.namety) = A_Namety(S_Symbol((yyvsp[-2].sval)), (yyvsp[0].ty));}
#line 1641 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 25:
#line 124 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.ty) = A_NameTy(EM_tokPos, S_Symbol((yyvsp[0].sval)));}
#line 1647 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 26:
#line 125 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.ty) = A_RecordTy(EM_tokPos, (yyvsp[-1].fieldList));}
#line 1653 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 27:
#line 126 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.ty) = A_ArrayTy(EM_tokPos, S_Symbol((yyvsp[0].sval)));}
#line 1659 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 28:
#line 128 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.fieldList) = NULL;}
#line 1665 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 29:
#line 129 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.fieldList) = A_FieldList((yyvsp[0].field), NULL); }
#line 1671 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 30:
#line 130 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.fieldList) = A_FieldList((yyvsp[-2].field), (yyvsp[0].fieldList)); }
#line 1677 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 31:
#line 132 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.field) = A_Field(EM_tokPos, S_Symbol((yyvsp[-2].sval)), S_Symbol((yyvsp[0].sval)));}
#line 1683 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 32:
#line 134 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                        {(yyval.dec) = A_VarDec(EM_tokPos, S_Symbol((yyvsp[-2].sval)), NULL, (yyvsp[0].exp));}
#line 1689 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 33:
#line 135 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                        {(yyval.dec) = A_VarDec(EM_tokPos, S_Symbol((yyvsp[-4].sval)), S_Symbol((yyvsp[-2].sval)), (yyvsp[0].exp));}
#line 1695 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 34:
#line 137 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.fundecList) = A_FundecList((yyvsp[0].fundec), NULL);}
#line 1701 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 35:
#line 138 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.fundecList) = A_FundecList((yyvsp[-1].fundec), (yyvsp[0].fundecList));}
#line 1707 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 36:
#line 140 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                                {(yyval.fundec) = A_Fundec(EM_tokPos, S_Symbol((yyvsp[-5].sval)), (yyvsp[-3].fieldList), NULL, (yyvsp[0].exp));}
#line 1713 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 37:
#line 141 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                                {(yyval.fundec) = A_Fundec(EM_tokPos, S_Symbol((yyvsp[-7].sval)), (yyvsp[-5].fieldList), S_Symbol((yyvsp[-2].sval)), (yyvsp[0].exp));}
#line 1719 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 38:
#line 145 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = A_IntExp(EM_tokPos, (yyvsp[0].ival));}
#line 1725 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 39:
#line 146 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = A_StringExp(EM_tokPos, (yyvsp[0].sval));}
#line 1731 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 40:
#line 147 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.exp) = A_NilExp(EM_tokPos);}
#line 1737 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 41:
#line 149 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.var) = A_SimpleVar(EM_tokPos, S_Symbol((yyvsp[0].sval)));}
#line 1743 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 42:
#line 151 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.var) = A_SubscriptVar(EM_tokPos, A_SimpleVar(EM_tokPos, S_Symbol((yyvsp[-3].sval))), (yyvsp[-1].exp));}
#line 1749 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 43:
#line 152 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.var) = A_FieldVar(EM_tokPos, (yyvsp[-2].var), S_Symbol((yyvsp[0].sval)));}
#line 1755 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 44:
#line 153 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                    {(yyval.var) = A_SubscriptVar(EM_tokPos, (yyvsp[-3].var), (yyvsp[-1].exp));}
#line 1761 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 45:
#line 155 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_ArrayExp(EM_tokPos, S_Symbol((yyvsp[-5].sval)), (yyvsp[-3].exp), (yyvsp[0].exp));}
#line 1767 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 46:
#line 157 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_SeqExp(EM_tokPos, (yyvsp[-1].expList));}
#line 1773 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 47:
#line 158 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_SeqExp(EM_tokPos, NULL);}
#line 1779 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 48:
#line 160 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.expList) = A_ExpList((yyvsp[0].exp), NULL);}
#line 1785 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 49:
#line 161 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.expList) = A_ExpList((yyvsp[-2].exp), (yyvsp[0].expList));}
#line 1791 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 50:
#line 163 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_CallExp(EM_tokPos, S_Symbol((yyvsp[-2].sval)), NULL);}
#line 1797 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 51:
#line 164 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_CallExp(EM_tokPos, S_Symbol((yyvsp[-3].sval)), (yyvsp[-1].expList));}
#line 1803 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 52:
#line 166 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.expList) = A_ExpList((yyvsp[0].exp), NULL);}
#line 1809 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 53:
#line 167 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.expList) = A_ExpList((yyvsp[-2].exp), (yyvsp[0].expList));}
#line 1815 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 54:
#line 169 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_plusOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1821 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 55:
#line 170 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_minusOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1827 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 56:
#line 171 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_timesOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1833 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 57:
#line 172 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_divideOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1839 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 58:
#line 173 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_minusOp, A_IntExp(EM_tokPos, 0), (yyvsp[0].exp));}
#line 1845 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 59:
#line 174 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_eqOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1851 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 60:
#line 175 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_neqOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1857 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 61:
#line 176 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_gtOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1863 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 62:
#line 177 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_ltOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1869 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 63:
#line 178 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_geOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1875 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 64:
#line 179 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_OpExp(EM_tokPos, A_leOp, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1881 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 65:
#line 180 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {
                                                    (yyval.exp) = A_IfExp(EM_tokPos,
                                                        (yyvsp[-2].exp),
                                                        (yyvsp[0].exp),
                                                        A_IntExp(EM_tokPos, 0));
                                                }
#line 1892 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 66:
#line 186 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {
                                                    (yyval.exp) = A_IfExp(EM_tokPos,
                                                        (yyvsp[-2].exp),
                                                        A_IntExp(EM_tokPos, 1),
                                                        (yyvsp[0].exp));
                                                }
#line 1903 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 67:
#line 193 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_RecordExp(EM_tokPos, S_Symbol((yyvsp[-2].sval)), NULL);}
#line 1909 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 68:
#line 194 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.exp) = A_RecordExp(EM_tokPos, S_Symbol((yyvsp[-3].sval)), (yyvsp[-1].efieldList));}
#line 1915 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 69:
#line 196 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.efieldList) = A_EfieldList((yyvsp[0].efield), NULL);}
#line 1921 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 70:
#line 197 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.efieldList) = A_EfieldList((yyvsp[-2].efield), (yyvsp[0].efieldList));}
#line 1927 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 71:
#line 199 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                {(yyval.efield) = A_Efield(S_Symbol((yyvsp[-2].sval)), (yyvsp[0].exp));}
#line 1933 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 72:
#line 203 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                    {(yyval.exp) = A_AssignExp(EM_tokPos, (yyvsp[-2].var), (yyvsp[0].exp));}
#line 1939 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 73:
#line 205 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                    {(yyval.exp) = A_IfExp(EM_tokPos, (yyvsp[-2].exp), (yyvsp[0].exp), NULL);}
#line 1945 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 74:
#line 206 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                    {(yyval.exp) = A_IfExp(EM_tokPos, (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1951 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 75:
#line 208 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                    {(yyval.exp) = A_WhileExp(EM_tokPos, (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1957 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 76:
#line 210 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                    {(yyval.exp) = A_ForExp(EM_tokPos, S_Symbol((yyvsp[-6].sval)), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp));}
#line 1963 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;

  case 77:
#line 212 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger.y"
                                                    {(yyval.exp) = A_LetExp(EM_tokPos, (yyvsp[-3].decList), A_SeqExp(EM_tokPos, (yyvsp[-1].expList)));}
#line 1969 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"
    break;


#line 1973 "/home/glasser/toys/tiger-compiler/src/compiler_rewrite/tiger_parser.c"

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

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
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
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
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
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



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
                      yytoken, &yylval);
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
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

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


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


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
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
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
  return yyresult;
}
