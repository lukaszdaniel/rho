/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2018  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* Internal header, not installed */

/** @file Defn.h
 *
 * @brief A ragbag.
 *
 * As rho development proceeds, the type definitions, many function
 * prototypes etc. defined in this header file will disappear, because
 * the relevant functionality will have been absorbed into the rho
 * core, and declared within the appropriate header file in the
 * <tt>src/include/rho</tt> directory.
 *
 * In a few cases, a declaration within this file is repeated in a
 * header file under <tt>src/include/rho</tt>; this is because source
 * files within the rho core never <tt>\#include</tt>s
 * <tt>Defn.h</tt> itself (nor <tt>Rinternals.h</tt>.  In such a case
 * the relevant rho header file is <tt>\#include</tt>d back into
 * <tt>Defn.h</tt>, so that the compiler can detect any inconsistency
 * between the two declarations.
 */

#ifndef DEFN_H_
#define DEFN_H_

#ifdef USE_RINTERNALS
#undef USE_RINTERNALS
#endif

#ifdef __cplusplus
#include <iostream>
#include <rho/ArgList.hpp>
#include <rho/Frame.hpp>
#include <rho/ListVector.hpp>

#define rhoLOG(x) std::cout << __FILE__ << ":" << __LINE__ << " " << x << std::endl;

namespace rho
{
    class BuiltInFunction;
}
#endif

/* To test the write barrier used by the generational collector,
   define TESTING_WRITE_BARRIER.  This makes the internal structure of
   SEXPRECs visible only inside of files that explicitly define
   USE_RINTERNALS, and all uses of SEXPREC fields that do not go
   through the appropriate functions or macros will become compilation
   errors.  Since this does impose a small but noticable performance
   penalty, code that includes Defn.h (or code that explicitly defines
   USE_RINTERNALS) can access a SEXPREC's fields directly. */

//#ifndef TESTING_WRITE_BARRIER
//# define USE_RINTERNALS
//#endif

#include <R_ext/Visibility.h>

#if 0
#ifdef __MAIN__
# define extern0 HIDDEN
#else
# define extern0 extern
#endif
#endif

#ifdef __cplusplus
constexpr size_t MAXELTSIZE = 8192; /* Used as a default for string buffer sizes,
			   and occasionally as a limit. */
#endif

#include <R_ext/Complex.h>
#ifdef __cplusplus
extern "C" {
#endif
void Rf_CoercionWarning(int); /* warning code */
int Rf_LogicalFromInteger(int, int*);
int Rf_LogicalFromReal(double, int*);
int Rf_LogicalFromComplex(Rcomplex, int*);
int Rf_IntegerFromLogical(int, int*);
int Rf_IntegerFromReal(double, int*);
int Rf_IntegerFromComplex(Rcomplex, int*);
double Rf_RealFromLogical(int, int*);
double Rf_RealFromInteger(int, int*);
double Rf_RealFromComplex(Rcomplex, int*);
Rcomplex Rf_ComplexFromLogical(int, int*);
Rcomplex Rf_ComplexFromInteger(int, int*);
Rcomplex Rf_ComplexFromReal(double, int*);
#ifdef __cplusplus
} //extern "C"
#endif

#define CALLED_FROM_DEFN_H 1
#include <Rinternals.h>		/*-> Arith.h, Boolean.h, Complex.h, Error.h,
				  Memory.h, PrtUtil.h, Utils.h */
#undef CALLED_FROM_DEFN_H
/* Below variables are defined in src/include/rho/PredefinedSymbols.hpp */
extern HIDDEN SEXP	R_CommentSymbol;    /* "comment" */
extern HIDDEN SEXP	R_DotEnvSymbol;     /* ".Environment" */
extern HIDDEN SEXP	R_ExactSymbol;	    /* "exact" */
extern HIDDEN SEXP	R_RecursiveSymbol;  /* "recursive" */
extern HIDDEN SEXP	R_WholeSrcrefSymbol;   /* "wholeSrcref" */
extern HIDDEN SEXP	R_TmpvalSymbol;     /* "*tmp*" */
extern HIDDEN SEXP	R_UseNamesSymbol;   /* "use.names" */
extern HIDDEN SEXP	R_ColonSymbol;         /* ":" */
//extern HIDDEN SEXP	R_DoubleColonSymbol;   /* "::" */
//extern HIDDEN SEXP	R_TripleColonSymbol;   /* ":::" */
extern HIDDEN SEXP    R_ConnIdSymbol;  /* "conn_id" */
extern HIDDEN SEXP    R_DevicesSymbol;  /* ".Devices" */

extern HIDDEN SEXP    R_dot_Methods;  /* ".Methods" */
extern HIDDEN SEXP    R_dot_Group;  /* ".Group" */
extern HIDDEN SEXP    R_dot_Class;  /* ".Class" */
extern HIDDEN SEXP    R_dot_GenericCallEnv;  /* ".GenericCallEnv" */
extern HIDDEN SEXP    R_dot_GenericDefEnv;  /* ".GenericDefEnv" */

//extern0 SEXP	R_StringHash;       /* Global hash of CHARSXPs */


 /* writable char access for R internal use only */
//#define CHAR_RW(x)	((char *) CHAR(x))

/* CHARSXP charset bits */
#if 0
#define BYTES_MASK (1<<1)
#define LATIN1_MASK (1<<2)
#define UTF8_MASK (1<<3)
/* (1<<4) is taken by S4_OBJECT_MASK */
#define CACHED_MASK (1<<5)
#define ASCII_MASK (1<<6)
#define HASHASH_MASK 1
#endif
/**** HASHASH uses the first bit -- see HASHASH_MASK defined below */

#ifdef __cplusplus
extern "C" {
#endif

#if 0 //ifdef USE_RINTERNALS
# define IS_BYTES(x) ((x)->sxpinfo.gp & BYTES_MASK)
# define SET_BYTES(x) (((x)->sxpinfo.gp) |= BYTES_MASK)
# define IS_LATIN1(x) ((x)->sxpinfo.gp & LATIN1_MASK)
# define SET_LATIN1(x) (((x)->sxpinfo.gp) |= LATIN1_MASK)
# define IS_ASCII(x) ((x)->sxpinfo.gp & ASCII_MASK)
# define SET_ASCII(x) (((x)->sxpinfo.gp) |= ASCII_MASK)
# define IS_UTF8(x) ((x)->sxpinfo.gp & UTF8_MASK)
# define SET_UTF8(x) (((x)->sxpinfo.gp) |= UTF8_MASK)
# define ENC_KNOWN(x) ((x)->sxpinfo.gp & (LATIN1_MASK | UTF8_MASK))
# define SET_CACHED(x) (((x)->sxpinfo.gp) |= CACHED_MASK)
# define IS_CACHED(x) (((x)->sxpinfo.gp) & CACHED_MASK)
#else
/* Needed only for write-barrier testing */
int IS_BYTES(SEXP x);
//void SET_BYTES(SEXP x);
int IS_LATIN1(SEXP x);
//void SET_LATIN1(SEXP x);
int IS_ASCII(SEXP x);
//void SET_ASCII(SEXP x);
int IS_UTF8(SEXP x);
//void SET_UTF8(SEXP x);
int ENC_KNOWN(SEXP x);
//int SET_CACHED(SEXP x);
int IS_CACHED(SEXP x); //not implemented in rho, because always true.
#endif
/* macros and declarations for managing CHARSXP cache */
#if 0
# define CXHEAD(x) (x)
# define CXTAIL(x) ATTRIB(x)
SEXP (SET_CXTAIL)(SEXP x, SEXP y);
#endif

#include <Errormsg.h>

extern void R_ProcessEvents(void);
#ifdef Win32
extern void R_WaitEvent(void);
#endif

#ifdef __cplusplus
} //extern "C"
#endif

#ifdef R_USE_SIGNALS
#ifdef Win32
# include <psignal.h>
#else
# include <signal.h>
# include <setjmp.h>
#endif
#endif

#ifdef Unix
# define OSTYPE      "unix"
# define FILESEP     "/"
#endif /* Unix */

#ifdef Win32
# define OSTYPE      "windows"
# define FILESEP     "/"
#endif /* Win32 */

#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)	x ## _
# define F77_QSYMBOL(x)	#x "_"
#else
# define F77_SYMBOL(x)	x
# define F77_QSYMBOL(x) #x
#endif

/*  Heap and Pointer Protection Stack Sizes.  */

/* These headers are all required by C99.
   However, we use types below such as uintptr_t which are optional in C11.
   And on some older systems they were in inttypes.h but not stdint.h.

   Up to 2.11.1 (r52035, May 2010) we had

#if !defined(HAVE_INTPTR_T) && !defined(intptr_t)
 typedef long intptr_t;
#endif
#if !defined(HAVE_UINTPTR_T) && !defined(uintptr_t)
 typedef unsigned long uintptr_t;
#endif
    but size_t might be better.

 */
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
/* According to POSIX inttypes.h should include stdint.h,
   but let's be sure. */
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif

#if defined HAVE_DECL_SIZE_MAX && HAVE_DECL_SIZE_MAX
#ifdef __cplusplus
using R_size_t = size_t;
constexpr R_size_t R_SIZE_T_MAX = std::numeric_limits<R_size_t>::max();
#else
typedef size_t R_size_t;
#define R_SIZE_T_MAX SIZE_MAX
#endif // __cplusplus
#else
#error SIZE_MAX is required for C99
#endif

#ifdef __cplusplus
constexpr size_t Mega = 1048576; /* 1 Mega Byte := 2^20 (= 1048576) Bytes */
constexpr size_t Giga = 1073741824; /* 1 Giga Byte := 2^30 Bytes */
#endif
/*  R_VSIZE	   The initial heap size in bytes */
/*  This is a default value and can be overridden in config.h
    The maxima and minima are in ../main/startup.cpp */

#if 0
#ifndef R_PPSSIZE
#define	R_PPSSIZE	50000L
#endif
#ifndef R_NSIZE
#define	R_NSIZE		350000L
#endif
#endif
#ifndef R_VSIZE
#define	R_VSIZE		16000000L
#endif

/* some commonly needed headers */
#include <math.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* declare substitutions */
#if !defined(strdup) && defined(HAVE_DECL_STRDUP) && !HAVE_DECL_STRDUP
extern char *strdup(const char *s1);
#endif
#if !defined(strncascmp) && defined(HAVE_DECL_STRNCASECMP) && !HAVE_DECL_STRNCASECMP
extern int strncasecmp(const char *s1, const char *s2, size_t n);
#endif

/* Glibc manages to not define this in -pedantic -ansi */
#if defined(HAVE_PUTENV) && !defined(putenv) && defined(HAVE_DECL_PUTENV) && !HAVE_DECL_PUTENV
extern int putenv(char *string);
#endif


/* Maximal length in bytes of an entire path name.
   POSIX has required this to be at least 255/256, and X/Open at least 1024.
   Solaris has 1024, Linux glibc has 4192.
   File names are limited to FILENAME_MAX bytes (usually the same as PATH_MAX)
   or NAME_MAX (often 255/256).
 */
#if !defined(PATH_MAX)
# if defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
# endif
# if !defined(PATH_MAX)
#  if defined(MAXPATHLEN)
/* Try BSD name */
#    define PATH_MAX MAXPATHLEN
#  elif defined(Win32)
/* seems this is now defined by MinGW to be 259, whereas FILENAME_MAX
   and MAX_PATH are 260.  It is not clear that this really is in bytes,
   but might be chars for the Unicode interfaces.

   260 is d:\ plus 256 chars plus nul.  Some but not all API calls
   allow filepaths of the form \\?\D:\very_long_path .
*/
#    define PATH_MAX 260
#  else
/* quite possibly unlimited, so we make this large, and test when used */
#    define PATH_MAX 5000
#  endif
# endif
#endif

#if 1 //#ifdef R_USE_SIGNALS
#ifdef HAVE_POSIX_SETJMP
# define SIGJMP_BUF sigjmp_buf
# define SIGSETJMP(x,s) sigsetjmp(x,s)
# define SIGLONGJMP(x,i) siglongjmp(x,i)
# define JMP_BUF sigjmp_buf
# define SETJMP(x) sigsetjmp(x,0)
# define LONGJMP(x,i) siglongjmp(x,i)
#else
# define SIGJMP_BUF jmp_buf
# define SIGSETJMP(x,s) setjmp(x)
# define SIGLONGJMP(x,i) longjmp(x,i)
# define JMP_BUF jmp_buf
# define SETJMP(x) setjmp(x)
# define LONGJMP(x,i) longjmp(x,i)
#endif
#endif

#if 0
#define HSIZE	  49157	/* The size of the hash table for symbols */
#define MAXIDSIZE 10000	/* Largest symbol size,
			   in bytes excluding terminator.
			   Was 256 prior to 2.13.0, now just a sanity check.
			*/

/* The type of the do_xxxx functions. */
/* These are the built-in R functions. */
typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

/* Information for Deparsing Expressions */
typedef enum {
    PP_INVALID  =  0,
    PP_ASSIGN   =  1,
    PP_ASSIGN2  =  2,
    PP_BINARY   =  3,
    PP_BINARY2  =  4,
    PP_BREAK    =  5,
    PP_CURLY    =  6,
    PP_FOR      =  7,
    PP_FUNCALL  =  8,
    PP_FUNCTION =  9,
    PP_IF 	= 10,
    PP_NEXT 	= 11,
    PP_PAREN    = 12,
    PP_RETURN   = 13,
    PP_SUBASS   = 14,
    PP_SUBSET   = 15,
    PP_WHILE 	= 16,
    PP_UNARY 	= 17,
    PP_DOLLAR 	= 18,
    PP_FOREIGN 	= 19,
    PP_REPEAT 	= 20
} PPkind;

typedef enum {
    PREC_FN	 = 0,
    PREC_EQ	 = 1,
    PREC_LEFT    = 2,
    PREC_RIGHT	 = 3,
    PREC_TILDE	 = 4,
    PREC_OR	 = 5,
    PREC_AND	 = 6,
    PREC_NOT	 = 7,
    PREC_COMPARE = 8,
    PREC_SUM	 = 9,
    PREC_PROD	 = 10,
    PREC_PERCENT = 11,
    PREC_COLON	 = 12,
    PREC_SIGN	 = 13,
    PREC_POWER	 = 14,
    PREC_SUBSET  = 15,
    PREC_DOLLAR	 = 16,
    PREC_NS	 = 17
} PPprec;

typedef struct {
	PPkind kind; 	 /* deparse kind */
	PPprec precedence; /* operator precedence */
	unsigned int rightassoc;  /* right associative? */
} PPinfo;

/* The type definitions for the table of built-in functions. */
/* This table can be found in ../main/names.cpp */
typedef struct {
    char   *name;    /* print name */
    CCODE  cfun;     /* c-code address */
    int	   code;     /* offset within c-code */
    int	   eval;     /* evaluate args? */
    int	   arity;    /* function arity */
    PPinfo gram;     /* pretty-print info */
} FUNTAB;
#endif

//#ifdef USE_RINTERNALS
/* There is much more in Rinternals.h, including function versions
 * of the Promise and Hashing groups.
 */

/* Primitive Access Macros */
#if 0
#define PRIMOFFSET(x)	((x)->u.primsxp.offset)
#define SET_PRIMOFFSET(x,v)	(((x)->u.primsxp.offset)=(v))
#define PRIMFUN(x)	(R_FunTab[(x)->u.primsxp.offset].cfun)
#define PRIMNAME(x)	(R_FunTab[(x)->u.primsxp.offset].name)
#define PRIMVAL(x)	(R_FunTab[(x)->u.primsxp.offset].code)
#define PRIMARITY(x)	(R_FunTab[(x)->u.primsxp.offset].arity)
#define PPINFO(x)	(R_FunTab[(x)->u.primsxp.offset].gram)
#define PRIMPRINT(x)	(((R_FunTab[(x)->u.primsxp.offset].eval)/100)%10)
#define PRIMINTERNAL(x)	(((R_FunTab[(x)->u.primsxp.offset].eval)%100)/10)

/* Promise Access Macros */
#define PRCODE(x)	((x)->u.promsxp.expr)
#define PRENV(x)	((x)->u.promsxp.env)
#define PRVALUE(x)	((x)->u.promsxp.value)
#define PRSEEN(x)	((x)->sxpinfo.gp)
#define SET_PRSEEN(x,v)	(((x)->sxpinfo.gp)=(v))

/* Hashing Macros */
#define HASHASH(x)      ((x)->sxpinfo.gp & HASHASH_MASK)
#define HASHVALUE(x)    ((int) TRUELENGTH(x))
#define SET_HASHASH(x,v) ((v) ? (((x)->sxpinfo.gp) |= HASHASH_MASK) : \
			  (((x)->sxpinfo.gp) &= (~HASHASH_MASK)))
#define SET_HASHVALUE(x,v) SET_TRUELENGTH(x, ((int) (v)))
#endif

/* Vector Heap Structure */
/* sizeof(VECREC) is used for some backwards-compatibility purposes in rho, and that's all. */
typedef struct
{
    union {
        SEXP backpointer;
        double align;
    } u;
} VECREC, *VECP;

/* Primitive Access Macros */
unsigned int PRIMVAL(SEXP x);
const char* PRIMNAME(SEXP x);

/* Vector Heap Macros */
//#define BACKPOINTER(v)	((v).u.backpointer)
#ifdef __cplusplus
inline size_t BYTE2VEC(int n)
{
    return (n > 0) ? (std::size_t(n) - 1)/sizeof(VECREC) + 1 : 0;
}

inline size_t INT2VEC(int n)
{
    return (n > 0) ? (std::size_t(n)*sizeof(int) - 1)/sizeof(VECREC) + 1 : 0;
}

inline size_t FLOAT2VEC(int n)
{
    return (n > 0) ? (std::size_t(n)*sizeof(double) - 1)/sizeof(VECREC) + 1 : 0;
}

inline size_t COMPLEX2VEC(int n)
{
    return (n > 0) ? (std::size_t(n)*sizeof(Rcomplex) - 1)/sizeof(VECREC) + 1 : 0;
}

inline size_t PTR2VEC(int n)
{
    return (n > 0) ? (std::size_t(n)*sizeof(SEXP) - 1)/sizeof(VECREC) + 1 : 0;
}

#endif // __cplusplus

/* Bindings */
/* use the same bits (15 and 14) in symbols and bindings */
#if 0
#define ACTIVE_BINDING_MASK (1<<15)
#define BINDING_LOCK_MASK (1<<14)
#define SPECIAL_BINDING_MASK (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_IS_LOCKED(b) ((b)->sxpinfo.gp & BINDING_LOCK_MASK)
#define SET_ACTIVE_BINDING_BIT(b) ((b)->sxpinfo.gp |= ACTIVE_BINDING_MASK)
#define LOCK_BINDING(b) ((b)->sxpinfo.gp |= BINDING_LOCK_MASK)
#define UNLOCK_BINDING(b) ((b)->sxpinfo.gp &= (~BINDING_LOCK_MASK))

#define BASE_SYM_CACHED_MASK (1 << 13)
#define SET_BASE_SYM_CACHED(b) ((b)->sxpinfo.gp |= BASE_SYM_CACHED_MASK)
#define UNSET_BASE_SYM_CACHED(b) ((b)->sxpinfo.gp &= (~BASE_SYM_CACHED_MASK))
#define BASE_SYM_CACHED(b) ((b)->sxpinfo.gp & BASE_SYM_CACHED_MASK)

#define SPECIAL_SYMBOL_MASK (1 << 12)
#define SET_SPECIAL_SYMBOL(b) ((b)->sxpinfo.gp |= SPECIAL_SYMBOL_MASK)
#define UNSET_SPECIAL_SYMBOL(b) ((b)->sxpinfo.gp &= (~SPECIAL_SYMBOL_MASK))
#define IS_SPECIAL_SYMBOL(b) ((b)->sxpinfo.gp & SPECIAL_SYMBOL_MASK)
#define SET_NO_SPECIAL_SYMBOLS(b) ((b)->sxpinfo.gp |= SPECIAL_SYMBOL_MASK)
#define UNSET_NO_SPECIAL_SYMBOLS(b) ((b)->sxpinfo.gp &= (~SPECIAL_SYMBOL_MASK))
#define NO_SPECIAL_SYMBOLS(b) ((b)->sxpinfo.gp & SPECIAL_SYMBOL_MASK)

//#else /* USE_RINTERNALS */

typedef struct VECREC *VECP;
int (PRIMOFFSET)(SEXP x);
void (SET_PRIMOFFSET)(SEXP x, int v);

#define PRIMFUN(x) (R_FunTab[PRIMOFFSET(x)].cfun)
#define PRIMNAME(x) (R_FunTab[PRIMOFFSET(x)].name)
#define PRIMVAL(x) (R_FunTab[PRIMOFFSET(x)].code)
#define PRIMARITY(x) (R_FunTab[PRIMOFFSET(x)].arity)
#define PPINFO(x) (R_FunTab[PRIMOFFSET(x)].gram)
#define PRIMPRINT(x) (((R_FunTab[PRIMOFFSET(x)].eval) / 100) % 10)
#define PRIMINTERNAL(x) (((R_FunTab[PRIMOFFSET(x)].eval) % 100) / 10)

Rboolean (IS_ACTIVE_BINDING)(SEXP b);
Rboolean (BINDING_IS_LOCKED)(SEXP b);
void (SET_ACTIVE_BINDING_BIT)(SEXP b);
void (LOCK_BINDING)(SEXP b);
void (UNLOCK_BINDING)(SEXP b);

void (SET_BASE_SYM_CACHED)(SEXP b);
void (UNSET_BASE_SYM_CACHED)(SEXP b);
Rboolean (BASE_SYM_CACHED)(SEXP b);

void (SET_SPECIAL_SYMBOL)(SEXP b);
void (UNSET_SPECIAL_SYMBOL)(SEXP b);
Rboolean (IS_SPECIAL_SYMBOL)(SEXP b);
void (SET_NO_SPECIAL_SYMBOLS)(SEXP b);
void (UNSET_NO_SPECIAL_SYMBOLS)(SEXP b);
Rboolean (NO_SPECIAL_SYMBOLS)(SEXP b);

//#endif /* USE_RINTERNALS */

#define TYPED_STACK
#ifdef TYPED_STACK
/* The typed stack's entries consist of a tag and a union. An entry
   can represent a standard SEXP value (tag = 0) or an unboxed scalar
   value. For now real, integer, and logical values are supported. It
   would in principle be possible to support complex scalars and short
   scalar strings, but it isn't clear if this is worth while.

   In addition to unboxed values the typed stack can hold partially
   evaluated or incomplete allocated values. For now this is only used
   for holding a short representation of an integer sequence as produce
   by the colon operator, seq_len, or seq_along, and as consumed by
   compiled 'for' loops. This could be used more extensively in the
   future.
*/
typedef struct
{
    int tag;
    union {
        int ival;
        double dval;
        SEXP sxpval;
    } u;
} R_bcstack_t;
# define PARTIALSXP_MASK (~255)
# define IS_PARTIAL_SXP_TAG(x) ((x) & PARTIALSXP_MASK)
# define RAWMEM_TAG 254
#else
typedef SEXP R_bcstack_t;
#endif
#ifdef BC_INT_STACK
typedef union { void *p; int i; } IStackval;
#endif

#ifdef R_USE_SIGNALS
/* Stack entry for pending promises */
typedef struct RPRSTACK {
    SEXP promise;
    struct RPRSTACK *next;
} RPRSTACK;

/* Evaluation Context Structure */
typedef struct RCNTXT {
    struct RCNTXT *nextcontext;	/* The next context up the chain */
    int callflag;		/* The context "type" */
    JMP_BUF cjmpbuf;		/* C stack and register information */
    int cstacktop;		/* Top of the pointer protection stack */
    int evaldepth;	        /* evaluation depth at inception */
    SEXP promargs;		/* Promises supplied to closure */
    SEXP callfun;		/* The closure called */
    SEXP sysparent;		/* environment the closure was called from */
    SEXP call;			/* The call that effected this context*/
    SEXP cloenv;		/* The environment */
    SEXP conexit;		/* Interpreted "on.exit" code */
    void (*cend)(void *);	/* C "on.exit" thunk */
    void *cenddata;		/* data for C "on.exit" thunk */
    void *vmax;		        /* top of R_alloc stack */
    int intsusp;                /* interrupts are suspended */
    int gcenabled;		/* R_GCEnabled value */
    int bcintactive;            /* R_BCIntActive value */
    SEXP bcbody;                /* R_BCbody value */
    void* bcpc;                 /* R_BCpc value */
    SEXP handlerstack;          /* condition handler stack */
    SEXP restartstack;          /* stack of available restarts */
    struct RPRSTACK *prstack;   /* stack of pending promises */
    R_bcstack_t *nodestack;
#ifdef BC_INT_STACK
    IStackval *intstack;
#endif
    SEXP srcref;	        /* The source line in effect */
    int browserfinish;          /* should browser finish this context without
                                   stopping */
    SEXP returnValue;           /* only set during on.exit calls */
    struct RCNTXT *jumptarget;	/* target for a continuing jump */
    int jumpmask;               /* associated LONGJMP argument */
} RCNTXT, *context;

/* The Various Context Types.

 * In general the type is a bitwise OR of the values below.
 * Note that CTXT_LOOP is already the or of CTXT_NEXT and CTXT_BREAK.
 * Only functions should have the third bit turned on;
 * this allows us to move up the context stack easily
 * with either RETURN's or GENERIC's or RESTART's.
 * If you add a new context type for functions make sure
 *   CTXT_NEWTYPE & CTXT_FUNCTION > 0
 */
enum {
    CTXT_TOPLEVEL = 0,
    CTXT_NEXT = 1,
    CTXT_BREAK = 2,
    CTXT_LOOP = 3, /* break OR next target */
    CTXT_FUNCTION = 4,
    CTXT_CCODE = 8,
    CTXT_RETURN = 12,
    CTXT_BROWSER = 16,
    CTXT_GENERIC = 20,
    CTXT_RESTART = 32,
    CTXT_BUILTIN = 64, /* used in profiling */
    CTXT_UNWIND = 128
};

/*    1 2 4 8 ...
TOP   0 0 0 0 0 0  = 0
NEX   1 0 0 0 0 0  = 1
BRE   0 1 0 0 0 0  = 2
LOO   1 1 0 0 0 0  = 3
FUN   0 0 1 0 0 0  = 4
CCO   0 0 0 1 0 0  = 8
BRO   0 0 0 0 1 0  = 16
RET   0 0 1 1 0 0  = 12
GEN   0 0 1 0 1 0  = 20
RES   0 0 0 0 0 0 1 = 32
BUI   0 0 0 0 0 0 0 1 = 64
*/

#define IS_RESTART_BIT_SET(flags) ((flags) & CTXT_RESTART)
#define SET_RESTART_BIT_ON(flags) (flags |= CTXT_RESTART)
#define SET_RESTART_BIT_OFF(flags) (flags &= ~CTXT_RESTART)
#endif
#endif

/* Miscellaneous Definitions */
inline Rboolean streql(const char* s, const char* t)
{
    return (Rboolean)(strcmp(s, t) == 0);
}
inline Rboolean streqln(const char* s, const char* t, size_t n)
{
    return (Rboolean)(strncmp(s, t, n) == 0);
}

/* Arithmetic and Relation Operators */
typedef enum
{
    PLUSOP = 1,
    MINUSOP,
    TIMESOP,
    DIVOP,
    POWOP,
    MODOP,
    IDIVOP
} ARITHOP_TYPE;

typedef enum
{
    EQOP = 1,
    NEOP,
    LTOP,
    LEOP,
    GEOP,
    GTOP
} RELOP_TYPE;

typedef enum
{
    MATPROD_DEFAULT = 1,
    MATPROD_INTERNAL,
    MATPROD_BLAS,
    MATPROD_DEFAULT_SIMD /* experimental */
} MATPROD_TYPE;

/* File Handling */
/*
#define R_EOF	65535
*/
#define R_EOF	-1


/*--- Global Variables ---------------------------------------------------- */

/* Defined and initialized in names.cpp (not main.cpp) :*/
#if 0
#ifndef __R_Names__
extern
#endif
FUNTAB	R_FunTab[];	    /* Built in functions */
#endif


#include <R_ext/libextern.h>

#if 0
#ifdef __MAIN__
# define INI_as(v) = v
#define extern0 HIDDEN
#else
# define INI_as(v)
#define extern0 extern
#endif
#endif

LibExtern SEXP  R_SrcfileSymbol;    /* "srcfile" */
LibExtern SEXP  R_SrcrefSymbol;     /* "srcref" */


LibExtern Rboolean R_interrupts_suspended;
LibExtern int R_interrupts_pending;

/* R Home Directory */
LibExtern char *R_Home;		    /* Root of the R tree */

/* Memory Management */
//extern0 R_size_t R_NSize  INI_as(R_NSIZE);/* Size of cons cell heap */
extern HIDDEN R_size_t R_VSize;/* Initial size of the heap */
#if 0
extern0 int	R_GCEnabled INI_as(1);
extern0 int	R_in_gc INI_as(0);
extern0 int	R_BCIntActive INI_as(0); /* bcEval called more recently than
                                            eval */
extern0 void*	R_BCpc INI_as(NULL);/* current byte code instruction */
extern0 SEXP	R_BCbody INI_as(NULL); /* current byte code object */
extern0 SEXP	R_NHeap;	    /* Start of the cons cell heap */
extern0 SEXP	R_FreeSEXP;	    /* Cons cell free list */
extern0 R_size_t R_Collected;	    /* Number of free cons cells (after gc) */
#endif
extern HIDDEN int	R_Is_Running;	    /* for Windows memory manager */

/* The Pointer Protection Stack */
#if 0
LibExtern int	R_PPStackSize	INI_as(R_PPSSIZE); /* The stack size (elements) */
LibExtern int	R_PPStackTop;	    /* The top of the stack */
LibExtern SEXP*	R_PPStack;	    /* The pointer protection stack */

/* Evaluation Environment */
extern0 SEXP	R_CurrentExpr;	    /* Currently evaluating expression */
extern0 SEXP	R_ReturnedValue;    /* Slot for return-ing values */
extern0 SEXP*	R_SymbolTable;	    /* The symbol table */
#ifdef R_USE_SIGNALS
extern0 RCNTXT R_Toplevel;	      /* Storage for the toplevel context */
extern0 RCNTXT* R_ToplevelContext;  /* The toplevel context */
LibExtern RCNTXT* R_GlobalContext;    /* The global context */
extern0 RCNTXT* R_SessionContext;   /* The session toplevel context */
extern0 RCNTXT* R_ExitContext;      /* The active context for on.exit processing */
#endif
#endif
extern Rboolean R_Visible;	    /* Value visibility flag */
extern HIDDEN int	R_EvalDepth;	/* Evaluation recursion depth */
extern HIDDEN int	R_BrowseLines;	/* lines/per call in browser :
						 * options(deparse.max.lines) */
//extern0 int	R_Expressions	INI_as(5000);	/* options(expressions) */
//extern0 int	R_Expressions_keep INI_as(5000);/* options(expressions) */
extern HIDDEN Rboolean R_KeepSource;	/* options(keep.source) */
extern HIDDEN Rboolean R_CBoundsCheck;	/* options(CBoundsCheck) */
extern HIDDEN MATPROD_TYPE R_Matprod; /* options(matprod) */
extern HIDDEN size_t	R_WarnLength;	/* Error/warning max length */
extern HIDDEN int	R_nwarnings;
extern uintptr_t R_CStackLimit;	/* C stack limit */
extern uintptr_t R_OldCStackLimit; /* Old value while
							   handling overflow */
extern uintptr_t R_CStackStart;	/* Initial stack address */
extern int	R_CStackDir;	/* C stack direction */

//#ifdef R_USE_SIGNALS
//extern0 struct RPRSTACK *R_PendingPromises INI_as(NULL); /* Pending promise stack */
//#endif

/* File Input/Output */
LibExtern Rboolean R_Interactive;	/* TRUE during interactive use*/
extern HIDDEN Rboolean R_Quiet;	/* Be as quiet as possible */
extern Rboolean  R_Slave;	/* Run as a slave process */
extern HIDDEN Rboolean R_Verbose;	/* Be verbose */
/* extern int	R_Console; */	    /* Console active flag */
/* IoBuffer R_ConsoleIob; : --> ./IOStuff.h */
/* R_Consolefile is used in the internet module */
extern FILE*	R_Consolefile;	/* Console output file */
extern FILE*	R_Outputfile;	/* Output file */
extern HIDDEN int	R_ErrorCon;	/* Error connection */
LibExtern char *R_TempDir;	/* Name of per-session dir */
extern HIDDEN char   *Sys_TempDir;	/* Name of per-session dir
						   if set by R itself */
extern HIDDEN char	R_StdinEnc[31];	/* Encoding assumed for stdin */

/* Objects Used In Parsing  */
LibExtern int	R_ParseError; /* Line where parse error occurred */
extern HIDDEN int	R_ParseErrorCol;    /* Column of start of token where parse error occurred */
extern HIDDEN SEXP	R_ParseErrorFile;   /* Source file where parse error was seen.  Either a
				       STRSXP or (when keeping srcrefs) a SrcFile ENVSXP */
#define PARSE_ERROR_SIZE 256	    /* Parse error messages saved here */
LibExtern char	R_ParseErrorMsg[PARSE_ERROR_SIZE];
#define PARSE_CONTEXT_SIZE 256	    /* Recent parse context kept in a circular buffer */
LibExtern char	R_ParseContext[PARSE_CONTEXT_SIZE];
LibExtern int	R_ParseContextLast; /* last character in context buffer */
LibExtern int	R_ParseContextLine; /* Line in file of the above */

/* Image Dump/Restore */
extern int	R_DirtyImage;	/* Current image dirty */

/* History */
LibExtern char *R_HistoryFile;	/* Name of the history file */
LibExtern int	R_HistorySize;	/* Size of the history file */
LibExtern int	R_RestoreHistory;	/* restore the history file? */
extern void 	R_setupHistory(void);

/* Warnings/Errors */
extern HIDDEN int	R_CollectWarnings;	/* the number of warnings */
#ifdef __cplusplus
  extern rho::GCRoot<rho::ListVector> R_Warnings;  /* the warnings and their calls */
#endif
extern HIDDEN int	R_ShowErrorMessages;	/* show error messages? */
//extern0 SEXP	R_HandlerStack;	/* Condition handler stack */
//extern0 SEXP	R_RestartStack;	/* Stack of available restarts */
//extern0 Rboolean R_warn_partial_match_args   INI_as(FALSE);
extern HIDDEN Rboolean R_warn_partial_match_dollar;
extern HIDDEN Rboolean R_warn_partial_match_attr;
extern HIDDEN Rboolean R_ShowWarnCalls;
extern HIDDEN Rboolean R_ShowErrorCalls;
extern HIDDEN int	R_NShowCalls;

LibExtern Rboolean utf8locale;  /* is this a UTF-8 locale? */
LibExtern Rboolean mbcslocale;  /* is this a MBCS locale? */
extern HIDDEN   Rboolean latin1locale; /* is this a Latin-1 locale? */
#ifdef Win32
LibExtern unsigned int localeCP; /* the locale's codepage */
extern HIDDEN   Rboolean WinUTF8out;  /* Use UTF-8 for output */
extern HIDDEN   void WinCheckUTF8(void);
#endif

extern const char* OutDec;  /* decimal point used for output */
extern HIDDEN Rboolean R_DisableNLinBrowser;
extern HIDDEN char R_BrowserLastCommand;

/* Initialization of the R environment when it is embedded */
extern int Rf_initEmbeddedR(int argc, char** argv);

/* GUI type */

extern const char* R_GUIType;
extern Rboolean R_isForkedChild; /* was this forked? */

extern HIDDEN double cpuLimit;
extern HIDDEN double cpuLimit2;
extern HIDDEN double cpuLimitValue;
extern HIDDEN double elapsedLimit;
extern HIDDEN double elapsedLimit2;
extern HIDDEN double elapsedLimitValue;

void resetTimeLimits(void);

//#define R_BCNODESTACKSIZE 200000
//extern0 R_bcstack_t *R_BCNodeStackBase, *R_BCNodeStackTop, *R_BCNodeStackEnd;
//#ifdef BC_INT_STACK
//# define R_BCINTSTACKSIZE 10000
//extern0 IStackval *R_BCIntStackBase, *R_BCIntStackTop, *R_BCIntStackEnd;
//#endif
extern HIDDEN int R_jit_enabled;
extern HIDDEN int R_compile_pkgs;
extern HIDDEN int R_check_constants;
//extern0 int R_disable_bytecode INI_as(0);
//extern SEXP R_cmpfun1(SEXP); /* unconditional fresh compilation */
extern void R_init_jit_enabled(void);
extern void R_initAssignSymbols(void);
#if 0
#ifdef R_USE_SIGNALS
extern SEXP R_findBCInterpreterSrcref(RCNTXT*);
#endif
extern SEXP R_getCurrentSrcref();
extern SEXP R_getBCInterpreterExpression();

LibExtern SEXP R_CachedScalarReal INI_as(NULL);
LibExtern SEXP R_CachedScalarInteger INI_as(NULL);
#endif
LibExtern int R_num_math_threads;
LibExtern int R_max_num_math_threads;

/* Pointer  type and utilities for dispatch in the methods package */
typedef SEXP (*R_stdGen_ptr_t)(SEXP, SEXP, SEXP); /* typedef */
//R_stdGen_ptr_t R_get_standardGeneric_ptr(void); /* get method */
R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t, SEXP); /* set method */
LibExtern SEXP R_MethodsNamespace;
SEXP R_deferred_default_method(void);
SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef, SEXP mlist);
SEXP do_set_prim_method(SEXP op, const char* code_string, SEXP fundef, SEXP mlist);
void R_set_quick_method_check(R_stdGen_ptr_t);
SEXP R_primitive_methods(SEXP op);
SEXP R_primitive_generic(SEXP op);

/* smallest decimal exponent, needed in format.cpp, set in Init_R_Machine */
extern HIDDEN int R_dec_min_exponent;

/* structure for caching machine accuracy values */
typedef struct {
    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
    double eps, epsneg, xmin, xmax;
} AccuracyInfo;

LibExtern AccuracyInfo R_AccuracyInfo;

extern unsigned int max_contour_segments;

/* used in package utils */
extern Rboolean known_to_be_latin1;
extern HIDDEN Rboolean known_to_be_utf8;

/* pre-allocated boolean values */
LibExtern SEXP R_TrueValue;
LibExtern SEXP R_FalseValue;
LibExtern SEXP R_LogicalNAValue;

/* for PCRE as from R 3.4.0 */
extern HIDDEN Rboolean R_PCRE_use_JIT;
extern HIDDEN int R_PCRE_study;
extern HIDDEN int R_PCRE_limit_recursion;


#if 0
#ifdef __MAIN__
# undef extern
# undef extern0
# undef LibExtern
#endif
#undef INI_as
#endif

#define checkArity(a, b) Rf_checkArityCall(a, b, call)

/*--- FUNCTIONS ------------------------------------------------------ */

/* These Rf_ macros are retained for backwards compatibility, but
 * their use is deprecated within rho.  In particular rho's own
 * header files should always use the Rf_ prefix explicitly, and not
 * rely on these macros to paste it in.
 */

#if 0
# define allocCharsxp		Rf_allocCharsxp
//# define asVecSize		Rf_asVecSize
# define begincontext		Rf_begincontext
//# define BindDomain		Rf_BindDomain
# define check_stack_balance	Rf_check_stack_balance
# define check1arg		Rf_check1arg
# define CheckFormals		Rf_CheckFormals
# define CleanEd		Rf_CleanEd
# define CoercionWarning       	Rf_CoercionWarning
# define ComplexFromInteger	Rf_ComplexFromInteger
# define ComplexFromLogical	Rf_ComplexFromLogical
# define ComplexFromReal	Rf_ComplexFromReal
# define ComplexFromString	Rf_ComplexFromString
# define copyMostAttribNoTs	Rf_copyMostAttribNoTs
# define createS3Vars		Rf_createS3Vars
# define currentTime		Rf_currentTime
# define CustomPrintValue	Rf_CustomPrintValue
# define DataFrameClass		Rf_DataFrameClass
# define ddfindVar		Rf_ddfindVar
# define deparse1		Rf_deparse1
# define deparse1m		Rf_deparse1m
# define deparse1w		Rf_deparse1w
# define deparse1line		Rf_deparse1line
# define deparse1s		Rf_deparse1s
# define DispatchGroup		Rf_DispatchGroup
# define DispatchOrEval		Rf_DispatchOrEval
# define DispatchAnyOrEval      Rf_DispatchAnyOrEval
# define dynamicfindVar		Rf_dynamicfindVar
# define EncodeChar             Rf_EncodeChar
# define EncodeRaw              Rf_EncodeRaw
# define EncodeReal2            Rf_EncodeReal2
# define EncodeString           Rf_EncodeString
# define EnsureString 		Rf_EnsureString
# define endcontext		Rf_endcontext
//# define errorcall_cpy		Rf_errorcall_cpy
# define ErrorMessage		Rf_ErrorMessage
# define evalList		Rf_evalList
# define evalListKeepMissing	Rf_evalListKeepMissing
# define factorsConform		Rf_factorsConform
# define findcontext		Rf_findcontext
# define findVar1		Rf_findVar1
# define FrameClassFix		Rf_FrameClassFix
# define framedepth		Rf_framedepth
# define frameSubscript		Rf_frameSubscript
# define get1index		Rf_get1index
# define GetOptionCutoff       	Rf_GetOptionCutoff
# define getVar			Rf_getVar
# define getVarInFrame		Rf_getVarInFrame
# define InitArithmetic		Rf_InitArithmetic
# define InitConnections	Rf_InitConnections
# define InitEd			Rf_InitEd
# define InitFunctionHashing	Rf_InitFunctionHashing
# define InitBaseEnv		Rf_InitBaseEnv
# define InitGlobalEnv		Rf_InitGlobalEnv
# define InitGraphics		Rf_InitGraphics
# define InitMemory		Rf_InitMemory
# define InitNames		Rf_InitNames
# define InitOptions		Rf_InitOptions
# define InitStringHash		Rf_InitStringHash
# define InitS3DefaultTypes	Rf_InitS3DefaultTypes
# define InitTempDir		Rf_InitTempDir
# define InitTypeTables		Rf_InitTypeTables
# define initStack		Rf_initStack
# define IntegerFromComplex	Rf_IntegerFromComplex
# define IntegerFromLogical	Rf_IntegerFromLogical
# define IntegerFromReal	Rf_IntegerFromReal
# define IntegerFromString	Rf_IntegerFromString
# define internalTypeCheck	Rf_internalTypeCheck
# define isValidName		Rf_isValidName
# define installTrChar		Rf_installTrChar
# define ItemName		Rf_ItemName
# define jump_to_toplevel	Rf_jump_to_toplevel
# define KillAllDevices		Rf_KillAllDevices
# define levelsgets		Rf_levelsgets
# define LogicalFromComplex	Rf_LogicalFromComplex
# define LogicalFromInteger	Rf_LogicalFromInteger
# define LogicalFromReal	Rf_LogicalFromReal
# define LogicalFromString	Rf_LogicalFromString
# define mainloop		Rf_mainloop
# define makeSubscript		Rf_makeSubscript
# define markKnown		Rf_markKnown
# define mat2indsub		Rf_mat2indsub
# define matchArg		Rf_matchArg
# define matchArgExact		Rf_matchArgExact
# define matchArgs		Rf_matchArgs
# define matchArgs_RC		Rf_matchArgs_RC
# define matchPar		Rf_matchPar
# define Mbrtowc		Rf_mbrtowc
# define mbtoucs		Rf_mbtoucs
//# define mbcsToUcs2		Rf_mbcsToUcs2
# define memtrace_report	Rf_memtrace_report
# define mkCLOSXP		Rf_mkCLOSXP
# define mkFalse		Rf_mkFalse
# define mkPROMISE		Rf_mkPROMISE
# define mkQUOTE		Rf_mkQUOTE
# define mkSYMSXP		Rf_mkSYMSXP
# define mkTrue			Rf_mkTrue
# define NewEnvironment		Rf_NewEnvironment
# define OneIndex		Rf_OneIndex
# define onintr			Rf_onintr
# define onintrNoResume		Rf_onintrNoResume
# define onsigusr1              Rf_onsigusr1
# define onsigusr2              Rf_onsigusr2
# define parse			Rf_parse
# define patchArgsByActuals	Rf_patchArgsByActuals
# define PrintDefaults		Rf_PrintDefaults
# define PrintGreeting		Rf_PrintGreeting
# define PrintValueEnv		Rf_PrintValueEnv
# define PrintValueRec		Rf_PrintValueRec
# define PrintVersion		Rf_PrintVersion
# define PrintVersion_part_1	Rf_PrintVersion_part_1
# define PrintVersionString    	Rf_PrintVersionString
# define PrintRhoVersionString  Rf_PrintRhoVersionString
# define PrintWarnings		Rf_PrintWarnings
# define promiseArgs		Rf_promiseArgs
# define RealFromComplex	Rf_RealFromComplex
# define RealFromInteger	Rf_RealFromInteger
# define RealFromLogical	Rf_RealFromLogical
# define RealFromString		Rf_RealFromString
# define Seql			Rf_Seql
# define sexptype2char		Rf_sexptype2char
//# define Scollate		Rf_Scollate
# define sortVector		Rf_sortVector
# define SrcrefPrompt		Rf_SrcrefPrompt
# define ssort			Rf_ssort
# define StringFromComplex	Rf_StringFromComplex
# define StringFromInteger	Rf_StringFromInteger
# define StringFromLogical	Rf_StringFromLogical
# define StringFromReal		Rf_StringFromReal
# define strIsASCII		Rf_strIsASCII
# define StrToInternal		Rf_StrToInternal
# define strmat2intmat		Rf_strmat2intmat
# define substituteList		Rf_substituteList
//# define TimeToSeed		Rf_TimeToSeed
# define tspgets		Rf_tspgets
# define type2symbol		Rf_type2symbol
# define unbindVar		Rf_unbindVar
# define usemethod		Rf_usemethod
# define ucstomb		Rf_ucstomb
# define ucstoutf8		Rf_ucstoutf8
#ifdef ADJUST_ENVIR_REFCNTS
# define unpromiseArgs		Rf_unpromiseArgs
#endif
# define utf8toucs		Rf_utf8toucs
# define utf8towcs		Rf_utf8towcs
//# define vectorIndex		Rf_vectorIndex
# define warningcall		Rf_warningcall
//# define WarningMessage		Rf_WarningMessage
# define wcstoutf8		Rf_wcstoutf8
//# define wtransChar		Rf_wtransChar
//# define yychar			Rf_yychar
//# define yylval			Rf_yylval
//# define yynerrs		Rf_yynerrs
//# define yyparse		Rf_yyparse
#endif /* R_NO_REMAP */

/* Platform Dependent Gui Hooks */
//enum GUIHooks { R_CONSOLE = 1, R_FILE = 2, R_TEXT = 3 };

/* The maximum length of input line which will be asked for,
   in bytes, including the terminator */
#define CONSOLE_BUFFER_SIZE 4096
int R_ReadConsole(const char*, unsigned char*, size_t, int);
void R_WriteConsole(const char*, int); /* equivalent to R_WriteConsoleEx(a, b, 0) */
void R_WriteConsoleEx(const char*, int, int);
void R_ResetConsole(void);
void R_FlushConsole(void);
void R_ClearerrConsole(void);
void R_Busy(int);
int R_ShowFiles(int, const char**, const char**, const char*, Rboolean, const char*);
int R_EditFiles(int, const char**, const char**, const char*);
int R_ChooseFile(int, char*, size_t);
char* R_HomeDir(void);
#ifdef __cplusplus
bool R_FileExists(const char*);
bool R_HiddenFile(const char*);
#endif
double R_FileMtime(const char*);
int R_GetFDLimit();
int R_EnsureFDLimit(int);

/* environment cell access */
// Used only by src/library/methods/src/methods_list_dispatch.cpp
#ifndef __cplusplus
/* In C code, R_varloc_t is an opaque pointer: */
typedef struct R_varloc_st *R_varloc_t;
#endif
R_varloc_t R_findVarLocInFrame(SEXP, SEXP);
//SEXP R_GetVarLocValue(R_varloc_t);
//SEXP R_GetVarLocSymbol(R_varloc_t);
Rboolean R_GetVarLocMISSING(R_varloc_t);
//void R_SetVarLocValue(R_varloc_t, SEXP);

/* deparse option bits: change do_dump if more are added */
enum DeparseOptionBits
{
    KEEPINTEGER = 1,
    QUOTEEXPRESSIONS = 2,
    SHOWATTRIBUTES = 4,
    USESOURCE = 8,
    WARNINCOMPLETE = 16,
    DELAYPROMISES = 32,
    KEEPNA = 64,
    S_COMPAT = 128,
    HEXNUMERIC = 256,
    DIGITS16 = 512,
    NICE_NAMES = 1024,
    /* common combinations of the above */
    SIMPLEDEPARSE = 0,
    DEFAULTDEPARSE = 1089, /* KEEPINTEGER | KEEPNA | NICE_NAMES, used for calls */
    FORSOURCING = 95       /* not DELAYPROMISES, used in edit.cpp */
};

/* Coercion functions */
int Rf_LogicalFromString(SEXP, int*);
int Rf_IntegerFromString(SEXP, int*);
double Rf_RealFromString(SEXP, int*);
Rcomplex Rf_ComplexFromString(SEXP, int*);
SEXP Rf_StringFromLogical(int, int*);
SEXP Rf_StringFromInteger(int, int*);
SEXP Rf_StringFromReal(double, int*);
SEXP Rf_StringFromComplex(Rcomplex, int*);
SEXP Rf_EnsureString(SEXP);

/* Other Internally Used Functions */

// SEXP Rf_allocCharsxp(R_len_t);
// SEXP Rf_append(SEXP, SEXP); /* apparently unused now */
R_xlen_t asVecSize(SEXP x);
void Rf_check1arg(SEXP args, SEXP call, const char*);
void Rf_checkArityCall(SEXP, SEXP, SEXP);
// void CheckFormals(SEXP);
void R_check_locale(void);
// void check_stack_balance(SEXP op, int save);
void Rf_CleanEd(void);
void Rf_copyListMatrix(SEXP, SEXP, Rboolean);
void Rf_copyMostAttribNoTs(SEXP, SEXP);
// SEXP createS3Vars(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
void Rf_CustomPrintValue(SEXP, SEXP);
double Rf_currentTime(void);
void DataFrameClass(SEXP);
SEXP Rf_ddfindVar(SEXP, SEXP);
SEXP Rf_deparse1(SEXP, Rboolean, int);
SEXP Rf_deparse1m(SEXP call, Rboolean abbrev, int opts);
SEXP Rf_deparse1w(SEXP, Rboolean, int);
SEXP Rf_deparse1line(SEXP, Rboolean);
SEXP Rf_deparse1line_(SEXP, Rboolean, int);
SEXP Rf_deparse1s(SEXP call);
// int DispatchAnyOrEval(SEXP, SEXP, const char *, SEXP, SEXP, SEXP*, int, int);
// int DispatchOrEval(SEXP, SEXP, const char *, SEXP, SEXP, SEXP*, int, int);
int Rf_DispatchGroup(const char*, SEXP, SEXP, SEXP, SEXP, SEXP*);
// R_xlen_t dispatch_xlength(SEXP, SEXP, SEXP);
// R_len_t dispatch_length(SEXP, SEXP, SEXP);
SEXP dispatch_subset2(SEXP, R_xlen_t, SEXP, SEXP);
SEXP Rf_duplicated(SEXP, Rboolean);
R_xlen_t Rf_any_duplicated(SEXP, Rboolean);
R_xlen_t Rf_any_duplicated3(SEXP, SEXP, Rboolean);
// SEXP evalList(SEXP, SEXP, SEXP, int);
// SEXP evalListKeepMissing(SEXP, SEXP);
// int factorsConform(SEXP, SEXP);
// NORET void findcontext(int, SEXP, SEXP);
SEXP Rf_findVar1(SEXP, SEXP, SEXPTYPE, int);
// void FrameClassFix(SEXP);
// SEXP frameSubscript(int, SEXP, SEXP);
R_xlen_t Rf_get1index(SEXP, SEXP, R_xlen_t, int, int, SEXP);
int Rf_GetOptionCutoff(void);
// SEXP getVar(SEXP, SEXP);
// SEXP getVarInFrame(SEXP, SEXP);
void Rf_InitArithmetic(void);
void Rf_InitConnections(void);
void Rf_InitEd(void);
void Rf_InitFunctionHashing(void);
// void InitBaseEnv(void);
void Rf_InitGlobalEnv(void);
Rboolean R_current_trace_state(void);
Rboolean R_current_debug_state(void);
void R_InitialData(void);
Rboolean inherits2(SEXP, const char*);

#ifdef __cplusplus
}  //extern "C"
#endif

#ifdef __cplusplus
bool R_has_methods(const rho::BuiltInFunction* func);

std::pair<bool, SEXP>
R_possible_dispatch(const rho::Expression* call, const rho::BuiltInFunction* op,
		    const rho::ArgList&, rho::Environment* env);
std::pair<bool, SEXP>
Rf_DispatchOrEval(const rho::Expression* call, const rho::BuiltInFunction* op,
                  rho::ArgList* args, rho::Environment* env,
                  rho::MissingArgHandling dropmissing);
// Like DispatchOrEval, but args have already been evaluated.
std::pair<bool, SEXP>
Rf_Dispatch(const rho::Expression* call, const rho::BuiltInFunction* op,
            const rho::ArgList& args, rho::Environment* env);
std::pair<bool, SEXP>
Rf_DispatchGroup(const char *group, const rho::Expression* call,
                 const rho::BuiltInFunction* op,
                 rho::ArgList&& args, rho::Environment* env);
std::pair<bool, SEXP> Rf_usemethod(const char* generic,
                                   const rho::RObject* object,
                                   const rho::Expression* call,
                                   rho::Environment* env,
                                   rho::Environment* callenv,
                                   rho::Environment* defenv);
R_xlen_t dispatch_xlength(rho::RObject* object,
                          const rho::Expression* call,
                          rho::Environment* rho);
#endif


#ifdef __cplusplus
extern "C" {
#endif

void Rf_InitGraphics(void);
void Rf_InitMemory(void);
void Rf_InitNames(void);
void Rf_InitOptions(void);
// void InitStringHash(void);
void Init_R_Variables(SEXP);
void Rf_InitTempDir(void);
void R_reInitTempDir(int);
void Rf_InitTypeTables(void);
// void initStack(void);
void Rf_InitS3DefaultTypes(void);
void Rf_internalTypeCheck(SEXP, SEXP, SEXPTYPE);
Rboolean isMethodsDispatchOn(void);
int Rf_isValidName(const char*);
// NORET void jump_to_toplevel(void);
void Rf_KillAllDevices(void);
// SEXP levelsgets(SEXP, SEXP);
void Rf_mainloop(void);
SEXP Rf_makeSubscript(SEXP, SEXP, R_xlen_t*, SEXP);
SEXP Rf_markKnown(const char*, SEXP);
SEXP Rf_mat2indsub(SEXP, SEXP, SEXP);
// SEXP matchArg(SEXP, SEXP*);
SEXP Rf_matchArgExact(SEXP, SEXP*);
// SEXP matchArgs(SEXP, SEXP, SEXP);
// SEXP matchArgs_RC(SEXP, SEXP, SEXP);
// SEXP matchPar(const char *, SEXP*);
// void memtrace_report(void *, void *);
SEXP Rf_mkCLOSXP(SEXP, SEXP, SEXP);
SEXP Rf_mkFalse(void);
// SEXP mkPRIMSXP (int, int);
SEXP Rf_mkPROMISE(SEXP, SEXP);
// SEXP R_mkEVPROMISE(SEXP, SEXP);
// SEXP R_mkEVPROMISE_NR(SEXP, SEXP);
// SEXP mkQUOTE(SEXP);
// SEXP mkSYMSXP(SEXP, SEXP);
SEXP Rf_mkTrue(void);
const char* R_nativeEncoding(void);
SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
void Rf_onintr(void);
void Rf_onintrNoResume(void);
RETSIGTYPE Rf_onsigusr1(int);
RETSIGTYPE Rf_onsigusr2(int);
R_xlen_t Rf_OneIndex(SEXP, SEXP, R_xlen_t, int, SEXP*, int, SEXP);
// SEXP parse(FILE*, int);
// SEXP patchArgsByActuals(SEXP, SEXP, SEXP);
void Rf_PrintDefaults(void);
void Rf_PrintGreeting(void);
void Rf_PrintValueEnv(SEXP, SEXP);
void Rf_PrintValueRec(SEXP, SEXP);
void Rf_PrintVersion(char*, size_t len);
void PrintVersion_part_1(char*, size_t len);
void Rf_PrintVersionString(char*, size_t len);
void Rf_PrintRhoVersionString(char*, size_t len);
void Rf_PrintWarnings(const char* hdr);
void process_site_Renviron(void);
void process_system_Renviron(void);
void process_user_Renviron(void);
// SEXP promiseArgs(SEXP, SEXP);
void Rcons_vprintf(const char*, va_list);
SEXP R_data_class(SEXP, Rboolean);
SEXP R_data_class2(SEXP);
char* R_LibraryFileName(const char*, char*, size_t);
SEXP R_LoadFromFile(FILE*, int);
SEXP R_NewHashedEnv(SEXP, SEXP);
extern int R_Newhashpjw(const char*);
FILE* R_OpenLibraryFile(const char*);
SEXP R_Primitive(const char*);
void R_RestoreGlobalEnv(void);
void R_RestoreGlobalEnvFromFile(const char*, Rboolean);
void R_SaveGlobalEnv(void);
void R_SaveGlobalEnvToFile(const char*);
void R_SaveToFile(SEXP, FILE*, int);
void R_SaveToFileV(SEXP, FILE*, int, int);
Rboolean R_seemsOldStyleS4Object(SEXP object);
int R_SetOptionWarn(int);
int R_SetOptionWidth(int);
void R_Suicide(const char*);
void R_getProcTime(double* data);
int R_isMissing(SEXP symbol, SEXP rho);
const char* sexptype2char(SEXPTYPE type);
void Rf_sortVector(SEXP, Rboolean);
void Rf_SrcrefPrompt(const char*, SEXP);
#ifdef __cplusplus
void Rf_ssort(rho::StringVector*, int);
#endif
SEXP Rf_strmat2intmat(SEXP, SEXP, SEXP);
SEXP Rf_substituteList(SEXP, SEXP);
unsigned int TimeToSeed(void);
SEXP Rf_tspgets(SEXP, SEXP);
SEXP Rf_type2symbol(SEXPTYPE);
void Rf_unbindVar(SEXP, SEXP);
#ifdef ALLOW_OLD_SAVE
void unmarkPhase(void);
#endif
#ifdef ADJUST_ENVIR_REFCNTS
void unpromiseArgs(SEXP);
#endif
SEXP R_LookupMethod(SEXP, SEXP, SEXP, SEXP);
// int usemethod(const char *, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP*);
SEXP vectorIndex(SEXP, SEXP, int, int, int, SEXP, Rboolean);

#if 0 //ifdef R_USE_SIGNALS
void begincontext(RCNTXT*, int, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP dynamicfindVar(SEXP, RCNTXT*);
void endcontext(RCNTXT*);
int framedepth(RCNTXT*);
void R_InsertRestartHandlers(RCNTXT *, const char *);
NORET void R_JumpToContext(RCNTXT *, int, SEXP);
SEXP R_syscall(int,RCNTXT*);
int R_sysparent(int,RCNTXT*);
SEXP R_sysframe(int,RCNTXT*);
SEXP R_sysfunction(int,RCNTXT*);

void R_run_onexits(RCNTXT *);
NORET void R_jumpctxt(RCNTXT *, int, SEXP);
#endif

/* ../main/bind.cpp */
SEXP Rf_ItemName(SEXP, R_xlen_t);

/* ../main/errors.cpp : */
NORET void errorcall_cpy(SEXP, const char*, ...);
NORET void ErrorMessage(SEXP, int, ...);
void WarningMessage(SEXP, int /* R_WARNING */, ...);
SEXP R_GetTraceback(int);

R_size_t R_GetMaxVSize(void);
void R_SetMaxVSize(R_size_t);
R_size_t R_GetMaxNSize(void);
void R_SetMaxNSize(R_size_t);
R_size_t R_Decode2Long(char* p, int* ierr);
void R_SetPPSize(R_size_t);

/* ../../main/printutils.cpp : */
typedef enum
{
    Rprt_adj_left = 0,
    Rprt_adj_right = 1,
    Rprt_adj_centre = 2,
    Rprt_adj_none = 3
} Rprt_adj;

int Rstrlen(SEXP, int);
const char* Rf_EncodeRaw(Rbyte, const char*);
const char* Rf_EncodeString(SEXP, int, int, Rprt_adj);
const char* Rf_EncodeReal2(double, int, int, int);
const char* Rf_EncodeChar(SEXP);

/* main/sort.cpp */
void orderVector1(int* indx, int n, SEXP key, Rboolean nalast, Rboolean decreasing, SEXP rho);

/* main/subset.cpp */
SEXP R_subset3_dflt(SEXP, SEXP, SEXP);

/* main/subassign.cpp */
SEXP R_subassign3_dflt(SEXP, SEXP, SEXP, SEXP);

#include <wchar.h>

/* main/util.cpp */
NORET void UNIMPLEMENTED_TYPE(const char* s, const SEXP x);
NORET void UNIMPLEMENTED_TYPEt(const char* s, const SEXPTYPE t);
Rboolean Rf_strIsASCII(const char* str);
int utf8clen(char c);
int Rf_AdobeSymbol2ucs2(int n);
double R_strtod5(const char* str, char** endptr, char dec, Rboolean NA, int exact);

typedef unsigned short ucs2_t;
size_t mbcsToUcs2(const char* in, ucs2_t* out, int nout, int enc);
/* size_t mbcsMblen(char *in);
size_t ucs2ToMbcs(ucs2_t *in, char *out);
size_t ucs2Mblen(ucs2_t *in); */
size_t Rf_utf8toucs(wchar_t* wc, const char* s);
size_t Rf_utf8towcs(wchar_t* wc, const char* s, size_t n);
size_t Rf_ucstomb(char* s, const unsigned int wc);
size_t Rf_ucstoutf8(char* s, const unsigned int wc);
size_t Rf_mbtoucs(unsigned int* wc, const char* s, size_t n);
size_t Rf_wcstoutf8(char* s, const wchar_t* wc, size_t n);

SEXP Rf_installTrChar(SEXP);

const wchar_t* Rf_wtransChar(SEXP x); /* from sysutils.cpp */

#define mbs_init(x) memset(x, 0, sizeof(mbstate_t))
size_t Rf_mbrtowc(wchar_t* wc, const char* s, size_t n, mbstate_t* ps);
Rboolean mbcsValid(const char* str);
Rboolean utf8Valid(const char* str);
char* Rf_strchr(const char* s, int c);
char* Rf_strrchr(const char* s, int c);

SEXP fixup_NaRm(SEXP args); /* summary.cpp */
void invalidate_cached_recodings(void); /* from sysutils.cpp */
void resetICUcollator(void); /* from util.cpp */
void dt_invalidate_locale(); /* from Rstrptime.h */
extern int R_OutputCon; /* from connections.cpp */
extern int R_InitReadItemDepth, R_ReadItemDepth; /* from serialize.cpp */
void get_current_mem(size_t*, size_t*, size_t*); /* from memory.cpp */
unsigned long get_duplicate_counter(void); /* from duplicate.cpp */
void reset_duplicate_counter(void); /* from duplicate.cpp */
void BindDomain(char*); /* from main.cpp */
extern Rboolean LoadInitFile; /* from startup.cpp */

// Unix and Windows versions
double R_getClockIncrement(void);
void R_getProcTime(double* data);
void InitDynload(void);
void R_CleanTempDir(void);

#ifdef Win32
void R_fixslash(char* s);
void R_fixbackslash(char* s);
wchar_t* filenameToWchar(const SEXP fn, const Rboolean expand);

#if defined(SUPPORT_UTF8_WIN32)
#define mbrtowc(a, b, c, d) Rmbrtowc(a, b)
#define wcrtomb(a, b, c) Rwcrtomb(a, b)
#define mbstowcs(a, b, c) Rmbstowcs(a, b, c)
#define wcstombs(a, b, c) Rwcstombs(a, b, c)
size_t Rmbrtowc(wchar_t* wc, const char* s);
size_t Rwcrtomb(char* s, const wchar_t wc);
size_t Rmbstowcs(wchar_t* wc, const char* s, size_t n);
size_t Rwcstombs(char* s, const wchar_t* wc, size_t n);
#endif
#endif

FILE* RC_fopen(const SEXP fn, const char* mode, const Rboolean expand);
int Rf_Seql(SEXP a, SEXP b);
int Scollate(SEXP a, SEXP b);

double R_strtod4(const char* str, char** endptr, char dec, Rboolean NA);
double R_strtod(const char* str, char** endptr);
double R_atof(const char* str);

/* unix/sys-std.cpp, main/options.cpp */
void set_rl_word_breaks(const char* str);

/* From localecharset.cpp */
extern const char* locale2charset(const char*);

/*
   alloca is neither C99 nor POSIX.

   It might be better to try alloca.h first, see
   https://www.gnu.org/software/autoconf/manual/autoconf-2.60/html_node/Particular-Functions.html
*/
#ifdef __GNUC__
// This covers GNU, Clang and Intel compilers
// The undef is needed in case some other header, e.g. malloc.h, already did this
# undef alloca
# define alloca(x) __builtin_alloca((x))
#else
# ifdef HAVE_ALLOCA_H
// Needed for native compilers on Solaris and AIX
#  include <alloca.h>
# endif
// it might have been defined via some other standard header, e.g. stdlib.h
# if !HAVE_DECL_ALLOCA
#  include <stddef.h> // for size_t
extern void *alloca(size_t);
# endif
#endif

/* Required by C99, but might be slow */
#include <R_ext/Ldouble.h>

/* int_fast64_t is required by C99/C11
   Alternative would be to use intmax_t.
 */
#ifdef HAVE_INT64_T
# define LONG_INT int64_t
# define LONG_INT_MAX INT64_MAX
#elif defined(HAVE_INT_FAST64_T)
# define LONG_INT int_fast64_t
# define LONG_INT_MAX INT_FAST64_MAX
#endif

// for reproducibility for now: use exp10 or pown later if accurate enough.
#define Rexp10(x) pow(10.0, x)

/*
 * 2007/06/06 arr:
 * Function prototypes that don't appear to be defined anywhere else:
 */

Rboolean R_access_X11(void); /* from src/unix/X11.cpp */
SEXP R_execMethod(SEXP op, SEXP rho);
double R_getClockIncrement();
SEXP do_gpregexpr(SEXP pat, SEXP vec, int igcase_opt, int useBytes);
SEXP do_pgsub(SEXP pat, SEXP rep, SEXP vec,
	      int global, int igcase_opt, int useBytes);

#ifdef __cplusplus
} //extern "C"
#endif
#endif /* DEFN_H_ */
/*
 *- Local Variables:
 *- page-delimiter: "^/\\*---"
 *- End:
 */
