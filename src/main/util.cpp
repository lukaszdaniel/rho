/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2018  The R Core Team
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

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <R_ext/Print.h>
#include "basedecl.h"

#include <vector>
#include "rho/BuiltInFunction.hpp"
#include "rho/ClosureContext.hpp"
#include "rho/RAllocStack.hpp"

#include <ctype.h>		/* for isspace */
#include <float.h>		/* for DBL_MAX */

using namespace std;
using namespace rho;

#undef COMPILING_R

#include <Print.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef Win32
void R_UTF8fixslash(char *s);
static void R_wfixslash(wchar_t *s);
#endif

#ifdef __cplusplus
extern "C" {
#endif
void F77_SYMBOL(rwarnc)(char *msg, int *nchar);
NORET void F77_SYMBOL(rexitc)(char *msg, int *nchar);

#ifdef __cplusplus
} //extern "C"
#endif

#include <rlocale.h>

/* Many small functions are included from ../include/Rinlinedfuns.h */

int Rf_nrows(SEXP s)
{
    SEXP t;
    if (Rf_isVector(s) || Rf_isList(s)) {
	t = Rf_getAttrib(s, Symbols::DimSymbol);
	if (!t) return LENGTH(s);
	return INTEGER(t)[0];
    }
    else if (Rf_isFrame(s)) {
	return Rf_nrows(CAR(s));
    }
    else Rf_error(_("object is not a matrix"));
    return -1;
}


int Rf_ncols(SEXP s)
{
    SEXP t;
    if (Rf_isVector(s) || Rf_isList(s)) {
	t = Rf_getAttrib(s, Symbols::DimSymbol);
	if (!t) return 1;
	if (LENGTH(t) >= 2) return INTEGER(t)[1];
	/* This is a 1D (or possibly 0D array) */
	return 1;
    }
    else if (Rf_isFrame(s)) {
	return Rf_length(s);
    }
    else Rf_error(_("object is not a matrix"));
    return -1;/*NOTREACHED*/
}

#ifdef UNUSED
const static char type_msg[] = "invalid type passed to internal function\n";

void Rf_internalTypeCheck(SEXP call, SEXP s, SEXPTYPE type)
{
    if (TYPEOF(s) != type) {
	if (call)
	    Rf_errorcall(call, type_msg);
	else
	    Rf_error(type_msg);
    }
}
#endif

SEXP Rf_asChar(SEXP x)
{
	if (Rf_isVectorAtomic(x) && XLENGTH(x) >= 1) {
	    int w, d, e, wi, di, ei;
	    char buf[MAXELTSIZE];  /* Probably 100 would suffice */

	    switch (TYPEOF(x)) {
	    case LGLSXP:
		if (LOGICAL(x)[0] == R_NaLog)
		    return R_NaString;
		if (LOGICAL(x)[0])
		    sprintf(buf, "TRUE");
		else
		    sprintf(buf, "FALSE");
		return Rf_mkChar(buf);
	    case INTSXP:
		if (INTEGER(x)[0] == R_NaInt)
		    return R_NaString;
		snprintf(buf, MAXELTSIZE, "%d", INTEGER(x)[0]);
		return Rf_mkChar(buf);
	    case REALSXP:
		Rf_PrintDefaults();
		formatReal(REAL(x), 1, &w, &d, &e, 0);
		return Rf_mkChar(EncodeReal0(REAL(x)[0], w, d, e, OutDec));
	    case CPLXSXP:
		Rf_PrintDefaults();
		formatComplex(COMPLEX(x), 1, &w, &d, &e, &wi, &di, &ei, 0);
		return Rf_mkChar(EncodeComplex(COMPLEX(x)[0], w, d, e, wi, di, ei, OutDec));
	    case STRSXP:
		return STRING_ELT(x, 0);
	    default:
		return R_NaString;
	    }
	} else if(TYPEOF(x) == CHARSXP) {
	    return x;
	} else if(TYPEOF(x) == SYMSXP)
	    return PRINTNAME(x);
    return R_NaString;
}

Rboolean Rf_isUnordered(SEXP s)
{
    return Rboolean(TYPEOF(s) == INTSXP
		    && Rf_inherits(s, "factor")
		    && !Rf_inherits(s, "ordered"));
}

Rboolean Rf_isOrdered(SEXP s)
{
    return Rboolean(TYPEOF(s) == INTSXP
		    && Rf_inherits(s, "factor")
		    && Rf_inherits(s, "ordered"));
}


const static struct TypeTable {
    const char * const str;
    const SEXPTYPE type;
}
TypeTable[] = {
    { "NULL",		NILSXP	   },  /* real types */
    { "symbol",		SYMSXP	   },
    { "pairlist",	LISTSXP	   },
    { "closure",	CLOSXP	   },
    { "environment",	ENVSXP	   },
    { "promise",	PROMSXP	   },
    { "language",	LANGSXP	   },
    { "special",	SPECIALSXP },
    { "builtin",	BUILTINSXP },
    { "char",		CHARSXP	   },
    { "logical",	LGLSXP	   },
    { "integer",	INTSXP	   },
    { "double",		REALSXP	   }, /*-  "real", for R <= 0.61.x */
    { "complex",	CPLXSXP	   },
    { "character",	STRSXP	   },
    { "...",		DOTSXP	   },
    { "any",		ANYSXP	   },
    { "expression",	EXPRSXP	   },
    { "list",		VECSXP	   },
    { "externalptr",	EXTPTRSXP  },
    { "bytecode",	BCODESXP   },
    { "weakref",	WEAKREFSXP },
    { "raw",		RAWSXP },
    { "S4",		S4SXP },
    { "RHO_extended",  CXXSXP },
    { "RHO_bailout",   BAILSXP },
    /* aliases : */
    { "numeric",	REALSXP	   },
    { "name",		SYMSXP	   },

    { nullptr,     SEXPTYPE(-1)         }
};


SEXPTYPE Rf_str2type(const char *s)
{
    for (int i = 0; TypeTable[i].str; i++) {
	if (streql(s, TypeTable[i].str))
	    return TypeTable[i].type;
    }
    return SEXPTYPE(-1);
}

static struct Type2Table {
    const char *cstrName;
    SEXP rcharName;
    SEXP rstrName;
    SEXP rsymName;
} Type2Table[MAX_NUM_SEXPTYPE];


static int findTypeInTypeTable(const SEXPTYPE t)
 {
    for (int i = 0; TypeTable[i].str; i++)
	if (TypeTable[i].type == t) return i;

    return -1;
}

// called from main.cpp
HIDDEN void Rf_InitTypeTables(void) {

    /* Type2Table */
    for (int type = 0; type < MAX_NUM_SEXPTYPE; type++) {
        int j = findTypeInTypeTable((SEXPTYPE)type);

	if (j != -1) {
	    const char *cstr = TypeTable[j].str;
	    SEXP rchar = PROTECT(Rf_mkChar(cstr));
	    SEXP rstr = Rf_ScalarString(rchar);
	    MARK_NOT_MUTABLE(rstr);
	    R_PreserveObject(rstr);
	    UNPROTECT(1); /* rchar */
	    SEXP rsym = rho::Symbol::obtain(cstr);

	    Type2Table[type].cstrName = cstr;
	    Type2Table[type].rcharName = rchar;
	    Type2Table[type].rstrName = rstr;
	    Type2Table[type].rsymName = rsym;
	} else {
	    Type2Table[type].cstrName = nullptr;
	    Type2Table[type].rcharName = nullptr;
	    Type2Table[type].rstrName = nullptr;
	    Type2Table[type].rsymName = nullptr;
	}
    }
}

SEXP Rf_type2str_nowarn(const SEXPTYPE t) /* returns a CHARSXP */
{
    if (t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
	SEXP res = Type2Table[t].rcharName;
	if (res) return res;
    }
    return nullptr;
}

SEXP Rf_type2str(const SEXPTYPE t) /* returns a CHARSXP */
{
    SEXP s = Rf_type2str_nowarn(t);
    if (s) return s;
    Rf_warning(_("type %d is unimplemented in '%s'"), t, "type2str");
    char buf[50];
    snprintf(buf, 50, "unknown type #%d", t);
    return Rf_mkChar(buf);
}

SEXP Rf_type2rstr(const SEXPTYPE t) /* returns a STRSXP */
{
    if (t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
	SEXP res = Type2Table[t].rstrName;
	if (res) return res;
    }
    Rf_error(_("type %d is unimplemented in '%s'"), t,
	  "type2ImmutableScalarString");
    return nullptr; /* for -Wall */
}

const char *Rf_type2char(const SEXPTYPE t) /* returns a char* */
{
    if (t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
	const char * res = Type2Table[t].cstrName;
	if (res) return res;
    }
    Rf_warning(_("type %d is unimplemented in '%s'"), t, "type2char");
    static char buf[50];
    snprintf(buf, 50, "unknown type #%d", t);
    return buf;
}

#ifdef UNUSED
NORET SEXP Rf_type2symbol(SEXPTYPE t)
{
    if (t >= 0 && t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
	SEXP res = Type2Table[t].rsymName;
	if (res != nullptr) {
	    return res;
	}
    }
    Rf_error(_("type %d is unimplemented in '%s'"), t, "type2symbol");
}
#endif

HIDDEN NORET void UNIMPLEMENTED_TYPEt(const char *s, const SEXPTYPE t)
{
    for (int i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == int(t))
	    Rf_error(_("unimplemented type '%s' in '%s'\n"), TypeTable[i].str, s);
    }
    Rf_error(_("unimplemented type (%d) in '%s'\n"), t, s);
}

NORET void UNIMPLEMENTED_TYPE(const char *s, const SEXP x)
{
    UNIMPLEMENTED_TYPEt(s, TYPEOF(x));
}

# include <R_ext/Riconv.h>
# include <sys/param.h>
# include <errno.h>


/* Previous versions of R (< 2.3.0) assumed wchar_t was in Unicode
   (and it commonly is).  These functions do not. */
# ifdef WORDS_BIGENDIAN
static const char UCS2ENC[] = "UCS-2BE";
# else
static const char UCS2ENC[] = "UCS-2LE";
# endif


/*
 * out=NULL returns the number of the MBCS chars
 */
/* Note: this does not terminate out, as all current uses are to look
 * at 'out' a wchar at a time, and sometimes just one char.
 */
size_t mbcsToUcs2(const char *in, ucs2_t *out, int nout, int enc)
{
    void   *cd = nullptr ;
    const char *i_buf;
    char *o_buf;
    size_t  i_len, o_len, status, wc_len;
    /* out length */
    wc_len = (enc == CE_UTF8)? Rf_utf8towcs(nullptr, in, 0) : mbstowcs(nullptr, in, 0);
    if (out == nullptr || int(wc_len) < 0) return wc_len;

    if (reinterpret_cast<void*>(-1) == (cd = Riconv_open(UCS2ENC, (enc == CE_UTF8) ? "UTF-8": "")))
	return size_t(-1);

    i_buf = in;
    i_len = strlen(in); /* not including terminator */
    o_buf = reinterpret_cast<char *>(out);
    o_len = (size_t(nout)) * sizeof(ucs2_t);
    status = Riconv(cd, &i_buf, &i_len, &o_buf, &o_len);
    int serrno = errno;
    Riconv_close(cd);
    if (status == size_t(-1)) {
	switch(serrno){
	case EINVAL:
	    return size_t(-2);
	case EILSEQ:
	    return size_t(-1);
	case E2BIG:
	    break;
	default:
	    errno = EILSEQ;
	    return size_t(-1);
	}
    }
    return wc_len; /* status would be better? */
}


#include <wctype.h>

/* This one is not in Rinternals.h, but is used in internet module */
Rboolean Rf_isBlankString(const char *s)
{
    if(mbcslocale) {
	wchar_t wc; size_t used; mbstate_t mb_st;
	mbs_init(&mb_st);
	while( (used = Rf_mbrtowc(&wc, s, MB_CUR_MAX, &mb_st)) ) {
	    if(!iswspace(wint_t( wc))) return FALSE;
	    s += used;
	}
    } else
	while (*s)
	    if (!isspace(int(*s++))) return FALSE;
    return TRUE;
}

Rboolean Rf_StringBlank(SEXP x)
{
    if (!x) return TRUE;
    return Rboolean(R_CHAR(x)[0] == '\0');
}

/* Function to test whether a string is a true value */

Rboolean Rf_StringTrue(const char* name)
{
    string str(name);
    vector<string> truenames { "T", "True", "TRUE", "true" };
    for (const string& word : truenames)
	if (str == word)
	    return TRUE;
    return FALSE;
}

Rboolean Rf_StringFalse(const char* name)
{
    vector<string> falsenames { "F", "False", "FALSE", "false" };
    string str(name);
    for (const string& word : falsenames)
	if (str == word)
	    return TRUE;
    return FALSE;
}

/* used in bind.cpp and options.cpp */
HIDDEN SEXP Rf_EnsureString(SEXP s)
{
    switch(TYPEOF(s)) {
    case SYMSXP:
	s = PRINTNAME(s);
	break;
    case STRSXP:
	s = STRING_ELT(s, 0);
	break;
    case CHARSXP:
	break;
    case NILSXP:
	s = R_BlankString;
	break;
    default:
	Rf_error(_("invalid tag in name extraction"));
    }
    return s;
}

void Rf_checkArityCall(SEXP op, SEXP args, SEXP call)
{
    BuiltInFunction* func = SEXP_downcast<BuiltInFunction*>(op);
    PairList* arglist = SEXP_downcast<PairList*>(args);
    Expression* callx = SEXP_downcast<Expression*>(call);
    func->checkNumArgs(listLength(arglist), callx);
}

HIDDEN void Rf_check1arg(SEXP arg, SEXP call,
				   const char *formal)
{
    SEXP tag = TAG(const_cast<RObject*>(arg));
    if (!tag) return;

    const char *supplied = R_CHAR(PRINTNAME(tag));
    if (strncmp(supplied, formal, strlen(supplied)) != 0)
	Rf_errorcall(const_cast<RObject*>(call),
		  _("supplied argument name '%s' does not match '%s'"),
		  supplied, formal);
}

HIDDEN void Expression::check1arg(const char *formal) const
{
    auto args = getArgs();
    if (!args) {
	Rf_errorcall(const_cast<Expression*>(this),
		     _((std::string("'") + formal + "' is missing").c_str()));
    }
    if (args->car() == DotsSymbol) {
	// In this case it's difficult to verify that the correct argument
	// name was used.  However, assuming that it was is mostly harmless,
	// so let it go.
	return;
    }
    return Rf_check1arg(const_cast<PairList*>(getArgs()),
			const_cast<Expression*>(this), formal);
}


SEXP Rf_nthcdr(SEXP s, int n)
{
    if (Rf_isList(s) || Rf_isLanguage(s) || Rf_isFrame(s) || TYPEOF(s) == DOTSXP ) {
	while( n-- > 0 ) {
	    if (!s)
		Rf_error(_("'nthcdr' list shorter than %d"), n);
	    s = CDR(s);
	}
	return s;
    }
    else Rf_error(_("'nthcdr' needs a list to CDR down"));
    return nullptr;/* for -Wall */
}


/* This is a primitive (with no arguments) */
HIDDEN SEXP do_nargs(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
{
    ClosureContext *cptr = R_GlobalContext();
    int nargs = R_NaInt;
    while (cptr && cptr->workingEnvironment() != rho)
	cptr = ClosureContext::innermost(cptr->nextOut());
    if (cptr)
	nargs = cptr->promiseArgs().size();
    return Rf_ScalarInteger(nargs);
}

template<typename T>
HIDDEN void setVector(T * vec, const int& len, const T& val) {
	for (int i = 0; i < len; i++) vec[i] = val;
}
/* formerly used in subscript.cpp, in Utils.h */
HIDDEN void Rf_setIVector(int * vec, int len, int val)
{
    setVector(vec, len, val);
}


/* unused in R, in Utils.h, apparently used in Rcpp  */
HIDDEN void Rf_setRVector(double * vec, int len, double val)
{
    setVector(vec, len, val);
}

/* unused in R, in Rinternals.h */
void Rf_setSVector(SEXP * vec, int len, SEXP val)
{
    setVector(vec, len, val);
}

/* Debugging functions (hence the d-prefix). */
/* These are intended to be called interactively from */
/* a debugger such as gdb, so you don't have to remember */
/* the names of the data structure components. */

int dtype(SEXP q)
{
    return(int(TYPEOF(q)));
}


SEXP dcar(SEXP l)
{
    return(CAR(l));
}


SEXP dcdr(SEXP l)
{
    return(CDR(l));
}

static void isort_with_index(int *x, int *indx, int n)
{
    int i, j, h, iv, v;

    for (h = 1; h <= n / 9; h = 3 * h + 1);
    for (; h > 0; h /= 3)
	for (i = h; i < n; i++) {
	    v = x[i]; iv = indx[i];
	    j = i;
	    while (j >= h && x[j - h] > v)
		 { x[j] = x[j - h]; indx[j] = indx[j-h]; j -= h; }
	    x[j] = v; indx[j] = iv;
	}
}


// body(x) without attributes "srcref", "srcfile", "wholeSrcref" :
// NOTE: Callers typically need  PROTECT(R_body_no_src(.))
SEXP R_body_no_src(SEXP x) {
    SEXP b = PROTECT(Rf_duplicate(BODY_EXPR(x)));
    /* R's removeSource() works *recursively* on the body()
       in  ../library/utils/R/sourceutils.R  but that seems unneeded (?) */
    Rf_setAttrib(b, Symbols::SrcrefSymbol, nullptr);
    Rf_setAttrib(b, Symbols::SrcfileSymbol, nullptr);
    Rf_setAttrib(b, Symbols::WholeSrcrefSymbol, nullptr);
    UNPROTECT(1);
    return b;
}

/* merge(xinds, yinds, all.x, all.y) */
/* xinds, yinds are along x and y rows matching into the (numeric)
   common indices, with 0 for non-matches.

   all.x and all.y are boolean.

   The return value is a list with 4 elements (xi, yi, x.alone, y.alone),
   which are index vectors for rows of x or y.
*/
HIDDEN SEXP do_merge(/*const*/ Expression* call, const BuiltInFunction* op, RObject* xinds_, RObject* yinds_, RObject* all_x_, RObject* all_y_)
{
    SEXP xi, yi, ansx, ansy, ans;
    int nx = 0, ny = 0, i, j, k, nx_lone = 0, ny_lone = 0;
    int all_x = 0, all_y = 0, ll = 0/* "= 0" : for -Wall */;
    int nnx, nny;

    xi = xinds_;
    // NB: long vectors are not supported for input
    if ( !Rf_isInteger(xi) || !(nx = LENGTH(xi)) )
	Rf_error(_("invalid '%s' argument"), "xinds");
    yi = yinds_;
    if ( !Rf_isInteger(yi) || !(ny = LENGTH(yi)) )
	Rf_error(_("invalid '%s' argument"), "yinds");
    if(!LENGTH(ans = all_x_) || R_NaLog == (all_x = Rf_asLogical(ans)))
	Rf_error(_("'all.x' must be TRUE or FALSE"));
    if(!LENGTH(ans = all_y_)|| R_NaLog == (all_y = Rf_asLogical(ans)))
	Rf_error(_("'all.y' must be TRUE or FALSE"));

    /* 0. sort the indices */
    int* ix = static_cast<int *>(RHO_alloc(size_t(nx), sizeof(int)));
    int* iy = static_cast<int *>(RHO_alloc(size_t(ny), sizeof(int)));
    for(i = 0; i < nx; i++) ix[i] = i+1;
    for(i = 0; i < ny; i++) iy[i] = i+1;
    isort_with_index(INTEGER(xi), ix, nx);
    isort_with_index(INTEGER(yi), iy, ny);

    /* 1. determine result sizes */
    for (i = 0; i < nx; i++)
	if (INTEGER(xi)[i] > 0)
	    break;
    nx_lone = i;
    for (i = 0; i < ny; i++)
	if (INTEGER(yi)[i] > 0)
	    break;
    ny_lone = i;
    double dnans = 0;
    for (i = nx_lone, j = ny_lone; i < nx; i = nnx, j = nny) {
	int tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	/* the next is not in theory necessary,
	   since we have the common values only */
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	/* printf("i %d nnx %d j %d nny %d\n", i, nnx, j, nny); */
	dnans += ((double)(nnx-i))*(nny-j);
    }
    if (dnans > double(R_XLEN_T_MAX))
	Rf_error(_("number of rows in the result exceeds maximum vector length"));
    R_xlen_t nans = (int) dnans;


    /* 2. allocate and store result components */

    const char *nms[] = {"xi", "yi", "x.alone", "y.alone", ""};
    ans = PROTECT(Rf_mkNamed(VECSXP, nms));
    ansx = Rf_allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 0, ansx);
    ansy = Rf_allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 1, ansy);

    if(all_x) {
	SEXP x_lone = Rf_allocVector(INTSXP, nx_lone);
	SET_VECTOR_ELT(ans, 2, x_lone);
	for (i = 0, ll = 0; i < nx_lone; i++)
	    INTEGER(x_lone)[ll++] = ix[i];
    }

    if(all_y) {
	SEXP y_lone = Rf_allocVector(INTSXP, ny_lone);
	SET_VECTOR_ELT(ans, 3, y_lone);
	for (i = 0, ll = 0; i < ny_lone; i++)
	    INTEGER(y_lone)[ll++] = iy[i];
    }

    for (i = nx_lone, j = ny_lone, k = 0; i < nx; i = nnx, j = nny) {
	int tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	for(int i0 = i; i0 < nnx; i0++)
	    for(int j0 = j; j0 < nny; j0++) {
		INTEGER(ansx)[k]   = ix[i0];
		INTEGER(ansy)[k++] = iy[j0];
	    }
    }

    UNPROTECT(1);
    return ans;
}


/* Functions for getting and setting the working directory. */
#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>
#endif

SEXP static intern_getwd(void)
{
    SEXP rval = nullptr;
    char buf[4*PATH_MAX+1];

#ifdef Win32
    {
	wchar_t wbuf[PATH_MAX+1];
	int res = GetCurrentDirectoryW(PATH_MAX, wbuf);
	if(res > 0) {
	    Rf_wcstoutf8(buf, wbuf, sizeof(buf));
	    R_UTF8fixslash(buf);
	    return Rf_ScalarString(Rf_mkCharCE(buf, CE_UTF8));
	}
    }
#else
    char *res = getcwd(buf, PATH_MAX); /* can return NULL */
    if(res) rval = Rf_mkString(buf);
#endif
    return(rval);
}

HIDDEN SEXP do_getwd(/*const*/ Expression* call, const BuiltInFunction* op)
{
    return(intern_getwd());
}


#if defined(Win32) && defined(_MSC_VER)
# include <direct.h> /* for chdir, via io.h */
#endif

HIDDEN SEXP do_setwd(/*const*/ Expression* call, const BuiltInFunction* op, RObject* dir)
{
    if (!Rf_isValidString(dir))
	Rf_error(_("character argument expected"));
    if (STRING_ELT(dir, 0) == R_NaString)
	Rf_error(_("missing value is invalid"));

    /* get current directory to return */
    SEXP wd = PROTECT(intern_getwd());

#ifdef Win32
    {
	const wchar_t *path = filenameToWchar(STRING_ELT(dir, 0), TRUE);
	if(_wchdir(path) < 0)
	    Rf_error(_("cannot change working directory"));
    }
#else
    {
	const char *path
	    = R_ExpandFileName(Rf_translateChar(STRING_ELT(dir, 0)));
    if(chdir(path) < 0)
	Rf_error(_("cannot change working directory"));
    }
#endif
    UNPROTECT(1); /* wd */
    return(wd);
}

/* remove portion of path before file separator if one exists */

#ifdef Win32
HIDDEN SEXP do_basename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = nullptr;	/* -Wall */
    char sp[4*PATH_MAX+1];
    wchar_t  buf[PATH_MAX], *p;
    const wchar_t *pp;
    int i, n;

    if (TYPEOF(s = CAR(args)) != STRSXP)
	Rf_error(_("a character vector argument expected"));
    PROTECT(ans = Rf_allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == R_NaString)
	    SET_STRING_ELT(ans, i, R_NaString);
	else {
	    pp = filenameToWchar(STRING_ELT(s, i), TRUE);
	    if (wcslen(pp) > PATH_MAX - 1) Rf_error(_("path too long"));
	    wcscpy(buf, pp);
	    R_wfixslash(buf);
	    /* remove trailing file separator(s) */
	    if (*buf) {
		p = buf + wcslen(buf) - 1;
		while (p >= buf && *p == L'/') *(p--) = L'\0';
	    }
	    if ((p = wcsrchr(buf, L'/'))) p++; else p = buf;
	    Rf_wcstoutf8(sp, p, sizeof(sp));
	    SET_STRING_ELT(ans, i, Rf_mkCharCE(sp, CE_UTF8));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#else
HIDDEN SEXP do_basename(/*const*/ Expression* call, const BuiltInFunction* op, RObject* path_)
{
    SEXP ans, s = nullptr;	/* -Wall */
    char  buf[PATH_MAX], *p, fsp = FILESEP[0];
    const char *pp;
    int i, n;

    if (TYPEOF(s = path_) != STRSXP)
	Rf_error(_("a character vector argument expected"));
    PROTECT(ans = Rf_allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == R_NaString)
	    SET_STRING_ELT(ans, i, R_NaString);
	else {
	    pp = R_ExpandFileName(Rf_translateChar(STRING_ELT(s, i)));
	    if (strlen(pp) > PATH_MAX - 1)
		Rf_error(_("path too long"));
	    strcpy (buf, pp);
	    if (*buf) {
		p = buf + strlen(buf) - 1;
		while (p >= buf && *p == fsp) *(p--) = '\0';
	    }
	    if ((p = Rf_strrchr(buf, fsp)))
		p++;
	    else
		p = buf;
	    SET_STRING_ELT(ans, i, Rf_mkChar(p));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#endif

/* remove portion of path after last file separator if one exists, else
   return "."
   */

#ifdef Win32
HIDDEN SEXP do_dirname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = nullptr;	/* -Wall */
    wchar_t buf[PATH_MAX], *p;
    const wchar_t *pp;
    char sp[4*PATH_MAX+1];
    int i, n;

    if (TYPEOF(s = CAR(args)) != STRSXP)
	Rf_error(_("a character vector argument expected"));
    PROTECT(ans = Rf_allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == R_NaString)
	    SET_STRING_ELT(ans, i, R_NaString);
	else {
	    memset(sp, 0, 4*PATH_MAX);
	    pp = filenameToWchar(STRING_ELT(s, i), TRUE);
	    if (wcslen(pp) > PATH_MAX - 1)
		Rf_error(_("path too long"));
	    if (wcslen(pp)) {
		wcscpy (buf, pp);
		R_wfixslash(buf);
		/* remove trailing file separator(s) */
		while ( *(p = buf + wcslen(buf) - 1) == L'/'  && p > buf
			&& (p > buf+2 || *(p-1) != L':')) *p = L'\0';
		p = wcsrchr(buf, L'/');
		if(p == nullptr) wcscpy(buf, L".");
		else {
		    while(p > buf && *p == L'/'
			  /* this covers both drives and network shares */
			  && (p > buf+2 || *(p-1) != L':')) --p;
		    p[1] = L'\0';
		}
		Rf_wcstoutf8(sp, buf, sizeof(sp));
	    }
	    SET_STRING_ELT(ans, i, Rf_mkCharCE(sp, CE_UTF8));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#else
HIDDEN SEXP do_dirname(/*const*/ Expression* call, const BuiltInFunction* op, RObject* path_)
{
    SEXP ans, s = nullptr;	/* -Wall */
    char buf[PATH_MAX], *p, fsp = FILESEP[0];
    const char *pp;
    int i, n;

    if (TYPEOF(s = path_) != STRSXP)
	Rf_error(_("a character vector argument expected"));
    PROTECT(ans = Rf_allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == R_NaString)
	    SET_STRING_ELT(ans, i, R_NaString);
	else {
	    pp = R_ExpandFileName(Rf_translateChar(STRING_ELT(s, i)));
	    if (strlen(pp) > PATH_MAX - 1)
		Rf_error(_("path too long"));
	    size_t ll = strlen(pp);
	    if (ll) { // svMisc calls this with ""
		strcpy (buf, pp);
		/* remove trailing file separator(s) */
		while ( *(p = buf + ll - 1) == fsp  && p > buf) *p = '\0';
		p = Rf_strrchr(buf, fsp);
		if(p == nullptr)
		    strcpy(buf, ".");
		else {
		    while(p > buf && *p == fsp) --p;
		    p[1] = '\0';
		}
	    } else buf[0] = '\0';
	    SET_STRING_ELT(ans, i, Rf_mkChar(buf));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#endif


#ifndef Win32 /* Windows version is in src/gnuwin32/extra.c */
#ifndef HAVE_DECL_REALPATH
extern char *realpath(const char *path, char *resolved_path);
#endif


HIDDEN SEXP do_normalizepath(rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* path_, rho::RObject* winslash_, rho::RObject* mustWork_)
{
    SEXP ans, paths = path_;
    int i, n = LENGTH(paths);
    const char *path;
    char abspath[PATH_MAX+1];

    if (!Rf_isString(paths))
	Rf_error(_("'path' must be a character vector"));

    int mustWork = Rf_asLogical(mustWork_); /* 1, R_NaLog or 0 */

/* Does any platform not have this? */
#ifdef HAVE_REALPATH
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	path = Rf_translateChar(STRING_ELT(paths, i));
	char *res = realpath(path, abspath);
	if (res)
	    SET_STRING_ELT(ans, i, Rf_mkChar(abspath));
	else {
	    SET_STRING_ELT(ans, i, STRING_ELT(paths, i));
	    /* and report the problem */
	    if (mustWork == 1)
		Rf_error("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	    else if (mustWork == R_NaLog)
		Rf_warning("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	}
    }
#else
    Rboolean OK;
    Rf_warning("this platform does not have realpath so the results may not be canonical");
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	path = Rf_translateChar(STRING_ELT(paths, i));
	OK = strlen(path) <= PATH_MAX;
	if (OK) {
	    if (path[0] == '/') strncpy(abspath, path, PATH_MAX);
	    else {
		OK = getcwd(abspath, PATH_MAX) != nullptr;
		OK = OK && (strlen(path) + strlen(abspath) + 1 <= PATH_MAX);
		if (OK) {strcat(abspath, "/"); strcat(abspath, path);}
	    }
	}
	/* we need to check that this exists */
	if (OK) OK = (access(abspath, 0 /* F_OK */) == 0);
	if (OK) SET_STRING_ELT(ans, i, Rf_mkChar(abspath));
	else {
	    SET_STRING_ELT(ans, i, STRING_ELT(paths, i));
	    /* and report the problem */
	    if (mustWork == 1)
		Rf_error("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	    else if (mustWork == R_NaLog)
		Rf_warning("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	}
    }
#endif
    UNPROTECT(1);
    return ans;
}

#ifdef USE_INTERNAL_MKTIME
extern "C" const char *getTZinfo(void)
{
    static char def_tz[PATH_MAX+1] = "";
    if (def_tz[0]) return def_tz;

    // call Sys.timezone()
    SEXP expr = PROTECT(Rf_install("Sys.timezone"));
    SEXP call = PROTECT(Rf_lang1(expr));
    SEXP ans = PROTECT(Rf_eval(call, R_GlobalEnv));
    if(TYPEOF(ans) == STRSXP && LENGTH(ans) == 1) {
	SEXP el = STRING_ELT(ans, 0);
	if (el != R_NaString) {
	    strcpy(def_tz, R_CHAR(el));
	    // printf("tz is %s\n", R_CHAR(el));
	    UNPROTECT(3);
	    return def_tz;
	}
    }
    UNPROTECT(3);
    Rf_warning("system timezone name is unknown: set environment variable TZ");
    strcpy(def_tz, "unknown");  // code will then use TZDEFAULT, which is "UTC"
    return def_tz;
}
#endif

#endif // not Win32


/* encodeString(x, w, quote, justify) */
HIDDEN SEXP do_encodeString(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* width_, RObject* quote_, RObject* na_encode_, RObject* justify_)
{
    SEXP ans, x, s;
    R_xlen_t i, len;
    int w, quote = 0, justify, na;
    const char *cs;
    Rboolean findWidth;

    if (TYPEOF(x = x_) != STRSXP)
	Rf_error(_("a character vector argument expected"));
    if(Rf_isNull(width_)) w = R_NaInt;
    else {
	w = Rf_asInteger(width_);
	if(w != R_NaInt && w < 0)
	    Rf_error(_("invalid '%s' value"), "width");
    }
    findWidth = Rboolean(w == R_NaInt);
    s = quote_;
    if(LENGTH(s) != 1 || TYPEOF(s) != STRSXP)
	Rf_error(_("invalid '%s' value"), "quote");
    cs = Rf_translateChar(STRING_ELT(s, 0));
    if(strlen(cs) > 0) quote = cs[0];
    if(strlen(cs) > 1)
	Rf_warning(_("only the first character of 'quote' will be used"));
    justify = Rf_asInteger(na_encode_);
    if(justify == R_NaInt || justify < 0 || justify > 3)
	Rf_error(_("invalid '%s' value"), "justify");
    if(justify == 3) w = 0;
    na = Rf_asLogical(justify_);
    if(na == R_NaLog) Rf_error(_("invalid '%s' value"), "na.encode");

    len = XLENGTH(x);
    if(findWidth && justify < 3) {
	w  = 0;
	for(i = 0; i < len; i++) {
	    s = STRING_ELT(x, i);
	    if(na || s != R_NaString)
		w = std::max(w, Rstrlen(s, quote));
	}
	if(quote) w +=2; /* for surrounding quotes */
    }
    PROTECT(ans = Rf_duplicate(x));
    for(i = 0; i < len; i++) {
	s = STRING_ELT(x, i);
	if(na || s != R_NaString) {
	    cetype_t ienc = Rf_getCharCE(s);
	    if(ienc == CE_UTF8) {
		const char *ss = Rf_EncodeString(s, w-1000000, quote,
					      (Rprt_adj) justify);
		SET_STRING_ELT(ans, i, Rf_mkCharCE(ss, ienc));
	    } else {
		const char *ss = Rf_EncodeString(s, w, quote, Rprt_adj( justify));
		SET_STRING_ELT(ans, i, Rf_mkChar(ss));
	    }
	}
    }
    UNPROTECT(1);
    return ans;
}

HIDDEN SEXP do_encoding(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP ans, x;
    R_xlen_t i, n;
    const char *tmp;

    if (TYPEOF(x = x_) != STRSXP)
	Rf_error(_("a character vector argument expected"));
    n = XLENGTH(x);
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	if(IS_BYTES(STRING_ELT(x, i))) tmp = "bytes";
	else if(IS_LATIN1(STRING_ELT(x, i))) tmp = "latin1";
	else if(IS_UTF8(STRING_ELT(x, i))) tmp = "UTF-8";
	else tmp = "unknown";
	SET_STRING_ELT(ans, i, Rf_mkChar(tmp));
    }
    UNPROTECT(1);
    return ans;
}

HIDDEN SEXP do_setencoding(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* value_)
{
    SEXP x, enc, tmp;
    int m;
    R_xlen_t i, n;
    const char *thiss;

    if (TYPEOF(x = x_) != STRSXP)
	Rf_error(_("a character vector argument expected"));
    if (TYPEOF(enc = value_) != STRSXP)
	Rf_error(_("a character vector 'value' expected"));
    m = LENGTH(enc);
    if(m == 0)
	Rf_error(_("'value' must be of positive length"));
    if(MAYBE_REFERENCED(x)) x = Rf_duplicate(x);
    PROTECT(x);
    n = XLENGTH(x);
    for(i = 0; i < n; i++) {
	cetype_t ienc = CE_NATIVE;
	thiss = R_CHAR(STRING_ELT(enc, i % m)); /* ASCII */
	if(streql(thiss, "latin1")) ienc = CE_LATIN1;
	else if(streql(thiss, "UTF-8")) ienc = CE_UTF8;
	else if(streql(thiss, "bytes")) ienc = CE_BYTES;
	tmp = STRING_ELT(x, i);
	if(tmp == R_NaString) continue;
	if (! ((ienc == CE_LATIN1 && IS_LATIN1(tmp)) ||
	       (ienc == CE_UTF8 && IS_UTF8(tmp)) ||
	       (ienc == CE_BYTES && IS_BYTES(tmp)) ||
	       (ienc == CE_NATIVE && ! IS_LATIN1(tmp) && ! IS_UTF8(tmp))))
	    SET_STRING_ELT(x, i, Rf_mkCharLenCE(R_CHAR(tmp), LENGTH(tmp), ienc));
    }
    UNPROTECT(1);
    return x;
}

HIDDEN SEXP Rf_markKnown(const char *s, SEXP ref)
{
    cetype_t ienc = CE_NATIVE;
    if(ENC_KNOWN(ref)) {
	if(known_to_be_latin1) ienc = CE_LATIN1;
	if(known_to_be_utf8) ienc = CE_UTF8;
    }
    return Rf_mkCharCE(s, ienc);
}

Rboolean Rf_strIsASCII(const char *str)
{
    const char *p;
    for(p = str; *p; p++)
	if(static_cast<unsigned int>(*p) > 0x7F) return FALSE;
    return TRUE;
}

/* Number of additional bytes */
static const unsigned char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5 };

HIDDEN int utf8clen(char c)
{
    /* This allows through 8-bit chars 10xxxxxx, which are invalid */
    if ((c & 0xc0) != 0xc0) return 1;
    return 1 + utf8_table4[c & 0x3f];
}

static Rwchar_t utf16toucs(wchar_t high, wchar_t low)
{
    return 0x10000 + ((int) (high & 0x3FF) << 10 ) + (int) (low & 0x3FF);
}

/* Return the low UTF-16 surrogate from a UTF-8 string; assumes all testing has been done. */
static wchar_t utf8toutf16low(const char *s)
{
    return (unsigned int) LOW_SURROGATE_START | ((s[2] & 0x0F) << 6) | (s[3] & 0x3F);
}

HIDDEN Rwchar_t utf8toucs32(wchar_t high, const char *s)
{
    return utf16toucs(high, utf8toutf16low(s));
}

/* These return the result in wchar_t.  If wchar_t is 16 bit (e.g. UTF-16LE on Windows
   only the high surrogate is returned; call utf8toutf16low next. */
HIDDEN size_t Rf_utf8toucs(wchar_t *wc, const char *s)
{
    unsigned int byte;
    wchar_t local, *w;
    byte = static_cast<unsigned char>(*s);
    w = wc ? wc: &local;

    if (byte == 0) {
	*w = wchar_t( 0);
	return 0;
    } else if (byte < 0xC0) {
	*w = wchar_t( byte);
	return 1;
    } else if (byte < 0xE0) {
	if(strlen(s) < 2) return size_t(-2);
	if ((s[1] & 0xC0) == 0x80) {
	    *w = wchar_t ((((byte & 0x1F) << 6) | (s[1] & 0x3F)));
	    return 2;
	} else return size_t(-1);
    } else if (byte < 0xF0) {
	if(strlen(s) < 3) return size_t(-2);
	if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80)) {
	    *w = wchar_t (((byte & 0x0F) << 12)
			  | static_cast<unsigned int>((s[1] & 0x3F) << 6)
			  | (s[2] & 0x3F));
	    byte = static_cast<unsigned int>(*w);
	    /* Surrogates range */
	    if(byte >= 0xD800 && byte <= 0xDFFF) return size_t(-1);
	    if(byte == 0xFFFE || byte == 0xFFFF) return size_t(-1);
	    return 3;
	} else return size_t(-1);

    } else if (byte < 0xf8) {
	if(strlen(s) < 4) return size_t(-2);
	if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80) && ((s[3] & 0xC0) == 0x80)) {
	    unsigned int cvalue = (((byte & 0x0F) << 18)
			| static_cast<unsigned int>(((s[1] & 0x3F) << 12))
			| static_cast<unsigned int>(((s[2] & 0x3F) << 6))
			| (s[3] & 0x3F));
	    if(sizeof(wchar_t) < 4) /* Assume UTF-16 and return high surrogate.  Users need to call utf8toutf16low next. */
		*w = (wchar_t) ((cvalue - 0x10000) >> 10) | 0xD800;
	    else
		*w = (wchar_t) cvalue;
	    return 4;
	} else return (size_t)-1;
    }
    if(sizeof(wchar_t) < 4) return (size_t)-2;
    /* So now handle 5.6 byte sequences with no testing */
    if (byte < 0xFC) {
	if(strlen(s) < 5) return size_t(-2);
	*w = wchar_t (((byte & 0x0F) << 24)
		      | static_cast<unsigned int>(((s[1] & 0x3F) << 12))
		      | static_cast<unsigned int>(((s[2] & 0x3F) << 12))
		      | static_cast<unsigned int>(((s[3] & 0x3F) << 6))
		      | (s[4] & 0x3F));
	return 5;
    } else {
	if(strlen(s) < 6) return size_t(-2);
	*w = wchar_t (((byte & 0x0F) << 30)
		      | static_cast<unsigned int>(((s[1] & 0x3F) << 24))
		      | static_cast<unsigned int>(((s[2] & 0x3F) << 18))
		      | static_cast<unsigned int>(((s[3] & 0x3F) << 12))
		      | static_cast<unsigned int>(((s[4] & 0x3F) << 6))
		      | (s[5] & 0x3F));
	return 6;
    }
}

size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n)
{
    ssize_t m, res = 0;
    const char *t;
    wchar_t *p;
    wchar_t local;

    if(wc)
	for(p = wc, t = s; ; p++, t += m) {
	    m  = ssize_t(Rf_utf8toucs(p, t));
	    if (m < 0) Rf_error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	    res ++;
	    if (res >= int(n)) break;
	    if (IS_HIGH_SURROGATE(*p)) {
	    	*(++p) = utf8toutf16low(t);
	    	res ++;
	    	if (res >= ssize_t(n)) break;
	    }
	}
    else
	for(t = s; ; res++, t += m) {
	    m  = ssize_t(Rf_utf8toucs(&local, t));
	    if (m < 0) Rf_error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	}
    return size_t(res);
}

/* based on pcre.c */
static const unsigned int utf8_table1[] =
  { 0x7f, 0x7ff, 0xffff, 0x1fffff, 0x3ffffff, 0x7fffffff};
static const unsigned int utf8_table2[] = { 0, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc};

/* s is NULL, or it contains at least n bytes.  Just write a a terminator if it's not big enough. */

static size_t Rwcrtomb32(char *s, Rwchar_t cvalue, size_t n)
{
    size_t i, j;
    if (!n) return 0;
    if (s) *s = 0;    /* Simplifies exit later */
    if(cvalue == 0) return 0;
    for (i = 0; i < int(sizeof(utf8_table1)/sizeof(int)); i++)
	if (static_cast<unsigned int>(cvalue) <= utf8_table1[i]) break;
    if (i >= n - 1) return 0;  /* need space for terminal null */
    if (s) {
    	s += i;
    	for (j = i; j > 0; j--) {
	    *s-- = (char) (0x80 | (cvalue & 0x3f));
	    cvalue >>= 6;
        }
    	*s = (char) (utf8_table2[i] | cvalue);
    }
    return i + 1;
}

/* on input, wc is a string encoded in UTF-16 or UCS-2 or UCS-4.
   s can be a buffer of size n>=0 chars, or NULL.  If n=0 or s=NULL, nothing is written. 
   The return value is the number of chars including the terminating null.  If the
   buffer is not big enough, the result is truncated but still null-terminated */
HIDDEN // but used in windlgs
size_t Rf_wcstoutf8(char *s, const wchar_t *wc, size_t n)
{
    size_t m, res=0;
    char *t;
    const wchar_t *p;
    if (!n) return 0;
    for(p = wc, t = s; ; p++) {
    	if (IS_SURROGATE_PAIR(*p, *(p+1))) {
    	    Rwchar_t cvalue =  ((*p & 0x3FF) << 10) + (*(p+1) & 0x3FF) + 0x010000;
	    m = Rwcrtomb32(t, cvalue, n - res);
	    p++;
    	} else 
    	    m = Rwcrtomb32(t, (Rwchar_t)(*p), n - res);
    	if (!m) break;
	res += m;
	if (t)
	    t += m;
    }
    return res + 1;
}

/* A version that reports failure as an error */
size_t Rf_mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps)
{
    size_t used;

    if(n <= 0 || !*s) return size_t(0);
    used = mbrtowc(wc, s, n, ps);
    if(int(used) < 0) {
	/* This gets called from the menu setup in RGui */
	if (!R_Is_Running) return size_t(-1);
	/* let's try to print out a readable version */
	vector<char> errv(4*strlen(s) + 1);
	char *err = &errv[0], *q;
	const char *p;
	for(p = s, q = err; *p; ) {
	    /* don't do the first to keep ps state straight */
	    if(p > s) used = mbrtowc(nullptr, p, n, ps);
	    if(used == 0) break;
	    else if(int(used) > 0) {
		memcpy(q, p, used);
		p += used;
		q += used;
		n -= used;
	    } else {
		sprintf(q, "<%02x>", static_cast<unsigned char>(*p++));
		q += 4;
		n--;
	    }
	}
	*q = '\0';
	Rf_error(_("invalid multibyte string at '%s'"), err);
    }
    return used;
}

HIDDEN Rboolean mbcsValid(const char *str)
{
    return  Rboolean((int(mbstowcs(nullptr, str, 0)) >= 0));
}

/* used in src/library/grDevices/src/cairo/cairoFns.cpp */
#include "valid_utf8.h"
Rboolean utf8Valid(const char *str)
{
    return  Rboolean(valid_utf8(str, strlen(str)) == 0);
}

HIDDEN SEXP do_validUTF8(Expression* call, const BuiltInFunction* op, RObject* x)
{
    if (!Rf_isString(x))
	Rf_error(_("invalid '%s' argument"), "x");
    R_xlen_t n = XLENGTH(x);
    SEXP ans = Rf_allocVector(LGLSXP, n); // no allocation below
    int *lans = LOGICAL(ans);
    for (R_xlen_t i = 0; i < n; i++)
	lans[i] = utf8Valid(R_CHAR(STRING_ELT(x, i)));
    return ans;
}

HIDDEN SEXP do_validEnc(Expression* call, const BuiltInFunction* op, RObject* x)
{
    if (!Rf_isString(x))
	Rf_error(_("invalid '%s' argument"), "x");
    R_xlen_t n = XLENGTH(x);
    SEXP ans = Rf_allocVector(LGLSXP, n); // no allocation below
    int *lans = LOGICAL(ans);
    for (R_xlen_t i = 0; i < n; i++) {
	SEXP p = STRING_ELT(x, i);
	if (IS_BYTES(p) || IS_LATIN1(p)) lans[i] = 1;
	else if (IS_UTF8(p) || utf8locale) lans[i] = utf8Valid(R_CHAR(p));
	else if(mbcslocale) lans[i] = mbcsValid(R_CHAR(p));
	else lans[i] = 1;
    }
    return ans;
}


/* MBCS-aware versions of common comparisons.  Only used for ASCII c */
char *Rf_strchr(const char *s, int c)
{
    char *p = const_cast<char *>(s);
    mbstate_t mb_st;
    size_t used;

    if(!mbcslocale || utf8locale) return const_cast<char *>(strchr(s, c));
    mbs_init(&mb_st);
    while( (used = Rf_mbrtowc(nullptr, p, MB_CUR_MAX, &mb_st)) ) {
	if(*p == c) return p;
	p += used;
    }
    return nullptr;
}

char *Rf_strrchr(const char *s, int c)
{
    char *p = const_cast<char *>(s), *plast = nullptr;
    mbstate_t mb_st;
    size_t used;

    if(!mbcslocale || utf8locale) return const_cast<char *>(strrchr(s, c));
    mbs_init(&mb_st);
    while( (used = Rf_mbrtowc(nullptr, p, MB_CUR_MAX, &mb_st)) ) {
	if(*p == c) plast = p;
	p += used;
    }
    return plast;
}

#ifdef Win32
void R_fixslash(char *s)
{
    char *p = s;

    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Rf_mbrtowc(NULL, p, MB_CUR_MAX, &mb_st))) {
	    if(*p == '\\') *p = '/';
	    p += used;
	}
    } else
	for (; *p; p++) if (*p == '\\') *p = '/';
    /* preserve network shares */
    if(s[0] == '/' && s[1] == '/') s[0] = s[1] = '\\';
}

void R_UTF8fixslash(char *s)
{
    char *p = s;

	for (; *p; p++) if (*p == '\\') *p = '/';
	/* preserve network shares */
	if(s[0] == '/' && s[1] == '/') s[0] = s[1] = '\\';
}

static void R_wfixslash(wchar_t *s)
{
    wchar_t *p = s;

    for (; *p; p++) if (*p == L'\\') *p = L'/';
    /* preserve network shares */
    if(s[0] == L'/' && s[1] == L'/') s[0] = s[1] = L'\\';
}


void R_fixbackslash(char *s)
{
    char *p = s;

    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Rf_mbrtowc(NULL, p, MB_CUR_MAX, &mb_st))) {
	    if(*p == '/') *p = '\\';
	    p += used;
	}
    } else
	for (; *p; p++) if (*p == '/') *p = '\\';
}
#endif

extern "C" {

NORET void F77_SYMBOL(rexitc)(char *msg, int *nchar)
{
    int nc = *nchar;
    char buf[256];
    if(nc > 255) {
	Rf_warning(_("error message truncated to 255 chars"));
	nc = 255;
    }
    strncpy(buf, msg, size_t(nc));
    buf[nc] = '\0';
    Rf_error("%s", buf);
}

void F77_SYMBOL(rwarnc)(char *msg, int *nchar)
{
    int nc = *nchar;
    char buf[256];
    if(nc > 255) {
	Rf_warning(_("warning message truncated to 255 chars"));
	nc = 255;
    }
    strncpy(buf, msg, size_t(nc));
    buf[nc] = '\0';
    Rf_warning("%s", buf);
}

void F77_SYMBOL(rchkusr)(void)
{
    R_CheckUserInterrupt();
}

/* Return a copy of a string using memory from R_alloc.
   NB: caller has to manage R_alloc stack.  Used in platform.cpp
*/
char *Rf_acopy_string(const char *in)
{
    char *out;
    size_t len = strlen(in);
    if (len > 0) {
	out = (char *) R_alloc(1 + len, sizeof(char));
	strcpy(out, in);
    } else
	out = const_cast<char *>("");
    return out;
}



/* Table from
http://unicode.org/Public/MAPPINGS/VENDORS/ADOBE/symbol.txt
*/

static int s2u[224] = {
    0x0020, 0x0021, 0x2200, 0x0023, 0x2203, 0x0025, 0x0026, 0x220D,
    0x0028, 0x0029, 0x2217, 0x002B, 0x002C, 0x2212, 0x002E, 0x002F,
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
    0x2245, 0x0391, 0x0392, 0x03A7, 0x0394, 0x0395, 0x03A6, 0x0393,
    0x0397, 0x0399, 0x03D1, 0x039A, 0x039B, 0x039C, 0x039D, 0x039F,
    0x03A0, 0x0398, 0x03A1, 0x03A3, 0x03A4, 0x03A5, 0x03C2, 0x03A9,
    0x039E, 0x03A8, 0x0396, 0x005B, 0x2234, 0x005D, 0x22A5, 0x005F,
    0xF8E5, 0x03B1, 0x03B2, 0x03C7, 0x03B4, 0x03B5, 0x03C6, 0x03B3,
    0x03B7, 0x03B9, 0x03D5, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BF,
    0x03C0, 0x03B8, 0x03C1, 0x03C3, 0x03C4, 0x03C5, 0x03D6, 0x03C9,
    0x03BE, 0x03C8, 0x03B6, 0x007B, 0x007C, 0x007D, 0x223C, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x20AC, 0x03D2, 0x2032, 0x2264, 0x2044, 0x221E, 0x0192, 0x2663,
    0x2666, 0x2665, 0x2660, 0x2194, 0x2190, 0x2191, 0x2192, 0x2193,
    0x00B0, 0x00B1, 0x2033, 0x2265, 0x00D7, 0x221D, 0x2202, 0x2022,
    0x00F7, 0x2260, 0x2261, 0x2248, 0x2026, 0xF8E6, 0xF8E7, 0x21B5,
    0x2135, 0x2111, 0x211C, 0x2118, 0x2297, 0x2295, 0x2205, 0x2229,
    0x222A, 0x2283, 0x2287, 0x2284, 0x2282, 0x2286, 0x2208, 0x2209,
    0x2220, 0x2207, 0xF6DA, 0xF6D9, 0xF6DB, 0x220F, 0x221A, 0x22C5,
    0x00AC, 0x2227, 0x2228, 0x21D4, 0x21D0, 0x21D1, 0x21D2, 0x21D3,
    0x25CA, 0x2329, 0xF8E8, 0xF8E9, 0xF8EA, 0x2211, 0xF8EB, 0xF8EC,
    0xF8ED, 0xF8EE, 0xF8EF, 0xF8F0, 0xF8F1, 0xF8F2, 0xF8F3, 0xF8F4,
    0x0020, 0x232A, 0x222B, 0x2320, 0xF8F5, 0x2321, 0xF8F6, 0xF8F7,
    0xF8F8, 0xF8F9, 0xF8FA, 0xF8FB, 0xF8FC, 0xF8FD, 0xF8FE, 0x0020
};

void *Rf_AdobeSymbol2utf8(char *work, const char *c0, int nwork)
{
    const unsigned char *c = reinterpret_cast<const unsigned char *>(c0);
    unsigned char *t = reinterpret_cast<unsigned char *>(work);
    while (*c) {
	if (*c < 32) *t++ = ' ';
	else {
	    unsigned int u = static_cast<unsigned int>(s2u[*c - 32]);
	    if (u < 128) *t++ = static_cast<unsigned char>(u);
	    else if (u < 0x800) {
		*t++ = static_cast<unsigned char>((0xc0 | (u >> 6)));
		*t++ = static_cast<unsigned char>((0x80 | (u & 0x3f)));
	    } else {
		*t++ = static_cast<unsigned char>((0xe0 | (u >> 12)));
		*t++ = static_cast<unsigned char>((0x80 | ((u >> 6) & 0x3f)));
		*t++ = static_cast<unsigned char>((0x80 | (u & 0x3f)));
	    }
	}
	if (t+6 > reinterpret_cast<unsigned char *>(work + nwork)) break;
	c++;
    }
    *t = '\0';
    return static_cast<char *>(work);
}

HIDDEN int Rf_AdobeSymbol2ucs2(int n)
{
    if(n >= 32 && n < 256) return s2u[n-32];
    else return 0;
}

double R_strtod5(const char *str, char **endptr, char dec,
		 Rboolean NA, int exact)
{
    LDOUBLE ans = 0.0, p10 = 10.0, fac = 1.0;
    int n, expn = 0, sign = 1, ndigits = 0, exph = -1;
    const char *p = str;

    /* optional whitespace */
    while (isspace(*p)) p++;

    if (NA && streqln(p, "NA", 2)) {
	ans = R_NaReal;
	p += 2;
	goto done;
    }

   /* optional sign */
    switch (*p) {
    case '-': sign = -1;
    case '+': p++;
    default: ;
    }

    if (strncasecmp(p, "NaN", 3) == 0) {
	ans = R_NaN;
	p += 3;
	goto done;
    /* C99 specifies this: must come first to avoid 'inf' match */
    } else if (strncasecmp(p, "infinity", 8) == 0) {
	ans = R_PosInf;
	p += 8;
	goto done;
    } else if (strncasecmp(p, "Inf", 3) == 0) {
	ans = R_PosInf;
	p += 3;
	goto done;
    }

    if(strlen(p) > 2 && p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
	/* This will overflow to Inf if appropriate */
	for(p += 2; p; p++) {
	    if('0' <= *p && *p <= '9') ans = 16*ans + (*p -'0');
	    else if('a' <= *p && *p <= 'f') ans = 16*ans + (*p -'a' + 10);
	    else if('A' <= *p && *p <= 'F') ans = 16*ans + (*p -'A' + 10);
	    else if(*p == dec) {exph = 0; continue;}
	    else break;
	    if (exph >= 0) exph += 4;
	}
#define strtod_EXACT_CLAUSE						\
	if(exact && ans > 0x1.fffffffffffffp52) {			\
	    if(exact == R_NaLog)					\
		Rf_warning(_(						\
		"accuracy loss in conversion from \"%s\" to numeric"),	\
			str);						\
	    else {							\
		ans = R_NaReal;						\
		p = str; /* back out */					\
		goto done;						\
	    }								\
	}
	strtod_EXACT_CLAUSE;
	if (*p == 'p' || *p == 'P') {
	    int expsign = 1;
	    double p2 = 2.0;
	    switch(*++p) {
	    case '-': expsign = -1;
	    case '+': p++;
	    default: ;
	    }
	    /* The test for n is in response to PR#16358; it's not right if the exponent is
	       very large, but the overflow or underflow below will handle it. */
#define MAX_EXPONENT_PREFIX 9999
	    for (n = 0; *p >= '0' && *p <= '9'; p++) n = (n < MAX_EXPONENT_PREFIX) ? n * 10 + (*p - '0') : n;
	    if (ans != 0.0) { /* PR#15976:  allow big exponents on 0 */
		expn += expsign * n;
		if(exph > 0) {
		    if (expn - exph < -122) {	/* PR#17199:  fac may overflow below if expn - exph is too small.  
		                                   2^-122 is a bit bigger than 1E-37, so should be fine on all systems */
		    	for (n = exph, fac = 1.0; n; n >>= 1, p2 *= p2)
			    if (n & 1) fac *= p2;
			ans /= fac;
			p2 = 2.0;
		    } else
			expn -= exph;
		}
		if (expn < 0) {
		    for (n = -expn, fac = 1.0; n; n >>= 1, p2 *= p2)
			if (n & 1) fac *= p2;
		    ans /= fac;
		} else {
		    for (n = expn, fac = 1.0; n; n >>= 1, p2 *= p2)
			if (n & 1) fac *= p2;
		    ans *= fac;
		}
	    }
	}
	goto done;
    } // end {hexadecimal case}

    for ( ; *p >= '0' && *p <= '9'; p++, ndigits++) ans = 10*ans + (*p - '0');
    if (*p == dec)
	for (p++; *p >= '0' && *p <= '9'; p++, ndigits++, expn--)
	    ans = 10*ans + (*p - '0');
    if (ndigits == 0) {
	ans = R_NaReal;
	p = str; /* back out */
	goto done;
    }
    strtod_EXACT_CLAUSE;

    if (*p == 'e' || *p == 'E') {
	int expsign = 1;
	switch(*++p) {
	case '-': expsign = -1;
	case '+': p++;
	default: ;
	}
	for (n = 0; *p >= '0' && *p <= '9'; p++) n = (n < MAX_EXPONENT_PREFIX) ? n * 10 + (*p - '0') : n;
	expn += expsign * n;
    }

    /* avoid unnecessary underflow for large negative exponents */
    if (expn + ndigits < -300) {
	for (n = 0; n < ndigits; n++) ans /= 10.0;
	expn += ndigits;
    }
    if (expn < -307) { /* use underflow, not overflow */
	for (n = -expn, fac = 1.0; n; n >>= 1, p10 *= p10)
	    if (n & 1) fac /= p10;
	ans *= fac;
    } else if (expn < 0) { /* positive powers are exact */
	for (n = -expn, fac = 1.0; n; n >>= 1, p10 *= p10)
	    if (n & 1) fac *= p10;
	ans /= fac;
    } else if (ans != 0.0) { /* PR#15976:  allow big exponents on 0, e.g. 0E4933 */
	for (n = expn, fac = 1.0; n; n >>= 1, p10 *= p10)
	    if (n & 1) fac *= p10;
	ans *= fac;
    }

    /* explicit overflow to infinity */
    if (ans > DBL_MAX) {
	if (endptr) *endptr = (char *) p;
	return (sign > 0) ? R_PosInf : R_NegInf;
    }

done:
    if (endptr) *endptr = (char *) p;
    return sign * (double) ans;
}


double R_strtod4(const char *str, char **endptr, char dec, Rboolean NA)
{
    return R_strtod5(str, endptr, dec, NA, FALSE);
}

double R_strtod(const char *str, char **endptr)
{
    return R_strtod5(str, endptr, '.', FALSE, FALSE);
}

double R_atof(const char *str)
{
    return R_strtod5(str, NULL, '.', FALSE, FALSE);
}
} // extern "C"

/* enc2native and enc2utf8, but they are the same in a UTF-8 locale */
/* primitive */
HIDDEN SEXP do_enc2(/*const*/ Expression* call, const BuiltInFunction* op, RObject* ans)
{
    SEXP el;
    R_xlen_t i;
    Rboolean duped = FALSE;

    if (!Rf_isString(ans))
	Rf_errorcall(call, "argument is not a character vector");
    for (i = 0; i < XLENGTH(ans); i++) {
	el = STRING_ELT(ans, i);
	if (el == R_NaString) continue;
	if (op->variant() || known_to_be_utf8) { /* enc2utf8 */
	    if (IS_UTF8(el) || IS_ASCII(el) || IS_BYTES(el)) continue;
	    if (!duped) { ans = PROTECT(Rf_duplicate(ans)); duped = TRUE; }
	    SET_STRING_ELT(ans, i,
			   Rf_mkCharCE(Rf_translateCharUTF8(el), CE_UTF8));
	} else if (ENC_KNOWN(el)) { /* enc2native */
	    if (IS_ASCII(el) || IS_BYTES(el)) continue;
	    if (known_to_be_latin1 && IS_LATIN1(el)) continue;
	    if (!duped) { PROTECT(ans = Rf_duplicate(ans)); duped = TRUE; }
	    if (known_to_be_latin1)
		SET_STRING_ELT(ans, i, Rf_mkCharCE(Rf_translateChar(el), CE_LATIN1));
	    else
		SET_STRING_ELT(ans, i, Rf_mkChar(Rf_translateChar(el)));
	}
    }
    if(duped) UNPROTECT(1);
    return ans;
}

#ifdef USE_ICU
# include <locale.h>
#ifdef USE_ICU_APPLE
/* macOS is missing the headers */
extern "C" {
    typedef int UErrorCode; /* really an enum these days */
    struct UCollator;
    typedef struct UCollator UCollator;

    enum UCollationResult {
	UCOL_EQUAL    = 0,
	UCOL_GREATER    = 1,
	UCOL_LESS    = -1
    };

    enum UColAttributeValue {
	UCOL_DEFAULT = -1,
	UCOL_PRIMARY = 0,
	UCOL_SECONDARY = 1,
	UCOL_TERTIARY = 2,
	UCOL_DEFAULT_STRENGTH = UCOL_TERTIARY,
	UCOL_CE_STRENGTH_LIMIT,
	UCOL_QUATERNARY=3,
	UCOL_IDENTICAL=15,
	UCOL_STRENGTH_LIMIT,
	UCOL_OFF = 16,
	UCOL_ON = 17,
	UCOL_SHIFTED = 20,
	UCOL_NON_IGNORABLE = 21,
	UCOL_LOWER_FIRST = 24,
	UCOL_UPPER_FIRST = 25,
	UCOL_ATTRIBUTE_VALUE_COUNT
    };

    typedef UColAttributeValue UCollationStrength;

    enum UColAttribute {
	UCOL_FRENCH_COLLATION, 
	UCOL_ALTERNATE_HANDLING, 
	UCOL_CASE_FIRST, 
	UCOL_CASE_LEVEL,
	UCOL_NORMALIZATION_MODE, 
	UCOL_DECOMPOSITION_MODE = UCOL_NORMALIZATION_MODE,
	UCOL_STRENGTH,
	UCOL_HIRAGANA_QUATERNARY_MODE,
	UCOL_NUMERIC_COLLATION, 
	UCOL_ATTRIBUTE_COUNT
    };

    /* UCharIterator struct has to be defined since we use its instances as
       local variables, but we don't actually use any of its members. */
    typedef struct UCharIterator {
	const void *context;
	int32_t length, start, index, limit, reservedField;
	void *fns[16]; /* we overshoot here (there is just 10 fns in ICU 3.6),
			  but we have to make sure that enough stack space
			  is allocated when used as a local var in future
			  versions */
    } UCharIterator;

    UCollator* ucol_open(const char *loc, UErrorCode *status);
    void ucol_close(UCollator *coll);
    void ucol_setAttribute(UCollator *coll, UColAttribute attr, 
			   UColAttributeValue value, UErrorCode *status);
    void ucol_setStrength(UCollator *coll, UCollationStrength strength);
    UCollationResult ucol_strcollIter(const UCollator *coll,
				      UCharIterator *sIter,
				      UCharIterator *tIter,
				      UErrorCode *status);
    void uiter_setUTF8(UCharIterator *iter, const char *s, int32_t length);

    void uloc_setDefault(const char* localeID, UErrorCode* status);
}  // extern "C"

enum ULocDataLocaleType {
    ULOC_ACTUAL_LOCALE = 0,
    ULOC_VALID_LOCALE = 1,
    ULOC_DATA_LOCALE_TYPE_LIMIT = 3
};


extern "C"
const char* ucol_getLocaleByType(const UCollator *coll,
				 ULocDataLocaleType type,
				 UErrorCode *status);

#define U_ZERO_ERROR 0
#define U_FAILURE(x) ((x)>U_ZERO_ERROR)

#else
#include <unicode/utypes.h>
#include <unicode/ucol.h>
#include <unicode/uloc.h>
#include <unicode/uiter.h>
#endif

static UCollator *collator = nullptr;
static int collationLocaleSet = 0;

/* called from platform.cpp */
HIDDEN void resetICUcollator(void)
{
    if (collator) ucol_close(collator);
    collator = nullptr;
    collationLocaleSet = 0;
}

static const struct ATtable {
    const char * const str;
    int val;
} ATtable[] = {
    { "case_first", UCOL_CASE_FIRST },
    { "upper", UCOL_UPPER_FIRST },
    { "lower", UCOL_LOWER_FIRST },
    { "default ", UCOL_DEFAULT },
    { "strength", 999 },
    { "primary ", UCOL_PRIMARY },
    { "secondary ", UCOL_SECONDARY },
    { "teritary ", UCOL_TERTIARY },
    { "guaternary ", UCOL_QUATERNARY },
    { "identical ", UCOL_IDENTICAL },
    { "french_collation", UCOL_FRENCH_COLLATION },
    { "on", UCOL_ON },
    { "off", UCOL_OFF },
    { "normalization", UCOL_NORMALIZATION_MODE },
    { "alternate_handling", UCOL_ALTERNATE_HANDLING },
    { "non_ignorable", UCOL_NON_IGNORABLE },
    { "shifted", UCOL_SHIFTED },
    { "case_level", UCOL_CASE_LEVEL },
    { "hiragana_quaternary", UCOL_HIRAGANA_QUATERNARY_MODE },
    { NULL,  0 }
};

#ifdef Win32
#define BUFFER_SIZE 512
typedef int (WINAPI *PGSDLN)(LPWSTR, int);

static const char *getLocale(void)
{
    const char *p = getenv("R_ICU_LOCALE");
    if (p && p[0]) return p;

    // This call is >= Vista/Server 2008
    // ICU should accept almost all of these, e.g. en-US and uz-Latn-UZ
    PGSDLN pGSDLN = (PGSDLN)
	GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
		       "GetSystemDefaultLocaleName");
    if(pGSDLN) {
	WCHAR wcBuffer[BUFFER_SIZE];
	pGSDLN(wcBuffer, BUFFER_SIZE);
	static char locale[BUFFER_SIZE];
	WideCharToMultiByte(CP_ACP, 0, wcBuffer, -1,
			    locale, BUFFER_SIZE, NULL, NULL);
	return locale;
    } else return "root";
}
#else
static const char *getLocale(void)
{
    const char *p = getenv("R_ICU_LOCALE");
    return (p && p[0]) ? p : setlocale(LC_COLLATE, NULL);
}
#endif

HIDDEN SEXP do_ICUset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x;
    UErrorCode  status = U_ZERO_ERROR;

    for (; args != nullptr; args = CDR(args)) {
	SEXP tag = TAG(args);
	if (Rf_isNull(tag)) Rf_error(_("all arguments must be named"));
	const char *thiss = R_CHAR(PRINTNAME(tag));
	const char *s;

	x = CAR(args);
	if (!Rf_isString(x) || LENGTH(x) != 1)
	    Rf_error(_("invalid '%s' argument"), thiss);
	s = R_CHAR(STRING_ELT(x, 0));
	if (streql(thiss, "locale")) {
	    if (collator) {
		ucol_close(collator);
		collator = nullptr;
	    }
	    if(streql(s, "ASCII")) {
		collationLocaleSet = 2;
	    } else {
		if(strcmp(s, "none")) {
		    if(streql(s, "default"))
			uloc_setDefault(getLocale(), &status);
		    else uloc_setDefault(s, &status);
		    if(U_FAILURE(status))
			Rf_error("failed to set ICU locale %s (%d)", s, status);
		    collator = ucol_open(NULL, &status);
		    if (U_FAILURE(status)) {
			collator = nullptr;
			Rf_error("failed to open ICU collator (%d)", status);
		    }
		}
		collationLocaleSet = 1;
	    }
	} else {
	    int i, at = -1, val = -1;
	    for (i = 0; ATtable[i].str; i++)
		if (streql(thiss, ATtable[i].str)) {
		    at = ATtable[i].val;
		    break;
		}
	    for (i = 0; ATtable[i].str; i++)
		if (streql(s, ATtable[i].str)) {
		    val = ATtable[i].val;
		    break;
		}
	    if (collator && at == 999 && val >= 0) {
		ucol_setStrength(collator, UCollationStrength(val));
	    } else if (collator && at >= 0 && val >= 0) {
		ucol_setAttribute(collator, UColAttribute(at), UColAttributeValue(val), &status);
		if (U_FAILURE(status))
		    Rf_error("failed to set ICU collator attribute");
	    }
	}
    }

    return nullptr;
}

HIDDEN SEXP do_ICUget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const char *ans = "unknown", *res;

    if (collationLocaleSet == 2) {
	ans = "ASCII";
    } else if(collator) {
	UErrorCode  status = U_ZERO_ERROR;
	int type = Rf_asInteger(CAR(args));
	if (type < 1 || type > 2)
	    Rf_error(_("invalid '%s' value"), "type");

	res = ucol_getLocaleByType(collator,
				   type == 1 ? ULOC_ACTUAL_LOCALE : ULOC_VALID_LOCALE,
				   &status);
	if(!U_FAILURE(status) && res) ans = res;
    } else ans = "ICU not in use";
    return Rf_mkString(ans);
}

/* Caller has to manage the R_alloc stack */
/* NB: strings can have equal collation weight without being identical */
HIDDEN
int Scollate(SEXP a, SEXP b)
{
    if (!collationLocaleSet) {
	int errsv = errno;      /* OSX may set errno in the operations below. */
	collationLocaleSet = 1;
#ifndef Win32
	if (strcmp("C", getLocale()) ) {
#else
	const char *p = getenv("R_ICU_LOCALE");
	if(p && p[0]) {
#endif
	    UErrorCode status = U_ZERO_ERROR;
	    uloc_setDefault(getLocale(), &status);
	    if(U_FAILURE(status))
		Rf_error("failed to set ICU locale (%d)", status);
	    collator = ucol_open(NULL, &status);
	    if (U_FAILURE(status)) {
		collator = nullptr;
		Rf_error("failed to open ICU collator (%d)", status);
	    }
	}
	errno = errsv;
    }
    if (collator == nullptr)
	return collationLocaleSet == 2 ?
	    strcmp(Rf_translateChar(a), Rf_translateChar(b)) :
	    strcoll(Rf_translateChar(a), Rf_translateChar(b));

    UCharIterator aIter, bIter;
    const char *as = Rf_translateCharUTF8(a), *bs = Rf_translateCharUTF8(b);
    int len1 = (int) strlen(as), len2 = (int) strlen(bs);
    uiter_setUTF8(&aIter, as, len1);
    uiter_setUTF8(&bIter, bs, len2);
    UErrorCode status = U_ZERO_ERROR;
    int result = ucol_strcollIter(collator, &aIter, &bIter, &status);
    if (U_FAILURE(status)) Rf_error("could not collate using ICU");
    return result;
}

#else /* not USE_ICU */

HIDDEN SEXP do_ICUset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rf_warning(_("ICU is not supported on this build"));
    return nullptr;
}

HIDDEN SEXP do_ICUget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return Rf_mkString("ICU not in use");
}

HIDDEN void resetICUcollator(void) {}

# ifdef Win32

static int Rstrcoll(const char *s1, const char *s2)
{
    vector<wchar_t> v1(strlen(s1)+1), v2(strlen(s2)+1);
    wchar_t* w1 = &v1[0];
    wchar_t* w2 = &v2[0];
    Rf_utf8towcs(w1, s1, strlen(s1));
    Rf_utf8towcs(w2, s2, strlen(s2));
    return wcscoll(w1, w2);
}

int Scollate(SEXP a, SEXP b)
{
    if(Rf_getCharCE(a) == CE_UTF8 || Rf_getCharCE(b) == CE_UTF8)
	return Rstrcoll(Rf_translateCharUTF8(a), Rf_translateCharUTF8(b));
    else
	return strcoll(Rf_translateChar(a), Rf_translateChar(b));
}

# else
HIDDEN int Scollate(SEXP a, SEXP b)
{
    return strcoll(Rf_translateChar(a), Rf_translateChar(b));
}

# endif
#endif

static void bincode(double *x, R_xlen_t n, double *breaks, int nb,
	int *code, int right, int include_border)
{
    int lo, hi, nb1 = nb - 1, newi;
    int lft = !right;

    /* This relies on breaks being sorted, so wise to check that */
    for(int i = 1; i < nb; i++)
	if(breaks[i-1] > breaks[i]) Rf_error(_("'breaks' is not sorted"));

    for(R_xlen_t i = 0; i < n; i++) {
	code[i] = R_NaInt;
	if(!std::isnan(x[i])) {
	    lo = 0;
	    hi = nb1;
	    if(x[i] <  breaks[lo] || breaks[hi] < x[i] ||
	       (x[i] == breaks[lft ? hi : lo] && ! include_border)) ;
	    else {
		while(hi - lo >= 2) {
		    newi = (hi + lo)/2;
		    if(x[i] > breaks[newi] || (lft && x[i] == breaks[newi]))
			lo = newi;
		    else
			hi = newi;
		}
		code[i] = lo + 1;
	    }
	}
    }
}

/* 'breaks' cannot be a long vector as the return codes are integer. */
HIDDEN SEXP do_bincode(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* breaks_, RObject* right_, RObject* include_lowest_)
{
    SEXP x, breaks, right, lowest;
    x = x_;
    breaks = breaks_;
    right = right_;
    lowest = include_lowest_;
#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(breaks))
	Rf_error(_("long vector '%s' is not supported"), "breaks");
#endif
    PROTECT(x = Rf_coerceVector(x, REALSXP));
    PROTECT(breaks = Rf_coerceVector(breaks, REALSXP));
    R_xlen_t n = XLENGTH(x);
    int nB = LENGTH(breaks), sr = Rf_asLogical(right), sl = Rf_asLogical(lowest);
    if (nB == R_NaInt) Rf_error(_("invalid '%s' argument"), "breaks");
    if (sr == R_NaInt) Rf_error(_("invalid '%s' argument"), "right");
    if (sl == R_NaInt) Rf_error(_("invalid '%s' argument"), "include.lowest");
    SEXP codes;
    PROTECT(codes = Rf_allocVector(INTSXP, n));
    bincode(REAL(x), n, REAL(breaks), nB, INTEGER(codes), sr, sl);
    UNPROTECT(3);
    return codes;
}

HIDDEN SEXP do_tabulate(/*const*/ Expression* call, const BuiltInFunction* op, RObject* bin_, RObject* nbins_)
{
    SEXP in = bin_, nbin = nbins_;
    if (TYPEOF(in) != INTSXP)  Rf_error("invalid input");
    R_xlen_t n = XLENGTH(in);
    int nb = Rf_asInteger(nbin);
    if (nb == R_NaInt || nb < 0)
	Rf_error(_("invalid '%s' argument"), "nbin");
    int *x = INTEGER(in);
    SEXP ans;
#ifdef LONG_VECTOR_SUPPORT
    if (n > INT_MAX) {
	ans = Rf_allocVector(REALSXP, nb);
	double *y = REAL(ans);
	if (nb) memset(y, 0, nb * sizeof(double));
	for(R_xlen_t i = 0 ; i < n ; i++)
	    if (x[i] != R_NaInt && x[i] > 0 && x[i] <= nb) y[x[i] - 1]++;
    } else
#endif
    {
	ans = Rf_allocVector(INTSXP, nb);
	int *y = INTEGER(ans);
	if (nb) memset(y, 0, nb * sizeof(int));
	for(R_xlen_t i = 0 ; i < n ; i++)
	    if (x[i] != R_NaInt && x[i] > 0 && x[i] <= nb) y[x[i] - 1]++;
    }
    return ans;
}

/* Note: R's findInterval( x , vec, ...)  has first two arguments swapped !
 * .Internal(findInterval(vec, x, rightmost.closed, all.inside,  left.open))
 *                         xt  x    right             inside       leftOp
 * x can be a long vector but xt cannot since the result is integer
*/
HIDDEN SEXP do_findinterval(/*const*/ Expression* call, const BuiltInFunction* op, RObject* vec_, RObject* x_, RObject* rightmost_closed_, RObject* all_inside_, RObject* left_op_)
{
    SEXP xt, x, right, inside, leftOp;
    xt = vec_;
    x = x_;
    right = rightmost_closed_;
    inside = all_inside_;
    leftOp = left_op_;
    if(TYPEOF(xt) != REALSXP || TYPEOF(x) != REALSXP) Rf_error("invalid input");
#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(xt))
	Rf_error(_("long vector '%s' is not supported"), "vec");
#endif
    int n = LENGTH(xt);
    if (n == R_NaInt) Rf_error(_("invalid '%s' argument"), "vec");
    R_xlen_t nx = XLENGTH(x);
    int sr = Rf_asLogical(right), si = Rf_asLogical(inside), lO = Rf_asLogical(leftOp);
    if (sr == R_NaInt)
	Rf_error(_("invalid '%s' argument"), "rightmost.closed");
    if (si == R_NaInt)
	Rf_error(_("invalid '%s' argument"), "all.inside");
    SEXP ans = Rf_allocVector(INTSXP, nx);
    double *rxt = REAL(xt), *rx = REAL(x);
    int ii = 1;
    for(int i = 0; i < nx; i++) {
	if (std::isnan(rx[i]))
	    ii = R_NaInt;
	else {
	    int mfl;
	    ii = findInterval2(rxt, n, rx[i], Rboolean(sr), Rboolean(si), Rboolean(lO), ii, &mfl); // -> ../appl/interv.c
	}
	INTEGER(ans)[i] = ii;
    }
    return ans;
}

#ifdef Win32
// this includes RS.h
# undef ERROR
#endif
#include <R_ext/Applic.h>
HIDDEN SEXP do_pretty(/*const*/ Expression* call, const BuiltInFunction* op, RObject* min_, RObject*  max_, RObject*  n_, RObject*  min_n_, RObject*  shrink_sml_, RObject*  bias_, RObject*  eps_correct_)
{
    SEXP ans, nm, hi;
    double l = Rf_asReal(min_);
    if (!std::isfinite(l)) Rf_error(_("invalid '%s' argument"), "l");
    double u = Rf_asReal(max_);
    if (!std::isfinite(u)) Rf_error(_("invalid '%s' argument"), "u");
    int n = Rf_asInteger(n_);
    if (n == R_NaInt || n < 0) Rf_error(_("invalid '%s' argument"), "n");
    int min_n = Rf_asInteger(min_n_);
    if (min_n == R_NaInt || min_n < 0 || min_n > n) 
	Rf_error(_("invalid '%s' argument"), "min.n");
    double shrink = Rf_asReal(shrink_sml_);
    if (!std::isfinite(shrink) || shrink <= 0.) 
	Rf_error(_("invalid '%s' argument"), "shrink.sml");
    PROTECT(hi = Rf_coerceVector(bias_, REALSXP));
    double z;
    if (!std::isfinite(z = REAL(hi)[0]) || z < 0.)
	Rf_error(_("invalid '%s' argument"), "high.u.bias");
    if (!std::isfinite(z = REAL(hi)[1]) || z < 0.)
	Rf_error(_("invalid '%s' argument"), "u5.bias");
    int eps = Rf_asInteger(eps_correct_); /* eps.correct */
    if (eps == R_NaInt || eps < 0 || eps > 2) 
	Rf_error(_("'eps.correct' must be 0, 1, or 2"));
    R_pretty(&l, &u, &n, min_n, shrink, REAL(hi), eps, 1);
    PROTECT(ans = Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(ans, 0, Rf_ScalarReal(l));
    SET_VECTOR_ELT(ans, 1, Rf_ScalarReal(u));
    SET_VECTOR_ELT(ans, 2, Rf_ScalarInteger(n));
    nm = Rf_allocVector(STRSXP, 3);
    Rf_setAttrib(ans, Symbols::NamesSymbol, nm);
    SET_STRING_ELT(nm, 0, Rf_mkChar("l"));
    SET_STRING_ELT(nm, 1, Rf_mkChar("u"));
    SET_STRING_ELT(nm, 2, Rf_mkChar("n"));
    UNPROTECT(2);
    return ans;
}

/*
    r <- .Internal(formatC(x, as.character(mode), width, digits,
		   as.character(format), as.character(flag), i.strlen))
*/

static void str_signif(void *x, R_xlen_t n, const char *type, int width, int digits,
	   const char *format, const char *flag, char **result);

HIDDEN SEXP do_formatC(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject*  mode_, RObject*  width_, RObject*  digits_, RObject*  format_, RObject*  flag_, RObject*  i_strlen_)
{
    SEXP x = x_;
    if (!Rf_isVector(x)) Rf_error(_("'x' must be a vector"));
    R_xlen_t n = XLENGTH(x);
    const char *type = R_CHAR(STRING_ELT(mode_, 0));
    int width = Rf_asInteger(width_);
    int digits = Rf_asInteger(digits_);
    const char *fmt = R_CHAR(STRING_ELT(format_, 0));
    const char *flag = R_CHAR(STRING_ELT(flag_, 0));
    SEXP i_strlen = PROTECT(Rf_coerceVector(i_strlen_, INTSXP));
    char **cptr = static_cast<char **>(RHO_alloc(n, sizeof(char*)));
    for (R_xlen_t i = 0; i < n; i++) {
	int ix = INTEGER(i_strlen)[i] + 2;
	cptr[i] = static_cast<char *>(RHO_alloc(ix + 1, sizeof(char)));
	memset(cptr[i], ' ', ix);
	cptr[i][ix] = 0;
    }
    void *px = nullptr /* -Wall */;
    switch(TYPEOF(x)) {
    case INTSXP: px = INTEGER(x); break;
    case REALSXP: px = REAL(x); break;
    default: Rf_error("unsupported type ");
    }
    str_signif(px, n, type, width, digits, fmt, flag, cptr);
    SEXP ans = PROTECT(Rf_allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; i++) SET_STRING_ELT(ans, i, Rf_mkChar(cptr[i]));
    UNPROTECT(2);
    return ans;
}

/* Former src/appl/strsignif.c
 *
 *  Copyright (C) Martin Maechler, 1994, 1998
 *  Copyright (C) 2001-2013 the R Core Team
 *
 *  I want you to preserve the copyright of the original author(s),
 *  and encourage you to send me any improvements by e-mail. (MM).
 *
 *  Originally from Bill Dunlap
 *  bill@stat.washington.edu
 *  Wed Feb 21, 1990
 *
 *  Much improved by Martin Maechler, including the "fg" format.
 *
 *  Patched by Friedrich.Leisch@ci.tuwien.ac.at
 *  Fri Nov 22, 1996
 *
 *  Some fixes by Ross Ihaka
 *  ihaka@stat.auckland.ac.nz
 *  Sat Dec 21, 1996
 *  Integer arguments changed from "long" to "int"
 *  Bus error due to non-writable strings fixed
 *
 *  BDR 2001-10-30 use R_alloc not Calloc as memory was not
 *  reclaimed on error (and there are many error exits).
 *
 *	type	"double" or "integer" (R - numeric 'mode').
 *
 *	width	The total field width; width < 0 means to left justify
 *		the number in this field (equivalent to flag = "-").
 *		It is possible that the result will be longer than this,
 *		but that should only happen in reasonable cases.
 *
 *	digits	The desired number of digits after the decimal point.
 *		digits < 0 uses the default for C, namely 6 digits.
 *
 *	format	"d" (for integers) or "f", "e","E", "g", "G" (for 'real')
 *		"f" gives numbers in the usual "xxx.xxx" format;
 *		"e" and "E" give n.ddde<nn> or n.dddE<nn> (scientific format);
 *		"g" and "G" puts them into scientific format if it saves
 *		space to do so.
 *	    NEW: "fg" gives numbers in "xxx.xxx" format as "f",
 *		  ~~  however, digits are *significant* digits and,
 *		      if digits > 0, no trailing zeros are produced, as in "g".
 *
 *	flag	Format modifier as in K&R "C", 2nd ed., p.243;
 *		e.g., "0" pads leading zeros; "-" does left adjustment
 *		the other possible flags are  "+", " ", and "#".
 *	  New (Feb.98): if flag has more than one character, all are passed..
 */

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef Win32
/* avoid latest MinGW's redefinition in stdio.h */
#include <trioremap.h>
#endif
#include <Rmath.h>		/* fround */

static void str_signif(void *x, R_xlen_t n, const char *type, int width, int digits,
		const char *format, const char *flag, char **result)
{
    int dig = abs(digits);
    Rboolean rm_trailing_0 = Rboolean(digits >= 0);
    Rboolean do_fg = Rboolean(streql("fg", format)); /* TRUE  iff  format == "fg" */
    double xx;
    int iex;
    size_t j, len_flag = strlen(flag);
    const void *vmax = vmaxget();

    char *f0  =	 R_alloc(size_t(do_fg) ? 1+1+len_flag+3 : 1, sizeof(char));
    char *form = R_alloc(size_t(1)+1+len_flag+3 + strlen(format),
			 sizeof(char));

    if (width == 0)
	Rf_error("width cannot be zero");

    if (streql("d", format)) {
	if (len_flag == 0)
	    strcpy(form, "%*d");
	else {
	    strcpy(form, "%");
	    strcat(form, flag);
	    strcat(form, "*d");
	}
	if (streql("integer", type))
	    for (R_xlen_t i = 0; i < n; i++)
		snprintf(result[i], strlen(result[i]) + 1,
			 form, width, (static_cast<int *>(x))[i]);
	else
	    Rf_error("'type' must be \"integer\" for  \"d\"-format");
    }
    else { /* --- floating point --- */
	if (len_flag == 0)
	    strcpy(form, "%*.*");
	else {
	    strcpy(form, "%");
	    strcat(form, flag);
	    strcat(form, "*.*");
	}

	if(do_fg) {
	    strcpy(f0, "%");
	    strcat(f0, flag);
	    strcat(f0, ".*f");
	    strcat(form, "g");
	}
	else
	    strcat(form, format);
#ifdef DEBUG
	fprintf(stderr, "strsignif.c: form='%s', width=%d, dig=%d\n",
		form, width, dig);
	if(do_fg) fprintf(stderr, "\t\"fg\": f0='%s'.", f0);
#endif
	if (streql("double", type)) {
	    if(do_fg) /* do smart "f" : */
		for (R_xlen_t i = 0; i < n; i++) {
		    xx = (static_cast<double *>(x))[i];
		    if(xx == 0.)
			strcpy(result[i], "0");
		    else {
			/* This was iex= (int)floor(log10(std::abs(xx)))
			   That's wrong, as xx might get rounded up,
			   and we do need some fuzz or 99.5 is correct.
			*/
			double xxx = std::abs(xx), X;
			iex = int(std::floor(std::log10(xxx) + 1e-12));
			X = fround(xxx/Rexp10((double)iex) + 1e-12,
				   (double)(dig-1));
			if(iex > 0 &&  X >= 10) {
			    xx = X * Rexp10((double)iex);
			    iex++;
			}
			if(iex == -4 && std::abs(xx) < 1e-4) {/* VERY rare case */
			    iex = -5;
			}
			if(iex < -4) {
				/* "g" would result in 'e-' representation:*/
			    snprintf(result[i], strlen(result[i]) + 1,
				     f0, dig-1 + -iex, xx);
#ifdef DEBUG
			    fprintf(stderr, " x[%d]=%g, iex=%d\n", i, xx, iex);
			    fprintf(stderr, "\tres. = '%s'; ", result[i]);
#endif
			    /* Remove trailing  "0"s __ IFF flag has no '#': */
			    if(rm_trailing_0) {
				j = strlen(result[i])-1;
#ifdef DEBUG
				int jL = j;
#endif
				while(result[i][j] == '0') j--;
				result[i][j+1] = '\0';
#ifdef DEBUG
				fprintf(stderr, "\t>>> jL=%d, j=%d; new res= '%s'\n",
					jL, j, result[i]);
#endif
			    }

			} else { /* iex >= -4:	NOT "e-" */
				/* if iex >= dig, would have "e+" representation */
#ifdef DEBUG
			    fprintf(stderr, "\t  iex >= -4; using %d for 'dig'\n",
				    (iex >= dig) ? (iex+1) : dig);
#endif
			    snprintf(result[i], strlen(result[i]) + 1,
				     form, width, (iex >= dig) ? (iex+1) : dig, xx);
			}
		    } /* xx != 0 */
		} /* if(do_fg) for(i..) */
	    else
		for (R_xlen_t i = 0; i < n; i++)
		    snprintf(result[i], strlen(result[i]) + 1,
			     form, width, dig, (static_cast<double *>(x))[i]);
	} else
	    Rf_error("'type' must be \"real\" for this format");
    }
    vmaxset(vmax);
}
