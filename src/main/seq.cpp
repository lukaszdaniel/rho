/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2017  The R Core Team.
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

/* The x:y  primitive calls do_colon(); do_colon() calls cross_colon() if
   both arguments are factors and seq_colon() otherwise.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <float.h>  /* for DBL_EPSILON */
#include <Rmath.h>
#include <R_ext/Itermacros.h>

#include "RBufferUtils.h"
#include "rho/ArgMatcher.hpp"
#include "rho/ExpressionVector.hpp"
#include "rho/GCStackRoot.hpp"

using namespace rho;

static R_StringBuffer cbuff = {nullptr, 0, MAXELTSIZE};

#define _S4_rep_keepClass
/* ==>  rep(<S4>, .) keeps class e.g., for list-like */

static SEXP cross_colon(SEXP call, SEXP s, SEXP t)
{
    SEXP a, la, ls, lt, rs, rt;
    int i, j, k, n, nls, nlt;
    char *cbuf;
    const void *vmax = vmaxget();

    if (Rf_length(s) != Rf_length(t))
	errorcall(call, _("unequal factor lengths"));
    n = Rf_length(s);
    ls = getAttrib(s, R_LevelsSymbol);
    lt = getAttrib(t, R_LevelsSymbol);
    nls = LENGTH(ls);
    nlt = LENGTH(lt);
    PROTECT(a = allocVector(INTSXP, n));
    PROTECT(rs = coerceVector(s, INTSXP));
    PROTECT(rt = coerceVector(t, INTSXP));
    for (i = 0; i < n; i++) {
	int vs = INTEGER(rs)[i];
	int vt = INTEGER(rt)[i];
	if ((vs == NA_INTEGER) || (vt == NA_INTEGER))
	    INTEGER(a)[i] = NA_INTEGER;
	else
	    INTEGER(a)[i] = vt + (vs - 1) * nlt;
    }
    UNPROTECT(2);
    if (!isNull(ls) && !isNull(lt)) {
	PROTECT(la = allocVector(STRSXP, nls * nlt));
	k = 0;
	/* FIXME: possibly UTF-8 version */
	for (i = 0; i < nls; i++) {
	    const char *vi = translateChar(STRING_ELT(ls, i));
	    size_t vs = strlen(vi);
	    for (j = 0; j < nlt; j++) {
		const char *vj = translateChar(STRING_ELT(lt, j));
		size_t vt = strlen(vj), len = vs + vt + 2;
		cbuf = static_cast<char*>(R_AllocStringBuffer(len, &cbuff));
		snprintf(cbuf, len, "%s:%s", vi, vj);
		SET_STRING_ELT(la, k, mkChar(cbuf));
		k++;
	    }
	}
	setAttrib(a, R_LevelsSymbol, la);
	UNPROTECT(1);
    }
    PROTECT(la = mkString("factor"));
    setAttrib(a, R_ClassSymbol, la);
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    vmaxset(vmax);
    return a;
}

/* interval at which to check interrupts */
#define NINTERRUPT 1000000U

static SEXP seq_colon(double n1, double n2, SEXP call)
{
    double r = fabs(n2 - n1);
    if(r >= R_XLEN_T_MAX)
	errorcall(call, _("result would be too long a vector"));

    SEXP ans;
    R_xlen_t n = (R_xlen_t)(r + 1 + FLT_EPSILON);

    Rboolean useInt = Rboolean((n1 <= INT_MAX) &&  (n1 == (int) n1));
    if(useInt) {
	if(n1 <= INT_MIN || n1 > INT_MAX)
	    useInt = FALSE;
	else {
	    /* r := " the effective 'to' "  of  from:to */
	    double dn = double( n);
	    r = n1 + ((n1 <= n2) ? dn-1 : -(dn-1));
	    if(r <= INT_MIN || r > INT_MAX) useInt = FALSE;
	}
    }
    if (useInt) {
	int in1 = (int)(n1);
	ans = allocVector(INTSXP, n);
	if (n1 <= n2)
	    for (int i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = in1 + i;
	    }
	else
	    for (int i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = in1 - i;
	    }
    } else {
	ans = allocVector(REALSXP, n);
	if (n1 <= n2)
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = n1 + double(i);
	    }
	else
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = n1 - double(i);
	    }
    }
    return ans;
}

SEXP attribute_hidden do_colon(/*const*/ Expression* call, const BuiltInFunction* op, RObject* from_, RObject* to_)
{
    SEXP s1, s2;
    double n1, n2;

    if (inherits(from_, "factor") && inherits(to_, "factor"))
	return(cross_colon(call, from_, to_));

    s1 = from_;
    s2 = to_;
    n1 = Rf_length(s1);
    n2 = Rf_length(s2);
    if (n1 == 0 || n2 == 0)
	errorcall(call, _("argument of length 0"));
    if (n1 > 1)
	warningcall(call,
		    n_("numerical expression has %d element: only the first used",
			     "numerical expression has %d elements: only the first used",
			     (int) n1), (int) n1);
    if (n2 > 1)
	warningcall(call,
		    n_("numerical expression has %d element: only the first used",
			     "numerical expression has %d elements: only the first used",
			     (int) n2), (int) n2);
    n1 = asReal(s1);
    n2 = asReal(s2);
    if (ISNAN(n1) || ISNAN(n2))
	errorcall(call, _("NA/NaN argument"));
    return seq_colon(n1, n2, call);
}

/* rep.int(x, times) for a vector times */
static SEXP rep2(SEXP s, SEXP ncopy)
{
    R_xlen_t i, j, nc, n;
    SEXP a, t;

#define R2_SWITCH_LOOP(it) \
    switch (TYPEOF(s)) { \
    case LGLSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = 0; j < (R_xlen_t) it[i]; j++) \
		LOGICAL(a)[n++] = LOGICAL(s)[i]; \
	} \
	break; \
    case INTSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		INTEGER(a)[n++] = INTEGER(s)[i]; \
	} \
	break; \
    case REALSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		REAL(a)[n++] = REAL(s)[i]; \
	} \
	break; \
    case CPLXSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		COMPLEX(a)[n++] = COMPLEX(s)[i]; \
	} \
	break; \
    case STRSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		SET_STRING_ELT(a, n++, STRING_ELT(s, i)); \
	} \
	break; \
    case VECSXP: \
    case EXPRSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    SEXP elt = lazy_duplicate(VECTOR_ELT(s, i)); \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		SET_VECTOR_ELT(a, n++, elt); \
	    if (j > 1) SET_NAMED(elt, 2); \
	} \
	break; \
    case RAWSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		RAW(a)[n++] = RAW(s)[i]; \
	} \
	break; \
    default: \
	UNIMPLEMENTED_TYPE("rep2", s); \
    }

#ifdef LONG_VECTOR_SUPPORT
    if (TYPEOF(ncopy) != INTSXP)
#else
    if (TYPEOF(ncopy) == REALSXP)
#endif
    PROTECT(t = coerceVector(ncopy, REALSXP));
    else
    PROTECT(t = coerceVector(ncopy, INTSXP));

    nc = xlength(ncopy);
    double sna = 0;
    if (TYPEOF(t) == REALSXP)
    for (i = 0; i < nc; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if (ISNAN(REAL(t)[i]) || REAL(t)[i] <= -1 ||
	    REAL(t)[i] >= R_XLEN_T_MAX+1.0)
	    error(_("invalid '%s' value"), "times");
	sna += (R_xlen_t) REAL(t)[i];
    }
    else
    for (i = 0; i < nc; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if (INTEGER(t)[i] == NA_INTEGER || INTEGER(t)[i] < 0)
	    error(_("invalid '%s' value"), "times");
	sna += INTEGER(t)[i];
    }
    if (sna > R_XLEN_T_MAX)
	error(_("invalid '%s' value"), "times");
    R_xlen_t na = (R_xlen_t) sna;
/*    R_xlen_t ni = NINTERRUPT, ratio;
    if(nc > 0) {
	ratio = na/nc; // average no of replications
	if (ratio > 1000U) ni = 1000U;
	} */
    PROTECT(a = allocVector(TYPEOF(s), na));
    n = 0;
    if (TYPEOF(t) == REALSXP)
	R2_SWITCH_LOOP(REAL(t))
    else
	R2_SWITCH_LOOP(INTEGER(t))
    UNPROTECT(2);
    return a;
}
#undef R2_SWITCH_LOOP

/* rep_len(x, len), also used for rep.int() with scalar 'times' */
static SEXP rep3(SEXP s, R_xlen_t ns, R_xlen_t na)
{
    R_xlen_t i, j;
    SEXP a;

    PROTECT(a = allocVector(TYPEOF(s), na));

    switch (TYPEOF(s)) {
    case LGLSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(a)[i] = LOGICAL(s)[j];
	});
	break;
    case INTSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    INTEGER(a)[i] = INTEGER(s)[j];
	});
	break;
    case REALSXP:
	MOD_ITERATE1(na, ns, i, j,  {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    REAL(a)[i] = REAL(s)[j];
	});
	break;
    case CPLXSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    COMPLEX(a)[i] = COMPLEX(s)[j];
	});
	break;
    case RAWSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    RAW(a)[i] = RAW(s)[j];
	});
	break;
    case STRSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(a, i, STRING_ELT(s, j));
	});
	break;
    case VECSXP:
    case EXPRSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(a, i, lazy_duplicate(VECTOR_ELT(s, j)));
	});
	break;
    default:
	UNIMPLEMENTED_TYPE("rep3", s);
    }
    UNPROTECT(1);
    return a;
}

SEXP attribute_hidden do_rep_int(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* times_)
{
    SEXP s = x_, ncopy = times_;
    R_xlen_t nc;
    SEXP a;

    if (!Rf_isVector(ncopy))
	Rf_error(_("incorrect type for second argument"));

    if (!Rf_isVector(s) && s != R_NilValue)
	Rf_error(_("attempt to replicate an object of type '%s'"),
	      Rf_type2char(TYPEOF(s)));

    nc = Rf_xlength(ncopy); // might be 0
    if (nc != 1 && nc == Rf_xlength(s))
	PROTECT(a = rep2(s, ncopy));
    else {
	if (nc != 1) Rf_error(_("invalid '%s' value"), "times");

	R_xlen_t ns = Rf_xlength(s);
	if (TYPEOF(ncopy) != INTSXP) {
	double snc = Rf_asReal(ncopy);
	if (!R_FINITE(snc) || snc <= -1 ||
	    (ns > 0 && snc >= R_XLEN_T_MAX+1.0))
	    Rf_error(_("invalid '%s' value"), "times");
	nc = ns == 0 ? 1 : (R_xlen_t) snc;
	} else
	if ((nc = Rf_asInteger(ncopy)) == NA_INTEGER || nc < 0)/* nc = 0 ok */
	    Rf_error(_("invalid '%s' value"), "times");
	if ((double) nc * ns > R_XLEN_T_MAX)
	    Rf_error(_("invalid '%s' value"), "times");
	PROTECT(a = rep3(s, ns, nc * ns));
    }

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(s)) { /* e.g. contains = "list" */
	setAttrib(a, R_ClassSymbol, getAttrib(s, R_ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    if (inherits(s, "factor")) {
	SEXP tmp;
	if(inherits(s, "ordered")) {
	    PROTECT(tmp = allocVector(STRSXP, 2));
	    SET_STRING_ELT(tmp, 0, mkChar("ordered"));
	    SET_STRING_ELT(tmp, 1, mkChar("factor"));
	} else PROTECT(tmp = mkString("factor"));
	setAttrib(a, R_ClassSymbol, tmp);
	UNPROTECT(1);
	setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
    }
    UNPROTECT(1);
    return a;
}

SEXP attribute_hidden do_rep_len(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* length_out_)
{
    R_xlen_t ns, na;
    SEXP a, s, len;

    s = x_;

    if (!Rf_isVector(s) && s != R_NilValue)
	Rf_error(_("attempt to replicate non-vector"));

    len = length_out_;
    if(Rf_length(len) != 1)
	Rf_error(_("invalid '%s' value"), "length.out");
    if (TYPEOF(len) != INTSXP) {
    double sna = Rf_asReal(len);
    if (ISNAN(sna) || sna <= -1 || sna >= R_XLEN_T_MAX+1.0)
	Rf_error(_("invalid '%s' value"), "length.out");
    na = (R_xlen_t) sna;
    } else
    if ((na = Rf_asInteger(len)) == NA_INTEGER || na < 0) /* na = 0 ok */
	Rf_error(_("invalid '%s' value"), "length.out");

    if (TYPEOF(s) == NILSXP && na > 0)
	Rf_error(_("cannot replicate NULL to a non-zero length"));
    ns = Rf_xlength(s);
    if (ns == 0) {
	SEXP a;
	PROTECT(a = Rf_duplicate(s));
	if(na > 0) a = Rf_xlengthgets(a, na);
	UNPROTECT(1);
	return a;
    }
    PROTECT(a = rep3(s, ns, na));

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(s)) { /* e.g. contains = "list" */
	setAttrib(a, R_ClassSymbol, getAttrib(s, R_ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    if (inherits(s, "factor")) {
	SEXP tmp;
	if(inherits(s, "ordered")) {
	    PROTECT(tmp = allocVector(STRSXP, 2));
	    SET_STRING_ELT(tmp, 0, mkChar("ordered"));
	    SET_STRING_ELT(tmp, 1, mkChar("factor"));
	} else PROTECT(tmp = mkString("factor"));
	setAttrib(a, R_ClassSymbol, tmp);
	UNPROTECT(1);
	setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
    }
    UNPROTECT(1);
    return a;
}

/* rep(), allowing for both times and each ;
 * -----  nt == length(times) ;  if (nt == 1)  'times' is *not* accessed  */
static SEXP rep4(SEXP x, SEXP times, R_xlen_t len, R_xlen_t each, R_xlen_t nt)
{
    SEXP a;
    R_xlen_t lx = xlength(x);
    R_xlen_t i, j, k, k2, k3, sum;

    // faster code for common special case
    if (each == 1 && nt == 1) return rep3(x, lx, len);

    PROTECT(a = allocVector(TYPEOF(x), len));

#define R4_SWITCH_LOOP(itimes) \
    switch (TYPEOF(x)) { \
    case LGLSXP: \
	    for(i = 0, k = 0, k2 = 0; i < lx; i++) { \
/*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
		for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
		for(k3 = 0; k3 < sum; k3++) { \
		    LOGICAL(a)[k2++] = LOGICAL(x)[i]; \
		    if(k2 == len) goto done; \
		} \
	    } \
	break; \
    case INTSXP: \
	    for(i = 0, k = 0, k2 = 0; i < lx; i++) { \
/*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
		for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
		for(k3 = 0; k3 < sum; k3++) { \
		    INTEGER(a)[k2++] = INTEGER(x)[i]; \
		    if(k2 == len) goto done; \
		} \
	    } \
	break; \
    case REALSXP: \
	    for(i = 0, k = 0, k2 = 0; i < lx; i++) { \
/*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
		for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
		for(k3 = 0; k3 < sum; k3++) { \
		    REAL(a)[k2++] = REAL(x)[i]; \
		    if(k2 == len) goto done; \
		} \
	    } \
	break; \
    case CPLXSXP: \
	    for(i = 0, k = 0, k2 = 0; i < lx; i++) { \
/*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
		for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
		for(k3 = 0; k3 < sum; k3++) { \
		    COMPLEX(a)[k2++] = COMPLEX(x)[i]; \
		    if(k2 == len) goto done; \
		} \
	    } \
	break; \
    case STRSXP: \
	    for(i = 0, k = 0, k2 = 0; i < lx; i++) { \
/*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
		for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
		for(k3 = 0; k3 < sum; k3++) { \
		    SET_STRING_ELT(a, k2++, STRING_ELT(x, i)); \
		    if(k2 == len) goto done; \
		} \
	    } \
	break; \
    case VECSXP: \
    case EXPRSXP: \
	    for(i = 0, k = 0, k2 = 0; i < lx; i++) { \
/*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
		for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
		for(k3 = 0; k3 < sum; k3++) { \
		    SET_VECTOR_ELT(a, k2++, VECTOR_ELT(x, i)); \
		    if(k2 == len) goto done; \
		} \
	    } \
	break; \
    case RAWSXP: \
	    for(i = 0, k = 0, k2 = 0; i < lx; i++) { \
/*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
		for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
		for(k3 = 0; k3 < sum; k3++) { \
		    RAW(a)[k2++] = RAW(x)[i]; \
		    if(k2 == len) goto done; \
		} \
	    } \
	break; \
    default: \
	UNIMPLEMENTED_TYPE("rep4", x); \
    }

    if(nt == 1)
    switch (TYPEOF(x)) {
    case LGLSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		LOGICAL(a)[i] = LOGICAL(x)[(i/each) % lx];
	    }
	break;
    case INTSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(a)[i] = INTEGER(x)[(i/each) % lx];
	    }
	break;
    case REALSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(a)[i] = REAL(x)[(i/each) % lx];
	    }
	break;
    case CPLXSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		COMPLEX(a)[i] = COMPLEX(x)[(i/each) % lx];
	    }
	break;
    case STRSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		SET_STRING_ELT(a, i, STRING_ELT(x, (i/each) % lx));
	    }
	break;
    case VECSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		SET_VECTOR_ELT(a, i, VECTOR_ELT(x, (i/each) % lx));
	    }
	break;
    case EXPRSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		SET_XVECTOR_ELT(a, i, XVECTOR_ELT(x, (i/each) % lx));
	    }
	break;
    case RAWSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		RAW(a)[i] = RAW(x)[(i/each) % lx];
	    }
	break;
    default:
	UNIMPLEMENTED_TYPE("rep4", x);
    }
    else if(TYPEOF(times) == REALSXP)
	R4_SWITCH_LOOP(REAL(times))
    else
	R4_SWITCH_LOOP(INTEGER(times))
done:
    UNPROTECT(1);
    return a;
}
#undef R4_SWITCH_LOOP

/* We are careful to use MissingArgHandling::Keep here (inside
   DispatchOrEval) to avoid dropping missing arguments so e.g.
   rep(1:3,,8) matches length.out */

/* This is a primitive SPECIALSXP with internal argument matching */
SEXP attribute_hidden do_rep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, times, length_out, each_, ignored;
    R_xlen_t i, lx, len = NA_INTEGER, each = 1, nt;
    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::RAW);

    /* includes factors, POSIX[cl]t, Date */
    auto dispatched = Rf_DispatchOrEval(SEXP_downcast<Expression*>(call),
                                        SEXP_downcast<BuiltInFunction*>(op),
                                        &arglist,
                                        SEXP_downcast<Environment*>(rho),
                                        MissingArgHandling::Keep);
    if (dispatched.first)
        return dispatched.second;

    /* This is a primitive, and we have not dispatched to a method
       so we manage the argument matching ourselves.  We pretend this is
       rep(x, times, length.out, each, ...)
    */
    static GCRoot<ArgMatcher> matcher = new ArgMatcher(
	{ "x", "times", "length.out", "each", "..." });
    matcher->match(arglist, { &x, &times, &length_out, &each_, &ignored });

    /* supported in R 2.15.x */
    if (TYPEOF(x) == LISTSXP)
	errorcall(call, "replication of pairlists is defunct");

    lx = xlength(x);

    if (TYPEOF(length_out) != INTSXP) {
    double slen = asReal(length_out);
    if (R_FINITE(slen)) {
	if (slen <= -1 || slen >= R_XLEN_T_MAX+1.0)
	    errorcall(call, _("invalid '%s' argument"), "length.out");
	len = R_xlen_t(slen);
    } else
	len = NA_INTEGER;
    } else {
	len = asInteger(length_out);
	if(len != NA_INTEGER && len < 0)
	    errorcall(call, _("invalid '%s' argument"), "length.out");
    }
    if(Rf_length(length_out) != 1)
	warningcall(call, _("first element used of '%s' argument"),
		    "length.out");

    if (TYPEOF(each_) != INTSXP) {
	double seach = asReal(each_);
	if (R_FINITE(seach)) {
	    if (seach <= -1 || (lx > 0 && seach >= R_XLEN_T_MAX+1.0))
		errorcall(call, _("invalid '%s' argument"), "each");
	    each = lx == 0 ? NA_INTEGER : (R_xlen_t) seach;
	} else each = NA_INTEGER;
    } else {
    each = asInteger(each_);
    if(each != NA_INTEGER && each < 0)
	errorcall(call, _("invalid '%s' argument"), "each");
	}
    if(Rf_length(each_) != 1)
	warningcall(call, _("first element used of '%s' argument"), "each");
    if(each == NA_INTEGER) each = 1;

    if(lx == 0) {
	if(len > 0 && x == R_NilValue)
	    warningcall(call, "'x' is NULL so the result will be NULL");
	SEXP a;
	PROTECT(a = duplicate(x));
	if(len != NA_INTEGER && len > 0) a = xlengthgets(a, len);
	UNPROTECT(3);
	return a;
    }
    if (!isVector(x))
	errorcall(call, "attempt to replicate an object of type '%s'",
		  type2char(TYPEOF(x)));

    /* So now we know x is a vector of positive length.  We need to
       replicate it, and its names if it has them. */

    int nprotect = 2;
    /* First find the final length using 'times' and 'each' */
    if(len != NA_INTEGER) { /* takes precedence over times */
	nt = 1;
    } else {
	double sum = 0;
	if(times == R_MissingArg)
	    PROTECT(times = ScalarInteger(1));
#ifdef LONG_VECTOR_SUPPORT
	else if(TYPEOF(times) != INTSXP)
#else
	else if(TYPEOF(times) == REALSXP)
#endif
	    PROTECT(times = coerceVector(times, REALSXP));
	else PROTECT(times = coerceVector(times, INTSXP));
	nprotect++;
	nt = XLENGTH(times);
	if(nt == 1) {
	    R_xlen_t it;
	    if (TYPEOF(times) == REALSXP) {
		double rt = REAL(times)[0];
		if (ISNAN(rt) || rt <= -1 || rt >= R_XLEN_T_MAX+1.0)
		    errorcall(call, _("invalid '%s' argument"), "times");
		it = (R_xlen_t) rt;
	    } else {
		it = INTEGER(times)[0];
		if (it == NA_INTEGER || it < 0)
		    errorcall(call, _("invalid '%s' argument"), "times");
	    }
	    if ((double) lx * it * each > R_XLEN_T_MAX)
		errorcall(call, _("invalid '%s' argument"), "times");
	    len = lx * it * each;
	} else { // nt != 1
	    if(nt != (double) lx * each)
		errorcall(call, _("invalid '%s' argument"), "times");
	    if (TYPEOF(times) == REALSXP)
	    for(i = 0; i < nt; i++) {
		double rt = REAL(times)[i];
		if (ISNAN(rt) || rt <= -1 || rt >= R_XLEN_T_MAX+1.0)
		    errorcall(call, _("invalid '%s' argument"), "times");
		sum += (R_xlen_t) rt;
	    }
	    else
	    for(i = 0; i < nt; i++) {
		int it = INTEGER(times)[i];
		if (it == NA_INTEGER || it < 0)
		    errorcall(call, _("invalid '%s' argument"), "times");
		sum += it;
	    }
	    if (sum > R_XLEN_T_MAX)
		errorcall(call, _("invalid '%s' argument"), "times");
	    len = (R_xlen_t) sum;
	}
    }
    if(len > 0 && each == 0)
	errorcall(call, _("invalid '%s' argument"), "each");
    SEXP xn = PROTECT(getAttrib(x, R_NamesSymbol));  nprotect++;
    PROTECT(ans = rep4(x, times, len, each, nt));    nprotect++;

    if (Rf_xlength(xn) > 0)
	Rf_setAttrib(ans, R_NamesSymbol, rep4(xn, times, len, each, nt));

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(x)) { /* e.g. contains = "list" */
	setAttrib(ans, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	SET_S4_OBJECT(ans);
    }
#endif
    UNPROTECT(nprotect);
    return ans;
}


/*
  This is a primitive SPECIALSXP with internal argument matching,
  implementing seq.int().

   'along' has to be used on an unevaluated argument, and evalList
   tries to evaluate language objects.
 */
#define FEPS 1e-10
/* to match seq.default */
SEXP attribute_hidden do_seq(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue /* -Wall */, from, to, by, len, along, ignored;
    int nargs = Rf_length(args), lf;
    Rboolean One = Rboolean(nargs == 1);
    R_xlen_t i, lout = NA_INTEGER;

    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
    auto dispatched = Rf_Dispatch(SEXP_downcast<Expression*>(call),
                                  SEXP_downcast<BuiltInFunction*>(op),
                                  arglist,
                                  SEXP_downcast<Environment*>(rho));
    if (dispatched.first)
        return dispatched.second;

    /* This is a primitive and we manage argument matching ourselves.
       We pretend this is
       seq(from, to, by, length.out, along.with, ...)
    */
    static GCRoot<ArgMatcher> matcher = new ArgMatcher(
	{ "from", "to", "by", "length.out", "along.with", "..." });
    matcher->match(arglist,
		   { &from, &to, &by, &len, &along, &ignored });

    Rboolean
	miss_from = Rboolean(from == R_MissingArg),
	miss_to   = Rboolean(to   == R_MissingArg);

    if(One && !miss_from) {
	lf = Rf_length(from);
	if(lf == 1 && (TYPEOF(from) == INTSXP || TYPEOF(from) == REALSXP)) {
	    double rfrom = asReal(from);
	    if (!R_FINITE(rfrom))
		Rf_errorcall(call, _("'%s' must be a finite number"), "from");
	    ans = seq_colon(1.0, rfrom, call);
	}
	else if (lf)
	    ans = seq_colon(1.0, double(lf), call);
	else
	    ans = allocVector(INTSXP, 0);
	goto done;
    }
    if(along != R_MissingArg) {
	lout = XLENGTH(along);
	if(One) {
	    ans = lout ? seq_colon(1.0, double(lout), call) : allocVector(INTSXP, 0);
	    goto done;
	}
    } else if(len != R_MissingArg && len != R_NilValue) {
	double rout = asReal(len);
	if(ISNAN(rout) || rout <= -0.5)
	    errorcall(call, _("'length.out' must be a non-negative number"));
	if(Rf_length(len) != 1)
	    warningcall(call, _("first element used of '%s' argument"),
			"length.out");
	lout = R_xlen_t( ceil(rout));
    }

    if(lout == NA_INTEGER) {
	double rfrom, rto, rby = asReal(by);
	if(miss_from) rfrom = 1.0;
	else {
	    if(Rf_length(from) != 1) errorcall(call, _("'%s' must be of length 1"), "from");
	    rfrom = asReal(from);
	    if(!R_FINITE(rfrom))
		errorcall(call, _("'%s' must be a finite number"), "from");
	}
	if(miss_to) rto = 1.0;
	else {
	    if(Rf_length(to) != 1) errorcall(call, _("'%s' must be of length 1"), "to");
	    rto = asReal(to);
	    if(!R_FINITE(rto))
		errorcall(call, _("'%s' must be a finite number"), "to");
	}
	if(by == R_MissingArg)
	    ans = seq_colon(rfrom, rto, call);
	else {
	    if(Rf_length(by) != 1) errorcall(call, _("'%s' must be of length 1"), "by");
	    double del = rto - rfrom;
	    if(del == 0.0 && rto == 0.0) {
		ans = to; // is *not* missing in this case
		goto done;
	    }
	    /* printf("from = %f, to = %f, by = %f\n", rfrom, rto, rby); */
	    double n = del/rby;
	    if(!R_FINITE(n)) {
		if(del == 0.0 && rby == 0.0) {
		    ans = miss_from ? ScalarReal(rfrom) : from;
		    goto done;
		} else
		    errorcall(call, _("invalid '(to - from)/by'"));
	    }
	    double dd = fabs(del)/fmax2(fabs(rto), fabs(rfrom));
	    if(dd < 100 * DBL_EPSILON) {
		ans = miss_from ? ScalarReal(rfrom) : from;
		goto done;
	    }
#ifdef LONG_VECTOR_SUPPORT
	    if(n > 100 * double( INT_MAX))
#else
	    if(n > double( INT_MAX))
#endif
		errorcall(call, _("'by' argument is much too small"));
	    if(n < - FEPS)
		errorcall(call, _("wrong sign in 'by' argument"));
	    R_xlen_t nn;
	    if((miss_from || TYPEOF(from) == INTSXP) &&
	       (miss_to   || TYPEOF(to)   == INTSXP) &&
	       TYPEOF(by) == INTSXP) {
		int *ia, ifrom = miss_from ? (int)rfrom : asInteger(from),
		    iby = asInteger(by);
		/* With the current limits on integers and FEPS
		   reduced below 1/INT_MAX this is the same as the
		   next, so this is future-proofing against longer integers.
		*/
		/* seq.default gives integer result from
		   from + (0:n)*by
		*/
		nn = R_xlen_t( n);
		ans = allocVector(INTSXP, nn+1);
		ia = INTEGER(ans);
		for(i = 0; i <= nn; i++)
		    ia[i] = int(ifrom + i * iby);
	    } else {
		nn = int(n + FEPS);
		ans = allocVector(REALSXP, nn+1);
		double *ra = REAL(ans);
		for(i = 0; i <= nn; i++)
		    ra[i] = rfrom + double(i) * rby;
		/* Added in 2.9.0 */
		if (nn > 0)
		    if((rby > 0 && ra[nn] > rto) || (rby < 0 && ra[nn] < rto))
			ra[nn] = rto;
	    }
	}
    } else if (lout == 0) {
	ans = allocVector(INTSXP, 0);
    } else if (One) {
	ans = seq_colon(1.0, double(lout), call);
    } else if (by == R_MissingArg) { // and  len := length.out  specified
	double rfrom = asReal(from), rto = asReal(to), rby = 0; // -Wall
	if(miss_to)   rto   = rfrom + (double)lout - 1;
	if(miss_from) rfrom = rto   - (double)lout + 1;
	if(!R_FINITE(rfrom)) errorcall(call, _("'%s' must be a finite number"), "from");
	if(!R_FINITE(rto))   errorcall(call, _("'%s' must be a finite number"), "to");
	if(lout > 2) rby = (rto - rfrom)/(double)(lout - 1);
	if(rfrom == (int)rfrom &&
	   (lout <= 1 || rto == (int)rto) &&
	   (lout <= 2 || rby == (int)rby) &&
	   rfrom <= INT_MAX && rfrom >= INT_MIN &&
	   rto   <= INT_MAX && rto   >= INT_MIN) {
	    ans = allocVector(INTSXP, lout);
	    if(lout > 0) INTEGER(ans)[0] = (int)rfrom;
	    if(lout > 1) INTEGER(ans)[lout - 1] = (int)rto;
	    if(lout > 2)
		for(i = 1; i < lout-1; i++) {
//		    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		    INTEGER(ans)[i] = (int)(rfrom + (double)i*rby);
		}
	} else {
	ans = allocVector(REALSXP, lout);
	if(lout > 0) REAL(ans)[0] = rfrom;
	if(lout > 1) REAL(ans)[lout - 1] = rto;
	if(lout > 2) {
	    rby = (rto - rfrom)/double(lout - 1);
	    for(i = 1; i < lout-1; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = rfrom + double(i)*rby;
	    }
	}
	}
    } else if (miss_to) {
	double rfrom = asReal(from), rby = asReal(by), rto;
	if(miss_from) rfrom = 1.0;
	if(!R_FINITE(rfrom)) errorcall(call, _("'%s' must be a finite number"), "from");
	if(!R_FINITE(rby))   errorcall(call, _("'%s' must be a finite number"), "by");
	rto = rfrom + double(lout-1)*rby;
	if(rby == int(rby) && rfrom <= INT_MAX && rfrom >= INT_MIN
	   && rto <= INT_MAX && rto >= INT_MIN) {
	    ans = allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = int(rfrom + double(i)*rby);
	    }
	} else {
	    ans = allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = rfrom + double(i)*rby;
	    }
	}
    } else if (miss_from) {
	double rto = asReal(to), rby = asReal(by),
	    rfrom = rto - double(lout-1)*rby;
	if(!R_FINITE(rto)) errorcall(call, _("'%s' must be a finite number"), "to");
	if(!R_FINITE(rby)) errorcall(call, _("'%s' must be a finite number"), "by");
	if(rby == int(rby) && rfrom <= INT_MAX && rfrom >= INT_MIN
	   && rto <= INT_MAX && rto >= INT_MIN) {
	    ans = allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = int(rto - double(lout - 1 - i)*rby);
	    }
	} else {
	    ans = allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = rto - double(lout - 1 - i)*rby;
	    }
	}
    } else
	errorcall(call, _("too many arguments"));

done:
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_seq_along(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP ans;

    static BuiltInFunction* length_op = BuiltInFunction::obtainPrimitive(
	"length");
    // The arguments have already been evaluated, so call do_length directly.
    RObject* length = do_length(call, length_op, rho, args, num_args, tags);
    R_xlen_t len = length->sexptype() == INTSXP ?
	INTEGER(length)[0] : REAL(length)[0];

#ifdef LONG_VECTOR_SUPPORT
    if (len > INT_MAX) {
	ans = allocVector(REALSXP, len);
	double *p = REAL(ans);
	for(R_xlen_t i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    p[i] = double( (i+1));
	}
    } else
#endif
    {
	ans = allocVector(INTSXP, len);
	int *p = INTEGER(ans);
	for(int i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    p[i] = i+1;
	}
    }
    return ans;
}

SEXP attribute_hidden do_seq_len(/*const*/ Expression* call, const BuiltInFunction* op, RObject* length_)
{
    SEXP ans;
    R_xlen_t len;

    if(Rf_length(length_) != 1)
	warningcall(call, _("first element used of '%s' argument"),
		    "length.out");

 #ifdef LONG_VECTOR_SUPPORT
    double dlen = asReal(length_);
    if(!R_FINITE(dlen) || dlen < 0)
	errorcall(call, _("argument must be coercible to non-negative integer"));
    len = R_xlen_t( dlen);
#else
    len = asInteger(length_);
    if(len == NA_INTEGER || len < 0)
	errorcall(call, _("argument must be coercible to non-negative integer"));
#endif

 #ifdef LONG_VECTOR_SUPPORT
    if (len > INT_MAX) {
	ans = allocVector(REALSXP, len);
	double *p = REAL(ans);
	for(R_xlen_t i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    p[i] = double( (i+1));
	}
    } else
#endif
    {
	ans = allocVector(INTSXP, len);
	int *p = INTEGER(ans);
	for(int i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    p[i] = i+1;
	}
    }
    return ans;
}
