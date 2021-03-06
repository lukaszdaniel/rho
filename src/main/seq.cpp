/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2017  The R Core Team.
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
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

#define R_NO_REMAP

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
#include <rho/ArgMatcher.hpp>
#include <rho/ExpressionVector.hpp>
#include <rho/GCStackRoot.hpp>
#include <rho/unrho.hpp>

using namespace rho;

static R_StringBuffer cbuff = { nullptr, 0, MAXELTSIZE };

#define _S4_rep_keepClass
/* ==>  rep(<S4>, .) keeps class e.g., for list-like */

static SEXP cross_colon(SEXP call, SEXP s, SEXP t)
{
    SEXP a, la, ls, lt, rs, rt;
    int i, j, k, n, nls, nlt;
    char* cbuf;
    const void* vmax = vmaxget();

    if (Rf_length(s) != Rf_length(t))
	Rf_errorcall(call, _("unequal factor lengths"));
    n = Rf_length(s);
    ls = Rf_getAttrib(s, Symbols::LevelsSymbol);
    lt = Rf_getAttrib(t, Symbols::LevelsSymbol);
    nls = LENGTH(ls);
    nlt = LENGTH(lt);
    PROTECT(a = Rf_allocVector(INTSXP, n));
    PROTECT(rs = Rf_coerceVector(s, INTSXP));
    PROTECT(rt = Rf_coerceVector(t, INTSXP));
    for (i = 0; i < n; i++) {
	int vs = INTEGER(rs)[i];
	int vt = INTEGER(rt)[i];
	if ((vs == R_NaInt) || (vt == R_NaInt))
	    INTEGER(a)[i] = R_NaInt;
	else
	    INTEGER(a)[i] = vt + (vs - 1) * nlt;
    }
    UNPROTECT(2);
    if (!Rf_isNull(ls) && !Rf_isNull(lt)) {
	PROTECT(la = Rf_allocVector(STRSXP, nls * nlt));
	k = 0;
	/* FIXME: possibly UTF-8 version */
	for (i = 0; i < nls; i++) {
	    const char *vi = Rf_translateChar(STRING_ELT(ls, i));
	    size_t vs = strlen(vi);
	    for (j = 0; j < nlt; j++) {
		const char *vj = Rf_translateChar(STRING_ELT(lt, j));
		size_t vt = strlen(vj), len = vs + vt + 2;
		cbuf = static_cast<char*>(R_AllocStringBuffer(len, &cbuff));
		snprintf(cbuf, len, "%s:%s", vi, vj);
		SET_STRING_ELT(la, k, Rf_mkChar(cbuf));
		k++;
	    }
	}
	Rf_setAttrib(a, Symbols::LevelsSymbol, la);
	UNPROTECT(1);
    }
    PROTECT(la = Rf_mkString("factor"));
    Rf_setAttrib(a, Symbols::ClassSymbol, la);
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    vmaxset(vmax);
    return a;
}

/* interval at which to check interrupts */
// constexpr R_xlen_t NINTERRUPT = 1000000;

static SEXP seq_colon(double n1, double n2, SEXP call)
{
    double r = std::abs(n2 - n1);
    if(r >= double(R_XLEN_T_MAX))
	Rf_errorcall(call, _("result would be too long a vector"));

    if (RHO_FALSE && n1 == (R_xlen_t) n1 && n2 == (R_xlen_t) n2)
	return R_compact_intrange((R_xlen_t) n1, (R_xlen_t) n2);

    SEXP ans;
    R_xlen_t n = R_xlen_t(r + 1 + FLT_EPSILON);

    Rboolean useInt = Rboolean((n1 <= INT_MAX) &&  (n1 == (int) n1));
    if(useInt) {
	if(n1 <= INT_MIN || n1 > INT_MAX)
	    useInt = FALSE;
	else {
	    /* r := " the effective 'to' "  of  from:to */
	    double dn = double(n);
	    r = n1 + ((n1 <= n2) ? dn-1 : -(dn-1));
	    if(r <= INT_MIN || r > INT_MAX) useInt = FALSE;
	}
    }
    if (useInt) {
	int in1 = int(n1);
	ans = Rf_allocVector(INTSXP, n);
	if (n1 <= n2)
	    //ans = R_compact_intrange((R_xlen_t) n1, (R_xlen_t)(n1 + n - 1));
	    for (int i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = in1 + i;
	    }
	else
	    //ans = R_compact_intrange((R_xlen_t) n1, (R_xlen_t)(n1 - n + 1));
	    for (int i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = in1 - i;
	    }
    } else {
	ans = Rf_allocVector(REALSXP, n);
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

HIDDEN SEXP do_colon(/*const*/ Expression* call, const BuiltInFunction* op, RObject* from_, RObject* to_)
{
    SEXP s1, s2;
    double n1, n2;

    if (Rf_inherits(from_, "factor") && Rf_inherits(to_, "factor"))
	return cross_colon(call, from_, to_);

    s1 = from_;
    s2 = to_;
    n1 = Rf_length(s1);
    n2 = Rf_length(s2);
    if (n1 == 0 || n2 == 0)
	Rf_errorcall(call, _("argument of length 0"));
    if (n1 > 1)
	Rf_warningcall(call,
		    n_("numerical expression has %d element: only the first used",
			     "numerical expression has %d elements: only the first used",
			     (int) n1), (int) n1);
    if (n2 > 1)
	Rf_warningcall(call,
		    n_("numerical expression has %d element: only the first used",
			     "numerical expression has %d elements: only the first used",
			     (int) n2), (int) n2);
    n1 = Rf_asReal(s1);
    n2 = Rf_asReal(s2);
    if (std::isnan(n1) || std::isnan(n2))
	Rf_errorcall(call, _("NA/NaN argument"));
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
	    SEXP elt = Rf_lazy_duplicate(VECTOR_ELT(s, i)); \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		SET_VECTOR_ELT(a, n++, elt); \
	    if (j > 1) ENSURE_NAMEDMAX(elt); \
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
    PROTECT(t = Rf_coerceVector(ncopy, REALSXP));
    else
    PROTECT(t = Rf_coerceVector(ncopy, INTSXP));

    nc = Rf_xlength(ncopy);
    double sna = 0;
    if (TYPEOF(t) == REALSXP)
    for (i = 0; i < nc; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if (std::isnan(REAL(t)[i]) || REAL(t)[i] <= -1 ||
	    REAL(t)[i] >= double(R_XLEN_T_MAX) + 1.0)
	    Rf_error(_("invalid '%s' value"), "times");
	sna += (R_xlen_t) REAL(t)[i];
    }
    else
    for (i = 0; i < nc; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if (INTEGER(t)[i] == R_NaInt || INTEGER(t)[i] < 0)
	    Rf_error(_("invalid '%s' value"), "times");
	sna += INTEGER(t)[i];
    }
    if (sna > double(R_XLEN_T_MAX))
	Rf_error(_("invalid '%s' value"), "times");
    R_xlen_t na = (R_xlen_t) sna;
/*    R_xlen_t ni = NINTERRUPT, ratio;
    if(nc > 0) {
	ratio = na/nc; // average no of replications
	if (ratio > 1000U) ni = 1000U;
	} */
    PROTECT(a = Rf_allocVector(TYPEOF(s), na));
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

    PROTECT(a = Rf_allocVector(TYPEOF(s), na));

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
	    SET_VECTOR_ELT(a, i, Rf_lazy_duplicate(VECTOR_ELT(s, j)));
	});
	break;
    default:
	UNIMPLEMENTED_TYPE("rep3", s);
    }
    UNPROTECT(1);
    return a;
}

// .Internal(rep.int(x, times))
HIDDEN SEXP do_rep_int(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* times_)
{
    SEXP s = x_, ncopy = times_;
    R_xlen_t nc;
    SEXP a;

    if (!Rf_isVector(ncopy))
	Rf_error(_("invalid type (%s) for '%s' (must be a vector)"),
	      Rf_type2char(TYPEOF(ncopy)), "times");

    if (!Rf_isVector(s) && s != nullptr)
	Rf_error(_("attempt to replicate an object of type '%s'"),
	      Rf_type2char(TYPEOF(s)));

    nc = Rf_xlength(ncopy); // might be 0
    if (nc == Rf_xlength(s))
	PROTECT(a = rep2(s, ncopy));
    else {
	if (nc != 1) Rf_error(_("invalid '%s' value"), "times");

	R_xlen_t ns = Rf_xlength(s);
	if (TYPEOF(ncopy) != INTSXP) {
	    double snc = Rf_asReal(ncopy);
	    if (!std::isfinite(snc) || snc <= -1. ||
		(ns > 0 && snc >= double(R_XLEN_T_MAX) + 1.0))
		Rf_error(_("invalid '%s' value"), "times");
	    nc = ns == 0 ? 1 : (R_xlen_t) snc;
	} else if ((nc = Rf_asInteger(ncopy)) == R_NaInt || nc < 0) // nc = 0 ok
	    Rf_error(_("invalid '%s' value"), "times");
	if ((double) nc * ns > double(R_XLEN_T_MAX))
	    Rf_error(_("invalid '%s' value"), "times");
	PROTECT(a = rep3(s, ns, nc * ns));
    }

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(s)) { /* e.g. contains = "list" */
	Rf_setAttrib(a, Symbols::ClassSymbol, Rf_getAttrib(s, Symbols::ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    if (Rf_inherits(s, "factor")) {
	SEXP tmp;
	if(Rf_inherits(s, "ordered")) {
	    PROTECT(tmp = Rf_allocVector(STRSXP, 2));
	    SET_STRING_ELT(tmp, 0, Rf_mkChar("ordered"));
	    SET_STRING_ELT(tmp, 1, Rf_mkChar("factor"));
	} else PROTECT(tmp = Rf_mkString("factor"));
	Rf_setAttrib(a, Symbols::ClassSymbol, tmp);
	UNPROTECT(1);
	Rf_setAttrib(a, Symbols::LevelsSymbol, Rf_getAttrib(s, Symbols::LevelsSymbol));
    }
    UNPROTECT(1);
    return a;
}

HIDDEN SEXP do_rep_len(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* length_out_)
{
    R_xlen_t ns, na;
    SEXP a, s, len;

    s = x_;

    if (!Rf_isVector(s) && s != nullptr)
	Rf_error(_("attempt to replicate non-vector"));

    len = length_out_;
    if(Rf_length(len) != 1)
	Rf_error(_("invalid '%s' value"), "length.out");
    if (TYPEOF(len) != INTSXP) {
	double sna = Rf_asReal(len);
	if (std::isnan(sna) || sna <= -1. || sna >= double(R_XLEN_T_MAX) + 1.0)
	    Rf_error(_("invalid '%s' value"), "length.out");
	na = (R_xlen_t) sna;
    } else
	if ((na = Rf_asInteger(len)) == R_NaInt || na < 0) /* na = 0 ok */
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
	Rf_setAttrib(a, Symbols::ClassSymbol, Rf_getAttrib(s, Symbols::ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    if (Rf_inherits(s, "factor")) {
	SEXP tmp;
	if(Rf_inherits(s, "ordered")) {
	    PROTECT(tmp = Rf_allocVector(STRSXP, 2));
	    SET_STRING_ELT(tmp, 0, Rf_mkChar("ordered"));
	    SET_STRING_ELT(tmp, 1, Rf_mkChar("factor"));
	} else PROTECT(tmp = Rf_mkString("factor"));
	Rf_setAttrib(a, Symbols::ClassSymbol, tmp);
	UNPROTECT(1);
	Rf_setAttrib(a, Symbols::LevelsSymbol, Rf_getAttrib(s, Symbols::LevelsSymbol));
    }
    UNPROTECT(1);
    return a;
}

/* rep(), allowing for both times and each ;
 * -----  nt == length(times) ;  if (nt == 1)  'times' is *not* accessed  */
static SEXP rep4(SEXP x, SEXP times, R_xlen_t len, R_xlen_t each, R_xlen_t nt)
{
    SEXP a;
    R_xlen_t lx = Rf_xlength(x);
    R_xlen_t i, j, k, k2, k3, sum;

    // faster code for common special case
    if (each == 1 && nt == 1) return rep3(x, lx, len);

    PROTECT(a = Rf_allocVector(TYPEOF(x), len));

#define R4_SWITCH_LOOP(itimes)						\
    switch (TYPEOF(x)) {						\
    case LGLSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		LOGICAL(a)[k2++] = LOGICAL(x)[i];			\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case INTSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		INTEGER(a)[k2++] = INTEGER(x)[i];			\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case REALSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		REAL(a)[k2++] = REAL(x)[i];				\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case CPLXSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		COMPLEX(a)[k2++] = COMPLEX(x)[i];			\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case STRSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		SET_STRING_ELT(a, k2++, STRING_ELT(x, i));		\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case VECSXP:							\
    case EXPRSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		SET_VECTOR_ELT(a, k2++, VECTOR_ELT(x, i));		\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case RAWSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		RAW(a)[k2++] = RAW(x)[i];				\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    default:								\
	UNIMPLEMENTED_TYPE("rep4", x);					\
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
HIDDEN SEXP do_rep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, times, length_out, each_, ignored;
    R_xlen_t i, lx, len = R_NaInt, each = 1, nt;
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
	Rf_errorcall(call, "replication of pairlists is defunct");

    lx = Rf_xlength(x);

    if (TYPEOF(length_out) != INTSXP) {
	double slen = Rf_asReal(length_out);
	if (std::isfinite(slen)) {
	    if (slen <= -1 || slen >= double(R_XLEN_T_MAX) + 1.0)
		Rf_errorcall(call, _("invalid '%s' argument"), "length.out");
	    len = R_xlen_t(slen);
	} else
	    len = R_NaInt;
    } else {
	len = Rf_asInteger(length_out);
	if(len != R_NaInt && len < 0)
	    Rf_errorcall(call, _("invalid '%s' argument"), "length.out");
    }
    if(Rf_length(length_out) != 1)
	Rf_warningcall(call, _("first element used of '%s' argument"),
		    "length.out");

    if (TYPEOF(each_) != INTSXP) {
	double seach = Rf_asReal(each_);
	if (std::isfinite(seach)) {
	    if (seach <= -1. || (lx > 0 && seach >= double(R_XLEN_T_MAX) + 1.0))
		Rf_errorcall(call, _("invalid '%s' argument"), "each");
	    each = lx == 0 ? R_NaInt : (R_xlen_t) seach;
	} else each = R_NaInt;
    } else {
	each = Rf_asInteger(each_);
	if(each != R_NaInt && each < 0)
	    Rf_errorcall(call, _("invalid '%s' argument"), "each");
    }
    if(Rf_length(each_) != 1)
	Rf_warningcall(call, _("first element used of '%s' argument"), "each");
    if(each == R_NaInt) each = 1;

    if(lx == 0) {
	if(len > 0 && x == nullptr)
	    Rf_warningcall(call, "'x' is NULL so the result will be NULL");
	SEXP a;
	PROTECT(a = Rf_duplicate(x));
	if(len != R_NaInt && len > 0 && x != nullptr)
	    a = Rf_xlengthgets(a, len);
	UNPROTECT(3);
	return a;
    }
    if (!Rf_isVector(x))
	Rf_errorcall(call, "attempt to replicate an object of type '%s'",
		  Rf_type2char(TYPEOF(x)));

    /* So now we know x is a vector of positive length.  We need to
       replicate it, and its names if it has them. */

    int nprotect = 2;
    /* First find the final length using 'times' and 'each' */
    if(len != R_NaInt) { /* takes precedence over times */
	nt = 1;
    } else {
	double sum = 0;
	if(times == R_MissingArg)
	    PROTECT(times = Rf_ScalarInteger(1));
#ifdef LONG_VECTOR_SUPPORT
	else if(TYPEOF(times) != INTSXP)
#else
	else if(TYPEOF(times) == REALSXP)
#endif
	    PROTECT(times = Rf_coerceVector(times, REALSXP));
	else PROTECT(times = Rf_coerceVector(times, INTSXP));
	nprotect++;
	nt = XLENGTH(times);
	if(nt == 1) {
	    R_xlen_t it;
	    if (TYPEOF(times) == REALSXP) {
		double rt = REAL(times)[0];
		if (std::isnan(rt) || rt <= -1 || rt >= double(R_XLEN_T_MAX) + 1.0)
		    Rf_errorcall(call, _("invalid '%s' argument"), "times");
		it = (R_xlen_t) rt;
	    } else {
		it = INTEGER(times)[0];
		if (it == R_NaInt || it < 0)
		    Rf_errorcall(call, _("invalid '%s' argument"), "times");
	    }
	    if ((double) lx * it * each > double(R_XLEN_T_MAX))
		Rf_errorcall(call, _("invalid '%s' argument"), "times");
	    len = lx * it * each;
	} else { // nt != 1
	    if(nt != (double) lx * each)
		Rf_errorcall(call, _("invalid '%s' argument"), "times");
	    if (TYPEOF(times) == REALSXP)
		for(i = 0; i < nt; i++) {
		    double rt = REAL(times)[i];
		    if (std::isnan(rt) || rt <= -1 || rt >= double(R_XLEN_T_MAX) + 1.0)
			Rf_errorcall(call, _("invalid '%s' argument"), "times");
		    sum += (R_xlen_t) rt;
		}
	    else
		for(i = 0; i < nt; i++) {
		    int it = INTEGER(times)[i];
		    if (it == R_NaInt || it < 0)
			Rf_errorcall(call, _("invalid '%s' argument"), "times");
		    sum += it;
		}
	    if (sum > double(R_XLEN_T_MAX))
		Rf_errorcall(call, _("invalid '%s' argument"), "times");
	    len = (R_xlen_t) sum;
	}
    }

    if(len > 0 && each == 0)
	Rf_errorcall(call, _("invalid '%s' argument"), "each");
    SEXP xn = PROTECT(Rf_getAttrib(x, Symbols::NamesSymbol));  nprotect++;
    PROTECT(ans = rep4(x, times, len, each, nt));    nprotect++;

    if (Rf_xlength(xn) > 0)
	Rf_setAttrib(ans, Symbols::NamesSymbol, rep4(xn, times, len, each, nt));

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(x)) { /* e.g. contains = "list" */
	Rf_setAttrib(ans, Symbols::ClassSymbol, Rf_getAttrib(x, Symbols::ClassSymbol));
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
HIDDEN SEXP do_seq(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = nullptr /* -Wall */, from, to, by, len, along, ignored;
    int nargs = Rf_length(args), lf;
    Rboolean One = Rboolean(nargs == 1);
    R_xlen_t i, lout = R_NaInt;

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
	    double rfrom = Rf_asReal(from);
	    if (!std::isfinite(rfrom))
		Rf_errorcall(call, _("'%s' must be a finite number"), "from");
	    ans = seq_colon(1.0, rfrom, call);
	}
	else if (lf)
	    ans = seq_colon(1.0, double(lf), call);
	else
	    ans = Rf_allocVector(INTSXP, 0);
	goto done;
    }
    if(along != R_MissingArg) {
	lout = XLENGTH(along);
	if(One) {
	    ans = lout ? seq_colon(1.0, double(lout), call) : Rf_allocVector(INTSXP, 0);
	    goto done;
	}
    } else if(len != R_MissingArg && len != nullptr) {
	double rout = Rf_asReal(len);
	if(std::isnan(rout) || rout <= -0.5)
	    Rf_errorcall(call, _("'length.out' must be a non-negative number"));
	if(Rf_length(len) != 1)
	    Rf_warningcall(call, _("first element used of '%s' argument"),
			"length.out");
	lout = R_xlen_t(ceil(rout));
    }

    if(lout == R_NaInt) {
	double rfrom, rto, rby = Rf_asReal(by);
	if(miss_from) rfrom = 1.0;
	else {
	    if(Rf_length(from) != 1) Rf_errorcall(call, _("'%s' must be of length 1"), "from");
	    rfrom = Rf_asReal(from);
	    if(!std::isfinite(rfrom))
		Rf_errorcall(call, _("'%s' must be a finite number"), "from");
	}
	if(miss_to) rto = 1.0;
	else {
	    if(Rf_length(to) != 1) Rf_errorcall(call, _("'%s' must be of length 1"), "to");
	    rto = Rf_asReal(to);
	    if(!std::isfinite(rto))
		Rf_errorcall(call, _("'%s' must be a finite number"), "to");
	}
	if(by == R_MissingArg)
	    ans = seq_colon(rfrom, rto, call);
	else {
	    if(Rf_length(by) != 1) Rf_errorcall(call, _("'%s' must be of length 1"), "by");
	    double del = rto - rfrom;
	    if(del == 0.0 && rto == 0.0) {
		ans = to; // is *not* missing in this case
		goto done;
	    }
	    /* printf("from = %f, to = %f, by = %f\n", rfrom, rto, rby); */
	    double n = del/rby;
	    if(!std::isfinite(n)) {
		if(del == 0.0 && rby == 0.0) {
		    ans = miss_from ? Rf_ScalarReal(rfrom) : from;
		    goto done;
		} else
		    Rf_errorcall(call, _("invalid '(to - from)/by'"));
	    }
	    double dd = std::abs(del)/std::max(std::abs(rto), std::abs(rfrom));
	    if(dd < 100 * DBL_EPSILON) {
		ans = miss_from ? Rf_ScalarReal(rfrom) : from;
		goto done;
	    }
#ifdef LONG_VECTOR_SUPPORT
	    if(n > 100 * double(INT_MAX))
#else
	    if(n > double(INT_MAX))
#endif
		Rf_errorcall(call, _("'by' argument is much too small"));
	    if(n < - FEPS)
		Rf_errorcall(call, _("wrong sign in 'by' argument"));
	    R_xlen_t nn;
	    if((miss_from || TYPEOF(from) == INTSXP) &&
	       (miss_to   || TYPEOF(to)   == INTSXP) &&
	       TYPEOF(by) == INTSXP) {
		int *ia, ifrom = miss_from ? (int)rfrom : Rf_asInteger(from),
		    iby = Rf_asInteger(by);
		/* With the current limits on integers and FEPS
		   reduced below 1/INT_MAX this is the same as the
		   next, so this is future-proofing against longer integers.
		*/
		/* seq.default gives integer result from
		   from + (0:n)*by
		*/
		nn = R_xlen_t(n);
		ans = Rf_allocVector(INTSXP, nn+1);
		ia = INTEGER(ans);
		for(i = 0; i <= nn; i++)
		    ia[i] = int(ifrom + i * iby);
	    } else {
		nn = int(n + FEPS);
		ans = Rf_allocVector(REALSXP, nn+1);
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
	ans = Rf_allocVector(INTSXP, 0);
    } else if (One) {
	ans = seq_colon(1.0, double(lout), call);
    } else if (by == R_MissingArg) { // and  len := length.out  specified
	double rfrom = Rf_asReal(from), rto = Rf_asReal(to), rby = 0; // -Wall
	if(miss_to)   rto   = rfrom + (double)lout - 1;
	if(miss_from) rfrom = rto   - (double)lout + 1;
	if(!std::isfinite(rfrom)) Rf_errorcall(call, _("'%s' must be a finite number"), "from");
	if(!std::isfinite(rto))   Rf_errorcall(call, _("'%s' must be a finite number"), "to");
	if(lout > 2) rby = (rto - rfrom)/(double)(lout - 1);
	if(rfrom <= INT_MAX && rfrom >= INT_MIN &&
	   rto   <= INT_MAX && rto   >= INT_MIN &&
	   rfrom == (int)rfrom &&
	   (lout <= 1 || rto == (int)rto) &&
	   (lout <= 2 || rby == (int)rby)) {
	    ans = Rf_allocVector(INTSXP, lout);
	    if(lout > 0) INTEGER(ans)[0] = (int)rfrom;
	    if(lout > 1) INTEGER(ans)[lout - 1] = (int)rto;
	    if(lout > 2)
		for(i = 1; i < lout-1; i++) {
//		    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		    INTEGER(ans)[i] = (int)(rfrom + (double)i*rby);
		}
	} else {
	ans = Rf_allocVector(REALSXP, lout);
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
	double rfrom = Rf_asReal(from), rby = Rf_asReal(by), rto;
	if(miss_from) rfrom = 1.0;
	if(!std::isfinite(rfrom)) Rf_errorcall(call, _("'%s' must be a finite number"), "from");
	if(!std::isfinite(rby))   Rf_errorcall(call, _("'%s' must be a finite number"), "by");
	rto = rfrom + double(lout-1)*rby;
	if(rby == int(rby) && rfrom <= INT_MAX && rfrom >= INT_MIN
	   && rto <= INT_MAX && rto >= INT_MIN) {
	    ans = Rf_allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = int(rfrom + double(i)*rby);
	    }
	} else {
	    ans = Rf_allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = rfrom + double(i)*rby;
	    }
	}
    } else if (miss_from) {
	double rto = Rf_asReal(to), rby = Rf_asReal(by),
	    rfrom = rto - double(lout-1)*rby;
	if(!std::isfinite(rto)) Rf_errorcall(call, _("'%s' must be a finite number"), "to");
	if(!std::isfinite(rby)) Rf_errorcall(call, _("'%s' must be a finite number"), "by");
	if(rby == int(rby) && rfrom <= INT_MAX && rfrom >= INT_MIN
	   && rto <= INT_MAX && rto >= INT_MIN) {
	    ans = Rf_allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = int(rto - double(lout - 1 - i)*rby);
	    }
	} else {
	    ans = Rf_allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = rto - double(lout - 1 - i)*rby;
	    }
	}
    } else
	Rf_errorcall(call, _("too many arguments"));

done:
    UNPROTECT(1);
    return ans;
}

HIDDEN SEXP do_seq_along(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
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
	ans = Rf_allocVector(REALSXP, len);
	double *p = REAL(ans);
	for(R_xlen_t i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    p[i] = double((i+1));
	}
    } else
#endif
    {
	ans = Rf_allocVector(INTSXP, len);
	int *p = INTEGER(ans);
	for(int i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    p[i] = i+1;
	}
    }
    return ans;
}

HIDDEN SEXP do_seq_len(/*const*/ Expression* call, const BuiltInFunction* op, RObject* length_)
{
    SEXP ans;
    R_xlen_t len;

    if(Rf_length(length_) != 1)
	Rf_warningcall(call, _("first element used of '%s' argument"),
		    "length.out");

 #ifdef LONG_VECTOR_SUPPORT
    double dlen = Rf_asReal(length_);
    if(!std::isfinite(dlen) || dlen < 0)
	Rf_errorcall(call, _("argument must be coercible to non-negative integer"));
    len = R_xlen_t(dlen);
#else
    len = Rf_asInteger(length_);
    if(len == R_NaInt || len < 0)
	Rf_errorcall(call, _("argument must be coercible to non-negative integer"));
#endif

 #ifdef LONG_VECTOR_SUPPORT
    if (len > INT_MAX) {
	ans = Rf_allocVector(REALSXP, len);
	double *p = REAL(ans);
	for(R_xlen_t i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    p[i] = double((i+1));
	}
    } else
#endif
    {
	ans = Rf_allocVector(INTSXP, len);
	int *p = INTEGER(ans);
	for(int i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    p[i] = i+1;
	}
    }
    return ans;
}
