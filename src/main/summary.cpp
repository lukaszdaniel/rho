/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2018  The R Core Team
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

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include "duplicate.h"

#include <rho/Closure.hpp>
#include <rho/GCStackRoot.hpp>
#include <R_ext/Itermacros.h>

using namespace rho;

#include <float.h> // for DBL_MAX

#define R_MSG_type	_("invalid 'type' (%s) of argument")

#define R_INT_MIN	(1+INT_MIN)
	/* since INT_MIN is the R_NaInt value ! */
#define Int2Real(i)	((i == R_NaInt) ? R_NaReal : double(i))

#ifdef DEBUG_sum
#define DbgP1(s) REprintf(s)
#define DbgP2(s,a) REprintf(s,a)
#define DbgP3(s,a,b) REprintf(s,a,b)
#else
#define DbgP1(s)
#define DbgP2(s,a)
#define DbgP3(s,a,b)
#endif

#ifdef LONG_INT
# define isum_INT LONG_INT
static int isum(SEXP sx, isum_INT *value, Rboolean narm, SEXP call)
{
    LONG_INT s = 0;  // at least 64-bit
    int updated = 0;
#ifdef LONG_VECTOR_SUPPORT
    int ii = R_INT_MIN; // need > 2^32 entries to overflow; checking earlier is a waste
/* NOTE: cannot use 64-bit *value to pass R_NaInt: that is "regular" 64bit int
 *      -> pass the NA information via return value ('updated').
 * After the first 2^32 entries, only check every 1000th time (related to GET_REGION_BUFSIZE=512 ?)
 * Assume LONG_INT_MAX >= 2^63-1 >=~ 9.223e18 >  (1000 * 9000..0L = 9 * 10^18)
 */
# define ISUM_OVERFLOW_CHECK do {					\
	if (ii++ > 1000) {						\
	    if (s > 9000000000000000L || s < -9000000000000000L) {	\
		DbgP2("|OVERFLOW triggered: s=%ld|", s);		\
		/* *value = s; no use, TODO continue from 'k' */	\
		return 42; /* was overflow, NA; now switch to irsum()*/ \
	    }								\
	    ii = 0;							\
	}								\
    } while (0)
#else
# define ISUM_OVERFLOW_CHECK do { } while(0)
#endif

    /**** assumes INTEGER(sx) and LOGICAL(sx) are identical!! */
    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (int k = 0; k < nbatch; k++) {
		if (x[k] != R_NaInt) {
		    if(!updated) updated = 1;
		    s += x[k];
		    ISUM_OVERFLOW_CHECK;
		} else if (!narm) {
		    // updated = R_NaInt;
		    return R_NaInt;
		}
	    }
	});
    *value = s;
    return updated;
#undef ISUM_OVERFLOW_CHECK
}
#else // no LONG_INT  : should never be used with a C99/C11 compiler
# define isum_INT int
static Rboolean isum(SEXP sx, isum_INT *value, Rboolean narm, SEXP call)
/* Version from R 3.0.0 */
{
    double s = 0.0;
    Rboolean updated = FALSE;

    /**** assumes INTEGER(sx) and LOGICAL(sx) are identical!! */
    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (int k = 0; k < nbatch; k++) {
		if (x[k] != R_NaInt) {
		    if(!updated) updated = TRUE;
		    s += x[k];
		} else if (!narm) {
		    if(!updated) updated = TRUE;
		    *value = R_NaInt;
		    return updated;
		}
	    }
	});
    if(s > INT_MAX || s < R_INT_MIN){
	Rf_warningcall(call, _("integer overflow - use sum(as.numeric(.))"));
	*value = R_NaInt;
    }
    else *value = int(s);

    return updated;
}
#endif

// Used instead of isum() for large vectors when overflow would occur:
static Rboolean risum(SEXP sx, double *value, Rboolean narm)
{
    LDOUBLE s = 0.0;
    Rboolean updated = FALSE;

    /**** assumes INTEGER(sx) and LOGICAL(sx) are identical!! */
    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (x[k] != R_NaInt) {
		    if(!updated) updated = TRUE;
		    s += (double) x[k];
		} else if (!narm) {
		    if(!updated) updated = TRUE;
		    *value = R_NaReal;
		    return updated;
		}
	    }
	});
    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}


static Rboolean rsum(SEXP sx, double *value, Rboolean narm)
{
    LDOUBLE s = 0.0;
    Rboolean updated = FALSE;

    ITERATE_BY_REGION(sx, x, i, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (!narm || !std::isnan(x[k])) {
		    if(!updated) updated = TRUE;
		    s += x[k];
		}
	    }
	});
    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}

static Rboolean csum(SEXP sx, Rcomplex *value, Rboolean narm)
{
    Rcomplex *x = COMPLEX(sx);
    R_xlen_t n = XLENGTH(sx);
    LDOUBLE sr = 0.0, si = 0.0;
    Rboolean updated = FALSE;

    for (R_xlen_t k = 0; k < n; k++) {
	if (!narm || (!std::isnan(x[k].r) && !std::isnan(x[k].i))) {
	    if(!updated) updated = TRUE;
	    sr += x[k].r;
	    si += x[k].i;
	}
    }
    value->r = double(sr);
    value->i = double(si);

    return updated;
}

static Rboolean imin(SEXP sx, int *value, Rboolean narm)
{
    Rboolean updated = FALSE;
    int s = 0;

    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (int k = 0; k < nbatch; k++) {
		if (x[k] != R_NaInt) {
		    if (!updated || s > x[k]) {
			s = x[k];
			if(!updated) updated = TRUE;
		    }
		}
		else if (!narm) {
		    *value = R_NaInt;
		    return(TRUE);
		}
	    }
	});
    *value = s;
    return updated;
}

static Rboolean rmin(SEXP sx, double *value, Rboolean narm)
{
    double s = 0.0; /* -Wall */
    Rboolean updated = FALSE;

    /* s = R_PosInf; */
    ITERATE_BY_REGION(sx, x, i, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (std::isnan(x[k])) {/* Na(N) */
		    if (!narm) {
			if(!ISNA(s)) s = x[k]; /* so any NA trumps all NaNs */
			if(!updated) updated = TRUE;
		    }
		}
		else if (!updated || x[k] < s) { /* Never true if s is NA/NaN */
		    s = x[k];
		    if(!updated) updated = TRUE;
		}
	    }
	});
    *value = s;
    return updated;
}

static Rboolean smin(SEXP x, SEXP *value, Rboolean narm)
{
    SEXP s = R_NaString; /* -Wall */
    Rboolean updated = FALSE;
    const void *vmax = vmaxget(); // precautionary for Scollate

    for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
	if (STRING_ELT(x, i) != R_NaString) {
	    if (!updated ||
		(s != STRING_ELT(x, i) && Scollate(s, STRING_ELT(x, i)) > 0)) {
		s = STRING_ELT(x, i);
		if(!updated) updated = TRUE;
	    }
	}
	else if (!narm) {
	    *value = R_NaString;
	    return(TRUE);
	}
    }
    *value = s;

    vmaxset(vmax);
    return updated;
}

static Rboolean imax(SEXP sx, int *value, Rboolean narm)
{
    int s = 0 /* -Wall */;
    Rboolean updated = FALSE;

    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (x[k] != R_NaInt) {
		    if (!updated || s < x[k]) {
			s = x[k];
			if(!updated) updated = TRUE;
		    }
		} else if (!narm) {
		    *value = R_NaInt;
		    return(TRUE);
		}
	    }
	});
    *value = s;
    return updated;
}

static Rboolean rmax(SEXP sx, double *value, Rboolean narm)
{
    double s = 0.0 /* -Wall */;
    Rboolean updated = FALSE;

    ITERATE_BY_REGION(sx, x, iii, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (std::isnan(x[k])) {/* Na(N) */
		    if (!narm) {
			if(!ISNA(s)) s = x[k]; /* so any NA trumps all NaNs */
			if(!updated) updated = TRUE;
		    }
		}
		else if (!updated || x[k] > s) { /* Never true if s is NA/NaN */
		    s = x[k];
		    if(!updated) updated = TRUE;
		}
	    }
	});
    *value = s;
    return updated;
}

static Rboolean smax(SEXP x, SEXP *value, Rboolean narm)
{
    SEXP s = R_NaString; /* -Wall */
    Rboolean updated = FALSE;
    const void *vmax = vmaxget(); // precautionary for Scollate

    for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
	if (STRING_ELT(x, i) != R_NaString) {
	    if (!updated ||
		(s != STRING_ELT(x, i) && Scollate(s, STRING_ELT(x, i)) < 0)) {
		s = STRING_ELT(x, i);
		if(!updated) updated = TRUE;
	    }
	}
	else if (!narm) {
	    *value = R_NaString;
	    return(TRUE);
	}
    }
    *value = s;

    vmaxset(vmax);
    return updated;
}

static Rboolean iprod(SEXP sx, double *value, Rboolean narm)
{
    LDOUBLE s = 1.0;
    Rboolean updated = FALSE;

    /**** assumes INTEGER(sx) and LOGICAL(sx) are identical!! */
    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (int k = 0; k < nbatch; k++) {
		if (x[k] != R_NaInt) {
		    s *= x[k];
		    if(!updated) updated = TRUE;
		}
		else if (!narm) {
		    if(!updated) updated = TRUE;
		    *value = R_NaReal;
		    return updated;
		}

		if(std::isnan(s)) {  /* how can this happen? */
		    *value = R_NaReal;
		    return updated;
		}
	    }
	});

    // This could over/underflow (does in package POT)
    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}

static Rboolean rprod(SEXP sx, double *value, Rboolean narm)
{
    LDOUBLE s = 1.0;
    Rboolean updated = FALSE;

    ITERATE_BY_REGION(sx, x, i, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (!narm || !std::isnan(x[k])) {
		    if(!updated) updated = TRUE;
		    s *= x[k];
		}
	    }
	});

    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}

static Rboolean cprod(SEXP sx, Rcomplex *value, Rboolean narm)
{
    Rcomplex *x = COMPLEX(sx);
    R_xlen_t n = XLENGTH(sx);
    LDOUBLE sr = 1.0, si = 0.0;
    Rboolean updated = FALSE;
    for (R_xlen_t k = 0; k < n; k++) {
	if (!narm || (!std::isnan(x[k].r) && !std::isnan(x[k].i))) {
	    if(!updated) updated = TRUE;
	    LDOUBLE tr = sr, ti = si;
	    sr = tr * x[k].r - ti * x[k].i;
	    si = tr * x[k].i + ti * x[k].r;
	}
    }
    value->r = double(sr);
    value->i = double(si);

    return updated;
}


HIDDEN
SEXP fixup_NaRm(SEXP args)
{
    SEXP t;

    /* Need to make sure na.rm is last and exists */
    GCStackRoot<> na_value(Rf_ScalarLogical(FALSE));
    for(SEXP a = args, prev = nullptr; a != nullptr; a = CDR(a)) {
	if(TAG(a) == Symbols::NaRmSymbol) {
	    if(CDR(a) == nullptr) return args;
	    na_value = CAR(a);
	    if(prev == nullptr) args = CDR(a);
	    else SETCDR(prev, CDR(a));
	}
	prev = a;
    }

    t = PairList::cons(na_value, nullptr);
    PROTECT(t);
    SET_TAG(t, Symbols::NaRmSymbol);
    if (args == nullptr)
	args = t;
    else {
	SEXP r = args;
	while (CDR(r) != nullptr) r = CDR(r);
	SETCDR(r, t);
    }
    UNPROTECT(1);
    return args;
}

/* do_summary provides a variety of data summaries
	op : 0 = sum, 1 = mean, 2 = min, 3 = max, 4 = prod
 */
/* NOTE: mean() is rather different as only one arg and no na.rm, and
 * dispatch is from an R-level generic, this being a special case of
 * mean.default.
 */

R_INLINE static SEXP logical_mean(SEXP x)
{
    R_xlen_t n = XLENGTH(x);
    LDOUBLE s = 0.0;
    for (R_xlen_t i = 0; i < n; i++) {
	int xi = LOGICAL_ELT(x, i);
	if(xi == R_NaLog)
	    return Rf_ScalarReal(R_NaReal);
	s += xi;
    }
    return Rf_ScalarReal((double) (s/n));
}

R_INLINE static SEXP integer_mean(SEXP x)
{
    R_xlen_t n = XLENGTH(x);
    LDOUBLE s = 0.0;
    for (R_xlen_t i = 0; i < n; i++) {
	int xi = INTEGER_ELT(x, i);
	if(xi == R_NaInt)
	    return Rf_ScalarReal(R_NaReal);
	s += xi;
    }
    return Rf_ScalarReal((double) (s/n));
}

R_INLINE static SEXP real_mean(SEXP x)
{
    R_xlen_t n = XLENGTH(x);
    LDOUBLE s = 0.0;
    ITERATE_BY_REGION(x, dx, i, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++)
		s += dx[k];
	});
    s /= n;
    if (std::isfinite((double) s)) {
	LDOUBLE t = 0.0;
	ITERATE_BY_REGION(x, dx, i, nbatch, double, REAL, {
		for (R_xlen_t k = 0; k < nbatch; k++)
		    t += (dx[k] - s);
	    });
	s += t/n;
    }
    return Rf_ScalarReal((double) s);
}

R_INLINE static SEXP complex_mean(SEXP x)
{
    R_xlen_t n = XLENGTH(x);
    LDOUBLE s = 0.0, si = 0.0;
    Rcomplex *px = COMPLEX(x);
    for (R_xlen_t i = 0; i < n; i++) {
	Rcomplex xi = px[i];
	s += xi.r;
	si += xi.i;
    }
    s /= n; si /= n;
    if( std::isfinite((double)s) && std::isfinite((double)si) ) {
	LDOUBLE t = 0.0, ti = 0.0;
	for (R_xlen_t i = 0; i < n; i++) {
	    Rcomplex xi = px[i];
	    t += xi.r - s;
	    ti += xi.i - si;
	}
	s += t/n; si += ti/n;
    }
    Rcomplex val = { double(s), double(si) };
    return Rf_ScalarComplex(val);
}

HIDDEN SEXP do_summary(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if(PRIMVAL(op) == 1) { /* mean */
	SEXP x = CAR(args);
	switch(TYPEOF(x)) {
	case LGLSXP:  return logical_mean(x);
	case INTSXP:  return integer_mean(x);
	case REALSXP: return real_mean(x);
	case CPLXSXP: return complex_mean(x);
	default:
	    Rf_error(R_MSG_type, Rf_type2char(TYPEOF(x)));
	    return nullptr; // -Wall on clang 4.2
	}
    }

    SEXP ans;
    /* match to foo(..., na.rm=FALSE) */
    PROTECT(args = fixup_NaRm(args));
    Expression* call2 = new Expression(CAR(call), SEXP_downcast<PairList*>(args));

    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
    auto dispatched = SEXP_downcast<BuiltInFunction*>(op)->InternalDispatch(
	call2, SEXP_downcast<Environment*>(env), std::move(arglist));
    if (dispatched.first) {
	UNPROTECT(1);
	return(dispatched.second);
    }

#ifdef DEBUG_Summary
    REprintf("C do_summary(op%s, *): did NOT dispatch\n", PRIMNAME(op));
#endif

    ans = Rf_matchArgExact(Symbols::NaRmSymbol, &args);
    Rboolean narm = Rboolean(Rf_asLogical(ans));

    if (ALTREP(CAR(args)) && CDDR(args) == nullptr &&
	(CDR(args) == nullptr || TAG(CDR(args)) == R_NaRmSymbol)) {
	SEXP toret = nullptr;
	SEXP vec = CAR(args);
	switch(PRIMVAL(op)) {
	case 0:
	    if(TYPEOF(vec) == INTSXP) 
		toret = ALTINTEGER_SUM(vec, narm);
	    else if (TYPEOF(vec) == REALSXP)
		toret = ALTREAL_SUM(vec, narm);
	    break; 
	case 2:
	    if(TYPEOF(vec) == INTSXP) 
		toret = ALTINTEGER_MIN(vec, narm);
	    else if (TYPEOF(vec) == REALSXP)
		toret = ALTREAL_MIN(vec, narm);
	    break;
	case 3:
	    if(TYPEOF(vec) == INTSXP) 
		toret = ALTINTEGER_MAX(vec, narm);
	    else if (TYPEOF(vec) == REALSXP)
		toret = ALTREAL_MAX(vec, narm);
	    break;
	default:
	    break;
	}
	if(toret != nullptr) {
	    UNPROTECT(1); /* args */
	    return toret;
	}
    }

    Rboolean int_a, real_a, complex_a,
	empty = TRUE;// <==> only zero-length arguments, or NA with na.rm=T
    int updated = 0; //
	/* updated = R_NaInt if encountered NA,
	   updated != 0 , as soon as (i)tmp (do_summary),
	   or *value ([ir]min / max) is assigned;  */
    SEXP a;
    double tmp = 0.0, s;
    Rcomplex ztmp, zcum={0.0, 0.0} /* -Wall */;
    int itmp = 0, icum = 0, warn = 0 /* dummy */;
    Rboolean use_isum = TRUE; // indicating if isum() should used; otherwise irsum()
    isum_INT iLtmp = (isum_INT)0, iLcum = iLtmp; // for isum() only
    SEXPTYPE ans_type;/* only INTEGER, REAL, COMPLEX or STRSXP here */

    int iop = PRIMVAL(op);
    switch(iop) {
    case 0:/* sum */
    /* we need to find out if _all_ the arguments are integer or logical
       in advance, as we might overflow before we find out.  NULL is
       documented to be the same as integer(0).
    */
	a = args;
        complex_a = real_a = FALSE;
	while (a != nullptr) {
            switch(TYPEOF(CAR(a))) {
	    case INTSXP:
	    case LGLSXP:
	    case NILSXP:
		break;
	    case REALSXP:
		real_a = TRUE;
		break;
	    case CPLXSXP:
		complex_a = TRUE;
		break;
	    default:
		a = CAR(a);
		Rf_errorcall(call, R_MSG_type, Rf_type2char(TYPEOF(a)));
		return nullptr;
	    }
	    a = CDR(a);
	}
        if(complex_a) {
            ans_type = CPLXSXP;
        } else if(real_a) {
            ans_type = REALSXP;
        } else {
            ans_type = INTSXP; iLcum = (isum_INT)0;
        }
	DbgP3("do_summary: sum(.. na.rm=%d): ans_type = %s\n",
	      narm, Rf_type2char(ans_type));
	zcum.r = zcum.i = 0.; icum = 0;
	break;

    case 2:/* min */
	DbgP2("do_summary: min(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
	zcum.r = R_PosInf;
	icum = INT_MAX;
	break;

    case 3:/* max */
	DbgP2("do_summary: max(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
	zcum.r = R_NegInf;;
	icum = R_INT_MIN;
	break;

    case 4:/* prod */
	ans_type = REALSXP;
	zcum.r = 1.;
	zcum.i = 0.;
	break;

    default:
	Rf_errorcall(call,
		  _("internal error ('op = %d' in do_summary).\t Call a Guru"),
		  iop);
	return nullptr;/*-Wall */
    }

    SEXP stmp = R_NaString,
	 scum = PROTECT(R_NaString);
    /*-- now loop over all arguments.  Do the 'op' switch INSIDE : */
    while (args != nullptr) {
	a = CAR(args);
	int_a = FALSE;// int_a = TRUE  <-->  a is INTEGER
	real_a = FALSE;

	if(Rf_xlength(a) > 0) {
	    updated = 0;/*- GLOBAL -*/

	    switch(iop) {
	    case 2:/* min */
	    case 3:/* max */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		    int_a = TRUE;
		    if (iop == 2) updated = imin(a, &itmp, narm);
		    else	  updated = imax(a, &itmp, narm);
		    break;
		case REALSXP:
		    real_a = TRUE;
		    if(ans_type == INTSXP) {/* change to REAL */
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    }
		    if (iop == 2) updated = rmin(a, &tmp, narm);
		    else	  updated = rmax(a, &tmp, narm);
		    break;
		case STRSXP:
		    if(!empty && ans_type == INTSXP) {
			scum = Rf_StringFromInteger(icum, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    } else if(!empty && ans_type == REALSXP) {
			scum = Rf_StringFromReal(zcum.r, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    }
		    ans_type = STRSXP;
		    if (iop == 2) updated = smin(a, &stmp, narm);
		    else updated = smax(a, &stmp, narm);
		    break;
		default:
		    Rf_errorcall(call, R_MSG_type, Rf_type2char(TYPEOF(a)));
		    return nullptr;
		}

		if(updated) {/* 'a' had non-NA elements; --> "add" tmp or itmp*/
		    DbgP1(" updated:");
		    if(ans_type == INTSXP) {
			DbgP3(" INT: (old)icum= %ld, itmp=%ld\n", icum,itmp);
			if (icum == R_NaInt); /* NA trumps anything */
			else if (itmp == R_NaInt ||
			    (iop == 2 && itmp < icum) || /* min */
			    (iop == 3 && itmp > icum))   /* max */
			    icum = itmp;
		    } else if(ans_type == REALSXP) {
			if (int_a) tmp = Int2Real(itmp);
			DbgP3(" REAL: (old)cum= %g, tmp=%g\n", zcum.r,tmp);
			if (ISNA(zcum.r)); /* NA trumps anything */
			else if (std::isnan(tmp)) {
			    if (ISNA(tmp)) zcum.r = tmp;
			    else zcum.r += tmp;/* NA or NaN */
			} else if(
			    (iop == 2 && tmp < zcum.r) ||
			    (iop == 3 && tmp > zcum.r))	zcum.r = tmp;
		    } else if(ans_type == STRSXP) {
			if(int_a)
			   stmp = Rf_StringFromInteger(itmp, &warn);
			else if(real_a)
			   stmp = Rf_StringFromReal(tmp, &warn);

			if(empty)
			    scum = stmp;
			else if (scum != R_NaString) {
			    PROTECT(stmp);
			    if(stmp == R_NaString ||
			       (iop == 2 && stmp != scum && Scollate(stmp, scum) < 0) ||
			       (iop == 3 && stmp != scum && Scollate(stmp, scum) > 0) )
				scum = stmp;
			    UNPROTECT(1); /* stmp */
			}
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    }
		}/*updated*/ else {
		    /*-- in what cases does this happen here at all?
		      -- if there are no non-missing elements.
		     */
		    DbgP2(" NOT updated [!! RARE !!]: int_a=%s\n", int_a ? "TRUE" : "FALSE");
		}

		break;/*--- end of  min() / max() ---*/

	    case 0:/* sum */
		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
#ifdef LONG_INT
		    updated = (use_isum ?
			       isum(a, &iLtmp, narm, call) :
			       risum(a,  &tmp, narm));
		    DbgP2(" int|lgl: updated=%d ", updated);
		    if(updated == R_NaInt)
			goto na_answer;
		    else if(use_isum && updated == 42) {
			// impending integer overflow --> switch to irsum()
			use_isum = FALSE;
			if(ans_type == INTSXP) ans_type = REALSXP;
			// re-sum() 'a' (a waste, rare; FIXME ?) :
			risum(a, &tmp, narm);
			zcum.r = (double) iLcum + tmp;
			DbgP3(" .. switching type to REAL, tmp=%g, zcum.r=%g",
			      tmp, zcum.r);
		    }
		    else if(updated) {
			// iLtmp is LONG_INT i.e. at least 64bit
			if(ans_type == INTSXP) {
			    s = (double) iLcum + (double) iLtmp;
			    if(s > INT_MAX || s < R_INT_MIN ||
			       iLtmp < -LONG_INT_MAX || LONG_INT_MAX < iLtmp) {
				ans_type = REALSXP;
				zcum.r = s;
				DbgP2(" int_1 switch: zcum.r = s = %g\n", s);
			    } else if(s < double(-LONG_INT_MAX) || double(LONG_INT_MAX) < s) {
				use_isum = FALSE;
				ans_type = REALSXP;
				zcum.r = s;
				DbgP2(" int_2 switch: zcum.r = s = %g\n", s);
			    }
			    else {
				iLcum += iLtmp;
				DbgP3(" int_3: (iLtmp,iLcum) = (%ld,%ld)\n",
				      iLtmp, iLcum);
			    }
			} else { // dealt with R_NaInt already above
			    zcum.r += use_isum ? (double)iLtmp : tmp;
			    DbgP3(" dbl: (*tmp, zcum.r) = (%g,%g)\n",
				  use_isum ? (double)iLtmp : tmp, zcum.r);
			}
		    }
#else
		    updated = isum(a, &iLtmp, narm, call);
		    if(updated) {
			if(iLtmp == R_NaInt) goto na_answer;
			if(ans_type == INTSXP) {
			    s = double(icum) + double(iLtmp);
			    if(s > INT_MAX || s < R_INT_MIN){
				Rf_warningcall(call,_(
				  "Integer overflow - use sum(as.numeric(.))"));
				goto na_answer;
			    }
			    else icum += iLtmp;
			} else
			    zcum.r += Int2Real(iLtmp);
		    }
#endif
		    break;
		case REALSXP:
		    if(ans_type == INTSXP) {
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(iLcum);
		    }
		    updated = rsum(a, &tmp, narm);
		    if(updated) {
			zcum.r += tmp;
		    }
		    break;
		case CPLXSXP:
		    if(ans_type == INTSXP) {
			ans_type = CPLXSXP;
			if(!empty) zcum.r = Int2Real(iLcum);
		    } else if (ans_type == REALSXP)
			ans_type = CPLXSXP;
		    updated = csum(a, &ztmp, narm);
		    if(updated) {
			zcum.r += ztmp.r;
			zcum.i += ztmp.i;
		    }
		    break;
		default:
		    Rf_errorcall(call, R_MSG_type, Rf_type2char(TYPEOF(a)));
		    return nullptr;
		}

		break;/* sum() part */

	    case 4:/* prod */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		case REALSXP:
		    if(TYPEOF(a) == REALSXP)
			updated = rprod(a, &tmp, narm);
		    else
			updated = iprod(a, &tmp, narm);
		    if(updated) {
			zcum.r *= tmp;
			zcum.i *= tmp;
		    }
		    break;
		case CPLXSXP:
		    ans_type = CPLXSXP;
		    updated = cprod(a, &ztmp, narm);
		    if(updated) {
			Rcomplex z;
			z.r = zcum.r;
			z.i = zcum.i;
			zcum.r = z.r * ztmp.r - z.i * ztmp.i;
			zcum.i = z.r * ztmp.i + z.i * ztmp.r;
		    }
		    break;
		default:
		    Rf_errorcall(call, R_MSG_type, Rf_type2char(TYPEOF(a)));
		    return nullptr;
		}

		break;/* prod() part */

	    } /* switch(iop) */

	} else { /* len(a)=0 */
	    /* Even though this has length zero it can still be invalid,
	       e.g. list() or raw() */
	    switch(TYPEOF(a)) {
	    case LGLSXP:
	    case INTSXP:
	    case REALSXP:
	    case NILSXP:  /* OK historically, e.g. PR#1283 */
		break;
	    case CPLXSXP:
		if (iop == 2 || iop == 3)
		    Rf_errorcall(call, R_MSG_type, Rf_type2char(TYPEOF(a)));
		return nullptr;
		break;
	    case STRSXP:
		if (iop == 2 || iop == 3) {
		    if(!empty && ans_type == INTSXP) {
			scum = Rf_StringFromInteger(icum, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    } else if(!empty && ans_type == REALSXP) {
			scum = Rf_StringFromReal(zcum.r, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    }
		    ans_type = STRSXP;
		    break;
		}
	    default:
		Rf_errorcall(call, R_MSG_type, Rf_type2char(TYPEOF(a)));
		return nullptr;
	    }
	    if(ans_type < TYPEOF(a) && ans_type != CPLXSXP) {
		if(!empty && ans_type == INTSXP)
		    zcum.r = Int2Real(icum);
		ans_type = TYPEOF(a);
	    }
	}
	DbgP3(" .. upd.=%d, empty=%d", updated, (int)empty);
	if(empty && updated) empty=FALSE;
	DbgP2(", new empty=%d\n", (int)empty);
	args = CDR(args);
    } /*-- while(..) loop over args */

    /*-------------------------------------------------------*/
    if(empty && (iop == 2 || iop == 3)) {
	if(ans_type == STRSXP) {
	    Rf_warningcall(call, _("no non-missing arguments, returning NA"));
	} else {
	    if(iop == 2)
		Rf_warningcall(call, _("no non-missing arguments to min; returning Inf"));
	    else
		Rf_warningcall(call, _("no non-missing arguments to max; returning -Inf"));
	    ans_type = REALSXP;
	}
    }

    switch(ans_type) {
    case INTSXP:   ans = (iop == 0) ? Rf_ScalarInteger(iLcum) : Rf_ScalarInteger(icum);break;
    case REALSXP:  ans = Rf_ScalarReal(zcum.r); break;
    case CPLXSXP:  ans = Rf_ScalarComplex(zcum);break;
    case STRSXP:   ans = Rf_ScalarString(scum); break;
    default:       break;  // -Wswitch
    }
    UNPROTECT(2); /* scum, args */
    return ans;

na_answer: /* only sum(INTSXP, ...) case currently used */
    switch(ans_type) {
    case INTSXP:	ans = Rf_ScalarInteger(R_NaInt); break;
    case REALSXP:	ans = Rf_ScalarReal(R_NaReal); break;
    case CPLXSXP:	ans = Rf_ScalarComplex({R_NaReal, R_NaReal}); break;
    case STRSXP:        ans = Rf_ScalarString(R_NaString); break;
    default:            break;  // -Wswitch
    }
    UNPROTECT(2); /* scum, args */
    return ans;
}/* do_summary */


HIDDEN SEXP do_range(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    PROTECT(args = fixup_NaRm(args));
    Expression* call2 = new Expression(CAR(call), SEXP_downcast<PairList*>(args));
    Environment* callenv = SEXP_downcast<Environment*>(env);

    ArgList arglist(SEXP_downcast<PairList*>(args), ArgList::EVALUATED);
    auto dispatched = SEXP_downcast<BuiltInFunction*>(op)->InternalDispatch(
	call2, callenv, std::move(arglist));
    if (dispatched.first) {
	UNPROTECT(1);
	return(dispatched.second);
    }

    PROTECT(op = Rf_findFun(Rf_install("range.default"), env));
    Closure* closure = SEXP_downcast<Closure*>(op);
    ans = call2->evaluateFunctionCall(closure, callenv, arglist);
    UNPROTECT(2);
    return(ans);
}

// which.min(x) : The index (starting at 1), of the first min(x) in x
// which.max(x) : The index (starting at 1), of the first max(x) in x
HIDDEN SEXP do_first_min(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP sx = x_, ans;
    R_xlen_t i, n, indx = -1;

    if (!Rf_isNumeric(sx)) {
	sx = Rf_coerceVector(sx, REALSXP);
    }
    n = XLENGTH(sx);
    switch(TYPEOF(sx)) {
    case LGLSXP: // with only (TRUE, FALSE, NA) -- may be fast
    {
	int *r = LOGICAL(sx);
	if(op->variant() == 0) { /* which.min */
	    for (i = 0; i < n; i++)
		if (r[i] == FALSE) {
		    indx = i; break; // found FALSE: done
		} else if (indx == -1 && r[i] != R_NaLog) {
		    indx = i; // first TRUE
		}
	} else { /* which.max */
	    for (i = 0; i < n; i++)
		if (r[i] == TRUE) {
		    indx = i; break; // found TRUE: done
		} else if (indx == -1 && r[i] != R_NaLog) {
		    indx = i; // first FALSE
		}
	}
    }
    break;

    case INTSXP:
    {
	int s, *r = INTEGER(sx);
	if(op->variant() == 0) { /* which.min */
	    s = INT_MAX;
	    for (i = 0; i < n; i++)
		if (r[i] != R_NaInt && (r[i] < s || indx == -1)) {
		    s = r[i]; indx = i;
		}
	} else { /* which.max */
	    s = INT_MIN;
	    for (i = 0; i < n; i++)
		if (r[i] != R_NaInt && (r[i] > s || indx == -1)) {
		    s = r[i]; indx = i;
		}
	}
    }
    break;

    case REALSXP:
    {
	double s, *r = REAL(sx);
	if(op->variant() == 0) { /* which.min */
	    s = R_PosInf;
	    for (i = 0; i < n; i++)
		if ( !std::isnan(r[i]) && (r[i] < s || indx == -1) ) {
		    s = r[i]; indx = i;
		}
	} else { /* which.max */
	    s = R_NegInf;
	    for (i = 0; i < n; i++)
		if ( !std::isnan(r[i]) && (r[i] > s || indx == -1) ) {
		    s = r[i]; indx = i;
		}
	}
    }
    break;
    default: Rf_error(_("invalid type")); break;
    } // switch()


    i = (indx != -1);
    bool large = (indx + 1) > INT_MAX;
    PROTECT(ans = Rf_allocVector(large ? REALSXP : INTSXP, i ? 1 : 0));
    if (i) {
	if(large)
	    REAL(ans)[0] = (double)indx + 1;
	else
	    INTEGER(ans)[0] = (int)indx + 1;
	if (Rf_getAttrib(sx, Symbols::NamesSymbol) != nullptr) { /* preserve names */
	    SEXP ansnam;
	    PROTECT(ansnam =
		    Rf_ScalarString(STRING_ELT(Rf_getAttrib(sx, Symbols::NamesSymbol), indx)));
	    Rf_setAttrib(ans, Symbols::NamesSymbol, ansnam);
	    UNPROTECT(1);
	}
    }
    return ans;
}

/* which(x) : indices of non-NA TRUE values in x */
HIDDEN SEXP do_which(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP v, v_nms, ans, ans_nms = nullptr;
    int i, j = 0, len, *buf;

    v = x_;
    if (!Rf_isLogical(v))
	Rf_error(_("argument to 'which' is not logical"));
    len = Rf_length(v);
    buf = reinterpret_cast<int *>(R_alloc(len, sizeof(int)));

    int *pv = LOGICAL(v);
    for (i = 0; i < len; i++) {
	if (pv[i] == TRUE) {
	    buf[j] = i + 1;
	    j++;
	}
    }

    len = j;
    PROTECT(ans = Rf_allocVector(INTSXP, len));
    if(len) memcpy(INTEGER(ans), buf, sizeof(int) * len);

    if ((v_nms = Rf_getAttrib(v, Symbols::NamesSymbol)) != nullptr) {
	PROTECT(ans_nms = Rf_allocVector(STRSXP, len));
	int *pa = INTEGER(ans);
	for (i = 0; i < len; i++) {
	    SET_STRING_ELT(ans_nms, i, STRING_ELT(v_nms, pa[i] - 1));
	}
	Rf_setAttrib(ans, Symbols::NamesSymbol, ans_nms);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return ans;
}


/* op = 0 is pmin, op = 1 is pmax
   NULL and logicals are handled as if they had been coerced to integer.
 */
HIDDEN SEXP do_pmin(/*const*/ Expression* call, const BuiltInFunction* op, Environment* rho, RObject* const* args, int num_args, const PairList* tags)
{
    SEXP x, ans;
    int narm;
    R_xlen_t i, n, len, i1;
    SEXPTYPE type, anstype;

    // Remove narm from the args.
    assert(num_args >= 1);
    narm = Rf_asLogical(args[0]);
    if(narm == R_NaLog)
	Rf_error(_("invalid '%s' value"), "na.rm");
    if(num_args < 2) Rf_error(_("no arguments"));
    args = (args + 1);
    num_args = num_args - 1;

    // Check that the types are valid and get the max length.
    len = Rf_xlength(args[0]);
    anstype = TYPEOF(args[0]);
    for (int arg = 0; arg < num_args; arg++) {
	x = args[arg];
	type = TYPEOF(x);
	switch(type) {
	case NILSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case STRSXP:
	    break;
	default:
	    Rf_error(_("invalid input type"));
	}
	if(type > anstype) anstype = type;
	n = Rf_xlength(x);
	if ((len > 0) ^ (n > 0)) {
	    // till 2.15.0:  Rf_error(_("cannot mix 0-length vectors with others"));
	    len = 0;
	    break;
	}
	len = std::max(len, n);
    }

    if (num_args == 1)
	return args[0]; /* one input */

    if(anstype < INTSXP) anstype = INTSXP;
    if(len == 0) return Rf_allocVector(anstype, 0);
    /* Check for fractional recycling (added in 2.14.0) */
    for (int arg = 0; arg < num_args; arg++) {
	n = Rf_length(args[arg]);
	if (len % n) {
	    Rf_warning(_("an argument will be fractionally recycled"));
	    break;
	}
    }

    PROTECT(ans = Rf_allocVector(anstype, len));
    switch(anstype) {
    case INTSXP:
    {
	int *r,  *ra = INTEGER(ans), tmp;
	PROTECT(x = Rf_coerceVector(args[0], anstype));
	r = INTEGER(x);
	n = XLENGTH(x);
	xcopyWithRecycle(ra, r, 0, len, n);
	UNPROTECT(1);
	for (int arg = 1; arg < num_args; arg++) {
	    x = args[arg];
	    PROTECT(x = Rf_coerceVector(x, anstype));
	    n = XLENGTH(x);
	    r = INTEGER(x);
	    MOD_ITERATE1(len, n, i, i1, {
		tmp = r[i1];
		if(op->variant() == 1) {
		    if( (narm && ra[i] == R_NaInt) ||
			(ra[i] != R_NaInt && tmp != R_NaInt
			 && tmp > ra[i]) ||
			(!narm && tmp == R_NaInt) )
			ra[i] = tmp;
		} else {
		    if( (narm && ra[i] == R_NaInt) ||
			(ra[i] != R_NaInt && tmp != R_NaInt
			 && tmp < ra[i]) ||
			(!narm && tmp == R_NaInt) )
			ra[i] = tmp;
		}
	    });
	    UNPROTECT(1);
	}
    }
	break;
    case REALSXP:
    {
	double *r, *ra = REAL(ans), tmp;
	PROTECT(x = Rf_coerceVector(args[0], anstype));
	r = REAL(x);
	n = XLENGTH(x);
	xcopyWithRecycle(ra, r, 0, len, n);
	UNPROTECT(1);
	for (int arg = 1; arg < num_args; arg++) {
	    x = args[arg];
	    PROTECT(x = Rf_coerceVector(x, anstype));
	    n = XLENGTH(x);
	    r = REAL(x);
	    MOD_ITERATE1(len, n, i, i1, {
		tmp = r[i1];
		if(op->variant() == 1) {
		    if( (narm && std::isnan(ra[i])) ||
			(!std::isnan(ra[i]) && !std::isnan(tmp) && tmp > ra[i]) ||
			(!narm && std::isnan(tmp)) )
			ra[i] = tmp;
		} else {
		    if( (narm && std::isnan(ra[i])) ||
			(!std::isnan(ra[i]) && !std::isnan(tmp) && tmp < ra[i]) ||
			(!narm && std::isnan(tmp)) )
			ra[i] = tmp;
		}
	    });
	    UNPROTECT(1);
	}
    }
	break;
    case STRSXP:
    {
	PROTECT(x = Rf_coerceVector(args[0], anstype));
	n = XLENGTH(x);
	xcopyStringWithRecycle(ans, x, 0, len, n);
	UNPROTECT(1);
	for (int arg = 1; arg < num_args; arg++) {
	    x = args[arg];
	    SEXP tmp, t2;
	    PROTECT(x = Rf_coerceVector(x, anstype));
	    n = XLENGTH(x);
	    MOD_ITERATE1(len, n, i, i1, {
		tmp = STRING_ELT(x, i1);
		t2 = STRING_ELT(ans, i);
		if(op->variant() == 1) {
		    if( (narm && t2 == R_NaString) ||
			(t2 != R_NaString && tmp != R_NaString && tmp != t2 && Scollate(tmp, t2) > 0) ||
			(!narm && tmp == R_NaString) )
			SET_STRING_ELT(ans, i, tmp);
		} else {
		    if( (narm && t2 == R_NaString) ||
			(t2 != R_NaString && tmp != R_NaString && tmp != t2 && Scollate(tmp, t2) < 0) ||
			(!narm && tmp == R_NaString) )
			SET_STRING_ELT(ans, i, tmp);
		}
	    });
	    UNPROTECT(1);
	}
    }
	break;
    default:
	break;
    }
    UNPROTECT(1);
    return ans;
}
